-- | The pure rules: one fixed tick from one state to the next.
--
-- Order inside a Playing tick, which the tests pin down:
--
--   1. note fresh presses (a press during a step or a wave is remembered, not lost)
--   2. advance the current step or wave; a landing step touches its new cell
--   3. if settled and no action is affordable, the level is lost (a win in step 2
--      has already changed the phase, so reaching the goal on the last second wins)
--   4. if still playing and settled: a pending undo, the pending shift, then a
--      pending or held move
--
-- Time is a budget, not a clock: ticks spend nothing. An action is charged once,
-- when it is accepted.
module Game.Logic
  ( initialGame
  , stepGame
  , tryShift
  , tryStep
  , tryEcho
  , openCell
  , doorsOpen
  , weights
  , echoPath
  , echoLanding
  , Jump (..)
  , mirrorAhead
  , jumpTarget
  , beamCells
  , lightPasses
  , crossings
  , Hint (..)
  , currentHint
  , objectiveText
  , hintText
  , blockedTitle
  , blockedDetail
  , tooCostlyTitle
  , tooCostlyDetail
  , pushBlockedTitle
  , pushBlockedDetail
  , pushAgainText
  , rustedText
  , deadEndText
  , peekText
  , victoryText
  , failureText
  , pickupText
  , echoNeedsStepsText
  , echoInDoorwayText
  , plateHeldText
  , jumpAgainText
  , mirrorPushText
  , mirrorFrontText
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Game.Level (walkable)
import Game.Types

-- | The title screen, with the level ready behind it.
initialGame :: Level -> Game
initialGame level = Game
  { gPhase        = Menu
  , gEra          = Present
  , gCell         = lvSpawn level
  , gHasCell      = False
  , gRemaining    = lvBudget level
  , gCrates       = lvCrates level
  , gFilled       = Set.empty
  , gMirrors      = lvMirrors level
  , gMovableMirrors = lvMovableMirrors level
  , gTrail        = take (ruEchoLength (lvRules level)) [(Present, lvSpawn level)]
  , gEcho         = Nothing
  , gHistory      = []
  , gPushing      = Nothing
  , gPushingMirror = Nothing
  , gAttempt      = 0
  , gMotion       = Settled
  , gFacing       = DirDown
  , gPendingShift = False
  , gPendingMove  = Nothing
  , gPendingUndo  = False
  , gPendingEcho  = False
  , gFeedback     = Nothing
  , gBump         = 0
  , gPrev         = noInput
  , gArmed        = False
  }

-- | One simulation tick.
stepGame :: Level -> TickInput -> Game -> Game
stepGame level input g = next { gPrev = input }
  where
    prev = gPrev g
    freshEnter   = tiEnter input   && not (tiEnter prev)
    freshRestart = tiRestart input && not (tiRestart prev)
    freshPause   = tiPause input   && not (tiPause prev)
    freshUndo    = tiUndo input    && not (tiUndo prev)
    begin = (initialGame level) { gPhase = Playing, gAttempt = gAttempt g + 1 }
    next = case gPhase g of
      Menu
        | freshEnter   -> begin
        | otherwise    -> g
      Playing
        | freshRestart -> begin
        | freshPause   -> quiet g { gPhase = Paused }
        | otherwise    -> playTick level input g
      Paused
        | freshRestart -> begin
        | freshPause   -> quiet g { gPhase = Playing }
        | otherwise    -> g
      Won
        | freshEnter || freshRestart -> begin
        | otherwise    -> g
      Lost
        | freshEnter || freshRestart -> begin
        | freshUndo    -> undo level g
        | otherwise    -> g

-- | Forget anything waiting to happen. Keys held across this need a fresh press.
quiet :: Game -> Game
quiet g = g { gPendingShift = False, gPendingMove = Nothing, gPendingUndo = False
            , gPendingEcho = False, gArmed = False }

playTick :: Level -> TickInput -> Game -> Game
playTick level input g0 = startNext level input (outOfTime level g2)
  where
    -- Letting go of a newer key shows the older one still held. That keeps a walk
    -- going, through gArmed, but it is not a press: it must not replace a buffered
    -- tap, and after an undo or a restart it must not start a step that costs time.
    freshDir
      | tiDir input /= tiDir (gPrev g0) && not (tiDirRevealed input) = tiDir input
      | otherwise = Nothing
    freshShift = tiShift input && not (tiShift (gPrev g0))
    freshUndo  = tiUndo input && not (tiUndo (gPrev g0))
    freshEcho  = tiEcho input && not (tiEcho (gPrev g0))
    locked     = case gMotion g0 of
      Shifting _ -> True
      _          -> False
    g1 = g0
      { gPendingShift = gPendingShift g0 || (freshShift && not locked)
      , gPendingMove  = freshDir <|> gPendingMove g0
      , gPendingUndo  = gPendingUndo g0 || freshUndo
      , gPendingEcho  = gPendingEcho g0 || freshEcho
      , gArmed        = isJust (tiDir input) && (gArmed g0 || isJust freshDir)
      , gFeedback     = decayFeedback (gFeedback g0)
      , gBump         = max 0 (gBump g0 - 1)
      }
    g2 = advanceMotion level g1

decayFeedback :: Maybe Feedback -> Maybe Feedback
decayFeedback Nothing = Nothing
decayFeedback (Just fb)
  | fbLeft fb <= 1 = Nothing
  | otherwise      = Just fb { fbLeft = fbLeft fb - 1 }

-- | Progress the current step or wave by one tick.
advanceMotion :: Level -> Game -> Game
advanceMotion level g = case gMotion g of
  Settled -> g
  Shifting left
    | left <= 1 -> g { gMotion = Settled }
    | otherwise -> g { gMotion = Shifting (left - 1) }
  Stepping to left
    | left <= 1 -> touchCell level g
        { gMotion = Settled, gCell = to, gPushing = Nothing, gPushingMirror = Nothing }
    | otherwise -> g { gMotion = Stepping to (left - 1) }

-- | The one place pickup and victory happen. Runs when a step lands and when an
-- era changes, because both put the player in contact with something new.
touchCell :: Level -> Game -> Game
touchCell level = victory . pickup
  where
    pickup g
      | gEra g == Past && gCell g == lvPowerCell level && not (gHasCell g) =
          g { gHasCell = True
            , gFeedback = Just (Feedback CellCollected pickupFeedbackTicks) }
      | otherwise = g
    victory g
      | gEra g == Present && gCell g == lvGoal level && gHasCell g =
          (quiet g) { gPhase = Won, gMotion = Settled, gFeedback = Nothing }
      | otherwise = g

-- | Begin whatever is next, if the player is free and the level is still on.
startNext :: Level -> TickInput -> Game -> Game
startNext level input g
  | gPhase g /= Playing || gMotion g /= Settled = g
  | gPendingUndo g = undo level g
  | gPendingEcho g = startNext level input (tryEcho level g)
  | gPendingShift g = startNext level input (tryShift level g)
  | Just dir <- gPendingMove g =
      tryStep level True dir g { gPendingMove = Nothing }
  | Just dir <- tiDir input, gArmed g =
      tryStep level False dir g
  | otherwise = g

-- | The level ends once nothing is affordable. An undo waiting to happen goes first,
-- because it would give time back.
outOfTime :: Level -> Game -> Game
outOfTime level g
  | gPhase g == Playing && gMotion g == Settled && not (gPendingUndo g)
    && gRemaining g < ruStepCost (lvRules level) = (quiet g) { gPhase = Lost, gFeedback = Nothing }
  | otherwise = g

-- | Pay for an action, remembering the board as it stood so undo can restore it.
charge :: Int -> Game -> Game
charge cost g = g
  { gRemaining = gRemaining g - cost
  , gHistory = before : gHistory g
  , gFeedback = case gFeedback g of
      -- Advice about pushing has done its job once the player moves.
      Just (Feedback kind _) | kind `elem` [PushNeedsPress, CrateRusted, MirrorNeedsPress, MirrorFrontBlocked] -> Nothing
      other -> other
  }
  where
    before = Snapshot
      { snEra = gEra g, snCell = gCell g, snHasCell = gHasCell g
      , snCrates = gCrates g, snFilled = gFilled g
      , snMirrors = gMirrors g, snMovableMirrors = gMovableMirrors g
      , snFacing = gFacing g, snTrail = gTrail g, snEcho = gEcho g, snCost = cost }

-- | Note where the player now stands, keeping only as many footprints as the level
-- allows. A level with no echo keeps none, so its states stay as few as before.
footprint :: Level -> Spot -> Game -> Game
footprint level spot g = g { gTrail = take (ruEchoLength (lvRules level)) (spot : gTrail g) }

-- | Rewind one accepted action. The board comes back exactly, the time only in
-- part, so undoing can never beat having played well. It also cancels anything
-- buffered, and a held direction needs a fresh press, or it would walk straight
-- back into the mistake.
undo :: Level -> Game -> Game
undo level g = case gHistory g of
  [] -> quiet g
  (s : rest) ->
    let rules = lvRules level
        remaining = gRemaining g + snCost s * ruUndoRefundPercent rules `div` 100
    in (quiet g)
         { gEra = snEra s, gCell = snCell s, gHasCell = snHasCell s
         , gCrates = snCrates s, gFilled = snFilled s, gFacing = snFacing s
         , gMirrors = snMirrors s, gMovableMirrors = snMovableMirrors s
         , gTrail = snTrail s, gEcho = snEcho s
         , gRemaining = remaining, gHistory = rest
         , gMotion = Settled, gPushing = Nothing, gPushingMirror = Nothing
         , gFeedback = Nothing, gBump = 0
         , gPhase = if remaining < ruStepCost rules then Lost else Playing
         }

-- | Switch era if this cell is open in the other one and the time is there. Only a
-- settled player can shift; a shift refused here changes nothing but the feedback,
-- and costs nothing, so testing a cell is never punished.
tryShift :: Level -> Game -> Game
tryShift level g
  | gMotion g /= Settled = g
  | not (openCell level g destination (gCell g)) =
      refuse (ShiftBlocked (gCell g))
  | gRemaining g < cost =
      refuse ShiftTooCostly
  | otherwise =
      touchCell level (footprint level (destination, gCell g) (charge cost cleared))
        { gEra      = destination
        , gMotion   = Shifting shiftTicks
        , gFeedback = keepPickupNote (gFeedback g)
        }
  where
    cost = ruShiftCost (lvRules level)
    destination = otherEra (gEra g)
    cleared = g { gPendingShift = False }
    refuse kind = cleared { gFeedback = Just (Feedback kind blockedFeedbackTicks) }
    keepPickupNote fb = case fb of
      Just (Feedback CellCollected _) -> fb
      _                               -> Nothing

-- | Whether the player could stand here right now: the era's map, plus any pits
-- filled so far, minus wherever the crates are. A crate has one position, so it
-- blocks that cell in both eras.
openCell :: Level -> Game -> Era -> Cell -> Bool
openCell level g era cell =
  cell `Set.notMember` gCrates g && mirrorAt g era cell == Nothing && (onMap || filled || door)
  where
    onMap = walkable level era cell
    filled = era == Past && cell `Set.member` gFilled g
    door = era == Present && cell `Set.member` lvDoors level && doorsOpen level (weights g)

-- | Every cell the player could switch era on right now: open in both eras, given
-- where the crates, filled pits and open doors are. Worked out from the map and the
-- board, so it can be drawn but never has to be kept up to date.
crossings :: Level -> Game -> [Cell]
crossings level g =
  [ c | y <- [0 .. lvHeight level - 1], x <- [0 .. lvWidth level - 1], let c = (x, y)
      , openCell level g Past c, openCell level g Present c ]

-- | The doors open while something rests on every plate. This is worked out from
-- where things are, not stored, so undo, restart and the solver need nothing new, and
-- a door can never disagree with its plates. The parser keeps plates away from
-- doorways, and an echo cannot be sent from a doorway.
doorsOpen :: Level -> Set.Set Cell -> Bool
doorsOpen level held = not (Set.null plates) && plates `Set.isSubsetOf` held
  where plates = lvPlates level

-- Mirrors ----------------------------------------------------------------------

mirrorAt :: Game -> Era -> Cell -> Maybe Dir
mirrorAt g era cell = case Map.lookup cell (gMirrors g) of
  Just (e, face) | e == era -> Just face
  _ -> Nothing

opposite :: Dir -> Dir
opposite DirUp    = DirDown
opposite DirDown  = DirUp
opposite DirLeft  = DirRight
opposite DirRight = DirLeft

-- | Whether light crosses this cell in this era: anywhere you could stand, and an
-- open pit. Walls, crates, shut doors and mirrors stop it.
lightPasses :: Level -> Game -> Era -> Cell -> Bool
lightPasses level g era cell =
  openCell level g era cell || (era == Past && cell `Set.member` lvPits level)

-- | The light a mirror throws out of its face, up to whatever stops it. Only in the
-- mirror's own era.
beamCells :: Level -> Game -> Cell -> [Cell]
beamCells level g mirror = case Map.lookup mirror (gMirrors g) of
  Just (era, face) -> takeWhile (lightPasses level g era) (drop 1 (iterate (stepCell face) mirror))
  Nothing -> []

-- | A mirror the player stands in the light of, facing it.
data Jump = Jump
  { jMirror :: !Cell
  , jTarget :: !Cell  -- ^ as far behind the mirror as the player stands in front of it
  , jOpen   :: !Bool  -- ^ whether the player could stand there, so the jump would happen
  } deriving (Eq, Show)

-- | Looking along @dir@ from the player: if the light reaches a mirror in this era
-- whose face looks back at them, that mirror and where it would put them. The
-- player is in its light exactly when nothing stands between them.
mirrorAhead :: Level -> Game -> Dir -> Maybe Jump
mirrorAhead level g dir = look (1 :: Int) (stepCell dir (gCell g))
  where
    era = gEra g
    look n c
      | lightPasses level g era c = look (n + 1) (stepCell dir c)
      | otherwise = case mirrorAt g era c of
          Just face | face == opposite dir ->
            let target = iterate (stepCell dir) c !! n
            in Just (Jump c target (openCell level g era target))
          _ -> Nothing

-- | Where pressing @dir@ would jump the player to, if it would.
jumpTarget :: Level -> Game -> Dir -> Maybe Cell
jumpTarget level g dir = case mirrorAhead level g dir of
  Just j | jOpen j -> Just (jTarget j)
  _ -> Nothing

-- | Every cell something is standing on: the crates, the player and the echo. A plate
-- is set in the floor of both eras, so it does not matter which era they are in.
weights :: Game -> Set.Set Cell
weights g = Set.insert (gCell g) (maybe id (Set.insert . snd) (gEcho g) (gCrates g))

-- | The way an echo sent now would walk: from the player's spot back along their
-- footprints, oldest last, stopping before any footprint that is no longer open,
-- such as one a crate has since been pushed onto. Shared by the rule and the drawing
-- of where it would land, so the preview cannot disagree with what happens.
echoPath :: Level -> Game -> [Spot]
echoPath level g = case gTrail g of
  here : older -> here : takeWhile free older
  [] -> []
  where
    free (era, cell) = openCell level g era cell

-- | Where an echo sent now would stand, if there is anywhere to walk back to.
echoLanding :: Level -> Game -> Maybe Spot
echoLanding level g = case echoPath level g of
  path@(_ : _ : _) -> Just (last path)
  _ -> Nothing

-- | Send the echo: it walks the footprints back and stays where they end, until the
-- next one replaces it. The footprints start again from here. Refused for free, with
-- a reason, if there is nothing to walk back along, or from a doorway, where the old
-- echo leaving its plate could shut the door on the player.
tryEcho :: Level -> Game -> Game
tryEcho level g
  | gMotion g /= Settled = g
  | ruEchoLength rules <= 0 = cleared
  | Nothing <- landing = told EchoNeedsSteps
  | gEra g == Present && gCell g `Set.member` lvDoors level = told EchoInDoorway
  | gRemaining g < cost = cleared
  | otherwise = (charge cost cleared)
      { gEcho = landing
      , gTrail = take (ruEchoLength rules) [(gEra g, gCell g)]
      }
  where
    rules = lvRules level
    cost = ruEchoCost rules
    landing = echoLanding level g
    cleared = g { gPendingEcho = False }
    told kind = cleared { gFeedback = Just (Feedback kind hintFeedbackTicks) }

-- | Start one step if the destination is open, or if it holds a crate that can be
-- pushed. Everything is checked here, before anything moves, and never again. The
-- crate moves when the step is accepted, like the charge, so no tick can see the
-- board half done. Only a fresh press pushes: holding a key walks up to a crate and
-- stops, so a walk can never shove one into a dead end. A crate exists in both eras,
-- so it can only go where both have room: never into a wall in the present. A pit
-- in the past is the exception, because the crate is used up there.
tryStep :: Level -> Bool -> Dir -> Game -> Game
tryStep level isFresh dir g
  | gMotion g /= Settled = g
  | Just target <- jumpTarget level g dir = jumpTo target
  | to `Set.member` gMovableMirrors g = pushMirrorOrRefuse
  | to `Set.member` gCrates g = pushOrRefuse
  | not (openCell level g (gEra g) to) = refused
  | gRemaining g < cost = g { gFacing = dir }
  | otherwise = (walked (charge cost g)) { gFacing = dir, gMotion = Stepping to stepTicks }
  where
    walked = footprint level (gEra g, to)
    -- Facing a mirror along its light, with somewhere to land: the key steps through
    -- it instead of walking. Like a push, only a fresh press does, so holding a key
    -- walks up into the light and stops there.
    jumpTo target
      | not isFresh = g { gFacing = dir, gFeedback = Just (Feedback JumpNeedsPress hintFeedbackTicks) }
      | gRemaining g < ruJumpCost (lvRules level) = g { gFacing = dir }
      | otherwise = (footprint level (gEra g, target) (charge (ruJumpCost (lvRules level)) g))
          { gFacing = dir, gMotion = Stepping target stepTicks }
    cost = ruStepCost (lvRules level)
    to = stepCell dir (gCell g)
    beyond = stepCell dir to
    refused = g { gFacing = dir, gBump = if isFresh then bumpTicks else gBump g }
    pit = gEra g == Past && beyond `Set.member` lvPits level && beyond `Set.notMember` gFilled g
    freeInPast = openCell level g Past beyond
    freeInPresent = walkable level Present beyond && mirrorAt g Present beyond == Nothing
    moved = Set.delete to (gCrates g)
    mirrorSpec = Map.lookup to (gMirrors g)
    mirrorDestinationOpen = openCell level g (gEra g) beyond
    movedMirrors = case mirrorSpec of
      Just spec -> Map.insert beyond spec (Map.delete to (gMirrors g))
      Nothing   -> gMirrors g
    movedMirrorCells = Set.insert beyond (Set.delete to (gMovableMirrors g))
    mirrorFront = case mirrorSpec of
      Just (_, face) -> gCell g == stepCell face to
      Nothing        -> False
    hinted kind = refused { gFeedback = Just (Feedback kind hintFeedbackTicks) }
    pushMirrorOrRefuse
      | Just (mirrorEra, _) <- mirrorSpec, mirrorEra /= gEra g = refused
      | mirrorFront = if isFresh then hinted MirrorFrontBlocked else refused
      | not isFresh = if mirrorDestinationOpen then hinted MirrorNeedsPress else refused
      | not mirrorDestinationOpen = refused
      | gRemaining g < cost = g { gFacing = dir }
      | otherwise = (walked (charge cost g))
          { gFacing = dir
          , gMotion = Stepping to stepTicks
          , gPushingMirror = Just beyond
          , gMirrors = movedMirrors
          , gMovableMirrors = movedMirrorCells
          }
    pushOrRefuse
      | gEra g == Present && not (ruPushInPresent (lvRules level)) =
          if isFresh then hinted CrateRusted else refused
      | not isFresh = if pit || (freeInPast && freeInPresent) then hinted PushNeedsPress else refused
      | not pit && not freeInPast = refused
      | not pit && not freeInPresent =
          refused { gFeedback = Just (Feedback (PushBlocked beyond) blockedFeedbackTicks) }
      | gRemaining g < cost = g { gFacing = dir }
      | otherwise = (walked (charge cost g))
          { gFacing  = dir
          , gMotion  = Stepping to stepTicks
          , gPushing = Just beyond
          , gCrates  = if pit then moved else Set.insert beyond moved
          , gFilled  = if pit then Set.insert beyond (gFilled g) else gFilled g
          }

-- Words ------------------------------------------------------------------------

data Hint
  = HintLampDark       -- ^ on the lighthouse without the cell
  | HintOnlyFloor      -- ^ on the lighthouse's footing in the past, with the cell
  | HintPlateHeld      -- ^ the player's own weight is what holds the door open
  deriving (Eq, Show)

-- | A hint from where the player stands. Never the route.
currentHint :: Level -> Game -> Maybe Hint
currentHint level g
  | gPhase g /= Playing = Nothing
  | gEra g == Present && here == lvGoal level && not (gHasCell g) = Just HintLampDark
  | gEra g == Past && here == lvGoal level && gHasCell g = Just HintOnlyFloor
  | doorsOpen level (weights g) && not (doorsOpen level (weights g `withoutPlayer` here))
      = Just HintPlateHeld
  | otherwise = Nothing
  where
    here = gCell g
    -- The player's cell, unless a crate or the echo is standing there too.
    withoutPlayer held c
      | c `Set.member` gCrates g || fmap snd (gEcho g) == Just c = held
      | otherwise = Set.delete c held

hintText :: Hint -> String
hintText HintLampDark      = "The lamp is dark. It has nothing to burn."
hintText HintOnlyFloor     = "Only a footing, back then. The lamp stands in the present."
hintText HintPlateHeld     = plateHeldText

objectiveText :: Game -> String
objectiveText g
  | gPhase g == Won = "The lighthouse is lit."
  | gHasCell g = "Bring it to the lighthouse in the present."
  | otherwise  = "Find the power cell in the past."

blockedTitle :: String
blockedTitle = "BLOCKED IN THE OTHER ERA"

-- | The era the player is trying to reach, in words.
blockedDetail :: Era -> String
blockedDetail Past    = "In the past, there is no standing here."
blockedDetail Present = "In the present, there is no standing here."

tooCostlyTitle :: String
tooCostlyTitle = "NOT ENOUGH TIME"

tooCostlyDetail :: Rules -> String
tooCostlyDetail rules = "A shift costs " ++ show (ruShiftCost rules) ++ " seconds."

pushBlockedTitle :: String
pushBlockedTitle = "THE CRATE WILL NOT FIT"

pushBlockedDetail :: String
pushBlockedDetail = "In the present, there is a wall here."

pushAgainText :: String
pushAgainText = "Press the key again to push."

mirrorPushText :: String
mirrorPushText = "Press again to push the mirror from this unlit side."

mirrorFrontText :: String
mirrorFrontText = "The light holds this side. Push the mirror from behind or beside it."

rustedText :: String
rustedText = "Rusted solid here. It only moves in the past."

deadEndText :: String
deadEndText = "This can no longer be won.   Z rewinds   *   R starts over"

peekText :: Era -> String
peekText Past    = "Looking back at the past. Let go of Tab to return."
peekText Present = "Looking ahead to the present. Let go of Tab to return."

pickupText :: String
pickupText = "The power cell travels with you."

echoNeedsStepsText :: String
echoNeedsStepsText = "Walk first. The echo retraces your last steps."

echoInDoorwayText :: String
echoInDoorwayText = "Not from a doorway. The door would close on you."

jumpAgainText :: String
jumpAgainText = "Press the key again to step through the mirror."

-- | Said while the player alone holds every plate down.
plateHeldText :: String
plateHeldText = "The door is open. But you cannot stay here."

victoryText, failureText :: String
victoryText = "You brought the light back"
failureText = "The light stayed out"

