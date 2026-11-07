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
  , swapKeepers
  , anyHasCell
  , carrierCell
  , landedAt
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
  , mirrorAt
  , opposite
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
  , mirrorFaceText
  , mirrorFixedText
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
  , gTrail        = take (ruEchoLength (lvRules level)) [(Present, lvSpawn level)]
  , gEcho         = Nothing
  , gHistory      = []
  , gPushing      = Nothing
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
  , gPartner      = fmap newPartner (lvSpawn2 level)
  }
  where
    newPartner c = Partner
      { ptCell = c, ptHasCell = False, ptMotion = Settled, ptFacing = DirDown
      , ptPushing = Nothing, ptPendingMove = Nothing, ptArmed = False, ptBump = 0 }

-- Two keepers ------------------------------------------------------------------
--
-- In a level for two, the second keeper plays by exactly the first one's rules:
-- every rule is written for "the player", and the second keeper's turn is taken by
-- swapping the two keepers' places in the game, playing, and swapping back. The era,
-- the crates, the mirrors, the time and the history stay where they are, shared.

-- | Trade places: the second keeper becomes the one the rules speak of.
swapKeepers :: Game -> Game
swapKeepers g = case gPartner g of
  Nothing -> g
  Just p -> g
    { gCell = ptCell p, gHasCell = ptHasCell p, gMotion = ptMotion p, gFacing = ptFacing p
    , gPushing = ptPushing p, gPendingMove = ptPendingMove p, gArmed = ptArmed p
    , gBump = ptBump p
    , gPartner = Just Partner
        { ptCell = gCell g, ptHasCell = gHasCell g, ptMotion = gMotion g
        , ptFacing = gFacing g, ptPushing = gPushing g, ptPendingMove = gPendingMove g
        , ptArmed = gArmed g, ptBump = gBump g }
    }

-- | Run a rule as the second keeper. Anything it charges is remembered from the
-- first keeper's side, like everything else in the history, so undo needs no turns.
asPartner :: (Game -> Game) -> Game -> Game
asPartner f g = case gPartner g of
  Nothing -> g
  Just _ ->
    let g' = swapKeepers (f (swapKeepers g))
    in case gHistory g' of
         s : rest | length (gHistory g') > length (gHistory g) -> g' { gHistory = swapSnapshot s : rest }
         _ -> g'

-- | A snapshot taken as the second keeper, told from the first keeper's side.
swapSnapshot :: Snapshot -> Snapshot
swapSnapshot s = case snPartner s of
  Nothing -> s
  Just p -> s
    { snCell = landedAt p, snHasCell = ptHasCell p, snFacing = ptFacing p
    , snPartner = Just Partner
        { ptCell = snCell s, ptHasCell = snHasCell s, ptMotion = Settled, ptFacing = snFacing s
        , ptPushing = Nothing, ptPendingMove = Nothing, ptArmed = False, ptBump = 0 }
    }

-- | Where a keeper is going to be: an accepted step has already been paid for.
landedAt :: Partner -> Cell
landedAt p = case ptMotion p of
  Stepping to _ -> to
  _ -> ptCell p

-- | A keeper brought to rest where they were headed, with nothing waiting.
settled :: Partner -> Partner
settled p = p { ptCell = landedAt p, ptMotion = Settled, ptPushing = Nothing
              , ptPendingMove = Nothing, ptArmed = False, ptBump = 0 }

-- | The second keeper is still walking, or is in the middle of anything at all.
partnerBusy :: Game -> Bool
partnerBusy g = maybe False ((/= Settled) . ptMotion) (gPartner g)

-- | Whether either keeper is carrying the power cell.
anyHasCell :: Game -> Bool
anyHasCell g = gHasCell g || maybe False ptHasCell (gPartner g)

-- | The cell of whichever keeper carries the power cell.
carrierCell :: Game -> Maybe Cell
carrierCell g
  | gHasCell g = Just (gCell g)
  | otherwise = case gPartner g of
      Just p | ptHasCell p -> Just (ptCell p)
      _ -> Nothing

-- | The keys each keeper reads: W A S D for the first, the arrows for the second.
-- The era, undo and echo keys belong to the first keeper's turn, so they are shared.
keysOf1, keysOf2 :: TickInput -> TickInput
keysOf1 i = i { tiDir = tiDir1 i, tiDirRevealed = tiDir1Revealed i }
keysOf2 i = i { tiDir = tiDir2 i, tiDirRevealed = tiDir2Revealed i
              , tiShift = False, tiUndo = False, tiEcho = False }

-- | One tick for two: both keepers note their keys, both walks advance, then the
-- first keeper's turn (which holds the shared era, undo and echo), then the second.
duoTick :: Level -> TickInput -> Game -> Game
duoTick level input g0 = settleOnWin (asPartner (startMove level in2) started)
  where
    prev = gPrev g0
    in1 = keysOf1 input
    in2 = keysOf2 input
    noted = asPartner (notePresses False (keysOf2 prev) in2) (notePresses True (keysOf1 prev) in1 g0)
    advanced = asPartner (advanceMotion level) (advanceMotion level noted)
    started = startNext level in1 (outOfTime level advanced)
    -- Either keeper may light the lamp; the other stops where they were going.
    settleOnWin g
      | gPhase g == Won = g
          { gCell = case gMotion g of { Stepping to _ -> to; _ -> gCell g }
          , gMotion = Settled, gPushing = Nothing, gPartner = fmap settled (gPartner g) }
      | otherwise = g

-- | The second keeper's turn: only walking, pushing and jumping. It waits while the
-- era is changing, and while a shift or an undo is waiting for both to stand still.
startMove :: Level -> TickInput -> Game -> Game
startMove level input g
  | gPhase g /= Playing || gMotion g /= Settled = g
  | gPendingUndo g || gPendingShift g = g
  | Just p <- gPartner g, Shifting _ <- ptMotion p = g
  | Just dir <- gPendingMove g = tryStep level True dir g { gPendingMove = Nothing }
  | Just dir <- tiDir input, gArmed g = tryStep level False dir g
  | otherwise = g

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
            , gPendingEcho = False, gArmed = False
            , gPartner = fmap (\p -> p { ptPendingMove = Nothing, ptArmed = False }) (gPartner g) }

playTick :: Level -> TickInput -> Game -> Game
playTick level input g0
  | Just _ <- gPartner g0 = duoTick level input g0
  | otherwise = startNext level input (outOfTime level (advanceMotion level (notePresses True (gPrev g0) input g0)))

-- | Step 1 for one keeper: fresh presses are remembered and the nudges decay. The
-- era, undo and echo keys and the feedback are shared, so only the first keeper's
-- turn (@shared@) takes them.
notePresses :: Bool -> TickInput -> TickInput -> Game -> Game
notePresses shared prev input g0
  | shared = walking
      { gPendingShift = gPendingShift g0 || (freshShift && not locked)
      , gPendingUndo  = gPendingUndo g0 || freshUndo
      , gPendingEcho  = gPendingEcho g0 || freshEcho
      , gFeedback     = decayFeedback (gFeedback g0)
      }
  | otherwise = walking
  where
    -- Letting go of a newer key shows the older one still held. That keeps a walk
    -- going, through gArmed, but it is not a press: it must not replace a buffered
    -- tap, and after an undo or a restart it must not start a step that costs time.
    freshDir
      | tiDir input /= tiDir prev && not (tiDirRevealed input) = tiDir input
      | otherwise = Nothing
    freshShift = tiShift input && not (tiShift prev)
    freshUndo  = tiUndo input && not (tiUndo prev)
    freshEcho  = tiEcho input && not (tiEcho prev)
    locked     = case gMotion g0 of
      Shifting _ -> True
      _          -> False
    walking = g0
      { gPendingMove  = freshDir <|> gPendingMove g0
      , gArmed        = isJust (tiDir input) && (gArmed g0 || isJust freshDir)
      , gBump         = max 0 (gBump g0 - 1)
      }

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
    | left <= 1 -> touchCell level g { gMotion = Settled, gCell = to, gPushing = Nothing }
    | otherwise -> g { gMotion = Stepping to (left - 1) }

-- | The one place pickup and victory happen. Runs when a step lands and when an
-- era changes, because both put the player in contact with something new.
touchCell :: Level -> Game -> Game
touchCell level = victory . pickup
  where
    pickup g
      | gEra g == Past && gCell g == lvPowerCell level && not (anyHasCell g) =
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
  -- With two keepers, an undo or a shift waits for the other one's step to land.
  | gPendingUndo g = if partnerBusy g then g else undo level g
  | gPendingEcho g = startNext level input (tryEcho level g)
  | gPendingShift g && partnerBusy g = g
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
  | gPhase g == Playing && gMotion g == Settled && not (partnerBusy g) && not (gPendingUndo g)
    && gRemaining g < ruStepCost (lvRules level) = (quiet g) { gPhase = Lost, gFeedback = Nothing }
  | otherwise = g

-- | Pay for an action, remembering the board as it stood so undo can restore it.
charge :: Int -> Game -> Game
charge cost g = g
  { gRemaining = gRemaining g - cost
  , gHistory = before : gHistory g
  , gFeedback = case gFeedback g of
      -- Advice about pushing has done its job once the player moves.
      Just (Feedback kind _) | kind `elem` [PushNeedsPress, CrateRusted, MirrorFaceSide, MirrorFixed] -> Nothing
      other -> other
  }
  where
    before = Snapshot
      { snEra = gEra g, snCell = gCell g, snHasCell = gHasCell g
      , snCrates = gCrates g, snFilled = gFilled g
      , snFacing = gFacing g, snTrail = gTrail g, snEcho = gEcho g
      , snMirrors = gMirrors g, snCost = cost, snPartner = gPartner g }

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
  (s : rest) -> asPartner pickedUp (pickedUp (restore s rest))
  where
    -- With two keepers, the one not being undone may have picked up the cell since
    -- the snapshot; standing on the cradle again, they hold it again.
    pickedUp h
      | gEra h == Past && gCell h == lvPowerCell level && not (anyHasCell h) = h { gHasCell = True }
      | otherwise = h
    restore s rest =
        let rules = lvRules level
            remaining = gRemaining g + snCost s * ruUndoRefundPercent rules `div` 100
        in (quiet g)
             { gEra = snEra s, gCell = snCell s, gHasCell = snHasCell s
             , gCrates = snCrates s, gFilled = snFilled s, gFacing = snFacing s
             , gTrail = snTrail s, gEcho = snEcho s, gMirrors = snMirrors s
             , gRemaining = remaining, gHistory = rest
             , gMotion = Settled, gPushing = Nothing, gFeedback = Nothing, gBump = 0
             , gPartner = fmap settled (snPartner s)
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
  -- The era is shared: both keepers go, so both need somewhere to stand.
  | Just p <- gPartner g, not (openCell level (swapKeepers g) destination (ptCell p)) =
      refuse (ShiftBlocked (ptCell p))
  | gRemaining g < cost =
      refuse ShiftTooCostly
  | otherwise = asPartner (touchCell level) $
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
-- filled so far, minus wherever the crates are and this era's mirrors. A crate has
-- one position, so it blocks that cell in both eras; a mirror only in its own.
openCell :: Level -> Game -> Era -> Cell -> Bool
openCell level g era cell = clearOfPartner && clearCell level g era cell
  where
    -- Two keepers never share a cell, or the one the other is stepping onto.
    clearOfPartner = case gPartner g of
      Just p -> cell /= ptCell p && cell /= landedAt p
      Nothing -> True

-- | 'openCell' without the other keeper: a keeper does not stop light.
clearCell :: Level -> Game -> Era -> Cell -> Bool
clearCell level g era cell =
  cell `Set.notMember` gCrates g && not (isJust (mirrorAt g era cell)) && (onMap || filled || door)
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
-- a door can never disagree with its plates. Nothing can leave a plate while the
-- player stands in a door: crates only move while the player is in the past, the
-- player is not on a plate while in a door, and the echo cannot be sent from one.
doorsOpen :: Level -> Set.Set Cell -> Bool
doorsOpen level held = not (Set.null plates) && plates `Set.isSubsetOf` held
  where plates = lvPlates level

-- Mirrors ----------------------------------------------------------------------

-- | Whether light crosses this cell in this era: anywhere you could stand, and an
-- open pit. Walls, crates, shut doors and mirrors stop it.
lightPasses :: Level -> Game -> Era -> Cell -> Bool
lightPasses level g era cell =
  clearCell level g era cell || (era == Past && cell `Set.member` lvPits level)

-- | The light a mirror throws out of its face, up to whatever stops it. Only in the
-- mirror's own era.
beamCells :: Level -> Game -> Cell -> [Cell]
beamCells level g mirror = case Map.lookup mirror (gMirrors g) of
  Just m -> takeWhile (lightPasses level g (mEra m)) (drop 1 (iterate (stepCell (mFace m)) mirror))
  Nothing -> []

-- | The mirror standing on this cell in this era, if there is one.
mirrorAt :: Game -> Era -> Cell -> Maybe Mirror
mirrorAt g era cell = case Map.lookup cell (gMirrors g) of
  Just m | mEra m == era -> Just m
  _ -> Nothing

opposite :: Dir -> Dir
opposite d = case d of
  DirUp -> DirDown
  DirDown -> DirUp
  DirLeft -> DirRight
  DirRight -> DirLeft

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
          Just m | mFace m == opposite dir ->
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
weights g = Set.insert (gCell g) (maybe id (Set.insert . ptCell) (gPartner g)
                                    (maybe id (Set.insert . snd) (gEcho g) (gCrates g)))

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
-- is the exception, because the crate is used up there and nothing is left standing.
tryStep :: Level -> Bool -> Dir -> Game -> Game
tryStep level isFresh dir g
  | gMotion g /= Settled = g
  | Just target <- jumpTarget level g dir = jumpTo target
  | Just m <- mirrorAt g (gEra g) to = pushMirror m
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
    pit = beyond `Set.member` lvPits level && beyond `Set.notMember` gFilled g
    freeInPast = openCell level g Past beyond
    freeInPresent = walkable level Present beyond && not (isJust (mirrorAt g Present beyond))
    moved = Set.delete to (gCrates g)
    hinted kind = refused { gFeedback = Just (Feedback kind hintFeedbackTicks) }
    pushOrRefuse
      | gEra g /= Past = if isFresh then hinted CrateRusted else refused
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
    -- A mirror is a door from its face and a crate from its back and sides. Pressing
    -- into its face only ever jumps, which was tried above, so here it explains why
    -- nothing happened. It moves in its own era, onto floor open there: never into a
    -- pit or a door, open or shut, like a crate.
    pushMirror m
      | not (mMovable m) = if isFresh then hinted MirrorFixed else refused
      | dir == opposite (mFace m) = if isFresh then hinted MirrorFaceSide else refused
      | not mirrorRoom = refused
      | not isFresh = hinted PushNeedsPress
      | gRemaining g < cost = g { gFacing = dir }
      | otherwise = (walked (charge cost g))
          { gFacing  = dir
          , gMotion  = Stepping to stepTicks
          , gPushing = Just beyond
          , gMirrors = Map.insert beyond m (Map.delete to (gMirrors g))
          }
    -- Mirrors never share a cell, even across eras: one cell holds one mirror.
    mirrorRoom = walkable level (gEra g) beyond && openCell level g (gEra g) beyond
                 && beyond `Map.notMember` gMirrors g

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
  | anyHasCell g = "Bring it to the lighthouse in the present."
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
pushAgainText = "Press the key again to push the crate."

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

mirrorFaceText :: String
mirrorFaceText = "A mirror's face only takes you through. Push it from the back or sides."

mirrorFixedText :: String
mirrorFixedText = "This mirror is set in the wall. It never moves."

-- | Said while the player alone holds every plate down.
plateHeldText :: String
plateHeldText = "The door is open. But you cannot stay here."

victoryText, failureText :: String
victoryText = "You brought the light back"
failureText = "The light stayed out"

