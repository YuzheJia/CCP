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
  , openCell
  , doorsOpen
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
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (isJust)
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
  , gHistory      = []
  , gPushing      = Nothing
  , gAttempt      = 0
  , gMotion       = Settled
  , gFacing       = DirDown
  , gPendingShift = False
  , gPendingMove  = Nothing
  , gPendingUndo  = False
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
quiet g = g { gPendingShift = False, gPendingMove = Nothing, gPendingUndo = False, gArmed = False }

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
    locked     = case gMotion g0 of
      Shifting _ -> True
      _          -> False
    g1 = g0
      { gPendingShift = gPendingShift g0 || (freshShift && not locked)
      , gPendingMove  = freshDir <|> gPendingMove g0
      , gPendingUndo  = gPendingUndo g0 || freshUndo
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
    | left <= 1 -> touchCell level g { gMotion = Settled, gCell = to, gPushing = Nothing }
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
      Just (Feedback kind _) | kind `elem` [PushNeedsPress, CrateRusted] -> Nothing
      other -> other
  }
  where
    before = Snapshot
      { snEra = gEra g, snCell = gCell g, snHasCell = gHasCell g
      , snCrates = gCrates g, snFilled = gFilled g
      , snFacing = gFacing g, snCost = cost }

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
         , gRemaining = remaining, gHistory = rest
         , gMotion = Settled, gPushing = Nothing, gFeedback = Nothing, gBump = 0
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
      touchCell level (charge cost cleared)
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
openCell level g era cell = cell `Set.notMember` gCrates g && (onMap || filled || door)
  where
    onMap = walkable level era cell
    filled = era == Past && cell `Set.member` gFilled g
    door = era == Present && cell `Set.member` lvDoors level && doorsOpen level (gCrates g)

-- | Every cell the player could switch era on right now: open in both eras, given
-- where the crates, filled pits and open doors are. Worked out from the map and the
-- board, so it can be drawn but never has to be kept up to date.
crossings :: Level -> Game -> [Cell]
crossings level g =
  [ c | y <- [0 .. lvHeight level - 1], x <- [0 .. lvWidth level - 1], let c = (x, y)
      , openCell level g Past c, openCell level g Present c ]

-- | The doors open while a crate rests on every plate. This is worked out from where
-- the crates are, not stored, so undo, restart and the solver need nothing new, and a
-- door can never disagree with its plates. A crate only moves while the player is in
-- the past, so no door can shut on someone standing in it.
doorsOpen :: Level -> Set.Set Cell -> Bool
doorsOpen level crates = not (Set.null plates) && plates `Set.isSubsetOf` crates
  where plates = lvPlates level

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
  | to `Set.member` gCrates g = pushOrRefuse
  | not (openCell level g (gEra g) to) = refused
  | gRemaining g < cost = g { gFacing = dir }
  | otherwise = (charge cost g) { gFacing = dir, gMotion = Stepping to stepTicks }
  where
    cost = ruStepCost (lvRules level)
    to = stepCell dir (gCell g)
    beyond = stepCell dir to
    refused = g { gFacing = dir, gBump = if isFresh then bumpTicks else gBump g }
    pit = beyond `Set.member` lvPits level && beyond `Set.notMember` gFilled g
    freeInPast = openCell level g Past beyond
    freeInPresent = walkable level Present beyond
    moved = Set.delete to (gCrates g)
    hinted kind = refused { gFeedback = Just (Feedback kind hintFeedbackTicks) }
    pushOrRefuse
      | gEra g /= Past = if isFresh then hinted CrateRusted else refused
      | not isFresh = if pit || (freeInPast && freeInPresent) then hinted PushNeedsPress else refused
      | not pit && not freeInPast = refused
      | not pit && not freeInPresent =
          refused { gFeedback = Just (Feedback (PushBlocked beyond) blockedFeedbackTicks) }
      | gRemaining g < cost = g { gFacing = dir }
      | otherwise = (charge cost g)
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
  deriving (Eq, Show)

-- | A hint from where the player stands. Never the route.
currentHint :: Level -> Game -> Maybe Hint
currentHint level g
  | gPhase g /= Playing = Nothing
  | gEra g == Present && here == lvGoal level && not (gHasCell g) = Just HintLampDark
  | gEra g == Past && here == lvGoal level && gHasCell g = Just HintOnlyFloor
  | otherwise = Nothing
  where
    here = gCell g

hintText :: Hint -> String
hintText HintLampDark      = "The lamp is dark. It has nothing to burn."
hintText HintOnlyFloor     = "Only a footing, back then. The lamp stands in the present."

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

victoryText, failureText :: String
victoryText = "You brought the light back"
failureText = "The light stayed out"

