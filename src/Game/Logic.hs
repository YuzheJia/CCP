-- | The pure rules: one fixed tick from one state to the next.
--
-- Order inside a Playing tick, which the tests pin down:
--
--   1. note fresh presses (a press during a step or a wave is remembered, not lost)
--   2. advance the current step or wave; a landing step touches its new cell
--   3. if settled: the pending shift first, then a pending or held move
--   4. spend one tick of the countdown, unless step 2 or 3 already won
module Game.Logic
  ( initialGame
  , stepGame
  , tryShift
  , tryStep
  , Hint (..)
  , currentHint
  , objectiveText
  , hintText
  , blockedTitle
  , blockedDetail
  , victoryText
  , failureText
  , pickupText
  ) where

import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import Game.Level (passageCells, walkable)
import Game.Types

-- | The title screen, with the level ready behind it.
initialGame :: Level -> Game
initialGame level = Game
  { gPhase        = Menu
  , gEra          = Present
  , gCell         = lvSpawn level
  , gHasCell      = False
  , gRemaining    = levelTicks
  , gMotion       = Settled
  , gFacing       = DirDown
  , gPendingShift = False
  , gPendingMove  = Nothing
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
    begin = (initialGame level) { gPhase = Playing }
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
        | otherwise    -> g

-- | Forget anything waiting to happen. Keys held across this need a fresh press.
quiet :: Game -> Game
quiet g = g { gPendingShift = False, gPendingMove = Nothing, gArmed = False }

playTick :: Level -> TickInput -> Game -> Game
playTick level input g0 = spendTick (startNext level input g2)
  where
    freshDir   = if tiDir input /= tiDir (gPrev g0) then tiDir input else Nothing
    freshShift = tiShift input && not (tiShift (gPrev g0))
    locked     = case gMotion g0 of
      Shifting _ -> True
      _          -> False
    g1 = g0
      { gPendingShift = gPendingShift g0 || (freshShift && not locked)
      , gPendingMove  = freshDir <|> gPendingMove g0
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
    | left <= 1 -> touchCell level g { gMotion = Settled, gCell = to }
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
          (quiet g) { gPhase = Won, gMotion = Settled }
      | otherwise = g

-- | Begin whatever is next, if the player is free and the level is still on.
startNext :: Level -> TickInput -> Game -> Game
startNext level input g
  | gPhase g /= Playing || gMotion g /= Settled = g
  | gPendingShift g = startNext level input (tryShift level g)
  | Just dir <- gPendingMove g =
      tryStep level True dir g { gPendingMove = Nothing }
  | Just dir <- tiDir input, gArmed g =
      tryStep level False dir g
  | otherwise = g

-- | Switch era if this cell is open in the other one. Only a settled player can
-- shift; a shift refused here changes nothing but the feedback.
tryShift :: Level -> Game -> Game
tryShift level g
  | gMotion g /= Settled = g
  | walkable level destination (gCell g) =
      touchCell level cleared
        { gEra      = destination
        , gMotion   = Shifting shiftTicks
        , gFeedback = keepPickupNote (gFeedback g)
        }
  | otherwise =
      cleared { gFeedback = Just (Feedback (ShiftBlocked (gCell g)) blockedFeedbackTicks) }
  where
    destination = otherEra (gEra g)
    cleared = g { gPendingShift = False }
    keepPickupNote fb = case fb of
      Just (Feedback (ShiftBlocked _) _) -> Nothing
      other                              -> other

-- | Start one step if the destination is open. The destination is checked here,
-- before anything moves, and never again.
tryStep :: Level -> Bool -> Dir -> Game -> Game
tryStep level isFresh dir g
  | gMotion g /= Settled = g
  | walkable level (gEra g) to =
      g { gFacing = dir, gMotion = Stepping to stepTicks }
  | otherwise =
      g { gFacing = dir, gBump = if isFresh then bumpTicks else gBump g }
  where
    to = stepCell dir (gCell g)

-- | Victory on this tick beats running out on this tick, because the win has
-- already changed the phase by the time the countdown is spent.
spendTick :: Game -> Game
spendTick g = case gPhase g of
  Won -> g { gRemaining = max 0 (gRemaining g - 1) }
  _ | remaining <= 0 -> (quiet g) { gPhase = Lost, gRemaining = 0 }
    | otherwise      -> g { gRemaining = remaining }
  where
    remaining = gRemaining g - 1

-- Words ------------------------------------------------------------------------

data Hint
  = HintDifferentOnce  -- ^ beside the collapsed door, with no idea yet
  | HintLampDark       -- ^ on the lighthouse without the cell
  | HintOnlyFloor      -- ^ on the lighthouse's footing in the past, with the cell
  deriving (Eq, Show)

-- | A hint from where the player stands. Never the route.
currentHint :: Level -> Game -> Maybe Hint
currentHint level g
  | gPhase g /= Playing = Nothing
  | gEra g == Present && here == lvGoal level && not (gHasCell g) = Just HintLampDark
  | gEra g == Past && here == lvGoal level && gHasCell g = Just HintOnlyFloor
  | gEra g == Present && not (gHasCell g) && any near pastDoors = Just HintDifferentOnce
  | otherwise = Nothing
  where
    here@(hx, hy) = gCell g
    pastDoors = [c | (Past, c) <- passageCells level]
    near (x, y) = abs (x - hx) + abs (y - hy) <= 2 && hx < x

hintText :: Hint -> String
hintText HintDifferentOnce = "This place was different once."
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
blockedDetail Past    = "In the past, this spot is a sealed bulkhead."
blockedDetail Present = "In the present, this spot is fallen rubble."

pickupText :: String
pickupText = "The power cell travels with you."

victoryText, failureText :: String
victoryText = "You brought the light back"
failureText = "The light stayed out"

