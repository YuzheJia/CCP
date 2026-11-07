-- | Decorative state: pure, but nothing in the rules reads it.
--
-- It is derived by comparing each tick's game with the one before, so effects
-- cannot disagree with what actually happened. Restarting clears all of it.
module Game.Fx
  ( Fx (..)
  , Wave (..)
  , Burst (..)
  , CostFloat (..)
  , EchoWalk (..)
  , RoomFade (..)
  , roomCols, roomRows
  , roomOf
  , roomedLevel
  , roomFadeTicks
  , roomFadeDark
  , costLife
  , echoStepTicks
  , echoWalkAt
  , initialFx
  , stepFx
  , waveProgress
  ) where

import Data.Maybe (fromMaybe)
import Game.Logic (anyHasCell, carrierCell)
import Game.Types

data Wave = Wave
  { waveAge    :: !Int
  , waveFrom   :: !Era
  , waveOrigin :: !Cell
  } deriving (Eq, Show)

data Burst = Burst
  { burstAge  :: !Int
  , burstCell :: !Cell
  } deriving (Eq, Show)

-- | The seconds an action just spent, or an undo gave back, rising off the player.
data CostFloat = CostFloat
  { cfAge    :: !Int
  , cfAmount :: !Int   -- ^ the change in time left: negative when spent
  , cfCell   :: !Cell
  } deriving (Eq, Show)

-- | The echo walking the footprints back, from the player to where it stays.
data EchoWalk = EchoWalk
  { ewAge  :: !Int
  , ewPath :: ![Spot]  -- ^ the player's spot first, the echo's last
  } deriving (Eq, Show)

-- | Moving to another room: the map darkens, the room changes at the darkest
-- moment, and it lightens again.
data RoomFade = RoomFade
  { rfAge  :: !Int
  , rfRoom :: !Cell  -- ^ the room being moved to
  } deriving (Eq, Show)

data Fx = Fx
  { fxClock    :: !Int           -- ^ ticks of ambient animation
  , fxWave     :: !(Maybe Wave)  -- ^ the era wave crossing the map
  , fxBurst    :: !(Maybe Burst) -- ^ the pickup flash
  , fxCarried  :: !Int           -- ^ ticks since pickup, for the carried light
  , fxEnd      :: !Int           -- ^ ticks since the level was won or lost
  , fxPaused   :: !Int           -- ^ ticks since pause
  , fxLevelAge :: !Int           -- ^ ticks since the level (re)started
  , fxCost     :: !(Maybe CostFloat)
  , fxEchoWalk :: !(Maybe EchoWalk)
  , fxRoom     :: !Cell            -- ^ the room on screen, in rooms, not cells
  , fxRoomFade :: !(Maybe RoomFade)
  , fxVisited  :: ![Cell]          -- ^ rooms seen so far, for the map in the panel
  } deriving (Eq, Show)

initialFx :: Fx
initialFx = Fx 0 Nothing Nothing 0 0 0 0 Nothing Nothing (0, 0) Nothing [(0, 0)]

-- | A level larger than one screen is a grid of rooms this size, and the screen
-- shows one room at a time. A one-room level is always room (0,0).
roomCols, roomRows :: Int
roomCols = 13
roomRows = 9

roomOf :: Cell -> Cell
roomOf (x, y) = (x `div` roomCols, y `div` roomRows)

-- | A level built as a grid of whole rooms is shown a room at a time. Any other
-- size is shown through a camera that follows the keeper instead, and its rooms,
-- which 'stepFx' still tracks, are never drawn.
roomedLevel :: Level -> Bool
roomedLevel level = lvWidth level `mod` roomCols == 0 && lvHeight level `mod` roomRows == 0

-- | The whole fade, out and back in. The room changes halfway.
roomFadeTicks :: Int
roomFadeTicks = 16

-- | How dark the map is during a room change, 0 to 1.
roomFadeDark :: Fx -> Float
roomFadeDark fx = case fxRoomFade fx of
  Nothing -> 0
  Just f ->
    let halfway = fromIntegral roomFadeTicks / 2 :: Float
        a = fromIntegral (rfAge f)
    in if a < halfway then a / halfway else max 0 (2 - a / halfway)

burstLife, costLife, echoStepTicks :: Int
burstLife = 40
costLife = 48
echoStepTicks = 5

-- | Where a walking echo is: the spot it is leaving, the one it is heading for, and
-- how far between them, 0 to 1. Nothing once it has arrived.
echoWalkAt :: EchoWalk -> Maybe (Spot, Spot, Float)
echoWalkAt w = case drop i (ewPath w) of
  from : to : _ -> Just (from, to, fromIntegral r / fromIntegral echoStepTicks)
  _ -> Nothing
  where (i, r) = ewAge w `divMod` echoStepTicks

-- | 0 to 1 as the wave crosses the map. Same length as the input lock.
waveProgress :: Wave -> Float
waveProgress w = fromIntegral (waveAge w) / fromIntegral shiftTicks

stepFx :: Game -> Game -> Fx -> Fx
stepFx old new fx
  -- A restart, or a new level, starts on the player's room with nothing to fade.
  | restarted = initialFx { fxRoom = roomOf (gCell new), fxVisited = [roomOf (gCell new)] }
  -- Pausing freezes the world's effects, but the fade-in must still finish.
  | gPhase new == Paused = fx { fxPaused = fxPaused fx + 1, fxLevelAge = fxLevelAge fx + 1 }
  | otherwise = room fx
      { fxClock    = fxClock fx + 1
      , fxWave     = wave
      , fxBurst    = burst
      , fxCarried  = if anyHasCell new then fxCarried fx + 1 else 0
      , fxEnd      = if gPhase new `elem` [Won, Lost] then fxEnd fx + 1 else 0
      , fxPaused   = 0
      , fxLevelAge = fxLevelAge fx + 1
      , fxCost     = cost
      , fxEchoWalk = echoWalk
      }
  where
    -- The screen follows the room the player is in, or stepping into, so the
    -- change starts as the step does. The room swaps at the darkest moment.
    focus = case gMotion new of
      Stepping to _ -> to
      _             -> gCell new
    wanted = roomOf focus
    room f = case fxRoomFade f of
      Nothing
        | wanted /= fxRoom f -> f { fxRoomFade = Just (RoomFade 0 wanted) }
        | otherwise -> f
      Just rf ->
        let age = rfAge rf + 1
            rf' = rf { rfAge = age, rfRoom = wanted }
            swapped = if age >= roomFadeTicks `div` 2 then rfRoom rf' else fxRoom f
            seen = if swapped `elem` fxVisited f then fxVisited f else fxVisited f ++ [swapped]
        in f { fxRoom = swapped
             , fxVisited = seen
             , fxRoomFade = if age >= roomFadeTicks then Nothing else Just rf'
             }
    -- A new echo walks back along the footprints it was sent down, which are still
    -- in the old game. An undo puts the old echo straight back.
    echoWalk
      | gEcho new /= gEcho old && not undone, Just landing <- gEcho new =
          Just (EchoWalk 0 (walkedTo landing (gTrail old)))
      | gEcho new /= gEcho old = Nothing
      | otherwise = case fxEchoWalk fx of
          Just w | Just _ <- echoWalkAt w -> Just w { ewAge = ewAge w + 1 }
          _ -> Nothing
    walkedTo landing trail = case trail of
      here : older -> here : takeThrough landing older
      [] -> []
    takeThrough x ys = case break (== x) ys of
      (before, found : _) -> before ++ [found]
      (before, []) -> before
    restarted = gAttempt new /= gAttempt old
    -- A step is charged before it moves, but the price belongs where it is going.
    floatCell = case (fmap ptMotion (gPartner old), fmap ptMotion (gPartner new)) of
      (Just Settled, Just (Stepping to _)) -> to  -- the second keeper set off
      _ -> case gMotion new of
        Stepping to _ -> to
        _             -> gCell new
    cost
      | gRemaining new /= gRemaining old =
          Just (CostFloat 0 (gRemaining new - gRemaining old) floatCell)
      | otherwise = case fxCost fx of
          Just c | cfAge c + 1 < costLife -> Just c { cfAge = cfAge c + 1 }
          _ -> Nothing
    -- An undo has no input lock for a wave to cross under, so it cuts straight back.
    undone = length (gHistory new) < length (gHistory old)
    wave
      | gEra new /= gEra old && undone = Nothing
      | gEra new /= gEra old = Just (Wave 0 (gEra old) (gCell new))
      | otherwise = case fxWave fx of
          Just w | waveAge w + 1 < shiftTicks -> Just w { waveAge = waveAge w + 1 }
          _ -> Nothing
    burst
      | not (anyHasCell new) = Nothing
      | not (anyHasCell old) = Just (Burst 0 (fromMaybe (gCell new) (carrierCell new)))
      | otherwise = case fxBurst fx of
          Just b | burstAge b + 1 < burstLife -> Just b { burstAge = burstAge b + 1 }
          _ -> Nothing
