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
  , costLife
  , echoStepTicks
  , echoWalkAt
  , initialFx
  , stepFx
  , waveProgress
  ) where

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
  } deriving (Eq, Show)

initialFx :: Fx
initialFx = Fx 0 Nothing Nothing 0 0 0 0 Nothing Nothing

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
  | restarted = initialFx
  -- Pausing freezes the world's effects, but the fade-in must still finish.
  | gPhase new == Paused = fx { fxPaused = fxPaused fx + 1, fxLevelAge = fxLevelAge fx + 1 }
  | otherwise = fx
      { fxClock    = fxClock fx + 1
      , fxWave     = wave
      , fxBurst    = burst
      , fxCarried  = if gHasCell new then fxCarried fx + 1 else 0
      , fxEnd      = if gPhase new `elem` [Won, Lost] then fxEnd fx + 1 else 0
      , fxPaused   = 0
      , fxLevelAge = fxLevelAge fx + 1
      , fxCost     = cost
      , fxEchoWalk = echoWalk
      }
  where
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
    floatCell = case gMotion new of
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
      | not (gHasCell new) = Nothing
      | not (gHasCell old) = Just (Burst 0 (gCell new))
      | otherwise = case fxBurst fx of
          Just b | burstAge b + 1 < burstLife -> Just b { burstAge = burstAge b + 1 }
          _ -> Nothing
