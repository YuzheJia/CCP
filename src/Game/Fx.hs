-- | Decorative state: pure, but nothing in the rules reads it.
--
-- It is derived by comparing each tick's game with the one before, so effects
-- cannot disagree with what actually happened. Restarting clears all of it.
module Game.Fx
  ( Fx (..)
  , Wave (..)
  , Burst (..)
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

data Fx = Fx
  { fxClock    :: !Int           -- ^ ticks of ambient animation
  , fxWave     :: !(Maybe Wave)  -- ^ the era wave crossing the map
  , fxBurst    :: !(Maybe Burst) -- ^ the pickup flash
  , fxCarried  :: !Int           -- ^ ticks since pickup, for the carried light
  , fxEnd      :: !Int           -- ^ ticks since the level was won or lost
  , fxPaused   :: !Int           -- ^ ticks since pause
  , fxLevelAge :: !Int           -- ^ ticks since the level (re)started
  } deriving (Eq, Show)

initialFx :: Fx
initialFx = Fx 0 Nothing Nothing 0 0 0 0

burstLife :: Int
burstLife = 40

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
      }
  where
    restarted = gPhase new == Playing && gRemaining new == levelTicks
                && (gPhase old /= Playing || gRemaining old /= levelTicks)
    wave
      | gEra new /= gEra old = Just (Wave 0 (gEra old) (gCell new))
      | otherwise = case fxWave fx of
          Just w | waveAge w + 1 < shiftTicks -> Just w { waveAge = waveAge w + 1 }
          _ -> Nothing
    burst
      | gHasCell new && not (gHasCell old) = Just (Burst 0 (gCell new))
      | otherwise = case fxBurst fx of
          Just b | burstAge b + 1 < burstLife -> Just b { burstAge = burstAge b + 1 }
          _ -> Nothing
