{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- | Every sound in the game, made from oscillators, noise and filters at startup.
-- Like the art, nothing is loaded from a file.
--
-- A sound is an array of samples at 'sampleRate', mono, roughly -1 to 1. Each
-- one-shot is normalised, so 'Audio' sets how loud it plays; each bed loops without
-- a seam.
module Synth
  ( Sound (..)
  , Signal
  , sampleRate
  , render
  , isBed
  , isEnding
  , len
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray, runSTUArray)
import Data.Array.Unboxed (UArray, amap, bounds)
import Data.Bits (shiftL, shiftR, xor)
import Data.Word (Word32)
import Game.Run (lampDiesAt)
import Game.Types (ticksPerSecond)

data Sound
  -- In a level
  = StepPast | StepPresent | Push | MirrorPush | PitFill | Jump | JumpTurn
  | ShiftPast | ShiftPresent | Pickup | Lamp | Lose | Bump | Refused
  | Undo | Restart | Room | PauseOn | PauseOff
  -- The title and the hall
  | MenuMove | MenuEnter | MenuBack
  -- The ending
  | Flare | LampDies | LightGoes | WallBreak | RushBegins | Barrier | BarrierPassed
  | Caught | LampCatch | Thunder | Arrival | Card
  -- Beds, which loop
  | BedSea | BedStorm | BedBeam | BedDrone | BedWind | BedPulse
  deriving (Eq, Ord, Show, Enum, Bounded)

isBed :: Sound -> Bool
isBed s = s >= BedSea

-- | Heard only in level 3's ending.
isEnding :: Sound -> Bool
isEnding s = (s >= Flare && s <= Card) || s `elem` [BedBeam, BedDrone, BedWind, BedPulse]

sampleRate :: Int
sampleRate = 44100

type Signal = UArray Int Float

sr :: Float
sr = fromIntegral sampleRate

-- Building blocks ----------------------------------------------------------------
--
-- Each takes whole signals and gives a new one, so a sound reads as a recipe. Inside,
-- each is one strict loop over an unboxed array.

samplesIn :: Float -> Int
samplesIn d = max 0 (round (d * sr))

len :: Signal -> Int
len s = let (_, hi) = bounds s in hi + 1

-- | @n@ samples of silence, to write into.
newSignal :: Int -> ST s (STUArray s Int Float)
newSignal n = newArray (0, n - 1) 0

-- | A signal of @n@ samples, built one sample at a time, each from those before.
build :: Int -> (STUArray s Int Float -> Int -> ST s Float) -> ST s (STUArray s Int Float)
build n f = do
  a <- newSignal n
  forM_ [0 .. n - 1] $ \i -> f a i >>= unsafeWrite a i
  pure a

generate :: Int -> (Int -> Float) -> Signal
generate n f = runSTUArray (build n (\_ i -> pure (f i)))

at :: Signal -> Int -> Float
at s i = if i >= 0 && i < len s then unsafeAt s i else 0

-- | An oscillator whose frequency may change over time. The wave reads a phase 0 to 1.
osc :: (Float -> Float) -> (Float -> Float) -> Float -> Signal
osc wave freq d = runSTUArray $ do
  let n = samplesIn d
  a <- newSignal n
  let go !i !p = when (i < n) $ do
        let q0 = p + freq (fromIntegral i / sr) / sr
            q = q0 - fromIntegral (floor q0 :: Int)
        unsafeWrite a i (wave q)
        go (i + 1) q
  go 0 0
  pure a

sine, tri :: Float -> Float
sine p = sin (2 * pi * p)
tri p = 4 * abs (p - 0.5) - 1

tone :: (Float -> Float) -> Float -> Float -> Signal
tone wave f = osc wave (const f)

-- | White noise, -1 to 1, the same every run for one seed.
noise :: Int -> Float -> Signal
noise seed d = runSTUArray $ do
  let n = samplesIn d
  a <- newSignal n
  let go !i !x = when (i < n) $ do
        let x1 = x `xor` (x `shiftL` 13)
            x2 = x1 `xor` (x1 `shiftR` 17)
            x3 = x2 `xor` (x2 `shiftL` 5) :: Word32
        unsafeWrite a i (fromIntegral x3 / 2147483648 - 1)
        go (i + 1) x3
  go 0 (fromIntegral seed * 2654435761 + 1)
  pure a

-- | Multiply by an envelope of time. The last 5 ms always fade to silence, so a
-- part that stops before its envelope has died away never ends on a click.
env :: (Float -> Float) -> Signal -> Signal
env f s = generate n (\i -> f (fromIntegral i / sr) * unsafeAt s i * min 1 (fromIntegral (n - 1 - i) / tailN))
  where
    n = len s
    tailN = fromIntegral (max 1 (samplesIn 0.005))

-- | Rises linearly over @a@ seconds, then decays by e every @d@ seconds.
perc :: Float -> Float -> Float -> Float
perc a d t
  | t < a = t / a
  | otherwise = exp (negate (t - a) / d)

-- | A one-pole low-pass with a cutoff that may change over time.
lowpass :: (Float -> Float) -> Signal -> Signal
lowpass fc s = runSTUArray $ do
  let n = len s
  a <- newSignal n
  let go !i !y = when (i < n) $ do
        let k = 1 - exp (-2 * pi * fc (fromIntegral i / sr) / sr)
            y' = y + k * (unsafeAt s i - y)
        unsafeWrite a i y'
        go (i + 1) y'
  go 0 0
  pure a

highpass :: (Float -> Float) -> Signal -> Signal
highpass fc s = let l = lowpass fc s in generate (len s) (\i -> unsafeAt s i - unsafeAt l i)

-- | A resonant band-pass (a state-variable filter) with a moving centre.
bandpass :: Float -> (Float -> Float) -> Signal -> Signal
bandpass q fc s = runSTUArray $ do
  let n = len s
  a <- newSignal n
  let go !i !low !band = when (i < n) $ do
        let f = 2 * sin (pi * min 8000 (fc (fromIntegral i / sr)) / sr)
            low' = low + f * band
            high = unsafeAt s i - low' - band / q
            band' = band + f * high
        unsafeWrite a i band'
        go (i + 1) low' band'
  go 0 0 0
  pure a

gain :: Float -> Signal -> Signal
gain k = amap (* k)

-- | Starts @d@ seconds late.
after :: Float -> Signal -> Signal
after d s = let m = samplesIn d in generate (len s + m) (\i -> if i < m then 0 else unsafeAt s (i - m))

-- | The first @n@ samples, or silence after the end.
cut :: Int -> Signal -> Signal
cut n s = generate n (at s)

-- | Everything at once: as long as the longest.
mix :: [Signal] -> Signal
mix ss = runSTUArray $ do
  let n = maximum (0 : map len ss)
  a <- newSignal n
  forM_ ss $ \s -> forM_ [0 .. len s - 1] $ \i -> do
    v <- unsafeRead a i
    unsafeWrite a i (v + unsafeAt s i)
  pure a

-- | The last few milliseconds faded out, so a sound never ends on a click.
fadeEnd :: Signal -> Signal
fadeEnd s = generate n (\i -> unsafeAt s i * min 1 (fromIntegral (n - 1 - i) / fromIntegral f))
  where
    n = len s
    f = max 1 (min (n `div` 4) (samplesIn 0.012))

-- | A struck bell: inharmonic partials, the higher ones dying first. It rings for
-- about @d@ seconds.
bell :: Float -> Float -> Signal
bell f d = mix
  [ gain g (env (perc 0.002 (d * k / 4)) (tone sine (f * r) d))
  | (r, g, k) <- [(1, 1, 1), (2.0, 0.55, 0.6), (2.76, 0.4, 0.45), (5.4, 0.22, 0.25), (8.93, 0.1, 0.15)] ]

-- | A pitch-dropping thump, the body of an impact.
thump :: Float -> Float -> Float -> Signal
thump from to d = env (perc 0.003 (d / 4)) (osc sine (\t -> to + (from - to) * exp (negate t / (d / 6))) d)

-- | A soft pad note: detuned saws through a low-pass.
pad :: Float -> Float -> Float -> Signal
pad f cutoff d = lowpass (const cutoff) (gain 0.6 (mix [ bsawTone (f * k) d | k <- [0.996, 1, 1.0045] ]))

-- | A filtered-feedback delay, for the reverb.
comb :: Int -> Float -> Float -> Signal -> Signal
comb n g damp s = runSTUArray $ do
  let m = len s
  a <- newSignal m
  let go !i !fed = when (i < m) $ do
        back <- if i >= n then unsafeRead a (i - n) else pure 0
        let fed' = fed + damp * (back - fed)
        unsafeWrite a i (unsafeAt s i + g * fed')
        go (i + 1) fed'
  go 0 0
  pure a

allpass :: Int -> Float -> Signal -> Signal
allpass n g s = runSTUArray $ do
  let m = len s
  a <- newSignal m
  forM_ [0 .. m - 1] $ \i -> do
    back <- if i >= n then unsafeRead a (i - n) else pure 0
    unsafeWrite a i (negate g * unsafeAt s i + at s (i - n) + g * back)
  pure a

-- | A hall in stereo, after Freeverb: eight damped combs and four all-passes a side,
-- the right side's delays a little longer, so the two sides ring differently and
-- the sound has width. @fb@ sets how long it rings, @wet@ how much of it is heard.
stereoHall :: Float -> Float -> Float -> Signal -> (Signal, Signal)
stereoHall fb wet tailSecs dry = (side 0, side 23)
  where
    padded = cut (len dry + samplesIn tailSecs) dry
    side spread = mix [padded, gain wet (foldr (\n -> allpass (n + spread) 0.5) (gain 0.125 (mix
      [ comb (n + spread) fb 0.35 padded | n <- [1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617] ]))
      [225, 341, 441, 556])]

-- | Pink noise (Paul Kellet's filter): the soft noise of wind and water, without
-- white noise's hiss.
pink :: Int -> Float -> Signal
pink seed d = runSTUArray $ do
  let w = noise seed d
      n = len w
  a <- newSignal n
  let go !i !b0 !b1 !b2 = when (i < n) $ do
        let x = unsafeAt w i
            b0' = 0.99765 * b0 + x * 0.0990460
            b1' = 0.96300 * b1 + x * 0.2965164
            b2' = 0.57000 * b2 + x * 1.0526913
        unsafeWrite a i ((b0' + b1' + b2' + x * 0.1848) * 0.3)
        go (i + 1) b0' b1' b2'
  go 0 0 0 0
  pure a

-- | Brown noise: a slow random walk, all rumble and swell and no hiss.
brown :: Int -> Float -> Signal
brown seed d = runSTUArray $ do
  let w = noise seed d
      n = len w
  a <- newSignal n
  let go !i !y = when (i < n) $ do
        let y' = 0.998 * y + 0.02 * unsafeAt w i
        unsafeWrite a i (y' * 4)
        go (i + 1) y'
  go 0 0
  pure a

-- | A sawtooth without aliasing (PolyBLEP): the plain one buzzes harshly up high.
bsaw :: (Float -> Float) -> Float -> Signal
bsaw freq d = runSTUArray $ do
  let n = samplesIn d
  a <- newSignal n
  let go !i !p = when (i < n) $ do
        let dt = min 0.5 (freq (fromIntegral i / sr) / sr)
            q0 = p + dt
            q = q0 - fromIntegral (floor q0 :: Int)
            blep
              | q < dt = let x = q / dt in x + x - x * x - 1
              | q > 1 - dt = let x = (q - 1) / dt in x * x + x + x + 1
              | otherwise = 0
        unsafeWrite a i (2 * q - 1 - blep)
        go (i + 1) q
  go 0 0
  pure a

bsawTone :: Float -> Float -> Signal
bsawTone f = bsaw (const f)

-- | A bed that loops without a seam: render @len + overlap@ and fold the overlap back.
loopOf :: Float -> Float -> (Float -> Signal) -> Signal
loopOf seconds overlap make = generate n blend
  where
    xs = make (seconds + overlap)
    n = samplesIn seconds
    o = samplesIn overlap
    blend i
      | i < o = let k = fromIntegral i / fromIntegral o in at xs i * sqrt k + at xs (i + n) * sqrt (1 - k)
      | otherwise = at xs i

-- | Semitones as a frequency ratio.
semis :: Float -> Float
semis s = 2 ** (s / 12)

a2, a3, a4 :: Float
a2 = 110
a3 = 220
a4 = 440

-- The sounds ---------------------------------------------------------------------
--
-- Each one-shot is a dry recipe and the room it is heard in. The room is a stereo
-- hall: small for footsteps and knocks, large for the lamp and the ending. Breath,
-- air and water are pink or brown noise, never raw white noise, which hisses.

-- | A recipe and its room: how long the room rings, how much of it, and for how
-- long after the sound ends.
data Shot = Shot !Float !Float !Float Signal

-- | The station's own rooms: plaster and stone, a short bright ring.
room :: Signal -> Shot
room = Shot 0.72 0.16 0.35

-- | A larger, longer space, for what should feel big.
hall :: Float -> Float -> Float -> Signal -> Shot
hall = Shot

-- | Both sides of a sound, left and right.
render :: Sound -> (Signal, Signal)
render s
  | isBed s = renderBed s
  | otherwise =
      let Shot fb wet tl dry = oneShot s
          -- A gentle top cut takes the glassy edge off everything synthesised.
          (l, r) = stereoHall fb wet tl (lowpass (const 11000) dry)
          k = 0.9 / max 1e-6 (max (peakOf l) (peakOf r))
      in (fadeEnd (gain k l), fadeEnd (gain k r))

peakOf :: Signal -> Float
peakOf s = foldr (\i acc -> max acc (abs (unsafeAt s i))) 0 [0 .. len s - 1]

-- | Two gentle low-passes: a buzz with its edge taken off.
dull :: Signal -> Signal
dull = lowpass (const 650) . lowpass (const 650)

-- | A footfall: the heel, then the toe a moment later and softer.
footfall :: Int -> Float -> Float -> Float -> Signal
footfall seed body click knock = mix
  [ gain 0.9 (thump (body * 1.9) body 0.08)
  , gain 0.35 (env (perc 0.001 0.012) (lowpass (const click) (pink seed 0.05)))
  , gain 0.25 (env (perc 0.001 0.03) (bandpass 6 (const knock) (pink (seed + 1) 0.08)))
  , after 0.045 (mix
      [ gain 0.4 (thump (body * 2.2) (body * 1.2) 0.05)
      , gain 0.18 (env (perc 0.001 0.01) (lowpass (const click) (pink (seed + 2) 0.04))) ]) ]

oneShot :: Sound -> Shot
oneShot s = case s of
  -- A soft step on boards and plaster: a hollow wooden knock under it.
  StepPast -> Shot 0.7 0.12 0.25 (footfall 1 72 1600 420)
  -- Stone, with the sea standing in puddles on it: a little splash after.
  StepPresent -> Shot 0.7 0.12 0.25 (mix
    [ footfall 4 62 2200 900
    , gain 0.22 (after 0.008 (env (perc 0.006 0.04) (bandpass 0.8 (\t -> 3200 - 9000 * t) (pink 3 0.14)))) ])
  -- A crate dragged a cell over the floor, juddering, then settling.
  Push -> room (mix
    [ env (\t -> perc 0.02 0.5 t * (0.65 + 0.35 * sin (t * 2 * pi * 31)) * (if t < 0.14 then 1 else exp (negate (t - 0.14) / 0.02)))
        (lowpass (const 800) (pink 4 0.2))
    , gain 0.3 (env (\t -> if t < 0.14 then 1 else exp (negate (t - 0.14) / 0.02)) (bandpass 8 (const 180) (pink 40 0.2)))
    , gain 0.8 (after 0.12 (thump 110 50 0.12)) ])
  -- The sled scrapes, and the glass on it rings.
  MirrorPush -> room (mix
    [ gain 0.8 (env (\t -> perc 0.02 0.5 t * (if t < 0.14 then 1 else exp (negate (t - 0.14) / 0.02))) (bandpass 2 (const 900) (pink 5 0.2)))
    , gain 0.22 (after 0.03 (bell 1760 0.5))
    , gain 0.13 (after 0.05 (bell 2349 0.4)) ])
  -- A crate drops into a pit: a deep hit and stones settling.
  PitFill -> hall 0.8 0.25 0.6 (mix
    [ thump 90 32 0.5
    , gain 0.6 (env (perc 0.005 0.15) (lowpass (const 900) (pink 6 0.6)))
    , gain 0.25 (after 0.08 (grains 7 0.4 1800)) ])
  -- Through the glass: a shimmer rising into a clear ring, as if passing through it.
  Jump -> hall 0.84 0.32 0.9 (mix
    [ gain 0.5 (env (\t -> (t / 0.2) ** 2 * (if t < 0.2 then 1 else 0)) (mix
        [ osc sine (\t -> 500 * k * (1 + 3 * t / 0.2)) 0.2 | k <- [1, 1.5, 2.02] ]))
    , gain 0.3 (env (\t -> if t < 0.2 then t / 0.2 else exp (negate (t - 0.2) / 0.08)) (bandpass 2 (\t -> 1200 + 4000 * min 1 t) (pink 8 0.5)))
    , gain 0.9 (after 0.19 (bell 1318.5 0.9))
    , gain 0.45 (after 0.21 (bell 1975.5 0.7)) ])
  -- Through a corner mirror: the shimmer rises to the glass, rings, and is turned,
  -- setting off again a fourth higher before its own, brighter ring.
  JumpTurn -> hall 0.84 0.32 0.9 (mix
    [ gain 0.45 (env (\t -> (t / 0.13) ** 2 * (if t < 0.13 then 1 else 0)) (mix
        [ osc sine (\t -> 500 * k * (1 + 1.6 * t / 0.13)) 0.13 | k <- [1, 1.5, 2.02] ]))
    , gain 0.6 (after 0.12 (bell 1318.5 0.45))
    , gain 0.4 (after 0.14 (env (\t -> sin (pi * min 1 (t / 0.13)) ** 2) (mix
        [ osc sine (\t -> 667 * k * (1 + 1.6 * t / 0.13)) 0.13 | k <- [1, 1.5, 2.02] ])))
    , gain 0.25 (env (\t -> if t < 0.27 then t / 0.27 else exp (negate (t - 0.27) / 0.08)) (bandpass 2 (\t -> 1200 + 4000 * min 1 (2 * t)) (pink 32 0.55)))
    , gain 0.9 (after 0.26 (bell 1760 0.9))
    , gain 0.45 (after 0.28 (bell 2637 0.7)) ])
  -- Into the past: a wave falling, warm. Into the present: rising, colder.
  ShiftPast -> shiftSound True
  ShiftPresent -> shiftSound False
  -- The power cell: a quick bright arpeggio over its hum.
  Pickup -> hall 0.85 0.35 1.0 (mix
    ( gain 0.3 (env (\t -> perc 0.05 0.3 t) (mix [ tone tri 110 0.7, gain 0.5 (tone sine 220 0.7) ]))
    : [ gain 0.7 (after (0.065 * fromIntegral i) (bell (a4 * 2 * semis st) 0.8))
      | (i, st) <- zip [0 :: Int ..] [0, 4, 7, 12] ] ))
  -- The lamp is lit: a warm chord swelling, a bell over it.
  Lamp -> hall 0.9 0.4 2.2 (mix
    [ env (\t -> min 1 (t / 0.25) * exp (negate (max 0 (t - 0.4)) / 1.1)) (mix
        [ gain g (pad (a2 * semis st) 1600 3.2) | (st, g) <- [(0, 0.5), (7, 0.35), (12, 0.35), (16, 0.3), (19, 0.25), (26, 0.12)] ])
    , gain 0.8 (bell (a4 * 2) 2.6)
    , gain 0.45 (after 0.18 (bell (a4 * 2 * semis 7) 2.2)) ])
  -- Out of time: three notes falling, dimming.
  Lose -> hall 0.86 0.35 1.6 (mix
    [ gain 0.7 (after (0.3 * fromIntegral i) (env (perc 0.02 0.5) (pad (a3 * semis st) 1100 1.2)))
    | (i, st) <- zip [0 :: Int ..] [0, -2, -5] ])
  -- Walked into something solid.
  Bump -> room (mix [ thump 95 55 0.11, gain 0.25 (env (perc 0.001 0.01) (lowpass (const 2000) (pink 9 0.03))) ])
  -- A shift the station will not allow: a dull double buzz.
  Refused -> room (mix
    [ env (perc 0.005 0.03) (dull (bsawTone 155 0.1))
    , after 0.11 (env (perc 0.005 0.06) (dull (bsawTone 130 0.18))) ])
  -- A tape run backwards.
  Undo -> room (env (\t -> sin (pi * t / 0.18)) (mix
    [ osc sine (\t -> 900 - 3000 * t) 0.18, gain 0.35 (bandpass 2 (\t -> 2500 - 10000 * t) (pink 10 0.18)) ]))
  Restart -> room (env (\t -> min 1 (t / 0.05) * exp (negate t / 0.15)) (mix
    [ osc sine (\t -> 700 - 1200 * t) 0.4, gain 0.45 (bandpass 1.5 (\t -> 3000 * exp (negate t * 5)) (pink 11 0.4)) ]))
  -- Through a doorway into the next room: air moving.
  Room -> room (env (\t -> sin (pi * t / 0.35)) (bandpass 1.2 (\t -> 400 + 900 * t) (pink 12 0.35)))
  PauseOn -> room (mix [ env (perc 0.003 0.08) (tone sine 660 0.25), after 0.07 (env (perc 0.003 0.1) (tone sine 440 0.3)) ])
  PauseOff -> room (mix [ env (perc 0.003 0.08) (tone sine 440 0.25), after 0.07 (env (perc 0.003 0.1) (tone sine 660 0.3)) ])
  MenuMove -> Shot 0.7 0.1 0.2 (env (perc 0.001 0.03) (mix [ tone sine 1500 0.1, gain 0.3 (tone tri 1500 0.1) ]))
  MenuEnter -> hall 0.8 0.28 0.7 (mix [ gain 0.8 (bell 880 0.6), after 0.08 (bell 1318.5 0.8) ])
  MenuBack -> room (mix [ gain 0.8 (bell 1318.5 0.4), after 0.07 (gain 0.8 (bell 880 0.5)) ])

  -- The ending.
  -- The flare: everything rising to the white, cut at its height.
  Flare -> let d = fromIntegral lampDiesAt / fromIntegral ticksPerSecond in
    -- It opens up as it rises: dark at first, full only at the very top.
    Shot 0.7 0.1 0.1 (env (\t -> (t / d) ** 2.2) (lowpass (\t -> 900 + 6000 * (t / d) ** 2) (mix
      [ gain 0.6 (bandpass 1.5 (\t -> 400 * 8 ** (t / d)) (pink 13 d))
      , gain 0.5 (mix [ bsaw (\t -> f * 2 ** (2 * t / d)) d | f <- [220, 277.2, 329.6] ]) ])))
  -- The lamp dies: a blow, glass breaking, and the power running down into the dark.
  LampDies -> hall 0.93 0.5 3.2 (mix
    [ thump 75 26 1.2
    , gain 0.8 (env (perc 0.001 0.18) (lowpass (const 1800) (pink 14 0.8)))
    , gain 0.3 (env (perc 0.001 0.08) (highpass (const 3500) (noise 15 0.3)))
    , gain 0.35 (grains 16 0.5 5000)
    , gain 0.3 (after 0.02 (mix [ after (0.03 * fromIntegral i) (bell f 0.6) | (i, f) <- zip [0 :: Int ..] [3520, 2960, 4186, 3135] ]))
    , gain 0.5 (after 0.1 (env (\t -> exp (negate t / 0.7)) (lowpass (const 1200) (bsaw (\t -> 40 + 560 * exp (negate t / 0.35)) 2.0)))) ])
  -- Its light leaves through the wall: a thin ring travelling off.
  LightGoes -> hall 0.88 0.42 1.4 (env (\t -> perc 0.03 0.5 t) (mix
    [ osc sine (\t -> 660 + 600 * min 1 (t / 0.3)) 1.2
    , gain 0.6 (osc sine (\t -> 990 + 900 * min 1 (t / 0.3)) 1.2)
    , gain 0.2 (bandpass 3 (\t -> 1800 + 2500 * t) (pink 17 1.2)) ]))
  -- A wall breaks open: the blow and the rubble falling.
  WallBreak -> hall 0.84 0.3 0.8 (mix
    [ thump 100 38 0.45
    , gain 0.7 (env (perc 0.002 0.1) (lowpass (const 1400) (pink 18 0.5)))
    , gain 0.45 (after 0.03 (grains 19 0.55 1800))
    , gain 0.15 (env (perc 0.001 0.03) (highpass (const 2500) (noise 20 0.08))) ])
  -- Swept away: a rush of air opening up.
  RushBegins -> hall 0.8 0.2 0.6 (mix
    [ env (\t -> min 1 (t / 0.5) ** 2 * exp (negate (max 0 (t - 0.6)) / 0.4)) (bandpass 1.2 (\t -> 200 + 1800 * min 1 (t / 0.7)) (pink 21 1.8))
    , gain 0.6 (env (\t -> min 1 (t / 0.6) * exp (negate (max 0 (t - 0.6)) / 0.3)) (osc sine (\t -> 55 + 55 * min 1 (t / 0.6)) 1.8)) ])
  -- Brought up short against a barrier.
  Barrier -> room (mix
    [ thump 85 40 0.3
    , gain 0.45 (env (perc 0.001 0.05) (bandpass 2 (const 600) (pink 22 0.2)))
    , gain 0.25 (grains 23 0.25 1400) ])
  BarrierPassed -> room (env (\t -> sin (pi * t / 0.35) ** 2) (bandpass 1.5 (\t -> 2000 - 3500 * t) (pink 24 0.35)))
  -- Left behind: the light runs on, and the dark falls.
  Caught -> hall 0.86 0.4 1.2 (env (\t -> min 1 (t / 0.05) * exp (negate t / 0.35)) (mix
    [ osc sine (\t -> 60 + 340 * exp (negate t * 4)) 0.9
    , gain 0.6 (bandpass 1.2 (\t -> 2500 * exp (negate t * 4) + 100) (pink 25 0.9)) ]))
  -- One lamp of the corridor catching as the keeper passes. Played higher for each.
  LampCatch -> hall 0.84 0.3 0.7 (mix
    [ bell a4 1.1
    , gain 0.15 (env (perc 0.001 0.02) (bandpass 1 (const 3000) (pink 26 0.05))) ])
  Thunder -> hall 0.88 0.3 1.5 (mix
    [ gain 0.4 (env (perc 0.002 0.08) (bandpass 0.7 (const 1500) (pink 27 0.3)))
    , gain 3 (env (\t -> perc 0.08 0.9 t * (0.7 + 0.3 * sin (t * 9) * sin (t * 3.1))) (lowpass (const 200) (brown 28 3.0))) ])
  -- The real lighthouse: the whole chord climbing into the white, the strike at
  -- its height, and the long ring as the dark comes down.
  Arrival -> hall 0.94 0.5 3.8 (mix
    [ env (\t -> (min 1 (t / 1.66)) ** 2.5 * (if t < 1.66 then 1 else exp (negate (t - 1.66) / 0.9)))
        (lowpass (\t -> 300 + 4000 * min 1 (t / 1.66) ** 2) (mix
          [ gain g (mix [ bsawTone (a2 * semis st * k) 3.5 | k <- [0.995, 1, 1.006] ])
          | (st, g) <- [(-12, 0.5), (0, 0.45), (7, 0.35), (12, 0.35), (16, 0.3), (19, 0.25), (24, 0.2), (28, 0.15)] ]))
    , gain 0.4 (env (\t -> min 1 (t / 1.6) ** 3 * (if t < 1.66 then 1 else 0)) (bandpass 1 (\t -> 300 + 4000 * min 1 (t / 1.66)) (pink 29 1.7)))
    , gain 0.25 (after 0.5 (mix [ after (0.12 * fromIntegral i) (bell (a4 * 2 * semis st) 1.0) | (i, st) <- zip [0 :: Int ..] [0, 4, 7, 12, 16, 19, 24, 19, 16] ]))
    , after 1.66 (mix [ thump 70 30 1.5, gain 0.9 (bell a3 4.0), gain 0.7 (bell a4 3.5), gain 0.4 (bell (a4 * semis 7) 3.0) ]) ])
  -- The card of the level beyond: one low bell in the dark.
  Card -> hall 0.92 0.42 2.8 (mix [ bell a3 3.0, gain 0.5 (after 0.25 (bell (a3 * semis 7) 2.5)) ])
  _ -> room (generate 0 (const 0))

-- | Crackles: short filtered clicks scattered over @d@ seconds, fewer as they go.
grains :: Int -> Float -> Float -> Signal
grains seed d centre = lowpass (const (centre * 1.6)) (bandpass 1.5 (const centre) (generate (len chance) click))
  where
    chance = noise seed d
    value = noise (seed + 100) d
    click i =
      let t = fromIntegral i / sr
      in if unsafeAt chance i > 1 - 0.012 * exp (negate t / (d / 3)) then unsafeAt value i * 3 else 0

shiftSound :: Bool -> Shot
shiftSound toPast = hall 0.84 0.32 0.8 (mix
  [ env (\t -> sin (pi * min 1 (t / 0.45)) ** 1.5) (bandpass 2 (\t -> if toPast then 2800 * 0.1 ** (t / 0.45) + 220 else 220 * 12 ** (t / 0.45)) (pink (if toPast then 30 else 31) 0.45))
  , gain 0.5 (env (\t -> sin (pi * min 1 (t / 0.45))) (osc sine (\t -> if toPast then 330 * 0.5 ** (t / 0.45) else 165 * 2 ** (t / 0.45)) 0.45))
  , gain 0.35 (after 0.1 (bell (if toPast then 659.3 else 987.8) 0.7)) ])

-- | A bed, left and right: the same recipe with different noise, so it surrounds
-- the player instead of hissing from the middle. Both sides share one peak.
renderBed :: Sound -> (Signal, Signal)
renderBed s = (gain k l, gain k r)
  where
    l = bedSide s 0
    r = bedSide s 500
    k = bedPeak s / max 1e-6 (max (peakOf l) (peakOf r))

bedPeak :: Sound -> Float
bedPeak s = if s == BedPulse then 0.7 else 0.6

bedSide :: Sound -> Int -> Signal
bedSide s o = case s of
  -- The past: a calm sea washing in and out, low and soft.
  BedSea -> loopOf 8 1 (\d -> mix
    [ env (\t -> 0.5 + 0.35 * sin (2 * pi * t / 4) + 0.15 * sin (2 * pi * t / (8 / 3))) (lowpass (const 450) (brown (40 + o) d))
    , gain 0.25 (env (\t -> max 0 (sin (2 * pi * t / 4 - 0.6)) ** 3) (bandpass 0.7 (const 700) (pink (46 + o) d))) ])
  -- The present: wind moaning round the walls, a distant roll, and rain.
  BedStorm -> loopOf 8 1 (\d -> mix
    [ env (\t -> 0.6 + 0.3 * sin (2 * pi * t / 8) + 0.1 * sin (2 * pi * t / 2)) (bandpass 4 (\t -> 380 + 120 * sin (2 * pi * t / 8 + fromIntegral o)) (brown (41 + o) d))
    , gain 0.6 (lowpass (const 140) (brown (42 + o) d))
    , gain 0.15 (lowpass (const 2600) (grains (43 + o) d 1800)) ])
  -- The lamp's light running through the walls.
  BedBeam -> loopOf 4 0.5 (\d -> mix
    ( gain 0.06 (bandpass 1 (const 3000) (pink (44 + o) d))
    : [ gain g (env (\t -> 0.8 + 0.2 * sin (2 * pi * t * r)) (tone sine f d)) | (f, g, r) <- [(880, 0.5, 1.5), (1318.5, 0.35, 2), (1760, 0.25, 2.5), (2637, 0.1, 3)] ]))
  -- The way waiting: a low open fifth, breathing.
  BedDrone -> loopOf 8 1 (\d -> env (\t -> 0.75 + 0.25 * sin (2 * pi * t / 4)) (mix
    [ pad 55 300 d, gain 0.8 (pad (82.4 * (1 + fromIntegral o * 0.000004)) 400 d), gain 0.4 (pad 110 600 d)
    , gain 0.15 (env (\t -> 0.5 + 0.5 * sin (2 * pi * t / 8)) (tone sine 659.3 d)) ]))
  -- Air rushing past: a deep whoosh, not a hiss.
  BedWind -> loopOf 4 1 (\d ->
    env (\t -> 0.7 + 0.3 * sin (2 * pi * t / 2)) (mix
      [ bandpass 1.5 (\t -> 500 + 200 * sin (2 * pi * t / 4)) (brown (45 + o) d)
      , gain 0.3 (bandpass 1.2 (const 900) (pink (47 + o) d)) ]))
  -- The run: a driving bar at 120 beats a minute, played faster as the rush speeds up.
  BedPulse -> pulseBar 2
  _ -> generate 0 (const 0)

-- | One bar: a kick on each beat, a bass line in eighths, and hats between.
pulseBar :: Float -> Signal
pulseBar bar = cut (samplesIn bar) (mix (kicks ++ bass ++ hats))
  where
    beat = bar / 4
    kicks = [ after (beat * fromIntegral i) (gain 0.9 (thump 130 45 0.3)) | i <- [0 .. 3 :: Int] ]
    bassNotes = [0, 0, 12, 0, 0, 0, 10, 12] :: [Float]
    bass = [ after (beat / 2 * fromIntegral i) (gain 0.45 (lowpass (const 500) (env (perc 0.005 0.08) (bsawTone (55 * semis st) (beat / 2)))))
           | (i, st) <- zip [0 :: Int ..] bassNotes ]
    hats = [ after (beat * fromIntegral i + beat / 2) (gain 0.12 (env (perc 0.001 0.02) (highpass (const 7000) (noise (50 + i) 0.06))))
           | i <- [0 .. 3 :: Int] ]
