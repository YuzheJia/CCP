{-# OPTIONS_GHC -fno-omit-yields #-}
-- | Sound out: hands the synthesised sounds to the mixer in cbits/audio.c, and
-- turns each 'Cue' into a sound played at its own loudness and pitch.
--
-- The mixer runs on a thread of its own and plays through Windows' waveOut. On
-- other systems it is silent, and everything here does nothing.
--
-- What this module does:
--
-- * At startup ('startAudio'), makes every sound with 'Synth.render' and copies its
--   samples into the mixer's memory, one numbered slot per sound. The opening
--   film's soundtrack goes into a slot of its own ('loadClip').
-- * Every frame, plays the cues that 'Game.Sound.cues' found ('playCue'), sets the
--   looping beds to the levels 'Game.Sound.beds' asked for ('setBeds'), and passes
--   on the M key's mute ('setMuted').
-- * For @--dump-sounds=DIR@, writes every sound to a WAV file instead
--   ('dumpSounds').
--
-- What it does not do: decide what is heard or when (that is the pure 'Game.Sound',
-- which the tests check), or how a sound is made (app/Synth.hs). It does no mixing
-- either: the C side adds the playing voices together, keeps the beds looping,
-- glides their levels and rounds off loud peaks, on its own thread, so a slow frame
-- never stutters the sound.
--
-- How app/Main.hs calls it: 'startAudio' once, before the window opens (not at all
-- for a @--script@ run, which stays silent), then 'loadClip' if the film was found.
-- If 'startAudio' said False, nothing else here is called. Then once a frame, after
-- that frame's ticks: 'setMuted', 'playCue' for each cue in the order they
-- happened, and 'setBeds'.
--
-- The numbers handed to the mixer:
--
-- * gain: a loudness, 1 for a sound at the level it was made. Every made one-shot
--   peaks at 0.9 ('Synth.render' normalises it), so the gains below are only how
--   loud one cue is against another. The mixer then multiplies everything by its
--   master volume (0.6) and rounds off whatever still goes over 0.6, so several
--   loud sounds at once bend rather than crackle.
-- * rate: a playback speed, 1 as made. The mixer reads the samples faster or more
--   slowly, so a rate of 2 is an octave higher and half as long. This is how one
--   made sound gives several pitches.
-- * pan: -1 all left, 0 centre (both sides in full), 1 all right; panned one way,
--   the far side fades.
-- * samples: 32-bit floats, roughly -1 to 1, at 'sampleRate' (44100 a second),
--   which must equal BL_RATE in audio.c. Both are 44100, and nothing checks that
--   they agree.
-- * slots: a made sound's slot is its place in the 'Sound' type ('soundId'), from 0.
--   The mixer has 64 slots (BL_SOUNDS), and the film's soundtrack takes the last.
--
-- The pragma on the first line, -fno-omit-yields, is explained under 'startAudio'.
module Audio
  ( startAudio
  , playCue
  , setBeds
  , setMuted
  , loadClip
  , dumpSounds
  ) where

import Control.Concurrent (forkIO, yield)
import Control.Exception (evaluate)
import Data.List (partition)
import Control.Monad (forM_, when)
import GHC.Clock (getMonotonicTime)
import qualified Data.ByteString.Builder as B
import Data.Int (Int16)
-- The C types int and float, as Haskell types, for the foreign calls below.
import Foreign.C.Types (CFloat (..), CInt (..))
-- Array indexing by position from 0, without the bounds check that '!' makes.
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, bounds, elems, (!))
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (pokeElemOff)
import Game.Sound
import Game.Types (Era (..), stepTicks)
import Synth
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.IO (IOMode (WriteMode), withBinaryFile)

-- The C side, cbits/audio.c. A @foreign import ccall@ names a C function and gives
-- it a Haskell type, so it is called like any IO action.
--
-- @safe@ and @unsafe@ say how much the runtime must prepare for the call. A safe
-- call lets the runtime carry on around it (on the threaded runtime other Haskell
-- threads keep running, and the C code may call back into Haskell). An unsafe call
-- is a plain, cheaper C call that holds the runtime up for its length and must never
-- call back. The two that can take a while, opening the output and copying a whole
-- sound, are safe; the ones made every frame only take the mixer's lock and change a
-- few numbers, so they are unsafe.
--
-- bl_audio_init: open the output and start the mixer's thread. 1 if sound will be
-- heard, 0 if not (no output device, or not Windows).
foreign import ccall safe "bl_audio_init" c_init :: IO CInt
-- bl_audio_load: put a sound into a slot: the slot, the left samples, the right
-- samples (or a null pointer for a mono sound, whose left side then plays on both),
-- and the length in samples. The mixer copies the samples into memory of its own, so
-- the arrays passed may be freed as soon as it returns. Loading a slot again
-- replaces what was in it.
foreign import ccall safe "bl_audio_load" c_load :: CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO ()
-- bl_audio_play: start a slot's sound once: the slot, gain, rate and pan. It takes
-- one of 48 voices; when all are busy the quietest gives way. Ignored if the slot is
-- still empty or the rate is not above 0.
foreign import ccall unsafe "bl_audio_play" c_play :: CInt -> CFloat -> CFloat -> CFloat -> IO ()
-- bl_audio_bed: set one of the mixer's 8 bed slots: the bed slot, the sound's slot,
-- gain and rate. A different sound from last time starts from its beginning, silent,
-- and rises; the same sound only glides to the new gain and rate, over about 40 ms.
foreign import ccall unsafe "bl_audio_bed" c_bed :: CInt -> CInt -> CFloat -> CFloat -> IO ()
-- bl_audio_mute: 1 silences the output, 0 brings it back.
foreign import ccall unsafe "bl_audio_mute" c_mute :: CInt -> IO ()
-- bl_audio_stop: every voice playing this slot's sound fades out, over about half a
-- second.
foreign import ccall unsafe "bl_audio_stop" c_stop :: CInt -> IO ()

-- | Open the output and make the sounds of the title, the hall and the levels.
-- False, and silent, if there is no output. The ending's sounds, which take longer
-- to make and are not wanted until level 3 is won, are made on a thread of their
-- own once the game is running.
--
-- On Windows the game uses the single-threaded runtime, so that thread takes turns
-- with the game loop on one core, and the runtime switches between them only where
-- a thread checks in with it. Synth's loops write unboxed arrays without
-- allocating, and such a loop never checked in, so one long sound (the arrival
-- takes half a second to make) held the game loop up for its whole length. Synth
-- and this module are compiled with -fno-omit-yields, which adds those check-ins,
-- and the thread also yields after each sound.
--
-- When the output cannot be opened, no sound is made at all, which saves the time.
-- A cue whose sound the background thread has not made yet finds its slot empty
-- and is not heard; that could only happen if level 3 were won within the first few
-- seconds.
startAudio :: IO Bool
startAudio = do
  -- @<$>@ is 'fmap': it applies a function to the result of an IO action. @(/= 0)@
  -- is an operator section, the function @\\x -> x /= 0@, so this turns the C int
  -- into a Bool.
  ok <- (/= 0) <$> c_init
  -- 'when' runs the action only if the condition holds. @$@ applies the function on
  -- its left to everything on its right, which saves brackets round the @do@ block.
  when ok $ do
    -- Every sound, in the order the 'Sound' type lists them (@deriving (Enum,
    -- Bounded)@ there is what allows @[minBound .. maxBound]@), split by 'partition'
    -- into those that pass the test (the ending's) and those that do not.
    let (later, now) = partition isEnding [minBound .. maxBound]
    -- Made and loaded one after another before this returns, so they all exist
    -- before the first frame.
    mapM_ load now
    -- A new Haskell thread for the rest. @(>> yield) . load@ is a composition: for
    -- each sound, load it and then yield, giving the game loop its turn. Nothing
    -- waits for the thread, so its ThreadId is dropped with @_ <-@.
    _ <- forkIO (mapM_ ((>> yield) . load) later)
    pure ()
  pure ok
  where
    -- Make one sound and copy both its sides into the mixer.
    load s = do
      -- 'render' is pure and the binding lazy: nothing is made until the first
      -- sample is read below, and an unboxed array is then built whole.
      let (l, r) = render s
          -- Both sides are made the same length; the smaller keeps the copy from
          -- reading past the end of either.
          n = min (len l) (len r)
          -- Write sample i of one side into the C array at i, as a CFloat.
          -- 'unsafeAt' skips the bounds check, which matters in a loop of tens of
          -- thousands, and is safe because i is below n.
          copy side p = forM_ [0 .. n - 1] $ \i -> pokeElemOff p i (realToFrac (unsafeAt side i))
      -- 'allocaArray' makes a C array of n floats, runs the function on a pointer
      -- to it, and frees it afterwards. That is safe because the mixer has copied
      -- the samples by the time c_load returns.
      allocaArray n $ \pl -> allocaArray n $ \pr -> do
        copy l pl
        copy r pr
        c_load (soundId s) pl pr (fromIntegral n)

-- | The mixer's slot for a made sound: its place in the 'Sound' type, from 0
-- ('fromEnum', which @deriving Enum@ gives), as a C int. There are 43 sounds, so
-- they take slots 0 to 42 and never reach 'clipSlot'. Written point-free: the
-- composition of two functions, with no named argument.
soundId :: Sound -> CInt
soundId = fromIntegral . fromEnum

-- | Start a made sound once: the sound, its gain, rate and pan, converted from
-- Haskell's Float to C's float with 'realToFrac'.
play :: Sound -> Float -> Float -> Float -> IO ()
play s g r p = c_play (soundId s) (realToFrac g) (realToFrac r) (realToFrac p)

-- | How loud, how high and where each cue plays.
--
-- One branch per 'Cue' (-Wall warns if one is missed). Most play their own made
-- sound at a fixed gain, rate 1 and centred, @play sound gain rate pan@; the comments
-- mark the ones that vary. The gains only weigh one cue against another: a menu
-- move is 0.22 and a footstep 0.3, while the lamp dying and the arrival are at 1.
playCue :: Cue -> IO ()
playCue c = case c of
  CueStep era foot pan who ->
    -- Three slightly different feet, so a walk is not one sound repeated. In a level
    -- for two, the red keeper's tread is lighter and higher, to tell them apart.
    -- @foot `div` stepTicks@ counts the clock in steps; in a steady walk a step
    -- begins every 'stepTicks' (8) ticks, so each step takes the next of the three
    -- rates, round and round (@`mod` 3@). @!!@ picks from a list by position.
    -- The red keeper plays 22% faster, so higher and shorter, and a little quieter.
    -- The era chooses the floor: boards and plaster in the past, wet stone now.
    let rate = [0.93, 1, 1.07] !! ((foot `div` stepTicks) `mod` 3)
        (whoRate, whoGain) = case who of
          Yellow -> (1, 0.3)
          Red -> (1.22, 0.26)
    in play (if era == Past then StepPast else StepPresent) whoGain (rate * whoRate) pan
  CuePush -> play Push 0.5 1 0
  CueMirrorPush -> play MirrorPush 0.5 1 0
  CuePitFill -> play PitFill 0.65 1 0
  -- A flat mirror rings; a corner mirror rings, turns and rises again.
  CueJump False -> play Jump 0.55 1 0
  CueJump True -> play JumpTurn 0.55 1 0
  -- A falling, warm wave into the past; a rising, colder one into the present.
  CueShift Past -> play ShiftPast 0.5 1 0
  CueShift Present -> play ShiftPresent 0.5 1 0
  CuePickup -> play Pickup 0.55 1 0
  CueLamp -> play Lamp 0.7 1 0
  CueLose -> play Lose 0.5 1 0
  CueBump -> play Bump 0.4 1 0
  CueRefused -> play Refused 0.4 1 0
  CueUndo -> play Undo 0.35 1 0
  CueRestart -> play Restart 0.4 1 0
  -- At each doorway between rooms; level 3 is the only level of several rooms.
  CueRoom -> play Room 0.18 1 0
  CuePause True -> play PauseOn 0.25 1 0
  CuePause False -> play PauseOff 0.25 1 0
  CueMenuMove -> play MenuMove 0.22 1 0
  CueMenuEnter -> play MenuEnter 0.35 1 0
  CueMenuBack -> play MenuBack 0.3 1 0
  CueFlare -> play Flare 0.65 1 0
  CueLampDies -> play LampDies 1 1 0
  CueLightGoes -> play LightGoes 0.5 1 0
  -- Each wall a little different, and a little further off.
  -- The first wall, beside the lamp, at 0.8, each one further along 0.04 quieter,
  -- never under 0.35; the rate goes round five values, so neighbours differ.
  CueWallBreaks k -> play WallBreak (max 0.35 (0.8 - 0.04 * fromIntegral k)) ([1, 0.9, 1.08, 0.95, 1.04] !! (k `mod` 5)) 0
  CueRushBegins -> play RushBegins 0.75 1 0
  CueBarrier -> play Barrier 0.55 1 0
  CueBarrierPassed -> play BarrierPassed 0.35 1 0
  CueCaught -> play Caught 0.7 1 0
  -- The corridor's lamps climb a pentatonic scale, one note each, as they catch.
  -- @steps@ are semitones above the first note: 0 2 4 7 9 is the major pentatonic,
  -- repeated an octave up (12 ...) and two octaves up (24 ...), 13 notes for the 13
  -- lamps ('lampCount'); the @`mod`@ only keeps the index in range. In equal
  -- temperament each semitone multiplies the frequency by the twelfth root of 2,
  -- and the rate multiplies the frequency, so @2 ** (st / 12)@ plays the note @st@
  -- semitones up ('**' is a power of Floats). The lamps alternate a little left and
  -- a little right, so the scale moves across the stereo.
  CueLampCatches n ->
    let steps = [0, 2, 4, 7, 9, 12, 14, 16, 19, 21, 24, 26, 28] :: [Float]
        st = steps !! (n `mod` length steps)
    in play LampCatch 0.38 (2 ** (st / 12)) (if even n then -0.35 else 0.35)
  CueThunder -> play Thunder 0.5 1 0
  CueArrival -> play Arrival 1 1 0
  CueCard -> play Card 0.55 1 0
  -- The opening film's own soundtrack, in the slot after every made sound.
  -- It is not a 'Sound', so 'play' cannot name it, and c_play is called with the
  -- slot directly. Rate 1 is its true speed, since 'loadClip' resampled it to the
  -- mixer's rate. Number literals such as 0.9 take whatever numeric type is wanted,
  -- here CFloat.
  CueIntro -> c_play clipSlot 0.9 1 0
  -- Skipped or played out, the soundtrack fades over about half a second.
  CueIntroEnd -> c_stop clipSlot
  CueTalkOpen -> play TalkOpen 0.42 1 0
  -- The menu's tick, softer and a quarter faster, so higher.
  CueTalkNext -> play MenuMove 0.16 1.25 0
  -- Letters blip at a few nearby pitches, so a line does not drone.
  -- Only letters 1, 4, 7, ... blip, so @k `div` 3@ is 0, 1, 2, ...: each blip
  -- takes the next of the four rates.
  CueTalkBlip k -> play TalkBlip 0.14 ([1, 1.12, 0.94, 1.06] !! ((k `div` 3) `mod` 4)) 0
  CueTalkClose -> play TalkClose 0.3 1 0

-- | The looping beds, each in its own slot of the mixer.
--
-- Bed slots 0 to 5 of the mixer's 8, each always given the same sound, so after the
-- first call only gains and rates change, and the mixer glides to them. Called every
-- frame, with what 'Game.Sound.beds' says for the run as it is. A bed at gain 0
-- costs the mixer nothing; one whose sound the background thread has not made yet
-- (the ending's beam, drone, wind and beat) is skipped until it has been.
setBeds :: Beds -> IO ()
setBeds b = do
  bed 0 BedSea (bdSea b) 1
  bed 1 BedStorm (bdStorm b) 1
  bed 2 BedBeam (bdBeam b) 1
  bed 3 BedDrone (bdDrone b) 1
  bed 4 BedWind (bdWind b) (bdWindRate b)
  bed 5 BedPulse (bdPulse b) (bdPulseRate b)
  where
    -- A helper in a @where@ can have its own type signature; this one fixes the
    -- bed slot's type as CInt, so the literal slot numbers above are C ints.
    bed :: CInt -> Sound -> Float -> Float -> IO ()
    bed slot s g r = c_bed slot (soundId s) (realToFrac g) (realToFrac r)

-- | Silence all output (True) or bring it back (False): the M key, or @--mute@.
-- Main passes the current setting every frame. Voices and beds go on playing
-- unheard, so unmuting comes back in the middle of whatever is playing.
setMuted :: Bool -> IO ()
setMuted m = c_mute (if m then 1 else 0)

-- | Every sound as a WAV file in @dir@, to listen to outside the game.
--
-- For @--dump-sounds=DIR@, after which Main exits; no output is opened. Each file is
-- named after its constructor in 'Sound' (@StepPast.wav@ and so on), and each line
-- printed says how long that sound took to make, in milliseconds.
dumpSounds :: FilePath -> IO ()
dumpSounds dir = do
  -- True: make missing parent directories too.
  createDirectoryIfMissing True dir
  -- Every sound in order; @:: Sound@ says whose 'minBound' and 'maxBound' are meant.
  forM_ [minBound .. maxBound :: Sound] $ \s -> do
    -- '</>' joins path parts with the system's separator; '<.>' adds an extension.
    let path = dir </> show s <.> "wav"
        (l, r) = render s
    -- 'evaluate' forces a value inside IO, at this point and no later. For an
    -- unboxed array that means every sample is made, so the time between the two
    -- clock readings (seconds, from a clock that never jumps) is the making alone,
    -- not the writing. The two sides share most of their work, so nearly all of it
    -- falls on the first.
    begin <- getMonotonicTime
    _ <- evaluate l
    _ <- evaluate r
    made <- getMonotonicTime
    -- Opens the file for writing as raw bytes (binary mode: no text encoding and
    -- no newline translation), runs the function on its handle, and closes it
    -- afterwards even if writing fails. 'elems' gives an array's values as a list.
    withBinaryFile path WriteMode (\h -> B.hPutBuilder h (wav (elems l) (elems r)))
    putStrLn (path ++ "   made in " ++ show (round ((made - begin) * 1000) :: Int) ++ " ms")

-- | A 16-bit stereo WAV file.
--
-- From the left and right samples, roughly -1 to 1. A Builder is a recipe for a
-- run of bytes that is cheap to join ('<>' joins two, 'mconcat' a list). The file is
-- a 44-byte header followed by the samples, every number little-endian:
--
-- * @RIFF@, the size of everything after these first 8 bytes (36 more bytes of
--   header, then the samples), and @WAVE@.
-- * The @fmt @ chunk, 16 bytes long: format 1 (plain PCM samples), 2 channels, the
--   samples a second, the bytes a second (4 for each frame: two channels of two
--   bytes), the bytes a frame (4) and the bits a sample (16).
-- * The @data@ chunk: its length in bytes, then the frames, each the left sample
--   then the right. 'zipWith' pairs the two lists and stops at the shorter one.
wav :: [Float] -> [Float] -> B.Builder
wav ls rs = mconcat
  [ B.string7 "RIFF", B.word32LE (36 + bytes), B.string7 "WAVE"
  , B.string7 "fmt ", B.word32LE 16, B.word16LE 1, B.word16LE 2
  , B.word32LE (fromIntegral sampleRate), B.word32LE (fromIntegral sampleRate * 4)
  , B.word16LE 4, B.word16LE 16
  , B.string7 "data", B.word32LE bytes
  , mconcat (zipWith (\l r -> sample l <> sample r) ls rs) ]
  where
    -- A frame is one sample for each channel; as many as the shorter side has, which
    -- is what zipWith writes. The header needs this before any sample, so both
    -- lists are walked first and held in memory until they are written: fine for a
    -- tool run once by hand.
    frames = min (length ls) (length rs)
    -- Four bytes a frame. Its type, a 32-bit word, is worked out from its use in
    -- 'B.word32LE'.
    bytes = fromIntegral (frames * 4)
    -- Held between -1 and 1, scaled to 32000 (a little under the 32767 a 16-bit
    -- sample can reach, as the mixer does), rounded and written as two bytes.
    sample x = B.int16LE (fromIntegral (round (max (-1) (min 1 x) * 32000) :: Int) :: Int16)

-- | The mixer's slot for a clip loaded from a file: the opening film's soundtrack.
-- The made sounds take the slots from 0 up, by their order in 'Sound'.
-- 63 is the last of the mixer's 64 slots (BL_SOUNDS in audio.c).
clipSlot :: CInt
clipSlot = 63

-- | Hand a mono clip to the mixer, resampled from @rate@ to the mixer's own.
--
-- Main calls it once at startup with the film's soundtrack (from app/Video.hs),
-- when sound is on and the film was found. @rate@ is the clip's samples a second;
-- the clip's array may be indexed from any @lo@ to @hi@. Resampling is linear: each
-- new sample lies between the two nearest old ones, weighted by how near it is to
-- each. The film's encoder writes 44100 a second, the mixer's own rate, so in
-- practice the step is 1 and every sample is copied as it is; the resampling is
-- there for a film encoded at another rate.
loadClip :: Int -> UArray Int Float -> IO ()
loadClip rate clip = do
  let (lo, hi) = bounds clip
      n0 = hi - lo + 1
      -- Old samples to each new one, in Double for a precise position through a
      -- long clip.
      step = fromIntegral rate / fromIntegral sampleRate :: Double
      -- New samples: the last one, at (n - 1) * step, must not pass the last old
      -- sample, n0 - 1. None for an empty clip.
      n = if n0 <= 0 then 0 else floor (fromIntegral (n0 - 1) / step) + 1
      -- New sample i: p is its position among the old samples, j the old sample
      -- at or before it and k how far on from j it is, 0 to 1. The @min (n0 - 1)@
      -- keeps j + 1 inside the clip at its very end. '!' is checked indexing by the
      -- array's own indices, hence @lo +@. a + (b - a) * k runs from a at k = 0 to
      -- b at k = 1.
      at i = let p = fromIntegral i * step
                 j = floor p :: Int
                 k = realToFrac (p - fromIntegral j) :: Float
                 a = clip ! (lo + min (n0 - 1) j)
                 b = clip ! (lo + min (n0 - 1) (j + 1))
             in a + (b - a) * k
  -- The mixer never plays a sound shorter than two samples, so such a clip is not
  -- loaded. A null pointer for the right side makes it mono: the one side plays on
  -- both.
  when (n > 1) $ allocaArray n $ \p -> do
    forM_ [0 .. n - 1] $ \i -> pokeElemOff p i (realToFrac (at i))
    c_load clipSlot p nullPtr (fromIntegral n)
