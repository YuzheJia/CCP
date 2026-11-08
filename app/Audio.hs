-- | Sound out: hands the synthesised sounds to the mixer in cbits/audio.c, and
-- turns each 'Cue' into a sound played at its own loudness and pitch.
--
-- The mixer runs on a thread of its own and plays through Windows' waveOut. On
-- other systems it is silent, and everything here does nothing.
module Audio
  ( startAudio
  , playCue
  , setBeds
  , setMuted
  , loadClip
  , dumpSounds
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import Data.List (partition)
import Control.Monad (forM_, when)
import GHC.Clock (getMonotonicTime)
import qualified Data.ByteString.Builder as B
import Data.Int (Int16)
import Foreign.C.Types (CFloat (..), CInt (..))
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

foreign import ccall safe "bl_audio_init" c_init :: IO CInt
foreign import ccall safe "bl_audio_load" c_load :: CInt -> Ptr CFloat -> Ptr CFloat -> CInt -> IO ()
foreign import ccall unsafe "bl_audio_play" c_play :: CInt -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "bl_audio_bed" c_bed :: CInt -> CInt -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "bl_audio_mute" c_mute :: CInt -> IO ()
foreign import ccall unsafe "bl_audio_stop" c_stop :: CInt -> IO ()

-- | Open the output and make the sounds of the title, the hall and the levels.
-- False, and silent, if there is no output. The ending's sounds, which take longer
-- to make and are not wanted until level 3 is won, are made on a thread of their
-- own once the game is running.
startAudio :: IO Bool
startAudio = do
  ok <- (/= 0) <$> c_init
  when ok $ do
    let (later, now) = partition isEnding [minBound .. maxBound]
    mapM_ load now
    _ <- forkIO (mapM_ load later)
    pure ()
  pure ok
  where
    load s = do
      let (l, r) = render s
          n = min (len l) (len r)
          copy side p = forM_ [0 .. n - 1] $ \i -> pokeElemOff p i (realToFrac (unsafeAt side i))
      allocaArray n $ \pl -> allocaArray n $ \pr -> do
        copy l pl
        copy r pr
        c_load (soundId s) pl pr (fromIntegral n)

soundId :: Sound -> CInt
soundId = fromIntegral . fromEnum

play :: Sound -> Float -> Float -> Float -> IO ()
play s g r p = c_play (soundId s) (realToFrac g) (realToFrac r) (realToFrac p)

-- | How loud, how high and where each cue plays.
playCue :: Cue -> IO ()
playCue c = case c of
  CueStep era foot pan who ->
    -- Three slightly different feet, so a walk is not one sound repeated. In a level
    -- for two, the red keeper's tread is lighter and higher, to tell them apart.
    let rate = [0.93, 1, 1.07] !! ((foot `div` stepTicks) `mod` 3)
        (whoRate, whoGain) = case who of
          Yellow -> (1, 0.3)
          Red -> (1.22, 0.26)
    in play (if era == Past then StepPast else StepPresent) whoGain (rate * whoRate) pan
  CuePush -> play Push 0.5 1 0
  CueMirrorPush -> play MirrorPush 0.5 1 0
  CuePitFill -> play PitFill 0.65 1 0
  CueJump False -> play Jump 0.55 1 0
  CueJump True -> play JumpTurn 0.55 1 0
  CueShift Past -> play ShiftPast 0.5 1 0
  CueShift Present -> play ShiftPresent 0.5 1 0
  CuePickup -> play Pickup 0.55 1 0
  CueLamp -> play Lamp 0.7 1 0
  CueLose -> play Lose 0.5 1 0
  CueBump -> play Bump 0.4 1 0
  CueRefused -> play Refused 0.4 1 0
  CueUndo -> play Undo 0.35 1 0
  CueRestart -> play Restart 0.4 1 0
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
  CueWallBreaks k -> play WallBreak (max 0.35 (0.8 - 0.04 * fromIntegral k)) ([1, 0.9, 1.08, 0.95, 1.04] !! (k `mod` 5)) 0
  CueRushBegins -> play RushBegins 0.75 1 0
  CueBarrier -> play Barrier 0.55 1 0
  CueBarrierPassed -> play BarrierPassed 0.35 1 0
  CueCaught -> play Caught 0.7 1 0
  -- The corridor's lamps climb a pentatonic scale, one note each, as they catch.
  CueLampCatches n ->
    let steps = [0, 2, 4, 7, 9, 12, 14, 16, 19, 21, 24, 26, 28] :: [Float]
        st = steps !! (n `mod` length steps)
    in play LampCatch 0.38 (2 ** (st / 12)) (if even n then -0.35 else 0.35)
  CueThunder -> play Thunder 0.5 1 0
  CueArrival -> play Arrival 1 1 0
  CueCard -> play Card 0.55 1 0
  -- The opening film's own soundtrack, in the slot after every made sound.
  CueIntro -> c_play clipSlot 0.9 1 0
  CueIntroEnd -> c_stop clipSlot
  CueTalkOpen -> play TalkOpen 0.42 1 0
  CueTalkNext -> play MenuMove 0.16 1.25 0
  -- Letters blip at a few nearby pitches, so a line does not drone.
  CueTalkBlip k -> play TalkBlip 0.14 ([1, 1.12, 0.94, 1.06] !! ((k `div` 3) `mod` 4)) 0
  CueTalkClose -> play TalkClose 0.3 1 0

-- | The looping beds, each in its own slot of the mixer.
setBeds :: Beds -> IO ()
setBeds b = do
  bed 0 BedSea (bdSea b) 1
  bed 1 BedStorm (bdStorm b) 1
  bed 2 BedBeam (bdBeam b) 1
  bed 3 BedDrone (bdDrone b) 1
  bed 4 BedWind (bdWind b) (bdWindRate b)
  bed 5 BedPulse (bdPulse b) (bdPulseRate b)
  where
    bed :: CInt -> Sound -> Float -> Float -> IO ()
    bed slot s g r = c_bed slot (soundId s) (realToFrac g) (realToFrac r)

setMuted :: Bool -> IO ()
setMuted m = c_mute (if m then 1 else 0)

-- | Every sound as a WAV file in @dir@, to listen to outside the game.
dumpSounds :: FilePath -> IO ()
dumpSounds dir = do
  createDirectoryIfMissing True dir
  forM_ [minBound .. maxBound :: Sound] $ \s -> do
    let path = dir </> show s <.> "wav"
        (l, r) = render s
    begin <- getMonotonicTime
    _ <- evaluate l
    _ <- evaluate r
    made <- getMonotonicTime
    withBinaryFile path WriteMode (\h -> B.hPutBuilder h (wav (elems l) (elems r)))
    putStrLn (path ++ "   made in " ++ show (round ((made - begin) * 1000) :: Int) ++ " ms")

-- | A 16-bit stereo WAV file.
wav :: [Float] -> [Float] -> B.Builder
wav ls rs = mconcat
  [ B.string7 "RIFF", B.word32LE (36 + bytes), B.string7 "WAVE"
  , B.string7 "fmt ", B.word32LE 16, B.word16LE 1, B.word16LE 2
  , B.word32LE (fromIntegral sampleRate), B.word32LE (fromIntegral sampleRate * 4)
  , B.word16LE 4, B.word16LE 16
  , B.string7 "data", B.word32LE bytes
  , mconcat (zipWith (\l r -> sample l <> sample r) ls rs) ]
  where
    frames = min (length ls) (length rs)
    bytes = fromIntegral (frames * 4)
    sample x = B.int16LE (fromIntegral (round (max (-1) (min 1 x) * 32000) :: Int) :: Int16)

-- | The mixer's slot for a clip loaded from a file: the opening film's soundtrack.
-- The made sounds take the slots from 0 up, by their order in 'Sound'.
clipSlot :: CInt
clipSlot = 63

-- | Hand a mono clip to the mixer, resampled from @rate@ to the mixer's own.
loadClip :: Int -> UArray Int Float -> IO ()
loadClip rate clip = do
  let (lo, hi) = bounds clip
      n0 = hi - lo + 1
      step = fromIntegral rate / fromIntegral sampleRate :: Double
      n = if n0 <= 0 then 0 else floor (fromIntegral (n0 - 1) / step) + 1
      at i = let p = fromIntegral i * step
                 j = floor p :: Int
                 k = realToFrac (p - fromIntegral j) :: Float
                 a = clip ! (lo + min (n0 - 1) j)
                 b = clip ! (lo + min (n0 - 1) (j + 1))
             in a + (b - a) * k
  when (n > 1) $ allocaArray n $ \p -> do
    forM_ [0 .. n - 1] $ \i -> pokeElemOff p i (realToFrac (at i))
    c_load clipSlot p nullPtr (fromIntegral n)
