{-# LANGUAGE BangPatterns #-}
-- | The opening film, in the game's own small format, decoded as it plays.
--
-- Gloss can draw a bitmap but cannot read a video, and the game takes no image
-- library, so tools/encode-intro.py turns the film into @assets/intro.blv@:
--
-- > "BLV1"  u16 width, height, frames, ticks a second (60), 0
-- > u16 per frame: the tick it appears on
-- > u32 audio rate, u16 channels (1), u32 samples; then the samples, s16
-- > per frame: u32 length, then: u8 flags (1: a palette follows: u16 n, n x RGB),
-- >   then spans until the end, each a byte: two bits of kind, six of length - 1
-- >   (63: a u16 follows, and the length is it + 64). Kind 0 keeps that many pixels
-- >   from the frame before, 1 fills them with the next byte, 2 copies the bytes
-- >   that follow.
--
-- Every number is little-endian. A palette comes with each shot, so each pixel is
-- one byte; a frame only codes what changed since the one before.
module Video
  ( Video
  , vdAudio
  , vdAudioRate
  , loadVideo
  , videoTicks
  , videoScale
  , Player
  , newPlayer
  , frameAt
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Int (Int16)
import Data.IORef
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeElemOff)
import Graphics.Gloss
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

data Video = Video
  { vdWidth     :: !Int
  , vdHeight    :: !Int
  , vdStarts    :: !(UArray Int Int)  -- ^ the tick each frame appears on, rising
  , vdFrames    :: ![BS.ByteString]   -- ^ each frame's coded spans
  , vdAudioRate :: !Int
  , vdAudio     :: !(UArray Int Float)  -- ^ mono, peaking at 0.9; empty for none
  }

-- | The film, if it can be found and read. It is looked for beside the program and
-- in each folder above it, so @cabal run@ finds the one in the project, and then
-- under the folder the game was started in.
loadVideo :: IO (Maybe Video)
loadVideo = do
  exe <- getExecutablePath
  let ups d = let p = takeDirectory d in if p == d then [d] else d : ups p
      candidates = map (\d -> d </> "assets" </> "intro.blv") (take 12 (ups (takeDirectory exe)))
                   ++ ["assets" </> "intro.blv"]
  found <- firstExisting candidates
  case found of
    Nothing -> pure Nothing
    Just path -> do
      r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      pure (either (const Nothing) parse r)
  where
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else firstExisting ps

-- | How many canvas units each of the film's pixels takes, to fill the 1280 across:
-- 1 for a film 1280 across.
videoScale :: Video -> Float
videoScale v = 1280 / fromIntegral (max 1 (vdWidth v))

-- | How long the film runs, in ticks: its last frame is held a moment.
videoTicks :: Video -> Int
videoTicks v = let (_, hi) = bounds (vdStarts v) in vdStarts v ! hi + 12

u16 :: BS.ByteString -> Int -> Int
u16 b i = fromIntegral (BU.unsafeIndex b i) .|. (fromIntegral (BU.unsafeIndex b (i + 1)) `shiftL` 8)

u32 :: BS.ByteString -> Int -> Int
u32 b i = u16 b i .|. (u16 b (i + 2) `shiftL` 16)

parse :: BS.ByteString -> Maybe Video
parse b
  | BS.length b < 14 || BS.take 4 b /= BS.pack [66, 76, 86, 49] = Nothing  -- "BLV1"
  | n <= 0 || w <= 0 || h <= 0 || BS.length b < audioAt + 10 = Nothing
  | channels /= 1 || BS.length b < framesAt = Nothing
  | otherwise = do
      frames <- chunks n framesAt
      Just Video
        { vdWidth = w, vdHeight = h
        , vdStarts = listArray (0, n - 1) [ u16 b (14 + 2 * j) | j <- [0 .. n - 1] ]
        , vdFrames = frames
        , vdAudioRate = rate
        , vdAudio = listArray (0, samples - 1) [ k * fromIntegral (sample i) | i <- [0 .. samples - 1] ]
        }
  where
    w = u16 b 4
    h = u16 b 6
    n = u16 b 8
    audioAt = 14 + 2 * n
    rate = u32 b audioAt
    channels = u16 b (audioAt + 4)
    samples = u32 b (audioAt + 6)
    pcmAt = audioAt + 10
    framesAt = pcmAt + 2 * samples
    sample i = fromIntegral (u16 b (pcmAt + 2 * i)) :: Int16
    peak = maximum (1 : [ abs (fromIntegral (sample i)) | i <- [0, 7 .. samples - 1] ]) :: Int
    k = 0.9 / fromIntegral peak :: Float
    chunks 0 _ = Just []
    chunks m at
      | at + 4 > BS.length b = Nothing
      | otherwise =
          let size = u32 b at
              body = BS.take size (BS.drop (at + 4) b)
          in if BS.length body /= size then Nothing else (body :) <$> chunks (m - 1) (at + 4 + size)

-- Playing ------------------------------------------------------------------------

-- | The film as it plays: the frame now decoded, which only moves forward.
data Player = Player
  { plVideo  :: !Video
  , plShown  :: !(IORef Int)             -- ^ the frame in the buffer; -1 before any
  , plPixels :: !(IOUArray Int Word8)    -- ^ the palette index of every pixel
  , plColour :: !(IOUArray Int Word32)   -- ^ the shot's palette, as RGBA in memory order
  , plPic    :: !(IORef Picture)
  }

newPlayer :: Video -> IO Player
newPlayer v = do
  shown <- newIORef (-1)
  pixels <- newArray (0, vdWidth v * vdHeight v - 1) 0
  colour <- newArray (0, 255) 0xFF000000
  pic <- newIORef Blank
  pure (Player v shown pixels colour pic)

-- | The film's picture at this many ticks in, centred, one pixel per canvas unit.
frameAt :: Player -> Int -> IO Picture
frameAt p tick = do
  shown <- readIORef (plShown p)
  let v = plVideo p
      (_, hi) = bounds (vdStarts v)
      wanted = last (0 : [ k | k <- [0 .. hi], vdStarts v ! k <= tick ])
  if wanted == shown
    then readIORef (plPic p)
    else do
      -- Frames only code their changes, so the way back is from the first.
      let from = if wanted < shown then 0 else shown + 1
      forM_ (take (wanted - from + 1) (drop from (vdFrames v))) (decode p)
      pic <- picture p
      writeIORef (plShown p) wanted
      writeIORef (plPic p) pic
      pure pic

decode :: Player -> BS.ByteString -> IO ()
decode p b = do
  let flags = BU.unsafeIndex b 0
  start <- if flags .&. 1 == 0 then pure 1 else do
    let count = u16 b 1
    forM_ [0 .. count - 1] $ \i -> do
      let at = 3 + 3 * i
          r = fromIntegral (BU.unsafeIndex b at) :: Word32
          g = fromIntegral (BU.unsafeIndex b (at + 1))
          bl = fromIntegral (BU.unsafeIndex b (at + 2))
      unsafeWrite (plColour p) i (r .|. (g `shiftL` 8) .|. (bl `shiftL` 16) .|. 0xFF000000)
    pure (3 + 3 * count)
  let size = BS.length b
      total = vdWidth (plVideo p) * vdHeight (plVideo p)
      spans :: Int -> Int -> IO ()
      spans !at !px = when (at < size && px < total) $ do
        let op = BU.unsafeIndex b at
            short = fromIntegral (op .&. 63) :: Int
            (n0, body) = if short == 63 then (u16 b (at + 1) + 64, at + 3) else (short + 1, at + 1)
            n = min n0 (total - px)
        case op `shiftR` 6 of
          0 -> spans body (px + n)
          1 -> do
            let c = BU.unsafeIndex b body
            forM_ [px .. px + n - 1] $ \i -> unsafeWrite (plPixels p) i c
            spans (body + 1) (px + n0)
          _ -> do
            forM_ [0 .. n - 1] $ \i -> unsafeWrite (plPixels p) (px + i) (BU.unsafeIndex b (body + i))
            spans (body + n0) (px + n0)
  spans start 0

picture :: Player -> IO Picture
picture p = do
  let v = plVideo p
      w = vdWidth v
      h = vdHeight v
  bytes <- BI.create (w * h * 4) $ \ptr -> do
    let out = castPtr ptr :: Ptr Word32
    forM_ [0 .. w * h - 1] $ \i -> do
      c <- unsafeRead (plPixels p) i
      rgba <- unsafeRead (plColour p) (fromIntegral c)
      pokeElemOff out i rgba
  pure (bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bytes False)
