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
--
-- The format in more detail, part by part:
--
-- * The header is 14 bytes: the four letters, then five u16. The tick rate (always
--   60, the game's own) and the last field (always 0) are written by the encoder but
--   not read here: the game takes it that a film runs at 60 ticks a second.
-- * The frame table has one u16 a frame: the tick, counted from the film's start, on
--   which that frame first shows. The ticks rise. The source runs at about 24 frames
--   a second against the game's 60 ticks, so a frame shows for 2 or 3 ticks (0, 3, 5,
--   8, 10 and so on), and the encoder keeps only frames that differ from the one
--   before, so a still moment is one frame held for as long as it lasts. A u16 tick
--   means a film can run at most 65,535 ticks, about 18 minutes.
-- * The soundtrack is its sample rate in Hz, its channel count (only 1, mono, is
--   accepted), its number of samples, then every sample as a signed 16-bit number.
--   Audio.loadClip resamples it to the mixer's rate and the mixer plays it in a
--   slot of its own.
-- * Each frame is a chunk that starts with its own length in bytes, so the parser
--   can find every frame without decoding any. A pixel is one byte, an index into a
--   palette (three bytes a colour: red, green, blue; the encoder always makes 256
--   colours) that the first frame of each shot carries. That frame also codes
--   every pixel; later frames of the shot code only what changed.
-- * Spans walk the picture as one long line of width x height pixels, row by row
--   from the top-left, so one span can run on from the end of a row into the next.
--   In the span's first byte the top two bits are the kind and the low six bits the
--   length less one, so 0 to 62 mean 1 to 63 pixels; 63 means the length is in the
--   next two bytes, plus 64 (64 to 65,599 pixels). A keep (kind 0) has no data: the
--   pixels stay as the frame before left them. A fill (kind 1) has one byte, a
--   palette index written into every pixel of the span. A copy (kind 2) is followed
--   by its pixels, a byte each. Kind 3 is never written, and is read as a copy. The
--   encoder fills any run of three or more equal pixels, copies the rest, and keeps
--   any pixel whose colour has barely changed, so a frame of slow movement is
--   mostly keeps.
--
-- The film in the game is 1280 by 720 pixels, 241 frames in five shots, about ten
-- seconds, and about 31 MB.
--
-- What this module does: finds and reads the file ('loadVideo'), checks that its
-- parts fit together ('parse'), and turns frames into Gloss pictures as the film
-- plays ('Player', 'frameAt'). What it does not do: decide when the film plays or
-- how a hold skips it (the @Intro@ mode in Game.Run), draw anything around it (the
-- black backdrop, the fade in and the skip hint are TalkBox.introScreen), or play
-- the sound (Main hands 'vdAudio' to Audio.loadClip, and Game.Sound's @CueIntro@
-- starts it).
--
-- The flow: at startup Main calls 'loadVideo' once and makes one 'Player' with
-- 'newPlayer'. While the film runs, every drawn frame Main calls 'frameAt' with the
-- number of ticks since the film began. 'frameAt' decodes only when the frame due
-- has changed, into a buffer of palette indices kept from call to call, and makes
-- a new Gloss bitmap from it; otherwise it hands back the picture it made last
-- time. The bitmap is drawn one film pixel to a canvas unit, centred, and
-- introScreen scales it by 'videoScale' so it spans the 1280 canvas units across
-- (a 1280 by 720 film leaves bands of 40 canvas units above and below on the 1280
-- by 800 canvas).
--
-- The decoder keeps its state in IO: an 'IORef' is a mutable variable and an
-- 'IOUArray' a mutable array of plain (unboxed) values. A new frame changes a few
-- pixels of the last one in place, where a pure version would copy the whole
-- picture every frame. 'Video' and 'Player' are exported without their
-- constructors (no @(..)@ in the export list), so other modules can only get one
-- from 'loadVideo' or 'newPlayer'; of the fields only 'vdAudio' and 'vdAudioRate'
-- are exported, for Main.
--
-- Bytes are read with Data.ByteString.Unsafe.unsafeIndex, which skips the bounds
-- check. 'parse' checks the outer file layout, but 'decode' trusts the frame
-- payload: flags, palette and span bytes are not fully checked. Damaged chunks
-- can therefore read outside their bytes or write beyond the 256-colour palette.
-- The bundled encoder supplies valid chunks; this is not a general safe decoder.
-- BangPatterns lets a loop argument be marked strict with @!@, so it is worked out
-- at each turn of the loop instead of piling up as an unevaluated sum.
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
-- Reads and writes of mutable arrays without a bounds check: the code keeps its
-- pixel indices in range; palette writes trust the encoded count (see 'decode').
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, newArray)
import Data.Array.Unboxed (UArray, bounds, listArray, (!))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
-- Strict byte strings: the whole file is one packed buffer in memory.
import qualified Data.ByteString as BS
-- 'BI.create', to fill a new byte string through a raw pointer.
import qualified Data.ByteString.Internal as BI
-- 'BU.unsafeIndex', a byte read without a bounds check.
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

-- | A film with its outer file layout checked, ready to decode. It holds the frames still
-- coded, not as pictures: decoded to RGBA, the 241 frames of 1280 by 720 would take
-- about 890 MB, against about 31 MB coded. A 'Player' decodes them a frame at a
-- time.
--
-- Every field is strict (the @!@ before its type): building a 'Video' forces it
-- to weak head normal form, not recursively through its contents. For the frame
-- list that means only its first cell. Separately, 'parse' has already walked
-- every frame to check its outer length before it builds the record.
data Video = Video
  { vdWidth     :: !Int  -- ^ the picture's width, in film pixels
  , vdHeight    :: !Int  -- ^ the picture's height, in film pixels
  , vdStarts    :: !(UArray Int Int)  -- ^ the tick each frame appears on, rising
                                      --   (index: the frame, from 0; value: ticks
                                      --   since the film began, 60 a second)
  , vdFrames    :: ![BS.ByteString]   -- ^ each frame's coded spans
                                      --   (the whole chunk after its length: flags,
                                      --   any palette, then the spans), in order.
                                      --   Each is a slice that shares the file's
                                      --   bytes rather than a copy, so the file's
                                      --   buffer stays in memory while the film does
  , vdAudioRate :: !Int  -- ^ the soundtrack's samples a second (44,100 in the game)
  , vdAudio     :: !(UArray Int Float)  -- ^ mono, peaking at 0.9; empty for none
                                        --   (the peak is measured on every seventh
                                        --   sample, so one between may be a little
                                        --   louder)
  }

-- | The film, if it can be found and read. It is looked for beside the program and
-- in each folder above it, so @cabal run@ finds the one in the project, and then
-- under the folder the game was started in.
--
-- 'Nothing' if no file is found, if it cannot be read, or if 'parse' finds it
-- malformed; the title then goes straight into level 1, and Main's @--trace@ says
-- the film was not found.
loadVideo :: IO (Maybe Video)
loadVideo = do
  -- The full path of the running program, the .exe itself.
  exe <- getExecutablePath
  -- 'ups' lists a folder and each folder above it, up to the root, where
  -- 'takeDirectory' gives back the same path and the list stops. @cabal run@ builds
  -- the program nine folders below the project (dist-newstyle, build,
  -- x86_64-windows, ghc-9.6.7, borrowed-light-0.1.0.0, x, borrowed-light, build,
  -- borrowed-light), so the project's assets folder is the tenth tried, inside the
  -- 12 kept. The list is lazy, so 'take' stops 'ups' after 12 folders. The last
  -- candidate is a relative path, looked up from the folder the game was started in.
  let ups d = let p = takeDirectory d in if p == d then [d] else d : ups p
      candidates = map (\d -> d </> "assets" </> "intro.blv") (take 12 (ups (takeDirectory exe)))
                   ++ ["assets" </> "intro.blv"]
  found <- firstExisting candidates
  case found of
    Nothing -> pure Nothing
    Just path -> do
      -- The whole file is read into memory at once. 'try' turns a failure to read
      -- it (the file vanished, no permission) into a 'Left' instead of an
      -- exception that would end the game; the type annotation says which
      -- exceptions to catch, here any 'IOException'. 'either' applies its first
      -- function to a 'Left' and its second to a 'Right', so a failed read and a
      -- file 'parse' rejects both come out as 'Nothing'.
      r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      pure (either (const Nothing) parse r)
  where
    -- The first path in the list that names an existing file, tried in order.
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else firstExisting ps

-- | How many canvas units each of the film's pixels takes, to fill the 1280 across:
-- 1 for a film 1280 across.
--
-- 2 for a film 640 across, and so on for any width. 'max 1' keeps a width of 0
-- from dividing by zero, though 'parse' refuses such a film anyway. Main passes it
-- to TalkBox.introScreen, which scales the centred picture by it both ways.
videoScale :: Video -> Float
videoScale v = 1280 / fromIntegral (max 1 (vdWidth v))

-- | How long the film runs, in ticks: its last frame is held a moment.
--
-- The tick the last frame appears on, plus 12 ticks (a fifth of a second): 612
-- for the film in the game. Main keeps it as @wIntroLen@, and when the film's age
-- reaches it, the film is over and level 1 begins. 'bounds' gives an array's lowest
-- and highest index, and @!@ reads one element; 'parse' makes sure there is at
-- least one frame, so @hi@ always names one.
videoTicks :: Video -> Int
videoTicks v = let (_, hi) = bounds (vdStarts v) in vdStarts v ! hi + 12

-- | The unsigned 16-bit number at byte @i@, little-endian: the low byte first, the
-- high byte shifted up eight bits and joined with a bitwise or ('.|.'). Each byte is
-- widened to an 'Int' ('fromIntegral') before the shift, so the shift is not
-- confined to eight bits. Unchecked: the caller must know that bytes @i@ and
-- @i + 1@ exist.
u16 :: BS.ByteString -> Int -> Int
u16 b i = fromIntegral (BU.unsafeIndex b i) .|. (fromIntegral (BU.unsafeIndex b (i + 1)) `shiftL` 8)

-- | The unsigned 32-bit number at byte @i@, little-endian: two 'u16', the low half
-- first. It is an 'Int', 64 bits on every machine the game is built for, so a
-- large u32 never comes out negative. Unchecked, like 'u16'.
u32 :: BS.ByteString -> Int -> Int
u32 b i = u16 b i .|. (u16 b (i + 2) `shiftL` 16)

-- | A whole file as a 'Video', or 'Nothing' if its parts do not fit together.
--
-- The guards are tried in order, and each makes the reads of the next safe. The
-- names in the @where@ below are only worked out when something uses them (Haskell
-- is lazy), so @w@, @h@ and @n@, bytes 4 to 9, are read only once the first guard
-- has made sure the file has its 14 bytes of header, the frame table and the
-- sound's own header only once the second has made sure they are there, and the
-- samples only once the third has. The order matters because
-- 'BU.unsafeIndex' does not check. What 'parse' does not check is inside each
-- frame: its palette and spans are trusted when 'decode' reads them.
parse :: BS.ByteString -> Maybe Video
parse b
  -- The 14-byte header is there and the file starts with the four letters.
  | BS.length b < 14 || BS.take 4 b /= BS.pack [66, 76, 86, 49] = Nothing  -- "BLV1"
  -- At least one frame, a picture with some area, and room for the frame table and
  -- the 10 bytes that open the sound. A u16 is never negative, so @<= 0@ is @== 0@.
  | n <= 0 || w <= 0 || h <= 0 || BS.length b < audioAt + 10 = Nothing
  -- Mono sound only, and room for every sample before the frames.
  | channels /= 1 || BS.length b < framesAt = Nothing
  | otherwise = do
      -- A @do@ block in 'Maybe': if 'chunks' gives 'Nothing' (a frame's length runs
      -- past the end of the file), the whole result is 'Nothing' and the record
      -- below is never built.
      frames <- chunks n framesAt
      -- Record syntax: each field set by name.
      Just Video
        { vdWidth = w, vdHeight = h
        -- The frame table, read with a list comprehension and packed by 'listArray'
        -- into an unboxed array with indices 0 to n - 1.
        , vdStarts = listArray (0, n - 1) [ u16 b (14 + 2 * j) | j <- [0 .. n - 1] ]
        , vdFrames = frames
        , vdAudioRate = rate
        -- Every sample scaled by @k@, so the loudest comes out at about 0.9. An
        -- unboxed array holds plain floats, not unevaluated ones, so building it
        -- works out every sample at once; Main uses the 'Video' at startup, so that
        -- happens before the film plays.
        , vdAudio = listArray (0, samples - 1) [ k * fromIntegral (sample i) | i <- [0 .. samples - 1] ]
        }
  where
    -- The header's three numbers that are used: width and height in pixels, and
    -- the number of frames.
    w = u16 b 4
    h = u16 b 6
    n = u16 b 8
    -- Where the sound begins: after the 14-byte header and 2 bytes a frame of
    -- frame table.
    audioAt = 14 + 2 * n
    -- The sound's own header, 10 bytes packed with no padding: rate (u32),
    -- channels (u16), number of samples (u32).
    rate = u32 b audioAt
    channels = u16 b (audioAt + 4)
    samples = u32 b (audioAt + 6)
    -- The first sample, and the first byte after the last (2 bytes a sample), where
    -- the frames begin.
    pcmAt = audioAt + 10
    framesAt = pcmAt + 2 * samples
    -- Sample @i@ as a signed number: the u16 converted to 'Int16' wraps 32,768 to
    -- 65,535 round to -32,768 to -1, which is how a signed 16-bit number is stored
    -- (two's complement).
    sample i = fromIntegral (u16 b (pcmAt + 2 * i)) :: Int16
    -- The loudest sample, looked for on every seventh one only (@[0, 7 ..]@ counts
    -- in sevens), which is quick and close enough. The @1 :@ keeps 'maximum' from
    -- failing on a film with no sound, and keeps @k@ finite. The magnitude is taken
    -- as an 'Int', where -32,768 has a positive match.
    peak = maximum (1 : [ abs (fromIntegral (sample i)) | i <- [0, 7 .. samples - 1] ]) :: Int
    k = 0.9 / fromIntegral peak :: Float
    -- @m@ frame chunks from byte @at@ on: each a u32 length and then that many bytes.
    -- 'BS.take' and 'BS.drop' make slices that share the file's bytes, with no copy.
    -- If a length runs past the end of the file, 'BS.take' gives fewer bytes than
    -- it asked for and the check fails. @(body :) <$> rest@ puts @body@ on the
    -- front of the list inside a 'Just' and leaves a 'Nothing' as it is ('<$>' is
    -- 'fmap'). To give 'Just', every frame down to the last has to pass, so the
    -- whole list has been checked by the time the result is known to be a 'Just'.
    -- Bytes after the last frame are ignored.
    chunks 0 _ = Just []
    chunks m at
      | at + 4 > BS.length b = Nothing
      | otherwise =
          let size = u32 b at
              body = BS.take size (BS.drop (at + 4) b)
          in if BS.length body /= size then Nothing else (body :) <$> chunks (m - 1) (at + 4 + size)

-- Playing ------------------------------------------------------------------------

-- | The film as it plays: the decoded frame and reusable buffers. Moving back
-- to an earlier tick replays the encoded frames from the beginning.
--
-- Mutable, and made once at startup by Main whether or not the film is watched.
-- Its pixel buffer is one byte a film pixel (about 0.9 MB at 1280 by 720), kept
-- from frame to frame because each frame only codes its changes to the one before.
-- The strictness marks force the references and arrays themselves when the
-- 'Player' is built, not what they hold.
data Player = Player
  { plVideo  :: !Video                   -- ^ the film it plays
  , plShown  :: !(IORef Int)             -- ^ the frame in the buffer; -1 before any
  , plPixels :: !(IOUArray Int Word8)    -- ^ the palette index of every pixel
                                         --   (width x height of them, row by row
                                         --   from the top-left, as the spans run)
  , plColour :: !(IOUArray Int Word32)   -- ^ the shot's palette, as RGBA in memory order
                                         --   (256 colours, each red, green, blue
                                         --   and 255 for alpha packed into one
                                         --   number from the low byte up, which a
                                         --   little-endian machine, as x86-64 and
                                         --   arm64 are, stores as the bytes R G B A)
  , plPic    :: !(IORef Picture)         -- ^ the picture made from the frame in the
                                         --   buffer, handed back until another
                                         --   frame is due
  }

-- | A player that has shown nothing yet: no frame in the buffer (-1), every pixel
-- palette index 0, every colour opaque black (0xFF000000: alpha 255 in the top
-- byte, red, green and blue 0), and a 'Blank' picture. The film's first frame
-- brings a palette and codes every pixel, so none of this is ever seen.
newPlayer :: Video -> IO Player
newPlayer v = do
  shown <- newIORef (-1)
  pixels <- newArray (0, vdWidth v * vdHeight v - 1) 0
  colour <- newArray (0, 255) 0xFF000000
  pic <- newIORef Blank
  -- The constructor applied to its fields in order, without their names.
  pure (Player v shown pixels colour pic)

-- | The film's picture at this many ticks in, centred, one pixel per canvas unit.
--
-- @tick@ counts from the film's start (Main passes the @Intro@ mode's age). The
-- frame due is the newest whose start tick has come; if it is the one already in
-- the buffer, which is the usual case since a frame lasts 2 or 3 ticks, the picture
-- made last time comes back at once. Otherwise the frames up to it are decoded and
-- a new picture is made. Ticks that skip a frame (after a slow moment) still decode
-- it, since each frame only makes sense on top of the one before.
frameAt :: Player -> Int -> IO Picture
frameAt p tick = do
  shown <- readIORef (plShown p)
  -- Every frame whose start has come, in order, so the last is the newest. The
  -- @0 :@ keeps the list from being empty, so 'last' is safe, and a tick before
  -- the first frame's start gives frame 0. It looks at every frame on every call:
  -- a few hundred comparisons, nothing beside a decode.
  let v = plVideo p
      (_, hi) = bounds (vdStarts v)
      wanted = last (0 : [ k | k <- [0 .. hi], vdStarts v ! k <= tick ])
  if wanted == shown
    then readIORef (plPic p)
    else do
      -- Frames only code their changes, so the way back is from the first.
      -- (The first frame carries its shot's palette and codes every pixel, so
      -- decoding from it rebuilds the picture from nothing; going back happens only
      -- if the film is started again.) Going forward, the frames after the one
      -- shown are decoded in order, up to and including the one wanted.
      let from = if wanted < shown then 0 else shown + 1
      -- 'decode p' is 'decode' given its first argument only: a function that is
      -- still waiting for a frame's bytes, which 'forM_' hands it one by one.
      forM_ (take (wanted - from + 1) (drop from (vdFrames v))) (decode p)
      pic <- picture p
      writeIORef (plShown p) wanted
      writeIORef (plPic p) pic
      pure pic

-- | Apply one frame's chunk to the player: load its palette, if it brings one, and
-- write its spans into the pixel buffer. See the module header for the layout.
decode :: Player -> BS.ByteString -> IO ()
decode p b = do
  -- Only bit 0 of the flags means anything: a palette follows.
  let flags = BU.unsafeIndex b 0
  -- An @if@ whose branches are both actions, giving the byte where the spans
  -- start: 1 (just after the flags) with no palette, or after the palette's u16
  -- count and its three bytes a colour. 'pure 1' does nothing and gives 1. The
  -- count is not checked against the palette's 256 places: the encoder always
  -- writes 256, and a larger count in a damaged file would write past the array.
  start <- if flags .&. 1 == 0 then pure 1 else do
    let count = u16 b 1
    forM_ [0 .. count - 1] $ \i -> do
      let at = 3 + 3 * i
          r = fromIntegral (BU.unsafeIndex b at) :: Word32
          g = fromIntegral (BU.unsafeIndex b (at + 1))
          bl = fromIntegral (BU.unsafeIndex b (at + 2))
      -- Red in the low byte, then green, then blue, and 255 in the top byte for
      -- alpha: see 'plColour'. (@bl@, since @b@ is the frame's bytes.)
      unsafeWrite (plColour p) i (r .|. (g `shiftL` 8) .|. (bl `shiftL` 16) .|. 0xFF000000)
    pure (3 + 3 * count)
  let size = BS.length b
      total = vdWidth (plVideo p) * vdHeight (plVideo p)
      -- One span at a time: @at@ is the byte the span starts at in the chunk, @px@
      -- the pixel it starts at in the picture. 'when' runs its action only if the
      -- condition holds and otherwise does nothing, which ends the loop: at the end
      -- of the chunk's bytes, or once every pixel is written. A frame whose spans
      -- stop short of the last pixel leaves the rest as they were, as a keep would.
      spans :: Int -> Int -> IO ()
      spans !at !px = when (at < size && px < total) $ do
        -- The span's first byte: the kind in its top two bits, the length less one
        -- in its low six ('.&. 63' keeps those six).
        let op = BU.unsafeIndex b at
            short = fromIntegral (op .&. 63) :: Int
            -- @n0@ is the length as coded and @body@ the byte where the span's data,
            -- if any, begins: after a u16 for a long span, straight after the first
            -- byte for a short one.
            (n0, body) = if short == 63 then (u16 b (at + 1) + 64, at + 3) else (short + 1, at + 1)
            -- Never write past the last pixel, whatever a span claims.
            n = min n0 (total - px)
        case op `shiftR` 6 of
          -- Keep: the pixels stay as they are; just move past them.
          0 -> spans body (px + n)
          -- Fill: one palette index, written into every pixel of the span.
          1 -> do
            let c = BU.unsafeIndex b body
            forM_ [px .. px + n - 1] $ \i -> unsafeWrite (plPixels p) i c
            spans (body + 1) (px + n0)
          -- Copy (kind 2, and the unused 3): the span's own pixel bytes, one each.
          -- The read position moves on by the coded length @n0@. Where @n@ was cut
          -- short, @px + n0@ is past the end, the same as @px + n@ would be: either
          -- way the loop stops.
          _ -> do
            forM_ [0 .. n - 1] $ \i -> unsafeWrite (plPixels p) (px + i) (BU.unsafeIndex b (body + i))
            spans (body + n0) (px + n0)
  spans start 0

-- | The pixel buffer as a Gloss bitmap: each pixel's palette index looked up in the
-- palette, four bytes a pixel.
--
-- 'BI.create' makes a new byte string of the given size and hands the function a
-- pointer to its bytes to fill; 'castPtr' lets those bytes be written as 'Word32'
-- (one whole pixel a write), and 'pokeElemOff' writes the @i@-th of them. About
-- 920,000 pixels for the film in the game, done only when a new frame is due,
-- about 24 times a second.
--
-- 'bitmapOfByteString' takes the width and height in pixels; 'TopToBottom' says the
-- first row in memory is the top of the picture (the encoder turns Blender's
-- bottom-up rows over, so it is); 'PxRGBA' says each pixel's bytes are red, green,
-- blue and alpha, in that order, which is what 'plColour' holds; and 'False' tells
-- Gloss not to keep the texture from one drawn frame to the next, as Gloss advises
-- for a picture made afresh, since each film frame is a new bitmap. Gloss copies the
-- bytes into a bitmap of its own, and draws it centred on the origin.
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
