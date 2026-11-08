-- | The courier's talk, on screen: her portrait rising from the bottom left, a
-- tilted black speech box with a white rim and a yellow shadow, a name tag, the line
-- typing out in the pixel font, and, on the board, a marker on what she is talking
-- about. Everything is drawn in whole pixels three canvas units square, the size of
-- the portrait's own pixels, so the box, the letters and her face are one grid.
--
-- Also the opening film's screen, which shares the pixel font.
-- Game.Story owns dialogue timing and focus; Game.Run owns film/skip timing.
-- Render supplies the board placement, Main supplies the decoded film picture,
-- and this module returns Gloss pictures without changing either state.
-- Coordinates are on the centred 1280 by 800 canvas, y upwards. Ages and clock
-- are 60 Hz ticks, not seconds or the puzzle's budget. Portrait's source rows
-- instead count pixels right/down; patch converts them into canvas coordinates.
module TalkBox
  ( talkLayer
  , askLayer
  , introScreen
  , pixelPoly
  , px
  ) where

import Data.Char (isAlpha, isUpper)
import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Draw
import Game.Run (introSkipTicks)
import Game.Story
import Game.Types (Era (..))
import Graphics.Gloss
import PixelFont
import Portrait

-- | One pixel, in canvas units.
px :: Float
px = 3

-- | Round a canvas coordinate to the nearest three-unit pixel-grid position.
-- This prevents tiny smooth motion from blurring the intended stepped edges.
snap :: Float -> Float
snap v = px * fromIntegral (round (v / px) :: Int)

-- | Shared 0xRRGGBB palette: black panel, pale edge, yellow accent, subdued text
-- and dark backdrop. Opacity is applied separately when a layer fades.
ink, rim, sun, muted, night :: Int
ink = 0x0D0B12   -- the box
rim = 0xF4F1EA   -- its edge, and the letters
sun = 0xF6B31C   -- her jacket: the shadow, the tag, words that matter
muted = 0x8C8796
night = 0x05080A

-- Shapes in whole pixels -----------------------------------------------------------

-- | A convex polygon filled pixel row by pixel row, so its slanted edges step like
-- pixel art. Rows with the same span are drawn as one rectangle.
pixelPoly :: [(Float, Float)] -> Picture
pixelPoly pts = Pictures [ rect x0 y0 (x1 - x0) (y1 - y0) | (y0, y1, x0, x1) <- merged ]
  where
    -- Input must be nonempty and convex. Sample each pixel row at its centre;
    -- taking the extreme intersections fills one span, suitable for these boxes.
    ys = map snd pts
    lo = px * fromIntegral (floor (minimum ys / px) :: Int)
    hi = px * fromIntegral (ceiling (maximum ys / px) :: Int)
    -- The Just pattern discards rows with no intersections. Snapping both ends
    -- gives whole pixels; collapsed spans disappear rather than draw slivers.
    rows = [ (y, snap a, snap b) | y <- takeWhile (< hi) (iterate (+ px) lo)
                                 , Just (a, b) <- [across (y + px / 2)], snap b > snap a ]
    edges = zip pts (drop 1 pts ++ take 1 pts)
    -- Interpolate x along each crossed edge. Half-open y intervals count a shared
    -- vertex once, and exclude horizontal edges so the divisor cannot be zero.
    across yc = case [ x1 + (yc - y1) * (x2 - x1) / (y2 - y1)
                     | ((x1, y1), (x2, y2)) <- edges, (y1 <= yc && yc < y2) || (y2 <= yc && yc < y1) ] of
      [] -> Nothing
      xs -> Just (minimum xs, maximum xs)
    -- foldl' forces each accumulator before the next row. Build it backwards,
    -- merging equal adjacent spans, then reverse to restore painting order.
    merged = reverse (foldl' join [] rows)
    join ((y0, y1, a, b) : rest) (y, a', b')
      | a == a' && b == b' && abs (y - y1) < 0.01 = (y0, y + px, a, b) : rest
    -- A changed span starts another rectangle instead of stretching old pixels.
    join acc (y, a, b) = (y, y + px, a, b) : acc

-- | Scale a shape about a point.
scaled :: Float -> (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
scaled k (cx, cy) = map (\(x, y) -> (cx + (x - cx) * k, cy + (y - cy) * k))

-- | Translate every polygon vertex by a canvas-unit displacement before filling.
moved :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
moved (dx, dy) = map (\(x, y) -> (x + dx, y + dy))

-- The portrait ---------------------------------------------------------------------

-- | Cached source-sized RGBA bitmap. TopToBottom matches Portrait's row order;
-- the final True lets Gloss cache its texture rather than upload it each frame.
portraitBitmap :: Picture
portraitBitmap = bitmapOfByteString portraitW portraitH (BitmapFormat TopToBottom PxRGBA) bytes True
  where
    -- Map lookup is case-sensitive; unlisted characters (including dots) become
    -- transparent black. Palette integers split into red, green, blue bytes.
    colours = Map.fromList portraitPalette
    bytes = BS.pack (concatMap pixel (concat portraitRows))
    pixel c = case Map.lookup c colours of
      Nothing -> [0, 0, 0, 0]
      Just v -> [fromIntegral (v `div` 65536), fromIntegral (v `div` 256 `mod` 256), fromIntegral (v `mod` 256), 255]

-- | Pixels laid over the portrait, in its own coordinates, centred like the bitmap.
patch :: [(Int, Int, Char)] -> Picture
patch ps = Pictures
  [ Color (hex v) (rect (fromIntegral x * px - w / 2) (h / 2 - fromIntegral (y + 1) * px) px px)
  | (x, y, c) <- ps, Just v <- [lookup c portraitPalette] ]
  where
    w = fromIntegral portraitW * px
    h = fromIntegral portraitH * px

-- | Shared overlay pictures for the three facial poses, already at px scale.
-- Top-level bindings are lazy and reused once built, rather than rebuilt per tick.
blinkPic, mouthOpenPic, mouthShutPic :: Picture
blinkPic = patch blinkPatch
mouthOpenPic = patch mouthOpenPatch
mouthShutPic = patch mouthShutPatch

-- | Where the portrait stands: its left edge just off screen, its bottom below it.
portraitAt :: (Float, Float)
portraitAt = (-645 + fromIntegral portraitW * px / 2, -420 + fromIntegral portraitH * px / 2)

-- The box ---------------------------------------------------------------------------

-- | Panel and slightly larger rim vertices, in canvas units and perimeter order.
-- Their sloping edges are rasterised by pixelPoly rather than antialiased.
boxShape, rimShape :: [(Float, Float)]
boxShape = [(-327, -387), (591, -369), (606, -219), (-306, -237)]
rimShape = [(-336, -396), (600, -378), (615, -210), (-315, -228)]

-- | Pivot for the panel's pop animation, in canvas units.
boxCentre :: (Float, Float)
boxCentre = (140, -300)

-- | The speech tail, pointing up at her.
tailRim, tailInk :: [(Float, Float)]
tailRim = [(-324, -252), (-321, -306), (-381, -228)]
tailInk = [(-312, -261), (-309, -297), (-363, -239)]

-- | Main name tag and smaller occupation tag, placed over the panel's upper left.
tagShape, subTagShape :: [(Float, Float)]
tagShape = [(-321, -246), (-150, -240), (-141, -192), (-312, -198)]
subTagShape = [(-162, -237), (-39, -233), (-33, -207), (-156, -211)]

-- | First dialogue row's position in canvas units, before the 15-unit lift.
textLeft, textTop :: Float
textLeft = -282
textTop = -255

-- | Pixels a line of her talk may take across.
textWidth :: Int
textWidth = 286

-- | The words she leans on, in capitals, are in yellow.
loud :: String -> Bool
-- Ignore punctuation, but require at least two letters so a lone capital does
-- not turn yellow. Both casing and character classification use Data.Char.
loud w = let letters = filter isAlpha w in length letters >= 2 && all isUpper letters

-- | Part of a line typed so far, each word in its colour. Letters of one colour in a
-- row are drawn together, so a line is a handful of pictures, not one per letter.
typedLine :: Int -> String -> Picture
typedLine shown s = Pictures (go 0 (runs (zip (take shown s) (colours s))))
  where
    -- The colour of each letter, from the word it belongs to in the whole line.
    colours str = concatMap (\w -> replicate (length w) (loud w)) (chunks str)
    -- Keep runs of spaces: using words would lose character positions and make
    -- the typed prefix disagree with Story's character count.
    chunks str = case str of
      [] -> []
      (c : _) | c == ' ' -> let (sp, rest) = span (== ' ') str in sp : chunks rest
              | otherwise -> let (w, rest) = break (== ' ') str in w : chunks rest
    -- span groups only adjacent equal emphasis. The whole word decides colour
    -- before truncation, so an incomplete capitalised word never changes colour.
    runs xs = case xs of
      [] -> []
      ((_, l) : _) -> let (same, rest) = span ((== l) . snd) xs in (map fst same, l) : runs rest
    go _ [] = []
    -- pixelW omits the trailing gap; restore one pixel between runs so splitting
    -- at a colour change does not alter the text's measured width.
    go x ((part, l) : rest) =
      Translate (fromIntegral x * px) 0 (Color (hex (if l then sun else rim)) (pixelText px part))
        : go (x + pixelW part + 1) rest

-- | The box and everything on it, popped up by @k@ (0 to 1) and at opacity @a@.
box :: Float -> Float -> Talk -> Int -> Picture
box k a t clock = boxWith k a (lnText (currentLine t)) (typedChars t) corner
  where
    corner = Pictures
      ( [ next | lineTyped t ]
      ++ [ Translate 573 (-226) (Color (hex muted) (pixelTextC 2 (show (tkLine t + 1) ++ "/" ++ show (length (tkLines t))))) ] )
    -- One-pixel prompt bob every 14 ticks, shown only after the line is typed.
    bob = if (clock `div` 14) `mod` 2 == 0 then 0 else -px
    next = Translate 0 bob (Pictures
      [ Color (hex sun) (pixelPoly [(552, -345), (576, -345), (564, -360)])
      , Color (hex muted) (Translate 474 (-345) (pixelText 2 "ENTER"))
      ])

-- | The box with a line, @typed@ letters of it shown, and something in its corner.
boxWith :: Float -> Float -> String -> Int -> Picture -> Picture
boxWith k a said typed corner = Pictures
  [ Color (fade a (hex sun)) (pixelPoly (moved (12, -12) (shape rimShape)))
  , Color (fade a (hex rim)) (pixelPoly (shape rimShape))
  , Color (fade a (hex rim)) (pixelPoly (shape tailRim))
  , Color (fade a (hex ink)) (pixelPoly (shape boxShape))
  , Color (fade a (hex ink)) (pixelPoly (shape tailInk))
  , if k > 0.9 then fadePicture a body else Blank
  ]
  where
    -- k changes geometry about boxCentre, while a changes alpha. Delay the text
    -- until the panel is nearly full size, keeping the glyphs on their own grid.
    shape = scaled (0.72 + 0.28 * k) boxCentre
    rows = pixelWrap textWidth said
    -- Letters shown on each row, counting the one space each row break took.
    shownRows = snd (foldl' (\(left, acc) r -> (max 0 (left - length r - 1), acc ++ [min (length r) left]))
                             (typed, []) rows)
    body = Pictures $
      [ Translate textLeft (textTop - fromIntegral (i * pixelLineH) * px) (typedLine n r)
      | (i, (r, n)) <- zip [0 :: Int ..] (zip rows shownRows) ]
      ++ [ corner ]

-- | The name tag over the box's corner.
tag :: Float -> Picture
tag a = fadePicture a (Pictures
  [ Color (hex sun) (pixelPoly subTagShape)
  , Color (hex ink) (Translate (-129) (-213) (pixelText 2 "COURIER"))
  , Color (hex rim) (pixelPoly tagShape)
  , Color (hex ink) (Translate (-297) (-204) (pixelText 4 speakerName))
  ])

-- | Draw the face at a slide fraction (0 home, 1 displaced left by 150 units),
-- using Talk's typing age and a global tick clock for mouth, bob and blink.
-- The bitmap scales once; patches are already expressed in canvas units.
portrait :: Float -> Talk -> Int -> Picture
portrait slide t = face slide (not (lineTyped t)) (tkAge t) (drop (max 0 (typedChars t - 1)) (lnText (currentLine t)))

-- | Her face, slid off to the left by @slide@: talking while @typing@, @age@ ticks
-- into the line, @letter@ onwards being the one just typed.
face :: Float -> Bool -> Int -> String -> Int -> Picture
face slide typing age letter clock = Translate (fst portraitAt - snap (slide * 150)) (snd portraitAt + bob) (Pictures
  [ Scale px px portraitBitmap
  , if blinking then blinkPic else Blank
  , mouth
  ])
  where
    -- A blink every few seconds, a little irregular.
    blinking = clock `mod` 233 < 7 || clock `mod` 541 < 6
    -- Five-tick mouth phases skip spaces; the vertical bob uses ten ticks.
    mouth
      | typing && (age `div` 5) `mod` 2 == 0 && take 1 letter /= " " = mouthOpenPic
      | typing = mouthShutPic
      | otherwise = Blank
    bob = if typing && (age `div` 10) `mod` 2 == 1 then px else 0

-- The marker on the board --------------------------------------------------------

-- | Short uppercase label for each story focus target; this is display text only.
thingName :: Thing -> String
thingName th = case th of
  TheWick -> "WICK"
  TheLamp -> "LAMP"
  TheCrate -> "CRATE"
  TheSled -> "MIRROR"

-- | The board dims around what she is talking about; brackets close on its cell and
-- an arrow over it carries its name. @f@ is the board's stand; @age@ the ticks the
-- line has been up.
marker :: Float -> Look -> Int -> Int -> Picture
marker f lk age clock = Pictures
  [ Color (hexA night (0.5 * open)) (Translate cx cy (ring 48 (cell * (0.75 + 6 * (1 - open))) 2200 1))
  , Pictures [ corner sx sy | sx <- [-1, 1], sy <- [-1, 1] ]
  , Translate (snap cx) (snap (cy + cell / 2 + 18) + bob) arrow
  , Translate (snap cx) (snap (cy + cell / 2 + 72) + bob) label
  ]
  where
    -- The marker is a canvas overlay, so transform the cell through the board's
    -- current placement. Taking the difference of two transformed x positions
    -- measures one scaled cell without retaining the board's translation.
    (cx, cy) = onBoard f (cellPos (lkCell lk))
    cell = fst (onBoard f (cs, 0)) - fst (onBoard f (0, 0))
    -- Over 12 ticks the dark ring's hole shrinks round the named cell. A wide
    -- annulus dims the rest of the screen without covering the target itself.
    open = smooth (fromIntegral age / 12)
    pulse = if (clock `div` 12) `mod` 2 == 0 then 0 else px
    bob = if (clock `div` 15) `mod` 2 == 0 then 0 else px * 2
    -- A bracket on one corner of the cell, in pixels, outlined.
    corner sx sy =
      let r = snap (cell / 2 + 6 - pulse)
          x = snap cx + sx * r
          y = snap cy + sy * r
          arm = 18
          l = [ (x - (if sx > 0 then arm else 0), y - (if sy > 0 then px * 2 else 0), arm, px * 2)
              , (x - (if sx > 0 then px * 2 else 0), y - (if sy > 0 then arm else 0), px * 2, arm) ]
      in Pictures
           ( [ Color (hex night) (rect (bx - px) (by - px) (bw + 2 * px) (bh + 2 * px)) | (bx, by, bw, bh) <- l ]
          ++ [ Color (hex sun) (rect bx by bw bh) | (bx, by, bw, bh) <- l ] )
    -- Each arrow row stores left offset and run length in pixels. The first
    -- pass draws an expanded dark outline; the second draws the yellow centre.
    arrowRows = [ (0, 7), (0, 7), (0, 7), (-3, 13), (-2, 11), (-1, 9), (0, 7), (1, 5), (2, 3), (3, 1) ] :: [(Int, Int)]
    arrow = Pictures
      [ Pictures [ Color (hex night) (rect (fromIntegral (x0 - 1) * px - 10.5) (fromIntegral (9 - i) * px - px) (fromIntegral (n + 2) * px) (px * 3))
                 | (i, (x0, n)) <- zip [0 :: Int ..] arrowRows ]
      , Pictures [ Color (hex sun) (rect (fromIntegral x0 * px - 10.5) (fromIntegral (9 - i) * px) (fromIntegral n * px) px)
                 | (i, (x0, n)) <- zip [0 :: Int ..] arrowRows ]
      ]
    -- Its name, and the era it is in: the board may be showing the other one.
    name = thingName (lkThing lk)
    era = case lkEra lk of
      Past -> "PAST"
      Present -> "PRESENT"
    -- These labels use two-unit font pixels. Measure the combined label before
    -- centring its backing, then snap the backing to the panel's three-unit grid.
    nameW = fromIntegral (pixelW name) * 2
    w = fromIntegral (pixelW (name ++ " - " ++ era)) * 2 + 12
    left = snap (negate w / 2) + 6
    label = Pictures
      [ Color (hex sun) (rect (snap (negate w / 2) - px) (-24) (snap w + 2 * px) 27)
      , Color (hex ink) (rect (snap (negate w / 2)) (-21) (snap w) 21)
      , Color (hex rim) (Translate left (-4) (pixelText 2 name))
      , Color (hex muted) (Translate (left + nameW + 6) (-4) (pixelText 2 "-"))
      , Color (hex sun) (Translate (left + nameW + fromIntegral (pixelW " - ") * 2 + 2) (-4) (pixelText 2 era))
      ]

-- | Everything the talk draws over the level, in canvas units: the marker on the
-- board (whose stand is @f@), then her portrait and the box.
talkLayer :: Story -> Float -> Int -> Picture
talkLayer s f clock = case (stTalk s, stClosing s) of
  -- A pattern guard chooses an active, visible talk before considering closure.
  -- The eased opening fraction slides the portrait and scales the panel together.
  (Just t, _) | talkShown t ->
    let k = smooth (fromIntegral (tkShown t) / fromIntegral popTicks)
        -- Overshoots a little as it lands, like a box slapped down.
        pop = min 1.04 (k * 1.04) - (if k >= 1 then 0.04 else 0)
    in Pictures
         [ maybe Blank (\lk -> marker f lk (tkAge t) clock) (lnLook (currentLine t))
         , portrait (1 - k) t clock
         , raised (box pop 1 t clock)
         , raised (tag k)
         ]
  -- Closing keeps a copy of the old talk. This record update changes only its
  -- drawing age: a very large age makes all letters visible as it slides away.
  (_, Just (t, n)) ->
    let gone = fromIntegral n / fromIntegral closeTicks
    in Pictures
         [ portrait (smooth gone) t { tkAge = 10 ^ (6 :: Int) } clock
         , raised (box (1 - 0.3 * gone) (1 - gone) t { tkAge = 10 ^ (6 :: Int) } clock)
         , raised (tag (1 - gone))
         ]
  -- A waiting/unshown story with no closing panel contributes no overlay.
  _ -> Blank
  where
    -- The box stands a little off the bottom, so its yellow shadow shows.
    raised = Translate 0 15

-- | Her question before an order is cancelled, in the talk's own box: @k@ (0 to 1)
-- as it comes up, the line with @typed@ letters shown, @age@ ticks since she asked,
-- and, in the box's corner where her talk says ENTER, how much of the hold that
-- tears the order is done (0 to 1), filling as the film's skip bar does.
askLayer :: Float -> String -> Int -> Int -> Float -> Int -> Picture
askLayer k said typed age heldK clock
  | k <= 0.01 = Blank
  | otherwise = Pictures
      [ face (1 - k) typing age (drop (max 0 (typed - 1)) said) clock
      , raised (boxWith (min 1.04 (k * 1.04) - (if k >= 1 then 0.04 else 0)) (min 1 (k * 1.5)) said typed corner)
      , raised (tag k)
      ]
  where
    typing = typed < length said
    raised = Translate 0 15
    hint = "HOLD ENTER"
    hintW = fromIntegral (pixelW hint) * 2
    corner = Pictures
      [ Color (hex (if heldK > 0 then rim else muted)) (Translate (588 - hintW) (-330) (pixelText 2 hint))
      , Color (hexA 0xFFFFFF 0.18) (rect (588 - hintW) (-354) hintW 3)
      , Color (hex sun) (rect (588 - hintW) (-354) (hintW * max 0 (min 1 heldK)) 3)
      ]

-- The opening film's screen ------------------------------------------------------

-- | The film, each of its pixels @k@ canvas units square, framed by black, fading
-- in from the title; and, in the bar under it, how to skip it, filling while Enter
-- is held.
introScreen :: Float -> Maybe Picture -> Int -> Int -> Picture
introScreen k frame age held = Pictures
  [ Color black (rectC canvasW canvasH)
  , maybe Blank (Scale k k) frame
  , Color (hexA 0x000000 (1 - smooth (fromIntegral age / 14))) (rectC canvasW canvasH)
  , Translate 594 (-370) (Pictures
      [ Color (hex (if held > 0 then rim else muted)) (Translate (negate (fromIntegral (pixelW hint) * 2)) 0 (pixelText 2 hint))
      , Color (hexA 0xFFFFFF 0.18) (rect (-fromIntegral (pixelW hint) * 2) (-24) (fromIntegral (pixelW hint) * 2) 3)
      , Color (hex sun) (rect (-fromIntegral (pixelW hint) * 2) (-24) (fromIntegral (pixelW hint) * 2 * heldK) 3)
      ])
  ]
  where
    hint = "HOLD ENTER TO SKIP"
    -- held is consecutive skip-hold ticks; clamp the bar at the threshold even
    -- if a final drawn frame still carries a count beyond introSkipTicks.
    heldK = min 1 (fromIntegral held / fromIntegral introSkipTicks)
