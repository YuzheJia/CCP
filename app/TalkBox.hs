-- | The talk, on screen: the speaker's portrait rising from a bottom corner, a
-- tilted black speech box with a white rim and a shadow in the speaker's colour, a
-- name tag, the line typing out in the pixel font, and, on the board, a marker on
-- what they are talking about. The courier stands at the left in yellow; the
-- lighthouse keeper at the right in red, with the box turned about to face them.
-- Everything is drawn in whole pixels three canvas units square, the size of the
-- portraits' own pixels, so the box, the letters and the faces are one grid.
--
-- Also the card that says every order is delivered, in the same style: once falsely,
-- as the last listed lamp burns, and once truly, after the last talk of all; the
-- courier's question before an order is cancelled (the save screen's); and the
-- opening film's screen, which shares the pixel font.
--
-- Game.Story owns dialogue timing and focus; Game.Run owns film/skip timing.
-- Render supplies the board placement, Main supplies the decoded film picture,
-- and this module returns Gloss pictures without changing either state.
-- Coordinates are on the centred 1280 by 800 canvas, y upwards. Ages and clock
-- are 60 Hz ticks, not seconds or the puzzle's budget. The portraits' source rows
-- instead count pixels right/down; 'faceOf' converts them into canvas coordinates.
module TalkBox
  ( talkLayer
  , endingLayer
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
import Game.Run (Finale (..), Stage (..), introSkipTicks)
import Game.Story
import Game.Types (Era (..))
import Graphics.Gloss
import qualified KeeperPortrait as KP
import PixelFont
import qualified Portrait as CP

-- | One pixel, in canvas units.
px :: Float
px = 3

-- | Round a canvas coordinate to the nearest three-unit pixel-grid position.
-- This prevents tiny smooth motion from blurring the intended stepped edges.
snap :: Float -> Float
snap v = px * fromIntegral (round (v / px) :: Int)

-- | Shared 0xRRGGBB palette: black panel, pale edge, the speakers' accents, subdued
-- text and dark backdrop. Opacity is applied separately when a layer fades.
ink, rim, sun, ember, muted, night :: Int
ink = 0x0D0B12   -- the box
rim = 0xF4F1EA   -- its edge, and the letters
sun = 0xF6B31C   -- her jacket: the shadow, the tag, words that matter
ember = 0xE5533A -- the keeper's: the second keeper's colour, for their box the same
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

-- The faces -------------------------------------------------------------------------

-- | A portrait, already at px scale, and the pixels laid over it to blink and to
-- talk, already in canvas units.
data Face = Face
  { fcPicture :: Picture
  , fcBlink, fcOpen, fcShut :: Picture
  , fcW, fcH :: Int
  }

-- | A face from a portrait's rows of letters, its palette and its patches.
-- The bitmap is source-sized RGBA. TopToBottom matches the rows' order; the final
-- True lets Gloss cache its texture rather than upload it each frame. Unlisted
-- characters (including dots) become transparent black. Palette integers split
-- into red, green, blue bytes.
faceOf :: Int -> Int -> [(Char, Int)] -> [String]
       -> [(Int, Int, Char)] -> [(Int, Int, Char)] -> [(Int, Int, Char)] -> Face
faceOf w h palette rows blink open shut = Face
  { fcPicture = Scale px px (bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bytes True)
  , fcBlink = patch blink, fcOpen = patch open, fcShut = patch shut
  , fcW = w, fcH = h
  }
  where
    colours = Map.fromList palette
    bytes = BS.pack (concatMap pixel (concat rows))
    pixel c = case Map.lookup c colours of
      Nothing -> [0, 0, 0, 0]
      Just v -> [fromIntegral (v `div` 65536), fromIntegral (v `div` 256 `mod` 256), fromIntegral (v `mod` 256), 255]
    -- Pixels laid over the portrait, in its own coordinates, centred like the bitmap.
    patch ps = Pictures
      [ Color (hex v) (rect (fromIntegral x * px - wf / 2) (hf / 2 - fromIntegral (y + 1) * px) px px)
      | (x, y, c) <- ps, Just v <- [Map.lookup c colours] ]
    wf = fromIntegral w * px
    hf = fromIntegral h * px

-- | The two faces. Top-level bindings are lazy and reused once built, rather than
-- rebuilt per tick.
courierFace, keeperFace :: Face
courierFace = faceOf CP.portraitW CP.portraitH CP.portraitPalette CP.portraitRows
                     CP.blinkPatch CP.mouthOpenPatch CP.mouthShutPatch
keeperFace = faceOf KP.portraitW KP.portraitH KP.portraitPalette KP.portraitRows
                    KP.blinkPatch KP.mouthOpenPatch KP.mouthShutPatch

-- | How a speaker is drawn: their face, the colour of their shadow, tag and loud
-- words, their name and what they are, and whether they stand at the right.
data Voice = Voice
  { vcFace   :: Face
  , vcAccent :: Int
  , vcName   :: String
  , vcRole   :: String
  , vcRight  :: Bool
  }

voiceOf :: Speaker -> Voice
voiceOf who = case who of
  Courier -> Voice courierFace sun speakerName "COURIER" False
  Keeper -> Voice keeperFace ember "KEEPER" "LIGHTHOUSE" True

voiceOfTalk :: Talk -> Voice
voiceOfTalk = voiceOf . lnWho . currentLine

-- | The layout is drawn for the left; a speaker at the right sees it turned about.
-- Shapes are mirrored point by point; a run of text @w@ units wide that starts at
-- @x@ starts at @-(x + w)@ instead, so it still reads left to right.
side :: Voice -> [(Float, Float)] -> [(Float, Float)]
side v = if vcRight v then map (\(x, y) -> (negate x, y)) else id

startX :: Voice -> Float -> Float -> Float
startX v x w = if vcRight v then negate (x + w) else x

-- | Where the portrait stands: an edge just off screen, its bottom below it.
portraitAt :: Voice -> (Float, Float)
portraitAt v =
  ( startX v (-645) w + w / 2
  , -420 + fromIntegral (fcH (vcFace v)) * px / 2 )
  where w = fromIntegral (fcW (vcFace v)) * px

-- The box ---------------------------------------------------------------------------

-- | Panel and slightly larger rim vertices, in canvas units and perimeter order,
-- for a speaker at the left. Their sloping edges are rasterised by pixelPoly rather
-- than antialiased.
boxShape, rimShape :: [(Float, Float)]
boxShape = [(-327, -387), (591, -369), (606, -219), (-306, -237)]
rimShape = [(-336, -396), (600, -378), (615, -210), (-315, -228)]

-- | Pivot for the panel's pop animation, in canvas units.
boxCentre :: (Float, Float)
boxCentre = (140, -300)

-- | The speech tail, pointing up at the speaker.
tailRim, tailInk :: [(Float, Float)]
tailRim = [(-324, -252), (-321, -306), (-381, -228)]
tailInk = [(-312, -261), (-309, -297), (-363, -239)]

-- | Main name tag, placed over the panel's upper corner by the speaker.
tagShape :: [(Float, Float)]
tagShape = [(-321, -246), (-150, -240), (-141, -192), (-312, -198)]

-- | The smaller tag under the name, saying what the speaker is: long enough for the
-- courier's, and longer for a longer word.
subTagShape :: String -> [(Float, Float)]
subTagShape role = [(-162, -237), (-39 + extra, -233), (-33 + extra, -207), (-156, -211)]
  where extra = max 0 (fromIntegral (pixelW role - pixelW "COURIER") * 2)

-- | First dialogue row's position in canvas units, before the 15-unit lift.
textLeft, textTop :: Float
textLeft = -282
textTop = -255

-- | Pixels a line of talk may take across.
textWidth :: Int
textWidth = 286

-- | The words a speaker leans on, in capitals, are in their colour.
loud :: String -> Bool
-- Ignore punctuation, but require at least two letters so a lone capital does
-- not change colour. Both casing and character classification use Data.Char.
loud w = let letters = filter isAlpha w in length letters >= 2 && all isUpper letters

-- | Part of a line typed so far, each word in its colour (@accent@ for the loud
-- ones). Letters of one colour in a row are drawn together, so a line is a handful
-- of pictures, not one per letter.
typedLine :: Int -> Int -> String -> Picture
typedLine accent shown s = Pictures (go 0 (runs (zip (take shown s) (colours s))))
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
      Translate (fromIntegral x * px) 0 (Color (hex (if l then accent else rim)) (pixelText px part))
        : go (x + pixelW part + 1) rest

-- | The box and everything on it, popped up by @k@ (0 to 1) and at opacity @a@.
box :: Float -> Float -> Talk -> Int -> Picture
box k a t clock = boxWith v k a (lnText (currentLine t)) (typedChars t) corner
  where
    v = voiceOfTalk t
    counter = show (tkLine t + 1) ++ "/" ++ show (length (tkLines t))
    -- The line count in the far top corner, the ENTER prompt bottom right,
    -- whichever way the box faces.
    corner = Pictures
      ( [ next | lineTyped t ]
      ++ [ Translate (if vcRight v then -573 else 573) (-226) (Color (hex muted) (pixelTextC 2 counter)) ] )
    -- One-pixel prompt bob every 14 ticks, shown only after the line is typed.
    bob = if (clock `div` 14) `mod` 2 == 0 then 0 else -px
    nextX = if vcRight v then -270 else 0
    next = Translate nextX bob (Pictures
      [ Color (hex (vcAccent v)) (pixelPoly [(552, -345), (576, -345), (564, -360)])
      , Color (hex muted) (Translate 474 (-345) (pixelText 2 "ENTER"))
      ])

-- | The box for voice @v@ with a line, @typed@ letters of it shown, and something
-- in its corner.
boxWith :: Voice -> Float -> Float -> String -> Int -> Picture -> Picture
boxWith v k a said typed corner = Pictures
  [ Color (fade a (hex (vcAccent v))) (pixelPoly (moved (12, -12) (shape rimShape)))
  , Color (fade a (hex rim)) (pixelPoly (shape rimShape))
  , Color (fade a (hex rim)) (pixelPoly (shape tailRim))
  , Color (fade a (hex ink)) (pixelPoly (shape boxShape))
  , Color (fade a (hex ink)) (pixelPoly (shape tailInk))
  , if k > 0.9 then fadePicture a body else Blank
  ]
  where
    -- k changes geometry about boxCentre, while a changes alpha. Delay the text
    -- until the panel is nearly full size, keeping the glyphs on their own grid.
    -- The shadow is moved after mirroring, so it always falls down and right.
    shape = side v . scaled (0.72 + 0.28 * k) boxCentre
    rows = pixelWrap textWidth said
    left = startX v textLeft (fromIntegral textWidth * px)
    -- Letters shown on each row, counting the one space each row break took.
    shownRows = snd (foldl' (\(remaining, acc) r -> (max 0 (remaining - length r - 1), acc ++ [min (length r) remaining]))
                             (typed, []) rows)
    body = Pictures $
      [ Translate left (textTop - fromIntegral (i * pixelLineH) * px) (typedLine (vcAccent v) n r)
      | (i, (r, n)) <- zip [0 :: Int ..] (zip rows shownRows) ]
      ++ [ corner ]

-- | The name tag over the box's corner by the speaker.
tag :: Voice -> Float -> Picture
tag v a = fadePicture a (Pictures
  [ Color (hex (vcAccent v)) (pixelPoly (side v (subTagShape (vcRole v))))
  , Color (hex ink) (Translate (startX v (-129) (wide 2 (vcRole v))) (-213) (pixelText 2 (vcRole v)))
  , Color (hex rim) (pixelPoly (side v tagShape))
  , Color (hex ink) (Translate (startX v (-297) (wide 4 (vcName v))) (-204) (pixelText 4 (vcName v)))
  ])
  where wide k s = fromIntegral (pixelW s) * k

-- | Draw the speaker's face at a slide fraction (0 home, 1 displaced 150 units off
-- their side of the screen), using Talk's typing age and a global tick clock for
-- mouth, bob and blink.
portrait :: Float -> Talk -> Int -> Picture
portrait slide t = face (voiceOfTalk t) slide (not (lineTyped t)) (tkAge t) (drop (max 0 (typedChars t - 1)) (lnText (currentLine t)))

-- | A face, slid off to its side by @slide@: talking while @typing@, @age@ ticks
-- into the line, @letter@ onwards being the one just typed.
face :: Voice -> Float -> Bool -> Int -> String -> Int -> Picture
face v slide typing age letter clock = Translate (x + offside) (y + bob) (Pictures
  [ fcPicture fc
  , if blinking then fcBlink fc else Blank
  , mouth
  ])
  where
    fc = vcFace v
    (x, y) = portraitAt v
    offside = (if vcRight v then 1 else -1) * snap (slide * 150)
    -- A blink every few seconds, a little irregular.
    blinking = clock `mod` 233 < 7 || clock `mod` 541 < 6
    -- Five-tick mouth phases skip spaces; the vertical bob uses ten ticks.
    mouth
      | typing && (age `div` 5) `mod` 2 == 0 && take 1 letter /= " " = fcOpen fc
      | typing = fcShut fc
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
  TheWay -> "WAY ON"
  TheKeeper -> "KEEPER"
  TheShard -> "SHARD"
  TheCorner -> "CORNER MIRROR"

-- | The board dims around what is being talked about; brackets close on its cell
-- and an arrow over it carries its name. @f@ is the board's stand; @age@ the ticks
-- the line has been up.
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

-- | Everything the talk draws over the level, in canvas units: the card if the
-- story is over, the marker on the board (whose stand is @f@), then the speaker's
-- portrait and the box.
talkLayer :: Story -> Float -> Int -> Picture
talkLayer s f clock = Pictures
  [ maybe Blank (\n -> endCard True (smooth (fromIntegral n / 40)) clock) (stTheEnd s)
  , talking
  ]
  where
    talking = case (stTalk s, stClosing s) of
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
             , raised (tag (voiceOfTalk t) k)
             ]
      -- Closing keeps a copy of the old talk. This record update changes only its
      -- drawing age: a very large age makes all letters visible as it slides away.
      (_, Just (t, n)) ->
        let gone = fromIntegral n / fromIntegral closeTicks
        in Pictures
             [ portrait (smooth gone) t { tkAge = 10 ^ (6 :: Int) } clock
             , raised (box (1 - 0.3 * gone) (1 - gone) t { tkAge = 10 ^ (6 :: Int) } clock)
             , raised (tag (voiceOfTalk t) (1 - gone))
             ]
      -- A waiting/unshown story with no closing panel contributes no overlay.
      _ -> Blank
    -- The box stands a little off the bottom, so its shadow shows.
    raised = Translate 0 15

-- | Over a level's ending: the card that says every order is delivered, as the lamp
-- burns, until the flare takes it; then the talk, on the board as it stands there.
endingLayer :: Story -> Finale -> Int -> Picture
endingLayer s f clock = Pictures [ endCard False shown clock, talkLayer s 1 clock ]
  where
    age = fromIntegral (fnAge f) :: Float
    shown = case fnStage f of
      Lit -> smooth ((age - 100) / 30)
      Flare -> 1 - smooth (age / 10)
      _ -> 0

-- | Her question before an order is cancelled, in the talk's own box: @k@ (0 to 1)
-- as it comes up, the line with @typed@ letters shown, @age@ ticks since she asked,
-- and, in the box's corner where her talk says ENTER, how much of the hold that
-- tears the order is done (0 to 1), filling as the film's skip bar does.
askLayer :: Float -> String -> Int -> Int -> Float -> Int -> Picture
askLayer k said typed age heldK clock
  | k <= 0.01 = Blank
  | otherwise = Pictures
      [ face mei (1 - k) typing age (drop (max 0 (typed - 1)) said) clock
      , raised (boxWith mei (min 1.04 (k * 1.04) - (if k >= 1 then 0.04 else 0)) (min 1 (k * 1.5)) said typed corner)
      , raised (tag mei k)
      ]
  where
    mei = voiceOf Courier
    typing = typed < length said
    raised = Translate 0 15
    hint = "HOLD ENTER"
    hintW = fromIntegral (pixelW hint) * 2
    corner = Pictures
      [ Color (hex (if heldK > 0 then rim else muted)) (Translate (588 - hintW) (-330) (pixelText 2 hint))
      , Color (hexA 0xFFFFFF 0.18) (rect (588 - hintW) (-354) hintW 3)
      , Color (hex sun) (rect (588 - hintW) (-354) (hintW * max 0 (min 1 heldK)) 3)
      ]

-- The end card --------------------------------------------------------------------

-- | Every order delivered, the end, thank you for playing: in the box's own style, a
-- tilted black panel over the board with a white rim and a yellow shadow. @really@
-- after the true end, which says so. Faded in by @k@.
endCard :: Bool -> Float -> Int -> Picture
endCard really k clock
  | k <= 0.01 = Blank
  | otherwise = Translate 0 (snap (150 + 24 * (1 - k))) (fadePicture k (Pictures
      [ Color (hex sun) (pixelPoly (moved (12, -12) panelRim))
      , Color (hex rim) (pixelPoly panelRim)
      , Color (hex ink) (pixelPoly panel)
      , Color (hex rim) (Translate 0 66 (pixelTextC 5 "ALL ORDERS DELIVERED"))
      , Color (hex muted) (Translate 0 18 (pixelTextC 3 subtitle))
      , Color (hex sun) (Translate 0 (-18) (pixelTextC 6 "THE END"))
      , Color (hex muted) (Translate 0 (-72) (pixelTextC 2 "THANK YOU FOR PLAYING"))
      , sparks
      ]))
  where
    panel = [(-348, -111), (345, -99), (354, 96), (-339, 84)]
    panelRim = [(-357, -120), (354, -108), (363, 105), (-348, 93)]
    subtitle
      | really = "EVERY ONE. FOR REAL THIS TIME."
      | otherwise = "EVERY LAMP LIT, EVERY ONE ON TIME"
    -- A few pixel stars twinkling round the title.
    sparks = Pictures
      [ Color (hex sun) (Translate x y (if on then plus else rect 0 0 px px))
      | (i, (x, y)) <- zip [0 :: Int ..] [(-318, 60), (300, 51), (-282, -60), (318, -45), (-210, 75), (234, -78)]
      , let on = ((clock + i * 17) `div` 20) `mod` 3 /= 0 ]
    plus = Pictures [rect 0 (-px) px (3 * px), rect (-px) 0 (3 * px) px]

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
