-- | The title screen: one tower, split down the middle into its two times.
--
-- The line between the times is x = 0. Left of it is the past: a sunset over a
-- teal sea, a kept red-and-white tower with its lamp lit, and the lamp's beam
-- swinging slowly out over that side. Right of it is the present: a dark night
-- over a black, choppy sea, rain, and the same tower rusted, cracked, mossy and
-- dark. The words BORROWED LIGHT stand either side of the tower in the pixel font
-- of the courier's talk, a line under each, and the menu lies on the past's sea.
--
-- This module only draws; it knows nothing of keys or saves. "Render" calls
-- 'titleScreen' while the game is in its Menu phase, with the ambient clock; the
-- app lays 'titleMenu' over it, and 'instructionsPopup' over that while the
-- instructions are open (stepTitle in "Game.Run" opens and shuts them). "Draw"
-- supplies the colours, shapes, noise and baking, "PixelFont" the title's
-- lettering and "Font" the instructions'. OrdersScreen reuses titleBackground
-- without the lettering behind its tickets.
--
-- Units: canvas units, 1280 across and 800 up, with the origin at the centre of
-- the screen and y growing upwards (Gloss's way, unlike the map's rows). The
-- horizon is at y = -150; the tower stands from its foot at y = -118 to the tip of
-- its cap at y = 232. Colours are hex RGB, 0xRRGGBB, made into Gloss colours by
-- 'hex', or by 'hexA' with an opacity from 0 to 1. The clock counts ticks, 60 a
-- second, and arrives as a Float.
--
-- Performance: what never moves (sky, sea, headland, tower and words) is made once,
-- as top-level values, and flattened with 'bake', so a frame only builds the beam,
-- the rain and the menu afresh. Every "random" placement uses 'rnd', which gives
-- the same number for the same inputs, so the baked art is the same every time.
module Title (titleScreen, titleBackground, titleMenu, instructionsPopup) where

import Draw
import Font
import Graphics.Gloss
import PixelFont (pixelText, pixelW)

-- | Everything that holds still, drawn once: the tower on its headland, and the
-- words over it.
-- Separate top-level values share their baked geometry across frames, while
-- OrdersScreen can reuse the tower without the title lettering. Baking merges
-- adjacent shapes of one colour after applying their transforms.
towerArt, labelArt :: Picture
towerArt = bake (Pictures [headland, tower])
labelArt = bake labels

-- | Height of the horizon, in canvas units (below the centre of the screen): where
-- the sky stops and the sea begins, and the base the clouds and waves are placed
-- from.
horizon :: Float
horizon = -150

-- | A vertical gradient as many thin bands, through these colour stops.
--
-- @bands x w stops y0 y1@ fills the rectangle from @x@ to @x + w@ across and from
-- @y0@ to @y1@ up with 36 horizontal strips. The first colour stop is at @y0@, the
-- last at @y1@ and the rest evenly between; each strip takes the colour at its
-- lower edge, mixed in a straight line between the two stops either side of it.
-- Gloss fills a polygon with one colour, so a gradient is built from strips; 36 are
-- enough that the steps do not show. It needs at least two stops.
bands :: Float -> Float -> [Int] -> Float -> Float -> Picture
bands x w stops y0 y1 = Pictures
  -- Each strip is a unit taller than its share, so it overlaps the next one and no
  -- hairline of what is behind shows between them.
  [ Color (colourAt (fromIntegral i / fromIntegral steps))
      (rect x (lerp (fromIntegral i / fromIntegral steps) y0 y1) w ((y1 - y0) / fromIntegral steps + 1))
  | i <- [0 .. steps - 1] ]
  where
    steps = 36 :: Int
    -- The number of gaps between stops, as a Float for the arithmetic below.
    n = fromIntegral (length stops - 1) :: Float
    -- The colour a fraction a (0 to 1) of the way up: pos counts stops along, j is
    -- the stop at or below it (never the last stop, so that j + 1 exists), and
    -- pos - j, from 0 to 1, how far towards the next. 'mix' 0 is the first colour
    -- given and 1 the second. '!!' indexes a list from 0 by walking it, which is
    -- fine for a handful of stops.
    colourAt a =
      let pos = a * n
          j = min (length stops - 2) (floor pos)
      in mix (pos - fromIntegral j) (hex (stops !! j)) (hex (stops !! (j + 1)))

-- | Behind the beam.
--
-- The sky, a half-transparent white hairline 1.6 units wide from the bottom of the
-- screen to the top along x = 0 (the seam between the two times), and the sea,
-- baked once. 'titleScreen' draws it first, so everything else lies over it.
backdrop :: Picture
backdrop = bake $ Pictures [sky, Color (hexA 0xFFFFFF 0.5) (rect (-0.8) (-400) 1.6 800), sea]

-- | The sky, from the horizon to the top of the screen (y = 400).
--
-- Left, the past: a sunset from orange at the horizon to dark violet at the top.
-- Right, the present: a slate night, grey-blue at the horizon and nearly black at
-- the top. Then the clouds. In @rnd i p s@, i is the cloud's number, p picks which
-- of its properties is wanted (across, up, length, and so on) and s, the salt, keeps
-- the two sets of clouds from sharing numbers; each is a fixed number from 0 to 1.
sky :: Picture
sky = Pictures
  [ bands (-640) 640 [0xD98145, 0xC36A44, 0xA45445, 0x834547, 0x643849, 0x4B2E47, 0x382640, 0x2A2036] horizon 400
  , bands 0 640 [0x25333C, 0x202D35, 0x1B262D, 0x162026, 0x121A20, 0x0E151A, 0x0B1115, 0x090E11] horizon 400
    -- Clouds: long calm bars on the left, torn scud on the right.
    -- The bars: 17 strips 3 units tall and 40 to 170 long, warm and faint, their
    -- left ends from x = -640 to -40, between 40 and 510 units above the horizon.
  , Pictures [ Color (hexA 0xF4B97E 0.25) (rect (-640 + 600 * rnd i 1 1) (horizon + 40 + 470 * rnd i 2 1) (40 + 130 * rnd i 3 1) 3)
             | i <- [0 .. 16 :: Int] ]
    -- The scud: 19 dark, half-clear blobs, each a 9-sided ellipse 60 to 180 units
    -- long and 10 to 26 tall, centred from x = 20 to 640 at the same heights as the
    -- bars, and turned between 8 degrees anticlockwise and 2 clockwise (Gloss turns
    -- positive angles clockwise), so they look torn and blown about.
  , Pictures [ Color (hexA 0x34444E 0.55)
                 (Translate (20 + 620 * rnd i 1 2) (horizon + 40 + 470 * rnd i 2 2)
                   (rot (-8 + 10 * rnd i 3 2) (ellipse 9 (30 + 60 * rnd i 4 2) (5 + 8 * rnd i 5 2))))
             | i <- [0 .. 18 :: Int] ]
  ]

-- | The sea, from the bottom of the screen (y = -400) up to the horizon.
--
-- Left, the past: teal, lighter towards the horizon, with 43 warm glints, each a
-- shallow caret 16 to 60 units across. Right, the present: nearly black, with 43
-- grey zigzags of choppy water. 'thin' draws a line one screen pixel wide. The
-- glints and the zigzags take their places from 'rnd' like the clouds, with salts
-- 3 and 4 so the two halves do not repeat each other. The let inside each list
-- comprehension names values for each i, used to build that element.
sea :: Picture
sea = Pictures
  [ bands (-640) 640 [0x0F3A44, 0x124450, 0x174F58, 0x1D5A60] (-400) horizon
  , bands 0 640 [0x060C0F, 0x081014, 0x0A1418, 0x0D191E] (-400) horizon
    -- Glints: centred from x = -630 to -10 and from y = -395 to 5 below the
    -- horizon, a half-width w of 8 to 30, the middle raised 1.5 units.
  , Pictures [ thin (hexA 0xFFD49A 0.5)
                 [(x - w, y), (x, y + 1.5), (x + w, y)]
             | i <- [0 .. 42 :: Int]
             , let x = -630 + 620 * rnd i 4 3
                   y = -395 + (horizon + 390) * rnd i 5 3
                   w = 8 + 22 * rnd i 6 3 ]
    -- Chop: the same spread on the right, x from 8 to 633, each line up 4 and
    -- down 3 between its ends, a small jagged wave.
  , Pictures [ thin (hexA 0x7D93A0 0.4)
                 [(x - w, y), (x - w / 3, y + 4), (x + w / 3, y - 3), (x + w, y)]
             | i <- [0 .. 42 :: Int]
             , let x = 8 + 625 * rnd i 4 4
                   y = -395 + (horizon + 390) * rnd i 5 4
                   w = 8 + 22 * rnd i 6 4 ]
  ]

-- | The headland the tower stands on, from y = -210 up to the tower's foot at
-- y = -118, split at x = 0 like everything else: in the past, brown earth with a
-- lighter strip along its top; in the present, dark slate with its own lighter top
-- edge and a lighter stone set in its slope. Each piece is a 'Polygon', its corners
-- listed in order round it.
headland :: Picture
headland = Pictures
  [ Color (hex 0x3A2A22) (Polygon [(-230, -210), (0, -210), (0, -128), (-130, -128)])
  , Color (hex 0x4C382C) (Polygon [(-130, -128), (0, -128), (0, -118), (-110, -118)])
  , Color (hex 0x0E1518) (Polygon [(0, -210), (250, -210), (120, -120), (0, -128)])
  , Color (hex 0x1B2429) (Polygon [(0, -128), (120, -120), (96, -112), (0, -118)])
  , Color (hex 0x151D22) (Polygon [(140, -210), (210, -210), (182, -168), (160, -178)])
  ]

-- | Half-width of the tower at a height.
--
-- In canvas units: 46 at the foot (y = -118), narrowing in a straight line to 28 at
-- the gallery (y = 150); 268 is the height between the two.
towerW :: Float -> Float
towerW y = 46 - 18 * (y + 118) / 268

-- | The tower, its two halves drawn separately so that each time has its own
-- colours and wear: six bands from the foot to the gallery, the door, the gallery,
-- the lantern and the cap. Every piece of the past lies left of x = 0 and every
-- piece of the present right of it, so the seam runs straight up the middle. The
-- picture is several lists joined with '++'; later pieces are drawn over earlier
-- ones.
tower :: Picture
tower = Pictures $
  -- Past half: clean bands.
  -- Red and cream in turn from the foot, then a warm light down its left edge,
  -- 12 units wide at the foot and 8 at the top, a third opaque.
  [ pieceL y0 y1 (if even i then 0xD8493B else 0xF4EADA)
  | (i, y0, y1) <- towerBands ] ++
  [ Color (hexA 0xFFE2A8 0.32) (Polygon [(-towerW (-118), -118), (-towerW (-118) + 12, -118), (-towerW 150 + 8, 150), (-towerW 150, 150)]) ] ++
  -- Present half: same bands, faded and streaked.
  -- Rust-brown and grey in turn, a half-clear dark streak down its right edge
  -- (14 units wide at the foot, 9 at the top), a crack climbing the middle with a
  -- branch off it, and two clumps of moss at the foot.
  [ pieceR y0 y1 (if even i then 0x6E3A30 else 0x7E8A8C)
  | (i, y0, y1) <- towerBands ] ++
  [ Color (hexA 0x10171B 0.5) (Polygon [(towerW (-118) - 14, -118), (towerW (-118), -118), (towerW 150, 150), (towerW 150 - 9, 150)])
  , thin (hex 0x10171B) [(6, -60), (14, -30), (8, 2), (20, 40), (12, 86), (19, 130)]
  , thin (hex 0x10171B) [(14, -30), (28, -12)]
  , Color (hexA 0x5F7F4E 0.85) (Translate 26 (-112) (ellipse 10 20 8))
  , Color (hexA 0x5F7F4E 0.7) (Translate 12 (-104) (ellipse 8 11 5))
  ] ++
  -- Door, split like everything else.
  -- 26 units wide and 46 tall at its peak, with a pointed top: timber on the
  -- past's side, a dark empty doorway on the present's.
  [ Color (hex 0x4A2E1E) (Polygon [(-13, -118), (0, -118), (0, -72), (-13, -80)])
  , Color (hex 0x0A0E11) (Polygon [(0, -118), (13, -118), (13, -80), (0, -72)])
  ] ++
  -- Gallery, lantern and cap.
  -- The gallery is a ledge 8 units tall on top of the bands: dark timber with a
  -- brass rail in the past, dark slate with a grey rail in the present, where both
  -- end a little shorter.
  [ Color (hex 0x2A1A12) (rect (-44) 150 44 8)
  , Color (hex 0xC89C50) (rect (-44) 156 44 3)
  , Color (hex 0x10171B) (rect 0 150 40 8)
  , Color (hex 0x4F5B60) (rect 0 156 36 2)
    -- Glazing: the left half whole, the right half dark and broken.
    -- The lantern runs from y = 158 to 202. The past's glass is lit, with a white
    -- core and two timber bars; the present's is dark, one of its bars broken off,
    -- with a crack across it. Then the cap, from y = 202 to its tip at 232: a red
    -- half and a dark brown half, with a black notch broken out of the brown
    -- half's lower corner.
  , Color (hex 0xFFEFC4) (rect (-24) 158 24 44)
  , Color (hex 0xFFFFFF) (rect (-14) 164 14 32)
  , Color (hex 0x2A1A12) (Pictures [rect (-25) 158 2.5 44, rect (-13) 158 1.6 44])
  , Color (hex 0x0D1317) (rect 0 158 24 44)
  , Color (hex 0x5E6C72) (Pictures [rect 22 158 2.5 44, rect 11 158 1.6 26])
  , thin (hex 0x8A9AA0) [(3, 200), (10, 182), (6, 170), (16, 160)]
  , Color (hex 0xA8362E) (Polygon [(-30, 202), (0, 202), (0, 232)])
  , Color (hex 0x4B3A38) (Polygon [(0, 202), (28, 202), (0, 232)])
  , Color (hexA 0x0A1014 1) (tri (16, 202) (28, 202) (24, 214))
  ]
  where
    -- Six bands of equal height between the foot (-118) and the gallery (150),
    -- each with its number i (0 at the foot), so the colours can alternate on
    -- even and odd. The @i :: Int@ fixes the type of the numbers counted.
    towerBands = [ (i :: Int, -118 + 268 * fromIntegral i / 6, -118 + 268 * fromIntegral (i + 1) / 6) | i <- [0 .. 5] ]
    -- One band's left (past) or right (present) half: a four-cornered piece
    -- between x = 0 and the tower's edge, narrower at the top.
    pieceL y0 y1 c = Color (hex c) (Polygon [(-towerW y0, y0), (0, y0), (0, y1), (-towerW y1, y1)])
    pieceR y0 y1 c = Color (hex c) (Polygon [(0, y0), (towerW y0, y0), (towerW y1, y1), (0, y1)])

-- | The still words: the title either side of the tower, a line under each half of
-- it, and the jam's name in the top corner. They are in the pixel font of the
-- courier's talk and her order tickets, so the title belongs with them: BORROWED in
-- the past's warm cream on her yellow, LIGHT in the present's cold grey on slate.
-- Each word hangs from its top edge ('pixelText'), 10 canvas units a pixel, so 70
-- tall, from y = 104; BORROWED ends 72 left of the seam and LIGHT starts 72 right
-- of it, with the tower between.
labels :: Picture
labels = Pictures
  [ logoWord "BORROWED" (-72) True 0xFFF3D6 0xF6B31C
  , logoWord "LIGHT" 72 False 0xC9D6DB 0x33434C
  -- The lines under the words, 2 units a pixel, 92 below their top.
  , Color (hex 0xFFE2A8) (Translate (-72 - pw 2 "PICK UP IN THE PAST") 12 (pixelText 2 "PICK UP IN THE PAST"))
  , Color (hex 0xA9B9BF) (Translate 72 12 (pixelText 2 "DELIVER TO THE PRESENT"))
  , Color (hexA 0x8C8796 0.75) (Translate (608 - pw 2 "CSS GAMEJAM 2026") 372 (pixelText 2 "CSS GAMEJAM 2026"))
  ]
  where
    -- A logo word: a dark shadow 8 units down and right, a coloured one at 4, then
    -- the letters; right-aligned on x if @right@, else starting at it.
    logoWord s x right fill shade =
      let word = pixelText 10 s
          left = if right then x - pw 10 s else x
      in Translate left 104 (Pictures
           [ Color (hexA 0x05080A 0.55) (Translate 8 (-8) word)
           , Color (hex shade) (Translate 4 (-4) word)
           , Color (hex fill) word ])

-- | The width of a line of the pixel font, in canvas units, at @px@ a pixel.
pw :: Float -> String -> Float
pw px s = fromIntegral (pixelW s) * px

-- | The still art, the turning beam and the rain, and the words. The menu is drawn
-- over it by the app ('titleMenu'), and the instructions over that.
--
-- @clock@ is the ambient tick count (60 a second) as a Float, from "Render".
titleScreen :: Float -> Picture
titleScreen clock = Pictures [titleBackground clock, labelArt]

-- | The title's menu, low on the past's sea: one line a choice, top to bottom as
-- 'Game.Run.titleStart' and the rest number them.
--
-- @choice@ is the chosen line ('gTitleChoice'), @saved@ whether there is an order
-- to go back to (then the first line continues instead of taking a new order), and
-- @muted@ whether the sound is off. The chosen line is a plate like the courier's
-- name tag: ink, her yellow shadow, the letters in rim white, and the talk's arrow
-- turned to point at it. The others are plain rim white, a little faded. Lines are
-- 3 units a pixel (21 tall), 40 apart, from y = -214, starting at x = -556.
titleMenu :: Int -> Bool -> Bool -> Picture
titleMenu choice saved muted = Pictures (zipWith menuLine [0 ..] items)
  where
    items =
      [ if saved then ("CONTINUE", "") else ("NEW ORDER", "opening film first")
      , ("HOW TO PLAY", "")
      , (if muted then "SOUND: OFF" else "SOUND: ON", "")
      , ("QUIT", "") ]
    x = -556
    h = 21
    menuLine :: Int -> (String, String) -> Picture
    menuLine i (label, note)
      | i == choice = Pictures
          [ Color (hex 0xF6B31C) (rect (x - 6) (y - h - 15) (w + 24) (h + 18))
          , Color (hex 0x0D0B12) (rect (x - 12) (y - h - 9) (w + 24) (h + 18))
          , Color (hex 0xF4F1EA) (Translate x y (pixelText 3 label))
          , arrowRight (x - 50) (y - h / 2)
          , Color (hex 0xF6B31C) (Translate (x + w + 30) (y - 4) (pixelText 2 note)) ]
      | otherwise = Pictures
          [ Color (hexA 0xF4F1EA 0.7) (Translate x y (pixelText 3 label))
          , Color (hexA 0x8C8796 0.9) (Translate (x + w + 18) (y - 4) (pixelText 2 note)) ]
      where
        y = -214 - 40 * fromIntegral i
        w = pw 3 label

-- | The courier's talk arrow, turned to point right: ten columns of 2-unit pixels
-- from its tail at x to its tip, centred on y, in her yellow with a dark rim.
-- Each pair is a column's lowest pixel row counted up from the middle, negated,
-- and how many pixels tall it is.
arrowRight :: Float -> Float -> Picture
arrowRight x y = Translate x y (Pictures
  [ Color (hex 0x05080A) (Pictures
      [ rect (fromIntegral i * px - px) (negate (fromIntegral (y0 + n + 1)) * px + off) (px * 3) (fromIntegral (n + 2) * px)
      | (i, (y0, n)) <- cols ])
  , Color (hex 0xF6B31C) (Pictures
      [ rect (fromIntegral i * px) (negate (fromIntegral (y0 + n)) * px + off) px (fromIntegral n * px)
      | (i, (y0, n)) <- cols ])
  ])
  where
    px = 2
    off = 3.5 * px
    cols = zip [0 :: Int ..] [(0, 7), (0, 7), (0, 7), (-3, 13), (-2, 11), (-1, 9), (0, 7), (1, 5), (2, 3), (3, 1 :: Int)]

-- | The title without its words: the split sky and sea, the turning beam, the
-- tower and the rain. The orders are laid out over it.
titleBackground :: Float -> Picture
titleBackground clock = Pictures
  [ backdrop
  , beam
  , towerArt
  , rain
  ]
  where
    -- How far the beam is turned from pointing straight left, in degrees: it swings
    -- between 1 and 15 degrees below level and back, once every 2 pi / 0.012 =
    -- 524 ticks, about 8.7 seconds.
    swing = -8 + 7 * sin (clock * 0.012)
    -- The beam: two see-through wedges laid along the positive x axis from the
    -- lamp, then turned by 180 + swing degrees, clockwise as Gloss turns, so that
    -- they point left, over the past. The outer wedge is wide and faint and the
    -- inner one narrow and a little brighter, a cheap glow. They start at the
    -- lantern's middle (0, 180) and are 760 units long, always past the left edge
    -- of the screen, 640 from the centre.
    beam = Translate 0 180 (Rotate (180 + swing) (Pictures
      [ Color (hexA 0xFFE2A8 0.1) (Polygon [(0, -14), (760, -150), (760, 150), (0, 14)])
      , Color (hexA 0xFFF1C2 0.14) (Polygon [(0, -10), (760, -70), (760, 70), (0, 10)])
      ]))
    -- Rain, on the present's side only: 35 streaks, each 15 by 1.2 units, tilted
    -- 22 degrees anticlockwise and faint (16% to 38% opaque). Streak i falls at its
    -- own speed, 2.4 to 4.6 units a tick, drifting left at 0.42 of that. Its place
    -- is its fixed random start moved by clock times speed, then wrapped back into
    -- the strip from x = 8 to 640 and the full height from y = -400 to 400, so it
    -- falls for ever with no state kept between frames.
    rain = Pictures
      [ Translate x y (Color (hexA 0xB7CEDA (0.16 + 0.22 * rnd i 3 8)) (Rotate (-22) (rectC 15 1.2)))
      | i <- [0 .. 34 :: Int]
      , let speed = 2.4 + 2.2 * rnd i 2 8
            x = 8 + wrapTo 632 (632 * rnd i 1 8 - clock * speed * 0.42)
            y = -400 + wrapTo 800 (800 * rnd i 4 8 - clock * speed)
      ]
    -- v brought into [0, size) by taking off whole multiples of size, for Floats
    -- ('mod' is for whole numbers). It works for negative v too, since 'floor'
    -- rounds down, not towards zero.
    wrapTo size v = v - size * fromIntegral (floor (v / size) :: Int)

-- | The instructions, over the title while they are open: a grey-blue veil across
-- the whole canvas, a dark panel 700 by 440 in the middle with a cream outline
-- ('rectangleWire' draws a centred rectangle's edges only), a heading and two
-- lines, and two columns of keycaps, each followed by what it does: the keys on
-- the left, the things met in the levels on the right.
--
-- The app draws it over the title and its menu while the game's gShowInstructions
-- flag is set. HOW TO PLAY or the I key opens it, and Enter, I or Escape shuts it
-- (see stepTitle in "Game.Run"). A top-level value, so the picture is made once.
instructionsPopup :: Picture
instructionsPopup = Pictures
  [Color (hexA 0x7D93A0 0.4) (rectC canvasW canvasH)
  , Color (hex 0x17252D) (rectC 700 440)
  , Color (hex 0xFFF3D6) (rectangleWire 700 440)

  , label 0 155 30 "HOW TO PLAY"
  , label 0 100 12 "Bring the power cell from the past to light up the lighthouse"
  , label 0 70 12 "Travel between past and present to solve the puzzles!"
  , Color (hexA 0xFFF3D6 0.6) (Translate 0 (-200) (textC 12 "Enter or Esc to close"))

  , Translate 0 (-100) (Pictures
    -- The keys. The group is lowered 100, so its rows, 40 apart, stand from
    -- y = 30 down to -170 on the screen, all starting at x = -280.
    [ item (-280) (130) "WASD / ARROWS" "Move"
    , item (-280) (90) "SPACE" "Travel through time"
    , item (-280) (50) "Z" "Undo"
    , item (-280) (10) "TAB" "Preview other timeline"
    , item (-280) (-30) "P" "Pause"
    , item (-280) (-70) "R" "Restart"
    ])

  , Translate 250 (-100) (Pictures
  -- Only what the campaign has: plates and doors were cut, and the mirror players
  -- missed is the one that slides.
  -- This group is moved 250 right and its rows start at x = -250, so the column
  -- begins at x = 0, level with the top three rows of keys.
  [ item (-250) (130) "BOX" "Push it in the past"
  , item (-250) (90) "MIRROR" "Walk into its light to jump"
  , item (-250) (50) "MIRROR" "Push sled from back or side"
  ])
  ]
  where
    -- A line of cream text, size canvas units tall, centred on x with its baseline
    -- on y.
    label x y size message = Translate x y (Color (hex 0xFFF3D6) (textC size message))

    -- One row: a keycap, then what the key does. (x, y) is the left end of the
    -- row's baseline. The cap is as wide as the key's lettering (size 12) plus 8
    -- units each side: a light rim 24 tall from 7 below the baseline, and a dark
    -- face inset 1.5 units inside it. The lettering is centred on the cap, and
    -- the description starts 9 units after it.
    item x y key what =
      let w = textW 12 key + 16
      in Translate x y (Pictures
        [ Color (hexA 0xFFFFFF 0.6) (rect 0 (-7) w 24)
        , Color (hex 0x0B1014) (rect 1.5 (-5.5) (w - 3) 21)
        , Color (hex 0xF4EEE2) (Translate (w / 2) 0 (textC 12 key))
        , Color (hex 0xF4EEE2) (Translate (w + 9) 0 (textL 12 what))
        ])