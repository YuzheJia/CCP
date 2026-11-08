-- | The title screen: one tower, split down the middle into its two times.
--
-- The line between the times is x = 0. Left of it is the past: a sunset over a
-- teal sea, a kept red-and-white tower with its lamp lit, and the lamp's beam
-- swinging slowly out over that side. Right of it is the present: a dark night
-- over a black, choppy sea, rain, and the same tower rusted, cracked, mossy and
-- dark. The words BORROWED LIGHT stand either side of the tower, PAST and PRESENT
-- in the top corners, and the premise and the way to the instructions on the water.
--
-- This module only draws; it knows nothing of keys or saves. "Render" calls
-- 'titleScreen' while the game is in its Menu phase, with the ambient clock, and
-- lays 'instructionsPopup' over it while the instructions are open (the I key opens
-- and shuts them, in stepTitle in "Game.Run"). "Draw" supplies the colours,
-- shapes, noise and baking, and "Font" the lettering.
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
-- the rain and the prompt afresh. Every "random" placement uses 'rnd', which gives
-- the same number for the same inputs, so the baked art is the same every time.
module Title (titleScreen, instructionsPopup) where

import Draw
import Font
import Graphics.Gloss

-- | Everything that holds still, drawn once.
--
-- The headland, the tower and the words, flattened by 'bake' into a few long lists
-- of polygons grouped by colour. A top-level value with no arguments is worked out
-- the first time it is drawn and then shared by every later frame. @bake $ x@ is
-- bake applied to x: '$' is function application at the lowest precedence, which
-- saves a pair of brackets round the long list.
titleArt :: Picture
titleArt = bake $ Pictures
  [ headland
  , tower
  , labels
  ]

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

-- | The still words: the era names in the top corners, the title either side of the
-- tower, and three lines on the water. Each is lettered with "Font", which gives
-- uncoloured strokes, inside a 'Color' and a 'Translate' to its place; the
-- position given is the left end, centre or right end of the baseline, depending
-- on whether textL, textC or textR draws it.
labels :: Picture
labels = Pictures
  [ Color (hex 0xF7DDB2) (Translate (-600) 356 (textL 15 "PAST"))
  , Color (hex 0xA9B9BF) (Translate 600 356 (textR 15 "PRESENT"))
    -- Title.
    -- BORROWED ends at x = -74 and LIGHT starts at x = 204, both on the baseline
    -- y = 44, 68 units tall with a pen 8.6 wide; then the whole title is nudged 4
    -- right and 4 down.
  , Translate 4 (-4) (Pictures
      [Color (hex 0xFFF3D6) (Translate (-74) wordY (ws 68 8.6 "BORROWED"))
      , Color (hex 0xE8F0F2) (Translate 204 wordY (wl 68 8.6 "LIGHT"))])

    -- Premise and controls, on the water.
    -- Three centred lines 14 units tall, 22 apart, below the headland; the middle
    -- one, about time, in a warmer yellow.
  , Color (hex 0xF2E6D0) (Translate 0 (-236) (textC 14 "Borrow a power cell from the past to light the present"))
  , Color (hex 0xFFE2A8) (Translate 0 (-258) (textC 14 "Every action spends time."))
  , Color (hex 0xF2E6D0) (Translate 0 (-280) (textC 14 "Press 'I' to see instructions"))
  ]
  where
    -- The baseline of the two title words.
    wordY = 44
    -- ws letters a word with a chosen pen weight and ends it at the origin, a
    -- right-aligned 'weighted' (textR only has the default weight); wl is plain
    -- 'weighted', which starts at the origin.
    ws size w s = Translate (negate (textW size s)) 0 (weighted size w s)
    wl = weighted

-- | The still art, the turning beam and the rain.
--
-- @clock@ is the ambient tick count (60 a second) as a Float, from "Render". The
-- layers, back to front: the baked sky and sea, the beam, the baked headland, tower
-- and words (so the tower stands in front of the root of its own beam), the rain,
-- and the prompt. Only the beam, the rain and the prompt are built each frame; the
-- rest are top-level values made once.
titleScreen :: Float -> Picture
titleScreen clock = Pictures
  [ backdrop
  , beam
  , titleArt
  , rain
  , prompt
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
    -- The prompt's brightness breathes between 0.44 and 1, once every 2 pi / 0.07,
    -- about 90 ticks or one and a half seconds.
    glowK = 0.72 + 0.28 * sin (clock * 0.07)
    -- "Press Enter to begin", 19 units tall, centred near the bottom of the screen
    -- (baseline at y = -350) on a dark, half-clear plate 300 by 42, raised 6 so it
    -- sits round the letters, which stand on the baseline.
    prompt = Translate 0 (-350) (Pictures
      [ Color (hexA 0x000000 0.45) (rectC 300 42 `shift` 6)
      , Color (fade glowK (hex 0xFFE2A8)) (Translate 0 0 (textC 19 "Press Enter to begin"))
      ])
    -- shift pic dy raises a picture by dy. Above it is written between backticks,
    -- rectC 300 42 `shift` 6, which uses an ordinary function as an infix operator.
    shift pic dy = Translate 0 dy pic

-- | The instructions, over the title while they are open: a grey-blue veil across
-- the whole canvas, a dark panel 700 by 440 in the middle with a cream outline
-- ('rectangleWire' draws a centred rectangle's edges only), a heading and two
-- lines, and two columns of keycaps, each followed by what it does: the keys on
-- the left, the things met in the levels on the right.
--
-- "Render" draws it over 'titleScreen' while the game's gShowInstructions flag is
-- set. The I key opens and shuts it, and Enter does nothing while it is open (see
-- stepTitle in "Game.Run"). A top-level value, so the picture is made once.
instructionsPopup :: Picture
instructionsPopup = Pictures
  [Color (hexA 0x7D93A0 0.4) (rectC canvasW canvasH)
  , Color (hex 0x17252D) (rectC 700 440)
  , Color (hex 0xFFF3D6) (rectangleWire 700 440)

  , label 0 155 30 "HOW TO PLAY"
  , label 0 100 12 "Bring the power cell from the past to light up the lighthouse"
  , label 0 70 12 "Travel between past and present to solve the puzzles!"

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