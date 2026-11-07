-- | The title screen: one tower, split down the middle into its two times.
module Title (titleScreen) where

import Draw
import Font
import Graphics.Gloss

-- | Everything that holds still, drawn once.
titleArt :: Picture
titleArt = bake $ Pictures
  [ headland
  , tower
  , labels
  ]

horizon :: Float
horizon = -150

-- | A vertical gradient as many thin bands, through these colour stops.
bands :: Float -> Float -> [Int] -> Float -> Float -> Picture
bands x w stops y0 y1 = Pictures
  [ Color (colourAt (fromIntegral i / fromIntegral steps))
      (rect x (lerp (fromIntegral i / fromIntegral steps) y0 y1) w ((y1 - y0) / fromIntegral steps + 1))
  | i <- [0 .. steps - 1] ]
  where
    steps = 36 :: Int
    n = fromIntegral (length stops - 1) :: Float
    colourAt a =
      let pos = a * n
          j = min (length stops - 2) (floor pos)
      in mix (pos - fromIntegral j) (hex (stops !! j)) (hex (stops !! (j + 1)))

-- | Behind the beam.
backdrop :: Picture
backdrop = bake $ Pictures [sky, Color (hexA 0xFFFFFF 0.5) (rect (-0.8) (-400) 1.6 800), sea]

sky :: Picture
sky = Pictures
  [ bands (-640) 640 [0xD98145, 0xC36A44, 0xA45445, 0x834547, 0x643849, 0x4B2E47, 0x382640, 0x2A2036] horizon 400
  , bands 0 640 [0x25333C, 0x202D35, 0x1B262D, 0x162026, 0x121A20, 0x0E151A, 0x0B1115, 0x090E11] horizon 400
    -- Clouds: long calm bars on the left, torn scud on the right.
  , Pictures [ Color (hexA 0xF4B97E 0.25) (rect (-640 + 600 * rnd i 1 1) (horizon + 40 + 470 * rnd i 2 1) (40 + 130 * rnd i 3 1) 3)
             | i <- [0 .. 16 :: Int] ]
  , Pictures [ Color (hexA 0x34444E 0.55)
                 (Translate (20 + 620 * rnd i 1 2) (horizon + 40 + 470 * rnd i 2 2)
                   (rot (-8 + 10 * rnd i 3 2) (ellipse 9 (30 + 60 * rnd i 4 2) (5 + 8 * rnd i 5 2))))
             | i <- [0 .. 18 :: Int] ]
  ]

sea :: Picture
sea = Pictures
  [ bands (-640) 640 [0x0F3A44, 0x124450, 0x174F58, 0x1D5A60] (-400) horizon
  , bands 0 640 [0x060C0F, 0x081014, 0x0A1418, 0x0D191E] (-400) horizon
  , Pictures [ thin (hexA 0xFFD49A 0.5)
                 [(x - w, y), (x, y + 1.5), (x + w, y)]
             | i <- [0 .. 42 :: Int]
             , let x = -630 + 620 * rnd i 4 3
                   y = -395 + (horizon + 390) * rnd i 5 3
                   w = 8 + 22 * rnd i 6 3 ]
  , Pictures [ thin (hexA 0x7D93A0 0.4)
                 [(x - w, y), (x - w / 3, y + 4), (x + w / 3, y - 3), (x + w, y)]
             | i <- [0 .. 42 :: Int]
             , let x = 8 + 625 * rnd i 4 4
                   y = -395 + (horizon + 390) * rnd i 5 4
                   w = 8 + 22 * rnd i 6 4 ]
  ]

headland :: Picture
headland = Pictures
  [ Color (hex 0x3A2A22) (Polygon [(-230, -210), (0, -210), (0, -128), (-130, -128)])
  , Color (hex 0x4C382C) (Polygon [(-130, -128), (0, -128), (0, -118), (-110, -118)])
  , Color (hex 0x0E1518) (Polygon [(0, -210), (250, -210), (120, -120), (0, -128)])
  , Color (hex 0x1B2429) (Polygon [(0, -128), (120, -120), (96, -112), (0, -118)])
  , Color (hex 0x151D22) (Polygon [(140, -210), (210, -210), (182, -168), (160, -178)])
  ]

-- | Half-width of the tower at a height.
towerW :: Float -> Float
towerW y = 46 - 18 * (y + 118) / 268

tower :: Picture
tower = Pictures $
  -- Past half: clean bands.
  [ pieceL y0 y1 (if even i then 0xD8493B else 0xF4EADA)
  | (i, y0, y1) <- towerBands ] ++
  [ Color (hexA 0xFFE2A8 0.32) (Polygon [(-towerW (-118), -118), (-towerW (-118) + 12, -118), (-towerW 150 + 8, 150), (-towerW 150, 150)]) ] ++
  -- Present half: same bands, faded and streaked.
  [ pieceR y0 y1 (if even i then 0x6E3A30 else 0x7E8A8C)
  | (i, y0, y1) <- towerBands ] ++
  [ Color (hexA 0x10171B 0.5) (Polygon [(towerW (-118) - 14, -118), (towerW (-118), -118), (towerW 150, 150), (towerW 150 - 9, 150)])
  , thin (hex 0x10171B) [(6, -60), (14, -30), (8, 2), (20, 40), (12, 86), (19, 130)]
  , thin (hex 0x10171B) [(14, -30), (28, -12)]
  , Color (hexA 0x5F7F4E 0.85) (Translate 26 (-112) (ellipse 10 20 8))
  , Color (hexA 0x5F7F4E 0.7) (Translate 12 (-104) (ellipse 8 11 5))
  ] ++
  -- Door, split like everything else.
  [ Color (hex 0x4A2E1E) (Polygon [(-13, -118), (0, -118), (0, -72), (-13, -80)])
  , Color (hex 0x0A0E11) (Polygon [(0, -118), (13, -118), (13, -80), (0, -72)])
  ] ++
  -- Gallery, lantern and cap.
  [ Color (hex 0x2A1A12) (rect (-44) 150 44 8)
  , Color (hex 0xC89C50) (rect (-44) 156 44 3)
  , Color (hex 0x10171B) (rect 0 150 40 8)
  , Color (hex 0x4F5B60) (rect 0 156 36 2)
    -- Glazing: the left half whole, the right half dark and broken.
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
    towerBands = [ (i :: Int, -118 + 268 * fromIntegral i / 6, -118 + 268 * fromIntegral (i + 1) / 6) | i <- [0 .. 5] ]
    pieceL y0 y1 c = Color (hex c) (Polygon [(-towerW y0, y0), (0, y0), (0, y1), (-towerW y1, y1)])
    pieceR y0 y1 c = Color (hex c) (Polygon [(0, y0), (towerW y0, y0), (towerW y1, y1), (0, y1)])

labels :: Picture
labels = Pictures
  [ Color (hex 0xF7DDB2) (Translate (-600) 356 (textL 15 "PAST"))
  , Color (hex 0xA9B9BF) (Translate 600 356 (textR 15 "PRESENT"))
    -- Title.
  , Translate 4 (-4) (Pictures
      [Color (hex 0xFFF3D6) (Translate (-74) wordY (ws 68 8.6 "BORROWED"))
      , Color (hex 0xE8F0F2) (Translate 204 wordY (wl 68 8.6 "LIGHT"))])
   
    -- Premise and controls, on the water.
  , Color (hex 0xF2E6D0) (Translate 0 (-236) (textC 14 "Borrow a power cell from the past to light the present"))
  , Color (hex 0xFFE2A8) (Translate 0 (-258) (textC 14 "Every action spends time."))
  , controlRow
  ]
  where
    wordY = 44
    ws size w s = Translate (negate (textW size s)) 0 (weighted size w s)
    wl = weighted

controlRow :: Picture
controlRow = Translate 0 (-300) (Pictures
  [ item (-440) "WASD / ARROWS" "Move"
  , item (-180) "SPACE" "Shift era"
  , item 18 "Z" "Undo"
  , item 118 "TAB" "Look"
  , item 250 "P" "Pause"
  , item 360 "R" "Restart"
  ])
  where
    item x key what =
      let w = textW 12 key + 16
      in Translate x 0 (Pictures
        [ Color (hexA 0xFFFFFF 0.6) (rect 0 (-7) w 24)
        , Color (hex 0x0B1014) (rect 1.5 (-5.5) (w - 3) 21)
        , Color (hex 0xF4EEE2) (Translate (w / 2) 0 (textC 12 key))
        , Color (hex 0xF4EEE2) (Translate (w + 9) 0 (textL 12 what))
        ])

-- | The still art, the turning beam and the rain.
titleScreen :: Float -> Picture
titleScreen clock = Pictures
  [ backdrop
  , beam
  , titleArt
  , rain
  , prompt
  ]
  where
    swing = -8 + 7 * sin (clock * 0.012)
    beam = Translate 0 180 (Rotate (180 + swing) (Pictures
      [ Color (hexA 0xFFE2A8 0.1) (Polygon [(0, -14), (760, -150), (760, 150), (0, 14)])
      , Color (hexA 0xFFF1C2 0.14) (Polygon [(0, -10), (760, -70), (760, 70), (0, 10)])
      ]))
    rain = Pictures
      [ Translate x y (Color (hexA 0xB7CEDA (0.16 + 0.22 * rnd i 3 8)) (Rotate (-22) (rectC 15 1.2)))
      | i <- [0 .. 34 :: Int]
      , let speed = 2.4 + 2.2 * rnd i 2 8
            x = 8 + wrapTo 632 (632 * rnd i 1 8 - clock * speed * 0.42)
            y = -400 + wrapTo 800 (800 * rnd i 4 8 - clock * speed)
      ]
    wrapTo size v = v - size * fromIntegral (floor (v / size) :: Int)
    glowK = 0.72 + 0.28 * sin (clock * 0.07)
    prompt = Translate 0 (-350) (Pictures
      [ Color (hexA 0x000000 0.45) (rectC 300 42 `shift` 6)
      , Color (fade glowK (hex 0xFFE2A8)) (Translate 0 0 (textC 19 "Press Enter to begin"))
      ])
    shift pic dy = Translate 0 dy pic
