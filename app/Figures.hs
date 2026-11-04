-- | Things that move or shine on top of the station.
module Figures
  ( keeper
  , powerCell
  , cellIcon
  , carriedLight
  , pickupBurst
  , lanternGlow
  , victoryRays
  , relitSconces
  , warmthOver
  , ambient
  , waveRing
  ) where

import Draw
import Game.Types
import Graphics.Gloss
import Station (pool, sconceCells)

-- The keeper -------------------------------------------------------------------

-- | The lighthouse keeper in a yellow oilskin. @phase@ runs 0..1 across a step.
keeper :: Dir -> Float -> Picture
keeper facing phase = Scale 1.08 1.08 (Pictures
  [ Color (hexA 0x000000 0.45) (Translate 0 (-20) (ellipse 16 17 7))
  , Translate 0 bob figure
  ])
  where
    bob = 2.2 * abs (sin (phase * pi))
    stride = sin (phase * 2 * pi) * 3
    outline = hex 0x17100B
    coat = hex 0xF7C81E
    coatShade = hex 0xC4920F
    hood = hex 0xFFD93E
    boots = hex 0x2A1B12
    skin = hex 0xEBC8A0
    figure = case facing of
      DirDown  -> front
      DirUp    -> back
      DirLeft  -> side
      DirRight -> Scale (-1) 1 side
    legs a b = Pictures
      [ Color outline (Pictures [rect (-9) (-21 + a) 8 10, rect 1 (-21 + b) 8 10])
      , Color boots (Pictures [rect (-8) (-20 + a) 6 8, rect 2 (-20 + b) 6 8])
      ]
    trunk = Pictures
      [ Color outline (Polygon [(-16, -14), (16, -14), (12, 12), (-12, 12)])
      , Color coat (Polygon [(-14, -12), (14, -12), (10.5, 11), (-10.5, 11)])
      , Color coatShade (Polygon [(3, -12), (14, -12), (10.5, 11), (2, 11)])
      ]
    head' = Pictures
      [ Color outline (Translate 0 15 (ngon 16 13))
      , Color hood (Translate 0 15 (ngon 16 11))
      ]
    front = Pictures
      [ legs (max 0 stride) (max 0 (negate stride))
      , trunk
      , Color outline (rect (-1) (-12) 2 21)
      , Color outline (Pictures [Translate (-5) (-2) (ngon 5 1.2), Translate (-5) 4 (ngon 5 1.2)])
      , head'
      , Color outline (Translate 0 13 (ellipse 12 7.5 6.5))
      , Color skin (Translate 0 13 (ellipse 12 6.2 5.2))
      , Color outline (Pictures [rect (-3.5) 13 2 2.2, rect 1.5 13 2 2.2])
      ]
    back = Pictures
      [ legs (max 0 stride) (max 0 (negate stride))
      , trunk
      , Color (hex 0x8C5A2B) (rect (-6) (-4) 12 11)
      , Color outline (Pictures [rect (-6) 6 12 1.2, rect (-6) (-4) 12 1.2])
      , head'
      , Color coatShade (rect (-1) 6 2 16)
      ]
    side = Pictures
      [ legs stride (negate stride)
      , trunk
      , Color coatShade (rect (-3) (-9) 7 13)
      , head'
      , Color outline (Translate (-5) 13 (ellipse 10 6 6))
      , Color skin (Translate (-6) 13 (ellipse 10 4.8 4.8))
      , Color outline (rect (-9) 13 2 2.2)
      , Color coatShade (Polygon [(-13, 22), (-4, 26), (2, 15), (-11, 19)])
      ]

-- The power cell ---------------------------------------------------------------

goldCore, goldGlow :: Color
goldCore = hex 0xFFF6D2
goldGlow = hex 0xFFC24A

-- | The cell as an object: a glass capsule with brass caps. Colour comes from
-- this picture, so it reads the same on the floor and in the HUD.
cellBody :: Float -> Picture
cellBody pulse = Pictures
  [ Color (hex 0x17100B) (rectC 18 30)
  , Color (hex 0xC89C50) (Pictures [rect (-8) 9 16 5, rect (-8) (-14) 16 5])
  , Color (hex 0xF2D083) (rect (-8) 12 16 1.5)
  , Color (mix pulse (hex 0xFFD36E) goldCore) (rectC 12 18)
  , Color (hexA 0xFFFFFF 0.85) (rect (-4) (-7) 2.5 14)
  , Color (hex 0x17100B) (Pictures [rect (-3) 14 6 2.5])
  ]

-- | The cell on its cradle, in the past: a small, bright focal point.
powerCell :: Float -> Picture
powerCell clock = Pictures
  [ Color (hexA 0xFFC24A 0.14) (ellipse 28 (46 + 5 * pulse) (34 + 4 * pulse))
  , Color (hexA 0xFFC24A 0.16) (ellipse 24 (28 + 3 * pulse) (20 + 2 * pulse))
  , Translate 0 (5 + 2.5 * sin (clock * 0.05)) $ Pictures
      [ glow goldGlow (24 + 5 * pulse) 4
      , Rotate (clock * 1.3) (orbit 22)
      , Rotate (negate clock * 0.8) (orbit 16)
      , Scale 0.92 0.92 (cellBody pulse)
      ]
  ]
  where
    pulse = 0.5 + 0.5 * sin (clock * 0.09)
    orbit r = Pictures [ Color (fade 0.9 goldCore) (Rotate a (Translate r 0 (rectC 2.4 2.4)))
                       | a <- [0, 120, 240] ]

-- | The HUD's version.
cellIcon :: Bool -> Float -> Picture
cellIcon has clock
  | has = Pictures [glow goldGlow 26 4, cellBody (0.5 + 0.5 * sin (clock * 0.09))]
  | otherwise = Pictures
      [ Color (hexA 0xFFFFFF 0.28) (Pictures [segs 1.5 [(-9, -15), (9, -15), (9, 15), (-9, 15), (-9, -15)]])
      , Color (hexA 0xFFFFFF 0.14) (rectC 11 17)
      ]

-- | The small light that stays with the keeper once they carry the cell.
carriedLight :: Float -> Float -> Picture
carriedLight clock age = Pictures
  [ Translate 0 (-8) (pool 0xFFC879 (0.26 * grow) (100 * grow) (76 * grow))
  , Translate (24 * cos (clock * 0.045)) (14 + 6 * sin (clock * 0.09)) (Pictures
      [ glow goldGlow (17 + 2 * pulse) 3
      , Color goldCore (ngon 12 5)
      , Color (hex 0xFFFFFF) (ngon 8 2.6)
      ])
  ]
  where
    grow = smooth (age / 40)
    pulse = sin (clock * 0.12)

-- | The flash at the moment of pickup.
pickupBurst :: Int -> Picture
pickupBurst age = Pictures
  [ Color (fade (0.75 * (1 - t)) goldCore) (ring 24 (6 + 90 * e) (2 + 4 * (1 - t)) 1)
  , Color (fade (0.5 * (1 - t) * (1 - t)) goldCore) (ngon 22 (10 + 34 * e))
  , Pictures
      [ Color (fade (1 - t) goldCore)
          (Rotate (fromIntegral i * 36) (Translate (14 + 76 * e) 0 (rectC (10 * (1 - t) + 2) 2)))
      | i <- [0 .. 9 :: Int] ]
  ]
  where
    t = clamp01 (fromIntegral age / 40)
    e = 1 - (1 - t) * (1 - t)

-- Victory ----------------------------------------------------------------------

-- | Where the lantern room sits, relative to the lighthouse cell's centre.
lanternOffset :: (Float, Float)
lanternOffset = (0, 34)

-- | The lamp burning again.
lanternGlow :: Float -> Float -> Picture
lanternGlow clock k = at lanternOffset (Pictures
  [ pool 0xFFD98A (0.42 * k) (72 + 6 * sin (clock * 0.05)) (72 + 6 * sin (clock * 0.05))
  , Color (fade k (hex 0xFFF1C2)) (rect (-8.5) (-6.5) 17 13)
  , Color (fade k (hex 0xFFFFFF)) (rect (-4) (-5) 8 10)
  ])

-- | Long beams turning slowly out of the lantern.
victoryRays :: Float -> Float -> Picture
victoryRays clock k = at lanternOffset (Pictures
  [ Rotate (clock * 0.12 + fromIntegral i * 60) (beam (fromIntegral i))
  | i <- [0 .. 5 :: Int] ])
  where
    reach = 1400 * smooth k
    beam i = Pictures
      [ Color (hexA 0xFFD98A (0.085 * k)) (Polygon [(0, 0), (reach, -reach * 0.1 - 6 * i), (reach, reach * 0.1 + 6 * i)])
      , Color (hexA 0xFFEDBE (0.075 * k)) (Polygon [(0, 0), (reach, -reach * 0.045), (reach, reach * 0.045)])
      ]

-- | The wall lamps, lit one by one as the warmth reaches them.
relitSconces :: Float -> (Float, Float) -> Picture
relitSconces age origin = Pictures
  [ at p (Pictures
      [ pool 0xFFC879 (0.17 * k) 165 125
      , Color (fade k (hex 0xFFE9B0)) (rect (-4) (-3) 8 8)
      ])
  | c <- sconceCells
  , let (px, py) = cellPos c
        p = (px, py - half + 12)
        d = dist p origin
        k = smooth ((age - d * 0.12) / 40) ]

-- | Warm light soaking outward from the lighthouse across the whole scene.
warmthOver :: Float -> (Float, Float) -> Picture
warmthOver age origin = at origin (pool 0xFFD48A 0.2 r (r * 0.86))
  where r = 40 + 1180 * smooth (age / 110)

dist :: (Float, Float) -> (Float, Float) -> Float
dist (a, b) (c, d) = sqrt ((a - c) * (a - c) + (b - d) * (b - d))

-- Ambient ----------------------------------------------------------------------

-- | Bounded drifting specks. Past: lit dust rising. Present: wind-driven spray
-- crossing the other way. A pure function of the clock.
ambient :: Era -> Float -> Float -> Picture
ambient era clock k
  | k <= 0.01 = Blank
  | otherwise = Pictures (map speck [0 .. count - 1])
  where
    count = 26 :: Int
    w = mapRight - mapLeft
    h = mapTop - mapBottom
    wrapTo size v = v - size * fromIntegral (floor (v / size) :: Int)
    speck i =
      let fx = rnd i 11 7
          fy = rnd i 13 8
          speed = 0.6 + 0.8 * rnd i 17 9
      in case era of
        Past ->
          let x = mapLeft + wrapTo w (fx * w + 12 * sin (clock * 0.01 + fromIntegral i))
              y = mapBottom + wrapTo h (fy * h + clock * 0.18 * speed)
              a = 0.14 + 0.3 * (0.5 + 0.5 * sin (clock * 0.03 + fromIntegral i))
          in Translate x y (Color (hexA 0xFFE2A8 (a * k)) (rectC 2.4 2.4))
        Present ->
          let x = mapLeft + wrapTo w (fx * w - clock * 1.5 * speed)
              y = mapBottom + wrapTo h (fy * h - clock * 0.65 * speed)
              a = 0.12 + 0.2 * rnd i 19 3
          in Translate x y (Color (hexA 0xB7CEDA (a * k)) (Rotate (-24) (rectC 10 1.3)))

-- The shift --------------------------------------------------------------------

-- | The bright front of the era wave.
waveRing :: Float -> Picture
waveRing t = Pictures
  [ Color (hexA 0xFFFFFF (0.5 * (1 - t))) (ring 40 (r - 3) 3 1)
  , Color (hexA 0xFFFFFF (0.16 * (1 - t))) (ring 40 (r - 16) 13 1)
  ]
  where r = 20 + 1050 * t
