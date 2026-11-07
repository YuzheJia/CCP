-- | Things that move or shine on top of the station.
module Figures
  ( keeper
  , powerCell
  , crate
  , crossingMark
  , echoFigure
  , footprintMark
  , landingMark
  , heldPlateGlow
  , mirrorArt
  , beamLight
  , jumpArrow
  , blockedLanding
  , screenDir
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
import Propaganda (star)
import Station (pool, sconceCells)

-- The keeper -------------------------------------------------------------------

-- | The lighthouse keeper in a yellow oilskin. @phase@ runs 0..1 across a step.
-- In the past they wear the red scarf of a model worker.
keeper :: Era -> Dir -> Float -> Picture
keeper era facing phase = Scale 1.08 1.08 (Pictures
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
    figure = Pictures [body, if era == Past then scarf else Blank]
    body = case facing of
      DirDown  -> front
      DirUp    -> back
      DirLeft  -> side
      DirRight -> Scale (-1) 1 side
    scarfRed = hex 0xD7191F
    redDark = hex 0x8E1014
    -- Knotted at the throat; the tails hang in front, behind, or fly behind a walker.
    scarf = case facing of
      DirDown -> Pictures
        [ Color redDark (Polygon [(-11.5, 5), (11.5, 5), (11, 10), (-11, 10)])
        , Color scarfRed (Polygon [(-10.5, 6), (10.5, 6), (10, 9.5), (-10, 9.5)])
        , Color redDark (Polygon [(1, 6), (7, 6), (6, -6), (2, -4)])
        , Color scarfRed (Polygon [(2, 5.5), (6, 5.5), (5, -4.5), (2.8, -3)])
        ]
      DirUp -> Pictures
        [ Color redDark (Polygon [(-11.5, 5), (11.5, 5), (11, 10), (-11, 10)])
        , Color scarfRed (Polygon [(-10.5, 6), (10.5, 6), (10, 9.5), (-10, 9.5)])
        ]
      DirLeft -> sideScarf
      DirRight -> Scale (-1) 1 sideScarf
    sideScarf = Pictures
      [ Color redDark (Polygon [(-11, 5), (11, 5), (10.5, 10), (-10.5, 10)])
      , Color scarfRed (Polygon [(-10, 6), (10, 6), (9.5, 9.5), (-9.5, 9.5)])
      , Color scarfRed (Polygon [(8, 8), (17, 6 + 2 * sin (phase * 2 * pi)), (16, 1 + 2 * sin (phase * 2 * pi)), (8, 4)])
      ]
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
  , Color (hex 0xC8161D) (Translate 1.5 (-1) (star 5))
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

-- | The crate: one object, seen at two times. Kept timber and brass that a keeper
-- could shove, then a seized block of rust. The same outline in both, raised above
-- the floor, so it reads as an obstacle in either era.
crate :: Era -> Picture
crate era = Pictures
  [ Color (hexA 0x000000 0.4) (Translate 2 (-4) (rectC 62 62))
  , Color edge (rectC 62 62)
  , Color front (rect (-29) (-29) 58 13)
  , Color top (rect (-29) (-16) 58 45)
  , Color shine (rect (-29) 26 58 3)
  , details
  ]
  where
    (edge, front, top, shine) = case era of
      Past    -> (hex 0x2E1C0E, hex 0x70482A, hex 0xA8763F, hex 0xD6A661)
      Present -> (hex 0x14100E, hex 0x4A2C1C, hex 0x7A4A2C, hex 0xA06A42)
    details = case era of
      Past -> Pictures
        [ Color (hex 0x70482A) (Pictures [ rect (-29) y 58 1.5 | y <- [-1, 13] ])
        , Color (hex 0x70482A) (seg 3 (-27, -14) (27, 26))
        , Color (hex 0xE8C47A) (Pictures
            [ rect (-29) 19 9 9, rect 20 19 9 9, rect (-29) (-16) 9 9, rect 20 (-16) 9 9 ])
        , Color (hex 0x5C4122) (Pictures
            [ Translate x y (ngon 6 1.6) | x <- [-25, 24], y <- [-12, 23] ])
        ]
      Present -> Pictures
        [ Color (hex 0x4A2C1C) (Pictures [ rect (-29) y 58 1.5 | y <- [-1, 13] ])
        , Color (hex 0x4A2C1C) (seg 3 (-27, -14) (27, 26))
        , Color (hexA 0xC27A3E 0.55) (Pictures
            [ rect (-18) (-16) 4 22, rect 3 (-6) 5 26, rect 18 (-16) 3 16 ])
        , Color (hex 0x2A1A12) (Pictures
            [ Translate x y (ngon 6 2.4) | x <- [-25, 24], y <- [-12, 23] ])
        , Color (hexA 0x5F7F4E 0.85) (Translate (-14) (-17) (ellipse 8 13 5))
        , Color (hexA 0x5F7F4E 0.7) (Translate 22 22 (ellipse 7 6 4))
        ]

-- | Four corner ticks on a cell you could switch era on. Shape, not just colour.
crossingMark :: Era -> Picture
crossingMark era = Color shade (Pictures
  [ Translate x y (Pictures [rect (min 0 (negate (signum x) * len)) (-1) len 2, rect (-1) (min 0 (negate (signum y) * len)) 2 len])
  | x <- [-corner, corner], y <- [-corner, corner] ])
  where
    corner = half - 7
    len = 9
    shade = case era of
      Past    -> hexA 0xFFE2A8 0.6
      Present -> hexA 0x9CC7D8 0.6

-- The echo ---------------------------------------------------------------------

-- | The keeper's echo: their outline in pale light. @k@ is how strongly it shows,
-- lower when it stands in the era the player is not looking at.
echoFigure :: Float -> Float -> Picture
echoFigure clock k = Scale 1.08 1.08 (Pictures
  [ Color (hexA 0x9CE7FF (0.16 * k)) (Translate 0 (-2) (ngon 24 (24 + 2 * sin (clock * 0.08))))
  , Color (hexA 0x0A2A33 (0.35 * k)) (Translate 0 (-20) (ellipse 16 15 6))
  , Color (hexA 0x9CE7FF (0.55 * k)) (Polygon [(-14, -12), (14, -12), (10.5, 11), (-10.5, 11)])
  , Color (hexA 0x9CE7FF (0.55 * k)) (Translate 0 15 (ngon 16 11))
  , Color (hexA 0xE6FAFF (0.85 * k)) (Pictures
      [ segs 1.6 [(-14, -12), (14, -12), (10.5, 11), (-10.5, 11), (-14, -12)]
      , ring 16 11 1.6 1 `atY` 15
      ])
  ])
  where atY p y = Translate 0 y p

-- | One of the footprints the echo would walk back along. @k@ fades the older ones.
footprintMark :: Float -> Picture
footprintMark k = Color (hexA 0x9CE7FF (0.45 * k)) (Pictures
  [ Translate (-6) (-3) (rot 10 (ellipse 8 4 6))
  , Translate 6 4 (rot 10 (ellipse 8 4 6))
  ])

-- | The footprint an echo sent now would stop on, ringed and turning.
landingMark :: Float -> Picture
landingMark clock = Pictures
  [ Color (hexA 0x9CE7FF (0.55 + 0.25 * sin (clock * 0.12))) (ring 28 30 2.5 1)
  , Color (hexA 0xE6FAFF 0.9) (Pictures
      [ rot (clock * 1.5 + a) (rect 18 (-2) 12 4) | a <- [0, 45 .. 315] ])
  ]

-- | A plate with something on it, lit from below.
heldPlateGlow :: Float -> Picture
heldPlateGlow clock = Pictures
  [ glow (hexA 0x9CE7FF 1) 30 4
  , Color (hexA 0xE6FAFF (0.5 + 0.2 * sin (clock * 0.1))) (ring 24 22 2 1)
  ]

-- Mirrors ----------------------------------------------------------------------

-- | A direction on screen, where up is +y.
screenDir :: Dir -> (Float, Float)
screenDir d = case d of
  DirUp    -> (0, 1)
  DirDown  -> (0, -1)
  DirLeft  -> (-1, 0)
  DirRight -> (1, 0)

-- | Gloss turns clockwise, in degrees.
faceAngle :: Dir -> Float
faceAngle d = case d of
  DirUp    -> 0
  DirRight -> 90
  DirDown  -> 180
  DirLeft  -> 270

beamColor :: Era -> Int
beamColor Past    = 0xFFE7B0
beamColor Present = 0xBFEFFF

-- | A standing mirror filling its cell, its glass on the side it faces: a brass frame
-- in the past, tarnished steel in the present.
mirrorArt :: Era -> Dir -> Float -> Picture
mirrorArt era face clock = rot (faceAngle face) (Pictures
  [ Color (hexA 0x000000 0.45) (rect (-half + 2) (-half + 2) (cs - 4) (cs - 4))
  , Color frame (rect (-half + 5) (-half + 5) (cs - 10) (cs - 10))
  , Color backing (rect (-half + 11) (-half + 11) (cs - 22) (cs - 26))
  , Color (hexA 0xFFFFFF 0.18) (Polygon [(-14, -20), (-4, -20), (8, 14), (-2, 14)])
  , Color (hexA 0xFFFFFF 0.12) (Polygon [(2, -20), (7, -20), (19, 14), (14, 14)])
  , Color (hexA (beamColor era) 0.9) (rect (-half + 8) (half - 15) (cs - 16) 8)
  , Color (hexA 0xFFFFFF (0.55 + 0.3 * sin (clock * 0.07))) (rect (-half + 14) (half - 13) 16 3)
  , Color (hexA (beamColor era) 0.35) (rect (-half + 4) (half - 6) (cs - 8) 3)
  ])
  where
    (frame, backing) = case era of
      Past    -> (hex 0xC2964F, hex 0x56676B)
      Present -> (hex 0x6E7C82, hex 0x2E3D44)

-- | The light a mirror throws, from its glass to the last cell it reaches.
beamLight :: Era -> Float -> (Float, Float) -> (Float, Float) -> Picture
beamLight era clock from to = Pictures $
  (if from == to then [] else
    [ Color (hexA c (0.10 + 0.03 * sin (clock * 0.11))) (seg 34 from to)
    , Color (hexA c 0.16) (seg 14 from to)
    , Color (hexA c 0.55) (seg 3 from to)
    ]) ++
  -- Where the light stops: on whatever is in its way, so a blocked beam still shows.
  [ at to (Pictures [ glow (hexA c 0.9) 18 3, Color (hexA 0xFFFFFF 0.8) (ngon 10 3.5) ]) ]
  where c = beamColor era

-- | A chevron beside the keeper, pointing at the mirror: this key steps through.
jumpArrow :: Dir -> Float -> Picture
jumpArrow d clock = rot (faceAngle d) (Translate 0 (46 + 3 * sin (clock * 0.2)) (Pictures
  [ Color (hexA 0x0A2A33 0.6) (Polygon [(-11, -3), (0, 9), (11, -3)])
  , Color (hexA 0xE6FAFF 0.95) (Polygon [(-9, -2), (0, 7), (9, -2)])
  ]))

-- | Where a reflection would land, but cannot: crossed out.
blockedLanding :: Picture
blockedLanding = Color (hexA 0xFF7A4A 0.75) (Pictures
  [ segs 3 [(-16, -16), (16, 16)]
  , segs 3 [(-16, 16), (16, -16)]
  , ring 24 22 2 1
  ])

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
