-- | The station itself: one painting for each cell of each era.
--
-- Every cell is a separate picture drawn inside its own square, so the era wave
-- can flip them one by one and nothing painted can disagree with the collision
-- map. Walls are always solid-looking and bright-capped; floors are always low
-- and quiet. The same foundations, floor inlay, carpet, conduit and doorways appear
-- in both eras: one kept, one left to the sea.
module Station
  ( cellArt
  , seaBackdrop
  , seaLife
  , seaShade
  , shore
  , farSurf
  , foundation
  , pool
  , lightPools
  , sconceCells
  , filledPit
  , doorway
  , vignette
  , presentRubble
  , pastBulkhead
  ) where

import Draw
import Font
import Game.Level
import Game.Types
import Graphics.Gloss
import qualified Data.Set as Set

-- Palettes ---------------------------------------------------------------------

data Pal = Pal
  { pFloorA, pFloorB, pGrout, pInlay, pCap, pCapHi, pCapLo, pFace, pFaceDark, pMortar
  , pShadow, pPipe, pPipeHi, pStencil :: Color }

palette :: Era -> Pal
palette Past = Pal
  { pFloorA = hex 0x5E3F2A, pFloorB = hex 0x553824, pGrout = hex 0x38231A
  , pInlay = hex 0xB98C4A
  , pCap = hex 0xC6A679, pCapHi = hex 0xE8D3A8, pCapLo = hex 0x9C7E58
  , pFace = hex 0x8E4A2E, pFaceDark = hex 0x5C2C1C, pMortar = hex 0xB57F5C
  , pShadow = hexA 0x170A05 0.42
  , pPipe = hex 0xB5853A, pPipeHi = hex 0xF2D083
  , pStencil = hexA 0xEBD3A3 0.28 }
palette Present = Pal
  { pFloorA = hex 0x2A373E, pFloorB = hex 0x253137, pGrout = hex 0x151D22
  , pInlay = hex 0x4E5D60
  , pCap = hex 0x83929A, pCapHi = hex 0xAEBBC0, pCapLo = hex 0x5D6B72
  , pFace = hex 0x3A464D, pFaceDark = hex 0x1E272C, pMortar = hex 0x56646B
  , pShadow = hexA 0x03070A 0.5
  , pPipe = hex 0x6B4630, pPipeHi = hex 0x94684A
  , pStencil = hexA 0xA9B6B8 0.16 }

-- Neighbours -------------------------------------------------------------------

solid :: Level -> Era -> Int -> Int -> Bool
solid level era x y = not (walkable level era (x, y) || isPit level era (x, y))

-- | A pit stops the player but is not a wall: it is low, and casts no wall's shadow.
isPit :: Level -> Era -> Cell -> Bool
isPit level era cell = era == Past && cell `Set.member` lvPits level

-- | Painting for a cell, in coordinates local to the cell's centre.
cellArt :: Level -> Era -> Cell -> Picture
cellArt level era cell@(x, y)
  | isPit level era cell = pit level cell
  | era == Past && cell == door && pastOnly = pastDoor level (x, y)
  | era == Present && cell == door && pastOnly = presentRubble
  | era == Past && cell == breach && presentOnly = pastBulkhead
  | era == Present && cell == breach && presentOnly = presentBreach level
  | solid level era x y = wall level era cell
  | otherwise = Pictures
      [ floorTile level era cell
      , floorDressing level era cell
      ]
  where
    door = (4, 2)
    breach = (8, 6)
    -- The painted passages say "open here, closed there", so they only appear where
    -- that is true of this map.
    pastOnly = walkable level Past cell && not (walkable level Present cell)
    presentOnly = walkable level Present cell && not (walkable level Past cell)

-- | Past only: a shaft in the floor, ringed in hazard paint. It has to read as a
-- place that cannot be walked on, and as a place something could be dropped into.
pit :: Level -> Cell -> Picture
pit level cell = Pictures
  [ floorTile level Past cell
  , Color (hex 0x17100B) (rectC (cs - 6) (cs - 6))
  , Color (hex 0xE0B23C) (rectC (cs - 12) (cs - 12))
  , Color (hex 0x241610) (Pictures [ Translate o (half - 9) (rot 45 (rectC 6 15)) | o <- stripes ])
  , Color (hex 0x241610) (Pictures [ Translate o (9 - half) (rot 45 (rectC 6 15)) | o <- stripes ])
  , Color (hex 0x241610) (Pictures [ Translate (half - 9) o (rot 45 (rectC 15 6)) | o <- stripes ])
  , Color (hex 0x241610) (Pictures [ Translate (9 - half) o (rot 45 (rectC 15 6)) | o <- stripes ])
  , Color (hex 0x050302) (rectC (cs - 26) (cs - 26))
  , Color (hex 0x1A100A) (rect (negate (half - 13)) (half - 21) (cs - 26) 8)
  ]
  where stripes = [-20, -7, 7, 20]

-- | Present only, drawn over the wall the map has there. Shut, it is a barred iron
-- gate; open, the bars are gone and the floor runs through. Its lamp agrees, but the
-- shape says it first.
doorway :: Level -> Cell -> Bool -> Picture
doorway level cell open
  | open = Pictures
      [ floorTile level Present cell
      , Color (hex 0x83929A) (Pictures [rect (-half) (-half) 7 cs, rect (half - 7) (-half) 7 cs])
      , Color (hex 0xAEBBC0) (rect (-half) (half - 9) cs 9)
      , Translate 0 (half - 5) (Color (hex 0x9CE0B0) (ngon 8 4))
      ]
  | otherwise = Pictures
      [ Color (hex 0x83929A) (rectC cs cs)
      , Color (hex 0x10171B) (rectC (cs - 14) (cs - 14))
      , Color (hex 0xAEBBC0) (Pictures [ rect x (-half + 7) 6 (cs - 14) | x <- [-25, -9, 7, 23] ])
      , Color (hex 0x5D6B72) (Pictures [ rect (-half + 7) y (cs - 14) 5 | y <- [-14, 12] ])
      , Translate 0 (half - 5) (Pictures
          [ Color (hex 0x10171B) (ngon 8 5.5), Color (hex 0xFF7A4A) (ngon 8 3.5) ])
      ]

-- | A plate set in the floor, in both eras, because a crate rests on it in both.
plate :: Era -> Picture
plate era = Pictures
  [ Color rim (rectC 50 50)
  , Color face (rectC 42 42)
  , Color mark (Pictures [rectC 30 3, rectC 3 30])
  , Color rim (ngon 12 6)
  , Color mark (ngon 12 3.5)
  ]
  where
    (rim, face, mark) = case era of
      Past    -> (hex 0x2A1A0E, hex 0xC89C50, hex 0xF2D083)
      Present -> (hex 0x10171B, hex 0x6B4630, hex 0x94684A)

-- | Past only: the same pit with a crate wedged in it, flush, and safe to walk over.
filledPit :: Level -> Cell -> Picture
filledPit level cell = Pictures
  [ floorTile level Past cell
  , Color (hex 0x17100B) (rectC (cs - 6) (cs - 6))
  , Color (hex 0x4E3820) (rectC (cs - 12) (cs - 12))
  , Color (hex 0x2E2012) (Pictures [ rect (-half + 6) y (cs - 12) 1.5 | y <- [-17, -3, 11] ])
  , Pictures [ Translate x y (Color (hex 0x8A6A3F) (ngon 6 2)) | x <- [-26, 26], y <- [-26, 26] ]
  ]

-- Floors -----------------------------------------------------------------------

floorTile :: Level -> Era -> Cell -> Picture
floorTile level era (x, y) = Pictures $
  [ Color base (rectC cs cs)
  , Color (pGrout p) (Pictures [rect (-half) (half - 1.5) cs 1.5, rect (-half) (-half) 1.5 cs])
  , Color (fade 0.5 (pGrout p)) (Pictures [rect (-half) (-half) cs 1, rect (half - 1) (-half) 1 cs])
  ] ++ inlay ++ damage ++ shadows
  where
    p = palette era
    base = if even (x + y) then pFloorA p else pFloorB p
    n s = rnd x y s
    -- The same inlay in both eras: a diamond at each tile's heart. The sea has
    -- taken some of them in the present.
    inlay
      | era == Past =
          [ Color (fade 0.55 (pInlay p)) (rot 45 (rectC 15 15))
          , Color base (rot 45 (rectC 10 10))
          , Color (pInlay p) (rot 45 (rectC 4.5 4.5))
          ]
      | n 1 < 0.28 = [Color (fade 0.5 (pGrout p)) (rot 45 (rectC 15 15))]
      | otherwise =
          [ Color (fade 0.55 (pInlay p)) (rot 45 (rectC 15 15))
          , Color base (rot 45 (rectC 10 10))
          , Color (fade 0.8 (pInlay p)) (rot 45 (rectC 4.5 4.5))
          ]
    damage
      | era == Past = []
      | otherwise = crack ++ chip
    crack
      | n 2 < 0.62 =
          let sx = -30 + 60 * n 3
              sy = -30 + 60 * n 4
              path = take 4 (iterate (\(a, b) -> (a + 8 + 12 * n 5, b + (-14) + 26 * n 6)) (sx, sy))
          in [thin (hex 0x10171B) (map keepIn path)]
      | otherwise = []
    chip
      | n 7 < 0.34 =
          let cx = (if n 8 < 0.5 then -1 else 1) * half * 0.72
              cy = (if n 9 < 0.5 then -1 else 1) * half * 0.72
          in [Color (hexA 0x10171B 0.38) (Translate cx cy (rot (40 * n 10) (rectC 13 9)))]
      | otherwise = []
    keepIn (a, b) = (max (-half) (min half a), max (-half) (min half b))
    shadows =
      [ Color (pShadow p) (rect (-half) (half - 13) cs 13) | solid level era x (y - 1) ] ++
      [ Color (fade 0.5 (pShadow p)) (rect (-half) (half - 22) cs 9) | solid level era x (y - 1) ] ++
      [ Color (fade 0.7 (pShadow p)) (rect (-half) (-half) 9 cs) | solid level era (x - 1) y ] ++
      [ Color (fade 0.25 (pShadow p)) (rect (half - 5) (-half) 5 cs) | solid level era (x + 1) y ]

floorDressing :: Level -> Era -> Cell -> Picture
floorDressing level era cell@(x, y) = Pictures $
  (if lvDressed level then carpet ++ water ++ stencil else []) ++ sand ++ moss ++ landmark
  where
    p = palette era
    n s = rnd x y s
    -- A runner down the middle hall. In the present it has rotted to strips.
    carpet
      | x /= 6 || y < 1 || y > 7 = []
      | era == Past =
          [ Color (hex 0x7E2A25) (rectC 34 cs)
          , Color (hex 0xC89C50) (Pictures [rect (-14) (-half) 1.5 cs, rect 12.5 (-half) 1.5 cs])
          , Color (hex 0xC89C50) (rot 45 (rectC 7 7))
          ] ++ topShade
      | otherwise =
          [ Color (hexA 0x5A3A36 0.5) (rect (-17 + 6 * n 21) (-half) (20 + 8 * n 22) cs)
          , thins (hexA 0x2A1E1E 0.7)
              [ [(sx, -half + 6 + 30 * n (24 + i)), (sx + 2 - 4 * n (28 + i), half - 4 - 30 * n (32 + i))]
              | i <- [0 .. 2], let sx = -14 + 14 * fromIntegral i + 4 * n (36 + i) ]
          , Color (fade 0.8 (pFloorA p)) (rect (-19) (-half + 40 * n 40) 40 (5 + 12 * n 41))
          ] ++ topShade
    topShade = [Color (pShadow p) (rect (-17) (half - 13) 34 13) | y == 1]
    -- Sand and lichen: only in the present, only flat.
    sand
      | era == Present && n 30 < 0.3 =
          [ Translate (-20 + 40 * n 31) (-20 + 40 * n 32)
              (Color (hexA 0x7D7766 0.22) (ellipse 10 (14 + 12 * n 33) (6 + 6 * n 34))) ]
      | otherwise = []
    moss
      | era == Past = []
      | otherwise =
          [ Translate (-half + 8 + 60 * n (40 + i)) (half - 5 - 6 * n (50 + i))
              (Color (hexA 0x5F7F4E (0.5 + 0.3 * n (60 + i))) (ellipse 7 (3 + 4 * n (70 + i)) (2 + 2 * n (80 + i))))
          | solid level era x (y - 1), i <- [1 .. 4], n (90 + i) < 0.7 ] ++
          [ Translate (-half + 4 + 5 * n (100 + i)) (half - 8 - 60 * n (110 + i))
              (Color (hexA 0x5F7F4E (0.4 + 0.3 * n (120 + i))) (ellipse 7 (2 + 2 * n (130 + i)) (3 + 4 * n (140 + i))))
          | solid level era (x - 1) y, i <- [1 .. 3], n (150 + i) < 0.6 ]
    -- Seawater, gathered where the floor has sunk: near the breach and elsewhere.
    water
      | era == Present && cell `elem` [(9, 6), (11, 6), (7, 6), (6, 4), (1, 5)] =
          [ Translate (-6 + 12 * n 160) (-6 + 12 * n 161)
              (Pictures
                [ Color (hexA 0x4D7486 0.45) (ellipse 14 (24 + 8 * n 162) (13 + 5 * n 163))
                , thin (hexA 0xB6D2DC 0.4) [(-12, 4), (-2, 6), (9, 4)]
                ]) ]
      | otherwise = []
    -- Stencilled room names: painted fresh, then worn away letter by letter.
    stencil = case (cell, era) of
      ((2, 7), Past)    -> [word "LAMP"]
      ((2, 7), Present) -> [word "L MP"]
      ((5, 7), Past)    -> [word "HALL"]
      ((5, 7), Present) -> [word "H  L"]
      ((10, 7), Past)   -> [word "CELLS"]
      ((10, 7), Present) -> [word "C LLS"]
      _ -> []
    word s = Color (pStencil p) (Translate 0 (-half + 10) (textC 10 s))
    landmark
      | cell == lvGoal level = [case era of { Past -> lampFooting; Present -> towerAt }]
      | cell == lvPowerCell level = [cradle era]
      | cell `Set.member` lvPlates level = [plate era]
      | lvDressed level && (cell == (2, 1) || cell == (10, 1)) = [channel era]
      | otherwise = []

-- | The conduit from the store to the lighthouse, sunk in the floor.
channel :: Era -> Picture
channel era = case era of
  Past -> Pictures
    [ Color (hexA 0x2A170C 0.8) (rect (-4) (-half) 8 cs)
    , Color (hex 0xB5853A) (rect (-2) (-half) 4 cs)
    , Color (hex 0xF2D083) (rect (-0.5) (-half) 1 cs)
    ]
  Present -> Pictures
    [ Color (hexA 0x10171B 0.8) (rect (-4) (-half) 8 cs)
    , Color (hex 0x6B4630) (rect (-2) (-half) 4 34)
    , Color (hex 0x6B4630) (rect (-2) 12 4 16)
    ]

-- | The ring a tower will stand on. Past: an ordinary, polished floor inlay.
lampFooting :: Picture
lampFooting = Translate 0 (-6) $ Pictures
  [ Color (hexA 0x2A170C 0.5) (ellipse 28 30 21)
  , Color (hex 0xC89C50) (ring 28 28 2 0.7)
  , Color (hexA 0xC89C50 0.6) (ring 24 19 1.2 0.7)
  , Color (hexA 0xF0DDB4 0.65) (Pictures [tri (-4, 0) (4, 0) (0, 16), tri (-4, 0) (4, 0) (0, -16)
                                         , tri (0, -3) (0, 3) (-20, 0), tri (0, -3) (0, 3) (20, 0)])
  ]

-- | The lighthouse in the present: same footing, tapered banded tower, dead lamp.
towerAt :: Picture
towerAt = Translate 0 (-8) $ Scale 1.16 1.16 $ Pictures
  [ Color (hexA 0x03070A 0.5) (Translate 3 (-2) (ellipse 28 32 22))
  , Color (hex 0x4F5B60) (ellipse 28 30 21)
  , Color (hex 0x6E7C82) (ring 28 28 2.5 0.7)
  , bands
  , door
  , Translate 0 0 lantern
  ]
  where
    bottom = -8 :: Float
    top = 34 :: Float
    widthAt y = 19 - 8 * (y - bottom) / (top - bottom)
    band i colr =
      let y0 = bottom + (top - bottom) * fromIntegral (i :: Int) / 5
          y1 = bottom + (top - bottom) * fromIntegral (i + 1) / 5
      in Color colr (Polygon [(-widthAt y0, y0), (widthAt y0, y0), (widthAt y1, y1), (-widthAt y1, y1)])
    bands = Pictures
      [ band 0 (hex 0xA65842), band 1 (hex 0xC2C5BA), band 2 (hex 0x954A38)
      , band 3 (hex 0xB2B6AC), band 4 (hex 0x8A4434)
      , Color (hexA 0x10171B 0.35) (Polygon [(5, bottom), (widthAt bottom, bottom), (widthAt top, top), (3, top)])
      , thin (hexA 0x10171B 0.8) [(-9, bottom + 4), (-6, 9), (-8, 16), (-3, 25)]
      , Color (hexA 0x5F7F4E 0.8) (Translate (-12) (-5) (ellipse 8 7 4))
      ]
    door = Color (hex 0x1A2024) (Polygon [(-4, -9), (4, -9), (4, 1), (0, 4), (-4, 1)])
    lantern = Pictures
      [ Color (hex 0x2D373C) (ellipse 16 15 5) `at'` (0, top)
      , Color (hex 0x10171B) (rect (-8) (top + 1) 16 11)
      , Color (hex 0x5E6C72) (Pictures [rect (-8) (top + 1) 1.6 11, rect 6.4 (top + 1) 1.6 11, rect (-0.8) (top + 1) 1.6 11])
      , thin (hexA 0x9FB1B8 0.7) [(-5, top + 3), (-2, top + 9)]
      , Color (hex 0x55484A) (Polygon [(-10, top + 12), (10, top + 12), (0, top + 20)])
      ]
    at' pic pos = at pos pic

-- | The bracket the power cell sits in. Shared by both eras.
cradle :: Era -> Picture
cradle era = case era of
  Past -> Pictures
    [ Color (hexA 0x2A170C 0.65) (ellipse 22 22 16)
    , Color (hex 0xC89C50) (ring 22 19 2.5 0.72)
    , Color (hex 0xF0DDB4) (Pictures [rect (-20) (-1) 5 2, rect 15 (-1) 5 2, rect (-1) 12 2 4, rect (-1) (-16) 2 4])
    , Color (hex 0x1F120B) (ellipse 16 9 6)
    ]
  Present -> Pictures
    [ Color (hexA 0x10171B 0.65) (ellipse 22 22 16)
    , Color (hex 0x6B4630) (ring 22 19 2.5 0.72)
    , Color (hexA 0x10171B 0.9) (rect (-23) (-6) 12 12)
    , Color (hex 0x10171B) (ellipse 16 9 6)
    , Color (hexA 0x5F7F4E 0.7) (Translate 12 (-9) (ellipse 8 7 3))
    ]

-- Walls ------------------------------------------------------------------------

faceH :: Float
faceH = 22

wall :: Level -> Era -> Cell -> Picture
wall level era (x, y) = Pictures (body ++ detail ++ (if lvDressed level then conduit else []))
  where
    p = palette era
    n s = rnd x y s
    hasFace = not (solid level era x (y + 1))
    capBottom = if hasFace then -half + faceH else -half
    capH = half - capBottom
    openLeft = not (solid level era (x - 1) y)
    openRight = not (solid level era (x + 1) y)
    openTop = not (solid level era x (y - 1))
    body =
      [ Color (pCap p) (rect (-half) capBottom cs capH)
      , Color (pCapHi p) (rect (-half) (half - 3) cs 3)
      , Color (pCapLo p) (rect (-half) capBottom cs 2.5)
      ] ++
      [ Color (pCapHi p) (rect (-half) capBottom 3 capH) | openLeft ] ++
      [ Color (pCapLo p) (rect (half - 3) capBottom 3 capH) | openRight ] ++
      [ Color (pFaceDark p) (rect (-half) (half - 2) cs 2) | openTop ] ++
      [ Color (pFace p) (rect (-half) (-half) cs faceH) | hasFace ] ++
      [ Color (pFaceDark p) (rect (-half) (-half) cs 3) | hasFace ] ++
      [ brickwork | hasFace ]
    brickwork = thins (pMortar p)
      ( [ [(-half, -half + 8), (half, -half + 8)], [(-half, -half + 15), (half, -half + 15)] ]
        ++ [ [(bx, -half + 8), (bx, -half + 15)] | bx <- [-30, -6, 18] ]
        ++ [ [(bx, -half + 15), (bx, -half + 22)] | bx <- [-18, 6, 30] ]
        ++ [ [(bx, -half + 3), (bx, -half + 8)] | bx <- [-18, 6, 30] ] )
    seams = thins (fade 0.7 (pCapLo p))
      [ [(-half, capBottom + capH * 0.5), (half, capBottom + capH * 0.5)]
      , [(-half * 0.25, capBottom + capH * 0.5), (-half * 0.25, half)]
      , [(half * 0.45, capBottom), (half * 0.45, capBottom + capH * 0.5)]
      ]
    bracket
      | not (lvDressed level && (x, y) `elem` sconceCells) = []
      | era == Past = [ Color (hex 0x3A2413) (rect (-8) (-half + 4) 16 15)
                      , Color (hex 0xC89C50) (rect (-6) (-half + 6) 12 11) ]
      | otherwise   = [ Color (hex 0x10171B) (rect (-8) (-half + 4) 16 15)
                      , Color (hex 0x6B4630) (Polygon [(-6, -half + 6), (5, -half + 6), (-2, -half + 17)]) ]
    detail = bracket ++ case era of
      Past -> [seams, Color (hex 0x8C6A3A) (Translate (half * 0.7) (half * 0.55) (ngon 6 2.2))]
      Present ->
        [ seams ] ++
        [ thin (hex 0x2E383D) (map (\(a, b) -> (a + 40 * (n 200 - 0.5), b + 10 * (n 201 - 0.5)))
            [(-14, half - 3), (-7, 16), (-12, 5), (0, -6)]) | n 202 < 0.75 ] ++
        -- Bites out of the cap's edge, where the top course has crumbled.
        [ Color (pCapLo p) (Translate (-half + 76 * n (210 + i)) (if n (220 + i) < 0.5 then half else capBottom)
            (Polygon [(-7 - 8 * n (230 + i), 0), (9 + 7 * n (240 + i), 0), (3, 3 + 5 * n (250 + i)), (-4, -3 - 5 * n (255 + i))]))
        | i <- [1 .. 2], n (260 + i) < 0.6 ] ++
        [ Color (hexA 0x5F7F4E 0.75) (Translate (-half + 76 * n (270 + i)) (capBottom + 6 * n (280 + i))
            (ellipse 7 (4 + 5 * n (290 + i)) (2 + 2 * n (300 + i))))
        | i <- [1 .. 2], n (310 + i) < 0.7 ] ++
        [ Color (hexA 0x6B4630 0.75) (rect (-half + 76 * n 320) (-half) (2.5 + 2 * n 321) (10 + 10 * n 322))
        | hasFace, n 323 < 0.5 ]
    -- The conduit along the north wall, joining cell store to lighthouse.
    conduit
      | y == 0 && x >= 2 && x <= 10 = case era of
          Past ->
            [ Color (hex 0x5C4122) (rect leftEnd (-5) (rightEnd - leftEnd) 11)
            , Color (pPipe p) (rect leftEnd (-3) (rightEnd - leftEnd) 8)
            , Color (pPipeHi p) (rect leftEnd 2 (rightEnd - leftEnd) 1.5)
            , Color (pPipeHi p) (rect (-4) (-5) 8 12)
            ] ++ downspout
          Present ->
            [ Color (pPipe p) (rect leftEnd (-3) (rightEnd - leftEnd - 34 * brokenBit) 7)
            , Color (pPipeHi p) (rect leftEnd 1 (rightEnd - leftEnd - 34 * brokenBit) 1)
            ] ++ downspout
      | otherwise = []
    leftEnd = if x == 2 then 0 else -half
    rightEnd = if x == 10 then 0 else half
    brokenBit = if x `elem` [5, 6, 9] then 1 else 0
    downspout
      | x == 2 || x == 10 =
          [ Color (pPipe p) (rect (-3) (-half) 7 half)
          , Color (pPipeHi p) (rect (-1) (-half) 1.5 half) ]
      | otherwise = []

-- The two opposite passages ----------------------------------------------------

-- | Past (4,2): a working door, standing open. The floor runs straight through,
-- brightly lit, so it reads as a way on rather than an object.
pastDoor :: Level -> Cell -> Picture
pastDoor level cell = Pictures
  [ floorTile level Past cell
  , Color (hexA 0xFFD78F 0.16) (rectC cs (cs - 6))
    -- Worn path through the doorway.
  , Color (hex 0x7E2A25) (rect (-half) (-15) cs 30)
  , Color (hex 0xC89C50) (Pictures [rect (-half) 13 cs 1.5, rect (-half) (-14.5) cs 1.5])
  , Color (hex 0xC89C50) (rot 45 (rectC 7 7))
    -- Brass thresholds at both faces of the wall.
  , Color (hex 0xE8C47A) (Pictures [rect (-half) (-half + 3) 4 (cs - 6), rect (half - 4) (-half + 3) 4 (cs - 6)])
    -- Door leaf folded back against the north jamb.
  , Color (hex 0x2F5A4E) (rect (-half + 4) (half - 11) (cs - 8) 9)
  , Color (hex 0x4C8A78) (rect (-half + 4) (half - 5) (cs - 8) 2)
  , Color (hex 0xE5C583) (Translate 24 (half - 7) (ngon 8 2.2))
    -- Lip of the south jamb, so the opening has an edge.
  , Color (hex 0x38231A) (rect (-half) (-half) cs 3)
  ]

-- | Present (4,2): the same doorway, fallen in. Solid: as bright as the wall.
presentRubble :: Picture
presentRubble = Pictures
  [ Color (hex 0x5D6B72) (rectC cs cs)
  , Color (hex 0x83929A) (rect (-half) (half - 11) cs 11)
  , Color (hex 0x83929A) (rect (-half) (-half) cs 11)
  , Color (hex 0x151D22) (rect (-half) (-half + 11) cs 3)
  , Color (hex 0x151D22) (rect (-half) (half - 14) cs 3)
  , Pictures (map stone
      [ (-25,  16, 21, 15,  12), ( 8,  20, 24, 16, -10), ( 27,  6, 17, 16,  22)
      , (-28, -10, 17, 18, -18), ( -6,  -1, 25, 20,   6), ( 20, -18, 24, 15,  -6)
      , (-12, -22, 20, 13,  14) ])
    -- The lintel, snapped and lying across the heap.
  , Translate 2 2 (rot (-24) (Pictures
      [ Color (hex 0x151D22) (rect (-34) (-7) 68 13)
      , Color (hex 0x5B4B40) (rect (-34) (-5) 68 11)
      , Color (hex 0x7A6858) (rect (-34) 3 68 2.5)
      , thin (hex 0x2E2520) [(-4, -5), (0, 0), (-3, 6)]
      ]))
  , Color (hex 0xC89C50) (rect (-half) (-half + 13) 3 9)
  ]
  where
    stone (x, y, w, h, a) = Translate x y (rot a (Pictures
      [ Color (hex 0x151D22) (Translate 2 (-3) (rectC w h))
      , Color (hex 0x8A99A0) (rectC w h)
      , Color (hex 0xB0BDC2) (rect (-w / 2) (h / 2 - 3) w 3)
      , Color (hex 0x5D6B72) (rect (-w / 2) (-h / 2) w 3)
      ]))

-- | Past (8,6): the same gap, sealed by a bolted bulkhead. Solid.
pastBulkhead :: Picture
pastBulkhead = Pictures
  [ Color (hex 0xD3B487) (rectC cs cs)
  , Color (hex 0xF0DDB4) (rect (-half) (half - 3) cs 3)
  , Color (hex 0x241610) (rect (-half + 6) (-half + 6) (cs - 12) (cs - 12))
  , Color (hex 0x3F5F58) (rect (-half + 9) (-half + 9) (cs - 18) (cs - 18))
  , Color (hex 0x5F8A80) (rect (-half + 9) (half - 12) (cs - 18) 3)
  , Pictures (map chevron [-20, -8, 4, 16])
  , Color (hex 0x241610) (rect (-half + 9) (-5) (cs - 18) 1)
  , Color (hex 0x241610) (rect (-half + 9) 8 (cs - 18) 1)
  , Pictures [ Translate rx ry (Color (hex 0xC6D3CC) (ngon 6 2.2))
             | rx <- [-26, 26], ry <- [-26, -10, 12, 26] ]
    -- Locking wheel at the heart of the door.
  , Color (hex 0x241610) (ngon 14 11)
  , Color (hex 0xD8A94C) (ring 14 7.5 2.5 1)
  , Color (hex 0xD8A94C) (Pictures [rot 45 (rectC 15 2.2), rot (-45) (rectC 15 2.2)])
  ]
  where
    chevron x = Translate x (-22) (Color (hex 0xE0B23C)
      (Polygon [(-3, -5), (3, -5), (6, 5), (0, 5)]))

-- | Present (8,6): sea erosion has torn the bulkhead out. Walkable, with torn
-- stubs of wall at both jambs and the wet floor running straight through.
presentBreach :: Level -> Picture
presentBreach level = Pictures
  [ floorTile level Present (8, 6)
  , Color (hexA 0x7D7766 0.28) (ellipse 12 26 12)
  , Color (hexA 0x4D7486 0.5) (Translate 10 (-6) (ellipse 14 22 10))
  , thin (hexA 0xB6D2DC 0.45) [(0, -3), (12, -1), (24, -4)]
    -- Torn wall: dark stubs under the face above, broken cap edge below.
  , Color (hex 0x1E272C) (Pictures
      [ rect (-half) (half - 7) cs 7
      , tri (-half, half - 7) (-20, half - 7) (-28, half - 20)
      , tri (-14, half - 7) (6, half - 7) (-5, half - 24)
      , tri (10, half - 7) (half, half - 7) (24, half - 17)
      ])
  , Color (hex 0x151D22) (Pictures
      [ tri (-half, -half + 8) (-14, -half + 8) (-24, -half + 21)
      , tri (-8, -half + 8) (16, -half + 8) (6, -half + 24)
      , tri (20, -half + 8) (half, -half + 8) (30, -half + 19)
      ])
  , Color (hex 0x83929A) (Pictures
      [ rect (-half) (-half) cs 7
      , tri (-half, -half + 7) (-14, -half + 7) (-24, -half + 18)
      , tri (-8, -half + 7) (16, -half + 7) (6, -half + 21)
      , tri (20, -half + 7) (half, -half + 7) (30, -half + 16)
      ])
    -- What is left of the door: a rusted corner, and bent bars.
  , Color (hex 0x6B4630) (Polygon [(half - 3, -half + 7), (half - 3, -half + 22), (half - 19, -half + 7)])
  , Color (hex 0x8A5A3C) (seg 2 (-28, half - 8) (-20, half - 26))
  , Color (hex 0x8A5A3C) (seg 2 (20, half - 8) (26, half - 24))
  ]

-- Outside the walls ------------------------------------------------------------

-- | Sea filling the canvas, drawn once per era. It is drawn at the canvas's own
-- scale, under the board, so its crests sit anywhere the board leaves open, except
-- along the bottom edge where the keys are written.
seaBackdrop :: Era -> Picture
seaBackdrop era = Pictures $
  [ Color deep (rectC canvasW canvasH) ] ++
  [ Translate px py (crest i)
  | i <- [0 .. 119 :: Int]
  , let px = canvasW * rnd i 3 1 - canvasW / 2
        py = canvasH / 2 - canvasH * rnd i 4 2
  , px < left - 30 || px > right + 30 || py > top + 8 || (py < bottom - 8 && py > keysTop) ]
  where
    (left, bottom) = onBoard 1 (mapLeft - 12, mapBottom - 17)
    (right, top) = onBoard 1 (mapRight + 12, mapTop + 12)
    keysTop = negate canvasH / 2 + 38
    deep = case era of
      Past -> hex 0x114552
      Present -> hex 0x0A1418
    crestC = case era of
      Past -> hexA 0xA8E0D8 0.32
      Present -> hexA 0x7D93A0 0.28
    -- The present's sea runs higher and longer.
    crest i =
      let (w, lift) = case era of
            Past -> (14 + 26 * rnd i 5 3, 2)
            Present -> (18 + 36 * rnd i 5 3, 2 + 5 * rnd i 6 4)
      in thin crestC [(-w, 0), (-w / 3, lift), (w / 3, -lift / 2), (w, 0)]

-- | The sea moving, at the canvas's scale, under the board. In the past the lamp is
-- lit: the real lighthouse's beam sweeps by now and then, and the sun glints off
-- the water to the east. In the present there is no beam, only rain.
seaLife :: Era -> Float -> Picture
seaLife Past clock = Pictures [beam, glints]
  where
    -- From the real lighthouse, off the bottom left, turning once every ten
    -- seconds: in sight for about two of them.
    (ox, oy) = (-900, -580)
    turn = 2 * pi * clock / 600
    beam = Pictures [ wedge 7.5 0.09, wedge 3.4 0.2 ]
    wedge halfDeg alpha = Pictures
      [ Color (hexA 0xFFEFC4 (alpha * (1 - fromIntegral i / 8)))
          (Polygon [ around r0 (turn - h), around r1 (turn - h), around r1 (turn + h), around r0 (turn + h) ])
      | i <- [0 .. 7 :: Int]
      , let r0 = 2400 * fromIntegral i / 8
            r1 = 2400 * fromIntegral (i + 1) / 8 ]
      where h = halfDeg * pi / 180
    around r t = (ox + r * cos t, oy + r * sin t)
    glints = Pictures
      [ Color (hexA 0xF4FBF6 (base * (0.35 + 0.65 * (0.5 + 0.5 * sin (clock * 0.06 + 6 * rnd i 9 5)))))
          (rect x y (2 + 5 * rnd i 8 5) 1.6)
      | i <- [0 .. 59 :: Int]
      , let x = canvasW / 2 * (rnd i 7 5 ** 0.6)
            y = canvasH * rnd i 6 5 - canvasH / 2
            base = 0.25 + 0.45 * rnd i 5 5 ]
seaLife Present clock = Color (hexA 0xB7CEDA 0.2) (Pictures
  [ Translate x y (Rotate 22 (rectC 1.2 (12 + 8 * rnd i 5 8)))
  | i <- [0 .. 119 :: Int]
  , let speed = 6 + 4 * rnd i 2 8
        x = wrapTo 1320 (1320 * rnd i 1 8 - clock * speed * 0.4) - 660
        y = wrapTo 840 (840 * rnd i 4 8 - clock * speed) - 420 ])
  where wrapTo size v = v - size * fromIntegral (floor (v / size) :: Int)

-- | The canvas's edges, darkened, so the eye rests in the middle.
seaShade :: Picture
seaShade = Color (hexA 0x000000 0.055) (Pictures [ edges d | d <- [24, 48 .. 144] ])
  where
    edges d = Pictures
      [ rect (-canvasW / 2) (-canvasH / 2) d canvasH
      , rect (canvasW / 2 - d) (-canvasH / 2) d canvasH
      , rect (-canvasW / 2) (-canvasH / 2) canvasW d
      , rect (-canvasW / 2) (canvasH / 2 - d) canvasW d
      ]

-- | Around the stone apron, on the board's own scale so it moves with the board:
-- the shelf the station stands on, a few rocks, and surf where the sea meets the
-- stone. Calm in the past; rougher and greyer in the present.
shore :: Era -> Picture
shore era = Pictures [ shelf, rocks, surfLayer era False ]
  where
    (x0, y0, x1, y1) = apron
    (tint, a) = case era of
      Past -> (hex 0x2A8088, 0.07)
      Present -> (hex 0x1E3440, 0.06)
    shelf = Color (fade a tint) (Pictures
      [ roundRect (x0 - e) (y0 - e) (x1 + e) (y1 + e) (e * 0.9) | i <- [1 .. 7 :: Int], let e = 13 * fromIntegral i ])
    (body, cap, foam, foamA) = case era of
      Past -> (hex 0x6B5642, hex 0x9A7F60, hex 0xF3EBD8, 0.4)
      Present -> (hex 0x1A2226, hex 0x3A464D, hex 0xC9D4D8, 0.35)
    rocks = Pictures [ rock (x1 + 52) 62 1, rock (x1 + 31) 34 0.65 ]
    rock x y s = Translate x y (Pictures
      [ Color (fade foamA foam) (Pictures
          [ plainSeg 1.6 (ring' t) (ring' (t + 0.32)) | j <- [0 .. 11 :: Int], let t = fromIntegral j * pi / 6 ])
      , Color body (Polygon [ (px * s, py * s) | (px, py) <- [(-14,-4),(-10,6),(-2,11),(8,9),(15,2),(13,-7),(2,-10),(-9,-9)] ])
      , Color cap (Polygon [ (px * s, py * s) | (px, py) <- [(-10,6),(-2,11),(8,9),(4,4),(-6,2)] ])
      ])
      where ring' t = (19 * s * cos t, -2 * s + 13 * s * sin t)

-- | The outer line of surf, drawn apart so it can breathe in and out.
farSurf :: Era -> Picture
farSurf era = surfLayer era True

-- | The stone apron's outline, on the board.
apron :: (Float, Float, Float, Float)
apron = (mapLeft - 12, mapBottom - 15, mapRight + 12, mapTop + 12)

-- | Short curling strokes and flecks along each side of the apron: never a line.
surfLayer :: Era -> Bool -> Picture
surfLayer era far = Color colour (Pictures (concat (zipWith along [0 ..] sides)))
  where
    (x0, y0, x1, y1) = apron
    (colour, strength, rough) = case era of
      Past -> (hex 0xF3EBD8, if far then 0.28 else 0.6, 1)
      Present -> (hex 0xC9D4D8, if far then 0.24 else 0.5, 1.6)
    -- Each side, walked with the sea on its outer hand: start, end, outward normal.
    sides = [ ((x0, y0), (x0, y1), (-1, 0)), ((x0, y1), (x1, y1), (0, 1))
            , ((x1, y1), (x1, y0), (1, 0)), ((x1, y0), (x0, y0), (0, -1)) ]
    layer = if far then 1 else 0 :: Int
    along :: Int -> ((Float, Float), (Float, Float), (Float, Float)) -> [Picture]
    along side ((ax, ay), (bx, by), (nx, ny)) = go 0 (12 * r 0 0)
      where
        len = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
        (ux, uy) = ((bx - ax) / len, (by - ay) / len)
        topSide = ny > 0
        r i k = rnd (side * 977 + i) (layer * 31 + k) 7
        go :: Int -> Float -> [Picture]
        go i t
          | t >= len = []
          | otherwise = stroke : fleck ++ go (i + 1) (t + seg' + gap)
          where
            seg' = (if far then 6 else 9) + r i 1 * (if far then 14 else 22) * rough
            gap = (if far then 14 else 4) + r i 2 * (if far then 26 else 10)
            off = (if far then 13 else 3) + r i 3 * (if far then 10 else 6) * rough
            alpha = strength * (0.45 + 0.75 * r i 4) * (if topSide then 0.55 else 1)
            width = if far then 1.1 else 1.4 + 1.2 * r i 5
            bulge = (1.5 + 2.5 * r i 6) * (if r i 7 < 0.5 then 1 else -1)
            (sx, sy) = (ax + ux * t + nx * off, ay + uy * t + ny * off)
            (ex, ey) = (sx + ux * seg', sy + uy * seg')
            (mx, my) = ((sx + ex) / 2 + nx * bulge, (sy + ey) / 2 + ny * bulge)
            curvePts = [ q s | s <- [0, 0.25, 0.5, 0.75, 1] ]
            q s = ( (1 - s) * (1 - s) * sx + 2 * (1 - s) * s * mx + s * s * ex
                  , (1 - s) * (1 - s) * sy + 2 * (1 - s) * s * my + s * s * ey )
            stroke = Color (fade (min 1 alpha) colour) (Pictures (zipWith (plainSeg width) curvePts (drop 1 curvePts)))
            fleck
              | far || r i 8 >= 0.35 = []
              | otherwise =
                  let d = 3 + 5 * r i 9
                  in [ Color (fade (min 1 (alpha * 0.8)) colour)
                         (Translate (ex + nx * d) (ey + ny * d) (ngon 6 (0.8 + 0.9 * r i 10))) ]

-- | A rectangle with rounded corners, as one convex polygon.
roundRect :: Float -> Float -> Float -> Float -> Float -> Picture
roundRect x0 y0 x1 y1 rad = Polygon
  (corner (x1 - rad) (y1 - rad) 0 ++ corner (x0 + rad) (y1 - rad) 90
   ++ corner (x0 + rad) (y0 + rad) 180 ++ corner (x1 - rad) (y0 + rad) 270)
  where
    corner cx cy a0 = [ (cx + rad * cos t, cy + rad * sin t)
                      | i <- [0 .. 6 :: Int], let t = (a0 + 15 * fromIntegral i) * pi / 180 ]

-- | The stone apron the station stands on. Dressed blocks, then eroded ones.
foundation :: Era -> Picture
foundation era = Pictures $
  [ Color (hexA 0x000000 0.4) (rect (mapLeft - 12) (mapBottom - 17) (mapRight - mapLeft + 24) (mapTop - mapBottom + 24))
  , Color stone (rect (mapLeft - 12) (mapBottom - 12) (mapRight - mapLeft + 24) (mapTop - mapBottom + 24))
  ] ++ blocks ++ erosion
  where
    stone = case era of
      Past -> hex 0x8C7253
      Present -> hex 0x3A464D
    joint = case era of
      Past -> hex 0x5C4634
      Present -> hex 0x1E272C
    blocks = [ thin joint [(x, mapTop + 12), (x, mapTop)] | x <- [mapLeft, mapLeft + 38 .. mapRight] ]
          ++ [ thin joint [(x, mapBottom - 12), (x, mapBottom)] | x <- [mapLeft + 19, mapLeft + 57 .. mapRight] ]
          ++ [ thin joint [(mapLeft - 12, y), (mapLeft, y)] | y <- [mapBottom, mapBottom + 38 .. mapTop] ]
    erosion = case era of
      Past -> []
      Present ->
        [ Color (hex 0x0A1418) (Translate (mapLeft - 12 + (mapRight - mapLeft + 24) * rnd i 1 9) (mapBottom - 12)
            (Polygon [(-10 - 14 * rnd i 3 9, -1), (10 + 14 * rnd i 4 9, -1), (2, 4 + 6 * rnd i 2 9)]))
        | i <- [0 .. 16 :: Int] ] ++
        [ Color (hex 0x0A1418) (Translate (mapLeft - 12 + (mapRight - mapLeft + 24) * rnd i 5 9) (mapTop + 12)
            (Polygon [(-8 - 12 * rnd i 7 9, 1), (8 + 12 * rnd i 8 9, 1), (2, -3 - 5 * rnd i 6 9)]))
        | i <- [0 .. 12 :: Int] ]

-- | Wall lamps, as cells whose south face carries one.
sconceCells :: [Cell]
sconceCells = [(1, 0), (6, 0), (11, 0)]

-- | A soft oval built from many faint layers, so no edge shows.
pool :: Int -> Float -> Float -> Float -> Picture
pool c strength rx ry = Pictures
  [ Color (hexA c (strength / fromIntegral layers)) (ellipse 28 (rx * f) (ry * f))
  | i <- [1 .. layers], let f = fromIntegral i / fromIntegral layers ]
  where layers = 9 :: Int

-- | Light falling on the floor at a strength from 0 to 1. Warm lamps in service;
-- in the present, only thin daylight at the breach.
lightPools :: Level -> Era -> Float -> Picture
lightPools _ _ k | k <= 0.01 = Blank
lightPools level Past k | not (lvDressed level) =
  at (cellPos (lvPowerCell level)) (pool 0xFFC879 (0.17 * k) 165 125)
lightPools level Present k | not (lvDressed level) =
  at (cellPos (lvGoal level)) (pool 0x9CC7D8 (0.14 * k) 120 90)
lightPools _ Past k = Pictures $
  [ at (sconcePos c) (pool 0xFFC879 (0.17 * k) 165 125) | c <- sconceCells ] ++
  [ at (cellPos (4, 2)) (pool 0xFFC879 (0.13 * k) 70 56) ] ++
  [ at (sconcePos c) (Color (hexA 0xFFE9B0 (0.95 * k)) (rect (-4) (-3) 8 8)) | c <- sconceCells ]
  where
    sconcePos (x, y) = let (px, py) = cellPos (x, y) in (px, py - half + 12)
lightPools _ Present k = Pictures
  [ at (cellPos (8, 6)) (pool 0x9CC7D8 (0.2 * k) 90 62)
  , at (cellPos (2, 5)) (rot 14 (pool 0x8FB2C2 (0.08 * k) 70 42))
  , at (cellPos (6, 3)) (rot 14 (pool 0x8FB2C2 (0.07 * k) 56 36))
  , at (cellPos (10, 5)) (rot 14 (pool 0x8FB2C2 (0.07 * k) 62 40))
  ]

-- | Darkened map edges, to hold the eye on the player.
vignette :: Era -> Picture
vignette era = Color shade (Pictures (concat
  [ [ rect mapLeft (mapTop - d) w d
    , rect mapLeft mapBottom w d
    , rect mapLeft mapBottom d h
    , rect (mapRight - d) mapBottom d h ]
  | d <- [10, 20, 30, 40, 50] ]))
  where
    w = mapRight - mapLeft
    h = mapTop - mapBottom
    shade = case era of
      Past -> hexA 0x1A0B04 0.022
      Present -> hexA 0x020608 0.04
