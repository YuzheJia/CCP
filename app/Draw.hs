-- | Small drawing vocabulary shared by all the art. Colours, shapes, layout, noise.
module Draw
  ( -- * Layout (a 1280 x 800 canvas centred on the origin)
    canvasW, canvasH, cs, half
  , mapLeft, mapTop, mapRight, mapBottom, panelX, panelW
  , cellPos, cellVec, at
    -- * Colour
  , hex, hexA, fade, mix
    -- * Shapes
  , rect, rectC, quad, tri, ngon, ellipse, ring, seg, plainSeg, segs, thin, thins
  , rot, glow
    -- * Baking
  , bake
    -- * Noise and easing
  , rnd, clamp01, smooth, lerp
  ) where

import Data.Bits (shiftR, xor)
import Graphics.Gloss
    ( makeColor,
      rgbaOfColor,
      Color,
      Picture(Pictures, Blank, Translate, Rotate, Scale, Polygon, Line,
              Color) )

canvasW, canvasH :: Float
canvasW = 1280
canvasH = 800

-- | One map cell, in canvas units.
cs, half :: Float
cs = 76
half = cs / 2

mapLeft, mapTop, mapRight, mapBottom :: Float
mapLeft = -612
mapTop = 342
mapRight = mapLeft + 13 * cs
mapBottom = mapTop - 9 * cs

panelX, panelW :: Float
panelX = 510
panelW = 228

-- | Centre of a cell, with fractional cells allowed for walking between them.
cellVec :: Float -> Float -> (Float, Float)
cellVec x y = (mapLeft + (x + 0.5) * cs, mapTop - (y + 0.5) * cs)

cellPos :: (Int, Int) -> (Float, Float)
cellPos (x, y) = cellVec (fromIntegral x) (fromIntegral y)

at :: (Float, Float) -> Picture -> Picture
at (x, y) = Translate x y

hex :: Int -> Color
hex v = hexA v 1

hexA :: Int -> Float -> Color
hexA v = makeColor (chan 16) (chan 8) (chan 0)
  where chan s = fromIntegral ((v `div` (2 ^ (s :: Int))) `mod` 256) / 255

-- | The same colour at this much of its own opacity.
fade :: Float -> Color -> Color
fade k c = let (r, g, b, a) = rgbaOfColor c in makeColor r g b (a * k)

mix :: Float -> Color -> Color -> Color
mix k a b =
  let (r1, g1, b1, a1) = rgbaOfColor a
      (r2, g2, b2, a2) = rgbaOfColor b
      f x y = x + (y - x) * k
  in makeColor (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)

-- | Corners, from lower left.
rect :: Float -> Float -> Float -> Float -> Picture
rect x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

rectC :: Float -> Float -> Picture
rectC w h = rect (-w / 2) (-h / 2) w h

quad :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Picture
quad a b c d = Polygon [a, b, c, d]

tri :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Picture
tri a b c = Polygon [a, b, c]

-- | A regular polygon. One draw call, unlike Gloss's circle.
ngon :: Int -> Float -> Picture
ngon n r = ellipse n r r

ellipse :: Int -> Float -> Float -> Picture
ellipse n rx ry = Polygon
  [ (rx * cos a, ry * sin a)
  | i <- [0 .. n - 1], let a = 2 * pi * fromIntegral i / fromIntegral n ]

-- | A band between two radii, squashed vertically by @sy@.
ring :: Int -> Float -> Float -> Float -> Picture
ring n r w sy = Pictures
  [ quad (p r a) (p (r + w) a) (p (r + w) b) (p r b)
  | i <- [0 .. n - 1]
  , let a = 2 * pi * fromIntegral i / fromIntegral n
        b = 2 * pi * fromIntegral (i + 1) / fromIntegral n ]
  where p rad t = (rad * cos t, rad * sy * sin t)

-- | A stroke of width @w@, square-ended and slightly overlong so corners close.
seg :: Float -> (Float, Float) -> (Float, Float) -> Picture
seg w (x1, y1) (x2, y2)
  | len < 0.0001 = at (x1, y1) (rectC w w)
  | otherwise = Polygon
      [ (ax + nx, ay + ny), (bx + nx, by + ny), (bx - nx, by - ny), (ax - nx, ay - ny) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    ux = dx / len * w / 2
    uy = dy / len * w / 2
    nx = -uy
    ny = ux
    ax = x1 - ux
    ay = y1 - uy
    bx = x2 + ux
    by = y2 + uy

-- | A stroke that stops exactly at its ends.
plainSeg :: Float -> (Float, Float) -> (Float, Float) -> Picture
plainSeg w (x1, y1) (x2, y2)
  | len < 0.0001 = Blank
  | otherwise = Polygon [(x1 + nx, y1 + ny), (x2 + nx, y2 + ny), (x2 - nx, y2 - ny), (x1 - nx, y1 - ny)]
  where
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    nx = -dy / len * w / 2
    ny = dx / len * w / 2

segs :: Float -> [(Float, Float)] -> Picture
segs w ps = Pictures (zipWith (seg w) ps (drop 1 ps))

-- | One-pixel line in a colour.
thin :: Color -> [(Float, Float)] -> Picture
thin c = Color c . Line

thins :: Color -> [[(Float, Float)]] -> Picture
thins c = Color c . Pictures . map Line

rot :: Float -> Picture -> Picture
rot = Rotate

-- | Layered translucent discs: an artistic stand-in for a real glow.
glow :: Color -> Float -> Int -> Picture
glow c radius layers = Pictures
  [ Color (fade (0.6 / fromIntegral layers) c) (ngon 28 (radius * fromIntegral i / fromIntegral layers))
  | i <- [layers, layers - 1 .. 1] ]

-- | Flatten a static picture: transforms are multiplied into its coordinates and
-- runs of one colour share a single Color node. It draws exactly the same image,
-- but Gloss spends a matrix push on every transform and a state query on every
-- colour, and those dominate the frame for art made of thousands of small shapes.
--
-- Only handles what this game draws: polygons, lines and the structural nodes.
bake :: Picture -> Picture
bake = Pictures . regroup . flatten (1, 0, 0, 1, 0, 0) Nothing
  where
    flatten m c pic = case pic of
      Blank -> []
      Pictures ps -> concatMap (flatten m c) ps
      Color c' p -> flatten m (Just c') p
      Translate x y p -> flatten (mul m (1, 0, 0, 1, x, y)) c p
      Rotate deg p ->
        let t = deg * pi / 180 -- Gloss rotates clockwise
        in flatten (mul m (cos t, negate (sin t), sin t, cos t, 0, 0)) c p
      Scale sx sy p -> flatten (mul m (sx, 0, 0, sy, 0, 0)) c p
      Polygon ps -> [(c, Polygon (map (apply m) ps))]
      Line ps -> [(c, Line (map (apply m) ps))]
      other -> [(c, other)]
    -- (a b e / c d f) applied to a column vector.
    mul (a, b, c, d, e, f) (a', b', c', d', e', f') =
      ( a * a' + b * c', a * b' + b * d'
      , c * a' + d * c', c * b' + d * d'
      , a * e' + b * f' + e, c * e' + d * f' + f )
    apply (a, b, c, d, e, f) (x, y) = (a * x + b * y + e, c * x + d * y + f)
    regroup [] = []
    regroup ((c, p) : rest) =
      let (same, others) = span ((== c) . fst) rest
          body = Pictures (p : map snd same)
      in maybe body (`Color` body) c : regroup others

-- | Repeatable noise in [0,1) from a position and a salt.
rnd :: Int -> Int -> Int -> Float
rnd x y salt = fromIntegral (scramble (scramble (scramble (x * 374761 + 97) + y * 668265) + salt * 2654435) `mod` 100000) / 100000
  where
    scramble v =
      let a = (v * 1597334677) `mod` 4294967296
          b = a `xor` (a `shiftR` 15)
          c = (b * 2246822519) `mod` 4294967296
      in c `xor` (c `shiftR` 13)

clamp01 :: Float -> Float
clamp01 = max 0 . min 1

smooth :: Float -> Float
smooth t = let u = clamp01 t in u * u * (3 - 2 * u)

lerp :: Float -> Float -> Float -> Float
lerp k a b = a + (b - a) * k
