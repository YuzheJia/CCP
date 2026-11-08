-- | Small drawing vocabulary shared by all the art. Colours, shapes, layout, noise.
-- Rendering modules build pictures with these helpers; this module knows neither
-- the game rules nor the window size. Render fits the completed 1280 by 800 canvas
-- to the window. Board cells count right and down; Gloss canvas units count right
-- and up from the centre. Distances here are canvas units unless stated otherwise.
-- Angles for sin/cos are radians; rot and Gloss Rotate take clockwise degrees.
-- Pictures are pure trees: constructing one does not draw or perform IO.
module Draw
  ( -- * Layout (a 1280 x 800 canvas centred on the origin)
    canvasW, canvasH, cs, half
  , mapLeft, mapTop, mapRight, mapBottom
  , board, onBoard
  , cellPos, cellVec, at
    -- * Colour
  , hex, hexA, fade, mix
    -- * Shapes
  , rect, rectC, quad, tri, ngon, ellipse, ring, seg, plainSeg, segs, thin, thins
  , rot, glow
    -- * Baking
  , bake
  , fadePicture
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

-- | Logical canvas width and height, independent of the window's pixel dimensions.
canvasW, canvasH :: Float
canvasW = 1280
canvasH = 800

-- | One map cell, in canvas units.
cs, half :: Float
cs = 76
half = cs / 2

-- | Edges of the 13 by 9 cell viewport before the board's scale and translation.
-- Larger levels move behind this viewport; these are not their map dimensions.
mapLeft, mapTop, mapRight, mapBottom :: Float
mapLeft = -612
mapTop = 342
mapRight = mapLeft + 13 * cs
mapBottom = mapTop - 9 * cs

-- | The board: the map and everything pinned to it, drawn in the coordinates above
-- and then set down on the canvas at a scale, so nothing on screen ever covers a
-- cell. It has two stands. While a level is introduced it stands smaller and to
-- the right, leaving a column for the watch and the keys; once the keeper acts it
-- grows and slides to the centre, with the sea around it. @f@ runs from 0, the
-- introduction, to 1, play, already eased.
data Stand = Stand
  { stK :: !Float  -- ^ scale
  , stX :: !Float  -- ^ where the map's centre lands, across
  , stY :: !Float  -- ^ and up
  }

-- | The two board placements. The introduction leaves 24 canvas units at the
-- right edge, including the 12-unit frame; play centres the map horizontally.
-- The strict fields in Stand force their Float values when a placement is built.
introStand, playStand :: Stand
introStand = Stand k (canvasW / 2 - 24 - (mapRight + 12 - mapCentreX) * k) 4
  -- Scale the map and its frame together, leaving the left column for the HUD.
  where k = 0.86
playStand = Stand 0.96 0 12

-- | Horizontal centre of the untransformed viewport, in canvas units.
mapCentreX :: Float
mapCentreX = (mapLeft + mapRight) / 2

-- | Interpolate placements with an already eased fraction (normally 0 to 1).
-- No clamp is applied here; callers own the timing and easing of the transition.
standAt :: Float -> Stand
standAt f = Stand (lerp f (stK introStand) (stK playStand))
                  (lerp f (stX introStand) (stX playStand))
                  (lerp f (stY introStand) (stY playStand))

-- | Place board art for a focus fraction. Subtracting its scaled centre makes
-- stX refer to the centre, rather than to the board's original coordinate origin.
-- Composition (.) applies Scale first, then Translate, to the supplied picture.
-- The local let exposes a Stand only within this expression; where bindings below
-- other definitions instead share the enclosing function's arguments.
board :: Float -> Picture -> Picture
board f = let Stand k x y = standAt f in Translate (x - mapCentreX * k) y . Scale k k

-- | Where a point of the board lands on the canvas.
onBoard :: Float -> (Float, Float) -> (Float, Float)
onBoard f (px, py) = let Stand k x y = standAt f in (px * k + x - mapCentreX * k, py * k + y)

-- | Centre of a cell, with fractional cells allowed for walking between them.
cellVec :: Float -> Float -> (Float, Float)
cellVec x y = (mapLeft + (x + 0.5) * cs, mapTop - (y + 0.5) * cs)

-- | Integer map cell to its centre in board coordinates; y increases downwards.
-- This does not apply a room/camera offset or the board placement.
cellPos :: (Int, Int) -> (Float, Float)
cellPos (x, y) = cellVec (fromIntegral x) (fromIntegral y)

-- | Move a local picture to a canvas or board point, without changing its scale.
at :: (Float, Float) -> Picture -> Picture
at (x, y) = Translate x y

-- | Opaque colour from a 0xRRGGBB integer, with eight bits per channel.
hex :: Int -> Color
hex v = hexA v 1

-- | Decode 0xRRGGBB and supply alpha separately (normally 0 to 1).
-- Division selects the channel and mod 256 drops higher channels; /255 converts
-- each integer intensity to Gloss's normalised colour components.
hexA :: Int -> Float -> Color
hexA v = makeColor (chan 16) (chan 8) (chan 0)
  where chan s = fromIntegral ((v `div` (2 ^ (s :: Int))) `mod` 256) / 255

-- | The same colour at this much of its own opacity.
fade :: Float -> Color -> Color
fade k c = let (r, g, b, a) = rgbaOfColor c in makeColor r g b (a * k)

-- | Interpolate all four RGBA components: 0 gives the first colour, 1 the second.
-- Fractions outside that range extrapolate; callers clamp where required.
mix :: Float -> Color -> Color -> Color
mix k a b =
  let (r1, g1, b1, a1) = rgbaOfColor a
      (r2, g2, b2, a2) = rgbaOfColor b
      f x y = x + (y - x) * k
  in makeColor (f r1 r2) (f g1 g2) (f b1 b2) (f a1 a2)

-- | Corners, from lower left.
rect :: Float -> Float -> Float -> Float -> Picture
rect x y w h = Polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

-- | Filled rectangle of the given width and height, centred on the local origin.
rectC :: Float -> Float -> Picture
rectC w h = rect (-w / 2) (-h / 2) w h

-- | Filled four-corner polygon, in perimeter order; no convexity check is made.
quad :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Picture
quad a b c d = Polygon [a, b, c, d]

-- | Filled triangle through three points in the current coordinate system.
tri :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Picture
tri a b c = Polygon [a, b, c]

-- | A regular polygon. One draw call, unlike Gloss's circle.
ngon :: Int -> Float -> Picture
ngon n r = ellipse n r r

-- | Approximate an ellipse with n vertices and horizontal/vertical radii.
-- The comprehension samples a full turn in radians; the polygon closes itself.
-- Callers supply at least three vertices and non-negative radii.
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
  -- Each quad covers one angular interval, from inner radius r to outer r+w.
  -- Multiplying y by sy flattens a circular band into a perspective ellipse.
  where p rad t = (rad * cos t, rad * sy * sin t)

-- | A stroke of width @w@, square-ended and slightly overlong so corners close.
seg :: Float -> (Float, Float) -> (Float, Float) -> Picture
seg w (x1, y1) (x2, y2)
  -- A near-point still leaves a square mark, without dividing by almost zero.
  | len < 0.0001 = at (x1, y1) (rectC w w)
  | otherwise = Polygon
      [ (ax + nx, ay + ny), (bx + nx, by + ny), (bx - nx, by - ny), (ax - nx, ay - ny) ]
  where
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    -- A unit tangent times half the width extends the ends. Turning it through
    -- 90 degrees gives the perpendicular used for the two sides of the stroke.
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
  -- Unlike seg, an exact-ended stroke has no visible length at a point.
  | len < 0.0001 = Blank
  | otherwise = Polygon [(x1 + nx, y1 + ny), (x2 + nx, y2 + ny), (x2 - nx, y2 - ny), (x1 - nx, y1 - ny)]
  where
    dx = x2 - x1
    dy = y2 - y1
    len = sqrt (dx * dx + dy * dy)
    -- Normalise the perpendicular and give it half the requested width.
    nx = -dy / len * w / 2
    ny = dx / len * w / 2

-- | Join consecutive points with thick strokes. Empty/singleton paths draw
-- nothing; callers repeat the first point if they want to close the path.
segs :: Float -> [(Float, Float)] -> Picture
segs w ps = Pictures (zipWith (seg w) ps (drop 1 ps))

-- | One-pixel line in a colour.
thin :: Color -> [(Float, Float)] -> Picture
thin c = Color c . Line

-- | Several one-pixel polylines sharing one colour node; empty paths are harmless.
thins :: Color -> [[(Float, Float)]] -> Picture
thins c = Color c . Pictures . map Line

-- | Rotate a picture clockwise about its local origin, by degrees.
rot :: Float -> Picture -> Picture
rot = Rotate

-- | Layered translucent discs: an artistic stand-in for a real glow.
glow :: Color -> Float -> Int -> Picture
-- Each disc has the same small alpha; smaller discs overlap in the middle, so
-- the centre is brighter. Non-positive layer counts produce an empty picture.
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
    -- Carry the accumulated affine transform and innermost explicit colour down
    -- the tree. Traversal preserves paint order, which matters for transparency.
    flatten m c pic = case pic of
      -- Blank contributes no primitive; Pictures concatenates children in order.
      Blank -> []
      Pictures ps -> concatMap (flatten m c) ps
      -- Inner colours replace outer colours in Gloss; they are not multiplied.
      Color c' p -> flatten m (Just c') p
      Translate x y p -> flatten (mul m (1, 0, 0, 1, x, y)) c p
      Rotate deg p ->
        let t = deg * pi / 180 -- Gloss rotates clockwise
        in flatten (mul m (cos t, negate (sin t), sin t, cos t, 0, 0)) c p
      Scale sx sy p -> flatten (mul m (sx, 0, 0, sy, 0, 0)) c p
      Polygon ps -> [(c, Polygon (map (apply m) ps))]
      Line ps -> [(c, Line (map (apply m) ps))]
      -- Unsupported primitives pass through without the accumulated transform.
      -- Bake is therefore only suitable for the polygon/line vocabulary above.
      other -> [(c, other)]
    -- (a b e / c d f) applied to a column vector.
    mul (a, b, c, d, e, f) (a', b', c', d', e', f') =
      ( a * a' + b * c', a * b' + b * d'
      , c * a' + d * c', c * b' + d * d'
      , a * e' + b * f' + e, c * e' + d * f' + f )
    apply (a, b, c, d, e, f) (x, y) = (a * x + b * y + e, c * x + d * y + f)
    -- Merge only adjacent equal colours. Sorting all primitives by colour would
    -- change overlaps and alpha blending, and would not preserve the image.
    regroup [] = []
    regroup ((c, p) : rest) =
      let (same, others) = span ((== c) . fst) rest
          body = Pictures (p : map snd same)
      in maybe body (`Color` body) c : regroup others

-- | A picture at this much of its own opacity: every colour in it is faded, so a
-- baked picture can come and go without being drawn again.
fadePicture :: Float -> Picture -> Picture
fadePicture k pic
  -- These endpoint tolerances avoid rebuilding a tree for an imperceptible fade.
  | k >= 0.999 = pic
  | k <= 0.001 = Blank
  | otherwise = go pic
  where
    go p = case p of
      Color c q -> Color (fade k c) (go q)
      Pictures ps -> Pictures (map go ps)
      Translate x y q -> Translate x y (go q)
      Rotate a q -> Rotate a (go q)
      Scale x y q -> Scale x y (go q)
      other -> other

-- | Repeatable noise in [0,1) from a position and a salt.
rnd :: Int -> Int -> Int -> Float
rnd x y salt = fromIntegral (scramble (scramble (scramble (x * 374761 + 97) + y * 668265) + salt * 2654435) `mod` 100000) / 100000
  where
    -- Integer mixing gives stable scenery without an IO random generator. The
    -- moduli bound intermediates to 32 bits on the game's 64-bit target; this is
    -- decorative noise, not a source for probabilities or cryptography.
    scramble v =
      let a = (v * 1597334677) `mod` 4294967296
          b = a `xor` (a `shiftR` 15)
          c = (b * 2246822519) `mod` 4294967296
      in c `xor` (c `shiftR` 13)

-- | Limit an ordinary finite fraction to the closed interval 0 to 1.
clamp01 :: Float -> Float
clamp01 = max 0 . min 1

-- | Clamped cubic smoothstep. Its slope is zero at both ends, so a transition
-- joins a still pose without a sudden change in velocity.
smooth :: Float -> Float
smooth t = let u = clamp01 t in u * u * (3 - 2 * u)

-- | Linear interpolation: fraction, start, end. This deliberately does not clamp.
lerp :: Float -> Float -> Float -> Float
lerp k a b = a + (b - a) * k
