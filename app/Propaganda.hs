-- | The glorious past's poster vocabulary: Chinese lettering, stars, the hammer and
-- sickle, the flag, lanterns and sunbursts. All drawn from shapes, so they bake like
-- the rest of the art and take any colour.
module Propaganda
  ( -- * Chinese lettering
    hanzi, hanziC, hanziW
    -- * Emblems
  , star, hammerSickle, flag, lantern, sunburst
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Draw
import Graphics.Gloss
import Hanzi

-- Lettering --------------------------------------------------------------------

-- | Each character's ink as horizontal runs on a unit grid, y up from the bottom:
-- (left, bottom, width). Worked out once.
runs :: Map.Map Char [(Float, Float, Float)]
runs = Map.fromList
  [ (c, rowRuns rows)
  | c <- "光属于人民举报违规黑暗灯塔万岁反对超额发荣的过去现在招租五分钟计划电池指示操作借来完成再教育"
  , Just rows <- [hanziBitmap c] ]
  where
    rowRuns rows = concat
      [ [ (fromIntegral c0, fromIntegral (hanziGrid - 1 - r), fromIntegral (c1 - c0)) | (c0, c1) <- spans 0 row ]
      | (r, row) <- zip [0 :: Int ..] rows ]
    spans :: Int -> String -> [(Int, Int)]
    spans _ [] = []
    spans i s@(ch : _)
      | ch == '#' = let (ink, rest) = span (== '#') s; n = length ink in (i, i + n) : spans (i + n) rest
      | otherwise = let (gap, rest) = span (/= '#') s in spans (i + length gap) rest

-- | Gap between characters, as a share of their size.
advance :: Float -> Float
advance size = size * 1.08

-- | A line of characters, each @size@ square, from the origin rightward with its
-- bottom on y = 0. A space leaves a character's room empty, which is how a slogan
-- loses letters in the present.
hanzi :: Float -> String -> Picture
hanzi size str = Pictures
  [ Translate (advance size * fromIntegral i) 0 (glyph size c) | (i, c) <- zip [0 :: Int ..] str ]

glyph :: Float -> Char -> Picture
glyph size c = Pictures
  [ rect (x * px) (y * px) (w * px + 0.25) (px + 0.25) | (x, y, w) <- fromMaybe [] (Map.lookup c runs) ]
  where px = size / fromIntegral hanziGrid

hanziW :: Float -> String -> Float
hanziW size str = case length str of
  0 -> 0
  n -> advance size * fromIntegral (n - 1) + size

-- | Centred on the origin, both ways.
hanziC :: Float -> String -> Picture
hanziC size str = Translate (negate (hanziW size str) / 2) (negate size / 2) (hanzi size str)

-- Emblems ----------------------------------------------------------------------

-- | A five-pointed star of radius @r@, one point up. Built from convex pieces,
-- because Gloss fills a polygon as if it were convex.
star :: Float -> Picture
star r = Pictures (Polygon inner : [ tri (outer i) (innerAt (i - 1)) (innerAt i) | i <- [0 .. 4] ])
  where
    ang k = pi / 2 + 2 * pi * k / 5
    outer i = let a = ang (fromIntegral (i :: Int)) in (r * cos a, r * sin a)
    -- The notch between point i and point i + 1.
    innerAt i = let a = ang (fromIntegral (i :: Int) + 0.5) in (0.382 * r * cos a, 0.382 * r * sin a)
    inner = [ innerAt i | i <- [0 .. 4] ]

-- | The hammer and sickle, @size@ across, centred. One colour: the caller's.
hammerSickle :: Float -> Picture
hammerSickle size = Scale k k (Pictures (blade ++ [grip, pommel, handle, head']))
  where
    k = size / 100
    -- Drawn on a 100-unit square with y down, then flipped into Gloss's y up.
    p (x, y) = (x - 50, 50 - y)
    onCircle (cx, cy) r deg = let a = deg * pi / 180 in (cx + r * cos a, cy + r * sin a)
    -- The blade: the band between two offset circles, from its tip at the top round
    -- the right and down to the grip, cut into quads so each piece is convex.
    steps = 24 :: Int
    edge i =
      let t = fromIntegral i / fromIntegral steps
      in ( p (onCircle (50, 50) 44 (-80 + 200 * t))
         , p (onCircle (47, 46) 35 (-70 + 190 * t)) )
    blade = [ quad o0 o1 i1 i0 | i <- [0 .. steps - 1], let (o0, i0) = edge i; (o1, i1) = edge (i + 1) ]
    grip = Polygon (map p [(28.8, 87.5), (13.8, 99.5), (8.2, 92.5), (23.2, 80.5)])
    pommel = Translate (-39) (-46) (ngon 12 4.5)
    handle = Polygon (map p [(41.5, 39.5), (89.5, 87.5), (82.5, 94.5), (34.5, 46.5)])
    head' = Polygon (map p [(52.8, 29.6), (41.5, 18.3), (13.2, 46.5), (24.6, 57.8)])

-- | The five-star red flag, @w@ wide, its top left corner at the origin.
flag :: Float -> Picture
flag w = Pictures
  [ Color (hex 0xDE2910) (rect 0 (negate h) w h)
  , Color (hex 0xFFDE00) (Pictures (bigStar : map smallStar [(10, 2), (12, 4), (12, 7), (10, 9)]))
  ]
  where
    h = w * 2 / 3
    u = w / 30
    -- The flag's own grid is 30 x 20 with y down.
    at' (gx, gy) = (gx * u, negate gy * u)
    bigStar = Translate (5 * u) (-5 * u) (star (3 * u))
    -- Each small star points one tip at the big star's centre.
    smallStar g@(gx, gy) =
      let (vx, vy) = (5 - gx, gy - 5)
          turn = atan2 vx vy * 180 / pi
      in at (at' g) (Rotate turn (star u))

-- | A red paper lantern with gold caps and a tassel, hanging from the origin.
lantern :: Picture
lantern = Pictures
  [ Color (hex 0x3A2413) (rect (-0.75) (-6) 1.5 6)
  , Color (hex 0xE3B341) (rect (-5) (-9) 10 3.5)
  , Color (hex 0x7A0E12) (Translate 0 (-20) (ellipse 18 12.5 12))
  , Color (hex 0xD7191F) (Translate 0 (-20) (ellipse 18 11 11))
  , Color (hex 0xF04A3A) (Translate (-3) (-17) (ellipse 12 4 6))
  , Color (hex 0xA3131A) (Pictures [rect (-5) (-29) 1.2 18, rect 4 (-29) 1.2 18])
  , Color (hex 0xE3B341) (rect (-5) (-33.5) 10 3.5)
  , Color (hex 0xE3B341) (Pictures [rect (-0.75) (-42) 1.5 9, Translate 0 (-43) (ngon 6 2)])
  ]

-- | Rays fanning upward from the origin, @r@ long: @n@ wedges across @spread@ degrees.
sunburst :: Int -> Float -> Float -> Picture
sunburst n r spread = Pictures
  [ tri (0, 0) (ray (a - w)) (ray (a + w))
  | i <- [0 .. n - 1]
  , let a = negate spread / 2 + spread * (fromIntegral i + 0.5) / fromIntegral n
        w = spread / fromIntegral n / 4 ]
  where ray deg = let t = deg * pi / 180 in (r * sin t, r * cos t)
