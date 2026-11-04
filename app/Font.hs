-- | Drafting-office lettering, drawn from strokes. Gloss's own Text is a one-pixel
-- font with no width metric; this one centres exactly and holds its weight.
--
-- Capitals are full height and lowercase letters are small capitals, so sentences
-- read as sentences while everything stays on one engraved plate-maker's alphabet.
module Font
  ( textL, textC, textR
  , textW
  , weighted
  , wrapText
  ) where

import Data.Char (isLower, toUpper)
import Draw
import Graphics.Gloss

type Stroke = [(Float, Float)]

curve :: Int -> Float -> Float -> Float -> Float -> Float -> Float -> Stroke
curve n cx cy rx ry a0 a1 =
  [ (cx + rx * cos t, cy + ry * sin t)
  | i <- [0 .. n]
  , let t = (a0 + (a1 - a0) * fromIntegral i / fromIntegral n) * pi / 180 ]

-- | Strokes on a grid 6 tall. @n@ sets how finely curves are cut.
glyph :: Int -> Char -> [Stroke]
glyph n ch = case ch of
  'A' -> [[(0,0),(2,6),(4,0)], [(0.75,2.1),(3.25,2.1)]]
  'B' -> [ [(0,3.1),(0,6),(2.2,6)] ++ a 2.2 4.55 1.5 1.45 90 (-90) ++ [(0,3.1)]
         , [(0,3.1),(2.4,3.1)] ++ a 2.4 1.55 1.6 1.55 90 (-90) ++ [(0,0),(0,3.1)] ]
  'C' -> [a 2.1 3 2.1 3 48 312]
  'D' -> [[(0,0),(0,6),(1.5,6)] ++ a 1.5 3 2.5 3 90 (-90) ++ [(0,0)]]
  'E' -> [[(3.6,6),(0,6),(0,0),(3.6,0)], [(0,3.1),(2.9,3.1)]]
  'F' -> [[(3.6,6),(0,6),(0,0)], [(0,3.1),(2.9,3.1)]]
  'G' -> [a 2.1 3 2.1 3 48 315 ++ [(4,2.7),(2.5,2.7)]]
  'H' -> [[(0,0),(0,6)], [(4,0),(4,6)], [(0,3.1),(4,3.1)]]
  'I' -> [[(0,0),(0,6)]]
  'J' -> [[(3,6),(3,1.5)] ++ a 1.5 1.5 1.5 1.5 0 (-160)]
  'K' -> [[(0,0),(0,6)], [(3.8,6),(0,2.3)], [(1.35,3.6),(4,0)]]
  'L' -> [[(0,6),(0,0),(3.4,0)]]
  'M' -> [[(0,0),(0,6),(2.5,1.6),(5,6),(5,0)]]
  'N' -> [[(0,0),(0,6),(4,0),(4,6)]]
  'O' -> [a 2.2 3 2.2 3 90 450]
  'P' -> [[(0,0),(0,6),(2.3,6)] ++ a 2.3 4.3 1.7 1.7 90 (-90) ++ [(0,2.6)]]
  'Q' -> [a 2.2 3 2.2 3 90 450, [(2.6,1.5),(4.4,-0.2)]]
  'R' -> [[(0,0),(0,6),(2.3,6)] ++ a 2.3 4.4 1.7 1.6 90 (-90) ++ [(0,2.8)], [(1.9,2.8),(4,0)]]
  'S' -> [a 2 4.5 1.9 1.5 35 270 ++ a 2 1.5 2 1.5 90 (-145)]
  'T' -> [[(0,6),(4,6)], [(2,6),(2,0)]]
  'U' -> [[(0,6),(0,2)] ++ a 2 2 2 2 180 360 ++ [(4,6)]]
  'V' -> [[(0,6),(2,0),(4,6)]]
  'W' -> [[(0,6),(1.4,0),(2.8,4.6),(4.2,0),(5.6,6)]]
  'X' -> [[(0,0),(4,6)], [(0,6),(4,0)]]
  'Y' -> [[(0,6),(2,3),(4,6)], [(2,3),(2,0)]]
  'Z' -> [[(0,6),(4,6),(0,0),(4,0)]]
  '0' -> [a 2 3 2 3 90 450]
  '1' -> [[(0,4.7),(1.5,6),(1.5,0)]]
  '2' -> [a 2 4.3 1.9 1.7 165 (-35) ++ [(0,0),(4,0)]]
  '3' -> [a 1.9 4.5 1.8 1.5 150 (-90) ++ a 1.9 1.5 2.1 1.5 90 (-150)]
  '4' -> [[(3,0),(3,6),(0,1.9),(4,1.9)]]
  '5' -> [[(3.6,6),(0.5,6),(0.2,3.3)] ++ a 2 1.9 2 1.9 125 (-145)]
  '6' -> [a 2.1 3 2 3 60 180 ++ a 2 1.9 2 1.9 180 540]
  '7' -> [[(0,6),(4,6),(1.3,0)]]
  '8' -> [a 2 4.55 1.75 1.45 (-90) 270, a 2 1.55 2 1.55 90 450]
  '9' -> [a 1.9 3 2 3 240 360 ++ a 2 4.1 2 1.9 0 360]
  '.' -> [[(0.3,0),(0.3,0.45)]]
  ',' -> [[(0.4,0.4),(0,-0.9)]]
  ':' -> [[(0.3,0.5),(0.3,1)], [(0.3,3.6),(0.3,4.1)]]
  '\'' -> [[(0.3,6),(0.3,4.4)]]
  '!' -> [[(0.3,6),(0.3,1.9)], [(0.3,0),(0.3,0.45)]]
  '-' -> [[(0,2.8),(2.4,2.8)]]
  '/' -> [[(0,-0.4),(2.8,6.4)]]
  '*' -> [[(0.2,2.8),(0.5,2.8)]]
  _   -> []
  where a = curve n

-- | Everything on one grid, at one drawing size.
layout :: Float -> Float -> String -> ([Stroke], Float)
layout size track str = foldl place ([], 0) str
  where
    k = size / 6
    place (acc, x) c
      | c == ' ' = (acc, x + 2.5 * k + track)
      | otherwise =
          let scaleBy = if isLower c then 0.8 else 1
              strokes = glyph fineness (toUpper c)
              width = case concat strokes of
                [] -> 2
                ps -> maximum (map fst ps)
              m = k * scaleBy
              placed = [ [ (x + px * m, py * m) | (px, py) <- s ] | s <- strokes ]
          in (acc ++ placed, x + width * m + 1.5 * k + track)
    fineness | size >= 40 = 12
             | size >= 18 = 7
             | otherwise  = 4

-- | Pen weight that suits a size.
defaultWeight :: Float -> Float
defaultWeight size = max 1.25 (size * 0.13)

-- | Width in canvas units of a line, at a size and with the default tracking.
textW :: Float -> String -> Float
textW size str = let (_, w) = layout size (tracking size) str in max 0 (w - 1.5 * size / 6 - tracking size)

tracking :: Float -> Float
tracking size = if size < 18 then 0.5 else 0

draw :: Float -> Float -> String -> Picture
draw size weight str = Pictures (concatMap stroke strokes)
  where
    (strokes, _) = layout size (tracking size) str
    -- Big letters get round joints so curves stay clean. Small ones keep the
    -- cheap square ends, which are too small to see.
    stroke s
      | size >= 26 = zipWith (plainSeg weight) s (drop 1 s)
                       ++ [ at p (ngon 12 (weight / 2)) | p <- s ]
      | otherwise = case s of
          [p] -> [seg weight p p]
          _   -> zipWith (seg weight) s (drop 1 s)

-- | Left-aligned at the origin, baseline at y = 0. Colourless, so it can be
-- tinted and cached.
textL, textC, textR :: Float -> String -> Picture
textL size = draw size (defaultWeight size)
textC size str = Translate (negate (textW size str) / 2) 0 (textL size str)
textR size str = Translate (negate (textW size str)) 0 (textL size str)

-- | As 'textL', with the pen weight chosen by hand.
weighted :: Float -> Float -> String -> Picture
weighted = draw

-- | Break a line into lines no wider than @maxW@.
wrapText :: Float -> Float -> String -> [String]
wrapText size maxW = go [] . words
  where
    go row [] = [unwords (reverse row) | not (null row)]
    go row (w : ws)
      | null row || textW size (unwords (reverse (w : row))) <= maxW = go (w : row) ws
      | otherwise = unwords (reverse row) : go [w] ws
