-- | Drafting-office lettering, drawn from strokes. Gloss's own Text is a one-pixel
-- font with no width metric; this one centres exactly and holds its weight.
--
-- Capitals are full height and lowercase letters are small capitals, so sentences
-- read as sentences while everything stays on one engraved plate-maker's alphabet.
--
-- How it works. Each character is a few strokes, polylines on a grid 6 units tall
-- (y = 0 is the baseline, y = 6 the top of a capital), written out by hand in
-- 'glyph'. 'layout' sets a string out on that grid at a given size, letter after
-- letter, and 'draw' turns every stroke into filled polygons of a pen width. A
-- lowercase letter is its capital's strokes at 0.8 of the size. Sizes, widths and
-- positions outside 'glyph' are canvas units (the screen is 1280 by 800 of them);
-- a size is the height of a capital.
--
-- What it does not do: colour (the caller wraps the result in 'Color'), caching
-- (a caller that draws the same words every frame can flatten them once with
-- 'Draw.bake', as the title screen does), vertical alignment other than on the
-- baseline, or any IO. A character with no strokes here draws as a blank 2 grid
-- units wide.
--
-- Who uses it: "Hud", "Lobby", "Station", "Render" and "Title" letter the watch
-- and its keys, the level hall, the stencils on the station's floor, the lesson
-- line and the costs on the board, and the title screen with it. The courier's
-- talk uses the pixel font in "PixelFont" instead.
module Font
  ( textL, textC, textR
  , textW
  , weighted
  , wrapText
  ) where

import Data.Char (isLower, toUpper)
import Draw
import Graphics.Gloss

-- | One pen stroke: a polyline, its points joined in order. In 'glyph' the points
-- are grid units (6 to a capital's height); 'layout' turns them into canvas units.
-- A stroke of a single point draws as a dot.
type Stroke = [(Float, Float)]

-- | Points along an arc of an ellipse, for the round parts of letters.
--
-- @curve n cx cy rx ry a0 a1@: centre @(cx, cy)@, radii @rx@ across and @ry@ up,
-- from angle @a0@ to angle @a1@ in degrees, measured anticlockwise from the
-- positive x axis as in school geometry (y grows upwards in Gloss). @a1@ may be
-- smaller than @a0@, to go clockwise, or past 360, to go on round through 0. The
-- result has @n + 1@ points evenly spaced in angle, both ends included, so @n@ is
-- the number of straight pieces the arc is cut into.
curve :: Int -> Float -> Float -> Float -> Float -> Float -> Float -> Stroke
curve n cx cy rx ry a0 a1 =
  -- A list comprehension with a let: for each i from 0 to n, t is that point's
  -- angle, turned from degrees into the radians that cos and sin take.
  [ (cx + rx * cos t, cy + ry * sin t)
  | i <- [0 .. n]
  , let t = (a0 + (a1 - a0) * fromIntegral i / fromIntegral n) * pi / 180 ]

-- | Strokes on a grid 6 tall. @n@ sets how finely curves are cut.
--
-- @glyph n ch@ gives the strokes of the capital, figure or sign @ch@; 'layout'
-- turns lowercase into capitals before it asks. x counts from 0 at the pen, and
-- 'layout' takes the largest x of any point as the glyph's width, so the next
-- letter starts after the rightmost point. y runs from 0 on the baseline to 6 at
-- the top of a capital; Q's tail, the comma and the slash reach below 0.
--
-- Straight letters are lists of corner points (A is a caret and a crossbar). Round
-- parts come from 'curve', called @a@ here, joined with '++' to straight runs, so
-- that a stem and its bowl (B, D, P, R) are one continuous stroke. @'*'@ is not a
-- star but a short dash at mid height, which "Hud" puts between the keys in its
-- lines ("P resume * R restart"). Any character not listed has no strokes.
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
  '+' -> [[(0,2.8),(2.8,2.8)], [(1.4,1.4),(1.4,4.2)]]
  '/' -> [[(0,-0.4),(2.8,6.4)]]
  '*' -> [[(0.2,2.8),(0.5,2.8)]]
  _   -> []
  -- A local abbreviation: a is curve with the fineness already given, so each
  -- arc above reads a cx cy rx ry from to.
  where a = curve n

-- | Everything on one grid, at one drawing size.
--
-- @layout size track str@ places every stroke of @str@ for letters @size@ canvas
-- units tall, with @track@ extra canvas units after each character, and returns
-- the placed strokes with the pen's x at the end. The text starts at x = 0 with its
-- baseline on y = 0.
--
-- A left fold walks the string carrying (strokes so far, pen x): @foldl place s0
-- "ab"@ is @place (place s0 'a') 'b'@. The lazy 'foldl' and the @acc ++ placed@
-- append (which copies the strokes so far for every letter) would be slow on long
-- text, but a line of lettering is a few dozen characters. Laziness also means a
-- caller that only wants the width ('textW') never builds the placed strokes.
layout :: Float -> Float -> String -> ([Stroke], Float)
layout size track str = foldl place ([], 0) str
  where
    -- Canvas units per grid unit: the grid is 6 units to a capital's height.
    k = size / 6
    -- One character: a space only moves the pen, 2.5 grid units and the tracking.
    place (acc, x) c
      | c == ' ' = (acc, x + 2.5 * k + track)
      | otherwise =
          -- A lowercase letter is drawn with its capital's strokes at 0.8 of the
          -- size, on the same baseline: a small capital.
          let scaleBy = if isLower c then 0.8 else 1
              strokes = glyph fineness (toUpper c)
              -- The rightmost point of any stroke; a glyph with no strokes (a
              -- character the font does not have) counts as 2 grid units wide.
              width = case concat strokes of
                [] -> 2
                ps -> maximum (map fst ps)
              -- Canvas units per grid unit for this letter.
              m = k * scaleBy
              -- Each point scaled to canvas units and slid along to the pen: a
              -- comprehension inside a comprehension, the outer one over strokes,
              -- the inner one over the points of each.
              placed = [ [ (x + px * m, py * m) | (px, py) <- s ] | s <- strokes ]
          -- The pen moves past the glyph, then a 1.5 grid unit gap at full size
          -- (even after a small capital, so letter spacing stays even), then the
          -- tracking.
          in (acc ++ placed, x + width * m + 1.5 * k + track)
    -- How many straight pieces each curve is cut into: big letters need more to
    -- look round, while on small ones the difference would not show and fewer
    -- pieces mean fewer polygons to draw.
    fineness | size >= 40 = 12
             | size >= 18 = 7
             | otherwise  = 4

-- | Pen weight that suits a size.
--
-- The pen's width in canvas units: 13% of the letter height, so lettering of every
-- size looks equally heavy, but never under 1.25, so the smallest text keeps a
-- solid line.
defaultWeight :: Float -> Float
defaultWeight size = max 1.25 (size * 0.13)

-- | Width in canvas units of a line, at a size and with the default tracking.
--
-- 'layout' leaves the pen after the last character's gap and tracking, so those
-- are taken off again: what is left runs from x = 0 to the rightmost point of the
-- last glyph. It measures the strokes' centre lines, so the pen's own thickness
-- sticks out about half a weight at each end. 'max 0' keeps the empty string at 0.
-- 'textC' and 'textR' align with it, and callers size plates and keycaps by it.
-- The let takes the pair apart and keeps only the pen position.
textW :: Float -> String -> Float
textW size str = let (_, w) = layout size (tracking size) str in max 0 (w - 1.5 * size / 6 - tracking size)

-- | Extra space after each character, in canvas units: half a unit for text under
-- 18 units tall, where letters would otherwise crowd, and none for larger text.
tracking :: Float -> Float
tracking size = if size < 18 then 0.5 else 0

-- | A string as filled polygons, at a size and a pen weight (both canvas units),
-- starting at x = 0 on the baseline y = 0, with no colour of its own.
draw :: Float -> Float -> String -> Picture
draw size weight str = Pictures (concatMap stroke strokes)
  where
    -- A pattern binding: only the strokes of layout's result are wanted.
    (strokes, _) = layout size (tracking size) str
    -- Big letters get round joints so curves stay clean. Small ones keep the
    -- cheap square ends, which are too small to see.
    --
    -- From size 26: a 'plainSeg' (a band that stops exactly at its ends) between
    -- each point and the next, which zipWith pairs up by walking the stroke beside
    -- itself without its first point, and a 12-sided disc as wide as the pen on
    -- every point, which rounds every joint and end. Below 26: 'seg', whose square
    -- ends run on half the pen's width past each point so corners close; a stroke
    -- of one point is then a square dot (seg draws a square for a zero length).
    stroke s
      | size >= 26 = zipWith (plainSeg weight) s (drop 1 s)
                       ++ [ at p (ngon 12 (weight / 2)) | p <- s ]
      | otherwise = case s of
          [p] -> [seg weight p p]
          _   -> zipWith (seg weight) s (drop 1 s)

-- | Left-aligned at the origin, baseline at y = 0. Colourless, so it can be
-- tinted and cached.
--
-- 'textL' starts the text at x = 0, 'textC' centres it on x = 0 and 'textR' ends it
-- at x = 0, all with the default pen weight; @size@ is a capital's height in canvas
-- units. The centred and right-aligned forms lay the text out twice, once to
-- measure it. textL is defined with its string argument left off both sides
-- (point-free): @textL size@ is @draw size (defaultWeight size)@, a function still
-- waiting for the string.
textL, textC, textR :: Float -> String -> Picture
textL size = draw size (defaultWeight size)
textC size str = Translate (negate (textW size str) / 2) 0 (textL size str)
textR size str = Translate (negate (textW size str)) 0 (textL size str)

-- | As 'textL', with the pen weight chosen by hand.
--
-- @weighted size weight str@, both in canvas units. "Title" draws BORROWED LIGHT
-- with it (68 tall, a pen 8.6 wide), and "Hud" uses it to keep a chosen weight on
-- text it then scales.
weighted :: Float -> Float -> String -> Picture
weighted = draw

-- | Break a line into lines no wider than @maxW@.
--
-- Greedy, like 'PixelFont.pixelWrap', but measured with 'textW' at @size@, so
-- @maxW@ is in canvas units. A word too wide for @maxW@ gets a line to itself and
-- sticks out. Nothing outside this module calls it at present.
wrapText :: Float -> Float -> String -> [String]
wrapText size maxW = go [] . words
  where
    -- row holds the current line's words newest first (cheap to add to); it is
    -- reversed when the line is given out, and the last line is given out only if
    -- it has any words.
    go row [] = [unwords (reverse row) | not (null row)]
    go row (w : ws)
      | null row || textW size (unwords (reverse (w : row))) <= maxW = go (w : row) ws
      | otherwise = unwords (reverse row) : go [w] ws
