-- | A pixel font for the courier's talk, the line under the opening film that says
-- how to skip it, and the tags on the board's markers: capitals 5 by 7, lowercase
-- with descenders, drawn as square pixels so it sits with her pixel-art portrait.
-- The station's own plates keep the stroke font in "Font".
--
-- Sizes are in pixels: a line is 'pixelLineH' pixels tall and each pixel is drawn
-- @px@ canvas units square. Text hangs from its top-left corner.
--
-- What it does: turns a 'String' into a Gloss 'Picture' made only of axis-aligned
-- rectangles ('pixelText', 'pixelTextC'), measures a line's width in pixels
-- ('pixelW') and wraps a long line into rows that fit a width ('pixelWrap'). What
-- it does not do: colour (the caller wraps the picture in 'Color', which tints
-- every rectangle inside at once), right alignment, line breaks inside a string,
-- or any IO.
--
-- Who uses it: only "TalkBox". Her lines are drawn at 3 canvas units a pixel (the
-- size of her portrait's pixels), her name at 4, and the small print (the line
-- count, ENTER, COURIER, the marker's tag and the film's skip hint) at 2. It needs
-- nothing but 'Draw.rect' and Gloss's 'Picture' type.
--
-- How a glyph is stored: 'table' at the bottom draws each character as rows of
-- @#@ (lit) and @.@ (clear), which is easy to read and edit by eye. 'glyphs' turns
-- those rows, once, into horizontal runs of lit pixels, so a letter is a few
-- rectangles rather than one per pixel: fewer polygons for Gloss to draw a frame.
--
-- Coordinates: pixel columns count to the right from the pen's position and pixel
-- rows count down from the top of the line. Gloss's y axis grows upwards, so a
-- glyph's row 0 is drawn just under the origin and later rows further down.
module PixelFont
  ( pixelText
  , pixelTextC
  , pixelW
  , pixelLineH
  , pixelWrap
  ) where

import qualified Data.Map.Strict as Map
import Draw (rect)
import Graphics.Gloss (Picture (..))

-- | Pixels from the top of one line to the top of the next.
--
-- 7 for a capital, 2 more for a descender (g j p q y hang that far below the
-- line), and 3 of space before the next line. "TalkBox" steps the rows of her
-- talk down by this many pixels.
pixelLineH :: Int
pixelLineH = 12

-- | Each glyph as rows of runs, @(row, column, length)@, and its width in pixels.
--
-- Built from 'table'. Row 0 is the top of the glyph; a run is a stretch of lit
-- pixels in one row, @length@ pixels long, starting @column@ pixels from the
-- glyph's left. The width is the glyph's longest row, clear pixels included, so a
-- glyph's own blank columns (the comma's leading dot, say) count towards it.
--
-- A top-level value with no arguments (a CAF, "constant applicative form"):
-- Haskell works it out at most once, the first time it is needed, and every later
-- use shares the result, so the table is turned into runs once each time the game
-- is started. A 'Map.Map' finds a character in O(log n) steps instead of walking the
-- whole list. "Data.Map.Strict" is the variant that evaluates each value as it is
-- stored (to its outer constructor, here the pair), rather than keeping a postponed
-- computation in the map.
glyphs :: Map.Map Char ([(Int, Int, Int)], Int)
glyphs = Map.fromList [ (c, (runs rows, width rows)) | (c, rows) <- table ]
  where
    -- The longest row decides the width. The 1 at the front of the list keeps
    -- 'maximum' from failing on a glyph with no rows ('maximum []' is an error), and
    -- makes every glyph at least a pixel wide.
    width rows = maximum (1 : map length rows)
    -- Pair each row with its number from the top ('zip' with the endless list
    -- [0 ..] stops when the rows run out), and gather the runs of every row into one
    -- list.
    runs rows = concat [ rowRuns y r | (y, r) <- zip [0 ..] rows ]
    -- The runs of row @y@: walk it from the left, @x@ being the column reached.
    rowRuns y r = go 0 r
      where
        go _ [] = []
        -- In s@(ch : _), an "as-pattern", s names the whole rest of the row and ch
        -- its first character. 'span' splits off the longest front part that passes
        -- its test, so one call takes a whole run of '#' (recorded as a run) or a
        -- whole gap (skipped), and the column moves on by that part's length.
        go x s@(ch : _)
          | ch == '#' = let (on, rest) = span (== '#') s in (y, x, length on) : go (x + length on) rest
          | otherwise = let (off, rest) = span (/= '#') s in go (x + length off) rest

-- | The runs and width of one character, as stored in 'glyphs'.
--
-- Typographic quotes and the long dash are first read as their plain forms, so
-- text typed in a word processor still draws. A character the table does not have
-- is drawn as @?@, so a missing glyph shows instead of vanishing. 'Map.findWithDefault'
-- takes the answer for a missing key first, then the key and the map; the inner
-- one, an empty glyph 3 pixels wide, would only be reached if @?@ itself were taken
-- out of the table.
glyphOf :: Char -> ([(Int, Int, Int)], Int)
glyphOf c = Map.findWithDefault (Map.findWithDefault ([], 3) '?' glyphs) (normal c) glyphs
  where
    -- Character codes here are Unicode code points in decimal: 8217 and 8216 are
    -- the right and left single quotes, 8220 and 8221 the left and right double
    -- quotes, 8212 the em dash. Anything else is left as it is.
    normal ch = case ch of
      '\8217' -> '\''
      '\8216' -> '\''
      '\8220' -> '"'
      '\8221' -> '"'
      '\8212' -> '-'
      _ -> ch

-- | Width of a line, in pixels.
--
-- Every character moves the pen on by its 'advance', which includes a 1-pixel gap
-- before the next character; the gap after the last one is not part of the line,
-- hence the @- 1@. 'max 0' keeps the empty string at 0 rather than -1. Spaces
-- advance three pixels; the final gap is subtracted even after a space. Multiply
-- by the pixel size for canvas units, as
-- "TalkBox" does (for instance @* 2@ for its small print).
pixelW :: String -> Int
pixelW s = max 0 (sum (map advance s) - 1)

-- | How far the pen moves after a character, in pixels: a space is 3 pixels of
-- nothing; any other character is its glyph's width plus a 1-pixel gap. The first
-- equation matches the space literally; every other character falls to the second.
advance :: Char -> Int
advance ' ' = 3
advance c = snd (glyphOf c) + 1

-- | A line of text, its top-left at the origin, each pixel @px@ units square.
-- Colourless, so it can be tinted.
--
-- @px@ is canvas units per font pixel (3 for her lines, 2 for small print). The
-- result is one 'Pictures' of rectangles, one per run of lit pixels. A glyph's row
-- @y@ (0 at the top) is drawn from @-(y + 1) * px@ up to @-y * px@, so the text
-- hangs below the origin: a capital reaches 7 pixels down, a descender 9. A
-- newline is not a line break here (it has no glyph, so it draws as @?@): break
-- long text into lines first, with 'pixelWrap', and place each line.
pixelText :: Float -> String -> Picture
pixelText px s = Pictures (go 0 s)
  where
    -- go walks the string with the pen's column x, in font pixels. A space only
    -- moves the pen 3 pixels (the same as its 'advance'). Any other character adds
    -- one rectangle per run, shifted right by x, followed by the rest of the line
    -- with the pen moved on. 'rect' takes the lower-left corner, then the width and
    -- the height (see "Draw"). The width that 'glyphOf' also returns is not needed
    -- here, since 'advance' looks it up itself, so the pattern (rs, _) ignores it.
    go _ [] = []
    go x (c : rest)
      | c == ' ' = go (x + 3) rest
      | otherwise =
          let (rs, _) = glyphOf c
          in [ rect (fromIntegral (x + cx) * px) (negate (fromIntegral (y + 1)) * px) (fromIntegral n * px) px
             | (y, cx, n) <- rs ]
             ++ go (x + advance c) rest

-- | Centred on the origin across, hanging from it.
--
-- 'pixelText' moved left by half the line's width in canvas units (pixels times
-- @px@); up and down it still hangs from y = 0. "TalkBox" uses it for the line
-- count in the corner of her box.
pixelTextC :: Float -> String -> Picture
pixelTextC px s = Translate (negate (fromIntegral (pixelW s) * px / 2)) 0 (pixelText px s)

-- | Break text into lines no wider than @maxW@ pixels.
--
-- Greedy: words are added to the current line while it still fits, and a word
-- that does not fit starts the next line. A single word wider than @maxW@ gets a
-- line to itself and sticks out, rather than being split. 'words' splits on any
-- run of white space and 'unwords' joins with single spaces, so a run of several
-- spaces comes back as one; "TalkBox" relies on the lines being joined by exactly
-- one space when it counts typed letters across them.
--
-- Written point-free: @pixelWrap maxW = go [] . words@ means "split into words,
-- then run go on them, starting with an empty line"; '.' composes two functions.
pixelWrap :: Int -> String -> [String]
pixelWrap maxW = go [] . words
  where
    -- row is the current line's words, newest first, since adding to the front of
    -- a list is cheap; it is reversed when the line is given out. At the end the
    -- last line is given out unless it is empty, so empty text gives no lines at
    -- all: [x | test] is a list of one element if the test holds and none if not.
    go row [] = [unwords (reverse row) | not (null row)]
    -- Measuring the whole candidate line for every word costs time in proportion
    -- to the square of its length, which is nothing for a line of talk.
    go row (w : ws)
      | null row || pixelW (unwords (reverse (w : row))) <= maxW = go (w : row) ws
      | otherwise = unwords (reverse row) : go [w] ws

-- | Glyph source rows. Capitals and figures stand 7 pixels tall; lowercase
-- x-height is 5 pixels from row 2, with ascenders reaching higher and g j p q y
-- hanging 2 below the line.
--
-- Each entry is a character and its rows from the top, one string per pixel row:
-- '#' is a lit pixel and any other character (here '.') is clear. The rows of one
-- glyph may differ in length, and the glyph is as wide as its longest row. Rows
-- need only go as far down as the lowest lit pixel, so some glyphs have fewer
-- than 7 (the quotes sit at the top in 2 rows, the hyphen's bar is in row 3, the
-- middle of a capital) and some more (the comma and the semicolon have a tail one
-- row below the line, at row 7). Characters missing here draw as '?' (see
-- 'glyphOf'). 8230, in decimal, is the ellipsis character: three dots on the line.
table :: [(Char, [String])]
table =
  [ ('A', [".###.", "#...#", "#...#", "#####", "#...#", "#...#", "#...#"])
  , ('B', ["####.", "#...#", "#...#", "####.", "#...#", "#...#", "####."])
  , ('C', [".###.", "#...#", "#....", "#....", "#....", "#...#", ".###."])
  , ('D', ["####.", "#...#", "#...#", "#...#", "#...#", "#...#", "####."])
  , ('E', ["#####", "#....", "#....", "####.", "#....", "#....", "#####"])
  , ('F', ["#####", "#....", "#....", "####.", "#....", "#....", "#...."])
  , ('G', [".###.", "#...#", "#....", "#.###", "#...#", "#...#", ".####"])
  , ('H', ["#...#", "#...#", "#...#", "#####", "#...#", "#...#", "#...#"])
  , ('I', ["###", ".#.", ".#.", ".#.", ".#.", ".#.", "###"])
  , ('J', ["..###", "...#.", "...#.", "...#.", "#..#.", "#..#.", ".##.."])
  , ('K', ["#...#", "#..#.", "#.#..", "##...", "#.#..", "#..#.", "#...#"])
  , ('L', ["#....", "#....", "#....", "#....", "#....", "#....", "#####"])
  , ('M', ["#...#", "##.##", "#.#.#", "#.#.#", "#...#", "#...#", "#...#"])
  , ('N', ["#...#", "#...#", "##..#", "#.#.#", "#..##", "#...#", "#...#"])
  , ('O', [".###.", "#...#", "#...#", "#...#", "#...#", "#...#", ".###."])
  , ('P', ["####.", "#...#", "#...#", "####.", "#....", "#....", "#...."])
  , ('Q', [".###.", "#...#", "#...#", "#...#", "#.#.#", "#..#.", ".##.#"])
  , ('R', ["####.", "#...#", "#...#", "####.", "#.#..", "#..#.", "#...#"])
  , ('S', [".####", "#....", "#....", ".###.", "....#", "....#", "####."])
  , ('T', ["#####", "..#..", "..#..", "..#..", "..#..", "..#..", "..#.."])
  , ('U', ["#...#", "#...#", "#...#", "#...#", "#...#", "#...#", ".###."])
  , ('V', ["#...#", "#...#", "#...#", "#...#", "#...#", ".#.#.", "..#.."])
  , ('W', ["#...#", "#...#", "#...#", "#.#.#", "#.#.#", "#.#.#", ".#.#."])
  , ('X', ["#...#", "#...#", ".#.#.", "..#..", ".#.#.", "#...#", "#...#"])
  , ('Y', ["#...#", "#...#", ".#.#.", "..#..", "..#..", "..#..", "..#.."])
  , ('Z', ["#####", "....#", "...#.", "..#..", ".#...", "#....", "#####"])
  , ('a', [".....", ".....", ".###.", "....#", ".####", "#...#", ".####"])
  , ('b', ["#....", "#....", "####.", "#...#", "#...#", "#...#", "####."])
  , ('c', ["....", "....", ".###", "#...", "#...", "#...", ".###"])
  , ('d', ["....#", "....#", ".####", "#...#", "#...#", "#...#", ".####"])
  , ('e', [".....", ".....", ".###.", "#...#", "#####", "#....", ".###."])
  , ('f', ["..##", ".#..", "####", ".#..", ".#..", ".#..", ".#.."])
  , ('g', [".....", ".....", ".####", "#...#", "#...#", "#...#", ".####", "....#", ".###."])
  , ('h', ["#....", "#....", "####.", "#...#", "#...#", "#...#", "#...#"])
  , ('i', ["#", ".", "#", "#", "#", "#", "#"])
  , ('j', ["..#", "...", "..#", "..#", "..#", "..#", "..#", "#.#", ".#."])
  , ('k', ["#...", "#...", "#..#", "#.#.", "##..", "#.#.", "#..#"])
  , ('l', ["#.", "#.", "#.", "#.", "#.", "#.", ".#"])
  , ('m', [".....", ".....", "##.#.", "#.#.#", "#.#.#", "#.#.#", "#.#.#"])
  , ('n', [".....", ".....", "####.", "#...#", "#...#", "#...#", "#...#"])
  , ('o', [".....", ".....", ".###.", "#...#", "#...#", "#...#", ".###."])
  , ('p', [".....", ".....", "####.", "#...#", "#...#", "#...#", "####.", "#....", "#...."])
  , ('q', [".....", ".....", ".####", "#...#", "#...#", "#...#", ".####", "....#", "....#"])
  , ('r', ["....", "....", "#.##", "##..", "#...", "#...", "#..."])
  , ('s', [".....", ".....", ".####", "#....", ".###.", "....#", "####."])
  , ('t', [".#..", ".#..", "####", ".#..", ".#..", ".#..", "..##"])
  , ('u', [".....", ".....", "#...#", "#...#", "#...#", "#...#", ".####"])
  , ('v', [".....", ".....", "#...#", "#...#", "#...#", ".#.#.", "..#.."])
  , ('w', [".....", ".....", "#...#", "#...#", "#.#.#", "#.#.#", ".#.#."])
  , ('x', [".....", ".....", "#...#", ".#.#.", "..#..", ".#.#.", "#...#"])
  , ('y', [".....", ".....", "#...#", "#...#", "#...#", "#...#", ".####", "....#", ".###."])
  , ('z', [".....", ".....", "#####", "...#.", "..#..", ".#...", "#####"])
  , ('0', [".###.", "#...#", "#..##", "#.#.#", "##..#", "#...#", ".###."])
  , ('1', [".#.", "##.", ".#.", ".#.", ".#.", ".#.", "###"])
  , ('2', [".###.", "#...#", "....#", "...#.", "..#..", ".#...", "#####"])
  , ('3', ["####.", "....#", "....#", ".###.", "....#", "....#", "####."])
  , ('4', ["...#.", "..##.", ".#.#.", "#..#.", "#####", "...#.", "...#."])
  , ('5', ["#####", "#....", "####.", "....#", "....#", "#...#", ".###."])
  , ('6', [".###.", "#....", "#....", "####.", "#...#", "#...#", ".###."])
  , ('7', ["#####", "....#", "...#.", "..#..", ".#...", ".#...", ".#..."])
  , ('8', [".###.", "#...#", "#...#", ".###.", "#...#", "#...#", ".###."])
  , ('9', [".###.", "#...#", "#...#", ".####", "....#", "....#", ".###."])
  , ('.', [".", ".", ".", ".", ".", ".", "#"])
  , (',', ["..", "..", "..", "..", "..", ".#", ".#", "#."])
  , ('!', ["#", "#", "#", "#", "#", ".", "#"])
  , ('?', [".###.", "#...#", "....#", "...#.", "..#..", ".....", "..#.."])
  , ('\'', ["#", "#"])
  , ('"', ["#.#", "#.#"])
  , (':', [".", ".", "#", ".", ".", ".", "#"])
  , (';', ["..", "..", ".#", "..", "..", "..", ".#", "#."])
  , ('-', ["....", "....", "....", "####"])
  , ('+', [".....", ".....", "..#..", "..#..", "#####", "..#..", "..#.."])
  , ('/', ["....#", "....#", "...#.", "..#..", ".#...", "#....", "#...."])
  , ('(', ["..#", ".#.", "#..", "#..", "#..", ".#.", "..#"])
  , (')', ["#..", ".#.", "..#", "..#", "..#", ".#.", "#.."])
  , ('#', [".#.#.", ".#.#.", "#####", ".#.#.", "#####", ".#.#.", ".#.#."])
  , ('%', ["##..#", "##..#", "...#.", "..#..", ".#...", "#..##", "#..##"])
  , ('>', ["#...", ".#..", "..#.", "...#", "..#.", ".#..", "#..."])
  , ('<', ["...#", "..#.", ".#..", "#...", ".#..", "..#.", "...#"])
  , ('\8230', [".....", ".....", ".....", ".....", ".....", ".....", "#.#.#"])
  ]
