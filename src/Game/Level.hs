-- | The one level, and the parser that refuses to build a bad one.
module Game.Level
  ( pastRows, presentRows
  , parseLevel
  , theLevel
  , walkable
  , passageCells
  ) where

import Data.List (nub)
import qualified Data.Set as Set
import Game.Types

pastRows :: [String]
pastRows =
  [ "#############"
  , "#...#...#...#"
  , "#.......#.B.#"
  , "#...#...#...#"
  , "#...#...#...#"
  , "#...#...#...#"
  , "#...#...#...#"
  , "#...#...#...#"
  , "#############"
  ]

presentRows :: [String]
presentRows =
  [ "#############"
  , "#...#...#...#"
  , "#.G.#...#...#"
  , "#...#...#...#"
  , "#...#...#...#"
  , "#...#...#...#"
  , "#.S.#.......#"
  , "#...#...#...#"
  , "#############"
  ]

-- | The level, or the reason it is malformed.
theLevel :: Either String Level
theLevel = parseLevel pastRows presentRows

walkable :: Level -> Era -> Cell -> Bool
walkable level era cell = cell `Set.member` cells
  where
    cells = case era of
      Past    -> lvPast level
      Present -> lvPresent level

-- | Cells open in exactly one era, in (x, y) order: the two passages.
passageCells :: Level -> [(Era, Cell)]
passageCells level =
  [ (Past, c)    | c <- Set.toAscList (lvPast level `Set.difference` lvPresent level) ] ++
  [ (Present, c) | c <- Set.toAscList (lvPresent level `Set.difference` lvPast level) ]

-- | Marks with their cells, for one map, after checking its shape and alphabet.
scanMap :: String -> Int -> Int -> String -> [String] -> Either String [(Char, Cell)]
scanMap name width height allowed rows
  | length rows /= height =
      Left (name ++ ": expected " ++ show height ++ " rows, found " ++ show (length rows))
  | otherwise = concat <$> traverse scanRow (zip [0 ..] rows)
  where
    scanRow (y, row)
      | length row /= width =
          Left (name ++ ": row " ++ show (y :: Int) ++ " has width " ++ show (length row)
                ++ ", expected " ++ show width)
      | otherwise = traverse (scanChar y) (zip [0 ..] row)
    scanChar y (x, c)
      | c `elem` allowed = Right (c, (x, y))
      | otherwise = Left (name ++ ": unexpected character " ++ show c
                          ++ " at " ++ show (x :: Int, y))

exactlyOne :: String -> Char -> [(Char, Cell)] -> Either String Cell
exactlyOne name mark marks = case [c | (m, c) <- marks, m == mark] of
  [cell] -> Right cell
  []     -> Left (name ++ ": missing " ++ show mark)
  many   -> Left (name ++ ": " ++ show (length many) ++ " copies of " ++ show mark)

-- | Validates dimensions, alphabet and markers, and that both maps are sealed
-- by walls. Markers are walkable; they add no collision of their own.
parseLevel :: [String] -> [String] -> Either String Level
parseLevel past present = do
  width <- case nub (map length past) of
    [w] | w > 0 -> Right w
    _           -> Left "past: rows are empty or of different widths"
  let height = length past
  pastMarks    <- scanMap "past" width height "#.B" past
  presentMarks <- scanMap "present" width height "#.GS" present
  powerCell <- exactlyOne "past" 'B' pastMarks
  goal      <- exactlyOne "present" 'G' presentMarks
  spawn     <- exactlyOne "present" 'S' presentMarks
  let open marks = Set.fromList [c | (m, c) <- marks, m /= '#']
      onBorder (x, y) = x == 0 || y == 0 || x == width - 1 || y == height - 1
      leaks = filter onBorder (Set.toList (open pastMarks) ++ Set.toList (open presentMarks))
  if null leaks
    then Right Level
      { lvWidth = width, lvHeight = height
      , lvPast = open pastMarks, lvPresent = open presentMarks
      , lvSpawn = spawn, lvPowerCell = powerCell, lvGoal = goal
      }
    else Left ("border is open at " ++ show leaks)
