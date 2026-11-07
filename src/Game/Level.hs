-- | The one level, and the parser that refuses to build a bad one.
module Game.Level
  ( pastRows, presentRows
  , parseLevel
  , theLevel
  , campaign
  , extraLevels
  , walkable
  , passageCells
  ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
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

-- | The original station, kept as the tests' fixture: its map, route and dressing are
-- pinned there. The campaign opens with a shorter way through the same walls.
theLevel :: Either String Level
theLevel = (\l -> l { lvName = "Harrow Point Light Station", lvDressed = True, lvPar = 108 })
  <$> parseLevel 128 pastRows presentRows

-- | The current three-room presentation. The former third room is now the second;
-- the finale is a much larger station explored through a following camera.
campaign :: Either String [Level]
campaign = sequence [optionAbove, threeReflections, grandStation]

optionAbove :: Either String Level
optionAbove = withLesson "OPTION A: Space to the past, then push the crate into the pocket above."
  <$> room baseRules "Option A - Pocket Above" 40 20
      -- In the present the rusted crate seals the only route out of the spawn.
      -- Shift on the lower stem, push it upward into the dead pocket in the past,
      -- collect the cell on the left, then reach the lamp through the right stem.
      [ "#############"
      , "#############"
      , "#########.###"
      , "#####.###.###"
      , "##B..C.....##"
      , "#####.#######"
      , "#####.#######"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#########G###"
      , "#####.###.###"
      , "##.........##"
      , "#####.#######"
      , "#####S#######"
      , "#############"
      , "#############"
      ]

threeReflections :: Either String Level
threeReflections = withLesson "Three mirrors, three crates. Clear the last beam before entering the cell room."
  <$> room baseRules "Level 2 - Three Reflections" 60 36
      -- The third crate blocks the final mirror and also the only standing place
      -- for pushing the entrance mirror. Move it into the upper pocket first.
      -- Two more crates then block the entrance beam at different distances.
      [ "#############"
      , "#############"
      , "#########.B.#"
      , "#####.###^###"
      , "####..d.....#"
      , "##.#>C....C.#"
      , "###.....##C.#"
      , "##########.##"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "##G######...#"
      , "##.##.#######"
      , "##.#........#"
      , "##.#........#"
      , "###.S...##..#"
      , "##########.##"
      , "#############"
      ]

-- | A three-wing station. Each wing hides the next part of the route outside the
-- camera. In each wing a crate must leave a mirror's beam and hold a pressure
-- plate; two movable mirrors must then be lowered into the cleared crossing lanes.
-- Once all plates are held, the present-day doors form the long route home.
grandStation :: Either String Level
grandStation = withLesson "Clear each beam with its crate, hold every plate, then move or use the mirror."
  <$> room baseRules "Level 3 - The Long Reflection" 224 76
      [ "#########################"
      , "#########################"
      , "#####...###...#####..B###"
      , "###.....#.......###..C..#"
      , "###.#..l#####..l###^...##"
      , "###...C.#.....C.#......##"
      , "#####P..###.#..####..v..#"
      , "##...##.###....####..C..#"
      , "##...##.######P####...###"
      , "##C.....#.......#..P#####"
      , "##...##.###d#.######.####"
      , "####....#.......#....####"
      , "###.....###...###....####"
      , "#########################"
      , "#########################"
      ]
      [ "#########################"
      , "#########################"
      , "##.G.####################"
      , "###.#..#############..###"
      , "###.#.###################"
      , "###.#..#######..#########"
      , "###.#.#######..####..####"
      , "###.#########..####...###"
      , "###.#########..####..####"
      , "##..###############..####"
      , "###.################.####"
      , "#.......D.......D....####"
      , "###S#####################"
      , "#########################"
      , "#########################"
      ]

withLesson :: String -> Level -> Level
withLesson lesson level = level { lvLesson = lesson }

-- | The echo levels, kept to build on.
echoLevels :: [Either String Level]
echoLevels =
  [ room echoRules "Echo" 38 16
      -- Standing on the plate opens the door, and stepping off shuts it. Four steps
      -- on from the plate, the oldest footprint is the plate itself.
      [ "#############"
      , "#############"
      , "#############"
      , "#B###########"
      , "#.#P#########"
      , "#############"
      , "#############"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#############"
      , "#############"
      , "#S......D.G##"
      , "#############"
      , "#############"
      , "#############"
      , "#############"
      ]
  , room echoRules "Two Places at Once" 44 12
      -- The plate can only be reached in the past, and the door is in the present.
      -- The echo walks back across the shift and stays on the plate in the past.
      [ "#############"
      , "#############"
      , "####B.#######"
      , "####..#######"
      , "####P..######"
      , "#############"
      , "#############"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#############"
      , "#############"
      , "####.#..D.G##"
      , "######.######"
      , "######S######"
      , "#############"
      , "#############"
      ]
  ]

-- | Walking and switching, and what each costs. No echo yet.
firstLight :: Either String Level
firstLight = room baseRules "First Light" 48 20
  [ "#############"
  , "#############"
  , "##....#######"
  , "##.##.#######"
  , "##.##.#######"
  , "##.##.#######"
  , "##.##B#######"
  , "#############"
  , "#############"
  ]
  [ "#############"
  , "#############"
  , "##G.#########"
  , "##.##########"
  , "##.##########"
  , "##.##########"
  , "##S##########"
  , "#############"
  , "#############"
  ]

-- | The earlier campaign, kept to build on. Not played, and not held to the
-- campaign's checks, since the player's weight on a plate can change their pars.
extraLevels :: Either String [Level]
extraLevels = sequence $ firstLight : echoLevels ++
  [ room baseRules "First Push" 42 16
      [ "#############"
      , "#############"
      , "#....#.....##"
      , "#....C.....##"
      , "#....#.....##"
      , "##########B##"
      , "#############"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#....#..G..##"
      , "#.S........##"
      , "#....#.....##"
      , "#############"
      , "#############"
      , "#############"
      , "#############"
      ]
  , room baseRules "Rust in the Beam" 34 16
      [ "#############"
      , "#############"
      , "#####.B.#####"
      , "#####...#####"
      , "#####C....###"
      , "#####.#######"
      , "#############"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#############"
      , "#...#########"
      , "#.G.>....S###"
      , "#...#.#######"
      , "#############"
      , "#############"
      , "#############"
      ]
  , room baseRules "One Way In" 42 12
      [ "#############"
      , "#############"
      , "#############"
      , "######.B#####"
      , "######..#####"
      , "######v######"
      , "##.....######"
      , "##.CP..######"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#############"
      , "#############"
      , "#.G.>D.######"
      , "#############"
      , "###S.########"
      , "###..########"
      , "#############"
      ]
  , room baseRules "The Plug" 56 12
      [ "#############"
      , "#############"
      , "#############"
      , "######.######"
      , "#####.C.#####"
      , "##.###.######"
      , "##........B##"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#...#####...#"
      , "#.S.##.##.G.#"
      , "#...........#"
      , "##.##########"
      , "#############"
      , "#############"
      , "#############"
      ]
  , room baseRules "The Buried Lamp" 64 12
      [ "#############"
      , "#...#########"
      , "#.....#######"
      , "#...#.#######"
      , "#####.###B###"
      , "#####..##.###"
      , "#####.C.#.###"
      , "#####..######"
      , "#############"
      ]
      [ "#############"
      , "#...#########"
      , "#..S#########"
      , "#...#########"
      , "#############"
      , "######.######"
      , "#####.....###"
      , "######G######"
      , "#############"
      ]
  , room baseRules "The Held Door" 54 12
      [ "#############"
      , "#############"
      , "#......######"
      , "#......#...##"
      , "#.C.P..#..B##"
      , "#############"
      , "#############"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#......#...##"
      , "#.S....D.G.##"
      , "#......#..###"
      , "#############"
      , "#############"
      , "#############"
      , "#############"
      ]
  , room baseRules "The Bridge" 44 12
      [ "#############"
      , "#############"
      , "#.....#######"
      , "#..C..#######"
      , "#......~...##"
      , "#.....###B###"
      , "#############"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#.....#######"
      , "#.....#######"
      , "#......#.G.##"
      , "#.S...#######"
      , "#############"
      , "#############"
      , "#############"
      ]
  , room baseRules "The Wrong Way First" 64 8
      [ "#############"
      , "#############"
      , "#############"
      , "#########B###"
      , "#...~..C...##"
      , "#############"
      , "#############"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#.S.....#####"
      , "#####.#.#####"
      , "#.G.#......##"
      , "#######....##"
      , "#############"
      , "#############"
      , "#############"
      ]
  ]

-- | A level with its rules, name and par. Its budget is the par plus the slack.
room :: Rules -> String -> Int -> Int -> [String] -> [String] -> Either String Level
room rules name par slack past present =
  (\l -> l { lvName = name, lvPar = par, lvRules = rules }) <$> parseLevel (par + slack) past present

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

-- | A mirror is drawn as the way its face looks: the light goes out that way.
mirrorMarks :: String
mirrorMarks = "^v<>"

-- | Lowercase mirrors have the same faces, but may be pushed left or right.
movableMirrorMarks :: String
movableMirrorMarks = "udlr"

mirrorFace :: Char -> Maybe Dir
mirrorFace c = case c of
  '^' -> Just DirUp
  'u' -> Just DirUp
  'v' -> Just DirDown
  'd' -> Just DirDown
  '<' -> Just DirLeft
  'l' -> Just DirLeft
  '>' -> Just DirRight
  'r' -> Just DirRight
  _   -> Nothing

exactlyOne :: String -> Char -> [(Char, Cell)] -> Either String Cell
exactlyOne name mark marks = case [c | (m, c) <- marks, m == mark] of
  [cell] -> Right cell
  []     -> Left (name ++ ": missing " ++ show mark)
  many   -> Left (name ++ ": " ++ show (length many) ++ " copies of " ++ show mark)

-- | Validates dimensions, alphabet and markers, and that both maps are sealed
-- by walls. Markers are walkable; they add no collision of their own. A crate has
-- one position shared by both eras, written on the past map, on floor in both.
-- A pit blocks the past until a crate fills it. A plate, on the past map, is floor
-- in both eras so a crate can rest on it. A door, on the present map, is a wall until
-- every plate holds a crate. The budget
-- is in seconds: the cheapest possible cost plus the level's slack.
parseLevel :: Int -> [String] -> [String] -> Either String Level
parseLevel budget past present = do
  width <- case nub (map length past) of
    [w] | w > 0 -> Right w
    _           -> Left "past: rows are empty or of different widths"
  let height = length past
  pastMarks    <- scanMap "past" width height ("#.BC~P" ++ mirrorMarks ++ movableMirrorMarks) past
  presentMarks <- scanMap "present" width height ("#.GSD" ++ mirrorMarks ++ movableMirrorMarks) present
  powerCell <- exactlyOne "past" 'B' pastMarks
  goal      <- exactlyOne "present" 'G' presentMarks
  spawn     <- exactlyOne "present" 'S' presentMarks
  let open marks = Set.fromList [c | (m, c) <- marks, m `notElem` ("#~D" ++ mirrorMarks)]
      mirrorsIn era marks = [ (c, (era, face)) | (m, c) <- marks, Just face <- [mirrorFace m] ]
      mirrors = Map.fromList (mirrorsIn Past pastMarks ++ mirrorsIn Present presentMarks)
      movableMirrors = Set.fromList
        [c | (m, c) <- pastMarks ++ presentMarks, m `elem` movableMirrorMarks]
      marked mark marks = Set.fromList [c | (m, c) <- marks, m == mark]
      pits = marked '~' pastMarks
      crates = marked 'C' pastMarks
      plates = marked 'P' pastMarks
      doors = marked 'D' presentMarks
      onBorder (x, y) = x == 0 || y == 0 || x == width - 1 || y == height - 1
      besideDoors = [ p | p <- Set.toList plates, d <- [minBound .. maxBound]
                        , stepCell d p `Set.member` doors ]
      leaks = filter onBorder (Set.toList (open pastMarks) ++ Set.toList (open presentMarks)
                               ++ Set.toList pits ++ Set.toList doors)
  if spawn `Set.member` crates
    then Left ("a crate stands on the spawn at " ++ show spawn)
  else if not (crates `Set.isSubsetOf` open presentMarks)
    then Left ("a crate stands in a present wall at "
               ++ show (Set.toList (crates `Set.difference` open presentMarks)))
  else if not (plates `Set.isSubsetOf` open presentMarks)
    then Left ("a plate is in a present wall, where no crate could rest, at "
               ++ show (Set.toList (plates `Set.difference` open presentMarks)))
  else if Set.null plates /= Set.null doors
    then Left "plates and doors need each other: a level has both or neither"
  else if Map.size mirrors /= length (mirrorsIn Past pastMarks ++ mirrorsIn Present presentMarks)
    then Left "a cell has a mirror in both eras; give each mirror its own cell"
  else if not (null besideDoors)
    then Left ("a plate is next to a door, so the player could hold it open while walking in, at "
               ++ show besideDoors)
  else if null leaks
    then Right Level
      { lvWidth = width, lvHeight = height
      , lvPast = open pastMarks, lvPresent = open presentMarks
      , lvSpawn = spawn, lvPowerCell = powerCell, lvGoal = goal
      , lvCrates = crates, lvPits = pits, lvPlates = plates, lvDoors = doors
      , lvMirrors = mirrors, lvMovableMirrors = movableMirrors
      , lvBudget = budget, lvRules = baseRules
      , lvName = "", lvLesson = "", lvPar = budget, lvDressed = False
      }
    else Left ("border is open at " ++ show leaks)
