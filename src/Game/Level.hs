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

-- | Every level in play order, or the reason one is malformed. A level's budget is
-- its par, which a test re-derives with the solver, plus slack that shrinks as the
-- game goes on. Short on purpose: it is played in about three minutes.
campaign :: Either String [Level]
campaign = sequence
  [ room baseRules "Through the Glass" 22 20
      -- Three cells below the mirror, your reflection would land in the wall: walking
      -- on is all the key does. Two below, it lands by the power cell, and the same
      -- key steps through.
      [ "#############"
      , "#############"
      , "#####.B.#####"
      , "#####...#####"
      , "######v######"
      , "######.######"
      , "######.######"
      , "######.######"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#####...#####"
      , "#####..G#####"
      , "#############"
      , "#############"
      , "#############"
      , "######S######"
      , "#############"
      ]
  , room baseRules "Rust in the Beam" 34 16
      -- The present mirror's light stops at a crate rusted solid. In the past it
      -- moves, and with it out of the way the light reaches the corridor.
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
      -- The past mirror is the only way into the cell's room, and the room's only way
      -- out is the present mirror, whose light a shut door stops. Step through first
      -- and you are shut in: the crate has to reach the plate before you go.
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
  ]

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

mirrorFace :: Char -> Maybe Dir
mirrorFace c = case c of
  '^' -> Just DirUp
  'v' -> Just DirDown
  '<' -> Just DirLeft
  '>' -> Just DirRight
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
  pastMarks    <- scanMap "past" width height ("#.BC~P" ++ mirrorMarks) past
  presentMarks <- scanMap "present" width height ("#.GSD" ++ mirrorMarks) present
  powerCell <- exactlyOne "past" 'B' pastMarks
  goal      <- exactlyOne "present" 'G' presentMarks
  spawn     <- exactlyOne "present" 'S' presentMarks
  let open marks = Set.fromList [c | (m, c) <- marks, m `notElem` ("#~D" ++ mirrorMarks)]
      mirrorsIn era marks = [ (c, (era, face)) | (m, c) <- marks, Just face <- [mirrorFace m] ]
      mirrors = Map.fromList (mirrorsIn Past pastMarks ++ mirrorsIn Present presentMarks)
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
      , lvMirrors = mirrors
      , lvBudget = budget, lvRules = baseRules
      , lvName = "", lvPar = budget, lvDressed = False
      }
    else Left ("border is open at " ++ show leaks)
