-- | Campaign and prototype maps, the original test fixture, and the parser that
-- refuses malformed levels. Game.Types defines the returned Level records;
-- Game.Logic supplies dynamic collision, and Game.Run selects levels for play.
-- This module validates map structure, not solvability or the claimed par: the
-- tests use Game.Solution to establish those through the real rules.
-- Rows run from top to bottom and columns from left to right, starting at zero.
-- Coordinates are cells, with y growing downwards, not Gloss canvas units.
-- Budgets, pars and slack are seconds of action cost, unrelated to real time.
-- Map strings are source data: every character, including #, occupies one cell.
module Game.Level
  ( pastRows, presentRows
  , parseLevel
  , theLevel
  , campaign
  , secretLevels
  , levelHall
  , extraLevels
  , walkable
  , passageCells
  ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Game.Types

-- | Past half of Harrow Point, the original fixture. The gap at (4,2) opens
-- the western partition; the eastern partition is sealed. The power cell at
-- (10,2) therefore requires another era crossing beyond that eastern wall.
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

-- | Present half of the fixture. Spawn (2,6) and lamp (2,2) share the west room.
-- Only this era opens the eastern partition at (8,6); the past's western gap
-- is closed here. Alternating passages force the outward and homeward shifts.
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
-- pinned there.
theLevel :: Either String Level
-- (<$>) changes only a successful Right Level, preserving any Left error.
-- The record update keeps the parsed maps while adding fixture metadata.
theLevel = (\l -> l { lvName = "Harrow Point Light Station", lvDressed = True, lvPar = 108 })
  <$> parseLevel 128 pastRows presentRows

-- | Every level in play order, or the reason one is malformed. A level's budget is
-- its par, which a test re-derives with the solver, plus slack that shrinks as the
-- game goes on. Short on purpose: it is played in about three minutes.
campaign :: Either String [Level]
-- sequence turns [Either error Level] into Either error [Level], preserving
-- order and stopping at the first malformed map. The room helper adds par/slack;
-- each following transformation adds explanatory or ending metadata.
campaign = sequence
  -- Clear the Way: the shared crate blocks the first route east; its past recess
  -- lets it be moved out of the way. The upper gaps then require both eras to
  -- reach the wick and return to the present lamp.
  [ withLesson "Crates only move in the past. In the present they are rusted solid."
    <$> room baseRules "Clear the Way" 48 20
      -- Push the crate into its recess in the past: in the present it is rusted
      -- solid, so the first thing to learn is to switch era. The passage above has
      -- alternating gaps: cross the first in the present and the second in
      -- the past, where the power cell waits below the lighthouse landing.
      [ "#############"
      , "#############"
      , "#########.###"
      , "#########B###"
      , "####..#...###"
      , "##..C.#######"
      , "#############"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "#######G..###"
      , "#########.###"
      , "####....#.###"
      , "##S...#######"
      , "#############"
      , "#############"
      , "#############"
      ]
  -- Two Alignments: one crate occupies useful launch space, and the past sled
  -- must serve two alignments. The present sled gives the return crossing from
  -- the isolated wick, so a jump is part of a route rather than a one-off trick.
  , withLesson "Push a mirror from its back or sides. Step into its light to jump through."
    <$> room baseRules "Two Alignments" 80 16
      -- Aim the past mirror at the battery alcove and clear its launch cell.
      -- Return through the present mirror, then aim the first mirror again
      -- to reach the lighthouse footing across a separate gap.
      [ "#############"
      , "#############"
      , "#######B#.###"
      , "####..#######"
      , "####.s....###"
      , "####......###"
      , "######.C..###"
      , "#############"
      , "#############"
      ]
      [ "#############"
      , "#############"
      , "###..e..#G###"
      , "###.#########"
      , "###.......###"
      , "####......###"
      , "######...S###"
      , "#############"
      , "#############"
      ]
  -- The Long Way Round: the shortcut crate prevents an early return to the hub.
  -- The other crates restrict launch cells, beams and gallery movement; the
  -- fixed and sliding mirrors connect rooms whose floors do not meet directly.
  -- withPassage names only the walls the ending breaks, not a playable route.
  , withLesson "Four rooms. A mirror set in the wall never moves: it is a one-way door."
    . withPassage [(11, 3), (10, 3), (6, 3), (0, 3)]
    <$> room baseRules "The Long Way Round" 306 40
      -- Four rooms of 13 by 9, one screen each, and each room has two ways through.
      -- Hub (top left) -> the west room (bottom left): either keep the present mirror
      -- low, tuck a crate behind your standing cell and chain three jumps, the last
      -- one through the fixed mirror into a pocket sealed in the past; or raise the
      -- mirror, push the crate off the fixed mirror's column and take its long jump
      -- -> the vault (bottom right): Crowded Track, holding the power cell -> the
      -- gallery (top right): lift both crates out of the past gallery row, using the
      -- movable mirror as a door, or standing on its cell in the other era -> push
      -- the shortcut crate at (12,4) west from (13,4) in the past, which only works
      -- from that side, and walk home to the lighthouse.
      [ "##########################"
      , "#.....####################"
      , "#.#.#.################.###"
      , "#.....#...############.###"
      , "#...........C.####>.C.C.##"
      , "#.#.#.#...############..##"
      , "#.....####################"
      , "#.....####################"
      , "###.######################"
      , "###.######################"
      , "#....#....C.##############"
      , "#.########.##########B####"
      , "#..C.#####.########.######"
      , "#....#####^####.s.C...####"
      , "##########.######.#.#.####"
      , "#################.#.#C####"
      , "##########............####"
      , "##########################"
      ]
      [ "##########################"
      , "#.....####################"
      , "#.....################.###"
      , "#.....#...##########...###"
      , "#..........G.######....###"
      , "#.....#...#########v..w.##"
      , "#.....#############.######"
      , "#..S..#############.######"
      , "###.###############.######"
      , "###.###############.######"
      , "#....#.###..#######...####"
      , "####w#####..#########.####"
      , "#....#...<..#######.######"
      , "###############.......####"
      , "##########.######.#.#n####"
      , "##########.######.#.#.####"
      , "##########............####"
      , "##########################"
      ]
  ]

-- | Levels the hall never lists. Each is found from the end of the level before it.
secretLevels :: Either String [Level]
-- Two Keepers: the two corner mirrors turn crossings round the water column.
-- The vault crate and jetty gate enforce an order that needs the other keeper;
-- the T marker provides that second starting position. It is reachable by the
-- ending or development flag, while lvSecret keeps it out of the ordinary hall.
secretLevels = sequence
  [ (\l -> l { lvSecret = True })
    . withLesson "Two keepers, one era. A corner mirror turns you out of its other face, as far as you stood."
    <$> room baseRules "Two Keepers" 118 60
      -- Two islands with water (column 6) between, crossed only by corner mirrors, and
      -- one way only: the far shore never sees their light. Whoever carries the cell
      -- goes east -> the east mirror (past) into the vault -> pushes its door crate out
      -- west -> the west mirror (present) onto the east jetty -> walks out once the
      -- gate crate at (8,4) is pushed into the east mirror's light, home to the lamp.
      -- It takes two: the gate can only be pushed from the east, after the carrier is
      -- in the vault (it shuts the vault for good), and the vault door crate lands on
      -- the one spot that crosses to the east island (5,2), so yellow must cross first.
      -- Either keeper can carry.
      [ "#############"
      , "#B...##7#...#"
      , "###.C.#....##"
      , "####..#.#..##"
      , "#.....#.C...#"
      , "#.....##.#..#"
      , "#.....##.#..#"
      , "#....##..#..#"
      , "#############"
      ]
      [ "#############"
      , "#########..G#"
      , "####..##...##"
      , "#####.###..##"
      , "#.....#..#..#"
      , "#.....##.#T.#"
      , "#.S...##.#..#"
      , "#....L#..#..#"
      , "#############"
      ]
  ]

-- | Everything the run can reach: the campaign, then its secret levels.
levelHall :: Either String [Level]
-- (<*>) combines a second successful Either value with the function inside the
-- first. A parse error in either list prevents constructing a partial run.
levelHall = (++) <$> campaign <*> secretLevels

-- | The first four-room level, before levels 3 and 4 were merged. Its west room asks
-- the player to fill pits with crates, which no earlier level teaches.
harrowPointBelow :: Either String Level
harrowPointBelow =
  withLesson "Four rooms. A mirror set in the wall never moves: it is a one-way door."
    <$> room baseRules "Harrow Point, Below" 318 40
      -- Four rooms of 13 by 9, one screen each. Hub (top left) -> the chasm (bottom
      -- left): push crates into pits to make the one spot two cells from the fixed
      -- mirror, the only distance that lands -> the vault (bottom right): Crowded
      -- Track, holding the power cell -> the gallery (top right): stand on a mirror's
      -- cell in the other era to push the crate beneath it -> push the shortcut crate
      -- at (12,4) west from (13,4) in the past, which only works from that side, and
      -- walk home to the lighthouse.
      [ "##########################"
      , "#.....####################"
      , "#.#.#.###########.########"
      , "#.....#...#######..#######"
      , "#...........C..##.C.######"
      , "#.#.#.#...#######~########"
      , "#.....####################"
      , "#.....####################"
      , "###.######################"
      , "###.######################"
      , "#....#####################"
      , "#C##.################B####"
      , "#.C..##############.######"
      , "#.#..~~~<#.####.s.C...####"
      , "#~#..#####.######.#.#.####"
      , "#.##C#####.######.#.#C####"
      , "####.#####............####"
      , "##########################"
      ]
      [ "##########################"
      , "#.....####################"
      , "#.....###########.########"
      , "#.....#...#######.########"
      , "#..........G.#..#e..######"
      , "#.....#...#######...######"
      , "#.....#############.######"
      , "#..S..#############.######"
      , "###.###############.######"
      , "###.###############.######"
      , "#....##############...####"
      , "#.#..################.####"
      , "#....##############.######"
      , "#.#...####.####.......####"
      , "###..#####.######.#.#n####"
      , "#.##.#####.######.#.#.####"
      , "#....#####............####"
      , "##########################"
      ]

-- | The team's level 3, from the level-hall branch. A three-wing station: each wing
-- hides the next part of the route outside the camera. In each wing a crate must
-- leave a mirror's beam and hold a pressure plate; two movable mirrors must then be
-- lowered into the cleared crossing lanes. Once all plates are held, the present-day
-- doors form the long route home. Its movable mirrors, written @l@ and @d@ there,
-- are @w@ and @s@ here.
theLongReflection :: Either String Level
theLongReflection =
  withLesson "Clear each beam with its crate, hold every plate, then move or use the mirror."
  -- Under these rules the cheapest route costs 188, not the 224 it was built for;
  -- the budget is that 224, so the route it was designed around still fits.
  <$> room baseRules "Level 3 - The Long Reflection" 188 36
      [ "#########################"
      , "#########################"
      , "#####...###...#####..B###"
      , "###.....#.......###..C..#"
      , "###.#..w#####..w###^...##"
      , "###...C.#.....C.#......##"
      , "#####P..###.#..####..v..#"
      , "##...##.###....####..C..#"
      , "##...##.######P####...###"
      , "##C.....#.......#..P#####"
      , "##...##.###s#.######.####"
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

-- | Walls that open once the level is won: the way on to the secret level after
-- it. The first is in the wall beside the lighthouse; the rest lie on one straight
-- line from there to the outer wall, where the way leaves the map.
withPassage :: [Cell] -> Level -> Level
withPassage cells level = level { lvPassage = cells }

-- | Attach a level's short teaching line; it changes no map, rule or budget.
-- The returned record shares every field except lvLesson with the input.
withLesson :: String -> Level -> Level
withLesson lesson level = level { lvLesson = lesson }

-- | The single-screen hard level that the four-room one grew out of; its vault is
-- this map. Kept to build on.
-- Its two crates obstruct the crossing track and launch area. Moving them in
-- the past lets the south-facing sled reach the wick's isolated cell; the
-- north-facing present sled provides a route towards the lamp above the track.
crowdedTrack :: Either String Level
crowdedTrack = room baseRules "Crowded Track" 112 12
  [ "#############"
  , "#############"
  , "########B####"
  , "######.######"
  , "##.s.C...####"
  , "####.#.#.####"
  , "####.#.#C####"
  , "####.....####"
  , "#############"
  ]
  [ "#############"
  , "######G..####"
  , "########.####"
  , "######.######"
  , "##.......####"
  , "####.#.#n####"
  , "####.#.#.####"
  , "####S....####"
  , "#############"
  ]

-- | The past mirror is the only way into the cell's room, and the room's only way
-- out is the present mirror, whose light a shut door stops. Step through first and
-- you are shut in: the crate has to reach the plate before you go. Kept to build on.
oneWayIn :: Either String Level
oneWayIn = room baseRules "One Way In" 42 12
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
-- The present corridor leads towards the lamp, but contains no wick. Shift
-- into the past at shared floor, follow its U-shaped corridor to (5,6), then
-- retrace the route and return to the present to deliver it. Walls prevent a
-- direct crossing between the two arms.
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
extraLevels = sequence $ theLongReflection : harrowPointBelow : firstLight : oneWayIn : crowdedTrack : echoLevels ++
  -- First Push: the shared crate plugs the past partition. Push it east to enter
  -- the wick's side, then use the present floor to return to the lamp; the past
  -- alcove under the east room makes changing era necessary for collection.
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
  -- The Plug: the crate occupies the centre of a small past cross. Move it off
  -- the north/south route so the upper present corridor can reach the lower
  -- past corridor containing the wick, then come back to the present lamp.
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
  -- The Buried Lamp: the crate's recess lies above the present lamp. The lower
  -- floor changes shape between eras; use those shared cells to approach and
  -- reposition the crate, reach the eastern wick spur, and leave the lamp free.
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
  -- The Held Door: slide the crate right onto the plate to keep the present
  -- door open after walking away. The wick is behind the partition in the past;
  -- the present door is the connection back to the lamp's side.
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
  -- The Bridge: the only past crossing to the wick is a pit. Bring the crate
  -- into its row and push it into the pit to make permanent floor; the present
  -- wall at the same coordinate prevents simply shifting across the gap.
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
  -- The Wrong Way First: the wick lies east, but the lamp is beyond the western
  -- pit. Get to the right of the crate via present floor and push it west into
  -- that pit before returning for the wick; pushing it east cannot build home.
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

-- | Membership in an era's static floor set. This deliberately ignores crates,
-- mirrors, filled pits and open doors: use Game.Logic.openCell for current
-- collision. Out-of-map cells are absent from the sets and return False.
walkable :: Level -> Era -> Cell -> Bool
walkable level era cell = cell `Set.member` cells
  where
    cells = case era of
      Past    -> lvPast level
      Present -> lvPresent level

-- | Static floor differences, grouped Past then Present, in (x,y) order within
-- each group. The original fixture has two passages; general levels have many.
-- Set.difference removes shared floor, without consulting dynamic occupants.
passageCells :: Level -> [(Era, Cell)]
passageCells level =
  [ (Past, c)    | c <- Set.toAscList (lvPast level `Set.difference` lvPresent level) ] ++
  [ (Present, c) | c <- Set.toAscList (lvPresent level `Set.difference` lvPast level) ]

-- | Marks with their cells, for one map, after checking its shape and alphabet.
scanMap :: String -> Int -> Int -> String -> [String] -> Either String [(Char, Cell)]
scanMap name width height allowed rows
  -- Check the outer size before traversing, so missing rows cannot silently
  -- become a smaller map. Each following row has the width of the past map.
  | length rows /= height =
      Left (name ++ ": expected " ++ show height ++ " rows, found " ++ show (length rows))
  -- traverse keeps each successful row or the first Left error; concat flattens
  -- the validated rows. zip supplies zero-based coordinates, including walls.
  | otherwise = concat <$> traverse scanRow (zip [0 ..] rows)
  where
    -- Keep y in scope for errors and each character's cell; a ragged row cannot
    -- be accepted just because the characters it does contain are valid.
    scanRow (y, row)
      | length row /= width =
          Left (name ++ ": row " ++ show (y :: Int) ++ " has width " ++ show (length row)
                ++ ", expected " ++ show width)
      | otherwise = traverse (scanChar y) (zip [0 ..] row)
    -- Alphabet checking happens before interpreting marks, so unknown symbols
    -- never disappear through a later filter.
    scanChar y (x, c)
      | c `elem` allowed = Right (c, (x, y))
      | otherwise = Left (name ++ ": unexpected character " ++ show c
                          ++ " at " ++ show (x :: Int, y))

-- | A mirror set in the wall is drawn as the way its face looks: the light goes out
-- that way. A movable one, standing on floor, is the compass letter of that way. A
-- corner mirror, always set in the wall, is drawn as the corner its two faces make,
-- as on a pipe map: @L@ looks north and east, @J@ north and west, @7@ south and west,
-- @F@ south and east.
fixedMirrorMarks, movableMirrorMarks, mirrorMarks :: String
fixedMirrorMarks = "^v<>LJ7F"
movableMirrorMarks = "nsew"
mirrorMarks = fixedMirrorMarks ++ movableMirrorMarks

-- | The face a mark looks out of, and a corner mirror's second face.
mirrorShape :: Char -> Maybe (Dir, Maybe Dir)
-- Straight and compass marks share directions but differ in movability, which
-- mirrorsIn decides separately. Corner marks always have two distinct faces;
-- Nothing lets callers reject/filter every non-mirror mark.
mirrorShape c = case c of
  '^' -> Just (DirUp, Nothing)
  'v' -> Just (DirDown, Nothing)
  '<' -> Just (DirLeft, Nothing)
  '>' -> Just (DirRight, Nothing)
  'n' -> Just (DirUp, Nothing)
  's' -> Just (DirDown, Nothing)
  'w' -> Just (DirLeft, Nothing)
  'e' -> Just (DirRight, Nothing)
  'L' -> Just (DirUp, Just DirRight)
  'J' -> Just (DirUp, Just DirLeft)
  '7' -> Just (DirDown, Just DirLeft)
  'F' -> Just (DirDown, Just DirRight)
  _   -> Nothing

-- | Require a single named marker in already scanned cells. Return its cell or
-- distinguish missing and duplicate markers in the error; no default is chosen.
exactlyOne :: String -> Char -> [(Char, Cell)] -> Either String Cell
exactlyOne name mark marks = case [c | (m, c) <- marks, m == mark] of
  [cell] -> Right cell
  []     -> Left (name ++ ": missing " ++ show mark)
  many   -> Left (name ++ ": " ++ show (length many) ++ " copies of " ++ show mark)

-- | Validates dimensions, alphabet and markers, and that both maps are sealed
-- by walls. Spawn, wick and lamp markers are floor; their labels add no collision. A crate has
-- one position shared by both eras, written on the past map, on floor in both.
-- A pit blocks the past until a crate fills it. A plate, on the past map, is floor
-- in both eras so a crate can rest on it. A door, on the present map, is a wall until
-- every plate is held (by a crate, keeper or echo in the rules). A mirror stands
-- in the era of the map it is on: one set
-- in the wall (^ v < >) is solid there for good, and a movable one (n s e w) stands
-- on floor that it leaves behind when pushed. The budget
-- is in seconds: the cheapest possible cost plus the level's slack.
parseLevel :: Int -> [String] -> [String] -> Either String Level
parseLevel budget past present = do
  -- nub removes duplicate widths: one positive width means all past rows agree.
  -- The pattern guard w > 0 rejects zero-width rows; an empty map also fails.
  -- Either's do notation short-circuits at the first Left without any IO.
  width <- case nub (map length past) of
    [w] | w > 0 -> Right w
    _           -> Left "past: rows are empty or of different widths"
  let height = length past
  -- Era-specific alphabets keep B/C/pits/plates in the past and G/S/T/doors in
  -- the present. Both maps must share the past's width and height.
  pastMarks    <- scanMap "past" width height ("#.BC~P" ++ mirrorMarks) past
  presentMarks <- scanMap "present" width height ("#.GSDT" ++ mirrorMarks) present
  powerCell <- exactlyOne "past" 'B' pastMarks
  goal      <- exactlyOne "present" 'G' presentMarks
  spawn     <- exactlyOne "present" 'S' presentMarks
  -- T is optional but unique. A two-keeper level still requires the ordinary S.
  spawn2    <- case [c | ('T', c) <- presentMarks] of
    []  -> Right Nothing
    [c] -> Right (Just c)
    _   -> Left "present: more than one 'T'; a level has at most two keepers"
  -- These let bindings share validated marks throughout the remaining checks.
  -- Static floor excludes fixed glass, pits and doors. Sliding mirrors have
  -- floor underneath; their current position is blocked later by Game.Logic.
  let open marks = Set.fromList [c | (m, c) <- marks, m `notElem` ("#~D" ++ fixedMirrorMarks)]
      -- A pattern generator Just ... <- [...] discards Nothing, selecting only
      -- mirrors. A Map holds one mirror per cell; detect any collision below
      -- before the overwritten entry could hide a second era's mirror.
      mirrorsIn era marks = [ (c, Mirror era face (m `elem` movableMirrorMarks) bend)
                            | (m, c) <- marks, Just (face, bend) <- [mirrorShape m] ]
      movable = [ c | (c, m) <- Map.toList mirrors, mMovable m ]
      mirrors = Map.fromList (mirrorsIn Past pastMarks ++ mirrorsIn Present presentMarks)
      -- Sets forget scan order and provide membership/subset checks. Crates
      -- belong to both eras even though only the past map writes their marker.
      marked mark marks = Set.fromList [c | (m, c) <- marks, m == mark]
      pits = marked '~' pastMarks
      crates = marked 'C' pastMarks
      plates = marked 'P' pastMarks
      doors = marked 'D' presentMarks
      onBorder (x, y) = x == 0 || y == 0 || x == width - 1 || y == height - 1
      -- A plate beside a present door would let the keeper start entering while
      -- still holding the plate. Reject that geometry rather than strand them.
      besideDoors = [ p | p <- Set.toList plates, d <- [minBound .. maxBound]
                        , stepCell d p `Set.member` doors ]
      -- Include all special non-wall cells, not just floor: a border pit, door
      -- or fixed mirror would otherwise escape the sealed-border check.
      leaks = filter onBorder (Set.toList (open pastMarks) ++ Set.toList (open presentMarks)
                               ++ Set.toList pits ++ Set.toList doors ++ Map.keys mirrors)
  -- Shared crates must not overlap either spawn or a sliding mirror, and must
  -- have present floor beneath them. These checks concern initial placement.
  if maybe False (`Set.member` crates) spawn2
    then Left ("a crate stands on the second spawn at " ++ show spawn2)
  else if spawn `Set.member` crates
    then Left ("a crate stands on the spawn at " ++ show spawn)
  else if any (`Set.member` crates) movable
    then Left "a movable mirror stands on a crate's cell"
  else if not (crates `Set.isSubsetOf` open presentMarks)
    then Left ("a crate stands in a present wall at "
               ++ show (Set.toList (crates `Set.difference` open presentMarks)))
  -- A plate needs present floor too, since a shared crate may rest on it.
  else if not (plates `Set.isSubsetOf` open presentMarks)
    then Left ("a plate is in a present wall, where no crate could rest, at "
               ++ show (Set.toList (plates `Set.difference` open presentMarks)))
  -- Doors use the single all-plates circuit. Having only one half of the circuit
  -- is treated as a map authoring error, rather than a special door behaviour.
  else if Set.null plates /= Set.null doors
    then Left "plates and doors need each other: a level has both or neither"
  -- Map construction collapses duplicate cells; comparing counts recovers the
  -- otherwise lost evidence of mirrors occupying one cell in opposite eras.
  else if Map.size mirrors /= length (mirrorsIn Past pastMarks ++ mirrorsIn Present presentMarks)
    then Left "a cell has a mirror in both eras; give each mirror its own cell"
  else if not (null besideDoors)
    then Left ("a plate is next to a door, so the player could hold it open while walking in, at "
               ++ show besideDoors)
  -- Only after every check succeeds construct the Level. Metadata defaults are
  -- overwritten by room/theLevel; no route search or positive-budget check runs
  -- here, so structural validity alone does not promise a playable level.
  else if null leaks
    then Right Level
      { lvWidth = width, lvHeight = height
      , lvPast = open pastMarks, lvPresent = open presentMarks
      , lvSpawn = spawn, lvPowerCell = powerCell, lvGoal = goal
      , lvCrates = crates, lvPits = pits, lvPlates = plates, lvDoors = doors
      , lvMirrors = mirrors
      , lvBudget = budget, lvRules = baseRules
      , lvName = "", lvLesson = "", lvPar = budget, lvDressed = False
      , lvSpawn2 = spawn2, lvPassage = [], lvSecret = False
      }
    else Left ("border is open at " ++ show leaks)
