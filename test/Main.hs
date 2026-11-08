-- | Plain assertions over the pure core. Numbered items match the brief's list.
--
-- This is the project's whole test suite, run with
-- @cabal test spec --test-show-details=direct@. It uses no test framework. A check
-- is a name and a 'Bool' ('Check'); 'allChecks' gathers every group of them; 'main'
-- prints one line per check ("ok" or "FAIL", then its name), then a few numbers
-- measured on the implementation ('report', which the README copies under "Test
-- results"), then the totals, and exits with a failure code if any check failed,
-- which is how cabal knows the suite failed.
--
-- What it covers: only the modules under src/Game, which have no graphics and no
-- IO, so every rule can be played here without a window. The numbered groups (1 to
-- 18) are the items the jam's brief required; the named groups pin down every rule
-- added since: crates, pits, plates and doors, the echo, mirrors, the campaign and
-- its dead-end search, the input adapter, effects, the ending, the level for two
-- keepers, sound, the opening film and the courier's talk. Nothing in app/ (drawing,
-- sound synthesis, the save file, the film's decoder) is tested here.
--
-- How the checks drive the game. 'stepGame' (Game.Logic) moves one 'Game' on by one
-- tick, given the keys held during that tick (a 'TickInput'). There are 60 ticks a
-- second; a step takes 'stepTicks' (8) of them, and a shift locks input for
-- 'shiftTicks' (13). A check either feeds ticks one at a time ('run', 'idle'), to
-- pin down timing, or a whole action at a time ('walk', 'shift', and 'applyAction'
-- from Game.Solution, which presses a key for one tick and then waits until the
-- keeper stands still), when only the outcome matters. The rules notice a key only
-- on the tick it goes down (they compare with the last tick's keys, kept in
-- 'gPrev'), so a list of inputs that presses the same key twice needs a tick
-- without it in between.
--
-- Most checks build the exact state they need with 'at' and Haskell's record update
-- syntax (@g { gCell = (3, 2) }@ is a copy of @g@ with that one field changed)
-- rather than playing up to it, then compare a field or two, or whole games: 'Game'
-- derives 'Eq', so @==@ compares every field.
--
-- Conventions. A cell is (x, y): (0, 0) is the top-left cell, x grows to the right
-- and y grows downward, so 'DirUp' takes one off y. Time is the level's budget in
-- seconds, spent only by accepted actions: a step costs 2, a shift 6, a jump through
-- a mirror 4 and an echo 4, and an undo gives back half the undone action's price.
-- These are 'baseRules'; a level may change them.
--
-- The fixture. Most numbered checks use 'theLevel', the original Harrow Point
-- station: 13 by 9 cells, budget 128 (its par, 108, plus 20). Walls in columns 4 and
-- 8 cut it into three rooms three cells wide. The west and middle rooms meet only at
-- (4,2), a door that stands open in the past and is rubble in the present; the
-- middle and east rooms meet only at (8,6), a breach open in the present and shut in
-- the past. The keeper starts at (2,6) in the present, the power cell is at (10,2)
-- in the past, and the lighthouse is at (2,2) in the present. Other checks use small
-- maps parsed in this file ('yard', 'glass', 'mirrorYard', 'cornerYard',
-- 'duoFixture') or levels taken from Game.Level by name.
--
-- Many checks ask the solver. 'search' (Game.Solution) plays every action from every
-- state through the real 'stepGame', so "no reachable state does X" is checked over
-- the keys of its 'srStates', which hold every state the level can reach.
--
-- Laziness keeps this affordable: a check's 'Bool' is only worked out when 'main'
-- prints it, and a search named in a @where@ clause that a group's checks share (as
-- in 'levelChecks') runs once for all of them, the first time one needs it. The
-- suite still takes a while: the campaign's four-room level, The Long Way Round, is
-- searched in full three times (for 'deadSearchChecks', 'levelChecks' and 'report'),
-- besides its restricted and dead-end searches.
module Main (main) where

-- forM runs an IO action for each element of a list and collects the results;
-- unless runs an action only when a condition is False.
import Control.Monad (forM, unless)
-- foldl' is the strict left fold (see 'run'); isInfixOf asks whether one list, here
-- a string, occurs inside another. sort is imported but not used.
import Data.List (foldl', isInfixOf, sort)
import Data.Maybe (isJust)
-- Map and Set are imported qualified, so their functions are written Map.size,
-- Set.member and so on. Many share a name with a Prelude list function (map,
-- filter, null), and the prefix says which is meant. Both are balanced search
-- trees: lookups take logarithmic time, and Set.toList and Map.keys come out in
-- ascending order (for cells, by x and then by y).
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
-- The whole pure core, every module of it. Nothing from app/ is imported, so the
-- suite builds without Gloss or OpenGL.
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic
import Game.Orders
import Game.Run
import Game.Solution
import Game.Sound
import Game.Story
import Game.Types
import System.Exit (exitFailure)

-- | One assertion: its name, printed in the log, and whether it held. The name is
-- the only thing a failure reports, so each says in a sentence what should be true.
-- The 'Bool' is left unevaluated until 'main' prints the check.
data Check = Check String Bool -- ^ Diagnostic name and lazily evaluated result.

-- | Builds a 'Check'. A lower-case name for the constructor, so the lists below read
-- @check "what should hold" condition@. Most checks write @check "..." $@ and put
-- the condition on the lines after: @$@ is function application with the lowest
-- precedence, so everything after it, however long, is the second argument.
check :: String -> Bool -> Check
check = Check

-- | One tick with the direction @d@ held and nothing else. This is the record update
-- syntax used all through this file: @noInput { tiDir = Just d }@ is a copy of
-- 'noInput' (no key held) with only the field 'tiDir' changed.
--
-- It sets 'tiDir' alone, the field a level for one keeper reads. A level for two
-- reads 'tiDir1' (W A S D) and 'tiDir2' (the arrows) instead, so there @press@
-- moves nobody; 'duoChecks' builds its own inputs for that.
press :: Dir -> TickInput
press d = noInput { tiDir = Just d }

-- | One tick with one key held: Space (shift era), Enter (start, retry, go on), R
-- (restart), P (pause), Z (undo) and Escape (back to the level hall). Like 'press',
-- each is a value for a single tick; holding a key for longer is a list that repeats
-- it, and pressing it again needs a tick without it in between.
space, enter, restartKey, pauseKey, undoKey, lobbyKey :: TickInput
space      = noInput { tiShift = True }
enter      = noInput { tiEnter = True }
restartKey = noInput { tiRestart = True }
pauseKey   = noInput { tiPause = True }
undoKey    = noInput { tiUndo = True }
lobbyKey   = noInput { tiLobby = True }

-- | Play a list of ticks, first to last, from @g@: one 'stepGame' per element, so a
-- list of 60 inputs is one second of play. The result is the game after the last
-- tick.
--
-- @foldl' f g [i1, i2]@ is @f (f g i1) i2@: it walks the list from the left,
-- carrying the game along. 'stepGame' takes the input before the game, and the fold
-- hands over the game first, so 'flip' swaps its two arguments. The prime marks the
-- strict fold: it evaluates each intermediate game to its outer constructor.
-- Strict fields (the @!@ in Game.Types) also force their outer values; this does
-- not recursively force every element of the board's containers. A lazy
-- @foldl@ would instead build a chain of 100000 unevaluated steps for the longest
-- check below before working any of them out.
run :: Level -> [TickInput] -> Game -> Game
run level inputs g = foldl' (flip (stepGame level)) g inputs

-- | @n@ ticks with no key held. Defined without its last argument (the game): a
-- function applied to fewer arguments than it takes is a function of the rest, so
-- @idle level n@ is itself a @Game -> Game@, ready to apply or to pass on.
idle :: Level -> Int -> Game -> Game
idle level n = run level (replicate n noInput)

-- | A playing state at a chosen spot, with no leftovers.
--
-- It is the level just started from its title screen ('startPlaying' presses Enter
-- once), with the keeper moved to @cell@ in @era@, carrying the power cell if
-- @hasCell@. Everything else is as the level starts: the whole budget, no history,
-- the crates where the map puts them. 'gPrev' is set back to no keys, so the Enter
-- that started the level is forgotten and the first key a check presses counts as a
-- fresh press.
--
-- Nothing checks that the keeper can stand at @cell@: the checks rely on this to
-- set up a moment directly, such as standing in a doorway. The footprints
-- ('gTrail'), in a level with an echo, still name the spawn, not @cell@.
at :: Level -> Era -> Cell -> Bool -> Game
at level era cell hasCell =
  (startPlaying level) { gEra = era, gCell = cell, gHasCell = hasCell, gPrev = noInput }

-- | One whole step in direction @d@ (or a push, or a jump through a mirror, whatever
-- that key does from here): 'applyAction' presses the key for one tick, then ticks
-- with no keys until the keeper stands still, so the move has landed and been paid
-- for. A refused key leaves the keeper standing at once, so no idle tick follows,
-- and the key is still recorded as held in 'gPrev': walking the same way again
-- straight after then counts as holding the key, not as a fresh press.
--
-- Point-free: @applyAction level . Go@ is the composition "wrap the direction in
-- 'Go', then apply the action", so @walk level d = applyAction level (Go d)@.
walk :: Level -> Dir -> Game -> Game
walk level = applyAction level . Go

-- | One whole shift of era, as 'walk' does a step: Space for one tick, then idle
-- ticks until the input lock of the wave is over.
shift :: Level -> Game -> Game
shift level = applyAction level ShiftEra

-- | What a game holds that the player can tell apart, ignoring time.
--
-- It is the solver's 'StateKey': the era, the keeper's cell, whether they carry the
-- power cell, the phase, the crates, the filled pits, the footprints, the echo and
-- the movable mirrors. Time left, the history, the motion under way, messages and
-- last tick's keys are not in it, so two games with equal essentials are the same
-- board, however they got there.
essentials :: Game -> StateKey
essentials = stateKey

-- | Parse the fixture, print every check, print the report, then the totals.
--
-- @case theLevel of@ looks at the 'Either' the parser returns: 'Left' with a
-- message if the map is malformed, which stops the suite at once ('>>' runs one IO
-- action and then the next), or 'Right' with the level. In the do block, 'forM'
-- runs the lambda for every check in order; the lambda's pattern @(Check name ok)@
-- takes the check apart, and printing it forces @ok@, which is when the check's
-- work is actually done. @results@ is the list of all the 'Bool's.
main :: IO ()
main = case theLevel of
  Left err -> putStrLn ("level does not parse: " ++ err) >> exitFailure
  Right level -> do
    results <- forM (allChecks level) $ \(Check name ok) -> do
      putStrLn ((if ok then "  ok    " else "  FAIL  ") ++ name)
      pure ok
    report level
    -- 'let' in a do block names a value for the lines after it.
    let failed = length (filter not results)
    putStrLn (show (length results - failed) ++ " passed, " ++ show failed ++ " failed")
    -- A non-zero exit code is what tells cabal the suite failed.
    unless (failed == 0) exitFailure

-- | Numbers worth reading in the log, measured on the implementation.
--
-- For the fixture: how many states the solver reaches and how many are dead ends,
-- the cheapest route's cost (in seconds), length and shifts, and that route as keys
-- (W A S D and Space); then how many ticks the intended route takes and the state it
-- ends in. Then one line per campaign level, and the secret level's ('reportDuo').
-- Nothing here passes or fails; the README's "Test results" is a copy of it. The
-- searches here repeat ones the checks already made, since nothing is shared
-- between this function and them, so the report adds several seconds to the run.
report :: Level -> IO ()
report level = do
  -- 'search' takes a filter on (game, action) saying which actions may be tried;
  -- @\_ _ -> True@ ignores both arguments and allows every action everywhere.
  let start = startPlaying level
      result = search level (\_ _ -> True) start
      route = routeInputs intendedRoute
      end = run level route start
  putStrLn ""
  -- 'srStates' maps every reachable state to its cheapest cost; 'Map.size' counts
  -- its entries.
  putStrLn ("search: " ++ show (Map.size (srStates result)) ++ " reachable states, "
            ++ show (length (srDeadEnds result)) ++ " dead ends")
  -- 'srGoal' is 'Nothing' if no state wins, or the cheapest cost and a route to it.
  case srGoal result of
    Nothing -> putStrLn "search: NO ROUTE"
    Just (n, actions) -> do
      putStrLn ("search: cheapest route costs " ++ show n ++ " s, " ++ show (length actions)
                ++ " actions, " ++ show (length (filter (== ShiftEra) actions)) ++ " of them shifts")
      putStrLn ("search: " ++ unwords (map actionLabel actions))
  -- The state is printed with the 'Show' instance 'StateKey' derives, field by field.
  putStrLn ("route:  intended route takes " ++ show (length route) ++ " ticks, ends "
            ++ show (essentials end))
  -- 'mapM_' runs an IO action for each element and keeps no results.
  case campaign of
    Left _ -> pure ()
    Right levels -> mapM_ reportLevel levels
  reportDuo
  putStrLn ""
  -- 'where' scopes helpers over the definition above it; a let scopes them over
  -- its following expression/block. Both can define functions with guards.
  where
    -- A pattern guard, @| Just known <- knownRoute l@: this equation applies only if
    -- 'knownRoute' gives a 'Just', and then @known@ names what is inside it.
    -- Otherwise the next equation is tried. A level has a pinned route when it is
    -- too big to search on every run; at present only The Long Reflection, kept in
    -- 'extraLevels', has one, so every campaign level takes the second equation.
    reportLevel l | Just known <- knownRoute l =
      putStrLn ("level:  " ++ lvName l ++ ": par " ++ show (lvPar l)
                ++ ", budget " ++ show (lvBudget l) ++ ", "
                ++ show (length known) ++ " actions, "
                ++ show (length (filter (== ShiftEra) known)) ++ " shifts, too big to search here, "
                ++ show (sharedPercent l) ++ "% shared")
    -- Any other level is searched in full. The par printed is the one the solver
    -- finds, not the one written in the level, so a level whose numbers drifted
    -- shows it here (and fails 'levelChecks').
    reportLevel l =
      -- @maybe d f m@ is @d@ when @m@ is 'Nothing' and @f x@ when it is @Just x@.
      -- @length . filter (== ShiftEra) . snd@ reads right to left: take the route
      -- out of the (cost, route) pair, keep its shifts, count them.
      let r = search l (\_ _ -> True) (startPlaying l)
          shifts = maybe 0 (length . filter (== ShiftEra) . snd) (srGoal r)
      in putStrLn ("level:  " ++ lvName l ++ ": par " ++ maybe "NONE" (show . fst) (srGoal r)
                   ++ ", budget " ++ show (lvBudget l)
                   ++ ", " ++ maybe "0" (show . length . snd) (srGoal r) ++ " actions, "
                   ++ show shifts ++ " shifts, "
                   ++ show (Map.size (srStates r)) ++ " states, "
                   ++ show (length (srDeadEnds r)) ++ " dead, "
                   ++ show (sharedPercent l) ++ "% shared")
    -- Of the cells open in either era, the share open in both, as a whole percentage.
    -- A rough measure of how much the two maps differ: the lower it is, the more the
    -- era matters. 'div' written in backquotes is used as an infix operator; it is
    -- integer division, rounding down.
    sharedPercent l =
      let both = Set.size (Set.intersection (lvPast l) (lvPresent l))
          every = Set.size (Set.union (lvPast l) (lvPresent l))
      in 100 * both `div` every

-- | Every check, in the order they are printed. Each group is a list of checks,
-- either built from the fixture @level@ or standing alone with its own maps;
-- 'concat' joins the lists into one. Building this list does no work: each check's
-- condition waits, unevaluated, until 'main' prints that check.
allChecks :: Level -> [Check]
allChecks level = concat
  [ dimensions level
  , parserChecks
  , collision level
  , shifting level
  , powerCell level
  , lighthouse level
  , restartAndPause level
  , timing level
  , undoChecks level
  , crateChecks
  , plateChecks
  , echoChecks level
  , mirrorChecks
  , campaignChecks
  , deadSearchChecks
  , pendingShifts level
  , routeChecks level
  , determinism level
  , inputAdapter level
  , fxChecks level
  , finaleChecks
  , duoChecks
  , cornerChecks
  , twoKeepersChecks
  , soundChecks level
  , introChecks
  , storyChecks
  , ordersChecks
  ]

-- 1. ---------------------------------------------------------------------------
-- | Item 1: the fixture's map is the one the brief describes. Its size, where the
-- spawn, the power cell and the lighthouse are, and that its only differences
-- between the eras are the two passages, (4,2) open only in the past and (8,6) open
-- only in the present. Every later numbered check leans on these coordinates.
dimensions :: Level -> [Check]
dimensions level =
  [ check "1. maps are 13 x 9" (lvWidth level == 13 && lvHeight level == 9)
  -- The rows as written in Game.Level, before parsing. @(== 13)@ is an operator
  -- section, the function "is equal to 13", and @(== 13) . length@ composes it
  -- after 'length': "is this row 13 long?".
  , check "1. both source maps have 9 rows of 13"
      (all (\rows -> length rows == 9 && all ((== 13) . length) rows) [pastRows, presentRows])
  , check "1. spawn, cell and lighthouse are where the brief says"
      (lvSpawn level == (2, 6) && lvPowerCell level == (10, 2) && lvGoal level == (2, 2))
  -- 'passageCells' lists the cells open in exactly one era, past ones first, so this
  -- says the maps differ in these two cells and nowhere else.
  , check "1. passages are (4,2) in the past and (8,6) in the present"
      (passageCells level == [(Past, (4, 2)), (Present, (8, 6))])
  -- A marker on the map (B, G, S) stands on floor: it adds nothing solid.
  , check "1. markers are walkable"
      (walkable level Past (10, 2) && walkable level Present (2, 2) && walkable level Present (2, 6))
  ]

-- | The level parser refuses malformed maps. Each check takes the fixture's rows
-- ('pastRows', 'presentRows'), breaks one thing, and expects 'parseLevel' to return
-- 'Left' (an error message) rather than 'Right' (a level). The last two parse good
-- maps and read back what the parser made of plates, doors, crates and pits.
--
-- A bad level must fail when the game starts, with a reason, never half work in
-- play; and the rules count on what the parser promises, such as a sealed border
-- (so no keeper walks off the map) and a crate only where both eras have floor.
--
-- Row numbers in @replaceAt@ count from 0 at the top, so @replaceAt 3 row@ replaces
-- the cells with y = 3.
parserChecks :: [Check]
parserChecks =
  [ check "parser: rejects a missing row" (isLeft (parse (drop 1 pastRows) presentRows))
  -- 'init' drops a row's last character; @(++ "#")@ adds one. Only the top row is
  -- changed, so the rows disagree on the width.
  , check "parser: rejects a short row" (isLeft (parse pastRows (mapHead init presentRows)))
  , check "parser: rejects a long row" (isLeft (parse (mapHead (++ "#") pastRows) presentRows))
  , check "parser: rejects an unknown character"
      (isLeft (parse pastRows (replaceAt 1 "#.?.#...#...#" presentRows)))
  -- The fixture's own cell is at (10,2); this adds another at (2,3).
  , check "parser: rejects a second power cell"
      (isLeft (parse (replaceAt 3 "#.B.#...#...#" pastRows) presentRows))
  -- Row 6 without its S at (2,6).
  , check "parser: rejects a missing spawn"
      (isLeft (parse pastRows (replaceAt 6 "#...#.......#" presentRows)))
  -- The lighthouse belongs on the present map; on the past map G is not a known mark.
  , check "parser: rejects the goal in the wrong era"
      (isLeft (parse (replaceAt 1 "#.G.#...#...#" pastRows) presentRows))
  -- Floor at (0,3) in the outer wall: the border must be solid all round, or a
  -- keeper could walk off the map.
  , check "parser: rejects an open border"
      (isLeft (parse (replaceAt 3 "....#...#...#" pastRows) presentRows))
  , check "parser: rejects an empty map" (isLeft (parse [] []))
  -- Crates and pits are written on the past map only: a crate has one position for
  -- both eras, and a pit is a hole in the past's floor.
  , check "parser: rejects a crate on the present map"
      (isLeft (parse pastRows (replaceAt 1 "#.C.#...#...#" presentRows)))
  , check "parser: rejects a pit on the present map"
      (isLeft (parse pastRows (replaceAt 1 "#.~.#...#...#" presentRows)))
  , check "parser: rejects a pit in the border"
      (isLeft (parse (replaceAt 3 "~...#...#...#" pastRows) presentRows))
  -- A crate at (2,6), where the keeper starts.
  , check "parser: rejects a crate on the spawn"
      (isLeft (parse (replaceAt 6 "#.C.#...#...#" pastRows) presentRows))
  -- The crate at (2,3) is on past floor, but the present row makes (2,3) a wall, and
  -- a crate stands in both eras.
  , check "parser: rejects a crate inside a present wall"
      (isLeft (parse (replaceAt 3 "#.C.#...#...#" pastRows) (replaceAt 3 "###.#...#...#" presentRows)))
  -- Plates and doors come together or not at all.
  , check "parser: rejects a plate with no door"
      (isLeft (parse (replaceAt 3 "#.P.#...#...#" pastRows) presentRows))
  , check "parser: rejects a door with no plate"
      (isLeft (parse pastRows (replaceAt 3 "#.D.#...#...#" presentRows)))
  -- A plate at (2,3) with its door at (2,5), two rows below so not beside it, but
  -- the present has a wall at (2,3), where no crate could rest on the plate.
  , check "parser: rejects a plate in a present wall"
      (isLeft (parse (replaceAt 3 "#.P.#...#...#" pastRows)
                     (replaceAt 3 "###.#...#...#" (replaceAt 5 "#.D.#...#...#" presentRows))))
  , check "parser: rejects a door on the past map and a plate on the present map"
      (isLeft (parse (replaceAt 3 "#.D.#...#...#" pastRows) presentRows)
       && isLeft (parse pastRows (replaceAt 3 "#.P.#...#...#" presentRows)))
  , check "parser: rejects a door in the border"
      (isLeft (parse (replaceAt 3 "#.P.#...#...#" pastRows) (replaceAt 3 "D...#...#...#" presentRows)))
  -- A good map this time: a plate at (2,3), a door at (2,5). The plate is floor in
  -- both eras, so a crate can rest on it; the door is a wall on the map, and only
  -- 'doorsOpen' (worked out from the board) lets anyone through.
  , check "parser: reads a plate as floor in both eras, and a door as shut" $
      case parse (replaceAt 3 "#.P.#...#...#" pastRows) (replaceAt 5 "#.D.#...#...#" presentRows) of
        Left _ -> False
        Right l -> lvPlates l == Set.fromList [(2, 3)] && lvDoors l == Set.fromList [(2, 5)]
                   && walkable l Past (2, 3) && walkable l Present (2, 3)
                   && not (walkable l Present (2, 5))
  -- Crates at (2,3) and (6,4), a pit at (6,3). A crate's cell stays floor on the
  -- map, since the crate itself lives in the game's 'gCrates' and moves; a pit is
  -- not floor until a crate fills it.
  , check "parser: a crate stands on floor; a pit is not floor" $
      case parse (replaceAt 3 "#.C.#.~.#...#" (replaceAt 4 "#...#.C.#...#" pastRows)) presentRows of
        Left _ -> False
        Right l -> lvCrates l == Set.fromList [(2, 3), (6, 4)] && lvPits l == Set.fromList [(6, 3)]
                   && walkable l Past (2, 3) && not (walkable l Past (6, 3))
  ]
  where
    -- The budget, 100 seconds, does not matter to any of these.
    parse = parseLevel 100
    isLeft e = case e of
      Left _  -> True
      Right _ -> False
    -- Change the first row only.
    mapHead f (x : xs) = f x : xs
    mapHead _ []       = []
    -- Row @i@ (from 0) replaced by @row@: the rows before it, the new one, the rest.
    replaceAt i row rows = take i rows ++ [row] ++ drop (i + 1) rows

-- 2. ---------------------------------------------------------------------------
-- | Static collision in both eras: walls, borders and opposite passages. The
-- exhaustive final check guards the global invariant behind the spot examples:
-- every settled reachable position must be floor in its own era.
collision :: Level -> [Check]
collision level =
  [ check "2. a wall stops a step, in the present"
      (gCell (walk level DirRight (at level Present (3, 3) False)) == (3, 3))
  , check "2. a wall stops a step, in the past"
      (gCell (walk level DirRight (at level Past (7, 3) False)) == (7, 3))
  , check "2. the outer wall stops a step" $
      all (\(c, d) -> gCell (walk level d (at level Present c False)) == c)
        [ ((1, 1), DirUp), ((1, 1), DirLeft), ((11, 7), DirDown), ((11, 7), DirRight) ]
  , check "2. the map boundary itself is solid"
      (not (any (walkable level Present) [(-1, 3), (13, 3), (3, -1), (3, 9)]))
  , check "2. the past door is open only in the past"
      (gCell (walk level DirRight (at level Past (3, 2) False)) == (4, 2)
       && gCell (walk level DirRight (at level Present (3, 2) False)) == (3, 2))
  , check "2. the present breach is open only in the present"
      (gCell (walk level DirRight (at level Present (7, 6) False)) == (8, 6)
       && gCell (walk level DirRight (at level Past (7, 6) False)) == (7, 6))
  , check "2. a held direction into a wall never starts a step" $
      let g = run level (replicate 60 (press DirRight)) (at level Present (3, 3) False)
      in gCell g == (3, 3) && gMotion g == Settled
  , check "2. no key can reach a wall cell: every reachable state is walkable" $
      all (\k -> walkable level (skEra k) (skCell k))
        (Map.keys (srStates (search level (\_ _ -> True) (startPlaying level))))
  ]

-- 3, 4, 5. ---------------------------------------------------------------------
-- | Shift atomicity and its 13-tick animation lock. A legal shift changes era
-- and charges immediately, while a refused shift changes feedback only. These
-- checks distinguish a queued direction from Space dropped during the lock.
shifting :: Level -> [Check]
shifting level =
  [ check "3. a legal shift changes the era, charges it and starts the lock, nothing else" $
      let g = at level Present (2, 6) False
          r = tryShift level g
      in r { gHistory = [] } == g { gEra = Past, gMotion = Shifting shiftTicks
                                  , gRemaining = gRemaining g - 6 }
         && map snCost (gHistory r) == [6]
  , check "3. the player's cell survives a shift both ways" $
      let g = shift level (shift level (at level Present (5, 3) False))
      in gCell g == (5, 3) && gEra g == Present
  , check "3. the wave itself spends nothing beyond the shift's price" $
      let g = run level (space : replicate (shiftTicks + 20) noInput) (at level Present (2, 6) False)
      in gRemaining g == lvBudget level - 6
  , check "3. nothing moves or shifts during the wave" $
      let g0 = run level [space] (at level Present (2, 6) False)
          g1 = run level [noInput, space, press DirUp, space] g0
      in gEra g1 == Past && gCell g1 == (2, 6)
         && gMotion g1 == Shifting (shiftTicks - 4)
  , check "3. a direction tapped during the wave waits, then walks" $
      let g0 = run level [space, press DirUp] (at level Present (2, 6) False)
          during = idle level (shiftTicks - 2) g0
          after = idle level 1 during
      in gCell during == (2, 6) && gMotion during == Shifting 1
         && gMotion after == Stepping (2, 5) stepTicks
  , check "4. shifting into a wall is rejected: past door" $
      gEra (tryShift level (at level Past (4, 2) False)) == Past
  , check "4. shifting into a wall is rejected: present breach" $
      gEra (tryShift level (at level Present (8, 6) False)) == Present
  , check "4. a rejected shift says which cell blocked it" $
      fmap fbKind (gFeedback (tryShift level (at level Past (4, 2) False)))
        == Just (ShiftBlocked (4, 2))
  , check "4. rejected-shift feedback fades on its own" $
      gFeedback (idle level blockedFeedbackTicks (tryShift level (at level Past (4, 2) False)))
        == Nothing
  , check "5. a rejected shift keeps era, position and inventory" $
      let g = at level Past (4, 2) True
          r = tryShift level g
      in r == g { gFeedback = gFeedback r } && essentials r == essentials g
  , check "5. a rejected shift keeps era, position and inventory, by key too" $
      let g = at level Present (8, 6) False
          r = run level [space] g
      in essentials r == essentials g && gMotion r == Settled
  ]

-- 6, 7, 8. ---------------------------------------------------------------------
-- | Pickup is tied to arrival/era change, not the start of motion. Once carried,
-- the wick persists through shifts and returning to its cradle cannot collect
-- it twice; this prevents repeated feedback and duplicated inventory.
powerCell :: Level -> [Check]
powerCell level =
  [ check "6. stepping on the cell in the past collects it" $
      gHasCell (walk level DirRight (at level Past (9, 2) False))
  , check "6. stepping on the same spot in the present does not" $
      not (gHasCell (walk level DirRight (at level Present (9, 2) False)))
  , check "6. shifting into the past on the spot collects it" $
      gHasCell (shift level (walk level DirRight (at level Present (9, 2) False)))
  , check "6. pickup happens when the step lands, not when it starts" $
      let g = run level [press DirRight] (at level Past (9, 2) False)
      in not (gHasCell g) && gHasCell (idle level stepTicks g)
  , check "7. it is collected once: coming back makes no second pickup" $
      let collected = walk level DirRight (at level Past (9, 2) False)
          quietened = collected { gFeedback = Nothing }
          back = walk level DirRight (walk level DirLeft quietened)
      in gHasCell collected && fmap fbKind (gFeedback collected) == Just CellCollected
         && gCell back == (10, 2) && gHasCell back && gFeedback back == Nothing
  , check "8. the cell travels through every shift" $
      let g0 = walk level DirRight (at level Past (9, 2) False)
          steps = take 7 (iterate (shift level) g0)
      in all gHasCell steps && map gEra steps == take 7 (cycle [Past, Present])
  ]

-- 9. ---------------------------------------------------------------------------
-- | Victory needs both the present era and the carried wick at the lamp.
-- Walking and shifting must call the same arrival rule; hints/objective text
-- are checked alongside those state changes so they describe the actual goal.
lighthouse :: Level -> [Check]
lighthouse level =
  [ check "9. present and cell: entering wins" $
      gPhase (walk level DirLeft (at level Present (3, 2) True)) == Won
  , check "9. present without the cell: entering does not win" $
      gPhase (walk level DirLeft (at level Present (3, 2) False)) == Playing
  , check "9. past with the cell: the same spot is ordinary floor" $
      let g = walk level DirLeft (at level Past (3, 2) True)
      in gPhase g == Playing && gCell g == (2, 2)
  , check "9. shifting to the present on the spot, with the cell, wins" $
      gPhase (run level [space] (at level Past (2, 2) True)) == Won
  , check "9. shifting to the present on the spot, without it, does not" $
      gPhase (run level [space] (at level Past (2, 2) False)) == Playing
  , check "9. hints: dark lamp, then footing, then none" $
      currentHint level (at level Present (2, 2) False) == Just HintLampDark
      && currentHint level (at level Past (2, 2) True) == Just HintOnlyFloor
      && currentHint level (at level Past (2, 2) False) == Nothing
  , check "9. objective text changes on pickup" $
      objectiveText (at level Present (2, 6) False) == "Find the power cell in the past."
      && objectiveText (at level Present (2, 6) True) == "Bring it to the lighthouse in the present."
      && objectiveText (walk level DirLeft (at level Present (3, 2) True)) == "The lighthouse is lit."
  ]

-- 10, 11. ----------------------------------------------------------------------
-- | Restart resets the board from any terminal/pause state, while pause preserves
-- an unfinished move. Held keys and queued actions must not leak across either
-- transition; comparisons ignore only the deliberate input/attempt bookkeeping.
restartAndPause :: Level -> [Check]
restartAndPause level =
  [ check "10. restart restores the level from a messy state" $
      let messy = run level [press DirUp, noInput { tiDir = Just DirUp, tiShift = True }]
                    (at level Past (9, 3) True)
          r = run level [restartKey] messy
      in r { gPrev = noInput, gAttempt = 0 } == (initialGame level) { gPhase = Playing }
         && gAttempt r == gAttempt messy + 1
  , check "10. restart restores it from won, lost and paused too" $
      let won    = walk level DirLeft (at level Present (3, 2) True)
          lost   = (at level Past (5, 5) True) { gPhase = Lost, gRemaining = 0 }
          paused = run level [pauseKey] (at level Past (5, 5) True)
          fresh  = (initialGame level) { gPhase = Playing }
          again g = (run level [restartKey] g) { gPrev = noInput, gAttempt = 0 }
      in map gPhase [won, lost, paused] == [Won, Lost, Paused]
         && map again [won, lost, paused] == [fresh, fresh, fresh]
  , check "10. Enter retries from won and lost, but does nothing while playing" $
      let won  = walk level DirLeft (at level Present (3, 2) True)
          lost = (at level Past (5, 5) True) { gPhase = Lost, gRemaining = 0 }
          playing = idle level 5 (at level Past (5, 5) True)
      in gPhase (run level [enter] won) == Playing && gRemaining (run level [enter] lost) == lvBudget level
         && gRemaining (run level [enter] playing) == gRemaining playing
         && gPhase (run level [enter] playing) == Playing
  , check "10. a key held through restart does not act until pressed again" $
      let held = noInput { tiDir = Just DirUp, tiRestart = True }
          g = run level (held : replicate 20 (press DirUp)) (at level Past (9, 5) True)
          g' = run level [noInput, press DirUp] g
      in gCell g == lvSpawn level && gMotion g == Settled
         && gMotion g' == Stepping (2, 5) stepTicks
  , check "10. the title screen ignores everything but Enter" $
      let g = initialGame level
      in run level [space, press DirUp, restartKey, pauseKey] g
           == g { gPrev = pauseKey }
  , check "11. pausing spends no time" $
      let g0 = at level Present (2, 6) False
          g1 = run level [pauseKey] g0
          g2 = idle level 500 g1
      in gPhase g1 == Paused && gRemaining g2 == gRemaining g0 && gPhase g2 == Paused
  , check "11. resuming spends no time either" $
      let g = run level ([pauseKey, noInput, pauseKey] ++ replicate 10 noInput)
                (at level Present (2, 6) False)
      in gPhase g == Playing && gRemaining g == lvBudget level
  , check "11. a step in progress freezes and finishes after resume" $
      let g0 = run level [press DirUp, noInput, pauseKey] (at level Present (2, 6) False)
          frozen = idle level 100 g0
          done = idle level (stepTicks - 1) (run level [pauseKey] frozen)
      in gMotion frozen == gMotion g0 && gCell done == (2, 5)
  , check "11. pause clears pending actions" $
      let g0 = run level [press DirUp, noInput { tiShift = True }, noInput
                         , noInput { tiDir = Just DirLeft }] (at level Present (2, 6) False)
          g1 = run level [pauseKey] g0
      in gPendingShift g0 && gPendingMove g0 == Just DirLeft
         && not (gPendingShift g1) && gPendingMove g1 == Nothing
  , check "11. a shift pressed while paused does nothing on resume" $
      let g = run level [pauseKey, space, noInput, pauseKey, noInput]
                (at level Present (2, 6) False)
      in gEra g == Present
  ]

-- 12, 13. ----------------------------------------------------------------------
-- | Budget accounting and terminal ordering, in seconds rather than tick time.
-- Check both sides of each affordability boundary: refused actions are free,
-- accepted actions charge once, and a last affordable arrival can still win.
-- Long idle/held-input traces catch accidental per-tick draining or overspending.
timing :: Level -> [Check]
timing level =
  [ check "12. the prices are 2 s a step and 6 s a shift, and the budget is par + 20" $
      lvRules level == Rules 2 6 50 4 0 4
      && routeCost == 108 && lvBudget level == routeCost + 20
  , check "12. standing still spends nothing, for as long as you like" $
      let g = idle level 100000 (startPlaying level)
      in gPhase g == Playing && gRemaining g == lvBudget level
  , check "12. the budget does not start on the title screen" $
      gRemaining (idle level 100 (initialGame level)) == lvBudget level
  , check "12. a step is charged once, the moment it is accepted" $
      let g0 = at level Present (2, 6) False
          g1 = run level [press DirUp] g0
          g2 = idle level (stepTicks + 10) g1
      in gRemaining g1 == gRemaining g0 - 2 && gRemaining g2 == gRemaining g0 - 2
  , check "12. a step into a wall is free" $
      let g0 = at level Present (3, 3) False
      in gRemaining (walk level DirRight g0) == gRemaining g0
  , check "12. a shift refused by a wall is free" $
      let g0 = at level Past (4, 2) False
      in gRemaining (run level [space] g0) == gRemaining g0
  , check "12. holding into a wall never drains time" $
      let g0 = at level Present (3, 3) False
      in gRemaining (run level (replicate 300 (press DirRight)) g0) == gRemaining g0
  , check "12. the time stops after a win" $
      let g = walk level DirLeft (at level Present (3, 2) True)
      in gRemaining (idle level 100 g) == gRemaining g
  , check "12. nothing moves after a loss" $
      let g = run level [noInput] (at level Present (5, 3) False) { gRemaining = 1 }
          g' = run level [press DirUp, noInput, space, noInput] g
      in gPhase g == Lost && essentials g' == essentials g && gRemaining g' == 1
  , check "13. with less than a step's price left, the level is lost" $
      gPhase (run level [noInput] (at level Present (5, 3) False) { gRemaining = 1 }) == Lost
  , check "13. with exactly a step's price left, it is not, and the step spends it" $
      let g0 = (at level Present (2, 6) False) { gRemaining = 2 }
          g1 = run level [noInput] g0
          g2 = run level [press DirUp] g1
          g3 = idle level (stepTicks - 1) g2
          g4 = idle level 1 g3
      in gPhase g1 == Playing && gRemaining g2 == 0 && gPhase g3 == Playing
         && gPhase g4 == Lost && gCell g4 == (2, 5)
  , check "13. a shift you cannot afford is refused for free, with its own message" $
      let g0 = (at level Present (2, 6) False) { gRemaining = 5 }
          g1 = run level [space] g0
      in gEra g1 == Present && gRemaining g1 == 5 && gPhase g1 == Playing
         && fmap fbKind (gFeedback g1) == Just ShiftTooCostly && null (gHistory g1)
  , check "13. a wall beats the price in explaining a refused shift" $
      let g = run level [space] (at level Past (4, 2) False) { gRemaining = 5 }
      in fmap fbKind (gFeedback g) == Just (ShiftBlocked (4, 2))
  , check "13. a shift with exactly its price left is accepted, then the level ends" $
      let g0 = (at level Present (2, 6) False) { gRemaining = 6 }
          g1 = run level [space] g0
          g2 = idle level shiftTicks g1
      in gEra g1 == Past && gRemaining g1 == 0 && gPhase g1 == Playing && gPhase g2 == Lost
  , check "13. a step landing on the goal on the last second wins" $
      let g = (at level Present (3, 2) True) { gRemaining = 2 }
          g' = walk level DirLeft g
      in gPhase g' == Won && gRemaining g' == 0
  , check "13. a shift onto the goal on the last second wins" $
      let g = (at level Past (2, 2) True) { gRemaining = 6 }
          g' = run level [space] g
      in gPhase g' == Won && gRemaining g' == 0
  , check "13. a held key cannot walk past zero" $
      let trail = scanl (flip (stepGame level)) (at level Present (2, 6) False) { gRemaining = 5 }
                    (replicate 200 (press DirUp))
      in all ((>= 0) . gRemaining) trail && gRemaining (last trail) == 1
         && gPhase (last trail) == Lost && gMotion (last trail) == Settled
  , check "13. a buffered key cannot walk past zero" $
      let g0 = (at level Present (2, 6) False) { gRemaining = 2 }
          trail = scanl (flip (stepGame level)) g0
                    ([press DirUp, noInput, press DirLeft, noInput, space, noInput]
                     ++ replicate 40 noInput)
      in all ((>= 0) . gRemaining) trail && gPhase (last trail) == Lost
         && gRemaining (last trail) == 0 && gCell (last trail) == (2, 5)
  , check "13. the full route wins with exactly its cost, and loses with less" $
      let route = routeInputs intendedRoute
          withTime n = (startPlaying level) { gRemaining = n }
          end n = run level route (withTime n)
      in gPhase (end routeCost) == Won && gRemaining (end routeCost) == 0
         && gPhase (end (routeCost - 1)) == Lost && gPhase (end (routeCost - 2)) == Lost
         && gRemaining (end (lvBudget level)) == 20
  , check "13. the intended route is as cheap as any: the solver finds nothing better" $
      fmap fst (srGoal (search level (\_ _ -> True) (startPlaying level))) == Just routeCost
  , check "13. a refusal on screen does not stay pinned to the win or lost screen" $
      let costly g = run level [space] g
          won = walk level DirLeft (costly (at level Present (3, 2) True) { gRemaining = 2 })
          lost = idle level 1 (walk level DirLeft (costly (at level Present (3, 3) False) { gRemaining = 2 }))
      in gPhase won == Won && gFeedback won == Nothing
         && gPhase lost == Lost && gFeedback lost == Nothing
  , check "13. Space during a shift's lock is dropped, not saved for later" $
      let g0 = run level [space, noInput, space, noInput] (at level Present (2, 6) False)
          g1 = idle level (shiftTicks + 5) g0
      in not (gPendingShift g0) && gEra g1 == Past && gRemaining g1 == lvBudget level - 6
  ]
  where
    -- This fixture route uses only steps and shifts. The local price sum is an
    -- independent expectation (108 s), not a replacement for the rule's charge.
    routeCost = sum (map price intendedRoute)
    price a = if a == ShiftEra then 6 else 2 :: Int

-- Crates and pits. -------------------------------------------------------------

-- | A small yard: the crate at (2,1), a pit at (6,1) that is floor in the present,
-- and open floor everywhere else.
yard :: Level
yard = case parseLevel 1000
  [ "#########"
  , "#.C...~.#"
  , "#..B....#"
  , "#.......#"
  , "#########"
  ]
  [ "#########"
  , "#S......#"
  , "#.......#"
  , "#......G#"
  , "#########"
  ] of
    Right l -> l
    Left err -> error err

-- | Shared crate placement, fresh-press pushing and permanent pit filling.
-- Artificial board edits isolate obstructions in either era; checks compare
-- position, cost and feedback to ensure a refusal cannot partly mutate the board.
-- Later checks cover solver identity and undo, since losing crate/fill state in
-- either would make a visually plausible game obey different rules afterwards.
crateChecks :: [Check]
crateChecks =
  [ check "crate: in the present it is solid, and walking into it is free" $
      let g0 = at yard Present (1, 1) False
          g = walk yard DirRight g0
      in gCell g == (1, 1) && gCrates g == lvCrates yard && gRemaining g == gRemaining g0
  , check "crate: in the past, walking into it pushes it one cell, for one step's price" $
      let g0 = at yard Past (1, 1) False
          g = walk yard DirRight g0
      in gCell g == (2, 1) && gCrates g == Set.fromList [(3, 1)]
         && gRemaining g == gRemaining g0 - 2 && length (gHistory g) == 1
  , check "crate: it moves when the push is accepted, and says where it went for drawing" $
      let g0 = at yard Past (1, 1) False
          g1 = run yard [press DirRight] g0
          g2 = idle yard stepTicks g1
      in gCrates g1 == Set.fromList [(3, 1)] && gPushing g1 == Just (3, 1)
         && gCell g1 == (1, 1) && gPushing g2 == Nothing && gCell g2 == (2, 1)
  , check "crate: it cannot be pulled: walking away leaves it behind" $
      let g = walk yard DirRight (at yard Past (3, 1) False)
      in gCell g == (4, 1) && gCrates g == lvCrates yard
  , check "crate: a wall in the present stops the push, says why, and is free" $
      let walled = yard { lvPresent = Set.delete (4, 1) (lvPresent yard) }
          g0 = (at walled Past (2, 1) False) { gCrates = Set.fromList [(3, 1)] }
          g = walk walled DirRight g0
      in gCell g == (2, 1) && gCrates g == gCrates g0 && gRemaining g == gRemaining g0
         && fmap fbKind (gFeedback g) == Just (PushBlocked (4, 1))
  , check "crate: a plain wall in the past needs no explaining" $
      let g0 = (at yard Past (1, 2) False) { gCrates = Set.fromList [(1, 3)] }
      in gFeedback (walk yard DirDown g0) == Nothing
  , check "crate: holding a key against that present wall stays silent" $
      let walled = yard { lvPresent = Set.delete (4, 1) (lvPresent yard) }
          g0 = (at walled Past (1, 1) False) { gCrates = Set.fromList [(3, 1)] }
          g = run walled (replicate 40 (press DirRight)) g0
      in gCell g == (2, 1) && gCrates g == gCrates g0 && gFeedback g == Nothing
  , check "pit: a crate can fall in even where the present is a wall, leaving nothing in it" $
      let walled = yard { lvPresent = Set.delete (6, 1) (lvPresent yard) }
          g0 = (at walled Past (4, 1) False) { gCrates = Set.fromList [(5, 1)] }
          g = walk walled DirRight g0
      in Set.null (gCrates g) && gFilled g == Set.fromList [(6, 1)]
  , check "pit: once filled, a second crate cannot rest there if the present is a wall" $
      let walled = yard { lvPresent = Set.delete (6, 1) (lvPresent yard) }
          g0 = (at walled Past (4, 1) False)
                 { gCrates = Set.fromList [(5, 1)], gFilled = Set.fromList [(6, 1)] }
          g = walk walled DirRight g0
      in gCrates g == gCrates g0 && fmap fbKind (gFeedback g) == Just (PushBlocked (6, 1))
  -- Feedback distinguishes an available push requiring a new press from rust or
  -- an impossible push; holding must not repeatedly spend or restart messages.
  , check "hint: holding a key against a crate that could move says to press again" $
      let g = run yard (replicate 60 (press DirRight)) (at yard Past (1, 1) False)
      in gCell g == (2, 1) && fmap fbKind (gFeedback g) == Just PushNeedsPress
  , check "hint: the first press pushes, with nothing to say" $
      gFeedback (walk yard DirRight (at yard Past (1, 1) False)) == Nothing
  , check "hint: it clears as soon as the player does something" $
      let held = run yard (replicate 60 (press DirRight)) (at yard Past (1, 1) False)
          next = walk yard DirDown (run yard [noInput] held)
          pushed = walk yard DirRight (run yard [noInput] held)
      in gFeedback next == Nothing && gFeedback pushed == Nothing
  , check "hint: pressing against a crate in the present says it only moves in the past" $
      let g = walk yard DirRight (at yard Present (1, 1) False)
      in fmap fbKind (gFeedback g) == Just CrateRusted
  , check "hint: holding against a crate in the present stays quiet after the first press" $
      let g0 = at yard Present (1, 1) False
          g = run yard (replicate 200 (press DirRight)) g0
      in gRemaining g == gRemaining g0 && gFeedback g == Nothing
  -- Crossing markers are a query of the live board, not just static map overlap.
  -- Compare them with real shifts over every currently open yard cell.
  , check "crossings: open in both eras, and never under a crate" $
      let g = (at yard Present (1, 1) False) { gCrates = Set.fromList [(2, 1)] }
          cells = crossings yard g
      in (1, 1) `elem` cells && (2, 2) `elem` cells && (2, 1) `notElem` cells
         && (0, 0) `notElem` cells
  , check "crossings: a pit opens up as one once a crate fills it" $
      let g = at yard Past (4, 1) False
      in (6, 1) `notElem` crossings yard g
         && (6, 1) `elem` crossings yard g { gFilled = Set.fromList [(6, 1)] }
  , check "crossings: a door is one only while it is open" $
      let g = at gatedYard Past (4, 1) False
      in (5, 2) `notElem` crossings gatedYard g { gCrates = Set.empty }
         && (5, 2) `elem` crossings gatedYard g { gCrates = Set.fromList [(4, 3)] }
  , check "crossings: they agree with what a shift actually does" $
      let g = (at gatedYard Past (1, 1) False) { gCrates = Set.fromList [(2, 1)] }
          cells = [ (x, y) | x <- [1 .. 7], y <- [1 .. 3], openCell gatedYard g Past (x, y) ]
          shifts c = gEra (run gatedYard [space] g { gCell = c }) == Present
      in all (\c -> shifts c == (c `elem` crossings gatedYard g)) cells
  , check "peek: Tab changes nothing the rules can see" $
      let g = at yard Past (1, 1) False
          peek = noInput { tiPeek = True }
      in (run yard (replicate 30 peek) g) { gPrev = noInput } == g
  -- Failed destinations, occupied cells and held keys all stop a push before
  -- charging. A genuinely new buffered press is the deliberate exception.
  , check "crate: a wall stops the push, and the refused push is free" $
      let g0 = (at yard Past (1, 2) False) { gCrates = Set.fromList [(1, 3)] }
          g = walk yard DirDown g0
      in gCell g == (1, 2) && gCrates g == gCrates g0 && gRemaining g == gRemaining g0
  , check "crate: another crate stops the push" $
      let crates = Set.fromList [(2, 1), (3, 1)]
          g0 = (at yard Past (1, 1) False) { gCrates = crates }
          g = walk yard DirRight g0
      in gCell g == (1, 1) && gCrates g == crates && gRemaining g == gRemaining g0
  , check "crate: holding a key pushes once, then stops against the crate" $
      let g0 = at yard Past (1, 1) False
          g = run yard (replicate 60 (press DirRight)) g0
      in gCell g == (2, 1) && gCrates g == Set.fromList [(3, 1)] && gMotion g == Settled
         && gRemaining g == gRemaining g0 - 2
  , check "crate: walking while holding a key stops short of a crate" $
      let g0 = (at yard Past (1, 2) False) { gCrates = Set.fromList [(4, 2)] }
          g = run yard (replicate 60 (press DirRight)) g0
      in gCrates g == gCrates g0 && gCell g == (3, 2) && gMotion g == Settled
  , check "crate: a tap buffered during a step pushes when it lands" $
      let g0 = (at yard Past (1, 3) False) { gCrates = Set.fromList [(3, 3)] }
          g = run yard (press DirRight : noInput : press DirRight : replicate (2 * stepTicks) noInput) g0
      in gCell g == (3, 3) && gCrates g == Set.fromList [(4, 3)]
  , check "crate: nobody ever stands on one, in any reachable state" $
      all (\k -> skCell k `Set.notMember` skCrates k)
        (Map.keys (srStates (search yard (\_ _ -> True) (startPlaying yard))))
  , check "crate: it can be pushed onto the power cell, and off it, which collects it" $
      let g0 = (at yard Past (1, 2) False) { gCrates = Set.fromList [(2, 2)] }
          g = walk yard DirRight g0
          g' = walk yard DirRight g
      in gCrates g == Set.fromList [(3, 2)] && not (gHasCell g)
         && gCrates g' == Set.fromList [(4, 2)] && gHasCell g'
  , check "crate: it can be pushed onto the lighthouse's cell" $
      let g0 = (at yard Past (5, 3) False) { gCrates = Set.fromList [(6, 3)] }
          g = walk yard DirRight g0
      in gCrates g == Set.fromList [(7, 3)] && gCell g == (6, 3)
  -- A pit consumes a crate once. Test both the immediate state change and the
  -- subsequent traversal, second crate, restart and undo that rely on it.
  , check "pit: it stops a step in the past, and a shift from the present" $
      let walkIn = walk yard DirRight (at yard Past (5, 1) False)
          shiftIn = run yard [space] (at yard Present (6, 1) False)
      in gCell walkIn == (5, 1) && gEra shiftIn == Present
         && fmap fbKind (gFeedback shiftIn) == Just (ShiftBlocked (6, 1))
  , check "pit: a crate pushed in is gone, the pit is floor, and the cell is a crossing" $
      let g0 = (at yard Past (4, 1) False) { gCrates = Set.fromList [(5, 1)] }
          pushed = walk yard DirRight g0
          across = walk yard DirRight pushed
          home = shift yard across
          back = shift yard home
      in Set.null (gCrates pushed) && gFilled pushed == Set.fromList [(6, 1)]
         && gCell pushed == (5, 1) && gCell across == (6, 1)
         && gEra home == Present && gCell home == (6, 1)
         && gRemaining home == gRemaining g0 - 2 - 2 - 6
         && gEra back == Past && gCell back == (6, 1)
  , check "pit: a second crate rolls over a filled pit like any floor" $
      let g0 = (at yard Past (4, 1) False)
                 { gCrates = Set.fromList [(5, 1)], gFilled = Set.fromList [(6, 1)] }
      in gCrates (walk yard DirRight g0) == Set.fromList [(6, 1)]
  , check "pit: the filled cell comes back on restart" $
      let g0 = (at yard Past (4, 1) False) { gCrates = Set.fromList [(5, 1)] }
          g = run yard [restartKey] (walk yard DirRight g0)
      in Set.null (gFilled g) && gCrates g == lvCrates yard
  , check "undo: a push comes back exactly, for half its price" $
      let g0 = at yard Past (1, 1) False
          g = run yard [undoKey] (walk yard DirRight g0)
      in g { gRemaining = gRemaining g0, gPrev = gPrev g0 } == g0
         && gRemaining g == gRemaining g0 - 1
  , check "undo: a filled pit comes back open, with its crate" $
      let g0 = (at yard Past (4, 1) False) { gCrates = Set.fromList [(5, 1)] }
          g = run yard [undoKey] (walk yard DirRight g0)
      in g { gRemaining = gRemaining g0, gPrev = gPrev g0 } == g0
  , check "solver: boards with different crates are different states" $
      let r = search yard (\_ _ -> True) (startPlaying yard)
      in Set.size (Set.fromList (map skCrates (Map.keys (srStates r)))) > 1
  , check "solver: boards with different filled pits are different states" $
      let g = startPlaying yard
      in stateKey g /= stateKey g { gFilled = Set.fromList [(6, 1)] }
  , check "route: two shifts in a row both happen when replayed through tick inputs" $
      let g = at yard Present (1, 2) False
          end = run yard (routeInputs [ShiftEra, ShiftEra, Go DirRight]) g
      in gEra end == Present && gCell end == (2, 2) && gRemaining end == gRemaining g - 14
  , check "crate: on a cell open in both eras, it closes that crossing both ways" $
      -- You cannot walk onto its cell in either era, so you cannot shift there either.
      let crates = Set.fromList [(3, 3)]
          past = (at yard Past (2, 3) False) { gCrates = crates, gRemaining = 100 }
          present = (at yard Present (2, 3) False) { gCrates = crates, gRemaining = 100 }
      in gCell (walk yard DirRight present) == (2, 3)
         && not (openCell yard past Past (3, 3)) && not (openCell yard past Present (3, 3))
  , check "crate: the cell it just left can be a crossing at once" $
      let g0 = at yard Past (1, 1) False
          g = shift yard (walk yard DirRight g0)
      in gEra g == Present && gCell g == (2, 1)
  ]

-- Plates and doors. ------------------------------------------------------------

-- | The yard with a plate at (4,3) and a door at (5,2).
gatedYard :: Level
gatedYard = yard
  { lvPlates = Set.fromList [(4, 3)]
  , lvDoors = Set.fromList [(5, 2)]
  , lvPresent = Set.delete (5, 2) (lvPresent yard)
  }

-- | A present door is open exactly when all plates have weight. Test crates and
-- keepers, removal of weight, shared-era placement restrictions and undo. The
-- parser's adjacency restriction prevents stepping off a plate into its door.
plateChecks :: [Check]
plateChecks =
  [ check "door: shut, it stops a step in the present, and a shift into it, for free" $
      let toDoor = at gatedYard Present (4, 2) False
          stepped = walk gatedYard DirRight toDoor
          shifted = run gatedYard [space] (at gatedYard Past (5, 2) False)
      in gCell stepped == (4, 2) && gRemaining stepped == gRemaining toDoor
         && gEra shifted == Past && fmap fbKind (gFeedback shifted) == Just (ShiftBlocked (5, 2))
  , check "door: it is only in the present; the past there is ordinary floor" $
      gCell (walk gatedYard DirRight (at gatedYard Past (4, 2) False)) == (5, 2)
  , check "door: a crate on the plate opens it, for a step and for a shift" $
      let held g = g { gCrates = Set.fromList [(4, 3)] }
          stepped = walk gatedYard DirRight (held (at gatedYard Present (4, 2) False))
          shifted = shift gatedYard (held (at gatedYard Past (5, 2) False))
      in gCell stepped == (5, 2) && gEra shifted == Present && gCell shifted == (5, 2)
  , check "door: pushing the crate onto the plate opens it, and off the plate shuts it" $
      let g0 = (at gatedYard Past (2, 3) False) { gCrates = Set.fromList [(3, 3)] }
          on = walk gatedYard DirRight g0
          off = walk gatedYard DirRight on
      in not (doorsOpen gatedYard (gCrates g0)) && doorsOpen gatedYard (gCrates on)
         && not (doorsOpen gatedYard (gCrates off))
  , check "door: a keeper standing on the plate holds it, in either era, until they step off" $
      let onIt era = (at gatedYard era (4, 3) False) { gCrates = Set.empty }
          off = walk gatedYard DirUp (onIt Present)
      in doorsOpen gatedYard (weights (onIt Past)) && doorsOpen gatedYard (weights (onIt Present))
         && gCell off == (4, 2) && not (doorsOpen gatedYard (weights off))
         && gCell (walk gatedYard DirRight off) == (4, 2)
  , check "door: the parser refuses a plate next to a door" $
      case parseLevel 50
             [ "#####", "#B..#", "#.P.#", "#...#", "#####" ]
             [ "#####", "#S..#", "#...#", "#.DG#", "#####" ] of
        Left err -> "next to a door" `isInfixOf` err
        Right _ -> False
  , check "door: with two plates, one crate is not enough" $
      let two = gatedYard { lvPlates = Set.fromList [(4, 3), (6, 3)] }
      in not (doorsOpen two (Set.fromList [(4, 3)]))
         && doorsOpen two (Set.fromList [(4, 3), (6, 3)])
  , check "door: a crate cannot be pushed into one, shut or open, and is told why" $
      let shut = (at gatedYard Past (3, 2) False) { gCrates = Set.fromList [(4, 2)] }
          open = shut { gCrates = Set.fromList [(4, 2), (4, 3)] }
          refused g = let r = walk gatedYard DirRight g
                      in gCrates r == gCrates g && gCell r == (3, 2)
                         && fmap fbKind (gFeedback r) == Just (PushBlocked (5, 2))
      in refused shut && refused open
  , check "door: undoing the push that opened it shuts it again" $
      let g0 = (at gatedYard Past (2, 3) False) { gCrates = Set.fromList [(3, 3)] }
          g = run gatedYard [undoKey] (walk gatedYard DirRight g0)
      in not (doorsOpen gatedYard (gCrates g))
         && g { gRemaining = gRemaining g0, gPrev = gPrev g0 } == g0
  , check "door: a crate used up in a pit holds nothing" $
      let plateOnPit = gatedYard { lvPlates = Set.fromList [(6, 1)] }
          g0 = (at plateOnPit Past (4, 1) False) { gCrates = Set.fromList [(5, 1)] }
      in not (doorsOpen plateOnPit (gCrates (walk plateOnPit DirRight g0)))
  , check "door: a level with no plates has no doors to open" $
      not (doorsOpen yard (lvCrates yard))
  , check "door: nobody ever stands in a shut one, in any reachable state" $
      nobodyInShutDoor gatedYard
  ]

-- | Every reachable state, checked directly: in the present, the player's cell is on
-- the map, or is a door opened by the state's crates, keeper and echo.
nobodyInShutDoor :: Level -> Bool
nobodyInShutDoor l = all ok (Map.keys (srStates (search l (\_ _ -> True) (startPlaying l))))
  where
    ok k = skEra k /= Present || walkable l Present (skCell k)
           || (skCell k `Set.member` lvDoors l && doorsOpen l (held k))
    -- Reconstruct weights from a one-keeper StateKey. maybe id leaves crates
    -- and player unchanged when there is no echo; otherwise insert its cell.
    held k = maybe id (Set.insert . snd) (skEcho k) (Set.insert (skCell k) (skCrates k))

-- The echo. --------------------------------------------------------------------

-- | The archived "Echo" level: spawn (1,4), plate (3,4), door (8,4), lighthouse
-- (10,4), all in one row of the present; the cell is at (1,3) in the past.
echoRoom :: Level
echoRoom = head [l | Right ls <- [extraLevels], l <- ls, lvName l == "Echo"]

-- | Request an echo through the real action adapter, retaining refusal/price
-- behaviour. It settles immediately, so another echo needs a released key.
sendEcho :: Level -> Game -> Game
sendEcho level = applyAction level SendEcho

-- | Apply whole direction actions in order. Empty directions return the input
-- game; the strict fold avoids retaining intermediate unevaluated games.
walks :: Level -> [Dir] -> Game -> Game
walks level ds g = foldl' (flip (walk level)) g ds

-- | Footprints are newest-first, bounded and era-aware; an echo stops at the
-- last reachable footprint and contributes only weight. Refusals, replacement,
-- undo and restart must preserve these rules. The supplied fixture tests the
-- disabled-echo path so ordinary levels do not grow unnecessary solver state.
echoChecks :: Level -> [Check]
echoChecks fixture =
  [ check "echo: steps and shifts leave footprints, newest first; refusals leave none" $
      let g0 = startPlaying echoRoom
          stepped = walk echoRoom DirRight g0
          bumped = walk echoRoom DirUp g0
          shifted = shift echoRoom g0
      in gTrail g0 == [pr (1, 4)]
         && gTrail stepped == [pr (2, 4), pr (1, 4)]
         && gTrail bumped == gTrail g0
         && gTrail shifted == [(Past, (1, 4)), pr (1, 4)]
  , check "echo: only the last five footprints are kept, the player's own included" $
      gTrail (walks echoRoom (replicate 6 DirRight) (startPlaying echoRoom))
        == map pr [(7, 4), (6, 4), (5, 4), (4, 4), (3, 4)]
  , check "echo: walking on four from the plate, it stops on the plate and the door opens" $
      let g0 = walks echoRoom (replicate 6 DirRight) (startPlaying echoRoom)
          g = sendEcho echoRoom g0
      in not (doorsOpen echoRoom (weights g0))
         && gEcho g == Just (pr (3, 4)) && doorsOpen echoRoom (weights g)
         && gRemaining g == gRemaining g0 - 4 && gTrail g == [pr (7, 4)]
         && gCell (walk echoRoom DirRight g) == (8, 4)
  , check "echo: it follows the footprints across a shift into the other era" $
      let g0 = walk echoRoom DirUp (shift echoRoom (startPlaying echoRoom))
      in gEcho (sendEcho echoRoom g0) == Just (pr (1, 4)) && gEra g0 == Past
  , check "echo: it stops before a footprint a crate now stands on" $
      let g = (at echoRoom Present (7, 4) False)
                { gTrail = map pr [(7, 4), (6, 4), (5, 4), (4, 4), (3, 4)]
                , gCrates = Set.fromList [(5, 4)] }
      in echoLanding echoRoom g == Just (pr (6, 4))
         && echoPath echoRoom g == map pr [(7, 4), (6, 4)]
  , check "echo: with nothing to walk back along, it is refused for free, and says why" $
      let g0 = startPlaying echoRoom
          g = sendEcho echoRoom g0
      in gEcho g == Nothing && gRemaining g == gRemaining g0
         && fmap fbKind (gFeedback g) == Just EchoNeedsSteps
  , check "echo: it is refused for free from a doorway, and the old echo stays" $
      let g0 = (at echoRoom Present (8, 4) False)
                 { gEcho = Just (pr (3, 4)), gTrail = map pr [(8, 4), (7, 4)] }
          g = sendEcho echoRoom g0
      in gEcho g == Just (pr (3, 4)) && gRemaining g == gRemaining g0
         && fmap fbKind (gFeedback g) == Just EchoInDoorway
  , check "echo: without the time for it, it is refused for free" $
      let g0 = (walks echoRoom [DirRight, DirRight] (startPlaying echoRoom)) { gRemaining = 3 }
          g = sendEcho echoRoom g0
      in gEcho g == Nothing && gRemaining g == 3
  , check "echo: a second echo replaces the first" $
      let first = sendEcho echoRoom (walks echoRoom [DirRight, DirRight] (startPlaying echoRoom))
          second = sendEcho echoRoom (walks echoRoom [DirRight, DirRight] first)
      in gEcho first == Just (pr (1, 4)) && gEcho second == Just (pr (3, 4))
  , check "echo: undo takes it back, footprints and all, for half its price" $
      let g0 = walks echoRoom (replicate 6 DirRight) (startPlaying echoRoom)
          g = run echoRoom [undoKey, noInput] (sendEcho echoRoom g0)
      in gEcho g == Nothing && gTrail g == gTrail g0 && gRemaining g == gRemaining g0 - 2
  , check "echo: restart clears it" $
      let g = sendEcho echoRoom (walks echoRoom [DirRight, DirRight] (startPlaying echoRoom))
          again = run echoRoom [restartKey, noInput] g
      in gEcho again == Nothing && gTrail again == [pr (1, 4)]
  , check "echo: a level without it keeps no footprints and ignores E" $
      let g0 = walk fixture DirUp (startPlaying fixture)
          g = sendEcho fixture g0
      in gTrail g0 == [] && g == g0 { gPrev = gPrev g }
  , check "fx: a new echo walks back from the player to where it stays" $
      let g0 = walks echoRoom (replicate 6 DirRight) (startPlaying echoRoom)
          g = sendEcho echoRoom g0
      in fmap ewPath (fxEchoWalk (stepFx g0 g initialFx))
           == Just (map pr [(7, 4), (6, 4), (5, 4), (4, 4), (3, 4)])
  ]
  where
    -- Shorthand for a present-era footprint, distinct from a bare cell.
    pr c = (Present, c)

-- Mirrors. ---------------------------------------------------------------------

-- | Find a trusted named fixture among campaign/prototype levels. Pattern
-- generators keep only Right parses. head deliberately fails if the expected
-- name disappears; this helper assumes the repository's fixed fixture names.
levelNamed :: String -> Level
levelNamed name = head [l | Right ls <- [campaign, extraLevels], l <- ls, lvName l == name]

-- | Fixed-glass micro-map, movable-mirror campaign room, and door-gated prototype.
-- The micro-map isolates reflection distance without unrelated routes; the
-- named rooms exercise mirror movement and plate-dependent beam occlusion.
glass, twoAlignments, oneWay :: Level
-- A past mirror facing down at (6,4), with the cell two spaces behind it.
glass = either error id $ parseLevel 42
  [ "########"
  , "########"
  , "######B#"
  , "######.#"
  , "######v#"
  , "######.#"
  , "######.#"
  , "######.#"
  , "########"
  ]
  [ "########"
  , "########"
  , "######G#"
  , "########"
  , "########"
  , "########"
  , "########"
  , "######S#"
  , "########"
  ]
twoAlignments = levelNamed "Two Alignments"
oneWay = levelNamed "One Way In"

-- | Beams, reflection distance and the jump-versus-walk decision share the real
-- rule queries. Check blocked landings and held keys, moving/fixed mirrors, door
-- occlusion and undo, then show that bypassing the intended setup can dead-end.
-- movableMirrorChecks adds destination and persistence edge cases below.
mirrorChecks :: [Check]
mirrorChecks =
  [ check "parser: a mirror belongs to the era it is drawn in, and is solid there" $
      Map.lookup (6, 4) (lvMirrors glass) == Just (Mirror Past DirDown False Nothing)
        && not (walkable glass Past (6, 4))
        && Map.lookup (5, 2) (lvMirrors twoAlignments) == Just (Mirror Present DirRight True Nothing)
        && not (openCell twoAlignments (startPlaying twoAlignments) Present (5, 2))
  , check "mirror: in its light and facing it, the key steps through, as far behind as in front" $
      let g0 = at glass Past (6, 6) False
          g = walk glass DirUp g0
      in gCell g == (6, 2) && gHasCell g && gRemaining g == gRemaining g0 - 4
  , check "mirror: with nowhere to land, the same key only walks" $
      let g0 = at glass Past (6, 7) False
          g = walk glass DirUp g0
      in fmap jOpen (mirrorAhead glass g0 DirUp) == Just False
         && gCell g == (6, 6) && gRemaining g == gRemaining g0 - 2
  , check "mirror: holding the key walks into the light and stops, saying to press again" $
      let g = run glass (press DirUp : replicate 40 (press DirUp)) (at glass Past (6, 7) False)
      in gCell g == (6, 6) && fmap fbKind (gFeedback g) == Just JumpNeedsPress
  , check "mirror: only the key that faces it steps through" $
      jumpTarget glass (at glass Past (6, 6) False) DirDown == Nothing
  , check "mirror: it only works in its own era" $
      jumpTarget twoAlignments (at twoAlignments Present (7, 2) False) DirLeft == Just (3, 2)
        && jumpTarget twoAlignments (at twoAlignments Past (7, 2) False) DirLeft == Nothing
  , check "mirror: a crate in the light stops it" $
      let l = twoAlignments
          g = (at l Past (7, 6) False)
                { gCrates = Set.singleton (7, 5)
                , gMirrors = Map.insert (7, 4) (Mirror Past DirDown True Nothing) (Map.delete (5, 4) (lvMirrors l)) }
      in beamCells l g (7, 4) == [] && mirrorAhead l g DirUp == Nothing
  , check "mirror: side and rear pushes preserve the face, and undo restores the position" $
      let l = twoAlignments
          side = at l Past (4, 4) False
          moved = walk l DirRight side
          rear = walk l DirDown (at l Past (5, 3) False)
          front = walk l DirUp (at l Past (5, 5) False)
          rewound = run l [undoKey, noInput] moved
      in gCell moved == (5, 4) && Map.lookup (6, 4) (gMirrors moved) == Just (Mirror Past DirDown True Nothing)
         && Map.notMember (5, 4) (gMirrors moved) && gRemaining moved == gRemaining side - 2
         && Map.lookup (5, 5) (gMirrors rear) == Just (Mirror Past DirDown True Nothing)
         && gCell front == (5, 3) && gMirrors front == lvMirrors l
         && gCell rewound == gCell side && gMirrors rewound == gMirrors side
  , check "two alignments: the past clears the mirror's launch cell; the crate is rusted now" $
      let l = twoAlignments
          shove era = walk l DirLeft (at l era (8, 6) False)
      in gCrates (shove Past) == Set.singleton (6, 6) && gCrates (shove Present) == lvCrates l
  , check "two alignments: with its mirrors fixed, the level cannot be won" $
      let l = twoAlignments { lvMirrors = Map.map (\m -> m { mMovable = False }) (lvMirrors twoAlignments) }
      in srGoal (search l (\_ _ -> True) (startPlaying l)) == Nothing
  , check "mirror: a shut door stops the light, and one held open lets it through" $
      let shut = at oneWay Present (6, 4) True
          held = shut { gCrates = Set.fromList [(4, 7)] }
      in jumpTarget oneWay shut DirLeft == Nothing && gCell (walk oneWay DirLeft shut) == (6, 4)
         && jumpTarget oneWay held DirLeft == Just (2, 4)
         && gPhase (walk oneWay DirLeft held) == Won
  , check "mirror: undo brings the player back through it, for half the price" $
      let g0 = at glass Past (6, 6) False
          g = run glass [undoKey, noInput] (walk glass DirUp g0)
      in gCell g == (6, 6) && not (gHasCell g) && gRemaining g == gRemaining g0 - 2
  , check "mirror: stepping through first in One Way In shuts the player in" $
      let g = foldl' (flip (applyAction oneWay)) (startPlaying oneWay)
                [ShiftEra, Go DirRight, Go DirRight, Go DirRight, Go DirUp]
      in gCell g == (6, 4) && stateKey g `Set.member` deadStates oneWay
  ] ++ movableMirrorChecks

-- The dead-end search the game runs a slice per frame. -------------------------
--
-- The game advances each level's 'DeadSearch' a few units a frame, so however the
-- work is cut up it must find exactly what the full cheapest-first 'search' calls
-- dead ends. Each campaign level's reference set is worked out once (The Long Way
-- Round's takes several seconds) and compared with the search advanced one unit, 7
-- units and 1000 units at a time: cut at every possible point, at uneven points,
-- and in big pieces, the way a frame's time budget might cut it.
-- | Resumable search must agree with exhaustive search regardless of slice size.
-- Also compare packed membership with the expanded set and probe an unseen key
-- with negative/large coordinates, exercising variable-length packing lookup.
deadSearchChecks :: [Check]
deadSearchChecks = case campaign of
  Left err -> [check ("dead search: campaign parses: " ++ err) False]
  Right levels -> concat
    [ [ check ("dead search: " ++ lvName l ++ ", advanced " ++ show k
               ++ " units at a time, finds exactly the full search's dead ends")
          (sliced k l == Just reference)
      | k <- [1, 7, 1000] ]
    | l <- levels
    , let reference = Set.fromList (srDeadEnds (search l (\_ _ -> True) (startPlaying l))) ]
    ++
    [ check "dead search: one not yet advanced knows nothing, so the game says nothing yet" $
        let plug = levelNamed "The Plug"
            lost = foldl' (flip (applyAction plug)) (startPlaying plug)
                     [Go DirDown, Go DirRight, Go DirRight, Go DirRight, ShiftEra, Go DirRight]
            fresh = startDeadSearch plug
        in deadSearchResult fresh == Nothing && not (deadSearchFinished fresh)
           && not (knownDeadEnd fresh (stateKey lost))
           && knownDeadEnd (finishDeadSearch fresh) (stateKey lost)
    , check "dead search: the game's quick lookup agrees with the set on every reachable state" $
        -- 'knownDeadEnd' looks keys up in the packed result rather than in the set;
        -- Two Alignments has some ten thousand dead states among thirteen thousand.
        let l = levels !! 1
            done = finishDeadSearch (startDeadSearch l)
            dead = deadStates l
        in all (\k -> knownDeadEnd done k == (k `Set.member` dead))
               (Map.keys (srStates (search l (\_ _ -> True) (startPlaying l))))
    , check "dead search: a key it never met is not dead" $
        let l = levels !! 1
            done = finishDeadSearch (startDeadSearch l)
            stranger = (stateKey (startPlaying l)) { skCell = (-7, 123456789), skPhase = Paused }
        in not (knownDeadEnd done stranger)
    ]
  where
    -- Advance @k@ units at a time until the search says it is finished.
    sliced k l = deadSearchResult (until deadSearchFinished (advanceDeadSearch k) (startDeadSearch l))

-- The campaign. -----------------------------------------------------------------
-- | Campaign structure and Run navigation: unlocks, retry, restored progress and
-- hall selection must agree with completion, without entering locked rooms.
-- Every listed level then undergoes the shared route/invariant checks below.
campaignChecks :: [Check]
campaignChecks = case campaign of
  Left err -> [check ("campaign: parses: " ++ err) False]
  Right levels@(first : _) -> concat
    [ [ check "campaign: slack only ever shrinks, as a share of par" $
          -- A big level needs more seconds of slack than a small one, but less of its
          -- par. Compared by cross-multiplying, so no fractions are rounded.
          let tighter a b = (lvBudget b - lvPar b) * lvPar a <= (lvBudget a - lvPar a) * lvPar b
          in and (zipWith tighter levels (drop 1 levels))
      , check "tutorial: the crate goes into its recess in the past, and is rusted in the present" $
          let beside = walk first DirRight (startPlaying first)
              past = applyAction first ShiftEra beside
              pushed = walk first DirRight past
              blockedByWall = walk first DirRight pushed
              rusted = walk first DirRight beside
          in gEra beside == Present && gEra past == Past
             && gCell pushed == (4, 5) && gCrates pushed == Set.singleton (5, 5)
             && gRemaining pushed == gRemaining past - ruStepCost (lvRules first)
             && gCell blockedByWall == gCell pushed && gCrates blockedByWall == gCrates pushed
             && gCrates rusted == lvCrates first && gCell rusted == gCell beside
      , check "run: Enter on a won level unlocks and starts the next, fresh" $
          let won = (runGame (startRun levels 0)) { gPhase = Won, gAttempt = 7 }
              next = stepRun levels enter (Run 0 0 0 InLevel won)
              held = stepRun levels enter next
          in runIndex next == 1 && gPhase (runGame next) == Playing
             && runMode next == InLevel && runCompleted next == 1 && runUnlocked next == 1
             && gRemaining (runGame next) == lvBudget (levels !! 1)
             && gCell (runGame next) == lvSpawn (levels !! 1)
             && gCrates (runGame next) == lvCrates (levels !! 1)
             && gAttempt (runGame next) == 8
             && runIndex held == 1 && gPhase (runGame held) == Playing
      , check "run: R on a won level plays it again" $
          let won = (runGame (startRun levels 0)) { gPhase = Won }
              again = stepRun levels restartKey (Run 0 0 0 InLevel won)
          in runIndex again == 0 && gPhase (runGame again) == Playing
      , check "run: Enter after the last level returns to the lobby" $
          let lastOne = length levels - 1
              won = (runGame (startRun levels lastOne)) { gPhase = Won }
              back = stepRun levels enter (Run lastOne lastOne (length levels) InLevel won)
              held = stepRun levels enter back
          in runIndex back == lastOne && runMode back == Lobby && gPhase (runGame back) == Menu
             && runMode held == Lobby
      , check "dead ends: pushing the Plug's crate the obvious way is known to be lost" $
          let plug = head [l | Right extra <- [extraLevels], l <- extra, lvName l == "The Plug"]
              actions = [Go DirDown, Go DirRight, Go DirRight, Go DirRight, ShiftEra, Go DirRight]
              ended = foldl' (flip (applyAction plug)) (startPlaying plug) actions
              rewound = run plug [undoKey] ended
              dead = deadStates plug
          in gCrates ended == Set.fromList [(7, 4)] && stateKey ended `Set.member` dead
             && stateKey rewound `Set.notMember` dead
             && stateKey (startPlaying plug) `Set.notMember` dead
      , check "campaign: none of it uses the original station's hand-placed dressing" $
          not (any lvDressed levels)
      , check "campaign: every level states the rule it introduces" $
          all (not . null . lvLesson) levels
      , check "run: Enter while lost retries the same level" $
          let lost = (runGame (startRun levels 1)) { gPhase = Lost, gRemaining = 0 }
              again = stepRun levels enter (Run 1 1 1 InLevel lost)
          in runIndex again == 1 && gPhase (runGame again) == Playing
             && gRemaining (runGame again) == lvBudget (levels !! 1)
      , check "run: restored progress opens the title on the saved level" $
          let r = startRunWithProgress levels 2 2
          in runIndex r == 2 && runUnlocked r == 2 && runMode r == Title
             && gPhase (runGame r) == Menu
             && gRemaining (runGame r) == lvBudget (levels !! 2)
      , check "run: the lobby cannot select a locked level" $
          let lobby = (startRun levels 0) { runMode = Lobby }
              released = stepRun levels noInput lobby
              r = stepRun levels (press DirDown) released
          in runIndex r == 0 && runUnlocked r == 0 && runMode r == Lobby
      , check "run: title, lobby and level form a reversible presentation flow" $
          let title = startRunWithProgress levels 1 1
              lobby = stepRun levels enter title
              ready = stepRun levels noInput lobby
              playing = stepRun levels enter ready
              released = stepRun levels noInput playing
              back = stepRun levels lobbyKey released
              backReleased = stepRun levels noInput back
              titleAgain = stepRun levels lobbyKey backReleased
          in runIndex playing == 1 && runMode playing == InLevel
             && gPhase (runGame playing) == Playing
             && runIndex back == 1 && runMode back == Lobby
             && gPhase (runGame back) == Menu && runMode titleAgain == Title
      ]
    , concatMap levelChecks levels
    ]
  Right [] -> [check "campaign: contains a tutorial" False]

-- | What every level must prove through the real rules.
levelChecks :: Level -> [Check]
levelChecks l | Just known <- knownRoute l =
  -- Too big to search on every run: its pinned route is replayed instead. The full
  -- search found that route, and par, once.
  let played n = run l (routeInputs known) (startPlaying l) { gRemaining = n }
      named what = check ("level " ++ lvName l ++ ": " ++ what)
  in [ named "it has slack to spare" $ lvBudget l > lvPar l
     , named "its pinned route costs exactly par" $
         let end = foldl' (flip (applyAction l)) (startPlaying l) known
         in gPhase end == Won && lvBudget l - gRemaining end == lvPar l
     , named "its pinned route wins, tick by tick, on exactly par" $
         gPhase (played (lvPar l)) == Won && gRemaining (played (lvPar l)) == 0
     , named "and cannot win with one step less" $
         gPhase (played (lvPar l - 2)) /= Won
     ]
levelChecks l =
  [ named "the solver confirms its par" $ fmap fst goal == Just (lvPar l)
  , named "it has slack to spare" $ lvBudget l > lvPar l
  , named "its route wins, tick by tick, on exactly par" $
      gPhase (play (lvPar l)) == Won && gRemaining (play (lvPar l)) == 0
  , named "and cannot win with one step less" $
      gPhase (play (lvPar l - 2)) /= Won
  -- An echo can strand the player too: sent again from beyond its door, it leaves
  -- the plate and shuts them in. Like a bad push, the game says so, and Z undoes it.
  , named "with no crates and no echo there are no dead ends" $
      not (Set.null (lvCrates l)) || ruEchoLength (lvRules l) > 0 || null (srDeadEnds result)
  , named "its crates matter: it cannot be won without pushing one, if it has any" $
      Set.null (lvCrates l) || srGoal (search l noPushing start) == Nothing
  -- A level of several rooms may reach more states, but its one-way gates keep them
  -- a sum over rooms rather than a product.
  , named "its search stays small enough to run" $
      Map.size (srStates result) <= (if roomOf (lvWidth l - 1, lvHeight l - 1) == (0, 0) then 20000 else 250000)
  -- A pit a crate has filled is past floor, so another crate may stand on it.
  , named "no crate is ever inside a wall, in either era" $
      all (\k -> all (\c -> (walkable l Past c || c `Set.member` skFilled k) && walkable l Present c) (skCrates k))
        (Map.keys (srStates result))
  , named "nobody ever stands in a shut door" $ nobodyInShutDoor l
  , named "the winning route never passes through a state called dead" $
      let trail = scanl (flip (applyAction l)) start route
          dead = deadStates l
      in all (\g -> stateKey g `Set.notMember` dead) trail
  , named "its door matters: it cannot be won without opening it, if it has one" $
      Set.null (lvDoors l) || srGoal (search (noDoors l) (\_ _ -> True) (startPlaying (noDoors l))) == Nothing
  , named "its echo matters: it cannot be won without one, if it allows one" $
      ruEchoLength (lvRules l) == 0
        || srGoal (search l (\_ a -> a /= SendEcho) start) == Nothing
  , named "its mirrors matter: it cannot be won without stepping through one, if it has any" $
      Map.null (lvMirrors l)
        || srGoal (search l (\g a -> case a of { Go d -> jumpTarget l g d == Nothing; _ -> True }) start) == Nothing
  , named "nobody ever stands on a crate" $
      all (\k -> skCell k `Set.notMember` skCrates k) (Map.keys (srStates result))
  ]
  where
    -- Shared where bindings reuse one exhaustive result across the checks for l.
    -- Absent goals give an empty route, but the explicit par/win checks still fail.
    named what = check ("level " ++ lvName l ++ ": " ++ what)
    start = startPlaying l
    result = search l (\_ _ -> True) start
    goal = srGoal result
    route = maybe [] snd goal
    -- An unreachable border plate prevents every door from opening, isolating
    -- whether the door mechanic is necessary to solve this level.
    noDoors lv = lv { lvPlates = Set.fromList [(0, 0)] }
    -- Reject only past commands directed into crates; shifts/echoes remain
    -- available so this asks whether crate movement, specifically, is essential.
    noPushing g action = case action of
      Go d -> stepCell d (gCell g) `Set.notMember` gCrates g || gEra g /= Past
      ShiftEra -> True
      SendEcho -> True
    -- Replay at a chosen budget through ticks, independently of search's refill.
    play n = run l (routeInputs route) start { gRemaining = n }

-- Undo. ------------------------------------------------------------------------
-- | Undo restores the latest accepted action's board snapshot but refunds only
-- its configured percentage. Test ordering during movement, key arming, pickup,
-- history exhaustion and recovery from loss, including a refund too small to
-- resume play. Whole-game comparisons normalise only time and last-tick keys.
undoChecks :: Level -> [Check]
undoChecks level =
  [ check "undo: a step comes back exactly, for half its price" $
      let g0 = at level Present (2, 6) False
          g1 = walk level DirUp g0
          g2 = run level [undoKey] g1
      in g2 { gRemaining = gRemaining g0, gPrev = gPrev g0 } == g0
         && gRemaining g2 == gRemaining g0 - 1
  , check "undo: a shift comes back exactly, for half its price" $
      let g0 = at level Present (2, 6) False
          g2 = run level [undoKey] (shift level g0)
      in g2 { gRemaining = gRemaining g0, gPrev = gPrev g0 } == g0
         && gRemaining g2 == gRemaining g0 - 3
  , check "undo: rewinds a pickup, and the cell can be taken again" $
      let g0 = at level Past (9, 2) False
          g1 = walk level DirRight g0
          g2 = run level [undoKey] g1
          g3 = walk level DirRight g2
      in gHasCell g1 && not (gHasCell g2) && gCell g2 == (9, 2) && gHasCell g3
         && gFeedback g2 == Nothing
  , check "undo: walks back through every action, newest first" $
      let g0 = startPlaying level
          route = take 12 intendedRoute
          played = foldl' (flip (applyAction level)) g0 route
          rewound = foldl' (\g _ -> run level [undoKey, noInput] g) played route
      in essentials rewound == essentials g0 && null (gHistory rewound)
         && gRemaining rewound == lvBudget level
            - sum (map (\a -> if a == ShiftEra then 3 else 1) route)
  , check "undo: can never give back more than was spent" $
      let g0 = startPlaying level
          played = foldl' (flip (applyAction level)) g0 (take 12 intendedRoute)
          trail = scanl (\g _ -> run level [undoKey, noInput] g) played [1 .. 30 :: Int]
      in all ((< lvBudget level) . gRemaining) trail
  , check "undo: with nothing to rewind, it does nothing" $
      let g0 = startPlaying level
      in run level [undoKey] g0 == g0 { gPrev = undoKey }
  , check "undo: holding Z rewinds exactly once" $
      let g1 = walk level DirUp (walk level DirUp (at level Present (2, 6) False))
          g2 = run level (replicate 100 undoKey) g1
      in gCell g2 == (2, 5) && length (gHistory g2) == 1
  , check "undo: pressed during a step, it waits for the landing, then rewinds it" $
      let g0 = at level Present (2, 6) False
          g1 = run level [press DirUp, undoKey] g0
          g2 = idle level (stepTicks - 1) g1
      in gPendingUndo g1 && gMotion g1 /= Settled
         && gCell g2 == (2, 6) && gMotion g2 == Settled && not (gPendingUndo g2)
         && gRemaining g2 == gRemaining g0 - 1
  , check "undo: pressed during the last affordable step, it still runs" $
      let g0 = (at level Present (2, 6) False) { gRemaining = 2 }
          g = idle level (stepTicks - 1) (run level [press DirUp, undoKey] g0)
      in gCell g == (2, 6) && gRemaining g == 1 && gPhase g == Lost
  , check "undo: cancels anything buffered" $
      let g0 = at level Present (2, 6) False
          g1 = run level [press DirUp, undoKey { tiShift = True }, noInput { tiDir = Just DirLeft }] g0
          g2 = idle level (stepTicks + shiftTicks) g1
      in gEra g2 == Present && gCell g2 == (2, 6) && null (gHistory g2)
  , check "undo: a direction held through it needs a fresh press" $
      let g1 = walk level DirUp (at level Present (2, 6) False)
          held = press DirUp
          g2 = run level (held { tiUndo = True } : replicate 40 held) g1
      in gCell g2 == (2, 6) && gMotion g2 == Settled
  , check "undo: letting go of a newer key does not walk the older one" $
      let held = keyDown KD (keyDown KA noKeys)
          g1 = walk level DirUp (at level Present (2, 6) False)
          ticking input = tickInput (afterTick input)
          g2 = run level [ (ticking held) { tiUndo = True }, ticking held ] g1
          g3 = run level (replicate 30 (ticking (keyUp KD held))) g2
      in gCell g3 == (2, 6) && gMotion g3 == Settled && gRemaining g3 == gRemaining g2
  , check "undo: does nothing on the win screen, while paused, or on the title" $
      let won = walk level DirLeft (at level Present (3, 2) True)
          paused = run level [pauseKey] (walk level DirUp (at level Present (2, 6) False))
          title = initialGame level
          same g = run level [noInput, undoKey] g == g { gPrev = undoKey }
      in gPhase won == Won && same won && same paused && same title
  , check "undo: restart clears the history" $
      let g = run level [restartKey] (walk level DirUp (startPlaying level))
      in null (gHistory g) && gRemaining g == lvBudget level
  , check "undo: from the lost screen, a shift's refund plays on" $
      let g0 = (at level Present (2, 6) False) { gRemaining = 6 }
          lost = idle level (shiftTicks + 1) (run level [space] g0)
          back = run level [undoKey] lost
      in gPhase lost == Lost && gPhase back == Playing && gEra back == Present
         && gRemaining back == 3
  , check "undo: from the lost screen, a step's refund alone is not enough to play on" $
      let g0 = (at level Present (2, 6) False) { gRemaining = 2 }
          lost = idle level 1 (walk level DirUp g0)
          once = run level [undoKey] lost
      in gPhase lost == Lost && gRemaining lost == 0
         && gCell once == (2, 6) && gRemaining once == 1 && gPhase once == Lost
         && gPhase (idle level 30 once) == Lost
  , check "undo: from the lost screen, enough rewinds get back to playing" $
      let g0 = (at level Present (2, 6) False) { gRemaining = 4 }
          lost = idle level 1 (walk level DirUp (walk level DirUp g0))
          once = run level [undoKey, noInput] lost
          twice = run level [undoKey, noInput] once
      in gPhase lost == Lost && gPhase once == Lost && gPhase twice == Playing
         && gCell twice == (2, 6) && gRemaining twice == 2
  , check "undo: the refund is a rule of the level, not of the code" $
      let generous = level { lvRules = (lvRules level) { ruUndoRefundPercent = 100 } }
          g0 = at generous Present (2, 6) False
          g1 = run generous [undoKey] (walk generous DirUp g0)
      in gRemaining g1 == gRemaining g0
  , check "prices are a rule of the level too" $
      let dear = level { lvRules = (lvRules level) { ruStepCost = 3, ruShiftCost = 7 } }
          g0 = at dear Present (2, 6) False
      in gRemaining (walk dear DirUp g0) == gRemaining g0 - 3
         && gRemaining (shift dear g0) == gRemaining g0 - 7
  ]

-- 14, 15. ----------------------------------------------------------------------
-- | Buffered input priority and fresh-press detection. Mid-step Space waits for
-- arrival, then precedes movement; repeated/held Space cannot become extra shifts.
-- Direction repeats and newest-key selection are separate from that one-shot rule.
pendingShifts :: Level -> [Check]
pendingShifts level =
  [ check "14. a shift pressed mid-step waits for the step to land" $
      let g = run level [press DirUp, noInput, space] (at level Present (2, 6) False)
      in gEra g == Present && gPendingShift g && gCell g == (2, 6)
  , check "14. it then applies at the new cell" $
      let g0 = run level [press DirUp, noInput, space] (at level Present (2, 6) False)
          g1 = idle level (stepTicks - 2) g0
      in gEra g1 == Past && gCell g1 == (2, 5) && not (gPendingShift g1)
         && gMotion g1 == Shifting shiftTicks
  , check "14. two presses mid-step still make one shift" $
      let g0 = run level [press DirUp, space, noInput, space, noInput]
                 (at level Present (2, 6) False)
          g1 = idle level (stepTicks + shiftTicks + 20) g0
      in gEra g1 == Past
  , check "14. a pending shift into a wall is refused where the step lands" $
      let g0 = run level [press DirRight, noInput, space] (at level Past (3, 2) False)
          g1 = idle level (stepTicks - 2) g0
      in gCell g1 == (4, 2) && gEra g1 == Past && gMotion g1 == Settled
         && fmap fbKind (gFeedback g1) == Just (ShiftBlocked (4, 2))
  , check "14. a pending shift goes before a pending move" $
      let g0 = run level [ press DirUp, noInput, space, noInput { tiDir = Just DirLeft } ]
                 (at level Present (2, 6) False)
          g1 = idle level (stepTicks - 3) g0
      in gEra g1 == Past && gCell g1 == (2, 5) && gMotion g1 == Shifting shiftTicks
         && gPendingMove g1 == Just DirLeft
  , check "15. holding Space shifts exactly once" $
      let g = run level (replicate 200 space) (at level Present (2, 6) False)
      in gEra g == Past && gMotion g == Settled
  , check "15. release and press shifts again" $
      let g = run level (replicate 20 space ++ [noInput] ++ replicate 20 space)
                (at level Present (2, 6) False)
      in gEra g == Present
  , check "15. holding Space through a rejection does not retry after walking" $
      let held = noInput { tiShift = True }
          g = run level (replicate 3 held) (at level Past (4, 2) False)
          g' = run level (replicate 30 held { tiDir = Just DirLeft }) g
      in gEra g' == Past && fst (gCell g') < 4
  , check "held direction repeats every step, and stops when released" $
      let g = run level (replicate (2 * stepTicks + 1) (press DirUp)) (at level Present (2, 6) False)
          g' = idle level (stepTicks + 5) g
      in gCell g == (2, 4) && gMotion g == Stepping (2, 3) stepTicks && gCell g' == (2, 3)
  , check "diagonals do not exist: the newest key wins" $
      let g = run level [press DirUp, press DirRight] (at level Present (2, 6) False)
      in gMotion g == Stepping (2, 5) (stepTicks - 1) && gPendingMove g == Just DirRight
  ]

-- 16, 17, reachability. --------------------------------------------------------
-- | Replay the intended fixture solution through both adapters and exhaust its
-- reachable graph. Restricted searches prove that era changes are necessary;
-- the final custom search counts shifts, rather than minimising total seconds.
routeChecks :: Level -> [Check]
routeChecks level =
  [ check "16. the intended route wins through tick inputs" $
      let g = run level (routeInputs intendedRoute) (startPlaying level)
      in gPhase g == Won && gHasCell g && gCell g == (2, 2) && gEra g == Present
  , check "16. the intended route wins through player actions" $
      gPhase (foldl' (flip (applyAction level)) (startPlaying level) intendedRoute) == Won
  , check "16. the intended route is 36 steps and 6 shifts" $
      length intendedRoute == 42 && length (filter (== ShiftEra) intendedRoute) == 6
  , check "16. each listed action on the route actually does something" $
      let states = scanl (flip (applyAction level)) (startPlaying level) intendedRoute
      in and (zipWith (\a b -> essentials a /= essentials b) states (drop 1 states))
  , check "reachability: cheapest solution through stepGame is 108 s, 42 actions with 6 shifts" $
      case srGoal (search level (\_ _ -> True) (startPlaying level)) of
        Just (n, route) -> n == 108 && length route == 42 && length (filter (== ShiftEra) route) == 6
        Nothing -> False
  , check "reachability: no reachable state is a dead end, ignoring the budget" $
      null (srDeadEnds (search level (\_ _ -> True) (startPlaying level)))
  , check "reachability: every walkable cell in both eras can be stood on" $
      let reached = Map.keys (srStates (search level (\_ _ -> True) (startPlaying level)))
          allOpen = [ (e, (x, y)) | e <- [Past, Present], x <- [0 .. 12], y <- [0 .. 8]
                    , walkable level e (x, y) ]
      in all (\(e, c) -> any (\k -> skEra k == e && skCell k == c) reached) allOpen
  , check "17. never shifting, from the start in the present, cannot win" $
      let r = search level (\_ a -> a /= ShiftEra) (startPlaying level)
      in srGoal r == Nothing && all (\k -> skEra k == Present && not (skHasCell k)) (Map.keys (srStates r))
  , check "17. staying in the past after one shift cannot win or reach the cell" $
      let r = search level (\_ a -> a /= ShiftEra) (shift level (startPlaying level))
      in srGoal r == Nothing && all (\k -> skEra k == Past && not (skHasCell k)) (Map.keys (srStates r))
  , check "17. with the cell in hand, staying in the past still cannot win" $
      let r = search level (\_ a -> a /= ShiftEra) (at level Past (10, 2) True)
      in srGoal r == Nothing
  , check "17. fewer than six shifts cannot win" $
      -- States carry the shift count here, so the search sees every budget.
      not (winsWithin 5) && winsWithin 6
  ]
  where
    -- Include shifts-used in the visited key: merging equal boards reached with
    -- different remaining allowances could discard the only feasible route.
    winsWithin budget = go [(startPlaying level, 0 :: Int)] [stateKeyB (startPlaying level) 0]
      where
        stateKeyB g n = (stateKey g, n)
        go [] _ = False
        -- This small breadth-first queue refills time after each edge. The test
        -- bounds shifts, not elapsed budget; all other real movement rules apply.
        go ((g, n) : rest) seen
          | gPhase g == Won = True
          | otherwise =
              let nexts = [ (next, n') | a <- allActions
                          , let n' = n + (if a == ShiftEra then 1 else 0)
                          , n' <= budget
                          , let next = (applyAction level a g) { gRemaining = lvBudget level, gHistory = [] }
                          , stateKeyB next n' `notElem` seen ]
                  uniq = dedupe nexts
              in go (rest ++ uniq) (map (uncurry stateKeyB) uniq ++ seen)
        -- Different refused keys can produce the same successor in one batch.
        -- Remove duplicates before adding them to both queue and visited list.
        dedupe = foldr (\x acc -> if any (\y -> uncurry stateKeyB y == uncurry stateKeyB x) acc
                                    then acc else x : acc) []

-- 18. --------------------------------------------------------------------------
-- | Both action/tick adapters agree on settled essentials. A fixed 6000-tick
-- mashing trace then checks position, timer ranges and budget at every tick.
-- The adjacency invariant applies to this fixture, which has no mirror jumps.
determinism :: Level -> [Check]
determinism level =
  [ check "18. a route ends in the same state, by tick inputs or by whole actions" $
      essentials (run level (routeInputs intendedRoute) (startPlaying level))
        == essentials (foldl' (flip (applyAction level)) (startPlaying level) intendedRoute)
  , check "18. a messy input stream stays in the rules at every tick" $
      let trail = scanl (flip (stepGame level)) (startPlaying level) messyInputs
      in all (\g -> walkable level (gEra g) (gCell g)) trail
         && all legalMotion trail
         && all (\g -> gRemaining g >= 0 && gRemaining g <= lvBudget level) trail
  ]
  where
    -- A countdown must remain within its declared duration; a stepping target
    -- must be adjacent floor. Settled carries no target or timer to validate.
    legalMotion g = case gMotion g of
      Stepping to left -> left >= 1 && left <= stepTicks
                          && walkable level (gEra g) to && adjacent to (gCell g)
      Shifting left    -> left >= 1 && left <= shiftTicks
      Settled          -> True
    adjacent (a, b) (c, d) = abs (a - c) + abs (b - d) == 1

-- | A fixed, ugly stream of keys from a small generator: mashing, not playing.
messyInputs :: [TickInput]
messyInputs = map fromSeed (take 6000 (iterate (\s -> (s * 1103515245 + 12345) `mod` 2147483648) 42))
  where
    -- Decode different base-16 slices of a deterministic generator into keys.
    -- This is reproducible stress input, not a uniform/random gameplay model.
    fromSeed :: Int -> TickInput
    fromSeed s =
      let bits n = (s `div` n) `mod` 16 :: Int
          dir = case bits 65536 of
            0 -> Just DirUp
            1 -> Just DirDown
            2 -> Just DirLeft
            3 -> Just DirRight
            4 -> Just DirUp
            5 -> Just DirRight
            _ -> Nothing
      in noInput { tiDir = dir, tiShift = bits 4096 < 4, tiPause = bits 256 == 0 && bits 16 == 0
                 , tiUndo = bits 16 == 1 }

-- Input adapter. ---------------------------------------------------------------
-- | OS event adaptation without OS IO: preserve between-tick taps, ignore
-- auto-repeat downs, and distinguish a fresh direction from an older held key
-- exposed by release. Integration checks feed those tick records into stepGame.
inputAdapter :: Level -> [Check]
inputAdapter level =
  [ check "input: a tap between two ticks is seen for one tick" $
      let k = keyUp KSpace (keyDown KSpace noKeys)
      in tiShift (tickInput k) && not (tiShift (tickInput (afterTick k)))
  , check "input: a held key stays down over ticks" $
      let k = afterTick (keyDown KSpace noKeys)
      in tiShift (tickInput k) && tiShift (tickInput (afterTick k))
  , check "input: auto-repeat downs change nothing" $
      let k = afterTick (keyDown KUp noKeys)
      in keyDown KUp k == k
  , check "input: the newest direction wins, and the older one returns" $
      let k = keyDown KRight (keyDown KW noKeys)
      in tiDir (tickInput k) == Just DirRight
         && tiDir (tickInput (afterTick (keyUp KRight k))) == Just DirUp
  , check "input: WASD and arrows agree" $
      map (tiDir . tickInput . flip keyDown noKeys) [KW, KUp, KS, KDown, KA, KLeft, KD, KRight]
        == map Just [DirUp, DirUp, DirDown, DirDown, DirLeft, DirLeft, DirRight, DirRight]
  , check "input: a press is a press; letting go of a newer key only reveals the older" $
      let both = afterTick (keyDown KD (afterTick (keyDown KA noKeys)))
          pressed = tickInput (keyDown KD (afterTick (keyDown KA noKeys)))
          released = tickInput (keyUp KD both)
      in not (tiDirRevealed pressed) && tiDirRevealed released && tiDir released == Just DirLeft
  , check "input: a tap made mid-step survives its own release, and is walked on landing" $
      let pressed = keyDown KD noKeys
          walking = afterTick pressed
          tapped = keyDown KW walking
          released = afterTick (keyUp KW (afterTick tapped))
          g0 = run level [tickInput pressed] (at level Present (5, 3) False)
          g1 = run level [tickInput tapped] g0
          g2 = run level [tickInput released] g1
          g3 = run level (replicate stepTicks (tickInput released)) g2
      in gPendingMove g1 == Just DirUp && gPendingMove g2 == Just DirUp
         && gCell g3 == (6, 3) && gMotion g3 == Stepping (6, 2) (stepTicks - 2)
  , check "input: letting go of a newer key still walks on with the older, while walking" $
      let pressed = keyDown KD (afterTick (keyDown KS noKeys))
          released = afterTick (keyUp KD (afterTick pressed))
          g0 = run level [tickInput (keyDown KS noKeys)] (at level Present (5, 3) False)
          g1 = run level [tickInput pressed] g0
          g2 = run level (replicate (3 * stepTicks) (tickInput released)) g1
      in gCell g2 /= (5, 3) && gMotion g2 /= Settled && snd (gCell g2) > 4
  , check "input: Z undoes and Tab peeks" $
      tiUndo (tickInput (keyDown KUndo noKeys)) && tiPeek (tickInput (keyDown KPeek noKeys))
      && not (tiUndo (tickInput noKeys)) && not (tiPeek (tickInput noKeys))
  , check "input: the level-hall key is carried into a tick" $
      tiLobby (tickInput (keyDown KLobby noKeys)) && not (tiLobby (tickInput noKeys))
  ]

-- Effects. ---------------------------------------------------------------------
-- | Effects derive from consecutive games and must never alter their rules.
-- Check wave/pickup/refund lifetimes, pause/restart behaviour, focus and room
-- handover at its midpoint; iterate/scanl retain intermediate ticks to expose
-- off-by-one errors that an endpoint-only comparison would miss.
fxChecks :: Level -> [Check]
fxChecks level =
  [ check "fx: a shift starts a wave as long as the input lock" $
      let g = at level Present (2, 6) False
          g' = run level [space] g
          fx = stepFx g g' initialFx
          ticks = iterate (stepFx g' g') fx
      in fmap waveFrom (fxWave fx) == Just Present
         && fmap waveAge (fxWave (ticks !! (shiftTicks - 1))) == Just (shiftTicks - 1)
         && fxWave (ticks !! shiftTicks) == Nothing
  , check "fx: no wave on a rejected shift" $
      let g = at level Past (4, 2) False
      in fxWave (stepFx g (run level [space] g) initialFx) == Nothing
  , check "fx: pickup makes a burst at the cell" $
      let g = (at level Past (9, 2) False) { gMotion = Stepping (10, 2) 1 }
      in fmap burstCell (fxBurst (stepFx g (run level [noInput] g) initialFx)) == Just (10, 2)
  , check "fx: pausing freezes effects but lets the start fade finish" $
      let g = run level [pauseKey] (at level Present (2, 6) False)
          dirty = Fx 100 (Just (Wave 3 Past (1, 1))) Nothing 0 0 0 5 Nothing Nothing (0, 0) Nothing [(0, 0)] 0
          fx = stepFx g g dirty
      in fxClock fx == 100 && fmap waveAge (fxWave fx) == Just 3 && fxLevelAge fx == 6
  , check "fx: restart clears every effect" $
      let g = at level Past (9, 2) True
          dirty = Fx 500 (Just (Wave 3 Past (1, 1))) (Just (Burst 3 (1, 1))) 50 9 9 9
                    (Just (CostFloat 3 (-2) (1, 1))) (Just (EchoWalk 2 [(Past, (1, 1)), (Past, (2, 1))]))
                    (1, 0) (Just (RoomFade 3 (1, 0))) [(0, 0), (1, 0)] 0
      in stepFx g (run level [restartKey] g) dirty == initialFx
  , check "fx: a restart keeps the board close in, and the first move closes in" $
      let g = at level Past (9, 2) True
          closeIn = initialFx { fxFocus = focusTicks }
          restarted = stepFx g (run level [restartKey] g) closeIn
          moved = stepFx g (walk level DirDown g) initialFx
          idle = stepFx g g initialFx
      in fxFocus restarted == focusTicks && fxFocus moved == 1 && fxFocus idle == 0
  , check "rooms: a cell's room counts in whole screens of 13 by 9" $
      map roomOf [(0, 0), (12, 8), (13, 8), (12, 9), (25, 17), (3, 9)]
        == [(0, 0), (0, 0), (1, 0), (0, 1), (1, 1), (0, 1)]
  , check "rooms: a one-room level never changes room or fades" $
      let g = at level Present (2, 6) False
          g' = run level [press DirUp, noInput, noInput] g
          fxs = take 40 (iterate (stepFx g' g') (stepFx g g' initialFx))
      in all (\f -> fxRoom f == (0, 0) && fxRoomFade f == Nothing) fxs
  , check "rooms: stepping into the next room darkens, swaps halfway, and lightens" $
      let g0 = at level Present (12, 4) False
          g1 = g0 { gMotion = Stepping (13, 4) stepTicks }
          f0 = stepFx g0 g1 initialFx
          fxs = iterate (stepFx g1 g1) f0
          halfway = roomFadeTicks `div` 2
      in fxRoom f0 == (0, 0) && fmap rfRoom (fxRoomFade f0) == Just (1, 0)
         && fxRoom (fxs !! (halfway - 2)) == (0, 0)
         && fxRoom (fxs !! halfway) == (1, 0)
         && roomFadeDark (fxs !! (halfway - 1)) > 0.8
         && fxRoomFade (fxs !! roomFadeTicks) == Nothing
         && fxVisited (fxs !! roomFadeTicks) == [(0, 0), (1, 0)]
  , check "rooms: a restart starts on the spawn's room with nothing to fade" $
      let g = (at level Present (14, 10) False) { gAttempt = 3 }
          g' = (at level Present (2, 6) False) { gAttempt = 4 }
          fx = stepFx g g' initialFx { fxRoom = (1, 1), fxVisited = [(0, 0), (1, 1)] }
      in fxRoom fx == roomOf (2, 6) && fxRoomFade fx == Nothing && fxVisited fx == [roomOf (2, 6)]
  , check "fx: resuming from a pause is not mistaken for a restart" $
      let g = run level [pauseKey] (startPlaying level)
          dirty = Fx 500 Nothing Nothing 0 0 3 9 Nothing Nothing (0, 0) Nothing [(0, 0)] 0
      in fxClock (stepFx g (run level [noInput, pauseKey] g) dirty) == 501
  , check "fx: undoing a pickup stops its burst" $
      let g = walk level DirRight (at level Past (9, 2) False)
          dirty = initialFx { fxBurst = Just (Burst 3 (10, 2)) }
      in fxBurst (stepFx g (run level [undoKey] g) dirty) == Nothing
  , check "fx: undoing a shift cuts straight back, with no wave" $
      let g = shift level (at level Present (2, 6) False)
      in fxWave (stepFx g (run level [undoKey] g) initialFx) == Nothing
  , check "fx: a step's price rises over where it is going" $
      let g = at level Present (2, 6) False
      in fmap cfCell (fxCost (stepFx g (run level [press DirUp] g) initialFx)) == Just (2, 5)
  , check "fx: an action's price floats up, and fades away" $
      let g = at level Present (2, 6) False
          g' = run level [press DirUp] g
          fx = stepFx g g' initialFx
          later = iterate (stepFx g' g') fx !! costLife
      in fmap cfAmount (fxCost fx) == Just (-2) && fxCost later == Nothing
  , check "fx: an undo's refund floats up as a gain" $
      let g = walk level DirUp (at level Present (2, 6) False)
      in fmap cfAmount (fxCost (stepFx g (run level [undoKey] g) initialFx)) == Just 1
  ]

-- | Open floor with a past mirror at (3,2) facing down and a present one at (5,3)
-- facing up, both movable. A crate at (6,3), a pit at (3,4), and a past mirror set
-- in the wall at (7,4), facing left.
mirrorYard :: Level
mirrorYard = case parseLevel 1000
  [ "#########"
  , "#B......#"
  , "#..s....#"
  , "#.....C.#"
  , "#..~...<#"
  , "#########"
  ]
  [ "#########"
  , "#S......#"
  , "#.......#"
  , "#....n..#"
  , "#.....G.#"
  , "#########"
  ] of
    Right l -> l
    Left err -> error err

-- | Where the mirrors stand, for comparing.
mirrorCells :: Game -> [Cell]
mirrorCells = Map.keys . gMirrors

-- | Sleds move from sides/back, jump through their face and keep their facing.
-- Unlike crates they move in their own era; unlike floor they block shifting.
-- Test all forbidden destinations, cross-era mirror collisions, held keys,
-- undo, moving beams and solver keys so the visual hint can use these same rules.
movableMirrorChecks :: [Check]
movableMirrorChecks =
  [ check "parser: a movable mirror stands on floor, in its own era only" $
      Map.lookup (3, 2) (lvMirrors mirrorYard) == Just (Mirror Past DirDown True Nothing)
        && Map.lookup (5, 3) (lvMirrors mirrorYard) == Just (Mirror Present DirUp True Nothing)
        && Map.lookup (7, 4) (lvMirrors mirrorYard) == Just (Mirror Past DirLeft False Nothing)
        && walkable mirrorYard Past (3, 2) && not (walkable mirrorYard Past (7, 4))
        && not (openCell mirrorYard (startPlaying mirrorYard) Past (3, 2))
        && openCell mirrorYard (startPlaying mirrorYard) Present (3, 2)
  , check "push mirror: from its back it moves one cell, for one step's price" $
      let g0 = at mirrorYard Past (3, 1) False
          g = walk mirrorYard DirDown g0
      in gCell g == (3, 2) && mirrorCells g == [(3, 3), (5, 3), (7, 4)]
         && gRemaining g == gRemaining g0 - 2
  , check "push mirror: from its side it moves too, still facing the same way" $
      let g = walk mirrorYard DirRight (at mirrorYard Past (2, 2) False)
      in gCell g == (3, 2) && Map.lookup (4, 2) (gMirrors g) == Just (Mirror Past DirDown True Nothing)
  , check "push mirror: from its face, an open landing means a jump, never a push" $
      let g = walk mirrorYard DirUp (at mirrorYard Past (3, 3) False)
      in gCell g == (3, 1) && mirrorCells g == mirrorCells (startPlaying mirrorYard)
  , check "push mirror: from its face with nowhere to land, nothing moves, it says why, free" $
      let g0 = (at mirrorYard Past (3, 3) False) { gCrates = Set.fromList [(3, 1)] }
          g = walk mirrorYard DirUp g0
      in gCell g == (3, 3) && gMirrors g == gMirrors g0 && gRemaining g == gRemaining g0
         && fmap fbKind (gFeedback g) == Just MirrorFaceSide
  , check "push mirror: one set in the wall never moves, and says so" $
      let g0 = at mirrorYard Past (6, 4) False
          g = walk mirrorYard DirRight g0
      in gCell g == (6, 4) && gMirrors g == gMirrors g0 && gRemaining g == gRemaining g0
         && fmap fbKind (gFeedback g) == Just MirrorFixed
  , check "push mirror: a crate, a pit or a wall stops it, for free" $
      let crated = (at mirrorYard Past (2, 2) False) { gCrates = Set.fromList [(4, 2)] }
          overPit = walks mirrorYard [DirDown] (at mirrorYard Past (3, 1) False)
          walled = (at mirrorYard Past (2, 1) False)
                     { gMirrors = Map.insert (1, 1) (Mirror Past DirDown True Nothing)
                                    (Map.delete (3, 2) (gMirrors (startPlaying mirrorYard))) }
          stays g0 d = let g = walk mirrorYard d g0
                       in gMirrors g == gMirrors g0 && gRemaining g == gRemaining g0
      in stays crated DirRight && stays overPit DirDown && stays walled DirLeft
  , check "push mirror: it never shares a cell with another mirror, even across eras" $
      let g0 = (at mirrorYard Past (5, 1) False)
                 { gMirrors = Map.insert (5, 2) (Mirror Past DirDown True Nothing)
                                (Map.delete (3, 2) (gMirrors (startPlaying mirrorYard))) }
          g = walk mirrorYard DirDown g0
      in gMirrors g == gMirrors g0 && gCell g == (5, 1)
  , check "push mirror: in the other era its cell is plain floor" $
      gCell (walk mirrorYard DirRight (at mirrorYard Present (2, 2) False)) == (3, 2)
  , check "push mirror: a present one moves in the present, though crates rust there" $
      let g = walk mirrorYard DirUp (at mirrorYard Present (5, 4) False)
      in gCell g == (5, 3) && Map.member (5, 2) (gMirrors g)
  , check "push crate: never onto a cell where the other era has a mirror" $
      let g0 = at mirrorYard Past (7, 3) False
          g = walk mirrorYard DirLeft g0
      in gCrates g == gCrates g0 && gCell g == (7, 3) && gRemaining g == gRemaining g0
  , check "shift: never onto a mirror standing in the other era" $
      let g = shift mirrorYard (at mirrorYard Present (3, 2) False)
      in gEra g == Present && fmap fbKind (gFeedback g) == Just (ShiftBlocked (3, 2))
  , check "push mirror: holding a key walks up to it and stops, saying to press again" $
      let g = run mirrorYard (replicate 40 (press DirRight)) (at mirrorYard Past (1, 2) False)
      in gCell g == (2, 2) && mirrorCells g == mirrorCells (startPlaying mirrorYard)
         && fmap fbKind (gFeedback g) == Just PushNeedsPress
  , check "push mirror: undo puts it back exactly, for half the price" $
      let g0 = at mirrorYard Past (2, 2) False
          g = run mirrorYard [undoKey, noInput] (walk mirrorYard DirRight g0)
      in gMirrors g == gMirrors g0 && gCell g == (2, 2) && gRemaining g == gRemaining g0 - 1
  , check "push mirror: its light moves with it" $
      let g0 = at mirrorYard Past (2, 2) False
          g = walk mirrorYard DirRight g0
      in beamCells mirrorYard g0 (3, 2) == [(3, 3), (3, 4)]
         && beamCells mirrorYard g (4, 2) == [(4, 3), (4, 4)]
         && beamCells mirrorYard g (3, 2) == []
  , check "push mirror: the solver tells mirror positions apart" $
      let g0 = at mirrorYard Past (2, 2) False
          g = walk mirrorYard DirRight g0
      in stateKey g /= stateKey g { gMirrors = gMirrors g0 }
  , check "parser: rejects a movable mirror on a crate's cell" $
      either (const True) (const False)
        (parseLevel 100 [ "#####", "#BC.#", "#####" ] [ "#####", "#SnG#", "#####" ])
  ]


-- The way on. --------------------------------------------------------------------
-- The last listed lamp goes out again, and its light leads to a level the hall
-- does not list.
-- | Ending stages, passage geometry and secret-level access through the real Run.
-- Exact boundary ticks pin down the handover from lamp to walking to rush/card.
-- The opened station uses free movement and its own reset; the rush uses separate
-- shift/barrier rules. Both paths must leave campaign completion consistent.
finaleChecks :: [Check]
finaleChecks = case (levelHall, campaign) of
  (Right levels, Right listed) ->
    let third = levels !! 2
        shown = lastVisible levels
        -- One shift from lighting the lamp: in the past, on its footing, with the cell.
        nearWin = (startPlaying third) { gEra = Past, gCell = lvGoal third, gHasCell = True, gPrev = noInput }
        before = Run 2 shown 2 InLevel nearWin
        -- Advance the whole run, not just Game, so cinematic stage clocks and
        -- level handovers happen exactly as they do in Main.
        stepAll r inputs = foldl' (flip (stepRun levels)) r inputs
        lit = stepRun levels space before
        open = stepAll lit (replicate (revealTicks third) noInput)
        -- Nothing distinguishes an ordinary level/hall from any ending stage.
        stageOf r = case runMode r of
          Ending f -> Just (fnStage f)
          _ -> Nothing
        beckoning r = stageOf r == Just Beckon
        -- Enter at the passage mouth, then use Run's barrier-aware rush script.
        -- The final extra ticks cover only the arrival card, not puzzle actions.
        walkedIn = stepAll open (press DirUp : replicate stepTicks noInput)
        arrived = stepAll walkedIn (rushInputs third ++ replicate arrivalTicks noInput)
        finaleOf r = case runMode r of
          Ending f -> Just f
          _ -> Nothing
        passage = lvPassage third
        (gx, gy) = lvGoal third
        way = finaleRun third
        dash = dashLevel third
    in
    [ check "ending: lighting the last listed lamp starts the ending, not the won plate" $
        stageOf lit == Just Lit && gPhase (runGame lit) == Won
        && runCompleted lit == 3 && runIndex lit == 2
    , check "ending: the hall never lists, selects or unlocks the secret level" $
        shown == 2 && lvSecret (levels !! 3) && not (any lvSecret listed)
        && runUnlocked lit == 2
    , check "ending: Enter does nothing while the lamp burns" $
        stageOf (stepRun levels enter lit) == Just Lit
    , check "ending: the keeper walks again once the wall is open, without the cell" $
        beckoning open && gPhase (runGame open) == Playing
        && not (gHasCell (runGame open)) && gCell (runGame open) == lvGoal third
    , check "ending: one tick earlier, the wall is still opening" $
        stageOf (stepAll lit (replicate (revealTicks third - 1) noInput)) == Just Opening
    -- At this particular lamp cell the alternate era is blocked, undo is disabled,
    -- and resetting to the lamp changes nothing. Later checks exercise free
    -- shifting elsewhere and R from a different cell; the label is historical.
    , check "ending: in the opened station there is no shifting, undoing or starting over" $
        let tried = stepAll open [space, noInput, undoKey, noInput, restartKey, noInput]
        in beckoning tried && gEra (runGame tried) == Present
           && gCell (runGame tried) == lvGoal third && gAttempt (runGame tried) == gAttempt (runGame open)
    , check "ending: stepping into the passage starts the rush, and walking is free" $
        stageOf walkedIn == Just Rush && gCell (runGame walkedIn) == head passage
        && gRemaining (runGame walkedIn) == gRemaining (runGame open)
    , check "ending: the rush ends at the real lighthouse, and the secret level begins" $
        runIndex arrived == 3 && runMode arrived == InLevel
        && gPhase (runGame arrived) == Playing && gPartner (runGame arrived) /= Nothing
        && gCell (runGame arrived) == lvSpawn (levels !! 3)
    , check "ending: one tick before that, the card is still showing" $
        stageOf (stepAll walkedIn (rushInputs third ++ replicate (arrivalTicks - 1) noInput)) == Just Arrival
    , check "ending: Escape at any point goes to the hall, the level cleared" $
        all (\r -> let out = stepRun levels lobbyKey r
                   in runMode out == Lobby && runIndex out == 2 && runCompleted out == 3)
            [lit, open, walkedIn]
    , check "ending: with nothing beyond it, the same win just shows the plate" $
        let r = stepRun listed space (Run 2 2 2 InLevel nearWin)
        in runMode r == InLevel && gPhase (runGame r) == Won
    , check "secret level: leaving it returns to the hall on a listed level" $
        let out = stepRun levels lobbyKey (stepRun levels noInput arrived)
        in runMode out == Lobby && runIndex out == shown
    , check "secret level: winning it returns to the hall, and unlocks nothing new" $
        let won = (runGame arrived) { gPhase = Won }
            out = stepRun levels enter (Run 3 shown 3 InLevel won)
        in runMode out == Lobby && runIndex out == shown && runUnlocked out == shown
    , check "secret level: a saved place beyond the hall comes back as its last level" $
        let r = startRunWithProgress levels 3 4
        in runIndex r == shown && runUnlocked r == shown && runMode r == Title
    , check "secret level: asked for by number, it opens straight away" $
        let r = startRunAt levels 3 3
        in runIndex r == 3 && runMode r == InLevel && gPhase (runGame r) == Playing
    , check "secret level: the hall cannot reach it, however far the player has got" $
        let r = foldl' (flip (stepRun levels)) (startRunWithProgress levels 2 4)
                  [enter, noInput, press DirDown, noInput, press DirDown]
        in runIndex r == shown && runMode r == Lobby
    , check "ending: the passage starts beside the lighthouse and runs west out of the map" $
        case way of
          Nothing -> False
          Just w ->
            take 1 passage == [(gx, gy - 1)] && frDir w == DirLeft && frEdge w == (0, gy - 1)
            && all ((== gy - 1) . snd) passage
            && not (any (\c -> walkable third Past c || walkable third Present c) passage)
    , check "ending: the light's way across the station is all floor once it is open" $
        let r = revealedLevel third
        in wayCells third == [ (x, gy - 1) | x <- [gx, gx - 1 .. 0] ]
           && all (walkable r Present) (wayCells third)
           && all (`notElem` wayCells third) (Set.toList (gCrates nearWin))
    , check "ending: walls break in the order the light reaches them" $
        let ticks = map (cellOpenTick third) [0 .. length passage - 1]
        in and (zipWith (<) ticks (drop 1 ticks)) && last ticks < openTicks third
    , check "ending: the open passage is floor in the present, and nowhere else changes" $
        let r = revealedLevel third
        in lvPresent r == Set.union (lvPresent third) (Set.fromList passage)
           && lvPast r == lvPast third && ruStepCost (lvRules r) == 0
    , check "ending: stepping onto the way anywhere starts the rush" $
        let further = stepAll open (press DirUp : replicate stepTicks noInput)
            stood = (runGame open) { gCell = (8, gy) }
            fromSide = stepAll (open { runGame = stood }) (press DirUp : replicate stepTicks noInput)
        in stageOf further == Just Rush && stageOf fromSide == Just Rush
           && gCell (runGame fromSide) == (8, gy - 1)
    , check "rush: a keeper who changes era at each barrier gets through, never taken back" $
        let r = stepAll walkedIn (rushInputs third)
            shifts = length (filter tiShift (rushInputs third))
        in stageOf r == Just Arrival && shifts == length barriers
           && maybe False ((== 0) . dsFails . fnDash) (finaleOf r)
    , check "rush: a keeper who never changes era is held at the first barrier, and taken back" $
        let r = stepAll walkedIn (replicate 1500 noInput)
        in stageOf r == Just Rush
           && maybe False (\f -> dsFails (fnDash f) > 0 && dsAlong (fnDash f) < 20) (finaleOf r)
    , check "rush: the era does not change inside the station, nor into a barrier" $
        case way of
          Nothing -> False
          Just w ->
            let start = head (wayCells third)
                g0 = (runGame walkedIn) { gPrev = noInput }
                inside = stepDash w start space g0 restingDash
                (a, e) = head (barriersFrom w start)
                atBarrier = restingDash { dsAlong = a, dsScroll = a }
                into = stepDash w start space g0 { gEra = otherEra e } atBarrier
            in fmap (gEra . snd) inside == Just Present && fmap (dsRefused . fst) inside == Just 24
               && fmap (gEra . snd) into == Just (otherEra e)
    , check "ending: the causeway is open all the way in one era or the other, never both shut" $
        case way of
          Nothing -> False
          Just w ->
            let (mx, my) = frMouth w
                (ax, _) = frArrival w
                line = [ (x, my) | x <- [ax .. mx] ] ++ [frTower w]
                open' c = walkable dash Present c || walkable dash Past c
                shutIn era' = [ outFrom w i 0 | (i, e) <- barriers, e == era' ]
            in all open' line
               && all (\c -> not (walkable dash Present c) && walkable dash Past c) (shutIn Present)
               && all (\c -> not (walkable dash Past c) && walkable dash Present c) (shutIn Past)
    , check "ending: in the opened station the era still shifts, for nothing" $
        let shifted = stepAll open { runGame = (runGame open) { gCell = (8, gy) } } [space, noInput]
        in beckoning shifted && gEra (runGame shifted) == Past
           && gRemaining (runGame shifted) == gRemaining (runGame open)
    , check "ending: R brings the keeper back to the lamp from any corner" $
        let stuck = open { runGame = (runGame open) { gCell = (6, 10), gEra = Past } }
            back = stepRun levels restartKey stuck
        in beckoning back && gCell (runGame back) == lvGoal third && gEra (runGame back) == Present
    , check "ending: stepping onto the way in the past does not start the rush" $
        let past = open { runGame = (runGame open) { gCell = (8, gy), gEra = Past } }
            r = stepAll past (press DirUp : replicate stepTicks noInput)
        in beckoning r && gCell (runGame r) == (8, gy - 1)
    , check "ending: a held key that lands on the way starts the rush there" $
        let r = stepAll open { runGame = (runGame open) { gCell = (8, gy) } } (replicate (3 * stepTicks) (press DirUp))
        in stageOf r == Just Rush && gCell (runGame r) == (8, gy - 1) && gMotion (runGame r) == Settled
    , check "ending: a crate left on the way is broken by the light" $
        let crated = Run 2 shown 2 InLevel nearWin { gCrates = Set.insert (8, gy - 1) (gCrates nearWin) }
            r = stepAll (stepRun levels space crated) (replicate (revealTicks third) noInput)
        in beckoning r && (8, gy - 1) `Set.notMember` gCrates (runGame r)
           && Set.size (gCrates (runGame r)) == Set.size (gCrates nearWin)
    , check "ending: the keeper faces the way the light went when they walk again" $
        gFacing (runGame open) == DirLeft
    ]
  (Left err, _) -> [check ("hall: parses: " ++ err) False]
  (_, Left err) -> [check ("campaign: parses: " ++ err) False]

-- Two keepers. -------------------------------------------------------------------

-- | The first map for two, kept as the fixture for the two-keeper rules: open rooms
-- where the cell, the lamp and the shared era are easy to reach.
duoFixture :: Either String Level
duoFixture = parseLevel 100
  [ "#############"
  , "#.....B.....#"
  , "#.....#.....#"
  , "#...........#"
  , "#....#.#....#"
  , "#....#.#....#"
  , "#....#.#....#"
  , "#...........#"
  , "#############"
  ]
  [ "#############"
  , "#####...#####"
  , "#.S.#####.T.#"
  , "#...........#"
  , "#....###....#"
  , "#....#G#....#"
  , "#....#.#....#"
  , "#...........#"
  , "#############"
  ]

-- | Two keepers share board, era, budget and history while keeping independent
-- motion/input. Check concurrent steps, mutual blocking, pickup ownership,
-- shared shifts and undo of overlapping actions, then verify event-key routing.
duoChecks :: [Check]
duoChecks = case duoFixture of
  Right duo ->
    let start = startPlaying duo
        -- Set both the legacy aggregate and the actor-specific field. A single
        -- tick may contain both directions; Game.Logic resolves shared collisions.
        wasd d = noInput { tiDir = Just d, tiDir1 = Just d }
        arrows d = noInput { tiDir = Just d, tiDir2 = Just d }
        both d1 d2 = noInput { tiDir = Just d1, tiDir1 = Just d1, tiDir2 = Just d2 }
        partnerAt g = fmap ptCell (gPartner g)
        -- fmap updates a partner only when present, preserving the Maybe shape.
        -- Direct placement isolates the rule being tested from route setup.
        moveTo c g = g { gPartner = fmap (\p -> p { ptCell = c }) (gPartner g) }
        settle = replicate stepTicks noInput
        budget = lvBudget duo
    in
    [ check "two keepers: both start where the map says" $
        gCell start == lvSpawn duo && partnerAt start == lvSpawn2 duo && lvSpawn2 duo == Just (10, 2)
    , check "two keepers: W A S D moves only the first" $
        let g = run duo (wasd DirDown : settle) start
        in gCell g == (2, 3) && partnerAt g == Just (10, 2) && gRemaining g == budget - 2
    , check "two keepers: the arrows move only the second" $
        let g = run duo (arrows DirDown : settle) start
        in gCell g == (2, 2) && partnerAt g == Just (10, 3) && gRemaining g == budget - 2
    , check "two keepers: both can walk at once, each paying for their own step" $
        let g = run duo (both DirDown DirDown : settle) start
        in gCell g == (2, 3) && partnerAt g == Just (10, 3) && gRemaining g == budget - 4
    , check "two keepers: one cannot walk into the other" $
        let g0 = moveTo (10, 3) start { gCell = (9, 3) }
            g = run duo (wasd DirRight : settle) g0
            h = run duo (arrows DirLeft : settle) g0
        in gCell g == (9, 3) && partnerAt h == Just (10, 3)
    , check "two keepers: one Space takes both into the other era, for one price" $
        let g = run duo (space : replicate shiftTicks noInput) start
        in gEra g == Past && gRemaining g == budget - ruShiftCost (lvRules duo)
           && gCell g == (2, 2) && partnerAt g == Just (10, 2)
    , check "two keepers: a shift waits for both to have floor in the other era" $
        let g0 = moveTo (9, 1) start { gEra = Past }
            g = run duo [space, noInput] g0
        in gEra g == Past && gRemaining g == gRemaining g0
           && fmap fbKind (gFeedback g) == Just (ShiftBlocked (9, 1))
    , check "two keepers: the second keeper picks up the cell, and lights the lamp" $
        let g0 = moveTo (5, 1) start { gEra = Past }
            picked = run duo (arrows DirRight : settle) g0
            atLamp = moveTo (6, 5) picked
            lit = run duo [space, noInput] atLamp
        in fmap ptHasCell (gPartner picked) == Just True && not (gHasCell picked)
           && objectiveText picked == "Bring it to the lighthouse in the present."
           && gPhase lit == Won
    , check "two keepers: only one of them can hold the cell" $
        let g0 = moveTo (5, 1) start { gEra = Past, gHasCell = True }
            g = run duo (arrows DirRight : settle) g0
        in fmap ptHasCell (gPartner g) == Just False && gHasCell g
    , check "two keepers: undo takes back the last step, whoever took it" $
        let walked = run duo (wasd DirDown : settle ++ arrows DirDown : settle) start
            once = run duo [undoKey, noInput] walked
            twice = run duo [undoKey, noInput] once
        in gCell once == (2, 3) && partnerAt once == Just (10, 2)
           && gCell twice == (2, 2) && partnerAt twice == Just (10, 2)
    , check "two keepers: the second keeper pushes a crate by the same rules" $
        case parseLevel 100 ["########", "#B.C...#", "#......#", "########"]
                            ["########", "#S.....#", "#G...T.#", "########"] of
          Left _ -> False
          Right small ->
            let g0 = (startPlaying small) { gEra = Past, gPrev = noInput }
                g1 = g0 { gPartner = fmap (\p -> p { ptCell = (4, 1) }) (gPartner g0) }
                pushed = run small (arrows DirLeft : settle) g1
                rusted = run small (arrows DirLeft : settle) g1 { gEra = Present }
            in gCrates pushed == Set.singleton (2, 1) && fmap ptCell (gPartner pushed) == Just (3, 1)
               && gCrates rusted == Set.singleton (3, 1)
    , check "two keepers: the keys reach the right keeper from the keyboard" $
        let i = tickInput (keyDown KUp (keyDown KD noKeys))
        in tiDir1 i == Just DirRight && tiDir2 i == Just DirUp && tiDir i == Just DirUp
    , check "parser: a level has at most two keepers" $
        case parseLevel 100 ["######", "#B...#", "######"] ["######", "#STTG#", "######"] of
          Left _ -> True
          Right _ -> False
    , check "parser: a level for one has no second keeper" $
        all (\l -> lvSpawn2 l == Nothing) (either (const []) id campaign)
    , check "two keepers: undo of one keeper keeps the cell the other just picked up" $
        let g0 = moveTo (5, 1) start { gEra = Past }
            both' = run duo (arrows DirRight : wasd DirDown : settle) g0
            undone = run duo [undoKey, noInput] both'
        in fmap ptHasCell (gPartner both') == Just True
           && fmap ptCell (gPartner undone) == Just (6, 1) && fmap ptHasCell (gPartner undone) == Just True
           && gCell undone == (2, 2)
    , check "two keepers: scripts and the solver's moves drive the first keeper" $
        let g = run duo (actionInputs (Go DirDown) ++ [noInput]) start
        in gCell g == (2, 3) && partnerAt g == Just (10, 2)
    ]
  _ -> [check "two keepers: the fixture parses" False]

-- Sound. -------------------------------------------------------------------------
-- What is heard is worked out by comparing ticks, like the effects, so these play
-- the real rules and listen.

-- | The run and its effects after each tick, and what that tick sounded.
traceRun :: [Level] -> Run -> [TickInput] -> [((Run, Fx), [Cue])]
traceRun levels r0 inputs = zip (drop 1 states) (zipWith (cues levels) states (drop 1 states))
  where
    -- scanl includes the initial state; dropping its first element aligns each
    -- post-tick state with cues computed from the preceding/current pair.
    states = scanl tick (r0, initialFx) inputs
    tick (r, fx) i = let r' = stepRun levels i r in (r', stepFx (runGame r) (runGame r') fx)

-- | What each tick of play in one level sounded.
playTrace :: Level -> Game -> [TickInput] -> [[Cue]]
playTrace l g inputs = map snd (traceRun [l] (Run 0 0 0 InLevel g) inputs)

-- | Cue decisions, not waveform quality: action counts must equal heard cues,
-- with one pickup/win and no spurious footstep for a mirror jump. Following
-- groups extend the same before/after comparison to ending and partner actions.
soundChecks :: Level -> [Check]
soundChecks level =
  [ check "sound: the route sounds every step and shift, and the pickup and the lamp once each" $
      let heard = concat (playTrace level (startPlaying level) (routeInputs intendedRoute))
      in length [ () | CueStep {} <- heard ] == 36
         && length [ () | CueShift _ <- heard ] == 6
         && length (filter (== CuePickup) heard) == 1
         && length (filter (== CueLamp) heard) == 1
         && CueBump `notElem` heard && CueRefused `notElem` heard
  , check "sound: standing still is silent" $
      all null (playTrace level (startPlaying level) (replicate 120 noInput))
  , check "sound: a footstep sounds the era it is taken in" $
      let heard = concat (playTrace level (startPlaying level) (actionInputs ShiftEra ++ actionInputs (Go DirRight)))
      in CueShift Past `elem` heard && [ e | CueStep e _ _ _ <- heard ] == [Past]
  , check "sound: stepping through a mirror sounds the mirror, not a footstep" $
      let heard = concat (playTrace glass (at glass Past (6, 6) False) (actionInputs (Go DirUp) ++ [noInput]))
      in CueJump False `elem` heard && null [ () | CueStep {} <- heard ] && CuePickup `elem` heard
  , check "sound: pushing a movable mirror sounds its sled, not a crate" $
      let heard = concat (playTrace twoAlignments (at twoAlignments Past (4, 4) False) (actionInputs (Go DirRight)))
      in CueMirrorPush `elem` heard && CuePush `notElem` heard
  , check "sound: undo and restart each sound once, and nothing else" $
      let moved = walk level DirRight (shift level (startPlaying level))
      in concat (playTrace level moved [undoKey, noInput]) == [CueUndo]
         && concat (playTrace level moved [restartKey, noInput]) == [CueRestart]
  , check "sound: a shift onto a wall is refused aloud" $
      case [ c | c <- Set.toList (lvPresent level), not (walkable level Past c) ] of
        c : _ -> CueRefused `elem` concat (playTrace level (at level Present c False) [space, noInput])
        [] -> False
  , check "sound: walking into a wall bumps" $
      case [ (c, d) | c <- Set.toList (lvPresent level), d <- [minBound .. maxBound]
                    , not (walkable level Present (stepCell d c)) ] of
        (c, d) : _ -> CueBump `elem` concat (playTrace level (at level Present c False) [press d, noInput])
        [] -> False
  ] ++ endingSounds ++ twoKeeperSounds

-- | Level 4, played through by both keepers.
twoKeeperSounds :: [Check]
twoKeeperSounds = case levelHall of
  Right levels | length levels > 3, Just _ <- lvSpawn2 (levels !! 3) ->
    let four = levels !! 3
        route = maybe [] snd (drGoal (duoSearch four (const True) (startPlaying four)))
        inputs = concatMap duoInputs route ++ [noInput]
        heard = concatMap snd (traceRun levels (Run 3 2 3 InLevel (startPlaying four)) inputs)
        moves = length [ () | a <- route, a /= ShiftBoth ]
        steps who = [ () | CueStep _ _ _ w <- heard, w == who ]
        jumps = [ turned | CueJump turned <- heard ]
    in
    [ check "sound: in level 4 every move is heard, each keeper's steps as their own" $
        not (null route)
        && length (steps Yellow) + length (steps Red) + length jumps == moves
        && not (null (steps Yellow)) && not (null (steps Red))
    , check "sound: level 4's corner mirrors turn the keeper, and sound it" $
        not (null jumps) && and jumps
    , check "sound: level 4 sounds its shifts, the pickup and the lamp" $
        length [ () | CueShift _ <- heard ] == length (filter (== ShiftBoth) route)
        && length (filter (== CuePickup) heard) == 1
        && length (filter (== CueLamp) heard) == 1
    ]
  _ -> [check "sound: level 4 is a level for two" False]

-- | Cinematic sounds and bed levels must track the same stage clocks as drawing.
-- Replay success and failure paths; inspect exact lamp/wall ticks, cue order,
-- corridor-lamp indices and silence after arrival rather than listening to audio.
endingSounds :: [Check]
endingSounds = case levelHall of
  Left _ -> [check "sound: the level hall parses" False]
  Right levels ->
    let third = levels !! 2
        nearWin = (startPlaying third) { gEra = Past, gCell = lvGoal third, gHasCell = True, gPrev = noInput }
        before = Run 2 (lastVisible levels) 2 InLevel nearWin
        walkIn = space : replicate (revealTicks third) noInput ++ press DirUp : replicate stepTicks noInput
        whole = traceRun levels before (walkIn ++ rushInputs third ++ replicate arrivalTicks noInput)
        heard = concatMap snd whole
        finaleOf r = case runMode r of
          Ending f -> Just f
          _ -> Nothing
        -- The finale after the tick that sounded this.
        when' c = [ f | ((r, _), cs) <- whole, c `elem` cs, Just f <- [finaleOf r] ]
        -- Filter only milestone cues for the ordering assertion; ordinary rush
        -- footsteps, shifts and weather may legitimately occur between them.
        moment c = case c of
          CueLamp -> True
          CueFlare -> True
          CueLampDies -> True
          CueLightGoes -> True
          CueWallBreaks _ -> True
          CueRushBegins -> True
          CueArrival -> True
          CueCard -> True
          _ -> False
        passage = lvPassage third
        -- Select continuous bed settings by post-tick finale predicate, letting
        -- assertions cover every tick of a stage rather than one sampled instant.
        bedsAt p = [ beds levels r | ((r, _), _) <- whole, Just f <- [finaleOf r], p f ]
        rushing f = fnStage f == Rush
    in
    [ check "sound: the ending's moments sound once each, in order" $
        filter moment heard
          == [CueLamp, CueFlare, CueLampDies, CueLightGoes] ++ map CueWallBreaks [0 .. length passage - 1]
             ++ [CueRushBegins, CueArrival, CueCard]
    , check "sound: the lamp dies aloud on the tick it goes dark" $
        map (\f -> (fnStage f, fnAge f)) (when' CueLampDies) == [(Flare, lampDiesAt)]
    , check "sound: each wall breaks aloud as the light reaches it" $
        and [ map (\f -> (fnStage f, fnAge f)) (when' (CueWallBreaks k)) == [(Opening, cellOpenTick third k)]
            | k <- [0 .. length passage - 1] ]
    , check "sound: every lamp of the corridor catches once, in order, as the keeper passes" $
        [ n | CueLampCatches n <- heard ] == [0 .. lampCount - 1]
    , check "sound: a keeper who reads the barriers shifts at each, and is never held up" $
        length [ () | ((r, _), cs) <- whole, Just f <- [finaleOf r], rushing f, CueShift _ <- cs ]
          == length barriers
        && CueBarrier `notElem` heard && CueCaught `notElem` heard
    , check "sound: a keeper who never shifts is held up, then taken back, and hears both" $
        let stuck = concatMap snd (traceRun levels before (walkIn ++ replicate 1500 noInput))
        in CueBarrier `elem` stuck && CueCaught `elem` stuck
    , check "sound: while the light breaks out, only the light is heard" $
        all (\b -> b { bdBeam = 0 } == silence) (bedsAt (\f -> fnStage f == Opening))
        && any ((> 0) . bdBeam) (bedsAt (\f -> fnStage f == Opening))
    , check "sound: the rush has its beat and its wind, and both fall away at the real lighthouse" $
        any (\b -> bdPulse b > 0 && bdWind b > 0) (bedsAt rushing)
        && case reverse (bedsAt ((== Arrival) . fnStage)) of
             b : _ -> bdPulse b == 0 && bdWind b == 0 && bdSea b == 0 && bdStorm b == 0 && bdDrone b == 0
             [] -> False
    ]

-- Corner mirrors. ------------------------------------------------------------------

-- | An open yard with a corner mirror at (5,4), in the present, looking north and
-- east, and a crate at (8,4) at the end of its east light.
cornerYard :: Either String Level
cornerYard = parseLevel 100
  [ "##########"
  , "#B.......#"
  , "#........#"
  , "#........#"
  , "#.......C#"
  , "#........#"
  , "#........#"
  , "##########"
  ]
  [ "##########"
  , "#S.......#"
  , "#........#"
  , "#........#"
  , "#....L...#"
  , "#........#"
  , "#G.......#"
  , "##########"
  ]

-- | Corner mirrors preserve distance while turning through the other face.
-- Verify both beams and both directions, blocked landing fallback, fixed backs
-- and era ownership so a bend cannot accidentally behave like a straight mirror.
cornerChecks :: [Check]
cornerChecks = case cornerYard of
  Left err -> [check ("corner mirrors: the yard parses: " ++ err) False]
  Right yard ->
    let stand c = at yard Present c False
    in
    [ check "parser: L J 7 F are corner mirrors set in the wall, with two faces" $
        case parseLevel 100 ["#######", "#B....#", "#.....#", "#######"] ["#######", "#SLJ7F#", "#G....#", "#######"] of
          Left _ -> False
          Right l ->
            map (\c -> fmap (\m -> (mFace m, mBend m, mMovable m)) (Map.lookup c (lvMirrors l))) [(2, 1), (3, 1), (4, 1), (5, 1)]
              == [ Just (DirUp, Just DirRight, False), Just (DirUp, Just DirLeft, False)
                 , Just (DirDown, Just DirLeft, False), Just (DirDown, Just DirRight, False) ]
            && not (walkable l Present (2, 1))
    , check "corner mirrors: light leaves by both faces, until something stops it" $
        beamsOf yard (stand (1, 1)) (5, 4) == [ (DirUp, [(5, 3), (5, 2), (5, 1)]), (DirRight, [(6, 4), (7, 4)]) ]
    , check "corner mirrors: two cells up its north light, you come out two cells east" $
        let g = applyAction yard (Go DirDown) (stand (5, 2))
        in gCell g == (7, 4) && gRemaining g == lvBudget yard - ruJumpCost (lvRules yard)
    , check "corner mirrors: and the other way, from the east light out of the north face" $
        gCell (applyAction yard (Go DirLeft) (stand (7, 4))) == (5, 2)
    , check "corner mirrors: a landing that is taken shows a cross, and the key only walks" $
        fmap jOpen (mirrorAhead yard (stand (5, 1)) DirDown) == Just False
        && gCell (applyAction yard (Go DirDown) (stand (5, 1))) == (5, 2)
    , check "corner mirrors: its back is a wall that never moves" $
        let g = applyAction yard (Go DirRight) (stand (4, 4))
        in gCell g == (4, 4) && fmap fbKind (gFeedback g) == Just MirrorFixed
    , check "corner mirrors: only in their own era" $
        mirrorAhead yard (at yard Past (5, 2) False) DirDown == Nothing
    ]

-- The secret level for two. --------------------------------------------------------

-- | W A S D move yellow, w a s d red (the arrows), @_@ shifts both.
duoRoute :: String -> [DuoAction]
-- Only trusted route strings are used: the fallback treats every character
-- outside the eight direction letters as a shared shift, including underscore.
duoRoute = map key
  where
    key c = case c of
      'W' -> Go1 DirUp
      'A' -> Go1 DirLeft
      'S' -> Go1 DirDown
      'D' -> Go1 DirRight
      'w' -> Go2 DirUp
      'a' -> Go2 DirLeft
      's' -> Go2 DirDown
      'd' -> Go2 DirRight
      _   -> ShiftBoth

-- | Red carries: yellow crosses first, red goes through the vault, yellow pushes
-- the gate, red comes home by the west mirror.
redCarries, yellowCarries :: [DuoAction]
redCarries = duoRoute $ concat
  [ "DDDWWWWS", "_", "wwawaass", "w", "aaadds", "d", "WWWAA", "DDD", "ssd", "_", "s", "www", "_", "ddwwwd", "_" ]
-- | Yellow carries, through red's mirror; red pushes the gate.
yellowCarries = duoRoute $ concat
  [ "DDDWWWWS", "_", "d", "WWWWAWAASS", "W", "AAADDS", "D", "awaa", "ddd", "SSD", "_", "S", "WWW", "_", "DDWWWD", "_" ]

-- | The secret puzzle needs cooperation, not just two available avatars.
-- Full/restricted searches establish par and the impossibility of either solo
-- keeper; authored routes prove either may carry. Bad ordering must remain a
-- dead end, since the two gate crates deliberately remove crossings for good.
twoKeepersChecks :: [Check]
twoKeepersChecks = case secretLevels of
  Right (l : _) ->
    let start = startPlaying l
        -- Share the expensive unrestricted search; each restricted search asks
        -- a different question and therefore needs its own graph exploration.
        full = duoSearch l (const True) start
        play route = foldl' (flip (applyDuo l)) start route
        replay route = run l (concatMap duoInputs route ++ [noInput]) start
        -- Removing the partner tests solo solvability; restricting Go1/Go2 below
        -- instead leaves the stationary partner as an obstacle and possible weight.
        alone c = l { lvSpawn = c, lvSpawn2 = Nothing }
        onlyYellow a = case a of { Go2 _ -> False; _ -> True }
        onlyRed a = case a of { Go1 _ -> False; _ -> True }
    in
    [ check "two keepers: the secret level is 13 x 9, unlisted, with both keepers on the map" $
        lvWidth l == 13 && lvHeight l == 9 && lvSecret l
        && lvSpawn l == (2, 6) && lvSpawn2 l == Just (10, 5)
    , check "two keepers: a corner mirror in each era, one on each island" $
        Map.lookup (5, 7) (lvMirrors l) == Just (Mirror Present DirUp False (Just DirRight))
        && Map.lookup (7, 1) (lvMirrors l) == Just (Mirror Past DirDown False (Just DirLeft))
    , check "two keepers: two can light it, and the par is the cheapest way" $
        fmap fst (drGoal full) == Just (lvPar l)
    , check "two keepers: red alone cannot, nor yellow alone" $
        srGoal (search (alone (10, 5)) (\_ _ -> True) (startPlaying (alone (10, 5)))) == Nothing
        && srGoal (search (alone (2, 6)) (\_ _ -> True) (startPlaying (alone (2, 6)))) == Nothing
    , check "two keepers: nor one, while the other stands still" $
        drGoal (duoSearch l onlyYellow start) == Nothing && drGoal (duoSearch l onlyRed start) == Nothing
    , check "two keepers: red can carry the cell" $
        gPhase (play redCarries) == Won && gPhase (replay redCarries) == Won
    , check "two keepers: and so can yellow, through red's mirror" $
        gPhase (play yellowCarries) == Won && gPhase (replay yellowCarries) == Won
    , check "two keepers: opening the vault door takes the crossing away for good" $
        let opened = play (take 25 redCarries)
        in Set.member (5, 2) (gCrates opened) && fmap ptCell (gPartner opened) == Just (4, 2)
    , check "two keepers: pushing the gate first shuts the vault for good" $
        let g = play (duoRoute "_waa")
        in Set.member (7, 4) (gCrates g) && drGoal (duoSearch l (const True) g) == Nothing
    , check "two keepers: opening the vault before yellow crosses is a dead end" $
        let g = play (duoRoute "_wwawaasswaaaddsd")
        in fmap ptCell (gPartner g) == Just (4, 2) && drGoal (duoSearch l (const True) g) == Nothing
    ]
  _ -> [check "two keepers: the secret level parses" False]

-- | The secret level's numbers, from the two-keeper search.
reportDuo :: IO ()
reportDuo = case secretLevels of
  Right (l : _) -> do
    let r = duoSearch l (const True) (startPlaying l)
    case drGoal r of
      Nothing -> putStrLn ("level:  " ++ lvName l ++ ": NO ROUTE for two, " ++ show (drStates r) ++ " states")
      Just (n, route) -> do
        putStrLn ("level:  " ++ lvName l ++ ": par " ++ show n ++ " for two, budget " ++ show (lvBudget l)
                  ++ ", " ++ show (length route) ++ " actions, "
                  ++ show (length (filter (== ShiftBoth) route)) ++ " shifts, "
                  ++ show (drStates r) ++ " states")
        putStrLn ("level:  " ++ concatMap duoLabel route)
  _ -> pure ()

-- Her orders: the saves ---------------------------------------------------------

ordersChecks :: [Check]
ordersChecks = case levelHall of
  Left err -> [check ("orders: the levels parse: " ++ err) False]
  Right levels ->
    let steps cp inputs = foldl' (flip (stepCampaign levels)) cp inputs
        tap i = [i, noInput]
        right = noInput { tiDir = Just DirRight }
        cancelKey = noInput { tiCancel = True }
        order c s = newOrder { odCompleted = c, odSelected = s, odTicks = 14 * 60 * 60 }
        book = Book [Just (order 2 2), Just (order 0 0), Nothing] 0
        title b = Campaign b closedDesk (startRunWithProgress levels 2 2)
        desk = steps (title book) (tap enter)
        isIntro cp = case runMode (cpRun cp) of
          Intro _ _ -> True
          _ -> False
        asking cp = case dkStep (cpDesk cp) of
          Asking _ _ -> True
          _ -> False
        held n = replicate n enter
        cancelling = steps desk (tap cancelKey)
        torn = steps cancelling (held holdTicks ++ replicate tearTicks noInput)
        shiftOnce level = let g0 = (startPlaying level) { gPrev = noInput } in (g0, applyAction level ShiftEra g0)
    in
    [ check "orders: with no order yet, Enter at the title takes the first one and plays the opening" $
        let cp = steps (Campaign emptyBook closedDesk (startRun levels 0)) (tap enter)
        in isIntro cp && bkActive (cpBook cp) == 0 && activeOrder (cpBook cp) /= Nothing
    , check "orders: with orders, Enter at the title opens the desk on the order last played" $
        runMode (cpRun desk) == Orders && dkSel (cpDesk desk) == 0
    , check "orders: while the instructions are open, Enter at the title does nothing" $
        let shown = (title book) { cpRun = (startRunWithProgress levels 2 2) { runGame = (runGame (startRunWithProgress levels 2 2)) { gShowInstructions = True } } }
        in runMode (cpRun (steps shown (tap enter))) == Title
    , check "orders: right chooses the next ticket, and stops at the last" $
        dkSel (cpDesk (steps desk (tap right))) == 1
        && dkSel (cpDesk (steps desk (concat (replicate 5 (tap right))))) == slotCount - 1
    , check "orders: Enter on an order with rooms cleared opens the hall where it was" $
        let cp = steps desk (tap enter)
        in runMode (cpRun cp) == Lobby && runCompleted (cpRun cp) == 2 && runIndex (cpRun cp) == 2
    , check "orders: Enter on an order with nothing cleared plays the opening, and makes it the one in play" $
        let cp = steps desk (tap right ++ tap enter)
        in isIntro cp && bkActive (cpBook cp) == 1 && runCompleted (cpRun cp) == 0
    , check "orders: Enter on an empty slot prints a new order, then plays the opening" $
        let printingNow = steps desk (concat (replicate 2 (tap right)) ++ tap enter)
            done = steps printingNow (replicate printTicks noInput)
        in runMode (cpRun printingNow) == Orders && orderAt 2 (cpBook printingNow) == Nothing
           && isIntro done && bkActive (cpBook done) == 2 && orderAt 2 (cpBook done) == Just newOrder
    , check "orders: Escape on the desk goes back to the title" $
        runMode (cpRun (steps desk (tap lobbyKey))) == Title
    , check "orders: Escape in the hall goes back to the desk, not the title" $
        let hall = steps desk (tap enter)
        in runMode (cpRun (steps hall (tap lobbyKey))) == Orders
    , check "orders: Delete asks first; a tap of Enter tears nothing" $
        let tapped = steps cancelling (tap enter ++ replicate 30 noInput)
        in asking cancelling && asking tapped && orderAt 0 (cpBook tapped) /= Nothing
    , check "orders: Delete on an empty slot does nothing" $
        dkStep (cpDesk (steps desk (concat (replicate 2 (tap right)) ++ tap cancelKey))) == Choosing
    , check "orders: Escape while she asks keeps the order" $
        let kept = steps cancelling (held 20 ++ tap lobbyKey)
        in dkStep (cpDesk kept) == Choosing && orderAt 0 (cpBook kept) /= Nothing
    , check "orders: letting go of Enter takes the tear back" $
        case dkStep (cpDesk (steps cancelling (held 30 ++ replicate 30 noInput))) of
          Asking _ h -> h == 0
          _ -> False
    , check "orders: Enter already down when she asks does not count towards the tear" $
        case dkStep (cpDesk (steps (title book) ([enter, enter { tiCancel = True }] ++ held 80))) of
          Asking _ h -> h == 0
          _ -> False
    , check "orders: Enter held long enough tears the order off and says so" $
        orderAt 0 (cpBook torn) == Nothing && dkNote (cpDesk torn) /= Nothing
        && dkStep (cpDesk torn) == Choosing && orderAt 1 (cpBook torn) /= Nothing
    , check "orders: cancelling the order in play leaves the run fresh" $
        runCompleted (cpRun torn) == 0 && runMode (cpRun torn) == Orders
    , check "orders: progress made in play is written into the order" $
        let hall = steps desk (tap enter)
            won = (cpRun hall) { runMode = InLevel, runIndex = 2, runGame = ((startPlaying (levels !! 2)) { gPhase = Won, gPrev = noInput }) }
            cp = steps hall { cpRun = won } [noInput]
        in fmap odCompleted (activeOrder (cpBook cp)) == Just 3
    , check "orders: time on the road counts in play, not at the title or the desk" $
        let r = startRun levels 0
            inLevel = r { runMode = InLevel }
            b = Book [Just newOrder, Nothing, Nothing] 0
        in fmap odTicks (activeOrder (tally inLevel inLevel b)) == Just 1
           && tally r r b == b && tally r r { runMode = Orders } b == b
    , check "orders: an era shift counts; an undo does not" $
        let level = levels !! 0
            (g0, g1) = shiftOnce level
            playing g = (startRun levels 0) { runMode = InLevel, runGame = g }
            b = Book [Just newOrder, Nothing, Nothing] 0
            -- An undo takes the shift back: the era changes and the history shrinks.
            undoneFrom = g1
            undone = g0
        in gEra g1 /= gEra g0 && length (gHistory g1) > length (gHistory g0)
           && fmap odShifts (activeOrder (tally (playing g0) (playing g1) b)) == Just 1
           && fmap odShifts (activeOrder (tally (playing undoneFrom) (playing undone) b)) == Just 0
    , check "orders: the save before the orders becomes the first order" $
        bookFromProgress 2 1 == Book [Just newOrder { odCompleted = 2, odSelected = 1 }, Nothing, Nothing] 0
    , check "orders: a book read back is made to fit: three slots, progress in range" $
        normaliseBook (length levels) (Book [Just newOrder { odCompleted = 99, odSelected = 99, odTicks = -5 }] 7)
          == Book [Just newOrder { odCompleted = length levels, odSelected = length levels - 1 }, Nothing, Nothing] 2
    , check "orders: a book survives being written and read" $
        let b = Book [Just (order 2 1) { odSaved = Just (Stamp 11 8 21 14) }, Nothing, Just newOrder] 2
        in read (show b) == b
    , check "orders: she names what a cancel would lose" $
        let said = cancelLine levels 1 (order 2 2)
        in "ORDER 1" `isInfixOf` said && "two rooms" `isInfixOf` said && "14 minutes" `isInfixOf` said
           && "DELIVERED" `isInfixOf` cancelLine levels 2 (order 3 2)
    , check "orders: the tear is heard as it goes, and when it comes off" $
        let a = cpDesk (steps cancelling (held 7))
            b = cpDesk (steps cancelling (held 8))
            last1 = cpDesk (steps cancelling (held (holdTicks - 1)))
            off = cpDesk (steps cancelling (held holdTicks))
        in CueTearing 1 `elem` deskCues levels book a b && CueTorn `elem` deskCues levels book last1 off
    , check "orders: the desk's moves into and out of play are heard" $
        CueMenuEnter `elem` cues levels (cpRun (title book), initialFx) (cpRun desk, initialFx)
        && CueIntro `elem` cues levels (cpRun desk, initialFx) (cpRun (steps desk (tap right ++ tap enter)), initialFx)
    ]

-- The opening film --------------------------------------------------------------

-- | Film flow and skip arming. A fresh save opens the film, a completed one the
-- hall; only a released-and-held key counts towards skipping. Handover carries
-- held inputs into the level so the skip key cannot immediately act there.
-- These test Run/sound decisions, not Video's file decoding or actual playback.
introChecks :: [Check]
introChecks = case levelHall of
  Left err -> [check ("intro: the levels parse: " ++ err) False]
  Right levels ->
    let fresh = startRun levels 0
        opening = stepRun levels enter fresh
        released = stepRun levels noInput opening
        steps r inputs = foldl' (flip (stepRun levels)) r inputs
        isIntro r = case runMode r of
          Intro _ _ -> True
          _ -> False
    in
    [ check "intro: a new campaign's Enter at the title plays the opening film" $
        runMode opening == Intro 0 0 && runIndex opening == 0
    , check "intro: with a level cleared, Enter at the title opens the hall" $
        runMode (stepRun levels enter (startRunWithProgress levels 0 1)) == Lobby
    , check "intro: the film counts its ticks" $
        runMode (steps released (replicate 9 noInput)) == Intro 10 0
    , check "intro: Escape skips straight into level 1, playing" $
        let r = stepRun levels lobbyKey released
        in runMode r == InLevel && runIndex r == 0 && gPhase (runGame r) == Playing
    , check "intro: holding Enter skips it; a tap does not" $
        let held = steps released (replicate introSkipTicks enter)
            tapped = steps released ([enter, noInput] ++ replicate 30 noInput)
        in runMode held == InLevel && runIndex held == 0 && isIntro tapped
    , check "intro: the Enter still down from the title does not count towards a skip" $
        isIntro (steps opening (replicate (introSkipTicks + 20) enter))
    , check "intro: when the film ends, level 1 begins, and held keys need a fresh press" $
        let held = stepRun levels space released
            r = finishIntro levels held
        in runMode r == InLevel && runIndex r == 0 && gPhase (runGame r) == Playing
           && tiShift (gPrev (runGame r))
           && gEra (runGame (stepRun levels space r)) == gEra (runGame r)
    , check "intro: finishIntro leaves anything else alone" $
        finishIntro levels fresh == fresh
    , check "intro: its soundtrack starts with it and stops when it ends; no weather under it" $
        CueIntro `elem` cues levels (fresh, initialFx) (opening, initialFx)
        && CueIntroEnd `elem` cues levels (released, initialFx) (stepRun levels lobbyKey released, initialFx)
        && beds levels opening == silence
    ]

-- The courier's talk --------------------------------------------------------------

-- | One tick as the game runs it: an open talk takes the keys and holds the run.
storyTick :: [Level] -> (Story, Run) -> TickInput -> (Story, Run)
-- Match Main's ordering: consume dialogue input first, hold/step the run second,
-- then notice new triggers. This keeps the closing Space from becoming a shift.
storyTick levels (s, r) input =
  let (s0, free) = talkTick input r s
      r' = if free then stepRun levels input r else holdRun input r
  in (notice levels r r' s0, r')

-- | Dialogue locks the board, types/advances on fresh keys and triggers only for
-- eligible visits. Check focus targets against actual objects, escape during a
-- line, closing-key consumption and the open/letter sounds from story deltas.
storyChecks :: [Check]
storyChecks = case levelHall of
  Right levels@(first : second : _) ->
    let play = foldl' (storyTick levels)
        lobby = (startRun levels 0) { runMode = Lobby }
        entered = play (noStory, lobby) [enter]
        talking = play entered (replicate (arrivalWait + 2) noInput)
        -- Press a key, let go, again, until she is done.
        through key st = case stTalk (fst st) of
          Nothing -> st
          Just _ -> through key (play st [key, noInput])
        done = through enter talking
        -- Pattern matching discards lines without a Look; targets must identify
        -- real map objects rather than positions duplicated in the test.
        looks b = [ lk | Line _ (Just lk) <- beatLines first (runGame (snd entered)) b ]
        playing = (runGame (snd done)) { gPrev = noInput }
        inLevel = Run 0 0 0 InLevel playing
        withCell = inLevel { runGame = playing { gHasCell = True } }
        rustedRun = inLevel { runGame = playing { gFeedback = Just (Feedback CrateRusted hintFeedbackTicks) } }
        -- Keep all story states while idling so consecutive deltas reveal the
        -- exact appearance tick and whether letters emitted any blips.
        trace0 st n = map fst (scanl (storyTick levels) st (replicate n noInput))
        heardOver ss = concat (zipWith storyCues ss (drop 1 ss))
    in
    [ check "story: entering level 1 for the first time opens its talk, after the fade-in" $
        case stTalk (fst entered) of
          Just t -> tkBeat t == ArriveFirst && tkWait t == arrivalWait && not (talkShown t)
          Nothing -> False
    , check "story: level 1's talk points at the wick in the past and the lamp in the present" $
        Look (lvPowerCell first) Past TheWick `elem` looks ArriveFirst
        && Look (lvGoal first) Present TheLamp `elem` looks ArriveFirst
    , check "story: while she talks the board waits, and keys spend nothing" $
        let pressed = play talking (concat (replicate 5 [press DirRight, noInput]))
            g = runGame (snd pressed)
            g0 = runGame (snd talking)
        in gCell g == gCell g0 && gRemaining g == gRemaining g0 && gEra g == gEra g0
    , check "story: Enter finishes a line, then moves on, and the last one closes the talk" $
        case stTalk (fst talking) of
          Just t ->
            let typed = play talking [enter]
                nextLine = play typed [noInput, enter]
            in not (lineTyped t)
               && maybe False lineTyped (stTalk (fst typed))
               && fmap tkLine (stTalk (fst nextLine)) == Just 1
               && stTalk (fst done) == Nothing && runMode (snd done) == InLevel
          Nothing -> False
    , check "story: the Space that closes the talk does not shift; a fresh one does" $
        let closed = through space talking
            -- On to the last line, typed out; then Space closes the talk and is held.
            lastLine st = case stTalk (fst st) of
              Just t | tkLine t + 1 == length (tkLines t) && lineTyped t -> st
              Just _ -> lastLine (play st [space, noInput])
              Nothing -> st
            held = play (lastLine talking) [space, space, space]
            again = play held [noInput, space]
        in stTalk (fst closed) == Nothing
           && isJust (stTalk (fst (lastLine talking)))
           && stTalk (fst held) == Nothing
           && gEra (runGame (snd held)) == Present
           && gEra (runGame (snd again)) == Past
    , check "story: a level already cleared tells nothing" $
        stTalk (fst (play (noStory, lobby { runCompleted = 1, runUnlocked = 1 }) [enter])) == Nothing
    , check "story: taking the wick in level 1 opens a talk pointing at the lamp" $
        case stTalk (notice levels inLevel withCell noStory) of
          Just t -> tkBeat t == WickFirst && fmap lkThing (lnLook (currentLine t)) == Just TheLamp
          Nothing -> False
    , check "story: walking into the rusted crate opens a talk, once a visit" $
        let s1 = notice levels inLevel rustedRun noStory
            s2 = notice levels inLevel rustedRun s1 { stTalk = Nothing }
        in fmap tkBeat (stTalk s1) == Just RustedCrate && stTalk s2 == Nothing
    , check "story: level 2's talk points at a mirror on a sled, which can move" $
        let g2 = runGame (stepRun levels enter (Run 1 1 1 Lobby (initialGame second)))
            sleds = [ lk | Line _ (Just lk) <- beatLines second g2 ArriveSecond, lkThing lk == TheSled ]
        in not (null sleds)
           && all (\lk -> maybe False mMovable (Map.lookup (lkCell lk) (lvMirrors second))) sleds
    , check "story: Escape still leaves a level mid-talk, and the talk with it" $
        let left = play talking [lobbyKey]
        in runMode (snd left) == Lobby && stTalk (fst left) == Nothing
    , check "story: the box pops up with a sound, and letters blip as they type" $
        let waiting = play entered (replicate (arrivalWait - 1) noInput)
            shown = play waiting [noInput]
            heard = heardOver (trace0 shown 20)
        in storyCues (fst waiting) (fst shown) == [CueTalkOpen]
           && not (null [ () | CueTalkBlip _ <- heard ])
    ]
  _ -> [check "story: the campaign has two levels" False]
