-- | Plain assertions over the pure core. Numbered items match the brief's list.
module Main (main) where

import Control.Monad (forM, unless)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic
import Game.Solution
import Game.Types
import System.Exit (exitFailure)

data Check = Check String Bool

check :: String -> Bool -> Check
check = Check

press :: Dir -> TickInput
press d = noInput { tiDir = Just d }

space, enter, restartKey, pauseKey :: TickInput
space      = noInput { tiShift = True }
enter      = noInput { tiEnter = True }
restartKey = noInput { tiRestart = True }
pauseKey   = noInput { tiPause = True }

run :: Level -> [TickInput] -> Game -> Game
run level inputs g = foldl' (flip (stepGame level)) g inputs

idle :: Level -> Int -> Game -> Game
idle level n = run level (replicate n noInput)

-- | A playing state at a chosen spot, with no leftovers.
at :: Level -> Era -> Cell -> Bool -> Game
at level era cell hasCell =
  (startPlaying level) { gEra = era, gCell = cell, gHasCell = hasCell, gPrev = noInput }

walk :: Level -> Dir -> Game -> Game
walk level = applyAction level . Go

shift :: Level -> Game -> Game
shift level = applyAction level ShiftEra

-- | What a game holds that the player can tell apart, ignoring timers.
essentials :: Game -> (Era, Cell, Bool, Phase)
essentials = stateKey

main :: IO ()
main = case theLevel of
  Left err -> putStrLn ("level does not parse: " ++ err) >> exitFailure
  Right level -> do
    results <- forM (allChecks level) $ \(Check name ok) -> do
      putStrLn ((if ok then "  ok    " else "  FAIL  ") ++ name)
      pure ok
    report level
    let failed = length (filter not results)
    putStrLn (show (length results - failed) ++ " passed, " ++ show failed ++ " failed")
    unless (failed == 0) exitFailure

-- | Numbers worth reading in the log, measured on the implementation.
report :: Level -> IO ()
report level = do
  let start = startPlaying level
      result = search level (\_ _ -> True) start
      route = routeInputs intendedRoute
      end = run level route start
  putStrLn ""
  putStrLn ("search: " ++ show (Map.size (srStates result)) ++ " reachable states, "
            ++ show (length (srDeadEnds result)) ++ " dead ends")
  case srGoal result of
    Nothing -> putStrLn "search: NO ROUTE"
    Just (n, actions) -> do
      putStrLn ("search: shortest route is " ++ show n ++ " actions, "
                ++ show (length (filter (== ShiftEra) actions)) ++ " of them shifts")
      putStrLn ("search: " ++ unwords (map actionLabel actions))
  putStrLn ("route:  intended route takes " ++ show (length route) ++ " ticks, ends "
            ++ show (essentials end))
  putStrLn ""

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
  , pendingShifts level
  , routeChecks level
  , determinism level
  , inputAdapter
  , fxChecks level
  ]

-- 1. ---------------------------------------------------------------------------
dimensions :: Level -> [Check]
dimensions level =
  [ check "1. maps are 13 x 9" (lvWidth level == 13 && lvHeight level == 9)
  , check "1. both source maps have 9 rows of 13"
      (all (\rows -> length rows == 9 && all ((== 13) . length) rows) [pastRows, presentRows])
  , check "1. spawn, cell and lighthouse are where the brief says"
      (lvSpawn level == (2, 6) && lvPowerCell level == (10, 2) && lvGoal level == (2, 2))
  , check "1. passages are (4,2) in the past and (8,6) in the present"
      (passageCells level == [(Past, (4, 2)), (Present, (8, 6))])
  , check "1. markers are walkable"
      (walkable level Past (10, 2) && walkable level Present (2, 2) && walkable level Present (2, 6))
  ]

parserChecks :: [Check]
parserChecks =
  [ check "parser: rejects a missing row" (isLeft (parseLevel (drop 1 pastRows) presentRows))
  , check "parser: rejects a short row" (isLeft (parseLevel pastRows (mapHead init presentRows)))
  , check "parser: rejects a long row" (isLeft (parseLevel (mapHead (++ "#") pastRows) presentRows))
  , check "parser: rejects an unknown character"
      (isLeft (parseLevel pastRows (replaceAt 1 "#.?.#...#...#" presentRows)))
  , check "parser: rejects a second power cell"
      (isLeft (parseLevel (replaceAt 3 "#.B.#...#...#" pastRows) presentRows))
  , check "parser: rejects a missing spawn"
      (isLeft (parseLevel pastRows (replaceAt 6 "#...#.......#" presentRows)))
  , check "parser: rejects the goal in the wrong era"
      (isLeft (parseLevel (replaceAt 1 "#.G.#...#...#" pastRows) presentRows))
  , check "parser: rejects an open border"
      (isLeft (parseLevel (replaceAt 3 "....#...#...#" pastRows) presentRows))
  , check "parser: rejects an empty map" (isLeft (parseLevel [] []))
  ]
  where
    isLeft e = case e of
      Left _  -> True
      Right _ -> False
    mapHead f (x : xs) = f x : xs
    mapHead _ []       = []
    replaceAt i row rows = take i rows ++ [row] ++ drop (i + 1) rows

-- 2. ---------------------------------------------------------------------------
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
      all (\(era, c, _, _) -> walkable level era c)
        (Map.keys (srStates (search level (\_ _ -> True) (startPlaying level))))
  ]

-- 3, 4, 5. ---------------------------------------------------------------------
shifting :: Level -> [Check]
shifting level =
  [ check "3. a legal shift changes the era and starts the lock, nothing else" $
      let g = at level Present (2, 6) False
      in tryShift level g == g { gEra = Past, gMotion = Shifting shiftTicks }
  , check "3. the player's cell survives a shift both ways" $
      let g = shift level (shift level (at level Present (5, 3) False))
      in gCell g == (5, 3) && gEra g == Present
  , check "3. the timer keeps running through the wave" $
      let g = run level (space : replicate (shiftTicks - 1) noInput) (at level Present (2, 6) False)
      in gRemaining g == levelTicks - shiftTicks
  , check "3. walking and shifting are refused during the wave" $
      let g0 = run level [space] (at level Present (2, 6) False)
          g1 = run level [noInput, space, noInput, space] g0
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
  , check "9. the first-door hint shows beside it, in the present, before pickup" $
      currentHint level (at level Present (3, 2) False) == Just HintDifferentOnce
      && currentHint level (at level Past (3, 2) False) == Nothing
      && currentHint level (at level Present (3, 2) True) == Nothing
      && currentHint level (at level Present (2, 6) False) == Nothing
  , check "9. objective text changes on pickup" $
      objectiveText (at level Present (2, 6) False) == "Find the power cell in the past."
      && objectiveText (at level Present (2, 6) True) == "Bring it to the lighthouse in the present."
      && objectiveText (walk level DirLeft (at level Present (3, 2) True)) == "The lighthouse is lit."
  ]

-- 10, 11. ----------------------------------------------------------------------
restartAndPause :: Level -> [Check]
restartAndPause level =
  [ check "10. restart restores the level from a messy state" $
      let messy = run level [press DirUp, noInput { tiDir = Just DirUp, tiShift = True }]
                    (at level Past (9, 3) True)
          r = run level [restartKey] messy
      in r { gPrev = noInput } == (initialGame level) { gPhase = Playing }
  , check "10. restart restores it from won, lost and paused too" $
      let won    = walk level DirLeft (at level Present (3, 2) True)
          lost   = (at level Past (5, 5) True) { gPhase = Lost, gRemaining = 0 }
          paused = run level [pauseKey] (at level Past (5, 5) True)
          fresh  = (initialGame level) { gPhase = Playing }
          again g = (run level [restartKey] g) { gPrev = noInput }
      in map gPhase [won, lost, paused] == [Won, Lost, Paused]
         && map again [won, lost, paused] == [fresh, fresh, fresh]
  , check "10. Enter retries from won and lost, but does nothing while playing" $
      let won  = walk level DirLeft (at level Present (3, 2) True)
          lost = (at level Past (5, 5) True) { gPhase = Lost, gRemaining = 0 }
          playing = idle level 5 (at level Past (5, 5) True)
      in gPhase (run level [enter] won) == Playing && gRemaining (run level [enter] lost) == levelTicks
         && gRemaining (run level [enter] playing) == gRemaining playing - 1
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
  , check "11. pause stops the countdown" $
      let g0 = at level Present (2, 6) False
          g1 = run level [pauseKey] g0
          g2 = idle level 500 g1
      in gPhase g1 == Paused && gRemaining g2 == gRemaining g0 && gPhase g2 == Paused
  , check "11. resume continues the countdown" $
      let g = run level ([pauseKey, noInput, pauseKey] ++ replicate 10 noInput)
                (at level Present (2, 6) False)
      in gPhase g == Playing && gRemaining g == levelTicks - 10
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
timing :: Level -> [Check]
timing level =
  [ check "12. the level lasts exactly 10,800 Playing ticks" $
      let g = startPlaying level
          before = idle level (levelTicks - 1) g
          after = idle level 1 before
      in levelTicks == 10800 && gRemaining g == 10800
         && gPhase before == Playing && gRemaining before == 1
         && gPhase after == Lost && gRemaining after == 0
  , check "12. the timer does not start on the title screen" $
      gRemaining (idle level 100 (initialGame level)) == levelTicks
  , check "12. nothing moves after a loss" $
      let g = idle level levelTicks (startPlaying level)
          g' = run level [press DirUp, noInput, space, noInput] g
      in gPhase g == Lost && essentials g' == essentials g && gRemaining g' == 0
  , check "12. the timer stops after a win" $
      let g = walk level DirLeft (at level Present (3, 2) True)
      in gRemaining (idle level 100 g) == gRemaining g
  , check "13. a step landing on the goal on the last tick wins" $
      let g = (at level Present (3, 2) True)
                { gMotion = Stepping (2, 2) 1, gRemaining = 1 }
          g' = run level [noInput] g
      in gPhase g' == Won && gRemaining g' == 0
  , check "13. a shift onto the goal on the last tick wins" $
      let g = (at level Past (2, 2) True) { gRemaining = 1 }
      in gPhase (run level [space] g) == Won
  , check "13. one tick earlier, the same step is too late" $
      let g = (at level Present (3, 2) True)
                { gMotion = Stepping (2, 2) 2, gRemaining = 1 }
      in gPhase (run level [noInput] g) == Lost
  , check "13. the full route wins on exactly its last tick, and loses a tick short" $
      let route = routeInputs intendedRoute
          ticksToWin = length (takeWhile ((/= Won) . gPhase)
                         (scanl (flip (stepGame level)) (startPlaying level) route))
          withTime n = (startPlaying level) { gRemaining = n }
      in gPhase (run level route (withTime ticksToWin)) == Won
         && gPhase (run level route (withTime (ticksToWin - 1))) == Lost
  ]

-- 14, 15. ----------------------------------------------------------------------
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
  , check "reachability: shortest solution through stepGame is 42 actions with 6 shifts" $
      case srGoal (search level (\_ _ -> True) (startPlaying level)) of
        Just (n, route) -> n == 42 && length (filter (== ShiftEra) route) == 6
        Nothing -> False
  , check "reachability: no reachable state is a dead end, ignoring the countdown" $
      null (srDeadEnds (search level (\_ _ -> True) (startPlaying level)))
  , check "reachability: every walkable cell in both eras can be stood on" $
      let reached = Map.keys (srStates (search level (\_ _ -> True) (startPlaying level)))
          allOpen = [ (e, (x, y)) | e <- [Past, Present], x <- [0 .. 12], y <- [0 .. 8]
                    , walkable level e (x, y) ]
      in all (\(e, c) -> any (\(e', c', _, _) -> e' == e && c' == c) reached) allOpen
  , check "17. never shifting, from the start in the present, cannot win" $
      let r = search level (\_ a -> a /= ShiftEra) (startPlaying level)
      in srGoal r == Nothing && all (\(e, _, h, _) -> e == Present && not h) (Map.keys (srStates r))
  , check "17. staying in the past after one shift cannot win or reach the cell" $
      let r = search level (\_ a -> a /= ShiftEra) (shift level (startPlaying level))
      in srGoal r == Nothing && all (\(e, _, h, _) -> e == Past && not h) (Map.keys (srStates r))
  , check "17. with the cell in hand, staying in the past still cannot win" $
      let r = search level (\_ a -> a /= ShiftEra) (at level Past (10, 2) True)
      in srGoal r == Nothing
  , check "17. fewer than six shifts cannot win" $
      -- States carry the shift count here, so the search sees every budget.
      not (winsWithin 5) && winsWithin 6
  ]
  where
    winsWithin budget = go [(startPlaying level, 0 :: Int)] [stateKeyB (startPlaying level) 0]
      where
        stateKeyB g n = (stateKey g, n)
        go [] _ = False
        go ((g, n) : rest) seen
          | gPhase g == Won = True
          | otherwise =
              let nexts = [ (next, n') | a <- allActions
                          , let n' = n + (if a == ShiftEra then 1 else 0)
                          , n' <= budget
                          , let next = (applyAction level a g) { gRemaining = levelTicks }
                          , stateKeyB next n' `notElem` seen ]
                  uniq = dedupe nexts
              in go (rest ++ uniq) (map (uncurry stateKeyB) uniq ++ seen)
        dedupe = foldr (\x acc -> if any (\y -> uncurry stateKeyB y == uncurry stateKeyB x) acc
                                    then acc else x : acc) []

-- 18. --------------------------------------------------------------------------
determinism :: Level -> [Check]
determinism level =
  [ check "18. the same inputs from the same state give the same result, at every tick" $
      let script = routeInputs intendedRoute ++ messyInputs
          trail = scanl (flip (stepGame level)) (startPlaying level) script
      in trail == scanl (flip (stepGame level)) (startPlaying level) script
         && last trail == run level script (startPlaying level)
         && length trail == length script + 1
  , check "18. a messy input stream replays identically and stays in the rules" $
      let trail = scanl (flip (stepGame level)) (startPlaying level) messyInputs
      in trail == scanl (flip (stepGame level)) (startPlaying level) messyInputs
         && all (\g -> walkable level (gEra g) (gCell g)) trail
         && all legalMotion trail
  ]
  where
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
      in noInput { tiDir = dir, tiShift = bits 4096 < 4, tiPause = bits 256 == 0 && bits 16 == 0 }

-- Input adapter. ---------------------------------------------------------------
inputAdapter :: [Check]
inputAdapter =
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
  ]

-- Effects. ---------------------------------------------------------------------
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
          dirty = Fx 100 (Just (Wave 3 Past (1, 1))) Nothing 0 0 0 5
          fx = stepFx g g dirty
      in fxClock fx == 100 && fmap waveAge (fxWave fx) == Just 3 && fxLevelAge fx == 6
  , check "fx: restart clears every effect" $
      let g = (at level Past (9, 2) True) { gRemaining = 5000 }
          dirty = Fx 500 (Just (Wave 3 Past (1, 1))) (Just (Burst 3 (1, 1))) 50 9 9 9
      in stepFx g (run level [restartKey] g) dirty == initialFx
  ]
