-- | Plain assertions over the pure core. Numbered items match the brief's list.
module Main (main) where

import Control.Monad (forM, unless)
import Data.List (foldl', isInfixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic
import Game.Run
import Game.Solution
import Game.Types
import System.Exit (exitFailure)

data Check = Check String Bool

check :: String -> Bool -> Check
check = Check

press :: Dir -> TickInput
press d = noInput { tiDir = Just d }

space, enter, restartKey, pauseKey, undoKey, lobbyKey :: TickInput
space      = noInput { tiShift = True }
enter      = noInput { tiEnter = True }
restartKey = noInput { tiRestart = True }
pauseKey   = noInput { tiPause = True }
undoKey    = noInput { tiUndo = True }
lobbyKey   = noInput { tiLobby = True }

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

-- | What a game holds that the player can tell apart, ignoring time.
essentials :: Game -> StateKey
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
      putStrLn ("search: cheapest route costs " ++ show n ++ " s, " ++ show (length actions)
                ++ " actions, " ++ show (length (filter (== ShiftEra) actions)) ++ " of them shifts")
      putStrLn ("search: " ++ unwords (map actionLabel actions))
  putStrLn ("route:  intended route takes " ++ show (length route) ++ " ticks, ends "
            ++ show (essentials end))
  case campaign of
    Left _ -> pure ()
    Right levels -> mapM_ reportLevel levels
  putStrLn ""
  where
    reportLevel l =
      let r = search l (\_ _ -> True) (startPlaying l)
          shifts = maybe 0 (length . filter (== ShiftEra) . snd) (srGoal r)
      in putStrLn ("level:  " ++ lvName l ++ ": par " ++ maybe "NONE" (show . fst) (srGoal r)
                   ++ ", budget " ++ show (lvBudget l)
                   ++ ", " ++ maybe "0" (show . length . snd) (srGoal r) ++ " actions, "
                   ++ show shifts ++ " shifts, "
                   ++ show (Map.size (srStates r)) ++ " states, "
                   ++ show (length (srDeadEnds r)) ++ " dead, "
                   ++ show (sharedPercent l) ++ "% shared")
    sharedPercent l =
      let both = Set.size (Set.intersection (lvPast l) (lvPresent l))
          every = Set.size (Set.union (lvPast l) (lvPresent l))
      in 100 * both `div` every

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
  , pendingShifts level
  , routeChecks level
  , determinism level
  , inputAdapter level
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
  [ check "parser: rejects a missing row" (isLeft (parse (drop 1 pastRows) presentRows))
  , check "parser: rejects a short row" (isLeft (parse pastRows (mapHead init presentRows)))
  , check "parser: rejects a long row" (isLeft (parse (mapHead (++ "#") pastRows) presentRows))
  , check "parser: rejects an unknown character"
      (isLeft (parse pastRows (replaceAt 1 "#.?.#...#...#" presentRows)))
  , check "parser: rejects a second power cell"
      (isLeft (parse (replaceAt 3 "#.B.#...#...#" pastRows) presentRows))
  , check "parser: rejects a missing spawn"
      (isLeft (parse pastRows (replaceAt 6 "#...#.......#" presentRows)))
  , check "parser: rejects the goal in the wrong era"
      (isLeft (parse (replaceAt 1 "#.G.#...#...#" pastRows) presentRows))
  , check "parser: rejects an open border"
      (isLeft (parse (replaceAt 3 "....#...#...#" pastRows) presentRows))
  , check "parser: rejects an empty map" (isLeft (parse [] []))
  , check "parser: rejects a crate on the present map"
      (isLeft (parse pastRows (replaceAt 1 "#.C.#...#...#" presentRows)))
  , check "parser: rejects a pit on the present map"
      (isLeft (parse pastRows (replaceAt 1 "#.~.#...#...#" presentRows)))
  , check "parser: rejects a pit in the border"
      (isLeft (parse (replaceAt 3 "~...#...#...#" pastRows) presentRows))
  , check "parser: rejects a crate on the spawn"
      (isLeft (parse (replaceAt 6 "#.C.#...#...#" pastRows) presentRows))
  , check "parser: rejects a crate inside a present wall"
      (isLeft (parse (replaceAt 3 "#.C.#...#...#" pastRows) (replaceAt 3 "###.#...#...#" presentRows)))
  , check "parser: rejects a plate with no door"
      (isLeft (parse (replaceAt 3 "#.P.#...#...#" pastRows) presentRows))
  , check "parser: rejects a door with no plate"
      (isLeft (parse pastRows (replaceAt 3 "#.D.#...#...#" presentRows)))
  , check "parser: rejects a plate in a present wall"
      (isLeft (parse (replaceAt 3 "#.P.#...#...#" pastRows)
                     (replaceAt 3 "###.#...#...#" (replaceAt 5 "#.D.#...#...#" presentRows))))
  , check "parser: rejects a door on the past map and a plate on the present map"
      (isLeft (parse (replaceAt 3 "#.D.#...#...#" pastRows) presentRows)
       && isLeft (parse pastRows (replaceAt 3 "#.P.#...#...#" presentRows)))
  , check "parser: rejects a door in the border"
      (isLeft (parse (replaceAt 3 "#.P.#...#...#" pastRows) (replaceAt 3 "D...#...#...#" presentRows)))
  , check "parser: reads a plate as floor in both eras, and a door as shut" $
      case parse (replaceAt 3 "#.P.#...#...#" pastRows) (replaceAt 5 "#.D.#...#...#" presentRows) of
        Left _ -> False
        Right l -> lvPlates l == Set.fromList [(2, 3)] && lvDoors l == Set.fromList [(2, 5)]
                   && walkable l Past (2, 3) && walkable l Present (2, 3)
                   && not (walkable l Present (2, 5))
  , check "parser: a crate stands on floor; a pit is not floor" $
      case parse (replaceAt 3 "#.C.#.~.#...#" (replaceAt 4 "#...#.C.#...#" pastRows)) presentRows of
        Left _ -> False
        Right l -> lvCrates l == Set.fromList [(2, 3), (6, 4)] && lvPits l == Set.fromList [(6, 3)]
                   && walkable l Past (2, 3) && not (walkable l Past (6, 3))
  ]
  where
    parse = parseLevel 100
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
      all (\k -> walkable level (skEra k) (skCell k))
        (Map.keys (srStates (search level (\_ _ -> True) (startPlaying level))))
  ]

-- 3, 4, 5. ---------------------------------------------------------------------
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
-- the map, or is a door that those crates have opened.
nobodyInShutDoor :: Level -> Bool
nobodyInShutDoor l = all ok (Map.keys (srStates (search l (\_ _ -> True) (startPlaying l))))
  where
    ok k = skEra k /= Present || walkable l Present (skCell k)
           || (skCell k `Set.member` lvDoors l && doorsOpen l (held k))
    held k = maybe id (Set.insert . snd) (skEcho k) (Set.insert (skCell k) (skCrates k))

-- The echo. --------------------------------------------------------------------

-- | The campaign's "Echo" level: spawn (1,4), plate (3,4), door (8,4), lighthouse
-- (10,4), all in one row of the present; the cell is at (1,3) in the past.
echoRoom :: Level
echoRoom = head [l | Right ls <- [extraLevels], l <- ls, lvName l == "Echo"]

sendEcho :: Level -> Game -> Game
sendEcho level = applyAction level SendEcho

walks :: Level -> [Dir] -> Game -> Game
walks level ds g = foldl' (flip (walk level)) g ds

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
    pr c = (Present, c)

-- Mirrors. ---------------------------------------------------------------------

levelNamed :: String -> Level
levelNamed name = head [l | Right ls <- [campaign, extraLevels], l <- ls, lvName l == name]

-- | "Through the Glass": a past mirror at (6,4) facing down, the cell at (6,2).
glass, rust, oneWay :: Level
glass = levelNamed "Through the Glass"
rust = levelNamed "Rust in the Beam"
oneWay = levelNamed "One Way In"

mirrorChecks :: [Check]
mirrorChecks =
  [ check "parser: a mirror belongs to the era it is drawn in, and is solid there" $
      Map.lookup (6, 4) (lvMirrors glass) == Just (Past, DirDown)
        && not (walkable glass Past (6, 4))
        && Map.lookup (4, 4) (lvMirrors rust) == Just (Present, DirRight)
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
      let open g = g { gCrates = Set.empty }
      in jumpTarget rust (open (at rust Present (6, 4) False)) DirLeft == Just (2, 4)
         && jumpTarget rust (open (at rust Past (6, 4) False)) DirLeft == Nothing
  , check "mirror: a crate in the light stops it" $
      let g = at rust Present (6, 4) False
      in mirrorAhead rust g DirLeft == Nothing && gCell (walk rust DirLeft g) == (6, 4)
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
  ]

-- The campaign. -----------------------------------------------------------------
campaignChecks :: [Check]
campaignChecks = case campaign of
  Left err -> [check ("campaign: parses: " ++ err) False]
  Right levels -> concat
    [ [ check "campaign: slack only ever shrinks" $
          let slacks = map (\l -> lvBudget l - lvPar l) levels
          in slacks == reverse (sort slacks)
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
      , check "campaign: all three are crate-and-era alternatives with no later mechanics" $
          length levels == 3
          && not (Set.null (lvCrates (levels !! 0))) && Map.null (lvMirrors (levels !! 0))
          && not (Set.null (lvCrates (levels !! 1))) && Map.null (lvMirrors (levels !! 1))
          && not (Set.null (lvCrates (levels !! 2))) && Map.null (lvMirrors (levels !! 2))
          && all Set.null (map lvPlates levels) && all Set.null (map lvDoors levels)
          && all (not . null . lvLesson) levels
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
          let title = startRun levels 0
              lobby = stepRun levels enter title
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

-- | What every level must prove through the real rules.
levelChecks :: Level -> [Check]
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
  , named "its search stays small enough to run" $ Map.size (srStates result) <= 20000
  , named "no crate is ever inside a wall, in either era" $
      all (\k -> all (\c -> walkable l Past c && walkable l Present c) (skCrates k))
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
    named what = check ("level " ++ lvName l ++ ": " ++ what)
    start = startPlaying l
    result = search l (\_ _ -> True) start
    goal = srGoal result
    route = maybe [] snd goal
    noDoors lv = lv { lvPlates = Set.fromList [(0, 0)] }
    noPushing g action = case action of
      Go d -> stepCell d (gCell g) `Set.notMember` gCrates g || gEra g /= Past
      ShiftEra -> True
      SendEcho -> True
    play n = run l (routeInputs route) start { gRemaining = n }

-- Undo. ------------------------------------------------------------------------
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
                          , let next = (applyAction level a g) { gRemaining = lvBudget level, gHistory = [] }
                          , stateKeyB next n' `notElem` seen ]
                  uniq = dedupe nexts
              in go (rest ++ uniq) (map (uncurry stateKeyB) uniq ++ seen)
        dedupe = foldr (\x acc -> if any (\y -> uncurry stateKeyB y == uncurry stateKeyB x) acc
                                    then acc else x : acc) []

-- 18. --------------------------------------------------------------------------
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
      in noInput { tiDir = dir, tiShift = bits 4096 < 4, tiPause = bits 256 == 0 && bits 16 == 0
                 , tiUndo = bits 16 == 1 }

-- Input adapter. ---------------------------------------------------------------
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
          dirty = Fx 100 (Just (Wave 3 Past (1, 1))) Nothing 0 0 0 5 Nothing Nothing
          fx = stepFx g g dirty
      in fxClock fx == 100 && fmap waveAge (fxWave fx) == Just 3 && fxLevelAge fx == 6
  , check "fx: restart clears every effect" $
      let g = at level Past (9, 2) True
          dirty = Fx 500 (Just (Wave 3 Past (1, 1))) (Just (Burst 3 (1, 1))) 50 9 9 9
                    (Just (CostFloat 3 (-2) (1, 1))) (Just (EchoWalk 2 [(Past, (1, 1)), (Past, (2, 1))]))
      in stepFx g (run level [restartKey] g) dirty == initialFx
  , check "fx: resuming from a pause is not mistaken for a restart" $
      let g = run level [pauseKey] (startPlaying level)
          dirty = Fx 500 Nothing Nothing 0 0 3 9 Nothing Nothing
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
