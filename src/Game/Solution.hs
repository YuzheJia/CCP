-- | Player-sized actions over the real 'stepGame': the intended route, and the
-- search that re-checks the level through the implementation itself.
module Game.Solution
  ( Action (..)
  , intendedRoute
  , actionInputs
  , routeInputs
  , applyAction
  , StateKey
  , stateKey
  , startPlaying
  , SearchResult (..)
  , search
  , allActions
  , actionLabel
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Game.Logic
import Game.Types

data Action = Go !Dir | ShiftEra
  deriving (Eq, Ord, Show)

allActions :: [Action]
allActions = ShiftEra : map Go [minBound .. maxBound]

actionLabel :: Action -> String
actionLabel ShiftEra      = "Space"
actionLabel (Go DirUp)    = "W"
actionLabel (Go DirDown)  = "S"
actionLabel (Go DirLeft)  = "A"
actionLabel (Go DirRight) = "D"

-- | The route the level is built around: 36 steps and 6 shifts.
intendedRoute :: [Action]
intendedRoute = concat
  [ [ShiftEra]                             -- into the Past, where the left door stands open
  , right 1, up 4, right 2                 -- through (4,2)
  , [ShiftEra]                             -- into the Present, where the right wall is breached
  , down 4, right 4                        -- through (8,6)
  , [ShiftEra]                             -- into the Past, where the cell still is
  , up 4, right 1                          -- onto (10,2)
  , [ShiftEra]                             -- back to the Present to use the breach again
  , down 4, left 3                         -- through (8,6)
  , [ShiftEra]                             -- back to the Past to use the door again
  , up 4, left 4                           -- through (4,2)
  , [ShiftEra]                             -- home to the Present
  , left 1                                 -- onto the lighthouse at (2,2)
  ]
  where
    up n    = replicate n (Go DirUp)
    down n  = replicate n (Go DirDown)
    left n  = replicate n (Go DirLeft)
    right n = replicate n (Go DirRight)

-- | Key states for one action: a press on the first tick, then idle. The player
-- is free again on the tick after the last one listed, which is when the next
-- action's press lands.
actionInputs :: Action -> [TickInput]
actionInputs action = case action of
  Go dir   -> noInput { tiDir = Just dir } : replicate (stepTicks - 1) noInput
  ShiftEra -> noInput { tiShift = True }   : replicate (shiftTicks - 1) noInput

-- | The whole route, plus the one idle tick on which its last action completes.
routeInputs :: [Action] -> [TickInput]
routeInputs actions = concatMap actionInputs actions ++ [noInput]

-- | Press Enter on the title screen.
startPlaying :: Level -> Game
startPlaying level = stepGame level noInput { tiEnter = True } (initialGame level)

-- | Press one key for one tick, then run until the player is settled again.
applyAction :: Level -> Action -> Game -> Game
applyAction level action g = settle (stepGame level press g)
  where
    press = case action of
      Go dir   -> noInput { tiDir = Just dir }
      ShiftEra -> noInput { tiShift = True }
    settle s
      | gPhase s == Playing && gMotion s /= Settled = settle (stepGame level noInput s)
      | otherwise = s

-- | Everything the rules can tell apart once the player has settled.
type StateKey = (Era, Cell, Bool, Phase)

stateKey :: Game -> StateKey
stateKey g = (gEra g, gCell g, gHasCell g, gPhase g)

data SearchResult = SearchResult
  { srStates   :: !(Map.Map StateKey (Int, [Action])) -- ^ distance and one shortest route
  , srGoal     :: !(Maybe (Int, [Action]))
  , srDeadEnds :: ![StateKey]                         -- ^ reachable, but cannot then win
  }

-- | Breadth-first over the real rules, with the countdown refilled before each
-- action so it cannot end the search. @allowed@ limits what the player may do
-- from a given state, which is how "never shift" is asked.
search :: Level -> (Game -> Action -> Bool) -> Game -> SearchResult
search level allowed start = SearchResult
  { srStates = visited
  , srGoal   = case [v | (k, v) <- Map.toList visited, isWon k] of
      []   -> Nothing
      wins -> Just (minimum' wins)
  , srDeadEnds = [k | k <- Map.keys visited, not (k `Set.member` canWin)]
  }
  where
    (visited, edges) = bfs [(start, 0, [])] (Map.singleton (stateKey start) (0, [])) []
    isWon (_, _, _, phase) = phase == Won
    minimum' = foldr1 (\a b -> if fst a <= fst b then a else b)

    bfs [] seen es = (seen, es)
    bfs frontier seen es =
      let moves = [ (next, d + 1, route ++ [a], stateKey g, stateKey next)
                  | (g, d, route) <- frontier
                  , gPhase g == Playing
                  , a <- allActions
                  , allowed g a
                  , let next = refill (applyAction level a (refill g)) ]
          es' = [(from, to) | (_, _, _, from, to) <- moves] ++ es
          (seen', frontier') = foldl visit (seen, []) moves
      in bfs (reverse frontier') seen' es'

    visit (seen, out) (next, d, route, _, key)
      | key `Map.member` seen = (seen, out)
      | otherwise = (Map.insert key (d, route) seen, (next, d, route) : out)

    refill g = g { gRemaining = levelTicks, gFeedback = Nothing, gBump = 0 }

    -- Walk the edges backwards from every won state.
    predecessors = Map.fromListWith (++) [(to, [from]) | (from, to) <- edges]
    canWin = spread (Set.fromList (filter isWon (Map.keys visited)))
                    (filter isWon (Map.keys visited))
    spread done [] = done
    spread done (k : rest) =
      let new = [p | p <- Map.findWithDefault [] k predecessors, not (p `Set.member` done)]
      in spread (foldr Set.insert done new) (new ++ rest)
