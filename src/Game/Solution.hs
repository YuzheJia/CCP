-- | Player-sized actions over the real 'stepGame': the intended route, and the
-- search that re-checks the level through the implementation itself.
module Game.Solution
  ( Action (..)
  , intendedRoute
  , knownRoute
  , actionInputs
  , routeInputs
  , applyAction
  , StateKey (..)
  , stateKey
  , startPlaying
  , SearchResult (..)
  , search
  , deadStates
  , allActions
  , actionLabel
  ) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Game.Logic
import Game.Types

data Action = Go !Dir | ShiftEra | SendEcho
  deriving (Eq, Ord, Show)

allActions :: [Action]
allActions = ShiftEra : SendEcho : map Go [minBound .. maxBound]

actionLabel :: Action -> String
actionLabel ShiftEra      = "Space"
actionLabel SendEcho      = "E"
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

-- | A cheapest route for each level too big to search on every run, found once by
-- the full 'search'. The Long Reflection reaches about four million states, which
-- takes minutes and gigabytes, so the tests and the demo replay this instead.
-- W A S D are steps and @_@ is a shift.
knownRoute :: Level -> Maybe [Action]
knownRoute level = map key <$> lookup (lvName level) routes
  where
    routes =
      [ ( "Level 3 - The Long Reflection"
        , "WWW_DDDDWWWWWSAWASAAWWDDDDDSASSSAAAWWADWDDWAWSSSDSSAAWASSD_SSS"
          ++ replicate 17 'A' ++ replicate 9 'W' )
      ]
    key c = case c of
      'W' -> Go DirUp
      'A' -> Go DirLeft
      'S' -> Go DirDown
      'D' -> Go DirRight
      _   -> ShiftEra

-- | Key states for one action: a press on the first tick, then idle, so the next
-- action's press lands when the player can take it. A direction pressed on the tick
-- a step lands is taken at once. Space pressed on a wave's last tick is dropped by
-- the lock, so a shift waits one tick longer, or two shifts in a row would be one.
actionInputs :: Action -> [TickInput]
actionInputs action = case action of
  -- As W A S D would give it, so in a level for two it moves the first keeper.
  Go dir   -> noInput { tiDir = Just dir, tiDir1 = Just dir } : replicate (stepTicks - 1) noInput
  ShiftEra -> noInput { tiShift = True }   : replicate shiftTicks noInput
  SendEcho -> [noInput { tiEcho = True }, noInput]

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
      Go dir   -> noInput { tiDir = Just dir, tiDir1 = Just dir }
      ShiftEra -> noInput { tiShift = True }
      SendEcho -> noInput { tiEcho = True }
    settle s
      | gPhase s == Playing && gMotion s /= Settled = settle (stepGame level noInput s)
      | otherwise = s

-- | Everything the rules can tell apart once the player has settled. Crates, filled
-- pits, footprints and the echo belong here: leave them out and different boards
-- would be merged. A level without an echo keeps no footprints, so for it these add
-- nothing.
data StateKey = StateKey
  { skEra     :: !Era
  , skCell    :: !Cell
  , skHasCell :: !Bool
  , skPhase   :: !Phase
  , skCrates  :: !(Set.Set Cell)
  , skFilled  :: !(Set.Set Cell)
  , skTrail   :: ![Spot]
  , skEcho    :: !(Maybe Spot)
  , skMirrors :: ![(Cell, Mirror)]  -- ^ the movable ones only: the rest never change
  } deriving (Eq, Ord, Show)

stateKey :: Game -> StateKey
stateKey g = StateKey (gEra g) (gCell g) (gHasCell g) (gPhase g) (gCrates g) (gFilled g)
                      (gTrail g) (gEcho g)
                      (filter (mMovable . snd) (Map.toList (gMirrors g)))

data SearchResult = SearchResult
  { srStates   :: !(Map.Map StateKey Int)    -- ^ cheapest cost to reach each state
  , srGoal     :: !(Maybe (Int, [Action]))   -- ^ cheapest cost to win, and a route
  , srDeadEnds :: ![StateKey]                -- ^ reachable, but cannot then win
  }

-- | Every state of a level that can be reached but can no longer be won, whatever
-- the time left. A level's own rules can tell the player when they have walked into
-- one, by looking it up.
deadStates :: Level -> Set.Set StateKey
deadStates level = Set.fromList (srDeadEnds (search level (\_ _ -> True) (startPlaying level)))

data Explored = Explored
  { exQueue   :: !(Set.Set (Int, StateKey))
  , exCosts   :: !(Map.Map StateKey Int)
  , exGames   :: !(Map.Map StateKey Game)
  , exParents :: !(Map.Map StateKey (StateKey, Action))
  , exEdges   :: ![(StateKey, StateKey)]
  }

-- | Cheapest-first over the real rules. The budget is refilled before each action
-- so it cannot end the search, and an action's cost is read from how much time the
-- real 'stepGame' took, so there is no second price list to keep in step. @allowed@
-- limits what the player may do from a given state, which is how "never shift" is
-- asked.
search :: Level -> (Game -> Action -> Bool) -> Game -> SearchResult
search level allowed start = SearchResult
  { srStates = costs
  , srGoal   = case [(c, k) | (k, c) <- Map.toList costs, isWon k] of
      []   -> Nothing
      wins -> let (c, k) = minimum wins in Just (c, routeTo k)
  , srDeadEnds = [k | k <- Map.keys costs, not (k `Set.member` canWin)]
  }
  where
    startKey = stateKey start
    explored = explore Explored
      { exQueue = Set.singleton (0, startKey)
      , exCosts = Map.singleton startKey 0
      , exGames = Map.singleton startKey start
      , exParents = Map.empty
      , exEdges = []
      }
    costs = exCosts explored
    edges = exEdges explored
    isWon k = skPhase k == Won

    explore ex = case Set.minView (exQueue ex) of
      Nothing -> ex
      Just ((cost, key), queue)
        | cost > Map.findWithDefault cost key (exCosts ex) || skPhase key /= Playing ->
            explore ex { exQueue = queue }
        | otherwise ->
            explore (foldl' (relax cost key (exGames ex Map.! key)) ex { exQueue = queue } allActions)

    relax cost key g ex action
      | not (allowed g action) || nextKey == key = ex
      | maybe True (nextCost <) (Map.lookup nextKey (exCosts ex)) = linked
          { exQueue   = Set.insert (nextCost, nextKey) (exQueue ex)
          , exCosts   = Map.insert nextKey nextCost (exCosts ex)
          , exGames   = Map.insert nextKey next (exGames ex)
          , exParents = Map.insert nextKey (key, action) (exParents ex)
          }
      | otherwise = linked
      where
        ready = refill g
        next = applyAction level action ready
        nextKey = stateKey next
        nextCost = cost + gRemaining ready - gRemaining next
        linked = ex { exEdges = (key, nextKey) : exEdges ex }

    refill g = g { gRemaining = plentyOfTime, gHistory = [], gFeedback = Nothing, gBump = 0 }
    plentyOfTime = 1000000 :: Int

    routeTo = go []
      where
        go route k = case Map.lookup k (exParents explored) of
          Nothing -> route
          Just (parent, action) -> go (action : route) parent

    -- Walk the edges backwards from every won state.
    predecessors = Map.fromListWith (++) [(to, [from]) | (from, to) <- edges]
    canWin = spread (Set.fromList (filter isWon (Map.keys costs)))
                    (filter isWon (Map.keys costs))
    spread done [] = done
    spread done (k : rest) =
      let new = [p | p <- Map.findWithDefault [] k predecessors, not (p `Set.member` done)]
      in spread (foldr Set.insert done new) (new ++ rest)
