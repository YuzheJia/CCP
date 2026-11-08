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
  , DuoAction (..)
  , duoActions
  , duoLabel
  , applyDuo
  , duoInputs
  , DuoResult (..)
  , duoSearch
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

-- Two keepers -----------------------------------------------------------------
--
-- A level for two is checked by its own small search, over the same real
-- 'stepGame': each action is one keeper's key, or the shared era key. It is only for
-- the tests, which ask it whether two keepers can light the lamp, and whether one,
-- with the other standing still, cannot.

data DuoAction = Go1 !Dir | Go2 !Dir | ShiftBoth
  deriving (Eq, Ord, Show)

duoActions :: [DuoAction]
duoActions = ShiftBoth : map Go1 [minBound .. maxBound] ++ map Go2 [minBound .. maxBound]

-- | W A S D for the first keeper, w a s d for the second (the arrows), @_@ a shift.
duoLabel :: DuoAction -> String
duoLabel a = case a of
  ShiftBoth -> "_"
  Go1 d -> actionLabel (Go d)
  Go2 d -> case d of
    DirUp -> "w"
    DirDown -> "s"
    DirLeft -> "a"
    DirRight -> "d"

duoPress :: DuoAction -> TickInput
duoPress a = case a of
  Go1 d -> noInput { tiDir = Just d, tiDir1 = Just d }
  Go2 d -> noInput { tiDir = Just d, tiDir2 = Just d }
  ShiftBoth -> noInput { tiShift = True }

-- | Key states for one action, as 'actionInputs' gives them for one keeper.
duoInputs :: DuoAction -> [TickInput]
duoInputs a = case a of
  ShiftBoth -> duoPress a : replicate shiftTicks noInput
  _ -> duoPress a : replicate (stepTicks - 1) noInput

-- | Press one key for one tick, then run until both keepers stand still.
applyDuo :: Level -> DuoAction -> Game -> Game
applyDuo level a g = settle (stepGame level (duoPress a) g)
  where
    settle s
      | gPhase s == Playing && (gMotion s /= Settled || maybe False ((/= Settled) . ptMotion) (gPartner s))
          = settle (stepGame level noInput s)
      | otherwise = s

-- | All a level for two can tell apart once both keepers stand still.
type DuoKey = (Era, Cell, Bool, Maybe (Cell, Bool), Phase, Set.Set Cell, [(Cell, Mirror)])

duoKey :: Game -> DuoKey
duoKey g = ( gEra g, gCell g, gHasCell g, fmap (\p -> (ptCell p, ptHasCell p)) (gPartner g)
           , gPhase g, gCrates g, filter (mMovable . snd) (Map.toList (gMirrors g)) )

data DuoResult = DuoResult
  { drStates :: !Int                        -- ^ states reached
  , drGoal   :: !(Maybe (Int, [DuoAction])) -- ^ cheapest cost to win, and a route
  }

-- | Cheapest-first over the real rules, as 'search' is, with time refilled before
-- each action. @allowed@ limits which actions may be taken.
duoSearch :: Level -> (DuoAction -> Bool) -> Game -> DuoResult
duoSearch level allowed start = go (Set.singleton (0, k0)) (Map.singleton k0 0)
                                   (Map.singleton k0 start) Map.empty
  where
    k0 = duoKey start
    acts = filter allowed duoActions
    go queue costs games parents = case Set.minView queue of
      Nothing -> DuoResult (Map.size costs) Nothing
      Just ((cost, key), rest)
        | cost > Map.findWithDefault cost key costs -> go rest costs games parents
        | phaseOf key == Won -> DuoResult (Map.size costs) (Just (cost, routeTo parents key))
        | phaseOf key /= Playing -> go rest costs games parents
        | otherwise ->
            let g = refill (games Map.! key)
                step (q, cs, gs, ps) a =
                  let next = applyDuo level a g
                      nk = duoKey next
                      nc = cost + gRemaining g - gRemaining next
                  in if nk == key || maybe False (<= nc) (Map.lookup nk cs) then (q, cs, gs, ps)
                     else (Set.insert (nc, nk) q, Map.insert nk nc cs, Map.insert nk next gs, Map.insert nk (key, a) ps)
                (q', cs', gs', ps') = foldl' step (rest, costs, games, parents) acts
            in go q' cs' gs' ps'
    phaseOf (_, _, _, _, p, _, _) = p
    refill g = g { gRemaining = 1000000, gHistory = [], gFeedback = Nothing, gBump = 0 }
    routeTo parents = walk []
      where
        walk route k = case Map.lookup k parents of
          Nothing -> route
          Just (p, a) -> walk (a : route) p
