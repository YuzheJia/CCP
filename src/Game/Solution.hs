{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Player-sized actions over the real 'stepGame': the intended route, and the
-- search that re-checks the level through the implementation itself.
-- Tests use the routes and cheapest-first searches; Main keeps and advances
-- the resumable dead-end search between frames. This module does no IO
-- and imports no graphics. A search edge presses one key and waits for animation
-- to settle, using the same Game.Logic rules as play. Costs are budget seconds,
-- not elapsed ticks; animation still advances at 60 ticks per second.
-- Map coordinates are integer cells, x rightwards and y downwards. Time, history
-- and decorative state are omitted from the one-keeper key on purpose: dead-end
-- reachability asks whether the board can be solved with replenished time.
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
  , DeadSearch
  , startDeadSearch
  , advanceDeadSearch
  , finishDeadSearch
  , deadSearchFinished
  , deadSearchResult
  , knownDeadEnd
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

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import qualified Control.Monad.ST.Lazy as Lazy
import Data.Array.Base (getNumElements, numElements, unsafeAt, unsafeFreeze, unsafeRead, unsafeWrite)
import Data.Array.ST (MArray, STUArray, newArray, newArray_)
import Data.Array.Unboxed (UArray)
import Data.Bits (finiteBitSize, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Int (Int32)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Word (Word32, Word64)
import Game.Logic
import Game.Types

-- | One fresh player command, not one animation tick. Go can walk, push or jump
-- according to the board. The bang forces its direction; deriving supplies
-- equality, ordering (for containers) and printable diagnostics.
-- Go carries the direction; ShiftEra changes the shared era; SendEcho requests
-- the optional echo mechanic, which ordinary campaign rules disable.
data Action = Go !Dir | ShiftEra | SendEcho -- ^ Move, shift or echo command.
  deriving (Eq, Ord, Show)

-- | Candidate commands in deterministic order, including echo even if disabled.
-- Enum/Bounded on Dir enumerate its four constructors; refused moves are filtered
-- by comparing resulting keys rather than by duplicating the rules here.
allActions :: [Action]
allActions = ShiftEra : SendEcho : map Go [minBound .. maxBound]

-- | Human-readable keyboard name for a command; used in test/route diagnostics.
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
    -- Each helper repeats a direction n times; concat above joins route segments.
    up n    = replicate n (Go DirUp)
    down n  = replicate n (Go DirDown)
    left n  = replicate n (Go DirLeft)
    right n = replicate n (Go DirRight)

-- | A cheapest route for each level too big to search on every run, found once by
-- the full 'search'. The Long Reflection reaches about four million states, which
-- takes minutes and gigabytes, so the tests and the demo replay this instead.
-- W A S D are steps and @_@ is a shift.
knownRoute :: Level -> Maybe [Action]
-- Lookup is by exact level name. (<$>) maps only a successful Maybe result;
-- a missing name leaves Nothing, telling the caller to use a real search.
knownRoute level = map key <$> lookup (lvName level) routes
  where
    routes =
      [ ( "Level 3 - The Long Reflection"
        , "WWW_DDDDWWWWWSAWASAAWWDDDDDSASSSAAAWWADWDDWAWSSSDSSAAWASSD_SSS"
          ++ replicate 17 'A' ++ replicate 9 'W' )
      ]
    -- The stored alphabet uses underscore for shifts; the fallback also treats
    -- any other non-direction character as a shift, so this is trusted data.
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
    -- Record update syntax copies noInput and replaces only the named fields.
    -- Both direction fields are set so the same command works for keeper one.
    press = case action of
      Go dir   -> noInput { tiDir = Just dir, tiDir1 = Just dir }
      ShiftEra -> noInput { tiShift = True }
      SendEcho -> noInput { tiEcho = True }
    -- Idle ticks finish movement but spend no extra budget. A terminal phase
    -- returns immediately; continuing to wait there could never settle a win.
    settle s
      | gPhase s == Playing && gMotion s /= Settled = settle (stepGame level noInput s)
      | otherwise = s

-- | Everything the rules can tell apart once the player has settled. Crates, filled
-- pits, footprints and the echo belong here: leave them out and different boards
-- would be merged. A level without an echo keeps no footprints, so for it these add
-- nothing.
data StateKey = StateKey
  { skEra     :: !Era -- ^ Shared era used for collision and pickup.
  , skCell    :: !Cell -- ^ Settled first-keeper cell, x right and y down.
  , skHasCell :: !Bool -- ^ Whether keeper one carries the power cell.
  , skPhase   :: !Phase -- ^ Playing or terminal status; wins must stay distinct.
  , skCrates  :: !(Set.Set Cell) -- ^ Crates, shared by both eras.
  , skFilled  :: !(Set.Set Cell) -- ^ Pits permanently filled by consumed crates.
  , skTrail   :: ![Spot] -- ^ Most recent footprint first, including its era.
  , skEcho    :: !(Maybe Spot) -- ^ Stationary echo's era and cell, if any.
  , skMirrors :: ![(Cell, Mirror)]  -- ^ the movable ones only: the rest never change
  } deriving (Eq, Ord, Show)

-- | Forget transient fields of a settled one-keeper game. Map.toList orders
-- mirrors by cell, making the retained movable mirrors canonical for equality.
-- Fixed mirrors come from the Level; the second keeper needs DuoKey instead.
stateKey :: Game -> StateKey
stateKey g = StateKey (gEra g) (gCell g) (gHasCell g) (gPhase g) (gCrates g) (gFilled g)
                      (gTrail g) (gEcho g)
                      (filter (mMovable . snd) (Map.toList (gMirrors g)))

-- | Exhaustive reachable-state result. Costs and the winning route use seconds;
-- the dead list ignores the actual level budget and includes no unreachable keys.
data SearchResult = SearchResult
  { srStates   :: !(Map.Map StateKey Int)    -- ^ cheapest cost to reach each state
  , srGoal     :: !(Maybe (Int, [Action]))   -- ^ cheapest cost to win, and a route
  , srDeadEnds :: ![StateKey]                -- ^ reachable, but cannot then win
  }

-- | Every state of a level that can be reached but can no longer be won, whatever
-- the time left. A level's own rules can tell the player when they have walked into
-- one, by looking it up.
--
-- For a level for one keeper this is exactly the set of 'srDeadEnds' that 'search'
-- finds from 'startPlaying', worked out all at once by the much cheaper
-- 'DeadSearch' below. The game itself runs that search a slice at a time instead,
-- so no frame waits for it.
deadStates :: Level -> Set.Set StateKey
deadStates level =
  -- A finished search always has its set, so the empty set is never used.
  fromMaybe Set.empty (deadSearchResult (finishDeadSearch (startDeadSearch level)))

-- | Work carried between Dijkstra iterations. A strict record field forces the
-- outer container, not every element. Maps own the best known costs and games;
-- the priority queue may still contain older, more expensive entries.
data Explored = Explored
  { exQueue   :: !(Set.Set (Int, StateKey)) -- ^ Cost in seconds, then tie-break key.
  , exCosts   :: !(Map.Map StateKey Int) -- ^ Least cost found for each key.
  , exGames   :: !(Map.Map StateKey Game) -- ^ Representative game for that cost.
  , exParents :: !(Map.Map StateKey (StateKey, Action)) -- ^ Best route's last edge.
  , exEdges   :: ![(StateKey, StateKey)] -- ^ All accepted directed edges, reversed.
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
    -- This where block shares the explored graph between all three result
    -- fields. Laziness means merely constructing SearchResult can defer work.
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

    -- Set orders pairs lexicographically, so minView supplies the cheapest
    -- queued state. Stale costs and terminal states need no further expansion.
    explore ex = case Set.minView (exQueue ex) of
      Nothing -> ex
      Just ((cost, key), queue)
        | cost > Map.findWithDefault cost key (exCosts ex) || skPhase key /= Playing ->
            explore ex { exQueue = queue }
        | otherwise ->
            explore (foldl' (relax cost key (exGames ex Map.! key)) ex { exQueue = queue } allActions)

    -- A strict left fold tries the six commands without retaining a chain of
    -- pending accumulator updates. Every non-self edge is kept for the backward
    -- reachability pass, even if it does not improve the cheapest route.
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
        -- Read the actual charge from stepGame. Refilled time removes the level
        -- budget constraint; rejected commands cannot invent a useful edge.
        ready = refill g
        next = applyAction level action ready
        nextKey = stateKey next
        nextCost = cost + gRemaining ready - gRemaining next
        linked = ex { exEdges = (key, nextKey) : exEdges ex }

    refill g = g { gRemaining = plentyOfTime, gHistory = [], gFeedback = Nothing, gBump = 0 }
    plentyOfTime = 1000000 :: Int

    -- Follow best parents back to the start, prepending commands so the result
    -- is already in play order. The start has no parent.
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
    -- Membership suppresses revisits, so cycles in the puzzle graph terminate.
    -- A comprehension filters predecessors not yet known to lead to a win.
    spread done (k : rest) =
      let new = [p | p <- Map.findWithDefault [] k predecessors, not (p `Set.member` done)]
      in spread (foldr Set.insert done new) (new ++ rest)

-- Dead ends, a slice at a time --------------------------------------------------
--
-- The game says "This can no longer be won" the moment the player's state is one
-- the level cannot be won from. Knowing that means finding every state the level
-- can reach, with the moves between them, and then following those moves backwards
-- from every won state: any state the backward walk never reaches is dead.
--
-- 'search' finds the same states and moves, but it also works out the cheapest cost
-- of every state, so it needs a priority queue and maps keyed by 'StateKey', whose
-- derived 'Ord' compares sets through lists. On The Long Way Round (190,082 states)
-- that took about 7.5 seconds and 300 MB, all of it inside the first frame of the
-- level, which froze the game. The search here needs no costs, so it takes states in
-- the order it finds them, and it differs from 'search' in how it stores things:
--
--   * Each state gets a number the first time it is seen. Its key is written once,
--     packed into a few machine words ('writeKey'), and found again through a hash
--     table, so a lookup compares a handful of words instead of whole sets.
--   * No game is kept. States are expanded in the order they were numbered, and
--     each one's game is rebuilt from its key when its turn comes ('gameAt').
--   * Each move is two numbers in flat arrays: where it came from, and the previous
--     move into the same state. That threads a list of predecessors through every
--     state with no boxed list at all.
--   * Everything above lives in unboxed arrays, which the garbage collector neither
--     copies nor reads. That matters here: a major collection copies every boxed
--     value alive, and the game runs one every second (the vendored Gloss used to
--     run one after every frame it drew).
--
-- Together these search The Long Way Round in about one second, with about 23 MB
-- alive at its peak (50 MB of heap in use), where 'search' takes seven and a half
-- seconds and 220 to 300 MB. Most of that
-- second is the real 'stepGame' itself, nine to fourteen ticks for each of the
-- million or so actions tried.
--
-- The work is cut into small units, and 'advanceDeadSearch' runs as many as it is
-- asked for, so the game can spend a couple of milliseconds a frame on it.
--
-- Why a game rebuilt from its key gives exactly 'search''s moves: 'search' expands
-- each state from the game of the cheapest path to it, refilled. The rebuilt game
-- agrees with it on everything the key holds, and on the mirrors fixed in the wall,
-- which never move; both have the time, history, feedback and nudge refilled; and
-- every action leaves the keeper settled with nothing pending or armed, as a fresh
-- start does. All that is left to differ is fields no rule reads ('gFacing',
-- 'gPushing', 'gAttempt') and last tick's keys ('gPrev'). Those are no keys at all
-- after every action but an echo, which is over on its own press tick and so leaves
-- the echo key held (and the start, which leaves Enter held, which no rule reads in
-- play). The only press a held key could swallow is another echo, and an echo sent
-- straight after an echo has no footprints to walk back along, so it is refused
-- either way and changes nothing. The tests check the two searches agree on every
-- campaign level; they also agree on every earlier level but The Long Reflection,
-- which is too big for 'search'.
--
-- A level for two is another matter: the second keeper is not in the key, so the
-- rebuilt game puts them back where they started. The game never looks for dead
-- ends in a level for two.

-- | A level's dead-end search, part way through or finished. 'startDeadSearch'
-- makes one without doing any work; 'advanceDeadSearch' runs some of it.
data DeadSearch
  = Searching Steps
    -- ^ Still going. Looking inside the 'Steps' runs the next unit of work, so it
    --   is left unevaluated until 'advanceDeadSearch' asks for it.
  | Searched !DeadEnds (Set.Set StateKey)
    -- ^ Finished: the dead states packed for quick lookups, and the same states as
    --   a set, which is only built if something asks for it.

-- | The work still to do, as a lazy chain. Evaluating a link runs one unit of work
-- and gives the next link, or the finished result once there is nothing left.
data Steps = Step Steps | Done DeadEnds -- ^ Deferred tail, or immutable result.

-- | The dead states of a finished search, packed: their keys back to back in one
-- unboxed array, and a hash table of their numbers, so a lookup is one packing and
-- a few word comparisons. The Long Way Round's 26,111 dead states take 1.4 MB like
-- this; as a 'Set.Set' of keys they take 29 MB of boxed heap, which every major
-- collection would copy again while the game runs.
data DeadEnds = DeadEnds
  { deCount  :: !Int                 -- ^ how many states are dead
  , deWords  :: !(UArray Int Word64) -- ^ every dead state's packed key, back to back
  , deStarts :: !(UArray Int Int)    -- ^ where each one's key starts; the next one's
                                     --   start is where it ends
  , deTable  :: !(UArray Int Int32)  -- ^ open addressing over their numbers: a number
                                     --   plus one, or 0 for an empty slot
  }

-- | A search for this level's dead states, not yet begun. Nothing is worked out
-- until 'advanceDeadSearch' asks for it, so this is free to make for every level.
startDeadSearch :: Level -> DeadSearch
startDeadSearch level = Searching (deadSteps level)

-- | Run up to @n@ more units of the search. A unit is one state expanded (six
-- actions through the real 'stepGame', a few microseconds), or a comparably small
-- piece of the backward walk or of packing the result. Evaluating the returned
-- search to weak head normal form does the work, so a caller can time it.
advanceDeadSearch :: Int -> DeadSearch -> DeadSearch
advanceDeadSearch n search0 = case search0 of
  Searched _ _ -> search0
  Searching steps0 -> go n steps0
  where
    -- Do not inspect steps at zero: inspecting its constructor would run another
    -- unit. Step's field is intentionally lazy; adding a bang would defeat this.
    go k steps
      | k <= 0 = Searching steps
      | otherwise = case steps of
          Step rest -> go (k - 1) rest
          Done ends -> Searched ends (deadEndSet ends)

-- | Run the search to the end, all at once.
finishDeadSearch :: DeadSearch -> DeadSearch
finishDeadSearch = advanceDeadSearch maxBound

-- | Whether the search has finished, so 'deadSearchResult' has the answer.
deadSearchFinished :: DeadSearch -> Bool
deadSearchFinished s = case s of
  Searched _ _ -> True
  Searching _ -> False

-- | Every reachable state that can no longer be won, once the search has finished:
-- the same set 'deadStates' gives. The set is built the first time it is asked for.
deadSearchResult :: DeadSearch -> Maybe (Set.Set StateKey)
deadSearchResult s = case s of
  Searched _ set -> Just set
  Searching _ -> Nothing

-- | Whether this state is known to be dead. Until the search has finished nothing
-- is known, so the answer is 'False' and the game simply says nothing yet. Cheap
-- enough to ask every frame: the key is packed once and looked up in a hash table.
knownDeadEnd :: DeadSearch -> StateKey -> Bool
knownDeadEnd s key = case s of
  Searched ends _ -> isDeadEnd ends key
  Searching _ -> False

-- | All of a level's work, as a lazy chain of units. Lazy 'Lazy.ST' runs each unit
-- only when its link is looked at, and always in order, so the chain is an ordinary
-- pure value even though the units inside update arrays in place.
deadSteps :: Level -> Steps
deadSteps level = Lazy.runST (Lazy.strictToLazyST (newEnv level) >>= loop Exploring)
  where
    -- strictToLazyST runs each mutable unit completely before exposing a Step.
    -- Lazy.runST hides the state-thread type s, so no mutable array escapes.
    -- Sharing an evaluated Step shares its result; it does not rerun mutations.
    -- Allocation at stage changes is also part of a unit, so n bounds work units,
    -- not milliseconds; Main measures and adjusts the amount it requests.
    loop stage env = do
      next <- Lazy.strictToLazyST (unit env stage)
      case next of
        Left ends -> pure (Done ends)
        -- The rest of the chain is only run when this link's field is looked at.
        Right stage' -> Step <$> loop stage' env

-- The working state ---------------------------------------------------------------

-- | Everything a search keeps between units. Arrays that grow sit in an 'STRef', so
-- a bigger copy can replace them; each starts small and doubles when full.
data Env s = Env
  { enLevel   :: !Level -- ^ Immutable rules and maps used by every expansion.
  , enFresh   :: !Game
    -- ^ the level's fresh start, which every game expanded is rebuilt from
  , enFixed   :: !(Map.Map Cell Mirror)
    -- ^ the mirrors set in the wall: they never move, so no key holds them
  , enCounts  :: !(STUArray s Int Int)
    -- ^ the counters, one slot per 'Counter'
  , enKeys    :: !(STRef s (STUArray s Int Word64))
    -- ^ every state's packed key, back to back, by state number
  , enStarts  :: !(STRef s (STUArray s Int Int))
    -- ^ where each state's key starts in 'enKeys'; state @n@'s key ends where state
    --   @n + 1@'s starts, so there is always one more entry than there are states
  , enHashes  :: !(STRef s (STUArray s Int Word32))
    -- ^ each state's hash, so growing the table needs no key read again
  , enTable   :: !(STRef s (STUArray s Int Int32))
    -- ^ open addressing over state numbers: a number plus one, or 0 for empty.
    --   Kept at most half full, so a probe seldom looks at more than two slots
  , enLatest  :: !(STRef s (STUArray s Int Int32))
    -- ^ for each state, the last move found into it, or -1 for none
  , enFrom    :: !(STRef s (STUArray s Int Int32))
    -- ^ for each move, the state it leaves
  , enEarlier :: !(STRef s (STUArray s Int Int32))
    -- ^ for each move, the move found before it into the same state, or -1. Together
    --   with 'enLatest' this is each state's list of predecessors, kept unboxed
  , enWon     :: !(STRef s [Int])
    -- ^ the numbers of the won states found so far
  }

-- | The counters a search keeps, each in its own slot of 'enCounts'.
data Counter
  = StatesFound   -- ^ states numbered so far; the next new state gets this number
  | WordsUsed     -- ^ words of 'enKeys' holding keys
  | MovesFound    -- ^ moves stored
  | Expanded      -- ^ states expanded so far: states are expanded in the order they
                  --   were numbered, so those from here to 'StatesFound' are waiting
  | OnStack       -- ^ states waiting in the backward walk's stack
  | CanWin        -- ^ states the backward walk has reached
  | CanWinWords   -- ^ the words of those states' keys
  | Scanned       -- ^ states looked at while packing the result
  | DeadPacked    -- ^ dead states packed so far
  | DeadWords     -- ^ words of dead keys packed so far
  deriving (Enum, Bounded)

-- | A counter's value.
counter :: Env s -> Counter -> ST s Int
counter env c = unsafeRead (enCounts env) (fromEnum c)

-- | Set a counter.
setCounter :: Env s -> Counter -> Int -> ST s ()
setCounter env c = unsafeWrite (enCounts env) (fromEnum c)

-- | The three stages of a search. The arrays of the later stages are made only when
-- the stage begins, sized by what the stages before found.
data Stage s
  = Exploring
    -- ^ expanding states in the order they were found
  | Spreading !(Backward s)
    -- ^ walking moves backwards from every won state
  | Packing !(Backward s) !(Packed s)
    -- ^ copying the keys of the states the walk never reached

-- | The backward walk's arrays, one entry per state.
data Backward s = Backward
  { bwCanWin :: !(STUArray s Int Bool)   -- ^ reached by the walk: the level can be won from here
  , bwStack  :: !(STUArray s Int Int32)  -- ^ reached, but its predecessors not yet looked at
  }

-- | The result, while it is being packed: what 'DeadEnds' holds, still writable.
data Packed s = Packed
  { pkWords  :: !(STUArray s Int Word64) -- ^ Dead keys only, in 64-bit words.
  , pkStarts :: !(STUArray s Int Int) -- ^ Word offsets, with a final sentinel.
  , pkTable  :: !(STUArray s Int Int32) -- ^ Dead-state index plus one, or zero.
  }

-- | A search with only the start state found.
newEnv :: Level -> ST s (Env s)
newEnv level = do
  -- Counters are indexed by the derived Enum values. Capacities below count
  -- elements, not bytes: Word64 keys, Int offsets and Int32 state/edge links.
  -- Arrays grow independently; -1 ends an edge chain while 0 empties a hash slot.
  counts <- newArray (fromEnum (minBound :: Counter), fromEnum (maxBound :: Counter)) 0
  keys <- newArray (0, 255) 0 >>= newSTRef
  starts <- newArray (0, 63) 0 >>= newSTRef
  hashes <- newArray (0, 63) 0 >>= newSTRef
  table <- newArray (0, 127) 0 >>= newSTRef
  latest <- newArray (0, 63) (-1) >>= newSTRef
  froms <- newArray (0, 255) 0 >>= newSTRef
  earlier <- newArray (0, 255) (-1) >>= newSTRef
  won <- newSTRef []
  let env = Env
        { enLevel = level
        , enFresh = initialGame level
        , enFixed = Map.filter (not . mMovable) (lvMirrors level)
        , enCounts = counts
        , enKeys = keys
        , enStarts = starts
        , enHashes = hashes
        , enTable = table
        , enLatest = latest
        , enFrom = froms
        , enEarlier = earlier
        , enWon = won
        }
      -- The start becomes state 0, the first to be expanded.
      key = stateKey (startPlaying level)
  (n, _) <- intern env key
  when (skPhase key == Won) (writeSTRef won [n])
  pure env

-- | The game a state is expanded from, rebuilt from its key: the level's fresh start
-- with everything the key holds put in, and the mirrors fixed in the wall where the
-- level puts them. The time is refilled, and there is no undo history, feedback or
-- nudge, as 'search''s own @refill@ leaves them before every action, so the time can
-- never end the search and old feedback never changes what an action does. A fresh
-- start is also how every action leaves a keeper: settled, with nothing pending or
-- armed. Why nothing else matters is explained at the top of this section.
gameAt :: Env s -> StateKey -> Game
gameAt env key = (enFresh env)
  { gPhase     = skPhase key
  , gEra       = skEra key
  , gCell      = skCell key
  , gHasCell   = skHasCell key
  , gRemaining = 1000000
  , gCrates    = skCrates key
  , gFilled    = skFilled key
  , gMirrors   = Map.union (Map.fromDistinctAscList (skMirrors key)) (enFixed env)
  , gTrail     = skTrail key
  , gEcho      = skEcho key
  }

-- | One unit of work in the given stage: the next stage, or the finished result.
unit :: Env s -> Stage s -> ST s (Either DeadEnds (Stage s))
unit env stage = case stage of
  -- Exhaust discovery before walking backwards: a late edge could otherwise
  -- make a supposedly dead state live. Only then can exact result sizes be known.
  Exploring -> do
    more <- expandNext env
    if more then pure (Right Exploring) else Right . Spreading <$> beginSpreading env
  Spreading back -> do
    more <- spreadBack env back
    if more then pure (Right stage) else Right . Packing back <$> beginPacking env
  Packing back packed -> do
    more <- pack env back packed
    if more then pure (Right stage) else Left <$> finishPacking env packed

-- Exploring -------------------------------------------------------------------------

-- | Expand the next state found and not yet expanded: every action from it, through
-- the real rules. 'False' once there is nothing left to expand.
--
-- States are expanded in the order they were numbered, so the ones waiting are
-- simply the numbers from 'Expanded' up to 'StatesFound', and their keys are
-- already in the key store. No game is kept between units: each is rebuilt from its
-- key ('gameAt'). This also reduces major-collection work. The old Gloss loop
-- collected after every frame; the vendored loop now requests it once a second.
-- In the earlier measurements, retaining a queue of games made each collection
-- take two thirds of a millisecond longer.
--
-- As in 'search': only a state still being played is expanded, every action in
-- 'allActions' is tried on its refilled game, and an action that leaves the state
-- as it was is no move at all.
expandNext :: Env s -> ST s Bool
expandNext env = do
  here <- counter env Expanded
  found <- counter env StatesFound
  if here >= found
    then pure False
    else do
      setCounter env Expanded (here + 1)
      key <- readKey env here
      when (skPhase key == Playing) (forM_ allActions (try here (gameAt env key)))
      pure True
  where
    -- Each action starts from the same refilled game, not the previous action's
    -- result. Interning first identifies self-loops and newly discovered wins.
    try here g action = do
      let next = applyAction (enLevel env) action g
          key = stateKey next
      (there, new) <- intern env key
      unless (there == here) $ do
        link env here there
        when (new && skPhase key == Won) (modifySTRef' (enWon env) (there :))

-- | State @c@'s key, unpacked from the key store.
readKey :: Env s -> Int -> ST s StateKey
readKey env c = do
  keys <- readSTRef (enKeys env)
  starts <- readSTRef (enStarts env)
  cs <- unsafeRead starts c
  ce <- unsafeRead starts (c + 1)
  -- A copy of just this key, which no later write can touch, to unpack from.
  copy <- newArray_ (0, ce - cs - 1)
  forM_ [0 .. ce - cs - 1] $ \i -> unsafeRead keys (cs + i) >>= unsafeWrite copy i
  frozen <- freezeWords copy
  pure (unpackKey (frozen `unsafeAt`))

-- | An array handed over as immutable; it must never be written again.
freezeWords :: STUArray s Int Word64 -> ST s (UArray Int Word64)
freezeWords = unsafeFreeze

-- | The number of the state with this key, and whether it was new. The key is packed
-- straight onto the end of the key store; it stays there only if it is new, and is
-- otherwise written over by the next one.
intern :: Env s -> StateKey -> ST s (Int, Bool)
intern env key = do
  used <- counter env WordsUsed
  keys <- roomFor (enKeys env) (used + keyBound key)
  end <- writeKey keys used key
  h <- hashWords keys used end
  table <- readSTRef (enTable env)
  hashes <- readSTRef (enHashes env)
  starts <- readSTRef (enStarts env)
  size <- getNumElements table
  -- Sizes are powers of two, so bitwise AND wraps a probe without division.
  -- A collision is not equality: compare hash, length and every packed word.
  -- At most half-full tables always have an empty slot to end a failed probe.
  let mask = size - 1
      probe i = do
        slot <- unsafeRead table i
        if slot == 0
          then do
            n <- addState env i h end
            pure (n, True)
          else do
            let c = fromIntegral slot - 1
            hc <- unsafeRead hashes c
            same <- if hc /= h then pure False else sameKey keys starts c used end
            if same then pure (c, False) else probe ((i + 1) .&. mask)
  probe (fromIntegral h .&. mask)

-- | Whether state @c@'s key is the same as the words from @from@ to @to@.
sameKey :: STUArray s Int Word64 -> STUArray s Int Int -> Int -> Int -> Int -> ST s Bool
sameKey keys starts c from to = do
  cs <- unsafeRead starts c
  ce <- unsafeRead starts (c + 1)
  let go i
        | i >= to - from = pure True
        | otherwise = do
            a <- unsafeRead keys (cs + i)
            b <- unsafeRead keys (from + i)
            if a == b then go (i + 1) else pure False
  if ce - cs /= to - from then pure False else go 0

-- | Number the key just packed, ending at @end@, and put it in table slot @slot@.
addState :: Env s -> Int -> Word32 -> Int -> ST s Int
addState env slot h end = do
  n <- counter env StatesFound
  table <- readSTRef (enTable env)
  -- Store index+1 so state zero can coexist with the empty-slot marker. Starts
  -- already contains this key's beginning; append its end as the next sentinel.
  unsafeWrite table slot (fromIntegral n + 1)
  starts <- roomFor (enStarts env) (n + 1)
  unsafeWrite starts (n + 1) end
  hashes <- roomFor (enHashes env) n
  unsafeWrite hashes n h
  latest <- roomFor (enLatest env) n
  unsafeWrite latest n (-1)
  setCounter env StatesFound (n + 1)
  setCounter env WordsUsed end
  size <- getNumElements table
  when (2 * (n + 1) > size) (growTable env (2 * size))
  pure n

-- | Move every state into a table of the given size, using the hashes kept for it.
growTable :: Env s -> Int -> ST s ()
growTable env size = do
  n <- counter env StatesFound
  hashes <- readSTRef (enHashes env)
  table <- newArray (0, size - 1) 0
  forM_ [0 .. n - 1] $ \c -> do
    h <- unsafeRead hashes c
    place table (size - 1) (fromIntegral h .&. (size - 1)) (fromIntegral c + 1)
  writeSTRef (enTable env) table

-- | Put a value in the first empty slot from @i@ on, wrapping round.
place :: STUArray s Int Int32 -> Int -> Int -> Int32 -> ST s ()
place table mask i v = do
  slot <- unsafeRead table i
  if slot == 0 then unsafeWrite table i v else place table mask ((i + 1) .&. mask) v

-- | Store the move from state @from@ to state @to@, at the front of @to@'s list of
-- moves in. A move found twice is stored twice, as 'search' does; the backward walk
-- does not mind.
link :: Env s -> Int -> Int -> ST s ()
link env from to = do
  -- Prepend edge e to the destination's incoming list: froms[e] is the source,
  -- earlier[e] is the old head, latest[to] becomes e. These are edge numbers,
  -- not neighbouring state numbers; following earlier eventually reaches -1.
  e <- counter env MovesFound
  froms <- roomFor (enFrom env) e
  earlier <- roomFor (enEarlier env) e
  latest <- readSTRef (enLatest env)
  before <- unsafeRead latest to
  unsafeWrite froms e (fromIntegral from)
  unsafeWrite earlier e before
  unsafeWrite latest to (fromIntegral e)
  setCounter env MovesFound (e + 1)

-- | The array in the 'STRef', first replaced by a bigger copy if index @i@ does not
-- fit. Doubling gives amortised constant growth cost per inserted element: the
-- total number of copied slots is linear in the final capacity. An
-- element-by-element copy of the biggest array here (The Long Way Round's keys,
-- about a million words) still takes under a millisecond.
roomFor :: MArray (STUArray s) e (ST s) => STRef s (STUArray s Int e) -> Int -> ST s (STUArray s Int e)
roomFor ref i = do
  arr <- readSTRef ref
  size <- getNumElements arr
  if i < size
    then pure arr
    else do
      -- newArray_ leaves unused slots unspecified. Only initialised prefixes are
      -- read as data; callers initialise every newly used counter, offset or key.
      let size' = until (> i) (* 2) (max 16 size)
      arr' <- newArray_ (0, size' - 1)
      forM_ [0 .. size - 1] $ \j -> unsafeRead arr j >>= unsafeWrite arr' j
      writeSTRef ref arr'
      pure arr'
{-# INLINE roomFor #-}

-- Spreading -------------------------------------------------------------------------

-- | Every state is found: start the backward walk at every won state.
beginSpreading :: Env s -> ST s (Backward s)
beginSpreading env = do
  n <- counter env StatesFound
  -- (<$>) applies the constructor to the first allocation; (<*>) supplies the
  -- next allocated array. Both actions stay inside ST, in left-to-right order.
  back <- Backward <$> newArray (0, n - 1) False <*> newArray (0, n - 1) 0
  won <- readSTRef (enWon env)
  mapM_ (reach env back) won
  pure back

-- | Mark a state as one the level can be won from, if it was not already, and queue
-- its predecessors to be looked at. Counts it and its key's words, so the packing
-- stage knows exactly how much room the dead states need.
reach :: Env s -> Backward s -> Int -> ST s ()
reach env back c = do
  already <- unsafeRead (bwCanWin back) c
  -- Mark before pushing: even duplicate incoming edges cannot push a state
  -- twice, so a stack with one slot per discovered state is sufficient.
  unless already $ do
    unsafeWrite (bwCanWin back) c True
    top <- counter env OnStack
    unsafeWrite (bwStack back) top (fromIntegral c)
    setCounter env OnStack (top + 1)
    winners <- counter env CanWin
    setCounter env CanWin (winners + 1)
    starts <- readSTRef (enStarts env)
    cs <- unsafeRead starts c
    ce <- unsafeRead starts (c + 1)
    w <- counter env CanWinWords
    setCounter env CanWinWords (w + ce - cs)

-- | Look at the moves into up to 64 states from the stack (a few hundred moves, about
-- as long as expanding one state takes). 'False' once the stack is empty.
spreadBack :: Env s -> Backward s -> ST s Bool
spreadBack env back = do
  latest <- readSTRef (enLatest env)
  froms <- readSTRef (enFrom env)
  earlier <- readSTRef (enEarlier env)
  let pop k = do
        top <- counter env OnStack
        if top == 0
          then pure False
          else if k == 0
            then pure True
            else do
              c <- unsafeRead (bwStack back) (top - 1)
              setCounter env OnStack (top - 1)
              e <- unsafeRead latest (fromIntegral c)
              walk e
              pop (k - 1)
      -- Follow the destination's linked edge chain, reaching each source.
      -- Negative indices are sentinels and must never reach unsafeRead.
      walk e = unless (e < 0) $ do
        from <- unsafeRead froms (fromIntegral e)
        reach env back (fromIntegral from)
        unsafeRead earlier (fromIntegral e) >>= walk
  pop (64 :: Int)

-- Packing ---------------------------------------------------------------------------

-- | The walk is over: everything it never reached is dead. Make the result's arrays,
-- exactly as big as the dead states need.
beginPacking :: Env s -> ST s (Packed s)
beginPacking env = do
  n <- counter env StatesFound
  winners <- counter env CanWin
  used <- counter env WordsUsed
  winWords <- counter env CanWinWords
  -- Reach counted both states and key words, avoiding another sizing pass.
  -- Even with no dead states the table has two empty slots; its word array is
  -- empty and its starts array holds just the terminating zero.
  let dead = n - winners
      size = until (>= 2 * dead) (* 2) 2
  Packed <$> newArray (0, used - winWords - 1) 0
         <*> newArray (0, dead) 0
         <*> newArray (0, size - 1) 0

-- | Scan the next 256 discovered states, copying only the dead keys into the
-- result and entering them in its table. 'False' once every state has been looked at.
pack :: Env s -> Backward s -> Packed s -> ST s Bool
pack env back packed = do
  n <- counter env StatesFound
  from <- counter env Scanned
  let to = min n (from + 256)
  keys <- readSTRef (enKeys env)
  starts <- readSTRef (enStarts env)
  hashes <- readSTRef (enHashes env)
  size <- getNumElements (pkTable packed)
  forM_ [from .. to - 1] $ \c -> do
    canWin <- unsafeRead (bwCanWin back) c
    unless canWin $ do
      d <- counter env DeadPacked
      at <- counter env DeadWords
      cs <- unsafeRead starts c
      ce <- unsafeRead starts (c + 1)
      forM_ [0 .. ce - cs - 1] $ \i -> unsafeRead keys (cs + i) >>= unsafeWrite (pkWords packed) (at + i)
      unsafeWrite (pkStarts packed) d at
      h <- unsafeRead hashes c
      place (pkTable packed) (size - 1) (fromIntegral h .&. (size - 1)) (fromIntegral d + 1)
      setCounter env DeadPacked (d + 1)
      setCounter env DeadWords (at + ce - cs)
  setCounter env Scanned to
  pure (to < n)

-- | Close the last key's range and hand the arrays over, never to be written again.
finishPacking :: Env s -> Packed s -> ST s DeadEnds
finishPacking env packed = do
  dead <- counter env DeadPacked
  at <- counter env DeadWords
  -- unsafeFreeze avoids copying. Ownership passes to DeadEnds here; no later
  -- Step may mutate these arrays. The final offset closes the last key range.
  unsafeWrite (pkStarts packed) dead at
  DeadEnds dead <$> unsafeFreeze (pkWords packed)
                <*> unsafeFreeze (pkStarts packed)
                <*> unsafeFreeze (pkTable packed)

-- Looking the result up -----------------------------------------------------------

-- | Whether a state is among the dead ones: its key is packed and looked for.
isDeadEnd :: DeadEnds -> StateKey -> Bool
isDeadEnd ends key = runST $ do
  buf <- newArray (0, keyBound key - 1) 0
  end <- writeKey buf 0 key
  h <- hashWords buf 0 end
  let table = deTable ends
      mask = numElements table - 1
      -- Lookups use the same hash, wrapping and full equality as insertion.
      -- The first empty slot proves absence, since keys were never deleted.
      probe i = case table `unsafeAt` i of
        0 -> pure False
        slot -> do
          same <- matches (fromIntegral slot - 1)
          if same then pure True else probe ((i + 1) .&. mask)
      matches c = do
        let cs = deStarts ends `unsafeAt` c
            ce = deStarts ends `unsafeAt` (c + 1)
            go i
              | i >= end = pure True
              | otherwise = do
                  w <- unsafeRead buf i
                  if w == deWords ends `unsafeAt` (cs + i) then go (i + 1) else pure False
        if ce - cs /= end then pure False else go 0
  probe (fromIntegral h .&. mask)

-- | Every dead state, unpacked into a set. Only 'deadStates' and the tests need it.
deadEndSet :: DeadEnds -> Set.Set StateKey
deadEndSet ends =
  Set.fromList
    [ unpackKey (\i -> deWords ends `unsafeAt` (start + i))
    | c <- [0 .. deCount ends - 1], let start = deStarts ends `unsafeAt` c ]

-- Packed keys -----------------------------------------------------------------------
--
-- A key is packed as a string of bytes, eight to a word, the last word padded with
-- zeros. Each field is written in turn, each number as a variable-length integer: 7
-- bits a byte, the top bit set on every byte but the last, after zig-zagging so a
-- small negative number stays short. A list or set is its length, then its
-- elements. Every key of The Long Way Round packs into 34 bytes, so five words.
--
-- Nothing is assumed about the numbers, so any key packs and unpacks exactly. A
-- key's bytes always say where they end, so no key's bytes begin another's, and the
-- zero padding cannot make two different keys look the same.

-- | More words than a key can pack into: no number takes more than ten bytes.
keyBound :: StateKey -> Int
keyBound key = 2 + (1 + 10 * numbers) `div` 8
  where
    -- Include collection lengths, the optional echo's worst case and coordinates.
    -- Ten bytes cover a zigzagged 64-bit Int; the extra words allow flags/padding.
    numbers = 10 + 2 * Set.size (skCrates key) + 2 * Set.size (skFilled key)
                 + 3 * length (skTrail key) + 3 * length (skMirrors key)

-- | Pack a key into the array from word @at@ on, and give the index just past it.
-- The array must have room for 'keyBound' words from @at@; whatever those words held
-- before is overwritten, padding included.
--
-- Written as plain recursion over a byte position, rather than as a general fold
-- over the key's bytes, so GHC keeps every position in a register and packing
-- allocates nothing: this runs for each of the million or so moves The Long Way
-- Round tries.
writeKey :: STUArray s Int Word64 -> Int -> StateKey -> ST s Int
writeKey buf at key = do
  p1 <- byte (8 * at) flags
  p2 <- pair p1 (skCell key)
  p3 <- list pair p2 (Set.size (skCrates key)) (Set.toAscList (skCrates key))
  p4 <- list pair p3 (Set.size (skFilled key)) (Set.toAscList (skFilled key))
  p5 <- list spot p4 (length (skTrail key)) (skTrail key)
  p6 <- case skEcho key of
    Nothing -> int p5 0
    Just s -> int p5 1 >>= \p -> spot p s
  p7 <- list mirror p6 (length (skMirrors key)) (skMirrors key)
  -- Round the last byte's position up to the word after it.
  pure ((p7 + 7) `shiftR` 3)
  where
    -- The era, whether the cell is carried, and the phase share the first byte.
    -- Bit 0 is era, bit 1 possession, bits 2 upwards encode Phase. Remaining
    -- fields are player x/y, crates, filled pits, trail, echo tag/spot, mirrors.
    -- Sets use ascending cell order so equivalent sets have identical bytes.
    flags = fromIntegral (fromEnum (skEra key)) .|. (if skHasCell key then 2 else 0)
            .|. (phaseCode (skPhase key) `shiftL` 2)
    -- One byte at byte position @p@ of the array; the next position. The first byte
    -- of a word replaces the whole word, which clears out anything left there by a
    -- key packed earlier and dropped.
    byte !p !b = do
      let i = p `shiftR` 3
          offset = 8 * (p .&. 7)
      if offset == 0
        then unsafeWrite buf i b
        else do
          w <- unsafeRead buf i
          unsafeWrite buf i (w .|. (b `shiftL` offset))
      pure (p + 1)
    -- Seven bits a byte, lowest first; the top bit says another byte follows.
    varint !p !z
      | z < 0x80 = byte p z
      | otherwise = byte p ((z .&. 0x7f) .|. 0x80) >>= \p' -> varint p' (z `shiftR` 7)
    -- Zigzag makes negative coordinates short too. Positions p are byte offsets,
    -- whereas at and the returned end are word offsets; eight bytes make a word.
    int p n = varint p (zigzag n)
    pair p (x, y) = int p x >>= \p' -> int p' y
    spot p (era, c) = int p (fromEnum era) >>= \p' -> pair p' c
    mirror p (c, m) = pair p c >>= \p' -> byte p' (mirrorCode m)
    -- A list or a set: how many, then each element.
    list put p n xs = int p n >>= \p' -> each p' xs
      where
        each !q [] = pure q
        each !q (x : rest) = put q x >>= \q' -> each q' rest

-- | A hash of the words from @from@ to @to@: each word is mixed in with the
-- finishing step of MurmurHash3, so every bit of it reaches the low bits the table
-- uses. Kept to 32 bits, which is plenty for a table of a few million slots.
hashWords :: STUArray s Int Word64 -> Int -> Int -> ST s Word32
hashWords buf from to = go from (fromIntegral (to - from))
  where
    -- Bang patterns evaluate the index/hash each iteration rather than building
    -- arithmetic thunks. The mixing arithmetic wraps as Word64; the result is
    -- truncated to Word32 only at the end. Bounds are [from,to), in words.
    go !i !h
      | i >= to = pure (fromIntegral h)
      | otherwise = do
          w <- unsafeRead buf i
          go (i + 1) (fmix (h `xor` w))
    fmix k0 =
      let k1 = (k0 `xor` (k0 `shiftR` 33)) * 0xff51afd7ed558ccd
          k2 = (k1 `xor` (k1 `shiftR` 33)) * 0xc4ceb9fe1a85ec53
      in k2 `xor` (k2 `shiftR` 33)

-- | Unpack a key, given its @i@-th word for each @i@: 'writeKey' read backwards.
unpackKey :: (Int -> Word64) -> StateKey
unpackKey word = case runUnpack keyU 0 of Got key _ -> key
  where
    -- Only consume data produced by writeKey, not untrusted bytes. Length prefixes
    -- delimit collections, and the echo tag delimits its optional spot. Canonical
    -- set ordering justifies the unchecked fromDistinctAscList constructors.
    keyU = do
      flags <- byteU
      cell <- pairU
      crates <- listU pairU
      filled <- listU pairU
      trail <- listU spotU
      echoTag <- intU
      echo <- if echoTag == 0 then pure Nothing else Just <$> spotU
      mirrors <- listU mirrorU
      pure StateKey
        { skEra = toEnum (flags .&. 1)
        , skCell = cell
        , skHasCell = flags .&. 2 /= 0
        , skPhase = phaseOfCode (flags `shiftR` 2)
        , skCrates = Set.fromDistinctAscList crates
        , skFilled = Set.fromDistinctAscList filled
        , skTrail = trail
        , skEcho = echo
        , skMirrors = mirrors
        }
    -- Word index = byte index div 8; the remainder selects the little-endian byte.
    -- This is bit packing, independent of the host's in-memory byte order.
    byteU = Unpack $ \b ->
      Got (fromIntegral (word (b `shiftR` 3) `shiftR` (8 * (b .&. 7))) .&. 0xff) (b + 1)
    varintU :: Unpack Word64
    varintU = do
      b <- byteU
      if b < 0x80 then pure (fromIntegral b) else do
        rest <- varintU
        pure (fromIntegral (b .&. 0x7f) .|. (rest `shiftL` 7))
    intU = unzigzag <$> varintU
    pairU = (,) <$> intU <*> intU
    listU u = intU >>= \n -> sequence (replicate n u)
    spotU = (,) <$> (toEnum <$> intU) <*> pairU
    mirrorU = (,) <$> pairU <*> (mirrorOfCode <$> byteU)

-- | Reads a packed key a byte at a time: given the next byte's index, a value and
-- the index after it. Strict in both, so a key is read in one pass with no thunks
-- left behind, since every state expanded is read back this way.
newtype Unpack a = Unpack { runUnpack :: Int -> Got a } -- ^ Read at a byte offset.

-- | A value read, and the index of the byte after it.
data Got a = Got !a !Int -- ^ Value (forced to WHNF) and next byte offset.

-- | Map a decoded value while preserving the byte position. This type-class
-- instance is what permits (<$>) in the small field decoders above.
instance Functor Unpack where
  fmap f (Unpack u) = Unpack (\b -> case u b of Got a b' -> Got (f a) b')
  {-# INLINE fmap #-}

-- | Decode a function then its argument, threading the consumed-byte position.
-- pure consumes no input. These operations let independent fields be combined.
instance Applicative Unpack where
  pure a = Unpack (\b -> Got a b)
  {-# INLINE pure #-}
  Unpack uf <*> Unpack ua = Unpack (\b -> case uf b of
    Got f b' -> case ua b' of
      Got a b'' -> Got (f a) b'')
  {-# INLINE (<*>) #-}

-- | Decode a value then choose a decoder with it, needed for lengths and tags.
-- INLINE invites GHC to remove this small wrapper in the hot unpacking path.
instance Monad Unpack where
  Unpack u >>= k = Unpack (\b -> case u b of Got a b' -> runUnpack (k a) b')
  {-# INLINE (>>=) #-}

-- | A number made non-negative, small numbers either side of zero staying small:
-- 0, -1, 1, -2 ... become 0, 1, 2, 3 ...
zigzag :: Int -> Word64
zigzag n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` (finiteBitSize n - 1)))

-- | 'zigzag' undone.
unzigzag :: Word64 -> Int
unzigzag z = fromIntegral (z `shiftR` 1) `xor` negate (fromIntegral (z .&. 1))

-- | A phase as a number, for packing ('Phase' has no 'Enum' instance).
phaseCode :: Phase -> Word64
phaseCode p = case p of
  Menu -> 0
  Playing -> 1
  Paused -> 2
  Won -> 3
  Lost -> 4

-- | 'phaseCode' undone.
phaseOfCode :: Int -> Phase
phaseOfCode c = case c of
  0 -> Menu
  1 -> Playing
  2 -> Paused
  3 -> Won
  _ -> Lost

-- | A mirror in one byte: its era, its face, whether it moves, and its bend if any.
mirrorCode :: Mirror -> Word64
-- Bit 0 is era, bits 1-2 the first face, bit 3 movability, and bits 4 upwards
-- the optional second face plus one (zero means no bend). Values are internal.
mirrorCode m = fromIntegral (fromEnum (mEra m) .|. (fromEnum (mFace m) `shiftL` 1)
                             .|. (if mMovable m then 8 else 0)
                             .|. (maybe 0 ((+ 1) . fromEnum) (mBend m) `shiftL` 4))

-- | 'mirrorCode' undone.
mirrorOfCode :: Int -> Mirror
mirrorOfCode c = Mirror
  { mEra = toEnum (c .&. 1)
  , mFace = toEnum ((c `shiftR` 1) .&. 3)
  , mMovable = c .&. 8 /= 0
  , mBend = case c `shiftR` 4 of
      0 -> Nothing
      b -> Just (toEnum (b - 1))
  }

-- Two keepers -----------------------------------------------------------------
--
-- A level for two is checked by its own small search, over the same real
-- 'stepGame': each action is one keeper's key, or the shared era key. It is only for
-- the tests, which ask it whether two keepers can light the lamp, and whether one,
-- with the other standing still, cannot.

-- | One fresh command for keeper one, keeper two, or both through the era key.
data DuoAction = Go1 !Dir | Go2 !Dir | ShiftBoth -- ^ Actor-tagged direction or shift.
  deriving (Eq, Ord, Show)

-- | Shared shift followed by each keeper's four directions; echoes are excluded.
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

-- | Turn a command into one tick's fresh input. The actor-specific direction
-- chooses the keeper; tiDir also keeps the ordinary input representation intact.
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

-- | Settled state for the current two-island puzzle, retaining each carrier.
-- This key omits filled pits and echoes: duoSearch is for this fixture, whose
-- maps have no pits and whose baseRules disable echoes, not arbitrary duo levels.
duoKey :: Game -> DuoKey
duoKey g = ( gEra g, gCell g, gHasCell g, fmap (\p -> (ptCell p, ptHasCell p)) (gPartner g)
           , gPhase g, gCrates g, filter (mMovable . snd) (Map.toList (gMirrors g)) )

-- | Number of states discovered and, if found, a cheapest win in budget seconds.
-- The search stops at its first minimal win, so drStates need not be exhaustive.
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
    -- Unlike search, allowed depends only on the command, so filter once. This
    -- supports tests where one keeper must remain still throughout the route.
    k0 = duoKey start
    acts = filter allowed duoActions
    go queue costs games parents = case Set.minView queue of
      Nothing -> DuoResult (Map.size costs) Nothing
      Just ((cost, key), rest)
        | cost > Map.findWithDefault cost key costs -> go rest costs games parents
        -- With non-negative action prices, the first popped win is cheapest.
        | phaseOf key == Won -> DuoResult (Map.size costs) (Just (cost, routeTo parents key))
        | phaseOf key /= Playing -> go rest costs games parents
        | otherwise ->
            let g = refill (games Map.! key)
                -- Relax one edge, preserving the previous maps if the command
                -- changes no key or offers no lower cost. Parents track only
                -- improvements, sufficient to reconstruct the first win.
                step (q, cs, gs, ps) a =
                  let next = applyDuo level a g
                      nk = duoKey next
                      nc = cost + gRemaining g - gRemaining next
                  in if nk == key || maybe False (<= nc) (Map.lookup nk cs) then (q, cs, gs, ps)
                     else (Set.insert (nc, nk) q, Map.insert nk nc cs, Map.insert nk next gs, Map.insert nk (key, a) ps)
                (q', cs', gs', ps') = foldl' step (rest, costs, games, parents) acts
            in go q' cs' gs' ps'
    -- Project the phase without assigning names to the other tuple components.
    phaseOf (_, _, _, _, p, _, _) = p
    refill g = g { gRemaining = 1000000, gHistory = [], gFeedback = Nothing, gBump = 0 }
    routeTo parents = walk []
      where
        walk route k = case Map.lookup k parents of
          Nothing -> route
          Just (p, a) -> walk (a : route) p
