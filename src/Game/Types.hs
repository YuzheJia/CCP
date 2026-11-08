-- | Plain data for the whole game. No Gloss, no IO.
module Game.Types
  ( Era (..), otherEra
  , Phase (..)
  , Cell
  , Dir (..), dirDelta, stepCell
  , TickInput (..), noInput
  , Motion (..)
  , Feedback (..), FeedbackKind (..)
  , Rules (..), baseRules, echoRules
  , Level (..)
  , Snapshot (..)
  , Spot
  , Game (..)
  , stepTicks, shiftTicks, ticksPerSecond
  , blockedFeedbackTicks, pickupFeedbackTicks, hintFeedbackTicks, bumpTicks
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)

data Era = Past | Present
  deriving (Eq, Ord, Show, Enum, Bounded)

otherEra :: Era -> Era
otherEra Past    = Present
otherEra Present = Past

data Phase = Menu | Playing | Paused | Won | Lost
  deriving (Eq, Ord, Show)

-- | (column, row). (0,0) is the top-left cell; y grows downward.
type Cell = (Int, Int)

data Dir = DirUp | DirDown | DirLeft | DirRight
  deriving (Eq, Ord, Show, Enum, Bounded)

dirDelta :: Dir -> Cell
dirDelta DirUp    = (0, -1)
dirDelta DirDown  = (0, 1)
dirDelta DirLeft  = (-1, 0)
dirDelta DirRight = (1, 0)

stepCell :: Dir -> Cell -> Cell
stepCell d (x, y) = let (dx, dy) = dirDelta d in (x + dx, y + dy)

-- | What is held down during one tick. The rules find fresh presses themselves
-- by comparing with the previous tick, so a held key can never fire twice.
data TickInput = TickInput
  { tiDir     :: !(Maybe Dir)  -- ^ most recently pressed direction still held
  , tiShift   :: !Bool
  , tiEnter   :: !Bool
  , tiRestart :: !Bool
  , tiPause   :: !Bool
  , tiUndo    :: !Bool
  , tiPeek    :: !Bool
  , tiEcho    :: !Bool
  ,tiInstructions :: Bool
  , tiDirRevealed :: !Bool     -- ^ no direction went down this tick, so if 'tiDir'
                               --   changed, a newer key was let go of, not a press
  } deriving (Eq, Show)

noInput :: TickInput
noInput = TickInput { tiDir = Nothing, tiShift = False, tiEnter = False, tiRestart = False, tiPause = False, tiUndo = False, tiPeek = False, tiEcho = False, tiInstructions = False, tiDirRevealed = False }

-- | The player does exactly one of these at a time. Being a sum type, "walking
-- while shifting" cannot be represented at all.
data Motion
  = Settled
  | Stepping { stepTo :: !Cell, stepLeft :: !Int }
    -- ^ 'gCell' stays the origin until the step lands; the destination was
    -- validated before the step began and is the only other cell involved.
  | Shifting { shiftLeft :: !Int }
    -- ^ The era has already changed. This only locks input during the wave.
  deriving (Eq, Show)

data FeedbackKind
  = ShiftBlocked !Cell  -- ^ this cell is a wall in the other era
  | ShiftTooCostly      -- ^ the cell is open, but there is not enough time left
  | PushBlocked !Cell   -- ^ a crate would be inside a wall in the other era here
  | PushNeedsPress      -- ^ holding a key walks up to a crate; pushing takes a new press
  | CrateRusted         -- ^ a crate was pressed against in the present
  | CellCollected
  | EchoNeedsSteps      -- ^ E with no footprints behind the player to walk back along
  | EchoInDoorway       -- ^ E while standing in a door: a new echo could shut it on you
  | JumpNeedsPress      -- ^ holding a key walks up a mirror's light; jumping takes a new press
  deriving (Eq, Show)

data Feedback = Feedback
  { fbKind :: !FeedbackKind
  , fbLeft :: !Int
  } deriving (Eq, Show)

-- | What actions cost, in seconds of a level's budget. Plain data, so a level (or
-- later a run's relics) can change the prices without touching the rules.
data Rules = Rules
  { ruStepCost          :: !Int
  , ruShiftCost         :: !Int
  , ruUndoRefundPercent :: !Int  -- ^ share of an undone action's cost given back
  , ruEchoCost          :: !Int
  , ruEchoLength        :: !Int  -- ^ footprints kept, counting the player's own spot;
                                 --   0 means this level has no echo at all
  , ruJumpCost          :: !Int  -- ^ stepping through a mirror, however far it carries you
  } deriving (Eq, Show)

-- | A switch is dearer than a step, so where and how often to switch is a real
-- choice. Even prices keep a half refund in whole seconds.
baseRules :: Rules
baseRules = Rules { ruStepCost = 2, ruShiftCost = 6, ruUndoRefundPercent = 50
                  , ruEchoCost = 4, ruEchoLength = 0, ruJumpCost = 4 }

-- | The levels that teach the echo: five footprints, the player's own included.
echoRules :: Rules
echoRules = baseRules { ruEchoLength = 5 }

-- | Both eras of the one location. Anything not in a walkable set is solid, so
-- out-of-bounds cells need no special case.
data Level = Level
  { lvWidth    :: !Int
  , lvHeight   :: !Int
  , lvPast     :: !(Set Cell)
  , lvPresent  :: !(Set Cell)
  , lvSpawn    :: !Cell
  , lvPowerCell :: !Cell
  , lvGoal     :: !Cell
  , lvCrates   :: !(Set Cell)
  , lvPits     :: !(Set Cell)
  , lvPlates   :: !(Set Cell)   -- ^ in the past: crates resting on all of them open...
  , lvDoors    :: !(Set Cell)   -- ^ ...these, in the present; solid walls otherwise
  , lvMirrors  :: !(Map Cell (Era, Dir))
    -- ^ a mirror is solid in its own era, and its face looks out this way
  , lvBudget   :: !Int          -- ^ seconds available in this level
  , lvRules    :: !Rules
  , lvName     :: !String
  , lvPar      :: !Int          -- ^ the cheapest possible cost; a test re-derives it
  , lvDressed  :: !Bool         -- ^ the original station, with its hand-placed dressing
  } deriving (Eq, Show)

-- | A place in time: which era, and which cell of it.
type Spot = (Era, Cell)

-- | The board as it was before one accepted action, and what that action cost.
data Snapshot = Snapshot
  { snEra     :: !Era
  , snCell    :: !Cell
  , snHasCell :: !Bool
  , snCrates  :: !(Set Cell)
  , snFilled  :: !(Set Cell)
  , snFacing  :: !Dir
  , snTrail   :: ![Spot]
  , snEcho    :: !(Maybe Spot)
  , snCost    :: !Int
  } deriving (Eq, Show)

data Game = Game
  { gPhase        :: !Phase
  , gEra          :: !Era
  , gCell         :: !Cell
  , gHasCell      :: !Bool
  , gRemaining    :: !Int          -- ^ seconds of budget left; only actions spend it
  , gCrates       :: !(Set Cell)
  , gFilled       :: !(Set Cell)   -- ^ pits a crate has been pushed into
  , gTrail        :: ![Spot]       -- ^ where the player has stood, newest first, their
                                   --   own spot included; an echo walks it back
  , gEcho         :: !(Maybe Spot) -- ^ where the echo stands, holding down any plate
  , gHistory      :: ![Snapshot]   -- ^ accepted actions, newest first, for undo
  , gPushing      :: !(Maybe Cell) -- ^ where this step's crate is going; only for drawing
  , gAttempt      :: !Int          -- ^ counts starts and restarts
  , gMotion       :: !Motion
  , gFacing       :: !Dir          -- ^ which way the keeper looks; no rule reads it
  , gPendingShift :: !Bool         -- ^ at most one shift waits for a step to land
  , gPendingMove  :: !(Maybe Dir)  -- ^ the last tap made while busy
  , gPendingUndo  :: !Bool         -- ^ an undo waits for the current action to end
  , gPendingEcho  :: !Bool         -- ^ an echo waits for the current action to end
  , gFeedback     :: !(Maybe Feedback)
  , gBump         :: !Int          -- ^ ticks of "walked into a wall" nudge left
  , gPrev         :: !TickInput    -- ^ last tick's keys, for finding fresh presses
  , gArmed        :: !Bool         -- ^ the held direction was freshly pressed in this
                                   --   screen, so it may repeat
  , gShowInstructions :: Bool
  } deriving (Eq, Show)

ticksPerSecond :: Int
ticksPerSecond = 60

-- | One cell of walking, which is also the held-key repeat interval.
stepTicks :: Int
stepTicks = 8

-- | About 0.22 s: the input lock while the era wave crosses the map.
shiftTicks :: Int
shiftTicks = 13

blockedFeedbackTicks, pickupFeedbackTicks, hintFeedbackTicks, bumpTicks :: Int
hintFeedbackTicks    = 120
blockedFeedbackTicks = 96
pickupFeedbackTicks  = 210
bumpTicks            = 8
