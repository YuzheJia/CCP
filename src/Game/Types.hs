-- | Plain data for the whole game. No Gloss, no IO.
module Game.Types
  ( Era (..), otherEra
  , Phase (..)
  , Cell
  , Dir (..), dirDelta, stepCell
  , TickInput (..), noInput
  , Motion (..)
  , Feedback (..), FeedbackKind (..)
  , Level (..)
  , Game (..)
  , stepTicks, shiftTicks, levelTicks, ticksPerSecond
  , blockedFeedbackTicks, pickupFeedbackTicks, bumpTicks
  ) where

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
  } deriving (Eq, Show)

noInput :: TickInput
noInput = TickInput Nothing False False False False

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
  | CellCollected
  deriving (Eq, Show)

data Feedback = Feedback
  { fbKind :: !FeedbackKind
  , fbLeft :: !Int
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

data Game = Game
  { gPhase        :: !Phase
  , gEra          :: !Era
  , gCell         :: !Cell
  , gHasCell      :: !Bool
  , gRemaining    :: !Int          -- ^ level ticks left
  , gMotion       :: !Motion
  , gFacing       :: !Dir          -- ^ which way the keeper looks; no rule reads it
  , gPendingShift :: !Bool         -- ^ at most one shift waits for a step to land
  , gPendingMove  :: !(Maybe Dir)  -- ^ the last tap made while busy
  , gFeedback     :: !(Maybe Feedback)
  , gBump         :: !Int          -- ^ ticks of "walked into a wall" nudge left
  , gPrev         :: !TickInput    -- ^ last tick's keys, for finding fresh presses
  , gArmed        :: !Bool         -- ^ the held direction was freshly pressed in this
                                   --   screen, so it may repeat
  } deriving (Eq, Show)

ticksPerSecond :: Int
ticksPerSecond = 60

-- | 180 seconds.
levelTicks :: Int
levelTicks = 180 * ticksPerSecond

-- | One cell of walking, which is also the held-key repeat interval.
stepTicks :: Int
stepTicks = 8

-- | About 0.22 s: the input lock while the era wave crosses the map.
shiftTicks :: Int
shiftTicks = 13

blockedFeedbackTicks, pickupFeedbackTicks, bumpTicks :: Int
blockedFeedbackTicks = 96
pickupFeedbackTicks  = 210
bumpTicks            = 8
