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
  , Mirror (..), mirrorFaces, exitFace
  , Level (..)
  , Partner (..)
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
  , tiLobby   :: !Bool          -- ^ leave a level and return to level select
  , tiCancel  :: !Bool          -- ^ Delete or Backspace: cancel the chosen order
  , tiDirRevealed :: !Bool     -- ^ no direction went down this tick, so if 'tiDir'
                               --   changed, a newer key was let go of, not a press
    -- In a level for two, each keeper has their own keys: W A S D for the first,
    -- the arrows for the second. 'tiDir' above is either set, for one player.
  , tiDir1    :: !(Maybe Dir)  -- ^ the W A S D keys alone
  , tiDir1Revealed :: !Bool
  , tiDir2    :: !(Maybe Dir)  -- ^ the arrow keys alone
  , tiDir2Revealed :: !Bool
  } deriving (Eq, Show)

noInput :: TickInput
noInput = TickInput Nothing False False False False False False False False False False False
                    Nothing False Nothing False

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
  | MirrorFaceSide      -- ^ pressed into a mirror's face with nowhere to land: faces never push
  | MirrorFixed         -- ^ pressed against a mirror set in the wall, which never moves
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

-- | A mirror stands in one era and throws light out of its face. A fixed one is set
-- in the wall; a movable one stands on floor and can be pushed from its back or sides.
data Mirror = Mirror
  { mEra     :: !Era
  , mFace    :: !Dir
  , mMovable :: !Bool
  , mBend    :: !(Maybe Dir)  -- ^ a corner mirror's second face, at right angles to
                              --   the first; it turns the light, and whoever steps through
  } deriving (Eq, Ord, Show)

-- | Every way a mirror throws light: one face, or two for a corner mirror.
mirrorFaces :: Mirror -> [Dir]
mirrorFaces m = mFace m : maybe [] pure (mBend m)

-- | Stepping into a mirror while moving @dir@: the way you come out. Straight on,
-- behind it, for a flat mirror; out of the other face for a corner one.
exitFace :: Mirror -> Dir -> Dir
exitFace m dir = case mBend m of
  Nothing -> dir
  Just b
    | b == reverseDir dir -> mFace m
    | otherwise -> b
  where
    reverseDir d = case d of
      DirUp -> DirDown
      DirDown -> DirUp
      DirLeft -> DirRight
      DirRight -> DirLeft

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
  , lvMirrors  :: !(Map Cell Mirror)
    -- ^ where the mirrors start; a mirror is solid in its own era
  , lvBudget   :: !Int          -- ^ seconds available in this level
  , lvRules    :: !Rules
  , lvName     :: !String
  , lvLesson   :: !String       -- ^ the mechanic this level introduces
  , lvPar      :: !Int          -- ^ the cheapest possible cost; a test re-derives it
  , lvDressed  :: !Bool         -- ^ the original station, with its hand-placed dressing
  , lvSpawn2   :: !(Maybe Cell) -- ^ where a second keeper starts, in a level for two
  , lvPassage  :: ![Cell]       -- ^ walls that open once the level is won, from the
                                --   lighthouse outward: the way on to a hidden level
  , lvSecret   :: !Bool         -- ^ never listed in the level hall; only found
  } deriving (Eq, Show)

-- | The second keeper, in a level for two. The fields are the keeper's own half of
-- 'Game'; the era, the crates, the mirrors and the time are shared.
data Partner = Partner
  { ptCell        :: !Cell
  , ptHasCell     :: !Bool
  , ptMotion      :: !Motion
  , ptFacing      :: !Dir
  , ptPushing     :: !(Maybe Cell)
  , ptPendingMove :: !(Maybe Dir)
  , ptArmed       :: !Bool
  , ptBump        :: !Int
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
  , snMirrors :: !(Map Cell Mirror)
  , snCost    :: !Int
  , snPartner :: !(Maybe Partner)  -- ^ the second keeper, where they stood
  } deriving (Eq, Show)

data Game = Game
  { gPhase        :: !Phase
  , gEra          :: !Era
  , gCell         :: !Cell
  , gHasCell      :: !Bool
  , gRemaining    :: !Int          -- ^ seconds of budget left; only actions spend it
  , gCrates       :: !(Set Cell)
  , gFilled       :: !(Set Cell)   -- ^ pits a crate has been pushed into
  , gMirrors      :: !(Map Cell Mirror) -- ^ where every mirror stands now
  , gTrail        :: ![Spot]       -- ^ where the player has stood, newest first, their
                                   --   own spot included; an echo walks it back
  , gEcho         :: !(Maybe Spot) -- ^ where the echo stands, holding down any plate
  , gHistory      :: ![Snapshot]   -- ^ accepted actions, newest first, for undo
  , gPushing      :: !(Maybe Cell) -- ^ where this step's crate or mirror is going; only for drawing
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
  , gPartner      :: !(Maybe Partner) -- ^ the second keeper, in a level for two
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
