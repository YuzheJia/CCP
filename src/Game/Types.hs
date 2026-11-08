-- | Plain data for the whole game. No Gloss, no IO.
--
-- This module is the shared vocabulary of the project: the types every other module
-- talks in, the constants that time things, and a few tiny helpers that look at one
-- value only (which era is the other one, which way a direction points, which way a
-- mirror lets you out). It deliberately holds no rules: what a tick does is
-- 'Game.Logic', how two text maps become a 'Level' is 'Game.Level', the decorative
-- state is 'Game.Fx', and all drawing and IO live under @app/@. It imports nothing
-- from the project, only the 'Map' and 'Set' types from the containers package, so
-- any module can import it without making an import cycle.
--
-- Who uses it: every module under @src/Game/@ ('Game.Level', 'Game.Logic',
-- 'Game.Input', 'Game.Fx', 'Game.Run', 'Game.Solution', 'Game.Sound', 'Game.Story'),
-- most of @app/@, and the tests in @test/Main.hs@.
--
-- Units and conventions, used the same way everywhere:
--
--   * Time on screen runs in ticks, 60 a second ('ticksPerSecond'). The rules move
--     one fixed tick at a time, and @app/Main.hs@ turns real time into ticks. Every
--     duration in the code (a step, a wave, how long a note stays) is in ticks.
--   * The time budget is in whole seconds ('Int'). It is a price list, not a clock:
--     only accepted actions spend it (see 'Rules'), never the passing ticks.
--   * A position is a 'Cell', (column, row), counted from the top-left cell (0, 0),
--     with y growing down the map, the way the level maps are written as text.
--     Drawing turns a cell into canvas units in @app/Draw.hs@: the screen is 1280 by
--     800 canvas units, a cell is @cs = 76@ of them, and because Gloss's y grows
--     upwards the row is flipped there, never here.
--   * A level is one place at two times: a past map and a present map of the same
--     size. A thing either stands in one era (a wall, a mirror) or in both at once
--     (a crate, a plate).
--
-- Strict fields: nearly every record field below is written with a @!@ before its
-- type (for example @gRemaining :: !Int@). A strict field is evaluated when the
-- record is built, instead of being stored as an unevaluated expression (a thunk).
-- The game builds a new 'Game' every tick, and the dead-end search in
-- 'Game.Solution' builds millions of them, so lazy fields would let chains such as
-- @((100 - 2) - 2) - 6@ pile up in memory until something finally read them. A @!@
-- only evaluates a value as far as its outermost constructor (weak head normal
-- form), which is all of it for an 'Int', a 'Bool' or a small 'Maybe'; a list field
-- is forced only as far as its first cell.
module Game.Types
  -- @T (..)@ exports a type together with all its constructors and record fields.
  -- Names without it are type synonyms ('Cell', 'Spot') or functions and values.
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

-- Only the type names are imported. The modules that build and search maps and sets
-- import Data.Map.Strict and Data.Set qualified (Map.insert, Set.member), so their
-- functions do not clash with the Prelude's list functions of the same names.
import Data.Map.Strict (Map)
import Data.Set (Set)

-- | The two times the one place exists in. The keeper is always in exactly one of
-- them ('gEra'), Space switches between them ('Game.Logic.tryShift'), and a new
-- level starts in the present.
--
-- The derived instances: 'Eq' lets eras be compared with @==@; 'Ord' is needed
-- wherever an era is part of something sorted or used as a key, such as the
-- solver's state key 'Game.Solution.StateKey'; 'Show' prints them for the tests and
-- the trace; 'Enum' numbers them in the order written (Past is 0, Present is 1),
-- which 'Game.Solution' uses to pack an era into one bit; and 'Bounded' supplies
-- 'minBound' and 'maxBound' (Past and Present).
data Era = Past | Present
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The era a shift goes to: the past from the present, the present from the past.
-- Used by the shift rule, and by drawing for the Tab peek, which shows the same
-- moment in the other era.
otherEra :: Era -> Era
otherEra Past    = Present
otherEra Present = Past

-- | Where one level's game stands. It decides what a tick does with the keys (the
-- @case gPhase g of@ in 'Game.Logic.stepGame') and what the screen shows.
--
--   * 'Menu': not started yet. 'Game.Logic.initialGame' builds a game in this
--     phase; the title and the level hall keep one waiting behind them, and a fresh
--     Enter begins play. @app/Render.hs@ draws a 'Menu' game as the title screen.
--   * 'Playing': the rules run every tick.
--   * 'Paused': P stopped the level. Nothing moves and nothing is spent; P resumes
--     and R restarts. Keys held across a pause need a fresh press afterwards.
--   * 'Won': the lamp is lit (a keeper carrying the power cell stood on the
--     lighthouse in the present). In 'Game.Logic.stepGame' Enter or R starts the
--     level again; in the campaign 'Game.Run' catches Enter first and moves on.
--   * 'Lost': settled, with no undo waiting, and less than one step's price left.
--     Enter or R starts again, and Z still undoes, which can give back enough time
--     to play on.
--
-- There is no 'Enum' instance, so 'Game.Solution' numbers the phases by hand
-- (@phaseCode@ and @phaseOfCode@); a new phase has to be added there too.
data Phase = Menu | Playing | Paused | Won | Lost
  deriving (Eq, Ord, Show)

-- | (column, row). (0,0) is the top-left cell; y grows downward.
--
-- A type synonym, not a new type: a 'Cell' is an ordinary pair of 'Int's, so pair
-- functions such as 'fst' and 'snd' work on it, and 'dirDelta' can return a step as
-- one. A cell outside the map is allowed; it is simply never walkable.
type Cell = (Int, Int)

-- | The four ways to move or face, named as seen on the screen: 'DirUp' is towards
-- row 0 (north on the map) and 'DirLeft' towards column 0 (west). A mirror's face
-- looks one of these ways too ('mFace').
--
-- 'Enum' numbers them 0 to 3 in the order written, which 'Game.Solution' packs into
-- two bits of a mirror's code. 'Bounded' lets @[minBound .. maxBound]@ list all
-- four, as the solver's list of moves and the parser's plate-beside-a-door check do.
data Dir = DirUp | DirDown | DirLeft | DirRight
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One step in a direction, as a change of (column, row). Because y grows down the
-- map, up is (0, -1). The result is typed 'Cell' only because it is a pair of
-- 'Int's: it is a difference between two cells, not a place.
dirDelta :: Dir -> Cell
dirDelta DirUp    = (0, -1)
dirDelta DirDown  = (0, 1)
dirDelta DirLeft  = (-1, 0)
dirDelta DirRight = (1, 0)

-- | The neighbouring cell in a direction. There is no bounds check: a cell off the
-- map is just a cell that no walkable set contains, so the rules treat it as solid.
--
-- @let (dx, dy) = ... in ...@ takes the pair apart by pattern matching. A @let@ is
-- an expression of its own; a @where@ clause, used elsewhere, belongs to a whole
-- equation and can be seen by all its guards.
stepCell :: Dir -> Cell -> Cell
stepCell d (x, y) = let (dx, dy) = dirDelta d in (x + dx, y + dy)

-- | What is held down during one tick. The rules find fresh presses themselves
-- by comparing with the previous tick, so a held key can never fire twice.
--
-- From the keyboard it is built once a tick by 'Game.Input.tickInput'; the solver
-- ('Game.Solution.actionInputs'), the demo and the @--script@ option write their own,
-- usually as @noInput { tiShift = True }@. The previous tick's input is kept in the
-- game as 'gPrev', and a key counts as a fresh press when it is down in this
-- 'TickInput' and was not down in 'gPrev'.
--
-- "Down" means held at the tick, or pressed and let go since the last tick (a tap),
-- so a tap shorter than a tick still counts, for one tick.
--
-- 'tiInstructions' is the one field without a @!@, so it alone is lazy; for a
-- 'Bool' that makes no difference to what the game does.
data TickInput = TickInput
  { tiDir     :: !(Maybe Dir)  -- ^ most recently pressed direction still held
                               --   (W A S D or the arrows); if none is held, one
                               --   tapped since the last tick. 'Nothing': none at all
  , tiShift   :: !Bool  -- ^ the era key: Space, right Shift or keypad 0
  , tiEnter   :: !Bool  -- ^ Enter: start, retry, play on; it also moves the courier's
                        --   talk on, and held down it skips the opening film
  , tiRestart :: !Bool  -- ^ R: start the level again
  , tiPause   :: !Bool  -- ^ P: pause or resume
  , tiUndo    :: !Bool  -- ^ Z: take back the last accepted action, for part of its price
  , tiPeek    :: !Bool  -- ^ Tab: look at the other era. No rule reads it; drawing
                        --   reads it from 'gPrev', the tick's keys kept in the game
  , tiEcho    :: !Bool  -- ^ E: send the echo, in a level that has one
  ,tiInstructions :: Bool  -- ^ I: show or hide the instructions on the title screen
  , tiLobby   :: !Bool          -- ^ leave a level and return to level select
                                --   (Escape; from the hall it goes back to the
                                --   title, and it also skips the opening film)
  , tiDirRevealed :: !Bool     -- ^ no direction went down this tick, so if 'tiDir'
                               --   changed, a newer key was let go of, not a press
                               --   ('noInput' leaves it False, so an input written
                               --   by hand with only 'tiDir' set reads as a press)
    -- In a level for two, each keeper has their own keys: W A S D for the first,
    -- the arrows for the second. 'tiDir' above is either set, for one player.
    -- 'Game.Logic' copies each pair into 'tiDir' and 'tiDirRevealed' for that
    -- keeper's turn.
  , tiDir1    :: !(Maybe Dir)  -- ^ the W A S D keys alone
  , tiDir1Revealed :: !Bool  -- ^ as 'tiDirRevealed', for the W A S D keys alone
  , tiDir2    :: !(Maybe Dir)  -- ^ the arrow keys alone
  , tiDir2Revealed :: !Bool  -- ^ as 'tiDirRevealed', for the arrow keys alone
  } deriving (Eq, Show)

-- | No key down and nothing tapped: the input of an idle tick, and the 'gPrev' a new
-- game starts with. Hand-written inputs are made from it with record update syntax,
-- @noInput { tiDir = Just DirUp }@, which copies a record with the named fields
-- replaced and every other field kept.
--
-- It is written positionally: the arguments fill the fields in the order they are
-- declared above ('tiDir', then ten 'Bool's up to 'tiDirRevealed', then each
-- keeper's direction and flag), so a new field has to be added here in its place.
noInput :: TickInput
noInput = TickInput Nothing False False False False False False False False False False
                    Nothing False Nothing False

-- | The player does exactly one of these at a time. Being a sum type, "walking
-- while shifting" cannot be represented at all.
--
-- The two busy states carry a countdown in ticks. 'Game.Logic.advanceMotion' takes
-- one off each tick and ends the motion on the tick it finds 1 or less, so a motion
-- started at @n@ lasts @n@ ticks. A new action only starts while 'Settled'; a key
-- pressed meanwhile is remembered in the pending fields ('gPendingMove',
-- 'gPendingShift' and the others), except Space during a wave, which is dropped.
--
-- The field names are there as documentation. The code takes the values apart by
-- pattern matching (@Stepping to left@) rather than calling 'stepTo', which, like
-- any field of only some constructors, would crash on the others.
data Motion
  = Settled  -- ^ standing still: the only state in which an action can start
  | Stepping { stepTo :: !Cell, stepLeft :: !Int }
    -- ^ 'gCell' stays the origin until the step lands; the destination was
    -- validated before the step began and is the only other cell involved.
    -- 'stepLeft' counts down from 'stepTicks'. A jump through a mirror is a
    -- 'Stepping' too, whose 'stepTo' is the landing beyond the mirror, not a
    -- neighbour, and it takes the same 'stepTicks'.
  | Shifting { shiftLeft :: !Int }
    -- ^ The era has already changed. This only locks input during the wave.
    -- 'shiftLeft' counts down from 'shiftTicks'.
  deriving (Eq, Show)

-- | Why a note is on screen: a refused action explained, or advice. The rules in
-- 'Game.Logic' set it as 'gFeedback'; @app/Render.hs@ turns each kind into a line
-- under the map (or, for a refused shift or push, a plate on the board, with the
-- cell in the way outlined where there is one), and 'Game.Sound' plays a sound when
-- a new one appears. The refusals ('ShiftBlocked', 'ShiftTooCostly', 'PushBlocked')
-- last 'blockedFeedbackTicks', the pickup note 'pickupFeedbackTicks', and every
-- other kind, which is advice, 'hintFeedbackTicks'.
data FeedbackKind
  = ShiftBlocked !Cell  -- ^ this cell is a wall in the other era
                        --   (more exactly: nowhere to stand there, which can also be
                        --   a shut door or a mirror of that era; in a level for two
                        --   the cell may be the other keeper's, as both must move)
  | ShiftTooCostly      -- ^ the cell is open, but there is not enough time left
  | PushBlocked !Cell   -- ^ a crate would be inside a wall in the other era here
                        --   (crates are only pushed in the past, and a crate stands
                        --   in both eras, so this cell is a wall or a door, or holds
                        --   a mirror, in the present)
  | PushNeedsPress      -- ^ holding a key walks up to a crate; pushing takes a new press
                        --   (the same for a movable mirror's back or side)
  | CrateRusted         -- ^ a crate was pressed against in the present
  | CellCollected       -- ^ the power cell was just picked up
  | EchoNeedsSteps      -- ^ E with no footprints behind the player to walk back along
  | EchoInDoorway       -- ^ E while standing in a door: a new echo could shut it on you
  | JumpNeedsPress      -- ^ holding a key walks up a mirror's light; jumping takes a new press
  | MirrorFaceSide      -- ^ pressed into a mirror's face with nowhere to land: faces never push
  | MirrorFixed         -- ^ pressed against a mirror set in the wall, which never moves
  deriving (Eq, Show)

-- | One note on screen and how long it has left. There is only ever one: a new note
-- replaces the old one.
data Feedback = Feedback
  { fbKind :: !FeedbackKind  -- ^ what the note is about
  , fbLeft :: !Int  -- ^ ticks before it goes; counted down once each tick of play, and
                    --   drawn fading over its last 20 to 30 ticks
  } deriving (Eq, Show)

-- | What actions cost, in seconds of a level's budget. Plain data, so a level (or
-- later a run's relics) can change the prices without touching the rules.
--
-- An action is charged once, when it is accepted, and never per tick. When less
-- than 'ruStepCost' is left and nothing is waiting, the level is lost. The ending
-- after level 3 sets every price to 0 ('Game.Run.revealedLevel') so the keeper
-- walks there for free.
data Rules = Rules
  { ruStepCost          :: !Int  -- ^ one step, and also one push of a crate or of a
                                 --   movable mirror
  , ruShiftCost         :: !Int  -- ^ one change of era
  , ruUndoRefundPercent :: !Int  -- ^ share of an undone action's cost given back
                                 --   (a percentage, rounded down by 'div')
  , ruEchoCost          :: !Int  -- ^ sending the echo
  , ruEchoLength        :: !Int  -- ^ footprints kept, counting the player's own spot;
                                 --   0 means this level has no echo at all
  , ruJumpCost          :: !Int  -- ^ stepping through a mirror, however far it carries you
  } deriving (Eq, Show)

-- | A switch is dearer than a step, so where and how often to switch is a real
-- choice. Even prices keep a half refund in whole seconds.
--
-- Every campaign level plays by these: a step 2, a shift 6, a jump 4 (and an echo
-- 4, where there is one), with half of a price given back by an undo. It is written
-- with record construction syntax, naming each field, so their order does not
-- matter. 'Game.Level.parseLevel' gives every level these rules, and
-- 'Game.Level.room' then sets the ones a level is written with.
baseRules :: Rules
baseRules = Rules { ruStepCost = 2, ruShiftCost = 6, ruUndoRefundPercent = 50
                  , ruEchoCost = 4, ruEchoLength = 0, ruJumpCost = 4 }

-- | The levels that teach the echo: five footprints, the player's own included.
--
-- @baseRules { ruEchoLength = 5 }@ is record update syntax: a copy of 'baseRules'
-- with that one field changed. Only the old echo levels, kept in
-- 'Game.Level.extraLevels', use it.
echoRules :: Rules
echoRules = baseRules { ruEchoLength = 5 }

-- | A mirror stands in one era and throws light out of its face. A fixed one is set
-- in the wall; a movable one stands on floor and can be pushed from its back or sides.
--
-- Stand in its light and press towards it, and if there is room you step through:
-- as far behind it as you stood in front, or for a corner mirror as far out of its
-- other face (see 'exitFace' and 'Game.Logic.mirrorAhead'). Mirrors live in a 'Map'
-- from their cell, first 'lvMirrors' and then, as they are pushed about,
-- 'gMirrors'. 'Ord' is derived because mirrors are part of the solver's sorted
-- state keys.
data Mirror = Mirror
  { mEra     :: !Era  -- ^ the era it stands and works in: the map it was drawn on.
                      --   It is solid in that era; in the other its cell is as the
                      --   map has it
  , mFace    :: !Dir  -- ^ the way its face looks; its light goes out that way
  , mMovable :: !Bool  -- ^ True for one on a sled (@n s e w@ on the map), which can
                       --   be pushed; False for one set in the wall, which never moves
  , mBend    :: !(Maybe Dir)  -- ^ a corner mirror's second face, at right angles to
                              --   the first; it turns the light, and whoever steps through
  } deriving (Eq, Ord, Show)

-- | Every way a mirror throws light: one face, or two for a corner mirror.
--
-- @maybe [] pure (mBend m)@ turns the optional second face into a list: 'maybe'
-- takes a value to give for 'Nothing' and a function to apply inside 'Just', and
-- 'pure' for lists makes a one-element list. So the result is @[face]@ or
-- @[face, bend]@, always with the main face first.
mirrorFaces :: Mirror -> [Dir]
mirrorFaces m = mFace m : maybe [] pure (mBend m)

-- | Stepping into a mirror while moving @dir@: the way you come out. Straight on,
-- behind it, for a flat mirror; out of the other face for a corner one.
--
-- It only means something when @dir@ points into one of the mirror's faces, that
-- is, when the keeper stands in that face's light and moves towards the mirror,
-- which is the only case 'Game.Logic.mirrorAhead' asks about. Example: an @L@
-- corner looks up and right (face up, bend right); standing north of it and
-- pressing down, you come out to the east.
exitFace :: Mirror -> Dir -> Dir
exitFace m dir = case mBend m of
  -- A flat mirror: carry on the same way, and come out behind it.
  Nothing -> dir
  -- A corner mirror. A case branch can have guards, like a function equation.
  Just b
    -- Moving against the bend face means you came in through the bend, so you
    -- leave by the main face.
    | b == reverseDir dir -> mFace m
    -- Otherwise you came in through the main face, and leave by the bend.
    | otherwise -> b
  where
    -- The same as 'Game.Logic.opposite', repeated here because this module cannot
    -- import 'Game.Logic', which imports this one.
    reverseDir d = case d of
      DirUp -> DirDown
      DirDown -> DirUp
      DirLeft -> DirRight
      DirRight -> DirLeft

-- | Both eras of the one location. Anything not in a walkable set is solid, so
-- out-of-bounds cells need no special case.
--
-- A 'Level' is what the maps say at the start, and it does not change in play;
-- everything that moves (crates, mirrors, filled pits, the keepers) is copied into
-- the 'Game' and moves there. (The ending after level 3 plays on an opened copy,
-- 'Game.Run.revealedLevel'.) 'Game.Level.parseLevel' builds and checks one from two
-- lists of strings, one map per era; the README lists the map characters.
data Level = Level
  { lvWidth    :: !Int  -- ^ columns, the same on both maps
  , lvHeight   :: !Int  -- ^ rows, the same on both maps
  , lvPast     :: !(Set Cell)  -- ^ the past map's open cells: all but walls, pits and
                               --   mirrors set in the wall. Markers (the power cell, a
                               --   crate, a plate, a movable mirror) are floor here
  , lvPresent  :: !(Set Cell)  -- ^ the same for the present map, where doors are left
                               --   out: a door is a wall until its plates open it
  , lvSpawn    :: !Cell  -- ^ where the keeper starts, in the present (@S@)
  , lvPowerCell :: !Cell  -- ^ the power cell's cradle, in the past (@B@)
  , lvGoal     :: !Cell  -- ^ the lighthouse, in the present (@G@)
  , lvCrates   :: !(Set Cell)  -- ^ where the crates start (@C@); each stands in both eras
  , lvPits     :: !(Set Cell)  -- ^ pits in the past (@~@): solid until a crate is pushed
                               --   in, and light crosses them
  , lvPlates   :: !(Set Cell)   -- ^ set in the floor of both eras (@P@ on the past
                                --   map): something resting on all of them (a crate, a
                                --   keeper or the echo, in either era) opens...
  , lvDoors    :: !(Set Cell)   -- ^ ...these, in the present; solid walls otherwise
  , lvMirrors  :: !(Map Cell Mirror)
    -- ^ where the mirrors start; a mirror is solid in its own era
  , lvBudget   :: !Int          -- ^ seconds available in this level
                                --   (the par plus the level's slack)
  , lvRules    :: !Rules  -- ^ the prices, and whether the level has an echo
  , lvName     :: !String  -- ^ shown in the hall and as the level is introduced
  , lvLesson   :: !String       -- ^ the mechanic this level introduces
                                --   (on its card in the hall, and under the map for
                                --   its first seconds; empty for none)
  , lvPar      :: !Int          -- ^ the cheapest possible cost; a test re-derives it
                                --   (in seconds of the budget)
  , lvDressed  :: !Bool         -- ^ the original station, with its hand-placed dressing
  , lvSpawn2   :: !(Maybe Cell) -- ^ where a second keeper starts, in a level for two
                                --   (@T@ on the present map)
  , lvPassage  :: ![Cell]       -- ^ walls that open once the level is won, from the
                                --   lighthouse outward: the way on to a hidden level
  , lvSecret   :: !Bool         -- ^ never listed in the level hall; only found
                                --   (level 4, reached from the end of level 3)
  } deriving (Eq, Show)

-- | The second keeper, in a level for two. The fields are the keeper's own half of
-- 'Game'; the era, the crates, the mirrors and the time are shared.
--
-- Each field means what the 'Game' field of the same name means for the first
-- keeper. 'Game.Logic.swapKeepers' trades the two halves, so every rule, written
-- for "the player", can be run for the second keeper unchanged.
data Partner = Partner
  { ptCell        :: !Cell  -- ^ as 'gCell'
  , ptHasCell     :: !Bool  -- ^ as 'gHasCell': this keeper carries the power cell
  , ptMotion      :: !Motion  -- ^ as 'gMotion'
  , ptFacing      :: !Dir  -- ^ as 'gFacing'
  , ptPushing     :: !(Maybe Cell)  -- ^ as 'gPushing'
  , ptPendingMove :: !(Maybe Dir)  -- ^ as 'gPendingMove'
  , ptArmed       :: !Bool  -- ^ as 'gArmed'
  , ptBump        :: !Int  -- ^ as 'gBump'
  } deriving (Eq, Show)

-- | A place in time: which era, and which cell of it.
--
-- Footprints ('gTrail') and the echo ('gEcho') are spots rather than cells, because
-- a shift leaves a footprint in the other era on the same cell.
type Spot = (Era, Cell)

-- | The board as it was before one accepted action, and what that action cost.
--
-- 'Game.Logic.charge' makes one each time an action is paid for and pushes it onto
-- 'gHistory'; 'Game.Logic.undo' pops one and copies its fields back. It holds what
-- an action can change and an undo must put back exactly. The time left is not
-- stored: undo adds part of 'snCost' to the time left now, so undoing never beats
-- playing well. Nor are the phase, motion, waiting keys and notes: an undo always
-- comes back settled, with nothing waiting.
--
-- Keeping whole sets and maps per action is cheap: 'Set' and 'Map' values are
-- immutable, so a snapshot shares nearly all of its structure with the live board,
-- and only the parts an action rebuilt are new.
data Snapshot = Snapshot
  { snEra     :: !Era  -- ^ 'gEra' before the action
  , snCell    :: !Cell  -- ^ 'gCell' before the action: where a step set off from
  , snHasCell :: !Bool  -- ^ 'gHasCell' before the action
  , snCrates  :: !(Set Cell)  -- ^ 'gCrates' before the action
  , snFilled  :: !(Set Cell)  -- ^ 'gFilled' before the action
  , snFacing  :: !Dir  -- ^ 'gFacing' before the action
  , snTrail   :: ![Spot]  -- ^ 'gTrail' before the action
  , snEcho    :: !(Maybe Spot)  -- ^ 'gEcho' before the action
  , snMirrors :: !(Map Cell Mirror)  -- ^ 'gMirrors' before the action
  , snCost    :: !Int  -- ^ seconds the action cost; undo gives back
                       --   'ruUndoRefundPercent' of it
  , snPartner :: !(Maybe Partner)  -- ^ the second keeper, where they stood
  } deriving (Eq, Show)

-- | Everything about one level in play: the board, the keeper (and in a level for
-- two the second keeper, 'gPartner'), the time left, the undo history and the keys
-- of the tick before. 'Game.Logic.initialGame' builds one and 'Game.Logic.stepGame'
-- makes the next one from it every tick; 'Game.Run' keeps the current one as
-- 'Game.Run.runGame'. Effects, sound and drawing only read it.
--
-- The fields described as only for drawing, or read by no rule, are left out of the
-- solver's state key, so two games that differ only there count as one state.
data Game = Game
  { gPhase        :: !Phase  -- ^ see 'Phase'
  , gEra          :: !Era  -- ^ the era both keepers are in
  , gCell         :: !Cell  -- ^ the keeper's cell (the first keeper's, in a level for
                            --   two); during a step, still the cell it set off from
  , gHasCell      :: !Bool  -- ^ the keeper carries the power cell
  , gRemaining    :: !Int          -- ^ seconds of budget left; only actions spend it
  , gCrates       :: !(Set Cell)  -- ^ where every crate stands now, the same cell in
                                  --   both eras
  , gFilled       :: !(Set Cell)   -- ^ pits a crate has been pushed into
                                   --   (each is past floor from then on)
  , gMirrors      :: !(Map Cell Mirror) -- ^ where every mirror stands now
  , gTrail        :: ![Spot]       -- ^ where the player has stood, newest first, their
                                   --   own spot included; an echo walks it back
                                   --   (at most 'ruEchoLength' long, so empty in a
                                   --   level without the echo)
  , gEcho         :: !(Maybe Spot) -- ^ where the echo stands, holding down any plate
  , gHistory      :: ![Snapshot]   -- ^ accepted actions, newest first, for undo
  , gPushing      :: !(Maybe Cell) -- ^ where this step's crate or mirror is going; only for drawing
  , gAttempt      :: !Int          -- ^ counts starts and restarts
                                   --   ('Game.Run' also counts each new level, hall
                                   --   and title it sets up); 'Game.Fx' and
                                   --   'Game.Sound' spot a fresh board by its change
  , gMotion       :: !Motion  -- ^ settled, stepping or shifting; see 'Motion'
  , gFacing       :: !Dir          -- ^ which way the keeper looks; no rule reads it
  , gPendingShift :: !Bool         -- ^ at most one shift waits for a step to land
  , gPendingMove  :: !(Maybe Dir)  -- ^ the last tap made while busy
                                   --   (a newer tap replaces it; it is taken as soon
                                   --   as the keeper is settled)
  , gPendingUndo  :: !Bool         -- ^ an undo waits for the current action to end
  , gPendingEcho  :: !Bool         -- ^ an echo waits for the current action to end
  , gFeedback     :: !(Maybe Feedback)  -- ^ the note on screen, if any
  , gBump         :: !Int          -- ^ ticks of "walked into a wall" nudge left
                                   --   (set to 'bumpTicks' by a fresh press into
                                   --   something solid; only for drawing and sound)
  , gPrev         :: !TickInput    -- ^ last tick's keys, for finding fresh presses
                                   --   (drawing also reads Tab from it)
  , gArmed        :: !Bool         -- ^ the held direction was freshly pressed in this
                                   --   screen, so it may repeat
                                   --   (cleared when no direction is held, and by a
                                   --   pause, an undo, a restart, a win or a loss)
  , gShowInstructions :: Bool  -- ^ the instructions are open on the title (I key);
                               --   the one lazy field, as it has no @!@
  , gPartner      :: !(Maybe Partner) -- ^ the second keeper, in a level for two
  } deriving (Eq, Show)

-- | The fixed rate of the rules: one tick is 1/60 of a second of game time.
-- @app/Main.hs@ runs as many ticks as real time calls for (at most six a frame, so a
-- long stall is dropped rather than replayed), and every duration in the code is
-- counted in ticks of this length.
ticksPerSecond :: Int
ticksPerSecond = 60

-- | One cell of walking, which is also the held-key repeat interval.
--
-- 8 ticks, about 0.13 s. Holding a key repeats at this rate because the next step
-- starts on the tick the last one lands. A jump through a mirror takes the same
-- time, however far it goes.
stepTicks :: Int
stepTicks = 8

-- | About 0.22 s: the input lock while the era wave crosses the map.
-- The wave of flipping tiles ('Game.Fx.waveProgress') lasts exactly as long.
shiftTicks :: Int
shiftTicks = 13

-- | How long notes and the bump last, in ticks. One type signature can name several
-- values at once, as here.
--
--   * 'hintFeedbackTicks' (2 s): advice, such as that a push takes a fresh press.
--   * 'blockedFeedbackTicks' (1.6 s): a refused shift or push. Drawing shakes the
--     keeper during its first 18 ticks.
--   * 'pickupFeedbackTicks' (3.5 s): the note that the power cell is being carried.
--   * 'bumpTicks' (about 0.13 s): the nudge after walking into something solid.
blockedFeedbackTicks, pickupFeedbackTicks, hintFeedbackTicks, bumpTicks :: Int
hintFeedbackTicks    = 120
blockedFeedbackTicks = 96
pickupFeedbackTicks  = 210
bumpTicks            = 8
