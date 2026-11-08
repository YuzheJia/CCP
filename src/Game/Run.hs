-- | Campaign navigation around the pure rules for one level, and the one ending
-- that is not a level of its own: the lamp that goes out again.
--
-- 'Game.Logic' knows how one level plays: 'stepGame' turns one 'Game' into the next.
-- This module knows everything around that: which screen the player is on (the
-- title, the opening film, the level hall, a level, or an ending), which level is
-- selected, how many levels are cleared and unlocked, and how to move between them.
-- Its main entry point is 'stepRun', one tick at a time. The app's Main calls it
-- through Game.Orders.stepCampaign, which handles the orders desk and intercepts
-- transitions to and from it. Main steps once a tick, 60 times a second, with the
-- keys held down that tick, keeps the 'Run'
-- it returns, and hands the run before and after (or their boards) to 'Game.Fx',
-- 'Game.Sound' and 'Game.Story', which work out the effects, the sounds and the
-- courier's talk by comparing the two. Each saved order keeps 'runCompleted' and
-- 'runIndex', plus its time, era-shift count and timestamp; a half-played board is
-- never saved.
--
-- It also holds the ending of the last listed level (level 3, The Long Way Round):
-- the stages 'Lit', 'Flare', 'Opening', 'Beckon', 'Rush' and 'Arrival'; the straight
-- way the light breaks open ('finaleRun', 'wayCells'); the map the rush is drawn on
-- ('dashLevel'); and the rush itself ('Dash', 'stepDash'). These live here, apart
-- from the rules, so the solver, the pars and the level tests never see them. A
-- level gets an ending from a passage ('lvPassage', set by @withPassage@ in
-- 'Game.Level') and a level after it, the secret one.
--
-- What it does not do: draw anything (the app's Render draws the ending from
-- 'Finale' and 'Dash', and its TalkBox draws the film's skip bar), choose sounds
-- ('Game.Sound' does, comparing two runs), or know how long the opening film is
-- (the app ends it with 'finishIntro'). Like the rest of the core it has no Gloss
-- and no IO, so the tests drive it directly with made-up keys.
--
-- Units used throughout. Time is counted in ticks, 60 to a second
-- ('ticksPerSecond'): every @...Ticks@ value and every age below is in ticks. A
-- 'Cell' is (column, row), with (0, 0) the top-left cell and y growing down the map.
-- The rush measures distance along its straight way in cells as a 'Float', so the
-- keeper can stand part of the way between two cells, and speed in cells per tick
-- (0.1 cells a tick is 6 cells a second). Level numbers in the code are list
-- indices from 0: level 1 on screen is index 0.
module Game.Run
  ( RunMode (..)
  , Run (..)
  , startRun
  , startRunWithProgress
  , startRunAt
  , stepRun
  , lastVisible
    -- * The opening
  , introSkipTicks
  , finishIntro
    -- * The way on
  , Stage (..)
  , Finale (..)
  , finale
  , litTicks, flareTicks, openTicks, arrivalTicks
  , lightSetsOff, lightCellTicks
  , lampDiesAt, revealTicks, cellOpenTick
  , hasEnding
  , revealedLevel
  , partlyRevealed
  , FinaleRun (..)
  , wayCells
  , rushLength
  , finaleRun
  , corridorLength
  , dashLevel
  , Dash (..)
  , restingDash
  , stepDash
  , barriers
  , barriersFrom
  , edgeFrom
  , inBarrier
  , outFrom
  , screenSpeed
  , dashTopSpeed
  , catchTicks
  , caughtGap
  , rushInputs
  , beckonKeys
  ) where

-- Imported qualified: its functions are called as Set.union, Set.fromList and so on,
-- so they cannot clash with the Prelude's list functions of the same names.
import qualified Data.Set as Set
-- The size of one room (13 by 9 cells) and which room a cell is in, for cutting the
-- station down to the lighthouse's room in 'dashLevel'.
import Game.Fx (roomCols, roomRows, roomOf)
-- 'initialGame' and 'stepGame', the rules of one level.
import Game.Logic
import Game.Types

-- | Where the run is: the title, orders, opening film, level hall, a level, or a
-- level's ending. The opening counts ticks since it began, and how long Enter (or
-- Space) has been held down to skip it.
--
-- So @Intro age held@: @age@ is ticks since the film began (the app's Main ends the
-- film when it reaches the film's length), @held@ is ticks of the current hold
-- towards 'introSkipTicks'. 'Lobby' is the level hall. 'InLevel' is a level being
-- played, including its won and lost plates. @Ending f@ is a level's ending, with
-- 'Finale' saying how far it has got.
--
-- The @!@ before a field's type makes it strict: the value is worked out when the
-- constructor is built instead of being kept as an unevaluated expression (a
-- thunk). State that is rebuilt every tick for minutes would otherwise pile up a
-- chain of such thunks. 'Eq' is derived so screens can be compared ('Game.Story'
-- and the tests do), and 'Show' for the @--trace@ output.
data RunMode = Title | Orders | Intro !Int !Int | Lobby | InLevel | Ending !Finale
-- ^ Title is the menu; Orders selects a save; Intro holds age and skip ticks;
-- Lobby selects a level; InLevel plays it; Ending holds its stage and rush.
  deriving (Eq, Ord, Show)

-- | @runCompleted@ is the number of campaign levels cleared. @runUnlocked@ is
-- the highest level index the player may choose. The IO boundary persists the
-- completion count and selection; puzzle board state stays session-local.
--
-- One 'Run' is the whole state of the campaign that the rules care about; the app
-- keeps it in its world, beside the effects, the story and the sound.
data Run = Run
  { runIndex    :: !Int   -- ^ the level being played, or selected in the hall or on
                          --   the title, as an index into the list of levels
  , runUnlocked :: !Int   -- ^ the highest index the hall lets the player choose; it
                          --   never reaches a secret level
  , runCompleted :: !Int  -- ^ how many levels have been cleared: indices below this
                          --   are cleared, and talks teach only from this index on
  , runMode     :: !RunMode  -- ^ which screen is showing
  , runGame     :: !Game  -- ^ the board of level 'runIndex': being played in a level
                          --   or an ending, fresh and unplayed on the other screens,
                          --   where it still keeps the last tick's keys in 'gPrev'
  } deriving (Eq, Show)

-- | The last level the hall lists. Secret levels come after all the others and are
-- never listed, never selected and never offered, however far the player has got.
--
-- Returns a list index from 0: with the three listed levels and the secret fourth,
-- it is 2. @takeWhile (not . lvSecret)@ keeps the levels up to the first secret one;
-- @not . lvSecret@ is function composition, the function that applies 'lvSecret'
-- and then 'not', written without naming its argument (point-free style). The
-- @max 0@ keeps the result from going negative for an empty list, or one that
-- starts with a secret level. Used for the unlock frontier here, for the hall's
-- count of levels in the app, and by the tests.
lastVisible :: [Level] -> Int
lastVisible levels = max 0 (length (takeWhile (not . lvSecret) levels) - 1)

-- | A new campaign starts at the title with only its first level unlocked.
--
-- @selected@ is the index the hall should start on; with nothing cleared only index
-- 0 is unlocked, so that is where it ends up. Used by the tests; the app starts
-- through 'startRunWithProgress' or 'startRunAt' with the saved progress.
startRun :: [Level] -> Int -> Run
startRun levels selected = startRunWithProgress levels selected 0

-- | Restore a validated lobby selection and unlock frontier.
--
-- Arguments: the hall's non-empty list of levels (secret ones last), the index
-- the player had
-- selected, and how many levels they have cleared, both from the save file (or
-- @--level=N@). The run opens on the 'Title' screen holding a fresh board of the
-- selected level, not yet being played ('initialGame' leaves its phase at 'Menu').
--
-- Nothing from the save is trusted: the cleared count is clamped into the range 0
-- to the number of levels, the unlock frontier is that count but never past the
-- last listed level, and the selection is clamped to what is unlocked. So a save
-- that names a secret level, or a level that no longer exists, comes back as the
-- last level the hall lists.
startRunWithProgress :: [Level] -> Int -> Int -> Run
startRunWithProgress levels selected completed =
  -- The constructor applied to its fields in order, without their names:
  -- runIndex, runUnlocked, runCompleted, runMode, runGame. xs !! n is element n of
  -- a list counting from 0; it walks the list, and crashes if n is out of range,
  -- so the selection is clamped first. This still requires a non-empty hall;
  -- the shipped levelHall supplies one.
  Run i u c Title (initialGame (levels !! i))
  where
    -- Levels cleared: never negative, never more than there are levels.
    c = clamp 0 (length levels) completed
    -- The highest index that may be chosen: index c, the first level not yet
    -- cleared (clearing level 1 unlocks level 2), but never a level the hall does
    -- not list.
    u = min (lastVisible levels) c
    -- The selection, kept inside what may be chosen.
    i = clamp 0 u selected

-- | Like 'startRunWithProgress', but a secret level asked for by number is opened
-- straight away, since the hall would never offer it.
--
-- The app uses it for @--level=N@ (index N - 1). If index @i@ is a secret level, the
-- run goes straight into it, already 'Playing', with the cleared count and unlock
-- frontier worked out as 'startRunWithProgress' would, so the hall reached from it
-- with Escape still lists only what is unlocked. Any other index, valid or not, is
-- treated as a saved selection by 'startRunWithProgress'.
startRunAt :: [Level] -> Int -> Int -> Run
startRunAt levels i completed
  -- '&&' only looks at its right side when its left side is True, so the bounds
  -- are checked before '!!' can be reached with a bad index.
  | i >= 0 && i < length levels && lvSecret (levels !! i) =
      -- Record update syntax: r { field = value } is a copy of the record r with
      -- only the named fields changed; the original is untouched. Here the fresh
      -- board is put straight into play, as its first attempt.
      Run i (min (lastVisible levels) c) c InLevel
          ((initialGame (levels !! i)) { gPhase = Playing, gAttempt = 1 })
  | otherwise = startRunWithProgress levels i completed
  where c = clamp 0 (length levels) completed

-- | One fixed tick. Enter moves from the title into the lobby, where Escape goes
-- back to the title; a campaign with nothing cleared yet plays the opening film
-- instead, and goes from it straight into the first level. Escape from a level
-- returns to the lobby. Winning unlocks the next room immediately, and Enter
-- continues there. A level with a passage does not wait for Enter: its ending runs,
-- and leads into the secret level after it.
--
-- Arguments: the hall's levels, the keys held down this tick (one 'TickInput', made
-- by 'Game.Input' from the keyboard), and the run as it stood. Every branch finds
-- fresh presses by comparing @input@ with the keys of the tick before, which the
-- board keeps in 'gPrev'. So every new board made here records @input@ as its
-- 'gPrev', or the key that made the move would count as a fresh press again on the
-- next tick, on the new screen.
--
-- The @case@ picks one local function per screen. They are defined in the @where@
-- block below, where they can see the arguments and the shared bindings above them.
-- A @where@ binding is only worked out if it is used (Haskell is lazy), so the many
-- bindings cost next to nothing on the screens that do not need them.
stepRun :: [Level] -> TickInput -> Run -> Run
stepRun levels input run = case runMode run of
  Title    -> stepTitle
  -- The orders screen is stepped by "Game.Orders", which knows the saves.
  Orders   -> run { runGame = g { gPrev = input } }
  Intro age held -> stepIntro age held
  Lobby    -> stepLobby
  InLevel  -> stepLevel
  Ending f -> stepEnding f
  where
    -- Short names for the parts of the run, used by every screen.
    i = runIndex run
    u = runUnlocked run
    c = runCompleted run
    g = runGame run
    -- The keys of the tick before.
    prev = gPrev g
    -- The level the hall shows when we go back to it. The hall never shows a
    -- secret level, so leaving one lands on the last listed level.
    shown = min i (lastVisible levels)
    -- A fresh press: down now, and up on the tick before. A key held down never
    -- presses again, however long it is held.
    freshEnter = tiEnter input && not (tiEnter prev)
    freshLobby = tiLobby input && not (tiLobby prev)
    -- A direction pressed this tick, for moving the hall's selection. 'tiDir' is
    -- the newest direction still held; if it changed only because a newer key was
    -- let go of and an older one still held shows through, 'tiDirRevealed' says so,
    -- and that is not a press.
    freshDir
      | tiDir input /= tiDir prev && not (tiDirRevealed input) = tiDir input
      | otherwise = Nothing
    -- The hall's selection after this tick: up or left goes to the level before,
    -- down or right to the one after, never below 0 or past the unlocked frontier.
    -- With nothing pressed it stays.
    selected = case freshDir of
      Just DirUp    -> max 0 (i - 1)
      Just DirLeft  -> max 0 (i - 1)
      Just DirDown  -> min u (i + 1)
      Just DirRight -> min u (i + 1)
      Nothing       -> i
    -- The board the hall holds for the selected level. A change of selection is a
    -- new attempt, which tells 'Game.Fx' that a different board has begun, so it
    -- starts its decorative state afresh. The 'if' is an ordinary expression giving
    -- 1 or 0, added to the count.
    selectedGame = (initialGame (levels !! selected))
      { gAttempt = gAttempt g + if selected /= i then 1 else 0
      , gPrev = input
      }
    -- A level entered to be played: a fresh board, already 'Playing' rather than
    -- waiting on its 'Menu' plate, as a new attempt.
    begin level = (initialGame level)
      { gPhase = Playing, gAttempt = gAttempt g + 1, gPrev = input }
    -- Back to the hall, or to the title, on the shown level, with its board fresh.
    -- Whatever was half played is dropped: an unfinished puzzle starts fresh.
    toLobby = Run shown u c Lobby ((initialGame (levels !! shown))
      { gAttempt = gAttempt g + 1, gPrev = input })
    toTitle = Run shown u c Title ((initialGame (levels !! shown))
      { gAttempt = gAttempt g + 1, gPrev = input })

    -- I opens and shuts the instructions; while they are open, Enter does nothing.
    -- The instructions are a flag on the board, 'gShowInstructions'; the app's
    -- Render draws them over the title screen while it is set.
    stepTitle
      |tiInstructions input && not (tiInstructions prev)=
        run {runGame = g
        {gShowInstructions = not (gShowInstructions g)
        , gPrev = input
        }}
      |gShowInstructions g = run {runGame = g {gPrev = input}}
      -- Enter with nothing cleared yet plays the opening film, from age 0 with
      -- nothing held, on level 1's board. When there is no film the app ends the
      -- opening at once, in the same tick, so this still leads straight into
      -- level 1.
      | freshEnter && c == 0 = Run 0 u c (Intro 0 0) ((initialGame (levels !! 0))
          { gAttempt = gAttempt g + 1, gPrev = input })
      -- With a level cleared, Enter opens the hall instead.
      | freshEnter = toLobby
      -- Any other tick changes nothing but the remembered keys.
      | otherwise  = run { runGame = g { gPrev = input } }

    -- Escape skips the film at once; Enter or Space skips it once held down for
    -- 'introSkipTicks', so a tap meant for something else cannot. Only a hold that
    -- starts during the film counts, not the Enter still down from the title.
    -- Skipping, or a hold reaching its length, starts level 1 straight away; any
    -- other tick ages the film by one and keeps the hold's new count.
    stepIntro age held
      | freshLobby || held' >= introSkipTicks = Run 0 u c InLevel (begin (levels !! 0))
      | otherwise = run { runMode = Intro (age + 1) held', runGame = g { gPrev = input } }
      where
        freshShift = tiShift input && not (tiShift prev)
        -- Either skip key down this tick.
        holding = tiEnter input || tiShift input
        -- The hold's count after this tick. It goes up only while a hold that began
        -- with a fresh press during the film goes on (a count above 0 means one has
        -- begun); letting go puts it back to 0, so taps never add up.
        held'
          | holding && (held > 0 || freshEnter || freshShift) = held + 1
          | otherwise = 0

    -- The hall. Escape goes back to the title, Enter plays the selected level, and
    -- anything else moves the selection (or keeps it) with a fresh board to show.
    stepLobby
      | freshLobby = toTitle
      | freshEnter = Run i u c InLevel (begin (levels !! i))
      | otherwise  = Run selected u c Lobby selectedGame

    -- A level. Escape always leaves for the hall.
    stepLevel
      | freshLobby = toLobby
      -- On the won plate, Enter goes on to the next level if it is listed in the
      -- hall; after the last listed level it goes back to the hall instead. The
      -- level just won is index i, so at least i + 1 levels are cleared, and the
      -- next is unlocked; 'max' keeps any further progress from an earlier visit.
      -- let ... in names a value inside one expression, where a where block names
      -- it for a whole definition.
      | gPhase g == Won && freshEnter =
          if i + 1 < length levels && not (lvSecret (levels !! (i + 1)))
            then let next = i + 1
                 in Run next (max u next) (max c (i + 1)) InLevel (begin (levels !! next))
            else toLobby
      -- Anything else is a tick of the rules ('stepGame'), which also handles every
      -- move, P (pause), R on the won plate, and Enter, R or Z on the lost one.
      | otherwise =
          -- g' is the board after this tick. wonNow holds only on the tick the
          -- lamp is lit, so an ending starts once. A win counts as cleared at once,
          -- not when Enter is pressed, so Escape from the won plate keeps it, and
          -- the app saves it that frame. A level with an ending starts it on the
          -- winning tick, at its first stage, instead of waiting on the plate.
          let g' = stepGame (levels !! i) input g
              wonNow = gPhase g' == Won && gPhase g /= Won
              completed' = if gPhase g' == Won then max c (i + 1) else c
              unlocked' = min (lastVisible levels) completed'
              mode = if wonNow && hasEnding levels i then Ending (finale Lit 0) else InLevel
          in Run i unlocked' completed' mode g'

    -- A level's ending. Escape leaves for the hall, the level already counted as
    -- cleared. Otherwise the ending plays a tick; once 'advanceFinale' says it is
    -- over ('Nothing'), the level after it begins, the secret one. The unlocked and
    -- cleared counts stay as they are: the hall never lists that level.
    stepEnding f
      | freshLobby = toLobby
      | otherwise = case advanceFinale (levels !! i) input f g of
          Just (f', g') -> Run i u c (Ending f') g'
          Nothing -> let next = i + 1 in Run next u c InLevel (begin (levels !! next))

-- | @clamp lo hi x@ is @x@ kept between @lo@ and @hi@, both included. It is written
-- point-free: @max lo . min hi@ is the function that applies @min hi@ first and then
-- @max lo@ (the dot composes functions, read right to left), so the third argument
-- never needs a name. Assumes @lo <= hi@.
clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- The opening ---------------------------------------------------------------------
--
-- The film itself belongs to the app, which knows how long it is; the run only
-- counts its ticks and takes the keys that skip it.

-- | How long Enter or Space must be held to skip the opening film: 40 ticks, two
-- thirds of a second. The app's TalkBox draws the hold's progress as a share of it.
introSkipTicks :: Int
introSkipTicks = 40

-- | The opening film has ended: the first level begins. Keys held as it ends need a
-- fresh press. Anything other than the opening is left as it is.
--
-- The app's Main calls it on the tick the film's age reaches its length (at once if
-- there is no film), after 'stepRun'. Level index 0 starts 'Playing', as a new
-- attempt. 'gPrev' carries over from the board the film held, so a key still down
-- from the last tick of the film is not a fresh press in the level.
finishIntro :: [Level] -> Run -> Run
finishIntro levels run = case runMode run of
  Intro _ _ ->
    let g = runGame run
    in run { runIndex = 0, runMode = InLevel
           , runGame = (initialGame (levels !! 0))
               { gPhase = Playing, gAttempt = gAttempt g + 1, gPrev = gPrev g } }
  _ -> run

-- The way on --------------------------------------------------------------------
--
-- The last lamp of the hall is lit, the plate says every lamp is lit, and then the
-- light is taken back: it flares and goes out, slips into the wall beside the tower,
-- and runs off across the station, breaking every wall in its way. The keeper
-- follows on foot, is swept along the way it opened faster and faster, out of the
-- station and down a long corridor, and arrives at the real lighthouse, which is
-- where the secret level begins.
--
-- In ticks: 'Lit' lasts 300, 'Flare' 80, 'Opening' as long as the light takes to
-- cross the station (158 in level 3), 'Beckon' and 'Rush' as long as the player
-- takes, and 'Arrival' 230. The rules are not used between the win and 'Beckon':
-- the board stands still, and only remembers the keys. The app's Render draws every
-- stage from the 'Finale' and the board, and 'Game.Sound' times the ending's sounds
-- against the same constants, so a change here moves picture and sound together.
--
-- In level 3 the lighthouse is at (11, 4). The passage starts in the wall above it,
-- at (11, 3), and runs west along row 3 to the outer wall at (0, 3), so the way is
-- the twelve cells (11, 3) to (0, 3).

-- | The parts of the ending, in order. The derived 'Ord' and 'Enum' follow the order
-- the constructors are written in, so @stage >= Opening@ asks whether the ending has
-- got that far (the app's Render asks this).
data Stage
  = Lit      -- ^ the lamp burns, and every lamp seems to be lit; the board is still
             --   'Won' ('litTicks')
  | Flare    -- ^ it flares white and goes out ('flareTicks'; dark at 'lampDiesAt')
  | Opening  -- ^ its light runs off through the walls, breaking them open
             --   ('openTicks')
  | Beckon   -- ^ the keeper walks again, free; the way waits
  | Rush     -- ^ on the way: carried along it, faster and faster
  | Arrival  -- ^ the real lighthouse, then the level beyond it ('arrivalTicks')
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | How far the ending has got: its stage, the ticks spent in that stage, and the
-- rush. It lives in the run as @Ending f@. It derives 'Ord' because 'RunMode',
-- which holds it, does.
data Finale = Finale
  { fnStage :: !Stage  -- ^ which part of the ending is playing
  , fnAge   :: !Int   -- ^ ticks into the stage
  , fnDash  :: !Dash  -- ^ the rush, once it has begun; 'restingDash' before it,
                      --   and its last state through 'Arrival'
  } deriving (Eq, Ord, Show)

-- | A stage at @age@ ticks into it, with the rush at rest. @finale Lit 0@ is
-- how every ending begins ('stepRun', on the winning tick).
finale :: Stage -> Int -> Finale
finale stage age = Finale stage age restingDash

-- | How long each stage with a fixed length lasts, in ticks: the lamp burns for 300
-- (5 seconds, long enough for the card that says it is all over to be believed),
-- the flare lasts 80 (about 1.3 seconds), and the arrival at the
-- real lighthouse 230 (about 3.8 seconds) before the level beyond begins.
litTicks, flareTicks, arrivalTicks :: Int
litTicks = 300
flareTicks = 80
arrivalTicks = 230

-- | The tick of the flare at which the lamp goes dark, under the brightest light.
-- 24 ticks, 0.4 seconds, into 'Flare': before it the lamp swells to white, and on
-- it the lamp dies, which Render draws and 'Game.Sound' marks with a sound.
lampDiesAt :: Int
lampDiesAt = 24

-- | The light leaves the wall beside the tower this far into 'Opening', and then
-- crosses one cell of the way every 'lightCellTicks'.
--
-- 26 ticks after 'Opening' begins, then 8 ticks a cell: 7.5 cells a second.
lightSetsOff, lightCellTicks :: Int
lightSetsOff = 26
lightCellTicks = 8

-- | When cell @k@ of the passage breaks open, in ticks into 'Opening': as the light
-- reaches it. It cracks for a while before that.
--
-- @k@ indexes 'lvPassage', the walls that break, from the mouth outward (not
-- 'wayCells', which also holds the floor between them). The distance from the mouth
-- is counted in cells across plus cells down; on the passage's straight line that
-- is simply the gap. In level 3 the walls are 0, 1, 5 and 11 cells out, so they
-- break 26, 34, 66 and 114 ticks into 'Opening'. The index must be non-negative;
-- one past the list, or a level with no passage, gives 0. Render draws the cracks
-- and Sound plays a crash on
-- the tick each wall breaks.
cellOpenTick :: Level -> Int -> Int
cellOpenTick level k = case lvPassage level of
  -- A case branch with a guard: taken only when the list is not empty and the
  -- guard holds. Otherwise the next branch, _, matches anything.
  mouth : _ | k < length (lvPassage level) ->
    lightSetsOff + lightCellTicks * apart mouth (lvPassage level !! k)
  _ -> 0
  where apart (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

-- | 'Opening' lasts until the light has left the station, and a little longer.
--
-- The light sets off, crosses every cell of the way at 'lightCellTicks' a cell, and
-- 36 ticks (0.6 seconds) follow before the keeper may walk. 158 ticks in level 3,
-- whose way is 12 cells long.
openTicks :: Level -> Int
openTicks level = lightSetsOff + lightCellTicks * length (wayCells level) + 36

-- | Ticks from the win until the keeper may walk again.
--
-- The three stages of fixed or known length added up: 448 ticks in level 3, about
-- seven and a half seconds. The demo and the tests wait this long.
revealTicks :: Level -> Int
revealTicks level = litTicks + flareTicks + openTicks level

-- | A level runs its ending when it has a passage and a level waits beyond it.
--
-- Asked by 'stepRun' on the winning tick. Both halves matter: the passage gives the
-- light somewhere to go, and the level after it gives the rush somewhere to arrive.
-- Without a level beyond, the same win just shows the won plate.
hasEnding :: [Level] -> Int -> Bool
hasEnding levels i = not (null (lvPassage (levels !! i))) && i + 1 < length levels

-- | The won level with its passage open, as the keeper walks it: free to walk, and
-- with nothing left to buy.
--
-- Every price is 0, so no move spends time, and with a step costing nothing the
-- keeper can never run out of time here. The record update reaches two levels deep:
-- new 'Rules' made from the old with four prices replaced, inside a new 'Level'.
-- 'advanceFinale' plays 'Beckon' on this level; Render draws from it.
revealedLevel :: Level -> Level
revealedLevel level = (partlyRevealed (length (lvPassage level)) level)
  { lvRules = (lvRules level) { ruStepCost = 0, ruShiftCost = 0, ruJumpCost = 0, ruEchoCost = 0 } }

-- | The level with the first @n@ cells of its passage open, in the present.
--
-- The passage's cells are added to the present's floor with 'Set.union'. The past
-- is left alone: the walls only break in the present. Render uses it to draw the
-- walls opening one at a time.
partlyRevealed :: Int -> Level -> Level
partlyRevealed n level =
  level { lvPresent = lvPresent level `Set.union` Set.fromList (take n (lvPassage level)) }

-- | Walking in the opened station: no undo, no restart, no echo, no peek. The era
-- can still change, for free: some corners of a won station (a pocket behind a
-- one-way mirror) are only left through the other era. The level is won; only the
-- way on is left.
--
-- A record update on the keys: these four read as up whatever is pressed, and
-- every other key (the directions, Space, P, Enter, Escape) passes through.
beckonKeys :: TickInput -> TickInput
beckonKeys i = i { tiUndo = False, tiRestart = False, tiEcho = False, tiPeek = False }
-- R is read by the ending itself: it brings the keeper back to the lamp.

-- | One tick of the ending. Nothing once it is over and the next level should begin.
--
-- Arguments: the won level, this tick's keys, the ending so far (taken apart by the
-- pattern into its stage, age and rush), and the board. The result pairs the next
-- 'Finale' with the next board; 'Nothing' tells 'stepRun' to start the level beyond.
-- Until 'Beckon' the board only remembers the keys. In 'Beckon' the ordinary rules
-- play on 'revealedLevel'. In 'Rush' the keeper is carried by 'stepDash', and the
-- board's 'gCell' stays the cell the rush began on: Render places the keeper
-- 'dsAlong' cells on from it.
advanceFinale :: Level -> TickInput -> Finale -> Game -> Maybe (Finale, Game)
advanceFinale level input (Finale stage age dash) g = case stage of
  -- The lamp burns, then flares; the flare dies, then the light runs off.
  Lit -> timed litTicks Flare held
  Flare -> timed flareTicks Opening held
  -- Once the light has gone, the keeper is let go: 'freed' below.
  Opening
    | age + 1 >= openTicks level -> Just (Finale Beckon 0 dash, freed)
    | otherwise -> Just (Finale Opening (age + 1) dash, held)
  Beckon
    -- R brings the keeper back to the lamp, whatever corner they are in.
    | tiRestart input && not (tiRestart (gPrev g)) ->
        Just (Finale Beckon (age + 1) dash, freed { gCell = lvGoal level, gEra = Present })
    -- Otherwise the rules play one tick on the opened, free level, with undo,
    -- restart, echo and peek taken away.
    | otherwise ->
    let g' = stepGame (revealedLevel level) (beckonKeys input) g
        -- A step that lands on the way starts the rush there, even with a key held
        -- that has already set off the next step: that step is not taken.
        landed = gCell g' /= gCell g && gCell g' `elem` wayCells level
        -- The board as that step landed: settled, with the step the held key began
        -- this same tick called off (a step keeps 'gCell' at its origin until it
        -- lands, so the keeper stays on the way), and the key no longer armed to
        -- repeat.
        arrived = g' { gMotion = Settled, gPushing = Nothing, gPendingMove = Nothing, gArmed = False }
    -- Standing still on the way in the present starts the rush. So does a step
    -- that has just landed on it, in the present and in play. Both need the
    -- present, the era whose walls the light broke. Anything else walks on.
    -- f $ x is f x: the dollar saves brackets around a long argument.
    in Just $ if onTheWay level g' then (Finale Rush 0 restingDash, g')
              else if landed && gEra g' == Present && gPhase g' == Playing then (Finale Rush 0 restingDash, arrived)
              else (Finale Beckon (age + 1) dash, g')
  -- The rush, carried by 'stepDash' from the cell it began on. When it is over, or
  -- if the level has no straight way (a passage of fewer than two cells), the
  -- arrival begins.
  Rush -> case finaleRun level of
    Nothing -> Just (Finale Arrival 0 dash, held)
    Just fr -> case stepDash fr (gCell g) input g dash of
      Nothing -> Just (Finale Arrival 0 dash, held)
      Just (dash', g') -> Just (Finale Rush (age + 1) dash', g' { gPrev = input })
  -- The real lighthouse, for 'arrivalTicks'; then Nothing, and the next level.
  Arrival
    | age + 1 >= arrivalTicks -> Nothing
    | otherwise -> Just (Finale Arrival (age + 1) dash, held)
  where
    -- The board unchanged, only remembering this tick's keys, so a key held through
    -- the ending is not taken for a fresh press later.
    held = g { gPrev = input }
    -- A stage of fixed length n: a tick older, or on to stage next at age 0 once
    -- its n ticks are done. The rush and the board g' go along unchanged.
    timed n next g' = Just (if age + 1 >= n then Finale next 0 dash else Finale stage (age + 1) dash, g')
    -- The cell is spent, and the light broke whatever stood on its way, crates too.
    -- Keys held through the ending need a fresh press. The keeper looks the way the
    -- light went, as they watched it go.
    -- The history is emptied, so nothing can be undone back past the win, and every
    -- waiting action and message is dropped. 'Set.difference' takes every cell of
    -- the way out of the crates. maybe DirLeft frDir m is 'DirLeft' when m is
    -- Nothing, and 'frDir' of what is inside it otherwise.
    freed = g
      { gPhase = Playing, gHistory = [], gHasCell = False, gMotion = Settled
      , gPushing = Nothing, gFacing = maybe DirLeft frDir (finaleRun level)
      , gFeedback = Nothing, gBump = 0
      , gCrates = gCrates g `Set.difference` Set.fromList (wayCells level)
      , gPendingShift = False, gPendingMove = Nothing, gPendingUndo = False
      , gPendingEcho = False, gArmed = False, gPrev = input }

-- | The rush starts as soon as the keeper stands anywhere on the way, in the present.
--
-- Standing means 'Settled', between steps and not in a shift's wave; 'Playing'
-- rules out a paused board (P still works while the keeper walks). 'elem' asks
-- whether a value is in a list; written in backticks, a two-argument function is
-- used between its arguments, like an operator.
onTheWay :: Level -> Game -> Bool
onTheWay level g = gPhase g == Playing && gMotion g == Settled && gEra g == Present
                   && gCell g `elem` wayCells level

-- | Where the rush goes. The passage starts in the wall beside the lighthouse, then
-- runs straight to the edge of the station; the light crosses everything between,
-- and that whole line is the way. Beyond the map a corridor carries straight on,
-- and opens into the chamber of the real lighthouse.
--
-- Cells beyond the map have coordinates outside it (negative, or past its width or
-- height), which is fine: a map is a set of open cells, so they need no special
-- case. In level 3 the mouth is (11, 3), the edge (0, 3), the direction 'DirLeft',
-- the arrival (-58, 3) and the tower (-59, 3).
data FinaleRun = FinaleRun
  { frMouth   :: !Cell  -- ^ the first cell of the passage, beside the lighthouse
  , frEdge    :: !Cell  -- ^ the last, in the outer wall of the station
  , frDir     :: !Dir   -- ^ the way the rush goes, from the mouth to the edge
  , frArrival :: !Cell  -- ^ where the keeper comes to rest: the middle of the island,
                        --   'corridorLength' + 4 cells beyond the edge
  , frTower   :: !Cell  -- ^ the real lighthouse, just beyond: one cell further on
  } deriving (Eq, Show)

-- | Cells of corridor beyond the edge of the map, before the chamber.
--
-- The causeway is 54 cells long; the island of the real lighthouse follows, 7 cells
-- deep. Barriers, lamp posts and the island are all placed by cells out from the
-- edge, so they all move together if this changes.
corridorLength :: Int
corridorLength = 54

-- | The straight way of a level's ending, worked out from its passage: the first
-- passage cell is the mouth, the last is the edge, and the direction is whichever
-- leads from one to the other. 'Nothing' when the passage has fewer than two cells,
-- so no direction. The passage is meant to lie along one row or column; only its
-- first and last cells are read here, and every cell between them is on the way
-- ('wayCells').
finaleRun :: Level -> Maybe FinaleRun
finaleRun level = case lvPassage level of
  -- As-patterns: mouth@(mx, my) names the first cell and also takes it apart, and
  -- rest@(_ : _) names the rest of the list while requiring it to be non-empty,
  -- which is what makes 'last' below safe.
  mouth@(mx, my) : rest@(_ : _) ->
    let edge@(ex, ey) = last rest
        -- Left or right if the edge is in another column, otherwise up or down.
        dir | ex < mx = DirLeft
            | ex > mx = DirRight
            | ey < my = DirUp
            | otherwise = DirDown
        -- The cell n cells beyond the edge, straight on.
        ahead n = let (dx, dy) = dirDelta dir in (ex + dx * n, ey + dy * n)
    -- A record built with its field names, in any order.
    in Just FinaleRun
         { frMouth = mouth, frEdge = edge, frDir = dir
         , frArrival = ahead (corridorLength + 4)
         , frTower = ahead (corridorLength + 5)
         }
  _ -> Nothing

-- | Every cell the light crosses in the station, from the mouth to the edge: the
-- passage it broke open and the floor between.
--
-- In level 3, the twelve cells from (11, 3) to (0, 3), in that order. Empty when
-- the level has no straight way. Standing on any of them in the present starts the
-- rush, and every crate on them is broken when the keeper is freed.
wayCells :: Level -> [Cell]
wayCells level = case finaleRun level of
  Just fr ->
    let (dx, dy) = dirDelta (frDir fr)
        (mx, my) = frMouth fr
        (ex, ey) = frEdge fr
        -- Cells from the mouth to the edge; one of the two terms is always 0.
        n = abs (ex - mx) + abs (ey - my)
    -- A list comprehension: for each i from 0 (the mouth) to n (the edge), the
    -- cell i steps from the mouth in the way's direction.
    in [ (mx + dx * i, my + dy * i) | i <- [0 .. n] ]
  Nothing -> []

-- | Cells from a start on the way to where the rush comes to rest.
--
-- Counted as cells across plus cells down. A start on the way lies on the same
-- line as the arrival, so this is just the number of cells between: 69 from level
-- 3's mouth. 'stepDash' takes it as the length of the whole rush.
rushLength :: FinaleRun -> Cell -> Int
rushLength fr (sx, sy) = let (ax, ay) = frArrival fr in abs (ax - sx) + abs (ay - sy)

-- | Where the causeway is blocked, counting cells out from the station's edge, and
-- the era it is blocked in: rubble fallen in the present, a shut gate in the past.
-- They alternate, so the keeper keeps changing era to get through, and come closer
-- together as the run speeds up.
--
-- The gaps shrink from six cells to three or four. A barrier shuts all three cells
-- across the causeway in its own era and leaves them open in the other
-- ('dashLevel'); the rush itself checks barriers by distance ('barriersFrom').
barriers :: [(Int, Era)]
barriers =
  [ (5, Present), (11, Past), (16, Present), (21, Past), (25, Present), (29, Past)
  , (33, Present), (36, Past), (40, Present), (43, Past), (47, Present) ]

-- | Cells out from the edge, @across@ to one side of the line.
--
-- @outFrom fr along across@ starts at the edge cell, goes @along@ cells further in
-- the way's direction, then @across@ cells sideways. The sideways step is the
-- direction (dx, dy) with its parts swapped, (dy, dx), which is at right angles to
-- it. Which side a positive @across@ lands on depends on the direction, so every
-- caller takes both sides alike (-1 to 1 for the causeway, -3 to 3 for the island,
-- 1 and -1 for the lamp posts). @outFrom fr n 0@ is on the line itself.
outFrom :: FinaleRun -> Int -> Int -> Cell
outFrom fr along across =
  let (dx, dy) = dirDelta (frDir fr)
      (ex, ey) = frEdge fr
  in (ex + dx * along + dy * across, ey + dy * along + dx * across)

-- | The whole way, as one map, for drawing the rush: the lighthouse's room with its
-- passage open; then, out over the sea, the causeway three cells wide with its
-- barriers; and the island of the real lighthouse, seven by seven. Outside the
-- station, whatever is not on this map is open sea.
--
-- Only drawing reads this map (the app's Render, and the tests check it): the rush
-- moves along one line and checks barriers by distance, so 'stepDash' never looks
-- at it. A level with no straight way is returned as it is.
dashLevel :: Level -> Level
dashLevel level = case finaleRun level of
  Nothing -> level
  Just fr ->
    let (rx, ry) = roomOf (lvGoal level)
        -- The lighthouse's room (13 by 9 cells) and a ring one cell wide around
        -- it, which keeps the first cell through each of its doorways. Open cells
        -- anywhere else in the station are cut away.
        nearHome (x, y) = x >= rx * roomCols - 1 && x <= (rx + 1) * roomCols
                          && y >= ry * roomRows - 1 && y <= (ry + 1) * roomRows
        -- The causeway's floor in one era: 1 to 'corridorLength' cells out, three
        -- cells across, without the barriers that stand in that era. A list
        -- comprehension with two generators (every i with every j) and a test.
        causeway era = Set.fromList
          [ outFrom fr i j | i <- [1 .. corridorLength], j <- [-1 .. 1]
          , (i, era) `notElem` barriers ]
        -- The island: the 7 cells after the causeway, 7 across, open in both eras.
        island = Set.fromList [ outFrom fr i j | i <- [corridorLength + 1 .. corridorLength + 7], j <- [-3 .. 3] ]
    -- The present keeps the passage open ('revealedLevel'); the past keeps its
    -- walls, since the light broke them only in the present.
    in level
         { lvPresent = Set.unions [Set.filter nearHome (lvPresent (revealedLevel level)), causeway Present, island]
         , lvPast = Set.unions [Set.filter nearHome (lvPast level), causeway Past, island]
         }

-- The rush ---------------------------------------------------------------------
--
-- Once on the way, the keeper runs by themselves and the screen is carried along
-- ahead of them, ever faster. Out on the causeway, barriers stand in one era or the
-- other: Space changes era, for free, to find the one where the way is open. A
-- keeper held up too long falls off the back of the screen, and starts again from
-- the last barrier they passed.
--
-- Everything here is measured in cells along the way from the cell the rush began
-- on, as a 'Float': the keeper ('dsAlong'), the screen ('dsScroll'), the edge of
-- the station ('edgeFrom') and the barriers ('barriersFrom'). Speeds are cells per
-- tick. Nothing is worked out from the map; the rush is one line.

-- | The run so far.
data Dash = Dash
  { dsAlong    :: !Float  -- ^ cells the keeper has come from where the rush began
  , dsSpeed    :: !Float  -- ^ the keeper's speed, cells per tick
  , dsScroll   :: !Float  -- ^ how far the screen has been carried, in cells; the
                          --   keeper stops gaining on it once 'leadMax' ahead, and
                          --   is caught once more than 'caughtGap' behind
  , dsClock    :: !Int    -- ^ ticks of carrying, which set the screen's speed
                          --   ('screenSpeed')
  , dsCheck    :: !Float  -- ^ where a keeper left behind starts again: 0 at first,
                          --   then one cell beyond the last barrier passed
  , dsCheckEra :: !Era    -- ^ the era they start again in: the one they passed that
                          --   barrier in
  , dsCaught   :: !Int    -- ^ ticks left of being taken back, 0 while running
  , dsRefused  :: !Int    -- ^ ticks left of showing a shift that was refused
  , dsShifted  :: !Int    -- ^ ticks since the era last changed
  , dsBlocked  :: !Bool   -- ^ standing at a barrier, unable to go on
  , dsFails    :: !Int    -- ^ times taken back
  } deriving (Eq, Ord, Show)

-- | At rest, before the rush.
--
-- The fields in order: at the start, still, the screen at the start and its clock
-- at 0, the checkpoint at the start in the present, not caught, nothing refused, 99
-- ticks since a shift (so no shift ripple shows at the start; Render shows one for
-- 'shiftTicks'), not blocked, never taken back.
restingDash :: Dash
restingDash = Dash 0 0 0 0 0 Present 0 0 99 False 0

-- | The screen's speed: from a walk to a sprint, then steady.
--
-- @clock@ is ticks of carrying. In cells per tick: 0.05 (3 cells a second) at 0,
-- rising by 0.00045 each tick, until 'min' holds it at 'dashTopSpeed' after about
-- 256 ticks, a little over four seconds.
screenSpeed :: Int -> Float
screenSpeed clock = min dashTopSpeed (0.05 + 0.00045 * fromIntegral clock)

-- | The screen's top speed: 0.165 cells a tick, about 10 cells a second. Render
-- and 'Game.Sound' measure the keeper's speed against it for streaks and wind.
dashTopSpeed :: Float
dashTopSpeed = 0.165

-- | How far ahead of the screen the keeper may run, and how far behind it they may
-- fall before they are taken back. Both are in cells, measured from 'dsScroll':
-- more than 4 ahead and the keeper keeps pace with the screen instead of gaining,
-- more than 5.4 behind and they are caught.
leadMax, caughtGap :: Float
leadMax = 4
caughtGap = 5.4

-- | The ticks a taking back lasts; the keeper is moved halfway through, while the
-- screen is dark. 50 ticks, under a second; Render darkens the screen towards that
-- middle tick and lightens it after.
catchTicks :: Int
catchTicks = 50

-- | Cells from a start on the way to the station's edge.
--
-- 'rushLength' counts to the arrival, which is 'corridorLength' + 4 cells beyond
-- the edge, so taking that off leaves the cells from the start to the edge cell.
-- 11 from level 3's mouth. A 'Float', to compare with 'dsAlong'.
edgeFrom :: FinaleRun -> Cell -> Float
edgeFrom fr start = fromIntegral (rushLength fr start - corridorLength - 4)

-- | Where each barrier stands, in cells from the start, and its era.
--
-- The same list as 'barriers', moved from cells out from the edge into the rush's
-- own measure. Each position is a barrier's centre.
barriersFrom :: FinaleRun -> Cell -> [(Float, Era)]
barriersFrom fr start = [ (edgeFrom fr start + fromIntegral i, e) | (i, e) <- barriers ]

-- | Whether the keeper, this far along, stands where a barrier of this era is.
--
-- A barrier counts as filling 0.75 cells either side of its centre. 'any' is True
-- when its test holds for some element; the test is a lambda (an unnamed function,
-- written with a backslash) that takes a barrier's pair apart. Used to refuse a
-- shift into a barrier, and by the demo's keeper.
inBarrier :: FinaleRun -> Cell -> Era -> Float -> Bool
inBarrier fr start era along = any (\(a, e) -> e == era && abs (along - a) < 0.75) (barriersFrom fr start)

-- | One tick of the rush. Nothing once the keeper has arrived.
--
-- Arguments: the straight way, the cell the rush began on (it stays the board's
-- 'gCell' throughout), this tick's keys (only Space is read), the board (only its
-- era and last keys) and the rush so far. Returns the next rush and the board with
-- its era changed if a shift was taken; 'advanceFinale' then records the keys.
--
-- A tick, in order: a keeper being taken back only waits, and is put back on the
-- middle tick; otherwise Space may change era (only out on the causeway, and never
-- into a barrier), the screen moves on at its speed, the keeper's speed eases
-- towards the speed they want, they run that far unless a barrier of their era
-- stops them, passing a barrier moves the checkpoint, and trailing the screen by
-- more than 'caughtGap' starts a taking back.
stepDash :: FinaleRun -> Cell -> TickInput -> Game -> Dash -> Maybe (Dash, Game)
stepDash fr start input g d
  -- Being taken back: count down, showing no refusal. On the middle tick, while the
  -- screen is darkest, the keeper is put back at the checkpoint, in the era they
  -- passed it in, standing still with the screen one cell behind them, the screen's
  -- speed set back to 0.08 cells a tick, and no shift ripple. div divides whole
  -- numbers, rounding down: the middle is tick 25.
  | dsCaught d > 0 =
      let left = dsCaught d - 1
          waiting = d { dsCaught = left, dsRefused = 0 }
      in Just $ if left == catchTicks `div` 2
           then ( waiting { dsAlong = dsCheck d, dsScroll = dsCheck d - 1, dsSpeed = 0
                     , dsClock = clockFor 0.08, dsBlocked = False, dsShifted = 99 }
                , g { gEra = dsCheckEra d } )
           else (waiting, g)
  -- At the arrival, to within a thousandth of a cell (sums of 'Float's are not
  -- exact): the rush is over.
  | dsAlong d >= len - 0.001 = Nothing
  -- Running: the board takes the era chosen this tick.
  | otherwise = Just (d', g { gEra = era' })
  where
    -- The whole rush, start to arrival, in cells. 'fromIntegral' turns an Int into
    -- any kind of number; the annotation :: Float says which.
    len = fromIntegral (rushLength fr start) :: Float
    -- The clock at which 'screenSpeed' gives this speed: its formula solved for the
    -- clock (67 for 0.08). A local function may have its own type signature.
    clockFor :: Float -> Int
    clockFor speed = round ((speed - 0.05) / 0.00045)
    -- A fresh press of Space this tick.
    fresh = tiShift input && not (tiShift (gPrev g))
    -- The era changes only out on the causeway, and only where the other era has
    -- floor under the keeper.
    -- Out means 0.3 of a cell past the edge cell, the station's outer wall.
    outside = dsAlong d > edgeFrom fr start + 0.3
    target = otherEra (gEra g)
    canShift = outside && not (inBarrier fr start target (dsAlong d))
    -- The era after this tick: changed only by a fresh press that is allowed.
    era' = if fresh && canShift then target else gEra g
    -- A press that is not allowed shows the refusal ring for 24 ticks, which Render
    -- fades out as this counts down to 0.
    refused = if fresh && not canShift then 24 else max 0 (dsRefused d - 1)
    -- The screen's speed this tick.
    v = screenSpeed (dsClock d)
    -- The screen stops where it frames the real lighthouse.
    -- That is three cells short of the arrival.
    scroll' = min (len - 3) (dsScroll d + v)
    -- The keeper runs a little faster than the screen, up to a lead, and slows to
    -- a stop in front of the real lighthouse.
    -- More than 'leadMax' ahead they want the screen's own speed; otherwise 30%
    -- more than it, and a little over, so they draw ahead even at a slow start.
    want | dsAlong d > dsScroll d + leadMax = v
         | otherwise = v * 1.3 + 0.012
    -- Ease speed s towards w: faster by at most 0.006 cells a tick each tick,
    -- slower by at most 0.01, so the keeper never leaps from a walk to a sprint.
    approach s w = if s < w then min w (s + 0.006) else max w (s - 0.01)
    -- Near the end each tick covers at most a twentieth of the distance left, so
    -- the keeper slows smoothly into the arrival; the floor of 0.015 cells a tick
    -- makes sure they get there rather than creeping closer for ever.
    speed0 = min (approach (dsSpeed d) want) (max 0.015 ((len - dsAlong d) * 0.05))
    -- Where that speed would take them, never past the arrival.
    proposed = min len (dsAlong d + speed0)
    -- Barriers of the keeper's era (after this tick's shift) that this move would
    -- reach. Each stops the keeper 0.7 cells short of its centre. A keeper already
    -- standing at that point (to within a thousandth) counts as reaching it again,
    -- so a blocked keeper stays blocked until the era changes. The pattern (a, e)
    -- takes each pair apart; the three tests after it pick the pairs kept.
    stops = [ a - 0.7 | (a, e) <- barriersFrom fr start, e == era'
                      , dsAlong d <= a - 0.7 + 0.001, proposed > a - 0.7 ]
    -- With no barrier in the way they run on. Otherwise the nearest stops them,
    -- standing, blocked; 'max' makes sure that never moves them backwards. The
    -- three results are taken out of one tuple by the pattern on the left.
    (along', speed', blocked) = case stops of
      [] -> (proposed, speed0, False)
      _ -> (max (dsAlong d) (minimum stops), 0, True)
    -- Passing a barrier is progress kept: a keeper taken back starts just beyond it.
    -- A barrier of either era counts as passed once the keeper is 0.75 cells
    -- beyond its centre.
    passed = [ a | (a, _) <- barriersFrom fr start, dsAlong d < a + 0.75, along' >= a + 0.75 ]
    -- The checkpoint: one cell beyond a barrier just passed, in the era it was
    -- passed in; otherwise as before. At most one barrier can be passed in a tick,
    -- since they stand cells apart and the keeper covers less than a cell a tick.
    (check', checkEra') = case passed of
      (a : _) -> (a + 1, era')
      [] -> (dsCheck d, dsCheckEra d)
    -- Too far behind the screen: taken back, starting next tick.
    caught = scroll' - along' > caughtGap
    -- The new rush. 'dsShifted' restarts at 0 on a change of era, for Render's
    -- ripple.
    d' = d { dsAlong = along', dsSpeed = speed', dsScroll = scroll', dsClock = dsClock d + 1
           , dsCheck = check', dsCheckEra = checkEra'
           , dsCaught = if caught then catchTicks else 0
           , dsFails = dsFails d + (if caught then 1 else 0)
           , dsRefused = refused
           , dsShifted = if era' /= gEra g then 0 else dsShifted d + 1
           , dsBlocked = blocked }

-- | Whether a keeper who reads the barriers ahead would press Space now: just before
-- one that stands in this era, if the other era is open here. For the demo.
--
-- In full: not being taken back, out past the edge as 'stepDash' requires, no
-- barrier of the other era here, and a barrier of this era whose stopping point
-- (0.7 cells short of it) is less than 1.4 cells ahead, or was reached just now
-- (up to 0.01 behind). Pressing then changes era before that barrier can stop
-- them; the tests check the demo's keeper gets through with one press a barrier
-- and is never taken back.
dashWantsShift :: FinaleRun -> Cell -> Era -> Dash -> Bool
dashWantsShift fr start era d =
  dsCaught d == 0 && dsAlong d > edgeFrom fr start + 0.3
  && not (inBarrier fr start (otherEra era) (dsAlong d))
  && any (\(a, e) -> e == era && a - 0.7 - dsAlong d < 1.4 && a - 0.7 - dsAlong d >= -0.01) (barriersFrom fr start)

-- | The keys a keeper who reads the barriers would press through the whole rush,
-- from the mouth of the passage, until they arrive. For the demo.
--
-- It plays the rush with 'stepDash' from the first cell of the way, where the demo
-- steps in, and keeps one 'TickInput' a tick: Space when 'dashWantsShift' says so,
-- but never two ticks running, since a press needs a tick up before it counts
-- again. The list ends with the tick on which 'stepDash' finds the keeper arrived,
-- the tick that moves the ending on to 'Arrival'. Used by the app's Main for
-- @--demo@, and by the tests; empty for a level with no straight way.
--
-- The list is lazy: 'go' gives its first input before working out the rest, so
-- the simulation runs only as far as the list is read. The stop at 20000 ticks
-- (over five minutes) ends the list if the keeper somehow never arrived.
rushInputs :: Level -> [TickInput]
rushInputs level = case (finaleRun level, wayCells level) of
  (Just fr, start : _) -> go fr start (startGame start) restingDash False (0 :: Int)
  _ -> []
  where
    -- A board standing at the start, in the present, with no keys held before.
    startGame start = (initialGame level) { gCell = start, gEra = Present, gPrev = noInput }
    -- go fr start g d pressedLast n: the board, the rush, whether Space was down
    -- on the tick before, and the ticks so far. The board keeps this tick's keys in
    -- 'gPrev' for the next, as 'advanceFinale' does in the game.
    go fr start g d pressedLast n
      | n > 20000 = []
      | otherwise =
          let press = not pressedLast && dashWantsShift fr start (gEra g) d
              input = noInput { tiShift = press }
          in case stepDash fr start input g d of
               Nothing -> [input]
               Just (d', g') -> input : go fr start (g' { gPrev = input }) d' press (n + 1)
