-- | The pure rules: one fixed tick from one state to the next.
--
-- Order inside a Playing tick, which the tests pin down:
--
--   1. buffer fresh presses (except Space during a wave; a newer move replaces
--      an older pending direction)
--   2. advance the current step or wave; a landing step touches its new cell
--   3. if settled and no action is affordable, the level is lost (a win in step 2
--      has already changed the phase, so reaching the goal on the last second wins)
--   4. if still playing and settled: a pending undo, a pending echo, the pending
--      shift, then a pending or held move
--
-- Time is a budget, not a clock: ticks spend nothing. An action is charged once,
-- when it is accepted.
--
-- What the module is for. Every rule of the game is here: walking, pushing crates
-- and movable mirrors, stepping through mirrors, switching era, the echo, undo,
-- picking up the power cell, winning and losing, and the second keeper of a level
-- for two. 'stepGame' is the entry point: a plain function from the level, this
-- tick's keys and the game to the game one tick later. Around it are the questions
-- the rules ask of the board ('openCell', 'doorsOpen', 'weights', 'mirrorAhead',
-- 'beamsOf', 'crossings', 'echoPath'). They are exported so that the drawing asks
-- exactly the same questions, and can never show something the rules would not do.
-- Last come the sentences the screen shows when a rule refuses something or has
-- something to say.
--
-- What it does not do. No drawing, no IO, no randomness and no wall clock: the same
-- level, keys and game always give the same next game, which is what lets the
-- solver and the tests replay a route tick for tick. It does not read the keyboard
-- ('Game.Input' turns key events into one 'TickInput' a tick), does not know which
-- level of the campaign is being played or move between levels ('Game.Run'), does
-- not decide effects or sounds ('Game.Fx' and 'Game.Sound' work those out by
-- comparing two successive games), and does not search ('Game.Solution').
--
-- Who feeds it and who uses it. It reads the types of 'Game.Types' and the maps'
-- floor through 'walkable' from 'Game.Level'. 'Game.Run' calls 'stepGame' once a
-- tick for the level being played; 'Game.Solution' plays whole routes through
-- 'stepGame' to find each level's par and dead ends; 'Game.Fx', 'Game.Sound' and
-- 'Game.Story' ask whether, and by whom, the power cell is carried; app/Render.hs
-- and app/Hud.hs ask the board questions and take the words; test/Main.hs checks
-- the rules directly.
--
-- Units and conventions:
--
--   * A tick is 1/60 of a second ('ticksPerSecond'). A step takes 'stepTicks' (8)
--     ticks to walk and the era wave 'shiftTicks' (13); messages ('gFeedback') and
--     the bump nudge ('gBump') also count down in ticks.
--   * Prices and the budget ('gRemaining') are whole seconds of the level's time
--     budget. They come from the level's 'Rules' record and are never written
--     here: in 'baseRules' a step or a push costs 2, a jump 4, an echo 4 and a
--     shift 6, and an undo gives back half.
--   * A 'Cell' is (x, y) in map cells, (0, 0) the top-left, y growing down the map.
--     Nothing here is in screen or canvas units.
--   * Much of the board is worked out when asked, not stored: whether the doors are
--     open, where the player could switch era, where light reaches. 'Game' keeps
--     only what actions change (where the keepers, crates, mirrors and echo are,
--     which pits are filled, the history), so undo, restart and the solver have
--     nothing else to keep in step.
--
-- Two keepers. Every rule is written for one "player": the fields 'gCell',
-- 'gMotion' and so on. In a level for two, the second keeper's turn is taken by
-- swapping the two keepers' fields ('swapKeepers'), running the same rule and
-- swapping back ('asPartner'), so both keepers obey one set of rules.
module Game.Logic
  -- Starting a level, and running it a tick at a time.
  ( initialGame
  , stepGame
  -- Two keepers: the swap that lets one set of rules serve both, and who carries.
  , swapKeepers
  , anyHasCell
  , carrierCell
  , landedAt
  -- The actions themselves, for the drawing's previews and the tests.
  , tryShift
  , tryStep
  , tryEcho
  -- Questions about the board, worked out fresh each time they are asked.
  , openCell
  , doorsOpen
  , weights
  , echoPath
  , echoLanding
  -- Mirrors: the light they throw and the jumps they offer. Jump (..) exports the
  -- type together with its constructor and fields.
  , Jump (..)
  , mirrorAhead
  , mirrorAt
  , opposite
  , jumpTarget
  , beamCells
  , beamsOf
  , lightPasses
  -- Where the player could switch era.
  , crossings
  -- Hints, and every sentence the screen shows for a rule.
  , Hint (..)
  , currentHint
  , objectiveText
  , hintText
  , blockedTitle
  , blockedDetail
  , tooCostlyTitle
  , tooCostlyDetail
  , pushBlockedTitle
  , pushBlockedDetail
  , pushAgainText
  , rustedText
  , deadEndText
  , peekText
  , victoryText
  , failureText
  , pickupText
  , echoNeedsStepsText
  , echoInDoorwayText
  , plateHeldText
  , jumpAgainText
  , mirrorFaceText
  , mirrorFixedText
  ) where

-- Map and Set are imported qualified (used as Map.lookup, Set.member) because many
-- of their names (lookup, member, filter, ...) would otherwise clash with the
-- Prelude's list functions. Both are balanced trees: looking a cell up takes time
-- growing with the logarithm of their size. Data.Map.Strict forces each stored
-- value to its outer constructor (WHNF), not recursively through all its fields.
import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Game.Level (walkable)
import Game.Types

-- | The title screen, with the level ready behind it.
--
-- The game before a level starts: in the 'Menu' phase, the keeper on the spawn in
-- the present, the whole budget, the crates and mirrors where the level's maps put
-- them, no history and no keys remembered. A fresh Enter in 'stepGame' turns it into
-- a 'Playing' game. 'Game.Run' builds its playing games from it directly, with a
-- record update such as @(initialGame level) { gPhase = Playing }@, and the solver
-- ('startPlaying' in 'Game.Solution') starts here and presses Enter.
--
-- @Game { field = value, ... }@ is record construction syntax: every field is named,
-- so their order does not matter, and a field left out is caught by the compiler.
initialGame :: Level -> Game
initialGame level = Game
  { gPhase        = Menu
  , gEra          = Present
  , gCell         = lvSpawn level
  , gHasCell      = False
  , gRemaining    = lvBudget level
  , gCrates       = lvCrates level
  , gFilled       = Set.empty
  , gMirrors      = lvMirrors level
    -- The spawn is the first footprint, in a level with the echo. In a level without
    -- it 'ruEchoLength' is 0, and taking 0 of the list keeps no footprints at all.
  , gTrail        = take (ruEchoLength (lvRules level)) [(Present, lvSpawn level)]
  , gEcho         = Nothing
  , gHistory      = []
  , gPushing      = Nothing
    -- Counts starts: 'stepGame' adds one at every start and restart, and 'Game.Run'
    -- whenever it sets up a fresh game; 'Game.Fx' and 'Game.Sound' take a change in
    -- it as a restart.
  , gAttempt      = 0
  , gMotion       = Settled
  , gFacing       = DirDown
  , gPendingShift = False
  , gPendingMove  = Nothing
  , gPendingUndo  = False
  , gPendingEcho  = False
  , gFeedback     = Nothing
  , gBump         = 0
    -- No keys down "last tick", so a key already down on the first tick counts as a
    -- fresh press. 'Game.Run' puts the current keys here when it starts a level, so
    -- the Enter that chose the level does nothing more.
  , gPrev         = noInput
  , gArmed        = False
  , gShowInstructions = False
  , gTitleChoice  = 0
    -- fmap applies a function inside a Maybe: 'Just' a new second keeper where the
    -- level has a second spawn, 'Nothing' (a level for one) otherwise.
  , gPartner      = fmap newPartner (lvSpawn2 level)
  }
  where
    -- A where clause names helpers for the whole equation above it. This one is the
    -- second keeper as a level starts: on cell @c@, at rest, facing down the map,
    -- without the power cell and with no key waiting.
    newPartner c = Partner
      { ptCell = c, ptHasCell = False, ptMotion = Settled, ptFacing = DirDown
      , ptPushing = Nothing, ptPendingMove = Nothing, ptArmed = False, ptBump = 0 }

-- Two keepers ------------------------------------------------------------------
--
-- In a level for two, the second keeper plays by exactly the first one's rules:
-- every rule is written for "the player", and the second keeper's turn is taken by
-- swapping the two keepers' places in the game, playing, and swapping back. The era,
-- the crates, the mirrors, the time and the history stay where they are, shared.

-- | Trade places: the second keeper becomes the one the rules speak of.
--
-- Every field that belongs to one keeper is exchanged between 'Game' (the keeper
-- the rules call "the player") and its 'Partner' (the other one): the cell, whether
-- they carry the power cell, the motion, the facing, the crate or mirror being
-- pushed, a buffered move, whether a held key may repeat, and the bump nudge. What
-- is shared (the era, crates, mirrors, time, history, the waiting shift, undo and
-- echo, the message) stays where it is. Swapping twice gives back the game it
-- started from, which is what lets 'asPartner' swap, play and swap back. In a level
-- for one ('gPartner' is 'Nothing') the game comes back unchanged.
--
-- Outside the rules, app/Render.hs draws the second keeper from the swapped game,
-- and app/Hud.hs looks for the second keeper's mirror jumps in it.
swapKeepers :: Game -> Game
swapKeepers g = case gPartner g of
  Nothing -> g
  -- @g { field = value, ... }@ is record update syntax: a copy of @g@ with only the
  -- named fields changed. Nothing is changed in place; @g@ itself stays as it was.
  Just p -> g
    { gCell = ptCell p, gHasCell = ptHasCell p, gMotion = ptMotion p, gFacing = ptFacing p
    , gPushing = ptPushing p, gPendingMove = ptPendingMove p, gArmed = ptArmed p
    , gBump = ptBump p
    , gPartner = Just Partner
        { ptCell = gCell g, ptHasCell = gHasCell g, ptMotion = gMotion g
        , ptFacing = gFacing g, ptPushing = gPushing g, ptPendingMove = gPendingMove g
        , ptArmed = gArmed g, ptBump = gBump g }
    }

-- | Run a rule as the second keeper. Anything it charges is remembered from the
-- first keeper's side, like everything else in the history, so undo needs no turns.
--
-- @f@ is any rule written for "the player": a stage of a tick, a landing, a pickup.
-- It runs on the swapped game and the result is swapped back. If @f@ accepted an
-- action, 'charge' pushed a 'Snapshot' while the keepers were swapped, so that one
-- snapshot speaks of the second keeper as the player; 'swapSnapshot' retells it.
-- Every snapshot in the history then speaks of the first keeper as the player, and
-- 'undo' never needs to know whose action it is taking back. In a level for one
-- there is no second keeper and @f@ is not run at all, so a rule can be wrapped in
-- asPartner without first asking whether there are two keepers.
asPartner :: (Game -> Game) -> Game -> Game
asPartner f g = case gPartner g of
  Nothing -> g
  Just _ ->
    -- let names a value for the expression after in; where does the same for a
    -- whole equation.
    let g' = swapKeepers (f (swapKeepers g))
    in case gHistory g' of
         -- The history grew, so @f@ took an action and pushed one snapshot, the
         -- newest (the pattern s : rest splits a list into its head and the rest).
         -- 'length' walks the whole history, which only holds the actions of this
         -- attempt that have not been undone.
         s : rest | length (gHistory g') > length (gHistory g) -> g' { gHistory = swapSnapshot s : rest }
         -- Nothing was charged: nothing to retell.
         _ -> g'

-- | A snapshot taken as the second keeper, told from the first keeper's side.
--
-- In such a snapshot @s@ the player's fields ('snCell', 'snHasCell', 'snFacing')
-- hold the second keeper, and 'snPartner' holds the first. They are exchanged back.
-- The first keeper is put at 'landedAt' their cell: if they were in the middle of a
-- step when the second keeper acted, that step was already paid for and has its own,
-- older snapshot, so undoing the second keeper's action must leave it taken. The
-- second keeper is stored at rest with nothing waiting, which is all 'undo' needs
-- (it settles a restored partner anyway). A snapshot without a partner is returned
-- as it is.
swapSnapshot :: Snapshot -> Snapshot
swapSnapshot s = case snPartner s of
  Nothing -> s
  Just p -> s
    { snCell = landedAt p, snHasCell = ptHasCell p, snFacing = ptFacing p
    , snPartner = Just Partner
        { ptCell = snCell s, ptHasCell = snHasCell s, ptMotion = Settled, ptFacing = snFacing s
        , ptPushing = Nothing, ptPendingMove = Nothing, ptArmed = False, ptBump = 0 }
    }

-- | Where a keeper is going to be: an accepted step has already been paid for.
--
-- While a keeper is 'Stepping', 'ptCell' is still the cell they are leaving (a step
-- only lands when its ticks run out), so this gives the destination instead; at rest
-- or during an era wave it is simply their cell. Used wherever a step in flight must
-- count as done: retelling snapshots, settling keepers, and keeping the other keeper
-- off the cell someone is walking onto ('openCell').
landedAt :: Partner -> Cell
landedAt p = case ptMotion p of
  -- A constructor pattern matches the fields by position: @to@ names the step's
  -- destination and _ ignores its ticks left.
  Stepping to _ -> to
  _ -> ptCell p

-- | A keeper brought to rest where they were headed, with nothing waiting.
--
-- A step in flight is finished at once (no landing rules run), and the buffered
-- move, the held-key repeat and the bump nudge are dropped, so a held key needs a
-- fresh press. Used by 'undo' for the restored second keeper, and by 'duoTick' when
-- the other keeper lights the lamp.
settled :: Partner -> Partner
settled p = p { ptCell = landedAt p, ptMotion = Settled, ptPushing = Nothing
              , ptPendingMove = Nothing, ptArmed = False, ptBump = 0 }

-- | The second keeper is still walking, or is in the middle of anything at all.
--
-- Always 'False' in a level for one. While it holds, the shared actions wait: an
-- undo or a shift ('startNext'), and the loss check ('outOfTime').
--
-- @maybe d f m@ gives @d@ when @m@ is 'Nothing' and @f x@ when it is @Just x@.
-- @(/= Settled) . ptMotion@ is written point-free, without naming its argument: the
-- dot composes two functions (first 'ptMotion', then the test), and @(/= Settled)@
-- is an operator section, the function that asks whether its argument is not
-- 'Settled'.
partnerBusy :: Game -> Bool
partnerBusy g = maybe False ((/= Settled) . ptMotion) (gPartner g)

-- | Whether either keeper is carrying the power cell.
--
-- There is only one power cell, so this also says whether it has left its cradle:
-- 'touchCell' and 'undo' only hand it to a keeper while this is 'False'. Used by
-- 'Game.Fx', 'Game.Sound', 'Game.Story', app/Hud.hs and app/Render.hs.
anyHasCell :: Game -> Bool
anyHasCell g = gHasCell g || maybe False ptHasCell (gPartner g)

-- | The cell of whichever keeper carries the power cell.
--
-- 'Nothing' while it is still in its cradle. A keeper in the middle of a step
-- answers with the cell they are leaving, since 'gCell' and 'ptCell' only change
-- when a step lands. 'Game.Fx' starts the pickup burst there, and app/Render.hs
-- draws the carried cell on that keeper.
--
-- The guards (| condition = result) are tried from the top; 'otherwise' is simply
-- 'True', the catch-all.
carrierCell :: Game -> Maybe Cell
carrierCell g
  | gHasCell g = Just (gCell g)
  | otherwise = case gPartner g of
      -- A case alternative can carry its own guard: this one is taken only for a
      -- second keeper who has the cell; anything else falls through to _.
      Just p | ptHasCell p -> Just (ptCell p)
      _ -> Nothing

-- | The keys each keeper reads: W A S D for the first, the arrows for the second.
-- The era, undo and echo keys belong to the first keeper's turn, so they are shared.
--
-- 'Game.Input' fills 'tiDir1' from W A S D alone and 'tiDir2' from the arrows alone.
-- Each function copies one keeper's direction into 'tiDir', the field every rule
-- reads, with its matching "revealed" flag (see 'notePresses'). The second keeper's
-- copy also has Space, Z and E switched off, so their turn can never shift, undo or
-- send an echo: those happen once, in the first keeper's turn. One type signature
-- can name several functions of the same type, as here.
keysOf1, keysOf2 :: TickInput -> TickInput
keysOf1 i = i { tiDir = tiDir1 i, tiDirRevealed = tiDir1Revealed i }
keysOf2 i = i { tiDir = tiDir2 i, tiDirRevealed = tiDir2Revealed i
              , tiShift = False, tiUndo = False, tiEcho = False }

-- | One tick for two: both keepers note their keys, both walks advance, then the
-- first keeper's turn (which holds the shared era, undo and echo), then the second.
--
-- These are the four stages of a tick for one (see the top of the module), each run
-- for the first keeper directly and for the second through 'asPartner'. The loss
-- check runs once, for both, because the time is shared. The where bindings below
-- are the stages in order, and the first line reads from the inside out: the first
-- keeper's turn (@started@), then the second keeper's move ('startMove'), then
-- @settleOnWin@. @input@ is this tick's whole set of keys; each keeper sees only
-- their own, through 'keysOf1' and 'keysOf2'.
duoTick :: Level -> TickInput -> Game -> Game
duoTick level input g0 = settleOnWin (asPartner (startMove level in2) started)
  where
    -- Last tick's keys, all of them.
    prev = gPrev g0
    in1 = keysOf1 input
    in2 = keysOf2 input
    -- Stage 1. The first keeper notes W A S D and the shared keys (True); the
    -- second, the arrows alone (False). Each compares with last tick's keys cut down
    -- the same way, so a press is fresh for its own keeper only.
    noted = asPartner (notePresses False (keysOf2 prev) in2) (notePresses True (keysOf1 prev) in1 g0)
    -- Stage 2. The first keeper's step or wave advances, then the second's; either
    -- landing may pick up the power cell or light the lamp.
    advanced = asPartner (advanceMotion level) (advanceMotion level noted)
    -- Stage 3, the loss check for both, then the first keeper's part of stage 4: a
    -- waiting undo, echo or shift (an undo or a shift waits for the second keeper to
    -- stand still), then a move.
    started = startNext level in1 (outOfTime level advanced)
    -- Either keeper may light the lamp; the other stops where they were going.
    -- Once the phase is 'Won', 'stepGame' runs no more ticks of play, so a step
    -- still in flight would never land: it is finished here, on its destination.
    -- The keeper who lit the lamp is already at rest ('touchCell'). The case is
    -- written with braces and semicolons, { a; b }, which means the same as putting
    -- each alternative on a line of its own.
    settleOnWin g
      | gPhase g == Won = g
          { gCell = case gMotion g of { Stepping to _ -> to; _ -> gCell g }
          , gMotion = Settled, gPushing = Nothing, gPartner = fmap settled (gPartner g) }
      | otherwise = g

-- | The second keeper's turn: only walking, pushing and jumping. It waits while the
-- era is changing, and while a shift or an undo is waiting for both to stand still.
--
-- It always runs inside 'asPartner', so here "the player" ('gCell', 'gMotion',
-- 'gPendingMove', 'gArmed') is the second keeper and 'gPartner' is the first. @input@
-- is the second keeper's keys ('keysOf2'). Unlike 'startNext' it has no undo, shift
-- or echo of its own and no loss check: those belong to the first keeper's turn.
startMove :: Level -> TickInput -> Game -> Game
startMove level input g
  -- Nothing once the level is won or lost, or while this keeper is still walking.
  | gPhase g /= Playing || gMotion g /= Settled = g
  -- A shared undo or shift waits for both keepers to stand still; starting a step
  -- now would only keep it waiting, for ever with a key held down.
  | gPendingUndo g || gPendingShift g = g
  -- Pattern guards: @Just p <- e@ only succeeds when @e@ matches @Just p@, and binds
  -- @p@ for the rest of the guard; both matches must succeed. Here: the first keeper
  -- has just shifted and the era wave, which locks input, is still crossing the map.
  | Just p <- gPartner g, Shifting _ <- ptMotion p = g
  -- A tap made while busy, now used up, as a fresh press (True): it may push or
  -- jump. Record update binds tighter than function application, so the last
  -- argument is (g { gPendingMove = Nothing }): the tap is cleared on the game
  -- handed to 'tryStep'.
  | Just dir <- gPendingMove g = tryStep level True dir g { gPendingMove = Nothing }
  -- A held key, once armed, keeps walking, but not fresh (False): it never pushes
  -- or jumps.
  | Just dir <- tiDir input, gArmed g = tryStep level False dir g
  | otherwise = g

-- | One simulation tick.
--
-- @level@ is the level being played, @input@ the keys held during this tick (made by
-- 'Game.Input'), and the game is the state after the last tick. 'Game.Run' calls it
-- once a tick, 60 times a second, for the level being played (and while a won
-- level's ending lets the keeper walk); 'Game.Solution' plays whole routes through
-- it as fast as it can, so the solver can never disagree with the game.
--
-- What a tick does depends on the phase. Only 'Playing' runs the rules
-- ('playTick'); the other phases only listen for the keys that leave them. Every
-- phase records this tick's keys in 'gPrev' (the first line), so the next tick can
-- tell a fresh press, down now and up the tick before, from a key held down: a held
-- one-shot key needs releasing before it fires again. Directions can repeat.
stepGame :: Level -> TickInput -> Game -> Game
stepGame level input g = next { gPrev = input }
  where
    prev = gPrev g
    -- Fresh presses: down this tick and up the tick before.
    freshEnter   = tiEnter input   && not (tiEnter prev)
    freshRestart = tiRestart input && not (tiRestart prev)
    freshPause   = tiPause input   && not (tiPause prev)
    -- Only read on the lost screen below; in play 'notePresses' finds a fresh Z
    -- itself.
    freshUndo    = tiUndo input    && not (tiUndo prev)
    freshInstruction = tiInstructions input && not (tiInstructions prev)
    -- A clean start of the level, for the first Enter and for every restart: all of
    -- 'initialGame' (the full budget, no history, nothing waiting, no held key
    -- armed) but the attempt count, which goes up by one. As the first line puts this
    -- tick's keys in 'gPrev', a key held through the restart needs a fresh press.
    begin = (initialGame level) { gPhase = Playing, gAttempt = gAttempt g + 1 }
    -- Each phase's alternative has guards, tried from the top.
    next =  case gPhase g of
      -- 'Game.Run' runs the real title screen itself, with the same rules for I and
      -- Enter; this branch is where the solver and the tests start a level. I shows
      -- or hides the instructions; while they are shown nothing else happens, so
      -- Enter cannot start the level underneath them.
      Menu
        |freshInstruction -> g {gShowInstructions = not (gShowInstructions g)}
        |gShowInstructions g -> g
        | freshEnter   -> begin
        | otherwise    -> g
      -- The only phase in which the rules run. R starts over. P pauses, through
      -- 'quiet', so nothing pressed before the pause fires after it (record update
      -- binds tighter, so this is quiet (g { gPhase = Paused })). A step in flight
      -- stays in flight and goes on after the pause.
      Playing
        | freshRestart -> begin
        | freshPause   -> quiet g { gPhase = Paused }
        | otherwise    -> playTick level input g
      -- No tick of play passes: a step in flight waits where it is, nothing is
      -- spent, and only R and P (to resume) do anything.
      Paused
        | freshRestart -> begin
        | freshPause   -> quiet g { gPhase = Playing }
        | otherwise    -> g
      -- 'Game.Run' sees a fresh Enter on a won level first and goes on to the next
      -- level, so in the game it is R here that plays the level again.
      Won
        | freshEnter || freshRestart -> begin
        | otherwise    -> g
      -- Z still works on the lost screen: 'undo' gives back half the last action's
      -- price, and brings the level back into play if that pays for a step.
      Lost
        | freshEnter || freshRestart -> begin
        | freshUndo    -> undo level g
        | otherwise    -> g

-- | Forget anything waiting to happen. Keys held across this need a fresh press.
--
-- Clears the waiting shift, move, undo and echo, and disarms the held-key repeat
-- ('gArmed'), for both keepers. The board, the motion and the message are left
-- alone. Used on pause and resume, on winning ('touchCell'), on losing ('outOfTime')
-- and on undo, so that nothing pressed before those moments acts after them. The
-- second keeper's fields are cleared with a lambda, @\\p -> ...@, an unnamed
-- function that fmap applies inside the Maybe.
quiet :: Game -> Game
quiet g = g { gPendingShift = False, gPendingMove = Nothing, gPendingUndo = False
            , gPendingEcho = False, gArmed = False
            , gPartner = fmap (\p -> p { ptPendingMove = Nothing, ptArmed = False }) (gPartner g) }

-- | One tick of play, in the order given at the top of the module.
--
-- A level for two goes through 'duoTick'. For one keeper the four stages are
-- nested calls, read from the innermost outwards: 'notePresses' (1),
-- 'advanceMotion' (2), 'outOfTime' (3) and 'startNext' (4). @g0@ is the game at
-- the start of the tick.
playTick :: Level -> TickInput -> Game -> Game
playTick level input g0
  -- There is a second keeper; _ matches them without naming them.
  | Just _ <- gPartner g0 = duoTick level input g0
  | otherwise = startNext level input (outOfTime level (advanceMotion level (notePresses True (gPrev g0) input g0)))

-- | Step 1 for one keeper: fresh presses are remembered and the nudges decay. The
-- era, undo and echo keys and the feedback are shared, so only the first keeper's
-- turn (@shared@) takes them.
--
-- @shared@ is 'True' for the only keeper of a level for one and for the first of
-- two, 'False' for the second. @prev@ is last tick's keys and @input@ this tick's,
-- both already cut down to this keeper's own keys in a level for two. A press made
-- while a step or a wave is under way can wait in the pending fields for
-- 'startNext' or 'startMove'. Only the newest direction is kept; Space during a
-- wave is deliberately dropped, as the locked check below explains.
notePresses :: Bool -> TickInput -> TickInput -> Game -> Game
notePresses shared prev input g0
  -- The first keeper also takes the shared keys. A pending flag stays set once set
  -- (||): pressing again while one waits adds nothing, so at most one shift, one
  -- undo and one echo can be waiting.
  | shared = walking
      { gPendingShift = gPendingShift g0 || (freshShift && not locked)
      , gPendingUndo  = gPendingUndo g0 || freshUndo
      , gPendingEcho  = gPendingEcho g0 || freshEcho
      , gFeedback     = decayFeedback (gFeedback g0)
      }
  | otherwise = walking
  where
    -- Letting go of a newer key shows the older one still held. That keeps a walk
    -- going, through gArmed, but it is not a press: it must not replace a buffered
    -- tap, and after an undo or a restart it must not start a step that costs time.
    -- So a direction is fresh only when it differs from last tick's and a direction
    -- key really went down this tick ('tiDirRevealed' is False). Letting go of every
    -- key also changes 'tiDir' (to Nothing), and is no press either.
    freshDir
      | tiDir input /= tiDir prev && not (tiDirRevealed input) = tiDir input
      | otherwise = Nothing
    freshShift = tiShift input && not (tiShift prev)
    freshUndo  = tiUndo input && not (tiUndo prev)
    freshEcho  = tiEcho input && not (tiEcho prev)
    -- Space pressed during the era wave is dropped, not kept for later, so a second
    -- press during a shift cannot queue a shift straight back. Space pressed during a
    -- step is kept, and shifts once the step lands.
    locked     = case gMotion g0 of
      Shifting _ -> True
      _          -> False
    walking = g0
        -- The newest tap replaces an older one still waiting; with no tap this tick,
        -- the waiting one stays. On Maybe, @a <|> b@ is @a@ if it is a Just, else @b@.
      { gPendingMove  = freshDir <|> gPendingMove g0
        -- A held direction may repeat (walk on, a cell after a cell) only once it has
        -- been pressed in this game: armed by a fresh press, kept while any direction
        -- is held, lost when all are let go. 'quiet' clears it, so a key held through
        -- a restart, an undo or a pause does nothing until it is pressed again.
      , gArmed        = isJust (tiDir input) && (gArmed g0 || isJust freshDir)
        -- The walked-into-a-wall nudge runs down a tick at a time, never below 0.
      , gBump         = max 0 (gBump g0 - 1)
      }

-- | Count a message's ticks down by one, dropping it when they run out.
--
-- A message set with @n@ ticks ('blockedFeedbackTicks', 'hintFeedbackTicks',
-- 'pickupFeedbackTicks') is shown for @n@ ticks, unless something replaces or clears
-- it first. Written as one equation per case: the first matches 'Nothing', the
-- second any @Just fb@, and its guards then look at the ticks left.
decayFeedback :: Maybe Feedback -> Maybe Feedback
decayFeedback Nothing = Nothing
decayFeedback (Just fb)
  | fbLeft fb <= 1 = Nothing
  | otherwise      = Just fb { fbLeft = fbLeft fb - 1 }

-- | Progress the current step or wave by one tick.
--
-- Each motion carries its ticks left. A step lands when that count reaches 1: the
-- keeper's cell becomes the destination, the pushed crate or mirror has finished
-- sliding ('gPushing'), and 'touchCell' sees whether the new cell picks up the power
-- cell or wins. Begun with 'stepTicks' (8), a step lands on the 8th tick after it
-- was accepted, about 0.13 s, and a held key starts the next one in that same tick,
-- so a walk covers a cell every 8 ticks. A wave only unlocks input when it ends: the
-- era itself changed when the shift was accepted.
advanceMotion :: Level -> Game -> Game
advanceMotion level g = case gMotion g of
  Settled -> g
  Shifting left
    | left <= 1 -> g { gMotion = Settled }
    | otherwise -> g { gMotion = Shifting (left - 1) }
  Stepping to left
    | left <= 1 -> touchCell level g { gMotion = Settled, gCell = to, gPushing = Nothing }
    | otherwise -> g { gMotion = Stepping to (left - 1) }

-- | The one place pickup and victory happen. Runs when a step lands and when an
-- era changes, because both put the player in contact with something new.
--
-- A jump is walked like a step, so its landing comes here too; an undo does not
-- ('undo' has its own pickup check). Written point-free: @victory . pickup@ is the
-- function that applies @pickup@ first and then @victory@, so @touchCell level g@
-- is @victory (pickup g)@.
touchCell :: Level -> Game -> Game
touchCell level = victory . pickup
  where
    -- In the past, on the power cell's cradle, while nobody carries it: take it,
    -- and say so for 'pickupFeedbackTicks' (210 ticks, 3.5 s).
    pickup g
      | gEra g == Past && gCell g == lvPowerCell level && not (anyHasCell g) =
          g { gHasCell = True
            , gFeedback = Just (Feedback CellCollected pickupFeedbackTicks) }
      | otherwise = g
    -- In the present, on the lighthouse, carrying the cell: the level is won. Only
    -- the keeper who carries it can win by standing there. Anything waiting is
    -- dropped, the keeper is put at rest (which also ends the era wave at once when
    -- the shift was the winning move) and the message is cleared for the win screen.
    victory g
      | gEra g == Present && gCell g == lvGoal level && gHasCell g =
          (quiet g) { gPhase = Won, gMotion = Settled, gFeedback = Nothing }
      | otherwise = g

-- | Begin whatever is next, if the player is free and the level is still on.
--
-- Stage 4 of a tick, for the only keeper or the first of two. @input@ is this
-- tick's keys (the first keeper's own, in a level for two). What waits is taken in
-- a fixed order: undo, echo, shift, a buffered tap, a held key. An echo or a shift
-- is tried and then startNext runs again on the result, so a refused shift (which
-- leaves the keeper at rest) does not swallow a move waiting behind it, while an
-- accepted shift leaves the keeper 'Shifting' and the second run stops at once. An
-- echo sets no motion, so a move can start in the same tick as an accepted echo.
-- The recursion ends because 'tryEcho' and 'tryShift' always clear their flag.
startNext :: Level -> TickInput -> Game -> Game
startNext level input g
  -- Nothing begins once the level is won or lost, or while a step or a wave is
  -- under way; what was pressed meanwhile stays in the pending fields.
  | gPhase g /= Playing || gMotion g /= Settled = g
  -- With two keepers, an undo or a shift waits for the other one's step to land.
  -- An undo goes first, and nothing else starts in its tick: 'undo' forgets
  -- everything else that was waiting.
  | gPendingUndo g = if partnerBusy g then g else undo level g
  | gPendingEcho g = startNext level input (tryEcho level g)
  | gPendingShift g && partnerBusy g = g
  | gPendingShift g = startNext level input (tryShift level g)
  -- A tap made during the last step, now used up, as a fresh press: it may push or
  -- jump.
  | Just dir <- gPendingMove g =
      tryStep level True dir g { gPendingMove = Nothing }
  -- A held key, once armed, keeps walking. It is not fresh, so it walks up to a
  -- crate, or into a mirror's light, and stops there.
  | Just dir <- tiDir input, gArmed g =
      tryStep level False dir g
  | otherwise = g

-- | The level ends once nothing is affordable. An undo waiting to happen goes first,
-- because it would give time back.
--
-- Stage 3 of a tick. A step is the cheapest action under the shipped rules (a push
-- costs the same; a jump, an echo and a shift cost more), so less than one step's
-- price left means nothing more can be done. The check waits until the keeper is at
-- rest and, with two keepers, until the other one has landed too: a step already
-- paid for may still reach the lamp, and then the win (which has already changed
-- the phase) stands. Losing drops anything waiting and clears the message for the
-- lost screen, where Z still works.
outOfTime :: Level -> Game -> Game
outOfTime level g
  | gPhase g == Playing && gMotion g == Settled && not (partnerBusy g) && not (gPendingUndo g)
    && gRemaining g < ruStepCost (lvRules level) = (quiet g) { gPhase = Lost, gFeedback = Nothing }
  | otherwise = g

-- | Pay for an action, remembering the board as it stood so undo can restore it.
--
-- @cost@ is in seconds of budget. @g@ is the game just before the action changes
-- anything, so the 'Snapshot' holds the board as it was. Callers check first that
-- the time is there, so the budget never goes below zero. The snapshot also keeps
-- the price, so 'undo' knows how much to give back, and the second keeper as they
-- were (whatever their motion), so undo can put them back too. Every accepted step,
-- push, jump, shift and echo comes through here, and nothing else does.
charge :: Int -> Game -> Game
charge cost g = g
  { gRemaining = gRemaining g - cost
  , gHistory = before : gHistory g
  , gFeedback = case gFeedback g of
      -- Advice about pushing has done its job once the player moves.
      -- The four messages about pressing against a crate or a mirror are cleared;
      -- any other (a refused shift, the pickup note, the advice to press again to
      -- jump) keeps counting down. Backticks make a two-argument function infix:
      -- @kind `elem` xs@ is @elem kind xs@, whether @kind@ is in the list @xs@.
      Just (Feedback kind _) | kind `elem` [PushNeedsPress, CrateRusted, MirrorFaceSide, MirrorFixed] -> Nothing
      other -> other
  }
  where
    -- Everything an action can change, as it was before it.
    before = Snapshot
      { snEra = gEra g, snCell = gCell g, snHasCell = gHasCell g
      , snCrates = gCrates g, snFilled = gFilled g
      , snFacing = gFacing g, snTrail = gTrail g, snEcho = gEcho g
      , snMirrors = gMirrors g, snCost = cost, snPartner = gPartner g }

-- | Note where the player now stands, keeping only as many footprints as the level
-- allows. A level with no echo keeps none, so its states stay as few as before.
--
-- @spot@ is the era and cell an action leaves the player on: a step's or a push's
-- destination, a jump's landing, or the same cell in the new era after a shift. The
-- trail is newest first, so 'take' keeps the newest 'ruEchoLength' of them. The trail
-- is part of the solver's state key ('Game.Solution'), so footprints kept in a level
-- without the echo would multiply the states it has to search.
footprint :: Level -> Spot -> Game -> Game
footprint level spot g = g { gTrail = take (ruEchoLength (lvRules level)) (spot : gTrail g) }

-- | Rewind one accepted action. The board comes back exactly, the time only in
-- part, so undoing can never beat having played well. It also cancels anything
-- buffered, and a held direction needs a fresh press, or it would walk straight
-- back into the mistake.
--
-- The newest snapshot comes off the history and the board is put back from it: the
-- era, where the keeper stands and faces, whether they carry the power cell, the
-- crates, the filled pits, the footprints, the echo, the mirrors and the second
-- keeper. The time comes back as 'ruUndoRefundPercent' (50 in 'baseRules') of the
-- action's price, rounded down; the prices are even, so that is whole seconds. The
-- phase is worked out again from the time left, so an undo from the lost screen can
-- bring the level back into play, or leave it lost if a step is still out of reach.
-- With an empty history there is nothing to rewind and only the waiting keys are
-- forgotten. Called by 'startNext' for a waiting Z and by 'stepGame' on the lost
-- screen, always with the keeper at rest.
undo :: Level -> Game -> Game
undo level g = case gHistory g of
  [] -> quiet g
  -- The board put back, then each keeper checked against the cradle: the first
  -- directly, the second through 'asPartner'.
  (s : rest) -> asPartner pickedUp (pickedUp (restore s rest))
  where
    -- With two keepers, the one not being undone may have picked up the cell since
    -- the snapshot; standing on the cradle again, they hold it again.
    pickedUp h
      | gEra h == Past && gCell h == lvPowerCell level && not (anyHasCell h) = h { gHasCell = True }
      | otherwise = h
    restore s rest =
        let rules = lvRules level
            -- Both * and `div` bind tighter than +, and group to the left, so this
            -- is gRemaining g + ((snCost s * percent) `div` 100). Multiplying first
            -- matters with whole numbers: 50 `div` 100 alone would be 0.
            remaining = gRemaining g + snCost s * ruUndoRefundPercent rules `div` 100
        in (quiet g)
             { gEra = snEra s, gCell = snCell s, gHasCell = snHasCell s
             , gCrates = snCrates s, gFilled = snFilled s, gFacing = snFacing s
             , gTrail = snTrail s, gEcho = snEcho s, gMirrors = snMirrors s
             , gRemaining = remaining, gHistory = rest
             , gMotion = Settled, gPushing = Nothing, gFeedback = Nothing, gBump = 0
               -- The second keeper as they stood, with any step they had in flight
               -- finished.
             , gPartner = fmap settled (snPartner s)
             , gPhase = if remaining < ruStepCost rules then Lost else Playing
             }

-- | Switch era if this cell is open in the other one and the time is there. Only a
-- settled player can shift. A refusal consumes the pending shift and sets
-- feedback, but spends no time, so testing a cell is never punished.
--
-- A shift is accepted when the player's cell is open in the other era (no wall,
-- crate or mirror there; see 'openCell'), with two keepers the other keeper's cell
-- too, and the budget holds 'ruShiftCost' (6 seconds in 'baseRules'). It is then paid
-- for, leaves a footprint in the new era, flips 'gEra' at once and starts the era
-- wave, which only locks input for 'shiftTicks' (13 ticks, about 0.22 s) while it is
-- drawn crossing the map. Arriving in the other era is contact with something new,
-- so 'touchCell' runs for each keeper: shifting into the past on the cradle picks up
-- the power cell, shifting into the present on the lamp with it wins. A refusal
-- leaves a message ('ShiftBlocked' with the cell, or 'ShiftTooCostly') for
-- 'blockedFeedbackTicks' (96 ticks, 1.6 s). Called by 'startNext' and the tests.
tryShift :: Level -> Game -> Game
tryShift level g
  -- Only from rest. 'startNext' never calls it otherwise; this keeps a direct call
  -- safe.
  | gMotion g /= Settled = g
  | not (openCell level g destination (gCell g)) =
      refuse (ShiftBlocked (gCell g))
  -- The era is shared: both keepers go, so both need somewhere to stand.
  -- 'openCell' is asked of the swapped game: asked of @g@ itself, the second
  -- keeper's cell would be "the other keeper's cell" and always count as taken.
  | Just p <- gPartner g, not (openCell level (swapKeepers g) destination (ptCell p)) =
      refuse (ShiftBlocked (ptCell p))
  | gRemaining g < cost =
      refuse ShiftTooCostly
  -- @f $ x@ is @f x@ at the lowest precedence, which saves brackets around all that
  -- follows. Read the accepted shift from the inside out: 'charge' on the cleared
  -- game (a snapshot of the old era, then the price), the footprint, then the record
  -- update with the new era, the wave and the message, and only then 'touchCell',
  -- for the first keeper and then (through 'asPartner') for the second. Record update
  -- binds tighter than function application, so the update applies to the bracketed
  -- game before 'touchCell' sees it, and 'touchCell' sees the new era.
  | otherwise = asPartner (touchCell level) $
      touchCell level (footprint level (destination, gCell g) (charge cost cleared))
        { gEra      = destination
        , gMotion   = Shifting shiftTicks
        , gFeedback = keepPickupNote (gFeedback g)
        }
  where
    cost = ruShiftCost (lvRules level)
    destination = otherEra (gEra g)
    -- The press is used up whatever happens.
    cleared = g { gPendingShift = False }
    refuse kind = cleared { gFeedback = Just (Feedback kind blockedFeedbackTicks) }
    -- The pickup note ("The power cell travels with you") outlives a shift; any
    -- other message, such as an earlier refusal, is cleared by a shift accepted.
    keepPickupNote fb = case fb of
      Just (Feedback CellCollected _) -> fb
      _                               -> Nothing

-- | Whether the player could stand here right now: the era's map, plus any pits
-- filled so far, minus wherever the crates are and this era's mirrors. A crate has
-- one position, so it blocks that cell in both eras; a mirror only in its own.
--
-- @era@ is the era asked about, which need not be the player's own ('tryShift' asks
-- about the other one). Nothing about which cells are open is stored: the answer is
-- worked out from the level and the game each time. app/Render.hs asks it too, for
-- the glow of the doorways between rooms, so the drawing cannot disagree.
openCell :: Level -> Game -> Era -> Cell -> Bool
openCell level g era cell = clearOfPartner && clearCell level g era cell
  where
    -- Two keepers never share a cell, or the one the other is stepping onto.
    -- So neither the other keeper's cell nor their destination ('landedAt') is
    -- open, and two steps can never end on one cell.
    clearOfPartner = case gPartner g of
      Just p -> cell /= ptCell p && cell /= landedAt p
      Nothing -> True

-- | 'openCell' without the other keeper: a keeper does not stop light.
--
-- The three tests are joined with && so the cheap ones decide first: Haskell's &&
-- and || only work out their right side when the left has not settled the answer.
-- That matters for the door test, the dearest, which builds the set of 'weights': it
-- is only worked out for a door in the present that nothing before it made open.
clearCell :: Level -> Game -> Era -> Cell -> Bool
clearCell level g era cell =
  cell `Set.notMember` gCrates g && not (isJust (mirrorAt g era cell)) && (onMap || filled || door)
  where
    -- Floor on this era's own map. Walls, pits, doors and mirrors set in the wall
    -- are not floor on the map they are drawn on; a crate or a movable mirror stands
    -- on floor, and is ruled out above instead.
    onMap = walkable level era cell
    -- A pit a crate has dropped into is floor in the past from then on, whatever the
    -- present has there: a bridge, if the present is a wall.
    filled = era == Past && cell `Set.member` gFilled g
    -- A door is a wall in the present until every plate is held down. Doors are
    -- only ever drawn on the present map.
    door = era == Present && cell `Set.member` lvDoors level && doorsOpen level (weights g)

-- | Every cell the player could switch era on right now: open in both eras, given
-- where the crates, filled pits and open doors are. Worked out from the map and the
-- board, so it can be drawn but never has to be kept up to date.
--
-- app/Render.hs marks the cell under a keeper's feet when it is in this list, and the
-- tests check it. A list comprehension: @y <- ...@ and @x <- ...@ are generators,
-- the later one varying fastest, so the cells come row by row from the top, left to
-- right; @let@ names a value inside it; the last line holds filters, and a cell is
-- kept only when both are True. It asks 'openCell' twice for every cell of the map.
crossings :: Level -> Game -> [Cell]
crossings level g =
  [ c | y <- [0 .. lvHeight level - 1], x <- [0 .. lvWidth level - 1], let c = (x, y)
      , openCell level g Past c, openCell level g Present c ]

-- | The doors open while something rests on every plate. This is worked out from
-- where things are, not stored, so undo, restart and the solver need nothing new, and
-- a door can never disagree with its plates. Nothing can leave a plate while the
-- player stands in a door: crates only move while the player is in the past, the
-- player is not on a plate while in a door, and the echo cannot be sent from one.
--
-- That argument is for one keeper: with two, the other keeper could walk off a plate
-- while one stands in a door. The one level for two has no plates.
--
-- @held@ is the set of cells something rests on, normally @'weights' g@. All the
-- doors of a level open and shut together. A level without plates has no doors
-- either (the parser insists on both or neither), and there this is 'False'.
-- @a `Set.isSubsetOf` b@ asks whether every element of @a@ is in @b@.
doorsOpen :: Level -> Set.Set Cell -> Bool
doorsOpen level held = not (Set.null plates) && plates `Set.isSubsetOf` held
  where plates = lvPlates level

-- Mirrors ----------------------------------------------------------------------

-- | Whether light crosses this cell: clear floor or any past-map pit.
--
-- Keepers and the echo never stop light (see 'clearCell'). Walls, crates, shut
-- doors and mirrors normally do. The pit exception is unconditional: it also
-- passes light through a crate standing on an already filled pit. That is a rule
-- bug, left unchanged here. Both 'beamsOf' and 'mirrorAhead' use this predicate,
-- so the drawn beams and the available jumps share that behaviour.
lightPasses :: Level -> Game -> Era -> Cell -> Bool
lightPasses level g era cell =
  clearCell level g era cell || (era == Past && cell `Set.member` lvPits level)

-- | The light a mirror throws out of each face, up to whatever stops it. Only in the
-- mirror's own era. A corner mirror throws two.
--
-- @mirror@ is the cell to look at; a cell with no mirror gives []. Each beam is the
-- face it leaves by and the cells it lights, nearest first, up to but not including
-- the first cell that stops it. It is worked out on the board of the mirror's own era
-- ('mEra'), whatever era the player is in; app/Render.hs decides when to show it.
-- 'Map.lookup' gives @Just@ the mirror stored on a cell, or 'Nothing'.
beamsOf :: Level -> Game -> Cell -> [(Dir, [Cell])]
beamsOf level g mirror = case Map.lookup mirror (gMirrors g) of
  -- @iterate (stepCell face) mirror@ is the endless list: the mirror's cell, the
  -- next cell that way, the one after, and so on. @drop 1@ leaves out the mirror's
  -- own cell and 'takeWhile' stops at the first cell light does not cross. Laziness
  -- makes the endless list safe: only the cells 'takeWhile' looks at are ever built,
  -- and a level is sealed by walls, so it always stops.
  Just m -> [ (face, takeWhile (lightPasses level g (mEra m)) (drop 1 (iterate (stepCell face) mirror)))
            | face <- mirrorFaces m ]
  Nothing -> []

-- | Every cell a mirror lights.
--
-- The cells of all its beams in one list: 'snd' takes the cells of each
-- (face, cells) pair and 'concatMap' joins them. Used by the tests.
beamCells :: Level -> Game -> Cell -> [Cell]
beamCells level g mirror = concatMap snd (beamsOf level g mirror)

-- | The mirror standing on this cell in this era, if there is one.
--
-- A mirror stands in one era only; in the other, its cell is whatever that era's map
-- says. The guard on the case alternative keeps the mirror found only when it is in
-- @era@. Used by the rules ('clearCell', 'mirrorAhead', 'tryStep') and by
-- app/Render.hs for the push hint.
mirrorAt :: Game -> Era -> Cell -> Maybe Mirror
mirrorAt g era cell = case Map.lookup cell (gMirrors g) of
  Just m | mEra m == era -> Just m
  _ -> Nothing

-- | The way back: down for up, right for left. A mirror's face looks back at a
-- player moving @d@ when it faces @opposite d@.
opposite :: Dir -> Dir
opposite d = case d of
  DirUp -> DirDown
  DirDown -> DirUp
  DirLeft -> DirRight
  DirRight -> DirLeft

-- | A mirror the player stands in the light of, facing it.
--
-- What 'mirrorAhead' finds. The fields are strict (the ! before each type): they are
-- worked out when a Jump is built, rather than kept as unevaluated expressions.
-- @deriving (Eq, Show)@ has the compiler write == and 'show' for the type, so jumps
-- can be compared and printed.
data Jump = Jump
  { jMirror :: !Cell  -- ^ the cell the mirror stands on
  , jTarget :: !Cell  -- ^ as far behind the mirror as the player stands in front of it,
                      --   or for a corner mirror, as far out of its other face
  , jOpen   :: !Bool  -- ^ whether the player could stand there, so the jump would happen
  } deriving (Eq, Show)

-- | Looking along @dir@ from the player: if the light reaches a mirror in this era
-- with a face looking back at them, that mirror and where it would put them. The
-- player is in its light exactly when nothing stands between them. A flat mirror
-- puts them behind it; a corner mirror turns them out of its other face.
--
-- @dir@ is the way the key points. The look starts at the cell next to the player
-- and walks on while light passes; the first cell that stops the light decides. If
-- it holds a mirror of this era with a face towards the player, the player stands in
-- that face's light. Anything else that stops the light (a wall, a crate, a shut
-- door, the back or side of a mirror) means no jump; a mirror of the other era is
-- not there in this one. The landing need not be open: 'jOpen' says whether it is,
-- so the drawing can show a red cross. This one function is behind the light drawn,
-- the reflection shown and the jump taken ('jumpTarget', used by 'tryStep'), so they
-- cannot disagree; app/Hud.hs and app/Render.hs ask it for each direction.
mirrorAhead :: Level -> Game -> Dir -> Maybe Jump
mirrorAhead level g dir = look (1 :: Int) (stepCell dir (gCell g))
  where
    era = gEra g
    -- @n@ is how far cell @c@ is from the player: 1 next to them. (1 :: Int) above
    -- is a type annotation on the literal. The level is sealed by walls, so the look
    -- always ends.
    look n c
      | lightPasses level g era c = look (n + 1) (stepCell dir c)
      | otherwise = case mirrorAt g era c of
          -- The landing is @n@ cells from the mirror, the way 'exitFace' gives:
          -- straight on for a flat mirror, so as far behind it as the player stands
          -- in front, or out of the other face for a corner mirror. @xs !! n@ is the
          -- element at index @n@ of a list, counting from 0.
          Just m | opposite dir `elem` mirrorFaces m ->
            let target = iterate (stepCell (exitFace m dir)) c !! n
            in Just (Jump c target (openCell level g era target))
          _ -> Nothing

-- | Where pressing @dir@ would jump the player to, if it would.
--
-- 'Just' the landing when the player stands in a mirror's light, facing it, and the
-- landing is open; 'Nothing' otherwise, and then the key does whatever it would do
-- without the mirror. 'tryStep' asks this before anything else.
jumpTarget :: Level -> Game -> Dir -> Maybe Cell
jumpTarget level g dir = case mirrorAhead level g dir of
  Just j | jOpen j -> Just (jTarget j)
  _ -> Nothing

-- | Every cell something is standing on: the crates, the player and the echo. A plate
-- is set in the floor of both eras, so it does not matter which era they are in.
--
-- With two keepers, the second keeper's cell too. Built from the inside out: the
-- crates, then the echo's cell, then the second keeper's, then the player's.
-- @maybe id f m@ is the identity function, adding nothing, when @m@ is 'Nothing', and
-- @f@ applied to what @m@ holds otherwise; @Set.insert . snd@ takes the cell out of
-- the echo's (era, cell) and inserts it. A keeper in the middle of a step still
-- weighs on the cell they are leaving, since 'gCell' and 'ptCell' only change when
-- the step lands. Used for 'doorsOpen' (through 'clearCell' and 'currentHint') and
-- by app/Render.hs and app/Hud.hs to draw the plates and doors.
weights :: Game -> Set.Set Cell
weights g = Set.insert (gCell g) (maybe id (Set.insert . ptCell) (gPartner g)
                                    (maybe id (Set.insert . snd) (gEcho g) (gCrates g)))

-- | The way an echo sent now would walk: from the player's spot back along their
-- footprints, oldest last, stopping before any footprint that is no longer open,
-- such as one a crate has since been pushed onto. Shared by the rule and the drawing
-- of where it would land, so the preview cannot disagree with what happens.
--
-- The first spot, the player's own, is always kept. Each footprint is checked in its
-- own era. Empty in a level without the echo, which keeps no footprints. Used by
-- 'echoLanding' and the tests.
echoPath :: Level -> Game -> [Spot]
echoPath level g = case gTrail g of
  here : older -> here : takeWhile free older
  [] -> []
  where
    free (era, cell) = openCell level g era cell

-- | Where an echo sent now would stand, if there is anywhere to walk back to.
--
-- The pattern @path\@(_ : _ : _)@ matches a list of at least two spots and names the
-- whole list @path@ (an as-pattern): the player's own spot and at least one footprint
-- to walk back to. With the player's spot alone there is nowhere to go. Used by
-- 'tryEcho', and by app/Render.hs to ring the footprint the echo would stop on.
echoLanding :: Level -> Game -> Maybe Spot
echoLanding level g = case echoPath level g of
  path@(_ : _ : _) -> Just (last path)
  _ -> Nothing

-- | Send the echo: it walks the footprints back and stays where they end, until the
-- next one replaces it. The footprints start again from here. Refused for free, with
-- a reason, if there is nothing to walk back along, or from a doorway, where the old
-- echo leaving its plate could shut the door on the player.
--
-- The echo is placed at once: it sets no motion and takes no ticks. It costs
-- 'ruEchoCost' (4 seconds in 'baseRules') and is charged like any action, so undo
-- takes it back for half. Only 'startNext' calls it, for a waiting E.
tryEcho :: Level -> Game -> Game
tryEcho level g
  | gMotion g /= Settled = g
  -- This level has no echo: E does nothing at all, and says nothing.
  | ruEchoLength rules <= 0 = cleared
  -- A pattern guard can match a constant too: this one only matches 'Nothing', when
  -- there is no footprint to walk back to.
  | Nothing <- landing = told EchoNeedsSteps
  -- Standing in a door: a new echo would lift the old one off its plate, and the
  -- door could shut with the player inside.
  | gEra g == Present && gCell g `Set.member` lvDoors level = told EchoInDoorway
  -- Not enough time: refused without a word, for free.
  | gRemaining g < cost = cleared
  -- The old echo, if any, is replaced. @landing@ is a 'Just' here, since 'Nothing'
  -- was refused above. The trail starts again from the player's spot, so the next
  -- echo only walks back over what comes after this one.
  | otherwise = (charge cost cleared)
      { gEcho = landing
      , gTrail = take (ruEchoLength rules) [(gEra g, gCell g)]
      }
  where
    rules = lvRules level
    cost = ruEchoCost rules
    landing = echoLanding level g
    -- The press is used up whatever happens.
    cleared = g { gPendingEcho = False }
    -- Refused with a reason, shown for 'hintFeedbackTicks' (120 ticks, 2 s).
    told kind = cleared { gFeedback = Just (Feedback kind hintFeedbackTicks) }

-- | Start one step if the destination is open, or if it holds a crate that can be
-- pushed. Everything is checked here, before anything moves, and never again. The
-- crate moves when the step is accepted, like the charge, so no tick can see the
-- board half done. Only a fresh press pushes: holding a key walks up to a crate and
-- stops, so a walk can never shove one into a dead end. A crate exists in both eras,
-- so it can only go where both have room: never into a wall in the present. A pit
-- is the exception, because the crate is used up there and nothing is left standing.
--
-- @isFresh@ is 'True' for a key pressed this tick or a tap kept from the last step,
-- 'False' for a held key repeating. @dir@ is the way the key points. The guards are
-- tried in order of precedence: a jump on offer beats everything, then a mirror on
-- the next cell, then a crate, then anything else solid, then the time, then an
-- ordinary step. Every refusal is free: the keeper only turns to face @dir@, and a
-- fresh press into something solid also starts the bump nudge ('bumpTicks', 8
-- ticks). An accepted step, push or jump is charged at once and starts a 'Stepping'
-- motion of 'stepTicks' (8) ticks; the keeper's cell only changes when it lands
-- ('advanceMotion'). app/Render.hs also calls it, with @isFresh@ True, on a copy of
-- the game to see where a movable mirror would slide (the push hint), so the hint
-- can never offer a push this function would refuse.
tryStep :: Level -> Bool -> Dir -> Game -> Game
tryStep level isFresh dir g
  | gMotion g /= Settled = g
  | Just target <- jumpTarget level g dir = jumpTo target
  -- A mirror of this era on the next cell.
  | Just m <- mirrorAt g (gEra g) to = pushMirror m
  -- A crate on the next cell, in either era: a crate stands in both.
  | to `Set.member` gCrates g = pushOrRefuse
  -- A wall, a shut door, an open pit or the other keeper.
  | not (openCell level g (gEra g) to) = refused
  -- Not enough time for a step: the keeper only turns. This can happen even after
  -- 'outOfTime' has let the tick go on: an echo accepted earlier in the same tick,
  -- or the other keeper in a level for two, may have spent the time.
  | gRemaining g < cost = g { gFacing = dir }
  | otherwise = (walked (charge cost g)) { gFacing = dir, gMotion = Stepping to stepTicks }
  where
    -- The footprint of the cell walked onto: a partly applied function, still
    -- waiting for the game to put it on.
    walked = footprint level (gEra g, to)
    -- Facing a mirror along its light, with somewhere to land: the key steps through
    -- it instead of walking. Like a push, only a fresh press does, so holding a key
    -- walks up into the light and stops there.
    -- A held key gets a message ('JumpNeedsPress') and no bump. The jump costs
    -- 'ruJumpCost' (4 seconds in 'baseRules') however far it carries, leaves its
    -- footprint on the landing and is walked like a step: 'Stepping' to the landing,
    -- which arrives after 'stepTicks' and is touched like any step's.
    jumpTo target
      | not isFresh = g { gFacing = dir, gFeedback = Just (Feedback JumpNeedsPress hintFeedbackTicks) }
      | gRemaining g < ruJumpCost (lvRules level) = g { gFacing = dir }
      | otherwise = (footprint level (gEra g, target) (charge (ruJumpCost (lvRules level)) g))
          { gFacing = dir, gMotion = Stepping target stepTicks }
    -- A step's price; a push costs the same.
    cost = ruStepCost (lvRules level)
    -- The cell the key points at, and the one after it, where a pushed crate or
    -- mirror would go.
    to = stepCell dir (gCell g)
    beyond = stepCell dir to
    -- Refused: the keeper turns to face @dir@. A fresh press bumps for 'bumpTicks';
    -- a held key against something solid just stands there, its bump running down.
    refused = g { gFacing = dir, gBump = if isFresh then bumpTicks else gBump g }
    -- The cell beyond is a pit not yet filled: a crate pushed there drops in.
    pit = beyond `Set.member` lvPits level && beyond `Set.notMember` gFilled g
    -- Room for a crate in the past: open there right now (floor or a filled pit,
    -- with no crate, past mirror or other keeper on it).
    freeInPast = openCell level g Past beyond
    -- Room for a crate in the present, where it also stands: floor on the present
    -- map, with no present mirror. 'walkable' counts a door as solid even when it is
    -- open, so a crate never goes into a door. Crates and keepers are ruled out by
    -- freeInPast already: a crate is in both eras, and a push only happens in the
    -- past, where both keepers are.
    freeInPresent = walkable level Present beyond && not (isJust (mirrorAt g Present beyond))
    -- The crates, with the pushed one lifted off its cell.
    moved = Set.delete to (gCrates g)
    -- Refused, with a message saying why, for 'hintFeedbackTicks' (120 ticks, 2 s).
    hinted kind = refused { gFeedback = Just (Feedback kind hintFeedbackTicks) }
    pushOrRefuse
      -- In the present every crate is rusted solid. Only a fresh press says so.
      | gEra g /= Past = if isFresh then hinted CrateRusted else refused
      -- A held key walks up to a crate and stops; if a fresh press would push it,
      -- the message says so.
      | not isFresh = if pit || (freeInPast && freeInPresent) then hinted PushNeedsPress else refused
      -- No room in the past: a wall, another crate, a mirror or the other keeper.
      | not pit && not freeInPast = refused
      -- Room in the past but none in the present: a plate on the cell beyond says
      -- so, for 'blockedFeedbackTicks' (96 ticks, 1.6 s).
      | not pit && not freeInPresent =
          refused { gFeedback = Just (Feedback (PushBlocked beyond) blockedFeedbackTicks) }
      | gRemaining g < cost = g { gFacing = dir }
      -- The push: the step into the crate's old cell is charged and footprinted, and
      -- the crate moves now, not when the step lands; 'gPushing' remembers where it
      -- went so the drawing can slide it during the step. Into a pit the crate is
      -- used up and the pit becomes floor ('gFilled'); otherwise it stands beyond.
      | otherwise = (walked (charge cost g))
          { gFacing  = dir
          , gMotion  = Stepping to stepTicks
          , gPushing = Just beyond
          , gCrates  = if pit then moved else Set.insert beyond moved
          , gFilled  = if pit then Set.insert beyond (gFilled g) else gFilled g
          }
    -- A mirror is a door from its face and a crate from its back and sides. Pressing
    -- into its face only ever jumps, which was tried above, so here it explains why
    -- nothing happened. It moves in its own era, onto floor open there: never into a
    -- pit or a door, open or shut, like a crate.
    pushMirror m
      -- A mirror set in the wall, every corner mirror among them, never moves.
      | not (mMovable m) = if isFresh then hinted MirrorFixed else refused
      -- Into its face: the jump was not on offer (its landing is taken), and a face
      -- never pushes.
      | opposite dir `elem` mirrorFaces m = if isFresh then hinted MirrorFaceSide else refused
      -- No room beyond: refused without a message.
      | not mirrorRoom = refused
      -- A held key walks up to it and stops. The message is the crate's kind,
      -- 'PushNeedsPress'.
      | not isFresh = hinted PushNeedsPress
      | gRemaining g < cost = g { gFacing = dir }
      -- The push, charged and footprinted like a step. The mirror moves now, keeping
      -- its facing (it is the same Mirror value): 'Map.delete' lifts it off its old
      -- cell and 'Map.insert' sets it on the cell beyond. 'gPushing' lets the
      -- drawing slide it during the step.
      | otherwise = (walked (charge cost g))
          { gFacing  = dir
          , gMotion  = Stepping to stepTicks
          , gPushing = Just beyond
          , gMirrors = Map.insert beyond m (Map.delete to (gMirrors g))
          }
    -- Mirrors never share a cell, even across eras: one cell holds one mirror.
    -- 'walkable' rules out walls, doors (open or shut) and pits (filled or not), which
    -- 'openCell' alone would let through in part; 'openCell' rules out crates, this
    -- era's mirrors and the other keeper; the last test, a mirror of either era.
    mirrorRoom = walkable level (gEra g) beyond && openCell level g (gEra g) beyond
                 && beyond `Map.notMember` gMirrors g

-- Words ------------------------------------------------------------------------
--
-- Every sentence the rules give the screen, kept beside the rules they explain.
-- app/Render.hs shows the hint line under the map; app/Hud.hs draws the refusal
-- plates and the won and lost screens.

-- | Something worth saying about where the player stands, shown on the hint line
-- when no timed message is showing (see 'currentHint'). An enumeration: each
-- constructor has no fields. Deriving 'Eq' lets the tests compare hints with ==.
data Hint
  = HintLampDark       -- ^ on the lighthouse without the cell
  | HintOnlyFloor      -- ^ on the lighthouse's footing in the past, with the cell
  | HintPlateHeld      -- ^ the player's own weight is what holds the door open
  deriving (Eq, Show)

-- | A hint from where the player stands. Never the route.
--
-- Only while playing, and about the first keeper alone in a level for two. The
-- guards are tried in order and the first that holds decides. Used by app/Render.hs
-- when no timed message is showing, and by the tests.
currentHint :: Level -> Game -> Maybe Hint
currentHint level g
  | gPhase g /= Playing = Nothing
  -- On the lamp in the present without the power cell: it cannot be lit yet.
  | gEra g == Present && here == lvGoal level && not (gHasCell g) = Just HintLampDark
  -- On the lamp's cell in the past with the cell: the lamp is in the present.
  | gEra g == Past && here == lvGoal level && gHasCell g = Just HintOnlyFloor
  -- The doors are open, and would shut without the player's own weight: walking off
  -- will close them. @withoutPlayer@ is used infix, in backticks, like 'elem' above.
  | doorsOpen level (weights g) && not (doorsOpen level (weights g `withoutPlayer` here))
      = Just HintPlateHeld
  | otherwise = Nothing
  where
    here = gCell g
    -- The player's cell, unless a crate or the echo is standing there too.
    -- (The echo blocks nobody, so it can share the player's cell.) @held@ is the set
    -- of weighted cells and @c@ the player's cell.
    withoutPlayer held c
      | c `Set.member` gCrates g || fmap snd (gEcho g) == Just c = held
      | otherwise = Set.delete c held

-- | The words for each hint, one equation per constructor.
hintText :: Hint -> String
hintText HintLampDark      = "The lamp is dark. It has nothing to burn."
hintText HintOnlyFloor     = "Only a footing, back then. The lamp stands in the present."
hintText HintPlateHeld     = plateHeldText

-- | What the player is trying to do now, in one sentence: find the power cell, bring
-- it home, or nothing more, once the lamp is lit. Checked by the tests; nothing on
-- screen shows it at present.
objectiveText :: Game -> String
objectiveText g
  | gPhase g == Won = "The lighthouse is lit."
  | anyHasCell g = "Bring it to the lighthouse in the present."
  | otherwise  = "Find the power cell in the past."

-- | Heading of the plate on a cell where a shift was refused because the cell is not
-- open in the other era ('ShiftBlocked'). Drawn by app/Hud.hs.
blockedTitle :: String
blockedTitle = "BLOCKED IN THE OTHER ERA"

-- | The era the player is trying to reach, in words.
--
-- The line under 'blockedTitle'. @era@ is the era the refused shift would have gone
-- to; app/Hud.hs passes the other era than the one the player is in.
blockedDetail :: Era -> String
blockedDetail Past    = "In the past, there is no standing here."
blockedDetail Present = "In the present, there is no standing here."

-- | Heading of the plate shown when a shift is refused for want of time
-- ('ShiftTooCostly').
tooCostlyTitle :: String
tooCostlyTitle = "NOT ENOUGH TIME"

-- | The line under 'tooCostlyTitle': the shift's price, read from the level's rules
-- so the words always match what is charged. 'show' turns the number into text.
tooCostlyDetail :: Rules -> String
tooCostlyDetail rules = "A shift costs " ++ show (ruShiftCost rules) ++ " seconds."

-- | Heading of the plate on the cell a crate could not be pushed onto, because the
-- present has no room there ('PushBlocked').
pushBlockedTitle :: String
pushBlockedTitle = "THE CRATE WILL NOT FIT"

-- | The line under 'pushBlockedTitle'.
pushBlockedDetail :: String
pushBlockedDetail = "In the present, there is a wall here."

-- | Hint line for 'PushNeedsPress': a held key has walked up to something a fresh
-- press would push.
pushAgainText :: String
pushAgainText = "Press the key again to push the crate."

-- | Hint line for 'CrateRusted': a crate was pressed against in the present.
rustedText :: String
rustedText = "Rusted solid here. It only moves in the past."

-- | Hint line once the board is in a state the level can no longer be won from: one
-- of the dead states 'Game.Solution' lists for the level. The asterisk, with spaces
-- either side, separates the two ways out.
deadEndText :: String
deadEndText = "This can no longer be won.   Z rewinds   *   R starts over"

-- | Hint line while Tab is held to look at the other era. @era@ is the era being
-- shown, which app/Render.hs passes (the other era than the player's).
peekText :: Era -> String
peekText Past    = "Looking back at the past. Let go of Tab to return."
peekText Present = "Looking ahead to the present. Let go of Tab to return."

-- | Hint line for a while after the power cell is picked up ('CellCollected').
pickupText :: String
pickupText = "The power cell travels with you."

-- | Hint line for 'EchoNeedsSteps': E with no footprint to walk back to.
echoNeedsStepsText :: String
echoNeedsStepsText = "Walk first. The echo retraces your last steps."

-- | Hint line for 'EchoInDoorway': E while standing in a door.
echoInDoorwayText :: String
echoInDoorwayText = "Not from a doorway. The door would close on you."

-- | Hint line for 'JumpNeedsPress': a held key has walked into a mirror's light.
jumpAgainText :: String
jumpAgainText = "Press the key again to step through the mirror."

-- | Hint line for 'MirrorFaceSide': pressed into a mirror's face with its landing
-- taken.
mirrorFaceText :: String
mirrorFaceText = "A mirror's face only takes you through. Push it from the back or sides."

-- | Hint line for 'MirrorFixed': pressed against a mirror set in the wall.
mirrorFixedText :: String
mirrorFixedText = "This mirror is set in the wall. It never moves."

-- | Said while the player alone holds every plate down.
-- The words for 'HintPlateHeld'.
plateHeldText :: String
plateHeldText = "The door is open. But you cannot stay here."

-- | Headlines of the won and lost screens, drawn by app/Hud.hs.
victoryText, failureText :: String
victoryText = "You brought the light back"
failureText = "The light stayed out"

