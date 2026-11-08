-- | Decorative state: pure, but nothing in the rules reads it.
--
-- It is derived by comparing each tick's game with the one before, so effects
-- cannot disagree with what actually happened. Restarting clears all of it, but for
-- 'fxFocus', so a restart keeps the closer view of the board.
--
-- What lives here: everything the screen animates that the rules have no need to
-- remember. The era wave after a shift, the flash when the power cell is picked up,
-- the price rising off the keeper, the echo walking back along the footprints, the
-- darkening while the screen moves to another room, the clocks that drive ambient
-- animation (the sea, the carried light, the pause and end screens), and how far
-- the screen has closed in on the board since the level was introduced. Keeping all
-- this out of 'Game' keeps the rules, the undo history and the solver's states free
-- of anything that is only for show.
--
-- Data flow, once per tick: @app/Main.hs@ runs 'Game.Run.stepRun' to get the next
-- run, then 'stepFx' with the game before the tick, the game after it, and the
-- current 'Fx'. 'stepFx' never sees the keys. Anything it shows must be visible as
-- a difference between the two games: the era changed, so there was a shift; the
-- time left changed, so an action was paid for or undone; the history got shorter,
-- so it was an undo. Main also sets 'fxFocus' back to 0 when a level is entered,
-- because 'stepFx' cannot tell entering a level from restarting it.
--
-- Who reads it: @app/Render.hs@ and @app/Hud.hs@ draw from it; 'Game.Sound' uses it
-- for a few sounds (a room change starting, and the clock for footsteps and
-- thunder); 'Game.Run' borrows the room size ('roomCols', 'roomRows', 'roomOf').
-- It reads only 'Game.Types' and two small helpers from 'Game.Logic'.
--
-- Units: ages and lifetimes are in ticks (60 a second); a 'Cell' is a map cell,
-- while the room on screen is counted in rooms. The fractions handed to drawing
-- ('focusProgress', 'roomFadeDark', 'waveProgress', 'echoWalkAt') run from 0 to 1
-- in straight lines; drawing eases them where it wants to (@smooth@ in
-- @app/Draw.hs@, the curve 3u^2 - 2u^3).
module Game.Fx
  ( Fx (..)
  , Wave (..)
  , Burst (..)
  , CostFloat (..)
  , EchoWalk (..)
  , RoomFade (..)
  , roomCols, roomRows
  , roomOf
  , roomedLevel
  , roomFadeTicks
  , roomFadeDark
  , costLife
  , focusTicks
  , focusProgress
  , echoStepTicks
  , echoWalkAt
  , initialFx
  , stepFx
  , waveProgress
  ) where

-- fromMaybe d m is the value inside m, or d when m is Nothing.
import Data.Maybe (fromMaybe)
-- Whether either keeper carries the power cell, and which cell that keeper is on:
-- the pickup flash is started and placed by these.
import Game.Logic (anyHasCell, carrierCell)
import Game.Types

-- | The era wave after a shift: the map's tiles flip edge-on from the old era to
-- the new, one by one in order of distance from 'waveOrigin', under a thin
-- expanding ring. It lasts 'shiftTicks', exactly as long as the shift's input lock.
-- An undo that changes the era makes no wave.
data Wave = Wave
  { waveAge    :: !Int  -- ^ ticks since the shift, from 0 up to @shiftTicks - 1@
  , waveFrom   :: !Era  -- ^ the era being left; the era arrived in is the game's 'gEra'
  , waveOrigin :: !Cell  -- ^ the keeper's cell at the shift, where the wave starts
  } deriving (Eq, Show)

-- | The flash when the power cell is picked up, drawn for 'burstLife' ticks.
data Burst = Burst
  { burstAge  :: !Int  -- ^ ticks since the pickup
  , burstCell :: !Cell  -- ^ where it was picked up: the carrier's cell at that tick
  } deriving (Eq, Show)

-- | The seconds an action just spent, or an undo gave back, rising off the player.
--
-- Drawn for 'costLife' ticks, rising and fading out; the watch in @app/Hud.hs@ shows
-- the same change on its ring, as a fading wake or a refill.
data CostFloat = CostFloat
  { cfAge    :: !Int  -- ^ ticks since the time left changed
  , cfAmount :: !Int   -- ^ the change in time left: negative when spent
  , cfCell   :: !Cell  -- ^ where the number rises from: where the action takes the
                       --   keeper (see @floatCell@ in 'stepFx')
  } deriving (Eq, Show)

-- | The echo walking the footprints back, from the player to where it stays.
data EchoWalk = EchoWalk
  { ewAge  :: !Int  -- ^ ticks since the echo was sent; it moves on a spot every
                    --   'echoStepTicks'
  , ewPath :: ![Spot]  -- ^ the player's spot first, the echo's last
  } deriving (Eq, Show)

-- | Moving to another room: the map darkens, the room changes at the darkest
-- moment, and it lightens again.
data RoomFade = RoomFade
  { rfAge  :: !Int  -- ^ ticks since the change began, from 0 up to
                    --   @roomFadeTicks - 1@
  , rfRoom :: !Cell  -- ^ the room being moved to
  } deriving (Eq, Show)

-- | All the decorative state of the game on screen. 'initialFx' is the empty one,
-- and 'stepFx' makes each tick's from the tick before.
--
-- The 'Maybe' fields are effects that come and go, 'Nothing' while none is
-- playing. 'stepFx' starts one when it sees its cause and drops it once its age
-- reaches its length; there is at most one of each kind, and a new cause starts it
-- again (a room change under way is turned towards the new room instead). The
-- 'Int' fields are clocks in ticks.
data Fx = Fx
  { fxClock    :: !Int           -- ^ ticks of ambient animation
                                 --   (the sea, surf, lamps and rain; it stands still
                                 --   while paused and starts from 0 on a new board)
  , fxWave     :: !(Maybe Wave)  -- ^ the era wave crossing the map
  , fxBurst    :: !(Maybe Burst) -- ^ the pickup flash
  , fxCarried  :: !Int           -- ^ ticks since pickup, for the carried light
                                 --   (0 while nobody carries the power cell)
  , fxEnd      :: !Int           -- ^ ticks since the level was won or lost
                                 --   (0 while it is neither; the end screens and the
                                 --   keeper stepping aside are timed by it)
  , fxPaused   :: !Int           -- ^ ticks since pause
                                 --   (0 when not paused; the pause screen fades in by it)
  , fxLevelAge :: !Int           -- ^ ticks since the level (re)started
                                 --   (it keeps counting while paused, so the fade-in
                                 --   from dark and the lesson under the map finish)
  , fxCost     :: !(Maybe CostFloat)  -- ^ the price or refund rising off the keeper
  , fxEchoWalk :: !(Maybe EchoWalk)  -- ^ the echo walking back, until it arrives
  , fxRoom     :: !Cell            -- ^ the room on screen, in rooms, not cells
  , fxRoomFade :: !(Maybe RoomFade)  -- ^ a change of room under way
  , fxVisited  :: ![Cell]          -- ^ rooms seen so far, for the map under the watch
                                   --   (in the order they were first seen)
  , fxFocus    :: !Int             -- ^ 0 while the level is being introduced; then
                                   --   ticks since the keeper first acted, up to
                                   --   'focusTicks'. A restart keeps it.
  } deriving (Eq, Show)

-- | No effects, every clock at 0, the screen on room (0, 0) with only that room
-- seen, and the level being introduced ('fxFocus' 0). @app/Main.hs@ starts with it,
-- and 'stepFx' goes back to it on a new board, keeping 'fxFocus'.
--
-- Written positionally: the arguments fill the fields in the order they are
-- declared in 'Fx', so a new field has to be added here in its place.
initialFx :: Fx
initialFx = Fx 0 Nothing Nothing 0 0 0 0 Nothing Nothing (0, 0) Nothing [(0, 0)] 0

-- | How long the screen takes to close in on the board once the keeper first acts.
--
-- 45 ticks, three quarters of a second. While the level is introduced the map
-- stands smaller to the right, beside a column with the watch, the level's name and
-- the keys; closing in, the column fades, the map grows and slides to the centre,
-- and the watch shrinks into the left margin (@app/Render.hs@, @app/Hud.hs@).
focusTicks :: Int
focusTicks = 45

-- | 0 while the level is introduced, 1 once the screen has closed in on the board.
--
-- A straight line in between; drawing eases it. 'min' caps the count at
-- 'focusTicks', so the result never passes 1. 'fromIntegral' turns each 'Int' into
-- a 'Float' (Haskell never converts numbers by itself), so the division is not a
-- whole-number one.
focusProgress :: Fx -> Float
focusProgress fx = fromIntegral (min focusTicks (fxFocus fx)) / fromIntegral focusTicks

-- | A level larger than one screen is a grid of rooms this size, and the screen
-- shows one room at a time. A one-room level is always room (0,0).
--
-- 13 by 9 cells is the size of the map on screen (and of the first level ever
-- made). A cell is 76 canvas units, so a room fills 988 by 684 units of the
-- 1280 by 800 canvas.
roomCols, roomRows :: Int
roomCols = 13
roomRows = 9

-- | The room a cell is in, counted in rooms, with (0, 0) the top-left room. 'div'
-- is whole-number division (written between its arguments with backquotes), so
-- columns 0 to 12 are in room column 0, columns 13 to 25 in room column 1, and so on.
roomOf :: Cell -> Cell
roomOf (x, y) = (x `div` roomCols, y `div` roomRows)

-- | A level built as a grid of whole rooms is shown a room at a time. Any other
-- size is shown through a camera that follows the keeper instead, and its rooms,
-- which 'stepFx' still tracks, are never drawn.
--
-- Whole rooms means both sides divide exactly ('mod' is the remainder). A 13 by 9
-- level counts, as a single room; The Long Way Round is two rooms by two.
roomedLevel :: Level -> Bool
roomedLevel level = lvWidth level `mod` roomCols == 0 && lvHeight level `mod` roomRows == 0

-- | The whole fade, out and back in. The room changes halfway.
-- 16 ticks, about a quarter of a second.
roomFadeTicks :: Int
roomFadeTicks = 16

-- | How dark the map is during a room change, 0 to 1.
--
-- A triangle over the fade's age: it rises in a straight line from 0 to 1 over the
-- first half and falls back to 0 over the second. Its peak, 1, comes at exactly
-- the tick on which 'stepFx' swaps the room, so the swap happens in the dark.
roomFadeDark :: Fx -> Float
roomFadeDark fx = case fxRoomFade fx of
  Nothing -> 0
  Just f ->
    -- The @:: Float@ annotation covers the whole expression before it, so the
    -- division is done in 'Float'.
    let halfway = fromIntegral roomFadeTicks / 2 :: Float
        a = fromIntegral (rfAge f)
    -- Darkening while under halfway; then 2 - a / halfway goes from 1 down to 0 at
    -- the end, and 'max 0' keeps it from going below.
    in if a < halfway then a / halfway else max 0 (2 - a / halfway)

-- | Lifetimes in ticks. The pickup flash lasts 40 ticks (two thirds of a second),
-- a price floats for 48 (0.8 s), and a walking echo takes 5 ticks over each
-- footprint. 'burstLife' is used only in this module, so it is not exported.
burstLife, costLife, echoStepTicks :: Int
burstLife = 40
costLife = 48
echoStepTicks = 5

-- | Where a walking echo is: the spot it is leaving, the one it is heading for, and
-- how far between them, 0 to 1. Nothing once it has arrived.
--
-- Each leg between two spots takes 'echoStepTicks'. A leg can join two eras on the
-- same cell, where a shift left a footprint; drawing then fades the echo from one
-- era into the other instead of moving it.
echoWalkAt :: EchoWalk -> Maybe (Spot, Spot, Float)
echoWalkAt w = case drop i (ewPath w) of
  -- At least two spots left: the echo is on its way from the first to the second.
  from : to : _ -> Just (from, to, fromIntegral r / fromIntegral echoStepTicks)
  -- One or none: it has reached the last spot.
  _ -> Nothing
  -- 'divMod' divides with a remainder: @i@ is how many legs are finished, which is
  -- also the index of the spot being left, and @r@ how many ticks into the current
  -- leg it is. A tuple on the left of @=@ takes the pair apart.
  where (i, r) = ewAge w `divMod` echoStepTicks

-- | 0 to 1 as the wave crosses the map. Same length as the input lock.
--
-- The wave is dropped once its age reaches 'shiftTicks', so the last value seen is
-- @(shiftTicks - 1) / shiftTicks@, just under 1; after that the board is drawn
-- wholly in the new era.
waveProgress :: Wave -> Float
waveProgress w = fromIntegral (waveAge w) / fromIntegral shiftTicks

-- | One tick of effects. @stepFx old new fx@ takes the game before this tick, the
-- game after it, and the effects so far, and gives the effects for this tick.
--
-- Three cases, tried in order:
--
--   * A new board ('gAttempt' changed: a restart, or a new level, hall or title
--     game set up by 'Game.Run'): everything starts again from 'initialFx', on the
--     keeper's room, keeping only 'fxFocus'.
--   * Paused: everything holds still, but 'fxPaused' counts the pause screen in and
--     'fxLevelAge' keeps counting so the level's fade-in finishes.
--   * Otherwise every clock and effect moves on a tick, new effects start where the
--     two games differ, and then @room@ moves the screen between rooms if need be.
--
-- It runs on every tick: on the title and in the hall (where the game waits in
-- 'Menu' and only the clock matters), during the opening film and the ending, and
-- while the courier talks, when the board is held still.
stepFx :: Game -> Game -> Fx -> Fx
stepFx old new fx
  -- A restart, or a new level, starts on the player's room with nothing to fade.
  -- @initialFx { ... }@ is record update syntax: a copy of 'initialFx' with these
  -- three fields replaced.
  | restarted = initialFx { fxRoom = roomOf (gCell new), fxVisited = [roomOf (gCell new)]
                          , fxFocus = fxFocus fx }
  -- Pausing freezes the world's effects, but the fade-in must still finish.
  | gPhase new == Paused = fx { fxPaused = fxPaused fx + 1, fxLevelAge = fxLevelAge fx + 1 }
  -- Record update binds tighter than applying a function, so the next line means
  -- @room (fx { ... })@: every field below is updated first, and @room@ then works
  -- on the result.
  | otherwise = room fx
      { fxClock    = fxClock fx + 1
      , fxWave     = wave
      , fxBurst    = burst
      -- Counts while either keeper carries the power cell; back to 0 if an undo
      -- takes the pickup back.
      , fxCarried  = if anyHasCell new then fxCarried fx + 1 else 0
      -- Counts from a win or a loss; an undo from the lost screen sets it to 0.
      , fxEnd      = if gPhase new `elem` [Won, Lost] then fxEnd fx + 1 else 0
      , fxPaused   = 0
      , fxLevelAge = fxLevelAge fx + 1
      , fxCost     = cost
      , fxEchoWalk = echoWalk
      , fxFocus    = closingIn
      }
  where
    -- The screen follows the room the player is in, or stepping into, so the
    -- change starts as the step does. The room swaps at the darkest moment.
    -- In a level for two it follows the first keeper.
    focus = case gMotion new of
      Stepping to _ -> to
      _             -> gCell new
    wanted = roomOf focus
    -- A case branch can carry guards, like an equation. With no change under way,
    -- start one if the keeper's room is not the one on screen. With one under way,
    -- age it and aim it at the room wanted now (if the keeper turns back before
    -- halfway, the swap lands on the room already shown); from halfway the shown
    -- room is the target, noted as seen; at the full length the fade ends.
    room f = case fxRoomFade f of
      Nothing
        | wanted /= fxRoom f -> f { fxRoomFade = Just (RoomFade 0 wanted) }
        | otherwise -> f
      Just rf ->
        let age = rfAge rf + 1
            rf' = rf { rfAge = age, rfRoom = wanted }
            -- The room on screen: the target from halfway (the darkest tick) on.
            swapped = if age >= roomFadeTicks `div` 2 then rfRoom rf' else fxRoom f
            -- Rooms seen, in the order first seen: a new one goes on the end. The
            -- list is a handful of rooms, so 'elem' and @++@ cost nothing.
            seen = if swapped `elem` fxVisited f then fxVisited f else fxVisited f ++ [swapped]
        in f { fxRoom = swapped
             , fxVisited = seen
             , fxRoomFade = if age >= roomFadeTicks then Nothing else Just rf'
             }
    -- A new echo walks back along the footprints it was sent down, which are still
    -- in the old game. An undo puts the old echo straight back.
    echoWalk
      -- A guard can mix tests and pattern guards, separated by commas, and all must
      -- hold. @Just landing <- gEcho new@ is a pattern guard: it holds only if the
      -- new game has an echo, and names where it stands.
      | gEcho new /= gEcho old && not undone, Just landing <- gEcho new =
          Just (EchoWalk 0 (walkedTo landing (gTrail old)))
      -- The echo changed some other way (an undo put the old one back, or took it
      -- away): no walk, it simply stands where it now is.
      | gEcho new /= gEcho old = Nothing
      -- No change: walk on while 'echoWalkAt' still finds the echo between two
      -- spots, and drop the walk once it has arrived.
      | otherwise = case fxEchoWalk fx of
          Just w | Just _ <- echoWalkAt w -> Just w { ewAge = ewAge w + 1 }
          _ -> Nothing
    -- The path walked: the player's spot (the newest footprint), then the older
    -- footprints up to and including one on the landing spot. If the trail crossed
    -- that spot more than once, the walk stops at the first, newest, time: the same
    -- place the rule put the echo, by a shorter way. An empty trail gives an empty
    -- path, which 'echoWalkAt' treats as already arrived.
    walkedTo landing trail = case trail of
      here : older -> here : takeThrough landing older
      [] -> []
    -- Everything in @ys@ up to and including the first @x@, or all of @ys@ if there
    -- is none. 'break' splits a list where a test first holds: the part before, and
    -- the rest, which starts with the match.
    takeThrough x ys = case break (== x) ys of
      (before, found : _) -> before ++ [found]
      (before, []) -> before
    -- A new board: 'gAttempt' changes only when a game is set up afresh, never on
    -- an undo.
    restarted = gAttempt new /= gAttempt old
    -- The introduction lasts until the keeper first does something that takes: a
    -- step, a push, a jump or a shift. A refused key does not end it.
    -- Something took if the time left changed (an action was paid for) or either
    -- keeper started moving (which also covers a free move, as in the ending).
    -- @maybe False f m@ is False for 'Nothing' and @f p@ for @Just p@; the
    -- composition @(/= Settled) . ptMotion@ takes the keeper's motion and then asks
    -- whether it is anything but 'Settled'.
    acted = gRemaining new /= gRemaining old || gMotion new /= Settled
         || maybe False ((/= Settled) . ptMotion) (gPartner new)
    -- Once started, count up to 'focusTicks' and stay there; start at 1 on the
    -- first action; stay at 0 until then.
    closingIn
      | fxFocus fx > 0 = min focusTicks (fxFocus fx + 1)
      | acted = 1
      | otherwise = 0
    -- A step is charged before it moves, but the price belongs where it is going.
    -- The pair of the second keeper's motions, before and after, is matched in one
    -- go. Only a second keeper setting off from standing still matches: a step they
    -- start on the tick the last one lands (a key held down) was already stepping
    -- before, so that price shows over the first keeper. Otherwise it is the first
    -- keeper's destination, or their cell for anything but a step (a shift, an
    -- undo). If both keepers pay in one tick, the one number is both prices together.
    floatCell = case (fmap ptMotion (gPartner old), fmap ptMotion (gPartner new)) of
      (Just Settled, Just (Stepping to _)) -> to  -- the second keeper set off
      _ -> case gMotion new of
        Stepping to _ -> to
        _             -> gCell new
    -- Any change in the time left starts a new float showing it: negative for a
    -- price, positive for an undo's refund. (A restart refills the time too, but was
    -- caught by the first case.) Otherwise the float ages until 'costLife'.
    cost
      | gRemaining new /= gRemaining old =
          Just (CostFloat 0 (gRemaining new - gRemaining old) floatCell)
      | otherwise = case fxCost fx of
          Just c | cfAge c + 1 < costLife -> Just c { cfAge = cfAge c + 1 }
          _ -> Nothing
    -- An undo has no input lock for a wave to cross under, so it cuts straight back.
    -- 'undone': the history got shorter, which here only an undo does (a restart
    -- empties it too, but was caught by the first case). 'length' counts every
    -- entry, one per action taken, which is cheap at the length of a level.
    undone = length (gHistory new) < length (gHistory old)
    -- An era change made by an undo: no wave. Any other era change (an accepted
    -- shift): a wave from the era left, starting at the keeper. No change: the
    -- current wave ages until it has run 'shiftTicks', then goes.
    wave
      | gEra new /= gEra old && undone = Nothing
      | gEra new /= gEra old = Just (Wave 0 (gEra old) (gCell new))
      | otherwise = case fxWave fx of
          Just w | waveAge w + 1 < shiftTicks -> Just w { waveAge = waveAge w + 1 }
          _ -> Nothing
    -- Nobody carries the power cell: no flash (an undo of the pickup ends one at
    -- once). Carried now but not before: it was just picked up, so a flash starts on
    -- the carrier. The 'fromMaybe' fallback is only there for the types: when
    -- 'anyHasCell' holds, 'carrierCell' always finds someone. Carried before and
    -- now: the flash ages until 'burstLife'.
    burst
      | not (anyHasCell new) = Nothing
      | not (anyHasCell old) = Just (Burst 0 (fromMaybe (gCell new) (carrierCell new)))
      | otherwise = case fxBurst fx of
          Just b | burstAge b + 1 < burstLife -> Just b { burstAge = burstAge b + 1 }
          _ -> Nothing
