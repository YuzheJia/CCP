-- | What should be heard: pure, and derived like 'Game.Fx' by comparing each tick
-- with the one before, so a sound cannot disagree with what happened.
--
-- 'cues' names the one-shot sounds a tick starts. 'beds' says how loud each looping
-- bed should be now. Neither knows how a sound is made or played: the app does that.
--
-- What this module is responsible for:
--
-- * Which short sounds (one-shots, the 'Cue' type) a tick starts. Nothing in the
--   rules ('Game.Logic') or the campaign ('Game.Run') ever says "play a sound here".
--   Instead 'cues' looks at the run and its effects before and after one tick and
--   notices what changed: the era is different, so a shift was heard; a crate moved,
--   so a push was heard. This is the trick 'Game.Fx' uses for its effects, and it
--   means a sound can only follow something that really happened, on the tick it
--   happened.
-- * How loud each looping background sound (a /bed/: the sea, the storm, and the
--   ending's beam, drone, wind and beat) should be right now, and how fast it plays
--   ('Beds'). A bed has no start or stop event: 'beds' works its level out afresh
--   from the run as it is, and the mixer glides towards it.
--
-- What it deliberately does not do: it does not know what a sound sounds like, how
-- loud a cue is in absolute terms, or how anything reaches the speakers. It has no
-- IO and imports nothing from the app, so it lives in the pure library beside the
-- rules and is checked by plain assertions in test/Main.hs (for example, that each
-- moment of level 3's ending sounds exactly once, on the tick its picture shows).
--
-- Who feeds it and who uses it:
--
-- * Fed by 'Game.Run' (the run's mode, the ending's 'Finale' and 'Dash', and the
--   ending's timings such as 'lampDiesAt'), 'Game.Fx' (the ambient clock, room fades
--   and the room size), 'Game.Logic' ('anyHasCell') and 'Game.Types' (the 'Game'
--   record and its parts).
-- * Used by app/Main.hs: every tick ('oneTick' there) calls 'cues' with the world
--   before and after and keeps what it returns; once a frame, after that frame's
--   ticks, Main plays the kept cues in order through 'Audio.playCue' and passes
--   @beds levels run@ to 'Audio.setBeds'. app/Audio.hs chooses each cue's made sound
--   (from app/Synth.hs), loudness, pitch and pan. 'Game.Story' makes the courier's
--   talk cues ('CueTalkOpen' and the rest) itself, with this module's 'Cue' type.
--
-- Units and conventions: a tick is 1/60 s ('ticksPerSecond'); a 'Cell' is
-- (column, row), with (0, 0) the top-left cell and rows growing down the map; a pan
-- runs from -1 (all left) through 0 (centre) to 1 (all right); a bed's loudness is
-- a gain from 0 (silent) up, small in practice (the sea in play is 0.07), and its
-- rate is a playback speed where 1 is the sound as it was made.
module Game.Sound
  -- @T (..)@ exports a type together with all its constructors (and, for a record,
  -- its field names), so other modules can build and match on them.
  ( Cue (..)
  , Keeper (..)
  , cues
  , Beds (..)
  , silence
  , beds
  , lampAt
  , lampCount
  ) where

-- A qualified import: Data.Set's functions are written Set.size and so on, so they
-- cannot clash with the list functions of the same names.
import qualified Data.Set as Set
import Game.Fx
import Game.Logic (anyHasCell)
import Game.Run
import Game.Types

-- | A one-shot sound that a tick starts. Each constructor names a moment, not a
-- sound file: app/Audio.hs decides which made sound plays for it and how.
--
-- The @!@ before a field's type makes that field strict: its value is worked out
-- when the cue is built, so a cue never carries an unevaluated computation (a
-- thunk) across to the app. @deriving (Eq, Show)@ asks the compiler to write the
-- equality test (the tests compare lists of cues) and the text form ('show', which
-- @--trace@ prints as @sound CueStep ...@).
data Cue
  = CueStep !Era !Int !Float !Keeper
                              -- ^ a keeper sets off: the era underfoot, which foot,
                              --   where on screen (-1 left to 1 right), and who.
                              --   The foot is the ambient clock ('fxClock', in
                              --   ticks) when the step began; Audio divides it by
                              --   'stepTicks' to pick one of three slightly
                              --   different feet. A jump is 'CueJump' instead.
  | CuePush                   -- ^ a crate pushed one cell, in the past, onto floor
  | CueMirrorPush             -- ^ a movable mirror pushed one cell on its sled
  | CuePitFill                -- ^ a crate into a pit, which it fills and is used up
                              --   in; heard instead of 'CuePush'
  | CueJump !Bool             -- ^ through a mirror; True for a corner mirror, which
                              --   turns the keeper out of its other face
  | CueShift !Era             -- ^ into this era; also Space during the rush
  | CuePickup                 -- ^ the power cell taken up, by either keeper
  | CueLamp                   -- ^ the lamp lit: the level won
  | CueLose                   -- ^ out of time: the level lost, with less than one
                              --   step's price left
  | CueBump                   -- ^ walked into something that will not move: a wall,
                              --   a crate rusted in the present, a push the other
                              --   era's wall refuses, a mirror set in the wall, or
                              --   a mirror's face with nowhere to land
  | CueRefused                -- ^ a shift that is not allowed here: a wall in the
                              --   other era, or not enough time left
  | CueUndo                   -- ^ Z took the last action back
  | CueRestart                -- ^ the level begun again: R, or Enter once out of
                              --   time
  | CueRoom                   -- ^ through a doorway into another room
  | CuePause !Bool            -- ^ paused, or resumed
  | CueMenuMove               -- ^ the hall's choice moved to another level
  | CueMenuEnter              -- ^ Enter: into the hall or the opening film, into a
                              --   level, or on to the next level from a won one
  | CueMenuBack               -- ^ Escape: back to the title, or back to the hall
    -- The ending, in order.
  | CueFlare                  -- ^ the lamp flares, rising to the white
  | CueLampDies               -- ^ and goes out
  | CueLightGoes              -- ^ its light leaves through the wall
  | CueWallBreaks !Int        -- ^ the light breaks the passage's wall number n,
                              --   counting from 0 at the wall beside the lighthouse
  | CueRushBegins             -- ^ the keeper stands on the way and is carried off
  | CueBarrier                -- ^ held up at a barrier
  | CueBarrierPassed          -- ^ past a barrier, which becomes the place to start
                              --   again from if the keeper falls behind
  | CueCaught                 -- ^ fallen behind the light, taken back
  | CueLampCatches !Int       -- ^ the corridor's lamp number n catches as the keeper
                              --   passes; n runs from 0 to 'lampCount' - 1
  | CueThunder                -- ^ thunder after the lightning, in the present
  | CueArrival                -- ^ the real lighthouse
  | CueCard                   -- ^ the level beyond names itself
    -- The opening film and the courier's talk.
  | CueIntro                  -- ^ the opening film's soundtrack starts
  | CueIntroEnd               -- ^ and stops, skipped or over
  | CueTalkOpen               -- ^ the talk box pops up
  | CueTalkNext               -- ^ on to the next line
  | CueTalkBlip !Int          -- ^ a letter typed out: the how-manyth, for its pitch.
                              --   Counted from 1; only every third letter (1, 4,
                              --   7, ...) that is not a space blips ('Game.Story')
  | CueTalkClose              -- ^ the talk box goes
    -- Her orders: the saves.
  | CuePrint                  -- ^ a row of a new order printed
  | CueTearing !Int           -- ^ a little more of an order torn, while Enter is held
  | CueTorn                   -- ^ and torn off: the order is cancelled
  deriving (Eq, Show)

-- | In a level for two: the yellow keeper (W A S D, and the only one in any other
-- level) or the red one (the arrows). Only 'CueStep' carries it, so Audio can give
-- the red keeper a lighter, higher tread.
data Keeper = Yellow | Red
  deriving (Eq, Show)

-- | The sounds one tick starts, from the run and its effects before and after.
--
-- @levels@ is the whole level list (the campaign, then the secret level), indexed by
-- 'runIndex'. @(r, fx)@ is the run and its effects before the tick and @(r', fx')@
-- after it; the effects matter for the ambient clock and room fades. The result is
-- in the order the sounds should start, usually empty and rarely more than two.
--
-- The work is one @case@ on the pair of modes, before and after: which screen the
-- run was on and is on now decides which group of sounds applies. The branches are
-- tried from the top and the first that matches wins, so their order matters. In a
-- pattern, @_@ matches anything and binds no name.
cues :: [Level] -> (Run, Fx) -> (Run, Fx) -> [Cue]
cues levels (r, fx) (r', fx') = case (runMode r, runMode r') of
  -- Enter on the title, once a level has been cleared, opens the hall.
  (Title, Lobby) -> [CueMenuEnter]
  -- The order desk selects an existing delivery or starts a new one via Intro.
  (Title, Orders) -> [CueMenuEnter]
  (Orders, Title) -> [CueMenuBack]
  (Orders, Lobby) -> [CueMenuEnter]
  (Orders, Intro _ _) -> [CueMenuEnter, CueIntro]
  (Lobby, Orders) -> [CueMenuBack]
  -- Direct title-to-film transitions remain useful for core-only simulations.
  (Title, Intro _ _) -> [CueMenuEnter, CueIntro]
  -- The film is over, skipped (Escape, or Enter held) or played to its end, and
  -- level 1 begins: its soundtrack fades out.
  (Intro _ _, InLevel) -> [CueIntroEnd]
  -- Escape from the hall, back to the title.
  (Lobby, Title) -> [CueMenuBack]
  -- Moving the hall's choice. A list comprehension with a condition and nothing
  -- drawn from a list gives a one-element list when the condition holds and [] when
  -- it does not: the idiom this module uses to turn a test into zero or one cue.
  -- Pressing up on the first level leaves the index as it is, and is silent.
  (Lobby, Lobby) -> [ CueMenuMove | runIndex r /= runIndex r' ]
  -- Enter in the hall starts the chosen level.
  (Lobby, InLevel) -> [CueMenuEnter]
  -- Anything else into the hall: Escape from a level or from the ending. It comes
  -- after (Lobby, Lobby), which would otherwise be caught here.
  (_, Lobby) -> [CueMenuBack]
  (InLevel, InLevel)
    -- Enter on a won level's plate starts the next level: a different board, whose
    -- first tick must not be compared with the old one's as if it were play.
    | runIndex r /= runIndex r' -> [CueMenuEnter]  -- on from a won level
    | otherwise -> inLevel
  -- The win that starts level 3's ending is still a tick of play, and carries its
  -- 'CueLamp' like any other win.
  (InLevel, Ending _) -> inLevel                     -- the win that starts it
  -- Inside the ending: its own scored moments, and ordinary play sounds while the
  -- keeper walks the opened station.
  (Ending f, Ending f') -> ending level f f' g g' fx' ++ walking f f'
  -- Everything else is silent: the title or the film standing still, and the
  -- arrival leading on into the secret level (its card has already sounded).
  _ -> []
  where
    -- The level as it is after the tick. '!!' indexes a list from 0. Haskell is
    -- lazy, so this is only looked up by the branches that use it, never for the
    -- menus.
    level = levels !! runIndex r'
    g = runGame r
    g' = runGame r'
    inLevel = playing level g g' fx fx'
    -- Walking the opened station is ordinary play, sounds and all.
    -- Only while the keeper walks free ('Beckon'), and on the tick the rush begins,
    -- whose game still came from ordinary play. A function in backticks, as in
    -- @x `elem` xs@, is used as an infix operator.
    walking f f'
      | fnStage f == Beckon && fnStage f' `elem` [Beckon, Rush] = playing level g g' fx fx'
      | otherwise = []

-- | The sounds of one tick of play, in @level@, from the game and effects before
-- (@g@, @fx@) and after (@g'@, @fx'@) the tick.
--
-- A restart or an undo is heard alone: everything else that changed on that tick
-- (the era flipping back, a crate jumping back) was its doing, and must not also
-- sound as a shift or a push. Otherwise each line of the list below adds zero or
-- one cue (the steps can add two in a level for two), and the lines are joined in a
-- fixed order with 'concat', which flattens a list of lists into one list.
playing :: Level -> Game -> Game -> Fx -> Fx -> [Cue]
playing level g g' fx fx'
  -- 'gAttempt' counts starts and restarts, so a change means the level began again
  -- (R, or Enter on the lost screen). It comes before the undo test because a
  -- restart also empties the history.
  | gAttempt g' /= gAttempt g = [CueRestart]
  | undone = [CueUndo]
  | otherwise = concat
      [ [ CuePause True | gPhase g' == Paused, gPhase g /= Paused ]
      , [ CuePause False | gPhase g == Paused, gPhase g' == Playing ]
      -- A shift changes the era on the tick it is accepted; the wave that follows
      -- is only drawing. In a level for two the era is shared, so this is one cue.
      , [ CueShift (gEra g') | gEra g' /= gEra g ]
      , steps
      -- Only a push moves a mirror (an undo or a restart was dealt with above), so
      -- any change to where the mirrors stand is a push. '/=' on a Map compares its
      -- contents.
      , [ CueMirrorPush | gMirrors g' /= gMirrors g ]
      -- A crate moved, and no pit was filled: an ordinary push. A crate that goes
      -- into a pit also leaves 'gCrates', so without the second test it would sound
      -- as a push and a pit at once. 'Set.size' is quick: a Set knows its size.
      , [ CuePush | gCrates g' /= gCrates g, Set.size (gFilled g') == Set.size (gFilled g) ]
      , [ CuePitFill | Set.size (gFilled g') > Set.size (gFilled g) ]
      -- Either keeper took the power cell up this tick.
      , [ CuePickup | anyHasCell g', not (anyHasCell g) ]
      , [ CueLamp | gPhase g' == Won, gPhase g /= Won ]
      , [ CueLose | gPhase g' == Lost, gPhase g /= Lost ]
      , [ CueBump | bumped ]
      , [ CueRefused | refused ]
      -- A room fade has just begun. @Just _ <- [fxRoomFade fx']@ draws the one value
      -- from a one-element list and keeps going only if it matches the pattern, so
      -- the line reads "a fade now, and none the tick before". Only in a level shown
      -- a room at a time: a level shown through the following camera still tracks
      -- its rooms in 'Fx' but never draws the fade, so it must not be heard either.
      , [ CueRoom | roomedLevel level, Just _ <- [fxRoomFade fx'], Nothing <- [fxRoomFade fx] ]
      ]
  where
    -- An undo pops the newest snapshot off the history, so the history got shorter.
    -- The history is as long as the actions taken so far, so its length is cheap
    -- enough to take once a tick.
    undone = length (gHistory g') < length (gHistory g)
    -- The yellow keeper is the game's own keeper ('gCell', 'gMotion'); in a level for
    -- two the red one is the partner, present before and after the tick. The era is
    -- the one after the tick, as the keeper sets off in it. The red keeper's foot is
    -- the clock plus one; Audio divides it by 'stepTicks', so the two keepers' feet
    -- differ only when the clock is on the last tick of a block of eight.
    steps = keeperSteps Yellow (gEra g') (gCell g, gMotion g) (gCell g', gMotion g') (fxClock fx)
         ++ case (gPartner g, gPartner g') of
              (Just p, Just p') -> keeperSteps Red (gEra g') (ptCell p, ptMotion p) (ptCell p', ptMotion p') (fxClock fx + 1)
              -- A level for one has no partner, and adds no steps here.
              _ -> []
    -- The kind of feedback raised this tick, if any. Feedback counts its ticks left
    -- down by one each tick ('fbLeft'), so it is new when there was none before, when
    -- its kind changed, or when its count went up: the same message raised again.
    -- @maybe True f m@ is True when @m@ is Nothing, and @f@ of the value otherwise.
    fresh = case gFeedback g' of
      Just f | maybe True (\o -> fbKind o /= fbKind f || fbLeft f > fbLeft o) (gFeedback g) -> Just (fbKind f)
      _ -> Nothing
    -- A shift refused: the other era is a wall here (the cell is ignored), or there
    -- is not enough time left for it.
    refused = case fresh of
      Just (ShiftBlocked _) -> True
      Just ShiftTooCostly -> True
      _ -> False
    -- The bump counter is set to 'bumpTicks' when a fresh press walks into something
    -- that will not move, and otherwise counts down by one a tick, so a rise means a
    -- new bump. Either keeper's counts.
    bumpUp = gBump g' > gBump g
             || case (gPartner g, gPartner g') of
                  (Just p, Just p') -> ptBump p' > ptBump p
                  _ -> False
    -- A bump, or a push refused with a message: a crate whose cell in the other era
    -- is a wall, a crate rusted in the present, a mirror set in the wall, a mirror's
    -- face with nowhere to land. Either way one 'CueBump'. The other messages (the
    -- hints about holding a key, the echo's hints, 'CellCollected') are silent.
    bumped = bumpUp || case fresh of
      Just (PushBlocked _) -> True
      Just CrateRusted -> True
      Just MirrorFixed -> True
      Just MirrorFaceSide -> True
      _ -> False

-- | A keeper setting off: a step to the next cell, or a jump through a mirror. A
-- flat mirror puts the keeper straight on, behind it; a corner mirror turns them, so
-- they land off the line they set off along.
--
-- Arguments: who it is, the era after the tick, the keeper's (cell, motion) before
-- and after the tick (only the motion of the first is read), and the foot number
-- that 'CueStep' carries. The result is empty or one cue.
--
-- @from\@(x, y)@ is an as-pattern: it names the whole pair @from@ and its parts @x@
-- and @y@ at once. While a step is under way 'gCell' is still the cell it set off
-- from, so @from@ is where the keeper stands.
keeperSteps :: Keeper -> Era -> (Cell, Motion) -> (Cell, Motion) -> Int -> [Cue]
keeperSteps who era (_, before) (from@(x, y), now) foot = case now of
  -- A step (or a jump, which the rules make an ordinary step to the landing cell)
  -- that began this tick: it starts with its full 'stepTicks' (8) left, and the
  -- motion differs from the tick before. The second test matters when the board
  -- stands still just after a step began, while paused or while the courier talks:
  -- the motion then stays at 8 for many ticks, and each would sound again.
  Stepping to@(tx, ty) left
    | left == stepTicks && before /= now ->
        -- Neighbouring cells are one apart, so anything further is a jump. A flat
        -- mirror lands on the same row or column; a corner mirror turns the keeper,
        -- so the landing differs in both.
        if apart from to > 1 then [CueJump (tx /= x && ty /= y)] else [CueStep era foot pan who]
  -- Standing, shifting, or part-way through a step: nothing new sets off.
  _ -> []
  where
    -- Cells between two cells along rows and columns (the taxicab distance).
    apart (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)
    -- Where in its room the keeper stands, for a room shown a room at a time.
    -- @x `mod` roomCols@ is the column within the room, 0 to 12, and 6 is the
    -- centre column, so this runs from -0.6 at the left wall to 0.6 at the right:
    -- never hard to one side. Every level in the hall is built from whole rooms
    -- 13 columns wide, so this is where on the screen the keeper is. 'fromIntegral'
    -- turns the Int column into a Float.
    pan = 0.6 * (fromIntegral (x `mod` roomCols) - 6) / 6

-- | The ending's own moments, on the ticks the picture marks them.
--
-- @level@ is the won level (whose passage the light breaks open); @f@ and @f'@ are
-- the ending's state before and after the tick, @g@ and @g'@ the game, and @fx'@
-- the effects after it (for the thunder's clock). Each branch is a change of stage
-- or a tick within one; 'fnAge' counts ticks into the current stage.
ending :: Level -> Finale -> Finale -> Game -> Game -> Fx -> [Cue]
ending level f f' g g' fx' = case (fnStage f, fnStage f') of
  -- The lit lamp's 'litTicks' are over and the flare begins.
  (Lit, Flare) -> [CueFlare]
  -- On tick 'lampDiesAt' (24) of the flare, under the brightest light. A case
  -- branch can carry a guard; when it fails, matching goes on to the branches below,
  -- here to the last one, which is silent.
  (Flare, Flare) | fnAge f' == lampDiesAt -> [CueLampDies]
  -- The light sets off 'lightSetsOff' ticks into the stage, then each wall of the
  -- passage breaks as the light reaches it ('cellOpenTick' gives wall k's tick).
  -- The passage is a short list, so checking every wall each tick is cheap.
  (Opening, Opening) ->
    [ CueLightGoes | fnAge f' == lightSetsOff ]
    ++ [ CueWallBreaks k | k <- [0 .. length (lvPassage level) - 1], fnAge f' == cellOpenTick level k ]
  -- The keeper stood on the way, in the present, and is carried off.
  (Beckon, Rush) -> [CueRushBegins]
  (Rush, Rush) -> rush ++ thunder
  -- The keeper has run the whole way to the real lighthouse.
  (Rush, Arrival) -> [CueArrival]
  -- The level beyond names itself 140 ticks into the arrival, as Render starts to
  -- fade its card in.
  (Arrival, Arrival) -> [ CueCard | fnAge f' == 140 ] ++ thunder
  _ -> []
  where
    -- The rush's state ('Dash'), before and after the tick.
    d = fnDash f
    d' = fnDash f'
    rush = concat
      -- Fallen too far behind the screen: 'dsCaught' jumps from 0 to 'catchTicks'.
      [ [ CueCaught | dsCaught d == 0, dsCaught d' > 0 ]
      -- Space changed the era. Halfway through being taken back, the rush itself
      -- puts the keeper back in the era of the last barrier passed; nobody pressed
      -- anything, so that change is silent.
      , [ CueShift (gEra g') | gEra g' /= gEra g, dsCaught d' == 0 ]
      , [ CueBarrier | dsBlocked d', not (dsBlocked d) ]
      -- Space where the other era has no floor (still inside the station, or inside
      -- a barrier of that era): the rush sets 'dsRefused' to 24 ticks and otherwise
      -- counts it down, so a rise is a fresh refusal.
      , [ CueRefused | dsRefused d' > dsRefused d ]
      -- 'dsCheck', where a keeper taken back starts again, only moves on when a
      -- barrier is passed.
      , [ CueBarrierPassed | dsCheck d' > dsCheck d ]
      -- Each lamp sounds on the tick the keeper's distance run ('dsAlong', in cells
      -- from where the rush began) crosses its catch point. A @let@ inside a list
      -- comprehension names a value for the rest of it. 'gCell' is the cell the
      -- rush began from: the rush moves the keeper by 'dsAlong' alone and leaves
      -- 'gCell' where it was. A keeper taken back behind a lamp crosses it again
      -- later, and it sounds again, as the picture lights it again.
      , [ CueLampCatches n | dsCaught d' == 0, n <- [0 .. lampCount - 1]
                           , let at = lampAt level (gCell g') n
                           , dsAlong d < at, dsAlong d' >= at ]
      ]
    -- Thunder follows the lightning over the sea, in the present.
    -- Every 330 ticks (5.5 s), 8 ticks into the cycle: between the two flashes that
    -- Render draws on ticks 0 to 4 and 9 to 12 of the same 'fxClock' cycle.
    thunder = [ CueThunder | gEra g' == Present, fxClock fx' `mod` 330 == 8 ]

-- | The corridor's lamps stand every four cells beyond the station, and each one
-- catches as the keeper comes within a cell and three quarters of it: at this far
-- along the rush, for a rush begun at @start@.
--
-- Lamp @n@ stands @2 + 4 * n@ cells beyond the station's edge, the same places
-- Render draws them. Render lights a lamp smoothly as the keeper comes from 2.5
-- cells short of it to 1 cell short; the sound is in the middle of that, 1.75 cells
-- short, when the lamp is half lit. The result is measured as 'dsAlong' is, in
-- cells from @start@; 'edgeFrom' gives the cells from @start@ to the edge.
--
-- A level with no ending has no corridor: @1 / 0@ is floating-point infinity, a
-- point the keeper never reaches, so no lamp ever catches.
lampAt :: Level -> Cell -> Int -> Float
lampAt level start n = case finaleRun level of
  Just fr -> edgeFrom fr start + fromIntegral (2 + 4 * n) - 1.75
  Nothing -> 1 / 0

-- | How many lamps the corridor has: 13. @[2, 6 .. corridorLength - 1]@ is the
-- arithmetic sequence 2, 6, 10, ... up to 53 (the corridor is 54 cells long), the
-- list Render draws the lamps from. Audio's rising scale has a note for each.
lampCount :: Int
lampCount = length [2 :: Int, 6 .. corridorLength - 1]

-- | How loud each looping bed should be, 0 to 1, and how fast it plays.
--
-- One field per bed, each with its own slot in the mixer (slots 0 to 5, set by
-- 'Audio.setBeds'). A loudness is the gain the mixer plays the bed at, before its
-- master volume; the beds are made with a peak of 0.6 (0.7 for the beat), and the
-- numbers used here are small: the sea in play is 0.07. A rate is a playback speed,
-- 1 as made; the mixer plays faster by stepping through the samples faster, so a
-- higher rate is also a higher pitch. Beds that have no rate field play at 1.
--
-- A record: each field has a name, and the name is also a function that reads the
-- field (@bdSea b@). The @!@ makes every field strict.
data Beds = Beds
  { bdSea       :: !Float  -- ^ the calm sea of the past
  , bdStorm     :: !Float  -- ^ wind and rain in the present
  , bdBeam      :: !Float  -- ^ the lamp's light running through the walls
  , bdDrone     :: !Float  -- ^ the way waiting to be walked
  , bdWind      :: !Float  -- ^ the air of the rush
  , bdWindRate  :: !Float  -- ^ the wind's rate: 0.7 standing to 1.5 at full speed,
                           --   so it rises in pitch as it grows louder
  , bdPulse     :: !Float  -- ^ the beat of the rush
  , bdPulseRate :: !Float  -- ^ the beat's rate, so its tempo: about 0.96 as the
                           --   screen sets off, 1.2 at its top speed
  } deriving (Eq, Show)

-- | Every bed silent, both rates at 1. The constructor is applied to the fields in
-- the order they are declared. Every other 'Beds' value below is @silence@ with a
-- few fields changed, so a bed nobody mentions stays silent. Used as it is for the
-- opening film, which brings its own sound.
silence :: Beds
silence = Beds 0 0 0 0 0 1 0 1

-- | How loud each bed should be for the run as it is now.
--
-- Unlike 'cues' this compares nothing: a bed is a level, not an event. Main calls it
-- once a frame, after that frame's ticks, and passes the result to 'Audio.setBeds';
-- the mixer glides to the new levels over about 40 ms, so a sudden change here is
-- heard as a quick fade, not a click.
beds :: [Level] -> Run -> Beds
beds levels r = case runMode r of
  -- The weather of the game behind the title. That is always a level's first state,
  -- which is in the present, so it is the storm, at half its level in play.
  Title -> weather 0.5
  Orders -> weather 0.45
  -- The film brings its own rain.
  Intro _ _ -> silence
  -- The hall holds a level's first state too: the storm, quieter still.
  Lobby -> weather 0.35
  InLevel
    -- Pausing lowers the weather rather than stopping it.
    | gPhase g == Paused -> weather 0.4
    | otherwise -> weather 1
  Ending f -> finaleBeds f
  where
    -- Only the ending's Opening stage needs the level, and laziness means it is
    -- looked up only then.
    level = levels !! runIndex r
    g = runGame r
    -- The era's weather at @k@ times its level in play: the sea in the past, the
    -- storm in the present. @silence { bdSea = ... }@ is record update syntax: a
    -- copy of @silence@ with that one field changed (nothing is changed in place).
    weather k = case gEra g of
      Past -> silence { bdSea = 0.07 * k }
      Present -> silence { bdStorm = 0.06 * k }
    -- 0 at tick 0, rising in a straight line to 1 at tick @n@, and 1 after that
    -- ('min' and 'max' hold it between 0 and 1). Fades a bed in or out over @n@
    -- ticks; @1 - ramp ...@ fades one out.
    ramp a n = min 1 (max 0 (fromIntegral a / n))
    finaleBeds f = case fnStage f of
      -- The lamp burns and the plate shows: the weather of play.
      Lit -> weather 1
      -- The weather drops away as the lamp dies, and the station holds its breath.
      -- It falls from full to nothing over the 24 ticks until 'lampDiesAt', and
      -- stays silent for the rest of the flare.
      Flare -> weather (1 - ramp (fnAge f) (fromIntegral lampDiesAt))
      -- Only the light is heard while it runs through the walls: from the tick it
      -- sets off until it has crossed the last cell of the way ('lightCellTicks'
      -- each), fading in over 10 ticks to 0.32. Silent before and after.
      -- @let ... in ...@ names values for one expression; unlike @where@, it is
      -- itself an expression, so it can sit inside one branch of a case.
      Opening ->
        let lightGone = lightSetsOff + lightCellTicks * length (wayCells level)
            a = fnAge f
        in silence { bdBeam = if a >= lightSetsOff && a < lightGone then 0.32 * ramp (a - lightSetsOff) 10 else 0 }
      -- The keeper walks free: the weather comes back over 120 ticks (2 s), to 0.6
      -- of its level in play, and the drone fades in over 90 ticks to 0.4. Record
      -- update works on any expression of the record's type, here in brackets.
      Beckon -> (weather (0.6 * ramp (fnAge f) 120)) { bdDrone = 0.4 * ramp (fnAge f) 90 }
      Rush ->
        -- speedK: the keeper's speed as a share of the screen's top speed, 0 to 1
        -- (held at 1, since the keeper may run a little faster than the screen).
        -- paceK: the screen's own speed as a share of its top, about 0.3 when it
        -- sets off and 1 at full speed. While the keeper is being taken back and
        -- the screen goes dark, everything falls to a fifth (k = 0.2).
        let d = fnDash f
            speedK = min 1 (dsSpeed d / dashTopSpeed)
            paceK = screenSpeed (dsClock d) / dashTopSpeed
            caughtK = if dsCaught d > 0 then 0.8 else 0
            k = 1 - caughtK
        -- The weather at 1.3 times its level in play; the drone under it; the wind
        -- from 0.08 standing to 0.44 at full speed; the beat fading in over the
        -- first 50 ticks of the rush ('dsClock') and quickening with the screen.
        in (weather (1.3 * k))
             { bdDrone = 0.3 * k
             , bdWind = (0.08 + 0.36 * speedK) * k
             , bdWindRate = 0.7 + 0.8 * speedK
             , bdPulse = 0.45 * ramp (dsClock d) 50 * k
             , bdPulseRate = 0.85 + 0.35 * paceK }
      -- Everything falls away under the real lamp's chord.
      -- The drone, wind and beat fade out over the first 60 ticks (1 s), at fixed
      -- rates (the beat at its top tempo, 1.2); the weather over 110 ticks.
      Arrival ->
        let k = 1 - ramp (fnAge f) 60
        in (weather (1.3 * (1 - ramp (fnAge f) 110)))
             { bdDrone = 0.3 * k, bdWind = 0.16 * k, bdWindRate = 0.9
             , bdPulse = 0.45 * k, bdPulseRate = 1.2 }
