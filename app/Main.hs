-- | The main IO boundary: window, clock and key events around the pure game.
-- Audio, Video and Save provide the other IO services used here.
--
--   borrowed-light                 play
--   borrowed-light --demo          watch the solver play every level, then take over
--   --pace=N                       with --demo, wait N extra ticks (60 a second) after each move
--   --leave=N                      with --demo, play only the selected level (the first, on a
--                                  new save), stop N moves short of winning it, and hand
--                                  over the keyboard
--   borrowed-light --level=N       select level N in the lobby, counting from 1; a secret
--                                  level, which the lobby never lists, is opened directly.
--                                  The levels before N count as cleared, and the save
--                                  keeps that, as it keeps any progress
--   --talk                         with --demo, keep the talks: the demo waits at each one
--                                  until Enter has moved it on, then plays on
--   --mute                         start with the sound off; M turns it on and off
--   --fullscreen                   fill the primary monitor instead of opening a window
--   --intro                        open on the opening film, whatever the save, to watch it again
--
-- Development flags, used to inspect exact moments of the game:
--   --dump-sounds=DIR  write every sound to DIR as a WAV file, and exit
--   --script=KEYS    run these keys instantly at startup, then hold the result:
--                    w a s d move, I J K L the arrows (the second keeper in a level for two),
--                    x shift, q echo, W A S D X the press alone, z undo,
--                    T Tab held, e Enter, p pause, r restart, ! Escape,
--                    i instructions, . one idle tick
--                    c Delete (cancel an order), H Enter held for one tick
--   --after=N        then run N more idle ticks
--   --fps            print measured frames per second to stdout
--   --trace          print the game state whenever phase, era, cell or inventory change
--                    (anything in its 'stateKey'), or the level or the mode does,
--                    and when each level's dead ends become known; also whether sound
--                    is on, whether the film was found, and every sound as it plays
--
-- What this module does. It reads the command line, the save file and the opening
-- film, builds the first 'World', and hands it to Gloss's 'playIO', which runs the
-- window's loop from then on through three callbacks written in 'main': one draws
-- the world, one records key events and resizes, and one lets time pass. Time
-- passing means: turning real time into fixed ticks of the pure game, 60 a second
-- ('onFrame', 'oneTick'); playing the sounds those ticks started; writing progress
-- to the save file when it changes; and spending a few milliseconds of each frame on
-- the searches that find each level's dead ends ('searchDeadEnds').
--
-- What it deliberately does not do: decide anything about the game. Which level is
-- played and how the player moves between title, film, hall and levels is
-- 'Game.Run', wrapped by 'Game.Orders' for the three saves and their desk;
-- the rules of a level are 'Game.Logic'; the effects, the sounds and the
-- courier's talk are worked out purely by 'Game.Fx', 'Game.Sound' and 'Game.Story',
-- each by comparing one tick with the one before. Turning keys into one input a tick
-- is 'Game.Input'. Drawing is 'Render', 'Lobby', 'OrdersScreen' and 'TalkBox'; making and playing
-- sounds is 'Audio'; the film is decoded by 'Video'; the save file is 'Save'; the
-- dead-end search itself is 'DeadSearch' in 'Game.Solution'. Nothing imports this
-- module: it is the program's entry point.
--
-- The state Gloss carries from one callback to the next is a pair: the monotonic
-- clock reading, in seconds, taken when the last step began (before the first step,
-- just before the window opened), and the 'World'. Gloss passes the step callback a
-- time step of its own, which is ignored; the time since the last step is measured
-- from the pair's clock instead, so the game runs 60 ticks to each second of real
-- time however Gloss happens to schedule its steps, apart from long stalls, which
-- are dropped ('onFrame').
--
-- Units. A tick is a 60th of a second ('ticksPerSecond'); the run, the effects and
-- the talk count everything in ticks. Real time is in seconds, as a 'Double'. The
-- window's size is in screen pixels. Everything is drawn on a canvas of 1280 x 800
-- canvas units centred on the origin, y growing upward, and scaled to fit the window
-- ('Render.fitted'). Map cells, (column, row) with rows counted downward, are left
-- to the modules above: this one never reads a cell's position.
module Main (main) where

import Audio
import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic (initialGame)
import Game.Orders
import Game.Run
import Game.Solution
import Game.Sound
import Game.Story
import Game.Types
import GHC.Clock (getMonotonicTime)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game
import qualified Lobby
import OrdersScreen (hallOverlay, ordersScreen)
import Title (instructionsPopup, titleMenu)
import Platform (exitWhenWindowCloses, keepLetterKeys)
import qualified Render
import Save
import TalkBox (endingLayer, introScreen, talkLayer)
import Video (frameAt, loadVideo, newPlayer, vdAudio, vdAudioRate, videoScale, videoTicks)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- | Everything the window loop keeps from one frame to the next, apart from the
-- clock reading Gloss carries beside it (see 'main'). It is never changed in place:
-- each callback takes a World and returns a new one, and Gloss keeps the latest.
--
-- The fields marked @!@ are strict: each is evaluated when the record is built,
-- rather than stored as an unevaluated expression (a thunk). A new World is built
-- every tick, so a field that is rarely looked at, such as 'wBank', would otherwise
-- pile up a chain of pending calculations. For a list, a @!@ would only evaluate its
-- first cell; the two list fields have none.
data World = World
  { wRun    :: !Run          -- ^ where the player is (title, film, hall, a level or its
                             --   ending), with the level's 'Game' ('Game.Run')
  , wFx     :: !Fx           -- ^ decorative state, derived tick by tick ('Game.Fx')
  , wKeys   :: !Keys         -- ^ keys held, and keys tapped since the last tick, as the
                             --   window's key events left them ('Game.Input')
  , wWindow :: !(Int, Int)   -- ^ the window's width and height in screen pixels,
                             --   following every resize
  , wBank   :: !Double       -- ^ unspent real time, in seconds
                             --   (always under a 60th of a second: see 'onFrame')
  , wScript :: [TickInput]   -- ^ overrides the keyboard while it lasts
                             --   (one input a tick: the demo's moves, or an inspection
                             --   script; empty in ordinary play. The demo's list is
                             --   worked out only as far as it has been played)
  , wFrozen :: !Bool         -- ^ inspection only: no ticks at all
                             --   (and no searches: the window holds one exact moment)
  , wBook   :: !Book         -- ^ her orders: the saves, the one in play kept up to date
  , wDesk   :: !Desk         -- ^ the orders screen
  , wSaved  :: !Book         -- ^ the orders as last written to the save file
  , wSlipAge :: !(Maybe Int) -- ^ ticks since the hall's saved slip came out, while it is out
  , wSlipMark :: !Int        -- ^ rooms the order in play had cleared when last slipped or entered
  , wCues   :: [Cue]         -- ^ sounds the ticks of the current step started, newest
                             --   first; the step callback plays them and empties the list
  , wQuit   :: !Bool         -- ^ QUIT was taken on the title: the step ends the game
                             --   once the orders are written
  , wMuted  :: !Bool         -- ^ True while the sound is off; M flips it, and the
                             --   mute flag starts the game with it True
  , wStory  :: !Story        -- ^ the courier's talk
                             --   (what she is saying now, and what she has said on
                             --   this visit to the level: 'Game.Story')
  , wTalks  :: !Bool         -- ^ off for the demo, whose keys would only be taken by it
  , wWaitTalk :: !Bool       -- ^ a demo that keeps its talks: it waits at each one
  , wIntroLen :: !Int        -- ^ ticks the opening film runs; 0 with no film, or once
                             --   it has been shown, so it plays once a session
  , wDead   :: !(IntMap.IntMap DeadSearch)
    -- ^ each level's search for the states it can no longer be won from, by its
    --   index in the hall, advanced a slice per frame ('searchDeadEnds'). A level
    --   with no entry is never told it is lost (see 'warnsOfDeadEnds').
    --   An 'IntMap' is a 'Map' specialised to 'Int' keys; the strict variant
    --   imported here evaluates each value as it is stored.
  }

-- | The 1280 x 800 the art is composed for, shrunk to fit a smaller display.
--
-- The argument is the screen's size in pixels, as 'getScreenSize' reports it; the
-- result is the window's size in pixels. The window is never made larger than
-- 1280 x 800, and keeps the canvas's 16:10 shape to within a pixel of rounding, so
-- the canvas fills it; black bars appear once the player gives it another shape. A
-- size of 0 or less in either direction, which no real screen has, gets the full
-- size.
windowFor :: (Int, Int) -> (Int, Int)
windowFor (screenW, screenH)
  | screenW <= 0 || screenH <= 0 = (1280, 800)
  | otherwise = (round (1280 * k), round (800 * k))
  where
    margin = 90 :: Float  -- title bar and menu bar
    -- The scale: 1 if the canvas fits, else the largest that leaves 20 pixels across
    -- for the window's borders and 'margin' pixels down. 'minimum' of the three.
    k = minimum [1, (fromIntegral screenW - 20) / 1280, (fromIntegral screenH - margin) / 800]

-- | Start the game: read the command line, the save and the film, build the first
-- 'World', and hand it to Gloss's 'playIO' with the three callbacks that run the
-- game from then on. The order in which Gloss calls them is described inside the
-- step callback, the last of the three.
main :: IO ()
main = do
  -- Before anything makes a window: an input method would otherwise take the letter keys.
  keepLetterKeys
  args <- getArgs
  -- The sound dump runs alone, before any window, save or level is touched.
  -- 'stripPrefix p a' is the rest of @a@ after @p@, if @a@ starts with it, so
  -- 'mapMaybe' keeps the value of each matching argument; 'listToMaybe' takes the
  -- first, or Nothing. '>>' runs one action, then the next.
  case listToMaybe (mapMaybe (stripPrefix "--dump-sounds=") args) of
    Just dir -> dumpSounds dir >> exitSuccess
    Nothing -> pure ()
  -- Fullscreen uses the primary monitor's size. Windowed play keeps its usual
  -- fitted size; resize events update wWindow once the display is open.
  screenSize <- getScreenSize
  let fullscreen = "--fullscreen" `elem` args
      windowSize = if fullscreen then screenSize else windowFor screenSize
  -- Every level the run can reach, campaign first and secret levels after, each
  -- parsed and checked from its two maps ('levelHall' in 'Game.Level'). A malformed
  -- map is a mistake in the code, so the game stops with the parser's reason: 'die'
  -- prints to stderr and exits with a failure. 'either f g' applies @f@ to a 'Left'
  -- and @g@ to a 'Right'; 'die . ("..." ++)' is a composition, adding the prefix to
  -- the message and then dying with it.
  levels <- either (die . ("Level is malformed: " ++)) pure levelHall
  -- 'let' in a do block names pure values; they can be used by every line after it.
  -- 'flag name' is the value given as --NAME=VALUE, the first if there are several.
  let flag name = listToMaybe (mapMaybe (stripPrefix ("--" ++ name ++ "=")) args)
      -- An inspection script, as one 'TickInput' a tick. 'fmap' over a 'Maybe'
      -- leaves Nothing as Nothing.
      prepared = fmap scriptInputs (flag "script")
      -- Idle ticks after the script. 'read' stops the program with an error if the
      -- value is not a number.
      after = maybe 0 read (flag "after") :: Int
      -- Inspecting: the world is frozen once the script has run, and silent.
      inspect = isJust prepared
      -- Every level, secret ones included.
      total = length levels
      -- The level asked for, counting from 1; Nothing if the flag is missing or not
      -- a number. '>>=' on 'Maybe' feeds a 'Just' on and stops at a Nothing.
      requested = flag "level" >>= readMaybe
  -- Read and normalise all three orders, migrating the old single save if needed.
  book0 <- loadBook total
  -- The optional opening film is played when an order starts its first delivery.
  video <- loadVideo
  -- A decoder for the film, if there is one. 'traverse' runs 'newPlayer' inside the
  -- 'Maybe': Nothing gives Nothing, and Just v gives Just the new player.
  player <- traverse newPlayer video
  -- Restore progress from the active order; an empty book starts at level zero.
  let savedC = maybe 0 odCompleted (activeOrder book0)
      savedS = maybe 0 odSelected (activeOrder book0)
      -- Command-line level numbers start at one; clamp the converted index.
      startAt = max 0 (min (total - 1) (fromMaybe (savedS + 1) requested - 1))
      -- An explicit level unlocks those before it in this active order.
      completedAtStart = max savedC (if isJust requested then startAt else 0)
      -- Opening on a level or on the film plays an order, so there must be one.
      book1
        | isJust requested = withProgress completedAtStart startAt book0
        | watchFilm = ensureActive book0
        | otherwise = book0
      -- The run as it opens: the saved place may be clamped back into the hall.
      -- The if-expression chooses the function, which is then given the three
      -- arguments. 'startRunAt' opens a secret level asked for by number straight
      -- away; both otherwise start on the title.
      start1 = (if isJust requested then startRunAt else startRunWithProgress) levels startAt completedAtStart
      -- --intro replays the film, but only if there is a film to play.
      watchFilm = "--intro" `elem` args && isJust video
      -- With --intro the run opens inside the film, before level 1, whatever the save
      -- says. 'start1 { ... }' is record update syntax: a copy of start1 with those
      -- three fields replaced. A definition with guards takes the first branch whose
      -- condition holds.
      start0
        | watchFilm = start1 { runIndex = 0, runMode = Intro 0 0, runGame = initialGame (levels !! 0) }
        | otherwise = start1
      demo = "--demo" `elem` args
      talkDemo = demo && "--talk" `elem` args
      -- Each level's 'Render.Scene': the pictures that are expensive to build and
      -- never change, baked once. Its arguments are the title above the map (the
      -- level's place in the hall, and its name), whether another listed level
      -- follows, and whether to write the keys under the map. The list is lazy, so a
      -- scene is only built the first time a picture needs it, and then kept,
      -- because the display callback below holds on to this list for the whole
      -- session.
      scenes =
        [ Render.mkScene (place i l, lvName l) (i + 1 < shownTotal) (teaches i l) l
        | (i, l) <- zip [0 ..] levels ]
      -- The hall counts only the levels it lists; a secret level gives its number alone.
      shownTotal = lastVisible levels + 1
      -- For example "2 of 3" for a listed level, and "4" for a secret one.
      place i l
        | lvSecret l = show (i + 1)
        | otherwise = show (i + 1) ++ " of " ++ show shownTotal
      -- The keys are written under the map for the first two levels only.
      teaches i l = i < 2 && not (lvSecret l)
      -- What an ending leads into, for the card at its end.
      -- The next level's number, counting from 1, and its name; two empty strings
      -- after the last level.
      beyond i
        | i + 1 < total = (show (i + 2), lvName (levels !! (i + 1)))
        | otherwise = ("", "")
      world0 = World
        { wRun = start0, wFx = initialFx, wKeys = noKeys, wWindow = windowSize, wBank = 0
        -- The demo's inputs: with --leave=N, the selected level's route stopped N
        -- moves short ('handOver'); otherwise every level from the selected one on
        -- ('demoInputs'). Both wait --pace=N extra ticks after each move.
        , wScript = if demo
            then case flag "leave" >>= readMaybe of
              Just n -> handOver (maybe 0 read (flag "pace")) n (levels !! runIndex start0)
              Nothing -> demoInputs (maybe 0 read (flag "pace")) (drop (runIndex start0) levels)
            else []
        , wFrozen = False
        , wBook = book1
        , wDesk = closedDesk
        , wSaved = book0
        , wSlipAge = Nothing
        , wSlipMark = savedC
        -- Opening inside the film skips the change from the title into it, which is
        -- what starts the film's soundtrack ('cues'), so its cue is queued here.
        -- '[ x | c ]' is a list comprehension with only a condition: [x] when @c@
        -- holds, [] when not.
        , wCues = [ CueIntro | watchFilm ]
        , wMuted = "--mute" `elem` args
        , wQuit = False
        -- A secret level opened by number is entered at once, as if from the hall.
        , wStory = if demo && not talkDemo then noStory else notice levels start0 { runMode = Lobby } start0 noStory
        , wTalks = not demo || talkDemo
        , wWaitTalk = talkDemo
        -- The demo never shows the film: with a length of 0 the opening ends on the
        -- tick it begins ('oneTick'). With no film, 'maybe' gives 0 as well.
        , wIntroLen = if demo then 0 else maybe 0 videoTicks video
        -- Made here, but nothing is worked out until 'searchDeadEnds' runs them.
        -- Only a level 'warnsOfDeadEnds' accepts gets a search; 'zip [0 ..] levels'
        -- pairs each level with its index from 0.
        , wDead = IntMap.fromList [ (i, startDeadSearch l) | (i, l) <- zip [0 ..] levels, warnsOfDeadEnds l ]
        }
      -- An inspection script is played instantly through the same ticks as the
      -- keyboard, talk and film included, and then the world holds still.
      -- 'iterate f x' is the endless list x, f x, f (f x), ..., so its element
      -- number n is the world after n ticks; each tick takes one input off
      -- 'wScript', so after 'length inputs' ticks the script is used up. The sounds
      -- those ticks queued are dropped: the moment held is a still picture.
      world1 = case prepared of
        Nothing -> world0
        Just keys ->
          let inputs = keys ++ replicate after noInput
          in (iterate (oneTick levels) world0 { wScript = inputs } !! length inputs)
               { wFrozen = True, wCues = [] }
  -- A frozen world runs no searches, so the level it shows has its search finished
  -- here, before the window opens, and the moment it holds can show the dead-end
  -- line exactly as play would. Inspection stays the same from run to run.
  world2 <- if inspect then finishShownSearch world1 else pure world1
  -- The sounds of the title, the hall and the levels are made here, before the
  -- window opens; the ending's are made on a thread of their own while the game
  -- runs ('startAudio'). 'sound' is False if there is no output device, or on a
  -- system other than Windows. Inspection opens no device at all, so it is silent.
  sound <- if inspect then pure False else startAudio
  -- 'when' runs an action only if its condition holds, and 'forM_' over a 'Maybe'
  -- runs once for a Just and not at all for Nothing: so if sound is on and there is
  -- a film, its soundtrack is handed to the mixer.
  when sound $ forM_ video (\v -> loadClip (vdAudioRate v) (vdAudio v))
  when ("--trace" `elem` args) $ do
    putStrLn (if sound then "sound on" else "sound off: no output device, or not Windows")
    putStrLn (maybe "no opening film: assets/intro.blv not found" (\v -> "opening film: " ++ show (videoTicks v) ++ " ticks") video)
    -- Output to a pipe or a file is buffered; flushing makes each line appear at once.
    hFlush stdout
  -- The clock reading the first step will measure from, in seconds.
  start <- getMonotonicTime
  -- Mutable cells ('IORef's) shared by the callbacks below, for what is not part of
  -- the world. For --fps: when the current second of counting began, and the frames
  -- drawn in it.
  counter <- newIORef (start, 0 :: Int)
  -- True until the first frame has been drawn.
  firstFrame <- newIORef True
  -- Seconds spent on dead-end searches since the last frame was drawn.
  searchSpent <- newIORef (0 :: Double)
  -- Called once a frame. With --fps, counts the frame, and once at least a second
  -- has gone by prints the rate over that time (this frame included) and starts
  -- counting again.
  let countFrame = when ("--fps" `elem` args) $ do
        now <- getMonotonicTime
        (since, frames) <- readIORef counter
        if now - since >= 1
          then do
            putStrLn ("fps " ++ show (fromIntegral (frames + 1) / (now - since) :: Double))
            hFlush stdout
            writeIORef counter (now, 0)
          else writeIORef counter (since, frames + 1)
  playIO
    -- FullScreen selects the primary monitor through GLFW. Otherwise create the
    -- titled window with its top left corner 40 pixels in from the screen's.
    (if fullscreen then FullScreen else InWindow "Borrowed Light" windowSize (40, 40))
    -- What shows wherever nothing is drawn.
    black
    -- How many times a second Gloss means to call the step callback. The game's own
    -- ticks are counted in 'onFrame' instead, so this only sets how often it looks.
    60
    -- The state Gloss carries: the clock reading and the world (see the header).
    (start, world2)
    -- The display callback: the world as a picture, in window pixels. It changes no
    -- game state: only the three IORefs above and, while the film shows, the film
    -- decoder's own buffer. Gloss passes it the pair, and the clock reading is not
    -- needed here, hence the '_'.
    (\(_, w) -> do
        first <- readIORef firstFrame
        -- Once, on the first frame, when the window exists. Under the GLFW backend
        -- this does nothing ('Platform'), but the call is kept in its place.
        when first (writeIORef firstFrame False >> exitWhenWindowCloses)
        countFrame
        -- A new frame: its dead-end searches have spent nothing yet.
        writeIORef searchSpent 0
        let run = wRun w
            i = runIndex run
            g = runGame run
            -- Known only once this level's search has finished; until then the
            -- dead-end line simply does not show. Never a wait: looking a state up
            -- in a finished search uses its hash table, probing onward on collisions.
            -- 'maybe False f m' is False for a level with no search and @f s@ for
            -- @Just s@; (`knownDeadEnd` key) is a section, the function that takes a
            -- search @s@ and gives @knownDeadEnd s key@.
            deadEnd key = maybe False (`knownDeadEnd` key) (IntMap.lookup i (wDead w))
            -- While she talks about something in the other era, the board shows
            -- that era, as if Tab were held.
            -- 'Render' takes Tab as held when last tick's keys ('gPrev') have
            -- 'tiPeek' set, so the copy drawn here has it set: a record update
            -- inside a record update. Only this picture sees it; the run's own game
            -- is unchanged. The branch 'Just lk | ...' is a case alternative with a
            -- guard: it matches only when she points at something in the other era.
            shown = case lookNow (wStory w) of
              Just lk | lkEra lk /= gEra g -> g { gPrev = (gPrev g) { tiPeek = True } }
              _ -> g
            -- The talk, drawn over a level. 'Render.renderWith' gives it the board's
            -- stand @f@: 0 while the level is being introduced, 1 once closed in.
            talk f = talkLayer (wStory w) f (fxClock (wFx w))
        -- What the run shows decides what is drawn. Each branch scales the canvas to
        -- the window itself. 'pure' wraps a plain picture as the IO result.
        case runMode run of
          -- The title belongs to the selected level's game, whose phase is 'Menu',
          -- and for that phase 'Render' draws the title screen (and the
          -- instructions, while they are open) instead of the board.
          -- Over it the menu, and over that the instructions while they are open.
          Title -> pure (Pictures
            [ Render.render deadEnd (scenes !! i) (levels !! i) g (wFx w) (wWindow w)
            , Render.fitted (wWindow w) (Pictures
                [ titleMenu (gTitleChoice g) (anyOrders (wBook w)) (wMuted w)
                , if gShowInstructions g then instructionsPopup else Blank ]) ])
          -- Three order tickets, drawn over the shared title background.
          Orders -> pure (Render.fitted (wWindow w) (ordersScreen levels (wBook w) (wDesk w) (fxClock (wFx w))))
          -- The film: decode the picture for this tick, if there is a film, and
          -- frame it, with the bar that says how to skip it. With no film the
          -- opening ends on the tick it starts ('oneTick'), so it is never drawn, and
          -- the scale of 2 is only a default.
          Intro age held -> do
            frame <- traverse (`frameAt` age) player
            pure (Render.fitted (wWindow w) (introScreen (maybe 2 videoScale video) frame age held))
          -- The hall; its selected card pulses with the effects clock, in ticks.
          Lobby ->
            let order = fromMaybe newOrder (activeOrder (wBook w))
                saved = durableKey (wSaved w) == durableKey (wBook w)
                slip = fmap (\age -> (age, saved, order)) (wSlipAge w)
                over = hallOverlay levels (bkActive (wBook w) + 1) slip
            in pure (Lobby.renderLobby levels run (fromIntegral (fxClock (wFx w))) over (wWindow w))
          -- A level, as 'shown', with the talk over it.
          InLevel -> pure (Render.renderWith talk deadEnd (scenes !! i) (levels !! i) shown (wFx w) (wWindow w))
          -- The won level's ending, and the card naming the level beyond; over it,
          -- the card that says every order is delivered, and the talk.
          Ending f -> pure (Pictures
            [ Render.renderEnding (scenes !! i) (levels !! i) (beyond i) f g (wFx w) (wWindow w)
            , Render.fitted (wWindow w) (endingLayer (wStory w) f (fxClock (wFx w))) ]))
    -- The event callback: a key went down or up, or the window was resized. Nothing
    -- happens to the game here; a key is only recorded, and the next tick reads it.
    -- The clock reading is passed through unchanged.
    (\event (t, w) -> pure (t, onEvent event w))
    -- The step callback: time has passed. Gloss's time step, the first argument, is
    -- not used; the time since the last step is measured from the pair's clock.
    (\_ (t, w) -> do
        now <- getMonotonicTime
        -- Every tick due since the last step, each one a full tick of the run, the
        -- effects, the talk and the sounds ('onFrame').
        let w' = onFrame levels (now - t) w
        when sound $ do
          -- The mixer is told every step whether the sound is off; M flips 'wMuted'.
          setMuted (wMuted w')
          -- 'wCues' is newest first, so it is reversed to play the sounds in the
          -- order the ticks started them. '$' before the lambda saves brackets.
          forM_ (reverse (wCues w')) $ \c -> do
            playCue c
            when ("--trace" `elem` args) (putStrLn ("sound " ++ show c) >> hFlush stdout)
          -- How loud each looping bed (the sea, the storm and the rest) should be
          -- now, for what the run shows ('beds' in 'Game.Sound').
          setBeds (beds levels (wRun w'))
        -- The cues have been played, or there is no sound to play them, so they are
        -- dropped before the world is kept; then progress is saved if it changed.
        persisted <- persistWorld w' { wCues = [] }
        -- QUIT on the title, now that the orders are written.
        when (wQuit persisted) exitSuccess
        -- Gloss's loop draws a frame, holds it until at least a hundredth of a
        -- second has passed since the last one, runs this step (none, one or two
        -- times, to keep 60 ticks a second), then swaps buffers, which waits for
        -- the screen's refresh. The searches go last in the step, and share one
        -- budget a frame however many steps the frame has: the display callback
        -- above resets what has been spent. Time spent here is time the next
        -- frame's hold does not sleep, so a frame that draws quickly loses nothing.
        --
        -- In more detail, each pass of the loop (the GLFW backend's, in
        -- vendor/gloss-1.13.2.2) does, in order:
        --   1. display: the display callback above makes the picture, OpenGL draws
        --      it, and a minor garbage collection follows (a major one at most once
        --      a second);
        --   2. the hold ('animateEnd'): it sleeps until a hundredth of a second has
        --      passed since the previous pass's hold ended, so at most 100 frames a
        --      second;
        --   3. this step callback, as many times as Gloss's own clock says steps are
        --      due at 60 a second: usually once, none after a quick pass, two or
        --      more after a slow one. A second call in the same pass finds little
        --      time has passed since the first, so it seldom runs a tick, and while
        --      search work is left the first call has used this frame's budget;
        --   4. swapBuffers, which shows the frame and, since Gloss asks for v-sync,
        --      waits for the screen's refresh;
        --   5. the key and resize events that have arrived, each through the event
        --      callback, so a key pressed now is first seen by the next pass's step.
        spent <- readIORef searchSpent
        -- What is left of this frame's budget, for what the run now shows. At or
        -- below zero, 'searchDeadEnds' runs nothing.
        (w'', used) <- searchDeadEnds (searchBudget (runMode (wRun persisted)) - spent) persisted
        writeIORef searchSpent (spent + used)
        -- With --trace: each level whose search finished during this step, that is,
        -- finished now but not in the world the step began with.
        when ("--trace" `elem` args) $
          forM_ (IntMap.toList (wDead w'')) $ \(n, s) ->
            when (deadSearchFinished s && maybe False (not . deadSearchFinished) (IntMap.lookup n (wDead w))) $ do
              putStrLn ("dead ends of level " ++ show (n + 1) ++ " known, " ++ show (now - start) ++ " s after opening")
              hFlush stdout
        let game = runGame (wRun w'')
        -- With --trace: the state, whenever this step changed the level's state key,
        -- the level, or the mode. 'filmless' stops every tick of the film counting
        -- as a change of mode.
        when ("--trace" `elem` args && (stateKey game /= stateKey (runGame (wRun w)) || runIndex (wRun w'') /= runIndex (wRun w) || filmless (runMode (wRun w'')) /= filmless (runMode (wRun w)))) $ do
          putStrLn ("mode=" ++ show (runMode (wRun w'')) ++ " level=" ++ show (runIndex (wRun w'') + 1) ++ " " ++ show (stateKey game) ++ " remaining=" ++ show (gRemaining game))
          hFlush stdout
        -- The pair Gloss keeps: the clock reading taken as this step began, so the
        -- next step's measure includes the time this one took, and nothing is lost.
        pure (now, w''))

-- | Whether the game looks for a level's dead ends at all, as it always has. The
-- search knows one keeper only, so a level for two is never told it is lost. A level
-- with three or more crates could reach too many states to search, unless it is
-- built of whole rooms: its one-way gates keep its states a sum over the rooms
-- rather than a product.
--
-- Used once, in 'main', to choose which levels get a search. In the hall as it is,
-- the three campaign levels do, and the secret level for two does not.
warnsOfDeadEnds :: Level -> Bool
warnsOfDeadEnds level =
  not (isJust (lvSpawn2 level)) && not (Set.size (lvCrates level) >= 3 && not (roomedLevel level))

-- | Seconds of dead-end search allowed in one frame, by what the screen shows. In
-- play a frame takes three or four milliseconds to draw, and Gloss holds each frame
-- for at least ten, so two milliseconds fit in the time it would otherwise sleep.
-- The title, the hall and the opening film draw less and nobody is steering, so
-- they take six. The Long Way Round takes about a second and a half of searching
-- in all, so even straight from the title its dead ends are known a few seconds
-- after the game opens, before anyone can walk into one.
searchBudget :: RunMode -> Double
searchBudget mode = case mode of
  -- In a level, and in its ending.
  InLevel -> 0.002
  Ending _ -> 0.002
  -- The title, the opening film and the hall.
  _ -> 0.006

-- | Units of search run between looks at the clock: about a tenth of a millisecond
-- of work, so a frame overshoots its budget by little.
-- A unit is what 'advanceDeadSearch' counts: one state expanded (six actions through
-- the real 'stepGame', a few microseconds in all), or a comparably small piece of
-- the backward walk or the packing that follow the expansion.
searchSlice :: Int
searchSlice = 16

-- | Advance the dead-end searches for up to @budget@ seconds of wall-clock time, and
-- say how long it took. The level being played, or chosen on the title or in the
-- hall, goes first; then the levels after it, which the player is likely to reach
-- next; then the ones before. A frozen world runs nothing, so it stays the same.
--
-- The clock is read after each slice, so at least one slice runs whenever the budget
-- is above zero and a search is unfinished, and the budget is overrun by at most one
-- slice. Returns the world with its searches advanced, and the seconds spent.
searchDeadEnds :: Double -> World -> IO (World, Double)
searchDeadEnds budget w
  | wFrozen w || budget <= 0 = pure (w, 0)
  | otherwise = do
      begun <- getMonotonicTime
      -- A loop in IO: run a slice of the next unfinished search, store it, and go
      -- round again while time is left. It stops early once every search is done.
      let go searches = case nextSearch searches of
            Nothing -> pure searches
            Just (i, s) -> do
              -- Evaluating the advanced search is what runs it, here and not later
              -- in some frame that happens to look at it.
              -- 'evaluate' forces its argument to its outermost constructor as an
              -- IO step, so the slice's work is done before the clock is read again,
              -- and the clock times it.
              s' <- evaluate (advanceDeadSearch searchSlice s)
              let searches' = IntMap.insert i s' searches
              now <- getMonotonicTime
              if now - begun < budget then go searches' else pure searches'
      searches <- go (wDead w)
      ended <- getMonotonicTime
      pure (w { wDead = searches }, ended - begun)
  where
    -- The index of the level the run is on: the one played, or the one chosen.
    current = runIndex (wRun w)
    -- The next search to advance, with its level's index. 'partitionWithKey' splits
    -- the map in two: the levels before the current one, and the rest (the current
    -- one and those after). 'lookupMin' gives the entry with the smallest key, if
    -- any, so the first unfinished level from the current one on comes first. '<|>'
    -- on 'Maybe' takes the left one if it is a Just, else the right: only when
    -- nothing from the current level on is left do the levels before it get a turn,
    -- again from the lowest.
    nextSearch searches =
      let (before, after) = IntMap.partitionWithKey (\i _ -> i < current) searches
          -- The searches still running. 'not . deadSearchFinished' is a
          -- composition: finished, then negated.
          unfinished = IntMap.filter (not . deadSearchFinished)
      in IntMap.lookupMin (unfinished after) <|> IntMap.lookupMin (unfinished before)

-- | Finish the search of the level the world shows, all at once.
--
-- Only for inspection, before the window opens (see 'main'). A level with no
-- search is left as it is: 'IntMap.adjust' changes an entry only if it is there.
-- 'evaluate' forces the new map, and a strict 'IntMap' evaluates the values it
-- stores, so the whole search runs here. For The Long Way Round that is about a
-- second.
finishShownSearch :: World -> IO World
finishShownSearch w = do
  searches <- evaluate (IntMap.adjust finishDeadSearch (runIndex (wRun w)) (wDead w))
  pure w { wDead = searches }

-- | Write the orders when what they hold changes: at once for progress, a new or
-- cancelled order, or a change of order; for the time on the road, every ten
-- seconds of it, and whenever the run is back at the title or the orders. Each
-- write stamps the order in play with the time. A write that fails is tried again.
persistWorld :: World -> IO World
persistWorld world
  | not due = pure world
  | otherwise = do
      now <- nowStamp
      let book = stampActive now (wBook world)
      written <- saveBook book
      pure (if written then world { wBook = book, wSaved = book } else world)
  where
    ticksOf = maybe 0 odTicks . activeOrder
    gap = abs (ticksOf (wBook world) - ticksOf (wSaved world))
    resting = runMode (wRun world) `elem` [Title, Orders]
    due = durableKey (wBook world) /= durableKey (wSaved world)
          || gap >= 10 * ticksPerSecond || (resting && gap > 0)

-- | The solver playing each level through the same keys a player would press.
--
-- @pace@ is how many idle ticks to add after each move, so a watcher can follow;
-- @levels@ are the levels to play, from the one selected on. The result is one
-- input a tick, for 'wScript'. It is built lazily, as the demo plays: a level's
-- route is only searched for when the script reaches it, so the demo can stop for
-- several seconds at the start of a large level while its search runs.
demoInputs :: Int -> [Level] -> [TickInput]
-- 'zipWith3' walks three lists side by side: each level's start, the level, and
-- the level after it (Just the next one, or Nothing after the last). It stops at the
-- end of the shortest list; 'concat' joins the levels' inputs into one list.
demoInputs pace levels = concat (zipWith3 play starts levels (map Just (drop 1 levels) ++ [Nothing]))
  where
    -- Enter held for one tick.
    press = noInput { tiEnter = True }
    -- A second of nothing, so a level can be seen before the solver moves.
    pauseTicks = replicate 60 noInput
    -- One move's ticks, then the extra wait.
    paced action = actionInputs action ++ replicate pace noInput
    -- The first level is started from the title, through the orders and the hall
    -- when there are any; one after an ending starts itself; any other is started
    -- from the last one's won plate.
    -- Three fresh Enter presses cover title, orders and hall; on a new order
    -- the film is skipped by the demo and later presses do nothing in play.
    -- Later won plates wait 150 ticks before Enter; an ending starts itself.
    starts = Just [press, noInput, press, noInput, press]
           : [ if null (lvPassage l) then Just (replicate 150 noInput ++ [press]) else Nothing | l <- levels ]
    -- One level's inputs: its start, a pause, the route, one idle tick on which the
    -- last move completes, and the ending if it has one and a level waits beyond.
    -- 'maybe [] id start' is the start's inputs, or none.
    play start level next =
      maybe [] id start
      ++ pauseTicks
      ++ moves level ++ [noInput]
      ++ (if not (null (lvPassage level)) && isJust next then ending level else [])
    -- Wait for the wall to open, step into the passage, and run it to the end,
    -- changing era at each barrier as a keeper reading the way ahead would.
    -- The passage of The Long Way Round, the one level with an ending, starts in
    -- the wall just above its lamp, where the keeper stands after winning, so one
    -- step up enters it. 'revealTicks' is the time from the win until the keeper may
    -- walk; the rush is followed by the arrival at the real lighthouse.
    ending level = replicate (revealTicks level + 50) noInput ++ actionInputs (Go DirUp)
             ++ [noInput] ++ rushInputs level ++ replicate (arrivalTicks + 30) noInput
    -- A level for two is played by the search for two, both keepers' keys.
    -- 'drGoal' is the cheapest win and its route, if there is one; no route means no
    -- moves, and the demo stands still in that level.
    moves level
      | isJust (lvSpawn2 level) =
          case drGoal (duoSearch level (const True) (startPlaying level)) of
            Just (_, actions) -> concatMap (\a -> duoInputs a ++ replicate pace noInput) actions
            Nothing -> []
      | otherwise = concatMap paced (route level)
    -- A route pinned in 'knownRoute' for a level too big to search, else the
    -- cheapest one 'search' finds, else nothing. A component of a pair matched by
    -- '_' is never evaluated, so for a level with a known route the search never
    -- runs. '\_ _ -> True' allows every action from every state.
    route level = case (knownRoute level, srGoal (search level (\_ _ -> True) (startPlaying level))) of
      (Just actions, _) -> actions
      (_, Just (_, actions)) -> actions
      _ -> []

-- | One tick of the campaign and its effects together, for a tick the run is free to
-- take (no talk is holding it). The effects are derived from the level's game before
-- and after the tick, as 'stepFx' always is. Takes and returns the campaign and effects
-- as a pair.
tickBoth :: [Level] -> (Campaign, Fx) -> TickInput -> (Campaign, Fx)
tickBoth levels (cp, fx) input =
  let cp' = stepCampaign levels input cp
      r = cpRun cp
      r' = cpRun cp'
      fx' = stepFx (runGame r) (runGame r') fx
      -- Coming into a level, from the hall or the level before, introduces it
      -- again; restarting the same level does not.
      -- (Coming from the film or from an ending counts too: neither is 'InLevel'.)
      -- 'fxFocus' at 0 puts the board back to its introducing stand ('Game.Fx').
      entered = inLevel r' && (not (inLevel r) || runIndex r' /= runIndex r)
      inLevel x = case runMode x of
        InLevel -> True
        _ -> False
  in (cp', if entered then fx' { fxFocus = 0 } else fx')

-- | Fixed ticks from real time. A long stall is dropped rather than replayed,
-- so the game never fast-forwards through input the player did not see land.
--
-- @elapsed@ is the real time since the last step, in seconds. It is added to the
-- bank; every whole 60th of a second in the bank is one tick, run through
-- 'oneTick', and the remainder (under a tick) waits for the next step, so on
-- average the game runs exactly 60 ticks a second. If more than six ticks (a tenth
-- of a second) are due, six run and the rest of the time is forgotten.
onFrame :: [Level] -> Double -> World -> World
onFrame levels elapsed w
  | wFrozen w = w
  -- Record update binds tighter than function application, so this is 'spend'
  -- applied to the tick count and to w with its bank already reduced.
  | otherwise = spend (ticksDue :: Int) w { wBank = bank - fromIntegral ticksDue * dt }
  where
    -- One tick, in seconds.
    dt = 1 / fromIntegral ticksPerSecond
    banked = wBank w + elapsed
    -- Whole ticks in the bank, rounded down.
    wanted = floor (banked / dt)
    ticksDue = min maxCatchUp wanted
    -- After a stall the bank is cut to exactly the ticks that will run, so it is
    -- left empty; otherwise it keeps its remainder.
    bank = if wanted > maxCatchUp then fromIntegral ticksDue * dt else banked
    maxCatchUp = 6
    -- Run n ticks, one after another. The first equation matches the literal 0, so
    -- the recursion stops there.
    spend 0 s = s
    spend n s = spend (n - 1) (oneTick levels s)

-- | One fixed tick of the whole game: take an input, let the courier's talk have it
-- or the run, step the effects, end the film when it has run its length, let the
-- talk notice what happened, and queue the sounds this tick started.
oneTick :: [Level] -> World -> World
oneTick levels w =
  -- This tick's input: the next one in the script while there is one, else the
  -- keyboard. While a script lasts, key events are still recorded, but taps are
  -- forgotten each tick, so only keys still held count once it ends.
  let (input, script) = case wScript w of
        -- A demo that keeps its talks waits at each one, and the keyboard answers it.
        pending@(_ : _) | wWaitTalk w, isJust (stTalk (wStory w)) -> (tickInput (wKeys w), pending)
        (i : rest) -> (i, rest)
        []         -> (tickInput (wKeys w), [])
      -- An open talk takes the keys, and the board waits for it.
      -- 'free' says whether the run may step this tick. Guards on a pattern binding
      -- work as on a function: the first condition that holds gives the pair.
      (story0, free)
        | wTalks w = talkTick input (wRun w) (wStory w)
        | otherwise = (wStory w, True)
      -- A held run does not step, but 'holdRun' remembers this tick's keys, so they
      -- need a fresh press once the talk closes; the effects still step, so the
      -- board's ambient animation goes on behind the talk.
      before = Campaign (wBook w) (wDesk w) (wRun w)
      (after0, fx0)
        | free = tickBoth levels (before, wFx w) input
        | otherwise = let held = holdRun input (wRun w)
                      in (before { cpRun = held }, stepFx (runGame (wRun w)) (runGame held) (wFx w))
      r0 = cpRun after0
      -- The title's sound and quit lines are the app's: Enter on them, this tick.
      taken = if free then titleChoiceTaken input (wRun w) else Nothing
      -- The film ends the run's opening once it has run its length, and is not
      -- shown again this session. With no film, the opening ends at once.
      -- The first level then begins, introduced from the start ('fxFocus' 0).
      (r, fx) = case runMode r0 of
        Intro age _ | age >= wIntroLen w -> (finishIntro levels r0, fx0 { fxFocus = 0 })
        _ -> (r0, fx0)
      -- The film was showing before this tick and is not now: it ended, or was
      -- skipped with Escape or a held Enter or Space.
      filmOver = isIntro (runMode (wRun w)) && not (isIntro (runMode r))
      -- After the run has moved: a talk this tick set off, such as arriving in a
      -- level or taking the wick ('notice').
      story
        | wTalks w = notice levels (wRun w) r story0
        | otherwise = story0
      book = tally (wRun w) r (cpBook after0)
      desk = cpDesk after0
      -- Run, dialogue and order-desk transitions each contribute their cues.
      heard = cues levels (wRun w, wFx w) (r, fx) ++ storyCues (wStory w) story
              ++ deskCues levels book (wDesk w) desk
      -- The hall prints a slip when it is reached with a room cleared since the
      -- last one, or since the order was taken up.
      cleared = maybe 0 odCompleted (activeOrder book)
      menu m = m `elem` [Title, Orders]
      (slip, mark)
        | menu (runMode (wRun w)) && not (menu (runMode r)) = (Nothing, cleared)
        | runMode r == Lobby && playing (runMode (wRun w)) && cleared > wSlipMark w = (Just 0, cleared)
        | runMode r == Lobby = (fmap (+ 1) (wSlipAge w), wSlipMark w)
        | otherwise = (Nothing, wSlipMark w)
  -- The new world. 'wCues' is kept newest first, so this tick's sounds go on the
  -- front, reversed; adding to the front of a list is cheap, adding to its end is
  -- not. 'afterTick' forgets the taps this tick has now seen.
  in w { wRun = r, wFx = fx, wScript = script, wKeys = afterTick (wKeys w)
       , wCues = reverse heard ++ wCues w, wStory = story
       , wIntroLen = if filmOver then 0 else wIntroLen w
       , wBook = book, wDesk = desk, wSlipAge = slip, wSlipMark = mark
       , wMuted = if taken == Just titleSound then not (wMuted w) else wMuted w
       , wQuit = wQuit w || taken == Just titleQuit }
  where
    -- Whether a mode is the opening film, whatever its ticks.
    isIntro m = case m of
      Intro _ _ -> True
      _ -> False
    playing m = case m of
      InLevel -> True
      Ending _ -> True
      _ -> False

-- | Record one window event in the world. Equations are tried from the top: a
-- resize, M (which toggles the sound here, not through the game's keys), any other
-- key going down or up, and anything else (mouse motion, for one) ignored. A mouse
-- button also arrives as a key, which 'gameKey' does not know, so it is ignored too.
onEvent :: Event -> World -> World
onEvent (EventResize size) w = w { wWindow = size }
-- A guard on an equation: if the character is not M, the next equations are tried.
onEvent (EventKey (Char c) Down _ _) w | c `elem` "mM" = w { wMuted = not (wMuted w) }
onEvent (EventKey key Down _ _) w = withKey key (keyDown) w
onEvent (EventKey key Up _ _) w = withKey key (keyUp) w
onEvent _ w = w

-- | Apply 'keyDown' or 'keyUp' (@f@) to the world's keys, if the window's key is one
-- the game uses; any other key leaves the world as it is.
withKey :: Key -> (GameKey -> Keys -> Keys) -> World -> World
withKey key f w = case gameKey key of
  Just k  -> w { wKeys = f k (wKeys w) }
  Nothing -> w

-- | The game's name for a key Gloss reports, or Nothing for a key the game does not
-- use. Several keys share a name where they mean the same thing.
--
-- Capital letters are what the GLUT backend reported with Shift or Caps Lock on,
-- since it gave the character typed. The patched GLFW backend the game now uses
-- reports every letter in lower case, by its place on the keyboard rather than the
-- layout's letter. Both backends report Space and Enter as special keys; their
-- character forms are mapped as well, and a form that never arrives costs nothing.
gameKey :: Key -> Maybe GameKey
gameKey key = case key of
  Char 'w' -> Just KW
  Char 'W' -> Just KW
  Char 'a' -> Just KA
  Char 'A' -> Just KA
  Char 's' -> Just KS
  Char 'S' -> Just KS
  Char 'd' -> Just KD
  Char 'D' -> Just KD
  Char 'p' -> Just KPause
  Char 'P' -> Just KPause
  Char 'r' -> Just KRestart
  Char 'R' -> Just KRestart
  Char 'z' -> Just KUndo
  Char 'Z' -> Just KUndo
  Char 'e' -> Just KEcho
  Char 'E' -> Just KEcho
  Char ' ' -> Just KSpace
  Char '\r' -> Just KEnter
  Char '\n' -> Just KEnter
  Char 'i' -> Just KInstructions
  Char 'I' -> Just KInstructions
  SpecialKey KeySpace -> Just KSpace
  -- In a level for two, the second keeper can shift from their side of the keyboard.
  SpecialKey KeyShiftR -> Just KSpace
  SpecialKey KeyPad0 -> Just KSpace
  SpecialKey KeyTab   -> Just KPeek
  SpecialKey KeyEsc   -> Just KLobby
  SpecialKey KeyDelete -> Just KCancel
  SpecialKey KeyBackspace -> Just KCancel
  SpecialKey KeyEnter -> Just KEnter
  -- GLFW names the keypad's keys apart from the main ones; GLUT gave the same keys.
  SpecialKey KeyPadEnter -> Just KEnter
  SpecialKey KeyPad8  -> Just KUp
  SpecialKey KeyPad2  -> Just KDown
  SpecialKey KeyPad4  -> Just KLeft
  SpecialKey KeyPad6  -> Just KRight
  SpecialKey KeyUp    -> Just KUp
  SpecialKey KeyDown  -> Just KDown
  SpecialKey KeyLeft  -> Just KLeft
  SpecialKey KeyRight -> Just KRight
  -- Everything else, M included (see 'onEvent'), means nothing to the game.
  _ -> Nothing

-- | The inspection script: each letter is one action, played to completion.
-- The letters are listed at the top of this module. A move or a shift is pressed
-- for one tick and then left idle until it completes ('actionInputs'); a capital
-- gives only the press, to look at the moment just after it, and T holds Tab for
-- one tick. e, p, r, z, ! and i are a press and then a tick with nothing held, so
-- the same key next in the script is a fresh press; . is one idle tick. Any other
-- character adds no ticks.
scriptInputs :: String -> [TickInput]
scriptInputs = concatMap one
  where
    one c = case c of
      'w' -> actionInputs (Go DirUp)
      's' -> actionInputs (Go DirDown)
      'a' -> actionInputs (Go DirLeft)
      'd' -> actionInputs (Go DirRight)
      -- The arrows, which move the second keeper in a level for two.
      'I' -> duoInputs (Go2 DirUp)
      'K' -> duoInputs (Go2 DirDown)
      'J' -> duoInputs (Go2 DirLeft)
      'L' -> duoInputs (Go2 DirRight)
      'x' -> actionInputs ShiftEra
      'q' -> actionInputs SendEcho
      'X' -> [noInput { tiShift = True }]  -- the press alone, to inspect mid-wave
      'W' -> [noInput { tiDir = Just DirUp, tiDir1 = Just DirUp }]  -- the press alone, to inspect mid-step
      'S' -> [noInput { tiDir = Just DirDown, tiDir1 = Just DirDown }]
      'A' -> [noInput { tiDir = Just DirLeft, tiDir1 = Just DirLeft }]
      'D' -> [noInput { tiDir = Just DirRight, tiDir1 = Just DirRight }]
      'T' -> [noInput { tiPeek = True }]  -- hold Tab for this tick, to inspect the peek
      'e' -> [noInput { tiEnter = True }, noInput]
      'p' -> [noInput { tiPause = True }, noInput]
      'r' -> [noInput { tiRestart = True }, noInput]
      'z' -> [noInput { tiUndo = True }, noInput]
      '!' -> [noInput { tiLobby = True }, noInput]  -- Escape: skips the film, leaves a level
      'c' -> [noInput { tiCancel = True }, noInput]  -- Delete: asks to cancel the chosen order
      'H' -> [noInput { tiEnter = True }]  -- Enter held for this tick, to tear an order
      'i' -> [noInput { tiInstructions = True }, noInput]  -- I: the instructions on the title
      '.' -> [noInput]
      _   -> []

-- | The solver plays one level from the title, but stops @n@ moves short of the
-- end and leaves the rest, and whatever follows, to the keyboard.
-- @pace@ is the idle ticks after each move, as for 'demoInputs'. Three Enter presses,
-- with an idle tick between them, cover the title, orders desk and hall. The film
-- lasts no ticks in the demo, and a second passes before the first move.
handOver :: Int -> Int -> Level -> [TickInput]
handOver pace n level =
  [press, noInput, press, noInput, press] ++ replicate 60 noInput
  ++ concatMap (\a -> actionInputs a ++ replicate pace noInput) (take (length route - n) route)
  -- The tick on which the last move completes.
  ++ [noInput]
  where
    press = noInput { tiEnter = True }
    -- The same route 'demoInputs' takes: pinned if the level has one, else searched.
    route = case (knownRoute level, srGoal (search level (\_ _ -> True) (startPlaying level))) of
      (Just actions, _) -> actions
      (_, Just (_, actions)) -> actions
      _ -> []

-- | A mode with the opening film's ticks forgotten, so the trace notes the film
-- starting and ending, not every tick of it.
filmless :: RunMode -> RunMode
filmless m = case m of
  Intro _ _ -> Intro 0 0
  _ -> m
