-- | The IO boundary: window, clock and key events. Everything else is pure.
--
--   borrowed-light                 play
--   borrowed-light --demo          watch the solver play every level, then take over
--   --pace=N                       with --demo, wait N extra ticks (60 a second) after each move
--   --leave=N                      with --demo, play only the first level, stop N moves short
--                                  of winning it, and hand over the keyboard
--   borrowed-light --level=N       select level N in the lobby, counting from 1; a secret
--                                  level, which the lobby never lists, is opened directly
--   --mute                         start with the sound off; M turns it on and off
--   --intro                        open on the opening film, whatever the save, to watch it again
--
-- Development flags, used to inspect exact moments of the game:
--   --dump-sounds=DIR  write every sound to DIR as a WAV file, and exit
--   --script=KEYS    run these keys instantly at startup, then hold the result:
--                    w a s d move, x shift, q echo, W A S D X the press alone, z undo,
--                    T Tab held, e Enter, p pause, r restart, ! Escape, i instructions, . one idle tick
--   --after=N        then run N more idle ticks
--   --fps            print measured frames per second to stdout
--   --trace          print the game state whenever phase, era, cell or inventory change,
--                    and when each level's dead ends become known
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
import Game.Run
import Game.Solution
import Game.Sound
import Game.Story
import Game.Types
import GHC.Clock (getMonotonicTime)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game
import qualified Lobby
import Platform (exitWhenWindowCloses, keepLetterKeys)
import qualified Render
import Save
import TalkBox (introScreen, talkLayer)
import Video (frameAt, loadVideo, newPlayer, vdAudio, vdAudioRate, videoScale, videoTicks)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data World = World
  { wRun    :: !Run
  , wFx     :: !Fx
  , wKeys   :: !Keys
  , wWindow :: !(Int, Int)
  , wBank   :: !Double       -- ^ unspent real time, in seconds
  , wScript :: [TickInput]   -- ^ overrides the keyboard while it lasts
  , wFrozen :: !Bool         -- ^ inspection only: no ticks at all
  , wSaved  :: !Progress     -- ^ last progress successfully offered to the save file
  , wCues   :: [Cue]         -- ^ sounds the ticks since the last frame started, newest first
  , wMuted  :: !Bool
  , wStory  :: !Story        -- ^ the courier's talk
  , wTalks  :: !Bool         -- ^ off for the demo, whose keys would only be taken by it
  , wIntroLen :: !Int        -- ^ ticks the opening film runs; 0 with no film, or once
                             --   it has been shown, so it plays once a session
  , wDead   :: !(IntMap.IntMap DeadSearch)
    -- ^ each level's search for the states it can no longer be won from, by its
    --   index in the hall, advanced a slice per frame ('searchDeadEnds'). A level
    --   with no entry is never told it is lost (see 'warnsOfDeadEnds')
  }

-- | The 1280 x 800 the art is composed for, shrunk to fit a smaller display.
windowFor :: (Int, Int) -> (Int, Int)
windowFor (screenW, screenH)
  | screenW <= 0 || screenH <= 0 = (1280, 800)
  | otherwise = (round (1280 * k), round (800 * k))
  where
    margin = 90 :: Float  -- title bar and menu bar
    k = minimum [1, (fromIntegral screenW - 20) / 1280, (fromIntegral screenH - margin) / 800]

main :: IO ()
main = do
  -- Before anything makes a window: an input method would otherwise take the letter keys.
  keepLetterKeys
  args <- getArgs
  case listToMaybe (mapMaybe (stripPrefix "--dump-sounds=") args) of
    Just dir -> dumpSounds dir >> exitSuccess
    Nothing -> pure ()
  windowSize <- windowFor <$> getScreenSize
  levels <- either (die . ("Level is malformed: " ++)) pure levelHall
  let flag name = listToMaybe (mapMaybe (stripPrefix ("--" ++ name ++ "=")) args)
      prepared = fmap scriptInputs (flag "script")
      after = maybe 0 read (flag "after") :: Int
      inspect = isJust prepared
      total = length levels
      requested = flag "level" >>= readMaybe
  saved <- loadProgress total
  video <- loadVideo
  player <- traverse newPlayer video
  let startAt = max 0 (min (total - 1) (fromMaybe (savedSelected saved + 1) requested - 1))
      completedAtStart = max (savedCompleted saved) (if isJust requested then startAt else 0)
      -- The run as it opens: the saved place may be clamped back into the hall.
      start1 = (if isJust requested then startRunAt else startRunWithProgress) levels startAt completedAtStart
      watchFilm = "--intro" `elem` args && isJust video
      start0
        | watchFilm = start1 { runIndex = 0, runMode = Intro 0 0, runGame = initialGame (levels !! 0) }
        | otherwise = start1
      demo = "--demo" `elem` args
      scenes =
        [ Render.mkScene (place i l, lvName l) (i + 1 < shownTotal) (teaches i l) l
        | (i, l) <- zip [0 ..] levels ]
      -- The hall counts only the levels it lists; a secret level gives its number alone.
      shownTotal = lastVisible levels + 1
      place i l
        | lvSecret l = show (i + 1)
        | otherwise = show (i + 1) ++ " of " ++ show shownTotal
      -- The keys are written under the map for the first two levels only.
      teaches i l = i < 2 && not (lvSecret l)
      -- What an ending leads into, for the card at its end.
      beyond i
        | i + 1 < total = (show (i + 2), lvName (levels !! (i + 1)))
        | otherwise = ("", "")
      world0 = World
        { wRun = start0, wFx = initialFx, wKeys = noKeys, wWindow = windowSize, wBank = 0
        , wScript = if demo
            then case flag "leave" >>= readMaybe of
              Just n -> handOver (maybe 0 read (flag "pace")) n (levels !! runIndex start0)
              Nothing -> demoInputs (maybe 0 read (flag "pace")) (drop (runIndex start0) levels)
            else []
        , wFrozen = False
        , wSaved = saved
        , wCues = [ CueIntro | watchFilm ]
        , wMuted = "--mute" `elem` args
        , wStory = noStory
        , wTalks = not demo
        , wIntroLen = if demo then 0 else maybe 0 videoTicks video
        -- Made here, but nothing is worked out until 'searchDeadEnds' runs them.
        , wDead = IntMap.fromList [ (i, startDeadSearch l) | (i, l) <- zip [0 ..] levels, warnsOfDeadEnds l ]
        }
      -- An inspection script is played instantly through the same ticks as the
      -- keyboard, talk and film included, and then the world holds still.
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
  -- Every sound is made here, before the window opens.
  sound <- if inspect then pure False else startAudio
  when sound $ forM_ video (\v -> loadClip (vdAudioRate v) (vdAudio v))
  when ("--trace" `elem` args) $ do
    putStrLn (if sound then "sound on" else "sound off: no output device, or not Windows")
    putStrLn (maybe "no opening film: assets/intro.blv not found" (\v -> "opening film: " ++ show (videoTicks v) ++ " ticks") video)
    hFlush stdout
  start <- getMonotonicTime
  counter <- newIORef (start, 0 :: Int)
  firstFrame <- newIORef True
  -- Seconds spent on dead-end searches since the last frame was drawn.
  searchSpent <- newIORef (0 :: Double)
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
    (InWindow "Borrowed Light" windowSize (40, 40))
    black
    60
    (start, world2)
    (\(_, w) -> do
        first <- readIORef firstFrame
        when first (writeIORef firstFrame False >> exitWhenWindowCloses)
        countFrame
        writeIORef searchSpent 0
        let run = wRun w
            i = runIndex run
            g = runGame run
            -- Known only once this level's search has finished; until then the
            -- dead-end line simply does not show. Never a wait: looking a state up
            -- in a finished search is one hash-table probe.
            deadEnd key = maybe False (`knownDeadEnd` key) (IntMap.lookup i (wDead w))
            -- While she talks about something in the other era, the board shows
            -- that era, as if Tab were held.
            shown = case lookNow (wStory w) of
              Just lk | lkEra lk /= gEra g -> g { gPrev = (gPrev g) { tiPeek = True } }
              _ -> g
            talk f = talkLayer (wStory w) f (fxClock (wFx w))
        case runMode run of
          Title -> pure (Render.render deadEnd (scenes !! i) (levels !! i) g (wFx w) (wWindow w))
          Intro age held -> do
            frame <- traverse (`frameAt` age) player
            pure (Render.fitted (wWindow w) (introScreen (maybe 2 videoScale video) frame age held))
          Lobby -> pure (Lobby.renderLobby levels run (fromIntegral (fxClock (wFx w))) (wWindow w))
          InLevel -> pure (Render.renderWith talk deadEnd (scenes !! i) (levels !! i) shown (wFx w) (wWindow w))
          Ending f -> pure (Render.renderEnding (scenes !! i) (levels !! i) (beyond i) f g (wFx w) (wWindow w)))
    (\event (t, w) -> pure (t, onEvent event w))
    (\_ (t, w) -> do
        now <- getMonotonicTime
        let w' = onFrame levels (now - t) w
        when sound $ do
          setMuted (wMuted w')
          forM_ (reverse (wCues w')) $ \c -> do
            playCue c
            when ("--trace" `elem` args) (putStrLn ("sound " ++ show c) >> hFlush stdout)
          setBeds (beds levels (wRun w'))
        persisted <- persistWorld w' { wCues = [] }
        -- Gloss's loop draws a frame, holds it until at least a hundredth of a
        -- second has passed since the last one, runs this step (none, one or two
        -- times, to keep 60 ticks a second), then swaps buffers, which waits for
        -- the screen's refresh. The searches go last in the step, and share one
        -- budget a frame however many steps the frame has: the display callback
        -- above resets what has been spent. Time spent here is time the next
        -- frame's hold does not sleep, so a frame that draws quickly loses nothing.
        spent <- readIORef searchSpent
        (w'', used) <- searchDeadEnds (searchBudget (runMode (wRun persisted)) - spent) persisted
        writeIORef searchSpent (spent + used)
        when ("--trace" `elem` args) $
          forM_ (IntMap.toList (wDead w'')) $ \(n, s) ->
            when (deadSearchFinished s && maybe False (not . deadSearchFinished) (IntMap.lookup n (wDead w))) $ do
              putStrLn ("dead ends of level " ++ show (n + 1) ++ " known, " ++ show (now - start) ++ " s after opening")
              hFlush stdout
        let game = runGame (wRun w'')
        when ("--trace" `elem` args && (stateKey game /= stateKey (runGame (wRun w)) || runIndex (wRun w'') /= runIndex (wRun w) || filmless (runMode (wRun w'')) /= filmless (runMode (wRun w)))) $ do
          putStrLn ("mode=" ++ show (runMode (wRun w'')) ++ " level=" ++ show (runIndex (wRun w'') + 1) ++ " " ++ show (stateKey game) ++ " remaining=" ++ show (gRemaining game))
          hFlush stdout
        pure (now, w''))

-- | Whether the game looks for a level's dead ends at all, as it always has. The
-- search knows one keeper only, so a level for two is never told it is lost. A level
-- with three or more crates could reach too many states to search, unless it is
-- built of whole rooms: its one-way gates keep its states a sum over the rooms
-- rather than a product.
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
  InLevel -> 0.002
  Ending _ -> 0.002
  _ -> 0.006

-- | Units of search run between looks at the clock: about a tenth of a millisecond
-- of work, so a frame overshoots its budget by little.
searchSlice :: Int
searchSlice = 16

-- | Advance the dead-end searches for up to @budget@ seconds of wall-clock time, and
-- say how long it took. The level being played, or chosen on the title or in the
-- hall, goes first; then the levels after it, which the player is likely to reach
-- next; then the ones before. A frozen world runs nothing, so it stays the same.
searchDeadEnds :: Double -> World -> IO (World, Double)
searchDeadEnds budget w
  | wFrozen w || budget <= 0 = pure (w, 0)
  | otherwise = do
      begun <- getMonotonicTime
      let go searches = case nextSearch searches of
            Nothing -> pure searches
            Just (i, s) -> do
              -- Evaluating the advanced search is what runs it, here and not later
              -- in some frame that happens to look at it.
              s' <- evaluate (advanceDeadSearch searchSlice s)
              let searches' = IntMap.insert i s' searches
              now <- getMonotonicTime
              if now - begun < budget then go searches' else pure searches'
      searches <- go (wDead w)
      ended <- getMonotonicTime
      pure (w { wDead = searches }, ended - begun)
  where
    current = runIndex (wRun w)
    nextSearch searches =
      let (before, after) = IntMap.partitionWithKey (\i _ -> i < current) searches
          unfinished = IntMap.filter (not . deadSearchFinished)
      in IntMap.lookupMin (unfinished after) <|> IntMap.lookupMin (unfinished before)

-- | Finish the search of the level the world shows, all at once.
finishShownSearch :: World -> IO World
finishShownSearch w = do
  searches <- evaluate (IntMap.adjust finishDeadSearch (runIndex (wRun w)) (wDead w))
  pure w { wDead = searches }

persistWorld :: World -> IO World
persistWorld world
  | progress == wSaved world = pure world
  | otherwise = do
      saved <- saveProgress progress
      pure (if saved then world { wSaved = progress } else world)
  where progress = progressOf (wRun world)

-- | The solver playing each level through the same keys a player would press.
demoInputs :: Int -> [Level] -> [TickInput]
demoInputs pace levels = concat (zipWith3 play starts levels (map Just (drop 1 levels) ++ [Nothing]))
  where
    press = noInput { tiEnter = True }
    pauseTicks = replicate 60 noInput
    paced action = actionInputs action ++ replicate pace noInput
    -- The first level is started from the title; one after an ending starts itself;
    -- any other is started from the last one's won plate.
    starts = Just [press, noInput, press]
           : [ if null (lvPassage l) then Just (replicate 150 noInput ++ [press]) else Nothing | l <- levels ]
    play start level next =
      maybe [] id start
      ++ pauseTicks
      ++ moves level ++ [noInput]
      ++ (if not (null (lvPassage level)) && isJust next then ending level else [])
    -- Wait for the wall to open, step into the passage, and run it to the end,
    -- changing era at each barrier as a keeper reading the way ahead would.
    ending level = replicate (revealTicks level + 50) noInput ++ actionInputs (Go DirUp)
             ++ [noInput] ++ rushInputs level ++ replicate (arrivalTicks + 30) noInput
    -- A level for two is played by the search for two, both keepers' keys.
    moves level
      | isJust (lvSpawn2 level) =
          case drGoal (duoSearch level (const True) (startPlaying level)) of
            Just (_, actions) -> concatMap (\a -> duoInputs a ++ replicate pace noInput) actions
            Nothing -> []
      | otherwise = concatMap paced (route level)
    route level = case (knownRoute level, srGoal (search level (\_ _ -> True) (startPlaying level))) of
      (Just actions, _) -> actions
      (_, Just (_, actions)) -> actions
      _ -> []

tickBoth :: [Level] -> (Run, Fx) -> TickInput -> (Run, Fx)
tickBoth levels (r, fx) input =
  let r' = stepRun levels input r
      fx' = stepFx (runGame r) (runGame r') fx
      -- Coming into a level, from the hall or the level before, introduces it
      -- again; restarting the same level does not.
      entered = inLevel r' && (not (inLevel r) || runIndex r' /= runIndex r)
      inLevel x = case runMode x of
        InLevel -> True
        _ -> False
  in (r', if entered then fx' { fxFocus = 0 } else fx')

-- | Fixed ticks from real time. A long stall is dropped rather than replayed,
-- so the game never fast-forwards through input the player did not see land.
onFrame :: [Level] -> Double -> World -> World
onFrame levels elapsed w
  | wFrozen w = w
  | otherwise = spend (ticksDue :: Int) w { wBank = bank - fromIntegral ticksDue * dt }
  where
    dt = 1 / fromIntegral ticksPerSecond
    banked = wBank w + elapsed
    wanted = floor (banked / dt)
    ticksDue = min maxCatchUp wanted
    bank = if wanted > maxCatchUp then fromIntegral ticksDue * dt else banked
    maxCatchUp = 6
    spend 0 s = s
    spend n s = spend (n - 1) (oneTick levels s)

oneTick :: [Level] -> World -> World
oneTick levels w =
  let (input, script) = case wScript w of
        (i : rest) -> (i, rest)
        []         -> (tickInput (wKeys w), [])
      -- An open talk takes the keys, and the board waits for it.
      (story0, free)
        | wTalks w = talkTick input (wRun w) (wStory w)
        | otherwise = (wStory w, True)
      (r0, fx0)
        | free = tickBoth levels (wRun w, wFx w) input
        | otherwise = let held = holdRun input (wRun w)
                      in (held, stepFx (runGame (wRun w)) (runGame held) (wFx w))
      -- The film ends the run's opening once it has run its length, and is not
      -- shown again this session. With no film, the opening ends at once.
      (r, fx) = case runMode r0 of
        Intro age _ | age >= wIntroLen w -> (finishIntro levels r0, fx0 { fxFocus = 0 })
        _ -> (r0, fx0)
      filmOver = isIntro (runMode (wRun w)) && not (isIntro (runMode r))
      story
        | wTalks w = notice levels (wRun w) r story0
        | otherwise = story0
      heard = cues levels (wRun w, wFx w) (r, fx) ++ storyCues (wStory w) story
  in w { wRun = r, wFx = fx, wScript = script, wKeys = afterTick (wKeys w)
       , wCues = reverse heard ++ wCues w, wStory = story
       , wIntroLen = if filmOver then 0 else wIntroLen w }
  where
    isIntro m = case m of
      Intro _ _ -> True
      _ -> False

onEvent :: Event -> World -> World
onEvent (EventResize size) w = w { wWindow = size }
onEvent (EventKey (Char c) Down _ _) w | c `elem` "mM" = w { wMuted = not (wMuted w) }
onEvent (EventKey key Down _ _) w = withKey key (keyDown) w
onEvent (EventKey key Up _ _) w = withKey key (keyUp) w
onEvent _ w = w

withKey :: Key -> (GameKey -> Keys -> Keys) -> World -> World
withKey key f w = case gameKey key of
  Just k  -> w { wKeys = f k (wKeys w) }
  Nothing -> w

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
  _ -> Nothing

-- | The inspection script: each letter is one action, played to completion.
scriptInputs :: String -> [TickInput]
scriptInputs = concatMap one
  where
    one c = case c of
      'w' -> actionInputs (Go DirUp)
      's' -> actionInputs (Go DirDown)
      'a' -> actionInputs (Go DirLeft)
      'd' -> actionInputs (Go DirRight)
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
      'i' -> [noInput { tiInstructions = True }, noInput]  -- I: the instructions on the title
      '.' -> [noInput]
      _   -> []

-- | The solver plays one level from the title, but stops @n@ moves short of the
-- end and leaves the rest, and whatever follows, to the keyboard.
handOver :: Int -> Int -> Level -> [TickInput]
handOver pace n level =
  [press, noInput, press] ++ replicate 60 noInput
  ++ concatMap (\a -> actionInputs a ++ replicate pace noInput) (take (length route - n) route)
  ++ [noInput]
  where
    press = noInput { tiEnter = True }
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
