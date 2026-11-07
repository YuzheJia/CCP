-- | The IO boundary: window, clock and key events. Everything else is pure.
--
--   borrowed-light                 play
--   borrowed-light --demo          watch the solver play every level, then take over
--   --pace=N                       with --demo, wait N extra ticks (60 a second) after each move
--   --leave=N                      with --demo, play only the first level, stop N moves short
--                                  of winning it, and hand over the keyboard
--   borrowed-light --level=N       select level N in the lobby, counting from 1; a secret
--                                  level, which the lobby never lists, is opened directly
--
-- Development flags, used to inspect exact moments of the game:
--   --script=KEYS    run these keys instantly at startup, then hold the result:
--                    w a s d move, x shift, q echo, W A S D X the press alone, z undo,
--                    T Tab held, e Enter, p pause, r restart, . one idle tick
--   --after=N        then run N more idle ticks
--   --fps            print measured frames per second to stdout
--   --trace          print the game state whenever phase, era, cell or inventory change
module Main (main) where

import Control.Monad (when)
import Data.IORef
import Data.List (foldl', stripPrefix)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Game.Fx
import Game.Input
import Game.Level
import Game.Run
import Game.Solution
import Game.Types
import GHC.Clock (getMonotonicTime)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game
import qualified Lobby
import Platform (exitWhenWindowCloses)
import qualified Render
import Save
import System.Environment (getArgs)
import System.Exit (die)
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
  args <- getArgs
  windowSize <- windowFor <$> getScreenSize
  levels <- either (die . ("Level is malformed: " ++)) pure levelHall
  let flag name = listToMaybe (mapMaybe (stripPrefix ("--" ++ name ++ "=")) args)
      prepared = fmap scriptInputs (flag "script")
      after = maybe 0 read (flag "after") :: Int
      inspect = isJust prepared
      total = length levels
      requested = flag "level" >>= readMaybe
  saved <- loadProgress total
  let startAt = max 0 (min (total - 1) (fromMaybe (savedSelected saved + 1) requested - 1))
      completedAtStart = max (savedCompleted saved) (if isJust requested then startAt else 0)
      -- The run as it opens: the saved place may be clamped back into the hall.
      start0 = (if isJust requested then startRunAt else startRunWithProgress) levels startAt completedAtStart
      (run0, fx0) = foldl' (tickBoth levels) (start0, initialFx)
                    (maybe [] id prepared ++ replicate after noInput)
      scenes =
        [ Render.mkScene (label i l) (i + 1 < shownTotal) l
        | (i, l) <- zip [0 ..] levels ]
      -- The hall counts only the levels it lists; a secret level gives its number alone.
      shownTotal = lastVisible levels + 1
      label i l
        | lvSecret l = show (i + 1) ++ "   " ++ lvName l
        | otherwise = show (i + 1) ++ " of " ++ show shownTotal ++ "   " ++ lvName l
      -- What an ending leads into, for the card at its end.
      beyond i
        | i + 1 < total = (show (i + 2), lvName (levels !! (i + 1)))
        | otherwise = ("", "")
      world0 = World
        { wRun = run0, wFx = fx0, wKeys = noKeys, wWindow = windowSize, wBank = 0
        , wScript = if "--demo" `elem` args
            then case flag "leave" >>= readMaybe of
              Just n -> handOver (maybe 0 read (flag "pace")) n (levels !! runIndex start0)
              Nothing -> demoInputs (maybe 0 read (flag "pace")) (drop (runIndex start0) levels)
            else []
        , wFrozen = inspect
        , wSaved = saved
        }
  start <- getMonotonicTime
  counter <- newIORef (start, 0 :: Int)
  firstFrame <- newIORef True
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
    (start, world0)
    (\(_, w) -> do
        first <- readIORef firstFrame
        when first (writeIORef firstFrame False >> exitWhenWindowCloses)
        countFrame
        let run = wRun w
            i = runIndex run
            picture = case runMode run of
              Title -> Render.render (scenes !! i) (levels !! i) (runGame run) (wFx w) (wWindow w)
              Lobby -> Lobby.renderLobby levels run (fromIntegral (fxClock (wFx w))) (wWindow w)
              InLevel -> Render.render (scenes !! i) (levels !! i) (runGame run) (wFx w) (wWindow w)
              Ending f -> Render.renderEnding (scenes !! i) (levels !! i) (beyond i) f (runGame run) (wFx w) (wWindow w)
        pure picture)
    (\event (t, w) -> pure (t, onEvent event w))
    (\_ (t, w) -> do
        now <- getMonotonicTime
        let w' = onFrame levels (now - t) w
        w'' <- persistWorld w'
        let game = runGame (wRun w'')
        when ("--trace" `elem` args && (stateKey game /= stateKey (runGame (wRun w)) || runIndex (wRun w'') /= runIndex (wRun w) || runMode (wRun w'') /= runMode (wRun w))) $ do
          putStrLn ("mode=" ++ show (runMode (wRun w'')) ++ " level=" ++ show (runIndex (wRun w'') + 1) ++ " " ++ show (stateKey game) ++ " remaining=" ++ show (gRemaining game))
          hFlush stdout
        pure (now, w''))

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
      ++ concatMap paced (route level) ++ [noInput]
      ++ (if not (null (lvPassage level)) && isJust next then ending level else [])
    -- Wait for the wall to open, step into the passage, and run it to the end,
    -- changing era at each barrier as a keeper reading the way ahead would.
    ending level = replicate (revealTicks level + 50) noInput ++ actionInputs (Go DirUp)
             ++ [noInput] ++ rushInputs level ++ replicate (arrivalTicks + 30) noInput
    route level = case (knownRoute level, srGoal (search level (\_ _ -> True) (startPlaying level))) of
      (Just actions, _) -> actions
      (_, Just (_, actions)) -> actions
      _ -> []

tickBoth :: [Level] -> (Run, Fx) -> TickInput -> (Run, Fx)
tickBoth levels (r, fx) input =
  let r' = stepRun levels input r in (r', stepFx (runGame r) (runGame r') fx)

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
      (r, fx) = tickBoth levels (wRun w, wFx w) input
  in w { wRun = r, wFx = fx, wScript = script, wKeys = afterTick (wKeys w) }

onEvent :: Event -> World -> World
onEvent (EventResize size) w = w { wWindow = size }
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
