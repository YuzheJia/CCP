-- | The IO boundary: window, clock and key events. Everything else is pure.
--
--   borrowed-light                 play
--   borrowed-light --demo          watch the solver play every level, then take over
--   borrowed-light --level=N       start at level N, counting from 1
--
-- Development flags, used to inspect exact moments of the game:
--   --script=KEYS    run these keys instantly at startup, then hold the result:
--                    w a s d move, x shift, W A S D X the press alone, z undo,
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
import Platform (exitWhenWindowCloses)
import qualified Render
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
  levels <- either (die . ("Level is malformed: " ++)) pure campaign
  let flag name = listToMaybe (mapMaybe (stripPrefix ("--" ++ name ++ "=")) args)
      prepared = fmap scriptInputs (flag "script")
      after = maybe 0 read (flag "after") :: Int
      inspect = isJust prepared
      total = length levels
      startAt = max 0 (min (total - 1) (fromMaybe 1 (flag "level" >>= readMaybe) - 1))
      (run0, fx0) = foldl' (tickBoth levels) (startRun levels startAt, initialFx)
                    (maybe [] id prepared ++ replicate after noInput)
      scenes =
        [ Render.mkScene (show (i + 1) ++ " of " ++ show total ++ "   " ++ lvName l) (i + 1 < total) l
        | (i, l) <- zip [0 ..] levels ]
      world0 = World
        { wRun = run0, wFx = fx0, wKeys = noKeys, wWindow = windowSize, wBank = 0
        , wScript = if "--demo" `elem` args then demoInputs (drop startAt levels) else []
        , wFrozen = inspect
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
        let i = runIndex (wRun w)
        pure (Render.render (scenes !! i) (levels !! i) (runGame (wRun w)) (wFx w) (wWindow w)))
    (\event (t, w) -> pure (t, onEvent event w))
    (\_ (t, w) -> do
        now <- getMonotonicTime
        let w' = onFrame levels (now - t) w
            game = runGame (wRun w')
        when ("--trace" `elem` args && (stateKey game /= stateKey (runGame (wRun w)) || runIndex (wRun w') /= runIndex (wRun w))) $ do
          putStrLn ("level=" ++ show (runIndex (wRun w') + 1) ++ " " ++ show (stateKey game) ++ " remaining=" ++ show (gRemaining game))
          hFlush stdout
        pure (now, w'))

-- | The solver playing each level through the same keys a player would press.
demoInputs :: [Level] -> [TickInput]
demoInputs levels = concat (zipWith play (True : repeat False) levels)
  where
    press = noInput { tiEnter = True }
    pauseTicks = replicate 60 noInput
    play first level =
      (if first then [press] else replicate 150 noInput ++ [press])
      ++ pauseTicks
      ++ routeInputs (route level)
    route level = case srGoal (search level (\_ _ -> True) (startPlaying level)) of
      Just (_, actions) -> actions
      Nothing -> []

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
  Char ' ' -> Just KSpace
  Char '\r' -> Just KEnter
  Char '\n' -> Just KEnter
  SpecialKey KeySpace -> Just KSpace
  SpecialKey KeyTab   -> Just KPeek
  SpecialKey KeyEnter -> Just KEnter
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
      'X' -> [noInput { tiShift = True }]  -- the press alone, to inspect mid-wave
      'W' -> [noInput { tiDir = Just DirUp }]  -- the press alone, to inspect mid-step
      'S' -> [noInput { tiDir = Just DirDown }]
      'A' -> [noInput { tiDir = Just DirLeft }]
      'D' -> [noInput { tiDir = Just DirRight }]
      'T' -> [noInput { tiPeek = True }]  -- hold Tab for this tick, to inspect the peek
      'e' -> [noInput { tiEnter = True }, noInput]
      'p' -> [noInput { tiPause = True }, noInput]
      'r' -> [noInput { tiRestart = True }, noInput]
      'z' -> [noInput { tiUndo = True }, noInput]
      '.' -> [noInput]
      _   -> []
