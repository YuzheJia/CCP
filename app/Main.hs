-- | The IO boundary: window, clock and key events. Everything else is pure.
--
--   borrowed-light                 play
--   borrowed-light --demo          watch the verified route play itself, then take over
--
-- Development flags, used to inspect exact moments of the game:
--   --script=KEYS    run these keys instantly at startup, then hold the result:
--                    w a s d move, x shift, X shift press alone, e Enter, p pause,
--                    r restart, . one idle tick
--   --after=N        then run N more idle ticks
--   --fps            print measured frames per second to stdout
--   --trace          print the game state whenever phase, era, cell or inventory change
module Main (main) where

import Control.Monad (when)
import Data.IORef
import Data.List (foldl', stripPrefix)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Game.Fx
import Game.Input
import Game.Level
import Game.Logic
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

data World = World
  { wGame   :: !Game
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
  level <- either (die . ("Level is malformed: " ++)) pure theLevel
  let flag name = listToMaybe (mapMaybe (stripPrefix ("--" ++ name ++ "=")) args)
      prepared = fmap scriptInputs (flag "script")
      after = maybe 0 read (flag "after") :: Int
      inspect = isJust prepared
      (g0, fx0) = foldl' (tickBoth level) (initialGame level, initialFx)
                    (maybe [] id prepared ++ replicate after noInput)
      scene = Render.mkScene level
      world0 = World
        { wGame = g0, wFx = fx0, wKeys = noKeys, wWindow = windowSize, wBank = 0
        , wScript = if "--demo" `elem` args
                      then tiEnterTick : replicate 60 noInput ++ routeInputs intendedRoute
                      else []
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
        pure (Render.render scene level (wGame w) (wFx w) (wWindow w)))
    (\event (t, w) -> pure (t, onEvent event w))
    (\_ (t, w) -> do
        now <- getMonotonicTime
        let w' = onFrame level (now - t) w
        when ("--trace" `elem` args && stateKey (wGame w') /= stateKey (wGame w)) $ do
          putStrLn (show (stateKey (wGame w')) ++ " remaining=" ++ show (gRemaining (wGame w')))
          hFlush stdout
        pure (now, w'))
  where
    tiEnterTick = noInput { tiEnter = True }

tickBoth :: Level -> (Game, Fx) -> TickInput -> (Game, Fx)
tickBoth level (g, fx) input =
  let g' = stepGame level input g in (g', stepFx g g' fx)

-- | Fixed ticks from real time. A long stall is dropped rather than replayed,
-- so the game never fast-forwards through a countdown the player did not see.
onFrame :: Level -> Double -> World -> World
onFrame level elapsed w
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
    spend n s = spend (n - 1) (oneTick level s)

oneTick :: Level -> World -> World
oneTick level w =
  let (input, script) = case wScript w of
        (i : rest) -> (i, rest)
        []         -> (tickInput (wKeys w), [])
      (g, fx) = tickBoth level (wGame w, wFx w) input
  in w { wGame = g, wFx = fx, wScript = script, wKeys = afterTick (wKeys w) }

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
  Char ' ' -> Just KSpace
  Char '\r' -> Just KEnter
  Char '\n' -> Just KEnter
  SpecialKey KeySpace -> Just KSpace
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
      'e' -> [noInput { tiEnter = True }, noInput]
      'p' -> [noInput { tiPause = True }, noInput]
      'r' -> [noInput { tiRestart = True }, noInput]
      '.' -> [noInput]
      _   -> []
