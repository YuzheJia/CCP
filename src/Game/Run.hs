-- | Campaign navigation around the pure rules for one level.
module Game.Run
  ( RunMode (..)
  , Run (..)
  , startRun
  , startRunWithProgress
  , stepRun
  ) where

import Game.Logic
import Game.Types

data RunMode = Title | Lobby | InLevel
  deriving (Eq, Ord, Show)

-- | @runCompleted@ is the number of campaign levels cleared. @runUnlocked@ is
-- the highest level index the player may choose. The IO boundary persists the
-- completion count and selection; puzzle board state stays session-local.
data Run = Run
  { runIndex    :: !Int
  , runUnlocked :: !Int
  , runCompleted :: !Int
  , runMode     :: !RunMode
  , runGame     :: !Game
  } deriving (Eq, Show)

-- | A new campaign starts at the title with only its first level unlocked.
startRun :: [Level] -> Int -> Run
startRun levels selected = startRunWithProgress levels selected 0

-- | Restore a validated lobby selection and unlock frontier.
startRunWithProgress :: [Level] -> Int -> Int -> Run
startRunWithProgress levels selected completed =
  Run i u c Title (initialGame (levels !! i))
  where
    lastIndex = max 0 (length levels - 1)
    c = clamp 0 (length levels) completed
    u = min lastIndex c
    i = clamp 0 u selected

-- | One fixed tick. Enter moves from the title into the lobby, where Escape goes
-- back to the title. Escape from a level returns to the lobby. Winning unlocks the
-- next room immediately, and Enter continues there.
stepRun :: [Level] -> TickInput -> Run -> Run
stepRun levels input run = case runMode run of
  Title   -> stepTitle
  Lobby   -> stepLobby
  InLevel -> stepLevel
  where
    i = runIndex run
    u = runUnlocked run
    c = runCompleted run
    g = runGame run
    prev = gPrev g
    freshEnter = tiEnter input && not (tiEnter prev)
    freshLobby = tiLobby input && not (tiLobby prev)
    freshDir
      | tiDir input /= tiDir prev && not (tiDirRevealed input) = tiDir input
      | otherwise = Nothing
    selected = case freshDir of
      Just DirUp    -> max 0 (i - 1)
      Just DirLeft  -> max 0 (i - 1)
      Just DirDown  -> min u (i + 1)
      Just DirRight -> min u (i + 1)
      Nothing       -> i
    selectedGame = (initialGame (levels !! selected))
      { gAttempt = gAttempt g + if selected /= i then 1 else 0
      , gPrev = input
      }
    begin level = (initialGame level)
      { gPhase = Playing, gAttempt = gAttempt g + 1, gPrev = input }
    toLobby = Run i u c Lobby ((initialGame (levels !! i))
      { gAttempt = gAttempt g + 1, gPrev = input })
    toTitle = Run i u c Title ((initialGame (levels !! i))
      { gAttempt = gAttempt g + 1, gPrev = input })

    stepTitle
      | freshEnter = toLobby
      | otherwise  = run { runGame = g { gPrev = input } }

    stepLobby
      | freshLobby = toTitle
      | freshEnter = Run i u c InLevel (begin (levels !! i))
      | otherwise  = Run selected u c Lobby selectedGame

    stepLevel
      | freshLobby = toLobby
      | gPhase g == Won && freshEnter =
          if i + 1 < length levels
            then let next = i + 1
                 in Run next (max u next) (max c (i + 1)) InLevel (begin (levels !! next))
            else toLobby
      | otherwise =
          let g' = stepGame (levels !! i) input g
              completed' = if gPhase g' == Won then max c (i + 1) else c
              unlocked' = min (length levels - 1) completed'
          in Run i unlocked' completed' InLevel g'

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
