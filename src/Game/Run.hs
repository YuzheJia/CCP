-- | The whole game: which level is being played, and moving between them. It wraps
-- 'stepGame' rather than changing it, so a level's rules know nothing of the others.
module Game.Run
  ( Run (..)
  , startRun
  , stepRun
  ) where

import Game.Logic
import Game.Types

data Run = Run
  { runIndex :: !Int
  , runGame  :: !Game
  } deriving (Eq, Show)

-- | The title screen, with this level ready behind it.
startRun :: [Level] -> Int -> Run
startRun levels i = Run i (initialGame (levels !! i))

-- | One tick. Enter on a won level moves on: to the next level, or from the last
-- back to the title. Anything else belongs to the level. The new game remembers this
-- tick's keys, so the Enter that moved on cannot also act there, and its new attempt
-- number tells the effects to start clean.
stepRun :: [Level] -> TickInput -> Run -> Run
stepRun levels input (Run i g)
  | gPhase g == Won && tiEnter input && not (tiEnter (gPrev g)) =
      if i + 1 < length levels
        then Run (i + 1) (fresh (levels !! (i + 1))) { gPhase = Playing }
        else Run 0 (fresh (levels !! 0))
  | otherwise = Run i (stepGame (levels !! i) input g)
  where
    fresh level = (initialGame level) { gAttempt = gAttempt g + 1, gPrev = input }
