-- | Tiny, versioned campaign save. Board state is not serialized: leaving a
-- puzzle starts it fresh next time, while cleared levels remain cleared.
module Save
  ( Progress (..)
  , defaultProgress
  , loadProgress
  , progressOf
  , saveProgress
  ) where

import Control.Exception (IOException, try)
import Game.Run
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>), takeDirectory)
import Text.Read (readMaybe)

data Progress = Progress
  { savedCompleted :: !Int
  , savedSelected  :: !Int
  } deriving (Eq, Read, Show)

defaultProgress :: Progress
defaultProgress = Progress 0 0

progressOf :: Run -> Progress
progressOf run = Progress (runCompleted run) (runIndex run)

loadProgress :: Int -> IO Progress
loadProgress levelCount = do
  result <- try readSave :: IO (Either IOException String)
  pure (normalise levelCount (either (const defaultProgress) parse result))
  where
    readSave = do
      path <- savePath
      body <- readFile path
      length body `seq` pure body
    parse body = maybe defaultProgress id (readMaybe body)

saveProgress :: Progress -> IO Bool
saveProgress progress = do
  result <- try writeSave :: IO (Either IOException ())
  pure (either (const False) (const True) result)
  where
    writeSave = do
      path <- savePath
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path (show progress ++ "\n")

savePath :: IO FilePath
savePath = (</> "progress-v1.save") <$> getXdgDirectory XdgData "borrowed-light"

normalise :: Int -> Progress -> Progress
normalise levelCount progress = Progress completed selected
  where
    completed = clamp 0 levelCount (savedCompleted progress)
    highest = min (max 0 (levelCount - 1)) completed
    selected = clamp 0 highest (savedSelected progress)

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
