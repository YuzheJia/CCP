-- | The saves on disk: her orders ("Game.Orders"), all three in one small versioned
-- file. Board state is not saved: leaving a puzzle starts it fresh next time,
-- while cleared levels stay cleared.
--
-- The game kept one save before it had orders, in @progress-v1.save@. When there
-- is no orders file yet, that save becomes the first order, so nobody loses their
-- progress; the old file is left as it was.
module Save
  ( loadBook
  , saveBook
  , nowStamp
  ) where

import Control.Exception (IOException, try)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, utcToLocalTime)
import Game.Orders
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>), takeDirectory)
import Text.Read (readMaybe)

-- | The save from before the orders: rooms cleared and the room chosen.
data Progress = Progress
  { savedCompleted :: !Int
  , savedSelected  :: !Int
  } deriving (Eq, Read, Show)

-- | The orders, made to fit a campaign of @levelCount@ levels.
loadBook :: Int -> IO Book
loadBook levelCount = do
  orders <- readFrom ordersFile
  book <- case orders >>= readMaybe of
    Just b -> pure b
    Nothing -> do
      old <- readFrom progressFile
      pure $ case old >>= readMaybe of
        Just (Progress c s) -> bookFromProgress c s
        Nothing -> emptyBook
  pure (normaliseBook levelCount book)
  where
    readFrom name = do
      result <- try (do
        path <- inData name
        body <- readFile path
        length body `seq` pure body) :: IO (Either IOException String)
      pure (either (const Nothing) Just result)

-- | Write every order. False if the file could not be written.
saveBook :: Book -> IO Bool
saveBook book = do
  result <- try (do
    path <- inData ordersFile
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path (show book ++ "\n")) :: IO (Either IOException ())
  pure (either (const False) (const True) result)

ordersFile, progressFile :: FilePath
ordersFile = "orders-v2.save"
progressFile = "progress-v1.save"

inData :: FilePath -> IO FilePath
inData name = (</> name) <$> getXdgDirectory XdgData "borrowed-light"

-- | The local time now, as a ticket prints it.
nowStamp :: IO Stamp
nowStamp = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone
  let LocalTime day (TimeOfDay h m _) = utcToLocalTime zone utc
      (_, month, dayOfMonth) = toGregorian day
  pure (Stamp month dayOfMonth h m)
