-- | The saves on disk: her orders ("Game.Orders"), all three in one small versioned
-- file. Board state is not saved: leaving a puzzle starts it fresh next time,
-- while cleared levels stay cleared.
--
-- The game kept one save before it had orders, in @progress-v1.save@. When there
-- is no orders file yet, that save becomes the first order, so nobody loses their
-- progress; the old file is left as it was.
--
-- Main loads the book once and writes changes through saveBook. Game.Orders
-- defines and normalises the pure records; this module handles files and local
-- timestamps only. Neither board positions nor animation state are persisted.
-- Files live in borrowed-light under getXdgDirectory XdgData: an absolute
-- XDG_DATA_HOME override, or AppData/Roaming on Windows and ~/.local/share on
-- Unix. A missing/unreadable/unparseable v2 file falls back to the v1 file, then
-- to an empty book. Each file uses derived Show/Read text with a final newline.
module Save
  ( loadBook
  , saveBook
  , nowStamp
  ) where

-- 'try' runs an action and returns any exception it throws as a value, instead of
-- letting it end the program; 'IOException' is the kind thrown by file operations.
import Control.Exception (IOException, try)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getCurrentTimeZone, utcToLocalTime)
import Game.Orders
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>), takeDirectory)
-- 'readMaybe' parses text with 'Read', giving Nothing rather than an error when it
-- does not parse.
import Text.Read (readMaybe)

-- | The save from before the orders: rooms cleared and the room chosen.
-- Both fields are strict Ints; derived Read retains the old record syntax for
-- migration. These values are normalised by Game.Orders before use.
data Progress = Progress
  { savedCompleted :: !Int  -- ^ how far the player has cleared: one past the
                            --   furthest level won, counting from 0 ('runCompleted').
                            --   Levels unlock in order, so this is also the number
                            --   cleared, and level index @savedCompleted@ is the
                            --   first not yet cleared, which may be played
  , savedSelected  :: !Int  -- ^ the level the run was on, from 0 ('runIndex'): the
                            --   hall's choice, or the level being played. It may be
                            --   the secret level; starting a run clamps it back into
                            --   the hall
  } deriving (Eq, Read, Show)

-- | The orders, made to fit a campaign of @levelCount@ levels.
loadBook :: Int -> IO Book
loadBook levelCount = do
  -- Maybe bind feeds readable text to readMaybe; either failure takes the
  -- legacy path. Only a valid v2 Book takes precedence over the old save.
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
    -- Force the lazy file before leaving try: read errors become Nothing here,
    -- and the handle closes before a later write to the same file.
    readFrom name = do
      result <- try (do
        path <- inData name
        body <- readFile path
        length body `seq` pure body) :: IO (Either IOException String)
      pure (either (const Nothing) Just result)

-- | Write all three orders and the active selection, creating the data directory
-- if needed. IOException returns False; Main retains its last saved snapshot
-- and retries on later frames. This replaces the file rather than appending.
saveBook :: Book -> IO Bool
saveBook book = do
  result <- try (do
    path <- inData ordersFile
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path (show book ++ "\n")) :: IO (Either IOException ())
  pure (either (const False) (const True) result)

-- | Current book and legacy single-progress filenames, read in that order.
ordersFile, progressFile :: FilePath
ordersFile = "orders-v2.save"
progressFile = "progress-v1.save"

-- | Resolve one filename under the game's user-data directory. The partially
-- applied path join is mapped over IO with <$>; callers catch directory errors.
inData :: FilePath -> IO FilePath
inData name = (</> name) <$> getXdgDirectory XdgData "borrowed-light"

-- | Local month, day, hour and minute for the active order's printed timestamp.
-- The operating system supplies the current time zone; year and seconds are
-- deliberately discarded because the ticket's Stamp has no fields for them.
nowStamp :: IO Stamp
nowStamp = do
  utc <- getCurrentTime
  zone <- getCurrentTimeZone
  let LocalTime day (TimeOfDay h m _) = utcToLocalTime zone utc
      (_, month, dayOfMonth) = toGregorian day
  pure (Stamp month dayOfMonth h m)
