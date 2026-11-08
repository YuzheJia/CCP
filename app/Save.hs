-- | Tiny, versioned campaign save. Board state is not serialized: leaving a
-- puzzle starts it fresh next time, while cleared levels remain cleared.
--
-- What is kept: two numbers, how far through the levels the player has cleared and
-- which level the run was on ('Progress'). What is not: anything about a board
-- (where the keeper stands, the crates, the time left), the mute setting, or
-- whether the opening film has been seen (a save with nothing cleared plays it
-- again, once a session).
--
-- Where: one text file, progress-v1.save, in a folder borrowed-light under the
-- user-data directory that 'getXdgDirectory' gives. That is $XDG_DATA_HOME when it
-- is set to an absolute path, on any system (so a test run can point it at an empty
-- folder); otherwise ~/.local/share on macOS and Linux, and %APPDATA% (the user's
-- AppData\\Roaming) on Windows. The v1 in the file name is the format's version, so
-- a later format can take a new name rather than misread this one.
--
-- The file holds the value as the derived 'Show' writes it, for example
--
-- > Progress {savedCompleted = 2, savedSelected = 1}
--
-- and is read back with the derived 'Read'. A missing or unreadable file counts as
-- no progress, and the numbers read are clamped into range ('normalise'), so a
-- damaged or hand-edited save can neither crash the game nor unlock a level that
-- does not exist.
--
-- Used only by app/Main.hs: 'loadProgress' once at startup, before the run is
-- built; and after every frame's ticks, 'progressOf' the current run, which
-- 'saveProgress' writes only when it differs from what was last saved. It is IO,
-- not game logic, so it lives in the app and not in the pure library.
module Save
  ( Progress (..)
  , defaultProgress
  , loadProgress
  , progressOf
  , saveProgress
  ) where

-- 'try' runs an action and returns any exception it throws as a value, instead of
-- letting it end the program; 'IOException' is the kind thrown by file operations.
import Control.Exception (IOException, try)
import Game.Run
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>), takeDirectory)
-- 'readMaybe' parses text with 'Read', giving Nothing rather than an error when it
-- does not parse.
import Text.Read (readMaybe)

-- | What the save keeps.
--
-- Both fields are strict (@!@), so a value is never a chain of unevaluated updates.
-- @deriving (Eq, Read, Show)@ has the compiler write equality (Main compares with
-- the last value saved, to skip writing when nothing changed) and the text format
-- of the file, both ways.
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

-- | No progress: nothing cleared and the first level chosen, as for a new save.
-- With nothing cleared, Enter on the title plays the opening film.
defaultProgress :: Progress
defaultProgress = Progress 0 0

-- | The part of a run that is saved: how far it has cleared and the level it is on.
progressOf :: Run -> Progress
progressOf run = Progress (runCompleted run) (runIndex run)

-- | The saved progress for a game of @levelCount@ levels (all of them, the secret
-- one included), clamped to fit them. 'defaultProgress' if there is no save, it
-- cannot be read, or it does not parse. It never throws for a missing or unreadable
-- file.
loadProgress :: Int -> IO Progress
loadProgress levelCount = do
  -- The annotation says which exceptions 'try' catches: only IOException (the file
  -- missing, no permission, no home directory). A Left holds the exception, a Right
  -- the file's text.
  result <- try readSave :: IO (Either IOException String)
  -- @either f g@ applies f to a Left and g to a Right. @const x@ is the function that
  -- ignores its argument and gives x: any error means no progress.
  pure (normalise levelCount (either (const defaultProgress) parse result))
  where
    readSave = do
      path <- savePath
      body <- readFile path
      -- 'readFile' is lazy: it returns at once and reads the file as the text is
      -- used, keeping the file open until it reaches the end. @a `seq` b@ forces
      -- @a@ before giving @b@, so taking the length reads it all now. A read error
      -- then happens here, inside 'try', and not later in pure code; and the file
      -- is closed, so it can be written again (GHC refuses to open a file for
      -- writing while it is still open for reading).
      length body `seq` pure body
    -- Nothing from 'readMaybe' (text that is not exactly one Progress, spaces and
    -- the final newline aside) means no progress. @maybe d id@ is the same as
    -- @fromMaybe d@.
    parse body = maybe defaultProgress id (readMaybe body)

-- | Write the progress, and say whether it was written.
--
-- Any IOException (a read-only disk, no permission) gives False instead of ending
-- the game. Main then keeps its record of the last value saved, so it tries again
-- after the next frame, and every frame after, until a write succeeds or the
-- progress no longer differs.
saveProgress :: Progress -> IO Bool
saveProgress progress = do
  result <- try writeSave :: IO (Either IOException ())
  pure (either (const False) (const True) result)
  where
    writeSave = do
      path <- savePath
      -- The folder first, with any missing parents (True); the first save of all
      -- makes it.
      createDirectoryIfMissing True (takeDirectory path)
      -- Replaces the whole file: one line of 'show', and a newline.
      writeFile path (show progress ++ "\n")

-- | Where the save lives (see the module header). @(</> "progress-v1.save")@ is an
-- operator section: the function that adds the file name to a folder path; @<$>@
-- applies it to the folder that 'getXdgDirectory' finds. Only called inside 'try',
-- so if no folder can be found, that is caught like any other file error.
savePath :: IO FilePath
savePath = (</> "progress-v1.save") <$> getXdgDirectory XdgData "borrowed-light"

-- | Clamp what was read to a game of @levelCount@ levels, so a save from a longer
-- campaign, or one edited by hand, can only ever choose a level that exists and is
-- unlocked. 'Game.Run.startRunWithProgress' clamps again, to the levels the hall
-- lists.
normalise :: Int -> Progress -> Progress
normalise levelCount progress = Progress completed selected
  where
    -- Between none cleared and all of them.
    completed = clamp 0 levelCount (savedCompleted progress)
    -- The furthest level that may be chosen: the first not yet cleared, which is
    -- index @completed@, but not past the last level, and 0 if there are none.
    highest = min (max 0 (levelCount - 1)) completed
    selected = clamp 0 highest (savedSelected progress)

-- | @clamp lo hi x@ is @x@ held between @lo@ and @hi@. Written point-free as a
-- composition: first @min hi@ (no more than hi), then @max lo@ (no less than lo).
-- @Ord a =>@ lets it work for any type whose values can be compared. The same as
-- Game.Run's own clamp, which that module does not export.
clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
