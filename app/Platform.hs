-- | The one platform difference the game used to care about: closing the window.
--
-- With Gloss's GLUT backend, Apple's GLUT made a window whose close button did nothing,
-- so this module registered a close handler through GLUT and the Objective-C runtime.
-- The game now draws through Gloss's GLFW backend (see cabal.project), whose windows
-- close normally on every platform, and GLUT is no longer initialised on macOS, so
-- calling into it here would be wrong. Kept as a no-op so Main's call site stays put.
module Platform (exitWhenWindowCloses) where

-- | Call once, after the window exists.
exitWhenWindowCloses :: IO ()
exitWhenWindowCloses = pure ()
