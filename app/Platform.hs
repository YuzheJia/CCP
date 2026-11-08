-- | The platform differences the game cares about: closing the window, and the input
-- methods that would take its letter keys.
--
-- With Gloss's GLUT backend, Apple's GLUT made a window whose close button did nothing,
-- so this module registered a close handler through GLUT and the Objective-C runtime.
-- The game now draws through Gloss's GLFW backend (see cabal.project), whose windows
-- close normally on every platform, and GLUT is no longer initialised on macOS, so
-- calling into it here would be wrong. Kept as a no-op so Main's call site stays put.
module Platform (exitWhenWindowCloses, keepLetterKeys) where

foreign import ccall unsafe "bl_disable_ime" c_disableIme :: IO ()

-- | Call once, after the window exists.
exitWhenWindowCloses :: IO ()
exitWhenWindowCloses = pure ()

-- | Turn input methods off for the game, so a Chinese, Japanese or Korean one in its
-- own mode cannot take W A S D, I and the other letter keys to compose text (see
-- cbits/ime.c). Call first thing, before any window is made. Elsewhere than Windows
-- it does nothing.
keepLetterKeys :: IO ()
keepLetterKeys = c_disableIme
