-- | The platform differences the game cares about: closing the window, and the input
-- methods that would take its letter keys.
--
-- With Gloss's GLUT backend, Apple's GLUT made a window whose close button did nothing,
-- so this module registered a close handler through GLUT and the Objective-C runtime.
-- The game now draws through Gloss's GLFW backend (see cabal.project), whose windows
-- close normally on every platform, and GLUT is no longer initialised on macOS, so
-- calling into it here would be wrong. Kept as a no-op so Main's call site stays put.
--
-- The input methods: a Chinese, Japanese or Korean input method in its own mode
-- takes letter keys to compose text before the game sees them, so W A S D looked
-- dead. 'keepLetterKeys' calls bl_disable_ime in cbits/ime.c, which on Windows turns
-- input methods off for the whole process (borrowed-light.cabal links imm32 for it)
-- and elsewhere is an empty function. The game has no text to type, so nothing is
-- lost.
--
-- Used only by app/Main.hs: 'keepLetterKeys' first thing in @main@, before any
-- window exists, and 'exitWhenWindowCloses' from the drawing callback on the first
-- frame, once the window is certainly there. Neither returns a result.
module Platform (exitWhenWindowCloses, keepLetterKeys) where

-- The C function in cbits/ime.c, given a Haskell type so it can be called like any
-- IO action. @unsafe@: a plain, cheap C call, right for a short function that never
-- calls back into Haskell.
foreign import ccall unsafe "bl_disable_ime" c_disableIme :: IO ()

-- | Call once, after the window exists.
-- It does nothing now (see the module header): GLFW's windows close normally.
-- @pure ()@ is the IO action that does nothing and gives back @()@, the empty value.
exitWhenWindowCloses :: IO ()
exitWhenWindowCloses = pure ()

-- | Turn input methods off for the game, so a Chinese, Japanese or Korean one in its
-- own mode cannot take W A S D, I and the other letter keys to compose text (see
-- cbits/ime.c). Call first thing, before any window is made. Elsewhere than Windows
-- it does nothing.
--
-- "Any window" includes the hidden helper window the backend makes for itself
-- (cbits/ime.c), and Main calls this as the very first thing in @main@, before even
-- reading its arguments. The C side passes -1 to ImmDisableIME, which means every
-- thread of this process. Defined as the C call itself, with no argument.
keepLetterKeys :: IO ()
keepLetterKeys = c_disableIme
