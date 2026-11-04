{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | The one platform difference the game has to care about: closing the window.
--
-- Gloss installs no window-close handler. freeglut is documented to end the program
-- when its window closes, so elsewhere nothing is needed. Apple's GLUT is different:
-- it creates its window without a working close button and has no glutCloseFunc. So
-- on macOS we register its own glutWMCloseFunc, and mark the window closable through
-- the Objective-C runtime.
module Platform (exitWhenWindowCloses) where

#ifdef darwin_HOST_OS
import Control.Monad (when)
import Data.Bits ((.|.))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CULong (..))
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import System.Exit (exitSuccess)

type Id = Ptr ()
type Sel = Ptr ()

foreign import ccall "wrapper" wrapCallback :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "glutWMCloseFunc" glutWMCloseFunc :: FunPtr (IO ()) -> IO ()
foreign import ccall "objc_getClass" objcClass :: CString -> IO Id
foreign import ccall "sel_registerName" selector :: CString -> IO Sel
-- objc_msgSend must be called at the exact type of the method it reaches.
foreign import ccall "objc_msgSend" sendId :: Id -> Sel -> IO Id
foreign import ccall "objc_msgSend" getMask :: Id -> Sel -> IO CULong
foreign import ccall "objc_msgSend" setMask :: Id -> Sel -> CULong -> IO ()

sel :: String -> IO Sel
sel name = withCString name selector

-- | Call once, after the window exists.
exitWhenWindowCloses :: IO ()
exitWhenWindowCloses = do
  wrapCallback exitSuccess >>= glutWMCloseFunc
  nsApp <- withCString "NSApplication" objcClass
  app <- sel "sharedApplication" >>= sendId nsApp
  windows <- sel "windows" >>= sendId app
  window <- sel "firstObject" >>= sendId windows
  when (window /= nullPtr) $ do
    mask <- sel "styleMask" >>= getMask window
    setter <- sel "setStyleMask:"
    setMask window setter (mask .|. closable)
  where
    closable = 2 -- NSWindowStyleMaskClosable
#else
exitWhenWindowCloses :: IO ()
exitWhenWindowCloses = pure ()
#endif
