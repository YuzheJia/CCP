-- | Turns key events, which arrive at any moment, into one 'TickInput' per tick.
-- Pure, so a tap shorter than a tick can be tested without a window.
module Game.Input
  ( GameKey (..)
  , Keys
  , noKeys
  , keyDown
  , keyUp
  , tickInput
  , afterTick
  ) where

import Game.Types

data GameKey
  = KW | KA | KS | KD
  | KUp | KDown | KLeft | KRight
  | KSpace | KEnter | KRestart | KPause
  deriving (Eq, Ord, Show)

data Keys = Keys
  { kHeld   :: ![GameKey]  -- ^ physically down, most recent first
  , kTapped :: ![GameKey]  -- ^ went down since the last tick, even if already up
  } deriving (Eq, Show)

noKeys :: Keys
noKeys = Keys [] []

-- | Operating-system auto-repeat is ignored: a key that is down cannot go down.
keyDown :: GameKey -> Keys -> Keys
keyDown k keys
  | k `elem` kHeld keys = keys
  | otherwise = Keys (k : kHeld keys) (k : kTapped keys)

keyUp :: GameKey -> Keys -> Keys
keyUp k keys = keys { kHeld = filter (/= k) (kHeld keys) }

dirOf :: GameKey -> Maybe Dir
dirOf k = case k of
  KW     -> Just DirUp
  KUp    -> Just DirUp
  KS     -> Just DirDown
  KDown  -> Just DirDown
  KA     -> Just DirLeft
  KLeft  -> Just DirLeft
  KD     -> Just DirRight
  KRight -> Just DirRight
  _      -> Nothing

-- | The state of the keys for the coming tick. A tap counts as held for one tick.
tickInput :: Keys -> TickInput
tickInput keys = TickInput
  { tiDir     = firstJust (map dirOf (kHeld keys ++ kTapped keys))
  , tiShift   = down KSpace
  , tiEnter   = down KEnter
  , tiRestart = down KRestart
  , tiPause   = down KPause
  }
  where
    down k = k `elem` kHeld keys || k `elem` kTapped keys
    firstJust xs = case [d | Just d <- xs] of
      (d : _) -> Just d
      []      -> Nothing

-- | Call once the tick has seen the keys.
afterTick :: Keys -> Keys
afterTick keys = keys { kTapped = [] }
