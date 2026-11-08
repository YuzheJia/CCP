-- | Turns key events, which arrive at any moment, into one 'TickInput' per tick.
-- Pure, so a tap shorter than a tick can be tested without a window.
--
-- Where it sits. Gloss calls @onEvent@ in @app/Main.hs@ for every key that goes down
-- or up, whenever the operating system delivers it, between frames. Main translates
-- the Gloss key into a 'GameKey' (its @gameKey@; keys the game does not use are
-- dropped there, and M, the sound key, is handled by Main itself and never comes
-- here) and applies 'keyDown' or 'keyUp' to the 'Keys' value it keeps. The rules,
-- though, run in fixed ticks, 60 a second, and after a slow frame several ticks run
-- back to back with no events between them. So once each tick Main asks
-- 'tickInput' for that tick's 'TickInput', hands it on (to the courier's talk while
-- she speaks, otherwise to 'Game.Run.stepRun' and so to 'Game.Logic.stepGame'), and
-- then calls 'afterTick' to forget the taps that tick has now seen.
--
-- The problem this solves: sampling only the keys held at each tick would miss a
-- key pressed and let go between two ticks. So 'Keys' also remembers every key that
-- went down since the last tick ('kTapped'), even if it is up again, and
-- 'tickInput' counts such a key as down for that one tick.
--
-- What it deliberately does not do: decide which keys are fresh presses. That needs
-- the previous tick's keys, which the game keeps ('gPrev'), so the rules compare
-- the two themselves. Nor does it know anything of Gloss or IO: 'GameKey' is the
-- game's own name for a key, so the tests drive this module with plain values.
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

-- | The game's own names for the keys it uses. @app/Main.hs@ maps several physical
-- keys onto one name where they mean the same thing: upper and lower case letters,
-- Enter and keypad Enter ('KEnter'), Space, right Shift and keypad 0 ('KSpace'), and
-- keypad 8, 2, 4 and 6 (the arrows).
--
-- The W A S D keys and the arrows stay apart instead of being merged into four
-- directions, because in a level for two they belong to different keepers. 'Eq'
-- is derived for 'elem' and 'filter' below, which compare keys.
data GameKey
  = KW | KA | KS | KD  -- the W A S D keys: up, left, down, right (the yellow keeper)
  | KUp | KDown | KLeft | KRight  -- the arrow keys (the red keeper, in a level for two)
  | KSpace | KEnter | KRestart | KPause | KUndo | KPeek | KEcho
    -- Space shifts the era, Enter starts, retries and goes on, and R, P, Z, Tab and
    -- E restart, pause, undo, look at the other era and send the echo.
  | KInstructions| KLobby | KCancel
    -- I opens the instructions on the title; Escape goes back to the hall (from a
    -- level) or to orders (from the hall), and skips the opening film.
    -- Delete/Backspace is KCancel: ask to cancel the chosen order.

  deriving (Eq, Ord, Show)

-- | What the keyboard has done that the next tick needs to know. Both fields are
-- short lists, newest first. A list keeps the order in which keys went down, which
-- a 'Set' would lose and 'tickInput' needs (the newest direction wins); with a
-- dozen keys at most, 'elem' and 'filter' over them cost nothing.
data Keys = Keys
  { kHeld   :: ![GameKey]  -- ^ physically down, most recent first
                           --   (never twice: 'keyDown' ignores a key already here)
  , kTapped :: ![GameKey]  -- ^ went down since the last tick, even if already up
                           --   (most recent first; emptied by 'afterTick')
  } deriving (Eq, Show)

-- | Nothing held and nothing tapped: how @app/Main.hs@ starts.
noKeys :: Keys
noKeys = Keys [] []

-- | Operating-system auto-repeat is ignored: a key that is down cannot go down.
--
-- Holding a key down makes the system send a stream of extra key-down events. The
-- vendored Gloss already drops them, but any that still arrived would change
-- nothing here, so holding a key can never look like a string of presses. A real
-- press goes on the front of both lists: held from now on, and tapped for the
-- coming tick.
--
-- @k `elem` kHeld keys@ is the function 'elem' written between its arguments:
-- backquotes turn any two-argument function into an infix operator.
keyDown :: GameKey -> Keys -> Keys
keyDown k keys
  | k `elem` kHeld keys = keys
  | otherwise = Keys (k : kHeld keys) (k : kTapped keys)

-- | A key going up. It leaves the held list, but stays in 'kTapped' if it went down
-- since the last tick, so a tap shorter than a tick is still seen by that tick.
--
-- @keys { kHeld = ... }@ is record update syntax: a copy of @keys@ with that one
-- field replaced. @(/= k)@ is an operator section: @/=@ with its right-hand side
-- filled in, a function asking whether a key differs from @k@. So the 'filter'
-- keeps every held key but this one.
keyUp :: GameKey -> Keys -> Keys
keyUp k keys = keys { kHeld = filter (/= k) (kHeld keys) }

-- | The direction a key moves in, if it is one of the eight direction keys. W A S D
-- and the arrows agree, so a keeper alone can use either. 'Nothing' for any other
-- key, which is how 'tickInput' skips them.
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
  _      -> Nothing  -- every key that is not a direction

-- | The state of the keys for the coming tick. A tap counts as held for one tick.
--
-- Called once a tick by @app/Main.hs@, before 'afterTick'. It only reports what is
-- down; the rules find the fresh presses by comparing with the tick before.
tickInput :: Keys -> TickInput
tickInput keys = TickInput
  -- The newest direction key still held wins, since 'kHeld' is newest first; let it
  -- go and the one held before it wins again. Only if no direction is held does a
  -- tapped one count. So a direction tapped and let go within one tick, while
  -- another is held, does not show in 'tiDir' at all.
  { tiDir     = firstJust (map dirOf (kHeld keys ++ kTapped keys))
  , tiShift   = down KSpace
  , tiEnter   = down KEnter
  , tiRestart = down KRestart
  , tiPause   = down KPause
  , tiUndo    = down KUndo
  , tiPeek    = down KPeek
  , tiEcho    = down KEcho
  , tiInstructions = down KInstructions
  , tiLobby   = down KLobby
  , tiCancel  = down KCancel
  -- True when no direction key went down since the last tick. If 'tiDir' changes
  -- anyway, a newer key was let go of and an older one shows through, which
  -- 'Game.Logic' must not take as a press. The list comprehension keeps only the
  -- tapped keys that 'dirOf' turns into a 'Just', and 'null' asks if none did.
  , tiDirRevealed = null [d | Just d <- map dirOf (kTapped keys)]
  -- The same three things for each keeper's own keys, for a level for two:
  -- W A S D for the first keeper, the arrows for the second.
  , tiDir1    = firstJust (map dirOf (wasd (kHeld keys ++ kTapped keys)))
  , tiDir1Revealed = null (wasd (kTapped keys))
  , tiDir2    = firstJust (map dirOf (arrows (kHeld keys ++ kTapped keys)))
  , tiDir2Revealed = null (arrows (kTapped keys))
  }
  where
    -- A key counts as down for this tick if it is held now or was tapped since the
    -- last tick, even if it is already up again.
    down k = k `elem` kHeld keys || k `elem` kTapped keys
    -- The first 'Just' in a list, unwrapped. In a list comprehension a pattern on
    -- the left of @<-@ also filters: elements that do not match @Just d@ (the
    -- 'Nothing's) are skipped, not an error. Laziness means only as much of the
    -- list is built as it takes to find the first one.
    firstJust xs = case [d | Just d <- xs] of
      (d : _) -> Just d
      []      -> Nothing
    -- Only the W A S D keys, or only the arrows, in the same newest-first order.
    -- (`elem` [...]) is a section of the infix 'elem': the function \k -> k `elem` [...].
    wasd = filter (`elem` [KW, KA, KS, KD])
    arrows = filter (`elem` [KUp, KDown, KLeft, KRight])

-- | Call once the tick has seen the keys.
--
-- It empties 'kTapped', so a tap is seen by exactly one tick; held keys stay held
-- until their key-up event. @app/Main.hs@ calls it after every tick, including the
-- ticks whose input comes from a prepared script (the demo, or the script option)
-- instead of the keyboard.
afterTick :: Keys -> Keys
afterTick keys = keys { kTapped = [] }
