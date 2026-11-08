-- | The level-select hall shown around the individual puzzle rooms.
--
-- The hall is drawn whenever the run's mode is 'Lobby' (see 'Main'): a heading, one
-- card for each level the hall lists, and the keys along the bottom. This module
-- only draws. Which level is selected, which are unlocked and which are cleared all
-- come from the 'Run'; moving the selection (W/S or the arrows), starting a level
-- (Enter) and going back to the title (Escape) are 'stepRun' in 'Game.Run'. Nothing
-- here reads the keyboard, the clock or the save file.
--
-- Fed by 'Main', which passes the level list, the run, the effects clock and the
-- window size, once a frame while the hall is shown. It uses 'Draw' for shapes and
-- colours and 'Font' for lettering, and nothing else of the app.
--
-- Coordinates. Everything is laid out on the game's 1280 x 800 canvas ('canvasW',
-- 'canvasH' in 'Draw'), with the origin at the centre of the screen, x growing to
-- the right and y growing upward, as Gloss has it. (Map cells count their rows
-- downward, but the hall has no cells.) 'renderLobby' then scales the canvas to the
-- window. Text sizes are the height of a capital letter, in canvas units ('Font');
-- colours are written as hex RGB, 0xRRGGBB, through 'hex' and 'hexA' in 'Draw'.
--
-- In Gloss, @Pictures [a, b, c]@ draws its list in order, so later pictures are
-- drawn over earlier ones; @Translate x y p@ moves @p@, and @Color c p@ paints every
-- shape of @p@ that has no colour of its own.
module Lobby (renderLobby) where

import Draw
import Font
import Game.Run
import Game.Types
import Graphics.Gloss

-- | The whole hall, for a window of @(winW, winH)@ screen pixels.
--
-- @levels@ is every level the run can reach, in order, the secret ones included
-- (they get no card); @run@ says which level is selected, which are unlocked and
-- how many are cleared; @clock@ is the effects clock ('fxClock' in 'Game.Fx') in
-- ticks, 60 a second, as a 'Float'. The clock only drives the selected card's pulse.
--
-- The scaling is the same as 'Render.fitted', written out again here: the canvas is
-- scaled by the largest factor at which all of it fits the window, so its 16:10
-- shape is kept, and black bars cover the strips of window on either side of it (or
-- above and below), hiding anything drawn past the canvas's edges.
-- @over@ adds the active order and save slip in canvas units before scaling.
renderLobby :: [Level] -> Run -> Float -> Picture -> (Int, Int) -> Picture
renderLobby levels run clock over (winW, winH) = Pictures
  [ Scale k k (Pictures [lobbyScreen levels run clock, over])
  , bars
  ]
  where
    -- The window's size in pixels, as Floats for the arithmetic below. Gloss puts the
    -- origin at the window's centre, so its edges are at x = +-wf/2 and y = +-hf/2.
    wf = fromIntegral winW :: Float
    hf = fromIntegral winH :: Float
    -- Pixels per canvas unit: the smaller of the two ratios, so neither the width nor
    -- the height of the canvas spills out of the window.
    k = min (wf / canvasW) (hf / canvasH)
    -- The width of each bar at the left and right (a window wider than 16:10) and
    -- the height of each bar at the top and bottom (a taller one), in pixels. Since
    -- 'k' is the smaller ratio, one of the two is always 0.
    barX = (wf - canvasW * k) / 2
    barY = (hf - canvasH * k) / 2
    -- The four bars. Each is a list comprehension with only a condition, so it gives
    -- a one-element list when the condition holds and an empty one when not: a bar
    -- under half a pixel would not show, so it is left out. 'rectC w h' is a w by h
    -- rectangle centred on the origin, moved so that its outer edge meets the
    -- window's edge: the left bar's centre is half a bar in from x = -wf/2.
    -- '$' is application with the lowest precedence: 'Pictures $ a ++ b' is
    -- 'Pictures (a ++ b)'.
    bars = Color black (Pictures $
      [ Translate (negate (wf / 2) + barX / 2) 0 (rectC barX hf) | barX > 0.5 ] ++
      [ Translate (wf / 2 - barX / 2) 0 (rectC barX hf) | barX > 0.5 ] ++
      [ Translate 0 (hf / 2 - barY / 2) (rectC wf barY) | barY > 0.5 ] ++
      [ Translate 0 (negate (hf / 2) + barY / 2) (rectC wf barY) | barY > 0.5 ])

-- | The hall on the canvas, before any scaling: the background, the heading at the
-- top, a card for each listed level from y = 145 down, and two lines of keys at the
-- bottom. The arguments are as for 'renderLobby'.
--
-- Three cards fit between the heading and the keys. A fourth listed level would need
-- another layout: its card would reach down to y = -342, over the keys at y = -326.
lobbyScreen :: [Level] -> Run -> Float -> Picture
lobbyScreen levels run clock = Pictures
  [ background
  -- The heading, centred: the game's name in cream, the hall's name in pale teal,
  -- and one line on how the hall works. Each 'Translate 0 y' sets the baseline.
  , Color (hex 0xFFF0CE) (Translate 0 306 (textC 42 "BORROWED LIGHT"))
  , Color (hex 0x8FB4BF) (Translate 0 267 (textC 15 "LEVEL HALL"))
  , Color (hex 0xC8D7DA) (Translate 0 235 (textC 12 "Choose a lesson. Cleared rooms stay unlocked."))
  -- A secret level is never listed: it is found, not chosen.
  -- 'zip [0 ..] levels' pairs each level with its index from 0, and the condition
  -- after the comma drops the secret ones. Secret levels come after every listed one
  -- ('lastVisible' in 'Game.Run'), so the listed levels keep their indices 0, 1, 2
  -- with no gaps, and those indices are the ones the run selects by.
  , Pictures [levelCard i level | (i, level) <- zip [0 ..] levels, not (lvSecret level)]
  -- The keys, in two quieter lines at the foot of the canvas.
  , Color (hex 0xDCE6E7) (Translate 0 (-326) (textC 13 "W S OR ARROWS   SELECT       ENTER   PLAY"))
  , Color (hex 0x89A4AA) (Translate 0 (-355) (textC 11 "ESC YOUR ORDERS       SAVED EVERY TIME A ROOM IS CLEARED"))
  ]
  where
    -- The index of the selected level, from 0. In the hall the run's level is the
    -- one the cursor is on.
    selected = runIndex run
    -- The highest index that may be chosen; every level after it is locked.
    unlocked = runUnlocked run
    -- How many levels are cleared: those with an index below this number.
    completed = runCompleted run
    -- The selected card's frame breathes between 40% and 100% of its opacity:
    -- 0.7 + 0.3 * sin, with the clock in ticks and 0.06 radians a tick, so one
    -- breath takes 2 * pi / 0.06, about 105 ticks or 1.75 seconds. Moving the
    -- selection gives the hall a new game with a new attempt number, which starts
    -- the effects clock again from 0 ('stepFx'), so each move starts the pulse from
    -- its middle, brightening.
    pulse = 0.7 + 0.3 * sin (clock * 0.06)
    -- Where card i stands, in canvas units: y = 145 for the first, then 145 lower
    -- for each one after (0, then -145). A card is 112 tall, leaving 33 between.
    cardY i = 145 - 145 * fromIntegral i
    -- One card: a frame, a darker panel inside it, the level's number, its name, the
    -- lesson it teaches, and its state at the right. All positions are relative to
    -- the card's own origin, set by 'cardY'. The panel spans y = -52 to 60, so it
    -- sits a little above that origin, around the two lines of text.
    levelCard i level = Translate 0 (cardY i) (Pictures
      -- The frame, 950 by 112 from its lower left corner: the pulsing gold on the
      -- selected card ('fade' scales a colour's opacity), slate grey on the others.
      [ Color (if i == selected then fade pulse (hex 0xFFD481) else hex 0x31444B)
          (rect (-475) (-52) 950 112)
      -- The panel, 6 units inside the frame on every side, so the frame shows as a
      -- border 6 units wide. A little lighter on the selected card.
      , Color (if i == selected then hex 0x162329 else hex 0x10191D)
          (rect (-469) (-46) 938 100)
      -- The number shown counts from 1, though the index counts from 0.
      , Color numberColor (Translate (-420) (-9) (textC 31 (show (i + 1))))
      -- The name above, the lesson ('lvLesson', a sentence on the mechanic the
      -- level introduces) below, both left-aligned at x = -350.
      , Color nameColor (Translate (-350) 16 (textL 22 (lvName level)))
      , Color noteColor (Translate (-350) (-19) (textL 12 (lvLesson level)))
      -- The state, right-aligned so that it ends at x = 418.
      , Color stateColor (Translate 418 7 (textR 12 stateText))
      ])
      where
        -- Beyond the unlock frontier: still listed, but dimmed. The selection never
        -- reaches it, since 'stepRun' stops the cursor at 'runUnlocked'.
        locked = i > unlocked
        -- The colours of the card's text. Guards are tried from the top, so a
        -- locked card is dim whatever else holds; the selected card's number takes
        -- the gold of its frame.
        numberColor | locked = hex 0x53666B
                    | i == selected = hex 0xFFD481
                    | otherwise = hex 0xA9C4C9
        nameColor | locked = hex 0x64767A
                  | otherwise = hex 0xEFF3EE
        noteColor | locked = hex 0x4C5E62
                  | otherwise = hex 0x98ADB1
        -- Cleared is asked first. A cleared level is never locked anyway: the run
        -- keeps 'runUnlocked' at the number cleared or at the last listed level's
        -- index, whichever is smaller, and a cleared listed level is below both.
        stateText | i < completed = "CLEARED"
                  | locked = "LOCKED"
                  | otherwise = "AVAILABLE"
        -- Green for cleared, grey for locked, amber for one waiting to be played.
        stateColor | i < completed = hex 0x8FD6A6
                   | locked = hex 0x5D6A6D
                   | otherwise = hex 0xE6BC70

-- | The hall's backdrop, covering the canvas exactly (x from -640 to 640, y from -400
-- to 400). A top-level value, so it is built once and shared by every frame, though
-- Gloss still draws its shapes each frame.
background :: Picture
background = Pictures
  -- A near-black blue over the whole canvas.
  [ Color (hex 0x071015) (rectC canvasW canvasH)
  -- The left half a little lighter than the right ('rect' takes the lower left
  -- corner, then the width and height).
  , Color (hex 0x111E24) (rect (-640) (-400) 640 800)
  , Color (hex 0x0B171C) (rect 0 (-400) 640 800)
  -- Two faint slanted washes of light, wider at the top than at the bottom: a warm
  -- one from the top left at 8% opacity and a cool one from the top right at 7%,
  -- echoing the warm past and the cold present of the rest of the game. Each
  -- polygon lists its corners in order around its edge.
  , Color (hexA 0xD38A52 0.08) (Polygon [(-640, 400), (-40, 400), (-260, -400), (-640, -400)])
  , Color (hexA 0x70A5B5 0.07) (Polygon [(40, 400), (640, 400), (640, -400), (260, -400)])
  -- Fifteen vertical lines one unit wide and the canvas's full height, 86 units
  -- apart from x = -590 to x = 614, in white at 5%: faint panelling.
  , Color (hexA 0xFFFFFF 0.05) (Pictures
      [ Translate (-590 + fromIntegral i * 86) 0 (rect 0 (-400) 1 800)
      | i <- [0 .. 14 :: Int] ])
  ]
