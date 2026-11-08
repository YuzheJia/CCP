-- | The level-select hall shown around the individual puzzle rooms.
module Lobby (renderLobby) where

import Draw
import Font
import Game.Run
import Game.Types
import Graphics.Gloss

renderLobby :: [Level] -> Run -> Float -> (Int, Int) -> Picture
renderLobby levels run clock (winW, winH) = Pictures
  [ Scale k k (lobbyScreen levels run clock)
  , bars
  ]
  where
    wf = fromIntegral winW :: Float
    hf = fromIntegral winH :: Float
    k = min (wf / canvasW) (hf / canvasH)
    barX = (wf - canvasW * k) / 2
    barY = (hf - canvasH * k) / 2
    bars = Color black (Pictures $
      [ Translate (negate (wf / 2) + barX / 2) 0 (rectC barX hf) | barX > 0.5 ] ++
      [ Translate (wf / 2 - barX / 2) 0 (rectC barX hf) | barX > 0.5 ] ++
      [ Translate 0 (hf / 2 - barY / 2) (rectC wf barY) | barY > 0.5 ] ++
      [ Translate 0 (negate (hf / 2) + barY / 2) (rectC wf barY) | barY > 0.5 ])

lobbyScreen :: [Level] -> Run -> Float -> Picture
lobbyScreen levels run clock = Pictures
  [ background
  , Color (hex 0xFFF0CE) (Translate 0 306 (textC 42 "BORROWED LIGHT"))
  , Color (hex 0x8FB4BF) (Translate 0 267 (textC 15 "LEVEL HALL"))
  , Color (hex 0xC8D7DA) (Translate 0 235 (textC 12 "Choose a lesson. Cleared rooms stay unlocked."))
  -- A secret level is never listed: it is found, not chosen.
  , Pictures [levelCard i level | (i, level) <- zip [0 ..] levels, not (lvSecret level)]
  , Color (hex 0xDCE6E7) (Translate 0 (-326) (textC 13 "W S OR ARROWS   SELECT       ENTER   PLAY"))
  , Color (hex 0x89A4AA) (Translate 0 (-355) (textC 11 "ESC TITLE SCREEN       PROGRESS SAVES AUTOMATICALLY"))
  ]
  where
    selected = runIndex run
    unlocked = runUnlocked run
    completed = runCompleted run
    pulse = 0.7 + 0.3 * sin (clock * 0.06)
    cardY i = 145 - 145 * fromIntegral i
    levelCard i level = Translate 0 (cardY i) (Pictures
      [ Color (if i == selected then fade pulse (hex 0xFFD481) else hex 0x31444B)
          (rect (-475) (-52) 950 112)
      , Color (if i == selected then hex 0x162329 else hex 0x10191D)
          (rect (-469) (-46) 938 100)
      , Color numberColor (Translate (-420) (-9) (textC 31 (show (i + 1))))
      , Color nameColor (Translate (-350) 16 (textL 22 (lvName level)))
      , Color noteColor (Translate (-350) (-19) (textL 12 (lvLesson level)))
      , Color stateColor (Translate 418 7 (textR 12 stateText))
      ])
      where
        locked = i > unlocked
        numberColor | locked = hex 0x53666B
                    | i == selected = hex 0xFFD481
                    | otherwise = hex 0xA9C4C9
        nameColor | locked = hex 0x64767A
                  | otherwise = hex 0xEFF3EE
        noteColor | locked = hex 0x4C5E62
                  | otherwise = hex 0x98ADB1
        stateText | i < completed = "CLEARED"
                  | locked = "LOCKED"
                  | otherwise = "AVAILABLE"
        stateColor | i < completed = hex 0x8FD6A6
                   | locked = hex 0x5D6A6D
                   | otherwise = hex 0xE6BC70

background :: Picture
background = Pictures
  [ Color (hex 0x071015) (rectC canvasW canvasH)
  , Color (hex 0x111E24) (rect (-640) (-400) 640 800)
  , Color (hex 0x0B171C) (rect 0 (-400) 640 800)
  , Color (hexA 0xD38A52 0.08) (Polygon [(-640, 400), (-40, 400), (-260, -400), (-640, -400)])
  , Color (hexA 0x70A5B5 0.07) (Polygon [(40, 400), (640, 400), (640, -400), (260, -400)])
  , Color (hexA 0xFFFFFF 0.05) (Pictures
      [ Translate (-590 + fromIntegral i * 86) 0 (rect 0 (-400) 1 800)
      | i <- [0 .. 14 :: Int] ])
  ]
