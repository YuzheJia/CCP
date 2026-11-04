-- | The instrument panel beside the map, and the plates that appear over it.
module Hud
  ( HudArt
  , mkHudArt
  , hud
  , mapCaption
  , wonCaption
  , hintBand
  , blockedPlate
  , pausedScreen
  , wonScreen
  , lostScreen
  , clockText
  ) where

import Draw
import Figures (cellIcon)
import Font
import Game.Logic
import Game.Types
import Graphics.Gloss

-- | Lettering that never changes, drawn once.
data HudArt = HudArt
  { artPast    :: Picture
  , artPresent :: Picture
  }

panelTop, panelBottom, panelLeft :: Float
panelTop = 388
panelBottom = -388
panelLeft = panelX - panelW / 2

data Skin = Skin { skInk, skDim, skRule, skAccent :: Color }

skin :: Era -> Skin
skin Past    = Skin (hex 0xF3E2BE) (hex 0xB89E76) (hex 0x8A6A3F) (hex 0xE8B85C)
skin Present = Skin (hex 0xD6E0E2) (hex 0x8A9AA0) (hex 0x4F5B60) (hex 0x9CC7D8)

mkHudArt :: HudArt
mkHudArt = HudArt (bake (staticPanel Past)) (bake (staticPanel Present))

-- | Backing plate, era plaque, section labels and controls.
staticPanel :: Era -> Picture
staticPanel era = Pictures
  [ Color (hexA 0x000000 0.5) (rect (panelLeft - 5) panelBottom (panelW + 10) (panelTop - panelBottom))
  , Color back (rect panelLeft panelBottom panelW (panelTop - panelBottom))
  , Color (skRule sk) (Pictures
      [ rect panelLeft panelBottom 2 (panelTop - panelBottom)
      , rect (panelLeft + panelW - 2) panelBottom 2 (panelTop - panelBottom)
      , rect panelLeft (panelTop - 2) panelW 2
      , rect panelLeft panelBottom panelW 2
      ])
  , plaque era
  , Color (skDim sk) (Pictures
      [ Translate (panelX - 92) 184 (textL 11 "Time left")
      , Translate (panelX - 92) 74 (textL 11 "Power cell")
      , Translate (panelX - 92) (-36) (textL 11 "Objective")
      , Translate (panelX - 92) (-182) (textL 11 "Controls")
      ])
  , Color (skRule sk) (Pictures [ rect (panelX - 92) y 184 1 | y <- [200, 90, -20, -166] ])
  , controls sk
  ]
  where
    sk = skin era
    back = case era of
      Past -> hex 0x2C1C12
      Present -> hex 0x10171B

-- | Era named in material as well as words: polished brass, or corroded steel.
plaque :: Era -> Picture
plaque era = Translate panelX 298 $ case era of
  Past -> Pictures
    [ Color (hex 0x17100B) (rectC 194 118)
    , Color (hex 0xC2964F) (rectC 188 112)
    , Color (hex 0xE9CB88) (rect (-94) 53 188 3)
    , Color (hex 0x8A6A3F) (rect (-94) (-56) 188 3)
    , rivets (hex 0x7A5A30)
    , Color (hex 0x2A1A0E) (Translate 0 (-10) (centred 34 4.4 "PAST"))
    , Color (hex 0x4A3218) (Translate 0 (-36) (textC 11 "The station in service"))
    , Translate 0 38 lampLit
    ]
  Present -> Pictures
    [ Color (hex 0x050709) (rectC 194 118)
    , Color (hex 0x38444A) (rectC 188 112)
    , Color (hexA 0x7A4A32 0.75) (Translate (-58) (-30) (rot 12 (rectC 62 26)))
    , Color (hexA 0x7A4A32 0.55) (Translate 66 28 (rot (-20) (rectC 44 30)))
    , thin (hex 0x10171B) [(70, 56), (62, 34), (72, 22), (66, 2)]
    , rivets (hex 0x1E272C)
    , Color (hex 0xE1E9EA) (Translate 0 (-8) (centred 28 3.6 "PRESENT"))
    , Color (hex 0xC2CCD0) (Translate 0 (-36) (textC 11 "Left to the sea"))
    , Translate 0 38 lampDead
    ]
  where
    centred size w s = Translate (negate (textW size s) / 2) 0 (weighted size w s)
    rivets c = Color c (Pictures [ Translate x y (ngon 7 2.6) | x <- [-85, 85], y <- [-46, 46] ])
    lampLit = Pictures
      [ Color (hex 0xFFF1C2) (ngon 14 7)
      , Color (hex 0x2A1A0E) (ring 14 7 1.5 1)
      , Color (hex 0x2A1A0E) (Pictures [ rot a (rect 10 (-1) 5 2) | a <- [0, 45, 90, 135, 180, 225, 270, 315] ])
      ]
    lampDead = Pictures
      [ Color (hex 0x10171B) (ngon 14 8)
      , Color (hex 0x83929A) (ring 14 8 1.6 1)
      , thin (hex 0x83929A) [(-4, 5), (1, 0), (-2, -4)]
      ]

controls :: Skin -> Picture
controls sk = Pictures
  [ row (-214) "WASD" "Move"
  , row (-250) "SPACE" "Shift era"
  , row (-286) "P" "Pause"
  , row (-322) "R" "Restart"
  ]
  where
    row y key what = Pictures
      [ Translate (panelX - 92) y (keyCap sk key)
      , Color (skInk sk) (Translate (panelX - 10) (y + 2) (textL 12 what))
      ]

-- | A key drawn as a key.
keyCap :: Skin -> String -> Picture
keyCap sk key = Pictures
  [ Color (skRule sk) (rect 0 (-6) w 22)
  , Color (fade 0.55 (hex 0x000000)) (rect 1.5 (-3) (w - 3) 17.5)
  , Color (skInk sk) (Translate (w / 2) 0 (textC 11 key))
  ]
  where w = max 26 (textW 11 key + 14)

clockText :: Int -> String
clockText ticks =
  let total = (ticks + ticksPerSecond - 1) `div` ticksPerSecond
      (m, s) = total `divMod` 60
  in show m ++ ":" ++ (if s < 10 then "0" else "") ++ show s

-- | The panel, with its live readings.
hud :: HudArt -> Game -> Float -> Picture
hud art g clock = Pictures
  [ case era of { Past -> artPast art; Present -> artPresent art }
    -- Countdown, with a tide line draining beneath it.
  , Color timeColor (Translate (panelX - 92) 128 (weighted 42 5.2 (clockText remaining)))
  , Color (fade 0.25 (skRule sk)) (rect (panelX - 92) 112 184 5)
  , Color timeColor (rect (panelX - 92) 112 (184 * share) 5)
    -- The cell slot.
  , Translate (panelX - 66) 30 (Pictures
      [ Color (skRule sk) (rect (-28) (-26) 56 52)
      , Color (hexA 0x000000 0.6) (rect (-26) (-24) 52 48)
      , Scale 0.95 0.95 (cellIcon (gHasCell g) clock)
      ])
  , Color (skInk sk) (Translate (panelX - 26) 35 (textL 13 cellState))
  , Color (skDim sk) (Translate (panelX - 26) 15 (textL 10 cellNote))
    -- Objective.
  , Color (skInk sk) (Pictures
      [ Translate (panelX - 92) (-62 - 22 * fromIntegral i) (textL 13 row)
      | (i, row) <- zip [0 :: Int ..] (wrapText 13 186 (objectiveText g)) ])
  ]
  where
    era = gEra g
    sk = skin era
    (cellState, cellNote)
      | gPhase g == Won = ("In the lamp", "Burning")
      | gHasCell g = ("Carried", "Travels with you")
      | otherwise = ("Not found", "Somewhere in time")
    remaining = gRemaining g
    share = fromIntegral remaining / fromIntegral levelTicks :: Float
    low = remaining < 30 * ticksPerSecond
    timeColor
      | low = mix (0.5 + 0.5 * sin (clock * 0.18)) (hex 0xFF6B4A) (hex 0xFFB39E)
      | otherwise = skInk sk

-- | Era name above the map, so it is always stated in words.
mapCaption :: Era -> Picture
mapCaption era = Pictures
  [ Color (skAccent sk) (Translate mapLeft (mapTop + 23) (textL 16 name))
  , Color (skDim sk) (Translate (mapLeft + textW 16 name + 18) (mapTop + 23) (textL 12 note))
  , Color (skDim sk) (Translate (mapRight - 4) (mapTop + 23) (textR 12 "Harrow Point Light Station"))
  ]
  where
    sk = skin era
    (name, note) = case era of
      Past -> ("PAST", "Lamps trimmed, brass polished")
      Present -> ("PRESENT", "Roof gone, the sea coming in")

-- | The same line, once the lamp is lit.
wonCaption :: Picture
wonCaption = bake $ Pictures
  [ Color (hex 0xFFE2A8) (Translate mapLeft (mapTop + 23) (textL 16 "PRESENT"))
  , Color (hex 0xD8BE90) (Translate (mapLeft + textW 16 "PRESENT" + 18) (mapTop + 23) (textL 12 "The lamp burns again"))
  , Color (hex 0xD8BE90) (Translate (mapRight - 4) (mapTop + 23) (textR 12 "Harrow Point Light Station"))
  ]

-- | One brief line under the map. The caller decides what, if anything.
hintBand :: Era -> Float -> String -> Picture
hintBand era k str = Pictures
  [ Color (hexA 0x000000 (0.55 * k)) (rect (mapLeft - 12) (mapBottom - 44) (mapRight - mapLeft + 24) 28)
  , Color (fade k (skInk (skin era))) (Translate ((mapLeft + mapRight) / 2) (mapBottom - 36) (textC 14 str))
  ]

-- | The refusal, pinned beside the cell it is about.
blockedPlate :: Era -> Cell -> Float -> Picture
blockedPlate era (x, y) k = Translate px py (Pictures
  [ Color (hexA 0x000000 (0.85 * k)) (rectC (w + 8) 62)
  , Color (fade k (hex 0xFF8A5C)) (Pictures [rect (-w / 2 - 4) 29 (w + 8) 2, rect (-w / 2 - 4) (-31) (w + 8) 2])
  , Color (fade k (hex 0xFFD9C8)) (Translate 0 4 (textC 15 blockedTitle))
  , Color (fade k (hex 0xF0E6DC)) (Translate 0 (-19) (textC 11 (blockedDetail (otherEra era))))
  ])
  where
    w = max (textW 15 blockedTitle) (textW 11 (blockedDetail (otherEra era))) + 22
    (cx, cy) = cellPos (x, y)
    px = max (mapLeft + w / 2 + 8) (min (mapRight - w / 2 - 8) cx)
    py = if y >= 3 then cy + 72 else cy - 72

centreOfMap :: Float
centreOfMap = (mapLeft + mapRight) / 2

-- | A wide plate over the map, for the screens that pause the game.
wide :: Float -> Float -> Float -> Color -> Color -> Picture
wide y h k fill edge = Translate centreOfMap y (Pictures
  [ Color (fade k fill) (rectC 620 h)
  , Color (fade k edge) (Pictures [rect (-310) (h / 2 - 2) 620 2, rect (-310) (-h / 2) 620 2])
  ])

pausedScreen :: Era -> Float -> Picture
pausedScreen era k = Pictures
  [ Color (hexA 0x05080A (0.55 * k)) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom))
  , wide 10 136 k (hexA 0x0B1014 0.93) (skRule (skin era))
  , Color (fade k (skInk (skin era))) (Translate centreOfMap 18 (weightedC 40 5 "PAUSED"))
  , Color (fade k (skDim (skin era))) (Translate centreOfMap (-30) (textC 14 "P resume   *   R restart"))
  ]

wonScreen :: Float -> Int -> Picture
wonScreen k remaining = Pictures
  [ wide (-236) 150 k (hexA 0x1C1208 0.86) (hex 0xE8B85C)
  , Color (fade k (hex 0xFFEFC4)) (Translate centreOfMap (-224) (weightedC 30 3.7 victoryText))
  , Color (fade k (hex 0xD8BE90)) (Translate centreOfMap (-254) (textC 13 ("Time left " ++ clockText remaining)))
  , Color (fade k (hex 0xFFEFC4)) (Translate centreOfMap (-287) (textC 13 "Press Enter to play again"))
  ]

lostScreen :: Float -> Picture
lostScreen k = Pictures
  [ Color (hexA 0x05080A (0.5 * k)) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom))
  , wide 10 150 k (hexA 0x0B1014 0.95) (hex 0xFF8A5C)
  , Color (fade k (hex 0xE6EDEE)) (Translate centreOfMap 22 (weightedC 32 4 failureText))
  , Color (fade k (hex 0x9AA9AE)) (Translate centreOfMap (-10) (textC 13 "Time ran out. The station stays dark."))
  , Color (fade k (hex 0xE6EDEE)) (Translate centreOfMap (-40) (textC 13 "Press Enter to try again"))
  ]

weightedC :: Float -> Float -> String -> Picture
weightedC size w s = Translate (negate (textW size s) / 2) 0 (weighted size w s)
