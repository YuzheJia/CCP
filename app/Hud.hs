-- | The instrument panel beside the map, and the plates that appear over it.
module Hud
  ( HudArt
  , mkHudArt
  , hud
  , mapCaption
  , wonCaption
  , hintBand
  , blockedPlate
  , tooCostlyPlate
  , pushBlockedPlate
  , pausedScreen
  , wonScreen
  , lostScreen
  , secondsText
  , costText
  , costColor
  ) where

import Draw
import Figures (cellIcon)
import Font
import Game.Fx
import Game.Logic
import Game.Types
import Graphics.Gloss
import Propaganda
import qualified Data.Set as Set

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
skin Past    = Skin (hex 0xFFF1D0) (hex 0xE3B341) (hex 0x8A2A1E) (hex 0xF2C14E)
skin Present = Skin (hex 0xD6E0E2) (hex 0x8A9AA0) (hex 0x4F5B60) (hex 0x9CC7D8)

mkHudArt :: Rules -> HudArt
mkHudArt rules = HudArt (bake (staticPanel rules Past)) (bake (staticPanel rules Present))

-- | Backing plate, era plaque, section labels and controls.
staticPanel :: Rules -> Era -> Picture
staticPanel rules era = Pictures
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
      [ label 184 "五分钟计划" "Plan" "Time left"
      , label 74 "人民电池" "Battery" "Power cell"
      , label (-36) "指示" "Directive" "Objective"
      , label (-182) "操作" "Controls" "Controls"
      ])
  , Color (skRule sk) (Pictures [ rect (panelX - 92) y 184 1 | y <- [200, 90, -20, -166] ])
  , controls rules sk
  ]
  where
    sk = skin era
    back = case era of
      Past -> hex 0x2E0607
      Present -> hex 0x10171B
    -- The past labels its gauges in Chinese first, as the poster would.
    label y zh en plain = case era of
      Past -> Translate (panelX - 92) (y - 3) (Pictures
        [ hanzi 15 zh
        , Translate (hanziW 15 zh + 8) 3 (textL 10 en) ])
      Present -> Translate (panelX - 92) y (textL 11 plain)

-- | Era named in material as well as words: polished brass, or corroded steel.
plaque :: Era -> Picture
plaque era = Translate panelX 298 $ case era of
  -- A little propaganda poster: sunburst, lighthouse, flag, emblem.
  Past -> Pictures
    [ Color (hex 0x17100B) (rectC 194 118)
    , Color (hex 0xB5161B) (rectC 188 112)
    , Color (hexA 0xF2C14E 0.28) (Translate 0 (-56) (sunburst 9 96 150))
    , Color (hex 0xF2C14E) (Translate 0 (-56) (Polygon [ (30 * cos a, 30 * sin a) | i <- [0 .. 16 :: Int], let a = pi * fromIntegral i / 16 ]))
    , Translate (-58) (-56) (Pictures
        [ Color (hex 0xFFF1D0) (Polygon [(-8, 0), (8, 0), (5, 40), (-5, 40)])
        , Color (hex 0xB5161B) (Pictures [rect (-7) 9 14 6, rect (-6) 25 12 6])
        , Color (hex 0xF2C14E) (rect (-6) 40 12 8)
        , Color (hex 0x7A0E12) (tri (-8, 48) (8, 48) (0, 56))
        ])
    , Color (hex 0x5A3A1E) (rect 50 (-56) 2.5 52)
    , Translate 52.5 (-4) (flag 30)
    , Color (hex 0xB5161B) (Translate 0 (-34) (hammerSickle 24))
    , Color (hex 0xE3B341) (Pictures [rect (-94) 53 188 3, rect (-94) (-56) 188 3, rect (-94) (-56) 3 112, rect 91 (-56) 3 112])
    , Color (hex 0xF2C14E) (Translate 0 31 (hanziC 22 "光荣的过去"))
    , Color (hex 0xFFF1D0) (Translate 0 4 (textC 11 "The glorious past"))
    ]
  -- The tourist board's sign, with a closure sticker slapped across it.
  Present -> Pictures
    [ Color (hex 0x050709) (rectC 194 118)
    , Color (hex 0x1F3A68) (rectC 188 112)
    , Color (hex 0xF1F3F2) (rectC 180 104)
    , Color (hex 0x1F3A68) (Translate 0 22 (centred 22 3 "PRESENT"))
    , Color (hex 0x2E6DB4) (Translate 0 6 (textC 9 "Harrow Point Lighthouse"))
    , Color (hex 0x4A5A60) (Translate 0 (-8) (textC 8 "Adults 12.50   Children 6"))
    , Color (hex 0x4A5A60) (Translate (-30) (-21) (textC 8 "Rated 2.1 / 5"))
    , Translate 36 (-36) (rot (-10) (Pictures
        [ Color (hexA 0x000000 0.3) (Translate 2 (-2) (rectC 92 20))
        , Color (hex 0xC8102E) (rectC 92 20)
        , Color (hex 0xFFFFFF) (Translate 0 (-6) (centred 13 2.2 "CLOSED"))
        ]))
    ]
  where
    centred size w s = Translate (negate (textW size s) / 2) 0 (weighted size w s)


-- | The prices are printed beside the keys, because they are the rules of the game.
controls :: Rules -> Skin -> Picture
controls rules sk = Pictures
  [ row y key what | (y, (key, what)) <- zip [-208, -208 - gap ..] keys ]
  where
    keys =
      [ ("WASD", "Move  -" ++ show (ruStepCost rules) ++ " s")
      , ("SPACE", "Shift  -" ++ show (ruShiftCost rules) ++ " s") ] ++
      [ ("E", "Echo  -" ++ show (ruEchoCost rules) ++ " s") | ruEchoLength rules > 0 ] ++
      [ ("Z", "Undo  +" ++ show (ruUndoRefundPercent rules) ++ " pct")
      , ("TAB", "Look  free")
      , ("R", "Restart  free")
      , ("P", "Pause")
      ]
    gap = if length keys > 6 then 27 else 30
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

secondsText :: Int -> String
secondsText s = show s ++ " s"

-- | The panel, with its live readings.
hud :: HudArt -> Level -> Game -> Fx -> Float -> Picture
hud art level g fx clock = Pictures
  [ case era of { Past -> artPast art; Present -> artPresent art }
    -- Time left, with a tide line draining beneath it.
  , Color timeColor (Translate (panelX - 92) 128 (weighted 42 5.2 (secondsText (ceiling shown))))
  , Color (fade 0.25 (skRule sk)) (rect (panelX - 92) 112 184 5)
  , Color timeColor (rect (panelX - 92) 112 (184 * share) 5)
  , costNote
    -- A push in the past changes a door you cannot see from there.
  , doorNote
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
      | gPhase g == Won = ("In the lamp", "Burning again")
      | gHasCell g && era == Past = ("Requisitioned", "Travels with you")
      | gHasCell g = ("Carried", "Travels with you")
      | otherwise = ("Not found", "Somewhere in time")
    doorNote
      | Set.null (lvDoors level) = Blank
      | otherwise = Color (skInk sk) (Translate (panelX - 92) (-142)
          (textL 13 (if doorsOpen level (weights g) then "The door is open" else "The door is shut")))
    rules = lvRules level
    budget = lvBudget level
    remaining = gRemaining g
    -- An action is charged when accepted, but the reading drains as it plays out,
    -- so the cost is felt rather than just subtracted.
    shown = fromIntegral remaining + stillDraining :: Float
    stillDraining = case gMotion g of
      Stepping (tx, ty) left ->
        let (fx', fy') = gCell g
            price = if abs (tx - fx') + abs (ty - fy') > 1 then ruJumpCost rules else ruStepCost rules
        in fromIntegral (price * left) / fromIntegral stepTicks
      Shifting left   -> fromIntegral (ruShiftCost rules * left) / fromIntegral shiftTicks
      Settled         -> 0
    share = shown / fromIntegral budget
    low = gPhase g /= Won && remaining * 5 <= budget
    costNote = case fxCost fx of
      Just c ->
        let t = fromIntegral (cfAge c) / fromIntegral costLife
        in Color (fade (1 - t * t) (costColor (cfAmount c)))
             (Translate (panelX + 92) (128 + 22 * t) (textR 20 (costText (cfAmount c))))
      Nothing -> Blank
    timeColor
      | low = mix (0.5 + 0.5 * sin (clock * 0.18)) (hex 0xFF6B4A) (hex 0xFFB39E)
      | otherwise = skInk sk

-- | How a change in time left is written.
costText :: Int -> String
costText n
  | n > 0 = "+" ++ show n
  | otherwise = show n

-- | Spending is hot, a refund is cool.
costColor :: Int -> Color
costColor n
  | n > 0 = hex 0x9CE0B0
  | otherwise = hex 0xFF9A6B

-- | Era name above the map, so it is always stated in words.
mapCaption :: String -> Era -> Picture
mapCaption title era = Pictures
  [ Color (skAccent sk) (Translate mapLeft (mapTop + 23) (textL 16 name))
  , Color (skAccent sk) (Translate (mapLeft + textW 16 name + 14) (mapTop + 17) (hanzi 18 zh))
  , Color (skDim sk) (Translate (mapLeft + textW 16 name + 14 + hanziW 18 zh + (if null zh then 4 else 16)) (mapTop + 23) (textL 12 note))
  , Color (skDim sk) (Translate (mapRight - 4) (mapTop + 23) (textR 12 title))
  ]
  where
    sk = skin era
    (name, zh, note) = case era of
      Past -> ("THE GLORIOUS PAST", "光荣的过去", "Lamps polished, quotas exceeded")
      Present -> ("PRESENT", "", "Closed for refurbishment since 2009")

-- | The same line, once the lamp is lit.
wonCaption :: String -> Picture
wonCaption title = bake $ Pictures
  [ Color (hex 0xFFE2A8) (Translate mapLeft (mapTop + 23) (textL 16 "PRESENT"))
  , Color (hex 0xD8BE90) (Translate (mapLeft + textW 16 "PRESENT" + 18) (mapTop + 23) (textL 12 "The lamp burns again"))
  , Color (hex 0xD8BE90) (Translate (mapRight - 4) (mapTop + 23) (textR 12 title))
  ]

-- | One brief line under the map. The caller decides what, if anything.
hintBand :: Era -> Float -> String -> Picture
hintBand era k str = Pictures
  [ Color (hexA 0x000000 (0.55 * k)) (rect (mapLeft - 12) (mapBottom - 44) (mapRight - mapLeft + 24) 28)
  , Color (fade k (skInk (skin era))) (Translate ((mapLeft + mapRight) / 2) (mapBottom - 36) (textC 14 str))
  ]

-- | The refusal, pinned beside the cell it is about.
blockedPlate :: Era -> Cell -> Float -> Picture
blockedPlate era cell = refusalPlate cell blockedTitle (blockedDetail (otherEra era))

-- | The same plate, when the only thing missing is time.
tooCostlyPlate :: Rules -> Cell -> Float -> Picture
tooCostlyPlate rules cell = refusalPlate cell tooCostlyTitle (tooCostlyDetail rules)

-- | The same plate, pinned to where a crate cannot go.
pushBlockedPlate :: Cell -> Float -> Picture
pushBlockedPlate cell = refusalPlate cell pushBlockedTitle pushBlockedDetail

refusalPlate :: Cell -> String -> String -> Float -> Picture
refusalPlate (x, y) title detail k = Translate px py (Pictures
  [ Color (hexA 0x000000 (0.85 * k)) (rectC (w + 8) 62)
  , Color (fade k (hex 0xFF8A5C)) (Pictures [rect (-w / 2 - 4) 29 (w + 8) 2, rect (-w / 2 - 4) (-31) (w + 8) 2])
  , Color (fade k (hex 0xFFD9C8)) (Translate 0 4 (textC 15 title))
  , Color (fade k (hex 0xF0E6DC)) (Translate 0 (-19) (textC 11 detail))
  ])
  where
    w = max (textW 15 title) (textW 11 detail) + 22
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

-- | The plate over the map when a level is won, knowing whether another follows.
wonScreen :: Bool -> Float -> Int -> Picture
-- The level is always won in the present, so the plate is the tourist board's.
wonScreen hasNext k remaining = Pictures
  [ wide (-232) 170 k (hexA 0x1F3A68 0.92) (hex 0xF2C200)
  , Color (fade k (hex 0xF2C200)) (Translate centreOfMap (-176) (textC 14 "GRAND REOPENING"))
  , Color (fade k (hex 0xFFFFFF)) (Translate centreOfMap (-218) (weightedC 30 3.7 victoryText))
  , Color (fade k (hex 0xB5D0F0)) (Translate centreOfMap (-248) (textC 13 ("Time left " ++ secondsText remaining ++ "   *   gift shop now open")))
  , Color (fade k (hex 0xFFFFFF)) (Translate centreOfMap (-284) (textC 13 next))
  ]
  where
    next
      | hasNext = "Enter next level   *   R play again"
      | otherwise = "Every lamp is lit   *   Enter returns to the title"

-- | Running out in the past gets the Party's verdict; in the present, the council's.
lostScreen :: Era -> Float -> Picture
lostScreen era k = Pictures
  [ Color (hexA 0x05080A (0.5 * k)) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom))
  , wide 10 170 k (hexA 0x0B1014 0.95) (hex 0xFF8A5C)
  , case era of
      Past -> Color (fade k (hex 0xFF8A5C)) (Translate centreOfMap 66 (hanziC 20 "再教育"))
      Present -> Blank
  , Color (fade k (hex 0xE6EDEE)) (Translate centreOfMap 20 (weightedC 32 4 headline))
  , Color (fade k (hex 0x9AA9AE)) (Translate centreOfMap (-12) (textC 13 detail))
  , Color (fade k (hex 0xE6EDEE)) (Translate centreOfMap (-42) (textC 13 "Z rewinds   *   Enter or R starts over"))
  ]


  where
    (headline, detail) = case era of
      Past -> ("The quota was not met", "Out of time. Please report for re-education.")
      Present -> (failureText, "Out of time. The station stays dark.")

weightedC :: Float -> Float -> String -> Picture
weightedC size w s = Translate (negate (textW size s) / 2) 0 (weighted size w s)
