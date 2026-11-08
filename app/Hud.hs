-- | The keeper's watch beside the board, the keys under it, and the plates that
-- appear over the board.
--
-- Time is the keeper's life, so it is read off a pocket watch: the ring of light is
-- the time left out of the level's budget, running back from twelve like a hand,
-- and the face says the seconds. One line under the number says the one thing that
-- matters about time right now: what was just spent, what a jump on offer would
-- cost, or, when time is short, how many steps are left. The era is named on a
-- plate hung under the case, in brass or in rusted steel, and the power cell has
-- a socket set into it.
module Hud
  ( HudArt
  , mkHudArt
  , hud
  , hudStopped
  , darkCaption
  , endedPlate
  , weightedC
  , mapCaption
  , mapCaptionFaded
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

import Data.Maybe (isJust)
import Draw
import Figures (Coat (..), cellIcon, redCoat, yellowCoat)
import Font
import Game.Fx
import Game.Logic
import Game.Types
import Graphics.Gloss
import qualified Data.Set as Set

-- | Everything about the watch and the keys that never changes, drawn once.
data HudArt = HudArt
  { artIntro   :: Era -> Picture  -- ^ the column beside the board while the level is introduced
  , artBack    :: Era -> Picture  -- ^ the case, behind the light
  , artFront   :: Era -> Picture  -- ^ the face, the era's plate and the socket, over it
  , artBeside  :: Era -> Picture  -- ^ under the watch: who is who, in a level for two
  , artKeys    :: Era -> Picture  -- ^ the keys under the board
  , artKeysLow :: Era -> Picture  -- ^ the same when time is short, the way out lit
  , artTeach   :: Bool            -- ^ whether this level writes its keys at all
  }

data Skin = Skin { skInk, skDim, skRule, skAccent :: Color }

skin :: Era -> Skin
skin Past    = Skin (hex 0xF3E2BE) (hex 0xB89E76) (hex 0x8A6A3F) (hex 0xE8B85C)
skin Present = Skin (hex 0xD6E0E2) (hex 0x8A9AA0) (hex 0x4F5B60) (hex 0x9CC7D8)

-- | The watch's materials. In the past it is polished brass and its light is the
-- lamp's; in the present it is corroded steel and its light is cold.
data Case = Case
  { caTop, caMid, caBot, caEdge, caTrack, caFaceIn, caFaceOut :: Color
  , caLightA, caLightB, caGlow :: Color
  , caGlowK :: Float
  , caPlate, caPlateHi, caPlateLo, caPlateInk :: Color
  , caName :: String
  }

caseOf :: Era -> Case
caseOf Past = Case
  { caTop = hex 0xF2D79A, caMid = hex 0xC2964F, caBot = hex 0x7E5F35, caEdge = hex 0x17100B
  , caTrack = hex 0x24170C, caFaceIn = hex 0x3A2718, caFaceOut = hex 0x1E140B
  , caLightA = hex 0xFFE9AE, caLightB = hex 0xE8B04E, caGlow = hex 0xFFC879, caGlowK = 0.75
  , caPlate = hex 0xC2964F, caPlateHi = hex 0xE9CB88, caPlateLo = hex 0x8A6A3F, caPlateInk = hex 0x2A1A0E
  , caName = "PAST" }
caseOf Present = Case
  { caTop = hex 0x66757B, caMid = hex 0x3E4A50, caBot = hex 0x20292D, caEdge = hex 0x050709
  , caTrack = hex 0x0A1013, caFaceIn = hex 0x18222A, caFaceOut = hex 0x0B1114
  , caLightA = hex 0xD4EEF5, caLightB = hex 0x86B8CB, caGlow = hex 0x9CC7D8, caGlowK = 0.35
  , caPlate = hex 0x38444A, caPlateHi = hex 0x55636A, caPlateLo = hex 0x1E272C, caPlateInk = hex 0xE1E9EA
  , caName = "PRESENT" }

-- | Time running short: the light turns to embers, whatever the era.
emberA, emberB, emberInk, rustC :: Color
emberA = hex 0xFFC2AE
emberB = hex 0xFF6B4A
emberInk = hex 0xFF8A66
rustC = hex 0x7A4A32

-- Layout -----------------------------------------------------------------------

-- | While a level is introduced the watch stands large at the head of a column
-- left of the board, over the level's name and its keys. Once the keeper acts it
-- shrinks into the margin left of the board, its crown level with the board's
-- top edge. Each place is (across, up, scale), the scale of the sizes below.
introWatch, playWatch :: (Float, Float, Float)
introWatch = ((negate canvasW / 2 + fst (onBoard 0 (mapLeft - 12, 0))) / 2, 250, 1.45)
playWatch = ((negate canvasW / 2 + fst (onBoard 1 (mapLeft - 12, 0))) / 2, 281, 1)

watchAt :: Float -> (Float, Float, Float)
watchAt f = let (x0, y0, k0) = introWatch
                (x1, y1, k1) = playWatch
            in (lerp f x0 x1, lerp f y0 y1, lerp f k0 k1)

-- | The bottom of the era's plate under the watch, in play.
underPlay :: Float
underPlay = let (_, y, k) = playWatch in y - 83.5 * k

-- | Where the introduction's column starts across, and how wide it is.
columnLeft, columnW :: Float
columnLeft = let (x, _, _) = introWatch in x - 132
columnW = 264

rCase, rTrackOut, rTrackIn, rLight, wLight, rFace, rSocket :: Float
rCase = 62
rTrackOut = 56.5
rTrackIn = 43.5
rLight = 50
wLight = 9
rFace = 43.5
rSocket = 16.5

-- | The socket for the power cell sits at four o'clock.
socketAt :: (Float, Float)
socketAt = clockAt (rCase + 2) 128

-- | A point at a clock angle: degrees from twelve, clockwise.
clockAt :: Float -> Float -> (Float, Float)
clockAt r a = (r * sin t, r * cos t)
  where t = a * pi / 180

-- | A band between two radii, from one clock angle to a later one. Each sliver
-- takes its colour from its angle, so a band can be lit from above.
band :: Float -> Float -> Float -> Float -> (Float -> Color) -> Picture
band r0 r1 a0 a1 col
  | a1 - a0 <= 0.01 = Blank
  | otherwise = Pictures
      [ Color (col ((u + v) / 2)) (Polygon [clockAt r0 u, clockAt r1 u, clockAt r1 v, clockAt r0 v])
      | i <- [0 .. n - 1]
      , let u = a0 + (a1 - a0) * fromIntegral i / fromIntegral n
            v = a0 + (a1 - a0) * fromIntegral (i + 1) / fromIntegral n ]
  where n = max 1 (ceiling ((a1 - a0) / 3)) :: Int

-- | Lit from above: the first colour at twelve, the last at six.
fromAbove :: Color -> Color -> Color -> Float -> Color
fromAbove top mid bot a
  | t < 0.45 = mix (t / 0.45) top mid
  | otherwise = mix ((t - 0.45) / 0.55) mid bot
  where t = (1 - cos (a * pi / 180)) / 2

-- | Small lettering for the watch, drawn at a size whose curves are cut finely and
-- whose joints are round, then scaled down: at its own size an S grows ticks.
fine :: Float -> Float -> String -> Picture
fine size w s = Scale k k (weighted 26 (w / k) s)
  where k = size / 26

fineW :: Float -> String -> Float
fineW size s = textW 26 s * size / 26

centred :: Float -> Float -> String -> Picture
centred size w s = Translate (negate (fineW size s) / 2) 0 (fine size w s)

-- The case -----------------------------------------------------------------------

-- | Shadow, crown, case and the channel the light runs in, with the hours engraved
-- in it where the light has gone.
caseBack :: Era -> Picture
caseBack era = Pictures
  [ Color (hexA 0x000000 0.5) (Translate 0 (-3) (ngon 64 (rCase + 2)))
  , Color (caEdge c) (rect (-9.5) (rCase - 3.5) 19 14)
  , Color (caMid c) (rect (-8) (rCase - 2) 16 11)
  , Color (caTop c) (rect (-8) (rCase + 6) 16 3)
  , Color (fade 0.45 (caEdge c)) (Pictures [ rect (x - 0.5) rCase 1 7 | x <- [-4, 0, 4] ])
  , Color (caEdge c) (ngon 72 (rCase + 1))
  , band rTrackOut rCase 0 360 (fromAbove (caTop c) (caMid c) (caBot c))
  , band (rCase - 2.8) (rCase - 1.8) 0 360 (const (hexA 0xFFFFFF (if era == Past then 0.28 else 0.12)))
  , if era == Present then rust else Blank
  , band rTrackIn rTrackOut 0 360 (const (caTrack c))
  , Pictures
      [ Color (fade (if major then 0.9 else 0.55) (skRule sk))
          (seg (if major then 1.6 else 1) (clockAt (rTrackIn + inset) a) (clockAt (rTrackOut - inset) a))
      | h <- [0 .. 11 :: Int]
      , let a = 30 * fromIntegral h
            major = h `mod` 3 == 0
            inset = if major then 2 else 3.5 ]
  ]
  where
    c = caseOf era
    sk = skin era
    -- Only on the case, never on the reading.
    rust = Pictures
      [ band (rTrackOut + 0.5) (rCase - 0.8) 206 238 (const (fade 0.8 rustC))
      , band (rCase - 4) (rCase - 0.5) 238 248 (const (fade 0.6 rustC))
      , band (rTrackOut + 1.5) (rCase - 1) 38 56 (const (fade 0.7 rustC))
      , band (rTrackOut + 0.5) (rCase - 0.5) 90 101 (const (hexA 0x6A3E2A 0.7))
      ]

-- | The face, the era's plate hung under the case, and the socket's rim.
caseFront :: Era -> Picture
caseFront era = Pictures
  [ Color (caEdge c) (ngon 64 (rFace + 0.75))
  , Pictures
      [ Color (mix k (caFaceOut c) (caFaceIn c)) (Translate 0 (3 * k) (ngon 64 (rFace * (1 - 0.12 * fromIntegral i))))
      | i <- [0 .. 5 :: Int], let k = fromIntegral i / 5 ]
  , plate
  , uncurry Translate socketAt socketRim
  ]
  where
    c = caseOf era
    pw = if era == Past then 74 else 96
    ph = 24
    py = negate (rCase - 4) - ph
    plate = Pictures
      [ Color (caEdge c) (rect (-pw / 2 - 1.5) (py - 1.5) (pw + 3) (ph + 3))
      , Color (caPlate c) (rect (-pw / 2) py pw ph)
      , Color (caPlateHi c) (rect (-pw / 2 + 2) (py + ph - 3.1) (pw - 4) 1.6)
      , Color (caPlateLo c) (rect (-pw / 2 + 2) (py + 1.4) (pw - 4) 1.6)
      , if era == Present then Color (fade 0.55 rustC) (Translate (-pw / 2 + 8) (py + 4) (rot 12 (ellipse 16 8 3.5))) else Blank
      , Color (caPlateLo c) (Pictures [ Translate (s * (pw / 2 - 6)) (py + ph / 2) (ngon 8 1.8) | s <- [-1, 1] ])
      , Color (caPlateInk c) (Translate 0 (py + ph / 2 - 6.5) (centred 13 2.1 (caName c)))
      ]
    socketRim = Pictures
      [ Color (hexA 0x000000 0.5) (Translate 0 (-2) (ngon 32 (rSocket + 1)))
      , Color (caEdge c) (ngon 32 (rSocket + 1.6))
      , band 0 rSocket 0 360 (fromAbove (caTop c) (caMid c) (caBot c))
      , Color (caEdge c) (ngon 32 (rSocket - 3.2))
      , Color (caTrack c) (ngon 32 (rSocket - 4))
      ]

-- | An empty socket: the cell's silhouette, terminal and all, unlit.
emptyCell :: Picture
emptyCell = Pictures
  [ Color (hexA 0xFFFFFF 0.1) (rect (-4.2) (-7) 8.4 12.5)
  , Color (hexA 0xFFFFFF 0.32) (Pictures
      [ segs 1.2 [(-4.2, -7), (4.2, -7), (4.2, 5.5), (-4.2, 5.5), (-4.2, -7)]
      , rect (-2.2) 5.8 4.4 2.4 ])
  ]

-- | A key direction, drawn as an arrow.
arrowFor :: Dir -> Picture
arrowFor d = Rotate angle (Pictures
  [ seg 1.4 (0, -3.6) (0, 3.2), seg 1.4 (-2.8, 0.6) (0, 3.4), seg 1.4 (2.8, 0.6) (0, 3.4) ])
  where
    angle = case d of
      DirUp -> 0
      DirRight -> 90
      DirDown -> 180
      DirLeft -> 270

-- The keys ------------------------------------------------------------------------

data KeyLook = Plain | Dimmed | Hot

-- | A key drawn as a key.
keyCap :: Color -> Color -> String -> Picture
keyCap edge ink key = Pictures
  [ Color edge (rect 0 (-6) w 22)
  , Color (fade 0.55 (hex 0x000000)) (rect 1.5 (-3) (w - 3) 17.5)
  , Color ink (Translate (w / 2) 0 (textC 11 key))
  ]
  where w = capW key

capW :: String -> Float
capW key = max 26 (textW 11 key + 14)

-- | One line of keys and what they cost, centred under the board.
keyStrip :: Skin -> [(String, String, KeyLook)] -> Picture
keyStrip sk items = Pictures (go x0 items)
  where
    inner = 8
    gap = 26
    itemW (key, label, _) = capW key + inner + textW 12 label
    total = sum (map itemW items) + gap * fromIntegral (length items - 1)
    x0 = fst (onBoard 1 ((mapLeft + mapRight) / 2, 0)) - total / 2
    y = negate canvasH / 2 + 12
    go _ [] = []
    go x (it@(key, label, look) : rest) = Translate x y (item key label look) : go (x + itemW it + gap) rest
    item key label look =
      let (edge, ink, words') = case look of
            Plain -> (skRule sk, skInk sk, skDim sk)
            Dimmed -> (fade 0.45 (skRule sk), fade 0.45 (skInk sk), fade 0.45 (skDim sk))
            Hot -> (hex 0xFF8A5C, hex 0xFFE4D8, hex 0xFFB39E)
      in Pictures [ keyCap edge ink key, Color words' (Translate (capW key + inner) 0 (textL 12 label)) ]

-- | The prices are printed beside the keys, because they are the rules of the game.
taughtKeys :: Rules -> [(String, String)]
taughtKeys rules =
  [ ("WASD", "Move  -" ++ show (ruStepCost rules) ++ " s")
  , ("SPACE", "Shift  -" ++ show (ruShiftCost rules) ++ " s") ] ++
  [ ("E", "Echo  -" ++ show (ruEchoCost rules) ++ " s") | ruEchoLength rules > 0 ] ++
  [ ("Z", "Undo  " ++ refund)
  , ("TAB", "Look  free")
  , ("R", "Restart")
  , ("P", "Pause")
  ]
  where
    refund
      | ruUndoRefundPercent rules == 50 = "+half"
      | otherwise = "+" ++ show (ruUndoRefundPercent rules) ++ " pct"

-- | The keys that get you out of trouble.
wayOut :: String -> Bool
wayOut key = key `elem` ["Z", "R"]

-- | Under the watch in a level for two: who is who, and the key they share. The
-- level is never taught, so its keys are always written.
duoLegend :: Skin -> Picture
duoLegend sk = Pictures
  [ Color (skDim sk) (Translate wx (underPlay - 19) (textC 11 "Shared clock"))
  , row (underPlay - 41) (Just yellowCoat) "WASD" ""
  , row (underPlay - 73) (Just redCoat) "ARROWS" ""
  , row (underPlay - 105) Nothing "SPACE" "both"
  ]
  where
    (wx, _, _) = playWatch
    lx = negate canvasW / 2 + 14
    row y coat key note = Pictures
      [ maybe Blank (Translate (lx + 9) (y + 5) . hood) coat
      , Translate (lx + 26) y (keyCap (skRule sk) (skInk sk) key)
      , Color (skDim sk) (Translate (lx + 26 + capW key + 8) y (textL 11 note))
      ]
    hood c = Pictures
      [ Color (hexA 0x000000 0.5) (ngon 20 10.5)
      , Color (coatMain c) (ngon 20 9)
      , Color (hex 0xF6D9B6) (Translate 0 (-1.5) (ngon 16 5.2))
      , Color (hex 0x3A2A1A) (Pictures [ Translate x (-1) (ngon 6 0.9) | x <- [-2, 2] ])
      ]

-- | The keys a level is introduced with. A level for two names each keeper's keys.
introKeys :: Bool -> Rules -> [(String, String)]
introKeys duo rules
  | duo = [ ("WASD", "Yellow  -" ++ show (ruStepCost rules) ++ " s")
          , ("ARROWS", "Red  -" ++ show (ruStepCost rules) ++ " s")
          , ("SPACE", "Both  -" ++ show (ruShiftCost rules) ++ " s") ]
          ++ [ k | k@(key, _) <- taughtKeys rules, key `notElem` ["WASD", "SPACE"] ]
  | otherwise = taughtKeys rules

-- | The column beside the board while a level is introduced: the level's place in
-- the hall, its name and a line about the era, then every key and its price.
-- The watch stands at its head. It leaves as soon as the keeper acts.
introColumn :: Bool -> (String, String) -> Rules -> Era -> Picture
introColumn duo (place, name) rules era = Pictures
  [ backdrop
  , Color (skRule sk) (rect columnLeft 100 columnW 1)
  , Color (skDim sk) (Translate columnLeft 74 (textL 11 place))
  , Color (skAccent sk) (Translate columnLeft 46 (textL 18 name))
  , Color (skDim sk) (Translate columnLeft 22 (textL 11 (eraNote era)))
  , Color (skRule sk) (rect columnLeft 0 columnW 1)
  , Pictures
      [ Pictures [ Translate columnLeft y (keyCap (skRule sk) (skInk sk) key)
                 , Color (skInk sk) (Translate (columnLeft + 86) y (textL 12 what)) ]
      | (y, (key, what)) <- zip [-38, -72 ..] (introKeys duo rules) ]
  ]
  where
    sk = skin era
    -- A soft shadow behind the column, so the sea moving under it never fights the
    -- lettering.
    edge = fst (onBoard 0 (mapLeft - 12, 0)) - 8
    w = (edge + canvasW / 2) / 12
    backdrop = Pictures
      [ Color (hexA 0x000000 (0.3 * (1 - fromIntegral i / 12))) (rect (negate canvasW / 2 + fromIntegral i * w) (negate canvasH / 2) w canvasH)
      | i <- [0 .. 11 :: Int] ]

-- | A line about the era, under a level's name.
eraNote :: Era -> String
eraNote Past = "Lamps trimmed, brass polished"
eraNote Present = "Roof gone, the sea coming in"

-- The watch, live -----------------------------------------------------------------

-- | @duo@: a level for two. @teach@: write the keys under the board in play.
-- @title@: the level's place in the hall and its name.
mkHudArt :: Bool -> Bool -> (String, String) -> Level -> HudArt
mkHudArt duo teach title level = HudArt
  { artIntro = perEra (bake . introColumn duo title (lvRules level))
  , artBack = perEra (bake . caseBack)
  , artFront = perEra (bake . caseFront)
  , artBeside = perEra (\e -> if duo then bake (duoLegend (skin e)) else Blank)
  , artKeys = perEra (\e -> bake (keyStrip (skin e) [ (k, l, Plain) | (k, l) <- taught ]))
  , artKeysLow = perEra (\e -> bake (keyStrip (skin e) lowKeys))
  , artTeach = teach
  }
  where
    taught = taughtKeys (lvRules level)
    lowKeys
      | teach = [ (k, l, if wayOut k then Hot else Dimmed) | (k, l) <- taught ]
      | otherwise = [ (k, l, Hot) | (k, l) <- taught, wayOut k ]
    perEra f = let p = f Past
                   q = f Present
               in \e -> case e of
                    Past -> p
                    Present -> q

secondsText :: Int -> String
secondsText s = show s ++ " s"

-- | The watch, the keys and whatever stands under the watch.
hud :: HudArt -> Level -> Game -> Fx -> Float -> Picture
hud = watchHud False

-- | The same, once an ending has the keeper walking free: the watch stops where
-- it was, and nothing it says is a warning.
hudStopped :: HudArt -> Level -> Game -> Fx -> Float -> Picture
hudStopped = watchHud True

watchHud :: Bool -> HudArt -> Level -> Game -> Fx -> Float -> Picture
watchHud stopped art level g fx clock = Pictures
  [ fadePicture introK (artIntro art era)
  , Translate wx wy (Scale wk wk (Pictures
      [ halo
      , artBack art era
      , trail
      , lightGlow
      , band (rLight - wLight / 2) (rLight + wLight / 2) 0 mainTo lightCol
      , previewPic
      , refundPic
      , shortPic
      , bead
      , artFront art era
      , numberPic
      , ctxPic
      , uncurry Translate socketAt socketPic
      ]))
  , fadePicture f (artBeside art era)
  , fadePicture f roomMap
  , fadePicture f doorNote
  , keysLine
  ]
  where
    -- 0 while the level is introduced, 1 once the screen has closed in on the
    -- board. The column goes in the first half of the move; the rest comes in.
    progress = if stopped then 1 else focusProgress fx
    f = smooth progress
    introK = 1 - smooth (min 1 (progress * 2))
    (wx, wy, wk) = watchAt f
    (pwx, _, _) = playWatch
    era = gEra g
    sk = skin era
    c = caseOf era
    rules = lvRules level
    budget = max 1 (lvBudget level)
    remaining = gRemaining g
    duo = isJust (gPartner g)
    pulse = 0.5 + 0.5 * sin (clock * 0.18)

    -- An action is charged when accepted, but the reading drains as it plays out,
    -- so the cost is felt rather than just subtracted.
    shown = fromIntegral remaining + stillDraining :: Float
    stillDraining = case gMotion g of
      Stepping (tx, ty) left ->
        let (fx', fy') = gCell g
            price = if abs (tx - fx') + abs (ty - fy') > 1 then ruJumpCost rules else ruStepCost rules
        in fromIntegral (price * left) / fromIntegral stepTicks
      Shifting left -> fromIntegral (ruShiftCost rules * left) / fromIntegral shiftTicks
      Settled -> 0
    deg v = 360 * clamp01 (v / fromIntegral budget)
    remDeg = deg (fromIntegral remaining)

    -- Once the level is won, or its ending is running, time no longer matters.
    live = not stopped && gPhase g /= Won
    low = live && remaining * 5 <= budget
    (lightA, lightB, glowC, glowK)
      | low = (emberA, emberB, emberB, 0.9)
      | otherwise = (caLightA c, caLightB c, caGlow c, caGlowK c)
    lightCol = fromAbove lightA (mix 0.5 lightA lightB) lightB

    -- What the last action spent, or an undo gave back, for as long as it floats.
    costNow
      | live = fxCost fx
      | otherwise = Nothing
    costAge cf = fromIntegral (cfAge cf) / fromIntegral costLife :: Float

    -- A jump the board is offering: the keeper stands still in a mirror's light,
    -- with open floor where the reflection stands.
    offer
      | not live || gPhase g /= Playing || tiPeek (gPrev g) = Nothing
      | otherwise = case [ d | gg <- g : [ swapKeepers g | duo ], gMotion gg == Settled
                             , d <- [minBound .. maxBound]
                             , Just j <- [mirrorAhead level gg d], jOpen j ] of
          d : _ -> Just d
          [] -> Nothing
    -- A shift refused for want of time.
    shortfall = case gFeedback g of
      Just (Feedback ShiftTooCostly left) | live -> Just (clamp01 (fromIntegral left / 20))
      _ -> Nothing

    -- The price on offer, or the whole of what is left when it is not enough: still
    -- yours, but marked to go.
    previewRange
      | Just _ <- shortfall = Just (0, remDeg)
      | Just _ <- offer = Just (deg (fromIntegral (remaining - ruJumpCost rules)), remDeg)
      | otherwise = Nothing
    mainTo = maybe (deg shown) fst previewRange

    halo
      | low = Pictures
          [ band (rCase + e0) (rCase + e1) 0 360 (const (fade (a * (0.6 + 0.4 * pulse)) emberB))
          | (e0, e1, a) <- [(0, 4, 0.32), (4, 8, 0.18), (8, 13, 0.08)] ]
      | otherwise = Blank

    -- The wake of the last spend, fading, as a fighting game leaves a damage trail.
    trail = case costNow of
      Just cf | cfAmount cf < 0 ->
        let p = costAge cf
        in band (rLight - wLight / 2) (rLight + wLight / 2) remDeg (deg (fromIntegral (remaining - cfAmount cf)))
             (const (fade (0.75 * (1 - p * p)) (costColor (cfAmount cf))))
      _ -> Blank

    lightGlow = Pictures
      [ band (rLight - wLight / 2 - e) (rLight + wLight / 2 + e) 0 (deg shown) (const (fade (a * glowK) glowC))
      | (e, a) <- [(3, 0.3), (6.5, 0.15), (10, 0.07)] ]

    previewPic = case previewRange of
      Just (a0, a1) | a1 > a0 ->
        let hatchK = 0.65 + 0.35 * sin (clock * 0.15)
            inner = rLight - wLight / 2
            outer = rLight + wLight / 2
        in Pictures
             [ band inner outer a0 a1 (const (fade 0.3 lightA))
             , Pictures [ band inner outer u (min a1 (u + 1.8)) (const (fade hatchK lightA))
                        | u <- takeWhile (< a1) [a0 + 0.6, a0 + 4.9 ..] ]
             , Color (skInk sk) (Pictures [ seg 1.6 (clockAt (rTrackIn - 1) a) (clockAt (rTrackOut + 1) a) | a <- [a0, a1] ])
             ]
      _ -> Blank

    refundPic = case costNow of
      Just cf | cfAmount cf > 0 ->
        let p = costAge cf
        in band (rLight - wLight / 2) (rLight + wLight / 2) (deg (fromIntegral (remaining - cfAmount cf))) remDeg
             (const (fade (1 - p * p) (costColor (cfAmount cf))))
      _ -> Blank

    -- The part of a refused price that is not there, as an outline.
    shortPic = case shortfall of
      Just k ->
        let a1 = deg (fromIntegral (ruShiftCost rules))
        in Pictures [ band (rLight - 3.5) (rLight + 3.5) u (min a1 (u + 3)) (const (fade k emberB))
                    | u <- takeWhile (< a1) [remDeg + 1, remDeg + 6.6 ..] ]
      Nothing -> Blank

    -- The bead of light at the front, where the next second goes from.
    bead
      | shown > 0 && shown < fromIntegral budget =
          uncurry Translate (clockAt rLight (deg shown)) (Pictures
            [ glow glowC 9 3, Color (hex 0xFFFDF6) (ngon 12 3) ])
      | otherwise = Blank

    numberPic =
      let txt = show (ceiling shown :: Int)
          size = case length txt of
            1 -> 34
            2 -> 34
            3 -> 29
            _ -> 24
          ink | low = mix pulse emberB (hex 0xFFB39E)
              | otherwise = skInk sk
      in Color ink (Translate 0 (-12) (Scale (size / 40) (size / 40) (weightedC 40 5 txt)))

    -- The one line under the number, most pressing first.
    ctx
      | Just k <- shortfall = Just ("shift " ++ show (ruShiftCost rules), emberInk, k, Nothing)
      | Just d <- offer = Just ("jump " ++ costText (negate (ruJumpCost rules)), skAccent sk, 1, Just d)
      | Just cf <- costNow, cfAmount cf /= 0 =
          let p = costAge cf in Just (costText (cfAmount cf), costColor (cfAmount cf), 1 - p * p, Nothing)
      | low = Just (stepsLeft, emberInk, 1, Nothing)
      | otherwise = Nothing
    stepsLeft =
      let n = remaining `div` max 1 (ruStepCost rules)
      in show n ++ (if n == 1 then " step" else " steps")
    ctxPic = case ctx of
      Nothing -> Blank
      Just (str, col, k, arrow) -> Color (fade k col) (Translate 0 (-30) (case arrow of
        Nothing -> centred 10.5 1.5 str
        Just d ->
          let total = 11 + fineW 10.5 str
          in Translate (negate total / 2) 0 (Pictures
               [ Translate 3.5 3.8 (arrowFor d), Translate 11 0 (fine 10.5 1.5 str) ])))

    socketPic
      | live && anyHasCell g = Scale 0.45 0.45 (cellIcon True clock)
      | otherwise = emptyCell

    -- A level of several screens gets a small map of its rooms under the watch:
    -- the rooms seen so far, and the one on screen picked out.
    roomsAcross = (lvWidth level + roomCols - 1) `div` roomCols
    roomsDown = (lvHeight level + roomRows - 1) `div` roomRows
    hasMap = roomsAcross * roomsDown > 1 && roomedLevel level
    mapW = fromIntegral roomsAcross * 31 - 3
    mapTopY = if duo then underPlay - 135 else underPlay - 16
    roomMap
      | not hasMap = Blank
      | otherwise = Pictures
          [ Translate (pwx - mapW / 2 + fromIntegral rx * 31) (mapTopY - 18 - fromIntegral ry * 21) (roomTile (rx, ry))
          | ry <- [0 .. roomsDown - 1], rx <- [0 .. roomsAcross - 1] ]
    roomTile r
      | r == fxRoom fx = Pictures
          [ Color (skAccent sk) (rect 0 0 28 18)
          , Color (hexA 0x000000 0.35) (rect 3 3 22 12)
          , Color (skAccent sk) (Translate 14 9 (ngon 10 3.2)) ]
      | r `elem` fxVisited fx = Color (skRule sk) (rect 0 0 28 18)
      | otherwise = Color (fade 0.6 (skRule sk)) (Pictures
          [ rect 0 0 28 1.5, rect 0 16.5 28 1.5, rect 0 0 1.5 18, rect 26.5 0 1.5 18 ])

    -- A push in the past changes a door you cannot see from there.
    doorNote
      | Set.null (lvDoors level) = Blank
      | otherwise =
          let y = mapTopY - 14 - (if hasMap then fromIntegral roomsDown * 21 + 8 else 0)
          in Color (skInk sk) (Translate pwx y
               (textC 12 (if doorsOpen level (weights g) then "Door open" else "Door shut")))

    keysLine
      | stopped || gPhase g /= Playing = Blank
      | low = artKeysLow art era
      | artTeach art = fadePicture f (artKeys art era)
      | otherwise = Blank

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

-- Over the board ------------------------------------------------------------------

-- | The level's name above the board, with a line about the era, and its place in
-- the hall at the other end. The era itself is named on the watch.
mapCaption :: (String, String) -> Era -> Picture
mapCaption title era = captionWith title (skAccent sk) (skDim sk) (eraNote era)
  where sk = skin era

-- | The same line, part way in.
mapCaptionFaded :: Float -> (String, String) -> Era -> Picture
mapCaptionFaded k title era = fadePicture k (mapCaption title era)

captionWith :: (String, String) -> Color -> Color -> String -> Picture
captionWith (place, name) strong soft note = Pictures
  [ Color strong (Translate mapLeft (mapTop + 23) (textL 16 name))
  , Color soft (Translate (mapLeft + textW 16 name + 18) (mapTop + 23) (textL 12 note))
  , Color soft (Translate (mapRight - 4) (mapTop + 23) (textR 12 place))
  ]

-- | The same line, once the lamp is lit.
wonCaption :: (String, String) -> Picture
wonCaption title = bake (captionWith title (hex 0xFFE2A8) (hex 0xD8BE90) "The lamp burns again")

-- | The line above the map once the lamp has gone out again.
darkCaption :: (String, String) -> Picture
darkCaption title = bake (captionWith title (skAccent sk) (skDim sk) "The lamp has gone out")
  where sk = skin Present

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
  , Color (fade k (skDim (skin era))) (Translate centreOfMap (-30) (textC 14 "P resume   *   R restart   *   ESC level hall"))
  ]

-- | The plate over the map when a level is won, knowing whether another follows.
wonScreen :: Bool -> Float -> Int -> Picture
wonScreen hasNext k remaining = Pictures
  [ wide (-236) 150 k (hexA 0x1C1208 0.86) (hex 0xE8B85C)
  , Color (fade k (hex 0xFFEFC4)) (Translate centreOfMap (-224) (weightedC 30 3.7 victoryText))
  , Color (fade k (hex 0xD8BE90)) (Translate centreOfMap (-254) (textC 13 ("Time left " ++ secondsText remaining)))
  , Color (fade k (hex 0xFFEFC4)) (Translate centreOfMap (-287) (textC 13 next))
  ]
  where
    next
      | hasNext = "Enter next level   *   R play again"
      | otherwise = "Every lamp is lit   *   Enter returns to the level hall"

lostScreen :: Float -> Picture
lostScreen k = Pictures
  [ Color (hexA 0x05080A (0.5 * k)) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom))
  , wide 10 150 k (hexA 0x0B1014 0.95) (hex 0xFF8A5C)
  , Color (fade k (hex 0xE6EDEE)) (Translate centreOfMap 22 (weightedC 32 4 failureText))
  , Color (fade k (hex 0x9AA9AE)) (Translate centreOfMap (-10) (textC 13 "Out of time. The station stays dark."))
  , Color (fade k (hex 0xE6EDEE)) (Translate centreOfMap (-40) (textC 13 "Z rewinds   *   Enter or R starts over   *   ESC level hall"))
  ]

weightedC :: Float -> Float -> String -> Picture
weightedC size w s = Translate (negate (textW size s) / 2) 0 (weighted size w s)

-- | The plate for a win that is not the end. It says what the last plate of all
-- says, and offers nothing to press.
endedPlate :: Float -> Int -> Picture
endedPlate k remaining = Pictures
  [ wide (-236) 150 k (hexA 0x1C1208 0.86) (hex 0xE8B85C)
  , Color (fade k (hex 0xFFEFC4)) (Translate centreOfMap (-224) (weightedC 30 3.7 victoryText))
  , Color (fade k (hex 0xD8BE90)) (Translate centreOfMap (-254) (textC 13 ("Time left " ++ secondsText remaining)))
  , Color (fade k (hex 0xFFEFC4)) (Translate centreOfMap (-287) (textC 13 "Every lamp is lit"))
  ]
