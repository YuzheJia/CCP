-- | Composes one frame from the game, the effects and the prebuilt art.
module Render (Scene, mkScene, render) where

import Draw
import Figures
import Game.Fx
import Game.Logic
import Game.Types
import Graphics.Gloss
import Hud
import Station
import Title

-- | Everything that is expensive to build and never changes.
data Scene = Scene
  { scPastCells    :: [(Cell, Picture)]
  , scPresentCells :: [(Cell, Picture)]
  , scPastWhole    :: Picture
  , scPresentWhole :: Picture
  , scPastBack     :: Picture
  , scPresentBack  :: Picture
  , scPastVig      :: Picture
  , scPresentVig   :: Picture
  , scPastCaption  :: Picture
  , scPresentCaption :: Picture
  , scHud          :: HudArt
  }

mkScene :: Level -> Scene
mkScene level = Scene
  { scPastCells = cells Past
  , scPresentCells = cells Present
  , scPastWhole = whole Past
  , scPresentWhole = whole Present
  , scPastBack = back Past
  , scPresentBack = back Present
  , scPastVig = bake (vignette Past)
  , scPresentVig = bake (vignette Present)
  , scPastCaption = bake (mapCaption Past)
  , scPresentCaption = bake (mapCaption Present)
  , scHud = mkHudArt
  }
  where
    ordered = [ (x, y) | y <- [0 .. lvHeight level - 1], x <- [0 .. lvWidth level - 1] ]
    cells era = [ (c, bake (cellArt level era c)) | c <- ordered ]
    whole era = bake (Pictures [ at (cellPos c) (cellArt level era c) | c <- ordered ])
    back era = bake (Pictures [seaBackdrop era, foundation era])

cellsOf :: Scene -> Era -> [(Cell, Picture)]
cellsOf s Past = scPastCells s
cellsOf s Present = scPresentCells s

wholeOf, backOf, vigOf, captionOf :: Scene -> Era -> Picture
wholeOf s Past = scPastWhole s
wholeOf s Present = scPresentWhole s
backOf s Past = scPastBack s
backOf s Present = scPresentBack s
vigOf s Past = scPastVig s
vigOf s Present = scPresentVig s
captionOf s Past = scPastCaption s
captionOf s Present = scPresentCaption s

-- | The picture for the current window: scaled to fit, bars over the remainder.
render :: Scene -> Level -> Game -> Fx -> (Int, Int) -> Picture
render scene level g fx (winW, winH) = Pictures
  [ Scale k k (frame scene level g fx)
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

frame :: Scene -> Level -> Game -> Fx -> Picture
frame scene level g fx = case gPhase g of
  Menu -> titleScreen clock
  _ -> Pictures
    [ world scene level g fx
    , hud (scHud scene) g clock
    , overlays g fx
    , startFade
    ]
  where
    clock = fromIntegral (fxClock fx)
    startFade
      | fxLevelAge fx < 26 =
          Color (hexA 0x05080A (1 - smooth (fromIntegral (fxLevelAge fx) / 26))) (rectC canvasW canvasH)
      | otherwise = Blank

world :: Scene -> Level -> Game -> Fx -> Picture
world scene level g fx = Pictures
  [ backOf scene shownEra
  , stationCells scene g fx
  , lightPools Past pastK
  , lightPools Present presentK
  , cellOnCradle
  , winBeneath
  , carried
  , ambient Past clock pastK
  , ambient Present clock presentK
  , blockedOutline
  , actor
  , burst
  , winAbove
  , vigOf scene shownEra
  , waveFront
  , if gPhase g == Won then wonCaption else captionOf scene (gEra g)
  , hintLine
  , blockedNote
  ]
  where
    clock = fromIntegral (fxClock fx) :: Float
    era = gEra g
    wave = fxWave fx
    t = maybe 1 waveProgress wave
    -- How strongly each era's light is showing.
    pastK = case wave of
      Nothing -> if era == Past then 1 else 0
      Just _ -> if era == Past then smooth t else 1 - smooth t
    presentK = 1 - pastK
    -- The sea swings over halfway across, as most of the tiles do.
    shownEra = case wave of
      Just w | t < 0.45 -> waveFrom w
      _ -> era

    -- Player position: authoritative cell, plus a purely visual ease to the next.
    (fromX, fromY) = gCell g
    (visX, visY, stepPhase) = case gMotion g of
      Stepping (toX, toY) left ->
        let p = 1 - fromIntegral left / fromIntegral stepTicks
            e = smooth p
        in ( lerp e (fromIntegral fromX) (fromIntegral toX)
           , lerp e (fromIntegral fromY) (fromIntegral toY), p)
      _ -> (fromIntegral fromX, fromIntegral fromY, 0)
    (dx, dy) = dirDelta (gFacing g)
    bumpK = sin (fromIntegral (gBump g) / fromIntegral bumpTicks * pi)
    (px0, py0) = cellVec visX visY
    refuseShake = case gFeedback g of
      Just (Feedback (ShiftBlocked _) left)
        | left > blockedFeedbackTicks - 18 ->
            3 * sin (fromIntegral left * 1.9) * fromIntegral (left - (blockedFeedbackTicks - 18)) / 18
      _ -> 0
    playerAt = ( px0 + 4 * bumpK * fromIntegral dx + refuseShake
               , py0 - 4 * bumpK * fromIntegral dy )
    -- At the win the keeper steps aside, so the tower they relit can be seen.
    asideK = if gPhase g == Won then smooth (fromIntegral (fxEnd fx) / 26) else 0
    actor = at (fst playerAt + 42 * asideK, snd playerAt + 6 - 26 * asideK)
               (keeper (if asideK > 0.5 then DirLeft else gFacing g)
                       (if gPhase g == Won then (if asideK < 1 then asideK * 2 else 0) else stepPhase))

    carried
      | gHasCell g && gPhase g /= Won = at playerAt (carriedLight clock (fromIntegral (fxCarried fx)))
      | otherwise = Blank

    -- The cell lies only in the past, and only appears once the wave has flipped
    -- its own tile, so it is never seen floating over the wrong era.
    powerAt = lvPowerCell level
    powerShown
      | gHasCell g = False
      | otherwise = case wave of
          Nothing -> era == Past
          Just w -> eraOfCellDuring w era powerAt == Past
    cellOnCradle
      | powerShown = at (cellPos powerAt) (powerCell clock)
      | otherwise = Blank

    burst = case fxBurst fx of
      Just b -> at (cellPos (burstCell b)) (pickupBurst (burstAge b))
      Nothing -> Blank

    -- Refused shift: outline the cell that is solid over there.
    blockedOutline = case gFeedback g of
      Just (Feedback (ShiftBlocked c) left) ->
        let k = clamp01 (fromIntegral left / 30)
            pulse = 0.65 + 0.35 * sin (fromIntegral left * 0.35)
            s = half - 2
        in at (cellPos c) (Color (hexA 0xFF7A4A (k * pulse)) (Pictures
             [ segs 4 [(-s, -s), (s, -s), (s, s), (-s, s), (-s, -s)]
             , Pictures [ seg 1.6 (a, -s) (b, s) | (a, b) <- [(-26, -8), (-8, 10), (10, 28)] ]
             ]))
      _ -> Blank
    blockedNote = case gFeedback g of
      Just (Feedback (ShiftBlocked c) left) ->
        blockedPlate era c (clamp01 (fromIntegral left / 20))
      _ -> Blank

    hintLine = case gFeedback g of
      Just (Feedback CellCollected left) | gPhase g `elem` [Playing, Paused] ->
        hintBand era (clamp01 (fromIntegral left / 30)) pickupText
      _ -> case currentHint level g of
        Just h -> hintBand era 1 (hintText h)
        Nothing -> Blank

    waveFront = case wave of
      Just w -> at (cellPos (waveOrigin w)) (waveRing t)
      Nothing -> Blank

    -- Victory: warmth under the keeper and the beams above.
    goalAt = cellPos (lvGoal level)
    endAge = fromIntegral (fxEnd fx) :: Float
    won = gPhase g == Won
    winBeneath
      | won = Pictures
          [ warmthOver endAge goalAt
          , relitSconces endAge goalAt
          ]
      | otherwise = Blank
    winAbove
      | won = at goalAt (Pictures
          [ victoryRays clock (smooth (endAge / 90))
          , lanternGlow clock (smooth (endAge / 24))
          ])
      | otherwise = Blank

-- | Which era a cell is showing partway through a wave.
eraOfCellDuring :: Wave -> Era -> Cell -> Era
eraOfCellDuring w destination c
  | flipOf w c >= 0.5 = destination
  | otherwise = waveFrom w

-- | How far through its own flip a cell is, 0 to 1. Cells flip in order of their
-- distance from the player, so the new era spreads outward from them.
flipOf :: Wave -> Cell -> Float
flipOf w (x, y) = clamp01 ((waveProgress w - delay) / 0.3)
  where
    (ox, oy) = waveOrigin w
    d = sqrt (fromIntegral ((x - ox) * (x - ox) + (y - oy) * (y - oy))) :: Float
    delay = 0.7 * d / 13

-- | The cells: a single cached picture at rest; a flipping crowd during a wave.
stationCells :: Scene -> Game -> Fx -> Picture
stationCells scene g fx = case fxWave fx of
  Nothing -> wholeOf scene (gEra g)
  Just w -> Pictures (zipWith (flipped w) (cellsOf scene (waveFrom w)) (cellsOf scene (gEra g)))
  where
    flipped w (c, old) (_, new) =
      let p = flipOf w c
          squash = abs (1 - 2 * p)
          pic = if p < 0.5 then old else new
          shine = sin (p * pi)
      in at (cellPos c) (Pictures
           [ Color (hex 0x06090B) (rectC cs cs)
           , Scale squash 1 pic
           , if shine > 0.02 then Color (hexA 0xFFFFFF (0.3 * shine)) (rectC (cs * squash) cs) else Blank
           ])

overlays :: Game -> Fx -> Picture
overlays g fx = case gPhase g of
  Paused -> pausedScreen (gEra g) (smooth (fromIntegral (fxPaused fx) / 8))
  Won -> wonScreen (smooth ((end - 70) / 30)) (gRemaining g)
  Lost -> lostScreen (smooth (end / 24))
  _ -> Blank
  where end = fromIntegral (fxEnd fx) :: Float
