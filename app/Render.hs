-- | Composes one frame from the game, the effects and the prebuilt art.
module Render (Scene, mkScene, render) where

import Draw
import Figures
import Font (textC)
import Game.Fx
import Game.Logic
import Game.Solution (StateKey, deadStates, stateKey)
import Game.Types
import Graphics.Gloss
import Hud
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
  , scWonCaption   :: Picture
  , scHasNext      :: Bool
  , scDead         :: Set.Set StateKey  -- ^ worked out once, the first time it is needed
  , scHud          :: HudArt
  }

-- | @title@ names the level above the map; @hasNext@ says whether another follows.
mkScene :: String -> Bool -> Level -> Scene
mkScene title hasNext level = Scene
  { scPastCells = cells Past
  , scPresentCells = cells Present
  , scPastWhole = whole Past
  , scPresentWhole = whole Present
  , scPastBack = back Past
  , scPresentBack = back Present
  , scPastVig = bake (vignette Past)
  , scPresentVig = bake (vignette Present)
  , scPastCaption = bake (mapCaption title Past)
  , scPresentCaption = bake (mapCaption title Present)
  , scWonCaption = wonCaption title
  , scHasNext = hasNext
  , scDead = deadStates level
  , scHud = mkHudArt (lvRules level)
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
  Menu -> Pictures
    [titleScreen clock
    , if gShowInstructions g then instructionsPopup else Blank
    ]

  _ -> Pictures
    [ world scene level (if peeking g fx then peekView g else g) fx
    , hud (scHud scene) level g fx clock
    , overlays scene g fx
    , startFade
    ]
  where
    clock = fromIntegral (fxClock fx)

    startFade
      | fxLevelAge fx < 26 =
          Color (hexA 0x05080A (1 - smooth (fromIntegral (fxLevelAge fx) / 26))) (rectC canvasW canvasH)
      | otherwise = Blank

-- | Holding Tab while nothing is flipping. The rules never read this key.
peeking :: Game -> Fx -> Bool
peeking g fx = tiPeek (gPrev g) && gPhase g == Playing && fxWave fx == Nothing

-- | The same moment seen from the other era: only drawing ever sees this game.
peekView :: Game -> Game
peekView g = g { gEra = otherEra (gEra g), gFeedback = Nothing }

world :: Scene -> Level -> Game -> Fx -> Picture
world scene level g fx = Pictures
  [ backOf scene shownEra
  , stationCells scene g fx
  , lightPools level Past pastK
  , lightPools level Present presentK
  , cellOnCradle
  , filledPits
  , gates
  , heldPlates
  , beams
  , mirrors
  , crossingMarks
  , footprints
  , crates
  , winBeneath
  , carried
  , ambient Past clock pastK
  , ambient Present clock presentK
  , blockedOutline
  , echo
  , reflections
  , jumpFlash
  , actor
  , burst
  , costFloat
  , winAbove
  , vigOf scene shownEra
  , waveFront
  , if gPhase g == Won then scWonCaption scene else captionOf scene (gEra g)
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
    -- A jump through a mirror: the keeper shrinks to nothing where they stood and
    -- grows again where they land, rather than sliding through the walls between.
    jumpP = case gMotion g of
      Stepping (toX, toY) left | abs (toX - fromX) + abs (toY - fromY) > 1 ->
        Just ((toX, toY), 1 - fromIntegral left / fromIntegral stepTicks)
      _ -> Nothing :: Maybe (Cell, Float)
    jumpK = maybe 1 (\(_, p) -> max 0.04 (abs (1 - 2 * p))) jumpP
    (visX, visY, stepPhase) = case gMotion g of
      Stepping (toX, toY) _ | Just (_, p) <- jumpP ->
        if p < 0.5 then (fromIntegral fromX, fromIntegral fromY, 0)
                   else (fromIntegral toX, fromIntegral toY, 0)
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
      Just (Feedback kind left)
        | kind /= CellCollected && left > blockedFeedbackTicks - 18 ->
            3 * sin (fromIntegral left * 1.9) * fromIntegral (left - (blockedFeedbackTicks - 18)) / 18
      _ -> 0
    playerAt = ( px0 + 4 * bumpK * fromIntegral dx + refuseShake
               , py0 - 4 * bumpK * fromIntegral dy )
    -- At the win the keeper steps aside, so the tower they relit can be seen.
    asideK = if gPhase g == Won then smooth (fromIntegral (fxEnd fx) / 26) else 0
    actor = at (fst playerAt + 42 * asideK, snd playerAt + 6 - 26 * asideK)
               (Scale jumpK jumpK (keeper (if asideK > 0.5 then DirLeft else gFacing g)
                       (if gPhase g == Won then (if asideK < 1 then asideK * 2 else 0) else stepPhase)))

    -- Mirrors stand in their own era, and their light only shows there.
    mirrors = Pictures
      [ at (cellPos c) (mirrorArt e face clock)
      | (c, (e, face)) <- Map.toList (lvMirrors level), eraOf c == e ]
    beams = Pictures
      [ beamLight e clock (edge c face) (if null cells then edge c face else edge (last cells) face)
      | wave == Nothing, gPhase g /= Menu
      , (c, (e, face)) <- Map.toList (lvMirrors level), e == era
      , let cells = beamCells level g c ]
    edge c face = let (x, y) = cellPos c; (ux, uy) = screenDir face in (x + half * ux, y + half * uy)

    -- Standing in a mirror's light: the reflection shows where its key would put
    -- the keeper, with an arrow on the key that does it, or a cross if it cannot.
    reflections
      | gPhase g /= Playing || gMotion g /= Settled || peeking g fx = Blank
      | otherwise = Pictures
          [ if jOpen j
              then Pictures [ at (cellPos (jTarget j)) (Pictures [landingMark clock, echoFigure clock 0.9])
                            , at playerAt (jumpArrow d clock) ]
              else at (cellPos (jTarget j)) blockedLanding
          | d <- [minBound .. maxBound], Just j <- [mirrorAhead level g d] ]
    jumpFlash = case jumpP of
      Just (to, p) -> Pictures
        [ at (cellPos (gCell g)) (Color (hexA 0xE6FAFF (0.7 * (1 - p))) (ring 28 (20 + 30 * p) 3 1))
        , at (cellPos to) (Color (hexA 0xE6FAFF (0.7 * p)) (ring 28 (50 - 30 * p) 3 1))
        ]
      Nothing -> Blank

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

    -- Each object swaps with its own tile as the wave passes, like the power cell.
    eraOf c = case wave of
      Nothing -> era
      Just w -> eraOfCellDuring w era c

    -- A crate being pushed moves with the keeper, from the cell they are stepping
    -- onto to the cell it was sent to. In a pit, it finishes the journey before the
    -- pit shows as filled.
    pushedCrate = case (gPushing g, gMotion g) of
      (Just c, Stepping to left) -> Just (c, to, smooth (1 - fromIntegral left / fromIntegral stepTicks))
      _ -> Nothing
    crates = Pictures $
      [ at (cellPos c) (crate (eraOf c))
      | c <- Set.toList (gCrates g), Just c /= fmap (\(moving, _, _) -> moving) pushedCrate ] ++
      [ at (cellVec (lerp e (fromIntegral fx0) (fromIntegral cx)) (lerp e (fromIntegral fy0) (fromIntegral cy)))
           (crate (eraOf (cx, cy)))
      | Just ((cx, cy), (fx0, fy0), e) <- [pushedCrate] ]
    -- Only under the keeper's feet: "you can switch era right here".
    crossingMarks = Pictures
      [ at (cellPos c) (crossingMark (eraOf c)) | c <- markedCells ]
    markedCells
      | gPhase g /= Playing || peeking g fx || gMotion g /= Settled = []
      | otherwise = filter (== gCell g) (crossings level g)
    gates = Pictures
      [ at (cellPos c) (doorway level c (doorsOpen level (weights g)))
      | c <- Set.toList (lvDoors level), eraOf c == Present ]
    -- A plate with anything on it glows, so it is clear what holds a door open.
    heldPlates = Pictures
      [ at (cellPos c) (heldPlateGlow clock)
      | gPhase g /= Menu, c <- Set.toList (lvPlates level), c `Set.member` weights g ]

    -- The footprints an echo would walk back along, fainter the older they are, and
    -- fainter still in the other era. The one it would stop on is ringed.
    echoing = ruEchoLength (lvRules level) > 0
    footprints
      | not echoing || gPhase g /= Playing || gMotion g /= Settled = Blank
      | otherwise = Pictures $
          [ at (cellPos c) (footprintMark (eraK e * (1 - 0.15 * fromIntegral i)))
          | (i, (e, c)) <- zip [0 :: Int ..] (drop 1 (gTrail g)) ] ++
          [ at (cellPos c) (Scale (0.6 + 0.4 * eraK e) (0.6 + 0.4 * eraK e) (landingMark clock))
          | Just (e, c) <- [echoLanding level g] ]
    eraK e = if e == era then 1 else 0.4

    -- The echo walks back along the footprints, then stays. Crossing a shift, it
    -- fades from one era into the other on the same cell.
    echo = case fxEchoWalk fx >>= echoWalkAt of
      _ | gPhase g == Menu -> Blank
      Just ((e0, (x0, y0)), (e1, (x1, y1)), p) ->
        let e = smooth p
            k = lerp e (eraK e0) (eraK e1)
        in at (cellVec (lerp e (fromIntegral x0) (fromIntegral x1)) (lerp e (fromIntegral y0) (fromIntegral y1)))
              (echoFigure clock k)
      Nothing -> case gEcho g of
        Just (e, c) -> at (cellPos c) (echoFigure clock (eraK e))
        Nothing -> Blank
    filledPits = Pictures
      [ at (cellPos c) (filledPit level c)
      | c <- Set.toList (gFilled g), eraOf c == Past, not (fillingNow c) ]
    -- Only the step that fills a pit hides its fill. A crate sliding over a pit
    -- filled earlier leaves it filled.
    fillingNow c = Just c == gPushing g && case gHistory g of
      (before : _) -> c `Set.notMember` snFilled before
      [] -> False

    burst = case fxBurst fx of
      Just b -> at (cellPos (burstCell b)) (pickupBurst (burstAge b))
      Nothing -> Blank

    -- Refused shift: outline the cell that is solid over there.
    blockedOutline = case gFeedback g of
      Just (Feedback kind left) | Just c <- refusedCell kind ->
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
      Just (Feedback ShiftTooCostly left) ->
        tooCostlyPlate (lvRules level) (gCell g) (clamp01 (fromIntegral left / 20))
      Just (Feedback (PushBlocked c) left) ->
        pushBlockedPlate c (clamp01 (fromIntegral left / 20))
      _ -> Blank

    -- What the last action just spent, or an undo gave back, rising off the keeper.
    costFloat = case fxCost fx of
      Just c | gPhase g /= Won ->
        let p = fromIntegral (cfAge c) / fromIntegral costLife :: Float
            (x, y) = cellPos (cfCell c)
        in at (x, y + half + 10 + 34 * smooth p) (Pictures
             [ Color (fade (1 - p * p) (hex 0x05080A)) (Translate 1.5 (-1.5) (textC 22 (costText (cfAmount c))))
             , Color (fade (1 - p * p) (costColor (cfAmount c))) (textC 22 (costText (cfAmount c)))
             ])
      _ -> Blank

    hintLine
      | peeking g fx = hintBand era 1 (peekText era)
      | stateKey g `Set.member` scDead scene = hintBand era 1 deadEndText
      | otherwise = case gFeedback g of
          Just (Feedback CellCollected left) | gPhase g `elem` [Playing, Paused] ->
            hintBand era (clamp01 (fromIntegral left / 30)) pickupText
          Just (Feedback PushNeedsPress left) ->
            hintBand era (clamp01 (fromIntegral left / 30)) pushAgainText
          Just (Feedback CrateRusted left) ->
            hintBand era (clamp01 (fromIntegral left / 30)) rustedText
          Just (Feedback EchoNeedsSteps left) ->
            hintBand era (clamp01 (fromIntegral left / 30)) echoNeedsStepsText
          Just (Feedback EchoInDoorway left) ->
            hintBand era (clamp01 (fromIntegral left / 30)) echoInDoorwayText
          Just (Feedback JumpNeedsPress left) ->
            hintBand era (clamp01 (fromIntegral left / 30)) jumpAgainText
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
          , if lvDressed level then relitSconces endAge goalAt else Blank
          ]
      | otherwise = Blank
    winAbove
      | won = at goalAt (Pictures
          [ victoryRays clock (smooth (endAge / 90))
          , lanternGlow clock (smooth (endAge / 24))
          ])
      | otherwise = Blank

-- | The cell a refusal is about, to outline it.
refusedCell :: FeedbackKind -> Maybe Cell
refusedCell kind = case kind of
  ShiftBlocked c -> Just c
  PushBlocked c  -> Just c
  _              -> Nothing

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

overlays :: Scene -> Game -> Fx -> Picture
overlays scene g fx = case gPhase g of
  Paused -> pausedScreen (gEra g) (smooth (fromIntegral (fxPaused fx) / 8))
  Won -> wonScreen (scHasNext scene) (smooth ((end - 70) / 30)) (gRemaining g)
  Lost -> lostScreen (smooth (end / 24))
  _ -> Blank
  where end = fromIntegral (fxEnd fx) :: Float
