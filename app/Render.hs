-- | Composes one frame from the game, the effects and the prebuilt art.
module Render (Scene, mkScene, render, renderWith, renderEnding, fitted) where

import Control.Monad (mfilter)
import Data.Maybe (isJust)
import Draw
import Figures
import Font (textC)
import Game.Fx
import Game.Logic
import Game.Run hiding (finale)
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
  { scPastCells    :: Map.Map Cell [(Cell, Picture)]  -- ^ by room, for the wave's flips
  , scPresentCells :: Map.Map Cell [(Cell, Picture)]
  , scPastWhole    :: Map.Map Cell Picture            -- ^ by room, one baked picture each
  , scPresentWhole :: Map.Map Cell Picture
  , scRooms        :: (Int, Int)                      -- ^ how many rooms across and down
  , scPastSea      :: Picture                         -- ^ canvas scale, under the board
  , scPresentSea   :: Picture
  , scSeaShade     :: Picture
  , scPastShore    :: Picture                         -- ^ shelf, rocks, surf: on the board
  , scPresentShore :: Picture
  , scPastSurf     :: Picture                         -- ^ the outer surf, breathing
  , scPresentSurf  :: Picture
  , scTitle        :: (String, String)
  , scPastBack     :: Picture                         -- ^ the stone apron, on the board
  , scPresentBack  :: Picture
  , scPastVig      :: Picture
  , scPresentVig   :: Picture
  , scPastCaption  :: Picture
  , scPresentCaption :: Picture
  , scWonCaption   :: Picture
  , scDarkCaption  :: Picture
  , scHasNext      :: Bool
  , scDead         :: Set.Set StateKey  -- ^ worked out once, the first time it is needed
  , scHud          :: HudArt
  , scEnding       :: EndingArt         -- ^ built the first time an ending is drawn
  }

-- | The art only an ending draws: the station as its passage opens, and the whole
-- way north from it.
data EndingArt = EndingArt
  { eaOpened :: [Picture]             -- ^ per passage cell, its neighbourhood once it is open
  , eaDash   :: Map.Map Cell Picture  -- ^ every cell of the rush, in the present
  , eaDashPast :: Map.Map Cell Picture  -- ^ and in the past
  , eaGoal   :: Picture               -- ^ the lighthouse cell again, over the opened wall
  }

-- | @title@ is the level's place in the hall and its name, shown above the map;
-- @hasNext@ says whether another follows; @teach@ writes the keys under the map.
mkScene :: (String, String) -> Bool -> Bool -> Level -> Scene
mkScene title hasNext teach level = Scene
  { scPastCells = cells Past
  , scPresentCells = cells Present
  , scPastWhole = whole Past
  , scPresentWhole = whole Present
  , scRooms = ( (lvWidth level + roomCols - 1) `div` roomCols
              , (lvHeight level + roomRows - 1) `div` roomRows )
  , scPastSea = bake (seaBackdrop Past)
  , scPresentSea = bake (seaBackdrop Present)
  , scSeaShade = bake seaShade
  , scPastShore = bake (shore Past)
  , scPresentShore = bake (shore Present)
  , scPastSurf = bake (farSurf Past)
  , scPresentSurf = bake (farSurf Present)
  , scTitle = title
  , scPastBack = bake (foundation Past)
  , scPresentBack = bake (foundation Present)
  , scPastVig = bake (vignette Past)
  , scPresentVig = bake (vignette Present)
  , scPastCaption = bake (mapCaption title Past)
  , scPresentCaption = bake (mapCaption title Present)
  , scWonCaption = wonCaption title
  , scDarkCaption = darkCaption title
  , scHasNext = hasNext
  -- Exhaustive dead-state search grows quickly with several pushable objects.
  -- Keep the level-select transition responsive in the larger rooms. A level of
  -- whole rooms keeps it: its one-way gates hold its search to the sum of its rooms.
  -- The search knows one keeper only, so a level for two is never told it is lost.
  , scDead = if duo || (Set.size (lvCrates level) >= 3 && not (roomedLevel level))
               then Set.empty else deadStates level
  , scHud = mkHudArt duo teach title level
  , scEnding = mkEndingArt level
  }
  where
    duo = isJust (lvSpawn2 level)
    ordered = [ (x, y) | y <- [0 .. lvHeight level - 1], x <- [0 .. lvWidth level - 1] ]
    -- Cells are kept by room, so a level of several screens only ever draws one.
    -- A level under the following camera is one room, drawn whole and masked.
    roomKey c = if roomedLevel level then roomOf c else (0, 0)
    byRoom f = Map.fromListWith (flip (++)) [ (roomKey c, [f c]) | c <- ordered ]
    cells era = byRoom (\c -> (c, bake (cellArt level era c)))
    whole era = Map.map (\pics -> bake (Pictures pics))
                        (byRoom (\c -> at (cellPos c) (cellArt level era c)))

mkEndingArt :: Level -> EndingArt
mkEndingArt level = EndingArt
  { eaOpened =
      [ bake (Pictures [ at (cellPos c) (cellArt (partlyRevealed k level) Present c) | c <- around p ])
      | (k, p) <- zip [1 ..] (lvPassage level) ]
  , eaDash = Map.fromList [ (c, bake (at (cellPos c) (cellArt dash Present c))) | c <- region ]
  , eaDashPast = Map.fromList [ (c, bake (at (cellPos c) (cellArt dash Past c))) | c <- region ]
  -- The tower stands taller than its cell, so the floor opened above it would cover
  -- its roof: it is drawn once more on top.
  , eaGoal = bake (at (cellPos (lvGoal level)) (cellArt (revealedLevel level) Present (lvGoal level)))
  }
  where
    -- Opening a wall changes the faces and shadows of the cells beside it too.
    -- Only cells of the map itself: the one beyond the edge belongs to the rush.
    around (x, y) = [ c | c@(cx, cy) <- [(x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                        , cx >= 0, cy >= 0, cx < lvWidth level, cy < lvHeight level ]
    dash = dashLevel level
    -- Everything the rush's camera can show: the floor of the way, and a screen of
    -- wall around it. Only the cells it does show are ever drawn.
    region
      | Set.null (lvPresent dash) || null (lvPassage level) = []
      | otherwise =
          let xs = Set.map fst (lvPresent dash)
              ys = Set.map snd (lvPresent dash)
          in [ (x, y) | y <- [Set.findMin ys - 10 .. Set.findMax ys + 10]
                      , x <- [Set.findMin xs - 14 .. Set.findMax xs + 14] ]

-- | One room's cells, and one room's baked picture.
cellsOf :: Scene -> Era -> Cell -> [(Cell, Picture)]
cellsOf s era room = Map.findWithDefault [] room (case era of
  Past -> scPastCells s
  Present -> scPresentCells s)

wholeOf :: Scene -> Era -> Cell -> Picture
wholeOf s era room = Map.findWithDefault Blank room (case era of
  Past -> scPastWhole s
  Present -> scPresentWhole s)

seaOf, shoreOf, surfOf, backOf, vigOf, captionOf :: Scene -> Era -> Picture
seaOf s Past = scPastSea s
seaOf s Present = scPresentSea s
shoreOf s Past = scPastShore s
shoreOf s Present = scPresentShore s
surfOf s Past = scPastSurf s
surfOf s Present = scPresentSurf s
backOf s Past = scPastBack s
backOf s Present = scPresentBack s
vigOf s Past = scPastVig s
vigOf s Present = scPresentVig s
captionOf s Past = scPastCaption s
captionOf s Present = scPresentCaption s

-- | The picture for the current window: scaled to fit, bars over the remainder.
render :: Scene -> Level -> Game -> Fx -> (Int, Int) -> Picture
render scene level g fx size = fitted size (frame scene level g fx)

-- | As 'render', with more drawn over the frame in canvas units: @ is given the
-- board's stand, 0 while the level is introduced and 1 once closed in.
renderWith :: (Float -> Picture) -> Scene -> Level -> Game -> Fx -> (Int, Int) -> Picture
renderWith over scene level g fx size =
  fitted size (Pictures [frame scene level g fx, over (focusOf Nothing fx)])

-- | A frame of a level's ending. @next@ is the number and name of the level that it
-- leads into, for the card at the end.
renderEnding :: Scene -> Level -> (String, String) -> Finale -> Game -> Fx -> (Int, Int) -> Picture
renderEnding scene level next f g fx size = fitted size (endingFrame scene level next f g fx)

fitted :: (Int, Int) -> Picture -> Picture
fitted (winW, winH) pic = Pictures
  [ Scale k k pic
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
    [ world Nothing scene level (if peeking g fx then peekView g else g) fx
    , hud (scHud scene) level g fx clock
    , board f (lessonPlate level g fx)
    , board f (overlays scene g fx)
    , startFade
    ]
  where
    clock = fromIntegral (fxClock fx)
    f = focusOf Nothing fx
    startFade
      | fxLevelAge fx < 26 =
          Color (hexA 0x05080A (1 - smooth (fromIntegral (fxLevelAge fx) / 26))) (rectC canvasW canvasH)
      | otherwise = Blank

-- | Each room states its new rule on entry, then gets out of the player's way.
lessonPlate :: Level -> Game -> Fx -> Picture
lessonPlate level g fx
  | null (lvLesson level) || gPhase g `notElem` [Playing, Paused] || age >= 420 = Blank
  | otherwise = Translate centreX (mapBottom + 35) (Pictures
      [ Color (hexA 0x05080A (0.88 * alpha)) (rectC 830 48)
      , Color (fade alpha (hex 0xE8B85C)) (rect (-415) 21 830 2)
      , Color (fade alpha (hex 0xF3E9D5)) (Translate 0 (-5) (textC 12 (lvLesson level)))
      ])
  where
    age = fxLevelAge fx
    alpha = if age < 330 then 1 else 1 - fromIntegral (age - 330) / 90
    centreX = (mapLeft + mapRight) / 2

-- | Holding Tab while nothing is flipping. The rules never read this key.
peeking :: Game -> Fx -> Bool
peeking g fx = tiPeek (gPrev g) && gPhase g == Playing && fxWave fx == Nothing

-- | The same moment seen from the other era: only drawing ever sees this game.
peekView :: Game -> Game
peekView g = g { gEra = otherEra (gEra g), gFeedback = Nothing }

-- | Said under the map once the passage is open.
beckonText :: Level -> String
beckonText level = "The light has gone on through the walls.   Step into it, and it carries you " ++ way ++ ".      R  back to the lamp"
  where
    way = case fmap frDir (finaleRun level) of
      Just DirUp -> "north"
      Just DirDown -> "south"
      Just DirRight -> "east"
      _ -> "west"

-- | Whether the lamp burns in this frame of an ending.
lampBurning :: Finale -> Bool
lampBurning f = fnStage f == Lit || (fnStage f == Flare && fnAge f < lampDiesAt)

-- | How many cells of the passage are open in this frame of an ending.
openedCells :: Level -> Finale -> Int
openedCells level f = case fnStage f of
  Opening -> length [ k | k <- [0 .. length (lvPassage level) - 1], fnAge f >= cellOpenTick level k ]
  s | s > Opening -> length (lvPassage level)
  _ -> 0

world :: Maybe Finale -> Scene -> Level -> Game -> Fx -> Picture
world finale scene level g fx = Pictures
  [ seaOf scene era
  , seaLife era (fromIntegral (fxClock fx))
  , scSeaShade scene
  , board (focusOf finale fx) (onTheBoard finale scene level g fx)
  ]
  where era = seaEra g fx

-- | 0 while a level is introduced, 1 once the screen has closed in on the board,
-- eased. An ending is always played close in.
focusOf :: Maybe Finale -> Fx -> Float
focusOf finale fx
  | isJust finale = 1
  | otherwise = smooth (focusProgress fx)

-- | The sea swings over halfway across, as most of the tiles do.
seaEra :: Game -> Fx -> Era
seaEra g fx = case fxWave fx of
  Just w | waveProgress w < 0.45 -> waveFrom w
  _ -> gEra g

-- | Everything 'world' draws on the board, in the board's own coordinates.
onTheBoard :: Maybe Finale -> Scene -> Level -> Game -> Fx -> Picture
onTheBoard finale scene level g fx = Pictures
  [ surfBreathing
  , shoreOf scene shownEra
  , backOf scene shownEra
  , inRoom (Pictures
      [ stationCells scene shownRoom g fx
      , openedArt
      , lightPools level Past pastK
      , lightPools level Present presentK
      , cellOnCradle
      , filledPits
      , gates
      , heldPlates
      , beams
      , mirrors
      , crossingMarks
      , exits
      , footprints
      , crates
      , winBeneath
      , passageLight
      , carried
      ])
  , ambient Past clock pastK
  , ambient Present clock presentK
  , inRoom (Pictures
      [ blockedOutline
      , echo
      , reflections
      , pushes
      , jumpFlash
      , actors
      , burst
      , costFloat
      , winAbove
      , passageFx
      ])
  , inRoom guide
  , if roomed then roomDark else inRoom waveFront
  , if roomed then Blank else viewportMask shownEra
  , vigOf scene shownEra
  , if roomed then inRoom waveFront else Blank
  , dimmed
  , caption
  , hintLine
  , blockedNote
  ]
  where
    clock = fromIntegral (fxClock fx) :: Float

    -- The screen shows one room of a level larger than the map. Everything placed by
    -- cell is moved so that room fills the map, and things in other rooms are left
    -- out, so they never spill over the frame or the panel.
    -- A level of any other size slides under the map instead, keeping the keeper
    -- near the centre, and a mask hides whatever lies outside the map.
    roomed = roomedLevel level
    shownRoom@(roomX, roomY) = if roomed then fxRoom fx else (0, 0)
    inRoom
      | roomed = Translate (negate (fromIntegral (roomX * roomCols)) * cs) (fromIntegral (roomY * roomRows) * cs)
      | otherwise = Translate cameraDX cameraDY
    visible c = not roomed || roomOf c == shownRoom
    -- The cell as the map shows it, for anything pinned to the map's own edges.
    onScreen (x, y)
      | roomed = (x - roomX * roomCols, y - roomY * roomRows)
      | otherwise = (x - round cameraX, y - round cameraY)
    -- Moving between rooms: the map darkens, swaps, and lightens.
    roomDark = case roomFadeDark fx of
      k | k > 0.01 -> Color (hexA 0x05080A (0.96 * k)) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom))
      _ -> Blank
    era = gEra g
    wave = fxWave fx
    t = maybe 1 waveProgress wave
    -- How strongly each era's light is showing.
    pastK = case wave of
      Nothing -> if era == Past then 1 else 0
      Just _ -> if era == Past then smooth t else 1 - smooth t
    presentK = 1 - pastK
    -- The sea swings over halfway across, as most of the tiles do.
    shownEra = seaEra g fx

    -- Two keepers in a level for two: the first in yellow, the second in red. The
    -- rules speak of "the player", so the second is drawn from the swapped game.
    keepers = (g, yellowCoat) : [ (swapKeepers g, redCoat) | isJust (gPartner g) ]

    -- Where a keeper is drawn: their cell, plus a purely visual ease to the next.
    -- A jump through a mirror: the keeper shrinks to nothing where they stood and
    -- grows again where they land, rather than sliding through the walls between.
    jumpOf gg = case gMotion gg of
      Stepping (toX, toY) left | abs (toX - fst (gCell gg)) + abs (toY - snd (gCell gg)) > 1 ->
        Just ((toX, toY), 1 - fromIntegral left / fromIntegral stepTicks)
      _ -> Nothing :: Maybe (Cell, Float)
    walkOf gg = case gMotion gg of
      Stepping (toX, toY) _ | Just (_, p) <- jumpOf gg ->
        if p < 0.5 then (fromIntegral (fst (gCell gg)), fromIntegral (snd (gCell gg)), 0)
                   else (fromIntegral toX, fromIntegral toY, 0)
      Stepping (toX, toY) left ->
        let p = 1 - fromIntegral left / fromIntegral stepTicks
            e = smooth p
        in ( lerp e (fromIntegral (fst (gCell gg))) (fromIntegral toX)
           , lerp e (fromIntegral (snd (gCell gg))) (fromIntegral toY), p)
      _ -> (fromIntegral (fst (gCell gg)), fromIntegral (snd (gCell gg)), 0 :: Float)
    placedAt gg shake =
      let (vx, vy, _) = walkOf gg
          (px0, py0) = cellVec vx vy
          (dx, dy) = dirDelta (gFacing gg)
          bumpK = sin (fromIntegral (gBump gg) / fromIntegral bumpTicks * pi)
      in (px0 + 4 * bumpK * fromIntegral dx + shake, py0 - 4 * bumpK * fromIntegral dy)
    (visX, visY, _) = walkOf g
    -- The original 13 x 9 board is now a viewport. Small levels remain fixed;
    -- larger ones slide smoothly under it while the keeper stays near the centre.
    viewCols = 13 :: Float
    viewRows = 9 :: Float
    cameraX = max 0 (min (fromIntegral (lvWidth level) - viewCols) (visX - 6))
    cameraY = max 0 (min (fromIntegral (lvHeight level) - viewRows) (visY - 4))
    cameraDX = negate (cameraX * cs)
    cameraDY = cameraY * cs
    refuseShake = case gFeedback g of
      Just (Feedback kind left)
        | refusal kind && left > blockedFeedbackTicks - 18 ->
            3 * sin (fromIntegral left * 1.9) * fromIntegral (left - (blockedFeedbackTicks - 18)) / 18
      _ -> 0
    refusal kind = case kind of
      ShiftBlocked _ -> True
      ShiftTooCostly -> True
      PushBlocked _  -> True
      _              -> False
    -- The refusal shakes the keeper it is about: in a level for two, whichever
    -- keeper is nearer the refused cell.
    partnerRefused = case (gPartner g, gFeedback g) of
      (Just p, Just (Feedback kind _)) | Just (rx, ry) <- refusedCell kind ->
        let apart (x, y) = abs (x - rx) + abs (y - ry) in apart (ptCell p) < apart (gCell g)
      _ -> False
    playerAt = placedAt g (if partnerRefused then 0 else refuseShake)

    -- The lamp is lit: by a win, or in the first part of an ending.
    lampLit = maybe (gPhase g == Won) lampBurning finale
    -- At the win the keeper steps aside, so the tower they relit can be seen. In an
    -- ending they stay there while the lamp goes out and the wall opens, then step
    -- back to face the passage.
    asideK = case finale of
      Nothing -> if gPhase g == Won then smooth (fromIntegral (fxEnd fx) / 26) else 0
      Just f -> case fnStage f of
        Lit -> smooth (fromIntegral (fnAge f) / 26)
        Flare -> 1
        Opening | fnAge f < openTicks level - 30 -> 1
                | otherwise -> 1 - smooth (fromIntegral (fnAge f - (openTicks level - 30)) / 26)
        _ -> 0
    -- In an ending the keeper watches the lamp, then turns to the light leaving it.
    endFacing = case finale of
      Just f | fnStage f == Lit || (fnStage f == Flare && fnAge f < lampDiesAt + 10) -> Just DirLeft
             | fnStage f == Flare || (fnStage f == Opening && lightReach f < 1) -> Just towardsMouth
             | fnStage f == Opening -> Just wayDir
      _ -> Nothing
    towardsMouth = case passage of
      (mx, my) : _ | my < snd (lvGoal level) -> DirUp
                   | my > snd (lvGoal level) -> DirDown
                   | mx < fst (lvGoal level) -> DirLeft
      _ -> DirRight
    actors = Pictures (map actor keepers)
    actor (gg, coat) =
      let (_, _, phase) = walkOf gg
          jumpK = maybe 1 (\(_, p) -> max 0.04 (abs (1 - 2 * p))) (jumpOf gg)
          (ax, ay) = if gCell gg == gCell g then playerAt else placedAt gg (if partnerRefused then refuseShake else 0)
          aside = if gCell gg == lvGoal level then asideK else 0
          facing = case endFacing of
            Just d -> d
            Nothing -> if aside > 0.5 then DirLeft else gFacing gg
          stride
            | aside > 0 && aside < 1 = aside * 2
            | lampLit || isJust endFacing = 0
            | otherwise = phase
      in at (ax + 42 * aside, ay + 6 - 26 * aside) (Scale jumpK jumpK (keeperIn coat facing stride))

    -- Mirrors stand in their own era, and their light only shows there.
    -- A mirror being pushed slides with the keeper, like a crate, and its light
    -- shows again once it has landed.
    pushedMirrors =
      [ (c, cellVec (lerp e (fromIntegral fx0) (fromIntegral (fst c)))
                    (lerp e (fromIntegral fy0) (fromIntegral (snd c))))
      | (gg, _) <- keepers
      , (Just c, Stepping (fx0, fy0) left) <- [(gPushing gg, gMotion gg)]
      , Map.member c (gMirrors g)
      , let e = smooth (1 - fromIntegral left / fromIntegral stepTicks) ]
    mirrors = Pictures
      [ at (maybe (cellPos c) snd (mfilter ((== c) . fst) (lookupPair c pushedMirrors)))
           (case mBend m of
              Just b -> bendMirrorArt (mEra m) (mFace m) b clock
              Nothing -> mirrorArt (mMovable m) (mEra m) (mFace m) clock)
      | (c, m) <- Map.toList (gMirrors g), visible c, eraOf c == mEra m ]
    lookupPair c ps = case [ p | p@(c', _) <- ps, c' == c ] of
      (p : _) -> Just p
      [] -> Nothing
    beams = Pictures
      [ beamLight (mEra m) clock (edge c face)
                  (if null cells then edge c face else edge (last cells) face)
      | wave == Nothing, gPhase g /= Menu
      , (c, m) <- Map.toList (gMirrors g), visible c, mEra m == era
      , c `notElem` map fst pushedMirrors
      , (face, lit) <- beamsOf level g c
      , let cells = takeWhile visible lit ]
    edge c face = let (x, y) = cellPos c; (ux, uy) = screenDir face in (x + half * ux, y + half * uy)

    -- Standing in a mirror's light: the reflection shows where its key would put
    -- the keeper, with an arrow on the key that does it, or a cross if it cannot.
    reflections
      | gPhase g /= Playing || peeking g fx = Blank
      | otherwise = Pictures
          [ if jOpen j
              then Pictures [ at (cellPos (jTarget j)) (Pictures [landingMark clock, echoFigure clock 0.9])
                            , at (placedAt gg 0) (jumpArrow d clock) ]
              else at (cellPos (jTarget j)) blockedLanding
          | (gg, _) <- keepers, gMotion gg == Settled
          , d <- [minBound .. maxBound], Just j <- [mirrorAhead level gg d], visible (jTarget j) ]
    -- A level shown a room at a time: each doorway on to another room glows with the
    -- light of the room beyond, chevrons running out through it, so a way on never
    -- passes for a dead end. Strongest towards a room not yet seen; faint where the
    -- way is open only in the other era. Worked out from the board, so a crate in a
    -- doorway puts its light out until it is moved.
    exits
      | not roomed || scRooms scene == (1, 1) || gPhase g /= Playing || isJust finale = Blank
      | otherwise = Pictures
          [ at (cellPos c) (doorwayHint era d (strength * fadeIn) (clock + fromIntegral (fst c * 17)))
          | c <- [ (x, y) | y <- [roomY * roomRows .. roomY * roomRows + roomRows - 1]
                          , x <- [roomX * roomCols .. roomX * roomCols + roomCols - 1] ]
          , d <- [minBound .. maxBound]
          , let n = stepCell d c
          , roomOf n /= shownRoom
          , let openIn e = openCell level g e c && openCell level g e n
          , openIn Past || openIn Present
          , let unseen = roomOf n `notElem` fxVisited fx
                strength = (if unseen then 1 else 0.35) * (if openIn era then 1 else 0.4) ]
    fadeIn = 1 - roomFadeDark fx

    -- Beside a movable mirror's back or side: where a push would slide it, worked
    -- out by the push rule itself, so the hint cannot promise what a key will not do.
    pushes
      | gPhase g /= Playing || peeking g fx = Blank
      | otherwise = Pictures
          [ at (cellPos to) (pushHint d clock)
          | (gg, _) <- keepers, gMotion gg == Settled
          , d <- [minBound .. maxBound]
          , Just m <- [mirrorAt gg (gEra gg) (stepCell d (gCell gg))], mMovable m
          , let pushed = tryStep level True d gg
          , gMirrors pushed /= gMirrors gg
          , Just to <- [gPushing pushed], visible to ]
    jumpFlash = Pictures
      [ Pictures
          [ at (cellPos (gCell gg)) (Color (hexA 0xE6FAFF (0.7 * (1 - p))) (ring 28 (20 + 30 * p) 3 1))
          , at (cellPos to) (Color (hexA 0xE6FAFF (0.7 * p)) (ring 28 (50 - 30 * p) 3 1))
          ]
      | (gg, _) <- keepers, Just (to, p) <- [jumpOf gg] ]

    carried = case carrierCell g of
      Just c | gPhase g /= Won ->
        let gg = if c == gCell g then g else swapKeepers g
        in at (if c == gCell g then playerAt else placedAt gg 0) (carriedLight clock (fromIntegral (fxCarried fx)))
      _ -> Blank

    -- The cell lies only in the past, and only appears once the wave has flipped
    -- its own tile, so it is never seen floating over the wrong era.
    powerAt = lvPowerCell level
    powerShown
      | anyHasCell g || isJust finale = False
      | otherwise = case wave of
          Nothing -> era == Past
          Just w -> eraOfCellDuring w era powerAt == Past
    cellOnCradle
      | powerShown && visible powerAt = at (cellPos powerAt) (powerCell clock)
      | otherwise = Blank

    -- Each object swaps with its own tile as the wave passes, like the power cell.
    eraOf c = case wave of
      Nothing -> era
      Just w -> eraOfCellDuring w era c

    -- A crate being pushed moves with the keeper, from the cell they are stepping
    -- onto to the cell it was sent to. In a pit, it finishes the journey before the
    -- pit shows as filled.
    pushedCrates =
      [ (c, to, smooth (1 - fromIntegral left / fromIntegral stepTicks))
      | (gg, _) <- keepers
      , (Just c, Stepping to left) <- [(gPushing gg, gMotion gg)]
      , not (Map.member c (gMirrors g)) ]
    crates = Pictures $
      [ at (cellPos c) (crate (eraOf c))
      | c <- Set.toList (gCrates g), visible c, not (crateBroken c)
      , c `notElem` [ moving | (moving, _, _) <- pushedCrates ] ] ++
      [ at (cellVec (lerp e (fromIntegral fx0) (fromIntegral cx)) (lerp e (fromIntegral fy0) (fromIntegral cy)))
           (crate (eraOf (cx, cy)))
      | ((cx, cy), (fx0, fy0), e) <- pushedCrates ]
    -- Only under the keepers' feet: "you can switch era right here".
    -- In an ending the era cannot change, so there is nothing to mark.
    crossingMarks = Pictures
      [ at (cellPos c) (crossingMark (eraOf c))
      | gPhase g == Playing, not (peeking g fx), finale == Nothing
      , (gg, _) <- keepers, gMotion gg == Settled
      , let c = gCell gg, c `elem` crossings level gg ]
    gates = Pictures
      [ at (cellPos c) (doorway level c (doorsOpen level (weights g)))
      | c <- Set.toList (lvDoors level), visible c, eraOf c == Present ]
    -- A plate with anything on it glows, so it is clear what holds a door open.
    heldPlates = Pictures
      [ at (cellPos c) (heldPlateGlow clock)
      | gPhase g /= Menu, c <- Set.toList (lvPlates level), visible c, c `Set.member` weights g ]

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
      | c <- Set.toList (gFilled g), visible c, eraOf c == Past, not (fillingNow c) ]
    -- Only the step that fills a pit hides its fill. A crate sliding over a pit
    -- filled earlier leaves it filled.
    fillingNow c = Just c `elem` [ gPushing gg | (gg, _) <- keepers ] && case gHistory g of
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
    -- The plates keep themselves inside the map, so they take the cell as shown.
    blockedNote = case gFeedback g of
      Just (Feedback (ShiftBlocked c) left) | visible c ->
        blockedPlate era (onScreen c) (clamp01 (fromIntegral left / 20))
      Just (Feedback ShiftTooCostly left) ->
        tooCostlyPlate (lvRules level) (onScreen (gCell g)) (clamp01 (fromIntegral left / 20))
      Just (Feedback (PushBlocked c) left) | visible c ->
        pushBlockedPlate (onScreen c) (clamp01 (fromIntegral left / 20))
      _ -> Blank

    -- What the last action just spent, or an undo gave back, rising off the keeper.
    costFloat = case fxCost fx of
      Just c | gPhase g /= Won, finale == Nothing ->
        let p = fromIntegral (cfAge c) / fromIntegral costLife :: Float
            (x, y) = cellPos (cfCell c)
        in at (x, y + half + 10 + 34 * smooth p) (Pictures
             [ Color (fade (1 - p * p) (hex 0x05080A)) (Translate 1.5 (-1.5) (textC 22 (costText (cfAmount c))))
             , Color (fade (1 - p * p) (costColor (cfAmount c))) (textC 22 (costText (cfAmount c)))
             ])
      _ -> Blank

    -- While the level is introduced its name stands in the column beside the
    -- board; the line above the board comes in as the screen closes in.
    caption
      | lampLit = scWonCaption scene
      | isJust finale = scDarkCaption scene
      | focus < 1 = mapCaptionFaded focus (scTitle scene) (gEra g)
      | otherwise = captionOf scene (gEra g)
    focus = focusOf finale fx

    -- The outer surf breathes in and out around the apron.
    surfBreathing =
      let s = 1 + 0.0035 * sin (clock * 0.035)
          cy = (mapTop + mapBottom) / 2
          cx = (mapLeft + mapRight) / 2
      in Translate cx cy (Scale s s (Translate (negate cx) (negate cy) (surfOf scene shownEra)))

    hintLine
      | Just f <- finale = case fnStage f of
          Beckon | gPhase g == Playing -> hintBand era (clamp01 (fromIntegral (fnAge f) / 40)) (beckonText level)
          _ -> Blank
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
          Just (Feedback MirrorFaceSide left) ->
            hintBand era (clamp01 (fromIntegral left / 30)) mirrorFaceText
          Just (Feedback MirrorFixed left) ->
            hintBand era (clamp01 (fromIntegral left / 30)) mirrorFixedText
          _ -> case currentHint level g of
            Just h -> hintBand era 1 (hintText h)
            Nothing -> Blank

    waveFront = case wave of
      Just w -> at (cellPos (waveOrigin w)) (waveRing t)
      Nothing -> Blank

    -- Victory: warmth under the keeper and the beams above. In an ending the lamp
    -- burns on its own clock, flares, and goes out.
    goalAt = cellPos (lvGoal level)
    lanternAt = let (gx, gy) = goalAt; (ox, oy) = lanternOffset in (gx + ox, gy + oy)
    endAge = case finale of
      Just f | fnStage f == Lit -> fromIntegral (fnAge f)
             | fnStage f == Flare -> fromIntegral (litTicks + fnAge f)
      _ -> fromIntegral (fxEnd fx) :: Float
    flareK = case finale of
      Just f | fnStage f == Flare && fnAge f < lampDiesAt -> smooth (fromIntegral (fnAge f) / fromIntegral lampDiesAt)
      _ -> 0
    winBeneath
      | lampLit && visible (lvGoal level) = Pictures
          [ warmthOver endAge goalAt
          , if lvDressed level then relitSconces endAge goalAt else Blank
          ]
      | otherwise = Blank
    winAbove
      | lampLit && visible (lvGoal level) = at goalAt (Pictures
          [ victoryRays clock (smooth (endAge / 90) + flareK)
          , if flareK > 0 then victoryRays (clock * 3) flareK else Blank
          , lanternGlow clock (smooth (endAge / 24))
          , if flareK > 0 then at lanternOffset (pool 0xFFF6DD (0.9 * flareK) (60 + 260 * flareK) (60 + 260 * flareK)) else Blank
          ])
      | otherwise = Blank

    -- The ending's own marks on the station, drawn in the present only: in the past
    -- the walls still stand.
    endingOn = maybe False (const ((roomOf (lvGoal level) == shownRoom || not roomed) && shownEra == Present)) finale
    passage = lvPassage level
    way = wayCells level
    wayDir = maybe DirLeft frDir (finaleRun level)
    -- The cells of the passage drawn open, each once it has broken.
    openedArt = case finale of
      Just f | endingOn, openedCells level f > 0 -> Pictures
        (take (openedCells level f) (eaOpened (scEnding scene)) ++ [eaGoal (scEnding scene)])
      _ -> Blank
    -- A crate left on the way breaks as the light reaches it, like the walls.
    crateBroken c = case finale of
      Just f -> case lookup c (zip way [0 :: Int ..]) of
        Just i -> lightReach f >= fromIntegral i
        Nothing -> False
      Nothing -> False
    -- The map darkens when the lamp dies, and stays a little darker than it was.
    dimmed = case finale of
      Just f | not (lampBurning f) ->
        let since = case fnStage f of
              Flare -> fromIntegral (fnAge f - lampDiesAt)
              _ -> 999 :: Float
            k = 0.13 + 0.55 * (1 - smooth (since / 50))
        in Color (hexA 0x05080A k) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom))
      _ -> Blank
    -- How far the light has run along the way, in cells from the mouth: waiting in
    -- the wall beside the tower until it sets off, then one cell every few ticks.
    lightReach f = case fnStage f of
      Opening -> fromIntegral (fnAge f - lightSetsOff) / fromIntegral lightCellTicks :: Float
      s | s > Opening -> 999
      _ -> -1
    -- Behind the light the way glows, brighter towards where it went.
    passageLight = case finale of
      Just f | endingOn && not (null way) ->
        let reach = lightReach f
            n = fromIntegral (length way) :: Float
            (band, core)
              | wayDir `elem` [DirLeft, DirRight] = (rectC cs 46, rectC cs 20)
              | otherwise = (rectC 46 cs, rectC 20 cs)
        in Pictures
             [ at (cellPos c) (Pictures
                 [ Color (hexA 0xFFE2A8 (0.2 * k * (0.45 + 0.55 * i / n))) band
                 , Color (hexA 0xFFF1C2 (0.13 * k * (0.45 + 0.55 * i / n))) core ])
             | (j, c) <- zip [0 :: Int ..] way
             , let i = fromIntegral j
                   k = smooth ((reach - i) / 3)
             , k > 0.01 ]
      _ -> Blank
    -- Once the way is open, the rest of the room sinks into shadow: only the way,
    -- and a little light around the keeper, stay lit. Arrows over the gap beside the
    -- tower say where to step.
    guide = case (finale, way) of
      (Just f, mouth : _) | endingOn, fnStage f >= Opening, fnStage f < Rush ->
        let k = case fnStage f of
              Opening -> smooth (lightReach f - fromIntegral (length way))
              _ -> 1
            dist (ax, ay) (bx', by') = sqrt ((ax - bx') * (ax - bx') + (ay - by') * (ay - by'))
            here = (visX, visY)
            cellF (x, y) = (fromIntegral x, fromIntegral y) :: (Float, Float)
            shadeOf c =
              let dWay = minimum [ dist (cellF c) (cellF w) | w <- way ]
                  dKeeper = dist (cellF c) here
              in min (clamp01 ((dWay - 0.6) / 1.6)) (clamp01 ((dKeeper - 1.1) / 1.8))
            roomCells = [ (x, y) | y <- [roomY * roomRows .. roomY * roomRows + roomRows - 1]
                                 , x <- [roomX * roomCols .. roomX * roomCols + roomCols - 1] ]
            (ux, uy) = screenDir towardsMouth
            onWay = gCell g `elem` way
            chevron i =
              let p = wrap01 (clock * 0.02 + fromIntegral i / 3)
                  (cx, cy) = cellPos mouth
                  ox = cx + ux * (p * 40 - 22)
                  oy = cy + uy * (p * 40 - 22)
                  a = 0.85 * sin (pi * p) * k
                  (px, py) = (-uy, ux)
              in Color (hexA 0xFFE9B0 a) (Pictures
                   [ plainSeg 4 (ox - px * 13 - ux * 9, oy - py * 13 - uy * 9) (ox, oy)
                   , plainSeg 4 (ox, oy) (ox + px * 13 - ux * 9, oy + py * 13 - uy * 9) ])
        in Pictures $
             [ at (cellPos c) (Color (hexA 0x05080A (0.66 * k * s)) (rectC (cs + 1) (cs + 1)))
             | c <- roomCells, let s = shadeOf c, s > 0.02 ] ++
             [ chevron i | not onWay, fnStage f == Beckon, i <- [0 .. 2 :: Int] ]
      _ -> Blank
    passageFx = case finale of
      Just f | endingOn -> Pictures [smoke f, leaving f, runner f, cracks f, motes f]
      _ -> Blank
    -- A thread of smoke from the dead lamp.
    smoke f = case fnStage f of
      Flare | fnAge f >= lampDiesAt -> wisps (fromIntegral (fnAge f - lampDiesAt))
      Opening -> wisps (fromIntegral (flareTicks - lampDiesAt + fnAge f))
      _ -> Blank
    wisps a = Pictures
      [ at (fst lanternAt + 8 * sin (r * 0.09 + fromIntegral i), snd lanternAt + 8 + r * 0.7)
           (Color (hexA 0xB7C4C9 (0.32 * (1 - r / 90) * clamp01 (1 - a / 180))) (ngon 10 (4 + r * 0.12)))
      | i <- [0 .. 5 :: Int]
      , let r = a + fromIntegral i * 15 - 90 * fromIntegral (floor ((a + fromIntegral i * 15) / 90) :: Int)
      , a > fromIntegral i * 15 - 15 ]
    lightOrb k = Pictures [pool 0xFFE9B0 (0.6 * k) 46 46, Color (fade k (hex 0xFFFFFF)) (ngon 12 5)]
    -- The light itself leaves the lamp and slips into the wall beside the tower,
    -- where it glows through the stone until it sets off.
    leaving f = case (fnStage f, way) of
      (Flare, mouth : _) | fnAge f >= lampDiesAt + 4 ->
        let p = smooth (fromIntegral (fnAge f - lampDiesAt - 4) / 26)
            (tx, ty) = cellPos mouth
            here = (lerp p (fst lanternAt) tx, lerp p (snd lanternAt) ty)
        in Pictures
             [ Color (hexA 0xFFE2A8 (0.35 * (1 - p))) (plainSeg 7 lanternAt here)
             , at here (lightOrb 1) ]
      _ -> Blank
    -- Then it runs along the way, breaking every wall it meets, and out of the map.
    runner f = case (fnStage f, way) of
      (Opening, (mx, my) : _) ->
        let reach = lightReach f
            n = fromIntegral (length way - 1) :: Float
            along = max 0 (min (n + 0.6) reach)
            (dx, dy) = dirDelta wayDir
            k = 1 - smooth ((reach - n) / 1.2)
        in if k <= 0.01 then Blank
           else at (cellVec (fromIntegral mx + fromIntegral dx * along) (fromIntegral my + fromIntegral dy * along))
                   (lightOrb k)
      _ -> Blank
    -- Each wall cracks with light behind it, then breaks in a puff of dust.
    cracks f = case fnStage f of
      Opening -> Pictures $
        [ at (cellPos c) (Pictures [crackLines (clamp01 ((a + 18) / 18)), breakPuff a])
        | (k, c) <- zip [0 ..] passage
        , let a = fromIntegral (fnAge f - cellOpenTick level k) :: Float
        , a > -18, a < 30 ] ++
        [ at (cellPos c) (breakPuff a)
        | (i, c) <- zip [0 :: Int ..] way, c `Set.member` gCrates g
        , let a = fromIntegral (fnAge f - lightSetsOff - lightCellTicks * i) :: Float
        , a >= 0, a < 30 ]
      _ -> Blank
    crackLines k
      | k >= 1 = Blank
      | otherwise = Color (hexA 0xFFE2A8 (0.9 * k)) (Pictures
          [ plainSeg 2.4 (0, 0) (lerp k 0 x, lerp k 0 y) | (x, y) <- [(-26, 24), (22, 30), (30, -10), (-18, -28), (6, -34), (-34, 2)] ])
    breakPuff a
      | a < 0 = Blank
      | otherwise = Pictures $
          [ Color (hexA 0xFFF6DD (0.85 * (1 - a / 14))) (rectC cs cs) | a < 14 ] ++
          [ Translate (cos ang * r) (sin ang * r * 0.8) (Color (hexA 0x9AA6AA (0.5 * (1 - a / 30))) (ngon 9 (6 + a * 0.5)))
          | i <- [0 .. 7 :: Int]
          , let ang = fromIntegral i * pi / 4 + 0.3
                r = 10 + a * 1.6 ]
    -- Specks of light drifting along the open way, the way to go.
    motes f
      | null way || lightReach f < fromIntegral (length way) = Blank
      | otherwise = Pictures
          [ at (cellVec (fromIntegral mx + fdx * along + fdy * wobble) (fromIntegral my + fdy * along + fdx * wobble))
               (Color (hexA 0xFFE9B0 (0.6 * sin (pi * u))) (rectC 3 3))
          | i <- [0 .. 15 :: Int]
          , let (mx, my) = head way
                (dx, dy) = dirDelta wayDir
                fdx = fromIntegral dx :: Float
                fdy = fromIntegral dy :: Float
                n = fromIntegral (length way) - 0.5 :: Float
                u = wrap01 (rnd i 3 5 + clock * 0.004 * (0.7 + 0.6 * rnd i 4 5))
                along = u * n
                wobble = 0.28 * sin (fromIntegral i * 2.1 + clock * 0.03) ]
    wrap01 v = v - fromIntegral (floor v :: Int)

-- | The rush along the way. The station falls behind, the keeper bursts out through
-- its outer wall onto a causeway across open sea, and the screen carries them on,
-- ever faster. The sea is two seas: calm and sunlit in the past, black and storm
-- driven in the present, so every change of era turns the whole world over. The
-- real lighthouse's beam sweeps the water from far ahead. Barriers stand in one era
-- or the other: the faint outline of the other era's ones shows where to change.
rushWorld :: Scene -> Level -> Finale -> Game -> Fx -> Picture
rushWorld scene level f g fx = case finaleRun level of
  Nothing -> Blank
  Just fr ->
    let start@(sx, sy) = gCell g
        d = fnDash f
        clock = fromIntegral (fxClock fx) :: Float
        era = gEra g
        len = rushLength fr start
        arriving = fnStage f == Arrival
        along = if arriving then fromIntegral len else dsAlong d
        (dx, dy) = dirDelta (frDir fr)
        fdx = fromIntegral dx :: Float
        fdy = fromIntegral dy :: Float
        kx = fromIntegral sx + fdx * along
        ky = fromIntegral sy + fdy * along
        horizontal = dx /= 0
        -- The screen is carried with a point a little behind the keeper's lead; it
        -- starts from where the room left it, and settles on the causeway's line.
        (rx, ry) = roomOf (lvGoal level)
        bx = fromIntegral (rx * roomCols) :: Float
        by = fromIntegral (ry * roomRows) :: Float
        (cameraX, cameraY)
          | horizontal =
              let scrollX = fromIntegral sx + fdx * dsScroll d
                  cx | dx < 0 = min bx (scrollX - 8)
                     | otherwise = max bx (scrollX - 5)
              in (cx, by + (fromIntegral sy - 4 - by) * smooth (min 1 (abs (cx - bx) / 9)))
          | otherwise =
              let scrollY = fromIntegral sy + fdy * dsScroll d
                  cy | dy < 0 = min by (scrollY - 5.5)
                     | otherwise = max by (scrollY - 3.5)
              in (bx + (fromIntegral sx - 6 - bx) * smooth (min 1 (abs (cy - by) / 9)), cy)
        -- Only cells inside the view: at rest that is exactly the room on screen.
        x0 = floor cameraX :: Int
        y0 = floor cameraY :: Int
        x1 = ceiling (cameraX + 13) - 1 :: Int
        y1 = ceiling (cameraY + 9) - 1 :: Int
        shown = [ (x, y) | y <- [y0 .. y1], x <- [x0 .. x1] ]
        (ex, ey) = frEdge fr
        alongOf (cx, cy) = (cx - ex) * dx + (cy - ey) * dy
        acrossOf (cx, cy) = (cx - ex) * dy + (cy - ey) * dx
        onCauseway c = alongOf c >= 1 && alongOf c <= corridorLength && abs (acrossOf c) <= 1
        onIsland c = alongOf c > corridorLength && alongOf c <= corridorLength + 7 && abs (acrossOf c) <= 3
        isSea c = alongOf c > 0 && not (onCauseway c) && not (onIsland c)
        art = case era of
          Past -> eaDashPast (scEnding scene)
          Present -> eaDash (scEnding scene)
        cellsShown = Pictures [ pic | c <- shown, not (isSea c), Just pic <- [Map.lookup c art] ]

        -- The sea, cell by cell, alive: swell lines that rise and fall, glints of
        -- sun in the past, whitecaps in the present, foam where it meets the stone.
        seaCells = filter isSea shown
        (seaBase, crestC, foamC) = case era of
          Past -> (hex 0x17596A, 0xA8E0D8, 0xE6F6F2)
          Present -> (hex 0x0A161B, 0x7D93A0, 0xC9D6DB)
        sea = Pictures $
          [ at (cellPos c) (Color seaBase (rectC (cs + 1) (cs + 1))) | c <- seaCells ] ++
          -- Long swells rolling across the whole sea, lit and shadowed in turn.
          [ at (cellPos c) (Translate (fromIntegral u * third) (fromIntegral v * third) (Color (if s > 0 then hexA swellLit (swellK * s) else hexA 0x000000 (swellK * negate s)) (rectC (third + 1) (third + 1))))
          | c@(cx, cy) <- seaCells, u <- [-1 .. 1 :: Int], v <- [-1 .. 1 :: Int]
          , let s = sin ((fromIntegral cx + fromIntegral u / 3) * 0.55 - (fromIntegral cy - fromIntegral v / 3) * 0.35 - clock * swellSpeed) ] ++
          [ at (cellPos c) (Translate (sin (clock * swell + rnd cx cy (3 + i) * 6) * 7) (-24 + 24 * fromIntegral i + 4 * sin (clock * 0.05 + rnd cx cy i * 6))
              (thin (hexA crestC (0.22 + 0.2 * rnd cx cy (9 + i))) [(-w, 0), (-w / 3, lift), (w / 3, -lift / 2), (w, 0)]))
          | c@(cx, cy) <- seaCells, i <- [0 .. 2 :: Int]
          , let w = 10 + 16 * rnd cx cy (20 + i)
                lift = if era == Past then 2 else 5 ] ++
          [ at (cellPos c) (Translate (-28 + 56 * rnd cx cy 31) (-28 + 56 * rnd cx cy 32)
              (Color (hexA glintC (k * k * k * k)) (rectC (if era == Past then 9 else 6) 1.6)))
          | c@(cx, cy) <- seaCells
          , let k = max 0 (sin (clock * (if era == Past then 0.07 else 0.11) + rnd cx cy 33 * 6.28)) ] ++
          [ foamEdge c n | c <- seaCells, n <- [minBound .. maxBound], not (isSea (stepCell n c)) ]
        swell = if era == Past then 0.02 else 0.045
        third = cs / 3
        (swellLit, swellK, swellSpeed) = case era of
          Past -> (0x7FD0D6, 0.1, 0.025 :: Float)
          Present -> (0x5C7A88, 0.16, 0.06)
        glintC = if era == Past then 0xFFD49A else 0xDCE8EC
        -- Foam along the stone, breathing; in the storm it bursts into spray.
        foamEdge c@(cx, cy) n =
          let (ux, uy) = screenDir n
              pulse = 0.5 + 0.5 * sin (clock * 0.06 + rnd cx cy 41 * 6.28)
              burst = wrap01 (clock * 0.011 + rnd cx cy 42)
              spray = if era == Present && burst < 0.25 then sprayBurst (burst / 0.25) else Blank
          in at (cellPos c) (Translate (ux * (half - 6)) (uy * (half - 6)) (Pictures
               [ Color (hexA foamC (0.25 + 0.3 * pulse)) (if ux /= 0 then rectC 5 cs else rectC cs 5)
               , Translate (-ux * 10) (-uy * 10) spray ]))
        sprayBurst p = Pictures
          [ Translate (fromIntegral (j - 2) * 12 + 4 * sin (fromIntegral j)) (-(p * 26) + fromIntegral (j `mod` 2) * 6)
              (Color (hexA 0xE6F0F2 (0.55 * (1 - p))) (ngon 8 (4 + p * 9)))
          | j <- [0 .. 4 :: Int] ]
        wrap01 v = v - fromIntegral (floor v :: Int)

        -- The causeway's low wall along both edges, with lamp posts: brass and whole
        -- in the past, broken through in places in the present.
        curb = Pictures
          [ at (cellPos c) (Translate (sgn * fdy * (half - 4)) (negate (sgn * fdx * (half - 4)))
              (Pictures [ Color capC (if horizontal then rectC cs 8 else rectC 8 cs)
                        , Color capHi (if horizontal then rect (-half) 1 cs 2 else rect 1 (-half) 2 cs) ]))
          | c <- shown, onCauseway c, let a = acrossOf c, abs a == 1
          , let sgn = fromIntegral a :: Float
                (cx, cy) = c
          , era == Past || rnd cx cy 51 > 0.2 ]
        (capC, capHi) = case era of
          Past -> (hex 0xC6A679, hex 0xE8D3A8)
          Present -> (hex 0x5D6B72, hex 0x83929A)
        keeperAlong = (kx - fromIntegral ex) * fdx + (ky - fromIntegral ey) * fdy
        lamps = Pictures
          [ at (lx + fdy * fromIntegral side * (half - 4), ly - fdx * fromIntegral side * (half - 4))
               (passageLamp (smooth ((keeperAlong - fromIntegral i + 2.5) / 1.5)))
          | i <- [2, 6 .. corridorLength - 1]
          , side <- [1, -1 :: Int]
          , let (lx, ly) = cellPos (outFrom fr i side) ]
        -- Each barrier dressed for its era; the other era's show as a faint outline,
        -- to say where the way will be shut once the era changes.
        barrierArt = Pictures
          [ at (cellPos c) (if e == era then (if e == Present then presentRubble else pastBulkhead) else ghost)
          | (i, e) <- barriers, j <- [-1 .. 1], let c = outFrom fr i j, c `elem` shown ]
        ghost = Color (hexA (if era == Past then 0x2A1A12 else 0xB7C4C9) 0.35)
                  (segs 2 [(-half + 6, -half + 6), (half - 6, -half + 6), (half - 6, half - 6), (-half + 6, half - 6), (-half + 6, -half + 6)])

        arrivingAge = if arriving then fromIntegral (fnAge f) else 0 :: Float
        towerK = 0.55 + 0.45 * smooth (arrivingAge / 30)
        towerAt' = cellPos (frTower fr)
        lanternAt = (fst towerAt', snd towerAt' + 150)
        -- The real lighthouse's beam, turning over the water from the far end.
        beam = at lanternAt (Rotate (clock * 0.55) (Pictures
          [ Color (hexA 0xFFE9B0 (if era == Past then 0.08 else 0.17)) (Polygon [(0, -6), (2600, -300), (2600, 300), (0, 6)])
          , Color (hexA 0xFFF6DD (if era == Past then 0.06 else 0.13)) (Polygon [(0, -3), (2600, -110), (2600, 110), (0, 3)]) ]))
        speedK = clamp01 ((dsSpeed d / dashTopSpeed - 0.3) / 0.7)
        trail = dsSpeed d * cs * 3.4
        streaks
          | speedK <= 0.01 || arriving = Blank
          | otherwise = Color (hexA 0xFFF1D6 (0.28 * speedK)) (Pictures
              [ if horizontal
                  then rect (if dx < 0 then px else px - trail) (py - 1) trail 2
                  else rect (px - 1) (if dy < 0 then py - trail else py) 2 trail
              | c@(cx, cy) <- shown, onCauseway c, rnd cx cy 61 < 0.4
              , let (qx, qy) = cellPos c
                    px = qx - 30 + 60 * rnd cx cy 62
                    py = qy - 30 + 60 * rnd cx cy 63 ])
        running = dsSpeed d > 0.01 && not arriving
        stride = if running then let s = along / 2 in s - fromIntegral (floor s :: Int) else 0
        bump = if dsBlocked d then 3 * sin (fromIntegral (fxClock fx) * 0.9) else 0
        keeperPos = let (px, py) = cellVec kx ky in (px - fdx * bump, py + 6 + fdy * bump)
        ripple
          | dsShifted d < shiftTicks = at keeperPos (waveRing (fromIntegral (dsShifted d) / fromIntegral shiftTicks))
          | otherwise = Blank
        refusedRing
          | dsRefused d > 0 = at keeperPos (Color (hexA 0xFF7A4A (fromIntegral (dsRefused d) / 24)) (ring 24 30 3 1))
          | otherwise = Blank

        -- Rain across the present, and now and then lightning over the whole sea.
        rain
          | era /= Present = Blank
          | otherwise = Pictures
              [ Translate x y (Color (hexA 0xB7CEDA (0.18 + 0.2 * rnd i 3 8)) (Rotate (-22) (rectC 16 1.3)))
              | i <- [0 .. 44 :: Int]
              , let speed = 2.6 + 2.4 * rnd i 2 8
                    w = mapRight - mapLeft
                    h = mapTop - mapBottom
                    x = mapLeft + wrapTo w (w * rnd i 1 8 - clock * speed * 0.45)
                    y = mapBottom + wrapTo h (h * rnd i 4 8 - clock * speed) ]
        wrapTo size v = v - size * fromIntegral (floor (v / size) :: Int)
        flashK
          | era /= Present = 0
          | otherwise = let t = fxClock fx `mod` 330 in
              if t < 5 then 0.26 else if t >= 9 && t < 13 then 0.16 else 0
        lightning = if flashK > 0 then Color (hexA 0xDCE8F0 flashK) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom)) else Blank
        -- Taken back: the screen goes dark, and lightens on the last barrier passed.
        caughtK = if dsCaught d > 0 then 1 - abs (fromIntegral (dsCaught d) - fromIntegral catchTicks / 2) / (fromIntegral catchTicks / 2) else 0
        caughtDark = if caughtK > 0 then Color (hexA 0x05080A (0.92 * smooth caughtK)) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom)) else Blank
        offset = max (abs (cameraX - bx)) (abs (cameraY - by))
        maskK = smooth (min 1 (offset * 3))
        darkK = 0.13 * (1 - smooth (along / fromIntegral (max 1 len) * 1.3))
        wayGlow = Pictures
          [ at (cellPos c) (Color (hexA 0xFFE2A8 (0.2 * (0.45 + 0.55 * fromIntegral j / fromIntegral (length (wayCells level))))) (if horizontal then rectC cs 46 else rectC 46 cs))
          | era == Present, (j, c) <- zip [0 :: Int ..] (wayCells level) ]
        caption = case era of
          Past -> captionOf scene Past
          Present -> scDarkCaption scene
    in Pictures [ seaOf scene era, scSeaShade scene, board 1 (Pictures
         [ backOf scene era
         , Translate (negate (cameraX * cs)) (cameraY * cs) (Pictures
             [ sea
             , cellsShown
             , if era == Present then lightPools level Present 1 else Blank
             , wayGlow
             , barrierArt
             , curb
             , Pictures [ at (cellPos c) (crate era) | c <- Set.toList (gCrates g), c `elem` shown ]
             , at towerAt' (pool 0xFFD98A (0.3 * towerK) 620 560)
             , beam
             , lamps
             , streaks
             , at towerAt' (trueTower clock towerK)
             , if arriving then at lanternAt (victoryRays clock (smooth (arrivingAge / 50))) else Blank
             , ripple
             , refusedRing
             , at keeperPos (keeper (frDir fr) stride)
             ])
         , ambient era clock 1
         , rain
         , lightning
         , if darkK > 0.005 then Color (hexA 0x05080A darkK) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom)) else Blank
         , caughtDark
         , viewportMaskK maskK era
         , vigOf scene era
         , caption
         , rushHint fr start era d
         ]) ]

-- | One line under the map while rushing, only when there is something to say.
rushHint :: FinaleRun -> Cell -> Era -> Dash -> Picture
rushHint fr start era d
  | dsCaught d > 0 = hintBand era 1 "The light ran on without you.   Again, from the last barrier."
  | dsRefused d > 0 = hintBand era (fromIntegral (dsRefused d) / 24) "Not here: in the other era a wall stands right where you are."
  | dsBlocked d = hintBand era 1 $ case era of
      Present -> "Fallen stone.   SPACE: in the past, the way was clear."
      Past -> "A shut gate.   SPACE: in the present, it is long gone."
  | dsFails d == 0 && firstAhead = hintBand era 1 "Out here, SPACE changes era for nothing.   Watch the way ahead."
  | otherwise = Blank
  where
    firstAhead = case barriersFrom fr start of
      (a, _) : _ -> dsAlong d > a - 7 && dsAlong d < a + 0.75
      [] -> False

-- | A whole frame of an ending: the station or the rush, the panel, and what is
-- laid over them: the plate that says it is over, the flash, the white of arrival,
-- and the card for the level beyond.
endingFrame :: Scene -> Level -> (String, String) -> Finale -> Game -> Fx -> Picture
endingFrame scene level (number, name) f g fx = Pictures
  [ Color (hex 0x050A0D) (rectC (canvasW + 40) (canvasH + 40))
  , Translate shakeX shakeY scenePic
  -- Once the keeper walks free, the watch stops: the walk costs nothing.
  , hudStopped (scHud scene) (if stage >= Beckon then revealedLevel level else level) g fx clock
  , board 1 plate
  , board 1 (overlays scene g fx) `onlyIf` (fnStage f == Beckon)
  , flash
  , card
  ]
  where
    stage = fnStage f
    age = fnAge f
    ageF = fromIntegral age :: Float
    clock = fromIntegral (fxClock fx) :: Float
    onlyIf p c = if c then p else Blank
    scenePic
      | stage >= Rush = rushWorld scene level f g fx
      | otherwise = world (Just f) scene level g fx
    -- The last plate of the hall, and then the lamp takes it back.
    plate = case stage of
      Lit -> endedPlate (smooth ((ageF - 70) / 30)) (gRemaining g)
      Flare | age < 12 -> endedPlate (1 - ageF / 12) (gRemaining g)
      _ -> Blank
    -- Each wall that breaks shakes the station a little.
    shakeAmp = case stage of
      Opening -> maximum (0 : [ 4 * (1 - a / 10) | k <- [0 .. length (lvPassage level) - 1]
                              , let a = fromIntegral (age - cellOpenTick level k), a >= 0, a < 10 ])
      Flare | age >= lampDiesAt && age < lampDiesAt + 8 -> 3
      _ -> 0
    shakeX = shakeAmp * sin (ageF * 2.3)
    shakeY = shakeAmp * cos (ageF * 3.1)
    -- The flare whites out everything, the panel too, and under it the lamp dies.
    -- Arriving, the real lamp does the same, and then the dark comes down.
    flash = case stage of
      Flare ->
        let a | age < 6 = 0
              | age < 16 = 0.92 * smooth ((ageF - 6) / 10)
              | age < lampDiesAt = 0.92
              | otherwise = 0.92 * (1 - smooth ((ageF - fromIntegral lampDiesAt) / 22))
        in Color (hexA 0xFFF6DD a) (rectC canvasW canvasH)
      Arrival -> Pictures
        [ Color (hexA 0xFFF8E6 (smooth ((ageF - 50) / 50))) (rectC canvasW canvasH)
        , Color (hexA 0x05080A (smooth ((ageF - 110) / 40))) (rectC canvasW canvasH)
        ]
      _ -> Blank
    -- The level beyond names itself in the dark, then the dark lifts on it.
    card = case stage of
      Arrival | age > 140 ->
        let k = smooth ((ageF - 140) / 25) * (1 - smooth ((ageF - 200) / 26))
        in Pictures
             [ Color (fade k (hex 0xFFE2A8)) (Translate 0 40 (weightedC 64 7 number))
             , Color (fade k (hex 0xF3E9D5)) (Translate 0 (-22) (textC 22 name))
             , Color (fade (k * 0.8) (hex 0x9AB0B6)) (Translate 0 (-58) (textC 12 "A level the hall does not list"))
             ]
      _ -> Blank

-- | Hide the parts of a large station outside the original 13 x 9 map window.
-- It is drawn on the board, which is smaller than the canvas, so it reaches a
-- whole canvas beyond the map on every side.
viewportMask :: Era -> Picture
viewportMask = viewportMaskK 1

-- | The same mask, partly drawn, @k@ 0 to 1.
viewportMaskK :: Float -> Era -> Picture
viewportMaskK k era = Color (fade k outside) (Pictures
  [ rect (mapLeft - canvasW) (mapBottom - canvasH) canvasW (mapTop - mapBottom + 2 * canvasH)
  , rect mapRight (mapBottom - canvasH) canvasW (mapTop - mapBottom + 2 * canvasH)
  , rect mapLeft mapTop (mapRight - mapLeft) canvasH
  , rect mapLeft (mapBottom - canvasH) (mapRight - mapLeft) canvasH
  ])
  where
    outside = case era of
      Past -> hex 0x102F36
      Present -> hex 0x050A0D

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
stationCells :: Scene -> Cell -> Game -> Fx -> Picture
stationCells scene room g fx = case fxWave fx of
  Nothing -> wholeOf scene (gEra g) room
  Just w -> Pictures (zipWith (flipped w) (cellsOf scene (waveFrom w) room) (cellsOf scene (gEra g) room))
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
