-- | Things that move or shine on top of the station.
--
-- The station itself (floors, walls, doors, the sea around it) is drawn by "Station",
-- and most of it is baked once per level. This module draws what is set on top of
-- it and moves, glows, or comes and goes: the keepers, the crates, the power cell,
-- the mirrors and the light they throw, the hints the board gives (the jump arrow,
-- the push hint, the doorway glow, the crossing ticks, the blocked landing), the
-- echo and its footprints, the flash of a pickup and the light that follows the
-- carrier, the lamp lit again at a win, the ending's real lighthouse and the lamps
-- of its passage, the drifting dust or spray, and the bright ring of the era wave.
--
-- What it does not do: it never looks at a 'Game' or a 'Level'. Every function here
-- takes plain values (a clock, a strength, a direction, an era) and returns a
-- 'Picture'. "Render" works out from the rules what is shown, where and how
-- strongly; this module only decides how each thing looks. Nothing here is baked:
-- these pictures change from frame to frame, so they are built again every frame
-- they are shown.
--
-- Used by "Render", for nearly all of it, and by "Hud", for the two heads and the
-- power cell's icon on the watch. It uses "Draw" for shapes, colours, positions and
-- easing, "Game.Types" for 'Dir' and 'Era', and "Station" for its soft pools of
-- light and the cells that carry wall lamps.
--
-- Conventions, which hold throughout unless a comment says otherwise:
--
-- Each figure is drawn centred on the origin, in board units, with y going up
-- (Gloss's way round; cell rows in "Game.Types" count the other way, down the map).
-- A cell is @cs@ = 76 units across, so a figure that fills its cell spans -38 to 38
-- (@half@). The caller moves it into place, usually with @at (cellPos c)@.
--
-- @clock@ is the effects clock, 'fxClock' in "Game.Fx", in ticks (60 a second). It
-- stops while the game is paused. A term such as @sin (clock * 0.09)@ goes round
-- once every 2 pi / 0.09 ticks, about 70 ticks or 1.2 seconds; the period of each
-- one is given beside it below.
--
-- A strength @k@ runs from 0 (not shown) to 1 (fully shown) and usually multiplies
-- an alpha. An alpha that strays below 0 or above 1 is safe: Gloss's 'makeColor',
-- which 'hexA' and 'fade' go through, clamps every component to 0..1.
--
-- Colours are written as hex RGB, 0xRRGGBB, through 'hex' (opaque) and 'hexA' (with
-- an alpha) from "Draw". Angles are in degrees, and Gloss turns a picture clockwise.
-- Art that points a way is drawn pointing up (+y) and then turned by 'faceAngle'.
module Figures
  ( Look (..)
  , courierLook
  , keeperLook
  , trueTower
  , passageLamp
  , powerCell
  , crate
  , crossingMark
  , footprintMark
  , landingMark
  , heldPlateGlow
  , mirrorArt
  , bendMirrorArt
  , beamLight
  , jumpArrow
  , pushHint
  , doorwayHint
  , blockedLanding
  , screenDir
  , cellIcon
  , carriedLight
  , pickupBurst
  , lanternGlow
  , lanternOffset
  , victoryRays
  , relitSconces
  , warmthOver
  , ambient
  , waveRing
  ) where

import Draw  -- shapes, colours, cell positions, easing and noise
import Game.Types  -- Dir and Era, the only game types the art needs
import Graphics.Gloss
import Station (pool, sconceCells)  -- soft pools of light; the cells with wall lamps

-- The walkers ------------------------------------------------------------------

-- | Who walks the board, drawn the three ways the game needs: the courier always,
-- and the lighthouse keeper beside her in a level for two.
data Look = Look
  { lookWalk :: Dir -> Float -> Picture
    -- ^ The figure, facing a way. The second argument runs 0..1 across a step.
  , lookEcho :: Float -> Float -> Picture
    -- ^ Their outline in pale light, where a jump would set them down: the clock,
    -- and how strongly it shows.
  , lookHead :: Picture
    -- ^ A head about 21 units across, for the legend of keys.
  }

courierLook, keeperLook :: Look
courierLook = Look courier courierEcho courierHead
keeperLook = Look lightKeeper keeperEcho keeperHead

type Pt = (Float, Float)

-- | The outline every figure is drawn in.
figureInk :: Color
figureInk = hex 0x17100B

-- | A filled shape with the outline round it: the stroke goes down first and the
-- fill over it, so half the stroke shows outside the shape.
blob :: Color -> Float -> [Pt] -> Picture
blob fill w pts = Pictures [Color figureInk (segs w (closed pts)), Color fill (Polygon pts)]

closed :: [Pt] -> [Pt]
closed ps = ps ++ take 1 ps

-- | Mirrored across the figure's middle, in the same winding.
flipX :: [Pt] -> [Pt]
flipX = reverse . map (\(x, y) -> (negate x, y))

-- | How a step moves a figure: the bob and the stride the walkers have always had,
-- and the hair that lags behind them. In order: how high the body rises, how far
-- a foot lifts, hair swinging across with the step, hair tips staying low as the
-- body rises, and hair streaming behind, seen from the side. Each is 0 standing
-- still.
data Gait = Gait !Float !Float !Float !Float !Float

gaitAt :: Float -> Gait
gaitAt phase = Gait bob (s * 3) (s * 1.4) (negate (0.5 * bob)) (abs s * 1.6)
  where
    s = sin (phase * 2 * pi)
    bob = 2.2 * abs (sin (phase * pi))

-- | On the board: the shadow, and over it the figure turned the way it faces,
-- bobbing. Seen from the right is seen from the left, mirrored.
stood :: Float -> Dir -> Picture -> Picture -> Picture -> Picture
stood bob facing front back side = Scale 1.08 1.08 (Pictures
  [ Color (hexA 0x000000 0.45) (Translate 0 (-20) (ellipse 16 17 7))
  , Translate 0 bob (case facing of
      DirDown  -> front
      DirUp    -> back
      DirLeft  -> side
      DirRight -> Scale (-1) 1 side)
  ])

figureLegs :: Color -> Float -> Float -> Picture
figureLegs boots a b = Pictures
  [ Color figureInk (Pictures [rect (-9) (-21 + a) 8 10, rect 1 (-21 + b) 8 10])
  , Color boots (Pictures [rect (-8) (-20 + a) 6 8, rect 2 (-20 + b) 6 8])
  ]

-- | The coat or jacket: one shape, lit from the left.
trunkShape :: [Pt]
trunkShape = [(-14, -12), (14, -12), (10.5, 11), (-10.5, 11)]

figureTrunk :: Color -> Color -> Picture
figureTrunk main shade = Pictures
  [ Color figureInk (Polygon [(-16, -14), (16, -14), (12, 12), (-12, 12)])
  , Color main (Polygon trunkShape)
  , Color shade (Polygon [(3, -12), (14, -12), (10.5, 11), (2, 11)])
  ]

-- The courier ------------------------------------------------------------------

-- Drawn from assets/source/courier.webp: a yellow hi-vis delivery jacket, white
-- hair in two tails tied with black bows, a curl of hair on top, and a black
-- insulated box on her back with a steaming bowl on it.

coCoat, coCoatShade, coStripe, coStripeShade, coDark, coDarkHi, coBuckle, coIcon,
  coLid, coLidShade, coHair, coHairShade, coHairDeep, coRibbon, coRibbonHi, coSkin,
  coEye, coMouth, coBoots :: Color
coCoat = hex 0xF7C81E
coCoatShade = hex 0xC4920F
coStripe = hex 0xEAE8E2
coStripeShade = hex 0xB9B5AC
coDark = hex 0x2C2B33
coDarkHi = hex 0x4C4A56
coBuckle = hex 0x9C9AA4
coIcon = hex 0xA9A7B2
coLid = hex 0xF2BC1A
coLidShade = hex 0xB98A10
coHair = hex 0xF2EFF7
coHairShade = hex 0xCDC5DD
coHairDeep = hex 0x9C92B4
coRibbon = hex 0x1D1A21
coRibbonHi = hex 0x56505F
coSkin = hex 0xF3D5BB
coEye = hex 0x7B3438
coMouth = hex 0xC98E7E
coBoots = hex 0x2A2530

-- | Her left tail seen from the front: from under the bow, out over the shoulder
-- and down past it. @s@ swings the tip across, @d@ drops it.
tailFront :: Float -> Float -> [Pt]
tailFront s d =
  [ (-10, 25), (-14.2, 25), (-17.4, 20.8), (-18.6, 14), (-18, 6)
  , (-16.8 + s * 0.6, -1 + d * 0.5), (-15.2 + s, -7 + d), (-13.6 + s * 0.6, -2.4 + d * 0.5)
  , (-12.8, 5), (-12.2, 13), (-10.4, 19.6) ]

-- | The shadowed inner half of that tail.
tailFrontShade :: Float -> Float -> [Pt]
tailFrontShade s d =
  [ (-12.6, 19), (-14.8, 15), (-15.2, 8), (-14.8 + s * 0.6, d * 0.5), (-15 + s, -5.4 + d)
  , (-13.8 + s * 0.6, -1.6 + d * 0.5), (-13.2, 6), (-12.6, 13) ]

-- | A black bow, its knot at (x, y), turned @tilt@ degrees.
bow :: Float -> Float -> Float -> Picture
bow x y tilt = Translate x y (Rotate tilt (Pictures
  [ Color coRibbon (Pictures
      [ tri (0, 0) (-4.6, 3) (-4.6, -3), tri (0, 0) (4.6, 3) (4.6, -3)
      , tri (-0.6, 0) (-2.4, -5) (-0.6, -4.2), tri (0.6, 0) (2.4, -5) (0.6, -4.2) ])
  , Color coRibbonHi (Pictures [tri (-1.4, 0.4) (-3.8, 2) (-3.8, 0.6), tri (1.4, 0.4) (3.8, 2) (3.8, 0.6)])
  , Color figureInk (ngon 8 1.7)
  ]))

-- | The curl on top of her head, swung about its root by @tilt@ degrees.
ahoge :: Float -> Float -> Picture
ahoge x tilt = Translate x 28.4 (Rotate tilt (Translate 0 (-28.4) (Pictures
  [ Color figureInk (segs 3.4 curl), Color coHair (segs 1.5 curl) ])))
  where curl = [(0.4, 28.4), (0.9, 32.4), (3.1, 35.2), (5.9, 35.1), (7, 32.7), (5.5, 31.1)]

-- | The courier. @phase@ runs 0..1 across a step.
courier :: Dir -> Float -> Picture
courier facing phase = stood bob facing front back side
  where
    Gait bob stride sway dip trail = gaitAt phase
    capOutline cx = Color figureInk (Translate cx 17 (ngon 20 14.2))
    -- the head of hair, its lower right in shadow
    hairCap cx = Pictures
      [ Color coHairShade (Translate cx 17 (ngon 20 12.6))
      , Color coHair (Translate (cx - 1.3) 17.8 (ngon 20 11.4))
      ]
    tails =
      let l = tailFront sway dip
          r = flipX (tailFront (negate sway) dip)
      in Pictures
        [ Color figureInk (Pictures [segs 3 (closed l), segs 3 (closed r)])
        , Color coHair (Polygon l)
        , Color coHairShade (Polygon r)
        , Color coHairShade (Polygon (tailFrontShade sway dip))
        , Color coHairDeep (Polygon (flipX (tailFrontShade (negate sway) dip)))
        ]
    bows = Pictures [bow (-11.8) 22.6 20, bow 11.8 22.6 (-20)]
    curl = ahoge 0.4 (negate sway * 4)
    frontLegs = figureLegs coBoots (max 0 stride) (max 0 (negate stride))
    fringe = [ (-8.5, 14.2), (-6.5, 15.6), (-4.6, 13.6), (-2.6, 15.8), (-0.4, 13.4)
             , (1.8, 15.6), (3.9, 13.8), (5.9, 15.8), (7.9, 14.2) ]
    -- the steaming bowl on the box
    bowl = Color coIcon (Pictures
      [ rect (-6) (-3.4) 12 1.3
      , Polygon [ (5.2 * cos a, -3.4 + 3.6 * sin a) | i <- [0 .. 8 :: Int], let a = pi + pi * fromIntegral i / 8 ]
      , segs 1 [(-2.6, -1), (-1.8, 0.8), (-2.6, 2.6)]
      , segs 1 [(0, -1), (0.8, 0.8), (0, 2.6)]
      , segs 1 [(2.6, -1), (3.4, 0.8), (2.6, 2.6)]
      ])
    backHair = [ (-12.2, 12), (-12, 9.6), (-9.8, 7.4), (-7.2, 9), (-4.6, 6.2), (-1.4, 8.2), (1.6, 5.8)
               , (4.6, 8.4), (7.4, 6.4), (10, 8.6), (12, 9.8), (12.2, 12) ]

    front = Pictures
      [ frontLegs
      , tails
      , figureTrunk coCoat coCoatShade
      -- the hi-vis band, shaded with the coat
      , Color coStripe (Polygon [(-13.1, -6.4), (13.1, -6.4), (12.7, -3.2), (-12.7, -3.2)])
      , Color coStripeShade (Polygon [(2.6, -6.4), (13.1, -6.4), (12.7, -3.2), (2.4, -3.2)])
      , Color figureInk (rect (-0.8) (-12) 1.6 22)
      , Color coDark (Pictures [rect (-3.6) 3.4 2 2, rect (-3.6) (-1.2) 2 2])
      -- the box's straps over her shoulders
      , Color coDark (Pictures
          [ Polygon [(-9.6, 11), (-6, 11), (-6.8, -1.4), (-10.4, -1.4)]
          , Polygon [(6, 11), (9.6, 11), (10.4, -1.4), (6.8, -1.4)] ])
      , Color coBuckle (Pictures [rect (-9.8) 1.4 3 1.6, rect 6.8 1.4 3 1.6])
      , capOutline 0, hairCap 0
      -- the black collar and its buckle, under her chin
      , Color coDark (rect (-5.4) 3.2 10.8 3.4), Color coBuckle (rect (-1.4) 4 2.8 1.8)
      , Color figureInk (Translate 0 12 (ellipse 16 8.9 7.7))
      , Color coSkin (Translate 0 12 (ellipse 16 7.6 6.4))
      , Color coHair (Polygon ((-8.8, 19.4) : fringe ++ [(8.8, 19.4)]))
      , Color coHairDeep (segs 1.1 fringe)
      -- the locks framing her face
      , Color coHair (Pictures
          [ Polygon [(-9.4, 17), (-7.2, 16), (-6.9, 9), (-7.6, 4.6), (-9.8, 6.4), (-10.4, 11)]
          , Polygon [(9.4, 17), (7.2, 16), (6.9, 9), (7.6, 4.6), (9.8, 6.4), (10.4, 11)] ])
      , Color figureInk (Pictures
          [ segs 1.3 [(-7.2, 16), (-6.9, 9), (-7.6, 4.6), (-9.8, 6.4)]
          , segs 1.3 [(7.2, 16), (6.9, 9), (7.6, 4.6), (9.8, 6.4)] ])
      , Color coEye (Pictures [rect (-4.7) 9.4 2.3 3.2, rect 2.4 9.4 2.3 3.2])
      , Color coMouth (rect (-0.7) 7.1 1.4 0.8)
      , bows
      , curl
      ]

    back = Pictures
      [ frontLegs
      , figureTrunk coCoat coCoatShade
      , Color coStripe (Polygon [(-13.1, -6.4), (-2, -6.4), (-2, -3.2), (-12.7, -3.2)])
      , Color coStripeShade (Polygon [(-2, -6.4), (13.1, -6.4), (12.7, -3.2), (-2, -3.2)])
      -- the box
      , Color figureInk (rect (-13.6) (-10) 27.2 24.6)
      , Color coDark (rect (-12) (-8.4) 24 21.4)
      , Color coDarkHi (rect (-12) (-8.4) 1.6 21.4)
      , Color coLid (rect (-12) 9.4 24 3.6), Color coLidShade (rect 3 9.4 9 3.6)
      , Color figureInk (rect (-12) 8.6 24 0.9)
      , bowl
      , tails
      -- her hair down the back, over the top of the box
      , Color figureInk (Pictures [Translate 0 17 (ngon 20 14.2), segs 3 backHair])
      , Color coHair (Pictures
          [ Translate 0 17 (ngon 20 12.6)
          , Polygon ((-12.3, 17) : backHair ++ [(12.3, 17)]) ])
      , Color coHairShade (Polygon [ (4.6, 8.4), (7.4, 6.4), (10, 8.6), (12, 9.8), (12.2, 12), (12.3, 17)
                                   , (11.6, 21.4), (9.6, 24.6), (7.6, 20), (6.4, 13) ])
      , Color coHairDeep (segs 1 [(0, 29.4), (0, 19.4)])
      , bows
      , curl
      ]

    -- Facing left. The box rides on her back, at +x; the near tail hangs from the
    -- back of her head over it, the far one shows just behind.
    tailSide dx dy =
      let s = trail + sway * 0.4
      in [ (4 + dx, 25.4 + dy), (8.8 + dx, 25.6 + dy), (12 + dx, 21.6 + dy), (13.2 + dx, 15 + dy)
         , (12.8 + dx, 7 + dy), (11.8 + dx + s * 0.6, 1 + dy + dip * 0.5), (10.4 + dx + s, -4.6 + dy + dip)
         , (8.8 + dx + s * 0.6, 0.4 + dy + dip * 0.5), (8.4 + dx, 8 + dy), (7.8 + dx, 15 + dy), (5.4 + dx, 20.6 + dy) ]
    sideFringe = [(-12.2, 14.2), (-10.4, 15.2), (-8.6, 13.6), (-6.4, 15.4), (-3.8, 15.6)]
    side = Pictures
      [ figureLegs coBoots stride (negate stride)
      -- the box, standing proud of her back
      , Color figureInk (rect 3.4 (-10) 14 24.4)
      , Color coDark (rect 5 (-8.4) 10.8 21.2)
      , Color coDarkHi (rect 5 (-8.4) 1.6 21.2)
      , Color coLid (rect 5 9.2 10.8 3.6), Color coLidShade (rect 11.6 9.2 4.2 3.6)
      , Color figureInk (rect 5 8.4 10.8 0.9)
      , Color figureInk (Polygon [(-16, -14), (5.4, -14), (5.4, 12), (-12, 12)])
      , Color coCoat (Polygon [(-14, -12), (4, -12), (4, 11), (-10.5, 11)])
      , Color coStripe (Polygon [(-13.1, -6.4), (4, -6.4), (4, -3.2), (-12.7, -3.2)])
      -- the arm, with the band round the sleeve, and the strap over the shoulder
      , Color figureInk (rect (-4.4) (-10) 9 15.6)
      , Color coCoatShade (rect (-3) (-8.6) 6.2 12.8)
      , Color coStripeShade (rect (-3) (-5.6) 6.2 2.4)
      , Color coDark (rect (-1) 4.4 4 6.6)
      , blob coHairDeep 3 (tailSide 2.6 1.6)
      , capOutline 1.4, hairCap 1.4
      , Color coDark (rect (-9.8) 3.2 9 3.4)
      , Color figureInk (Translate (-5.4) 12.2 (ngon 16 6.9))
      , Color coSkin (Translate (-6.4) 12.2 (ngon 16 5.6))
      , Color coHair (Polygon ((-12.8, 15.6) : sideFringe ++ [(-3.4, 19.6), (-12, 19.6)]))
      , Color coHairDeep (segs 1.1 sideFringe)
      , Color coEye (rect (-10.6) 9.4 2.2 3.2)
      -- the lock in front of her ear
      , Color coHair (Polygon [(-2.2, 17.4), (0.8, 16.8), (0.6, 9), (-0.6, 5.6), (-2.2, 7.6)])
      , Color figureInk (segs 1.3 [(0.8, 16.8), (0.6, 9), (-0.6, 5.6), (-2.2, 7.6), (-2.2, 17.4)])
      , blob coHair 3 (tailSide 0 0)
      , Color coHairShade (Polygon [(8.6, 7), (11.2, 12), (11.6, 5), (10.4 + trail + sway * 0.4, -3 + dip), (9.2, 1)])
      , bow 6.4 23.4 (-10)
      , ahoge 0 (negate sway * 4 + trail * 3)
      ]

-- | Where a jump would set her down: her outline, tails and all, in pale light.
-- @k@ is how strongly it shows, lower in the era the player is not looking at.
courierEcho :: Float -> Float -> Picture
courierEcho clock k = echoOf clock k
  [Polygon l, Polygon r]
  [segs 1.6 (take 9 l), segs 1.6 (drop 2 r)]
  where
    l = tailFront 0 0
    r = flipX l

courierHead :: Picture
courierHead = Pictures
  [ Color (hexA 0x000000 0.5) (ngon 20 10.5)
  , Color coCoat (Polygon [(-7, -10), (7, -10), (5, -5), (-5, -5)])
  , Color coHair (ngon 20 9)
  , Color (hex 0xF6D9B6) (Translate 0 (-1.8) (ngon 16 5.2))
  , Color coHair (Polygon legendFringe)
  , legendEyes
  , bow (-8) 5 25, bow 8 5 (-25)
  ]

-- The lighthouse keeper ---------------------------------------------------------

-- Drawn from her portrait: long black hair, a brown field jacket worn open over a
-- black shirt, pale piping on its edges, flap pockets, a brass patch on the sleeve.

keJacket, keJacketShade, keJacketDeep, kePiping, keShirt, keBrass, keHair, keHairDeep,
  keSheen, keSheenHi, keSkin, keEye, keMouth, keBoots :: Color
keJacket = hex 0x75523F
keJacketShade = hex 0x523829
keJacketDeep = hex 0x3E2A20
kePiping = hex 0xC9B9A6
keShirt = hex 0x24212A
keBrass = hex 0xC29A45
keHair = hex 0x2B2633
keHairDeep = hex 0x18151D
keSheen = hex 0x5D5674
keSheenHi = hex 0x8A84A6
keSkin = hex 0xF0D0B4
keEye = hex 0x4A4048
keMouth = hex 0xBE8A7C
keBoots = hex 0x1F1A17

-- | Her hair seen from the front, falling behind her to the small of her back.
keeperBackHair :: Float -> Float -> [Pt]
keeperBackHair s d =
  [ (-12.4, 22), (12.4, 22), (15.4, 14), (16.2, 3), (15 + s, -7 + d), (11, -6.2)
  , (-11, -6.2), (-15 + s, -7 + d), (-16.2, 3), (-15.4, 14) ]

-- | The lighthouse keeper. @phase@ runs 0..1 across a step.
lightKeeper :: Dir -> Float -> Picture
lightKeeper facing phase = stood bob facing front back side
  where
    Gait bob stride sway dip trail = gaitAt phase
    frontLegs = figureLegs keBoots (max 0 stride) (max 0 (negate stride))
    -- a lock over each shoulder; the right one mirrors the left but swings the same way
    lock s =
      [ (-10.4, 18), (-7.6, 16), (-7.4, 8), (-7.4 + s * 0.4, -1 + dip * 0.5), (-8.8 + s * 0.7, -3.8 + dip)
      , (-10.2 + s * 0.3, -0.6 + dip * 0.5), (-10.8, 8) ]
    fringe = [ (-8.7, 12.6), (-6.7, 14.6), (-5.2, 12), (-3, 14.2), (-1.2, 11.6), (0.6, 14.6)
             , (2.6, 12.4), (4.6, 14.6), (6.4, 12.2), (8.3, 13.8) ]

    front = Pictures
      [ blob keHairDeep 3 (keeperBackHair sway dip)
      , frontLegs
      , figureTrunk keJacket keJacketShade
      -- the jacket open on her shirt, piped down both edges
      , Color keShirt (Polygon [(-4.4, 11), (4.4, 11), (3.6, -12), (-3.6, -12)])
      , Color kePiping (Pictures [segs 1.1 [(-4.4, 11), (-3.6, -12)], segs 1.1 [(4.4, 11), (3.6, -12)]])
      , Color keJacketShade (rect (-11.6) (-4.6) 5.8 5), Color keJacketDeep (rect 5.8 (-4.6) 5.6 5)
      , Color kePiping (Pictures [rect (-11.8) 0.2 6.2 1, rect 5.6 0.2 6.2 1])
      , Color keBrass (rect (-14.4) (-9.6) 3.8 1.7)
      , Color figureInk (Translate 0 17 (ngon 20 14.2))
      , Color keHair (Translate 0 17 (ngon 20 12.6))
      , Color keSheen (Pictures
          [ Polygon [(-9.6, 23), (-5.8, 26.6), (-0.6, 27.9), (3.2, 27.5), (-0.8, 25.6), (-5.4, 24.4), (-8.8, 21)]
          , Polygon [(4.8, 26.6), (8, 24.6), (9, 22.6), (6.2, 24.4)] ])
      , Color keSheenHi (Polygon [(-6.6, 25.4), (-3, 27), (-1.6, 26.4), (-4.6, 25.2)])
      , Color keShirt (rect (-5.4) 3.2 10.8 3.4)
      , Color figureInk (Translate 0 12 (ellipse 16 8.9 7.7))
      , Color keSkin (Translate 0 12 (ellipse 16 7.6 6.4))
      , Color keHair (Polygon ((-8.9, 19.4) : fringe ++ [(8.9, 19.4)]))
      , Color figureInk (segs 1.1 fringe)
      -- eyes half under their lids
      , Color keEye (Pictures [rect (-4.9) 9.2 2.7 2.1, rect 2.2 9.2 2.7 2.1])
      , Color figureInk (Pictures [rect (-5.1) 11.1 3.1 0.9, rect 2 11.1 3.1 0.9])
      , Color keMouth (rect (-0.7) 7 1.4 0.7)
      , blob keHair 2.4 (lock sway), blob keHair 2.4 (flipX (lock (negate sway)))
      , Color keSheen (Pictures [segs 1 [(-9, 14), (-8.9, 4)], segs 1 [(9, 14), (8.9, 4)]])
      ]

    mass = [ (-12.6, 18), (12.6, 18), (15, 12), (15.4, 2), (14 + sway, -7.6 + dip), (9.6, -5.6), (5.6, -8)
           , (1.2, -5.8), (-3.4, -8), (-7.6, -5.8), (-12 + sway, -7.6 + dip), (-15.4, 2), (-15, 12) ]
    back = Pictures
      [ frontLegs
      , figureTrunk keJacket keJacketShade
      , Color keJacketDeep (rect (-11) 5 22 1)
      , Color keBrass (rect 10.6 (-9.6) 3.8 1.7)
      , Color figureInk (Pictures [Translate 0 17 (ngon 20 14.2), segs 3 (closed mass)])
      , Color keHair (Pictures [Translate 0 17 (ngon 20 12.6), Polygon mass])
      , Color keHairDeep (Pictures
          [ segs 1 [(-2.4, 22), (-3, 8), (-2.4, -4)], segs 1 [(5, 22), (5.4, 8), (5.6, -4)]
          , segs 1 [(-9, 16), (-10.4, 4), (-10 + sway * 0.5, -4)] ])
      , Color keSheen (Pictures
          [ segs 1.3 [(-6, 25), (-6.8, 14), (-6.2, 3)], segs 1.3 [(1.5, 27), (1.2, 13), (2, 0)]
          , segs 1.3 [(7.6, 24), (8.6, 12), (8.2, 2)]
          , Polygon [(-9.6, 22.4), (-4.6, 26.4), (1.6, 27.8), (6.8, 26.4), (9.8, 22.6), (5.6, 24.4), (0.4, 25.4), (-5, 24.6)] ])
      , Color keSheenHi (Polygon [(-3.4, 26.2), (1.6, 27.2), (4.4, 26.4), (0.6, 26)])
      ]

    -- Facing left: her hair falls down her back, at +x, and one lock over her
    -- shoulder in front of the ear.
    backFall = [ (1, 24), (9, 24.6), (13.2, 18), (14.6, 8), (14.4 + trail * 0.6, -1.5 + dip * 0.5)
               , (12.2 + trail, -7.4 + dip), (8.6, -4.6), (6.2, 2), (4.6, 10) ]
    sideLock = [ (-2.2, 17.6), (0.8, 17.2), (0.6, 7), (0.2 + trail * 0.3, -1 + dip * 0.5)
               , (-1.4 + trail * 0.5, -3.6 + dip), (-2.6, 1), (-2.4, 9) ]
    sideFringe = [(-12.2, 13), (-10.4, 14.6), (-8.6, 12.4), (-6.4, 14.4), (-3.8, 15.4)]
    side = Pictures
      [ figureLegs keBoots stride (negate stride)
      , figureTrunk keJacket keJacketShade
      , Color keShirt (Polygon [(-11.2, 11), (-8.4, 11), (-9.6, -12), (-13.2, -12)])
      , Color kePiping (segs 1.1 [(-8.4, 11), (-9.6, -12)])
      , Color keJacketShade (rect (-8) (-4.6) 4.6 5), Color kePiping (rect (-8.2) 0.2 5 1)
      -- the arm, with its cuff and the brass patch
      , Color figureInk (rect (-4.4) (-10) 9 15.6)
      , Color keJacketShade (rect (-3) (-8.6) 6.2 12.8)
      , Color keJacketDeep (rect (-3) (-8.6) 6.2 1.6)
      , Color keBrass (rect (-2.4) 0.6 5 1.7)
      , blob keHair 3 backFall
      , Color keHairDeep (segs 1 [(8, 20), (10.4, 8), (10 + trail * 0.6, -3)])
      , Color keSheen (Pictures [segs 1.3 [(5, 20), (7.4, 10), (7.4, 2)], segs 1.3 [(11.2, 18), (12.4, 8)]])
      , Color figureInk (Translate 1.4 17 (ngon 20 14.2))
      , Color keHair (Translate 1.4 17 (ngon 20 12.6))
      , Color keSheen (Polygon [(-8.6, 23.4), (-3.6, 27), (2.6, 28.4), (8, 26.4), (3, 26), (-3, 24.6)])
      , Color keSheenHi (Polygon [(-2.4, 26.6), (2.6, 27.6), (4.6, 27), (0.6, 26.4)])
      , Color keShirt (rect (-9.8) 3.2 9 3.4)
      , Color figureInk (Translate (-5.4) 12.2 (ngon 16 6.9))
      , Color keSkin (Translate (-6.4) 12.2 (ngon 16 5.6))
      , Color keHair (Polygon ((-12.8, 15.4) : sideFringe ++ [(-3.4, 19.6), (-12.2, 19.6)]))
      , Color figureInk (segs 1.1 sideFringe)
      , Color keEye (rect (-10.8) 9.2 2.4 2.1), Color figureInk (rect (-11) 11.1 2.8 0.9)
      , blob keHair 2.4 sideLock
      , Color keSheen (segs 1 [(-0.8, 15), (-0.9, 4)])
      ]

-- | Where a jump would set her down: her outline, long hair and all, in pale light.
keeperEcho :: Float -> Float -> Picture
keeperEcho clock k = echoOf clock k
  [Polygon hair]
  [segs 1.6 (take 5 (drop 1 hair)), segs 1.6 (drop 6 hair ++ take 1 hair)]
  where hair = keeperBackHair 0 0

keeperHead :: Picture
keeperHead = Pictures
  [ Color (hexA 0x000000 0.5) (ngon 20 10.5)
  , Color keHairDeep (Polygon [(-9, 0), (9, 0), (10, -9), (-10, -9)])
  , Color keJacket (Polygon [(-7, -10), (7, -10), (5, -5), (-5, -5)])
  , Color keHair (ngon 20 9)
  , Color keSkin (Translate 0 (-1.8) (ngon 16 5.2))
  , Color keHair (Polygon legendFringe)
  , legendEyes
  ]

legendFringe :: [Pt]
legendFringe = [(-5.4, 1), (5.4, 1), (5, -0.6), (3, 0.2), (1, -0.8), (-1, 0.2), (-3, -0.8), (-5, 0)]

legendEyes :: Picture
legendEyes = Color (hex 0x3A2A1A) (Pictures [Translate x (-2.4) (ngon 6 0.9) | x <- [-2, 2]])

-- | An echo: a walker's body and head in pale light over a soft halo, with
-- whatever hair is their own given as fills and as edges.
echoOf :: Float -> Float -> [Picture] -> [Picture] -> Picture
echoOf clock k hairFills hairEdges = Scale 1.08 1.08 (Pictures
  [ Color (hexA 0x9CE7FF (0.16 * k)) (Translate 0 (-2) (ngon 24 (24 + 2 * sin (clock * 0.08))))
  , Color (hexA 0x0A2A33 (0.35 * k)) (Translate 0 (-20) (ellipse 16 15 6))
  , Color (hexA 0x9CE7FF (0.55 * k)) (Pictures (hairFills ++ [Polygon trunkShape, Translate 0 17 (ngon 20 12.6)]))
  , Color (hexA 0xE6FAFF (0.85 * k)) (Pictures (hairEdges ++
      [ segs 1.6 (closed trunkShape)
      , Translate 0 17 (ring 20 11.8 1.6 1)
      ]))
  ])

-- The power cell ---------------------------------------------------------------

-- | The power cell's two golds, shared by every picture of it and by the light that
-- follows its carrier: @goldCore@ is the near-white of its bright heart, @goldGlow@
-- the warm amber of the light around it.
goldCore, goldGlow :: Color
goldCore = hex 0xFFF6D2
goldGlow = hex 0xFFC24A

-- | The cell as an object: a glass capsule with brass caps. Colour comes from
-- this picture, so it reads the same on the floor and in the HUD.
--
-- @pulse@ runs from 0 to 1 and blends the glass from amber (0) to near white (1).
-- The capsule is 18 by 30 units: a dark outline, brass caps at top and bottom with a
-- highlight along the top cap, the glowing glass between them, a white glint left of
-- centre, and a small dark terminal on top. Drawn by 'powerCell' (on its cradle) and
-- 'cellIcon' (on the watch). Not exported.
cellBody :: Float -> Picture
cellBody pulse = Pictures
  [ Color (hex 0x17100B) (rectC 18 30)
  , Color (hex 0xC89C50) (Pictures [rect (-8) 9 16 5, rect (-8) (-14) 16 5])
  , Color (hex 0xF2D083) (rect (-8) 12 16 1.5)
  -- mix k a b blends from a (k = 0) to b (k = 1); see "Draw".
  , Color (mix pulse (hex 0xFFD36E) goldCore) (rectC 12 18)
  , Color (hexA 0xFFFFFF 0.85) (rect (-4) (-7) 2.5 14)
  , Color (hex 0x17100B) (Pictures [rect (-3) 14 6 2.5])
  ]

-- | The cell on its cradle, in the past: a small, bright focal point.
--
-- @clock@ is in ticks. Everything breathes with @pulse@, which goes from 0 to 1 and
-- back every 70 ticks or so (1.2 seconds). Two soft halos lie on the floor; the cell
-- itself floats above them in its own glow, bobbing, ringed by two orbits of three
-- sparks that turn opposite ways. Render draws it on its cradle's cell only while
-- the cell is still there to be taken, and only where the past is showing.
powerCell :: Float -> Picture
powerCell clock = Pictures
  -- The halos are outside the float, so the cell seems to hover over its own light:
  -- 28-sided ellipses, wider than tall, growing a little with the pulse.
  [ Color (hexA 0xFFC24A 0.14) (ellipse 28 (46 + 5 * pulse) (34 + 4 * pulse))
  , Color (hexA 0xFFC24A 0.16) (ellipse 24 (28 + 3 * pulse) (20 + 2 * pulse))
  -- The floating part: 5 units up, bobbing 2.5 either way every 2 pi / 0.05, about
  -- 126 ticks (2.1 seconds). f $ x means f x; it saves brackets round a long last
  -- argument, here the whole list below.
  , Translate 0 (5 + 2.5 * sin (clock * 0.05)) $ Pictures
      [ glow goldGlow (24 + 5 * pulse) 4
      -- The sparks: radius 22 turning clockwise 1.3 degrees a tick (78 a second),
      -- radius 16 turning back 0.8 degrees a tick (48 a second).
      , Rotate (clock * 1.3) (orbit 22)
      , Rotate (negate clock * 0.8) (orbit 16)
      , Scale 0.92 0.92 (cellBody pulse)
      ]
  ]
  where
    -- 0.5 + 0.5 sin: from 0 to 1 and back, period 2 pi / 0.09, about 70 ticks.
    pulse = 0.5 + 0.5 * sin (clock * 0.09)
    -- Three tiny squares at radius r, 120 degrees apart. A list comprehension: one
    -- picture for each angle a taken from the list after the bar.
    orbit r = Pictures [ Color (fade 0.9 goldCore) (Rotate a (Translate r 0 (rectC 2.4 2.4)))
                       | a <- [0, 120, 240] ]

-- | The crate: one object, seen at two times. Kept timber and brass that a keeper
-- could shove, then a seized block of rust. The same outline in both, raised above
-- the floor, so it reads as an obstacle in either era.
--
-- @era@ picks the look. The crate is 62 by 62 units in its 76-unit cell, with a dark
-- drop shadow 2 units right and 4 down. It is drawn as a box seen from above and a
-- little from the south: a darker front strip along the bottom 13 units, the lighter
-- top above it, and a highlight along the top's far edge. Render draws one per crate
-- in the era its cell is showing (which can differ cell by cell while the wave
-- passes), and slides it from cell to cell while it is pushed.
crate :: Era -> Picture
crate era = Pictures
  [ Color (hexA 0x000000 0.4) (Translate 2 (-4) (rectC 62 62))
  , Color edge (rectC 62 62)
  , Color front (rect (-29) (-29) 58 13)
  , Color top (rect (-29) (-16) 58 45)
  , Color shine (rect (-29) 26 58 3)
  , details
  ]
  where
    -- The box's four shades for the era: the dark rim, the front strip, the top and
    -- the lit far edge. A tuple pattern binding: the case gives back a 4-tuple and
    -- the left-hand side names its four parts at once.
    (edge, front, top, shine) = case era of
      Past    -> (hex 0x2E1C0E, hex 0x70482A, hex 0xA8763F, hex 0xD6A661)
      Present -> (hex 0x14100E, hex 0x4A2C1C, hex 0x7A4A2C, hex 0xA06A42)
    -- The marks on the top face.
    details = case era of
      -- Kept: two plank seams, a diagonal brace, a brass plate on each corner of the
      -- top face, and a nail head in each plate. A comprehension with two generators
      -- (x <- ..., y <- ...) gives every pairing of an x with a y: four nails.
      Past -> Pictures
        [ Color (hex 0x70482A) (Pictures [ rect (-29) y 58 1.5 | y <- [-1, 13] ])
        , Color (hex 0x70482A) (seg 3 (-27, -14) (27, 26))
        , Color (hex 0xE8C47A) (Pictures
            [ rect (-29) 19 9 9, rect 20 19 9 9, rect (-29) (-16) 9 9, rect 20 (-16) 9 9 ])
        , Color (hex 0x5C4122) (Pictures
            [ Translate x y (ngon 6 1.6) | x <- [-25, 24], y <- [-12, 23] ])
        ]
      -- Left to the sea: the same seams and brace in the darker rust brown, three
      -- streaks of orange rust, larger bolt heads swollen with rust (radius 2.4
      -- rather than 1.6) where the nails were, and two patches of green moss.
      Present -> Pictures
        [ Color (hex 0x4A2C1C) (Pictures [ rect (-29) y 58 1.5 | y <- [-1, 13] ])
        , Color (hex 0x4A2C1C) (seg 3 (-27, -14) (27, 26))
        , Color (hexA 0xC27A3E 0.55) (Pictures
            [ rect (-18) (-16) 4 22, rect 3 (-6) 5 26, rect 18 (-16) 3 16 ])
        , Color (hex 0x2A1A12) (Pictures
            [ Translate x y (ngon 6 2.4) | x <- [-25, 24], y <- [-12, 23] ])
        , Color (hexA 0x5F7F4E 0.85) (Translate (-14) (-17) (ellipse 8 13 5))
        , Color (hexA 0x5F7F4E 0.7) (Translate 22 22 (ellipse 7 6 4))
        ]

-- | Four corner ticks on a cell you could switch era on. Shape, not just colour.
--
-- Render draws it under a settled keeper's feet when their cell is open in both eras
-- ('crossings' in Game.Logic), so Space would shift right there; never while peeking
-- with Tab or in an ending. @era@ is the era showing on that cell: warm pale gold in
-- the past, cool blue-grey in the present, both at 60%. Each corner gets an L of two
-- arms 9 units long and 2 thick, its corner 7 units in from the cell's corner.
crossingMark :: Era -> Picture
crossingMark era = Color shade (Pictures
  -- For a corner at (x, y), each arm starts at the corner and runs inwards: signum x
  -- is 1 on the right and -1 on the left, so -(signum x) * len points back towards
  -- the centre, and min 0 turns that into where the 9-unit rectangle starts.
  [ Translate x y (Pictures [rect (min 0 (negate (signum x) * len)) (-1) len 2, rect (-1) (min 0 (negate (signum y) * len)) 2 len])
  | x <- [-corner, corner], y <- [-corner, corner] ])
  where
    -- 31 units from the centre: 7 units in from the cell's edge at 38.
    corner = half - 7
    len = 9
    shade = case era of
      Past    -> hexA 0xFFE2A8 0.6
      Present -> hexA 0x9CC7D8 0.6

-- The echo ---------------------------------------------------------------------

-- | One of the footprints the echo would walk back along. @k@ fades the older ones.
--
-- Two small soles (8-sided ellipses, radii 4 and 6) side by side, one ahead of the
-- other, both turned 10 degrees clockwise. Render passes k as 1 in the era on show
-- (0.4 in the other) times 1 - 0.15 for each step older. Only drawn in levels with
-- the echo (those using 'echoRules'), while the keeper stands still.
footprintMark :: Float -> Picture
footprintMark k = Color (hexA 0x9CE7FF (0.45 * k)) (Pictures
  [ Translate (-6) (-3) (rot 10 (ellipse 8 4 6))
  , Translate 6 4 (rot 10 (ellipse 8 4 6))
  ])

-- | The footprint an echo sent now would stop on, ringed and turning.
--
-- Also drawn, with the reflection, on the cell a mirror jump would land on. @clock@
-- is in ticks. A ring from radius 30 to 32.5 whose alpha pulses 0.55 +/- 0.25 every
-- 2 pi / 0.12, about 52 ticks, and eight ticks 12 long and 4 wide, from radius 18 out
-- to 30, the whole set turning clockwise 1.5 degrees a tick (90 a second).
landingMark :: Float -> Picture
landingMark clock = Pictures
  [ Color (hexA 0x9CE7FF (0.55 + 0.25 * sin (clock * 0.12))) (ring 28 30 2.5 1)
  -- [0, 45 .. 315] counts in steps of 45: 0, 45, 90 and so on up to 315.
  , Color (hexA 0xE6FAFF 0.9) (Pictures
      [ rot (clock * 1.5 + a) (rect 18 (-2) 12 4) | a <- [0, 45 .. 315] ])
  ]

-- | A plate with something on it, lit from below.
--
-- Render draws it on every pressure plate with something resting on it (a crate, a
-- keeper or the echo), so it is clear what holds a door open. @clock@ is in ticks. A
-- soft cyan glow of radius 30 in four layers, and a ring from radius 22 to 24 whose
-- alpha breathes 0.5 +/- 0.2 every 2 pi / 0.1, about 63 ticks.
heldPlateGlow :: Float -> Picture
heldPlateGlow clock = Pictures
  [ glow (hexA 0x9CE7FF 1) 30 4
  , Color (hexA 0xE6FAFF (0.5 + 0.2 * sin (clock * 0.1))) (ring 24 22 2 1)
  ]

-- Mirrors ----------------------------------------------------------------------

-- | A direction on screen, where up is +y.
--
-- A unit vector in board units. Its y has the opposite sign from 'dirDelta' in
-- Game.Types, where rows count down the map: up is (0, -1) as a step between cells
-- but (0, 1) on screen. Render uses it to find the edge of a cell that a mirror's
-- light leaves by, and in the ending to aim its chevrons towards the passage and to
-- set foam along the edges of the stone.
screenDir :: Dir -> (Float, Float)
screenDir d = case d of
  DirUp    -> (0, 1)
  DirDown  -> (0, -1)
  DirLeft  -> (-1, 0)
  DirRight -> (1, 0)

-- | Gloss turns clockwise, in degrees.
--
-- The turn that takes art drawn facing up to face @d@: right is a quarter turn
-- clockwise, down a half turn, left three quarters. Not exported; it turns the
-- mirrors, the jump arrow, the push hint and the doorway glow.
faceAngle :: Dir -> Float
faceAngle d = case d of
  DirUp    -> 0
  DirRight -> 90
  DirDown  -> 180
  DirLeft  -> 270

-- | The colour of a mirror's light in each era, as a hex RGB number rather than a
-- 'Color', so that each use can give it its own alpha through 'hexA'. Warm pale
-- yellow in the past, cold blue-white in the present.
beamColor :: Era -> Int
beamColor Past    = 0xFFE7B0
beamColor Present = 0xBFEFFF

-- | A standing mirror, its glass on the side it faces: a brass frame in the past,
-- tarnished steel in the present. One set in the wall fills its cell. A movable one
-- stands smaller on a timber sled with four castors, like the crates, so it reads as
-- something a keeper could shove.
--
-- @movable@: a movable mirror (n s e w on the map) rather than one set in the wall
-- (^ v < >). @era@: the era it stands in, which picks the metal and the light.
-- @face@: the way its face looks, where its light comes out. @clock@: ticks, for the
-- glint on the glass. A corner mirror is drawn by 'bendMirrorArt' instead.
mirrorArt :: Bool -> Era -> Dir -> Float -> Picture
mirrorArt movable era face clock
  -- Guards: the first condition that holds picks the result, and otherwise is
  -- simply True, so it catches everything left.
  | movable = Pictures
      -- The sled: the crate's shadow and size (62 units), a dark rim, a timber top 56
      -- units across, and a castor near each corner, a dark ring round a brass
      -- (past) or steel (present) centre.
      [ Color (hexA 0x000000 0.4) (Translate 2 (-4) (rectC 62 62))
      , Color sledEdge (rectC 62 62)
      , Color sledTop (rectC 56 56)
      , Color sledEdge (Pictures [ Translate x y (ngon 8 5) | x <- [-23, 23], y <- [-23, 23] ])
      , Color castor (Pictures [ Translate x y (ngon 8 2.5) | x <- [-23, 23], y <- [-23, 23] ])
      -- The mirror itself, shrunk to 0.74 (about 56 units) to stand on the sled.
      , Scale 0.74 0.74 (fixedMirrorArt era face clock)
      -- Which side is the face matters more once the mirror moves: the face is the
      -- one side that never pushes. A notch of its light marks it on the sled.
      -- Drawn on the up side and turned to the face: a small triangle pointing out
      -- over the sled's rim, and a thin line of light along that rim.
      , rot (faceAngle face) (Pictures
          [ Color (hexA (beamColor era) 0.95) (Polygon [(-10, 26), (10, 26), (0, 33)])
          , Color (hexA (beamColor era) 0.5) (rect (-24) 27 48 2)
          ])
      ]
  | otherwise = fixedMirrorArt era face clock
  where
    -- The sled's rim, top and castor centres: dark timber and brass in the past,
    -- near-black, faded timber and grey steel in the present.
    (sledEdge, sledTop, castor) = case era of
      Past    -> (hex 0x2E1C0E, hex 0x8A5E33, hex 0xE8C47A)
      Present -> (hex 0x14100E, hex 0x5A3A26, hex 0x8A9AA0)

-- | A flat mirror set in the wall, filling its cell. Drawn facing up, then turned to
-- @face@ by 'faceAngle'. Also the mirror on a movable mirror's sled, scaled down by
-- 'mirrorArt'. Not exported.
--
-- From the outside in: a dark shadow nearly the size of the cell, the frame (brass
-- in the past, tarnished steel in the present), the dark body below the glass with
-- two faint diagonal sheens on it, the glass as a band of the era's light along the
-- top (face) edge, a white glint on the glass, and a faint line of light spilling
-- past the glass near the cell's edge, where the beam begins.
fixedMirrorArt :: Era -> Dir -> Float -> Picture
fixedMirrorArt era face clock = rot (faceAngle face) (Pictures
  -- Shadow 2 units in from the cell's edge, frame 5 in, body 11 in (stopping 15
  -- below the top edge, to leave room for the glass).
  [ Color (hexA 0x000000 0.45) (rect (-half + 2) (-half + 2) (cs - 4) (cs - 4))
  , Color frame (rect (-half + 5) (-half + 5) (cs - 10) (cs - 10))
  , Color backing (rect (-half + 11) (-half + 11) (cs - 22) (cs - 26))
  , Color (hexA 0xFFFFFF 0.18) (Polygon [(-14, -20), (-4, -20), (8, 14), (-2, 14)])
  , Color (hexA 0xFFFFFF 0.12) (Polygon [(2, -20), (7, -20), (19, 14), (14, 14)])
  -- The glass: an 8-unit band of the era's light, 60 units long, at the face edge.
  , Color (hexA (beamColor era) 0.9) (rect (-half + 8) (half - 15) (cs - 16) 8)
  -- The glint: alpha 0.55 +/- 0.3, every 2 pi / 0.07, about 90 ticks (1.5 seconds).
  , Color (hexA 0xFFFFFF (0.55 + 0.3 * sin (clock * 0.07))) (rect (-half + 14) (half - 13) 16 3)
  -- The spill: a 3-unit line from 32 to 35 units up, past the glass.
  , Color (hexA (beamColor era) 0.35) (rect (-half + 4) (half - 6) (cs - 8) 3)
  ])
  where
    -- The frame and the dark body, per era.
    (frame, backing) = case era of
      Past    -> (hex 0xC2964F, hex 0x56676B)
      Present -> (hex 0x6E7C82, hex 0x2E3D44)

-- | A corner mirror set in the wall: the glass runs corner to corner, facing the two
-- sides it throws light out of, and the wall fills the half behind it. Drawn looking
-- north and east, then turned.
--
-- @era@ picks the metal and light, as for 'fixedMirrorArt'. @a@ and @b@ are its two
-- faces (a mirror's 'mFace' and 'mBend'), in either order. @clock@: ticks, for the
-- glass's shimmer. In the drawing, before the turn, the frame fills the cell, so
-- the lower-left half behind the glass shows as wall; a dark panel with a sheen
-- fills the upper-right triangle in front of the glass; a white line along the
-- diagonal is the glass; and the top and right edges are lit with the era's light,
-- each with a fainter spill beyond it.
bendMirrorArt :: Era -> Dir -> Dir -> Float -> Picture
bendMirrorArt era a b clock = rot turn (Pictures
  [ Color (hexA 0x000000 0.45) (rect (-half + 2) (-half + 2) (cs - 4) (cs - 4))
  , Color frame (rect (-half + 5) (-half + 5) (cs - 10) (cs - 10))
  -- The triangle with corners top left, top right and bottom right, 11 units in.
  , Color backing (Polygon [(-half + 11, half - 11), (half - 11, half - 11), (half - 11, -half + 11)])
  , Color (hexA 0xFFFFFF 0.16) (Polygon [(-6, 14), (4, 14), (14, 4), (14, -6)])
  -- The glass, top left to bottom right, its alpha shimmering 0.45 +/- 0.3 every
  -- 2 pi / 0.07, about 90 ticks.
  , Color (hexA 0xFFFFFF (0.45 + 0.3 * sin (clock * 0.07))) (seg 3 (-half + 13, half - 13) (half - 13, -half + 13))
  -- The two lit edges: the light leaves by these.
  , Color (hexA (beamColor era) 0.9) (rect (-half + 13) (half - 15) (cs - 26) 6)
  , Color (hexA (beamColor era) 0.9) (rect (half - 15) (-half + 13) 6 (cs - 26))
  , Color (hexA (beamColor era) 0.35) (rect (-half + 4) (half - 6) (cs - 8) 3)
  , Color (hexA (beamColor era) 0.35) (rect (half - 6) (-half + 4) 3 (cs - 8))
  ])
  where
    -- The clockwise turn that takes the drawn faces, up and right, to a and b.
    -- Taking min and max first puts the pair in one order (Dir derives Ord, so
    -- DirUp < DirDown < DirLeft < DirRight), so each corner needs one case:
    -- L (up, right) 0; F (down, right) 90, as a quarter turn takes up to right and
    -- right to down; 7 (down, left) 180; and the rest, J (up, left), 270.
    turn = case (min a b, max a b) of
      (DirUp, DirRight) -> 0
      (DirDown, DirRight) -> 90
      (DirDown, DirLeft) -> 180
      _ -> 270
    -- The same frame and dark panel colours as a flat mirror's.
    (frame, backing) = case era of
      Past    -> (hex 0xC2964F, hex 0x56676B)
      Present -> (hex 0x6E7C82, hex 0x2E3D44)

-- | The light a mirror throws, from its glass to the last cell it reaches.
--
-- @from@ and @to@ are board points. Render passes the middle of the mirror cell's
-- edge on its face side, and the same edge of the last cell the light reaches, so
-- the beam ends where whatever stops it begins. When the light is stopped straight
-- away the two are equal, and only the end point is drawn. The beam is three strokes
-- of the era's colour, from wide and faint to thin and bright: 34 units wide at an
-- alpha of 0.10 flickering by 0.03 (every 2 pi / 0.11, about 57 ticks), 14 wide at
-- 0.16, and a 3-unit core at 0.55. @clock@ is in ticks.
beamLight :: Era -> Float -> (Float, Float) -> (Float, Float) -> Picture
beamLight era clock from to = Pictures $
  -- ++ joins two lists: the strokes (none if the beam has no length) and the end.
  (if from == to then [] else
    [ Color (hexA c (0.10 + 0.03 * sin (clock * 0.11))) (seg 34 from to)
    , Color (hexA c 0.16) (seg 14 from to)
    , Color (hexA c 0.55) (seg 3 from to)
    ]) ++
  -- Where the light stops: on whatever is in its way, so a blocked beam still shows.
  [ at to (Pictures [ glow (hexA c 0.9) 18 3, Color (hexA 0xFFFFFF 0.8) (ngon 10 3.5) ]) ]
  where c = beamColor era

-- | A chevron beside the keeper, pointing at the mirror: this key steps through.
--
-- @d@ is the key's direction, the way to the mirror; @clock@ (ticks) bobs it. Drawn
-- above the keeper's centre and turned to @d@: 46 units out, just past the cell's
-- edge at 38, moving 3 units in and out every 2 pi / 0.2, about 31 ticks (half a
-- second). A dark chevron under a pale one, so it shows on any floor, in the cool
-- white-blue of the echo's light (the push hint is warm, so the two are not
-- confused). Render places it at the keeper, and only when the landing is open.
jumpArrow :: Dir -> Float -> Picture
jumpArrow d clock = rot (faceAngle d) (Translate 0 (46 + 3 * sin (clock * 0.2)) (Pictures
  [ Color (hexA 0x0A2A33 0.6) (Polygon [(-11, -3), (0, 9), (11, -3)])
  , Color (hexA 0xE6FAFF 0.95) (Polygon [(-9, -2), (0, 7), (9, -2)])
  ]))

-- | Beside a movable mirror's back or side: where a push would slide its sled.
-- Its outline waits on that cell, and two chevrons keep running the way it would go,
-- warm, so they are not taken for the cool arrow of a jump.
--
-- @d@: the way the push would send the mirror; @clock@: ticks. Render draws it on the
-- cell the mirror would slide to, worked out by the push rule itself ('tryStep'), so
-- it only ever shows a push that would happen.
pushHint :: Dir -> Float -> Picture
pushHint d clock = Pictures
  -- A warm square 58 units across, a little wider than the sled's top, its alpha
  -- 0.1 +/- 0.05, every 2 pi / 0.12, about 52 ticks.
  [ Color (hexA 0xFFC45C (0.1 + 0.05 * sin (clock * 0.12))) (rectC 58 58)
  -- The outline's corners: one L-shaped bracket at the lower left, turned to each of
  -- the four corners in turn.
  , Color (hexA 0xFFD27A (0.6 + 0.25 * sin (clock * 0.12)))
      (Pictures [ rot a (segs 3.5 [(-28, -14), (-28, -28), (-14, -28)]) | a <- [0, 90, 180, 270] ])
  -- The chevrons, drawn pointing up and turned to @d@. Each gives a pair of strokes,
  -- a dark 8-unit shadow under a warm 5-unit line, and concat flattens the list of
  -- pairs into one list of pictures.
  , rot (faceAngle d) (Pictures (concat
      [ [ Color (hexA 0x2A1A08 (0.55 * k)) (segs 8 [(-17, y - 9), (0, y + 6), (17, y - 9)])
        , Color (hexA 0xFFD98C k) (segs 5 [(-16, y - 8), (0, y + 6), (16, y - 8)]) ]
      | i <- [0, 1 :: Int]
      -- A let in a comprehension names values for each i. t is the fractional part
      -- of a count that grows by 1 every 40 ticks, so it runs 0 to 1 and starts
      -- again; the second chevron is 20 ticks, half a run, behind the first. Each
      -- runs from y = -20 to 12, and k = sin (pi t) fades it in and out at the ends.
      -- properFraction splits a number into whole and fractional parts; the
      -- annotation (Int, Float) fixes the type of the whole part, which nothing else
      -- here would. The clock never goes below 0, so t is never negative.
      , let t = snd (properFraction ((clock + fromIntegral i * 20) / 40) :: (Int, Float))
            y = -20 + 32 * t
            k = sin (pi * t) ]))
  ]

-- | A way on into another room, on the doorway's cell: light from the room beyond
-- spilling in over its outer edge, and chevrons drifting out through it, the way
-- @d@. @k@ is how strongly, 0 to 1; the light is the era's own, warm in the past and
-- cold in the present.
--
-- Drawn pointing up, then turned by 'faceAngle', so the drawing's top edge becomes
-- the doorway's outer edge, the one it shares with the next room. Render passes @k@
-- as 1 towards a room not yet seen and 0.35 towards one already seen, times 0.4
-- where the way is open only in the other era, and fades it with the change of
-- room; it adds 17 ticks per column to @clock@, so doorways side by side do not
-- pulse together.
doorwayHint :: Era -> Dir -> Float -> Float -> Picture
doorwayHint era d k clock = rot (faceAngle d) (Pictures (wash ++ edge ++ chevrons))
  where
    -- Warm pale gold in the past, pale cold blue in the present, as hex RGB.
    tint = if era == Past then 0xFFE2A8 else 0xD8EEF6
    -- Between 0.5 and 1, every 2 pi / 0.05, about 126 ticks (2 seconds).
    breath = 0.75 + 0.25 * sin (clock * 0.05)
    -- Seven strips 66 wide and 6 deep, stacked inwards from the outer edge (i = 0 is
    -- the outermost), each fainter than the one outside it by 1/7 of 0.3, so the
    -- light seems to spill in from beyond and fade over the first 42 units.
    wash = [ Color (hexA tint (k * breath * 0.3 * (1 - fromIntegral i / 7)))
                   (rect (-33) (half - 6 * fromIntegral (i + 1)) 66 6)
           | i <- [0 .. 6 :: Int] ]
    -- A brighter 3-unit line right on the outer edge.
    edge = [ Color (hexA tint (k * breath * 0.6)) (rect (-30) (half - 3) 60 3) ]
    -- Two chevrons, each running outwards from y = -16 to 24 over 70 ticks, the
    -- second 35 ticks behind the first; sin (pi t) fades each in and out. The same
    -- fractional-part count as in 'pushHint'.
    chevrons =
      [ Color (hexA tint (k * 0.8 * sin (pi * t))) (segs 3 [(-11, y - 7), (0, y + 2), (11, y - 7)])
      | i <- [0, 1 :: Int]
      , let t = snd (properFraction ((clock + fromIntegral i * 35) / 70) :: (Int, Float))
            y = -16 + 40 * t ]

-- | Where a reflection would land, but cannot: crossed out.
--
-- A red-orange cross, two 3-unit strokes from corner to corner of a 32-unit square,
-- inside a ring from radius 22 to 24, all at 75% alpha. Render draws it on the
-- landing cell when a keeper stands in a mirror's light but the reflection's cell is
-- taken, so that key would only walk.
blockedLanding :: Picture
blockedLanding = Color (hexA 0xFF7A4A 0.75) (Pictures
  [ segs 3 [(-16, -16), (16, 16)]
  , segs 3 [(-16, 16), (16, -16)]
  , ring 24 22 2 1
  ])

-- | The HUD's version.
--
-- @has@: whether the cell is carried. @clock@: ticks, for the same pulse as
-- 'powerCell' (about 70 ticks). Carried: the cell's body in its gold glow, without
-- the float and sparks. Not carried: only a faint outline of the capsule, 18 by 30,
-- over a fainter fill. Hud draws it in the watch's socket at 0.45 scale, and only in
-- its carried form: for a cell not yet carried it draws its own empty socket
-- (@emptyCell@ in Hud), so the second branch is not used at present.
cellIcon :: Bool -> Float -> Picture
cellIcon has clock
  | has = Pictures [glow goldGlow 26 4, cellBody (0.5 + 0.5 * sin (clock * 0.09))]
  | otherwise = Pictures
      [ Color (hexA 0xFFFFFF 0.28) (Pictures [segs 1.5 [(-9, -15), (9, -15), (9, 15), (-9, 15), (-9, -15)]])
      , Color (hexA 0xFFFFFF 0.14) (rectC 11 17)
      ]

-- | The small light that stays with the keeper once they carry the cell.
--
-- @clock@: ticks. @age@: ticks since the pickup ('fxCarried' in Game.Fx). A warm pool
-- on the floor that grows in over the first 40 ticks, and a small spark circling the
-- keeper's upper body. Render draws it where the carrier is, in a level for two
-- whichever keeper that is, and not once the level is won.
carriedLight :: Float -> Float -> Picture
carriedLight clock age = Pictures
  [ Translate 0 (-8) (pool 0xFFC879 (0.26 * grow) (100 * grow) (76 * grow))
  -- The spark's path: x = 24 cos, and y = 14 + 6 sin at twice the rate, which traces
  -- a figure of eight, once every 2 pi / 0.045, about 140 ticks (2.3 seconds).
  , Translate (24 * cos (clock * 0.045)) (14 + 6 * sin (clock * 0.09)) (Pictures
      -- Its glow's radius goes between 15 and 19 as pulse runs from -1 to 1.
      [ glow goldGlow (17 + 2 * pulse) 3
      , Color goldCore (ngon 12 5)
      , Color (hex 0xFFFFFF) (ngon 8 2.6)
      ])
  ]
  where
    -- 0 to 1 over the first 40 ticks after the pickup, eased (see 'smooth'), and
    -- then held at 1, as smooth clamps its input.
    grow = smooth (age / 40)
    -- -1 to 1, every 2 pi / 0.12, about 52 ticks.
    pulse = sin (clock * 0.12)

-- | The flash at the moment of pickup.
--
-- @age@: ticks since the pickup, from 0 until the burst ends before 40 ('burstLife'
-- in Game.Fx). A ring spreading from radius 6 to 96 as it thins from 6 units wide to
-- 2, a disc growing from radius 10 to 44 that fades faster than the rest, and ten
-- short rays, 36 degrees apart, flying out from 14 to 90 units as they shrink from
-- 12 units long to 2. Everything fades to nothing as the burst ends.
pickupBurst :: Int -> Picture
pickupBurst age = Pictures
  [ Color (fade (0.75 * (1 - t)) goldCore) (ring 24 (6 + 90 * e) (2 + 4 * (1 - t)) 1)
  -- (1 - t) squared: this disc is gone well before the ring.
  , Color (fade (0.5 * (1 - t) * (1 - t)) goldCore) (ngon 22 (10 + 34 * e))
  , Pictures
      [ Color (fade (1 - t) goldCore)
          (Rotate (fromIntegral i * 36) (Translate (14 + 76 * e) 0 (rectC (10 * (1 - t) + 2) 2)))
      | i <- [0 .. 9 :: Int] ]
  ]
  where
    -- The burst's progress, 0 to 1 over 40 ticks.
    t = clamp01 (fromIntegral age / 40)
    -- An ease-out: 1 - (1 - t)^2 starts fast and slows to a stop at t = 1, so the
    -- flash bursts out and settles.
    e = 1 - (1 - t) * (1 - t)

-- Victory ----------------------------------------------------------------------

-- | Where the lantern room sits, relative to the lighthouse cell's centre.
--
-- 34 units up, in board units. Render uses it to place the lamp's glow, its beams
-- and the ending's flare.
lanternOffset :: (Float, Float)
lanternOffset = (0, 34)

-- | The lamp burning again.
--
-- @clock@: ticks, for a slow breath of the pool (radius 72 +/- 6, every 2 pi / 0.05,
-- about 126 ticks). @k@: how lit, 0 to 1; Render eases it in over the first 24 ticks
-- after the win. Drawn relative to the lighthouse cell's centre: a warm pool, and
-- the lantern's window, a pale 17 by 13 pane with a white heart, faded in by @k@.
lanternGlow :: Float -> Float -> Picture
lanternGlow clock k = at lanternOffset (Pictures
  [ pool 0xFFD98A (0.42 * k) (72 + 6 * sin (clock * 0.05)) (72 + 6 * sin (clock * 0.05))
  , Color (fade k (hex 0xFFF1C2)) (rect (-8.5) (-6.5) 17 13)
  , Color (fade k (hex 0xFFFFFF)) (rect (-4) (-5) 8 10)
  ])

-- | Long beams turning slowly out of the lantern.
--
-- @clock@: ticks. @k@: strength from 0; it may go past 1, since Render adds the
-- ending's flare to it (and draws a second, three times faster set during the flare).
-- Six beams 60 degrees apart, turning clockwise 0.12 degrees a tick, a whole turn in
-- 3000 ticks (50 seconds). Each reaches out to 1400 units, past the canvas's edge, as
-- k eases up to 1; their alpha keeps growing with k beyond that. Drawn relative to
-- the lighthouse cell's centre.
victoryRays :: Float -> Float -> Picture
victoryRays clock k = at lanternOffset (Pictures
  [ Rotate (clock * 0.12 + fromIntegral i * 60) (beam (fromIntegral i))
  | i <- [0 .. 5 :: Int] ])
  where
    -- The beams' length: 0 to 1400 units as k eases from 0 to 1, then held there,
    -- since 'smooth' clamps its input.
    reach = 1400 * smooth k
    -- Beam number i, lying along +x before it is turned: a wide wedge whose half-width
    -- at its far end is a tenth of its reach plus 6 units for each beam number, so the
    -- six differ a little, and a narrower, brighter core at 0.045 of its reach.
    beam i = Pictures
      [ Color (hexA 0xFFD98A (0.085 * k)) (Polygon [(0, 0), (reach, -reach * 0.1 - 6 * i), (reach, reach * 0.1 + 6 * i)])
      , Color (hexA 0xFFEDBE (0.075 * k)) (Polygon [(0, 0), (reach, -reach * 0.045), (reach, reach * 0.045)])
      ]

-- | The real lighthouse, at the end of the passage: whole, banded, and burning. It
-- stands on its cell and rises about two and a half cells above it. @k@ is how
-- brightly its lamp burns, 0 to 1.
--
-- @clock@: ticks, for a slow breath of the lamp's pool (about 126 ticks). Render
-- draws it at the far end of the ending's rush. Built from the ground up: a shadow
-- and a rock footing; six bands, red and cream, on a shaft narrowing from 34 units
-- either side of its centre at the bottom to 20 at the top; its right side in shade
-- and its left edge lit by the lamp; a dark door; the gallery and its brass rail; the
-- lantern, its glass blending from dark to warm with @k@, a white heart and glazing
-- bars; the red roof with a short rod on top; and last the lamp's pool of light.
trueTower :: Float -> Float -> Picture
trueTower clock k = Pictures
  [ Color (hexA 0x000000 0.45) (Translate 5 (-25) (ellipse 28 54 18))
  , Color (hex 0x5C4634) (Translate 0 (-21) (ellipse 28 50 17))
  , Color (hex 0x8C7253) (Translate 0 (-18) (ellipse 28 46 14))
  -- Even bands red and odd bands cream, so the bottom band is red.
  , Pictures [ band i (if even i then 0xD8493B else 0xF4EADA) | i <- [0 .. 5] ]
  -- The shade, from 6 units right of the centre line (4 at the top) to the right edge.
  , Color (hexA 0x2A1A12 0.28) (Polygon [(6, bottom), (widthAt bottom, bottom), (widthAt top, top), (4, top)])
  -- The lit left edge, 10 units wide at the bottom and 6 at the top, brighter with k.
  , Color (hexA 0xFFE2A8 (0.3 * k)) (Polygon [(-widthAt bottom, bottom), (-widthAt bottom + 10, bottom), (-widthAt top + 6, top), (-widthAt top, top)])
  -- The door: 14 wide, 26 high, with a pointed top.
  , Color (hex 0x2A1A12) (Polygon [(-7, bottom), (7, bottom), (7, bottom + 20), (0, bottom + 26), (-7, bottom + 20)])
  -- The gallery floor and its rail.
  , Color (hex 0x2A1A12) (rect (-27) top 54 7)
  , Color (hex 0xC89C50) (rect (-27) (top + 5) 54 3)
  -- The lantern: a dark housing, its glass, its white heart and the glazing bars.
  , Color (hex 0x2A1A12) (rect (-16) (top + 7) 32 30)
  , Color (mix k (hex 0x3A3020) (hex 0xFFEFC4)) (rect (-13) (top + 9) 26 26)
  , Color (fade k (hex 0xFFFFFF)) (rect (-6) (top + 13) 12 18)
  , Color (hex 0x2A1A12) (Pictures [rect (-1) (top + 9) 2 26, rect (-13) (top + 21) 26 1.6])
  -- The roof, up to 60 units above the shaft's top, and the rod on it.
  , Color (hex 0xA8362E) (Polygon [(-20, top + 37), (20, top + 37), (0, top + 60)])
  , Color (hex 0x2A1A12) (rect (-1.5) (top + 60) 3 9)
  -- The lamp's light, centred on the lantern's glass and breathing by 8 units.
  , Translate 0 (top + 22) (pool 0xFFD98A (0.55 * k) (95 + 8 * sin (clock * 0.05)) (85 + 8 * sin (clock * 0.05)))
  ]
  where
    -- Where the banded shaft starts and ends, in board units above the cell's centre.
    bottom = -18 :: Float
    top = 128 :: Float
    -- Half the shaft's width at height y: a straight line from 34 at the bottom to
    -- 20 at the top.
    widthAt y = 34 - 14 * (y - bottom) / (top - bottom)
    -- Band i (0 at the bottom) of six equal bands: a trapezoid from its lower edge y0
    -- to its upper edge y1, as wide as the shaft at each. let ... in names y0 and y1
    -- for this one expression, where a where clause names things for a whole
    -- definition.
    band i colr =
      let y0 = bottom + (top - bottom) * fromIntegral (i :: Int) / 6
          y1 = bottom + (top - bottom) * fromIntegral (i + 1) / 6
      in Color (hex colr) (Polygon [(-widthAt y0, y0), (widthAt y0, y0), (widthAt y1, y1), (-widthAt y1, y1)])

-- | A lamp on the wall of the passage, dark until the keeper reaches it, @k@ 0 to 1.
--
-- In the ending's rush the lamps line both walls of the corridor, one every four
-- cells, and Render lights each one as the keeper comes within two and a half cells
-- of it. A warm pool, a dark housing, glass that blends from a cold dark to warm
-- light with @k@, and a white heart faded in by @k@.
passageLamp :: Float -> Picture
passageLamp k = Pictures
  [ pool 0xFFC879 (0.24 * k) 120 95
  , Color (hex 0x10171B) (rect (-7) (-9) 14 18)
  , Color (mix k (hex 0x3A464D) (hex 0xFFE9B0)) (rect (-4.5) (-6.5) 9 13)
  , Color (fade k (hex 0xFFFFFF)) (rect (-2) (-3) 4 6)
  ]

-- | The wall lamps, lit one by one as the warmth reaches them.
--
-- @age@: ticks since the lamp was lit (the win, or the ending's own count). @origin@:
-- the lighthouse cell's centre, in board units. Unlike most figures this one places
-- itself: it draws at every wall lamp ('sconceCells' in Station, on the south face
-- of three cells of the top wall) a lamp face and a pool which at full strength
-- matches the lamp's usual pool in the past. The warmth spreads from @origin@ at
-- 1 / 0.12, about 8.3 units a tick (500 a second), and each lamp takes 40 ticks to
-- come up once it is reached. Render draws it only on dressed levels ('lvDressed').
relitSconces :: Float -> (Float, Float) -> Picture
relitSconces age origin = Pictures
  [ at p (Pictures
      [ pool 0xFFC879 (0.17 * k) 165 125
      , Color (fade k (hex 0xFFE9B0)) (rect (-4) (-3) 8 8)
      ])
  | c <- sconceCells
  -- A tuple pattern in a let: px and py name the two halves of the cell's centre.
  -- p is 12 units above the bottom edge of the wall cell, on its south face; d is how
  -- far the warmth has to travel to it; k is how far it has come up.
  , let (px, py) = cellPos c
        p = (px, py - half + 12)
        d = dist p origin
        k = smooth ((age - d * 0.12) / 40) ]

-- | Warm light soaking outward from the lighthouse across the whole scene.
--
-- @age@: ticks since the lamp was lit. @origin@: the lighthouse cell's centre. One
-- soft pool ('pool' in Station) whose radius grows, eased, from 40 to 1220 units over
-- 110 ticks (just under 2 seconds), 0.86 as tall as it is wide.
warmthOver :: Float -> (Float, Float) -> Picture
warmthOver age origin = at origin (pool 0xFFD48A 0.2 r (r * 0.86))
  where r = 40 + 1180 * smooth (age / 110)

-- | The straight-line distance between two board points, in board units. Not
-- exported; used by 'relitSconces'.
dist :: (Float, Float) -> (Float, Float) -> Float
dist (a, b) (c, d) = sqrt ((a - c) * (a - c) + (b - d) * (b - d))

-- Ambient ----------------------------------------------------------------------

-- | Bounded drifting specks. Past: lit dust rising. Present: wind-driven spray
-- crossing the other way. A pure function of the clock.
--
-- @era@ picks dust or spray. @clock@: ticks. @k@: strength, 0 to 1; Render passes
-- each era's share of the screen while the wave crosses, so dust and spray blend,
-- and below 0.01 nothing is drawn at all. There are 26 specks over the map window
-- (mapLeft to mapRight and mapBottom to mapTop), fixed to the window rather than to
-- the cells. Each speck's starting place and speed come from 'rnd' with its number,
-- so they look scattered but are the same every frame, and a speck that drifts off
-- one edge comes back on the opposite one. Nothing is stored between frames: each
-- frame works out where every speck is from @clock@ alone.
ambient :: Era -> Float -> Float -> Picture
ambient era clock k
  | k <= 0.01 = Blank
  | otherwise = Pictures (map speck [0 .. count - 1])
  where
    -- The annotation fixes the speck numbers' type to Int, which rnd takes.
    count = 26 :: Int
    -- The window's size in board units: 13 by 9 cells, 988 by 684.
    w = mapRight - mapLeft
    h = mapTop - mapBottom
    -- v wrapped into 0 .. size, by taking off whole multiples of size; floor rounds
    -- down, so this works for negative v too. This keeps every speck in the window.
    wrapTo size v = v - size * fromIntegral (floor (v / size) :: Int)
    -- Speck number i. fx and fy are its place in the window as fractions 0 to 1, and
    -- speed is 0.6 to 1.4; the different second and third arguments to rnd keep the
    -- three numbers independent of each other.
    speck i =
      let fx = rnd i 11 7
          fy = rnd i 13 8
          speed = 0.6 + 0.8 * rnd i 17 9
      in case era of
        -- Dust rising in the lamplight: it sways 12 units side to side very slowly
        -- (every 2 pi / 0.01, about 630 ticks), rises 0.18 units a tick times its
        -- speed, and twinkles between 0.14 and 0.44 alpha (about 210 ticks); adding
        -- i to the phase puts each speck out of step with the rest. A warm square
        -- 2.4 units across.
        Past ->
          let x = mapLeft + wrapTo w (fx * w + 12 * sin (clock * 0.01 + fromIntegral i))
              y = mapBottom + wrapTo h (fy * h + clock * 0.18 * speed)
              a = 0.14 + 0.3 * (0.5 + 0.5 * sin (clock * 0.03 + fromIntegral i))
          in Translate x y (Color (hexA 0xFFE2A8 (a * k)) (rectC 2.4 2.4))
        -- Spray on the wind: it drifts left 1.5 and down 0.65 units a tick times its
        -- speed, a path about 23 degrees below the horizontal, and each speck is a
        -- 10 by 1.3 streak turned 24 degrees anticlockwise (Gloss turns clockwise, so
        -- -24) to lie along it. Each speck has its own fixed faintness.
        Present ->
          let x = mapLeft + wrapTo w (fx * w - clock * 1.5 * speed)
              y = mapBottom + wrapTo h (fy * h - clock * 0.65 * speed)
              a = 0.12 + 0.2 * rnd i 19 3
          in Translate x y (Color (hexA 0xB7CEDA (a * k)) (Rotate (-24) (rectC 10 1.3)))

-- The shift --------------------------------------------------------------------

-- | The bright front of the era wave.
--
-- @t@: how far the wave has gone, 0 to 1 across the shift's 13 ticks ('waveProgress'
-- in Game.Fx). Render centres it on the cell the shift was made on, and in the
-- ending's rush on the keeper. Two rings of 40 segments grow from radius 20 to 1070
-- units and fade out as @t@ nears 1: a sharp 3-unit edge, and a soft band 13 units
-- deep just inside it.
waveRing :: Float -> Picture
waveRing t = Pictures
  [ Color (hexA 0xFFFFFF (0.5 * (1 - t))) (ring 40 (r - 3) 3 1)
  , Color (hexA 0xFFFFFF (0.16 * (1 - t))) (ring 40 (r - 16) 13 1)
  ]
  where r = 20 + 1050 * t
