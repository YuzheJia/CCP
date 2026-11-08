-- | The station itself: one painting for each cell of each era.
--
-- Every cell is a separate picture, so the era wave can flip them one by one.
-- Ground and wall bases follow the collision map; raised details such as the
-- tower can extend past their cell's square. Walls are always solid-looking and
-- bright-capped; floors are always low
-- and quiet. The same foundations, floor inlay, carpet, conduit and doorways appear
-- in both eras: one kept, one left to the sea.
--
-- What it draws. 'cellArt' paints one map cell in one era: a floor (with its inlay,
-- cracks, sand, moss and landmarks such as a plate, the power cell's cradle or the
-- lighthouse), a wall, a pit, or one of the two opposite passages of the original
-- station. A few pieces are drawn over the cells as the game changes: a plate-worked
-- 'doorway', open or shut, a 'filledPit', and the 'lightPools' of each era's lamps.
-- Around the station it draws the sea ('seaBackdrop', 'seaLife', 'seaShade'), the
-- shelf, rocks and surf ('shore', 'farSurf'), the stone apron the station stands on
-- ('foundation') and the darkened map edges ('vignette').
--
-- What it does not do. It holds no state and decides no rules. Whether a cell is
-- wall, floor or pit comes from the level ('walkable' in "Game.Level", 'lvPits');
-- whether a door is open or a pit filled comes from the game, which "Render" reads
-- before it calls 'doorway' and 'filledPit'. Crates, mirrors, the keeper, the power
-- cell itself, the shift wave and the watch are drawn in "Figures", "Render" and
-- "Hud".
--
-- Where it sits. It uses "Draw" (shapes, colours, layout constants, noise), "Font"
-- (the stencilled room names), "Game.Level" and "Game.Types" (the 'Level' record and
-- 'walkable'). "Render" uses everything it exports but 'sconceCells'; "Figures"
-- borrows 'pool' and 'sconceCells'.
--
-- Per level and per frame. "Render.mkScene" builds a level's scene once, the first
-- time the level is drawn, and "Main" keeps it for the rest of the session. It
-- calls 'cellArt' for every cell of both eras and bakes the results ("Draw.bake"
-- multiplies a picture's transforms into its points and merges runs of one colour,
-- so thousands of small shapes cost little to draw again): one picture per cell,
-- for the shift wave, which squashes each cell edge-on and swaps it for the other
-- era's, and one per room, for every other frame. It bakes the
-- sea, shore, surf, foundation and vignette of each era the same way. Each frame
-- then only draws those baked pictures, plus the few things built afresh here:
-- 'seaLife' (it moves with the clock), 'lightPools' (it fades with the shift),
-- 'doorway' and 'filledPit' (they follow the game), and the barriers of the ending's
-- rush ('presentRubble', 'pastBulkhead').
--
-- Coordinates and units. A cell is @cs = 76@ canvas units square, and @half = 38@.
-- Cell paintings are drawn around the cell's own centre, x to the right and y up,
-- from @-half@ to @half@; the caller moves each into place with @at (cellPos c)@.
-- Map cells are @(x, y)@ with y growing down the map, while Gloss's y grows up the
-- screen, so the neighbour at @(x, y - 1)@, to the north, lies against a painting's
-- top edge (@+half@) and the one at @(x, y + 1)@ against its bottom edge. The
-- board-scale pictures ('shore', 'foundation', 'vignette', 'lightPools') use the
-- board's own coordinates, where the map's 13 x 9 frame runs from @mapLeft@ (-612)
-- to @mapRight@ (376) across and from @mapBottom@ (-342) to @mapTop@ (342) up, and
-- @cellPos (x, y)@ is the centre of cell (x, y) in them; "Draw.board" then scales
-- the board and sets it on the 1280 x 800 canvas. The sea pictures are in canvas
-- coordinates, centred on the origin, and lie under the board. Clocks are in
-- ticks, 60 a second.
--
-- Light. The station is seen from above and a little from the south: a wall shows
-- its brick south face only where open ground lies in front of it, and its shadows
-- fall deepest from the north and the west, as if lit from the north-west.
--
-- Variation. Every irregular detail (a crack, a patch of moss, a crest on the sea)
-- is placed by 'rnd', a repeatable hash of two integers and a salt that gives a
-- number in [0, 1), never by a random generator. The same cell always gets the same
-- cracks, so a picture can be baked once and look the same in every frame and every
-- run. Within one function each detail has its own salts, so its numbers do not
-- move together.
--
-- Colours are hex RGB integers (0xRRGGBB) given to 'hex', or to 'hexA' with an
-- opacity from 0 to 1; 'fade' scales a colour's opacity. Angles given to 'rot'
-- (Gloss's Rotate) are degrees, turning clockwise; angles inside the maths ('cos',
-- 'sin') are radians, turning anticlockwise from east.
module Station
  ( cellArt
  , seaBackdrop
  , seaLife
  , seaShade
  , shore
  , farSurf
  , foundation
  , pool
  , lightPools
  , sconceCells
  , filledPit
  , doorway
  , vignette
  , presentRubble
  , pastBulkhead
  ) where

import Draw
import Font
import Game.Level
import Game.Types
import Graphics.Gloss
import qualified Data.Set as Set

-- Palettes ---------------------------------------------------------------------

-- | One era's colours for the plain building: floors, walls and the conduit. Both
-- eras fill the same fields, so the drawing code is written once and only the
-- palette changes; details that belong to one era (brass, rust, moss) are written
-- where they are drawn instead.
--
-- The record declares several fields of one type at once (@a, b, c :: Color@), which
-- is the same as giving each its own line. All are Gloss 'Color's:
--
-- @pFloorA@, @pFloorB@: the two floor tones, laid like a chessboard. @pGrout@: the
-- joints between floor tiles. @pInlay@: the diamond set in each floor tile. @pCap@:
-- the top of a wall, seen from above; @pCapHi@ its lit edges (the top of every wall
-- cell, and the west side where the wall ends); @pCapLo@ its shaded edges (the
-- cap's lower edge, and the east side where the wall ends). @pFace@: a wall's brick
-- south face; @pFaceDark@ the dark foot of that face, and the line along a cap's
-- north edge where the floor beyond drops away. @pMortar@: the joints between the
-- bricks. @pShadow@: the translucent shadow a wall casts on the floor. @pPipe@,
-- @pPipeHi@: the conduit along the north wall, and the light along it. @pStencil@:
-- the translucent paint of the stencilled room names.
data Pal = Pal
  { pFloorA, pFloorB, pGrout, pInlay, pCap, pCapHi, pCapLo, pFace, pFaceDark, pMortar
  , pShadow, pPipe, pPipeHi, pStencil :: Color }
-- ^ Floor tones, grout, inlay, wall cap and face shades, mortar, shadow, conduit
-- and stencil colours; the named roles above apply in either era.

-- | The palette for an era. The past is warm: brown tiles, sandstone caps, red
-- brick and polished brass. The present is the same building gone cold: slate
-- floors, grey stone and rusted pipe. The present's shadows are a little denser
-- (opacity 0.5 against 0.42) and its stencils fainter (0.16 against 0.28), worn away.
--
-- @Pal { field = value, ... }@ is record construction syntax: each field is named,
-- so their order does not matter, and leaving one out would draw a warning.
palette :: Era -> Pal
palette Past = Pal
  { pFloorA = hex 0x5E3F2A, pFloorB = hex 0x553824, pGrout = hex 0x38231A
  , pInlay = hex 0xB98C4A
  , pCap = hex 0xC6A679, pCapHi = hex 0xE8D3A8, pCapLo = hex 0x9C7E58
  , pFace = hex 0x8E4A2E, pFaceDark = hex 0x5C2C1C, pMortar = hex 0xB57F5C
  , pShadow = hexA 0x170A05 0.42
  , pPipe = hex 0xB5853A, pPipeHi = hex 0xF2D083
  , pStencil = hexA 0xEBD3A3 0.28 }
palette Present = Pal
  { pFloorA = hex 0x2A373E, pFloorB = hex 0x253137, pGrout = hex 0x151D22
  , pInlay = hex 0x4E5D60
  , pCap = hex 0x83929A, pCapHi = hex 0xAEBBC0, pCapLo = hex 0x5D6B72
  , pFace = hex 0x3A464D, pFaceDark = hex 0x1E272C, pMortar = hex 0x56646B
  , pShadow = hexA 0x03070A 0.5
  , pPipe = hex 0x6B4630, pPipeHi = hex 0x94684A
  , pStencil = hexA 0xA9B6B8 0.16 }

-- Neighbours -------------------------------------------------------------------

-- | Whether the cell at column @x@, row @y@ is drawn as solid wall in @era@: it is
-- neither floor nor a pit. Cells off the map are in no walkable set, so they count
-- as solid and the outer walls need no special case. A door in the present and a
-- mirror set in the wall are not in the walkable set either (the parser leaves them
-- out), so they are painted as wall, and "Render" draws the 'doorway' or the mirror
-- over it. It takes x and y apart, rather than a 'Cell', because its callers ask
-- about neighbours, such as @solid level era x (y - 1)@ for the cell to the north.
solid :: Level -> Era -> Int -> Int -> Bool
solid level era x y = not (walkable level era (x, y) || isPit level era (x, y))

-- | A pit stops the player but is not a wall: it is low, and casts no wall's shadow.
-- Pits belong to the past map only, so the present never has one. A pit that a
-- crate has since filled is still a pit here, since this is the level as built;
-- "Render" draws 'filledPit' over the ones the game has filled. @Set.member@ looks
-- the cell up in a balanced tree, in O(log n) time.
isPit :: Level -> Era -> Cell -> Bool
isPit level era cell = era == Past && cell `Set.member` lvPits level

-- | Painting for a cell, in coordinates local to the cell's centre.
--
-- @level@ is the level as built (not the game being played), @era@ the era to paint
-- and @cell@ the map cell @(x, y)@. The picture fills the square from @-half@ to
-- @half@ each way, and the caller places it with @at (cellPos cell)@.
-- "Render.mkScene" calls it once for every cell of both eras and bakes the results;
-- the ending calls it again on copies of the level whose walls it has opened.
--
-- The guards are tried from the top and the first that holds decides: pits, then
-- the two opposite passages of the original station, then walls, and whatever is
-- left is floor. The passages sit at fixed cells of the original Harrow Point map,
-- (4, 2) for the door and (8, 6) for the breach. On any other map those paintings
-- only appear if that cell is open in exactly the one era they need, so they never
-- claim a passage the map does not have.
cellArt :: Level -> Era -> Cell -> Picture
cellArt level era cell@(x, y)
  -- cell@(x, y) is an as-pattern: cell names the whole pair, x and y its parts.
  -- A pit first: it is neither floor nor wall, and has its own painting.
  | isPit level era cell = pit level cell
  -- The door at (4, 2): standing open in the past, fallen in in the present.
  | era == Past && cell == door && pastOnly = pastDoor level (x, y)
  | era == Present && cell == door && pastOnly = presentRubble
  -- The breach at (8, 6): sealed in the past, torn open in the present.
  | era == Past && cell == breach && presentOnly = pastBulkhead
  | era == Present && cell == breach && presentOnly = presentBreach level
  -- Anything else that is not open in this era is wall.
  | solid level era x y = wall level era cell
  -- Open floor: the tile, then whatever lies on it. 'Pictures' draws its list in
  -- order, each picture over the ones before it, so the dressing lies on the tile.
  | otherwise = Pictures
      [ floorTile level era cell
      , floorDressing level era cell
      ]
  where
    -- The two passages' cells on the original station's map.
    door = (4, 2)
    breach = (8, 6)
    -- The painted passages say "open here, closed there", so they only appear where
    -- that is true of this map.
    pastOnly = walkable level Past cell && not (walkable level Present cell)
    presentOnly = walkable level Present cell && not (walkable level Past cell)

-- | Past only: a shaft in the floor, ringed in hazard paint. It has to read as a
-- place that cannot be walked on, and as a place something could be dropped into.
--
-- Nested squares, each drawn over the last: the floor tile (so the shadows of the
-- walls around still fall at its edges), a near-black outline 70 units wide, a band
-- of yellow hazard paint 64 wide, dark diagonal dashes across it on all four sides,
-- the black shaft 50 wide over the middle of the yellow (which leaves it a band 7
-- units wide), and a slightly lighter strip inside the shaft's top edge: its far
-- wall, seen a little from the south as every wall is.
pit :: Level -> Cell -> Picture
pit level cell = Pictures
  [ floorTile level Past cell
  , Color (hex 0x17100B) (rectC (cs - 6) (cs - 6))
  , Color (hex 0xE0B23C) (rectC (cs - 12) (cs - 12))
    -- The dashes, four on each side, centred on the yellow band (29 units out from
    -- the centre). Each is a 6 x 15 bar turned 45 degrees; the bars of the left and
    -- right sides are laid 15 x 6, so they lean the other way. The shaft drawn next
    -- covers their inner ends.
  , Color (hex 0x241610) (Pictures [ Translate o (half - 9) (rot 45 (rectC 6 15)) | o <- stripes ])
  , Color (hex 0x241610) (Pictures [ Translate o (9 - half) (rot 45 (rectC 6 15)) | o <- stripes ])
  , Color (hex 0x241610) (Pictures [ Translate (half - 9) o (rot 45 (rectC 15 6)) | o <- stripes ])
  , Color (hex 0x241610) (Pictures [ Translate (9 - half) o (rot 45 (rectC 15 6)) | o <- stripes ])
  , Color (hex 0x050302) (rectC (cs - 26) (cs - 26))
    -- The shaft's far wall: an 8-unit strip along the inside of its top edge.
  , Color (hex 0x1A100A) (rect (negate (half - 13)) (half - 21) (cs - 26) 8)
  ]
  -- Where the dashes sit along each side, in units from the middle of the side.
  where stripes = [-20, -7, 7, 20]

-- | Present only, drawn over the wall the map has there. Shut, it is a barred iron
-- gate; open, the bars are gone and the floor runs through. Its lamp agrees, but the
-- shape says it first.
--
-- @cell@ is one of the level's doors ('lvDoors'); @open@ says whether every plate is
-- held down at this moment ("Render" asks @doorsOpen@). "Render" draws it each
-- frame at @cellPos cell@, over the baked wall, wherever the present is on show. The
-- rules ('openCell' in "Game.Logic") treat the cell as floor exactly when the door
-- is open, so what is drawn follows the rule. The greys are the present palette's
-- cap colours (0x83929A, 0xAEBBC0, 0x5D6B72), so the frame reads as part of the wall.
doorway :: Level -> Cell -> Bool -> Picture
doorway level cell open
  -- Open: the floor runs through between two jambs (7-unit strips down the left and
  -- right edges), under a lighter bar across the top, and a small green lamp.
  | open = Pictures
      [ floorTile level Present cell
      , Color (hex 0x83929A) (Pictures [rect (-half) (-half) 7 cs, rect (half - 7) (-half) 7 cs])
      , Color (hex 0xAEBBC0) (rect (-half) (half - 9) cs 9)
      , Translate 0 (half - 5) (Color (hex 0x9CE0B0) (ngon 8 4))
      ]
  -- Shut: the whole cell in the frame's grey, a dark opening 62 units wide, four
  -- upright bars 6 wide and 16 apart, two cross rails, and an orange-red lamp in a
  -- dark rim. The x in the comprehension is a name for that list only.
  | otherwise = Pictures
      [ Color (hex 0x83929A) (rectC cs cs)
      , Color (hex 0x10171B) (rectC (cs - 14) (cs - 14))
      , Color (hex 0xAEBBC0) (Pictures [ rect x (-half + 7) 6 (cs - 14) | x <- [-25, -9, 7, 23] ])
      , Color (hex 0x5D6B72) (Pictures [ rect (-half + 7) y (cs - 14) 5 | y <- [-14, 12] ])
      , Translate 0 (half - 5) (Pictures
          [ Color (hex 0x10171B) (ngon 8 5.5), Color (hex 0xFF7A4A) (ngon 8 3.5) ])
      ]

-- | A plate set in the floor, in both eras, because a crate rests on it in both.
--
-- Drawn by 'floorDressing' on every cell of 'lvPlates', 50 units square: a dark rim,
-- the plate's face, a cross marked on it, and a round boss at its centre. (A
-- 12-sided 'ngon' stands in for a circle: it is one polygon, one draw call.)
-- Polished brass in the past; rusted, on near-black, in the present. The glow of a
-- plate with something resting on it is drawn over this by "Figures"
-- (@heldPlateGlow@).
plate :: Era -> Picture
plate era = Pictures
  [ Color rim (rectC 50 50)
  , Color face (rectC 42 42)
  , Color mark (Pictures [rectC 30 3, rectC 3 30])
  , Color rim (ngon 12 6)
  , Color mark (ngon 12 3.5)
  ]
  where
    -- One case gives all three colours at once, and the tuple pattern on the left
    -- takes them apart into three names.
    (rim, face, mark) = case era of
      Past    -> (hex 0x2A1A0E, hex 0xC89C50, hex 0xF2D083)
      Present -> (hex 0x10171B, hex 0x6B4630, hex 0x94684A)

-- | Past only: the same pit with a crate wedged in it, flush, and safe to walk over.
--
-- "Render" draws it each frame over the baked pit, at every cell the game has filled
-- (@gFilled@), except while the push that fills it is still under way. The same
-- outline as 'pit', then the crate's top filling the hole: brown boards with three
-- dark joints across them, and a nail head at each corner. Being level with the
-- floor, it has no face and casts no shadow.
filledPit :: Level -> Cell -> Picture
filledPit level cell = Pictures
  [ floorTile level Past cell
  , Color (hex 0x17100B) (rectC (cs - 6) (cs - 6))
  , Color (hex 0x4E3820) (rectC (cs - 12) (cs - 12))
  , Color (hex 0x2E2012) (Pictures [ rect (-half + 6) y (cs - 12) 1.5 | y <- [-17, -3, 11] ])
    -- Two generators: every x with every y, so four nails, one near each corner.
  , Pictures [ Translate x y (Color (hex 0x8A6A3F) (ngon 6 2)) | x <- [-26, 26], y <- [-26, 26] ]
  ]

-- Floors -----------------------------------------------------------------------

-- | A plain floor tile for @cell@ in @era@: its tone, grout and inlay, the present's
-- cracks and chips, and the shadows of the walls beside it. It is the base of every
-- open cell, and of a pit, the two open passages and an open 'doorway'.
--
-- The list is joined with @++@ from pieces that may be empty, so a piece that does
-- not apply simply adds nothing. @Pictures $ xs@ is @Pictures xs@: @$@ applies a
-- function to everything after it, which saves brackets around a long argument.
floorTile :: Level -> Era -> Cell -> Picture
floorTile level era (x, y) = Pictures $
  [ Color base (rectC cs cs)
    -- Grout: a firm line along the tile's top and left edges, and a fainter, thinner
    -- one along its bottom and right. Where two floor tiles meet, one's firm line
    -- and the other's faint one lie side by side, so the joint has a dark and a
    -- lighter half.
  , Color (pGrout p) (Pictures [rect (-half) (half - 1.5) cs 1.5, rect (-half) (-half) 1.5 cs])
  , Color (fade 0.5 (pGrout p)) (Pictures [rect (-half) (-half) cs 1, rect (half - 1) (-half) 1 cs])
  ] ++ inlay ++ damage ++ shadows
  where
    p = palette era
    -- The two tones alternate like a chessboard: x + y is even on one and odd on the
    -- other, so no two tiles side by side share a tone.
    base = if even (x + y) then pFloorA p else pFloorB p
    -- A repeatable number in [0, 1) for this cell and the salt s. Each detail below
    -- has its own salts, so, for instance, whether a tile has a crack does not decide
    -- where the crack starts.
    n s = rnd x y s
    -- The same inlay in both eras: a diamond at each tile's heart. The sea has
    -- taken some of them in the present.
    -- The diamond is a square turned 45 degrees: an outline 15 units square at 55%
    -- of the inlay colour, a 10-unit square of the tile's own tone inside it to
    -- hollow it out, and a small solid core. In the present 28% of tiles show only
    -- the faint empty socket; the rest keep the inlay, with a dimmer core.
    inlay
      | era == Past =
          [ Color (fade 0.55 (pInlay p)) (rot 45 (rectC 15 15))
          , Color base (rot 45 (rectC 10 10))
          , Color (pInlay p) (rot 45 (rectC 4.5 4.5))
          ]
      | n 1 < 0.28 = [Color (fade 0.5 (pGrout p)) (rot 45 (rectC 15 15))]
      | otherwise =
          [ Color (fade 0.55 (pInlay p)) (rot 45 (rectC 15 15))
          , Color base (rot 45 (rectC 10 10))
          , Color (fade 0.8 (pInlay p)) (rot 45 (rectC 4.5 4.5))
          ]
    -- Wear, in the present only: a crack and a chipped corner, each on some tiles.
    damage
      | era == Past = []
      | otherwise = crack ++ chip
    -- A crack on 62% of present tiles: a one-pixel line through four points, from a
    -- start anywhere within 30 units of the centre. 'iterate' applies the step again
    -- and again to make an endless list of points, and 'take 4' keeps the first
    -- four; being lazy, Haskell never works out the rest. n 5 and n 6 are fixed for
    -- the cell, so every step is the same, 8 to 20 units right and -14 to 12 units
    -- up: the crack is a straight line of three equal pieces. keepIn holds each point
    -- inside the tile, so a crack that would leave it runs along the edge instead.
    -- (let ... in names things for one expression only; where names them for the
    -- whole definition.)
    crack
      | n 2 < 0.62 =
          let sx = -30 + 60 * n 3
              sy = -30 + 60 * n 4
              path = take 4 (iterate (\(a, b) -> (a + 8 + 12 * n 5, b + (-14) + 26 * n 6)) (sx, sy))
          in [thin (hex 0x10171B) (map keepIn path)]
      | otherwise = []
    -- A chipped corner on 34% of present tiles: a dark, translucent 13 x 9 patch at
    -- one of the four corners (each axis by a coin toss, 72% of the way from the
    -- centre to the edge, about 27 units), turned by up to 40 degrees.
    chip
      | n 7 < 0.34 =
          let cx = (if n 8 < 0.5 then -1 else 1) * half * 0.72
              cy = (if n 9 < 0.5 then -1 else 1) * half * 0.72
          in [Color (hexA 0x10171B 0.38) (Translate cx cy (rot (40 * n 10) (rectC 13 9)))]
      | otherwise = []
    -- Clamps a point into the tile's square, -half to half on each axis.
    keepIn (a, b) = (max (-half) (min half a), max (-half) (min half b))
    -- Shadows of the walls beside the tile. These comprehensions have a condition and
    -- no generator: [x | cond] is [x] when cond holds and [] when it does not, a
    -- compact way to write an optional item. The wall to the north (y - 1, against
    -- the tile's top edge) casts the deepest: a 13-unit band, and a 9-unit band at
    -- half strength below it to soften its edge. The wall to the west casts a 9-unit
    -- band at 70%, and the wall to the east only a 5-unit line at 25%, so the light
    -- seems to come from the north-west.
    shadows =
      [ Color (pShadow p) (rect (-half) (half - 13) cs 13) | solid level era x (y - 1) ] ++
      [ Color (fade 0.5 (pShadow p)) (rect (-half) (half - 22) cs 9) | solid level era x (y - 1) ] ++
      [ Color (fade 0.7 (pShadow p)) (rect (-half) (-half) 9 cs) | solid level era (x - 1) y ] ++
      [ Color (fade 0.25 (pShadow p)) (rect (half - 5) (-half) 5 cs) | solid level era (x + 1) y ]

-- | What lies on a floor tile, drawn over 'floorTile'.
--
-- On the original station's map (@lvDressed@) it adds the hand-placed carpet,
-- seawater and stencilled room names, all at cells of that 13 x 9 map. On every map
-- it adds the present's sand and moss, and the landmarks: the lighthouse's footing
-- or tower, the power cell's cradle, the plates, and the conduit's channel. No level
-- of the campaign is dressed (a test checks this), so in play only the second group
-- appears; the dressed map survives as the tests' fixture.
floorDressing :: Level -> Era -> Cell -> Picture
floorDressing level era cell@(x, y) = Pictures $
  (if lvDressed level then carpet ++ water ++ stencil else []) ++ sand ++ moss ++ landmark
  where
    p = palette era
    n s = rnd x y s
    -- A runner down the middle hall. In the present it has rotted to strips.
    -- It runs down column 6 from row 1 to row 7, between the outer walls; the first
    -- guard turns away every other cell. Past: a red runner 34 units wide, with a
    -- gold line along each side and a gold diamond on every tile. Present: a
    -- narrower, faded strip at a wandering offset (20 to 28 wide), three dark
    -- threads running along it, and a band where it has rotted through to the floor,
    -- painted in the floor's own tone.
    carpet
      | x /= 6 || y < 1 || y > 7 = []
      | era == Past =
          [ Color (hex 0x7E2A25) (rectC 34 cs)
          , Color (hex 0xC89C50) (Pictures [rect (-14) (-half) 1.5 cs, rect 12.5 (-half) 1.5 cs])
          , Color (hex 0xC89C50) (rot 45 (rectC 7 7))
          ] ++ topShade
      | otherwise =
          [ Color (hexA 0x5A3A36 0.5) (rect (-17 + 6 * n 21) (-half) (20 + 8 * n 22) cs)
            -- Each thread is a two-point line, from 6 to 36 units above the tile's
            -- bottom edge to 4 to 34 below its top, leaning by up to 2 units. The
            -- let inside the comprehension names sx for each i: about -14, 0 and 14
            -- across, each nudged up to 4 units right.
          , thins (hexA 0x2A1E1E 0.7)
              [ [(sx, -half + 6 + 30 * n (24 + i)), (sx + 2 - 4 * n (28 + i), half - 4 - 30 * n (32 + i))]
              | i <- [0 .. 2], let sx = -14 + 14 * fromIntegral i + 4 * n (36 + i) ]
          , Color (fade 0.8 (pFloorA p)) (rect (-19) (-half + 40 * n 40) 40 (5 + 12 * n 41))
          ] ++ topShade
    -- Row 1 lies under the north wall, whose shadow the runner would otherwise
    -- cover: the shadow's darker band is drawn again over the runner, as wide as it.
    topShade = [Color (pShadow p) (rect (-17) (half - 13) 34 13) | y == 1]
    -- Sand and lichen: only in the present, only flat.
    -- Sand: on 30% of present tiles, one pale, translucent drift, 28 to 52 units
    -- long and 12 to 24 deep, within 20 units of the centre. (ellipse 10 is a
    -- ten-sided polygon.)
    sand
      | era == Present && n 30 < 0.3 =
          [ Translate (-20 + 40 * n 31) (-20 + 40 * n 32)
              (Color (hexA 0x7D7766 0.22) (ellipse 10 (14 + 12 * n 33) (6 + 6 * n 34))) ]
      | otherwise = []
    -- Moss grows at the foot of walls, where the shadows are. Along the top edge,
    -- under a wall to the north: up to four flat patches, each kept with a 70% chance.
    -- Down the left edge, beside a wall to the west: up to three upright ones, 60%
    -- each. The condition comes first in each comprehension, so a tile without that
    -- wall makes no patches at all; then i numbers the patches so each has its own
    -- salts (40 + i, 50 + i, ...).
    moss
      | era == Past = []
      | otherwise =
          [ Translate (-half + 8 + 60 * n (40 + i)) (half - 5 - 6 * n (50 + i))
              (Color (hexA 0x5F7F4E (0.5 + 0.3 * n (60 + i))) (ellipse 7 (3 + 4 * n (70 + i)) (2 + 2 * n (80 + i))))
          | solid level era x (y - 1), i <- [1 .. 4], n (90 + i) < 0.7 ] ++
          [ Translate (-half + 4 + 5 * n (100 + i)) (half - 8 - 60 * n (110 + i))
              (Color (hexA 0x5F7F4E (0.4 + 0.3 * n (120 + i))) (ellipse 7 (2 + 2 * n (130 + i)) (3 + 4 * n (140 + i))))
          | solid level era (x - 1) y, i <- [1 .. 3], n (150 + i) < 0.6 ]
    -- Seawater, gathered where the floor has sunk: near the breach and elsewhere.
    -- Five hand-picked cells of the dressed map. Each holds one translucent pool, 48
    -- to 64 units across and moved up to 6 units off centre, with a pale three-point
    -- line for the light on its surface. `elem` in backticks is the function elem
    -- written between its arguments: is cell one of the list's members?
    water
      | era == Present && cell `elem` [(9, 6), (11, 6), (7, 6), (6, 4), (1, 5)] =
          [ Translate (-6 + 12 * n 160) (-6 + 12 * n 161)
              (Pictures
                [ Color (hexA 0x4D7486 0.45) (ellipse 14 (24 + 8 * n 162) (13 + 5 * n 163))
                , thin (hexA 0xB6D2DC 0.4) [(-12, 4), (-2, 6), (9, 4)]
                ]) ]
      | otherwise = []
    -- Stencilled room names: painted fresh, then worn away letter by letter.
    -- The dressed map's three rooms, each named on a floor cell of its bottom row:
    -- the lamp room on the west, the hall in the middle, and the store of power cells
    -- on the east. In the present a lost letter is a space. The case matches the pair
    -- (cell, era) in one go, and _ catches every other pair.
    stencil = case (cell, era) of
      ((2, 7), Past)    -> [word "LAMP"]
      ((2, 7), Present) -> [word "L MP"]
      ((5, 7), Past)    -> [word "HALL"]
      ((5, 7), Present) -> [word "H  L"]
      ((10, 7), Past)   -> [word "CELLS"]
      ((10, 7), Present) -> [word "C LLS"]
      _ -> []
    -- The word s in the era's stencil paint, centred across the tile ('textC'
    -- centres a string on the origin), its baseline 10 units above the tile's bottom
    -- edge and its capitals 10 units tall.
    word s = Color (pStencil p) (Translate 0 (-half + 10) (textC 10 s))
    -- What marks this cell, if anything. The first guard that holds wins, so a cell
    -- carries one landmark at most. The goal is the lighthouse: its bare footing in
    -- the past, the dead tower in the present. Then the power cell's cradle, a
    -- plate, and on the dressed map the two cells under the conduit's downspouts,
    -- where it comes down the north wall and sinks into the floor. The case in braces,
    -- its branches split by a semicolon, is an ordinary case written on one line.
    landmark
      | cell == lvGoal level = [case era of { Past -> lampFooting; Present -> towerAt }]
      | cell == lvPowerCell level = [cradle era]
      | cell `Set.member` lvPlates level = [plate era]
      | lvDressed level && (cell == (2, 1) || cell == (10, 1)) = [channel era]
      | otherwise = []

-- | The conduit from the store to the lighthouse, sunk in the floor.
--
-- One floor cell's length of it, running straight up and down the cell: a dark
-- groove 8 units wide with the pipe in it. In the past the brass pipe (the past's
-- pPipe and pPipeHi) runs the whole cell, with a line of light along its middle,
-- and meets the downspout coming down the wall above. In the present the rusted
-- pipe is broken into two lengths, with a gap between them and another at the top,
-- under the wall. Drawn on cells (2, 1) and (10, 1) of the dressed map.
channel :: Era -> Picture
channel era = case era of
  Past -> Pictures
    [ Color (hexA 0x2A170C 0.8) (rect (-4) (-half) 8 cs)
    , Color (hex 0xB5853A) (rect (-2) (-half) 4 cs)
    , Color (hex 0xF2D083) (rect (-0.5) (-half) 1 cs)
    ]
  Present -> Pictures
    [ Color (hexA 0x10171B 0.8) (rect (-4) (-half) 8 cs)
    , Color (hex 0x6B4630) (rect (-2) (-half) 4 34)
    , Color (hex 0x6B4630) (rect (-2) 12 4 16)
    ]

-- | The ring a tower will stand on. Past: an ordinary, polished floor inlay.
--
-- Drawn on the goal cell in the past, 6 units below the cell's centre, near where
-- the tower's base stands in the present ('towerAt'). A dark oval recess; a brass
-- ring of radius 28 and a fainter one of radius 19 inside it ('ring' takes the
-- number of sides, the radius, the width and a vertical squash), both squashed to
-- 70% of their height, like the recess, so they read as circles on a floor seen at
-- a slant; and a four-pointed star of pale brass triangles, its points 16 units up
-- and down and 20 to either side.
lampFooting :: Picture
lampFooting = Translate 0 (-6) $ Pictures
  [ Color (hexA 0x2A170C 0.5) (ellipse 28 30 21)
  , Color (hex 0xC89C50) (ring 28 28 2 0.7)
  , Color (hexA 0xC89C50 0.6) (ring 24 19 1.2 0.7)
  , Color (hexA 0xF0DDB4 0.65) (Pictures [tri (-4, 0) (4, 0) (0, 16), tri (-4, 0) (4, 0) (0, -16)
                                         , tri (0, -3) (0, 3) (-20, 0), tri (0, -3) (0, 3) (20, 0)])
  ]

-- | The lighthouse in the present: same footing, tapered banded tower, dead lamp.
--
-- Drawn on the goal cell in the present, scaled up by 16% and moved 8 units down. It
-- stands taller than its cell: the roof reaches about 17 units past the cell's top
-- edge, into the cell above (which is why the ending draws the goal cell once more
-- over the floor it opens there). From the bottom up: a shadow cast to the lower
-- right, the slate footing and its ring, five bands of a tapering tower with a
-- door at its foot, and the lantern room, dark glass under a roof. No light burns in
-- it here; the lit lamp of a win is drawn by "Render" and "Figures".
towerAt :: Picture
towerAt = Translate 0 (-8) $ Scale 1.16 1.16 $ Pictures
  [ Color (hexA 0x03070A 0.5) (Translate 3 (-2) (ellipse 28 32 22))
  , Color (hex 0x4F5B60) (ellipse 28 30 21)
  , Color (hex 0x6E7C82) (ring 28 28 2.5 0.7)
  , bands
  , door
    -- The lantern is already in place: Translate 0 0 moves nothing.
  , Translate 0 0 lantern
  ]
  where
    -- The tower's foot and the base of its lantern, in the tower's own units (before
    -- the 1.16 scale). ":: Float" fixes the type of each literal, which could
    -- otherwise be any numeric type.
    bottom = -8 :: Float
    top = 34 :: Float
    -- Half the tower's width at height y: 19 at the foot, narrowing in a straight
    -- line to 11 at the top.
    widthAt y = 19 - 8 * (y - bottom) / (top - bottom)
    -- Band i of five, counting from 0 at the foot: a trapezoid over that fifth of
    -- the tower's height, its sides following widthAt, so the bands stack into one
    -- tapered shape. (i :: Int) fixes i's type, which only fromIntegral would
    -- otherwise constrain.
    band i colr =
      let y0 = bottom + (top - bottom) * fromIntegral (i :: Int) / 5
          y1 = bottom + (top - bottom) * fromIntegral (i + 1) / 5
      in Color colr (Polygon [(-widthAt y0, y0), (widthAt y0, y0), (widthAt y1, y1), (-widthAt y1, y1)])
    -- Faded red and white bands, the reds darker towards the top; then the tower's
    -- shaded east side (a translucent dark quad over its right part, from x = 5 at
    -- the foot to x = 3 at the top), a crack and a patch of moss.
    bands = Pictures
      [ band 0 (hex 0xA65842), band 1 (hex 0xC2C5BA), band 2 (hex 0x954A38)
      , band 3 (hex 0xB2B6AC), band 4 (hex 0x8A4434)
      , Color (hexA 0x10171B 0.35) (Polygon [(5, bottom), (widthAt bottom, bottom), (widthAt top, top), (3, top)])
      , thin (hexA 0x10171B 0.8) [(-9, bottom + 4), (-6, 9), (-8, 16), (-3, 25)]
      , Color (hexA 0x5F7F4E 0.8) (Translate (-12) (-5) (ellipse 8 7 4))
      ]
    -- A dark doorway with a pointed head, 8 units wide, standing on the footing.
    door = Color (hex 0x1A2024) (Polygon [(-4, -9), (4, -9), (4, 1), (0, 4), (-4, 1)])
    -- The lantern room: the gallery (a flat dark oval at the tower's top), the dark
    -- glass 16 wide and 11 tall, three glazing bars, a glint on the glass, and the
    -- roof, a triangle 20 wide and 8 tall.
    lantern = Pictures
      [ Color (hex 0x2D373C) (ellipse 16 15 5) `at'` (0, top)
      , Color (hex 0x10171B) (rect (-8) (top + 1) 16 11)
      , Color (hex 0x5E6C72) (Pictures [rect (-8) (top + 1) 1.6 11, rect 6.4 (top + 1) 1.6 11, rect (-0.8) (top + 1) 1.6 11])
      , thin (hexA 0x9FB1B8 0.7) [(-5, top + 3), (-2, top + 9)]
      , Color (hex 0x55484A) (Polygon [(-10, top + 12), (10, top + 12), (0, top + 20)])
      ]
    -- 'at' with its arguments the other way round, so it reads picture `at'`
    -- position. Backticks let any two-argument function be written between its
    -- arguments, like an operator.
    at' pic pos = at pos pic

-- | The bracket the power cell sits in. Shared by both eras.
--
-- Drawn on the power cell's cell ('lvPowerCell') in both eras. "Render" draws the
-- power cell itself over it, in the past, until it is carried away; in the present
-- the cradle is always empty. Past: a dark recess, a brass ring, four pale clamps
-- at the compass points and a dark socket. Present: the ring rusted, a dark gap
-- where the west clamp and that side of the ring have gone, and moss.
cradle :: Era -> Picture
cradle era = case era of
  Past -> Pictures
    [ Color (hexA 0x2A170C 0.65) (ellipse 22 22 16)
    , Color (hex 0xC89C50) (ring 22 19 2.5 0.72)
    , Color (hex 0xF0DDB4) (Pictures [rect (-20) (-1) 5 2, rect 15 (-1) 5 2, rect (-1) 12 2 4, rect (-1) (-16) 2 4])
    , Color (hex 0x1F120B) (ellipse 16 9 6)
    ]
  Present -> Pictures
    [ Color (hexA 0x10171B 0.65) (ellipse 22 22 16)
    , Color (hex 0x6B4630) (ring 22 19 2.5 0.72)
    , Color (hexA 0x10171B 0.9) (rect (-23) (-6) 12 12)
    , Color (hex 0x10171B) (ellipse 16 9 6)
    , Color (hexA 0x5F7F4E 0.7) (Translate 12 (-9) (ellipse 8 7 3))
    ]

-- Walls ------------------------------------------------------------------------

-- | Height of a wall's south face, in canvas units: the brick front that shows below
-- the wall's top wherever open ground lies to its south. The cap fills the rest of
-- the cell above it.
faceH :: Float
faceH = 22

-- | A wall cell: its cap, the flat top seen from above, and, where open ground lies
-- to the south, its brick face across the bottom 'faceH' units.
--
-- Every cell's cap has a lit strip along its top and a shaded strip along its lower
-- edge, so even a thick block of wall shows its cells as courses of stone. Where
-- the wall ends to the west its cap is lit down that side, and where it ends to the
-- east it is shaded, which keeps the light from the north-west. The past adds slab
-- seams and a brass stud; the present adds cracks, crumbled bites out of the cap's
-- edges, moss and rust. On the dressed map it also carries the lamp brackets and the
-- conduit along the north wall.
wall :: Level -> Era -> Cell -> Picture
wall level era (x, y) = Pictures (body ++ detail ++ (if lvDressed level then conduit else []))
  where
    p = palette era
    n s = rnd x y s
    -- The neighbour to the south (y + 1, below on screen) is open, so the wall's
    -- front face shows.
    hasFace = not (solid level era x (y + 1))
    -- Where the cap ends: above the face if there is one, else at the cell's bottom
    -- edge; and so the cap's height.
    capBottom = if hasFace then -half + faceH else -half
    capH = half - capBottom
    -- Which other neighbours are open, for the cap's edges.
    openLeft = not (solid level era (x - 1) y)
    openRight = not (solid level era (x + 1) y)
    openTop = not (solid level era x (y - 1))
    -- The cap, its lit top strip and its shaded lower edge; a lit strip down the
    -- west side and a shaded one down the east where the wall ends there; a dark
    -- line along the top where the floor to the north drops away from it (drawn over
    -- the lit strip); and the face, with its dark foot and its brickwork.
    body =
      [ Color (pCap p) (rect (-half) capBottom cs capH)
      , Color (pCapHi p) (rect (-half) (half - 3) cs 3)
      , Color (pCapLo p) (rect (-half) capBottom cs 2.5)
      ] ++
      [ Color (pCapHi p) (rect (-half) capBottom 3 capH) | openLeft ] ++
      [ Color (pCapLo p) (rect (half - 3) capBottom 3 capH) | openRight ] ++
      [ Color (pFaceDark p) (rect (-half) (half - 2) cs 2) | openTop ] ++
      [ Color (pFace p) (rect (-half) (-half) cs faceH) | hasFace ] ++
      [ Color (pFaceDark p) (rect (-half) (-half) cs 3) | hasFace ] ++
      [ brickwork | hasFace ]
    -- Mortar on the face: joints across it 8 and 15 units up split it into three
    -- courses over the dark foot, and the upright joints are 24 units apart, shifted
    -- by 12 in the middle course, so the bricks are staggered as they are laid.
    -- 'thins' draws each list of points as a one-pixel line, all in one colour.
    brickwork = thins (pMortar p)
      ( [ [(-half, -half + 8), (half, -half + 8)], [(-half, -half + 15), (half, -half + 15)] ]
        ++ [ [(bx, -half + 8), (bx, -half + 15)] | bx <- [-30, -6, 18] ]
        ++ [ [(bx, -half + 15), (bx, -half + 22)] | bx <- [-18, 6, 30] ]
        ++ [ [(bx, -half + 3), (bx, -half + 8)] | bx <- [-18, 6, 30] ] )
    -- Joints between the cap's slabs: one across the cap halfway up, and one upright
    -- in each half, at different places, so the slabs are staggered like the bricks.
    seams = thins (fade 0.7 (pCapLo p))
      [ [(-half, capBottom + capH * 0.5), (half, capBottom + capH * 0.5)]
      , [(-half * 0.25, capBottom + capH * 0.5), (-half * 0.25, half)]
      , [(half * 0.45, capBottom), (half * 0.45, capBottom + capH * 0.5)]
      ]
    -- The bracket of a wall lamp, low on the wall's face, on the dressed map's
    -- 'sconceCells' only (the first guard turns every other wall away). Past: a dark
    -- plate with the brass lamp on it; its light is drawn by 'lightPools'. Present:
    -- the plate, with only a jagged shard of the lamp left on it.
    bracket
      | not (lvDressed level && (x, y) `elem` sconceCells) = []
      | era == Past = [ Color (hex 0x3A2413) (rect (-8) (-half + 4) 16 15)
                      , Color (hex 0xC89C50) (rect (-6) (-half + 6) 12 11) ]
      | otherwise   = [ Color (hex 0x10171B) (rect (-8) (-half + 4) 16 15)
                      , Color (hex 0x6B4630) (Polygon [(-6, -half + 6), (5, -half + 6), (-2, -half + 17)]) ]
    -- Detail for the era. Past: the slab seams and one brass stud on the cap.
    -- Present: the seams, then a crack across the cap on 75% of walls (one
    -- four-point shape, moved as a whole by up to 20 units across and 5 up or down),
    -- then the bites, moss and rust below.
    detail = bracket ++ case era of
      Past -> [seams, Color (hex 0x8C6A3A) (Translate (half * 0.7) (half * 0.55) (ngon 6 2.2))]
      Present ->
        [ seams ] ++
        [ thin (hex 0x2E383D) (map (\(a, b) -> (a + 40 * (n 200 - 0.5), b + 10 * (n 201 - 0.5)))
            [(-14, half - 3), (-7, 16), (-12, 5), (0, -6)]) | n 202 < 0.75 ] ++
        -- Bites out of the cap's edge, where the top course has crumbled.
        -- Up to two (60% each), anywhere across the cell's 76 units, on the cap's top
        -- edge or its lower edge (a coin toss): an irregular four-cornered piece in
        -- the cap's shade colour, straddling that edge.
        [ Color (pCapLo p) (Translate (-half + 76 * n (210 + i)) (if n (220 + i) < 0.5 then half else capBottom)
            (Polygon [(-7 - 8 * n (230 + i), 0), (9 + 7 * n (240 + i), 0), (3, 3 + 5 * n (250 + i)), (-4, -3 - 5 * n (255 + i))]))
        | i <- [1 .. 2], n (260 + i) < 0.6 ] ++
        -- Up to two flat patches of moss (70% each) just above the cap's lower edge.
        [ Color (hexA 0x5F7F4E 0.75) (Translate (-half + 76 * n (270 + i)) (capBottom + 6 * n (280 + i))
            (ellipse 7 (4 + 5 * n (290 + i)) (2 + 2 * n (300 + i))))
        | i <- [1 .. 2], n (310 + i) < 0.7 ] ++
        -- On half the walls with a face, a rust stain 10 to 20 units tall rising from
        -- the face's foot.
        [ Color (hexA 0x6B4630 0.75) (rect (-half + 76 * n 320) (-half) (2.5 + 2 * n 321) (10 + 10 * n 322))
        | hasFace, n 323 < 0.5 ]
    -- The conduit along the north wall, joining cell store to lighthouse.
    -- Dressed map only: along row 0 from column 2 to column 10, across the middle of
    -- each wall cell. Past: a dark casing, the brass pipe, a line of light along it
    -- and a collar on every cell. Present: the bare rusted pipe, 34 units short in
    -- columns 5, 6 and 9, where it has broken. Both end in a downspout.
    conduit
      | y == 0 && x >= 2 && x <= 10 = case era of
          Past ->
            [ Color (hex 0x5C4122) (rect leftEnd (-5) (rightEnd - leftEnd) 11)
            , Color (pPipe p) (rect leftEnd (-3) (rightEnd - leftEnd) 8)
            , Color (pPipeHi p) (rect leftEnd 2 (rightEnd - leftEnd) 1.5)
            , Color (pPipeHi p) (rect (-4) (-5) 8 12)
            ] ++ downspout
          Present ->
            [ Color (pPipe p) (rect leftEnd (-3) (rightEnd - leftEnd - 34 * brokenBit) 7)
            , Color (pPipeHi p) (rect leftEnd 1 (rightEnd - leftEnd - 34 * brokenBit) 1)
            ] ++ downspout
      | otherwise = []
    -- The pipe starts and ends at the centres of its end cells, where it turns down
    -- the wall; elsewhere it runs edge to edge.
    leftEnd = if x == 2 then 0 else -half
    rightEnd = if x == 10 then 0 else half
    -- 1 in the cells where the present's pipe has broken, else 0: a number, so it
    -- can switch the 34-unit gap on or off by multiplying.
    brokenBit = if x `elem` [5, 6, 9] then 1 else 0
    -- At both ends the pipe turns down the wall, from the cell's middle to its bottom
    -- edge, where 'channel' carries it on into the floor below.
    downspout
      | x == 2 || x == 10 =
          [ Color (pPipe p) (rect (-3) (-half) 7 half)
          , Color (pPipeHi p) (rect (-1) (-half) 1.5 half) ]
      | otherwise = []

-- The two opposite passages ----------------------------------------------------

-- | Past (4,2): a working door, standing open. The floor runs straight through,
-- brightly lit, so it reads as a way on rather than an object.
--
-- Drawn by 'cellArt' at (4, 2) in the past, when that cell is open in the past only.
-- On the original map the way through runs west to east, so the thresholds are at
-- the left and right edges and the jambs to the north and south.
pastDoor :: Level -> Cell -> Picture
pastDoor level cell = Pictures
  [ floorTile level Past cell
    -- A wash of warm light over the whole opening.
  , Color (hexA 0xFFD78F 0.16) (rectC cs (cs - 6))
    -- Worn path through the doorway.
    -- It is the hall's red runner, laid west to east, 30 units wide, with its gold
    -- lines and diamond.
  , Color (hex 0x7E2A25) (rect (-half) (-15) cs 30)
  , Color (hex 0xC89C50) (Pictures [rect (-half) 13 cs 1.5, rect (-half) (-14.5) cs 1.5])
  , Color (hex 0xC89C50) (rot 45 (rectC 7 7))
    -- Brass thresholds at both faces of the wall.
  , Color (hex 0xE8C47A) (Pictures [rect (-half) (-half + 3) 4 (cs - 6), rect (half - 4) (-half + 3) 4 (cs - 6)])
    -- Door leaf folded back against the north jamb.
    -- A green panel 9 units deep across the top, its lit edge, and a brass knob.
  , Color (hex 0x2F5A4E) (rect (-half + 4) (half - 11) (cs - 8) 9)
  , Color (hex 0x4C8A78) (rect (-half + 4) (half - 5) (cs - 8) 2)
  , Color (hex 0xE5C583) (Translate 24 (half - 7) (ngon 8 2.2))
    -- Lip of the south jamb, so the opening has an edge.
  , Color (hex 0x38231A) (rect (-half) (-half) cs 3)
  ]

-- | Present (4,2): the same doorway, fallen in. Solid: as bright as the wall.
--
-- Drawn by 'cellArt' at (4, 2) in the present, when that cell is open in the past
-- only, and exported because the ending's rush uses it for the present's barriers.
-- The ground is the present cap's shade colour, with cap-coloured strips along the
-- top and bottom where the wall still stands on either side; over it, seven stones,
-- the fallen lintel across them, and a scrap of the brass threshold.
presentRubble :: Picture
presentRubble = Pictures
  [ Color (hex 0x5D6B72) (rectC cs cs)
  , Color (hex 0x83929A) (rect (-half) (half - 11) cs 11)
  , Color (hex 0x83929A) (rect (-half) (-half) cs 11)
  , Color (hex 0x151D22) (rect (-half) (-half + 11) cs 3)
  , Color (hex 0x151D22) (rect (-half) (half - 14) cs 3)
    -- Each stone as (x, y, width, height, turn in degrees).
  , Pictures (map stone
      [ (-25,  16, 21, 15,  12), ( 8,  20, 24, 16, -10), ( 27,  6, 17, 16,  22)
      , (-28, -10, 17, 18, -18), ( -6,  -1, 25, 20,   6), ( 20, -18, 24, 15,  -6)
      , (-12, -22, 20, 13,  14) ])
    -- The lintel, snapped and lying across the heap.
    -- A beam 68 units long, turned 24 degrees anticlockwise (a negative angle turns
    -- Gloss's Rotate the other way), with a dark outline, a lit top and a crack.
  , Translate 2 2 (rot (-24) (Pictures
      [ Color (hex 0x151D22) (rect (-34) (-7) 68 13)
      , Color (hex 0x5B4B40) (rect (-34) (-5) 68 11)
      , Color (hex 0x7A6858) (rect (-34) 3 68 2.5)
      , thin (hex 0x2E2520) [(-4, -5), (0, 0), (-3, 6)]
      ]))
    -- What is left of the brass threshold, at the west edge.
  , Color (hex 0xC89C50) (rect (-half) (-half + 13) 3 9)
  ]
  where
    -- One stone: a dark shadow 2 units right and 3 down, the stone, a lit top edge
    -- and a shaded bottom edge, all turned together. The tuple pattern in the
    -- argument takes the five numbers apart.
    stone (x, y, w, h, a) = Translate x y (rot a (Pictures
      [ Color (hex 0x151D22) (Translate 2 (-3) (rectC w h))
      , Color (hex 0x8A99A0) (rectC w h)
      , Color (hex 0xB0BDC2) (rect (-w / 2) (h / 2 - 3) w 3)
      , Color (hex 0x5D6B72) (rect (-w / 2) (-h / 2) w 3)
      ]))

-- | Past (8,6): the same gap, sealed by a bolted bulkhead. Solid.
--
-- Drawn by 'cellArt' at (8, 6) in the past, when that cell is open in the present
-- only, and by the ending's rush for the past's barriers. A frame of light stone,
-- brighter than the past's wall caps, so it reads as solid; a dark recess; a green
-- steel door with a lit top edge, yellow hazard stripes along its foot, two panel
-- seams and four rivets down each side; and a brass locking wheel at its centre.
pastBulkhead :: Picture
pastBulkhead = Pictures
  [ Color (hex 0xD3B487) (rectC cs cs)
  , Color (hex 0xF0DDB4) (rect (-half) (half - 3) cs 3)
  , Color (hex 0x241610) (rect (-half + 6) (-half + 6) (cs - 12) (cs - 12))
  , Color (hex 0x3F5F58) (rect (-half + 9) (-half + 9) (cs - 18) (cs - 18))
  , Color (hex 0x5F8A80) (rect (-half + 9) (half - 12) (cs - 18) 3)
  , Pictures (map chevron [-20, -8, 4, 16])
  , Color (hex 0x241610) (rect (-half + 9) (-5) (cs - 18) 1)
  , Color (hex 0x241610) (rect (-half + 9) 8 (cs - 18) 1)
  , Pictures [ Translate rx ry (Color (hex 0xC6D3CC) (ngon 6 2.2))
             | rx <- [-26, 26], ry <- [-26, -10, 12, 26] ]
    -- Locking wheel at the heart of the door.
    -- A dark disc of radius 11, a brass ring of radius 7.5 (not squashed: 1), and two
    -- spokes crossed at 45 degrees either way.
  , Color (hex 0x241610) (ngon 14 11)
  , Color (hex 0xD8A94C) (ring 14 7.5 2.5 1)
  , Color (hex 0xD8A94C) (Pictures [rot 45 (rectC 15 2.2), rot (-45) (rectC 15 2.2)])
  ]
  where
    -- One hazard stripe at x: a parallelogram 6 units wide and 10 tall, leaning
    -- right, from 27 to 17 units below the centre.
    chevron x = Translate x (-22) (Color (hex 0xE0B23C)
      (Polygon [(-3, -5), (3, -5), (6, 5), (0, 5)]))

-- | Present (8,6): sea erosion has torn the bulkhead out. Walkable, with torn
-- stubs of wall at both jambs and the wet floor running straight through.
--
-- Drawn by 'cellArt' at (8, 6) in the present, when that cell is open in the
-- present only. It takes the level only for the floor tile, which it always draws
-- for (8, 6), the one cell it is used on. On the original map the way runs west to
-- east, so the jambs are the cell's top and bottom edges. Over the floor: a drift of
-- sand, a pool of seawater with a glint, the torn stubs, and what is left of the
-- door.
presentBreach :: Level -> Picture
presentBreach level = Pictures
  [ floorTile level Present (8, 6)
  , Color (hexA 0x7D7766 0.28) (ellipse 12 26 12)
  , Color (hexA 0x4D7486 0.5) (Translate 10 (-6) (ellipse 14 22 10))
  , thin (hexA 0xB6D2DC 0.45) [(0, -3), (12, -1), (24, -4)]
    -- Torn wall: dark stubs under the face above, broken cap edge below.
    -- At the top, a dark band with three jagged teeth hanging from it. At the
    -- bottom, three teeth of the wall's cap rising from a cap-coloured strip, each
    -- drawn first in near-black a little higher, which leaves a dark rim along it.
  , Color (hex 0x1E272C) (Pictures
      [ rect (-half) (half - 7) cs 7
      , tri (-half, half - 7) (-20, half - 7) (-28, half - 20)
      , tri (-14, half - 7) (6, half - 7) (-5, half - 24)
      , tri (10, half - 7) (half, half - 7) (24, half - 17)
      ])
  , Color (hex 0x151D22) (Pictures
      [ tri (-half, -half + 8) (-14, -half + 8) (-24, -half + 21)
      , tri (-8, -half + 8) (16, -half + 8) (6, -half + 24)
      , tri (20, -half + 8) (half, -half + 8) (30, -half + 19)
      ])
  , Color (hex 0x83929A) (Pictures
      [ rect (-half) (-half) cs 7
      , tri (-half, -half + 7) (-14, -half + 7) (-24, -half + 18)
      , tri (-8, -half + 7) (16, -half + 7) (6, -half + 21)
      , tri (20, -half + 7) (half, -half + 7) (30, -half + 16)
      ])
    -- What is left of the door: a rusted corner, and bent bars.
    -- The corner at the bottom right; the bars hang from the top, 2 units thick.
  , Color (hex 0x6B4630) (Polygon [(half - 3, -half + 7), (half - 3, -half + 22), (half - 19, -half + 7)])
  , Color (hex 0x8A5A3C) (seg 2 (-28, half - 8) (-20, half - 26))
  , Color (hex 0x8A5A3C) (seg 2 (20, half - 8) (26, half - 24))
  ]

-- Outside the walls ------------------------------------------------------------

-- | Sea filling the canvas, drawn once per era. It is drawn at the canvas's own
-- scale, under the board, so its crests sit anywhere the board leaves open, except
-- along the bottom edge where the keys are written.
--
-- The deep water fills the whole 1280 x 800 canvas, and 120 short crests are
-- scattered over it by 'rnd'. A crest is kept only where the board does not cover
-- it in play: left or right of the stone apron with 30 units to spare, above it with
-- 8, or below it with 8 but above the bottom 38 units, where the keys are written.
-- The apron is placed where the board stands in play (@onBoard 1@); while a level is
-- introduced the board stands smaller and further right, and the water it then
-- leaves open on the left has no crests. "Render" bakes one for each era.
seaBackdrop :: Era -> Picture
seaBackdrop era = Pictures $
  [ Color deep (rectC canvasW canvasH) ] ++
  -- Crest i at a place in the canvas: across from -640 to 640, and down from 400.
  -- The let inside the comprehension names px and py for each i, and the last line
  -- is a condition that drops the crests the board would cover.
  [ Translate px py (crest i)
  | i <- [0 .. 119 :: Int]
  , let px = canvasW * rnd i 3 1 - canvasW / 2
        py = canvasH / 2 - canvasH * rnd i 4 2
  , px < left - 30 || px > right + 30 || py > top + 8 || (py < bottom - 8 && py > keysTop) ]
  where
    -- The apron's lower-left and upper-right corners on the canvas: 12 units of rim
    -- beyond the map, and 17 below it, which takes in the apron's drop shadow
    -- ('foundation').
    (left, bottom) = onBoard 1 (mapLeft - 12, mapBottom - 17)
    (right, top) = onBoard 1 (mapRight + 12, mapTop + 12)
    -- The top of the strip kept clear for the keys, 38 units above the canvas's
    -- bottom edge.
    keysTop = negate canvasH / 2 + 38
    -- Past: deep teal. Present: nearly black.
    deep = case era of
      Past -> hex 0x114552
      Present -> hex 0x0A1418
    -- A pale, translucent line for the crests.
    crestC = case era of
      Past -> hexA 0xA8E0D8 0.32
      Present -> hexA 0x7D93A0 0.28
    -- The present's sea runs higher and longer.
    -- A crest is a four-point line, a ripple shaped like a lazy S: half-length w and
    -- rise lift. Past: half-lengths 14 to 40, rising 2. Present: half-lengths 18 to
    -- 54, rising 2 to 7.
    crest i =
      let (w, lift) = case era of
            Past -> (14 + 26 * rnd i 5 3, 2)
            Present -> (18 + 36 * rnd i 5 3, 2 + 5 * rnd i 6 4)
      in thin crestC [(-w, 0), (-w / 3, lift), (w / 3, -lift / 2), (w, 0)]

-- | The sea moving, at the canvas's scale, under the board. In the past the lamp is
-- lit: the real lighthouse's beam sweeps by now and then, and the sun glints off
-- the water to the east. In the present there is no beam, only rain.
--
-- @clock@ is the ambient clock in ticks (60 a second). "Render" draws this every
-- frame between the baked sea and 'seaShade', so the board covers whatever of it
-- lies under the board. There is one equation for each era: the constructor in the
-- first argument picks which runs.
seaLife :: Era -> Float -> Picture
seaLife Past clock = Pictures [beam, glints]
  where
    -- From the real lighthouse, off the bottom left, turning once every ten
    -- seconds: in sight for about two of them.
    -- (-900, -580) lies beyond the canvas's lower-left corner (-640, -400). turn is
    -- the beam's angle in radians, a whole turn (2 pi) every 600 ticks. Seen from
    -- there the canvas spans angles of about 7 to 75 degrees, a fifth of a turn,
    -- hence about two seconds.
    (ox, oy) = (-900, -580)
    turn = 2 * pi * clock / 600
    -- Two wedges, one over the other: a wide faint one (7.5 degrees either side of
    -- the centre line, at 9%) and a narrow brighter core (3.4 degrees, at 20%).
    beam = Pictures [ wedge 7.5 0.09, wedge 3.4 0.2 ]
    -- One wedge, 2400 units long, cut into eight lengths so it can fade with
    -- distance: length i is a quad between radii r0 and r1, at alpha times
    -- (1 - i / 8), so the far end is an eighth as bright as the near. h is the
    -- half-angle in radians; this inner where belongs to wedge alone, so h can use
    -- wedge's argument.
    wedge halfDeg alpha = Pictures
      [ Color (hexA 0xFFEFC4 (alpha * (1 - fromIntegral i / 8)))
          (Polygon [ around r0 (turn - h), around r1 (turn - h), around r1 (turn + h), around r0 (turn + h) ])
      | i <- [0 .. 7 :: Int]
      , let r0 = 2400 * fromIntegral i / 8
            r1 = 2400 * fromIntegral (i + 1) / 8 ]
      where h = halfDeg * pi / 180
    -- The point at distance r and angle t (radians) from the real lighthouse.
    around r t = (ox + r * cos t, oy + r * sin t)
    -- Sixty glints of sun on the east half of the water, each a short bright dash
    -- 2 to 7 units long. x is 640 times rnd to the power 0.6: a power under 1 pushes
    -- numbers in [0, 1) towards 1, so the glints crowd towards the east edge. Each
    -- twinkles: sin (clock * 0.06 + phase) runs a cycle every 2 pi / 0.06, about 105
    -- ticks or 1.75 seconds, and 0.5 + 0.5 * sin ... maps it to 0 to 1, so the
    -- opacity swings between 35% and 100% of the glint's own base (0.25 to 0.7). The
    -- phases, 0 to 6 radians, keep them out of step.
    glints = Pictures
      [ Color (hexA 0xF4FBF6 (base * (0.35 + 0.65 * (0.5 + 0.5 * sin (clock * 0.06 + 6 * rnd i 9 5)))))
          (rect x y (2 + 5 * rnd i 8 5) 1.6)
      | i <- [0 .. 59 :: Int]
      , let x = canvasW / 2 * (rnd i 7 5 ** 0.6)
            y = canvasH * rnd i 6 5 - canvasH / 2
            base = 0.25 + 0.45 * rnd i 5 5 ]
-- Rain over the whole canvas: 120 streaks 1.2 units wide and 12 to 20 long, all in
-- one translucent colour (one Color node for the lot). Each falls 6 to 10 units a
-- tick and drifts left by 0.4 of that; the streak is turned 22 degrees clockwise,
-- which lines it up with that slant (atan 0.4 is about 21.8 degrees). A streak
-- starts at a fixed place in a 1320 x 840 field, moves with the clock and wraps
-- round; the field reaches 20 units past the canvas on every side, so a streak
-- wraps out of sight.
seaLife Present clock = Color (hexA 0xB7CEDA 0.2) (Pictures
  [ Translate x y (Rotate 22 (rectC 1.2 (12 + 8 * rnd i 5 8)))
  | i <- [0 .. 119 :: Int]
  , let speed = 6 + 4 * rnd i 2 8
        x = wrapTo 1320 (1320 * rnd i 1 8 - clock * speed * 0.4) - 660
        y = wrapTo 840 (840 * rnd i 4 8 - clock * speed) - 420 ])
  -- v modulo size for a Float, always in [0, size) even when v is negative: v less
  -- as many whole sizes as fit at or below it ('floor' rounds towards minus
  -- infinity, where 'truncate' would round towards zero).
  where wrapTo size v = v - size * fromIntegral (floor (v / size) :: Int)

-- | The canvas's edges, darkened, so the eye rests in the middle.
--
-- Six frames, 24 to 144 units deep (@[24, 48 .. 144]@ counts in steps of 24), each
-- 5.5% black. They overlap, so the darkness builds in steps towards the edge: about
-- 29% in the outer 24 units (1 - 0.945^6), down to 5.5% from 120 to 144 units in.
-- "Render" bakes it once and draws it over the sea and 'seaLife', under the board.
seaShade :: Picture
seaShade = Color (hexA 0x000000 0.055) (Pictures [ edges d | d <- [24, 48 .. 144] ])
  where
    -- One frame d units deep: strips down the left and right and across the bottom
    -- and top. The strips overlap at the corners, which come out a little darker.
    edges d = Pictures
      [ rect (-canvasW / 2) (-canvasH / 2) d canvasH
      , rect (canvasW / 2 - d) (-canvasH / 2) d canvasH
      , rect (-canvasW / 2) (-canvasH / 2) canvasW d
      , rect (-canvasW / 2) (canvasH / 2 - d) canvasW d
      ]

-- | Around the stone apron, on the board's own scale so it moves with the board:
-- the shelf the station stands on, a few rocks, and surf where the sea meets the
-- stone. Calm in the past; rougher and greyer in the present.
--
-- "Render" bakes one for each era and draws it on the board, under the apron. The
-- near line of surf is part of it; the far line is 'farSurf', kept apart.
shore :: Era -> Picture
shore era = Pictures [ shelf, rocks, surfLayer era False ]
  where
    (x0, y0, x1, y1) = apron
    -- The shelf's colour, and the opacity of each of its layers.
    (tint, a) = case era of
      Past -> (hex 0x2A8088, 0.07)
      Present -> (hex 0x1E3440, 0.06)
    -- The shallow water over the shelf: seven rounded rectangles, each 13 units
    -- further out than the last (out to 91), with corners rounded to 0.9 of that,
    -- all faint. They pile up, so the water is lightest next to the stone and fades
    -- out into the deep.
    shelf = Color (fade a tint) (Pictures
      [ roundRect (x0 - e) (y0 - e) (x1 + e) (y1 + e) (e * 0.9) | i <- [1 .. 7 :: Int], let e = 13 * fromIntegral i ])
    -- A rock's colours: its body, its lit top, and the foam round it, with the
    -- foam's opacity.
    (body, cap, foam, foamA) = case era of
      Past -> (hex 0x6B5642, hex 0x9A7F60, hex 0xF3EBD8, 0.4)
      Present -> (hex 0x1A2226, hex 0x3A464D, hex 0xC9D4D8, 0.35)
    -- Two rocks off the east side of the apron, a large one and one at 65% size.
    rocks = Pictures [ rock (x1 + 52) 62 1, rock (x1 + 31) 34 0.65 ]
    -- A rock at (x, y), at scale s: twelve dashes of foam round an oval 38s wide
    -- and 26s deep, centred 2s below the rock, each dash covering 0.32 radians of
    -- every 30 degrees (0.52 radians), so the ring is broken; then the rock's
    -- outline as one polygon, and its lit top as a smaller one. The where below
    -- belongs to rock, so ring' can use s.
    rock x y s = Translate x y (Pictures
      [ Color (fade foamA foam) (Pictures
          [ plainSeg 1.6 (ring' t) (ring' (t + 0.32)) | j <- [0 .. 11 :: Int], let t = fromIntegral j * pi / 6 ])
      , Color body (Polygon [ (px * s, py * s) | (px, py) <- [(-14,-4),(-10,6),(-2,11),(8,9),(15,2),(13,-7),(2,-10),(-9,-9)] ])
      , Color cap (Polygon [ (px * s, py * s) | (px, py) <- [(-10,6),(-2,11),(8,9),(4,4),(-6,2)] ])
      ])
      -- A point on the foam's oval, at angle t in radians.
      where ring' t = (19 * s * cos t, -2 * s + 13 * s * sin t)

-- | The outer line of surf, drawn apart so it can breathe in and out.
--
-- "Render" bakes one for each era and, each frame, scales it about the map's centre
-- by up to 0.35% either way, on a cycle of about three seconds, so the far line
-- seems to come and go.
farSurf :: Era -> Picture
farSurf era = surfLayer era True

-- | The stone apron's outline, on the board.
--
-- (left, bottom, right, top) in board units: the map plus 12 units of stone rim to
-- the west, north and east, and 15 to the south (the stone's own rim is 12 all
-- round; see 'foundation'). The shelf, the rocks and the surf are laid out round it.
apron :: (Float, Float, Float, Float)
apron = (mapLeft - 12, mapBottom - 15, mapRight + 12, mapTop + 12)

-- | Short curling strokes and flecks along each side of the apron: never a line.
--
-- @far@ picks the outer line (sparser, further out, thinner and fainter) or the near
-- one (close to the stone, with flecks of spray). The present's surf is rougher: its
-- strokes run longer and stand further out. Each stroke sets its own opacity with
-- 'fade' on the line's colour.
surfLayer :: Era -> Bool -> Picture
surfLayer era far = Color colour (Pictures (concat (zipWith along [0 ..] sides)))
  where
    (x0, y0, x1, y1) = apron
    -- The foam's colour; the line's strength (the near line about twice the far);
    -- and roughness, which scales the random part of each stroke's length and offset.
    (colour, strength, rough) = case era of
      Past -> (hex 0xF3EBD8, if far then 0.28 else 0.6, 1)
      Present -> (hex 0xC9D4D8, if far then 0.24 else 0.5, 1.6)
    -- Each side, walked with the sea on its outer hand: start, end, outward normal.
    -- West going up, north going east, east going down, south going west: clockwise
    -- round the apron. zipWith along [0 ..] sides numbers them 0 to 3 (zipWith stops
    -- at the end of the shorter list, so the endless [0 ..] is safe), and concat
    -- joins their lists of strokes into one.
    sides = [ ((x0, y0), (x0, y1), (-1, 0)), ((x0, y1), (x1, y1), (0, 1))
            , ((x1, y1), (x1, y0), (1, 0)), ((x1, y0), (x0, y0), (0, -1)) ]
    -- 0 for the near line and 1 for the far, so the two take different noise.
    layer = if far then 1 else 0 :: Int
    -- Walks side number @side@, laying strokes along it until it runs out. A
    -- function bound in a where can carry its own type signature, as here. It starts
    -- with stroke 0 a random 0 to 12 units along the side.
    along :: Int -> ((Float, Float), (Float, Float), (Float, Float)) -> [Picture]
    along side ((ax, ay), (bx, by), (nx, ny)) = go 0 (12 * r 0 0)
      where
        -- The side's length; the (2 :: Int) exponent is typed so that -Wall does not
        -- warn about it defaulting to Integer.
        len = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
        -- The unit vector along the side.
        (ux, uy) = ((bx - ax) / len, (by - ay) / len)
        -- The north side, whose surf is drawn at 55% of the others' strength.
        topSide = ny > 0
        -- Noise for stroke i's property k. The side is spread out by 977 and the
        -- layer by 31, so no two sides or layers share numbers.
        r i k = rnd (side * 977 + i) (layer * 31 + k) 7
        -- Stroke i, starting t units along the side: the stroke, its fleck if it
        -- has one, then the rest of the side, recursively, stopping at the side's
        -- end. ':' and '++' both group to the right, so this is
        -- stroke : (fleck ++ go ...).
        go :: Int -> Float -> [Picture]
        go i t
          | t >= len = []
          | otherwise = stroke : fleck ++ go (i + 1) (t + seg' + gap)
          where
            -- The stroke's length along the side: 9 to 31 units near and 6 to 20
            -- far, the random part 1.6 times as long in the present.
            seg' = (if far then 6 else 9) + r i 1 * (if far then 14 else 22) * rough
            -- The gap before the next stroke: 4 to 14 near, 14 to 40 far.
            gap = (if far then 14 else 4) + r i 2 * (if far then 26 else 10)
            -- How far out from the stone: 3 to 9 near, 13 to 23 far, the random part
            -- again 1.6 times as large in the present.
            off = (if far then 13 else 3) + r i 3 * (if far then 10 else 6) * rough
            -- Opacity: the line's strength times 0.45 to 1.2, less on the north
            -- side. It never passes 0.72, so the 'min 1' where it is used is only a
            -- safeguard.
            alpha = strength * (0.45 + 0.75 * r i 4) * (if topSide then 0.55 else 1)
            -- Stroke width: 1.1 far, 1.4 to 2.6 near (the else takes the whole sum).
            width = if far then 1.1 else 1.4 + 1.2 * r i 5
            -- How far the stroke curls, out to sea or in towards the stone: 1.5 to 4
            -- units, either way by a coin toss.
            bulge = (1.5 + 2.5 * r i 6) * (if r i 7 < 0.5 then 1 else -1)
            -- Start: t along the side and off out from it. End: seg' further along.
            -- Control point: the midpoint, pushed out along the normal by bulge.
            (sx, sy) = (ax + ux * t + nx * off, ay + uy * t + ny * off)
            (ex, ey) = (sx + ux * seg', sy + uy * seg')
            (mx, my) = ((sx + ex) / 2 + nx * bulge, (sy + ey) / 2 + ny * bulge)
            -- Five points on the quadratic Bezier curve from start to end, pulled
            -- towards the control point: q s = (1 - s)^2 start + 2 (1 - s) s control
            -- + s^2 end, for s from 0 to 1. Halfway along, the curve has come half
            -- the way to the control point, so its middle stands bulge / 2 off the
            -- straight line.
            curvePts = [ q s | s <- [0, 0.25, 0.5, 0.75, 1] ]
            q s = ( (1 - s) * (1 - s) * sx + 2 * (1 - s) * s * mx + s * s * ex
                  , (1 - s) * (1 - s) * sy + 2 * (1 - s) * s * my + s * s * ey )
            -- The curve as four straight pieces: zipWith pairs each point with the
            -- next (the list against itself without its head).
            stroke = Color (fade (min 1 alpha) colour) (Pictures (zipWith (plainSeg width) curvePts (drop 1 curvePts)))
            -- Spray: on the near line only, after 35% of strokes, a small dot 3 to 8
            -- units further out than the stroke's end, at 80% of its opacity.
            fleck
              | far || r i 8 >= 0.35 = []
              | otherwise =
                  let d = 3 + 5 * r i 9
                  in [ Color (fade (min 1 (alpha * 0.8)) colour)
                         (Translate (ex + nx * d) (ey + ny * d) (ngon 6 (0.8 + 0.9 * r i 10))) ]

-- | A rectangle with rounded corners, as one convex polygon.
--
-- From (x0, y0), lower left, to (x1, y1), upper right, with corners of radius @rad@.
-- The outline runs anticlockwise from the top-right corner: each corner is a quarter
-- circle of seven points, from angle a0 to a0 + 90 degrees in steps of 15, about a
-- centre @rad@ in from the corner, and the straight edges join one corner's last
-- point to the next corner's first. @rad@ must be at most half the width and the
-- height, or the corners cross. Used for the layers of the shelf in 'shore'.
roundRect :: Float -> Float -> Float -> Float -> Float -> Picture
roundRect x0 y0 x1 y1 rad = Polygon
  (corner (x1 - rad) (y1 - rad) 0 ++ corner (x0 + rad) (y1 - rad) 90
   ++ corner (x0 + rad) (y0 + rad) 180 ++ corner (x1 - rad) (y0 + rad) 270)
  where
    -- The quarter circle about (cx, cy) starting at a0 degrees, turned to radians
    -- for cos and sin.
    corner cx cy a0 = [ (cx + rad * cos t, cy + rad * sin t)
                      | i <- [0 .. 6 :: Int], let t = (a0 + 15 * fromIntegral i) * pi / 180 ]

-- | The stone apron the station stands on. Dressed blocks, then eroded ones.
--
-- In board units: the map with a 12-unit rim of stone round it, and its shadow on
-- the sea, the same size, 5 units lower. Past: warm stone with joints cut across
-- the rim. Present: the same joints in cold slate, and notches the sea has bitten
-- out of its north and south edges. "Render" bakes one for each era and draws it
-- under the cells.
foundation :: Era -> Picture
foundation era = Pictures $
  [ Color (hexA 0x000000 0.4) (rect (mapLeft - 12) (mapBottom - 17) (mapRight - mapLeft + 24) (mapTop - mapBottom + 24))
  , Color stone (rect (mapLeft - 12) (mapBottom - 12) (mapRight - mapLeft + 24) (mapTop - mapBottom + 24))
  ] ++ blocks ++ erosion
  where
    stone = case era of
      Past -> hex 0x8C7253
      Present -> hex 0x3A464D
    joint = case era of
      Past -> hex 0x5C4634
      Present -> hex 0x1E272C
    -- Joints across the rim, one block every 38 units (half a cell): along the north
    -- rim from the map's west edge, along the south rim shifted by 19 so the blocks
    -- do not line up, and up the west rim; the east rim has none. [a, b .. c] counts
    -- from a in steps of b - a, as far as c.
    blocks = [ thin joint [(x, mapTop + 12), (x, mapTop)] | x <- [mapLeft, mapLeft + 38 .. mapRight] ]
          ++ [ thin joint [(x, mapBottom - 12), (x, mapBottom)] | x <- [mapLeft + 19, mapLeft + 57 .. mapRight] ]
          ++ [ thin joint [(mapLeft - 12, y), (mapLeft, y)] | y <- [mapBottom, mapBottom + 38 .. mapTop] ]
    -- Present only, in the present's sea colour: 17 notches along the south edge,
    -- each a triangle 20 to 48 units wide at the edge reaching 4 to 10 units in, and
    -- 13 along the north edge, 16 to 40 wide and 3 to 8 deep, spread by 'rnd' along
    -- the apron's whole width.
    erosion = case era of
      Past -> []
      Present ->
        [ Color (hex 0x0A1418) (Translate (mapLeft - 12 + (mapRight - mapLeft + 24) * rnd i 1 9) (mapBottom - 12)
            (Polygon [(-10 - 14 * rnd i 3 9, -1), (10 + 14 * rnd i 4 9, -1), (2, 4 + 6 * rnd i 2 9)]))
        | i <- [0 .. 16 :: Int] ] ++
        [ Color (hex 0x0A1418) (Translate (mapLeft - 12 + (mapRight - mapLeft + 24) * rnd i 5 9) (mapTop + 12)
            (Polygon [(-8 - 12 * rnd i 7 9, 1), (8 + 12 * rnd i 8 9, 1), (2, -3 - 5 * rnd i 6 9)]))
        | i <- [0 .. 12 :: Int] ]

-- | Wall lamps, as cells whose south face carries one.
--
-- Cells of the dressed map's north wall (row 0), over the lamp room, the hall and
-- the store. 'wall' puts a bracket on their face, 'lightPools' lights them in the
-- past, and "Figures" relights them one by one at a win on that map.
sconceCells :: [Cell]
sconceCells = [(1, 0), (6, 0), (11, 0)]

-- | A soft oval built from many faint layers, so no edge shows.
--
-- @c@ is a colour as hex RGB (0xRRGGBB), @strength@ the summed layer alpha, and
-- @rx@, @ry@ the oval's radii, in the units of wherever it is drawn. It is nine
-- 28-sided ovals, from a ninth of the full size up to the full size, each at a
-- ninth of the strength. A point lies inside every oval larger than its distance
-- from the centre. With ordinary alpha blending the centre opacity is
-- 1 - (1 - strength / 9)^9, approaching strength for faint pools. It falls
-- towards strength / 9 at the rim as fewer layers overlap.
-- Used for the lamps' light here, and for many glows and flares in "Figures" and
-- "Render".
pool :: Int -> Float -> Float -> Float -> Picture
pool c strength rx ry = Pictures
  [ Color (hexA c (strength / fromIntegral layers)) (ellipse 28 (rx * f) (ry * f))
  | i <- [1 .. layers], let f = fromIntegral i / fromIntegral layers ]
  -- Nine layers; each is one polygon, so a pool costs nine draw calls.
  where layers = 9 :: Int

-- | Light falling on the floor at a strength from 0 to 1. Warm lamps in service;
-- in the present, only thin daylight at the breach.
--
-- @k@ is how strongly this era's light shows: 1 while the era is on show, 0 while
-- it is not, and in between while a shift's wave crosses the map ("Render" passes
-- its pastK and presentK, which add up to 1), so one era's light fades as the
-- other's comes. In board units; drawn every frame, over the cells.
--
-- The equations are tried in order, and one whose guard fails falls through to the
-- next. First, a light too faint to see draws nothing, so it costs no draw calls.
-- Then the plain levels (every level of the campaign) have one pool each: warm over
-- the power cell's cradle in the past, cold over the lighthouse in the present.
-- Last, the dressed map's own lights.
lightPools :: Level -> Era -> Float -> Picture
lightPools _ _ k | k <= 0.01 = Blank
lightPools level Past k | not (lvDressed level) =
  at (cellPos (lvPowerCell level)) (pool 0xFFC879 (0.17 * k) 165 125)
lightPools level Present k | not (lvDressed level) =
  at (cellPos (lvGoal level)) (pool 0x9CC7D8 (0.14 * k) 120 90)
-- The dressed past: a warm pool under each wall lamp, a smaller one in the open
-- doorway at (4, 2), and the lamps themselves, small bright squares.
lightPools _ Past k = Pictures $
  [ at (sconcePos c) (pool 0xFFC879 (0.17 * k) 165 125) | c <- sconceCells ] ++
  [ at (cellPos (4, 2)) (pool 0xFFC879 (0.13 * k) 70 56) ] ++
  [ at (sconcePos c) (Color (hexA 0xFFE9B0 (0.95 * k)) (rect (-4) (-3) 8 8)) | c <- sconceCells ]
  where
    -- Where a wall lamp hangs: on the wall's face, 12 units above the cell's bottom.
    sconcePos (x, y) = let (px, py) = cellPos (x, y) in (px, py - half + 12)
-- The dressed present: cold daylight in the breach at (8, 6), and three fainter
-- patches of it elsewhere, each turned 14 degrees.
lightPools _ Present k = Pictures
  [ at (cellPos (8, 6)) (pool 0x9CC7D8 (0.2 * k) 90 62)
  , at (cellPos (2, 5)) (rot 14 (pool 0x8FB2C2 (0.08 * k) 70 42))
  , at (cellPos (6, 3)) (rot 14 (pool 0x8FB2C2 (0.07 * k) 56 36))
  , at (cellPos (10, 5)) (rot 14 (pool 0x8FB2C2 (0.07 * k) 62 40))
  ]

-- | Darkened map edges, to hold the eye on the player.
--
-- Five frames just inside the map's 13 x 9 frame, 10 to 50 units deep, each very
-- faint; where they overlap the darkness adds up, so the outer 10 units are the
-- darkest. Each frame is a strip across the top and the bottom and one down each
-- side; concat joins the five frames' lists of strips into one. The present's is
-- nearly twice as dark (0.04 against 0.022 a layer). "Render" bakes one for each era
-- and draws it on the board over the cells and everything on them.
vignette :: Era -> Picture
vignette era = Color shade (Pictures (concat
  [ [ rect mapLeft (mapTop - d) w d
    , rect mapLeft mapBottom w d
    , rect mapLeft mapBottom d h
    , rect (mapRight - d) mapBottom d h ]
  | d <- [10, 20, 30, 40, 50] ]))
  where
    w = mapRight - mapLeft
    h = mapTop - mapBottom
    shade = case era of
      Past -> hexA 0x1A0B04 0.022
      Present -> hexA 0x020608 0.04
