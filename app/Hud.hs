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
--
-- What this module draws:
--
--   * The watch itself, every frame ('hud' in play, 'hudStopped' in an ending).
--   * While a level is introduced, a column on the left: the watch at full size,
--     the level's place in the hall, its name, a line about the era, and every key
--     with its price ('introColumn').
--   * In play, the line of keys and prices under the board, in the first two levels
--     only, and the way out (Z and R) lit in every level when time runs short.
--   * Under the watch in play: who is who in a level for two, a small map of the
--     rooms in a level of several rooms, and whether the doors are open.
--   * Over the board: the caption above the map, the hint band under it, the plates
--     that explain a refused shift or push, and the pause, win and loss plates.
--
-- What it deliberately does not do: it decides nothing about the game. It reads the
-- 'Game' (the rules' state), the 'Level' and the 'Fx' (decorative state that
-- "Game.Fx" works out by comparing each tick's game with the one before), and never
-- changes them. Which hint line to show, which refusal plate is up and how strongly,
-- and how far the pause, win and loss plates have faded in are all decided in
-- "Render", which passes the answer in as a string, a cell or an opacity. Where the
-- board stands on the screen belongs to "Draw" ('board', 'onBoard'); this module
-- only asks where the board's edges land.
--
-- Fed by: "Game.Types" (the game's data), "Game.Logic" (the jump on offer, the state
-- of the doors, who carries the cell, and the words of refusal, victory and
-- failure), "Game.Fx" (the cost floating off the keeper, the room on screen, the
-- rooms seen, how far the screen has closed in), "Draw" (shapes, colours, easing,
-- the board's place), "Font" (the stroke lettering) and "Figures" (the keepers'
-- coat colours and the power cell). Used by: "Render" only. Its 'mkScene' builds
-- one 'HudArt' per level; its 'frame' draws 'hud' every frame of a level and its
-- 'endingFrame' draws 'hudStopped'; the captions, the hint band, the plates and the
-- cost text are drawn from its 'onTheBoard', 'overlays' and the ending.
--
-- Coordinates and units:
--
--   * Canvas units. The screen is a canvas of 1280 x 800 ('canvasW', 'canvasH'),
--     with the origin at its centre and y growing upward, as everywhere in Gloss.
--     The watch, the introduction's column, the keys line and what stands under the
--     watch are drawn straight onto the canvas, outside the board's transform. They
--     use 'onBoard' to find where a point of the board lands on the canvas at a
--     stand: 0 while the level is introduced, 1 in play.
--   * Board units. Everything in the section "Over the board" is drawn in the
--     board's own coordinates, where the 13 x 9 map spans 'mapLeft' to 'mapRight'
--     across and 'mapBottom' to 'mapTop' up, one cell being cs = 76 units. "Render"
--     draws these inside 'board', which scales and moves them with the map. A 'Cell'
--     is (column, row) with row 0 at the top, so on the map y grows downward while
--     on the canvas it grows upward; 'cellPos' converts one to the other.
--   * Watch units. The watch is drawn about its own centre at scale 1, where the
--     case is 62 units in radius, then moved and scaled as a whole: 1.45 while the
--     level is introduced, 1 in play.
--   * Clock angles. Around the watch an angle is in degrees, clockwise from twelve
--     ('clockAt'), and a whole turn of 360 degrees is the level's whole budget.
--   * Time. "Seconds" here are seconds of a level's time budget, which only actions
--     spend, not seconds of real time. Animation runs on ticks, 60 a second; the
--     @clock@ argument is 'fxClock' as a 'Float', a count of ticks.
--   * Colours are written as hex RGB: 'hex' 0xRRGGBB is opaque, 'hexA' 0xRRGGBB a
--     has an opacity a from 0 to 1, and 'fade' k multiplies a colour's opacity by k.
--
-- Per frame, 'hud' reads from the 'Game' the time left, how much of the action under
-- way has still to drain off the reading, any jump on offer and any shift refused
-- for want of time, and from the 'Fx' the cost that is floating, the room on screen
-- and how far the screen has closed in, and builds the light around the ring from
-- them. Everything that never changes during a level (the case, the face, the
-- plate, the column, the keys) is drawn once per era by 'mkHudArt' and baked
-- ('bake' multiplies a picture's moves and scales into its points and groups runs of
-- one colour, so Gloss draws it cheaply), and a frame only lays those pictures down.
--
-- A Gloss 'Picture' is plain data: a tree of shapes ('Polygon', 'Line'), groups
-- ('Pictures' [...]) and wrappers that move ('Translate' x y), scale ('Scale' sx sy),
-- turn ('Rotate' degrees, clockwise) or colour ('Color' c) what is inside them; in a
-- group, later pictures are drawn over earlier ones. Every function here is pure and
-- only builds such a tree, and 'Blank' is the picture of nothing.
module Hud
  ( HudArt            -- the art that never changes, one per level, kept in Render's scene
  , mkHudArt          -- builds it, in Render's 'mkScene'
  , hud               -- the watch in play, every frame (Render's 'frame')
  , hudStopped        -- the same during an ending (Render's 'endingFrame')
  , darkCaption       -- the caption once the lamp has gone out, in an ending
  , endedPlate        -- the plate at the start of an ending
  , weightedC         -- centred lettering at a chosen weight; also the ending's card
  , mapCaption        -- the caption above the map, baked per era by 'mkScene'
  , mapCaptionFaded   -- the same while the screen closes in
  , wonCaption        -- the caption once the lamp burns
  , hintBand          -- the one line under the map
  , blockedPlate      -- a refused shift: a wall in the other era
  , tooCostlyPlate    -- a refused shift: not enough time
  , pushBlockedPlate  -- a refused push: the crate would end up in a wall
  , pausedScreen      -- the three plates Render's 'overlays' lays over the map
  , wonScreen
  , lostScreen
  , secondsText       -- "42 s"; exported, though only this module uses it now
  , costText          -- "+3" or "-2"; also the cost floating off the keeper
  , costColor         -- green for time given back, orange for time spent; likewise
  ) where

-- 'isJust' asks whether a 'Maybe' holds a value. "Draw" gives the shapes, colours,
-- easing and the board's place on the canvas; "Font" the stroke lettering; "Figures"
-- the keepers' coat colours and the power cell's icon (Coat (..) imports the type
-- with its constructor and fields); "Game.Fx" the decorative state; "Game.Logic" the
-- questions the rules answer and their words; "Game.Types" the game's data;
-- "Graphics.Gloss" the 'Picture' type and its constructors. "Data.Set" is imported
-- qualified, as Set, because names such as null and map would clash with the
-- Prelude's list functions: Set.null is the set's own.
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
--
-- One is built per level by 'mkHudArt' when "Render" makes that level's scene
-- ('mkScene'), and it stays in the scene for as long as the game runs. Each field
-- that takes an 'Era' holds two finished, baked pictures and only picks one of them,
-- so nothing here is drawn again frame after frame. 'artBack' and 'artFront' are in
-- watch units, about the watch's centre, and 'watchHud' moves and scales them with
-- the rest of the watch; every other picture is in canvas units, already in place.
--
-- This is a record: each field name is also a function that reads that field, so
-- @artBack art@ is the case's art held in @art@.
data HudArt = HudArt
  { artIntro   :: Era -> Picture  -- ^ the column beside the board while the level is introduced
                                  --   ('introColumn'); fades out as the screen closes in
  , artBack    :: Era -> Picture  -- ^ the case, behind the light
                                  --   ('caseBack': shadow, crown, rim, channel, hours)
  , artFront   :: Era -> Picture  -- ^ the face, the era's plate and the socket, over it
                                  --   ('caseFront'); drawn after the light, so the face
                                  --   covers the inner edge of the light's glow
  , artBeside  :: Era -> Picture  -- ^ under the watch: who is who, in a level for two
                                  --   ('duoLegend'); 'Blank' in a level for one
  , artKeys    :: Era -> Picture  -- ^ the keys under the board
                                  --   ('keyStrip', every key drawn plain)
  , artKeysLow :: Era -> Picture  -- ^ the same when time is short, the way out lit
                                  --   (Z and R hot; see @lowKeys@ in 'mkHudArt')
  , artTeach   :: Bool            -- ^ whether this level writes its keys at all
                                  --   (only the first two levels of the hall do; "Main"
                                  --   decides, and passes it through 'mkScene')
  }

-- | The colours of the lettering and rules around the watch in one era: everything
-- that is not the watch's own metal, such as the column, the keys, the line under
-- the number, the legend for two and the room map. The four fields have one type,
-- so they share one signature (each still gets its own reading function):
--
--   * 'skInk': the main lettering (the number, the letters on the keys, the words
--     beside the keys in the column, the door note, PAUSED).
--   * 'skDim': quieter lettering (the level's place, the era's line, the words in
--     the keys line under the board, the notes).
--   * 'skRule': ruled lines, the rims of key caps, the hour marks and the room map.
--   * 'skAccent': what should catch the eye (the level's name, a jump on offer, the
--     room on screen).
data Skin = Skin { skInk, skDim, skRule, skAccent :: Color }
-- ^ Main ink, dim ink, ruled lines and highlighted ink, as Gloss colours.

-- | The 'Skin' of an era: warm in the past (cream, tan, brown, amber), cold in the
-- present (pale blue-grey, grey, slate, sea blue).
--
-- A function on an 'Era' is written as one equation per constructor, and Haskell
-- uses the equation whose pattern matches. The 'Skin' is built positionally: with
-- no field names, the arguments fill ink, dim, rule and accent in the order the
-- fields were declared.
skin :: Era -> Skin
skin Past    = Skin (hex 0xF3E2BE) (hex 0xB89E76) (hex 0x8A6A3F) (hex 0xE8B85C)
skin Present = Skin (hex 0xD6E0E2) (hex 0x8A9AA0) (hex 0x4F5B60) (hex 0x9CC7D8)

-- | The watch's materials. In the past it is polished brass and its light is the
-- lamp's; in the present it is corroded steel and its light is cold.
--
-- Unlike 'Skin', which colours the lettering around the watch, this colours the
-- watch itself. 'caseOf' gives the one for each era. When time runs short the
-- light's colours give way to embers (see 'emberA'), but the metal stays.
data Case = Case
  { caTop, caMid, caBot, caEdge, caTrack, caFaceIn, caFaceOut :: Color
    -- ^ The case's metal, lit from above ('fromAbove'): 'caTop' at twelve, 'caMid'
    --   at about three and nine o'clock, 'caBot' at six. 'caEdge' is the dark
    --   outline round the case, the crown, the face, the plate and the socket.
    --   'caTrack' is the dark channel the light runs in. 'caFaceOut' is the face at
    --   its rim and 'caFaceIn' at its centre, which is lighter.
  , caLightA, caLightB, caGlow :: Color
    -- ^ The light in the channel: 'caLightA' at twelve and 'caLightB' at six,
    --   blended between by 'fromAbove'; 'caGlow' is the soft glow round it and the
    --   colour of the bead at its end.
  , caGlowK :: Float
    -- ^ How strong the glow is, 0 to 1: strong in the past, where the light is the
    --   lamp's, faint in the present.
  , caPlate, caPlateHi, caPlateLo, caPlateInk :: Color
    -- ^ The era's plate under the case: its face, the bright line along its top,
    --   the shadow line along its bottom (and its two rivets), and its lettering.
  , caName :: String
    -- ^ The word on the plate: PAST or PRESENT.
  }

-- | The materials of each era's watch.
--
-- Written with record construction syntax, @Case { field = value, ... }@, which
-- names every field, so their order does not matter. The past: brass running from
-- pale gold at the top to dark bronze at the bottom, a near-black brown outline,
-- warm golden light and a glow at three quarters strength, and a brass plate with
-- dark lettering. The present: steel from grey to near black, cold blue-white light
-- with a glow at about a third strength, and a dark steel plate with pale lettering.
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
--
-- 'emberA' (pale coral) and 'emberB' (deep orange-red) take the place of the era's
-- two light colours, and 'emberB' also colours the glow, the breathing halo round the
-- case and the dashed outline of the time a refused shift lacked. 'emberInk' writes
-- the line under the number when it warns about time. 'rustC' is not an ember: it
-- is the rust on the present's case and plate, kept here with the other colours the
-- two eras share. One signature can declare several names of the same type.
emberA, emberB, emberInk, rustC :: Color
emberA = hex 0xFFC2AE
emberB = hex 0xFF6B4A
emberInk = hex 0xFF8A66
rustC = hex 0x7A4A32

-- Layout -----------------------------------------------------------------------
--
-- Where the watch and the introduction's column stand on the canvas. These numbers
-- are canvas units, except the radii, which are watch units (the watch at scale 1).

-- | While a level is introduced the watch stands large at the head of a column
-- left of the board, over the level's name and its keys. Once the keeper acts it
-- shrinks into the margin left of the board, its crown level with the board's
-- top edge. Each place is (across, up, scale), the scale of the sizes below.
--
-- Across, the watch is halfway between the canvas's left edge (-canvasW / 2, that is
-- -640) and the board's left edge, taken 12 units outside the map ('mapLeft' - 12)
-- and placed on the canvas by 'onBoard' at each stand, so it is centred in the
-- margin: about -447 while introduced (stand 0, the board smaller and to the right)
-- and about -563 in play (stand 1). Up, it stands at 250 and then 281; its scale is
-- 1.45 and then 1. 'negate' is used because Haskell reads a minus sign between
-- arguments as subtraction: a negative argument needs brackets, as in (-3), or
-- 'negate'. 'fst' takes the first part of a pair, here the x of a point.
introWatch, playWatch :: (Float, Float, Float)
introWatch = ((negate canvasW / 2 + fst (onBoard 0 (mapLeft - 12, 0))) / 2, 250, 1.45)
playWatch = ((negate canvasW / 2 + fst (onBoard 1 (mapLeft - 12, 0))) / 2, 281, 1)

-- | Where the watch stands, and how large, part way between its two places.
--
-- @f@ runs from 0 while the level is introduced to 1 in play, already eased: it is
-- the same value "Render" gives the board's stand, so the watch and the board move
-- together. Each of the three numbers is interpolated on its own ('lerp' f a b is
-- the value f of the way from a to b). The @let ... in@ names values for use inside
-- one expression (a @where@, by contrast, belongs to a whole equation), and its
-- tuple patterns take each triple apart into three names at once.
watchAt :: Float -> (Float, Float, Float)
watchAt f = let (x0, y0, k0) = introWatch
                (x1, y1, k1) = playWatch
            in (lerp f x0 x1, lerp f y0 y1, lerp f k0 k1)

-- | The bottom of the era's plate under the watch, in play.
--
-- In canvas units: the watch's height in play less 83.5 watch units, which is how
-- far below the watch's centre the plate's dark outline ends ('caseFront' hangs the
-- plate, 24 tall, from 4 units inside the case's radius of 62, and gives it an
-- outline of 1.5: 58 + 24 + 1.5). It comes to 197.5. What stands under the watch in
-- play ('duoLegend', the room map, the door note) is placed down from here.
underPlay :: Float
underPlay = let (_, y, k) = playWatch in y - 83.5 * k

-- | Where the introduction's column starts across, and how wide it is.
--
-- The column is 264 canvas units wide, centred under the watch's introduction place
-- (132 each side of it), so it runs from about -579 to -315 across.
columnLeft, columnW :: Float
columnLeft = let (x, _, _) = introWatch in x - 132
columnW = 264

-- | The watch's radii, in watch units:
--
--   * 'rCase' 62: the outer edge of the case.
--   * 'rTrackOut' 56.5 and 'rTrackIn' 43.5: the outer and inner edges of the dark
--     channel the light runs in, 13 units wide.
--   * 'rLight' 50 and 'wLight' 9: the middle and the width of the band of light, so
--     it runs from 45.5 to 54.5 and leaves 2 units of dark channel on each side.
--   * 'rFace' 43.5: the face, which fills everything inside the channel.
--   * 'rSocket' 16.5: the outer radius of the power cell's socket.
rCase, rTrackOut, rTrackIn, rLight, wLight, rFace, rSocket :: Float
rCase = 62
rTrackOut = 56.5
rTrackIn = 43.5
rLight = 50
wLight = 9
rFace = 43.5
rSocket = 16.5

-- | The socket for the power cell sits at four o'clock.
--
-- Precisely, 128 degrees clockwise from twelve (four o'clock itself is 120), and 64
-- units from the centre, just outside the case's edge: the socket bulges out of the
-- rim and covers a short stretch of the channel there. In watch units.
socketAt :: (Float, Float)
socketAt = clockAt (rCase + 2) 128

-- | A point at a clock angle: degrees from twelve, clockwise.
--
-- @r@ is the distance from the centre and @a@ the angle in degrees. School maths
-- measures angles anticlockwise from three o'clock, with x = r cos t and y = r sin t;
-- measuring clockwise from twelve swaps the two: x = r sin t, y = r cos t. So 0 is
-- straight up, 90 to the right, 180 straight down and 270 to the left (Gloss's y
-- points up). The @where@ names @t@, the angle in radians, which 'sin' and 'cos'
-- expect.
clockAt :: Float -> Float -> (Float, Float)
clockAt r a = (r * sin t, r * cos t)
  where t = a * pi / 180

-- | A band between two radii, from one clock angle to a later one. Each sliver
-- takes its colour from its angle, so a band can be lit from above.
--
-- Arguments: the inner radius @r0@, the outer radius @r1@, the start and end angles
-- @a0@ and @a1@ (clock degrees, as for 'clockAt'), and @col@, which gives the colour
-- at an angle (pass @const c@ for one colour all round: 'const' c is the function
-- that ignores its argument and returns c). From 0 to 360 it is a whole ring.
--
-- To vary colour around the arc, the band uses slivers of at most 3 degrees,
-- each a four-cornered 'Polygon' from the inner radius to the outer one, coloured at
-- its middle angle; at 3 degrees the straight edges pass for a curve. A whole ring
-- is 120 polygons, each with its own colour, so the fixed rings of the case are
-- drawn once and baked rather than built again every frame.
--
-- The lines starting with a bar are guards: the first whose condition holds gives
-- the result, and 'otherwise' is simply True. An empty or reversed span (@a1@ not
-- more than 0.01 degree past @a0@) draws nothing, so callers can pass a span that has
-- shrunk to nothing without checking first.
band :: Float -> Float -> Float -> Float -> (Float -> Color) -> Picture
band r0 r1 a0 a1 col
  | a1 - a0 <= 0.01 = Blank
  | otherwise = Pictures
      -- A list comprehension: for each sliver number i from 0 to n - 1, the 'let'
      -- works out its start angle u and end angle v (equal shares of the span), and
      -- the expression before the bar draws that sliver between the two radii.
      [ Color (col ((u + v) / 2)) (Polygon [clockAt r0 u, clockAt r1 u, clockAt r1 v, clockAt r0 v])
      | i <- [0 .. n - 1]
      , let u = a0 + (a1 - a0) * fromIntegral i / fromIntegral n
            v = a0 + (a1 - a0) * fromIntegral (i + 1) / fromIntegral n ]
  -- The number of slivers: the span over 3 degrees, rounded up, and at least one.
  -- 'ceiling' can return any whole-number type, so ':: Int' says which one is meant;
  -- 'fromIntegral' above turns i and n back into Floats for the division.
  where n = max 1 (ceiling ((a1 - a0) / 3)) :: Int

-- | Lit from above: the first colour at twelve, the last at six.
--
-- @t@ turns the clock angle into how far down the watch the point is: 0 at twelve
-- and 1 at six, the same on the left and the right (1 - cos a is 0 at 0 degrees, 1
-- at 90 and 2 at 180, so halving it gives 0 to 1). The colour then runs in two
-- straight stretches: from @top@ to @mid@ over the upper 45 per cent of that height,
-- which ends just above three and nine o'clock, and from @mid@ to @bot@ over the
-- rest. 'mix' k a b is the colour k of the way from a to b. Given its three colours,
-- @fromAbove top mid bot@ is a function of the angle alone, just what 'band' asks
-- for: a function given fewer arguments than it takes returns a function of the rest
-- (partial application).
fromAbove :: Color -> Color -> Color -> Float -> Color
fromAbove top mid bot a
  | t < 0.45 = mix (t / 0.45) top mid
  | otherwise = mix ((t - 0.45) / 0.55) mid bot
  where t = (1 - cos (a * pi / 180)) / 2

-- | Small lettering for the watch, drawn at a size whose curves are cut finely and
-- whose joints are round, then scaled down: at its own size an S grows ticks.
--
-- "Font" draws letters of size 26 and over with round joints, and smaller ones with
-- cheap square-ended strokes, whose corners stick out at the bends of a curve and
-- show at the watch's scale. So the text is drawn at 26 and scaled by k = size / 26.
-- The pen weight @w@ is divided by k first, so that once scaled the strokes are @w@
-- units thick. @size@ is the height of a capital; the text starts at x = 0 with its
-- baseline on y = 0.
fine :: Float -> Float -> String -> Picture
fine size w s = Scale k k (weighted 26 (w / k) s)
  where k = size / 26

-- | The width of 'fine' text: its width at 26, scaled the same way.
fineW :: Float -> String -> Float
fineW size s = textW 26 s * size / 26

-- | 'fine' text centred across on x = 0, its baseline on y = 0: moved left by half
-- its width. Used for the era's name on the plate and the line under the number.
centred :: Float -> Float -> String -> Picture
centred size w s = Translate (negate (fineW size s) / 2) 0 (fine size w s)

-- The case -----------------------------------------------------------------------

-- | Shadow, crown, case and the channel the light runs in, with the hours engraved
-- in it where the light has gone.
--
-- In watch units, about the watch's centre. Drawn once per era and baked by
-- 'mkHudArt'. 'watchHud' draws the light over it, so the hour marks in the channel
-- show only where the light has drained away. The list is drawn in order, each
-- layer over the ones before.
caseBack :: Era -> Picture
caseBack era = Pictures
  -- A soft shadow, 3 units below, so the watch seems to stand off the screen.
  [ Color (hexA 0x000000 0.5) (Translate 0 (-3) (ngon 64 (rCase + 2)))
  -- The crown at twelve: a dark outline, the stem, a bright cap and three grooves
  -- for grip. It is drawn before the case, so the case covers its foot.
  , Color (caEdge c) (rect (-9.5) (rCase - 3.5) 19 14)
  , Color (caMid c) (rect (-8) (rCase - 2) 16 11)
  , Color (caTop c) (rect (-8) (rCase + 6) 16 3)
  , Color (fade 0.45 (caEdge c)) (Pictures [ rect (x - 0.5) rCase 1 7 | x <- [-4, 0, 4] ])
  -- The case: a dark disc for its outline, then the ring of metal from the channel
  -- out to the edge, lit from above, and a thin bright line just inside the edge,
  -- polished in the past (28 per cent white) and dull in the present (12).
  , Color (caEdge c) (ngon 72 (rCase + 1))
  , band rTrackOut rCase 0 360 (fromAbove (caTop c) (caMid c) (caBot c))
  , band (rCase - 2.8) (rCase - 1.8) 0 360 (const (hexA 0xFFFFFF (if era == Past then 0.28 else 0.12)))
  , if era == Present then rust else Blank
  -- The channel the light runs in, dark all round.
  , band rTrackIn rTrackOut 0 360 (const (caTrack c))
  -- The hour marks across the channel, one every 30 degrees. Those at twelve,
  -- three, six and nine ('major', h divisible by 3) are brighter, thicker and
  -- longer (2 units in from each side of the channel rather than 3.5). The
  -- annotation ':: Int' fixes h's type, which nothing else here would (GHC would
  -- default it to Integer, and -Wall warns about that). A function written in
  -- backquotes, such as `mod`, is used between its arguments, like an operator.
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
    -- Patches between the channel and the case's edge, so the time is never hidden:
    -- at about seven to eight o'clock (206 to 248 degrees), between one and two (38
    -- to 56), and a darker one at three (90 to 101).
    rust = Pictures
      [ band (rTrackOut + 0.5) (rCase - 0.8) 206 238 (const (fade 0.8 rustC))
      , band (rCase - 4) (rCase - 0.5) 238 248 (const (fade 0.6 rustC))
      , band (rTrackOut + 1.5) (rCase - 1) 38 56 (const (fade 0.7 rustC))
      , band (rTrackOut + 0.5) (rCase - 0.5) 90 101 (const (hexA 0x6A3E2A 0.7))
      ]

-- | The face, the era's plate hung under the case, and the socket's rim.
--
-- In watch units, baked once per era by 'mkHudArt'. 'watchHud' draws it over the
-- light, so the face covers the inner edge of the light's glow, and draws the
-- number, the line under it and the socket's contents over the face.
caseFront :: Era -> Picture
caseFront era = Pictures
  [ Color (caEdge c) (ngon 64 (rFace + 0.75))
  -- The face: six discs, each smaller than the one before by 12 per cent of the
  -- face's radius and 3k units higher, running from the rim's colour to the
  -- centre's (k = i / 5 goes from 0 to 1), so the face is lightest a little above
  -- its middle.
  , Pictures
      [ Color (mix k (caFaceOut c) (caFaceIn c)) (Translate 0 (3 * k) (ngon 64 (rFace * (1 - 0.12 * fromIntegral i))))
      | i <- [0 .. 5 :: Int], let k = fromIntegral i / 5 ]
  , plate
  -- 'uncurry' turns a function of two arguments into one of a pair, so the
  -- socket's (x, y) can be given to 'Translate' as it is.
  , uncurry Translate socketAt socketRim
  ]
  where
    c = caseOf era
    -- The plate: 74 wide in the past and 96 in the present, for the longer word;
    -- 24 tall; @py@ its bottom edge, chosen so its top is 4 units inside the case's
    -- edge, which makes it overlap the case and look hung from it.
    pw = if era == Past then 74 else 96
    ph = 24
    py = negate (rCase - 4) - ph
    -- Outline, face, a bright line along the top and a shadow line along the
    -- bottom, a rust stain at the left end in the present, two rivets 6 units in from
    -- the ends, and the era's name, 13 tall, its baseline 6.5 below the plate's middle
    -- so the capitals sit centred.
    plate = Pictures
      [ Color (caEdge c) (rect (-pw / 2 - 1.5) (py - 1.5) (pw + 3) (ph + 3))
      , Color (caPlate c) (rect (-pw / 2) py pw ph)
      , Color (caPlateHi c) (rect (-pw / 2 + 2) (py + ph - 3.1) (pw - 4) 1.6)
      , Color (caPlateLo c) (rect (-pw / 2 + 2) (py + 1.4) (pw - 4) 1.6)
      , if era == Present then Color (fade 0.55 rustC) (Translate (-pw / 2 + 8) (py + 4) (rot 12 (ellipse 16 8 3.5))) else Blank
      , Color (caPlateLo c) (Pictures [ Translate (s * (pw / 2 - 6)) (py + ph / 2) (ngon 8 1.8) | s <- [-1, 1] ])
      , Color (caPlateInk c) (Translate 0 (py + ph / 2 - 6.5) (centred 13 2.1 (caName c)))
      ]
    -- The socket's rim, about its own centre: a shadow 2 units below, a dark
    -- outline, a disc of the case's metal lit from above (a band from radius 0 is a
    -- whole disc), then a dark ring and the hollow, about 4 units in from the edge,
    -- where 'watchHud' puts the power cell or its empty outline.
    socketRim = Pictures
      [ Color (hexA 0x000000 0.5) (Translate 0 (-2) (ngon 32 (rSocket + 1)))
      , Color (caEdge c) (ngon 32 (rSocket + 1.6))
      , band 0 rSocket 0 360 (fromAbove (caTop c) (caMid c) (caBot c))
      , Color (caEdge c) (ngon 32 (rSocket - 3.2))
      , Color (caTrack c) (ngon 32 (rSocket - 4))
      ]

-- | An empty socket: the cell's silhouette, terminal and all, unlit.
--
-- In watch units, about the socket's centre: a faint body 8.4 by 12.5, its outline
-- (a closed run of strokes: the first point is repeated at the end), and the small
-- terminal on top. Shown while nobody carries the power cell, and again once the
-- level is won or an ending runs (see @socketPic@ in 'watchHud').
emptyCell :: Picture
emptyCell = Pictures
  [ Color (hexA 0xFFFFFF 0.1) (rect (-4.2) (-7) 8.4 12.5)
  , Color (hexA 0xFFFFFF 0.32) (Pictures
      [ segs 1.2 [(-4.2, -7), (4.2, -7), (4.2, 5.5), (-4.2, 5.5), (-4.2, -7)]
      , rect (-2.2) 5.8 4.4 2.4 ])
  ]

-- | A key direction, drawn as an arrow.
--
-- Drawn pointing up, about 7 units tall (a shaft and two strokes for the head),
-- then turned: Gloss's 'Rotate' takes degrees and turns clockwise, so 90 points it
-- right. Used beside "jump -4" under the number, to say which key would jump. The
-- @case@ expression picks the angle by matching @d@ against each direction in turn.
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

-- | How a key is drawn in the line under the board:
--
--   * 'Plain': in the era's rule and ink colours, as usual.
--   * 'Dimmed': at 45 per cent, when time is short and the key would not help.
--   * 'Hot': lit orange, when time is short and the key is a way out (Z or R).
--
-- A plain enumeration: a type whose three constructors carry no data.
data KeyLook = Plain | Dimmed | Hot
-- ^ Plain uses normal ink; Dimmed reduces its alpha; Hot marks an escape key.

-- | A key drawn as a key.
--
-- @edge@ colours the key's rim, @ink@ its letters, and @key@ is what is printed on
-- it. In canvas units, with its left edge on x = 0 and the letters' baseline on
-- y = 0: the cap runs from 6 below the baseline to 16 above (22 tall), with a dark
-- inset face (black at 55 per cent) inside its rim and the letters, 11 tall,
-- centred across it. Its width is 'capW'.
keyCap :: Color -> Color -> String -> Picture
keyCap edge ink key = Pictures
  [ Color edge (rect 0 (-6) w 22)
  , Color (fade 0.55 (hex 0x000000)) (rect 1.5 (-3) (w - 3) 17.5)
  , Color ink (Translate (w / 2) 0 (textC 11 key))
  ]
  where w = capW key

-- | How wide a key cap is for this text: the letters at size 11 with 7 units either
-- side, and never narrower than 26, so a single letter still sits on a whole key.
capW :: String -> Float
capW key = max 26 (textW 11 key + 14)

-- | One line of keys and what they cost, centred under the board.
--
-- @sk@ gives the era's colours, and each item is (key, label, look): the text on
-- the cap, the words after it, and how to draw it ('KeyLook'). In canvas units: each
-- item is its cap, a space of 8, then its label at size 12, with 26 units between
-- items. The line is centred under the board as it stands in play (stand 1 of
-- 'onBoard'), its baseline 12 units above the bottom of the canvas. 'mkHudArt' bakes
-- it, so this layout is worked out once per era.
keyStrip :: Skin -> [(String, String, KeyLook)] -> Picture
keyStrip sk items = Pictures (go x0 items)
  where
    inner = 8
    gap = 26
    -- An item's width; the pattern takes the triple apart, and '_' ignores the look.
    itemW (key, label, _) = capW key + inner + textW 12 label
    -- The whole line: every item, and a gap between each pair ('length' gives an
    -- Int, so 'fromIntegral' makes it a Float to multiply by).
    total = sum (map itemW items) + gap * fromIntegral (length items - 1)
    -- The left end, so that the line's middle lands under the board's middle.
    x0 = fst (onBoard 1 ((mapLeft + mapRight) / 2, 0)) - total / 2
    y = negate canvasH / 2 + 12
    -- Walks the items left to right, carrying the x where the next one starts. The
    -- as-pattern it@(key, label, look) names the whole triple 'it' and its three
    -- parts at once, so 'itemW' can take the whole and 'item' the parts.
    go _ [] = []
    go x (it@(key, label, look) : rest) = Translate x y (item key label look) : go (x + itemW it + gap) rest
    -- One item, coloured by its look: the cap's rim, its letters, and the words
    -- after it. ('words' is a Prelude function, so the name takes a prime: words'.)
    item key label look =
      let (edge, ink, words') = case look of
            Plain -> (skRule sk, skInk sk, skDim sk)
            Dimmed -> (fade 0.45 (skRule sk), fade 0.45 (skInk sk), fade 0.45 (skDim sk))
            Hot -> (hex 0xFF8A5C, hex 0xFFE4D8, hex 0xFFB39E)
      in Pictures [ keyCap edge ink key, Color words' (Translate (capW key + inner) 0 (textL 12 label)) ]

-- | The prices are printed beside the keys, because they are the rules of the game.
--
-- Each pair is (the text on the key, the words after it), built from the level's
-- 'Rules', so a printed price can never disagree with the one charged: "-2 s" is two
-- seconds of the budget. ('++' joins two lists, and a 'String' is a list of
-- characters.) The echo's line appears only in a level that has the echo:
-- @[x | condition]@ is a list comprehension with no generator, which is @[x]@ when
-- the condition holds and @[]@ otherwise, a common way to put an item in a list
-- conditionally. An undo gives back 'ruUndoRefundPercent' per cent of the undone
-- action's price, written "+half" for the usual 50 and otherwise as a number and
-- "pct", since the stroke font has no per cent sign. Used for the keys line and,
-- through 'introKeys', for the introduction's column.
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

-- | The keys that get you out of trouble: Z, which undoes the last action and gives
-- back part of its price, and R, which starts the level again with its whole budget.
-- They are lit when time runs short. @key `elem` list@ asks whether key is in the
-- list.
wayOut :: String -> Bool
wayOut key = key `elem` ["Z", "R"]

-- | Under the watch in a level for two: who is who, and the key they share. The
-- level is never taught, so its keys are always written.
--
-- (The only level for two is the secret one, which is never a teaching level, so it
-- has no keys line under the board; this legend takes its place.) In canvas units,
-- down from 'underPlay', at the watch's place in play: "Shared clock" centred under
-- the plate, then three rows 32 units apart, each a small hood in that keeper's
-- coat colour (none for the shared key), the key cap, and a note after it. The rows
-- start 14 units in from the canvas's left edge.
duoLegend :: Skin -> Picture
duoLegend sk = Pictures
  [ Color (skDim sk) (Translate wx (underPlay - 19) (textC 11 "Shared clock"))
  , row (underPlay - 41) (Just yellowCoat) "WASD" ""
  , row (underPlay - 73) (Just redCoat) "ARROWS" ""
  , row (underPlay - 105) Nothing "SPACE" "both"
  ]
  where
    -- A pattern binding: only the first of the three is wanted, '_' ignores the rest.
    (wx, _, _) = playWatch
    lx = negate canvasW / 2 + 14
    -- One row at baseline @y@. 'maybe' d f m gives d for 'Nothing' and f x for
    -- 'Just' x; here f is a composition ('.' runs the right-hand function first):
    -- draw the hood in that coat, then move it beside the key.
    row y coat key note = Pictures
      [ maybe Blank (Translate (lx + 9) (y + 5) . hood) coat
      , Translate (lx + 26) y (keyCap (skRule sk) (skInk sk) key)
      , Color (skDim sk) (Translate (lx + 26 + capW key + 8) y (textL 11 note))
      ]
    -- A keeper's hooded head, 9 units in radius: a shadow, the coat's main colour,
    -- a face (a warm skin tone) a little low in the hood, and two dark eyes.
    hood c = Pictures
      [ Color (hexA 0x000000 0.5) (ngon 20 10.5)
      , Color (coatMain c) (ngon 20 9)
      , Color (hex 0xF6D9B6) (Translate 0 (-1.5) (ngon 16 5.2))
      , Color (hex 0x3A2A1A) (Pictures [ Translate x (-1) (ngon 6 0.9) | x <- [-2, 2] ])
      ]

-- | The keys a level is introduced with. A level for two names each keeper's keys.
--
-- With @duo@, moving is split by keeper (W A S D for yellow, the arrows for red,
-- each at a step's price), Space shifts both, and the rest of 'taughtKeys' follows
-- without its own move and shift lines. In that comprehension, k@(key, _) names the
-- whole pair k and its first part key (an as-pattern), and the condition after the
-- comma keeps only the pairs whose key is not one of the two replaced.
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
--
-- @duo@ is whether the level is for two and @(place, name)@ its title, such as
-- ("1 of 3", "Clear the Way"). In canvas units, from 'columnLeft'; the watch over it
-- is drawn by 'watchHud', not here. From the top: a rule at y = 100, the place in
-- the dim colour, the name in the accent colour at size 18, the era's line
-- ('eraNote'), another rule at y = 0, then one row per key, 34 units apart from
-- y = -38: its cap, and 86 units from the column's left edge, what it does and costs.
-- Baked once per era by 'mkHudArt'; 'watchHud' fades it out in the first half of the
-- closing in.
introColumn :: Bool -> (String, String) -> Rules -> Era -> Picture
introColumn duo (place, name) rules era = Pictures
  [ backdrop
  , Color (skRule sk) (rect columnLeft 100 columnW 1)
  , Color (skDim sk) (Translate columnLeft 74 (textL 11 place))
  , Color (skAccent sk) (Translate columnLeft 46 (textL 18 name))
  , Color (skDim sk) (Translate columnLeft 22 (textL 11 (eraNote era)))
  , Color (skRule sk) (rect columnLeft 0 columnW 1)
  -- [-38, -72 ..] is an endless list of heights counting down by 34. 'zip' pairs it
  -- with the keys and stops at the shorter list, so, Haskell being lazy, only as
  -- many heights as there are keys are ever worked out.
  , Pictures
      [ Pictures [ Translate columnLeft y (keyCap (skRule sk) (skInk sk) key)
                 , Color (skInk sk) (Translate (columnLeft + 86) y (textL 12 what)) ]
      | (y, (key, what)) <- zip [-38, -72 ..] (introKeys duo rules) ]
  ]
  where
    sk = skin era
    -- A soft shadow behind the column, so the sea moving under it never fights the
    -- lettering.
    -- It runs from the canvas's left edge to 8 units short of the board's edge as
    -- it stands while introduced, in 12 strips of width @w@, each a full canvas
    -- tall: black at 30 per cent at the screen's edge, losing a twelfth of that with
    -- each strip, a stepped gradient that Gloss cannot draw in one shape.
    edge = fst (onBoard 0 (mapLeft - 12, 0)) - 8
    w = (edge + canvasW / 2) / 12
    backdrop = Pictures
      [ Color (hexA 0x000000 (0.3 * (1 - fromIntegral i / 12))) (rect (negate canvasW / 2 + fromIntegral i * w) (negate canvasH / 2) w canvasH)
      | i <- [0 .. 11 :: Int] ]

-- | A line about the era, under a level's name: in the introduction's column, and
-- in the caption above the map ('mapCaption').
eraNote :: Era -> String
eraNote Past = "Lamps trimmed, brass polished"
eraNote Present = "Roof gone, the sea coming in"

-- The watch, live -----------------------------------------------------------------

-- | @duo@: a level for two. @teach@: write the keys under the board in play.
-- @title@: the level's place in the hall and its name.
--
-- Builds the 'HudArt' of one level, whose 'Rules' give the prices on the keys.
-- Called by "Render"'s 'mkScene'; "Main" keeps one scene per level for the whole
-- run, so this runs at most once per level. Every picture is baked, and Haskell
-- evaluates a value only when it is first needed and then keeps it, so each one is
-- drawn and baked the first time it is shown, and never again.
mkHudArt :: Bool -> Bool -> (String, String) -> Level -> HudArt
mkHudArt duo teach title level = HudArt
  -- 'bake . introColumn duo title (lvRules level)' is a composition: give the era
  -- to the partly applied 'introColumn', then bake what it draws. The same pattern
  -- runs through the fields below; @\e -> ...@ is an unnamed function of the era.
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
    -- When time is short, a level that writes its keys shows them all, the way out
    -- hot and the others dimmed; a level that does not still lights Z and R, and
    -- shows nothing else (the comprehension's condition keeps only those two).
    lowKeys
      | teach = [ (k, l, if wayOut k then Hot else Dimmed) | (k, l) <- taught ]
      | otherwise = [ (k, l, Hot) | (k, l) <- taught, wayOut k ]
    -- The heart of the caching. 'f Past' and 'f Present' are named outside the
    -- returned function, so each is worked out once (when first asked for) and the
    -- function only chooses between the two results. Written as \e -> f e, every
    -- call would draw and bake again, which is every frame.
    perEra f = let p = f Past
                   q = f Present
               in \e -> case e of
                    Past -> p
                    Present -> q

-- | A whole number of seconds of budget, as the plates write it: "42 s". 'show'
-- turns a value into its text.
secondsText :: Int -> String
secondsText s = show s ++ " s"

-- | The watch, the keys and whatever stands under the watch.
--
-- Drawn by "Render" every frame of a level ('frame'), straight onto the canvas,
-- after the board. Arguments: the level's 'HudArt', the 'Level', this frame's 'Game'
-- and 'Fx', and the clock (ticks of animation, 'fxClock', as a 'Float'). Defined by
-- partial application: 'watchHud' with its first argument fixed, so it takes the
-- other five.
hud :: HudArt -> Level -> Game -> Fx -> Float -> Picture
hud = watchHud False

-- | The same, for a level's ending, where the keeper walks free: the watch stops
-- where it was, and nothing it says is a warning.
--
-- Drawn by "Render"'s 'endingFrame' for the whole ending, from the lit lamp on.
-- Once the keeper may walk, "Render" passes the opened level ('revealedLevel'),
-- whose prices are all 0, so a free step does not drain the reading either.
hudStopped :: HudArt -> Level -> Game -> Fx -> Float -> Picture
hudStopped = watchHud True

-- | The watch and everything around it, for 'hud' and 'hudStopped'.
--
-- @stopped@ is True in an ending: the screen is taken as fully closed in, and the
-- watch neither warns nor offers anything. The picture is built back to front: the
-- introduction's column (fading out), the watch moved to its place and scaled, then
-- what stands under the watch in play (the legend for two, the room map and the
-- door note), which comes in as the screen closes in, and last the line of keys
-- under the board.
--
-- The ring: the light runs clockwise from twelve to the angle of the time left, a
-- whole turn being the whole budget, and drains back towards twelve as time is
-- spent. Around it: the orange wake of the last spend, the glow, a hatched preview
-- of a jump's price (or of everything left, when a shift was refused for want of
-- time), the green refill of an undo, the dashed outline of the time a refused
-- shift lacked, and a bright bead at the light's end. Most of these are local
-- definitions in the @where@ below.
watchHud :: Bool -> HudArt -> Level -> Game -> Fx -> Float -> Picture
watchHud stopped art level g fx clock = Pictures
  [ fadePicture introK (artIntro art era)
  -- The watch itself, in watch units: everything in this list is drawn about the
  -- watch's centre at scale 1, in this order, then scaled by wk and moved to
  -- (wx, wy) as one.
  , Translate wx wy (Scale wk wk (Pictures
      [ halo
      , artBack art era
      , trail
      , lightGlow
      -- The light itself, from twelve to the reading, or to where a preview starts.
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
  -- Under the watch, at its place in play, coming in with the screen.
  , fadePicture f (artBeside art era)
  , fadePicture f roomMap
  , fadePicture f doorNote
  , keysLine
  ]
  where
    -- 0 while the level is introduced, 1 once the screen has closed in on the
    -- board. The column goes in the first half of the move; the rest comes in.
    -- @progress@ is linear in time ('focusProgress': 45 ticks, three quarters of a
    -- second, from the keeper's first action). @f@ eases it with 'smooth' (3u^2 -
    -- 2u^3, slow at both ends), as "Render" eases the board's stand ('focusOf'), so
    -- the watch and the board move as one. @introK@ is the column's opacity: 1 at
    -- first and 0 once @progress@ reaches a half, eased the same way.
    progress = if stopped then 1 else focusProgress fx
    f = smooth progress
    introK = 1 - smooth (min 1 (progress * 2))
    (wx, wy, wk) = watchAt f
    -- The watch's x in play, for what stands under it only in play.
    (pwx, _, _) = playWatch
    era = gEra g
    sk = skin era
    c = caseOf era
    rules = lvRules level
    -- 'max 1' keeps the divisions by the budget below safe in a level without one.
    budget = max 1 (lvBudget level)
    -- Whole seconds of budget left, already charged for any action under way.
    remaining = gRemaining g
    duo = isJust (gPartner g)
    -- A slow breath for the low-time halo and number: 0 to 1 and back about every
    -- 35 ticks (2 pi / 0.18), a little over half a second.
    pulse = 0.5 + 0.5 * sin (clock * 0.18)

    -- An action is charged when accepted, but the reading drains as it plays out,
    -- so the cost is felt rather than just subtracted.
    -- @shown@ is the reading: what is left, plus the part of the action under way
    -- that has not drained yet. A step plays out over 8 ticks ('stepTicks') while
    -- @left@ counts down from 8 to 1, so the reading falls by an eighth of the price
    -- each tick and reaches @remaining@ as the step lands; a shift drains over its 13
    -- ticks ('shiftTicks') the same way. Only the first keeper's motion is read.
    -- The ':: Float' pins the sum's type ('fromIntegral' can make any kind of
    -- number).
    shown = fromIntegral remaining + stillDraining :: Float
    stillDraining = case gMotion g of
      -- 'gCell' stays where the step started until it lands, so the distance to
      -- the destination tells a jump from a walk or a push: those move one cell,
      -- while a jump always lands at least two cells away, beyond its mirror. The
      -- primes only make new names: fx' is the start's x, nothing to do with 'fx',
      -- the effects.
      Stepping (tx, ty) left ->
        let (fx', fy') = gCell g
            price = if abs (tx - fx') + abs (ty - fy') > 1 then ruJumpCost rules else ruStepCost rules
        in fromIntegral (price * left) / fromIntegral stepTicks
      Shifting left -> fromIntegral (ruShiftCost rules * left) / fromIntegral shiftTicks
      Settled -> 0
    -- The clock angle of @v@ seconds, the whole budget being a whole turn; 'clamp01'
    -- keeps it within 0 to 360 degrees whatever @v@ is.
    deg v = 360 * clamp01 (v / fromIntegral budget)
    -- The angle of what is truly left, as charged, without the part still draining.
    remDeg = deg (fromIntegral remaining)

    -- Once the level is won, or its ending is running, time no longer matters.
    -- (A lost level is still live, so the watch goes on warning beside the loss
    -- plate.)
    live = not stopped && gPhase g /= Won
    -- A fifth of the budget or less. Comparing remaining * 5 with the budget keeps
    -- to whole numbers and avoids a division.
    low = live && remaining * 5 <= budget
    -- One binding with guards can name several values at once through a tuple
    -- pattern: the light's two colours, the glow's colour and the glow's strength,
    -- embers when time is short and the era's own otherwise.
    (lightA, lightB, glowC, glowK)
      | low = (emberA, emberB, emberB, 0.9)
      | otherwise = (caLightA c, caLightB c, caGlow c, caGlowK c)
    -- The light's colour at an angle: lightA at twelve, half way to lightB just
    -- above three and nine, lightB at six ('fromAbove'). A function, for 'band'.
    lightCol = fromAbove lightA (mix 0.5 lightA lightB) lightB

    -- What the last action spent, or an undo gave back, for as long as it floats.
    -- ('fxCost' is set on the tick the time changes and kept for 'costLife', 48
    -- ticks; it is ignored once time no longer matters.)
    costNow
      | live = fxCost fx
      | otherwise = Nothing
    -- How far through its life a floating cost is: 0 when new, just under 1 at the
    -- end. The fades below use 1 - p * p, which stays near full at first and falls
    -- away faster towards the end.
    costAge cf = fromIntegral (cfAge cf) / fromIntegral costLife :: Float

    -- A jump the board is offering: the keeper stands still in a mirror's light,
    -- with open floor where the reflection stands.
    -- None when time no longer matters, when the level is not being played, or
    -- while Tab is held ('gPrev' is the last tick's keys): the board then shows the
    -- other era, where this light is not. Otherwise every keeper standing still is
    -- asked (the second too, in a level for two, swapped into the player's place by
    -- 'swapKeepers'; @[x | duo]@ adds it only then), for each of the four
    -- directions, whether 'mirrorAhead' finds a mirror with an open landing. The
    -- first direction found is the one shown. In a comprehension, Just j <- [...]
    -- draws from a list of one 'Maybe' and skips it when the pattern does not match,
    -- so only real jumps get through. [minBound .. maxBound] lists every 'Dir' (up,
    -- down, left, right), which works because 'Dir' derives 'Enum' and 'Bounded'.
    offer
      | not live || gPhase g /= Playing || tiPeek (gPrev g) = Nothing
      | otherwise = case [ d | gg <- g : [ swapKeepers g | duo ], gMotion gg == Settled
                             , d <- [minBound .. maxBound]
                             , Just j <- [mirrorAhead level gg d], jOpen j ] of
          d : _ -> Just d
          [] -> Nothing
    -- A shift refused for want of time.
    -- A case branch with a guard: taken only if the pattern matches and the
    -- condition after the bar holds. The number is the refusal's strength, its
    -- ticks left over 20: 1 for most of its 96 ticks ('blockedFeedbackTicks'), then
    -- fading over the last 20, a third of a second.
    shortfall = case gFeedback g of
      Just (Feedback ShiftTooCostly left) | live -> Just (clamp01 (fromIntegral left / 20))
      _ -> Nothing

    -- The price on offer, or the whole of what is left when it is not enough: still
    -- yours, but marked to go.
    -- As a pair of clock angles (from, to). A refused shift marks everything left;
    -- a jump on offer marks its price, from where the time would fall to up to where
    -- it stands now. These are pattern guards: | Just _ <- shortfall holds when
    -- shortfall is a 'Just', whatever it holds. Their order is their priority.
    previewRange
      | Just _ <- shortfall = Just (0, remDeg)
      | Just _ <- offer = Just (deg (fromIntegral (remaining - ruJumpCost rules)), remDeg)
      | otherwise = Nothing
    -- Where the plain light ends: where the preview starts, if there is one, so the
    -- hatching takes the light's place there, and otherwise at the reading.
    -- 'maybe' d f m is d for 'Nothing' and f x for 'Just' x; 'fst' takes "from".
    mainTo = maybe (deg shown) fst previewRange

    -- Time running short: three rings of ember glow just outside the case, fainter
    -- outward (32, 18 and 8 per cent), breathing with @pulse@ between 60 and 100
    -- per cent of that. The comprehension takes each triple apart as it draws.
    halo
      | low = Pictures
          [ band (rCase + e0) (rCase + e1) 0 360 (const (fade (a * (0.6 + 0.4 * pulse)) emberB))
          | (e0, e1, a) <- [(0, 4, 0.32), (4, 8, 0.18), (8, 13, 0.08)] ]
      | otherwise = Blank

    -- The wake of the last spend, fading, as a fighting game leaves a damage trail.
    -- Only after a spend (a negative amount). It runs from the angle of what is now
    -- left to where the reading stood before the spend, and lies under the light, so
    -- it shows as the light drains back off it, in the spend's orange, and fades.
    trail = case costNow of
      Just cf | cfAmount cf < 0 ->
        let p = costAge cf
        in band (rLight - wLight / 2) (rLight + wLight / 2) remDeg (deg (fromIntegral (remaining - cfAmount cf)))
             (const (fade (0.75 * (1 - p * p)) (costColor (cfAmount cf))))
      _ -> Blank

    -- The glow round the light: three bands, each wider than the light by @e@ on
    -- both sides and fainter (30, 15 and 7 per cent, times the glow's strength),
    -- from twelve to the reading. Layered transparency, not real bloom.
    lightGlow = Pictures
      [ band (rLight - wLight / 2 - e) (rLight + wLight / 2 + e) 0 (deg shown) (const (fade (a * glowK) glowC))
      | (e, a) <- [(3, 0.3), (6.5, 0.15), (10, 0.07)] ]

    -- The preview, hatched, only when its span is not empty (a1 > a0): a pale wash
    -- of the light at 30 per cent over the span; dashes 1.8 degrees long every 4.3
    -- degrees, their strength @hatchK@ breathing between 30 and 100 per cent about
    -- every 42 ticks; and two ink ticks across the channel at the span's ends.
    -- [a0 + 0.6, a0 + 4.9 ..] is an endless list counting up by 4.3, of which
    -- 'takeWhile' keeps the part below a1 (laziness: the rest is never made); (< a1)
    -- is an operator section, the function asking whether a number is below a1.
    -- 'min a1' keeps the last dash inside the span.
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

    -- An undo's refund (a positive amount): the stretch the time came back over,
    -- from the old reading up to the new one, drawn over the light in green and
    -- fading to the light's own colour as the refund ages.
    refundPic = case costNow of
      Just cf | cfAmount cf > 0 ->
        let p = costAge cf
        in band (rLight - wLight / 2) (rLight + wLight / 2) (deg (fromIntegral (remaining - cfAmount cf))) remDeg
             (const (fade (1 - p * p) (costColor (cfAmount cf))))
      _ -> Blank

    -- The part of a refused price that is not there, as an outline.
    -- A shift's whole price would reach from twelve to @a1@; what is there reaches
    -- to the reading, so the stretch from the reading on to @a1@ is the time
    -- missing. Dashes 3 degrees long every 5.6 degrees, a little narrower than the
    -- light, in ember, fading with the refusal (@k@).
    shortPic = case shortfall of
      Just k ->
        let a1 = deg (fromIntegral (ruShiftCost rules))
        in Pictures [ band (rLight - 3.5) (rLight + 3.5) u (min a1 (u + 3)) (const (fade k emberB))
                    | u <- takeWhile (< a1) [remDeg + 1, remDeg + 6.6 ..] ]
      Nothing -> Blank

    -- The bead of light at the front, where the next second goes from.
    -- A small glow and a near-white dot on the light's middle line at the reading's
    -- angle. Not drawn with nothing left or with the whole budget, where it would
    -- only sit at twelve.
    bead
      | shown > 0 && shown < fromIntegral budget =
          uncurry Translate (clockAt rLight (deg shown)) (Pictures
            [ glow glowC 9 3, Color (hex 0xFFFDF6) (ngon 12 3) ])
      | otherwise = Blank

    -- The seconds left, on a baseline 12 units below the face's centre, so the
    -- digits stand a little above the middle with room for the line under them.
    -- The reading is rounded up ('ceiling'), so it shows 0 only when nothing at all
    -- is left, and it counts down through each second as an action drains. One or
    -- two digits are 34 units tall, three are 29 and more 24, to stay inside the
    -- face. Drawn at size 40 with a pen of 5, where "Font" cuts curves finely and
    -- rounds joints, then scaled to the size wanted. When time is short the ink
    -- breathes between ember and a pale salmon with @pulse@.
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
    -- Each is (text, colour, strength 0 to 1, the direction of a key whose arrow
    -- goes before the text, if any): a refused shift's price ("shift 6"), fading
    -- with the refusal; a jump on offer, in the accent colour, with the arrow of
    -- its key ("jump -4"); what the last action spent or an undo gave back ("-2",
    -- "+1"), fading as it ages; when time is short, the steps it still buys. The
    -- third guard has two parts, a pattern and a condition, and both must hold.
    ctx
      | Just k <- shortfall = Just ("shift " ++ show (ruShiftCost rules), emberInk, k, Nothing)
      | Just d <- offer = Just ("jump " ++ costText (negate (ruJumpCost rules)), skAccent sk, 1, Just d)
      | Just cf <- costNow, cfAmount cf /= 0 =
          let p = costAge cf in Just (costText (cfAmount cf), costColor (cfAmount cf), 1 - p * p, Nothing)
      | low = Just (stepsLeft, emberInk, 1, Nothing)
      | otherwise = Nothing
    -- How many steps the time left still buys: 'div' divides whole numbers and
    -- rounds down, and 'max 1' guards against a level whose steps are free.
    stepsLeft =
      let n = remaining `div` max 1 (ruStepCost rules)
      in show n ++ (if n == 1 then " step" else " steps")
    -- The line, 30 units under the centre, 10.5 tall with a pen of 1.5. With an
    -- arrow, the arrow takes 11 units first and the two are centred together: the
    -- whole is moved left by half its width, and the arrow, drawn about its own
    -- middle, is raised 3.8 towards the letters' middle.
    ctxPic = case ctx of
      Nothing -> Blank
      Just (str, col, k, arrow) -> Color (fade k col) (Translate 0 (-30) (case arrow of
        Nothing -> centred 10.5 1.5 str
        Just d ->
          let total = 11 + fineW 10.5 str
          in Translate (negate total / 2) 0 (Pictures
               [ Translate 3.5 3.8 (arrowFor d), Translate 11 0 (fine 10.5 1.5 str) ])))

    -- The power cell in its socket: the lit cell from "Figures", scaled to 45 per
    -- cent to fit the socket's hollow, while either keeper carries it ('anyHasCell')
    -- and time still matters; otherwise the empty outline, so once the level is won
    -- the socket shows empty again.
    socketPic
      | live && anyHasCell g = Scale 0.45 0.45 (cellIcon True clock)
      | otherwise = emptyCell

    -- A level of several screens gets a small map of its rooms under the watch:
    -- the rooms seen so far, and the one on screen picked out.
    -- The rooms across and down, rounding up: (a + b - 1) `div` b is a division that
    -- rounds up. Only a level built of whole rooms ('roomedLevel', shown a room at a
    -- time) with more than one room gets the map; a level of any other size is shown
    -- through a camera that follows the keeper instead. Each tile is 28 by 18 and
    -- they stand 3 apart (31 across, 21 down), the map centred under the watch's
    -- place in play: just under the plate, or under the legend in a level for two.
    roomsAcross = (lvWidth level + roomCols - 1) `div` roomCols
    roomsDown = (lvHeight level + roomRows - 1) `div` roomRows
    hasMap = roomsAcross * roomsDown > 1 && roomedLevel level
    mapW = fromIntegral roomsAcross * 31 - 3
    mapTopY = if duo then underPlay - 135 else underPlay - 16
    -- Rooms are numbered like cells, (across, down) with row 0 at the top, so the
    -- tiles go right with rx and down the screen with ry. With two generators the
    -- later one, rx, varies fastest.
    roomMap
      | not hasMap = Blank
      | otherwise = Pictures
          [ Translate (pwx - mapW / 2 + fromIntegral rx * 31) (mapTopY - 18 - fromIntegral ry * 21) (roomTile (rx, ry))
          | ry <- [0 .. roomsDown - 1], rx <- [0 .. roomsAcross - 1] ]
    -- The room on screen ('fxRoom'): filled in the accent colour, with a dark
    -- window and a dot for the keeper. A room seen before ('fxVisited', a short
    -- list, so 'elem' does): filled in the rule colour. A room not seen yet: an
    -- outline only, four thin strips.
    roomTile r
      | r == fxRoom fx = Pictures
          [ Color (skAccent sk) (rect 0 0 28 18)
          , Color (hexA 0x000000 0.35) (rect 3 3 22 12)
          , Color (skAccent sk) (Translate 14 9 (ngon 10 3.2)) ]
      | r `elem` fxVisited fx = Color (skRule sk) (rect 0 0 28 18)
      | otherwise = Color (fade 0.6 (skRule sk)) (Pictures
          [ rect 0 0 28 1.5, rect 0 16.5 28 1.5, rect 0 0 1.5 18, rect 26.5 0 1.5 18 ])

    -- A push in the past changes a door you cannot see from there.
    -- Only in a level with doors ('Set.null' asks whether the set of door cells is
    -- empty). Whether they are open is worked out now, as the rules do, from whether
    -- something stands on every plate ('doorsOpen' over 'weights', the cells a crate,
    -- a keeper or the echo stands on). It stands under the room map if there is one
    -- (each row of tiles 21, then 8 more), or just under the watch.
    doorNote
      | Set.null (lvDoors level) = Blank
      | otherwise =
          let y = mapTopY - 14 - (if hasMap then fromIntegral roomsDown * 21 + 8 else 0)
          in Color (skInk sk) (Translate pwx y
               (textC 12 (if doorsOpen level (weights g) then "Door open" else "Door shut")))

    -- The line of keys under the board. Nothing in an ending or when the level is
    -- not being played (the pause, win and loss plates carry their own keys). When
    -- time is short, the version with the way out lit, at once and in every level;
    -- otherwise only in a level that writes its keys, coming in with the screen.
    keysLine
      | stopped || gPhase g /= Playing = Blank
      | low = artKeysLow art era
      | artTeach art = fadePicture f (artKeys art era)
      | otherwise = Blank

-- | How a change in time left is written: "+3" for time given back, "-2" for time
-- spent ('show' of a negative number already has its minus), "0" for none. Used in
-- the line under the number and by the cost that floats off the keeper ("Render").
costText :: Int -> String
costText n
  | n > 0 = "+" ++ show n
  | otherwise = show n

-- | Spending is hot, a refund is cool.
--
-- Light green (9CE0B0) for time given back; orange (FF9A6B) for time spent, or
-- none. Used on the watch's ring and line, and by the floating cost in "Render".
costColor :: Int -> Color
costColor n
  | n > 0 = hex 0x9CE0B0
  | otherwise = hex 0xFF9A6B

-- Over the board ------------------------------------------------------------------
--
-- Everything from here on is drawn in the board's own coordinates, inside 'board' in
-- "Render", so it grows and slides with the map. The 13 x 9 map spans 'mapLeft'
-- (-612) to 'mapRight' (376) across and 'mapBottom' (-342) to 'mapTop' (342) up, a
-- cell being 76 units; text sizes are the height of a capital in these units.

-- | The level's name above the board, with a line about the era, and its place in
-- the hall at the other end. The era itself is named on the watch.
--
-- @title@ is (place, name), as everywhere here. "Render" bakes it once per era when
-- it makes the level's scene, and draws the baked one once the screen has closed in.
mapCaption :: (String, String) -> Era -> Picture
mapCaption title era = captionWith title (skAccent sk) (skDim sk) (eraNote era)
  where sk = skin era

-- | The same line, part way in.
--
-- @k@ runs from 0 to 1. "Render" draws this while the screen closes in, unbaked,
-- since it only lasts that long. 'fadePicture' gives 'Blank' at once for k near 0,
-- and Haskell being lazy, the caption is then never even built.
mapCaptionFaded :: Float -> (String, String) -> Era -> Picture
mapCaptionFaded k title era = fadePicture k (mapCaption title era)

-- | The caption's layout: @name@ in the @strong@ colour at size 16 from the map's
-- left edge, 23 units above its top; @note@ in the @soft@ colour at size 12, 18
-- units after the name; and @place@, also soft, at size 12, ending 4 units inside
-- the map's right edge ('textR' right-aligns), all on one baseline.
captionWith :: (String, String) -> Color -> Color -> String -> Picture
captionWith (place, name) strong soft note = Pictures
  [ Color strong (Translate mapLeft (mapTop + 23) (textL 16 name))
  , Color soft (Translate (mapLeft + textW 16 name + 18) (mapTop + 23) (textL 12 note))
  , Color soft (Translate (mapRight - 4) (mapTop + 23) (textR 12 place))
  ]

-- | The same line, once the lamp is lit.
--
-- In warm gold and pale tan, whatever the era. Baked here, once per level, and
-- drawn by "Render" while the lamp burns: on a win, and in an ending until the lamp
-- goes out.
wonCaption :: (String, String) -> Picture
wonCaption title = bake (captionWith title (hex 0xFFE2A8) (hex 0xD8BE90) "The lamp burns again")

-- | The line above the map once the lamp has gone out again.
--
-- In the present's colours, whatever the era: drawn through the rest of an ending.
darkCaption :: (String, String) -> Picture
darkCaption title = bake (captionWith title (skAccent sk) (skDim sk) "The lamp has gone out")
  where sk = skin Present

-- | One brief line under the map. The caller decides what, if anything.
--
-- @era@ picks the ink, @k@ (0 to 1) fades the whole band, and @str@ is the line.
-- A dark band 28 tall, across the map and 12 units beyond each side, its top 16
-- under the map's bottom edge, with the line at size 14 centred in it. "Render"
-- chooses the line, such as a refusal's advice, a dead end, the Tab view, a hint
-- or the ending's way on.
hintBand :: Era -> Float -> String -> Picture
hintBand era k str = Pictures
  [ Color (hexA 0x000000 (0.55 * k)) (rect (mapLeft - 12) (mapBottom - 44) (mapRight - mapLeft + 24) 28)
  , Color (fade k (skInk (skin era))) (Translate ((mapLeft + mapRight) / 2) (mapBottom - 36) (textC 14 str))
  ]

-- | The refusal, pinned beside the cell it is about.
--
-- A shift refused because the cell is solid in the other era. @era@ is the era the
-- keeper is in, so the plate's second line speaks of the other one, where the shift
-- would have gone ('blockedDetail'). @cell@ is the cell as the map shows it: in a
-- level of rooms, counted within the room on screen. The opacity is left off, so
-- @blockedPlate era cell@ is itself a function of the opacity, as its type says
-- (partial application again).
blockedPlate :: Era -> Cell -> Float -> Picture
blockedPlate era cell = refusalPlate cell blockedTitle (blockedDetail (otherEra era))

-- | The same plate, when the only thing missing is time.
--
-- The second line gives the shift's price from @rules@; the cell is the keeper's.
tooCostlyPlate :: Rules -> Cell -> Float -> Picture
tooCostlyPlate rules cell = refusalPlate cell tooCostlyTitle (tooCostlyDetail rules)

-- | The same plate, pinned to where a crate cannot go.
--
-- A push refused because the crate would end up inside a wall in the present.
pushBlockedPlate :: Cell -> Float -> Picture
pushBlockedPlate cell = refusalPlate cell pushBlockedTitle pushBlockedDetail

-- | A plate of two lines, @title@ over @detail@, pinned near the map's cell (x, y)
-- as shown, at opacity @k@ (which "Render" fades over the refusal's last 20 ticks).
--
-- A dark plate 62 tall and as wide as the longer line plus 30, with orange rules
-- along its top and bottom, the title at size 15 and the detail at size 11.
-- Up and down, it stands 72 units (just under a cell) above the cell, or below it
-- for a cell in the top three rows, where above would run off the top of the map
-- (rows count down from 0 at the top). Across, it is centred on the cell but kept 8
-- units inside the map's edges: 'max' lo ('min' hi x) clamps x between lo and hi,
-- so a cell by the wall never pushes the plate off the map.
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

-- | The middle of the map across, in board units (-118).
centreOfMap :: Float
centreOfMap = (mapLeft + mapRight) / 2

-- | A wide plate over the map, for the screens that pause the game.
--
-- @y@ is the plate's middle, @h@ its height and @k@ its opacity; @fill@ colours its
-- body and @edge@ the rules 2 units thick along its top and bottom. It is 620 wide,
-- centred across the map, which is 988 wide.
wide :: Float -> Float -> Float -> Color -> Color -> Picture
wide y h k fill edge = Translate centreOfMap y (Pictures
  [ Color (fade k fill) (rectC 620 h)
  , Color (fade k edge) (Pictures [rect (-310) (h / 2 - 2) 620 2, rect (-310) (-h / 2) 620 2])
  ])

-- | The pause: the whole map dimmed, and a plate across its middle that says PAUSED
-- over the keys to resume, restart or go back to the hall.
--
-- @era@ picks the colours and @k@ fades it all in ("Render" eases it over the first
-- 8 ticks of the pause). The asterisks in the line print as small dots, since
-- "Font" draws '*' as a very short stroke at mid-height: a separator.
pausedScreen :: Era -> Float -> Picture
pausedScreen era k = Pictures
  [ Color (hexA 0x05080A (0.55 * k)) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom))
  , wide 10 136 k (hexA 0x0B1014 0.93) (skRule (skin era))
  , Color (fade k (skInk (skin era))) (Translate centreOfMap 18 (weightedC 40 5 "PAUSED"))
  , Color (fade k (skDim (skin era))) (Translate centreOfMap (-30) (textC 14 "P resume   *   R restart   *   ESC level hall"))
  ]

-- | The plate over the map when a level is won, knowing whether another follows.
--
-- @hasNext@ is whether another level follows in the hall, @k@ the opacity ("Render"
-- brings it in from 70 ticks after the win, over 30, so the victory plays first),
-- and @remaining@ the seconds left, given as the result. The plate stands low, its
-- middle 236 units under the map's middle, leaving most of the board in view. The
-- map is not dimmed, unlike the pause and the loss.
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

-- | Out of time: the map dimmed, and a plate across its middle with orange rules,
-- the words of failure ('failureText') and the ways on. Z rewinds the last action,
-- which gives back part of its price and so can bring the level back; Enter or R
-- starts again; Escape leaves for the hall. @k@ fades it in ("Render" eases it over
-- the 24 ticks after the loss).
lostScreen :: Float -> Picture
lostScreen k = Pictures
  [ Color (hexA 0x05080A (0.5 * k)) (rect mapLeft mapBottom (mapRight - mapLeft) (mapTop - mapBottom))
  , wide 10 150 k (hexA 0x0B1014 0.95) (hex 0xFF8A5C)
  , Color (fade k (hex 0xE6EDEE)) (Translate centreOfMap 22 (weightedC 32 4 failureText))
  , Color (fade k (hex 0x9AA9AE)) (Translate centreOfMap (-10) (textC 13 "Out of time. The station stays dark."))
  , Color (fade k (hex 0xE6EDEE)) (Translate centreOfMap (-40) (textC 13 "Z rewinds   *   Enter or R starts over   *   ESC level hall"))
  ]

-- | Centred lettering with a chosen pen weight: "Font"'s 'weighted' at @size@ with
-- a pen @w@ units thick, centred across on x = 0, its baseline on y = 0. Used for
-- the watch's number and the headings of the plates here, and by "Render" for the
-- number of the level an ending leads into.
weightedC :: Float -> Float -> String -> Picture
weightedC size w s = Translate (negate (textW size s) / 2) 0 (weighted size w s)

-- | The plate for a win that is not the end. It says what the last plate of all
-- says, and offers nothing to press.
--
-- Drawn by "Render" at the start of an ending: fading in while the lamp burns
-- ('Lit') and out over the first moments of the flare. Laid out as 'wonScreen', its
-- last line saying that every lamp is lit.
endedPlate :: Float -> Int -> Picture
endedPlate k remaining = Pictures
  [ wide (-236) 150 k (hexA 0x1C1208 0.86) (hex 0xE8B85C)
  , Color (fade k (hex 0xFFEFC4)) (Translate centreOfMap (-224) (weightedC 30 3.7 victoryText))
  , Color (fade k (hex 0xD8BE90)) (Translate centreOfMap (-254) (textC 13 ("Time left " ++ secondsText remaining)))
  , Color (fade k (hex 0xFFEFC4)) (Translate centreOfMap (-287) (textC 13 "Every lamp is lit"))
  ]
