-- | What should be heard: pure, and derived like 'Game.Fx' by comparing each tick
-- with the one before, so a sound cannot disagree with what happened.
--
-- 'cues' names the one-shot sounds a tick starts. 'beds' says how loud each looping
-- bed should be now. Neither knows how a sound is made or played: the app does that.
module Game.Sound
  ( Cue (..)
  , Keeper (..)
  , cues
  , Beds (..)
  , silence
  , beds
  , lampAt
  , lampCount
  ) where

import qualified Data.Set as Set
import Game.Fx
import Game.Logic (anyHasCell)
import Game.Run
import Game.Types

data Cue
  = CueStep !Era !Int !Float !Keeper
                              -- ^ a keeper sets off: the era underfoot, which foot,
                              --   where on screen (-1 left to 1 right), and who
  | CuePush                   -- ^ a crate pushed
  | CueMirrorPush             -- ^ a movable mirror pushed
  | CuePitFill                -- ^ a crate into a pit
  | CueJump !Bool             -- ^ through a mirror; True for a corner mirror, which
                              --   turns the keeper out of its other face
  | CueShift !Era             -- ^ into this era
  | CuePickup                 -- ^ the power cell taken up
  | CueLamp                   -- ^ the lamp lit: the level won
  | CueLose
  | CueBump                   -- ^ walked into something that will not move
  | CueRefused                -- ^ a shift that is not allowed here
  | CueUndo
  | CueRestart
  | CueRoom                   -- ^ through a doorway into another room
  | CuePause !Bool            -- ^ paused, or resumed
  | CueMenuMove
  | CueMenuEnter
  | CueMenuBack
    -- The ending, in order.
  | CueFlare                  -- ^ the lamp flares, rising to the white
  | CueLampDies               -- ^ and goes out
  | CueLightGoes              -- ^ its light leaves through the wall
  | CueWallBreaks !Int        -- ^ the light breaks the passage's wall number n
  | CueRushBegins
  | CueBarrier                -- ^ held up at a barrier
  | CueBarrierPassed
  | CueCaught                 -- ^ fallen behind the light, taken back
  | CueLampCatches !Int       -- ^ the corridor's lamp number n catches as the keeper passes
  | CueThunder
  | CueArrival                -- ^ the real lighthouse
  | CueCard                   -- ^ the level beyond names itself
    -- The opening film and the courier's talk.
  | CueIntro                  -- ^ the opening film's soundtrack starts
  | CueIntroEnd               -- ^ and stops, skipped or over
  | CueTalkOpen               -- ^ the talk box pops up
  | CueTalkNext               -- ^ on to the next line
  | CueTalkBlip !Int          -- ^ a letter typed out: the how-manyth, for its pitch
  | CueTalkClose              -- ^ the talk box goes
  deriving (Eq, Show)

-- | In a level for two: the yellow keeper (W A S D, and the only one in any other
-- level) or the red one (the arrows).
data Keeper = Yellow | Red
  deriving (Eq, Show)

-- | The sounds one tick starts, from the run and its effects before and after.
cues :: [Level] -> (Run, Fx) -> (Run, Fx) -> [Cue]
cues levels (r, fx) (r', fx') = case (runMode r, runMode r') of
  (Title, Lobby) -> [CueMenuEnter]
  (Title, Intro _ _) -> [CueMenuEnter, CueIntro]
  (Intro _ _, InLevel) -> [CueIntroEnd]
  (Lobby, Title) -> [CueMenuBack]
  (Lobby, Lobby) -> [ CueMenuMove | runIndex r /= runIndex r' ]
  (Lobby, InLevel) -> [CueMenuEnter]
  (_, Lobby) -> [CueMenuBack]
  (InLevel, InLevel)
    | runIndex r /= runIndex r' -> [CueMenuEnter]  -- on from a won level
    | otherwise -> inLevel
  (InLevel, Ending _) -> inLevel                     -- the win that starts it
  (Ending f, Ending f') -> ending level f f' g g' fx' ++ walking f f'
  _ -> []
  where
    level = levels !! runIndex r'
    g = runGame r
    g' = runGame r'
    inLevel = playing level g g' fx fx'
    -- Walking the opened station is ordinary play, sounds and all.
    walking f f'
      | fnStage f == Beckon && fnStage f' `elem` [Beckon, Rush] = playing level g g' fx fx'
      | otherwise = []

-- | The sounds of one tick of play.
playing :: Level -> Game -> Game -> Fx -> Fx -> [Cue]
playing level g g' fx fx'
  | gAttempt g' /= gAttempt g = [CueRestart]
  | undone = [CueUndo]
  | otherwise = concat
      [ [ CuePause True | gPhase g' == Paused, gPhase g /= Paused ]
      , [ CuePause False | gPhase g == Paused, gPhase g' == Playing ]
      , [ CueShift (gEra g') | gEra g' /= gEra g ]
      , steps
      , [ CueMirrorPush | gMirrors g' /= gMirrors g ]
      , [ CuePush | gCrates g' /= gCrates g, Set.size (gFilled g') == Set.size (gFilled g) ]
      , [ CuePitFill | Set.size (gFilled g') > Set.size (gFilled g) ]
      , [ CuePickup | anyHasCell g', not (anyHasCell g) ]
      , [ CueLamp | gPhase g' == Won, gPhase g /= Won ]
      , [ CueLose | gPhase g' == Lost, gPhase g /= Lost ]
      , [ CueBump | bumped ]
      , [ CueRefused | refused ]
      , [ CueRoom | roomedLevel level, Just _ <- [fxRoomFade fx'], Nothing <- [fxRoomFade fx] ]
      ]
  where
    undone = length (gHistory g') < length (gHistory g)
    steps = keeperSteps Yellow (gEra g') (gCell g, gMotion g) (gCell g', gMotion g') (fxClock fx)
         ++ case (gPartner g, gPartner g') of
              (Just p, Just p') -> keeperSteps Red (gEra g') (ptCell p, ptMotion p) (ptCell p', ptMotion p') (fxClock fx + 1)
              _ -> []
    fresh = case gFeedback g' of
      Just f | maybe True (\o -> fbKind o /= fbKind f || fbLeft f > fbLeft o) (gFeedback g) -> Just (fbKind f)
      _ -> Nothing
    refused = case fresh of
      Just (ShiftBlocked _) -> True
      Just ShiftTooCostly -> True
      _ -> False
    bumpUp = gBump g' > gBump g
             || case (gPartner g, gPartner g') of
                  (Just p, Just p') -> ptBump p' > ptBump p
                  _ -> False
    bumped = bumpUp || case fresh of
      Just (PushBlocked _) -> True
      Just CrateRusted -> True
      Just MirrorFixed -> True
      Just MirrorFaceSide -> True
      _ -> False

-- | A keeper setting off: a step to the next cell, or a jump through a mirror. A
-- flat mirror puts the keeper straight on, behind it; a corner mirror turns them, so
-- they land off the line they set off along.
keeperSteps :: Keeper -> Era -> (Cell, Motion) -> (Cell, Motion) -> Int -> [Cue]
keeperSteps who era (_, before) (from@(x, y), now) foot = case now of
  Stepping to@(tx, ty) left
    | left == stepTicks && before /= now ->
        if apart from to > 1 then [CueJump (tx /= x && ty /= y)] else [CueStep era foot pan who]
  _ -> []
  where
    apart (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)
    -- Where in its room the keeper stands, for a room shown a room at a time.
    pan = 0.6 * (fromIntegral (x `mod` roomCols) - 6) / 6

-- | The ending's own moments, on the ticks the picture marks them.
ending :: Level -> Finale -> Finale -> Game -> Game -> Fx -> [Cue]
ending level f f' g g' fx' = case (fnStage f, fnStage f') of
  (Lit, Flare) -> [CueFlare]
  (Flare, Flare) | fnAge f' == lampDiesAt -> [CueLampDies]
  (Opening, Opening) ->
    [ CueLightGoes | fnAge f' == lightSetsOff ]
    ++ [ CueWallBreaks k | k <- [0 .. length (lvPassage level) - 1], fnAge f' == cellOpenTick level k ]
  (Beckon, Rush) -> [CueRushBegins]
  (Rush, Rush) -> rush ++ thunder
  (Rush, Arrival) -> [CueArrival]
  (Arrival, Arrival) -> [ CueCard | fnAge f' == 140 ] ++ thunder
  _ -> []
  where
    d = fnDash f
    d' = fnDash f'
    rush = concat
      [ [ CueCaught | dsCaught d == 0, dsCaught d' > 0 ]
      , [ CueShift (gEra g') | gEra g' /= gEra g, dsCaught d' == 0 ]
      , [ CueBarrier | dsBlocked d', not (dsBlocked d) ]
      , [ CueRefused | dsRefused d' > dsRefused d ]
      , [ CueBarrierPassed | dsCheck d' > dsCheck d ]
      , [ CueLampCatches n | dsCaught d' == 0, n <- [0 .. lampCount - 1]
                           , let at = lampAt level (gCell g') n
                           , dsAlong d < at, dsAlong d' >= at ]
      ]
    -- Thunder follows the lightning over the sea, in the present.
    thunder = [ CueThunder | gEra g' == Present, fxClock fx' `mod` 330 == 8 ]

-- | The corridor's lamps stand every four cells beyond the station, and each one
-- catches as the keeper comes within a cell and a half of it: at this far along
-- the rush, for a rush begun at @start@.
lampAt :: Level -> Cell -> Int -> Float
lampAt level start n = case finaleRun level of
  Just fr -> edgeFrom fr start + fromIntegral (2 + 4 * n) - 1.75
  Nothing -> 1 / 0

lampCount :: Int
lampCount = length [2 :: Int, 6 .. corridorLength - 1]

-- | How loud each looping bed should be, 0 to 1, and how fast it plays.
data Beds = Beds
  { bdSea       :: !Float  -- ^ the calm sea of the past
  , bdStorm     :: !Float  -- ^ wind and rain in the present
  , bdBeam      :: !Float  -- ^ the lamp's light running through the walls
  , bdDrone     :: !Float  -- ^ the way waiting to be walked
  , bdWind      :: !Float  -- ^ the air of the rush
  , bdWindRate  :: !Float
  , bdPulse     :: !Float  -- ^ the beat of the rush
  , bdPulseRate :: !Float
  } deriving (Eq, Show)

silence :: Beds
silence = Beds 0 0 0 0 0 1 0 1

beds :: [Level] -> Run -> Beds
beds levels r = case runMode r of
  Title -> weather 0.5
  -- The film brings its own rain.
  Intro _ _ -> silence
  Lobby -> weather 0.35
  InLevel
    | gPhase g == Paused -> weather 0.4
    | otherwise -> weather 1
  Ending f -> finaleBeds f
  where
    level = levels !! runIndex r
    g = runGame r
    weather k = case gEra g of
      Past -> silence { bdSea = 0.07 * k }
      Present -> silence { bdStorm = 0.06 * k }
    ramp a n = min 1 (max 0 (fromIntegral a / n))
    finaleBeds f = case fnStage f of
      Lit -> weather 1
      -- The weather drops away as the lamp dies, and the station holds its breath.
      Flare -> weather (1 - ramp (fnAge f) (fromIntegral lampDiesAt))
      Opening ->
        let lightGone = lightSetsOff + lightCellTicks * length (wayCells level)
            a = fnAge f
        in silence { bdBeam = if a >= lightSetsOff && a < lightGone then 0.32 * ramp (a - lightSetsOff) 10 else 0 }
      Beckon -> (weather (0.6 * ramp (fnAge f) 120)) { bdDrone = 0.4 * ramp (fnAge f) 90 }
      Rush ->
        let d = fnDash f
            speedK = min 1 (dsSpeed d / dashTopSpeed)
            paceK = screenSpeed (dsClock d) / dashTopSpeed
            caughtK = if dsCaught d > 0 then 0.8 else 0
            k = 1 - caughtK
        in (weather (1.3 * k))
             { bdDrone = 0.3 * k
             , bdWind = (0.08 + 0.36 * speedK) * k
             , bdWindRate = 0.7 + 0.8 * speedK
             , bdPulse = 0.45 * ramp (dsClock d) 50 * k
             , bdPulseRate = 0.85 + 0.35 * paceK }
      -- Everything falls away under the real lamp's chord.
      Arrival ->
        let k = 1 - ramp (fnAge f) 60
        in (weather (1.3 * (1 - ramp (fnAge f) 110)))
             { bdDrone = 0.3 * k, bdWind = 0.16 * k, bdWindRate = 0.9
             , bdPulse = 0.45 * k, bdPulseRate = 1.2 }
