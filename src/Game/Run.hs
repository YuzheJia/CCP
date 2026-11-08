-- | Campaign navigation around the pure rules for one level, and the one ending
-- that is not a level of its own: the lamp that goes out again.
module Game.Run
  ( RunMode (..)
  , Run (..)
  , startRun
  , startRunWithProgress
  , startRunAt
  , stepRun
  , lastVisible
    -- * The opening
  , introSkipTicks
  , finishIntro
    -- * The way on
  , Stage (..)
  , Finale (..)
  , finale
  , litTicks, flareTicks, openTicks, arrivalTicks
  , lightSetsOff, lightCellTicks
  , lampDiesAt, revealTicks, cellOpenTick
  , hasEnding
  , revealedLevel
  , partlyRevealed
  , FinaleRun (..)
  , wayCells
  , rushLength
  , finaleRun
  , corridorLength
  , dashLevel
  , Dash (..)
  , restingDash
  , stepDash
  , barriers
  , barriersFrom
  , edgeFrom
  , inBarrier
  , outFrom
  , screenSpeed
  , dashTopSpeed
  , catchTicks
  , caughtGap
  , rushInputs
  , beckonKeys
  ) where

import qualified Data.Set as Set
import Game.Fx (roomCols, roomRows, roomOf)
import Game.Logic
import Game.Types

-- | Where the run is: the title, the opening film, the level hall, a level, or a
-- level's ending. The opening counts ticks since it began, and how long Enter (or
-- Space) has been held down to skip it.
data RunMode = Title | Intro !Int !Int | Lobby | InLevel | Ending !Finale
  deriving (Eq, Ord, Show)

-- | @runCompleted@ is the number of campaign levels cleared. @runUnlocked@ is
-- the highest level index the player may choose. The IO boundary persists the
-- completion count and selection; puzzle board state stays session-local.
data Run = Run
  { runIndex    :: !Int
  , runUnlocked :: !Int
  , runCompleted :: !Int
  , runMode     :: !RunMode
  , runGame     :: !Game
  } deriving (Eq, Show)

-- | The last level the hall lists. Secret levels come after all the others and are
-- never listed, never selected and never offered, however far the player has got.
lastVisible :: [Level] -> Int
lastVisible levels = max 0 (length (takeWhile (not . lvSecret) levels) - 1)

-- | A new campaign starts at the title with only its first level unlocked.
startRun :: [Level] -> Int -> Run
startRun levels selected = startRunWithProgress levels selected 0

-- | Restore a validated lobby selection and unlock frontier.
startRunWithProgress :: [Level] -> Int -> Int -> Run
startRunWithProgress levels selected completed =
  Run i u c Title (initialGame (levels !! i))
  where
    c = clamp 0 (length levels) completed
    u = min (lastVisible levels) c
    i = clamp 0 u selected

-- | Like 'startRunWithProgress', but a secret level asked for by number is opened
-- straight away, since the hall would never offer it.
startRunAt :: [Level] -> Int -> Int -> Run
startRunAt levels i completed
  | i >= 0 && i < length levels && lvSecret (levels !! i) =
      Run i (min (lastVisible levels) c) c InLevel
          ((initialGame (levels !! i)) { gPhase = Playing, gAttempt = 1 })
  | otherwise = startRunWithProgress levels i completed
  where c = clamp 0 (length levels) completed

-- | One fixed tick. Enter moves from the title into the lobby, where Escape goes
-- back to the title; a campaign with nothing cleared yet plays the opening film
-- instead, and goes from it straight into the first level. Escape from a level
-- returns to the lobby. Winning unlocks the next room immediately, and Enter
-- continues there. A level with a passage does not wait for Enter: its ending runs,
-- and leads into the secret level after it.
stepRun :: [Level] -> TickInput -> Run -> Run
stepRun levels input run = case runMode run of
  Title    -> stepTitle
  Intro age held -> stepIntro age held
  Lobby    -> stepLobby
  InLevel  -> stepLevel
  Ending f -> stepEnding f
  where
    i = runIndex run
    u = runUnlocked run
    c = runCompleted run
    g = runGame run
    prev = gPrev g
    shown = min i (lastVisible levels)
    freshEnter = tiEnter input && not (tiEnter prev)
    freshLobby = tiLobby input && not (tiLobby prev)
    freshDir
      | tiDir input /= tiDir prev && not (tiDirRevealed input) = tiDir input
      | otherwise = Nothing
    selected = case freshDir of
      Just DirUp    -> max 0 (i - 1)
      Just DirLeft  -> max 0 (i - 1)
      Just DirDown  -> min u (i + 1)
      Just DirRight -> min u (i + 1)
      Nothing       -> i
    selectedGame = (initialGame (levels !! selected))
      { gAttempt = gAttempt g + if selected /= i then 1 else 0
      , gPrev = input
      }
    begin level = (initialGame level)
      { gPhase = Playing, gAttempt = gAttempt g + 1, gPrev = input }
    toLobby = Run shown u c Lobby ((initialGame (levels !! shown))
      { gAttempt = gAttempt g + 1, gPrev = input })
    toTitle = Run shown u c Title ((initialGame (levels !! shown))
      { gAttempt = gAttempt g + 1, gPrev = input })

    -- I opens and shuts the instructions; while they are open, Enter does nothing.
    stepTitle
      |tiInstructions input && not (tiInstructions prev)=
        run {runGame = g
        {gShowInstructions = not (gShowInstructions g)
        , gPrev = input
        }}
      |gShowInstructions g = run {runGame = g {gPrev = input}}
      | freshEnter && c == 0 = Run 0 u c (Intro 0 0) ((initialGame (levels !! 0))
          { gAttempt = gAttempt g + 1, gPrev = input })
      | freshEnter = toLobby
      | otherwise  = run { runGame = g { gPrev = input } }

    -- Escape skips the film at once; Enter or Space skips it once held down for
    -- 'introSkipTicks', so a tap meant for something else cannot. Only a hold that
    -- starts during the film counts, not the Enter still down from the title.
    stepIntro age held
      | freshLobby || held' >= introSkipTicks = Run 0 u c InLevel (begin (levels !! 0))
      | otherwise = run { runMode = Intro (age + 1) held', runGame = g { gPrev = input } }
      where
        freshShift = tiShift input && not (tiShift prev)
        holding = tiEnter input || tiShift input
        held'
          | holding && (held > 0 || freshEnter || freshShift) = held + 1
          | otherwise = 0

    stepLobby
      | freshLobby = toTitle
      | freshEnter = Run i u c InLevel (begin (levels !! i))
      | otherwise  = Run selected u c Lobby selectedGame

    stepLevel
      | freshLobby = toLobby
      | gPhase g == Won && freshEnter =
          if i + 1 < length levels && not (lvSecret (levels !! (i + 1)))
            then let next = i + 1
                 in Run next (max u next) (max c (i + 1)) InLevel (begin (levels !! next))
            else toLobby
      | otherwise =
          let g' = stepGame (levels !! i) input g
              wonNow = gPhase g' == Won && gPhase g /= Won
              completed' = if gPhase g' == Won then max c (i + 1) else c
              unlocked' = min (lastVisible levels) completed'
              mode = if wonNow && hasEnding levels i then Ending (finale Lit 0) else InLevel
          in Run i unlocked' completed' mode g'

    stepEnding f
      | freshLobby = toLobby
      | otherwise = case advanceFinale (levels !! i) input f g of
          Just (f', g') -> Run i u c (Ending f') g'
          Nothing -> let next = i + 1 in Run next u c InLevel (begin (levels !! next))

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- The opening ---------------------------------------------------------------------
--
-- The film itself belongs to the app, which knows how long it is; the run only
-- counts its ticks and takes the keys that skip it.

-- | How long Enter or Space must be held to skip the opening film.
introSkipTicks :: Int
introSkipTicks = 40

-- | The opening film has ended: the first level begins. Keys held as it ends need a
-- fresh press. Anything other than the opening is left as it is.
finishIntro :: [Level] -> Run -> Run
finishIntro levels run = case runMode run of
  Intro _ _ ->
    let g = runGame run
    in run { runIndex = 0, runMode = InLevel
           , runGame = (initialGame (levels !! 0))
               { gPhase = Playing, gAttempt = gAttempt g + 1, gPrev = gPrev g } }
  _ -> run

-- The way on --------------------------------------------------------------------
--
-- The last lamp of the hall is lit, the plate says every lamp is lit, and then the
-- light is taken back: it flares and goes out, slips into the wall beside the tower,
-- and runs off across the station, breaking every wall in its way. The keeper
-- follows on foot, is swept along the way it opened faster and faster, out of the
-- station and down a long corridor, and arrives at the real lighthouse, which is
-- where the secret level begins.

-- | The parts of the ending, in order.
data Stage
  = Lit      -- ^ the lamp burns, and every lamp seems to be lit
  | Flare    -- ^ it flares white and goes out
  | Opening  -- ^ its light runs off through the walls, breaking them open
  | Beckon   -- ^ the keeper walks again, free; the way waits
  | Rush     -- ^ on the way: carried along it, faster and faster
  | Arrival  -- ^ the real lighthouse, then the level beyond it
  deriving (Eq, Ord, Show, Enum, Bounded)

data Finale = Finale
  { fnStage :: !Stage
  , fnAge   :: !Int   -- ^ ticks into the stage
  , fnDash  :: !Dash  -- ^ the rush, once it has begun
  } deriving (Eq, Ord, Show)

-- | A stage of the ending, from its start.
finale :: Stage -> Int -> Finale
finale stage age = Finale stage age restingDash

-- | How long each stage with a fixed length lasts, in ticks.
litTicks, flareTicks, arrivalTicks :: Int
litTicks = 210
flareTicks = 80
arrivalTicks = 230

-- | The tick of the flare at which the lamp goes dark, under the brightest light.
lampDiesAt :: Int
lampDiesAt = 24

-- | The light leaves the wall beside the tower this far into 'Opening', and then
-- crosses one cell of the way every 'lightCellTicks'.
lightSetsOff, lightCellTicks :: Int
lightSetsOff = 26
lightCellTicks = 8

-- | When cell @k@ of the passage breaks open, in ticks into 'Opening': as the light
-- reaches it. It cracks for a while before that.
cellOpenTick :: Level -> Int -> Int
cellOpenTick level k = case lvPassage level of
  mouth : _ | k < length (lvPassage level) ->
    lightSetsOff + lightCellTicks * apart mouth (lvPassage level !! k)
  _ -> 0
  where apart (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

-- | 'Opening' lasts until the light has left the station, and a little longer.
openTicks :: Level -> Int
openTicks level = lightSetsOff + lightCellTicks * length (wayCells level) + 36

-- | Ticks from the win until the keeper may walk again.
revealTicks :: Level -> Int
revealTicks level = litTicks + flareTicks + openTicks level

-- | A level runs its ending when it has a passage and a level waits beyond it.
hasEnding :: [Level] -> Int -> Bool
hasEnding levels i = not (null (lvPassage (levels !! i))) && i + 1 < length levels

-- | The won level with its passage open, as the keeper walks it: free to walk, and
-- with nothing left to buy.
revealedLevel :: Level -> Level
revealedLevel level = (partlyRevealed (length (lvPassage level)) level)
  { lvRules = (lvRules level) { ruStepCost = 0, ruShiftCost = 0, ruJumpCost = 0, ruEchoCost = 0 } }

-- | The level with the first @n@ cells of its passage open, in the present.
partlyRevealed :: Int -> Level -> Level
partlyRevealed n level =
  level { lvPresent = lvPresent level `Set.union` Set.fromList (take n (lvPassage level)) }

-- | Walking in the opened station: no undo, no restart, no echo, no peek. The era
-- can still change, for free: some corners of a won station (a pocket behind a
-- one-way mirror) are only left through the other era. The level is won; only the
-- way on is left.
beckonKeys :: TickInput -> TickInput
beckonKeys i = i { tiUndo = False, tiRestart = False, tiEcho = False, tiPeek = False }
-- R is read by the ending itself: it brings the keeper back to the lamp.

-- | One tick of the ending. Nothing once it is over and the next level should begin.
advanceFinale :: Level -> TickInput -> Finale -> Game -> Maybe (Finale, Game)
advanceFinale level input (Finale stage age dash) g = case stage of
  Lit -> timed litTicks Flare held
  Flare -> timed flareTicks Opening held
  Opening
    | age + 1 >= openTicks level -> Just (Finale Beckon 0 dash, freed)
    | otherwise -> Just (Finale Opening (age + 1) dash, held)
  Beckon
    -- R brings the keeper back to the lamp, whatever corner they are in.
    | tiRestart input && not (tiRestart (gPrev g)) ->
        Just (Finale Beckon (age + 1) dash, freed { gCell = lvGoal level, gEra = Present })
    | otherwise ->
    let g' = stepGame (revealedLevel level) (beckonKeys input) g
        -- A step that lands on the way starts the rush there, even with a key held
        -- that has already set off the next step: that step is not taken.
        landed = gCell g' /= gCell g && gCell g' `elem` wayCells level
        arrived = g' { gMotion = Settled, gPushing = Nothing, gPendingMove = Nothing, gArmed = False }
    in Just $ if onTheWay level g' then (Finale Rush 0 restingDash, g')
              else if landed && gEra g' == Present && gPhase g' == Playing then (Finale Rush 0 restingDash, arrived)
              else (Finale Beckon (age + 1) dash, g')
  Rush -> case finaleRun level of
    Nothing -> Just (Finale Arrival 0 dash, held)
    Just fr -> case stepDash fr (gCell g) input g dash of
      Nothing -> Just (Finale Arrival 0 dash, held)
      Just (dash', g') -> Just (Finale Rush (age + 1) dash', g' { gPrev = input })
  Arrival
    | age + 1 >= arrivalTicks -> Nothing
    | otherwise -> Just (Finale Arrival (age + 1) dash, held)
  where
    held = g { gPrev = input }
    timed n next g' = Just (if age + 1 >= n then Finale next 0 dash else Finale stage (age + 1) dash, g')
    -- The cell is spent, and the light broke whatever stood on its way, crates too.
    -- Keys held through the ending need a fresh press. The keeper looks the way the
    -- light went, as they watched it go.
    freed = g
      { gPhase = Playing, gHistory = [], gHasCell = False, gMotion = Settled
      , gPushing = Nothing, gFacing = maybe DirLeft frDir (finaleRun level)
      , gFeedback = Nothing, gBump = 0
      , gCrates = gCrates g `Set.difference` Set.fromList (wayCells level)
      , gPendingShift = False, gPendingMove = Nothing, gPendingUndo = False
      , gPendingEcho = False, gArmed = False, gPrev = input }

-- | The rush starts as soon as the keeper stands anywhere on the way, in the present.
onTheWay :: Level -> Game -> Bool
onTheWay level g = gPhase g == Playing && gMotion g == Settled && gEra g == Present
                   && gCell g `elem` wayCells level

-- | Where the rush goes. The passage starts in the wall beside the lighthouse, then
-- runs straight to the edge of the station; the light crosses everything between,
-- and that whole line is the way. Beyond the map a corridor carries straight on,
-- and opens into the chamber of the real lighthouse.
data FinaleRun = FinaleRun
  { frMouth   :: !Cell  -- ^ the first cell of the passage, beside the lighthouse
  , frEdge    :: !Cell  -- ^ the last, in the outer wall of the station
  , frDir     :: !Dir   -- ^ the way the rush goes, from the mouth to the edge
  , frArrival :: !Cell  -- ^ where the keeper comes to rest
  , frTower   :: !Cell  -- ^ the real lighthouse, just beyond
  } deriving (Eq, Show)

-- | Cells of corridor beyond the edge of the map, before the chamber.
corridorLength :: Int
corridorLength = 54

finaleRun :: Level -> Maybe FinaleRun
finaleRun level = case lvPassage level of
  mouth@(mx, my) : rest@(_ : _) ->
    let edge@(ex, ey) = last rest
        dir | ex < mx = DirLeft
            | ex > mx = DirRight
            | ey < my = DirUp
            | otherwise = DirDown
        ahead n = let (dx, dy) = dirDelta dir in (ex + dx * n, ey + dy * n)
    in Just FinaleRun
         { frMouth = mouth, frEdge = edge, frDir = dir
         , frArrival = ahead (corridorLength + 4)
         , frTower = ahead (corridorLength + 5)
         }
  _ -> Nothing

-- | Every cell the light crosses in the station, from the mouth to the edge: the
-- passage it broke open and the floor between.
wayCells :: Level -> [Cell]
wayCells level = case finaleRun level of
  Just fr ->
    let (dx, dy) = dirDelta (frDir fr)
        (mx, my) = frMouth fr
        (ex, ey) = frEdge fr
        n = abs (ex - mx) + abs (ey - my)
    in [ (mx + dx * i, my + dy * i) | i <- [0 .. n] ]
  Nothing -> []

-- | Cells from a start on the way to where the rush comes to rest.
rushLength :: FinaleRun -> Cell -> Int
rushLength fr (sx, sy) = let (ax, ay) = frArrival fr in abs (ax - sx) + abs (ay - sy)

-- | Where the causeway is blocked, counting cells out from the station's edge, and
-- the era it is blocked in: rubble fallen in the present, a shut gate in the past.
-- They alternate, so the keeper keeps changing era to get through, and come closer
-- together as the run speeds up.
barriers :: [(Int, Era)]
barriers =
  [ (5, Present), (11, Past), (16, Present), (21, Past), (25, Present), (29, Past)
  , (33, Present), (36, Past), (40, Present), (43, Past), (47, Present) ]

-- | Cells out from the edge, @across@ to one side of the line.
outFrom :: FinaleRun -> Int -> Int -> Cell
outFrom fr along across =
  let (dx, dy) = dirDelta (frDir fr)
      (ex, ey) = frEdge fr
  in (ex + dx * along + dy * across, ey + dy * along + dx * across)

-- | The whole way, as one map, for drawing the rush: the lighthouse's room with its
-- passage open; then, out over the sea, the causeway three cells wide with its
-- barriers; and the island of the real lighthouse, seven by seven. Outside the
-- station, whatever is not on this map is open sea.
dashLevel :: Level -> Level
dashLevel level = case finaleRun level of
  Nothing -> level
  Just fr ->
    let (rx, ry) = roomOf (lvGoal level)
        nearHome (x, y) = x >= rx * roomCols - 1 && x <= (rx + 1) * roomCols
                          && y >= ry * roomRows - 1 && y <= (ry + 1) * roomRows
        causeway era = Set.fromList
          [ outFrom fr i j | i <- [1 .. corridorLength], j <- [-1 .. 1]
          , (i, era) `notElem` barriers ]
        island = Set.fromList [ outFrom fr i j | i <- [corridorLength + 1 .. corridorLength + 7], j <- [-3 .. 3] ]
    in level
         { lvPresent = Set.unions [Set.filter nearHome (lvPresent (revealedLevel level)), causeway Present, island]
         , lvPast = Set.unions [Set.filter nearHome (lvPast level), causeway Past, island]
         }

-- The rush ---------------------------------------------------------------------
--
-- Once on the way, the keeper runs by themselves and the screen is carried along
-- ahead of them, ever faster. Out on the causeway, barriers stand in one era or the
-- other: Space changes era, for free, to find the one where the way is open. A
-- keeper held up too long falls off the back of the screen, and starts again from
-- the last barrier they passed.

-- | The run so far.
data Dash = Dash
  { dsAlong    :: !Float  -- ^ cells the keeper has come from where the rush began
  , dsSpeed    :: !Float  -- ^ the keeper's speed, cells per tick
  , dsScroll   :: !Float  -- ^ how far the screen has been carried, in cells
  , dsClock    :: !Int    -- ^ ticks of carrying, which set the screen's speed
  , dsCheck    :: !Float  -- ^ where a keeper left behind starts again
  , dsCheckEra :: !Era
  , dsCaught   :: !Int    -- ^ ticks left of being taken back, 0 while running
  , dsRefused  :: !Int    -- ^ ticks left of showing a shift that was refused
  , dsShifted  :: !Int    -- ^ ticks since the era last changed
  , dsBlocked  :: !Bool   -- ^ standing at a barrier, unable to go on
  , dsFails    :: !Int    -- ^ times taken back
  } deriving (Eq, Ord, Show)

-- | At rest, before the rush.
restingDash :: Dash
restingDash = Dash 0 0 0 0 0 Present 0 0 99 False 0

-- | The screen's speed: from a walk to a sprint, then steady.
screenSpeed :: Int -> Float
screenSpeed clock = min dashTopSpeed (0.05 + 0.00045 * fromIntegral clock)

dashTopSpeed :: Float
dashTopSpeed = 0.165

-- | How far ahead of the screen the keeper may run, and how far behind it they may
-- fall before they are taken back. The ticks a taking back lasts; the keeper is
-- moved halfway through, while the screen is dark.
leadMax, caughtGap :: Float
leadMax = 4
caughtGap = 5.4

catchTicks :: Int
catchTicks = 50

-- | Cells from a start on the way to the station's edge.
edgeFrom :: FinaleRun -> Cell -> Float
edgeFrom fr start = fromIntegral (rushLength fr start - corridorLength - 4)

-- | Where each barrier stands, in cells from the start, and its era.
barriersFrom :: FinaleRun -> Cell -> [(Float, Era)]
barriersFrom fr start = [ (edgeFrom fr start + fromIntegral i, e) | (i, e) <- barriers ]

-- | Whether the keeper, this far along, stands where a barrier of this era is.
inBarrier :: FinaleRun -> Cell -> Era -> Float -> Bool
inBarrier fr start era along = any (\(a, e) -> e == era && abs (along - a) < 0.75) (barriersFrom fr start)

-- | One tick of the rush. Nothing once the keeper has arrived.
stepDash :: FinaleRun -> Cell -> TickInput -> Game -> Dash -> Maybe (Dash, Game)
stepDash fr start input g d
  | dsCaught d > 0 =
      let left = dsCaught d - 1
          waiting = d { dsCaught = left, dsRefused = 0 }
      in Just $ if left == catchTicks `div` 2
           then ( waiting { dsAlong = dsCheck d, dsScroll = dsCheck d - 1, dsSpeed = 0
                     , dsClock = clockFor 0.08, dsBlocked = False, dsShifted = 99 }
                , g { gEra = dsCheckEra d } )
           else (waiting, g)
  | dsAlong d >= len - 0.001 = Nothing
  | otherwise = Just (d', g { gEra = era' })
  where
    len = fromIntegral (rushLength fr start) :: Float
    clockFor :: Float -> Int
    clockFor speed = round ((speed - 0.05) / 0.00045)
    fresh = tiShift input && not (tiShift (gPrev g))
    -- The era changes only out on the causeway, and only where the other era has
    -- floor under the keeper.
    outside = dsAlong d > edgeFrom fr start + 0.3
    target = otherEra (gEra g)
    canShift = outside && not (inBarrier fr start target (dsAlong d))
    era' = if fresh && canShift then target else gEra g
    refused = if fresh && not canShift then 24 else max 0 (dsRefused d - 1)
    v = screenSpeed (dsClock d)
    -- The screen stops where it frames the real lighthouse.
    scroll' = min (len - 3) (dsScroll d + v)
    -- The keeper runs a little faster than the screen, up to a lead, and slows to
    -- a stop in front of the real lighthouse.
    want | dsAlong d > dsScroll d + leadMax = v
         | otherwise = v * 1.3 + 0.012
    approach s w = if s < w then min w (s + 0.006) else max w (s - 0.01)
    speed0 = min (approach (dsSpeed d) want) (max 0.015 ((len - dsAlong d) * 0.05))
    proposed = min len (dsAlong d + speed0)
    stops = [ a - 0.7 | (a, e) <- barriersFrom fr start, e == era'
                      , dsAlong d <= a - 0.7 + 0.001, proposed > a - 0.7 ]
    (along', speed', blocked) = case stops of
      [] -> (proposed, speed0, False)
      _ -> (max (dsAlong d) (minimum stops), 0, True)
    -- Passing a barrier is progress kept: a keeper taken back starts just beyond it.
    passed = [ a | (a, _) <- barriersFrom fr start, dsAlong d < a + 0.75, along' >= a + 0.75 ]
    (check', checkEra') = case passed of
      (a : _) -> (a + 1, era')
      [] -> (dsCheck d, dsCheckEra d)
    caught = scroll' - along' > caughtGap
    d' = d { dsAlong = along', dsSpeed = speed', dsScroll = scroll', dsClock = dsClock d + 1
           , dsCheck = check', dsCheckEra = checkEra'
           , dsCaught = if caught then catchTicks else 0
           , dsFails = dsFails d + (if caught then 1 else 0)
           , dsRefused = refused
           , dsShifted = if era' /= gEra g then 0 else dsShifted d + 1
           , dsBlocked = blocked }

-- | Whether a keeper who reads the barriers ahead would press Space now: just before
-- one that stands in this era, if the other era is open here. For the demo.
dashWantsShift :: FinaleRun -> Cell -> Era -> Dash -> Bool
dashWantsShift fr start era d =
  dsCaught d == 0 && dsAlong d > edgeFrom fr start + 0.3
  && not (inBarrier fr start (otherEra era) (dsAlong d))
  && any (\(a, e) -> e == era && a - 0.7 - dsAlong d < 1.4 && a - 0.7 - dsAlong d >= -0.01) (barriersFrom fr start)

-- | The keys a keeper who reads the barriers would press through the whole rush,
-- from the mouth of the passage, until they arrive. For the demo.
rushInputs :: Level -> [TickInput]
rushInputs level = case (finaleRun level, wayCells level) of
  (Just fr, start : _) -> go fr start (startGame start) restingDash False (0 :: Int)
  _ -> []
  where
    startGame start = (initialGame level) { gCell = start, gEra = Present, gPrev = noInput }
    go fr start g d pressedLast n
      | n > 20000 = []
      | otherwise =
          let press = not pressedLast && dashWantsShift fr start (gEra g) d
              input = noInput { tiShift = press }
          in case stepDash fr start input g d of
               Nothing -> [input]
               Just (d', g') -> input : go fr start (g' { gPrev = input }) d' press (n + 1)
