-- | Her orders: the saves. The courier carries each save as an order, kept on a
-- ticket: how far along the route it is (rooms cleared, the room chosen last), how
-- long she has been on the road, how often she changed era, and when it was last
-- saved. Three tickets lie on the desk.
--
-- Pure, like the rules. The app reads and writes the book, draws the desk and the
-- tickets, and stamps the time of each save.
--
-- The desk follows what players of other games already know: left and right
-- choose, Enter continues or takes a new order, Escape goes back. Cancelling an
-- order is the one thing that cannot be undone, so it takes two steps: Delete
-- brings the ticket forward and she says what would be lost, and then Enter must
-- be held, as it is to skip the opening film, while the ticket tears along its
-- perforation. Letting go stops the tear; Escape keeps the order.
module Game.Orders
  ( -- * The book of orders
    Order (..)
  , Stamp (..)
  , Book (..)
  , slotCount
  , emptyBook
  , newOrder
  , activeOrder
  , orderAt
  , anyOrders
  , normaliseBook
  , bookFromProgress
  , ensureActive
  , withProgress
  , stampActive
  , durableKey
  , delivered
    -- * The desk
  , DeskStep (..)
  , Desk (..)
  , Note (..)
  , closedDesk
  , openDesk
  , liftOf
  , focusOf
  , askTyped
  , holdTicks, tearTicks, printTicks, forwardTicks, noteTicks, liftTicks, askPopTicks
    -- * Stepping
  , Campaign (..)
  , stepCampaign
  , tally
  , deskCues
  , cancelLine
  ) where

import Data.Char (isSpace)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, isJust)
import Game.Logic (initialGame)
import Game.Run
import Game.Sound (Cue (..))
import Game.Types

-- The book of orders ------------------------------------------------------------

-- | When an order was last saved, in local time, as the ticket prints it.
data Stamp = Stamp
  { smMonth  :: !Int  -- ^ 1 to 12
  , smDay    :: !Int
  , smHour   :: !Int
  , smMinute :: !Int
  } deriving (Eq, Read, Show)

data Order = Order
  { odCompleted :: !Int            -- ^ rooms cleared, as 'runCompleted'
  , odSelected  :: !Int            -- ^ the room chosen last, as 'runIndex'
  , odTicks     :: !Int            -- ^ ticks on the road: in the hall, a room or an ending
  , odShifts    :: !Int            -- ^ times she changed era in a room
  , odSaved     :: !(Maybe Stamp)  -- ^ when it was last saved
  } deriving (Eq, Read, Show)

-- | Every slot on the desk, empty or holding an order, and the one in play.
data Book = Book
  { bkOrders :: ![Maybe Order]
  , bkActive :: !Int
  } deriving (Eq, Read, Show)

slotCount :: Int
slotCount = 3

emptyBook :: Book
emptyBook = Book (replicate slotCount Nothing) 0

newOrder :: Order
newOrder = Order 0 0 0 0 Nothing

orderAt :: Int -> Book -> Maybe Order
orderAt k b = case drop k (bkOrders b) of
  o : _ | k >= 0 -> o
  _ -> Nothing

activeOrder :: Book -> Maybe Order
activeOrder b = orderAt (bkActive b) b

anyOrders :: Book -> Bool
anyOrders = any isJust . bkOrders

setSlot :: Int -> Maybe Order -> Book -> Book
setSlot k o b = b { bkOrders = [ if i == k then o else x | (i, x) <- zip [0 ..] (bkOrders b) ] }

modifyActive :: (Order -> Order) -> Book -> Book
modifyActive f b = setSlot (bkActive b) (fmap f (activeOrder b)) b

-- | Exactly 'slotCount' slots, every order's progress inside the campaign of
-- @levelCount@ levels, and the active slot one of them.
normaliseBook :: Int -> Book -> Book
normaliseBook levelCount b = Book orders (clamp 0 (slotCount - 1) (bkActive b))
  where
    orders = take slotCount (map (fmap fix) (bkOrders b) ++ repeat Nothing)
    fix o =
      let c = clamp 0 levelCount (odCompleted o)
          highest = min (max 0 (levelCount - 1)) c
      in o { odCompleted = c, odSelected = clamp 0 highest (odSelected o)
           , odTicks = max 0 (odTicks o), odShifts = max 0 (odShifts o) }

-- | The one save the game kept before it had orders, as the first order.
bookFromProgress :: Int -> Int -> Book
bookFromProgress completed selected =
  setSlot 0 (Just newOrder { odCompleted = completed, odSelected = selected }) emptyBook

-- | The active slot holds an order, a new one if it was empty.
ensureActive :: Book -> Book
ensureActive b
  | isJust (activeOrder b) = b
  | otherwise = setSlot (bkActive b) (Just newOrder) b

-- | The order in play, made if need be, with these rooms cleared and this room
-- chosen. For opening the game on a level during development.
withProgress :: Int -> Int -> Book -> Book
withProgress completed selected =
  modifyActive (\o -> o { odCompleted = completed, odSelected = selected }) . ensureActive

stampActive :: Stamp -> Book -> Book
stampActive s = modifyActive (\o -> o { odSaved = Just s })

-- | What must reach the disk at once when it changes. The time on the road is left
-- out: it changes every tick, and is written now and then instead.
durableKey :: Book -> (Int, [Maybe (Int, Int, Int)])
durableKey b = (bkActive b, map (fmap (\o -> (odCompleted o, odSelected o, odShifts o))) (bkOrders b))

-- | Every room the hall lists has been cleared: the wick is at the lamp.
delivered :: [Level] -> Order -> Bool
delivered levels o = odCompleted o > lastVisible levels

-- The desk -------------------------------------------------------------------------

data DeskStep
  = Choosing
  | Asking !Int !Int  -- ^ ticks since she asked; ticks Enter has been held to tear
  | Tearing !Int      -- ^ ticks into tearing the order off
  | Printing !Int     -- ^ ticks into printing a new order
  deriving (Eq, Show)

-- | A word under the tickets for a while.
newtype Note = Cancelled Int  -- ^ this order number has just been cancelled
  deriving (Eq, Show)

data Desk = Desk
  { dkSel   :: !Int                  -- ^ the ticket chosen
  , dkFrom  :: !Int                  -- ^ the ticket chosen before it
  , dkMoved :: !Int                  -- ^ ticks since the choice changed
  , dkAge   :: !Int                  -- ^ ticks since the desk opened
  , dkStep  :: !DeskStep
  , dkFocus :: !Int                  -- ^ 0 to 'forwardTicks': how far forward a ticket in question is
  , dkNote  :: !(Maybe (Note, Int))  -- ^ and ticks left to show it
  } deriving (Eq, Show)

-- | Ticks Enter must be held to tear an order off; the tear itself; the printing of
-- a new order; a ticket coming forward to be asked about; a note; a ticket lifting
-- when chosen; and the talk box popping up before her line types.
holdTicks, tearTicks, printTicks, forwardTicks, noteTicks, liftTicks, askPopTicks :: Int
holdTicks = 60
tearTicks = 50
printTicks = 70
forwardTicks = 14
noteTicks = 150
liftTicks = 8
askPopTicks = 9

closedDesk :: Desk
closedDesk = Desk 0 0 liftTicks 0 Choosing 0 Nothing

-- | The desk as it opens: the order last in play is chosen, or failing that the
-- first order there is.
openDesk :: Book -> Desk
openDesk b = closedDesk { dkSel = s, dkFrom = s }
  where
    s | isJust (activeOrder b) = bkActive b
      | otherwise = fromMaybe (bkActive b) (findIndex isJust (bkOrders b))

-- | How far ticket @k@ is lifted, 0 to 1: the chosen one rises, the one left sinks.
liftOf :: Desk -> Int -> Float
liftOf d k
  | k == dkSel d = e
  | k == dkFrom d = 1 - e
  | otherwise = 0
  where e = ease (fromIntegral (dkMoved d) / fromIntegral liftTicks)

-- | How far forward the chosen ticket has come to be asked about, 0 to 1.
focusOf :: Desk -> Float
focusOf d = ease (fromIntegral (dkFocus d) / fromIntegral forwardTicks)

ease :: Float -> Float
ease t = let u = max 0 (min 1 t) in u * u * (3 - 2 * u)

-- | Letters of her question typed this many ticks after she asked: as her talk
-- types, once the box is up.
askTyped :: Int -> Int
askTyped age = max 0 (age - askPopTicks) * 4 `div` 5

-- | What she says about cancelling order number @n@.
cancelLine :: [Level] -> Int -> Order -> String
cancelLine levels n o
  | delivered levels o =
      "Cancel ORDER " ++ show n ++ "? It's DELIVERED: all " ++ count (lastVisible levels + 1)
      ++ " rooms and " ++ time ++ " on the road. HOLD ENTER to tear it up. ESC keeps it."
  | otherwise =
      "Cancel ORDER " ++ show n ++ "? That's " ++ rooms ++ " and " ++ time
      ++ " on the road, gone for good. HOLD ENTER to tear it up. ESC keeps it."
  where
    rooms = case odCompleted o of
      0 -> "no rooms yet"
      1 -> "one room"
      c -> count c ++ " rooms"
    minutes = odTicks o `div` (ticksPerSecond * 60)
    time = case minutes of
      0 -> "not a minute"
      1 -> "a minute"
      m -> show m ++ " minutes"
    count c = case c of
      2 -> "two"
      3 -> "three"
      4 -> "four"
      5 -> "five"
      _ -> show c

-- What leaves the desk, and where to.
data Outcome = Continue !Int | Begin !Int | Back | Torn !Int

stepDesk :: TickInput -> TickInput -> Book -> Desk -> (Desk, Maybe Outcome)
stepDesk input prev book d = case dkStep d of
  Choosing
    | fresh tiLobby -> (aged, Just Back)
    | Just dir <- freshDir, dir `elem` [DirLeft, DirUp] -> (choose (sel - 1), Nothing)
    | Just dir <- freshDir, dir `elem` [DirRight, DirDown] -> (choose (sel + 1), Nothing)
    | fresh tiEnter -> if isJust (orderAt sel book)
        then (aged, Just (Continue sel))
        else (now (Printing 0), Nothing)
    | fresh tiCancel && isJust (orderAt sel book) -> (now (Asking 0 0), Nothing)
    | otherwise -> (aged, Nothing)
  Asking age held
    | fresh tiLobby -> (now Choosing, Nothing)
    | held' >= holdTicks -> (now (Tearing 0), Nothing)
    | otherwise -> (now (Asking (age + 1) held'), Nothing)
    where
      -- Only a hold that starts while she asks counts, and letting go undoes it.
      held'
        | tiEnter input && (held > 0 || fresh tiEnter) = held + 1
        | otherwise = max 0 (held - 4)
  Tearing t
    | t + 1 >= tearTicks -> ((now Choosing) { dkNote = Just (Cancelled (sel + 1), noteTicks) }, Just (Torn sel))
    | otherwise -> (now (Tearing (t + 1)), Nothing)
  Printing t
    | t + 1 >= printTicks -> (now Choosing, Just (Begin sel))
    | otherwise -> (now (Printing (t + 1)), Nothing)
  where
    sel = dkSel d
    fresh key = key input && not (key prev)
    freshDir
      | tiDir input /= tiDir prev && not (tiDirRevealed input) = tiDir input
      | otherwise = Nothing
    aged = now (dkStep d)
    now step = d
      { dkStep = step
      , dkAge = dkAge d + 1
      , dkMoved = min liftTicks (dkMoved d + 1)
      , dkFocus = if forward step then min forwardTicks (dkFocus d + 1) else max 0 (dkFocus d - 1)
      , dkNote = case dkNote d of
          Just (n, left) | left > 1 -> Just (n, left - 1)
          _ -> Nothing
      }
    forward step = case step of
      Asking _ _ -> True
      Tearing _ -> True
      _ -> False
    choose k
      | k' == sel = aged
      | otherwise = aged { dkSel = k', dkFrom = sel, dkMoved = 0 }
      where k' = clamp 0 (slotCount - 1) k

-- Stepping -----------------------------------------------------------------------

-- | The saves, the desk, and the run, which plays the active order.
data Campaign = Campaign
  { cpBook :: !Book
  , cpDesk :: !Desk
  , cpRun  :: !Run
  } deriving (Eq, Show)

-- | One tick. Enter on the title opens the desk once there is an order to choose;
-- with none, it takes the first order and plays the opening, as before. Escape in
-- the hall goes back to the desk, not the title. Whatever the run does in an order
-- is written into it as it happens.
stepCampaign :: [Level] -> TickInput -> Campaign -> Campaign
stepCampaign levels input (Campaign book desk run) = case runMode run of
  Title
    | opens && anyOrders book ->
        Campaign book (openDesk book) held { runMode = Orders }
    | opens ->
        let book' = ensureActive book { bkActive = 0 }
            fresh0 = run { runIndex = 0, runUnlocked = 0, runCompleted = 0 }
        in synced (Campaign book' desk (stepRun levels input fresh0))
    | otherwise -> Campaign book desk (stepRun levels input run)
  Orders ->
    let (desk', out) = stepDesk input prev book desk
    in case out of
      Nothing -> Campaign book desk' held
      Just Back -> Campaign book desk' held { runMode = Title }
      Just (Continue k) -> case orderAt k book of
        Just o -> Campaign book { bkActive = k } desk' (enterOrder o)
        Nothing -> Campaign book desk' held
      Just (Begin k) ->
        Campaign (setSlot k (Just newOrder) book) { bkActive = k } desk' (enterOrder newOrder)
      Just (Torn k)
        -- The order in play is gone: what is left of the run starts over.
        | k == bkActive book ->
            let r0 = startRun levels 0
            in Campaign (setSlot k Nothing book) desk'
                 r0 { runMode = Orders, runGame = (runGame r0) { gAttempt = gAttempt g + 1, gPrev = input } }
        | otherwise -> Campaign (setSlot k Nothing book) desk' held
  mode ->
    let run' = stepRun levels input run
    in if mode == Lobby && runMode run' == Title
         then synced (Campaign book (openDesk book) run' { runMode = Orders })
         else synced (Campaign book desk run')
  where
    g = runGame run
    prev = gPrev g
    held = run { runGame = g { gPrev = input } }
    opens = titleChoiceTaken input run == Just titleStart
    -- Into an order: the hall, or the opening if nothing is cleared yet. The keys
    -- down now need a fresh press there.
    enterOrder o =
      let base = startRunWithProgress levels (odSelected o) (odCompleted o)
          attempt = gAttempt g + 1
      in if odCompleted o == 0
           then base { runIndex = 0, runMode = Intro 0 0
                     , runGame = (initialGame (levels !! 0)) { gAttempt = attempt, gPrev = input } }
           else base { runMode = Lobby, runGame = (runGame base) { gAttempt = attempt, gPrev = input } }
    synced cp = case runMode (cpRun cp) of
      Title -> cp
      Orders -> cp
      _ -> let r = cpRun cp
           in cp { cpBook = modifyActive (\o -> o { odCompleted = runCompleted r, odSelected = runIndex r }) (cpBook cp) }

-- | After the run has stepped from @r@ to @r'@, whether stepped or held by a talk:
-- a tick on the road for the order in play, and an era changed in a room.
tally :: Run -> Run -> Book -> Book
tally r r' book
  | onRoad (runMode r') = modifyActive (\o -> o { odTicks = odTicks o + 1, odShifts = odShifts o + shifted }) book
  | otherwise = book
  where
    onRoad m = case m of
      Lobby -> True
      InLevel -> True
      Ending _ -> True
      _ -> False
    g = runGame r
    g' = runGame r'
    -- As the shift's sound is told apart: not a restart, not an undo.
    shifted
      | runMode r == InLevel && runMode r' == InLevel && runIndex r == runIndex r'
        && gEra g' /= gEra g && gAttempt g' == gAttempt g
        && length (gHistory g') >= length (gHistory g) = 1
      | otherwise = 0

-- | The desk's sounds this tick: a move, her question popping up and typing, the
-- tear going on while Enter is held and tearing off, a new order printing.
deskCues :: [Level] -> Book -> Desk -> Desk -> [Cue]
deskCues levels book d d' = moved ++ case (dkStep d, dkStep d') of
  (Choosing, Asking _ _) -> [CueTalkOpen]
  (Asking _ _, Choosing) -> [CueTalkClose]
  (Asking _ _, Tearing _) -> [CueTorn]
  (Asking a h, Asking a' h') -> typing a a' ++ [ CueTearing (h' `div` 8) | h' > h, h' `div` 8 > h `div` 8 ]
  (Choosing, Printing _) -> [CuePrint]
  (Printing _, Printing t) | t `mod` 7 == 0 -> [CuePrint]
  _ -> []
  where
    moved = [ CueMenuMove | dkSel d' /= dkSel d ]
    text = maybe "" (cancelLine levels (dkSel d' + 1)) (orderAt (dkSel d') book)
    typing a a' =
      let from = min (length text) (askTyped a)
          to = min (length text) (askTyped a')
      in [ CueTalkBlip k | k <- [from + 1 .. to], k `mod` 3 == 1, not (isSpace (text !! (k - 1))) ]

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi
