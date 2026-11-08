-- | The courier's side of the story: when she speaks, what she says, and what the
-- board points at while she does. Pure, like the rules, and like 'Game.Sound' it is
-- worked out by comparing each tick of the run with the one before.
--
-- She is a delivery rider with an odd order: pick up a lamp wick, which only the
-- lighthouse's past still has, and deliver it to the lamp at the top, in the present.
--
-- A talk is modal. While one is open the run does not step: the board waits, Enter
-- or Space moves the talk on, and Escape still leaves the level. A level that
-- teaches something has an arrival talk and a few that what happens in it sets off;
-- all of them are told only until the level is first cleared, and each at most once
-- a visit.
module Game.Story
  ( Story (..)
  , Talk (..)
  , Line (..)
  , Look (..)
  , Thing (..)
  , Beat (..)
  , noStory
  , speakerName
  , beatLines
  , talkTick
  , notice
  , holdRun
  , talkShown
  , currentLine
  , typedChars
  , lineTyped
  , lookNow
  , storyCues
  , arrivalWait
  , popTicks
  , closeTicks
  ) where

import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Set as Set
import Game.Logic (anyHasCell)
import Game.Run
import Game.Sound (Cue (..))
import Game.Types

-- | The name on her tag.
speakerName :: String
speakerName = "MEI"

-- | What a marker on the board names.
data Thing = TheWick | TheLamp | TheCrate | TheSled
  deriving (Eq, Show)

-- | Where the board points while a line is said: a cell, and the era the board shows
-- it in, which may be the one the keeper is not standing in, as if Tab were held.
data Look = Look
  { lkCell  :: !Cell
  , lkEra   :: !Era
  , lkThing :: !Thing
  } deriving (Eq, Show)

data Line = Line
  { lnText :: !String
  , lnLook :: !(Maybe Look)
  } deriving (Eq, Show)

-- | Everything she says, by occasion.
data Beat
  = ArriveFirst   -- ^ level 1: where the wick is, and where it goes
  | RustedCrate   -- ^ level 1: walked into the rusted crate in the present
  | WickFirst     -- ^ level 1: the wick taken up
  | ArriveSecond  -- ^ level 2: the wick out of reach, and a mirror on a sled
  | WickSecond    -- ^ level 2: the wick taken up
  deriving (Eq, Ord, Show, Enum, Bounded)

data Talk = Talk
  { tkBeat  :: !Beat
  , tkLines :: ![Line]
  , tkLine  :: !Int  -- ^ the line being said, from 0
  , tkAge   :: !Int  -- ^ ticks into this line
  , tkShown :: !Int  -- ^ ticks since the box appeared, for its pop
  , tkWait  :: !Int  -- ^ ticks left before the box appears
  } deriving (Eq, Show)

data Story = Story
  { stTalk    :: !(Maybe Talk)
  , stTold    :: !(Set.Set Beat)       -- ^ told during this visit to the level
  , stClosing :: !(Maybe (Talk, Int))  -- ^ the talk just closed, and ticks since
  } deriving (Eq, Show)

noStory :: Story
noStory = Story Nothing Set.empty Nothing

-- | An arrival talk waits for the level's fade-in first.
arrivalWait :: Int
arrivalWait = 34

-- | How long the box takes to pop up, and to go.
popTicks, closeTicks :: Int
popTicks = 9
closeTicks = 8

-- | Letters typed by this many ticks into a line: about 48 a second.
typedAt :: Int -> Int
typedAt age = age * 4 `div` 5

-- | Ticks until a line of @n@ letters is typed out.
ticksFor :: Int -> Int
ticksFor n = (n * 5 + 3) `div` 4

talkShown :: Talk -> Bool
talkShown t = tkWait t <= 0

currentLine :: Talk -> Line
currentLine t = case drop (tkLine t) (tkLines t) of
  l : _ -> l
  [] -> Line "" Nothing

typedChars :: Talk -> Int
typedChars t = min (length (lnText (currentLine t))) (typedAt (tkAge t))

lineTyped :: Talk -> Bool
lineTyped t = typedChars t >= length (lnText (currentLine t))

-- | Where the board points now, once the box is up.
lookNow :: Story -> Maybe Look
lookNow s = case stTalk s of
  Just t | talkShown t -> lnLook (currentLine t)
  _ -> Nothing

-- The lines -------------------------------------------------------------------

-- | What she says on each occasion, pointing at the level's own wick, lamp, crate
-- and mirror, wherever they are.
beatLines :: Level -> Game -> Beat -> [Line]
beatLines level g beat = case beat of
  ArriveFirst ->
    [ say "Delivery note: pick up one lamp wick, bring it to the top floor. Okay. Easy enough."
    , look wick "Pickup's that cradle over there. Only... the wick's in it in the PAST. Right now it's an empty socket."
    , look lamp "Drop-off is the lamp. Wick in, light on, order done."
    , say "SPACE flips me between then and now. Every move eats the clock, so no detours."
    ]
  RustedCrate ->
    [ look crate "Rusted to the floor. Back when someone kept this place, it'd still slide. SPACE, then push." ]
  WickFirst ->
    [ look lamp "Got it, still warm. Now up to the lamp. It's the present that needs the light." ]
  ArriveSecond ->
    [ look wick "Next wick's shut in that alcove. No door, no way in on foot."
    , look sled "But this mirror sits on a sled. Shove it from the back or the sides and it slides."
    , look sled "Its light is a way through, too: stand in the beam, walk at the glass, come out behind it."
    , look lamp "Lamp's over there. Same job as before: wick in, light on."
    ]
  WickSecond ->
    [ look lamp "Got it. Now a way back out of here, and over to the lamp." ]
  where
    say s = Line s Nothing
    look l s = Line s l
    wick = Just (Look (lvPowerCell level) Past TheWick)
    lamp = Just (Look (lvGoal level) Present TheLamp)
    crate = fmap (\c -> Look c Present TheCrate) (listToMaybe (Set.toList (gCrates g)))
    -- The sled to point at: one in the past if there is one, where the wick is.
    sleds = [ (c, mEra m) | (c, m) <- Map.toList (gMirrors g), mMovable m ]
    sled = fmap (\(c, era) -> Look c era TheSled)
      (listToMaybe ([ s | s@(_, Past) <- sleds ] ++ sleds))

-- | The talks of the campaign's first two levels, by place in the hall.
arrivalBeat, wickBeat :: Int -> Maybe Beat
arrivalBeat i = case i of
  0 -> Just ArriveFirst
  1 -> Just ArriveSecond
  _ -> Nothing
wickBeat i = case i of
  0 -> Just WickFirst
  1 -> Just WickSecond
  _ -> Nothing

-- Ticking ---------------------------------------------------------------------

-- | Before the run steps: an open talk takes this tick's keys. True if the run may
-- step this tick.
talkTick :: TickInput -> Run -> Story -> (Story, Bool)
talkTick input run s = case stTalk s of
  Nothing -> (aged, True)
  Just t
    -- Escape still leaves the level, and takes the talk with it.
    | fresh tiLobby -> (aged { stTalk = Nothing }, True)
    | tkWait t > 0 -> (aged { stTalk = Just t { tkWait = tkWait t - 1 } }, False)
    | not (fresh tiEnter || fresh tiShift) -> (aged { stTalk = Just (older t) }, False)
    -- A press while the line is still typing finishes it at once.
    | not (lineTyped t) ->
        (aged { stTalk = Just (older t) { tkAge = ticksFor (length (lnText (currentLine t))) } }, False)
    | tkLine t + 1 < length (tkLines t) ->
        (aged { stTalk = Just (older t) { tkLine = tkLine t + 1, tkAge = 0 } }, False)
    -- The last line: the box goes. The run is held once more, so the key that
    -- closed it is already down when play resumes, and does nothing there.
    | otherwise -> (aged { stTalk = Nothing, stClosing = Just (t, 0) }, False)
  where
    prev = gPrev (runGame run)
    fresh key = key input && not (key prev)
    older t = t { tkAge = tkAge t + 1, tkShown = tkShown t + 1 }
    aged = s { stClosing = case stClosing s of
                 Just (t, n) | n + 1 < closeTicks -> Just (t, n + 1)
                 _ -> Nothing }

-- | The run while a talk holds it: nothing moves, but the keys down now are
-- remembered, so they need a fresh press once it closes.
holdRun :: TickInput -> Run -> Run
holdRun input run = run { runGame = (runGame run) { gPrev = input, gArmed = False } }

-- | After the run has stepped from @r@ to @r'@: the talk, if any, that this tick
-- set off. Entering a level starts its visit afresh; leaving it ends any talk.
notice :: [Level] -> Run -> Run -> Story -> Story
notice levels r r' s
  | runMode r' /= InLevel = s { stTalk = Nothing }
  | entered = open arrivalWait (arrivalBeat i) s { stTold = Set.empty, stTalk = Nothing }
  | isJust (stTalk s) || gPhase g' /= Playing = s
  | anyHasCell g' && not (anyHasCell g) = open 0 (wickBeat i) s
  | i == 0 && rusted = open 0 (Just RustedCrate) s
  | otherwise = s
  where
    i = runIndex r'
    g = runGame r
    g' = runGame r'
    level = levels !! i
    entered = runMode r /= InLevel || runIndex r /= i
    -- The talks teach, so they are for a level not yet cleared.
    teaching = i >= runCompleted r'
    rusted = case gFeedback g' of
      Just (Feedback CrateRusted left) ->
        maybe True (\o -> fbKind o /= CrateRusted || fbLeft o < left) (gFeedback g)
      _ -> False
    open wait mb st = case mb of
      Just b | teaching, Set.notMember b (stTold st), lns@(_ : _) <- beatLines level g' b ->
        st { stTalk = Just (Talk b lns 0 0 0 wait), stTold = Set.insert b (stTold st) }
      _ -> st

-- | The sounds the talk makes this tick: the box popping up and going, the next
-- line, and a blip for every third letter typed, as it types.
storyCues :: Story -> Story -> [Cue]
storyCues s s' = case (stTalk s, stTalk s') of
  (old, Just t')
    | talkShown t' && not (maybe False talkShown old) -> [CueTalkOpen]
    | Just t <- old, tkBeat t == tkBeat t', tkLine t' /= tkLine t -> [CueTalkNext]
    | Just t <- old, tkBeat t == tkBeat t', talkShown t ->
        let from = typedChars t
            to = typedChars t'
            text = lnText (currentLine t')
        in [ CueTalkBlip k | to - from <= 2, k <- [from + 1 .. to]
                           , k `mod` 3 == 1, not (isSpace (text !! (k - 1))) ]
    | otherwise -> []
  (Just _, Nothing) | isJust (stClosing s') -> [CueTalkClose]
  _ -> []
