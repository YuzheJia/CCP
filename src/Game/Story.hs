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
--
-- How it fits into one tick of the app's Main (@oneTick@): first 'talkTick' hands an
-- open talk this tick's keys and says whether the run may step; if it may not,
-- 'holdRun' stands in for 'stepRun'. Then 'notice' compares the run before and after
-- the tick and opens any talk the tick set off. Last, 'storyCues' compares the story
-- before and after for the talk's sounds. The app keeps the 'Story' beside the run.
--
-- What it does not do: draw, or change the rules. The app's TalkBox draws the box,
-- her portrait and the marker on the board from 'currentLine', 'typedChars',
-- 'lineTyped', the tick counts in 'Talk' and the timings here; while she talks about
-- something in the other era, Main shows the board in that era, as if Tab were
-- held ('lookNow'). A talk never moves anything on the board: it only holds the run
-- still, and 'holdRun' only records the keys. The demo (@--demo@) turns talks off.
--
-- Units: timing values are in ticks, 60 to a second. A line types at 48 letters a
-- second. Levels are list indices from 0, so level 1 is index 0. A 'Cell' is
-- (column, row) with y growing down the map.
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

-- 'isSpace', so a space typed out makes no blip.
import Data.Char (isSpace)
-- Maps and sets, imported qualified: their functions are called as Map.toList,
-- Set.insert and so on, so they cannot clash with the Prelude's list functions.
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Set as Set
-- Whether either keeper carries the power cell.
import Game.Logic (anyHasCell)
-- The run: its screens ('runMode') and which level it is on ('runIndex').
import Game.Run
-- The sounds a talk makes.
import Game.Sound (Cue (..))
import Game.Types

-- | The name on her tag. The app's TalkBox draws it on the tag above the box.
speakerName :: String
speakerName = "MEI"

-- | What a marker on the board names. The app's TalkBox writes each as one word on
-- the marker's tag, with the era it is shown in: 'TheWick' as WICK (the power cell),
-- 'TheLamp' as LAMP (the lighthouse, the level's goal), 'TheCrate' as CRATE, and
-- 'TheSled' as MIRROR (a movable mirror on its sled). 'Eq' is derived so the tests
-- can compare them, 'Show' to print them.
data Thing = TheWick | TheLamp | TheCrate | TheSled
-- ^ TheWick names the power cell; TheLamp the goal; TheCrate a box; TheSled a
-- movable mirror. These choose the labels on the dialogue's focus marker.
  deriving (Eq, Show)

-- | Where the board points while a line is said: a cell, and the era the board shows
-- it in, which may be the one the keeper is not standing in, as if Tab were held.
--
-- The @!@ before each field's type makes it strict: the value is worked out when the
-- record is built, not left as an unevaluated expression (a thunk) to be worked
-- out later. For a list or a 'Maybe' that only means its outermost constructor.
data Look = Look
  { lkCell  :: !Cell   -- ^ the cell the marker's brackets close on
  , lkEra   :: !Era    -- ^ the era the board shows while the line is said
  , lkThing :: !Thing  -- ^ what the tag over it names
  } deriving (Eq, Show)

-- | One line of her talk: what she says, and what the board points at meanwhile.
data Line = Line
  { lnText :: !String        -- ^ the words; any word of two or more letters all in
                             --   capitals (SPACE, PAST) is drawn in yellow
  , lnLook :: !(Maybe Look)  -- ^ what the board points at, or 'Nothing' to leave
                             --   the board alone
  } deriving (Eq, Show)

-- | Everything she says, by occasion.
--
-- The derived 'Ord' lets beats be kept in a 'Set.Set' ('stTold'); 'Enum' and
-- 'Bounded' allow @[minBound .. maxBound]@, every beat in order.
data Beat
  = ArriveFirst   -- ^ level 1: where the wick is, and where it goes
  | RustedCrate   -- ^ level 1: walked into the rusted crate in the present
  | WickFirst     -- ^ level 1: the wick taken up
  | ArriveSecond  -- ^ level 2: the wick out of reach, and a mirror on a sled
  | WickSecond    -- ^ level 2: the wick taken up
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A talk in progress: one beat's lines, said one after another.
data Talk = Talk
  { tkBeat  :: !Beat    -- ^ the occasion this talk is for
  , tkLines :: ![Line]  -- ^ every line of it, from 'beatLines', never empty
  , tkLine  :: !Int  -- ^ the line being said, from 0
  , tkAge   :: !Int  -- ^ ticks into this line
                     --   (the letters typed so far come from it; TalkBox also
                     --   opens the marker and moves her mouth by it)
  , tkShown :: !Int  -- ^ ticks since the box appeared, for its pop
  , tkWait  :: !Int  -- ^ ticks left before the box appears
  } deriving (Eq, Show)

-- | The whole state of the story, kept by the app's Main beside the run.
data Story = Story
  { stTalk    :: !(Maybe Talk)         -- ^ the talk open now, if any; one still
                                       --   waiting to appear counts as open
  , stTold    :: !(Set.Set Beat)       -- ^ told during this visit to the level
  , stClosing :: !(Maybe (Talk, Int))  -- ^ the talk just closed, and ticks since
                                       --   (for the box's going, 'closeTicks')
  } deriving (Eq, Show)

-- | No talk open, nothing told, nothing closing: the story as the game starts.
noStory :: Story
noStory = Story Nothing Set.empty Nothing

-- | An arrival talk waits for the level's fade-in first.
--
-- 34 ticks, a little over half a second: the board fades in from black over its
-- first 26 ticks (in the app's Render), and the box appears a moment after. The run
-- is held through the wait too, so the keeper cannot set off under the fade.
arrivalWait :: Int
arrivalWait = 34

-- | How long the box takes to pop up, and to go.
--
-- 9 ticks (0.15 seconds) up and 8 ticks away; TalkBox eases the box's size and
-- fade over them.
popTicks, closeTicks :: Int
popTicks = 9
closeTicks = 8

-- | Letters typed by this many ticks into a line: about 48 a second.
--
-- Four letters every five ticks: four fifths of a letter a tick, at 60 ticks a
-- second. 'div' divides whole numbers, rounding down. @age * 4 `div` 5@ reads as
-- @(age * 4) `div` 5@: '*' and 'div' have the same precedence and group to the
-- left. Backticks let a two-argument function be written between its arguments.
typedAt :: Int -> Int
typedAt age = age * 4 `div` 5

-- | Ticks until a line of @n@ letters is typed out.
--
-- The smallest age at which 'typedAt' reaches @n@: five quarters of @n@, rounded up
-- (adding 3 before dividing by 4 rounds up). Used when a press finishes a line at
-- once.
ticksFor :: Int -> Int
ticksFor n = (n * 5 + 3) `div` 4

-- | Whether the box is up: its wait before appearing is over. A talk still waiting
-- already holds the run, but shows nothing and points at nothing yet.
talkShown :: Talk -> Bool
talkShown t = tkWait t <= 0

-- | The line being said. Past the last line, which 'talkTick' never lets happen, it
-- is an empty line pointing at nothing rather than an error: 'drop' and a @case@
-- make a safe version of @!!@, which would crash.
currentLine :: Talk -> Line
currentLine t = case drop (tkLine t) (tkLines t) of
  l : _ -> l
  [] -> Line "" Nothing

-- | How many letters of the current line are shown so far, never more than it has.
-- TalkBox draws that many.
typedChars :: Talk -> Int
typedChars t = min (length (lnText (currentLine t))) (typedAt (tkAge t))

-- | Whether the whole current line is shown, so the next press moves on rather than
-- finishing it. TalkBox shows its mark for the next line then.
lineTyped :: Talk -> Bool
lineTyped t = typedChars t >= length (lnText (currentLine t))

-- | Where the board points now, once the box is up.
--
-- 'Nothing' with no talk, while the box is still waiting, or for a line that points
-- at nothing. The app's Main reads it to show the board in the look's era when that
-- is not the keeper's. The first branch is a case alternative with a guard: it is
-- taken only for a talk whose box is up; anything else falls to @_@.
lookNow :: Story -> Maybe Look
lookNow s = case stTalk s of
  Just t | talkShown t -> lnLook (currentLine t)
  _ -> Nothing

-- The lines -------------------------------------------------------------------

-- | What she says on each occasion, pointing at the level's own wick, lamp, crate
-- and mirror, wherever they are.
--
-- Arguments: the level (for the wick's cradle, 'lvPowerCell', and the lamp,
-- 'lvGoal'), the board as she starts (for where the crates and mirrors stand now)
-- and the beat. No cell is written in by hand, so a thing moved in a map moves its
-- marker with it. A look with nothing to point at (a level with no crate, say) is
-- 'Nothing', and its line is said with the board left alone. Each list here has at
-- least one line, which 'notice' relies on to open a talk.
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
    -- A line that points at nothing.
    say s = Line s Nothing
    -- A line that points at l, which may itself be Nothing.
    look l s = Line s l
    -- The wick's cradle, shown in the past, where the wick is; the lamp, shown in
    -- the present, where it is to be lit.
    wick = Just (Look (lvPowerCell level) Past TheWick)
    lamp = Just (Look (lvGoal level) Present TheLamp)
    -- The first crate on the board (level 1 has only one), shown in the present,
    -- where it is rusted. 'Set.toList' lists the cells in order and 'listToMaybe'
    -- takes the first, or Nothing from an empty list; 'fmap' applies the lambda to
    -- what is inside a Just and leaves Nothing alone.
    crate = fmap (\c -> Look c Present TheCrate) (listToMaybe (Set.toList (gCrates g)))
    -- The sled to point at: one in the past if there is one, where the wick is.
    -- 'Map.toList' gives the board's mirrors as (cell, mirror) pairs, and only the
    -- movable ones are kept, each with its era. Level 2 has one in each era. In the
    -- comprehension s@(_, Past) <- sleds, the pattern does the choosing: a sled in
    -- the present does not match it and is skipped. With every sled appended after
    -- those, 'listToMaybe' falls back on a present one when there is none in the
    -- past.
    sleds = [ (c, mEra m) | (c, m) <- Map.toList (gMirrors g), mMovable m ]
    sled = fmap (\(c, era) -> Look c era TheSled)
      (listToMaybe ([ s | s@(_, Past) <- sleds ] ++ sleds))

-- | The talks of the campaign's first two levels, by place in the hall.
--
-- By list index: 0 is level 1 and 1 is level 2. Any other level has no arrival or
-- wick talk ('Nothing'). One type signature can declare several names at once.
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
--
-- Arguments: this tick's keys, the run (for the keys of the tick before, in its
-- board's 'gPrev', which 'holdRun' keeps up to date while the run is held) and the
-- story. With no talk open the run steps as usual. With one open, the guards are
-- tried in order: Escape closes it and lets the run step, so the run sees Escape
-- and leaves the level; a box still waiting counts down; with no fresh Enter or
-- Space the line types on; with one, an unfinished line is finished at once, a
-- finished one gives way to the next, and the last one closes the talk.
talkTick :: TickInput -> Run -> Story -> (Story, Bool)
talkTick input run s = case stTalk s of
  Nothing -> (aged, True)
  Just t
    -- Escape still leaves the level, and takes the talk with it.
    | fresh tiLobby -> (aged { stTalk = Nothing }, True)
    -- The box has not appeared yet: wait on, holding the run.
    | tkWait t > 0 -> (aged { stTalk = Just t { tkWait = tkWait t - 1 } }, False)
    -- No press: the line types on by a tick.
    | not (fresh tiEnter || fresh tiShift) -> (aged { stTalk = Just (older t) }, False)
    -- A press while the line is still typing finishes it at once.
    -- Record update binds tighter than applying a function, so
    -- Just (older t) { tkAge = ... } means Just ((older t) { tkAge = ... }).
    | not (lineTyped t) ->
        (aged { stTalk = Just (older t) { tkAge = ticksFor (length (lnText (currentLine t))) } }, False)
    -- A press on a finished line moves on to the next, typing from its start.
    | tkLine t + 1 < length (tkLines t) ->
        (aged { stTalk = Just (older t) { tkLine = tkLine t + 1, tkAge = 0 } }, False)
    -- The last line: the box goes. The run is held once more, so the key that
    -- closed it is already down when play resumes, and does nothing there.
    | otherwise -> (aged { stTalk = Nothing, stClosing = Just (t, 0) }, False)
  where
    -- Last tick's keys, as the held run remembered them.
    prev = gPrev (runGame run)
    -- A fresh press of one key. Its argument is a field accessor, a function from
    -- 'TickInput' to Bool such as 'tiEnter', so one definition serves every key.
    fresh key = key input && not (key prev)
    -- The talk a tick older: its line typing on, and its pop moving on.
    older t = t { tkAge = tkAge t + 1, tkShown = tkShown t + 1 }
    -- The story with any closing box a tick further on, dropped once its
    -- 'closeTicks' are done. Every branch above starts from this, so a box that has
    -- just gone finishes going whatever else happens.
    aged = s { stClosing = case stClosing s of
                 Just (t, n) | n + 1 < closeTicks -> Just (t, n + 1)
                 _ -> Nothing }

-- | The run while a talk holds it: nothing moves, but the keys down now are
-- remembered, so they need a fresh press once it closes.
--
-- Only two fields of the board change: 'gPrev' takes this tick's keys, so a key
-- still down when play resumes is not fresh, and 'gArmed' goes off, so a direction
-- held through the talk does not start walking either until it is pressed again.
-- The app's Main calls it in place of 'stepRun' while 'talkTick' says the run must
-- wait.
holdRun :: TickInput -> Run -> Run
holdRun input run = run { runGame = (runGame run) { gPrev = input, gArmed = False } }

-- | After the run has stepped from @r@ to @r'@: the talk, if any, that this tick
-- set off. Entering a level starts its visit afresh; leaving it ends any talk.
--
-- Arguments: the hall's levels, the run before this tick and after it, and the story
-- after 'talkTick'. The guards are tried in order. Off a level (the title, the
-- hall, an ending), any talk is dropped. Just come into a level, from another
-- screen or another level: the told set is emptied and the level's arrival talk,
-- if it has one, opens after 'arrivalWait'. With a talk already open, or the board
-- not in play (won, lost or paused), nothing new opens, so talks never stack. The
-- power cell just picked up, by either keeper: the level's wick talk. In level 1,
-- walking into the rusted crate in the present: 'RustedCrate'. A restart stays on
-- the same level, so it keeps the told set: each talk is told at most once a visit.
notice :: [Level] -> Run -> Run -> Story -> Story
notice levels r r' s
  | runMode r' /= InLevel = s { stTalk = Nothing }
  -- Record update binds tighter than applying a function: the last argument here
  -- is s with two fields changed.
  | entered = open arrivalWait (arrivalBeat i) s { stTold = Set.empty, stTalk = Nothing }
  | isJust (stTalk s) || gPhase g' /= Playing = s
  | anyHasCell g' && not (anyHasCell g) = open 0 (wickBeat i) s
  | i == 0 && rusted = open 0 (Just RustedCrate) s
  | otherwise = s
  where
    -- The level after the tick, the boards before and after, and its level.
    i = runIndex r'
    g = runGame r
    g' = runGame r'
    level = levels !! i
    -- Coming into a level: the run was on another screen, or on another level.
    entered = runMode r /= InLevel || runIndex r /= i
    -- The talks teach, so they are for a level not yet cleared.
    -- Level index i is cleared once more than i levels are.
    teaching = i >= runCompleted r'
    -- The board has just said the crate is rusted: the message is new this tick,
    -- or set again (more ticks left than before, since a message only counts
    -- down). 'maybe' gives True when there was no message before, and otherwise
    -- applies the lambda to it.
    rusted = case gFeedback g' of
      Just (Feedback CrateRusted left) ->
        maybe True (\o -> fbKind o /= CrateRusted || fbLeft o < left) (gFeedback g)
      _ -> False
    -- Open beat mb after wait ticks, if there is a beat, the level is still
    -- teaching, the beat has not been told this visit, and it has lines. The last
    -- guard is a pattern guard: lns@(_ : _) <- beatLines ... matches only a
    -- non-empty list, and names it lns. The beat counts as told from the moment it
    -- opens, so neither Escape nor a restart brings it back this visit.
    open wait mb st = case mb of
      Just b | teaching, Set.notMember b (stTold st), lns@(_ : _) <- beatLines level g' b ->
        st { stTalk = Just (Talk b lns 0 0 0 wait), stTold = Set.insert b (stTold st) }
      _ -> st

-- | The sounds the talk makes this tick: the box popping up and going, the next
-- line, and a blip for every third letter typed, as it types.
--
-- Compares the story before this tick (@s@) with the story after (@s'@), as
-- 'Game.Sound' compares runs; the app's Main adds these to the tick's other sounds.
-- The @case@ looks at the talk before and after together, as a pair.
storyCues :: Story -> Story -> [Cue]
storyCues s s' = case (stTalk s, stTalk s') of
  (old, Just t')
    -- The box has just appeared: up now, and before there was no talk, or it was
    -- still waiting.
    | talkShown t' && not (maybe False talkShown old) -> [CueTalkOpen]
    -- The same talk on another line. Just t <- old is a pattern guard: it holds only
    -- when there was a talk before, and names it t.
    | Just t <- old, tkBeat t == tkBeat t', tkLine t' /= tkLine t -> [CueTalkNext]
    -- The same talk and line, already up: a blip for each letter typed this tick
    -- whose place in the line (counting from 1) is 1, 4, 7 and so on, unless it is
    -- a space; letter k is text !! (k - 1). Only when at most two letters appeared:
    -- a press that finishes the line at once types many, and would make a burst.
    -- Each blip carries its letter's place, which Audio turns into its pitch.
    | Just t <- old, tkBeat t == tkBeat t', talkShown t ->
        let from = typedChars t
            to = typedChars t'
            text = lnText (currentLine t')
        in [ CueTalkBlip k | to - from <= 2, k <- [from + 1 .. to]
                           , k `mod` 3 == 1, not (isSpace (text !! (k - 1))) ]
    | otherwise -> []
  -- The talk has just gone and its box is going: the closing sound. Escape, or
  -- leaving the level, drops a talk without starting a closing box.
  (Just _, Nothing) | isJust (stClosing s') -> [CueTalkClose]
  _ -> []
