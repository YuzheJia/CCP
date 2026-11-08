# Borrowed Light

Retrieve a power cell from the past and bring it back to illuminate a lighthouse in the present.

A short campaign of a top-down time-shifting puzzle with a time budget, crates and mirrors, in Haskell with Gloss, for the University of Bristol CSS GameJam 2026.

| Past | Present |
|---|---|
| ![Past](docs/3-past.png) | ![Present](docs/2-present.png) |

## Run it

Needs GHC 9.6.7 and cabal-install 3.12.1.0 (`cabal.project` pins the compiler). On this machine they are in `~/.ghcup/bin`.

```bash
export PATH="$HOME/.ghcup/bin:$PATH"
```

```bash
cabal test spec --test-show-details=direct
```

```bash
cabal run borrowed-light
```

`cabal run borrowed-light -- --demo` has the solver play every level, then hands you the keyboard. Add `--level=N` to select level N in the hall (and unlock it for development); `--level=4` opens the secret level directly. Add `--pace=N` to wait N ticks (60 a second) after each of the demo's moves.

The game opens on its title screen. The first time, Enter takes a new order (a new
save), plays the **opening film** and goes from it straight into level 1 (see "The
courier" below). Once there is a save, Enter opens **Your orders**, the saves, with
the one last played already chosen, so Enter twice carries on (see "Her orders"
below). An order with a room cleared opens the **Level Hall**: use W/S or the arrow
keys to choose an unlocked lesson and Enter to play it. Escape returns to the hall
from any point in a level, and from the hall to the orders. Clearing a room unlocks
the next one, and the order is saved at once; an unfinished puzzle starts fresh when
it is entered again.

### Controls

| Key | Action |
|---|---|
| WASD / arrows | Move |
| Space | Shift era |
| Z | Undo the last action (gives back half its time) |
| Tab | Hold to look at the other era, for free |
| P | Pause / resume |
| R | Restart |
| Enter | Start / retry; on with the courier's talk (Space too) |
| Enter, held | Skip the opening film |
| Escape | Return to the Level Hall, and from it to the orders; skip the opening film |
| Delete / Backspace | On the orders: cancel the chosen order (then hold Enter to tear it up) |
| M | Sound off / on |
| Window close button | Exit |

### Her orders

Each save is one of Mei's orders, kept on a delivery ticket, and three tickets are
taped up over the title's lighthouse. Their design was worked out first as mockups,
in [docs/ui-mockups/save-screen/](docs/ui-mockups/save-screen/), which can also be
opened as a page and played with the keyboard.

A ticket says, from the top: whose order and whether it is en route or done, the
order number (the largest thing on it, since a player looks for theirs first), the
route from the wick through the three rooms to the lamp, with cleared rooms black and
the next stop yellow, the next stop by name, then the time on the road, how many
times she changed era, and when it was last saved. A delivered order is stamped, and
says so if the hidden level has been cleared too, without naming it. An empty slot
offers a new order, and says the opening film comes first.

A/D or the arrows choose a ticket; the chosen one lifts, with her yellow shadow and
the talk's arrow over it. Enter continues it, to the hall, or to the opening if
nothing is cleared yet; on an empty slot a new ticket prints, row by row, and the
opening plays. Escape goes back to the title.

**Cancelling** is the one thing here that cannot be undone, so it takes two steps.
Delete (or Backspace) brings the ticket forward and Mei, in her talk box, says what
would be lost: how many rooms and how many minutes on the road. Enter must then be
held, as it is to skip the film: the ticket tears along its perforation as it is
held, and letting go takes the tear back. Held for a second, the lower part drops
away, and a note says the order is cancelled. Escape keeps it.

**Saving.** The orders are written to `orders-v2.save` in the operating system's
user-data directory: at once when a room is cleared, an order is taken or cancelled,
or another order is chosen; the time on the road every ten seconds of play, and on
returning to the title or the orders. Each write stamps the order in play with the
time. On arriving in the hall with a room newly cleared, a slip prints at its top
right, "ORDER 1 SAVED", and is taken back after three seconds; if the file could not
be written it is red and says so, and the game keeps trying. The save from before
the orders, `progress-v1.save`, is read as the first order if there is no orders
file yet, and is left as it was.

The desk is pure and tested, in [src/Game/Orders.hs](src/Game/Orders.hs); it is drawn
by [app/OrdersScreen.hs](app/OrdersScreen.hs).

### The courier

The keeper is a delivery rider, Mei, with an odd order: pick up a lamp wick and
deliver it to the top floor of Cape Lighthouse. The wick is the power cell, which only
the lighthouse's past still has; the top floor is the lamp, in the present.

**The opening film** ([assets/intro.blv](assets/intro.blv), ten seconds, five shots:
the order coming in, the ride out along the causeway, the delivery note at the door)
plays on a new save between the title and level 1, with its own soundtrack. Holding
Enter (or Space) for two thirds of a second skips it, and so does Escape; a tap does
not, and neither does the Enter still held from the title. It plays once a session.
If the file is missing, the title goes straight into level 1.

**Her talk** is a box in the style of a comic-book speech panel: her portrait rising
from the bottom left, a tilted black box with a white rim and a yellow shadow, a name
tag, and the line typing out in a pixel font, words in capitals in yellow, a blip
every few letters. Box, letters and portrait share one pixel grid, three canvas units
to a pixel. Enter or Space finishes a line, then moves on. While she talks the board
waits: keys move nothing and cost nothing, the key that closes the talk does nothing
in play, and Escape still leaves the level. When she talks about something, the board
dims around it, brackets close on its cell and a tag over it names it and its era;
if it is in the other era, the board shows that era, as Tab would.

What she says, and when, is [src/Game/Story.hs](src/Game/Story.hs), pure and tested,
and only in a level not yet cleared:

| Level | When | What she points at |
|---|---|---|
| 1 | On arrival | The wick, in its cradle in the past; the lamp, in the present; then Space and the clock |
| 1 | Walking into the rusted crate | The crate: it would slide in the past |
| 1 | Taking the wick | The lamp |
| 2 | On arrival | The wick, shut in its alcove; the mirror on a sled (it slides when shoved from the back or sides, and its light carries her through); the lamp |
| 2 | Taking the wick | The lamp |

**The push hint.** Players did not find out that a movable mirror can be pushed, so
the board shows it, the way a jump shows its landing: standing beside a movable
mirror's back or side, its outline waits on the cell it would slide to, with warm
chevrons running that way. It is worked out by the push rule itself (`tryStep`), so
it never offers a push that would not happen, and it appears in every level.

**Doorways.** In a level shown a room at a time, a way on into another room could
pass for a dead end, so each doorway glows with the light of the room beyond, warm in
the past and cold in the present, with chevrons drifting out through it. It is
strongest towards a room not yet seen, faint back towards one already seen, and
fainter still where the way is open only in the other era. It is worked out from the
board (`openCell` on both sides), so a crate standing in a doorway, like the shortcut
home in level 3, puts its light out until it is moved.

Regenerating the assets needs Blender, for its FFmpeg and numpy:

```bash
blender -b --factory-startup --python tools/encode-intro.py -- FILM.mp4 assets/intro.blv
```

```bash
blender -b --factory-startup --python tools/make-portrait.py -- assets/source/courier.webp app/Portrait.hs
```

The film format is described at the top of [app/Video.hs](app/Video.hs): a
256-colour palette per shot, and each frame coded against the one before. The film
in the game comes from a 1920 by 1080 source, shrunk to 1280 by 720, one pixel to a
canvas unit, so it fills the screen across. An optional last argument to the script
sets another width (640 makes a file a quarter the size, but soft on a large
screen), and the game scales whatever width it finds to fill the screen.

### Time

Time is a budget, not a clock. A step costs 2 seconds and a shift costs 6, charged when the action is accepted. Standing still, pausing, refused actions and restarting cost nothing. Z restores the board exactly but refunds only half the action's cost, and R refills the budget at the price of walking it all again. With less than one step's price left, the level is lost, though Z still works from the lost screen. The prices live in a plain `Rules` record on each `Level`.

Each option's budget is its verified route target plus enough slack for a first-time player to experiment.

### The keeper's watch

Time is read off a pocket watch beside the map (`app/Hud.hs`). Nothing on screen ever
covers a cell.

**Two stands.** Entering a level, from the hall or the level before, introduces it:
the map stands smaller, to the right (0.86), and a column on the left carries the
watch at full size, the level's place, name and era, and every key with its price.
The first step, push, jump or shift that takes closes in on the board over about
three quarters of a second: the column fades, the map grows to 0.96 and slides to
the centre, and the watch shrinks into the left margin. A refused key does not end
the introduction, and a restart keeps the closer view (`fxFocus` in `Game.Fx`).

**The sea around it** says which era it is (`seaLife`, `shore` in `app/Station.hs`).
In the past the shelf around the station is light, the surf calm, the sun glints to
the east and the real lighthouse's beam sweeps by behind the station every ten
seconds. In the present the surf is rougher and grey, rain falls across the whole
screen, and there is no beam. The shelf, rocks and surf belong to the board and move
with it; the beam, glints, rain and darkened edges belong to the screen.

- **The ring** is the time left out of the level's budget, running back from twelve
  like a hand. In the past it is brass and glows like the lamp; in the present it is
  rusted steel and its light is cold.
- **The number** is the seconds left. The one line under it says the most pressing
  thing about time: a shift refused for want of time (`shift 6`), a mirror jump on
  offer and its key (`jump -4`), what the last action spent or an undo gave back, or,
  when under a fifth of the budget is left, how many steps remain.
- **Spending** leaves an orange wake on the ring that fades; a jump on offer marks its
  price as hatching; an undo's refund refills in green; a refused shift outlines the
  part that is missing.
- **Low time** turns the ring to embers with a breathing halo, and lights Z and R in
  the line of keys.
- **The plate** under the case names the era; **the socket** at four o'clock shows the
  power cell's outline, lit once it is carried.

In play, the first two levels keep their keys and prices in one line under the map.
A level for two lists each keeper's keys under the watch instead, and a level of
several rooms puts its room map there.

### Current levels

The Level Hall contains the opening crate lesson, a mirror chamber and a four-room
station shown a room at a time.

| # | Level | Idea | Par | Budget |
|---|---|---|---|---|
| 1 | Clear the Way | The crate is rusted in the present: switch to the past to push it into its recess, then switch eras to cross two blocked passages | 48 | 68 |
| 2 | Two Alignments | Clear a launch cell, then reposition the same mirror for two separate jumps | 80 | 96 |
| 3 | The Long Way Round | Four rooms, two ways through each: chain jumps through a mirror set in the wall or clear its beam, Crowded Track's vault, lift two crates out of the gallery using the movable mirror as a door or standing on its cell in the other era, and open the shortcut home from its far side | 306 | 346 |

**Two cameras.** A level built from whole rooms of 13 by 9 (The Long Way Round is 2
by 2) is shown one room at a time: stepping through a doorway darkens the map, swaps
the room and lightens again, and a small map under the watch shows the rooms seen so far.
A level of any other size (such as The Long Reflection, kept in `extraLevels`) slides under the map
instead, keeping the keeper near the centre.

The Long Reflection (no longer in the campaign) reaches about four million states, which takes the solver minutes
and gigabytes, so its cheapest route is pinned in `knownRoute` in
`src/Game/Solution.hs`. The tests and `--demo` replay that route instead of searching.
Under these rules it costs 188 rather than the 224 the level was built for, so its
budget is that 224.

Earlier levels are kept in `extraLevels` in `src/Game/Level.hs`: The Long Reflection and Harrow Point, Below (the levels merged into level 3; Harrow's west room fills pits, which no level teaches), First Light, One Way In, the two echo levels, and the crate levels (First Push, The Plug, The Buried Lamp, The Held Door, The Bridge, The Wrong Way First). They are not played or checked, because a player standing on a plate can now change their pars.

### The ending, and the level the hall does not list

Lighting level 3's lamp is not the end. The usual plate appears and says every lamp
is lit; then the lamp flares white and goes out. Its light slips into the wall above
the tower and runs west along row 3 across the whole hall, breaking each wall it
meets, out through the outer wall. The keeper walks again (free: no time is spent,
and the era, undo and restart keys do nothing), and stepping onto that row starts
the rush: the keeper is carried west, faster and faster, down a corridor of lamps
that catch as they pass, to the real lighthouse. It blazes, the screen whites out,
and level 4 begins.

Level 4, "Two Keepers", is secret: the level hall never lists it, never selects it
and never counts it, however much has been cleared. It is reached from the end of
level 3, or directly with `--level=4` for development. It is for two players:

| Keys | Who |
|---|---|
| `W A S D` | the yellow keeper |
| arrow keys | the red keeper |
| `Space` (or right `Shift`, keypad `0`) | shifts the era for both |

Both keepers play by the same rules: either can push crates in the past, step
through mirrors, carry the power cell and light the lamp. They never share a cell.
The era is shared, so a shift waits until both stand still, and is refused (with the
usual outline) unless both have floor in the other era. Time is shared too, and undo
takes back the last action, whichever keeper took it. The game's own solver knows
one keeper only, so a level for two is never told it is a dead end.

The level is two islands with water between (column 6), crossed only by corner
mirrors, and only one way, since the far shore never sees their light. The power
cell is in a vault on the west island that only the east island's mirror (past)
reaches; the vault's door crate only pushes out, onto the one spot from which the
west mirror (present) carries a keeper to the east island, so yellow has to cross
before the vault is opened. Whoever carries the cell comes back east onto a jetty
shut by a gate crate, which can only be pushed from the east, into the east mirror's
light, where it shuts the vault for good. So the gate is pushed by the keeper who
stayed east, after the carrier has gone into the vault. Either keeper can carry.

The tests check this with `duoSearch` in `Game.Solution`, a small search for two
over the real `stepGame` (yellow's keys, red's keys, the shared Space). It finds the
par (118), and finds no win for either keeper alone, or while the other stands
still. It is only for the tests.

The ending lives in `Game.Run` (`Ending` mode, stages `Lit` `Flare` `Opening`
`Beckon` `Rush` `Arrival`), apart from the rules, so the solver, pars and level
tests are unchanged. A level gets an ending from `withPassage` in `src/Game/Level.hs`:
the first cell beside the lighthouse, then the walls on one straight line to the
outer wall. The second keeper is a `T` on the present map.

### Mirrors

A mirror stands in one era, solid there, and throws light out of its face in a straight line until a wall, a crate, a shut door or another mirror stops it. Light crosses an open pit. On the map it is drawn as the way its face looks, `^ v < >`, on the map of the era it works in.

- **Where it puts you:** as far behind the mirror as you stand in front of it. Two cells in front, two cells behind.
- **How to use it:** stand in its light and press the key that points at the mirror. If you could stand where your reflection is, you step through, in the same era. If not, the key just walks you a cell, as it always would.
- **Reading it before you press:** in the light, your reflection shows where you would land, with an arrow beside you on the key that does it. If the landing is blocked, a red cross shows instead and the key only walks. Other keys never jump.
- **Holding a key** walks up into the light and stops; jumping takes a fresh press, like a push.
- **Price:** 4 seconds, however far. Z takes it back for half, like any action.

The light, the reflection and the jump are all worked out from the board by one function, `mirrorAhead`, so what is drawn cannot disagree with what happens. Jumping is an ordinary move, so the solver needed no new action.

#### Movable mirrors

A movable mirror (`n s e w` on the map, the compass letter of the way its face looks) stands on a timber sled, with a notch of light on its face side. **From its face it is a door; from its back and sides it is a crate.** The same key in the same spot only ever means one thing, so pushing never gets in the way of jumping.

- Pressing into its back or a side pushes it one cell, for a step's price, keeping its facing; its light and reflection move with it. Like a crate, holding a key walks up to it and stops, and a push takes a fresh press.
- Pressing into its face jumps if the landing is open, as with any mirror. If it is not, nothing moves and the hint line says faces only take you through. So a mirror facing down can be pushed left, right and down, never up.
- It moves only in its own era, and in the other era its cell is plain floor. Crates rust in the present; mirrors don't.
- It only goes onto floor that is open in its era: never into a wall, a crate, a pit or a door, and never onto a cell that holds another mirror, even in the other era. A crate can't be pushed onto a cell where the other era has a mirror, because a crate stands in both.
- You can't switch era onto a mirror's cell. It doesn't hold plates down.

A mirror set in the wall (`^ v < >`) never moves, and says so if you press against it. Each mirror is fixed or movable on its own, so one level can mix both. The mirrors live in the game state (`gMirrors`), so undo, restart, the solver and the dead-end warning all cover them.

#### Corner mirrors

A corner mirror (`L J 7 F` on the map, the corner its two faces make: `L` looks north and east, `J` north and west, `7` south and west, `F` south and east) is set in the wall with its glass running corner to corner, and throws light out of both faces. It turns whoever steps through it: stand in one face's light and press toward it, and you come out of the other face, **as far out as you stood in**. Two cells up its north light puts you two cells east of it. Everything else is as for any mirror: the reflection shows the landing, a taken landing shows a red cross and the key only walks, and it costs a jump's price. Like a flat mirror set in the wall, it carries you across walls; but light does not cross them, so from the far side it cannot be seen, and the crossing is one way. Corner mirrors never move. In code a corner mirror is a `Mirror` with a second face, `mBend`, and `mirrorAhead` turns the jump out of it with `exitFace`.

### Inactive engine prototype: the echo

E sends an echo, in levels that use `echoRules`. It starts on your spot and walks back along your footprints, then stays where they end and stands in for you. The game keeps your last five footprints, your own spot included, so walking straight, the echo stops four steps back. A shift leaves a footprint too, so an echo can walk back across one and end up in the other era.

- It costs 4 seconds, and Z takes it back for half, like any action.
- There is one echo at a time. A new one replaces the old, and your footprints start again from where you sent it.
- It stops early before any footprint that is no longer open, such as one a crate has since been pushed onto.
- It holds plates and nothing else: it doesn't block anyone, pick up the cell or light the lamp.
- It is refused for free with nothing to walk back along, and from a doorway, where the old echo leaving its plate could shut the door on you.

The footprints are drawn on the floor and the one the echo would stop on is ringed, worked out by the same function the rule uses. A level turns the echo on with `echoRules`; with `baseRules` it keeps no footprints and its search is as small as before.

### Help the game gives

Corner ticks under the keeper's feet mean you can switch era right here. Holding Tab shows the other era without spending anything. The moment a push makes the level unwinnable, a line under the map says so, using a list of dead states the solver works out for the level. It says nothing about running out of time, because that would give away the par.

### Crates and pits

A crate has one position, shared by both eras, so it only goes where both have room. In the past, walking into it pushes it one cell, and it can never be pulled. In the present it is rusted solid, in every level. A crate occupies its cell in both eras, so moving it can open a crossing. Holding a direction walks up to a crate and stops: a push needs a fresh press.

A pit (`~` on the past map) blocks the past until a crate is pushed in. The crate is used up and the cell becomes past floor, even if the present is a wall there, which makes it a bridge. If the present is open too, it is also a new crossing.

A pressure plate is set in the floor of both eras. A door in the present is a wall until something rests on every plate (a crate, the player or the echo, in either era), and it shuts again when that leaves. Doors are worked out from where things stand, not stored, so undo, restart and the solver needed no new state. A crate can't be pushed into a door, open or shut. The parser refuses a plate next to a door, or the player could hold it open while walking in.

Pushes can't be walked back, so a level can now reach a state it can't be won from. So can stepping through a mirror into a room with no way out yet, or an echo sent again from beyond its door. The game says so at once, and Z and R are the way out.

### Writing a level

A level is two maps of the same size in `campaign` in `src/Game/Level.hs`, sealed by walls:

| Character | Meaning | Map |
|---|---|---|
| `#` `.` | Wall, floor | Both |
| `B` | Power cell, exactly one | Past |
| `C` | Crate, on a cell that is floor in both eras | Past |
| `~` | Pit | Past |
| `P` | Pressure plate, on a cell that is floor in both eras | Past |
| `D` | Door, opened by the plates | Present |
| `S` | Spawn, exactly one, never under a crate | Present |
| `G` | Lighthouse, exactly one | Present |
| `^` `v` `<` `>` | Mirror set in the wall, facing that way | Either: the era it works in |
| `n` `s` `e` `w` | Movable mirror on floor, facing north, south, east or west; never on a crate's cell | Either: the era it works in |
| `L` `J` `7` `F` | Corner mirror set in the wall, looking north and east, north and west, south and west, or south and east | Either: the era it works in |
| `T` | The second keeper's spawn, in a level for two | Present |

Give it a name, a par and a slack, then run the tests. They re-derive the par through the real rules and print it, with the route's actions, the state count and the dead states. The solver proves a level can be won, not that it is any good, so each level should have one idea you can name in a sentence.

A Chinese, Japanese or Korean input method in its own mode used to swallow the letter keys (W A S D, I and the rest) before the game saw them. On Windows the game now turns input methods off for itself at startup (`keepLetterKeys` in [app/Platform.hs](app/Platform.hs), [cbits/ime.c](cbits/ime.c)), so it plays the same whatever mode the input method is in.

### Native runtime requirements

- **macOS (tested, arm64):** Apple's GLUT and OpenGL frameworks, which ship with the OS. Both are deprecated by Apple but present.
- **Windows x64 (the team's target):** Gloss's GLUT backend needs `libfreeglut.dll` beside the executable or on `PATH`. With GHCup's MSYS2 it is in `C:\ghcup\msys64\mingw64\bin`. See "Outstanding".

### Sound

Every sound is synthesised in code at startup, from oscillators, noise and filters in
[app/Synth.hs](app/Synth.hs). Nothing is loaded from a file, as with the art, but
the opening film's soundtrack, which comes with the film. Each
sound is heard in a stereo hall (small for footsteps, large for the lamp and the
ending); breath, air and water are soft pink or brown noise rather than white noise,
which hisses; and saw waves are band-limited, so they do not buzz.

| Moment | What is heard |
|---|---|
| A step | A footstep: boards and plaster in the past, wet stone in the present, three slightly different feet; in level 4 the red keeper's tread is lighter and higher than the yellow's |
| A push | A crate scraping one cell and settling; a movable mirror's sled scraping, its glass ringing; a crate falling into a pit |
| Through a mirror | A shimmer rising into a clear glass ring; through a corner mirror it rings, turns and rises again a fourth higher |
| A shift | A falling, warm wave into the past; a rising, colder one into the present |
| The power cell | A quick bright arpeggio over its hum |
| The lamp | A warm chord swelling, a bell over it |
| Refused, undone, out of time | A bump, a dull double buzz, a tape run backwards, three falling notes |
| Behind it all | Quietly, in wide stereo: a calm sea in the past; wind moaning, a distant roll and rain in the present |
| The courier's talk | The box slapping up on a rising chirp, a square-wave blip every third letter at a few nearby pitches, a falling chirp as it goes |

Level 3's ending is scored to its picture, tick for tick: the chord of the lit lamp;
a surge climbing into the white of the flare, cut by the blow and breaking glass as
the lamp dies and its power runs down into silence; a thin ring as the light leaves
through the wall, and a crash for each wall it breaks, on the frame the screen
shakes. While the keeper may walk, a low drone waits. The rush has a beat that
speeds up with the screen and wind that rises with the keeper's speed; each lamp of
the corridor catches on the next note of a rising scale, barriers thud, passing one
whooshes, falling behind drops into the dark, and thunder follows the present's
lightning. At the real lighthouse the whole chord climbs into the white, strikes at
its height and rings on as the dark comes down; a low bell names the level beyond.

What plays is worked out like the effects, by comparing each tick with the one
before: `cues` and `beds` in [src/Game/Sound.hs](src/Game/Sound.hs), pure and tested.
[app/Audio.hs](app/Audio.hs) turns each cue into a sound at its own loudness and
pitch, and [cbits/audio.c](cbits/audio.c) mixes and plays them on a thread of its own
through Windows' `waveOut`, so a slow frame never stutters the sound. The ending's
sounds are made on a background thread once the game is running. On macOS and Linux
the mixer is a stub and the game is silent.

`--mute` starts with the sound off. `--dump-sounds=DIR` writes every sound to `DIR`
as a WAV file and exits, to listen to them outside the game.

### Development flags

`--intro` opens on the opening film whatever the save says, to watch it again. `--script=KEYS --after=N` runs keys instantly at startup, through the same ticks as the keyboard (talk and film included), and then freezes, to inspect an exact moment; `!` in it is Escape, `c` is Delete, and `H` holds Enter for one tick. `--trace` prints state changes, and whether sound is on and the film was found, and `--fps` prints frame rate. Full list at the top of [app/Main.hs](app/Main.hs).

To try the first-time flow without touching your own save, point `XDG_DATA_HOME` at an empty folder for that run: the save lives under it.

## How it fits together

```
src/Game/   pure core: no Gloss, no IO, compiled without a graphics dependency
  Types     Era, Phase, Motion, TickInput, Level, Game
  Level     the campaign and the parser that validates its maps
  Logic     stepGame and tryShift: all the rules
  Input     key events into one TickInput per tick
  Fx        decorative state, derived by comparing successive games
  Solution  the intended route, and cheapest-first search over real stepGame
  Run       which level is being played, and moving between them, the opening film too
  Sound     which sounds a tick starts, and how loud each bed is
  Story     when the courier talks, what she says and what the board points at
app/        Gloss only: Main (IO), Render, Station, Figures, Hud, Title, Font, Draw, Platform;
            Synth (making the sounds) and Audio (playing them);
            Video (the film's decoder), TalkBox (her talk and the film's screen),
            PixelFont, Portrait (generated)
cbits/      audio.c: the mixer and Windows sound output
assets/     intro.blv, the opening film; source/, the portrait it was made from
tools/      Blender scripts that make the film and the portrait
test/       plain assertions over the core
```

### The era transition

A shift is an ordinary pure function. It flips the era if the player's cell is open in the other one and the time is there, and records why not if it isn't:

```haskell
tryShift level g
  | gMotion g /= Settled = g
  | not (openCell level g destination (gCell g)) = refuse (ShiftBlocked (gCell g))
  | gRemaining g < cost = refuse ShiftTooCostly
  | otherwise =
      touchCell level (charge cost cleared) { gEra = destination, gMotion = Shifting shiftTicks, ... }
```

The era changes at once. `Shifting` only locks input for 13 ticks (about 0.22 s) while the wave crosses the map, so what you see and what collides can't disagree. `Motion` is a sum type (`Settled | Stepping | Shifting`), so walking and shifting at the same time can't be represented. `touchCell` is the only place pickup and victory happen, and it runs both when a step lands and when an era changes.

Each `stepGame` tick notes fresh presses, advances the current step or wave, ends the level if nothing is affordable, then starts what is pending (undo, then shift, then move). Ticks spend nothing: an action is charged once, when it is accepted. A win changes the phase before the affordability check, which is why reaching the goal on the last second wins.

## Art direction

**Harrow Point Light Station**, a coastal lighthouse facility seen from above, at two times.

- **Past: kept.** Warm plaster, brick and polished brass, a red runner down the hall, lit wall lamps, dust rising in the light, calm teal water.
- **Present: left to the sea.** The same walls in cold slate with cracks and moss, the runner rotted to strips, the lamps smashed, seawater pooled on sunken floor, spray blowing the other way, dark water.

Both eras share foundations, the floor's diamond inlay, the room stencils (worn from `LAMP` to `L MP`), the conduit along the north wall, the lamp brackets, the power-cell cradle and the lighthouse's circular footing. Colour is never the only cue: the era is written above the map, named on a plaque that is polished brass in one era and corroded steel in the other, and shown in the condition of every surface.

The two opposite passages explain themselves:

| Cell | Past | Present |
|---|---|---|
| (4,2) | A door standing open, with brass thresholds and a runner through it | The lintel has fallen; a heap of rubble fills it |
| (8,6) | A bolted bulkhead with a locking wheel and hazard stripes | The sea has torn it out, leaving jagged stubs and wet floor |

Walls and crates are bright and raised, and walkable cells are dark and flat. A pit is the one blocked cell drawn low, so it has a hazard-striped rim around a black shaft.

**The shift** is the centrepiece: tiles flip edge-on, one by one, in order of distance from the player, behind a thin expanding ring. A refused shift outlines and hatches the cell and says why, with only a small shudder on the keeper. **Victory** moves the keeper aside, relights the tower, turns long beams through the scene and soaks warm light outward.

Everything is procedural geometry, with no external assets but the opening film and the courier's portrait, both pixel art, kept to their own pixel grid. Glows are layered transparency, an approximation rather than real bloom. Lettering is a custom stroke font in [app/Font.hs](app/Font.hs), because Gloss's built-in text is one pixel wide.

## Solution walkthrough

For the original Harrow Point map, which is now the tests' fixture and not part of the campaign. Found by searching through the real `stepGame`, and also replayed through tick inputs and OS key events.

| # | Keys | Where you end up |
|---|---|---|
| 1 | Space | Past, so the left door is open |
| 2 | D, W W W W, D D | (5,2), through the door at (4,2) |
| 3 | Space | Present, so the right wall is breached |
| 4 | S S S S, D D D D | (9,6), through the breach at (8,6) |
| 5 | Space | Past, where the cell still exists |
| 6 | W W W W, D | (10,2): cell collected |
| 7 | Space | Present, to use the breach again |
| 8 | S S S S, A A A | (7,6) |
| 9 | Space | Past, to use the door again |
| 10 | W W W W, A A A A | (3,2) |
| 11 | Space | Present |
| 12 | A | (2,2), the lighthouse: win |

36 steps and 6 shifts make 42 actions, costing 108 seconds of budget.

## Evidence

### Verified: executed or directly observed here

Toolchain, on macOS 26.5.2 (arm64):

| Component | Version |
|---|---|
| GHC | 9.6.7 |
| cabal-install | 3.12.1.0 |
| gloss | 1.13.2.2 |
| gloss-rendering | 1.13.2.1 |
| GLUT (Haskell binding) | 2.7.0.16 |
| OpenGL (Haskell binding) | 3.0.3.0 |
| containers | 0.6.7 |

- **Automated tests:** all pass, covering the 18 required items plus parser, input-adapter and effects checks. Latest count: see "Test results" below.
- **Reachability in the implementation (the Harrow Point fixture):** 255 reachable states, 0 dead ends with the budget refilled, cheapest route 42 actions with 6 shifts. This confirms the earlier abstract model. Every walkable cell in both eras can be reached, and no route uses fewer than six shifts.
- **Real window, real OS key events** (posted to the process): Enter, Space, tap, hold-to-repeat, P, R and the route to victory. Holding Space for 1 s shifted once. Space during a step shifted after landing. Keys pressed while paused were ignored.
- **Tick rate against the wall clock:** 616 ticks in about 10.2 s, including process launch overhead. Measured before time became a budget, when the same ticks drove a countdown.
- **Window close:** pressing the close button through the accessibility API exits with code 0.
- **Resizing:** scales and letterboxes correctly at 900×700.
- **Visual QA:** every screen in `docs/` was captured from the running game and inspected.

### Documentation-only: documented, not tested here

- Gloss on Windows needs freeglut.
- freeglut ends the program when its window closes, by default, which is why `Platform.hs` does nothing outside macOS.

### Unverified: not established

- **On Windows:** window close and frame rate. It builds, passes its tests and runs on Windows 11 x64, with key events posted to the window and a person playing some of the levels.
- A standalone, redistributable build on any platform.
- Physical key feel, including real auto-repeat behaviour: testing used injected events. Gloss turns GLUT key repeat off and `Game.Input` ignores repeated key-downs regardless.
- A physical mouse click on the close button: the test pressed it through accessibility.
- Keys held while the window loses focus: the key-up may never arrive, which would leave a direction held. Not tested.

## Outstanding checks on Windows

1. Play the whole campaign with a real keyboard, including holding keys.
2. Close the window and confirm the process exits.
3. Check the frame rate on team laptops.

## Known issues and limitations

- **Frame rate:** 40–58 fps measured here, while other sessions were using the GPU and CPU, and about 30 fps with every core saturated. The rules are fixed-step, so the route still won at that load, and frame rate cannot affect the time budget. Gloss issues one draw call per shape, so a weaker GPU may be slower. If so, reduce the dressing in `floorDressing` first.
- **Rendering is 1× on Retina Macs.** Apple's GLUT has no high-resolution mode, so the window is upscaled and edges look slightly pixelated. No anti-aliasing is available through Gloss.
- **Off-route script runs:** one of 12 scripted key-event runs left the route. A later run recorded Up moves that the script never sent, so keystrokes from outside the script reached the window, and that is the likely cause, though it is not proven for that specific run.
- **Two taps of one key inside a single tick count as one press.** That needs a stall, and did not appear under load testing.
- **`Platform.hs` uses deprecated macOS APIs and the Objective-C runtime** because Apple's GLUT disables the close button. It is the only platform-specific code.
- The optional alternate-era preview key was not added.
- Sound plays on Windows only. macOS and Linux build and run silently.
- The sounds have been checked by measurement (length, peaks, clicks, loop seams) and their timing by the tests, not yet by ear on every machine.

## Asset credits

None. All art, lettering and sound is procedural code in this repository.

## Test results

```
search: 255 reachable states, 0 dead ends
search: cheapest route costs 108 s, 42 actions, 6 of them shifts
search: W W W W D Space D D S S S S D D Space D D W W W W D Space A S S S S Space A A A A W W W W Space A A A Space
route:  intended route takes 373 ticks, ends StateKey {skEra = Present, skCell = (2,2), skHasCell = True, skPhase = Won, skCrates = fromList [], skFilled = fromList []}
level:  Clear the Way: par 48, budget 68, 16 actions, 4 shifts, 47 states, 0 dead, 71% shared
level:  Two Alignments: par 80, budget 96, 29 actions, 4 shifts, 13011 states, 10513 dead, 69% shared
level:  The Long Way Round: par 306, budget 346, 109 actions, 18 shifts, 190082 states, 26111 dead, 74% shared
level:  Two Keepers: par 118 for two, budget 178, 48 actions, 4 shifts, 20500 states
level:  WWDDDWWSWwWwW_AawaasswADWaaaddsdssd_swww_ddwwwd_
384 passed, 0 failed
```
