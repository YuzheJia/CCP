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

`cabal run borrowed-light -- --demo` has the solver play every level, then hands you the keyboard. Add `--level=N` to start at level N.

### Controls

| Key | Action |
|---|---|
| WASD / arrows | Move |
| Space | Shift era |
| Z | Undo the last action (gives back half its time) |
| Tab | Hold to look at the other era, for free |
| P | Pause / resume |
| R | Restart |
| Enter | Start / retry |
| Window close button | Exit |

### Time

Time is a budget, not a clock. A step costs 2 seconds and a shift costs 6, charged when the action is accepted. Standing still, pausing, refused actions and restarting cost nothing. Z restores the board exactly but refunds only half the action's cost, and R refills the budget at the price of walking it all again. With less than one step's price left, the level is lost, though Z still works from the lost screen. The prices live in a plain `Rules` record on each `Level`.

Each level's budget is its par, the cheapest possible cost found by the solver, plus slack that shrinks as the game goes on.

### The campaign

Three levels, sized for a three-minute demo, each adding one idea:

| # | Level | Idea | Par | Budget |
|---|---|---|---|---|
| 1 | Through the Glass | Stand in a mirror's light and the key toward it steps through; how far depends on where you stand | 22 | 42 |
| 2 | Rust in the Beam | A crate rusted into the present's light can be moved in the past | 34 | 50 |
| 3 | One Way In | A mirror is a one-way door: open the way out, a door the crate holds, before stepping in | 42 | 54 |

Earlier levels are kept in `extraLevels` in `src/Game/Level.hs`: First Light, the two echo levels, and the crate levels (First Push, The Plug, The Buried Lamp, The Held Door, The Bridge, The Wrong Way First). They are not played or checked, because a player standing on a plate can now change their pars.

### Mirrors

A mirror stands in one era, solid there, and throws light out of its face in a straight line until a wall, a crate, a shut door or another mirror stops it. Light crosses an open pit. On the map it is drawn as the way its face looks, `^ v < >`, on the map of the era it works in.

- **Where it puts you:** as far behind the mirror as you stand in front of it. Two cells in front, two cells behind.
- **How to use it:** stand in its light and press the key that points at the mirror. If you could stand where your reflection is, you step through, in the same era. If not, the key just walks you a cell, as it always would.
- **Reading it before you press:** in the light, your reflection shows where you would land, with an arrow beside you on the key that does it. If the landing is blocked, a red cross shows instead and the key only walks. Other keys never jump.
- **Holding a key** walks up into the light and stops; jumping takes a fresh press, like a push.
- **Price:** 4 seconds, however far. Z takes it back for half, like any action.

The light, the reflection and the jump are all worked out from the board by one function, `mirrorAhead`, so what is drawn cannot disagree with what happens. Jumping is an ordinary move, so the solver needed no new action.

### The echo (not in the campaign)

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

A crate has one position, shared by both eras, so it only goes where both have room: it can't be pushed into a cell that is a wall in the present. In the past, walking into it pushes it one cell, and it can never be pulled. In the present it is rusted solid. You can't stand on its cell in either era, so a crate on a cell that is open in both closes that crossing, and moving it can open one. Holding a direction walks up to a crate and stops: a push needs a fresh press, so a walk can't shove one into a dead end.

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

Give it a name, a par and a slack, then run the tests. They re-derive the par through the real rules and print it, with the route's actions, the state count and the dead states. The solver proves a level can be won, not that it is any good, so each level should have one idea you can name in a sentence.

With a Chinese input method in Chinese mode, Windows swallows the letter keys before the game sees them. Switch it to English, or use the arrow keys for movement.

### Native runtime requirements

- **macOS (tested, arm64):** Apple's GLUT and OpenGL frameworks, which ship with the OS. Both are deprecated by Apple but present.
- **Windows x64 (the team's target):** Gloss's GLUT backend needs `libfreeglut.dll` beside the executable or on `PATH`. With GHCup's MSYS2 it is in `C:\ghcup\msys64\mingw64\bin`. See "Outstanding".

### Development flags

`--script=KEYS --after=N` runs keys instantly at startup and then freezes, to inspect an exact moment. `--trace` prints state changes and `--fps` prints frame rate. Full list at the top of [app/Main.hs](app/Main.hs).

## How it fits together

```
src/Game/   pure core: no Gloss, no IO, compiled without a graphics dependency
  Types     Era, Phase, Motion, TickInput, Level, Game
  Level     the campaign and the parser that validates its maps
  Logic     stepGame and tryShift: all the rules
  Input     key events into one TickInput per tick
  Fx        decorative state, derived by comparing successive games
  Solution  the intended route, and cheapest-first search over real stepGame
  Run       which level is being played, and moving between them
app/        Gloss only: Main (IO), Render, Station, Figures, Hud, Title, Font, Draw, Platform
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

**The propaganda theme.** The past is the station's glorious era, painted like a 1960s Chinese propaganda poster, and the present is the same building after the glory has expired.

- **Past:** red floors with gold star inlays, cream walls and red brick faces. Slogan banners run along the top and bottom walls, one character per wall cell: 光属于人民 ("the light belongs to the people") and 举报违规黑暗 ("report unauthorised darkness"). Couplets hang down the side walls: 灯塔万岁 ("long live the lighthouse") and 超额发光 ("overfulfil the glow quota"). Flags, hammer-and-sickle roundels and red stars are scattered over the rest of the wall, and lanterns hang over the rooms. The keeper wears a red scarf, the power cell carries a red star, and the side panel is a small sunburst poster.
- **Present:** nothing Chinese at all, to catch the player out. The station is a shut-down British seaside attraction, the Harrow Point Lighthouse Experience: the same wall spots now hold its sagging blue banner, hazard tape with DANGER and KEEP OUT signs, CCTV signs and graffiti, bare flagpoles with gulls on them, FOR SALE boards and a GIFT SHOP sign. Crisp packets and cans litter the floor, the side panel is a tourist-board sign with a CLOSED sticker, and the win is a grand reopening.

Decorations only go on wall that is solid in both eras, so every banner, flag and emblem in the past has its counterpart in the same spot in the present. [app/Station.hs](app/Station.hs) `decorPlan` places them on every map automatically. Emblems, flags and stars are drawn in [app/Propaganda.hs](app/Propaganda.hs).

Chinese characters are 32 x 32 pixel bitmaps in the generated [app/Hanzi.hs](app/Hanzi.hs), because Gloss cannot draw system fonts. To use a new character, add it to `wanted` in [tools/hanzi.swift](tools/hanzi.swift) and to the list in `Propaganda.runs`, then regenerate on a Mac:

```bash
swift tools/hanzi.swift > app/Hanzi.hs
```

The original art direction below still describes the shapes, the shift and the dressed fixture level.

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

Everything is procedural geometry, with no external assets. Glows are layered transparency, an approximation rather than real bloom. Lettering is a custom stroke font in [app/Font.hs](app/Font.hs), because Gloss's built-in text is one pixel wide.

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
- There is no audio, by design.

## Asset credits

None. All art and lettering is procedural code in this repository.

## Test results

```
search: 255 reachable states, 0 dead ends
search: cheapest route costs 108 s, 42 actions, 6 of them shifts
search: W W W W D Space D D S S S S D D Space D D W W W W D Space A S S S S Space A A A A W W W W Space A A A Space
route:  intended route takes 373 ticks, ends StateKey {skEra = Present, skCell = (2,2), skHasCell = True, skPhase = Won, skCrates = fromList [], skFilled = fromList []}
level:  Through the Glass: par 22, budget 42, 6 actions, 2 shifts, 15 states, 0 dead, 77% shared
level:  Rust in the Beam: par 34, budget 50, 12 actions, 2 shifts, 76 states, 9 dead, 28% shared
level:  One Way In: par 42, budget 54, 15 actions, 2 shifts, 46 states, 12 dead, 29% shared
275 passed, 0 failed
```
