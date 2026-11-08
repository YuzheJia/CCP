// Borrowed Light — cover concepts, drawn the way the game draws: a 1280 x 800 canvas
// centred on the origin with y up. title-core.js gives the fonts, the title art and
// the ticket pieces; sprites.js (a copy of the character sheet's) gives the courier walker.
// Everything new is in whole canvas units, so it can be written again in Gloss.
"use strict";

const SP = window.Sprites;
const WARM = 0xFFF3D6, COOL = 0xC9D6DB, SLATE = 0x33434C, GLOW = 0xFFE2A8;

// ---- Small helpers ---------------------------------------------------------------

// Draw into a scratch canvas covering the world box centred on (xc, yc), optionally
// recolour what was drawn (post runs with source-atop), then lay it on the screen.
function offscreen(xc, yc, w, h, draw, post) {
  const cw = Math.ceil(w * SCALE), ch = Math.ceil(h * SCALE);
  const cv = document.createElement("canvas"); cv.width = cw; cv.height = ch;
  const saved = C; C = cv.getContext("2d");
  C.setTransform(SCALE, 0, 0, -SCALE, -(xc - w / 2) * SCALE, (yc + h / 2) * SCALE);
  draw();
  if (post) { C.globalCompositeOperation = "source-atop"; post(); C.globalCompositeOperation = "source-over"; }
  C = saved;
  push(); C.setTransform(1, 0, 0, 1, 0, 0);
  C.drawImage(cv, DEV_W / 2 + (xc - w / 2) * SCALE, DEV_H / 2 - (yc + h / 2) * SCALE);
  pop();
}

// The courier walker from the character sheet, feet on y = feetY.
function courierFig(x, feetY, k, dir, phase = 0) {
  push(); T(x, feetY + 21 * k); C.scale(k, k);
  SP.courier(dir, phase)({ x: C, c: "#000" });
  pop();
}

// The talk arrow turned to point right; px is its pixel, (x, y) its left end, centred.
function arrowRight(x, y, px = 3) {
  const rows = [[0, 7], [0, 7], [0, 7], [-3, 13], [-2, 11], [-1, 9], [0, 7], [1, 5], [2, 3], [3, 1]];
  const off = 3.5 * px;
  push(); T(x, y);
  paint(NIGHT); rows.forEach(([y0, n], i) => R(i * px - px, -(y0 + n + 1) * px + off, px * 3, (n + 2) * px));
  paint(SUN); rows.forEach(([y0, n], i) => R(i * px, -(y0 + n) * px + off, px, n * px));
  pop();
}

// A pixel-font word as a logo: a drop shadow, then the letters. align L / C / R on x.
function pixelLogo(px, s, x, yTop, align, fill, shade, dx, dy) {
  const w = pixelW(s) * px;
  const L = align === "R" ? x - w : align === "C" ? x - w / 2 : x;
  paint(NIGHT, 0.55); ptext(px, s, L + dx * 2, yTop - dy * 2);
  paint(shade); ptext(px, s, L + dx, yTop - dy);
  paint(fill); ptext(px, s, L, yTop);
}

// A vertical menu. The chosen item is a talk-box tag (ink plate, rim letters, her
// yellow shadow) with the talk arrow beside it; the rest are plain rim letters.
// onInk: the menu sits on an ink panel, so the chosen plate is her yellow instead.
function menu(items, sel, x, yTop, pitch = 42, px = 3, onInk = false) {
  const h = 7 * px;
  items.forEach((it, i) => {
    const y = yTop - i * pitch;
    const w = pixelW(it.label) * px;
    if (i === sel && onInk) {
      paint(SUN); R(x - 12, y - h - 9, w + 24, h + 18);
      paint(INK); ptext(px, it.label, x, y);
      arrowRight(x - 50, y - h / 2, 2);
      if (it.note) { paint(SUN); ptext(2, it.note, x + w + 24, y - 4); }
    } else if (i === sel) {
      paint(SUN); R(x - 12 + 6, y - h - 9 - 6, w + 24, h + 18);
      paint(INK); R(x - 12, y - h - 9, w + 24, h + 18);
      paint(RIM); ptext(px, it.label, x, y);
      arrowRight(x - 50, y - h / 2, 2);
      if (it.note) { paint(SUN); ptext(2, it.note, x + w + 30, y - 4); }
    } else {
      paint(RIM, it.dim ? 0.38 : 0.7); ptext(px, it.label, x, y);
      if (it.note) { paint(MUTED, 0.9); ptext(2, it.note, x + w + 18, y - 4); }
    }
  });
}

// Pixel stars: small squares, some brighter.
function stars(x0, x1, y0, y1, n, salt, alpha = 1) {
  for (let i = 0; i < n; i++) {
    const x = x0 + (x1 - x0) * rnd(i, 1, salt), y = y0 + (y1 - y0) * rnd(i, 2, salt);
    const big = rnd(i, 3, salt) > 0.82;
    paint(0xF4EEDF, alpha * (0.35 + 0.5 * rnd(i, 4, salt)));
    R(x, y, big ? 3 : 2, big ? 3 : 2);
  }
}

// Rain over any strip, as Title.rain does for the present's half.
function rainStrip(clock, x0, w, n, salt, alpha = 1) {
  for (let i = 0; i < n; i++) {
    const speed = 2.4 + 2.2 * rnd(i, 2, salt);
    const x = x0 + wrapTo(w, w * rnd(i, 1, salt) - clock * speed * 0.42);
    const y = -400 + wrapTo(800, 800 * rnd(i, 4, salt) - clock * speed);
    paint(0xB7CEDA, alpha * (0.16 + 0.22 * rnd(i, 3, salt)));
    push(); T(x, y); ROT(-22); RC(15, 1.2); pop();
  }
}

const footer = () => { paint(MUTED, 0.75); ptextR(2, "CSS GAMEJAM 2026", 608, 372); };

// ---- Now: the title as it is (Title.hs) ------------------------------------------

function renderNow(S) {
  titleScene(S.t);
  paint(0xF7DDB2); stext(15, "PAST", -600, 356, "L");
  paint(0xA9B9BF); stext(15, "PRESENT", 600, 356, "R");
  push(); T(4, -4);
  paint(0xFFF3D6); stext(68, "BORROWED", -74, 44, "R", 8.6);
  paint(0xE8F0F2); stext(68, "LIGHT", 204, 44, "L", 8.6);
  pop();
  paint(0xF2E6D0); stext(14, "Borrow a power cell from the past to light the present", 0, -236, "C");
  paint(0xFFE2A8); stext(14, "Every action spends time.", 0, -258, "C");
  paint(0xF2E6D0); stext(14, "Press 'I' to see instructions", 0, -280, "C");
  const glow = 0.72 + 0.28 * Math.sin(S.t * 0.07);
  paint(0x000000, 0.45); R(-150, -350 + 6 - 21, 300, 42);
  paint(0xFFE2A8, glow); stext(19, "Press Enter to begin", 0, -350, "C");
}

// ---- A: the split tower, with her at the seam ------------------------------------

// The present's half of the sky and sea once the lamp is lit again: the storm gone,
// a cold pre-dawn, stars, and the lamp's gold on the water.
function presentAfter(clock) {
  bands(0, 640, [0x4B5A66, 0x3C4A57, 0x2E3A47, 0x222C38, 0x18202A, 0x11171F, 0x0C1117, 0x090D12], HORIZON, 400);
  stars(10, 636, 20, 396, 70, 41);
  paint(0x5A6A76, 0.35);
  for (let i = 0; i <= 8; i++) {
    push(); T(40 + 590 * rnd(i, 1, 42), HORIZON + 30 + 200 * rnd(i, 2, 42)); ROT(-3 + 5 * rnd(i, 3, 42));
    ell(9, 40 + 70 * rnd(i, 4, 42), 4 + 4 * rnd(i, 5, 42)); pop();
  }
  bands(0, 640, [0x08131A, 0x0B1A22, 0x10222B, 0x172C35], -400, HORIZON);
  for (let i = 0; i <= 42; i++) {
    const x = 8 + 625 * rnd(i, 4, 4), y = -395 + (HORIZON + 390) * rnd(i, 5, 4), w = 8 + 22 * rnd(i, 6, 4);
    thin(0x7D93A0, 0.3, [[x - w, y], [x, y + 1.5], [x + w, y]]);
  }
  for (let i = 0; i <= 26; i++) {
    const x = 30 + 560 * rnd(i, 4, 44), y = -395 + (HORIZON + 390) * rnd(i, 5, 44), w = 8 + 18 * rnd(i, 6, 44);
    thin(0xFFD49A, 0.45, [[x - w, y], [x, y + 1.5], [x + w, y]]);
  }
  paint(0xFFFFFF, 0.35); R(-0.8, -400, 1.6, 800);
}
// The present's lantern lit, the broken bar still broken, and someone up there.
function presentLampLit(keeper) {
  paint(0xFFEFC4); R(0, 158, 24, 44);
  paint(0xFFFFFF); R(0, 164, 14, 32);
  if (keeper) {
    paint(0x2B2228);
    P([[4, 158], [19, 158], [17, 172], [6, 172]]);
    push(); T(11.5, 180); ell(10, 5.2, 6.4); pop();
    P([[6.5, 168], [16.5, 168], [17, 182], [6, 182]]);
  }
  paint(0x5E6C72); R(22, 158, 2.5, 44); R(11, 158, 1.6, 26);
  thin(0x8A9AA0, 0.6, [[3, 200], [10, 182], [6, 170], [16, 160]]);
}

// Her at the tower's door, standing on the line between the times: the half of her
// in the present is cooled and dimmed, the half in the past catches the sunset.
function meiAtSeam(dir, warmth = 1, coolK = 1) {
  offscreen(0, -85, 150, 160, () => courierFig(0, -134, 1.35, dir), () => {
    paint(0xFF9A50, 0.14 * warmth); R(-75, -170, 75, 170);
    paint(0x0B1A22, 0.5 * coolK); R(0, -170, 75, 170);
  });
}

// The order slip on the present's side: what Continue opens.
const SLIP = { x: 436, top: -146, w: 340, h: 222 };
function orderSlip(o, sel) {
  const { x, top, w, h } = SLIP, L = x - 150, Rr = x + 150;
  if (sel) { paint(SUN); paperShape(x + 10, top - 10, w, h); } else { paint(NIGHT, 0.6); paperShape(x + 6, top - 6, w, h); }
  paint(PAPER); paperShape(x, top, w, h);
  paint(FAINT); ptext(2, "RIDER: MEI", L, top - 22);
  if (o.cleared >= 3) chip("DONE", Rr, top - 17, INK, RIM); else chip("EN ROUTE", Rr, top - 17, SUN, INK);
  paint(INK); ptext(4, "ORDER " + o.n, L, top - 46);
  dashes(L - 2, Rr + 2, top - 86, FAINT, 0.7);
  routeStrip(x, top - 118, o.cleared);
  if (o.cleared >= 3) {
    stamp(x, top - 188, -5, "DELIVERED", "8 NOV  21:14", 3);
  } else {
    paint(FAINT); ptext(2, "NEXT", L, top - 162);
    paint(SUN); R(L + 56, top - 180, 22, 22);
    paint(INK); ptextC(2, String(o.cleared + 1), L + 67, top - 162);
    paint(INK); ptext(2, ROOMS[o.cleared], L + 88, top - 162);
    statRow(L, Rr, top - 192, "ON THE ROAD", "0:14:32");
  }
  tape(x, top, 1);
}

// The menu for each moment: first launch, a save on the way, and after the end.
const MENUS = {
  first: [{ label: "NEW ORDER", note: "opening film first" }, { label: "HOW TO PLAY" }, { label: "SOUND: ON" }, { label: "QUIT" }],
  save: [{ label: "CONTINUE", note: "ORDER 1" }, { label: "YOUR ORDERS" }, { label: "HOW TO PLAY" }, { label: "SOUND: ON" }, { label: "QUIT" }],
  done: [{ label: "CONTINUE", note: "ORDER 1" }, { label: "YOUR ORDERS" }, { label: "HOW TO PLAY" }, { label: "SOUND: ON" }, { label: "QUIT" }],
};

function renderA(S) {
  const mode = S.mode, done = mode === "done", clock = S.t;
  const swing = -8 + 7 * Math.sin(clock * 0.012);
  layer("titleBack", titleBackdrop);
  if (done) layer("presentAfter", () => presentAfter(0));
  beam(180 + swing);
  if (done) beam(-swing - 0.0001, 0.9);
  layer("titleFront", () => { headland(); tower(); });
  if (done) layer("presentLamp", () => presentLampLit(S.keeper !== false));
  meiAtSeam(done ? "down" : "up", 1, done ? 0.55 : 1);
  if (!done) rain(clock);

  // The logo: BORROWED in the past's warm cream on her yellow, LIGHT in the
  // present's cold grey on slate, until the lamp is lit again.
  const top = 104, px = 10;
  pixelLogo(px, "BORROWED", -72, top, "R", WARM, SUN, 4, 4);
  if (done) pixelLogo(px, "LIGHT", 72, top, "L", WARM, SUN, 4, 4);
  else pixelLogo(px, "LIGHT", 72, top, "L", COOL, SLATE, 4, 4);
  paint(GLOW); ptextR(2, "PICK UP IN THE PAST", -72, top - 92);
  paint(done ? GLOW : 0xA9B9BF); ptext(2, done ? "DELIVERED TO THE PRESENT" : "DELIVER TO THE PRESENT", 72, top - 92);

  // Menu over the past's sea; the slip over the present's.
  const items = MENUS[mode];
  menu(items, S.sel, -556, mode === "first" ? -214 : -196, 40);
  if (mode !== "first") orderSlip(done ? { n: 1, cleared: 3 } : { n: 1, cleared: 2 }, S.sel === 0);
  footer();
}

// ---- B: her portrait, and the order as a delivery-app card --------------------------

const B_HORIZON = -176;
function bBackdrop() {
  bands(-640, 1280, [0x26343E, 0x212E37, 0x1B262E, 0x162027, 0x111A20, 0x0D141A, 0x0A1015, 0x080C10], B_HORIZON, 400);
  paint(0x34444E, 0.5);
  for (let i = 0; i <= 30; i++) {
    push(); T(-640 + 1280 * rnd(i, 1, 52), B_HORIZON + 40 + 520 * rnd(i, 2, 52)); ROT(-8 + 10 * rnd(i, 3, 52));
    ell(9, 30 + 70 * rnd(i, 4, 52), 5 + 9 * rnd(i, 5, 52)); pop();
  }
  // A thin line of old sunset under the clouds, far out: the past, out of reach.
  bands(-640, 1280, [0x5A3C3A, 0x3A3036, 0x26343E], B_HORIZON, B_HORIZON + 26);
  bands(-640, 1280, [0x050A0D, 0x070E12, 0x0A1418, 0x0D191E], -400, B_HORIZON);
  for (let i = 0; i <= 80; i++) {
    const x = -632 + 1264 * rnd(i, 4, 53), y = -395 + (B_HORIZON + 390) * rnd(i, 5, 53), w = 8 + 22 * rnd(i, 6, 53);
    thin(0x7D93A0, 0.35, [[x - w, y], [x - w / 3, y + 4], [x + w / 3, y - 3], [x + w, y]]);
  }
  // The tower, far off and wholly in the present: its own right half, mirrored.
  push(); T(36, B_HORIZON + 14 + 118 * 0.36); C.scale(0.36, 0.36);
  push(); C.beginPath(); C.rect(0, -400, 400, 800); C.clip(); headland(); tower(); pop();
  push(); C.beginPath(); C.rect(-400, -400, 400, 800); C.clip(); C.scale(-1, 1); headland(); tower(); pop();
  pop();
}

function orderCard(S, x, top) {
  const w = 452, h = 452, L = x + 26;
  // Talk box: rim, ink, and her yellow shadow, in 3-unit pixels.
  paint(SUN); R(x + 12, top - h - 12, w, h);
  paint(RIM); R(x, top - h, w, h);
  paint(INK); R(x + 6, top - h + 6, w - 12, h - 12);
  // Header: a new order has come in, or the one she is on.
  const first = S.mode === "first";
  const blink = Math.floor(S.t / 30) % 2 === 0;
  paint(blink ? SUN : 0x7A5A10); R(L, top - 40, 15, 15);
  paint(SUN); ptext(3, first ? "NEW ORDER" : "ORDER 1", L + 28, top - 22);
  paint(MUTED); ptextR(2, first ? "JUST NOW" : "EN ROUTE", x + w - 26, top - 26);
  dashes(L, x + w - 26, top - 60, MUTED, 0.5);
  paint(RIM); ptext(3, "1x LAMP WICK", L, top - 80);
  const rows = first
    ? [["PICK UP", "THE PAST"], ["DELIVER", "THE PRESENT"], ["DROP OFF", "LIGHTHOUSE, TOP FLOOR"]]
    : [["NEXT STOP", "3  THE LONG WAY ROUND"], ["ON ROAD", "0:14:32"], ["SAVED", "8 NOV 21:14"]];
  rows.forEach(([k, v], i) => {
    paint(MUTED); ptext(2, k, L, top - 122 - i * 26);
    paint(RIM); ptext(2, v, L + 118, top - 122 - i * 26);
  });
  dashes(L, x + w - 26, top - 210, MUTED, 0.5);
  const items = S.mode === "first"
    ? [{ label: "ACCEPT ORDER" }, { label: "HOW TO PLAY" }, { label: "SOUND: ON" }, { label: "QUIT" }]
    : [{ label: "CONTINUE", note: "NEXT STOP 3" }, { label: "YOUR ORDERS" }, { label: "HOW TO PLAY" }, { label: "QUIT" }];
  menu(items, S.sel, L + 52, top - 240, 42, 3, true);
  paint(MUTED, 0.85); ptext(2, "W/S CHOOSE    ENTER " + (S.mode === "first" ? "ACCEPT" : "GO"), L, top - h + 30);
}

function renderB(S) {
  layer("bBack", bBackdrop);
  // Her portrait, bottom right, looking left at the tower; cooled a little for the night.
  const px = 4, pw = PORTRAIT.w * px, ph = PORTRAIT.h * px, bx = 160;
  offscreen(bx + pw / 2, -400 + ph / 2, pw, ph, () => blit(portraitBitmap(), bx, -400 + ph, pw, ph), () => {
    paint(0x0B1C26, 0.2); R(bx, -400, pw, ph);
    paint(0xFFB45A, 0.1); R(bx, -400, 60, ph);
  });
  rainStrip(S.t, -640, 1280, 70, 61);
  // Logo, top left, and the line under it.
  pixelLogo(9, "BORROWED", -586, 352, "L", COOL, SLATE, 4, 4);
  pixelLogo(9, "LIGHT", -586, 270, "L", WARM, SUN, 4, 4);
  paint(0xA9B9BF); ptext(2, "Pick up in the past. Deliver to the present.", -586, 192);
  orderCard(S, -592, 150);
  paint(MUTED, 0.75); ptextR(2, "CSS GAMEJAM 2026", 608, 372);
}

// ---- Drawing a stage ---------------------------------------------------------------

const RENDER = { now: renderNow, A: renderA, B: renderB };
function drawStage(cv, S, cssW) {
  const dpr = S.dpr ?? (window.devicePixelRatio || 1);
  const w = Math.round(cssW * dpr), h = Math.round(cssW * 800 / 1280 * dpr);
  if (cv.width !== w || cv.height !== h) { cv.width = w; cv.height = h; }
  C = cv.getContext("2d");
  SCALE = w / 1280; DEV_W = w; DEV_H = h;
  C.setTransform(1, 0, 0, 1, 0, 0); C.clearRect(0, 0, w, h);
  C.setTransform(SCALE, 0, 0, -SCALE, w / 2, h / 2);
  RENDER[S.screen](S);
}
window.TitleMockups = { drawStage, MENUS };
