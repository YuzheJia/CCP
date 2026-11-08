// Shared drawing core for the cover concepts: lines 1-541 of
// docs/ui-mockups/save-screen/save-screen.js, copied as they stand (canvas core, Draw.rnd,
// the pixel and stroke fonts, the title art, the ticket pieces, the courier portrait).
// Borrowed Light save screen mockups, drawn the way the game draws: a 1280 x 800
// canvas centred on the origin with y up, the stroke font from app/Font.hs, the
// pixel font from app/PixelFont.hs, the title art from app/Title.hs, the talk box
// from app/TalkBox.hs and the hall from app/Lobby.hs. Everything new is in whole
// canvas units, so it can be written again in Gloss as it stands.
"use strict";

// ---- Palette -------------------------------------------------------------------
const INK = 0x0D0B12, RIM = 0xF4F1EA, SUN = 0xF6B31C, MUTED = 0x8C8796, NIGHT = 0x05080A;
const PAPER = 0xF2EEE4, FAINT = 0x6B6575, STAMP = 0xC63D2F, TAPE = 0xF6D27A;
const ROOMS = ["CLEAR THE WAY", "TWO ALIGNMENTS", "THE LONG WAY ROUND"];
const HALL_NAMES = ["Clear the Way", "Two Alignments", "The Long Way Round"];
const LESSONS = [
  "Crates only move in the past. In the present they are rusted solid.",
  "Push a mirror from its back or sides. Step into its light to jump through.",
  "Four rooms. A mirror set in the wall never moves: it is a one-way door.",
];
const SECRET = "TWO KEEPERS";

// ---- Canvas core ---------------------------------------------------------------
let C = null;
const css = (h, a = 1) => `rgba(${(h >> 16) & 255},${(h >> 8) & 255},${h & 255},${a})`;
const mixHex = (k, a, b) => {
  const ch = s => Math.round(((a >> s) & 255) + ((((b >> s) & 255) - ((a >> s) & 255)) * k));
  return (ch(16) << 16) | (ch(8) << 8) | ch(0);
};
function paint(h, a = 1) { const c = css(h, a); C.fillStyle = c; C.strokeStyle = c; }
function R(x, y, w, h) { C.fillRect(x, y, w, h); }
function RC(w, h) { C.fillRect(-w / 2, -h / 2, w, h); }
function P(pts) {
  C.beginPath(); C.moveTo(pts[0][0], pts[0][1]);
  for (let i = 1; i < pts.length; i++) C.lineTo(pts[i][0], pts[i][1]);
  C.closePath(); C.fill();
}
function thin(h, a, pts) {
  C.strokeStyle = css(h, a); C.lineWidth = 1; C.lineCap = "butt"; C.lineJoin = "miter";
  C.beginPath(); C.moveTo(pts[0][0], pts[0][1]);
  for (let i = 1; i < pts.length; i++) C.lineTo(pts[i][0], pts[i][1]);
  C.stroke();
}
const push = () => C.save(), pop = () => C.restore();
const T = (x, y) => C.translate(x, y);
const ROT = deg => C.rotate(-deg * Math.PI / 180);   // Gloss turns clockwise
function ell(n, rx, ry) {
  const pts = [];
  for (let i = 0; i < n; i++) { const a = 2 * Math.PI * i / n; pts.push([rx * Math.cos(a), ry * Math.sin(a)]); }
  P(pts);
}
const lerp = (k, a, b) => a + (b - a) * k;
const clamp01 = v => Math.max(0, Math.min(1, v));
const smooth = t => { const u = clamp01(t); return u * u * (3 - 2 * u); };
function clipAbove(y) { C.beginPath(); C.rect(-2000, y, 4000, 2000); C.clip(); }
function clipBelow(y) { C.beginPath(); C.rect(-2000, y - 2000, 4000, 2000); C.clip(); }

// Draw.rnd, exactly: 64-bit Int arithmetic.
const M32 = 4294967296n;
function scramble(v) {
  let a = BigInt.asIntN(64, v * 1597334677n); a = ((a % M32) + M32) % M32;
  const b = a ^ (a >> 15n);
  let c = BigInt.asIntN(64, b * 2246822519n); c = ((c % M32) + M32) % M32;
  return c ^ (c >> 13n);
}
const rndCache = new Map();
function rnd(x, y, salt) {
  const key = x + "," + y + "," + salt;
  let v = rndCache.get(key);
  if (v === undefined) {
    const s = scramble(BigInt.asIntN(64, scramble(BigInt.asIntN(64, scramble(BigInt(x) * 374761n + 97n) + BigInt(y) * 668265n)) + BigInt(salt) * 2654435n));
    v = Number(((s % 100000n) + 100000n) % 100000n) / 100000;
    rndCache.set(key, v);
  }
  return v;
}

// ---- Pixel font (PixelFont.hs) -------------------------------------------------
const PF = {};
for (const [k, v] of Object.entries(PIXELFONT)) {
  const rows = v.split("|"), runs = [];
  rows.forEach((r, y) => {
    let x = 0;
    while (x < r.length) {
      if (r[x] === "#") { let n = 0; while (r[x + n] === "#") n++; runs.push([y, x, n]); x += n; } else x++;
    }
  });
  PF[k] = { runs, w: Math.max(1, ...rows.map(r => r.length)) };
}
const PF_ALIAS = { "’": "'", "‘": "'", "“": '"', "”": '"', "—": "-" };
const glyphOf = c => PF[c] || PF[PF_ALIAS[c]] || PF["?"];
const advance = c => (c === " " ? 3 : glyphOf(c).w + 1);
function pixelW(s) { let t = 0; for (const c of s) t += advance(c); return Math.max(0, t - 1); }
function ptext(px, s, x, y) {
  let cx = 0;
  for (const c of s) {
    if (c === " ") { cx += 3; continue; }
    const g = glyphOf(c);
    for (const [ry, rx, n] of g.runs) C.fillRect(x + (cx + rx) * px, y - (ry + 1) * px, n * px, px);
    cx += g.w + 1;
  }
}
const ptextC = (px, s, x, y) => ptext(px, s, x - pixelW(s) * px / 2, y);
const ptextR = (px, s, x, y) => ptext(px, s, x - pixelW(s) * px, y);
function pixelWrap(maxW, text) {
  const out = []; let row = [];
  for (const w of text.split(/\s+/).filter(Boolean)) {
    if (!row.length || pixelW([...row, w].join(" ")) <= maxW) row.push(w);
    else { out.push(row.join(" ")); row = [w]; }
  }
  if (row.length) out.push(row.join(" "));
  return out;
}
// A convex polygon filled row by row in whole pixels (TalkBox.pixelPoly).
function pixelPoly(pts, px = 3) {
  const snap = v => px * Math.round(v / px);
  const ys = pts.map(p => p[1]);
  const lo = px * Math.floor(Math.min(...ys) / px), hi = px * Math.ceil(Math.max(...ys) / px);
  for (let y = lo; y < hi - 1e-6; y += px) {
    const yc = y + px / 2, xs = [];
    for (let i = 0; i < pts.length; i++) {
      const [x1, y1] = pts[i], [x2, y2] = pts[(i + 1) % pts.length];
      if ((y1 <= yc && yc < y2) || (y2 <= yc && yc < y1)) xs.push(x1 + (yc - y1) * (x2 - x1) / (y2 - y1));
    }
    if (xs.length) { const a = snap(Math.min(...xs)), b = snap(Math.max(...xs)); if (b > a) C.fillRect(a, y, b - a, px); }
  }
}

// ---- Stroke font (Font.hs) -----------------------------------------------------
function curve(n, cx, cy, rx, ry, a0, a1) {
  const out = [];
  for (let i = 0; i <= n; i++) { const t = (a0 + (a1 - a0) * i / n) * Math.PI / 180; out.push([cx + rx * Math.cos(t), cy + ry * Math.sin(t)]); }
  return out;
}
function glyph(n, ch) {
  const a = (cx, cy, rx, ry, a0, a1) => curve(n, cx, cy, rx, ry, a0, a1);
  switch (ch) {
    case "A": return [[[0, 0], [2, 6], [4, 0]], [[0.75, 2.1], [3.25, 2.1]]];
    case "B": return [[[0, 3.1], [0, 6], [2.2, 6], ...a(2.2, 4.55, 1.5, 1.45, 90, -90), [0, 3.1]],
                      [[0, 3.1], [2.4, 3.1], ...a(2.4, 1.55, 1.6, 1.55, 90, -90), [0, 0], [0, 3.1]]];
    case "C": return [a(2.1, 3, 2.1, 3, 48, 312)];
    case "D": return [[[0, 0], [0, 6], [1.5, 6], ...a(1.5, 3, 2.5, 3, 90, -90), [0, 0]]];
    case "E": return [[[3.6, 6], [0, 6], [0, 0], [3.6, 0]], [[0, 3.1], [2.9, 3.1]]];
    case "F": return [[[3.6, 6], [0, 6], [0, 0]], [[0, 3.1], [2.9, 3.1]]];
    case "G": return [[...a(2.1, 3, 2.1, 3, 48, 315), [4, 2.7], [2.5, 2.7]]];
    case "H": return [[[0, 0], [0, 6]], [[4, 0], [4, 6]], [[0, 3.1], [4, 3.1]]];
    case "I": return [[[0, 0], [0, 6]]];
    case "J": return [[[3, 6], [3, 1.5], ...a(1.5, 1.5, 1.5, 1.5, 0, -160)]];
    case "K": return [[[0, 0], [0, 6]], [[3.8, 6], [0, 2.3]], [[1.35, 3.6], [4, 0]]];
    case "L": return [[[0, 6], [0, 0], [3.4, 0]]];
    case "M": return [[[0, 0], [0, 6], [2.5, 1.6], [5, 6], [5, 0]]];
    case "N": return [[[0, 0], [0, 6], [4, 0], [4, 6]]];
    case "O": return [a(2.2, 3, 2.2, 3, 90, 450)];
    case "P": return [[[0, 0], [0, 6], [2.3, 6], ...a(2.3, 4.3, 1.7, 1.7, 90, -90), [0, 2.6]]];
    case "Q": return [a(2.2, 3, 2.2, 3, 90, 450), [[2.6, 1.5], [4.4, -0.2]]];
    case "R": return [[[0, 0], [0, 6], [2.3, 6], ...a(2.3, 4.4, 1.7, 1.6, 90, -90), [0, 2.8]], [[1.9, 2.8], [4, 0]]];
    case "S": return [[...a(2, 4.5, 1.9, 1.5, 35, 270), ...a(2, 1.5, 2, 1.5, 90, -145)]];
    case "T": return [[[0, 6], [4, 6]], [[2, 6], [2, 0]]];
    case "U": return [[[0, 6], [0, 2], ...a(2, 2, 2, 2, 180, 360), [4, 6]]];
    case "V": return [[[0, 6], [2, 0], [4, 6]]];
    case "W": return [[[0, 6], [1.4, 0], [2.8, 4.6], [4.2, 0], [5.6, 6]]];
    case "X": return [[[0, 0], [4, 6]], [[0, 6], [4, 0]]];
    case "Y": return [[[0, 6], [2, 3], [4, 6]], [[2, 3], [2, 0]]];
    case "Z": return [[[0, 6], [4, 6], [0, 0], [4, 0]]];
    case "0": return [a(2, 3, 2, 3, 90, 450)];
    case "1": return [[[0, 4.7], [1.5, 6], [1.5, 0]]];
    case "2": return [[...a(2, 4.3, 1.9, 1.7, 165, -35), [0, 0], [4, 0]]];
    case "3": return [[...a(1.9, 4.5, 1.8, 1.5, 150, -90), ...a(1.9, 1.5, 2.1, 1.5, 90, -150)]];
    case "4": return [[[3, 0], [3, 6], [0, 1.9], [4, 1.9]]];
    case "5": return [[[3.6, 6], [0.5, 6], [0.2, 3.3], ...a(2, 1.9, 2, 1.9, 125, -145)]];
    case "6": return [[...a(2.1, 3, 2, 3, 60, 180), ...a(2, 1.9, 2, 1.9, 180, 540)]];
    case "7": return [[[0, 6], [4, 6], [1.3, 0]]];
    case "8": return [a(2, 4.55, 1.75, 1.45, -90, 270), a(2, 1.55, 2, 1.55, 90, 450)];
    case "9": return [[...a(1.9, 3, 2, 3, 240, 360), ...a(2, 4.1, 2, 1.9, 0, 360)]];
    case ".": return [[[0.3, 0], [0.3, 0.45]]];
    case ",": return [[[0.4, 0.4], [0, -0.9]]];
    case ":": return [[[0.3, 0.5], [0.3, 1]], [[0.3, 3.6], [0.3, 4.1]]];
    case "'": return [[[0.3, 6], [0.3, 4.4]]];
    case "!": return [[[0.3, 6], [0.3, 1.9]], [[0.3, 0], [0.3, 0.45]]];
    case "-": return [[[0, 2.8], [2.4, 2.8]]];
    case "+": return [[[0, 2.8], [2.8, 2.8]], [[1.4, 1.4], [1.4, 4.2]]];
    case "/": return [[[0, -0.4], [2.8, 6.4]]];
    case "*": return [[[0.2, 2.8], [0.5, 2.8]]];
    default: return [];
  }
}
const fineness = size => (size >= 40 ? 12 : size >= 18 ? 7 : 4);
const tracking = size => (size < 18 ? 0.5 : 0);
const isLower = c => c !== c.toUpperCase() && c === c.toLowerCase();
function layout(size, track, str) {
  const k = size / 6, out = [];
  let x = 0;
  for (const c of str) {
    if (c === " ") { x += 2.5 * k + track; continue; }
    const sc = isLower(c) ? 0.8 : 1;
    const strokes = glyph(fineness(size), c.toUpperCase());
    const pts = strokes.flat();
    const width = pts.length ? Math.max(...pts.map(p => p[0])) : 2;
    const m = k * sc;
    for (const s of strokes) out.push(s.map(([px, py]) => [x + px * m, py * m]));
    x += width * m + 1.5 * k + track;
  }
  return [out, x];
}
const textW = (size, str) => Math.max(0, layout(size, tracking(size), str)[1] - 1.5 * size / 6 - tracking(size));
function stext(size, str, x, y, align = "L", weight) {
  const w = weight ?? Math.max(1.25, size * 0.13);
  const ox = align === "C" ? -textW(size, str) / 2 : align === "R" ? -textW(size, str) : 0;
  const [strokes] = layout(size, tracking(size), str);
  C.lineWidth = w; C.miterLimit = 2;
  C.lineCap = size >= 26 ? "round" : "square";
  C.lineJoin = size >= 26 ? "round" : "miter";
  for (const s of strokes) {
    if (s.length === 1) { R(x + ox + s[0][0] - w / 2, y + s[0][1] - w / 2, w, w); continue; }
    C.beginPath(); C.moveTo(x + ox + s[0][0], y + s[0][1]);
    for (let i = 1; i < s.length; i++) C.lineTo(x + ox + s[i][0], y + s[i][1]);
    C.stroke();
  }
}

// ---- Layers: static art drawn once per scale ------------------------------------
const layers = new Map();
let SCALE = 1, DEV_W = 1280, DEV_H = 800;
function layer(key, draw) {
  const id = key + "@" + DEV_W;
  let cv = layers.get(id);
  if (!cv) {
    cv = document.createElement("canvas"); cv.width = DEV_W; cv.height = DEV_H;
    const saved = C; C = cv.getContext("2d");
    C.setTransform(SCALE, 0, 0, -SCALE, DEV_W / 2, DEV_H / 2);
    draw();
    C = saved; layers.set(id, cv);
  }
  push(); C.setTransform(1, 0, 0, 1, 0, 0); C.drawImage(cv, 0, 0); pop();
}
// An image placed by its top-left corner, in canvas units.
function blit(img, x, yTop, w, h) {
  push(); T(x, yTop); C.scale(1, -1); C.imageSmoothingEnabled = false; C.drawImage(img, 0, 0, w, h); pop();
}

// ---- Title art (Title.hs) ------------------------------------------------------
const HORIZON = -150;
function bands(x, w, stops, y0, y1) {
  const steps = 36, n = stops.length - 1;
  for (let i = 0; i < steps; i++) {
    const a = i / steps, pos = a * n, j = Math.min(stops.length - 2, Math.floor(pos));
    paint(mixHex(pos - j, stops[j], stops[j + 1]));
    R(x, lerp(a, y0, y1), w, (y1 - y0) / steps + 1);
  }
}
function titleBackdrop() {
  bands(-640, 640, [0xD98145, 0xC36A44, 0xA45445, 0x834547, 0x643849, 0x4B2E47, 0x382640, 0x2A2036], HORIZON, 400);
  bands(0, 640, [0x25333C, 0x202D35, 0x1B262D, 0x162026, 0x121A20, 0x0E151A, 0x0B1115, 0x090E11], HORIZON, 400);
  paint(0xF4B97E, 0.25);
  for (let i = 0; i <= 16; i++) R(-640 + 600 * rnd(i, 1, 1), HORIZON + 40 + 470 * rnd(i, 2, 1), 40 + 130 * rnd(i, 3, 1), 3);
  paint(0x34444E, 0.55);
  for (let i = 0; i <= 18; i++) {
    push(); T(20 + 620 * rnd(i, 1, 2), HORIZON + 40 + 470 * rnd(i, 2, 2)); ROT(-8 + 10 * rnd(i, 3, 2));
    ell(9, 30 + 60 * rnd(i, 4, 2), 5 + 8 * rnd(i, 5, 2)); pop();
  }
  paint(0xFFFFFF, 0.5); R(-0.8, -400, 1.6, 800);
  bands(-640, 640, [0x0F3A44, 0x124450, 0x174F58, 0x1D5A60], -400, HORIZON);
  bands(0, 640, [0x060C0F, 0x081014, 0x0A1418, 0x0D191E], -400, HORIZON);
  for (let i = 0; i <= 42; i++) {
    const x = -630 + 620 * rnd(i, 4, 3), y = -395 + (HORIZON + 390) * rnd(i, 5, 3), w = 8 + 22 * rnd(i, 6, 3);
    thin(0xFFD49A, 0.5, [[x - w, y], [x, y + 1.5], [x + w, y]]);
  }
  for (let i = 0; i <= 42; i++) {
    const x = 8 + 625 * rnd(i, 4, 4), y = -395 + (HORIZON + 390) * rnd(i, 5, 4), w = 8 + 22 * rnd(i, 6, 4);
    thin(0x7D93A0, 0.4, [[x - w, y], [x - w / 3, y + 4], [x + w / 3, y - 3], [x + w, y]]);
  }
}
function headland() {
  paint(0x3A2A22); P([[-230, -210], [0, -210], [0, -128], [-130, -128]]);
  paint(0x4C382C); P([[-130, -128], [0, -128], [0, -118], [-110, -118]]);
  paint(0x0E1518); P([[0, -210], [250, -210], [120, -120], [0, -128]]);
  paint(0x1B2429); P([[0, -128], [120, -120], [96, -112], [0, -118]]);
  paint(0x151D22); P([[140, -210], [210, -210], [182, -168], [160, -178]]);
}
const towerW = y => 46 - 18 * (y + 118) / 268;
function tower() {
  const tb = [0, 1, 2, 3, 4, 5].map(i => [i, -118 + 268 * i / 6, -118 + 268 * (i + 1) / 6]);
  for (const [i, y0, y1] of tb) { paint(i % 2 === 0 ? 0xD8493B : 0xF4EADA); P([[-towerW(y0), y0], [0, y0], [0, y1], [-towerW(y1), y1]]); }
  paint(0xFFE2A8, 0.32); P([[-towerW(-118), -118], [-towerW(-118) + 12, -118], [-towerW(150) + 8, 150], [-towerW(150), 150]]);
  for (const [i, y0, y1] of tb) { paint(i % 2 === 0 ? 0x6E3A30 : 0x7E8A8C); P([[0, y0], [towerW(y0), y0], [towerW(y1), y1], [0, y1]]); }
  paint(0x10171B, 0.5); P([[towerW(-118) - 14, -118], [towerW(-118), -118], [towerW(150), 150], [towerW(150) - 9, 150]]);
  thin(0x10171B, 1, [[6, -60], [14, -30], [8, 2], [20, 40], [12, 86], [19, 130]]);
  thin(0x10171B, 1, [[14, -30], [28, -12]]);
  paint(0x5F7F4E, 0.85); push(); T(26, -112); ell(10, 20, 8); pop();
  paint(0x5F7F4E, 0.7); push(); T(12, -104); ell(8, 11, 5); pop();
  paint(0x4A2E1E); P([[-13, -118], [0, -118], [0, -72], [-13, -80]]);
  paint(0x0A0E11); P([[0, -118], [13, -118], [13, -80], [0, -72]]);
  paint(0x2A1A12); R(-44, 150, 44, 8);
  paint(0xC89C50); R(-44, 156, 44, 3);
  paint(0x10171B); R(0, 150, 40, 8);
  paint(0x4F5B60); R(0, 156, 36, 2);
  paint(0xFFEFC4); R(-24, 158, 24, 44);
  paint(0xFFFFFF); R(-14, 164, 14, 32);
  paint(0x2A1A12); R(-25, 158, 2.5, 44); R(-13, 158, 1.6, 44);
  paint(0x0D1317); R(0, 158, 24, 44);
  paint(0x5E6C72); R(22, 158, 2.5, 44); R(11, 158, 1.6, 26);
  thin(0x8A9AA0, 1, [[3, 200], [10, 182], [6, 170], [16, 160]]);
  paint(0xA8362E); P([[-30, 202], [0, 202], [0, 232]]);
  paint(0x4B3A38); P([[0, 202], [28, 202], [0, 232]]);
  paint(0x0A1014); P([[16, 202], [28, 202], [24, 214]]);
}
function beam(deg, k = 1) {
  push(); T(0, 180); ROT(deg);
  paint(0xFFE2A8, 0.1 * k); P([[0, -14], [760, -150], [760, 150], [0, 14]]);
  paint(0xFFF1C2, 0.14 * k); P([[0, -10], [760, -70], [760, 70], [0, 10]]);
  pop();
}
const wrapTo = (size, v) => v - size * Math.floor(v / size);
function rain(clock) {
  for (let i = 0; i <= 34; i++) {
    const speed = 2.4 + 2.2 * rnd(i, 2, 8);
    const x = 8 + wrapTo(632, 632 * rnd(i, 1, 8) - clock * speed * 0.42);
    const y = -400 + wrapTo(800, 800 * rnd(i, 4, 8) - clock * speed);
    paint(0xB7CEDA, 0.16 + 0.22 * rnd(i, 3, 8));
    push(); T(x, y); ROT(-22); RC(15, 1.2); pop();
  }
}
function titleScene(clock, beamDeg) {
  layer("titleBack", titleBackdrop);
  beam(beamDeg ?? 180 + (-8 + 7 * Math.sin(clock * 0.012)));
  layer("titleFront", () => { headland(); tower(); });
  rain(clock);
}

// ---- Shared pieces ---------------------------------------------------------------
// The keys under a screen, in the title's keycap style.
function keyRow(items, y, alpha = 1) {
  if (alpha <= 0.01) return;
  const gap = 34;
  const widths = items.map(([k, l]) => textW(12, k) + 16 + 9 + textW(12, l));
  let x = -(widths.reduce((s, w) => s + w, 0) + gap * (items.length - 1)) / 2;
  items.forEach(([key, label], i) => {
    const w = textW(12, key) + 16;
    paint(0xFFFFFF, 0.6 * alpha); R(x, y - 7, w, 24);
    paint(0x0B1014, alpha); R(x + 1.5, y - 5.5, w - 3, 21);
    paint(0xF4EEE2, alpha); stext(12, key, x + w / 2, y, "C");
    paint(0xF4EEE2, 0.85 * alpha); stext(12, label, x + w + 9, y);
    x += widths[i] + gap;
  });
}
// The marker arrow from the talk system, pointing down at y.
function arrowAt(x, y) {
  const px = 3, rows = [[0, 7], [0, 7], [0, 7], [-3, 13], [-2, 11], [-1, 9], [0, 7], [1, 5], [2, 3], [3, 1]];
  push(); T(x, y);
  paint(NIGHT); rows.forEach(([x0, n], i) => R((x0 - 1) * px - 10.5, (9 - i) * px - px, (n + 2) * px, px * 3));
  paint(SUN); rows.forEach(([x0, n], i) => R(x0 * px - 10.5, (9 - i) * px, n * px, px));
  pop();
}
// A pixel tag, centred: paper-white letters on ink with a yellow shadow.
function toastTag(text, y, alpha = 1, warn = false) {
  if (alpha <= 0.01) return;
  const w = pixelW(text) * 2 + 30, h = 36;
  paint(warn ? STAMP : SUN, alpha); R(-w / 2 + 6, y - h - 6, w, h);
  paint(INK, alpha); R(-w / 2, y - h, w, h);
  paint(RIM, alpha); ptextC(2, text, 0, y - 11);
}

// ---- The ticket -----------------------------------------------------------------
const TW = 288, TH = 450;
function paperShape(x, top, w = TW, h = TH) {
  const L = x - w / 2, n = 24, tw = w / n, t = 6;
  R(L, top - h + t, w, h - 2 * t);
  for (let i = 0; i < n; i++) {
    pixelPoly([[L + i * tw, top - t], [L + (i + 1) * tw, top - t], [L + (i + 0.5) * tw, top]]);
    pixelPoly([[L + i * tw, top - h + t], [L + (i + 0.5) * tw, top - h], [L + (i + 1) * tw, top - h + t]]);
  }
}
function dashes(L, R_, y, colour, alpha = 1) {
  paint(colour, alpha);
  for (let x = L; x < R_ - 2; x += 10) R(x, y, Math.min(6, R_ - x), 2);
}
function statRow(L, R_, y, label, value) {
  paint(FAINT); ptext(2, label, L, y);
  paint(INK); ptextR(2, value, R_, y);
  const a = L + pixelW(label) * 2 + 8, b = R_ - pixelW(value) * 2 - 8;
  paint(FAINT, 0.7);
  for (let x = a; x < b - 1; x += 6) R(x, y - 14, 2, 2);
}
function chip(text, right, top, bg, fg) {
  const w = pixelW(text) * 2 + 14;
  paint(bg); R(right - w, top - 24, w, 24);
  paint(fg); ptext(2, text, right - w + 7, top - 5);
}
function check(x, y, colour) {   // a tick in 3-unit pixels, centred
  paint(colour);
  [[-2, 0], [-1, -1], [0, -2], [1, -1], [2, 0], [3, 1]].forEach(([i, j]) => R(x + i * 3 - 1.5, y + j * 3 - 1.5, 3, 3));
}
function wickIcon(x, y) {
  paint(INK); R(x - 6, y - 12, 12, 21); R(x - 3, y + 9, 6, 3);
  paint(SUN); R(x - 3, y - 9, 6, 15);
  paint(0xFFF1C2); R(x - 3, y + 0, 3, 6);
}
function lighthouseIcon(x, y, lit) {
  const rows = [15, 15, 12, 12, 12, 9, 9];
  rows.forEach((w, i) => {
    paint(lit ? (Math.floor(i / 2) % 2 === 0 ? 0xD8493B : 0xE9DFCF) : (Math.floor(i / 2) % 2 === 0 ? 0x9F98A6 : 0xC9C3CE));
    R(x - w / 2, y - 15 + i * 3, w, 3);
  });
  paint(INK); R(x - 9, y + 6, 18, 3);
  paint(lit ? SUN : INK); R(x - 4.5, y + 9, 9, 6);
  paint(lit ? 0xA8362E : INK); R(x - 6, y + 15, 12, 3); R(x - 3, y + 18, 6, 3);
  if (lit) { paint(SUN); R(x - 24, y + 10.5, 12, 3); R(x + 12, y + 10.5, 12, 3); R(x - 21, y + 18, 9, 3); R(x + 12, y + 18, 9, 3); }
}
// Wick, three rooms, the lamp: what is behind her, where she is going.
function routeStrip(cx, cy, cleared) {
  const L = cx - 126, xs = [L + 12, L + 69, L + 126, L + 183, L + 240];
  const done = cleared >= 3;
  for (let i = 0; i < 4; i++) {
    const a = xs[i] + 13, b = xs[i + 1] - 13;
    if (i < cleared || done) { paint(INK); R(a, cy - 1.5, b - a, 3); }
    else {
      paint(i === cleared ? INK : FAINT, i === cleared ? 1 : 0.6);
      for (let x = a; x < b - 1; x += 6) R(x, cy - 1.5, 3, 3);
    }
  }
  wickIcon(xs[0], cy);
  for (let k = 1; k <= 3; k++) {
    const x = xs[k];
    if (k <= cleared) { paint(INK); R(x - 10.5, cy - 10.5, 21, 21); check(x, cy, RIM); }
    else if (k === cleared + 1) { paint(INK); R(x - 12, cy - 12, 24, 24); paint(SUN); R(x - 9, cy - 9, 18, 18); }
    else { paint(FAINT); R(x - 10.5, cy - 10.5, 21, 21); paint(PAPER); R(x - 7.5, cy - 7.5, 15, 15); }
    paint(k <= cleared + 1 ? INK : FAINT); ptextC(2, String(k), x, cy - 18);
  }
  lighthouseIcon(xs[4], cy, done);
  paint(FAINT); ptextC(2, "WICK", xs[0], cy - 18); ptextC(2, "LAMP", xs[4], cy - 18);
}
function stamp(x, y, deg, text, sub, seed) {
  push(); T(x, y); ROT(deg);
  const w = pixelW(text) * 3 + 30, h = 62;
  paint(STAMP, 0.88);
  R(-w / 2, -h / 2, w, 3); R(-w / 2, h / 2 - 3, w, 3); R(-w / 2, -h / 2, 3, h); R(w / 2 - 3, -h / 2, 3, h);
  R(-w / 2 + 6, -h / 2 + 6, w - 12, 2); R(-w / 2 + 6, h / 2 - 8, w - 12, 2);
  ptextC(3, text, 0, h / 2 - 13);
  ptextC(2, sub, 0, -h / 2 + 24);
  // Worn rubber: a few specks of paper show through.
  paint(PAPER, 0.75);
  for (let i = 0; i < 26; i++) R(-w / 2 + w * rnd(seed, i, 31), -h / 2 + h * rnd(seed, i, 32), 3, 3);
  pop();
}
function barcode(cx, top, seed) {
  const widths = [], gaps = [];
  let total = 0;
  for (let i = 0; total < 196; i++) {
    const w = [2, 2, 4, 2, 6][Math.floor(rnd(seed, i, 7) * 5)], g = [2, 4, 2, 4][Math.floor(rnd(seed, i, 9) * 4)];
    widths.push(w); gaps.push(g); total += w + g;
  }
  let x = cx - total / 2;
  paint(INK);
  widths.forEach((w, i) => { R(x, top - 34, w, 34); x += w + gaps[i]; });
}
// One filled ticket. `top` is the top of its paper.
function ticketBody(o, x, top) {
  const L = x - 126, Rr = x + 126;
  paint(FAINT); ptext(2, "RIDER: MEI", L, top - 24);
  paint(INK); ptext(4, "ORDER " + o.n, L, top - 50);
  if (o.cleared >= 3) chip("DONE", Rr, top - 19, INK, RIM);
  else chip("EN ROUTE", Rr, top - 19, SUN, INK);
  paint(FAINT); ptext(2, "1x LAMP WICK > TOP FLOOR", L, top - 90);
  if (o.tearK === undefined) dashes(L - 2, Rr + 2, top - 114, FAINT, 0.7);
  routeStrip(x, top - 152, o.cleared);
  if (o.cleared >= 3) {
    paint(FAINT); ptext(2, "DELIVERED TO", L, top - 196);
    paint(INK); ptext(2, "THE LAMP, TOP FLOOR", L, top - 218);
    if (o.secret) { paint(STAMP); ptext(2, "+ HIDDEN STOP FOUND", L, top - 240); }
  } else {
    paint(FAINT); ptext(2, "NEXT STOP", L, top - 196);
    paint(SUN); R(L, top - 238, 22, 22);
    paint(INK); ptextC(2, String(o.cleared + 1), L + 11, top - 220);
    paint(INK); ptext(2, ROOMS[o.cleared], L + 30, top - 220);
  }
  dashes(L - 2, Rr + 2, top - 260, FAINT, 0.7);
  statRow(L, Rr, top - 276, "ON THE ROAD", o.time);
  statRow(L, Rr, top - 302, "ERA SHIFTS", String(o.shifts));
  statRow(L, Rr, top - 328, "SAVED", o.saved);
  dashes(L - 2, Rr + 2, top - 352, FAINT, 0.7);
  barcode(x, top - 364, o.n * 7 + o.cleared);
  paint(FAINT); ptextC(2, "No. 000" + o.n + "  " + Math.min(3, o.cleared) + "/3", x, top - 408);
  if (o.cleared >= 3) stamp(x + 38, top - 384, -8, "DELIVERED", o.saved.split("  ")[0], o.n);
}
function tape(x, top, alpha) {
  paint(TAPE, 0.8 * alpha);
  R(x - 39, top - 12, 78, 24);
  for (let i = 0; i < 4; i++) { R(x - 42, top - 12 + i * 6, 3, 3); R(x + 39, top - 9 + i * 6, 3, 3); }
}
// A ticket with its shadow; `dim` darkens a ticket that is not chosen.
function ticket(o, x, top, sel, dim, alpha = 1) {
  if (alpha <= 0.01) return;
  push(); C.globalAlpha = alpha;
  if (sel) { paint(SUN); paperShape(x + 12, top - 12); } else { paint(NIGHT, 0.6); paperShape(x + 6, top - 6); }
  paint(PAPER); paperShape(x, top);
  ticketBody(o, x, top);
  if (o.tearK !== undefined) perforation(x, top - 114, o.tearK);
  if (dim > 0.01) { paint(NIGHT, 0.55 * dim); paperShape(x, top); }
  tape(x, top, 1 - 0.45 * dim);
  pop();
}
function emptyTicket(n, x, top, sel, dim) {
  const L = x - TW / 2, B = top - TH;
  const line = sel ? SUN : MUTED, a = sel ? 1 : 0.7 - 0.3 * dim;
  paint(NIGHT, 0.45); R(L, B, TW, TH);
  paint(line, a);
  for (let xx = L; xx < L + TW; xx += 12) { R(xx, top - 3, 6, 3); R(xx, B, 6, 3); }
  for (let yy = B; yy < top; yy += 12) { R(L, yy, 3, 6); R(L + TW - 3, yy, 3, 6); }
  paint(MUTED, a); ptext(2, "ORDER " + n, L + 18, top - 22); ptextR(2, "EMPTY", L + TW - 18, top - 22);
  paint(sel ? SUN : MUTED, a); R(x - 21, top - 174, 42, 6); R(x - 3, top - 192, 6, 42);
  paint(RIM, sel ? 1 : 0.55 - 0.25 * dim); ptextC(4, "NEW ORDER", x, top - 226);
  paint(MUTED, a); ptextC(2, "Starts a new save,", x, top - 278); ptextC(2, "with the opening film.", x, top - 302);
}
function perforation(x, y, k) {
  const L = x - TW / 2;
  paint(NIGHT, 0.45);
  for (let xx = L + 6; xx < L + TW - 3; xx += 9) R(xx, y - 1.5, 3, 3);
  if (k > 0) {
    paint(NIGHT);
    const end = L + TW * k;
    for (let xx = L, i = 0; xx < end; xx += 3, i++) {
      const up = 3 + 3 * Math.floor(rnd(i, 1, 5) * 2), down = 3 + 3 * Math.floor(rnd(i, 2, 5) * 2);
      R(xx, y - down, 3, up + down);
    }
  }
  // Scissors, off the paper's left edge, blades towards the tear.
  const rows = [".##.......", "#..#....##", ".##.#.##..", ".....#....", ".##.#.##..", "#..#....##", ".##......."];
  const sx = L - 42;
  rows.forEach((r, j) => { for (let i = 0; i < r.length; i++) if (r[i] === "#") { paint(NIGHT); R(sx + i * 3 - 1.5, y + 9 - j * 3 - 1.5, 6, 6); } });
  rows.forEach((r, j) => { for (let i = 0; i < r.length; i++) if (r[i] === "#") { paint(k > 0 ? SUN : RIM); R(sx + i * 3, y + 9 - j * 3, 3, 3); } });
}

// ---- The talk box (TalkBox.hs), drawn whole -------------------------------------
let portraitImg = null;
function portraitBitmap() {
  if (portraitImg) return portraitImg;
  const cv = document.createElement("canvas"); cv.width = PORTRAIT.w; cv.height = PORTRAIT.h;
  const g = cv.getContext("2d");
  for (const run of PORTRAIT.runs.split(";")) {
    const [x, y, n, c] = run.split(",");
    g.fillStyle = "#" + PORTRAIT.pal[c]; g.fillRect(+x, +y, +n, 1);
  }
  return (portraitImg = cv);
}
