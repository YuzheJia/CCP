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
const scaledShape = (k, [cx, cy], pts) => pts.map(([x, y]) => [cx + (x - cx) * k, cy + (y - cy) * k]);
const movedShape = ([dx, dy], pts) => pts.map(([x, y]) => [x + dx, y + dy]);
const BOX = [[-327, -387], [591, -369], [606, -219], [-306, -237]];
const RIMS = [[-336, -396], [600, -378], [615, -210], [-315, -228]];
const TAIL_RIM = [[-324, -252], [-321, -306], [-381, -228]];
const TAIL_INK = [[-312, -261], [-309, -297], [-363, -239]];
const TAG = [[-321, -246], [-150, -240], [-141, -192], [-312, -198]];
const SUBTAG = [[-162, -237], [-39, -233], [-33, -207], [-156, -211]];
const loud = w => { const l = w.replace(/[^A-Za-z]/g, ""); return l.length >= 2 && l === l.toUpperCase(); };
function talk(k, text, held, clock) {
  if (k <= 0.01) return;
  const slide = 1 - k;
  const px = 3, pw = PORTRAIT.w * px, ph = PORTRAIT.h * px;
  const snap = v => px * Math.round(v / px);
  blit(portraitBitmap(), -645 - snap(slide * 150), -420 + ph, pw, ph);
  const pop = Math.min(1.04, k * 1.04) - (k >= 1 ? 0.04 : 0);
  const sh = pts => scaledShape(0.72 + 0.28 * pop, [140, -300], pts);
  push(); T(0, 15); C.globalAlpha = Math.min(1, k * 1.5);
  paint(SUN); pixelPoly(movedShape([12, -12], sh(RIMS)));
  paint(RIM); pixelPoly(sh(RIMS)); pixelPoly(sh(TAIL_RIM));
  paint(INK); pixelPoly(sh(BOX)); pixelPoly(sh(TAIL_INK));
  if (k > 0.9) {
    pixelWrap(286, text).forEach((row, i) => {
      let x = -282;
      for (const word of row.split(" ")) {
        paint(loud(word) ? SUN : RIM); ptext(3, word, x, -255 - i * 36);
        x += (pixelW(word) + 4) * 3;
      }
    });
    const hint = "HOLD ENTER", hw = pixelW(hint) * 2;
    paint(held > 0 ? RIM : MUTED); ptext(2, hint, 588 - hw, -330);
    paint(0xFFFFFF, 0.18); R(588 - hw, -352, hw, 3);
    paint(SUN); R(588 - hw, -352, hw * clamp01(held / 60), 3);
  }
  paint(SUN); pixelPoly(SUBTAG); paint(INK); ptext(2, "COURIER", -129, -213);
  paint(RIM); pixelPoly(TAG); paint(INK); ptext(4, "MEI", -297, -204);
  pop();
}

// ---- Screen A: the orders ----------------------------------------------------------
const SLOT_X = [-330, 0, 330];
const SLOT_TOP = 234;
function cancelLine(o) {
  const rooms = ["no rooms yet", "one room", "two rooms"][Math.min(2, o.cleared)];
  return o.cleared >= 3
    ? `Cancel ORDER ${o.n}? It's DELIVERED, all three rooms and ${o.mins} minutes on the road. HOLD ENTER to tear it up. ESC keeps it.`
    : `Cancel ORDER ${o.n}? That's ${rooms} and ${o.mins} minutes on the road, gone for good. HOLD ENTER to tear it up. ESC keeps it.`;
}
function renderOrders(S) {
  const clock = S.t;
  titleScene(clock);
  const ck = smooth(S.confirmK);
  paint(NIGHT, 0.6 + 0.22 * ck); RC(1280, 800);
  // Header.
  const ha = 1 - ck;
  paint(0xFFF0CE, ha); stext(30, "YOUR ORDERS", 0, 316, "C");
  paint(0x98ADB1, ha); stext(13, "Three saves. Each one is kept every time a room is cleared.", 0, 286, "C");
  // Tickets: the chosen one last, so it stands over the others.
  const order = [0, 1, 2].filter(i => i !== S.sel).concat([S.sel]);
  for (const i of order) {
    const o = S.orders[i], sel = i === S.sel;
    let x = SLOT_X[i], top = SLOT_TOP + S.lift[i];
    let alpha = 1;
    if (sel) { x = lerp(ck, x, 140); top = lerp(ck, top, 291); } else alpha = 1 - 0.96 * ck;
    const dim = sel ? 0 : 1;
    if (sel && S.view === "printing") { printing(o, x, top, S.printT); continue; }
    if (!o) { push(); C.globalAlpha = alpha; emptyTicket(i + 1, x, top, sel, dim); pop(); continue; }
    if (sel && S.view === "tearing") { tearing(o, x, top, S.tearT); continue; }
    const shown = sel && S.view === "confirm" ? { ...o, tearK: clamp01(S.held / 60) } : o;
    ticket(shown, x, top, sel, dim, alpha);
  }
  if (S.view === "select") {
    const bob = Math.floor(clock / 15) % 2 === 0 ? 0 : 6;
    arrowAt(SLOT_X[S.sel], SLOT_TOP + S.lift[S.sel] + 30 + bob);
  }
  const o = S.orders[S.sel];
  const keys = o
    ? [["A / D", "Choose"], ["ENTER", "Continue"], ["DEL", "Cancel order"], ["ESC", "Title"]]
    : [["A / D", "Choose"], ["ENTER", "New order"], ["ESC", "Title"]];
  keyRow(keys, -358, ha);
  if (S.toast) toastTag(S.toast.text, -270, clamp01(S.toast.left / 12), S.toast.warn);
  if (S.confirmK > 0.01 && S.talkOrder) talk(smooth(S.confirmK), cancelLine(S.talkOrder), S.view === "tearing" ? 60 : S.held, clock);
}
function printing(o, x, top, t) {
  const p = Math.floor(clamp01(t / 70) * 30) / 30;
  const slotY = top - TH - 3;
  push(); clipAbove(slotY);
  T(0, -TH * (1 - p));
  if (p > 0) ticket(o, x, top, true, 0);
  pop();
  // The printer's mouth, under the slot.
  paint(INK); R(x - 168, slotY - 21, 336, 21);
  paint(0x2A2833); R(x - 168, slotY - 6, 336, 6);
  paint(RIM, 0.35); R(x - 156, slotY - 3, 312, 3);
  paint(SUN); R(x + 138, slotY - 15, 9, 6);
}
function tearing(o, x, top, t) {
  const f = clamp01(t / 50), y = top - 114;
  const upper = 1 - smooth((t - 30) / 20);
  push(); C.globalAlpha = upper; clipAbove(y); ticket({ ...o, tearK: 1 }, x, top, true, 0); pop();
  push(); C.globalAlpha = 1 - f; clipBelow(y);
  T(x, y - 10 * f - 700 * f * f); ROT(10 * f); T(-x, -y);
  ticket({ ...o, tearK: 1 }, x, top, true, 0);
  pop();
}

// ---- Screen A: the hall, with the order it belongs to (Lobby.hs) ----------------
function renderHall(S) {
  const clock = S.t, o = S.orders[S.hallOrder] || S.orders[0];
  paint(0x071015); RC(1280, 800);
  paint(0x111E24); R(-640, -400, 640, 800);
  paint(0x0B171C); R(0, -400, 640, 800);
  paint(0xD38A52, 0.08); P([[-640, 400], [-40, 400], [-260, -400], [-640, -400]]);
  paint(0x70A5B5, 0.07); P([[40, 400], [640, 400], [640, -400], [260, -400]]);
  paint(0xFFFFFF, 0.05); for (let i = 0; i <= 14; i++) R(-590 + i * 86, -400, 1, 800);
  paint(0xFFF0CE); stext(42, "BORROWED LIGHT", 0, 306, "C");
  paint(0x8FB4BF); stext(15, "LEVEL HALL", 0, 267, "C");
  paint(0xC8D7DA); stext(12, "Choose a lesson. Cleared rooms stay unlocked.", 0, 235, "C");
  const completed = Math.min(3, o.cleared), unlocked = Math.min(2, completed), selected = S.hallSel;
  const pulse = 0.7 + 0.3 * Math.sin(clock * 0.06);
  for (let i = 0; i < 3; i++) {
    push(); T(0, 145 - 145 * i);
    const locked = i > unlocked;
    paint(i === selected ? 0xFFD481 : 0x31444B, i === selected ? pulse : 1); R(-475, -52, 950, 112);
    paint(i === selected ? 0x162329 : 0x10191D); R(-469, -46, 938, 100);
    paint(locked ? 0x53666B : i === selected ? 0xFFD481 : 0xA9C4C9); stext(31, String(i + 1), -420, -9, "C");
    paint(locked ? 0x64767A : 0xEFF3EE); stext(22, HALL_NAMES[i], -350, 16);
    paint(locked ? 0x4C5E62 : 0x98ADB1); stext(12, LESSONS[i], -350, -19);
    const st = i < completed ? ["CLEARED", 0x8FD6A6] : locked ? ["LOCKED", 0x5D6A6D] : ["AVAILABLE", 0xE6BC70];
    paint(st[1]); stext(12, st[0], 418, 7, "R");
    pop();
  }
  paint(0xDCE6E7); stext(13, "W S OR ARROWS   SELECT       ENTER   PLAY", 0, -326, "C");
  paint(0x89A4AA); stext(11, "ESC YOUR ORDERS       SAVED EVERY TIME A ROOM IS CLEARED", 0, -355, "C");
  // Whose order this is, pinned top left.
  const tagText = "ORDER " + o.n, tw = pixelW(tagText) * 2 + 24;
  paint(SUN); R(-610 + 6, 346 - 6, tw, 32);
  paint(RIM); R(-610, 346, tw, 32);
  paint(INK); ptext(2, tagText, -598, 369);
  paint(0x98ADB1); stext(11, "Mei's delivery", -610 + tw + 12, 356);
  // The saved slip, printed down from the top edge, then taken back.
  const t = S.hallT;
  const k = t < 14 ? smooth(t / 14) : t < 170 ? 1 : 1 - smooth((t - 170) / 14);
  if (k > 0.01) savedSlip(o, 470, 400 + 108 - 108 * k, S.saveFailed);
}
function savedSlip(o, x, top, failed) {
  const w = 300, h = 108, L = x - w / 2, n = 20, tw = w / n;
  paint(NIGHT, 0.6); R(L + 6, top - h + 6 - 6, w, h - 6);
  paint(failed ? 0xF3D9D2 : PAPER); R(L, top - h + 6, w, h - 6);
  for (let i = 0; i < n; i++) pixelPoly([[L + i * tw, top - h + 6], [L + (i + 0.5) * tw, top - h], [L + (i + 1) * tw, top - h + 6]]);
  if (failed) {
    paint(STAMP); R(L + 14, top - 52, 24, 24);
    paint(PAPER); R(L + 24, top - 46, 4, 4); R(L + 24, top - 42 + 6, 4, 10);
    paint(STAMP); ptext(2, "COULD NOT SAVE", L + 48, top - 30);
    paint(FAINT); ptext(2, "Will keep trying.", L + 48, top - 54);
  } else {
    paint(INK); R(L + 14, top - 52, 24, 24); check(L + 26, top - 40, SUN);
    paint(INK); ptext(2, "ORDER " + o.n + " SAVED", L + 48, top - 30);
    paint(FAINT); ptext(2, Math.min(3, o.cleared) + " OF 3 ROOMS  " + o.time, L + 48, top - 54);
  }
  dashes(L + 14, L + w - 14, top - 76, FAINT, 0.5);
  paint(FAINT); ptext(2, "CAPE COURIERS", L + 14, top - 82);
}

// ---- Screen B: the keeper's log ---------------------------------------------------
const PLATE_X = [-392, 0, 392], PLATE_Y = -276, PW = 352, PH = 112;
function plateTarget(i) { return -Math.atan2(PLATE_Y - 180, PLATE_X[i]) * 180 / Math.PI; }
function renderLog(B) {
  const clock = B.t;
  layer("titleBack", titleBackdrop);
  paint(NIGHT, 0.18); RC(1280, 800);
  beam(B.beamDeg, 1.5);
  layer("titleFront", () => { headland(); tower(); });
  const o = B.orders[B.sel];
  const cleared = o ? o.cleared : 0;
  // A window lights for each room cleared, bottom first; the present's lamp once delivered.
  [-72, -12, 48].forEach((y, k) => {
    if (k < cleared) {
      paint(0xFFE2A8, 0.22); push(); T(0, y); ell(20, 20, 20); pop();
      paint(0xFFE2A8); R(-5, y - 8, 10, 16); paint(0xFFFFFF); R(-2, y - 4, 4, 8);
    } else { paint(0x1A1210); R(-5, y - 8, 10, 16); paint(0x3A2A22); R(-5, y - 8, 10, 3); }
  });
  if (cleared >= 3) { paint(0xFFEFC4); R(0, 158, 24, 44); paint(0xFFFFFF); R(0, 164, 14, 32); paint(0xFFE2A8, 0.2); push(); T(0, 180); ell(24, 60, 60); pop(); }
  rain(clock);
  paint(0xF7DDB2); stext(15, "PAST", -600, 356);
  paint(0xA9B9BF); stext(15, "PRESENT", 600, 356, "R");
  paint(0xFFF3D6); stext(28, "KEEPER'S LOG", 0, 330, "C");
  paint(0xC8D7DA); stext(12, "Choose a save. The tower shows how far it got.", 0, 300, "C");
  B.orders.forEach((p, i) => plate(p, i, i === B.sel));
  keyRow(o ? [["A / D", "Choose"], ["ENTER", "Continue"], ["DEL", "Clear log"], ["ESC", "Title"]]
           : [["A / D", "Choose"], ["ENTER", "New game"], ["ESC", "Title"]], -370);
}
function plate(o, i, sel) {
  const x = PLATE_X[i], L = x - PW / 2, top = PLATE_Y + PH / 2, B = PLATE_Y - PH / 2;
  if (sel) { paint(SUN, 0.1); push(); T(x, PLATE_Y); ell(28, PW * 0.7, PH * 0.95); pop(); }
  paint(NIGHT, 0.6); R(L + 7, B - 7, PW, PH);
  if (!o) {
    paint(0x141C20); R(L, B, PW, PH);
    paint(sel ? SUN : 0x4F5B60);
    for (let xx = L; xx < L + PW; xx += 12) { R(xx, top - 3, 6, 3); R(xx, B, 6, 3); }
    for (let yy = B; yy < top; yy += 12) { R(L, yy, 3, 6); R(L + PW - 3, yy, 3, 6); }
    paint(sel ? 0xFFF3D6 : 0x8A9AA0); stext(20, "LOG " + (i + 1), L + 22, top - 38, "L", 2.6);
    paint(0x8A9AA0); stext(12, "EMPTY", L + PW - 20, top - 34, "R");
    paint(sel ? 0xE8F0F2 : 0x8A9AA0); stext(15, "Start a new game", L + 22, top - 66);
    paint(0x6F8187); stext(11, "The opening film plays first", L + 22, top - 92);
    return;
  }
  const tarnish = c => (sel ? c : mixHex(0.6, c, 0x2B2A27));
  paint(0x2A1A12); R(L - 4, B - 4, PW + 8, PH + 8);
  bands(L, PW, [0xA97C38, 0xC89C50, 0xDDB766, 0xE9CB82].map(tarnish), B, top);
  paint(tarnish(0x7A5A2E)); R(L + 8, B + 8, PW - 16, 2); R(L + 8, top - 10, PW - 16, 2); R(L + 8, B + 8, 2, PH - 16); R(L + PW - 10, B + 8, 2, PH - 16);
  paint(tarnish(0x5A4128)); [[L + 18, B + 18], [L + PW - 18, B + 18], [L + 18, top - 18], [L + PW - 18, top - 18]].forEach(([sx, sy]) => { push(); T(sx, sy); ell(10, 4, 4); pop(); });
  const ink = sel ? 0x2A1A12 : 0x1E1A16, soft = sel ? 0x5A4128 : 0x3A342E;
  paint(ink); stext(20, "LOG " + (i + 1), L + 30, top - 40, "L", 2.6);
  paint(ink); stext(12, o.cleared >= 3 ? "LAMP LIT" : o.cleared + " OF 3 ROOMS", L + PW - 30, top - 36, "R");
  paint(ink); stext(15, o.cleared >= 3 ? "Delivered" : HALL_NAMES[o.cleared], L + 30, top - 68);
  paint(soft); stext(11, o.time + " on watch     " + o.saved.replace("NOV", "Nov"), L + 30, top - 92);
}

// ---- Prototype state ---------------------------------------------------------------
const ORDERS = () => [
  { n: 1, cleared: 2, time: "0:14:32", shifts: 212, saved: "8 NOV  21:14", secret: false, mins: 14 },
  { n: 2, cleared: 3, time: "0:31:08", shifts: 405, saved: "7 NOV  23:52", secret: true, mins: 31 },
  null,
];
function freshA() {
  return { view: "select", sel: 0, orders: ORDERS(), t: 600, lift: [15, 0, 0], confirmK: 0, held: 0, holding: false,
           tearT: 0, printT: 0, hallOrder: 0, hallSel: 2, hallT: 0, toast: null, talkOrder: null, saveFailed: false };
}
function presetA(name) {
  const S = freshA();
  if (name === "confirm") { S.view = "confirm"; S.confirmK = 1; S.held = 27; S.talkOrder = S.orders[0]; }
  if (name === "print") { S.sel = 2; S.lift = [0, 0, 15]; S.view = "printing"; S.printT = 44; S.orders[2] = newOrder(3); }
  if (name === "hall") { S.view = "hall"; S.hallT = 60; }
  if (name === "hallfail") { S.view = "hall"; S.hallT = 60; S.saveFailed = true; }
  return S;
}
function newOrder(n) { return { n, cleared: 0, time: "0:00:00", shifts: 0, saved: "8 NOV  22:05", secret: false, mins: 0 }; }
function stepA(S) {
  S.t++;
  for (let i = 0; i < 3; i++) S.lift[i] += ((i === S.sel ? 15 : 0) - S.lift[i]) * 0.3;
  const wantConfirm = S.view === "confirm" || S.view === "tearing";
  S.confirmK = clamp01(S.confirmK + (wantConfirm ? 1 : -1) / 14);
  if (S.view === "confirm") {
    S.held = S.holding ? Math.min(60, S.held + 1) : Math.max(0, S.held - 4);
    if (S.held >= 60) { S.view = "tearing"; S.tearT = 0; }
  } else if (S.view === "tearing") {
    if (++S.tearT >= 50) {
      const n = S.orders[S.sel].n; S.orders[S.sel] = null; S.view = "select"; S.held = 0; S.holding = false;
      S.toast = { text: "ORDER " + n + " CANCELLED", left: 120, warn: true };
    }
  } else if (S.view === "printing") {
    if (++S.printT >= 70) { S.view = "select"; S.toast = { text: "NEW ORDER TAKEN. THE OPENING FILM PLAYS NEXT.", left: 150 }; }
  } else if (S.view === "hall") S.hallT++;
  if (S.toast && --S.toast.left <= 0) S.toast = null;
}
function keyA(S, key, down) {
  const v = S.view;
  const left = ["ArrowLeft", "a", "A"].includes(key), right = ["ArrowRight", "d", "D"].includes(key);
  const enter = key === "Enter" || key === " ", del = key === "Delete" || key === "Backspace", esc = key === "Escape";
  if (!down) { if (enter) S.holding = false; return false; }
  if (v === "select") {
    if (left || right) { S.sel = Math.max(0, Math.min(2, S.sel + (left ? -1 : 1))); return true; }
    if (enter) {
      if (!S.orders[S.sel]) { S.orders[S.sel] = newOrder(S.sel + 1); S.view = "printing"; S.printT = 0; }
      else { S.view = "hall"; S.hallOrder = S.sel; S.hallT = 0; S.hallSel = Math.min(2, S.orders[S.sel].cleared); }
      return true;
    }
    if (del && S.orders[S.sel]) { S.view = "confirm"; S.held = 0; S.holding = false; S.talkOrder = S.orders[S.sel]; return true; }
    if (esc) { S.toast = { text: "ESC GOES BACK TO THE TITLE", left: 100 }; return true; }
  } else if (v === "confirm") {
    if (enter) { S.holding = true; return true; }
    if (esc) { S.view = "select"; S.held = 0; S.holding = false; return true; }
  } else if (v === "hall") {
    const o = S.orders[S.hallOrder], unlocked = Math.min(2, o.cleared);
    if (["ArrowUp", "w", "W"].includes(key)) { S.hallSel = Math.max(0, S.hallSel - 1); return true; }
    if (["ArrowDown", "s", "S"].includes(key)) { S.hallSel = Math.min(unlocked, S.hallSel + 1); return true; }
    if (esc) { S.view = "select"; return true; }
    if (enter) return true;
  }
  return enter || left || right || del || esc;
}
function renderA(S) { if (S.view === "hall") renderHall(S); else renderOrders(S); }

function freshB() { return { sel: 0, orders: ORDERS(), t: 600, beamDeg: plateTarget(0) }; }
function stepB(B) { B.t++; B.beamDeg += (plateTarget(B.sel) - B.beamDeg) * 0.12; }
function keyB(B, key, down) {
  if (!down) return false;
  if (["ArrowLeft", "a", "A"].includes(key)) { B.sel = Math.max(0, B.sel - 1); return true; }
  if (["ArrowRight", "d", "D"].includes(key)) { B.sel = Math.min(2, B.sel + 1); return true; }
  return false;
}

// ---- Mounting ----------------------------------------------------------------------
function sizeCanvas(cv, fixed) {
  const w = fixed ? 1280 : Math.min(2560, Math.round(cv.clientWidth * (window.devicePixelRatio || 1)));
  const h = Math.round(w * 800 / 1280);
  if (cv.width !== w || cv.height !== h) { cv.width = w; cv.height = h; }
  return [w, h];
}
function drawOn(cv, fixed, fn) {
  const [w, h] = sizeCanvas(cv, fixed);
  C = cv.getContext("2d");
  SCALE = w / 1280; DEV_W = w; DEV_H = h;
  C.setTransform(1, 0, 0, 1, 0, 0); C.globalAlpha = 1;
  C.fillStyle = "#000"; C.fillRect(0, 0, w, h);
  C.setTransform(SCALE, 0, 0, -SCALE, w / 2, h / 2);
  fn();
}
window.SaveScreen = { freshA, presetA, stepA, keyA, renderA, freshB, stepB, keyB, renderLog, drawOn };
