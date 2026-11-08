// The two walkers, redrawn. Written in the same small vocabulary as app/Draw.hs
// (y up, units of the board, polygons only) so each shape ports to Figures.hs
// line for line: pics = Pictures, col = Color, tr = Translate, sc = Scale,
// rt = Rotate (clockwise degrees), poly = Polygon, rect/ellipse/ngon/seg/segs as Draw.

// ---- vocabulary ---------------------------------------------------------------

const hexS = (v, a = 1) => `rgba(${(v >> 16) & 255},${(v >> 8) & 255},${v & 255},${a})`;
const pics = (...ps) => g => { for (const p of ps.flat(Infinity)) if (p) p(g); };
const col = (c, ...ps) => g => { const o = g.c; g.c = typeof c === 'number' ? hexS(c) : c; pics(...ps)(g); g.c = o; };
const tr = (x, y, ...ps) => g => { g.x.save(); g.x.translate(x, y); pics(...ps)(g); g.x.restore(); };
const sc = (sx, sy, ...ps) => g => { g.x.save(); g.x.scale(sx, sy); pics(...ps)(g); g.x.restore(); };
const rt = (deg, ...ps) => g => { g.x.save(); g.x.rotate(-deg * Math.PI / 180); pics(...ps)(g); g.x.restore(); };
const poly = pts => g => {
  const x = g.x; x.beginPath(); x.moveTo(pts[0][0], pts[0][1]);
  for (let i = 1; i < pts.length; i++) x.lineTo(pts[i][0], pts[i][1]);
  x.closePath(); x.fillStyle = g.c; x.fill();
};
const rect = (x, y, w, h) => poly([[x, y], [x + w, y], [x + w, y + h], [x, y + h]]);
const ellipse = (n, rx, ry) => poly([...Array(n)].map((_, i) => {
  const a = 2 * Math.PI * i / n; return [rx * Math.cos(a), ry * Math.sin(a)];
}));
const ngon = (n, r) => ellipse(n, r, r);
const seg = (w, [x1, y1], [x2, y2]) => {
  const dx = x2 - x1, dy = y2 - y1, len = Math.hypot(dx, dy);
  if (len < 1e-4) return tr(x1, y1, rect(-w / 2, -w / 2, w, w));
  const ux = dx / len * w / 2, uy = dy / len * w / 2, nx = -uy, ny = ux;
  const ax = x1 - ux, ay = y1 - uy, bx = x2 + ux, by = y2 + uy;
  return poly([[ax + nx, ay + ny], [bx + nx, by + ny], [bx - nx, by - ny], [ax - nx, ay - ny]]);
};
const segs = (w, ps) => pics(ps.slice(1).map((p, i) => seg(w, ps[i], p)));
const closed = ps => [...ps, ps[0]];
// A band between two radii, as Draw.ring with sy = 1.
const ring = (n, r, w) => pics([...Array(n)].map((_, i) => {
  const a = 2 * Math.PI * i / n, b = 2 * Math.PI * (i + 1) / n, p = (rad, t) => [rad * Math.cos(t), rad * Math.sin(t)];
  return poly([p(r, a), p(r + w, a), p(r + w, b), p(r, b)]);
}));
const flipX = ps => ps.map(([x, y]) => [-x, y]).reverse();
// A filled shape with the house outline round it: outline first, fill on top, so
// half the stroke shows outside, as the hood and coat do now.
const OUT = 0x17100B;
const blob = (fill, pts, w = 3) => pics(col(OUT, segs(w, closed(pts))), col(fill, poly(pts)));
const shadowUnder = col(hexS(0, 0.45), tr(0, -20, ellipse(16, 17, 7)));

// The walk: the same bob and stride the oilskin keeper has, plus hair that lags.
function gait(phase) {
  const s = Math.sin(phase * 2 * Math.PI);
  const bob = 2.2 * Math.abs(Math.sin(phase * Math.PI));
  return { bob, stride: s * 3, sway: s * 1.4, drop: -0.5 * bob, trail: Math.abs(s) * 1.6 };
}

function legsOf(boots, a, b) {
  return pics(
    col(OUT, rect(-9, -21 + a, 8, 10), rect(1, -21 + b, 8, 10)),
    col(boots, rect(-8, -20 + a, 6, 8), rect(2, -20 + b, 6, 8)));
}

const TRUNK_OUT = [[-16, -14], [16, -14], [12, 12], [-12, 12]];
const TRUNK = [[-14, -12], [14, -12], [10.5, 11], [-10.5, 11]];
const TRUNK_SHADE = [[3, -12], [14, -12], [10.5, 11], [2, 11]];
const trunkOf = (main, shade) => pics(col(OUT, poly(TRUNK_OUT)), col(main, poly(TRUNK)), col(shade, poly(TRUNK_SHADE)));

const facingOf = (dir, front, back, side) =>
  dir === 'down' ? front : dir === 'up' ? back : dir === 'left' ? side : sc(-1, 1, side);

// ---- the courier --------------------------------------------------------------
// Yellow hi-vis delivery jacket, white hair in two tails with black bows, an
// ahoge, a black insulated box on her back with a steaming bowl on it.

const CO = {
  coat: 0xF7C81E, coatShade: 0xC4920F,
  stripe: 0xEAE8E2, stripeShade: 0xB9B5AC,
  dark: 0x2C2B33, darkHi: 0x4C4A56, buckle: 0x9C9AA4, icon: 0xA9A7B2,
  lid: 0xF2BC1A, lidShade: 0xB98A10,
  hair: 0xF2EFF7, hairShade: 0xCDC5DD, hairDeep: 0x9C92B4,
  ribbon: 0x1D1A21, ribbonHi: 0x56505F,
  skin: 0xF3D5BB, eye: 0x7B3438, mouth: 0xC98E7E,
  boots: 0x2A2530,
};

// One tail, the left one seen from the front: from under the bow, out over the
// shoulder and down past it. s sways the tip across, d drops it.
const tailFront = (s, d) => [
  [-10, 25], [-14.2, 25], [-17.4, 20.8], [-18.6, 14], [-18, 6],
  [-16.8 + s * 0.6, -1 + d * 0.5], [-15.2 + s, -7 + d], [-13.6 + s * 0.6, -2.4 + d * 0.5],
  [-12.8, 5], [-12.2, 13], [-10.4, 19.6]];
const tailFrontShade = (s, d) => [
  [-12.6, 19], [-14.8, 15], [-15.2, 8], [-14.8 + s * 0.6, 0 + d * 0.5], [-15 + s, -5.4 + d],
  [-13.8 + s * 0.6, -1.6 + d * 0.5], [-13.2, 6], [-12.6, 13]];

const bow = (x, y, tilt) => tr(x, y, rt(tilt,
  col(CO.ribbon, poly([[0, 0], [-4.6, 3], [-4.6, -3]]), poly([[0, 0], [4.6, 3], [4.6, -3]]),
      poly([[-0.6, 0], [-2.4, -5], [-0.6, -4.2]]), poly([[0.6, 0], [2.4, -5], [0.6, -4.2]])),
  col(CO.ribbonHi, poly([[-1.4, 0.4], [-3.8, 2], [-3.8, 0.6]]), poly([[1.4, 0.4], [3.8, 2], [3.8, 0.6]])),
  col(OUT, ngon(8, 1.7))));

const ahoge = (x, tilt) => tr(x, 28.4, rt(tilt, tr(0, -28.4,
  col(OUT, segs(3.4, [[0.4, 28.4], [0.9, 32.4], [3.1, 35.2], [5.9, 35.1], [7, 32.7], [5.5, 31.1]])),
  col(CO.hair, segs(1.5, [[0.4, 28.4], [0.9, 32.4], [3.1, 35.2], [5.9, 35.1], [7, 32.7], [5.5, 31.1]])))));

function courier(dir, phase) {
  const { bob, stride, sway, drop, trail } = gait(phase);
  const hairCap = (cx) => pics(
    col(CO.hairShade, tr(cx, 17, ngon(20, 12.6))),
    col(CO.hair, tr(cx - 1.3, 17.8, ngon(20, 11.4))));
  const capOutline = (cx) => col(OUT, tr(cx, 17, ngon(20, 14.2)));
  const tails = (s, d) => {
    const l = tailFront(s, d), r = flipX(tailFront(-s, d));
    return pics(
      col(OUT, segs(3, closed(l)), segs(3, closed(r))),
      col(CO.hair, poly(l)), col(CO.hairShade, poly(r)),
      col(CO.hairShade, poly(tailFrontShade(s, d))), col(CO.hairDeep, poly(flipX(tailFrontShade(-s, d)))));
  };

  const front = pics(
    legsOf(CO.boots, Math.max(0, stride), Math.max(0, -stride)),
    tails(sway, drop),
    trunkOf(CO.coat, CO.coatShade),
    // the hi-vis band, shaded with the coat
    col(CO.stripe, poly([[-13.1, -6.4], [13.1, -6.4], [12.7, -3.2], [-12.7, -3.2]])),
    col(CO.stripeShade, poly([[2.6, -6.4], [13.1, -6.4], [12.7, -3.2], [2.4, -3.2]])),
    col(OUT, rect(-0.8, -12, 1.6, 22)),
    col(CO.dark, rect(-3.6, 3.4, 2, 2), rect(-3.6, -1.2, 2, 2)),
    // the box's straps over her shoulders
    col(CO.dark, poly([[-9.6, 11], [-6, 11], [-6.8, -1.4], [-10.4, -1.4]]),
                 poly([[6, 11], [9.6, 11], [10.4, -1.4], [6.8, -1.4]])),
    col(CO.buckle, rect(-9.8, 1.4, 3, 1.6), rect(6.8, 1.4, 3, 1.6)),
    capOutline(0), hairCap(0),
    // the black collar with its buckle, under the chin
    col(CO.dark, rect(-5.4, 3.2, 10.8, 3.4)), col(CO.buckle, rect(-1.4, 4, 2.8, 1.8)),
    col(OUT, tr(0, 12, ellipse(16, 8.9, 7.7))),
    col(CO.skin, tr(0, 12, ellipse(16, 7.6, 6.4))),
    // fringe
    col(CO.hair, poly([[-8.8, 19.4], [-8.5, 14.2], [-6.5, 15.6], [-4.6, 13.6], [-2.6, 15.8], [-0.4, 13.4],
                       [1.8, 15.6], [3.9, 13.8], [5.9, 15.8], [7.9, 14.2], [8.8, 19.4]])),
    col(CO.hairDeep, segs(1.1, [[-8.5, 14.2], [-6.5, 15.6], [-4.6, 13.6], [-2.6, 15.8], [-0.4, 13.4],
                               [1.8, 15.6], [3.9, 13.8], [5.9, 15.8], [7.9, 14.2]])),
    // locks framing the face
    col(CO.hair, poly([[-9.4, 17], [-7.2, 16], [-6.9, 9], [-7.6, 4.6], [-9.8, 6.4], [-10.4, 11]]),
                 poly([[9.4, 17], [7.2, 16], [6.9, 9], [7.6, 4.6], [9.8, 6.4], [10.4, 11]])),
    col(OUT, segs(1.3, [[-7.2, 16], [-6.9, 9], [-7.6, 4.6], [-9.8, 6.4]]),
             segs(1.3, [[7.2, 16], [6.9, 9], [7.6, 4.6], [9.8, 6.4]])),
    col(CO.eye, rect(-4.7, 9.4, 2.3, 3.2), rect(2.4, 9.4, 2.3, 3.2)),
    col(CO.mouth, rect(-0.7, 7.1, 1.4, 0.8)),
    bow(-11.8, 22.6, 20), bow(11.8, 22.6, -20),
    ahoge(0.4, -sway * 4));

  const back = pics(
    legsOf(CO.boots, Math.max(0, stride), Math.max(0, -stride)),
    trunkOf(CO.coat, CO.coatShade),
    col(CO.stripe, poly([[-13.1, -6.4], [-2, -6.4], [-2, -3.2], [-12.7, -3.2]])),
    col(CO.stripeShade, poly([[-2, -6.4], [13.1, -6.4], [12.7, -3.2], [-2, -3.2]])),
    // the box
    col(OUT, rect(-13.6, -10, 27.2, 24.6)),
    col(CO.dark, rect(-12, -8.4, 24, 21.4)),
    col(CO.darkHi, rect(-12, -8.4, 1.6, 21.4)),
    col(CO.lid, rect(-12, 9.4, 24, 3.6)), col(CO.lidShade, rect(3, 9.4, 9, 3.6)),
    col(OUT, rect(-12, 8.6, 24, 0.9)),
    col(CO.icon, rect(-6, -3.4, 12, 1.3),
        poly([...Array(9)].map((_, i) => { const a = Math.PI + Math.PI * i / 8; return [5.2 * Math.cos(a), -3.4 + 3.6 * Math.sin(a)]; })),
        segs(1, [[-2.6, -1], [-1.8, 0.8], [-2.6, 2.6]]), segs(1, [[0, -1], [0.8, 0.8], [0, 2.6]]),
        segs(1, [[2.6, -1], [3.4, 0.8], [2.6, 2.6]])),
    tails(sway, drop),
    // hair down the back, over the top of the box
    col(OUT, tr(0, 17, ngon(20, 14.2)),
        segs(3, [[-12.2, 12], [-12, 9.6], [-9.8, 7.4], [-7.2, 9], [-4.6, 6.2], [-1.4, 8.2], [1.6, 5.8],
                 [4.6, 8.4], [7.4, 6.4], [10, 8.6], [12, 9.8], [12.2, 12]])),
    col(CO.hair, tr(0, 17, ngon(20, 12.6)),
        poly([[-12.3, 17], [-12.2, 12], [-12, 9.6], [-9.8, 7.4], [-7.2, 9], [-4.6, 6.2], [-1.4, 8.2], [1.6, 5.8],
              [4.6, 8.4], [7.4, 6.4], [10, 8.6], [12, 9.8], [12.2, 12], [12.3, 17]])),
    col(CO.hairShade, poly([[4.6, 8.4], [7.4, 6.4], [10, 8.6], [12, 9.8], [12.2, 12], [12.3, 17], [11.6, 21.4],
                            [9.6, 24.6], [7.6, 20], [6.4, 13]])),
    col(CO.hairDeep, segs(1, [[0, 29.4], [0, 19.4]])),
    bow(-11.8, 22.6, 20), bow(11.8, 22.6, -20),
    ahoge(0.4, -sway * 4));

  // Facing left (-x). The box rides on her back, at +x; the near tail hangs from
  // the back of her head over it, the far one shows just behind.
  const tailSide = (dx, dy) => {
    const s = trail + sway * 0.4;
    return [[4 + dx, 25.4 + dy], [8.8 + dx, 25.6 + dy], [12 + dx, 21.6 + dy], [13.2 + dx, 15 + dy], [12.8 + dx, 7 + dy],
            [11.8 + dx + s * 0.6, 1 + dy + drop * 0.5], [10.4 + dx + s, -4.6 + dy + drop], [8.8 + dx + s * 0.6, 0.4 + dy + drop * 0.5],
            [8.4 + dx, 8 + dy], [7.8 + dx, 15 + dy], [5.4 + dx, 20.6 + dy]];
  };
  const side = pics(
    legsOf(CO.boots, stride, -stride),
    // box, standing proud of her back
    col(OUT, rect(3.4, -10, 14, 24.4)),
    col(CO.dark, rect(5, -8.4, 10.8, 21.2)),
    col(CO.darkHi, rect(5, -8.4, 1.6, 21.2)),
    col(CO.lid, rect(5, 9.2, 10.8, 3.6)), col(CO.lidShade, rect(11.6, 9.2, 4.2, 3.6)),
    col(OUT, rect(5, 8.4, 10.8, 0.9)),
    col(OUT, poly([[-16, -14], [5.4, -14], [5.4, 12], [-12, 12]])),
    col(CO.coat, poly([[-14, -12], [4, -12], [4, 11], [-10.5, 11]])),
    col(CO.stripe, poly([[-13.1, -6.4], [4, -6.4], [4, -3.2], [-12.7, -3.2]])),
    // the arm, with the band round the sleeve
    col(OUT, rect(-4.4, -10, 9, 15.6)),
    col(CO.coatShade, rect(-3, -8.6, 6.2, 12.8)),
    col(CO.stripeShade, rect(-3, -5.6, 6.2, 2.4)),
    col(CO.dark, poly([[-1, 11], [3, 11], [3, 4.4], [-1, 4.4]])),
    // far tail, behind
    blob(CO.hairDeep, tailSide(2.6, 1.6), 3),
    capOutline(1.4), hairCap(1.4),
    col(CO.dark, rect(-9.8, 3.2, 9, 3.4)),
    col(OUT, tr(-5.4, 12.2, ngon(16, 6.9))),
    col(CO.skin, tr(-6.4, 12.2, ngon(16, 5.6))),
    col(CO.hair, poly([[-12.8, 15.6], [-12.2, 14.2], [-10.4, 15.2], [-8.6, 13.6], [-6.4, 15.4], [-3.8, 15.6], [-3.4, 19.6], [-12, 19.6]])),
    col(CO.hairDeep, segs(1.1, [[-12.2, 14.2], [-10.4, 15.2], [-8.6, 13.6], [-6.4, 15.4], [-3.8, 15.6]])),
    col(CO.eye, rect(-10.6, 9.4, 2.2, 3.2)),
    // the lock in front of the ear
    col(CO.hair, poly([[-2.2, 17.4], [0.8, 16.8], [0.6, 9], [-0.6, 5.6], [-2.2, 7.6]])),
    col(OUT, segs(1.3, [[0.8, 16.8], [0.6, 9], [-0.6, 5.6], [-2.2, 7.6], [-2.2, 17.4]])),
    // near tail
    pics(col(OUT, segs(3, closed(tailSide(0, 0)))), col(CO.hair, poly(tailSide(0, 0)))),
    col(CO.hairShade, poly([[8.6, 7], [11.2, 12], [11.6, 5], [10.4 + trail + sway * 0.4, -3 + drop], [9.2, 1]])),
    bow(6.4, 23.4, -10),
    ahoge(0, -sway * 4 + trail * 3));

  return sc(1.08, 1.08, shadowUnder, tr(0, bob, facingOf(dir, front, back, side)));
}

// ---- the keeper ---------------------------------------------------------------
// Long black hair, a brown field jacket worn open over a black shirt, pale piping
// on its edges, flap pockets, a brass patch on the sleeve.

const KE = {
  jacket: 0x75523F, jacketShade: 0x523829, jacketDeep: 0x3E2A20, piping: 0xC9B9A6,
  shirt: 0x24212A, brass: 0xC29A45,
  hair: 0x2B2633, hairDeep: 0x18151D, sheen: 0x5D5674, sheenHi: 0x8A84A6,
  skin: 0xF0D0B4, eye: 0x4A4048, mouth: 0xBE8A7C,
  boots: 0x1F1A17,
};

function keeper(dir, phase) {
  const { bob, stride, sway, drop, trail } = gait(phase);
  const s = sway, d = drop;

  const jacketFront = pics(
    trunkOf(KE.jacket, KE.jacketShade),
    col(KE.shirt, poly([[-4.4, 11], [4.4, 11], [3.6, -12], [-3.6, -12]])),
    col(KE.piping, segs(1.1, [[-4.4, 11], [-3.6, -12]]), segs(1.1, [[4.4, 11], [3.6, -12]])),
    col(KE.jacketShade, rect(-11.6, -4.6, 5.8, 5)), col(KE.jacketDeep, rect(5.8, -4.6, 5.6, 5)),
    col(KE.piping, rect(-11.8, 0.2, 6.2, 1), rect(5.6, 0.2, 6.2, 1)),
    col(KE.brass, rect(-14.4, -9.6, 3.8, 1.7)));

  const backHair = [[-12.4, 22], [12.4, 22], [15.4, 14], [16.2, 3], [15 + s, -7 + d], [11, -6.2],
                    [-11, -6.2], [-15 + s, -7 + d], [-16.2, 3], [-15.4, 14]];
  const lockL = [[-10.4, 18], [-7.6, 16], [-7.4, 8], [-7.4 + s * 0.4, -1 + d * 0.5], [-8.8 + s * 0.7, -3.8 + d],
                 [-10.2 + s * 0.3, -0.6 + d * 0.5], [-10.8, 8]];
  // the right lock is the left one mirrored, swaying the same way across
  const lockRs = flipX([[-10.4, 18], [-7.6, 16], [-7.4, 8], [-7.4 - s * 0.4, -1 + d * 0.5], [-8.8 - s * 0.7, -3.8 + d],
                        [-10.2 - s * 0.3, -0.6 + d * 0.5], [-10.8, 8]]);

  const front = pics(
    blob(KE.hairDeep, backHair, 3),
    legsOf(KE.boots, Math.max(0, stride), Math.max(0, -stride)),
    jacketFront,
    col(OUT, tr(0, 17, ngon(20, 14.2))),
    col(KE.hair, tr(0, 17, ngon(20, 12.6))),
    col(KE.sheen, poly([[-9.6, 23], [-5.8, 26.6], [-0.6, 27.9], [3.2, 27.5], [-0.8, 25.6], [-5.4, 24.4], [-8.8, 21]]),
                  poly([[4.8, 26.6], [8, 24.6], [9, 22.6], [6.2, 24.4]])),
    col(KE.sheenHi, poly([[-6.6, 25.4], [-3, 27], [-1.6, 26.4], [-4.6, 25.2]])),
    col(KE.shirt, rect(-5.4, 3.2, 10.8, 3.4)),
    col(OUT, tr(0, 12, ellipse(16, 8.9, 7.7))),
    col(KE.skin, tr(0, 12, ellipse(16, 7.6, 6.4))),
    col(KE.hair, poly([[-8.9, 19.4], [-8.7, 12.6], [-6.7, 14.6], [-5.2, 12], [-3, 14.2], [-1.2, 11.6], [0.6, 14.6],
                       [2.6, 12.4], [4.6, 14.6], [6.4, 12.2], [8.3, 13.8], [8.9, 19.4]])),
    col(OUT, segs(1.1, [[-8.7, 12.6], [-6.7, 14.6], [-5.2, 12], [-3, 14.2], [-1.2, 11.6], [0.6, 14.6],
                        [2.6, 12.4], [4.6, 14.6], [6.4, 12.2], [8.3, 13.8]])),
    col(KE.eye, rect(-4.9, 9.2, 2.7, 2.1), rect(2.2, 9.2, 2.7, 2.1)),
    col(OUT, rect(-5.1, 11.1, 3.1, 0.9), rect(2, 11.1, 3.1, 0.9)),
    col(KE.mouth, rect(-0.7, 7, 1.4, 0.7)),
    blob(KE.hair, lockL, 2.4), blob(KE.hair, lockRs, 2.4),
    col(KE.sheen, segs(1, [[-9, 14], [-8.9, 4]]), segs(1, [[9, 14], [8.9, 4]])));

  const mass = [[-12.6, 18], [12.6, 18], [15, 12], [15.4, 2], [14 + s, -7.6 + d], [9.6, -5.6], [5.6, -8],
                [1.2, -5.8], [-3.4, -8], [-7.6, -5.8], [-12 + s, -7.6 + d], [-15.4, 2], [-15, 12]];
  const back = pics(
    legsOf(KE.boots, Math.max(0, stride), Math.max(0, -stride)),
    trunkOf(KE.jacket, KE.jacketShade),
    col(KE.jacketDeep, rect(-11, 5, 22, 1)),
    col(KE.brass, rect(10.6, -9.6, 3.8, 1.7)),
    col(OUT, tr(0, 17, ngon(20, 14.2)), segs(3, closed(mass))),
    col(KE.hair, tr(0, 17, ngon(20, 12.6)), poly(mass)),
    col(KE.hairDeep, segs(1, [[-2.4, 22], [-3, 8], [-2.4, -4]]), segs(1, [[5, 22], [5.4, 8], [5.6, -4]]),
        segs(1, [[-9, 16], [-10.4, 4], [-10 + s * 0.5, -4]])),
    col(KE.sheen, segs(1.3, [[-6, 25], [-6.8, 14], [-6.2, 3]]), segs(1.3, [[1.5, 27], [1.2, 13], [2, 0]]),
        segs(1.3, [[7.6, 24], [8.6, 12], [8.2, 2]]),
        poly([[-9.6, 22.4], [-4.6, 26.4], [1.6, 27.8], [6.8, 26.4], [9.8, 22.6], [5.6, 24.4], [0.4, 25.4], [-5, 24.6]])),
    col(KE.sheenHi, poly([[-3.4, 26.2], [1.6, 27.2], [4.4, 26.4], [0.6, 26]])));

  const backFall = [[1, 24], [9, 24.6], [13.2, 18], [14.6, 8], [14.4 + trail * 0.6, -1.5 + d * 0.5],
                    [12.2 + trail, -7.4 + d], [8.6, -4.6], [6.2, 2], [4.6, 10]];
  const sideLock = [[-2.2, 17.6], [0.8, 17.2], [0.6, 7], [0.2 + trail * 0.3, -1 + d * 0.5], [-1.4 + trail * 0.5, -3.6 + d],
                    [-2.6, 1], [-2.4, 9]];
  const side = pics(
    legsOf(KE.boots, stride, -stride),
    trunkOf(KE.jacket, KE.jacketShade),
    col(KE.shirt, poly([[-11.2, 11], [-8.4, 11], [-9.6, -12], [-13.2, -12]])),
    col(KE.piping, segs(1.1, [[-8.4, 11], [-9.6, -12]])),
    col(KE.jacketShade, rect(-8, -4.6, 4.6, 5)), col(KE.piping, rect(-8.2, 0.2, 5, 1)),
    col(OUT, rect(-4.4, -10, 9, 15.6)),
    col(KE.jacketShade, rect(-3, -8.6, 6.2, 12.8)),
    col(KE.jacketDeep, rect(-3, -8.6, 6.2, 1.6)),
    col(KE.brass, rect(-2.4, 0.6, 5, 1.7)),
    blob(KE.hair, backFall, 3),
    col(KE.hairDeep, segs(1, [[8, 20], [10.4, 8], [10 + trail * 0.6, -3]])),
    col(KE.sheen, segs(1.3, [[5, 20], [7.4, 10], [7.4, 2]]), segs(1.3, [[11.2, 18], [12.4, 8]])),
    col(OUT, tr(1.4, 17, ngon(20, 14.2))),
    col(KE.hair, tr(1.4, 17, ngon(20, 12.6))),
    col(KE.sheen, poly([[-8.6, 23.4], [-3.6, 27], [2.6, 28.4], [8, 26.4], [3, 26], [-3, 24.6]])),
    col(KE.sheenHi, poly([[-2.4, 26.6], [2.6, 27.6], [4.6, 27], [0.6, 26.4]])),
    col(KE.shirt, rect(-9.8, 3.2, 9, 3.4)),
    col(OUT, tr(-5.4, 12.2, ngon(16, 6.9))),
    col(KE.skin, tr(-6.4, 12.2, ngon(16, 5.6))),
    col(KE.hair, poly([[-12.8, 15.4], [-12.2, 13], [-10.4, 14.6], [-8.6, 12.4], [-6.4, 14.4], [-3.8, 15.4], [-3.4, 19.6], [-12.2, 19.6]])),
    col(OUT, segs(1.1, [[-12.2, 13], [-10.4, 14.6], [-8.6, 12.4], [-6.4, 14.4], [-3.8, 15.4]])),
    col(KE.eye, rect(-10.8, 9.2, 2.4, 2.1)), col(OUT, rect(-11, 11.1, 2.8, 0.9)),
    blob(KE.hair, sideLock, 2.4),
    col(KE.sheen, segs(1, [[-0.8, 15], [-0.9, 4]])));

  return sc(1.08, 1.08, shadowUnder, tr(0, bob, facingOf(dir, front, back, side)));
}

// ---- what exists now (Figures.keeperIn), for side-by-side ---------------------

const COATS = { yellow: [0xF7C81E, 0xC4920F, 0xFFD93E], red: [0xE5533A, 0xA9361F, 0xF4765B] };
function oilskin(coat, dir, phase) {
  const [main, shade, hood] = COATS[coat];
  const bob = 2.2 * Math.abs(Math.sin(phase * Math.PI)), stride = Math.sin(phase * 2 * Math.PI) * 3;
  const skin = 0xEBC8A0;
  const head = pics(col(OUT, tr(0, 15, ngon(16, 13))), col(hood, tr(0, 15, ngon(16, 11))));
  const legs = (a, b) => legsOf(0x2A1B12, a, b);
  const front = pics(legs(Math.max(0, stride), Math.max(0, -stride)), trunkOf(main, shade),
    col(OUT, rect(-1, -12, 2, 21)), col(OUT, tr(-5, -2, ngon(5, 1.2)), tr(-5, 4, ngon(5, 1.2))),
    head, col(OUT, tr(0, 13, ellipse(12, 7.5, 6.5))), col(skin, tr(0, 13, ellipse(12, 6.2, 5.2))),
    col(OUT, rect(-3.5, 13, 2, 2.2), rect(1.5, 13, 2, 2.2)));
  const back = pics(legs(Math.max(0, stride), Math.max(0, -stride)), trunkOf(main, shade),
    col(0x8C5A2B, rect(-6, -4, 12, 11)), col(OUT, rect(-6, 6, 12, 1.2), rect(-6, -4, 12, 1.2)),
    head, col(shade, rect(-1, 6, 2, 16)));
  const side = pics(legs(stride, -stride), trunkOf(main, shade), col(shade, rect(-3, -9, 7, 13)), head,
    col(OUT, tr(-5, 13, ellipse(10, 6, 6))), col(skin, tr(-6, 13, ellipse(10, 4.8, 4.8))),
    col(OUT, rect(-9, 13, 2, 2.2)), col(shade, poly([[-13, 22], [-4, 26], [2, 15], [-11, 19]])));
  return sc(1.08, 1.08, shadowUnder, tr(0, bob, facingOf(dir, front, back, side)));
}

// ---- the echo and the legend heads --------------------------------------------

// The landing echo, in the courier's outline: tails and all.
function echoCourier(clock, k) {
  const glowC = hexS(0x9CE7FF, 0.55 * k), edge = hexS(0xE6FAFF, 0.85 * k);
  const l = tailFront(0, 0), r = flipX(tailFront(0, 0));
  return sc(1.08, 1.08,
    col(hexS(0x9CE7FF, 0.16 * k), tr(0, -2, ngon(24, 24 + 2 * Math.sin(clock * 0.08)))),
    col(hexS(0x0A2A33, 0.35 * k), tr(0, -20, ellipse(16, 15, 6))),
    col(glowC, poly(TRUNK), poly(l), poly(r), tr(0, 17, ngon(20, 12.6))),
    col(edge, segs(1.6, closed(TRUNK)), segs(1.6, l.slice(0, 9)), segs(1.6, r.slice(2)),
        tr(0, 17, ring(20, 11.8, 1.6))));
}

// The little heads beside WASD / ARROWS in a level for two.
function legendHead(who) {
  const c = who === 'courier';
  const hair = c ? CO.hair : KE.hair, collar = c ? CO.coat : KE.jacket;
  return pics(
    col(hexS(0, 0.5), ngon(20, 10.5)),
    c ? null : col(KE.hairDeep, poly([[-9, 0], [9, 0], [10, -9], [-10, -9]])),
    col(collar, poly([[-7, -10], [7, -10], [5, -5], [-5, -5]])),
    col(hair, ngon(20, 9)),
    col(c ? 0xF6D9B6 : KE.skin, tr(0, -1.8, ngon(16, 5.2))),
    col(hair, poly([[-5.4, 1], [5.4, 1], [5, -0.6], [3, 0.2], [1, -0.8], [-1, 0.2], [-3, -0.8], [-5, 0]])),
    col(0x3A2A1A, tr(-2, -2.4, ngon(6, 0.9)), tr(2, -2.4, ngon(6, 0.9))),
    c ? pics(bow(-8, 5, 25), bow(8, 5, -25)) : null);
}

window.Sprites = { courier, keeper, oilskin, echoCourier, legendHead, hexS, pics, col, tr, sc };
