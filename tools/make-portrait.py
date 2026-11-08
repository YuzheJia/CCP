# Rebuild a talk-box portrait from its character sheet:
#
#   blender -b --factory-startup --python tools/make-portrait.py -- assets/source/courier.webp app/Portrait.hs courier
#   blender -b --factory-startup --python tools/make-portrait.py -- assets/source/keeper.webp app/KeeperPortrait.hs keeper
#
# A sheet is pixel art drawn large (several screen pixels to one of its own). This
# finds that grid, gives the picture a 40-colour palette, takes the commonest colour
# of the middle of each grid cell, and writes the result as rows of letters, one per
# pixel, which the game turns into a bitmap. A sheet on a white ground instead of a
# clear one has the white that reaches its edges keyed out first.
#
# The blink and the talking mouth are pixels laid over those rows. Their positions
# below are for these sheets: a new drawing needs new ones.
import bpy, sys, os, numpy as np
from collections import deque

argv = sys.argv[sys.argv.index("--") + 1:]
src, hs = argv[0], argv[1]
sheet = argv[2] if len(argv) > 2 else "courier"
K = 40
rng = np.random.default_rng(3)

img = bpy.data.images.load(src)
w, h = img.size
a = np.empty(w * h * 4, dtype=np.float32)
img.pixels.foreach_get(a)
f = a.reshape(h, w, 4)[::-1].astype(np.float64)
rgb, alpha = f[..., :3] * 255, f[..., 3].copy()

# A white ground: clear every near-white pixel joined to the edge of the sheet.
if (alpha < 0.5).mean() < 0.01:
    pale = (rgb > 228).all(2)
    seen = np.zeros((h, w), bool)
    todo = deque((y, x) for y in range(h) for x in (0, w - 1) if pale[y, x])
    todo.extend((y, x) for x in range(w) for y in (0, h - 1) if pale[y, x])
    while todo:
        y, x = todo.popleft()
        if seen[y, x]: continue
        seen[y, x] = True
        for yy, xx in ((y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)):
            if 0 <= yy < h and 0 <= xx < w and pale[yy, xx] and not seen[yy, xx]:
                todo.append((yy, xx))
    alpha[seen] = 0

# The grid: the period and offset that make its cells most nearly one colour.
grey = rgb.mean(2) * (alpha > 0.5)
def spread(p, ox, oy):
    xs = np.floor((np.arange(w) - ox) / p).astype(int); xs -= xs.min()
    ys = np.floor((np.arange(h) - oy) / p).astype(int); ys -= ys.min()
    ids = (ys[:, None] * (xs.max() + 1) + xs[None, :]).ravel()
    n = np.bincount(ids); s = np.bincount(ids, grey.ravel()); s2 = np.bincount(ids, (grey ** 2).ravel())
    return (s2 - s * s / np.maximum(n, 1)).sum()
lo, hi = {"courier": (7.0, 10.01), "keeper": (6.0, 12.01)}[sheet]
_, p, ox, oy = min((spread(p, ox, oy), p, ox, oy)
                   for p in np.arange(lo, hi, 0.1)
                   for ox in np.arange(0, p, 1.0) for oy in np.arange(0, p, 1.0))

# The palette.
opaque = rgb[alpha > 0.5]
sample = opaque[rng.choice(len(opaque), 60000, replace=False)]
def nearest(px, pal):
    return ((pal ** 2).sum(1)[None, :] - 2 * px @ pal.T).argmin(1)
centres = sample[rng.choice(len(sample), K, replace=False)].copy()
for _ in range(15):
    lab = nearest(sample, centres)
    for j in range(K):
        m = sample[lab == j]
        if len(m): centres[j] = m.mean(0)
pal = np.round(centres)
flat, solid = rgb.reshape(-1, 3), alpha.reshape(-1) > 0.5
labels = np.full(h * w, -1)
labels[solid] = np.concatenate([nearest(flat[solid][s:s + 50000], pal) for s in range(0, solid.sum(), 50000)])
labels = labels.reshape(h, w)

# One pixel per cell: the commonest colour of its middle, or clear.
nx, ny = int((w - ox) // p), int((h - oy) // p)
grid = np.full((ny, nx), -1)
for gy in range(ny):
    for gx in range(nx):
        ya, yb = int(round(oy + gy * p)), int(round(oy + (gy + 1) * p))
        xa, xb = int(round(ox + gx * p)), int(round(ox + (gx + 1) * p))
        cell = labels[ya:yb, xa:xb].ravel()
        if (cell >= 0).mean() < 0.5: continue
        middle = labels[ya + 1:yb - 1, xa + 1:xb - 1].ravel()
        middle = middle[middle >= 0]
        grid[gy, gx] = np.bincount(middle if len(middle) else cell[cell >= 0]).argmax()
ys, xs = np.nonzero(grid >= 0)
grid = grid[ys.min():ys.max() + 1, xs.min():xs.max() + 1]
H, W = grid.shape
letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
rows = ["".join("." if v < 0 else letters[v] for v in row) for row in grid]
colours = {letters[j]: int(pal[j][0]) * 65536 + int(pal[j][1]) * 256 + int(pal[j][2]) for j in range(K)}

# The face, by hand, for each sheet. The letters are palette entries: find the skin
# and the dark lash colours in the rows if the sheet changes.
def block(x0, x1, y0, y1, c): return [(x, y, c) for y in range(y0, y1 + 1) for x in range(x0, x1 + 1)]
blink, mouth_open, mouth_shut = {
    "courier": lambda: (
        block(72, 76, 61, 70, 'B') + block(89, 93, 60, 68, 'B') + block(94, 94, 60, 61, 'B')
        + block(72, 76, 61, 61, 'k') + block(73, 77, 62, 62, 'C')
        + block(89, 94, 60, 60, 'k') + block(90, 95, 61, 61, 'C'),
        block(83, 86, 78, 78, 'k') + [(83, 79, 'k'), (84, 79, 'b'), (85, 79, 'b'), (86, 79, 'k')] + block(84, 85, 80, 80, 'k'),
        block(83, 86, 78, 79, 'B') + block(84, 85, 79, 79, 'k')),
    # The keeper's eyes shut under the lashes already drawn above them.
    "keeper": lambda: (
        block(66, 70, 50, 58, 'l') + block(87, 90, 50, 57, 'l')
        + block(66, 70, 50, 50, 'z') + block(87, 90, 50, 50, 'z'),
        block(78, 81, 69, 69, 'C') + [(78, 70, 'C'), (79, 70, 'G'), (80, 70, 'G'), (81, 70, 'C')] + block(79, 80, 71, 71, 'C'),
        block(78, 81, 69, 70, 'l') + block(79, 80, 69, 69, 'B')),
}[sheet]()

who = {"courier": ("The courier's", "her"), "keeper": ("The lighthouse keeper's", "their")}[sheet]
module = os.path.splitext(os.path.basename(hs))[0]
def patch(ps): return "[ " + "\n  , ".join("(%d, %d, '%s')" % t for t in ps) + " ]" if ps else "[]"
with open(hs, "w", newline="\n") as fh:
    fh.write('''-- | %s portrait for the talk box, pixel by pixel: %d by %d, from the
-- character sheet, at its own pixel grid. Generated by tools/make-portrait.py; edit
-- the patches here by hand if %s face changes.
module %s
  ( portraitW, portraitH
  , portraitPalette
  , portraitRows
  , blinkPatch
  , mouthOpenPatch
  , mouthShutPatch
  ) where

portraitW, portraitH :: Int
portraitW = %d
portraitH = %d

-- | Each letter of 'portraitRows' and its colour; @.@ is clear.
portraitPalette :: [(Char, Int)]
portraitPalette =
''' % (who[0], W, H, who[1], module, W, H))
    fh.write("  [ " + "\n  , ".join("('%s', 0x%06X)" % kv for kv in colours.items()) + " ]\n\n")
    fh.write("portraitRows :: [String]\nportraitRows =\n")
    fh.write("  [ " + "\n  , ".join('"%s"' % r for r in rows) + " ]\n\n")
    fh.write("-- | Pixels (column, row, colour) laid over the rows: %s eyes shut, for a blink.\n"
             "blinkPatch :: [(Int, Int, Char)]\nblinkPatch =\n  " % who[1] + patch(blink) + "\n\n")
    fh.write("-- | %s mouth open on a letter, and shut between words.\n"
             "mouthOpenPatch, mouthShutPatch :: [(Int, Int, Char)]\nmouthOpenPatch =\n  " % who[1].capitalize() + patch(mouth_open)
             + "\nmouthShutPatch =\n  " + patch(mouth_shut) + "\n")
print("wrote", hs, ":", W, "by", H, "pixels, grid", round(p, 2), "offset", ox, oy)
