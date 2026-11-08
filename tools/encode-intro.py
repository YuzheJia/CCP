# Turn the opening film into assets/intro.blv, the game's own small video format
# (described at the top of app/Video.hs). Gloss cannot read video and the game takes
# no image library, so the film is decoded by the game itself, from this.
#
# Runs inside Blender, which carries its own FFmpeg and numpy:
#
#   blender -b --factory-startup --python tools/encode-intro.py -- FILM.mp4 assets/intro.blv [WIDTH]
#
# What it does: renders every frame of the film at its own size; shrinks it to WIDTH
# pixels across (1280 by default), averaging the area each new pixel covers, and the
# game draws it back at 1280 across; keeps only the frames that differ from the one
# before (the source is 24 fps held over 60); gives each shot (between hard cuts) a
# 256-colour palette; and codes each frame against the one before, keeping any pixel
# that has barely changed. The sound is kept as 16-bit mono.
#
# 1920 x 1080 makes 1280 x 720, one pixel to a canvas unit: about 34 MB. 640 x 360
# makes about 9 MB, but looks soft on a large screen.
import bpy, sys, os, struct, tempfile, numpy as np
from fractions import Fraction

argv = sys.argv[sys.argv.index("--") + 1:]
src, out = argv[0], argv[1]
W = int(argv[2]) if len(argv) > 2 else 1280
KEEP = float(argv[3]) if len(argv) > 3 else 16.0   # how far a pixel may drift and be kept
rng = np.random.default_rng(7)
work = tempfile.mkdtemp(prefix="blv-")

# 1. Every frame, and the sound -------------------------------------------------
clip = bpy.data.movieclips.load(src)
sw, sh = clip.size
# Shrinking by p/q: each pixel is first repeated q times each way, then each p by p
# block is averaged, which is exactly the area each new pixel covers.
ratio = Fraction(sw, W)
p, q = ratio.numerator, ratio.denominator
if (sh * W) % sw:
    raise SystemExit("%d x %d does not shrink to %d across in whole pixels" % (sw, sh, W))
H = sh * W // sw
sc = bpy.context.scene
sc.sequence_editor_create()
ed = sc.sequence_editor
strips = ed.strips if hasattr(ed, "strips") else ed.sequences
movie = strips.new_movie("film", src, channel=1, frame_start=1)
count = movie.frame_final_duration
sc.frame_start, sc.frame_end = 1, count
r = sc.render
r.resolution_x, r.resolution_y, r.resolution_percentage = sw, sh, 100
r.fps = int(round(clip.fps))
r.image_settings.file_format = 'PNG'
r.image_settings.color_mode = 'RGB'
sc.view_settings.view_transform = 'Standard'
ticks_per_frame = 60.0 / clip.fps
print("film", sw, "x", sh, "->", W, "x", H, ";", count, "frames")

import aud
wav = os.path.join(work, "sound.wav")
try:
    aud.Sound(src).write(wav, rate=44100, channels=aud.CHANNELS_STEREO, format=aud.FORMAT_S16,
                         container=aud.CONTAINER_WAV, codec=aud.CODEC_PCM)
except Exception as e:  # Blender may complain at the very end, after writing it all
    print("sound:", e)

# One frame at a time, rendered, shrunk and thrown away, so a large film never fills
# the disk.
def load(i):
    sc.frame_set(i)
    path = os.path.join(work, "frame.png")
    r.filepath = path
    bpy.ops.render.render(write_still=True)
    img = bpy.data.images.load(path)
    a = np.empty(img.size[0] * img.size[1] * 4, dtype=np.float32)
    img.pixels.foreach_get(a)
    h, w = img.size[1], img.size[0]
    bpy.data.images.remove(img)
    os.remove(path)
    f = a.reshape(h, w, 4)[::-1, :, :3] * 255.0
    if q > 1:
        f = np.repeat(np.repeat(f, q, axis=0), q, axis=1)
    return f.reshape(H, p, W, p, 3).mean(axis=(1, 3)).astype(np.float32)

# 2. Which frames are new, and where the cuts are -------------------------------
frames, starts, cut_at, changes = {}, [], [], []
prev = None
for i in range(1, count + 1):
    f = load(i)
    d = None if prev is None else float(np.abs(f - prev).mean())
    if d is not None:
        changes.append(d)
    if d is None or d > 0.05:
        frames[i] = np.round(f).astype(np.uint8)  # a byte a channel keeps a long film in memory
        starts.append(int(round((i - 1) * ticks_per_frame)))
        if d is None or d > 20:
            cut_at.append(i)
    prev = f
order = sorted(frames)
shot_of = lambda i: max(k for k, c in enumerate(cut_at) if c <= i)
print("frames kept", len(order), "of", count, "; cuts at", cut_at,
      "; change between frames, quartiles", np.percentile(changes, [25, 50, 75]).round(2))

# 3. A palette for each shot ----------------------------------------------------
def nearest(px, pal):
    out = np.empty(len(px), dtype=np.int64)
    p2 = (pal ** 2).sum(1)
    for s in range(0, len(px), 16384):
        b = px[s:s + 16384]
        out[s:s + 16384] = (p2[None, :] - 2 * b @ pal.T).argmin(1)
    return out

def kmeans(px, k=256, iters=10):
    c = px[rng.choice(len(px), k, replace=False)].copy()
    for _ in range(iters):
        lab = nearest(px, c)
        for j in range(k):
            m = px[lab == j]
            c[j] = m.mean(axis=0) if len(m) else px[rng.integers(len(px))]
    return c

pals = []
for k in range(len(cut_at)):
    members = [i for i in order if shot_of(i) == k]
    sample = np.concatenate([frames[i].reshape(-1, 3).astype(np.float32)[rng.choice(W * H, 12000, replace=False)] for i in members])
    pals.append(np.clip(np.round(kmeans(sample)), 0, 255).astype(np.float32))

# 4. Code each frame against the one before -------------------------------------
def spans(vals, keep):
    ops = bytearray()
    def head(kind, n):
        if n <= 63: ops.append((kind << 6) | (n - 1))
        else: ops.append((kind << 6) | 63); ops.extend(struct.pack('<H', n - 64))
    def chunked(kind, n, body=None):
        while n > 0:
            m = min(n, 65535 + 64); head(kind, m)
            if body is not None: ops.extend(body(m))
            n -= m
    edges = np.flatnonzero(np.diff(keep.astype(np.int8))) + 1
    for a, b in zip(np.concatenate([[0], edges]), np.concatenate([edges, [len(vals)]])):
        if keep[a]:
            chunked(0, b - a)
            continue
        seg = vals[a:b]
        e2 = np.flatnonzero(np.diff(seg)) + 1
        lit = bytearray()
        def flush():
            j = 0
            while j < len(lit):
                m = min(len(lit) - j, 65535 + 64); head(2, m); ops.extend(lit[j:j + m]); j += m
            lit.clear()
        for x, y in zip(np.concatenate([[0], e2]), np.concatenate([e2, [len(seg)]])):
            if y - x >= 3:
                flush()
                colour = int(seg[x])
                chunked(1, y - x, lambda m: bytes([colour]))
            else:
                lit.extend(bytes(seg[x:y].astype(np.uint8)))
        flush()
    return bytes(ops)

body = bytearray()
shown, shot = None, -1
for i in order:
    k = shot_of(i); pal = pals[k]
    want = frames[i].reshape(-1, 3).astype(np.float32)
    idx = nearest(want, pal)
    if k != shot or shown is None:
        head = bytes([1]) + struct.pack('<H', len(pal)) + bytes(pal.astype(np.uint8).reshape(-1))
        keep = np.zeros(W * H, dtype=bool)
        shot = k
    else:
        head = bytes([0])
        err_keep = np.sqrt(((pal[shown] - want) ** 2).sum(1))
        err_new = np.sqrt(((pal[idx] - want) ** 2).sum(1))
        keep = (shown == idx) | (err_keep < np.maximum(KEEP, err_new + 0.5 * KEEP))
    shown = idx.copy() if not keep.any() else np.where(keep, shown, idx)
    payload = head + spans(shown.astype(np.uint8), keep)
    body += struct.pack('<I', len(payload)) + payload

# 5. The sound, and the file ----------------------------------------------------
raw = open(wav, 'rb').read() if os.path.exists(wav) else b''
fmt, pcm, p = None, b'', 12
while p < len(raw) - 8:
    cid = raw[p:p + 4]; n = struct.unpack('<I', raw[p + 4:p + 8])[0]
    if cid == b'fmt ': fmt = struct.unpack('<HHIIHH', raw[p + 8:p + 24])
    if cid == b'data': pcm = raw[p + 8:p + 8 + n]
    p += 8 + n + (n & 1)
if fmt:
    ch, rate = fmt[1], fmt[2]
    a = np.frombuffer(pcm[:len(pcm) // (2 * ch) * 2 * ch], dtype='<i2').reshape(-1, ch).astype(np.float32)
    mono = np.round(a.mean(axis=1)).astype('<i2')
else:
    rate, mono = 44100, np.zeros(0, dtype='<i2')

with open(out, 'wb') as fh:
    fh.write(b'BLV1' + struct.pack('<HHHHH', W, H, len(order), 60, 0))
    fh.write(struct.pack('<' + 'H' * len(order), *starts))
    fh.write(struct.pack('<IHI', rate, 1, len(mono)))
    fh.write(mono.tobytes())
    fh.write(body)
print("wrote", out, ":", len(order), "frames,", len(body), "bytes of picture,", len(mono) * 2, "of sound")
