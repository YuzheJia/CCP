/* The sound mixer: the one piece of the game that is not Haskell.
 *
 * Haskell synthesises every sound at startup and hands the samples over with
 * bl_audio_load. From then on it only says what to play (bl_audio_play) and how
 * loud each looping bed should be (bl_audio_bed). The mixing and the output run on
 * a thread of this file's own, which never calls back into Haskell, so a slow frame
 * never stutters the sound, and the single-threaded runtime is never re-entered.
 *
 * On Windows it plays through winmm's waveOut, which every Windows has. Elsewhere
 * every function is a no-op, and the game is silent.
 */
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define BL_RATE   44100
#define BL_SOUNDS 64
#define BL_VOICES 48
#define BL_BEDS   8

int bl_audio_rate(void) { return BL_RATE; }

#ifdef _WIN32

#include <windows.h>
#include <mmsystem.h>

#define BL_FRAMES 441   /* 10 ms a buffer */
#define BL_BUFFERS 5    /* 50 ms queued: the latency, and the slack for a busy machine */

/* A sound has a left and a right side; a mono one shares one array for both. */
typedef struct { float *left, *right; int len; } Sound;
typedef struct { int active, sound; double pos; float rate, left, right, fade; } Voice;
typedef struct { int sound; double pos; float gain, rate, wantGain, wantRate; } Bed;

static Sound sounds[BL_SOUNDS];
static Voice voices[BL_VOICES];
static Bed beds[BL_BEDS];
static float master = 0.6f;
static volatile LONG muted = 0;

static CRITICAL_SECTION lock;
static HWAVEOUT device;
static HANDLE wake;
static WAVEHDR headers[BL_BUFFERS];
static short buffers[BL_BUFFERS][BL_FRAMES * 2];
static volatile LONG running = 0;

static float sampleAt(const Sound *s, const float *data, double pos, int wrap)
{
  int i = (int)pos;
  float frac = (float)(pos - i);
  int j = i + 1;
  if (wrap) { i %= s->len; j %= s->len; }
  else if (j >= s->len) return data[i] * (1 - frac);
  return data[i] + (data[j] - data[i]) * frac;
}

/* Loud moments are rounded off rather than clipped: untouched up to 0.6, then
   bending smoothly towards full scale. */
static float limit(float x)
{
  float a = fabsf(x), y;
  if (a < 0.6f) return x;
  y = 0.6f + 0.4f * tanhf((a - 0.6f) / 0.4f);
  return x < 0 ? -y : y;
}

static void mix(short *out)
{
  float left[BL_FRAMES], right[BL_FRAMES];
  int v, b, n;
  memset(left, 0, sizeof left);
  memset(right, 0, sizeof right);
  EnterCriticalSection(&lock);
  for (v = 0; v < BL_VOICES; v++) {
    Voice *vc = &voices[v];
    const Sound *s;
    if (!vc->active) continue;
    s = &sounds[vc->sound];
    for (n = 0; n < BL_FRAMES; n++) {
      float k = 1;
      if (vc->pos >= s->len - 1) { vc->active = 0; break; }
      if (vc->fade > 0) {
        vc->fade *= 0.99965f;
        if (vc->fade < 0.001f) { vc->active = 0; break; }
        k = vc->fade;
      }
      left[n] += sampleAt(s, s->left, vc->pos, 0) * vc->left * k;
      right[n] += sampleAt(s, s->right, vc->pos, 0) * vc->right * k;
      vc->pos += vc->rate;
    }
  }
  /* A bed glides to the level and speed it was last asked for, over about 40 ms,
     so changing them every frame never clicks. */
  for (b = 0; b < BL_BEDS; b++) {
    Bed *bd = &beds[b];
    const Sound *s;
    if (bd->sound < 0) continue;
    s = &sounds[bd->sound];
    if (s->len < 2) continue;
    if (bd->gain < 0.0005f && bd->wantGain < 0.0005f) { bd->gain = 0; continue; }
    for (n = 0; n < BL_FRAMES; n++) {
      bd->gain += (bd->wantGain - bd->gain) * 0.0006f;
      bd->rate += (bd->wantRate - bd->rate) * 0.0006f;
      left[n] += sampleAt(s, s->left, bd->pos, 1) * bd->gain;
      right[n] += sampleAt(s, s->right, bd->pos, 1) * bd->gain;
      bd->pos += bd->rate;
      while (bd->pos >= s->len) bd->pos -= s->len;
    }
  }
  LeaveCriticalSection(&lock);
  for (n = 0; n < BL_FRAMES; n++) {
    float k = muted ? 0 : master;
    out[2 * n] = (short)(limit(left[n] * k) * 32000);
    out[2 * n + 1] = (short)(limit(right[n] * k) * 32000);
  }
}

static DWORD WINAPI pump(LPVOID unused)
{
  int i;
  (void)unused;
  while (running) {
    WaitForSingleObject(wake, 20);
    for (i = 0; i < BL_BUFFERS; i++) {
      if (headers[i].dwFlags & WHDR_DONE) {
        mix(buffers[i]);
        waveOutWrite(device, &headers[i], sizeof(WAVEHDR));
      }
    }
  }
  return 0;
}

/* 1 if sound will be heard, 0 if there is no output device. */
int bl_audio_init(void)
{
  WAVEFORMATEX fmt;
  HANDLE thread;
  int i;
  if (running) return 1;
  InitializeCriticalSection(&lock);
  for (i = 0; i < BL_BEDS; i++) { beds[i].sound = -1; beds[i].rate = beds[i].wantRate = 1; }
  memset(&fmt, 0, sizeof fmt);
  fmt.wFormatTag = WAVE_FORMAT_PCM;
  fmt.nChannels = 2;
  fmt.nSamplesPerSec = BL_RATE;
  fmt.wBitsPerSample = 16;
  fmt.nBlockAlign = 4;
  fmt.nAvgBytesPerSec = BL_RATE * 4;
  wake = CreateEvent(NULL, FALSE, FALSE, NULL);
  if (!wake) return 0;
  if (waveOutOpen(&device, WAVE_MAPPER, &fmt, (DWORD_PTR)wake, 0, CALLBACK_EVENT) != MMSYSERR_NOERROR)
    return 0;
  running = 1;
  for (i = 0; i < BL_BUFFERS; i++) {
    memset(&headers[i], 0, sizeof(WAVEHDR));
    memset(buffers[i], 0, sizeof buffers[i]);
    headers[i].lpData = (LPSTR)buffers[i];
    headers[i].dwBufferLength = sizeof buffers[i];
    waveOutPrepareHeader(device, &headers[i], sizeof(WAVEHDR));
    waveOutWrite(device, &headers[i], sizeof(WAVEHDR));
  }
  thread = CreateThread(NULL, 0, pump, NULL, 0, NULL);
  if (!thread) { running = 0; waveOutReset(device); waveOutClose(device); return 0; }
  SetThreadPriority(thread, THREAD_PRIORITY_HIGHEST);
  CloseHandle(thread);
  return 1;
}

/* right may be NULL for a mono sound. */
void bl_audio_load(int id, const float *left, const float *right, int len)
{
  float *l, *r = NULL, *oldL, *oldR;
  size_t bytes = sizeof(float) * (size_t)len;
  if (id < 0 || id >= BL_SOUNDS || len <= 0) return;
  l = (float *)malloc(bytes);
  if (!l) return;
  memcpy(l, left, bytes);
  if (right) {
    r = (float *)malloc(bytes);
    if (!r) { free(l); return; }
    memcpy(r, right, bytes);
  }
  if (running) EnterCriticalSection(&lock);
  oldL = sounds[id].left;
  oldR = sounds[id].right;
  sounds[id].left = l;
  sounds[id].right = r ? r : l;
  sounds[id].len = len;
  if (running) LeaveCriticalSection(&lock);
  if (oldR != oldL) free(oldR);
  free(oldL);
}

/* pan -1 is all left, 1 all right. The quietest playing voice gives way when all
   are busy. */
void bl_audio_play(int id, float gain, float rate, float pan)
{
  int v, slot = -1;
  float quietest = 1e9f;
  if (!running || id < 0 || id >= BL_SOUNDS || sounds[id].len < 2 || rate <= 0) return;
  if (pan < -1) pan = -1;
  if (pan > 1) pan = 1;
  EnterCriticalSection(&lock);
  for (v = 0; v < BL_VOICES; v++) {
    float loud;
    if (!voices[v].active) { slot = v; break; }
    loud = voices[v].left + voices[v].right;
    if (loud < quietest) { quietest = loud; slot = v; }
  }
  voices[slot].active = 1;
  voices[slot].sound = id;
  voices[slot].pos = 0;
  voices[slot].rate = rate;
  voices[slot].fade = 0;
  /* Balance: centred, both sides play in full; panned, the far side fades. */
  voices[slot].left = gain * (pan > 0 ? 1 - pan : 1);
  voices[slot].right = gain * (pan < 0 ? 1 + pan : 1);
  LeaveCriticalSection(&lock);
}

void bl_audio_bed(int slot, int id, float gain, float rate)
{
  if (!running || slot < 0 || slot >= BL_BEDS || id < 0 || id >= BL_SOUNDS) return;
  EnterCriticalSection(&lock);
  if (beds[slot].sound != id) {
    beds[slot].sound = id;
    beds[slot].pos = 0;
    beds[slot].gain = 0;
    beds[slot].rate = rate;
  }
  beds[slot].wantGain = gain < 0 ? 0 : gain;
  beds[slot].wantRate = rate <= 0 ? 1 : rate;
  LeaveCriticalSection(&lock);
}

/* Every voice playing this sound fades out, over about half a second. */
void bl_audio_stop(int id)
{
  int v;
  if (!running) return;
  EnterCriticalSection(&lock);
  for (v = 0; v < BL_VOICES; v++)
    if (voices[v].active && voices[v].sound == id && voices[v].fade == 0) voices[v].fade = 1;
  LeaveCriticalSection(&lock);
}

void bl_audio_mute(int on) { InterlockedExchange(&muted, on ? 1 : 0); }

#else

int bl_audio_init(void) { return 0; }
void bl_audio_load(int id, const float *left, const float *right, int len) { (void)id; (void)left; (void)right; (void)len; }
void bl_audio_play(int id, float gain, float rate, float pan) { (void)id; (void)gain; (void)rate; (void)pan; }
void bl_audio_bed(int slot, int id, float gain, float rate) { (void)slot; (void)id; (void)gain; (void)rate; }
void bl_audio_stop(int id) { (void)id; }
void bl_audio_mute(int on) { (void)on; }

#endif
