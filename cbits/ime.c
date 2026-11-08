/* The game is played on letter keys: W A S D, Z, R, P, I. A Chinese, Japanese or
 * Korean input method in its own mode takes those keys to compose text before the
 * game sees them, so the game turned on in Chinese mode looked dead to them. The
 * game has no text to type, so it switches input methods off for itself. This must
 * happen before any window is made, the backend's hidden helper window included.
 * Elsewhere than Windows it does nothing.
 */
#ifdef _WIN32

#include <windows.h>
#include <imm.h>

void bl_disable_ime(void)
{
  ImmDisableIME((DWORD)-1);  /* every thread of this process */
}

#else

void bl_disable_ime(void) {}

#endif
