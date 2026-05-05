/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef PCE_CHARWIDTH_H_INCLUDED
#define PCE_CHARWIDTH_H_INCLUDED

#include <stdint.h>

/* uchar_t is also defined in terminal.h; use the same guard to avoid
 * duplicate typedef errors when both headers are included. */
#ifndef UCHAR_T_DEFINED
#define UCHAR_T_DEFINED
typedef uint32_t uchar_t;
#endif

/* Forward declaration of the host's wcwidth wrapper (defined in
 * src/itf/interface.c).  Keeps this header free of <wchar.h> so the
 * static inline below can be included without _XOPEN_SOURCE plumbing. */
extern int hostWcWidth(int c);

/** Return the display column width of a Unicode code point.
 *
 * Returns 0 for combining / non-spacing characters (they attach to
 * the preceding base character and consume no extra column), 2 for
 * East-Asian wide / fullwidth characters, and 1 for everything else.
 *
 * Delegates to the host (Prolog) wcwidth via hostWcWidth() — which is
 * the same implementation pl-read.c / pl-write.c use — so xpce stays
 * consistent with the rest of SWI-Prolog regardless of the process
 * locale.  Explicit table look-ups are kept for the most common
 * combining and wide-character ranges so the result is correct even
 * before the host has registered its callback (e.g., during early
 * xpce initialisation).
 *
 * This function is shared between txt/terminal.c, txt/textimage.c, and
 * txt/editor.c.  It is declared static inline so each translation unit
 * gets its own copy without link-time conflicts.
 */

static inline int
uchar_display_width(uchar_t c)
{ if ( c == 0 )
    return 0;
  /* Non-spacing / combining characters.  Checked before wcwidth so the
   * result is independent of the process locale. */
  if ( (c >= 0x0300 && c <= 0x036F) ||	/* Combining Diacritical Marks */
       (c >= 0x1AB0 && c <= 0x1AFF) ||	/* Combining Diacritical Marks Extended */
       (c >= 0x1DC0 && c <= 0x1DFF) ||	/* Combining Diacritical Marks Supplement */
       (c >= 0x20D0 && c <= 0x20FF) ||	/* Combining Diacritical Marks for Symbols */
       (c >= 0xFE20 && c <= 0xFE2F) ||	/* Combining Half Marks */
       c == 0x200C || c == 0x200D ||	/* ZWNJ / ZWJ */
       c == 0xFE0E || c == 0xFE0F )	/* Variation Selectors 15 / 16 */
    return 0;
  /* East-Asian wide / fullwidth characters and wide emoji. */
  if ( (c >= 0x1100 && c <= 0x115F) ||	/* Hangul Jamo */
       (c >= 0x2E80 && c <= 0x303E) ||	/* CJK Radicals etc. */
       (c >= 0x3041 && c <= 0x33BF) ||	/* Hiragana .. CJK Compatibility */
       (c >= 0x3400 && c <= 0x4DBF) ||	/* CJK Extension A */
       (c >= 0x4E00 && c <= 0x9FFF) ||	/* CJK Unified Ideographs */
       (c >= 0xA000 && c <= 0xA4CF) ||	/* Yi */
       (c >= 0xA960 && c <= 0xA97F) ||	/* Hangul Jamo Extended-A */
       (c >= 0xAC00 && c <= 0xD7FF) ||	/* Hangul Syllables + Jamo Ext-B */
       (c >= 0xF900 && c <= 0xFAFF) ||	/* CJK Compatibility Ideographs */
       (c >= 0xFE10 && c <= 0xFE1F) ||	/* Vertical Forms */
       (c >= 0xFE30 && c <= 0xFE4F) ||	/* CJK Compatibility Forms */
       (c >= 0xFF01 && c <= 0xFF60) ||	/* Fullwidth Latin/Katakana */
       (c >= 0xFFE0 && c <= 0xFFE6) ||	/* Fullwidth Signs */
       (c >= 0x1B000 && c <= 0x1B0FF) ||/* Kana Supplement */
       (c >= 0x1F004 && c <= 0x1F0CF) ||/* Mahjong/Domino tiles */
       (c >= 0x1F300 && c <= 0x1F9FF) ||/* Misc Symbols, Emoticons */
       (c >= 0x20000 && c <= 0x2FFFD) ||/* CJK Extension B-F */
       (c >= 0x30000 && c <= 0x3FFFD) ) /* CJK Extension G-H */
    return 2;
  /* Fall back to the host's wcwidth for rare cases not covered above.
   * Treat -1 (host not yet registered, or code point unknown to its
   * tables) as 1 column. */
  { int w = hostWcWidth((int)c);
    if ( w >= 0 )
      return w;
  }
  return 1;
}

#endif /*PCE_CHARWIDTH_H_INCLUDED*/
