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

/* Conversion between wchar_t (the host's wide-char type, used by the
 * embedding API, ENC_WCHAR streams, and Win32 file APIs) and charW (the
 * internal storage type used by xpce string and text_buffer code).
 *
 * Used at the few external boundaries where xpce sees a wchar_t buffer
 * from the outside world.  Internal text-handling stays in charW.
 *
 * When sizeof(wchar_t) == sizeof(charW) — Linux today, Linux/Windows
 * with charW still typedef'd to wchar_t — the helpers compile to a
 * straight per-element copy.  When sizeof(charW) > sizeof(wchar_t) —
 * Windows after charW is widened to uint32_t while wchar_t remains
 * 16-bit — they perform UTF-16 surrogate decode/encode so the internal
 * buffer holds one Unicode code point per slot.
 *
 * The size comparisons fold to compile-time constants; the dead branch
 * disappears in the optimised binary.
 */

#ifndef CHARW_H_INCLUDED
#define CHARW_H_INCLUDED

#include <stddef.h>
#include "str.h"			/* charW typedef */

#ifndef CHARW_IS_UTF16_LEAD
#define CHARW_IS_UTF16_LEAD(c)	((c) >= 0xD800 && (c) <= 0xDBFF)
#define CHARW_IS_UTF16_TRAIL(c)	((c) >= 0xDC00 && (c) <= 0xDFFF)
#endif

/** Convert a wchar_t buffer to a charW buffer.
 *
 * @param dst     destination charW buffer; must hold at least
 *                wchar_to_charW_len(src, src_len) entries.
 * @param src     source wchar_t buffer.
 * @param src_len number of wchar_t units in src.
 * @return        end pointer in dst (one past last written charW).
 */
static inline charW *
wchar_to_charW(charW *dst, const wchar_t *src, size_t src_len)
{ if ( sizeof(wchar_t) == sizeof(charW) )
  { for ( size_t i = 0; i < src_len; i++ )
      dst[i] = (charW)src[i];
    return dst + src_len;
  }
  /* sizeof(wchar_t) < sizeof(charW): combine UTF-16 surrogate pairs. */
  const wchar_t *e = src + src_len;
  while ( src < e )
  { int c = (int)*src++;
    if ( CHARW_IS_UTF16_LEAD(c) && src < e && CHARW_IS_UTF16_TRAIL((int)*src) )
    { int trail = (int)*src++;
      c = ((c - 0xD800) << 10) + (trail - 0xDC00) + 0x10000;
    }
    *dst++ = (charW)c;
  }
  return dst;
}

/** Number of charW slots required to hold a wchar_t source. */
static inline size_t
wchar_to_charW_len(const wchar_t *src, size_t src_len)
{ if ( sizeof(wchar_t) == sizeof(charW) )
    return src_len;
  size_t out = 0;
  const wchar_t *e = src + src_len;
  while ( src < e )
  { int c = (int)*src++;
    if ( CHARW_IS_UTF16_LEAD(c) && src < e && CHARW_IS_UTF16_TRAIL((int)*src) )
      src++;
    out++;
  }
  return out;
}

/** Convert a charW buffer to a wchar_t buffer.
 *
 * @param dst     destination wchar_t buffer; must hold at least
 *                charW_to_wchar_len(src, src_len) entries.
 * @param src     source charW buffer (one code point per slot).
 * @param src_len number of charW slots in src.
 * @return        end pointer in dst (one past last written wchar_t).
 */
static inline wchar_t *
charW_to_wchar(wchar_t *dst, const charW *src, size_t src_len)
{ if ( sizeof(wchar_t) == sizeof(charW) )
  { for ( size_t i = 0; i < src_len; i++ )
      dst[i] = (wchar_t)src[i];
    return dst + src_len;
  }
  /* sizeof(charW) > sizeof(wchar_t): emit UTF-16 surrogate pair for SMP. */
  for ( size_t i = 0; i < src_len; i++ )
  { int c = (int)src[i];
    if ( c <= 0xFFFF )
    { *dst++ = (wchar_t)c;
    } else
    { int adj = c - 0x10000;
      *dst++ = (wchar_t)(0xD800 + (adj >> 10));
      *dst++ = (wchar_t)(0xDC00 + (adj & 0x3FF));
    }
  }
  return dst;
}

/** Number of wchar_t units required to hold a charW source. */
static inline size_t
charW_to_wchar_len(const charW *src, size_t src_len)
{ if ( sizeof(wchar_t) == sizeof(charW) )
    return src_len;
  size_t out = 0;
  for ( size_t i = 0; i < src_len; i++ )
    out += (src[i] > 0xFFFF) ? 2 : 1;
  return out;
}

#endif /*CHARW_H_INCLUDED*/
