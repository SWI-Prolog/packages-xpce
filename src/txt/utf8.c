/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2005-2011, University of Amsterdam
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

#include <wchar.h>
#include <SWI-Prolog.h>
#include <h/utf8.h>

/* Decode as Prolog does: see PL_utf8_code_point().  The decoder stops
   at the 0-byte of a C string.  Callers that decode a buffer that is
   not 0-terminated must make sure the sequence is complete (see
   UTF8_FBN()).
*/

char *
F_UTF8_GET_CHAR(const char *in, int *chr)
{ PL_utf8_code_point(&in, NULL, chr);

  return (char *)in;
}


char *
F_UTF8_PUT_CHAR(char *out, int chr)
{ if ( chr < 0x80 )
  { *out++ = chr;
  } else if ( chr < 0x800 )
  { *out++ = 0xc0|((chr>>6)&0x1f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x10000 )
  { *out++ = 0xe0|((chr>>12)&0x0f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x200000 )
  { *out++ = 0xf0|((chr>>18)&0x07);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( chr < 0x4000000 )
  { *out++ = 0xf8|((chr>>24)&0x03);
    *out++ = 0x80|((chr>>18)&0x3f);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  } else if ( (unsigned)chr < 0x80000000 )
  { *out++ = 0xfc|((chr>>30)&0x01);
    *out++ = 0x80|((chr>>24)&0x3f);
    *out++ = 0x80|((chr>>18)&0x3f);
    *out++ = 0x80|((chr>>12)&0x3f);
    *out++ = 0x80|((chr>>6)&0x3f);
    *out++ = 0x80|(chr&0x3f);
  }

  return out;
}


size_t
F_UTF8_STRLEN(const char *s, size_t len)
{ const char *e = &s[len];
  size_t l = 0;

  while(s<e)
  { int chr;

    s = utf8_get_char(s, &chr);
    l++;
  }

  return l;
}


size_t
F_UTF8_ENCLENW(const wchar_t *s, size_t len)
{ char out[10];
  size_t n = 0;
  const wchar_t *e = &s[len];

  while ( s < e )
  { int c;
    char *z;

    s = get_wchar(s, &c);	/* combines UTF-16 surrogate pairs */
    z = F_UTF8_PUT_CHAR(out, c);
    n += z - out;
  }

  return n;
}


size_t
F_UTF8_ENCLENA(const char *s, size_t len)
{ char out[10];
  const unsigned char *f = (const unsigned char *)s;
  const unsigned char *e = &f[len];
  size_t n = 0;

  for( ; f<e; f++)
  { char *z = F_UTF8_PUT_CHAR(out, *f);

    n += z-out;
  }

  return n;
}


