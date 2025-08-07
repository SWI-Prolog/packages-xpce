/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2005-2025, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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

#include <h/kernel.h>
#include <h/utf8.h>
#include <stdbool.h>

#define utf8_get_uchar(s, chr) (unsigned char*)utf8_get_char((char *)(s), chr)

#ifndef MB_LEN_MAX
#define MB_LEN_MAX 6
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These  functions  translate  CharArray  (PceString,  Name)  into  a  format
suitable to drive operating- or windowsystem   calls,  such as accessing
filenames, window titles, etc.

Both UTF-8 and locale-defined multibyte strings are not designed to deal
with embedded 0-bytes and APIs   generally  accept 0-terminated strings.
Only for wide-character arrays we work with sizes.

	* MB
	CTYPE Locale defined translation

	* UTF-8
	Well known UTF-8 encoding of UNICODE

	* WC
	wchar_t representation of UNICODE

The returned strings of this library are   stored in a ring of RING_SIZE
fields.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	      RING		*
		 *******************************/

#define RING_SIZE 16

typedef struct rcell
{ char		*data;			/* actual data */
  char		*bufp;			/* pointer in buffer */
  char		*limitp;		/* pointer to end */
  size_t	allocated;		/* bytes allocated */
} rcell;

static rcell ring[RING_SIZE] = {{0}};
static int   ring_index = 0;

static rcell *
find_ring(void)
{ rcell *c = &ring[ring_index++];

  if ( ring_index == RING_SIZE )
    ring_index = 0;

  if ( c->allocated == 0 )
  { c->allocated = 256;
    c->data = pceMalloc(c->allocated);
  } else if ( c->allocated >= 4096 )
  { c->allocated = 256;
    pceFree(c->data);
    c->data = pceMalloc(c->allocated);
  }
  c->bufp   = c->data;
  c->limitp = &c->data[c->allocated];

  return c;
}


static void
roomBuffer(rcell *c, size_t room)
{ while ( c->bufp + room > c->limitp )
  { size_t size = c->bufp - c->data;

    c->allocated *= 2;
    c->data   = pceRealloc(c->data, c->allocated);
    c->limitp = &c->data[c->allocated];
    c->bufp   = &c->data[size];
  }
}


static void
addByte(rcell *c, int byte)
{ roomBuffer(c, 1);

  *c->bufp++ = byte;
}


		 /*******************************
		 *	  CHARARRAY -->		*
		 *******************************/

typedef const unsigned char cuchar;
typedef const wchar_t       cwchar;

char *
stringToUTF8(PceString str, size_t *olen)
{ rcell *out;

  if ( isstrA(str) )
  { cuchar *s = (cuchar*) str->s_textA;
    cuchar *e = &s[str->s_size];

    for( ; s<e; s++ )			/* do we need conversion */
    { if ( *s & 0x80 )
	break;
    }
    if ( s == e )
    { if ( olen )
	*olen = str->s_size;
      return (char *)str->s_textA;	/* no */
    }

    out = find_ring();
    for(s = (cuchar*) str->s_textA; s<e; s++ )
    { roomBuffer(out, 2);		/* max bytes per UTF-8 < 256 */

      out->bufp = utf8_put_char(out->bufp, *s);
    }
  } else
  { cwchar *s = str->s_textW;
    cwchar *e = &s[str->s_size];

    out = find_ring();
    while( s < e )
    { int c;

      roomBuffer(out, 6);		/* max bytes per UTF-8 */
      s = get_wchar(s, &c);
      out->bufp = utf8_put_char(out->bufp, c);
    }
  }

  if ( olen )
    *olen = out->bufp - out->data;
  addByte(out, 0);

  return out->data;
}


static char *
stringToMB(PceString str)
{ rcell *out;
  mbstate_t mbs;
  char b[MB_LEN_MAX];
  size_t rc;

  memset(&mbs, 0, sizeof(mbs));

  if ( isstrA(str) )
  { cuchar *s = (cuchar*) str->s_textA;
    cuchar *e = &s[str->s_size];

    for( ; s<e; s++ )			/* do we need conversion? */
    { if ( (rc=wcrtomb(b, *s, &mbs)) == 1 && b[0] == *s )
	continue;
      if ( rc == (size_t)-1 )
	return NULL;			/* cannot convert */
    }
    if ( s == e )
      return (char *)str->s_textA;		/* no */

    memset(&mbs, 0, sizeof(mbs));
    out = find_ring();
    for( ; s <= e; s++ )		/* <=: also 0-byte! */
    { roomBuffer(out, MB_LEN_MAX);

      if ( (rc=wcrtomb(out->bufp, *s, &mbs)) == (size_t)-1 )
	return NULL;
      out->bufp += rc;
    }
  } else
  { cwchar *s = str->s_textW;
    cwchar *e = &s[str->s_size];

    out = find_ring();
    for( ; s<e; s++ )
    { roomBuffer(out, MB_LEN_MAX);

      if ( (rc=wcrtomb(out->bufp, *s, &mbs)) == (size_t)-1 )
	return NULL;
      out->bufp += rc;
    }
  }

  roomBuffer(out, MB_LEN_MAX+1);	/* add restore state + 0-byte */
  if ( wcrtomb(out->bufp, 0, &mbs) ==  (size_t)-1 )
    return NULL;

  return out->data;
}


wchar_t *
charArrayToWC(CharArray ca, size_t *len)
{ PceString str = &ca->data;

  if ( len )
    *len = str->s_size;

  if ( isstrA(str) )
  { rcell *out = find_ring();
    cuchar *s = (cuchar*) str->s_textA;
    cuchar *e = &s[str->s_size];
    wchar_t *o;

    roomBuffer(out, (str->s_size+1)*sizeof(wchar_t));

    for(o=(wchar_t*)out->data ; s<e; )
    { *o++ = *s++;
    }
    *o = 0;

    return (wchar_t *)out->data;
  } else
    return str->s_textW;
}


char *
charArrayToUTF8(CharArray ca)
{ return stringToUTF8(&ca->data, NULL);
}


char *
charArrayToMB(CharArray ca)
{ return stringToMB(&ca->data);
}


char *
nameToMB(Name nm)
{ return stringToMB(&nm->data);
}


char *
nameToUTF8(Name nm)
{ return stringToUTF8(&nm->data, NULL);
}


wchar_t *
nameToWC(Name nm, size_t *len)
{ return charArrayToWC((CharArray)nm, len);
}


		 /*******************************
		 *	    <-- NAME		*
		 *******************************/

static Any
UTF8ToCharArray(const char *utf8, bool asname)
{ cuchar *in;
  cuchar *e;
  size_t len;
  bool wide;

  for(in=(cuchar*)utf8; *in; in++)
  { if ( (*in)&0x80 )
      break;
  }

  if ( *in == EOS )			/* simple ASCII string */
  { if ( asname )
      return CtoName(utf8);
    else
      return CtoString(utf8);
  }

  e = in + strlen((const char*)in);
  for(in=(cuchar*)utf8, len=0, wide=FALSE; in < e; )
  { int chr;

    in = utf8_get_uchar(in, &chr);
    if ( chr > 0xff )
      wide = true;
    len++;
#if SIZEOF_WCHAR_T == 2
    if ( chr > 0xffff )
      len++;
#endif
  }

  if ( wide )
  { wchar_t *ws, *o;
    bool mlcd;
    string s;
    Any nm;

    if ( len < 1024 )
    { ws = alloca((len+1)*sizeof(wchar_t));
      mlcd = false;
    } else
    { ws = pceMalloc((len+1)*sizeof(wchar_t));
      mlcd = true;
    }

    for(in=(cuchar*)utf8, o=ws; in < e; )
    { int chr;

      in = utf8_get_uchar(in, &chr);
      o = put_wchar(o, chr);
    }

    str_set_n_wchar(&s, len, ws);
    nm = asname ? (Any)StringToName(&s) : (Any)StringToString(&s);

    if ( mlcd )
      pceFree(ws);

    return nm;
  } else
  { char *as, *o;
    bool mlcd;
    string s;
    Any nm;

    if ( len < 1024 )
    { as = alloca((len+1));
      mlcd = false;
    } else
    { as = pceMalloc((len+1));
      mlcd = true;
    }

    for(in=(cuchar*)utf8, o=as; in < e; )
    { int chr;

      in = utf8_get_uchar(in, &chr);
      *o++ = (char)chr;
    }

    str_set_n_ascii(&s, len, as);
    nm = asname ? (Any)StringToName(&s) : (Any)StringToString(&s);

    if ( mlcd )
      pceFree(as);

    return nm;
  }
}

Name
UTF8ToName(const char *utf8)
{ return UTF8ToCharArray(utf8, true);
}


StringObj
UTF8ToString(const char *utf8)
{ return UTF8ToCharArray(utf8, false);
}

Name
MBToName(const char *mb)
{ size_t len;
  mbstate_t mbs;
  const char *in = mb;

  memset(&mbs, 0, sizeof(mbs));
  if ( (len = mbsrtowcs(NULL, &in, 0, &mbs)) != (size_t)(-1) )
  { string s;
    wchar_t *ws;
    int mlcd;
    Name nm;

    if ( len < 1024 )
    { ws = alloca((len+1)*sizeof(wchar_t));
      mlcd = FALSE;
    } else
    { ws = pceMalloc((len+1)*sizeof(wchar_t));
      mlcd = TRUE;
    }

    memset(&mbs, 0, sizeof(mbs));
    in = mb;
    mbsrtowcs(ws, &in, len+1, &mbs);
    str_set_n_wchar(&s, len, ws);
    nm = StringToName(&s);

    if ( mlcd )
      pceFree(ws);

    return nm;
  }

  return NULL;
}


Name
WCToName(const wchar_t *wc, size_t len)
{ if ( wc )
  { string s;

    if ( len == (size_t)-1 )
      len = wcslen(wc);

    str_set_n_wchar(&s, len, (wchar_t *)wc);

    return StringToName(&s);
  }

  return NULL;
}


StringObj
WCToString(const wchar_t *wc, size_t len)
{ if ( wc )
  { string s;

    str_set_n_wchar(&s, len, (wchar_t *)wc);

    return StringToString(&s);
  }

  return NULL;
}


		 /*******************************
		 *	     FILE-NAMES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Turn  an  OS  filename  into  an  XPCE  name.  With  XOS,  the  filename
representation is always UTF-8
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

Name
FNToName(const char *name)
{ Name rc;
#ifdef O_XOS
  rc = UTF8ToName(name);
#else
  rc = MBToName(name);
#endif

  if ( !rc )				/* Illegal Multibyte; use plain */
    rc = CtoName(name);

  return rc;
}


char *
charArrayToFN(CharArray ca)
{
#ifdef O_XOS
   return charArrayToUTF8(ca);
#else
   return charArrayToMB(ca);
#endif
}


char *
stringToFN(PceString s)
{
#ifdef O_XOS
  return stringToUTF8(s, NULL);
#else
   return stringToMB(s);
#endif
}


char *
nameToFN(Name nm)
{ return stringToFN(&nm->data);
}
