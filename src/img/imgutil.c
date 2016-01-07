/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2003-2011, University of Amsterdam
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
#include <h/graphics.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Guess the image content type from the   first  <size> bytes of the image
data. The answer of this routine  often   isn't  authoritive, but can be
used to select the proper loading routine  without blind testing and the
risc crashing poorly designed loading  routines   on  what is actually a
valid image format.  Size is supposed to be at least 64
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int string_prefix(const char *data, int len, int offset, const char *match);

int
image_type_from_data(char *data, int size)
{ if ( size > 2 )
  { unsigned short beshort = data[0]<<8|data[1];

    switch(beshort)
    { case 0xffd8:
	return IMG_IS_JPEG;
      default:
	break;
    }
  }

  if ( string_prefix(data, size, 0, "#define ") )
    return IMG_IS_XBM;			/* X11 bitmap (.BM) file */
  if ( string_prefix(data, size, 0, "/* Format_version=1, Width=") )
    return IMG_IS_SUNICON;		/* Old SUN Icon files */
  if ( string_prefix(data, size, 0, "/* XPM */") )
    return IMG_IS_XPM;			/* XPM Image file */
  if ( string_prefix(data, size, 0, "GIG8") )
    return IMG_IS_GIF;			/* GIF Image files */
  if ( data[0] == 'P' && data[1] >= '1' && data[1] <= '7' )
    return IMG_IS_PNM;			/* BPMPLUS images */
  if ( string_prefix(data, size, 0, "\x89PNG\x0d\x0a\x1a\x0a") )
    return IMG_IS_PNG;			/* PNG's (Not GIF) images */
  if ( string_prefix(data, size, 0, "BM") )
    return IMG_IS_BMP;			/* Windows BMP files */
  if ( string_prefix(data, size, 0, "IC") )
    return IMG_IS_ICO;			/* Windows ICO files */
  if ( string_prefix(data, size, 0, "CI") )
    return IMG_IS_ICO;			/* Windows ICO (color) files */

  return IMG_IS_UNKNOWN;		/* Don't know */
}


static int
string_prefix(const char *data, int len, int offset, const char *match)
{ data += offset;
  len  -= offset;

  while(*data == *match && len > 0 )
  { data++;
    match++;
    len--;
  }
  if ( len >= 0 && *match == 0 )
    return TRUE;

  return FALSE;
}
