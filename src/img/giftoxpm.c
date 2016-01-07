/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1997-2013, University of Amsterdam
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

#ifdef __WINDOWS__
#include <msw/include.h>
#define  FOR_MSW 1
#include <msw/xpm.h>
#else
#include <h/kernel.h>
#include <X11/xpm.h>
#endif

#include "gif.h"
#include <stdlib.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#define XpmMalloc(size) (void *)malloc((size))

static int
alloc_colortable(int ncolors, void *closure)
{ XpmImage *img = closure;

  if ( ncolors < 0 || ncolors > 256 )
    return GIF_INVALID;

  img->ncolors    = ncolors;
  img->colorTable = XpmMalloc(sizeof(XpmColor) * ncolors);

  if ( img->colorTable )
  { memset(img->colorTable, 0, sizeof(XpmColor) * ncolors);

    return GIF_OK;
  }

  return GIF_NOMEM;
}


static int
alloc_color(int index, int r, int g, int b, void *closure)
{ XpmImage *img = closure;
  XpmColor *c;

  if ( index < 0 || index >= (int)img->ncolors )
    return GIF_INVALID;
  c = &img->colorTable[index];

  if ( (c->c_color = XpmMalloc(8)) )
  { sprintf(c->c_color, "#%02x%02x%02x", r, g, b);

    return GIF_OK;
  }

  return GIF_NOMEM;
}


static int
gif_extension(int ext, void *data, void *closure)
{ XpmImage *img = closure;

  switch(ext)
  { case GIFEXT_TRANSPARENT:
    { XpmColor *c;
      long i = (long)(intptr_t)data;

      DEBUG(NAME_gif, Cprintf("Using %d as transparent (ncolors=%d)\n",
			      i, img->ncolors));

      if ( i < 0 || i >= img->ncolors )
	return GIF_INVALID;

      c = &img->colorTable[i];
      strcpy(c->c_color, "None");	/* malloced 8 bytes, so ok. */
      break;
    }
    default:
      assert(0);
  }

  return GIF_OK;
}


int
XpmReadGIF(IOSTREAM *fd, XpmImage *img)
{ long here = Stell(fd);
  int w, h;

  img->ncolors    = 0;
  img->colorTable = NULL;
  img->data       = NULL;

  switch( GIFReadFD(fd,
		    &img->data,
		    &w,
		    &h,
		    alloc_colortable,
		    alloc_color,
		    gif_extension,
		    img) )
  { case GIF_OK:
      img->width = w;
      img->height = h;
      return XpmSuccess;
    case GIF_NOMEM:
      Sseek(fd, here, SIO_SEEK_SET);
      return XpmNoMemory;
    case GIF_INVALID:
    default:
      Sseek(fd, here, SIO_SEEK_SET);
      return XpmFileInvalid;
  }
}
