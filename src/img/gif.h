/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1997-2011, University of Amsterdam
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

#ifndef GIFHDRH
#define GIFHDRH

typedef unsigned int PIXEL;		/* for X11 compatibility */

typedef int (*GIFAllocColor)(int index,
			     int r, int g, int b,
			     void *closure);
typedef int (*GIFAllocColorTable)(int size, void *closure);
typedef int (*GIFDoExtension)(int ext, void *data, void *closure);

#define GIFEXT_TRANSPARENT 0		/* data = colour index */

#define GIF_OK		0
#define GIF_NOMEM	1
#define GIF_INVALID	2

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define MAXCOLORMAPSIZE	256
#define CM_RED	0
#define CM_GREEN 1
#define CM_BLUE 2
#define UCHAR unsigned char
#define MAX_LZW_BITS	12

COMMON(int) GIFReadFD(IOSTREAM *fd,
		      PIXEL **data, int *width, int *height,
		      GIFAllocColorTable at,
		      GIFAllocColor ac,
		      GIFDoExtension doext,
		      void *closure);
COMMON(const char)  *GIFError(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
First include <X11/xpm.h> to get  this   prototype.  The  function is in
giftoxpm.c. The usage in x11/xconvert.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef XPM_h
COMMON(int)	XpmReadGIF(IOSTREAM *fd, XpmImage *image);
#endif

#endif /*GIFHDRH*/
