/*
 * Copyright (C) 1989-95 GROUPE BULL
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * GROUPE BULL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of GROUPE BULL shall not be
 * used in advertising or otherwise to promote the sale, use or other dealings
 * in this Software without prior written authorization from GROUPE BULL.
 */

/*****************************************************************************\
* simx.h: 0.1a                                                                *
*                                                                             *
* This emulates some Xlib functionality for MSW. It's not a general solution, *
* it is close related to XPM-lib. It is only intended to satisfy what is need *
* there. Thus allowing to read XPM files under MS windows.                    *
*                                                                             *
* Developed by HeDu 3/94 (hedu@cul-ipn.uni-kiel.de)                           *
\*****************************************************************************/


#ifndef _SIMX_H
#define _SIMX_H

#ifdef FOR_MSW

#include "windows.h"			/* MS windows GDI types */

extern void * boundCheckingMalloc(long s);
extern void * boundCheckingCalloc(long num, long s);
extern void * boundCheckingRealloc(void *p, long s);

/* define MSW types for X window types,
   I don't know much about MSW, but the following defines do the job */

typedef HDC Display;			/* this should be similar */
typedef void *Screen;			/* not used */
typedef void *Visual;			/* not used yet, is for GRAY, COLOR,
					 * MONO */

typedef HPALETTE Colormap;		/* this should be similar */
typedef COLORREF Pixel;

#define PIXEL_ALREADY_TYPEDEFED		/* to let xpm.h know about it */

typedef struct {
    Pixel pixel;
    BYTE red, green, blue;
}      XColor;

typedef struct {
    HBITMAP bitmap;
    unsigned int width;
    unsigned int height;
    unsigned int depth;
}      XImage;

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif
/* some replacements for X... functions */

/* XDefaultXXX */
    extern Visual * XDefaultVisual(Display *display, Screen *screen);
    extern Screen * XDefaultScreen(Display *d);
    extern Colormap XDefaultColormap(Display *display, Screen *screen);
    extern int XDefaultDepth(Display *d, Screen *s);

/* color related */
    extern int XParseColor(Display *, Colormap *, char *, XColor *);
    extern int XAllocColor(Display *, Colormap *, XColor *);
    extern void XQueryColors(Display *display, Colormap colormap,
			      XColor *xcolors, int ncolors);
    extern int XFreeColors(Display *d, Colormap cmap,
			    unsigned long pixels[],
			    int npixels, unsigned long planes);
/* XImage */
    extern XImage * XCreateImage(Display *, Visual *, int depth, int format,
				  int x, int y, int width, int height,
				  int pad, int foo);

/* free and destroy bitmap */
    extern void /* ? */  XDestroyImage(XImage *);
/* free only, bitmap remains */
    extern void XImageFree(XImage *);
#if defined(__cplusplus) || defined(c_plusplus)
} /* end of extern "C" */
#endif /* cplusplus */

#define ZPixmap 1			/* not really used */
#define XYBitmap 1			/* not really used */

#ifndef True
#define True 1
#define False 0
#endif
#ifndef Bool
typedef BOOL Bool;		/* take MSW bool */
#endif

#endif /* def FOR_MSW */

#endif /* _SIMX_H */
