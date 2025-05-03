/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

#ifndef _PCE_X11_INCLUDED
#define _PCE_X11_INCLUDED

#define O_XIM 1				/* Use X input methods */

#define O_XDND 1			/* include Gnome/KDE drag-and-drop */
#define USE_XFONTSET 1			/* Use Xwc* functions */

#ifdef HAVE_XMISMOTIFWMRUNNING
#define O_MOTIF 1
#endif

#ifndef O_PPM
#define O_PPM 1

#define PNM_PNM	0			/* Portable aNy Map */
#define PNM_PBM	1			/* Portable BitMap */
#define PNM_PGM	2			/* Portable GreyMap */
#define PNM_PPM	3			/* Portable PixMap */

#define PNM_ASCII   0
#define PNM_RAWBITS 3
#define PNM_RUNLEN  6
#endif

#define O_GIFWRITE 1

#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>		/* XtConfigureWidget() prototype */
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Shell.h>
#undef index				/* X11 defines this too */

#if XT_VERSION == 10
error XPCE cannot be build for X version 10.  Sorry.
#endif

#ifdef O_XDND
#include "xdnd.h"
#endif

#ifdef USE_XFT
#include <ft2build.h>
#include <X11/Xft/Xft.h>
#endif

#ifdef HAVE_LIBXPM
#include <X11/xpm.h>
#endif


		 /*******************************
		 *	      IMAGES		*
		 *******************************/

typedef enum
{ IMG_OK,				/* Image loaded fine */
  IMG_UNRECOGNISED,			/* Routine didn't recognise it */
  IMG_NOMEM,				/* Not enough memory */
  IMG_INVALID,				/* Something wrong with the image */
  IMG_NO_STATIC_COLOUR			/* true colour routine can't handle */
} img_status;

img_status staticColourReadJPEGFile(Image image, IOSTREAM *fd, XImage **ret);


		 /*******************************
		 *	      FRAME		*
		 *******************************/

typedef struct
{ Widget	widget;
  Window	busy_window;
  int		win_gravity;
  int		check_geometry_when_mapped;
#ifdef O_XIM
  XIC		ic;			/* input context */
  Window	icwin;			/* Window for IC */
#endif
} frame_ws_ref, *FrameWsRef;


		 /*******************************
		 *	     DISPLAY		*
		 *******************************/

typedef struct
{ Display      *display_xref;		/* X-windows reference */
  int		screen;			/* Default screen */
  Visual       *visual;			/* Default visual */
  Colormap	colour_map;		/* Colourmap of the display */
  Widget	shell_xref;		/* Top of widget tree */
  Pixmap	root_bitmap;		/* Bitmap for GC creation */
  DrawContext	pixmap_context;		/* Context for pixmap operations */
  DrawContext	bitmap_context;		/* Context for bitmap operations */
  int		depth;			/* depth of display */
  unsigned long white_pixel;		/* a white pixel */
  unsigned long black_pixel;		/* a black pixel */
  unsigned long foreground_pixel;	/* foreground pixel */
  unsigned long background_pixel;	/* background pixel */
#ifdef O_XDND
  DndClass     *dnd;			/* The DND handler */
  Atom		XdndTextUriList;	/* "text/uri-list" */
#endif
#ifdef O_XIM
  XIM		im;			/* Input method */
#endif
} display_ws_ref, *DisplayWsXref;

#define display_x11_ref(d, field) (((DisplayWsXref)((d)->ws_ref))->field)


		 /*******************************
		 *	      FONTS		*
		 *******************************/

typedef struct xpce_font_info *XpceFontInfo;

#ifdef USE_XFT
struct xpce_font_info
{ XftFont	*xft_font;		/* FontSet structure */
};

#else /*USE_XFT*/

#ifdef USE_XFONTSET

struct xpce_font_info
{ XFontSet	 font_set;		/* FontSet structure */
  char	       **missing;		/* Missing charsets */
  int		nmissing;		/* # missing charsets */
  char	        *def_string;		/* Default for missing glyphs */
};

#else /*USE_XFONTSET*/

typedef char cwidth;

struct xpce_font_info
{ XFontStruct  *info;			/* the X info structure */
  cwidth       *widths;			/* array of ints for the widths */
  int		maxchar;		/* maximum char value */
};

#endif /*USE_XFONTSET*/
#endif /*USE_XFT*/

		/********************************
		*        GRAPHICS CONTEXT	*
		********************************/

struct draw_context
{ Name		kind;			/* Kind of device */

  GC		workGC;			/* Dynamic GC for simple graphicals */
  GC		clearGC;		/* Write current background */
  GC		andGC;			/* Stipple background */
  GC		fillGC;			/* Tile/Stipple area */
  GC		complementGC;		/* Complement an area */
  GC		bitmapGC;		/* Paint bitmap to pixmap */
  GC		copyGC;			/* Simple pixmap copy */
  GC		opGC;			/* Logical area operations */
  GC		shadowGC;		/* Paint shadows */
  GC		reliefGC;		/* Paint opposite of shadow */

  int		pen;			/* Current pen */
  int		depth;			/* #bits per pixel */
  Name		dash;			/* Current dash pattern */
  Name		arcmode;		/* Current arc mode (filling arcs) */
  Any		fill;			/* Image or Colour */
  Image		and_pattern;		/* Current andpattern */
  FontObj	font;			/* Current font */
#ifdef USE_XFT
  XftFont     * xft_font;		/* XFT font representation */
#else
#ifdef USE_XFONTSET
  XFontSet      font_set;		/* font-set description */
#else
  XFontStruct * font_info;		/* X-font for this display */
  wint_t	maxchar;		/* max char value for font */
  cwidth      * char_widths;		/* array with widths of characters */
#endif /*USE_XFONTSET*/
#endif /*USE_XFT*/
  Any		colour;			/* Current colour */
  Any		background;		/* Current background colour */
  unsigned long foreground_pixel;	/* X pixel value of foreground */
  unsigned long background_pixel;	/* X pixel value of background */
  BoolObj		subwindow_mode;		/* Draw in subwindows too */
  BoolObj		invert_mode;		/* Just invert */
  Elevation	elevation;		/* 3-d elevation GC's */
};


		 /*******************************
		 *	    PROTOTYPES		*
		 *******************************/

/* x11-common.c */
COMMON(Widget)		widgetWindow(PceWindow sw);
COMMON(Widget)		widgetFrame(FrameObj fr);
COMMON(Atom)		DisplayAtom(DisplayObj d, Name name);
COMMON(char *)		DisplayAtomToString(DisplayObj d, Atom a);
COMMON(Atom)		FrameAtom(FrameObj fr, Name name);
COMMON(char *)		FrameAtomToString(FrameObj fr, Atom a);
COMMON(Atom)		WmProtocols(FrameObj fr);
COMMON(EventObj)	CtoEvent(Any window, XEvent *event);
COMMON(unsigned long)   getPixelColour(Colour c, DisplayObj d);
COMMON(void)		setXImageImage(Image image, XImage *i);
COMMON(int)		shift_for_mask(unsigned long mask);
COMMON(status)		postscriptXImage(XImage *im, XImage *mask,
					 int x, int y, int w, int h,
					 Display *disp, Colormap cmap,
					 int depth,
					 int colorimage);
COMMON(int)		intensityXColor(XColor *c);
COMMON(void)		x11_set_gc_foreground(DisplayObj d, Any fg,
					      int gcs, GC *gc);
COMMON(void)		x_frame_realize_geometry(FrameObj fr);
#ifdef O_XDND
COMMON(status)		setDndAwareFrame(FrameObj fr);
#endif

/* x11-conversion.c */
COMMON(XImage *)	readImageFile(Image image, IOSTREAM *fd);
COMMON(XImage *)	CreateXImageFromData(unsigned char *data, int w, int h);
COMMON(XImage *)	read_ppm_file(Display *disp, Colormap map,
				      int depth, IOSTREAM *fd);
COMMON(int)		write_pnm_file(IOSTREAM *fd, XImage *img,
				       Display *disp, Colormap cmap,
				       int scale, int fmt, int asascii);
COMMON(int)		write_jpeg_file(IOSTREAM *fd, XImage *img,
					Display *disp, Colormap cmap,
					Image image);
COMMON(int)		write_gif_file(IOSTREAM *fd, XImage *img, XImage *msk,
				       Display *disp, Colormap cmap);
#if X11_GRAPHICS || WIN32_GRAPHICS
COMMON(XImage *)	attachXpmImageImage(Image image, XpmImage *xpm);
#endif
COMMON(unsigned long *)	XImageToRGBA(XImage *img, XImage *msk,
				     Display *disp, Colormap cmap, size_t *lenp);

/* xcolour.c */
COMMON(status)		allocNearestColour(Display *display, Colormap map,
					   int depth, Name vt, XColor *c);

extern XtAppContext ThePceXtAppContext;	/* X toolkit application context */

COMMON(unsigned long*)	ws_image_to_rgba(Image image, Image mask, size_t *lenp);

#endif /*_PCE_X11_INCLUDED*/

