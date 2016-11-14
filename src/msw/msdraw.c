/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2013, University of Amsterdam
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

#include "include.h"
#include "mswin.h"
#include <h/text.h>
#include <math.h>
#ifndef M_PI
#define M_PI (3.141593)
#endif

#define MAX_CLIP_DEPTH (50)		/* clip nesting depth */
#define MAX_CTX_DEPTH (10)		/* Max draw context depth */

typedef struct
{ PceWindow	window;			/* Pce's notion of the window */
  HWND		hwnd;			/* current Windows window */
  HBITMAP	hbitmap;		/* current Image */
  PAINTSTRUCT	ps;			/* paint structure */
  HDC		hdc;			/* device context */
  HPEN		hpen;			/* Current created pen */
  HRGN		hrgn;			/* Current created region */
  HBRUSH	hbrush;			/* Currently selected brush */

  HPEN		ohpen;			/* Original pen */
  HRGN		ohrgn;			/* Original region */
  HBITMAP	ohbitmap;		/* Original bitmap */
  HBRUSH	ohbrush;		/* Original brush */
  int		stockpen;		/* Pen comes from GetStockObject() */
  HFONT		ohfont;			/* Original font */
  HPALETTE	hpal;			/* selected palette */
  HPALETTE	ohpal;			/* Original palette */
  HPALETTE	ochpal;			/* Original cache bitmap palette */

  HBITMAP	cache;			/* background drawing */
  HDC		cached_hdc;		/* hdc of original device */
  int		cache_x;		/* X-corner of cache */
  int		cache_y;		/* Y-corner of cache */
  int		cache_w;		/* Width of cache */
  int		cache_h;		/* Height of cache */
  HBITMAP	cache_ohbitmap;		/* original bitmap handle */

  int		offset_x;		/* d_offset(), r_offset() */
  int		offset_y;		/* same */
  int		r_offset_x;		/* r_offset() */
  int		r_offset_y;		/* r_offset() */
  int		fill_offset_x;		/* filling offset */
  int		fill_offset_y;		/* filling offset */
  int		fixed_colours;		/* The colours are fixed */

  int		open;			/* is context opened? */
  int		invert;			/* paint inverted */

  Image		fill_pattern;		/* PCE fill-pattern image */
  Colour	colour;			/* Current colour */
  Colour	default_colour;		/* The default colour */
  Any		background;		/* Background colour */
  Any		default_background;	/* @default background */
  COLORREF	rgb;			/* RGB of colour */
  COLORREF	background_rgb;		/* RBG of background */
  int		thickness;		/* Current pen */
  Name		texture;		/* Current dash-pattern */
  int		modified_pen;		/* Pen is modified */
  FontObj	font;			/* Currently mounted font */
  WsFont	wsf;			/* Window System Font reference */
  Any		device;			/* XPCE device in use */
  DisplayObj	display;		/* The XPCE display */
  int		depth;			/* # bits/pixel */
  int		transformed;		/* A scaling-mapping is active */
  int		compatible;		/* HDC is compatible to screen */

  Elevation	elevation;		/* current elevation context */
  HPEN		relief_pen;		/* standing-edge pen */
  HPEN		shadow_pen;		/* falling edge pen */


  struct
  { RECT	orect;			/* old clipping rect */
  } clip_stack[MAX_CLIP_DEPTH];
  int		clip_depth;		/* #entries on clip stack */

} wdraw_context, *WDrawContext;

#ifdef __WINDOWS__
#define MoveTo(hdc, x, y) MoveToEx((hdc), (x), (y), NULL);
#define SetWindowOrg(hdc, x, y) SetWindowOrgEx((hdc), (x), (y), NULL);
#define SetViewportOrg(hdc, x, y) SetViewportOrgEx((hdc), (x), (y), NULL);
#endif /*__WINDOWS__*/

static int		cache = 1;	/* Do or don't */
static int		quick;		/* Prefer speed */
static int		has_cmap;	/* Do we have a colourmap? */
static wdraw_context	context;	/* current context */
static wdraw_context	ctx_stack[MAX_CTX_DEPTH];  /* Context stack */
static int		ctx_stacked;	/* Saved frames */
static os_platform	platform;	/* For platform hacks */

static HDC		default_hdc;	/* Default context */
static HBITMAP		default_hdc_hbitmap; /* Memory for default_hdc */
static HBITMAP		default_hdc_ohbitmap; /* Original memory */
static DisplayObj	TheDisplay;	/* @display */
static int		display_depth;	/* depth of the display */

static void	r_update_pen(void);	/* Update the pen context */
static void	r_default_background(Any bg);
static void	push_context(void);
static void	empty_brush_cache(void);
static void	make_default_context(void);
static HBRUSH	r_fillbrush(Any fill);
static int	needWin95FillHack(int x, int y, Any fill);

#include <gra/graphstate.c>

static void
reset_context()
{ context.fill_pattern       = WHITE_IMAGE;
  context.font               = NIL;
  context.thickness          = 1;
  context.texture            = NAME_none;
  context.hbrush	     = 0;
  context.ohbrush	     = 0;
  context.ohpen		     = 0;
  context.ohfont	     = 0;
  context.hpal		     = NULL;
  context.ohpal		     = NULL;
  context.ochpal	     = NULL;
  context.hpen		     = 0;
  context.stockpen	     = FALSE;
  context.colour             = BLACK_COLOUR;
  context.background         = WHITE_COLOUR;	/* is this true? */
  context.default_background = WHITE_COLOUR;
  context.rgb	             = RGB(0, 0, 0);
  context.background_rgb     = RGB(255, 255, 255);
  context.hwnd	             = 0;
  context.window	     = NIL;
  context.hbitmap            = 0;
  context.modified_pen       = FALSE;
  context.open	             = 0;
  context.invert	     = FALSE;
  context.hdc	             = default_hdc;
  context.display            = TheDisplay;
  context.cache	             = 0;
  context.elevation	     = NIL;
  context.relief_pen	     = 0;
  context.shadow_pen	     = 0;
  context.depth		     = display_depth;
  context.r_offset_x	     = 0;
  context.r_offset_y	     = 0;
  context.fixed_colours	     = 0;
  context.compatible	     = TRUE;
}


void
initDraw()
{ platform = ws_platform();
  make_default_context();
  if ( !TheDisplay )
    TheDisplay = CurrentDisplay(NIL);
  if ( !display_depth )
  { display_depth = ws_depth_display(TheDisplay);
    has_cmap = ws_has_colourmap(TheDisplay);
  }

  resetDraw();

  at_pce_exit(exitDraw, ATEXIT_FILO);
}


void
resetDraw()
{ context.open  = 0;
  ctx_stacked   = 0;

  reset_context();
}


static void
make_default_context()
{ if ( !default_hdc )
  { if ( !(default_hdc = CreateCompatibleDC(NULL)) ||
	 !(default_hdc_hbitmap = ZCreateCompatibleBitmap(default_hdc,16,16)) ||
	 !(default_hdc_ohbitmap = ZSelectObject(default_hdc,
						default_hdc_hbitmap)) )
      Cprintf("WARNING: Failed to make scratch context");
					/* TBD: Must be fatal error */
  }

  resetDraw();
}


static void
remove_default_context()
{ if ( default_hdc )
  { ZSelectObject(default_hdc, default_hdc_ohbitmap);
    ZDeleteObject(default_hdc_hbitmap);
    DeleteDC(default_hdc);

    default_hdc = NULL;
  }
}


void
exitDraw(int rval)
{ DisplayObj d = TheDisplay;

  remove_default_context();
					/* Windows frames and windows */
  if ( d && notNil(d) )
  { Cell cell;

    for_cell(cell, d->frames)
      send(cell->value, NAME_uncreate, EAV);
  }

  SetCursor(LoadCursor(NULL, IDC_WAIT));

  closeAllXrefs();
  resetDraw();
}


void
d_offset(int x, int y)
{ DEBUG(NAME_cache, Cprintf("d_offset(%d, %d)\n", x, y));

  context.offset_x = x;
  context.offset_y = y;

  x = -x;
  y = -y;

  if ( context.cache )
  { SetWindowOrg(context.cached_hdc, x, y);
  } else
    SetWindowOrg(context.hdc, x, y);
}


void
r_offset(int x, int y)
{ if ( x == 0 && y == 0 )
    return;				/* very common! */

  if ( !context.cache )
  { d_offset(context.offset_x + x, context.offset_y + y);
  } else
  { POINT old;
    int rval;

    context.r_offset_x += x;
    context.r_offset_y += y;

    DEBUG(NAME_offset, Cprintf("r_offset(%d, %d): vp-offset %d, %d\n",
			       x, y,
			       context.r_offset_x - context.cache_x,
			       context.r_offset_y - context.cache_y));

    rval = SetViewportOrgEx(context.hdc,
			    context.r_offset_x - context.cache_x,
			    context.r_offset_y - context.cache_y,
			    &old);
    assert(rval);
    DEBUG(NAME_offset, Cprintf("\told = %d, %d\n", old.x, old.y));
  }
}


static void
d_set_filloffset()
{ int tsx = (context.fill_offset_x - context.cache_x);
  int tsy = (context.fill_offset_y - context.cache_y);

  if ( context.cache )
  { tsx += context.r_offset_x;
    tsy += context.r_offset_y;
  } else
  { tsx += context.offset_x;
    tsy += context.offset_y;
  }

  SetBrushOrgEx(context.hdc, tsx, tsy, NULL);
}


void
r_filloffset(Point offset, int x0, int y0, fill_state *state)
{ state->x = context.fill_offset_x;
  state->y = context.fill_offset_y;

  if ( notNil(offset) )
  { context.fill_offset_x = valInt(offset->x) + x0;
    context.fill_offset_y = valInt(offset->y) + y0;

    d_set_filloffset();
  }
}


void
r_fillrestore(fill_state *state)
{ if ( state->x != context.fill_offset_x ||
       state->y != context.fill_offset_y )
  { context.fill_offset_x = state->x;
    context.fill_offset_y = state->y;

    d_set_filloffset();
  }
}


DisplayObj
d_display(DisplayObj d)
{ DisplayObj old = context.display;

  if ( isDefault(d) )
    d = CurrentDisplay(NIL);

  if ( context.display != d )
  { openDisplay(d);
    context.display = d;
    quick = (d->quick_and_dirty == ON);
  }


  return old;
}


void
d_ensure_display()
{ if ( context.display == NULL )
    d_display(CurrentDisplay(NIL));
}


void
d_flush(void)
{
}


status
d_mswindow(PceWindow sw, IArea a, int clear)
{ HPALETTE hpal = window_palette(sw);

  push_context();

  context.window	 = sw;
  context.hwnd           = getHwndWindow(sw);
  context.hdc            = BeginPaint(context.hwnd, &context.ps);
  context.device         = sw;
  context.default_colour = sw->colour;
  context.hpal		 = hpal;
  context.open++;

  if ( !IsRectEmpty(&context.ps.rcPaint) )
  { RECT *r = &context.ps.rcPaint;

    a->x = r->left   - valInt(sw->scroll_offset->x);
    a->y = r->top    - valInt(sw->scroll_offset->y);
    a->w = r->right  - r->left;
    a->h = r->bottom - r->top;

    if ( hpal )
    { int mapped;
      context.ohpal = SelectPalette(context.hdc, hpal, FALSE);
      if ( !context.ohpal )
	Cprintf("Failed to select palette: %s\n",
		strName(WinStrError(GetLastError())));
      mapped = RealizePalette(context.hdc);
      DEBUG(NAME_colourMap,
	    Cprintf("%s: added %d colours\n", pp(sw), mapped));
      if ( mapped == GDI_ERROR )
	Cprintf("RealizePalette(): %s\n", strName(WinStrError(GetLastError())));
    }

    if ( cache && clear )
    { context.cached_hdc     = context.hdc;
      context.ochpal	     = context.ohpal;
      context.cache_x        = a->x;
      context.cache_y        = a->y;
      context.cache_w        = a->w + 1;
      context.cache_h        = a->h + 1;
      context.cache	     = ZCreateCompatibleBitmap(context.hdc,
						       context.cache_w,
						       context.cache_h);
      context.hdc            = CreateCompatibleDC(context.hdc);
      context.cache_ohbitmap = ZSelectObject(context.hdc, context.cache);
      if ( context.hpal )
	context.ohpal = SelectPalette(context.hdc, hpal, FALSE);

      r_default_background(sw->background);
      SetViewportOrg(context.hdc, -context.cache_x, -context.cache_y);
      SetBrushOrgEx(context.hdc, -context.cache_x, -context.cache_y, NULL);

      r_clear(context.cache_x, context.cache_y,
	      context.cache_w, context.cache_h);

      DEBUG(NAME_cache, Cprintf("%s: created cache for %d %d %d %d\n",
				pp(sw),
				context.cache_x, context.cache_y,
				context.cache_w, context.cache_h));
    } else
    { context.cache_x = 0;
      context.cache_y = 0;
      r_default_background(sw->background);
//    SetViewportOrg(context.hdc, 0, 0);
//    SetBrushOrgEx(context.hdc, 0, 0, NULL);
      if ( clear )
	r_clear(a->x, a->y, a->w, a->h);
    }

    SetBkMode(context.hdc, TRANSPARENT);

    succeed;
  } else
  { DEBUG(NAME_redraw, Cprintf("Empty rect: (ltrb) %d-%d %d-%d\n",
			       context.ps.rcPaint.left,
			       context.ps.rcPaint.top,
			       context.ps.rcPaint.right,
			       context.ps.rcPaint.bottom));
  }

  fail;
}


status
d_window(PceWindow sw, int x, int y, int w, int h, int clear, int limit)
{ DisplayObj d = getDisplayGraphical((Graphical)sw);

  if ( !d )
    fail;

  d_display(d);

  if ( !context.open++ )
  { push_context();

    context.window	       = sw;
    context.hwnd               = getHwndWindow(sw);
    context.hdc                = BeginPaint(context.hwnd, &context.ps);
    context.default_colour     = sw->colour;
    context.background	       = sw->background;
    context.default_background = sw->background;

    if ( clear )
      r_clear(x, y, w, h);
  }

  succeed;
}


static void
push_context()
{ if ( context.open )
    ctx_stack[ctx_stacked++] = context;
  if ( ctx_stacked >= MAX_CTX_DEPTH )
    Cprintf("**************** ERROR: Draw Context Stack overflow\n");

  reset_context();
}


void
d_image(Image i, int x, int y, int w, int h)
{ Colour background;
  DisplayObj d = CurrentDisplay(NIL);
  HPALETTE hpal;

  if ( notNil(d->colour_map) && notDefault(d->colour_map) )
    hpal = getPaletteColourMap(d->colour_map);
  else
    hpal = NULL;

  push_context();

  DEBUG(NAME_redraw, Cprintf("d_image(%s, %d, %d, %d, %d)\n",
			     pp(i), x, y, w, h));

  context.open++;
  d_display(notNil(i->display) ? i->display : DEFAULT);
  context.device = i;

  context.hbitmap  = (HBITMAP) getXrefObject(i, context.display);
  context.hdc      = CreateCompatibleDC(NULL);
  context.ohbitmap = ZSelectObject(context.hdc, context.hbitmap);
  context.depth    = valInt(i->depth);
  context.hpal     = hpal;

  if ( hpal )
  { int mapped;
    context.ohpal = SelectPalette(context.hdc, hpal, FALSE);
    if ( !context.ohpal )
      Cprintf("Failed to select palette: %s\n",
	      strName(WinStrError(GetLastError())));
    mapped = RealizePalette(context.hdc);
    DEBUG(NAME_colourMap,
	  Cprintf("%s: added %d colours\n", pp(i), mapped));
    if ( mapped == GDI_ERROR )
      Cprintf("RealizePalette(): %s\n", strName(WinStrError(GetLastError())));
  }

  if ( x != 0 || y != 0 || w != valInt(i->size->w) || h != valInt(i->size->h) )
  { HRGN clip_region = ZCreateRectRgn(x, y, x+w, y+h);

    ZSelectObject(context.hdc, clip_region);
    ZDeleteObject(clip_region);
  }

  if ( notDefault(i->foreground) )
    context.default_colour = i->foreground;
  else
  { if ( i->kind == NAME_bitmap )
      context.default_colour = BLACK_COLOUR;
    else
      context.default_colour = context.display->foreground;
  }

  if ( notDefault(i->background) )
    background = i->background;
  else
  { if ( i->kind == NAME_bitmap )
      background = WHITE_COLOUR;
    else
      background = context.display->background;
  }

  SetBkMode(context.hdc, TRANSPARENT);
  r_default_background(background);
  r_colour(DEFAULT);
}


void
d_hdc(HDC hdc, Colour fg, Colour bg, int compatible)
{ push_context();

  d_display(DEFAULT);
  if ( isDefault(fg) )
    fg = context.display->foreground;
  if ( isDefault(bg) )
    bg = context.display->background;

  context.open++;
  context.hdc = hdc;
  context.device = NIL;			/* anonymous device */
  context.compatible = compatible;

  if ( notNil(bg) )
    r_default_background(bg);
  if ( notNil(fg) )
    r_default_colour(fg);
}


void
d_screen(DisplayObj d)
{ HDC hdc = GetDC(NULL);

  d_hdc(hdc, DEFAULT, DEFAULT, TRUE);
}


status
d_winmf(const wchar_t *fn, int x, int y, int w, int h, const wchar_t *descr)
{ HDC hdc;
  RECT bb;
  HDC refdc = GetDC(NULL);
  int hmm   = GetDeviceCaps(refdc, HORZSIZE);
  int hpxl  = GetDeviceCaps(refdc, HORZRES);
  int vmm   = GetDeviceCaps(refdc, VERTSIZE);
  int vpxl  = GetDeviceCaps(refdc, VERTRES);

  bb.left   = (x*hmm*100)/hpxl;			/* bb in .01 mm units */
  bb.right  = ((x+w)*hmm*100+hpxl+20)/hpxl;	/* bit too big for MS bugs */
  bb.top    = (y*vmm*100)/vpxl;
  bb.bottom = ((y+h)*vmm*100+vpxl+20)/vpxl;

  DEBUG(NAME_winMetafile,
	Cprintf("BB in pixels = %d %d %d %d\n"
		"hmm = %d, hpxl = %d, vmm = %d, vpxl = %d\n"
		"BB in .01 mm = %d,%d,%d,%d\n",
		x, y, x+w, y+h,
		hmm, hpxl, vmm, vpxl,
		bb.left, bb.top, bb.right, bb.bottom));

  DEBUG(NAME_winMetafile,
	Cprintf("BB in .01 mm = %d,%d,%d,%d\n",
		bb.left, bb.top, bb.right, bb.bottom));

  if ( (hdc = CreateEnhMetaFileW(refdc,		/* HDC reference */
				 fn,		/* name of the metafile */
				 &bb,		/* bounding box */
//				 NULL,		/* or let GDI compute BB */
				 descr)) )
  { ReleaseDC(NULL, refdc);
    d_hdc(hdc, DEFAULT, DEFAULT, FALSE);
    SetBkMode(hdc, TRANSPARENT);

    succeed;
  }

  Cprintf("CreateEnhMetaFile() failed\n");
  fail;
}


HENHMETAFILE
d_winmfdone()
{ HENHMETAFILE hmf = CloseEnhMetaFile(context.hdc);

  d_done();

  return hmf;
}


HDC
d_current_hdc()
{ return context.hdc;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Clipping is nasty in MS-Windows.  The   task of the pair r_clip(x,y,w,h)
and r_clip_done() is to set the clipping   area  of the device (given in
the current coordinate system determined   by d_offset() and r_offset())
and revert it back to the old clipping   area.  Clipping is used both by
class device if `device<-clip_area' is  present   and  by  class text to
handle scrolled text.

In Windows, clipping is established by   creating a region and selecting
it  into  the  current  device  context.   The  region  is  in  *device*
coordinates.  Unlike for the  other   graphical  attributes, selecting a
clipping region does  *not*  return  a   handle  to  the  old situation.
Therefore we use GetClipBox() to  find   the  clipping  region before we
started clipping.  But ...  the region   returned  by GetClipBox() is in
*logical* coordinates!  Hence all the offsets.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
d_clip(int x, int y, int w, int h)
{ if ( context.clip_depth < MAX_CLIP_DEPTH )
  { RECT *rect = &context.clip_stack[context.clip_depth].orect;
/*  HRGN hrgn; */

    GetClipBox(context.hdc, rect);

    DEBUG(NAME_clip, { Cprintf("d_clip(%d %d %d %d): ", x, y, w, h);
		       Cprintf("ClipBox = %d %d %d %d --> ",
			       rect->left, rect->top,
			       rect->right - rect->left,
			       rect->bottom - rect->top);
		     });

#if 1
  { POINT pts[5];
    pts[0].x = x;   pts[0].y = y;
    pts[1].x = x+w; pts[1].y = y;
    pts[2].x = x+w; pts[2].y = y+h;
    pts[3].x = x;   pts[3].y = y+h;
    pts[4].x = x;   pts[4].y = y;

    if ( !BeginPath(context.hdc) ||
	 !Polygon(context.hdc, pts, 5) ||
	 !EndPath(context.hdc) ||
	 !SelectClipPath(context.hdc, RGN_AND) )
      Cprintf("Failed to set clip-path\n");
  }
#else
    if ( context.cache )
    { x += context.r_offset_x - context.cache_x; /* TRY ... */
      y += context.r_offset_y - context.cache_y;
    } else if ( context.hwnd )
    { POINT offset;

      GetWindowOrgEx(context.hdc, &offset);
      x -= offset.x;
      y -= offset.y;
    }

    hrgn = ZCreateRectRgn(x, y, x+w, y+h);
    ZSelectObject(context.hdc, hrgn);
    ZDeleteObject(hrgn);
#endif

    DEBUG(NAME_clip, { RECT nrect;
		       GetClipBox(context.hdc, &nrect);
		       Cprintf("%d %d %d %d\n",
			       nrect.left, nrect.top,
			       nrect.right - nrect.left,
			       nrect.bottom - nrect.top);
		     });

    context.clip_depth++;
  } else
    sysPce("Too many levels of clipping");
}


void
d_done(void)
{ if ( --context.open == 0 )
  { DEBUG(NAME_redraw,
	  Cprintf("d_done(%s)\n",
		  context.window ? pp(context.window) : "(image)"));

    if ( context.ohbrush )
    { ZSelectObject(context.hdc, context.ohbrush);
      context.hbrush = 0;
      context.ohbrush = 0;
    }
    empty_brush_cache();
    if ( context.hpen )
    { ZSelectObject(context.hdc, context.ohpen);
      if ( !context.stockpen )
	ZDeleteObject(context.hpen);
      context.hpen = 0;
    }
    if ( context.ohfont )
    { ZSelectObject(context.hdc, context.ohfont);
      context.ohfont = 0;
    }
    if ( context.relief_pen )
    { ZDeleteObject(context.relief_pen);
      context.relief_pen = 0;
    }
    if ( context.shadow_pen )
    { ZDeleteObject(context.shadow_pen);
      context.shadow_pen = 0;
    }

    if ( context.cache )
    { DEBUG(NAME_cache, Cprintf("Writing cache to window\n"));
      SetViewportOrg(context.hdc, 0, 0);
      BitBlt(context.cached_hdc,
	     context.cache_x, context.cache_y,
	     context.cache_w, context.cache_h,
	     context.hdc, 0, 0, SRCCOPY);
      ZSelectObject(context.hdc, context.cache_ohbitmap);
      ZDeleteObject(context.cache);
      if ( context.ochpal )
      { SelectPalette(context.cached_hdc, context.ochpal, FALSE);
	context.ochpal = NULL;
      }
      if ( context.ohpal )
      { SelectPalette(context.hdc, context.ohpal, FALSE);
	context.ohpal = NULL;
      }
      DeleteDC(context.hdc);
    } else
    { if ( context.ohpal )
      { SelectPalette(context.hdc, context.ohpal, FALSE);
	context.ohpal = NULL;
      }
    }

    if ( instanceOfObject(context.device, ClassWindow) )
    { EndPaint(context.hwnd, &context.ps);
    } else if ( instanceOfObject(context.device, ClassImage) )
    { ZSelectObject(context.hdc, context.ohbitmap);
      DeleteDC(context.hdc);
    } else				/* d_hdc() context */
    { ;
    }

    if ( ctx_stacked )
      context = ctx_stack[--ctx_stacked];
    else
      reset_context();
  }
}


void
d_clip_done(void)
{ RECT *rect;
/*HRGN hrgn;*/

  if ( context.clip_depth-- < 0 )
    sysPce("Clip stack underfow!");

  rect = &context.clip_stack[context.clip_depth].orect;
  DEBUG(NAME_clip,  Cprintf("d_clip_done(%d %d %d %d) --> ",
			    rect->left, rect->top,
			    rect->right - rect->left,
			    rect->bottom - rect->top));

#if 1
{ POINT pts[5];
  pts[0].x = rect->left;  pts[0].y = rect->top;
  pts[1].x = rect->right; pts[1].y = rect->top;
  pts[2].x = rect->right; pts[2].y = rect->bottom;
  pts[3].x = rect->left;  pts[3].y = rect->bottom;
  pts[4].x = rect->left;  pts[4].y = rect->top;

  if ( !BeginPath(context.hdc) ||
       !Polygon(context.hdc, pts, 5) ||
       !EndPath(context.hdc) ||
       !SelectClipPath(context.hdc, RGN_COPY) )
    Cprintf("Failed to set UN-clip-path\n");
}

#else
  if ( context.cache )
  { ox = context.r_offset_x - context.cache_x;
    oy = context.r_offset_y - context.cache_y;
  } else if ( context.hwnd )
  { POINT offset;

    GetWindowOrgEx(context.hdc, &offset);
    ox = -offset.x;
    oy = -offset.y;
  }
  rect->left   += ox;
  rect->top    += oy;
  rect->right  += ox;
  rect->bottom += oy;

  hrgn = ZCreateRectRgnIndirect(rect);
  ZSelectObject(context.hdc, hrgn);
  ZDeleteObject(hrgn);
#endif

  DEBUG(NAME_clip, { RECT nrect;
		     GetClipBox(context.hdc, &nrect);
		     Cprintf("%d %d %d %d\n",
			     nrect.left, nrect.top,
			     nrect.right, nrect.bottom);
		   });
}


void
intersection_iarea(IArea a, IArea b)
{ int x, y, w, h;

  x = (a->x > b->x ? a->x : b->x);
  y = (a->y > b->y ? a->y : b->y);
  w = (a->x + a->w < b->x + b->w ? a->x + a->w : b->x + b->w) - x;
  h = (a->y + a->h < b->y + b->h ? a->y + a->h : b->y + b->h) - y;

  if ( w < 0 ) w = 0;
  if ( h < 0 ) h = 0;

  a->x = x;
  a->y = y;
  a->w = w;
  a->h = h;
}


void
r_clear(int x, int y, int w, int h)
{ r_fill(x, y, w, h, context.background);
}


void
r_complement(int x, int y, int w, int h)
{ RECT rect;

  rect.left   = x;
  rect.right  = x + w;
  rect.top    = y;
  rect.bottom = y + h;

  InvertRect(context.hdc, &rect);
}


void
r_and(int x, int y, int w, int h, Image pattern)
{ HBITMAP bm = (HBITMAP) getXrefObject(pattern, context.display);
  HBRUSH brush = ZCreatePatternBrush(bm);
  HBRUSH obrush = ZSelectObject(context.hdc, brush);

  PatBlt(context.hdc, x, y, w, h, 0xFA0089); /* P|D */

  ZSelectObject(context.hdc, obrush);
  ZDeleteObject(brush);
}

static const DWORD dotted[]	= { 1, 2 };
static const DWORD dashed[]     = { 7, 7 };
static const DWORD dashdot[]    = { 7, 3, 1, 7 };
static const DWORD dashdotted[] = { 9, 3, 1, 3, 1, 3, 1, 3 };
static const DWORD longdash[]   = { 13, 7 };

static struct dashpattern
{ Name	       dash;
  DWORD        style;			/* non-PS_USERSTYLE style */
  const DWORD *dash_list;
  int	       dash_list_length;
} dash_patterns[] =
{ { NAME_none,		PS_SOLID,	NULL,		0},
  { NAME_dotted,	PS_ALTERNATE,	dotted,		2},
  { NAME_dashed,	PS_DOT,		dashed,		2},
  { NAME_dashdot,	PS_DASHDOT,	dashdot,	4},
  { NAME_dashdotted,	PS_DASHDOTDOT,	dashdotted,	8},
  { NAME_longdash,	PS_DASH,	longdash,	2},
  { 0,			PS_SOLID,	NULL,		0},
};


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
It appears you cannot create a NULL_PEN using CreatePen(). As we need to
use stock pens anyway, I decided to use   one  for the very common solid
1-thick  black  pen  too,  assuming    GetStockObject()  will  use  less
resources. In this  implementation,  we   will  call  DeleteObject() for
stock-objects. Should this be done or not?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
r_update_pen()
{ if ( context.modified_pen )
  { HPEN org, old = (!context.stockpen ? context.hpen : 0);

    if ( context.thickness <= 0 )
    { context.hpen = GetStockObject(NULL_PEN);
      context.stockpen = TRUE;
    } else
    { if ( context.texture == NAME_none &&
	   context.thickness == 1 &&
	   context.rgb == RGB(0,0,0) &&
	   !context.transformed )
      { context.hpen = GetStockObject(BLACK_PEN);
	context.stockpen = TRUE;
      } else
      { struct dashpattern *pat;

	context.stockpen = FALSE;

	for(pat=dash_patterns; pat->dash; pat++)
	{ if ( pat->dash == context.texture )
	    break;
	}

	if ( ( context.thickness <= 1 &&
	       !context.transformed &&
	       pat->style != PS_ALTERNATE ) ||
	     ( pat->style == PS_SOLID ) ||
	     ( platform < NT && (context.thickness > 1 ||
				 context.transformed) ) )
	{ DEBUG(NAME_pen, Cprintf("Using CreatePen()\n"));
	  context.hpen = ZCreatePen(pat->style,
				    context.thickness,
				    context.rgb);
	} else
	{ LOGBRUSH lbrush;
	  int style;

	  lbrush.lbStyle = BS_SOLID;
	  lbrush.lbColor = context.rgb;
	  lbrush.lbHatch = 0L;

	  if ( platform < NT )
	  { if ( pat->style == PS_ALTERNATE )
	    { context.hpen = ZCreatePen(PS_DOT,
					context.thickness,
					context.rgb);
	    } else
	    { style = PS_COSMETIC;
	      style |= pat->style;

	      context.hpen = ZExtCreatePen(style,
					   1,
					   &lbrush,
					   0,
					   NULL);
	    }
	  } else
	  { style = PS_GEOMETRIC|PS_ENDCAP_FLAT;

	    if ( pat->dash_list_length )
	      style |= PS_USERSTYLE;
	    else
	      style |= pat->style;

	    context.hpen = ZExtCreatePen(style,
					 context.thickness,
					 &lbrush,
					 pat->dash_list_length,
					 pat->dash_list);
	  }
	}
      }
    }

    org = ZSelectObject(context.hdc, context.hpen);
    if ( !context.ohpen )
      context.ohpen = org;

    if ( old )
      ZDeleteObject(old);

    context.modified_pen = FALSE;
  }
}


void
r_thickness(int pen)
{ if ( context.thickness != pen )
  { context.modified_pen = TRUE;
    context.thickness = pen;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Indicate there is a non-unit mapping  to   the  device.  This is used to
avoid speedups based on  this  assumption,   for  example  the  usage of
cosmetic pens over geometric pens.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
r_transformed(int val)
{ int old = context.transformed;

  context.transformed = (val ? TRUE : FALSE);

  return old;
}


void
r_dash(Name dash)
{ if ( context.texture != dash )
  { context.modified_pen = TRUE;
    context.texture	 = dash;
  }
}


Any
r_colour(Any colour)
{ Any old = context.colour;

  if ( context.fixed_colours )
    return old;

  if ( isDefault(colour) )
  { assert(notDefault(context.default_colour));
    colour = context.default_colour;
  } else if ( !instanceOfObject(colour, ClassColour) )
    colour = getReplacementColourPixmap(colour);

  if ( context.colour != colour )
  { context.modified_pen = TRUE;
    context.colour       = colour;
    context.rgb		 = cref_colour(colour);
    SetTextColor(context.hdc, context.rgb);
  }

  return old;
}

#define BRUSH_CACHE_SIZE 5

typedef struct
{ Any	 object;			/* object for brush */
  HBRUSH brush;				/* associated brush */
  int	 times;				/* # times used */
} brush_cache_element;

brush_cache_element brush_cache[BRUSH_CACHE_SIZE];

static HBRUSH
lookup_brush(Any fill)
{ int i;
  brush_cache_element *e;

  for(i=0, e=brush_cache; i<BRUSH_CACHE_SIZE; i++, e++)
  { if ( e->object == fill )
    { e->times++;
      return e->brush;
    }
  }

  return (HBRUSH)0;
}


static void
empty_brush_cache()
{ int i;
  brush_cache_element *e;

  DEBUG(NAME_fill, Cprintf("empty_brush_cache()\n"));

  for(i=0, e=brush_cache; i<BRUSH_CACHE_SIZE; i++, e++)
  { if ( e->brush )
    { ZDeleteObject(e->brush);
      e->brush = (HBRUSH)0;
    }
    e->object = NULL;
    e->times = 0;
  }
}


static void
add_brush(Any fill, HBRUSH brush)
{ int i;
  brush_cache_element *e;
  int leastusage;
  brush_cache_element *leastused;

  DEBUG(NAME_fill, Cprintf("add_brush(%s, 0x%x) ... ", pp(fill), brush));

  for(i=0, e=brush_cache; i<BRUSH_CACHE_SIZE; i++, e++)
  { if ( !e->object )
    { e->object = fill;
      e->brush  = brush;
      e->times  = 1;
      DEBUG(NAME_fill, Cprintf("ok\n"));
      return;
    }
  }

  leastusage = 1000000;
  leastused  = NULL;

  for(i=0, e=brush_cache; i<BRUSH_CACHE_SIZE; i++, e++)
  { if ( e->brush != context.hbrush )
    { if ( !leastused || e->times < leastusage )
      { leastusage = e->times;
	leastused  = e;
      }
    }
  }

  assert(leastused);
  DEBUG(NAME_fill, Cprintf("replaced %s --> 0x%x\n",
			   pp(leastused->object), leastused->brush));

  ZDeleteObject(leastused->brush);
  leastused->object = fill;
  leastused->brush  = brush;
  leastused->times  = 1;
}


static HashTable
winBrushTable()
{ static HashTable table;

  if ( !table )
  { table = createHashTable(toInt(16), NAME_none);

    declareWindowsBrush(NIL,     GetStockObject(NULL_BRUSH));
    declareWindowsBrush(DEFAULT, GetStockObject(NULL_BRUSH)); /* play safe */
  }

  return table;
}


void
declareWindowsBrush(Any obj, HBRUSH brush)
{ Int b = toInt((intptr_t)brush);

  assert((HBRUSH) valInt(b) == brush);
  appendHashTable(winBrushTable(), obj, b);
}


HBRUSH
standardWindowsBrush(Any obj)
{ Int b;

  if ( (b = getMemberHashTable(winBrushTable(), obj)) )
    return (HBRUSH) valInt(b);

  return 0;
}


static HBRUSH
r_fillbrush(Any fill)
{ HBRUSH hbrush;

  if ( isDefault(fill) )
    fill = context.colour;
  else if ( fill == NAME_current )
    fill = context.fill_pattern;

  if ( !(hbrush = standardWindowsBrush(fill)) )
  { if ( !(hbrush = lookup_brush(fill)) )
    { if ( instanceOfObject(fill, ClassImage) )
      { Image img = fill;
	HBITMAP bm = (HBITMAP) getXrefObject(fill, context.display);

	if ( (valInt(img->size->w) < 8 || valInt(img->size->h) < 8) &&
	     platform < NT )
	{ int sw = valInt(img->size->w);
	  int sh = valInt(img->size->h);
	  int fw, fh;
	  HBITMAP bm2;
	  HDC hdc = CreateCompatibleDC(context.hdc);
	  HDC mhdc = CreateCompatibleDC(context.hdc);
	  HPALETTE opal1 = 0, opal2 = 0;
	  HBITMAP obm1, obm2;
	  int x = 0, y = 0;

	  if ( platform == WIN95 )
	  { fw = fh = 8;
	  } else
	  { fw = max(sw, 8);
	    fh = max(sh, 8);
	  }
	  bm2 = ZCreateCompatibleBitmap(context.hdc, fw, fh);

	  DEBUG(NAME_fill, Cprintf("Created %dx%d fill image\n", fw, fh));

	  if ( context.hpal )
	  { opal1 = SelectPalette(hdc, context.hpal, FALSE);
	    opal2 = SelectPalette(mhdc, context.hpal, FALSE);
	  }
	  obm1 = ZSelectObject(hdc, bm2);
	  obm2 = ZSelectObject(mhdc, bm);

	  for(x=0; x<fw; x+=sw)
	  { for(y=0; y<fh; y+=sh)
	      BitBlt(hdc, x, y, sw, sh, mhdc, 0, 0, SRCCOPY);
	  }
	  ZSelectObject(hdc, obm1);
	  ZSelectObject(mhdc, obm2);
	  if ( context.hpal )
	  { SelectPalette(hdc, opal1, FALSE);
	    SelectPalette(mhdc, opal2, FALSE);
	  }
	  DeleteDC(hdc);
	  DeleteDC(mhdc);
	  hbrush = ZCreatePatternBrush(bm2);
	  ZDeleteObject(bm2);
	} else
	  hbrush = ZCreatePatternBrush(bm);

	if ( !hbrush )
	  Cprintf("Warning: failed to get brush\n");
      } else /* instanceOfObject(fill, ClassColour) */
      { COLORREF rgb = cref_colour(fill);

	hbrush = ZCreateSolidBrush(rgb);
      }

      add_brush(fill, hbrush);
    }
  }

  return hbrush;
}


void
r_fillpattern(Any fill, Name which)			/* colour or image */
{ if ( context.fixed_colours && !instanceOfObject(fill, ClassImage) )
    fill = (which == NAME_foreground ? context.colour
				     : context.background);

  if ( isDefault(fill) )
    fill = context.colour;
  else if ( fill == NAME_current )
    return;

  if ( context.fill_pattern != fill )
  { HBRUSH new, old;

    new = r_fillbrush(fill);
    context.hbrush = new;
    old = ZSelectObject(context.hdc, new);
    if ( !context.ohbrush )
      context.ohbrush = old;

    context.fill_pattern = fill;
  }
}


void
r_arcmode(Name mode)
{					/* handled by r_msarc() itself */
}


void
r_fix_colours(Any fg, Any bg, ColourContext ctx)
{ ctx->foreground = context.colour;
  ctx->background = context.background;
  ctx->lock	  = context.fixed_colours;

  if ( !context.fixed_colours )
  { if ( !fg || isNil(fg) ) fg = DEFAULT;
    if ( !bg || isNil(bg) ) bg = DEFAULT;

    r_default_colour(fg);
    r_background(bg);
  }

  context.fixed_colours++;
}


void
r_unfix_colours(ColourContext ctx)
{ if ( (context.fixed_colours = ctx->lock) == 0 )
  { r_default_colour(ctx->foreground);
    r_background(ctx->background);
  }
}


Any
r_default_colour(Any c)
{ Any old = context.default_colour;

  if ( !context.fixed_colours )
  { if ( notDefault(c) )
    { if ( !instanceOfObject(c, ClassColour) )
	c = getReplacementColourPixmap(c);

      context.default_colour = c;
    }

    r_colour(context.default_colour);
  }

  return old;
}


Any
r_background(Any c)
{ Any old = context.background;

  if ( context.fixed_colours )
    return old;

  if ( isDefault(c) )
  { c = context.default_background;
    DEBUG(NAME_background, Cprintf("Using default background %s\n", pp(c)));
  }

  if ( context.background != c )
  { COLORREF rgb;

    if ( instanceOfObject(c, ClassColour) )
    { rgb = cref_colour(c);
    } else
    { Colour colour = getReplacementColourPixmap(c);
      rgb = cref_colour(colour);
    }

    SetBkColor(context.hdc, rgb);
    context.background     = c;
    context.background_rgb = rgb;
  }

  return old;
}


static void
r_default_background(Any bg)
{ r_background(bg);
  context.default_background = context.background;

  DEBUG(NAME_background, Cprintf("r_default_background(%s)\n", pp(bg)));
}


void
r_swap_background_and_foreground(void)
{ Colour tc = context.background;

  r_background(context.colour);
  r_colour(tc);
}


BoolObj
r_subwindow_mode(BoolObj val)
{ return OFF;
}


void
r_invert_mode(BoolObj val)
{ context.invert = (val == ON);
}


void
r_translate(int x, int y, int *ox, int *oy)
{
}


void
r_box(int x, int y, int w, int h, int r, Any fill)
{ int maxr = min(abs(w), abs(h))/2;

  r = min(r, maxr);

  if ( notNil(fill) && r == 0 && needWin95FillHack(x, y, fill) )
  { r_fill(x, y, w, h, fill);
    fill = NIL;
  }

  if ( context.thickness > 0 || notNil(fill) )
  { if ( context.thickness > 0 || r > 1 )
    { int da = context.thickness / 2;
      int db = max(0, (context.thickness - 1) / 2);

      DEBUG(NAME_pen, Cprintf("context.thickness = %d\n", context.thickness));
      x += da;    y += da;
      w -= da+db; h -= da+db;

      if ( w < 2 || h < 2 )
	return;				/* TBD: too small (make line) */

      r_fillpattern(fill, NAME_background);
      r_update_pen();

      if ( r == 0 )
      { Rectangle(context.hdc, x, y, x+w, y+h);
      } else
      { RoundRect(context.hdc, x, y, x+w, y+h, r*2, r*2);
      }
    } else
    { r_fill(x, y, w, h, fill);
    }
  }
}


void
r_shadow_box(int x, int y, int w, int h, int r, int shadow, Image fill)
{ NormaliseArea(x,y,w,h);

  if ( !shadow )
  { r_box(x, y, w, h, r, fill);
  } else
  { if ( shadow > h ) shadow = h;
    if ( shadow > w ) shadow = w;

    r_colour(BLACK_COLOUR);
    r_box(x+shadow, y+shadow, w-shadow, h-shadow, r, BLACK_IMAGE);
    r_colour(DEFAULT);
    r_box(x, y, w-shadow, h-shadow, r, isNil(fill) ? WHITE_IMAGE : fill);
  }
}


COLORREF
cref_colour(Colour c)
{ COLORREF r;

  if ( !(r = (COLORREF)(intptr_t) getExistingXrefObject(c, context.display)) )
  { r = (COLORREF)(intptr_t) getXrefObject(c, context.display);
    if ( context.hpal )
      RealizePalette(context.hdc);
  }

  if ( r & EXACT_COLOUR_MASK )
    return r &= ~EXACT_COLOUR_MASK;
  else
  { if ( context.hpal )
    { int n = GetNearestPaletteIndex(context.hpal, r);

      return PALETTEINDEX(n);
    }

    return GetNearestColor(context.hdc, r);
  }
}


Any
r_elevation_shadow(Elevation e)
{ if ( isDefault(e->shadow) )
  { Any bg = context.background;

    if ( instanceOfObject(bg, ClassColour) && context.depth != 1 )
      return getReduceColour(bg, DEFAULT);
    else
      return BLACK_COLOUR;
  } else
    return e->shadow;
}

static Any
r_elevation_relief(Elevation e)
{ if ( isDefault(e->relief) )
  { Any bg = context.background;

    if ( instanceOfObject(bg, ClassColour) && context.depth != 1 )
      return getHiliteColour(bg, DEFAULT);
    else
      return WHITE_COLOUR;
  } else
    return e->relief;
}

static void
r_elevation(Elevation e)
{ if ( context.elevation != e )
  { Any relief = r_elevation_relief(e);
    Any shadow = r_elevation_shadow(e);

    if ( context.relief_pen )
      ZDeleteObject(context.relief_pen);
    if ( context.shadow_pen )
      ZDeleteObject(context.shadow_pen);

    context.relief_pen = ZCreatePen(PS_SOLID, 1, cref_colour(relief));
    context.shadow_pen = ZCreatePen(PS_SOLID, 1, cref_colour(shadow));

    DEBUG(NAME_elevation, Cprintf("ok\n"));

    context.elevation = e;
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
r_elevation_fillpattern(Elevation e, int up)
    Sets the fill-pattern for the interior of elevated areas and returns
    TRUE if the interior needs to be filled.  Returns FALSE otherwise.
    The special colours `reduced' and `hilited' are interpreted as relative
    colours to the background.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
r_elevation_fillpattern(Elevation e, int up)
{ Any fill = NIL;

  if ( up && notDefault(e->colour) )
  { fill = e->colour;
  } else if ( !up && notDefault(e->background) )
  { fill = e->background;
  }

  if ( isNil(fill) )
    fail;

  if ( fill == NAME_reduced || fill == NAME_hilited )
  { Any bg = context.background;

    if ( instanceOfObject(bg, ClassColour) && context.depth != 1 )
    { if ( fill == NAME_reduced )
	fill = getReduceColour(bg, DEFAULT);
      else
	fill = getHiliteColour(bg, DEFAULT);
    } else
      fail;
  }

  r_fillpattern(fill, NAME_background);

  succeed;
}


typedef struct
{ int x1, y1;				/* start of line */
  int x2, y2;				/* end of line */
} lsegment;


typedef struct
{ int x, y;
  int width, height;
  int angle1, angle2;
} larc;


static void
draw_segments(lsegment *s, int n, HPEN pen)
{ HPEN old = ZSelectObject(context.hdc, pen);

  for( ; n > 0; n--, s++ )
  { MoveTo(context.hdc, s->x1, s->y1);
    LineTo(context.hdc, s->x2, s->y2);
  }

  ZSelectObject(context.hdc, old);
}


void
r_3d_segments(int n, ISegment s, Elevation e, int light)
{ HPEN old;
  int i, x = 0, y = 0;

  r_elevation(e);

  old = ZSelectObject(context.hdc, light ? context.relief_pen
					 : context.shadow_pen);
  for(i=0; i<n; i++, s++)
  { if ( i == 0 || x != s->x1 || y != s->y1 )
      MoveTo(context.hdc, s->x1, s->y1);
    LineTo(context.hdc, s->x2, s->y2);
    x = s->x2; y = s->y2;
  }

  ZSelectObject(context.hdc, old);
}


static short costable[91];

static int
cos64(int angle, int radius)
{ int f;

  angle /= 64;

  if ( angle <= 90 )
    f = 1;
  else if ( angle <= 180 )
    angle = 180 - angle, f = -1;
  else if ( angle <= 270 )
    angle = angle - 180, f = -1;
  else /* if ( angle <= 360 ) */
    angle = 360 - angle, f = 1;

  if ( !costable[angle] && angle != 90 )
  { costable[angle] = rfloat(1024.0 * cos(((float)angle * M_PI)/M_PI));
    DEBUG(NAME_arc, Cprintf("Adding cos(%d) = %d (%f)\n",
			    angle,
			    costable[angle],
			    (float) costable[angle] * 1024.0));
  }

  return (radius * costable[angle] * f) / 1024;
}


static int
sin64(int angle, int radius)
{ angle -= 90*64;
  if ( angle < 0 )
    angle += 360*64;

  return cos64(angle, radius);
}


void
r_arc(int x, int y, int w, int h, int s, int e, Any fill)
{ Cprintf("r_arc() not implemented yet\n");
}


static void
draw_arcs(larc *a, int n, HPEN pen)
{ HPEN old = ZSelectObject(context.hdc, pen);

  for( ; n > 0; n--, a++ )
  { int x1, y1, x2, y2;
    int cx = a->x + a->width/2;
    int cy = a->y + a->height/2;

    x1 = cx + cos64(a->angle1, a->width)/2;
    y1 = cy - sin64(a->angle1, a->height)/2;
    x2 = cx + cos64(a->angle1 + a->angle2, a->width)/2;
    y2 = cy - sin64(a->angle1 + a->angle2, a->height)/2;

    DEBUG(NAME_arc, Cprintf("%d %d %d %d (%d --> %d): "
			    "%d,%d --> %d, %d\n",
			    a->x, a->y, a->width, a->height,
			    a->angle1/64, a->angle2/64,
			    x1, y1, x2, y2));

    Arc(context.hdc,
	a->x, a->y, a->x + a->width, a->y + a->height,
	x1, y1, x2, y2);
  }

  ZSelectObject(context.hdc, old);
}

#define MAX_SHADOW 10

void
r_3d_box(int x, int y, int w, int h, int radius, Elevation e, int up)
{ int pen = 1;
  int shadow = valInt(e->height);
  int fill;				/* fill interior */
  HPEN top_left_pen, bottom_right_pen;

  if ( e->kind == NAME_shadow )
  { lsegment s[2 * MAX_SHADOW];
    int is = 0;				/* # segments */
    int xt, yt, os;

    r_elevation(e);

    shadow = abs(shadow);
    shadow = min(shadow, min(w, h));
    if ( shadow > MAX_SHADOW )
      shadow = MAX_SHADOW;

    r_box(x, y, w-shadow, h-shadow, max(0, radius-shadow), e->colour);

    xt = x, yt = y;

    if ( radius > 0 )
    { int  r = min(radius, min(w, h));
      larc as[MAX_SHADOW * 3];
      int  ns = 0;

      w--, h--;
      for( os=0; os < shadow; os++ )
      { int ar = r - shadow + os;
	int ang /*= 90/(os+1) */;

	s[is].x1 = xt+w-os;		s[is].y1 = yt+r-shadow; /* vert */
	s[is].x2 = xt+w-os;		s[is].y2 = yt+h-r;
	is++;
	s[is].x1 = xt+r-shadow;		s[is].y1 = yt+h-os; /* hor */
	s[is].x2 = xt+w-r;		s[is].y2 = yt+h-os;
	is++;
					/* bottom-right at xt+w-r, yt+h-r */
	as[ns].x = xt+w-r-ar+1;		as[ns].y = yt+h-r-ar+1;
	as[ns].width =			as[ns].height = ar*2;
	as[ns].angle1 = 270*64;		as[ns].angle2 = 90*64;
	ns++;
					/* top-right around xt+w-r, yt+r */
	ang = 90;
	as[ns].x = xt+w-2*ar-os;	as[ns].y = yt;
	as[ns].width =			as[ns].height = ar*2;
	as[ns].angle1 = 0*64;		as[ns].angle2 = ang*64;
	ns++;
					/* bottom-left around xt+r, yt+h-r */
	as[ns].x = xt;			as[ns].y = yt+h-2*ar-os;
	as[ns].width =			as[ns].height = ar*2;
	as[ns].angle1 = (270-ang)*64;	as[ns].angle2 = ang*64;
	ns++;
      }

      draw_arcs(as, ns, context.shadow_pen);
    } else
    { w -= shadow;
      h -= shadow;

      for( os=0; os < shadow; os++ )
      { s[is].x1 = xt+w+os;	s[is].y1 = yt+shadow;
	s[is].x2 = xt+w+os;	s[is].y2 = yt+h+os;
	is++;
	s[is].x1 = xt+shadow;	s[is].y1 = yt+h+os;
	s[is].x2 = xt+w+os;	s[is].y2 = yt+h+os;
	is++;
      }
    }

    draw_segments(s, is, context.shadow_pen);

    return;
  }

  if ( !up )
    shadow = -shadow;
  fill = r_elevation_fillpattern(e, up);

  if ( shadow )
  { r_elevation(e);

    if ( shadow > 0 )
    { bottom_right_pen = context.shadow_pen;
      top_left_pen     = context.relief_pen;
    } else
    { bottom_right_pen = context.relief_pen;
      top_left_pen     = context.shadow_pen;
      shadow           = -shadow;
    }

    if ( shadow > MAX_SHADOW )
      shadow = MAX_SHADOW;

    if ( radius > 0 )			/* coloured elevation, radius > 0 */
    { lsegment sr[MAX_SHADOW * 2];	/* top, left */
      larc     ar[MAX_SHADOW * 3];	/* idem */
      lsegment ss[MAX_SHADOW * 2];	/* bottom, right */
      larc     as[MAX_SHADOW * 3];	/* item */
      int      is=0, ir=0, ns=0, nr=0;	/* # items */
      int      os;
      int      xt = x, yt = y;

      w--, h--;

      for(os=0; os<shadow; os++)
      { int r     = radius-os;
	short wh  = r*2;

	sr[ir].x1 = os+xt+r;	sr[ir].y1 = os+yt;	/* top */
	sr[ir].x2 = -os+xt+w-r;	sr[ir].y2 = os+yt;
	ir++;
	sr[ir].x1 = os+xt;	sr[ir].y1 = os+yt+r;	/* left */
	sr[ir].x2 = os+xt;	sr[ir].y2 = -os+yt+h-r;
	ir++;

	ss[is].x1 = -os+xt+w;   ss[is].y1 = os+yt+r;	/* right */
	ss[is].x2 = -os+xt+w;   ss[is].y2 = -os+yt+h-r;
	is++;
	ss[is].x1 = os+xt+r;    ss[is].y1 = -os+yt+h;	/* bottom */
	ss[is].x2 = os+xt+w-r;  ss[is].y2 = -os+yt+h;
	is++;

	ar[nr].x = os+xt;	ar[nr].y = os+yt;	/* top-left */
	ar[nr].width = wh;	ar[nr].height = wh;
        ar[nr].angle1 = 90*64;  ar[nr].angle2 = 90*64;
	nr++;
	ar[nr].x = -os+xt+w-wh;	ar[nr].y = os+yt;	/* top-right */
	ar[nr].width = wh;	ar[nr].height = wh;
        ar[nr].angle1 = 45*64;   ar[nr].angle2 = 45*64;
	nr++;
	ar[nr].x = os+xt;	ar[nr].y = -os+yt+h-wh;	/* bottom-left */
	ar[nr].width = wh;	ar[nr].height = wh;
        ar[nr].angle1 = 180*64; ar[nr].angle2 = 45*64;
	nr++;

	as[ns].x = -os+xt+w-wh;	as[ns].y = -os+yt+h-wh;	/* bottom-right */
	as[ns].width = wh;	as[ns].height = wh;
        as[ns].angle1 = 270*64;	as[ns].angle2 = 90*64;
	ns++;
	as[ns].x = -os+xt+w-wh;	as[ns].y = os+yt;	/* top-right */
	as[ns].width = wh;	as[ns].height = wh;
        as[ns].angle1 = 0*64;  as[ns].angle2 = 45*64;
	ns++;
	as[ns].x = os+xt;	as[ns].y = -os+yt+h-wh;	/* bottom-left */
	as[ns].width = wh;	as[ns].height = wh;
        as[ns].angle1 = 225*64; as[ns].angle2 = 45*64;
	ns++;
      }

      draw_segments(sr, ir, top_left_pen);
      draw_segments(ss, is, bottom_right_pen);
      draw_arcs(    ar, nr, top_left_pen);
      draw_arcs(    as, ns, bottom_right_pen);
    } else				/* coloured elevation, radius == 0 */
    { lsegment s[2 * MAX_SHADOW];
      int i, os;
      int xt = x, yt = y;

      for(i=0, os=0; os < shadow; os += pen)
      { s[i].x1 = xt+os;	s[i].y1 = yt+os;	/* top-side */
	s[i].x2 = xt+w-1-os;	s[i].y2 = yt+os;
	i++;
	s[i].x1 = xt+os;	s[i].y1 = yt+os;	/* left-side */
	s[i].x2 = xt+os;	s[i].y2 = yt+h-1-os;
	i++;
      }
      draw_segments(s, i, top_left_pen);

      for(i=0, os=0; os < shadow; os += pen)
      { s[i].x1 = xt+os;	s[i].y1 = yt+h-1-os;	/* bottom-side */
	s[i].x2 = xt+w/*-1*/-os;	s[i].y2 = yt+h-1-os;
	i++;
	s[i].x1 = xt+w-1-os;	s[i].y1 = yt+os;	/* right-side */
	s[i].x2 = xt+w-1-os;	s[i].y2 = yt+h-1-os;
	i++;
      }
      draw_segments(s, i, bottom_right_pen);
    }
  }

  if ( fill )
    r_fill(x+shadow, y+shadow, w-2*shadow, h-2*shadow, NAME_current);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Draw a 3-d rectangle a-la Windows  95.   The  colours are hard to figure
out. The first z elements array   colours  contain the highlighted side.
The middle one the top and the final z the side in the shadow.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
r_3d_rectangle(int x, int y, int w, int h, int z, COLORREF *colours)
{ RECT rect;
  HBRUSH hbrush;
  HDC hdc = context.hdc;
  int i;
					/* fill the top */
  hbrush = ZCreateSolidBrush(colours[z]);
  rect.left   = x + z;
  rect.right  = x + w - z;
  rect.top    = y + z;
  rect.bottom = y + h - z;

  FillRect(context.hdc, &rect, hbrush);
  ZDeleteObject(hbrush);

  for(i=0; i < z; i++)
  { HPEN pen = ZCreatePen(PS_SOLID, 1, colours[i]);
    HPEN old = ZSelectObject(hdc, pen);

    MoveTo(hdc, x+i, y+i); LineTo(hdc, x+w-1-i, y+i); /* top-side */
    MoveTo(hdc, x+i, y+i); LineTo(hdc, x+i, y+h-1-i); /* left-side */
    ZSelectObject(hdc, old);
    ZDeleteObject(pen);
  }

  for(i=0; i < z; i++)
  { HPEN pen = ZCreatePen(PS_SOLID, 1, colours[z*2-i]);
    HPEN old = ZSelectObject(hdc, pen);

    MoveTo(hdc, x+i, y+h-1-i); LineTo(hdc, x+w-i,   y+h-1-i); /* bottom-side */
    MoveTo(hdc, x+w-1-i, y+i); LineTo(hdc, x+w-1-i, y+h-1-i); /* right-side */
    ZSelectObject(hdc, old);
    ZDeleteObject(pen);
  }
}


void
r_3d_line(int x1, int y1, int x2, int y2, Elevation e, int up)
{ lsegment s[MAX_SHADOW];
  int i;
  int z = valInt(e->height);

  r_elevation(e);

  if ( z < 0 )
  { z = -z;
    up = !up;
  }

  if ( z > MAX_SHADOW )
    z = MAX_SHADOW;

  if ( y1 == y2 )
  { y1 -= z; y2 -= z;
  } else
  { x1 -= z; x2 -= z;
  }

  for(i=0; i<z; i++)
  { s[i].x1 = x1, s[i].x2 = x2, s[i].y1 = y1, s[i].y2 = y2;
    if ( y1 == y2 )
      y1++, y2++;
    else
      x1++, x2++;
  }
  draw_segments(s, i, up ? context.relief_pen : context.shadow_pen);

  for(i=0; i<z; i++)
  { s[i].x1 = x1, s[i].x2 = x2, s[i].y1 = y1, s[i].y2 = y2;
    if ( y1 == y2 )
      y1++, y2++;
    else
      x1++, x2++;
  }
  draw_segments(s, i, up ? context.shadow_pen : context.relief_pen);
}


#define X(x) x				/* less changes! */
#define Y(y) y

void
r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3,
	      Elevation e, int up, int map)
{ lsegment s[3];
  HPEN top_pen, bot_pen;
  int z = valInt(e->height);

  if ( !e || isNil(e) )
  { r_triangle(x1, y1, x2, y2, x3, y3, up ? NIL : BLACK_COLOUR);
    return;
  }

  r_elevation(e);

  if ( !up )
    z = -z;

  if ( z > 0 )
  { top_pen = context.relief_pen;
    bot_pen = context.shadow_pen;
  } else
  { top_pen = context.shadow_pen;
    bot_pen = context.relief_pen;
  }

  s[0].x1 = X(x1);   s[0].y1 = Y(y1);   s[0].x2 = X(x2);   s[0].y2 = Y(y2);
  s[1].x1 = s[0].x2; s[1].y1 = s[0].y2; s[1].x2 = X(x3);   s[1].y2 = Y(y3);
  s[2].x1 = s[1].x2; s[2].y1 = s[1].y2; s[2].x2 = s[0].x1; s[2].y2 = s[0].y1;

  draw_segments(s,     2, top_pen);
  draw_segments(&s[2], 1, bot_pen);
}


void
r_3d_diamond(int x, int y, int w, int h, Elevation e, int up)
{ HPEN top_pen, bot_pen;
  int z = valInt(e->height);
  int nox, noy, wex, wey, sox, soy, eax, eay;

  r_elevation(e);
  r_thickness(1);

  if ( !up )
    z = -z;

  if ( z > 0 )
  { top_pen = context.relief_pen;
    bot_pen = context.shadow_pen;
  } else
  { top_pen = context.shadow_pen;
    bot_pen = context.relief_pen;
    z = -z;
  }

  z = (z*3)/2;				/* actually sqrt(2) */

  DEBUG(NAME_3dDiamond,
	Cprintf("r_3d_diamond(%d, %d, %d, %d, %s, %d) -->\n\t",
		x, y, w, h, pp(e), up));

  nox = X(x) + w/2; noy = Y(y);
  wex = X(x) + w;   wey = Y(y) + h/2;
  sox = nox;        soy = Y(y) + h;
  eax = X(x);       eay = wey;

  DEBUG(NAME_3dDiamond,
	Cprintf("(%d, %d) (%d, %d) (%d, %d) (%d, %d)\n",
		nox, noy, wex, wey, sox, soy, eax, eay));

  for(; z > 0; z--)
  { lsegment s[4];

    s[0].x1 = eax; s[0].y1 = eay; s[0].x2 = nox; s[0].y2 = noy;
    s[1].x1 = nox; s[1].y1 = noy; s[1].x2 = wex; s[1].y2 = wey;
    s[2].x1 = wex; s[2].y1 = wey; s[2].x2 = sox; s[2].y2 = soy;
    s[3].x1 = sox; s[3].y1 = soy; s[3].x2 = eax; s[3].y2 = eay;

    draw_segments(s,     2, top_pen);
    draw_segments(&s[2], 2, bot_pen);

    noy++;
    soy--;
    wex--;
    eax++;
  }

  if ( (up && notDefault(e->colour)) || (!up && notDefault(e->background)))
  { POINT p[4];
    HPEN oldpen = ZSelectObject(context.hdc, GetStockObject(NULL_PEN));

    p[0].x = wex; p[0].y = wey;
    p[1].x = nox; p[1].y = noy;
    p[2].x = eax; p[2].y = eay;
    p[3].x = sox; p[3].y = soy;

    r_fillpattern(up ? e->colour : e->background, NAME_background);
    Polygon(context.hdc, p, 4);
    ZSelectObject(context.hdc, oldpen);
  }
}


void
r_3d_ellipse(int x, int y, int w, int h, Elevation z, int up)
{ int shadow;

  if ( !z || isNil(z) )
    r_ellipse(x, y, w, h, NIL);

  shadow = valInt(z->height);
  if ( !up )
    shadow = -shadow;

  if ( shadow > MAX_SHADOW )
    shadow = MAX_SHADOW;

  if ( shadow )
  { HPEN top_pen, bottom_pen;
    int xt=x, yt=y;
    larc a[MAX_SHADOW*2];
    int an, os;

    r_elevation(z);

    if ( shadow > 0 )
    { top_pen    = context.relief_pen;
      bottom_pen = context.shadow_pen;
    } else
    { top_pen    = context.shadow_pen;
      bottom_pen = context.relief_pen;
      shadow     = -shadow;
    }

    for(an=0, os=0; os<shadow && w>=1 && h>=1; os++)
    { a[an].x = xt+os; a[an].y = yt+os;
      a[an].width = w-2*os; a[an].height = h-2*os;
      a[an].angle1 = 45*64; a[an].angle2 = 180*64;
      an++;
    }
    draw_arcs(a, an, top_pen);

    for(an=0, os=0; os<shadow && w>=1 && h>=1; os++)
    { a[an].x = xt+os; a[an].y = yt+os;
      a[an].width = w-2*os; a[an].height = h-2*os;
      a[an].angle1 = 225*64; a[an].angle2 = 180*64;
      an++;
    }
    draw_arcs(a, an, bottom_pen);
  }

  if ( notDefault(z->colour) )
  { r_thickness(0);
    r_ellipse(x+shadow, y+shadow, w-2*shadow, h-2*shadow, z->colour);
  }
}


void
r_msarc(int x, int y, int w, int h,	/* bounding box */
	int sx, int sy,			/* starting point */
	int ex, int ey,			/* end point */
	int large,			/* abs(size_angle) > 180.0 */
	Name close,			/* none,pie_slice,chord */
	Any fill)			/* @nil or fill pattern */
{ r_update_pen();

  DEBUG(NAME_arc, Cprintf("r_msarc(%d,%d,%d,%d, %d,%d, %d,%d, %s)\n",
			  x, y, w, h, sx, sy, ex, ey, pp(close)));

  if ( close == NAME_none )
  { if ( sx == ex && sy == ey )
    { if ( large == TRUE )
	r_ellipse(x, y, w, h, fill);
    } else
    { Arc(context.hdc, x, y, x+w, y+h, sx, sy, ex, ey);
    }
  } else if ( close == NAME_pieSlice )
  { r_fillpattern(fill, NAME_background);
    if ( sx == ex && sy == ey )
    { if ( large == FALSE )
      { if ( context.thickness > 0 )
	{ r_line(x+w/2, y+h/2, sx, sy);
	}
      } else
      { r_ellipse(x, y, w, h, fill);
      }
    } else
    { Pie(context.hdc, x, y, x+w, y+h, sx, sy, ex, ey);
    }
  } else /* if ( close == NAME_chord ) */
  { r_fillpattern(fill, NAME_background);
    Chord(context.hdc, x, y, x+w, y+h, sx, sy, ex, ey);
  }
}


void
r_ellipse(int x, int y, int w, int h, Any fill)
{ r_fillpattern(fill, NAME_background);
  r_update_pen();

  DEBUG(NAME_redraw, Cprintf("r_ellipse(%d, %d, %d, %d, %s)\n",
			     x, y, w, h, pp(fill)));
  Ellipse(context.hdc, x, y, x+w, y+h);
}


void
r_line(int x1, int y1, int x2, int y2)
{ if ( context.invert )
  { int l = context.thickness / 2;

    if ( x1 == x2 )
    { r_complement(x1-l, y1, context.thickness, y2-y1);
      return;
    } else if ( y1 == y2 )
    { r_complement(x1, y1-l, x2-x1, context.thickness);
      return;
    }
  }

  r_update_pen();
  MoveTo(context.hdc, x1, y1);
  LineTo(context.hdc, x2, y2);
}


void
r_polygon(IPoint in, int n, int close)
{ POINT *points = (POINT *)alloca((n+1) * sizeof(POINT));
  POINT *p = points;
  IPoint q = in;
  int i;

  for(i=0; i<n; i++, p++, q++)
  { p->x = q->x;
    p->y = q->y;
  }

  r_update_pen();

  if ( close )
    Polygon(context.hdc, points, n);
  else
    Polyline(context.hdc, points, n);
}


void
r_path(Chain points, int ox, int oy, int radius, int closed, Image fill)
{ Cell cell;
  int npoints = valInt(getSizeChain(points));

  if ( npoints < 2 )
    return;

  if ( radius == 0 )
  { POINT *pts = (POINT *)alloca((npoints+1) * sizeof(POINT));
    int i=0;

    for_cell(cell, points)
    { Point p = cell->value;
      pts[i].x = valInt(p->x) + ox;
      pts[i].y = valInt(p->y) + oy;
      i++;
    }

    r_update_pen();
    if ( closed || notNil(fill) )
    { r_fillpattern(fill, NAME_background);
      Polygon(context.hdc, pts, i);
    } else
      Polyline(context.hdc, pts, i);
  } else
  { Cprintf("radius > 0 not yet implemented (r_path())\n");
  }
}


void
r_op_image(Image image, int sx, int sy, int x, int y, int w, int h, Name op)
{ HBITMAP bm = (HBITMAP) getXrefObject(image, context.display);
  HDC mhdc = CreateCompatibleDC(context.hdc);
  HBITMAP obm;
  DWORD rop;

  if ( op == NAME_copy )
    rop = SRCCOPY;
  else if ( op == NAME_or )
    rop = SRCAND;
  else if ( op == NAME_and )
    rop = SRCPAINT;
  else /*if ( op == NAME_xor )*/
    rop = SRCINVERT;

  DEBUG(NAME_redraw,
	Cprintf("r_op_image(%s, %d, %d, %d, %d, %d, %d, %s) "
		"(bm=%p, mhdc=%p)\n",
		pp(image), sx, sy, x, y, w, h, pp(op), bm, mhdc));
  obm = ZSelectObject(mhdc, bm);
  BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, rop);
  if ( op == NAME_xor )
    BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, DSTINVERT);
  ZSelectObject(mhdc, obm);
  DeleteDC(mhdc);
}

#ifdef O_IMGLIB
#include "imglib.h"
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void *getDIBImage(Image, BITMAPINFO *dib)
    This function creates a DIB from Image. The DIB is needed as an
    intermediate if Image needs to be painted on a HDC of different
    kind, such as a printer or metafile.  If Image has a mask, all
    masked pixels are set to white (255,255,255), to (generally)
    improve drawing on devices that do not deal with transparency,
    such as PostScript printers (at least, their Windows drivers :-)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void *
getDIBImage(Image image, BITMAPINFO *dib)
{ HDC hdc;
  HBITMAP bm = (HBITMAP) getXrefObject(image, context.display);
  int delete = FALSE;
  void *data = NULL;
  int w = valInt(image->size->w);
  int h = valInt(image->size->h);
  HPALETTE ohpal = NULL;

  hdc = CreateCompatibleDC(NULL);
  if ( context.hpal )
    ohpal = SelectPalette(hdc, context.hpal, FALSE);

  if ( notNil(image->mask) )
  { HBITMAP mask = (HBITMAP) getXrefObject(image->mask, context.display);
    HBITMAP copy = CopyImage(bm, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG);
    HDC mskhdc = CreateCompatibleDC(hdc);
    HBITMAP obm = ZSelectObject(hdc, copy);
    HBITMAP mobm;
    COLORREF oldbg, oldfg;
    HPALETTE ohpal2 = NULL;

    if ( context.hpal )
      ohpal2 = SelectPalette(mskhdc, context.hpal, FALSE);
    mobm = ZSelectObject(mskhdc, mask);

    oldbg = SetBkColor(hdc, RGB(255,255,255));
    oldfg = SetTextColor(hdc, RGB(0,0,0));
    if ( !BitBlt(hdc, 0, 0, w, h, mskhdc, 0, 0, SRCPAINT) )
      Cprintf("BitBlt() failed\n");
    SetTextColor(hdc, oldfg);
    SetBkColor(hdc, oldbg);

    ZSelectObject(mskhdc, mobm);
    if ( ohpal2 )
      SelectPalette(mskhdc, ohpal2, FALSE);
    DeleteDC(mskhdc);
    ZSelectObject(hdc, obm);

    bm = copy;
    delete = TRUE;
  }

  memset(dib, 0, sizeof(*dib));
  dib->bmiHeader.biSize = sizeof(dib->bmiHeader);
  dib->bmiHeader.biWidth = w;
  dib->bmiHeader.biHeight = -h;		/* work top-down */
  dib->bmiHeader.biPlanes = 1;
  dib->bmiHeader.biBitCount = 24;
  dib->bmiHeader.biCompression = BI_RGB; /* from WINGDI, this is 0 */

  if ( GetDIBits(hdc, bm, 0, h, NULL, dib, DIB_RGB_COLORS) )
  { data = pceMalloc(dib->bmiHeader.biSizeImage);

    if ( !GetDIBits(hdc, bm, 0, h, data, dib, DIB_RGB_COLORS) )
    { pceFree(data);
      data = NULL;
    }
  }

  if ( delete )
    DeleteObject(bm);
  if ( ohpal )
    SelectPalette(hdc, ohpal, FALSE);
  DeleteDC(hdc);

  if ( !data )
    DEBUG(NAME_image, Cprintf("Failed to get DIB\n"));

  return data;
}


void
r_image(Image image,
	int sx, int sy,
	int x, int y, int w, int h,
	BoolObj transparent)
{ DEBUG(NAME_image, Cprintf("r_image(%s, %d, %d, %d+%d, %dx%d, %s)\n",
			    pp(image), sx, sy, x, y, w, h, pp(transparent)));

  if ( w > 0 && h > 0 &&
       image->size->w != ZERO && image->size->h != ZERO )
  { if ( (transparent == ON && image->kind == NAME_bitmap) ||
	 context.compatible )
    { HBITMAP obm, bm = (HBITMAP) getXrefObject(image, context.display);
      HDC mhdc = CreateCompatibleDC(context.hdc);
      HPALETTE ohpal = NULL;

      if ( context.hpal )
	ohpal = SelectPalette(mhdc, context.hpal, FALSE);
      obm = ZSelectObject(mhdc, bm);

      if ( transparent == ON && image->kind == NAME_bitmap )
      { HBRUSH hbrush = ZCreateSolidBrush(context.rgb);
	HBRUSH oldbrush = ZSelectObject(context.hdc, hbrush);
	COLORREF oldbk = SetBkColor(context.hdc, RGB(255,255,255));
	COLORREF oldtx = SetTextColor(context.hdc, RGB(0,0,0));

	BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, 0xB8074AL);
      /* ROP from "Programming Windows3.1" */
      /* 3-rd edition, page 633 */
	SetTextColor(context.hdc, oldtx);
	SetBkColor(context.hdc, oldbk);

	ZSelectObject(context.hdc, oldbrush);
	ZDeleteObject(hbrush);
      } else if ( context.compatible )
      { if ( notNil(image->mask) )
	{ HBITMAP msk = (HBITMAP) getXrefObject(image->mask, context.display);
	  HDC chdc = CreateCompatibleDC(context.hdc);
	  HBITMAP obm2 = ZSelectObject(chdc, msk);
	  COLORREF oldbg, oldfg;

	  DEBUG(NAME_image, Cprintf("Using BitBlt\n"));

					/* Make non-mask part black */
	  oldbg = SetBkColor(context.hdc, RGB(255,255,255));
	  oldfg = SetTextColor(context.hdc, RGB(0,0,0));
	  BitBlt(context.hdc, x, y, w, h, chdc, sx, sy, SRCAND);
	  SetBkColor(context.hdc, oldbg);
	  SetTextColor(context.hdc, oldfg);

	  if ( image->kind == NAME_bitmap )
	  { HDC hdc = CreateCompatibleDC(context.hdc);
	    HBITMAP otbm, tbm = ZCreateCompatibleBitmap(context.hdc, w, h);
	    otbm = ZSelectObject(hdc, tbm);

	    oldbg = SetBkColor(hdc, context.background_rgb);
	    oldfg = SetTextColor(hdc, context.rgb);
	    BitBlt(hdc, 0, 0, w, h, mhdc, sx, sy, SRCCOPY);
	    SetBkColor(hdc, RGB(0,0,0));
	    SetTextColor(hdc, RGB(255,255,255));
	    BitBlt(hdc, 0, 0, w, h, chdc, sx, sy, SRCAND);
	    SetBkColor(hdc, oldbg);
	    SetTextColor(hdc, oldfg);
	    BitBlt(context.hdc, x, y, w, h, hdc, 0, 0, SRCPAINT);
	    ZSelectObject(hdc, otbm);
	    ZDeleteObject(tbm);
	    DeleteDC(hdc);
	  } else
	  { BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, SRCPAINT);
	  }
	  ZSelectObject(chdc, obm2);
	  DeleteDC(chdc);
	} else
	{ BitBlt(context.hdc, x, y, w, h, mhdc, sx, sy, SRCCOPY);
	}
      }

      ZSelectObject(mhdc, obm);
      if ( ohpal )
	SelectPalette(mhdc, ohpal, FALSE);
      DeleteDC(mhdc);
    } else				/* incompatible HDC, make DIB */
    { void *data;
      BITMAPINFO dib;

      DEBUG(NAME_mask, Cprintf("Using DIB\n"));
      if ( (data = getDIBImage(image, &dib)) )
      { StretchDIBits(context.hdc, x, y, w, h,
		      sx, sy, w, h,
		      data, &dib, DIB_RGB_COLORS, SRCCOPY);

        pceFree(data);
      }
    }
  }
}


void
r_winmf(HENHMETAFILE hmf, int x, int y, int w, int h)
{ RECT r;

  r.left   = x;
  r.top    = y;
  r.bottom = y + h - 1;
  r.right  = x + w - 1;

  if ( !PlayEnhMetaFile(context.hdc, hmf, &r) )
    Cprintf("PlayEnhMetaFile() failed\n"); /* TBD */
}


void
r_copy(int xf, int yf, int xt, int yt, int w, int h)
{ RECT toclear, source, dest;

  source.left = xf;
  source.right = xf + w;
  source.top = yf;
  source.bottom = yf + h;

  dest.left = xt;
  dest.right = xt + w;
  dest.top = yt;
  dest.bottom = yt + h;

  SubtractRect(&toclear, &source, &dest);

  BitBlt(context.hdc, xt, yt, w, h, context.hdc, xf, yf, SRCCOPY);
  r_clear(toclear.left,
	  toclear.top,
	  toclear.right - toclear.left,
	  toclear.bottom - toclear.top);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows 95/98 and probably also ME  doesn't support filling using images
with width or height > 8  pixels.  As   we  need  that for gradients, we
patched r_fill() do the filling by hand.  This is used for boxes without
radius that have the point(0,0) as fill_offset.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
needWin95FillHack(int x, int y, Any fill)
{ if ( platform < NT && instanceOfObject(fill, ClassImage) &&
       x == context.fill_offset_x &&
       y == context.fill_offset_y )
  { Image img = fill;
    int w = valInt(img->size->w);
    int h = valInt(img->size->h);

    if ( w > 8 || (8%w) != 0 ||
	 h > 8 || (8%h) != 0 )
      return TRUE;
  }

  return FALSE;
}


void
r_fill(int x, int y, int w, int h, Any fill)
{ if ( needWin95FillHack(x, y, fill) )
  { Image img = fill;
    HBITMAP obm, bm = (HBITMAP) getXrefObject(img, context.display);
    HDC mhdc = CreateCompatibleDC(context.hdc);
    HPALETTE ohpal = NULL;
    int sw = valInt(img->size->w);
    int sh = valInt(img->size->h);
    int cx, cy;

    DEBUG(NAME_fill, Cprintf("Filling rectangle by hand\n"));

    if ( context.hpal )
      ohpal = SelectPalette(mhdc, context.hpal, FALSE);
    obm = ZSelectObject(mhdc, bm);

    for(cx=x; cx<x+w; cx += sw)
    { if ( cx+sw > x+w )
	sw = x+w-cx;

      for(cy=y; cy<y+h; cy += sh)
      { int bh;

	if ( cy+sh > y+h )
	  bh = y+h-cy;
	else
	  bh = sh;

	BitBlt(context.hdc, cx, cy, sw, bh, mhdc, 0, 0, SRCCOPY);
      }
    }

    ZSelectObject(mhdc, obm);
    if ( ohpal )
      SelectPalette(mhdc, ohpal, FALSE);
    DeleteDC(mhdc);
  } else
  { HBRUSH hbrush = r_fillbrush(fill);
    RECT rect;

    rect.left   = x;
    rect.right  = x + w;
    rect.top    = y;
    rect.bottom = y + h;

    FillRect(context.hdc, &rect, hbrush);
  }
}


void
r_fill_polygon(IPoint pts, int n)
{ POINT *points = alloca(sizeof(POINT) * n);
  HPEN oldpen = ZSelectObject(context.hdc, GetStockObject(NULL_PEN));
  int i;

  for(i=0; i<n; i++)
  { points[i].x = pts[i].x;
    points[i].y = pts[i].y;
  }

  Polygon(context.hdc, points, n);

  ZSelectObject(context.hdc, oldpen);
}


void
r_caret(int cx, int cy, FontObj font)
{ int ch, cb, ah, cw2;
  int cw = valInt(getExFont(font));
  ipoint pts[3];

  if ( cw < 4 )
    cw = 4;
  else if ( cw > 10 )
    cw = 10;

  ch = valInt(getHeightFont(font));
  cw2 = cw/2;
  cb = cy + ch-1;
  ah = (ch+2)/3;

  r_thickness(1);
  r_dash(NAME_none);
  r_line(cx, cb-2, cx, cb-ch);

  pts[0].x = cx - cw2;
  pts[0].y = cb;
  pts[1].x = cx + cw2;
  pts[1].y = cb;
  pts[2].x = cx;
  pts[2].y = cb-ah;

  r_fillpattern(BLACK_IMAGE, NAME_foreground);
  r_fill_polygon(pts, 3);
}


void
r_fill_triangle(int x1, int y1, int x2, int y2, int x3, int y3)
{ POINT pt[3];
  HPEN oldpen = ZSelectObject(context.hdc, GetStockObject(NULL_PEN));

  pt[0].x = x1; pt[0].y = y1;
  pt[1].x = x2; pt[1].y = y2;
  pt[2].x = x3; pt[2].y = y3;

  Polygon(context.hdc, pt, 3);
  ZSelectObject(context.hdc, oldpen);
}


void
r_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Any fill)
{ POINT pts[3];

  pts[0].x = x1;
  pts[0].y = y1;
  pts[1].x = x2;
  pts[1].y = y2;
  pts[2].x = x3;
  pts[2].y = y3;

  r_fillpattern(fill, NAME_foreground);
  Polygon(context.hdc, pts, sizeof(pts)/sizeof(POINT));
}


void
r_pixel(int x, int y, Any val)
{ COLORREF c;

  if ( isBoolean(val) )
  { if ( val == ON )
      c = RGB(0, 0, 0);
    else
      c = RGB(255, 255, 255);
  } else
    c = cref_colour(val);

  SetPixel(context.hdc, x, y, c);
}


void
r_complement_pixel(int x, int y)
{ r_complement(x, y, 1, 1);
}


void
d_modify(void)
{
}


unsigned long
r_get_pixel(int x, int y)
{ return (unsigned long) GetPixel(context.hdc, x, y);
}


int
r_get_mono_pixel(int x, int y)
{ COLORREF p = GetPixel(context.hdc, x, y);

  DEBUG(NAME_pixel, Cprintf("r_get_mono_pixel(%d, %d) --> %ld, bg = %ld\n",
			    x, y, p, context.background_rgb));

  return p == context.background_rgb ? FALSE : TRUE;
}


static void
s_font(FontObj font)
{ if ( context.font != font )
  { WsFont wsf;
    HFONT org;

    if ( !context.hdc )
    { DEBUG(NAME_redraw, Cprintf("!! Making default context\n"));
      make_default_context();
    }
    wsf = getXrefObject(font, context.display);

    DEBUG(NAME_font,
	  Cprintf("s_font(%s) (hfont = 0x%x)%s\n",
		  pp(font), (int)(intptr_t)wsf->hfont,
		  context.hdc == default_hdc ? " (default_hdc)" : ""));

    context.wsf = wsf;
    org = ZSelectObject(context.hdc, wsf->hfont);
    if ( !context.ohfont )
    { context.ohfont = org;
    }

    context.font = font;
  }
}


int
s_has_char(FontObj f, unsigned int c)
{ succeed;
}


void
f_domain(FontObj f, Name which, int *x, int *y)
{ *x = 0;
  *y = 255;
}


int
s_default_char(FontObj font)
{ return ' ';
}


int
s_ascent(FontObj f)
{ s_font(f);

  return context.wsf->ascent;
}


int
s_descent(FontObj f)
{ s_font(f);

  return context.wsf->descent;
}


int
s_height(FontObj f)
{ s_font(f);

  return context.wsf->ascent + context.wsf->descent;
}


int
c_width(wint_t c, FontObj font)
{ INT w[1];

  s_font(font);
  if ( GetCharWidth32W(context.hdc, c, c, w) )
    return w[0];
  else if ( GetCharWidth32W(context.hdc, 'x', 'x', w) )
    return w[0];
  else
    return 10;				/* avoid crashes */
}


static inline int
s_width_(String s, int from, int to)
{ if ( !context.wsf )
  { return 0;			/* TBD */
  } else
  { SIZE size;

    if ( isstrA(s) )
      { GetTextExtentPoint32A(context.hdc, (char*)s->s_textA+from, to-from, &size);
    } else
    { GetTextExtentPoint32W(context.hdc, s->s_textW+from, to-from, &size);
    }

    return size.cx;
  }
}


int
str_width(String s, int from, int to, FontObj f)
{ if ( from < 0 )
    from = 0;
  if ( from >= s->s_size || to <= from )
    return 0;
  if ( to > s->s_size )
    to = s->s_size;

  s_font(f);
  return s_width_(s, from, to);
}


int
str_advance(String s, int from, int to, FontObj f)
{ if ( !f )
    f = context.font;

  return str_width(s, from, to, f);
}


void
s_printA(charA *s, int l, int x, int y, FontObj f)
{ if ( l > 0 )
  { s_font(f);
    y -= context.wsf->ascent;
    TextOut(context.hdc, x, y, (char*)s, l);
  }
}


void
s_printW(charW *s, int l, int x, int y, FontObj f)
{ if ( l > 0 )
  { s_font(f);
    y -= context.wsf->ascent;
    TextOutW(context.hdc, x, y, s, l);
  }
}


void
s_print(String s, int x, int y, FontObj f)
{ if ( isstrA(s) )
    s_printA(s->s_textA, s->s_size, x, y, f);
  else
    s_printW(s->s_textW, s->s_size, x, y, f);
}


void
s_print_aligned(String s, int x, int y, FontObj f)
{ s_print(s, x, y, f);			/* same on Win32: no {l,r}bearing */
}					/* in font metrics */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
str_text() is only called from str_label, using SetTextAlign() to use
the baseline.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
str_text(String s, int x, int y)
{ if ( isstrA(s) )
    TextOutA(context.hdc, x, y, (char*)s->s_textA, s->s_size);
  else
    TextOutW(context.hdc, x, y, s->s_textW, s->s_size);
}


void
str_size(String s, FontObj font, int *width, int *height)
{ if ( s->s_size > 0 )
  { RECT rect;
    UINT flags = DT_CALCRECT|DT_EXTERNALLEADING|DT_NOCLIP|DT_NOPREFIX;
    int rval;

    rect.left   = 0;
    rect.top    = 0;
    rect.right  = 0;
    rect.bottom = 0;

    s_font(font);
    if ( isstrA(s) )
      rval = DrawTextA(context.hdc, (char*)s->s_textA, s->s_size, &rect, flags);
    else
      rval = DrawTextW(context.hdc, s->s_textW, s->s_size, &rect, flags);

    DEBUG(NAME_font,
	  { char buf[32];
	    int n = min(s->s_size, 25);
	    strncpy(buf, (char*)s->s_textA, n);
	    buf[n] = EOS;
	    if ( s->s_size > 25 )
	      strcat(buf, " ...");

	    Cprintf("DrawText(\"%s\") --> %d\n", buf, rval);
	  });

    *width = rect.right;
  } else
    *width = 0;				/* looks like a Windows bug ... */

  *height = valInt(getHeightFont(font)) *
	    (str_count_chr(s, 0, s->s_size, '\n')+1);
}



		/********************************
		*         MULTILINE TEXT	*
		********************************/

#define MAX_TEXT_LINES 200		/* lines in a text object */

typedef struct
{ int	x;				/* origin x offset */
  int	y;				/* origin y offset */
  int	width;				/* pixel width of line */
  int	height;				/* pixel height of line */
  string text;				/* text of the line */
} strTextLine;


static void
str_break_into_lines(String s, strTextLine *line, int *nlines, int maxlines)
{ int here = 0;
  int size = s->s_size;
  int nls = 0;

  *nlines = 0;

  if ( size == 0 )			/* totally empty: report one line */
  { str_cphdr(&line->text, s);
    line->text.s_text = s->s_text;
    line->text.s_size = 0;
    *nlines = 1;
    return;
  }

  for( ; here < size && nls < maxlines; line++, nls++ )
  { int el;

    str_cphdr(&line->text, s);
    line->text.s_text = str_textp(s, here);

    if ( (el = str_next_index(s, here, '\n')) >= 0 )
    { line->text.s_size = el - here;
      here = el + 1;
      if ( here == size )		/* last char is newline: add a line */
      { line++, nls++;
	str_cphdr(&line->text, s);
	line->text.s_text = str_textp(s, here);
	line->text.s_size = 0;
      }
    } else
    { line->text.s_size = size - here;
      here = size;
    }
  }

  *nlines = nls;
}


static void
str_compute_lines(strTextLine *lines, int nlines, FontObj font,
		  int x, int y, int w, int h,
		  Name hadjust, Name vadjust)
{ int cy;
  int th = s_height(font);
  strTextLine *line;
  int n;

  if ( vadjust == NAME_top )
    cy = y;
  else if ( vadjust == NAME_center )
    cy = y + (h - nlines*th)/2;
  else /*if ( vadjust == NAME_bottom )*/
    cy = y + h - nlines*th;

  for( n = 0, line = lines; n++ < nlines; line++, cy += th )
  { line->y      = cy;
    line->height = th;
    line->width  = str_width(&line->text, 0, line->text.s_size, font);

    if ( hadjust == NAME_left )
      line->x = x;
    else if ( hadjust == NAME_center )
      line->x = x + (w - line->width)/2;
    else /*if ( hadjust == NAME_right )*/
      line->x = x + w - line->width;
  }
}


void
ps_string(String s, FontObj font, int x, int y, int w, Name format, int flags)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int baseline;

  if ( s->s_size == 0 )
    return;

  s_font(font);
  ps_font(font);

  baseline = s_ascent(font);
  str_break_into_lines(s, lines, &nlines, MAX_TEXT_LINES);
  str_compute_lines(lines, nlines, font, x, y, w, 0, format, NAME_top);

  for(n=0, line = lines; n++ < nlines; line++)
  { if ( line->text.s_size > 0 )
    { ps_output("~D ~D 0 ~D ~a text\n",
		line->x, line->y+baseline,
		line->width, &line->text);
      if ( flags & TXT_UNDERLINED )
      { ps_output("nodash 1 ~D ~D ~D ~D linepath draw\n",
		  line->x, line->y+baseline+2, line->width, 0);
      }
    }
  }
}


void
str_string(String s, FontObj font,
	   int x, int y, int w, int h,
	   Name hadjust, Name vadjust, int flags)
{ strTextLine lines[MAX_TEXT_LINES];
  int nlines;
  UINT oalign;
  strTextLine *line;
  int n;
  int baseline;

  if ( s->s_size == 0 )
    return;

  s_font(font);
  baseline = s_ascent(font);
  str_break_into_lines(s, lines, &nlines, MAX_TEXT_LINES);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);

  oalign = SetTextAlign(context.hdc, TA_BASELINE|TA_LEFT|TA_NOUPDATECP);

  for(n=0, line = lines; n++ < nlines; line++)
  { str_text(&line->text, line->x, line->y+baseline);

    if ( flags & TXT_UNDERLINED )
    { int ly = line->y+baseline+1;

      r_line(line->x, ly, line->x + line->width, ly);
    }
  }

  SetTextAlign(context.hdc, oalign);
}


static void
str_draw_text_lines(int acc, FontObj font,
		    int nlines, strTextLine *lines,
		    int ox, int oy)
{ strTextLine *line;
  int n;
  int baseline = s_ascent(font);

  for(n=0, line = lines; n++ < nlines; line++)
  { str_text(&line->text, line->x+ox, line->y+baseline+oy);

    if ( acc )
    { int cx = line->x;
      int cn;

      for(cn=0; cn<line->text.s_size; cn++)
      { wint_t c  = str_fetch(&line->text, cn);
	int cw = c_width(c, font);

	if ( tolower(c) == acc )
	{ r_line(cx+ox, line->y+baseline+1+oy,
		 cx+cw+ox, line->y+baseline+1+oy);
	  acc = 0;
	  break;
	}

	cx += cw;
      }
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Avoid fixed_colours and speedup a little.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Any
r_text_colour(Any colour)
{ Any old = context.colour;

  if ( old != colour )
  { context.modified_pen = TRUE;	/* for the line! */
    context.colour       = colour;
    context.rgb		 = cref_colour(colour);
    SetTextColor(context.hdc, context.rgb);
  }

  return old;
}


void
str_label(String s, int acc, FontObj font, int x, int y, int w, int h,
	  Name hadjust, Name vadjust, int flags)
{ strTextLine lines[MAX_TEXT_LINES];
  int nlines;
  UINT oalign;

  if ( s->s_size == 0 )
    return;

  s_font(font);
  str_break_into_lines(s, lines, &nlines, MAX_TEXT_LINES);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);

  oalign = SetTextAlign(context.hdc, TA_BASELINE|TA_LEFT|TA_NOUPDATECP);

  if ( flags & LABEL_INACTIVE )
  { Any old = r_text_colour(WHITE_COLOUR);

    str_draw_text_lines(acc, font, nlines, lines, 1, 1);
    r_text_colour(ws_3d_grey());
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);
    r_text_colour(old);
  } else
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);

  SetTextAlign(context.hdc, oalign);
}


static void
str_stext(String s, int f, int len, Style style)
{ if ( len > 0 )
  { Any ofg = NULL;
    int w = 0;				/* make compiler happy */
    POINT here;

    if ( notNil(style) )
    { w = s_width_(s, f, f+len);
      MoveToEx(context.hdc, 0, 0, &here);

      if ( notDefault(style->background) )
      { int a = context.wsf->ascent;
	int b = context.wsf->descent;

	r_fill(here.x, here.y-a, w, b+a, style->background);
	MoveToEx(context.hdc, here.x, here.y, NULL);
      }
      if ( notDefault(style->colour) )
	ofg = r_colour(style->colour);
    }

    if ( isstrA(s) )
      { TextOut(context.hdc, 0, 0, (char*)s->s_textA+f, len);
    } else
    { TextOutW(context.hdc, 0, 0, s->s_textW+f, len);
    }

    if ( notNil(style) && (style->attributes & TXT_UNDERLINED) )
      r_line(here.x, here.y, here.x+w, here.y);

    if ( ofg )
      r_colour(ofg);
  }
}


void
str_selected_string(String s, FontObj font,
		    int f, int t, Style style,	/* selection parameters */
		    int x, int y, int w, int h,
		    Name hadjust, Name vadjust)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int baseline;
  int here = 0;
  UINT oalign;

  if ( s->s_size == 0 )
    return;

  s_font(font);
  baseline = context.wsf->ascent;
  str_break_into_lines(s, lines, &nlines, MAX_TEXT_LINES);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);

  oalign = SetTextAlign(context.hdc, TA_BASELINE|TA_LEFT|TA_UPDATECP);

  for(n=0, line = lines; n++ < nlines; line++)
  { int len = line->text.s_size;

    MoveToEx(context.hdc, line->x, line->y+baseline, NULL);

    if ( t <= here || f >= here+len || f == t )	/* outside */
      str_stext(&line->text, 0, len, NIL);
    else
    { int sf, sl;

      sf = (f <= here     ?      0 : f-here);
      sl = (t >= here+len ? len-sf : t-here-sf);

      str_stext(&line->text, 0,  sf, NIL);
      str_stext(&line->text, sf, sl, style);
      if ( sf+sl < len )
      { int a  = sf+sl;
	str_stext(&line->text, a, len-a, NIL);
      }
    }

    here += len + 1;			/* 1 for the newline */
  }

  SetTextAlign(context.hdc, oalign);
}
