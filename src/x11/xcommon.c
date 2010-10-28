/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/graphics.h>
#include <math.h>
#include "include.h"
#include <X11/keysym.h>
#include <X11/Xproto.h>			/* get request codes */
#include <locale.h>

#undef roundup
#define roundup(v, n)		((((v)+(n)-1)/(n))*(n))
#define rescale(v, o, n)	((v) * (n) / (o))
#define XBRIGHT ((1L<<16)-1)
#define INTENSITY(r, g, b) ((r*20 + g*32 + b*18)/(20+32+18))

XtAppContext	ThePceXtAppContext;	/* X toolkit application context */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The Mac keyboard has a bit different   viewpoint. Alt is primairily used
for creating non-ascii characters, while the command key is used to give
commands. The command key is by default passed as `Mod2'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __APPLE__
#define DefMetaMask Mod2Mask
#else
#define DefMetaMask Mod1Mask
#endif

static unsigned int MetaMask = DefMetaMask;	/* Key-mask for meta */


		 /*******************************
		 *       X11 APP CONTEXT	*
		 *******************************/

static int
x_error_handler(Display *display, XErrorEvent *error)
{ if ( !catchedErrorPce(PCE, NAME_xError) )
  { char msg[1024];
    char request[100];
    char buf[100];

					/* XSetInputFocus() can generate a */
					/* BadMatch that is hard to avoid */
    if ( error->request_code == X_SetInputFocus &&
	 error->error_code == BadMatch )
      return 0;

    XGetErrorText(display, error->error_code, msg, 1024);
    sprintf(buf, "%d", error->request_code);
    XGetErrorDatabaseText(display, "XRequest", buf,
			  "Unknown request", request, 100);
    Cprintf("X error of failed request: %s\n", msg);
    Cprintf("Major opcode of failed request: %d (%s)\n",
	    error->request_code, request);
    Cprintf("Minor opcode of failed request: %d\n", error->minor_code);
    Cprintf("Resource id in failed request:  0x%x\n",
	    (unsigned int) error->resourceid);
    Cprintf("Serial number of failed request: %ld\n", error->serial);

    errorPce(NIL, NAME_xError);
  }

  return 0;				/* what to return here? */
}


#define USE_XDEFAULT_APP_CONTEXT 1

#ifdef USE_XDEFAULT_APP_CONTEXT
extern XtAppContext _XtDefaultAppContext(void);
#undef XtCreateApplicationContext
#define XtCreateApplicationContext _XtDefaultAppContext
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(*) XInitThreads() must  be  called  if   multiple  thread  access  Xlib
functions concurrently. Given the  current  locking,   I  think  this is
cannot happen as we lock the central  XPCE   lock  both if we receive an
event and if we send a message from Prolog.

Nevertheless, XPCE uses this  flag  to   prepare  for  more fine-grained
locking. It turns out the X11 version  distributed with MacOSX has a but
that causes a deadlock in handling dead-keys.  Therefore, we do not lock
for the Mac.  Here is the stack-trace:

#1  0x900019cc in pthread_mutex_lock ()
#2  0x9ba85b9c in _XLockDisplay ()
#3  0x9ba61054 in XPutBackEvent ()
#4  0x9baba0b4 in _XimLocalFilter ()
#5  0x9ba8548c in XFilterEvent ()
#6  0x9bde9538 in _XtDefaultDispatcher ()
#7  0x9bde97b0 in XtDispatchEvent ()
#8  0x9bdf5934 in XtAppProcessEvent ()
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __APPLE__
static int use_x_init_threads = FALSE;
#else
static int use_x_init_threads = TRUE;
#endif

void *
pceXtAppContext(void * ctx)
{ if ( ThePceXtAppContext == NULL )
  { if ( ctx != NULL )
    { ThePceXtAppContext = ctx;
      XSetErrorHandler(x_error_handler);
    } else
    {
#if defined(_REENTRANT) && defined(HAVE_XINITTHREADS)
      if ( XPCE_mt == TRUE )
      { if ( use_x_init_threads )
	  XInitThreads();
      } else
      { XPCE_mt = -1;
      }
#else
	XPCE_mt = -1;
#endif

      XtToolkitInitialize();
      XSetErrorHandler(x_error_handler);

      if ( (ThePceXtAppContext = XtCreateApplicationContext()) == NULL )
      { errorPce(TheDisplayManager(), NAME_noApplicationContext);
	fail;
      }

      if ( !XtSetLanguageProc(ThePceXtAppContext, NULL, NULL) )
      { errorPce(TheDisplayManager(), NAME_noLocaleSupport,
		 CtoName(setlocale(LC_ALL, NULL)));
	fail;
      }
    }
  }

  return ThePceXtAppContext;
}


status
X11ThreadsDisplay(DisplayObj d, BoolObj val)
{ if ( ThePceXtAppContext )
    return errorPce(d, NAME_x11Threads);

  use_x_init_threads = (val == ON ? TRUE : FALSE);

  succeed;
}


		 /*******************************
		 *	 WIDGET REFERENCE	*
		 *******************************/

Widget
widgetWindow(PceWindow sw)
{ return (Widget) sw->ws_ref;
}


Widget
widgetFrame(FrameObj fr)
{ return fr->ws_ref ? ((frame_ws_ref *)fr->ws_ref)->widget : NULL;
}

void
setXImageImage(Image image, XImage *i)
{ image->ws_ref = i;
}


		 /*******************************
		 *	     X11 ATOMS		*
		 *******************************/

#if O_MOTIF
#include <Xm/Xm.h>
#include <Xm/AtomMgr.h>			/* was X11? */
#include <Xm/Protocols.h>		/* idem */

#define XInternAtom(d, nm, v) XmInternAtom(d, nm, v)
#define XGetAtomName(d, atom) XmAtomToName(d, atom)
#endif

Atom
DisplayAtom(DisplayObj d, Name name)
{ DisplayWsXref r = d->ws_ref;

  return XInternAtom(r->display_xref, strName(name), False);
}


char *
DisplayAtomToString(DisplayObj d, Atom atom)
{ DisplayWsXref r = d->ws_ref;

  return XGetAtomName(r->display_xref, atom);
}


Atom
FrameAtom(FrameObj fr, Name name)
{ return DisplayAtom(fr->display, name);
}


char *
FrameAtomToString(FrameObj fr, Atom a)
{ return DisplayAtomToString(fr->display, a);
}


Atom
WmProtocols(FrameObj fr)
{ return FrameAtom(fr, CtoName("WM_PROTOCOLS"));
}


		 /*******************************
		 *	     POSTSCRIPT		*
		 *******************************/

typedef struct
{ int bits;				/* bit remaining */
  int depth;				/* postscript depth */
  int val;				/* current value */
  int count;				/* # emited bytes */
} ps_stat;


static void
put_value(ps_stat *s, int val)
{ static char print[] = { '0', '1', '2', '3', '4', '5', '6', '7',
			  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };
  s->bits -= s->depth;
  s->val |= val << s->bits;

  if ( s->bits == 0 )
  { ps_put_char(print[(s->val >> 4) & 0xf]);
    ps_put_char(print[s->val & 0xf]);
    if ( (++s->count % 32) == 0 )
      ps_put_char('\n');
    s->bits = 8; s->val = 0;
  }
}


int
shift_for_mask(unsigned long mask)
{ unsigned long m = 0x1;
  int shift = 0;

  assert(mask);
  while((mask&m) == 0)
  { m <<= 1;
    shift++;
  }

  return shift;
}


status
postscriptXImage(XImage *im, XImage *mask,
		 int fx, int fy, int w, int h,
		 Display *disp,
		 Colormap cmap,
		 int depth,
		 int iscolor)
{ int x, y, w8;
  unsigned char psmap[256];
  int psbright;
  int direct = FALSE;
  ps_stat stat;

  if ( depth == 0 )			/* PostScript depth is 1, 2, 4, or 8 */
  { depth = im->depth;

    if ( depth == 3 )
      depth = 2;
    else if ( depth > 4 && depth < 8 )
      depth = 4;
    else if ( depth > 8 )
      depth = 8;
  }

  psbright = (1<<depth) - 1;

  if ( im->format == XYBitmap )		/* binary bitmap */
  { psmap[0] = 1;
    psmap[1] = 0;
  } else if ( im->depth <= 8 )		/* colour-mapped */
  { int entries	= 1<<im->depth;
    int i;
    XColor data[256];

    for(i=0; i<entries; i++)
      data[i].pixel = i;

    XQueryColors(disp, cmap, data, entries);

    for(i=0; i<entries; i++)
    { int val = intensityXColor(&data[i]);

      psmap[i] = rescale(val, XBRIGHT, psbright);
    }
  } else				/* direct colour */
  { direct = TRUE;
  }

  w8 = roundup(w, 8);
  stat.count = 0;
  stat.val = 0;
  stat.bits = 8;
  stat.depth = depth;

  for(y = fy; y < h; y++)
  { if ( !direct )
    { for(x = fx; x < w8; x++)
      { int pixval;

	if ( mask && XGetPixel(mask, x, y) == 0L )
	  pixval = psbright;
	else
	  pixval = (x < w ? psmap[XGetPixel(im, x, y)] : psbright);

	put_value(&stat, pixval);
      }
    } else
    { int r_shift = shift_for_mask(im->red_mask);
      int g_shift = shift_for_mask(im->green_mask);
      int b_shift = shift_for_mask(im->blue_mask);
      int r_bright = im->red_mask   >> r_shift;
      int g_bright = im->green_mask >> g_shift;
      int b_bright = im->blue_mask  >> b_shift;

      DEBUG(NAME_image, Cprintf("Line %03d", y));
      for(x = fx; x < w8; x++)
      { unsigned long pixel;
	int pixval;
	int r, g, b;

	if ( mask && XGetPixel(mask, x, y) == 0L )
	{ if ( iscolor )
	  { put_value(&stat, psbright);
	    put_value(&stat, psbright);
	    put_value(&stat, psbright);
	  } else
	  { put_value(&stat, psbright);
	  }
	} else
	{ pixel = XGetPixel(im, x, y);
	  r = (pixel & im->red_mask)   >> r_shift;
	  g = (pixel & im->green_mask) >> g_shift;
	  b = (pixel & im->blue_mask)  >> b_shift;
	  DEBUG(NAME_image, Cprintf(" %x/%x/%x", r, g, b));

	  if ( depth == 1 )
	  { if ( r+g+b > (r_bright+g_bright+b_bright)/2 )
	      pixval = 1;
	    else
	      pixval = 0;

	    put_value(&stat, pixval);
	  } else
	  { r = rescale(r, r_bright, psbright);
	    g = rescale(g, g_bright, psbright);
	    b = rescale(b, b_bright, psbright);

	    if ( iscolor )
	    { put_value(&stat, r);
	      put_value(&stat, g);
	      put_value(&stat, b);
	    } else
	    { pixval = (x < w ? INTENSITY(r, g, b) : psbright);

	      put_value(&stat, pixval);
	    }
	  }
	}
      }
      DEBUG(NAME_image, Cprintf("\n"));
    }
  }

  succeed;
}


		 /*******************************
		 *       COLOUR HANDLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Return greyscale intensity of an XColor object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
intensityXColor(XColor *c)
{ unsigned int r = c->red;
  unsigned int g = c->green;
  unsigned int b = c->blue;

  return INTENSITY(r, g, b);
}


unsigned long
getPixelColour(Colour c, DisplayObj d)
{ XColor *color = (XColor *) getXrefObject(c, d);

  return color ? color->pixel : 0L;
}


void
x11_set_gc_foreground(DisplayObj d, Any fg, int gcs, GC *gc)
{ XGCValues values;
  unsigned long mask;
  DisplayWsXref r = d->ws_ref;

  if ( instanceOfObject(fg, ClassColour) )
  { unsigned long pixel = getPixelColour(fg, d);

    values.foreground = pixel;
    values.fill_style = FillSolid;
    mask	      = (GCForeground|GCFillStyle);
  } else
  { Pixmap pm   = (Pixmap) getXrefObject(fg, d);

    values.tile       = pm;
    values.fill_style = FillTiled;
    mask	      = (GCTile|GCFillStyle);
  }

  for(; gcs > 0; gcs--, gc++)
    XChangeGC(r->display_xref, *gc, mask, &values);
}


static int
distanceColours(Name vt, XColor *c1, XColor *c2)
{ if ( vt == NAME_greyScale )
  { int i1 = intensityXColor(c1);
    int i2 = intensityXColor(c2);

    return abs(i1 - i2);
  } else
  { int dr = ((int)c1->red - (int)c2->red) / 4;
    int dg = ((int)c1->green - (int)c2->green) / 4;
    int db = ((int)c1->blue - (int)c2->blue) / 4;

    return (int)sqrt((double)(dr*dr + dg*dg + db*db)) * 4;
  }
}


status
allocNearestColour(Display *display, Colormap map, int depth, Name vt,
		   XColor *c)
{ XColor *colors;
  int entries = 1<<depth;

  if ( (colors = alloc(entries * sizeof(XColor))) )
  { int i, j;

    for(i=0; i<entries; i++)
      colors[i].pixel = i;

    DEBUG(NAME_colour, Cprintf("Looking for %d %d %d\n",
			       c->red, c->green, c->blue));

    if ( isDefault(vt) )		/* TBD */
    { Visual *v = XDefaultVisual(display, DefaultScreen(display));
      int vclass = v->class;

      switch(vclass)
      { case StaticGray: vt = NAME_staticGrey;
        case GrayScale:	 vt = NAME_greyScale;
      }
    }

    XQueryColors(display, map, colors, entries);

    for(j=0; j<entries; j++)
    { XColor *cb = NULL;
      int badness = 1000000;
      XColor *e = colors;

      for(i=0; i<entries; i++, e++)
      { if ( e->flags != -1 )		/* tried this one */
	{ int d = distanceColours(vt, c, e);

	  if ( d < badness )
	  { cb = e;
	    badness = d;
	  }
	}
      }

      assert(cb);

      DEBUG(NAME_colour, Cprintf("Mapped colour %d %d %d --> %d %d %d\n",
				 c->red, c->green, c->blue,
				 cb->red, cb->green, cb->blue));

      *c = *cb;
      if ( XAllocColor(display, map, c) )
      { unalloc(entries * sizeof(XColor), colors);
	succeed;
      } else
      {	cb->flags = -1;			/* don't try this one anymore! */
	DEBUG(NAME_colour, Cprintf("Can't allocate, trying another one\n"));
      }
    }
  }

  fail;
}


		/********************************
		*      X-EVENT TRANSLATION	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This function gets the keyboard event id   from an Xevent. Function keys
are mapped to XPCE names. Normal keys   must  be mapped to their UNICODE
character.  This  is  a  bit  complicated.    According   to  the  docs,
XLookupString only returns ISO-Latin-1 characters.   In practice it also
appears to return multibyte sequences, especially   UTF-8. It is unclear
whether all X11 systems do this and whether the output is always UTF-8.

Dispite many references, XtSetLanguageProc() and  XLookupString() do not
ensure proper handling of dead keys (' +   e). Possibly this is due to a
weird combination of event-related X/Xt  functions   used  in  low level
XPCE, but even with browsing the X11 sources I could not find it. As the
long term aim was  to  move   to  XwcLookupString()  anyway, proper -but
simple- support for XIM has been implemented.

A good document is:

	http://home.catv.ne.jp/pp/ginoue/im/xim-e.html

One of the issues is the window for an IC. Can/should this always be the
frame? At the moment it is the  X11 window realising the event receiver.
Using the frame window for all XIM operations does not work.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef O_XIM
static void
adjustEventMask(Display *d, Window w, XIC ic)
{ XWindowAttributes winatts;
  long fevent;

  XGetICValues(ic, XNFilterEvents, &fevent, NULL);
  XGetWindowAttributes(d, w, &winatts);
  XSelectInput(d, w, winatts.your_event_mask|fevent);
}


static XIC
getICWindow(Any obj)
{ FrameObj fr;
  FrameWsRef wsfr;
  DisplayWsXref d;
  Widget w;

  if ( instanceOfObject(obj, ClassFrame) )
  { fr = obj;
    w = widgetFrame(fr);
    DEBUG(NAME_event, Cprintf("Associating IC with %s ...", pp(fr)));
  } else if ( instanceOfObject(obj, ClassWindow) )
  { PceWindow sw = obj;
    if ( !(fr = getFrameWindow(sw, OFF)) )
      fail;
    w = widgetWindow(sw);
    DEBUG(NAME_event, Cprintf("Associating IC with %s ...", pp(sw)));
  } else
    fail;

  if ( !w )
    fail;				/* possible on MacOS? */
  d = fr->display->ws_ref;

  if ( (wsfr = fr->ws_ref) && d->im )
  { XIC ic;
    Window xwin = XtWindow(w);

    if ( wsfr->ic )
    { if ( wsfr->icwin != xwin )
      { XSetICValues(wsfr->ic,
		     XNClientWindow, xwin,
		     NULL);
	wsfr->icwin = xwin;
	adjustEventMask(d->display_xref, xwin, wsfr->ic);
	DEBUG(NAME_event, Cprintf("Re-using IC %p (switched window)\n",
				  wsfr->ic));
      } else
      { DEBUG(NAME_event, Cprintf("Re-using IC %p\n", wsfr->ic));
      }

      return wsfr->ic;
    }

    ic = XCreateIC(d->im,
		   XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
		   XNClientWindow, xwin,
		   NULL);
    if( !ic )
    { DEBUG(NAME_event, Cprintf("Could not open X Input Context\n"));
      fail;
    }
    adjustEventMask(d->display_xref, xwin, ic);

    DEBUG(NAME_event, Cprintf("Created IC %p\n", ic));
    wsfr->ic    = ic;
    wsfr->icwin = xwin;

    return ic;
  }

  fail;
}

#endif /*O_XIM*/

static Any
keycode_to_name(Any sw, XEvent *event)
{ char buf[256];
  int count;
  KeySym sym;
#ifdef O_XIM
  XIC ic = getICWindow(sw);

  if ( ic )
  { charW wbuf[256];
    charW *winput = wbuf;
    Status status;
    int has_chars = FALSE;
    int has_sym = FALSE;

  again:
    count = XwcLookupString(ic, (XKeyPressedEvent*)event,
			    winput, sizeof(wbuf)/sizeof(wchar_t),
			    &sym, &status);
    DEBUG(NAME_event,
	  { int i;
	    Cprintf("Got %d wide characters:", count);
	    for(i=0; i<count; i++)
	      Cprintf(" %d", wbuf[i]);
		Cprintf("\n");
	  });

    switch(status)
    { case XBufferOverflow:
	winput = pceMalloc(sizeof(wchar_t)*(count+1));
        goto again;
      case XLookupNone:
	fail;
      case XLookupKeySym:
	has_sym = TRUE;
        break;
      case XLookupBoth:
	has_chars = has_sym = TRUE;
        break;
      case XLookupChars:
	has_chars = TRUE;
        break;
    }

    if ( has_sym )
    { switch(sym)
      { case XK_BackSpace:
	  if ( event->xkey.state & MetaMask )
	    return toInt(8+META_OFFSET);
					/* cannot be a buffer overflow */
	return NAME_backspace;
      }
    }

    if ( count == 1 )
    { int c = wbuf[0];

      if ( event->xkey.state & MetaMask )	/* meta depressed */
	c += META_OFFSET;

      return toInt(c);
    }

    if ( !has_sym )
    { if ( winput != wbuf )		/* TBD: multi-character input */
	pceFree(winput);

      fail;
    }
  } else
#endif /*O_XIM*/
  { count = XLookupString((XKeyPressedEvent *) event,
			  buf, sizeof(buf), &sym, NULL);
    DEBUG(NAME_event,
	  { int i;
	    Cprintf("Got %d characters:", count);
	    for(i=0; i<count; i++)
	      Cprintf(" %d", buf[i]&0xff);
		Cprintf("\n");
	  });

    switch(sym)				/* special ones */
    { case XK_BackSpace:
	if ( event->xkey.state & MetaMask )
	  return toInt(8+META_OFFSET);
        return NAME_backspace;
    }

    if ( count == 1 )
    { int c = buf[0] & 0xff;

      if ( event->xkey.state & MetaMask )	/* meta depressed */
	c += META_OFFSET;

      return toInt(c);
    }

    if ( count > 1 )			/* see above */
    { char *e;
      int c;

      if ( (e = utf8_get_char(buf, &c)) && e-buf == count )
      { DEBUG(NAME_event, Cprintf("\t-->UTF-8 sequence for %d\n", c));

	if ( event->xkey.state & MetaMask )	/* meta depressed */
	  c += META_OFFSET;

	return toInt(c);
      }
    }
  }

  switch(sym)
  { case XK_F1:		return NAME_keyTop_1;
    case XK_F2:		return NAME_keyTop_2;
    case XK_F3:		return NAME_keyTop_3;
    case XK_F4:		return NAME_keyTop_4;
    case XK_F5:		return NAME_keyTop_5;
    case XK_F6:		return NAME_keyTop_6;
    case XK_F7:		return NAME_keyTop_7;
    case XK_F8:		return NAME_keyTop_8;
    case XK_F9:		return NAME_keyTop_9;
    case XK_F10:	return NAME_keyTop_10;

    case XK_L1:		return NAME_keyLeft_1;
    case XK_L2:		return NAME_keyLeft_2;
    case XK_L3:		return NAME_keyLeft_3;
    case XK_L4:		return NAME_keyLeft_4;
    case XK_L5:		return NAME_keyLeft_5;
    case XK_L6:		return NAME_keyLeft_6;
    case XK_L7:		return NAME_keyLeft_7;
    case XK_L8:		return NAME_keyLeft_8;
    case XK_L9:		return NAME_keyLeft_9;
    case XK_L10:	return NAME_keyLeft_10;

    case XK_R1:		return NAME_keyRight_1;
    case XK_R2:		return NAME_keyRight_2;
    case XK_R3:		return NAME_keyRight_3;
    case XK_R4:		return NAME_keyRight_4;
    case XK_R5:		return NAME_keyRight_5;
    case XK_R6:		return NAME_keyRight_6;
    case XK_R7:		return NAME_keyRight_7;
    case XK_R8:		return NAME_keyRight_8;
    case XK_R9:		return NAME_keyRight_9;
    case XK_R10:	return NAME_keyRight_10;
    case XK_R11:	return NAME_keyRight_11;
    case XK_R12:	return NAME_keyRight_12;
    case XK_R13:	return NAME_keyRight_13;
    case XK_R14:	return NAME_keyRight_14;
    case XK_R15:	return NAME_keyRight_14;

/* Cursor motion */

#ifndef XK_Page_Up
#define XK_Page_Up XK_Prior
#endif
#ifndef XK_Page_Down
#define XK_Page_Down XK_Next
#endif

    case XK_Home:	return NAME_cursorHome;
    case XK_Left:	return NAME_cursorLeft;
    case XK_Up:		return NAME_cursorUp;
    case XK_Right:	return NAME_cursorRight;
    case XK_Down:	return NAME_cursorDown;
    case XK_Page_Up:	return NAME_pageUp;
    case XK_Page_Down:	return NAME_pageDown;
    case XK_End:	return NAME_end;
    case XK_Begin:	return NAME_begin;

/* Misc Functions */

    case XK_Select:	return NAME_select;
    case XK_Print:	return NAME_print;
    case XK_Execute:	return NAME_execute;
    case XK_Insert:	return NAME_insert;
    case XK_Undo:	return NAME_undo;
    case XK_Redo:	return NAME_redo;
    case XK_Menu:	return NAME_menu;
    case XK_Find:	return NAME_find;
    case XK_Cancel:	return NAME_cancel;
    case XK_Help:	return NAME_help;
    case XK_Break:	return NAME_break;
  }

  DEBUG(NAME_keysym, Cprintf("sym = 0x%X\n", (unsigned int)sym));

  fail;
}


static Int
state_to_buttons(unsigned int state, Name name)
{ int r = 0;

  if ( state & Button1Mask )	r |= BUTTON_ms_left;
  if ( state & Button2Mask )	r |= BUTTON_ms_middle;
  if ( state & Button3Mask )	r |= BUTTON_ms_right;
  if ( state & Button4Mask )	r |= BUTTON_ms_button4;
  if ( state & Button5Mask )	r |= BUTTON_ms_button5;
  if ( state & ShiftMask )	r |= BUTTON_shift;
  if ( state & ControlMask )	r |= BUTTON_control;
  if ( state & MetaMask )	r |= BUTTON_meta;

  if ( name == NAME_msLeftDown )
    r |= BUTTON_ms_left;
  else if ( name == NAME_msMiddleDown )
    r |= BUTTON_ms_middle;
  else if ( name == NAME_msRightDown )
    r |= BUTTON_ms_right;
  else if ( name == NAME_msButton4Down )
    r |= BUTTON_ms_button4;
  else if ( name == NAME_msButton5Down )
    r |= BUTTON_ms_button5;
  else if ( name == NAME_msLeftUp )
    r &= ~BUTTON_ms_left;
  else if ( name == NAME_msMiddleUp )
    r &= ~BUTTON_ms_middle;
  else if ( name == NAME_msRightUp )
    r &= ~BUTTON_ms_right;
  else if ( name == NAME_msButton4Up )
    r &= ~BUTTON_ms_button4;
  else if ( name == NAME_msButton5Up )
    r &= ~BUTTON_ms_button5;

  return toInt(r);
}


static Name
button_to_name(int press, unsigned int button)
{ switch( button )
  { case Button1:	return press ? NAME_msLeftDown     : NAME_msLeftUp;
    case Button2:	return press ? NAME_msMiddleDown   : NAME_msMiddleUp;
    case Button3:	return press ? NAME_msRightDown    : NAME_msRightUp;
    case Button4:	return press ? NAME_msButton4Down  : NAME_msButton4Up;
    case Button5:	return press ? NAME_msButton5Down  : NAME_msButton5Up;
  }

  fail;
}



EventObj
CtoEvent(Any window, XEvent *event)	/* window or frame */
{ Time time;
  int state = 0;
  int x;
  int y;
  Name ctx_name = NULL;
  Any name, ctx = NULL;
  static BoolObj do_wheel;
  EventObj ev;

  if ( !do_wheel )
    do_wheel = getClassVariableValueClass(ClassEvent, NAME_x11WheelMouse);

  switch( event->xany.type )
  { case KeyPress:			/* Key pressed */
    { x     = event->xkey.x;
      y     = event->xkey.y;
      state = event->xkey.state;
      time  = event->xkey.time;
      name  = keycode_to_name(window, event);
      if ( name == FAIL )
        fail;

      break;
    }
    case ButtonPress:			/* Button pressed/released */
    case ButtonRelease:
    { x     = event->xbutton.x;
      y     = event->xbutton.y;
      state = event->xbutton.state;
      time  = event->xbutton.time;

      if ( do_wheel )			/* Map button4 and 5 to wheel */
      { if ( event->xbutton.button == Button4 ||
	     event->xbutton.button == Button5 )
	{ if ( event->xany.type == ButtonPress )
	  { name = NAME_wheel;
	    ctx_name = NAME_rotation;
	    if ( event->xbutton.button == Button4 )
	      ctx = toInt(120);
	    else
	      ctx = toInt(-120);
	    break;
	  }
	  fail;
	}
      }

      name  = button_to_name(event->xany.type == ButtonPress,
			     event->xbutton.button);
      if ( name == FAIL )
        fail;

      break;
    }
    case MotionNotify:			/* Pointer motion events */
    { x     = event->xmotion.x;
      y     = event->xmotion.y;
      state = event->xmotion.state;
      time  = event->xmotion.time;

      if ( state & Button1Mask )
        name = NAME_msLeftDrag;
      else if ( state & Button2Mask )
        name = NAME_msMiddleDrag;
      else if ( state & Button3Mask )
        name = NAME_msRightDrag;
      else
        name = NAME_locMove;

      break;
    }
    case EnterNotify:
    case LeaveNotify:
    { x     = event->xcrossing.x;
      y     = event->xcrossing.y;
      state = event->xcrossing.state;
      time  = event->xcrossing.time;

#     define AnyButtonMask (Button1Mask|Button2Mask|Button3Mask)

      if ( event->xany.type == EnterNotify )
      { name = (state & AnyButtonMask ? NAME_areaResume
		 		      : NAME_areaEnter);
      } else
      { name = (state & AnyButtonMask ? NAME_areaCancel
			    	      : NAME_areaExit);
      }

      break;
    }
   default:				/* unknown event: do not convert */
      fail;
  }

  setLastEventTime(time);

  ev = answerObject(ClassEvent,
		    name,
		    window,
		    toInt(x), toInt(y),
		    state_to_buttons(state, name),
		    EAV);
  if ( ctx_name )
    attributeObject(ev, ctx_name, ctx);

  return ev;
}


typedef struct
{ const char *name;
  unsigned int mask;
} modmask;

static const modmask modmasks[] =
{ { "mod1", Mod1Mask },
  { "mod2", Mod2Mask },
  { "mod3", Mod3Mask },
  { "mod4", Mod4Mask },
  { "mod5", Mod5Mask },
  { NULL, 0 }
};


status
metaModifierDisplay(DisplayObj d, Name name)
{ const char *s = strName(name);
  const modmask *mm = modmasks;

  for( ; mm->name; mm++)
  { if ( streq(s, mm->name) )
    { MetaMask = mm->mask;
      succeed;
    }
  }

  fail;
}


		 /*******************************
		 *      COMPATIBILITY HACKS	*
		 *******************************/

#if XT_REVISION <= 3

Status XSetWMProtocols (dpy, w, protocols, count)
    Display *dpy;
    Window w;
    Atom *protocols;
    int count;
{   static Atom a = None;

    if ( a == None )
	a = XInternAtom (dpy, "WM_PROTOCOLS", False);

    XChangeProperty (dpy, w, a, XA_ATOM, 32,
		     PropModeReplace, (unsigned char *) protocols, count);
    return True;
}

#endif

#if !defined(HAVE_XTPOPUPSPRINGLOADED) && !defined(__sun__)
					/* sun's X11R5 fails on AC_HAVE_FUNC */
					/* for X11 functions */

void XtPopupSpringLoaded (widget)
    Widget widget;
{
    _XtPopup(widget, XtGrabExclusive, True);
}

#endif




