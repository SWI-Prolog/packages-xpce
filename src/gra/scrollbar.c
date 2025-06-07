/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www-swi-prolog.org
    Copyright (c)  1985-2025, University of Amsterdam
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
#include <h/graphics.h>

#ifdef __WINDOWS__			/* timer broken in Win95 */
#define SCROLLTIMER_USE_SLEEP 1
#endif

#define swap(x, y)	{ int z; z=x; x=y; y=z; }
#define swapInt(x, y)	{ Int z; z=x; x=y; y=z; }
#define BOUNDS(n, l, h) ((n) > (h) ? (h) : (n) < (l) ? (l) : (n))
#define MIN_BUBBLE 6			/* smallest variable bubble */

#define Repeating(sb) ((sb)->status == NAME_repeatDelay || \
		       (sb)->status == NAME_repeat)

typedef struct bubble_info *BubbleInfo;
typedef struct sb_draw_data *SbDrawData;

struct bubble_info
{ int	start;				/* start of bubble */
  int	length;				/* lenght of bubble */
  int	bar_start;			/* start of the bar */
  int   bar_length;			/* length of the bar */
};

struct sb_draw_data
{ int x, y, w, h;			/* from initialiseDeviceGraphical() */
  int vertical;				/* true if vertical bar */
  int arrow;				/* height of arrows */
  struct bubble_info bubble;		/* bubble info */
};


static void	compute_bubble(ScrollBar, struct bubble_info *,
			       int start, int min_bubble,
			       int fixed_bubble);
static status	orientationScrollBar(ScrollBar s, Name or);
static status	lookScrollBar(ScrollBar s, Name look);
static int	offset_event_scrollbar(ScrollBar s, EventObj ev);
static status	forwardScrollBar(ScrollBar s);
static Int	promilage_event_scrollbar(ScrollBar s, EventObj ev);
static status	detachTimerScrollBar(ScrollBar s);

static status
initialiseScrollBar(ScrollBar s, Any obj, Name orientation, Message msg)
{ Int w = getClassVariableValueObject(s, NAME_width);

  if ( !w || isDefault(w) )
    w = ws_default_scrollbar_width();

  initialiseGraphical(s, ZERO, ZERO, w, toInt(100));
  assign(s, cursor, getClassVariableValueObject(s, NAME_cursor));
  assign(s, orientation,   NAME_vertical);

  assign(s, view,	   toInt(-1));	/* length of view */
  assign(s, start,	   toInt(-1));	/* position in object */
  assign(s, length,	   toInt(-1));	/* length of scrollable object */

  assign(s, bubble_start,  toInt(-1));
  assign(s, bubble_length, toInt(-1));

  assign(s, message,	   msg);
  assign(s, object,	   obj);

  assign(s, drag,	   ON);
  assign(s, amount,	   ZERO);
  assign(s, direction,	   NAME_forwards);
  assign(s, unit,	   NAME_file);
  assign(s, status,	   NAME_inactive);
  assign(s, offset,	   ZERO);

  obtainClassVariablesObject(s);
  if ( orientation == NAME_horizontal )
    orientationScrollBar(s, orientation);

  requestComputeGraphical(s, DEFAULT);

  succeed;
}


		/********************************
		*            COMPUTE		*
		********************************/

static int
arrow_height_scrollbar(ScrollBar sb)
{ if ( sb->look == NAME_gtk ||
       sb->look == NAME_win )
  { int ah;

    if ( (ah = ws_arrow_height_scrollbar(sb)) < 0 )
    { ah = valInt(sb->orientation == NAME_vertical ? sb->area->w
						   : sb->area->h);
    }

    return ah;
  }

  return 0;
}


static status
ComputeScrollBar(ScrollBar sb)
{ if ( notNil(sb->request_compute) )
  { struct bubble_info bi;
    int arrow = arrow_height_scrollbar(sb);

    compute_bubble(sb, &bi, arrow, MIN_BUBBLE, FALSE);
    if ( valInt(sb->bubble_start) != bi.start ||
	 valInt(sb->bubble_length) != bi.length )
    { DEBUG(NAME_scroll, Cprintf("%s: start %ld --> %d; length %ld --> %d\n",
				 pp(sb), valInt(sb->bubble_start), bi.start,
				 valInt(sb->bubble_length), bi.length));
      assign(sb, bubble_start, toInt(bi.start));
      assign(sb, bubble_length, toInt(bi.length));

      CHANGING_GRAPHICAL(sb, changedEntireImageGraphical(sb));
    }

    assign(sb, request_compute, NIL);
  }

  succeed;
}


Int
getMarginScrollBar(ScrollBar sb)
{ if ( sb->displayed == OFF )
    answer(ZERO);

  if ( sb->orientation == NAME_horizontal )
  { if ( memberChain(sb->placement, NAME_bottom) )
      answer(add(sb->area->h, sb->distance));
    else
      answer(neg(add(sb->area->h, sb->distance)));
  } else				/* vertical */
  { if ( memberChain(sb->placement, NAME_right) )
      answer(add(sb->area->w, sb->distance));
    else
      answer(neg(add(sb->area->w, sb->distance)));
  }
}


status
placeScrollBar(ScrollBar sb, Graphical gr)
{ if ( isDefault(gr) )
    gr = sb->object;

  if ( instanceOfObject(gr, ClassGraphical) )
  { if ( sb->orientation == NAME_horizontal )
    { setGraphical(sb,
		   gr->area->x,
		   memberChain(sb->placement, NAME_bottom)
		       ? add(gr->area->y, add(gr->area->h, sb->distance))
		       : sub(gr->area->y, add(sb->area->h, sb->distance)),
		   gr->area->w,
		   DEFAULT);
    } else				/* vertical */
    { setGraphical(sb,
		   memberChain(sb->placement, NAME_right)
			? add(gr->area->x, add(gr->area->w, sb->distance))
			: sub(gr->area->x, add(sb->area->w, sb->distance)),
		   gr->area->y,
		   DEFAULT,
		   gr->area->h);
    }
  }

  succeed;
}


static status
computeScrollBar(ScrollBar sb)
{ if ( notNil(sb->request_compute) )
  { if ( hasSendMethodObject(sb->object, NAME_bubbleScrollBar) )
      send(sb->object, NAME_bubbleScrollBar, sb, EAV);
    else if ( hasGetMethodObject(sb->object, NAME_start) &&
	      hasGetMethodObject(sb->object, NAME_view) &&
	      hasGetMethodObject(sb->object, NAME_length) )
    { Int s = getv(sb->object, NAME_start, 0, NULL);
      Int v = getv(sb->object, NAME_view, 0, NULL);
      Int l = getv(sb->object, NAME_length, 0, NULL);

      if ( s != FAIL && v != FAIL && l != FAIL )
	bubbleScrollBar(sb, l, s, v);

    }

    ComputeScrollBar(sb);
  }

  succeed;
}


static void
compute_bubble(ScrollBar s, struct bubble_info *bi,
	       int bar_start, int min_bubble, int fixed_bubble)
{ int len   = valInt(s->length);
  int start = (valInt(s->start) > len ? len : valInt(s->start));
  int view  = valInt(s->view);

					/* bar sizes */
  bi->bar_start   = bar_start;
  bi->bar_length  = (s->orientation == NAME_vertical
			 ? valInt(s->area->h)
			 : valInt(s->area->w));
  bi->bar_length -= 2 * bi->bar_start;

					/* bubble characteristics */
  if ( fixed_bubble )
  { int free, below, above;

    if ( bi->bar_length < min_bubble )
    { bi->bar_length += 2 * bi->bar_start;
      bi->bar_start = 0;

      if ( bi->bar_length < min_bubble )
	min_bubble = bi->bar_length;
    }

    bi->length = min_bubble;
    free  = bi->bar_length - bi->length;
    below = len - (start+view);
    above = start;

    if ( (above + below) <= 0 )
      bi->start = 0;
    else
      bi->start = (free * above) / (above + below);
  } else
  { double bubble_prom = (len != 0 ? (double)start/(double)len
				   : 0.0);
    double bubble_lp   = (len != 0 ? (double)valInt(s->view)/(double)len
				   : 1.0);

    bi->length = (int)((double)bi->bar_length * bubble_lp) + min_bubble;
    bi->start  = (int)((double)bi->bar_length * bubble_prom) - min_bubble/2;
  }

  bi->start    = BOUNDS(bi->start, 0, bi->bar_length-min_bubble);
  bi->start   += bi->bar_start;
  bi->length   = BOUNDS(bi->length, 0,
			(bi->bar_length + bi->bar_start - bi->start));
}


		 /*******************************
		 *		REDRAW		*
		 *******************************/

static void
sb_init_draw_data(ScrollBar s, Area a, SbDrawData d, Any bg)
{ int m;

  initialiseDeviceGraphical(s, &d->x, &d->y, &d->w, &d->h);
  NormaliseArea(d->x, d->y, d->w, d->h);

  if ( instanceOfObject(bg, ClassElevation) )
  { Elevation z = bg;

    r_3d_box(d->x, d->y, d->w, d->h, 0, bg, TRUE);

    m = labs(valInt(z->height)) + 1;
    d->x += m;
    d->y += m;
    d->w -= 2*m;
    d->h -= 2*m;
  } else
    m = 0;

  d->vertical = (s->orientation == NAME_vertical);
  d->arrow = arrow_height_scrollbar(s);
  compute_bubble(s, &d->bubble,
		 m ? d->arrow - 1 : d->arrow,
		 MIN_BUBBLE, FALSE);
  d->bubble.start -= m;
  d->arrow -= 2*m;
}

static Elevation
getElevationScrollBar(ScrollBar s)
{ Elevation z = getClassVariableValueObject(s, NAME_elevation);

					/* TBD: make default one */
  return z;
}


static void
draw_arrow(ScrollBar s, int x, int y, int w, int h, Name which, int up)
{ if ( !ws_draw_scrollbar_arrow(s, x, y, w, h, which, up) )
  { Elevation z = getElevationScrollBar(s);

    DEBUG(NAME_arrow, Cprintf("Arrow box(%d, %d, %d, %d)\n", x, y, w, h));

    Image img;
    int iw, ih;

    r_thickness(valInt(s->pen));

    if ( up )
      r_3d_box(x, y, w, h, 0, z, TRUE);
    else
      r_box(x, y, w, h, 0, isDefault(z->colour) ? NIL : (Any) z->colour);

         if ( which == NAME_up )       img = SCROLL_UP_IMAGE;
    else if ( which == NAME_down )     img = SCROLL_DOWN_IMAGE;
    else if ( which == NAME_left )     img = SCROLL_LEFT_IMAGE;
    else /* ( which == NAME_right ) */ img = SCROLL_RIGHT_IMAGE;

    if ( img )
    { iw = valInt(img->size->w);
      ih = valInt(img->size->h);

      r_image(img, 0, 0, x+(w-iw)/2, y+(h-ih)/2, iw, ih, ON);
    } else
    { Cprintf("No scroll_bar arrow image\n");
    }
  }
}


static void
draw_arrows(ScrollBar s, SbDrawData d)
{ int faup = TRUE;			/* first-arrow-up */
  int saup = TRUE;			/* second-arrow-up */
  int ah = d->arrow;

  if ( Repeating(s) && s->unit == NAME_line )
  { if ( s->direction == NAME_forwards )
      saup = FALSE;
    else
      faup = FALSE;
  }

  if ( d->vertical )
  { draw_arrow(s, d->x, d->y, d->w, ah, NAME_up, faup);
    draw_arrow(s, d->x, d->y + d->h - ah, d->w, ah, NAME_down, saup);
  } else
  { draw_arrow(s, d->x, d->y, ah, d->h, NAME_left, faup);
    draw_arrow(s, d->x + d->w-ah, d->y, ah, d->h, NAME_right, saup);
  }
}


static void
draw_bubble(ScrollBar s, SbDrawData d)
{ int p = valInt(s->pen);
  Elevation z = getClassVariableValueObject(s, NAME_elevation);
  int x = d->x, y = d->y, w = d->w, h = d->h;
  BubbleInfo bi = &d->bubble;
  int pf=FALSE, pb=FALSE;

  if ( !instanceOfObject(z, ClassElevation) )
    z = NULL;

  if ( s->look == NAME_win &&
       Repeating(s) &&
       s->unit == NAME_page )
  { if ( s->direction == NAME_forwards )
      pf = TRUE;
    else
      pb = TRUE;
  }

  if ( d->vertical )
  { int ym, hm;

    x += p;
    w -= 2*p;

    ym = y+bi->bar_start; hm = bi->start - bi->bar_start;
    if ( pb )
      r_fill(x, ym, w, hm, BLACK_COLOUR);
    else if ( s->look == NAME_win )
      r_fill(x, ym, w, hm, GREY50_COLOUR);
    else
      r_clear(x, ym, w, hm);

    ym = y+bi->start;
    hm = bi->length;
    if ( !ws_draw_sb_thumb(x, ym, w, hm) )
    { if ( z )
	r_3d_box(x, ym, w, hm, 0, z, TRUE);
      else
	r_fill(x, ym, w, hm, GREY50_COLOUR);
    }

    ym += hm;
    hm = (bi->bar_start+bi->bar_length) - (bi->start+bi->length);
    if ( hm > 0 )
    { if ( pf )
	r_fill(x, ym, w, hm, BLACK_COLOUR);
      else if ( s->look == NAME_win && z )
	r_fill(x, ym, w, hm, GREY50_COLOUR);
      else
	r_clear(x, ym, w, hm);
    }
  } else /* horizontal */
  { int xm, wm;

    y += p;
    h -= 2*p;

    xm = x+bi->bar_start; wm = bi->start - bi->bar_start;
    if ( pb )
      r_fill(xm, y, wm, h, BLACK_COLOUR);
    else if ( s->look == NAME_win && z )
      r_fill(xm, y, wm, h, GREY50_COLOUR);
    else
      r_clear(xm, y, wm, h);

    xm = x+bi->start;
    wm = bi->length;
    if ( !ws_draw_sb_thumb(xm, y, wm, h) )
    { if ( z )
	r_3d_box(xm, y, wm, h, 0, z, TRUE);
      else
	r_fill(xm, y, wm, h, GREY50_COLOUR);
    }

    xm += wm;
    wm = (bi->bar_start+bi->bar_length) - (bi->start+bi->length);
    if ( wm > 0 )
    { if ( pf )
	r_fill(xm, y, wm, h, BLACK_COLOUR);
      else if ( s->look == NAME_win && z )
	r_fill(xm, y, wm, h, GREY50_COLOUR);
      else
	r_clear(xm, y, wm, h);
    }
  }
}


static status
RedrawAreaScrollBar(ScrollBar s, Area a)
{ Any bg = getClassVariableValueObject(s, NAME_background);
  Any obg = NIL;
  Elevation z = NIL;

  if ( bg )
  { if ( instanceOfObject(bg, ClassColour) ||
	 instanceOfObject(bg, ClassPixmap) )
      obg = r_background(bg);
    else if ( instanceOfObject(bg, ClassElevation) )
    { z = bg;
      if ( instanceOfObject(z->colour, ClassColour) )
	obg = r_background(z->colour);
    }
  }

  struct sb_draw_data d;
  sb_init_draw_data(s, a, &d, z);
  draw_bubble(s, &d);
  if ( d.arrow )
    draw_arrows(s, &d);

  if ( notNil(obg) )
    r_background(obg);

  return RedrawAreaGraphical(s, a);
}


static Timer	ScrollBarRepeatTimer;
static Message  ScrollBarRepeatMessage;

static Timer
scrollBarRepeatTimer()
{ if ( !ScrollBarRepeatTimer )
    ScrollBarRepeatTimer = globalObject(NAME_scrollBarRepeatTimer,
					ClassTimer, CtoReal(0.08),
					( ScrollBarRepeatMessage =
					  newObject(ClassMessage, NIL,
						    NAME_repeat, EAV)), EAV);
  return ScrollBarRepeatTimer;
}


static status
repeatScrollBar(ScrollBar s)
{ again:

  if ( getIsDisplayedGraphical((Graphical)s, DEFAULT) != ON )
  { DEBUG(NAME_repeat, Cprintf("%s: no longer displayed\n", pp(s)));

    detachTimerScrollBar(s);
    fail;
  }

  if ( Repeating(s) )
  { unsigned long clk = mclock();

    if ( s->unit == NAME_page )
    { if ( s->direction == NAME_backwards )
      { if ( valInt(s->start) <= 0 )
	{ detachTimerScrollBar(s);
	  succeed;
	}
      } else if ( valInt(s->view) + valInt(s->start) >= valInt(s->length) )
      { detachTimerScrollBar(s);
	succeed;
      }
    }
    forwardScrollBar(s);
    synchroniseGraphical((Graphical) s, ON);
    if ( Repeating(s) )		/* synchroniseGraphical() can handle up */
    { Real t = getClassVariableValueObject(s, NAME_repeatInterval);
      int ct = (int)(valReal(t) * 1000.0) - (float)(mclock() - clk);

      assign(s, status, NAME_repeat);

      if ( ct > 5 )
      {
#ifdef SCROLLTIMER_USE_SLEEP
	msleep(ct);
	goto again;
#else
        Timer tmr = scrollBarRepeatTimer();

	intervalTimer(tmr, CtoReal((float)ct / 1000.0));
	statusTimer(tmr, NAME_once);
#endif
      } else
	goto again;
    }
  }

  succeed;
}

static status
detachTimerScrollBar(ScrollBar s)
{ if ( ScrollBarRepeatMessage && ScrollBarRepeatMessage->receiver == s )
  { stopTimer(ScrollBarRepeatTimer);
    assign(ScrollBarRepeatMessage, receiver, NIL);

    succeed;
  }

  fail;
}


static void
attachTimerScrollBar(ScrollBar s)
{ Timer t = scrollBarRepeatTimer();

  detachTimerScrollBar(s);
  intervalTimer(t, getClassVariableValueObject(s, NAME_repeatDelay));
  assign(ScrollBarRepeatMessage, receiver, s);
  startTimer(t, NAME_once);
}


static status
unlinkScrollBar(ScrollBar s)
{ detachTimerScrollBar(s);

  return unlinkGraphical((Graphical) s);
}


static status
barEventScrollBar(ScrollBar s, EventObj ev)
{ if ( !isAEvent(ev, NAME_button) )
    fail;

  if ( isAEvent(ev, NAME_msLeft) )
  { int vertical = (s->orientation == NAME_vertical);
    int ah = ws_arrow_height_scrollbar(s);
    Int w = s->area->w;
    Int h = s->area->h;
    int offset = offset_event_scrollbar(s, ev);
    int len = (vertical ? valInt(h) : valInt(w));

    if ( ah < 0 )
      ah = (vertical ? valInt(w) : valInt(h));

    if ( isAEvent(ev, NAME_msLeftDown) )
    { DEBUG(NAME_scrollBar,
	    Cprintf("%s: received ms_left_down at %d of %d\n",
		    pp(s), offset, len));

      if ( offset < ah )		/* line-up */
      { assign(s, unit,      NAME_line);
	assign(s, direction, NAME_backwards);
	assign(s, amount,    ONE);
	assign(s, status,    NAME_repeatDelay);
      } else if ( offset > len - ah )	/* line-down */
      { assign(s, unit,      NAME_line);
	assign(s, direction, NAME_forwards);
	assign(s, amount,    ONE);
	assign(s, status,    NAME_repeatDelay);
      } else				/* not on the arrows */
      { struct bubble_info bi;

	compute_bubble(s, &bi, ah, MIN_BUBBLE, FALSE);

	if ( offset < bi.start )	/* page-up */
	{ assign(s, unit,      NAME_page);
	  assign(s, direction, NAME_backwards);
	  assign(s, amount,    toInt(SCROLL_PAGE_PROM));
	  assign(s, status,    NAME_repeatDelay);
	} else if ( offset > bi.start + bi.length ) /* page-down */
	{ assign(s, unit,      NAME_page);
	  assign(s, direction, NAME_forwards);
	  assign(s, amount,    toInt(SCROLL_PAGE_PROM));
	  assign(s, status,    NAME_repeatDelay);
	} else				/* on the bubble */
	{ assign(s, unit,      NAME_file);
	  assign(s, direction, NAME_goto);
	  assign(s, amount,    promilage_event_scrollbar(s, ev));
	  assign(s, offset,    toInt(offset - bi.start));
	  assign(s, status,    NAME_running);
	}
      }

      if ( s->status == NAME_repeatDelay )
      { attachTimerScrollBar(s);
	changedEntireImageGraphical(s);
      }
    } else if ( isAEvent(ev, NAME_msLeftDrag) && s->status == NAME_running )
    { int offset = offset_event_scrollbar(s, ev);
      struct bubble_info bi;
      int prom;

      compute_bubble(s, &bi, ah, MIN_BUBBLE, FALSE);
      if ( bi.bar_length <= bi.length )
	prom = 0;			/* avoid division by 0 */
      else
	prom = ((offset - bi.bar_start - valInt(s->offset)) * 1000) /
		   (bi.bar_length - bi.length);
      prom = BOUNDS(prom, 0, 1000);

      assign(s, amount, toInt(prom));
      forwardScrollBar(s);
    } else if ( isAEvent(ev, NAME_msLeftUp) )
    { if ( s->unit != NAME_file && s->status != NAME_repeat )
	forwardScrollBar(s);

      assign(s, status, NAME_inactive);

      if ( detachTimerScrollBar(s) )
	changedEntireImageGraphical(s);
    }

    succeed;
  }

  if ( isAEvent(ev, NAME_msMiddle) )
  { if ( isAEvent(ev, NAME_msMiddleDown) )
    { assign(s, unit,      NAME_file);
      assign(s, direction, NAME_goto);
      assign(s, amount,    promilage_event_scrollbar(s, ev));
      assign(s, status,    NAME_running);

      forwardScrollBar(s);
    } else if ( s->status == NAME_running &&
		s->drag == ON &&
		isAEvent(ev, NAME_msMiddleDrag) )
    { assign(s, amount,    promilage_event_scrollbar(s, ev));
      forwardScrollBar(s);
    } else if ( isAEvent(ev, NAME_msMiddleUp) )
    { assign(s, status, NAME_inactive);
    }

    succeed;
  }

  fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Scrollbar event handling.  Status field:

  inactive:	Doing nothing; pointer is outside the scrollbar.
  active:	Pointer is in the scrollbar, no button pressed.
  running:	A button is depressed.  Handle events to the up.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
offset_event_scrollbar(ScrollBar s, EventObj ev)
{ if ( s->orientation == NAME_horizontal )
    return valInt(getXEvent(ev, s));
  else
    return valInt(getYEvent(ev, s));
}


static Int
promilage_event_scrollbar(ScrollBar s, EventObj ev)
{ struct bubble_info bi;
  int ah = arrow_height_scrollbar(s);
  int offset = offset_event_scrollbar(s, ev);
  int rval;

  compute_bubble(s, &bi, ah, MIN_BUBBLE, FALSE);
  rval = ((offset - bi.bar_start) * 1000) / bi.bar_length;

  return toInt(BOUNDS(rval, 0, 1000));
}


static status
forwardScrollBar(ScrollBar s)
{ if ( isNil(s->message) )
    succeed;

  if ( isDefault(s->message) )
  { send(s->object,
	 (equalName(s->orientation, NAME_horizontal)
	  ? NAME_scrollHorizontal
	  : NAME_scrollVertical),
	 s->direction, s->unit, s->amount, EAV);
  } else
    forwardReceiverCode(s->message, s->object,
			s->direction, s->unit, s->amount, EAV);

  succeed;
}


static status
eventScrollBar(ScrollBar s, EventObj ev)
{ if ( mapWheelMouseEvent(ev, s->object) )
    succeed;

  if ( barEventScrollBar(s, ev) )
    succeed;

  return eventGraphical(s, ev);
}


static status
orientationScrollBar(ScrollBar s, Name or)
{ if ( s->orientation == or )
    succeed;

  CHANGING_GRAPHICAL(s,
	swapInt(s->area->h, s->area->w);
	assign(s, orientation, or);
	changedEntireImageGraphical(s));

  succeed;
}


static status
viewScrollBar(ScrollBar s, Int n)
{ if (valInt(n) < 0)
    n = ZERO;

  if ( s->view != n )
  { assign(s, view, n);
    requestComputeGraphical(s, DEFAULT);
  }

  succeed;
}


static status
startScrollBar(ScrollBar s, Int n)
{ if (valInt(n) < 0)
    n = ZERO;

  if ( s->start != n )
  { assign(s, start, n);
    requestComputeGraphical(s, DEFAULT);
  }

  succeed;
}


static status
lengthScrollBar(ScrollBar s, Int n)
{ if (valInt(n) < 0)
    n = ZERO;

  if ( s->length != n )
  { assign(s, length, n);
    requestComputeGraphical(s, DEFAULT);
  }

  succeed;
}


status
bubbleScrollBar(ScrollBar sb, Int l, Int s, Int v)
{ if ( valInt(l) < 0 ) l = ZERO;
  if ( valInt(s) < 0 ) s = ZERO;
  if ( valInt(v) < 0 ) v = ZERO;

  if ( sb->length == l && sb->start == s && sb->view == v )
    succeed;

  DEBUG(NAME_scroll, Cprintf("bubbleScrollBar(%s, %d, %d, %d)\n",
			     pp(sb), valInt(l), valInt(s), valInt(v)));

  assign(sb, length, l);
  assign(sb, start,  s);
  assign(sb, view,   v);

  if ( sb->auto_hide == ON &&
       hasSendMethodObject(sb->object, NAME_showScrollBar) )
  { if ( s == ZERO && valInt(v) >= valInt(l) ) /* all is shown */
    { if ( sb->displayed == ON )
      { if ( send(sb->object, NAME_showScrollBar, OFF, sb, EAV) )
	  succeed;
      }
    } else
    { if ( sb->displayed == OFF )
      { send(sb->object, NAME_showScrollBar, ON, sb, EAV);
      }
    }
  }

  return requestComputeGraphical(sb, DEFAULT);
}


static status
autoHideScrollBar(ScrollBar sb, BoolObj val)
{ if ( sb->auto_hide != val )
  { assign(sb, auto_hide, val);
    requestComputeGraphical(sb, DEFAULT);
  }

  succeed;
}


static status
lookScrollBar(ScrollBar s, Name look)
{ CHANGING_GRAPHICAL(s,
		     assign(s, look, look);
		     assign(s, distance, toInt(1));
		     changedEntireImageGraphical(s));

  succeed;
}


static status
convertLoadedObjectScrollBar(ScrollBar sb, Int ov, Int nv)
{ if ( isName(sb->placement) )
  { Chain ch = newObject(ClassChain, EAV);
    static char *names[] = {"left", "right", "top", "bottom"};
    int i;

    for(i=0; i<4; i++)
    { Name place = CtoKeyword(names[i]);

      if ( send(sb->placement, NAME_sub, place, ON, EAV) )
	appendChain(ch, place);
    }
    assign(sb, placement, ch);
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_convertLoadedObject[] =
        { "int", "int" };
static char *T_bubble[] =
        { "length=int", "start=int", "view=int" };
static char *T_initialise[] =
        { "object=object", "orientation={horizontal,vertical}", "message=[code]*" };

/* Instance Variables */

static vardecl var_scrollBar[] =
{ IV(NAME_message, "[code]*", IV_BOTH,
     NAME_action, "Message used to inform object"),
  IV(NAME_object, "graphical*", IV_BOTH,
     NAME_client, "Graphical to be scrolled scrolled"),
  IV(NAME_placement, "chain", IV_GET,
     NAME_layout, "Relative automatic placement"),
  IV(NAME_distance, "int", IV_GET,
     NAME_layout, "Relative distance (pixels)"),
  IV(NAME_status, "name", IV_GET,
     NAME_event, "Current status for event parsing"),
  SV(NAME_orientation, "{horizontal,vertical}", IV_GET|IV_STORE, orientationScrollBar,
     NAME_appearance, "Scroll object horizontal or vertical"),
  SV(NAME_view, "int", IV_GET|IV_STORE, viewScrollBar,
     NAME_scroll, "Length of visible part"),
  SV(NAME_start, "int", IV_GET|IV_STORE, startScrollBar,
     NAME_scroll, "Start of visible part"),
  SV(NAME_length, "int", IV_GET|IV_STORE, lengthScrollBar,
     NAME_scroll, "Total length of object"),
  IV(NAME_bubbleStart, "int", IV_NONE,
     NAME_internal, "Pixel position of bubble"),
  IV(NAME_bubbleLength, "int", IV_NONE,
     NAME_internal, "Pixel size of bubble"),
  SV(NAME_look, "{win,gtk}", IV_GET|IV_STORE, lookScrollBar,
     NAME_appearance, "Look-and-feel"),
  IV(NAME_drag, "bool", IV_BOTH,
     NAME_event, "If @on, messages are sent continuously"),
  IV(NAME_amount, "int", IV_NONE,
     NAME_internal, "Amount to scroll"),
  IV(NAME_direction, "{forwards,backwards,goto}", IV_NONE,
     NAME_internal, "Direction in which to scroll or jump"),
  IV(NAME_unit, "{line,page,file}", IV_NONE,
     NAME_internal, "Unit to scroll"),
  IV(NAME_offset, "int", IV_NONE,
     NAME_internal, "Offset of down from top of bubble"),
  SV(NAME_autoHide, "bool", IV_GET|IV_STORE, autoHideScrollBar,
     NAME_internal, "If @on, hide if all is visible")
};

/* Send Methods */

static senddecl send_scrollBar[] =
{ SM(NAME_compute, 0, NULL, computeScrollBar,
     DEFAULT, "Recompute the scrollbar values"),
  SM(NAME_convertLoadedObject, 2, T_convertLoadedObject, convertLoadedObjectScrollBar,
     DEFAULT, "Convert placement attribute"),
  SM(NAME_event, 1, "event", eventScrollBar,
     DEFAULT, "Process a user event"),
  SM(NAME_initialise, 3, T_initialise, initialiseScrollBar,
     DEFAULT, "Create from object, orientation and message"),
  SM(NAME_unlink, 0, NULL, unlinkScrollBar,
     DEFAULT, "Stop/disconnect repeat timer"),
  SM(NAME_place, 1, "[graphical]", placeScrollBar,
     NAME_area, "Position scrollbar relative to object"),
  SM(NAME_bubble, 3, T_bubble, bubbleScrollBar,
     NAME_scroll, "Set length, start and view"),
  SM(NAME_repeat, 0, NULL, repeatScrollBar,
     NAME_scroll, "Repeat last action")
};

/* Get Methods */

#define get_scrollBar NULL
/*
static getdecl get_scrollBar[] =
{
};
*/

/* Resources */

static classvardecl rc_scrollBar[] =
{ RC(NAME_background, "[elevation|colour|pixmap]",
     UXWIN("elevation(@nil, 1, grey66)", "win_window"),
     "Colour of background parts"),
  RC(NAME_colour, "[colour]", UXWIN("@_dialog_bg", "win_btnface"),
     "Colour of foreground parts"),
  RC(NAME_distance, "int", UXWIN("2", "0"),
     "Distance to graphical"),
  RC(NAME_elevation, "elevation*",
     UXWIN("elevation(@nil, 1, @_dialog_bg)",
	   "elevation(@nil, 2, win_menu)"),
     "3-D effect elevation"),
  RC(NAME_look, "{x,open_look,motif,gtk,win}", UXWIN("gtk", "win"),
     "Look-and-feel"),
  RC(NAME_pen, "int", UXWIN("@_win_pen", "0"),
     "Thickness of surrounding box"),
  RC(NAME_placement, "chain", "[right,bottom]",
     "Relative placement"),
  RC(NAME_repeatDelay, "real", "0.35",
     "OpenLook: time to wait until start of repeat"),
  RC(NAME_repeatInterval, "real", "0.06",
     "OpenLook: interval between repeats"),
  RC(NAME_width, "[int]", UXWIN("4mm", "@default"),
     "Width of the scroll_bar"),
  RC(NAME_autoHide, "bool", "@on",
     "Automatically hide bar if all is shown"),
  RC(NAME_cursor, "cursor", UXWIN("top_left_arrow", "win_arrow"), NULL)
};

/* Class Declaration */

static Name scrollBar_termnames[] =
	{ NAME_object, NAME_orientation, NAME_message };

ClassDecl(scrollBar_decls,
          var_scrollBar, send_scrollBar, get_scrollBar, rc_scrollBar,
          3, scrollBar_termnames,
          "$Rev$");


status
makeClassScrollBar(Class class)
{ declareClass(class, &scrollBar_decls);
  setRedrawFunctionClass(class, RedrawAreaScrollBar);

  succeed;
}
