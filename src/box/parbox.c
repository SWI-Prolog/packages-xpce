/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2013, University of Amsterdam
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

#include "boxes.h"

static status appendParBox(ParBox pb, HBox hb);

		/********************************
		*            CREATE		*
		********************************/

static status
initialiseParBox(ParBox pb, Int width, Name alignment, int argc, HBox *argv)
{ initialiseDevice((Device) pb);

  obtainClassVariablesObject(pb);
  assign(pb, content, newObject(ClassVector, EAV));

  if ( notDefault(alignment) ) assign(pb, alignment,  alignment);
  if ( notDefault(width) )     assign(pb, line_width, width);

  assign(pb, sel_start_box,  NIL);
  assign(pb, sel_start_char, NIL);
  assign(pb, sel_end_box,    NIL);
  assign(pb, sel_end_char,   NIL);
  assign(pb, sel_mark_box,   NIL);
  assign(pb, sel_mark_char,  NIL);

  for(int i=0; i<argc; i++)
    appendParBox(pb, argv[i]);

  succeed;
}

		 /*******************************
		 *	      CONTENT		*
		 *******************************/

static status
appendParBox(ParBox pb, HBox hb)
{ appendVector(pb->content, 1, (Any *)&hb);

  if ( instanceOfObject(hb, ClassGrBox) )
  { GrBox grb = (GrBox) hb;

    deviceGraphical(grb->graphical, (Device)pb);
    DisplayedGraphical(grb->graphical, ON);
  }

  return requestComputeGraphical(pb, DEFAULT);
}


static status
clearParBox(ParBox pb)
{ clearVector(pb->content);

  return clearDevice((Device)pb, NAME_erase);
}


static HBox
makeDefaultSpace(TBox refbox, Style style)
{ HBox hb;
  FontObj f;

  if ( notNil(refbox) )
  { if ( (hb=get(refbox, NAME_space, EAV)) )
      return hb;
  }

  if ( notDefault(style) && notDefault(style->font) )
    f = style->font;
  else
    f = getClassVariableValueClass(ClassTBox, NAME_font);

  return getSpaceHBoxFont(f);
}


static status
cdataParBox(ParBox pb, StringObj cdata,
	    Style style,
	    HBox space,
	    Name ignore_blanks)
{ string *text = &cdata->data;
  int here = 0;
  int end = text->s_size;
  Any refbox = NIL;

  if ( ignore_blanks == NAME_leading || ignore_blanks == NAME_both )
  { while( here<end && iswspace(str_fetch(text, here)) )
      here++;
  }
  if ( ignore_blanks == NAME_trailing || ignore_blanks == NAME_both )
  { while( end > here && iswspace(str_fetch(text, end-1)) )
      end--;
  }

  while( here < end )
  { if ( iswspace(str_fetch(text, here)) )
    { while( here<end && iswspace(str_fetch(text, here)) )
	here++;

      if ( isDefault(space) )
	space = makeDefaultSpace(refbox, style);

      appendParBox(pb, space);		/* send? */
    } else
    { int start = here;
      string s;
      Name n;

      while( here<end && !iswspace(str_fetch(text, here)) )
	here++;

      str_cphdr(&s, text);
      s.s_size = here-start;
      if ( isstrA(text) )
	s.s_textA = &text->s_textA[start];
      else
	s.s_textW = &text->s_textW[start];

      n = StringToName(&s);

      appendParBox(pb, (refbox=newObject(ClassTBox, n, style, EAV)));
    }
  }

  succeed;
}

		 /*******************************
		 *	  LOW-LEVEL DATA	*
		 *******************************/

#define MAXPENDINGGR	10		/* aligned graphicals pending */
#define GR_SEP		5		/* distance from aligned graphical */

#define PC_GRAPHICAL	0x01		/* contains a graphics */
#define PC_ALIGNED_GR	0x02		/* left/right aligned graphical */
#define PC_PLACED	0x04		/* we already placed the graphical */
#define PC_GRMASK	(PC_GRAPHICAL|PC_ALIGNED_GR)

typedef struct
{ int	start_y;
  int	end_y;
  int   x;
} shape_cell;


typedef struct
{ ParBox     parbox;			/* Box we are working on */
  int	     line_width;		/* full width of the line */
  int	     ln;			/* # in left-queue */
  int	     rn;			/* # in right queue */
  shape_cell left[MAXPENDINGGR];
  shape_cell right[MAXPENDINGGR];
} parshape;

static int	fill_line(ParBox pb, int here,
			  parline *line, parshape *shape,
			  bool compute);
static void	justify_line(parline *line, Name alignment);
static void	init_shape(parshape *s, ParBox pb, int w);
static void	push_shape_graphicals(parline *l, parshape *s);
static void	PlaceAlignedGr(GrBox grb,
			       parline *line, parshape *shape, int below);


		/********************************
		*             REDRAW		*
		********************************/

static void
drawHBox(HBox hb, int x, int y, int w, parline const *line, Area a,
	 int sel_from, int sel_to, Style sel_style, bool in_sel)
{ if ( instanceOfObject(hb, ClassTBox) )
  { drawTBoxSel((TBox)hb, x, y, w, line, sel_from, sel_to, sel_style);
  } else
  { int ly = y - line->ascent;
    int lh = line->ascent + line->descent;

    if ( in_sel && notNil(sel_style) && notDefault(sel_style->background) )
      r_fill(x, ly, w, lh, sel_style->background);
    else
      r_clear(x, ly, w, lh);

    if ( instanceOfObject(hb, ClassGrBox) )
    { Graphical gr = ((GrBox)hb)->graphical;

      if ( gr->displayed == ON )
	RedrawArea(gr, a);
    }
  }
}


static status
RedrawAreaParBox(ParBox pb, Area a)
{ int w = valInt(pb->line_width);
  int y = 0;
  device_draw_context ctx;
  parline l;
  parshape shape;

  init_shape(&shape, pb, w);
  DEBUG(NAME_parbox,
	{ Area a2 = pb->area;

	  r_fill(valInt(a2->x), valInt(a2->y), valInt(a2->w), valInt(a2->h),
	       newObject(ClassColour, CtoName("light_blue"), EAV));
	});


  if ( EnterRedrawAreaDevice((Device)pb, a, &ctx) )
  { int here = valInt(getLowIndexVector(pb->content));
    int ay = valInt(a->y);		/* start of redraw area */
    int zy = ay + valInt(a->h);		/* end of it */
    bool has_sel = notNil(pb->sel_start_box);
    int sb = 0, sc = 0, eb = 0, ec = 0;
    Style sel_style = NIL;

    if ( has_sel )
    { sb = valInt(pb->sel_start_box);
      sc = valInt(pb->sel_start_char);
      eb = valInt(pb->sel_end_box);
      ec = valInt(pb->sel_end_char);
      sel_style = getClassVariableValueObject(pb, NAME_selectionStyle);
      if ( !sel_style ) sel_style = NIL;
    }

    while(here <= valInt(getHighIndexVector(pb->content)) && y < zy)
    { parcell *pc;
      int i;
      int line_start = here;

      l.x = 0;
      l.y = y;
      l.w = w;
      l.size = MAXHBOXES;
      here = fill_line(pb, line_start, &l, &shape, false);
      if ( l.shape_graphicals )
	push_shape_graphicals(&l, &shape);

      if ( y+l.ascent+l.descent < valInt(a->y) )
      { y += l.ascent+l.descent;	/* above display */
	continue;
      }
      justify_line(&l, pb->alignment);

      y += l.ascent;			/* the baseline */

      for(i=0, pc = l.hbox; i<l.size; i++, pc++)
      { int gi = line_start + i;
	int sel_from = 0, sel_to = 0;
	bool in_sel = false;

	if ( has_sel && gi >= sb && gi <= eb )
	{ in_sel = true;
	  if ( instanceOfObject(pc->box, ClassTBox) )
	  { int len = ((TBox)pc->box)->text->data.s_size;
	    sel_from = (gi == sb) ? sc : 0;
	    sel_to   = (gi == eb) ? ec : len;
	  }
	}
	drawHBox(pc->box, pc->x, y, pc->w, &l, a,
		 sel_from, sel_to, sel_style, in_sel);
      }

      if ( l.size )
      { pc = &l.hbox[l.size-1];
	r_clear(pc->x+pc->w, l.y,
		l.x+l.w - pc->x+pc->w,
		l.ascent + l.descent);
      } else
      { r_clear(l.x, l.y, l.w, l.ascent + l.descent);
      }

      y += l.descent;
    }

    ExitRedrawAreaDevice((Device)pb, a, &ctx);
  }

  return RedrawAreaGraphical(pb, a);
}

static Num
getAscentParBox(ParBox pb)
{ int w = valInt(pb->line_width);
  parshape shape;
  parline l;

  init_shape(&shape, pb, w);
  l.x = 0;
  l.w = w;
  l.size = MAXHBOXES;
  fill_line(pb, valInt(getLowIndexVector(pb->content)),
	    &l, &shape, false);

  answer(toNum(l.ascent));
}

		 /*******************************
		 *	LOCATIONS AND EVENTS	*
		 *******************************/

/* Locate the (hbox-index, char-in-tbox) under an event.
   The char index is the offset within the tbox text under the click, or 0
   when the cell is not a tbox.  Returns true if the event lies on a cell.
*/
static bool
locate_event_parbox(ParBox pb, EventObj ev, int *box_out, int *char_out)
{ Int X, Y;

  *box_out  = 0;
  *char_out = 0;

  if ( get_xy_event(ev, pb, OFF, &X, &Y) )
  { int ex = valInt(X);
    int ey = valInt(Y);
    int w = valInt(pb->line_width);
    int y = 0;
    parline l;
    parshape shape;
    HBox *content = (HBox*)pb->content->elements-1;
    int here = valInt(getLowIndexVector(pb->content));
    int hi   = valInt(getHighIndexVector(pb->content));
    int h2;

    init_shape(&shape, pb, w);

    for(; here <= hi; here = h2)
    { parcell *pc;
      int i;

      l.x = 0;
      l.y = y;
      l.w = w;
      l.size = MAXHBOXES;
      h2 = fill_line(pb, here, &l, &shape, false);
      if ( l.shape_graphicals )
      { int g = 0;

	for(i=0, pc = l.hbox; i<l.size; i++, pc++)
	{ if ( pc->flags & PC_ALIGNED_GR )
	  { Graphical gr = ((GrBox)pc->box)->graphical;

	    if ( ex > valInt(gr->area->x) &&
		 ex < valInt(gr->area->x) + valInt(gr->area->w) &&
		 ey > valInt(gr->area->y) &&
		 ey < valInt(gr->area->y) + valInt(gr->area->h) )
	      goto found;
	    if ( ++g == l.shape_graphicals )
	      break;			/* no more */
	  }
	}

	push_shape_graphicals(&l, &shape);
      }

      if ( y+l.ascent+l.descent < ey )
      { y += l.ascent+l.descent;	/* before event */
	continue;
      }
      justify_line(&l, pb->alignment);

      for(i=0, pc = l.hbox; i<l.size; i++, pc++)
      { if ( pc->flags & PC_ALIGNED_GR )
	  continue;			/* check? */

	if ( ex > pc->x && ex <= pc->x + pc->w )
	{ int chr = 0;
	  found:
	  here += i;
	  assert(content[here] == pc->box);
	  if ( instanceOfObject(pc->box, ClassTBox) )
	  { TBox tb = (TBox)pc->box;
	    FontObj f = getFontTBox(tb);
	    int len = tb->text->data.s_size;

	    chr = str_x_to_index(&tb->text->data, 0, len, f,
				 ex - pc->x, 1);
	  }
	  *box_out  = here;
	  *char_out = chr;
	  (void)content;
	  return true;
	}
      }
      (void)content;

      return false;
    }
  }

  return false;
}


static Int
getLocateEventParBox(ParBox pb, EventObj ev)
{ int box, chr;

  if ( locate_event_parbox(pb, ev, &box, &chr) )
    answer(toInt(box));

  fail;
}


static Tuple
getLocateCharParBox(ParBox pb, EventObj ev)
{ int box, chr;

  if ( locate_event_parbox(pb, ev, &box, &chr) )
    answer(answerObject(ClassTuple, toInt(box), toInt(chr), EAV));

  fail;
}


static HBox
getBoxParBox(ParBox pb, Int index)
{ HBox hb = getElementVector(pb->content, index);

  if (notNil(hb))
    answer(hb);

  fail;
}


static Area
getBoxAreaParBox(ParBox pb, Any target, Device relto)
{ int w = valInt(pb->line_width);
  int y = 0;
  parline l;
  parshape shape;
  long here = valInt(getLowIndexVector(pb->content));
  long hi   = valInt(getHighIndexVector(pb->content));
  HBox box;
  long index;

  if ( instanceOfObject(target, ClassHBox) )
  { box = target;
    index = -1;				/* keep compiler happy */
  } else
  { box = NULL;
    index = valInt(target);
  }

  init_shape(&shape, pb, w);

  while(here <= hi)
  { long h2;
    parcell *pc;
    int i;

    l.x = 0;
    l.y = y;
    l.w = w;
    l.size = MAXHBOXES;
    h2 = fill_line(pb, here, &l, &shape, false);
    if ( l.shape_graphicals )
      push_shape_graphicals(&l, &shape);

    if ( box )
    { for(i=0, pc = l.hbox; i<l.size; i++, pc++)
      { if ( pc->box == box )
	{ Area a;
	  out:

	  if ( pc->flags & PC_ALIGNED_GR )
	  { Graphical gr = ((GrBox)pc->box)->graphical;

	    a = getCopyArea(gr->area);
	  } else
	  { justify_line(&l, pb->alignment);

	    a = answerObject(ClassArea,
			     toInt(pc->x), toInt(y),
			     toInt(pc->w), toInt(l.ascent+l.descent), EAV);
	  }

	  if ( notDefault(relto) )
	  { int dx = 0;
	    int dy = 0;
	    Device dev = (Device) pb;

	    for( ; notNil(dev) && dev != relto && !instanceOfObject(dev, ClassWindow)
		 ; dev = dev->device )
	    {  Point p = dev->offset;
	       dx += valInt(p->x);
	       dy += valInt(p->y);
	    }
	    if ( dev == relto )
	    { assign(a, x, toInt(valInt(a->x) + dx));
	      assign(a, y, toInt(valInt(a->y) + dy));
	    } else
	      fail;			/* ??? */
	  }

	  answer(a);
	}
      }
    } else				/* index provided */
    { if ( index >= here && index < h2 )
      { pc = &l.hbox[index-here];
	goto out;
      } else if ( index < here )
	fail;				/* line-break */
    }

    y += l.ascent + l.descent;
    here = h2;
  }

  fail;
}

		 /*******************************
		 *	      SEARCH		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
for_parbox(ParBox, Function, Closure)
    Excecutes a recursive search through a parbox, stopping as soon as
    `Function yields non-zero'.  The return value of the function is
    returned, or zero if the end of the search is reached.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static long for_device_parbox(Device dev,
			      long (*f)(ParBox pb, HBox hb, long index, void *closure),
			      void *closure);


static long
for_parbox(ParBox pb,
	   long (*f)(ParBox pb, HBox hb, long index, void *closure),
	   void *closure)
{ int here = valInt(getLowIndexVector(pb->content));
  int hi   = valInt(getHighIndexVector(pb->content));

  for(; here <= hi; here++)
  { HBox hb = getElementVector(pb->content, toInt(here));
    long rval;

    if ( (rval = (*f)(pb, hb, here, closure)) )
      return rval;

    if ( instanceOfObject(hb, ClassGrBox) )
    { GrBox grb = (GrBox)hb;

      if ( instanceOfObject(grb->graphical, ClassDevice) )
      { if ( (rval = for_device_parbox((Device)grb->graphical, f, closure)) )
	  return rval;
      }
    }
  }

  return 0;
}


static long
for_device_parbox(Device dev,
		  long (*f)(ParBox pb, HBox hb, long index, void *closure),
		  void *closure)
{ Cell cell;

  if ( instanceOfObject(dev, ClassParBox) )
    return for_parbox((ParBox)dev, f, closure);

  for_cell(cell, dev->graphicals)
  { if ( instanceOfObject(cell->value, ClassDevice) )
    { long rval;

      if ( (rval = for_device_parbox(cell->value, f, closure)) )
	return rval;
    }
  }

  return 0;
}


typedef struct
{ Code   test;				/* Test code to use */
  ParBox parbox;			/* Found on this parbox */
  long   index;				/* at this index */
} testcl;


static long
test_get_find_parbox(ParBox pb, HBox hb, long index, void *closure)
{ testcl *cl = closure;

  if ( forwardReceiverCode(cl->test, pb, hb, toInt(index), EAV) )
  { cl->parbox = pb;
    cl->index  = index;
    return TRUE;
  }

  return FALSE;
}


Tuple					/* parbox, index */
getFindParBox(ParBox pb, Code test)
{ testcl cl;

  cl.test = test;

  if ( for_parbox(pb, test_get_find_parbox, &cl) )
  { answer(answerObject(ClassTuple, cl.parbox, toInt(cl.index), EAV));
  }

  fail;
}


static Int
getMinimumWidthParBox(ParBox pb)
{ HBox *content = (HBox *)pb->content->elements-1;
  int hi   = valInt(getHighIndexVector(pb->content));
  int here = valInt(getLowIndexVector(pb->content));
  int w = 0;

  for( ; here <= hi; here++ )
  { int wb = valInt(content[here]->width);

    if ( wb > w )
      w = wb;
  }

  answer(toInt(w));
}


#if 0

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Determine the width if there is  no   limit  on  the line-width. This is
mainly used to calculate rubber for parbox objects used in tables.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Int
getNaturalWidthParBox(ParBox pb, Int maxwidth)
{ int w = isDefault(maxwidth) ? 2000 : valInt(maxwidth);
  int y = 0;
  int mw = 0;
  int lm = 0;				/* left margin */
  int here = valInt(getLowIndexVector(pb->content));
  int hi   = valInt(getHighIndexVector(pb->content));
  parshape shape;

  init_shape(&shape, pb, w);

  while(here <= hi)
  { parline l;

    l.x = 0;
    l.y = y;
    l.w = w;
    l.size = MAXHBOXES;

    here = fill_line(pb, here, &l, &shape, true);

    y += l.ascent + l.descent;
    mw = max(mw, l.maxx);
    lm = min(lm, l.minx);

    if ( l.shape_graphicals )
    { parcell *pc = l.hbox, *epc = pc+l.size;

      for( ; pc < epc; pc++ )
      { if ( (pc->flags & PC_ALIGNED_GR) && !(pc->flags & PC_PLACED) )
	{ GrBox grb = (GrBox)pc->box;

	  PlaceAlignedGr(grb, &l, &shape, TRUE);
	}
      }
    }
  }

  requestComputeGraphical(pb, DEFAULT);

  answer(toInt(mw-lm));
}

#endif


		 /*******************************
		 *	 PARAGRAPH-SHAPE	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void current_margins(parshape *s, int y, int h, int *lm, int *lw)
    If we want to place a line at y of height h, compute the left-side and
    width of the line we can place there.  Assume `s' is cleaned upto y.

void clean_margins(parshape *s, int y)
    Delete any `old' margin declarations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
init_shape(parshape *s, ParBox pb, int w)
{ s->line_width = w;
  s->parbox     = pb;
  s->ln = s->rn = 0;
}


static int
y_extend_shape(parshape *s)		/* deepest shape-graphical */
{ int y = 0;
  int i;

  for(i=0; i<s->ln; i++)
  { if ( s->left[i].end_y > y )
      y = s->left[i].end_y;
  }
  for(i=0; i<s->rn; i++)
  { if ( s->right[i].end_y > y )
      y = s->right[i].end_y;
  }

  return y;
}



static void
current_margins(parshape *s, int y, int *lm, int *lw)
{ int l	= 0;
  int r = s->line_width;
  int i;

  for(i=0; i<s->ln; i++)
  { if ( !(s->left[i].start_y > y) )
      l = max(l, s->left[i].x);
  }
  for(i=0; i<s->rn; i++)
  { if ( !(s->right[i].start_y > y) )
    { r = min(r, s->right[i].x);
    }
  }

  *lm = l;
  *lw = r-l;
}


static void
clean_margins(parshape *s, int y)
{ while( s->ln > 0 && s->left[0].end_y < y )
  { s->ln--;
    memmove(&s->left[0], &s->left[1], s->ln*sizeof(shape_cell));
  }
  while( s->rn > 0 && s->right[0].end_y < y )
  { s->rn--;
    memmove(&s->right[0], &s->right[1], s->rn*sizeof(shape_cell));
  }
}


static void
add_left_margin(parshape *s, int y, int h, int x)
{ int i;

  DEBUG(NAME_parbox, Cprintf("add_left_margin(%d %d %d)\n", y, h, x));
  for(i=0; i<s->ln && s->left[i].end_y < y+h; i++)
    ;
  if ( s->ln > i )
    memmove(&s->left[s->ln+1], &s->left[s->ln], (s->ln-i)*sizeof(shape_cell));
  s->left[i].start_y = y;
  s->left[i].end_y   = y+h;
  s->left[i].x       = x + GR_SEP;
  s->ln++;
}


static void
add_right_margin(parshape *s, int y, int h, int x)
{ int i;

  for(i=0; i<s->rn && s->right[i].end_y < y+h; i++)
    ;
  if ( s->rn > i )
    memmove(&s->right[s->rn+1], &s->right[s->rn],
	    (s->rn-i)*sizeof(shape_cell));
  s->right[i].start_y = y;
  s->right[i].end_y   = y+h;
  s->right[i].x       = x - GR_SEP;
  s->rn++;
}


		 /*******************************
		 *	     COMPUTE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Fill a line, start using hboxes from the given cell. Returns cell of the
next hbox or NIL  if  the  contents   is  complete.  Flags  is  used for
refinement of the behaviour.

At entry, x and w must be  filled.   size  should be set to to allocated
size. If size is too small, the other fields are filled nevertheless, so
this can be used for dimension testing.

y is left untouched, the other fields are filled by this routine.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
justify_line(parline *line, Name alignment)
{ int i;
  parcell *c;

  if ( line->end_of_par && alignment == NAME_justify )
    alignment = NAME_left;
  if ( line->rlevel >= 3 )		/* line contains hfill */
    alignment = NAME_justify;

  if ( alignment == NAME_right )
  { int shift = line->w - line->maxx;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
      c->x += shift;
  } else if ( alignment == NAME_center )
  { int shift = (line->w - line->maxx)/2;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
      c->x += shift;
  } else if ( alignment == NAME_justify )
  { stretch *stretches = alloca(sizeof(stretch) * line->size);
    stretch *sp = stretches;
    int dw = line->x + line->w - line->maxx; /* width to distribute */
    int cx = line->x;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
    { HBox hb = c->box;

      if ( notNil(hb->rubber) && valInt(hb->rubber->level) == line->rlevel )
      { sp->ideal   = c->w; /*valInt(hb->width);*/
	sp->stretch = valInt(hb->rubber->stretch);
	sp->shrink  = valInt(hb->rubber->shrink);
	sp->minimum = 0;
	sp->maximum = INT_MAX;

	dw += sp->ideal;
	sp++;
      }
    }

    distribute_stretches(stretches, sp-stretches, dw);
    sp = stretches;

    for( i=0, c = line->hbox; i++ < line->size; c++ )
    { HBox hb = c->box;

      if ( notNil(hb->rubber) && valInt(hb->rubber->level) == line->rlevel )
      { c->w = sp->size;
	sp++;
      }

      c->x = cx;
      if ( !(c->flags & PC_ALIGNED_GR) )
	cx += c->w;
      if ( cx > line->maxx )
	line->maxx = cx;
    }
  }
}


static void
compute_line(parline *line)
{ parcell *pc	 = line->hbox;
  parcell *epc	 = &pc[line->size];
  int cx	 = line->x;
  int ascent	 = 0;
  int descent	 = 0;
  int rlevel	 = 0;
  int minx	 = cx;
  int maxx	 = cx;

  line->graphicals = 0;
  line->shape_graphicals = 0;

  for( pc = line->hbox; pc < epc; pc++ )
  { HBox hb = pc->box;

    pc->x = cx;

    if ( !(pc->flags & PC_ALIGNED_GR) )
    { ascent  = max(ascent,  valInt(hb->ascent));
      descent = max(descent, valInt(hb->descent));
      cx += pc->w;
      minx = min(minx, cx);
      maxx = max(maxx, cx);
      if ( notNil(hb->rubber) )
	rlevel = max(rlevel, valInt(hb->rubber->level));
    }

    if ( pc->flags & PC_GRAPHICAL )
    { if ( pc->flags & PC_ALIGNED_GR )
	line->shape_graphicals++;
      else
	line->graphicals++;
    }
  }

  line->ascent	= ascent;
  line->descent	= descent;
  line->minx	= minx;
  line->maxx	= maxx;
  line->rlevel	= rlevel;
}


static int
fill_line(ParBox pb, int here, parline *line, parshape *shape,
	  bool compute)
{ int cx, ex;
  HBox *content = (HBox *)pb->content->elements-1;
  int hi = valInt(getHighIndexVector(pb->content));
  int last_break_index = here;
  parcell *last_break = NULL;
  parcell *pc = line->hbox, *epc = pc+line->size;
  int blank = TRUE;			/* only emitted blank space */

  clean_margins(shape, line->y);
  current_margins(shape, line->y, &line->x, &line->w);
  cx = line->x;
  ex = cx + line->w;

  for( ; here <= hi && pc < epc; here++, pc++ )
  { HBox hb = content[here];
    int  bw;

    if ( isNil(hb) )			/* should we allow for nil in the */
      continue;				/* vector? */

    bw = valInt(hb->width);
    if ( cx+bw > ex && last_break )
    { pc   = last_break;
      here = last_break_index;
      line->end_of_par = FALSE;
      break;
    }

    if ( notNil(hb->rubber) && notNil(hb->rubber->linebreak) )
    { if ( cx+bw > ex )
      { line->end_of_par = FALSE;
	break;
      }
      if ( hb->rubber->linebreak == NAME_force )
      { line->end_of_par = TRUE;
	break;
      }

      last_break       = pc;
      last_break_index = here;
    }

    pc->box   = hb;
    pc->w     = bw;
    pc->flags = 0;

    if ( instanceOfObject(hb, ClassGrBox) )
    { GrBox grb = (GrBox)hb;

      if ( compute )
      { Graphical gr = grb->graphical;

	if ( pb->request_compute == NAME_lineWidth )
	{ Any av[2];

	  av[0] = pb->line_width;
	  av[1] = DEFAULT;

	  qadSendv(gr, NAME_containerSizeChanged, 2, av);
	}

	if ( notNil(gr->request_compute) )
	{ ComputeGraphical(gr);
	  computeGrBox(grb);
	}

	pc->w = bw = valInt(hb->width);
      }

      pc->flags |= PC_GRAPHICAL;
      if ( grb->alignment == NAME_left || grb->alignment == NAME_right )
      { pc->flags |= PC_ALIGNED_GR;

	if ( blank )
	{ int lx;

	  pc->flags |= PC_PLACED;
	  PlaceAlignedGr(grb, line, shape, FALSE);
	  current_margins(shape, line->y, &lx, &line->w);
	  cx += lx - line->x;
	  ex = cx + line->w;
	  DEBUG(NAME_parbox,
		Cprintf("Placed %s; line %d to %d\n",
			pp(grb->graphical), cx, ex));
	  line->x = lx;
	}
      }
    }

    if ( !(pc->flags & PC_ALIGNED_GR) )
    { if ( !(hb->width == ZERO ||
	     (hb->ascent == ZERO && hb->descent == ZERO)) )
	blank = FALSE;
      cx += pc->w;
    }
  }

  if ( here <= hi )
    here++;
  else
    line->end_of_par = TRUE;

  line->size = pc-line->hbox;
  compute_line(line);

  return here;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
push_shape_graphicals()
    Used by RedrawAreaParBox() to add already placed graphicals to the
    margin-shape.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
push_shape_graphicals(parline *l, parshape *s)
{ parcell *pc = l->hbox, *epc = pc+l->size;

  for( ; pc < epc; pc++ )
  { if ( (pc->flags & PC_ALIGNED_GR) && !(pc->flags & PC_PLACED) )
    { GrBox grb = (GrBox)pc->box;
      int h = valInt(grb->ascent)+valInt(grb->descent);
      int y = l->y + l->ascent + l->descent;
      int grw = valInt(grb->width);

      if ( grb->alignment == NAME_left )
      { add_left_margin(s, y, h, grw);
      } else
      { int grx = l->w-pc->w;

	add_right_margin(s, y, h, grx);
      }
      if ( --l->shape_graphicals <= 0 )
	break;
    }
  }
}


static void				/* debugging help */
print_line(parline *l)
{ parcell *pc = l->hbox, *epc = pc+l->size;

  for(; pc < epc; pc++)
  { if ( instanceOfObject(pc->box, ClassTBox) )
    { TBox tb = (TBox) pc->box;

      Cprintf("[%s] ", strName(tb->text));
    } else if ( instanceOfObject(pc->box, ClassGrBox) )
    { GrBox grb = (GrBox) pc->box;
      Cprintf("%s ", pp(grb->graphical));
    } else
    { Cprintf("|%d+%d-%d|",
	      valInt(pc->box->width),
	      valInt(pc->box->ascent),
	      valInt(pc->box->descent));
    }
  }

  Cprintf("\n");
}


/*
static void
compute_ascent_descent_line(parline *l)
{ int ascent  = 0;
  int descent = 0;
  int i;
  parcell *pc;

  for(i=0, pc = l->hbox; i < l->size; i++, pc++)
  { HBox hb = pc->box;

    ascent  = max(ascent,  valInt(hb->ascent));
    descent = max(descent, valInt(hb->descent));
  }

  l->ascent  = ascent;
  l->descent = descent;
}
*/

static status
PlaceGrBox(ParBox pb, GrBox grb, parline *l, Int x, Int y, Int w)
{ Graphical gr = grb->graphical;

  DEBUG(NAME_parbox,
	Cprintf("Placing %s (grbox %s) on %s at %d,%d (width = %d)\n",
		pp(gr), pp(grb), pp(pb), valInt(x), valInt(y), valInt(w)));

  if ( gr->area->x != x || gr->area->y != y || gr->area->w != w )
  { int h, ascent, descent;

    setGraphical(gr, x, y, w, DEFAULT);
    ComputeGraphical(gr);

    if ( l )
    { h = valInt(gr->area->h);

      if ( grb->alignment == NAME_top )
      { ascent  = l->ascent;
	descent = h-ascent;
      } else if ( grb->alignment == NAME_bottom )
      { descent = l->descent;
	ascent = h-descent;
      } else /*if ( grb->alignment == NAME_center )*/
      { ascent = (l->ascent - l->descent)/2 + h/2;
	descent = h-ascent;
      }

      if ( grb->ascent  != toInt(ascent) ||
	   grb->descent != toInt(descent) )
      { assign(grb, ascent,  toInt(ascent));
	assign(grb, descent, toInt(descent));
	DEBUG(NAME_parbox, Cprintf("    --> Size changed\n"));
	fail;				/* modified */
      }
    }
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Place left/right aligned graphical.  If `below' is TRUE, place it below the
current line.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
PlaceAlignedGr(GrBox grb, parline *line, parshape *shape, int below)
{ Int grw = grb->width;
  int y = line->y;

  if ( below )
    y += line->ascent + line->descent;

  DEBUG(NAME_parbox, Cprintf("PLacing %s (y=%d)\n", pp(grb), y));

  if ( grb->alignment == NAME_left )
  { PlaceGrBox(shape->parbox, grb, NULL, toInt(line->x), toInt(y), grw);
    add_left_margin(shape,
		    y,
		    valInt(grb->ascent)+valInt(grb->descent),
		    valInt(grw));
  } else
  { int grx = line->x + line->w - valInt(grw);

    PlaceGrBox(shape->parbox, grb, NULL, toInt(grx), toInt(y), grw);
    add_right_margin(shape,
		     y,
		     valInt(grb->ascent)+valInt(grb->descent),
		     grx);
  }
}


static status
computeParBox(ParBox pb)
{ if ( notNil(pb->request_compute) )
  { int w = valInt(pb->line_width);
    int y = 0;
    int mw = (pb->auto_crop == ON ? 0 : w);
    int lm = 0;				/* left margin */
    int ax, aw;				/* area x/w */
    int here = valInt(getLowIndexVector(pb->content));
    int hi   = valInt(getHighIndexVector(pb->content));
    parshape shape;
    int lineno = 0;

    init_shape(&shape, pb, w);

    while(here <= hi)
    { parline l;

      l.x = 0;
      l.y = y;
      l.w = w;
      l.size = MAXHBOXES;
      here = fill_line(pb, here, &l, &shape, TRUE);
      lineno++;

      DEBUG(NAME_parbox,
	    if ( l.maxx > l.x + l.w )
	    { Cprintf("%s: Overfull line %d\n", pp(pb), lineno);
	      print_line(&l);
	    });

      if ( l.graphicals )
      { parcell *pc;
	int i;
	int maxloop = 3;

	while(--maxloop >= 0)
	{ int modified = FALSE;
	  int gr = 0;

	  justify_line(&l, pb->alignment);

	  for(i=0, pc = l.hbox; i<l.size; i++, pc++)
	  { if ( (pc->flags & PC_GRMASK) == PC_GRAPHICAL )
	    { GrBox grb = (GrBox)pc->box;

	      if ( !PlaceGrBox(pb, grb,
			       &l,
			       toInt(pc->x),
			       toInt(y + l.ascent - valInt(grb->ascent)),
			       toInt(pc->w)) )
		modified = TRUE;
	      if ( ++gr == l.graphicals )
		break;			/* we had them all */
	    }
	  }

          if ( modified )
	    compute_line(&l);
	  else
	    break;
	}
      }

      y += l.ascent + l.descent;	/* + skip? */
      mw = max(mw, l.maxx);		/* things that don't fit */
      lm = min(lm, l.minx);

      if ( l.shape_graphicals )
      { parcell *pc = l.hbox, *epc = pc+l.size;

	for( ; pc < epc; pc++ )
	{ if ( (pc->flags & PC_ALIGNED_GR) && !(pc->flags & PC_PLACED) )
	  { GrBox grb = (GrBox)pc->box;

	    PlaceAlignedGr(grb, &l, &shape, TRUE);
	  }
	}
      }
    }

    ax = valInt(pb->offset->x) + lm;
    aw = mw-lm;				/* valInt(pb->offset->x) + mw - ax */

    y = max(y_extend_shape(&shape), y);

    if ( toInt(y)  != pb->area->h ||
	 toInt(aw) != pb->area->w ||
	 toInt(ax) != pb->area->x )
    { DEBUG(NAME_parbox,
	    Cprintf("computeParBox(%s) --> x,w,h = %d,%d,%d\n",
		    pp(pb), ax, aw, y));
      CHANGING_GRAPHICAL(pb,
      { assign(pb->area, h, toInt(y));
	assign(pb->area, w, toInt(aw));
	assign(pb->area, x, toInt(ax));
	changedEntireImageGraphical(pb);
      });
    } else
    { DEBUG(NAME_parbox,
	    Cprintf("computeParBox(%s) --> no change\n", pp(pb)));
    }

    assign(pb, request_compute, NIL);
  }

  succeed;
}


		 /*******************************
		 *	     GEOMETRY		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Interpret ->width from the offset
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
requestGeometryParBox(ParBox pb, Int x, Int y, Int w, Int h)
{ Int av[4];
  int lm = valInt(pb->area->x) - valInt(pb->offset->x);

  if ( isDefault(x) )
    av[0] = x;
  else
    av[0] = toInt(valInt(x) + lm);
  if ( isDefault(w) )
    av[2] = w;
  else
    av[2] = toInt(valInt(w) - lm);
  av[1] = y;
  av[3] = h;


  return qadSendv(pb, NAME_geometry, 4, av);
}


static status
geometryParBox(ParBox pb, Int x, Int y, Int w, Int h)
{ Area a = pb->area;
  Point o = pb->offset;
  int chw;

/*
  DEBUG(NAME_lbox,
	Cprintf("geometryParBox() pb->offset = %d,%d, xyw=%d,%d,%d, lw=%d\n",
		valInt(o->x), valInt(o->y),
		valInt(a->x), valInt(a->y), valInt(a->w),
		valInt(pb->line_width)));
*/

  if ( isDefault(x) ) x = a->x;
  if ( isDefault(y) ) y = a->y;
  if ( isDefault(w) )
  { w = a->w;
    chw = FALSE;
  } else
  { if ( pb->auto_crop == ON )
    { assign(pb, auto_crop, OFF);
      chw = TRUE;
    } else
      chw = (w != a->w);
  }

  if ( x != a->x || y != a->y || chw )
  { Int dx = sub(x, a->x);
    Int dy = sub(y, a->y);

    CHANGING_GRAPHICAL(pb,
		       { int lw;

			 assign(o, x, add(o->x, dx));
			 assign(o, y, add(o->y, dy));

			 lw = valInt(x)+valInt(w)-valInt(o->x);

			 if ( lw < 0 )
			 { w = toInt(valInt(w)-lw);
			   lw = 0;
			 }

			 assign(a, w, w);
			 assign(a, x, x);
			 assign(a, y, y);
			 if ( chw )
			 { if ( pb->line_width != toInt(lw) )
			   { send(pb, NAME_lineWidth, toInt(lw), EAV);
			     computeParBox(pb); /* update ->height */
			   }
			 }
		       });

    updateConnectionsDevice((Device) pb, sub(pb->level, ONE));
  }

  succeed;
}


static status
alignmentParBox(ParBox pb, Name alignment)
{ return assignGraphical(pb, NAME_alignment, alignment);
}


		 /*******************************
		 *	    SELECTION		*
		 *******************************/

static int
sel_cmp(int b1, int c1, int b2, int c2)
{ if ( b1 != b2 ) return b1 < b2 ? -1 : 1;
  if ( c1 != c2 ) return c1 < c2 ? -1 : 1;
  return 0;
}


static status
selectionParBox(ParBox pb, Int sbox, Int schar, Int ebox, Int echar)
{ if ( isDefault(sbox) || isNil(sbox) )
  { if ( isNil(pb->sel_start_box) )
      succeed;

    assign(pb, sel_start_box,  NIL);
    assign(pb, sel_start_char, NIL);
    assign(pb, sel_end_box,    NIL);
    assign(pb, sel_end_char,   NIL);
    assign(pb, sel_mark_box,   NIL);
    assign(pb, sel_mark_char,  NIL);
    changedEntireImageGraphical(pb);
    succeed;
  }

  int sb = valInt(sbox);
  int sc = (isDefault(schar) || isNil(schar)) ? 0 : valInt(schar);
  int eb = (isDefault(ebox)  || isNil(ebox))  ? sb : valInt(ebox);
  int ec = (isDefault(echar) || isNil(echar)) ? sc : valInt(echar);

  if ( sel_cmp(sb, sc, eb, ec) > 0 )
  { int tb = sb, tc = sc; sb = eb; sc = ec; eb = tb; ec = tc;
  }

  Int sb_i = toInt(sb), sc_i = toInt(sc);
  Int eb_i = toInt(eb), ec_i = toInt(ec);

  if ( pb->sel_start_box  != sb_i ||
       pb->sel_start_char != sc_i ||
       pb->sel_end_box    != eb_i ||
       pb->sel_end_char   != ec_i )
  { assign(pb, sel_start_box,  sb_i);
    assign(pb, sel_start_char, sc_i);
    assign(pb, sel_end_box,    eb_i);
    assign(pb, sel_end_char,   ec_i);
    changedEntireImageGraphical(pb);
  }

  succeed;
}


static Tuple
getSelectionParBox(ParBox pb)
{ if ( isNil(pb->sel_start_box) )
    fail;

  Point p1 = answerObject(ClassPoint,
			  pb->sel_start_box, pb->sel_start_char, EAV);
  Point p2 = answerObject(ClassPoint,
			  pb->sel_end_box,   pb->sel_end_char,   EAV);
  answer(answerObject(ClassTuple, p1, p2, EAV));
}


static status
hasSelectionParBox(ParBox pb)
{ if ( isNil(pb->sel_start_box) )
    fail;
  if ( pb->sel_start_box  == pb->sel_end_box &&
       pb->sel_start_char == pb->sel_end_char )
    fail;
  succeed;
}


static StringObj
getSelectedTextParBox(ParBox pb)
{ if ( isNil(pb->sel_start_box) )
    fail;

  int sb = valInt(pb->sel_start_box);
  int sc = valInt(pb->sel_start_char);
  int eb = valInt(pb->sel_end_box);
  int ec = valInt(pb->sel_end_char);
  int lo = valInt(getLowIndexVector(pb->content));
  int hi = valInt(getHighIndexVector(pb->content));

  if ( sb < lo ) sb = lo;
  if ( eb > hi ) eb = hi;

  tmp_string tmp;
  str_tmp_init(&tmp);

  for(int i=sb; i<=eb; i++)
  { HBox hb = getElementVector(pb->content, toInt(i));

    if ( !hb || isNil(hb) )
      continue;

    if ( instanceOfObject(hb, ClassTBox) )
    { TBox tb = (TBox)hb;
      PceString str = &tb->text->data;
      int from = (i == sb) ? sc : 0;
      int to   = (i == eb) ? ec : str->s_size;

      if ( from < 0 ) from = 0;
      if ( to > str->s_size ) to = str->s_size;

      for(int j=from; j<to; j++)
	str_tmp_put(&tmp, str_fetch(str, j));
    } else if ( instanceOfObject(hb, ClassGrBox) )
    { /* graphic: skip */
    } else if ( instanceOfObject(hb, ClassHBox) )
    { if ( notNil(hb->rubber) &&
	   hb->rubber->linebreak == NAME_force )
	str_tmp_put(&tmp, '\n');
      else
	str_tmp_put(&tmp, ' ');
    }
  }

  StringObj rval = StringToString(&tmp.s);
  str_tmp_done(&tmp);

  if ( !rval )
    fail;
  answer(rval);
}


static status
copyParBox(ParBox pb)
{ StringObj s = getSelectedTextParBox(pb);
  DisplayObj d = getDisplayGraphical((Graphical)pb);

  if ( !d )
  { EventObj ev;

    if ( instanceOfObject((ev=getValueVar(EVENT)), ClassEvent) )
      d = getDisplayEvent(ev);
  }

  if ( s && d )
    return send(d, NAME_copy, s, EAV);

  fail;
}


static status
markSelectionParBox(ParBox pb, EventObj ev)
{ int box, chr;

  if ( !locate_event_parbox(pb, ev, &box, &chr) )
    fail;

  assign(pb, sel_mark_box,  toInt(box));
  assign(pb, sel_mark_char, toInt(chr));
  return selectionParBox(pb, toInt(box), toInt(chr), toInt(box), toInt(chr));
}


static status
selectWordParBox(ParBox pb, EventObj ev)
{ int box, chr;

  if ( !locate_event_parbox(pb, ev, &box, &chr) )
    fail;

  HBox hb = getElementVector(pb->content, toInt(box));
  if ( !hb || isNil(hb) || !instanceOfObject(hb, ClassTBox) )
    fail;

  TBox tb = (TBox)hb;
  int len = tb->text->data.s_size;

  assign(pb, sel_mark_box,  toInt(box));
  assign(pb, sel_mark_char, toInt(0));
  return selectionParBox(pb, toInt(box), toInt(0), toInt(box), toInt(len));
}


static int
find_tbox_at_or_before(ParBox pb, int box)
{ int lo = valInt(getLowIndexVector(pb->content));
  for(int i=box; i >= lo; i--)
  { HBox hb = getElementVector(pb->content, toInt(i));
    if ( hb && notNil(hb) && instanceOfObject(hb, ClassTBox) )
      return i;
  }
  return -1;
}


static int
find_tbox_at_or_after(ParBox pb, int box)
{ int hi = valInt(getHighIndexVector(pb->content));
  for(int i=box; i <= hi; i++)
  { HBox hb = getElementVector(pb->content, toInt(i));
    if ( hb && notNil(hb) && instanceOfObject(hb, ClassTBox) )
      return i;
  }
  return -1;
}


static status
extendSelectionWordParBox(ParBox pb, EventObj ev)
{ int loc_box, loc_chr;

  (void)loc_chr;

  if ( !locate_event_parbox(pb, ev, &loc_box, &loc_chr) )
    fail;

  if ( isNil(pb->sel_mark_box) )
    return selectWordParBox(pb, ev);

  int mb = valInt(pb->sel_mark_box);
  int snapped;

  if ( loc_box < mb )
    snapped = find_tbox_at_or_after(pb, loc_box);
  else
    snapped = find_tbox_at_or_before(pb, loc_box);

  if ( snapped < 0 )
    snapped = mb;

  HBox mhb = getElementVector(pb->content, toInt(mb));
  int mb_len = (mhb && instanceOfObject(mhb, ClassTBox))
               ? ((TBox)mhb)->text->data.s_size : 0;
  HBox shb = getElementVector(pb->content, toInt(snapped));
  int snapped_len = (shb && instanceOfObject(shb, ClassTBox))
                    ? ((TBox)shb)->text->data.s_size : 0;

  if ( snapped <= mb )
    return selectionParBox(pb, toInt(snapped), toInt(0),
			       toInt(mb),      toInt(mb_len));
  else
    return selectionParBox(pb, toInt(mb),      toInt(0),
			       toInt(snapped), toInt(snapped_len));
}


static status
extendSelectionParBox(ParBox pb, EventObj ev)
{ int box, chr;

  if ( !locate_event_parbox(pb, ev, &box, &chr) )
    fail;

  if ( isNil(pb->sel_mark_box) )
  { assign(pb, sel_mark_box,  toInt(box));
    assign(pb, sel_mark_char, toInt(chr));
  }

  int mb = valInt(pb->sel_mark_box);
  int mc = valInt(pb->sel_mark_char);

  if ( sel_cmp(mb, mc, box, chr) <= 0 )
    return selectionParBox(pb, toInt(mb), toInt(mc), toInt(box), toInt(chr));
  else
    return selectionParBox(pb, toInt(box), toInt(chr), toInt(mb), toInt(mc));
}


static status
lineWidthParBox(ParBox pb, Int w)
{ if ( valInt(w) < 0 )
    w = ZERO;

  if ( pb->line_width != w )
  { assign(pb, line_width, w);

    requestComputeGraphical(pb, NAME_lineWidth);
  }

  succeed;
}


static status
autoCropParBox(ParBox pb, BoolObj crop)
{ return assignGraphical(pb, NAME_autoCrop, crop);
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
	{ "width=[int]",
	  "alignment=[{left,center,right,justify}]",
	  "content=hbox ..."
	};
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_boxArea[] =
	{ "for=hbox|1..", "relative_to=[device]" };
static char *T_cdata[] =
	{ "cdata=string", "style=[style]", "space=[hbox]",
	  "ignore_blanks=[{none,leading,trailing,both}]" };
static char *T_selection[] =
        { "start_box=[int]*", "start_char=[int]*",
	  "end_box=[int]*",   "end_char=[int]*" };


/* Instance Variables */

static vardecl var_parbox[] =
{ SV(NAME_lineWidth, "int", IV_GET|IV_STORE,
     lineWidthParBox, NAME_area, "Maximum width of a textline"),
  IV(NAME_content, "vector", IV_GET,
     NAME_content, "Contained hbox objects"),
  SV(NAME_alignment, "{left,right,center,justify}", IV_GET|IV_STORE,
     alignmentParBox, NAME_layout, "Alignment of text in box"),
  SV(NAME_autoCrop, "bool", IV_GET|IV_STORE,
     autoCropParBox, NAME_layout, "If @on, make <-area fit content"),
  IV(NAME_selStartBox, "int*", IV_NONE,
     NAME_selection, "Selection start: hbox index"),
  IV(NAME_selStartChar, "int*", IV_NONE,
     NAME_selection, "Selection start: character offset in tbox"),
  IV(NAME_selEndBox, "int*", IV_NONE,
     NAME_selection, "Selection end: hbox index"),
  IV(NAME_selEndChar, "int*", IV_NONE,
     NAME_selection, "Selection end: character offset in tbox"),
  IV(NAME_selMarkBox, "int*", IV_NONE,
     NAME_selection, "Drag anchor: hbox index"),
  IV(NAME_selMarkChar, "int*", IV_NONE,
     NAME_selection, "Drag anchor: character offset in tbox")
};

/* Send Methods */

static senddecl send_parbox[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseParBox,
     DEFAULT, "Create parbox from width and content"),
  SM(NAME_compute, 0, NULL, computeParBox,
     DEFAULT, "Compute height"),
  SM(NAME_requestGeometry, 4, T_geometry, requestGeometryParBox,
     DEFAULT, "Change parbox width"),
  SM(NAME_geometry, 4, T_geometry, geometryParBox,
     DEFAULT, "Change parbox width"),
  SM(NAME_append, 1, "hbox", appendParBox,
     NAME_content, "Append a hbox"),
  SM(NAME_cdata, 4, T_cdata, cdataParBox,
     NAME_content, "Append CDATA after breaking into words"),
  SM(NAME_clear, 0, NULL, clearParBox,
     NAME_content, "Delete all contents"),
  SM(NAME_selection, 4, T_selection, selectionParBox,
     NAME_selection, "Set or clear selection [start, end)"),
  SM(NAME_markSelection, 1, "event", markSelectionParBox,
     NAME_selection, "Start a zero-width selection under event"),
  SM(NAME_extendSelection, 1, "event", extendSelectionParBox,
     NAME_selection, "Extend selection to position under event"),
  SM(NAME_selectWord, 1, "event", selectWordParBox,
     NAME_selection, "Select the tbox (word) under event"),
  SM(NAME_extendSelectionWord, 1, "event", extendSelectionWordParBox,
     NAME_selection, "Extend selection by whole words to event position"),
  SM(NAME_hasSelection, 0, NULL, hasSelectionParBox,
     NAME_selection, "Succeed if a non-empty selection is set"),
  SM(NAME_copy, 0, NULL, copyParBox,
     NAME_selection, "Copy selection to clipboard (\\C-c)")
};

/* Get Methods */

static getdecl get_parbox[] =
{ GM(NAME_ascent, 0, "num", NULL, getAscentParBox,
     NAME_dimension, "Ascent of first line"),
  GM(NAME_locateEvent, 1, "1..", "event", getLocateEventParBox,
     NAME_event, "Find hbox from event"),
  GM(NAME_locateChar, 1, "tuple", "event", getLocateCharParBox,
     NAME_event, "Find tuple(hbox, char) from event"),
  GM(NAME_selection, 0, "tuple", NULL, getSelectionParBox,
     NAME_selection, "Tuple(point(box,char), point(box,char))"),
  GM(NAME_selectedText, 0, "string", NULL, getSelectedTextParBox,
     NAME_selection, "New string with contents of selection"),
  GM(NAME_box, 1, "hbox", "1..", getBoxParBox,
     NAME_content, "Get hbox from index"),
  GM(NAME_boxArea, 2, "area", T_boxArea, getBoxAreaParBox,
     NAME_area, "Get bounding box of indicated hbox"),
  GM(NAME_find, 1, "tuple", "code", getFindParBox,
     NAME_iterate, "Return tuple(parbox, index) of matching hbox"),
  GM(NAME_minimumWidth, 0, "int", NULL, getMinimumWidthParBox,
     NAME_dimension, "Return width of largest hbox in paragraph")
#if 0
  GM(NAME_naturalWidth, 1, "int", "max=[0..]", getNaturalWidthParBox,
     NAME_dimension, "Width if no ->line_width limit is imposed")
#endif
};


/* Resources */

static classvardecl rc_parbox[] =
{ RC(NAME_lineWidth, NULL, "500",  NULL),
  RC(NAME_alignment, NULL, "left", NULL),
  RC(NAME_autoCrop,  NULL, "@off", NULL),
  RC(NAME_selectionStyle, "style",
     UXWIN("style(background := yellow)",
	   "@_select_style"),
     "Style for selected text")
};

/* Class Declaration */

static Name parbox_termnames[] = { NAME_width, NAME_alignment };

ClassDecl(parbox_decls,
          var_parbox, send_parbox, get_parbox, rc_parbox,
          2, parbox_termnames);


status
makeClassParBox(Class class)
{ declareClass(class, &parbox_decls);

  setRedrawFunctionClass(class, RedrawAreaParBox);
  solidClass(class, ON);

  succeed;
}
