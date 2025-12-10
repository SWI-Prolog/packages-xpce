/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
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
#include <h/text.h>

#define X_MARGIN    3			/* Space from the border */

forwards Fragment scan_fragment_icons(
		      TextMargin,
		      status (*func)(TextMargin, int x, int y, Fragment, Any ctx),
		      Name, Any);

static status
initialiseTextMargin(TextMargin m, Editor e, Int w, Int h)
{ initialiseGraphical(m, ZERO, ZERO, w, h);
  assign(m, editor, e);
  assign(m, background, getClassVariableValueObject(m, NAME_background));
  assign(m, gap, newObject(ClassSize, EAV));
  copySize(m->gap, getClassVariableValueObject(m, NAME_gap));

  Size isize = getClassVariableValueObject(m, NAME_iconSize);
  if ( isize && instanceOfObject(isize, ClassSize) )
  { assign(m, icon_size, newObject(ClassSize, EAV));
    copySize(m->icon_size, isize);
  }

  succeed;
}


static Style
fragment_style(TextMargin m, Fragment f)
{ Attribute a = getMemberSheet(m->editor->styles, (Any) f->style);

  return a == FAIL ? NIL : a->value;
}


		/********************************
		*            REDRAW		*
		********************************/

static int margin_x, margin_y;

static void
icon_size(TextMargin m, Image icon, int *w, int *h)
{ int iw = valInt(icon->size->w);
  int ih = valInt(icon->size->h);

  if ( isNil(m->icon_size) )
  { *w = iw;
    *h = ih;
  } else
  { double mw = valNum(m->icon_size->w);
    double mh = valNum(m->icon_size->h);

    double f = min(mw/max(mw,iw), mh/max(mh,ih));
    *w = iw*f;
    *h = ih*f;
  }
}


static status
paint_fragment(TextMargin m, int x, int y, Fragment fragment, Any ctx)
{ Image icon;
  Style s;
  (void)ctx;

  if ( notNil(s = fragment_style(m, fragment)) && notNil(icon = s->icon) )
  { int w, h;

    x += margin_x;
    y += margin_y;
    icon_size(m, icon, &w, &h);

    r_image(icon, 0, 0, x, y, w, h);
    if ( m->editor->selected_fragment == fragment )
      r_complement(x, y, w, h);
  }

  succeed;
}


static status
RedrawAreaTextMargin(TextMargin m, Area a)
{ int x, y, w, h;
  Elevation z = getClassVariableValueObject(m, NAME_elevation);
  Any obg;

  initialiseDeviceGraphical(m, &x, &y, &w, &h);

  margin_x = x;
  margin_y = y;

  obg = r_background(m->background);
  r_clear(x, y, w, h);
  if ( z && notNil(z) )
  { r_3d_box(x, y, w, h, 0, z, FALSE);
  } else
  { r_thickness(valInt(m->pen));
    r_dash(m->texture);
    r_box(x, y, w, h, 0, NIL);
  }

  scan_fragment_icons(m, paint_fragment, NAME_forSome, NIL);
  RedrawAreaGraphical(m, a);
  r_background(obg);

  succeed;
}


		/********************************
		*           ATTRIBUTES		*
		********************************/

static status
gapTextMargin(TextMargin m, Size size)
{ return assignGraphical(m, NAME_gap, size);
}

static status
iconSizeMargin(TextMargin m, Size size)
{ return assignGraphical(m, NAME_iconSize, size);
}

static status
backgroundTextMargin(TextMargin m, Any bg)
{ return assignGraphical(m, NAME_background, bg);
}



		/********************************
		*        EVENT HANDLING		*
		********************************/

typedef struct
{ int	x;				/* Target X,Y */
  int   y;
  iarea icon_area;			/* Area of found icon */
} position;


static status
find_fragment(TextMargin m, int x, int y, Fragment fragment, Any ctx)
{ Style s;
  int ex, ey;
  position *pos = ctx;

  if ( isNil(s = fragment_style(m, fragment)) || isNil(s->icon) )
    fail;

  ex = pos->x; ey = pos->y;
  int iw, ih;
  icon_size(m, s->icon, &iw, &ih);
  if ( ex >= x && ey >= y &&
       ex <= x + iw && ey <= y + ih )
  { pos->icon_area.x = x;
    pos->icon_area.y = y;
    pos->icon_area.w = iw;
    pos->icon_area.h = ih;
    succeed;
  }

  fail;
}


static Fragment
getFragmentTextMargin(TextMargin m, EventObj ev)
{ Int ex, ey;

  if ( get_xy_event(ev, m, ON, &ex, &ey) )
  { position pos = {.x = valInt(ex), .y = valInt(ey) };

    answer(scan_fragment_icons(m, find_fragment, NAME_find, &pos));
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
scan_fragment_icons(m, func, how, context)

Scans   through  the  fragmentlist associated   with   'e' and   calls
(*func)(e, x,  y, fragment)  for  all  fragments  that start   at some
displayed line. It assumes e->lines to be initialised. x and y are the
x-y coordinate  at which the top-left corner   of  the icon associated
with 'fragment' should be.

if how  == NAME_forAll succeed if all  succeed, fail on first  failing
function. NAME_find  succeed after   first succeeded fails  otherwise.
NAME_forSome always succeeds;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Fragment
scan_fragment_icons(TextMargin m,
		    status (*func)(TextMargin, int x, int y, Fragment, Any ctx),
		    Name how, Any ctx)
{ Editor e = m->editor;
  TextBuffer tb = e->text_buffer;
  TextImage  ti = e->image;
  Fragment fragment = tb->first_fragment;
  int x = X_MARGIN, y = -1000;
  int mw = valInt(m->area->w);
  int line = 0, lines = ti->map->length;
  int gw = valInt(m->gap->w);
  int gh = valInt(m->gap->h);
  int btm = -gh;
  Style s;
  int skip = ti->map->skip;

  for(; notNil(fragment) && line < lines; line++ )
  { TextLine tl = &ti->map->lines[line + skip];

    while( notNil(fragment) && fragment->start < tl->start )
      fragment = fragment->next;
    DEBUG(NAME_fragment,
	  Cprintf("Scanning line from %ld..%ld.  Fragment=%s\n",
		  tl->start, tl->end, pp(fragment)));

    if ( btm + gh <= tl->y )		/* open the icon-line */
    { y = tl->y;
      x = X_MARGIN;
      btm = tl->y;
    }
    DEBUG(NAME_fragment, Cprintf("tl->y = %d\n", tl->y));

    for( ; notNil(fragment) && fragment->start < tl->end
	 ; fragment = fragment->next )
    { Image icon;

      if ( notNil(s = fragment_style(m, fragment)) && notNil(icon = s->icon) )
      { int aw, ah;

	DEBUG(NAME_fragment, Cprintf("%s has icon %s\n", pp(fragment), pp(icon)));
	icon_size(m, icon, &aw, &ah);
	if ( (x + aw) > mw - X_MARGIN && aw <= mw -X_MARGIN)
        { y = btm + gh;			/* does not fit: next line */
          x = X_MARGIN;
          btm = y;
	}
	int iy;
	if ( y == tl->y )
	  iy = y + (tl->h - ah)/2;
	else
	  iy = y;
	DEBUG(NAME_fragment, Cprintf("Placing %s at %d,%d\n", pp(icon), x, iy));
	if ( equalName(how, NAME_forAll) )
	{ if ( (*func)(m, x, iy, fragment, ctx) == FAIL )
	    fail;
	} else if ( equalName(how, NAME_forSome) )
	{ (*func)(m, x, iy, fragment, ctx);
	} else if ( equalName(how, NAME_find) )
	{ if ( (*func)(m, x, iy, fragment, ctx) == SUCCEED )
	    return fragment;
	}
        x += aw + gw;
        if ( iy+ah > btm )
          btm = iy+ah;
      }
    }
  }

  if ( equalName(how, NAME_find) )
    fail;

  return (Fragment) SUCCEED;
}


static status
eventTextMargin(TextMargin m, EventObj ev)
{ Editor e = (Editor)m->device;

  if ( isNil(e) )
    fail;

  if ( isAEvent(ev, NAME_locMove) )
  { CursorObj c = getClassVariableValueObject(m, NAME_fragmentCursor);

    if ( c && notNil(c) )
    { Int ex, ey;

      if ( get_xy_event(ev, m, ON, &ex, &ey) )
      { position pos = {.x = valInt(ex), .y = valInt(ey) };
	Fragment f = scan_fragment_icons(m, find_fragment, NAME_find, &pos);

	if ( !f ) f = NIL;

	if ( f != m->armed )
	{ assign(m, cursor, isNil(f) ? (CursorObj)NIL : c);
	  assign(m, armed, f);
	  if ( isNil(f) )
	  { send(e, NAME_hoverFragmentIcon, f, EAV);
	  } else
	  { Area a = tempObject(ClassArea,
				toInt(pos.icon_area.x), toInt(pos.icon_area.y),
				toInt(pos.icon_area.w), toInt(pos.icon_area.h),
				EAV);
	    send(e, NAME_hoverFragmentIcon, f, a, EAV);
	    considerPreserveObject(a);
	  }
	}
      }
    }
  } else if ( isAEvent(ev, NAME_areaExit) &&
	      notNil(m->armed) )
  { assign(m, cursor, NIL);
    assign(m, armed, NIL);
    send(e, NAME_hoverFragmentIcon, NIL, EAV);
  }

  if ( isAEvent(ev, NAME_msLeftUp) &&
       getMulticlickEvent(ev) == NAME_single &&
       valInt(getClickDisplacementEvent(ev)) < 5 )
  { Fragment f = getFragmentTextMargin(m, ev);

    if ( f )
      send(e, NAME_selectedFragment, f, EAV);
    else
      send(e, NAME_selectedFragment, NIL, EAV);

    succeed;
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "editor=editor", "width=int", "height=int" };

/* Instance Variables */

static vardecl var_textMargin[] =
{ IV(NAME_editor, "editor", IV_GET,
     NAME_storage, "Editor I'm part of"),
  SV(NAME_gap, "size", IV_GET|IV_STORE, gapTextMargin,
     NAME_layout, "Distance between icons in X and Y"),
  SV(NAME_iconSize, "size*", IV_GET|IV_STORE, iconSizeMargin,
     NAME_layout, "Scale icons to this size"),
  SV(NAME_background, "[colour|pixmap]", IV_GET|IV_STORE, backgroundTextMargin,
     NAME_appearance, "Background colour"),
  IV(NAME_armed, "fragment*", IV_GET,
     NAME_event, "Icon of this fragment is hovered")
};

/* Send Methods */

static senddecl send_textMargin[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseTextMargin,
     DEFAULT, "Create from editor, width and height"),
  SM(NAME_event, 1, "event", eventTextMargin,
     NAME_event, "Handle fragment-selection")
};

/* Get Methods */

static getdecl get_textMargin[] =
{ GM(NAME_fragment, 1, "fragment", "event", getFragmentTextMargin,
     NAME_fragment, "Find the fragment at the event-position")
};

/* Resources */

static classvardecl rc_textMargin[] =
{ RC(NAME_gap, "size", "size(5,2)",
     "Distance between icons in X and Y"),
  RC(NAME_iconSize, "size*", "size(16,16)",
     "Scale icons into this size"),
  RC(NAME_placement, "{left,right}", "left",
     "Placement relative to the image"),
  RC(NAME_elevation, "elevation*", "@nil",
     "Elevation from the background"),
  RC(NAME_background, "[colour|pixmap]", "white",
     "Background colour for the text"),
  RC(NAME_fragmentCursor, "cursor*", "pointer",
     "Cursor when hovering a fragment")
};

/* Class Declaration */

static Name textMargin_termnames[] = { NAME_editor, NAME_width, NAME_height };

ClassDecl(textMargin_decls,
          var_textMargin, send_textMargin, get_textMargin, rc_textMargin,
          3, textMargin_termnames,
          "$Rev$");



status
makeClassTextMargin(Class class)
{ declareClass(class, &textMargin_decls);
  setRedrawFunctionClass(class, RedrawAreaTextMargin);

  succeed;
}
