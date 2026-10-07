/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
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

/* A text caret comes in a number of styles:

     bar        A thin vertical bar left of the character at the caret
     block      A box over the character at the caret
     underline  A line below the character at the caret
     xpce       The classic xpce caret: a small triangle below the
		insertion point
     image      An image with a hot spot (see ->image)

   The caret is drawn by class text_cursor for an editor, and by class
   text and class terminal_image, which draw their own caret.  They all
   use text_caret_style(), text_caret_area() and draw_text_caret() below,
   such that they share the style and colours of class text_cursor.  The
   style depends on whether the font is fixed width or proportional.

   The caret with the keyboard focus blinks.  As only one caret has the
   focus, there is a single blinker: the graphical that owns the caret
   registers using caret_blink_start() and caret_blink_stop(), calls
   caret_blink_reset() if the caret moves and does not draw its caret if
   caret_blink_hidden() is true.
*/

#define CARET_LINE_WIDTH  2.0		/* width of a bar or underline */
#define CARET_BLOCK_ALPHA 0.5		/* opacity of a block */

static status	styleTextCursor(TextCursor c, Name style);

/* The caret style for a font: the class variable
 * text_cursor.fixed_font_style or text_cursor.proportional_font_style
 */

Name
text_caret_style(FontObj font)
{ Name style = NULL;

  if ( notDefault(font) && notNil(font) )
    style = getClassVariableValueClass(ClassTextCursor,
				       getFixedWidthFont(font) == ON
					 ? NAME_fixedFontStyle
					 : NAME_proportionalFontStyle);
  else
    style = getClassVariableValueClass(ClassTextCursor,
				       NAME_proportionalFontStyle);

  return style ? style : NAME_xpce;
}


/* Size of the xpce caret, from the class variable text_cursor.height
 */

static double
xpce_caret_size(void)
{ Int h = getClassVariableValueClass(ClassTextCursor, NAME_height);

  return h ? valNum(h) : 11.0;
}


/* text_caret_area() computes the area of a caret of the given style
 * for the character cell at (x,y) of width w and height h.  b is the
 * baseline, relative to y.
 */

void
text_caret_area(Name style,
		double x, double y, double w, double h, double b,
		double *ax, double *ay, double *aw, double *ah)
{ if ( style == NAME_block )
  { *ax = x;
    *ay = y;
    *aw = w;
    *ah = h;
  } else if ( style == NAME_underline )
  { double uy = y + b + 1.0;

    if ( uy > y + h - CARET_LINE_WIDTH )
      uy = y + h - CARET_LINE_WIDTH;
    *ax = x;
    *ay = uy;
    *aw = w;
    *ah = CARET_LINE_WIDTH;
  } else if ( style == NAME_xpce )
  { double s = xpce_caret_size();

    *ax = x - s/2.0;
    *ay = y + b - 1.0;
    *aw = s;
    *ah = s;
  } else				/* bar */
  { *ax = x - CARET_LINE_WIDTH/2.0;
    *ay = y;
    *aw = CARET_LINE_WIDTH;
    *ah = h;
  }
}


/* draw_text_caret() draws a caret of the given style in the area
 * computed by text_caret_area().  An inactive caret is drawn in its
 * inactive form: a block is drawn as an outline and the xpce caret as
 * a diamond.
 */

void
draw_text_caret(Name style, double x, double y, double w, double h,
		bool active, Any colour)
{ if ( style == NAME_block )
  { if ( active )
    { r_push_group();
      r_fill(x, y, w, h, colour);
      r_pop_group_with_alpha(CARET_BLOCK_ALPHA);
    } else
    { Any old = r_colour(colour);

      r_thickness(1);
      r_dash(NAME_none);
      r_box((int)x, (int)y, (int)w, (int)h, 0, NIL);
      r_colour(old);
    }
  } else if ( style == NAME_xpce )
  { r_fillpattern(colour, NAME_foreground);

    if ( active )
    { double cx = x + w/2.0;

      r_fill_triangle(cx, y, x, y+h, x+w, y+h);
    } else
    { fpoint pts[4];
      double cx = x + w/2.0;
      double cy = y + h/2.0;

      pts[0].x = cx;  pts[0].y = y;
      pts[1].x = x;   pts[1].y = cy;
      pts[2].x = cx;  pts[2].y = y+h;
      pts[3].x = x+w; pts[3].y = cy;

      r_fill_polygon(pts, 4);
    }
  } else				/* bar, underline */
  { r_fill(x, y, w, h, colour);
  }
}


/* The colour of a caret.  The active colour of a text_cursor is its
 * <-colour, which defaults to the class variable.
 */

Any
text_caret_colour(bool active)
{ return getClassVariableValueClass(ClassTextCursor,
				    active ? NAME_colour
					   : NAME_inactiveColour);
}


		/********************************
		*            BLINKING		*
		********************************/

/* The graphical whose caret blinks is the first member of BlinkOwner.
 * If another graphical draws it (a text_item draws its text), this is
 * the second member, which we damage to redraw the caret.  Being a
 * chain, it keeps the graphicals alive while we refer to them.  The
 * timer sends ->blink to @caret_blinker, a text_cursor that is never
 * displayed.
 */

static Chain BlinkOwner = NULL;
static Timer BlinkTimer = NULL;
static void (*blink_changed)(Graphical gr) = NULL;
static bool  blink_off;			/* caret is in its hidden phase */
static int   blink_phases;		/* phases since the last reset */
static int   blink_interval;		/* milliseconds per phase */

static void
blink_init(void)
{ if ( !BlinkTimer )
  { TextCursor blinker;

    BlinkOwner = globalObject(NAME_caretBlinkOwner, ClassChain, EAV);
    blinker    = globalObject(NAME_caretBlinker, ClassTextCursor, EAV);
    BlinkTimer = globalObject(NAME_caretBlinkTimer, ClassTimer,
			      toNum(0.5),
			      newObject(ClassMessage, blinker, NAME_blink, EAV),
			      EAV);
  }
}


static Graphical
blink_owner(void)
{ if ( BlinkOwner )
  { Graphical gr = getHeadChain(BlinkOwner);

    return gr ? gr : NULL;
  }

  return NULL;
}


static void
blink_redraw(Graphical gr)
{ Graphical damage;

  if ( blink_changed )
    (*blink_changed)(gr);
  else if ( (damage = getNth1Chain(BlinkOwner, TWO)) && !isFreedObj(damage) )
    changedEntireImageGraphical(damage);
  else
    changedEntireImageGraphical(gr);
}


/* gr has the active caret.  changed() is called to redraw the caret,
 * or ->changed_entire_image if NULL.
 */

void
caret_blink_start(Graphical gr, void (*changed)(Graphical gr))
{ Int ms;

  blink_init();
  if ( blink_owner() != gr )
  { clearChain(BlinkOwner);
    appendChain(BlinkOwner, gr);
  }
  blink_changed = changed;
  blink_off     = false;
  blink_phases  = 0;
  stopTimer(BlinkTimer);

  if ( getClassVariableValueClass(ClassTextCursor, NAME_blink) == ON &&
       (ms = getClassVariableValueClass(ClassTextCursor,
					NAME_blinkInterval)) &&
       valInt(ms) > 0 )
  { blink_interval = valInt(ms);
    intervalTimer(BlinkTimer, toNum(blink_interval/1000.0));
    startTimer(BlinkTimer, NAME_repeat, DEFAULT);
  }
}


/* The blinking caret of gr is drawn by the graphical damage
 */

void
caret_blink_damage(Graphical gr, Graphical damage)
{ if ( BlinkOwner && blink_owner() == gr )
  { Graphical old = getNth1Chain(BlinkOwner, TWO);

    if ( old != damage )
    { if ( old )
	deleteChain(BlinkOwner, old);
      appendChain(BlinkOwner, damage);
    }
  }
}


/* gr lost the active caret
 */

void
caret_blink_stop(Graphical gr)
{ if ( BlinkTimer && blink_owner() == gr )
  { bool was_off = blink_off;

    stopTimer(BlinkTimer);
    blink_off = false;
    if ( was_off )
      blink_redraw(gr);
    blink_changed = NULL;
    clearChain(BlinkOwner);
  }
}


/* The caret of gr moved or the user typed: show the caret and start the
 * blink cycle again.
 */

void
caret_blink_reset(Graphical gr)
{ if ( BlinkTimer && blink_owner() == gr )
  { bool was_off = blink_off;

    caret_blink_start(gr, blink_changed);
    if ( was_off )
      blink_redraw(gr);
  }
}


/* True if the active caret of gr is in its hidden phase
 */

bool
caret_blink_hidden(Graphical gr)
{ return blink_off && blink_owner() == gr;
}


/* ->blink on @caret_blinker: next phase.  After text_cursor.blink_timeout
 * seconds without a reset the caret stops blinking, visible.
 */

static status
blinkTextCursor(TextCursor c)
{ Graphical gr = blink_owner();
  Int timeout;

  if ( !gr || isFreedObj(gr) )
  { stopTimer(BlinkTimer);
    blink_changed = NULL;
    clearChain(BlinkOwner);
    succeed;
  }

  blink_off = !blink_off;
  blink_phases++;
  if ( !blink_off &&
       (timeout = getClassVariableValueClass(ClassTextCursor,
					     NAME_blinkTimeout)) &&
       valInt(timeout) > 0 &&
       blink_phases * blink_interval >= valInt(timeout) * 1000 )
    stopTimer(BlinkTimer);

  blink_redraw(gr);

  succeed;
}


		/********************************
		*          TEXT_CURSOR		*
		********************************/

static status
initialiseTextCursor(TextCursor c, FontObj font)
{ initialiseGraphical(c, ZERO, ZERO, ZERO, ZERO);
  assign(c, style, text_caret_style(DEFAULT));

  if ( notDefault(font) )
    return send(c, NAME_font, font, EAV);

  succeed;
}


static status
RedrawAreaTextCursor(TextCursor c, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(c, &x, &y, &w, &h);

  if ( c->style == NAME_image )
  { r_image(c->image, 0, 0, x, y, w, h);
  } else if ( c->active == ON && caret_blink_hidden((Graphical)c) )
  { succeed;
  } else
  { bool active = (c->active == ON);
    Any colour;

    if ( active )
      colour = getDisplayColourGraphical((Graphical)c);
    else
      colour = getClassVariableValueObject(c, NAME_inactiveColour);
    if ( !colour )
      colour = BLACK_COLOUR;

    draw_text_caret(c->style, x, y, w, h, active, colour);
  }

  succeed;
}


/* Set the style for the font and the size of the cursor before the
 * editor places it using ->set
 */

static status
fontTextCursor(TextCursor c, FontObj font)
{ if ( c->style != NAME_image )
    TRY(styleTextCursor(c, text_caret_style(font)));

  return geometryGraphical(c, DEFAULT, DEFAULT,
			   getExFont(font), getHeightFont(font));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the text_cursor; (x,y) is the top-left corner of the character at the
insertion point.  w is the width of this character and h is the height of
the line.  y+b is the baseline of the line.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
setTextCursor(TextCursor c, Int x, Int y, Int w, Int h, Int b)
{ double ox = valNum(c->area->x);
  double oy = valNum(c->area->y);

  if ( c->style == NAME_image )
  { TRY(geometryGraphical(c,
			  sub(x, c->hot_spot->x),
			  sub(add(y, b), c->hot_spot->y),
			  c->image->size->w, c->image->size->h));
  } else
  { double ax, ay, aw, ah;

    text_caret_area(c->style,
		    valNum(x), valNum(y), valNum(w), valNum(h), valNum(b),
		    &ax, &ay, &aw, &ah);
    TRY(geometryGraphical(c, toNum(ax), toNum(ay), toNum(aw), toNum(ah)));
  }

  if ( c->active == ON &&		/* the caret moved */
       (valNum(c->area->x) != ox || valNum(c->area->y) != oy) )
    caret_blink_reset((Graphical)c);

  succeed;
}

		/********************************
		*           ATTRIBUTES		*
		********************************/

/* The area depends on the style, so we ask the editor to place the
 * cursor again.
 */

static status
styleTextCursor(TextCursor c, Name style)
{ if ( style == NAME_image &&
       (isNil(c->image) || isNil(c->hot_spot)) )
    return errorPce(c, NAME_needImageAndHotSpot);

  CHANGING_GRAPHICAL(c,
		     assign(c, style, style);
		     changedEntireImageGraphical(c));
  if ( instanceOfObject(c->device, ClassEditor) )
    requestComputeGraphical(c->device, DEFAULT);

  succeed;
}


/* The active caret blinks
 */

static status
activeTextCursor(TextCursor c, BoolObj val)
{ if ( c->active != val )
  { CHANGING_GRAPHICAL(c,
		       assign(c, active, val);
		       changedEntireImageGraphical(c));
  }
  if ( val == ON )
    caret_blink_start((Graphical)c, NULL);
  else
    caret_blink_stop((Graphical)c);

  succeed;
}


static status
imageTextCursor(TextCursor c, Image image, Point hot)
{ CHANGING_GRAPHICAL(c,
	assign(c, image,    image);
	assign(c, hot_spot, hot);
	assign(c, style,    NAME_image);
	changedEntireImageGraphical(c));

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_set[] =
        { "x=int", "y=int", "width=int", "height=int", "baseline=int" };

/* Instance Variables */

static vardecl var_textCursor[] =
{ SV(NAME_style, "{bar,block,underline,xpce,image}", IV_GET|IV_STORE,
     styleTextCursor,
     NAME_appearance, "How the text_cursor object is visualised"),
  SV(NAME_image, "image*", IV_GET|IV_STORE, imageTextCursor,
     NAME_appearance, "Image when <->style is image"),
  IV(NAME_hotSpot, "point*", IV_GET,
     NAME_appearance, "The `hot-spot' of the image")
};

/* Send Methods */

static senddecl send_textCursor[] =
{ SM(NAME_initialise, 1, "for=[font]", initialiseTextCursor,
     DEFAULT, "Create for specified font"),
  SM(NAME_font, 1, "font", fontTextCursor,
     NAME_appearance, "Set the style and initial size from the font"),
  SM(NAME_active, 1, "bool", activeTextCursor,
     NAME_appearance, "The caret is active (blinks) or not"),
  SM(NAME_blink, 0, NULL, blinkTextCursor,
     NAME_internal, "Next blink phase (@caret_blinker)"),
  SM(NAME_set, 5, T_set, setTextCursor,
     NAME_area, "Set x, y, w, h and baseline")
};

/* Get Methods */

#define get_textCursor NULL
/*
static getdecl get_textCursor[] =
{
};
*/

/* Resources */

static classvardecl rc_textCursor[] =
{ RC(NAME_fixedFontStyle, "{bar,block,underline,xpce}", "xpce",
     "Caret style for fixed width fonts"),
  RC(NAME_proportionalFontStyle, "{bar,block,underline,xpce}", "xpce",
     "Caret style for proportional fonts"),
  RC(NAME_blink, "bool", "@on",
     "If @on, the active caret blinks"),
  RC(NAME_blinkInterval, "1..", "500",
     "Milliseconds the caret is shown and hidden when blinking"),
  RC(NAME_blinkTimeout, "0..", "10",
     "Stop blinking after this many seconds without typing (0: never)"),
  RC(NAME_colour, RC_REFINE, "ui_cursor", NULL),
  RC(NAME_inactiveColour, RC_REFINE, "ui_cursor_inactive", NULL),
  RC(NAME_height, "int", "11", "Size of the xpce style caret")
};

/* Class Declaration */

static Name textCursor_termnames[] = { NAME_style };

ClassDecl(textCursor_decls,
          var_textCursor, send_textCursor, get_textCursor, rc_textCursor,
          1, textCursor_termnames);

status
makeClassTextCursor(Class class)
{ declareClass(class, &textCursor_decls);
  setRedrawFunctionClass(class, RedrawAreaTextCursor);

  succeed;
}
