/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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
#include <cairo/cairo.h>
#include <math.h>
#include "sdldraw.h"
#include "sdlcolour.h"
#include "sdlfont.h"
#include "sdlimage.h"
#include "sdldisplay.h"
#include "sdlframe.h"
#include "sdlwindow.h"

#define MAX_CTX_DEPTH (10)		/* Max draw context depth */

typedef struct
{ int		open;			/* Allow for nested open */
  PceWindow	window;			/* Pce's notion of the window */
  FrameObj	frame;			/* Pce's frame of the window */
  DisplayObj	display;		/* Pce's display for the frame */
  cairo_surface_t *target;		/* Target for rendering to */
  cairo_t      *cr;			/* Cairo context */
  int		offset_x;		/* Paint offset in X direction */
  int		offset_y;		/* Paint offset in Y direction */
  int		fixed_colours;		/* Colours are fixed */
  Any		colour;			/* Current colour */
  Any		background;		/* Background colour */
  Any		default_colour;
  Any		default_background;
  Any		fill_pattern;		/* Default for fill operations */
  Name		dash;			/* Dash pattern */
  double	pen;			/* Drawing thickness */
} sdl_draw_context;

#define X(x) ((x) + context.offset_x)
#define Y(y) ((y) + context.offset_y)
#define Translate(x, y)	 { (x) = X(x); (y) = Y(y); }
#define InvTranslate(x, y) { x -= context.offset_x; y -= context.offset_y; }
#define CR (context.cr)

#define FloatArea(x, y, w, h)		     \
  double _lw = (double)context.pen/2.0;	     \
  double fx = (x)+_lw/2.0;		     \
  double fy = (y)+_lw/2.0;		     \
  double fw = (w)-_lw;			     \
  double fh = (h)-_lw;

static void pce_cairo_set_source_color(cairo_t *cr, Colour pce);
#if 0
static bool validate_cairo_text_consistency(cairo_t *draw_cr);
#endif

static void r_fill_fgbg(double x, double y, double w, double h,
			Any fill, Name which);


		 /*******************************
		 *        CONTEXT STACK         *
		 *******************************/

#include <gra/graphstate.c>

static sdl_draw_context	context;	/* current context */
static sdl_draw_context	ctx_stack[MAX_CTX_DEPTH];  /* Context stack */
static int		ctx_stacked;	/* Saved frames */

static void
reset_context(void)
{ memset(&context, 0, sizeof(context));
}

static void
push_context(void)
{ if ( context.open )
    ctx_stack[ctx_stacked++] = context;
  if ( ctx_stacked >= MAX_CTX_DEPTH )
    Cprintf("**************** ERROR: Draw Context Stack overflow\n");

  reset_context();
}


/**
 * Reset the drawing state to its default values.
 */
void
resetDraw(void)
{ memset(&context, 0, sizeof(context));
}

void
d_init_surface(cairo_surface_t *surf, Any background)
{ int width   = cairo_image_surface_get_width(surf);
  int height  = cairo_image_surface_get_height(surf);
  cairo_t *cr = cairo_create(surf);
  cairo_new_path(cr);
  if ( instanceOfObject(background, ClassColour) )
  { pce_cairo_set_source_color(cr, background);
  } else
  { Cprintf("stub: non-colour background: %s\n", pp(background));
  }
  cairo_rectangle(cr, 0, 0, width, height);
  cairo_fill(cr);
  cairo_destroy(cr);
}

/**
 * Set the drawing offset for subsequent drawing operations.
 *
 * @param x The x-coordinate offset.
 * @param y The y-coordinate offset.
 */
void
d_offset(int x, int y)
{ DEBUG(NAME_redraw, Cprintf("d_offset(%d, %d)\n", x, y));

  context.offset_x = x;
  context.offset_y = y;
}

/**
 * Set the rendering offset for subsequent rendering operations.
 *
 * @param x The x-coordinate offset.
 * @param y The y-coordinate offset.
 */
void
r_offset(int x, int y)
{ context.offset_x += x;
  context.offset_y += y;
}

/**
 * Initialize the fill state with the specified offset and starting point.
 *
 * @param offset Pointer to the Point object representing the offset.
 * @param x0 The starting x-coordinate.
 * @param y0 The starting y-coordinate.
 * @param state Pointer to the fill_state structure to be initialized.
 */
void
r_filloffset(Point offset, int x0, int y0, fill_state *state)
{
}

/**
 * Restore the fill state from the provided state structure.
 *
 * @param state Pointer to the fill_state structure to be restored.
 */
void
r_fillrestore(fill_state *state)
{
}

/**
 * Switch to a new display, returning the old display
 *
 * @param d Pointer to the DisplayObj.
 * @return The previous DisplayObj.
 */
DisplayObj
d_display(DisplayObj d)
{ DisplayObj old = context.display;
  context.display = d;
  return old;
}

/**
 * Ensure that the display is properly initialized and ready for drawing.
 */
void
d_ensure_display(void)
{ if ( context.display == NULL )
    d_display(CurrentDisplay(NIL));
}

/**
 * Flush all pending drawing operations to the display.
 */
void
d_flush(void)
{
}

/**
 * Start  drawing in  a window.   The x,y,w,h  describe the  region to
 * paint in  window client  coordinates.  The drawing  code translates
 * this  to  window coordinates  using  Translate(x,y),  based on  the
 * window `scroll_offset`.
 *
 * @param sw Pointer to the PceWindow object.
 * @param x X offset of the region we will paint
 * @param y Y offset of the region we will paint
 * @param w The width of the region we will paint.
 * @param h The height of the region we will paint.
 * @param clear Flag indicating whether to clear the region
 * @param limit is unused and always TRUE.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
d_window(PceWindow sw, int x, int y, int w, int h, int clear, int limit)
{ if ( !ws_created_window(sw) )
  { Cprintf("d_window(%s): not created\n");
    fail;
  }

  DEBUG(NAME_redraw,
	Cprintf("d_window(%s, %d, %d, %d, %d, %d) off=%d,%d\n",
		pp(sw), x, y, w, h, clear,
		valInt(sw->scroll_offset->x),
		valInt(sw->scroll_offset->y)));

  if ( context.open && context.window == sw )
  { Cprintf("d_window(%s): Context is already open\n",
	    pp(sw));
    context.open++;
    succeed;
  }

  push_context();
  context.open = 1;

  FrameObj  fr = getFrameWindow(sw, OFF);
  DisplayObj d = fr->display;
  WsWindow wsw = sw->ws_ref;

  context.window     = sw;
  context.frame      = fr;
  context.display    = d;
  context.target     = wsw->backing;
  context.cr         = cairo_create(context.target);
  context.offset_x   = valInt(sw->scroll_offset->x);
  context.offset_y   = valInt(sw->scroll_offset->y);
  context.colour     = notDefault(sw->colour) ? sw->colour : d->foreground;
  context.background = sw->background;
  context.default_colour = context.colour;
  context.default_background = context.background;

  /* do we need to clip? */

  d_clip(x, y, w, h);
  if ( clear )
    r_fill(x, y, w, h, context.background);

  succeed;
}

/**
 * Draw into an image at the specified location with given dimensions.
 *
 * @param i Pointer to the Image object.
 * @param x
 * @param y
 * @param w
 * @param h Area of the image that will be affected.
 */
status
d_image(Image i, int x, int y, int w, int h)
{ DisplayObj d =  CurrentDisplay(NIL);
  ws_open_image(i, d, valReal(i->scale));
  Any colour = i->foreground;
  Any background = i->background;

  if ( isDefault(colour) )
    colour = d->foreground;
  if ( isDefault(background) )
    background = d->background;

  push_context();
  context.open = 1;

  context.display            = d;
  context.target             = i->ws_ref;
  context.cr                 = cairo_create(context.target);
  context.background         = background;
  context.colour             = colour;
  context.default_colour     = context.colour;
  context.default_background = context.background;

  succeed;
}

/**
 * Set the current drawing target to the specified display.
 *
 * @param d Pointer to the DisplayObj.
 */
status
d_screen(DisplayObj d)
{ Cprintf("stub: d_screen(%s)\n", pp(d));
  fail;
}

/**
 * Draw a frame at the specified location with given dimensions.
 *
 * @param fr Pointer to the FrameObj.
 * @param x The x-coordinate of the frame.
 * @param y The y-coordinate of the frame.
 * @param w The width of the frame.
 * @param h The height of the frame.
 */
status
d_frame(FrameObj fr, int x, int y, int w, int h)
{ Cprintf("stub: d_frame(%s)\n", pp(fr));
  fail;
}

/**
 * Define a clipping region for subsequent drawing operations.
 *
 * @param x The x-coordinate of the clipping region.
 * @param y The y-coordinate of the clipping region.
 * @param w The width of the clipping region.
 * @param h The height of the clipping region.
 */
void
d_clip(int x, int y, int w, int h)
{ NormaliseArea(x, y, w, h);
  Translate(x, y);
  cairo_reset_clip(CR);
  cairo_rectangle(CR, x, y, w, h);
  cairo_clip(CR);
}

/**
 * Finalize the current drawing operation.
 */
void
d_done(void)
{ DEBUG(NAME_redraw, Cprintf("d_done(): open = %d\n", context.open));
  if ( --context.open == 0 )
  { cairo_destroy(context.cr);
    context.cr = NULL;
    if ( ctx_stacked )
      context = ctx_stack[--ctx_stacked];
    else
      reset_context();
  }
}

/**
 * Remove the current clipping region.
 */
void
d_clip_done(void)
{ cairo_reset_clip(CR);
}

/**
 * Compute the intersection of two IArea structures.
 *
 * @param a Pointer to the first IArea.
 * @param b Pointer to the second IArea.
 */
void
intersection_iarea(IArea a, IArea b)
{
}

		 /*******************************
		 *         CAIRO UTILS          *
		 *******************************/

static void
pce_cairo_set_source_color(cairo_t *cr, Colour pce)
{ SDL_Color c = pceColour2SDL_Color(pce);
  cairo_set_source_rgba(cr, c.r/256.0, c.g/256.0, c.b/256.0, c.a/256.0);
}

static void
pce_cairo_set_font(cairo_t *cr, FontObj pce)
{ WsFont wsf = ws_get_font(pce);
  if ( wsf )
  { cairo_set_scaled_font(cr, wsf->font);
    cairo_set_font_matrix(cr, &wsf->matrix);
  } else
    Cprintf("stub: No font for %s\n", pp(pce));
}

#if 0
static bool
ctm_equal(cairo_t *cr1, cairo_t *cr2)
{ cairo_matrix_t m1, m2;
  cairo_get_matrix(cr1, &m1);
  cairo_get_matrix(cr2, &m2);
  return memcmp(&m1, &m2, sizeof(cairo_matrix_t)) == 0;
}

static bool
font_options_equal(cairo_t *cr1, cairo_t *cr2)
{ cairo_font_options_t *fo1 = cairo_font_options_create();
  cairo_font_options_t *fo2 = cairo_font_options_create();

  cairo_get_font_options(cr1, fo1);
  cairo_get_font_options(cr2, fo2);

  bool same = cairo_font_options_equal(fo1, fo2);

  cairo_font_options_destroy(fo1);
  cairo_font_options_destroy(fo2);
  return same;
}

static bool
font_matrix_equal(cairo_t *cr1, cairo_t *cr2)
{ cairo_matrix_t fm1, fm2;
  cairo_get_font_matrix(cr1, &fm1);
  cairo_get_font_matrix(cr2, &fm2);
  return memcmp(&fm1, &fm2, sizeof(cairo_matrix_t)) == 0;
}

static bool
validate_cairo_text_consistency(cairo_t *draw_cr)
{ DisplayObj d = context.display;
  if ( !d )
    d = CurrentDisplay(NIL);
  if ( d )
  { WsDisplay wsd = d->ws_ref;
    cairo_t *hidden_cr = wsd->hidden_cairo;

    bool ctm_ok = ctm_equal(draw_cr, hidden_cr);
    bool opt_ok = font_options_equal(draw_cr, hidden_cr);
    bool mtx_ok = font_matrix_equal(draw_cr, hidden_cr);
    bool rc = ctm_ok&&opt_ok&&mtx_ok;

    if ( !rc )
    { Cprintf("Inconsistent text context for %s: %d %d %d\n",
	      pp(context.window), ctm_ok, opt_ok, mtx_ok);
    }
    return rc;
  } else
  { Cprintf("validate_cairo_text_consistency(): no display\n");
    return true;
  }
}
#endif


		 /*******************************
		 *      DRAWING PRIMITIVES      *
		 *******************************/

/**
 * Invert the colors within a specified rectangular area.
 *
 * @param x The x-coordinate of the top-left corner of the rectangle.
 * @param y The y-coordinate of the top-left corner of the rectangle.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 */
void
r_complement(int x, int y, int w, int h)
{
}

/**
 * Perform a bitwise AND operation with a pattern over a rectangular area.
 *
 * @param x The x-coordinate of the top-left corner of the rectangle.
 * @param y The y-coordinate of the top-left corner of the rectangle.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @param pattern The image pattern to use for the operation.
 */
void
r_and(int x, int y, int w, int h, Image pattern)
{
}

/**
 * Set the thickness of lines for subsequent drawing operations.
 *
 * @param pen The desired pen thickness.
 */
double
r_thickness(double pen)
{ double old = context.pen;
  context.pen = pen;
  return old;
}

/**
 * Apply a transformation to a value, typically for coordinate adjustments.
 *
 * @param val The value to be transformed.
 * @return The transformed value.
 */
int
r_transformed(int val)
{
    return 0;
}

/**
 * Set the dash pattern for lines.
 *
 * @param name The name of the dash pattern to apply.
 */
static struct dashpattern
{ Name	  dash;
  double  *dash_list;
  int	  dash_list_length;
} dash_patterns[] =
{ { NAME_none,	     NULL,					  0},
  { NAME_dotted,     (double[]){1.0,2.0},			  2},
  { NAME_dashed,     (double[]){7.0,7.0},			  2},
  { NAME_dashdot,    (double[]){7.0,3.0,1.0,7.0},		  4},
  { NAME_dashdotted, (double[]){9.0,3.0,1.0,3.0,1.0,3.0,1.0,3.0}, 8},
  { NAME_longdash,   (double[]){13.0,7.0},			  2},
  { 0,		     NULL,					  0},
};

void
r_dash(Name name)
{ if ( name != context.dash )
  { struct dashpattern *dp = dash_patterns;

    for( ; dp->dash != 0; dp++ )
    { if ( dp->dash == name )
      { cairo_set_dash(CR, dp->dash_list, dp->dash_list_length, 0);

	context.dash = name;
	return;
      }
    }
    errorPce(name, NAME_badTexture);
  }
}

/**
 * Set the current pen for drawing operations.
 *
 * @param pen The pen object defining drawing attributes.
 */
void
d_pen(Pen pen)
{
}

/**
 * Set the fill pattern for subsequent drawing operations.
 *
 * @param fill The fill pattern to use.
 * @param which One of `foreground` or `background` if `fixed_colours`
 *        is active.   Currently ignored.
 */
void
r_fillpattern(Any fill, Name which)
{ if ( context.fixed_colours && !instanceOfObject(fill, ClassImage) )
  { fill = (which == NAME_foreground ? context.colour
				     : context.background);
  } else if ( isDefault(fill) )
    fill = context.colour;
  else if ( fill == NAME_foreground )
    fill = context.colour;
  else if ( fill == NAME_background )
    fill = context.background;
  else if ( fill == NAME_current )
    return;

  context.fill_pattern = fill;
}

/**
 * Set the arc drawing mode (e.g., pie slice, chord).
 *
 * @param mode The name of the arc mode to set.
 */
void
r_arcmode(Name mode)
{
}

/**
 * Fix the foreground and background colours.  This is used to draw
 * selected or (de-)activated objects with a particular colour.
 *
 * @param fg The foreground color.
 * @param bg The background color.
 * @param ctx Context to save current setting to be restored
 *        by r_unfix_colours()
 */
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

/**
 * Restore the previous color context, undoing any temporary color changes.
 *
 * @param ctx The color context to restore.
 */
void
r_unfix_colours(ColourContext ctx)
{ if ( (context.fixed_colours = ctx->lock) == 0 )
  { r_default_colour(ctx->foreground);
    r_background(ctx->background);
  }
}

/**
 * Retrieve the default color for a given input.
 *
 * @param c The input color or identifier.
 * @return The default color associated with the input.
 */
Any
r_default_colour(Any c)
{ Any old = context.default_colour;

  if ( !context.fixed_colours )
  { if ( notDefault(c) )
      context.default_colour = c;

    r_colour(context.default_colour);
  }

  return old;
}

/**
 * Retrieve the current drawing color.
 *
 * @param c The input color or identifier.
 * @return The current drawing color.
 */
Any
r_colour(Any c)
{ Any old = context.colour;

  if ( context.fixed_colours )
    return old;

  if ( isDefault(c) )
     c = context.default_colour;

  context.colour = c;

  return old;
}

/**
 * Set the background color.
 *
 * @param c A color, DEFAULT or an image.
 * @return The old background.
 */
Any
r_background(Any c)
{ Any old = context.background;

  if ( isDefault(c) || context.fixed_colours )
    return old;
  context.background = c;

  return old;
}

/**
 * Swap the foreground and background colors.
 */
void
r_swap_background_and_foreground(void)
{ Any tmp = context.background;

  context.background = context.colour;
  context.colour = tmp;
}

/**
 * Set or retrieve the subwindow drawing mode.
 *
 * @param val The boolean value to set or retrieve the mode.
 * @return The current subwindow mode.
 */
BoolObj
r_subwindow_mode(BoolObj val)
{
    return val;
}

/**
 * Set or retrieve the invert drawing mode.
 *
 * @param val The boolean value to set or retrieve the mode.
 */
void
r_invert_mode(BoolObj val)
{
}

/**
 * Translate coordinates by a specified offset.
 *
 * @param x The x-offset.
 * @param y The y-offset.
 * @param ox Pointer to store the original x-coordinate.
 * @param oy Pointer to store the original y-coordinate.
 */
void
r_translate(int x, int y, int *ox, int *oy)
{ Translate(x, y);

  *ox = x;
  *oy = y;
}

void
my_cairo_rounded_rectangle(cairo_t *cr, double x, double y, double w, double h,
			   double r)
{ double x0 = x,     y0 = y;
  double x1 = x + w, y1 = y + h;

  if (r > w / 2) r = w / 2;
  if (r > h / 2) r = h / 2;

  cairo_new_sub_path(cr);
  cairo_arc(cr, x1 - r, y0 + r, r, -90 * M_PI/180.0,   0 * M_PI/180.0);
  cairo_arc(cr, x1 - r, y1 - r, r,   0 * M_PI/180.0,  90 * M_PI/180.0);
  cairo_arc(cr, x0 + r, y1 - r, r,  90 * M_PI/180.0, 180 * M_PI/180.0);
  cairo_arc(cr, x0 + r, y0 + r, r, 180 * M_PI/180.0, 270 * M_PI/180.0);
  cairo_close_path(cr);
}


/**
 * Draw a rectangle (box) with optional rounded corners and fill.
 *
 * @param x The x-coordinate of the top-left corner.
 * @param y The y-coordinate of the top-left corner.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @param r The radius for rounded corners.
 * @param fill The fill pattern or color.
 */
void
r_box(int x, int y, int w, int h, int r, Any fill)
{ Translate(x, y);
  NormaliseArea(x, y, w, h);
  FloatArea(x, y, w, h);	/* reduce by pen */

  DEBUG(NAME_draw,
	Cprintf("r_box(%d, %d, %d, %d, %d, %s)\n",
		x, y, w, h, r, pp(fill)));

  cairo_new_path(CR);
  cairo_set_line_width(CR, context.pen);
  cairo_save(CR);
  cairo_set_antialias(CR, CAIRO_ANTIALIAS_NONE);
  if ( r )
    my_cairo_rounded_rectangle(CR, fx, fy, fw, fh, r);
  else
    cairo_rectangle(CR, fx, fy, fw, fh);
  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_background);
    pce_cairo_set_source_color(CR, context.fill_pattern);
    cairo_fill_preserve(CR);
  }
  if ( context.pen )
  { pce_cairo_set_source_color(CR, context.colour);
    cairo_stroke(CR);
  }
  cairo_restore(CR);
}

/**
 * Draw a rectangle with a shadow effect.
 *
 * @param x The x-coordinate of the top-left corner.
 * @param y The y-coordinate of the top-left corner.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @param r The radius for rounded corners.
 * @param shadow The size of the shadow.
 * @param fill The fill pattern or image.
 */
void
r_shadow_box(int x, int y, int w, int h, int r, int shadow, Any fill)
{ if ( !shadow )
  { r_box(x, y, w, h, r, fill);
  } else
  { if ( shadow > h ) shadow = h;
    if ( shadow > w ) shadow = w;

    r_colour(BLACK_COLOUR);
    r_box(x+shadow, y+shadow, w-shadow, h-shadow, r, BLACK_COLOUR);
    r_colour(DEFAULT);
    r_box(x, y, w-shadow, h-shadow, r, isNil(fill) ? WHITE_COLOUR : fill);
  }
}

/**
 * Sets  the  fill-pattern for  the  interior  of elevated  areas  and
 * returns TRUE  if the  interior needs to  be filled.   Returns FALSE
 * otherwise.   The special  colours `reduced'  and `highlighted'  are
 * interpreted as relative colours to the background.
*/

static bool
r_elevation_fillpattern(Elevation e, bool up)
{ Any fill = NIL;

  if ( up && notDefault(e->colour) )
  { fill = e->colour;
  } else if ( !up && notDefault(e->background) )
  { fill = e->background;
  }

  if ( isNil(fill) )
    return false;

  if ( fill == NAME_reduced || fill == NAME_hilited )
  { Any bg = context.background;

    if ( instanceOfObject(bg, ClassColour) )
    { if ( fill == NAME_reduced )
	fill = getReduceColour(bg, DEFAULT);
      else
	fill = getHiliteColour(bg, DEFAULT);
    } else
      return false;
  }

  r_fillpattern(fill, NAME_background);

  return true;
}


/**
 * Retrieve the shadow associated with a specific elevation level.
 *
 * @param e The elevation level.
 * @return The shadow corresponding to the elevation.
 */
Any
r_elevation_shadow(Elevation e)
{ if ( isDefault(e->shadow) )
  { Any bg = context.background;

    if ( instanceOfObject(bg, ClassColour) )
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

    if ( instanceOfObject(bg, ClassColour) )
      return getHiliteColour(bg, DEFAULT);
    else
      return WHITE_COLOUR;
  } else
    return e->relief;
}

/**
 * Draw a series of 3D segments with lighting effects.
 *
 * @param n The number of segments.
 * @param s The array of segments to draw.
 * @param e The elevation level for 3D effect.
 * @param light The lighting intensity or direction.
 */
void
r_3d_segments(int n, ISegment s, Elevation e, int light)
{
}

/**
 * Draw a 3D-styled rectangle.
 *
 * @param x The x-coordinate of the top-left corner.
 * @param y The y-coordinate of the top-left corner.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @param radius The corner radius for rounding.
 * @param e The elevation level for 3D effect.
 * @param up Boolean indicating if the box appears raised.
 */

#define MAX_SHADOW 10

void
r_3d_box(int x, int y, int w, int h, int radius, Elevation e, int up)
{ int shadow = valInt(e->height);

  DEBUG(NAME_draw,
	Cprintf("r_3d_box(%d, %d, %d, %d, %d, %s, %d)\n",
		x, y, w, h, radius, pp(e), up));

  NormaliseArea(x, y, w, h);
  FloatArea(x, y, w, h);
  if ( radius > 0 )
  { int maxr = min(w,h)/2;

    if ( radius > maxr )
      radius = maxr;
  }

  if ( e->kind == NAME_shadow )
  { shadow = abs(shadow);
    shadow = min(shadow, min(w, h));
    if ( shadow > MAX_SHADOW )
      shadow = MAX_SHADOW;
    r_box(x, y, w-shadow, h-shadow, radius-shadow, e->colour);

    int xt = x, yt = y;
    Translate(xt, yt);

    if ( radius > 0 )
    { Cprintf("stub: r_3d_box(): shadow and radius;\n");
    } else
    { w -= shadow;
      h -= shadow;

      pce_cairo_set_source_color(CR, r_elevation_shadow(e));
      cairo_set_line_width(CR, 1);
      for( int os=0; os < shadow; os++ )
      { cairo_move_to(CR, xt+w+os,   yt+shadow);
	cairo_line_to(CR, xt+w+os,   yt+h+os);
	cairo_line_to(CR, xt+shadow, yt+h+os);
	cairo_stroke(CR);
      }
    }
  } else
  { bool fill = r_elevation_fillpattern(e, up);

    if ( !up  )
      shadow = -shadow;

    Translate(fx, fy);
    if ( shadow )
    { Colour top_left_color;
      Colour bottom_right_color;

      if ( shadow > 0 )
      { top_left_color     = r_elevation_relief(e);
	bottom_right_color = r_elevation_shadow(e);
      } else
      { top_left_color     = r_elevation_shadow(e);
	bottom_right_color = r_elevation_relief(e);
	shadow             = -shadow;
      }

      if ( shadow > MAX_SHADOW )
	shadow = MAX_SHADOW;

      if ( radius > 0 )			/* with rounded corners */
      { Cprintf("r_3d_box(): with radius\n");
      } else
      { double fr = fx+fw;
	double fb = fy+fh;

	pce_cairo_set_source_color(CR, top_left_color);
	cairo_set_line_width(CR, 1);
	for(int os=0; os<shadow; os++)
	{ cairo_move_to(CR, fr-os, fy-os);
	  cairo_line_to(CR, fx+os, fy+os);
	  cairo_line_to(CR, fx+os, fb-os);
	  cairo_stroke(CR);
	}
	pce_cairo_set_source_color(CR, bottom_right_color);
	for(int os=0; os<shadow; os++)
	{ cairo_move_to(CR, fr-os, fy-os);
	  cairo_line_to(CR, fr-os, fb-os);
	  cairo_line_to(CR, fx+os, fb-os);
	  cairo_stroke(CR);
	}
      }
    }

    if ( fill )			/* r_fill_fgbg() uses floats  */
      r_fill_fgbg(fx+shadow, fy+shadow, fw-2*shadow,
		  fh-2*shadow, NAME_current, NAME_background);
  }
}

/**
 * Draw a 3D-styled line between two points.
 *
 * @param x1 The x-coordinate of the starting point.
 * @param y1 The y-coordinate of the starting point.
 * @param x2 The x-coordinate of the ending point.
 * @param y2 The y-coordinate of the ending point.
 * @param e The elevation level for 3D effect.
 * @param up Boolean indicating if the line appears raised.
 */
void
r_3d_line(int x1, int y1, int x2, int y2, Elevation e, int up)
{ int z = valInt(e->height);
  Colour up_color, down_color;

  Translate(x1, y1);
  Translate(x2, y2);

  if ( up )
  { up_color   = r_elevation_relief(e);
    down_color = r_elevation_shadow(e);
  } else
  { down_color = r_elevation_shadow(e);
    up_color   = r_elevation_relief(e);
  }

  if ( abs(y1-y2) < abs(x1-x2) )
  { cairo_set_line_width(CR, z);
    pce_cairo_set_source_color(CR, down_color);
    cairo_move_to(CR, x1, (double)y1-(double)z/2);
    cairo_line_to(CR, x2, (double)y2-(double)z/2);
    cairo_stroke(CR);
    pce_cairo_set_source_color(CR, up_color);
    cairo_move_to(CR, x1, (double)y1+(double)z/2);
    cairo_line_to(CR, x2, (double)y2+(double)z/2);
    cairo_stroke(CR);
  } else
  { Cprintf("stub: r_3d_line() (not horizontal)\n");
  }
}

/**
 * Draw a 3D-styled triangle.
 *
 * @param x1 The x-coordinate of the first vertex.
 * @param y1 The y-coordinate of the first vertex.
 * @param x2 The x-coordinate of the second vertex.
 * @param y2 The y-coordinate of the second vertex.
 * @param x3 The x-coordinate of the third vertex.
 * @param y3 The y-coordinate of the third vertex.
 * @param e The elevation level for 3D effect.
 * @param up Boolean indicating if the triangle appears raised.
 * @param map Bitmap of up/down edges
 */
static inline void
step_to(int *x1, int *y1, int tx, int ty)
{ if ( tx > *x1 )
    (*x1)++;
  else if ( tx < *x1 )
    (*x1)--;

  if ( ty > *y1 )
    (*y1)++;
  else if ( ty < *y1 )
    (*y1)--;
}

void
r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3,
	      Elevation e, int up, int map)
{ int shadow = valInt(e->height);
  Colour up_color, down_color;

  DEBUG(NAME_draw,
	Cprintf("r_3d_triangle(%d,%d, %d,%d, %d,%d %s, %d)\n",
		x1,y1, x2,y2, x3,y3, pp(e), up));

  if ( !up  )
    shadow = -shadow;
  if ( shadow > 0 )
  { up_color   = r_elevation_relief(e);
    down_color = r_elevation_shadow(e);
  } else
  { down_color = r_elevation_shadow(e);
    up_color   = r_elevation_relief(e);
  }

  int cx = (x1 + x2 + x3)/3;
  int cy = (y1 + y2 + y3)/3;

  cairo_set_line_width(CR, 1);
  for(int os=0; os<shadow; os++)
  { if ( map == 0x3 )		/* line 1 and 3 up */
    { pce_cairo_set_source_color(CR, up_color);
      cairo_move_to(CR, X(x1), Y(y1));
      cairo_line_to(CR, X(x2), Y(y2));
      cairo_line_to(CR, X(x3), Y(y3));
      cairo_stroke(CR);
      pce_cairo_set_source_color(CR, down_color);
      cairo_move_to(CR, X(x3), Y(y3));
      cairo_line_to(CR, X(x1), Y(y1));
      cairo_stroke(CR);
    } else
    { Cprintf("stub: r_3d_triangle(): map=0x%x\n", map);
    }

    step_to(&x1, &y1, cx, cy);
    step_to(&x2, &y2, cx, cy);
    step_to(&x3, &y3, cx, cy);
  }

  if ( r_elevation_fillpattern(e, up) )
    r_fill_triangle(x1, y1, x2, y2, x3, y3);
}

/**
 * Draw a 3D-styled diamond shape.
 *
 * @param x The x-coordinate of the top-left corner.
 * @param y The y-coordinate of the top-left corner.
 * @param w The width of the diamond.
 * @param h The height of the diamond.
 * @param e The elevation level for 3D effect.
 * @param up Boolean indicating if the diamond appears raised.
 */
void
r_3d_diamond(int x, int y, int w, int h, Elevation e, int up)
{
}

/**
 * Draw an arc within a specified rectangle.
 *
 * @param x The x-coordinate of the top-left corner of the bounding rectangle.
 * @param y The y-coordinate of the top-left corner of the bounding rectangle.
 * @param w The width of the bounding rectangle.
 * @param h The height of the bounding rectangle.
 * @param s The starting angle of the arc.
 * @param e The ending angle of the arc.
 * @param fill The fill pattern or color.
 */
void
r_arc(int x, int y, int w, int h, int s, int e, Any fill)
{
}

/**
 * Draw an ellipse within a specified rectangle.
 *
 * @param x The x-coordinate of the top-left corner of the bounding rectangle.
 * @param y The y-coordinate of the top-left corner of the bounding rectangle.
 * @param w The width of the bounding rectangle.
 * @param h The height of the bounding rectangle.
 * @param fill The fill pattern or color.
 */
void
r_ellipse(int x, int y, int w, int h, Any fill)
{ Translate(x, y);
  NormaliseArea(x, y, w, h);
  FloatArea(x, y, w, h);

  DEBUG(NAME_draw,
	Cprintf("r_ellipse(%d, %d, %d, %d, %s)\n",
		x, y, w, h, pp(fill)));

  cairo_save(CR);
  cairo_translate(CR, fx + fw / 2.0, fy + fh / 2.0);  // Move to center
  cairo_scale(CR, fw / 2.0, fh / 2.0);              // Scale unit circle
  cairo_arc(CR, 0, 0, 1.0, 0, 2 * M_PI);
  cairo_restore(CR);
  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_foreground);
    pce_cairo_set_source_color(CR, context.fill_pattern);
    cairo_fill_preserve(CR);
  }
  if ( context.pen )
  { pce_cairo_set_source_color(CR, context.colour);
    cairo_stroke(CR);
  }
}

/**
 * Draw a 3D-styled ellipse.
 *
 * @param x The x-coordinate of the top-left corner of the bounding rectangle.
 * @param y The y-coordinate of the top-left corner of the bounding rectangle.
 * @param w The width of the bounding rectangle.
 * @param h The height of the bounding rectangle.
 * @param z The elevation level for 3D effect.
 * @param up Boolean indicating if the ellipse appears raised.
 */
void
r_3d_ellipse(int x, int y, int w, int h, Elevation z, int up)
{
}

/**
 * Draw a straight line between two points.
 *
 * @param x1 The x-coordinate of the starting point.
 * @param y1 The y-coordinate of the starting point.
 * @param x2 The x-coordinate of the ending point.
 * @param y2 The y-coordinate of the ending point.
 */
void
r_line(int x1, int y1, int x2, int y2)
{ Translate(x1, y1);
  Translate(x2, y2);
  DEBUG(NAME_draw, Cprintf("r_line(%d, %d, %d, %d)\n",
			   x1, y1, x2, y2));

  cairo_new_path(CR);
  pce_cairo_set_source_color(CR, context.colour);
  cairo_set_line_width(CR, context.pen);
  cairo_move_to(CR, x1, y1);
  cairo_line_to(CR, x2, y2);
  cairo_stroke(CR);
}

/**
 * Draw a polygon defined by a series of points.  Used for
 * class `bezier` and `graphical->draw_poly`.
 *
 * @param pts An array of points defining the polygon.
 * @param n The number of points in the array.
 * @param close Boolean indicating whether to close the polygon.
 * @todo  Combine with r_fill_polygon().  This avoids creating the
 *        Cairo path twice.
 * @todo  Use Cairo's native Bezier curve support.
 */
void
r_polygon(IPoint pts, int n, int close)
{ if ( context.pen > 0 )
  { cairo_new_path(CR);
    bool first = true;
    for(int i=0; i<n; i++)
    { int x = pts[i].x;
      int y = pts[i].y;
      Translate(x,y);
      if ( first )
      { cairo_move_to(CR, x, y);
	first = false;
      } else
      { cairo_line_to(CR, x, y);
      }
    }
    if ( close )
      cairo_close_path(CR);

    pce_cairo_set_source_color(CR, context.colour);
    cairo_set_line_width(CR, context.pen);
    cairo_stroke(CR);
  }
}

/**
 * Draw a cubic Bezier curve using Cairo.
 * The coordinates are translated by the current drawing offset.
 *
 * @param start The starting point of the curve.
 * @param control1 The first control point for the cubic Bezier.
 * @param control2 The second control point for the cubic Bezier.
 * @param end The end point of the curve.
 */
void
r_bezier(ipoint start, ipoint control1, ipoint control2, ipoint end)
{ if ( context.pen > 0 )
  { cairo_new_path(CR);
    Translate(start.x, start.y);
    Translate(end.x, end.y);
    Translate(control1.x, control1.y);
    Translate(control2.x, control2.y);

    cairo_move_to(CR, start.x, start.y);
    cairo_curve_to(CR, control1.x, control1.y, control2.x, control2.y, end.x, end.y);

    pce_cairo_set_source_color(CR, context.colour);
    cairo_set_line_width(CR, context.pen);
    cairo_stroke(CR);
  }
}

/**
 * Draw a path defined by a chain of points.
 *
 * @param points The chain of points defining the path.
 * @param ox The x-offset for the path.
 * @param oy The y-offset for the path.
 * @param radius The corner radius for rounded corners.
 * @param closed Boolean indicating whether the path is closed.
 * @param fill The fill pattern or image.
 */
void
r_path(Chain points, int ox, int oy, int radius, int closed, Image fill)
{ if ( radius )
    Cprintf("stub: r_path(): radius ignored\n");

  cairo_new_path(CR);
  Cell cell;
  bool first = true;
  for_cell(cell, points)
  { Point p = cell->value;
    int x = valInt(p->x)+ox;
    int y = valInt(p->y)+oy;
    Translate(x,y);
    if ( first )
    { cairo_move_to(CR, x, y);
      first = false;
    } else
    { cairo_line_to(CR, x, y);
    }
  }
  if ( closed )
    cairo_close_path(CR);

  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_foreground);
    pce_cairo_set_source_color(CR, context.fill_pattern);
    cairo_fill_preserve(CR);
  }

  if ( context.pen )
  { pce_cairo_set_source_color(CR, context.colour);
    cairo_set_line_width(CR, context.pen);
    cairo_stroke(CR);
  }
}

/**
 * Perform an image operation with a specified operator.
 *
 * @param image The image to operate on.
 * @param sx The source x-coordinate.
 * @param sy The source y-coordinate.
 * @param x The destination x-coordinate.
 * @param y The destination y-coordinate.
 * @param w The width of the area to operate on.
 * @param h The height of the area to operate on.
 * @param op The operation to perform.
 */
void
r_op_image(Image image, int sx, int sy, int x, int y, int w, int h, Name op)
{
}

/**
 * Draw an image onto the display.
 *
 * @param image The image to draw.
 * @param sx The source x-coordinate.
 * @param sy The source y-coordinate.
 * @param x The destination x-coordinate.
 * @param y The destination y-coordinate.
 * @param w The width of the area to draw.
 * @param h The height of the area to draw.
 * @param transparent Boolean indicating whether to handle transparency.
 */
void
r_image(Image image, int sx, int sy,
	int x, int y, int w, int h, BoolObj transparent)
{ cairo_surface_t *surface = pceImage2CairoSurface(image);

  Translate(x, y);
  NormaliseArea(x, y, w, h);
  if ( w == 0 || h == 0 )
    return;

  DEBUG(NAME_draw,
	Cprintf("r_image(%s, %d, %d -> %d, %d, %d, %d, %s)\n",
		pp(image), sx, sy, x, y, w, h, pp(transparent)));

  if ( surface )
  { int width  = cairo_image_surface_get_width(surface);
    int height = cairo_image_surface_get_height(surface);
    if ( w == width && h == height )
    { cairo_new_path(CR);
      cairo_set_source_surface(CR, surface, x, y);
      cairo_paint(CR);
    } else
    { cairo_save(CR);
      cairo_translate(CR, x, y);
      cairo_scale(CR, (float)w/(float)width, (float)h/(float)height);
      cairo_set_source_surface(CR, surface, 0, 0);
      cairo_paint(CR);
      cairo_restore(CR);
    }
  }
}

static void
r_fill_fgbg(double x, double y, double w, double h, Any fill, Name which)
{ NormaliseArea(x, y, w, h);
  if ( w > 0 && h > 0 )
  { r_fillpattern(fill, which);
    DEBUG(NAME_draw,
	  Cprintf("r_fill(%.1f, %.1f, %.1f, %.1f, %s->%s)\n",
		  x, y, w, h, pp(fill), pp(context.fill_pattern)));

    Translate(x, y);

    if ( instanceOfObject(context.fill_pattern, ClassColour) )
    { pce_cairo_set_source_color(CR, context.fill_pattern);
      cairo_rectangle(CR, x, y, w, h);
      cairo_fill(CR);
    } else if ( isNil(context.fill_pattern) )
    { //Cprintf("r_fill(): Transparent\n");
      cairo_set_source_rgba(CR, 0, 0, 0, 0);
      cairo_rectangle(CR, x, y, w, h);
      cairo_fill(CR);
    } else
    { Cprintf("stub: r_fill(%s)\n", pp(context.fill_pattern));
    }
  }
}

/**
 * Clear a rectangular area on the screen.  This
 *
 * @param x The x-coordinate of the top-left corner of the rectangle.
 * @param y The y-coordinate of the top-left corner of the rectangle.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 */
void
r_clear(int x, int y, int w, int h)
{ r_fill_fgbg(x, y, w, h, NAME_background, NAME_background);
}

/**
 * Fill a rectangular area with a specified pattern.
 *
 * @param x The x-coordinate of the top-left corner.
 * @param y The y-coordinate of the top-left corner.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @param pattern The fill pattern or color.  If DEFAULT, use the
 *        current colour.
 */

void
r_fill(int x, int y, int w, int h, Any fill)
{ r_fill_fgbg(x, y, w, h, fill, NAME_foreground);
}


/**
 * Fill a polygon defined by a series of points.
 *
 * @param pts An array of points defining the polygon.
 * @param n The number of points in the array.
 */
void
r_fill_polygon(IPoint pts, int n)
{ if ( n <= 0 ) return;

  cairo_new_path(CR);

  // 2. Set background to transparent (optional)
  cairo_set_source_rgba(CR, 0, 0, 0, 0);
  cairo_paint(CR);

  pce_cairo_set_source_color(CR, context.fill_pattern);
  int x = pts[0].x;
  int y = pts[0].y;
  Translate(x, y);
  cairo_move_to(CR, x, y);
  for (int i = 1; i < n; i++)
  { int x = pts[i].x;
    int y = pts[i].y;
    Translate(x, y);
    cairo_line_to(CR, x, y);
  }
  cairo_close_path(CR);
  cairo_fill(CR);
}

/**
 * Draw a caret (text cursor) at the specified position.
 *
 * @param cx The x-coordinate of the caret.
 * @param cy The y-coordinate of the caret.
 * @param font The font used for the caret.
 */
void
r_caret(int cx, int cy, FontObj font)
{
}

/**
 * Fill a triangle defined by three points.
 *
 * @param x1 The x-coordinate of the first vertex.
 * @param y1 The y-coordinate of the first vertex.
 * @param x2 The x-coordinate of the second vertex.
 * @param y2 The y-coordinate of the second vertex.
 * @param x3 The x-coordinate of the third vertex.
 * @param y3 The y-coordinate of the third vertex.
 */
void
r_fill_triangle(int x1, int y1, int x2, int y2, int x3, int y3)
{ ipoint pts[3] =
    { {.x = x1, .y = y1 },
      {.x = x2, .y = y2 },
      {.x = x3, .y = y3 }
    };

  r_fill_polygon(pts, 3);
}

/**
 * Draw a triangle defined by three points with an optional fill.
 *
 * @param x1 The x-coordinate of the first vertex.
 * @param y1 The y-coordinate of the first vertex.
 * @param x2 The x-coordinate of the second vertex.
 * @param y2 The y-coordinate of the second vertex.
 * @param x3 The x-coordinate of the third vertex.
 * @param y3 The y-coordinate of the third vertex.
 * @param fill The fill pattern or color.
 */
void
r_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Any fill)
{
}

/**
 * Set the color of a specific pixel.
 *
 * @param x The x-coordinate of the pixel.
 * @param y The y-coordinate of the pixel.
 * @param val The color value to set.
 */
void
r_pixel(int x, int y, Any val)
{
}

/**
 * Invert the color of a specific pixel.
 *
 * @param x The x-coordinate of the pixel.
 * @param y The y-coordinate of the pixel.
 */
void
r_complement_pixel(int x, int y)
{
}

/**
 * Modify the current drawing context or state.
 */
void
d_modify(void)
{
}

/**
 * Retrieve the monochrome value of a specific pixel.
 *
 * @param x The x-coordinate of the pixel.
 * @param y The y-coordinate of the pixel.
 * @return The monochrome value of the pixel.
 */
int
r_get_mono_pixel(int x, int y)
{
    return 0;
}

/**
 * Retrieve the color value of a specific pixel.
 *
 * @param x The x-coordinate of the pixel.
 * @param y The y-coordinate of the pixel.
 * @return The color value of the pixel.
 */
unsigned long
r_get_pixel(int x, int y)
{ unsigned char *data = cairo_image_surface_get_data(context.target);
  int stride = cairo_image_surface_get_stride(context.target);
  cairo_format_t format = cairo_image_surface_get_format(context.target);

  assert(format == CAIRO_FORMAT_ARGB32);

  unsigned char *p = data + y * stride + x * 4;
  uint8_t b = p[0];
  uint8_t g = p[1];
  uint8_t r = p[2];
  uint8_t a = p[3];

  Cprintf("Pixel at %d,%d = %d %d %d %d\n",
	  x, y, r, g, b, a);

  return RGBA(r,g,b,a);
}

/**
 * Check if a font contains a specific character.
 *
 * @param f The font object.
 * @param c The character code to check.
 * @return Non-zero if the character is present; otherwise, zero.
 */
int
s_has_char(FontObj f, unsigned int c)
{ return c >=0 && c <= 0x10ffff;
}

/**
 * Retrieve the range of valid characters for a font.  We'll
 * assume modern fonts are Unicode.
 *
 * @param f The font object.
 * @param Either `x` or `y`.  What does this mean?
 * @param a Pointer to first valid character
 * @param z Pointer to last valid character
 */
void
f_domain(FontObj f, Name which, int *a, int *z)
{ *a = 0;
  *z = 0x10ffff;
}

/**
 * Retrieve the default character code for a font.
 *
 * @param font The font object.
 * @return The default character code.
 */
int
s_default_char(FontObj font)
{ return 'x';
}

/**
 * Retrieve the ascent (height above baseline) of a font.
 *
 * @param font The font object.
 * @return The ascent value.
 */
int
s_ascent(FontObj font)
{ WsFont wsf = ws_get_font(font);
  return wsf ? wsf->ascent : 0;
}

/**
 * Retrieve the descent (depth below baseline) of a font.
 *
 * @param font The font object.
 * @return The descent value.
 */
int
s_descent(FontObj font)
{ WsFont wsf = ws_get_font(font);
  return wsf ? wsf->descent : 0;
}

/**
 * Retrieve the line height for the font.  Note that older version sum
 * the ascent and descent.
 *
 * @param font The font object.
 * @return The total height.
 */
int
s_height(FontObj font)
{ WsFont wsf = ws_get_font(font);
  return wsf ? wsf->height : 0;
}

static
cairo_t *
ws_font_context(void)
{ if ( context.cr )
  { assert(context.open);
    return context.cr;
  }

  DisplayObj d = CurrentDisplay(NIL);
  WsDisplay wsd = d->ws_ref;
  return wsd->hidden_cairo;
}

/**
 * Retrieve the width of a specific character in a font.
 *
 * @param c The character code.
 * @param font The font object.
 * @return The width of the character.
 */
double
c_width(wint_t c, FontObj font)
{ cairo_text_extents_t extents;
  char s[2] = {c};
  cairo_t *cr = ws_font_context();
  cairo_save(cr);
  pce_cairo_set_font(cr, font);
  cairo_text_extents(cr, s, &extents);
  cairo_restore(cr);

  return extents.x_advance;
}

static void
s_extents_utf8(const char *u, size_t ulen, FontObj font,
	       cairo_text_extents_t *extents)
{ cairo_t *cr = ws_font_context();
  cairo_save(cr);
  pce_cairo_set_font(cr, font);
  if ( u[ulen] == 0 && strlen(u) == ulen )
  { cairo_text_extents(cr, u, extents);
  } else
  { char buf[1000];
    char *tmp;
    if ( ulen < 1000 )
      tmp = buf;
    else
      tmp = malloc(ulen+1);
    memcpy(tmp, u, ulen);
    tmp[ulen] = 0;
    assert(strlen(tmp) == ulen);	/* TODO: What if there are 0-bytes */
    cairo_text_extents(cr, tmp, extents);
    if ( tmp != buf )
      free(tmp);
  }
  cairo_restore(cr);
}


static void
s_extents(PceString s, int from, int to, FontObj font,
	  cairo_text_extents_t *extents)
{ string s2 = *s;
  if ( from > s2.s_size )
    from = s2.s_size;
  if ( to > s2.s_size )
    to = s2.s_size;
  if ( to <= from )
  { memset(extents, 0, sizeof(*extents));
    return;
  }

  if ( s2.s_iswide )
  { s2.s_textW += from;
  } else
  { s2.s_textA += from;
  }
  s2.s_size = to-from;

  size_t ulen;
  const char *u = stringToUTF8(&s2, &ulen);
  s_extents_utf8(u, ulen, font, extents);
}

/**
 * Calculate the width of a substring within a string using a specific font.
 *
 * @param s The string object.
 * @param from The starting index of the substring.
 * @param to The ending index of the substring.
 * @param font The font object.
 * @return The width of the substring.
 */
int
str_width(PceString s, int from, int to, FontObj font)
{ if ( to > from )
  { cairo_text_extents_t extents;

    s_extents(s, from, to, font, &extents);
    int w = (int)(extents.width+0.9);  /* round up */
    return w;
  } else
    return 0;
}

/**
 * Calculate the advance width (cursor movement) of a substring within
 * a string using a specific font.
 *
 * @param s The string object.
 * @param from The starting index of the substring.
 * @param to The ending index of the substring.
 * @param font The font object.
 * @return The advance width of the substring.
 */
int
str_advance(PceString s, int from, int to, FontObj font)
{ if ( to > from )
  { cairo_text_extents_t extents;

    s_extents(s, from, to, font, &extents);
    int w = (int)(extents.x_advance+0.5);
    return w;
  } else
    return 0;
}

int
str_advance_utf8(const char *u, int ulen, FontObj font)
{ if ( ulen > 0 )
  { cairo_text_extents_t extents;

    s_extents_utf8(u, ulen, font, &extents);
    return (int)(extents.x_advance+0.5);
  }

  return 0;
}

void
s_print_utf8(const char *u, size_t len, int x, int y, FontObj font)
{ DEBUG(NAME_draw,
	Cprintf("s_printU(\"%s\", %d, %d, %d, %s) (color: %s)\n",
		u, len, x, y, pp(font), pp(context.colour)));

  Translate(x, y);
  cairo_new_path(CR);
  pce_cairo_set_font(CR, font);
  pce_cairo_set_source_color(CR, context.colour);
  cairo_move_to(CR, x, y);
  if ( u[len] == 0 && strlen(u) == len )
  { cairo_show_text(CR, u);
  } else
  { char buf[1000];
    char *tmp;
    if ( len < 1000 )
      tmp = buf;
    else
      tmp = malloc(len+1);
    memcpy(tmp, u, len);
    tmp[len] = 0;
    if ( strlen(tmp) != len )
    { for(size_t i=0; i<len; i++)
	if ( !tmp[i] )
	  tmp[i] = 1;
    }
    cairo_show_text(CR, tmp);
    if ( tmp != buf )
      free(tmp);
  }
}

/**
 * Render a string of ISO-Latin-1 characters at a specified position
 * using a specific font.
 *
 * @param s The array of ASCII characters.
 * @param l The length of the character array.
 * @param x The x-coordinate for rendering.
 * @param y The y-coordinate for rendering.
 * @param f The font object.
 */
void
s_printA(charA *s, int l, int x, int y, FontObj font)
{ if ( l <= 0 )
    return;
  string str = { .text_union = { .textA = s },
                 .hdr.f = { .size = l,
			    .iswide = false,
			    .readonly = true
			  }
               };

  size_t ulen;
  const char *u = stringToUTF8(&str, &ulen);
  s_print_utf8(u, ulen, x, y, font);
}

/**
 * Render a string of wide characters  at a specified position using a
 * specific  font.   This   is  the  print  function   used  by  class
 * `text_image`.
 *
 * @param s The array of wide characters.
 * @param l The length of the character array.
 * @param x The x-coordinate for rendering.
 * @param y The y-coordinate for the baseline.
 * @param font The font object.
 */
void
s_printW(charW *s, int l, int x, int y, FontObj font)
{ if ( l <= 0 )
    return;
  string str = { .text_union = { .textW = s },
                 .hdr.f = { .size = l,
			    .iswide = true,
			    .readonly = true
			  }
               };

  size_t ulen;
  const char *u = stringToUTF8(&str, &ulen);
  s_print_utf8(u, ulen, x, y, font);
}

/**
 * Render a PceString at a specified position using a specific font.
 *
 * @param s The PceString object.
 * @param x The x-coordinate for rendering.
 * @param y The y-coordinate for rendering.
 * @param f The font object.
 */
void
s_print(PceString s, int x, int y, FontObj f)
{ if ( isstrA(s) )
    s_printA(s->s_textA, s->s_size, x, y, f);
  else
    s_printW(s->s_textW, s->s_size, x, y, f);
}

/**
 * Render a PceString at a specified position with alignment using a
 * specific font.
 *
 * @param s The PceString object.
 * @param x The x-coordinate for rendering.
 * @param y The y-coordinate for rendering.
 * @param f The font object.
 */
void
s_print_aligned(PceString s, int x, int y, FontObj f)
{ if ( s->s_size > 0 )
  { x += 0; //lbearing(str_fetch(s, 0));
    s_print(s, x, y, f);
  }
}

/**
 * Print part of a PceString at x,y
 */

static void
str_draw_text(FontObj font, PceString s, int offset, int len, int x, int y)
{ if ( offset >= s->s_size )
    return;

  if ( offset < 0 )
  { len += offset;
    offset = 0;
  }

  if ( offset + len > s->s_size )
    len = s->s_size - offset;

  if ( s->s_size > 0 )
  { InvTranslate(x, y);			/* Hack */

    if ( isstrA(s) )
    { s_printA(s->s_textA+offset, len, x, y, font);
    } else
    { s_printW(s->s_textW+offset, len, x, y, font);
    }
  }
}

static void
str_text(FontObj font, PceString s, int x, int y)
{ if ( s->s_size > 0 )
  { x += 0; //lbearing(str_fetch(s, 0));

    str_draw_text(font, s, 0, s->s_size, x, y);
  }
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

/**
 * Break a string into multiple lines.
 */

static void
str_break_into_lines(PceString s, strTextLine *line, int *nlines, int maxlines)
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

/**
 * Given a string  broken into lines, compute the  dimensions for each
 * of the lines given the target area and adjustment.
 */

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
    cy = y + (1 + h - nlines*th)/2;
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

/**
 * Calculate the rendered size of a PceString using the specified font.
 *
 * @param s Pointer to the PceString object.
 * @param font Pointer to the FontObj used for rendering.
 * @param width Pointer to an integer where the width will be stored.
 * @param height Pointer to an integer where the height will be stored.
 */
void
str_size(PceString s, FontObj font, int *width, int *height)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int w = 0;

  str_break_into_lines(s, lines, &nlines, MAX_TEXT_LINES);
  for(n = 0, line = lines; n++ < nlines; line++)
  { if ( line->text.s_size > 0 )
    { int lw = str_width(&line->text, 0, line->text.s_size, font);

      if ( w < lw )
	w = lw;
    }
  }

  *width  = w;
  *height = nlines * s_height(font);
}


/**
 * Draw a string within a specified area, applying horizontal and
 * vertical alignment.
 *
 * @param s Pointer to the PceString object to be drawn.
 * @param font Pointer to the FontObj specifying the font to use.
 * @param x The x-coordinate of the top-left corner of the drawing area.
 * @param y The y-coordinate of the top-left corner of the drawing area.
 * @param w The width of the drawing area.
 * @param h The height of the drawing area.
 * @param hadjust Name indicating horizontal alignment (e.g., left, center, right).
 * @param vadjust Name indicating vertical alignment (e.g., top, center, bottom).
 * @param flags Additional flags controlling rendering behavior.  Defined flags:
 *	  - TXT_UNDERLINED
 */
void
str_string(PceString s, FontObj font,
	   int x, int y, int w, int h,
	   Name hadjust, Name vadjust, int flags)
{ strTextLine lines[MAX_TEXT_LINES];
  strTextLine *line;
  int nlines, n;
  int baseline;

  if ( s->s_size == 0 )
    return;

  Translate(x, y);
  baseline = s_ascent(font);
  str_break_into_lines(s, lines, &nlines, MAX_TEXT_LINES);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);

  for(n=0, line = lines; n++ < nlines; line++)
  { str_text(font, &line->text, line->x, line->y+baseline);
    if ( flags & TXT_UNDERLINED )
    { cairo_new_path(CR);
      cairo_set_line_width(CR, UNDERLINE_PEN);
      double y = line->y+baseline+UNDERLINE_SEP;
      cairo_move_to(CR, line->x, y);
      cairo_line_to(CR, line->x+line->width, y);
      cairo_stroke(CR);
    }
  }
}

/**
 * Draw a selected substring within a specified area, applying styles and alignment.
 *
 * @param s Pointer to the PceString object containing the text.
 * @param font Pointer to the FontObj specifying the font to use.
 * @param f The starting index of the selection.
 * @param t The ending index of the selection.
 * @param style The Style to apply to the selected text.
 * @param x The x-coordinate of the top-left corner of the drawing area.
 * @param y The y-coordinate of the top-left corner of the drawing area.
 * @param w The width of the drawing area.
 * @param h The height of the drawing area.
 * @param hadjust Name indicating horizontal alignment.
 * @param vadjust Name indicating vertical alignment.
 */
void
str_selected_string(PceString s, FontObj font,
		    int f, int t, Style style,
		    int x, int y, int w, int h,
		    Name hadjust, Name vadjust)
{
}

/**
 * Render a string in PostScript format at a specified location.
 *
 * @param s Pointer to the PceString object to be rendered.
 * @param font Pointer to the FontObj specifying the font to use.
 * @param x The x-coordinate where the text begins.
 * @param y The y-coordinate where the text baseline is located.
 * @param w The maximum width for the text rendering.
 * @param format Name indicating the PostScript format to use.
 * @param flags Additional flags controlling rendering behavior.
 */
void
ps_string(PceString s, FontObj font,
	  int x, int y, int w,
	  Name format, int flags)
{
}

/**
 * Draws a string, just like str_string(), but underscores the first
 * character matching the accelerator (case-insensitive).
 */

static void
str_draw_text_lines(int acc, FontObj font,
		    int nlines, strTextLine *lines,
		    int ox, int oy)
{ strTextLine *line;
  int n;
  int baseline = s_ascent(font);

  for(n=0, line = lines; n++ < nlines; line++)
  { str_text(font, &line->text, line->x+ox, line->y+baseline+oy);

    if ( acc )
    { int cx = line->x+ox;
      int cn;

      cx += 0; // lbearing(str_fetch(&line->text, 0));

      for(cn=0; cn<line->text.s_size; cn++)
      { int c  = str_fetch(&line->text, cn);

	if ( (int)tolower(c) == acc )
	{ cx += str_advance(&line->text, 0, cn, font);
	  int cw = str_width(&line->text, cn, cn+1, font);
	  int cy = line->y+baseline+oy+UNDERLINE_SEP;

	  cairo_new_path(CR);
	  cairo_set_line_width(CR, UNDERLINE_PEN);
	  cairo_move_to(CR, cx, cy);
	  cairo_line_to(CR, cx+cw, cy);
	  cairo_stroke(CR);

	  acc = 0;
	  break;
	}
      }
    }
  }
}


/**
 * Draw a label with an optional accelerator key indicator within a
 * specified area.
 *
 * @param s Pointer to the PceString object representing the label text.
 * @param acc The index of the character to underline as the accelerator key.
 * @param font Pointer to the FontObj specifying the font to use.
 * @param x The x-coordinate of the top-left corner of the drawing area.
 * @param y The y-coordinate of the top-left corner of the drawing area.
 * @param w The width of the drawing area.
 * @param h The height of the drawing area.
 * @param hadjust Name indicating horizontal alignment.
 * @param vadjust Name indicating vertical alignment.
 * @param flags Additional flags controlling rendering behavior.
 */
void
str_label(PceString s, int acc, FontObj font,
	  int x, int y, int w, int h,
	  Name hadjust, Name vadjust, int flags)
{ strTextLine lines[MAX_TEXT_LINES];
  int nlines;

  if ( s->s_size == 0 )
    return;

  Translate(x, y);
  str_break_into_lines(s, lines, &nlines, MAX_TEXT_LINES);
  str_compute_lines(lines, nlines, font, x, y, w, h, hadjust, vadjust);

  if ( flags & LABEL_INACTIVE )
  { Colour old = context.colour; /* ignore "fixed_colours" */
    context.colour = WHITE_COLOUR;

    str_draw_text_lines(acc, font, nlines, lines, 1, 1);
    context.colour = ws_3d_grey();
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);
    context.colour = old;
  } else
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);
}
