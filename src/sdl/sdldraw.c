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
  int		offset_x;		/* Paint offset in X direction */
  int		offset_y;		/* Paint offset in Y direction */
  Any		colour;			/* Current colour */
  Any		background;		/* Background colour */
  Any		default_colour;
  Any		default_background;
  Any		fill_pattern;		/* Default for fill operations */
  int		pen;			/* Drawing thickness */
} sdl_draw_context;

#define X(x) ((x) + context.offset_x)
#define Y(y) ((y) + context.offset_y)
#define Translate(x, y)	 { (x) = X(x); (y) = Y(y); }
#define InvTranslate(x, y) { x -= context.offset_x; y -= context.offset_y; }

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

#if 0
static void
d_ensure_context(void)
{ if ( !context.open++ )
  { push_context();
    d_ensure_display();
    WsDisplay wsd = context.display->ws_ref;
    context.renderer = wsd->hidden_renderer;
  }
}
#endif

/**
 * Flush all pending drawing operations to the display.
 */
void
d_flush(void)
{
}

/**
 * Define a drawing window with specified dimensions and properties.
 *
 * @param sw Pointer to the PceWindow object.
 * @param x The x-coordinate of the window.
 * @param y The y-coordinate of the window.
 * @param w The width of the window.
 * @param h The height of the window.
 * @param clear Flag indicating whether to clear the window.
 * @param limit Flag indicating whether to limit drawing to the window bounds.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
d_window(PceWindow sw, int x, int y, int w, int h, int clear, int limit)
{ if ( !ws_created_window(sw) )
  { Cprintf("d_window(%s): not created\n");
    fail;
  }

  DEBUG(NAME_redraw,
	Cprintf("d_window(%s, %d, %d, %d, %d, %d, %d)\n",
		pp(sw), x, y, w, h, clear, limit));

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
  context.offset_x   = valInt(sw->scroll_offset->x);
  context.offset_y   = valInt(sw->scroll_offset->y);
  context.colour     = notDefault(sw->colour) ? sw->colour : d->foreground;
  context.background = sw->background;
  context.default_colour = context.colour;
  context.default_background = context.background;

  Translate(x, y);
  NormaliseArea(x, y, w, h);
  // SDL_Rect crect = {(float)x, (float)y, (float)w, (float)h};
  // SDL_SetRenderClipRect(context.renderer, &crect);

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
{ Cprintf("stub: d_image(%s)\n", pp(i));
  fail;
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
{
}

/**
 * Finalize the current drawing operation.
 */
void
d_done(void)
{ DEBUG(NAME_redraw, Cprintf("d_done(): open = %d\n", context.open));
  if ( --context.open == 0 )
  { if ( ctx_stacked )
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
{
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
cairo_set_source_color(cairo_t *cr, Colour pce)
{ SDL_Color c = pceColour2SDL_Color(pce);
  cairo_set_source_rgba(cr, c.r/256.0, c.g/256.0, c.b/256.0, c.a/256.0);
}

static void
cairo_set_font(cairo_t *cr, FontObj pce)
{ WsFont wsf = ws_get_font(pce);
  if ( wsf )
    cairo_set_scaled_font(cr, wsf->font);
}



		 /*******************************
		 *      DRAWING PRIMITIVES      *
		 *******************************/

/**
 * Clear a rectangular area on the screen.
 *
 * @param x The x-coordinate of the top-left corner of the rectangle.
 * @param y The y-coordinate of the top-left corner of the rectangle.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 */
void
r_clear(int x, int y, int w, int h)
{ NormaliseArea(x, y, w, h);
  Translate(x, y);

  r_fill(x, y, w, h, context.background);
}

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
void
r_thickness(int pen)
{ context.pen = pen;
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
void
r_dash(Name name)
{
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
{ if ( isDefault(fill) )
    fill = context.colour;
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
 * Apply foreground and background colors within a specific color context.
 *
 * @param fg The foreground color.
 * @param bg The background color.
 * @param ctx The color context to apply the colors to.
 */
void
r_fix_colours(Any fg, Any bg, ColourContext ctx)
{
}

/**
 * Restore the previous color context, undoing any temporary color changes.
 *
 * @param ctx The color context to restore.
 */
void
r_unfix_colours(ColourContext ctx)
{
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

  if ( notDefault(c) )
    context.default_colour = c;

  r_colour(context.default_colour);

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

  if ( isDefault(c) )
    return old;
  context.background = c;

  return old;
}

/**
 * Swap the foreground and background colors.
 */
void
r_swap_background_and_foreground(void)
{
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

  DEBUG(NAME_stub,
	Cprintf("r_box(%d, %d, %d, %d, %d, %s)\n",
		x, y, w, h, r, pp(fill)));
  int maxr = min(w, h)/2;

  r = min(r, maxr);
  if ( notNil(fill) && r == 0 )
  { r_fill(x, y, w, h, fill);
    fill = NIL;
  }

#if 0
  if ( context.pen )
  { SDL_FRect r = { x, y, w, h };
    sdl_set_draw_color(DEFAULT);
    for(int i=0; i<context.pen; i++)
    { SDL_RenderRect(context.renderer, &r);
      r.x++, r.y++, r.w-=2, r.h-=2;
    }
  }
#endif
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
  { Cprintf("r_shadow_box(%d, %d, %d, %d, %s)\n", x, y, w, h, pp(fill));
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

  DEBUG(NAME_stub,
	Cprintf("stub: r_3d_box(%d, %d, %d, %d, %d, %s, %d)\n",
		x, y, w, h, radius, pp(e), up));

  Translate(x, y)
  NormaliseArea(x, y, w, h);
  if ( radius > 0 )
  { int maxr = min(w,h)/2;

    if ( radius > maxr )
      radius = maxr;
  }

  if ( e->kind == NAME_shadow )
  { Cprintf("r_3d_box(): shadow\n");
  } else
  { bool fill = r_elevation_fillpattern(e, up);

    if ( !up  )
      shadow = -shadow;

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
#if 0
      } else
      { int r = x+w;
	int b = y+h;
	SDL_Color c = pceColour2SDL_Color(top_left_color);
	SDL_SetRenderDrawColor(context.renderer, c.r, c.g, c.b, c.a);
	for(int os=0; os<shadow; os++)
	{ SDL_FPoint pts[3] =
	    { { r-os, y-os }, { x+os, y+os }, { x+os, b-os } };
	  SDL_RenderLines(context.renderer, pts, 3);
	}
	c = pceColour2SDL_Color(bottom_right_color);
	SDL_SetRenderDrawColor(context.renderer, c.r, c.g, c.b, c.a);
	for(int os=0; os<shadow; os++)
	{ SDL_FPoint pts[3] =
	    { { r-os, y-os }, { r-os, b-os }, { x+os, b-os } };
	  SDL_RenderLines(context.renderer, pts, 3);
	}
#else
	(void)top_left_color;
	(void)bottom_right_color;
#endif
      }
    }

    if ( fill )
      r_fill(x+shadow, y+shadow, w-2*shadow, h-2*shadow, NAME_current);
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
{
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
 * @param map Additional mapping parameter for rendering.
 */
void
r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3,
	      Elevation e, int up, int map)
{
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
{
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
#if 0
  sdl_set_draw_color(DEFAULT);
  SDL_RenderLine(context.renderer, x1, y1, x2, y2);
#endif
}

/**
 * Draw a polygon defined by a series of points.
 *
 * @param pts An array of points defining the polygon.
 * @param n The number of points in the array.
 * @param close Boolean indicating whether to close the polygon.
 */
void
r_polygon(IPoint pts, int n, int close)
{
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
{
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
  DEBUG(NAME_draw,
	Cprintf("r_image(%s, %d, %d -> %d, %d, %d, %d, %s)\n",
		pp(image), sx, sy, x, y, w, h, pp(transparent)));

  if ( surface )
  { cairo_t *cr = cairo_create(context.target);
    cairo_set_source_surface(cr, surface, x, y);
    cairo_paint(cr);
    cairo_destroy(cr);
  }
}

/**
 * Fill a rectangular area with a specified pattern.
 *
 * @param x The x-coordinate of the top-left corner.
 * @param y The y-coordinate of the top-left corner.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @param pattern The fill pattern or color.  If DEFAULT, use the
 * current colour.
 */
void
r_fill(int x, int y, int w, int h, Any fill)
{ NormaliseArea(x, y, w, h);
  if ( w > 0 && h > 0 )
  { DEBUG(NAME_stub,
	  Cprintf("r_fill(%d, %d, %d, %d, %s)\n",
		  x, y, w, h, pp(fill)));

    r_fillpattern(fill, NAME_foreground);

    if ( instanceOfObject(context.fill_pattern, ClassColour) )
    { cairo_t *cr = cairo_create(context.target);
      cairo_set_source_color(cr, context.fill_pattern);
      cairo_rectangle(cr, x, y, w, h);
      cairo_fill(cr);
      cairo_destroy(cr);
    } else
    { Cprintf("stub: r_fill(%s)\n", pp(context.fill_pattern));
    }
  }
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
#if 0
  SDL_Rect bounds;

  polygon_bb(pts, n, &bounds);
  cairo_surface_t *surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
							bounds.w, bounds.h);
  cairo_t *cr = cairo_create(surface);

  // 2. Set background to transparent (optional)
  cairo_set_source_rgba(cr, 0, 0, 0, 0);
  cairo_paint(cr);

  cairo_set_source_color(cr, context.fill_pattern);
  cairo_move_to(cr, pts[0].x - bounds.x, pts[0].y - bounds.y);
  for (int i = 1; i < n; i++)
  { cairo_line_to(cr, pts[i].x - bounds.x, pts[i].y - bounds.y);
  }
  cairo_close_path(cr);
  cairo_fill(cr);
  cairo_destroy(cr);

  cairo_draw_surface(surface, bounds.x, bounds.y);
  cairo_surface_destroy(surface);
#endif
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
{
#if 0
  SDL_Color   c = pceColour2SDL_Color(context.fill_pattern);
  SDL_FColor fc = {.r = c.r/255.0f, .g = c.g/255.0f,
		   .b = c.b/255.0f, .a = c.a/255.0f };

  DEBUG(NAME_stub,
	Cprintf("stub: r_fill_triangle(%d, %d, %d, %d, %d, %d, %s)\n",
		x1, y1, x2, y2, x3, y3, pp(context.fill_pattern)));

  // Create 3 vertices with positions and uniform color
  SDL_Vertex verts[3] = {
    { .position = { (float)x1, (float)y1 }, .color = fc },
    { .position = { (float)x2, (float)y2 }, .color = fc },
    { .position = { (float)x3, (float)y3 }, .color = fc },
  };

  SDL_RenderGeometry(context.renderer, NULL, verts, 3, NULL, 0);
#endif
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
{
    return 0;
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

/**
 * Retrieve the width of a specific character in a font.
 *
 * @param c The character code.
 * @param font The font object.
 * @return The width of the character.
 */
int
c_width(wint_t c, FontObj font)
{ cairo_t *cr = cairo_create(context.target);
  cairo_set_font(cr, font);
  cairo_text_extents_t extents;
  char s[2] = {c};
  cairo_text_extents(cr, s, &extents);
  cairo_destroy(cr);

  return extents.width;
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
  { string s2 = *s;
    if ( s2.s_iswide )
    { s2.s_textW += from;
    } else
    { s2.s_textA += from;
    }
    s2.s_size = to-from;

    size_t ulen;
    const char *u = stringToUTF8(&s2, &ulen);

    cairo_t *cr = cairo_create(context.target);
    cairo_set_font(cr, font);
    cairo_text_extents_t extents;
    cairo_text_extents(cr, u, &extents);
    cairo_destroy(cr);

    return extents.width;
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
 * @param f The font object.
 * @return The advance width of the substring.
 */
int
str_advance(PceString s, int from, int to, FontObj f)
{ return str_width(s, from, to, f); /* for now */
}

static void
s_printU(const char *u, size_t len, int x, int y, FontObj font)
{ DEBUG(NAME_stub,
	Cprintf("s_printU(\"%s\", %d, %d, %d, %s) (color: %s)\n",
		u, len, x, y, pp(font), pp(context.colour)));

  Translate(x, y);
  cairo_t *cr = cairo_create(context.target);
  cairo_set_font(cr, font);
  cairo_set_source_color(cr, context.colour);
  cairo_move_to(cr, x, y);
  cairo_show_text(cr, u);
  cairo_destroy(cr);
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
  s_printU(u, ulen, x, y, font);
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
  s_printU(u, ulen, x, y, font);
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
    { int lw;

      lw = 0; //lbearing(str_fetch(&line->text, 0));
      lw += str_advance(&line->text, 0, line->text.s_size, font);

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
#if 0
    if ( flags & TXT_UNDERLINED )
      SDL_RenderLine(context.renderer,
		     line->x, line->y+baseline+1,
		     line->x+line->width, line->y+baseline+1);
#endif
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
	  int cw = str_advance(&line->text, cn, cn+1, font);
	  int cy = line->y+baseline+oy+2;

#if 0
	  sdl_set_draw_color(DEFAULT);
	  SDL_RenderLine(context.renderer, cx, cy, cx+cw-2, cy);
#else
	  (void)cw;
	  (void)cy;
#endif
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
  { Any old = r_colour(WHITE_COLOUR);

    str_draw_text_lines(acc, font, nlines, lines, 1, 1);
    r_colour(ws_3d_grey());
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);
    r_colour(old);
  } else
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);
}
