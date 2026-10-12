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
#ifdef CAIRO_HAS_PDF_SURFACE
#include <cairo/cairo-pdf.h>
#endif
#define _USE_MATH_DEFINES
#include <math.h>
#include "sdldraw.h"
#include "sdlcolour.h"
#include "sdlfont.h"
#include "sdlimage.h"
#include "sdldisplay.h"
#include "sdlframe.h"
#include "sdlwindow.h"

#define MAX_CTX_DEPTH (10)		/* Max draw context depth */

typedef enum
{ DCTX_WINDOW,
  DCTX_IMAGE,
  DCTX_PDF
} context_type;

typedef struct
{ int		open;			/* Allow for nested open */
  PceWindow	window;			/* Pce's notion of the window */
  FrameObj	frame;			/* Pce's frame of the window */
  DisplayObj	display;		/* Pce's display for the frame */
  context_type  type;			/* DCTX_* */
  cairo_surface_t *target;		/* Target for rendering to */
  cairo_t      *cr;			/* Cairo context */
  int		offset_x;		/* Paint offset in X direction */
  int		offset_y;		/* Paint offset in Y direction */
  SDL_Rect	clip_base;		/* Region we may paint (device px) */
  int		fixed_colours;		/* Colours are fixed */
  Any		colour;			/* Current colour */
  Any		background;		/* Background colour */
  Any		default_colour;
  Any		default_background;
  Any		fill;		/* Default for fill operations */
  int		fill_origin_x;		/* Origin of image fill patterns */
  int		fill_origin_y;
  Name		dash;			/* Named dash pattern in cr (cache) */
  Any		texture;		/* texture_name or stroke_style */
  Any		stroke_paint;		/* Paint from stroke_style or NIL */
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
static void pce_cairo_set_source_gradient(cairo_t *cr, Gradient g);
static void pce_cairo_set_source_fill(cairo_t *cr, Any fill);
#if 0
static bool validate_cairo_text_consistency(cairo_t *draw_cr);
#endif

static bool r_set_fill_fgbg(Any fill, Name which);
static void r_fill_fgbg(double x, double y, double w, double h,
			Any fill, Name which);


		 /*******************************
		 *        CONTEXT STACK         *
		 *******************************/

static sdl_draw_context	context;	/* current context */
static sdl_draw_context	ctx_stack[MAX_CTX_DEPTH];  /* Context stack */
static int		ctx_stacked;	/* Saved frames */

#include <gra/graphstate.c>		/* uses `context` */

static void
reset_context(void)
{ memset(&context, 0, sizeof(context));
  context.dash = NAME_none;		/* a new cairo_t has no dash */
  context.texture = NAME_none;
  context.stroke_paint = NIL;
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
  cairo_t *cr;

  if ( isNil(background) )		/* leave it transparent */
    return;
  cr = cairo_create(surf);
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
 * Apply a 2D affine transform on top of the current drawing context.
 *
 * The current integer pen offset is folded into the cairo transform as
 * a translation, the user-supplied matrix is then composed on top, and
 * the offset is reset to zero.  Subsequent r_* calls therefore deliver
 * coordinates directly as input to the matrix.
 *
 * Pair with r_pop_transform(saved) to undo.
 */
void
r_push_transform(Transform t, r_transform_save *saved)
{ saved->saved_offset_x = context.offset_x;
  saved->saved_offset_y = context.offset_y;

  cairo_save(CR);
  cairo_translate(CR,
		  (double)context.offset_x,
		  (double)context.offset_y);

  cairo_matrix_t m;
  m.xx = valNum(t->xx);
  m.yx = valNum(t->yx);
  m.xy = valNum(t->xy);
  m.yy = valNum(t->yy);
  m.x0 = valNum(t->tx);
  m.y0 = valNum(t->ty);
  cairo_transform(CR, &m);

  context.offset_x = 0;
  context.offset_y = 0;
}

void
r_pop_transform(r_transform_save *saved)
{ context.offset_x = saved->saved_offset_x;
  context.offset_y = saved->saved_offset_y;
  cairo_restore(CR);
}

/**
 * Begin an offscreen drawing group on the current cairo context.
 * Subsequent draws accumulate into the group instead of the target.
 * Pair with r_pop_group_with_alpha() to composite.
 */
void
r_push_group(void)
{ cairo_push_group(CR);
}

/**
 * Composite the topmost cairo group onto the underlying target with
 * the given alpha (0.0..1.0).  Values are clamped to that range.
 */
void
r_pop_group_with_alpha(double alpha)
{ if ( alpha < 0.0 ) alpha = 0.0;
  else if ( alpha > 1.0 ) alpha = 1.0;

  cairo_pop_group_to_source(CR);
  cairo_paint_with_alpha(CR, alpha);
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
{ state->x = context.fill_origin_x;
  state->y = context.fill_origin_y;

  if ( notNil(offset) )
  { context.fill_origin_x = x0 + valInt(offset->x);
    context.fill_origin_y = y0 + valInt(offset->y);
  }
}

/**
 * Restore the fill state from the provided state structure.
 *
 * @param state Pointer to the fill_state structure to be restored.
 */
void
r_fillrestore(fill_state *state)
{ context.fill_origin_x = state->x;
  context.fill_origin_y = state->y;
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

  /* A redraw re-entered from inside a redraw of the same window: the
   * inner one paints under the clip and the damage bookkeeping of the
   * outer one, which is the best we can do once we are here.  See
   * live_resize_watch() in sdlframe.c for how we got here.
   */
  if ( context.open && context.window == sw )
  { DEBUG(NAME_redraw,
	  Cprintf("d_window(%s): Context is already open\n", pp(sw)));
    context.open++;
    succeed;
  }

  push_context();
  context.open = 1;

  FrameObj  fr = getFrameWindow(sw, OFF);
  DisplayObj d = fr->display;
  WsWindow wsw = sw->ws_ref;

  context.type       = DCTX_WINDOW;
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
  cairo_scale(context.cr, wsw->scale, wsw->scale);

  { int cx = X(x), cy = Y(y), cw = w, ch = h;

    NormaliseArea(cx, cy, cw, ch);
    context.clip_base.x = (int)floor(cx*wsw->scale);
    context.clip_base.y = (int)floor(cy*wsw->scale);
    context.clip_base.w = (int)ceil ((cx+cw)*wsw->scale) - context.clip_base.x;
    context.clip_base.h = (int)ceil ((cy+ch)*wsw->scale) - context.clip_base.y;
    ws_dirty_window(sw,
		    context.clip_base.x, context.clip_base.y,
		    context.clip_base.w, context.clip_base.h);
  }

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
{ DisplayObj d = CurrentDisplay(NIL);
  Any colour, background;

  ws_open_image(i);
  if ( d )
  { colour     = d->foreground;
    background = d->background;
  } else	/* no display: an image is offscreen, so draw as d_pdf() does */
  { d          = NIL;
    colour     = BLACK_COLOUR;
    background = WHITE_COLOUR;
  }

  push_context();
  context.open = 1;

  context.type               = DCTX_IMAGE;
  context.display            = d;
  context.target             = i->ws_ref;
  context.cr                 = cairo_create(context.target);
  context.background         = background;
  context.colour             = colour;
  context.default_colour     = context.colour;
  context.default_background = context.background;

  succeed;
}

#ifdef CAIRO_HAS_PDF_SURFACE
status
d_pdf(const char *file, int w, int h, double scale)
{ push_context();
  context.open = 1;

  Colour background = WHITE_COLOUR;
  Colour colour     = BLACK_COLOUR;

  context.type               = DCTX_PDF;
  context.display            = NIL;
  context.target             = cairo_pdf_surface_create(file,
							w*scale, h*scale);
  context.cr                 = cairo_create(context.target);
  context.background         = background;
  context.colour             = colour;
  context.default_colour     = context.colour;
  context.default_background = context.background;

  cairo_scale(context.cr, scale, scale);

  succeed;
}
#else
#warning "No PDF backend in Cairo"
#endif

void
d_done_pdf(void)
{ cairo_show_page(context.cr);
  cairo_destroy(context.cr);
  cairo_surface_finish(context.target);
  cairo_surface_destroy(context.target);
}


/* Intersect the clipping region with <-clip_base, the area d_window()
 * was asked to repaint.  d_clip() and d_clip_done() both discard the
 * clipping region rather than restoring the enclosing one, so without
 * this a graphical that clips itself lifts the clip of the window it
 * is in and the ones painted after it may paint anywhere.  We must
 * know what a redraw changed to upload no more than that (see
 * ws_dirty_window()), so this has to hold.
 *
 * The base is in device pixels because the current transformation is
 * whatever the graphical being painted made of it.
 */

static void
d_clip_to_base(void)
{ if ( context.clip_base.w > 0 )
  { cairo_matrix_t m;

    cairo_get_matrix(CR, &m);
    cairo_identity_matrix(CR);
    cairo_rectangle(CR,
		    context.clip_base.x, context.clip_base.y,
		    context.clip_base.w, context.clip_base.h);
    cairo_clip(CR);
    cairo_set_matrix(CR, &m);
  }
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
  d_clip_to_base();
}

/**
 * Finalize the current drawing operation.
 */
void
d_done(void)
{ DEBUG(NAME_redraw, Cprintf("d_done(): open = %d\n", context.open));
  if ( --context.open == 0 )
  { if ( context.type == DCTX_PDF )
    { d_done_pdf();
    } else
    { cairo_destroy(context.cr);
    }
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
  d_clip_to_base();
}

/**
 * Compute the intersection of two IArea structures.
 *
 * @param a Pointer to the first IArea.
 * @param b Pointer to the second IArea.
 */
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

		 /*******************************
		 *         CAIRO UTILS          *
		 *******************************/

static void
pce_cairo_set_source_color(cairo_t *cr, Colour pce)
{ SDL_Color c = pceColour2SDL_Color(pce);
  cairo_set_source_rgba(cr, c.r/255.0, c.g/255.0, c.b/255.0, c.a/255.0);
}

/* Build a cairo pattern from an XPCE `gradient' object and set it as
 * the source on `cr'.  cairo_set_source() takes a reference to the
 * pattern, so we destroy our local one afterwards.
 */
/* Map the pattern's 0..1 space onto the extents of the current path,
 * which must be built before the source is set.  The pattern matrix
 * maps user space to pattern space.  Without a path (or an empty one)
 * the pattern stays in user space.
 */
static void
gradient_to_bounding_box(cairo_t *cr, cairo_pattern_t *pat)
{ double x1, y1, x2, y2;

  cairo_path_extents(cr, &x1, &y1, &x2, &y2);
  if ( x2 > x1 && y2 > y1 )
  { cairo_matrix_t m;

    cairo_matrix_init(&m, 1.0/(x2-x1), 0.0, 0.0, 1.0/(y2-y1),
		      -x1/(x2-x1), -y1/(y2-y1));
    cairo_pattern_set_matrix(pat, &m);
  }
}

static void
pce_cairo_set_source_gradient(cairo_t *cr, Gradient g)
{ cairo_pattern_t *pat;
  /* Translate the gradient's coordinates through the current draw-
   * context offset, the same shift shape-drawing macros apply so the
   * pattern lines up with the shape being filled.
   */
  double x0 = X(valNum(g->p0->x));
  double y0 = Y(valNum(g->p0->y));
  double x1 = X(valNum(g->p1->x));
  double y1 = Y(valNum(g->p1->y));

  if ( g->units == NAME_boundingBox )	/* not shifted: fractions */
  { x0 -= context.offset_x; y0 -= context.offset_y;
    x1 -= context.offset_x; y1 -= context.offset_y;
  }

  if ( g->kind == NAME_radial )
  { double r0 = notNil(g->r0) ? valNum(g->r0) : 0.0;
    double r1 = notNil(g->r1) ? valNum(g->r1) : 0.0;
    pat = cairo_pattern_create_radial(x0, y0, r0, x1, y1, r1);
  } else
  { pat = cairo_pattern_create_linear(x0, y0, x1, y1);
  }

  if ( notNil(g->stops) )
  { Cell cell;

    for_cell(cell, g->stops)
    { Tuple t = (Tuple)cell->value;

      if ( instanceOfObject(t, ClassTuple) &&
	   instanceOfObject(t->second, ClassColour) )
      { double frac = valNum(t->first);
	SDL_Color c = pceColour2SDL_Color((Colour)t->second);
	cairo_pattern_add_color_stop_rgba(pat, frac,
					  c.r/255.0, c.g/255.0,
					  c.b/255.0, c.a/255.0);
      }
    }
  }

  cairo_pattern_set_extend(pat,
			   g->extend == NAME_none    ? CAIRO_EXTEND_NONE :
			   g->extend == NAME_repeat  ? CAIRO_EXTEND_REPEAT :
			   g->extend == NAME_reflect ? CAIRO_EXTEND_REFLECT :
						       CAIRO_EXTEND_PAD);
  if ( g->units == NAME_boundingBox )
    gradient_to_bounding_box(cr, pat);
  cairo_set_source(cr, pat);
  cairo_pattern_destroy(pat);
}

/* Dispatch: set the source appropriate for whatever the fill slot
 * carries.  Colours go via pce_cairo_set_source_color; gradients via
 * pce_cairo_set_source_gradient.  Non-matching values fall through to
 * the colour path (which will error/default).
 */
static cairo_surface_t *recolor_bw_surface(cairo_surface_t* input,
					   Colour foreground,
					   Colour background,
					   bool dbg);

/* Tile the image, starting at the fill origin (see r_filloffset()).  A
 * bitmap is drawn in the current colours, as r_image() does.
 */
static void
pce_cairo_set_source_image(cairo_t *cr, Image image)
{ cairo_surface_t *surface = pceImage2CairoSurface(image);
  cairo_surface_t *copy = NULL;

  if ( !surface )
  { cairo_set_source_rgba(cr, 0, 0, 0, 0);
    return;
  }
  if ( image->kind == NAME_bitmap &&
       ( context.colour != BLACK_COLOUR ||
	 context.background != WHITE_COLOUR ) &&
       (copy = recolor_bw_surface(surface, context.colour,
				  context.background, false)) )
    surface = copy;

  cairo_pattern_t *pat = cairo_pattern_create_for_surface(surface);
  cairo_matrix_t m;

  cairo_pattern_set_extend(pat, CAIRO_EXTEND_REPEAT);
  cairo_matrix_init_translate(&m, -X(context.fill_origin_x),
				  -Y(context.fill_origin_y));
  cairo_pattern_set_matrix(pat, &m);
  cairo_set_source(cr, pat);
  cairo_pattern_destroy(pat);		/* the context holds a reference */
  if ( copy )
    cairo_surface_destroy(copy);	/* and the pattern to the surface */
}

static bool
is_paint(Any fill)
{ return ( instanceOfObject(fill, ClassColour) ||
	   instanceOfObject(fill, ClassGradient) ||
	   instanceOfObject(fill, ClassImage) );
}

static void
pce_cairo_set_source_fill(cairo_t *cr, Any fill)
{ if ( instanceOfObject(fill, ClassGradient) )
    pce_cairo_set_source_gradient(cr, (Gradient)fill);
  else if ( instanceOfObject(fill, ClassImage) )
    pce_cairo_set_source_image(cr, (Image)fill);
  else
    pce_cairo_set_source_color(cr, (Colour)fill);
}

/* The source for drawing lines: the paint of the stroke_style if there
 * is one, unless the colours are fixed (e.g., for an inactive graphical).
 */
static void
pce_cairo_set_stroke_source(cairo_t *cr)
{ if ( context.stroke_paint && notNil(context.stroke_paint) &&
       !context.fixed_colours )
    pce_cairo_set_source_fill(cr, context.stroke_paint);
  else
    pce_cairo_set_source_color(cr, context.colour);
}

static PangoLayout *
pce_cairo_set_font(cairo_t *cr, FontObj pce)
{ WsFont wsf = ws_get_font(pce);
  if ( wsf )
  { pango_cairo_update_layout(cr, wsf->layout);
    return wsf->layout;
  } else
  { Cprintf("stub: No font for %s\n", pp(pce));
    return NULL;
  }
}

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

static struct dashpattern *
named_dash(Name name)
{ for(struct dashpattern *dp = dash_patterns; dp->dash != 0; dp++)
  { if ( dp->dash == name )
      return dp;
  }

  errorPce(name, NAME_badTexture);
  return NULL;
}

static cairo_line_cap_t
stroke_cap(StrokeStyle s)
{ return ( s->cap == NAME_round  ? CAIRO_LINE_CAP_ROUND :
	   s->cap == NAME_square ? CAIRO_LINE_CAP_SQUARE :
				   CAIRO_LINE_CAP_BUTT );
}

static cairo_line_join_t
stroke_join(StrokeStyle s)
{ return ( s->join == NAME_round ? CAIRO_LINE_JOIN_ROUND :
	   s->join == NAME_bevel ? CAIRO_LINE_JOIN_BEVEL :
				   CAIRO_LINE_JOIN_MITER );
}

/* Set the Cairo stroke state, except for the line width, from a
 * texture_name or stroke_style.  Returns the paint of the style.
 */

static Any
cairo_set_texture(cairo_t *cr, Any texture)
{ if ( instanceOfObject(texture, ClassStrokeStyle) )
  { StrokeStyle s = texture;
    double offset = valNum(s->dash_offset);

    cairo_set_line_cap(cr, stroke_cap(s));
    cairo_set_line_join(cr, stroke_join(s));
    cairo_set_miter_limit(cr, valNum(s->miter_limit));

    if ( instanceOfObject(s->dash, ClassVector) )
    { Vector v = s->dash;
      int n = valInt(v->size);
      double *dashes = alloca(sizeof(double)*(n > 0 ? n : 1));

      for(int i=0; i<n; i++)
	dashes[i] = valNum(v->elements[i]);
      cairo_set_dash(cr, dashes, n, offset);
    } else
    { struct dashpattern *dp = NULL;

      if ( notNil(s->dash) && !(dp = named_dash(s->dash)) )
	dp = NULL;
      if ( dp )
	cairo_set_dash(cr, dp->dash_list, dp->dash_list_length, offset);
      else
	cairo_set_dash(cr, NULL, 0, 0.0);
    }

    return s->paint;
  } else
  { struct dashpattern *dp = named_dash(texture);

    cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT);
    cairo_set_line_join(cr, CAIRO_LINE_JOIN_MITER);
    cairo_set_miter_limit(cr, 10.0);
    if ( dp )
      cairo_set_dash(cr, dp->dash_list, dp->dash_list_length, 0);

    return NIL;
  }
}

/**
 * Set how lines are drawn from a texture_name or stroke_style.
 */
void
r_dash(Any texture)
{ context.texture = texture;

  if ( texture != context.dash )	/* the cache is for names only */
  { context.stroke_paint = cairo_set_texture(CR, texture);
    context.dash = (instanceOfObject(texture, ClassStrokeStyle)
			? NIL : texture);
  }
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
{ if ( context.fixed_colours )
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

  context.fill = fill;
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

Any
r_current_colour(void)
{ return context.colour;
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
			   double r, bool sub)
{ double x0 = x,     y0 = y;
  double x1 = x + w, y1 = y + h;

  if (r > w / 2) r = w / 2;
  if (r > h / 2) r = h / 2;

  if ( sub )
    cairo_new_sub_path(cr);
  else
    cairo_new_path(cr);
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
    my_cairo_rounded_rectangle(CR, fx, fy, fw, fh, r, true);
  else
    cairo_rectangle(CR, fx, fy, fw, fh);
  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_background);
    pce_cairo_set_source_fill(CR, context.fill);
    if ( context.pen )
      cairo_fill_preserve(CR);
    else
      cairo_fill(CR);
  }
  if ( context.pen )
  { pce_cairo_set_stroke_source(CR);
    cairo_stroke(CR);
  }
  cairo_restore(CR);
}

/**
 * Draw an anti-aliased box with rounded corners.  Unlike r_box(), which
 * draws crisp pixel-aligned edges, the coordinates and radius are real
 * and the corners are smooth.  Used for controls with a modern look.
 *
 * @param x The x-coordinate of the top-left corner.
 * @param y The y-coordinate of the top-left corner.
 * @param w The width of the rectangle.
 * @param h The height of the rectangle.
 * @param r The radius for rounded corners.
 * @param fill The fill colour or @nil.
 */
void
r_smooth_box(double x, double y, double w, double h, double r, Any fill)
{ Translate(x, y);
  double lw = (double)context.pen/2.0;	/* the line is centred on the */
  double fx = x+lw;			/* path, so this keeps the outline */
  double fy = y+lw;			/* inside the box and puts a 1px */
  double fw = w-2*lw;			/* line on pixel centres */
  double fh = h-2*lw;

  if ( fw <= 0 || fh <= 0 )
    return;

  cairo_new_path(CR);
  if ( r > 0 )
    my_cairo_rounded_rectangle(CR, fx, fy, fw, fh, r, false);
  else
    cairo_rectangle(CR, fx, fy, fw, fh);
  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_background);
    pce_cairo_set_source_fill(CR, context.fill);
    if ( context.pen )
      cairo_fill_preserve(CR);
    else
      cairo_fill(CR);
  }
  if ( context.pen )
  { cairo_set_line_width(CR, context.pen);
    pce_cairo_set_stroke_source(CR);
    cairo_stroke(CR);
  }
  cairo_new_path(CR);
}

/**
 * Draw an anti-aliased outline with rounded corners around what has
 * been drawn inside x,y,w,h.  The parts of the box outside the rounded
 * corners are painted in `outside`, so a rectangular background drawn
 * inside the box does not show there.  If `outside` is @default they are
 * made transparent, which shows what is behind a window that supports
 * transparency, such as a popup.  The outline is drawn in the current
 * colour and thickness.
 *
 * @param x The x-coordinate of the top-left corner.
 * @param y The y-coordinate of the top-left corner.
 * @param w The width of the box.
 * @param h The height of the box.
 * @param r The radius for rounded corners.
 * @param outside The colour outside the corners, @default (clear) or @nil.
 */
void
r_rounded_frame(double x, double y, double w, double h, double r,
		Any outside)
{ Translate(x, y);
  double lw = (double)context.pen/2.0;

  if ( w <= 0 || h <= 0 )
    return;

  if ( r > 0 && notNil(outside) )
  { cairo_save(CR);
    cairo_new_path(CR);
    cairo_set_fill_rule(CR, CAIRO_FILL_RULE_EVEN_ODD);
    cairo_rectangle(CR, x, y, w, h);
    my_cairo_rounded_rectangle(CR, x, y, w, h, r, true);
    if ( isDefault(outside) )
    { cairo_set_operator(CR, CAIRO_OPERATOR_CLEAR);
    } else
    { r_fillpattern(outside, NAME_background);
      pce_cairo_set_source_fill(CR, context.fill);
    }
    cairo_fill(CR);
    cairo_restore(CR);
  }

  if ( context.pen )
  { if ( r > 0 )
      my_cairo_rounded_rectangle(CR, x+lw, y+lw, w-2*lw, h-2*lw,
				 max(0, r-lw), false);
    else
    { cairo_new_path(CR);
      cairo_rectangle(CR, x+lw, y+lw, w-2*lw, h-2*lw);
    }
    cairo_set_line_width(CR, context.pen);
    pce_cairo_set_stroke_source(CR);
    cairo_stroke(CR);
  }
  cairo_new_path(CR);
}

/**
 * Make the rectangle ox,oy,ow,oh transparent, except for the box x,y,w,h
 * with corners rounded by `r' inside it.  Used for the margin of a
 * popup window that holds its drop shadow.
 */
void
r_clear_outside(double ox, double oy, double ow, double oh,
		double x, double y, double w, double h, double r)
{ Translate(ox, oy);
  Translate(x, y);

  cairo_save(CR);
  cairo_new_path(CR);
  cairo_set_fill_rule(CR, CAIRO_FILL_RULE_EVEN_ODD);
  cairo_rectangle(CR, ox, oy, ow, oh);
  if ( r > 0 )
    my_cairo_rounded_rectangle(CR, x, y, w, h, r, true);
  else
  { cairo_new_sub_path(CR);
    cairo_rectangle(CR, x, y, w, h);
  }
  cairo_set_operator(CR, CAIRO_OPERATOR_CLEAR);
  cairo_fill(CR);
  cairo_restore(CR);
}


/* One pass of a box blur of radius `r' over `n' values of `line', which
 * are `step' bytes apart, using `tmp' (n bytes) as scratch.
 */

static void
box_blur_line(unsigned char *line, int n, int step, int r, unsigned char *tmp)
{ int w = 2*r+1;
  int sum = 0;

  for(int i=0; i<n; i++)
    tmp[i] = line[(size_t)i*step];
  for(int i=-r; i<=r; i++)
    sum += (i >= 0 && i < n) ? tmp[i] : 0;
  for(int i=0; i<n; i++)
  { line[(size_t)i*step] = (unsigned char)((sum + w/2) / w);
    int out = i-r, in = i+r+1;
    sum -= (out >= 0 && out < n) ? tmp[out] : 0;
    sum += (in  >= 0 && in  < n) ? tmp[in]  : 0;
  }
}


/* Blur an A8 surface.  Three box blurs approximate a Gaussian blur with
 * standard deviation `sigma' (pixels).
 */

static void
blur_a8_surface(cairo_surface_t *surf, double sigma)
{ int w = cairo_image_surface_get_width(surf);
  int h = cairo_image_surface_get_height(surf);
  int stride = cairo_image_surface_get_stride(surf);
  unsigned char *data = cairo_image_surface_get_data(surf);
  int r = (int)((sqrt(4.0*sigma*sigma+1.0)-1.0)/2.0 + 0.5);
  unsigned char *tmp;

  if ( r < 1 || !(tmp = malloc(max(w, h))) )
    return;

  cairo_surface_flush(surf);
  for(int pass=0; pass<3; pass++)
  { for(int y=0; y<h; y++)
      box_blur_line(data + (size_t)y*stride, w, 1, r, tmp);
    for(int x=0; x<w; x++)
      box_blur_line(data + x, h, stride, r, tmp);
  }
  cairo_surface_mark_dirty(surf);
  free(tmp);
}


static void
shadow_shape_path(cairo_t *cr, Name shape,
		  double x, double y, double w, double h, double r)
{ if ( shape == NAME_ellipse )
  { cairo_new_path(cr);
    cairo_save(cr);
    cairo_translate(cr, x+w/2.0, y+h/2.0);
    cairo_scale(cr, w/2.0, h/2.0);
    cairo_arc(cr, 0, 0, 1, 0, 2*M_PI);
    cairo_restore(cr);
  } else if ( r > 0 )
  { my_cairo_rounded_rectangle(cr, x, y, w, h, r, false);
  } else
  { cairo_new_path(cr);
    cairo_rectangle(cr, x, y, w, h);
  }
}


/**
 * Draw the drop shadow `s' of a shape, as CSS `box-shadow' does.  The
 * shape is a box (with corner radius `r') or an ellipse in x,y,w,h.  The
 * shadow is the shape moved by the offset of `s', grown by its spread
 * and blurred over its blur.  It is not drawn under the shape itself,
 * so it shows correctly for a shape without (opaque) fill.  Call this
 * before drawing the shape.
 *
 * @param s The shadow object.
 * @param shape NAME_box or NAME_ellipse.
 */
void
r_drop_shadow(Shadow s, Name shape,
	      double x, double y, double w, double h, double r)
{ Translate(x, y);
  double sp = valInt(s->spread);
  double blur = valInt(s->blur);
  double sx = x + valInt(s->x_offset) - sp;
  double sy = y + valInt(s->y_offset) - sp;
  double sw = w + 2*sp;
  double sh = h + 2*sp;
  double sr = (r > 0 ? max(0, r+sp) : 0);
  double ux = 1.0, uy = 0.0;

  if ( sw <= 0 || sh <= 0 || w <= 0 || h <= 0 )
    return;

  cairo_save(CR);
  cairo_new_path(CR);			/* everything but the shape */
  cairo_set_fill_rule(CR, CAIRO_FILL_RULE_EVEN_ODD);
  cairo_rectangle(CR, sx-blur-1, sy-blur-1, sw+2*blur+2, sh+2*blur+2);
  if ( shape == NAME_ellipse )
  { cairo_new_sub_path(CR);
    cairo_save(CR);
    cairo_translate(CR, x+w/2.0, y+h/2.0);
    cairo_scale(CR, w/2.0, h/2.0);
    cairo_arc(CR, 0, 0, 1, 0, 2*M_PI);
    cairo_restore(CR);
  } else if ( r > 0 )
  { my_cairo_rounded_rectangle(CR, x, y, w, h, r, true);
  } else
  { cairo_new_sub_path(CR);
    cairo_rectangle(CR, x, y, w, h);
  }
  cairo_clip(CR);
  cairo_set_fill_rule(CR, CAIRO_FILL_RULE_WINDING);
  pce_cairo_set_source_color(CR, s->colour);

  cairo_user_to_device_distance(CR, &ux, &uy);
  double scale = sqrt(ux*ux + uy*uy);
  int pw = (int)ceil((sw+2*blur)*scale);
  int ph = (int)ceil((sh+2*blur)*scale);

  if ( blur <= 0 || scale <= 0 || (double)pw*ph > 16e6 )
  { shadow_shape_path(CR, shape, sx, sy, sw, sh, sr);
    cairo_fill(CR);
  } else
  { cairo_surface_t *mask = cairo_image_surface_create(CAIRO_FORMAT_A8,
							pw, ph);
    cairo_t *mcr = cairo_create(mask);

    cairo_scale(mcr, scale, scale);
    cairo_set_source_rgba(mcr, 0, 0, 0, 1);
    shadow_shape_path(mcr, shape, blur, blur, sw, sh, sr);
    cairo_fill(mcr);
    cairo_destroy(mcr);

    blur_a8_surface(mask, blur*scale/2.0);	/* CSS: blur is 2 sigma */
    cairo_surface_set_device_scale(mask, scale, scale);
    cairo_mask_surface(CR, mask, sx-blur, sy-blur);
    cairo_surface_destroy(mask);
  }

  cairo_restore(CR);
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
{ Any colour;

  if ( light )
    colour = r_elevation_relief(e);
  else
    colour = r_elevation_shadow(e);
  r_thickness(1);

  DEBUG(NAME_draw, Cprintf("Drawing %d segments with %s (pen=%.1f)\n", n, pp(colour), context.pen));

  pce_cairo_set_source_color(CR, colour);
  cairo_set_line_width(CR, context.pen);

  ISegment p = s;
  ISegment end = &s[n];
  while(p<end)
  { cairo_new_path(CR);
    cairo_move_to(CR, X(p->x1), Y(p->y1));
    cairo_line_to(CR, X(p->x2), Y(p->y2));
    while ( p < end && p[1].x1 == p->x2 && p[1].y1 == p->y2 )
    { p++;
      cairo_line_to(CR, X(p->x2), Y(p->y2));
    }
    cairo_stroke(CR);
    p++;
  }
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
r_3d_box(double x, double y, double w, double h,
	 double radius, Elevation e, bool up0)
{ double shadow = valNum(e->height);
  bool up = up0;

  if ( shadow < 0.0 )
  { shadow = -shadow;
    up = !up;
  }
  radius = max(0.0,radius);

  DEBUG(NAME_draw,
	Cprintf("r_3d_box(%f, %f, %f, %f, %f, %s, %d)\n",
		x, y, w, h, radius, pp(e), up));

  NormaliseArea(x, y, w, h);
  if ( w < 0.1 || h < 0.1 )
    return;

  radius = min(radius, min(w,h)/2);

  if ( e->kind == NAME_shadow )
  { shadow = min(shadow, min(w, h));
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
  { bool fill = r_elevation_fillpattern(e, up0);

    if ( shadow != 0.0 )
    { Colour top_left_color;
      Colour bottom_right_color;

      if ( up )
      { top_left_color     = r_elevation_relief(e);
	bottom_right_color = r_elevation_shadow(e);
      } else
      { top_left_color     = r_elevation_shadow(e);
	bottom_right_color = r_elevation_relief(e);
      }

      if ( shadow > MAX_SHADOW )
	shadow = MAX_SHADOW;

      Translate(x, y);
      if ( radius > 0 )			/* with rounded corners */
      {	pce_cairo_set_source_color(CR, top_left_color);
	cairo_new_path(CR);
	cairo_move_to(CR, x, y);
	cairo_line_to(CR, x+w, y);
	cairo_line_to(CR, x, y+h);
	cairo_close_path(CR);
	cairo_clip(CR);

	my_cairo_rounded_rectangle(CR, x, y, w, h, radius, false);
	cairo_fill(CR);
	cairo_reset_clip(CR);

	pce_cairo_set_source_color(CR, bottom_right_color);
	cairo_new_path(CR);
	cairo_move_to(CR, x+w, y);
	cairo_line_to(CR, x+w, y+h);
	cairo_line_to(CR, x, y+h);
	cairo_close_path(CR);
	cairo_clip(CR);

	my_cairo_rounded_rectangle(CR, x, y, w, h, radius, false);
	cairo_fill(CR);
	cairo_reset_clip(CR);

	// Fill base
	{ double ix = x+shadow, iy = y+shadow, iw = w-2*shadow, ih = h-2*shadow;
	  double ir = radius * ((w+h)-2*shadow)/(w+h);

	  my_cairo_rounded_rectangle(CR, ix, iy, iw, ih, ir, true);
	  if ( r_set_fill_fgbg(NAME_current, NAME_background) )
	    cairo_fill(CR);
	  else
	    cairo_new_path(CR);
	}
      } else
      { pce_cairo_set_source_color(CR, top_left_color);
	// Top (light)
	cairo_move_to(CR, x, y);
	cairo_line_to(CR, x + w, y);
	cairo_line_to(CR, x + w - shadow, y + shadow);
	cairo_line_to(CR, x + shadow, y + shadow);
	cairo_close_path(CR);
	cairo_fill(CR);
	// Left (light)
	cairo_move_to(CR, x, y);
	cairo_line_to(CR, x, y + h);
	cairo_line_to(CR, x + shadow, y + h - shadow);
	cairo_line_to(CR, x + shadow, y + shadow);
	cairo_close_path(CR);
	cairo_fill(CR);
        // Bottom (dark)
	pce_cairo_set_source_color(CR, bottom_right_color);
	cairo_move_to(CR, x, y + h);
	cairo_line_to(CR, x + w, y + h);
	cairo_line_to(CR, x + w - shadow, y + h - shadow);
	cairo_line_to(CR, x + shadow, y + h - shadow);
	cairo_close_path(CR);
	cairo_fill(CR);
	// Right (dark)
	cairo_move_to(CR, x + w, y);
	cairo_line_to(CR, x + w, y + h);
	cairo_line_to(CR, x + w - shadow, y + h - shadow);
	cairo_line_to(CR, x + w - shadow, y + shadow);
	cairo_close_path(CR);
	cairo_fill(CR);
      }
      InvTranslate(x,y);
    }

    if ( fill && radius == 0.0 ) /* r_fill_fgbg() uses floats  */
      r_fill_fgbg(x+shadow, y+shadow, w-2*shadow,
		  h-2*shadow, NAME_current, NAME_background);
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
r_3d_line(int x1, int y1, int x2, int y2, Elevation e, bool up)
{ double z = valNum(e->height);
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
/* Find the intersection point of two lines, each given as point + direction. */
static inline void
line_intersect(double px, double py, double pdx, double pdy,
	       double qx, double qy, double qdx, double qdy,
	       double *rx, double *ry)
{ double denom = pdx*qdy - pdy*qdx;

  if ( fabs(denom) > 1e-10 )
  { double t = ((qx-px)*qdy - (qy-py)*qdx) / denom;
    *rx = px + t*pdx;
    *ry = py + t*pdy;
  } else			/* parallel — fall back to midpoint */
  { *rx = (px+qx)/2.0;
    *ry = (py+qy)/2.0;
  }
}

void
r_3d_triangle(double x1, double y1, double x2, double y2, double x3, double y3,
	      Elevation e, bool up, unsigned int map)
{ double shadow = valNum(e->height);

  DEBUG(NAME_draw,
	Cprintf("r_3d_triangle(%1f,%1f, %1f,%1f, %1f,%1f %s, %d)\n",
		x1,y1, x2,y2, x3,y3, pp(e), up));

  if ( shadow < 0.0 )
  { shadow = -shadow;
    up = !up;
  }

  double ix1 = x1, iy1 = y1;
  double ix2 = x2, iy2 = y2;
  double ix3 = x3, iy3 = y3;

  if ( shadow > 0.1 )
  { Colour up_color, down_color;

    if ( up )
    { up_color   = r_elevation_relief(e);
      down_color = r_elevation_shadow(e);
    } else
    { up_color   = r_elevation_shadow(e);
      down_color = r_elevation_relief(e);
    }

    /* Compute the inner (inset) triangle such that every edge is exactly
     * shadow pixels wide (perpendicular distance).  For each edge compute
     * its inward unit normal, offset the edge line by shadow along that
     * normal, then intersect adjacent offset lines to get inner vertices.
     */
    // Edge directions
    double d1x = x2-x1, d1y = y2-y1, l1 = sqrt(d1x*d1x + d1y*d1y);
    double d2x = x3-x2, d2y = y3-y2, l2 = sqrt(d2x*d2x + d2y*d2y);
    double d3x = x1-x3, d3y = y1-y3, l3 = sqrt(d3x*d3x + d3y*d3y);
    // Inward unit normals (rotate edge 90°, then flip if pointing outward)
    double cx = (x1+x2+x3)/3.0, cy = (y1+y2+y3)/3.0;
    double n1x = -d1y/l1, n1y =  d1x/l1;
    if ( (cx-(x1+x2)/2)*n1x + (cy-(y1+y2)/2)*n1y < 0 ) { n1x=-n1x; n1y=-n1y; }
    double n2x = -d2y/l2, n2y =  d2x/l2;
    if ( (cx-(x2+x3)/2)*n2x + (cy-(y2+y3)/2)*n2y < 0 ) { n2x=-n2x; n2y=-n2y; }
    double n3x = -d3y/l3, n3y =  d3x/l3;
    if ( (cx-(x3+x1)/2)*n3x + (cy-(y3+y1)/2)*n3y < 0 ) { n3x=-n3x; n3y=-n3y; }
    // Inner vertices: intersect pairs of adjacent offset edge lines
    line_intersect(x3+shadow*n3x, y3+shadow*n3y, d3x, d3y,
		   x1+shadow*n1x, y1+shadow*n1y, d1x, d1y, &ix1, &iy1);
    line_intersect(x1+shadow*n1x, y1+shadow*n1y, d1x, d1y,
		   x2+shadow*n2x, y2+shadow*n2y, d2x, d2y, &ix2, &iy2);
    line_intersect(x2+shadow*n2x, y2+shadow*n2y, d2x, d2y,
		   x3+shadow*n3x, y3+shadow*n3y, d3x, d3y, &ix3, &iy3);

    /* Draw each edge as a filled quadrilateral with the correct colour.
     * Adjacent quads share the edge outer_Vn → inner_Vn so they connect
     * without gaps regardless of which colour each edge has.
     * map bit 0: edge (x1,y1)→(x2,y2); bit 1: (x2,y2)→(x3,y3);
     * bit 2: (x3,y3)→(x1,y1).  Set bit = up_color, clear bit = down_color.
     */
    pce_cairo_set_source_color(CR, (map & 0x1) ? up_color : down_color);
    cairo_move_to(CR, X(x1), Y(y1));
    cairo_line_to(CR, X(x2), Y(y2));
    cairo_line_to(CR, X(ix2), Y(iy2));
    cairo_line_to(CR, X(ix1), Y(iy1));
    cairo_close_path(CR);
    cairo_fill(CR);

    pce_cairo_set_source_color(CR, (map & 0x2) ? up_color : down_color);
    cairo_move_to(CR, X(x2), Y(y2));
    cairo_line_to(CR, X(x3), Y(y3));
    cairo_line_to(CR, X(ix3), Y(iy3));
    cairo_line_to(CR, X(ix2), Y(iy2));
    cairo_close_path(CR);
    cairo_fill(CR);

    pce_cairo_set_source_color(CR, (map & 0x4) ? up_color : down_color);
    cairo_move_to(CR, X(x3), Y(y3));
    cairo_line_to(CR, X(x1), Y(y1));
    cairo_line_to(CR, X(ix1), Y(iy1));
    cairo_line_to(CR, X(ix3), Y(iy3));
    cairo_close_path(CR);
    cairo_fill(CR);
  }

  if ( r_elevation_fillpattern(e, up) )
    r_fill_triangle(ix1, iy1, ix2, iy2, ix3, iy3);
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
 * Draw an arc within a specified rectangle in the clockwise direction.
 *
 * @param x The x-coordinate of the top-left corner of the bounding rectangle.
 * @param y The y-coordinate of the top-left corner of the bounding rectangle.
 * @param w The width of the bounding rectangle.
 * @param h The height of the bounding rectangle.
 * @param s The starting angle in degrees (xpce convention: positive = CCW
 *	    on screen, 0 = +x axis).
 * @param sz The angular size in degrees (signed; positive = CCW on screen).
 * @param close {none, chord, pie_slice}
 * @param fill The fill pattern or color.
 */
void
r_arc(double x, double y, double w, double h,
      double s, double sz, Name close, Any fill)
{ Translate(x, y);
  if ( w < 0 ) { x += w; w = -w; }
  if ( h < 0 ) { y += h; h = -h; }
  FloatArea(x, y, w, h);
  /* xpce angles are CCW from +x with y-up; cairo angles are CW on screen
     because cairo's y points down.  Negate to convert. */
  double fs = -s * M_PI / 180.0;
  double fe = -(s + sz) * M_PI / 180.0;

  cairo_new_path(CR);
  cairo_save(CR);
  cairo_translate(CR, fx + fw / 2.0, fy + fh / 2.0);  // Move to center
  cairo_scale(CR, fw / 2.0, fh / 2.0);              // Scale unit circle
  if ( close == NAME_pieSlice )
    cairo_move_to(CR, 0, 0);
  if ( sz >= 0 )
    cairo_arc_negative(CR, 0, 0, 1.0, fs, fe);
  else
    cairo_arc(CR, 0, 0, 1.0, fs, fe);
  cairo_restore(CR);
  if ( close == NAME_pieSlice || close == NAME_chord )
  { cairo_close_path(CR);
  }
  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_foreground);
    pce_cairo_set_source_fill(CR, context.fill);
    if ( context.pen )
      cairo_fill_preserve(CR);
    else
      cairo_fill(CR);
  }
  if ( context.pen )
  { cairo_set_line_width(CR, context.pen);
    pce_cairo_set_stroke_source(CR);
    cairo_stroke(CR);
  }
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
{ r_arc(x, y, w, h, 0, 360, NAME_none, fill);
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
r_line(double x1, double y1, double x2, double y2)
{ Translate(x1, y1);
  Translate(x2, y2);
  DEBUG(NAME_draw, Cprintf("r_line(%d, %d, %d, %d)\n",
			   x1, y1, x2, y2));

  cairo_new_path(CR);
  pce_cairo_set_stroke_source(CR);
  cairo_set_line_width(CR, context.pen);
  cairo_move_to(CR, x1, y1);
  cairo_line_to(CR, x2, y2);
  cairo_stroke(CR);
}

/**
 * Draw a text underline.
 *  param underline is either a colour or DEFAULT or the boolean OFF.
 *  If DEFAULT, we paint using the current colour
 */

void
r_underline(FontObj font, double x, double base, double w,
	    Any underline, Name texture)
{ if ( underline != OFF )
  { WsFont wsf = ws_get_font(font);
    Any oldc = NULL;
    if ( instanceOfObject(underline, ClassColour) )
      oldc = r_colour(underline);
    double o_pen = r_thickness(wsf->ul_thickness);
    r_dash(isDefault(texture) || !texture ? NAME_none : texture);
    double uly = base + wsf->ul_thickness/2.0 - wsf->ul_position;
    r_line(x, uly, x+w, uly);
    r_thickness(o_pen);
    r_dash(NAME_none);
    if ( oldc )
      r_colour(oldc);
  }
}

/* Draw a strikethrough line for the run `[x, x+w)` whose text was
 * placed on baseline `base`.  `strike` is the same Bool|Colour idiom as
 * `r_underline`'s underline arg; `texture` picks a dash pattern from
 * the line-texture vocabulary.
 */
void
r_strikethrough(FontObj font, double x, double base, double w,
		Any strike, Name texture)
{ if ( strike != OFF )
  { WsFont wsf = ws_get_font(font);
    Any oldc = NULL;
    if ( instanceOfObject(strike, ClassColour) )
      oldc = r_colour(strike);
    double o_pen = r_thickness(wsf->st_thickness);
    r_dash(isDefault(texture) || !texture ? NAME_none : texture);
    double sty = base - wsf->st_position + wsf->st_thickness/2.0;
    r_line(x, sty, x+w, sty);
    r_thickness(o_pen);
    r_dash(NAME_none);
    if ( oldc )
      r_colour(oldc);
  }
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
r_polygon(FPoint pts, int n, int close)
{ if ( context.pen > 0 )
  { cairo_new_path(CR);
    bool first = true;
    for(int i=0; i<n; i++)
    { double x = pts[i].x;
      double y = pts[i].y;
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

    pce_cairo_set_stroke_source(CR);
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
r_bezier(fpoint start, fpoint control1, fpoint control2, fpoint end)
{ if ( context.pen > 0 )
  { cairo_new_path(CR);
    Translate(start.x, start.y);
    Translate(end.x, end.y);
    Translate(control1.x, control1.y);
    Translate(control2.x, control2.y);

    cairo_move_to(CR, start.x, start.y);
    cairo_curve_to(CR, control1.x, control1.y, control2.x, control2.y, end.x, end.y);

    pce_cairo_set_stroke_source(CR);
    cairo_set_line_width(CR, context.pen);
    cairo_stroke(CR);
  }
}

		 /*******************************
		 *	       PATHS		*
		 *******************************/

/* A path is a chain of points.  The class of each point says how the
 * path gets there from the previous one (see gra/pathseg.c).  The
 * points are first turned into an array of nodes so that rounding a
 * corner can look at both neighbours.
 */

typedef enum
{ PN_MOVE,				/* start a sub-path */
  PN_LINE,				/* straight line */
  PN_CURVE,				/* Bezier curve (curve_point) */
  PN_ARC				/* elliptical arc (arc_point) */
} path_node_kind;

typedef struct path_node
{ path_node_kind kind;
  double	 x, y;
  Point		 point;			/* the point it comes from */
  bool		 closed;		/* PN_MOVE: close the sub-path */
} path_node;

typedef struct path_corner
{ double	 t1x, t1y;		/* where the arc starts */
  double	 t2x, t2y;		/* where the arc ends */
  double	 cx, cy, r;		/* the circle */
  double	 a1, a2;		/* start and end angle */
  bool		 negative;		/* draw with cairo_arc_negative() */
} path_corner;

static int
path_nodes(Chain points, bool closed, double dx, double dy, path_node *nodes)
{ int n = 0;
  Cell cell;

  for_cell(cell, points)
  { Point pt = cell->value;
    path_node *nd = &nodes[n];

    nd->x = valNum(pt->x) + dx;
    nd->y = valNum(pt->y) + dy;
    nd->point = pt;
    nd->closed = closed;

    if ( n == 0 || instanceOfObject(pt, ClassMovePoint) )
    { nd->kind = PN_MOVE;
      if ( instanceOfObject(pt, ClassMovePoint) )
      { MovePoint mp = (MovePoint)pt;

	if ( notNil(mp->closed) )
	  nd->closed = (mp->closed == ON);
      }
    } else if ( instanceOfObject(pt, ClassCurvePoint) )
      nd->kind = PN_CURVE;
    else if ( instanceOfObject(pt, ClassArcPoint) )
      nd->kind = PN_ARC;
    else
      nd->kind = PN_LINE;

    n++;
  }

  return n;
}


/* Round the corner at P between the straight edges from A and to B
 * with a circle of the given radius.  The radius is reduced if the
 * edges are too short, such that each corner uses at most half of an
 * edge.  Fails if there is no corner (the edges are in line).
 */

static bool
path_round_corner(double ax, double ay, double px, double py,
		  double bx, double by, double radius, path_corner *c)
{ double ux = ax-px, uy = ay-py;
  double vx = bx-px, vy = by-py;
  double lu = hypot(ux, uy);
  double lv = hypot(vx, vy);

  if ( lu < 1e-9 || lv < 1e-9 )
    return false;
  ux /= lu; uy /= lu;
  vx /= lv; vy /= lv;

  double cosa = ux*vx + uy*vy;
  if ( cosa > 1.0 ) cosa = 1.0;
  if ( cosa < -1.0 ) cosa = -1.0;
  double theta = acos(cosa);		/* angle between the edges */
  if ( theta < 1e-6 || M_PI-theta < 1e-6 )
    return false;

  double t = tan(theta/2.0);
  double d = radius/t;			/* distance from P to the tangents */
  double r = radius;
  double dmax = (lu < lv ? lu : lv)/2.0;

  if ( d > dmax )
  { d = dmax;
    r = d*t;
  }

  double bisx = ux+vx, bisy = uy+vy;
  double lb = hypot(bisx, bisy);
  double dc = r/sin(theta/2.0);		/* distance from P to the centre */

  c->t1x = px + ux*d; c->t1y = py + uy*d;
  c->t2x = px + vx*d; c->t2y = py + vy*d;
  c->cx  = px + bisx/lb*dc;
  c->cy  = py + bisy/lb*dc;
  c->r   = r;
  c->a1  = atan2(c->t1y-c->cy, c->t1x-c->cx);
  c->a2  = atan2(c->t2y-c->cy, c->t2x-c->cx);

  double delta = c->a2 - c->a1;
  while ( delta > M_PI )   delta -= 2.0*M_PI;
  while ( delta <= -M_PI ) delta += 2.0*M_PI;
  c->negative = (delta < 0);

  return true;
}


static void
path_emit_corner(cairo_t *cr, const path_corner *c)
{ cairo_line_to(cr, c->t1x, c->t1y);
  if ( c->negative )
    cairo_arc_negative(cr, c->cx, c->cy, c->r, c->a1, c->a2);
  else
    cairo_arc(cr, c->cx, c->cy, c->r, c->a1, c->a2);
}


/* Elliptical arc from the current point (x1,y1) to (x2,y2), using the
 * SVG endpoint parameterization (SVG 1.1, appendix F.6.5).
 */

static void
path_emit_arc(cairo_t *cr, double x1, double y1, ArcPoint a,
	      double x2, double y2)
{ double rx = fabs(valNum(a->rx));
  double ry = fabs(valNum(a->ry));
  bool large = (a->large == ON);
  bool sweep = (a->sweep == ON);

  if ( x1 == x2 && y1 == y2 )
    return;
  if ( rx == 0.0 || ry == 0.0 )
  { cairo_line_to(cr, x2, y2);
    return;
  }

  double phi = valNum(a->rotation)*M_PI/180.0;
  double cp = cos(phi), sp = sin(phi);
  double hx = (x1-x2)/2.0, hy = (y1-y2)/2.0;
  double x1p =  cp*hx + sp*hy;
  double y1p = -sp*hx + cp*hy;

  double lambda = (x1p*x1p)/(rx*rx) + (y1p*y1p)/(ry*ry);
  if ( lambda > 1.0 )			/* radii too small: scale up */
  { double s = sqrt(lambda);
    rx *= s;
    ry *= s;
  }

  double num = rx*rx*ry*ry - rx*rx*y1p*y1p - ry*ry*x1p*x1p;
  double den = rx*rx*y1p*y1p + ry*ry*x1p*x1p;
  double coef = (num > 0.0 && den > 0.0) ? sqrt(num/den) : 0.0;
  if ( large == sweep )
    coef = -coef;

  double cxp =  coef*rx*y1p/ry;
  double cyp = -coef*ry*x1p/rx;
  double cx = cp*cxp - sp*cyp + (x1+x2)/2.0;
  double cy = sp*cxp + cp*cyp + (y1+y2)/2.0;

  double t1 = atan2((y1p-cyp)/ry, (x1p-cxp)/rx);
  double dt = atan2((-y1p-cyp)/ry, (-x1p-cxp)/rx) - t1;
  if ( sweep && dt < 0.0 )
    dt += 2.0*M_PI;
  else if ( !sweep && dt > 0.0 )
    dt -= 2.0*M_PI;

  cairo_save(cr);
  cairo_translate(cr, cx, cy);
  cairo_rotate(cr, phi);
  cairo_scale(cr, rx, ry);
  if ( sweep )
    cairo_arc(cr, 0.0, 0.0, 1.0, t1, t1+dt);
  else
    cairo_arc_negative(cr, 0.0, 0.0, 1.0, t1, t1+dt);
  cairo_restore(cr);
}


/* Is the node at index i of the sub-path [s,e) a corner we must round?
 * That is, does it have a straight edge in and a straight edge out?
 * For the first node of a closed sub-path, the edge in is the closing
 * edge.
 */

static bool
path_is_corner(const path_node *nodes, int s, int e, int i)
{ bool closed = nodes[s].closed;

  if ( i == s )
    return closed && e-s > 2 && nodes[s+1].kind == PN_LINE;
  if ( nodes[i].kind != PN_LINE )
    return false;
  if ( i+1 < e )
    return nodes[i+1].kind == PN_LINE;
  return closed && e-s > 2;
}


static void
path_emit_subpath(cairo_t *cr, const path_node *nodes, int s, int e,
		  double radius)
{ bool closed = nodes[s].closed;
  path_corner first;
  bool round_first = ( radius > 0.0 &&
		       path_is_corner(nodes, s, e, s) &&
		       path_round_corner(nodes[e-1].x, nodes[e-1].y,
					 nodes[s].x, nodes[s].y,
					 nodes[s+1].x, nodes[s+1].y,
					 radius, &first) );

  if ( round_first )
    cairo_move_to(cr, first.t2x, first.t2y);
  else
    cairo_move_to(cr, nodes[s].x, nodes[s].y);

  for(int i=s+1; i<e; i++)
  { const path_node *nd = &nodes[i];
    const path_node *pv = &nodes[i-1];

    switch(nd->kind)
    { case PN_LINE:
      { path_corner c;
	const path_node *nx = (i+1 < e ? &nodes[i+1] : &nodes[s]);

	if ( radius > 0.0 && path_is_corner(nodes, s, e, i) &&
	     path_round_corner(pv->x, pv->y, nd->x, nd->y, nx->x, nx->y,
			       radius, &c) )
	  path_emit_corner(cr, &c);
	else
	  cairo_line_to(cr, nd->x, nd->y);
	break;
      }
      case PN_CURVE:
      { CurvePoint cp = (CurvePoint)nd->point;
	double dx = nd->x - valNum(cp->x);	/* the offset */
	double dy = nd->y - valNum(cp->y);
	double c1x = valNum(cp->control1->x)+dx;
	double c1y = valNum(cp->control1->y)+dy;

	if ( isNil(cp->control2) )	/* quadratic: raise to cubic */
	{ cairo_curve_to(cr,
			 pv->x + 2.0/3.0*(c1x-pv->x),
			 pv->y + 2.0/3.0*(c1y-pv->y),
			 nd->x + 2.0/3.0*(c1x-nd->x),
			 nd->y + 2.0/3.0*(c1y-nd->y),
			 nd->x, nd->y);
	} else
	{ cairo_curve_to(cr, c1x, c1y,
			 valNum(cp->control2->x)+dx,
			 valNum(cp->control2->y)+dy,
			 nd->x, nd->y);
	}
	break;
      }
      case PN_ARC:
	path_emit_arc(cr, pv->x, pv->y, (ArcPoint)nd->point, nd->x, nd->y);
	break;
      case PN_MOVE:
	assert(0);
    }
  }

  if ( closed )
  { if ( round_first )
      path_emit_corner(cr, &first);
    cairo_close_path(cr);
  }
}


/* Add the path described by `points' to `cr', shifted by dx,dy.
 */

static void
path_emit(cairo_t *cr, Chain points, double dx, double dy,
	  double radius, bool closed)
{ int size = valInt(points->size);

  if ( size == 0 )
    return;

  path_node *nodes = alloca(sizeof(path_node)*size);
  int n = path_nodes(points, closed, dx, dy, nodes);

  for(int s=0; s<n; )
  { int e = s+1;

    while(e < n && nodes[e].kind != PN_MOVE)
      e++;
    path_emit_subpath(cr, nodes, s, e, radius);
    s = e;
  }
}


static void
path_set_fill_rule(cairo_t *cr, Name fill_rule)
{ cairo_set_fill_rule(cr, fill_rule == NAME_evenOdd
			    ? CAIRO_FILL_RULE_EVEN_ODD
			    : CAIRO_FILL_RULE_WINDING);
}


/**
 * Draw a path defined by a chain of points.
 *
 * @param points The chain of points defining the path.  The class
 *	  of each point determines the segment to it.
 * @param ox The x-offset for the path.
 * @param oy The y-offset for the path.
 * @param radius The corner radius for rounded corners.
 * @param closed Boolean indicating whether the (sub-)paths are closed.
 * @param fill The fill pattern or @nil.
 * @param fill_rule `winding' or `even_odd'
 */
void
r_path(Chain points, double ox, double oy, double radius, bool closed,
       Any fill, Name fill_rule)
{ cairo_new_path(CR);
  path_emit(CR, points, X(ox), Y(oy), radius, closed);

  if ( notNil(fill) )
  { r_fillpattern(fill, NAME_foreground);
    pce_cairo_set_source_fill(CR, context.fill);
    path_set_fill_rule(CR, fill_rule);
    cairo_fill_preserve(CR);
    cairo_set_fill_rule(CR, CAIRO_FILL_RULE_WINDING);
  }

  if ( context.pen )
  { pce_cairo_set_stroke_source(CR);
    cairo_set_line_width(CR, context.pen);
    cairo_stroke(CR);
  } else
  { cairo_new_path(CR);
  }
}


/* A context for computing path geometry.  It is never drawn on and
 * has an identity transformation.
 */

static cairo_t *
scratch_context(void)
{ static cairo_t *cr;

  if ( !cr )
  { cairo_surface_t *surf =
      cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 64, 64);

    cr = cairo_create(surf);
    cairo_surface_destroy(surf);	/* the context holds a reference */
  }

  return cr;
}


static cairo_t *
path_geometry_context(Chain points, double radius, bool closed)
{ cairo_t *cr = scratch_context();

  cairo_new_path(cr);
  path_emit(cr, points, 0.0, 0.0, radius, closed);

  return cr;
}


static void
union_extents(double *ext, double x1, double y1, double x2, double y2)
{ if ( x2 > x1 || y2 > y1 )
  { if ( ext[2] < ext[0] )		/* empty */
    { ext[0] = x1; ext[1] = y1; ext[2] = x2; ext[3] = y2;
    } else
    { if ( x1 < ext[0] ) ext[0] = x1;
      if ( y1 < ext[1] ) ext[1] = y1;
      if ( x2 > ext[2] ) ext[2] = x2;
      if ( y2 > ext[3] ) ext[3] = y2;
    }
  }
}


/**
 * Compute the area covered by drawing the path with the given pen and
 * texture, including what a fill covers.  The coordinates are those of the
 * points.
 *
 * @return false if the path covers nothing.
 */
bool
r_path_extents(Chain points, double radius, bool closed,
	       double pen, Any texture,
	       double *x1, double *y1, double *x2, double *y2)
{ cairo_t *cr = path_geometry_context(points, radius, closed);
  double ext[4] = {0.0, 0.0, -1.0, -1.0};
  double ex1, ey1, ex2, ey2;

  cairo_fill_extents(cr, &ex1, &ey1, &ex2, &ey2);
  union_extents(ext, ex1, ey1, ex2, ey2);
  if ( pen > 0.0 )
  { cairo_set_line_width(cr, pen);
    cairo_set_texture(cr, texture);	/* caps and joins */
    cairo_stroke_extents(cr, &ex1, &ey1, &ex2, &ey2);
    cairo_set_texture(cr, NAME_none);
    union_extents(ext, ex1, ey1, ex2, ey2);
  }
  cairo_new_path(cr);

  if ( ext[2] < ext[0] )
    return false;

  *x1 = ext[0]; *y1 = ext[1];
  *x2 = ext[2]; *y2 = ext[3];
  return true;
}


/**
 * Test whether (x,y) hits the path.  It hits if it is inside the
 * filled area and `area' is true, or if it is within `tolerance' of
 * the line.
 */
bool
r_path_hit(Chain points, double radius, bool closed, Name fill_rule,
	   bool area, double tolerance, double x, double y)
{ cairo_t *cr = path_geometry_context(points, radius, closed);
  bool rc = false;

  if ( area )
  { path_set_fill_rule(cr, fill_rule);
    rc = cairo_in_fill(cr, x, y);
    cairo_set_fill_rule(cr, CAIRO_FILL_RULE_WINDING);
  }
  if ( !rc && tolerance > 0.0 )
  { cairo_set_line_width(cr, tolerance*2.0);
    cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
    cairo_set_line_join(cr, CAIRO_LINE_JOIN_ROUND);
    rc = cairo_in_stroke(cr, x, y);
    cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT);
    cairo_set_line_join(cr, CAIRO_LINE_JOIN_MITER);
  }
  cairo_new_path(cr);

  return rc;
}


static double
distance_segment_point(double x1, double y1, double x2, double y2,
		       double px, double py)
{ double dx = x2-x1, dy = y2-y1;
  double l2 = dx*dx + dy*dy;
  double t = 0.0;

  if ( l2 > 0.0 )
  { t = ((px-x1)*dx + (py-y1)*dy)/l2;
    if ( t < 0.0 ) t = 0.0;
    if ( t > 1.0 ) t = 1.0;
  }

  return hypot(px-(x1+t*dx), py-(y1+t*dy));
}


/**
 * Distance from (x,y) to the line of the path.
 *
 * @return the distance or -1 if the path is empty.
 */
double
r_path_distance(Chain points, double radius, bool closed,
		double x, double y)
{ cairo_t *cr = path_geometry_context(points, radius, closed);
  cairo_path_t *path = cairo_copy_path_flat(cr);
  double best = -1.0;
  double cx = 0.0, cy = 0.0, sx = 0.0, sy = 0.0;

  for(int i=0; i < path->num_data; i += path->data[i].header.length)
  { cairo_path_data_t *d = &path->data[i];
    double nx, ny;

    switch(d->header.type)
    { case CAIRO_PATH_MOVE_TO:
	sx = cx = d[1].point.x;
	sy = cy = d[1].point.y;
	nx = cx; ny = cy;		/* a lonely point has a distance */
	break;
      case CAIRO_PATH_LINE_TO:
	nx = d[1].point.x;
	ny = d[1].point.y;
	break;
      case CAIRO_PATH_CLOSE_PATH:
	nx = sx; ny = sy;
	break;
      default:				/* no curves in a flat path */
	continue;
    }

    double dist = distance_segment_point(cx, cy, nx, ny, x, y);
    if ( best < 0.0 || dist < best )
      best = dist;
    cx = nx; cy = ny;
  }

  cairo_path_destroy(path);
  cairo_new_path(cr);

  return best;
}


/**
 * Find the tip and reference point for an arrow at the start (first
 * is true) or end of the path.  The reference point is the nearest
 * point on the flattened path that differs from the tip, so that the
 * arrow follows the tangent of a curve.
 */
bool
r_path_end(Chain points, double radius, bool closed, bool first,
	   double *tx, double *ty, double *rx, double *ry)
{ cairo_t *cr = path_geometry_context(points, radius, closed);
  cairo_path_t *path = cairo_copy_path_flat(cr);
  int npts = 0;
  double px[2], py[2];			/* first two or last two */
  bool rc = false;

  bool after_close = false;

  for(int i=0; i < path->num_data; i += path->data[i].header.length)
  { cairo_path_data_t *d = &path->data[i];

    switch(d->header.type)
    { case CAIRO_PATH_CLOSE_PATH:
	after_close = true;
	continue;
      case CAIRO_PATH_MOVE_TO:
	if ( after_close )		/* cairo moves back to the start */
	{ after_close = false;
	  continue;
	}
	if ( first && npts == 2 )
	  goto out;
	npts = 0;			/* new sub-path */
	break;
      case CAIRO_PATH_LINE_TO:
	after_close = false;
	break;
      default:				/* no curves in a flat path */
	continue;
    }

    double x = d[1].point.x, y = d[1].point.y;

    if ( npts > 0 && x == px[npts-1] && y == py[npts-1] )
      continue;				/* no progress */
    if ( npts == 2 )
    { if ( first )
	break;
      px[0] = px[1]; py[0] = py[1];
      npts = 1;
    }
    px[npts] = x; py[npts] = y; npts++;
  }

out:
  if ( npts == 2 )
  { if ( first )
    { *tx = px[0]; *ty = py[0];
      *rx = px[1]; *ry = py[1];
    } else
    { *tx = px[1]; *ty = py[1];
      *rx = px[0]; *ry = py[0];
    }
    rc = true;
  }

  cairo_path_destroy(path);
  cairo_new_path(cr);

  return rc;
}

static cairo_surface_t *
recolor_bw_surface(cairo_surface_t* input,
		   Colour foreground,
		   Colour background,
		   bool dbg)
{ uint32_t fg, bg;

  ws_named_colour(foreground);
  ws_named_colour(background);
  fg = valInt(foreground->rgba);
  bg = valInt(background->rgba);
  uint8_t bg_r = ColorRValue(bg);
  uint8_t bg_g = ColorGValue(bg);
  uint8_t bg_b = ColorBValue(bg);

  int width = cairo_image_surface_get_width(input);
  int height = cairo_image_surface_get_height(input);

  cairo_surface_t* output =
    cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
  if ( cairo_surface_status(output) != CAIRO_STATUS_SUCCESS )
    return NULL;

  uint32_t* in_data = (uint32_t*)cairo_image_surface_get_data(input);
  uint32_t* out_data = (uint32_t*)cairo_image_surface_get_data(output);
  int stride = cairo_image_surface_get_stride(input) / 4;

  for (int y = 0; y < height; ++y)
  { if ( dbg ) Cprintf("\ny=%d", y);
    for (int x = 0; x < width; ++x)
    { uint32_t px = in_data[y * stride + x];
      uint8_t a = (px >> 24) & 0xFF;
      uint8_t r = (px >> 16) & 0xFF;
      uint8_t g = (px >> 8) & 0xFF;
      uint8_t b = px & 0xFF;

      uint32_t out_px;

      if ( a == 0 )
      { out_px = 0;
      } else if ( r < 128 && g < 128 && b < 128 )
      { out_px = (fg & 0xFFFFFF) | (0xff << 24);
      } else
      { uint8_t r = (bg_r*a)/255; /* pre-multiply alpha */
	uint8_t g = (bg_g*a)/255;
	uint8_t b = (bg_b*a)/255;
	out_px = RGBA(r,g,b,a);
      }

      if ( dbg ) Cprintf(" 0x%08x->0x%08x", px, out_px);

      out_data[y * stride + x] = out_px;
    }
  }
  if ( dbg ) Cprintf("\n");

  cairo_surface_mark_dirty(output);
  return output;
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
	int x, int y, int w, int h)
{ cairo_surface_t *surface = pceImage2CairoSurface(image);
  bool free_surface = false;

  if ( !surface )
    return;

  if ( image->kind == NAME_bitmap &&
       ( context.colour != BLACK_COLOUR ||
	 context.background != WHITE_COLOUR ) )
  { bool dbg = false;
    DEBUG(NAME_bitmap, dbg = true);
    if ( dbg ) Cprintf("%s: re-colouring\n", pp(image));
    cairo_surface_t *copy = recolor_bw_surface(surface,
					       context.colour,
					       context.background,
					       dbg);
    if ( copy )
    { surface = copy;
      free_surface = true;
    }
  }

  Translate(x, y);
  NormaliseArea(x, y, w, h);
  if ( w == 0 || h == 0 )
    return;

  DEBUG(NAME_draw,
	Cprintf("r_image(%s, %d, %d -> %d, %d, %d, %d)\n",
		pp(image), sx, sy, x, y, w, h));

  int width  = cairo_image_surface_get_width(surface);
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

  if ( free_surface )
    cairo_surface_destroy(surface);
}

static bool
r_set_fill_fgbg(Any fill, Name which)
{ r_fillpattern(fill, which);
  DEBUG(NAME_draw,
	Cprintf("fill with %s->%s\n", pp(fill), pp(context.fill)));
  if ( is_paint(context.fill) )
  { pce_cairo_set_source_fill(CR, context.fill);
    return true;
  } else if ( isNil(context.fill) )
  { cairo_set_source_rgba(CR, 0, 0, 0, 0);
    return true;
  }

  return false;
}

static void
r_fill_fgbg(double x, double y, double w, double h, Any fill, Name which)
{ NormaliseArea(x, y, w, h);
  if ( w > 0 && h > 0 )
  { Translate(x, y);
    cairo_new_path(CR);
    cairo_rectangle(CR, x, y, w, h);	/* before the source: see */
    if ( r_set_fill_fgbg(fill, which) )	/* gradient_to_bounding_box() */
    { bool transparent = isNil(context.fill);
      cairo_operator_t saved_op;

      if ( transparent )
      { saved_op = cairo_get_operator(CR);
	cairo_set_operator(CR, CAIRO_OPERATOR_SOURCE);
      }
      cairo_fill(CR);
      if ( transparent )
	cairo_set_operator(CR, saved_op);
    } else
    { cairo_new_path(CR);
      Cprintf("stub: r_fill(%s)\n", pp(context.fill));
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
r_fill(double x, double y, double w, double h, Any fill)
{ r_fill_fgbg(x, y, w, h, fill, NAME_foreground);
}


/**
 * Fill a polygon defined by a series of points.
 *
 * @param pts An array of points defining the polygon.
 * @param n The number of points in the array.
 */
void
r_fill_polygon(FPoint pts, int n)
{ if ( n <= 0 ) return;

  cairo_new_path(CR);
  double x = pts[0].x;
  double y = pts[0].y;
  Translate(x, y);
  cairo_move_to(CR, x, y);
  for (int i = 1; i < n; i++)
  { double x = pts[i].x;
    double y = pts[i].y;
    Translate(x, y);
    cairo_line_to(CR, x, y);
  }
  cairo_close_path(CR);
  pce_cairo_set_source_fill(CR, context.fill);
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
r_fill_triangle(double x1, double y1, double x2, double y2, double x3, double y3)
{ fpoint pts[3] =
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
static uint32_t *
target_pixel(int x, int y)
{ int width  = cairo_image_surface_get_width(context.target);
  int height = cairo_image_surface_get_height(context.target);
  int stride = cairo_image_surface_get_stride(context.target);
  uint8_t *data = cairo_image_surface_get_data(context.target);

  assert(cairo_image_surface_get_format(context.target) == CAIRO_FORMAT_ARGB32);

  if ( x >= 0 && x < width && y >= 0 && y < height )
    return (uint32_t*)(data + y * stride + x * 4);

  return NULL;
}

bool
r_pixel(int x, int y, Any val)
{ uint32_t *p = target_pixel(x, y);

  if ( p )
  { uint32_t rgba;

    if ( instanceOfObject(val, ClassColour) )
    { Colour c = val;
      ws_named_colour(c);
      rgba = (uint32_t)valInt(c->rgba);
    } else
    { if ( isOn(val) )
	rgba = 0xff000000;
      else
	rgba = 0xffffffff;
    }

    if ( *p != rgba )
    { *p = rgba;
      cairo_surface_mark_dirty(context.target);
    }
    return true;
  } else
  { return false;
  }
}

/**
 * Modify the current drawing context or state.
 */
void
d_modify(void)
{
}

/**
 * Retrieve the color value of a specific pixel.
 *
 * @param x The x-coordinate of the pixel.
 * @param y The y-coordinate of the pixel.
 * @return The color value of the pixel.
 */
COLORRGBA
r_get_pixel(int x, int y)
{ unsigned char *p = (unsigned char *)target_pixel(x, y);

  if ( p )
  { uint8_t b = p[0];
    uint8_t g = p[1];
    uint8_t r = p[2];
    uint8_t a = p[3];

    return RGBA(r,g,b,a);
  } else
  { return 0;
  }
}

/**
 * Check if a font contains a specific character.
 *
 * @param f The font object.
 * @param c The character code to check.
 * @return Non-zero if the character is present; otherwise, zero.
 */
bool
s_has_char(FontObj f, unsigned int c)
{ if ( c > 0x10ffff )
    return false;
  WsFont wsf = ws_get_font(f);

  return wsf && wsf->font && pango_font_has_char(wsf->font, c);
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
double
s_ascent(FontObj font)
{ (void)ws_get_font(font);
  return isNum(font->ascent) ? valNum(font->ascent) : 0.0;
}

/**
 * Retrieve the descent (depth below baseline) of a font.
 *
 * @param font The font object.
 * @return The descent value.
 */
double
s_descent(FontObj font)
{ (void)ws_get_font(font);
  return isNum(font->descent) ? valNum(font->descent) : 0.0;
}

/**
 * Retrieve the line height for the font.  Note that older version sum
 * the ascent and descent.
 *
 * @param font The font object.
 * @return The total height.
 */
double
s_height(FontObj font)
{ return s_ascent(font) + s_descent(font);
}

static
cairo_t *
ws_font_context(void)
{ if ( context.cr )
  { assert(context.open);
    return context.cr;
  }

  DisplayObj d = CurrentDisplay(NIL);
  if ( d )
  { WsDisplay wsd = d->ws_ref;

    if ( wsd && wsd->hidden_cairo )
      return wsd->hidden_cairo;
  }

  /* No display, or none opened: measure on a scratch surface of our own.
   * Text metrics come from Pango rather than from the window system, so
   * they are as good as the ones a display would have given, and drawing
   * on an image works headless (see d_image()).
   */
  return scratch_context();
}

static void
s_extents_utf8(const char *u, size_t ulen, FontObj font,
	       PangoRectangle *ink, PangoRectangle *logical)
{ cairo_t *cr = ws_font_context();
  cairo_save(cr);
  PangoLayout *layout = pce_cairo_set_font(cr, font);
  pango_layout_set_text(layout, u, ulen);
  pango_layout_get_extents(layout, ink, logical);
  cairo_restore(cr);
}


static void
s_extents(PceString s, int from, int to, FontObj font,
	  PangoRectangle *ink, PangoRectangle *logical)
{ string s2 = *s;
  if ( from > s2.s_size )
    from = s2.s_size;
  if ( to > s2.s_size )
    to = s2.s_size;
  if ( to <= from )
  { if ( ink ) memset(ink, 0, sizeof(*ink));
    if ( logical ) memset(logical, 0, sizeof(*logical));
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
  s_extents_utf8(u, ulen, font, ink, logical);
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
double
str_width(PceString s, int from, int to, FontObj font)
{ if ( to > from )
  { PangoRectangle logical;

    s_extents(s, from, to, font, NULL, &logical);
    return logical.width/(double)PANGO_SCALE;
  } else
    return 0.0;
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
double
str_advance(PceString s, int from, int to, FontObj font)
{ if ( to > from )
  { PangoRectangle logical;

    s_extents(s, from, to, font, NULL, &logical);
    return P2D(logical.width);
  } else
    return 0.0;
}

double
str_advance_utf8(const char *u, int ulen, FontObj font)
{ if ( ulen > 0 )
  { PangoRectangle logical;

    s_extents_utf8(u, ulen, font, NULL, &logical);
    return P2D(logical.width);
  }

  return 0.0;
}

/** Advance width of a charW buffer, measured as a Pango run.
 *
 * Unlike summing c_width() per character, this asks Pango to measure
 * the complete sequence, so font-fallback substitution and any GPOS
 * kerning adjustments are included.  Used by paint_line to size the
 * highlight fill rectangle correctly.
 */
double
str_advance_W(charW *s, int l, FontObj font)
{ if ( l <= 0 )
    return 0.0;
  string str = { .text_union = { .textW = s },
                 .hdr.f = { .size = l, .iswide = true, .readonly = true } };
  return str_advance(&str, 0, l, font);
}

/** Map an x pixel offset to a character index within s[from..to).
 *
 * Uses the same Pango layout that renders the text, so font-fallback
 * substitution and GPOS kerning are accounted for.  Summing c_width()
 * per character (as a hand-written hit-test would) drifts from the
 * real glyph positions for proportional fonts with fallback glyphs.
 * The returned value is an absolute index into `s` in the range
 * [from, to].  When `round` is true the result is the nearest caret
 * position (a point past the middle of a glyph maps to the next one,
 * as for click-to-position-caret).  When false it is the index of
 * the glyph the point is inside (hit-test, e.g. picking a symbol).
 */
int
str_x_to_index(PceString s, int from, int to, FontObj font, int x,
	       int round)
{ string s2 = *s;
  if ( from > s2.s_size )
    from = s2.s_size;
  if ( to > s2.s_size )
    to = s2.s_size;
  if ( to <= from )
    return from;

  if ( s2.s_iswide )
    s2.s_textW += from;
  else
    s2.s_textA += from;
  s2.s_size = to-from;

  size_t ulen;
  const char *u = stringToUTF8(&s2, &ulen);

  cairo_t *cr = ws_font_context();
  cairo_save(cr);
  PangoLayout *layout = pce_cairo_set_font(cr, font);
  pango_layout_set_text(layout, u, ulen);

  if ( x < 0 )
    x = 0;
  int byte_index = 0, trailing = 0;
  pango_layout_xy_to_index(layout, x*PANGO_SCALE, 0,
                           &byte_index, &trailing);
  cairo_restore(cr);

  const char *p = u + byte_index;
  if ( round )
  { while ( trailing-- > 0 && p < u+ulen )
      p = g_utf8_next_char(p);
  }

  long off = g_utf8_pointer_to_offset(u, p);
  return from + (int)off;
}

/**
 * Retrieve the width of a specific character in a font.
 *
 * @param c The character code.
 * @param font The font object.
 * @return The width of the character.
 * @todo: cache for fixed-width fonts
 */
double
c_width(uchar_t c, FontObj font)
{ float cw;

  if ( s_cwidth(c, font, &cw) )
  { return cw;
  } else
  { char s[10];
    char *o = utf8_put_char(s, c);
    double w = str_advance_utf8(s, o-s, font);
    s_setcwidth(c, font, w);
    return w;
  }
}

/**
 * Render UTF-8 text, condensed horizontally to `xscale` of its natural
 * advance.
 *
 * A caller laying text out on a grid of its own -- the terminal -- has
 * a width the run must not exceed, and a font is free to disagree: a
 * glyph taken from a fallback face is regularly half again as wide as
 * the cell it has to sit in.  Squeezing it is the graceful way out.  It
 * keeps the whole glyph, unlike clipping, and at the ratios this is
 * called with (mostly 1.1-1.2, occasionally 2) it reads as a slightly
 * narrow symbol rather than a broken one.
 *
 * `xscale` >= 1 draws unscaled: nothing needs squeezing, and the
 * transform would only cost a save/restore.
 *
 * @param u      UTF-8 text
 * @param len    Its length in bytes
 * @param x      Left edge of the run
 * @param y      Its baseline
 * @param font   Font to draw in
 * @param xscale Horizontal scale factor, in (0, 1] to condense
 */
void
s_print_utf8_scaled(const char *u, size_t len, int x, int y, FontObj font,
		    double xscale)
{ DEBUG(NAME_draw,
	{ const char *du = u;
	  char buf[100];
	  if ( u[len] )
	  { size_t dlen = len;
	    if ( dlen > sizeof(buf)-1 )
	      dlen = sizeof(buf)-1;
	    memcpy(buf, u, dlen);
	    buf[dlen] = 0;
	    du = buf;
	  }
	  Cprintf("s_print_utf8(\"%s\", %d, %d, %d, %s) "
		  "(color: %s, xscale: %.2f)\n",
		  du, len, x, y, pp(font), pp(context.colour), xscale);
	});

  Translate(x, y);
  cairo_new_path(CR);
  PangoLayout *layout = pce_cairo_set_font(CR, font);
  pce_cairo_set_source_color(CR, context.colour);
  /* pango_layout_get_baseline reports the baseline of whatever text
   * is currently in the layout, so set the text first — otherwise a
   * previous call that rendered an emoji (whose glyph font has a
   * different baseline than the roman font) makes the next plain-text
   * call draw a few pixels off.  Visible as the "see😀aap" selection
   * shifting the unselected tail. */
  pango_layout_set_text(layout, u, len);
  int baseline = pango_layout_get_baseline(layout);
  double ty = y-P2D(baseline);

  if ( xscale < 1.0 )
  { cairo_save(CR);
    cairo_translate(CR, x, ty);
    cairo_scale(CR, xscale, 1.0);
    /* The layout was synced to the unscaled matrix by
       pce_cairo_set_font(); re-sync it so Pango shapes and hints for
       the matrix we are actually drawing under. */
    pango_cairo_update_layout(CR, layout);
    cairo_move_to(CR, 0.0, 0.0);
    pango_cairo_show_layout(CR, layout);
    cairo_restore(CR);
  } else
  { cairo_move_to(CR, x, ty);
    pango_cairo_show_layout(CR, layout);
  }
}

		 /*******************************
		 *	   PANGO MARKUP		*
		 *******************************/

/**
 * Parse Pango markup.  On success, `plain' (if not NULL) is the text
 * without the markup.  On failure, `error' (if not NULL) is Pango's
 * message.
 */
bool
ws_parse_markup(CharArray markup, StringObj *plain, StringObj *error)
{ const char *s = charArrayToUTF8(markup);
  char *text = NULL;
  GError *err = NULL;

  if ( pango_parse_markup(s, -1, 0, NULL, &text, NULL, &err) )
  { if ( plain )
      *plain = UTF8ToString(text);
    g_free(text);
    return true;
  }

  if ( error )
    *error = UTF8ToString(err->message);
  g_error_free(err);
  return false;
}


/* Create a layout for `m' for drawing on `cr'.  Markup that is not
 * valid is shown as is.  The caller must g_object_unref() the layout.
 */
static PangoLayout *
markup_layout(cairo_t *cr, const markup_spec *m)
{ PangoLayout *layout = ws_new_layout(m->font);

  if ( !layout )
    return NULL;

  const char *s = charArrayToUTF8(m->markup);
  if ( pango_parse_markup(s, -1, 0, NULL, NULL, NULL, NULL) )
    pango_layout_set_markup(layout, s, -1);
  else
    pango_layout_set_text(layout, s, -1);

  if ( m->width > 0.0 )
  { pango_layout_set_width(layout, (int)(m->width*PANGO_SCALE));
    pango_layout_set_wrap(layout, PANGO_WRAP_WORD_CHAR);
  }
  pango_layout_set_alignment(layout,
			     m->alignment == NAME_center ? PANGO_ALIGN_CENTER :
			     m->alignment == NAME_right  ? PANGO_ALIGN_RIGHT :
							   PANGO_ALIGN_LEFT);
  pango_layout_set_justify(layout, m->justify);
  pango_layout_set_ellipsize(layout,
			     m->ellipsize == NAME_start  ? PANGO_ELLIPSIZE_START :
			     m->ellipsize == NAME_middle ? PANGO_ELLIPSIZE_MIDDLE :
			     m->ellipsize == NAME_end    ? PANGO_ELLIPSIZE_END :
							   PANGO_ELLIPSIZE_NONE);
#if PANGO_VERSION_CHECK(1,44,0)
  if ( m->line_spacing > 0.0 )
    pango_layout_set_line_spacing(layout, (float)m->line_spacing);
#endif

  pango_cairo_update_layout(cr, layout);

  return layout;
}


/**
 * Compute the size of the markup when drawn.  If a wrap width is
 * given, the width is at least that.
 */
bool
r_markup_extents(const markup_spec *m, double *w, double *h)
{ cairo_t *cr = ws_font_context();
  bool rc = false;

  cairo_save(cr);
  PangoLayout *layout = markup_layout(cr, m);
  if ( layout )
  { PangoRectangle logical;

    pango_layout_get_extents(layout, NULL, &logical);
    *w = ceil(P2D(logical.x + logical.width));
    *h = ceil(P2D(logical.y + logical.height));
    if ( m->width > 0.0 && *w < m->width )
      *w = m->width;
    g_object_unref(layout);
    rc = true;
  }
  cairo_restore(cr);

  return rc;
}


/**
 * Draw the markup with its top-left corner at x,y.  Text without a
 * colour in the markup uses the current colour.
 */
void
r_markup(const markup_spec *m, double x, double y)
{ Translate(x, y);
  cairo_new_path(CR);

  PangoLayout *layout = markup_layout(CR, m);
  if ( layout )
  { pce_cairo_set_source_color(CR, context.colour);
    cairo_move_to(CR, x, y);
    pango_cairo_show_layout(CR, layout);
    g_object_unref(layout);
  }
}


void
s_print_utf8(const char *u, size_t len, int x, int y, FontObj font)
{ s_print_utf8_scaled(u, len, x, y, font, 1.0);
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

void
str_stext(FontObj font, PceString s, int f, int len,
	  int x, int y, Style style)
{ if ( len > 0 )
  { Any ofg = NULL;

    if ( notNil(style) )
    { double w = str_advance(s, f, f+len, font);

      if ( notDefault(style->background) )
      { double a = s_ascent(font);
	double b = s_descent(font);
	double fx = x, fy = y-a;

	InvTranslate(fx, fy);		/* x,y are translated; r_fill() */
	r_fill(fx, fy, w, b+a, style->background); /* translates again */
      }
      if ( notDefault(style->colour) )
	ofg = r_colour(style->colour);
    }

    str_draw_text(font, s, f, len, x, y);

    if ( ofg )
      r_colour(ofg);
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
 * @param underline is a bool or colour, defining underlining
 * @param flags is currently unused
 */
void
str_string(PceString s, FontObj font,
	   int x, int y, int w, int h,
	   Name hadjust, Name vadjust, Any underline, int flags)
{ str_string_decorated(s, font, x, y, w, h, hadjust, vadjust,
		       underline, OFF);
}

/* A decoration (underline, strikethrough) is @on/@off, a colour or a
 * line texture name.  Find the bool-or-colour and the texture.
 */

static bool
text_decoration(Any spec, Any *value, Name *texture)
{ if ( !spec || isNil(spec) || isDefault(spec) || spec == OFF )
    return false;
  if ( instanceOfObject(spec, ClassName) )	/* texture name */
  { *value   = ON;
    *texture = spec;
  } else
  { *value   = spec;
    *texture = NAME_none;
  }

  return true;
}

/**
 * As str_string(), but also draw a strikethrough.
 *
 * @param underline, strike are @on/@off, a colour or a texture name
 */
void
str_string_decorated(PceString s, FontObj font,
		     int x, int y, int w, int h,
		     Name hadjust, Name vadjust, Any underline, Any strike)
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

  Any ul, st;
  Name ul_texture, st_texture;
  bool do_ul = text_decoration(underline, &ul, &ul_texture);
  bool do_st = text_decoration(strike, &st, &st_texture);

  for(n=0, line = lines; n++ < nlines; line++)
  { str_text(font, &line->text, line->x, line->y+baseline);
    if ( do_ul || do_st )
    { int lx = line->x;			/* r_line() translates again */
      int ly = line->y+baseline;

      InvTranslate(lx, ly);
      if ( do_ul )
	r_underline(font, lx, ly, line->width, ul, ul_texture);
      if ( do_st )
	r_strikethrough(font, lx, ly, line->width, st, st_texture);
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

  int here = 0;
  for(n=0, line = lines; n++ < nlines; line++)
  { int len = line->text.s_size;

    if ( t <= here || f >= here+len )	/* outside */
    { str_text(font, &line->text, line->x, line->y+baseline);
    } else
    { int sf, sx, sl;

      sf = (f <= here     ?      0 : f-here);
      sl = (t >= here+len ? len-sf : t-here-sf);
      sx = str_advance(&line->text, 0, sf, font);

      str_stext(font, &line->text, 0,  sf, line->x,    line->y+baseline, NIL);
      str_stext(font, &line->text, sf, sl, line->x+sx, line->y+baseline, style);
      if ( sf+sl < len )
      { int a  = sf+sl;
	int ax = sx + str_advance(&line->text, sf, a, font);

	str_stext(font, &line->text, a, len-a, line->x+ax, line->y+baseline, NIL);
      }
    }

    here += len + 1;			/* 1 for the newline */
  }
}

/* The index of the character in `s` to underline for accelerator
 * `acc`, or -1.  As the accelerators are assigned (see acc_index() in
 * dialogitem.c), prefer the start of a word, then an uppercase letter
 * and then any occurrence: "Switch to buffer" underlines the `t` of
 * "to".
 */

static int
accelerator_index(PceString s, int acc)
{ int upper = -1, any = -1;

  for(int cn=0; cn<s->s_size; cn++)
  { int c = str_fetch(s, cn);

    if ( (int)tolower(c) != acc )
      continue;
    if ( cn == 0 || iswspace(str_fetch(s, cn-1)) )
      return cn;
    if ( upper < 0 && iswupper(c) )
      upper = cn;
    if ( any < 0 )
      any = cn;
  }

  return upper >= 0 ? upper : any;
}


/**
 * Draws a string, just like str_string(), but underscores the character
 * of the accelerator (case-insensitive).  See accelerator_index().
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
    { int cn = accelerator_index(&line->text, acc);

      if ( cn >= 0 )
      { int cx = line->x+ox + str_advance(&line->text, 0, cn, font);
	int cw = str_width(&line->text, cn, cn+1, font);
	int cy = line->y+baseline+oy;

	InvTranslate(cx, cy);		/* lines are translated; */
	r_underline(font, cx, cy, cw, DEFAULT, NAME_none); /* r_line() too */
	acc = 0;
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

  if ( flags & LABEL_INACTIVE )		/* greyed out, flat */
  { Colour old = context.colour; /* ignore "fixed_colours" */

    context.colour = ws_3d_grey();
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);
    context.colour = old;
  } else
    str_draw_text_lines(acc, font, nlines, lines, 0, 0);
}
