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
#include "sdldraw.h"
#include "sdlcolour.h"
#include "sdlframe.h"

typedef struct
{ PceWindow	window;			/* Pce's notion of the window */
  FrameObj	frame;			/* Pce's frame of the window */
  SDL_Renderer *renderer;		/* The SDL renderer for the window */
  Any		colour;			/* Current colour */
  Any		background;		/* Background colour */
} sdl_draw_context;

#include <gra/graphstate.c>

static sdl_draw_context	context;	/* current context */

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
{
}

/**
 * Set the rendering offset for subsequent rendering operations.
 *
 * @param x The x-coordinate offset.
 * @param y The y-coordinate offset.
 */
void
r_offset(int x, int y)
{
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
 * Retrieve the display object associated with the given display.
 *
 * @param d Pointer to the DisplayObj.
 * @return The associated DisplayObj.
 */
DisplayObj
d_display(DisplayObj d)
{
    return d;
}

/**
 * Ensure that the display is properly initialized and ready for drawing.
 */
void
d_ensure_display(void)
{
}

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
{ Cprintf("d_window(%s, %d, %d, %d, %d, %d, %d)\n",
	  pp(sw), x, y, w, h, clear, limit);

  context.window     = sw;
  context.frame      = getFrameWindow(sw, OFF);
  context.colour     = sw->colour;
  context.background = sw->background;

  if ( context.frame )
  { WsFrame f = context.frame->ws_ref;

    context.renderer = f->ws_renderer;
  } else
  { assert(0);
    fail;
  }

  if ( clear )
    r_clear(x, y, w, h);

  succeed;
}

/**
 * Draw an image at the specified location with given dimensions.
 *
 * @param i Pointer to the Image object.
 * @param x The x-coordinate where the image will be drawn.
 * @param y The y-coordinate where the image will be drawn.
 * @param w The width to draw the image.
 * @param h The height to draw the image.
 */
void
d_image(Image i, int x, int y, int w, int h)
{
}

/**
 * Set the current drawing target to the specified display.
 *
 * @param d Pointer to the DisplayObj.
 */
void
d_screen(DisplayObj d)
{
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
void
d_frame(FrameObj fr, int x, int y, int w, int h)
{
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
{ Cprintf("d_done()\n");
  SDL_RenderPresent(context.renderer);
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
{ r_fill(x, y, w, h, context.background);
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
{
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
 * @param which Identifier for the fill pattern.
 */
void
r_fillpattern(Any fill, Name which)
{
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
{
    return NULL;
}

/**
 * Retrieve the current drawing color.
 *
 * @param c The input color or identifier.
 * @return The current drawing color.
 */
Any
r_colour(Any c)
{
    return NULL;
}

/**
 * Retrieve the current background color.
 *
 * @param c The input color or identifier.
 * @return The current background color.
 */
Any
r_background(Any c)
{
    return NULL;
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
{
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
{ Cprintf("r_box(%d, %d, %d, %d, %s)\n", x, y, w, h, pp(fill));
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
r_shadow_box(int x, int y, int w, int h, int r, int shadow, Image fill)
{ Cprintf("r_shadow_box(%d, %d, %d, %d, %s)\n", x, y, w, h, pp(fill));
}

/**
 * Retrieve the shadow associated with a specific elevation level.
 *
 * @param e The elevation level.
 * @return The shadow corresponding to the elevation.
 */
Any
r_elevation_shadow(Elevation e)
{
    return NULL;
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
void
r_3d_box(int x, int y, int w, int h, int radius, Elevation e, int up)
{
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
r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Elevation e, int up, int map)
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
{
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
r_image(Image image, int sx, int sy, int x, int y, int w, int h, BoolObj transparent)
{
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
{ Cprintf("r_fill(%d, %d, %d, %d, %s)\n", x, y, w, h, pp(fill));

  if ( isDefault(fill) )
    fill = context.colour;
  if ( instanceOfObject(fill, ClassColour) )
  { sdl_color c = pceColour2SDL(fill);
    SDL_SetRenderDrawColor(context.renderer, c.r, c.g, c.b, c.a);
    SDL_FRect rect = { (float)x, (float)y, (float)w, (float)h };
    SDL_RenderFillRect(context.renderer, &rect);
  } else
  { Cprintf("stub: r_fill(%s)\n", pp(fill));
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
{
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
{
    return 0;
}

/**
 * Retrieve the domain (bounding box) of a font.
 *
 * @param f The font object.
 * @param which The aspect of the domain to retrieve.
 * @param x Pointer to store the x-dimension.
 * @param y Pointer to store the y-dimension.
 */
void
f_domain(FontObj f, Name which, int *x, int *y)
{
}

/**
 * Retrieve the default character code for a font.
 *
 * @param font The font object.
 * @return The default character code.
 */
int
s_default_char(FontObj font)
{
    return 0;
}

/**
 * Retrieve the ascent (height above baseline) of a font.
 *
 * @param f The font object.
 * @return The ascent value.
 */
int
s_ascent(FontObj f)
{
    return 0;
}

/**
 * Retrieve the descent (depth below baseline) of a font.
 *
 * @param f The font object.
 * @return The descent value.
 */
int
s_descent(FontObj f)
{
    return 0;
}

/**
 * Retrieve the total height of a font (ascent + descent).
 *
 * @param f The font object.
 * @return The total height.
 */
int
s_height(FontObj f)
{
    return 0;
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
{
    return 0;
}

/**
 * Calculate the width of a substring within a string using a specific font.
 *
 * @param s The string object.
 * @param from The starting index of the substring.
 * @param to The ending index of the substring.
 * @param f The font object.
 * @return The width of the substring.
 */
int
str_width(PceString s, int from, int to, FontObj f)
{
    return 0;
}

/**
 * Calculate the advance width (cursor movement) of a substring within a string using a specific font.
 *
 * @param s The string object.
 * @param from The starting index of the substring.
 * @param to The ending index of the substring.
 * @param f The font object.
 * @return The advance width of the substring.
 */
int
str_advance(PceString s, int from, int to, FontObj f)
{
    return 0;
}

/**
 * Render a string of ASCII characters at a specified position using a specific font.
 *
 * @param s The array of ASCII characters.
 * @param l The length of the character array.
 * @param x The x-coordinate for rendering.
 * @param y The y-coordinate for rendering.
 * @param f The font object.
 */
void
s_printA(charA *s, int l, int x, int y, FontObj f)
{
}

/**
 * Render a string of wide characters at a specified position using a specific font.
 *
 * @param s The array of wide characters.
 * @param l The length of the character array.
 * @param x The x-coordinate for rendering.
 * @param y The y-coordinate for rendering.
 * @param f The font object.
 */
void
s_printW(charW *s, int l, int x, int y, FontObj f)
{
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
{
}

/**
 * Render a PceString at a specified position with alignment using a specific font.
 *
 * @param s The PceString object.
 * @param x The x-coordinate for rendering.
 * @param y The y-coordinate for rendering.
 * @param f The font object.
 */
void
s_print_aligned(PceString s, int x, int y, FontObj f)
{
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
{
    *width = 0;
    *height = 0;
}


/**
 * Draw a string within a specified area, applying horizontal and vertical alignment.
 *
 * @param s Pointer to the PceString object to be drawn.
 * @param font Pointer to the FontObj specifying the font to use.
 * @param x The x-coordinate of the top-left corner of the drawing area.
 * @param y The y-coordinate of the top-left corner of the drawing area.
 * @param w The width of the drawing area.
 * @param h The height of the drawing area.
 * @param hadjust Name indicating horizontal alignment (e.g., left, center, right).
 * @param vadjust Name indicating vertical alignment (e.g., top, center, bottom).
 * @param flags Additional flags controlling rendering behavior.
 */
void
str_string(PceString s, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags)
{
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
str_selected_string(PceString s, FontObj font, int f, int t, Style style, int x, int y, int w, int h, Name hadjust, Name vadjust)
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
ps_string(PceString s, FontObj font, int x, int y, int w, Name format, int flags)
{
}

/**
 * Draw a label with an optional accelerator key indicator within a specified area.
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
str_label(PceString s, int acc, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags)
{
}
