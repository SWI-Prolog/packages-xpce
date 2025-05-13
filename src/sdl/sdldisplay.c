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
#include "sdldisplay.h"

/**
 * Flush the display buffer, ensuring all pending drawing operations are executed.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_flush_display(DisplayObj d)
{
}

/**
 * Synchronize the display, ensuring all operations are completed.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_synchronise_display(DisplayObj d)
{
}

/**
 * Sound the system bell with the specified volume.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param volume The volume level for the bell sound.
 */
void
ws_bell_display(DisplayObj d, int volume)
{
}

/**
 * Retrieve the size of the display in pixels.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param w Pointer to an integer to store the width.
 * @param h Pointer to an integer to store the height.
 */
void
ws_get_size_display(DisplayObj d, int *w, int *h)
{ Monitor mon;

  if ( openDisplay(d) &&
       (mon = getHeadChain(d->monitors)) &&
       instanceOfObject(mon, ClassMonitor) )
  { *w = valInt(mon->area->w);
    *h = valInt(mon->area->h);
  } else
  { *w = *h = 0;
  }
}

/**
 * Get the visual type of the display (e.g., TrueColor, PseudoColor).
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return Name object representing the visual type.
 */
Name
ws_get_visual_type_display(DisplayObj d)
{
    return NULL;
}

/**
 * Retrieve the color depth (bits per pixel) of the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return Integer representing the color depth.
 */
int
ws_depth_display(DisplayObj d)
{ return 32;			/* 8-bit RGBA */
}

/**
 * Get the resolution of the display in pixels per inch.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param rx Pointer to an integer to store the horizontal resolution.
 * @param ry Pointer to an integer to store the vertical resolution.
 * @return Integer status code indicating success or failure.
 */
int
ws_resolution_display(DisplayObj d, int *rx, int *ry)
{
    return 0;
}

/**
 * Activate the screen saver on the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_activate_screen_saver(DisplayObj d)
{
}

/**
 * Deactivate the screen saver on the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_deactivate_screen_saver(DisplayObj d)
{
}

/**
 * Initialize the display, preparing it for graphical operations.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_init_display(DisplayObj d)
{
}

/**
 * Check if the provided display name is legal and can be used.
 *
 * @param s Pointer to a string representing the display name.
 * @return SUCCEED if the name is legal; otherwise, FAIL.
 */
status
ws_legal_display_name(const char *s)
{
    return SUCCEED;
}

/**
 * Check if the display has been successfully opened.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED if the display is open; otherwise, FAIL.
 */
status
ws_opened_display(DisplayObj d)
{ return !!d->ws_ref;
}

/**
 * Open the display, establishing a connection for graphical operations.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_open_display(DisplayObj d)
{ WsDisplay wsd = d->ws_ref = alloc(sizeof(ws_display));
  memset(wsd, 0, sizeof(*wsd));

  wsd->hidden_window = SDL_CreateWindow(
    "xpce hidden window", 64, 64,
    SDL_WINDOW_HIDDEN);
  wsd->hidden_renderer = SDL_CreateRenderer(wsd->hidden_window, NULL);
  wsd->hidden_surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
						   64, 64);
  wsd->hidden_cairo = cairo_create(wsd->hidden_surface);
}

/**
 * Close the display, terminating the connection.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_quit_display(DisplayObj d)
{ WsDisplay wsd = d->ws_ref;

  if ( wsd )
  { d->ws_ref = NULL;
    SDL_DestroyRenderer(wsd->hidden_renderer);
    SDL_DestroyWindow(wsd->hidden_window);
    cairo_destroy(wsd->hidden_cairo);
    cairo_surface_destroy(wsd->hidden_surface);
    unalloc(sizeof(*wsd), wsd);
  }
}

/**
 * Initialize graphics-related resources for the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful initialization; otherwise, FAIL.
 */
status
ws_init_graphics_display(DisplayObj d)
{
    return SUCCEED;
}

/**
 * Initialize monitor-related information for the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful initialization; otherwise, FAIL.
 */
status
ws_init_monitors_display(DisplayObj d)
{ int count;
  SDL_DisplayID *displays = SDL_GetDisplays(&count);

  assign(d, monitors, newObject(ClassChain, EAV));

  for(int i=0; i<count; i++)
  { SDL_Rect rect;
    SDL_GetDisplayBounds(displays[i], &rect);
    const char *name = SDL_GetDisplayName(displays[i]);
    appendChain(d->monitors,
		newObject(ClassMonitor, UTF8ToName(name),
			  newObject(ClassArea,
				    toInt(rect.x),
				    toInt(rect.y),
				    toInt(rect.w),
				    toInt(rect.h), EAV), EAV));
  }
  SDL_free(displays);

  succeed;
}

/**
 * Set the foreground color for the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param c Pointer to the Colour object representing the foreground color.
 */
void
ws_foreground_display(DisplayObj d, Colour c)
{
}

/**
 * Set the background color for the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param c Pointer to the Colour object representing the background color.
 */
void
ws_background_display(DisplayObj d, Colour c)
{
}

/**
 * Draw a graphical object on the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param gr Pointer to the Graphical object to be drawn.
 * @param invert BoolObj indicating whether to invert the drawing.
 * @param subtoo BoolObj indicating whether to include subcomponents.
 */
void
ws_draw_in_display(DisplayObj d, Graphical gr, BoolObj invert, BoolObj subtoo)
{
}

/**
 * Grab the server, preventing other clients from accessing it.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_grab_server(DisplayObj d)
{
}

/**
 * Release the server, allowing other clients to access it.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_ungrab_server(DisplayObj d)
{
}

/**
 * Retrieve the connection number for the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return Int representing the connection number.
 */
Int
ws_display_connection_number(DisplayObj d)
{
    return (Int)0;
}

/**
 * Check if there are events queued on the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED if events are queued; otherwise, FAIL.
 */
status
ws_events_queued_display(DisplayObj d)
{
    return SUCCEED;
}

/**
 * Get the current pointer (mouse) location on the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param x Pointer to an integer to store the x-coordinate.
 * @param y Pointer to an integer to store the y-coordinate.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_pointer_location_display(DisplayObj d, int *x, int *y)
{
    return SUCCEED;
}

/**
 * Set the contents of the specified cut buffer.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param n The cut buffer number to set.
 * @param s Pointer to the PceString containing the data.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_set_cutbuffer(DisplayObj d, int n, PceString s)
{
    return SUCCEED;
}

/**
 * Retrieve the contents of the specified cut buffer.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param n The cut buffer number to retrieve.
 * @return Pointer to the StringObj containing the data; NULL if unavailable.
 */
StringObj
ws_get_cutbuffer(DisplayObj d, int n)
{
    return NULL;
}

/**
 * Get the current selection timeout value.
 *
 * @return Unsigned long representing the selection timeout in milliseconds.
 */
unsigned long
ws_get_selection_timeout(void)
{
    return 0;
}

/**
 * Set the selection timeout value.
 *
 * @param time Unsigned long representing the new selection timeout in milliseconds.
 */
void
ws_set_selection_timeout(unsigned long time)
{
}

/**
 * Retrieve the current selection for the specified type and target.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param which Name object specifying the selection type.
 * @param target Name object specifying the target format.
 * @return Pointer to the Any object containing the selection data; NULL if unavailable.
 */
Any
ws_get_selection(DisplayObj d, Name which, Name target)
{
    return NULL;
}

/**
 * Disown a previously owned X selection.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param selection The selection type to disown (e.g., PRIMARY, CLIPBOARD).
 */
void
ws_disown_selection(DisplayObj d, Name selection)
{
}

/**
 * Claim ownership of an X selection.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param selection The selection to claim ownership of.
 * @param type The type of content provided for this selection.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_own_selection(DisplayObj d, Name selection, Name type)
{
    return SUCCEED;
}

/**
 * Retrieve the name of the window manager in use.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return A Name object identifying the current window manager.
 */
Name
ws_window_manager(DisplayObj d)
{
    return NULL;
}

/**
 * Enable synchronous mode for display requests (useful for debugging).
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_synchronous(DisplayObj d)
{
}

/**
 * Revert to asynchronous mode for display requests (default behavior).
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_asynchronous(DisplayObj d)
{
}

/**
 * Enable PostScript support on the display if available.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param iscolor A boolean flag indicating if color should be used.
 * @return SUCCEED if PostScript output is supported; otherwise, FAIL.
 */
status
ws_postscript_display(DisplayObj d, int iscolor)
{
    return SUCCEED;
}

/**
 * Capture a region of the screen as an Image object.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param x The x-coordinate of the region's top-left corner.
 * @param y The y-coordinate of the region's top-left corner.
 * @param width The width of the region to capture.
 * @param height The height of the region to capture.
 * @return An Image object containing the screen capture; NULL on failure.
 */
Image
ws_grab_image_display(DisplayObj d, int x, int y, int width, int height)
{
    return NULL;
}
