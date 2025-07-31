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
#include "sdluserevent.h"

static void	ws_open_display(DisplayObj d, SDL_DisplayID id);

static DisplayObj
ws_create_display(SDL_DisplayID id)
{ ASSERT_SDL_MAIN();
  SDL_Rect rect;
  SDL_GetDisplayBounds(id, &rect);
  const char *name = SDL_GetDisplayName(id);
  DisplayObj dsp;

  dsp = newObject(ClassDisplay,
		  UTF8ToName(name),
		  newObject(ClassArea, toInt(rect.x), toInt(rect.y),
				       toInt(rect.w), toInt(rect.h), EAV),
		  EAV);

  if ( dsp )
  { ws_open_display(dsp, id);
    SDL_GetDisplayUsableBounds(id, &rect);
    assign(dsp, work_area,
	   newObject(ClassArea, toInt(rect.x), toInt(rect.y),
				toInt(rect.w), toInt(rect.h), EAV));
  }

  return dsp;
}

static DisplayObj
ws_update_primary_display(DisplayManager dm)
{ SDL_DisplayID pid = SDL_GetPrimaryDisplay();
  Cell cell;
  DisplayObj primary = NULL;

  for_cell(cell, dm->members)
  { DisplayObj dsp = cell->value;
    WsDisplay  wsd = dsp->ws_ref;
    BoolObj isprimary = ( wsd && wsd->id == pid) ? ON : OFF;
    if ( isDefault(dsp->primary) )
      assign(dsp, primary, isprimary);
    else if ( dsp->primary != isprimary )
      send(dsp, NAME_primary, isprimary, EAV);
    if ( isOn(isprimary) )
      primary = dsp;
  }

  return primary;
}

bool
ws_init_displays(void)
{ int count;
  SDL_DisplayID *displays = SDL_GetDisplays(&count);

  for(int i=0; i<count; i++)
    ws_create_display(displays[i]);

  SDL_free(displays);

  DisplayObj primary = ws_update_primary_display(TheDisplayManager());
  if ( primary )
    nameReferenceObject(primary, NAME_display);

  succeed;
}

DisplayObj
dsp_id_to_display(SDL_DisplayID id)
{ DisplayManager dm = TheDisplayManager();
  Cell cell;

  for_cell(cell, dm->members)
  { DisplayObj d = cell->value;
    WsDisplay wsd = d->ws_ref;

    if ( wsd && wsd->id == id )
      return d;
  }

  return NULL;
}

static void
update_area(Area a, SDL_Rect *rect)
{ assign(a, x, toInt(rect->x));
  assign(a, y, toInt(rect->y));
  assign(a, w, toInt(rect->w));
  assign(a, h, toInt(rect->h));
}

status
ws_poll_dimensions_display(DisplayObj dsp)
{ WsDisplay wsd = dsp->ws_ref;

  if ( wsd )
  { ASSERT_SDL_MAIN();
    SDL_DisplayID id = wsd->id;
    SDL_Rect rect;
    SDL_GetDisplayBounds(id, &rect);
    update_area(dsp->area, &rect);
    SDL_GetDisplayUsableBounds(id, &rect);
    update_area(dsp->work_area, &rect);
  }

  return true;
}


bool
sdl_display_event(SDL_Event *ev)
{ switch(ev->type)
  { case SDL_EVENT_DISPLAY_ADDED:
    { SDL_DisplayID id = ev->display.displayID;
      DisplayObj dsp = ws_create_display(id);
      ws_update_primary_display(TheDisplayManager());
      DEBUG(NAME_display, Cprintf("Added display %s\n", pp(dsp)));
      return true;
    }
    case SDL_EVENT_DISPLAY_REMOVED:
    { SDL_DisplayID id = ev->display.displayID;
      DisplayObj dsp = dsp_id_to_display(id);
      DEBUG(NAME_display, Cprintf("Removed display %s\n", pp(dsp)));
      if ( emptyChain(dsp->frames) )
      { send(dsp, NAME_removed, EAV);
      } else
      { assign(dsp, removed, ON);
	Cprintf("Cannot destroy display %s: has frames\n", pp(dsp));
      }
      return true;
    }
    case SDL_EVENT_DISPLAY_MOVED:
    { SDL_DisplayID id = ev->display.displayID;
      DisplayObj dsp = dsp_id_to_display(id);
      DEBUG(NAME_display, Cprintf("Moved display %s\n", pp(dsp)));
      return ws_poll_dimensions_display(dsp);
    }
  }

  fail;
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
bool
ws_resolution_display(DisplayObj d, int *rx, int *ry)
{ float scale = ws_pixel_density_display(d);
  *rx = 96*scale;
  *ry = 96*scale;
  return true;
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
 * Open the display, establishing a connection for graphical operations.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
static void
ws_open_display(DisplayObj d, SDL_DisplayID id)
{ ASSERT_SDL_MAIN();
  WsDisplay wsd = d->ws_ref = alloc(sizeof(ws_display));
  memset(wsd, 0, sizeof(*wsd));
  SDL_WindowFlags flags = SDL_WINDOW_HIDDEN;

#if O_HDP
  flags |= SDL_WINDOW_HIGH_PIXEL_DENSITY;
#endif

  wsd->id = id;
  wsd->hidden_window = SDL_CreateWindow(
    "xpce hidden window", 64, 64,
    flags);
  wsd->hidden_renderer = SDL_CreateRenderer(wsd->hidden_window, NULL);
  wsd->hidden_surface = cairo_image_surface_create(
    CAIRO_FORMAT_ARGB32, 64, 64);
  wsd->hidden_cairo = cairo_create(wsd->hidden_surface);
  wsd->scale = SDL_GetWindowPixelDensity(wsd->hidden_window);
  cairo_scale(wsd->hidden_cairo, wsd->scale, wsd->scale);
}

/**
 * Close the SDL resources for a display
 *
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_close_display(DisplayObj d)
{ WsDisplay wsd = d->ws_ref;

  if ( wsd )
  { ASSERT_SDL_MAIN();
    d->ws_ref = NULL;
    SDL_DestroyRenderer(wsd->hidden_renderer);
    SDL_DestroyWindow(wsd->hidden_window);
    cairo_destroy(wsd->hidden_cairo);
    cairo_surface_destroy(wsd->hidden_surface);
    unalloc(sizeof(*wsd), wsd);
  }
}

float
ws_pixel_density_display(Any obj)
{ DisplayObj dsp = CurrentDisplay(obj);

  if ( dsp && dsp->ws_ref )
  { WsDisplay wsd = dsp->ws_ref;
    return wsd->scale;
  } else
  { ASSERT_SDL_MAIN();
    SDL_DisplayID display_id = SDL_GetPrimaryDisplay();
    return SDL_GetDisplayContentScale(display_id);
  }
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
 * Check if there are events queued on the display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED if events are queued; otherwise, FAIL.
 */
status
ws_events_queued_display(DisplayObj d)
{ /* may be called from any thread */
  return SDL_HasEvents(0, MY_EVENT_HIGHEST);
}

status
ws_selection_display(DisplayObj d, Name which, StringObj data)
{ ASSERT_SDL_MAIN();
  char *u8 = charArrayToUTF8((CharArray)data);

  if ( which == NAME_primary )
    return SDL_SetPrimarySelectionText(u8);
  if ( which == NAME_clipboard )
    return SDL_SetClipboardText(u8);

  fail;
}

/**
 * Retrieve the current selection for the specified type and target.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @param which Name object specifying the selection source as one of
 *        NAME_clipboard or NAME_primary.
 * @param target Name object specifying the target format.  Normally
 *        NAME_text
 * @return Object representing the selection.
 */
Any
ws_get_selection(DisplayObj d, Name which, Name target)
{ ASSERT_SDL_MAIN();
  if ( target == NAME_text || target == NAME_utf8_string )
  { const char *text = NULL;

    if ( which == NAME_clipboard )
      text = SDL_GetClipboardText();
    else if ( which == NAME_primary )
      text = SDL_GetPrimarySelectionText();

    if ( text )
      return UTF8ToString(text);
  }

  Cprintf("ws_get_selection(%s, %s, %s): not supported\n",
	  pp(d), pp(which), pp(target));
  fail;
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

Name
ws_get_system_theme_display(DisplayObj d)
{ ASSERT_SDL_MAIN();
  SDL_SystemTheme theme = SDL_GetSystemTheme();

  switch(theme)
  { case SDL_SYSTEM_THEME_UNKNOWN:
      fail;
    case SDL_SYSTEM_THEME_LIGHT:
      return NAME_light;
    case SDL_SYSTEM_THEME_DARK:
      return NAME_dark;
  }

  fail;
}
