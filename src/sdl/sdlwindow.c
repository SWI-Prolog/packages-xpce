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
#include "sdlwindow.h"
#include "sdlframe.h"
#include "sdldisplay.h"
#include "sdlevent.h"
#include "sdlcursor.h"
#include "sdldraw.h"

/**
 * Check if the specified window has been created.
 *
 * @param sw Pointer to the PceWindow object.
 * @return SUCCEED if the window exists; otherwise, FAIL.
 */
status
ws_created_window(PceWindow sw)
{ if ( instanceOfObject(sw, ClassFrame) )
  {  return ws_created_frame((FrameObj)sw);
  } else
  { WsWindow wsw = sw->ws_ref;
    assert(instanceOfObject(sw, ClassWindow));
    if ( wsw && wsw->backing )
    { FrameObj fr = getFrameWindow(sw, OFF);
      return fr && ws_created_frame(fr);
    }
  }

  fail;
}

/**
 * Destroy the native window associated with the specified PceWindow.
 *
 * @param sw Pointer to the PceWindow object to be destroyed.
 */
void
ws_uncreate_window(PceWindow sw)
{ WsWindow wsw = sw->ws_ref;

  if ( wsw )
  { if ( wsw->backing )
      cairo_surface_destroy(wsw->backing);
    if ( wsw->texture )
    { ASSERT_SDL_MAIN();
      SDL_DestroyTexture(wsw->texture);
    }
    unalloc(sizeof(*wsw), wsw);
    sw->ws_ref = NULL;
  }

  ws_event_destroyed_target(sw);
}

/**
 * Create a native window for the specified PceWindow, optionally as a
 * child of  another.  In  SDL, native windows  are not  window system
 * windows.  They are merely areas  that have a Cairo surface attached
 * in  which  the  drawing  takes  place.   This  implies  that  their
 * "created" state is independent from a frame.
 *
 * @param sw Pointer to the PceWindow object to be created.
 * @param parent Pointer to the parent PceWindow, or NULL for
 *        a top-level window.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_window(PceWindow sw, PceWindow parent)
{ WsWindow wsw = sw->ws_ref;

  if ( !wsw )
  { wsw = sw->ws_ref = alloc(sizeof(ws_window));
    memset(wsw, 0, sizeof(ws_window));
  }

  wsw->scale   = ws_pixel_density_display(sw);
  wsw->w       = valInt(sw->area->w)*wsw->scale;
  wsw->h       = valInt(sw->area->h)*wsw->scale;
  wsw->backing = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
					    wsw->w,  wsw->h);
  assert(wsw->backing);
  d_init_surface(wsw->backing, sw->background);

  DEBUG(NAME_sdl, Cprintf("ws_create_window(%s)\n", pp(sw)));

  succeed;
}

/**
 * Map (display) the specified window on the screen.
 *
 * @param sw Pointer to the PceWindow object to be displayed.
 */
void
ws_manage_window(PceWindow sw)
{
}

/**
 * Unmap (hide) the specified window from the screen.
 *
 * @param sw Pointer to the PceWindow object to be hidden.
 */
void
ws_unmanage_window(PceWindow sw)
{
}

/**
 * Reassociate the native window from one PceWindow to another.
 *
 * @param from Pointer to the source PceWindow.
 * @param to Pointer to the target PceWindow.
 */
void
ws_reassociate_ws_window(PceWindow from, PceWindow to)
{
}

/**
 * Called when the geometry of the window is updated.  Current task
 * is to adjust the size of the backing store texture.
 *
 * @param sw Pointer to the PceWindow object.
 * @param x The new x-coordinate of the window.
 * @param y The new y-coordinate of the window.
 * @param w The new width of the window.
 * @param h The new height of the window.
 * @param pen The border width of the window.
 */
void
ws_geometry_window(PceWindow sw, int x, int y, int w, int h, int pen)
{ WsWindow wsw = sw->ws_ref;

  if ( wsw && wsw->backing )
  { double scale = ws_pixel_density_display(sw);

    if ( wsw->w != w*scale || wsw->h != h*scale )
    { wsw->scale = scale;
      wsw->w = w*scale;
      wsw->h = h*scale;
      cairo_surface_destroy(wsw->backing);
      wsw->backing = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
						wsw->w,  wsw->h);
      assert(wsw->backing);
      d_init_surface(wsw->backing, sw->background);
      if ( wsw->texture )
      { ASSERT_SDL_MAIN();
	SDL_DestroyTexture(wsw->texture);
	wsw->texture = NULL;
      }
      DEBUG(NAME_sdl, Cprintf("Resized %s to %dx%d\n", pp(sw), w, h));
      send(sw, NAME_resize, EAV);
      changed_window(sw,
		     -valInt(sw->scroll_offset->x),
		     -valInt(sw->scroll_offset->y), w, h, TRUE);
      addChain(ChangedWindows, sw);
    }
  }
}

/**
 * Set the 'topmost' status of the specified window.
 *
 * @param sw Pointer to the PceWindow object.
 * @param topmost A BoolObj indicating whether the window should be topmost.
 */
void
ws_topmost_window(PceWindow sw, BoolObj topmost)
{
}

/**
 * Grab or release the pointer (mouse) input for the specified window.
 *
 * @param sw Pointer to the PceWindow object.
 * @param val A BoolObj indicating whether to grab (true) or release
 * (false) the pointer.
 */
void
ws_grab_pointer_window(PceWindow sw, BoolObj val)
{ FrameObj fr = getFrameWindow(sw, OFF);

  if ( fr )
  { WsFrame wfr = fr->ws_ref;
    if ( wfr )
    {
#if 0
      ASSERT_SDL_MAIN();
      if ( !SDL_CaptureMouse(val == ON) )
	DEBUG(NAME_capture,
	      Cprintf("ws_grab_pointer_window(%s, %s) failed: %s\n",
		      pp(sw), pp(val), SDL_GetError()));
      if ( val == ON )
      { if ( SDL_SetWindowMouseRect(wfr->ws_window, NULL) )
	{ DEBUG(NAME_capture,
		Cprintf("Grabbed mouse for %s (%s)\n", pp(fr), pp(sw)));
	} else
	{ DEBUG(NAME_capture,
		Cprintf("SDL_SetWindowMouseRect(%s, %s) failed\n",
			pp(sw), pp(val)));
	}
      }
#endif

      ev_event_grab_window(val == ON ? sw : NIL);
    }
  }
}

/**
 * Grab or release the keyboard input for the specified window.
 *
 * @param sw Pointer to the PceWindow object.
 * @param val A BoolObj indicating whether to grab (true) or release (false) the keyboard.
 */
void
ws_grab_keyboard_window(PceWindow sw, BoolObj val)
{
}

/**
 * Release all input grabs, including pointer and keyboard, from all windows.
 */
void
ws_ungrab_all(void)
{
}

/**
 * Flash a specific rectangular area within the window for a given duration.
 *
 * @param sw Pointer to the PceWindow object.
 * @param x The x-coordinate of the area.
 * @param y The y-coordinate of the area.
 * @param w The width of the area.
 * @param h The height of the area.
 * @param msecs The duration to flash the area, in milliseconds.
 */
void
ws_flash_area_window(PceWindow sw, int x, int y, int w, int h, int msecs)
{
}

/**
 * Flash the entire window for a given duration.
 *
 * @param sw Pointer to the PceWindow object.
 * @param msecs The duration to flash the window, in milliseconds.
 * @todo  This flashes the entire frame as in SDL only the frame is
 * an SDL window.
 */
void
ws_flash_window(PceWindow sw, int msecs)
{ FrameObj fr = getFrameWindow(sw, OFF);
  WsFrame wfr = fr->ws_ref;
  if ( wfr->ws_window )
  { ASSERT_SDL_MAIN();
    SDL_FlashWindow(wfr->ws_window, SDL_FLASH_BRIEFLY);
  }
}

/**
 * Move the pointer (mouse cursor) to a specific location within the window.
 *
 * @param sw Pointer to the PceWindow object.
 * @param x The x-coordinate to move the pointer to.
 * @param y The y-coordinate to move the pointer to.
 */
void
ws_move_pointer(PceWindow sw, int x, int y)
{
}

/**
 * Set the cursor  shape for the specified window. In  SDL, the cursor
 * is global for the application, i.e., it is _not_ set for a window.
 *
 * @param sw Pointer to the PceWindow object.
 * @param cursor The CursorObj representing the new cursor shape.
 */
void
ws_window_cursor(PceWindow sw, CursorObj cursor)
{ SDL_Cursor *c = pceCursor2SDL_Cursor(cursor);
  if ( c )
  { ASSERT_SDL_MAIN();
    SDL_SetCursor(c);
  }
}

/**
 * Set the background of the specified window.
 *
 * @param sw Pointer to the PceWindow object.
 * @param c An Any object representing the new background.
 */
void
ws_window_background(PceWindow sw, Any c)
{
}

/**
 * Raise the specified window above all other windows.
 *
 * @param sw Pointer to the PceWindow object to be raised.
 */
void
ws_raise_window(PceWindow sw)
{
}

/**
 * Lower the specified window below all other windows.
 *
 * @param sw Pointer to the PceWindow object to be lowered.
 */
void
ws_lower_window(PceWindow sw)
{
}

/**
 * Enable or disable the specified window.  An enabled window
 * processed keyboard and mouse events.
 *
 * @param sw Pointer to the PceWindow object.
 * @param enable An integer indicating whether to enable
 *        (non-zero) or disable (zero) the window.
 * @return TRUE or FALSE
 */
int
ws_enable_window(PceWindow sw, int enable)
{ succeed;
}

/**
 * Retrieve the thread identifier associated with the specified window.
 *
 * @param sw Pointer to the PceWindow object.
 * @return An Int representing the thread ID.
 */
Int
ws_window_thread(PceWindow sw)
{
    return (Int)0;
}

/**
 * Schedule a delayed redraw of the specified window.
 *
 * @param sw Pointer to the PceWindow object.
 * @return An integer status code indicating success (non-zero) or failure (zero).
 */
int
ws_delayed_redraw_window(PceWindow sw)
{
    return 0;
}
