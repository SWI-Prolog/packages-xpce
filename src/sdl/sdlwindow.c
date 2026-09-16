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
  ws_dirty_all_window(sw);

  DEBUG(NAME_sdl, Cprintf("ws_create_window(%s)\n", pp(sw)));

  succeed;
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
						max(wsw->w, 1),
						max(wsw->h, 1));
      assert(wsw->backing);
      d_init_surface(wsw->backing, sw->background);
      wsw->ndirty = 0;			/* rectangles refer to the old size */
      ws_dirty_all_window(sw);
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

		 /*******************************
		 *	      DAMAGE		*
		 *******************************/

/* A  window's `backing`  is a  Cairo surface  that is  uploaded into a
 * `texture` of  the frame's renderer  to get  it on the  screen.  That
 * upload is by  far the most expensive step of  drawing a frame: it is
 * a  copy of  the  full window  through the  GL  driver.  As  ordinary
 * updates  touch a  tiny part  of a  window we  keep track  of what  a
 * redraw actually changed and upload no more than that.  See
 * ws_draw_window() in sdlframe.c.
 *
 * All rectangles are in device pixels, i.e., they are scaled by
 * <-pixel_density.  We keep at most MAX_DIRTY_RECTS of them apart and
 * merge the two that lose the least by being merged if more arrive.
 */

static int
rect_area(const SDL_Rect *r)
{ return r->w*r->h;
}

static void
rect_union(SDL_Rect *r, const SDL_Rect *a)
{ int x2 = max(r->x+r->w, a->x+a->w);
  int y2 = max(r->y+r->h, a->y+a->h);

  r->x = min(r->x, a->x);
  r->y = min(r->y, a->y);
  r->w = x2 - r->x;
  r->h = y2 - r->y;
}

static int
merge_cost(const SDL_Rect *a, const SDL_Rect *b)
{ SDL_Rect u = *a;

  rect_union(&u, b);
  return rect_area(&u) - rect_area(a) - rect_area(b);
}

/**
 * Add (x,y,w,h) to the damaged region of `sw`.
 *
 * @param x,y,w,h Damaged area in device pixels, relative to the
 *        window's backing surface.
 */
void
ws_dirty_window(PceWindow sw, int x, int y, int w, int h)
{ WsWindow wsw = sw->ws_ref;

  if ( !wsw || !wsw->backing )
    return;

  if ( x < 0 ) { w += x; x = 0; }		/* clip to the surface */
  if ( y < 0 ) { h += y; y = 0; }
  if ( x+w > wsw->w ) w = wsw->w - x;
  if ( y+h > wsw->h ) h = wsw->h - y;
  if ( w <= 0 || h <= 0 )
    return;

  SDL_Rect add = { x, y, w, h };

  for(int i=0; i<wsw->ndirty; i++)		/* already covered? */
  { SDL_Rect *r = &wsw->dirty[i];

    if ( add.x >= r->x && add.y >= r->y &&
	 add.x+add.w <= r->x+r->w &&
	 add.y+add.h <= r->y+r->h )
      return;
  }

  if ( wsw->ndirty < MAX_DIRTY_RECTS )
  { wsw->dirty[wsw->ndirty++] = add;
    return;
  }
						/* full: merge cheapest pair */
  int bi = 0, bj = 1;
  int best = merge_cost(&wsw->dirty[0], &wsw->dirty[1]);

  for(int i=0; i<wsw->ndirty; i++)
  { for(int j=i+1; j<wsw->ndirty; j++)
    { int c = merge_cost(&wsw->dirty[i], &wsw->dirty[j]);

      if ( c < best )
      { best = c; bi = i; bj = j;
      }
    }
    int c = merge_cost(&wsw->dirty[i], &add);
    if ( c < best )
    { best = c; bi = i; bj = -1;
    }
  }

  if ( bj == -1 )			/* merge the new one into dirty[bi] */
  { rect_union(&wsw->dirty[bi], &add);
  } else				/* merge bj into bi and take its slot */
  { rect_union(&wsw->dirty[bi], &wsw->dirty[bj]);
    wsw->dirty[bj] = wsw->dirty[--wsw->ndirty];
    wsw->dirty[wsw->ndirty++] = add;
  }
}

/**
 * Mark the entire window as damaged.
 */
void
ws_dirty_all_window(PceWindow sw)
{ WsWindow wsw = sw->ws_ref;

  if ( wsw && wsw->backing )
  { wsw->ndirty = 1;
    wsw->dirty[0] = (SDL_Rect){ 0, 0, wsw->w, wsw->h };
  }
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
{ if ( val == ON )
  { FrameObj fr = getFrameWindow(sw, OFF);

    if ( fr && fr->ws_ref )		/* only grab for a realised frame */
      ev_event_grab_window(sw);
  } else
  { /* Release unconditionally.  The frame may already be on its way
       out, and the old test left the grab in place if it was.
    */
    ev_event_grab_window(NIL);
  }
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
{ FrameObj fr = getFrameWindow(sw, OFF);
  WsFrame wfr = fr ? fr->ws_ref : NULL;
  if ( !wfr || !wfr->ws_window )
    return;
  ASSERT_SDL_MAIN();
  float scale = SDL_GetWindowPixelDensity(wfr->ws_window);
  float ox = 0.0f, oy = 0.0f;
  ws_window_frame_position(sw, fr, &ox, &oy);
  wfr->flash_rect   = (SDL_FRect){ (ox+x)*scale, (oy+y)*scale,
				    w*scale,       h*scale };
  wfr->flash_end_ms = SDL_GetTicks() + msecs;
  SDL_AddTimer(msecs, flash_end_callback, fr);
  ws_draw_frame(fr);
}

/**
 * Flash the entire window for a given duration.
 *
 * @param sw Pointer to the PceWindow object.
 * @param msecs The duration to flash the window, in milliseconds.
 */
void
ws_flash_window(PceWindow sw, int msecs)
{ ws_flash_area_window(sw,
		       0, 0, valInt(sw->area->w), valInt(sw->area->h),
		       msecs);
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
{ FrameObj fr = getFrameWindow(sw, OFF);
  WsFrame wfr = fr ? fr->ws_ref : NULL;
  float ox = 0.0f, oy = 0.0f;

  if ( wfr && wfr->ws_window &&
       ws_window_frame_position(sw, fr, &ox, &oy) )
  { ASSERT_SDL_MAIN();
    SDL_WarpMouseInWindow(wfr->ws_window, ox+x, oy+y);
  }
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
{ if ( ws_busy_cursor() )               /* covers every window */
    return;

  SDL_Cursor *c = pceCursor2SDL_Cursor(cursor);
  ASSERT_SDL_MAIN();
  SDL_SetCursor(c ? c : SDL_GetDefaultCursor()); /* the window may have none */
}
