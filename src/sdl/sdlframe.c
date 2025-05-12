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
#include "sdlframe.h"
#include "sdlwindow.h"
#include "sdlcolour.h"

#define MainWindow(fr)	     ( isNil(fr->members->head) ? (Any) fr : \
			       fr->members->head->value )

WsFrame
sdl_frame(FrameObj fr, bool create)
{ WsFrame f;

  if ( !(f=fr->ws_ref) && create )
  { f = fr->ws_ref = alloc(sizeof(*f));
    memset(f, 0, sizeof(*f));
  }

  return f;
}


/**
 * Check if the frame has been created.
 *
 * @param fr Pointer to the FrameObj to check.
 * @return SUCCEED if the frame is created; otherwise, FAIL.
 */
status
ws_created_frame(FrameObj fr)
{ WsFrame f = sdl_frame(fr, false);

  return f && f->ws_window;
}

/**
 * Uncreate or destroy the specified frame.
 *
 * @param fr Pointer to the FrameObj to uncreate.
 */
void
ws_uncreate_frame(FrameObj fr)
{ WsFrame f = sdl_frame(fr, false);

  if ( f && f->ws_window )
  { deleteChain(ChangedFrames, fr);
    SDL_DestroyRenderer(f->ws_renderer);
    SDL_DestroyWindow(f->ws_window);
    unalloc(sizeof(*f), f);
    fr->ws_ref = NULL;
  }
}

/**
 * Create the specified frame.
 *
 * @param fr Pointer to the FrameObj to create.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_frame(FrameObj fr)
{ SDL_WindowFlags flags = 0;
  SDL_Window *w = NULL;

  if ( fr->can_resize == ON )
    flags |= SDL_WINDOW_RESIZABLE;
  if ( fr->kind == NAME_popup )
  { flags |= SDL_WINDOW_POPUP_MENU;
    Any pfr = getAttributeObject(fr, NAME_parent);
    if ( pfr && instanceOfObject(pfr, ClassFrame) )
    { WsFrame   pf = sdl_frame(pfr, false);
      DEBUG(NAME_popup, Cprintf("Opening popup for %s\n", pp(pfr)));
      w = SDL_CreatePopupWindow(
	pf->ws_window,
	valInt(fr->area->x),
	valInt(fr->area->y),
	valInt(fr->area->w),
	valInt(fr->area->h),
	flags);
    } else
    { Cprintf("a popup frame can only be created with a parent\n");
      fail;
    }
  } else
  { w = SDL_CreateWindow(
      nameToMB(fr->label),
      valInt(fr->area->w),
      valInt(fr->area->h),
      flags);
  }

  if ( w )
  { SDL_Renderer *renderer = SDL_CreateRenderer(w, NULL);
    assert(renderer);
    SDL_RenderPresent(renderer); /* Probably temporary */

    WsFrame f = sdl_frame(fr, true);
    f->ws_window = w;
    f->ws_renderer = renderer;
    f->ws_id = SDL_GetWindowID(w);

    DEBUG(NAME_sdl,
	  Cprintf("Registered window %p with id %d\n", w, f->ws_id));
    succeed;
  }

  return errorPce(fr, NAME_xOpen, fr->display);
}

/**
 * Translate a SDL window id into a frame object.  For now this simply
 * walks all frames.  That may  actually be good enough, especially if
 * we would cache the frame that got the last event.
 *
 * @return Frame for id or NULL
 */

FrameObj
wsid_to_frame(SDL_WindowID id)
{ DisplayManager dm = TheDisplayManager();
  Cell c1;

  for_cell(c1, dm->members)
  { DisplayObj d = c1->value;
    Cell c2;

    for_cell(c2, d->frames)
    { FrameObj fr = c2->value;
      WsFrame f = sdl_frame(fr, false);
      if ( f && f->ws_id == id )
	return fr;
    }
  }

  fail;
}

static bool
frame_displayed(FrameObj fr, BoolObj val)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;
    send(sw, NAME_displayed, val, EAV);
  }

  return true;
}

#define Area2FRect(a)					\
  { (float)valInt(a->x), (float)valInt(a->y),		\
    (float)valInt(a->w), (float)valInt(a->h)		\
  }
#define AreaSize2FRect(a)				\
  { 0.0f, 0.0f,						\
    (float)valInt(a->w), (float)valInt(a->h)		\
  }

typedef struct
{ float x;
  float y;
} foffset;

static void
ws_draw_window(FrameObj fr, PceWindow sw, foffset *off)
{ WsFrame  wfr = fr->ws_ref;
  WsWindow wsw = sw->ws_ref;

  if ( wsw )
  { Area a = sw->area;
    SDL_FRect dstrect = Area2FRect(a);

    dstrect.x += off->x;
    dstrect.y += off->y;
    DEBUG(NAME_sdl,
	  Cprintf("Draw %s in %s %d %d %d %d\n",
		  pp(sw), pp(fr),
		  valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h)));

    int width    = cairo_image_surface_get_width(wsw->backing);
    int height   = cairo_image_surface_get_height(wsw->backing);
    int stride   = cairo_image_surface_get_stride(wsw->backing);
    Uint32 *data = (Uint32 *)cairo_image_surface_get_data(wsw->backing);
    SDL_Surface *sdl_surf = SDL_CreateSurfaceFrom(width, height,
						  SDL_PIXELFORMAT_ARGB8888,
						  data, stride);
    SDL_Texture *tex = SDL_CreateTextureFromSurface(wfr->ws_renderer,
						    sdl_surf);
    SDL_DestroySurface(sdl_surf);
    SDL_RenderTexture(wfr->ws_renderer, tex, NULL, &dstrect);
    SDL_DestroyTexture(tex);

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { foffset off2;
      off2.x = off->x + (float)valInt(sw->area->x);
      off2.y = off->y + (float)valInt(sw->area->y);
      WindowDecorator dw = (WindowDecorator)sw;
      ws_draw_window(fr, dw->window, &off2);
    }
    if ( notNil(sw->subwindows) && !emptyChain(sw->subwindows) )
    { Cell cell;

      for_cell(cell, sw->subwindows)
      { PceWindow sub = cell->value;
	PceWindow me = DEFAULT;
	Int x, y;
	get_absolute_xy_graphical((Graphical)sub, (Device *)&me, &x, &y);
	assert(me == sw);

	foffset off2;
	off2.x = off->x + (float)(valInt(sw->area->x) + valInt(x));
	off2.y = off->y + (float)(valInt(sw->area->y) + valInt(y));
	DEBUG(NAME_sdl,
	      Cprintf("Drawing subwindow %s of %s at %f,%f\n",
		      pp(sub), pp(sw), pp(me), off2.x, off2.y));

	ws_draw_window(fr, sub, &off2);
      }
    }
  }
}

static bool
ws_draw_frame(FrameObj fr)
{ if ( !ws_created_frame(fr) )
    false;

  WsFrame wfr = fr->ws_ref;

  DEBUG(NAME_sdl,
	Cprintf("BEGIN ws_draw_frame(%s)\n", pp(fr)));
  assert(instanceOfObject(fr->background, ClassColour));
  SDL_Color c = pceColour2SDL_Color(fr->background);
  SDL_SetRenderDrawColor(wfr->ws_renderer, c.r, c.g, c.b, c.a);
  SDL_RenderClear(wfr->ws_renderer);
  Cell cell;
  for_cell(cell, fr->members)
  { foffset off = {0.0f,0.0f};
    ws_draw_window(fr, cell->value, &off);
  }
  SDL_RenderPresent(wfr->ws_renderer);
  DEBUG(NAME_sdl,
	Cprintf("END ws_draw_frame(%s)\n", pp(fr)));

  return true;
}


void
ws_redraw_changed_frames(void)
{ if ( ChangedFrames && !emptyChain(ChangedFrames) )
  { Cell cell;

    for_cell(cell, ChangedFrames)
    { FrameObj fr = cell->value;
      ws_draw_frame(fr);
      deleteChain(ChangedFrames, fr);
    }
  }
}


/**
 * @see https://wiki.libsdl.org/SDL3/SDL_WindowEvent
 */

bool				/* true when processed */
sdl_frame_event(SDL_Event *ev)
{ FrameObj fr = wsid_to_frame(ev->window.windowID);

  if ( fr )
  { switch(ev->type)
    { case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
      { Code msg;

	if ( (msg = checkType(getValueSheet(fr->wm_protocols,
					    CtoName("WM_DELETE_WINDOW")),
			      TypeCode, fr)) )
	{ return forwardReceiverCode(msg, fr, MainWindow(fr), EAV);
	} else
	{ return send(fr, NAME_destroy, EAV);
	}
      }
      case SDL_EVENT_WINDOW_SHOWN:
	DEBUG(NAME_sdl, Cprintf("Mapped %s\n", pp(fr)));
	return frame_displayed(fr, ON);
      case SDL_EVENT_WINDOW_HIDDEN:
	return frame_displayed(fr, OFF);
      case SDL_EVENT_WINDOW_EXPOSED:
	RedrawDisplayManager(TheDisplayManager());
	return ws_draw_frame(fr);
      case SDL_EVENT_WINDOW_MOVED:
      { int new_x = ev->window.data1;
	int new_y = ev->window.data2;

	assign(fr->area, x, toInt(new_x));
	assign(fr->area, y, toInt(new_y));

	return true;
      }
      case SDL_EVENT_WINDOW_RESIZED:
      { int new_w = ev->window.data1;

	int new_h = ev->window.data2;

	if ( new_w != valInt(fr->area->w) ||
	     new_h != valInt(fr->area->h) )
	{ assign(fr->area, w, toInt(new_w));
	  assign(fr->area, h, toInt(new_h));

	  send(fr, NAME_resize, EAV);
	}

	return true;
      }
      case SDL_EVENT_WINDOW_FOCUS_GAINED:
	return send(fr, NAME_inputFocus, ON, EAV);
      case SDL_EVENT_WINDOW_FOCUS_LOST:
	return send(fr, NAME_inputFocus, OFF, EAV);
    }
  }

  return false;
}

/**
 * Realize the specified frame, making it visible on the display.
 *
 * @param fr Pointer to the FrameObj to realize.
 */
void
ws_realise_frame(FrameObj fr)
{
}

/**
 * Determine which window within the frame holds a specific point.
 *
 * @param fr Pointer to the FrameObj to examine.
 * @return The PceWindow holding the point, or NULL if none.
 */
PceWindow
ws_window_holding_point_frame(FrameObj fr)
{
    return NULL;
}

/**
 * Raise the specified frame above other windows.
 *
 * @param fr Pointer to the FrameObj to raise.
 */
void
ws_raise_frame(FrameObj fr)
{
}

/**
 * Lower the specified frame below other windows.
 *
 * @param fr Pointer to the FrameObj to lower.
 */
void
ws_lower_frame(FrameObj fr)
{
}

/**
 * Attach window manager protocols to the specified frame.
 *
 * @param fr Pointer to the FrameObj to attach protocols to.
 * @return SUCCEED on successful attachment; otherwise, FAIL.
 */
status
ws_attach_wm_prototols_frame(FrameObj fr)
{
    return SUCCEED;
}

/**
 * Set the frame to be aware of drag-and-drop operations.
 *
 * @param fr Pointer to the FrameObj to set.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
setDndAwareFrame(FrameObj fr)
{
    return SUCCEED;
}

/**
 * Set the cursor for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param cursor Pointer to the CursorObj to set.
 */
void
ws_frame_cursor(FrameObj fr, CursorObj cursor)
{
}

/**
 * Grab or release the pointer for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param grab Boolean indicating whether to grab (ON) or release (OFF)
 *        the pointer.
 * @param cursor Pointer to the CursorObj to use during the grab.
 */
void
ws_grab_frame_pointer(FrameObj fr, BoolObj grab, CursorObj cursor)
{
}

/**
 * Enable/disable the (virtual) keyboard for the window in
 * which gr is displayed.
 */

status
ws_enable_text_input(Graphical gr, BoolObj enable)
{ FrameObj fr = getFrameGraphical(gr);
  if ( fr )
  { WsFrame wfr = fr->ws_ref;

    if ( wfr && wfr->ws_window )
    { if ( isOn(enable) )
	return SDL_StartTextInput(wfr->ws_window);
      else
	return SDL_StopTextInput(wfr->ws_window);
    }
  }

  fail;
}

/**
 * Retrieve the bounding box of the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param x Pointer to store the x-coordinate.
 * @param y Pointer to store the y-coordinate.
 * @param w Pointer to store the width.
 * @param h Pointer to store the height.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h)
{
    return SUCCEED;
}

/**
 * Set the geometry of the frame using a specification string.
 *
 * @param fr Pointer to the FrameObj.
 * @param spec Name object containing the geometry specification.
 * @param mon Monitor object representing the target monitor.
 */
void
ws_x_geometry_frame(FrameObj fr, Name spec, Monitor mon)
{
}

/**
 * Set the geometry of the frame using explicit coordinates and dimensions.
 *
 * @param fr Pointer to the FrameObj.
 * @param x X-coordinate position.
 * @param y Y-coordinate position.
 * @param w Width of the frame.
 * @param h Height of the frame.
 * @param mon Monitor object representing the target monitor.
 */
void
ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h, Monitor mon)
{
}

/**
 * Set the border width for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param b Border width in pixels.
 */
void
ws_border_frame(FrameObj fr, int b)
{
}

/**
 * Set a busy cursor for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param c Pointer to the CursorObj representing the busy cursor.
 */
void
ws_busy_cursor_frame(FrameObj fr, CursorObj c)
{
}

/**
 * Set the background color or pattern for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param c Any object representing the background (e.g., color or pattern).
 */
void
ws_frame_background(FrameObj fr, Any c)
{
}

/**
 * Set the icon for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 */
void
ws_set_icon_frame(FrameObj fr)
{
}

/**
 * Set the icon label for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 */
void
ws_set_icon_label_frame(FrameObj fr)
{
}

/**
 * Set the position of the frame's icon.
 *
 * @param fr Pointer to the FrameObj.
 * @param x X-coordinate position.
 * @param y Y-coordinate position.
 */
void
ws_set_icon_position_frame(FrameObj fr, int x, int y)
{
}

/**
 * Retrieve the position of the frame's icon.
 *
 * @param fr Pointer to the FrameObj.
 * @param x Pointer to store the x-coordinate.
 * @param y Pointer to store the y-coordinate.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_get_icon_position_frame(FrameObj fr, int *x, int *y)
{
    return SUCCEED;
}

/**
 * Enable or disable modal behavior for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param val Boolean indicating whether to enable (TRUE) or disable (FALSE) modal behavior.
 */
void
ws_enable_modal(FrameObj fr, BoolObj val)
{
}

/**
 * Set the status of the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param Status of the frame.  One of `unmapped`, `hidden`,
 * `iconic`, `window` or `full_screen`
 */
void
ws_status_frame(FrameObj fr, Name status)
{ if ( fr->kind == NAME_popup )
  { if ( status == NAME_hidden )
      ws_uncreate_frame(fr);	/* TODO: also uncreate the windows? */
  }
}

/**
 * Set the specified frame to be topmost or not.
 *
 * @param fr Pointer to the FrameObj.
 * @param topmost Boolean indicating whether to set the frame as topmost (TRUE) or not (FALSE).
 */
void
ws_topmost_frame(FrameObj fr, BoolObj topmost)
{
}

/**
 * Set the label for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 */
void
ws_set_label_frame(FrameObj fr)
{
}

/**
 * Retrieve the image representation of the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @return Pointer to the Image object representing the frame.
 */
Image
ws_image_of_frame(FrameObj fr)
{
    return NULL;
}

/**
 * Set the specified frame as transient for another frame.
 *
 * @param fr Pointer to the FrameObj to set as transient.
 * @param fr2 Pointer to the FrameObj that owns the transient frame.
 */
void
ws_transient_frame(FrameObj fr, FrameObj fr2)
{
}

/**
 * Generate a PostScript representation of the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param iscolor Integer indicating whether to use color (non-zero) or not (zero).
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_postscript_frame(FrameObj fr, int iscolor)
{
    return SUCCEED;
}

/**
 * Retrieve the thread identifier associated with the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @return Integer representing the thread identifier.
 */
Int
ws_frame_thread(FrameObj fr)
{
    return 0;
}

/**
 * Enable or disable the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param enable Integer indicating whether to enable (non-zero) or disable (zero) the frame.
 * @return Integer status code.
 */
int
ws_enable_frame(FrameObj fr, int enable)
{
    return 0;
}
