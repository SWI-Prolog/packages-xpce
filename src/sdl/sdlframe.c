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
#include "sdlframe.h"
#include "sdlwindow.h"
#include "sdlcolour.h"
#include "sdlevent.h"
#include "sdlcursor.h"
#include <math.h>

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

  ws_event_destroyed_target(fr);
}

static SDL_Window *
sdl_parent_window(FrameObj fr)
{ Any pfr = fr->transient_for;

  if ( isNil(pfr) )
    pfr = getAttributeObject(fr, NAME_parent);
  if ( pfr && instanceOfObject(pfr, ClassFrame) )
  { WsFrame pf = sdl_frame(pfr, false);
    if ( pf )
      return pf->ws_window;
  }

  return NULL;
}

/**
 * Create the specified frame.
 *
 * @param fr Pointer to the FrameObj to create.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_frame(FrameObj fr)
{ SDL_Window *win = NULL;
  SDL_Window *parent = sdl_parent_window(fr);
  int x = valInt(fr->area->x);
  int y = valInt(fr->area->y);
  int w = valInt(fr->area->w);
  int h = valInt(fr->area->h);

  if ( fr->kind == NAME_popup && parent )
  { SDL_WindowFlags flags = 0;

#if O_HDP
    flags = SDL_WINDOW_HIGH_PIXEL_DENSITY;
    //float scale = SDL_GetWindowPixelDensity(parent);
    //x = x/scale; y = y/scale; w = w/scale; h = h/scale;
#endif
    DEBUG(NAME_popup,
	  Cprintf("Creating popup frame %s %dx%d\n", pp(fr), w, h));
    flags |= SDL_WINDOW_POPUP_MENU;
    win = SDL_CreatePopupWindow(parent, x, y, w, h, flags);
  } else
  {
#if O_HDPX
    float scale = ws_pixel_density_display(fr);
    DEBUG(NAME_sdl, Cprintf("%s: scale = %.2f\n", pp(fr), scale));
    x = x/scale; y = y/scale; w = w/scale; h = h/scale;
#endif

    DEBUG(NAME_sdl, Cprintf("Create %s as transient for %p at %d %d %dx%d\n",
			    pp(fr), parent, x, y, w, h));


    SDL_PropertiesID props = SDL_CreateProperties();
    SDL_SetStringProperty(props, SDL_PROP_WINDOW_CREATE_TITLE_STRING,
			  nameToUTF8(fr->label));
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_WIDTH_NUMBER, w);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_HEIGHT_NUMBER, h);
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_RESIZABLE_BOOLEAN,
			   fr->can_resize == ON);
#if O_HDP
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_HIGH_PIXEL_DENSITY_BOOLEAN,
			   true);
#endif
    if ( parent )
    { SDL_SetPointerProperty(props, SDL_PROP_WINDOW_CREATE_PARENT_POINTER,
			     parent);
    }
    if ( fr->placed == ON )
    {
#ifdef __WINDOWS__
      x += GetSystemMetrics(SM_CXBORDER);
      y += GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYCAPTION);
#endif
      SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_X_NUMBER, x);
      SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_Y_NUMBER, y);
    }
    win = SDL_CreateWindowWithProperties(props);
    SDL_DestroyProperties(props);
  }

  if ( win )
  { SDL_Renderer *renderer = SDL_CreateRenderer(win, NULL);
    assert(renderer);
    SDL_RenderPresent(renderer); /* Probably temporary */

    WsFrame f = sdl_frame(fr, true);
    f->ws_window = win;
    f->ws_renderer = renderer;
    f->ws_id = SDL_GetWindowID(win);
#ifdef __WINDOWS__
    SDL_PropertiesID props = SDL_GetWindowProperties(win);
    f->hwnd = SDL_GetPointerProperty(
      props,
      SDL_PROP_WINDOW_WIN32_HWND_POINTER,
      NULL);
#endif

    DEBUG(NAME_sdl,
	  Cprintf("Registered window %p with id %d\n", win, f->ws_id));

    SDL_RaiseWindow(win);

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

/**
 * Find  the x,y  offset of  a window,  possibly the  frame itself,  a
 * direct window, a  window inside a decorator or a  subwindow of some
 * other window, relative to the frame.
 *
 * @returns `false` if `window` is not displayed on `fr`
 * @todo  We   should  unify  the  subwindow   notion  between  window
 * decorators and "normal" subwindows.
 */

static bool
ws_window_frame_position_(Any window, FrameObj fr, float *ox, float *oy)
{ if ( window == fr )
    return true;
  if ( instanceOfObject(window, ClassFrame) )
    return false;

  if ( instanceOfObject(window, ClassWindow) )
  { PceWindow sw = window;
    if ( notNil(sw->frame) )
    { if ( sw->frame == fr )
      { *ox += valNum(sw->area->x);
	*oy += valNum(sw->area->y);
	return true;
      }
      return false;
    }

    if ( notNil(sw->parent) )
    { PceWindow me = DEFAULT;
      Int x, y;
      get_absolute_xy_graphical((Graphical)sw, (Device *)&me, &x, &y);
      assert(me == sw->parent);
      *ox += valNum(x);
      *oy += valNum(y);
      return ws_window_frame_position_(sw->parent, fr, ox, oy);
    }

    if ( instanceOfObject(sw->device, ClassWindowDecorator) )
    { *ox += valNum(sw->area->x);
      *oy += valNum(sw->area->y);

      return ws_window_frame_position_(sw->device, fr, ox, oy);
    }
  }

  Cprintf("ws_window_frame_position(%s) failed\n", pp(window));
  return false;
}

bool
ws_window_frame_position(Any window, FrameObj fr, float *ox, float *oy)
{ float x = *ox, y = *oy;
  if ( ws_window_frame_position_(window, fr, &x, &y) )
  { *ox = x;
    *oy = y;
    return true;
  }

  return false;
}

#define Area2FRect(a)			\
  { valNum(a->x), valNum(a->y),		\
    valNum(a->w), valNum(a->h)		\
  }
#define AreaSize2FRect(a)		\
  { 0.0f, 0.0f,				\
    valNum(a->w), valNum(a->h)		\
  }

#define scaleFRect(r, scale)		\
  do					\
  { r.x *= scale;			\
    r.y *= scale;			\
    r.w *= scale;			\
    r.h *= scale;			\
  } while(0)


typedef struct
{ float x;
  float y;
} foffset;

static void*
ws_draw_resize_area_frame(Any ctx, TileObj t, Int x, Int y, Int w, Int h)
{ FrameObj fr = ctx;
  WsFrame wfr = fr->ws_ref;
  float x1, y1, x2, y2;

  //Cprintf("Resize area %s: %d %d %d %d\n", pp(fr),
  //valInt(x), valInt(y), valInt(w), valInt(h));

  if ( t->super->orientation == NAME_horizontal )
  { x1 = valNum(x) + valNum(w)/2.0;
    y1 = valNum(y);
    x2 = x1;
    y2 = valInt(y) + valNum(h);
  } else
  { x1 = valNum(x);
    y1 = valNum(y) + valNum(h)/2.0;
    x2 = valNum(x) + valNum(w);
    y2 = y1;
  }
  float scale = SDL_GetWindowPixelDensity(wfr->ws_window);
  x1 = rintf(x1*scale);
  y1 = rintf(y1*scale);
  x2 = rintf(x2*scale);
  y2 = rintf(y2*scale);

  SDL_RenderLine(wfr->ws_renderer, x1, y1, x2, y2);

  return NULL;			/* continue */
}

static void
ws_draw_resize_frame(FrameObj fr)
{ TileObj tile = getTileFrame(fr);

  if ( tile )
  { WsFrame wfr = fr->ws_ref;
    Colour fg = fr->display->foreground;
    SDL_Color c = pceColour2SDL_Color(fg);

    SDL_SetRenderDrawColor(wfr->ws_renderer, c.r, c.g, c.b, c.a);
    forResizeAreaTile(tile, ws_draw_resize_area_frame, fr);
  }
}


static void
ws_draw_window(FrameObj fr, PceWindow sw, foffset *off)
{ WsFrame  wfr = fr->ws_ref;
  WsWindow wsw = sw->ws_ref;

  if ( wsw )
  { Area a = sw->area;
    SDL_FRect dstrect = Area2FRect(a);
    float scale = SDL_GetWindowPixelDensity(wfr->ws_window);

    dstrect.x += off->x;
    dstrect.y += off->y;
    scaleFRect(dstrect, scale);
    DEBUG(NAME_sdl,
	  Cprintf("Draw %s in %s %d %d %d %d\n",
		  pp(sw), pp(fr),
		  valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h)));

    SDL_Color  bg = pceColour2SDL_Color(sw->background);
    SDL_SetRenderDrawColor(wfr->ws_renderer, bg.r, bg.g, bg.b, bg.a);
    SDL_RenderRect(wfr->ws_renderer, &dstrect);

    cairo_surface_flush(wsw->backing);
    int width    = cairo_image_surface_get_width(wsw->backing);
    int height   = cairo_image_surface_get_height(wsw->backing);
    int stride   = cairo_image_surface_get_stride(wsw->backing);
    Uint32 *data = (Uint32 *)cairo_image_surface_get_data(wsw->backing);
    SDL_Surface *sdl_surf = SDL_CreateSurfaceFrom(width, height,
						  SDL_PIXELFORMAT_ARGB8888,
						  data, stride);
    if ( !wsw->texture )
      wsw->texture = SDL_CreateTexture(wfr->ws_renderer,
				       SDL_PIXELFORMAT_ARGB8888,
				       SDL_TEXTUREACCESS_STREAMING,
				       width, height);

    SDL_UpdateTexture(wsw->texture, NULL, data, stride);
    SDL_RenderTexture(wfr->ws_renderer, wsw->texture, NULL, &dstrect);
    SDL_DestroySurface(sdl_surf);

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { foffset off2;
      off2.x = off->x + valNum(sw->area->x);
      off2.y = off->y + valNum(sw->area->y);
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
  ws_draw_resize_frame(fr);
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
#if __WINDOWS__
      WsFrame wfr = fr->ws_ref;
      if ( wfr && wfr->hwnd )
      { DEBUG(NAME_sdl, Cprintf("Invalidate %p\n", wfr->hwnd));
	InvalidateRect(wfr->hwnd, NULL, FALSE);
      }
#else
      ws_draw_frame(fr);
#endif
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
					    NAME_WM_DELETE_WINDOW),
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
	//return frame_displayed(fr, OFF);
	return true;
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
      { int new_w, new_h;

#if O_HDPX
	WsFrame f = sdl_frame(fr, false);
	SDL_GetWindowSizeInPixels(f->ws_window, &new_w, &new_h);
#else
	new_w = ev->window.data1;
	new_h = ev->window.data2;
#endif

	if ( new_w != valInt(fr->area->w) ||
	     new_h != valInt(fr->area->h) )
	{ assign(fr->area, w, toInt(new_w));
	  assign(fr->area, h, toInt(new_h));

	  send(fr, NAME_resize, EAV);
	}

	return true;
      }
      case SDL_EVENT_WINDOW_FOCUS_GAINED:
	DEBUG(NAME_keyboard, Cprintf("Input focus on %s\n", pp(fr)));
	return send(fr, NAME_inputFocus, ON, EAV);
      case SDL_EVENT_WINDOW_FOCUS_LOST:
	DEBUG(NAME_keyboard, Cprintf("Input focus lost for %s\n", pp(fr)));
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
{ WsFrame wfr = fr->ws_ref;
  if ( wfr && wfr->ws_window )
    SDL_RaiseWindow(wfr->ws_window);
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
 * Set the cursor  shape for the specified window. In  SDL, the cursor
 * is global for the application, i.e., it is _not_ set for a window.
 *
 * @param sw Pointer to the PceWindow object.
 * @param cursor The CursorObj representing the new cursor shape.
 */
void
ws_frame_cursor(FrameObj fr, CursorObj cursor)
{ SDL_Cursor *c = pceCursor2SDL_Cursor(cursor);
  if ( c )
    SDL_SetCursor(c);
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
    { DEBUG(NAME_keyboard,
	    Cprintf("ws_enable_text_input() %s -> %s: %s\n",
		    pp(gr), pp(fr), pp(enable)));
      if ( isOn(enable) )
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
{ *x = valInt(fr->area->x);
  *y = valInt(fr->area->y);
  *w = valInt(fr->area->w);
  *h = valInt(fr->area->h);

  succeed;
}

/**
 * Set the geometry  of the frame using a  specification string.  This
 * is used by e.g. class `persistent_frame` for restoring the size and
 * position of a frame.  As SDL does not allow restoring the position,
 * most of this  is worthless and overly complicated.  We  keep it for
 * now.   Eventually  we  should   simplify  this  and  modernise  the
 * interface.
 *
 * @param fr Pointer to the FrameObj.
 * @param spec Name object containing the geometry specification.
 * @param mon Monitor object representing the target monitor.
 */
#define MIN_VISIBLE 32			/* pixels that must be visible */
#define WIN_NOMOVE 0x1
#define WIN_NOSIZE 0x2

void
ws_x_geometry_frame(FrameObj fr, Name spec, Monitor mon)
{ char *e, *s = strName(spec);
  int x, y, w, h, w0, h0;
  int eh;
  int dw, dh;
  int flags = 0;
  char signx[10], signy[10];
  bool ok = false;
  Int X,Y,W,H;
  int offX=0;			/* window manager frame offset */

  if ( isDefault(mon) && (e=strchr(s, '@')) )
  { int n = atoi(e+1);

    if ( !(mon = getNth0Chain(fr->display->monitors, toInt(n))) )
      mon = (Monitor)DEFAULT;
  }

  if ( instanceOfObject(mon, ClassMonitor) )
  { Area a = (notNil(mon->work_area) ? mon->work_area : mon->area);

    dw = valInt(a->w);
    dh = valInt(a->h);
  } else
  { dw = valInt(getWidthDisplay(fr->display));
    dh = valInt(getHeightDisplay(fr->display));
  }

  if ( !ws_frame_bb(fr, &x, &y, &w0, &h0) )
    return;
  w = w0;
  h = h0;
  eh = h - valInt(fr->area->h);		/* height of decorations */

  switch(sscanf(s, "%dx%d%[+-]%d%[+-]%d", &w, &h, signx, &x, signy, &y))
  { case 2:
      /*w += ew; h += eh;*/
      flags |= WIN_NOMOVE;
      ok = true;
      break;
    case 6:
      /*w += ew; h += eh;*/
      if ( signx[1] == '-' )
	x = -x;
      if ( signy[1] == '-' )
	y = -y;
      if ( signx[0] == '-' )
	x = dw - x - w - offX;
      if ( signy[0] == '-' )
	y = dh - y - h - eh;		/* why not offY */
      ok = true;
      break;
    default:				/* [<Sign>]X<Sign>Y */
      if ( sscanf(s, "%[+-]%d%[+-]%d", signx, &x, signy, &y) != 4 )
      { signx[0] = '+';
	if ( sscanf(s, "%d%[+-]%d", &x, signy, &y) != 3 )
	  break;
      }

      DEBUG(NAME_frame,
	    Cprintf("signx = %s, x = %d, signy = %s,"
		    "y = %d, w0 = %d, h0 = %d\n",
		    signx, x, signy, y, w0, h0));

      flags |= WIN_NOSIZE;
      if ( signx[1] == '-' )
	x = -x;
      if ( signy[1] == '-' )
	y = -y;
      if ( signx[0] == '-' )
	x = dw - x - w0 - offX;
      if ( signy[0] == '-' )
	y = dh - y - h0 - eh;
      ok = true;
      break;
  }

  if ( ok )
  { if ( y < 1 )			/* above the screen */
      y = 1;
    else if ( y > dh-MIN_VISIBLE )	/* below the screen */
      y = dh - MIN_VISIBLE;
    if ( x < 1 )			/* left of the screen */
      x = 1;
    else if ( x > dw-MIN_VISIBLE )	/* right of the screen */
      x = dw - MIN_VISIBLE;
  }

  X = Y = W = H = (Int)DEFAULT;
  if ( !(flags & WIN_NOMOVE) )
  { X = toInt(x);
    Y = toInt(y);
  }
  if ( !(flags & WIN_NOSIZE) )
  { W = toInt(w);
    H = toInt(h);
  }

  send(fr, NAME_set, X, Y, W, H, mon, EAV);
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
{ if ( status == NAME_unmapped ||
       status == NAME_hidden )
  { ws_uncreate_frame(fr);
  } else if ( status == NAME_window || status == NAME_fullScreen )
  { if ( ws_created_frame(fr) )
    { WsFrame wfr = fr->ws_ref;
      SDL_SetWindowFullscreen(wfr->ws_window, status == NAME_fullScreen);
    } else
    { assign(fr, status, status);
      ws_create_frame(fr);
    }
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
{ WsFrame wfr = fr->ws_ref;
  if ( wfr && wfr->ws_window )
  { SDL_SetWindowTitle(wfr->ws_window, nameToUTF8(fr->label));
  }
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
