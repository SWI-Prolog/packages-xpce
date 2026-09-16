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
#include "sdluserevent.h"
#include "sdlcursor.h"
#include <math.h>

#define MainWindow(fr)	     ( isNil(fr->members->head) ? (Any) fr : \
			       fr->members->head->value )

bool		ws_draw_frame(FrameObj fr);


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
 * Uncreate the windows  in a frame after we unmap  the frame.  We can
 * leave the window  backing in place (we could also  destroy it), but
 * we must delete the SDL texture  as that is connected to the frame's
 * texture.
 */

static void
uncreate_window_frame(PceWindow sw)
{ ASSERT_SDL_MAIN();
  WsWindow wsw = sw->ws_ref;

  if ( wsw && wsw->texture )		/* ws_ref is gone if the window was */
  { SDL_DestroyTexture(wsw->texture);	/* uncreated before the frame */
    wsw->texture = NULL;
  }

  if ( instanceOfObject(sw, ClassWindowDecorator) )
  { WindowDecorator dw = (WindowDecorator)sw;
    uncreate_window_frame(dw->window);
  }
  if ( notNil(sw->subwindows) && !emptyChain(sw->subwindows) )
  { Cell cell;

    for_cell(cell, sw->subwindows)
      uncreate_window_frame(cell->value);
  }
}

static void
uncreate_windows_frame(FrameObj fr)
{ Cell cell;
  for_cell(cell, fr->members)
  { uncreate_window_frame(cell->value);
  }
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
  { ASSERT_SDL_MAIN();
    deleteChain(ChangedFrames, fr);
    SDL_DestroyRenderer(f->ws_renderer);
    SDL_DestroyWindow(f->ws_window);
    unalloc(sizeof(*f), f);
    fr->ws_ref = NULL;
    uncreate_windows_frame(fr);
  }

  ws_event_destroyed_target(fr);
}

static SDL_Window *
sdl_parent_window(FrameObj fr, FrameObj *frp)
{ Any pfr = fr->transient_for;

  if ( isNil(pfr) )
    pfr = getAttributeObject(fr, NAME_parent);
  if ( pfr && instanceOfObject(pfr, ClassFrame) )
  { if ( frp )
      *frp = pfr;
    WsFrame pf = sdl_frame(pfr, false);
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
{ ASSERT_SDL_MAIN();
  SDL_Window *win = NULL;
  FrameObj pfr = NIL;
  SDL_Window *parent = sdl_parent_window(fr, &pfr);
  int x = valInt(fr->area->x);
  int y = valInt(fr->area->y);
  int w = valInt(fr->area->w);
  int h = valInt(fr->area->h);
  bool focusable = true;

  if ( fr->kind == NAME_popup && parent )
  { focusable = false;
    SDL_PropertiesID props = SDL_CreateProperties();
    SDL_SetPointerProperty(props, SDL_PROP_WINDOW_CREATE_PARENT_POINTER,
			   parent);
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_MENU_BOOLEAN, true);
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_HIDDEN_BOOLEAN, true);
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_FOCUSABLE_BOOLEAN, false);
#if defined(__APPLE__) && defined(SDL_PROP_WINDOW_CREATE_CONSTRAIN_POPUP_BOOLEAN)
    /* SDL on MacOS does not handle popup placement correctly on secondary displays */
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_CONSTRAIN_POPUP_BOOLEAN, false);
#endif
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_X_NUMBER, x);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_Y_NUMBER, y);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_WIDTH_NUMBER, w);
    SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_HEIGHT_NUMBER, h);
#if O_HDP
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_HIGH_PIXEL_DENSITY_BOOLEAN,
			   true);
#endif

    win = SDL_CreateWindowWithProperties(props);
    SDL_DestroyProperties(props);
#ifdef __APPLE__
    if ( win )
      SDL_SetWindowPosition(win, x, y);
#endif
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
    SDL_SetBooleanProperty(props, SDL_PROP_WINDOW_CREATE_HIDDEN_BOOLEAN, true);
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

    Area da = fr->display->area;	/* work_area is unreliable */

    if ( isOn(fr->placed) )
    {
#ifdef __WINDOWS__
      x += GetSystemMetrics(SM_CXBORDER);
      y += GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYCAPTION);
#endif
      x += valInt(da->x);
      y += valInt(da->y);
      DEBUG(NAME_frame,
	    Cprintf("%s: passing position %d,%d\n", pp(fr), x, y));
      SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_X_NUMBER, x);
      SDL_SetNumberProperty(props, SDL_PROP_WINDOW_CREATE_Y_NUMBER, y);
    } else
    { x = valInt(da->x) + (valInt(da->w)-w)/2;
      y = valInt(da->y) + (valInt(da->h)-h)/2;
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

    ws_draw_frame(fr);
    SDL_ShowWindow(win);
    if ( focusable )
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
 * Say that `sub` sits in the <-subwindows of `sw` while its <-device
 * chain says otherwise, naming what it does hang under.  A window that
 * is erased from its device, or moved to one held by another window,
 * should be uncreated and taken out of the chain; that it was not is a
 * bug in whoever moved it, but the drawing code is the wrong place to
 * die over one.  Reported once per window: this runs on every repaint.
 */

static void
report_stray_subwindow(PceWindow sw, PceWindow sub, PceWindow me)
{ static PceWindow reported;

  if ( sub == reported )
    return;
  reported = sub;

  Cprintf("xpce: %s is in the <-subwindows of %s, but its <-device "
	  "chain ends at %s (<-parent = %s).  Not drawing it.\n",
	  pp(sub), pp(sw), pp(me), pp(sub->parent));
  for(Graphical gr = (Graphical)sub; notNil(gr->device); gr = (Graphical)gr->device)
    Cprintf("\t%s is displayed on %s\n", pp(gr), pp(gr->device));
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
      if ( me != sw->parent )
      { report_stray_subwindow(sw->parent, sw, me);
	return false;
      }
      *ox += valNum(x);
      *oy += valNum(y);
      return ws_window_frame_position_(sw->parent, fr, ox, oy);
    }

    if ( instanceOfObject(sw->device, ClassWindowDecorator) )
    { *ox += valNum(sw->area->x);
      *oy += valNum(sw->area->y);

      return ws_window_frame_position_(sw->device, fr, ox, oy);
    }

    /* A pane lives on a device inside another window -- see class
     * tab_frame in library(tab_frame) -- and <-parent only says which
     * window it was created inside, which is nothing until it has been.
     * Walk the device chain to the window it is drawn in and go on from
     * there.
     */
    if ( notNil(sw->device) )
    { PceWindow me = DEFAULT;
      Int x, y;

      if ( get_absolute_xy_graphical((Graphical)sw, (Device *)&me, &x, &y) &&
	   instanceOfObject(me, ClassWindow) )
      { *ox += valNum(x);
	*oy += valNum(y);

	return ws_window_frame_position_(me, fr, ox, oy);
      }
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
{ ASSERT_SDL_MAIN();
  FrameObj fr = ctx;
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
{ ASSERT_SDL_MAIN();
  TileObj tile = getTileFrame(fr);

  if ( tile )
  { WsFrame wfr = fr->ws_ref;
    Colour fg = fr->display->foreground;
    SDL_Color c = pceColour2SDL_Color(fg);

    SDL_SetRenderDrawColor(wfr->ws_renderer, c.r, c.g, c.b, c.a);
    forResizeAreaTile(tile, ws_draw_resize_area_frame, fr);
  }
}


/**
 * Where the coordinate system of `sub`, a subwindow of `sw`, starts
 * relative to the origin of `sw`.
 *
 * get_absolute_xy_graphical() answers the position of `sub` itself,
 * which already includes its <-area.  The routines below add the area
 * again when they place the window, so take it out here.  The two are
 * the same only while a subwindow sits in the top-left corner of its
 * device, which is what class window_tab does and class tab_frame (see
 * library(tab_frame)) does not.
 *
 * @param sw  Window holding `sub` in its <-subwindows
 * @param sub The subwindow
 * @return `false` if `sub` does not hang under `sw` after all, in which
 *         case there is no offset to be had and it must not be drawn.
 */

static bool
subwindow_offset(PceWindow sw, PceWindow sub, float *ox, float *oy)
{ PceWindow me = DEFAULT;
  Int x, y;

  get_absolute_xy_graphical((Graphical)sub, (Device *)&me, &x, &y);
  if ( me != sw )
  { report_stray_subwindow(sw, sub, me);
    return false;
  }

  *ox = (float)(valInt(x) - valInt(sub->area->x));
  *oy = (float)(valInt(y) - valInt(sub->area->y));

  return true;
}


/* How far a window and everything drawn inside it is faded.  A window
 * that has scrollbars or a label is wrapped in a window_decorator and
 * the two are one thing to the user, so <-opacity of either fades both:
 * library(pane_frame) sets it on the pane, which is the window inside.
 * See ws_draw_window().
 */

static double
window_group_opacity(PceWindow sw)
{ double op = valNum(sw->opacity);

  if ( instanceOfObject(sw, ClassWindowDecorator) )
    op *= valNum(((WindowDecorator)sw)->window->opacity);

  return op;
}


/**
 * Draw one window of `fr` and the windows it holds.
 *
 * @param off Where the window sits in the frame.
 * @param opacity Alpha to draw this window and its children with; see
 *        window_group_opacity().
 */

static void
ws_draw_window(FrameObj fr, PceWindow sw, foffset *off, double opacity)
{ WsFrame  wfr = fr->ws_ref;
  WsWindow wsw = sw->ws_ref;

  if ( wsw )
  { ASSERT_SDL_MAIN();
    Area a = sw->area;

    /* A window may be laid out with no room at all: a dialog holding
     * only a menu_bar that is shown natively asks for no height -- see
     * the comment at non_empty_tiles() in src/win/tile.c.  It has
     * nothing to show, and drawing it anyway shows something.
     */

    if ( valInt(a->w) <= 0 || valInt(a->h) <= 0 )
      return;

    SDL_FRect dstrect = Area2FRect(a);
    float scale = SDL_GetWindowPixelDensity(wfr->ws_window);

    dstrect.x += off->x;
    dstrect.y += off->y;
    scaleFRect(dstrect, scale);
    DEBUG(NAME_sdl,
	  Cprintf("Draw %s in %s %d %d %d %d\n",
		  pp(sw), pp(fr),
		  valInt(a->x), valInt(a->y), valInt(a->w), valInt(a->h)));

    SDL_Color bg = pceColour2SDL_Color(sw->background);

    cairo_surface_flush(wsw->backing);
    int width    = cairo_image_surface_get_width(wsw->backing);
    int height   = cairo_image_surface_get_height(wsw->backing);
    int stride   = cairo_image_surface_get_stride(wsw->backing);
    Uint8 *data  = cairo_image_surface_get_data(wsw->backing);

    /* Uploading the  backing is  the expensive part  of drawing  a frame.
     * A new texture must be filled completely; an existing one only needs
     * the region the redraw changed.  See ws_dirty_window().
     */

    if ( !wsw->texture )
    { wsw->texture = SDL_CreateTexture(wfr->ws_renderer,
				       SDL_PIXELFORMAT_ARGB8888,
				       SDL_TEXTUREACCESS_STREAMING,
				       width, height);
      SDL_SetTextureBlendMode(wsw->texture, SDL_BLENDMODE_BLEND);
      SDL_UpdateTexture(wsw->texture, NULL, data, stride);
    } else
    { for(int i=0; i<wsw->ndirty; i++)
      { SDL_Rect *r = &wsw->dirty[i];

	SDL_UpdateTexture(wsw->texture, r,
			  data + (size_t)r->y*stride + (size_t)r->x*4,
			  stride);
      }
    }
    wsw->ndirty = 0;

    SDL_SetTextureAlphaModFloat(wsw->texture, (float)opacity);
    SDL_RenderTexture(wfr->ws_renderer, wsw->texture, NULL, &dstrect);
    if ( wfr->flash_end_ms && SDL_GetTicks() < wfr->flash_end_ms )
    { int lum = (int)(0.299f*bg.r + 0.587f*bg.g + 0.114f*bg.b);
      Uint8 v = lum > 128 ? 0 : 255;		/* dark on light, light on dark */
      SDL_SetRenderDrawBlendMode(wfr->ws_renderer, SDL_BLENDMODE_BLEND);
      SDL_SetRenderDrawColor(wfr->ws_renderer, v, v, v, 128);
      if ( wfr->flash_rect.w > 0.0f )
      { SDL_FRect isect;
	if ( SDL_GetRectIntersectionFloat(&dstrect, &wfr->flash_rect, &isect) )
	  SDL_RenderFillRect(wfr->ws_renderer, &isect);
      } else
      { SDL_RenderFillRect(wfr->ws_renderer, &dstrect);
      }
      SDL_SetRenderDrawBlendMode(wfr->ws_renderer, SDL_BLENDMODE_NONE);
    }

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { foffset off2;
      off2.x = off->x + valNum(sw->area->x);
      off2.y = off->y + valNum(sw->area->y);
      WindowDecorator dw = (WindowDecorator)sw;
      ws_draw_window(fr, dw->window, &off2, opacity);
    }
    if ( notNil(sw->subwindows) && !emptyChain(sw->subwindows) )
    { Cell cell;

      for_cell(cell, sw->subwindows)
      { PceWindow sub = cell->value;
	float sx, sy;

	if ( !subwindow_offset(sw, sub, &sx, &sy) )
	  continue;

	foffset off2;
	off2.x = off->x + (float)valInt(sw->area->x) + sx;
	off2.y = off->y + (float)valInt(sw->area->y) + sy;
	DEBUG(NAME_sdl,
	      Cprintf("Drawing subwindow %s of %s at %f,%f\n",
		      pp(sub), pp(sw), off2.x, off2.y));

	ws_draw_window(fr, sub, &off2, opacity*window_group_opacity(sub));
      }
    }
  }
}

/* The renderer lost the contents of its textures
 * (SDL_EVENT_RENDER_TARGETS_RESET) or the device holding them
 * (SDL_EVENT_RENDER_DEVICE_RESET).  We normally upload no more than
 * what a redraw changed, so the textures must be thrown away and
 * filled from the backing surfaces again.
 */

static void
reset_texture_window(PceWindow sw)
{ WsWindow wsw = sw->ws_ref;

  if ( wsw )
  { if ( wsw->texture )
    { ASSERT_SDL_MAIN();
      SDL_DestroyTexture(wsw->texture);
      wsw->texture = NULL;
    }
    ws_dirty_all_window(sw);
  }

  if ( instanceOfObject(sw, ClassWindowDecorator) )
    reset_texture_window(((WindowDecorator)sw)->window);
  if ( notNil(sw->subwindows) )
  { Cell cell;

    for_cell(cell, sw->subwindows)
      reset_texture_window(cell->value);
  }
}


static void
ws_reset_textures(void)
{ DisplayManager dm = TheDisplayManager();
  Cell c1;

  for_cell(c1, dm->members)
  { DisplayObj d = c1->value;
    Cell c2;

    for_cell(c2, d->frames)
    { FrameObj fr = c2->value;
      Cell c3;

      if ( !ws_created_frame(fr) )
	continue;

      for_cell(c3, fr->members)
	reset_texture_window(c3->value);

      ws_draw_frame(fr);
    }
  }
}


bool
ws_draw_frame(FrameObj fr)
{ if ( !ws_created_frame(fr) )
    false;

  WsFrame wfr = fr->ws_ref;
  ASSERT_SDL_MAIN();

  DEBUG(NAME_sdl,
	Cprintf("BEGIN ws_draw_frame(%s)\n", pp(fr)));
  assert(instanceOfObject(fr->background, ClassColour));
  SDL_Color c = pceColour2SDL_Color(fr->background);
  SDL_SetRenderDrawColor(wfr->ws_renderer, c.r, c.g, c.b, c.a);
  SDL_RenderClear(wfr->ws_renderer);
  Cell cell;
  for_cell(cell, fr->members)
  { foffset off = {0.0f,0.0f};
    PceWindow sw = cell->value;

    ws_draw_window(fr, sw, &off, window_group_opacity(sw));
  }
  ws_draw_resize_frame(fr);
  SDL_RenderPresent(wfr->ws_renderer);
  DEBUG(NAME_sdl,
	Cprintf("END ws_draw_frame(%s)\n", pp(fr)));

  return true;
}


Uint32 SDLCALL
flash_end_callback(void *userdata, SDL_TimerID id, Uint32 interval)
{ FrameObj fr = userdata;

  if ( onFlag(fr, F_FREEING|F_FREED) )
    return 0;

  SDL_Event ev;
  SDL_zero(ev);
  ev.type       = MY_EVENT_FLASH_END;
  ev.user.data1 = fr;
  addCodeReference(fr);			/* released by MY_EVENT_FLASH_END handler */
  SDL_PushEvent(&ev);
  return 0;				/* one-shot */
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


		 /*******************************
		 *	    LIVE RESIZE		*
		 *******************************/

/* On MacOS and Windows the OS runs a modal event loop while the user
 * drags a window border (Cocoa's resize tracking loop, Win32's
 * WM_ENTERSIZEMOVE loop).  Our main loop is blocked inside
 * SDL_WaitEvent() (see ws_dispatch()) for the whole drag, so the
 * SDL_EVENT_WINDOW_RESIZED and SDL_EVENT_WINDOW_EXPOSED events SDL
 * generates from inside that loop are queued but not processed: the
 * window only gets its new content after the user releases the mouse.
 * MacOS meanwhile stretches the last Metal drawable and Windows pads
 * with black.  X11 and Wayland have no modal loop, so there resizing
 * is immediate.
 *
 * SDL_AddEventWatch() callbacks run at SDL_PushEvent() time, i.e., on
 * the main thread from inside the modal loop, which is our only chance
 * to react.  SDL meets us half way: while a live resize is in progress
 * it runs a ~60Hz timer calling SDL_OnWindowLiveResizeUpdate(), which
 * posts SDL_EVENT_WINDOW_EXPOSED for applications that (like us) do not
 * use the SDL_AppIterate() callback API.
 *
 * Note that events handled here are still added to the queue.  We
 * record the timestamp of the last one we processed in the frame so
 * that sdl_live_resize_handled() can drop them when the main loop gets
 * to run again.  Without that, a three second drag ends in a replay of
 * some 200 full repaints.
 */

static int in_live_resize = 0;		/* do not recurse */

static bool
live_resize_event(const SDL_Event *ev)
{ return ( ev->type == SDL_EVENT_WINDOW_RESIZED ||
	   ev->type == SDL_EVENT_WINDOW_EXPOSED );
}


static bool SDLCALL
live_resize_watch(void *closure, SDL_Event *ev)
{ (void)closure;

  if ( !live_resize_event(ev) ||
       !SDL_IsMainThread() ||	/* watches may be called from any thread */
       in_live_resize )
    return true;

  in_live_resize++;
  if ( pceMTTryLock() )		/* blocking would freeze the modal loop */
  { FrameObj fr = wsid_to_frame(ev->window.windowID);
    WsFrame wfr = fr ? fr->ws_ref : NULL;

    if ( wfr && ws_created_frame(fr) )
    { AnswerMark mark;

      markAnswerStack(mark);
      if ( ev->type == SDL_EVENT_WINDOW_RESIZED )
	sdl_frame_event(ev);	/* update the area and run the tile layout */
      RedrawDisplayManager(TheDisplayManager());
      if ( ChangedFrames )
	deleteChain(ChangedFrames, fr);	/* paint it here and now, rather */
      ws_draw_frame(fr);		/* than through WM_PAINT on Windows */
      ws_redraw_changed_frames();	/* other frames, if any */
      rewindAnswerStack(mark, NIL);
      wfr->live_ts = ev->common.timestamp;
    }
    pceMTUnlock();
  }
  in_live_resize--;

  return true;			/* ignored for event watches */
}


void
sdl_start_live_resize_watch(void)
{ SDL_AddEventWatch(live_resize_watch, NULL);
}


/**
 * Did live_resize_watch() already deal with this event?  Called from
 * the normal dispatch loop with the xpce lock held.
 *
 * @return true if the event may be discarded.
 */

bool
sdl_live_resize_handled(const SDL_Event *ev)
{ if ( live_resize_event(ev) )
  { FrameObj fr = wsid_to_frame(ev->window.windowID);
    WsFrame wfr = fr ? fr->ws_ref : NULL;

    if ( wfr && wfr->live_ts && ev->common.timestamp <= wfr->live_ts )
      return true;
  }

  return false;
}


/**
 * @see https://wiki.libsdl.org/SDL3/SDL_WindowEvent
 */

bool				/* true when processed */
sdl_frame_event(SDL_Event *ev)
{ if ( ev->type == SDL_EVENT_RENDER_TARGETS_RESET ||
       ev->type == SDL_EVENT_RENDER_DEVICE_RESET )
  { ws_reset_textures();
    return true;
  }

  FrameObj fr = wsid_to_frame(ev->window.windowID);

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
	WsFrame wfr = fr->ws_ref;
	if ( wfr )
	{ SDL_DisplayID did = SDL_GetDisplayForWindow(wfr->ws_window);
	  DisplayObj dsp = dsp_id_to_display(did);
	  if ( dsp && dsp != fr->display )
	  { DEBUG(NAME_display, Cprintf("Opened %s on %s\n", pp(fr), pp(dsp)));
	    assign(fr, display, dsp);
	  }
	}
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
	Area da = fr->display->area;

	new_x -= valInt(da->x);
	new_y -= valInt(da->y);

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
      { PceWindow sw = ws_grabbing_window();

	ws_menubar_activate_frame(fr);	/* show this frame's native menu */
	if ( sw )
	{ FrameObj fr2 = getFrameWindow(sw, OFF);

	  DEBUG(NAME_keyboard,
		Cprintf("Input focus on %s (grabbing=%s on %s)\n",
			pp(fr), pp(sw), pp(fr2)));

	  if ( fr2 != fr )
	  { WsFrame wfr = fr->ws_ref;
	    SDL_StartTextInput(wfr->ws_window);
	    return true;
	  }
	} else
	{ DEBUG(NAME_keyboard,
		Cprintf("Input focus on %s (not grabbing)\n",
			pp(fr)));
	}
	return send(fr, NAME_inputFocus, ON, EAV);
      }
      case SDL_EVENT_WINDOW_FOCUS_LOST:
      { PceWindow sw = ws_grabbing_window();
	DEBUG(NAME_keyboard, Cprintf("Input focus lost for %s (grabbing=%s)\n",
				     pp(fr), pp(sw)));
	if ( sw && getFrameWindow(sw, OFF) != fr )
	{ WsFrame wfr = fr->ws_ref;
	  SDL_StopTextInput(wfr->ws_window);
	  return true;
	}
	return send(fr, NAME_inputFocus, OFF, EAV);
      }
      case SDL_EVENT_WINDOW_MOUSE_LEAVE:
	ws_pointer_left_frame(fr);
	return true;
      case SDL_EVENT_WINDOW_DISPLAY_CHANGED:
      { DisplayObj new_display = dsp_id_to_display(ev->window.data1);
	DEBUG(NAME_display, Cprintf("%s moved to %s\n",
				    pp(fr), pp(new_display)));
	return send(fr, NAME_display, new_display, EAV);
      }
    }
  }

  return false;
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
  { ASSERT_SDL_MAIN();
    SDL_RaiseWindow(wfr->ws_window);
  }
}

/**
 * Does the window system consider this frame to have the keyboard?
 *
 * `frame <-input_focus' is kept up to date from the FOCUS_GAINED and
 * FOCUS_LOST events, which can be missed: they are not sent when the
 * window that has the focus already had it.  This is the truth to fall
 * back on.
 *
 * @param fr Pointer to the FrameObj to test.
 * @return true if the frame holds the keyboard focus.
 */
bool
ws_frame_has_input_focus(FrameObj fr)
{ WsFrame wfr = fr->ws_ref;

  if ( !SDL_IsMainThread() )	/* only the main thread may ask; taking */
    return false;		/* our own word for it is the safe answer */

  return ( wfr && wfr->ws_window &&
	   SDL_GetKeyboardFocus() == wfr->ws_window );
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
{ if ( ws_busy_cursor() )               /* covers every frame */
    return;

  SDL_Cursor *c = pceCursor2SDL_Cursor(cursor);
  ASSERT_SDL_MAIN();
  SDL_SetCursor(c ? c : SDL_GetDefaultCursor()); /* the frame may have none */
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
    { ASSERT_SDL_MAIN();
      DEBUG(NAME_keyboard,
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
 * @param dsp Display object representing the target monitor.
 */
#define MIN_VISIBLE 32			/* pixels that must be visible */
#define WIN_NOMOVE 0x1
#define WIN_NOSIZE 0x2

void
ws_x_geometry_frame(FrameObj fr, Name spec, DisplayObj dsp)
{ char *s = strName(spec);
  int x, y, w, h, w0, h0;
  int eh;
  int dw, dh;
  int flags = 0;
  char signx[10], signy[10];
  bool ok = false;
  Int X,Y,W,H;
  int offX=0;			/* window manager frame offset */

  if ( isDefault(dsp) )
  { char *e = strchr(s, '@');
    int n;

    if ( e )
      n = atoi(e+1);
    else
      n = 1;

    if ( !(dsp=getMemberDisplayManager(TheDisplayManager(), toInt(n))) )
      dsp = fr->display;
  }

  Area a = dsp->area;		/* work-area seems unreliable */
  dw = valInt(a->w);
  dh = valInt(a->h);

  if ( !ws_frame_bb(fr, &x, &y, &w0, &h0) )
    return;

  x -= valInt(a->x);			/* relative to display origin */
  y -= valInt(a->y);
  DEBUG(NAME_geometry,
	Cprintf("%s at %d,%d,%d,%d on %s\n",
		pp(fr), x, y, w0, h0, pp(dsp)));

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
    assign(fr, placed, ON);
  }
  if ( !(flags & WIN_NOSIZE) )
  { W = toInt(w);
    H = toInt(h);
  }

  send(fr, NAME_set, X, Y, W, H, dsp, EAV);
}

/**
 * Set the geometry of the frame using explicit coordinates and dimensions.
 *
 * @param fr Pointer to the FrameObj.
 * @param x X-coordinate position.
 * @param y Y-coordinate position.
 * @param w Width of the frame.
 * @param h Height of the frame.
 * @param dsp Display object representing the target monitor.
 */
status
ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h, DisplayObj dsp)
{ WsFrame wsf = fr->ws_ref;

  if ( wsf )
  { if ( notDefault(w) || notDefault(h) )
    { int iw = isDefault(w) ? valInt(fr->area->w) : valInt(w);
      int ih = isDefault(h) ? valInt(fr->area->h) : valInt(h);

#if O_HDPX
      float scale = ws_pixel_density_display(fr);
      iw = iw/scale; ih = ih/scale;
#endif
      DEBUG(NAME_set,
	    Cprintf("SDL_SetWindowSize(%s, %d, %d)\n",
		    pp(fr), iw, ih));
      ASSERT_SDL_MAIN();
      if ( !SDL_SetWindowSize(wsf->ws_window, iw, ih) )
	Cprintf("Could not set size of %s: %s\n",
		pp(fr), SDL_GetError());
    }

    if ( notDefault(x) || notDefault(y) )
    { int ix = isDefault(x) ? valInt(fr->area->x) : valInt(x);
      int iy = isDefault(y) ? valInt(fr->area->y) : valInt(y);

      if ( notDefault(dsp) )
      { ix += valInt(dsp->area->x);
	iy += valInt(dsp->area->y);
      }

#if O_HDPX
      float scale = ws_pixel_density_display(fr);
      ix = ix/scale; iy = iy/scale;
#endif
      DEBUG(NAME_set,
	    Cprintf("SDL_SetWindowPosition(%s, %d, %d)\n",
		    pp(fr), ix, iy));
      ASSERT_SDL_MAIN();
      if ( !SDL_SetWindowPosition(wsf->ws_window, ix, iy) )
      { DEBUG(NAME_set,
	      Cprintf("Could not set position of %s: %s\n",
		      pp(fr), SDL_GetError()));
      }
    }
  }

  succeed;
}

/**
 * Set a busy cursor for the specified frame.
 *
 * @param fr Pointer to the FrameObj.
 * @param c Pointer to the CursorObj representing the busy cursor.
 */
void
ws_busy_cursor_frame(FrameObj fr, CursorObj c)
{ if ( isDefault(c) )
    c = getClassVariableValueObject(fr, NAME_busyCursor);

  ws_set_busy_cursor(c);
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
      ASSERT_SDL_MAIN();
      SDL_SetWindowFullscreen(wfr->ws_window, status == NAME_fullScreen);
    } else
    { assign(fr, status, status);
      ws_create_frame(fr);
    }
  }
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
  { ASSERT_SDL_MAIN();
    SDL_SetWindowTitle(wfr->ws_window, nameToUTF8(fr->label));
  }
}

/**
 * Recursively composite a window's cairo backing onto a target cairo context.
 * Mirrors the layout logic of ws_draw_window(), handling WindowDecorator and
 * subwindows.
 *
 * @param cr    Target cairo context (frame-size surface).
 * @param sw    The window to composite.
 * @param ox    Accumulated x offset in logical coords (from parent).
 * @param oy    Accumulated y offset in logical coords (from parent).
 * @param scale Pixel density multiplier.
 */
static void
composite_window_to_cairo(cairo_t *cr, PceWindow sw,
			   float ox, float oy, float scale, double opacity)
{ WsWindow wsw = sw->ws_ref;
  if ( !wsw || !wsw->backing )
    return;
  if ( valInt(sw->area->w) <= 0 ||	/* nothing to show: see */
       valInt(sw->area->h) <= 0 )	/* ws_draw_window() */
    return;

  float wx = (ox + valInt(sw->area->x)) * scale;
  float wy = (oy + valInt(sw->area->y)) * scale;
  cairo_surface_flush(wsw->backing);
  cairo_set_source_surface(cr, wsw->backing, wx, wy);
  cairo_paint_with_alpha(cr, opacity);

  if ( instanceOfObject(sw, ClassWindowDecorator) )
  { WindowDecorator dw = (WindowDecorator)sw;
    composite_window_to_cairo(cr, dw->window,
			      ox + valNum(sw->area->x),
			      oy + valNum(sw->area->y),
			      scale, opacity);
  }
  if ( notNil(sw->subwindows) && !emptyChain(sw->subwindows) )
  { Cell cell;
    for_cell(cell, sw->subwindows)
    { PceWindow sub = cell->value;
      float sx, sy;

      if ( !subwindow_offset(sw, sub, &sx, &sy) )
	continue;
      composite_window_to_cairo(cr, sub,
				ox + valNum(sw->area->x) + sx,
				oy + valNum(sw->area->y) + sy,
				scale, opacity*window_group_opacity(sub));
    }
  }
}


/**
 * A cairo surface of `pw' x `ph' device pixels to composite an image
 * of a frame or of a window onto, filled with `background'.  The two
 * <-image methods differ only in what they put on it and how big it
 * is; pixel_image_start() and pixel_image_finish() are the rest.
 *
 * @return The context to draw on, or NULL if the surface could not be
 *         created.  `surf' is set to the surface it draws on, which
 *         pixel_image_finish() hands to the Image.
 */
static cairo_t *
pixel_image_start(cairo_surface_t **surf, int pw, int ph, Any background)
{ cairo_surface_t *s = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, pw, ph);

  if ( !s )
    return NULL;

  d_init_surface(s, background);
  cairo_t *cr = cairo_create(s);
  if ( !cr )
  { cairo_surface_destroy(s);
    return NULL;
  }

  *surf = s;
  return cr;
}

/**
 * Wrap the surface drawn by pixel_image_start() in an xpce Image,
 * which takes it over.  Destroys the context either way.
 *
 * @return The Image, or NULL if it could not be created.
 */
static Image
pixel_image_finish(cairo_surface_t *surf, cairo_t *cr, int pw, int ph)
{ cairo_destroy(cr);

  Image image = newObject(ClassImage, NIL, EAV);
  if ( !image )
  { cairo_surface_destroy(surf);
    return NULL;
  }
  assign(image, kind,    NAME_pixmap);
  assign(image->size, w, toInt(pw));
  assign(image->size, h, toInt(ph));
  image->ws_ref = surf;

  return image;
}

/**
 * Retrieve the image representation of the specified frame.
 * Composites all window backing cairo surfaces onto a new frame-size
 * cairo surface and wraps it in an xpce Image object.
 *
 * @param fr Pointer to the FrameObj.
 * @return Pointer to the Image object representing the frame, or NULL on failure.
 */
Image
ws_image_of_frame(FrameObj fr)
{ if ( !ws_created_frame(fr) )
    return NULL;

  WsFrame wfr   = fr->ws_ref;
  float   scale = SDL_GetWindowPixelDensity(wfr->ws_window);
  int     fw    = (int)(valInt(fr->area->w) * scale);
  int     fh    = (int)(valInt(fr->area->h) * scale);

  cairo_surface_t *surf;
  cairo_t *cr = pixel_image_start(&surf, fw, fh, fr->background);
  if ( !cr )
    return NULL;

  Cell cell;
  for_cell(cell, fr->members)
  { PceWindow member = cell->value;

    composite_window_to_cairo(cr, member, 0.0f, 0.0f, scale,
			      window_group_opacity(member));
  }

  return pixel_image_finish(surf, cr, fw, fh);
}

/**
 * Retrieve the image representation of the specified window: the same
 * pixels <-image of its frame holds for it, on a surface of the
 * window's own size and with the window at its origin.  A window
 * decorator and any subwindows come along, as they do for the frame.
 *
 * @param sw Pointer to the PceWindow.
 * @return Pointer to the Image object representing the window, or NULL
 *         on failure.
 */
Image
ws_image_of_window(PceWindow sw)
{ FrameObj fr;

  if ( !ws_created_window(sw) ||
       !(fr=getFrameWindow(sw, OFF)) ||
       !ws_created_frame(fr) )
    return NULL;

  WsFrame wfr   = fr->ws_ref;
  float   scale = SDL_GetWindowPixelDensity(wfr->ws_window);
  int     ww    = (int)(valInt(sw->area->w) * scale);
  int     wh    = (int)(valInt(sw->area->h) * scale);

  cairo_surface_t *surf;
  cairo_t *cr = pixel_image_start(&surf, ww, wh, sw->background);
  if ( !cr )
    return NULL;

  /* composite_window_to_cairo() places the window at its position in
   * the frame; the offset below takes that back out, so the window
   * lands on the origin of a surface of its own size.
   */
  composite_window_to_cairo(cr, sw,			/* an image of a window */
			    -(float)valInt(sw->area->x),	/* is not faded */
			    -(float)valInt(sw->area->y),
			    scale, 1.0);

  return pixel_image_finish(surf, cr, ww, wh);
}
