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
#include "../../swipl/pcecall.h"
#include "sdlevent.h"
#include "sdlinput.h"
#include "sdlframe.h"
#include "sdltimer.h"
#include "sdlstream.h"
#ifdef HAVE_POLL
#include <poll.h>
#endif

#define InitAreaA	int ax = valInt(a->x), ay = valInt(a->y),	\
			    aw = valInt(a->w), ah = valInt(a->h)


static bool
xy_in_area(Area a, int x, int y)
{ InitAreaA;

  NormaliseArea(ax, ay, aw, ah);
  return ( x >= ax && x <= ax+aw &&
	   y >= ay && y <= ay+ah );
}

static void event_window(Any *target, int *x, int *y);

static bool
descent_to_window(Any *target, PceWindow sw, int *x, int *y)
{ if ( xy_in_area(sw->area, *x, *y) &&
       ws_created_window(sw) )
  { *x -= valInt(sw->area->x);
    *y -= valInt(sw->area->y);
    *target = sw;
    event_window(target, x, y);
    return true;
  }

  return false;
}

static void
event_window(Any *target, int *x, int *y)
{ if ( instanceOfObject(*target, ClassFrame) )
  { FrameObj fr = *target;
    Cell cell;

    for_cell(cell, fr->members)
    { if ( descent_to_window(target, cell->value, x, y) )
	return;
    }
  } else if ( instanceOfObject(*target, ClassWindowDecorator) )
  { WindowDecorator dm = *target;
    descent_to_window(target, dm->window, x, y);
  } else
  { assert(instanceOfObject(*target, ClassWindow));
    PceWindow sw = *target;
    if ( notNil(sw->subwindows) && !emptyChain(sw->subwindows) )
    { Cell cell;

      /* TBD: overlapping windows? */
      for_cell(cell, sw->subwindows)
      { PceWindow sub = cell->value;
	PceWindow me = DEFAULT;
	Int wx, wy;
	get_absolute_xy_graphical((Graphical)sub, (Device *)&me, &wx, &wy);
	assert(me == sw);
	if ( *x >= valInt(wx) && *x <= valInt(wx) + valInt(sub->area->w) &&
	     *y >= valInt(wy) && *y <= valInt(wy) + valInt(sub->area->h) )
	{ *x -= valInt(wx);
	  *y -= valInt(wy);
	  *target = sub;
	  event_window(target, x, y);
	  break;
	}
      }
    }
  }
}

static Name
button_to_name(bool press, Uint8 button)
{ switch( button )
  { case 1:	return press ? NAME_msLeftDown     : NAME_msLeftUp;
    case 2:	return press ? NAME_msMiddleDown   : NAME_msMiddleUp;
    case 3:	return press ? NAME_msRightDown    : NAME_msRightUp;
    case 4:	return press ? NAME_msButton4Down  : NAME_msButton4Up;
    case 5:	return press ? NAME_msButton5Down  : NAME_msButton5Up;
  }

  fail;
}

#define ShiftMask   (SDL_KMOD_LSHIFT|SDL_KMOD_RSHIFT)
#define ControlMask (SDL_KMOD_LCTRL|SDL_KMOD_RCTRL)
#define MetaMask    (SDL_KMOD_LALT|SDL_KMOD_RALT)

static Int
state_to_buttons(SDL_MouseButtonFlags flags, SDL_Keymod mod)
{ int r = 0;

  if ( flags & SDL_BUTTON_LMASK )  r |= BUTTON_ms_left;
  if ( flags & SDL_BUTTON_MMASK )  r |= BUTTON_ms_middle;
  if ( flags & SDL_BUTTON_RMASK )  r |= BUTTON_ms_right;
  if ( flags & SDL_BUTTON_X1MASK ) r |= BUTTON_ms_button4;
  if ( flags & SDL_BUTTON_X2MASK ) r |= BUTTON_ms_button5;
  if ( mod & ShiftMask )	   r |= BUTTON_shift;
  if ( mod & ControlMask )	   r |= BUTTON_control;
  if ( mod & MetaMask )            r |= BUTTON_meta;

  return toInt(r);
}

static Any
keycode_to_name(SDL_Event *event)
{ if ( event->key.key >= 32 && event->key.key < 128 )
  { if ( event->key.mod & MetaMask )
      return toInt(Meta(event->key.key));
    if ( event->key.mod & ControlMask )
      return toInt(Control(event->key.key));
  }

  switch(event->key.key)
  { case SDLK_RETURN:
    case SDLK_ESCAPE:
    case SDLK_TAB:
      return toInt(event->key.key);
    case SDLK_BACKSPACE:
      if ( event->key.mod & MetaMask )
	return toInt(Meta(Control('H')));
      return NAME_backspace;
    case SDLK_DELETE:
      return NAME_delete;
    case SDLK_PAUSE:	return NAME_pause;
    case SDLK_HOME:	return NAME_cursorHome;
    case SDLK_PAGEUP:	return NAME_pageUp;
    case SDLK_END:	return NAME_end;
    case SDLK_PAGEDOWN:	return NAME_pageDown;
    case SDLK_RIGHT:	return NAME_cursorRight;
    case SDLK_LEFT:	return NAME_cursorLeft;
    case SDLK_DOWN:	return NAME_cursorDown;
    case SDLK_UP:	return NAME_cursorUp;
    case SDLK_AT:	if ( event->key.mod & ControlMask ) return ZERO;
  }

  fail;
}

/**
 * @see https://wiki.libsdl.org/SDL3/SDL_Event
 */

static SDL_Keymod lastmod = SDL_KMOD_NONE;
static Any grabbing_window = NIL;
static Any mouse_tracking_window = NIL;
static Uint8 mouse_tracking_button;

void
ev_event_grab_window(Any window)
{ grabbing_window = window;
}

void
ws_event_destroyed_target(Any window)
{ if ( window == mouse_tracking_window )
    mouse_tracking_window = NIL;
  if ( window == grabbing_window )
    grabbing_window = NIL;
}

EventObj
CtoEvent(SDL_Event *event)
{ unsigned int time;
  SDL_MouseButtonFlags mouse_flags = 0;
  float fx, fy;
  Any name = NULL;
  Name ctx_name = NULL;
  Any ctx = NULL;
  SDL_WindowID wid = 0;
  FrameObj frame = NIL;		/* ev->frame */
  Any window;			/* ev->window */

  if ( sdl_call_event(event) )	/* support in_pce_thread/1 */
    fail;
  if ( sdl_frame_event(event) )	/* window events (close/open/size/...) */
    fail;
  if ( sdl_timer_event(event) )	/* Timer event */
    fail;
  if ( sdl_stream_event(event) ) /* I/O stream event */
    fail;
  mouse_flags = SDL_GetMouseState(&fx, &fy);

  switch (event->type)
  { case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
      /* https://wiki.libsdl.org/SDL3/SDL_MouseButtonEvent */
      fx = event->button.x;	/* these are floats */
      fy = event->button.y;
      wid  = event->button.windowID;
      time = event->button.timestamp/1000000; // ns -> ms
      name = button_to_name(event->button.down, event->button.button);
      if ( !name )
	fail;
      break;
      /* https://wiki.libsdl.org/SDL3/SDL_MouseMotionEvent */
    case SDL_EVENT_MOUSE_MOTION:
      fx   = event->motion.x;	/* these are floats */
      fy   = event->motion.y;
      wid  = event->motion.windowID;
      time = event->motion.timestamp/1000000; // ns -> ms
      mouse_flags = event->motion.state;

      if ( mouse_flags & SDL_BUTTON_LMASK )
	name = NAME_msLeftDrag;
      else if ( mouse_flags & SDL_BUTTON_MMASK )
	name = NAME_msMiddleDrag;
      else if ( mouse_flags & SDL_BUTTON_RMASK )
	name = NAME_msRightDrag;
      else
	name = NAME_locMove;
      break;
    case SDL_EVENT_MOUSE_WHEEL:
      wid  = event->wheel.windowID;
      time = event->wheel.timestamp/1000000;
      name = NAME_wheel;
      ctx_name = NAME_rotation;
      int dx = event->wheel.x;
      int dy = event->wheel.y;

      if ( event->wheel.direction == SDL_MOUSEWHEEL_FLIPPED )
      { dx = -dx;
        dy = -dy;
      }
      if ( dy > 0 )
	ctx = toInt(120);
      else
	ctx = toInt(-120);
      break;
      /* https://wiki.libsdl.org/SDL3/SDL_KeyboardEvent */
    case SDL_EVENT_KEY_UP:
      lastmod = event->key.mod;
      fail;		      /* only update modifiers */
      /* https://wiki.libsdl.org/SDL3/SDL_TextInputEvent */
    case SDL_EVENT_TEXT_INPUT: /* Needed for input composition.  TBD */
    { int codepoint;
      char const *u = event->text.text;
      u = utf8_get_char(u, &codepoint);
      if ( *u )
        Cprintf("SDL_EVENT_TEXT_INPUT: multi-char input not yet supported\n");
      wid     = event->text.windowID;
      time    = event->text.timestamp/1000000;
      name    = toInt(codepoint);
      break;
    }
    case SDL_EVENT_KEY_DOWN:
      wid     = event->key.windowID;
      time    = event->key.timestamp/1000000;
      lastmod = event->key.mod;
      name    = keycode_to_name(event);
      if ( !name ) fail;
      break;
    default:
      fail;			/* for now */
  }

  frame = wsid_to_frame(wid);
  DEBUG(NAME_event,
	if ( name != NAME_locMove )
	  Cprintf("Event %s on %s at %1f,%1f, id=%d\n",
		  pp(name), pp(frame), wid, fx, fy));
  if ( !frame )
    fail;

  setLastEventTime(time);

  int x = (int)(fx+0.5);
  int y = (int)(fy+0.5);
  if ( notNil(mouse_tracking_window) )
  { int ox=0, oy=0;
    bool rc = ws_window_frame_position(mouse_tracking_window, frame, &ox, &oy);
    assert(rc);
    (void)rc;
    window = mouse_tracking_window;

    x -= ox;
    y -= oy;
    if ( event->type == SDL_EVENT_MOUSE_BUTTON_UP &&
	 mouse_tracking_button == event->button.button )
      mouse_tracking_window = NIL;
  } else if ( notNil(grabbing_window) )
  { int ox=0, oy=0;
    bool rc = ws_window_frame_position(grabbing_window, frame, &ox, &oy);
    if ( rc )			/* grabbing window on same frame */
    { x -= ox;
      y -= oy;
    }
    window = grabbing_window;
  } else
  { window = frame;
    event_window(&window, &x, &y);
    if ( event->type == SDL_EVENT_MOUSE_BUTTON_DOWN )
    { mouse_tracking_window = window;
      mouse_tracking_button = event->button.button;
    }
  }

  EventObj ev = answerObject(ClassEvent,
			     name,
			     window,
			     toInt(x), toInt(y),
			     state_to_buttons(mouse_flags, lastmod),
			     EAV);
  assign(ev, frame, frame);

  if ( ctx_name )
    attributeObject(ev, ctx_name, ctx);

  return ev;
}


static bool
dispatch_event(EventObj ev)
{ Any target = ev->window;
  AnswerMark mark;
  status rc;

  DEBUG(NAME_event,
	if ( ev->id != NAME_locMove )
	  Cprintf("Dispatching %s (%s at %d,%d) to %s\n",
		  pp(ev), pp(ev->id), valInt(ev->x), valInt(ev->y),
		  pp(target)));

  ServiceMode(is_service_window(target),
	      { markAnswerStack(mark);
		addCodeReference(ev);
		rc = postNamedEvent(ev, target, DEFAULT, NAME_postEvent);
		delCodeReference(ev);
		freeableObj(ev);
		rewindAnswerStack(mark, NIL);
		RedrawDisplayManager(TheDisplayManager());
	      });

  return rc;
}


/**
 * Reset the internal event dispatching state.
 *
 * This function clears any pending events and resets the dispatching
 * mechanism to its initial state. It is typically called before
 * starting a new event loop or when reinitializing the event system.
 */
void
resetDispatch(void)
{
}

static int dispatch_fd = -1;
static FDWatch *watch  = NULL;

static void
set_watch(int fd)
{ if ( fd >= 0 )
  { if ( watch )
    { if ( watch->fd != fd )
      { remove_fd_watch(watch);
	watch = add_fd_to_watch(fd, FD_READY_DISPATCH, NULL);
      } else
      { processed_fd_watch(watch); /* re-enable */
      }
    } else
    { watch = add_fd_to_watch(fd, FD_READY_DISPATCH, NULL);
    }
  }
}

static bool
dispatch_ready_event(void)
{ SDL_Event ev;

  if ( SDL_PollEvent(&ev) )
  { EventObj event = CtoEvent(&ev);
    if ( event )
      dispatch_event(event);
    return true;
  }

  return false;
}

/**
 * Dispatch events from the event queue.
 *
 * @param FD The file descriptor to monitor for events.
 * @param timeout The maximum time to wait for an event.
 * @return true if an event was processed.  false on timeout.
 */

status
ws_dispatch(Int FD, Any timeout)
{ int tmo;
  int fd = (isDefault(FD) ? dispatch_fd :
	    isNil(FD)	  ? -1
			  : valInt(FD));
  if ( fd >= 0 )
    dispatch_fd = fd;

  if ( isNil(timeout) )
  { tmo = -1;
  } else if ( isDefault(timeout) )
  { tmo = 250;
  } else if ( isInteger(timeout) )
  { tmo = valInt(timeout);
  } else if ( instanceOfObject(timeout, ClassReal) )
  { tmo = (int)(valReal(timeout)*1000.0);
  } else
  { tmo = 250;
  }

  if ( SDL_IsMainThread() )
  { if ( dispatch_ready_event() )
      succeed;

    if ( pceMTTryLock(LOCK_PCE) )
    { RedrawDisplayManager(TheDisplayManager());
      ws_redraw_changed_frames();
      pceMTUnlock(LOCK_PCE);
    }

    if ( dispatch_ready_event() )
      succeed;

    if ( fd >= 0 )
      set_watch(fd);

    bool rc;
    SDL_Event ev;
    if ( tmo == -1 )
    { rc = SDL_WaitEvent(&ev);
    } else
    { rc = SDL_WaitEventTimeout(&ev, tmo);
    }

    if ( rc )
    { if ( ev.type == MY_EVENT_FD_READY &&
	   ev.user.code == FD_READY_DISPATCH )
	succeed;

      EventObj event = CtoEvent(&ev);
      if ( event )
	dispatch_event(event);
    }

    considerLocStillEvent();

    return rc;
  } else			/* SDL not yet initialised */
  { int ready;
    int to;
    struct pollfd fds[1];

    if ( isNil(timeout) )
    { to = -1;
    } else if ( isDefault(timeout) )
    { to = 250;
    } else if ( isInteger(timeout) )
    { to = valInt(timeout);
    } else if ( instanceOfObject(timeout, ClassReal) )
    { to = (int)(valReal(timeout)*1000.0);
    } else
      to = 256;

    fds[0].fd = fd;
    fds[0].events = POLLIN;

    ready = poll(fds, 1, to);
    return ready > 0;
  }
}

static bool
input_on_fd(int fd)
{
#ifdef HAVE_POLL
  struct pollfd fds[1];

  fds[0].fd = fd;
  fds[0].events = POLLIN;

  return poll(fds, 1, 0) != 0;
#else
#ifndef __WINDOWS__
  if ( fd < FD_SETSIZE )
#endif
  { fd_set rfds;
    struct timeval tv;

    FD_ZERO(&rfds);
    FD_SET(fd, &rfds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;

    return select(fd+1, &rfds, NULL, NULL, &tv) != 0;
  } else
    return true;
#endif
}

/**
 * Discard any pending input events.
 *
 * @param msg A message indicating the reason for discarding input.
 */
void
ws_discard_input(const char *msg)
{ if ( dispatch_fd >= 0 && input_on_fd(dispatch_fd) )
  { char buf[1024];

    Cprintf("%s; discarding input ...", msg);
    if ( read(dispatch_fd, buf, sizeof(buf)) >= 0 )
      Cprintf("ok\n");
    else
      Cprintf("failed\n");
  }
}

/**
 * Find a  target window  when the  pointer has  left the  window that
 * grabbed the  pointer.  This is used  for drag and drop  and well as
 * for example  the Visual Hierarchy  tool to find the  target window.
 * If root  is @default, find the  frame, else find the  window inside
 * the frame that is below the pointer.
 *
 * @param ev The event object to examine.
 * @param root The root window to compare against.
 * @return The subwindow where the event occurred, or NULL if not applicable.
 */
Any
ws_event_in_subwindow(EventObj ev, Any root)
{ if ( isDefault(root) )
  { return ev->frame;
  } else if ( instanceOfObject(root, ClassFrame) )
  { if ( getFrameWindow(ev->window, OFF) == ev->frame )
    { return ev->window;
    } else
    { Any window = root;
      int x = valInt(ev->x);
      int y = valInt(ev->y);

      event_window(&window, &x, &y);
      return window;
    }
  } else
    fail;
}

/**
 * Wait for a key event within a specified timeout.
 *
 * @param maxwait The maximum time to wait for a key event, in milliseconds.
 * @return 1 if a key event was received; 0 if the timeout expired.
 */
int
ws_wait_for_key(int maxwait)
{
    return 0;
}
