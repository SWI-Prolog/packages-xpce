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

#define SWIPL_WINDOWS_NATIVE_ACCESS 1 /* get Swinhandle() */
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

static void event_window(Any *target, float *x, float *y);

static bool
descent_to_window(Any *target, PceWindow sw, float *x, float *y)
{ if ( xy_in_area(sw->area, *x, *y) &&
       ws_created_window(sw) )
  { *x -= valNum(sw->area->x);
    *y -= valNum(sw->area->y);
    *target = sw;
    event_window(target, x, y);
    return true;
  }

  return false;
}

static void
event_window(Any *target, float *x, float *y)
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
	if ( *x >= valNum(wx) && *x <= valNum(wx) + valNum(sub->area->w) &&
	     *y >= valNum(wy) && *y <= valNum(wy) + valNum(sub->area->h) )
	{ *x -= valNum(wx);
	  *y -= valNum(wy);
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
  if ( mod & SDL_KMOD_GUI )        r |= BUTTON_gui;

  return toInt(r);
}

static Any
keycode_to_name(SDL_Event *event)
{ if ( event->key.key >= 32 && event->key.key < DEL )
  { if ( event->key.mod & MetaMask )
      return toInt(Meta(event->key.key));
    if ( event->key.mod & ControlMask )
    { if ( event->key.key >= 'a' && event->key.key <= 'z' )
	return toInt(Control(event->key.key));
      else if ( event->key.key == SDLK_AT )
	return ZERO;
      else
	return toInt(event->key.key);
    }
    if ( event->key.mod & SDL_KMOD_GUI )
      return toInt(event->key.key);
  }

  switch(event->key.key)
  { case SDLK_RETURN:    return NAME_RET;
    case SDLK_ESCAPE:    return NAME_ESC;
    case SDLK_TAB:       return NAME_TAB;
    case SDLK_BACKSPACE: return NAME_BS;
    case SDLK_DELETE:    return NAME_DEL;
    case SDLK_PAUSE:	 return NAME_pause;
    case SDLK_HOME:	 return NAME_cursorHome;
    case SDLK_PAGEUP:	 return NAME_pageUp;
    case SDLK_END:	 return NAME_end;
    case SDLK_PAGEDOWN:	 return NAME_pageDown;
    case SDLK_RIGHT:	 return NAME_cursorRight;
    case SDLK_LEFT:	 return NAME_cursorLeft;
    case SDLK_DOWN:	 return NAME_cursorDown;
    case SDLK_UP:	 return NAME_cursorUp;
    case SDLK_F1:	 return NAME_f1;
    case SDLK_F2:	 return NAME_f2;
    case SDLK_F3:	 return NAME_f3;
    case SDLK_F4:	 return NAME_f4;
    case SDLK_F5:	 return NAME_f5;
    case SDLK_F6:	 return NAME_f6;
    case SDLK_F7:	 return NAME_f7;
    case SDLK_F8:	 return NAME_f8;
    case SDLK_F9:	 return NAME_f9;
    case SDLK_F10:	 return NAME_f10;
    case SDLK_F11:	 return NAME_f11;
    case SDLK_F12:	 return NAME_f12;

      /* The keypad sends SDLK_KP_* events, with or without
	 numlock.   Here we assign the functions.  Next, we
	 use the text input distinction to see whether the
	 keypad sends a number or a command.

	 But, what is the correct mapping from keypad numbers
	 to functions?   This works on a default US keyboard.
       */
    case SDLK_KP_8:	 return NAME_cursorUp;
    case SDLK_KP_4:	 return NAME_cursorLeft;
    case SDLK_KP_6:	 return NAME_cursorRight;
    case SDLK_KP_2:	 return NAME_cursorDown;
    case SDLK_KP_7:	 return NAME_cursorHome;
    case SDLK_KP_1:	 return NAME_end;
    case SDLK_KP_9:	 return NAME_pageUp;
    case SDLK_KP_3:	 return NAME_pageDown;
    case SDLK_KP_PERIOD: return NAME_DEL;
    case SDLK_KP_ENTER:	 return NAME_RET;
  }

  fail;
}

/**
 * @see https://wiki.libsdl.org/SDL3/SDL_Event
 */

static SDL_Keymod lastmod = SDL_KMOD_NONE;
static Any grabbing_window = NIL;
static Any mouse_tracking_window = NIL; /* Window or Frame */
static Uint8 mouse_tracking_button;
static SDL_WindowID mouse_tracking_wid = 0;
static SDL_DisplayID last_display_id = 0;
static Uint32 keyboard_timer = 0;
static SDL_Event keydown_event = {0};
static Uint64 keyinput_time = 0;

static Uint32
tm_keyboard_timeout(void *udata, SDL_TimerID id, Uint32 interval)
{ keyboard_timer = 0;

  if ( keyinput_time < keydown_event.key.timestamp )
  { SDL_Event ev;

    SDL_zero(ev);
    ev.type = MY_EVENT_KEYDOWN_TIMEOUT;
    SDL_PushEvent(&ev);
    DEBUG(NAME_keyboard,
	  Cprintf("%"PRIu64" < %"PRIu64": Pushed delayed keyboard event\n",
		  keyinput_time, keydown_event.key.timestamp));
  }

  return 0;
}

/** True if the character should be passed as a printable
 *  character rather than a keydown event with a modifier.
 */

static StringObj macOSOptionCharacters = NULL;

static bool
isOptionPrintCharacter(int code)
{ if ( code >= 32 && code <= 126 )
    return true;

  if ( !macOSOptionCharacters &&
       !isNil(macOSOptionCharacters) )
  { macOSOptionCharacters = getClassVariableValueClass(
      ClassEvent,
      NAME_macosOptionCharacters);
    if ( macOSOptionCharacters )
    { DEBUG(NAME_keyboard,
	    Cprintf("macOSOptionCharacters = %s\n",
		    pp(macOSOptionCharacters)));
    } else
    { macOSOptionCharacters = (StringObj)NIL;
    }
  }

  if ( notNil(macOSOptionCharacters) )
    return str_index(&macOSOptionCharacters->data, code) != -1;

  return false;
}


PceWindow
ws_grabbing_window(void)
{ if ( notNil(grabbing_window) )
    return grabbing_window;

  return NULL;
}

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

DisplayObj
ws_last_display_from_event(void)
{ return dsp_id_to_display(last_display_id);
}

EventObj
CtoEvent(SDL_Event *event)
{ ASSERT_SDL_MAIN();
  unsigned int time;
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
  if ( sdl_display_event(event) ) /* display events (add/delete/change) */
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
      mouse_tracking_wid = event->button.windowID;
      goto mouse_cont;
    case SDL_EVENT_MOUSE_BUTTON_UP:
      /* https://wiki.libsdl.org/SDL3/SDL_MouseButtonEvent */
      if ( !event->button.windowID ) /* Seems to happen on MacOS */
	event->button.windowID = mouse_tracking_wid;
      mouse_tracking_wid = 0;
    mouse_cont:
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
    { static int last_time = 0;
      wid  = event->wheel.windowID;
      time = event->wheel.timestamp/1000000;
      name = NAME_wheel;
      ctx_name = NAME_rotation;
      //float dx = event->wheel.x;
      float dy = event->wheel.y;

      DEBUG(NAME_wheel,
	    Cprintf("Mouse wheel event.  dy=%.6f, dt=%dms%s\n",
		    dy, time-last_time,
		    event->wheel.direction == SDL_MOUSEWHEEL_FLIPPED
		    ? " (flipped)" : ""));
      last_time = time;

      if ( dy > 0.0 )
	ctx = toInt(120);
      else if ( dy < 0.0 )
	ctx = toInt(-120);
      else
	fail;
      break;
    }
      /* https://wiki.libsdl.org/SDL3/SDL_KeyboardEvent */
    case SDL_EVENT_KEYMAP_CHANGED:
      DEBUG(NAME_keyboard,
	    Cprintf("Got SDL_EVENT_KEYMAP_CHANGED\n"));
      fail;
    case SDL_EVENT_KEY_UP:
      lastmod = event->key.mod;
      fail;		      /* only update modifiers */
      /* https://wiki.libsdl.org/SDL3/SDL_TextInputEvent */
    case SDL_EVENT_TEXT_INPUT:
    { int codepoint;
      char const *u = event->text.text;
      u = utf8_get_char(u, &codepoint);
      if ( *u )
        Cprintf("SDL_EVENT_TEXT_INPUT: multi-char input not yet supported\n");

      DEBUG(NAME_keyboard,
	    Cprintf("SDL_EVENT_TEXT_INPUT: %s at %" PRIu64 "\n",
		    event->text.text, event->text.timestamp));

      if ( keyboard_timer )
      { if ( isOptionPrintCharacter(codepoint) )
	{ SDL_RemoveTimer(keyboard_timer);
	  keyboard_timer = 0;
	  keyinput_time = event->text.timestamp;
	} else
	{ fail;			/* process as key-down */
	}
      }

      wid     = event->text.windowID;
      time    = event->text.timestamp/1000000;
      name    = toInt(codepoint);
      break;
    }
    case SDL_EVENT_KEY_DOWN:
    { SDL_Keymod isdown = (SDL_KMOD_LCTRL|SDL_KMOD_RCTRL|SDL_KMOD_GUI);
#ifndef __APPLE__
      isdown |= SDL_KMOD_LALT;
#endif

      lastmod = event->key.mod;
      name = keycode_to_name(event);
      if ( !name )
      { DEBUG(NAME_scancode,
	      Cprintf("Ignoring keydown.  Mod=0x%x, scancode=%d, key=0x%x\n",
		      event->key.mod, event->key.scancode, event->key.key));
	fail;
      }

      if ( event->key.mod & isdown )
	goto immediate_down_event;

      keydown_event  = *event;
      keyinput_time  = 0;
      keyboard_timer = SDL_AddTimer(10, tm_keyboard_timeout, NULL);
      DEBUG(NAME_keyboard,
	    Cprintf("Delaying keyboard down event at %" PRIu64 "\n"));
      fail;
    }
    case MY_EVENT_KEYDOWN_TIMEOUT:
      event   = &keydown_event;
    immediate_down_event:
      wid     = event->key.windowID;
      time    = event->key.timestamp/1000000;
      name    = keycode_to_name(event);
      DEBUG(NAME_keyboard,
	    bool delayed = (&keydown_event == event);
	    Cprintf("SDL_EVENT_KEY_DOWN: %s at %"PRIu64"%s\n",
		    pp(name), event->text.timestamp,
		    delayed ? " (delayed)" : ""));
      break;
    default:
      fail;			/* for now */
  }

  frame = wsid_to_frame(wid);
  DEBUG(NAME_event,
	if ( name != NAME_locMove )
	  Cprintf("Event %s on %s at %1f,%1f, SDL window-id=%d\n",
		  pp(name), pp(frame), fx, fy, wid));
  if ( !frame )
    fail;

  setLastEventTime(time);

#if O_HDPX
  WsFrame wfr = sdl_frame(frame, false);
  float scale = SDL_GetWindowPixelDensity(wfr->ws_window);
#else
  float scale = 1.0;
#endif

  if ( name != NAME_locMove )
  { WsFrame wfr = sdl_frame(frame, false);
    last_display_id = SDL_GetDisplayForWindow(wfr->ws_window);
  }

  float x = fx*scale;
  float y = fy*scale;
  if ( notNil(mouse_tracking_window) )
  { if ( onFlag(mouse_tracking_window, F_FREED|F_FREEING) ||
	 !ws_created_window(mouse_tracking_window) )
    { Cprintf("Mouse tracking window (%s) is lost?\n",
	      pp(mouse_tracking_window));
      delCodeReference(mouse_tracking_window);
      mouse_tracking_window = NIL;
      fail;
    }

    float ox=0, oy=0;
    bool rc = ws_window_frame_position(mouse_tracking_window, frame, &ox, &oy);
    if ( rc )
    { window = mouse_tracking_window;
      x -= ox;
      y -= oy;
    } else
    { window = frame;
      event_window(&window, &x, &y);
    }
    if ( event->type == SDL_EVENT_MOUSE_BUTTON_UP &&
	 mouse_tracking_button == event->button.button )
    { delCodeReference(mouse_tracking_window);
      mouse_tracking_window = NIL;
    }
  } else if ( notNil(grabbing_window) )
  { if ( onFlag(grabbing_window, F_FREED|F_FREEING) )
    { Cprintf("Grabbing window %s lost?\n", pp(grabbing_window));
      grabbing_window = NIL;
      goto not_grabbing;
    }
    float ox=0, oy=0;
    bool rc = ws_window_frame_position(grabbing_window, frame, &ox, &oy);
    if ( rc )			/* grabbing window on same frame */
    { x -= ox;
      y -= oy;
    }
    window = grabbing_window;
  } else
  { not_grabbing:
    window = frame;
    event_window(&window, &x, &y);
    if ( event->type == SDL_EVENT_MOUSE_BUTTON_DOWN )
    { mouse_tracking_window = window;
      mouse_tracking_button = event->button.button;
      addCodeReference(mouse_tracking_window);
    }
  }

  EventObj ev = answerObject(ClassEvent,
			     name,
			     window,
			     toInt(x), toInt(y),
			     state_to_buttons(mouse_flags, lastmod),
			     EAV);
  if ( ev )
  { assign(ev, frame, frame);

    if ( ctx_name )
      attributeObject(ev, ctx_name, ctx);
  } else
  { Cprintf("Failed to create event with id %s\n", pp(name));
  }

  return ev;
}


static bool
dispatch_event(EventObj ev)
{ Any target = ev->window;
  AnswerMark mark;
  status rc;

  DEBUG(NAME_event,
	if ( ev->id != NAME_locMove )
	  Cprintf("Dispatching %s (%s at %.1f,%.1f) to %s\n",
		  pp(ev), pp(ev->id), valNum(ev->x), valNum(ev->y),
		  pp(target)));

  if ( onFlag(target, F_FREED|F_FREEING) )
  { Cprintf("Event %s on %s; ignored\n", pp(ev->id), pp(target));
    ws_event_destroyed_target(target);
    return false;
  }

  ServiceMode(is_service_window(target),
	      { markAnswerStack(mark);
		addCodeReference(ev);
		rc = postNamedEvent(ev, target, DEFAULT, NAME_postEvent);
		delCodeReference(ev);
		freeableObj(ev);
		rewindAnswerStack(mark, NIL);
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

static waitable_t dispatch_fd = NO_WAITABLE;
static FDWatch *watch  = NULL;

static void
set_watch(waitable_t fd)
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
{ ASSERT_SDL_MAIN();
  SDL_Event ev;

  if ( SDL_PollEvent(&ev) )
  { EventObj event = CtoEvent(&ev);
    if ( event )
      dispatch_event(event);
    return true;
  }

  return false;
}

#ifdef __WINDOWS__
static bool
win_wait_for_handle(HANDLE hConsole, int tmo)
{ if ( !hConsole )
    return true;

  for(;;)
  { DWORD rc = MsgWaitForMultipleObjects(1,
					 &hConsole,
					 false,	/* wait for either event */
					 tmo,
					 QS_ALLINPUT);

    if ( rc == WAIT_OBJECT_0+1 )
    { MSG msg;

      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
      }
      return false;
    } else if ( rc == WAIT_OBJECT_0 )
    { return true;
    } else
    { DEBUG(NAME_stream,
	    Sdprintf("MsgWaitForMultipleObjects(): 0x%x\n", rc));
    }
  }
}
#endif

/**
 * Dispatch events from the event queue.
 *
 * @param FD The file descriptor to monitor for events.
 * @param timeout The maximum time to wait for an event.
 * @return true if an event was processed.  false on timeout.
 */

status
ws_dispatch(IOSTREAM *input, Any timeout)
{ int tmo;
  waitable_t fd;

  if ( !input )
  { fd = NO_WAITABLE;
  } else if ( input == DEFAULT )
  { fd = dispatch_fd;
  } else
  {
#if __WINDOWS__
    fd = Swinhandle(input);
#else
    fd = Sfileno(input);
#endif
  }

  if ( fd != NO_WAITABLE )
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

    if ( pceMTTryLock() )
    { RedrawDisplayManager(TheDisplayManager());
      ws_redraw_changed_frames();
      pceMTUnlock();
    }

    if ( dispatch_ready_event() )
      succeed;

    if ( fd != NO_WAITABLE )
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
      { FDWatch *watch = ev.user.data1;
	cmp_and_set_watch(watch, WATCH_PENDING, WATCH_PROCESSING);
	succeed;
      }

      EventObj event = CtoEvent(&ev);
      if ( event )
	dispatch_event(event);
    }

    considerLocStillEvent();

    return rc;
  } else			/* SDL not yet initialised */
  { int to;

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

#if HAVE_POLL
    int ready;
    struct pollfd fds[1];
    fds[0].fd = fd;
    fds[0].events = POLLIN;

    ready = poll(fds, 1, to);
    return ready > 0;
#elif __WINDOWS__
    return win_wait_for_handle(fd, to);
#else
    (void)to;
    return true;
#endif
  }
}

/**
 * @param fd is a file descriptor on POSIX systems and a HANDLE on
 * Windows.
 * @return true if there is input in `fd`.
 */

static bool
input_on_fd(waitable_t fd)
{
#ifdef __WINDOWS__
  return WaitForSingleObject(fd, 0) == WAIT_OBJECT_0;
#elif HAVE_POLL
  struct pollfd fds[1];

  fds[0].fd = fd;
  fds[0].events = POLLIN;

  return poll(fds, 1, 0) != 0;
#else
  if ( fd < FD_SETSIZE )
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

#ifdef __WINDOWS__
static bool
isconsole(HANDLE h)
{ DWORD mode;

  return GetConsoleMode(h, &mode);
}
#endif

/**
 * Discard any pending input events.
 *
 * @param msg A message indicating the reason for discarding input.
 */
void
ws_discard_input(const char *msg)
{ if ( dispatch_fd != NO_WAITABLE && input_on_fd(dispatch_fd) )
  { Cprintf("%s; discarding input ...", msg);
#ifdef __WINDOWS__
    if ( isconsole(dispatch_fd) )
      FlushConsoleInputBuffer(dispatch_fd);
#else
    char buf[1024];
    if ( read(dispatch_fd, buf, sizeof(buf)) >= 0 )
      Cprintf("ok\n");
    else
      Cprintf("failed\n");
#endif
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
  { Any window = root;
    float x = valNum(ev->x);
    float y = valNum(ev->y);

    event_window(&window, &x, &y);
    return window;
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
