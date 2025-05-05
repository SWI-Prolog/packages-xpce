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
#include "sdlevent.h"
#include "sdlframe.h"
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

static Int
state_to_buttons(SDL_MouseButtonFlags flags)
{ int r = 0;

  if ( flags & SDL_BUTTON_LMASK )  r |= BUTTON_ms_left;
  if ( flags & SDL_BUTTON_MMASK )  r |= BUTTON_ms_middle;
  if ( flags & SDL_BUTTON_RMASK )  r |= BUTTON_ms_right;
  if ( flags & SDL_BUTTON_X1MASK ) r |= BUTTON_ms_button4;
  if ( flags & SDL_BUTTON_X2MASK ) r |= BUTTON_ms_button5;
#if 0
  if ( state & ShiftMask )	r |= BUTTON_shift;
  if ( state & ControlMask )	r |= BUTTON_control;
  if ( state & MetaMask )	r |= BUTTON_meta;
#endif

  return toInt(r);
}

/**
 * @see https://wiki.libsdl.org/SDL3/SDL_Event
 */

EventObj
CtoEvent(SDL_Event *event)
{ unsigned int time;
  SDL_MouseButtonFlags mouse_flags = 0;
  float fx, fy;
  Any name = NULL;
  SDL_WindowID wid = 0;
  Any window = NIL;		/* TODO */

  if ( sdl_frame_event(event) )
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
    case SDL_EVENT_MOUSE_MOTION:
    case SDL_EVENT_KEY_DOWN:
    default:
      fail;			/* for now */
  }

  window = wsid_to_frame(wid);
  DEBUG(NAME_event, Cprintf("Event on %s, id=%d\n", pp(window), wid));
  if ( !window )
    fail;

  setLastEventTime(time);

  int x = (int)(fx+0.5);
  int y = (int)(fy+0.5);
  event_window(&window, &x, &y);

  EventObj ev = answerObject(ClassEvent,
			     name,
			     window,
			     toInt(x), toInt(y),
			     state_to_buttons(mouse_flags),
			     EAV);

  return ev;
}


static bool
dispatch_event(EventObj ev)
{ Any sw = ev->window;
  AnswerMark mark;
  status rc;

  DEBUG(NAME_event, Cprintf("Dispatching %s to %s\n", pp(ev), pp(sw)));

  ServiceMode(is_service_window(sw),
	      { markAnswerStack(mark);
		addCodeReference(ev);
		rc = postNamedEvent(ev, (Graphical) sw, DEFAULT, NAME_postEvent);
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
 * This function clears any pending events and resets the dispatching mechanism
 * to its initial state. It is typically called before starting a new event loop
 * or when reinitializing the event system.
 */
void
resetDispatch(void)
{
}

/**
 * Dispatch events from the event queue.
 *
 * @param FD The file descriptor to monitor for events.
 * @param timeout The maximum time to wait for an event.
 * @return true if an event is ready, false on a timeout.
 */

static int	  dispatch_fd = -1;

status
ws_dispatch(Int FD, Any timeout)
{ int fd = (isDefault(FD) ? dispatch_fd :
	    isNil(FD)	  ? -1
			  : valInt(FD));
  int tmo;

  if ( isNil(timeout) )
  { tmo = -1;
  } else if ( isDefault(timeout) )
  { tmo = 250;
  } else if ( isInteger(timeout) )
  { tmo = valInt(timeout);
  } else if ( instanceOfObject(timeout, ClassReal) )
  { tmo = (int)(valReal(timeout)*1000.0);
  } else
  { tmo = 256;
  }

  if ( fd >= 0 )
    dispatch_fd = fd;

  if ( pceMTTryLock(LOCK_PCE) )
  { RedrawDisplayManager(TheDisplayManager());
    ws_redraw_changed_frames();
    pceMTUnlock(LOCK_PCE);
  }

  bool rc;
  SDL_Event ev;
  if ( tmo == -1 )
  { rc = SDL_WaitEvent(&ev);
  } else
  { rc = SDL_WaitEventTimeout(&ev, tmo);
  }

  if ( rc )
  { EventObj event = CtoEvent(&ev);
    if ( event )
      dispatch_event(event);
  }

  return rc;
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
 * Determine if an event occurred within a subwindow.
 *
 * @param ev The event object to examine.
 * @param root The root window to compare against.
 * @return The subwindow where the event occurred, or NULL if not applicable.
 */
Any
ws_event_in_subwindow(EventObj ev, Any root)
{
    return NULL;
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
