/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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
#include "include.h"
#include <sys/time.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef FD_ZERO
#include <sys/select.h>
#endif
#if defined(HAVE_POLL_H)
#include <poll.h>
#endif
#ifdef HAVE_BSTRING_H
#include <bstring.h>
#endif

#define MAX_DECORATION_NESTING	4

		/********************************
		*       EVENT DISPATCHING	*
		********************************/

void
resetDispatch()
{
}


static void
is_pending(XtPointer ctx, int *source, XtInputId *id)
{
}

static void
is_timeout(XtPointer ctx, XtIntervalId *id)
{ status *p = (status *)ctx;

  *p = FAIL;
}

#ifndef FD_ZERO
#define FD_ZERO(x)	{(x)->fds_bits[0] = 0;}
#define FD_SET(n, x)	{(x)->fds_bits[0] |= 1<<(n); }
#endif

static int	  dispatch_fd = -1;

status
ws_dispatch(Int FD, Any timeout)
{ XtIntervalId tid = 0;
  XtInputId iid = 0;
  status rval = SUCCEED;
  int ofd = dispatch_fd;
  int fd = (isDefault(FD) ? dispatch_fd :
	    isNil(FD)	  ? -1
			  : valInt(FD));

					/* No context: wait for input */
					/* timeout */
  if ( ThePceXtAppContext == NULL )
  { int ready;
#ifdef HAVE_POLL
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
#else
    struct timeval to;
    struct timeval *tp = &to;
    fd_set readfds;
    int setmax = 0;

    if ( isNil(timeout) )
    { tp = NULL;
    } else if ( isDefault(timeout) )
    { to.tv_sec = 0;
      to.tv_usec = 250000;
    } else
    { double v;

      if ( isInteger(timeout) )
	v = (double)valInt(timeout)/1000.0;
      else if ( instanceOfObject(timeout, ClassReal) )
	v = valReal(timeout);
      else
	v = 0.25;

      to.tv_sec  = (long)v;
      to.tv_usec = (long)(v * 1000000.0) % 1000000;
    }

    FD_ZERO(&readfds);
    if ( fd >= 0 )
    { FD_SET(fd, &readfds);
      setmax = max(setmax, fd);
      dispatch_fd = fd;
    }

    ready = select(setmax+1, &readfds, NULL, NULL, tp);
#endif
    dispatch_fd = ofd;

    return (ready > 0 ? SUCCEED : FAIL);
  }					/* A display: dispatch until there */
					/* is input or a timeout */

  if ( fd >= 0 )
  { iid = XtAppAddInput(ThePceXtAppContext, fd,
			(XtPointer) XtInputReadMask, is_pending, NULL);
    dispatch_fd = fd;
  }

  if ( notNil(timeout) )
  { long to = -1;

    if ( isInteger(timeout) )
      to = valInt(timeout);
    else if ( instanceOfObject(timeout, ClassReal) )
      to = (long)(valReal(timeout)*1000.0);

    if ( to > 0 )
      tid = XtAppAddTimeOut(ThePceXtAppContext, to, is_timeout,
			    (XtPointer) &rval);
  }

  DEBUG(NAME_dispatch, Cprintf("Dispatch: timeout = %s, tid = %p\n",
			       pp(timeout), (void*)tid));

  if ( pceMTTryLock(LOCK_PCE) )
  { RedrawDisplayManager(TheDisplayManager());
    pceMTUnlock(LOCK_PCE);
  }
					/* All callbacks must be locked! */
  XtAppProcessEvent(ThePceXtAppContext,
		    XtIMXEvent|XtIMTimer|XtIMAlternateInput);

  if ( tid && rval )			/* if rval = FAIL, we had a timeout */
    XtRemoveTimeOut(tid);
  if ( iid )
    XtRemoveInput(iid);
  dispatch_fd = ofd;

  considerLocStillEvent();

  return rval;
}


static int
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
    return 1;
#endif
}


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


		 /*******************************
		 *     WINDOW TRANSLATIONS	*
		 *******************************/

Any
ws_event_in_subwindow(EventObj ev, Any root)
{ DisplayObj d = getDisplayEvent(ev);
  DisplayWsXref r = d->ws_ref;
  Window src_w = XtWindow(widgetWindow(ev->window));
  int dx, dy;
  Window child;
  int root_is_display;

  if ( isDefault(root) )
    root = d;

  if ( (root_is_display = instanceOfObject(root, ClassDisplay)) )
  { XWindowAttributes atts;
    int depth = MAX_DECORATION_NESTING;

    if ( d != root )
    { errorPce(ev, NAME_notSameDisplay, root);
      fail;
    }

    XGetWindowAttributes(r->display_xref, XtWindow(r->shell_xref), &atts);
    XTranslateCoordinates(r->display_xref, src_w, atts.root,
			  valInt(ev->x), valInt(ev->y),
			  &dx, &dy, &child);

#if 0
    DEBUG(NAME_pointer,
					/* TEST STUFF */
	  ({ Window rr, cr;
	    int rx, ry, wx, wy, mask;

	    if ( XQueryPointer(r->display_xref, atts.root, &rr, &cr,
			       &rx, &ry, &wx, &wy, &mask) )
	    { Cprintf("XTranslateCoordinates --> %d\nXQueryPointer --> %d\n",
		      child, cr);
	    }
	  }));
#endif

    while ( child != None && depth-- > 0 )
    { Cell cell;

      for_cell(cell, d->frames)
      { FrameObj fr = cell->value;
	Widget w;

	if ( (w=widgetFrame(fr)) && child == XtWindow(w) )
	  answer(fr);
      }

      XTranslateCoordinates(r->display_xref, src_w, child,
			    valInt(ev->x), valInt(ev->y),
			    &dx, &dy, &child);
    }

    fail;
  }

  if ( instanceOfObject(root, ClassFrame) )
  { FrameObj fr = root;
    PceWindow sw;

    XTranslateCoordinates(r->display_xref, src_w, XtWindow(widgetFrame(fr)),
			  valInt(ev->x), valInt(ev->y),
			  &dx, &dy, &child);
    if ( child != None && (sw = getMemberHashTable(WindowTable, (Any) child)))
    { if ( instanceOfObject(sw, ClassWindowDecorator) )
      { XTranslateCoordinates(r->display_xref, src_w, child,
			      valInt(ev->x), valInt(ev->y), &dx, &dy,
			      &child);

	if ( child != None )
	  answer(getMemberHashTable(WindowTable, (Any) child));
      }
      answer(sw);
    }
  } else /*if ( instanceOfObject(root, ClassWindow) )*/
  { PceWindow sw = root;

    XTranslateCoordinates(r->display_xref, src_w, XtWindow(widgetWindow(sw)),
			  valInt(ev->x), valInt(ev->y),
			  &dx, &dy, &child);
    if ( child != None )
      answer(getMemberHashTable(WindowTable, (Any) child));
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
We would like to wait for at   most  0.25 seconds to distinguish between
C-x  (cut)  and  C-xC-x  (exchange-point-and-mark)    commands  for  the
emulation of the  CUA  mode.  This  isn't   really  ideal  as  it  waits
unconditionally  without  handling  any  messages.   We  inprove  a  bit
splitting it into a couple of shorter waits.

XCheckIfEvent() removes the  matching  event.   Hence  we  always return
FALSE, but set a flag if we find the target event.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
is_key_event(Display *dsp, XEvent *ev, XPointer arg)
{ if ( ev->xany.type == KeyPress )
  { int *p = (int *)arg;

    *p = TRUE;
  }

  return FALSE;
}


static int
key_waiting(DisplayObj d)
{ DisplayWsXref r = d->ws_ref;
  int waiting = FALSE;
  XEvent event;

  XCheckIfEvent(r->display_xref, &event, is_key_event, (XPointer) &waiting);

  return waiting;
}


int
ws_wait_for_key(int maxwait)
{ msleep(maxwait);

  return key_waiting(CurrentDisplay(NIL));
}
