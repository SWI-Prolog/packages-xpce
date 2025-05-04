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

  (void)fd;			/* to be done */

  bool rc;
  SDL_Event ev;
  if ( tmo == -1 )
  { rc = SDL_WaitEvent(&ev);
  } else
  { rc = SDL_WaitEventTimeout(&ev, tmo);
  }

  return rc;
}

/**
 * Discard any pending input events.
 *
 * @param msg A message indicating the reason for discarding input.
 */
void
ws_discard_input(const char *msg)
{
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
