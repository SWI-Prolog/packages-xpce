/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

#include "include.h"

void
resetDispatch()
{
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There is little reason for timeout here. This function returns everytime
the loc_still timer expires (250 milliseconds).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_dispatch(Int FD, Any timeout)
{ MSG msg;

  if ( GetMessage(&msg, NULL, 0, 0) )
  { TranslateMessage(&msg);
    DispatchMessage(&msg);

    succeed;				/* processed an event */
  }

  ExitProcess(0);			/* WM_QUIT received */
  fail;					/* make compiler happy */
}


Any
ws_event_in_subwindow(EventObj ev, Any root)
{ DisplayObj d = getDisplayEvent(ev);
  Int ex, ey;
  POINT pt;

  if ( isDefault(root) )
    root = d;

  get_xy_event(ev, root, ON, &ex, &ey);
  pt.x = valInt(ex);
  pt.y = valInt(ey);
  DEBUG(NAME_drag, Cprintf("Point at %d,%d to %s\n", pt.x, pt.y, pp(root)));

  if ( instanceOfObject(root, ClassDisplay) )
  { HWND win = WindowFromPoint(pt);
    Any obj;

    if ( (obj=getObjectFromHWND(win)) )
      return get(obj, NAME_frame, EAV);
  } else if ( instanceOfObject(root, ClassFrame) )
  { PceWindow sw = get_window_holding_point(root, &pt);

    if ( sw && instanceOfObject(sw, ClassWindowDecorator) )
      return ws_event_in_subwindow(ev, sw);

    return sw;
  } else /*if ( instanceOfObject(root, ClassWindow) )*/
  { HWND win;
    PceWindow sw;

    if ( (win = ChildWindowFromPoint(getHwndWindow(root), pt)) &&
	 (sw  = getObjectFromHWND(win)) &&
	 instanceOfObject(sw, ClassWindow) )
      return sw;
  }

  fail;
}


		 /*******************************
		 *	       LOC-STILL	*
		 *******************************/

static VOID CALLBACK
locStillTimer(HWND hwnd, UINT msg, UINT id, DWORD now)
{ DEBUG(NAME_locStill, Cprintf("locStillTimer() called\n"));
  considerLocStillEvent();

  /*ws_init_loc_still_timer();*/
}


void
ws_init_loc_still_timer()
{ if ( !SetTimer(NULL, 0, (UINT)(250), (TIMERPROC) locStillTimer) )
    Cprintf("SetTimer() failed\n");
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Wait for 250 milliseconds to see whether another key is pressed. This is
used to merge CUA accelerators C-x  and   C-c  with the Emacs ones. Note
that we check for key-down as there will be an up waiting for us.

In practice C-x C-x is only trick handled through this code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
ws_wait_for_key(int maxwait)
{ MSG msg;

  msleep(maxwait);

  if ( PeekMessage(&msg, NULL, WM_KEYDOWN, WM_KEYDOWN, PM_NOREMOVE) )
  { succeed;
  } else
  { fail;
  }
}
