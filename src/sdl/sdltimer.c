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
#include "sdltimer.h"
#include "../../swipl/pcecall.h"

static Uint32
tm_callback(void *udata, SDL_TimerID id, Uint32 interval)
{ Timer tm = udata;

  if ( onFlag(tm, F_FREEING|F_FREED) )
    return 0;

  SDL_Event ev;
  SDL_zero(ev);
  ev.type = MY_EVENT_TIMER;
  ev.user.data1 = tm;
  SDL_PushEvent(&ev);

  if ( tm->status == NAME_once )
  { assign(tm, status, NAME_idle);
    return 0;
  }
  return interval;
}

bool
sdl_timer_event(SDL_Event *event)
{ if ( event->type == MY_EVENT_TIMER )
  { Timer tm = event->user.data1;

    if ( !onFlag(tm, F_FREEING|F_FREED) &&
	 instanceOfObject(tm, ClassTimer) )
    { pceMTLock(LOCK_PCE);
      if ( tm->service == ON )
      { ServiceMode(PCE_EXEC_SERVICE, executeTimer(tm));
      } else
      { executeTimer(tm);
      }
      pceMTUnlock(LOCK_PCE);
    }
    return true;
  }

  return false;
}


static void
start_timer(Timer tm)
{ Uint32 ms = valReal(tm->interval)*1000.0+0.5;
  Uint32 id = SDL_AddTimer(ms, tm_callback, tm);
  tm->ws_ref = (void*)(intptr_t)id;
}


/**
 * Set the  status of the  specified timer.  This function  enables or
 * disables the timer based on the given status.
 *
 * @param tm Pointer to the Timer object.
 * @param status Name indicating the desired status.  One of
 * NAME_idle, NAME_once or NAME_repeat.
 */
void
ws_status_timer(Timer tm, Name status)
{ if ( tm->ws_ref )
  { Uint32 id = (Uint32)(intptr_t)tm->ws_ref;
    SDL_RemoveTimer(id);
    tm->ws_ref = NULL;
  }

  if ( status == NAME_repeat || status == NAME_once )
    start_timer(tm);
}
