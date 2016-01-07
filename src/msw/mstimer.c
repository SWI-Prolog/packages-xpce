/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2013, University of Amsterdam
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

static HashTable TimerTable;

static UINT
getIdTimer(Timer tm)
{ return (UINT)(intptr_t) tm->ws_ref;
}


static void
setIdTimer(Timer tm, UINT id)
{ DEBUG(NAME_timer, Cprintf("setIdTimer(%s, %d)\n", pp(tm), id));
  tm->ws_ref = (WsRef) (intptr_t)id;
}


static void
do_timer_proc(Timer tm, UINT id)
{ if ( tm->status != NAME_repeat )
  { KillTimer(NULL, id);
    deleteHashTable(TimerTable, toInt(id));
    assign(tm, status, NAME_idle);
  }

  executeTimer(tm);

  RedrawDisplayManager(TheDisplayManager());
}


VOID CALLBACK
timer_proc(HWND hwnd, UINT msg, UINT id, DWORD now)
{ DEBUG(NAME_timer, Cprintf("Firing timer %d\n", id));

  if ( TimerTable )
  { Timer tm;

    pceMTLock(LOCK_PCE);
    if ( (tm = getMemberHashTable(TimerTable, toInt(id))) )
    { if ( tm->service == ON )
      { ServiceMode(PCE_EXEC_SERVICE, do_timer_proc(tm, id));
      } else
	do_timer_proc(tm, id);

      pceMTUnlock(LOCK_PCE);

      return;
    }
    pceMTUnlock(LOCK_PCE);
  }

  KillTimer(NULL, id);			/* Unexpected timer.  Get rid of it */
}


void
ws_status_timer(Timer tm, Name status)
{ UINT id;

  if ( (id = getIdTimer(tm)) )
  { KillTimer(NULL, id);
    deleteHashTable(TimerTable, toInt(id));
    setIdTimer(tm, 0);
  }

  if ( status != NAME_idle )
  { long msec = (long) (valReal(tm->interval) * 1000.0);

    if ( (id = SetTimer(NULL, 0, (UINT)msec, (TIMERPROC) timer_proc)) )
    { if ( !TimerTable )
      { TimerTable = globalObject(CtoName("active_timers"),
				  ClassHashTable, EAV);
	assign(TimerTable, refer, NAME_none);
      }
      appendHashTable(TimerTable, toInt(id), tm);
      setIdTimer(tm, id);
      DEBUG(NAME_timer, Cprintf("Created timer of %d milliseconds (id = %d)\n",
				msec, id));
    } else
      Cprintf("Failed SetTimer()\n");
  }
}


#ifdef O_LICENCE
#include "../../../licence/mstimeout.c"
#endif
