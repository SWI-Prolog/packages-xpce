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

static void	trapTimer(XtPointer xtm, XtIntervalId *id);


static XtIntervalId
getIdTimer(Timer tm)
{ return (XtIntervalId) tm->ws_ref;
}


static void
setIdTimer(Timer tm, XtIntervalId id)
{ tm->ws_ref = (WsRef) id;
}


static void
doTrapTimer(Timer tm)
{ setIdTimer(tm, 0);

  executeTimer(tm);

  if ( tm->status == NAME_repeat )
  { long msec = (long) (valReal(tm->interval) * 1000.0);
    XtIntervalId id;

    id = XtAppAddTimeOut(pceXtAppContext(NULL),
			 msec,
			 trapTimer,
			 (XtPointer) tm);
    setIdTimer(tm, id);
    DEBUG(NAME_timer, Cprintf("\tre-registered %s with id=%p\n",
			      pp(tm), id));
  } else if ( tm->status == NAME_once )
    assign(tm, status, NAME_idle);
}


static void
trapTimer(XtPointer xtm, XtIntervalId *id)
{ Timer tm = (Timer) xtm;

  pceMTLock(LOCK_PCE);
  DEBUG(NAME_timer, Cprintf("trapTimer(%s, %p) (tm->id = %p)\n",
			    pp(tm), *id, getIdTimer(tm)));

  if ( getIdTimer(tm) == *id )
  { if ( tm->service == ON )
    { ServiceMode(PCE_EXEC_SERVICE, doTrapTimer(tm));
    } else
      doTrapTimer(tm);
  }
  pceMTUnlock(LOCK_PCE);
}


void
ws_status_timer(Timer tm, Name status)
{ XtIntervalId id;

  if ( (id = getIdTimer(tm)) )
  { setIdTimer(tm, 0);
    XtRemoveTimeOut(id);
  }

  if ( status != NAME_idle )
  { long msec = (long) (valReal(tm->interval) * 1000.0);
    XtIntervalId nid;

    nid = XtAppAddTimeOut(pceXtAppContext(NULL),
			  msec,
			  trapTimer,
			  (XtPointer) tm);
    setIdTimer(tm, nid);
  }
}


#ifdef O_LICENCE
#include "../../../licence/xtimeout.c"
#endif
