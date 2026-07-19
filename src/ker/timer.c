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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static status	runningTimer(Timer tm, BoolObj val);

static status
initialiseTimer(Timer tm, Real interval, Code msg)
{ if ( isDefault(msg) )
    msg = NIL;

  assign(tm, interval, CtoReal(0.0));
  assign(tm, message,  msg);
  assign(tm, status,   NAME_idle);
  assign(tm, service,  OFF);

  intervalTimer(tm, interval);

  succeed;
}


static status
unlinkTimer(Timer tm)
{ runningTimer(tm, OFF);

  succeed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A  scheduled timer  holds a  code reference  to itself.   This keeps  it
alive while it is pending, so a timer that is not referenced from
elsewhere can simply be created and started:

    new(T, timer(0.01, message(Gr, expand_all))), send(T, start, once)

The reference is dropped when the timer  becomes idle again.  If it just
fired and nothing else refers to it,  the timer is destroyed.  If it was
stopped explicitly it is  left as it was, so it  remains available until
the goal that created it completes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
scheduleTimer(Timer tm)
{ addCodeReference(tm);			/* survive garbage collection */
}


static void
unscheduleTimer(Timer tm)
{ delCodeReference(tm);
}


/**
 * Drop the code reference of a timer that completed and destroy the
 * timer if nothing refers to it.  Used by the window system layer after
 * a `once' timer fired and by ->delay.
 *
 * The timer is destroyed before dropping our reference: the message may
 * have destroyed the timer, in which case dropping the last reference
 * unallocs it.  Therefore tm may not be used after delCodeReference().
 *
 * @param tm Pointer to the Timer object.  May not be used afterwards.
 */

void
releaseTimer(Timer tm)
{ if ( tm->status == NAME_idle &&	/* ->message did not restart us */
       refsObject(tm) == 0 &&		/* nothing else refers to us */
       !onFlag(tm, F_LOCKED|F_PROTECTED|F_FREEING|F_FREED) )
  { deleteAnswerObject(tm);		/* we are the last one interested */
    freeObject(tm);			/* unalloc deferred: we hold a ref */
  }

  delCodeReference(tm);			/* may unalloc tm */
}


status
intervalTimer(Timer tm, Real interval)
{ if ( valReal(interval) == valReal(tm->interval) )
    succeed;

  assign(tm, interval, interval);
  if ( tm->status == NAME_repeat )
    statusTimer(tm, NAME_repeat);

  succeed;
}


status
executeTimer(Timer tm)
{ if ( notNil(tm->message) )
    return forwardReceiverCode(tm->message, tm, EAV);

  fail;
}


status
statusTimer(Timer tm, Name stat)
{ Name old = tm->status;

  ws_status_timer(tm, stat);
  assign(tm, status, stat);

  if ( stat != old )
  { if ( old == NAME_idle )
      scheduleTimer(tm);
    else if ( stat == NAME_idle )
      unscheduleTimer(tm);
  }

  succeed;
}


static status
delayTimer(Timer tm)
{ DisplayObj d = CurrentDisplay(NIL);

  addCodeReference(tm);			/* we watch <-status below */
  statusTimer(tm, NAME_once);
  while( tm->status == NAME_once )
  { if ( dispatchDisplay(d) )
      ws_discard_input("Timer running");
  }
  releaseTimer(tm);

  succeed;
}


status
startTimer(Timer tm, Name mode)
{ if ( isDefault(mode) )
    mode = NAME_repeat;

  return statusTimer(tm, mode);
}


status
stopTimer(Timer tm)
{ return statusTimer(tm, NAME_idle);
}


static status
runningTimer(Timer tm, BoolObj val)
{ return (val == ON ? startTimer(tm, NAME_repeat) : stopTimer(tm));
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "interval=real", "message=[code]*" };

/* Instance Variables */

static vardecl var_timer[] =
{ SV(NAME_interval, "real", IV_GET|IV_STORE, intervalTimer,
     NAME_time, "Interval between messages in seconds"),
  IV(NAME_message, "code*", IV_BOTH,
     NAME_action, "Code executed each time"),
  SV(NAME_status, "{idle,repeat,once}", IV_GET|IV_STORE, statusTimer,
     NAME_status, "Status of timer"),
  IV(NAME_service, "bool", IV_BOTH,
     NAME_debugging, "If @on, execution cannot be debugged"),
  IV(NAME_wsRef, "alien:WsRef", IV_GET,
     NAME_internal, "Window System Reference")
};

/* Send Methods */

static senddecl send_timer[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseTimer,
     DEFAULT, "Create for interval and message"),
  SM(NAME_unlink, 0, NULL, unlinkTimer,
     DEFAULT, "Destroy X-timer"),
  SM(NAME_execute, 0, NULL, executeTimer,
     NAME_action, "Fire the timer right now"),
  SM(NAME_delay, 0, NULL, delayTimer,
     NAME_status, "Delay for <-interval"),
  SM(NAME_running, 1, "running=bool", runningTimer,
     NAME_status, "Start/stop the timer in `repeat' mode"),
  SM(NAME_start, 1, "how=[{repeat,once}]", startTimer,
     NAME_status, "Equivalent to ->status: [repeat]"),
  SM(NAME_stop, 0, NULL, stopTimer,
     NAME_status, "Equivalent to ->status: idle")
};

/* Get Methods */

#define get_timer NULL
/*
static getdecl get_timer[] =
{
};
*/

/* Resources */

#define rc_timer NULL
/*
static classvardecl rc_timer[] =
{
};
*/

/* Class Declaration */

static Name timer_termnames[] = { NAME_interval, NAME_message };

ClassDecl(timer_decls,
          var_timer, send_timer, get_timer, rc_timer,
          2, timer_termnames);

status
makeClassTimer(Class class)
{ return declareClass(class, &timer_decls);
}
