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

#include "md.h"
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <signal.h>
#include <stddef.h>
#ifdef _MSC_VER
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
#if (_MSC_VER < 1300)
typedef long intptr_t;
typedef unsigned long uintptr_t;
#endif
#else
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <inttypes.h>			/* more portable than stdint.h */
#endif
#include <h/interface.h>
#include "stub.h"

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#ifdef SYSLIB_H
#include SYSLIB_H
#endif

typedef void (*VoidFunc)();
typedef void (*sig_handler_t)(int);
typedef void (*atexit_hook_t)(void);
typedef void (*onexit_hook_t)(int, void *);

static char * host_action_names[] =
{ "HOST_QUERY",
  "HOST_TRACE",
  "HOST_BACKTRACE",
  "HOST_HALT",
  "HOST_BREAK",
  "<unused (5)>",
  "HOST_ABORT",
  "HOST_SIGNAL",
  "HOST_RECOVER_FROM_FATAL_ERROR",
  "HOST_ATEXIT",
  "HOST_CONSOLE",
  "HOST_CHECK_INTERRUPT"
};

#define HIGHEST_HOST_ACTION_NAME (sizeof(host_action_names) / sizeof(char *))

int
Stub__HostActionv(int action, va_list args)
{ int rval = PCE_SUCCEED;

  switch(action)
  { case HOST_ATEXIT:
#if HAVE_ON_EXIT
    { onexit_hook_t func = va_arg(args, onexit_hook_t);

      on_exit(func, NULL);
      break;
    }
#else
#if HAVE_ATEXIT
    { atexit_hook_t func = va_arg(args, atexit_hook_t);

      atexit(func);
      break;
    }
#endif
#endif
    case HOST_TRACE:
    case HOST_BACKTRACE:
    case HOST_BREAK:
    case HOST_ABORT:
    case HOST_RECOVER_FROM_FATAL_ERROR:
      Cprintf("hostAction(%d (=%s)) not supported for C++-interface\n",
	      action, host_action_names[action]);
      rval = PCE_FAIL;
      break;
    case HOST_HALT:
      exit(va_arg(args, int));
      break;
    case HOST_SIGNAL:
      signal(va_arg(args, int), va_arg(args, sig_handler_t));
      break;
    case HOST_CHECK_INTERRUPT:
      return PCE_FAIL;
    default:
      Cprintf("Unknown action request from PCE: %d\n", action);
      rval = PCE_FAIL;
  }

  return rval;
}


int
Stub__HostQuery(int what, PceCValue *value)
{ switch(what)
  { case HOST_CONSOLE:
      return PCE_FAIL;
    default:
      Cprintf("Unknown query from PCE: %d\n", what);
      return PCE_FAIL;
  }
}


int
Stub__HostSend(PceObject prolog, PceName sel, int argc, PceObject *argv)
{ Cprintf("hostSend() not implemented.  See class `c'\n");

  return PCE_FAIL;
}


PceObject
Stub__HostGet(PceObject prolog, PceName sel, int argc, PceObject *argv)
{ Cprintf("hostGet() not implemented.  See class `c'\n");

  return PCE_FAIL;
}


int
Stub__HostCall(PceGoal goal)
{ Cprintf("hostCall() not implemented\n");

  return PCE_FAIL;
}

