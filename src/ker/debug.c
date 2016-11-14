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

#include <signal.h>
#include <h/kernel.h>
#include <h/interface.h>

static void
errorSignal(int sig)
{ char *msg;
  char tmp[25];

  switch(sig)
  {
#ifdef SIGQUIT
    case SIGQUIT:		msg = "Quit";			break;
#endif
    case SIGILL:		msg = "Illegal instruction";	break;
#ifdef SIGEMT
    case SIGEMT:		msg = "EMT trap";		break;
#endif
#ifdef SIGBUS
    case SIGBUS:		msg = "Bus error";		break;
#endif
    case SIGSEGV:		msg = "Segmentation violation";	break;
#ifdef SIGSYS
    case SIGSYS:		msg = "Bad system call";	break;
#endif
#ifdef SIGPIPE
    case SIGPIPE:		msg = "Pipe error";		break;
#endif
    case SIGFPE:		msg = "Floating point exception"; break;
    default:			msg = tmp;
				sprintf(tmp, "%d", sig);	break;
  }

  errorPce(PCE, NAME_signal, CtoName(msg));
}


void
catchErrorSignals(BoolObj yes)
{ Func handler = (yes == ON ? (Func)errorSignal : (Func)SIG_DFL);

#ifdef SIGQUIT
  hostAction(HOST_SIGNAL, SIGQUIT, handler);
#endif
  hostAction(HOST_SIGNAL, SIGILL,  handler);
#ifdef SIGEMT
  hostAction(HOST_SIGNAL, SIGEMT,  handler);
#endif
#ifdef SIGBUS
  hostAction(HOST_SIGNAL, SIGBUS,  handler);
#endif
  hostAction(HOST_SIGNAL, SIGSEGV, handler);
#ifdef SIGSYS
  hostAction(HOST_SIGNAL, SIGSYS,  handler);
#endif
  hostAction(HOST_SIGNAL, SIGFPE,  handler);
}

#ifndef O_RUNTIME

status
confirmTerminal(char *question, char *def)
{ char line[256];

  Cprintf("%s [%s] ? ", question, *def == 'n' ? "ny" : "yn");
  if ( Cgetline(line, sizeof(line)) == NULL )
    return *def == 'y';
  switch(line[0])
  { case 'n':
    case 'N':	return FALSE;
    case 'y':
    case 'Y':	return TRUE;
    case '\0':	return *def == 'y' ? TRUE : FALSE;
    default:	Cprintf("Please answer 'yes' or 'no'\n");
		return confirmTerminal(question, def);
  }
}

#endif /*O_RUNTIME*/
