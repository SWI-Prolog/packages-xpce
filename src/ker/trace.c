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
#include <h/trace.h>
#include <h/interface.h>
#include <h/graphics.h>

void
resetDebugger(void)
{ CurrentGoal = NULL;
  ServiceMode = PCE_EXEC_USER;
}


void
initDebugger(void)
{ resetDebugger();
}


int
isProperGoal(PceGoal g)
{ int dummy;

  if ( !g )
    fail;

#if defined(STACK_DIRECTION) && STACK_DIRECTION > 0
  if ( (uintptr_t)g > (uintptr_t)&dummy ) /* stack grows up */
#else
  if ( (uintptr_t)g < (uintptr_t)&dummy ) /* stack grows down */
#endif
    fail;
  if ( isProperObject(g->implementation) &&
       isProperObject(g->receiver) )
    succeed;

  fail;
}

#ifndef O_RUNTIME

static int
levelGoal(PceGoal g)
{ int i;

  for(i=0; isProperGoal(g); g=g->parent)
    i++;

  return i;
}


static void
actionHelp(void)
{ writef("\nXPCE Tracer options:\n");
  writef(" a\t\tabort\t\tAbort to host-language toplevel\n");
  writef(" b\t\tbreak\t\tStart interactive toplevel\n");
  writef(" e[iwef] [id]\terror kind\tSet kind to [ignored/warning/error/fatal]\n");
  writef(" g[h] [depth]\tgoals\t\tPrint stack [host]\n");
  writef(" q\t\tquit\t\tQuit XPCE\n");
  writef(" c\t\tcontinue\tContinue execution\n");
  writef(" ? (h)\t\thelp\t\tPrint this text\n\n");
}


static void
breakGoal(PceGoal g)
{ char buf[LINESIZE];
  char *s;

start:
  writef(" ? ");
  Cflush();
  s = Cgetline(buf, sizeof(buf));

  if ( s )
  { int argc = 0;
    char *argv[100];
    char *q;
    Int numarg = DEFAULT;

    for(q = s; *q; )
    { while(*q && isspace(*q))
	q++;
      if ( *q == EOS )
	break;
      argv[argc++] = q;
      while(*q && !isspace(*q))
	q++;
      if ( *q != EOS )
	*q++ = EOS;
    }

    if ( argc >= 2 && isdigit(argv[1][0]) )
      numarg = toInt(atoi(argv[1]));

    if ( argc == 0 )
      return;

    switch(argv[0][0])
    { case 'g':
	ServiceMode(PCE_EXEC_SERVICE,
		    if ( argv[0][1] == 'h' )
		      hostAction(HOST_BACKTRACE,
				 isDefault(numarg) ? 5 : valInt(numarg));
		    else
		      pceBackTrace(g, isDefault(numarg) ? 5 : valInt(numarg)));
        goto again;
      case 'b':
	if ( !hostAction(HOST_BREAK) )
	  send(HostObject(), NAME_break, EAV);
        goto again;
      case 'a':
	if ( !hostAction(HOST_ABORT) )
	  send(HostObject(), NAME_abort, EAV);
        goto again;			/* should not happen */
      case 'e':
      { Error e;

	if ( argc == 2 )
	{ if ( !(e = getConvertError(ClassError, CtoName(argv[1]))) )
	  { writef("No such error: %s\n", CtoName(argv[1]));
	    goto again;
	  }
	} else
	{ if ( !(e = getConvertError(ClassError, PCE->last_error)) )
	  { writef("No current error\n");
	    goto again;
	  }
	}

	if ( !e )
	{ writef("No current error\n");
	  goto again;
	}

	switch(argv[0][1])
	{ case 'i':
	    assign(e, kind, NAME_ignored);
	    break;
	  case 'e':
	    assign(e, kind, NAME_error);
	    break;
	  case 'f':
	    assign(e, kind, NAME_fatal);
	    break;
	  default:
	  case 'w':
	    assign(e, kind, NAME_warning);
	    break;
	}

	writef("Switched error \"%s\" to ->kind \"%s\"\n",
	       e->id, e->kind);

	goto again;
      }
      case 'q':
	debuggingPce(PCE, OFF);
	send(PCE, NAME_die, EAV);
        exit(1);			/* should not happen */
      case 'c':
      case EOS:
	return;
      case '?':
      case 'h':
	actionHelp();
        goto again;
      default:
	writef("Unknown option. (? for help)\n");
        goto again;
    }
  } else
  { hostAction(HOST_HALT);
    exit(1);
  }

  again:
    writef("[%d] ", toInt(levelGoal(g)));
    writeGoal(g);
    goto start;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTES:
	* not-yet-filled aguments are NULL
	* Foreign-arguments are non-NULL, but otherwise only know to the
	  foreogn-language.  This must be handled using a call-back.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
writeGoal(PceGoal g)
{ Name id, arrow;
  int i, argn = 0;

  if ( !isProperGoal(g) )
  { writef("<bad goal-frame>");
    return;
  }

  if ( g->flags & PCE_GF_SEND )
    arrow = CtoName("->");
  else if ( g->flags & PCE_GF_GET )
    arrow = CtoName("<-");
  else
    return;				/* unknown goal */

  if ( notNil(g->implementation) )
    id = qadGetv(g->implementation, NAME_manIndicator, 0, NULL);
  else
    id = CtoName("?");

  writef("%s %O %s%s(", id, g->receiver, arrow, g->selector);

  if ( g->flags & PCE_GF_HOSTARGS )
  { if ( TheCallbackFunctions.writeGoalArgs )
      (*TheCallbackFunctions.writeGoalArgs)(g);
    else
      writef("<host goal-frame>");
  } else
  { for(i=0; i<g->argc; i++)
    { if ( argn++ )
	writef(", ");
      if ( g->argv[i] )
	writef("%O", g->argv[i]);
      else
	writef("(nil)");
    }
    if ( g->va_type )
    { for(i=0; i<g->va_argc; i++)
      { if ( argn++ )
	  writef(", ");
	writef("%O", g->va_argv[i]);
      }
    }
  }

  writef(")");
}


void
pceBackTrace(PceGoal g, int depth)
{ int level;

  if ( !g )
  { g = CurrentGoal;
    if ( !g )
      writef("\t<No goal>\n");
  }
  level = levelGoal(g);

  if ( !depth )
    depth = 5;

  for( ; depth-- > 0 && isProperGoal(g); g = g->parent, level-- )
  { writef("\t[%2d] ", toInt(level));
    writeGoal(g);
    writef("\n");
  }
}


void
writeErrorGoal()
{ PceGoal g = CurrentGoal;

  while(isProperGoal(g) && !(g->flags & PCE_GF_EXCEPTION) )
    g = g->parent;

  if ( isProperGoal(g) )
    writeGoal(g);
  else
    writef("\t<No exception goal>\n");
}

int
pceDebugging(Name subject)
{ if ( ServiceMode == PCE_EXEC_SERVICE )
    fail;

  return memberChain(PCEdebugSubjects, subject);
}

void
pcePrintEnterGoal(PceGoal g)
{ if ( DebuggingProgramObject(g->implementation,
			      D_TRACE_ENTER|D_BREAK_ENTER) &&
       !(g->flags & PCE_GF_HOST) )
  { writef("[%d] enter ", toInt(levelGoal(g)));
    writeGoal(g);
    if ( DebuggingProgramObject(g->implementation, D_BREAK_ENTER) )
      breakGoal(g);
    else
      writef("\n");
  }
}


void
pcePrintReturnGoal(PceGoal g, status rval)
{ Name port;
  int do_break;

  if ( g->flags & PCE_GF_HOST )
    return;

  if ( rval )
  { if ( !DebuggingProgramObject(g->implementation,
				 D_TRACE_EXIT|D_BREAK_EXIT) )
      return;
    do_break = DebuggingProgramObject(g->implementation, D_BREAK_EXIT);
    port = NAME_exit;
  } else
  { if ( !DebuggingProgramObject(g->implementation,
				 D_TRACE_FAIL|D_BREAK_FAIL) )
      return;
    port = NAME_fail;
    do_break = DebuggingProgramObject(g->implementation, D_BREAK_FAIL);
  }

  writef("[%d] %s ", toInt(levelGoal(g)), port);
  writeGoal(g);

  if ( rval && g->flags & PCE_GF_GET )
    writef(" --> %O", g->rval);

  if ( do_break )
    breakGoal(g);
  else
    writef("\n");
}

#endif /*O_RUNTIME*/
