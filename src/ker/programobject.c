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

status
initialiseProgramObject(Any obj)
{ ProgramObject o = obj;

  o->dflags = (uintptr_t) ZERO | D_SYSTEM;

  succeed;
}


status
initialiseNewSlotProgramObject(ProgramObject obj, Variable var)
{ if ( var->name == NAME_dflags )
    obj->dflags = (uintptr_t) ZERO;

  succeed;
}


#ifndef O_RUNTIME
static unsigned long
nameToTraceFlag(Name name)
{ if ( name == NAME_enter )
    return D_TRACE_ENTER;
  if ( name == NAME_exit )
    return D_TRACE_EXIT;
  if ( name == NAME_fail )
    return D_TRACE_FAIL;
/*if ( name == NAME_full || isDefault(what) )*/
    return D_TRACE;
}


static unsigned long
nameToBreakFlag(Name name)
{ if ( name == NAME_enter )
    return D_BREAK_ENTER;
  if ( name == NAME_exit )
    return D_BREAK_EXIT;
  if ( name == NAME_fail )
    return D_BREAK_FAIL;
/*if ( name == NAME_full || isDefault(what) )*/
    return D_BREAK;
}


static status
traceProgramObject(ProgramObject obj, Name what, BoolObj val)
{ unsigned long flag = nameToTraceFlag(what);

  if ( val != OFF )
  { setDFlag(obj, flag);
    debuggingPce(PCE, ON);
  } else
    clearDFlag(obj, flag);

  succeed;
}


static BoolObj
getTraceProgramObject(ProgramObject obj, Name what)
{ unsigned long flag = nameToTraceFlag(what);

  answer(onDFlag(obj, flag) ? ON : OFF);
}


static status
breakProgramObject(ProgramObject obj, Name what, BoolObj val)
{ unsigned long flag = nameToBreakFlag(what);

  if ( val != OFF )
  { setDFlag(obj, flag);
    debuggingPce(PCE, ON);
  } else
    clearDFlag(obj, flag);

  succeed;
}


static BoolObj
getBreakProgramObject(ProgramObject obj, Name what)
{ unsigned long flag = nameToBreakFlag(what);

  answer(onDFlag(obj, flag) ? ON : OFF);
}

#endif /*O_RUNTIME*/

static status
systemProgramObject(ProgramObject obj, BoolObj val)
{ if ( val == ON )
    setDFlag(obj, D_SYSTEM);
  else
    clearDFlag(obj, D_SYSTEM);

  succeed;
}


static BoolObj
getSystemProgramObject(ProgramObject obj)
{ answer(onDFlag(obj, D_SYSTEM) ? ON : OFF);
}


#ifndef TAGGED_LVALUE
void
setDFlagProgramObject(Any obj, unsigned long mask)
{ ProgramObject po = obj;

  po->dflags |= mask;
}


void
clearDFlagProgramObject(Any obj, unsigned long mask)
{ ProgramObject po = obj;

  po->dflags &= ~mask;
}

#endif /*TAGGED_LVALUE*/

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

#ifndef O_RUNTIME
static char *T_debug[] =
        { "ports=[{full,enter,exit,fail}]", "value=[bool]" };
#endif

/* Instance Variables */

static vardecl var_programObject[] =
{ IV(NAME_dflags, "int", IV_BOTH,
     NAME_debugging, "Debugging-flags of the program_object")
};

/* Send Methods */

static senddecl send_programObject[] =
{ SM(NAME_initialise, 0, NULL, initialiseProgramObject,
     DEFAULT, "Create program_object"),
  SM(NAME_initialiseNewSlot, 1, "variable", initialiseNewSlotProgramObject,
     NAME_compatibility, "Initialise <-dflags"),
#ifndef O_RUNTIME
  SM(NAME_break, 2, T_debug, breakProgramObject,
     NAME_debugging, "set/clear break-point on object"),
  SM(NAME_trace, 2, T_debug, traceProgramObject,
     NAME_debugging, "set/clear trace-point on object"),
#endif /*O_RUNTIME*/
  SM(NAME_system, 1, "bool", systemProgramObject,
     NAME_meta, "System defined object?")
};

/* Get Methods */

static getdecl get_programObject[] =
{
#ifndef O_RUNTIME
  GM(NAME_break, 1, "bool", "port=[{enter,exit,fail}]", getBreakProgramObject,
     NAME_debugging, "Current setting of break-point"),
  GM(NAME_trace, 1, "bool", "port=[{enter,exit,fail}]", getTraceProgramObject,
     NAME_debugging, "Current setting of trace-point"),
#endif /*O_RUNTIME*/
  GM(NAME_system, 0, "bool", NULL, getSystemProgramObject,
     NAME_meta, "System defined object?")
};

/* Resources */

#define rc_programObject NULL
/*
static classvardecl rc_programObject[] =
{
};
*/

/* Class Declaration */

ClassDecl(programObject_decls,
          var_programObject, send_programObject,
	  get_programObject, rc_programObject,
          0, NULL,
          "$Rev$");


status
makeClassProgramObject(Class class)
{ declareClass(class, &programObject_decls);

  succeed;
}

