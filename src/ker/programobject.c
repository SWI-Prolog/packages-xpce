/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/trace.h>

status
initialiseProgramObject(Any obj)
{ ProgramObject o = obj;

  o->dflags = (unsigned long) ZERO | D_SYSTEM;

  succeed;
}


status
initialiseNewSlotProgramObject(ProgramObject obj, Variable var)
{ if ( var->name == NAME_dflags )
    obj->dflags = (unsigned long) ZERO;

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

