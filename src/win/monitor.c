/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2006-2011, University of Amsterdam
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


status
initialiseMonitor(Monitor m, Name name, Area a)
{ if ( isDefault(name) )
    name = NIL;

  assign(m, name, name);
  assign(m, area, a);

  succeed;
}


static Monitor
getConvertMonitor(Class class, Any value)
{ DisplayObj d = CurrentDisplay(NIL);

  if ( d )
  { Chain ch = get(d, NAME_monitors, EAV);

    if ( ch && instanceOfObject(ch, ClassChain) )
    { if ( isInteger(value) )
	return getNth0Chain(ch, value);
    } else
    { Cell cell;

      for_cell(cell, ch)
      { Monitor m = cell->value;

	if ( m->name == value)
	  return m;
      }
    }
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=[name|int]*", "area=area" };

/* Instance Variables */

static vardecl var_monitor[] =
{ IV(NAME_name, "name|int*", IV_GET,
     NAME_name, "Name of the monitor"),
  IV(NAME_area, "area", IV_GET,
     NAME_area, "Area of the monitor"),
  IV(NAME_workArea, "area*", IV_GET,
     NAME_area, "User area of the monitor"),
  IV(NAME_primary, "bool*", IV_GET,
     NAME_monitor, "If @on, this is the primary monitor")
};

/* Send Methods */

static senddecl send_monitor[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseMonitor,
     DEFAULT, "Create monitor from name and area")
};

/* Get Methods */

static getdecl get_monitor[] =
{ GM(NAME_convert, 1, "monitor", "int|name", getConvertMonitor,
     DEFAULT, "Convert number or name of monitor"),
};

/* Resources */

#define rc_monitor NULL
/*
static classvardecl rc_monitor[] =
{
};
*/

/* Class Declaration */

static Name monitor_termnames[] = { NAME_name, NAME_area };

ClassDecl(monitor_decls,
          var_monitor, send_monitor, get_monitor, rc_monitor,
          2, monitor_termnames,
          "$Rev$");


status
makeClassMonitor(Class class)
{ return declareClass(class, &monitor_decls);
}

