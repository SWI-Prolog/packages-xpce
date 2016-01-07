/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

static status
initialiseDisplayManager(DisplayManager dm)
{ assign(dm, members, newObject(ClassChain, EAV));
  assign(dm, current, newObject(ClassChain, EAV));

  protectObject(dm);

  succeed;
}


status
appendDisplayManager(DisplayManager dm, DisplayObj d)
{ appendChain(dm->members, d);
  if ( emptyChain(dm->current) )
    prependChain(dm->current, d);

  succeed;
}


DisplayObj
getMemberDisplayManager(DisplayManager dm, Name address)
{ Cell cell;

  for_cell(cell, dm->members)
  { DisplayObj d = cell->value;

    if ( d->address == address )
      answer(d);
  }

  fail;
}


static status
currentDisplayManager(DisplayManager dm, DisplayObj d)
{ return prependChain(dm->current, d);
}


static DisplayObj
getCurrentDisplayManager(DisplayManager dm)
{ if ( emptyChain(dm->current) )
  { realiseClass(ClassDisplay);

    if ( emptyChain(dm->current) )
    { errorPce(dm, NAME_noCurrentDisplay);
      fail;
    }
  }

  answer(dm->current->head->value);
}


static status
popCurrentDisplayManager(DisplayManager dm)
{ if ( getSizeChain(dm->current) == ONE )
    return errorPce(dm, NAME_stackEmpty, NAME_current);

  return deleteHeadChain(dm->current);
}


DisplayObj
CurrentDisplay(Any obj)
{ DisplayObj d;

  if ( instanceOfObject(obj, ClassGraphical) &&
       (d = getDisplayGraphical((Graphical) obj)) )
    return d;

  return getCurrentDisplayManager(TheDisplayManager());
}


static PceWindow
getWindowOfLastEventDisplayManager(DisplayManager dm)
{ PceWindow sw = WindowOfLastEvent();

  answer(sw);
}


static status
eventQueuedDisplayManager(DisplayManager dm)
{ Cell cell;

  for_cell(cell, dm->members)
  { if ( ws_events_queued_display(cell->value) )
      succeed;
  }

  fail;
}

#define TestBreakDraw(dm) \
	if ( dm->test_queue == ON && \
	     eventQueuedDisplayManager(dm) ) \
	  fail;

static status
redrawDisplayManager(DisplayManager dm)
{ if ( ChangedWindows && !emptyChain(ChangedWindows) )
  { PceWindow sw = WindowOfLastEvent();

    obtainClassVariablesObject(dm);

    TestBreakDraw(dm);
    if ( sw && memberChain(ChangedWindows, sw) )
      RedrawWindow(sw);

    while( !emptyChain(ChangedWindows) )
    { TestBreakDraw(dm);

      for_chain(ChangedWindows, sw,
		{ if ( !instanceOfObject(sw, ClassWindowDecorator) )
		    RedrawWindow(sw);
		});

      TestBreakDraw(dm);

      for_chain(ChangedWindows, sw,
		{ if ( instanceOfObject(sw, ClassWindowDecorator) )
		    RedrawWindow(sw);
		});
    }
  }

  succeed;
}


status
RedrawDisplayManager(DisplayManager dm)
{ return sendv(dm, NAME_redraw, 0, NULL);
}


status
dispatchDisplayManager(DisplayManager dm, Int fd, Int timeout)
{ if ( isDefault(timeout) )
    timeout = toInt(250);

  return ws_dispatch(fd, timeout);
}


static status
dispatch_events(int fd, int timeout)
{ return dispatchDisplayManager(TheDisplayManager(),
				fd >= 0 ? toInt(fd) : NIL,
				toInt(timeout));
}

		/********************************
		*             VISUAL		*
		********************************/

static Chain
getContainsDisplayManager(DisplayManager dm)
{ answer(dm->members);
}


		 /*******************************
		 *	     GLOBAL		*
		 *******************************/

DisplayManager
TheDisplayManager()
{ static DisplayManager dm = NULL;

  if ( !dm )
    dm = findGlobal(NAME_displayManager);

  return dm;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_dispatch[] =
        { "file_descriptor=[int]", "milliseconds=[int]*" };

/* Instance Variables */

static vardecl var_displayManager[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_display, "Available displays"),
  IV(NAME_current, "chain", IV_NONE,
     NAME_current, "Stack with current displays"),
  IV(NAME_testQueue, "bool", IV_BOTH,
     NAME_event, "Test queue in event-loop")
};

/* Send Methods */

static senddecl send_displayManager[] =
{ SM(NAME_initialise, 0, NULL, initialiseDisplayManager,
     DEFAULT, "Create the display manager"),
  SM(NAME_current, 1, "display", currentDisplayManager,
     NAME_current, "Make display the current display"),
  SM(NAME_popCurrent, 0, NULL, popCurrentDisplayManager,
     NAME_current, "Pop the current stack"),
  SM(NAME_append, 1, "display", appendDisplayManager,
     NAME_display, "Attach a new display to the manager"),
  SM(NAME_dispatch, 2, T_dispatch, dispatchDisplayManager,
     NAME_event, "Dispatch events for 1/4th second"),
  SM(NAME_redraw, 0, NULL, redrawDisplayManager,
     NAME_event, "Flush all pending changes to the screen")
};

/* Get Methods */

static getdecl get_displayManager[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsDisplayManager,
     DEFAULT, "Contained displays"),
  GM(NAME_current, 0, "display", NULL, getCurrentDisplayManager,
     NAME_current, "Get the current display"),
  GM(NAME_member, 1, "display", "name", getMemberDisplayManager,
     NAME_display, "Find display for specified address"),
  GM(NAME_windowOfLastEvent, 0, "window", NULL, getWindowOfLastEventDisplayManager,
     NAME_event, "Find window that received last event")
};

/* Resources */

static classvardecl rc_displayManager[] =
{ RC(NAME_testQueue, "bool", "@on", NULL)
};

/* Class Declaration */

ClassDecl(displayManager_decls,
          var_displayManager, send_displayManager,
	  get_displayManager, rc_displayManager,
          0, NULL,
          "$Rev$");


status
makeClassDisplayManager(Class class)
{ declareClass(class, &displayManager_decls);

  globalObject(NAME_displayManager, ClassDisplayManager, EAV);
  DispatchEvents = dispatch_events;

  succeed;
}

