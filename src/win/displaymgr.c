/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2025, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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
#ifdef X11_GRAPHICS
#include <x11/include.h>
#include <time.h>
#endif

static status
initialiseDisplayManager(DisplayManager dm)
{ assign(dm, members, newObject(ClassChain, EAV));

  protectObject(dm);

  succeed;
}


status
appendDisplayManager(DisplayManager dm, DisplayObj d)
{ return appendChain(dm->members, d);
}


DisplayObj
getMemberDisplayManager(DisplayManager dm, Name name)
{ Cell cell;

  for_cell(cell, dm->members)
  { DisplayObj d = cell->value;

    if ( d->name == name )
      answer(d);
  }

  fail;
}


status
deleteDisplayManager(DisplayManager dm, DisplayObj d)
{ return deleteChain(dm->members, d);
}


static DisplayObj
getPrimaryDisplayManager(DisplayManager dm)
{ Cell cell;

  for_cell(cell, dm->members)
  { DisplayObj dsp = cell->value;

    if ( isOn(dsp->primary) )
      answer(dsp);
  }

  answer(getHeadChain(dm->members));
}


static DisplayObj
getCurrentDisplayManager(DisplayManager dm)
{ DisplayObj dsp = ws_last_display_from_event();

  if ( dsp )
    answer(dsp);
  answer(getPrimaryDisplayManager(dm));
}


DisplayObj
CurrentDisplay(Any obj)
{ DisplayObj dsp;

  if ( instanceOfObject(obj, ClassDisplay) )
    return obj;
  if ( instanceOfObject(obj, ClassGraphical) &&
       (dsp = getDisplayGraphical((Graphical) obj)) )
    return dsp;

  answer(getCurrentDisplayManager(TheDisplayManager()));
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
{
#ifdef X11_GRAPHICS
  if ( MappedFrames && !emptyChain(MappedFrames) )
  { FrameObj fr;

    for_chain(MappedFrames, fr,
	      { DEBUG(NAME_delay,
		      Cprintf("[%d] x_frame_realize_geometry(%s)\n",
			      (int)(time(NULL)%1000), pp(fr)));
		x_frame_realize_geometry(fr);
		deleteChain(MappedFrames, fr);
		DEBUG(NAME_delay,
		      Cprintf("[%d]   done\n",
			      (int)(time(NULL)%1000)));
	      });
  }
#endif

  if ( ChangedWindows && !emptyChain(ChangedWindows) )
  { PceWindow sw = WindowOfLastEvent();

    obtainClassVariablesObject(dm);

    TestBreakDraw(dm);
    if ( sw && memberChain(ChangedWindows, sw) )
      pceRedrawWindow(sw);

    while( !emptyChain(ChangedWindows) )
    { TestBreakDraw(dm);

      for_chain(ChangedWindows, sw,
		{ if ( !instanceOfObject(sw, ClassWindowDecorator) )
		    pceRedrawWindow(sw);
		});

      TestBreakDraw(dm);

      for_chain(ChangedWindows, sw,
		{ if ( instanceOfObject(sw, ClassWindowDecorator) )
		    pceRedrawWindow(sw);
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
dispatchDisplayManager(DisplayManager dm, IOSTREAM *fd, Int timeout)
{ if ( isDefault(timeout) )
    timeout = toInt(250);

  return ws_dispatch(fd, timeout);
}


static status
dispatch_events(IOSTREAM *fd, int timeout)
{ return dispatchDisplayManager(TheDisplayManager(),
				fd,
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
TheDisplayManager(void)
{ static DisplayManager dm = NULL;

  if ( !dm )
    dm = findGlobal(NAME_displayManager);

  return dm;
}

static status
hasVisibleFramesDisplayManager(DisplayManager dm)
{ if ( notNil(dm->members) )
  { Cell cell;

    for_cell(cell, dm->members)
    { DisplayObj dsp = cell->value;
      if ( !onFlag(dsp, F_FREED|F_FREEING) )
      { if ( hasVisibleFramesDisplay(dsp) )
	  succeed;
      }
    }
  }

  fail;
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Instance Variables */

static vardecl var_displayManager[] =
{ IV(NAME_members, "chain", IV_GET,
     NAME_display, "Available displays"),
  IV(NAME_testQueue, "bool", IV_BOTH,
     NAME_event, "Test queue in event-loop")
};

/* Send Methods */

static senddecl send_displayManager[] =
{ SM(NAME_initialise, 0, NULL, initialiseDisplayManager,
     DEFAULT, "Create the display manager"),
  SM(NAME_append, 1, "display", appendDisplayManager,
     NAME_display, "Attach a new display to the manager"),
  SM(NAME_redraw, 0, NULL, redrawDisplayManager,
     NAME_event, "Flush all pending changes to the screen"),
  SM(NAME_hasVisibleFrames, 0, NULL, hasVisibleFramesDisplayManager,
     NAME_organisation, "True if there is at least one visible frame")
};

/* Get Methods */

static getdecl get_displayManager[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsDisplayManager,
     DEFAULT, "Contained displays"),
  GM(NAME_primary, 0, "display", NULL, getPrimaryDisplayManager,
     NAME_display, "Get the primary display"),
  GM(NAME_current, 0, "display", NULL, getCurrentDisplayManager,
     NAME_current, "Get the current display"),
  GM(NAME_member, 1, "display", "name", getMemberDisplayManager,
     NAME_display, "Find display from name"),
  GM(NAME_windowOfLastEvent, 0, "window", NULL,
     getWindowOfLastEventDisplayManager,
     NAME_event, "Find window that received last event")
};

/* Resources */

static classvardecl rc_displayManager[] =
{ RC(NAME_testQueue, "bool", UXWIN("@off", "@on"), NULL)
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
