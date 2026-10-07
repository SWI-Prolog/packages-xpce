/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1995-2026, University of Amsterdam
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

static status
initialiseDisplayManager(DisplayManager dm)
{ assign(dm, members, newObject(ClassChain, EAV));
  assign(dm, focus_message, NIL);
  assign(dm, system_colours_message, NIL);
  assign(dm, inspect_handlers, newObject(ClassChain, EAV));

  obtainClassVariablesObject(dm);
  protectObject(dm);

  succeed;
}


/* Called from inputFocusFrame() when a frame gains keyboard focus.
   Forwards the frame to <-focus_message so tools (e.g. the symbol
   picker) can track the active window without polling.
*/

status
forwardFocusDisplayManager(Any focus)
{ DisplayManager dm = TheDisplayManager();

  if ( dm && notNil(dm->focus_message) )
  { Any av = focus;

    forwardCodev(dm->focus_message, 1, &av);
  }

  succeed;
}


status
appendDisplayManager(DisplayManager dm, DisplayObj d)
{ return appendChain(dm->members, d);
}


/* The inspect handlers are shared by all displays: the chain is also
   <-inspect_handlers of each display, so handlers added through any
   display or through @display apply to all of them.
*/

static status
inspectHandlerDisplayManager(DisplayManager dm, Handler h)
{ return addChain(dm->inspect_handlers, h);
}


static status
busyCursorDisplayManager(DisplayManager dm, CursorObj c, BoolObj block)
{ Cell cell;

  for_cell(cell, dm->members)
    busyCursorDisplay(cell->value, c, block);

  succeed;
}


static Chain
getFramesDisplayManager(DisplayManager dm)
{ Chain frames = answerObject(ClassChain, EAV);
  Cell dcell, fcell;

  for_cell(dcell, dm->members)
  { DisplayObj d = dcell->value;

    for_cell(fcell, d->frames)
      appendChain(frames, fcell->value);
  }

  answer(frames);
}


DisplayObj
getMemberDisplayManager(DisplayManager dm, Any name)
{ Cell cell;

  for_cell(cell, dm->members)
  { DisplayObj d = cell->value;

    if ( d->name == name ||
	 d->number == name )
      answer(d);
  }

  fail;
}


status
deleteDisplayManager(DisplayManager dm, DisplayObj d)
{ return deleteChain(dm->members, d);
}


/* A removed (hotplug) display stays a member while it has frames, and
   the last display is kept as a parking place for the frames until a
   display is added.  Removed displays are only used if there is no other
   display, so there is always a display, but never a stale one if there
   is a live display.
*/

DisplayObj
getPrimaryDisplayManager(DisplayManager dm)
{ DisplayObj live = NULL;
  Cell cell;

  for_cell(cell, dm->members)
  { DisplayObj dsp = cell->value;

    if ( isOn(dsp->removed) )
      continue;
    if ( isOn(dsp->primary) )
      answer(dsp);
    if ( !live )
      live = dsp;
  }

  if ( live )
    answer(live);

  answer(getHeadChain(dm->members));
}


static DisplayObj
getCurrentDisplayManager(DisplayManager dm)
{ DisplayObj dsp = ws_last_display_from_event();

  if ( dsp && isOff(dsp->removed) )
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

  if ( ChangedWindows && !emptyChain(ChangedWindows) )
  { PceWindow sw = WindowOfLastEvent();

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


/* Redraw a window and the windows displayed inside it */

/* Redraw all windows after colours changed their value in place, e.g.,
 * after changing the value of theme colours.  Windows may be nested in
 * other devices, e.g., the tabs of a tab_stack, so we walk all devices
 * of each frame using an agenda.
 *
 * Some applications paint colours into images or compute colours from
 * other colours.  A frame or graphical whose class defines
 * ->colours_changed is sent this message before it is redrawn, so it
 * can update these.  The built-in classes do not define it.
 */

static void
notify_colours_changed(Any obj)
{ if ( !isFreeingObj(obj) &&
       getSendMethodClass(classOfObject(obj), NAME_coloursChanged) )
    send(obj, NAME_coloursChanged, EAV);
}


static status
coloursChangedDisplayManager(DisplayManager dm)
{ Chain agenda = answerObject(ClassChain, EAV);
  Cell dcell, fcell;

  for_cell(dcell, dm->members)
  { DisplayObj d = dcell->value;

    for_cell(fcell, d->frames)
      appendChain(agenda, fcell->value);
  }

  Any obj;
  while( (obj = getDeleteHeadChain(agenda)) )
  { notify_colours_changed(obj);
    if ( isFreeingObj(obj) )
      continue;

    if ( instanceOfObject(obj, ClassFrame) )
    { Cell cell;

      for_cell(cell, ((FrameObj)obj)->members)
	appendChain(agenda, cell->value);
      continue;
    }
    if ( instanceOfObject(obj, ClassWindow) )
      redrawWindow(obj, DEFAULT);
    if ( instanceOfObject(obj, ClassDevice) )
    { Cell cell;

      for_cell(cell, ((Device)obj)->graphicals)
	appendChain(agenda, cell->value);
    }
  }
  doneObject(agenda);

  succeed;
}

/* Fonts changed their size or family in place, e.g., after changing
 * font.scale or font.pango_families: reload them and recompute and
 * redraw all windows.  Everything that shows text has to recompute its
 * size, and dialogs and frames their layout.  A frame or graphical
 * whose class defines ->fonts_changed is sent this message, so it can
 * drop what it computed from the font metrics.  A graphical gets it
 * after its contents were recomputed, just before it is recomputed
 * itself.  As for ->colours_changed, we walk all devices of each frame
 * using an agenda.
 */

static status
fontsChangedDisplayManager(DisplayManager dm)
{ Chain agenda  = answerObject(ClassChain, EAV);
  Chain graphicals = answerObject(ClassChain, EAV);
  Chain dialogs = answerObject(ClassChain, EAV);
  Chain frames  = answerObject(ClassChain, EAV);
  Cell dcell, fcell;

  reloadFonts();

  for_cell(dcell, dm->members)
  { DisplayObj d = dcell->value;

    for_cell(fcell, d->frames)
    { appendChain(agenda, fcell->value);
      appendChain(frames, fcell->value);
    }
  }

  Any obj;
  while( (obj = getDeleteHeadChain(agenda)) )
  { if ( isFreeingObj(obj) )
      continue;

    if ( instanceOfObject(obj, ClassFrame) )
    { Cell cell;

      if ( getSendMethodClass(classOfObject(obj), NAME_fontsChanged) )
	send(obj, NAME_fontsChanged, EAV);

      for_cell(cell, ((FrameObj)obj)->members)
	appendChain(agenda, cell->value);
      continue;
    }
    if ( instanceOfObject(obj, ClassGraphical) )
    { requestComputeGraphical(obj, DEFAULT);
      prependChain(graphicals, obj);	/* contents before devices */
    }
    if ( instanceOfObject(obj, ClassDialog) )
      appendChain(dialogs, obj);
    if ( instanceOfObject(obj, ClassWindow) )
      redrawWindow(obj, DEFAULT);
    if ( instanceOfObject(obj, ClassDevice) )
    { Cell cell;

      for_cell(cell, ((Device)obj)->graphicals)
	appendChain(agenda, cell->value);
    }
  }

				/* compute now, contents first, so */
				/* the layout below uses the new sizes */
  while( (obj = getDeleteHeadChain(graphicals)) )
  { if ( isFreeingObj(obj) )
      continue;
    if ( getSendMethodClass(classOfObject(obj), NAME_fontsChanged) )
      send(obj, NAME_fontsChanged, EAV);
    ComputeGraphical(obj);
  }
  while( (obj = getDeleteHeadChain(dialogs)) )
  { if ( !isFreeingObj(obj) )
      send(obj, NAME_layout, EAV);
  }
  while( (obj = getDeleteHeadChain(frames)) )
  { if ( !isFreeingObj(obj) && createdFrame(obj) )
      send(obj, NAME_resize, EAV);
  }

  doneObject(agenda);
  doneObject(graphicals);
  doneObject(dialogs);
  doneObject(frames);

  succeed;
}


/* Called if the user changed the desktop settings: reload the system
 * colours (sys_*, etc.), make the theme colours follow them, send
 * <-system_colours_message, which allows the application to select
 * another theme, and redraw all windows.
 */

static status
systemColoursChangedDisplayManager(DisplayManager dm)
{ int changed = ws_reload_system_colours();

  if ( changed > 0 )
    invalidateThemeColours();	/* may be derived from system colours */

  if ( notNil(dm->system_colours_message) )
  { forwardCodev(dm->system_colours_message, 0, NULL);
    changed++;
  }

  if ( changed > 0 )
    return coloursChangedDisplayManager(dm);

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
hasVisibleFramesDisplayManager(DisplayManager dm, BoolObj keep_alive)
{ if ( notNil(dm->members) )
  { Cell cell;

    for_cell(cell, dm->members)
    { DisplayObj dsp = cell->value;
      if ( !onFlag(dsp, F_FREED|F_FREEING) )
      { if ( hasVisibleFramesDisplay(dsp, keep_alive) )
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
     NAME_event, "Test queue in event-loop"),
  IV(NAME_focusMessage, "code*", IV_BOTH,
     NAME_event, "Sent with the frame that gained keyboard focus"),
  IV(NAME_systemColoursMessage, "code*", IV_BOTH,
     NAME_colour, "Sent after reloading the system colours"),
  IV(NAME_inspectHandlers, "chain", IV_GET,
     NAME_event, "Handlers to support inspector tools (all displays)")
};

static char *T_busyCursor[] =
        { "cursor=[cursor]*", "block_input=[bool]" };

/* Send Methods */

static senddecl send_displayManager[] =
{ SM(NAME_initialise, 0, NULL, initialiseDisplayManager,
     DEFAULT, "Create the display manager"),
  SM(NAME_append, 1, "display", appendDisplayManager,
     NAME_display, "Attach a new display to the manager"),
  SM(NAME_redraw, 0, NULL, redrawDisplayManager,
     NAME_event, "Flush all pending changes to the screen"),
  SM(NAME_hasVisibleFrames, 1, "keep_alive=[bool]",
     hasVisibleFramesDisplayManager,
     NAME_organisation, "True if there is a visible (keep_alive) frame"),
  SM(NAME_systemColoursChanged, 0, NULL, systemColoursChangedDisplayManager,
     NAME_colour, "Reload the system colours and redraw"),
  SM(NAME_fontsChanged, 0, NULL, fontsChangedDisplayManager,
     NAME_font, "Reload fonts, recompute and redraw all windows"),
  SM(NAME_coloursChanged, 0, NULL, coloursChangedDisplayManager,
     NAME_colour, "Tell frames and graphicals colours changed and redraw"),
  SM(NAME_inspectHandler, 1, "handler", inspectHandlerDisplayManager,
     NAME_event, "Register handler for inspector tools"),
  SM(NAME_busyCursor, 2, T_busyCursor, busyCursorDisplayManager,
     NAME_event, "Define (temporary) cursor for all frames on all displays")
};

/* Get Methods */

static getdecl get_displayManager[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsDisplayManager,
     DEFAULT, "Contained displays"),
  GM(NAME_primary, 0, "display", NULL, getPrimaryDisplayManager,
     NAME_display, "Get the primary display"),
  GM(NAME_current, 0, "display", NULL, getCurrentDisplayManager,
     NAME_current, "Get the current display"),
  GM(NAME_member, 1, "display", "name|1..", getMemberDisplayManager,
     NAME_display, "Find display from name or number"),
  GM(NAME_frames, 0, "chain", NULL, getFramesDisplayManager,
     NAME_organisation, "New chain with the frames of all displays"),
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
          0, NULL);


status
makeClassDisplayManager(Class class)
{ declareClass(class, &displayManager_decls);

  globalObject(NAME_displayManager, ClassDisplayManager, EAV);
  DispatchEvents = dispatch_events;

  succeed;
}
