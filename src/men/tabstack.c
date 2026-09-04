/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1996-2011, University of Amsterdam
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
#include <h/dialog.h>

static Tab	getOnTopTabStack(TabStack ts);


		 /*******************************
		 *	      LABELS		*
		 *******************************/

/* <-hide_single_label leaves the whole of the stack to a tab that is the
   only one in it: a label naming the one thing there is says nothing, and
   costs a strip across the top.  The label comes back as soon as there is
   a second tab to tell it from.
*/

static int
count_tabs(TabStack ts)
{ Cell cell;
  int n = 0;

  for_cell(cell, ts->graphicals)
  { if ( instanceOfObject(cell->value, ClassTab) )
      n++;
  }

  return n;
}


/* The stack holds the buttons on the label row and the editor over a
   label being renamed as well as the tabs, so what is meant for a tab has
   to say so.
*/

static bool
is_tab(Any gr)
{ return instanceOfObject(gr, ClassTab) && !isFreedObj(gr);
}


static Tab
first_tab(TabStack ts, bool last)
{ Tab found = NULL;
  Cell cell;

  for_cell(cell, ts->graphicals)
  { if ( is_tab(cell->value) )
    { found = cell->value;
      if ( !last )
	return found;
    }
  }

  return found;
}


static Tab
next_tab(TabStack ts, Tab t)
{ Cell cell;
  bool seen = false;

  for_cell(cell, ts->graphicals)
  { if ( cell->value == t )
      seen = true;
    else if ( seen && is_tab(cell->value) )
      return cell->value;
  }

  return NULL;
}


bool
labelsShownTabStack(TabStack ts)
{ return ts->hide_single_label != ON || count_tabs(ts) > 1;
}


/* Appending or erasing a tab can be what turns the labels on or off, and
   that changes the room every tab has.  The stack does not do the sizing
   -- whoever holds it does, from ->resize -- so ask for it again.
*/

static void
relayout_tab_stack(TabStack ts)
{ Cell cell;

  for_cell(cell, ts->graphicals)
  { Graphical gr = cell->value;

    if ( !instanceOfObject(gr, ClassTab) )
      continue;

    requestComputeGraphical(gr, DEFAULT);
    ComputeGraphical(gr);		/* a tab sits <-label_height below */
    setGraphical(gr, ZERO, ZERO, DEFAULT, DEFAULT);   /* its own top-left,
					   so it has to be put back */
  }

  if ( notNil(ts->device) && hasSendMethodObject(ts->device, NAME_resize) )
    send(ts->device, NAME_resize, EAV);
}


static void
labels_may_have_changed_tab_stack(TabStack ts)
{ if ( ts->hide_single_label == ON && count_tabs(ts) <= 2 )
    relayout_tab_stack(ts);
}


static status
hideSingleLabelTabStack(TabStack ts, BoolObj hide)
{ if ( ts->hide_single_label != hide )
  { assign(ts, hide_single_label, hide);
    relayout_tab_stack(ts);
  }

  succeed;
}


		/********************************
		*            CREATE		*
		********************************/

static status
initialiseTabStack(TabStack t, int argc, Tab tabs[])
{ int n;

  initialiseDevice((Device) t);
  for(n=0; n<argc; n++)
    TRY(send(t, NAME_append, tabs[n], EAV));

  succeed;
}

		 /*******************************
		 *	      REDRAW		*
		 *******************************/

static status
RedrawAreaTabStack(TabStack t, Area a)
{ Device dev = (Device) t;
  device_draw_context ctx;

  if ( EnterRedrawAreaDevice(dev, a, &ctx) )
  { Cell cell;

    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;
      Tab t;

      if ( !instanceOfObject(gr, ClassTab) )
      { RedrawArea(gr, a);		/* a button or an editor on the */
	continue;			/* label row */
      }

      t = (Tab)gr;
      if ( t->status == NAME_onTop )
	RedrawArea(t, a);
      else
	RedrawArea(t, t->area);		/* ignore the fact that it is not */
					/* displayed if not on top */
    }

    ExitRedrawAreaDevice(dev, a, &ctx);
  }

  return RedrawAreaGraphical(dev, a);
}

		 /*******************************
		 *	       EVENT		*
		 *******************************/

static status
eventTabStack(TabStack t, EventObj ev)
{ Cell cell;

  /* The label row belongs to the tab it names, so a button sitting on it
     would never be reached: the loop below hands the event to the tab.
     Offer it to what is not a tab first.
  */

  for_cell(cell, t->graphicals)
  { Graphical gr = cell->value;
    Int X, Y;

    if ( instanceOfObject(gr, ClassTab) || gr->displayed != ON )
      continue;

    if ( get_xy_event(ev, gr, ON, &X, &Y) &&
	 valInt(X) >= 0 && valInt(X) < valInt(gr->area->w) &&
	 valInt(Y) >= 0 && valInt(Y) < valInt(gr->area->h) &&
	 postEvent(ev, gr, DEFAULT) )
      succeed;
  }

  for_cell(cell, t->graphicals)
  { if ( instanceOfObject(cell->value, ClassTab) )
    { Tab tab = cell->value;
      Int X, Y;
      int x, y;

      get_xy_event(ev, tab, OFF, &X, &Y);
      x = valInt(X), y = valInt(Y);

      if ( y < 0 )			/* tab-bar */
      { int lh = labelHeightTab(tab);

	if ( lh > 0 && y > -lh &&
	     x > valInt(tab->label_offset) &&
	     x < valInt(tab->label_offset) + valInt(tab->label_size->w) )
	{ if ( postNamedEvent(ev, (Graphical)tab, DEFAULT, NAME_labelEvent) )
	    succeed;
	}
      }
    }
  }

  return eventDevice(t, ev);
}


		 /*******************************
		 *	     MEMBERS		*
		 *******************************/

static status
appendTabStack(TabStack ts, Tab t)
{ setGraphical(t, ZERO, ZERO, DEFAULT, DEFAULT);
  displayDevice(ts, t, DEFAULT);

  if ( ts->graphicals->size == ONE )
  { send(t, NAME_status, NAME_onTop, EAV);
  } else
  { send(t, NAME_status, NAME_hidden, EAV);
    send(ts, NAME_layoutLabels, EAV);
  }
  labels_may_have_changed_tab_stack(ts);

  succeed;
}


static status
eraseTabStack(TabStack ts, Graphical gr)
{ if ( instanceOfObject(gr, ClassTab) )
  { Tab t = (Tab) gr;
    Tab newtop = NULL;

    if ( t->status == NAME_onTop )
    { if ( !(notNil(t->previous_top) &&
	     (newtop = (Tab)getMemberDevice((Device)ts, t->previous_top)) &&
	     is_tab(newtop)) )
      { newtop = next_tab(ts, t);	/* not getNextChain(): what follows */
					/* may be a button on the label row */
	if ( !newtop )
	{ newtop = first_tab(ts, false);
	  if ( newtop == t )
	    newtop = NULL;
	}
      }
    } else
      changedLabelImageTab(t);

    eraseDevice((Device)ts, gr);
    send(ts, NAME_layoutLabels, EAV);
    if ( newtop )
      send(ts, NAME_onTop, newtop, EAV);
    labels_may_have_changed_tab_stack(ts);
  } else
    eraseDevice((Device)ts, gr);

  succeed;
}


		 /*******************************
		 *	      LAYOUT		*
		 *******************************/

static status
layoutLabelsTabStack(TabStack ts)
{ int offset = 0;
  Cell cell;

  for_cell(cell, ts->graphicals)
  { Tab t = cell->value;

    if ( instanceOfObject(t, ClassTab) )
    { ComputeGraphical(t);		/* the label box may have a new width */

      if ( t->label_offset != toInt(offset) )
      { changedLabelImageTab(t);	/* clear old and new location */
	send(t, NAME_labelOffset, toInt(offset), EAV);
	changedLabelImageTab(t);
      }
      offset += valInt(t->label_size->w);
    }
  }

  send(ts, NAME_labelsLaidOut, EAV);	/* a hook: see library(tabbed_window),
					   which puts buttons on the labels */

  succeed;
}


static status
labelsLaidOutTabStack(TabStack ts)
{ succeed;				/* nothing to do here; see above */
}


static status
layoutDialogTabStack(TabStack ts, Size s)
{ int w, h;
  Tab first;
  Cell cell;

  if ( !(first = first_tab(ts, false)) )
    succeed;				/* no tabs */

  if ( isDefault(s) )
  { struct area a;
    Tab last;
    int lw;

    for_cell(cell, ts->graphicals)
    { Graphical gr = cell->value;
      BoolObj old;

      if ( !instanceOfObject(gr, ClassTab) )
	continue;

      old = gr->displayed;
      assign(gr, displayed, ON);	/* why? */
      send(gr, NAME_layoutDialog, EAV);
      assign(gr, displayed, old);
    }

    initHeaderObj(&a, ClassArea);
    a.x = a.y = a.w = a.h = ZERO;
    for_cell(cell, ts->graphicals)
    { Graphical gr = cell->value;

      if ( instanceOfObject(gr, ClassTab) )
	unionNormalisedArea(&a, gr->area);
    }
    w = valInt(a.w);
    h = valInt(a.h);

    if ( !(last = first_tab(ts, true)) )
      fail;
    lw = labelsShownTabStack(ts)
		? valInt(last->label_offset) + valInt(last->label_size->w)
		: 0;
    w = max(w, lw);
  } else
  { w = valInt(s->w);
    h = valInt(s->h);
  }

  h -= labelHeightTab(first);

  for_cell(cell, ts->graphicals)
  { Size sz;

    if ( !instanceOfObject(cell->value, ClassTab) )
      continue;				/* a button keeps its own size */

    sz = answerObject(ClassSize, toInt(w), toInt(h), EAV);
    send(cell->value, NAME_size, sz, EAV);
  }

  succeed;
}


static status
onTopTabStack(TabStack ts, Tab t)
{ if ( t->status != NAME_onTop )
  { Cell cell;
    Tab prev;

    if ( (prev = getOnTopTabStack(ts)) )
    { assign(t, previous_top, prev->name);
      DEBUG(NAME_tabStack,
	    Cprintf("Set %s->previous_top to %s\n", pp(t), pp(prev->name)));
    }

    for_cell(cell, ts->graphicals)
    { if ( !instanceOfObject(cell->value, ClassTab) )
	continue;

      send(cell->value, NAME_status,
	   (Tab)cell->value == t ? NAME_onTop : NAME_hidden, EAV);
    }

    send(t, NAME_advance, EAV);		/* initialise keyboard focus */
  }

  succeed;
}


static Tab
getOnTopTabStack(TabStack ts)
{ Cell cell;

  for_cell(cell, ts->graphicals)
  { if ( instanceOfObject(cell->value, ClassTab) )
    { Tab t = cell->value;

      if ( t->status == NAME_onTop )
	answer(t);
    }
  }

  fail;
}


/* Instance Variables */

static vardecl var_tab_stack[] =
{ SV(NAME_hideSingleLabel, "bool", IV_GET|IV_STORE, hideSingleLabelTabStack,
     NAME_appearance, "Give a lone tab the room its label would take")
};

/* Send Methods */

static senddecl send_tab_stack[] =
{ SM(NAME_initialise, 1, "member=tab ...", initialiseTabStack,
     DEFAULT, "Create from list of tab objects"),
  SM(NAME_event, 1, "event", eventTabStack,
     NAME_event, "Process an event"),
  SM(NAME_append, 1, "tab", appendTabStack,
     NAME_organisation, "Append a tab object"),
  SM(NAME_erase, 1, "graphical", eraseTabStack,
     NAME_organisation, "Erase a tab (or graphical)"),
  SM(NAME_layoutLabels, 0, NULL, layoutLabelsTabStack,
     NAME_layout, "Assign positions for the labels"),
  SM(NAME_labelsLaidOut, 0, NULL, labelsLaidOutTabStack,
     NAME_layout, "The labels have been given their places"),
  SM(NAME_layoutDialog, 1, "[size]", layoutDialogTabStack,
     NAME_layout, "Adjust the members"),
  SM(NAME_onTop, 1, "member:tab", onTopTabStack,
     NAME_stack, "Put indicated tab on top of the others")
};

/* Get Methods */

static getdecl get_tab_stack[] =
{ GM(NAME_onTop, 0, "tab", NULL, getOnTopTabStack,
     NAME_stack, "Find tab on top")
};

/* Resources */

static classvardecl rc_tab_stack[] =
{ RC(NAME_hideSingleLabel, "bool", "@off",
     "Give a lone tab the room its label would take")
};

/* Class Declaration */

ClassDecl(tab_stack_decls,
          var_tab_stack, send_tab_stack, get_tab_stack, rc_tab_stack,
          ARGC_UNKNOWN, NULL);


status
makeClassTabStack(Class class)
{ declareClass(class, &tab_stack_decls);
  setRedrawFunctionClass(class, RedrawAreaTabStack);

  succeed;
}

