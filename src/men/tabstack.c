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
    { Tab t = cell->value;

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

  for_cell(cell, t->graphicals)
  { if ( instanceOfObject(cell->value, ClassTab) )
    { Tab tab = cell->value;
      Int X, Y;
      int x, y;

      get_xy_event(ev, tab, OFF, &X, &Y);
      x = valInt(X), y = valInt(Y);

      if ( y < 0 )			/* tab-bar */
      { if ( y > -valInt(tab->label_size->h) &&
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

  succeed;
}


static status
eraseTabStack(TabStack ts, Graphical gr)
{ if ( instanceOfObject(gr, ClassTab) )
  { Tab t = (Tab) gr;
    Tab newtop = NULL;

    if ( t->status == NAME_onTop )
    { if ( !(notNil(t->previous_top) &&
	     (newtop = (Tab)getMemberDevice((Device)ts, t->previous_top))) )
      { newtop = getNextChain(ts->graphicals, t);

	if ( !newtop )
	{ newtop = getHeadChain(ts->graphicals);
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
    { if ( t->label_offset != toInt(offset) )
      { changedLabelImageTab(t);	/* clear old and new location */
	send(t, NAME_labelOffset, toInt(offset), EAV);
	changedLabelImageTab(t);
      }
      offset += valInt(t->label_size->w);
    }
  }

  succeed;
}


static status
layoutDialogTabStack(TabStack ts, Size s)
{ int w, h;
  Tab first;
  Cell cell;

  if ( !(first = getHeadChain(ts->graphicals)) )
    succeed;				/* empty stack */
  if ( !instanceOfObject(first, ClassTab) )
    fail;

  if ( isDefault(s) )
  { struct area a;
    Tab last;
    int lw;

    for_cell(cell, ts->graphicals)
    { Graphical gr = cell->value;
      BoolObj old = gr->displayed;

      assign(gr, displayed, ON);	/* why? */
      send(cell->value, NAME_layoutDialog, EAV);
      assign(gr, displayed, old);
    }

    initHeaderObj(&a, ClassArea);
    a.x = a.y = a.w = a.h = ZERO;
    for_cell(cell, ts->graphicals)
    { Graphical gr = cell->value;

      unionNormalisedArea(&a, gr->area);
    }
    w = valInt(a.w);
    h = valInt(a.h);

    if ( !instanceOfObject((last=getTailChain(ts->graphicals)), ClassTab) )
      fail;
    lw = valInt(last->label_offset) + valInt(last->label_size->w);
    w = max(w, lw);
  } else
  { w = valInt(s->w);
    h = valInt(s->h);
  }

  h -= valInt(first->label_size->h);

  for_cell(cell, ts->graphicals)
  { Size sz = answerObject(ClassSize, toInt(w), toInt(h), EAV);

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
    { send(cell->value, NAME_status,
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

/* Class Declaration */

ClassDecl(tab_stack_decls,
          NULL, send_tab_stack, get_tab_stack, NULL,
          ARGC_UNKNOWN, NULL,
          "$Rev$");


status
makeClassTabStack(Class class)
{ declareClass(class, &tab_stack_decls);
  setRedrawFunctionClass(class, RedrawAreaTabStack);

  succeed;
}

