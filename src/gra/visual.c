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
#include <h/graphics.h>

static status
containedInVisual(VisualObj v, VisualObj super)
{ while( v != FAIL && notNil(v) )
  { if ( v == super )
      succeed;
    v = get(v, NAME_containedIn, EAV);
  }

  fail;
}


static VisualObj
getContainedInVisual(VisualObj v)
{ fail;
}


static Chain
getContainsVisual(VisualObj v)
{ fail;
}


static VisualObj
getContainerVisual(VisualObj v, Any cond)
{ while(v)
  { if ( instanceOfObject(cond, ClassClass) &&
	 instanceOfObject(v, (Class)cond) )
      return v;
    if ( instanceOfObject(cond, ClassCode) &&
	 forwardCodev(cond, 1, (Any *)&v) )
      return v;

    v = getv(v, NAME_containedIn, 0, NULL);
  }

  fail;
}


static VisualObj
getMasterVisual(VisualObj v)
{ answer(v);
}


status
resetVisual(VisualObj v)
{ Chain ch = get(v, NAME_contains, EAV);

  if ( ch != FAIL )
  { Cell cell;

    for_cell(cell, ch)
      send(cell->value, NAME_reset, EAV);

    doneObject(ch);
  }

  succeed;
}


static void
collectSubsVisual(VisualObj v, Chain ch, int root)
{ if ( root || !onFlag(v, F_PROTECTED|F_LOCKED|F_FREED|F_FREEING) )
  { Chain subs = getv(v, NAME_contains, 0, NULL);

    appendChain(ch, v);
    if ( subs )
    { Cell cell;
      for_cell(cell, subs)
	collectSubsVisual(cell->value, ch, FALSE);
    }
  }
}


status
destroyVisual(VisualObj v)
{ if ( !onFlag(v, F_PROTECTED|F_FREED) )
  { Chain subs = newObject(ClassChain, EAV);
    VisualObj sub;

    collectSubsVisual(v, subs, TRUE);
    for_chain(subs, sub,
	      { DEBUG(NAME_destroy, Cprintf("%s ->free\n", pp(sub)));
		sendv(sub, NAME_free, 0, NULL);
	      });
    freeObject(subs);

    succeed;
  }

  fail;
}



FrameObj
getFrameVisual(VisualObj v)
{ for(;;)
  { if ( instanceOfObject(v, ClassFrame) )
      answer((FrameObj)v);
    if ( !instanceOfObject(v, ClassVisual) ||
         !(v = get(v, NAME_containedIn, EAV)) )
      fail;
  }
}


Any
getReportToVisual(VisualObj v)
{ return getv(v, NAME_containedIn, 0, NULL);
}


status
reportVisual(VisualObj v, Name kind, CharArray fmt, int argc, Any *argv)
{ VisualObj super;
  status rval = FAIL;

  if ( (super = getv(v, NAME_reportTo, 0, NULL)) )
  { ArgVector(av, argc + 2);

    av[0] = kind;
    av[1] = fmt;
    copyArgs(argc, argv, &av[2]);

    if ( isNil(REPORTEE->value) )
    { Chain visited = answerObject(ClassChain, v, EAV);

      withLocalVars(assignVar(REPORTEE, visited, NAME_local);
		    rval = sendv(super, NAME_report, argc+2, av));

      doneObject(visited);
    } else
    { appendChain(REPORTEE->value, v);
      rval = sendv(super, NAME_report, argc+2, av);
    }
  }

  return rval;
}


status
alertReporteeVisual(Any v)
{ Any obj = (isNil(REPORTEE->value) ? v : getHeadChain(REPORTEE->value));

  while( obj && !isNil(obj) && !hasSendMethodObject(obj, NAME_alert) )
    obj = getv(obj, NAME_containedIn, 0, NULL);

  if ( obj && !isNil(obj) )
    send(obj, NAME_alert, EAV);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

/* Instance Variables */

#define var_visual NULL
/*
vardecl var_visual[] =
{
};
*/

/* Send Methods */

static senddecl send_visual[] =
{ SM(NAME_reset, 0, NULL, resetVisual,
     NAME_abort, "Send a ->reset to all contained objects"),
  SM(NAME_destroy, 0, NULL, destroyVisual,
     NAME_oms, "Destroy consists-of tree of visual objects"),
  SM(NAME_containedIn, 1, "visual", containedInVisual,
     NAME_organisation, "Test if I'm contained in argument"),
  SM(NAME_report, 3, T_report, reportVisual,
     NAME_report, "Report message (send to <-contained_in)")
};

/* Get Methods */

static getdecl get_visual[] =
{ GM(NAME_master, 0, "visual", NULL, getMasterVisual,
     NAME_event, "Principal visual I'm part of (self)"),
  GM(NAME_containedIn, 0, "visual", NULL, getContainedInVisual,
     NAME_organisation, "Visual I'm contained in (parent)"),
  GM(NAME_container, 1, "condition=visual", "class|code", getContainerVisual,
     NAME_organisation, "Innermost visual that satisfies condition"),
  GM(NAME_contains, 0, "chain", NULL, getContainsVisual,
     NAME_organisation, "Chain with visuals I manage"),
  GM(NAME_frame, 0, "frame", NULL, getFrameVisual,
     NAME_organisation, "Frame I'm part of (if present)"),
  GM(NAME_reportTo, 0, "visual", NULL, getReportToVisual,
     NAME_report, "Object for ->report (equivalent to <-contained_in")
};

/* Resources */

#define rc_visual NULL
/*
static classvardecl rc_visual[] =
{
};
*/

/* Class Declaration */


ClassDecl(visual_decls,
          var_visual, send_visual, get_visual, rc_visual,
          0, NULL,
          "$Rev$");

status
makeClassVisual(Class class)
{ return declareClass(class, &visual_decls);

  succeed;
}
