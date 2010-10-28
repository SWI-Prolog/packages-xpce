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
#include <h/graphics.h>

static status	computeFormatDevice(Device dev);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class device is an abstract superclass used to define method common
to  Pictures and  Figures:  manipulating  a chain   of  graphicals and
dispatching events.

A device is a subclass of graphical and thus can be displayed on other
devices.

Devices  maintain the  graphical's  attribute  <->area  to reflect the
bounding box  of   all displayed  graphicals  (e.g.   graphicals  with
<->displayed equals @on).  To the X-Y  coordinate of this bounding box
the  <->offset is  added  to  ensure  smooth intergration  with  class
figure.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		/********************************
		*        CREATE/DESTROY		*
		********************************/

status
initialiseDevice(Device dev)
{ initialiseGraphical(dev, ZERO, ZERO, ZERO, ZERO);

  assign(dev, level, ZERO);
  assign(dev, offset, newObject(ClassPoint, EAV));
  assign(dev, graphicals, newObject(ClassChain, EAV));
  assign(dev, badBoundingBox, OFF);
  assign(dev, badFormat, OFF);
  assign(dev, format, NIL);
  assign(dev, pointed, newObject(ClassChain, EAV));
  assign(dev, clip_area, NIL);
  assign(dev, recompute, newObject(ClassChain, EAV));

  succeed;
}


status
unlinkDevice(Device dev)
{ if ( notNil(dev->graphicals) )
  { Graphical gr;

    for_chain(dev->graphicals, gr, DeviceGraphical(gr, NIL));
  }

  return unlinkGraphical((Graphical) dev);
}

		/********************************
		*             CURSOR		*
		********************************/

CursorObj
getDisplayedCursorDevice(Device dev)
{ CursorObj c2;
  Cell cell;

  for_cell(cell, dev->pointed)
  { if ( (c2=qadGetv(cell->value, NAME_displayedCursor, 0, NULL)) &&
	 notNil(c2) )
      answer(c2);
  }

  answer(dev->cursor);			/* = getDisplayedCursorGraphical()! */
}


		/********************************
		*         EVENT HANDLING	*
		********************************/

static Chain
get_pointed_objects_device(Device dev, Int x, Int y, Chain ch)
{ Cell cell;

  if ( isDefault(ch) )
    ch = answerObject(ClassChain, EAV);
  else
    clearChain(ch);

  for_cell(cell, dev->graphicals)
  { register Graphical gr = cell->value;

    if ( gr->displayed == ON &&
	 inEventAreaGraphical(gr, x, y) )
      prependChain(ch, gr);
  }

  if ( notDefault(ch) )
    answer(ch);

  fail;
}


Chain
getPointedObjectsDevice(Device dev, Any pos, Chain ch)
{ Int x, y;

  if ( instanceOfObject(pos, ClassPoint) )
  { Point pt = pos;

    x = pt->x;
    y = pt->y;
  } else /*if ( instanceOfObject(pos, ClassEvent) )*/
    get_xy_event(pos, dev, OFF, &x, &y);

  return get_pointed_objects_device(dev, x, y, ch);
}

#define MAX_ACTIVE 250			/* Objects under the mouse */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
updatePointedDevice() updates the <->pointed chain of the device.
The <->pointed chain is a  chain  holding all events that overlap with
the mouse  position and  are  editable  and  displayed.   It sends  an
area_enter event to all graphicals  that have  been added to the chain
and an area_exit event to all graphicals that have been deleted to the
chain.  Care is taken to prevent this function from creating  too many
intermediate objects as is it called very frequently.

The event area_cancel is ok, but area_resume should verify the buttons
are in the same  state as when  the area is  left and at  least one is
down.   This requires us  to store the status  of the button with  the
graphical object, costing us an additional 4 bytes on  each graphical.
To do or not to do?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
updatePointedDevice(Device dev, EventObj ev)
{ Cell cell, c2;
  Graphical active[MAX_ACTIVE];
  int n, an = 0;
  Int x, y;
  Name enter, exit;

  if ( allButtonsUpEvent(ev) )
  { enter = NAME_areaEnter;
    exit  = NAME_areaExit;
  } else
  { enter = NAME_areaResume;
    exit  = NAME_areaCancel;
  }

					/* Exit event: leave all children */
  if ( isAEvent(ev, NAME_areaExit) )
  { for_cell(cell, dev->pointed)
      generateEventGraphical(cell->value, exit);

    clearChain(dev->pointed);
    succeed;
  }

  get_xy_event(ev, dev, OFF, &x, &y);

					/* See which graphicals are left */
  for_cell_save(cell, c2, dev->pointed)
  { register Graphical gr = cell->value;

    if ( gr->displayed == OFF || !inEventAreaGraphical(gr, x, y) )
    { DEBUG(NAME_event, Cprintf("Leaving %s\n", pp(gr)));
      deleteChain(dev->pointed, gr);
      generateEventGraphical(gr, exit);
    }
  }

					/* See which graphicals are entered */
  for_cell(cell, dev->graphicals)
  { register Graphical gr = cell->value;

    if ( gr->displayed == ON && inEventAreaGraphical(gr, x, y) )
    { active[an++] = gr;

      if ( memberChain(dev->pointed, gr) != SUCCEED )
      { DEBUG(NAME_event, Cprintf("Entering %s\n", pp(gr)));
        generateEventGraphical(gr, enter);
      }

      if ( an == MAX_ACTIVE )		/* Shift to keep top ones */
      { int n;
        for( n = 0; n < MAX_ACTIVE-1; n++ )
	  active[n] = active[n+1];
	an--;
      }
    }
  }

					/* Update the ->pointed chain */
  for( cell = dev->pointed->head, n = an-1; n >= 0; n--, cell = cell->next )
  { if ( isNil(cell) )			/* Chain is out; extend it */
    { for( ; n >=0; n-- )
        appendChain(dev->pointed, active[n]);
      break;
    }

    cellValueChain(dev->pointed, PointerToInt(cell), active[n]);
  }

  while( notNil(cell) )			/* Remove the tail of the chain */
  { c2 = cell->next;
    deleteChain(dev->pointed, cell->value);
    cell = c2;
  }

  succeed;
}


status
inspectDevice(Device dev, EventObj ev)
{ Cell cell;
  DisplayObj d = CurrentDisplay(dev);

  updatePointedDevice(dev, ev);

  for_cell(cell, dev->pointed)
  { if ( instanceOfObject(cell->value, ClassDevice) )
    { if ( inspectDevice(cell->value, ev) )
    	succeed;
    } else
    { if ( inspectDisplay(d, cell->value, ev) )
	succeed;
    }
  }

  return inspectDisplay(d, (Graphical) dev, ev);
}


static Graphical
get_find_device(Device dev, Int x, Int y, Code cond)
{ LocalArray(Graphical, grv, valInt(dev->graphicals->size));
  int i, grn;
  Cell cell;

  grn=0;
  for_cell(cell, dev->graphicals)
    grv[grn++] = cell->value;

  for(i=grn-1; i >= 0; i--)
  { Graphical gr = grv[i];

    if ( notDefault(x) && !inEventAreaGraphical(gr, x, y) )
      continue;

    if ( instanceOfObject(gr, ClassDevice) )
    { Device dev2 = (Device) gr;
      Any rval;

      if ( (rval=get_find_device(dev2,
				 isDefault(x) ? x : sub(x, dev2->offset->x),
				 isDefault(y) ? y : sub(y, dev2->offset->y),
				 cond)) )
	answer(rval);
    } else
    { if ( isDefault(cond) ||
	   forwardCodev(cond, 1, (Any *)&gr) )
	answer(gr);
    }
  }

  if ( isDefault(cond) ||
       forwardCodev(cond, 1, (Any *)&dev) )
    answer((Graphical) dev);

  fail;
}


static Graphical
getFindDevice(Device dev, Any location, Code cond)
{ Int x, y;

  if ( instanceOfObject(location, ClassEvent) )
    get_xy_event(location, dev, OFF, &x, &y);
  else if ( isDefault(location) )
  { x = y = (Int) DEFAULT;
  } else
  { Point p = location;

    x = p->x;
    y = p->y;
  }

  return get_find_device(dev, x, y, cond);
}


status
eventDevice(Any obj, EventObj ev)
{ Device dev = obj;

  if ( dev->active != OFF )
  { Graphical gr;
    int done = FALSE;

    updatePointedDevice(dev, ev);

    for_chain(dev->pointed, gr,
	      if ( !done && postEvent(ev, gr, DEFAULT) )
	        done = TRUE);
    if ( done )
      succeed;

    return eventGraphical(dev, ev);
  }

  fail;
}


static status
typedDevice(Device dev, EventId id, BoolObj delegate)
{ Any key = characterName(id);
  Graphical gr;

  for_chain(dev->graphicals, gr,
	    if ( sendv(gr, NAME_key, 1, &key) )
	      succeed);

  if ( delegate == ON && notNil(dev->device) )
    return send(dev->device, NAME_typed, id, delegate, EAV);

  fail;
}


Button
getDefaultButtonDevice(Device d)
{ Cell cell;

  for_cell(cell, d->graphicals)
  { Button b = cell->value;

    if ( instanceOfObject(b, ClassButton) &&
	 b->default_button == ON )
      answer(b);
  }

  fail;
}


static Int
getWantsKeyboardFocusGraphical(Graphical gr)
{ if ( qadSendv(gr, NAME_WantsKeyboardFocus, 0, NULL) )
  { if ( instanceOfObject(gr, ClassTextItem) )
      return toInt(10);
    if ( instanceOfObject(gr, ClassButton) &&
	 ((Button)gr)->default_button == ON )
      return toInt(5);

    return toInt(1);
  }

  fail;
}


status
advanceDevice(Device dev, Graphical gr, BoolObj propagate, Name direction)
{ Cell cell;
  int skip = TRUE;			/* before gr */
  Graphical first = NIL;
  Graphical last = NIL;
  PceWindow sw;

  TRY( sw = getWindowGraphical((Graphical) dev) );

  if ( isDefault(gr) )
    gr = NIL;

					/* Find initial focus */
  if ( isNil(gr) )
  { Graphical focus = NIL;
    int best = -1;

    for_cell(cell, dev->graphicals)
    { Int v;

      if ( (v = getWantsKeyboardFocusGraphical(cell->value)) &&
	   valInt(v) > best )
      { best = valInt(v);
	focus = cell->value;
      }
    }

    if ( best != -1 )
      return keyboardFocusWindow(sw, focus);
  } else
  { if ( isDefault(direction) )
      direction = NAME_forwards;

    for_cell(cell, dev->graphicals)
    { if ( skip )
      { if ( isNil(first) &&
	     qadSendv(cell->value, NAME_WantsKeyboardFocus, 0, NULL) )
	  first = cell->value;

	if ( direction == NAME_backwards )
	{ if ( cell->value == gr )
	  { if ( notNil(last) )
	      return keyboardFocusWindow(sw, last);
	  } else
	  { if ( qadSendv(cell->value, NAME_WantsKeyboardFocus, 0, NULL) )
	      last = cell->value;
	  }
	}

	if ( cell->value == gr )
	  skip = FALSE;

	continue;
      }

      if ( send(cell->value, NAME_WantsKeyboardFocus, EAV) )
      { if ( direction == NAME_forwards )
	  return keyboardFocusWindow(sw, cell->value);
	else
	  last = cell->value;
      }
    }

    if ( last && direction == NAME_backwards )
      return keyboardFocusWindow(sw, last);
  }

  if ( isDefault(propagate) )
    propagate = ((Device) sw != dev ? ON : OFF);

  if ( propagate == ON && notNil(dev->device) )
    send(dev->device, NAME_advance, dev, EAV);
  else
    keyboardFocusWindow(sw, first);	/* may be NIL */

  succeed;
}


		/********************************
		*       REPAINT MANAGEMENT	*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The device's repaint manager is responsible for  keeping  track of the
devices area (the bounding box of its displayed graphicals) and of the
area that  needs repainting if  a ->RedrawArea is received.  It should
issue to appropriate ->RedrawArea request on its associated graphicals
if it receives an ->RedrawArea from its parent.

A number of changes are recognised:

   *) A graphical is added to the device or its displayed attribute has
      changed to @on.
   *) A graphical is erased from the device or its displayed attribute
      has changed to @off.
   *) The image of a graphical has changed.
   *) The area of a graphical has changed.

Graphicals indicate changes through the following call:

      CHANGING_GRAPHICAL(gr,
	  <code>
	  [changedImageGraphical(gr, x, y, w, h)]
	  [changedEntireImageGraphical(gr)]
      )
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
requestComputeDevice(Device dev, Any val)
{ DEBUG(NAME_compute, Cprintf("requestComputeDevice(%s)\n", pp(dev)));
  assign(dev, badBoundingBox, ON);
  assign(dev, badFormat, ON);

  return requestComputeGraphical(dev, val);
}


status
computeGraphicalsDevice(Device dev)
{ Chain ch = dev->recompute;

  while( !emptyChain(ch) )		/* tricky! */
  { Cell cell;
    int i, size = valInt(ch->size);
    ArgVector(array, size);

    for(i=0, cell = ch->head; notNil(cell); cell = cell->next)
      array[i++] = cell->value;

    clearChain(ch);
    for(i=0; i<size; i++)
    { Graphical gr = array[i];

      if ( !isFreedObj(gr) && notNil(gr->request_compute) )
      { qadSendv(gr, NAME_compute, 0, NULL);
	assign(gr, request_compute, NIL);
      }
    }
  }

  succeed;
}


status
computeLayoutDevice(Device dev)
{ if ( notNil(dev->format) )
    computeFormatDevice(dev);
  else if ( notNil(dev->layout_manager) &&
	    notNil(dev->layout_manager->request_compute) )
    qadSendv(dev->layout_manager, NAME_compute, 0, NULL);

  succeed;
}


status
computeDevice(Any obj)
{ Device dev = obj;

  if ( notNil(dev->request_compute) )
  { computeGraphicalsDevice(dev);
    computeLayoutDevice(dev);
    computeBoundingBoxDevice(dev);

    assign(dev, request_compute, NIL);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Updates the bounding box and succeeds if if changed.  Fails if there are
no modifications.  The old bounding box is returned in `od', a vector of
4 integers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
updateBoundingBoxDevice(Device dev, Int *od)
{ Cell cell;
  Area a = dev->area;

  od[0] = a->x; od[1] = a->y; od[2] = a->w; od[3] = a->h;

  if ( isNil(dev->layout_manager) ||
       !qadSendv(dev->layout_manager, NAME_computeBoundingBox, 0, NULL) )
  { clearArea(a);

    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON )
	unionNormalisedArea(a, gr->area);
    }
  }
  relativeMoveArea(a, dev->offset);

  if ( od[0] != a->x || od[1] != a->y || od[3] != a->w || od[4] != a->h )
    succeed;

  fail;
}


status
computeBoundingBoxDevice(Device dev)
{ if ( dev->badBoundingBox == ON )
  { Int od[4];				/* ax, ay, aw, ah */

    if ( updateBoundingBoxDevice(dev, od) )
    { if ( notNil(dev->device) )
      { requestComputeDevice(dev->device, DEFAULT);
      	updateConnectionsGraphical((Graphical) dev, sub(dev->level, ONE));
      }

      qadSendv(dev, NAME_changedUnion, 4, od);
    }

    if ( notNil(dev->clip_area) )
    { Area a = dev->area;

      relativeMoveBackArea(a, dev->offset);
      if ( intersectionArea(dev->area, dev->clip_area) == FAIL )
      { assign(dev->area, w, ZERO);
        assign(dev->area, h, ZERO);
      }
      relativeMoveArea(a, dev->offset);
    }

    assign(dev, badBoundingBox, OFF);
  }

  succeed;
}


static status
changedUnionDevice(Device dev, Int ox, Int oy, Int ow, Int oh)
{ succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Our  parent requests  us to  repaint an area.    This  area is in  the
coordinate system of  the device we  are  displayed on.  The requested
repaint area may be larger than the area of myself.

This algorithm can be made more clever on  a number of  points.  First
of all we could be  more  clever  with none-square graphicals, notably
lines.  Next,  we could determine that  objects  are obscured by other
objects and thus  do not need to be  repainted.  We  will  leave these
optimisations for later.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
EnterRedrawAreaDevice(Device dev, Area a, DeviceDrawContext ctx)
{ if ( a->w != ZERO && a->h != ZERO )
  { int ox = valInt(dev->offset->x);
    int oy = valInt(dev->offset->y);

    ctx->x = a->x;
    ctx->y = a->y;
    ctx->w = a->w;
    ctx->h = a->h;

    qassign(a, x, toInt(valInt(a->x) - ox));
    qassign(a, y, toInt(valInt(a->y) - oy));
    r_offset(ox, oy);

    if ( notNil(dev->clip_area) )
    { if ( !intersectionArea(a, dev->clip_area) )
      { qassign(a, x, ctx->x);
	qassign(a, y, ctx->y);
	qassign(a, w, ctx->w);
	qassign(a, h, ctx->h);

	fail;
      }

      clipGraphical((Graphical)dev, a);
    }

    succeed;
  }

  fail;
}


void
ExitRedrawAreaDevice(Device dev, Area a, DeviceDrawContext ctx)
{ int ox = valInt(dev->offset->x);
  int oy = valInt(dev->offset->y);

  if ( notNil(dev->clip_area) )
    unclipGraphical((Graphical)dev);

  r_offset(-ox, -oy);

  qassign(a, x, ctx->x);
  qassign(a, y, ctx->y);
  qassign(a, w, ctx->w);
  qassign(a, h, ctx->h);
}



status
RedrawAreaDevice(Device dev, Area a)
{ device_draw_context ctx;

  if ( EnterRedrawAreaDevice(dev, a, &ctx) )
  { Cell cell;

    if ( notNil(dev->layout_manager) )
      qadSendv(dev->layout_manager, NAME_redrawBackground, 1, (Any*)&a);

    for_cell(cell, dev->graphicals)
      RedrawArea(cell->value, a);

    if ( notNil(dev->layout_manager) )
      qadSendv(dev->layout_manager, NAME_redrawForeground, 1, (Any*)&a);

    ExitRedrawAreaDevice(dev, a, &ctx);
  }

  return RedrawAreaGraphical(dev, a);
}


status
flashDevice(Device dev, Area a, Int time)
{ if ( isDefault(a) ||
       (dev->offset->x == dev->area->x &&
	dev->offset->y == dev->area->y) )
    return flashGraphical((Graphical)dev, a, time);
  else
  { Area a2;
    int nx = valInt(a->x) + valInt(dev->offset->x) - valInt(dev->area->x);
    int ny = valInt(a->y) + valInt(dev->offset->y) - valInt(dev->area->y);

    a2 = answerObject(ClassArea, toInt(nx), toInt(ny), a->w, a->h, EAV);
    flashGraphical((Graphical)dev, a2, time);
    doneObject(a2);
  }

  succeed;
}


		/********************************
		*         DISPLAY/ERASE		*
		********************************/


status
clearDevice(Device dev, Name how)
{ Chain ch = dev->graphicals;

  if ( how == NAME_destroy )
  { while( !emptyChain(ch) )
      qadSendv(getHeadChain(ch), NAME_destroy, 0, NULL);
  } else if ( how == NAME_free )
  { while( !emptyChain(ch) )
      qadSendv(getHeadChain(ch), NAME_free, 0, NULL);
  } else /* if ( how == NAME_erase ) */
  { while( !emptyChain(ch) )
      eraseDevice(dev, getHeadChain(ch));
  }

  succeed;
}


status
displayDevice(Any Dev, Any Gr, Point pos)
{ Device dev = Dev;
  Graphical gr = Gr;

  if ( notDefault(pos) )
  { Variable var;

    if ( (var = getInstanceVariableClass(classOfObject(gr), NAME_autoAlign)) )
      sendVariable(var, gr, OFF);

    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);
  }

  DeviceGraphical(gr, dev);
  DisplayedGraphical(gr, ON);

  succeed;
}


status
appendDevice(Device dev, Graphical gr)
{ appendChain(dev->graphicals, gr);
  assign(gr, device, dev);
  if ( notNil(gr->request_compute) )
  { appendChain(dev->recompute, gr);
    if ( isNil(dev->request_compute) )
      requestComputeDevice(dev, DEFAULT);
  }

  if ( gr->displayed == ON )
    displayedGraphicalDevice(dev, gr, ON);

  qadSendv(gr, NAME_reparent, 0, NULL);

  succeed;
}

/* True if sub is gr or a graphical inside the (device) gr
*/

status
subGraphical(Graphical gr, Graphical sub)
{ while( notNil(sub) )
  { if ( sub == gr )
      succeed;
    sub = (Graphical)sub->device;
  }

  fail;
}


status
eraseDevice(Device dev, Graphical gr)
{ if ( gr->device == dev )
  { PceWindow sw = getWindowGraphical((Graphical) dev);

    if ( sw )
    { if ( subGraphical(gr, sw->keyboard_focus) )
	keyboardFocusWindow(sw, NIL);
      if ( subGraphical(gr, sw->focus) )
	focusWindow(sw, NIL, NIL, NIL, NIL);
    }

    if ( gr->displayed == ON )
      displayedGraphicalDevice(dev, gr, OFF);

    deleteChain(dev->recompute, gr);
    deleteChain(dev->pointed, gr);
    assign(gr, device, NIL);
    GcProtect(dev, deleteChain(dev->graphicals, gr));
    if ( !isFreedObj(gr) )
      qadSendv(gr, NAME_reparent, 0, NULL);
  }

  succeed;
}


status
displayedGraphicalDevice(Device dev, Graphical gr, BoolObj val)
{ BoolObj old = gr->displayed;

  if ( onFlag(gr, F_SOLID) )
  { clearFlag(gr, F_SOLID);
    changedEntireImageGraphical(gr);
    setFlag(gr, F_SOLID);
  } else
    changedEntireImageGraphical(gr);

  gr->displayed = val;
  if ( instanceOfObject(gr, ClassDevice) )
    updateConnectionsDevice((Device) gr, dev->level);
  else
    updateConnectionsGraphical(gr, dev->level);

  requestComputeDevice(dev, DEFAULT); /* TBD: ON: just union */
  gr->displayed = old;

  succeed;
}

		/********************************
		*           EXPOSURE		*
		********************************/

status
exposeDevice(Device dev, Graphical gr, Graphical gr2)
{ if ( gr->device != dev || (notDefault(gr2) && gr2->device != dev) )
    fail;

  if ( isDefault(gr2) )
  { addCodeReference(gr);
    deleteChain(dev->graphicals, gr);
    appendChain(dev->graphicals, gr);
    delCodeReference(gr);
  } else
  { moveAfterChain(dev->graphicals, gr, gr2);
    changedEntireImageGraphical(gr2);
  }
  requestComputeDevice(dev, DEFAULT);	/* Actually only needs format */

  changedEntireImageGraphical(gr);

  succeed;
}


status
hideDevice(Device dev, Graphical gr, Graphical gr2)
{ if ( gr->device != dev || (notDefault(gr2) && gr2->device != dev) )
    fail;

  if ( isDefault(gr2) )
  { addCodeReference(gr);
    deleteChain(dev->graphicals, gr);
    prependChain(dev->graphicals, gr);
    delCodeReference(gr);
  } else
  { moveBeforeChain(dev->graphicals, gr, gr2);
    changedEntireImageGraphical(gr2);
  }
  requestComputeDevice(dev, DEFAULT);	/* Actually only needs format */

  changedEntireImageGraphical(gr);

  succeed;
}


status
swapGraphicalsDevice(Device dev, Graphical gr, Graphical gr2)
{ if ( gr->device != dev || (notDefault(gr2) && gr2->device != dev) )
    fail;

  swapChain(dev->graphicals, gr, gr2);

  changedEntireImageGraphical(gr);
  changedEntireImageGraphical(gr2);
  requestComputeDevice(dev, DEFAULT);		/* Actually only needs format */

  succeed;
}


		/********************************
		*          SELECTION		*
		********************************/


static status
selectionDevice(Device dev, Any obj)
{ Cell cell;

  if ( instanceOfObject(obj, ClassChain) )
  { int size = valInt(getSizeChain(obj));
    ArgVector(selection, size);
    int i = 0;

    for_cell(cell, (Chain)obj)
      selection[i++] = checkType(cell->value, TypeGraphical, dev);

    for_cell(cell, dev->graphicals)
    { for(i=0; i<size; i++)
      { if ( selection[i] == cell->value )
	  break;
      }
      if ( i < size )
	send(cell->value, NAME_selected, ON, EAV);
      else
	send(cell->value, NAME_selected, OFF, EAV);
    }

    succeed;
  }

  for_cell(cell, dev->graphicals)
    send(cell->value, NAME_selected, cell->value == obj ? ON : OFF, EAV);

  succeed;
}


static Chain
getSelectionDevice(Device dev)
{ Chain ch = answerObject(ClassChain, EAV);
  Cell cell;

  for_cell(cell, dev->graphicals)
  { if ( ((Graphical)cell->value)->selected == ON )
      appendChain(ch, cell->value);
  }

  answer(ch);
}


		 /*******************************
		 *	      LAYOUT		*
		 *******************************/

static status
layoutManagerDevice(Device dev, LayoutManager mgr)
{ if ( dev->layout_manager != mgr )
  { if ( notNil(dev->layout_manager) )
      qadSendv(dev->layout_manager, NAME_detach, 0, NULL);
    assign(dev, layout_manager, mgr);
    if ( notNil(mgr) )
      qadSendv(mgr, NAME_attach, 1, (Any *)&dev);
  }

  succeed;
}


		/********************************
		*           FORMATTING          *
		*********************************/

static status
formatDevice(Device dev, Any obj, Any arg)
{ status rval = SUCCEED;

  if ( isNil(obj) || instanceOfObject(obj, ClassFormat) )
  { assign(dev, format, obj);
  } else
  { if ( isNil(dev->format) )
      assign(dev, format, newObject(ClassFormat, EAV));

    rval = send(dev->format, (Name)obj, arg, EAV);
  }
  requestComputeDevice(dev, DEFAULT);

  return rval;
}

static void
move_graphical(Graphical gr, int x, int y)
{ Int X = toInt(x);
  Int Y = toInt(y);

  if ( X != gr->area->x || Y != gr->area->y )
    doSetGraphical(gr, X, Y, DEFAULT, DEFAULT);
}


static status
computeFormatDevice(Device dev)
{ Format l;

  if ( dev->badFormat == OFF || isNil(l=dev->format) )
    succeed;

#define HV(h, v) (l->direction == NAME_horizontal ? (h) : (v))
#define MUSTBEVISIBLE(dev, gr) { if (gr->displayed == OFF) continue; }

  if ( l->columns == ON )
  { int *cw;				/* column widths */
    int *rh;				/* row heights */
    char *cf;				/* column format */
    int cs = valInt(l->column_sep);	/* column separator size */
    int rs = valInt(l->row_sep);	/* row separator size */
    Cell cell;
    int c, r = 0;
    int cols = valInt(l->width);
    int rows = (valInt(getSizeChain(dev->graphicals)) + cols - 1)/cols;
    int x = 0;
    int y = 0;

    if ( !(cw = alloca(sizeof(int) * cols)) ||
	 !(cf = alloca(sizeof(char) * cols)) ||
	 !(rh = alloca(sizeof(int) * (rows+1))) )
      return errorPce(dev, NAME_notEnoughMemory);

    for(c=0; c < cols; c++)
    { cw[c] = 0;
      cf[c] = 'l';
    }

    if ( notNil(l->adjustment) )
    { for(c=0; c < cols; c++)
      { Name format = (Name) getElementVector(l->adjustment, toInt(c+1));

	if ( equalName(format, NAME_center) )
	  cf[c] = 'c';
	else if ( equalName(format, NAME_right) )
	  cf[c] = 'r';
	else
	  cf[c] = 'l';
      }
    }

    rh[r] = c = 0;
    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;
      int gw, gh;

      MUSTBEVISIBLE(dev, gr);
      gw = valInt(HV(gr->area->w, gr->area->h));
      gh = valInt(HV(gr->area->h, gr->area->w));

      cw[c] = max(cw[c], gw);
      rh[r] = max(rh[r], gh);

      if ( ++c >= cols )
      { c = 0;
        rh[++r] = 0;
      }
    }

    c = r = 0;

    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;
      MUSTBEVISIBLE(dev, gr);

      if ( l->direction == NAME_horizontal )
      { switch( cf[c] )
        { case 'l':	move_graphical(gr, x, y);
			break;
          case 'r':	move_graphical(gr, x+cw[c]-valInt(gr->area->w), y);
			break;
	  case 'c':	move_graphical(gr, x+(cw[c]-valInt(gr->area->w))/2, y);
	  		break;
	}
      } else
      { switch( cf[c] )
        { case 'l':	move_graphical(gr, y, x);
			break;
          case 'r':	move_graphical(gr, y, x+cw[c]-valInt(gr->area->h));
			break;
	  case 'c':	move_graphical(gr, y, x+(cw[c]-valInt(gr->area->h))/2);
	  		break;
	}
      }

      if ( c+1 >= cols )
      { y += rh[r++] + rs;
        c = 0;
	x = 0;
      } else
      { x += cw[c++] + cs;
      }
    }
  } else				/* non-column device */
  { int x = 0;
    int y = 0;
    int w = valInt(l->width);
    int cs = valInt(l->column_sep);
    int rs = valInt(l->row_sep);
    int rh = 0;
    int first = TRUE;
    Cell cell;

    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;
      int gw, gh;

      MUSTBEVISIBLE(dev, gr);
      gw = valInt(HV(gr->area->w, gr->area->h));
      gh = valInt(HV(gr->area->h, gr->area->w));

      if ( !first && x + gw > w )	/* start next column */
      { y += rh + rs;
        rh = 0;
        x = 0;
        first = TRUE;
      }
      move_graphical(gr, HV(x, y), HV(y, x));
      x += gw + cs;
      rh = max(rh, gh);
      first = FALSE;
    }
  }
#undef HV

  assign(dev, badFormat, OFF);

  succeed;
}


		/********************************
		*	  DIALOG LAYOUT		*
		********************************/

static HashTable PlacedTable = NULL;	/* placed objects */

static int max_rows    = 20;		/* will be expanded as needed */
static int max_columns = 10;		/* same */

					/* flags values */
#define DLF_STRETCH_TO_BB	0x1	/* Stretch-right to BB */

typedef struct _unit			/* can't use cell! */
{ Graphical item;			/* Item displayed here */
  short x;				/* X-position (of column) */
  short height;				/* Height above reference */
  short	depth;				/* Depth below reference */
  short right;				/* Right of reference point */
  short left;				/* Left of reference point */
  short	hstretch;			/* Strechable horizontal */
  short vstretch;			/* Strechable vertical */
  Name  alignment;			/* alignment of the item */
  int	flags;				/* Misc alignment flags */
} unit, *Unit;

static unit empty_unit = { (Graphical) NIL,
			   0, 0, 0, 0, 0, 0, 0,
			   NAME_column, 0
			 };

typedef struct _matrix
{ int cols;				/* actual size */
  int rows;
  unit **units;
} matrix, *Matrix;


#define IsPlaced(gr)  (getMemberHashTable(PlacedTable, gr) == ON)
#define SetPlaced(gr) (appendHashTable(PlacedTable, gr, ON))

static status
shift_x_matrix(Matrix m, int *max_x, int *max_y)
{ int x, y;

  if ( *max_x + 1 > max_columns )
    fail;

  m->units[*max_x] = alloc(sizeof(unit) * max_rows);
  for(y=0; y < *max_y; y++)
  { for(x = *max_x; x > 0; x--)
      m->units[x][y] = m->units[x-1][y];

    m->units[0][y] = empty_unit;
  }

  (*max_x)++;
  succeed;
}


static status
shift_y_matrix(Matrix m, int *max_x, int *max_y)
{ int x, y;

  if ( *max_y + 1 > max_rows )
    fail;

  for(x=0; x < *max_x; x++)
  { for(y = *max_y; y > 0; y--)
      m->units[x][y] = m->units[x][y-1];

    m->units[x][0] = empty_unit;
  }

  (*max_y)++;
  succeed;
}


static status
expand_x_matrix(Matrix m, int *max_x, int *max_y)
{ int y;

  if ( *max_x + 1 > max_columns )
    fail;

  m->units[*max_x] = alloc(sizeof(unit) * max_rows);
  for(y=0; y < *max_y; y++)
    m->units[*max_x][y] = empty_unit;

  (*max_x)++;
  succeed;
}


static status
expand_y_matrix(Matrix m, int *max_x, int *max_y)
{ int x;

  if ( *max_y + 1 > max_rows )
    fail;

  for(x=0; x < *max_x; x++)
    m->units[x][*max_y] = empty_unit;

  (*max_y)++;
  succeed;
}


static void
free_matrix_columns(Matrix m, int max_x)
{ int x;

  for(x=0; x<max_x; x++)
    unalloc(sizeof(unit) * max_rows, m->units[x]);
}


static status
placeDialogItem(Device d, Matrix m, Graphical i,
		int x, int y, int *max_x, int *max_y)
{ Graphical gr;

  if ( IsPlaced(i) ||
       get(i, NAME_autoAlign, EAV) != ON  )
    succeed;

  if ( isNil(i->device) )
    displayDevice(d, i, DEFAULT);

/*
  if ( i->displayed == OFF )
    succeed;
*/

  SetPlaced(i);

  DEBUG(NAME_layout, Cprintf("placing %s\n", pp(i)));

  while( x < 0 ) { TRY(shift_x_matrix(m, max_x, max_y)); x++; }
  while( y < 0 ) { TRY(shift_y_matrix(m, max_x, max_y)); y++; }
  while( x >= *max_x ) TRY(expand_x_matrix(m, max_x, max_y));
  while( y >= *max_y ) TRY(expand_y_matrix(m, max_x, max_y));

  m->units[x][y].item = i;
  m->units[x][y].alignment = get(i, NAME_alignment, EAV);
  if ( !m->units[x][y].alignment )
    m->units[x][y].alignment = NAME_left;

  if ( instanceOfObject((gr = get(i, NAME_below, EAV)), ClassGraphical) )
    TRY(placeDialogItem(d, m, gr, x, y-1, max_x, max_y));
  if ( instanceOfObject((gr = get(i, NAME_above, EAV)), ClassGraphical) )
    TRY(placeDialogItem(d, m, gr, x, y+1, max_x, max_y));
  if ( instanceOfObject((gr = get(i, NAME_left, EAV)), ClassGraphical) )
    TRY(placeDialogItem(d, m, gr, x+1, y, max_x, max_y));
  if ( instanceOfObject((gr = get(i, NAME_right, EAV)), ClassGraphical)  )
    TRY(placeDialogItem(d, m, gr,  x-1, y, max_x, max_y));

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Adjust  to  the  bounding  box  by   adjusting  all  columns  containing
stretchable items.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
stretchColumns(Matrix m, Size gap, Size bb, Size border)
{ if ( notDefault(bb) )
  { int twidth = valInt(bb->w) - 2 * valInt(border->w); /* total width */
    Stretch s = alloca(sizeof(stretch) * m->cols);
    Stretch sp;
    int x, y;

    twidth -= (m->cols-1) * valInt(gap->w);

    for(sp=s, x=0; x<m->cols; x++, sp++)
    { int stretch = 0, noshrink=FALSE;

      sp->ideal   = m->units[x][0].left + m->units[x][0].right;
      sp->minimum = 0;
      sp->maximum = INT_MAX;

      for(y=0; y<m->rows; y++)
      { if ( m->units[x][y].alignment == NAME_column )
	{ stretch = max(stretch, m->units[x][y].hstretch);
	  if ( m->units[x][y].hstretch == 0 )
	    noshrink = TRUE;
	}
      }

      sp->stretch = stretch;
      if ( stretch > 0 && !noshrink )
	sp->shrink = stretch;
      else
	sp->shrink = 0;
    }

    distribute_stretches(s, m->cols, twidth);

    for(sp=s, x=0; x<m->cols; x++, sp++)
    { for(y=0; y<m->rows; y++)
      { if ( m->units[x][y].alignment == NAME_column )
	  m->units[x][0].right = sp->size - m->units[x][0].left;
      }
    }
  }
}


static void
determineXColumns(Matrix m, Size gap, Size bb, Size border)
{ int x, y;
  int cx = valInt(border->w);

  for(x=0; x<m->cols; x++)
  { int maxr = 0;

    for(y=0; y<m->rows; y++)
    { int r;

      if ( x == 0 || m->units[x][y].alignment == NAME_column )
	m->units[x][y].x = cx;
      else
	m->units[x][y].x = m->units[x-1][y].x +
			   m->units[x-1][y].left +
			   m->units[x-1][y].right +
			   valInt(gap->w);
      r = m->units[x][y].x + m->units[x][y].left + m->units[x][y].right;
      maxr = max(maxr, r);
    }

    cx = maxr + valInt(gap->w);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stretchRows()  stretches  the  rows   to    deal   with  objects  having
<-ver_stretch defined. bbh is the total height   that should be taken by
the objects. itemssh is the amount currently used.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
stretchRows(Matrix m, int bbh)
{ int x, y;
  Stretch s = alloca(sizeof(stretch) * m->rows);
  Stretch sp;

  for(sp = s, y=0; y<m->rows; y++)
  { int stretch = 0, noshrink=FALSE;

    if ( m->units[0][y].height == 0 && m->units[0][y].depth == 0 )
      continue;

    sp->ideal  = m->units[0][y].height + m->units[0][y].depth;
    sp->minimum = 0;
    sp->maximum = INT_MAX;

    for(x=0; x<m->cols; x++)
    { stretch = max(stretch, m->units[x][y].vstretch);
      if ( m->units[x][y].vstretch == 0 &&
	   notNil(m->units[x][y].item) )
	noshrink = TRUE;
    }

    sp->stretch = stretch;
    if ( stretch > 0 && !noshrink )
      sp->shrink = stretch;
    else
      sp->shrink = 0;

    if ( stretch == 0 && y < m->rows - 1 )
      sp->stretch = 1;

    sp++;
  }

  distribute_stretches(s, sp-s, bbh);

  for(sp=s, y=0; y<m->rows; y++)
  { if ( m->units[0][y].height == 0 && m->units[0][y].depth == 0 )
      continue;

    for(x=0; x<m->cols; x++)
    { if ( !(sp->shrink == 0 &&
	     sp->size < m->units[x][y].depth + m->units[x][y].height) )
	m->units[x][y].depth = sp->size - m->units[x][y].height;
    }

    sp++;
  }
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
adjustDialogItem() is as doSetGraphical, but returns 0 if there was no
change and 1 if there was a change.

We need a special hack here  to   deal  with  windows. Actually, we need
something to tell an object to have a certain geometry in pixels and not
negotiate, but deal properly with containers/decorations, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
adjustDialogItem(Any obj, Int x, Int y, Int w, Int h)
{ Graphical gr = obj;

  DEBUG(NAME_layout,
	Cprintf("%s --> %s %s %s %s\n",
		pp(obj), pp(x), pp(y), pp(w), pp(h)));

  if ( instanceOfObject(gr, ClassWindow) && 		/* HACK */
       notNil(((PceWindow)gr)->decoration) )
    gr = (Graphical)((PceWindow)gr)->decoration;

#define Changed(a) (gr->area->a != a && notDefault(a))
  if ( Changed(x) || Changed(y) || Changed(w) || Changed(h) )
  { Int av[4];

    av[0] = x; av[1] = y; av[2] = w; av[3] = h;

    qadSendv(gr, NAME_geometry, 4, av);
    return 1;
  }
#undef Changed

  return 0;
}


static int
grow_max_matrix(int max_x, int max_y)
{ if ( max_x >= max_columns )
    max_columns *= 2;
  if ( max_y >= max_rows )
    max_rows *= 2;

  succeed;
}


status
layoutDialogDevice(Device d, Size gap, Size bb, Size border)
{ matrix m;
  int x, y, max_x, max_y;
  int px, py;
  Graphical gr;
  Cell cell;
  int ln;
  int found, changed;
  char *align_flags;

retry:
  max_x = 0, max_y = 0;
  found = 0;
  changed = 1;
  if ( !(m.units = alloca(sizeof(unit*)*max_columns)) )
  { Cprintf("Alloca(%d) failed\n", sizeof(unit*)*max_columns);
    fail;
  }

  if ( isDefault(gap) )
  { PceWindow sw = getWindowGraphical((Graphical) d);

    if ( instanceOfObject(sw, ClassDialog) )
      gap = getClassVariableValueObject(sw, NAME_gap);
    else
      gap = getClassVariableValueClass(ClassDialog, NAME_gap);

    if ( !gap )
      gap = answerObject(ClassSize, toInt(15), toInt(8), EAV);
  }
  if ( isDefault(border) )
    border = gap;

  for_cell(cell, d->graphicals)
    send(cell->value, NAME_layoutDialog, EAV);

  if ( !PlacedTable )
    PlacedTable = createHashTable(toInt(32), NAME_none);
  else
    clearHashTable(PlacedTable);

  for_cell(cell, d->graphicals)
  { if ( !IsPlaced(cell->value) &&
	 get(cell->value, NAME_autoAlign, EAV) == ON )
    { if ( !placeDialogItem(d, &m, cell->value, 0, 0, &max_x, &max_y) )
      { free_matrix_columns(&m, max_x);
	if ( grow_max_matrix(max_x, max_y) )
	  goto retry;
	else
	  fail;
      }
      found++;
      break;
    }
  }

  if ( found == 0 )
    succeed;				/* finished */

  m.cols = max_x;
  m.rows = max_y;
  align_flags = alloca(max_y+1);

  for(ln = 0; changed && ln < 4; ln++)	/* avoid endless recursion */
  { changed = 0;			/* see whether something changed */

    for(x=0; x<max_x; x++)		/* Align labels and values */
    { int lw = -1;
      int vw = -1;
      int chl = FALSE;
      int chv = FALSE;

#define AUTO_ALIGN_LABEL 1
#define AUTO_ALIGN_VALUE 2

      for(y=0; y<max_y; y++)
      { align_flags[y] = 0;

	if ( notNil(gr = m.units[x][y].item) &&
	     gr->displayed == ON &&
	     m.units[x][y].alignment == NAME_column )
	{ int w;

	  if ( get(gr, NAME_autoLabelAlign, EAV) == ON )
	  { if ( (w = valInt(get(gr, NAME_labelWidth, EAV))) != lw )
	    { if ( lw >= 0 )
		chl++;
	      lw = max(w, lw);
	    }
	    align_flags[y] |= AUTO_ALIGN_LABEL;
	  }

	  if ( get(gr, NAME_autoValueAlign, EAV) == ON )
	  { if ( (w = valInt(get(gr, NAME_valueWidth, EAV))) != vw )
	    { if ( vw >= 0 )
		chv++;
	      vw = max(w, vw);
	    }
	    align_flags[y] |= AUTO_ALIGN_VALUE;
	  }
	}
      }
      if ( chl )
      { changed++;

	for(y=0; y<max_y; y++)
	{ if ( (align_flags[y] & AUTO_ALIGN_LABEL) )
	    send(m.units[x][y].item, NAME_labelWidth, toInt(lw), EAV);
	}
      }
      if ( chv )
      { changed++;

	for(y=0; y<max_y; y++)
	{ if ( (align_flags[y] & AUTO_ALIGN_VALUE) )
	    send(m.units[x][y].item, NAME_valueWidth, toInt(vw), EAV);
	}
      }
    }

    ComputeGraphical(d);		/* recompute for possible changes */

    for(x=0; x<max_x; x++)		/* Get sizes */
    { for(y=0; y<max_y; y++)
      { Unit u = &m.units[x][y];

	if ( notNil(gr = u->item) && gr->displayed == ON )
	{ Point reference = get(gr, NAME_reference, EAV);
	  int rx = (reference ? valInt(reference->x) : 0);
	  int ry = (reference ? valInt(reference->y) : 0);
	  Int hs = get(gr, NAME_horStretch, EAV);
	  Int vs = get(gr, NAME_verStretch, EAV);

	  if ( !hs ) hs = ZERO;
	  if ( !vs ) vs = ZERO;

	  u->left     = rx;
	  u->height   = ry;
	  u->depth    = valInt(gr->area->h) - ry;
	  u->right    = valInt(gr->area->w) - rx;
	  u->hstretch = valInt(hs);
	  u->vstretch = valInt(vs);

	  DEBUG(NAME_layout,
		Cprintf("%d,%d %s lrhd=%d %d %d %d\n",
			x+1, y+1, pp(gr),
			u->left, u->right, u->height, u->depth));
	} else
	{ u->left     = 0;
	  u->height   = 0;
	  u->depth    = 0;
	  u->right    = 0;
	}
      }
    }


    for(x=0; x<max_x; x++)		/* Determine unit width */
    { int r = 0, l = 0;

      for(y=0; y<max_y; y++)
      { if ( m.units[x][y].alignment == NAME_column )
	{ if ( m.units[x][y].right > r ) r = m.units[x][y].right;
	  if ( m.units[x][y].left  > l ) l = m.units[x][y].left;
	}
      }

      for(y=0; y<max_y; y++)
      { if ( m.units[x][y].alignment == NAME_column )
	{ m.units[x][y].right = r;
	  m.units[x][y].left = l;
	}
      }
    }
    stretchColumns(&m, gap, bb, border);
    determineXColumns(&m, gap, bb, border);


  { int gaph = valInt(gap->h);

    for(y=0; y<max_y; y++)		/* Determine unit height */
    { int h = -1000, d = -1000;

      for(x=0; x<max_x; x++)
      { if ( m.units[x][y].height > h ) h = m.units[x][y].height;
	if ( m.units[x][y].depth  > d ) d = m.units[x][y].depth;
      }
      DEBUG(NAME_layout,
	    Cprintf("Row %d +ascent-descent +%d-%d\n", y+1, h, d));
      for(x=0; x<max_x; x++)
      { m.units[x][y].height = h;
	m.units[x][y].depth = d;
      }
    }

					/* distribute in Y-direction */
    if ( notDefault(bb) && max_y > 1 && valInt(bb->h) > 0 )
      stretchRows(&m,
		  valInt(bb->h) - valInt(border->h) * 2 - (max_y-1) * gaph);

					  /* Place the items */
    for(py = valInt(border->h), y=0; y<max_y; y++)
    { int px = valInt(border->w);
      int lx = px;			/* x for left aligned items */
      int gapw = valInt(gap->w);

      if ( m.units[0][y].depth == 0 && m.units[0][y].height == 0 )
      { DEBUG(NAME_layout, Cprintf("Skipping empty row %d\n", y+1));
	continue;			/* empty row (not displayed) */
      }

      for(x = 0; x < max_x; x++)
      { if ( notNil(gr = m.units[x][y].item) &&
	     gr->displayed == ON )
	{ Point reference = get(gr, NAME_reference, EAV);
	  int rx = (reference ? valInt(reference->x) : 0);
	  int ry = (reference ? valInt(reference->y) : 0);
	  int ix, iy = py + m.units[x][y].height;
	  Int iw = DEFAULT;
	  Int ih = DEFAULT;

	  DEBUG(NAME_layout,
		Cprintf("Placing %s at %d,%d\n", pp(gr), x+1,y+1));

	  if ( m.units[x][y].alignment == NAME_column )
	    ix = m.units[x][y].x;
	  else
	    ix = lx;
	  ix += m.units[x][y].left;

					/* hor_stretch handling */
	  if ( m.units[x][y].hstretch )
	  { int nx=0;			/* make compiler happy */

	    if ( x+1 < max_x && notNil(m.units[x+1][y].item) )
	    { nx = m.units[x][y].left + m.units[x][y].right + gapw;

	      if ( m.units[x+1][y].alignment == NAME_column )
		nx += px;
	      else
		nx += ix - rx;

	      DEBUG(NAME_layout,
		    Cprintf("Right stretch of %s to next column at %d\n",
			    pp(gr), nx));
	    } else if ( notDefault(bb) )
	    { iw = toInt(valInt(bb->w) - valInt(border->w) - (ix - rx));
	      DEBUG(NAME_layout,
		    Cprintf("Right stretch of %s to BB at %d\n",
			    pp(gr), valInt(bb->w)));
	    } else
	    { m.units[x][y].flags |= DLF_STRETCH_TO_BB;
	      DEBUG(NAME_layout,
		    Cprintf("Flagged right stretch of %s to BB\n",
			    pp(gr)));
	      iw = toInt(m.units[x][y].left + m.units[x][y].right);
	      /* DEBUG(NAME_layout,
		    Cprintf("Right stretch of %s to column width %d\n",
			    pp(gr), valInt(iw))); */
	    }

	    if ( isDefault(iw) )
	      iw = toInt(nx - gapw - (ix - rx));
	  }

	  if ( m.units[x][y].vstretch )	/* ver_stretch handling */
	  { ih = toInt(m.units[x][y].height + m.units[x][y].depth);
	  }

	  changed += adjustDialogItem(gr,
				      toInt(ix - rx), toInt(iy - ry),
				      iw, ih);
	  lx = valInt(gr->area->x) + valInt(gr->area->w) + gapw;
	}
	px += m.units[x][y].left + m.units[x][y].right + gapw;
      }

      py += m.units[0][y].depth + m.units[0][y].height + gaph;
      DEBUG(NAME_layout, Cprintf("Moving to row %d at %d\n",
				 y+1, py));
    }
  }

    ComputeGraphical(d);		/* recompute bounding-box */

    for(y = 0; y < max_y; y++)
    { if ( notDefault(bb) )
      { px = valInt(bb->w);		/* px: right-side of bb */
      } else
      { if ( instanceOfObject(d, ClassWindow) )
	{ PceWindow sw = (PceWindow) d;

	  px = valInt(sw->bounding_box->x) +
	       valInt(sw->bounding_box->w) +
	       valInt(border->w);
	} else
	{ px = valInt(d->area->x) - valInt(d->offset->x) +
	       valInt(d->area->w)/* + valInt(border->w)*/;
	}
      }

      for(x = max_x-1; x >= 0; x--)
      { if ( notNil(gr = m.units[x][y].item) &&
	     gr->displayed == ON )
	{ if ( m.units[x][y].flags & DLF_STRETCH_TO_BB )
	  { int iw = px-valInt(border->w)-valInt(gr->area->x);

	    if ( iw > valInt(gr->area->w) )
	    { adjustDialogItem(gr, DEFAULT, DEFAULT, toInt(iw), DEFAULT);
	      DEBUG(NAME_layout,
		    Cprintf("Delayed right stretch of %s to BB %d\n",
			    pp(gr), iw));
	    }
	  } else if ( m.units[x][y].alignment == NAME_right ||
		      m.units[x][y].alignment == NAME_center )
	  { Name algnmt = m.units[x][y].alignment;
	    int x2;
	    Graphical gr2 = NULL, grl = gr;
	    int tw, dx;

	    DEBUG(NAME_layout, Cprintf("%s is aligned %s\n",
				       pp(gr), pp(algnmt)));

	    for(x2 = x-1; x2 >= 0; x2--)
	    { if ( notNil(gr2 = m.units[x2][y].item) &&
		   gr2->displayed == ON )
	      { if ( m.units[x2][y].alignment != algnmt )
		  break;
		else
		{ DEBUG(NAME_layout, Cprintf("\tadding %s\n",
					     pp(m.units[x2][y].item)));
		  grl = gr2;
		}
	      }
	    }

	    tw = valInt(getRightSideGraphical(gr)) - valInt(grl->area->x);

	    if ( m.units[x][y].alignment == NAME_right )
	      dx = px - tw - valInt(gap->w);
	    else
	    { int sx = (x2 < 0 ? 0 : valInt(getRightSideGraphical(gr2)));
	      DEBUG(NAME_layout, Cprintf("sx = %d; ", sx));
	      dx = (px - sx - tw)/2 + sx;
	    }
	    dx -= valInt(getLeftSideGraphical(grl));

	    DEBUG(NAME_layout,
		  Cprintf("R = %d; Total width = %d, shift = %d\n",
			  px, tw, dx));

	    for(; ; x--)
	    { if ( notNil(gr = m.units[x][y].item) &&
		   gr->displayed == ON )
	      { changed += adjustDialogItem(gr,
					    toInt(valInt(gr->area->x) + dx),
					    DEFAULT, DEFAULT, DEFAULT);
		DEBUG(NAME_layout, Cprintf("\t moved %s\n", pp(gr)));
		if ( gr == grl )
		  break;
	      }
	    }
	  }

	  px = valInt(gr->area->x);
	}
      }
    }
  }

  free_matrix_columns(&m, max_x);

  if ( hasSendMethodObject(d, NAME_assignAccelerators) )
    send(d, NAME_assignAccelerators, EAV);

  { PceWindow sw;

    if ( (sw = getWindowGraphical((Graphical) d)) &&
	 isNil(sw->keyboard_focus) )
      send(d, NAME_advance, NIL, EAV);
  }

  succeed;
}

status
appendDialogItemDevice(Device d, Graphical item, Name where)
{ Graphical di;
  Name algmnt;

  if ( emptyChain(d->graphicals) )
    return appendDialogItemNetworkDevice(d, item);

  send(item, NAME_autoAlign, ON, EAV);

  di = getTailChain(d->graphicals);
  if ( isDefault(where) )
  { if ( instanceOfObject(di, ClassButton) &&
	 instanceOfObject(item, ClassButton) )
      where = NAME_right;
    else
      where = NAME_nextRow;
  } else if ( where == NAME_right &&
	      (algmnt = get(di, NAME_alignment, EAV)) != NAME_column )
    send(item, NAME_alignment, algmnt, EAV);

  if ( where == NAME_nextRow )
  { Graphical left;

    while ( (left = get(di, NAME_right, EAV)) && notNil(left) )
      di = left;
    where = NAME_below;
  }
					/* Do not use the implementation of */
					/* class window */
  return vm_send(item, where, ClassGraphical, 1, (Any *)&di);
}


		/********************************
		*         MISCELLANEOUS		*
		********************************/


static status
convertLoadedObjectDevice(Device dev, Int ov, Int cv)
{ if ( isNil(dev->recompute) )
    assign(dev, recompute, newObject(ClassChain, EAV));

  succeed;
}


static status
reparentDevice(Device dev)
{ Cell cell;

  if ( isNil(dev->device) )
    assign(dev, level, ZERO);
  else
    assign(dev, level, add(dev->device->level, ONE));

  for_cell(cell, dev->graphicals)
    qadSendv(cell->value, NAME_reparent, 0, NULL);

  return reparentGraphical((Graphical) dev);
}


static status
roomDevice(Device dev, Area area)
{ register Cell cell;

  ComputeGraphical(dev);
  for_cell(cell, dev->graphicals)
  { Graphical gr = cell->value;

    if ( gr->displayed == ON && overlapArea(gr->area, area) )
      fail;
  }

  succeed;
}


static Chain
getInsideDevice(Device dev, Area a)
{ register Cell cell;
  Chain ch;

  ch = answerObject(ClassChain, EAV);

  ComputeGraphical(dev);
  for_cell(cell, dev->graphicals)
  { if ( insideArea(a, ((Graphical) cell->value)->area) )
      appendChain(ch, cell->value);
  }

  answer(ch);
}


static status
resizeDevice(Device dev, Real xfactor, Real yfactor, Point origin)
{ float xf, yf;
  int ox = valInt(dev->offset->x);
  int oy = valInt(dev->offset->y);
  Point p;
  Cell cell;

  init_resize_graphical(dev, xfactor, yfactor, origin, &xf, &yf, &ox, &oy);
  if ( xf == 1.0 && yf == 1.0 )
    succeed;

  p = tempObject(ClassPoint, toInt(ox - valInt(dev->offset->x)),
		 	     toInt(oy - valInt(dev->offset->y)), EAV);
  for_cell(cell, dev->graphicals)
    send(cell->value, NAME_resize, xfactor, yfactor, p, EAV);
  considerPreserveObject(p);

  succeed;
}


		/********************************
		*         NAMED MEMBERS		*
		********************************/

Graphical
getMemberDevice(Device dev, Name name)
{ if ( notNil(dev->graphicals) )
  { Cell cell;

    for_cell(cell, dev->graphicals)
    { if (((Graphical)cell->value)->name == name)
	answer(cell->value);
    }
  }

  fail;
}


static status
forSomeDevice(Device dev, Name name, Code msg)
{ Cell cell, c2;

  for_cell_save(cell, c2, dev->graphicals)
  { Graphical gr = cell->value;

    if ( isDefault(name) || gr->name == name )
      forwardReceiverCode(msg, dev, gr, EAV);
  }

  succeed;
}


static status
forAllDevice(Device dev, Name name, Code msg)
{ Cell cell, c2;

  for_cell_save(cell, c2, dev->graphicals)
  { Graphical gr = cell->value;

    if ( isDefault(name) || gr->name == name )
      TRY(forwardReceiverCode(msg, dev, gr, EAV));
  }

  succeed;
}

		/********************************
		*            MOVING		*
		********************************/


status
updateConnectionsDevice(Device dev, Int level)
{ Cell cell;

  for_cell(cell, dev->graphicals)
  { if ( instanceOfObject(cell->value, ClassDevice) )
      updateConnectionsDevice(cell->value, level);
    else
      updateConnectionsGraphical(cell->value, level);
  }

  return updateConnectionsGraphical((Graphical) dev, level);
}


status
geometryDevice(Device dev, Int x, Int y, Int w, Int h)
{ ComputeGraphical(dev);

  if ( isDefault(x) ) x = dev->area->x;
  if ( isDefault(y) ) y = dev->area->y;

  if ( x != dev->area->x || y != dev->area->y )
  { Int dx = sub(x, dev->area->x);
    Int dy = sub(y, dev->area->y);

    CHANGING_GRAPHICAL(dev,
	assign(dev->offset, x, add(dev->offset->x, dx));
	assign(dev->offset, y, add(dev->offset->y, dy));
	if ( notNil(dev->clip_area) )
	{ assign(dev, badBoundingBox, ON); /* TBD: ??? */
	  computeBoundingBoxDevice(dev);
	} else
	{ assign(dev->area, x, x);
	  assign(dev->area, y, y);
	});

    updateConnectionsDevice(dev, sub(dev->level, ONE));
  }

  succeed;
}


		/********************************
		*           REFERENCE		*
		********************************/


static status
referenceDevice(Device dev, Point pos)
{ Int x, y;

  if ( isDefault(pos) )
  { ComputeGraphical(dev);
    x = sub(dev->area->x, dev->offset->x);
    y = sub(dev->area->y, dev->offset->y);
  } else
  { x = pos->x;
    y = pos->y;
  }

  if ( x != ZERO || y != ZERO )
  { Cell cell;
    Point move = tempObject(ClassPoint, sub(ZERO, x), sub(ZERO, y), EAV);

    offsetPoint(dev->offset, x, y);
    for_cell(cell, dev->graphicals)
      relativeMoveGraphical(cell->value, move);

    considerPreserveObject(move);
  }

  succeed;
}


static status
set_position_device(Device dev, Int x, Int y)
{ ComputeGraphical(dev);

  if ( isDefault(x) ) x = dev->offset->x;
  if ( isDefault(y) ) y = dev->offset->y;

  x = add(dev->area->x, sub(x, dev->offset->x));
  y = add(dev->area->y, sub(y, dev->offset->y));

  return setGraphical(dev, x, y, DEFAULT, DEFAULT);
}


static status
positionDevice(Device dev, Point pos)
{ return set_position_device(dev, pos->x, pos->y);
}


static status
xDevice(Device dev, Int x)
{ return set_position_device(dev, x, DEFAULT);
}


static status
yDevice(Device dev, Int y)
{ return set_position_device(dev, DEFAULT, y);
}


static Point
getPositionDevice(Device dev)
{ ComputeGraphical(dev);
  answer(dev->offset);
}


static Int
getXDevice(Device dev)
{ answer(getPositionDevice(dev)->x);
}


static Int
getYDevice(Device dev)
{ answer(getPositionDevice(dev)->y);
}


static Point
getOffsetDevice(Device dev)
{ ComputeGraphical(dev);
  answer(dev->offset);
}


		/********************************
		*           CATCH ALL		*
		********************************/

static Any
getCatchAllDevice(Device dev, Name name)
{ Name base;

  if ( (base = getDeleteSuffixName(name, NAME_Member)) )
    answer(getMemberDevice(dev, base));

  errorPce(dev, NAME_noBehaviour, CtoName("<-"), name);
  fail;
}

		/********************************
		*             VISUAL		*
		********************************/

static Chain
getContainsDevice(Device dev)
{ answer(dev->graphicals);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_DnameD_code[] =
        { "[name]", "code" };
static char *T_find[] =
        { "at=[point|event]", "condition=[code]" };
static char *T_pointedObjects[] =
        { "at=point|event", "append_to=[chain]" };
static char *T_typed[] =
        { "event_id", "[bool]" };
static char *T_format[] =
        { "format*|name", "[any]" };
static char *T_layout[] =
        { "gap=[size]", "size=[size]", "border=[size]" };
static char *T_modifiedItem[] =
        { "graphical", "bool" };
static char *T_display[] =
        { "graphical", "position=[point]" };
static char *T_appendDialogItem[] =
        { "item=graphical", "relative_to_last=[{below,right,next_row}]" };
static char *T_convertLoadedObject[] =
        { "old_version=int", "new_version=int" };
static char *T_changedUnion[] =
        { "ox=int", "oy=int", "ow=int", "oh=int" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_resize[] =
        { "x_factor=real", "y_factor=[real]", "origin=[point]" };
static char *T_flash[] =
	{ "area=[area]", "time=[int]" };
static char *T_advance[] =
	{ "from=[graphical]*",
	  "propagate=[bool]",
	  "direction=[{forwards,backwards}]"
	};

/* Instance Variables */

static vardecl var_device[] =
{ IV(NAME_level, "int", IV_GET,
     NAME_organisation, "Nesting depth to topmost device"),
  IV(NAME_offset, "point", IV_NONE,
     NAME_area, "Offset of origin"),
  IV(NAME_clipArea, "area*", IV_NONE,
     NAME_scroll, "Clip all graphicals to this area"),
  IV(NAME_graphicals, "chain", IV_GET,
     NAME_organisation, "Displayed graphicals (members)"),
  IV(NAME_pointed, "chain", IV_GET,
     NAME_event, "Graphicals pointed-to by the mouse"),
  SV(NAME_layoutManager, "layout_manager*", IV_GET|IV_STORE, layoutManagerDevice,
     NAME_layout, "Layout manager for <-graphicals"),
  IV(NAME_format, "format*", IV_GET,
     NAME_layout, "Use tabular layout"),
  IV(NAME_badFormat, "bool", IV_NONE,
     NAME_update, "Format needs to be recomputed"),
  IV(NAME_badBoundingBox, "bool", IV_NONE,
     NAME_update, "Indicate bounding box is out-of-date"),
  IV(NAME_recompute, "chain", IV_NONE,
     NAME_update, "Graphicals that requested a recompute")
};

/* Send Methods */

static senddecl send_device[] =
{ SM(NAME_geometry, 4, T_geometry, geometryDevice,
     DEFAULT, "Move device"),
  SM(NAME_initialise, 0, NULL, initialiseDevice,
     DEFAULT, "Create an empty device"),
  SM(NAME_unlink, 0, NULL, unlinkDevice,
     DEFAULT, "Clear device and unlink from super-device"),
  SM(NAME_typed, 2, T_typed, typedDevice,
     NAME_accelerator, "Handle accelerators"),
  SM(NAME_foreground, 1, "colour", colourGraphical,
     NAME_appearance, "Default colour for all members"),
  SM(NAME_move, 1, "point", positionDevice,
     NAME_area, "Set origin"),
  SM(NAME_position, 1, "point", positionDevice,
     NAME_area, "Set origin"),
  SM(NAME_reference, 1, "[point]", referenceDevice,
     NAME_area, "Move origin, while moving members opposite"),
  SM(NAME_resize, 3, T_resize, resizeDevice,
     NAME_area, "Resize device with specified factor"),
  SM(NAME_x, 1, "int", xDevice,
     NAME_area, "Set X of origin"),
  SM(NAME_y, 1, "int", yDevice,
     NAME_area, "Set Y of origin"),
  SM(NAME_convertLoadedObject, 2, T_convertLoadedObject,
     convertLoadedObjectDevice,
     NAME_compatibility, "Initialise recompute and request_compute"),
  SM(NAME_event, 1, "event", eventDevice,
     NAME_event, "Process an event"),
  SM(NAME_updatePointed, 1, "event", updatePointedDevice,
     NAME_event, "Update <-pointed, sending area_enter and area_exit events"),
  SM(NAME_advance, 3, T_advance, advanceDevice,
     NAME_focus, "Advance keyboard focus to next item"),
  SM(NAME_flash, 2, T_flash, flashDevice,
     NAME_report, "Alert visual by temporary inverting"),
  SM(NAME_forAll, 2, T_DnameD_code, forAllDevice,
     NAME_iterate, "Run code on graphicals; demand acceptance"),
  SM(NAME_forSome, 2, T_DnameD_code, forSomeDevice,
     NAME_iterate, "Run code on all graphicals"),
  SM(NAME_format, 2, T_format, formatDevice,
     NAME_layout, "Use tabular layout"),
  SM(NAME_layoutDialog, 3, T_layout, layoutDialogDevice,
     NAME_layout, "(Re)compute layout of dialog_items"),
  SM(NAME_room, 1, "area", roomDevice,
     NAME_layout, "Test if no graphicals are in area"),
  SM(NAME_appendDialogItem, 2, T_appendDialogItem, appendDialogItemDevice,
     NAME_organisation, "Append dialog_item {below,right,next_row} last"),
  SM(NAME_clear, 1, "[{destroy,free,erase}]", clearDevice,
     NAME_organisation, "Erase all graphicals"),
  SM(NAME_display, 2, T_display, displayDevice,
     NAME_organisation, "Display graphical at point"),
  SM(NAME_erase, 1, "graphical", eraseDevice,
     NAME_organisation, "Erase a graphical"),
  SM(NAME_reparent, 0, NULL, reparentDevice,
     NAME_organisation, "Device's parent-chain has changed"),
  SM(NAME_DrawPostScript, 1, "{head,body}", drawPostScriptDevice,
     NAME_postscript, "Create PostScript"),
  SM(NAME_changedUnion, 4, T_changedUnion, changedUnionDevice,
     NAME_resize, "Trap changes to the union of all graphicals"),
  SM(NAME_selection, 1, "graphical|chain*", selectionDevice,
     NAME_selection, "Set selection to graphical or chain"),
  SM(NAME_compute, 0, NULL, computeDevice,
     NAME_update, "Recompute device"),
  SM(NAME_modifiedItem, 2, T_modifiedItem, failObject,
     NAME_virtual, "Trap modification of item (fail)")
};

/* Get Methods */

static getdecl get_device[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsDevice,
     DEFAULT, "Chain with visuals contained"),
  GM(NAME_offset, 0, "point", NULL, getOffsetDevice,
     NAME_area, "Get origin (also <-position)"),
  GM(NAME_position, 0, "point", NULL, getPositionDevice,
     NAME_area, "Get origin"),
  GM(NAME_x, 0, "int", NULL, getXDevice,
     NAME_area, "Get X of origin"),
  GM(NAME_y, 0, "int", NULL, getYDevice,
     NAME_area, "Get Y of origin"),
  GM(NAME_pointedObjects, 2, "chain", T_pointedObjects,
     getPointedObjectsDevice,
     NAME_event, "New chain holding graphicals at point|event"),
  GM(NAME_defaultButton, 0, "button", NULL, getDefaultButtonDevice,
     NAME_accelerator, "Current Button connected to `RET'"),
  GM(NAME_catchAll, 1, "graphical", "name", getCatchAllDevice,
     NAME_organisation, "Find named graphicals"),
  GM(NAME_member, 1, "graphical", "graphical_name=name", getMemberDevice,
     NAME_organisation, "Find named graphical"),
  GM(NAME_find, 2, "graphical", T_find, getFindDevice,
     NAME_search, "Find most local graphical"),
  GM(NAME_displayedCursor, 0, "cursor*", NULL, getDisplayedCursorDevice,
     NAME_cursor, "Currently displayed cursor"),
  GM(NAME_inside, 1, "chain", "area", getInsideDevice,
     NAME_selection, "New chain with graphicals inside area"),
  GM(NAME_selection, 0, "chain", NULL, getSelectionDevice,
     NAME_selection, "New chain of selected graphicals")
};

/* Resources */

#define rc_device NULL
/*
static classvardecl rc_device[] =
{
};
*/

/* Class Declaration */

ClassDecl(device_decls,
          var_device, send_device, get_device, rc_device,
          0, NULL,
          "$Rev$");


status
makeClassDevice(Class class)
{ declareClass(class, &device_decls);
  setRedrawFunctionClass(class, RedrawAreaDevice);

  succeed;
}
