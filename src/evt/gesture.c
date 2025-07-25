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

static int	 tryDragScrollGesture(Gesture g, EventObj ev);
static int	 cancelDragScrollGesture(Gesture g);
static Graphical getScrollTarget(Gesture g, EventObj ev);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
A  gesture describes  a  sequence  of mouse-events,   starting  with a
button-down upto the corresponding button-up event.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
initialiseGesture(Gesture g, Name button, Modifier modifier)
{ if ( notDefault(button) )
    assign(g, button, button);
  if ( notDefault(modifier) )
    assign(g, modifier, modifier);

  assign(g, active,	 ON);
  assign(g, status,	 NAME_inactive);
  assign(g, cursor,	 DEFAULT);

  return obtainClassVariablesObject(g);
}


status
eventGesture(Any obj, EventObj ev)
{ Gesture g = obj;

  if ( g->active == OFF )
    fail;

  if ( g->status == NAME_active &&
       notNil(g->drag_scroll) )
  { Graphical gr;

    if ( tryDragScrollGesture(g, ev) )
      succeed;
    if ( isAEvent(ev, NAME_wheel) &&
	 (gr=getScrollTarget(g, ev)) )
      return postEvent(ev, gr, DEFAULT);
  }

  if ( isDownEvent(ev) &&
       hasModifierEvent(ev, g->modifier) &&
       getButtonEvent(ev) == g->button &&
       (isNil(g->condition) || forwardReceiverCode(g->condition, g, ev, EAV)) &&
       send(g, NAME_verify, ev, EAV) )
  { TRY( send(g, NAME_initiate, ev, EAV) );
    assign(g, status, NAME_active);
    send(ev->window, NAME_focus,
	 ev->receiver, g, g->cursor, getButtonEvent(ev), EAV);
    succeed;
  } else if ( g->status != NAME_inactive )
  { if ( isDragEvent(ev) )
    { send(g, NAME_drag, ev, EAV);
      succeed;
    } else if ( isUpEvent(ev) && getButtonEvent(ev) == g->button )
    { cancelDragScrollGesture(g);
      send(g, NAME_terminate, ev, EAV);
      assign(g, status, NAME_inactive);
      succeed;
    }
  }

  fail;
}


static status
succeedGesture(Gesture g, EventObj ev)
{ succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cancel a gesture. Typically deals with  click-gestures after the pointer
has moved too far. The gesture  undoes   focus,  switches itself off and
then reposts the initial event to see   if another gesture wants to have
the event. It posts the event on which  is was started. This is normally
window<-current event. Hence the hack   there. See postEventWindow() for
further details.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
cancelGesture(Gesture g, EventObj ev)
{ PceWindow sw = ev->window;
  EventObj fe = sw->focus_event;
  EventObj oev;

  addCodeReference(fe);
  assign(g, active, OFF);
  send(sw, NAME_focus, NIL, EAV);
  if ( notNil(fe) )
    send(sw, NAME_event, fe, EAV);

  if ( ev != sw->focus_event )
  { addCodeReference(ev);
    oev = sw->current_event;
    assign(sw, current_event, NIL);
    send(sw, NAME_postEvent, ev, EAV);
    assign(sw, current_event, oev);
    delCodeReference(ev);
  }

  assign(g, active, ON);
  delCodeReference(fe);
  freeableObj(fe);
  assign(g, status, NAME_inactive);

  succeed;
}


		 /*******************************
		 *	   DRAG-SCROLLING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Drag-scrolling.  This  deals  with  dragging  the    mouse  out  of  the
client-area. If the object is scrollable,   we  should scroll the object
and submit drag/move events.

This behaviour is required for pictures, editors and list_browsers. This
suggests implementation at the level of  devices. This doesn't appear to
be very attractive as a  significant   amount  of context information is
required and I don't want more slots in class device.

A good solution would be the implementation  as a recogniser. This makes
it possible to associate them with any  possible object, deal with focus
handling and avoid further complication of   the  generic event handling
code.  Unfortunately  however,  events  are    normally  focussed  on  a
recogniser while dragging is in progress,   so  the new recogniser won't
see them.

The only solution appears implementation of the infra-structure into the
generic gesture infra-structure. This makes it   independent of the type
of graphical, relatively low cost (there should  not be too many gesture
instances in an application) and easy to modify.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
restrictAreaEvent(EventObj ev, Graphical gr)
{ Int X, Y;
  int ex, ey, aw, ah;
  int dx=0, dy=0;

  if ( isDefault(gr) )
    gr = ev->receiver;
  if ( !get_xy_event(ev, gr, ON, &X, &Y) )
    fail;

  ex = valInt(X);
  ey = valInt(Y);

  aw = valInt(gr->area->w);
  ah = valInt(gr->area->h);

  if ( ex < 0 )
    dx = -ex;
  else if ( ex > aw )
    dx = aw-ex;
  if ( ey < 0 )
    dy = -ey;
  else if ( ey > ah )
    dy = ah-ey;

  if ( dx )
    assign(ev, x, toInt(valInt(ev->x)+dx));
  if ( dy )
    assign(ev, y, toInt(valInt(ev->y)+dy));

  succeed;
}


static Graphical
getScrollTarget(Gesture g, EventObj ev)
{ Graphical gr = ev->receiver;

  if ( g->drag_scroll == NAME_device )
  { gr = (Graphical)gr->device;
  } else if ( g->drag_scroll == NAME_search )
  { for(;;)
    { if ( hasSendMethodObject(gr, NAME_scrollHorizontal) ||
	   hasSendMethodObject(gr, NAME_scrollVertical) )
	break;

      gr = (Graphical)gr->device;
      if ( isNil(gr) )
	fail;
    }
  }

  return gr;
}


#define DRAGSCROLL_MARGIN 50

static status
scrollMessage(Gesture g, EventObj ev,
	      Graphical *client, Name *Msg, Int *Amount)
{ Graphical gr = ev->receiver;
  Int X, Y;
  int ex, ey, aw, ah;
  Name msg;
  Int amount;

  if ( !(isDragEvent(ev) ||
	 isAEvent(ev, NAME_locMove) ||
	 isAEvent(ev, NAME_area)) )
    fail;

  if ( !(gr = getScrollTarget(g, ev)) )
    fail;
  if ( !get_xy_event(ev, gr, ON, &X, &Y) )
    fail;
  ex = valInt(X);
  ey = valInt(Y);

  aw = valInt(gr->area->w);
  ah = valInt(gr->area->h);

  DEBUG(NAME_dragScroll,
	Cprintf("Event on %s at %d,%d, area 0,0-%d,%d\n",
		pp(gr), ex, ey, aw, ah));

  if ( ex < 0 && ey >= 0 && ey <= ah && ex > -DRAGSCROLL_MARGIN )
  { msg = NAME_scrollHorizontal;
    amount = toInt(-1);
  } else if ( ex > aw && ey >= 0 && ey <= ah && ex < aw + DRAGSCROLL_MARGIN )
  { msg = NAME_scrollHorizontal;
    amount = toInt(1);
  } else if ( ey < 0 && ex >= 0 && ey <= aw && ey > -DRAGSCROLL_MARGIN )
  { msg = NAME_scrollVertical;
    amount = toInt(-1);
  } else if ( ey > ah && ex >= 0 && ey <= aw && ey < ah + DRAGSCROLL_MARGIN )
  { msg = NAME_scrollVertical;
    amount = toInt(1);
  } else
    fail;

  DEBUG(NAME_dragScroll,
	if ( !Msg )
	  Cprintf("%s %s\n", pp(msg), pp(amount)));

  if ( Msg )
    *Msg = msg;
  if ( Amount )
    *Amount = amount;
  if ( client )
    *client = gr;

  succeed;
}


static int
scrollGesture(Gesture g)
{ Name msg;
  Int amount;
  Graphical gr;
  Name dir = NAME_forwards;

  if ( !scrollMessage(g, g->drag_scroll_event, &gr, &msg, &amount) )
    fail;

  if ( valInt(amount) < 0 )
  { dir = NAME_backwards;
    amount = toInt(-valInt(amount));
  }

  if ( hasSendMethodObject(gr, msg) &&
       send(gr, msg, dir, NAME_line, amount, EAV) )
  { EventObj ev = getCloneObject(g->drag_scroll_event); /* TBD: optimise? */

    DEBUG(NAME_dragScroll,
	  Cprintf("Drag event = %s, receiver %s\n",
		  pp(ev->id), pp(ev->receiver)));

    ComputeGraphical(gr);
    restrictAreaEvent(ev, gr);		/* Normalise to area of rec */
    send(g, NAME_drag, ev, EAV);
    doneObject(ev);
  }

  succeed;
}


static int
cancelDragScrollGesture(Gesture g)
{ if ( notNil(g->drag_scroll_timer) )
  { stopTimer(g->drag_scroll_timer);
    assign(g, drag_scroll_timer, NIL);
  }

  assign(g, drag_scroll_event, NIL);

  succeed;
}


static int
tryDragScrollGesture(Gesture g, EventObj ev)
{ status doscroll = scrollMessage(g, ev, NULL, NULL, NULL);

  if ( doscroll )
  { if ( isNil(g->drag_scroll_event) )
    { assign(g, drag_scroll_timer,
	     newObject(ClassTimer, CtoReal(0.06), /* TBD */
		       newObject(ClassMessage, g,
				 NAME_scroll, EAV), EAV));
      startTimer(g->drag_scroll_timer, NAME_repeat);
      assign(g, drag_scroll_event, getCloneObject(ev));
    }
  } else
  { cancelDragScrollGesture(g);
  }

  return doscroll;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "button=[button_name]", "modifier=[modifier]" };

/* Instance Variables */

static vardecl var_gesture[] =
{ IV(NAME_button, "button_name", IV_GET,
     NAME_event, "Mouse button to initiate on"),
  IV(NAME_modifier, "modifier", IV_BOTH,
     NAME_event, "Key modifiers (shift, control and/or meta)"),
  IV(NAME_condition, "code*", IV_BOTH,
     NAME_event, "Additional conditions"),
  IV(NAME_status, "{active,inactive}", IV_BOTH,
     NAME_status, "Current status"),
  IV(NAME_cursor, "[cursor]", IV_BOTH,
     NAME_cursor, "Cursor while active"),
  IV(NAME_dragScroll, "{self,device,search}*", IV_BOTH,
     NAME_scroll, "Scroll if dragged outside area"),
  IV(NAME_dragScrollTimer, "timer*", IV_GET,
     NAME_scroll, "Timer used for drag-scrolling"),
  IV(NAME_dragScrollEvent, "event*", IV_GET,
     NAME_scroll, "Lat event in drag-scrolling")
};

/* Send Methods */

static senddecl send_gesture[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseGesture,
     DEFAULT, "Create from button and modifier"),
  SM(NAME_cancel, 1, "event", cancelGesture,
     NAME_cancel, "Cancel this gesture and try the next"),
  SM(NAME_drag, 1, "event", succeedGesture,
     NAME_event, "Mouse has been dragged (just succeeds)"),
  SM(NAME_event, 1, "event", eventGesture,
     NAME_event, "Handle an event"),
  SM(NAME_initiate, 1, "event", succeedGesture,
     NAME_event, "Initiate the gesture (just succeeds)"),
  SM(NAME_terminate, 1, "event", succeedGesture,
     NAME_event, "Mouse button went up (just succeeds)"),
  SM(NAME_verify, 1, "event", succeedGesture,
     NAME_event, "Verify additional conditions (just succeeds)"),
  SM(NAME_scroll, 0, NULL, scrollGesture,
     NAME_scroll, "Scroll client and issue ->drag")
};

/* Get Methods */

#define get_gesture NULL
/*
static getdecl get_gesture[] =
{
};
*/

/* Resources */

static classvardecl rc_gesture[] =
{ RC(NAME_button, "button_name", "left",
     "Active on which button?"),
  RC(NAME_cursor, "[cursor]", "@default",
     "Cursor while active"),
  RC(NAME_modifier, "modifier", "",
     "Condition on shift, control and meta"),
  RC(NAME_dragScroll, "{self,device,search}*", "@nil",
     "Automatically scroll window while dragging outside"),
  RC(NAME_repeatInterval, "real", "0.06",
     "Scrolling speed for ->drag_scroll'ing")
};

/* Class Declaration */

static Name gesture_termnames[] = { NAME_button, NAME_modifier };

ClassDecl(gesture_decls,
          var_gesture, send_gesture, get_gesture, rc_gesture,
          2, gesture_termnames,
          "$Rev$");

status
makeClassGesture(Class class)
{ return declareClass(class, &gesture_decls);
}
