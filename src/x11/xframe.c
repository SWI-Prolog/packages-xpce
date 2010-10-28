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
#include "include.h"
#include "fshell.h"

#if O_MOTIF
#define XMSTRINGDEFINES
#include <Xm/Xm.h>
#endif

static void	xEventFrame(Widget, FrameObj, XEvent *);
static void	expose_frame(Widget w, FrameObj fr, Region xregion);
static void	destroyFrame(Widget, FrameObj, XtPointer);
static status   updateAreaFrame(FrameObj fr, Int border);
static int	ws_group_frame(FrameObj fr);

#define MainWindow(fr)	     ( isNil(fr->members->head) ? (Any) fr : \
			       fr->members->head->value )


		 /*******************************
		 *	    REFERENCES		*
		 *******************************/

static FrameWsRef
ensureWsRefFrame(FrameObj fr)
{ FrameWsRef frws = fr->ws_ref;

  if ( !frws )
  { frws = alloc(sizeof(frame_ws_ref));
    memset(frws, 0, sizeof(frame_ws_ref));
    frws->check_geometry_when_mapped = TRUE;
    fr->ws_ref = frws;
  }

  return frws;
}


static void
setWidgetFrame(FrameObj fr, Widget w)
{ ensureWsRefFrame(fr)->widget = w;
}


static void
setBusyWindowFrame(FrameObj fr, Window w)
{ ensureWsRefFrame(fr)->busy_window = w;
}


static Window
busyWindowFrame(FrameObj fr)
{ return fr->ws_ref ? ((frame_ws_ref *)fr->ws_ref)->busy_window : 0;
}


static inline Display *
getXDisplayFrame(FrameObj fr)
{ if ( notNil(fr->display) )
  { DisplayWsXref dr = fr->display->ws_ref;

    if ( dr )
      return dr->display_xref;
  }

  return NULL;
}


		 /*******************************
		 *	     (UN)CREATE		*
		 *******************************/

status
ws_created_frame(FrameObj fr)
{ if ( widgetFrame(fr) )
    succeed;

  fail;
}


void
ws_uncreate_frame(FrameObj fr)
{ Widget w;

  if ( (w = widgetFrame(fr)) )
  { DEBUG(NAME_frame, Cprintf("ws_uncreate_frame(%s)\n", pp(fr)));

    XtPopdown(w);
    assign(fr, status, NAME_unmapped);
    setWidgetFrame(fr, NULL);

    XtRemoveCallback(w, XtNdestroyCallback,
		     (XtCallbackProc)destroyFrame, fr);
    XtRemoveCallback(w, XtNeventCallback,
		     (XtCallbackProc)xEventFrame, fr);

    if ( fr->ws_ref )
    { FrameWsRef wsfr = fr->ws_ref;

#ifdef O_XIM
      if ( wsfr->ic )
	XDestroyIC(wsfr->ic);
#endif
      unalloc(sizeof(frame_ws_ref), wsfr);
      fr->ws_ref = NULL;
    }

    XtDestroyWidget(w);
  }
}


status
ws_create_frame(FrameObj fr)
{ Arg args[25];
  Cardinal n = 0;
  Widget w;
  DisplayObj d = fr->display;
  DisplayWsXref r = d->ws_ref;

  XtSetArg(args[n], XtNtitle,		  nameToMB(fr->label));   n++;
  XtSetArg(args[n], XtNmappedWhenManaged, False);                 n++;
  XtSetArg(args[n], XtNwidth,      	  valInt(fr->area->w));   n++;
  XtSetArg(args[n], XtNheight,      	  valInt(fr->area->h));   n++;
  XtSetArg(args[n], XtNinput,		  True);		  n++;
  if ( instanceOfObject(fr->background, ClassColour) )
  { XtSetArg(args[n], XtNbackground, getPixelColour(fr->background, d));
    n++;
  } else
  { Pixmap pm = (Pixmap) getXrefObject(fr->background, d);

    XtSetArg(args[n], XtNbackgroundPixmap, pm); n++;
  }

  if ( notNil(fr->icon_label) )
  { XtSetArg(args[n], XtNiconName, nameToMB(getIconLabelFrame(fr)));
    n++;
  }
  if ( fr->kind == NAME_popup )
  { XtSetArg(args[n], XtNsaveUnder, True);
    n++;
  }
  if ( notNil(fr->icon_image) )
  { XtSetArg(args[n], XtNiconPixmap,
	     getXrefObject(fr->icon_image, fr->display));
    n++;
    if ( notNil(fr->icon_image->mask) )
    { XtSetArg(args[n], XtNiconMask,
	       getXrefObject(fr->icon_image->mask, fr->display));
      n++;
    }
  }
  if ( notNil(fr->icon_position) )
  { XtSetArg(args[n], XtNiconX, valInt(fr->icon_position->x)); n++;
    XtSetArg(args[n], XtNiconY, valInt(fr->icon_position->y)); n++;
  }
#if O_MOTIF
  XtSetArg(args[n], XmNdeleteResponse, XmDO_NOTHING); n++;
#endif

  if ( fr->kind == NAME_toplevel )
    w = XtAppCreateShell(nameToMB(fr->label),
			 "Pce",		/* Resource Class */
			 topLevelFrameWidgetClass,
			 r->display_xref,
			 args, n);
  else
    w = XtCreatePopupShell(
		    nameToMB(fr->label),
		    fr->kind == NAME_popup     ? overrideFrameWidgetClass  :
		    fr->kind == NAME_transient ? transientFrameWidgetClass :
					         topLevelFrameWidgetClass,
		    r->shell_xref,
		    args, n);

  if ( !w )
    return errorPce(fr, NAME_xOpen, fr->display);

  XtAddCallback(w, XtNeventCallback,
		(XtCallbackProc) xEventFrame, (caddr_t) fr);
  XtAddCallback(w, XtNexposeCallback,
		(XtCallbackProc) expose_frame, (caddr_t) fr);
  XtAddCallback(w, XtNdestroyCallback,
		(XtCallbackProc) destroyFrame, (caddr_t) fr);

  setWidgetFrame(fr, w);

  succeed;
}


void
ws_realise_frame(FrameObj fr)
{ LocalArray(Widget, children, valInt(getSizeChain(fr->members)));
  int n = 0;
  Widget w = widgetFrame(fr);
  Cell cell;
  DisplayWsXref r = fr->display->ws_ref;
  XClassHint clhint = {0};

  for_cell(cell, fr->members)
    children[n++] = widgetWindow(cell->value);

  XtManageChildren(children, n);
  XtRealizeWidget(w);

  for_cell(cell, fr->members)
    send(cell->value, NAME_geometry, EAV); /* see note at ws_create_window */

  if ( notNil(fr->transient_for) )
  { XSetTransientForHint(r->display_xref,
			 XtWindow(w),
			 XtWindow(widgetFrame(fr->transient_for)));
  }
#if O_MOTIF
  else
  { XDeleteProperty(r->display_xref,
		    XtWindow(w),
		    XA_WM_TRANSIENT_FOR);
  }
#endif
  clhint.res_name  = nameToMB(fr->label);
  clhint.res_class = nameToMB(get(fr->class->name, NAME_labelName, EAV));
  XSetClassHint(r->display_xref, XtWindow(w), & clhint);


  ws_frame_background(fr, fr->background); /* Why is this necessary? */
  ws_group_frame(fr);
}


		 /*******************************
		 *   FIND WINDOW HOLDING POINT	*
		 *******************************/

PceWindow
ws_window_holding_point_frame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { WindowDecorator dw = (WindowDecorator) sw;
      sw = dw->window;
    }

    if ( sw->has_pointer == ON )
      answer(sw);
  }

  fail;
}

		 /*******************************
		 *	    HIDE/EXPOSE		*
		 *******************************/

void
ws_raise_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w )
  { Window win = XtWindow(w);
    static Atom atom;
    XEvent xev;
    XWindowAttributes attr;

    XMapWindow(r->display_xref, win);
    XRaiseWindow(r->display_xref, win);

    if ( !atom )
      atom = XInternAtom(r->display_xref, "_NET_ACTIVE_WINDOW", False);

    xev.xclient.type = ClientMessage;
    xev.xclient.serial = 0;
    xev.xclient.send_event = True;
    xev.xclient.display = r->display_xref;
    xev.xclient.window = win;
    xev.xclient.message_type = atom;
    xev.xclient.format = 32;
    xev.xclient.data.l[0] = 2;
    xev.xclient.data.l[1] = 0;
    xev.xclient.data.l[2] = 0;
    xev.xclient.data.l[3] = 0;
    xev.xclient.data.l[4] = 0;

    XGetWindowAttributes(r->display_xref, win, &attr);
    XSendEvent(r->display_xref,
	       attr.root, False,
	       SubstructureRedirectMask | SubstructureNotifyMask,
	       &xev);

    DEBUG(NAME_frame, Cprintf("Sent _NET_ACTIVE_WINDOW\n"));
  }

  send(fr, NAME_exposed, EAV);		/* doesn't appear to generate a */
					/* CirculateNotify event */
}


void
ws_lower_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w )
    XLowerWindow(r->display_xref, XtWindow(w));
}


		 /*******************************
		 *	   WM-PROTOCOL		*
		 *******************************/

status
ws_attach_wm_prototols_frame(FrameObj fr)
{ Atom *pr = (Atom *)alloca(valInt(getSizeChain(fr->wm_protocols->attributes))
			    * sizeof(Atom));
  Cell cell;
  int n = 0;
  DisplayWsXref r = fr->display->ws_ref;

  for_cell(cell, fr->wm_protocols->attributes)
  { Attribute a = cell->value;
    Name name = checkType(a->name, TypeName, fr);

    if ( name != FAIL )
      pr[n++] = FrameAtom(fr, name);
  }

  DEBUG(NAME_frame, Cprintf("Attaching WM_PROTOCOLS\n"));

  XSetWMProtocols(r->display_xref, XtWindow(widgetFrame(fr)), pr, n);

  assign(fr, wm_protocols_attached, ON);

  succeed;
}


		 /*******************************
		 *	    WINDOW GROUP	*
		 *******************************/

static int
ws_group_frame(FrameObj fr)
{ FrameObj leader;
  Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w &&
       notNil(fr->application) &&
       notNil((leader=fr->application->leader)) &&
       fr != leader )
  { if ( createdFrame(leader) ||
	 send(leader, NAME_create, EAV) )
    { Widget lw = widgetFrame(leader);
      XWMHints hints;

      memset(&hints, 0, sizeof(hints));
      hints.flags = WindowGroupHint;
      hints.window_group = XtWindow(lw);
      XSetWMHints(r->display_xref, XtWindow(w), &hints);
      DEBUG(NAME_leader,
	    Cprintf("Set WindowGroupHint of %s to %s (Xwindow=0x%x)\n",
		    pp(fr), pp(leader), (unsigned int) hints.window_group));

      succeed;
    }
  }

  fail;
}

#ifdef O_XDND
		 /*******************************
		 *	   DRAG-AND-DROP	*
		 *******************************/

struct xdnd_get_drop_info
{ FrameObj frame;			/* accepting frame */
  Window frameWindow;			/* X window of the frame */
  Window root;				/* XPCE root window */
  PceWindow window;			/* client window */
  unsigned char *drop_data;		/* raw drop-data */
  int drop_data_length;			/* length of this */
  int x, y;				/* position of the drop */
  int dropfile;				/* dropping a file */
  Atom XdndTextUriList;			/* text/uri */
  Atom return_type;			/* selected type */
  Atom return_action;			/* selected action */
  Atom *typelist;			/* accepted types */
  Atom *actionlist;			/* accepted actions */
};


static DndClass *
getDndDisplay(DisplayObj display)
{ DisplayWsXref wsref = display->ws_ref;

  if ( !wsref->dnd )
  { wsref->dnd = alloc(sizeof(DndClass));

    xdnd_init(wsref->dnd, wsref->display_xref);
    wsref->XdndTextUriList = XInternAtom(wsref->display_xref,
					 "text/uri-list", False);
  }

  return wsref->dnd;
}


status
setDndAwareFrame(FrameObj fr)
{ Window w = XtWindow(widgetFrame(fr));

  if ( w )
  { DEBUG(NAME_dnd, Cprintf("Registered %s for drag-and-drop\n", pp(fr)));
    xdnd_set_dnd_aware(getDndDisplay(fr->display), w, NULL);
  }

  succeed;
}


static int
widget_insert_drop(DndClass * dnd, unsigned char *data,
		   int length, int remaining,
		   Window into, Window from, Atom type)
{ struct xdnd_get_drop_info *i;

  i = (struct xdnd_get_drop_info *) dnd->user_hook1;

  if (!i->drop_data)
  { i->drop_data = pceMalloc(length);
    if (!i->drop_data)
      return 1;
    memcpy(i->drop_data, data, length);
    i->drop_data_length = length;
  } else
  { unsigned char *t;
    t = pceMalloc(i->drop_data_length + length);
    if (!t)
    { pceFree(i->drop_data);
      i->drop_data = 0;
      return 1;
    }
    memcpy(t, i->drop_data, i->drop_data_length);
    memcpy(t + i->drop_data_length, data, length);
    pceFree(i->drop_data);
    i->drop_data = t;
    i->drop_data_length += length;
  }

  return 0;
}


static int
memberAtomList(Atom a, Atom *list)
{ int i;

  for(i=0; list[i]; i++)
  { if ( list[i] == a )
      succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
x,y are positions in display coordinate-system.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
widget_apply_position(DndClass *dnd, Window widgets_window, Window from,
		      Atom action, int x, int y, Time t, Atom *typelist,
		      int *want_position,
		      Atom *supported_action_return,
		      Atom *desired_type,
		      XRectangle *rectangle)
{ struct xdnd_get_drop_info *info;
  PceWindow target = NIL;

  info = (struct xdnd_get_drop_info *) dnd->user_hook1;

  { DisplayWsXref r = info->frame->display->ws_ref;
    int dx, dy;
    Window child;

    XTranslateCoordinates(r->display_xref,
			  info->root, info->frameWindow,
			  x, y,
			  &dx, &dy, &child);
    if ( child != None &&
	 (target = getMemberHashTable(WindowTable, (Any) child)))
    { if ( instanceOfObject(target, ClassWindowDecorator) )
      { XTranslateCoordinates(r->display_xref, info->root, child,
			      x, y, &dx, &dy,
			      &child);
	if ( child != None )
	  target = getMemberHashTable(WindowTable, (Any) child);
	else
	  target = NIL;
      }
    }
  }

  if ( !target || !hasSendMethodObject(target, NAME_dropFiles) )
    return 0;
  if ( typelist &&
       !memberAtomList(info->XdndTextUriList, typelist) )
    return 0;
  if ( action != dnd->XdndActionCopy )
    return 0;

  *want_position = 1;
  *desired_type = info->XdndTextUriList;
  rectangle->x = rectangle->y = 0;
  rectangle->width = rectangle->height = 0;

  info->window = target;
  info->dropfile = TRUE;
  info->x = x;
  info->y = y;

  return 1;
}


static int
dndEventFrame(FrameObj fr, XEvent *xevent)
{ DndClass *dnd = getDndDisplay(fr->display);

  if ( xevent->type == ClientMessage &&
       xevent->xclient.message_type == dnd->XdndEnter)
  { struct xdnd_get_drop_info i;
    DisplayWsXref r = (DisplayWsXref)fr->display->ws_ref;
    XWindowAttributes atts;

    XGetWindowAttributes(r->display_xref, XtWindow(r->shell_xref), &atts);

    memset(&i, 0, sizeof(i));
    dnd->user_hook1   =	&i;
    i.frame	      = fr;
    i.root	      =	atts.root;
    i.frameWindow     =	XtWindow(widgetFrame(fr));
    i.XdndTextUriList =	r->XdndTextUriList;

    dnd->widget_insert_drop    = widget_insert_drop;
    dnd->widget_apply_position = widget_apply_position;

    for (;;)
    { xdnd_handle_drop_events(dnd, xevent);
      if ( dnd->stage == XDND_DROP_STAGE_IDLE )
	break;
      XNextEvent(dnd->display, xevent);
    }

    if ( i.dropfile )
    { DEBUG(NAME_dnd,
	    Cprintf("%s: got drop-file at %d,%d: %s\n",
		    pp(i.window), i.x, i.y,
		    i.drop_data));

      ServiceMode(is_service_window(i.window),
		  { AnswerMark mark;
		    Chain files;
		    Point pos;
		    char *s = (char *)i.drop_data;
		    char *e = s + i.drop_data_length;

		    markAnswerStack(mark);
		    files = answerObject(ClassChain, EAV);
		    pos   = answerObject(ClassPoint,
					 toInt(i.x), toInt(i.y), EAV);

		    for(; s<e; )
		    { char *start;
		      string str;

		      start = s;
		      while(s<e && !(*s == '\r' || *s == '\n'))
			s++;
		      str_inithdr(&str, FALSE);

		      if ( e-start > 5 && strncmp(start, "file:", 5) == 0 )
			start += 5;

		      if ( str_set_n_ascii(&str, s-start, start) )
			appendChain(files, StringToName(&str));
		      while(s<e && (*s == '\r' || *s == '\n'))
			s++;
		    }

		    pceFree(i.drop_data);

		    send(i.window, NAME_dropFiles, files, pos, EAV);
		    RedrawDisplayManager(TheDisplayManager());
		    rewindAnswerStack(mark, NIL);
		  });
    }

    succeed;
  }

  fail;
}

#endif /*O_XDND*/

		 /*******************************
		 *     XT-CALLBACK HANDLING	*
		 *******************************/

static int
service_frame(FrameObj fr)
{ Application app = fr->application;

  DEBUG(NAME_service, Cprintf("Event on %s, app %s, kind %s\n",
			      pp(fr), pp(app),
			      notNil(app) ? pp(app->kind) : "-"));

  return (notNil(app) && app->kind == NAME_service ? PCE_EXEC_SERVICE
						   : PCE_EXEC_USER);
}


static void
destroyFrame(Widget w, FrameObj fr, XtPointer data)
{ pceMTLock(LOCK_PCE);
  if ( fr->ws_ref )
  { unalloc(sizeof(frame_ws_ref), fr->ws_ref);
    fr->ws_ref = NULL;
  }

  ServiceMode(service_frame(fr),
	      freeObject(fr));
  pceMTUnlock(LOCK_PCE);
}


static void
x_event_frame(Widget w, FrameObj fr, XEvent *event)
{ FrameWsRef wsfr = fr->ws_ref;

  DEBUG(NAME_event, Cprintf("x_event_frame(): X-event %d on %s\n",
			    event->xany.type, pp(fr)));

#ifdef O_XDND
  if ( dndEventFrame(fr, event) )
    return;
#endif /*O_XDND*/

  switch( event->xany.type )
  { case ClientMessage:
    { DEBUG(NAME_frame, Cprintf("Received client message\n"));
					/* Window Manager Request */
      if ( event->xclient.message_type == WmProtocols(fr) )
      { Name name;
	Code msg;

	DEBUG(NAME_frame,
	      Cprintf("Protocol message %s\n",
		      FrameAtomToString(fr, event->xclient.data.l[0])));

	name = CtoName(FrameAtomToString(fr, event->xclient.data.l[0]));
	if ( (msg = checkType(getValueSheet(fr->wm_protocols, name),
			      TypeCode, fr)) )
	  forwardReceiverCode(msg, fr, MainWindow(fr), EAV);
      }
      return;
    }
    case ConfigureNotify:
    { updateAreaFrame(fr, toInt(event->xconfigure.border_width));
      return;
    }
    case PropertyNotify:
      if ( fr->wm_protocols_attached == OFF &&
	   event->xproperty.atom == WmProtocols(fr) )
      { if ( fr->kind != NAME_popup )
	  ws_attach_wm_prototols_frame(fr);
      }
      return;
    case MapNotify:
    { Cell cell;

      for_cell(cell, fr->members)
	send(cell->value, NAME_displayed, ON, EAV);
      updateAreaFrame(fr, DEFAULT);
      send(fr, NAME_mapped, ON, EAV);
      if ( wsfr && wsfr->check_geometry_when_mapped &&
	   notNil(fr->geometry) )	/* see ws_x_geometry_frame() */
      { wsfr->check_geometry_when_mapped = FALSE;
	ws_x_geometry_frame(fr, fr->geometry, DEFAULT);
      }
      assign(fr, status, NAME_window);

					/* some window managers don't */
					/* do this, so we do it ourselves */
					/* Note that XSetInputFocus can */
					/* generate errors.  There seem */
					/* to be no easy way out */
      if ( notNil(fr->modal) )
      { Display *d = getXDisplayFrame(fr);
	Widget wfr = widgetFrame(fr);
	Window win = XtWindow(wfr);

	if ( d && wfr == w && win )
	  XSetInputFocus(d, win, RevertToParent, CurrentTime);
      }

      return;
    }
    case UnmapNotify:
    { Cell cell;

      for_cell(cell, fr->members)
	send(cell->value, NAME_displayed, OFF, EAV);
      if ( !isFreedObj(fr) || isFreeingObj(fr) )
	send(fr, NAME_mapped, OFF, EAV);
      assign(fr, status, NAME_hidden);
      return;
    }
    case CirculateNotify:
      if ( event->xcirculate.place == PlaceOnTop )
      	send(fr, NAME_exposed, EAV);
      else
	send(fr, NAME_hidden, EAV);
      return;
    case FocusIn:
    {
#if 0
      FrameObj fr2;		/* this code causes BadMatch errors.  Why? */

      if ( (fr2=blockedByModalFrame(fr)) )
      { Display *d = getXDisplayFrame(fr2);
	Widget wfr = widgetFrame(fr2);
	Window win = XtWindow(wfr);

	if ( d && win )
	  XSetInputFocus(d, win, RevertToParent, CurrentTime);
      } else
#endif
      {
#ifdef O_XIM
	if ( wsfr && wsfr->ic )
	  XSetICFocus(wsfr->ic);
#endif
	send(fr, NAME_inputFocus, ON, EAV);
      }
      return;
    }
    case FocusOut:
    {
#ifdef O_XIM
	if ( wsfr && wsfr->ic )
	  XUnsetICFocus(wsfr->ic);
#endif
      send(fr, NAME_inputFocus, OFF, EAV);
      return;
    }
    case KeyPress:
    { EventObj ev;
      FrameObj fr2;
      PceWindow sw;

      if ( !(fr2=blockedByModalFrame(fr)) )
	fr2 = fr;

      if ( (sw=getKeyboardFocusFrame(fr2)) &&
	   (ev=CtoEvent(sw, event)) )
      { addCodeReference(ev);
	postNamedEvent(ev, (Graphical) sw, DEFAULT, NAME_postEvent);
	delCodeReference(ev);
	freeableObj(ev);
      }

      return;
    }
    default:
    { EventObj ev;
      AnswerMark mark;
      markAnswerStack(mark);

      if ( (ev = CtoEvent(fr, event)) )
      { addCodeReference(ev);
	send(fr, NAME_event, ev, EAV);
	delCodeReference(ev);
	freeableObj(ev);
      }

      rewindAnswerStack(mark, NIL);

      return;
    }
  }
}


static void
xEventFrame(Widget w, FrameObj fr, XEvent *event)
{ pceMTLock(LOCK_PCE);
  ServiceMode(service_frame(fr),
	      x_event_frame(w, fr, event));
  pceMTUnlock(LOCK_PCE);
}


static void
expose_frame(Widget w, FrameObj fr, Region region)
{ XRectangle rect;

  pceMTLock(LOCK_PCE);
  XClipBox(region, &rect);
  DEBUG(NAME_frame, Cprintf("expose_frame(%s, %d,%d,%d,%d)\n",
			    pp(fr), rect.x, rect.y, rect.width, rect.height));

  ServiceMode(service_frame(fr),
	      { Area a;

		a = tempObject(ClassArea, toInt(rect.x), toInt(rect.y),
			       toInt(rect.width), toInt(rect.height), EAV);
		redrawFrame(fr, a);
		considerPreserveObject(a);
	      });
  pceMTUnlock(LOCK_PCE);
}


		 /*******************************
		 *	      CURSOR		*
		 *******************************/

void
ws_frame_cursor(FrameObj fr, CursorObj cursor)
{ Widget w = widgetFrame(fr);

  if ( w )
  { DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;

    XDefineCursor(d,
		  XtWindow(w),
		  !instanceOfObject(cursor, ClassCursor)
		      ? None
		      : (Cursor) getXrefObject(cursor, fr->display));
  }
}


void
ws_grab_frame_pointer(FrameObj fr, BoolObj grab, CursorObj cursor)
{ Widget w = widgetFrame(fr);

  if ( w )
  { if ( grab == ON )
    { Cursor c = (!instanceOfObject(cursor, ClassCursor)
		      ? None
		      : (Cursor) getXrefObject(cursor, fr->display));

      XtGrabPointer(w,
		    False,
		    ButtonPressMask|ButtonReleaseMask|
		    EnterWindowMask|LeaveWindowMask|
		    PointerMotionMask|ButtonMotionMask,
		    GrabModeAsync, GrabModeAsync,
		    None,
		    c,
		    CurrentTime);
    } else
    { XtUngrabPointer(w, CurrentTime);
    }
  }
}


		 /*******************************
		 *     GEOMETRY MANAGEMENT	*
		 *******************************/

static status
updateAreaFrame(FrameObj fr, Int border)
{ Widget wdg;

  if ( (wdg = widgetFrame(fr)) )
  { DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;
    Window me, root, child;
    int x, y;
    unsigned int w, h, bw, depth;

    if ( (me = XtWindow(wdg)) )
    { Area a = fr->area;
      Int ow = a->w, oh = a->h;

      XGetGeometry(d, me, &root, &x, &y, &w, &h, &bw, &depth);
      XTranslateCoordinates(d, me, root, 0, 0, &x, &y, &child);

      assign(a, x, toInt(x));
      assign(a, y, toInt(y));
      assign(a, w, toInt(w));
      assign(a, h, toInt(h));
      if ( notDefault(border) )
	assign(fr, border, border);

      if ( a->w != ow || a->h != oh )
	send(fr, NAME_resize, EAV);
    }

    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Returns the window which  we  believe   is  the  window manager's window
encapsulating our window as well as the offset of the client-area of our
window relative to the real outside  of the window-manager's window. The
latter is used to correct the position if   we send ->geometry to a life
window.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static Window
getWMFrameFrame(FrameObj fr, int *dxp, int *dyp)
{ Widget wdg;
  Window w = 0;
  int dx = 0, dy = 0;

  if ( (wdg = widgetFrame(fr)) )
  { DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;

    w = XtWindow(wdg);
    if ( fr->kind != NAME_popup )
    { Window root, parent, *children;
      unsigned int nchildren;
      int m = 0;

      while( m++ < 5 )			/* avoid a loop */
      { if ( !XQueryTree(d, w, &root, &parent,
			 &children, &nchildren) )
	  break;
	XFree((char *) children);	/* declared char * ???? */

	if ( dxp || dyp )
	{ int x, y;
	  unsigned int width, h, bw, depth;

	  XGetGeometry(d, w, &root, &x, &y, &width, &h, &bw, &depth);

	  dx += bw;
	  dy += bw;

	  if ( parent != root )
	  { dx += x;
	    dy += y;
	  }

	  DEBUG(NAME_frame,
		Cprintf("w = %ld; root = %ld; parent = %ld; "
			"dx=%d; dy=%d; bw = %d\n",
			w, root, parent, dx, dy, bw));
	}

	if ( parent == root )
	  break;

	w = parent;
      }
    }
  }

  if ( dxp )
    *dxp = dx;
  if ( dyp )
    *dyp = dy;

  return w;
}


status
ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h)
{ Window win;

  if ( (win = getWMFrameFrame(fr, NULL, NULL)) )
  { DisplayWsXref r = fr->display->ws_ref;
    XWindowAttributes atts;
    int bw = isDefault(fr->border) ? 1 : valInt(fr->border);

    XGetWindowAttributes(r->display_xref, win, &atts);
    *x = atts.x - bw;
    *y = atts.y - bw;
    *w = atts.width + 2*bw;
    *h = atts.height + 2*bw;

    succeed;
  }

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ws_x_geometry_frame() updates the window position using an X geometry
request.

This is a mess, totally unclear when  we   have  to  add which border to
where. At the moment we do do-your-own  as the system version doesn't do
the monitor extension. Copied mostly from the version in msw/msframe.c.

Note  that  the  computation  of  the   size  of  the  decorated  window
(ws_frame_bb) works fine if the  window   is  displayed, but not before.
Therefore we re-do our work if the window is mapped.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MIN_VISIBLE 32			/* pixels that must be visible */
#define SWP_NOMOVE 0x1
#define SWP_NOSIZE 0x2

void
ws_x_geometry_frame(FrameObj fr, Name spec, Monitor mon)
{ Widget wdg = widgetFrame(fr);

  DEBUG(NAME_frame, Cprintf("ws_x_geometry_frame(%s, %s, %s)\n",
			    pp(fr), pp(spec), pp(mon)));

  if ( wdg )
  { char *e, *s = strName(spec);
    int x, y, w, h, w0, h0;
    int ew, eh;
    int dx, dy, dw, dh;
    int flags = 0;
    char signx[10], signy[10];
    int ok=0;
    Int X,Y,W,H;
    int offX, offY;

    if ( isDefault(mon) && (e=strchr(s, '@')) )
    { int n = atoi(e+1);

      if ( !(mon = getNth0Chain(fr->display->monitors, toInt(n))) )
	mon = (Monitor)DEFAULT;
    }

    if ( instanceOfObject(mon, ClassMonitor) )
    { Area a = (notNil(mon->work_area) ? mon->work_area : mon->area);

      dx = valInt(a->x);
      dy = valInt(a->y);
      dw = valInt(a->w);
      dh = valInt(a->h);
    } else
    { dx = dy = 0;
      dw = valInt(getWidthDisplay(fr->display));
      dh = valInt(getHeightDisplay(fr->display));
    }

    if ( !ws_frame_bb(fr, &x, &y, &w0, &h0) )
      return;
    w = w0;
    h = h0;
    ew = w - valInt(fr->area->w);	/* width/height of decorations */
    eh = h - valInt(fr->area->h);

    getWMFrameFrame(fr, &offX, &offY);

    switch(sscanf(s, "%dx%d%[+-]%d%[+-]%d", &w, &h, signx, &x, signy, &y))
    { case 2:
	/*w += ew; h += eh;*/
	flags |= SWP_NOMOVE;
	ok++;
	break;
      case 6:
	/*w += ew; h += eh;*/
	if ( signx[1] == '-' )
	  x = -x;
	if ( signy[1] == '-' )
	  y = -y;
	if ( signx[0] == '-' )
	  x = dw - x - w - offX;
	if ( signy[0] == '-' )
	  y = dh - y - h - eh;		/* why not offY */
	ok++;
	break;
      default:				/* [<Sign>]X<Sign>Y */
	if ( sscanf(s, "%[+-]%d%[+-]%d", signx, &x, signy, &y) != 4 )
	{ signx[0] = '+';
	  if ( sscanf(s, "%d%[+-]%d", &x, signy, &y) != 3 )
	    break;
	}

	DEBUG(NAME_frame,
	      Cprintf("signx = %s, x = %d, signy = %s,"
		      "y = %d, w0 = %d, h0 = %d\n",
		      signx, x, signy, y, w0, h0));

	flags |= SWP_NOSIZE;
	if ( signx[1] == '-' )
	  x = -x;
	if ( signy[1] == '-' )
	  y = -y;
	if ( signx[0] == '-' )
	  x = dw - x - w0 - offX;
	if ( signy[0] == '-' )
	  y = dh - y - h0 - eh;
	ok++;
	break;
    }

    if ( ok )
    { int mw = (w < MIN_VISIBLE ? MIN_VISIBLE : w);

      if ( y < 0 )			/* above the screen */
	y = 0;
      if ( y > dh-MIN_VISIBLE )		/* below the screen */
	y = dh - MIN_VISIBLE;
      if ( x+mw < MIN_VISIBLE )		/* left of the screen */
	x = MIN_VISIBLE-mw;
      if ( x > dw-MIN_VISIBLE )		/* right of the screen */
	x = dw - MIN_VISIBLE;
    }

    X = Y = W = H = (Int)DEFAULT;
    if ( !(flags & SWP_NOMOVE) )
    { X = toInt(x);
      Y = toInt(y);
    }
    if ( !(flags & SWP_NOSIZE) )
    { W = toInt(w);
      H = toInt(h);
    }

    send(fr, NAME_set, X, Y, W, H, mon, EAV);
  }
}


void
ws_geometry_frame(FrameObj fr, Int x, Int y, Int w, Int h, Monitor mon)
{ Widget wdg = widgetFrame(fr);

  if ( wdg )
  { XtWidgetGeometry in, out;
    DisplayWsXref r = fr->display->ws_ref;

    in.request_mode = 0;
    if ( notDefault(x) ) in.request_mode |= CWX;
    if ( notDefault(y) ) in.request_mode |= CWY;
    if ( notDefault(w) ) in.request_mode |= CWWidth;
    if ( notDefault(h) ) in.request_mode |= CWHeight;

    in.x      = valInt(fr->area->x);
    in.y      = valInt(fr->area->y);
    in.width  = valInt(fr->area->w);
    in.height = valInt(fr->area->h);

    if ( notDefault(mon) )
    { in.x += valInt(mon->area->x);
      in.y += valInt(mon->area->y);
    }

    DEBUG(NAME_frame, Cprintf("%s: doing widget geometry request\n", pp(fr)));
    XtMakeGeometryRequest(wdg, &in, &out);

    if ( fr->kind != NAME_popup )
    { XSizeHints *hints = XAllocSizeHints();
      FrameWsRef wsref  = fr->ws_ref;

      if ( notDefault(x) || notDefault(y) ) hints->flags |= USPosition;
      if ( notDefault(w) || notDefault(h) ) hints->flags |= USSize;

      hints->x      = valInt(fr->area->x);
      hints->y      = valInt(fr->area->y);
      hints->width  = valInt(fr->area->w);
      hints->height = valInt(fr->area->h);

      if ( wsref->win_gravity )
      {  hints->win_gravity = wsref->win_gravity;
	 hints->flags |= PWinGravity;
      }

      DEBUG(NAME_frame, Cprintf("%s: setting WM hints\n", pp(fr)));
      XSetWMNormalHints(r->display_xref, XtWindow(wdg), hints);
      DEBUG(NAME_frame, Cprintf("\tok\n"));
      XFree(hints);
    }
  }
}


void
ws_border_frame(FrameObj fr, int b)
{ Widget w = widgetFrame(fr);

  if ( w )
  { XtWidgetGeometry in, out;
    in.request_mode = CWBorderWidth;
    in.border_width = b;

    XtMakeGeometryRequest(w, &in, &out);
  }
}

		 /*******************************
		 *	     CURSOR		*
		 *******************************/

void
ws_busy_cursor_frame(FrameObj fr, CursorObj c)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( !w )
    return;

#define BlockAllMask (KeyPressMask | KeyReleaseMask | \
		      ButtonPressMask | ButtonReleaseMask | \
		      PointerMotionMask)

  if ( !busyWindowFrame(fr) && notNil(c) )
  { unsigned long valuemask = CWCursor;
    XSetWindowAttributes attributes;
    Size size = getSizeDisplay(fr->display);
    Window bw;

    if ( isDefault(c) )
    { if ( !(c = getClassVariableValueObject(fr, NAME_busyCursor)) )
	return;				/* TBD: default? */
      if ( isNil(c) )
	goto out;
    }
    attributes.cursor = (Cursor) getXrefObject(c, fr->display);

    bw = XCreateWindow(r->display_xref,
		       XtWindow(widgetFrame(fr)), 0, 0,
		       valInt(size->w), valInt(size->h),
		       (unsigned int) 0, 0, InputOnly,
		       CopyFromParent, valuemask, &attributes);

    if ( bw )
      setBusyWindowFrame(fr, bw);
    else
      errorPce(fr, NAME_failedToCreate, 0); /* TBD */
  } else if ( busyWindowFrame(fr) && instanceOfObject(c, ClassCursor) )
  { unsigned long valuemask = 0L;
    XSetWindowAttributes attributes;

    if ( notDefault(c) )
    { valuemask |= CWCursor;
      attributes.cursor = (Cursor) getXrefObject(c, fr->display);
    }

    XChangeWindowAttributes(r->display_xref,
			    busyWindowFrame(fr),
			    valuemask, &attributes);
  }

out:
  if ( notNil(c) )
    XMapRaised(r->display_xref, busyWindowFrame(fr));
  else if ( busyWindowFrame(fr) )
    XUnmapWindow(r->display_xref, busyWindowFrame(fr));
}

		 /*******************************
		 *	      COLOUR		*
		 *******************************/

void
ws_frame_background(FrameObj fr, Any c)
{ Widget w = widgetFrame(fr);

  if ( w )
  { Arg args[2];
    DisplayObj d = fr->display;
    int i=0;

    if ( instanceOfObject(c, ClassColour) )
    { XtSetArg(args[i], XtNbackground, getPixelColour(c, d));		i++;
      XtSetArg(args[i], XtNbackgroundPixmap, XtUnspecifiedPixmap);	i++;
    } else
    { Pixmap pm = (Pixmap) getXrefObject(c, d);

      XtSetArg(args[i], XtNbackgroundPixmap, pm);			i++;
    }

    XtSetValues(w, args, i);
  }
}


		 /*******************************
		 *	      ICONS		*
		 *******************************/

void
ws_set_icon_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);

  if ( w )
  { Arg args[3];
    int n=0;

    XtSetArg(args[n], XtNiconPixmap,
	     getXrefObject(fr->icon_image, fr->display));
    n++;
    if ( notNil(fr->icon_image->mask) )
    { XtSetArg(args[n], XtNiconMask,
	       getXrefObject(fr->icon_image->mask, fr->display));
      n++;
    }
    XtSetArg(args[n], XtNiconName,
	     nameToMB(getIconLabelFrame(fr)));
    n++;

    XtSetValues(w, args, n);
  }
}


void
ws_set_icon_label_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);

  if ( w )
  { Arg args[1];

    XtSetArg(args[0], XtNiconName,
	     nameToMB(getIconLabelFrame(fr)));

    XtSetValues(w, args, 1);
  }
}


void
ws_set_icon_position_frame(FrameObj fr, int x, int y)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w )
  { XWMHints hints;

    hints.flags = IconPositionHint;
    hints.icon_x = x;
    hints.icon_y = y;

    XSetWMHints(r->display_xref, XtWindow(w), &hints);
  }
}


status
ws_get_icon_position_frame(FrameObj fr, int *x, int *y)
{ Widget w = widgetFrame(fr);
  DisplayWsXref r = fr->display->ws_ref;

  if ( w )
  { XWMHints *hints = XGetWMHints(r->display_xref, XtWindow(w));

    if ( hints )
    { *x = hints->icon_x;
      *y = hints->icon_y;
      XFree((void *)hints);

      succeed;
    }
  }

  fail;
}


int
ws_enable_frame(FrameObj fr, int val)
{ Widget w;

  if ( (w = widgetFrame(fr)) )
  { Arg args[1];

    XtSetArg(args[0], XtNinput, val ? True : False);
    XtSetValues(w, args, 1);

    succeed;
  }

  fail;
}


void
ws_enable_modal(FrameObj fr, BoolObj val)
{ if ( fr->modal == NAME_transient && notNil(fr->transient_for) )
  { ws_enable_frame(fr->transient_for, val == ON ? TRUE : FALSE);
  } else if ( fr->modal == NAME_application && notNil(fr->application) )
  { Cell cell;

    for_cell(cell, fr->application->members)
      ws_enable_frame(cell->value, val == ON ? TRUE : FALSE);
  }
}


#define GNOME_HINTS 1
#ifdef GNOME_HINTS

typedef int CARD32;
#define XA_WIN_STATE "_WIN_STATE"

#define WIN_STATE_STICKY          (1<<0) /*everyone knows sticky*/
#define WIN_STATE_MINIMIZED       (1<<1) /*Reserved - definition is unclear*/
#define WIN_STATE_MAXIMIZED_VERT  (1<<2) /*window in maximized V state*/
#define WIN_STATE_MAXIMIZED_HORIZ (1<<3) /*window in maximized H state*/
#define WIN_STATE_HIDDEN          (1<<4) /*not on taskbar but window visible*/
#define WIN_STATE_SHADED          (1<<5) /*shaded (MacOS / Afterstep style)*/
#define WIN_STATE_HID_WORKSPACE   (1<<6) /*not on current desktop*/
#define WIN_STATE_HID_TRANSIENT   (1<<7) /*owner of transient is hidden*/
#define WIN_STATE_FIXED_POSITION  (1<<8) /*window is fixed in position even*/
#define WIN_STATE_ARRANGE_IGNORE  (1<<9) /*ignore for auto arranging*/

#endif

void
ws_status_frame(FrameObj fr, Name status)
{ Widget w = widgetFrame(fr);

  if ( status == NAME_window || status == NAME_fullScreen )
  { if ( w )
    { Arg args[1];
      XtSetArg(args[0], XtNiconic, False);
      XtSetValues(w, args, 1);

#ifdef GNOME_HINTS
      /* GNOME: http://developer.gnome.org/doc/standards/wm/x62.html */
      if ( status == NAME_fullScreen )
      { XClientMessageEvent xev;
	CARD32 change = WIN_STATE_MAXIMIZED_VERT|WIN_STATE_MAXIMIZED_HORIZ;
	CARD32 bits = (status == NAME_fullScreen ? change : 0);
	DisplayWsXref r = (DisplayWsXref)fr->display->ws_ref;
	XWindowAttributes atts;

	XGetWindowAttributes(r->display_xref, XtWindow(r->shell_xref), &atts);

	xev.type   = ClientMessage;
	xev.window = XtWindow(w);
	xev.message_type = XInternAtom(r->display_xref, XA_WIN_STATE, False);
	xev.format = 32;
	xev.data.l[0] = change;
	xev.data.l[1] = bits;
	XSendEvent(r->display_xref, atts.root,
		   False, SubstructureNotifyMask, (XEvent *)&xev);
      }
#endif

      XtPopup(w, XtGrabNone);
    }
    ws_enable_modal(fr, OFF);
  } else
  { if ( status == NAME_iconic )
    { if ( w )
      { Arg args[1];
	XtSetArg(args[0], XtNiconic, True);
	XtSetValues(w, args, 1);
      }
    } else if ( status == NAME_hidden )
    { if ( w )
	XtPopdown(w);
    }
    ws_enable_modal(fr, ON);
  }
}


void
ws_topmost_frame(FrameObj fr, BoolObj topmost)
{
}

		 /*******************************
		 *	       LABEL		*
		 *******************************/

void
ws_set_label_frame(FrameObj fr)
{ Widget w = widgetFrame(fr);

  if ( w )
  { Arg args[1];

    XtSetArg(args[0], XtNtitle, nameToMB(fr->label));
    XtSetValues(w, args, 1);
  }
}



		 /*******************************
		 *	   MISCELLANEOUS	*
		 *******************************/

Image
ws_image_of_frame(FrameObj fr)
{ Window win;

  if ( (win = getWMFrameFrame(fr, NULL, NULL)) )
  { Window root, child;
    DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;
    int x, y;
    unsigned int w, h, bw, depth;
    Image im;
    XImage *ix;

    XGetGeometry(d, win, &root, &x, &y, &w, &h, &bw, &depth);
    XTranslateCoordinates(d, win, root, 0, 0, &x, &y, &child);
    if ( notDefault(fr->border) )
      bw = valInt(fr->border);

    TRY(im = answerObject(ClassImage, NIL,
			  toInt(w+2*bw), toInt(h+2*bw), NAME_pixmap, EAV));

    ix = XGetImage(d, root,
		   x-bw, y-bw, w+2*bw, h+2*bw, AllPlanes, ZPixmap);
    setXImageImage(im, ix);
    assign(im, depth, toInt(ix->depth));
    answer(im);
  }

  fail;
}


void
ws_transient_frame(FrameObj fr, FrameObj fr2)
{ Widget w1 = widgetFrame(fr);
  Widget w2 = widgetFrame(fr2);


  if ( w1 && w2 )
  { DisplayWsXref r = fr->display->ws_ref;

    XSetTransientForHint(r->display_xref,
			 XtWindow(w1),
			 XtWindow(w2));
  }
}

static int
psdepthXImage(XImage *im)
{ if ( im->depth < 3 )			/* 1, 2 */
    return im->depth;
  if ( im->depth < 8 )
    return 4;
  return 8;
}



status
ws_postscript_frame(FrameObj fr, int iscolor)
{ Window win;

  if ( (win = getWMFrameFrame(fr, NULL, NULL)) )
  { Window root, child;
    DisplayWsXref r = fr->display->ws_ref;
    Display *d = r->display_xref;
    int x, y;
    unsigned int w, h, bw, depth;
    XImage *im;
    int iw, ih;
    XWindowAttributes atts;

    XGetGeometry(d, win, &root, &x, &y, &w, &h, &bw, &depth);
    XTranslateCoordinates(d, win, root, 0, 0, &x, &y, &child);
    XGetWindowAttributes(d, root, &atts);

    if ( notDefault(fr->border) )
      bw = valInt(fr->border);

    iw = w+2*bw; ih = h+2*bw;		/* include the frame-border */
    x -= bw; y -= bw;
    if ( x < 0 ) { iw += x; x = 0; }	/* clip to the display */
    if ( y < 0 ) { ih += y; y = 0; }
    if ( x + iw > atts.width )  iw = atts.width - x;
    if ( y + ih > atts.height ) ih = atts.height - y;

    DEBUG(NAME_postscript, Cprintf("frame at %d %d %d %d\n", x, y, iw, ih));

    im = XGetImage(d, root, x, y, iw, ih, AllPlanes, ZPixmap);

    ps_output("0 0 ~D ~D ~D ~N\n", iw, ih,
	      psdepthXImage(im),
	      iscolor ? NAME_rgbimage : NAME_greymap);
    postscriptXImage(im, NULL, 0, 0, iw, ih,
		     r->display_xref, r->colour_map, 0, iscolor);
    ps_output("\n");

    XDestroyImage(im);
    succeed;
  }

  return errorPce(fr, NAME_mustBeOpenBeforePostscript);
}


Int
ws_frame_thread(FrameObj fr)
{ fail;
}
