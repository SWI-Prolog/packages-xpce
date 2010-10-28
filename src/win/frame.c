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

TileObj		getTileFrame(FrameObj);
forwards int	get_position_from_center_frame(FrameObj, Monitor, Point, int *, int *);
static void	ensure_on_display(FrameObj, Monitor, int *, int *);
static status	closedFrame(FrameObj, BoolObj);
static status	openFrame(FrameObj fr, Point pos, BoolObj grab, BoolObj normalise);
static status	doneMessageFrame(FrameObj fr, Code msg);
static status	geometryFrame(FrameObj fr, Name spec, Monitor mon);
static status	setFrame(FrameObj fr, Int x, Int y, Int w, Int h, Monitor mon);
static status	flushFrame(FrameObj fr);
static status	kindFrame(FrameObj fr, Name kind);
static status	informTransientsFramev(FrameObj fr, Name selector,
				       int argc, Any *argv);
static status	grabPointerFrame(FrameObj fr, BoolObj grab, CursorObj cursor);
static status	cursorFrame(FrameObj fr, CursorObj cursor);
static status   statusFrame(FrameObj fr, Name stat);

#define isOpenFrameStatus(s) ((s) == NAME_window || (s) == NAME_fullScreen)

static status
initialiseFrame(FrameObj fr, Name label, Name kind,
		DisplayObj display,
		Application app)
{ if ( isDefault(kind) )
    kind = NAME_toplevel;
  if ( isDefault(display) )
    display = CurrentDisplay(NIL);
  if ( isDefault(label) )
    label = CtoName("Untitled");
  if ( isDefault(app) )
    app = NIL;

  assign(fr, name,	    	    getClassNameObject(fr));
  assign(fr, label,         	    label);
  assign(fr, display,       	    display);
  assign(fr, border,		    DEFAULT);
  assign(fr, area,	    	    newObject(ClassArea, EAV));
  assign(fr, members,	    	    newObject(ClassChain, EAV));
  assign(fr, kind,	    	    kind);
  assign(fr, status,	    	    NAME_unmapped);
  assign(fr, can_delete,    	    ON);
  assign(fr, input_focus,   	    OFF);
  assign(fr, sensitive,   	    ON);
  assign(fr, fitting,		    OFF);
  assign(fr, wm_protocols,  	    newObject(ClassSheet, EAV));
  assign(fr, wm_protocols_attached, OFF);
  obtainClassVariablesObject(fr);

  doneMessageFrame(fr, newObject(ClassMessage, RECEIVER, NAME_wmDelete, EAV));

  fr->ws_ref = NULL;			/* Window System Reference */

  if ( notNil(app) )
    send(app, NAME_append, fr, EAV);

  succeed;
}


static status
unlinkFrame(FrameObj fr)
{ if ( fr->status != NAME_unlinking )
  { FrameObj sfr;
    PceWindow sw;
    Cell cell;

    assign(fr, status, NAME_unlinking);

    for_cell(cell, fr->members)		/* suppress any updates */
    { PceWindow sw = cell->value;

      assign(sw, displayed, OFF);
    }

    ws_enable_modal(fr, ON);
    if ( notNil(fr->transients) )
      for_chain(fr->transients, sfr, send(sfr, NAME_destroy, EAV));
    if ( notNil(fr->transient_for) && notNil(fr->transient_for->transients) )
      send(fr->transient_for, NAME_detachTransient, fr, EAV);

    ws_uncreate_frame(fr);
    deleteChain(fr->display->frames, fr);
    if ( notNil(fr->application) )
      send(fr->application, NAME_delete, fr, EAV);

    for_chain(fr->members, sw, freeObject(sw));

    unlinkedWindowEvent(fr);
  }

  succeed;
}


static FrameObj
getConvertFrame(Class class, PceWindow sw)
{ answer(getFrameWindow(sw, DEFAULT));
}

		 /*******************************
		 *	     SAVE-LOAD		*
		 *******************************/

static status
storeFrame(FrameObj fr, FileObj file)
{ return storeSlotsObject(fr, file);
}


static status
loadFrame(FrameObj fr, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(fr, fd, def));
  assign(fr, wm_protocols_attached, OFF);
  assign(fr, input_focus, OFF);

  if ( isOpenFrameStatus(fr->status) )
  { assign(fr, status, NAME_unmapped);
    restoreMessage(newObject(ClassMessage, fr, NAME_open,
			     get(fr->area, NAME_position, EAV), EAV));
  }

  succeed;
}


static status
convertOldSlotFrame(FrameObj fr, Name var, Any value)
{ if ( var == NAME_show )
    assign(fr, status, value == ON ? NAME_open : NAME_hidden);

  succeed;
}


static status
initialiseNewSlotFrame(FrameObj fr, Variable var)
{ if ( var->name == NAME_background )
    assign(fr, background, getClassVariableValueObject(fr, NAME_background));

  succeed;
}

		/********************************
		*          OPEN/CREATE		*
		********************************/

static Constant ConstantNotReturned;

Any
getConfirmFrame(FrameObj fr, Point pos, BoolObj grab, BoolObj normalise)
{ Any rval;

  TRY( openFrame(fr, pos, grab, normalise) );
  busyCursorDisplay(fr->display, NIL, DEFAULT);

  assign(fr, return_value, ConstantNotReturned);
  synchroniseDisplay(fr->display);
  while( offFlag(fr, F_FREED|F_FREEING) &&
	 fr->return_value == ConstantNotReturned )
  { dispatchDisplay(fr->display);
    ws_discard_input("Confirmer running");
  }

  if ( onFlag(fr, F_FREED|F_FREEING) )
    fail;

  rval = fr->return_value;
  if ( isObject(rval) )
  { addCodeReference(rval);
    assign(fr, return_value, ConstantNotReturned);
    delCodeReference(rval);
    pushAnswerObject(rval);
  } else
    assign(fr, return_value, ConstantNotReturned);

  answer(rval);
}


Any
getConfirmCenteredFrame(FrameObj fr, Point pos, BoolObj grab, Monitor mon)
{ int x, y;
  Point p2;
  Any rval;

  TRY( send(fr, NAME_create, EAV) );

  get_position_from_center_frame(fr, mon, pos, &x, &y);
  ensure_on_display(fr, mon, &x, &y);
  p2 = tempObject(ClassPoint, toInt(x), toInt(y), EAV);

  rval = getConfirmFrame(fr, p2, grab, OFF);
  considerPreserveObject(p2);
  return rval;
}


static status
returnFrame(FrameObj fr, Any obj)
{ assign(fr, return_value, obj);

  succeed;
}


static status
openFrame(FrameObj fr, Point pos, BoolObj grab, BoolObj normalise)
{ Int x, y;
  Int w = DEFAULT, h = DEFAULT;

  if ( !createdFrame(fr) )
    TRY( send(fr, NAME_create, EAV) );

  if ( isDefault(pos) && isOpenFrameStatus(fr->status) )
    succeed;

  if ( notDefault(pos) )		/* X11 transient is done by WM */
  { x = pos->x;
    y = pos->y;

#ifdef __WINDOWS__
  setpos:
#endif
    if ( normalise == ON )
    { int mx, my, mw, mh;
      int fw = valInt(fr->area->w), fh = valInt(fr->area->h);
      Area a;
      Area tmp = tempObject(ClassArea,
			    toInt(x), toInt(y), fr->area->w, fr->area->h, EAV);
      Monitor mon = getMonitorDisplay(fr->display, tmp);

      considerPreserveObject(tmp);

      if ( !mon )
	mon = getMonitorDisplay(fr->display, DEFAULT);
      if ( !mon )
	mon = getHeadChain(fr->display->monitors);
      a = isNil(mon->work_area) ? mon->area : mon->work_area;

      mx = valInt(a->x);
      my = valInt(a->y);
      mw = valInt(a->w);
      mh = valInt(a->h);

      if ( valInt(x) + fw > mx+mw ) x = toInt(mx+mw - fw);
      if ( valInt(y) + fh > my+mh ) y = toInt(my+mh - fh);
      if ( valInt(x) < mx ) x = toInt(mx);
      if ( valInt(y) < my ) y = toInt(my);
    }

    setFrame(fr, x, y, w, h, DEFAULT);
  } else if ( notNil(fr->geometry) )
  { ws_x_geometry_frame(fr, fr->geometry, DEFAULT);
  }
#ifdef __WINDOWS__				/* But in Windows `do-it-yourself' */
  else if ( notNil(fr->transient_for) )
  { Area pa = fr->transient_for->area;
    int xb, yb, ycap;

    ws_frame_border(fr, &xb, &yb, &ycap);

    x = toInt(valInt(pa->x) + xb*2);
    y = toInt(valInt(pa->y) + yb*2 + ycap);
    if ( isDefault(normalise) )
      normalise = ON;
    goto setpos;
  }
#endif

  if ( !isOpenFrameStatus(fr->status) )
    statusFrame(fr, NAME_window);

  succeed;
}


static status
openCenteredFrame(FrameObj fr, Point pos, BoolObj grab, Monitor mon)
{ int x, y;
  int rval;
  Point p2;

  TRY( send(fr, NAME_create, EAV) );

  get_position_from_center_frame(fr, mon, pos, &x, &y);
  ensure_on_display(fr, DEFAULT, &x, &y);
  p2 = answerObject(ClassPoint, toInt(x), toInt(y), EAV);
  rval = openFrame(fr, p2, grab, OFF);
  doneObject(p2);

  return rval;
}


static status
resizeFrame(FrameObj fr)
{ Area a = fr->area;
  TileObj t = getTileFrame(fr);

  if ( t )
    send(t, NAME_layout, ZERO, ZERO, a->w, a->h, EAV);

  succeed;
}


		/********************************
		*         WM_PROTOCOLS		*
		********************************/

static status
attachWmProtocolsFrame(FrameObj fr)
{ ws_attach_wm_prototols_frame(fr);

  succeed;
}


static status
wmProtocolFrame(FrameObj fr, Name name, Code msg)
{ valueSheet(fr->wm_protocols, name, msg);
  if ( fr->wm_protocols_attached == ON )
    attachWmProtocolsFrame(fr);

  succeed;
}


static status
deleteWmProtocolFrame(FrameObj fr, Name name)
{ if ( isAttributeSheet(fr->wm_protocols, name) == SUCCEED )
  { deleteSheet(fr->wm_protocols, name);
    if ( fr->wm_protocols_attached == ON )
      attachWmProtocolsFrame(fr);
  }

  succeed;
}


static status
wmDeleteFrame(FrameObj fr)
{ if ( fr->can_delete == OFF )
    fail;

  if ( fr->confirm_done == ON )
  { TRY(send(fr->display, NAME_confirm,
	     CtoName("Delete window ``%s''"), fr->label, EAV));
  }

  return send(fr, NAME_destroy, EAV);
}


static status
doneMessageFrame(FrameObj fr, Code msg)
{ return wmProtocolFrame(fr, CtoName("WM_DELETE_WINDOW"), msg);
}


static status
saveMessageFrame(FrameObj fr, Code msg)
{ return wmProtocolFrame(fr, CtoName("WM_SAVE_YOURSELF"), msg);
}


static status
mappedFrame(FrameObj fr, BoolObj val)
{ Any stat = (val == ON ? NAME_window : NAME_hidden);
  informTransientsFramev(fr, NAME_status, 1, &stat);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
To create a frame:

  1) Give all children the change to request a size.
  2) Fit the the windows in the frame and give them the size they should
     do with.
  3) Create the shell widget
  4) Create the children
  5) Manage the children
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
createdFrame(FrameObj fr)
{ return ws_created_frame(fr);
}


static status
createFrame(FrameObj fr)
{ Cell cell;

  if ( createdFrame(fr) )
    succeed;

  obtainClassVariablesObject(fr);
  TRY(openDisplay(fr->display));
  appendChain(fr->display->frames, fr);

  TRY(send(fr, NAME_fit, EAV));

  ws_create_frame(fr);

  for_cell(cell, fr->members)
    send(cell->value, NAME_create, EAV);

  ws_realise_frame(fr);
  assign(fr, status, NAME_hidden);

  attachWmProtocolsFrame(fr);

  if ( isName(fr->geometry) )
    geometryFrame(fr, fr->geometry, DEFAULT);

  for_cell(cell, fr->members)
  { updateCursorWindow(cell->value);
    qadSendv(cell->value, NAME_resize, 0, NULL);
  }

  send(fr, NAME_updateTileAdjusters, EAV);

  succeed;
}


static status
uncreateFrame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
    send(cell->value, NAME_uncreate, EAV);

  ws_uncreate_frame(fr);
  succeed;
}


static status
fitFrame(FrameObj fr)
{ TileObj t;
  Cell cell;
  Int border;

  if ( fr->fitting == ON ||
       !(t = getTileFrame(fr)) )
  { setFrame(fr, DEFAULT, DEFAULT,
	     toInt(100), toInt(100), DEFAULT);
    succeed;
  }

  assign(fr, fitting, ON);
  enforceTile(t, OFF);

  for_cell(cell, fr->members)
    send(cell->value, NAME_ComputeDesiredSize, EAV);

  enforceTile(t, ON);
  border = mul(t->border, TWO);

  assign(fr->area, w, ZERO);		/* ensure ->resize */

  setFrame(fr, DEFAULT, DEFAULT,
	   add(t->idealWidth, border),
	   add(t->idealHeight, border), DEFAULT);
  assign(fr, fitting, OFF);

  succeed;
}


static status
statusFrame(FrameObj fr, Name stat)
{ if ( stat != NAME_unmapped && !createdFrame(fr) )
    TRY(send(fr, NAME_create, EAV));

  if ( stat == NAME_open )
    stat = NAME_window;

  if ( fr->status != stat )
  { int opened = (isOpenFrameStatus(stat) && !isOpenFrameStatus(fr->status));

    ws_status_frame(fr, stat);
    assign(fr, status, stat);

    if ( opened )
    { resizeFrame(fr);
      flushFrame(fr);
    }
  }

  succeed;
}


static status
frame_is_upto_date(FrameObj fr)
{ Cell cell;

  if ( fr->status == NAME_hidden )
    fail;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;

    if ( ChangedWindows && memberChain(ChangedWindows, sw) )
      fail;
  }

  succeed;
}


static status
waitFrame(FrameObj fr)
{ if ( fr->status == NAME_unmapped )
    TRY(send(fr, NAME_open, EAV));

  while( !frame_is_upto_date(fr) )
  { if ( dispatchDisplay(fr->display) )
      ws_discard_input("Waiting for frame to open");
  }

  if ( isOpenFrameStatus(fr->status) )
    succeed;

  fail;					/* error? */
}


static status
showFrame(FrameObj fr, BoolObj val)
{ if ( val == ON )
  { if ( isOpenFrameStatus(fr->status) )
      succeed;
    else
      return statusFrame(fr, NAME_window);
  } else
    return statusFrame(fr, NAME_hidden);
}


static BoolObj
getShowFrame(FrameObj fr)
{ answer(isOpenFrameStatus(fr->status) ? ON : OFF);
}


		/********************************
		*           HIDE/EXPOSE		*
		********************************/

status
exposeFrame(FrameObj fr)
{ showFrame(fr, ON);
  ws_raise_frame(fr);

  succeed;
}


status
hideFrame(FrameObj fr)
{ ws_lower_frame(fr);

  succeed;
}


static status
exposedFrame(FrameObj fr)
{ moveAfterChain(fr->display->frames, fr, DEFAULT);
  informTransientsFramev(fr, NAME_expose, 0, NULL);

  succeed;
}


static status
hiddenFrame(FrameObj fr)
{ moveAfterChain(fr->display->frames, fr, getTailChain(fr->display->frames));
  informTransientsFramev(fr, NAME_hide, 0, NULL);

  succeed;
}

		/********************************
		*       AREA MANAGEMENENT	*
		********************************/

static Monitor
CurrentMonitor(FrameObj fr)
{ DisplayObj d = fr->display;
  Monitor mon;

  if ( isOpenFrameStatus(fr->status) )
  { if ( notNil(d) && (mon=getMonitorDisplay(d, fr->area)) )
      return mon;
  } else if ( notNil(d) && instanceOfObject(EVENT->value, ClassEvent) )
  { EventObj ev = EVENT->value;
    Point pt;

    if ( notNil(ev->window) && offFlag(ev->window, F_FREEING|F_FREED) )
    { pt = getPositionEvent(ev, d);
    } else
    { pt = getPointerLocationDisplay(d);
    }

    if ( pt && (mon=getMonitorDisplay(d, pt)) )
      return mon;
  }

  if ( isNil(d) )
    d = CurrentDisplay(fr);

  if ( notNil(d->monitors) )
    return getHeadChain(d->monitors);

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get XY coordinate of frame if  its  center   must  be  at pos. If Pos is
DEFAULT it is centered in the given   monitor. If the monitor is default
too, we deduce the most recent monitor from the event.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_position_from_center_frame(FrameObj fr, Monitor mon, Point pos,
			       int *x, int *y)
{ if ( isDefault(pos) )
  { if ( isDefault(mon) )
      mon = CurrentMonitor(fr);

    if ( mon )
    { *x = valInt(mon->area->x) + valInt(mon->area->w)/2;
      *y = valInt(mon->area->y) + valInt(mon->area->h)/2;
    } else
    { *x = *y = 0;
    }
  } else
  { *x = valInt(pos->x);
    *y = valInt(pos->y);
  }

  *x -= valInt(fr->area->w) / 2;
  *y -= valInt(fr->area->h) / 2;

  succeed;
}


static void
ensure_on_display(FrameObj fr, Monitor mon, int *x, int *y)
{ int rm, bm;

  if ( isDefault(mon) )
    mon = CurrentMonitor(fr);

  rm = valInt(mon->area->x) + valInt(mon->area->w);
  bm = valInt(mon->area->y) + valInt(mon->area->h);

  if ( *x + valInt(fr->area->w) > rm )
    *x -= *x + valInt(fr->area->w) - rm;
  if ( *y + valInt(fr->area->h) > bm )
    *y -= *y + valInt(fr->area->h) - bm;
  if ( *x < valInt(mon->area->x) )
    *x = valInt(mon->area->x);
  if ( *y < valInt(mon->area->y) )
    *y = valInt(mon->area->y);
}


static Size
getSizeFrame(FrameObj fr)
{ answer(getSizeArea(fr->area));
}


static Point
getPositionFrame(FrameObj fr)
{ answer(getPositionArea(fr->area));
}


static Area
getBoundingBoxFrame(FrameObj fr)
{ int x, y, w, h;

  if ( ws_frame_bb(fr, &x, &y, &w, &h) )
    answer(answerObject(ClassArea,
			toInt(x), toInt(y), toInt(w), toInt(h),
			EAV));

  fail;
}



static Monitor
getMonitorFrame(FrameObj fr)
{ if ( notNil(fr->display) )
    answer(getMonitorDisplay(fr->display, fr->area));

  fail;
}



static Name
getGeometryFrame(FrameObj fr)
{ int x, y, ww, wh;

  if ( ws_frame_bb(fr, &x, &y, &ww, &wh) ) 	/* outer area */
  { int mx, my, mw, mh;
    int xn=FALSE, yn=FALSE;
    char buf[100];
    Monitor mon;
    int cw, ch;

    cw = valInt(fr->area->w);			/* Client area */
    ch = valInt(fr->area->h);

    if ( (mon=getMonitorFrame(fr)) )
    { Area a = (isNil(mon->work_area) ? mon->area : mon->work_area);

      mx = valInt(a->x);
      my = valInt(a->y);
      mw = valInt(a->w);
      mh = valInt(a->h);

      DEBUG(NAME_geometry, Cprintf("%s on %s: %d %d %d %d\n", pp(fr), pp(mon),
				   mx, my, mw, mh));
    } else
    { Size size = getSizeDisplay(fr->display);

      mx = my = 0;
      mw = valInt(size->w);
      mh = valInt(size->h);
    }

    if ( x-mx > ((mx+mw) - (x+ww))*2 )	/* over 2/3th */
    { x = (mx+mw) - (x+ww);
      xn = TRUE;
    } else
    { x -= mx;
    }
    if ( y-my > ((my+mh) - (y+wh))*2 )
    { y = (my+mh) - (y+wh);
      yn = TRUE;
    } else
    { y -= my;
    }

    if ( fr->can_resize != OFF )
      sprintf(buf, "%dx%d", cw, ch);
    else
      buf[0] = EOS;

    sprintf(buf+strlen(buf),
	    "%s%d%s%d", xn ? "-" : "+", x, yn ? "-" : "+", y);

    if ( mon && fr->display->monitors->size != ONE )
    { Int n = getIndexChain(fr->display->monitors, mon);

      if ( n )
	sprintf(buf+strlen(buf), "@" INTPTR_FORMAT, valInt(n)-1);
    }

    answer(CtoName(buf));
  }

  fail;
}


static Image
getImageFrame(FrameObj fr)
{ if ( createdFrame(fr) )
    answer(ws_image_of_frame(fr));

  errorPce(fr, NAME_mustBeCreatedBefore, NAME_image);
  fail;
}


static status
geometryFrame(FrameObj fr, Name spec, Monitor mon)
{ assign(fr, geometry, spec);

  ws_x_geometry_frame(fr, spec, mon);

  succeed;
}


static status
setFrame(FrameObj fr, Int x, Int y, Int w, Int h, Monitor mon)
{ Area a = fr->area;
  Int ow = a->w;
  Int oh = a->h;

  if ( notDefault(mon) )
  { if ( notDefault(x) )
      x = add(x, mon->area->x);
    if ( notDefault(y) )
      y = add(y, mon->area->y);

    mon = DEFAULT;
  }

  setArea(a, x, y, w, h);
  if ( valInt(a->w) <= 0 )		/* Window systems don't like that */
    assign(a, w, ONE);
  if ( valInt(a->h) <= 0 )
    assign(a, h, ONE);

  if ( createdFrame(fr) )
  { ws_geometry_frame(fr, x, y, w, h, mon);

    if ( ow != a->w || oh != a->h )
      resizeFrame(fr);
  }

  succeed;
}


static status
xFrame(FrameObj fr, Int x)
{ return setFrame(fr, x, DEFAULT, DEFAULT, DEFAULT, DEFAULT);
}


static status
yFrame(FrameObj fr, Int y)
{ return setFrame(fr, DEFAULT, y, DEFAULT, DEFAULT, DEFAULT);
}


static status
widthFrame(FrameObj fr, Int w)
{ return setFrame(fr, DEFAULT, DEFAULT, w, DEFAULT, DEFAULT);
}


static status
heightFrame(FrameObj fr, Int h)
{ return setFrame(fr, DEFAULT, DEFAULT, DEFAULT, h, DEFAULT);
}


static status
sizeFrame(FrameObj fr, Size sz)
{ return setFrame(fr, DEFAULT, DEFAULT, sz->w, sz->h, DEFAULT);
}


static status
positionFrame(FrameObj fr, Point pos)
{ return setFrame(fr, pos->x, pos->y, DEFAULT, DEFAULT, DEFAULT);
}


static status
centerFrame(FrameObj fr, Point pos, Monitor mon)
{ int x, y;

  get_position_from_center_frame(fr, mon, pos, &x, &y);
  return setFrame(fr, toInt(x), toInt(y), DEFAULT, DEFAULT, DEFAULT);
}


static status
areaFrame(FrameObj fr, Area area)
{ return setFrame(fr, area->x, area->y, area->w, area->h, DEFAULT);
}


static status
showLabelFrame(FrameObj fr, BoolObj val)
{ return kindFrame(fr, val == ON ? NAME_toplevel : NAME_transient);
}


static status
borderFrame(FrameObj fr, Int width)
{ if ( fr->border != width )
  { assign(fr, border, width);

    if ( ws_created_frame(fr) )
      ws_border_frame(fr, valInt(width));
  }

  succeed;
}


static status
backgroundFrame(FrameObj fr, Any bg)
{ if ( fr->background != bg )
  { assign(fr, background, bg);

    if ( ws_created_frame(fr) )
      ws_frame_background(fr, bg);
  }

  succeed;
}


static void
forwardColourMapChange(Device d)
{ Cell cell;

  if ( instanceOfObject(d, ClassWindow) )
    redrawWindow((PceWindow)d, DEFAULT);

  for_cell(cell, d->graphicals)
  { if ( instanceOfObject(cell->value, ClassDevice) )
      forwardColourMapChange(cell->value);
  }
}


status
forwardColourMapChangeFrame(FrameObj fr)
{ if ( !(isFreedObj(fr) || isFreeingObj(fr)) )
  { Cell cell;

    for_cell(cell, fr->members)
    { forwardColourMapChange(cell->value);
    }
  }

  succeed;
}


static status
colourMapFrame(FrameObj fr, ColourMap cm)
{ assign(fr, colour_map, cm);

  return forwardColourMapChangeFrame(fr);
}


		 /*******************************
		 *	     CURSORS		*
		 *******************************/

status
busyCursorFrame(FrameObj fr, CursorObj c, BoolObj block_events)
{ if ( createdFrame(fr) )
    ws_busy_cursor_frame(fr, c);

  succeed;
}


static status
resetFrame(FrameObj fr)
{ busyCursorFrame(fr, NIL, DEFAULT);
  assign(fr, fitting, OFF);

  return resetVisual((VisualObj) fr);
}



		/********************************
		*             ICONS		*
		********************************/

static status
iconFrame(FrameObj fr, Image image, Name label)
{ assign(fr, icon_image, image);
  if ( notDefault(label) )
    assign(fr, icon_label, label);
  ws_set_icon_frame(fr);

  succeed;
}


Name
getIconLabelFrame(FrameObj fr)
{ answer(notNil(fr->icon_label) ? fr->icon_label : fr->label);
}


static status
iconLabelFrame(FrameObj fr, Name label)
{ assign(fr, icon_label, label);
  ws_set_icon_label_frame(fr);

  succeed;
}


static status
iconPositionFrame(FrameObj fr, Point pos)
{ assign(fr, icon_position, pos);

  if ( notNil(pos) )
    ws_set_icon_position_frame(fr, valInt(pos->x), valInt(pos->y));

  succeed;
}


static Point
getIconPositionFrame(FrameObj fr)
{ int x, y;

  if ( ws_get_icon_position_frame(fr, &x, &y) )
    answerObject(ClassPoint, toInt(x), toInt(y));

  answer(fr->icon_position);
}


static status
closedFrame(FrameObj fr, BoolObj val)
{ if ( val == ON )
  { if ( isOpenFrameStatus(fr->status) )
      succeed;
    else
      return statusFrame(fr, NAME_window);
  } else
    return statusFrame(fr, NAME_iconic);
}


static BoolObj
getClosedFrame(FrameObj fr)
{ answer(fr->status == NAME_iconic ? ON : OFF);
}


		/********************************
		*          MISCELENEOUS		*
		********************************/

static status
flushFrame(FrameObj fr)
{ return flushDisplay(fr->display);
}


static status
synchroniseFrame(FrameObj fr)
{ return synchroniseDisplay(fr->display);
}


static status
bellFrame(FrameObj fr, Int volume)
{ return bellDisplay(fr->display, volume);
}


TileObj
getTileFrame(FrameObj fr)
{ if ( notNil(fr->members->head) )
  { PceWindow sw = getHeadChain(fr->members);

    return getRootTile(sw->tile);
  }

  fail;
}


static status
labelFrame(FrameObj fr, Name label, Name icon)
{ assign(fr, label, label);

  ws_set_label_frame(fr);

  if ( notDefault(icon) )
    iconLabelFrame(fr, icon);

  succeed;
}


static status
appendFrame(FrameObj fr, PceWindow sw)
{ return frameWindow(sw, fr);
}


static Chain
getMembersFrame(FrameObj fr)
{ Chain rval = answerObject(ClassChain, EAV);
  Cell cell;

  for_cell(cell, fr->members)
  { if ( instanceOfObject(cell->value, ClassWindowDecorator) )
    { WindowDecorator wd = cell->value;

      appendChain(rval, wd->window);
    } else
      appendChain(rval, cell->value);
  }

  answer(rval);
}


status
AppendFrame(FrameObj fr, PceWindow sw)
{ appendChain(fr->members, sw);

  if ( createdFrame(fr) )
  { TRY(send(sw, NAME_create, EAV));

    ws_manage_window(sw);

    if ( getClassVariableValueObject(fr, NAME_fitAfterAppend) == ON )
      send(fr, NAME_fit, EAV);
    else
      send(fr, NAME_resize, EAV);

    if ( isOpenFrameStatus(fr->status) )
      send(sw, NAME_displayed, ON, EAV);
  }

  succeed;
}


status
DeleteFrame(FrameObj fr, PceWindow sw)
{ if ( instanceOfObject(sw->device, ClassWindowDecorator) )
    return DeleteFrame(fr, (PceWindow) sw->device);

  if ( sw->frame != fr )
    return errorPce(fr, NAME_noMember, sw);

  addCodeReference(fr);
  deleteChain(fr->members, sw);
  assign(sw, frame, NIL);		/* may kill the frame */

  if ( !isFreedObj(fr) && createdFrame(fr) )
  { ws_unmanage_window(sw);
    send(sw, NAME_uncreate, EAV);
    unrelateTile(sw->tile);
    if ( getClassVariableValueObject(fr, NAME_fitAfterAppend) == ON )
      send(fr, NAME_fit, EAV);
    else
      send(fr, NAME_resize, EAV);
  }
  delCodeReference(fr);

  succeed;
}


static status
deleteFrame(FrameObj fr, PceWindow sw)
{ /*if ( valInt(fr->members->size) <= 1 )
    fail;*/				/* cannot delete last (yet) */

  return DeleteFrame(fr, sw);
}


static PceWindow
getMemberFrame(FrameObj fr, Name name)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow w;

    if ( (w=getUserWindow(cell->value))->name == name )
      answer(w);
  }

  fail;
}


static FrameObj
getFrameFrame(FrameObj fr)
{ answer(fr);
}


static PceWindow
getPointerWindowFrame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { WindowDecorator dw = (WindowDecorator)sw;

      sw = dw->window;
      if ( sw->has_pointer == ON )
	answer(sw);
    }

    if ( sw->has_pointer == ON )
      answer(sw);
  }

  fail;
}


static status
applicationFrame(FrameObj fr, Application app)
{ if ( fr->application != app )
  { if ( notNil(app) )
      return send(app, NAME_append, fr, EAV);
    else if ( notNil(fr->application) )
      return send(fr->application, NAME_delete, fr, EAV);
  }

  succeed;
}


		 /*******************************
		 *	   EVENT HANDLING	*
		 *******************************/

static status
keyboardFocusFrame(FrameObj fr, PceWindow sw)
{ if ( getHyperedObject(fr, NAME_keyboardFocus, DEFAULT) != sw )
    freeHypersObject(fr, NAME_keyboardFocus, DEFAULT);

  if ( instanceOfObject(sw, ClassWindowDecorator) )
  { WindowDecorator dw = (WindowDecorator)sw;
    sw = dw->window;
  }

  if ( instanceOfObject(sw, ClassWindow) )
  { newObject(ClassHyper, fr, sw, NAME_keyboardFocus, NAME_KeyboardFocus, EAV);
    if ( fr->input_focus == ON )
      send(fr, NAME_inputWindow, sw, EAV);
  } else if ( fr->input_focus == ON )
  { PceWindow iw = getPointerWindowFrame(fr);

    send(fr, NAME_inputWindow, iw, EAV);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find the window  for  redirecting  keyboard   strokes.  If  there  is an
explicit focus, this is easy.  Otherwise,  use   the  window  that has a
keyboard-focus or the window that has a focus (in this order).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

PceWindow
getKeyboardFocusFrame(FrameObj fr)
{ PceWindow sw;
  Cell cell;

  if ( (sw = getHyperedObject(fr, NAME_keyboardFocus, DEFAULT)) )
    answer(sw);

  if ( getSizeChain(fr->members) == ONE )
  { sw = getHeadChain(fr->members);

    if ( instanceOfObject(sw, ClassWindowDecorator) )
    { WindowDecorator dw = (WindowDecorator)sw;
      answer(dw->window);
    }
  }

  for_cell(cell, fr->members)
  { PceWindow sw2 = cell->value;

    if ( instanceOfObject(sw2, ClassWindowDecorator) )
    { WindowDecorator dw = (WindowDecorator)sw2;
      sw2 = dw->window;
    }

    if ( notNil(sw2->focus) )
      answer(sw2);
  }

  answer(sw);
}


static status
inputWindowFrame(FrameObj fr, PceWindow iw)
{ PceWindow ow;

  if ( (ow=getHyperedObject(fr, NAME_inputWindow, DEFAULT)) && ow != iw )
  { inputFocusWindow(ow, OFF);
    freeHypersObject(fr, NAME_inputWindow, DEFAULT);
  }

  if ( fr->input_focus == ON && notNil(iw) )
  { newObject(ClassHyper, fr, iw, NAME_inputWindow, EAV);
    inputFocusWindow(iw, ON);
  }

  succeed;
}


static status
inputFocusFrame(FrameObj fr, BoolObj val)
{ if ( fr->input_focus != val )
  { Cell cell;

    assign(fr, input_focus, val);
    if ( val == ON )
    { PceWindow iw;

      if ( (iw = getKeyboardFocusFrame(fr)) ||
	   (iw = ws_window_holding_point_frame(fr)) )
	inputWindowFrame(fr, iw);
    } else
    { for_cell(cell, fr->members)
      { inputFocusWindow(cell->value, OFF);
      }
    }
  }

  succeed;
}


static status
sensitiveFrame(FrameObj fr, BoolObj sensitive)
{ if ( fr->sensitive != sensitive )
  { assign(fr, sensitive, sensitive);

    ws_enable_frame(fr, sensitive == ON ? TRUE : FALSE);
  }

  succeed;
}


status
redrawFrame(FrameObj fr, Area a)
{ succeed;
}


		 /*******************************
		 *	  SUBTILE LAYOUT	*
		 *******************************/


static status
updateTileAdjustersFrame(FrameObj fr, TileObj t)
{ if ( isDefault(t) )
  { if ( !(t = getTileFrame(fr)) )
      succeed;				/* no tiles, nothing to do */
  }

  if ( notNil(t) )
  { if ( notNil(t->super) && getCanResizeTile(t) == ON )
    { if ( isNil(t->adjuster) )
      { PceWindow adj = newObject(ClassTileAdjuster, t, EAV);
	assert(adj);

	appendFrame(fr, adj);
	ws_topmost_window(adj, ON);
/*	Cprintf("%s: Area = %s, %s, %s, %s\n", pp(adj),
		pp(adj->area->x), pp(adj->area->y),
		pp(adj->area->w), pp(adj->area->h));
*/
      }

      send(t, NAME_updateAdjusterPosition, EAV);
    } else if ( notNil(t->adjuster) )
      freeObject(t->adjuster);

    if ( notNil(t->members) )
    { Cell cell;

      for_cell(cell, t->members)
	updateTileAdjustersFrame(fr, cell->value);
    }
  }

  succeed;
}



		 /*******************************
		 *	       MODAL		*
		 *******************************/

FrameObj
blockedByModalFrame(FrameObj fr)
{ if ( !fr )
    fail;

  if ( notNil(fr->application) )
  { Cell cell;

    for_cell(cell, fr->application->modal)
    { FrameObj fr2 = cell->value;

      if ( fr == fr2 )
	break;

      if ( isOpenFrameStatus(fr2->status) )
	return fr2;
    }
  }

  if ( notNil(fr->transients) )
  { Cell cell;

    for_cell(cell, fr->transients)
    { FrameObj fr2 = cell->value;

      DEBUG(NAME_transient,
	    Cprintf("blockedByModalFrame(%s) checking %s\n",
		    pp(fr), pp(fr2)));

      if ( fr2->modal == NAME_transient &&
	   isOpenFrameStatus(fr2->status) )
      { DEBUG(NAME_transient, Cprintf("\tBlocked on %s\n", pp(fr2)));
	return fr2;
      }
    }
  }

  fail;
}


static status
postEventFrame(FrameObj fr, EventObj ev)
{ fail;
}


status
eventFrame(FrameObj fr, EventObj ev)
{ FrameObj bfr;

  if ( isAEvent(ev, NAME_keyboard ) )
  { PceWindow sw;

    if ( (bfr=blockedByModalFrame(fr)) )
    {
    blocked:
      send(bfr, NAME_expose, EAV);
      send(bfr, NAME_event, ev, EAV);
      fail;
    }

    if ( (sw = getKeyboardFocusFrame(fr)) )
      return postNamedEvent(ev, (Graphical) sw, DEFAULT, NAME_postEvent);

    return send(fr, NAME_typed, ev, EAV);
  }

  if ( isDownEvent(ev) && (bfr=blockedByModalFrame(fr)) )
    goto blocked;

  fail;
}


static status
cursorFrame(FrameObj fr, CursorObj cursor)
{ ws_frame_cursor(fr, cursor);

  succeed;
}


static status
grabPointerFrame(FrameObj fr, BoolObj grab, CursorObj cursor)
{ ws_grab_frame_pointer(fr, grab, cursor);

  succeed;
}


static status
modalFrame(FrameObj fr, Name how)
{ assign(fr, modal, how);

  if ( notNil(fr->application) &&
       memberChain(fr->application->modal, fr) &&
       how != NAME_application )
  { deleteChain(fr->application->modal, fr);
  } else
  { if ( how == NAME_application && notNil(fr->application) )
      send(fr->application, NAME_modal, fr, EAV);
  }

  succeed;
}


static status
typedFrame(FrameObj fr, EventId id)
{ PceWindow sw;

  for_chain(fr->members, sw,
	    if ( send(sw, NAME_typed, id, EAV) )
	      succeed;);

  fail;
}


		/********************************
		*           FRAME KINDS		*
		********************************/

static status
kindFrame(FrameObj fr, Name kind)
{ if ( fr->kind != kind )
  { if ( createdFrame(fr) )
      return errorPce(fr, NAME_noChangeAfterOpen);

    if ( kind == NAME_transient )
    { assign(fr, icon_image, NIL);
      assign(fr, can_resize, OFF);
    }

    assign(fr, kind, kind);
  }

  succeed;
}


static status
transientForFrame(FrameObj fr, FrameObj fr2)
{ if ( fr->transient_for != fr2 )
  { if ( !createdFrame(fr) )
      kindFrame(fr, NAME_transient);

    if ( notNil(fr->transient_for) && notNil(fr->transient_for->transients) )
      send(fr->transient_for, NAME_detachTransient, fr, EAV);

    assign(fr, transient_for, fr2);

    if ( notNil(fr2) )
    { send(fr2, NAME_attachTransient, fr, EAV);

      if ( fr->kind == NAME_transient )
	ws_transient_frame(fr, fr2);
    }
  }

  succeed;
}


static status
attachTransientFrame(FrameObj fr, FrameObj tr)
{ if ( isNil(fr->transients) )
    assign(fr, transients, newObject(ClassChain, tr, EAV));
  else
    addChain(fr->transients, tr);

  succeed;
}


static status
detachTransientFrame(FrameObj fr, FrameObj tr)
{ if ( notNil(fr->transients) )
    return deleteChain(fr->transients, tr);

  fail;
}


static status
informTransientsFramev(FrameObj fr, Name selector, int argc, Any *argv)
{ FrameObj sfr;

  if ( notNil(fr->transients) )
    for_chain(fr->transients, sfr, sendv(sfr, selector, argc, argv));

  succeed;
}


		/********************************
		*            CATCH-ALL		*
		********************************/

static Any
getCatchAllFramev(FrameObj fr, Name name)
{ Name base;

  if ( (base = getDeleteSuffixName(name, NAME_Member)) )
    answer(getMemberFrame(fr, base));

  errorPce(fr, NAME_noBehaviour, CtoName("<-"), name);
  fail;
}


extern status postscriptFrame(FrameObj fr, Name hb);

		/********************************
		*             VISUAL		*
		********************************/

static status
reportFrame(FrameObj fr, Name kind, CharArray fmt, int argc, Any *argv)
{ Any window, reporter;
  ArgVector(av, argc + 2);

  av[0] = kind;
  av[1] = fmt;
  copyArgs(argc, argv, &av[2]);

  if ( (reporter = getv(fr, NAME_reportTo, 0, NULL)) &&
       reporter != fr->display )
    return sendv(reporter, NAME_report, argc+2, av);

  for_chain(fr->members, window,
	    if ( !(notNil(REPORTEE->value) &&
		   memberChain(REPORTEE->value, window)) &&
		 sendv(window, NAME_report, argc+2, av) )
	      succeed);

  if ( notNil(fr->transient_for) &&
       sendv(fr->transient_for, NAME_report, argc+2, av) )
    succeed;

  return reportVisual((VisualObj)fr, kind, fmt, argc, argv);
}


static DisplayObj
getContainedInFrame(FrameObj fr)
{ answer(fr->display);
}


static Chain
getContainsFrame(FrameObj fr)
{ answer(fr->members);
}

		 /*******************************
		 *	     THREADS		*
		 *******************************/

static Int
getThreadFrame(FrameObj fr)
{ return ws_frame_thread(fr);
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_openCentered[] =
        { "center=[point]", "grab=[bool]", "monitor=[monitor]" };
static char *T_busyCursor[] =
        { "cursor=[cursor]*", "block_input=[bool]" };
static char *T_icon[] =
        { "image=image", "icon_label=[name]" };
static char *T_initialise[] =
        { "label=[name]",
	  "kind=[{toplevel,transient,popup}]",
	  "display=[display]",
	  "application=[application]"};
static char *T_label[] =
        { "label=name", "icon_label=[name]" };
static char *T_postscript[] =
        { "landscape=[bool]", "scale_in=[area]" };
static char *T_positionADpointD_grabADboolD_normaliseADboolD[] =
        { "position=[point]", "grab=[bool]", "normalise=[bool]" };
static char *T_wmProtocol[] =
        { "protocol=name", "action=code" };
static char *T_convertOldSlot[] =
        { "slot=name", "value=any" };
static char *T_set[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	  "monitor=[monitor]" };
static char *T_grab_pointer[] =
	{ "grab=bool", "cursor=[cursor]" };
static char *T_center[] =
	{ "center=[point]", "monitor=[monitor]" };
static char *T_geometry[] =
	{ "geometry=name", "monitor=[monitor]" };

/* Instance Variables */

static vardecl var_frame[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_name, "Name of the frame"),
  IV(NAME_label, "name", IV_GET,
     NAME_label, "Label of the frame"),
  SV(NAME_iconLabel, "name*", IV_NONE|IV_STORE, iconLabelFrame,
     NAME_icon, "Label in the iconic representation"),
  IV(NAME_iconImage, "image*", IV_GET,
     NAME_icon, "Image used for the iconic representation"),
  SV(NAME_iconPosition, "point*", IV_GET|IV_STORE, iconPositionFrame,
     NAME_icon, "Position of the iconic image"),
  SV(NAME_application, "application*", IV_GET|IV_STORE, applicationFrame,
     NAME_organisation, "Application the frame belongs too"),
  IV(NAME_display, "display", IV_BOTH,
     NAME_organisation, "Display the frame resides on"),
  IV(NAME_border, "[int]", IV_GET,
     NAME_appearance, "Width of border"),
  SV(NAME_background, "colour|pixmap", IV_GET|IV_STORE, backgroundFrame,
     NAME_appearance, "Background of the frame"),
  SV(NAME_colourMap, "[colour_map]*", IV_GET|IV_STORE, colourMapFrame,
     NAME_appearance, "Colourmap for the window's frame"),
  SV(NAME_area, "area", IV_GET|IV_STORE, areaFrame,
     NAME_area, "Area of the opened frame on the display"),
  IV(NAME_geometry, "name*", IV_NONE,
     NAME_area, "X-window geometry specification"),
  IV(NAME_members, "chain", IV_NONE,
     NAME_organisation, "Windows in the frame"),
  SV(NAME_kind, "{toplevel,transient,popup}", IV_GET|IV_STORE, kindFrame,
     NAME_appearance, "Tool, support or popup"),
  SV(NAME_transientFor, "frame*", IV_GET|IV_STORE, transientForFrame,
     NAME_transient, "Frame I'm transient for (i.e. support for)"),
  IV(NAME_transients, "chain*", IV_GET,
     NAME_transient, "Back pointer for transient frames"),
  SV(NAME_modal, "{application,transient}*", IV_GET|IV_STORE, modalFrame,
     NAME_modal, "Operate as modal window"),
  IV(NAME_returnValue, "unchecked", IV_NONE,
     NAME_modal, "Bin for value of ->return"),
  SV(NAME_inputFocus, "bool", IV_GET|IV_STORE, inputFocusFrame,
     NAME_event, "Frame has focus for keyboard events"),
  SV(NAME_sensitive, "bool", IV_GET|IV_STORE, sensitiveFrame,
     NAME_event, "@on: window accepts user input"),
  IV(NAME_status, "{unlinking,unmapped,hidden,iconic,window,full_screen}", IV_GET,
     NAME_visibility, "Current visibility of the frame"),
  IV(NAME_canDelete, "bool", IV_BOTH,
     NAME_permission, "Frame can be deleted by user"),
  IV(NAME_canResize, "bool", IV_BOTH,
     NAME_permission, "Frame can be resized by user"),
  IV(NAME_confirmDone, "bool", IV_BOTH,
     NAME_permission, "Ask confirmation on user-delete"),
  IV(NAME_fitting, "bool", IV_BOTH,
     NAME_internal, "We are running ->fit"),
  IV(NAME_wmProtocols, "sheet", IV_GET,
     NAME_windowManager, "Protocol-name --> message"),
  IV(NAME_wmProtocolsAttached, "bool", IV_GET,
     NAME_internal, "Have we registered the protocols"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_windowSystem, "Window-System reference")
};

/* Send Methods */

static senddecl send_frame[] =
{ SM(NAME_convertOldSlot, 2, T_convertOldSlot, convertOldSlotFrame,
     DEFAULT, "Convert old `show' slot"),
  SM(NAME_initialise, 4, T_initialise, initialiseFrame,
     DEFAULT, "Create from label, kind and display"),
  SM(NAME_initialiseNewSlot, 1, "var=variable", initialiseNewSlotFrame,
     DEFAULT, "Initialise <-background"),
  SM(NAME_reset, 0, NULL, resetFrame,
     DEFAULT, "Remove ->busy_cursor"),
  SM(NAME_unlink, 0, NULL, unlinkFrame,
     DEFAULT, "Destroy windows and related X-window"),
  SM(NAME_status, 1, "{unmapped,hidden,iconic,window,full_screen,open}",
     statusFrame, DEFAULT, "Current visibility of the frame"),
  SM(NAME_typed, 1, "event|event_id", typedFrame,
     NAME_accelerator, "Dispatch over available windows"),
  SM(NAME_flush, 0, NULL, flushFrame,
     NAME_animate, "Flush X-server"),
  SM(NAME_synchronise, 0, NULL, synchroniseFrame,
     NAME_animate, "->flush and process pending events"),
  SM(NAME_border, 1, "thickness=int", borderFrame,
     NAME_appearance, "X-border width"),
  SM(NAME_showLabel, 1, "show=bool", showLabelFrame,
     NAME_appearance, "If @off, sets <->kind to `transient'"),
  SM(NAME_center, 2, T_center, centerFrame,
     NAME_area, "Move the frame to make point its center"),
  SM(NAME_height, 1, "height=int", heightFrame,
     NAME_area, "Set height of frame"),
  SM(NAME_move, 1, "position=point", positionFrame,
     NAME_area, "Move the frame on the display"),
  SM(NAME_position, 1, "position=point", positionFrame,
     NAME_area, "Move the frame on the display"),
  SM(NAME_set, 5, T_set, setFrame,
     NAME_area, "Set XYWH of frame on display"),
  SM(NAME_size, 1, "size=size", sizeFrame,
     NAME_area, "Resize the frame"),
  SM(NAME_width, 1, "width=int", widthFrame,
     NAME_area, "Set width of frame"),
  SM(NAME_x, 1, "x=int", xFrame,
     NAME_area, "Set X-coordinate of frame"),
  SM(NAME_y, 1, "x=int", yFrame,
     NAME_area, "Set Y-coordinate of frame"),
  SM(NAME_geometry, 2, T_geometry, geometryFrame,
     NAME_area, "X-window geometry specification"),
  SM(NAME_busyCursor, 2, T_busyCursor, busyCursorFrame,
     NAME_event, "Define (temporary) cursor for all windows in the frame"),
  SM(NAME_inputWindow, 1, "window", inputWindowFrame,
     NAME_focus, "Input is directed to this window"),
  SM(NAME_keyboardFocus, 1, "[window]*", keyboardFocusFrame,
     NAME_focus, "Redirect (default) keyboard input here"),
  SM(NAME_closed, 1, "open=bool", closedFrame,
     NAME_icon, "Open/iconify frame"),
  SM(NAME_icon, 2, T_icon, iconFrame,
     NAME_icon, "Set image and icon_label"),
  SM(NAME_label, 2, T_label, labelFrame,
     NAME_label, "Set label of the frame"),
  SM(NAME_fit, 0, NULL, fitFrame,
     NAME_layout, "Recompute windows and resize the frame"),
  SM(NAME_resize, 0, NULL, resizeFrame,
     NAME_layout, "Recompute layout of sub-windows"),
  SM(NAME_return, 1, "unchecked", returnFrame,
     NAME_modal, "Return after a <-confirm"),
  SM(NAME_create, 0, NULL, createFrame,
     NAME_open, "Establish window-system counterpart"),
  SM(NAME_mapped, 1, "bool", mappedFrame,
     NAME_open, "Inform transients using ->show"),
  SM(NAME_open, 3, T_positionADpointD_grabADboolD_normaliseADboolD, openFrame,
     NAME_open, "->create and map on the display"),
  SM(NAME_openCentered, 3, T_openCentered, openCenteredFrame,
     NAME_open, "Open centered around point"),
  SM(NAME_uncreate, 0, NULL, uncreateFrame,
     NAME_open, "Destroy window-system counterpart"),
  SM(NAME_wait, 0, NULL, waitFrame,
     NAME_open, "Wait till <-status is `open'"),
  SM(NAME_append, 1, "subwindow=window", appendFrame,
     NAME_organisation, "Append a window to the frame"),
  SM(NAME_delete, 1, "member:window", deleteFrame,
     NAME_organisation, "Delete window from the frame"),
  SM(NAME_Postscript, 1, "{head,body}", postscriptFrame,
     NAME_postscript, "Create PostScript for interior"),
  SM(NAME_bell, 1, "volume=[int]", bellFrame,
     NAME_report, "Ring the bell on display"),
  SM(NAME_report, 3, T_report, reportFrame,
     NAME_report, "Report message (send to <-members)"),
  SM(NAME_expose, 0, NULL, exposeFrame,
     NAME_stacking, "Put frame above all others on the display"),
  SM(NAME_exposed, 0, NULL, exposedFrame,
     NAME_stacking, "Inform transient windows to expose"),
  SM(NAME_hidden, 0, NULL, hiddenFrame,
     NAME_stacking, "Inform transient windows to hide"),
  SM(NAME_hide, 0, NULL, hideFrame,
     NAME_stacking, "Put frame below all others on the display"),
  SM(NAME_show, 1, "show=bool", showFrame,
     NAME_visibility, "(Un)show the frame on the display"),
  SM(NAME_deleteWmProtocol, 1, "protocol=name", deleteWmProtocolFrame,
     NAME_windowManager, "Delete window manager protocol"),
  SM(NAME_doneMessage, 1, "action=code", doneMessageFrame,
     NAME_windowManager, "Trap window manager WM_DELETE_WINDOW"),
  SM(NAME_saveMessage, 1, "action=code", saveMessageFrame,
     NAME_windowManager, "Trap window manager WM_SAVE_YOURSELF"),
  SM(NAME_wmDelete, 0, NULL, wmDeleteFrame,
     NAME_windowManager, "Default handling for WM_DELETE_WINDOW"),
  SM(NAME_wmProtocol, 2, T_wmProtocol, wmProtocolFrame,
     NAME_windowManager, "Register window manager protocol"),
  SM(NAME_event, 1, "event", eventFrame,
     NAME_event, "Handle event on frame-background"),
  SM(NAME_postEvent, 1, "event", postEventFrame,
     NAME_event, "Handle keyboard event on frame-background (fail)"),
  SM(NAME_cursor, 1, "[cursor]", cursorFrame,
     NAME_event, "Define the cursor for the frame-background"),
  SM(NAME_grabPointer, 2, T_grab_pointer, grabPointerFrame,
     NAME_event, "Grap all pointer-events"),
  SM(NAME_redraw, 1, "[area]", redrawFrame,
     NAME_redraw, "Redraw subwindow adjust buttons"),

  SM(NAME_updateTileAdjusters, 1, "[tile]", updateTileAdjustersFrame,
     NAME_tile, "Update display and location of tile-adjusters"),

  SM(NAME_attachTransient, 1, "frame", attachTransientFrame,
     NAME_transient, "A frame is attached as a transient for me"),
  SM(NAME_detachTransient, 1, "frame", detachTransientFrame,
     NAME_transient, "A frame is detached as a transient for me")
};

/* Get Methods */

static getdecl get_frame[] =
{ GM(NAME_containedIn, 0, "display", NULL, getContainedInFrame,
     DEFAULT, "Display that contains me"),
  GM(NAME_contains, 0, "chain", NULL, getContainsFrame,
     DEFAULT, "Chain with windows contained"),
  GM(NAME_convert, 1, "frame", "window", getConvertFrame,
     DEFAULT, "Frame of the window"),
  GM(NAME_geometry, 0, "name", NULL, getGeometryFrame,
     NAME_area, "X-geometry specification"),
  GM(NAME_position, 0, "point", NULL, getPositionFrame,
     NAME_area, "Position on the display"),
  GM(NAME_size, 0, "size", NULL, getSizeFrame,
     NAME_area, "Size on the display"),
  GM(NAME_monitor, 0, "monitor", NULL, getMonitorFrame,
     NAME_organisation, "Monitor frame is displayed on"),
  GM(NAME_image, 1, "image", "[{bitmap,pixmap}]", getImageFrame,
     NAME_conversion, "Image with the pixels of the frame"),
  GM(NAME_keyboardFocus, 0, "window", NULL, getKeyboardFocusFrame,
     NAME_focus, "Window for default keyboard input"),
  GM(NAME_closed, 0, "bool", NULL, getClosedFrame,
     NAME_icon, "Open (@off) or iconic (@on)"),
  GM(NAME_iconLabel, 0, "name", NULL, getIconLabelFrame,
     NAME_icon, "Name of the icon"),
  GM(NAME_iconPosition, 0, "point*", NULL, getIconPositionFrame,
     NAME_icon, "(Current) position of the icon"),
  GM(NAME_tile, 0, "tile", NULL, getTileFrame,
     NAME_layout, "Find tile managing object"),
  GM(NAME_confirm, 3, "return_value=any", T_positionADpointD_grabADboolD_normaliseADboolD, getConfirmFrame,
     NAME_modal, "Start sub-eventloop until ->return"),
  GM(NAME_confirmCentered, 3, "return_value=any", T_openCentered, getConfirmCenteredFrame,
     NAME_modal, "As <-confirm, but centered around point"),
  GM(NAME_catchAll, 1, "window", "window_name=name", getCatchAllFramev,
     NAME_organisation, "Get named window"),
  GM(NAME_frame, 0, "frame", NULL, getFrameFrame,
     NAME_organisation, "Returns itself"),
  GM(NAME_member, 1, "window", "name", getMemberFrame,
     NAME_organisation, "Find member window by name"),
  GM(NAME_members, 0, "chain", NULL, getMembersFrame,
     NAME_organisation, "New chain holding all member windows"),
  GM(NAME_boundingBox, 0, "area", NULL, getBoundingBoxFrame,
     NAME_postscript, "Bounding for PostScript"),
  GM(NAME_postscript, 2, "postscript=string", T_postscript, getPostscriptObject,
     NAME_postscript, "Get PostScript representation of frame"),
  GM(NAME_show, 0, "bool", NULL, getShowFrame,
     NAME_visibility, "@on iff <-status = open; @off otherwise"),
  GM(NAME_thread, 0, "int", NULL, getThreadFrame,
     NAME_thread, "Return system thread-id that owns the frame")
};

/* Resources */

static classvardecl rc_frame[] =
{ RC(NAME_background, "colour|pixmap", "@_dialog_bg",
     "Default background colour"),
  RC(NAME_busyCursor, "cursor*", UXWIN("watch", "win_wait"),
     "Default cursor displayed by ->busy_cursor"),
  RC(NAME_confirmDone, "bool", "@off",
     "Show confirmer on `Delete'"),
  RC(NAME_geometry, "name*", "@nil",
     "Position/size of the frame"),
  RC(NAME_iconImage, "image*", "@pce_image",
     "Image displayed for an icon"),
  RC(NAME_iconLabel, "name*", "@nil",
     "Label displayed in the icon"),
  RC(NAME_canResize, "bool", "@on",
     "Window can be resized by user"),
  RC(NAME_horizontalResizeCursor, "cursor",
     UXWIN("sb_h_double_arrow", "win_sizewe"),
     "Cursor for horizontally resizing tile"),
  RC(NAME_verticalResizeCursor, "cursor",
     UXWIN("sb_v_double_arrow", "win_sizens"),
     "Cursor for vertically resizing tile"),
  RC(NAME_fitAfterAppend, "bool", "@off",
     "Automatically ->fit the frame after a subwindow was added"),
  RC(NAME_decorateTransient, "bool", "@on",
     "Decorate transient windows (if possible)"),
  RC(NAME_colourMap, "[colour_map]*", "@default",
     "Colourmap for the window's frame")
};

/* Class Declaration */

static Name frame_termnames[] = { NAME_label, NAME_kind, NAME_display };

ClassDecl(frame_decls,
          var_frame, send_frame, get_frame, rc_frame,
          3, frame_termnames,
          "$Rev$");


status
makeClassFrame(Class class)
{ declareClass(class, &frame_decls);
  setLoadStoreFunctionClass(class, loadFrame, storeFrame);

  ConstantNotReturned = globalObject(NAME_NotReturned, ClassConstant,
				     NAME_NotReturned,
				     CtoString("Used for `frame <-confirm'"),
				     EAV);
  succeed;
}

