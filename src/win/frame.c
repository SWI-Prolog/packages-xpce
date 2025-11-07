/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
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
#include <h/dialog.h>

TileObj		getTileFrame(FrameObj);
forwards int	get_position_from_center_frame(FrameObj, DisplayObj, Point, int *, int *);
static void	ensure_on_display(FrameObj, DisplayObj, int *, int *);
static status	closedFrame(FrameObj, BoolObj);
static status	openFrame(FrameObj fr, Point pos, DisplayObj dsp,
			  BoolObj grab);
static status	doneMessageFrame(FrameObj fr, Code msg);
static status	geometryFrame(FrameObj fr, Name spec, DisplayObj mon);
static status	setFrame(FrameObj fr, Int x, Int y, Int w, Int h, DisplayObj mon);
static status	kindFrame(FrameObj fr, Name kind);
static status	informTransientsFramev(FrameObj fr, Name selector,
				       int argc, Any *argv);
static status	grabPointerFrame(FrameObj fr, BoolObj grab, CursorObj cursor);
static status	cursorFrame(FrameObj fr, CursorObj cursor);
static status   statusFrame(FrameObj fr, Name stat);

#define isOpenFrameStatus(s) ((s) == NAME_window || (s) == NAME_fullScreen)

static status
initialiseFrame(FrameObj fr, Name label, Name kind,
		DisplayObj display, Application app)
{ if ( isDefault(kind) )
    kind = NAME_toplevel;
  if ( isDefault(display) )
    display = CurrentDisplay(NIL);
  if ( isDefault(label) )
    label = CtoName("Untitled");
  if ( isDefault(app) )
    app = NIL;

  assign(fr, name,		    getClassNameObject(fr));
  assign(fr, label,		    label);
  assign(fr, display,		    display);
  assign(fr, border,		    DEFAULT);
  assign(fr, area,		    newObject(ClassArea, EAV));
  assign(fr, placed,		    OFF);
  assign(fr, members,		    newObject(ClassChain, EAV));
  assign(fr, kind,		    kind);
  assign(fr, status,		    NAME_unmapped);
  assign(fr, can_delete,	    ON);
  assign(fr, input_focus,	    OFF);
  assign(fr, sensitive,		    ON);
  assign(fr, fitting,		    OFF);
  assign(fr, wm_protocols,	    newObject(ClassSheet, EAV));
  assign(fr, wm_protocols_attached, OFF);
  obtainClassVariablesObject(fr);

  doneMessageFrame(fr, newObject(ClassMessage, RECEIVER, NAME_wmDelete, EAV));

  fr->ws_ref = NULL;			/* Window System Reference */

  if ( notNil(app) )
    send(app, NAME_append, fr, EAV);

  succeed;
}

static void
destroyTransientFrame(FrameObj fr)
{ if ( isProtectedObj(fr) )	/* a special (reusable) frame */
  { if ( destroyCompleterFrame(fr) )
      return;
  }
  send(fr, NAME_destroy, EAV);
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
      for_chain(fr->transients, sfr, destroyTransientFrame(sfr));
    if ( notNil(fr->transient_for) && notNil(fr->transient_for->transients) )
      send(fr->transient_for, NAME_detachTransient, fr, EAV);

    ws_uncreate_frame(fr);
    deleteChain(fr->display->frames, fr);
    deleteChain(MappedFrames, fr);
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

static status
displayFrame(FrameObj fr, DisplayObj d)
{ if ( fr->display != d )
  { DisplayObj odsp = fr->display;

    appendChain(d->frames, fr);
    assign(fr, display, d);
    deleteChain(odsp->frames, fr);
    if ( emptyChain(odsp->frames) && isOn(odsp->removed) )
      send(odsp, NAME_removed, EAV);
  }

  succeed;
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

static status
SdlWaitConfirmFrame(FrameObj fr)
{ ASSERT_SDL_MAIN();

  addCodeReference(fr);
  while( offFlag(fr, F_FREED|F_FREEING) &&
	 fr->return_value == ConstantNotReturned )
  { dispatchDisplay(fr->display);
    ws_discard_input("Confirmer running");
  }
  status rc = offFlag(fr, F_FREED|F_FREEING);
  delCodeReference(fr);
  return rc;
}


static Any
getWaitConfirmFrame(FrameObj fr)
{ status rc;

  assign(fr, return_value, ConstantNotReturned);

  if ( SDL_IsMainThread() )
    rc = SdlWaitConfirmFrame(fr);
  else
    rc = wait_host(true, fr, NAME_SdlWaitConfirm, EAV);

  if ( rc )
  { Any rval = fr->return_value;

    if ( isObject(rval) )
    { addCodeReference(rval);
      assign(fr, return_value, ConstantNotReturned);
      pushAnswerObject(rval);
      delCodeReference(rval);
    } else
      assign(fr, return_value, ConstantNotReturned);

    return rval;
  }

  fail;
}

Any
getConfirmFrame(FrameObj fr, Point pos, BoolObj grab)
{ TRY( openFrame(fr, pos, DEFAULT, grab) &&
       exposeFrame(fr) );
  busyCursorDisplay(fr->display, NIL, DEFAULT);

  return getWaitConfirmFrame(fr);
}

Any
getConfirmCenteredFrame(FrameObj fr, Any where, BoolObj grab, DisplayObj dsp)
{ TRY( send(fr, NAME_create, EAV) );

  if ( isDefault(where) && notNil(fr->transient_for) )
    where = fr->transient_for;
  DEBUG(NAME_confirm, Cprintf("%s <-confirm_centered(%s)\n", pp(fr), pp(where)));

  if ( instanceOfObject(where, ClassFrame) )
  { FrameObj rfr = where;
    int ox = (valInt(rfr->area->w)-valInt(fr->area->w))/2;
    int oy = (valInt(rfr->area->h)-valInt(fr->area->h))/2;
    ox += valInt(rfr->area->x);	/* SDL pos sometimes seems relative to parent */
    oy += valInt(rfr->area->y);
    assign(fr->area, x, toInt(ox));
    assign(fr->area, y, toInt(oy));
    send(fr, NAME_transientFor, rfr, EAV);
  }

  return getConfirmFrame(fr, DEFAULT, grab);
}


static status
returnFrame(FrameObj fr, Any obj)
{ assign(fr, return_value, obj);

  succeed;
}


static status
openFrame(FrameObj fr, Point pos, DisplayObj dsp, BoolObj grab)
{ Int x, y;
  Int w = DEFAULT, h = DEFAULT;

  if ( isDefault(pos) && isOpenFrameStatus(fr->status) )
    succeed;
  if ( notDefault(pos) )
    assign(fr, placed, ON);

  if ( notDefault(pos) )
  { x = pos->x;
    y = pos->y;

    setFrame(fr, x, y, w, h, DEFAULT);
  } else if ( notNil(fr->geometry) )
  { ws_x_geometry_frame(fr, fr->geometry, DEFAULT);
  }

  if ( !createdFrame(fr) )
  { DEBUG(NAME_frame, Cprintf("Creating %s\n", pp(fr)));
    TRY( send(fr, NAME_create, EAV) );
  }

  if ( !isOpenFrameStatus(fr->status) )
    return sdl_send(fr, NAME_status, false, NAME_window, EAV);

  succeed;
}


static status
openCenteredFrame(FrameObj fr, Point pos, DisplayObj dsp, BoolObj grab)
{ int x, y;
  int rval;
  Point p2;

  TRY( send(fr, NAME_create, EAV) );

  get_position_from_center_frame(fr, dsp, pos, &x, &y);
  ensure_on_display(fr, DEFAULT, &x, &y);
  p2 = answerObject(ClassPoint, toInt(x), toInt(y), EAV);
  rval = openFrame(fr, p2, dsp, grab);
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
{ return wmProtocolFrame(fr, NAME_WM_DELETE_WINDOW, msg);
}


static status
saveMessageFrame(FrameObj fr, Code msg)
{ return wmProtocolFrame(fr, NAME_WM_SAVE_YOURSELF, msg);
}


static status
mappedFrame(FrameObj fr, BoolObj val)
{ Any stat = (val == ON ? NAME_window : NAME_hidden);
  informTransientsFramev(fr, NAME_status, 1, &stat);

  succeed;
}


/**
 * Create a frame.  In the pre-SDL  days, this would create the system
 * windows without showing them.  In SDL, windows are no longer system
 * windows  and we  only create  the SDL  window when  it is  visible.
 * Creating a frame now implies realizing the embedded windows and the
 * layout.  The actual mapping is done by statusFrame()
 */

status
createdFrame(FrameObj fr)
{ return fr->status != NAME_unmapped;
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

  assign(fr, status, NAME_hidden);
  for_cell(cell, fr->members)
    send(cell->value, NAME_create, EAV);

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
      resizeFrame(fr);
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
SdlWaitFrame(FrameObj fr)
{ ASSERT_SDL_MAIN();
  if ( fr->status == NAME_unmapped )
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
waitFrame(FrameObj fr)
{ if ( isOpenFrameStatus(fr->status) )
    succeed;

  if ( SDL_IsMainThread() )
    return SdlWaitFrame(fr);
  else
    return wait_host(true, fr, NAME_SdlWait, EAV);
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
{ sdl_send(fr, NAME_SdlExpose, false, EAV);

  succeed;
}

static status
SdlExposeFrame(FrameObj fr)
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
		*       AREA MANAGENENT		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get XY coordinate of frame if  its  center   must  be  at pos. If Pos is
DEFAULT it is centered in the given   display. If the monitor is default
too, we deduce the most recent monitor from the event.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
get_position_from_center_frame(FrameObj fr, DisplayObj d, Point pos,
			       int *x, int *y)
{ if ( isDefault(pos) )
  { d = CurrentDisplay(fr);

    if ( d )
    { Area a = d->area;
      *x = valInt(a->w)/2;
      *y = valInt(a->h)/2;
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
ensure_on_display(FrameObj fr, DisplayObj dsp, int *x, int *y)
{ int rm, bm;

  if ( isDefault(dsp) )
    dsp = CurrentDisplay(fr);

  Area a = dsp->area;
  rm = valInt(a->x) + valInt(a->w);
  bm = valInt(a->y) + valInt(a->h);

  if ( *x + valInt(fr->area->w) > rm )
    *x -= *x + valInt(fr->area->w) - rm;
  if ( *y + valInt(fr->area->h) > bm )
    *y -= *y + valInt(fr->area->h) - bm;
  if ( *x < valInt(dsp->area->x) )
    *x = valInt(dsp->area->x);
  if ( *y < valInt(dsp->area->y) )
    *y = valInt(dsp->area->y);
}


static Size
getSizeFrame(FrameObj fr)
{ answer(getSizeArea(fr->area));
}


static Point
getPositionFrame(FrameObj fr)
{ answer(getPositionArea(fr->area));
}

static Name
getGeometryFrame(FrameObj fr)
{ int x, y, ww, wh;

  if ( ws_frame_bb(fr, &x, &y, &ww, &wh) )	/* outer area */
  { int mx, my, mw, mh;
    bool xn=false, yn=false;
    char buf[100];
    DisplayObj d = fr->display;
    int cw, ch;

    Area a = d->area;		/* work area seems unreliable */
    cw = valInt(fr->area->w);			/* Client area */
    ch = valInt(fr->area->h);

    mx = my = 0;
    mw = valInt(a->w);
    mh = valInt(a->h);

    if ( x-mx > ((mx+mw) - (x+ww))*2 )	/* over 2/3th */
    { x = (mx+mw) - (x+ww);
      xn = true;
    } else
    { x -= mx;
    }
    if ( y-my > ((my+mh) - (y+wh))*2 )
    { y = (my+mh) - (y+wh);
      yn = true;
    } else
    { y -= my;
    }

    if ( fr->can_resize != OFF )
      sprintf(buf, "%dx%d", cw, ch);
    else
      buf[0] = EOS;

    sprintf(buf+strlen(buf),
	    "%s%d%s%d", xn ? "-" : "+", x, yn ? "-" : "+", y);

    if ( d->number != ONE )
      sprintf(buf+strlen(buf), "@%d", (int)valInt(d->number));

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
geometryFrame(FrameObj fr, Name spec, DisplayObj dsp)
{ assign(fr, geometry, spec);

  ws_x_geometry_frame(fr, spec, dsp);

  succeed;
}


static status
setFrame(FrameObj fr, Int x, Int y, Int w, Int h, DisplayObj dsp)
{ Area a = fr->area;

  if ( ws_created_frame(fr) )
  { setArea(a, x, y, DEFAULT, DEFAULT);
    sdl_send(fr, NAME_SdlSet, false, x, y, w, h, dsp, EAV);
  } else
  { if ( notDefault(dsp) && dsp != fr->display )
      assign(fr, display, dsp);

    if ( notDefault(x) || notDefault(y) )
      assign(fr, placed, ON);
    setArea(a, x, y, w, h);
    if ( valInt(a->w) <= 0 )		/* Window systems don't like that */
      assign(a, w, ONE);
    if ( valInt(a->h) <= 0 )
      assign(a, h, ONE);
  }

  succeed;
}

static status
SdlSetFrame(FrameObj fr, Int x, Int y, Int w, Int h, DisplayObj dsp)
{ return ws_geometry_frame(fr, x, y, w, h, dsp);
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
centerFrame(FrameObj fr, Point pos, DisplayObj dsp)
{ int x, y;

  get_position_from_center_frame(fr, dsp, pos, &x, &y);
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

  if ( ws_created_frame(fr) )
    sdl_send(fr, NAME_SdlSetLabel, false, EAV);

  if ( notDefault(icon) )
    iconLabelFrame(fr, icon);

  succeed;
}


static status
SdlSetLabelFrame(FrameObj fr)
{ ws_set_label_frame(fr);
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

    if ( notNil(sw2->focus) || notNil(sw2->keyboard_focus) )
      answer(sw2);
  }

  answer(sw);
}


static status
inputWindowFrame(FrameObj fr, PceWindow iw)
{ PceWindow ow;

  if ( (ow=getHyperedObject(fr, NAME_inputWindow, DEFAULT)) && ow != iw )
  { send(ow, NAME_inputFocus, OFF, EAV);
    freeHypersObject(fr, NAME_inputWindow, DEFAULT);
  }

  if ( fr->input_focus == ON && notNil(iw) )
  { newObject(ClassHyper, fr, iw, NAME_inputWindow, EAV);
    send(iw, NAME_inputFocus, ON, EAV);
  }

  succeed;
}


static status
inputFocusFrame(FrameObj fr, BoolObj val)
{ DEBUG(NAME_keyboard,
	Cprintf("inputFocusFrame(%s, %s->%s)\n",
		pp(fr), pp(fr->input_focus), pp(val)));

  if ( fr->input_focus != val )
  { PceWindow iw;

    assign(fr, input_focus, val);
    if ( val == ON )
    { if ( (iw = getKeyboardFocusFrame(fr)) ||
	   (iw = ws_window_holding_point_frame(fr)) )
	inputWindowFrame(fr, iw);
    } else
    { Cell cell;
      for_cell(cell, fr->members)
	send(cell->value, NAME_inputFocus, OFF, EAV);
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

/* This  used  to  update  small  adjuster  widgets.   Now  it  merely
 * establishes the can_resize attribute of all subtiles.
 */

static status
updateTileAdjustersFrame(FrameObj fr, TileObj t)
{ if ( isDefault(t) )
  { if ( !(t = getTileFrame(fr)) )
      succeed;				/* no tiles, nothing to do */
  }

  if ( notNil(t) )
  { if ( notNil(t->super) )
    { (void) getCanResizeTile(t);
    }

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

static TileObj resizingTile = NIL;

static status
tileResizeEvent(EventObj ev)
{ if ( ev->window == ev->frame )
  { if ( ev->id == NAME_locMove || ev->id == NAME_msLeftDown )
    { TileObj tile = getTileFrame(ev->frame);

      if ( tile )
      { Point pt = tempObject(ClassPoint, ev->x, ev->y, EAV);
	TileObj sub = getSubTileToResizeTile(tile, pt);
	considerPreserveObject(pt);
	if ( sub )
	{ static CursorObj hresize = NULL;
	  static CursorObj vresize = NULL;
	  static CursorObj cursor;

	  if ( !hresize )
	    hresize = getClassVariableValueObject(ev->frame,
						  NAME_horizontalResizeCursor);
	  if ( !vresize )
	    vresize = getClassVariableValueObject(ev->frame,
						  NAME_verticalResizeCursor);

	  if ( sub->super->orientation == NAME_vertical )
	    cursor = vresize;
	  else
	    cursor = hresize;

	  ws_frame_cursor(ev->frame, cursor);
	  if ( ev->id == NAME_msLeftDown )
	  { DEBUG(NAME_tile, Cprintf("Start resizing %s\n", pp(sub)));
	    resizingTile = sub;
	  }

	  DEBUG(NAME_tile,
		Cprintf("Resize for %s (%s) at %d,%d; cursor = %s\n",
			pp(sub), pp(sub->super->orientation),
			valInt(sub->area->x), valInt(sub->area->y),
			pp(cursor)));

	  succeed;
	}
      }
    } else if ( notNil(resizingTile) &&
		(ev->id == NAME_msLeftDrag || ev->id == NAME_msLeftUp) )
    { TileObj sub = resizingTile;
      if ( sub->super->orientation == NAME_vertical )
      { int h = valInt(ev->y) - valInt(sub->area->y);
	send(sub, NAME_height, toInt(h), EAV);
      } else
      { int w = valInt(ev->x) - valInt(sub->area->x);
	send(sub, NAME_width, toInt(w), EAV);
      }

      if ( ev->id == NAME_msLeftUp )
	resizingTile = NIL;

      succeed;
    }
  }
  fail;
}

static status
postEventFrame(FrameObj fr, EventObj ev)
{ return qadSendv(fr, NAME_event, 1, (Any*)&ev);
}


status
eventFrame(FrameObj fr, EventObj ev)
{ FrameObj bfr;

  if ( isAEvent(ev, NAME_keyboard ) )
  { PceWindow sw;

    if ( (bfr=blockedByModalFrame(fr)) )
    {
    blocked:
      DEBUG(NAME_modal, Cprintf("%s: forwarding %s to modal frame %s\n",
				pp(fr), pp(ev), pp(bfr)));
      send(bfr, NAME_expose, EAV);
      send(bfr, NAME_event, ev, EAV);
      fail;
    }

    if ( (sw = getKeyboardFocusFrame(fr)) )
    { DEBUG(NAME_keyboard, Cprintf("%s: forward %s to focussed %s\n",
				   pp(fr), pp(ev->id), pp(sw)));
      return postNamedEvent(ev, (Graphical) sw, DEFAULT, NAME_postEvent);
    }

    return send(fr, NAME_typed, ev, EAV);
  }

  tileResizeEvent(ev);

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
	    Chain rv;
	    if ( !(notNil((rv=getValueVar(REPORTEE))) &&
		   memberChain(rv, window)) &&
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
		 *       SDL FILE DIALOGUE        *
		 *******************************/

#define MAX_FILE_PATTERNS 25

typedef struct open_file_context
{ FrameObj		frame;
  SDL_DialogFileFilter  filters[MAX_FILE_PATTERNS];
  int			nfilters;
  bool			allow_many;
} open_file_context;

static status
getFileFilters(Any from, open_file_context *ctx)
{ ctx->nfilters = 0;

  if ( instanceOfObject(from, ClassChain) )
  { Cell cell;

    for_cell(cell, (Chain)from)
    { if ( instanceOfObject(cell->value, ClassTuple) )
      { Tuple t = cell->value;

	if ( ctx->nfilters == MAX_FILE_PATTERNS )
	  return errorPce(from, NAME_tooManyFilters);

	if ( instanceOfObject(t->first, ClassCharArray) )
	{ ctx->filters[ctx->nfilters].name = charArrayToUTF8(t->first);

	  if ( instanceOfObject(t->second, ClassCharArray) )
	  { ctx->filters[ctx->nfilters].pattern = charArrayToUTF8(t->second);
	  } else if ( instanceOfObject(t->second, ClassChain) )
	  { StringObj s = tempObject(ClassString, EAV);
	    Cell ext;

	    for_cell(ext, (Chain)t->second)
	    { if ( instanceOfObject(ext->value, ClassCharArray) )
	      { if ( s->data.s_size != 0 )
		{ CharArray sep = CtoScratchCharArray(";");
		  str_insert_string(s, DEFAULT, &sep->data);
		  doneScratchCharArray(sep);
		}
		str_insert_string(s, DEFAULT, &((CharArray)ext->value)->data);
	      } else
	      { considerPreserveObject(s);
		return errorPce(ext->value, NAME_unexpectedType, TypeCharArray);
	      }
	    }
	    Name patterns = StringToName(&s->data);
	    ctx->filters[ctx->nfilters].pattern = nameToUTF8(patterns);
	    considerPreserveObject(s);
	  } else
	    return errorPce(cell->value, NAME_unexpectedType, CtoType("char_array|chain"));

	  ctx->nfilters++;
	} else
	  return errorPce(t->first, NAME_unexpectedType, TypeCharArray);
      } else
	return errorPce(cell->value, NAME_unexpectedType, CtoType("tuple"));
    }

    succeed;
  } else if ( isDefault(from ) )
  { succeed;
  } else
  { fail;			/* should not happen */
  }
}



static Name
utf8_to_canonical_file(const char *in)
{
#if O_XOS
  char file[PATH_MAX];
  if ( _xos_canonical_filename(in, file, sizeof(file), 0) )
    return UTF8ToName(file);
  fail;
#else
  return UTF8ToName(in);
#endif
}

static void
open_file_callback(void *udata, const char * const *filelist, int filter)
{ open_file_context *ctx = udata;
  FrameObj fr = ctx->frame;

  if ( !filelist )
  { assign(fr, return_value, CtoString(SDL_GetError())); /* error */
  } else if ( !filelist[0] )
  { assign(fr, return_value, OFF); /* cancelled */
  } else if ( !ctx->allow_many )
  { Name fn = utf8_to_canonical_file(filelist[0]);
    if ( fn )
    { assign(fr, return_value, fn);
    } else
    { assign(fr, return_value, CtoString("File name too long"));
    }
  } else
  { Chain ch = newObject(ClassChain, EAV);
    for( ; *filelist; filelist++ )
    { Name fn = utf8_to_canonical_file(*filelist);
      if ( fn )
      { appendChain(ch, fn);
      } else
      { assign(fr, return_value, CtoString("File name too long"));
	sdl_alert();
	return;
      }
    }
    assign(fr, return_value, ch);
  }

  sdl_alert();
}

static Any
getOpenFileFrame(FrameObj fr, Chain filters,
		 CharArray default_location, BoolObj allow_many)
{ WsFrame wsf = sdl_frame(fr, false);

  if ( wsf && wsf->ws_window )
  { open_file_context ctx = {
      .frame = fr,
      .allow_many = (allow_many == ON)
    };
    if ( !getFileFilters(filters, &ctx) )
      fail;

    char *def = NULL;
#if O_XOS
    char buffer[PATH_MAX];
#endif
    if ( notDefault(default_location) )
    {
#if O_XOS
      if ( !_xos_os_filename(charArrayToUTF8(default_location),
			     buffer, sizeof(buffer)) )
	fail;
      def = buffer;
#else
      def = charArrayToUTF8(default_location);
#endif
    }

    SDL_ShowOpenFileDialog(
      open_file_callback, &ctx,
      wsf->ws_window,
      ctx.nfilters > 0 ? ctx.filters : NULL, ctx.nfilters,
      def,
      allow_many == ON);

    Any rval = getWaitConfirmFrame(fr);
    if ( !rval || rval == OFF )
      fail;
    if ( instanceOfObject(rval, ClassString) )
      return errorPce(fr, NAME_SDLFileDialog, rval),NULL;

    answer(rval);
  }

  return false;			/* error? */
}

static Any
getSaveFileFrame(FrameObj fr, Chain filters, CharArray default_location)
{ WsFrame wsf = sdl_frame(fr, false);

  if ( wsf && wsf->ws_window )
  { open_file_context ctx = {
      .frame = fr,
      .allow_many = false
    };
    if ( !getFileFilters(filters, &ctx) )
      fail;

    SDL_ShowSaveFileDialog(
      open_file_callback, &ctx,
      wsf->ws_window,
      ctx.nfilters > 0 ? ctx.filters : NULL, ctx.nfilters,
      isDefault(default_location) ? NULL : charArrayToUTF8(default_location));

    Any rval = getWaitConfirmFrame(fr);
    if ( !rval || rval == OFF )
      fail;
    if ( instanceOfObject(rval, ClassString) )
      return errorPce(fr, NAME_SDLFileDialog, rval),NULL;

    answer(rval);
  }

  return false;			/* error? */
}



		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_openCentered[] =
        { "center=[point|frame]", "display=[display]", "grab=[bool]" };
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
static char *T_open[] =
        { "position=[point]", "display=[display]", "grab=[bool]" };
static char *T_wmProtocol[] =
        { "protocol=name", "action=code" };
static char *T_convertOldSlot[] =
        { "slot=name", "value=any" };
static char *T_set[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]",
	  "display=[display]" };
static char *T_grab_pointer[] =
	{ "grab=bool", "cursor=[cursor]" };
static char *T_center[] =
	{ "center=[point]", "display=[display]" };
static char *T_geometry[] =
	{ "geometry=name", "display=[display]" };
static char *T_openFile[] =
	{ "filters=[chain]", "default=[char_array]", "allow_many=[bool]" };
static char *T_saveFile[] =
	{ "filters=[chain]", "default=[char_array]" };

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
  SV(NAME_area, "area", IV_GET|IV_STORE, areaFrame,
     NAME_area, "Area of the opened frame on the display"),
  IV(NAME_geometry, "name*", IV_NONE,
     NAME_area, "X-window geometry specification"),
  IV(NAME_placed, "bool", IV_GET,
     NAME_area, "If @on, desired position is set explicitly"),
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
  SM(NAME_display, 1, "display", displayFrame,
     NAME_organisation, "The display of the frame has changed"),
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
  SM(NAME_SdlSet, 5, T_set, SdlSetFrame,
     NAME_area, "Update open frame position and size"),
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
  SM(NAME_SdlSetLabel, 0, NULL, SdlSetLabelFrame,
     NAME_thread, "Update the label of an open frame"),
  SM(NAME_fit, 0, NULL, fitFrame,
     NAME_layout, "Recompute windows and resize the frame"),
  SM(NAME_resize, 0, NULL, resizeFrame,
     NAME_layout, "Recompute layout of sub-windows"),
  SM(NAME_return, 1, "unchecked", returnFrame,
     NAME_modal, "Return after a <-confirm"),
  SM(NAME_create, 0, NULL, createFrame,
     NAME_open, "Establish window-system counterpart"),
  SM(NAME_Create, 0, NULL, createFrame,
     NAME_open, "Establish window-system counterpart (internal)"),
  SM(NAME_mapped, 1, "bool", mappedFrame,
     NAME_open, "Inform transients using ->show"),
  SM(NAME_open, 3, T_open, openFrame,
     NAME_open, "->create and map on the display"),
  SM(NAME_openCentered, 3, T_openCentered, openCenteredFrame,
     NAME_open, "Open centered around point"),
  SM(NAME_uncreate, 0, NULL, uncreateFrame,
     NAME_open, "Destroy window-system counterpart"),
  SM(NAME_wait, 0, NULL, waitFrame,
     NAME_open, "Wait till <-status is `open'"),
  SM(NAME_SdlWait, 0, NULL, SdlWaitFrame,
     NAME_open, "SDL main thread helper for frame->wait"),
  SM(NAME_SdlWaitConfirm, 0, NULL, SdlWaitConfirmFrame,
     NAME_open, "SDL main thread helper for frame<-confirm"),
  SM(NAME_append, 1, "subwindow=window", appendFrame,
     NAME_organisation, "Append a window to the frame"),
  SM(NAME_delete, 1, "member:window", deleteFrame,
     NAME_organisation, "Delete window from the frame"),
  SM(NAME_bell, 1, "volume=[int]", bellFrame,
     NAME_report, "Ring the bell on display"),
  SM(NAME_report, 3, T_report, reportFrame,
     NAME_report, "Report message (send to <-members)"),
  SM(NAME_expose, 0, NULL, exposeFrame,
     NAME_stacking, "Put frame above all others on the display"),
  SM(NAME_SdlExpose, 0, NULL, SdlExposeFrame,
     NAME_stacking, "SDL Thread support message"),
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
  GM(NAME_confirm, 2, "return_value=any", T_open, getConfirmFrame,
     NAME_modal, "Start sub-eventloop until ->return"),
  GM(NAME_confirmCentered, 3, "return_value=any",
     T_openCentered, getConfirmCenteredFrame,
     NAME_modal, "As <-confirm, but centered around point"),
  GM(NAME_catchAll, 1, "window", "window_name=name", getCatchAllFramev,
     NAME_organisation, "Get named window"),
  GM(NAME_frame, 0, "frame", NULL, getFrameFrame,
     NAME_organisation, "Returns itself"),
  GM(NAME_member, 1, "window", "name", getMemberFrame,
     NAME_organisation, "Find member window by name"),
  GM(NAME_members, 0, "chain", NULL, getMembersFrame,
     NAME_organisation, "New chain holding all member windows"),
  GM(NAME_show, 0, "bool", NULL, getShowFrame,
     NAME_visibility, "@on iff <-status = open; @off otherwise"),
  GM(NAME_thread, 0, "int", NULL, getThreadFrame,
     NAME_thread, "Return system thread-id that owns the frame"),
  GM(NAME_openFile, 3, "name|chain", T_openFile, getOpenFileFrame,
     NAME_dialog, "Use OS dialog to prompt for a file for reading"),
  GM(NAME_saveFile, 2, "name", T_saveFile, getSaveFileFrame,
     NAME_dialog, "Use OS dialog to prompt for a file for writing")
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
  RC(NAME_horizontalResizeCursor, "cursor", "ew_resize",
     "Cursor for horizontally resizing tile"),
  RC(NAME_verticalResizeCursor, "cursor", "ns_resize",
     "Cursor for vertically resizing tile"),
  RC(NAME_fitAfterAppend, "bool", "@off",
     "Automatically ->fit the frame after a subwindow was added"),
  RC(NAME_decorateTransient, "bool", "@on",
     "Decorate transient windows (if possible)")
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

  MappedFrames = globalObject(NAME_mappedFrames, ClassChain, EAV);

  succeed;
}
