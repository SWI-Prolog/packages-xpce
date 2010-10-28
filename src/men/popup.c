/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 1985-2005, University of Amsterdam

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
#include <h/dialog.h>

static status closePopup(PopupObj);

static status
initialisePopup(PopupObj p, Name label, Code msg)
{ if ( isDefault(label) )
    label = NAME_options;

  assign(p, update_message, NIL);
  assign(p, button,	    NAME_right);
  assign(p, show_current,   OFF);
  initialiseMenu((Menu) p, label, NAME_popup, msg);
  assign(p, auto_align,	    OFF);

  succeed;
}


		/********************************
		*             WINDOW		*
		********************************/


static Chain windows = NIL;

static PceWindow
createPopupWindow(DisplayObj d)
{ Cell cell;
  PceWindow sw;
  Any frame;

  if ( isNil(windows) )
    windows = globalObject(NAME_PopupWindows, ClassChain, EAV);

  for_cell(cell, windows)
  { sw = cell->value;

    if ( emptyChain(sw->graphicals) && sw->frame->display == d )
      return sw;
  }


  sw = newObject(ClassDialog, NAME_popup, DEFAULT, d, EAV);

  send(sw, NAME_kind, NAME_popup, EAV);
  send(sw, NAME_pen, ZERO, EAV);
  send(sw, NAME_create, EAV);
  frame = get(sw, NAME_frame, EAV);
  send(frame, NAME_border, ONE, EAV);
  send(getTileFrame(frame), NAME_border, ZERO, EAV);

  appendChain(windows, sw);

  return sw;
}


		/********************************
		*            UPDATE		*
		********************************/

static Any updateContext;		/* HACK of pullright menus */

static status
updatePopup(PopupObj p, Any context)
{ updateContext = context;

  if ( notNil(p->update_message) )
    forwardReceiverCode(p->update_message, p, context, EAV);

  return updateMenu((Menu) p, context);
}


static status
resetPopup(PopupObj p)
{ return closePopup(p);
}


static MenuItem
getDefaultMenuItemPopup(PopupObj p)
{ Cell cell;

  if ( isNil(p->default_item) ||
       equalName(p->default_item, NAME_first) )
  { for_cell(cell, p->members)
    { MenuItem mi = cell->value;

      if ( mi->active == ON )
	answer(mi);
    }

    fail;
  }

  if ( equalName(p->default_item, NAME_selection) )
  { for_cell(cell, p->members)
    { MenuItem mi = cell->value;

      if ( mi->selected == ON )
	answer(mi);
    }

    fail;
  }

  answer(findMenuItemMenu((Menu) p, (Any) p->default_item));
}

		/********************************
		*           OPEN/CLOSE		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Show a popup on a graphical.  Pos is the position relative to the graphical
on which to display the popup.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
openPopup(PopupObj p, Graphical gr, Point pos,
	  BoolObj pos_is_pointer, BoolObj warp_pointer, BoolObj ensure_on_display)
{ PceWindow sw;
  int moved = FALSE;			/* Cursor needs be moved */
  int cx, cy;				/* mouse X-Y */
  int px, py;				/* Popup X-Y */
  int pw, ph;				/* Popup W-H */
  int dx, dy;				/* Popup-Pointer offset */
  Point offset;
  DisplayObj d = CurrentDisplay(gr);
  MenuItem mi;
  FrameObj fr, swfr;

  if ( emptyChain(p->members) )
    fail;

  if ( isDefault(pos_is_pointer) )	pos_is_pointer = ON;
  if ( isDefault(warp_pointer) )	warp_pointer = ON;
  if ( isDefault(ensure_on_display) )	ensure_on_display = ON;

  sw = createPopupWindow(d);
  send(sw, NAME_display, p, EAV);

  if ( !(offset = getDisplayPositionGraphical(gr)) )
    return errorPce(p, NAME_graphicalNotDisplayed, gr);

  plusPoint(pos, offset);
  doneObject(offset);

					/* get sizes and coordinates */
  ComputeGraphical((Graphical) p);
  dy = valInt(p->area->y);
  dx = valInt(p->area->x);

  if ( (mi = getDefaultMenuItemPopup(p)) != FAIL )
  { int ix, iy, iw, ih;
    area_menu_item((Menu) p, mi, &ix, &iy, &iw, &ih);
    dy += iy +ih/2;
    dx += ix;
  } else
  { mi = NIL;
    dy += 10;
  }

  if ( notNil(p->default_item) )
  { dx += 2;
    previewMenu((Menu) p, mi);
  } else
  { dx = -4;
    previewMenu((Menu) p, NIL);
  }
  pw = valInt(p->area->w);
  ph = valInt(p->area->h);

  if ( pos_is_pointer == ON )
  { cx = valInt(pos->x);
    cy = valInt(pos->y);
    px = cx - dx;
    py = cy - dy;
  } else
  { px = valInt(pos->x);
    py = valInt(pos->y);
    cx = px + dx;
    cy = py + dy;
    moved = TRUE;
  }

  if ( ensure_on_display == ON )
  { Monitor mon;			/* Monitor displaying gr */
    int mx, my, mw, mh;			/* Monitor area */

    if ( (mon=get(gr, NAME_monitor, EAV)) )
    { mx = valInt(mon->area->x);
      my = valInt(mon->area->y);
      mw = valInt(mon->area->w);
      mh = valInt(mon->area->h);
    } else				/* Or give error? */
    { mx = my = 0;
      mw = valInt(getWidthDisplay(d));
      mh = valInt(getHeightDisplay(d));
    }

    if ( px < mx )         moved = TRUE, px = mx;
    if ( py < my )         moved = TRUE, py = my;
    if ( px + pw > mw+mx ) moved = TRUE, px = mw+mx - pw;
    if ( py + ph > mh+my ) moved = TRUE, py = mh+my - ph;
  }

  swfr = getFrameGraphical((Graphical) sw);
  fr   = getFrameGraphical(gr);
  if ( fr )
    send(swfr, NAME_application, fr->application, EAV);
  send(swfr, NAME_set, toInt(px), toInt(py), toInt(pw), toInt(ph), EAV);
  send(sw, NAME_show, ON, EAV);
  ws_topmost_frame(swfr, ON);
  if ( moved && warp_pointer == ON )
  { Point pos = tempObject(ClassPoint, toInt(dx), toInt(dy), EAV);
    send(sw, NAME_pointer, pos, EAV);
    considerPreserveObject(pos);
  }

  send(sw, NAME_sensitive, ON, EAV);

  succeed;
}


static status
closePopup(PopupObj p)
{ PceWindow sw;

  if ( notNil(p->pullright) )
  { send(p->pullright, NAME_close, EAV);
    assign(p, pullright, NIL);
  }

  if ( notNil(sw = (PceWindow) p->device) )
  { send(sw, NAME_show, OFF, EAV);
    send(sw, NAME_sensitive, OFF, EAV);
    send(sw, NAME_clear, EAV);
    assign(p, displayed, OFF);
  }

  succeed;
}


		/********************************
		*         EVENT HANDLING	*
		********************************/

static status
keyPopup(PopupObj p, Name key)
{ Cell cell;

  for_cell(cell, p->members)
  { MenuItem mi = cell->value;

    if ( (mi->accelerator == key && mi->active == ON) ||
	 (notNil(mi->popup) && keyPopup(mi->popup, key)) )
    { assign(p, selected_item, mi);
      succeed;
    }
  }

  fail;
}


#undef BUSY
#define BUSY(g) { busyCursorDisplay(d, DEFAULT, DEFAULT); \
		  g; \
		  busyCursorDisplay(d, NIL, DEFAULT); \
		}

static status
executePopup(PopupObj p, Any context)
{ DisplayObj d = CurrentDisplay(context);

  if ( p->kind == NAME_cyclePopup )
  { Menu m = context;

    if ( instanceOfObject(m, ClassMenu) )
    { if ( notNil(p->selected_item) )
      { selectionMenu(m, p->selected_item);
	flushGraphical(m);
	BUSY(forwardMenu(m, m->message, EVENT->value));
      }
    } else
      return errorPce(context, NAME_unexpectedType, ClassMenu);
  } else
  { Code def_msg = DEFAULT;

    for( ; instanceOfObject(p, ClassPopup); p = p->selected_item )
    { if ( notDefault(p->message) )
	def_msg = p->message;

      if ( instanceOfObject(p->selected_item, ClassMenuItem) )
      { MenuItem mi = p->selected_item;

	BUSY(if ( p->multiple_selection == ON )
	     { toggleMenu((Menu) p, mi);
	       if ( isDefault(mi->message) )
	       { if ( notDefault(def_msg) && notNil(def_msg) )
		   forwardReceiverCode(def_msg, p,
				       mi->value, mi->selected, context, EAV);
	       } else if ( notNil(mi->message) )
		 forwardReceiverCode(mi->message, p, mi->selected, context, EAV);
	     } else
	     { if ( isDefault(mi->message) )
	       { if ( notDefault(def_msg) && notNil(def_msg) )
		   forwardReceiverCode(def_msg, p, mi->value, context, EAV);
	       } else if ( notNil(mi->message) )
		 forwardReceiverCode(mi->message, p, context, EAV);
	     })

	succeed;
      }
    }
  }

  succeed;
}


static status
showPullrightMenuPopup(PopupObj p, MenuItem mi, EventObj ev, Any context)
{ if ( isDefault(context) && validPceDatum(updateContext) )
    context = updateContext;

  send(mi->popup, NAME_update, context, EAV);

  if ( !emptyChain(mi->popup->members) )
  { Point pos;		/* Create PULLRIGHT */
    int ix, iy, ih, iw;
    int rx;

    area_menu_item((Menu)p, mi, &ix, &iy, &iw, &ih);
    if ( notNil(p->popup_image) )
      rx = ix+iw-valInt(p->popup_image->size->w);
    else
      rx = ix+iw-8;

    previewMenu((Menu) p, mi);
    pos = tempObject(ClassPoint, toInt(rx), toInt(iy), EAV);

    assign(p, pullright, mi->popup);
    assign(p->pullright, default_item, NIL); /* Initialy do not select */
    send(p->pullright, NAME_open, p, pos, OFF, OFF, ON, EAV);
    considerPreserveObject(pos);
    assign(p->pullright, button, p->button);
    if ( notDefault(ev) )
      postEvent(ev, (Graphical) p->pullright, DEFAULT);

    succeed;
  }

  fail;
}


static status
inPullRigthPopup(PopupObj p, MenuItem mi, EventObj ev)
{ Int ex, ey;
  int ix, iy, ih, iw;
  int rx;

  area_menu_item((Menu)p, mi, &ix, &iy, &iw, &ih);
  if ( notNil(p->popup_image) )
    rx = ix+iw-valInt(p->popup_image->size->w);
  else
    rx = ix+iw-8;
  rx -= 2*valInt(p->border);

  get_xy_event(ev, p, ON, &ex, &ey);
  if ( valInt(ex) >= rx )
    succeed;

  fail;
}


static status
dragPopup(PopupObj p, EventObj ev, BoolObj check_pullright)
{ MenuItem mi;

  if ( !(mi = getItemFromEventMenu((Menu) p, ev)) )
    previewMenu((Menu) p, NIL);
  else
  { if ( mi->active == ON )
    { previewMenu((Menu) p, mi);

      if ( notNil(mi->popup) && check_pullright != OFF )
      { if ( inPullRigthPopup(p, mi, ev) )
	  send(p, NAME_showPullrightMenu, mi, ev, EAV);
      }
    } else
      previewMenu((Menu) p, NIL);
  }

  succeed;
}


static status
kbdSelectPopup(PopupObj p, MenuItem mi)
{ if ( notNil(mi->popup) )
  { previewMenu((Menu) p, mi);
    send(p, NAME_showPullrightMenu, mi, EAV);
    previewMenu((Menu)mi->popup, getHeadChain(mi->popup->members));
  } else
  { assign(p, selected_item, mi);
    send(p, NAME_close, EAV);
  }

  succeed;
}


static status
typedPopup(PopupObj p, Any id)
{ int prev;

  if ( id == toInt(13) )			/* RETURN ... */
  { return kbdSelectPopup(p, p->preview);
  } else if ( (prev = (id == NAME_cursorUp)) || /* cursor up/down */
	      id == NAME_cursorDown )
  { MenuItem mi;

    if ( prev )
    { if ( !(mi = getPreviousChain(p->members, p->preview)) )
	mi = getTailChain(p->members);
    } else
    { if ( !(mi = getNextChain(p->members, p->preview)) )
	mi = getHeadChain(p->members);
    }

    if ( mi )
      previewMenu((Menu) p, mi);

    succeed;
  } else
  { Name key = characterName(id);		/* accelerator of item */
    Cell cell;

    for_cell(cell, p->members)
    { MenuItem mi = cell->value;

      if ( mi->accelerator == key )
	return kbdSelectPopup(p, mi);
    }

    send(p, NAME_alert, EAV);
  }

  fail;
}


static status
eventPopup(PopupObj p, EventObj ev)
{ 					/* Showing PULLRIGHT menu */
  if ( notNil(p->pullright) )
  { status rval = postEvent(ev, (Graphical) p->pullright, DEFAULT);

    if ( isDragEvent(ev) )
    { if ( isNil(p->pullright->preview) )
      { MenuItem mi;

	if ( (mi = getItemFromEventMenu((Menu) p, ev)) &&
	     mi->popup != p->pullright )
	{ send(p->pullright, NAME_close, EAV);
	  assign(p, pullright, NIL);
	  return send(p, NAME_drag, ev, EAV);
	}
      }
    } else if ( isAEvent(ev, NAME_locMove) )
    { if ( isNil(p->pullright->preview) )
      { MenuItem mi;

	if ( (mi = getItemFromEventMenu((Menu) p, ev)) &&
	     mi->popup != p->pullright )
	{ send(p->pullright, NAME_close, EAV);
	  assign(p, pullright, NIL);
	  if ( mi->active == ON && notNil(mi->popup) )
	    goto still;
	  else
	    return send(p, NAME_drag, ev, EAV);
	}
      }
    } else if ( ((isUpEvent(ev) &&	/* execute it */
		  getButtonEvent(ev) == p->pullright->button) ||
		 (rval &&
		  isAEvent(ev, NAME_keyboard) &&
		  !isAEvent(ev, NAME_cursor))) &&
		isNil(p->pullright->pullright) )
    { if ( notNil(p->pullright->selected_item) )
    	assign(p, selected_item, p->pullright);
      else
      	assign(p, selected_item, NIL);
      assign(p, pullright, NIL);
      send(p, NAME_close, EAV);
    }

    succeed;
  }

					/* UP: execute */
  if ( isUpEvent(ev) )
  { if ( notNil(p->preview) &&
	 notNil(p->preview->popup) &&
	 valInt(getClickTimeEvent(ev)) < 400 &&
	 valInt(getClickDisplacementEvent(ev)) < 10 )
    { send(p, NAME_showPullrightMenu, p->preview, EAV);
    } else if ( notNil(p->preview) &&
		notNil(p->preview->popup) &&
		!instanceOfObject(p->preview->message, ClassCode) )
    { send(p, NAME_showPullrightMenu, p->preview, EAV);
    } else if ( getButtonEvent(ev) == p->button )
    { assign(p, selected_item, p->preview);
      send(p, NAME_close, EAV);
      succeed;
    }
  } else if ( isDownEvent(ev) )		/* DOWN: set button */
  { assign(p, selected_item, NIL);
    assign(p, button, getButtonEvent(ev));
    send(p, NAME_drag, ev, OFF, EAV);
    succeed;
  } else if ( isDragEvent(ev) )		/* DRAG: highlight entry */
  { send(p, NAME_drag, ev, EAV);
    succeed;
  } else if ( isAEvent(ev, NAME_locMove) )
  { send(p, NAME_drag, ev, EAV);
    succeed;
  } else if ( isAEvent(ev, NAME_locStill) )
  { MenuItem mi;

  still:
    mi = getItemFromEventMenu((Menu) p, ev);

    if ( mi && mi->active == ON && notNil(mi->popup) )
    { previewMenu((Menu) p, mi);
      send(p, NAME_showPullrightMenu, mi, EAV);

      succeed;
    }
  } else if ( isAEvent(ev, NAME_keyboard) )
  { return typedPopup(p, ev->id);
  }

  succeed;				/* accept all events */
}


		/********************************
		*            MENU ITEM		*
		********************************/


static status
endGroupPopup(PopupObj p, BoolObj val)
{ if ( notNil(p->context) )
    return send(p->context, NAME_endGroup, val, EAV);

  fail;
}


static status
appendPopup(PopupObj p, Any obj)
{ if ( obj == NAME_gap )
  { MenuItem tail = getTailChain(p->members);

    if ( tail )
      send(tail, NAME_endGroup, ON, EAV);

    succeed;
  } else
    return appendMenu((Menu)p, obj);
}


status
defaultPopupImages(PopupObj p)
{ if ( p->show_current == ON )
  { if ( p->multiple_selection == ON && p->look == NAME_win )
      assign(p, on_image, NAME_marked);
    else
      assign(p, on_image, MS_MARK_IMAGE);
  } else
    assign(p, on_image, NIL);

  assign(p, off_image, NIL);

  succeed;
}


static status
showCurrentPopup(PopupObj p, BoolObj show)
{ assign(p, show_current, show);

  return defaultPopupImages(p);
}


static status
activePopup(PopupObj p, BoolObj active)
{ if ( instanceOfObject(p->context, ClassMenuBar) )
    send(p->context, NAME_activeMember, p, active, EAV);

  return activeGraphical((Graphical)p, active);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_drag[] =
        { "event", "check_pullright=[bool]" };
static char *T_showPullrightMenu[] =
        { "item=menu_item", "event=[event]", "context=[any]" };
static char *T_initialise[] =
        { "name=[name]", "message=[code]*" };
static char *T_open[] =
        { "on=graphical", "offset=point", "offset_is_pointer=[bool]", "warp=[bool]", "ensure_on_display=[bool]" };

/* Instance Variables */

static vardecl var_popup[] =
{ IV(NAME_context, "any*", IV_BOTH,
     NAME_context, "Invoking context"),
  IV(NAME_updateMessage, "code*", IV_BOTH,
     NAME_active, "Ran just before popup is displayed"),
  IV(NAME_pullright, "popup*", IV_NONE,
     NAME_part, "Currently shown pullright menu"),
  IV(NAME_selectedItem, "menu_item|popup*", IV_GET,
     NAME_selection, "Selected menu-item/sub-popup"),
  IV(NAME_button, "button_name", IV_GET,
     NAME_event, "Name of invoking button"),
  IV(NAME_defaultItem, "{first,selection}|any*", IV_BOTH,
     NAME_appearance, "Initial previewed item"),
  SV(NAME_showCurrent, "bool", IV_GET|IV_STORE, showCurrentPopup,
     NAME_appearance, "If @on, show the currently selected value")
};

/* Send Methods */

static senddecl send_popup[] =
{ SM(NAME_event, 1, "event", eventPopup,
     DEFAULT, "Handle an event"),
  SM(NAME_initialise, 2, T_initialise, initialisePopup,
     DEFAULT, "Create from name and message"),
  SM(NAME_key, 1, "key=name", keyPopup,
     NAME_accelerator, "Set <-selected_item according to accelerator"),
  SM(NAME_update, 1, "context=any", updatePopup,
     NAME_active, "Update entries using context object)"),
  SM(NAME_active, 1, "bool", activePopup,
     NAME_event, "If @off, greyed out and insensitive"),
  SM(NAME_endGroup, 1, "bool", endGroupPopup,
     NAME_appearance, "Pullright: separation line below item in super"),
  SM(NAME_append, 1, "menu_item|{gap}", appendPopup,
     DEFAULT, "Append menu-item or gap (end_group)"),
  SM(NAME_drag, 2, T_drag, dragPopup,
     NAME_event, "Handle a drag event"),
  SM(NAME_showPullrightMenu, 3, T_showPullrightMenu, showPullrightMenuPopup,
     NAME_event, "Show pullright for this item"),
  SM(NAME_execute, 1, "context=[object]*", executePopup,
     NAME_execute, "Execute selected message of item"),
  SM(NAME_close, 0, NULL, closePopup,
     NAME_open, "Finish after ->open"),
  SM(NAME_open, 5, T_open, openPopup,
     NAME_open, "Open on point relative to graphical"),
  SM(NAME_reset, 0, NULL, resetPopup,
     NAME_reset, "Close popup after an abort")
};

/* Get Methods */

#define get_popup NULL
/*
static getdecl get_popup[] =
{
};
*/

/* Resources */

static classvardecl rc_popup[] =
{ RC(NAME_acceleratorFont, "font*", "@nil",
     "Show the accelerators"),
  RC(NAME_border, "int", UXWIN("4", "5"),
     "Default border around items"),
  RC(NAME_cursor, "cursor", "right_ptr",
     "Cursor when popup is active"),
  RC(NAME_defaultItem, "name*", "first",
     "Item to select as default"),
  RC(NAME_feedback, "name", "image",
     "Feedback style"),
  RC(NAME_kind, "name", "popup",
     "Menu kind"),
  RC(NAME_layout, "name", "vertical",
     "Put items below each other"),
  RC(NAME_multipleSelection, "bool", "@off",
     "Can have multiple selection"),
  RC(NAME_offImage, "{marked}|image*", "@nil",
     "Marker for items not in selection"),
  RC(NAME_onImage, "{marked}|image*", "@nil",
     "Marker for items in selection"),
  RC(NAME_pen, "int", "0",
     "Thickness of the drawing-pen"),
  RC(NAME_popupImage, "image*",
     UXWIN("when(@colour_display, @nil, @ol_pullright_image)",
	   "@ms_left_arrow_image"),
     "Marker for items with popup"),
  RC(NAME_previewFeedback, "name", UXWIN("box", "colour"),
     "Feedback on `preview' item"),
  RC(NAME_showLabel, "bool", "@off",
     "Label is visible"),
  RC(NAME_valueWidth, "int", "80",
     "Minimum width in pixels"),
  RC(NAME_itemElevation, RC_REFINE,
     UXWIN("when(@colour_display, 0, @nil)", "0"),
     NULL),
  RC(NAME_elevation, RC_REFINE, "when(@colour_display, 1, @nil)", NULL),
  RC(NAME_labelSuffix, RC_REFINE, "", NULL),
  RC(NAME_previewElevation, RC_REFINE,
     "elevation(preview, 1, hilited)",
     NULL),
  RC(NAME_format, RC_REFINE, "left", NULL),
  RC(NAME_margin, RC_REFINE, "1",    NULL),
  RC(NAME_look,   RC_REFINE,
     UXWIN("when(@colour_display, motif, open_look)",
	   "win"),
     NULL)
};

/* Class Declaration */

static Name popup_termnames[] = { NAME_name, NAME_message };

ClassDecl(popup_decls,
          var_popup, send_popup, get_popup, rc_popup,
          2, popup_termnames,
          "$Rev$");

status
makeClassPopup(Class class)
{ return declareClass(class, &popup_decls);
}

