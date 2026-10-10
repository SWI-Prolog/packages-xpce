/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2005, University of Amsterdam
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
#include <stdbool.h>

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



static PceWindow
createPopupWindow(DisplayObj d)
{ PceWindow sw;
  Any frame;


  sw = newObject(ClassDialog, NAME_popup, DEFAULT, d, EAV);

  send(sw, NAME_kind, NAME_popup, EAV);
  send(sw, NAME_pen, ZERO, EAV);
  send(sw, NAME_gap, newObject(ClassSize, ZERO, ZERO, EAV), EAV);
  frame = get(sw, NAME_frame, EAV);
  send(getTileFrame(frame), NAME_border, ZERO, EAV);


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
		*           SHADOW		*
		********************************/

/* The drop shadow of a popup (class variable `shadow') or NULL.  It is
 * drawn in the window of the popup, around the popup itself, so it is
 * only there if the window can be transparent (see ws_rounded_popups()).
 * `fr' is used to find that out if it is not yet known.
 */

Shadow
popupShadow(PopupObj p, Any fr)
{ Any val = getClassVariableValueObject(p, NAME_shadow);
  Shadow s;

  if ( val && (s = toShadow(val)) && ws_rounded_popups(fr) )
    return s;

  return NULL;
}


/* Room around the popup in its window for the drop shadow */

int
popupShadowMargin(PopupObj p, Any fr)
{ Shadow s = popupShadow(p, fr);

  return s ? extentShadow(s) : 0;
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
	  BoolObj pos_is_pointer, BoolObj warp_pointer,
	  BoolObj ensure_on_display)
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

  fr = getFrameGraphical(gr);
  int m = popupShadowMargin(p, fr);	/* room for the shadow */

  sw = createPopupWindow(d);
  if ( m )				/* keep the room right and below */
    send(sw, NAME_border, tempObject(ClassSize, toInt(m), toInt(m), EAV), EAV);
  send(sw, NAME_display, p, tempObject(ClassPoint, toInt(m), toInt(m), EAV),
       EAV);

  offset = getFramePositionGraphical(gr);
  if ( !offset )
    return errorPce(p, NAME_graphicalNotDisplayed, gr);

  DEBUG(NAME_popup,
	Cprintf("Show %s on %s at %d,%d offset = %d,%d\n",
		pp(p), pp(gr), valInt(pos->x), valInt(pos->y),
		valInt(offset->x), valInt(offset->y)));

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
  pw = valInt(p->area->w) + 2*m;
  ph = valInt(p->area->h) + 2*m;

  if ( pos_is_pointer == ON )		/* dx,dy include the margin */
  { cx = valInt(pos->x);
    cy = valInt(pos->y);
    px = cx - dx;
    py = cy - dy;
  } else
  { px = valInt(pos->x) - m;		/* the popup itself at pos */
    py = valInt(pos->y) - m;
    cx = px + dx;
    cy = py + dy;
    moved = TRUE;
  }

  swfr = getFrameGraphical((Graphical) sw);
  if ( fr )
  { send(swfr, NAME_application, fr->application, EAV);
    attributeObject(swfr, NAME_parent, fr);
  }
  send(swfr, NAME_set, toInt(px), toInt(py), toInt(pw), toInt(ph), EAV);
  send(swfr, NAME_show, ON, EAV);
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
{ deleteAttributeObject(p, NAME_keyboard); /* see accelerator_key() */
  if ( notNil(p->pullright) )
  { send(p->pullright, NAME_close, EAV);
    assign(p, pullright, NIL);
  }

  FrameObj fr = getFrameGraphical((Graphical)p);
  if ( fr )
  { if ( notNil(p->device) )
    { eraseDevice(p->device, (Graphical)p);
      assign(p, displayed, OFF);
    }
    send(fr, NAME_destroy, EAV);
  }

  succeed;
}


		/********************************
		*         EVENT HANDLING	*
		********************************/

/* True if `key` selects item mi: its mnemonic, Alt-<letter>, or, in an
 * open popup (`plain`), just the letter, as on Windows and Gnome.
 */

static bool
mnemonic_key(MenuItem mi, Name key, bool plain)
{ const char *m, *k;

  if ( mi->active != ON || !isName(mi->mnemonic) )
    return false;
  if ( mi->mnemonic == key )
    return true;

  return ( plain && isName(key) &&
	   (m = strName(mi->mnemonic)) && (k = strName(key)) &&
	   m[0] == '\\' && m[1] == 'e' && m[2] && !m[3] &&
	   k[0] && !k[1] &&
	   tolower((unsigned char)k[0]) == (unsigned char)m[2] );
}


static status
keyPopup(PopupObj p, Name key)
{ Cell cell;

  for_cell(cell, p->members)
  { MenuItem mi = cell->value;

    if ( mnemonic_key(mi, key, false) ||
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
  Code def_msg = DEFAULT;

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
    rx = ix+iw-popup_indicator_width((Menu)p, mi);

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
  rx = ix+iw-popup_indicator_width((Menu)p, mi);
  rx -= 2*valInt(p->border);

  if ( !get_xy_event(ev, p, ON, &ex, &ey) )
    fail;
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
    attributeObject(mi->popup, NAME_keyboard, ON);
    send(p, NAME_showPullrightMenu, mi, EAV);
    previewMenu((Menu)mi->popup, getHeadChain(mi->popup->members));
  } else
  { assign(p, selected_item, mi);
    send(p, NAME_close, EAV);
  }

  succeed;
}


/* True if the highlighted item of p opens a submenu.
 */

bool
previewHasSubmenuPopup(PopupObj p)
{ return ( notNil(p->preview) && p->preview->active == ON &&
	   notNil(p->preview->popup) );
}


static status
typedPopup(PopupObj p, Any ev)
{ Any id = (instanceOfObject(ev, ClassEvent) ? ((EventObj)ev)->id : ev);
  int prev;

  if ( id == toInt(13) || id == NAME_RET ) /* RETURN ... */
  { if ( isNil(p->preview) )
      fail;
    return kbdSelectPopup(p, p->preview);
  } else if ( id == NAME_cursorRight || id == NAME_cursorLeft )
  { if ( id == NAME_cursorRight && previewHasSubmenuPopup(p) )
      return kbdSelectPopup(p, p->preview); /* open the submenu */
    fail;				/* see eventPopup() and menu_bar */
  } else if ( id == toInt(27) || id == NAME_ESC )
  { assign(p, selected_item, NIL);	/* ESC: close without selection */
    send(p, NAME_close, EAV);
    succeed;
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
  { Name key = characterName(ev);		/* mnemonic of item */
    Cell cell;

    for_cell(cell, p->members)
    { MenuItem mi = cell->value;

      if ( mnemonic_key(mi, key, true) )
	return kbdSelectPopup(p, mi);
    }

    send(p, NAME_alert, EAV);
  }

  fail;
}


#define WindowOfEvent(ev) ((PceWindow)(ev)->window)

static status
eventPopup(PopupObj p, EventObj ev)
{					/* Showing PULLRIGHT menu */
  DEBUG(NAME_popup,
	Cprintf("eventPopup: %s at %s,%s\n",
		pp(ev->id), pp(ev->x), pp(ev->y)));

  if ( notNil(p->pullright) )
  { status rval;

    if ( isNil(p->pullright->pullright) && /* Left, Escape: close the */
	 (ev->id == NAME_cursorLeft ||	   /* innermost submenu */
	  ev->id == NAME_ESC || ev->id == toInt(27)) )
    { send(p->pullright, NAME_close, EAV);
      assign(p, pullright, NIL);
      succeed;
    }

    rval = postEvent(ev, (Graphical) p->pullright, DEFAULT);

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
      DEBUG(NAME_popup,
	    Cprintf("Selected %s; context = %s\n",
		    pp(p->preview), pp(p->context)));
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
  { if ( !getAttributeObject(p, NAME_keyboard) )
    { attributeObject(p, NAME_keyboard, ON); /* show the mnemonics, */
      changedDialogItem(p);		     /* see accelerator_key() */
    }
    return typedPopup(p, ev);
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
  { assign(p, on_image, NAME_marked);	/* a check mark */
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
  RC(NAME_cursor, "cursor", "arrow",
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
  RC(NAME_pen, "0..", "0",
     "Thickness of the drawing-pen"),
  RC(NAME_previewFeedback, "name", "colour",
     "Feedback on `preview' item"),
  RC(NAME_showLabel, "bool", "@off",
     "Label is visible"),
  RC(NAME_valueWidth, "int", "80",
     "Minimum width in pixels"),
  RC(NAME_radius, "0..", UXWINMAC("6", "8", "10"),
     "Radius of the corners"),
  RC(NAME_borderColour, "colour", "ui_separator",
     "Colour of the outline"),
  RC(NAME_shadow, "shadow*",
     UXWINMAC("shadow(0, 4, 12, colour(@default, 0, 0, 0, 70))",
	      "shadow(0, 4, 12, colour(@default, 0, 0, 0, 70))",
	      "@nil"),
     "Drop shadow (needs a compositor; MacOS adds its own)"),
  RC(NAME_labelSuffix, RC_REFINE, "", NULL),
  RC(NAME_format, RC_REFINE, "left", NULL),
  RC(NAME_margin, RC_REFINE, "1",    NULL),
  RC(NAME_look,   RC_REFINE,
     UXWIN("xpce", "win"),
     NULL)
};

/* Class Declaration */

static Name popup_termnames[] = { NAME_name, NAME_message };

ClassDecl(popup_decls,
          var_popup, send_popup, get_popup, rc_popup,
          2, popup_termnames);

status
makeClassPopup(Class class)
{ realiseClass(ClassImage);
  return declareClass(class, &popup_decls);
}
