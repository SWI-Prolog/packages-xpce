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
#include <h/text.h>

static status
initialiseBrowser(Browser b, Name name, Size size, DisplayObj display)
{ ListBrowser lb;
  TileObj t;

  if ( isDefault(size) )
    size = getClassVariableValueObject(b, NAME_size);

  TRY(lb = newObject(ClassListBrowser, DEFAULT, size->w, size->h, EAV) );
  send(lb, NAME_pen, ZERO, EAV);		/* leave to window */
  initialiseWindow((PceWindow)b, name,
		   getSizeGraphical((Graphical) lb), display);
  t = get(b, NAME_tile, EAV);
  assign(t, horShrink,  ZERO);
  assign(t, horStretch, ONE);		/* provide resizer */
  assign(b, list_browser, lb);
  send(lb, NAME_set, ZERO, ZERO, EAV);
  send(b, NAME_display, lb, EAV);
  send(b, NAME_resizeMessage,
       newObject(ClassMessage, lb, NAME_Size, Arg(2), EAV),
       EAV);
  assign(b, keyboard_focus, lb);

  succeed;
}


static status
unlinkBrowser(Browser b)
{ ListBrowser lb = b->list_browser;

  unlinkWindow((PceWindow) b);
  assign(b, list_browser, NIL);
  freeObject(lb);

  succeed;
}

		/********************************
		*           OVERRULE		*
		********************************/

static status
requestGeometryBrowser(Browser b, Int x, Int y, Int w, Int h)
{ ListBrowser lb = b->list_browser;

					/* pushes upto window! */
  return requestGeometryListBrowser(lb, x, y, w, h);
}

static Any
getSelectionBrowser(Browser b)
{ answer(getSelectionListBrowser(b->list_browser));
}

static status
selectionBrowser(Browser b, Any obj)
{ return selectionListBrowser(b->list_browser, obj);
}

static status
selectedBrowser(Browser b, Any obj)
{ return selectedListBrowser(b->list_browser, obj);
}

static status
scrollToBrowser(Browser b, Int line)
{ return scrollToListBrowser(b->list_browser, line);
}


static status
normaliseBrowser(Browser b, Any obj)
{ return normaliseListBrowser(b->list_browser, obj);
}


static status
typedBrowser(Browser b, EventId id, BoolObj delegate)
{ if ( delegate == ON )
    return typedWindow((PceWindow) b, id, delegate);

  return typedListBrowser(b->list_browser, id);
}


static status
clearBrowser(Browser b)
{ return clearDict(b->list_browser->dict);
}


static status
cursorBrowser(Browser b, CursorObj cursor)
{ return send(b->list_browser->image, NAME_cursor, cursor, EAV);
}


static status
popupBrowser(Browser b, PopupObj pop)
{ assign(b->list_browser, popup, pop);

  succeed;
}


static PopupObj
getPopupBrowser(Browser b)
{ answer(b->list_browser->popup);
}


static DictItem
getMemberBrowser(Browser b, Any key)
{ answer(getMemberListBrowser(b->list_browser, key));
}


static Size
getSizeBrowser(Browser b)
{ answer(getSizeListBrowser(b->list_browser));
}


static status
backgroundBrowser(Browser b, Any bg)
{ return backgroundListBrowser(b->list_browser, bg);
}


		/********************************
		*            VISUAL		*
		********************************/

static Chain
getContainsBrowser(Browser b)
{ answer(getContainsListBrowser(b->list_browser));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_typed[] =
        { "event_id", "delegate=[bool]" };
static char *T_initialise[] =
        { "label=[name]", "size=[size]", "display=[display]" };
static char *T_requestGeometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_browser[] =
{ IV(NAME_listBrowser, "list_browser", IV_GET,
     NAME_delegate, "Displayed list_browser")
};

/* Send Methods */

static senddecl send_browser[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseBrowser,
     DEFAULT, "Create from label, size and display"),
  SM(NAME_requestGeometry, 4, T_requestGeometry, requestGeometryBrowser,
     DEFAULT, "Map size to character units"),
  SM(NAME_unlink, 0, NULL, unlinkBrowser,
     DEFAULT, "Delete the list_browser"),
  SM(NAME_typed, 2, T_typed, typedBrowser,
     NAME_accelerator, "Handle typed character"),
  SM(NAME_cursor, 1, "cursor*", cursorBrowser,
     NAME_cursor, "Cursor when in focus of events"),
  SM(NAME_popup, 1, "popup*", popupBrowser,
     NAME_menu, "Associated popup menu"),
  SM(NAME_clear, 0, NULL, clearBrowser,
     NAME_delete, "Delete all items"),
  SM(NAME_normalise, 1, "member:dict_item", normaliseBrowser,
     NAME_scroll, "Ensure (named) item is visible"),
  SM(NAME_scrollTo, 1, "int", scrollToBrowser,
     NAME_scroll, "Scroll to nth-1 item"),
  SM(NAME_selected, 1, "member:dict_item", selectedBrowser,
     NAME_selection, "Test if object is selected"),
  SM(NAME_selection, 1, "member:dict_item|chain*", selectionBrowser,
     NAME_selection, "Set selected items"),
  SM(NAME_background, 1, "[colour|pixmap]", backgroundBrowser,
     NAME_appearance, "Background colour")

};

/* Get Methods */

static getdecl get_browser[] =
{ GM(NAME_contains, 0, "chain", NULL, getContainsBrowser,
     DEFAULT, "The dict object contained"),
  GM(NAME_size, 0, "characters=size", NULL, getSizeBrowser,
     NAME_area, "Size in character units"),
  GM(NAME_member, 1, "dict_item", "any", getMemberBrowser,
     NAME_lookup, "Dict_item with given key value"),
  GM(NAME_popup, 0, "popup*", NULL, getPopupBrowser,
     NAME_menu, "Get popup menu of the list_browser"),
  GM(NAME_selection, 0, "chain|dict_item*", NULL, getSelectionBrowser,
     NAME_selection, "Get selected items")
};

/* Resources */

static classvardecl rc_browser[] =
{ RC(NAME_pen, "int", "0",
     "Pen (done by <-list_browser)"),
  RC(NAME_size, "size", "size(25,10)",
     "Size in `characters x lines'"),
  RC(NAME_background, RC_REFINE, "@_dialog_bg", NULL)
};

/* Class Declaration */

static Name browser_termnames[] = { NAME_name };

ClassDecl(browser_decls,
          var_browser, send_browser, get_browser, rc_browser,
          1, browser_termnames,
          "$Rev$");

status
makeClassBrowser(Class class)
{ declareClass(class, &browser_decls);
  prependDelegateClass(class, NAME_listBrowser);

  succeed;
}

