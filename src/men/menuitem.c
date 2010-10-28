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
#include <h/dialog.h>

static status	labelMenuItem(MenuItem mi, Any label);

static status
initialiseMenuItem(MenuItem m, Any value, Message msg, Any label,
		   BoolObj eg, Code cond, Name acc)
{ if ( isDefault(eg) )
    eg = OFF;
  if ( isDefault(label) && !(label = get(m, NAME_defaultLabel, value, EAV)) )
    return errorPce(m, NAME_noDefaultLabel, value);
  if ( isDefault(cond) )
    cond = NIL;

  assign(m, value,     value);
  assign(m, message,   msg);
  assign(m, label,     label);
  assign(m, font,      DEFAULT);
  assign(m, colour,    DEFAULT);
  assign(m, selected,  OFF);
  assign(m, active,    ON);
  assign(m, condition, cond);
  assign(m, end_group, eg);

  if ( notDefault(acc) )
    assign(m, accelerator, acc);

  return labelMenuItem(m, label);
}


static status
unlinkMenuItem(MenuItem mi)
{ if ( notNil(mi->menu) )
    deleteMenu(mi->menu, mi);

  succeed;
}


static MenuItem
getConvertMenuItem(Class class, Any value)
{ if ( instanceOfObject(value, ClassPopup) )
  { PopupObj popup = value;
    MenuItem mi = newObject(ClassMenuItem, popup->name, EAV);

    assign(mi, popup, popup);
    assign(popup, context, mi);

    answer(mi);
  }

  answer(newObject(ClassMenuItem, value, EAV));
}


		/********************************
		*      CHANGING ATTRIBUTES	*
		********************************/

static status
changedMenuItem(MenuItem mi)
{ if ( notNil(mi->menu) )
    return qadSendv(mi->menu, NAME_ChangedItem, 1, (Any *) &mi);

  succeed;
}


static status
labelMenuItem(MenuItem mi, Any label)
{ if ( mi->label != label )
  { assign(mi, label, label);
    if ( notNil(mi->menu) )
      requestComputeGraphical(mi->menu, DEFAULT); /* may change layout */
    changedMenuItem(mi);
  }

  succeed;
}


static status
valueMenuItem(MenuItem mi, Any value, Any label)
{ if ( isDefault(label) && !(label = get(mi, NAME_defaultLabel, value, EAV)) )
    return errorPce(mi, NAME_noDefaultLabel, value);

  assign(mi, value, value);
  labelMenuItem(mi, label);

  succeed;
}


static status
fontMenuItem(MenuItem mi, FontObj font)
{ if ( mi->font != font )
  { assign(mi, font, font);
    changedMenuItem(mi);
  }

  succeed;
}


static status
colourMenuItem(MenuItem mi, Colour colour)
{ if ( mi->colour != colour )
  { assign(mi, colour, colour);
    changedMenuItem(mi);
  }

  succeed;
}


static status
backgroundMenuItem(MenuItem mi, Colour colour)
{ if ( mi->background != colour )
  { assign(mi, background, colour);
    changedMenuItem(mi);
  }

  succeed;
}


static status
activeMenuItem(MenuItem mi, BoolObj val)
{ if ( mi->active != val )
  { assign(mi, active, val);
    changedMenuItem(mi);
  }

  succeed;
}


status
selectedMenuItem(MenuItem mi, BoolObj val)
{ if ( mi->selected != val )
  { assign(mi, selected, val);
    changedMenuItem(mi);
  }

  succeed;
}


static status
endGroupMenuItem(MenuItem mi, BoolObj val)
{ if ( mi->end_group != val )
  { assign(mi, end_group, val);
    changedMenuItem(mi);
  }

  succeed;
}


static status
popupMenuItem(MenuItem mi, PopupObj p)
{ if ( mi->popup != p )
  { if ( isNil(p) || (isNil(mi->popup) && notNil(mi->menu)) )
      requestComputeGraphical(mi->menu, DEFAULT); /* HACK */
    assign(mi, popup, p);
    changedMenuItem(mi);
  }

  succeed;
}


static status
onMenuItem(MenuItem mi)
{ return activeMenuItem(mi, ON);
}


static status
offMenuItem(MenuItem mi)
{ return activeMenuItem(mi, OFF);
}


		/********************************
		*            MESSAGES		*
		********************************/

static status
messageMenuItem(MenuItem mi, Code msg)
{ assign(mi, message, msg);
  if ( notNil(mi->popup) )
    assign(mi->popup, message, msg);

  succeed;
}


static Code
getMessageMenuItem(MenuItem mi)
{ answer(notNil(mi->popup) ? mi->popup->message
			   : mi->message);
}


status
hasValueMenuItem(MenuItem mi, Any value)
{ string s1, s2;

  if ( mi->value == value )
    succeed;

  if ( toString(mi->value, &s1) &&
       toString(value, &s2) &&
       str_eq(&s1, &s2) )
    succeed;

  fail;
}



static Any
getDefaultLabelMenuItem(MenuItem m, Any value)
{ Name name;
  Graphical gr;

  if ( (gr = checkType(value, nameToType(NAME_graphical), m)) )
  { Image image = answerObject(ClassImage, NIL,
			       getAreaGraphical(gr)->w,
			       getAreaGraphical(gr)->h, EAV);
    Point p = tempObject(ClassPoint, EAV);
    TRY( send(image, NAME_drawIn, gr, p, EAV) );
    considerPreserveObject(p);

    answer(image);
  }

  if ( (name = checkType(value, TypeName, m)) )
    answer(GetLabelNameName(name));

  if ( isObject(value) && (name = get(value, NAME_name, EAV)) )
    answer(GetLabelNameName(name));

  answer((Any) CtoName(pp(value)));
}


static CharArray
getPrintNameMenuItem(MenuItem mi)
{ return getv(mi->value, NAME_printName, 0, NULL);
}


		/********************************
		*             VISUAL		*
		********************************/

static Menu
getContainedInMenuItem(MenuItem mi)
{ answer(mi->menu);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_value[] =
        { "value=any", "label=[name|image]" };
static char *T_initialise[] =
        { "value=any", "message=[code]*", "label=[name|image]", "end_group=[bool]", "condition=[code]*", "accelerator=[name]" };

/* Instance Variables */

static vardecl var_menuItem[] =
{ IV(NAME_menu, "menu*", IV_GET,
     NAME_organisation, "Menu I'm part of"),
  IV(NAME_value, "any", IV_GET,
     NAME_value, "Value of the item"),
  SV(NAME_message, "[code]*", IV_BOTH|IV_STORE, messageMenuItem,
     NAME_action, "Message sent when selected"),
  SV(NAME_label, "[name|image]", IV_GET|IV_STORE, labelMenuItem,
     NAME_appearance, "Image or string displayed"),
  SV(NAME_font, "[font]", IV_GET|IV_STORE, fontMenuItem,
     NAME_appearance, "Font for label"),
  SV(NAME_colour, "[colour]", IV_GET|IV_STORE, colourMenuItem,
     NAME_appearance, "Colour for label"),
  SV(NAME_background, "[colour]", IV_GET|IV_STORE, backgroundMenuItem,
     NAME_appearance, "Background for label"),
  SV(NAME_selected, "bool", IV_GET|IV_STORE, selectedMenuItem,
     NAME_selection, "Member of menu-selection"),
  SV(NAME_active, "bool", IV_GET|IV_STORE, activeMenuItem,
     NAME_active, "Can be selected by user"),
  IV(NAME_condition, "code*", IV_BOTH,
     NAME_active, "If true, item becomes active"),
  SV(NAME_endGroup, "bool", IV_GET|IV_STORE, endGroupMenuItem,
     NAME_group, "Popup: add separation-line"),
  SV(NAME_popup, "popup*", IV_GET|IV_STORE, popupMenuItem,
     NAME_menu, "Associated popup (pull-right)"),
  IV(NAME_accelerator, "name*", IV_BOTH,
     NAME_accelerator, "Activate when ->key: name is received")
};

/* Send Methods */

static senddecl send_menuItem[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseMenuItem,
     DEFAULT, "Create from value, message, label, end and cond"),
  SM(NAME_unlink, 0, NULL, unlinkMenuItem,
     DEFAULT, "Unlink from menu"),
  SM(NAME_value, 2, T_value, valueMenuItem,
     DEFAULT, "Set value and recompute label"),
  SM(NAME_off, 0, NULL, offMenuItem,
     NAME_active, "Deactivate item"),
  SM(NAME_on, 0, NULL, onMenuItem,
     NAME_active, "Activate item")
};

/* Get Methods */

static getdecl get_menuItem[] =
{ GM(NAME_containedIn, 0, "menu", NULL, getContainedInMenuItem,
     DEFAULT, "Menu I'm contained in"),
  GM(NAME_convert, 1, "menu_item", "value=any", getConvertMenuItem,
     DEFAULT, "Convert value"),
  GM(NAME_printName, 0, "char_array", NULL, getPrintNameMenuItem,
     DEFAULT, "<-print_name of <-value"),
  GM(NAME_message, 0, "message=[code]*", NULL, getMessageMenuItem,
     NAME_action, "Message that will be executed"),
  GM(NAME_defaultLabel, 1, "label=name|image", "value=any", getDefaultLabelMenuItem,
     NAME_label, "Compute default label from value")
};

/* Resources */

#define rc_menuItem NULL
/*
static classvardecl rc_menuItem[] =
{
};
*/

/* Class Declaration */

static Name menuItem_termnames[] = { NAME_value, NAME_message, NAME_label, NAME_endGroup, NAME_condition, NAME_accelerator };

ClassDecl(menuItem_decls,
          var_menuItem, send_menuItem, get_menuItem, rc_menuItem,
          6, menuItem_termnames,
          "$Rev$");


status
makeClassMenuItem(Class class)
{ return declareClass(class, &menuItem_decls);
}

