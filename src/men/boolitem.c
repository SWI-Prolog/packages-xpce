/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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


/* Class bool_item is a dialog item for a boolean value that looks like
 * the switches of modern desktops: a knob that slides in a rounded
 * track, which changes colour with the value.  Changing the value
 * animates the knob using a timer.
 */

#include <h/kernel.h>
#include <h/dialog.h>

static status	displayedValueBoolItem(BoolItem b, BoolObj val);
static status	applyBoolItem(BoolItem b, BoolObj always);
static status	restoreBoolItem(BoolItem b);

#define KNOB_STEP   25			/* % of the travel per animation step */
#define KNOB_TICK   0.016		/* seconds between animation steps */

static status
initialiseBoolItem(BoolItem b, Name name, Any def, Code msg)
{ if ( isDefault(name) )
    name = NAME_value;

  createDialogItem(b, name);

  assign(b, show_label,    ON);
  assign(b, message,	   msg);
  assign(b, timer,	   NIL);
  assign(b, default_value, isDefault(def) ? OFF : def);
  if ( !restoreBoolItem(b) )
  { assign(b, selection, OFF);
    displayedValueBoolItem(b, OFF);
  }
  assign(b, knob_position, b->displayed_value == ON ? toInt(100) : ZERO);

  return requestComputeGraphical(b, DEFAULT);
}


/* Stop the timer while we still refer to it.  Dropping our reference
 * frees it if nothing else refers to it.
 */

static status
unlinkBoolItem(BoolItem b)
{ if ( notNil(b->timer) )
  { stopTimer(b->timer);
    assign(b, timer, NIL);
  }

  return unlinkDialogItem((DialogItem) b);
}


		/********************************
		*            GEOMETRY		*
		********************************/

static void
compute_label_bool_item(BoolItem b, int *lw, int *lh)
{ if ( b->show_label == ON )
  { if ( isDefault(b->label_font) )
      obtainClassVariablesObject(b);

    dia_label_size(b, lw, lh, NULL);
    *lw += valInt(getAvgCharWidthFont(b->label_font));
    if ( notDefault(b->label_width) )
      *lw = max(valInt(b->label_width), *lw);
  } else
  { *lw = *lh = 0;
  }
}


/* Compute the layout: the label at (0,ly), the track at (sx,sy) of
 * size sw x sh.  The track size is scaled for the display resolution.
 * The track has a margin fm around it for the focus ring.
 */

#define knob_pad(sh)	 max(2, (sh)/10)
#define focus_margin(sh) (knob_pad(sh)+2)

static void
compute_bool_item(BoolItem b,
		  int *ly, int *sx, int *sy, int *sw, int *sh)
{ int lw, lh, hm, fm;

  obtainClassVariablesObject(b);
  compute_label_bool_item(b, &lw, &lh);
  if ( lh == 0 )			/* no label: lay out as if there */
    lh = valInt(getHeightFont(b->label_font)); /* is one, so the */
					/* reference is a text baseline */
  *sw = valInt(b->switch_size->w);
  *sh = valInt(b->switch_size->h);
  fm  = focus_margin(*sh);
  hm  = max(lh, *sh+2*fm);

  *sx = lw + fm;
  *ly = (hm - lh) / 2;
  *sy = (hm - *sh) / 2;
}


static status
computeBoolItem(BoolItem b)
{ if ( notNil(b->request_compute) )
  { int ly, sx, sy, sw, sh;
    int lw, lh;

    compute_bool_item(b, &ly, &sx, &sy, &sw, &sh);
    compute_label_bool_item(b, &lw, &lh);

    { int fm = focus_margin(sh);

      CHANGING_GRAPHICAL(b,
	    assign(b->area, w, toInt(sx + sw + fm));
	    assign(b->area, h, toInt(max(lh, sh+2*fm))));
    }

    assign(b, request_compute, NIL);
  }

  succeed;
}


static Point
getReferenceBoolItem(BoolItem b)
{ Point ref;

  if ( !(ref = getReferenceDialogItem(b)) )
  { int ly, sx, sy, sw, sh;
    int ascent;

    ComputeGraphical(b);
    compute_bool_item(b, &ly, &sx, &sy, &sw, &sh);
    ascent = valInt(getAscentFont(b->label_font));

    ref = answerObject(ClassPoint, ZERO, toInt(ascent + ly), EAV);
  }

  answer(ref);
}


static Int
getLabelWidthBoolItem(BoolItem b)
{ int lw, lh;

  compute_label_bool_item(b, &lw, &lh);
  answer(toInt(lw));
}


static status
labelWidthBoolItem(BoolItem b, Int w)
{ if ( b->show_label == ON && b->label_width != w )
  { assign(b, label_width, w);
    CHANGING_GRAPHICAL(b,
	requestComputeGraphical(b, DEFAULT));
  }

  succeed;
}


		/********************************
		*            REDRAW		*
		********************************/

static status
RedrawAreaBoolItem(BoolItem b, Area a)
{ int x, y, w, h;
  int ly, sx, sy, sw, sh;
  int pos = valInt(b->knob_position);
  int lflags = (b->active == ON ? 0 : LABEL_INACTIVE);

  initialiseDeviceGraphical(b, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);
  compute_bool_item(b, &ly, &sx, &sy, &sw, &sh);
  r_clear(x, y, w, h);

  if ( b->show_label == ON )
  { int ex = valInt(getAvgCharWidthFont(b->label_font));

    RedrawLabelDialogItem(b,
			  accelerator_code(b->accelerator),
			  x, y+ly, sx-focus_margin(sh)-ex, 0,
			  b->label_format, NAME_top,
			  lflags);
  }

  { int tx  = x+sx, ty = y+sy;
    int pad = knob_pad(sh);
    int d   = sh - 2*pad;
    int kx  = tx + pad + ((sw - sh) * pos) / 100;
    Any track = (pos >= 50 ? b->on_colour : b->off_colour);
    Any old;

    if ( b->active == OFF )		/* faded, as the label */
      r_push_group();
    r_thickness(0);
    r_dash(NAME_none);
    r_pill(tx, ty, sw, sh, track);
    r_arc(kx, ty+pad, d, d, 0, 360, NAME_none, b->knob_colour);
    if ( b->active == OFF )
      r_pop_group_with_alpha(INACTIVE_ALPHA);

    if ( hasInputFocusDialogItem(b) )
    { int m = focus_margin(sh)-1;	/* a gap, so it shows on the track */

      old = r_colour(b->on_colour);
      r_thickness(1);
      r_pill_outline(tx-m, ty-m, sw+2*m, sh+2*m);
      r_colour(old);
    }
    r_thickness(1);
  }

  return RedrawAreaGraphical(b, a);
}


		/********************************
		*           ANIMATION		*
		********************************/

static int
knob_target(BoolItem b)
{ return b->displayed_value == ON ? 100 : 0;
}


static status
animateBoolItem(BoolItem b)
{ int pos = valInt(b->knob_position);
  int target = knob_target(b);

  if ( pos < target )
    pos = min(target, pos + KNOB_STEP);
  else if ( pos > target )
    pos = max(target, pos - KNOB_STEP);

  assign(b, knob_position, toInt(pos));
  changedDialogItem(b);

  if ( pos == target && notNil(b->timer) )
    stopTimer(b->timer);

  succeed;
}


/* Move the knob to its target.  Animate only if we are displayed,
 * i.e., the user can see it move.
 */

static void
move_knob(BoolItem b)
{ if ( valInt(b->knob_position) == knob_target(b) )
    return;

  if ( getIsDisplayedGraphical((Graphical)b, DEFAULT) == ON )
  { if ( isNil(b->timer) )
      assign(b, timer,
	     newObject(ClassTimer, toNum(KNOB_TICK),
		       newObject(ClassMessage, b, NAME_animate, EAV),
		       EAV));
    startTimer(b->timer, NAME_repeat, DEFAULT);
  } else
  { assign(b, knob_position, toInt(knob_target(b)));
    changedDialogItem(b);
  }
}


		/********************************
		*        EVENT HANDLING		*
		********************************/

static status
WantsKeyboardFocusBoolItem(BoolItem b)
{ return b->active == ON;
}


static status
toggleBoolItem(BoolItem b)
{ displayedValueBoolItem(b, b->displayed_value == ON ? OFF : ON);

  if ( !send(b->device, NAME_modifiedItem, b, ON, EAV) )
    applyBoolItem(b, ON);

  succeed;
}


/* As a check box on Windows and Gnome, the accelerator focusses and
 * toggles the item.
 */

static status
keyBoolItem(BoolItem b, Name key)
{ if ( b->active == ON && isName(b->accelerator) && b->accelerator == key )
  { send(b, NAME_keyboardFocus, ON, EAV);
    return toggleBoolItem(b);
  }

  fail;
}


static status
eventBoolItem(BoolItem b, EventObj ev)
{ if ( eventDialogItem(b, ev) )
    succeed;

  if ( b->active == ON )
  { int infocus = (getKeyboardFocusGraphical((Graphical) b) == ON);

    if ( isAEvent(ev, NAME_focus) )
    { changedDialogItem(b);
      succeed;
    }

    if ( isAEvent(ev, NAME_msLeftDown) )
    { if ( !infocus )
	send(b, NAME_keyboardFocus, ON, EAV);
      succeed;
    }

    if ( isAEvent(ev, NAME_msLeftUp) )
      return toggleBoolItem(b);

    if ( infocus && (ev->id == toInt(' ') || ev->id == toInt(13)) )
      return toggleBoolItem(b);
  }

  fail;
}


		/********************************
		*          ATTRIBUTES		*
		********************************/

static status
displayedValueBoolItem(BoolItem b, BoolObj val)
{ if ( b->displayed_value != val )
  { assign(b, displayed_value, val);
    move_knob(b);
  }

  succeed;
}


static status
showLabelBoolItem(BoolItem b, BoolObj val)
{ return assignGraphical(b, NAME_showLabel, val);
}


static status
switchSizeBoolItem(BoolItem b, Size sz)
{ return assignGraphical(b, NAME_switchSize, sz);
}


static status
onColourBoolItem(BoolItem b, Colour c)
{ return assignGraphical(b, NAME_onColour, c);
}


static status
offColourBoolItem(BoolItem b, Colour c)
{ return assignGraphical(b, NAME_offColour, c);
}


static status
knobColourBoolItem(BoolItem b, Colour c)
{ return assignGraphical(b, NAME_knobColour, c);
}


		/********************************
		*         COMMUNICATION		*
		********************************/

static status
selectionBoolItem(BoolItem b, BoolObj val)
{ assign(b, selection, val);

  return displayedValueBoolItem(b, val);
}


static BoolObj
getSelectionBoolItem(BoolItem b)
{ assign(b, selection, b->displayed_value);

  answer(b->selection);
}


static BoolObj
getModifiedBoolItem(BoolItem b)
{ answer(b->selection == b->displayed_value ? OFF : ON);
}


static status
modifiedBoolItem(BoolItem b, BoolObj val)
{ if ( val == OFF )
    displayedValueBoolItem(b, b->selection);

  succeed;
}


static BoolObj
getDefaultBoolItem(BoolItem b)
{ answer(checkType(b->default_value, TypeBool, b));
}


static status
defaultBoolItem(BoolItem b, Any val)
{ if ( b->default_value != val )
  { assign(b, default_value, val);

    return restoreBoolItem(b);
  }

  succeed;
}


static status
restoreBoolItem(BoolItem b)
{ BoolObj val;

  if ( (val = getDefaultBoolItem(b)) )
    return selectionBoolItem(b, val);

  fail;
}


static status
applyBoolItem(BoolItem b, BoolObj always)
{ BoolObj val;

  if ( instanceOfObject(b->message, ClassCode) &&
       (always == ON || getModifiedBoolItem(b) == ON) &&
       (val = getSelectionBoolItem(b)) )
  { forwardReceiverCode(b->message, b, val, EAV);
    succeed;
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=[name]", "selection=[bool|function]", "message=[code]*" };

/* Instance Variables */

static vardecl var_bool_item[] =
{ SV(NAME_selection, "bool", IV_GET|IV_STORE, selectionBoolItem,
     NAME_selection, "Current selection"),
  IV(NAME_default, "bool|function", IV_NONE,
     NAME_apply, "The default selection or function to get it"),
  SV(NAME_displayedValue, "bool", IV_GET|IV_STORE, displayedValueBoolItem,
     NAME_selection, "Currently displayed value"),
  SV(NAME_showLabel, "bool", IV_GET|IV_STORE, showLabelBoolItem,
     NAME_appearance, "Whether label is shown"),
  SV(NAME_switchSize, "size", IV_GET|IV_STORE, switchSizeBoolItem,
     NAME_appearance, "Size of the track (before scaling)"),
  SV(NAME_onColour, "colour", IV_GET|IV_STORE, onColourBoolItem,
     NAME_appearance, "Colour of the track if @on"),
  SV(NAME_offColour, "colour", IV_GET|IV_STORE, offColourBoolItem,
     NAME_appearance, "Colour of the track if @off"),
  SV(NAME_knobColour, "colour", IV_GET|IV_STORE, knobColourBoolItem,
     NAME_appearance, "Colour of the knob"),
  IV(NAME_knobPosition, "0..100", IV_GET,
     NAME_appearance, "Position of the knob in % of its travel"),
  IV(NAME_timer, "timer*", IV_NONE,
     NAME_appearance, "Timer that animates the knob")
};

/* Send Methods */

static senddecl send_bool_item[] =
{ SM(NAME_compute, 0, NULL, computeBoolItem,
     DEFAULT, "Compute desired size"),
  SM(NAME_event, 1, "event", eventBoolItem,
     DEFAULT, "Process an event"),
  SM(NAME_initialise, 3, T_initialise, initialiseBoolItem,
     DEFAULT, "Create from label, default and message"),
  SM(NAME_unlink, 0, NULL, unlinkBoolItem,
     DEFAULT, "Stop and destroy the animation timer"),
  SM(NAME_key, 1, "key=name", keyBoolItem,
     NAME_accelerator, "Toggle if key is my accelerator"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, WantsKeyboardFocusBoolItem,
     NAME_event, "Test if ready to accept input (active)"),
  SM(NAME_animate, 0, NULL, animateBoolItem,
     NAME_appearance, "Move the knob a step towards its position"),
  SM(NAME_toggle, 0, NULL, toggleBoolItem,
     NAME_selection, "Invert the value as if clicked"),
  SM(NAME_apply, 1, "always=[bool]", applyBoolItem,
     NAME_apply, "->execute if <-modified or @on"),
  SM(NAME_default, 1, "value=bool|function", defaultBoolItem,
     NAME_apply, "Set variable -default and ->selection"),
  SM(NAME_modified, 1, "bool", modifiedBoolItem,
     NAME_apply, "Reset modified flag"),
  SM(NAME_restore, 0, NULL, restoreBoolItem,
     NAME_apply, "Set ->selection to <-default"),
  SM(NAME_labelWidth, 1, "pixels=[int]", labelWidthBoolItem,
     NAME_layout, "Set width of label in pixels")
};

/* Get Methods */

static getdecl get_bool_item[] =
{ GM(NAME_reference, 0, "point", NULL, getReferenceBoolItem,
     DEFAULT, "Baseline of label"),
  GM(NAME_default, 0, "bool", NULL, getDefaultBoolItem,
     NAME_apply, "Current default value"),
  GM(NAME_modified, 0, "bool", NULL, getModifiedBoolItem,
     NAME_apply, "If @on, the value has been modified"),
  GM(NAME_labelWidth, 0, "int", NULL, getLabelWidthBoolItem,
     NAME_layout, "Get minimal width required for label"),
  GM(NAME_selection, 0, "bool", NULL, getSelectionBoolItem,
     NAME_selection, "Current value of the selection")
};

/* Resources */

static classvardecl rc_bool_item[] =
{ RC(NAME_switchSize, "size", "size(40,22)",
     "Size of the track (before scaling)"),
  RC(NAME_onColour, "colour", "ui_accent",
     "Colour of the track if @on"),
  RC(NAME_offColour, "colour", "grey70",
     "Colour of the track if @off"),
  RC(NAME_knobColour, "colour", "white",
     "Colour of the knob"),
  RC(NAME_inactiveColour, RC_REFINE, "@nil",
     "@nil: an inactive switch fades rather than using fixed colours")
};

/* Class Declaration */

static Name bool_item_termnames[] = { NAME_label, NAME_selection, NAME_message };

ClassDecl(bool_item_decls,
          var_bool_item, send_bool_item, get_bool_item, rc_bool_item,
          3, bool_item_termnames);

status
makeClassBoolItem(Class class)
{ declareClass(class, &bool_item_decls);
  setRedrawFunctionClass(class, RedrawAreaBoolItem);

  succeed;
}
