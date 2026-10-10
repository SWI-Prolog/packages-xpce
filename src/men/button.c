/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi.psy.uva.nl/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
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
#include <h/dialog.h>
#include <math.h>

static status
initialiseButton(Button b, Name name, Message msg, Name acc)
{ createDialogItem(b, name);

  assign(b, default_button, OFF);
  assign(b, show_focus_border, ON);

  assign(b, message, msg);
  if ( notDefault(acc) )		/* fixed: see ->accelerator */
  { assign(b, accelerator, normaliseAccelerator(acc));
    assign(b, accelerator_fixed, ON);
  }

  return requestComputeGraphical(b, DEFAULT);
}


/* The character of accelerator `a` that is underlined in a label, or
 * 0.  As on Windows, Gnome and KDE, the underlines are only shown while
 * Alt (Option on MacOS) is held, unless the class variable
 * dialog_item.accelerator_cues says `always` (or `never`).  The window
 * system reports Alt through acceleratorCuesFrame().  A popup opened
 * from the keyboard shows them anyway: see accelerator_key().
 */

static bool alt_held = false;

static Name
accelerator_cues(void)
{ Name mode = getClassVariableValueClass(ClassDialogItem,
					  NAME_acceleratorCues);

  return mode ? mode : NAME_alt;
}


int
accelerator_code(Name a)
{ Name mode = accelerator_cues();

  if ( mode == NAME_never || (mode == NAME_alt && !alt_held) )
    return 0;

  return accelerator_key(a);
}


/* Alt went down or up in frame `fr`.  Redraw it if the underlines
 * change.  That includes the windows displayed inside its windows,
 * such as the dialogs on the tabs of a tabbed_window, which are not
 * members of the frame, and the open popups, which are frames of their
 * own.
 */

status
acceleratorCuesFrame(FrameObj fr, bool held)
{ if ( alt_held != held )
  { alt_held = held;

    if ( fr && notNil(fr) && !isFreeingObj(fr) &&
	 accelerator_cues() == NAME_alt )
    { Chain agenda = answerObject(ClassChain, EAV);
      Device dev;
      Cell cell;

      for_cell(cell, fr->members)
	appendChain(agenda, cell->value);
      if ( notNil(fr->display) )
      { Cell fc;

	for_cell(fc, fr->display->frames)
	{ FrameObj pf = fc->value;

	  if ( pf != fr && pf->kind == NAME_popup )
	  { for_cell(cell, pf->members)
	      appendChain(agenda, cell->value);
	  }
	}
      }

      while( (dev = getDeleteHeadChain(agenda)) )
      { if ( instanceOfObject(dev, ClassWindow) )
	  send(dev, NAME_redraw, EAV);

	for_cell(cell, dev->graphicals)
	{ if ( instanceOfObject(cell->value, ClassDevice) )
	    appendChain(agenda, cell->value);
	}
      }

      doneObject(agenda);
    }
  }

  succeed;
}


int
accelerator_key(Name a)
{ if ( isName(a) )
  { char *s = strName(a);

    if ( s[0] == '\\' && s[1] == 'e' && isalpha(s[2]) && s[3] == EOS )
      return s[2];
    if ( s[1] == EOS && isalpha(s[0]) )
      return s[0];
  }

  return 0;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Draw face of the button. This is  really   a  mess. There are simply too
many style and other options. In addition,  the Motif/Gtk choice to draw
a large sunken region around the focus/default button make the mess even
bigger. We generally should not do that   for  all buttons (i.e. not for
closely stacked buttons in a button-bar,   which  means normally not for
buttons having images.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* The face of a button: a flat rounded box.  The default button is
 * filled with the accent colour.  A pressed button is darker, an
 * inactive one faded.  The keyboard focus is shown by a wider border
 * in the accent colour or, on the default button, a white ring inside
 * the border.  Both stay inside the area of the button.
 */

static bool
is_pressed(Button b)
{ return b->status == NAME_preview || b->status == NAME_execute;
}


static bool
accent_face(Button b, int defb)
{ return defb && b->active == ON && !is_pressed(b);
}


static void
draw_generic_button_face(Button b,
			 int x, int y, int w, int h,
			 int up, int defb, int focus)
{ int r = valInt(b->radius);
  int pen = valInt(b->pen);
  bool accent = accent_face(b, defb);
  Any fill, border;
  Any old;

  if ( is_pressed(b) )
    fill = getClassVariableValueObject(b, NAME_pressedColour);
  else if ( accent )
    fill = getClassVariableValueObject(b, NAME_accentColour);
  else
    fill = getClassVariableValueObject(b, NAME_faceColour);
  if ( accent )
    border = fill;
  else if ( focus )
  { border = getClassVariableValueObject(b, NAME_accentColour);
    pen++;
  } else
    border = getClassVariableValueObject(b, NAME_borderColour);

  if ( b->active == OFF )
    r_push_group();
  old = r_colour(border);
  r_dash(NAME_none);
  r_thickness(pen);
  r_smooth_box(x, y, w, h, r, fill);
  if ( b->active == OFF )
    r_pop_group_with_alpha(INACTIVE_ALPHA);

  if ( focus && accent )		/* ring inside the accent face */
  { int m = pen+1;

    r_colour(getClassVariableValueObject(b, NAME_selectedForeground));
    r_thickness(1);
    r_smooth_box(x+m, y+m, w-2*m, h-2*m, max(0, r-m), NIL);
  }

  r_thickness(1);
  r_colour(old);
}


/* A button with a popup shows a marker at its right: <-popup_image or
 * a down chevron.  If the button also has a message it is a split
 * button: a line separates the label, which runs the message, from the
 * marker, which opens the popup.  See on_popup_marker().
 */

static int
popup_marker_width(Button b)
{ double ex = valNum(getExFont(b->label_font));

  if ( notNil(b->popup_image) )
    return valInt(b->popup_image->size->w) + (int)ex;

  return (int)(ex*2.5 + 0.5);
}


static int
draw_button_popup_indicator(Button b, int x, int y, int w, int h, bool up)
{ int rm = popup_marker_width(b);	/* required right margin */
  double ex = valNum(getExFont(b->label_font));

  if ( notNil(b->popup_image) )
  { int iw = valInt(b->popup_image->size->w);
    int ih = valInt(b->popup_image->size->h);

    r_image(b->popup_image, 0, 0, x+w-rm, y + (h-ih)/2, iw, ih);
  } else
  { double cw = ex*0.9;			/* the chevron */
    double ch = cw*0.5;
    double cx = x+w-rm + (rm-cw)/2.0 - (b->message != NIL ? 0 : ex*0.3);
    double cy = y + (h-ch)/2.0;
    Any old = NULL;

    if ( b->active == OFF )
      old = r_colour(ws_3d_grey());	/* as the inactive label */
    r_chevron(cx, cy, cw, max(1.5, ex/5.0));
    if ( old )
      r_colour(old);

    if ( notNil(b->message) )		/* split button */
    { Any c = r_colour(getClassVariableValueObject(b, NAME_separatorColour));

      r_thickness(1);
      r_line(x+w-rm, y+h/4.0, x+w-rm, y+h-h/4.0);
      r_colour(c);
    }
  }

  return rm;
}


/* True if `ev` is on the popup marker, so it opens the popup.  A button
 * without a message is all marker.
 */

static bool
on_popup_marker(Button b, EventObj ev)
{ Int X, Y;

  if ( isNil(b->popup) )
    return false;
  if ( isNil(b->message) )
    return true;

  return ( get_xy_event(ev, b, ON, &X, &Y) &&
	   valInt(X) >= valInt(b->area->w) - popup_marker_width(b) );
}


status
RedrawAreaButton(Button b, Area a)
{ int x, y, w, h;
  int defb;
  int rm = 0;				/* right-margin */
  PceWindow sw;
  bool kbf;				/* Button has keyboard focus */
  bool focus;
  bool up;
  int flags = 0;

  if ( b->active == OFF )
    flags |= LABEL_INACTIVE;

  up = (b->status == NAME_active || b->status == NAME_inactive);
  defb = (b->default_button == ON);
  initialiseDeviceGraphical(b, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);

  if ( (sw = getWindowGraphical((Graphical)b)) )
  { kbf   = (sw->keyboard_focus == (Graphical) b);
    focus = (sw->input_focus == ON);
  } else
    kbf = focus = false;		/* should not happen */

  draw_generic_button_face(b, x, y, w, h, up, defb, kbf && focus);

  Any old = NULL;
  if ( accent_face(b, defb) )
    old = r_colour(getClassVariableValueObject(b, NAME_selectedForeground));

  if ( notNil(b->popup) && !instanceOfObject(b->label, ClassImage) )
    rm = draw_button_popup_indicator(b, x, y, w, h, up);

  RedrawLabelDialogItem(b, accelerator_code(b->accelerator),
			x, y, w-rm, h,
			NAME_center, NAME_center, flags);
  if ( old )
    r_colour(old);

  return RedrawAreaGraphical(b, a);
}


static status
computeButton(Button b)
{ if ( notNil(b->request_compute) )
  { int w, h, isimage;

    TRY(obtainClassVariablesObject(b));

    dia_label_size(b, &w, &h, &isimage);

    if ( isimage )
    { w += 4;
      h += 4;
    } else		/* sync with draw_button_popup_indicator() */
    { Size size = getClassVariableValueObject(b, NAME_size);

      h += 6; w += 10 + valInt(b->radius);
      if ( notNil(b->popup) )
      { w += popup_marker_width(b);
	if ( notNil(b->message) )	/* room between label and separator */
	  w += valInt(getExFont(b->label_font));
      }
      w = max(valInt(size->w), w);
      h = max(valInt(size->h), h);
    }

    CHANGING_GRAPHICAL(b,
	 assign(b->area, w, toInt(w));
	 assign(b->area, h, toInt(h)));

    assign(b, request_compute, NIL);
  }

  succeed;
}


Point
getReferenceButton(Button b)
{ Point ref;

  if ( !(ref = getReferenceDialogItem(b)) &&
       !instanceOfObject(b->label, ClassImage) )
  { int fh, ascent, h, rx = 0;

    ComputeGraphical(b);
    fh     = valInt(getHeightFont(b->label_font));
    ascent = valInt(getAscentFont(b->label_font));
    h      = valInt(b->area->h);

    ref = answerObject(ClassPoint, toInt(rx), toInt((h - fh)/2 + ascent), EAV);
  }

  answer(ref);
}


static status
statusButton(Button b, Name stat)
{ if ( stat != b->status )
  { Name oldstat = b->status;

    assign(b, status, stat);

					/* These are equal: do not redraw */
    if ( !( (stat == NAME_active || stat == NAME_inactive) &&
	    (oldstat == NAME_active || oldstat == NAME_inactive)
	  ) )
      changedDialogItem(b);
  }

  succeed;
}


status
makeButtonGesture(void)
{ if ( GESTURE_button != NULL )
    succeed;

  GESTURE_button =
    globalObject(NAME_ButtonGesture, ClassClickGesture,
		 NAME_left, DEFAULT, DEFAULT,
		 newObject(ClassMessage, RECEIVER, NAME_execute, EAV),
		 newObject(ClassMessage, RECEIVER, NAME_status,NAME_preview,EAV),
		 newObject(ClassMessage, RECEIVER, NAME_cancel, EAV),
		 EAV);

  assert(GESTURE_button);
  succeed;
}


static status
WantsKeyboardFocusButton(Button b)
{ return b->active == ON;
}


static status
eventButton(Button b, EventObj ev)
{ if ( eventDialogItem(b, ev) )
    succeed;

  if ( b->active == ON )
  { int infocus = (getKeyboardFocusGraphical((Graphical) b) == ON);

    makeButtonGesture();

    if ( infocus && isAEvent(ev, NAME_keyboard) &&
	 !(valInt(ev->buttons) & (BUTTON_control|BUTTON_meta|BUTTON_gui)) )
    { if ( notNil(b->popup) &&		/* Down, or the button only */
	   ( ev->id == NAME_cursorDown || /* has a popup */
	     ( isNil(b->message) &&
	       (ev->id == NAME_RET || ev->id == toInt(13) ||
		ev->id == toInt(' ')) ) ) )
	return keyboardPopupGesture((Graphical)b);

      if ( ev->id == NAME_RET || ev->id == toInt(13) ||
	   ev->id == toInt(' ') )	/* Return, Space: press */
      { send(b, NAME_execute, EAV);
	succeed;
      }
    }

    if ( isAEvent(ev, NAME_msLeftDown) && !infocus )
      send(b, NAME_keyboardFocus, ON, EAV);

    if ( isAEvent(ev, NAME_msLeftDown) && on_popup_marker(b, ev) )
      return postPopupGestureEvent(ev);

    if ( isAEvent(ev, NAME_focus) )
    { changedDialogItem(b);
      succeed;
    }

    return eventGesture(GESTURE_button, ev);
  }

  fail;
}


/* Return runs the default button.  Escape, and Command-period on
 * MacOS, run a button named `cancel`, as in the dialogs of Windows,
 * Gnome and MacOS.
 */

static status
keyButton(Button b, Name key)
{ if ( b->active == ON )
  { static Name ret, esc, cmd_period;

    if ( !ret )
    { ret = CtoName("RET");
      esc = CtoName("\\e");
      cmd_period = CtoName("\\s-.");
    }

    if ( (key == esc || key == cmd_period) && b->name == NAME_cancel )
      return send(b, NAME_execute, EAV);

    if ( b->accelerator == key && notNil(b->popup) && isNil(b->message) )
      return keyboardPopupGesture((Graphical)b);

    if ( b->accelerator == key ||
	 (b->default_button == ON && key == ret) )
      return send(b, NAME_execute, EAV);
  }

  fail;
}


static status
executeButton(Button b)
{ if ( notNil(b->message) )
  { DisplayObj d = getDisplayGraphical((Graphical) b);

    addCodeReference(b);
    if ( d )
      busyCursorDisplay(d, DEFAULT, DEFAULT);
    statusButton(b, NAME_execute);
    send(b, NAME_forward, EAV);
    if ( d )
      busyCursorDisplay(d, NIL, DEFAULT);

    if ( !isFreedObj(b) )
      statusButton(b, NAME_inactive);
    delCodeReference(b);
  }

  succeed;
}


static status
forwardButton(Button b)
{ if ( isNil(b->message) )
    succeed;

  if ( notDefault(b->message) )
    return forwardReceiverCode(b->message, b, EAV);

  return send(b->device, b->name, EAV);
}


		/********************************
		*          ATTRIBUTES		*
		********************************/

static status
defaultButtonButton(Button b, BoolObj val)
{ if ( isDefault(val) )
    val = ON;

  if ( hasSendMethodObject(b->device, NAME_defaultButton) )
    return send(b->device, NAME_defaultButton, b, EAV);
  else
    assign(b, default_button, val);

  succeed;
}


status
isApplyButton(Button b)
{ if ( b->name == NAME_apply )
    succeed;

  if ( instanceOfObject(b->message, ClassMessage) )
  { Message m = (Message)b->message;

    if ( m->selector == NAME_apply )
      succeed;
  }

  fail;
}


static status
radiusButton(Button b, Int radius)
{ return assignGraphical(b, NAME_radius, radius);
}


static status
popupButton(Button b, PopupObj p)
{ return assignGraphical(b, NAME_popup, p);
}


static PopupObj
getPopupButton(Button b, BoolObj create)
{ if ( notNil(b->popup) || create != ON )
    answer(b->popup);
  else
  { PopupObj p = newObject(ClassPopup, b->label, EAV);

    send(p, NAME_append,
	 newObject(ClassMenuItem,
		   b->name,
		   newObject(ClassMessage, Arg(1), NAME_execute, EAV),
		   b->label, EAV), EAV);
    popupButton(b, p);
    answer(p);
  }
}


static status
labelButton(Button b, Any label)
{ if ( b->label != label )
  { int ltype = instanceOfObject(label, ClassImage);
    int sametype = (instanceOfObject(b->label, ClassImage) == ltype);

    if ( !sametype )
    { assign(b, radius, ltype ? ZERO
			      : getClassVariableValueObject(b, NAME_radius));
      assign(b, show_focus_border, ltype ? OFF : ON);
    }
    assignGraphical(b, NAME_label, label);
  }

  succeed;
}


static status
showFocusBorderButton(Button b, BoolObj show)
{ return assignGraphical(b, NAME_showFocusBorder, show);
}


static status
shadowButton(Button b, Int shadow)
{ return assignGraphical(b, NAME_shadow, shadow);
}


static status
popupImageButton(Button b, Image img)
{ return assignGraphical(b, NAME_popupImage, img);
}


static Name
getSelectionButton(Button b)
{ answer(b->label);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=name", "message=[code]*", "label=[name]" };

/* Instance Variables */

static vardecl var_button[] =
{ SV(NAME_radius, "int", IV_GET|IV_STORE, radiusButton,
     NAME_appearance, "Rounding radius for corners"),
  SV(NAME_shadow, "int", IV_GET|IV_STORE, shadowButton,
     NAME_appearance, "Shadow shown around the box"),
  SV(NAME_popupImage, "image*", IV_GET|IV_STORE, popupImageButton,
     NAME_appearance, "Indication that button has a popup menu"),
  SV(NAME_defaultButton, "[bool]", IV_GET|IV_STORE, defaultButtonButton,
     NAME_accelerator, "Button is default button for its <-device"),
  SV(NAME_showFocusBorder, "bool", IV_GET|IV_STORE, showFocusBorderButton,
     NAME_appearance, "Show wide border around focus/default button")
};

/* Send Methods */

static senddecl send_button[] =
{ SM(NAME_compute, 0, NULL, computeButton,
     DEFAULT, "Compute desired size (from command)"),
  SM(NAME_event, 1, "event", eventButton,
     DEFAULT, "Process an event"),
  SM(NAME_initialise, 3, T_initialise, initialiseButton,
     DEFAULT, "Create from name and command"),
  SM(NAME_status, 1, "{inactive,active,preview,execute}", statusButton,
     DEFAULT, "Status for event-processing"),
  SM(NAME_popup, 1, "popup*", popupButton,
     DEFAULT, "Associated popup menu"),
  SM(NAME_key, 1, "key=name", keyButton,
     NAME_accelerator, "Handle accelerator key `name'"),
  SM(NAME_execute, 0, NULL, executeButton,
     NAME_action, "->forward and deal with UI"),
  SM(NAME_forward, 0, NULL, forwardButton,
     NAME_action, "Perform associated action"),
  SM(NAME_font, 1, "font", labelFontDialogItem,
     NAME_appearance, "same as ->label_font"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, WantsKeyboardFocusButton,
     NAME_event, "Test if ready to accept input"),
  SM(NAME_label, 1, "char_array|image*", labelButton,
     NAME_label, "Sets the visible label"),
  SM(NAME_selection, 1, "char_array|image*", labelButton,
     NAME_label, "Equivalent to ->label"),
  SM(NAME_isApply, 0, NULL, isApplyButton,
     NAME_apply, "Test if button ->apply the dialog")
};

/* Get Methods */

static getdecl get_button[] =
{ GM(NAME_popup, 1, "popup*", "create=[bool]", getPopupButton,
     DEFAULT, "Associated popup (make one if create = @on)"),
  GM(NAME_reference, 0, "point", NULL, getReferenceButton,
     DEFAULT, "Left, baseline of label"),
  GM(NAME_selection, 0, "name", NULL, getSelectionButton,
     NAME_label, "Equivalent to <-label")
};

/* Resources */

static classvardecl rc_button[] =
{ RC(NAME_look, RC_REFINE, UXWIN("xpce", "win"),
     NULL),
  RC(NAME_alignment, "{column,left,center,right}", "center",
     "Alignment in the row"),
  RC(NAME_labelFont, "font", "normal",
     "Default font for labels"),
  RC(NAME_labelSuffix, "name", "",
     "Ensured suffix of label"),
  RC(NAME_pen, "0..", "1",
     "Thickness of box"),
  RC(NAME_selectedForeground, "colour",
     "ui_selection_foreground",
     "Label colour of the default button"),
  RC(NAME_selectedBackground, "colour",
     "ui_selection_background",
     "Background when in preview mode (Windows menu-bar)"),
  RC(NAME_separatorColour, "colour", "ui_inactive",
     "Line between the label and popup marker of a split button"),
  RC(NAME_popupImage, "image*", "@nil",
     "Image to indicate presence of popup menu"),
  RC(NAME_radius, "0..", "4",
     "Rounding radius of box"),
  RC(NAME_faceColour, "colour", "ui_button_background",
     "Fill of the button"),
  RC(NAME_pressedColour, "colour", "ui_button_pressed",
     "Fill of the button while it is pressed"),
  RC(NAME_borderColour, "colour", "ui_separator",
     "Outline of the button"),
  RC(NAME_accentColour, "colour", "ui_accent",
     "Fill of the default button and colour of the focus ring"),
  RC(NAME_shadow, "int", "0",
     "Shadow shown around the box"),
  RC(NAME_size, "size", UXWIN("size(50,20)", "size(80,24)"),
     "Minimum size in pixels"),
  RC(NAME_elevation, RC_REFINE,
     UXWIN("button", "elevation(@nil, 2, @_dialog_bg)"),
     NULL)
};

/* Class Declaration */

static Name button_termnames[] = { NAME_label, NAME_message, NAME_accelerator };

ClassDecl(button_decls,
          var_button, send_button, get_button, rc_button,
          3, button_termnames);


status
makeClassButton(Class class)
{ declareClass(class, &button_decls);
  setRedrawFunctionClass(class, RedrawAreaButton);

  succeed;
}
