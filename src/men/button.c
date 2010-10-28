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

static status
initialiseButton(Button b, Name name, Message msg, Name acc)
{ createDialogItem(b, name);

  assign(b, default_button, OFF);
  assign(b, show_focus_border, ON);

  assign(b, message, msg);
  if ( notDefault(acc) )
    assign(b, accelerator, acc);

  return requestComputeGraphical(b, DEFAULT);
}


int
accelerator_code(Name a)
{ if ( isName(a) )
  { char *s = strName(a);

    if ( s[0] == '\\' && s[1] == 'e' && isalpha(s[2]) && s[3] == EOS )
      return s[2];
    if ( s[1] == EOS && isalpha(s[0]) )
      return s[0];
  }

  return 0;
}


static status
RedrawMenuBarButton(Button b, Area a)
{ int x, y, w, h;
  Any ofg = NIL;
  int flags = 0;

  initialiseDeviceGraphical(b, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);

  if ( b->status == NAME_preview )
  { Elevation e;

    if ( b->look == NAME_gtkMenuBar &&
	 (e = getClassVariableValueObject(b, NAME_previewElevation)) &&
	 notNil(e) )
    { r_3d_box(x, y, w, h, 0, e, TRUE);
    } else /* if ( b->look == NAME_winMenuBar ) */
    { Any fg = getClassVariableValueObject(b, NAME_selectedForeground);
      Any bg = getClassVariableValueObject(b, NAME_selectedBackground);

      if ( !fg ) fg = WHITE_COLOUR;
      if ( !bg ) bg = BLACK_COLOUR;
      r_fill(x, y, w, h, bg);
      ofg = r_colour(fg);
    }
  }

  if ( b->active == OFF )
    flags |= LABEL_INACTIVE;

  RedrawLabelDialogItem(b, accelerator_code(b->accelerator),
			x, y, w, h,
			NAME_center, NAME_center, flags);

  if ( notNil(ofg) )
    r_colour(ofg);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Draw face of the button. This is  really   a  mess. There are simply too
many style and other options. In addition,  the Motif/Gtk choice to draw
a large sunken region around the focus/default button make the mess even
bigger. We generally should not do that   for  all buttons (i.e. not for
closely stacked buttons in a button-bar,   which  means normally not for
buttons having images.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
draw_generic_button_face(Button b,
			 int x, int y, int w, int h,
			 int up, int defb, int focus)
{ Elevation z = getClassVariableValueObject(b, NAME_elevation);
  int r = valInt(b->radius);

  if ( z && notNil(z) )			/* 3-d style */
  { int up = (b->status == NAME_inactive || b->status == NAME_active);

    if ( b->look == NAME_motif ||
	 b->look == NAME_gtk ||
	 b->look == NAME_win )
    { int bx = x, by = y, bw = w, bh = h;

      if ( b->look == NAME_motif ||
	   b->look == NAME_gtk )
      {
	if ( b->show_focus_border == ON )
	{ PceWindow sw = getWindowGraphical((Graphical)b);
	  Graphical kbfocus = (sw ? sw->keyboard_focus : NIL);

	  if ( focus ||
	       kbfocus == (Graphical) b ||	/* inactive focus */
	       (defb && !instanceOfObject(kbfocus, ClassButton)) )
	  { static Elevation e = NULL;

	    if ( !e )
	      e = newObject(ClassElevation, ONE, EAV);

	    bx -= GTK_BUTTON_MARGIN;
	    by -= GTK_BUTTON_MARGIN;
	    bw += GTK_BUTTON_MARGIN * 2;
	    bh += GTK_BUTTON_MARGIN * 2;
	    r_3d_box(bx, by, bw, bh, r, e, FALSE);
	  }
	}

	if ( focus )
	{ int pen = valInt(b->pen);

	  if ( pen > 0 )
	  { r_thickness(pen);
	    r_box(x-pen, y-pen, w+2*pen, h+2*pen, r, NIL);
	  }
	}
      } else
      { if ( defb )
	{ int pen = valInt(b->pen);

	  bx -= pen; by -= pen; bw += 2*pen; bh += 2*pen;
	  r_thickness(pen);
	  r_box(bx, by, bw, bh, r, NIL);
	}
      }
    }

    r_3d_box(x, y, w, h, r, z, up);
    if ( b->look == NAME_openLook && defb )
    { Any old;

      old = r_colour(r_elevation_shadow(z));
      r_box(x+2, y+2, w-4, h-4, r, NIL);
      r_colour(old);
    }
  } else				/* 2-d style */
  { int swapc  = 0;
    int pen    = valInt(b->pen);
    int radius = valInt(b->radius);
    int shadow = valInt(b->shadow);

    if ( defb && b->look != NAME_openLook )
      pen++;

    r_thickness(pen);
    r_dash(b->texture);

    if ( up )
    { r_shadow_box(x, y, w, h, radius, shadow, NIL);
    } else if ( b->status == NAME_preview )
    { r_shadow_box(x, y, w, h, radius, shadow, BLACK_IMAGE);
      swapc = TRUE;
    } else if ( b->status == NAME_execute )
    { r_shadow_box(x, y, w, h, radius, shadow, GREY25_IMAGE);
    }

    if ( swapc )
      r_swap_background_and_foreground();

    if ( defb && b->look == NAME_openLook )
      r_box(x+pen, y+pen, w-2*pen-shadow, h-2*pen-shadow, radius, NIL);

    if ( swapc )
      r_swap_background_and_foreground();
  }
}


static int
draw_button_popup_indicator(Button b, int x, int y, int w, int h, int up)
{ int rm;				/* required right margin */

  if ( notNil(b->popup_image) )
  { int iw = valInt(b->popup_image->size->w);
    int ih = valInt(b->popup_image->size->h);

    rm = iw+8;
    r_image(b->popup_image, 0, 0, x+w-rm, y + (h-ih)/2, iw, ih, ON);
  } else
  { Elevation z = getClassVariableValueObject(b, NAME_elevation);

    if ( b->look == NAME_motif || b->look == NAME_gtk )
    { int bw = 12;
      int bh = 8;

      rm = bw+8;
      r_3d_box(x+w-bw-8, y+(h-bh)/2, bw, bh, 0, z, TRUE);
    } else
    { int th = 8;
      int tw = 9;
      int tx, ty;

      rm = tw+8;
      tx = x+w-rm;
      ty = y + (h-th)/2;

      r_3d_triangle(tx+tw/2, ty+th, tx, ty, tx+tw, ty, z, up, 0x3);
      rm = tw;
    }
  }

  return rm;
}


status
RedrawAreaButton(Button b, Area a)
{ int x, y, w, h;
  int defb;
  int rm = 0;				/* right-margin */
  PceWindow sw;
  int kbf;				/* Button has keyboard focus */
  int obhf;				/* Other button has focus */
  int focus;
  int swapbg = FALSE;
  int up;
  int flags = 0;
  Elevation z;

  if ( b->look == NAME_winMenuBar ||
       b->look == NAME_gtkMenuBar )
    return RedrawMenuBarButton(b, a);

  if ( b->active == OFF )
    flags |= LABEL_INACTIVE;

  up = (b->status == NAME_active || b->status == NAME_inactive);
  defb = (b->default_button == ON);
  initialiseDeviceGraphical(b, &x, &y, &w, &h);
  NormaliseArea(x, y, w, h);

  if ( (sw = getWindowGraphical((Graphical)b)) )
  { kbf   = (sw->keyboard_focus == (Graphical) b);
    obhf  = (!kbf && instanceOfObject(sw->keyboard_focus, ClassButton));
    focus = (sw->input_focus == ON);
  } else
    kbf = obhf = focus = FALSE;		/* should not happen */

  if ( !ws_draw_button_face((DialogItem)b,
			    x, y, w, h,
			    up, defb, kbf && focus) )
    draw_generic_button_face(b, x, y, w, h, up, defb, kbf && focus);

  if ( b->look == NAME_openLook && b->status == NAME_preview &&
       !((z = getClassVariableValueObject(b, NAME_elevation)) && notNil(z)) )
  { swapbg = TRUE;
    r_swap_background_and_foreground();
  }

  if ( notNil(b->popup) && !instanceOfObject(b->label, ClassImage) )
    rm = draw_button_popup_indicator(b, x, y, w, h, up);

  RedrawLabelDialogItem(b, accelerator_code(b->accelerator),
			x, y, w-rm, h,
			NAME_center, NAME_center, flags);

  if ( swapbg )
    r_swap_background_and_foreground();

  return RedrawAreaGraphical(b, a);
}


static status
computeButton(Button b)
{ if ( notNil(b->request_compute) )
  { int w, h, isimage;

    TRY(obtainClassVariablesObject(b));

    dia_label_size(b, &w, &h, &isimage);

    if ( b->look == NAME_winMenuBar ||
         b->look == NAME_gtkMenuBar )
    { if ( !isimage )
      { w += valInt(getExFont(b->label_font)) * 2;

	if ( b->look == NAME_gtkMenuBar )
	  h += 4;
      } else
      { w += 4;
	h += 4;
      }
    } else
    { if ( isimage )
      { w += 4;
	h += 4;
      } else
      { Size size = getClassVariableValueObject(b, NAME_size);

	h += 6; w += 10 + valInt(b->radius);
	if ( notNil(b->popup) )
	{ if ( notNil(b->popup->popup_image) )
	    w += valInt(b->popup->popup_image->size->w) + 5;
	  else if ( b->look == NAME_motif || b->look == NAME_gtk )
	    w += 12 + 5;
	  else
	    w += 9 + 5;
	}
	w = max(valInt(size->w), w);
	h = max(valInt(size->h), h);
      }
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

    if ( b->look == NAME_winMenuBar ||
	 b->look == NAME_gtkMenuBar )
      rx = valInt(getExFont(b->label_font));

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
makeButtonGesture()
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
{ if ( b->active == ON &&
       ( b->look == NAME_motif ||
	 b->look == NAME_gtk ||
	 b->look == NAME_win) )
    succeed;

  fail;
}


static status
eventButton(Button b, EventObj ev)
{ if ( eventDialogItem(b, ev) )
    succeed;

  if ( b->active == ON )
  { int infocus = (getKeyboardFocusGraphical((Graphical) b) == ON);

    makeButtonGesture();

    if ( ev->id == toInt(13) && infocus ) /* RETURN */
    { send(b, NAME_execute, EAV);
      succeed;
    }

    if ( isAEvent(ev, NAME_msLeftDown) && !infocus )
      send(b, NAME_keyboardFocus, ON, EAV);

    if ( isAEvent(ev, NAME_focus) )
    { changedDialogItem(b);
      succeed;
    }

    return eventGesture(GESTURE_button, ev);
  }

  fail;
}


static status
keyButton(Button b, Name key)
{ if ( b->active == ON )
  { static Name ret;

    if ( !ret )
      ret = CtoName("RET");

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

    if ( d )
      busyCursorDisplay(d, DEFAULT, DEFAULT);
    statusButton(b, NAME_execute);
    flushGraphical(b);
    send(b, NAME_forward, EAV);
    if ( d )
      busyCursorDisplay(d, NIL, DEFAULT);

    if ( !isFreedObj(b) )
      statusButton(b, NAME_inactive);
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
{ RC(NAME_look, RC_REFINE, UXWIN("gtk", "win"),
     NULL),
  RC(NAME_alignment, "{column,left,center,right}", "center",
     "Alignment in the row"),
  RC(NAME_labelFont, "font", "normal",
     "Default font for labels"),
  RC(NAME_labelSuffix, "name", "",
     "Ensured suffix of label"),
  RC(NAME_pen, "int", "1",
     "Thickness of box"),
  RC(NAME_selectedForeground, "colour",
     UXWIN("white", "win_highlighttext"),
     "Colour when in preview mode (Windows menu-bar)"),
  RC(NAME_selectedBackground, "colour",
     UXWIN("black", "win_highlight"),
     "Background when in preview mode (Windows menu-bar)"),
  RC(NAME_popupImage, "image*",
     "when(@colour_display, @nil, @ol_pulldown_image)",
     "Image to indicate presence of popup menu"),
  RC(NAME_radius, "int", "0",
     "Rounding radius of box"),
  RC(NAME_shadow, "int", "when(@colour_display, 0, 1)",
     "Shadow shown around the box"),
  RC(NAME_size, "size", UXWIN("size(50,20)", "size(80,24)"),
     "Minimum size in pixels"),
  RC(NAME_previewElevation, "elevation*",
     "elevation(preview, 1, hilited)",
     "Elevation of item in preview mode"),
  RC(NAME_elevation, RC_REFINE,
     UXWIN("when(@colour_display, button, @nil)",
	   "elevation(@nil, 2, @_dialog_bg)"),
     NULL)
};

/* Class Declaration */

static Name button_termnames[] = { NAME_label, NAME_message, NAME_accelerator };

ClassDecl(button_decls,
          var_button, send_button, get_button, rc_button,
          3, button_termnames,
          "$Rev$");


status
makeClassButton(Class class)
{ declareClass(class, &button_decls);
  setRedrawFunctionClass(class, RedrawAreaButton);

  succeed;
}

