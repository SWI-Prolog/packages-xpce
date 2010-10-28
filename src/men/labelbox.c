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

static status	restoreLabelBox(LabelBox lb);

		/********************************
		*             CREATE		*
		********************************/

static status
initialiseLabelBox(LabelBox lb, Name name, Code msg)
{ assign(lb, label_width,  DEFAULT);

  initialiseDialogGroup((DialogGroup) lb, name, DEFAULT);

  if ( isDefault(msg) )
    msg = NIL;

  assign(lb, pen, 		toInt(0));
  assign(lb, border, 		newObject(ClassSize, EAV));
  assign(lb, auto_label_align,	ON);
  assign(lb, message,		msg);
  assign(lb, modified,		OFF);

  succeed;
}

		 /*******************************
		 *	      LABEL		*
		 *******************************/

static void
compute_label(LabelBox lb, int *w, int *h, int *y)
{ compute_label_size_dialog_group((DialogGroup)lb, w, h);

  if ( *w > 0 )
  { if ( instanceOfObject(lb->label_font, ClassFont) )
      *w += valInt(getExFont(lb->label_font));
    else
      *w += 5;
  }

  if ( notDefault(lb->label_width) && *w < valInt(lb->label_width) )
    *w = valInt(lb->label_width);

  if ( y )
  { *y = 0;

    if ( instanceOfObject(lb->label, ClassCharArray) )
    { Graphical gr = getHeadChain(lb->graphicals);

      for( ; gr && notNil(gr); gr = get(gr, NAME_right, EAV))
      { Point pt;

	if ( (pt = get(gr, NAME_reference, EAV)) )
	{ int ry = valInt(pt->y);
	  int af = valInt(getAscentFont(lb->label_font));

	  if ( ry > af )
	    *y = ry-af;

	  break;
	}
      }
    }
  }
}


static status
labelWidthLabelBox(LabelBox lb, Int w)
{ if ( lb->label_width != w )
  { assign(lb, label_width, w);
    return requestComputeDevice((Device)lb, DEFAULT);
  }

  succeed;
}


static status
labelFormatLabelBox(LabelBox lb, Name fmt)
{ if ( lb->label_format != fmt )
  { assign(lb, label_width, fmt);
    return requestComputeDevice((Device)lb, DEFAULT);
  }

  succeed;
}


static Int
getLabelWidthLabelBox(LabelBox lb)
{ int w, h;

  compute_label(lb, &w, &h, NULL);

  answer(toInt(w));
}


		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

static status
computeLabelBox(LabelBox lb)
{ if ( notNil(lb->request_compute) )
  { int x, y, w, h;
    Area a = lb->area;
    int lw, lh;
    Size border;

    obtainClassVariablesObject(lb);
    border = (isDefault(lb->border) ? lb->gap : lb->border);
    compute_label(lb, &lw, &lh, NULL);
    computeGraphicalsDevice((Device) lb);

    if ( isDefault(lb->size) )		/* implicit size */
    { Cell cell;

      clearArea(a);
      for_cell(cell, lb->graphicals)
      { Graphical gr = cell->value;

	unionNormalisedArea(a, gr->area);
      }
      relativeMoveArea(a, lb->offset);

      x = valInt(a->x) -     valInt(border->w);
      y = valInt(a->y) -     valInt(border->h);
      w = valInt(a->w) + 2 * valInt(border->w);
      h = valInt(a->h) + 2 * valInt(border->h);

      w += lw;
      x -= lw;
    } else				/* explicit size */
    { x = valInt(lb->offset->x);
      y = valInt(lb->offset->y);
      w = valInt(lb->size->w);
      h = valInt(lb->size->h);
      x -= lw;
    }

    h = max(h, lh);
    w = max(w, lw);

    CHANGING_GRAPHICAL(lb,
	assign(a, x, toInt(x));
	assign(a, y, toInt(y));
	assign(a, w, toInt(w));
	assign(a, h, toInt(h)));

    assign(lb, request_compute, NIL);
  }

  succeed;
}

		 /*******************************
		 *	       GEOMETRY		*
		 *******************************/

static status
geometryLabelBox(LabelBox lb, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) || notDefault(h) )
  { Any size;
    int lw, lh;

    compute_label(lb, &lw, &lh, NULL);

    if ( isDefault(w) )
      w = getWidthGraphical((Graphical) lb);
    if ( isDefault(h) )
      h = getHeightGraphical((Graphical) lb);

    size = newObject(ClassSize, w, h, EAV);
    qadSendv(lb, NAME_size, 1, &size);
    doneObject(size);
  }

  return geometryDevice((Device) lb, x, y, w, h);
}


static status
layoutDialogLabelBox(LabelBox lb)
{ int lw, lh;

  obtainClassVariablesObject(lb);
  compute_label(lb, &lw, &lh, NULL);

  if ( notDefault(lb->size) )
    lb->size->w = toInt(valInt(lb->size->w) - lw);
  layoutDialogDevice((Device)lb, lb->gap, lb->size, lb->border);
  if ( notDefault(lb->size) )
    lb->size->w = toInt(valInt(lb->size->w) + lw);

  succeed;
}



		 /*******************************
		 *	       REDRAW		*
		 *******************************/

static status
RedrawAreaLabelBox(LabelBox lb, Area a)
{ Device dev = (Device)lb;
  device_draw_context ctx;

  if ( EnterRedrawAreaDevice(dev, a, &ctx) )
  { Cell cell;
    int lw, lh, ly, sx = 5;

    compute_label(lb, &lw, &lh, &ly);
    if ( instanceOfObject(lb->label_font, ClassFont) )
      sx = valInt(getExFont(lb->label_font));

    RedrawLabelDialogGroup((DialogGroup)lb, 0,
			   -lw, ly, lw-sx, lh,
			   lb->label_format, NAME_top, 0);

    for_cell(cell, dev->graphicals)
    { Graphical gr = cell->value;

      if ( gr->displayed == ON && overlapArea(a, gr->area) )
	RedrawArea(gr, a);
    }

    ExitRedrawAreaDevice(dev, a, &ctx);
  }

  return RedrawAreaGraphical(dev, a);
}


static Point
getReferenceLabelBox(LabelBox lb)
{ Point pt;

  if ( (pt = getAttributeObject(lb, NAME_reference)) &&
       instanceOfObject(pt, ClassPoint) )
    answer(pt);

  obtainClassVariablesObject(lb);

  answer(answerObject(ClassPoint, ZERO, getAscentFont(lb->label_font), EAV));
}

		 /*******************************
		 *	  MODIFIED/APPLY	*
		 *******************************/

static status
modifiedLabelBox(LabelBox lb, BoolObj m)
{ assign(lb, modified, m);

  if ( m == ON && notNil(lb->device) )
    send(lb->device, NAME_modifiedItem, lb, ON, EAV);

  succeed;
}


static status
modifiedItemLabelBox(LabelBox lb, Graphical item, BoolObj m)
{ if ( m == ON )
    send(lb, NAME_modified, ON, EAV);

  succeed;
}


static status
defaultLabelBox(LabelBox lb, Any def)
{ if ( lb->default_value != def )
  { assign(lb, default_value, def);

    return restoreLabelBox(lb);
  }

  succeed;
}


static Any
getDefaultLabelBox(LabelBox lb)
{ answer(checkType(lb->default_value, TypeAny, lb));
}


static status
restoreLabelBox(LabelBox lb)
{ Any val;

  TRY(val = getDefaultLabelBox(lb));
  return send(lb, NAME_selection, val, EAV);
}


static status
applyLabelBox(LabelBox lb, BoolObj always)
{ Any val;

  if ( instanceOfObject(lb->message, ClassCode) &&
       (always == ON || lb->modified == ON) &&
       (val = getv(lb, NAME_selection, 0, NULL)) )
    return forwardReceiverCode(lb->message, lb, val, EAV);

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=[name]", "message=[code]*" };
static char *T_modifiedItem[] =
        { "item=graphical", "modified=bool" };
static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };

/* Instance Variables */

static vardecl var_label_box[] =
{ SV(NAME_labelFormat, "{left,center,right}", IV_GET|IV_STORE|IV_REDEFINE,
     labelFormatLabelBox,
     NAME_layout, "Align label in its box"),
  IV(NAME_labelWidth, "[int]", IV_NONE,
     NAME_layout, "Width of the label"),
  IV(NAME_autoLabelAlign, "bool", IV_BOTH,
     NAME_layout, "Automatically align label"),
  IV(NAME_message, "code*", IV_BOTH,
     NAME_action, "Associated action"),
  IV(NAME_default, "any|function", IV_NONE,
     NAME_apply, "The default value"),
  IV(NAME_modified, "bool", IV_GET,
     NAME_apply, "Item has been modified")
};

/* Send Methods */

static senddecl send_label_box[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseLabelBox,
     DEFAULT, "Create a label_box"),
  SM(NAME_geometry, 4, T_geometry, geometryLabelBox,
     DEFAULT, "Move/resize label box"),
  SM(NAME_compute, 0, NULL, computeLabelBox,
     NAME_update, "Recompute area"),
  SM(NAME_labelWidth, 1, "[int]", labelWidthLabelBox,
     NAME_layout, "Specify width of the label"),
  SM(NAME_layoutDialog, 0, NULL, layoutDialogLabelBox,
     NAME_layout, "(Re)compute layout of dialog_items"),
  SM(NAME_modified, 1, "bool", modifiedLabelBox,
     NAME_apply, "Forward to all <-graphicals"),
  SM(NAME_default, 1, "any|function", defaultLabelBox,
     NAME_apply, "Set variable -default and ->selection"),
  SM(NAME_restore, 0, NULL, restoreLabelBox,
     NAME_apply, "Set ->selection to <-default"),
  SM(NAME_apply, 1, "[bool]", applyLabelBox,
     NAME_apply, "->execute if <-modified or @on"),
  SM(NAME_selection, 1, "any", failObject,
     NAME_selection, "Virtual method"),
  SM(NAME_modifiedItem, 2, T_modifiedItem, modifiedItemLabelBox,
     NAME_apply, "Indicates item has changed state")
};

/* Get Methods */

static getdecl get_label_box[] =
{ GM(NAME_reference, 0, "point", NULL, getReferenceLabelBox,
     DEFAULT, "Left, baseline of label"),
  GM(NAME_labelWidth, 0, "int", NULL, getLabelWidthLabelBox,
     NAME_layout, "Current width of the label"),
  GM(NAME_default, 0, "any", NULL, getDefaultLabelBox,
     NAME_apply, "Current default value"),
  GM(NAME_selection, 0, "any", NULL, getFailObject,
     NAME_selection, "Virtual method")
};

/* Resources */

static classvardecl rc_label_box[] =
{ RC(NAME_labelFormat, "{left,center,right}", "right",
     "Alignment of the label in its box"),
  RC(NAME_labelSuffix, "name", ":",
     "Ensured suffix of label")
};

/* Class Declaration */

ClassDecl(label_box_decls,
          var_label_box, send_label_box, get_label_box, rc_label_box,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassLabelBox(Class class)
{ declareClass(class, &label_box_decls);

  setRedrawFunctionClass(class, RedrawAreaLabelBox);

  succeed;
}

