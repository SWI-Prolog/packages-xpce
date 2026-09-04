/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1996-2011, University of Amsterdam
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

#define HIDDEN_TAB_SHRINK 3
#define LOWER_LABEL 2

		/********************************
		*            CREATE		*
		********************************/

static status
initialiseTab(Tab t, Name name)
{ assign(t, label_offset, ZERO);
  assign(t, status,	  NAME_onTop);
  assign(t, size,	  DEFAULT);

  obtainClassVariablesObject(t);
  initialiseDialogGroup((DialogGroup) t, name, DEFAULT);

  succeed;
}

		 /*******************************
		 *	      COMPUTE		*
		 *******************************/

/* Room for the close button: a cross beside the text rather than a button
   around it, so it is drawn a little smaller than the label is tall and
   sits on the baseline of the text it belongs to.
*/

static int
close_button_size(int label_height)
{ int s = (label_height * 55) / 100;

  return s < 6 ? 6 : s;
}


static status
computeLabelTab(Tab t)
{ if ( notNil(t->label_size) )
  { int w, h;
    Size minsize = getClassVariableValueObject(t, NAME_labelSize);
    int ex = valInt(getAvgCharWidthFont(t->label_font));

    if ( notNil(t->label) && t->label != NAME_ )
    { compute_label_size_dialog_group((DialogGroup) t, &w, &h);
      if ( instanceOfObject(t->label, ClassCharArray) )
	h += 2+HIDDEN_TAB_SHRINK;
      w += 2*ex;
    } else				/* no label: the box shrinks to its */
    { w = 2*ex;				/* minimum rather than keeping the */
      h = valInt(getHeightFont(t->label_font)) + 2 + HIDDEN_TAB_SHRINK;
    }					/* size of a label it no longer has */
    w = max(w, valInt(minsize->w));
    h = max(h, valInt(minsize->h));
    if ( t->closable == ON )		/* room for the button on the right; */
      w += close_button_size(h) + ex/2;	/* <-label_format is left, so it */
					/* lands there */

    if ( t->label_size != minsize )
      setSize(t->label_size, toInt(w), toInt(h));
    else				/* do not write the class-variable! */
      assign(t, label_size, newObject(ClassSize, toInt(w), toInt(h), EAV));
  }

  succeed;
}


/* A tab stack may be told to drop the label of a tab that is the only one
   in it, leaving the whole of the stack to the tab's contents.  The label
   still has a size -- it gets it back as soon as there is a second tab --
   so the height it takes is asked for here rather than read off
   <-label_size, both below and by whoever lays a stack out.
*/

int
labelHeightTab(Tab t)
{ if ( instanceOfObject(t->device, ClassTabStack) &&
       !labelsShownTabStack((TabStack)t->device) )
    return 0;

  return valInt(t->label_size->h);
}


static Int
getLabelHeightTab(Tab t)
{ answer(toInt(labelHeightTab(t)));
}


static int
label_width_tab(Tab t)
{ return labelHeightTab(t) == 0 ? 0 : valInt(t->label_size->w);
}


static Area
getLabelButtonAreaTab(Tab t)
{ int lh = labelHeightTab(t);

  if ( lh > 0 )
  { int s    = close_button_size(lh);
    int ex   = valInt(getAvgCharWidthFont(t->label_font));
    int gap  = ex/2;
    int box  = lh - HIDDEN_TAB_SHRINK;	/* the strip the label is drawn in */
    int fh   = valInt(getHeightFont(t->label_font));
    int top  = HIDDEN_TAB_SHRINK + LOWER_LABEL + (box-fh)/2;
    int base = top + valInt(getAscentFont(t->label_font));

    answer(answerObject(ClassArea,
			toInt(valInt(t->label_offset) +
			      valInt(t->label_size->w) - gap - s),
			toInt(base - s),
			toInt(s), toInt(s), EAV));
  }

  fail;
}


static Area
getCloseButtonAreaTab(Tab t)
{ if ( t->closable == ON )
    answer(getLabelButtonAreaTab(t));

  fail;
}


static status
computeTab(Tab t)
{ if ( notNil(t->request_compute) )
  { int x, y, w, h;
    Area a = t->area;

    obtainClassVariablesObject(t);
    computeLabelTab(t);
    computeGraphicalsDevice((Device) t);

    if ( isDefault(t->size) )		/* implicit size */
    { Cell cell;

      clearArea(a);
      for_cell(cell, t->graphicals)
      { Graphical gr = cell->value;

	unionNormalisedArea(a, gr->area);
      }
      relativeMoveArea(a, t->offset);

      w = valInt(a->w) + 2 * valInt(t->gap->w);
      h = valInt(a->h) + 2 * valInt(t->gap->h);
    } else				/* explicit size */
    { w = valInt(t->size->w);
      h = valInt(t->size->h);
    }

    h += labelHeightTab(t);
    x = valInt(t->offset->x);
    y = valInt(t->offset->y) - labelHeightTab(t);

    CHANGING_GRAPHICAL(t,
	assign(a, x, toInt(x));
	assign(a, y, toInt(y));
	assign(a, w, toInt(w));
	assign(a, h, toInt(h)));

    assign(t, request_compute, NIL);
  }

  succeed;
}

		 /*******************************
		 *	       GEOMETRY		*
		 *******************************/

static status
geometryTab(Tab t, Int x, Int y, Int w, Int h)
{ if ( notDefault(w) || notDefault(h) )
  { Any size;

    if ( isDefault(w) )
      w = getWidthGraphical((Graphical) t);
    if ( isDefault(h) )
      h = getHeightGraphical((Graphical) t);

    size = newObject(ClassSize, w, h, EAV);
    qadSendv(t, NAME_size, 1, &size);
  }

  geometryDevice((Device) t, x, y, w, h);
  requestComputeGraphical(t, DEFAULT);

  succeed;
}

		 /*******************************
		 *	     NAME/LABEL		*
		 *******************************/

status
changedLabelImageTab(Tab t)
{ Elevation e = getClassVariableValueObject(t, NAME_elevation);
  Int eh = e->height;
  BoolObj old = t->displayed;

  t->displayed = ON;
  changedImageGraphical(t,
			t->label_offset, ZERO,
			t->label_size->w,
			add(t->label_size->h, eh));
  t->displayed = old;

  succeed;
}


static status
ChangedLabelTab(Tab t)
{ Int lw, lh;

  if ( isDefault(t->label_size) )
  { lw = lh = ZERO;
  } else
  { lw = t->label_size->w;
    lh = t->label_size->h;
  }

  changedLabelImageTab(t);
  assign(t, request_compute, ON);
  computeTab(t);
  changedLabelImageTab(t);

  if ( notDefault(t->label_size) &&
       ( t->label_size->w != lw ||
	 t->label_size->h != lh
       ) &&
       instanceOfObject(t->device, ClassTabStack) )
  { send(t->device, NAME_layoutLabels, EAV);
  }

  succeed;
}


static status
closableTab(Tab t, BoolObj val)
{ if ( t->closable != val )
  { assign(t, closable, val);
    requestComputeGraphical(t, DEFAULT);   /* the label box changes width */
    if ( instanceOfObject(t->device, ClassTabStack) )
      send(t->device, NAME_layoutLabels, EAV);
  }

  succeed;
}


static status
labelOffsetTab(Tab t, Int offset)
{ if ( t->label_offset != offset )
  { int chl, chr;

    chl = valInt(t->label_offset);
    chr = chl + valInt(t->label_size->w);

    assign(t, label_offset, offset);
    if ( valInt(offset) < chl )
      chl = valInt(offset);		/* shift left */
    else
      chr = valInt(offset) + valInt(t->label_size->w); /* shift right */

    changedImageGraphical(t,
			  toInt(chl), ZERO,
			  toInt(chr), t->label_size->h);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Hack! Maybe we should  make  hidden   tabs  non-displayed,  and make the
tab_stack handle the redraw?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
statusTab(Tab t, Name stat)
{ assignGraphical(t, NAME_status, stat);

  displayedGraphical(t, stat == NAME_hidden ? OFF : ON);

  succeed;
}



		/********************************
		*             REDRAW		*
		********************************/

#define GOTO(p, a, b)	 p->x = (a), p->y = (b), p++
#define RMOVE(p, dx, dy) p->x = p[-1].x + (dx), p->y = p[-1].y + (dy), p++


static status
RedrawAreaTab(Tab t, Area a)
{ int x, y, w, h;
  Elevation e = getClassVariableValueObject(t, NAME_elevation);
  int lh      = labelHeightTab(t);
  int lw      = label_width_tab(t)-1;
  int loff    = valInt(t->label_offset);
  int eh      = valInt(e->height);
  int ex      = valInt(getAvgCharWidthFont(t->label_font));
  int r       = 1;			/* radius of label corners */
  int lflags  = (t->active == OFF ? LABEL_INACTIVE : 0);

  initialiseDeviceGraphical(t, &x, &y, &w, &h);
  w -= 1;
  h -= 1;

  if ( lh == 0 )			/* no label: a plain box */
  { if ( t->status == NAME_onTop )
    { Cell cell;
      Int ax = a->x, ay = a->y;
      Point offset = t->offset;
      int ox = valInt(offset->x);
      int oy = valInt(offset->y);

      r_3d_box(x, y, w, h, 0, e, true);

      assign(a, x, toInt(valInt(a->x) - ox));
      assign(a, y, toInt(valInt(a->y) - oy));
      r_offset(ox, oy);

      d_clip(x+eh, y+eh, w-2*eh, h-2*eh);
      for_cell(cell, t->graphicals)
	RedrawArea(cell->value, a);
      d_clip_done();

      r_offset(-ox, -oy);
      assign(a, x, ax);
      assign(a, y, ay);
    }
  } else if ( t->status == NAME_onTop )
  { fpoint pts[10];
    FPoint p = pts;

    if ( loff == 0 )
    { GOTO(p, x, y+r);			/* top-left of label */
    } else
    { GOTO(p, x, y+lh);			/* top-left of contents */
      RMOVE(p, loff, 0);
      RMOVE(p, 0, -lh+r);		/* top-left of label */
    }
    RMOVE(p, r, -r);
    RMOVE(p, lw-2*r, 0);		/* top-right of label */
    RMOVE(p, r, r);
    RMOVE(p, 0, lh-r);
    GOTO(p, x+w, y+lh);
    RMOVE(p, 0, h-lh);
    RMOVE(p, -w, 0);

    r_3d_rectangular_polygon(p-pts, pts, e, DRAW_3D_FILLED|DRAW_3D_CLOSED);

    RedrawLabelDialogGroup((DialogGroup)t, 0,
			   x+loff+ex, y+HIDDEN_TAB_SHRINK+LOWER_LABEL, lw-2*ex, lh-HIDDEN_TAB_SHRINK,
			   t->label_format, NAME_center,
			   lflags);

    { Cell cell;
      Int ax = a->x, ay = a->y;
      Point offset = t->offset;
      int ox = valInt(offset->x);
      int oy = valInt(offset->y);

      assign(a, x, toInt(valInt(a->x) - ox));
      assign(a, y, toInt(valInt(a->y) - oy));
      r_offset(ox, oy);

      d_clip(x+eh, y+eh, w-2*eh, h-2*eh); /* check if needed! */
      for_cell(cell, t->graphicals)
	RedrawArea(cell->value, a);
      d_clip_done();

      r_offset(-ox, -oy);
      assign(a, x, ax);
      assign(a, y, ay);
    }
  } else /* if ( t->status == NAME_hidden ) */
  { fpoint pts[6];
    FPoint p = pts;
    Colour obg = r_background(DEFAULT);
    static Real dot9;

    if ( !dot9 )
    { dot9 = CtoReal(0.85);
      lockObject(dot9, ON);
    }

    y  += HIDDEN_TAB_SHRINK;
    lh -= HIDDEN_TAB_SHRINK;

    r_fill(x+loff+1, y, lw-1, lh, getReduceColour(obg, dot9));

    GOTO(p, x+loff, y+lh);		/* bottom-left */
    RMOVE(p, 0, -lh+r+1);		/* top-left */
    RMOVE(p, r, -r);
    RMOVE(p, lw-2*r, 0);		/* top-right */
    RMOVE(p, r, r);
    RMOVE(p, 0, lh-r);			/* bottom-right */

    r_3d_rectangular_polygon(p-pts, pts, e, DRAW_3D_FILLED);

    RedrawLabelDialogGroup((DialogGroup)t, 0,
			   x+loff+ex, y+LOWER_LABEL, lw-2*ex, lh,
			   t->label_format, NAME_center,
			   lflags);
  }

  return RedrawAreaGraphical(t, a);
}


		 /*******************************
		 *	       EVENT		*
		 *******************************/

static status
inEventAreaTab(Tab t, Int X, Int Y)
{ int x = valInt(X) - valInt(t->offset->x);
  int y = valInt(Y) - valInt(t->offset->y);

  if ( y < 0 )				/* tab-bar */
  { if ( y > -labelHeightTab(t) &&
	 x > valInt(t->label_offset) &&
	 x < valInt(t->label_offset) + label_width_tab(t) )
      succeed;
  } else
  { if ( t->status == NAME_onTop )
      succeed;
  }

  fail;
}


static status
eventTab(Tab t, EventObj ev)
{ Int X, Y;
  int x, y;

  TRY(get_xy_event(ev, t, OFF, &X, &Y));
  x = valInt(X), y = valInt(Y);

  if ( y < 0 )				/* tab-bar */
  { if ( y > -valInt(t->label_size->h) &&
	 x > valInt(t->label_offset) &&
	 x < valInt(t->label_offset) + valInt(t->label_size->w) )
    { if ( postNamedEvent(ev, (Graphical)t, DEFAULT, NAME_labelEvent) )
	succeed;
    }

    fail;				/* pass to next one */
  }

  if ( t->status == NAME_onTop )
    return eventDialogGroup((DialogGroup) t, ev);

  fail;
}


static status
labelEventTab(Tab t, EventObj ev)
{ if ( isAEvent(ev, NAME_msLeftDown) && t->active != OFF )
  { send(t->device, NAME_onTop, t, EAV);
    succeed;
  }

  fail;
}


static status
flashTab(Tab t, Area a, Int time)
{ if ( notDefault(a) )
    return flashDevice((Device)t, a, DEFAULT);

  a = answerObject(ClassArea,
		   t->label_offset, neg(t->label_size->h),
		   t->label_size->w, t->label_size->h, EAV);

  flashDevice((Device)t, a, DEFAULT);
  doneObject(a);

  succeed;
}


static status
advanceTab(Tab t, Graphical gr, BoolObj propagate, Name direction)
{ if ( isDefault(propagate) )
    propagate = OFF;

  return advanceDevice((Device)t, gr, propagate, direction);
}


static status
activeTab(Tab t, BoolObj active)
{ if ( t->active != active )
  { assign(t, active, active);
    qadSendv(t, NAME_ChangedLabel, 0, NULL);
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_geometry[] =
        { "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_flash[] =
	{ "area=[area]", "time=[int]" };
static char *T_advance[] =
	{ "from=[graphical]*",
	  "propagate=[bool]",
	  "direction=[{forwards,backwards}]"
	};

/* Instance Variables */

static vardecl var_tab[] =
{ IV(NAME_labelSize, "size", IV_GET,
     NAME_layout, "Size of the label-box"),
  SV(NAME_labelOffset, "int", IV_GET|IV_STORE, labelOffsetTab,
     NAME_layout, "X-Offset of label-box"),
  IV(NAME_editableLabel, "bool", IV_BOTH,
     NAME_appearance, "Label can be edited in place"),
  SV(NAME_closable, "bool", IV_GET|IV_STORE, closableTab,
     NAME_appearance, "Label carries a button to close me"),
  SV(NAME_status, "{on_top,hidden}", IV_GET|IV_STORE, statusTab,
     NAME_appearance, "Currently displayed status"),
  IV(NAME_previousTop, "name*", IV_NONE,
     NAME_update, "Name of tab on top before me"),
  SV(NAME_labelFormat, "{left,center,right}", IV_GET|IV_STORE|IV_REDEFINE,
     labelFormatDialogGroup,
     NAME_appearance, "Alignment of label in box")
};

/* Send Methods */

static senddecl send_tab[] =
{ SM(NAME_initialise, 1, "name=[name]", initialiseTab,
     DEFAULT, "Create a new tab-entry"),
  SM(NAME_geometry, 4, T_geometry, geometryTab,
     DEFAULT, "Move/resize tab"),
  SM(NAME_event, 1, "event", eventTab,
     NAME_event, "Process event"),
  SM(NAME_labelEvent, 1, "event", labelEventTab,
     NAME_event, "Process event event on label"),
  SM(NAME_flash, 2, T_flash, flashTab,
     NAME_report, "Flash label of the tab"),
  SM(NAME_position, 1, "point", positionGraphical,
     NAME_area, "Top-left corner of tab"),
  SM(NAME_x, 1, "int", xGraphical,
     NAME_area, "Left-side of tab"),
  SM(NAME_y, 1, "int", yGraphical,
     NAME_area, "Top-side of tab"),
  SM(NAME_compute, 0, NULL, computeTab,
     NAME_update, "Recompute area"),
  SM(NAME_advance, 3, T_advance, advanceTab,
     NAME_focus, "Advance keyboard focus to next item"),
  SM(NAME_active, 1, "bool", activeTab,
     NAME_event, "Enable/disable the tab"),
  SM(NAME_ChangedLabel, 0, NULL, ChangedLabelTab,
     NAME_update, "Add label-area to the update")
};

/* Get Methods */

static getdecl get_tab[] =
{ GM(NAME_position, 0, "point", NULL, getPositionGraphical,
     NAME_area, "Top-left corner of tab"),
  GM(NAME_x, 0, "int", NULL, getXGraphical,
     NAME_area, "Left-side of tab"),
  GM(NAME_y, 0, "int", NULL, getYGraphical,
     NAME_area, "Top-side of tab"),
  GM(NAME_labelHeight, 0, "int", NULL, getLabelHeightTab,
     NAME_layout, "Height my label takes (0 if it is not shown)"),
  GM(NAME_closeButtonArea, 0, "area", NULL, getCloseButtonAreaTab,
     NAME_layout, "Where my close button goes, relative to my area"),
  GM(NAME_labelButtonArea, 0, "area", NULL, getLabelButtonAreaTab,
     NAME_layout, "Where a button on my label goes, relative to my area")
};

/* Resources */

static classvardecl rc_tab[] =
{ RC(NAME_elevation, "elevation", "1",
     "Elevation above environment"),
  RC(NAME_inactiveColour, "colour*",
     "@nil", NULL),
  RC(NAME_gap, "size", "size(15, 8)",
     "Distance between items in X and Y"),
  RC(NAME_labelFont, "font", "normal",
     "Font used to display the label"),
  RC(NAME_labelFormat, "{left,center,right}", "left",
     "Alignment of label in box"),
  RC(NAME_labelSize, "size", "size(50, 24)",
     "Size of box for label"),
  RC(NAME_editableLabel, "bool", "@off",
     "Label can be edited in place"),
  RC(NAME_closable, "bool", "@off",
     "Label carries a button to close the tab")
};

/* Class Declaration */

ClassDecl(tab_decls,
          var_tab, send_tab, get_tab, rc_tab,
          ARGC_INHERIT, NULL);


status
makeClassTab(Class class)
{ declareClass(class, &tab_decls);

  setRedrawFunctionClass(class, RedrawAreaTab);
  setInEventAreaFunctionClass(class, inEventAreaTab);

  succeed;
}
