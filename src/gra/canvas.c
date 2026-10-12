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

#include <h/kernel.h>
#include <h/graphics.h>

/* Class `canvas' is a graphical to draw on using operations that are
 * modelled after Cairo: build a path using ->move_to, ->line_to, etc.
 * and ->stroke or ->fill it using the current ->source, ->line_width,
 * ->stroke_style and transformation.  What is drawn is recorded, so the
 * canvas redraws without application code and remains vectors when
 * scaled or written as PDF or SVG.  The recording and drawing state are
 * kept by the backend: see ws_canvas_op() in sdldraw.c.
 */

static status
initialiseCanvas(Canvas c, Int w, Int h)
{ initialiseGraphical(c, ZERO, ZERO, w, h);
  ws_canvas_size(c, valInt(w), valInt(h));

  succeed;
}


static status
unlinkCanvas(Canvas c)
{ ws_canvas_destroy(c);

  return unlinkGraphical((Graphical)c);
}


static status
RedrawAreaCanvas(Canvas c, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(c, &x, &y, &w, &h);
  r_canvas(c, x, y);

  return RedrawAreaGraphical(c, a);
}


static status
geometryCanvas(Canvas c, Int x, Int y, Int w, Int h)
{ Int ow = c->area->w, oh = c->area->h;

  TRY(geometryGraphical(c, x, y, w, h));
  if ( c->area->w != ow || c->area->h != oh )
    ws_canvas_size(c, valInt(c->area->w), valInt(c->area->h));

  succeed;
}


static status
canvas_op(Canvas c, Name op, int argc, Any *argv)
{ bool changed;
  status rc = ws_canvas_op(c, op, argc, argv, &changed);

  if ( changed )
    changedEntireImageGraphical(c);

  return rc;
}

#define CANVAS_OP0(func, name) \
static status func(Canvas c) \
{ return canvas_op(c, name, 0, NULL); \
}
#define CANVAS_OP1(func, name) \
static status func(Canvas c, Any a0) \
{ Any av[] = {a0}; return canvas_op(c, name, 1, av); \
}
#define CANVAS_OP2(func, name) \
static status func(Canvas c, Any a0, Any a1) \
{ Any av[] = {a0, a1}; return canvas_op(c, name, 2, av); \
}
#define CANVAS_OP3(func, name) \
static status func(Canvas c, Any a0, Any a1, Any a2) \
{ Any av[] = {a0, a1, a2}; return canvas_op(c, name, 3, av); \
}
#define CANVAS_OP4(func, name) \
static status func(Canvas c, Any a0, Any a1, Any a2, Any a3) \
{ Any av[] = {a0, a1, a2, a3}; return canvas_op(c, name, 4, av); \
}
#define CANVAS_OP5(func, name) \
static status func(Canvas c, Any a0, Any a1, Any a2, Any a3, Any a4) \
{ Any av[] = {a0, a1, a2, a3, a4}; return canvas_op(c, name, 5, av); \
}
#define CANVAS_OP6(func, name) \
static status func(Canvas c, Any a0, Any a1, Any a2, Any a3, Any a4, Any a5) \
{ Any av[] = {a0, a1, a2, a3, a4, a5}; return canvas_op(c, name, 6, av); \
}

CANVAS_OP2(moveToCanvas,	NAME_moveTo)
CANVAS_OP2(lineToCanvas,	NAME_lineTo)
CANVAS_OP2(relMoveToCanvas,	NAME_relMoveTo)
CANVAS_OP2(relLineToCanvas,	NAME_relLineTo)
CANVAS_OP6(curveToCanvas,	NAME_curveTo)
CANVAS_OP5(arcCanvas,		NAME_arc)
CANVAS_OP5(arcNegativeCanvas,	NAME_arcNegative)
CANVAS_OP4(rectangleCanvas,	NAME_rectangle)
CANVAS_OP0(closePathCanvas,	NAME_closePath)
CANVAS_OP0(newPathCanvas,	NAME_newPath)
CANVAS_OP0(newSubPathCanvas,	NAME_newSubPath)
CANVAS_OP1(pathCanvas,		NAME_path)
CANVAS_OP1(strokeCanvas,	NAME_stroke)
CANVAS_OP1(fillCanvas,		NAME_fill)
CANVAS_OP1(clipCanvas,		NAME_clip)
CANVAS_OP0(resetClipCanvas,	NAME_resetClip)
CANVAS_OP1(paintCanvas,		NAME_paint)
CANVAS_OP1(sourceCanvas,	NAME_source)
CANVAS_OP1(lineWidthCanvas,	NAME_lineWidth)
CANVAS_OP1(strokeStyleCanvas,	NAME_strokeStyle)
CANVAS_OP1(fillRuleCanvas,	NAME_fillRule)
CANVAS_OP0(saveStateCanvas,	NAME_saveState)
CANVAS_OP0(restoreStateCanvas,	NAME_restoreState)
CANVAS_OP2(translateCanvas,	NAME_translate)
CANVAS_OP2(scaleCanvas,		NAME_scale)
CANVAS_OP1(rotateCanvas,	NAME_rotate)
CANVAS_OP1(transformCanvas,	NAME_transform)
CANVAS_OP0(identityCanvas,	NAME_identity)
CANVAS_OP3(imageCanvas,		NAME_image)


static status
textCanvas(Canvas c, CharArray markup, Num x, Num y,
	   FontObj font, Num width, Name alignment)
{ if ( isDefault(font) )
    font = getClassVariableValueObject(c, NAME_font);

  Any av[] = { markup, x, y, font, width, alignment };
  return canvas_op(c, NAME_text, 6, av);
}


static status
clearCanvas(Canvas c)
{ ws_canvas_clear(c, valInt(c->area->w), valInt(c->area->h));
  changedEntireImageGraphical(c);

  succeed;
}


/* Draw a graphical on the canvas, as image->draw_in.  The current
 * transformation of the canvas applies.
 */

static status
drawInCanvas(Canvas c, Graphical gr, Point pos)
{ Int oldx = DEFAULT, oldy = DEFAULT;
  Device dev = NIL;
  BoolObj olddisplayed = gr->displayed;

  if ( notDefault(pos) )
  { oldx = gr->area->x;
    oldy = gr->area->y;
    dev = gr->device;
    gr->device = NIL;
    setGraphical(gr, pos->x, pos->y, DEFAULT, DEFAULT);
  }

  displayedGraphical(gr, ON);
  ComputeGraphical(gr);
  if ( d_canvas(c) )
  { RedrawArea(gr, gr->area);
    d_done();
    changedEntireImageGraphical(c);
  }

  displayedGraphical(gr, olddisplayed);
  if ( notDefault(oldx) )
  { setGraphical(gr, oldx, oldy, DEFAULT, DEFAULT);
    gr->device = dev;
  }

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

static char *T_size[]	   = { "width=int", "height=int" };
static char *T_xy[]	   = { "x=num", "y=num" };
static char *T_dxy[]	   = { "dx=num", "dy=num" };
static char *T_curveTo[]   = { "x1=num", "y1=num", "x2=num", "y2=num",
			       "x3=num", "y3=num" };
static char *T_arc[]	   = { "xc=num", "yc=num", "radius=num",
			       "angle1=num", "angle2=num" };
static char *T_rectangle[] = { "x=num", "y=num", "width=num", "height=num" };
static char *T_scale[]	   = { "sx=num", "sy=[num]" };
static char *T_text[]	   = { "markup=char_array", "x=num", "y=num",
			       "font=[font]", "width=[num]*",
			       "alignment=[{left,center,right}]" };
static char *T_image[]	   = { "image=image", "x=num", "y=num" };
static char *T_geometry[]  = { "x=[int]", "y=[int]", "width=[int]",
			       "height=[int]" };
static char *T_drawIn[]	   = { "graphical", "at=[point]" };

static vardecl var_canvas[] =
{ IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_storage, "Recording and drawing state")
};

static senddecl send_canvas[] =
{ SM(NAME_initialise, 2, T_size, initialiseCanvas,
     DEFAULT, "Create a canvas of width x height pixels"),
  SM(NAME_unlink, 0, NULL, unlinkCanvas,
     DEFAULT, "Destroy the recording"),
  SM(NAME_geometry, 4, T_geometry, geometryCanvas,
     DEFAULT, "Move and resize (keeps the drawing)"),
  SM(NAME_clear, 0, NULL, clearCanvas,
     NAME_draw, "Erase the drawing and reset the drawing state"),
  SM(NAME_drawIn, 2, T_drawIn, drawInCanvas,
     NAME_draw, "Draw a graphical on the canvas"),

  SM(NAME_moveTo, 2, T_xy, moveToCanvas,
     NAME_path, "Start a new sub-path at (x,y)"),
  SM(NAME_lineTo, 2, T_xy, lineToCanvas,
     NAME_path, "Add a line to (x,y)"),
  SM(NAME_relMoveTo, 2, T_dxy, relMoveToCanvas,
     NAME_path, "Start a new sub-path relative to the current point"),
  SM(NAME_relLineTo, 2, T_dxy, relLineToCanvas,
     NAME_path, "Add a line relative to the current point"),
  SM(NAME_curveTo, 6, T_curveTo, curveToCanvas,
     NAME_path, "Add a cubic Bezier curve to (x3,y3)"),
  SM(NAME_arc, 5, T_arc, arcCanvas,
     NAME_path, "Add a circular arc in the direction of increasing angles"),
  SM(NAME_arcNegative, 5, T_arc, arcNegativeCanvas,
     NAME_path, "Add a circular arc in the direction of decreasing angles"),
  SM(NAME_rectangle, 4, T_rectangle, rectangleCanvas,
     NAME_path, "Add a closed rectangle"),
  SM(NAME_closePath, 0, NULL, closePathCanvas,
     NAME_path, "Close the current sub-path"),
  SM(NAME_newPath, 0, NULL, newPathCanvas,
     NAME_path, "Discard the current path"),
  SM(NAME_newSubPath, 0, NULL, newSubPathCanvas,
     NAME_path, "Start a new sub-path without a current point"),
  SM(NAME_path, 1, "path", pathCanvas,
     NAME_path, "Add the points of a path object"),

  SM(NAME_stroke, 1, "preserve=[bool]", strokeCanvas,
     NAME_draw, "Draw the line of the path (@on: keep the path)"),
  SM(NAME_fill, 1, "preserve=[bool]", fillCanvas,
     NAME_draw, "Fill the path (@on: keep the path)"),
  SM(NAME_paint, 1, "alpha=[num]", paintCanvas,
     NAME_draw, "Paint the source everywhere within the clip"),
  SM(NAME_text, 6, T_text, textCanvas,
     NAME_draw, "Draw Pango markup with its top-left corner at (x,y)"),
  SM(NAME_image, 3, T_image, imageCanvas,
     NAME_draw, "Draw an image with its top-left corner at (x,y)"),

  SM(NAME_clip, 1, "preserve=[bool]", clipCanvas,
     NAME_clip, "Restrict drawing to the path (@on: keep the path)"),
  SM(NAME_resetClip, 0, NULL, resetClipCanvas,
     NAME_clip, "Remove the clip"),

  SM(NAME_source, 1, "colour|gradient|image", sourceCanvas,
     NAME_state, "Paint for ->stroke, ->fill, ->paint and ->text"),
  SM(NAME_lineWidth, 1, "num", lineWidthCanvas,
     NAME_state, "Width of lines drawn by ->stroke"),
  SM(NAME_strokeStyle, 1, TYPE_TEXTURE, strokeStyleCanvas,
     NAME_state, "Dashes, caps and joins of lines drawn by ->stroke"),
  SM(NAME_fillRule, 1, "{winding,even_odd}", fillRuleCanvas,
     NAME_state, "Which areas ->fill and ->clip consider inside"),
  SM(NAME_saveState, 0, NULL, saveStateCanvas,
     NAME_state, "Save the drawing state"),
  SM(NAME_restoreState, 0, NULL, restoreStateCanvas,
     NAME_state, "Restore the state of the matching ->save_state"),

  SM(NAME_translate, 2, T_dxy, translateCanvas,
     NAME_transform, "Move the origin"),
  SM(NAME_scale, 2, T_scale, scaleCanvas,
     NAME_transform, "Scale the coordinates"),
  SM(NAME_rotate, 1, "degrees=num", rotateCanvas,
     NAME_transform, "Rotate the coordinates (clockwise)"),
  SM(NAME_transform, 1, "transform", transformCanvas,
     NAME_transform, "Apply an affine transformation"),
  SM(NAME_identity, 0, NULL, identityCanvas,
     NAME_transform, "Reset the transformation")
};

#define get_canvas NULL

static classvardecl rc_canvas[] =
{ RC(NAME_font, "font", "normal",
     "Default font for ->text")
};

static Name canvas_termnames[] = { NAME_width, NAME_height };

ClassDecl(canvas_decls,
	  var_canvas, send_canvas, get_canvas, rc_canvas,
	  2, canvas_termnames);


status
makeClassCanvas(Class class)
{ declareClass(class, &canvas_decls);
  setRedrawFunctionClass(class, RedrawAreaCanvas);

  succeed;
}
