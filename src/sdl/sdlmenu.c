/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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
#include <stdbool.h>
#include "sdlmenu.h"

/**
 * Draw a scrollbar arrow at the specified location and direction.
 *
 * @param s Pointer to the ScrollBar.
 * @param x The x-coordinate of the arrow box.
 * @param y The y-coordinate of the arrow box.
 * @param w The width of the arrow box.
 * @param h The height of the arrow box.
 * @param which Name indicating which arrow to draw (e.g., up, down).
 * @param up Boolean indicating whether the arrow is in the up state.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_draw_scrollbar_arrow(ScrollBar s,
			int x, int y, int w, int h,
			Name which, int up)
{ fail;
}

/**
 * Return the height of a scrollbar arrow.
 *
 * @param s Pointer to the ScrollBar.
 * @return The height in pixels.
 */
int
ws_arrow_height_scrollbar(ScrollBar s)
{ return -1;
}

/**
 * Draw a scrollbar thumb (slider handle).
 *
 * @param x The x-coordinate.
 * @param y The y-coordinate.
 * @param w The width.
 * @param h The height.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_draw_sb_thumb(int x, int y, int w, int h)
{ fail;
}

/**
 * Get the default 3D grey colour.
 *
 * @return The Colour object representing 3D grey.
 */
Colour
ws_3d_grey(void)
{ static Colour c;

  if ( !c )
    c = newObject(ClassColour, CtoKeyword("grey60"), EAV);

  return c;
}

/**
 * Draw a button face with 3D effects and state indication.
 *
 * @param di Pointer to the DialogItem.
 * @param x The x-coordinate.
 * @param y The y-coordinate.
 * @param w The width.
 * @param h The height.
 * @param up Boolean indicating button state.
 * @param defb Boolean indicating default button.
 * @param focus Boolean indicating focus state.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_draw_button_face(DialogItem di, int x, int y, int w, int h,
		    int up, int defb, int focus)
{ fail;
}

		 /*******************************
		 *	      TEXTITEM		*
		 *******************************/

static Elevation noedit_elevation;
static Elevation edit_elevation;
static Elevation button_elevation;


static void
init_entry_resources()
{ static bool done = false;

  if ( !done )
  { done = true;

    noedit_elevation = globalObject(NIL, ClassElevation, NIL,
				    toInt(-1), EAV);
    edit_elevation   = globalObject(NIL, ClassElevation, NIL,
				    toInt(-1), WHITE_COLOUR, EAV);
    button_elevation = getClassVariableValueClass(ClassButton,
						  NAME_elevation);
  }
}

/**
 * Return the width of a combo box control.
 *
 * @param gr Pointer to the Graphical object.
 * @return Width in pixels.
 */
int
ws_combo_box_width(Graphical gr)
{ return dpi_scale(gr, 14);
}

/**
 * Return the width of a stepper (spinbox arrows).
 *
 * @param gr Pointer to the Graphical object.
 * @return Width in pixels.
 */
int
ws_stepper_width(Graphical gr)
{ return ws_combo_box_width(gr);
}

/**
 * Return the horizontal margin for entry fields.
 *
 * @return Margin in pixels.
 */
int
ws_entry_field_margin(void)
{ return 1;
}

/**
 * ws_entry_field() is used by classes that need to create an editable
 * field  of specified  dimensions. If  the  field happens  to be  not
 * editable now, this is indicated by `editable'.
 *
 * @param gr Pointer to the Graphical object.
 * @param x The x-coordinate.
 * @param y The y-coordinate.
 * @param w The width.
 * @param h The height.
 * @param flags Rendering flags.
 * @return SUCCEED on success; otherwise, FAIL.
 */

status
ws_entry_field(Graphical gr, int x, int y, int w, int h, int flags)
{ init_entry_resources();

  if ( !(flags & TEXTFIELD_EDITABLE) )
  { r_3d_box(x, y, w, h, 0, noedit_elevation, TRUE);
  } else
  { r_3d_box(x, y, w, h, 0, edit_elevation, TRUE);

    if ( flags & TEXTFIELD_COMBO )
    { int iw = valInt(SCROLL_DOWN_IMAGE->size->w);
      int ih = valInt(SCROLL_DOWN_IMAGE->size->h);
      int iy = y+2 + (h-4-valInt(SCROLL_DOWN_IMAGE->size->h))/2;
      int cw = ws_combo_box_width(gr);
      int up = !(flags & TEXTFIELD_COMBO_DOWN);

      if ( cw < 0 ) cw = dpi_scale(NULL, 14);
      r_3d_box(x+w-cw-2, y+2, cw, h-4, 0, button_elevation, up);
      r_image(SCROLL_DOWN_IMAGE, 0, 0, x+w-cw+(cw-iw)/2-2, iy, iw, ih, ON);
    }
    if ( flags & TEXTFIELD_STEPPER )
    { int cw = ws_stepper_width(gr);
      int bh = (h-4)/2;
      int b1up, b2up;

      if ( cw < 0 ) cw = dpi_scale(NULL, 14);
      b1up = !(flags & TEXTFIELD_INCREMENT);
      b2up = !(flags & TEXTFIELD_DECREMENT);

      r_3d_box(x+w-cw-2, y+2,    cw, bh, 0, button_elevation, b1up);
      r_3d_box(x+w-cw-2, y+2+bh, cw, bh, 0, button_elevation, b2up);

      { int iw = valInt(INT_ITEM_IMAGE->size->w)/2;
	int ih = valInt(INT_ITEM_IMAGE->size->h);
	int ix = x+w-2-(cw+iw)/2;
	int dy = (bh-ih+1)/2;

	r_image(INT_ITEM_IMAGE, 0,  0, ix, y+2+dy,      iw, ih, ON);
	r_image(INT_ITEM_IMAGE, iw, 0, ix, y+h-2-dy-ih, iw, ih, ON);
      }
    }
  }

  succeed;
}

/**
 * Draw a checkbox widget.
 *
 * @param x The x-coordinate.
 * @param y The y-coordinate.
 * @param w The width.
 * @param h The height.
 * @param b Border size or state.
 * @param flags Rendering flags.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_draw_checkbox(int x, int y, int w, int h, int b, int flags)
{ fail;
}

/**
 * Compute the size of a checkbox widget.
 *
 * @param flags Flags that affect size calculation.
 * @param w Pointer to output width.
 * @param h Pointer to output height.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_checkbox_size(int flags, int *w, int *h)
{ *w = 0;
  *h = 0;

  fail;
}

/**
 * Show a message box with the specified message and flags.
 *
 * @param msg Message to display.
 * @param flags Style and modality flags.
 * @return Selected button code or -1 on failure.
 */
int
ws_message_box(Any msg, int flags)
{
    return 0;
}
