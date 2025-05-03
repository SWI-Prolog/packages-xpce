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
ws_draw_scrollbar_arrow(ScrollBar s, int x, int y, int w, int h, Name which, int up)
{
    return SUCCEED;
}

/**
 * Return the height of a scrollbar arrow.
 *
 * @param s Pointer to the ScrollBar.
 * @return The height in pixels.
 */
int
ws_arrow_height_scrollbar(ScrollBar s)
{
    return 10;
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
{
    return SUCCEED;
}

/**
 * Get the default 3D grey colour.
 *
 * @return The Colour object representing 3D grey.
 */
Colour
ws_3d_grey(void)
{
    return NULL;
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
ws_draw_button_face(DialogItem di, int x, int y, int w, int h, int up, int defb, int focus)
{
    return SUCCEED;
}

/**
 * Return the width of a combo box control.
 *
 * @param gr Pointer to the Graphical object.
 * @return Width in pixels.
 */
int
ws_combo_box_width(Graphical gr)
{
    return 20;
}

/**
 * Return the width of a stepper (spinbox arrows).
 *
 * @param gr Pointer to the Graphical object.
 * @return Width in pixels.
 */
int
ws_stepper_width(Graphical gr)
{
    return 16;
}

/**
 * Return the horizontal margin for entry fields.
 *
 * @return Margin in pixels.
 */
int
ws_entry_field_margin(void)
{
    return 4;
}

/**
 * Render an entry field box.
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
{
    return SUCCEED;
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
{
    return SUCCEED;
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
{
    *w = 16;
    *h = 16;
    return SUCCEED;
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
