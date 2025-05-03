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
#include "sdlfont.h"

/**
 * Create a native font resource associated with the specified FontObj on the given display.
 *
 * @param f Pointer to the FontObj to be created.
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_font(FontObj f, DisplayObj d)
{
    return SUCCEED;
}

/**
 * Destroy the native font resource associated with the specified FontObj on the given display.
 *
 * @param f Pointer to the FontObj to be destroyed.
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_destroy_font(FontObj f, DisplayObj d)
{
}

/**
 * Initialize or enumerate the system fonts available on the specified display.
 *
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED if fonts were successfully enumerated or loaded; otherwise, FAIL.
 */
status
ws_system_fonts(DisplayObj d)
{
    return SUCCEED;
}
