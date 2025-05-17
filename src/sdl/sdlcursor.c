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
#include "sdlcursor.h"

static Sheet	cursorNames = NIL;

static struct standardCursor
{ char *name;				/* X name of the cursor */
  int	id;				/* X font id of the cursor */
} standard_cursors[] =
{					/* Map X11 names to SDL */
  { "arrow",               SDL_SYSTEM_CURSOR_DEFAULT },
  { "top_left_arrow",      SDL_SYSTEM_CURSOR_DEFAULT },
  { "watch",               SDL_SYSTEM_CURSOR_WAIT },
  { "hand2",               SDL_SYSTEM_CURSOR_POINTER },
  { "xterm",               SDL_SYSTEM_CURSOR_TEXT },
  { "sb_h_double_arrow",   SDL_SYSTEM_CURSOR_EW_RESIZE },
  { "sb_v_double_arrow",   SDL_SYSTEM_CURSOR_NS_RESIZE },
  { "bottom_right_corner", SDL_SYSTEM_CURSOR_SE_RESIZE },
					/* Native SDL names */
  { "default",	   SDL_SYSTEM_CURSOR_DEFAULT },
  { "text",	   SDL_SYSTEM_CURSOR_TEXT },
  { "wait",	   SDL_SYSTEM_CURSOR_WAIT },
  { "crosshair",   SDL_SYSTEM_CURSOR_CROSSHAIR },
  { "progress",	   SDL_SYSTEM_CURSOR_PROGRESS },
  { "nwse_resize", SDL_SYSTEM_CURSOR_NWSE_RESIZE },
  { "nesw_resize", SDL_SYSTEM_CURSOR_NESW_RESIZE },
  { "ew_resize",   SDL_SYSTEM_CURSOR_EW_RESIZE },
  { "ns_resize",   SDL_SYSTEM_CURSOR_NS_RESIZE },
  { "move",	   SDL_SYSTEM_CURSOR_MOVE },
  { "not_allowed", SDL_SYSTEM_CURSOR_NOT_ALLOWED },
  { "pointer",	   SDL_SYSTEM_CURSOR_POINTER },
  { "nw_resize",   SDL_SYSTEM_CURSOR_NW_RESIZE },
  { "n_resize",	   SDL_SYSTEM_CURSOR_N_RESIZE },
  { "ne_resize",   SDL_SYSTEM_CURSOR_NE_RESIZE },
  { "e_resize",	   SDL_SYSTEM_CURSOR_E_RESIZE },
  { "se_resize",   SDL_SYSTEM_CURSOR_SE_RESIZE },
  { "s_resize",	   SDL_SYSTEM_CURSOR_S_RESIZE },
  { "sw_resize",   SDL_SYSTEM_CURSOR_SW_RESIZE },
  { "w_resize",	   SDL_SYSTEM_CURSOR_W_RESIZE },
  { NULL,	   0 }
};

/**
 * Initialize the cursor font resources.
 *
 * This function sets up any necessary font resources required for
 * cursor rendering.  It should be called during the initialization
 * phase of the application.
 */
void
ws_init_cursor_font(void)
{ struct standardCursor *sc;

  cursorNames = globalObject(NAME_cursorNames, ClassSheet, EAV);

  for(sc = standard_cursors; sc->name; sc++)
    valueSheet(cursorNames, (Any) CtoName(sc->name), toInt(sc->id));
}

/**
 * Retrieve the font index corresponding to a given cursor name.
 *
 * @param name Pointer to the Name object representing the cursor name.
 * @return The font index associated with the specified cursor name.
 */

Int
ws_cursor_font_index(Name name)
{ Int idx = getValueSheet(cursorNames, name);
  if ( !idx )
  { Cprintf("Unknown cursor %s\n", pp(name));
    return toInt(SDL_SYSTEM_CURSOR_DEFAULT);
  }

  return idx;
}

/**
 * Create a native cursor resource associated with the specified CursorObj on the given display.
 *
 * @param c Pointer to the CursorObj to be created.
 * @param d Pointer to the DisplayObj representing the display context.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_create_cursor(CursorObj c, DisplayObj d)
{ if ( !c->ws_ref )
  { if ( isNil(c->image) )
    { Int idx = ws_cursor_font_index(c->name);
      assert(idx);
      SDL_Cursor *cursor = SDL_CreateSystemCursor(valInt(idx));
      c->ws_ref = cursor;
    } else
    { Cprintf("stub: No image cursors yet\n");
      /* Use SDL_CreateColorCursor() */
      fail;
    }
  }

  succeed;
}

/**
 * Destroy the native cursor resource associated with the specified CursorObj on the given display.
 *
 * @param c Pointer to the CursorObj to be destroyed.
 * @param d Pointer to the DisplayObj representing the display context.
 */
void
ws_destroy_cursor(CursorObj c, DisplayObj d)
{ if ( c->ws_ref )
  { SDL_DestroyCursor(c->ws_ref);
    c->ws_ref = NULL;
  }
}
