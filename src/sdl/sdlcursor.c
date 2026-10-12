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
#include "sdlevent.h"
#include "sdlwindow.h"

static Sheet	cursorNames = NIL;

typedef struct standardCursor
{ char *name;				/* Name of the cursor */
  int	id;				/* SDL_SystemCursor */
  char *summary;			/* Description (SDL cursors only) */
} standard_cursor;

static standard_cursor x11_cursors[] =
{					/* Map old (X11) cursor names to SDL */
  { "arrow",               SDL_SYSTEM_CURSOR_DEFAULT },
  { "mouse",               SDL_SYSTEM_CURSOR_DEFAULT },
  { "win_arrow",           SDL_SYSTEM_CURSOR_DEFAULT },
  { "top_left_arrow",      SDL_SYSTEM_CURSOR_DEFAULT },
  { "cross_reverse",       SDL_SYSTEM_CURSOR_DEFAULT },
  { "cross",		   SDL_SYSTEM_CURSOR_CROSSHAIR },
  { "plus",		   SDL_SYSTEM_CURSOR_CROSSHAIR },
  { "watch",               SDL_SYSTEM_CURSOR_WAIT },
  { "hand2",               SDL_SYSTEM_CURSOR_POINTER },
  { "xterm",               SDL_SYSTEM_CURSOR_TEXT },
  { "win_ibeam",           SDL_SYSTEM_CURSOR_TEXT },
  { "sb_h_double_arrow",   SDL_SYSTEM_CURSOR_EW_RESIZE },
  { "win_sizewe",          SDL_SYSTEM_CURSOR_EW_RESIZE },
  { "sb_v_double_arrow",   SDL_SYSTEM_CURSOR_NS_RESIZE },
  { "win_sizens",          SDL_SYSTEM_CURSOR_NS_RESIZE },
  { "bottom_side",         SDL_SYSTEM_CURSOR_S_RESIZE },
  { "top_side",            SDL_SYSTEM_CURSOR_N_RESIZE },
  { "left_side",           SDL_SYSTEM_CURSOR_W_RESIZE },
  { "right_side",          SDL_SYSTEM_CURSOR_E_RESIZE },
  { "bottom_right_corner", SDL_SYSTEM_CURSOR_SE_RESIZE },
  { "bottom_left_corner",  SDL_SYSTEM_CURSOR_SW_RESIZE },
  { "top_left_corner",     SDL_SYSTEM_CURSOR_NW_RESIZE },
  { "top_right_corner",    SDL_SYSTEM_CURSOR_NE_RESIZE },
  { NULL,		   0 }
};

static standard_cursor sdl_cursors[] =
{					/* Native SDL names, in menu order */
  { "default",	   SDL_SYSTEM_CURSOR_DEFAULT,	  "Normal pointer" },
  { "pointer",	   SDL_SYSTEM_CURSOR_POINTER,
    "Pointing hand, e.g., for a link" },
  { "text",	   SDL_SYSTEM_CURSOR_TEXT,	  "Text insertion (I-beam)" },
  { "crosshair",   SDL_SYSTEM_CURSOR_CROSSHAIR,
    "Crosshair for precise selection" },
  { "move",	   SDL_SYSTEM_CURSOR_MOVE,	  "Move something" },
  { "wait",	   SDL_SYSTEM_CURSOR_WAIT,	  "Busy" },
  { "progress",	   SDL_SYSTEM_CURSOR_PROGRESS,	  "Busy in the background" },
  { "not_allowed", SDL_SYSTEM_CURSOR_NOT_ALLOWED, "Operation not allowed" },
  { "ew_resize",   SDL_SYSTEM_CURSOR_EW_RESIZE,	  "Resize horizontally" },
  { "ns_resize",   SDL_SYSTEM_CURSOR_NS_RESIZE,	  "Resize vertically" },
  { "nwse_resize", SDL_SYSTEM_CURSOR_NWSE_RESIZE, "Resize diagonally (\\)" },
  { "nesw_resize", SDL_SYSTEM_CURSOR_NESW_RESIZE, "Resize diagonally (/)" },
  { "n_resize",	   SDL_SYSTEM_CURSOR_N_RESIZE,	  "Resize at the top" },
  { "e_resize",	   SDL_SYSTEM_CURSOR_E_RESIZE,	  "Resize at the right" },
  { "s_resize",	   SDL_SYSTEM_CURSOR_S_RESIZE,	  "Resize at the bottom" },
  { "w_resize",	   SDL_SYSTEM_CURSOR_W_RESIZE,	  "Resize at the left" },
  { "ne_resize",   SDL_SYSTEM_CURSOR_NE_RESIZE,
    "Resize at the top-right corner" },
  { "nw_resize",   SDL_SYSTEM_CURSOR_NW_RESIZE,
    "Resize at the top-left corner" },
  { "se_resize",   SDL_SYSTEM_CURSOR_SE_RESIZE,
    "Resize at the bottom-right corner" },
  { "sw_resize",   SDL_SYSTEM_CURSOR_SW_RESIZE,
    "Resize at the bottom-left corner" },
  { NULL,	   0,				  NULL }
};

/* display ->busy_cursor names one cursor for all the frames at once.  SDL
 * has a single cursor for the application, so hold it here and leave the
 * per-window cursor alone while it is set: updateCursorWindow() runs after
 * every event and would otherwise take it straight off again.
 */

static SDL_Cursor *busy_cursor;

bool
ws_busy_cursor(void)
{ return busy_cursor != NULL;
}


/* Put the normal cursor back when the busy period ends.  We cannot
 * leave that to updateCursorWindow() after the next event: the event
 * that started the busy period is often the last one for a while (a
 * key that makes the debugger continue, for example), and it is the
 * window under the pointer that decides the shape, which need not be
 * the window that got the event.  The cursor would keep the busy shape
 * until the user moves the mouse.
 */

static void
restore_cursor(void)
{ PceWindow sw = ws_pointer_window();

  DEBUG(NAME_cursor, Cprintf("Busy cursor done; restoring from %s\n",
			     sw ? pp(sw) : "(no pointer window)"));

  if ( sw && ws_created_window(sw) )
  { updateCursorWindow(sw);
  } else				/* pointer not in one of our */
  { ASSERT_SDL_MAIN();			/* windows */
    SDL_SetCursor(SDL_GetDefaultCursor());
  }
}


void
ws_set_busy_cursor(CursorObj c)
{ if ( !c || isNil(c) || isDefault(c) )   /* isDefault: unresolved */
  { if ( busy_cursor )
    { busy_cursor = NULL;
      restore_cursor();
    }
  } else
  { SDL_Cursor *sc = pceCursor2SDL_Cursor(c);

    if ( sc )
    { ASSERT_SDL_MAIN();
      busy_cursor = sc;
      SDL_SetCursor(sc);
    }
  }
}


/**
 * Initialize the cursor font resources.
 *
 * This function sets up any necessary font resources required for
 * cursor rendering.  It should be called during the initialization
 * phase of the application.
 */
void
ws_init_cursor_font(void)
{ standard_cursor *sc;

  cursorNames = globalObject(NAME_cursorNames, ClassSheet, EAV);

  for(sc = sdl_cursors; sc->name; sc++)
    valueSheet(cursorNames, (Any) CtoName(sc->name), toInt(sc->id));
  for(sc = x11_cursors; sc->name; sc++)
    valueSheet(cursorNames, (Any) CtoName(sc->name), toInt(sc->id));
}

/**
 * Tell where a predefined cursor comes from.
 *
 * @param name Name of a predefined cursor.
 * @return `standard` for a native SDL cursor, `x11` for a name kept
 * for compatibility with the X11 cursor font, or FAIL if `name` is not
 * a predefined cursor.
 */

static standard_cursor *
find_cursor(standard_cursor *table, const char *s)
{ for(standard_cursor *sc = table; sc->name; sc++)
  { if ( streq(sc->name, s) )
      return sc;
  }

  return NULL;
}

/**
 * Describe a predefined cursor.
 *
 * @param name Name of a predefined cursor.
 * @return The description of the cursor.  For an old X11 name this is
 * the description of the standard cursor it maps to.  NULL if `name`
 * is not a predefined cursor.
 */

const char *
ws_cursor_summary(Name name)
{ const char *s = strName(name);
  standard_cursor *sc;

  if ( (sc = find_cursor(sdl_cursors, s)) )
    return sc->summary;
  if ( (sc = find_cursor(x11_cursors, s)) )
  { for(standard_cursor *n = sdl_cursors; n->name; n++)
    { if ( n->id == sc->id )
	return n->summary;
    }
  }

  return NULL;
}

Name
ws_cursor_kind(Name name)
{ const char *s = strName(name);

  if ( find_cursor(x11_cursors, s) )
    return NAME_x11;
  if ( find_cursor(sdl_cursors, s) )
    return NAME_standard;

  fail;
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
 * Create a native cursor resource associated with the specified
 * CursorObj on the given display.
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
      ASSERT_SDL_MAIN();
      SDL_Cursor *cursor = SDL_CreateSystemCursor(valInt(idx));
      c->ws_ref = cursor;
    } else
    { SDL_Surface *sdl_surf = pceImage2SDL_Surface(c->image);
      ASSERT_SDL_MAIN();
      SDL_Cursor *cursor = SDL_CreateColorCursor(sdl_surf,
						 valInt(c->hot_spot->x),
						 valInt(c->hot_spot->y));
      /* SDL surface is owned by cursor */
      c->ws_ref = cursor;
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
  { ASSERT_SDL_MAIN();
    SDL_DestroyCursor(c->ws_ref);
    c->ws_ref = NULL;
  }
}
