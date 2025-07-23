/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2013, University of Amsterdam
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
#include "mscolour.h"
#include <sdl/sdlcolour.h>
#include <windows.h>

struct system_colour
{ char *name;
  int  id;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Windows system colors as obtained from GetSysColor().

Updated with new colors at Jul 23, 2005 using MSVC 6.0 documentation
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const struct system_colour window_colours[] =
{ { "sys_scrollbar_background",	   COLOR_BTNFACE },
  { "sys_dialog_background",	   COLOR_MENU },
  { "sys_dialog_foreground",	   COLOR_MENUTEXT },
  { "sys_window_background",	   COLOR_WINDOW },
  { "sys_window_foreground",	   COLOR_WINDOWTEXT },
  { "sys_relief",		   COLOR_BTNFACE },
  { "sys_shadow",		   COLOR_BTNSHADOW },
  { "sys_inactive",		   COLOR_GRAYTEXT },

  { "win_3ddkshadow",		   COLOR_3DDKSHADOW },
  { "win_3dface",		   COLOR_3DFACE },
  { "win_3dhighlight",		   COLOR_3DHIGHLIGHT },
  { "win_3dhilight",		   COLOR_3DHILIGHT },
  { "win_3dlight",		   COLOR_3DLIGHT },
  { "win_3dshadow",		   COLOR_3DSHADOW },
  { "win_activeborder",		   COLOR_ACTIVEBORDER },
  { "win_activecaption",	   COLOR_ACTIVECAPTION },
  { "win_activecaption",	   COLOR_ACTIVECAPTION },
  { "win_appworkspace",		   COLOR_APPWORKSPACE },
  { "win_background",		   COLOR_BACKGROUND },
  { "win_btnface",		   COLOR_BTNFACE },
  { "win_btnhighlight",		   COLOR_BTNHIGHLIGHT },
  { "win_btnhilight",		   COLOR_BTNHILIGHT },
  { "win_btnshadow",		   COLOR_BTNSHADOW },
  { "win_btntext",		   COLOR_BTNTEXT },
  { "win_captiontext",		   COLOR_CAPTIONTEXT },
  { "win_desktop",		   COLOR_DESKTOP },
#ifdef COLOR_GRADIENTACTIVECAPTION
  { "win_gradientactivecaption",   COLOR_GRADIENTACTIVECAPTION },
  { "win_gradientinactivecaption", COLOR_GRADIENTINACTIVECAPTION },
#endif
  { "win_graytext",		   COLOR_GRAYTEXT },
  { "win_highlight",		   COLOR_HIGHLIGHT },
  { "win_highlighttext",	   COLOR_HIGHLIGHTTEXT },
#ifdef COLOR_HOTLIGHT
  { "win_hotlight",		   COLOR_HOTLIGHT },
#endif
  { "win_inactiveborder",	   COLOR_INACTIVEBORDER },
  { "win_inactivecaption",	   COLOR_INACTIVECAPTION },
  { "win_inactivecaptiontext",	   COLOR_INACTIVECAPTIONTEXT },
  { "win_infobk",		   COLOR_INFOBK },
  { "win_infotext",		   COLOR_INFOTEXT },
  { "win_menu",			   COLOR_MENU },
  { "win_menutext",		   COLOR_MENUTEXT },
  { "win_scrollbar",		   COLOR_SCROLLBAR },
  { "win_window",		   COLOR_WINDOW },
  { "win_windowframe",		   COLOR_WINDOWFRAME },
  { "win_windowtext",		   COLOR_WINDOWTEXT
 },

  { NULL,			   0 }
};


static void
ws_system_colour(HashTable ColourNames, const char *name, COLORREF rgb)
{ int r = GetRValue(rgb);
  int g = GetGValue(rgb);
  int b = GetBValue(rgb);

  COLORRGBA rgba = RGBA(r, g, b, 255);
  appendHashTable(ColourNames, CtoKeyword(name), toInt(rgba));
}


void
ws_system_colours(HashTable ColourNames)
{ const struct system_colour *sc = window_colours;

  for( ; sc->name; sc++ )
  { DWORD rgb = GetSysColor(sc->id);

    ws_system_colour(ColourNames, sc->name, rgb);
  }
}
