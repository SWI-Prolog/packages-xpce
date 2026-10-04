/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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
{ /* The common sys_* names (see load_system_colours() in sdlcolour.c) */
  { "sys_window_background",	   COLOR_WINDOW },
  { "sys_window_foreground",	   COLOR_WINDOWTEXT },
  { "sys_dialog_background",	   COLOR_BTNFACE },
  { "sys_dialog_foreground",	   COLOR_BTNTEXT },
  { "sys_button_background",	   COLOR_BTNFACE },
  { "sys_button_foreground",	   COLOR_BTNTEXT },
  { "sys_button_pressed",	   COLOR_3DLIGHT },
  { "sys_selection_background",	   COLOR_HIGHLIGHT },
  { "sys_selection_foreground",	   COLOR_HIGHLIGHTTEXT },
  { "sys_tooltip_background",	   COLOR_INFOBK },
  { "sys_tooltip_foreground",	   COLOR_INFOTEXT },
  { "sys_inactive",		   COLOR_GRAYTEXT },
#ifdef COLOR_HOTLIGHT
  { "sys_link",			   COLOR_HOTLIGHT },
#endif
  { "sys_accent",		   COLOR_HIGHLIGHT },
  { "sys_separator",		   COLOR_BTNSHADOW },
  { "sys_shadow",		   COLOR_BTNSHADOW },

  { "win_3ddkshadow",		   COLOR_3DDKSHADOW },
  { "win_3dface",		   COLOR_3DFACE },
  { "win_3dhighlight",		   COLOR_3DHIGHLIGHT },
  { "win_3dhilight",		   COLOR_3DHILIGHT },
  { "win_3dlight",		   COLOR_3DLIGHT },
  { "win_3dshadow",		   COLOR_3DSHADOW },
  { "win_activeborder",		   COLOR_ACTIVEBORDER },
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
#ifdef COLOR_MENUBAR
  { "win_menubar",		   COLOR_MENUBAR },
  { "win_menuhilight",		   COLOR_MENUHILIGHT },
#endif
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


/* Windows dark mode does not change the colours returned by
 * GetSysColor(): these remain the light colours unless the user selects
 * a contrast theme.  If the user selected dark mode for applications
 * and no contrast theme is active, we define the sys_* colours from the
 * palette below, which follows the Windows 11 dark appearance.  The
 * win_* names keep the values from GetSysColor().
 */

static const struct dark_colour
{ char	   *name;
  COLORREF  rgb;
} dark_colours[] =
{ { "sys_window_background",	RGB( 32,  32,  32) },
  { "sys_window_foreground",	RGB(255, 255, 255) },
  { "sys_dialog_background",	RGB( 43,  43,  43) },
  { "sys_dialog_foreground",	RGB(255, 255, 255) },
  { "sys_button_background",	RGB( 55,  55,  55) },
  { "sys_button_foreground",	RGB(255, 255, 255) },
  { "sys_button_pressed",	RGB( 69,  69,  69) },
  { "sys_tooltip_background",	RGB( 43,  43,  43) },
  { "sys_tooltip_foreground",	RGB(255, 255, 255) },
  { "sys_inactive",		RGB(138, 138, 138) },
  { "sys_link",			RGB( 96, 205, 255) },
  { "sys_separator",		RGB( 69,  69,  69) },
  { "sys_shadow",		RGB( 16,  16,  16) },
  { NULL,			0 }
};

static bool
high_contrast(void)
{ HIGHCONTRASTW hc = { .cbSize = sizeof(hc) };

  return ( SystemParametersInfoW(SPI_GETHIGHCONTRAST, sizeof(hc), &hc, 0) &&
	   (hc.dwFlags & HCF_HIGHCONTRASTON) );
}

static bool
reg_dword(const wchar_t *key, const wchar_t *name, DWORD *value)
{ DWORD size = sizeof(*value);

  return RegGetValueW(HKEY_CURRENT_USER, key, name, RRF_RT_REG_DWORD,
		      NULL, value, &size) == ERROR_SUCCESS;
}

static bool
dark_mode(void)
{ DWORD light;

  return ( reg_dword(L"Software\\Microsoft\\Windows\\CurrentVersion"
		     L"\\Themes\\Personalize", L"AppsUseLightTheme", &light) &&
	   light == 0 &&
	   !high_contrast() );
}

/* The accent colour as selected in Personalization/Colors.  It is
 * stored as 0xAABBGGRR, so the low 24 bits are a COLORREF.  Default
 * to the Windows default blue.
 */

static COLORREF
accent_colour(void)
{ DWORD abgr;

  if ( reg_dword(L"Software\\Microsoft\\Windows\\DWM", L"AccentColor",
		 &abgr) )
    return abgr & 0xffffff;

  return RGB(0, 120, 212);
}

static void
ws_dark_mode_colours(HashTable ColourNames)
{ COLORREF accent = accent_colour();
  double y = ( 0.299*GetRValue(accent) +
	       0.587*GetGValue(accent) +
	       0.114*GetBValue(accent) );

  for(const struct dark_colour *dc = dark_colours; dc->name; dc++)
    ws_system_colour(ColourNames, dc->name, dc->rgb);

  ws_system_colour(ColourNames, "sys_accent", accent);
  ws_system_colour(ColourNames, "sys_selection_background", accent);
  ws_system_colour(ColourNames, "sys_selection_foreground",
		   y < 160.0 ? RGB(255, 255, 255) : RGB(0, 0, 0));
}


void
ws_system_colours(HashTable ColourNames)
{ const struct system_colour *sc = window_colours;

  for( ; sc->name; sc++ )
  { DWORD rgb = GetSysColor(sc->id);

    ws_system_colour(ColourNames, sc->name, rgb);
  }

  if ( dark_mode() )
    ws_dark_mode_colours(ColourNames);
}


/* True if the window background of the system colours is dark.  This is
 * the case for contrast themes such as "Night sky", which do not set the
 * dark mode reported by SDL_GetSystemTheme().
 */

bool
ws_dark_system_colours(void)
{ COLORREF rgb = GetSysColor(COLOR_WINDOW);
  double y = ( 0.299*GetRValue(rgb) +
	       0.587*GetGValue(rgb) +
	       0.114*GetBValue(rgb) );

  return y < 128.0;
}


/* SDL reports switching between light and dark using
 * SDL_EVENT_SYSTEM_THEME_CHANGED, but only if the light/dark setting
 * changed.  Windows announces a new contrast theme by broadcasting
 * WM_SYSCOLORCHANGE and a new accent colour by broadcasting
 * WM_SETTINGCHANGE for "ImmersiveColorSet" to all top level windows.
 * SDL ignores these, so we create a hidden top level window that
 * receives them and reports them using SDL_EVENT_SYSTEM_THEME_CHANGED,
 * which reloads the system colours.  This must run in the SDL main
 * thread, such that SDL's event loop dispatches the messages for our
 * window.
 */

static LRESULT CALLBACK
sys_colour_wnd_proc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{ if ( msg == WM_SYSCOLORCHANGE ||
       ( msg == WM_SETTINGCHANGE && lParam &&
	 wcscmp((const wchar_t*)lParam, L"ImmersiveColorSet") == 0 ) )
  { SDL_Event ev = { .type = SDL_EVENT_SYSTEM_THEME_CHANGED };

    ev.common.timestamp = SDL_GetTicksNS();
    SDL_PushEvent(&ev);
    return 0;
  }

  return DefWindowProcW(hwnd, msg, wParam, lParam);
}

void
ws_watch_system_colours(void)
{ static bool done = false;
  HINSTANCE instance = GetModuleHandleW(NULL);
  WNDCLASSW wc = {0};

  if ( done )
    return;
  done = true;

  wc.lpfnWndProc   = sys_colour_wnd_proc;
  wc.hInstance     = instance;
  wc.lpszClassName = L"XPCE_SysColourWatcher";
  if ( !RegisterClassW(&wc) )
    return;

  /* Not HWND_MESSAGE: message-only windows do not receive broadcasts */
  CreateWindowExW(WS_EX_TOOLWINDOW, wc.lpszClassName, L"", WS_POPUP,
		  0, 0, 0, 0, NULL, NULL, instance, NULL);
}
