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
#include "sdlcolour.h"
#ifdef __WINDOWS__
#include <msw/mscolour.h>
#elif defined(__APPLE__)
#include "sdlnscolour.h"
#else
#include "sdlkdecolour.h"
#include "sdlgnomecolour.h"
#endif

static HashTable ColourNames;		/* name --> rgb (packed in Int) */
static Chain	 CSSColourList;		/* name (for preserved ordering) */

static void	load_system_colours(HashTable cn);

#include "csscolours.c"			/* get CSS colour names */

static Name	canonical_colour_name(Name in);

/* System colours.  The sys_* names are defined on all platforms.  The
 * platform backend defines them from the user's desktop settings,
 * together with platform specific names (win_*, mac_*, kde_*).  Names the
 * backend leaves undefined get the fallback below, which is xpce's
 * traditional Unix look.  The mapping is documented in the userguide,
 * section "System colours" (man/userguide/images.plx).
 */

static const struct sys_colour
{ const char *name;
  COLORRGBA   fallback;
} sys_colours[] =
{ { "sys_window_background",	RGBA(255, 255, 255, 255) },
  { "sys_window_foreground",	RGBA(  0,   0,   0, 255) },
  { "sys_dialog_background",	RGBA(204, 204, 204, 255) }, /* grey80 */
  { "sys_dialog_foreground",	RGBA(  0,   0,   0, 255) },
  { "sys_button_background",	RGBA(204, 204, 204, 255) },
  { "sys_button_foreground",	RGBA(  0,   0,   0, 255) },
  { "sys_button_pressed",	RGBA(179, 179, 179, 255) }, /* grey70 */
  { "sys_selection_background",	RGBA(  0,   0,   0, 255) },
  { "sys_selection_foreground",	RGBA(255, 255, 255, 255) },
  { "sys_tooltip_background",	RGBA(255, 211, 155, 255) }, /* burlywood1 */
  { "sys_tooltip_foreground",	RGBA(  0,   0,   0, 255) },
  { "sys_inactive",		RGBA(127, 127, 127, 255) }, /* grey50 */
  { "sys_link",			RGBA(  0,   0, 238, 255) },
  { "sys_accent",		RGBA( 30, 144, 255, 255) }, /* dodger_blue */
  { "sys_separator",		RGBA(127, 127, 127, 255) },
  { "sys_shadow",		RGBA(127, 127, 127, 255) },
  { NULL,			0 }
};

static void
add_system_colour(HashTable cn, const char *name, COLORRGBA rgba)
{ Name key = CtoKeyword(name);

  if ( !getMemberHashTable(cn, key) )
    appendHashTable(cn, key, toInt(rgba));
}

#ifndef __WINDOWS__
static void
add_platform_colour(const char *name,
		    unsigned r, unsigned g, unsigned b, unsigned a,
		    void *closure)
{ add_system_colour(closure, name, RGBA(r, g, b, a));
}
#endif

#if !defined(__WINDOWS__) && !defined(__APPLE__)
/* True if `name` appears in the colon separated $XDG_CURRENT_DESKTOP,
 * e.g., `ubuntu:GNOME`.
 */

static bool
xdg_current_desktop(const char *name)
{ const char *s = getenv("XDG_CURRENT_DESKTOP");
  size_t nlen = strlen(name);

  while( s && *s )
  { const char *e = strchr(s, ':');
    size_t len = e ? (size_t)(e-s) : strlen(s);

    if ( len == nlen && strncasecmp(s, name, len) == 0 )
      return true;
    s = e ? e+1 : NULL;
  }

  return false;
}

static bool
running_kde(void)
{ const char *full = getenv("KDE_FULL_SESSION");

  return xdg_current_desktop("KDE") ||
	 (full && strcasecmp(full, "true") == 0);
}
#endif

/* The background of selected text.  Selected text keeps its colour, so
 * syntax highlighting remains visible.  This requires a background that
 * differs little from the window background.  Unless the platform
 * provides one, we tint the window background with the accent colour.
 */

#define TEXT_SELECTION_TINT 0.3

static void
add_text_selection_colour(HashTable cn)
{ Int bg     = getMemberHashTable(cn, CtoKeyword("sys_window_background"));
  Int accent = getMemberHashTable(cn, CtoKeyword("sys_accent"));

  if ( bg && accent )
  { COLORRGBA b = (COLORRGBA)valInt(bg);
    COLORRGBA a = (COLORRGBA)valInt(accent);
#define MIX(f) (unsigned)(f(b) + (f(a)-(double)f(b))*TEXT_SELECTION_TINT + 0.5)

    add_system_colour(cn, "sys_text_selection_background",
		      RGBA(MIX(ColorRValue), MIX(ColorGValue), MIX(ColorBValue),
			   255));
#undef MIX
  }
}

static void
load_system_colours(HashTable cn)
{
#ifdef __WINDOWS__
  ws_system_colours(cn);
#elif defined(__APPLE__)
  ns_system_colours(add_platform_colour, cn);
#else
  if ( running_kde() )
    kde_system_colours(add_platform_colour, cn);
  else if ( xdg_current_desktop("GNOME") )
    gnome_system_colours(add_platform_colour, cn);
#endif

  for(const struct sys_colour *sc = sys_colours; sc->name; sc++)
    add_system_colour(cn, sc->name, sc->fallback);
  add_text_selection_colour(cn);
}


/* Reload the system colours after the user changed the desktop
 * settings.  Each name that changed gets its new value, and so does the
 * Colour object of that name if it exists.  As drawing uses the Colour
 * objects, the caller only needs to redraw the windows.  Returns the
 * number of colours that changed.
 */

int
ws_reload_system_colours(void)
{ HashTable cn = LoadColourNames();
  HashTable fresh = createHashTable(toInt(256), NAME_none);
  int changed = 0;

  load_system_colours(fresh);
  for_hash_table(fresh, s,
		 { if ( getMemberHashTable(cn, s->name) != s->value )
		   { Colour c;

		     appendHashTable(cn, s->name, s->value);
		     if ( (c=getMemberHashTable(ColourTable, s->name)) &&
			  c->kind == NAME_named )
		       rgbaColour(c, s->value);
		     changed++;
		   }
		 });
  freeHashTable(fresh);

  return changed;
}


Int
getNamedRGB(Name name)
{ Int Rgb;

  HashTable ht = LoadColourNames();

  if ( (Rgb = getMemberHashTable(ht, name)) ||
       (Rgb = getMemberHashTable(ht, canonical_colour_name(name))) )
    return Rgb;

  fail;
}

/**
 * If a colour is a named colour, fill its rgba.
 *
 * @param c Pointer to the Colour object to be created.
 * @return SUCCEED on successful creation; otherwise, FAIL.
 */
status
ws_named_colour(Colour c)
{ if ( isDefault(c->rgba) )
  { if ( c->kind == NAME_theme )
      return resolveThemeColour((ThemeColour)c);
    if ( c->kind == NAME_named )
    { Int Rgb = getNamedRGB(c->name);

      if ( Rgb )
      { assign(c, rgba, Rgb);
	succeed;
      }
    }

    Cprintf("%s: not named or no existing name (using grey50)\n", pp(c));
    assign(c, rgba, (intptr_t)RGBA(127,127,127,255));

    fail;
  }

  succeed;
}

static Name
canonical_colour_name(Name in)
{ char *s = strName(in);
  char buf[100];
  int left = sizeof(buf);
  char *q = buf;
  int changed = 0;

  for( ; *s && --left > 0; s++, q++ )
  { if ( *s == ' ' )
    { *q = '_';
      changed++;
    } else if ( isupper(*s) )
    { *q = tolower(*s);
      changed++;
    } else
      *q = *s;
  }

  if ( left && changed )
  { *q = EOS;
    return CtoKeyword(buf);
  }

  return in;
}

/**
 * Convert a pixel value to its corresponding Colour object
 *
 * @param pixel The pixel value to be converted.
 * @return Pointer to the corresponding Colour object;
 */
Colour
ws_pixel_to_colour(COLORRGBA pixel)
{ Colour c;

  if ( RevColourTable && (c=getMemberHashTable(RevColourTable, toInt(pixel))) )
    return c;

  SDL_Color sdlc = rgba2SDL_Color(pixel);
  return answerObject(ClassColour, DEFAULT,
		      toInt(sdlc.r), toInt(sdlc.g), toInt(sdlc.b),
		      toInt(sdlc.a), EAV);
}
