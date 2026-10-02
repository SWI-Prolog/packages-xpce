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

/* System colours for GNOME.
 *
 * GNOME does not publish its colours: they are defined by the CSS of
 * GTK and libadwaita.  What it does publish, through the XDG Desktop
 * Portal settings namespace `org.freedesktop.appearance`, is whether the
 * user prefers a dark appearance (`color-scheme`), high contrast
 * (`contrast`) and the accent colour (`accent-color`, GNOME 47 and
 * later).  We combine these with a built-in copy of the libadwaita
 * palette.
 *
 * The portal is accessed over D-Bus using GIO.  Without GIO we use the
 * light palette and the default accent colour.
 */

#include <config.h>
#include <stdbool.h>
#include "sdlgnomecolour.h"
#ifdef HAVE_GIO
#include <gio/gio.h>
#endif

typedef struct
{ double r, g, b;			/* 0..1 */
} gnome_rgb;

typedef struct
{ bool		dark;			/* color-scheme is prefer-dark */
  bool		high_contrast;		/* contrast is high */
  bool		have_accent;		/* accent-color is valid */
  gnome_rgb	accent;
} gnome_settings;

#define RGB8(r, g, b) { (r)/255.0, (g)/255.0, (b)/255.0 }

/* The libadwaita palette (libadwaita 1.6).  The foreground (ink) of the
 * light palette is translucent, rgba(0,0,6,0.8).  Button, separator,
 * etc. colours are alpha(currentColor, f) in the libadwaita CSS.  We
 * compose them over the dialog background.
 *
 * The libadwaita window background is almost white, which makes xpce
 * dialogs indistinguishable from the content windows.  We therefore use
 * the sidebar background, the colour libadwaita uses for panels next to
 * the content, as dialog background.
 */

static const gnome_rgb adw_light_sidebar_bg = RGB8(235, 235, 237);
static const gnome_rgb adw_light_view_bg   = RGB8(255, 255, 255);
static const gnome_rgb adw_light_ink       = RGB8(  0,   0,   6);
static const gnome_rgb adw_dark_sidebar_bg  = RGB8( 46,  46,  50);
static const gnome_rgb adw_dark_view_bg    = RGB8( 29,  29,  32);
static const gnome_rgb adw_dark_ink        = RGB8(255, 255, 255);
static const gnome_rgb adw_tooltip_bg      = RGB8(  0,   0,   6);
static const gnome_rgb adw_accent          = RGB8( 53, 132, 228);
static const gnome_rgb adw_white           = RGB8(255, 255, 255);
static const gnome_rgb adw_black           = RGB8(  0,   0,   0);

static gnome_rgb
mix(gnome_rgb bg, gnome_rgb fg, double f)
{ gnome_rgb c = { bg.r + (fg.r-bg.r)*f,
		  bg.g + (fg.g-bg.g)*f,
		  bg.b + (fg.b-bg.b)*f };
  return c;
}

static void
add_colour(sys_colour_callback add, void *closure,
	   const char *name, gnome_rgb c)
{ (*add)(name,
	 (unsigned)(c.r*255.0+0.5),
	 (unsigned)(c.g*255.0+0.5),
	 (unsigned)(c.b*255.0+0.5),
	 255,
	 closure);
}

/* Text on the accent colour.  libadwaita uses white on its accent
 * colours.  A custom accent colour may be light though.
 */

static gnome_rgb
accent_foreground(gnome_rgb accent)
{ double y = 0.299*accent.r + 0.587*accent.g + 0.114*accent.b;

  return y > 0.6 ? adw_black : adw_white;
}

static void
palette_colours(sys_colour_callback add, void *closure,
		const gnome_settings *s)
{ gnome_rgb dialog_bg = s->dark ? adw_dark_sidebar_bg : adw_light_sidebar_bg;
  gnome_rgb view_bg   = s->dark ? adw_dark_view_bg   : adw_light_view_bg;
  gnome_rgb ink       = s->dark ? adw_dark_ink       : adw_light_ink;
  double    ink_alpha = s->dark || s->high_contrast ? 1.0 : 0.8;
  gnome_rgb accent    = s->have_accent ? s->accent : adw_accent;

  add_colour(add, closure, "sys_window_background", view_bg);
  add_colour(add, closure, "sys_window_foreground",
	     mix(view_bg, ink, ink_alpha));
  add_colour(add, closure, "sys_dialog_background", dialog_bg);
  add_colour(add, closure, "sys_dialog_foreground",
	     mix(dialog_bg, ink, ink_alpha));
  add_colour(add, closure, "sys_button_background",
	     mix(dialog_bg, ink, 0.1*ink_alpha));
  add_colour(add, closure, "sys_button_foreground",
	     mix(dialog_bg, ink, ink_alpha));
  add_colour(add, closure, "sys_button_pressed",
	     mix(dialog_bg, ink, 0.3*ink_alpha));
  add_colour(add, closure, "sys_selection_background", accent);
  add_colour(add, closure, "sys_selection_foreground",
	     accent_foreground(accent));
  add_colour(add, closure, "sys_tooltip_background",
	     mix(dialog_bg, adw_tooltip_bg, 0.8));
  add_colour(add, closure, "sys_tooltip_foreground", adw_white);
  add_colour(add, closure, "sys_inactive",
	     mix(dialog_bg, ink, 0.5*ink_alpha));
  add_colour(add, closure, "sys_link", accent);
  add_colour(add, closure, "sys_accent", accent);
  add_colour(add, closure, "sys_separator",
	     mix(dialog_bg, ink, (s->high_contrast ? 0.5 : 0.15)*ink_alpha));
  add_colour(add, closure, "sys_shadow",
	     mix(dialog_bg, ink, (s->high_contrast ? 0.7 : 0.3)*ink_alpha));
}


#ifdef HAVE_GIO

static bool
lookup_uint32(GVariant *dict, const char *key, guint32 *value)
{ GVariant *v = g_variant_lookup_value(dict, key, G_VARIANT_TYPE_UINT32);

  if ( v )
  { *value = g_variant_get_uint32(v);
    g_variant_unref(v);
    return true;
  }

  return false;
}

static void
read_appearance(GVariant *app, gnome_settings *s)
{ guint32 u;
  GVariant *v;

  if ( lookup_uint32(app, "color-scheme", &u) )
    s->dark = (u == 1);			/* 0: default, 1: dark, 2: light */
  if ( lookup_uint32(app, "contrast", &u) )
    s->high_contrast = (u == 1);	/* 0: default, 1: high */

  if ( (v=g_variant_lookup_value(app, "accent-color",
				 G_VARIANT_TYPE("(ddd)"))) )
  { gnome_rgb c;

    g_variant_get(v, "(ddd)", &c.r, &c.g, &c.b);
    g_variant_unref(v);
					/* out of range: no accent colour */
    if ( c.r >= 0.0 && c.r <= 1.0 &&
	 c.g >= 0.0 && c.g <= 1.0 &&
	 c.b >= 0.0 && c.b <= 1.0 )
    { s->accent = c;
      s->have_accent = true;
    }
  }
}

/* Read the appearance settings from the portal using a single ReadAll
 * call.  If there is no session bus or no portal we keep the defaults.
 * The timeout avoids a long delay if the portal does not respond.
 */

#define PORTAL_TIMEOUT 1000		/* milliseconds */

static void
read_portal(gnome_settings *s)
{ GError *error = NULL;
  GDBusConnection *bus = g_bus_get_sync(G_BUS_TYPE_SESSION, NULL, &error);

  if ( bus )
  { const char *namespaces[] = { "org.freedesktop.appearance", NULL };
    GVariant *reply =
      g_dbus_connection_call_sync(bus,
				  "org.freedesktop.portal.Desktop",
				  "/org/freedesktop/portal/desktop",
				  "org.freedesktop.portal.Settings",
				  "ReadAll",
				  g_variant_new("(^as)", namespaces),
				  G_VARIANT_TYPE("(a{sa{sv}})"),
				  G_DBUS_CALL_FLAGS_NONE,
				  PORTAL_TIMEOUT,
				  NULL,
				  &error);

    if ( reply )
    { GVariant *all = g_variant_get_child_value(reply, 0);
      GVariant *app = g_variant_lookup_value(all,
					     "org.freedesktop.appearance",
					     G_VARIANT_TYPE("a{sv}"));

      if ( app )
      { read_appearance(app, s);
	g_variant_unref(app);
      }
      g_variant_unref(all);
      g_variant_unref(reply);
    }

    g_object_unref(bus);
  }

  g_clear_error(&error);
}

#else /*HAVE_GIO*/

static void
read_portal(gnome_settings *s)
{ (void)s;
}

#endif /*HAVE_GIO*/


void
gnome_system_colours(sys_colour_callback add, void *closure)
{ gnome_settings s = { .dark = false };

  read_portal(&s);
  palette_colours(add, closure, &s);
}
