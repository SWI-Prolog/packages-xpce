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

/* System colours from the KDE colour scheme.
 *
 * KDE stores the active colour scheme in the `kdeglobals` files of the
 * XDG configuration directories, using groups such as [Colors:View]
 * with keys such as `BackgroundNormal=255,255,255`.  This file does
 * not depend on Qt or KDE: it reads these files, which are in KConfig
 * (INI) format.
 *
 * Each colour is exported as kde_<group>_<key> in snake_case, e.g.,
 * [Colors:View] BackgroundNormal becomes kde_view_background_normal and
 * [WM] activeBackground becomes kde_wm_active_background.  The accent
 * colour of [General] AccentColor becomes kde_accent.
 *
 * If the user never changed the colour scheme, kdeglobals may not hold
 * the colours.  KDE applications then use the default scheme, Breeze
 * Light, so we start from its colours.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include "sdlkdecolour.h"

#define KDE_NAME_SIZE 80
#define KDE_MAX_COLOURS 512

typedef struct
{ char		name[KDE_NAME_SIZE];
  unsigned char r, g, b, a;
} kde_colour;

typedef struct
{ kde_colour   *colours;
  int		count;
} kde_table;

/* The colours of Breeze Light that are used for the sys_* names */

static const kde_colour breeze_light[] =
{ { "kde_view_background_normal",	  255, 255, 255, 255 },
  { "kde_view_foreground_normal",	   35,  38,  41, 255 },
  { "kde_view_foreground_inactive",	  112, 125, 138, 255 },
  { "kde_view_foreground_link",		   41, 128, 185, 255 },
  { "kde_view_decoration_focus",	   61, 174, 233, 255 },
  { "kde_window_background_normal",	  239, 240, 241, 255 },
  { "kde_window_foreground_normal",	   35,  38,  41, 255 },
  { "kde_window_foreground_inactive",	  112, 125, 138, 255 },
  { "kde_button_background_normal",	  252, 252, 252, 255 },
  { "kde_button_background_alternate",	  163, 212, 250, 255 },
  { "kde_button_foreground_normal",	   35,  38,  41, 255 },
  { "kde_selection_background_normal",	   61, 174, 233, 255 },
  { "kde_selection_foreground_normal",	  255, 255, 255, 255 },
  { "kde_tooltip_background_normal",	  247, 247, 247, 255 },
  { "kde_tooltip_foreground_normal",	   35,  38,  41, 255 },
  { "",					    0,   0,   0,   0 }
};

/* The sys_* names (see load_system_colours() in sdlcolour.c).
 * sys_accent, sys_separator and sys_shadow are handled separately.
 */

static const struct
{ const char *name;
  const char *kde;
} sys_colours[] =
{ { "sys_window_background",	"kde_view_background_normal" },
  { "sys_window_foreground",	"kde_view_foreground_normal" },
  { "sys_dialog_background",	"kde_window_background_normal" },
  { "sys_dialog_foreground",	"kde_window_foreground_normal" },
  { "sys_button_background",	"kde_button_background_normal" },
  { "sys_button_foreground",	"kde_button_foreground_normal" },
  { "sys_button_pressed",	"kde_button_background_alternate" },
  { "sys_selection_background",	"kde_selection_background_normal" },
  { "sys_selection_foreground",	"kde_selection_foreground_normal" },
  { "sys_tooltip_background",	"kde_tooltip_background_normal" },
  { "sys_tooltip_foreground",	"kde_tooltip_foreground_normal" },
  { "sys_inactive",		"kde_window_foreground_inactive" },
  { "sys_link",			"kde_view_foreground_link" },
  { NULL,			NULL }
};


static kde_colour *
lookup_colour(kde_table *t, const char *name)
{ for(int i=0; i<t->count; i++)
  { if ( strcmp(t->colours[i].name, name) == 0 )
      return &t->colours[i];
  }

  return NULL;
}

static void
set_colour(kde_table *t, const char *name,
	   unsigned r, unsigned g, unsigned b, unsigned a)
{ kde_colour *c = lookup_colour(t, name);

  if ( !c )
  { if ( t->count >= KDE_MAX_COLOURS )
      return;
    c = &t->colours[t->count++];
    strcpy(c->name, name);
  }

  c->r = r; c->g = g; c->b = b; c->a = a;
}


/* Append `in` to `out` in snake_case.  `in` is the KConfig group or key
 * name, e.g., `BackgroundNormal` or `activeBackground`.  A '_' is
 * inserted before an upper case letter that follows a lower case letter
 * or digit, so `WM` becomes `wm`.  Returns false if `out` overflows.
 */

static bool
snake_case(char *out, size_t size, const char *in, size_t len)
{ size_t o = strlen(out);

  for(size_t i=0; i<len; i++)
  { int c = (unsigned char)in[i];

    if ( isupper(c) && i > 0 &&
	 (islower((unsigned char)in[i-1]) || isdigit((unsigned char)in[i-1])) )
    { if ( o+1 >= size )
	return false;
      out[o++] = '_';
    }
    if ( o+1 >= size )
      return false;
    out[o++] = (char)tolower(c);
  }
  out[o] = '\0';

  return true;
}

/* Map a KConfig group and key to our colour name.  The group is the
 * text between the outer brackets of the group line, e.g.,
 * `Colors:Header][Inactive`.  Returns false for keys that do not
 * define a colour we export.
 */

static bool
colour_name(const char *group, const char *key, char *buf, size_t size)
{ if ( strcmp(group, "General") == 0 )
  { if ( strcmp(key, "AccentColor") == 0 && size > 10 )
    { strcpy(buf, "kde_accent");
      return true;
    }
    return false;
  }

  if ( strncmp(group, "Colors:", 7) == 0 )
    group += 7;
  else if ( strcmp(group, "WM") != 0 )
    return false;

  strcpy(buf, "kde_");			/* size >= KDE_NAME_SIZE */
  while(*group)				/* Header][Inactive -> header_inactive */
  { const char *e = strstr(group, "][");
    size_t len = e ? (size_t)(e-group) : strlen(group);

    if ( !snake_case(buf, size, group, len) ||
	 !snake_case(buf, size, "_", 1) )
      return false;
    group += len;
    if ( *group )
      group += 2;
  }

  return snake_case(buf, size, key, strlen(key));
}

static char *
strip(char *s)
{ char *e;

  while(isspace((unsigned char)*s))
    s++;
  e = s+strlen(s);
  while(e > s && isspace((unsigned char)e[-1]))
    *--e = '\0';

  return s;
}

/* Parse `r,g,b` or `r,g,b,a` */

static bool
parse_rgb(const char *s, unsigned *r, unsigned *g, unsigned *b, unsigned *a)
{ int n = -1;

  *a = 255;
  if ( (sscanf(s, "%u , %u , %u , %u %n", r, g, b, a, &n) == 4 ||
	sscanf(s, "%u , %u , %u %n", r, g, b, &n) == 3) &&
       n >= 0 && s[n] == '\0' &&
       *r <= 255 && *g <= 255 && *b <= 255 && *a <= 255 )
    return true;

  return false;
}

static void
read_kdeglobals(kde_table *t, const char *dir)
{ char path[4096];
  char line[1024];
  char group[256] = "";
  FILE *fd;

  if ( snprintf(path, sizeof(path), "%s/kdeglobals", dir) >=
						(int)sizeof(path) ||
       !(fd = fopen(path, "r")) )
    return;

  while( fgets(line, sizeof(line), fd) )
  { char *s = strip(line);

    if ( !*s || *s == '#' )
      continue;

    if ( *s == '[' )			/* [group] or [group][sub][$i] */
    { char *e = strrchr(s, ']');
      char *flags;

      group[0] = '\0';
      if ( !e || (size_t)(e-s) >= sizeof(group) )
	continue;
      *e = '\0';
      strcpy(group, s+1);
      if ( (flags=strstr(group, "][$")) )
	*flags = '\0';
    } else if ( group[0] )
    { char *eq = strchr(s, '=');
      char name[KDE_NAME_SIZE];
      unsigned r, g, b, a;

      if ( !eq )
	continue;
      *eq = '\0';
      char *key = strip(s);
      char *value = strip(eq+1);
      char *opt = strchr(key, '[');

      if ( opt )			/* Key[$e] or Key[de] */
      { if ( opt[1] != '$' )
	  continue;			/* localized value */
	*opt = '\0';
      }

      if ( colour_name(group, key, name, sizeof(name)) &&
	   parse_rgb(value, &r, &g, &b, &a) )
	set_colour(t, name, r, g, b, a);
    }
  }

  fclose(fd);
}

/* Read the kdeglobals files.  XDG_CONFIG_DIRS lists the directories
 * in order of decreasing importance and XDG_CONFIG_HOME is more
 * important than all of them.  We read the least important first, so
 * more important files override.
 */

static void
read_dirs_reversed(kde_table *t, const char *dirs)
{ const char *sep = strrchr(dirs, ':');

  if ( sep )
  { char *head = strndup(dirs, (size_t)(sep-dirs));

    if ( head )
    { if ( sep[1] )
	read_kdeglobals(t, sep+1);
      read_dirs_reversed(t, head);
      free(head);
    }
  } else if ( *dirs )
  { read_kdeglobals(t, dirs);
  }
}

static void
read_config(kde_table *t)
{ const char *dirs = getenv("XDG_CONFIG_DIRS");
  const char *home = getenv("XDG_CONFIG_HOME");

  read_dirs_reversed(t, dirs && *dirs ? dirs : "/etc/xdg");

  if ( home && *home )
  { read_kdeglobals(t, home);
  } else if ( (home=getenv("HOME")) )
  { char dir[4096];

    if ( snprintf(dir, sizeof(dir), "%s/.config", home) < (int)sizeof(dir) )
      read_kdeglobals(t, dir);
  }
}

static void
add_colour(sys_colour_callback add, void *closure,
	   const char *name, const kde_colour *c)
{ (*add)(name, c->r, c->g, c->b, c->a, closure);
}

/* Mix `fg` into `bg` for `f` (0..1).  Used for the separator and
 * shadow, for which KDE has no colours.
 */

static void
add_mixed_colour(sys_colour_callback add, void *closure, const char *name,
		 const kde_colour *bg, const kde_colour *fg, double f)
{ kde_colour c;

  c.r = (unsigned char)(bg->r + (fg->r - bg->r)*f + 0.5);
  c.g = (unsigned char)(bg->g + (fg->g - bg->g)*f + 0.5);
  c.b = (unsigned char)(bg->b + (fg->b - bg->b)*f + 0.5);
  c.a = 255;
  add_colour(add, closure, name, &c);
}

void
kde_system_colours(sys_colour_callback add, void *closure)
{ kde_table t;
  kde_colour *c, *bg, *fg;

  if ( !(t.colours = malloc(KDE_MAX_COLOURS*sizeof(kde_colour))) )
    return;
  t.count = 0;

  for(const kde_colour *d = breeze_light; d->name[0]; d++)
    set_colour(&t, d->name, d->r, d->g, d->b, d->a);
  read_config(&t);

  for(int i=0; i<t.count; i++)
    add_colour(add, closure, t.colours[i].name, &t.colours[i]);

  for(int i=0; sys_colours[i].name; i++)
  { if ( (c=lookup_colour(&t, sys_colours[i].kde)) )
      add_colour(add, closure, sys_colours[i].name, c);
  }

  if ( (c=lookup_colour(&t, "kde_accent")) ||
       (c=lookup_colour(&t, "kde_view_decoration_focus")) )
    add_colour(add, closure, "sys_accent", c);

  if ( (bg=lookup_colour(&t, "kde_window_background_normal")) &&
       (fg=lookup_colour(&t, "kde_window_foreground_normal")) )
  { add_mixed_colour(add, closure, "sys_separator", bg, fg, 0.2);
    add_mixed_colour(add, closure, "sys_shadow",    bg, fg, 0.4);
  }

  free(t.colours);
}
