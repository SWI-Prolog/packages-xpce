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

/* The MacOS system colours.  Includes Cocoa but NO XPCE header; see
 * sdlnsmenu.h for why.
 *
 * AppKit colours are dynamic: their value depends on the appearance
 * (light, dark, increased contrast) and the accent colour chosen by the
 * user.  We resolve them once, under the appearance of the application
 * or, if there is no NSApp yet, under the appearance selected in the
 * system settings.
 */

#import <Cocoa/Cocoa.h>
#import <objc/message.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>
#include <SDL3/SDL.h>
#include "sdlnscolour.h"

/* AppKit named colours.  Each is exported as mac_<name>, where <name> is
 * the selector without "Color", in snake_case.  E.g.,
 * `selectedContentBackgroundColor` becomes
 * `mac_selected_content_background`.  Selectors that are not
 * implemented by the running MacOS version are skipped.
 */

static const char *ns_colours[] =
{ "labelColor",
  "secondaryLabelColor",
  "tertiaryLabelColor",
  "quaternaryLabelColor",
  "textColor",
  "placeholderTextColor",
  "selectedTextColor",
  "textBackgroundColor",
  "selectedTextBackgroundColor",
  "keyboardFocusIndicatorColor",
  "unemphasizedSelectedTextColor",
  "unemphasizedSelectedTextBackgroundColor",
  "linkColor",
  "separatorColor",
  "selectedContentBackgroundColor",
  "unemphasizedSelectedContentBackgroundColor",
  "selectedMenuItemTextColor",
  "gridColor",
  "headerTextColor",
  "controlAccentColor",
  "controlColor",
  "controlBackgroundColor",
  "controlTextColor",
  "disabledControlTextColor",
  "selectedControlColor",
  "selectedControlTextColor",
  "alternateSelectedControlTextColor",
  "windowBackgroundColor",
  "windowFrameTextColor",
  "underPageBackgroundColor",
  "findHighlightColor",
  "highlightColor",
  "shadowColor",
  "systemFillColor",
  "secondarySystemFillColor",
  "tertiarySystemFillColor",
  "quaternarySystemFillColor",
  "quinarySystemFillColor",
  "systemRedColor",
  "systemOrangeColor",
  "systemYellowColor",
  "systemGreenColor",
  "systemMintColor",
  "systemTealColor",
  "systemCyanColor",
  "systemBlueColor",
  "systemIndigoColor",
  "systemPurpleColor",
  "systemPinkColor",
  "systemBrownColor",
  "systemGrayColor",
  NULL
};

/* The common sys_* names (see load_system_colours() in sdlcolour.c).
 * A NULL selector stands for the dialog background (see
 * dialog_background()).
 */

static const struct
{ const char *name;
  const char *selector;
} sys_colours[] =
{ { "sys_window_background",	"textBackgroundColor" },
  { "sys_window_foreground",	"textColor" },
  { "sys_dialog_background",	NULL },
  { "sys_dialog_foreground",	"labelColor" },
  { "sys_button_background",	"controlColor" },
  { "sys_button_foreground",	"controlTextColor" },
  { "sys_button_pressed",	"selectedControlColor" },
  { "sys_selection_background",	"selectedContentBackgroundColor" },
  { "sys_selection_foreground",	"alternateSelectedControlTextColor" },
  { "sys_text_selection_background", "selectedTextBackgroundColor" },
  { "sys_tooltip_background",	NULL },
  { "sys_tooltip_foreground",	"labelColor" },
  { "sys_inactive",		"disabledControlTextColor" },
  { "sys_link",			"linkColor" },
  { "sys_accent",		"controlAccentColor" },
  { "sys_separator",		"separatorColor" },
  { "sys_shadow",		"tertiaryLabelColor" },
  { NULL,			NULL }
};

typedef struct
{ CGFloat r, g, b, a;
} ns_rgba;

static CGFloat
clamp01(CGFloat v)
{ return v < 0.0 ? 0.0 : v > 1.0 ? 1.0 : v;
}

/* Get the sRGB value of the colour named by `selector`.  Many AppKit
 * colours are translucent.  If `under` is given, the colour is composed
 * over it, so the result is opaque.
 */

static bool
ns_colour_rgba(const char *selector, const ns_rgba *under, ns_rgba *c)
{ SEL sel = sel_getUid(selector);

  if ( ![NSColor respondsToSelector:sel] )
    return false;

  NSColor *col  = ((NSColor *(*)(id, SEL))objc_msgSend)([NSColor class], sel);
  NSColor *srgb = [col colorUsingColorSpace:[NSColorSpace sRGBColorSpace]];

  if ( !srgb )				/* e.g., a pattern colour */
    return false;

  [srgb getRed:&c->r green:&c->g blue:&c->b alpha:&c->a];
  c->r = clamp01(c->r);			/* extended sRGB may be out of range */
  c->g = clamp01(c->g);
  c->b = clamp01(c->b);
  c->a = clamp01(c->a);

  if ( under && c->a < 1.0 )
  { c->r = c->a*c->r + (1.0-c->a)*under->r;
    c->g = c->a*c->g + (1.0-c->a)*under->g;
    c->b = c->a*c->b + (1.0-c->a)*under->b;
    c->a = 1.0;
  }

  return true;
}

static void
add_colour(sys_colour_callback add, void *closure,
	   const char *name, const ns_rgba *c)
{ (*add)(name,
	 (unsigned)(c->r*255.0+0.5),
	 (unsigned)(c->g*255.0+0.5),
	 (unsigned)(c->b*255.0+0.5),
	 (unsigned)(c->a*255.0+0.5),
	 closure);
}

/* labelColor --> mac_label */

static bool
mac_colour_name(const char *selector, char *buf, size_t size)
{ static const char prefix[] = "mac_";
  size_t len = strlen(selector);
  char *o = buf, *e = buf+size-1;

  if ( len > 5 && strcmp(selector+len-5, "Color") == 0 )
    len -= 5;
  if ( size < sizeof(prefix) )
    return false;
  strcpy(o, prefix);
  o += sizeof(prefix)-1;

  for(size_t i=0; i<len; i++)
  { char ch = selector[i];

    if ( ch >= 'A' && ch <= 'Z' )
    { if ( o+2 > e )
	return false;
      *o++ = '_';
      *o++ = ch - 'A' + 'a';
    } else
    { if ( o+1 > e )
	return false;
      *o++ = ch;
    }
  }
  *o = '\0';

  return true;
}

static NSAppearance *
system_appearance(void)
{ if ( NSApp )
    return [NSApp effectiveAppearance];

  NSString *style = [[NSUserDefaults standardUserDefaults]
		      stringForKey:@"AppleInterfaceStyle"];
  bool dark = style && [style caseInsensitiveCompare:@"Dark"] == NSOrderedSame;

  return [NSAppearance appearanceNamed:(dark ? NSAppearanceNameDarkAqua
					     : NSAppearanceNameAqua)];
}

static bool
same_colour(const ns_rgba *c1, const ns_rgba *c2)
{ const CGFloat eps = 4.0/255.0;

  return ( fabs(c1->r-c2->r) < eps &&
	   fabs(c1->g-c2->g) < eps &&
	   fabs(c1->b-c2->b) < eps );
}

/* The background for dialogs.  Up to MacOS 15, windowBackgroundColor is
 * a light or dark grey that differs from the content background
 * (textBackgroundColor).  Since MacOS 26, both are the same, which makes
 * dialogs indistinguishable from content windows.  In that case we
 * compose secondarySystemFillColor over it, which gives about the
 * sidebar colour of libadwaita that we use on GNOME.
 */

static void
dialog_background(const ns_rgba *window, ns_rgba *c)
{ ns_rgba text;

  *c = *window;
  if ( ns_colour_rgba("textBackgroundColor", NULL, &text) &&
       same_colour(window, &text) )
    ns_colour_rgba("secondarySystemFillColor", window, c);
}

static double
luminance(const ns_rgba *c)
{ return 0.299*c->r + 0.587*c->g + 0.114*c->b;
}

/* The colour for inactive (disabled) text.  disabledControlTextColor is
 * translucent and disabled text appears on dialogs as well as on
 * buttons.  Composed over the dialog background it may be the same as
 * the button background, e.g., in dark mode, where both are white at
 * 25% opacity, making the label of a disabled button invisible.  We
 * compose it over the dialog and the button background and use the
 * one that remains most visible on both.
 */

static bool
inactive_colour(const ns_rgba *dialog_bg, ns_rgba *c)
{ ns_rgba button_bg, on_dialog, on_button;

  if ( !ns_colour_rgba("disabledControlTextColor", dialog_bg, &on_dialog) )
    return false;
  if ( !ns_colour_rgba("controlColor", dialog_bg, &button_bg) ||
       !ns_colour_rgba("disabledControlTextColor", &button_bg, &on_button) )
  { *c = on_dialog;
    return true;
  }

  double ld = luminance(dialog_bg);
  double lb = luminance(&button_bg);
  double l1 = luminance(&on_dialog);
  double l2 = luminance(&on_button);
  double min1 = fmin(fabs(l1-ld), fabs(l1-lb));
  double min2 = fmin(fabs(l2-ld), fabs(l2-lb));

  *c = min1 >= min2 ? on_dialog : on_button;
  return true;
}

static void
resolve_colours(sys_colour_callback add, void *closure)
{ ns_rgba bg, dialog_bg, c;
  const ns_rgba *under = NULL;
  char name[100];

  if ( ns_colour_rgba("windowBackgroundColor", NULL, &bg) )
    under = &bg;

  for(const char **s = ns_colours; *s; s++)
  { if ( ns_colour_rgba(*s, under, &c) &&
	 mac_colour_name(*s, name, sizeof(name)) )
      add_colour(add, closure, name, &c);
  }

  if ( !under )
    return;
  dialog_background(under, &dialog_bg);

  /* The sys_* colours are mostly used on dialogs.  Compose them over the
   * dialog background, so translucent ones such as the separator remain
   * visible.
   */
  for(int i=0; sys_colours[i].name; i++)
  { const char *sel = sys_colours[i].selector;

    if ( !sel )
      add_colour(add, closure, sys_colours[i].name, &dialog_bg);
    else if ( strcmp(sys_colours[i].name, "sys_inactive") == 0 )
    { if ( inactive_colour(&dialog_bg, &c) )
	add_colour(add, closure, sys_colours[i].name, &c);
    } else if ( ns_colour_rgba(sel, &dialog_bg, &c) )
      add_colour(add, closure, sys_colours[i].name, &c);
  }
}

void
ns_system_colours(sys_colour_callback add, void *closure)
{ @autoreleasepool
  { NSAppearance *appearance = system_appearance();

    /* Not @available(): below a macOS 11 deployment target that calls
       __isPlatformVersionAtLeast from clang's compiler-rt, which is
       missing when GCC links the library */
    if ( [appearance respondsToSelector:
		       @selector(performAsCurrentDrawingAppearance:)] )
    {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunguarded-availability-new"
      [appearance performAsCurrentDrawingAppearance:^{
	resolve_colours(add, closure);
      }];
#pragma clang diagnostic pop
    } else
    {
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
      NSAppearance *old = [NSAppearance currentAppearance];

      [NSAppearance setCurrentAppearance:appearance];
      resolve_colours(add, closure);
      [NSAppearance setCurrentAppearance:old];
#pragma clang diagnostic pop
    }
  }
}

/* SDL reports switching between light and dark using
 * SDL_EVENT_SYSTEM_THEME_CHANGED.  MacOS also changes the system
 * colours if the user selects another accent or highlight colour or
 * increases the contrast.  We report these changes using the same
 * event, which reloads the system colours.
 */

void
ns_watch_system_colours(void)
{ static bool done = false;

  if ( done )
    return;
  done = true;

  [[NSNotificationCenter defaultCenter]
    addObserverForName:NSSystemColorsDidChangeNotification
		object:nil
		 queue:[NSOperationQueue mainQueue]
	    usingBlock:^(NSNotification *note) {
	      (void)note;
	      SDL_Event ev = { .type = SDL_EVENT_SYSTEM_THEME_CHANGED };
	      ev.common.timestamp = SDL_GetTicksNS();
	      SDL_PushEvent(&ev);
	    }];
}
