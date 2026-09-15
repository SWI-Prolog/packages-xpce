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

#ifndef SDLNSMENU_H
#define SDLNSMENU_H
#include <stddef.h>
#include <stdbool.h>

/* Bridge between the XPCE menu_bar model (sdlmenubar.c) and Cocoa
 * (sdlnsmenu.m).
 *
 * These two files can NOT be merged: XPCE typedefs `Class' and `Method'
 * (src/h/types.h:77,110), which collide head-on with the Objective-C
 * runtime typedefs of the same name.  Therefore sdlnsmenu.m includes
 * Cocoa but no XPCE header, sdlmenubar.c includes XPCE but no Cocoa
 * header, and they talk to each other through the plain C types below.
 *
 * XPCE objects are passed as opaque `void *'.  Strings are copied into
 * the caller's buffers so that the ObjC side never holds a pointer into
 * XPCE's UTF-8 ring buffer (see charArrayToUTF8(), src/txt/i18n.c).
 *
 * There is exactly one NSMenu for the process, as MacOS has exactly one
 * menu bar.  ns_menubar_install() re-populates it for the menu_bar of
 * the frame that just got the focus.  We do not keep an NSMenu per
 * frame: an NSMenu can have only one supermenu, so the application,
 * Window and Help menus cannot be shared between per-frame instances.
 */

#define PCE_MI_NORMAL		0	/* plain item */
#define PCE_MI_SUBMENU		1	/* pull-right */

#define PCE_MOD_SHIFT		0x1
#define PCE_MOD_CONTROL		0x2
#define PCE_MOD_OPTION		0x4
#define PCE_MOD_COMMAND		0x8

typedef struct
{ char		title[256];	/* UTF-8 label */
  char		key[32];	/* UTF-8 key equivalent, "" if none */
  char		shortcut[64];	/* accelerator we cannot register as a key
				   equivalent, to show as plain text */
  unsigned	modifiers;	/* PCE_MOD_* mask for <-key */
  int		kind;		/* PCE_MI_* */
  bool		enabled;	/* menu_item<-active */
  bool		checked;	/* menu_item<-selected */
  bool		separator;	/* menu_item<-end_group: separator after */
  void	       *token;		/* the menu_item */
  void	       *submenu;	/* the popup, if kind == PCE_MI_SUBMENU */
} pce_menu_item;

		 /*******************************
		 *	  Cocoa -> XPCE		*
		 *******************************/

/* All of these run on the SDL main thread and may run Prolog, so the
 * caller must be prepared for re-entrancy.  pce_menu_invoke() does not
 * run Prolog itself; it defers through the SDL event queue.
 */

int	pce_menubar_popup_count(void *mb);
bool	pce_menubar_popup(void *mb, int i,
			  char *title, size_t titlelen,
			  void **popup, bool *is_help);
void	pce_popup_update(void *mb, void *popup);
int	pce_popup_item_count(void *popup);
bool	pce_popup_item(void *popup, int i, pce_menu_item *out);
/* `popups' runs from the popup directly holding `item' up to the
 * top-level popup of the menu bar; popup->execute needs that whole
 * chain.  It cannot be recovered from the XPCE model: popup<-context
 * is set when a popup is appended to a menu (getConvertMenuItem()) but
 * not when it is attached with menu_item->popup (popupMenuItem()),
 * which is what Epilog's pull-rights use.  Cocoa knows the nesting, so
 * we take it from there.
 */

#define PCE_MAX_MENU_DEPTH 16

void	pce_menu_invoke(void *mb, void **popups, int npopups, void *item);
void	pce_menu_quit(void);

		 /*******************************
		 *	  XPCE -> Cocoa		*
		 *******************************/

/* Create the shared main menu holding just the application menu. */
void	ns_menubar_setup(const char *appname);
/* Populate the shared main menu for `mb', or for no menu bar if NULL. */
void	ns_menubar_install(void *mb);
/* `mb' changed; rebuild if it is the one currently installed. */
void	ns_menubar_dirty(void *mb);
/* `mb' is being destroyed; forget it if it is currently installed. */
void	ns_menubar_forget(void *mb);
/* True if the menu bar currently shown has a key equivalent for this
 * keystroke and will therefore act on it itself.  `key' is the UTF-8
 * key equivalent and `mods' a PCE_MOD_* mask, both as they are handed
 * to ns_menubar_install() in pce_menu_item.
 */
bool	ns_menubar_owns_key(const char *key, unsigned mods);
/* The modifier keys that are down right now, as a PCE_MOD_* mask. */
unsigned ns_current_modifiers(void);

#endif /* SDLNSMENU_H */
