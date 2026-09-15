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

/* The Cocoa half of the native MacOS menu bar.  Includes Cocoa but NO
 * XPCE header; see sdlnsmenu.h for why.
 */

#import <Cocoa/Cocoa.h>
#import <objc/runtime.h>
#include "sdlnsmenu.h"

		 /*******************************
		 *	    THE PAYLOAD		*
		 *******************************/

/* Carried by each NSMenuItem so the action knows what to execute. */

@interface XPCEMenuRef : NSObject
@property (nonatomic, assign) void *mb;
@property (nonatomic, assign) void *popup;
@property (nonatomic, assign) void *item;
@end

@implementation XPCEMenuRef
@end


		 /*******************************
		 *	    THE DELEGATE	*
		 *******************************/

/* One delegate per popup.  -menuNeedsUpdate: is the analogue of
 * showPopupMenuBar() in src/men/menubar.c: it runs `popup->update' and
 * then rebuilds the items.
 */

@interface XPCEMenuDelegate : NSObject <NSMenuDelegate>
@property (nonatomic, assign) void *mb;
@property (nonatomic, assign) void *popup;
@end

static void	populate_menu(NSMenu *menu, void *mb, void *popup, int depth);

/* NSMenu holds its delegate weakly; tie the delegate to the menu. */
static const char delegate_key;

@implementation XPCEMenuDelegate

- (void)menuNeedsUpdate:(NSMenu *)menu
{ pce_popup_update(self.mb, self.popup);
  populate_menu(menu, self.mb, self.popup, 0);
}

/* Implementing this is what keeps AppKit from calling
 * -menuNeedsUpdate: --- and therefore Prolog --- on every single
 * keystroke while it searches for a key equivalent.  We answer from
 * the items that are already there, which were populated when the menu
 * bar was installed and are refreshed whenever the menu is opened.
 */

- (BOOL)menu:(NSMenu *)menu
      hasKeyEquivalent:(NSEvent *)event
		target:(id *)target
		action:(SEL *)action
{ NSString *chars = [event charactersIgnoringModifiers];
  NSEventModifierFlags mods = [event modifierFlags] &
	  ( NSEventModifierFlagShift	 | NSEventModifierFlagControl |
	    NSEventModifierFlagOption	 | NSEventModifierFlagCommand );

  if ( [chars length] == 0 )
    return NO;
  chars = [chars lowercaseString];

  for(NSMenuItem *mi in [menu itemArray])
  { if ( ![mi isEnabled] || [[mi keyEquivalent] length] == 0 )
      continue;
    if ( [[[mi keyEquivalent] lowercaseString] isEqualToString:chars] &&
	 [mi keyEquivalentModifierMask] == mods )
    { *target = [mi target];
      *action = [mi action];
      return YES;
    }
  }

  return NO;
}

@end


		 /*******************************
		 *	     THE TARGET		*
		 *******************************/

@interface XPCEMenuTarget : NSObject
- (void)invoke:(id)sender;
- (void)quit:(id)sender;
@end

@implementation XPCEMenuTarget

- (void)invoke:(id)sender
{ NSMenuItem *item = (NSMenuItem *)sender;
  XPCEMenuRef *ref = [item representedObject];
  void *popups[PCE_MAX_MENU_DEPTH];
  int n = 0;

  if ( !ref )
    return;

  /* Collect the popups from the one holding the item up to the
   * top-level one.  main_menu carries no delegate, which ends the walk.
   */
  for(NSMenu *m = [item menu]; m && n < PCE_MAX_MENU_DEPTH; m = [m supermenu])
  { XPCEMenuDelegate *d = objc_getAssociatedObject(m, &delegate_key);

    if ( !d )
      break;
    popups[n++] = d.popup;
  }

  if ( n > 0 )
    pce_menu_invoke(ref.mb, popups, n, ref.item);
}

- (void)quit:(id)sender
{ (void)sender;
  pce_menu_quit();
}

/* Menu items are enabled explicitly from menu_item<-active, so keep
 * AppKit's automatic enabling out of it.
 */

- (BOOL)validateMenuItem:(NSMenuItem *)item
{ (void)item;
  return YES;
}

@end


		 /*******************************
		 *	     GLOBALS		*
		 *******************************/

static NSMenu	      *main_menu;	/* [NSApp mainMenu] */
static NSMenu	      *app_menu;	/* the application menu */
static NSMenu	      *window_menu;	/* the Window menu */
static XPCEMenuTarget *menu_target;
static void	      *installed_mb;	/* menu_bar currently shown */

		 /*******************************
		 *	    BUILDING		*
		 *******************************/

static NSString *
utf8(const char *s)
{ NSString *str = s ? [NSString stringWithUTF8String:s] : nil;

  return str ? str : @"";
}


static NSEventModifierFlags
ns_modifiers(unsigned mods)
{ NSEventModifierFlags m = 0;

  if ( mods & PCE_MOD_SHIFT )   m |= NSEventModifierFlagShift;
  if ( mods & PCE_MOD_CONTROL ) m |= NSEventModifierFlagControl;
  if ( mods & PCE_MOD_OPTION )  m |= NSEventModifierFlagOption;
  if ( mods & PCE_MOD_COMMAND ) m |= NSEventModifierFlagCommand;

  return m;
}


/* (Re)fill `menu' from `popup'.  The caller has already run
 * `popup->update', so menu_item<-active is current.
 */

/* An accelerator MacOS cannot own -- a two-key sequence like `^X ^S',
 * or one we will not take from the editor -- is still worth showing.
 * It goes in the title, after the label.
 *
 * Not as the key equivalent: AppKit draws only the first character of a
 * multi-character one.  Nor as a right-aligned attributed title: that
 * sits at a tab stop of ours while AppKit right-aligns the real key
 * equivalents in a column of its own, which gives a menu two columns of
 * accelerators.
 */

/* A pull-right is filled here rather than left to its own
 * -menuNeedsUpdate:, so that its key equivalents are live before the
 * menu has ever been opened.  MAX_DEPTH guards against a popup that
 * (directly or indirectly) contains itself.
 */

#define MAX_DEPTH 8

static void
populate_menu(NSMenu *menu, void *mb, void *popup, int depth)
{ int n = pce_popup_item_count(popup);

  [menu removeAllItems];

  if ( depth >= MAX_DEPTH )
    return;

  for(int i=0; i<n; i++)
  { pce_menu_item pmi;

    if ( !pce_popup_item(popup, i, &pmi) )
      continue;

    NSString *title = utf8(pmi.title);

    if ( pmi.shortcut[0] && !pmi.key[0] )
      title = [NSString stringWithFormat:@"%@   %@",
			title, utf8(pmi.shortcut)];

    NSMenuItem *mi = [[NSMenuItem alloc] initWithTitle:title
					 action:NULL
					 keyEquivalent:@""];
    [mi setEnabled:pmi.enabled ? YES : NO];
    [mi setState:pmi.checked ? NSControlStateValueOn
			     : NSControlStateValueOff];

    if ( pmi.kind == PCE_MI_SUBMENU )
    { NSMenu *sub = [[NSMenu alloc] initWithTitle:utf8(pmi.title)];
      XPCEMenuDelegate *d = [[XPCEMenuDelegate alloc] init];

      d.mb = mb;
      d.popup = pmi.submenu;
      [sub setDelegate:d];
      [sub setAutoenablesItems:NO];
      objc_setAssociatedObject(sub, &delegate_key, d,
			       OBJC_ASSOCIATION_RETAIN);
      /* Fill from the current model so key equivalents are live before
       * the menu is ever opened, but do NOT run `popup->update' here:
       * see the comment on ns_menubar_install().
       */
      populate_menu(sub, mb, pmi.submenu, depth+1);
      [mi setSubmenu:sub];
    } else
    { XPCEMenuRef *ref = [[XPCEMenuRef alloc] init];

      ref.mb    = mb;
      ref.popup = popup;
      ref.item  = pmi.token;
      [mi setRepresentedObject:ref];
      [mi setTarget:menu_target];
      [mi setAction:@selector(invoke:)];

      if ( pmi.key[0] )
      { [mi setKeyEquivalent:utf8(pmi.key)];
	[mi setKeyEquivalentModifierMask:ns_modifiers(pmi.modifiers)];
      }						/* else: in the title */
    }

    [menu addItem:mi];

    if ( pmi.separator )
      [menu addItem:[NSMenuItem separatorItem]];
  }
}


		 /*******************************
		 *	  KEY EQUIVALENTS	*
		 *******************************/

/* SDL hands us every key event, including the ones MacOS has just
 * dispatched to a menu item as a key equivalent: SDL's Cocoa backend
 * reports the key from -sendEvent: whether or not the main menu
 * consumed it.  Without this test both paths run the command, which is
 * why Command-V pasted twice.  We answer from the items rather than
 * from -performKeyEquivalent:, which would run the item a third time.
 */

static bool
menu_owns_key(NSMenu *menu, NSString *key, NSEventModifierFlags mods,
	      int depth)
{ if ( depth >= MAX_DEPTH )
    return false;

  for(NSMenuItem *mi in [menu itemArray])
  { NSMenu *sub = [mi submenu];

    if ( sub && menu_owns_key(sub, key, mods, depth+1) )
      return true;

    if ( ![mi isEnabled] || [[mi keyEquivalent] length] == 0 )
      continue;
    if ( [[[mi keyEquivalent] lowercaseString] isEqualToString:key] &&
	 [mi keyEquivalentModifierMask] == mods )
      return true;
  }

  return false;
}


bool
ns_menubar_owns_key(const char *key, unsigned mods)
{ if ( !main_menu || !key || !key[0] )
    return false;

  @autoreleasepool
  { NSString *k = [utf8(key) lowercaseString];

    return menu_owns_key(main_menu, k, ns_modifiers(mods), 0);
  }
}


		 /*******************************
		 *	  THE APP MENU		*
		 *******************************/

static void
build_app_menu(const char *appname)
{ NSString *name = utf8(appname);

  app_menu = [[NSMenu alloc] initWithTitle:name];
  [app_menu setAutoenablesItems:NO];

  NSMenuItem *mi;

  mi = [app_menu addItemWithTitle:
	   [NSString stringWithFormat:@"About %@", name]
	   action:@selector(orderFrontStandardAboutPanel:)
	   keyEquivalent:@""];
  [mi setTarget:NSApp];

  [app_menu addItem:[NSMenuItem separatorItem]];

  mi = [app_menu addItemWithTitle:
	   [NSString stringWithFormat:@"Hide %@", name]
	   action:@selector(hide:) keyEquivalent:@"h"];
  [mi setTarget:NSApp];

  mi = [app_menu addItemWithTitle:@"Hide Others"
	   action:@selector(hideOtherApplications:) keyEquivalent:@"h"];
  [mi setKeyEquivalentModifierMask:NSEventModifierFlagCommand |
				   NSEventModifierFlagOption];
  [mi setTarget:NSApp];

  mi = [app_menu addItemWithTitle:@"Show All"
	   action:@selector(unhideAllApplications:) keyEquivalent:@""];
  [mi setTarget:NSApp];

  [app_menu addItem:[NSMenuItem separatorItem]];

  mi = [app_menu addItemWithTitle:
	   [NSString stringWithFormat:@"Quit %@", name]
	   action:@selector(quit:) keyEquivalent:@"q"];
  [mi setTarget:menu_target];
}


		 /*******************************
		 *	     ENTRIES		*
		 *******************************/

void
ns_menubar_setup(const char *appname)
{ if ( main_menu )
    return;

  @autoreleasepool
  { menu_target = [[XPCEMenuTarget alloc] init];

    build_app_menu(appname);
    window_menu = [[NSMenu alloc] initWithTitle:@"Window"];

    main_menu = [[NSMenu alloc] initWithTitle:@""];
    [main_menu setAutoenablesItems:NO];

    NSMenuItem *appitem = [[NSMenuItem alloc] initWithTitle:@""
					      action:NULL keyEquivalent:@""];
    [appitem setSubmenu:app_menu];
    [main_menu addItem:appitem];

    [NSApp setMainMenu:main_menu];
    [NSApp setWindowsMenu:window_menu];
  }
}


/* Populate the whole bar by READING the XPCE model only.  We must not
 * run `popup->update' here: this is called from SDL_EVENT_WINDOW_FOCUS_
 * GAINED, i.e. from inside event dispatch while a frame is being opened
 * and other threads are mid-handshake.  Epilog's Debug and Settings
 * conditions reach terminal_prolog_flag/4 -> call_in_thread/3, which
 * waits on the toplevel thread while that thread may itself be waiting
 * on this one through in_pce_thread_sync/1 -- a circular wait that
 * beachballs the application.
 *
 * Conditions are therefore evaluated only in -menuNeedsUpdate:, when
 * the user actually opens a menu.  That is the same moment at which
 * showPopupMenuBar() runs them for the drawn menu bar.
 */

void
ns_menubar_install(void *mb)
{ if ( !main_menu )
    return;

  @autoreleasepool
  { NSMenuItem *helpitem = nil;

    installed_mb = mb;

    /* Drop everything except the application menu at index 0. */
    while( [main_menu numberOfItems] > 1 )
      [main_menu removeItemAtIndex:1];

    if ( mb )
    { int n = pce_menubar_popup_count(mb);

      for(int i=0; i<n; i++)
      { char title[256];
	void *popup;
	bool is_help = false;

	if ( !pce_menubar_popup(mb, i, title, sizeof(title),
				&popup, &is_help) )
	  continue;

	NSMenu *sub = [[NSMenu alloc] initWithTitle:utf8(title)];
	XPCEMenuDelegate *d = [[XPCEMenuDelegate alloc] init];

	d.mb = mb;
	d.popup = popup;
	[sub setDelegate:d];
	[sub setAutoenablesItems:NO];
	objc_setAssociatedObject(sub, &delegate_key, d,
				 OBJC_ASSOCIATION_RETAIN);
	populate_menu(sub, mb, popup, 0);

	NSMenuItem *mi = [[NSMenuItem alloc] initWithTitle:utf8(title)
					     action:NULL keyEquivalent:@""];
	[mi setSubmenu:sub];

	if ( is_help )
	  helpitem = mi;
	else
	  [main_menu addItem:mi];
      }
    }

					/* Window before Help */
    NSMenuItem *witem = [[NSMenuItem alloc] initWithTitle:@"Window"
					    action:NULL keyEquivalent:@""];
    [witem setSubmenu:window_menu];
    [main_menu addItem:witem];

    if ( helpitem )
    { [main_menu addItem:helpitem];
      [NSApp setHelpMenu:[helpitem submenu]];
    } else
    { [NSApp setHelpMenu:nil];
    }
  }
}


void
ns_menubar_dirty(void *mb)
{ if ( mb && mb == installed_mb )
    ns_menubar_install(mb);
}


void
ns_menubar_forget(void *mb)
{ if ( mb && mb == installed_mb )
    ns_menubar_install(NULL);
}


		 /*******************************
		 *	    MODIFIERS		*
		 *******************************/

/* +modifierFlags is the state of the keys now, not as SDL remembers
 * it.  SDL misses a modifier that went down while another window had
 * the focus.
 */

unsigned
ns_current_modifiers(void)
{ NSEventModifierFlags f = [NSEvent modifierFlags];
  unsigned mods = 0;

  if ( f & NSEventModifierFlagShift )   mods |= PCE_MOD_SHIFT;
  if ( f & NSEventModifierFlagControl ) mods |= PCE_MOD_CONTROL;
  if ( f & NSEventModifierFlagOption )  mods |= PCE_MOD_OPTION;
  if ( f & NSEventModifierFlagCommand ) mods |= PCE_MOD_COMMAND;

  return mods;
}
