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

/* The XPCE half of the native MacOS menu bar.  Includes XPCE but NO
 * Cocoa header.
 *
 * The two halves cannot be merged.  In Objective-C `Class' is a builtin
 * type, so XPCE's `typedef struct class * Class' (src/h/types.h:77) is
 * silently ignored rather than rejected, and every XPCE use of `Class'
 * would quietly mean the Objective-C class type instead.  That is a
 * miscompile, not a compile error, so keep Cocoa out of this file.
 */

#include <h/kernel.h>
#include <h/graphics.h>
#include "sdl.h"
#include "sdlframe.h"
#include "sdlmenubar.h"
#include "sdluserevent.h"

#ifdef __APPLE__
#include "sdlnsmenu.h"

static MenuBar	installed_menubar;	/* what Cocoa currently shows */
static bool	menubar_setup_done;

		 /*******************************
		 *	    AVAILABILITY	*
		 *******************************/

/* Native menus need a real Cocoa NSApp.  Under SDL_VIDEODRIVER=dummy,
 * which is what the test suite uses, there is none, so we keep drawing
 * the XPCE menu bar and the headless tests see unchanged geometry.
 */

bool
ws_has_native_menubar(MenuBar mb)
{ const char *drv = SDL_GetCurrentVideoDriver();
  Any val;

  if ( !drv || strcmp(drv, "cocoa") != 0 )
    return false;			/* not cached: this may be called
					   before the video driver is up */

  /* Per instance, so a subclass can opt out with its own
     class_variable(native, bool, @off). */
  val = ( mb ? getClassVariableValueObject(mb, NAME_native)
	     : getClassVariableValueClass(ClassMenuBar, NAME_native) );

  return val == ON;
}


		 /*******************************
		 *	  FRAME -> MENU_BAR	*
		 *******************************/

static MenuBar
device_menu_bar(Device dev)
{ Cell cell;

  for_cell(cell, dev->graphicals)
  { Graphical gr = cell->value;

    if ( instanceOfObject(gr, ClassMenuBar) )
      return (MenuBar)gr;
    if ( instanceOfObject(gr, ClassDevice) )
    { MenuBar mb = device_menu_bar((Device)gr);

      if ( mb )
	return mb;
    }
  }

  return NULL;
}


static MenuBar
frame_menu_bar(FrameObj fr)
{ Cell cell;

  if ( fr->kind != NAME_toplevel )
    return NULL;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;
    MenuBar mb = device_menu_bar((Device)sw);

    if ( mb )
      return mb;
  }

  return NULL;
}


		 /*******************************
		 *	     ACTIVATION		*
		 *******************************/

static void
menubar_setup(void)
{ if ( !menubar_setup_done )
  { /* Same name SDL was given in setPceThread(): `SWI-Prolog' for a
       bundle, `swipl-win' for epilog, else `swipl'.  See sdl.c. */
    const char *app = SDL_GetAppMetadataProperty(
			  SDL_PROP_APP_METADATA_NAME_STRING);

    menubar_setup_done = true;
    ns_menubar_setup(app && app[0] ? app : "SWI-Prolog");
  }
}


void
ws_menubar_activate_frame(FrameObj fr)
{ if ( !ws_has_native_menubar(NULL) )
    return;

  menubar_setup();

  MenuBar mb = frame_menu_bar(fr);

  if ( mb != installed_menubar )
  { installed_menubar = mb;
    ns_menubar_install(mb);
  }
}


void
ws_menubar_changed(MenuBar mb)
{ if ( ws_has_native_menubar(mb) && mb == installed_menubar )
    ns_menubar_dirty(mb);
}


void
ws_menubar_destroyed(MenuBar mb)
{ if ( mb == installed_menubar )
  { installed_menubar = NULL;
    if ( ws_has_native_menubar(mb) )
      ns_menubar_forget(mb);
  }
}


		 /*******************************
		 *	    ACCELERATORS	*
		 *******************************/

/* menu_item<-accelerator is a display label and comes in two flavours:
 * hard-coded ASCII such as `Shift-Ctrl-M' or `F5' (epilog.pl), and the
 * Apple glyph form `<cmd><shift>M' produced by key_binding
 * <-accelerator_label (prolog/boot/pce_keybinding.pl).  Accept both and
 * fail silently on anything else.
 *
 * A Control accelerator is offered to MacOS as Command: that is purely
 * additive, as the original Control keystroke still reaches XPCE.  An
 * accelerator that already says Command is passed through unchanged.
 */

typedef struct
{ const char *name;
  unsigned    mod;
} modname;

static const modname modnames[] =
{ { "\\C-",	 PCE_MOD_CONTROL },
  { "Ctrl-",	 PCE_MOD_CONTROL },
  { "Control-",	 PCE_MOD_CONTROL },
  { "⌃",	 PCE_MOD_CONTROL },		/* UP ARROWHEAD */
  { "\\S-",	 PCE_MOD_SHIFT   },
  { "Shift-",	 PCE_MOD_SHIFT   },
  { "⇧",	 PCE_MOD_SHIFT   },		/* UPWARDS WHITE ARROW */
  { "\\s-",	 PCE_MOD_COMMAND },
  { "Super-",	 PCE_MOD_COMMAND },
  { "Cmd-",	 PCE_MOD_COMMAND },
  { "Command-",	 PCE_MOD_COMMAND },
  { "⌘",	 PCE_MOD_COMMAND },		/* PLACE OF INTEREST SIGN */
  { "Alt-",	 PCE_MOD_OPTION  },
  { "Meta-",	 PCE_MOD_OPTION  },
  { "M-",	 PCE_MOD_OPTION  },
  { "⌥",	 PCE_MOD_OPTION  },		/* OPTION KEY */
  { NULL,	 0 }
};

/* Function keys are NSF1FunctionKey... in the Unicode private use area.
 * We pass them as UTF-8 so that the Cocoa side needs no table.
 */
#define NS_F1_KEY 0xF704

static bool
utf8_put(char *out, size_t len, unsigned int code)
{ if ( code < 0x80 )
  { if ( len < 2 ) return false;
    out[0] = (char)code;
    out[1] = 0;
  } else if ( code < 0x800 )
  { if ( len < 3 ) return false;
    out[0] = (char)(0xc0 | (code>>6));
    out[1] = (char)(0x80 | (code & 0x3f));
    out[2] = 0;
  } else
  { if ( len < 4 ) return false;
    out[0] = (char)(0xe0 | (code>>12));
    out[1] = (char)(0x80 | ((code>>6) & 0x3f));
    out[2] = (char)(0x80 | (code & 0x3f));
    out[3] = 0;
  }

  return true;
}


static bool
parse_accelerator(const char *s, char *key, size_t keylen, unsigned *modp)
{ unsigned mods = 0;
  bool fkey = false;			/* F1..F12: safe to take bare */

  if ( !s )
    return false;

  for(;;)				/* strip modifiers */
  { const modname *m;

    while( s[0] == (char)0xe2 && s[1] == (char)0x80 &&
	   s[2] == (char)0x89 )		/* U+2009 THIN SPACE */
      s += 3;

    for(m = modnames; m->name; m++)
    { size_t l = strlen(m->name);

      if ( strncmp(s, m->name, l) == 0 && s[l] )
      { mods |= m->mod;
	s += l;
	break;
      }
    }
    if ( !m->name )
      break;
  }

  if ( !s[0] )
    return false;

					/* function keys */
  if ( (s[0] == 'F' || s[0] == 'f') && isdigit((unsigned char)s[1]) )
  { char *e;
    long n = strtol(s+1, &e, 10);

    if ( *e == 0 && n >= 1 && n <= 12 )
    { if ( !utf8_put(key, keylen, (unsigned int)(NS_F1_KEY + n - 1)) )
	return false;
      fkey = true;
      goto done;
    }
  }

  /* Named keys.  characterName() writes them between angle brackets,
   * e.g. `<cursor_left>', and key_binding<-accelerator_label turns that
   * into a glyph on MacOS, so accept both.  MacOS spells these in the
   * Unicode private use area and NSMenuItem draws the usual glyph.
   */
  { static const struct { const char *name; unsigned int code; } fkeys[] =
      { { "<cursor_up>",    0xF700 },	/* NSUpArrowFunctionKey */
	{ "↑",		    0xF700 },
	{ "<cursor_down>",  0xF701 },
	{ "↓",		    0xF701 },
	{ "<cursor_left>",  0xF702 },
	{ "←",		    0xF702 },
	{ "<cursor_right>", 0xF703 },
	{ "→",		    0xF703 },
	{ "<cursor_home>",  0xF729 },	/* NSHomeFunctionKey */
	{ "↖",		    0xF729 },
	{ "<end>",	    0xF72B },	/* NSEndFunctionKey */
	{ "↘",		    0xF72B },
	{ "<page_up>",	    0xF72C },
	{ "⇞",		    0xF72C },
	{ "<page_down>",    0xF72D },
	{ "⇟",		    0xF72D },
	{ NULL, 0 }
      };
    int i;

    for(i=0; fkeys[i].name; i++)
    { if ( strcmp(s, fkeys[i].name) == 0 )
      { if ( !utf8_put(key, keylen, fkeys[i].code) )
	  return false;
	goto done;
      }
    }
  }

  if ( s[0] == '<' )
  { if ( (s[1] == 'f' || s[1] == 'F') && isdigit((unsigned char)s[2]) )
    { char *e;
      long n = strtol(s+2, &e, 10);

      if ( strcmp(e, ">") == 0 && n >= 1 && n <= 12 )
      { if ( !utf8_put(key, keylen, (unsigned int)(NS_F1_KEY + n - 1)) )
	  return false;
	fkey = true;
	goto done;
      }
    }

    return false;			/* some other named key */
  }

  /* Both the bare name characterName() writes and the glyph
   * key_binding<-accelerator_label turns it into on MacOS.
   */
  { static const struct { const char *name; char chr; } named[] =
      { { "RET", '\r' }, { "Return", '\r' }, { "↩", '\r' },
	{ "TAB", '\t' }, { "Tab", '\t' }, { "⇥", '\t' },
	{ "SPC", ' ' },   { "Space", ' ' },  { "␣", ' ' },
	{ "BS", '\b' },   { "Backspace", '\b' }, { "⌫", '\b' },
	{ "DEL", 0x7f },  { "Delete", 0x7f }, { "⌦", 0x7f },
	{ "ESC", 0x1b },  { "Escape", 0x1b }, { "⎋", 0x1b },
	{ NULL, 0 }
      };
    int i;

    for(i=0; named[i].name; i++)
    { if ( strcmp(s, named[i].name) == 0 )
      { if ( keylen < 2 ) return false;
	key[0] = named[i].chr;
	key[1] = 0;
	goto done;
      }
    }
  }

  if ( strlen(s) == 1 )			/* a plain character */
  { if ( keylen < 2 ) return false;
    key[0] = (char)tolower((unsigned char)s[0]);
    key[1] = 0;
    goto done;
  }

  key[0] = 0;
  return false;				/* not understood; no shortcut */

done:
  /* A Control accelerator is offered to MacOS as Command, which is
   * additive: the Control keystroke still reaches XPCE.  But when the
   * accelerator already uses Command the Control is part of the key --
   * Control-Command-Left is not Command-Left -- so keep both.
   */
  if ( (mods & PCE_MOD_CONTROL) && !(mods & PCE_MOD_COMMAND) )
  { mods &= ~PCE_MOD_CONTROL;
    mods |= PCE_MOD_COMMAND;
  }
  /* Command or Option means the accelerator is a real XPCE binding we
   * can hand to MacOS: routing it through the menu runs the same
   * command.  A function key is safe to take bare, as it is not text
   * input.  Anything else -- a bare or merely shifted key -- would be
   * taken from ordinary typing, so leave it to XPCE and let the caller
   * show it as text.
   */
  if ( !(mods & (PCE_MOD_COMMAND|PCE_MOD_OPTION)) && !fkey )
  { key[0] = 0;				/* we got as far as writing one */
    return false;
  }

  *modp = mods;
  return true;
}


/* True when the MacOS menu bar will act on this keystroke itself.
 * SDL's Cocoa backend reports every key event, including the ones the
 * main menu has just dispatched as a key equivalent, so without this
 * test both the menu item and the XPCE key binding run the command:
 * Command-V pasted twice.  We translate the SDL keystroke the way
 * parse_accelerator() translates a menu_item<-accelerator and ask
 * Cocoa whether an enabled item claims it.
 */

bool
ws_menubar_key_equivalent(SDL_Event *ev)
{ SDL_Keycode k = ev->key.key;
  SDL_Keymod m  = ev->key.mod;
  unsigned mods = 0;
  char key[8];

  if ( !menubar_setup_done || !ws_has_native_menubar(NULL) )
    return false;

  if ( m & SDL_KMOD_SHIFT ) mods |= PCE_MOD_SHIFT;
  if ( m & SDL_KMOD_CTRL )  mods |= PCE_MOD_CONTROL;
  if ( m & SDL_KMOD_ALT )   mods |= PCE_MOD_OPTION;
  if ( m & SDL_KMOD_GUI )   mods |= PCE_MOD_COMMAND;

  /* Only the accelerators parse_accelerator() hands to MacOS can be
   * claimed, so ordinary typing never walks the menus.
   */
  if ( !(mods & (PCE_MOD_COMMAND|PCE_MOD_OPTION)) &&
       !(k >= SDLK_F1 && k <= SDLK_F12) )
    return false;

  if ( k >= ' ' && k < DEL )
  { key[0] = (char)tolower((int)k);
    key[1] = 0;
  } else if ( k >= SDLK_F1 && k <= SDLK_F12 )
  { if ( !utf8_put(key, sizeof(key),
		   (unsigned int)(NS_F1_KEY + (k-SDLK_F1))) )
      return false;
  } else
  { unsigned int code;

    switch(k)
    { case SDLK_RETURN:
      case SDLK_KP_ENTER:  code = '\r';   break;
      case SDLK_TAB:	   code = '\t';   break;
      case SDLK_BACKSPACE: code = '\b';   break;
      case SDLK_ESCAPE:	   code = 0x1b;   break;
      case SDLK_DELETE:	   code = 0x7f;   break;
      case SDLK_UP:	   code = 0xF700; break;
      case SDLK_DOWN:	   code = 0xF701; break;
      case SDLK_LEFT:	   code = 0xF702; break;
      case SDLK_RIGHT:	   code = 0xF703; break;
      case SDLK_HOME:	   code = 0xF729; break;
      case SDLK_END:	   code = 0xF72B; break;
      case SDLK_PAGEUP:	   code = 0xF72C; break;
      case SDLK_PAGEDOWN:  code = 0xF72D; break;
      default:		   return false;
    }
    if ( !utf8_put(key, sizeof(key), code) )
      return false;
  }

  return ns_menubar_owns_key(key, mods);
}


		 /*******************************
		 *	  COCOA CALLBACKS	*
		 *******************************/

/* Everything below is called from sdlnsmenu.m on the SDL main thread.
 * Labels are copied into the caller's buffer because nameToUTF8() and
 * charArrayToUTF8() answer a pointer into a rotating ring buffer.
 */

static bool
alive(Any obj, Class cl)
{ return ( obj && !isNil(obj) &&
	   !onFlag(obj, F_FREED|F_FREEING) &&
	   instanceOfObject(obj, cl) );
}


static void
label_text(Any label, Any fallback, char *out, size_t len)
{ char *s = NULL;

  if ( instanceOfObject(label, ClassCharArray) )
    s = charArrayToUTF8((CharArray)label);
  else if ( instanceOfObject(fallback, ClassCharArray) )
    s = charArrayToUTF8((CharArray)fallback);

  if ( s && s[0] )
  { strncpy(out, s, len-1);
    out[len-1] = 0;
  } else				/* e.g. an image label */
  { strncpy(out, "...", len-1);
    out[len-1] = 0;
  }
}


int
pce_menubar_popup_count(void *mb)
{ MenuBar m = mb;

  if ( !alive(m, ClassMenuBar) )
    return 0;

  return valInt(getSizeChain(m->members));
}


bool
pce_menubar_popup(void *mb, int i, char *title, size_t titlelen,
		  void **popup, bool *is_help)
{ MenuBar m = mb;
  PopupObj p;

  if ( !alive(m, ClassMenuBar) ||
       !(p = getNth0Chain(m->members, toInt(i))) ||
       !alive(p, ClassPopup) )
    return false;

  label_text(p->label, p->name, title, titlelen);
  *popup   = p;
  *is_help = (p->name == NAME_help);

  return true;
}


/* Menu code routinely refers to @event: Epilog builds both the messages
 * and the conditions of its menu bar around `@event?receiver?frame'.
 * The drawn menu bar runs them from inside event dispatch, where @event
 * is bound.  We run from -menuNeedsUpdate: and from the SDL event
 * queue, so we must bind one ourselves.
 *
 * Leaving it unbound is not merely a failed lookup: `@nil <-receiver'
 * raises an XPCE error, and reporting that error writes to the Epilog
 * console.  That write blocks once the pty buffer fills, and the pty is
 * drained by the main thread -- which, when this runs from a menu
 * delegate, is sitting inside AppKit's menu tracking loop.  The
 * application deadlocks.
 *
 * Answer NULL if the menu_bar is not in a window; the caller then runs
 * without a bound event.
 */

static EventObj
menubar_event(MenuBar mb)
{ PceWindow sw = getWindowGraphical((Graphical)mb);

  if ( !sw )
    return NULL;

  return newObject(ClassEvent, NAME_msLeftUp, sw,
		   DEFAULT, DEFAULT, DEFAULT, DEFAULT, EAV);
}


void
pce_popup_update(void *mb, void *popup)
{ PopupObj p = popup;
  MenuBar m = mb;
  static bool updating;			/* guard against re-entry */

  if ( updating || !alive(p, ClassPopup) || !alive(m, ClassMenuBar) )
    return;

  updating = true;
  { AnswerMark mark;
    EventObj ev;

    pceMTLock();
    markAnswerStack(mark);
    if ( (ev=menubar_event(m)) )
    { addCodeReference(ev);
      withLocalVars({ assignVar(EVENT, ev, NAME_local);
		      assign(ev, receiver, m);
		      send(p, NAME_update, m, EAV);
		    });			/* as showPopupMenuBar() does */
      delCodeReference(ev);
      freeableObj(ev);
    } else
    { send(p, NAME_update, m, EAV);
    }
    rewindAnswerStack(mark, NIL);
    pceMTUnlock();
  }
  updating = false;
}


int
pce_popup_item_count(void *popup)
{ PopupObj p = popup;

  if ( !alive(p, ClassPopup) )
    return 0;

  return valInt(getSizeChain(p->members));
}


bool
pce_popup_item(void *popup, int i, pce_menu_item *out)
{ PopupObj p = popup;
  MenuItem mi;

  if ( !alive(p, ClassPopup) ||
       !(mi = getNth0Chain(p->members, toInt(i))) ||
       !alive(mi, ClassMenuItem) )
    return false;

  memset(out, 0, sizeof(*out));
  label_text(mi->label, mi->value, out->title, sizeof(out->title));
  out->enabled   = (mi->active == ON);
  out->checked   = (p->show_current == ON && mi->selected == ON);
  out->separator = (mi->end_group == ON);
  out->token     = mi;

  if ( notNil(mi->popup) && alive(mi->popup, ClassPopup) )
  { out->kind    = PCE_MI_SUBMENU;
    out->submenu = mi->popup;
  } else
  { out->kind = PCE_MI_NORMAL;

    if ( isName(mi->accelerator) )
    { const char *acc = nameToUTF8(mi->accelerator);

      if ( !parse_accelerator(acc, out->key, sizeof(out->key),
			      &out->modifiers) )
      { /* Not a keystroke MacOS can attach to the item: a two-key
	   sequence such as `^X ^S', or a key we will not take from the
	   editor.  Show it as text so the binding is still visible. */
	strncpy(out->shortcut, acc, sizeof(out->shortcut)-1);
	out->shortcut[sizeof(out->shortcut)-1] = 0;
      }
    }
  }

  return true;
}


		 /*******************************
		 *	     EXECUTE		*
		 *******************************/

/* popup->execute (src/men/popup.c) walks p = p->selected_item, so for a
 * pull-right the parent popup must hold the sub-POPUP and only the
 * innermost popup the menu_item.  The chain comes from Cocoa (see
 * pce_menu_invoke() in sdlnsmenu.h); we cannot walk popup<-context,
 * which menu_item->popup does not set.
 */

typedef struct
{ MenuBar  mb;
  MenuItem item;
  int      npopups;
  PopupObj popups[PCE_MAX_MENU_DEPTH];	/* innermost first */
} menu_invocation;


/* Called from the Cocoa action.  We may still be inside AppKit's menu
 * tracking loop, so do not run Prolog here: hand the invocation to the
 * SDL event queue and let ws_dispatch() execute it.
 */

void
pce_menu_invoke(void *mb, void **popups, int npopups, void *item)
{ MenuItem mi = item;
  menu_invocation *inv;
  SDL_Event ev;
  int i;

  if ( !alive(mi, ClassMenuItem) || !alive(mb, ClassMenuBar) ||
       npopups <= 0 || npopups > PCE_MAX_MENU_DEPTH )
    return;

  for(i=0; i<npopups; i++)
  { if ( !alive(popups[i], ClassPopup) )
      return;
  }

  if ( !(inv=malloc(sizeof(*inv))) )
    return;

  inv->mb      = mb;
  inv->item    = mi;
  inv->npopups = npopups;
  addCodeReference(mi);			/* all released by */
  addCodeReference(inv->mb);		/* ws_menubar_event() */
  for(i=0; i<npopups; i++)
  { inv->popups[i] = popups[i];
    addCodeReference(inv->popups[i]);
  }

  SDL_zero(ev);
  ev.type = MY_EVENT_MENU;
  ev.user.data1 = inv;
  if ( !SDL_PushEvent(&ev) )
  { delCodeReference(mi);
    delCodeReference(inv->mb);
    for(i=0; i<npopups; i++)
      delCodeReference(inv->popups[i]);
    free(inv);
  }
}


void
pce_menu_quit(void)
{ SDL_Event ev;

  SDL_zero(ev);
  ev.type = MY_EVENT_MENU;
  ev.user.data1 = NULL;		/* NULL item means Quit */
  SDL_PushEvent(&ev);
}


bool
ws_menubar_event(SDL_Event *ev)
{ if ( ev->type != MY_EVENT_MENU )
    return false;

  menu_invocation *inv = ev->user.data1;

  if ( !inv )				/* Quit from the application menu */
  { hostAction(HOST_HALT);
    return true;
  }

  if ( alive(inv->item, ClassMenuItem) && alive(inv->mb, ClassMenuBar) )
  { PopupObj top = inv->popups[inv->npopups-1];
    AnswerMark mark;
    EventObj ev2;
    int i;

    pceMTLock();
    markAnswerStack(mark);

    /* The popup holding the item selects the item; every popup above it
       selects the popup below it. */
    for(i=0; i<inv->npopups; i++)
    { if ( alive(inv->popups[i], ClassPopup) )
	assign(inv->popups[i], selected_item,
	       i == 0 ? (Any)inv->item : (Any)inv->popups[i-1]);
    }

    if ( (ev2=menubar_event(inv->mb)) )
    { addCodeReference(ev2);
      withLocalVars({ assignVar(EVENT, ev2, NAME_local);
		      assign(ev2, receiver, inv->mb);
		      send(top, NAME_execute, inv->mb, EAV);
		    });
      delCodeReference(ev2);
      freeableObj(ev2);
    } else
    { send(top, NAME_execute, inv->mb, EAV);
    }

    for(i=0; i<inv->npopups; i++)
    { if ( !onFlag(inv->popups[i], F_FREED|F_FREEING) )
	assign(inv->popups[i], selected_item, NIL);
    }

    rewindAnswerStack(mark, NIL);
    pceMTUnlock();
  }

  delCodeReference(inv->item);
  delCodeReference(inv->mb);
  for(int i=0; i<inv->npopups; i++)
    delCodeReference(inv->popups[i]);
  free(inv);

  return true;
}

#endif /*__APPLE__*/
