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

#include <h/kernel.h>
#include <h/graphics.h>
#include "sdluserevent.h"

/* Icons in the system tray.  The menu of the icon is built from the
 * popup of the tray_icon.  SDL calls the callback of a menu entry from
 * its event processing.  We then push MY_EVENT_TRAY and execute the
 * popup from sdl_tray_event(), as for the native MacOS menu bar (see
 * sdlmenubar.c).
 */

#define MAX_DEPTH 8			/* nesting of sub-popups */
#define LABEL_LEN 256

typedef struct tray_entry
{ struct tray_entry *next;		/* all entries of the tray */
  TrayIcon	tray;
  MenuItem	item;
  int		npopups;		/* popup of item first, top last */
  PopupObj	popups[MAX_DEPTH];
  SDL_TrayEntry *entry;
} tray_entry;

typedef struct ws_tray
{ SDL_Tray     *tray;
  tray_entry   *entries;
} ws_tray;


static void
label_text(Any label, Any fallback, char *out, size_t len)
{ char *s = NULL;

  if ( instanceOfObject(label, ClassCharArray) )
    s = charArrayToUTF8((CharArray)label);
  else if ( instanceOfObject(fallback, ClassCharArray) )
    s = charArrayToUTF8((CharArray)fallback);

  snprintf(out, len, "%s", s && s[0] ? s : "...");
}

static bool
alive(Any obj, Class cl)
{ return ( obj && notNil(obj) &&
	   !onFlag(obj, F_FREED|F_FREEING) &&
	   instanceOfObject(obj, cl) );
}


static void
free_entries(ws_tray *wt)
{ tray_entry *te, *next;

  for(te = wt->entries; te; te = next)
  { next = te->next;
    free(te);
  }
  wt->entries = NULL;
}


static void SDLCALL
entry_clicked(void *userdata, SDL_TrayEntry *entry)
{ tray_entry *te = userdata;
  tray_entry *inv = malloc(sizeof(*inv));	/* survives a rebuild */
  SDL_Event ev;

  if ( !inv )
    return;
  *inv = *te;
  addCodeReference(inv->tray);
  addCodeReference(inv->item);
  for(int i=0; i<inv->npopups; i++)
    addCodeReference(inv->popups[i]);

  SDL_zero(ev);
  ev.type = MY_EVENT_TRAY;
  ev.user.data1 = inv;
  SDL_PushEvent(&ev);
}


/* Add the items of `p` to `menu`.  `path` holds the popups above `p`,
 * top first.
 */

static void
build_menu(ws_tray *wt, TrayIcon t, SDL_TrayMenu *menu, PopupObj p,
	   PopupObj *path, int depth)
{ Cell cell;

  for_cell(cell, p->members)
  { MenuItem mi = cell->value;
    char label[LABEL_LEN];
    SDL_TrayEntryFlags flags;

    label_text(mi->label, mi->value, label, sizeof(label));
    if ( notNil(mi->popup) && depth+1 < MAX_DEPTH )
    { flags = SDL_TRAYENTRY_SUBMENU;
      if ( mi->active == OFF )
	flags |= SDL_TRAYENTRY_DISABLED;

      SDL_TrayEntry *e = SDL_InsertTrayEntryAt(menu, -1, label, flags);
      SDL_TrayMenu *sub = e ? SDL_CreateTraySubmenu(e) : NULL;

      if ( sub )
      { path[depth] = p;
	build_menu(wt, t, sub, mi->popup, path, depth+1);
      }
    } else
    { if ( p->multiple_selection == ON )
      { flags = SDL_TRAYENTRY_CHECKBOX;
	if ( mi->selected == ON )
	  flags |= SDL_TRAYENTRY_CHECKED;
      } else
	flags = SDL_TRAYENTRY_BUTTON;
      if ( mi->active == OFF )
	flags |= SDL_TRAYENTRY_DISABLED;

      SDL_TrayEntry *e = SDL_InsertTrayEntryAt(menu, -1, label, flags);
      tray_entry *te;

      if ( e && (te = calloc(1, sizeof(*te))) )
      { te->tray  = t;
	te->item  = mi;
	te->entry = e;
	te->popups[te->npopups++] = p;
	for(int i=depth-1; i>=0; i--)
	  te->popups[te->npopups++] = path[i];
	te->next = wt->entries;
	wt->entries = te;
	SDL_SetTrayEntryCallback(e, entry_clicked, te);
      }
    }

    if ( mi->end_group == ON && notNil(cell->next) )
      SDL_InsertTrayEntryAt(menu, -1, NULL, 0); /* separator */
  }
}


static SDL_Tray *
create_sdl_tray(TrayIcon t)
{ SDL_Surface *icon = pceImage2SDL_Surface(t->image);
  char *tooltip = notNil(t->tooltip) ? charArrayToUTF8(t->tooltip) : NULL;
  SDL_Tray *tray = SDL_CreateTray(icon, tooltip);

  if ( icon )
    SDL_DestroySurface(icon);

  return tray;
}


static status
build_tray_menu(ws_tray *wt, TrayIcon t)
{ if ( notNil(t->popup) )
  { PopupObj path[MAX_DEPTH];
    SDL_TrayMenu *menu = SDL_CreateTrayMenu(wt->tray);

    if ( !menu )
      return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));
    build_menu(wt, t, menu, t->popup, path, 0);
  }

  succeed;
}


/* Rebuild the menu.  Removing the entries of a menu with a submenu
 * crashes the GTK implementation of SDL 3.4, so we create a new tray.
 */

status
ws_tray_menu(TrayIcon t)
{ ws_tray *wt = t->ws_ref;
  SDL_Tray *tray;

  if ( !wt )
    fail;
  if ( !(tray = create_sdl_tray(t)) )
    return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));

  SDL_DestroyTray(wt->tray);
  free_entries(wt);
  wt->tray = tray;

  return build_tray_menu(wt, t);
}


status
ws_create_tray(TrayIcon t)
{ SDL_Tray *tray = create_sdl_tray(t);

  if ( !tray )
    return errorPce(t, NAME_cannotCreate, CtoName(SDL_GetError()));

  ws_tray *wt = calloc(1, sizeof(*wt));
  if ( !wt )
  { SDL_DestroyTray(tray);
    fail;
  }
  wt->tray = tray;
  t->ws_ref = wt;

  return build_tray_menu(wt, t);
}


void
ws_destroy_tray(TrayIcon t)
{ ws_tray *wt = t->ws_ref;

  if ( wt )
  { SDL_DestroyTray(wt->tray);
    free_entries(wt);
    free(wt);
    t->ws_ref = NULL;
  }
}


status
ws_tray_image(TrayIcon t)
{ ws_tray *wt = t->ws_ref;
  SDL_Surface *icon;

  if ( !wt || !(icon = pceImage2SDL_Surface(t->image)) )
    fail;
  SDL_SetTrayIcon(wt->tray, icon);
  SDL_DestroySurface(icon);

  succeed;
}


status
ws_tray_tooltip(TrayIcon t)
{ ws_tray *wt = t->ws_ref;

  if ( !wt )
    fail;
  SDL_SetTrayTooltip(wt->tray,
		     notNil(t->tooltip) ? charArrayToUTF8(t->tooltip) : NULL);

  succeed;
}


status
ws_tray_test_click(TrayIcon t, CharArray label)
{ ws_tray *wt = t->ws_ref;
  char *l = charArrayToUTF8(label);

  if ( !wt )
    fail;
  for(tray_entry *te = wt->entries; te; te = te->next)
  { const char *el = SDL_GetTrayEntryLabel(te->entry);

    if ( el && streq(el, l) )
    { SDL_ClickTrayEntry(te->entry);
      succeed;
    }
  }

  fail;
}


/* Execute a clicked entry.  The popup holding the item selects the item
 * and every popup above it the popup below it, so ->execute of the top
 * popup runs the right message.
 */

bool
sdl_tray_event(SDL_Event *event)
{ if ( event->type != MY_EVENT_TRAY )
    return false;

  tray_entry *inv = event->user.data1;

  if ( alive(inv->item, ClassMenuItem) && alive(inv->tray, ClassTrayIcon) )
  { PopupObj top = inv->popups[inv->npopups-1];
    AnswerMark mark;

    pceMTLock();
    markAnswerStack(mark);
    for(int i=0; i<inv->npopups; i++)
    { if ( alive(inv->popups[i], ClassPopup) )
	assign(inv->popups[i], selected_item,
	       i == 0 ? (Any)inv->item : (Any)inv->popups[i-1]);
    }
    if ( alive(top, ClassPopup) )
      send(top, NAME_execute, inv->tray, EAV);
    for(int i=0; i<inv->npopups; i++)
    { if ( alive(inv->popups[i], ClassPopup) )
	assign(inv->popups[i], selected_item, NIL);
    }
    rewindAnswerStack(mark, NIL);
    pceMTUnlock();
  }

  delCodeReference(inv->tray);
  delCodeReference(inv->item);
  for(int i=0; i<inv->npopups; i++)
    delCodeReference(inv->popups[i]);
  free(inv);

  return true;
}
