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

#define SWIPL_WINDOWS_NATIVE_ACCESS 1 /* get Swinhandle() */
#include <h/kernel.h>
#include <h/graphics.h>
#include <h/dialog.h>			/* acceleratorCuesFrame() */
#include <math.h>
#include "../../swipl/pcecall.h"
#include "sdlevent.h"
#include "sdlinput.h"
#include "sdlframe.h"
#include "sdltimer.h"
#include "sdlstream.h"
#ifdef __APPLE__
#include "sdlnsmenu.h"
#endif
#ifdef HAVE_POLL
#include <poll.h>
#endif
#ifndef __WINDOWS__
#include <termios.h>
#endif

#define InitAreaA	int ax = valInt(a->x), ay = valInt(a->y),	\
			    aw = valInt(a->w), ah = valInt(a->h)


static bool
xy_in_area(Area a, int x, int y)
{ InitAreaA;

  NormaliseArea(ax, ay, aw, ah);
  return ( x >= ax && x <= ax+aw &&
	   y >= ay && y <= ay+ah );
}

static void event_window(Any *target, float *x, float *y);

static bool
descent_to_window(Any *target, PceWindow sw, float *x, float *y)
{ if ( xy_in_area(sw->area, *x, *y) &&
       ws_created_window(sw) )
  { *x -= valNum(sw->area->x);
    *y -= valNum(sw->area->y);
    *target = sw;
    event_window(target, x, y);
    return true;
  }

  return false;
}

static void
event_window(Any *target, float *x, float *y)
{ if ( instanceOfObject(*target, ClassFrame) )
  { FrameObj fr = *target;
    Cell cell;

    for_cell(cell, fr->members)
    { if ( descent_to_window(target, cell->value, x, y) )
	return;
    }
  } else if ( instanceOfObject(*target, ClassWindowDecorator) )
  { WindowDecorator dm = *target;
    descent_to_window(target, dm->window, x, y);
  } else
  { assert(instanceOfObject(*target, ClassWindow));
    PceWindow sw = *target;
    if ( notNil(sw->subwindows) && !emptyChain(sw->subwindows) )
    { Cell cell;

      /* TBD: overlapping windows? */
      for_cell(cell, sw->subwindows)
      { PceWindow sub = cell->value;
	PceWindow me = DEFAULT;
	Int wx, wy;
	get_absolute_xy_graphical((Graphical)sub, (Device *)&me, &wx, &wy);
	assert(me == sw);
	if ( *x >= valNum(wx) && *x <= valNum(wx) + valNum(sub->area->w) &&
	     *y >= valNum(wy) && *y <= valNum(wy) + valNum(sub->area->h) )
	{ *x -= valNum(wx);
	  *y -= valNum(wy);
	  *target = sub;
	  event_window(target, x, y);
	  break;
	}
      }
    }
  }
}

static Name
button_to_name(bool press, Uint8 button)
{ switch( button )
  { case 1:	return press ? NAME_msLeftDown     : NAME_msLeftUp;
    case 2:	return press ? NAME_msMiddleDown   : NAME_msMiddleUp;
    case 3:	return press ? NAME_msRightDown    : NAME_msRightUp;
    case 4:	return press ? NAME_msButton4Down  : NAME_msButton4Up;
    case 5:	return press ? NAME_msButton5Down  : NAME_msButton5Up;
  }

  fail;
}

#define ShiftMask   (SDL_KMOD_LSHIFT|SDL_KMOD_RSHIFT)
#define ControlMask (SDL_KMOD_LCTRL|SDL_KMOD_RCTRL)
#define MetaMask    (SDL_KMOD_LALT|SDL_KMOD_RALT)

static Int
state_to_buttons(SDL_MouseButtonFlags flags, SDL_Keymod mod)
{ int r = 0;

  if ( flags & SDL_BUTTON_LMASK )  r |= BUTTON_ms_left;
  if ( flags & SDL_BUTTON_MMASK )  r |= BUTTON_ms_middle;
  if ( flags & SDL_BUTTON_RMASK )  r |= BUTTON_ms_right;
  if ( flags & SDL_BUTTON_X1MASK ) r |= BUTTON_ms_button4;
  if ( flags & SDL_BUTTON_X2MASK ) r |= BUTTON_ms_button5;
  if ( mod & ShiftMask )	   r |= BUTTON_shift;
  if ( mod & ControlMask )	   r |= BUTTON_control;
  if ( mod & MetaMask )            r |= BUTTON_meta;
  if ( mod & SDL_KMOD_GUI )        r |= BUTTON_gui;

  return toInt(r);
}

/* Whether SDL reports text for the keys typed in the window.  If not,
 * as in a window without a focused text_item or editor, SDL sends no
 * SDL_EVENT_TEXT_INPUT and the key-down itself is all there is.
 */

static bool
text_input_active(SDL_WindowID wid)
{ SDL_Window *w = SDL_GetWindowFromID(wid);

  return w && SDL_TextInputActive(w);
}

static bool
is_printable_key(SDL_Keycode key)
{ return key >= ' ' && key != DEL && !(key & SDLK_SCANCODE_MASK);
}

static Any
keycode_to_name(SDL_Event *event)
{ if ( event->key.key >= 32 && event->key.key < DEL )
  { if ( event->key.mod & ControlMask )
    { /* The C0 controls: Control() strips all but the low five bits,
       * which turns `@' into NUL, the letters into 1..26 and the five
       * keys after `Z' into 27..31, giving us ^[, ^\, ^] and friends.
       * Without these, Ctrl+\ reached the client as a backslash and a
       * program on a terminal could never be sent SIGQUIT.
       */
      SDL_Keycode key = event->key.key;

      /* SDL reports the unshifted key, but three of the C0 controls
       * live on shifted characters: on a US keyboard ^^ is Shift-6 and
       * ^_ is Shift-`-'.  Taking the key as it comes sent a bare `6' or
       * `-', so Control-Shift-`-' never reached the client as ^_, which
       * is what libedit binds undo to.  Ask the keymap what this key
       * produces with the modifiers actually held.
       */
      if ( (key < '@' || key >= DEL) && (event->key.mod & SDL_KMOD_SHIFT) )
      { SDL_Keycode shifted = SDL_GetKeyFromScancode(event->key.scancode,
						     event->key.mod, false);

	if ( shifted >= '@' && shifted < DEL )
	  key = shifted;
      }

      if ( key >= '@' && key < DEL )
	return toInt(Control(key));
      else
	return toInt(key);
    }
    /* Meta is not part of the id: the event's buttons carry it, as
     * they carry the other modifiers.
     */
    if ( event->key.mod & (SDL_KMOD_GUI|MetaMask) )
      return toInt(event->key.key);
  }

  /* Without text input, a printable key is the character it types,
   * as the keymap says with the modifiers held: Shift and AltGr give
   * the symbol on the key.  Dead keys and input methods need text
   * input, so they do not compose here.
   */
  if ( is_printable_key(event->key.key) &&
       !(event->key.mod & (ControlMask|SDL_KMOD_GUI)) &&
       !text_input_active(event->key.windowID) )
  { SDL_Keycode key = SDL_GetKeyFromScancode(event->key.scancode,
					     event->key.mod, false);

    if ( is_printable_key(key) )
      return toInt(key);
  }

  switch(event->key.key)
  { case SDLK_RETURN:    return NAME_RET;
    case SDLK_ESCAPE:    return NAME_ESC;
    case SDLK_TAB:       return NAME_TAB;
    case SDLK_BACKSPACE: return NAME_BS;
    case SDLK_DELETE:    return NAME_DEL;
    case SDLK_PAUSE:	 return NAME_pause;
    case SDLK_HOME:	 return NAME_cursorHome;
    case SDLK_PAGEUP:	 return NAME_pageUp;
    case SDLK_END:	 return NAME_end;
    case SDLK_PAGEDOWN:	 return NAME_pageDown;
    case SDLK_RIGHT:	 return NAME_cursorRight;
    case SDLK_LEFT:	 return NAME_cursorLeft;
    case SDLK_DOWN:	 return NAME_cursorDown;
    case SDLK_UP:	 return NAME_cursorUp;
    case SDLK_F1:	 return NAME_f1;
    case SDLK_F2:	 return NAME_f2;
    case SDLK_F3:	 return NAME_f3;
    case SDLK_F4:	 return NAME_f4;
    case SDLK_F5:	 return NAME_f5;
    case SDLK_F6:	 return NAME_f6;
    case SDLK_F7:	 return NAME_f7;
    case SDLK_F8:	 return NAME_f8;
    case SDLK_F9:	 return NAME_f9;
    case SDLK_F10:	 return NAME_f10;
    case SDLK_F11:	 return NAME_f11;
    case SDLK_F12:	 return NAME_f12;

      /* F13..F24 are on a few full-size keyboards and on the Mac,
	 where the row above the number row runs that far.  X11 and
	 the terminfo entries of this world know them, so a client
	 that binds them can be given them (see typedTerminalImage()
	 in src/txt/terminal.c).
       */
    case SDLK_F13:	 return NAME_f13;
    case SDLK_F14:	 return NAME_f14;
    case SDLK_F15:	 return NAME_f15;
    case SDLK_F16:	 return NAME_f16;
    case SDLK_F17:	 return NAME_f17;
    case SDLK_F18:	 return NAME_f18;
    case SDLK_F19:	 return NAME_f19;
    case SDLK_F20:	 return NAME_f20;
    case SDLK_F21:	 return NAME_f21;
    case SDLK_F22:	 return NAME_f22;
    case SDLK_F23:	 return NAME_f23;
    case SDLK_F24:	 return NAME_f24;

      /* The named keys: the editing key that is not Delete, and the
	 keys the VT220 and the Sun and HP keyboards after it put
	 beside the alphabet.  xpce has had event ids for these all
	 along -- see initial_tree[] in src/evt/event.c -- but nothing
	 produced them, so <-key of an editor or a terminal could name
	 a key the window would never see.
       */
    case SDLK_INSERT:	 return NAME_insert;
    case SDLK_PRINTSCREEN: return NAME_print;
    case SDLK_APPLICATION:		/* the key beside the right Ctrl */
    case SDLK_MENU:	 return NAME_menu;
    case SDLK_HELP:	 return NAME_help;
    case SDLK_FIND:	 return NAME_find;
    case SDLK_SELECT:	 return NAME_select;
    case SDLK_EXECUTE:	 return NAME_execute;
    case SDLK_UNDO:	 return NAME_undo;
    case SDLK_AGAIN:	 return NAME_redo;
    case SDLK_CUT:	 return NAME_cut;
    case SDLK_COPY:	 return NAME_copy;
    case SDLK_PASTE:	 return NAME_paste;
    case SDLK_STOP:			/* both are Sun's L1/Cancel */
    case SDLK_CANCEL:	 return NAME_cancel;

      /* The keypad sends SDLK_KP_* events, with or without
	 numlock.   Here we assign the functions.  Next, we
	 use the text input distinction to see whether the
	 keypad sends a number or a command.

	 But, what is the correct mapping from keypad numbers
	 to functions?   This works on a default US keyboard.
       */
    case SDLK_KP_8:	 return NAME_cursorUp;
    case SDLK_KP_4:	 return NAME_cursorLeft;
    case SDLK_KP_6:	 return NAME_cursorRight;
    case SDLK_KP_2:	 return NAME_cursorDown;
    case SDLK_KP_7:	 return NAME_cursorHome;
    case SDLK_KP_1:	 return NAME_end;
    case SDLK_KP_9:	 return NAME_pageUp;
    case SDLK_KP_3:	 return NAME_pageDown;
    case SDLK_KP_0:	 return NAME_insert;
    case SDLK_KP_5:	 return NAME_begin;
    case SDLK_KP_PERIOD: return NAME_DEL;
    case SDLK_KP_ENTER:	 return NAME_RET;
  }

  fail;
}

/**
 * @see https://wiki.libsdl.org/SDL3/SDL_Event
 */

static SDL_Keymod lastmod = SDL_KMOD_NONE;

/* The Alt key that shows the accelerators of dialog items while it is
 * held.  Elsewhere, the right Alt key is AltGr, which types characters.
 * See acceleratorCuesFrame().
 */

static bool
is_cue_alt_key(SDL_Keycode key)
{
#ifdef __APPLE__
  return key == SDLK_LALT || key == SDLK_RALT;
#else
  return key == SDLK_LALT;
#endif
}
static Any grabbing_window = NIL;
static Any drop_tracking_window = NIL;	/* Window of the last drop_position */
static float drop_tracking_x, drop_tracking_y; /* its position there */
static bool drop_tracking_done = false;	/* drop_complete was delivered */
static SDL_Event lost_up_event;		/* see lost_mouse_up() */
static SDL_DisplayID last_display_id = 0;
static Uint32 keyboard_timer = 0;
static SDL_Event keydown_event = {0};
static Uint64 keyinput_time = 0;
static Uint64 menubar_key_time = 0;	/* key-down taken by the MacOS menu */

/* Milliseconds we hold a key-down while waiting to see whether SDL
 * turns it into text input, and equally the window in which a text
 * input event is taken to belong to the key-down just before it.
 */

#define KEYBOARD_DELAY 10
#define ms_to_ns(ms) ((Uint64)(ms)*1000000)

/* The keys that are down, with the event<-key of their key-down, so
 * the release can be reported as key_up with the same key, whatever
 * modifiers changed in between.  Only keys that produced an event are
 * here.
 */

#define MAX_KEYS_DOWN 32

typedef struct
{ SDL_Scancode	scancode;
  Name		key;
} key_down;

static key_down keys_down[MAX_KEYS_DOWN];
static int keys_down_count = 0;

/* With text input, the key-down of a printable key gives no event: the
 * text input event that follows it does.  Keep the key-down, so that
 * text can be paired with its key.
 */

static SDL_KeyboardEvent text_keydown;
static bool text_keydown_valid = false;

static void
remember_key_down(SDL_Scancode scancode, Name key)
{ for(int i=0; i<keys_down_count; i++)
  { if ( keys_down[i].scancode == scancode )
    { keys_down[i].key = key;		/* auto-repeat, maybe other mods */
      return;
    }
  }
  if ( keys_down_count == MAX_KEYS_DOWN ) /* lost releases: drop oldest */
  { memmove(&keys_down[0], &keys_down[1],
	    (MAX_KEYS_DOWN-1)*sizeof(keys_down[0]));
    keys_down_count--;
  }
  keys_down[keys_down_count].scancode = scancode;
  keys_down[keys_down_count].key      = key;
  keys_down_count++;
}

static Name
forget_key_down(SDL_Scancode scancode)
{ for(int i=0; i<keys_down_count; i++)
  { if ( keys_down[i].scancode == scancode )
    { Name key = keys_down[i].key;

      keys_down[i] = keys_down[--keys_down_count];
      return key;
    }
  }

  return NULL;
}

/* The window system state of a pointer device: the implicit grab
 * from a button-down to the matching up and the window that holds the
 * pointer.  Keyboard and drop events use the state of the last pointer
 * device.
 */

typedef struct sdl_pointer
{ InputDevice	device;			/* the device */
  Any		tracking_window;	/* Window or Frame of the down */
  Uint8		tracking_button;	/* button of tracking_window */
  SDL_WindowID	tracking_wid;		/* SDL window of the down */
  Uint8		down_button;		/* button of tracking_wid */
  Any		pointer_window;		/* Window holding the pointer */
  float		wheel_dx;		/* scroll since last wheel event */
  float		wheel_dy;
  float		last_x;			/* position of the last event */
  float		last_y;
  SDL_WindowID	last_wid;		/* window of last_x, last_y */
					/* Pens */
  SDL_PenInputFlags pen_state;		/* last reported state */
  float		pen_axes[SDL_PEN_AXIS_COUNT]; /* last value of each axis */
  float		pen_x;			/* last position */
  float		pen_y;
  SDL_WindowID	pen_wid;		/* window of pen_x, pen_y */
					/* Touch screens */
  int		fingers;		/* fingers on the screen */
  bool		has_primary;		/* primary finger is down */
  SDL_FingerID	primary;		/* finger for ms_left_* */
} sdl_pointer;

/* Records are allocated one by one, so a record does not move when
 * a handler of an event we are processing adds a device.
 */

static sdl_pointer **sdl_pointers = NULL;
static int sdl_pointer_count = 0;
static int sdl_pointer_allocated = 0;

static sdl_pointer *
sdl_pointer_state(InputDevice d)
{ if ( !isPointerDevice(d) )
    d = lastPointerDevice();

  for(int i=0; i<sdl_pointer_count; i++)
  { if ( sdl_pointers[i]->device == d )
      return sdl_pointers[i];
  }

  if ( sdl_pointer_count == sdl_pointer_allocated )
  { int n = sdl_pointer_allocated ? sdl_pointer_allocated*2 : 4;
    sdl_pointers = pceRealloc(sdl_pointers, n*sizeof(*sdl_pointers));
    sdl_pointer_allocated = n;
  }

  sdl_pointer *sp = pceMalloc(sizeof(*sp));
  sdl_pointers[sdl_pointer_count++] = sp;
  memset(sp, 0, sizeof(*sp));
  sp->device          = d;
  sp->tracking_window = NIL;
  sp->pointer_window  = NIL;

  return sp;
}

static void	set_pointer_window(sdl_pointer *sp, Any window);

/* Called if `d` was removed */

void
ws_forget_pointer(InputDevice d)
{ for(int i=0; i<sdl_pointer_count; i++)
  { sdl_pointer *sp = sdl_pointers[i];

    if ( sp->device == d )
    { set_pointer_window(sp, NIL);
      if ( notNil(sp->tracking_window) )
	delCodeReference(sp->tracking_window);
      sdl_pointers[i] = sdl_pointers[--sdl_pointer_count];
      pceFree(sp);
      return;
    }
  }
}

static Uint32
tm_keyboard_timeout(void *udata, SDL_TimerID id, Uint32 interval)
{ keyboard_timer = 0;

  if ( keyinput_time < keydown_event.key.timestamp )
  { SDL_Event ev;

    SDL_zero(ev);
    ev.type = MY_EVENT_KEYDOWN_TIMEOUT;
    SDL_PushEvent(&ev);
    DEBUG(NAME_keyboard,
	  Cprintf("%"PRIu64" < %"PRIu64": Pushed delayed keyboard event\n",
		  keyinput_time, keydown_event.key.timestamp));
  }

  return 0;
}

/** True if the character should be passed as a printable
 *  character rather than a keydown event with a modifier.
 */

static StringObj macOSOptionCharacters = NULL;

static bool
isOptionPrintCharacter(int code)
{ if ( code >= 32 && code <= 126 )
    return true;

  if ( !macOSOptionCharacters &&
       !isNil(macOSOptionCharacters) )
  { macOSOptionCharacters = getClassVariableValueClass(
      ClassEvent,
      NAME_macosOptionCharacters);
    if ( macOSOptionCharacters )
    { DEBUG(NAME_keyboard,
	    Cprintf("macOSOptionCharacters = %s\n",
		    pp(macOSOptionCharacters)));
    } else
    { macOSOptionCharacters = (StringObj)NIL;
    }
  }

  if ( notNil(macOSOptionCharacters) )
    return str_index(&macOSOptionCharacters->data, code) != -1;

  return false;
}


PceWindow
ws_grabbing_window(void)
{ if ( notNil(grabbing_window) )
    return grabbing_window;

  return NULL;
}


/* The window the pointer is in, if any.  SDL has one cursor for the
 * whole application, so this is the window that decides which shape it
 * must have.
 */

PceWindow
ws_pointer_window(void)
{ Any pointer_window = sdl_pointer_state(DEVICE_mouse)->pointer_window;

  if ( notNil(pointer_window) &&
       instanceOfObject(pointer_window, ClassWindow) &&
       !onFlag(pointer_window, F_FREED|F_FREEING) )
    return pointer_window;

  return NULL;
}


/* Hold a code reference while we grab, as we do for the mouse tracking
   window.  A menu item that destroys its own window -- Epilog's "Halt
   Prolog" is one -- is executed while its menu bar still holds the
   grab, and without the reference the object is unallocated under us
   and the next event reads freed memory.
*/

void
ev_event_grab_window(Any window)
{ if ( window == grabbing_window )
    return;

  if ( notNil(grabbing_window) )
    delCodeReference(grabbing_window);
  grabbing_window = window;
  if ( notNil(grabbing_window) )
    addCodeReference(grabbing_window);
}

/* A drop sequence belongs to the window the user dragged over.  SDL
   gives drop_file, drop_text and drop_complete no position of their
   own: they carry that of the last drop_position, which every
   drop_complete resets to (0,0).  Routing them by position is thus
   fragile: the files may go to whatever is at the top-left of the
   frame, and the window showing the drop hint never hears the drop
   has completed.  Send them to the window of the last drop_position.

   We keep this window after drop_complete.  Using X11, SDL reports
   XdndLeave, which the source sends before it drops, as drop_complete.
   The data follows as a new sequence drop_begin, drop_file ...,
   drop_complete without a drop_position.  As SDL has reset the
   position by then, we also use the position of the last
   drop_position.
*/

static void
set_drop_tracking_window(Any window)
{ if ( window == drop_tracking_window )
    return;

  if ( notNil(drop_tracking_window) )
    delCodeReference(drop_tracking_window);
  drop_tracking_window = window;
  if ( notNil(drop_tracking_window) )
    addCodeReference(drop_tracking_window);
}

void
ws_event_destroyed_target(Any window)
{ if ( window == drop_tracking_window )
    set_drop_tracking_window(NIL);
  if ( window == grabbing_window )
    ev_event_grab_window(NIL);
  for(int i=0; i<sdl_pointer_count; i++)
  { sdl_pointer *sp = sdl_pointers[i];

    if ( window == sp->tracking_window )
    { delCodeReference(sp->tracking_window);
      sp->tracking_window = NIL;
    }
    if ( window == sp->pointer_window )
      set_pointer_window(sp, NIL);
  }
}


		 /*******************************
		 *	   ENTER/LEAVE		*
		 *******************************/

/* SDL only reports enter/leave for its own (native) windows, which are
   our frames.  We therefore synthesise `area_enter' and `area_exit'
   for the xpce window that receives the mouse events.  These maintain
   PceWindow <->has_pointer as well as the <->pointed chain of the
   devices in the window, which is what makes e.g. scroll bars react to
   the pointer entering and leaving them.
*/

static void
post_area_event(Any window, Name id, float x, float y, Any buttons,
		Any device)
{ if ( onFlag(window, F_FREED|F_FREEING) )
    return;

  ServiceMode(is_service_window(window),
	      { AnswerMark mark;
		EventObj ev;

		markAnswerStack(mark);	/* before creating the event */
		if ( (ev=answerObject(ClassEvent, id, window,
				      toNum(x), toNum(y), buttons,
				      DEFAULT, device, EAV)) )
		{ addCodeReference(ev);
		  postNamedEvent(ev, window, DEFAULT, NAME_postEvent);
		  delCodeReference(ev);
		}
		rewindAnswerStack(mark, NIL);
	      });
}


/* Note that `fx' and `fy' are relative to the frame.  We translate
   them to each of the two windows involved.
*/

static void
set_pointer_window(sdl_pointer *sp, Any window)
{ if ( notNil(sp->pointer_window) )
    delCodeReference(sp->pointer_window);
  sp->pointer_window = window;
  if ( notNil(window) )
    addCodeReference(window);		/* keep it around while we
					   hold a pointer to it */
}


static void
update_pointer_window(sdl_pointer *sp, Any window, FrameObj frame,
		      float fx, float fy, Any buttons)
{ Any old = sp->pointer_window;

  if ( !instanceOfObject(window, ClassWindow) )
    window = NIL;			/* on the frame, but not in a
					   window */
  if ( old == window )
    return;

  addCodeReference(old);		/* survives set_pointer_window() */
  set_pointer_window(sp, window);	/* update before we post: the
					   handlers may generate events */

  if ( notNil(old) && !onFlag(old, F_FREED|F_FREEING) )
  { float ox=0, oy=0;

    ws_window_frame_position(old, frame, &ox, &oy);
    post_area_event(old, NAME_areaExit, fx-ox, fy-oy, buttons, sp->device);
  }
  delCodeReference(old);

  if ( notNil(window) )
  { float ox=0, oy=0;

    ws_window_frame_position(window, frame, &ox, &oy);
    post_area_event(window, NAME_areaEnter, fx-ox, fy-oy, buttons,
		    sp->device);
  }
}


/* A drag moved from the window of the last drop_position to `window'.
   SDL only reports leaving the frame (as drop_complete), so tell the old
   window the drag left by sending it a drop_complete without files.
   This removes e.g. the drop hint of pce_drop_target.pl.  The window
   passes the event to the graphical below it, so we must use the last
   position inside the old window rather than the current one.  `x' and
   `y' are relative to `window'.
*/

static void
update_drop_tracking_window(Any window, float x, float y)
{ Any old = drop_tracking_window;
  float old_x = drop_tracking_x, old_y = drop_tracking_y;
  bool done = drop_tracking_done;	/* old already had drop_complete */

  drop_tracking_x = x;
  drop_tracking_y = y;
  drop_tracking_done = false;
  if ( old == window )
    return;

  addCodeReference(old);		/* survives set_drop_tracking_window() */
  set_drop_tracking_window(window);	/* update before we post */

  if ( notNil(old) && !done && !onFlag(old, F_FREED|F_FREEING) )
    post_area_event(old, NAME_dropComplete, old_x, old_y, ZERO, DEFAULT);
  delCodeReference(old);
}


/* Called when the pointer leaves a frame.  SDL does report this one. */

void
ws_pointer_left_frame(FrameObj fr)
{ sdl_pointer *sp = sdl_pointer_state(DEVICE_mouse);

  if ( notNil(sp->pointer_window) &&
       getFrameWindow(sp->pointer_window, OFF) == fr )
    update_pointer_window(sp, NIL, fr, 0.0, 0.0, ZERO);
}

DisplayObj
ws_last_display_from_event(void)
{ return dsp_id_to_display(last_display_id);
}

/* A mouse button was pressed in the SDL window `tracking_wid', but the
   drag or up event is reported for another SDL window `*wid'.  The
   xpce window that got the down must see the remainder of the
   interaction: a gesture that misses its up-event remains active.
   Translate the position using the screen positions of the two SDL
   windows (unknown on Wayland, where this gives a wrong position, but
   still delivers the event to the right window).  This happens on X11
   with SDL 3.2, which reports the up-event of the click that opens a
   popup for the popup window when it takes the focus.  A menu_bar must
   see that up-event at the position of the down to keep the popup open.
*/

static void
redirect_to_tracking_frame(SDL_WindowID *wid, SDL_WindowID tracking_wid,
			   float *fx, float *fy)
{ SDL_Window *from = *wid ? SDL_GetWindowFromID(*wid) : NULL;
  SDL_Window *to = SDL_GetWindowFromID(tracking_wid);
  int x0, y0, x1, y1;

  if ( !to )
    return;
  if ( from &&
       sdl_window_screen_position(from, &x0, &y0) &&
       sdl_window_screen_position(to, &x1, &y1) )
  { *fx += (float)(x0-x1);
    *fy += (float)(y0-y1);
  }

  DEBUG(NAME_event,
	Cprintf("Redirecting mouse event from SDL window %d to %d\n",
		*wid, tracking_wid));
  *wid = tracking_wid;
}

/* Complete a wheel event for `window'.  `*rotation' holds the whole
   notches SDL reports, or is NULL.  If `window' has <-smooth_scroll
   @on, every SDL wheel event is delivered with the rotation and detail
   of this event alone.  Otherwise we only deliver whole notches, and
   the detail holds the precise scrolling since the last delivered
   wheel event.  False if there is no event to deliver.
*/

static bool
wheel_event(SDL_Event *event, sdl_pointer *sp, Any window,
	    Any *name, Int *rotation, Any *detail)
{ BoolObj flipped = ( event->wheel.direction == SDL_MOUSEWHEEL_FLIPPED
		      ? ON : OFF );

  if ( instanceOfObject(window, ClassWindow) &&
       ((PceWindow)window)->smooth_scroll == ON )
  { float px = event->wheel.x, py = event->wheel.y;

    if ( py != 0.0f && fabsf(py) >= fabsf(px) )
    { *name     = NAME_wheel;
      *rotation = toNum(py*15.0);
    } else if ( px != 0.0f )
    { *name     = NAME_horizontalWheel;
      *rotation = toNum(px*15.0);
    } else
      return false;

    *detail = answerObject(ClassWheelData, toNum(px), toNum(py), flipped, EAV);
  } else
  { if ( !*rotation )
      return false;			/* not a whole notch */
    *detail = answerObject(ClassWheelData,
			   toNum(sp->wheel_dx), toNum(sp->wheel_dy), flipped,
			   EAV);
  }
  sp->wheel_dx = sp->wheel_dy = 0.0f;

  return true;
}

		 /*******************************
		 *	       PENS		*
		 *******************************/

/* The tip of a pen is the left button.  The first and second button
 * on its barrel are the right and middle button, as with most drawing
 * programs.
 */

static int
pen_button_to_mouse_button(Uint8 button)
{ switch(button)
  { case 1: return SDL_BUTTON_RIGHT;
    case 2: return SDL_BUTTON_MIDDLE;
    case 3: return SDL_BUTTON_X1;
    case 4: return SDL_BUTTON_X2;
    default: return 0;
  }
}

static SDL_MouseButtonFlags
pen_mouse_flags(SDL_PenInputFlags state)
{ SDL_MouseButtonFlags flags = 0;

  if ( state & SDL_PEN_INPUT_DOWN )     flags |= SDL_BUTTON_LMASK;
  if ( state & SDL_PEN_INPUT_BUTTON_1 ) flags |= SDL_BUTTON_RMASK;
  if ( state & SDL_PEN_INPUT_BUTTON_2 ) flags |= SDL_BUTTON_MMASK;
  if ( state & SDL_PEN_INPUT_BUTTON_3 ) flags |= SDL_BUTTON_X1MASK;
  if ( state & SDL_PEN_INPUT_BUTTON_4 ) flags |= SDL_BUTTON_X2MASK;

  return flags;
}

/* Remember the position of a pen event.  The proximity events have no
 * position; they use the last.
 */

static void
pen_position(sdl_pointer *sp, SDL_WindowID wid, float x, float y)
{ sp->pen_wid = wid;
  sp->pen_x   = x;
  sp->pen_y   = y;
}

static Any
pen_detail(sdl_pointer *sp)
{ float *a = sp->pen_axes;
  SDL_PenInputFlags state = sp->pen_state;

  return answerObject(ClassPenState,
		      toNum(a[SDL_PEN_AXIS_PRESSURE]),
		      toNum(a[SDL_PEN_AXIS_XTILT]),
		      toNum(a[SDL_PEN_AXIS_YTILT]),
		      toNum(a[SDL_PEN_AXIS_DISTANCE]),
		      toNum(a[SDL_PEN_AXIS_ROTATION]),
		      toNum(a[SDL_PEN_AXIS_SLIDER]),
		      toNum(a[SDL_PEN_AXIS_TANGENTIAL_PRESSURE]),
		      (state & SDL_PEN_INPUT_ERASER_TIP) ? ON : OFF,
		      toInt((state >> 1) & 0x1f),	/* BUTTON_1 .. BUTTON_5 */
		      EAV);
}

		 /*******************************
		 *	   TOUCH SCREENS	*
		 *******************************/

/* SDL reports finger positions normalised to the window.  Get them in
 * pixels.
 */

static void
finger_position(SDL_TouchFingerEvent *f, float *x, float *y,
		float *dx, float *dy)
{ SDL_Window *win = SDL_GetWindowFromID(f->windowID);
  int w = 0, h = 0;

  if ( win )
    SDL_GetWindowSize(win, &w, &h);
  *x  = f->x*w;
  *y  = f->y*h;
  *dx = f->dx*w;
  *dy = f->dy*h;
}

/* A motion event shows the button pressed in `sp->tracking_wid' is no
   longer held, but we never saw it going up.  This may happen if it is
   released outside our windows.  Turn the motion into the lost up-event
   such that the gesture that handled the down terminates.
*/

static SDL_Event *
lost_mouse_up(sdl_pointer *sp, SDL_Event *motion)
{ if ( !sp->tracking_wid ||
       (motion->motion.state & SDL_BUTTON_MASK(sp->down_button)) )
    return motion;

  DEBUG(NAME_event,
	Cprintf("Synthesizing lost up-event for button %d\n",
		sp->down_button));

  memset(&lost_up_event, 0, sizeof(lost_up_event));
  lost_up_event.button.type	 = SDL_EVENT_MOUSE_BUTTON_UP;
  lost_up_event.button.timestamp = motion->motion.timestamp;
  lost_up_event.button.windowID  = motion->motion.windowID;
  lost_up_event.button.which	 = motion->motion.which;
  lost_up_event.button.button	 = sp->down_button;
  lost_up_event.button.down	 = false;
  lost_up_event.button.clicks	 = 1;
  lost_up_event.button.x	 = motion->motion.x;
  lost_up_event.button.y	 = motion->motion.y;

  return &lost_up_event;
}

EventObj
CtoEvent(SDL_Event *event)
{ ASSERT_SDL_MAIN();
  unsigned int time;
  SDL_MouseButtonFlags mouse_flags = 0;
  float fx, fy;
  Any name = NULL;
  Any detail = NIL;			/* ev->detail */
  Int rotation = NULL;			/* wheel events */
  const SDL_KeyboardEvent *keydown = NULL; /* key-down of a key event */
  Name keyup = NULL;			/* event<-key of a key_up */
  int press = 0;			/* button that went down */
  int release = 0;			/* button that went up */
  bool moved = false;			/* pointer may have moved */
  SDL_WindowID wid = 0;
  SDL_WindowID tracking_wid = 0;	/* frame of the down event */
  FrameObj frame = NIL;		/* ev->frame */
  Any window;			/* ev->window */

  if ( sdl_call_event(event) )	/* support in_pce_thread/1 */
    fail;
  if ( sdl_display_event(event) ) /* display events (add/delete/change) */
    fail;
  if ( sdl_gamepad_hotplug_event(event) ) /* gamepad added/removed */
    fail;
  if ( sdl_audio_event(event) )	/* sounds, recorders and cameras */
    fail;
  if ( sdl_camera_event(event) ) /* camera permission and removal */
    fail;
  if ( sdl_tray_event(event) )	/* tray menu entry clicked */
    fail;
  if ( sdl_frame_event(event) )	/* window events (close/open/size/...) */
    fail;
  if ( sdl_timer_event(event) )	/* Timer event */
    fail;
  if ( sdl_stream_event(event) ) /* I/O stream event */
    fail;
  if ( ws_menubar_event(event) ) /* Native (MacOS) menu selection */
    fail;
  if ( event->type == MY_EVENT_FLASH_END )
  { FrameObj fr = event->user.data1;
    if ( !onFlag(fr, F_FREED|F_FREEING) && instanceOfObject(fr, ClassFrame) )
    { WsFrame wfr = fr->ws_ref;
      if ( wfr )
      { wfr->flash_end_ms = 0;
	pceMTLock();
	ws_draw_frame(fr);
	pceMTUnlock();
      }
    }
    delCodeReference(fr);
    fail;
  }
  mouse_flags = SDL_GetMouseState(&fx, &fy);

  if ( !DEVICE_mouse )			/* creates @mouse_device */
    realiseClass(ClassInputDevice);
  Any device = DEFAULT;			/* keyboard or last pointer */
  switch ( event->type )
  { case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
    case SDL_EVENT_MOUSE_MOTION:
    case SDL_EVENT_MOUSE_WHEEL:
      /* We disabled SDL's emulation (see sdl.c), but be safe */
      if ( event->motion.which == SDL_PEN_MOUSEID ||
	   event->motion.which == SDL_TOUCH_MOUSEID )
	fail;
      device = DEVICE_mouse;
      break;
    case SDL_EVENT_GAMEPAD_BUTTON_DOWN:
    case SDL_EVENT_GAMEPAD_BUTTON_UP:
    case SDL_EVENT_GAMEPAD_AXIS_MOTION:
    { SDL_Window *focus = SDL_GetKeyboardFocus();

      if ( !focus ||			/* we are not in the foreground */
	   !sdl_gamepad_input_event(event, &device, &name, &detail, &time) )
	fail;
      wid = SDL_GetWindowID(focus);
      break;
    }
#if SDL_VERSION_ATLEAST(3, 4, 0)
    case SDL_EVENT_PINCH_BEGIN:		/* a trackpad gesture */
    case SDL_EVENT_PINCH_UPDATE:
    case SDL_EVENT_PINCH_END:
      device = DEVICE_mouse;
      break;
#endif
    case SDL_EVENT_PEN_PROXIMITY_IN:	/* all have `which` at the */
    case SDL_EVENT_PEN_PROXIMITY_OUT:	/* same offset */
    case SDL_EVENT_PEN_DOWN:
    case SDL_EVENT_PEN_UP:
    case SDL_EVENT_PEN_MOTION:
    case SDL_EVENT_PEN_BUTTON_DOWN:
    case SDL_EVENT_PEN_BUTTON_UP:
    case SDL_EVENT_PEN_AXIS:
      device = findInputDevice(NAME_pen, (intptr_t)event->pproximity.which,
			       NULL);
      if ( !device )
	fail;
      break;
    case SDL_EVENT_FINGER_DOWN:
    case SDL_EVENT_FINGER_UP:
    case SDL_EVENT_FINGER_MOTION:
    case SDL_EVENT_FINGER_CANCELED:
    { SDL_TouchID id = event->tfinger.touchID;

      if ( id == SDL_PEN_TOUCHID || id == SDL_MOUSE_TOUCHID )
	fail;				/* emulated; see sdl.c */
      device = findInputDevice(NAME_touch, (intptr_t)id,
			       SDL_GetTouchDeviceName(id));
      if ( !device )
	fail;
      break;
    }
    default:
      break;
  }
  sdl_pointer *sp = sdl_pointer_state(isDefault(device) ? lastPointerDevice()
						      : device);

  if ( event->type == SDL_EVENT_MOUSE_MOTION )
    event = lost_mouse_up(sp, event);

  switch (event->type)
  { case SDL_EVENT_MOUSE_BUTTON_DOWN:
      sp->tracking_wid = event->button.windowID;
      sp->down_button = event->button.button;
      goto mouse_cont;
    case SDL_EVENT_MOUSE_BUTTON_UP:
      /* https://wiki.libsdl.org/SDL3/SDL_MouseButtonEvent */
      if ( !event->button.windowID ) /* Seems to happen on MacOS */
	event->button.windowID = sp->tracking_wid;
      tracking_wid = sp->tracking_wid;
      sp->tracking_wid = 0;
    mouse_cont:
      if ( event->button.down )
	press = event->button.button;
      else
	release = event->button.button;
      moved = true;
      fx = event->button.x;	/* these are floats */
      fy = event->button.y;
      wid  = event->button.windowID;
      time = event->button.timestamp/1000000; // ns -> ms
      name = button_to_name(event->button.down, event->button.button);
      if ( !name )
	fail;
      break;
      /* https://wiki.libsdl.org/SDL3/SDL_MouseMotionEvent */
    case SDL_EVENT_MOUSE_MOTION:
      moved = true;
      fx   = event->motion.x;	/* these are floats */
      fy   = event->motion.y;
      wid  = event->motion.windowID;
      time = event->motion.timestamp/1000000; // ns -> ms
      mouse_flags = event->motion.state;
      if ( mouse_flags & (SDL_BUTTON_LMASK|SDL_BUTTON_MMASK|SDL_BUTTON_RMASK) )
	tracking_wid = sp->tracking_wid;

      if ( mouse_flags & SDL_BUTTON_LMASK )
	name = NAME_msLeftDrag;
      else if ( mouse_flags & SDL_BUTTON_MMASK )
	name = NAME_msMiddleDrag;
      else if ( mouse_flags & SDL_BUTTON_RMASK )
	name = NAME_msRightDrag;
      else
	name = NAME_locMove;
      break;
    case SDL_EVENT_MOUSE_WHEEL:
    { static int last_time = 0;
      wid  = event->wheel.windowID;
      time = event->wheel.timestamp/1000000;
      fx   = event->wheel.mouse_x;
      fy   = event->wheel.mouse_y;
      sp->wheel_dx += event->wheel.x;	/* see the detail below */
      sp->wheel_dy += event->wheel.y;
      int dx = 0, dy = 0;
#if SDL_VERSION_ATLEAST(3, 2, 12)
      dx = event->wheel.integer_x;
      dy = event->wheel.integer_y;
#else
      static double dxf = 0.0, dyf = 0.0;
      dxf += event->wheel.x;
      dyf += event->wheel.y;
      if ( dxf >= 1.0 || dxf <= -1.0 )
      { dx = trunc(dxf);
	dxf -= dx;
      }
      if ( dyf >= 1.0 || dyf <= -1.0 )
      { dy = trunc(dyf);
	dyf -= dy;
      }
#endif

      DEBUG(NAME_wheel,
	    Cprintf("Mouse wheel event.  fx=%.6f, fy=%.6f, ix=%d, iy=%d, "
		    "dt=%dms%s\n",
		    event->wheel.x, event->wheel.y, dx, dy, time-last_time,
		    event->wheel.direction == SDL_MOUSEWHEEL_FLIPPED
		    ? " (flipped)" : ""));
      /* An xpce event scrolls in one direction.  If a trackpad scrolls
	 diagonally, we use the dominant direction.  Whether we deliver
	 an event that is not a whole notch depends on the window.  See
	 smooth_wheel_event(). */
      if ( dy && abs(dy) >= abs(dx) )
      { name = NAME_wheel;
	rotation = toInt(dy*15);
      } else if ( dx )
      { name = NAME_horizontalWheel;
	rotation = toInt(dx*15);
      } else
	name = NAME_wheel;		/* a fraction of a notch */
      last_time = time;
      break;
    }
      /* https://wiki.libsdl.org/SDL3/CategoryPen */
    case SDL_EVENT_PEN_PROXIMITY_IN:
    case SDL_EVENT_PEN_PROXIMITY_OUT:
      wid  = event->pproximity.windowID ? event->pproximity.windowID
					: sp->pen_wid;
      fx   = sp->pen_x;
      fy   = sp->pen_y;
      time = event->pproximity.timestamp/1000000;
      sp->pen_state = event->pproximity.pen_state;
      mouse_flags = pen_mouse_flags(sp->pen_state);
      name = ( event->type == SDL_EVENT_PEN_PROXIMITY_IN
	       ? NAME_penProximityIn : NAME_penProximityOut );
      break;
    case SDL_EVENT_PEN_DOWN:
    case SDL_EVENT_PEN_UP:
      pen_position(sp, event->ptouch.windowID,
		   event->ptouch.x, event->ptouch.y);
      wid  = event->ptouch.windowID;
      fx   = event->ptouch.x;
      fy   = event->ptouch.y;
      time = event->ptouch.timestamp/1000000;
      sp->pen_state = event->ptouch.pen_state;
      if ( event->ptouch.eraser )
	sp->pen_state |= SDL_PEN_INPUT_ERASER_TIP;
      if ( event->ptouch.down )
      { sp->pen_state |= SDL_PEN_INPUT_DOWN;
	sp->tracking_wid = wid;
	press = SDL_BUTTON_LEFT;
      } else
      { sp->pen_state &= ~SDL_PEN_INPUT_DOWN;
	tracking_wid = sp->tracking_wid;
	sp->tracking_wid = 0;
	release = SDL_BUTTON_LEFT;
      }
      mouse_flags = pen_mouse_flags(sp->pen_state);
      name = button_to_name(event->ptouch.down, SDL_BUTTON_LEFT);
      moved = true;
      break;
    case SDL_EVENT_PEN_BUTTON_DOWN:
    case SDL_EVENT_PEN_BUTTON_UP:
    { int button = pen_button_to_mouse_button(event->pbutton.button);

      if ( !button )
	fail;
      pen_position(sp, event->pbutton.windowID,
		   event->pbutton.x, event->pbutton.y);
      wid  = event->pbutton.windowID;
      fx   = event->pbutton.x;
      fy   = event->pbutton.y;
      time = event->pbutton.timestamp/1000000;
      sp->pen_state = event->pbutton.pen_state;
      SDL_PenInputFlags bit = SDL_PEN_INPUT_BUTTON_1 << (event->pbutton.button-1);
      if ( event->pbutton.down )
      { sp->pen_state |= bit;
	sp->tracking_wid = wid;
	press = button;
      } else
      { sp->pen_state &= ~bit;
	tracking_wid = sp->tracking_wid;
	sp->tracking_wid = 0;
	release = button;
      }
      mouse_flags = pen_mouse_flags(sp->pen_state);
      name = button_to_name(event->pbutton.down, button);
      moved = true;
      break;
    }
    case SDL_EVENT_PEN_MOTION:
      pen_position(sp, event->pmotion.windowID,
		   event->pmotion.x, event->pmotion.y);
      wid  = event->pmotion.windowID;
      fx   = event->pmotion.x;
      fy   = event->pmotion.y;
      time = event->pmotion.timestamp/1000000;
      sp->pen_state = event->pmotion.pen_state;
      mouse_flags = pen_mouse_flags(sp->pen_state);
      if ( mouse_flags )
	tracking_wid = sp->tracking_wid;
      if ( mouse_flags & SDL_BUTTON_LMASK )
	name = NAME_msLeftDrag;
      else if ( mouse_flags & SDL_BUTTON_RMASK )
	name = NAME_msRightDrag;
      else if ( mouse_flags & SDL_BUTTON_MMASK )
	name = NAME_msMiddleDrag;
      else
	name = NAME_locMove;
      moved = true;
      break;
    case SDL_EVENT_PEN_AXIS:
      pen_position(sp, event->paxis.windowID,
		   event->paxis.x, event->paxis.y);
      wid  = event->paxis.windowID;
      fx   = event->paxis.x;
      fy   = event->paxis.y;
      time = event->paxis.timestamp/1000000;
      sp->pen_state = event->paxis.pen_state;
      mouse_flags = pen_mouse_flags(sp->pen_state);
      if ( mouse_flags )
	tracking_wid = sp->tracking_wid;
      if ( event->paxis.axis < SDL_PEN_AXIS_COUNT )
	sp->pen_axes[event->paxis.axis] = event->paxis.value;
      name = NAME_penAxis;
      break;
      /* https://wiki.libsdl.org/SDL3/SDL_TouchFingerEvent */
    case SDL_EVENT_FINGER_DOWN:
    case SDL_EVENT_FINGER_UP:
    case SDL_EVENT_FINGER_MOTION:
    case SDL_EVENT_FINGER_CANCELED:
    { SDL_TouchFingerEvent *f = &event->tfinger;
      float dx, dy;
      bool primary;

      finger_position(f, &fx, &fy, &dx, &dy);
      wid  = f->windowID;
      time = f->timestamp/1000000;

      if ( event->type == SDL_EVENT_FINGER_DOWN )
      { primary = (sp->fingers++ == 0);
	if ( primary )
	{ sp->has_primary = true;
	  sp->primary = f->fingerID;
	}
      } else
      { primary = sp->has_primary && sp->primary == f->fingerID;
      }

      if ( primary )
      { switch(event->type)
	{ case SDL_EVENT_FINGER_DOWN:
	    sp->tracking_wid = wid;
	    press = SDL_BUTTON_LEFT;
	    name = NAME_msLeftDown;
	    break;
	  case SDL_EVENT_FINGER_MOTION:
	    tracking_wid = sp->tracking_wid;
	    name = NAME_msLeftDrag;
	    break;
	  default:			/* up or canceled */
	    tracking_wid = sp->tracking_wid;
	    sp->tracking_wid = 0;
	    sp->has_primary = false;
	    release = SDL_BUTTON_LEFT;
	    name = NAME_msLeftUp;
	}
	moved = true;
      } else
      { switch(event->type)
	{ case SDL_EVENT_FINGER_DOWN:	  name = NAME_touchDown;   break;
	  case SDL_EVENT_FINGER_MOTION:	  name = NAME_touchMotion; break;
	  case SDL_EVENT_FINGER_UP:	  name = NAME_touchUp;     break;
	  default:			  name = NAME_touchCancel; break;
	}
      }
      if ( event->type == SDL_EVENT_FINGER_UP ||
	   event->type == SDL_EVENT_FINGER_CANCELED )
      { if ( sp->fingers > 0 )
	  sp->fingers--;
      }
      mouse_flags = ( sp->has_primary || press ) ? SDL_BUTTON_LMASK : 0;
      detail = answerObject(ClassTouchPoint,
			    toNum((double)f->fingerID), toNum(f->pressure),
			    toNum(dx), toNum(dy), EAV);
      break;
    }
    case SDL_EVENT_GAMEPAD_BUTTON_DOWN:	/* see the device switch */
    case SDL_EVENT_GAMEPAD_BUTTON_UP:
    case SDL_EVENT_GAMEPAD_AXIS_MOTION:
      break;
#if SDL_VERSION_ATLEAST(3, 4, 0)
      /* https://wiki.libsdl.org/SDL3/SDL_PinchFingerEvent.  This has no
	 position.  Use that of the last mouse event in this window, or
	 the position of the mouse from SDL_GetMouseState(). */
    case SDL_EVENT_PINCH_BEGIN:
    case SDL_EVENT_PINCH_UPDATE:
    case SDL_EVENT_PINCH_END:
      wid  = event->pinch.windowID;
      time = event->pinch.timestamp/1000000;
      if ( wid == sp->last_wid )	/* sp is that of the mouse */
      { fx = sp->last_x;
	fy = sp->last_y;
      }
      switch(event->type)
      { case SDL_EVENT_PINCH_BEGIN:  name = NAME_pinchBegin;  break;
	case SDL_EVENT_PINCH_UPDATE: name = NAME_pinchUpdate; break;
	default:		     name = NAME_pinchEnd;    break;
      }
      detail = answerObject(ClassPinchData, toNum(event->pinch.scale), EAV);
      break;
#endif
      /* https://wiki.libsdl.org/SDL3/SDL_KeyboardEvent */
    case SDL_EVENT_KEYMAP_CHANGED:
      DEBUG(NAME_keyboard,
	    Cprintf("Got SDL_EVENT_KEYMAP_CHANGED\n"));
      fail;
    case SDL_EVENT_KEY_UP:
      lastmod = event->key.mod;
      if ( is_cue_alt_key(event->key.key) )
	acceleratorCuesFrame(wsid_to_frame(event->key.windowID), false);
      if ( !(keyup = forget_key_down(event->key.scancode)) )
	fail;			/* no event for its key-down */
      wid  = event->key.windowID;
      time = event->key.timestamp/1000000;
      name = NAME_keyUp;
      break;
      /* https://wiki.libsdl.org/SDL3/SDL_TextInputEvent */
    case SDL_EVENT_TEXT_INPUT:
    { int codepoint;
      char const *u = event->text.text;
      u = utf8_get_char(u, &codepoint);
      if ( *u )
        Cprintf("SDL_EVENT_TEXT_INPUT: multi-char input not yet supported\n");

      DEBUG(NAME_keyboard,
	    Cprintf("SDL_EVENT_TEXT_INPUT: %d (\"%s\") at %" PRIu64 "\n",
		    codepoint, event->text.text, event->text.timestamp));

      /* A key-down the MacOS menu bar claimed is not delayed, so there
       * is no timer and the test below would let its text through.
       * MacOS composes text from Option, so an Option accelerator would
       * insert a character on top of running the menu item.  Drop the
       * text that belongs to the claimed key-down.
       */
      if ( !keyboard_timer && menubar_key_time &&
	   event->text.timestamp >= menubar_key_time &&
	   event->text.timestamp - menubar_key_time <
	   ms_to_ns(KEYBOARD_DELAY) )
      { DEBUG(NAME_keyboard,
	      Cprintf("Dropping text input for a keystroke claimed by "
		      "the native menu bar\n"));
	menubar_key_time = 0;
	fail;
      }

      if ( keyboard_timer )
      { if ( isOptionPrintCharacter(codepoint) )
	{ SDL_RemoveTimer(keyboard_timer);
	  keyboard_timer = 0;
	  keyinput_time = event->text.timestamp;
	  keydown = &keydown_event.key;	/* the text is of this key */
	} else
	{ fail;			/* process as key-down */
	}
      }

      if ( !keydown && text_keydown_valid &&
	   event->text.timestamp >= text_keydown.timestamp &&
	   event->text.timestamp - text_keydown.timestamp <
	   ms_to_ns(KEYBOARD_DELAY) )
	keydown = &text_keydown;	/* the text is of this key */
      text_keydown_valid = false;

      /* Do not signal the meta-key(s) used to create text input */
      lastmod &= ~SDL_KMOD_RALT;
#ifdef __APPLE__
      lastmod &= ~SDL_KMOD_LALT;
#endif
      wid     = event->text.windowID;
      time    = event->text.timestamp/1000000;
      name    = toInt(codepoint);
      break;
    }
    /* https://wiki.libsdl.org/SDL3/SDL_DropEvent */
    case SDL_EVENT_DROP_BEGIN:
    case SDL_EVENT_DROP_FILE:
    case SDL_EVENT_DROP_TEXT:
    case SDL_EVENT_DROP_POSITION:
    case SDL_EVENT_DROP_COMPLETE:
    { switch ( event->type )
      { case SDL_EVENT_DROP_BEGIN:    name = NAME_dropBegin;    break;
	case SDL_EVENT_DROP_FILE:     name = NAME_dropFile;     break;
	case SDL_EVENT_DROP_TEXT:     name = NAME_dropText;     break;
	case SDL_EVENT_DROP_POSITION: name = NAME_dropPosition; break;
	case SDL_EVENT_DROP_COMPLETE: name = NAME_dropComplete; break;
	default: break; /* unreachable */
      }
      fx   = event->drop.x;
      fy   = event->drop.y;
      wid  = event->drop.windowID;
      time = event->drop.timestamp/1000000;
      { const char *data = event->drop.data;
	Any path = DEFAULT, text = DEFAULT, source = DEFAULT;

	if ( data && event->type == SDL_EVENT_DROP_TEXT )
	  text = CtoString(data);
	else if ( data && event->type == SDL_EVENT_DROP_FILE )
	  path = CtoName(data);
	if ( event->drop.source )
	  source = CtoName(event->drop.source);
	detail = answerObject(ClassDropData, path, text, source, EAV);
      }
      DEBUG(NAME_event,
	    Cprintf("Drop event %s on SDL window-id=%u: %s\n",
		    pp(name), wid,
		    event->drop.data ? event->drop.data : "(none)"));
      break;
    }
    case SDL_EVENT_KEY_DOWN:
    { SDL_Keymod isdown = (SDL_KMOD_LCTRL|SDL_KMOD_RCTRL|SDL_KMOD_GUI);
#ifndef __APPLE__
      isdown |= SDL_KMOD_LALT;
#else
      /* SDL forgets the modifiers when a window loses the focus and
       * learns them again only when they change.  Option held while
       * the focus moves to a new window -- from M-x into its prompt --
       * then arrives without SDL_KMOD_ALT, the key-down is not held
       * back and the text MacOS composed from Option is typed.
       */
      if ( !(event->key.mod & SDL_KMOD_ALT) &&
	   (ns_current_modifiers() & PCE_MOD_OPTION) )
	event->key.mod |= SDL_KMOD_LALT;
#endif

      lastmod = event->key.mod;
      if ( is_cue_alt_key(event->key.key) )
      { acceleratorCuesFrame(wsid_to_frame(event->key.windowID), true);
	fail;
      }
      menubar_key_time = 0;
      text_keydown_valid = false;
      if ( ws_menubar_key_equivalent(event) )
      { DEBUG(NAME_keyboard,
	      Cprintf("Keystroke claimed by the native menu bar.  "
		      "Mod=0x%x, key=0x%x\n",
		      event->key.mod, event->key.key));
	menubar_key_time = event->key.timestamp;
	fail;			/* MacOS runs the menu item itself */
      }
      name = keycode_to_name(event);
      if ( !name )
      { text_keydown = event->key;	/* text input may follow */
	text_keydown_valid = true;
	DEBUG(NAME_keyboard,
	      Cprintf("Ignoring keydown.  Mod=0x%x, scancode=%d, key=0x%x\n",
		      event->key.mod, event->key.scancode, event->key.key));
	fail;
      }

      if ( (event->key.mod & isdown) ||
	   !text_input_active(event->key.windowID) ) /* no text will follow */
	goto immediate_down_event;

      keydown_event  = *event;
      keyinput_time  = 0;
      keyboard_timer = SDL_AddTimer(KEYBOARD_DELAY, tm_keyboard_timeout, NULL);
      DEBUG(NAME_keyboard,
	    Cprintf("Delaying keyboard down event at %" PRIu64 "\n",
		    event->key.timestamp));
      fail;
    }
    case MY_EVENT_KEYDOWN_TIMEOUT:
      event   = &keydown_event;
    immediate_down_event:
      wid     = event->key.windowID;
      time    = event->key.timestamp/1000000;
      name    = keycode_to_name(event);
      keydown = &event->key;
      DEBUG(NAME_keyboard,
	    bool delayed = (&keydown_event == event);
	    Cprintf("SDL_EVENT_KEY_DOWN: %s at %"PRIu64"%s\n",
		    pp(name), event->text.timestamp,
		    delayed ? " (delayed)" : ""));
      break;
    default:
      fail;			/* for now */
  }

  if ( moved )
  { sp->last_x   = fx;
    sp->last_y   = fy;
    sp->last_wid = wid;
  }
  if ( tracking_wid && wid != tracking_wid )
    redirect_to_tracking_frame(&wid, tracking_wid, &fx, &fy);

  frame = wsid_to_frame(wid);
  DEBUG(NAME_event,
	if ( name != NAME_locMove )
	  Cprintf("Event %s on %s at %1f,%1f, SDL window-id=%d\n",
		  pp(name), pp(frame), fx, fy, wid));
  if ( !frame )
    fail;

  setLastEventTime(time);

#if O_HDPX
  WsFrame wfr = sdl_frame(frame, false);
  float scale = SDL_GetWindowPixelDensity(wfr->ws_window);
#else
  float scale = 1.0;
#endif

  if ( name != NAME_locMove )
  { WsFrame wfr = sdl_frame(frame, false);
    last_display_id = SDL_GetDisplayForWindow(wfr->ws_window);
  }

  float x = fx*scale;
  float y = fy*scale;
  float frame_x = x, frame_y = y;	/* for update_pointer_window() */
  bool drop_tracked = false;
  switch ( event->type )
  { case SDL_EVENT_DROP_BEGIN:
    case SDL_EVENT_DROP_FILE:
    case SDL_EVENT_DROP_TEXT:
    case SDL_EVENT_DROP_COMPLETE:
      if ( notNil(drop_tracking_window) )
      { float ox=0, oy=0;

	if ( !onFlag(drop_tracking_window, F_FREED|F_FREEING) &&
	     ws_created_window(drop_tracking_window) &&
	     ws_window_frame_position(drop_tracking_window, frame, &ox, &oy) )
	{ window = drop_tracking_window;
	  x = drop_tracking_x;
	  y = drop_tracking_y;
	  drop_tracked = true;
	}
	if ( !drop_tracked )
	  set_drop_tracking_window(NIL);
	else if ( event->type == SDL_EVENT_DROP_COMPLETE )
	  drop_tracking_done = true;	/* see set_drop_tracking_window() */
      }
      break;
    default:
      break;
  }

  if ( drop_tracked )
  { /* window, x and y are set above */
  } else if ( notNil(sp->tracking_window) )
  { if ( onFlag(sp->tracking_window, F_FREED|F_FREEING) ||
	 !ws_created_window(sp->tracking_window) )
    { Cprintf("Mouse tracking window (%s) is lost?\n",
	      pp(sp->tracking_window));
      delCodeReference(sp->tracking_window);
      sp->tracking_window = NIL;
      fail;
    }

    float ox=0, oy=0;
    bool rc = ws_window_frame_position(sp->tracking_window, frame, &ox, &oy);
    if ( rc )
    { window = sp->tracking_window;
      x -= ox;
      y -= oy;
    } else
    { window = frame;
      event_window(&window, &x, &y);
    }
    if ( release && sp->tracking_button == release )
    { delCodeReference(sp->tracking_window);
      sp->tracking_window = NIL;
    }
  } else if ( notNil(grabbing_window) )
  { if ( onFlag(grabbing_window, F_FREED|F_FREEING) )
    { DEBUG(NAME_event,			/* the grabber destroyed itself */
	    Cprintf("Grabbing window %s lost\n", pp(grabbing_window)));
      ev_event_grab_window(NIL);
      goto not_grabbing;
    }
    float ox=0, oy=0;
    bool rc = ws_window_frame_position(grabbing_window, frame, &ox, &oy);
    if ( rc )			/* grabbing window on same frame */
    { x -= ox;
      y -= oy;
    }
    window = grabbing_window;
  } else
  { not_grabbing:
    window = frame;
    event_window(&window, &x, &y);
    if ( press )
    { sp->tracking_window = window;
      sp->tracking_button = press;
      addCodeReference(sp->tracking_window);
    }
  }

  if ( event->type == SDL_EVENT_MOUSE_WHEEL &&
       !wheel_event(event, sp, window, &name, &rotation, &detail) )
    fail;

  if ( notDefault(device) &&		/* like keys, gamepad input goes */
       ((InputDevice)device)->kind == NAME_gamepad ) /* to the focus */
  { PceWindow sw = getKeyboardFocusFrame(frame);
    float ox = 0, oy = 0;

    if ( sw && ws_window_frame_position(sw, frame, &ox, &oy) )
    { window = sw;
      x = frame_x - ox;
      y = frame_y - oy;
    } else
    { window = frame;
      x = frame_x;
      y = frame_y;
    }
  }

  if ( event->type == SDL_EVENT_DROP_POSITION )
    update_drop_tracking_window(window, x, y);

  /* For pointer events, read modifiers live: cached lastmod can be
   * stale because event->key.mod on KEY_UP is not always reliable (we
   * have observed a phantom RSHIFT bit set on LSHIFT release with SDL3
   * on Linux, which then leaked into the next mouse click).  For other
   * events keep lastmod, which the TEXT_INPUT path masks to suppress
   * AltGr.
   */
  SDL_Keymod mod_for_event = isDefault(device) ? lastmod : SDL_GetModState();

  Any buttons = state_to_buttons(mouse_flags, mod_for_event);

  if ( moved )				/* pointer moved to another window? */
    update_pointer_window(sp, window, frame, frame_x, frame_y, buttons);
  else if ( event->type == SDL_EVENT_PEN_PROXIMITY_OUT )
    update_pointer_window(sp, NIL, frame, frame_x, frame_y, buttons);
  if ( notDefault(device) && ((InputDevice)device)->kind == NAME_pen )
    detail = pen_detail(sp);

  EventObj ev = answerObject(ClassEvent,
			     name,
			     window,
			     toNum(x), toNum(y),
			     buttons,
			     DEFAULT,	/* time: see setLastEventTime() */
			     device,
			     EAV);
  if ( ev )
  { setFrameEvent(ev, frame);

    if ( rotation )
      assign(ev, rotation, rotation);
    if ( keyup )
      detail = answerObject(ClassKeyData, keyup, OFF, EAV);
    else if ( keydown && isAEvent(ev, NAME_keyboard) )
    { Name key = characterName(ev);

      remember_key_down(keydown->scancode, key);
      detail = answerObject(ClassKeyData, key,
			    keydown->repeat ? ON : OFF, EAV);
    }
    if ( notNil(detail) )
      assign(ev, detail, detail);
  } else
  { Cprintf("Failed to create event with id %s\n", pp(name));
  }

  return ev;
}


static bool
dispatch_event(EventObj ev)
{ Any target = ev->window;
  status rc;

  DEBUG(NAME_event,
	if ( ev->id != NAME_locMove )
	  Cprintf("Dispatching %s (%s at %.1f,%.1f) to %s\n",
		  pp(ev), pp(ev->id), valNum(ev->x), valNum(ev->y),
		  pp(target)));

  if ( onFlag(target, F_FREED|F_FREEING) )
  { Cprintf("Event %s on %s; ignored\n", pp(ev->id), pp(target));
    ws_event_destroyed_target(target);
    return false;
  }

  ServiceMode(is_service_window(target),
	      { addCodeReference(ev);
		rc = postNamedEvent(ev, target, DEFAULT, NAME_postEvent);
		delCodeReference(ev);
	      });

  return rc;
}


/* Tell the application that device `d` was added or removed.  Post a
 * `id` event to the window with the keyboard focus of each frame.
 */

void
sdl_post_device_event(Name id, InputDevice d)
{ DisplayManager dm = TheDisplayManager();
  Cell dcell, fcell;

  for_cell(dcell, dm->members)
  { DisplayObj dsp = dcell->value;

    for_cell(fcell, dsp->frames)
    { FrameObj fr = fcell->value;

      if ( onFlag(fr, F_FREED|F_FREEING) || !ws_created_frame(fr) )
	continue;

      Any target = getKeyboardFocusFrame(fr);
      AnswerMark mark;
      EventObj ev;

      if ( !target )
	target = fr;
      markAnswerStack(mark);
      if ( (ev=answerObject(ClassEvent, id, target, DEFAULT, DEFAULT,
			    DEFAULT, DEFAULT, d, EAV)) )
	dispatch_event(ev);
      rewindAnswerStack(mark, NIL);
    }
  }
}


/**
 * Reset the internal event dispatching state.
 *
 * This function clears any pending events and resets the dispatching
 * mechanism to its initial state. It is typically called before
 * starting a new event loop or when reinitializing the event system.
 */
void
resetDispatch(void)
{
}

/* Registry of consoles whose pending input ws_discard_input() drops
   when a modal/confirmer is active.  There is at most one stdin console
   (registered by the host on init when stdin is a terminal) plus zero
   or more Epilog terminal consoles (registered by terminal.c when a
   pty pair is created).  Lookup is rare (only on discard), so a small
   linear-search array is sufficient.
*/

#define MAX_CONSOLES 16

typedef struct
{ bool          used;
  ConDrainKind  kind;
  waitable_t    handle;
} ConsoleEntry;

static ConsoleEntry consoles[MAX_CONSOLES];

bool
pceRegisterConsole(waitable_t handle, ConDrainKind kind)
{ for(int i=0; i<MAX_CONSOLES; i++)
  { if ( !consoles[i].used )
    { consoles[i].used   = true;
      consoles[i].kind   = kind;
      consoles[i].handle = handle;
      return true;
    }
  }
  return false;
}

bool
pceUnregisterConsole(waitable_t handle)
{ for(int i=0; i<MAX_CONSOLES; i++)
  { if ( consoles[i].used && consoles[i].handle == handle )
    { consoles[i].used = false;
      return true;
    }
  }
  return false;
}

/**
 * Turn an SDL event into an xpce event and post it, holding the xpce
 * lock.  ws_dispatch() itself runs unlocked, so that other threads can
 * use xpce while we wait for the next event, but making the event and
 * posting it touch the object base and must be serialised.  The lock is
 * recursive, so the handlers that claim it themselves (timers, streams,
 * the menu bar) are unaffected, and the callback into Prolog releases it
 * for the duration of the call (see pceMTUnlockAll()).
 */

static int  input_blocked = 0;		/* see sdl_dispatch_without_input() */
static int  held_count = 0;
static SDL_Event held_events[16];

static bool
hold_user_input(const SDL_Event *ev)
{ switch(ev->type)
  { case SDL_EVENT_MOUSE_BUTTON_UP:
      if ( held_count < (int)(sizeof(held_events)/sizeof(held_events[0])) )
	held_events[held_count++] = *ev;
      return true;
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_MOTION:
    case SDL_EVENT_MOUSE_WHEEL:
    case SDL_EVENT_KEY_DOWN:
    case SDL_EVENT_KEY_UP:
    case SDL_EVENT_TEXT_INPUT:
    case SDL_EVENT_TEXT_EDITING:
    case SDL_EVENT_TEXT_EDITING_CANDIDATES:
    case SDL_EVENT_DROP_BEGIN:
    case SDL_EVENT_DROP_FILE:
    case SDL_EVENT_DROP_TEXT:
    case SDL_EVENT_DROP_POSITION:
    case SDL_EVENT_DROP_COMPLETE:
    case SDL_EVENT_GAMEPAD_BUTTON_DOWN:
    case SDL_EVENT_GAMEPAD_BUTTON_UP:
    case SDL_EVENT_GAMEPAD_AXIS_MOTION:
      return true;
    default:
      return false;
  }
}

static void
dispatch_sdl_event(SDL_Event *ev)
{ EventObj event;
  AnswerMark mark;

  if ( input_blocked && hold_user_input(ev) )
    return;

  pceMTLock();
  if ( !sdl_live_resize_handled(ev) ) /* already done from the modal loop */
  { markAnswerStack(mark);		/* CtoEvent() creates answer objects */
    if ( (event=CtoEvent(ev)) )
      dispatch_event(event);
    rewindAnswerStack(mark, NIL);
  }
  pceMTUnlock();
}


		 /*******************************
		 *	   POLLING KEYS		*
		 *******************************/

/* Is a key held down?  This is for applications such as games that
 * poll the keyboard rather than following key and key_up events.  `key`
 * is a name of a key event (cursor_left, RET, f1, ...), a single
 * character or the name SDL uses for the key (e.g., `Left Shift`).
 * Returns false if the key is unknown.
 */

static const struct
{ const char   *name;
  SDL_Keycode	key;
} named_keys[] =
{ { "RET",	   SDLK_RETURN },
  { "ESC",	   SDLK_ESCAPE },
  { "TAB",	   SDLK_TAB },
  { "BS",	   SDLK_BACKSPACE },
  { "DEL",	   SDLK_DELETE },
  { "pause",	   SDLK_PAUSE },
  { "insert",	   SDLK_INSERT },
  { "cursor_home", SDLK_HOME },
  { "end",	   SDLK_END },
  { "page_up",	   SDLK_PAGEUP },
  { "page_down",   SDLK_PAGEDOWN },
  { "cursor_left", SDLK_LEFT },
  { "cursor_right",SDLK_RIGHT },
  { "cursor_up",   SDLK_UP },
  { "cursor_down", SDLK_DOWN },
  { "space",	   SDLK_SPACE },
  { NULL,	   0 }
};

static SDL_Scancode
key_scancode(const char *name)
{ for(int i=0; named_keys[i].name; i++)
  { if ( streq(named_keys[i].name, name) )
      return SDL_GetScancodeFromKey(named_keys[i].key, NULL);
  }

  int fn;
  char c;
  if ( sscanf(name, "f%d%c", &fn, &c) == 1 && fn >= 1 && fn <= 24 )
    return fn <= 12 ? SDL_SCANCODE_F1 + (fn-1)
		    : SDL_SCANCODE_F13 + (fn-13);

  if ( name[0] && !name[1] )		/* a character: use the layout */
  { SDL_Keycode k = (SDL_Keycode)tolower((unsigned char)name[0]);

    return SDL_GetScancodeFromKey(k, NULL);
  }

  return SDL_GetScancodeFromName(name); /* SDL_SCANCODE_UNKNOWN if none */
}

bool
ws_key_down(Name key, bool *down)
{ SDL_Scancode sc = key_scancode(strName(key));
  int count = 0;
  const bool *state;

  if ( sc == SDL_SCANCODE_UNKNOWN )
    return false;
  state = SDL_GetKeyboardState(&count);
  *down = (int)sc < count && state[sc];

  return true;
}


		 /*******************************
		 *	  TESTING SUPPORT	*
		 *******************************/

/* For testing input that cannot be generated otherwise, e.g., pens and
 * touch screens: build an SDL event of `type` for `fr` from the
 * attributes of `fields` (named as the fields of the SDL event
 * structure; missing fields are 0) and process it as if SDL reported
 * it.  Positions are relative to the frame, except for the finger
 * events, whose x and y are normalised to 0..1, as in SDL.
 */

static float
field(Sheet fields, const char *name)
{ Any v = getValueSheet(fields, CtoName(name));

  if ( v && isInteger(v) )
    return (float)valNum(v);
  if ( v == ON )
    return 1.0f;

  return 0.0f;
}

#define F(name) field(fields, #name)
/* Device ids are unsigned, but SDL's special ids are negative, e.g.,
 * SDL_PEN_MOUSEID is (SDL_MouseID)-2.  Converting a negative float
 * directly to an unsigned integer is undefined: it gives 0 on ARM64.
 */
#define ID(name) ((Sint64)field(fields, #name))

typedef struct
{ const char *name;
  SDL_EventType type;
} test_event_type;

static const test_event_type test_event_types[] =
{ { "mouse_motion",	 SDL_EVENT_MOUSE_MOTION },
  { "mouse_button_down", SDL_EVENT_MOUSE_BUTTON_DOWN },
  { "mouse_button_up",	 SDL_EVENT_MOUSE_BUTTON_UP },
  { "mouse_wheel",	 SDL_EVENT_MOUSE_WHEEL },
  { "pen_proximity_in",	 SDL_EVENT_PEN_PROXIMITY_IN },
  { "pen_proximity_out", SDL_EVENT_PEN_PROXIMITY_OUT },
  { "pen_down",		 SDL_EVENT_PEN_DOWN },
  { "pen_up",		 SDL_EVENT_PEN_UP },
  { "pen_motion",	 SDL_EVENT_PEN_MOTION },
  { "pen_button_down",	 SDL_EVENT_PEN_BUTTON_DOWN },
  { "pen_button_up",	 SDL_EVENT_PEN_BUTTON_UP },
  { "pen_axis",		 SDL_EVENT_PEN_AXIS },
  { "finger_down",	 SDL_EVENT_FINGER_DOWN },
  { "finger_up",	 SDL_EVENT_FINGER_UP },
  { "finger_motion",	 SDL_EVENT_FINGER_MOTION },
  { "finger_canceled",	 SDL_EVENT_FINGER_CANCELED },
#if SDL_VERSION_ATLEAST(3, 4, 0)
  { "pinch_begin",	 SDL_EVENT_PINCH_BEGIN },
  { "pinch_update",	 SDL_EVENT_PINCH_UPDATE },
  { "pinch_end",	 SDL_EVENT_PINCH_END },
#endif
  { "key_down",		 SDL_EVENT_KEY_DOWN },
  { "key_up",		 SDL_EVENT_KEY_UP },
  { "text_input",	 SDL_EVENT_TEXT_INPUT },
  { NULL, 0 }
};

status
ws_test_event(FrameObj fr, Name type, Sheet fields)
{ WsFrame wfr = sdl_frame(fr, false);
  const test_event_type *t;
  SDL_Event ev;

  if ( !wfr || !wfr->ws_window )
    fail;
  for(t=test_event_types; t->name; t++)
  { if ( streq(t->name, strName(type)) )
      break;
  }
  if ( !t->name )
    return errorPce(fr, NAME_unexpectedType, type);

  SDL_WindowID wid = SDL_GetWindowID(wfr->ws_window);
  Uint64 now = SDL_GetTicksNS();

  SDL_zero(ev);
  ev.type = t->type;
  switch(t->type)
  { case SDL_EVENT_MOUSE_MOTION:
      ev.motion.timestamp = now;
      ev.motion.windowID  = wid;
      ev.motion.which     = (SDL_MouseID)ID(which);
      ev.motion.state     = (SDL_MouseButtonFlags)F(state);
      ev.motion.x	  = F(x);
      ev.motion.y	  = F(y);
      break;
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
      ev.button.timestamp = now;
      ev.button.windowID  = wid;
      ev.button.which     = (SDL_MouseID)ID(which);
      ev.button.button    = (Uint8)F(button);
      ev.button.down      = t->type == SDL_EVENT_MOUSE_BUTTON_DOWN;
      ev.button.clicks    = 1;
      ev.button.x	  = F(x);
      ev.button.y	  = F(y);
      break;
    case SDL_EVENT_MOUSE_WHEEL:
      ev.wheel.timestamp = now;
      ev.wheel.windowID  = wid;
      ev.wheel.which     = (SDL_MouseID)ID(which);
      ev.wheel.x	 = F(dx);
      ev.wheel.y	 = F(dy);
      ev.wheel.integer_x = (Sint32)F(integer_x);
      ev.wheel.integer_y = (Sint32)F(integer_y);
      ev.wheel.mouse_x	 = F(x);
      ev.wheel.mouse_y	 = F(y);
      ev.wheel.direction = F(flipped) ? SDL_MOUSEWHEEL_FLIPPED
				      : SDL_MOUSEWHEEL_NORMAL;
      break;
    case SDL_EVENT_PEN_PROXIMITY_IN:
    case SDL_EVENT_PEN_PROXIMITY_OUT:
      ev.pproximity.timestamp = now;
      ev.pproximity.windowID  = wid;
      ev.pproximity.which     = (SDL_PenID)ID(which);
      break;
    case SDL_EVENT_PEN_DOWN:
    case SDL_EVENT_PEN_UP:
      ev.ptouch.timestamp = now;
      ev.ptouch.windowID  = wid;
      ev.ptouch.which     = (SDL_PenID)ID(which);
      ev.ptouch.pen_state = (SDL_PenInputFlags)F(state);
      ev.ptouch.x	  = F(x);
      ev.ptouch.y	  = F(y);
      ev.ptouch.eraser    = F(eraser) != 0.0f;
      ev.ptouch.down      = t->type == SDL_EVENT_PEN_DOWN;
      break;
    case SDL_EVENT_PEN_MOTION:
      ev.pmotion.timestamp = now;
      ev.pmotion.windowID  = wid;
      ev.pmotion.which     = (SDL_PenID)ID(which);
      ev.pmotion.pen_state = (SDL_PenInputFlags)F(state);
      ev.pmotion.x	   = F(x);
      ev.pmotion.y	   = F(y);
      break;
    case SDL_EVENT_PEN_BUTTON_DOWN:
    case SDL_EVENT_PEN_BUTTON_UP:
      ev.pbutton.timestamp = now;
      ev.pbutton.windowID  = wid;
      ev.pbutton.which     = (SDL_PenID)ID(which);
      ev.pbutton.pen_state = (SDL_PenInputFlags)F(state);
      ev.pbutton.x	   = F(x);
      ev.pbutton.y	   = F(y);
      ev.pbutton.button    = (Uint8)F(button);
      ev.pbutton.down      = t->type == SDL_EVENT_PEN_BUTTON_DOWN;
      break;
    case SDL_EVENT_PEN_AXIS:
      ev.paxis.timestamp = now;
      ev.paxis.windowID  = wid;
      ev.paxis.which     = (SDL_PenID)ID(which);
      ev.paxis.pen_state = (SDL_PenInputFlags)F(state);
      ev.paxis.x	 = F(x);
      ev.paxis.y	 = F(y);
      ev.paxis.axis	 = (SDL_PenAxis)F(axis);
      ev.paxis.value	 = F(value);
      break;
    case SDL_EVENT_FINGER_DOWN:
    case SDL_EVENT_FINGER_UP:
    case SDL_EVENT_FINGER_MOTION:
    case SDL_EVENT_FINGER_CANCELED:
      ev.tfinger.timestamp = now;
      ev.tfinger.windowID  = wid;
      ev.tfinger.touchID   = (SDL_TouchID)ID(touch_id);
      ev.tfinger.fingerID  = (SDL_FingerID)ID(finger_id);
      ev.tfinger.x	   = F(x);
      ev.tfinger.y	   = F(y);
      ev.tfinger.dx	   = F(dx);
      ev.tfinger.dy	   = F(dy);
      ev.tfinger.pressure  = F(pressure);
      break;
#if SDL_VERSION_ATLEAST(3, 4, 0)
    case SDL_EVENT_PINCH_BEGIN:
    case SDL_EVENT_PINCH_UPDATE:
    case SDL_EVENT_PINCH_END:
      ev.pinch.timestamp = now;
      ev.pinch.windowID  = wid;
      ev.pinch.scale	 = F(scale);
      break;
#endif
    case SDL_EVENT_KEY_DOWN:
    case SDL_EVENT_KEY_UP:
      ev.key.timestamp = now;
      ev.key.windowID  = wid;
      ev.key.scancode  = (SDL_Scancode)F(scancode);
      ev.key.key       = (SDL_Keycode)F(key);
      ev.key.mod       = (SDL_Keymod)F(mod);
      ev.key.down      = t->type == SDL_EVENT_KEY_DOWN;
      ev.key.repeat    = F(repeat) != 0.0f;
      break;
    case SDL_EVENT_TEXT_INPUT:
    { static char text[8];		/* SDL's text outlives the event */
      int chr = (int)F(char);
      char *e = utf8_put_char(text, chr);

      *e = EOS;
      ev.text.timestamp = now;
      ev.text.windowID  = wid;
      ev.text.text      = text;
      break;
    }
    default:
      fail;
  }

  dispatch_sdl_event(&ev);
  succeed;
}

#undef F
#undef ID


static bool
dispatch_ready_event(void)
{ ASSERT_SDL_MAIN();
  SDL_Event ev;

  if ( SDL_PollEvent(&ev) )
  { dispatch_sdl_event(&ev);
    return true;
  }

  return false;
}

#ifdef __WINDOWS__
static bool
win_wait_for_handle(HANDLE hConsole, int tmo)
{ if ( !hConsole )
    return true;

  for(;;)
  { DWORD rc = MsgWaitForMultipleObjects(1,
					 &hConsole,
					 false,	/* wait for either event */
					 tmo,
					 QS_ALLINPUT);

    if ( rc == WAIT_OBJECT_0+1 )
    { MSG msg;

      while( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
      { TranslateMessage(&msg);
	DispatchMessage(&msg);
      }
      return false;
    } else if ( rc == WAIT_OBJECT_0 )
    { return true;
    } else
    { DEBUG(NAME_stream,
	    Sdprintf("MsgWaitForMultipleObjects(): 0x%x\n", rc));
    }
  }
}
#endif

/**
 * Dispatch events from the event queue.
 *
 * @param FD The file descriptor to monitor for events.
 * @param timeout The maximum time to wait for an event.
 * @return true if an event was processed.  false on timeout.
 */

/* The timeout of ws_dispatch() is a num in seconds.  Return it in
 * milliseconds: @nil or a negative value waits forever (-1) and
 * @default is 0.25 seconds.
 */

static int
timeout_ms(Num timeout)
{ if ( isNil(timeout) )
    return -1;
  if ( isNum(timeout) )
  { double v = valNum(timeout);

    return v < 0.0 ? -1 : (int)(v*1000.0);
  }

  return 250;
}

status
ws_dispatch(IOSTREAM *input, Num timeout)
{ int tmo;
  waitable_t fd = NO_WAITABLE;

  if ( input && input != DEFAULT )
  {
#if __WINDOWS__
    fd = Swinhandle(input);
#else
    fd = Sfileno(input);
#endif
  }

  tmo = timeout_ms(timeout);

  if ( SDL_IsMainThread() )
  { if ( dispatch_ready_event() )
      succeed;

    if ( pceMTTryLock() )
    { RedrawDisplayManager(TheDisplayManager());
      ws_redraw_changed_frames();
      pceMTUnlock();
    }

    if ( dispatch_ready_event() )
      succeed;

    FDWatch *transient = NULL;
    if ( fd != NO_WAITABLE )
      transient = add_fd_to_watch(fd, FD_READY_DISPATCH, NULL);

    bool rc;
    SDL_Event ev;
    if ( tmo == -1 )
    { rc = SDL_WaitEvent(&ev);
    } else
    { rc = SDL_WaitEventTimeout(&ev, tmo);
    }

    if ( rc )
    { if ( ev.type == MY_EVENT_FD_READY &&
	   ev.user.code == FD_READY_DISPATCH )
      { FDWatch *watch = ev.user.data1;
	cmp_and_set_watch(watch, WATCH_PENDING, WATCH_PROCESSING);
	if ( transient )
	  remove_fd_watch(transient);
	succeed;
      }

      dispatch_sdl_event(&ev);
    }

    if ( transient )
      remove_fd_watch(transient);

    considerLocStillEvent();

    return rc;
  } else			/* SDL not yet initialised */
  { int to = timeout_ms(timeout);

#if HAVE_POLL
    int ready;
    struct pollfd fds[1];
    fds[0].fd = fd;
    fds[0].events = POLLIN;

    ready = poll(fds, 1, to);
    return ready > 0;
#elif __WINDOWS__
    return win_wait_for_handle(fd, to);
#else
    (void)to;
    return true;
#endif
  }
}

/**
 * @param fd is a file descriptor on POSIX systems and a HANDLE on
 * Windows.
 * @return true if there is input in `fd`.
 */

static bool
input_on_fd(waitable_t fd)
{
#ifdef __WINDOWS__
  return WaitForSingleObject(fd, 0) == WAIT_OBJECT_0;
#elif HAVE_POLL
  struct pollfd fds[1];

  fds[0].fd = fd;
  fds[0].events = POLLIN;

  return poll(fds, 1, 0) != 0;
#else
  if ( fd < FD_SETSIZE )
  { fd_set rfds;
    struct timeval tv;

    FD_ZERO(&rfds);
    FD_SET(fd, &rfds);
    tv.tv_sec = 0;
    tv.tv_usec = 0;

    return select(fd+1, &rfds, NULL, NULL, &tv) != 0;
  } else
    return true;
#endif
}

/**
 * Discard any pending input events on all registered consoles.
 *
 * Called when a modal/confirmer is up so that keystrokes the user typed
 * at a terminal (intended for the program but unwanted by the modal)
 * do not leak through after the modal closes.  Uses tcflush() on
 * POSIX tty/pty fds and FlushConsoleInputBuffer() on Windows console
 * handles; both drop pending bytes without consuming them, so this
 * never races with a Prolog engine reading the same fd.
 *
 * @param msg A message indicating the reason for discarding input.
 */
void
ws_discard_input(const char *msg)
{ bool announced = false;

  for(int i=0; i<MAX_CONSOLES; i++)
  { ConsoleEntry *c = &consoles[i];
    if ( !c->used || !input_on_fd(c->handle) )
      continue;

    if ( !announced )
    { Cprintf("%s; discarding input ...", msg);
      announced = true;
    }

    switch(c->kind)
    {
#ifdef __WINDOWS__
      case CON_DRAIN_WIN_CONSOLE:
	FlushConsoleInputBuffer(c->handle);
	break;
#else
      case CON_DRAIN_TCFLUSH:
	tcflush(c->handle, TCIFLUSH);
	break;
#endif
      case CON_DRAIN_NONE:
      default:
	break;
    }
  }

  if ( announced )
    Cprintf("ok\n");
}

/**
 * Find a  target window  when the  pointer has  left the  window that
 * grabbed the  pointer.  This is used  for drag and drop  and well as
 * for example  the Visual Hierarchy  tool to find the  target window.
 * If root  is @default, find the  frame, else find the  window inside
 * the frame that is below the pointer.
 *
 * @param ev The event object to examine.
 * @param root The root window to compare against.
 * @return The subwindow where the event occurred, or NULL if not applicable.
 */
Any
ws_event_in_subwindow(EventObj ev, Any root)
{ if ( isDefault(root) )
  { return ev->frame;
  } else if ( instanceOfObject(root, ClassFrame) )
  { Any window = root;
    float x = valNum(ev->x);
    float y = valNum(ev->y);

    if ( instanceOfObject(ev->window, ClassWindow) )
    { float ox = 0, oy = 0;
      if ( ws_window_frame_position(ev->window, root, &ox, &oy) )
      { x += ox;
	y += oy;
      }
    }

    event_window(&window, &x, &y);
    return window;
  } else
    fail;
}

/**
 * Wait for a key event within a specified timeout.
 *
 * @param maxwait The maximum time to wait for a key event, in milliseconds.
 * @return 1 if a key event was received; 0 if the timeout expired.
 */
int
ws_wait_for_key(int maxwait)
{
    return 0;
}

void
sdl_alert(void)
{ SDL_Event ev = {0};
  ev.type = MY_EVENT_ALERT;
  SDL_PushEvent(&ev);
}

/**
 * Keep the event loop running in the main thread until done(closure)
 * returns true, while user input is not delivered.  This is used while
 * another thread runs a blocking native dialog: windows keep redrawing
 * and timers and streams keep being served, but the application cannot
 * be operated behind the dialog's back.  Keyboard, text, wheel, motion,
 * button-press and drop events are discarded.  Button releases are held
 * and re-queued afterwards, so a gesture that started the dialog still
 * sees its release.  The code that makes done() true must call
 * sdl_alert() to wake us up.
 */

void
sdl_dispatch_without_input(bool (*done)(void *closure), void *closure)
{ ASSERT_SDL_MAIN();

  input_blocked++;
  while( !(*done)(closure) )
    ws_dispatch(NULL, DEFAULT);
  if ( --input_blocked == 0 )
  { for(int i=0; i<held_count; i++)
      SDL_PushEvent(&held_events[i]);
    held_count = 0;
  }
}
