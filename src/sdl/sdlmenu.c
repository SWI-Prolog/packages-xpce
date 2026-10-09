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
#include <h/dialog.h>
#include <stdbool.h>
#include "sdlmenu.h"

/* A colour of the theme, so it follows a change of theme
 */

static Colour
theme_colour(Colour *cache, const char *name)
{ if ( !*cache )
  { *cache = newObject(ClassColour, CtoKeyword(name), EAV);
    lockObject(*cache, ON);
  }

  return *cache;
}

static Colour c_field, c_accent, c_separator, c_pressed;


/**
 * Colour for greyed out (inactive) text and marks: the theme colour
 * `ui_inactive`, so it follows a change of theme.
 *
 * @return The Colour object.
 */
Colour
ws_3d_grey(void)
{ static Colour c;

  return theme_colour(&c, "ui_inactive");
}

		 /*******************************
		 *	      TEXTITEM		*
		 *******************************/


/* Draw a chevron of width `w` centred in the box x,y,w,h, pointing
 * down or up
 */

static void
entry_chevron(double x, double y, double w, double h, double cw, bool down)
{ double ch = cw/2.0;
  double cx = x + (w-cw)/2.0;
  double cy = y + (h-ch)/2.0;
  fpoint dpts[3] = { { cx, cy }, { cx+cw/2.0, cy+ch }, { cx+cw, cy } };
  fpoint upts[3] = { { cx, cy+ch }, { cx+cw/2.0, cy }, { cx+cw, cy+ch } };
  double pen = r_thickness(1.5);

  r_dash(NAME_none);
  r_polygon(down ? dpts : upts, 3, FALSE);
  r_thickness(pen);
}

/**
 * Return the horizontal margin for entry fields.
 *
 * @return Margin in pixels.
 */
int
ws_entry_field_margin(void)
{ return 1;
}

/**
 * ws_entry_field() is used by classes that need to create an editable
 * field  of specified  dimensions. If  the  field happens  to be  not
 * editable now, this is indicated by `editable'.
 *
 * The field is a flat rounded box.  An editable field is filled with
 * the theme colour `ui_window_background`; its border is `ui_separator`,
 * or a wider `ui_accent` border if it has the keyboard focus.  The
 * combo box and stepper buttons are chevrons in the `bw` pixels at the
 * right of the field.
 *
 * @param gr Pointer to the Graphical object.
 * @param x The x-coordinate.
 * @param y The y-coordinate.
 * @param w The width.
 * @param h The height.
 * @param bw The width of the combo box or stepper buttons.
 * @param flags Rendering flags.
 * @return SUCCEED on success; otherwise, FAIL.
 */

status
ws_entry_field(Graphical gr, int x, int y, int w, int h, int bw, int flags)
{ bool editable = (flags & TEXTFIELD_EDITABLE);
  bool focus = editable && hasInputFocusDialogItem(gr);
  Any fill = editable ? (Any)theme_colour(&c_field, "ui_window_background") : NIL;
  Any old = r_colour(focus ? theme_colour(&c_accent, "ui_accent")
			   : theme_colour(&c_separator, "ui_separator"));

  r_thickness(focus ? 2 : 1);
  r_dash(NAME_none);
  if ( gr->active == OFF )		/* drawn in <-inactive_colour: fade */
  { r_push_group();
    r_smooth_box(x, y, w, h, FIELD_RADIUS, fill);
    r_pop_group_with_alpha(INACTIVE_ALPHA);
  } else
    r_smooth_box(x, y, w, h, FIELD_RADIUS, fill);
  r_thickness(1);
  r_colour(editable ? old : (Any)ws_3d_grey());

  if ( flags & TEXTFIELD_COMBO )
  { entry_chevron(x+w-bw, y, bw, h, 8.4,
		  !(flags & TEXTFIELD_COMBO_DOWN));
  }
  if ( flags & TEXTFIELD_STEPPER )
  { double cw = bw;
    double bx, bh = h/2.0;

    bx = x+w-cw;

    if ( flags & (TEXTFIELD_INCREMENT|TEXTFIELD_DECREMENT) )
    { double by = (flags & TEXTFIELD_INCREMENT) ? y+2 : y+bh;

      r_fill(bx, by, cw-2, bh-2, theme_colour(&c_pressed, "ui_button_pressed"));
    }
    entry_chevron(bx, y+1,  cw, bh, 7, false);
    entry_chevron(bx, y+bh-1, cw, bh, 7, true);
  }

  r_colour(old);
  succeed;
}

/**
 * Draw a checkbox widget.
 *
 * @param x The x-coordinate.
 * @param y The y-coordinate.
 * @param w The width.
 * @param h The height.
 * @param b Border size or state.
 * @param flags Rendering flags.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_draw_checkbox(int x, int y, int w, int h, int b, int flags)
{ fail;
}

/**
 * Compute the size of a checkbox widget.
 *
 * @param flags Flags that affect size calculation.
 * @param w Pointer to output width.
 * @param h Pointer to output height.
 * @return SUCCEED on success; otherwise, FAIL.
 */
status
ws_checkbox_size(int flags, int *w, int *h)
{ *w = 0;
  *h = 0;

  fail;
}

/* SDL_ShowMessageBox() blocks the calling thread until the user closes
   the box.  On Wayland it runs zenity as a separate process, so if the
   main thread makes the call, no events are processed, the windows are
   not redrawn and the compositor may report the application as not
   responding.  We therefore run it in a helper thread while the main
   thread keeps dispatching events (see sdl_dispatch_without_input()).
*/

#if !defined(__APPLE__) && !defined(__WINDOWS__)
#define MESSAGE_BOX_THREAD 1
#endif

#ifdef MESSAGE_BOX_THREAD

typedef struct
{ SDL_MessageBoxData *data;
  int		      buttonid;
  bool		      rc;
  SDL_AtomicInt	      done;
} message_box_job;

static int SDLCALL
message_box_thread(void *closure)
{ message_box_job *job = closure;

  job->rc = SDL_ShowMessageBox(job->data, &job->buttonid);
  SDL_SetAtomicInt(&job->done, 1);
  sdl_alert();

  return 0;
}

static bool
message_box_done(void *closure)
{ message_box_job *job = closure;

  return SDL_GetAtomicInt(&job->done);
}

static bool
wayland_driver(void)
{ const char *driver = SDL_GetCurrentVideoDriver();

  return driver && strcmp(driver, "wayland") == 0;
}
#endif /*MESSAGE_BOX_THREAD*/

static bool
show_message_box(SDL_MessageBoxData *data, int *buttonid)
{
#ifdef MESSAGE_BOX_THREAD
  if ( SDL_IsMainThread() && wayland_driver() )
  { message_box_job job = { .data = data, .buttonid = *buttonid };
    SDL_Thread *thread;

    SDL_SetAtomicInt(&job.done, 0);
    if ( (thread=SDL_CreateThread(message_box_thread, "message-box", &job)) )
    { sdl_dispatch_without_input(message_box_done, &job);
      SDL_WaitThread(thread, NULL);
      *buttonid = job.buttonid;
      return job.rc;
    }
  }
#endif

  return SDL_ShowMessageBox(data, buttonid);
}

/**
 * Show a message box with the specified message and flags.
 *
 * @param msg Message to display.
 * @param MBX_INFORM, MBX_ERROR or MBX_CONFIRM
 * @return MBX_OK, MBX_CANCEL or MBX_NOTHANDLED.   The latter
 * is returned if the system message box fails.
 */
int
ws_message_box(Any client, CharArray title, CharArray msg, int flags)
{ SDL_MessageBoxButtonData btns[2];
  SDL_MessageBoxData data =
    { .flags = SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT,
      .title = "SWI-Prolog",
      .buttons = btns
    };

  /* Copy: the ring buffer of stringToUTF8() is reused while we dispatch */
  char *message = SDL_strdup(stringToUTF8(&msg->data, NULL));
  char *title_s = notDefault(title) ? SDL_strdup(stringToUTF8(&title->data,
								NULL))
				    : NULL;
  data.message = message;
  if ( title_s )
    data.title = title_s;

  FrameObj fr = getFrameVisual(client);
  DEBUG(NAME_inform,
	Cprintf("client: %s; frame: %s\n", pp(client), pp(fr)));
  if ( fr )
  { WsFrame f = sdl_frame(fr, false);
    if ( f )
      data.window = f->ws_window;
  }

  switch(flags)
  { case MBX_INFORM:
      data.flags |= SDL_MESSAGEBOX_INFORMATION;
      btns[0].text = "OK";
      btns[0].buttonID = MBX_OK;
      data.numbuttons = 1;
      break;
    case MBX_ERROR:
      data.flags |= SDL_MESSAGEBOX_INFORMATION;
      btns[0].text = "OK";
      btns[0].buttonID = MBX_OK;
      data.numbuttons = 1;
      break;
    case MBX_CONFIRM:
      data.flags |= SDL_MESSAGEBOX_WARNING;
      btns[0].text = "OK";
      btns[0].buttonID = MBX_OK;
      btns[1].text = "Cancel";
      btns[1].buttonID = MBX_CANCEL;
      data.numbuttons = 2;
      break;
  }

  int buttonid = MBX_NOTHANDLED;
  bool rc = show_message_box(&data, &buttonid);

  SDL_free(message);
  SDL_free(title_s);

  return rc ? buttonid : MBX_NOTHANDLED;
}
