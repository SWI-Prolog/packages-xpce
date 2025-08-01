/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  1999-2025, University of Amsterdam
                              VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

#define _XOPEN_SOURCE 600	/* Get PTY API */
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0B00	/* Get PseudoConsole API */
#define SWIPL_WINDOWS_NATIVE_ACCESS 1
#include <h/kernel.h>
#include <h/text.h>
#include "terminal.h"
#ifdef HAVE_POLL
#include <poll.h>
#endif
#include <SWI-Stream.h>

/* This file  implements a terminal  emulator in XPCE.  A  terminal is
 * connected to a Prolog thread, the _client_.
 *
 * ## I/O Handling
 *
 * Typed  characters are  added to  the terminal's  input queue  using
 * rlc_add_queue().   The _client_  reads input  using getch().   This
 * returns the  next character  from the input  queue or  blocks until
 * input  becomes available.   The blocking  mechanism depends  on the
 * platform.   On Unix  systems we  use a  pipe as  this allows  us to
 * process signals.  On Windows we use an event (TBD).
 */

#define MAXLINE	     1024		/* max chars per line */

#define GWL_DATA	0		/* offset for client data */

#define CHG_RESET	0		/* unchanged */
#define CHG_CHANGED	1		/* changed, but no clear */
#define CHG_CLEAR	2		/* clear */
#define CHG_CARET	4		/* caret has moved */

#define SEL_CHAR	0		/* character-unit selection */
#define SEL_WORD	1		/* word-unit selection */
#define SEL_LINE	2		/* line-unit selection */

#ifndef EOS
#define EOS 0
#endif

#define ESC 27				/* the escape character */

#define IMODE_RAW	1		/* char-by-char */
#define IMODE_COOKED	2		/* line-by-line */

#define NextLine(b, i) ((i) < (b)->height-1 ? (i)+1 : 0)
#define PrevLine(b, i) ((i) > 0 ? (i)-1 : (b)->height-1)
#define Bounds(v, mn, mx) ((v) < (mn) ? (mn) : (v) > (mx) ? (mx) : (v))

#define OPT_SIZE	0x01
#define OPT_POSITION	0x02

		 /*******************************
		 *	     FUNCTIONS		*
		 *******************************/

static void	rlc_destroy_buffer(RlcData b);
static bool	rlc_caret_xy(RlcData b, int *x, int *y);
static void	rlc_resize_pixel_units(RlcData b, int w, int h);
static RlcData	rlc_make_buffer(int w, int h);
static int	rlc_count_lines(RlcData b, int from, int to);
static void	rlc_add_line(RlcData b);
static void	rlc_open_line(RlcData b);
static void	rlc_update_scrollbar(RlcData b);
static void	rlc_init_text_dimensions(RlcData b, FontObj f);
static int	rlc_add_lines(RlcData b, int here, int add);
static void	rlc_start_selection(RlcData b, int x, int y);
static void	rlc_extend_selection(RlcData b, int x, int y);
static void	rlc_word_selection(RlcData b, int x, int y);
static void	rlc_line_selection(RlcData b, int x, int y);
static bool	rlc_has_selection(RlcData b);
static void	rlc_select_all(RlcData b);
static uchar_t   *rlc_selection(RlcData b);
static void	rlc_set_selection(RlcData b, int sl, int sc, int el, int ec);
static const uchar_t *rlc_clicked_link(RlcData b, int x, int y);
static const uchar_t *rlc_over_link(RlcData b, int x, int y);
static href    *rlc_add_link(RlcTextLine tl, const uchar_t *link,
			     int start, int len);
static void	rlc_free_link(href *hr);
static void	rlc_free_links(href *links);
static void	rcl_check_links(RlcTextLine tl);
static bool	rlc_copy(RlcData b, Name to);
static void	rlc_request_redraw(RlcData b);
static void	rlc_redraw(RlcData b, int x, int y, int w, int h);
static void	rlc_resize(RlcData b, int w, int h);
static void	rlc_adjust_line(RlcData b, int line);
static int	text_width(RlcData b, const text_char *text, int len);
static int	tchar_width(RlcData b, const char *text,
			    size_t ulen, size_t len, FontObj font);
static void     rlc_reinit_line(RlcData b, int line);
static void	rlc_free_line(RlcData b, int line);
static void	rlc_destroy_saved_screen(RlcData b);
static int	rlc_between(RlcData b, int f, int t, int v);
static void	typed_char(RlcData b, int chr);
static void	rlc_putansi(RlcData b, int chr);
static void	rlc_update(rlc_console c);
static void	changed_caret(RlcData b);
static bool	rlc_open_pty_pair(RlcData b, int cols, int rows);
static void	rlc_close_connection(RlcData b);
static ssize_t	rlc_send(RlcData b, const char *buffer, size_t count);
static void	rlc_resize_pty(RlcData b, int cols, int rows);
static Name	TCHAR2Name(const uchar_t *str);
static StringObj TCHAR2String(const uchar_t *str);
static void	rlc_scroll_bubble(RlcData b,
				  int *length, int *start, int *view);
static void	rlc_scroll_lines(RlcData b, int lines);
static void	rlc_shift_up(RlcData b, int shift);


		 /*******************************
		 *        DEBUG SUPPORT         *
		 *******************************/

static void Dprint_links(RlcTextLine tl, const char *msg);
static void Dprint_line(RlcTextLine tl, bool links);
static void Dprint_lines(RlcData b, int from, int to);
static void Dprint_chr(int chr);
static void Dprint_csi(RlcData b, int chr);

static void
rlc_check_assertions(RlcData b)
{ int window_last = rlc_add_lines(b, b->window_start, b->window_size-1);
  int y;

  assert(b->last != b->first || b->first == 0);
  assert(b->caret_x >= 0 && b->caret_x < b->width);
					/* TBD: debug properly */
/*assert(rlc_between(b, b->window_start, window_last, b->caret_y));*/
  (void)window_last;

  for(y=0; y<b->height; y++)
  { RlcTextLine tl = &b->lines[y];

    assert(tl->size >= 0 && tl->size <= b->width);
    (void)tl;
  }
}

		 /*******************************
		 *         XPCE BINDING         *
		 *******************************/

#ifdef __WINDOWS__
static status launchTerminalImage(TerminalImage ti, CharArray cmdline);
#endif

static status
initialiseTerminalImage(TerminalImage ti, Int w, Int h)
{ if ( isDefault(w) )
    w = toInt(200);
  if ( isDefault(h) )
    h = toInt(100);
  initialiseGraphical(ti, ZERO, ZERO, w, h);
  assign(ti, bindings, newObject(ClassKeyBinding, NIL, NAME_terminal, EAV));
  obtainClassVariablesObject(ti);

  // compute width in characters from w
  int cw = (double)valInt(w)/c_width('m', ti->font);

  RlcData b = rlc_make_buffer(cw, valInt(ti->save_lines));
  ti->data = b;
  b->object = ti;
  rlc_init_text_dimensions(b, ti->font);

  succeed;
}

static status
unlinkTerminalImage(TerminalImage ti)
{ ScrollBar sb = ti->scroll_bar;

  if ( sb )
  { assign(ti, scroll_bar, NIL);
    send(sb, NAME_destroy, EAV);
  }
  if ( ti->data )
  { ti->data->object = NULL;
    rlc_destroy_buffer(ti->data);
    ti->data = NULL;
  }

  return unlinkGraphical((Graphical)ti);
}

static status
computeTerminalImage(TerminalImage ti)
{ if ( notNil(ti->request_compute) )
  { rlc_init_text_dimensions(ti->data, ti->font);
    rlc_resize_pixel_units(ti->data, valInt(ti->area->w), valInt(ti->area->h));
  }

  succeed;
}

static status
RedrawAreaTerminalImage(TerminalImage ti, Area a)
{ int x, y, w, h;

  initialiseDeviceGraphical(ti, &x, &y, &w, &h);

  rlc_redraw(ti->data, x, y, w, h);

  return RedrawAreaGraphical(ti, a);
}

static status
ChangedEntireTerminalImage(TerminalImage ti)
{ requestComputeGraphical(ti, DEFAULT);

  succeed;
}

static status
geometryTerminalImage(TerminalImage ti, Int x, Int y, Int w, Int h)
{
#define Changed(a) ( notDefault(a) && (a) != ti->area->a )

  if ( Changed(w) || Changed(h) )	/* resize */
  { geometryGraphical(ti, x, y, w, h);
    rlc_resize_pixel_units(ti->data, valInt(ti->area->w), valInt(ti->area->h));
    ChangedEntireTerminalImage(ti);
  } else
    geometryGraphical(ti, x, y, DEFAULT, DEFAULT); /* move only */
#undef Changed

  succeed;
}

static status
bubbleScrollBarTerminalImage(TerminalImage ti, ScrollBar sb)
{ int length, start, view;
  rlc_scroll_bubble(ti->data, &length, &start, &view);
  send(sb, NAME_bubble,
       toInt(length), toInt(start), toInt(view), EAV);

  succeed;
}

static status
scrollVerticalTerminalImage(TerminalImage ti,
			    Name dir, Name unit, Int amount)
{ RlcData b = ti->data;

  if ( unit == NAME_file )
  { int lines = rlc_count_lines(b, b->first, b->last);
    int start = lines*valInt(amount)/1000;
    b->window_start = rlc_add_lines(b, b->first, start);
    b->changed |= CHG_CARET|CHG_CLEAR|CHG_CHANGED;
    rlc_request_redraw(b);
  } else if ( unit == NAME_line )
  { int lines = valInt(amount);
    if ( dir == NAME_backwards )
      lines = -lines;
    rlc_scroll_lines(b, lines);
  } else if ( unit == NAME_page )
  { int lines = b->window_size*valInt(amount)/1000;
    if ( dir == NAME_backwards )
      lines = -lines;
    rlc_scroll_lines(b, lines);
  } else
  { Cprintf("scroll unit is %s\n", pp(unit));
    fail;
  }

  succeed;
}

static status
clickedLinkTerminalImage(TerminalImage ti, Name href)
{ if ( ti->link_message )
    forwardReceiverCode(ti->link_message, ti, href, EAV);
  succeed;
}

static status
eventTerminalImage(TerminalImage ti, EventObj ev)
{ if ( ev->id == NAME_locMove && notNil(ti->link_message) )
  { Int x, y;
    get_xy_event(ev, ti, ON, &x, &y);
    if ( rlc_over_link(ti->data, valInt(x), valInt(y)) )
    { assign(ti, armed_link, ON);
    } else
    { assign(ti, armed_link, OFF);
    }

    fail;
  }

  DEBUG(NAME_event, Cprintf("Event: %s\n", pp(ev->id)));

  if ( mapWheelMouseEvent(ev, ti) )
    succeed;

  if ( isAEvent(ev, NAME_focus) )
  { RlcData b = ti->data;
    if ( isAEvent(ev, NAME_activateKeyboardFocus) )
    { ws_enable_text_input((Graphical)ti, ON);
      b->has_focus = true;
      if ( b->focus_inout_events )
      { const char *focus_in = "\e[I";
	rlc_send(b, focus_in, strlen(focus_in));
      }
    } else if ( isAEvent(ev, NAME_deactivateKeyboardFocus) )
    { ws_enable_text_input((Graphical)ti, OFF);
      b->has_focus = false;
      if ( b->focus_inout_events )
      { const char *focus_out = "\e[O";
	rlc_send(b, focus_out, strlen(focus_out));
      }
    }
    changed_caret(b);

    succeed;
  }

  if ( isAEvent(ev, NAME_keyboard) )
    return send(ti, NAME_typed, ev, EAV);
  if ( isAEvent(ev, NAME_msMiddleUp) )
    return send(ti, NAME_paste, NAME_primary, EAV);
  if ( isAEvent(ev, NAME_msLeftDown) )
  { RlcData b = ti->data;
    Int x, y;
    get_xy_event(ev, ti, ON, &x, &y);
    Name multi = getMulticlickEvent(ev);

    if ( multi == NAME_double )
    { rlc_word_selection(b, valInt(x), valInt(y));
      if ( rlc_has_selection(b) )
	rlc_copy(b, NAME_primary);
    } else if ( multi == NAME_triple )
    { rlc_line_selection(b, valInt(x), valInt(y));
      if ( rlc_has_selection(b) )
	rlc_copy(b, NAME_primary);
    } else
      rlc_start_selection(b, valInt(x), valInt(y));
    succeed;
  }
  if ( isAEvent(ev, NAME_msLeftUp) )
  { RlcData b = ti->data;
    Int x, y;
    get_xy_event(ev, ti, ON, &x, &y);
    static const uchar_t *lnk;
    if ( (lnk=rlc_clicked_link(b, valInt(x), valInt(y))) &&
	 notNil(ti->link_message) )
    { Name href = TCHAR2Name(lnk);
      clickedLinkTerminalImage(ti, href);
    } else if ( rlc_has_selection(b) )
      rlc_copy(b, NAME_primary);
    succeed;
  }
  if ( isAEvent(ev, NAME_msLeftDrag) )
  { RlcData b = ti->data;
    Int x, y;
    get_xy_event(ev, ti, ON, &x, &y);
    rlc_extend_selection(b, valInt(x), valInt(y));
    succeed;
  }
  if ( isAEvent(ev, NAME_msRightUp) )
  { RlcData b = ti->data;
    if ( rlc_has_selection(b) )
    { Int x, y;

      get_xy_event(ev, ti, ON, &x, &y);
      rlc_extend_selection(b, valInt(x), valInt(y));
      if ( rlc_has_selection(b) )
	rlc_copy(ti->data, NAME_primary);
      succeed;
    }
  }

  fail;
}

static status
typedTerminalImage(TerminalImage ti, EventObj ev)
{ int chr;
  const char *seq = NULL;
  char buf[10];
  RlcData b = ti->data;

  if ( typedKeyBinding(ti->bindings, ev, (Graphical)ti) )
    succeed;

  if ( isInteger(ev->id) )
  { chr = valInt(ev->id);
  } else if ( ev->id == NAME_BS )
  { chr = 127;
    if ( valInt(ev->buttons) & BUTTON_meta )
      chr += META_OFFSET;
  } else if ( ev->id == NAME_TAB )
  { chr = '\t';
    if ( valInt(ev->buttons) & BUTTON_meta )
      chr += META_OFFSET;
  } else if ( ev->id == NAME_RET )
  { chr = '\r';
    if ( valInt(ev->buttons) & BUTTON_meta )
      chr += META_OFFSET;
  } else if ( ev->id == NAME_ESC )
  { chr = '\e';
  } else if ( ev->id == NAME_cursorUp )
  { seq = b->app_escape ? "\e0A" : "\e[A";
  } else if ( ev->id == NAME_cursorDown )
  { seq = b->app_escape ? "\e0B" : "\e[B";
  } else if ( ev->id == NAME_cursorLeft )
  { seq = b->app_escape ? "\e0D" : "\e[D";
  } else if ( ev->id == NAME_cursorRight )
  { seq = b->app_escape ? "\e0C" : "\e[C";
  } else if ( ev->id == NAME_delete )
  { seq = "\e[3~";
  } else
    fail;

  if ( !seq && chr >= META_OFFSET )
  { buf[0] = '\e';
    buf[1] = chr-META_OFFSET;
    buf[2] = 0;
    seq = buf;
  }

  if ( seq )
    rlc_send(ti->data, seq, strlen(seq));
  else
    typed_char(ti->data, chr);

  succeed;
}

static CursorObj
getDisplayedCursorTerminalImage(TerminalImage ti)
{ if ( isOn(ti->armed_link) )
    return getClassVariableValueObject(ti, NAME_linkCursor);
  else
    return ti->cursor;
}

static status
hasSelectionTerminalImage(TerminalImage ti)
{ RlcData b = ti->data;
  return rlc_has_selection(b);
}

static status
selectAllTerminalImage(TerminalImage ti)
{ RlcData b = ti->data;
  rlc_select_all(b);
  succeed;
}

static StringObj
getSelectedTerminalImage(TerminalImage ti)
{ RlcData b = ti->data;
  uchar_t *sel = rlc_selection(b);

  if ( sel )
  { StringObj str = TCHAR2String(sel);
    if ( str )
      pushAnswerObject(str);
    answer(str);
  }

  fail;
}

static status
clearSelectionTerminalImage(TerminalImage ti)
{ rlc_set_selection(ti->data, 0, 0, 0, 0);
  succeed;
}

static status
pasteTerminalImage(TerminalImage ti, Name which)
{ if ( isDefault(which) )
    which = NAME_clipboard;
  StringObj str = get(CurrentDisplay(ti), NAME_paste, which, EAV);
  size_t ulen;
  const char *u = stringToUTF8(&str->data, &ulen);
  const char *bsm_start = "\e[200~";
  const char *bsm_end = "\e[201~";

  clearSelectionTerminalImage(ti);
  DEBUG(NAME_paste, Cprintf("Paste %zd bytes from %s\n", ulen, pp(which)));
  if ( ti->data->bracketed_paste_mode )
    rlc_send(ti->data, bsm_start, strlen(bsm_start));
  if ( rlc_send(ti->data, u, ulen) != ulen )
  { Cprintf("Failed to send %s\n", u);
    fail;
  }
  if ( ti->data->bracketed_paste_mode )
    rlc_send(ti->data, bsm_end, strlen(bsm_end));

  succeed;
}

static status
copyTerminalImage(TerminalImage ti, Name which)
{ if ( isDefault(which) )
    which = NAME_clipboard;

  return rlc_copy(ti->data, which);
}

static status
interruptTerminalImage(TerminalImage ti)
{ succeed;
}

static status
copyOrInterruptTerminalImage(TerminalImage ti)
{ if ( send(ti, NAME_copy, NAME_clipboard, EAV) )
    succeed;
  return send(ti, NAME_interrupt, EAV);
}

static status
cursorEndTerminalImage(TerminalImage ti)
{ RlcData b = ti->data;
  const char *seq = b->app_escape ? "\e0F" : "\e[F";
  rlc_send(ti->data, seq, strlen(seq));
  succeed;
}

static status
cursorHomeTerminalImage(TerminalImage ti)
{ RlcData b = ti->data;
  const char *seq = b->app_escape ? "\e0H" : "\e[H";
  rlc_send(ti->data, seq, strlen(seq));
  succeed;
}

static status
cursorPageUpTerminalImage(TerminalImage ti)
{ return send(ti, NAME_scrollVertical, NAME_backwards,
	      NAME_page, toInt(900), EAV);
}

static status
cursorPageDownTerminalImage(TerminalImage ti)
{ return send(ti, NAME_scrollVertical, NAME_forwards,
	      NAME_page, toInt(900), EAV);
}


static status
saveLinesTerminalImage(TerminalImage ti, Int lines)
{ assign(ti, save_lines, lines);
  succeed;
}

static status
refreshTerminalImage(TerminalImage ti)
{ RlcData b = ti->data;
  if ( b )
  { b->changed |= CHG_CARET|CHG_CLEAR|CHG_CHANGED;
    rlc_request_redraw(b);
  }
  succeed;
}

static status
fontTerminalImage(TerminalImage ti, FontObj font, FontObj bold)
{ assign(ti, font, font);
  if ( isDefault(bold) )
    bold = newObject(ClassFont, font->family, NAME_bold, font->points, EAV);
  CharArray cmp = (CharArray)NAME_x;
  if ( getAdvanceFont(font, cmp) != getAdvanceFont(bold, cmp) )
  { Cprintf("Fonts need to have the same pitch\n");
    bold = NIL;
  }
  assign(ti, bold_font, bold);
  requestComputeGraphical(ti, DEFAULT);
  changedEntireImageGraphical(ti);

  succeed;
}

static status
backgroundTerminalImage(TerminalImage ti, Colour bg)
{ assign(ti, background, bg);
  return refreshTerminalImage(ti);
}

static status
selectionStyleTerminalImage(TerminalImage ti, Style sel)
{ assign(ti, selection_style, sel);
  return refreshTerminalImage(ti);
}

static status
ansiColoursTerminalImage(TerminalImage ti, Vector colours)
{ assign(ti, ansi_colours, colours);
  return refreshTerminalImage(ti);
}

static status
windowLabelTerminalImage(TerminalImage ti, CharArray label)
{ PceWindow sw = getWindowGraphical((Graphical)ti);
  if ( sw )
    return send(sw, NAME_label, label, EAV);
  fail;
}

static status
insertTerminalImage(TerminalImage ti, CharArray ca)
{ PceString s = &ca->data;

  if ( s->s_iswide )
  { for(size_t i=0; i<s->s_size; i++)
      rlc_putansi(ti->data, s->s_textW[i]);
  } else
  { for(size_t i=0; i<s->s_size; i++)
      rlc_putansi(ti->data, s->s_textA[i]);
  }
  rlc_update(ti->data);

  succeed;
}

static status
sendTerminalImage(TerminalImage ti, CharArray ca)
{ size_t ulen;
  const char *u8 = stringToUTF8(&ca->data, &ulen);

  return rlc_send(ti->data, u8, ulen) == ulen;
}

static Name
getPtyNameTerminalImage(TerminalImage ti)
{
#if HAVE_POSIX_OPENPT
  RlcData b = ti->data;

  if ( !b->pty.slave_name[0] )
  { if ( !rlc_open_pty_pair(b, 80, 25) )
      fail;
  }

  return CtoName(b->pty.slave_name);
#else
  return NIL;
#endif
}

		 /*******************************
		 *       TEMPORARY STUFF        *
		 *******************************/

static status
printTerminalImage(TerminalImage ti, Int start, Int count)
{ RlcData b = ti->data;
  Cprintf("################\n");
  Cprintf("# first = %d; last = %d; window_start = %d\n",
	  b->first, b->last, b->window_start);

  int from  = isDefault(start) ? b->window_start : valInt(start);
  int lines = isDefault(count) ? b->window_size  : valInt(count);

  Dprint_lines(b, from, rlc_add_lines(b, from, lines));

  succeed;
}

/* Type declarations */

static char *T_initialise[] =
{ "width=[int]", "height=[int]" };
static char *T_geometry[] =
{ "x=[int]", "y=[int]", "width=[int]", "height=[int]" };
static char *T_scrollVertical[] =
{ "direction={forwards,backwards,goto}",
  "unit={file,page,line}", "amount=int" };
static char *T_font[] =
{ "font=font", "bold=[font]" };
static char *T_print[] =
{ "start=[int]", "count=[int]" };

static vardecl var_terminal_image[] =
{ IV(NAME_bindings, "key_binding", IV_BOTH,
     NAME_accelerator, "key_binding table"),
  IV(NAME_font, "font", IV_GET,
     NAME_appearance, "Font used to draw the string"),
  IV(NAME_boldFont, "font*", IV_GET,
     NAME_appearance, "Font for bold text"),
  SV(NAME_background, "[colour]", IV_GET|IV_STORE, backgroundTerminalImage,
     NAME_appearance, "Terminal background colour"),
  SV(NAME_selectionStyle, "[style]", IV_GET|IV_STORE,
     selectionStyleTerminalImage,
     NAME_appearance, "Feedback for the selection"),
  SV(NAME_ansiColours, "vector*", IV_GET|IV_STORE, ansiColoursTerminalImage,
     NAME_appearance, "The 16 ansi colours"),
  IV(NAME_armedLink, "bool", IV_GET,
     NAME_event, "Hovering a link"),
  IV(NAME_linkMessage, "code*", IV_BOTH,
     NAME_event, "Hovering a link"),
  IV(NAME_scrollBar, "scroll_bar*", IV_BOTH,
     NAME_event, "Associated scroll_bar"),
  SV(NAME_saveLines, "int", IV_GET|IV_STORE, saveLinesTerminalImage,
     NAME_memory, "How many lines are saved for scroll back"),
  IV(NAME_syntax, "syntax_table", IV_BOTH,
     NAME_language, "Description of the used syntax"),
  IV(NAME_data, "alien:RlcData", IV_NONE,
     NAME_cache, "Line buffer and related data")
};

static senddecl send_terminal_image[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseTerminalImage,
     DEFAULT, "Create terminal_image from width and height and font"),
  SM(NAME_unlink, 0, NULL, unlinkTerminalImage,
     DEFAULT, "Destroy data"),
  SM(NAME_geometry, 4, T_geometry, geometryTerminalImage,
     DEFAULT, "Change geometry"),
  SM(NAME_font, 2, T_font, fontTerminalImage,
     NAME_appearance, "Set font and bold font"),
  SM(NAME_compute, 0, NULL, computeTerminalImage,
     NAME_repaint, "Recompute the terminal image"),
  SM(NAME_bubbleScrollBar, 1, "scroll_bar", bubbleScrollBarTerminalImage,
     NAME_scroll, "Update bubble of given scroll_bar object"),
  SM(NAME_scrollVertical, 3, T_scrollVertical, scrollVerticalTerminalImage,
     NAME_scroll, "Trap scroll_bar request"),
  SM(NAME_WantsKeyboardFocus, 0, NULL, succeedObject,
     NAME_event, "Test if ready to accept input (true)"),
  SM(NAME_event, 1, "event", eventTerminalImage,
     NAME_event, "Handle a general event"),
  SM(NAME_typed, 1, "event", typedTerminalImage,
     NAME_event, "Process a keystroke"),
  SM(NAME_copy, 1, "which=[{primary,clipboard}]", copyTerminalImage,
     NAME_selection, "Copy selected text to clipboard or primary selection"),
  SM(NAME_paste, 1, "which=[{primary,clipboard}]", pasteTerminalImage,
     NAME_selection, "Paste content of clipboard or primary selection"),
  SM(NAME_interrupt, 0, NULL, interruptTerminalImage,
     NAME_event, "Virtual method called on Ctrl-C"),
  SM(NAME_copyOrInterrupt, 0, NULL, copyOrInterruptTerminalImage,
     NAME_selection, "Copy if there is selected text; else interrupt"),
  SM(NAME_cursorEnd, 0, NULL, cursorEndTerminalImage,
     NAME_event, "Handle 'end'-key"),
  SM(NAME_cursorHome, 0, NULL, cursorHomeTerminalImage,
     NAME_event, "Handle 'home'-key"),
  SM(NAME_cursorPageUp, 0, NULL, cursorPageUpTerminalImage,
     NAME_event, "Handle 'page-up'-key"),
  SM(NAME_cursorPageDown, 0, NULL, cursorPageDownTerminalImage,
     NAME_event, "Handle 'page-down'-key"),
  SM(NAME_hasSelection, 0, NULL, hasSelectionTerminalImage,
     NAME_selection, "True if the image has a non-empty selection"),
  SM(NAME_selectAll, 0, NULL, selectAllTerminalImage,
     NAME_selection, "Select all text in the buffer"),
  SM(NAME_send, 1, "text=char_array", sendTerminalImage,
     NAME_insert, "Send text to the connected process"),
  SM(NAME_insert, 1, "text=char_array", insertTerminalImage,
     NAME_insert, "Insert text at caret (moves caret)"),
  SM(NAME_print, 2, T_print, printTerminalImage,
     NAME_debug, "Print content of the window"),
  SM(NAME_windowLabel, 1, "char_array", windowLabelTerminalImage,
     NAME_label, "Called on OSC 0 <window title>"),
#ifdef __WINDOWS__
  SM(NAME_launch, 1, "char_array", launchTerminalImage,
     NAME_process, "Run process in the terminal"),
#endif
};

static getdecl get_terminal_image[] =
{
  GM(NAME_ptyName, 0, "pty=name*", NULL, getPtyNameTerminalImage,
     NAME_process, "Path name for the pty"),
  GM(NAME_displayedCursor, 0, "cursor=cursor", NULL,
     getDisplayedCursorTerminalImage,
     NAME_event, "Indicate normal cursor or link"),
  GM(NAME_selected, 0, "string", NULL,
     getSelectedTerminalImage,
     NAME_selection, "New string with contents of selection")
};

static classvardecl rc_terminal_image[] =
{ RC(NAME_cursor, "cursor", "xterm",
     "Default cursor"),
  RC(NAME_linkCursor, "cursor", "pointer",
     "Default cursor when hovering a link"),
  RC(NAME_background, "colour", "white",
     "Colour of the background"),
  RC(NAME_colour, "colour", "black",
     "Colour for the foreground"),
  RC(NAME_selectionStyle, "[style]",
     UXWIN("style(background := yellow)",
	   "@_select_style"),
     "Style for <-selection"),
  RC(NAME_saveLines, "int", "1000",
     "How many lines are saved for scroll back"),
  RC(NAME_syntax, "[syntax_table]", "default",
     "Syntax definition"),
  RC(NAME_font, "font", "tt",
     "Default font"),
  RC(NAME_boldFont, "font*", "boldtt",
     "Bold font"),
  RC(NAME_ansiColours, "vector*",
     "vector("
     "colour(black),"
     "colour(red3),"
     "colour(green3),"
     "colour(yellow3),"
     "colour(blue2),"
     "colour(magenta3),"
     "colour(cyan3),"
     "colour(gray90),"
     /* Bright versions */
     "colour(gray50),"
     "colour(red),"
     "colour(green),"
     "colour(yellow),"
     "colour(blue),"		/* TBD: 92,92,255 */
     "colour(magenta),"
     "colour(cyan),"
     "colour(white)"
     ")",
     "The ANSI colours")
};

static Name terminal_image_termnames[] =
	{ NAME_width, NAME_height };

ClassDecl(terminal_image_decls,
          var_terminal_image, send_terminal_image, get_terminal_image,
	  rc_terminal_image,
          2, terminal_image_termnames,
          "$Rev$");

status
makeClassTerminalImage(Class class)
{ declareClass(class, &terminal_image_decls);

//  setCloneFunctionClass(class, cloneEditor);
  setRedrawFunctionClass(class, RedrawAreaTerminalImage);

  succeed;
}

		 /*******************************
		 *            MALLOC            *
		 *******************************/

static void *
rlc_malloc(size_t bytes)
{ return malloc(bytes);
}

static void *
rlc_realloc(void *ptr, size_t size)
{ return realloc(ptr, size);
}

static void
rlc_free(void *ptr)
{ free(ptr);
}

		 /*******************************
		 *       STRING FUNCTIONS       *
		 *******************************/

static size_t
ucslen(const uchar_t *s)
{ size_t len = 0;
  for( ; *s; s++ )
    len++;
  return len;
}

static void
ucscpy(uchar_t *dst, const uchar_t *src)
{ while(*src)
    *dst++ = *src++;
  *dst = 0;
}

#if 0
static void
ucsncpy(uchar_t *dst, const uchar_t *src, size_t len)
{ while(*src && len-- > 0)
    *dst++ = *src++;
  *dst = 0;
}
#endif

static int
cuncmp(const char *s1, const uchar_t *s2, size_t len)
{ const unsigned char *u1 = (const unsigned char*)s1;
  for(; len-- > 0; u1++, s2++)
  { int d = *u1 - *s2;
    if ( d )
      return d;
  }
  return 0;
}

static int
ucscmp(const uchar_t *s1, const uchar_t *s2)
{ while(*s1 == *s2 && *s1)
  { s1++;
    s2++;
  }

  return *s1-*s2;
}


static uchar_t *
ucstr(const uchar_t *haystack, const char *needle)
{ for(; *haystack; haystack++)
  { const unsigned char *n = (const unsigned char*)needle;
    const uchar_t *h = haystack;
    for(; *n; n++, h++)
    { if ( *n != *h )
	break;
      if ( !*h )
	return NULL;
    }
    if ( !*n )
      return (uchar_t*)haystack;
  }

  return NULL;
}


static Name
TCHAR2Name(const uchar_t *str)
{ char tmp[MAXLINE*4];
  char *u8 = tmp;

  char *o = u8;
  for(const uchar_t *s = str; *s; s++)
  { if ( o < tmp+sizeof(tmp)-8 )
      o = utf8_put_char(o, *s);
    else
      break;			/* for now, truncate  */
  }
  *o = 0;

  Name nm = UTF8ToName(tmp);
  return nm;
}

static StringObj
TCHAR2String(const uchar_t *str)
{ char tmp[MAXLINE*4];
  char *u8 = tmp;
  char *end = tmp+sizeof(tmp)-8;
  char *o;

again:
  o = u8;
  for(const uchar_t *s = str; *s; s++)
  { if ( o < end )
    { o = utf8_put_char(o, *s);
    } else
    { size_t len = ucslen(str);
      if ( !(u8=rlc_malloc((len+1)*4)) )
	fail;
      end = u8+len*4-8;
      goto again;
    }
  }
  *o = 0;

  StringObj s = UTF8ToString(u8);
  if ( u8 != tmp )
    rlc_free(u8);
  return s;
}


		 /*******************************
		 *         HANDLE INPUT         *
		 *******************************/

/**
 * Handle  an typed  character.  C-c  and C-v  are handled  here.  All
 * other characters are added to the queue of this terminal.
 */
static void
typed_char(RlcData b, int chr)
{ rlc_set_selection(b, 0, 0, 0, 0);

  DEBUG(NAME_term, Cprintf("Send %d to client\n", chr));
  char buf[6];
  char *e = utf8_put_char(buf, chr);
  size_t count = e-buf;
  if ( rlc_send(b, buf, count) != count )
    Cprintf("Failed to send %d\n", chr);
}


		 /*******************************
		 *	 CHARACTER TYPES	*
		 *******************************/

static bool
rlc_is_word_char(RlcData b, int chr)
{ TerminalImage ti = b->object;

  return tisalnum(ti->syntax, chr);
}


		 /*******************************
		 *	    SELECTION		*
		 *******************************/

#define SelLT(l1, c1, l2, c2) ((l1) < (l2) || ((l1) == (l2) && (c1) < (c2)))
#define SelEQ(l1, c1, l2, c2) ((l1) == (l2) && (c1) == (c2))

static int
rlc_min(RlcData b, int x, int y)
{ if ( rlc_count_lines(b, b->first, x) < rlc_count_lines(b, b->first, y) )
    return x;

  return y;
}


static int
rlc_max(RlcData b, int x, int y)
{ if ( rlc_count_lines(b, b->first, x) > rlc_count_lines(b, b->first, y) )
    return x;

  return y;
}


static void
rlc_changed_line(RlcData b, int i, int mask)
{ b->lines[i].changed |= mask;
}


static void
rlc_set_selection(RlcData b, int sl, int sc, int el, int ec)
{ int sch = rlc_min(b, sl, b->sel_start_line);
  int ech = rlc_max(b, el, b->sel_end_line);
  int nel = NextLine(b, el);
  int nsel= NextLine(b, b->sel_end_line);
  int i;
  int innow  = false;
  int insoon = false;

					/* find the lines that changed */
  for(i=sch; ; i = NextLine(b, i))
  { if ( i == sl )
    { insoon = true;
      if ( i == b->sel_start_line )
      { innow = true;
	if ( sc != b->sel_start_char ||
	     (i == el && i != b->sel_end_line) ||
	     (i == b->sel_end_line && i != el) )
	  rlc_changed_line(b, i, CHG_CHANGED);
      } else
	rlc_changed_line(b, i, CHG_CHANGED);
    } else if ( i == b->sel_start_line )
    { innow = true;
      rlc_changed_line(b, i, CHG_CHANGED);
    }

    if ( i == b->sel_end_line )
    { if ( (i == el && ec != b->sel_end_char) || el != i )
	rlc_changed_line(b, i, CHG_CHANGED);
    }

    if ( innow != insoon )
      rlc_changed_line(b, i, CHG_CHANGED);

    if ( i == nel )
    { insoon = false;
      if ( i == nsel )
	innow = false;
      else
	rlc_changed_line(b, i, CHG_CHANGED);
    } else if ( i == nsel )
    { innow = false;
      rlc_changed_line(b, i, CHG_CHANGED);
    }

    if ( i == ech )
      break;
  }

					/* update the attributes */
  b->sel_start_line = sl;
  b->sel_start_char = sc;
  b->sel_end_line   = el;
  b->sel_end_char   = ec;

					/* ... and request a repaint */
  rlc_request_redraw(b);
}


void
rlc_translate_mouse(RlcData b, int x, int y, int *line, int *chr)
{ int ln = b->window_start;
  int n = b->window_size;		/* # lines */
  RlcTextLine tl;
  x-= b->cw;				/* margin */

//  if ( !b->window )
//    return;

  while( y > b->ch && ln != b->last && n-- > 0 )
  { ln = NextLine(b, ln);
    y -= b->ch;
  }
  *line = ln;
  tl = &b->lines[ln];

  if ( b->fixedfont )
  { *chr = min(x/b->cw, tl->size);
  } else if ( tl->size == 0 )
  { *chr = 0;
  } else
  { text_char *s = tl->text;
    int f = 0;
    int t = tl->size;
    int m = (f+t)/2;
    int i;

    for(i=10; --i > 0; m=(f+t)/2)
    { int w;

      w = text_width(b, s, m);
      if ( x > w )
      { int cw = 0;

	if ( x < w+cw )
	{ *chr = m;
	  return;
	}
	f = m+1;
      } else
      { t = m;
      }
    }

    *chr = m;
  }
}


static void
rlc_start_selection(RlcData b, int x, int y)
{ int l, c;

  rlc_translate_mouse(b, x, y, &l, &c);
  b->sel_unit = SEL_CHAR;
  b->sel_org_line = l;
  b->sel_org_char = c;
  rlc_set_selection(b, l, c, l, c);
}

static const uchar_t *
rlc_clicked_link(RlcData b, int x, int y)
{ int l, c;

  rlc_translate_mouse(b, x, y, &l, &c);
  if ( b->sel_unit == SEL_CHAR &&
       b->sel_org_line == l &&
       b->sel_org_char == c )
  { RlcTextLine tl = &b->lines[l];
    for(href *hr=tl->links; hr; hr = hr->next)
    { if ( c >= hr->start && c <= hr->start + hr->length )
      { return hr->link;
      }
    }
  }

  return NULL;
}

static const uchar_t *
rlc_over_link(RlcData b, int x, int y)
{ int l, c;

  rlc_translate_mouse(b, x, y, &l, &c);
  { RlcTextLine tl = &b->lines[l];
    if ( c >= 0 && c < tl->size )
    { text_char *chr = &tl->text[c];
      if ( TF_LINK(chr->flags) )
      { //DEBUG(Dprintf(_T("On link at %d,%d\n"), l, c));
	for(href *hr=tl->links; hr; hr = hr->next)
	{ if ( c >= hr->start && c <= hr->start + hr->length )
	  { //DEBUG(Dprintf(_T("  link: %d(%d) -> \"%ls\"\n"),
	    //	    hr->start, hr->length, hr->link));
	    return hr->link;
	  }
	}
      }
    }
  }

  return NULL;
}

static int				/* v >= f && v <= t */
rlc_between(RlcData b, int f, int t, int v)
{ int h = rlc_count_lines(b, b->first, v);

  if ( h >= rlc_count_lines(b, b->first, f) &&
       h <= rlc_count_lines(b, b->first, t) )
    return true;

  return false;
}


static void
rlc_word_selection(RlcData b, int x, int y)
{ int l, c;

  rlc_translate_mouse(b, x, y, &l, &c);
  if ( rlc_between(b, b->first, b->last, l) )
  { RlcTextLine tl = &b->lines[l];

    if ( c < tl->size && rlc_is_word_char(b, tl->text[c].code) )
    { int f, t;

      for(f=c; f>0 && rlc_is_word_char(b, tl->text[f-1].code); f--)
	;
      for(t=c; t<tl->size && rlc_is_word_char(b, tl->text[t].code); t++)
	;
      rlc_set_selection(b, l, f, l, t);
    }
  }

  b->sel_unit = SEL_WORD;
}

static void
rlc_line_selection(RlcData b, int x, int y)
{ int l, c;

  rlc_translate_mouse(b, x, y, &l, &c);
  if ( rlc_between(b, b->first, b->last, l) )
  { RlcTextLine tl = &b->lines[l];

    rlc_set_selection(b, l, 0, l, tl->size);
  }

  b->sel_unit = SEL_LINE;
}

static void
rlc_extend_selection(RlcData b, int x, int y)
{ int l, c;
  int el = b->sel_org_line;
  int ec = b->sel_org_char;

  rlc_translate_mouse(b, x, y, &l, &c);
  if ( SelLT(l, c, b->sel_org_line, b->sel_org_char) )
  {				/* Backward */
    if ( b->sel_unit == SEL_WORD )
    { if ( rlc_between(b, b->first, b->last, l) )
      { RlcTextLine tl = &b->lines[l];

	if ( c < tl->size && rlc_is_word_char(b, tl->text[c].code) )
	  for(; c > 0 && rlc_is_word_char(b, tl->text[c-1].code); c--)
	    ;
      }
      if ( rlc_between(b, b->first, b->last, el) )
      { RlcTextLine tl = &b->lines[el];

	if ( ec < tl->size && rlc_is_word_char(b, tl->text[ec].code) )
	  for(; ec < tl->size && rlc_is_word_char(b, tl->text[ec].code); ec++)
	    ;
      }
    } else if ( b->sel_unit == SEL_LINE )
    { ec = b->width;
      c = 0;
    }
    rlc_set_selection(b, l, c, el, ec);
  } else if ( SelLT(b->sel_org_line, b->sel_org_char, l, c) )
  {				/* forward */
    if ( b->sel_unit == SEL_WORD )
    { if ( rlc_between(b, b->first, b->last, l) )
      { RlcTextLine tl = &b->lines[l];

	if ( c < tl->size && rlc_is_word_char(b, tl->text[c].code) )
	  for(; c < tl->size && rlc_is_word_char(b, tl->text[c].code); c++)
	    ;
      }
      if ( rlc_between(b, b->first, b->last, el) )
      { RlcTextLine tl = &b->lines[el];

	if ( ec < tl->size && rlc_is_word_char(b, tl->text[ec].code) )
	  for(; ec > 0 && rlc_is_word_char(b, tl->text[ec-1].code); ec--)
	    ;
      }
    } else if ( b->sel_unit == SEL_LINE )
    { ec = 0;
      c = b->width;
    }

    rlc_set_selection(b, el, ec, l, c);
  }
}

static void
rlc_select_all(RlcData b)
{ rlc_set_selection(b, b->first, 0, b->last, b->width);
}

static uchar_t *
rlc_read_from_window(RlcData b, int sl, int sc, int el, int ec)
{ int bufsize = 256;
  uchar_t *buf;
  int i = 0;

  if ( el < sl || (el == sl && ec < sc) )
    return NULL;			/* invalid region */
  if ( !(buf = rlc_malloc(bufsize * sizeof(uchar_t))) )
    return NULL;			/* not enough memory */

  for( ; ; sc = 0, sl = NextLine(b, sl))
  { RlcTextLine tl = &b->lines[sl];
    if ( tl )
    { int e = (sl == el ? ec : tl->size);

      if ( e > tl->size )
	e = tl->size;

      while(sc < e)
      { if ( i+1 >= bufsize )
	{ bufsize *= 2;
	  if ( !(buf = rlc_realloc(buf, bufsize * sizeof(uchar_t))) )
	    return NULL;		/* not enough memory */
	}
	buf[i++] = tl->text[sc++].code;
      }
    }

    if ( sl == el || sl == b->last )
    { buf[i++] = '\0';			/* Always room for the 0 */
      return buf;
    }

    if ( tl && !tl->softreturn )
    { if ( i+2 >= bufsize )
      { bufsize *= 2;
	if ( !(buf = rlc_realloc(buf, bufsize * sizeof(uchar_t))) )
	  return NULL;			/* not enough memory */
      }
      buf[i++] = '\r';			/* Bill ... */
      buf[i++] = '\n';
    }
  }
}


static bool
rlc_has_selection(RlcData b)
{ if ( SelEQ(b->sel_start_line, b->sel_start_char,
	     b->sel_end_line,   b->sel_end_char) )
    return false;
  return true;
}


static uchar_t *
rlc_selection(RlcData b)
{ if ( rlc_has_selection(b) )
    return rlc_read_from_window(b,
				b->sel_start_line, b->sel_start_char,
				b->sel_end_line,   b->sel_end_char);
  return NULL;
}


static bool
rlc_copy(RlcData b, Name to)	/* NAME_clipboard or NAME_primary */
{ uchar_t *sel = rlc_selection(b);

  if ( sel )
  { StringObj str = TCHAR2String(sel);
    addCodeReference(str);
    send(CurrentDisplay(b->object), NAME_selection, to, str, EAV);
    DEBUG(NAME_term, Cprintf("Copy to %s: \"%s\"\n", pp(to), pp(str)));
    considerPreserveObject(str);

    rlc_free(sel);
    return true;
  }

  return false;
}


		 /*******************************
		 *           REPAINT		*
		 *******************************/

static bool
rlc_caret_xy(RlcData b, int *x, int *y)
{ int line = rlc_count_lines(b, b->window_start, b->caret_y);

  if ( line < b->window_size )
  { *y = line * b->ch;
    if ( b->fixedfont )
    { *x = (b->caret_x + 1) * b->cw;
    } else
    { int tw;
      RlcTextLine tl = &b->lines[b->caret_y];
      tw = text_width(b, tl->text, b->caret_x);
      *x = b->cw + tw;
    }
    return true;
  }

  return false;
}

static void
rlc_draw_caret(RlcData b, int x, int y)
{ if ( b->caret_is_shown && !b->hide_caret )
  { Int h = getClassVariableValueClass(ClassTextCursor, NAME_height);

    double ols = h ? valNum(h) : 11;
    double cx = x + b->caret_px - ols/2.0;
    double cy = y + b->caret_py + b->cb - 3.0;

    DEBUG(NAME_caret, Cprintf("Drawing caret at %.1f,%.1f\n", cx, cy));
    draw_caret(cx, cy, ols, ols, b->has_focus);
  }
}


/* change the image area where the caret is */
static void
changed_caret(RlcData b)
{ if (  b->caret_is_shown )
  { Int h = getClassVariableValueClass(ClassTextCursor, NAME_height);

    double ols = h ? valNum(h) : 11;
    changedImageGraphical(b->object,
			  toNum(b->caret_px - ols/2.0),
			  toNum(b->caret_py + b->cb - 3),
			  toNum(ols), toNum(ols));
  }
}

static void
rlc_place_caret(RlcData b)
{ int x, y;

  if ( rlc_caret_xy(b, &x, &y) )
  { DEBUG(NAME_caret, Cprintf("Caret at %d,%d\n", x, y));
    if ( b->caret_is_shown && b->caret_px == x && b->caret_py == y )
    { return;
    } else if ( b->caret_is_shown )
    { changed_caret(b);
    }
    b->caret_is_shown = true;
    b->caret_px = x;
    b->caret_py = y;
    changed_caret(b);
  } else if ( b->caret_is_shown )
  { changed_caret(b);
    b->caret_is_shown = false;
  }
}


static void
rlc_update_scrollbar(RlcData b)
{ TerminalImage ti = b->object;
  if ( notNil(ti->scroll_bar) )
    requestComputeGraphical(ti->scroll_bar, DEFAULT);
}

static void
rlc_scroll_bubble(RlcData b, int *length, int *start, int *view)
{ int nsb_lines = rlc_count_lines(b, b->first, b->last);
  int nsb_start = rlc_count_lines(b, b->first, b->window_start);
  int nsb_view  = rlc_count_lines(b, b->window_start, b->last);
  if ( nsb_view > b->window_size )
    nsb_view = b->window_size;

  *length = nsb_lines;
  *start  = nsb_start;
  *view   = nsb_view;
}

static void
rlc_scroll_lines(RlcData b, int lines)
{ if ( lines == 0 )
    return;

  if ( lines > 0 )
  { for( ; lines && b->window_start != b->last; lines--)
      b->window_start = NextLine(b, b->window_start);
  } else
  { for( ; lines && b->window_start != b->first; lines++)
      b->window_start = PrevLine(b, b->window_start);
  }

  b->changed |= CHG_CARET|CHG_CLEAR|CHG_CHANGED;
  rlc_request_redraw(b);
}

/** Draw a line of the terminal
 */

static void
rcl_paint_text(RlcData b,
	       RlcTextLine tl, int from, int to,
	       int ty, int *cx, bool insel)
{ TerminalImage ti = b->object;
  text_char *chars, *s;
  text_char buf[MAXLINE];
  char text[MAXLINE*4];		/* UTF-8 */
  int len = to-from;
  int i;

  if ( len <= 0 )
    return;

  if ( tl->text && to <= tl->size )
  { chars = &tl->text[from];
  } else
  { text_char *o;
    int copy;

    o = chars = buf;
    s = &tl->text[from];
    copy = tl->text ? tl->size-from : 0;
    for(i=0; i<copy; i++)
      *o++ = *s++;
    for(; i<len; i++, o++)
    { o->code = ' ';
      o->flags = TF_DEFAULT;
    }
  }

  char *t;
  for(t=text, s=chars, i=0; i < len; i++, s++)
    t = utf8_put_char(t, s->code);
  *t = 0;

  if ( insel )					/* TBD: Cache */
  { Any ofg = r_colour(ti->selection_style->colour);
    Any obg = r_background(ti->selection_style->background);
    int x0 = *cx;
    *cx += tchar_width(b, text, t-text, len, ti->font);
    r_clear(x0, ty-b->cb, *cx-x0, b->ch);
    s_print_utf8(text, t-text, x0, ty, ti->font);
    r_colour(ofg);
    r_background(obg);
  } else
  { int start, segment, ulen;

    for(start=0, s=chars, t=text;
	start<len;
	start+=segment, s+=segment, t+=ulen)
    { text_flags flags = s->flags;
      int left = len-start;

      char *ut = t;
      for(segment=0;
	  segment<left && s[segment].flags == flags;
	  segment++)
      { int chr;		/* TODO: just skip UTF8 is easier */
	ut = utf8_get_char(ut, &chr);
      }
      ulen = ut-t;

      Colour ofg = DEFAULT;
      Colour obg = DEFAULT;
      int ifg = TF_FG(flags);
      int ibg = TF_BG(flags);
      if ( ifg != ANSI_COLOR_DEFAULT )
      { Colour fg = getElementVector(ti->ansi_colours, toInt(ifg+1));
	if ( fg )
	  ofg = r_colour(fg);
      }
      if ( ibg != ANSI_COLOR_DEFAULT )
      { Colour bg = getElementVector(ti->ansi_colours, toInt(ifg+1));
	if ( bg )
	  obg = r_background(bg);
      }
      if ( TF_INVERSE(flags) )
	r_swap_background_and_foreground();
      FontObj font = ti->font;
      if ( TF_BOLD(flags) )
      { if ( notNil(ti->bold_font) )
	  font = ti->bold_font;
	else
	  Cprintf("No bold font\n");
      }

      //Cprintf("Print \"%s\" at %d,%d using %s\n", t, *cx, ty, pp(ti->font));
      int x0 = *cx;
      *cx += tchar_width(b, t, ulen, segment, ti->font);
      r_clear(x0, ty-b->cb, *cx-x0, b->ch);
      s_print_utf8(t, ulen, x0, ty, font);
      if ( TF_UNDERLINE(flags) )
      { double o_pen = r_thickness(UNDERLINE_PEN);
	r_line(x0, ty+UNDERLINE_SEP, *cx, ty+UNDERLINE_SEP);
	r_thickness(o_pen);
      }

      if ( TF_INVERSE(flags) )
	r_swap_background_and_foreground();
      if ( notDefault(ofg) )
	r_colour(ofg);
      if ( notDefault(obg) )
	r_background(obg);
    }
  }
}

static void
rlc_redraw(RlcData b, int x, int y, int w, int h)
{ TerminalImage ti = b->object;
  int sl = 0;
  int el = b->window_size;
  int l = rlc_add_lines(b, b->window_start, sl);
  int pl = sl;				/* physical line */
  bool insel = false;			/* selected lines? */

  if ( rlc_count_lines(b, b->first, b->sel_start_line) <
       rlc_count_lines(b, b->first, l) &&
       rlc_count_lines(b, b->first, b->sel_end_line) >=
       rlc_count_lines(b, b->first, l) )
    insel = true;

  r_background(ti->background);

  for(; pl <= el; l = NextLine(b, l), pl++)
  { RlcTextLine tl = &b->lines[l];
    int ty = y + b->cb + b->ch * pl;
    int cx = x + b->cw;

    r_clear(x, ty-b->cb, b->cw, b->ch); /* clear margin */

					/* compute selection */
    if ( l == b->sel_start_line )
    { int cf = b->sel_start_char;
      int ce = (b->sel_end_line != b->sel_start_line ? b->width
						     : b->sel_end_char);

      rcl_paint_text(b, tl,  0, cf, ty, &cx, insel);
      insel = true;
      rcl_paint_text(b, tl, cf, ce, ty, &cx, insel);
      if ( l == b->sel_end_line )
      { insel = false;
	rcl_paint_text(b, tl, ce, b->width, ty, &cx, insel);
      } else
	insel = true;
    } else if ( l == b->sel_end_line )	/* end of selection */
    { int ce = b->sel_end_char;

      rcl_paint_text(b, tl, 0, ce, ty, &cx, insel);
      insel = false;
      rcl_paint_text(b, tl, ce, b->width, ty, &cx, insel);
    } else				/* entire line in/out selection */
    { rcl_paint_text(b, tl, 0, b->width, ty, &cx, insel);
    }

					/* clear remainder of line */
    if ( cx < x+b->width * (b->cw+1) )
    { r_clear(cx, y+b->ch*pl, x+w-cx, b->ch);
    }

    tl->changed = CHG_RESET;

    if ( l == b->last )			/* clear to end of window */
    { int yb = y+b->ch * (pl+1);
      r_clear(x, yb, w, h-yb);
      break;
    }
  }
  rlc_draw_caret(b, x, y);

  b->changed = CHG_RESET;

  rlc_update_scrollbar(b);
}


static void
rlc_request_redraw(RlcData b)
{ TerminalImage ti = b->object;

  if ( b->changed & CHG_CHANGED )
  { changedEntireImageGraphical(ti);
    rlc_place_caret(b);
  } else
  { int i = b->window_start;
    int y = 0;
    int ymin=0, ymax;
    bool first = true;

    for(; y < b->window_size; y++, i = NextLine(b, i))
    { RlcTextLine l = &b->lines[i];

      if ( l->changed & CHG_CHANGED )
      { // Cprintf("Line %p is changed\n", l);
	if ( first )
	{ ymin = y * b->ch;
	  ymax = ymin + b->ch;
	  first = false;
	} else
	{ ymax = (y+1) * b->ch;
	}
      }
      if ( i == b->last )
	break;
    }

    if ( !first )
    { changedImageGraphical(ti, ZERO, toInt(ymin),
			    ti->area->w, toInt(ymax-ymin));
    }
    if ( b->changed & CHG_CARET )
    { rlc_place_caret(b);
    }
  }
}


static bool
rlc_normalise(RlcData b)
{ if ( rlc_count_lines(b, b->window_start, b->caret_y) >= b->window_size )
  { b->window_start = rlc_add_lines(b, b->caret_y, -(b->window_size-1));
    b->changed |= CHG_CARET|CHG_CLEAR|CHG_CHANGED;
    rlc_request_redraw(b);
    return true;
  }

  return false;
}


static void
rlc_resize_pixel_units(RlcData b, int w, int h)
{ int nw = max(20, w/b->cw)-2;		/* 1 character space for margins */
  int nh = max(1, h/b->ch);

  DEBUG(NAME_term,
	Cprintf("rlc_resize_pixel_units(%p, %d, %d) (%dx%d)\n",
		b, w, h, nw, nh));

  if ( b->width == nw && b->window_size == nh )
    return;				/* no real change */

  rlc_resize(b, nw, nh);
  rlc_request_redraw(b);
  rlc_resize_pty(b, nw, nh);
}

		 /*******************************
		 *	       FONT		*
		 *******************************/

static void
rlc_init_text_dimensions(RlcData b, FontObj font)
{ b->cw = valNum(getAvgCharWidthFont(font));
  b->cb = s_ascent(font);
  b->ch = s_height(font);
  b->fixedfont = font->fixed_width == ON;
}


static int
text_width(RlcData b, const text_char *text, int len)
{ if ( b->fixedfont )
  { return len * b->cw;
  } else
  { TerminalImage ti = b->object;
    char tmp[MAXLINE*4];
    char *o = tmp;

    for(int i=0; i<len && o<&tmp[sizeof(tmp)-7]; i++)
      o = utf8_put_char(o, text[i].code);
    *o = 0;

    return str_advance_utf8(tmp, o-tmp, ti->font);
  }
}


/**
 * Determine the x-advance of printing `text`
 *
 * @param ulen is the length in bytes
 * @param len is the number of UTF-8 characters, __not__ bytes!
 */

static int
tchar_width(RlcData b, const char *text, size_t ulen, size_t len, FontObj font)
{ if ( b->fixedfont )
    return len * b->cw;
  else
    return str_advance_utf8(text, ulen, font);
}


		 /*******************************
		 *     BUFFER INITIALISATION	*
		 *******************************/

static RlcData
rlc_make_buffer(int w, int h)
{ RlcData b = rlc_malloc(sizeof(rlc_data));
  int i;

  memset(b, 0, sizeof(*b));
  b->magic = RLC_MAGIC;

  b->height         = h;
  b->width          = w;
  b->window_size    = 25;
  b->lines          = rlc_malloc(sizeof(rlc_text_line) * h);
  b->cmdstat	    = CMD_INITIAL;
  b->changed	    = CHG_CARET|CHG_CHANGED|CHG_CLEAR;
  b->sgr_flags	    = TF_DEFAULT;

  memset(b->lines, 0, sizeof(rlc_text_line) * h);
  for(i=0; i<h; i++)
    b->lines[i].adjusted = true;

  return b;
}

static void
rlc_destroy_buffer(RlcData b)
{ b->magic = 42;			/* so next gets errors */

  if ( b->lines )
  { int i;

    for(i=0; i<b->height; i++)
    { RlcTextLine tl = &b->lines[i];
      if ( tl->text )
	rlc_free(tl->text);
      href *links = tl->links;
      if ( links )
	rlc_free_links(links);
    }

    rlc_free(b->lines);
  }

  rlc_destroy_saved_screen(b);
  rlc_close_connection(b);

  free(b);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copy all lines one `back' (i.e.  towards   older  lines).  If the oldest
(first) line is adjacent to the last, throw it away.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
rlc_shift_lines_down(RlcData b, int line)
{ int i = b->first;
  int p = PrevLine(b, i);

  if ( p != b->last )			/* move first (oldest line) */
  { b->first = p;
    b->lines[p] = b->lines[i];
  } else				/* delete first (oldest) line */
    rlc_free_line(b, b->first);
					/* copy the lines */
  for(p=i, i = NextLine(b, i); p != line; p=i, i = NextLine(b, i))
  { b->lines[p] = b->lines[i];
    b->lines[p].line_no = p;
  }

  b->lines[line].text       = NULL;	/* make this one `free' */
  b->lines[line].links      = NULL;
  b->lines[line].size       = 0;
  b->lines[line].adjusted   = true;
  b->lines[line].softreturn = false;
}


static void
rlc_shift_lines_up(RlcData b, int line)
{ int prev = PrevLine(b, line);

  while(line != b->first)
  { b->lines[line] = b->lines[prev];
    b->lines[line].line_no = line;
    line = prev;
    prev = PrevLine(b, prev);
  }

  rlc_reinit_line(b, b->first);
  b->first = NextLine(b, b->first);
}

static void
unlink_href(RlcTextLine from, href *hr)
{ for(href **hp = &from->links; *hp; hp = &(*hp)->next)
  { if ( *hp == hr )
    { *hp = hr->next;
      return;
    }
  }
  assert(0);
}

static void
move_href(href *hr, RlcTextLine from, RlcTextLine to)
{ unlink_href(from, hr);
  hr->next = to->links;
  to->links = hr;
}

/* Update  links after  `moved` chars  were moved  from `l1`  to `l2`.
 * `l1` has all the links, `l2` has none as it is a fresh line.
 */
static void
update_links(RlcTextLine l1, RlcTextLine l2, int moved)
{ href *next;

  DEBUG(NAME_term,
	Dprint_links(l1, "l1");
	Dprint_links(l2, "l2"));
  for(href *hr = l1->links; hr; hr=next)
  { next = hr->next;

    if ( hr->start > l1->size )	/* move completely */
    { move_href(hr, l1, l2);
      hr->start -= l1->size;
    } else if ( hr->start + hr->length > l1->size )
    { rlc_add_link(l2, hr->link, 0, hr->start + hr->length - l1->size);
      hr->length = l1->size - hr->start;
    }
  }
  DEBUG(NAME_term,
	Dprint_links(l1, "l1 (after)");
	Dprint_links(l2, "l2 (after)"));

  rcl_check_links(l1);
  rcl_check_links(l2);
}

static void
move_link_positions(RlcTextLine tl, int offset)
{ for(href *hr = tl->links; hr; hr=hr->next)
    hr->start += offset;
}

static void
move_links(RlcTextLine from, RlcTextLine to)
{ href *next;

  DEBUG(NAME_term,
	Cprintf("Move links from %p to %p\n", from, to);
	Dprint_links(from, "On from");
	Dprint_links(to, "On to"));
  for(href *hr = from->links; hr; hr=next)
  { next = hr->next;
    unlink_href(from, hr);
    for(href *hr2 = to->links; hr2; hr2=hr2->next)
    { if ( hr->start+hr->length == hr2->start &&
	   ucscmp(hr->link, hr2->link) == 0 )
      {	DEBUG(NAME_term, Cprintf("Rejoin split link\n"));
	hr2->start = hr->start;
	hr2->length += hr->length;
	rlc_free_link(hr);
	goto next_link;
      }
    }
    DEBUG(NAME_term, Cprintf("Moved %p\n", hr));
    hr->next = to->links;
    to->links = hr;
  next_link:
    ;
  }

  DEBUG(NAME_term, Dprint_links(to, "After move"));
}

static void
move_links_soft(RlcTextLine from, RlcTextLine to)
{ href *next;

  DEBUG(NAME_term,
	Cprintf("Move links from %p to %p\n", from, to);
	Dprint_links(from, "On from");
	Dprint_links(to, "On to"));
  for(href *hr = from->links; hr; hr=next)
  { next = hr->next;
    if ( hr->start >= from->size )
    { unlink_href(from, hr);
      hr->start -= from->size;
      for(href *hr2 = to->links; hr2; hr2=hr2->next)
      { if ( hr->start+hr->length == hr2->start &&
	     ucscmp(hr->link, hr2->link) == 0 )
	{ DEBUG(NAME_term, Cprintf("Rejoin split link\n"));
	  hr2->start = hr->start;
	  hr2->length += hr->length;
	  rlc_free_link(hr);
	  goto next_link;
	}
      }
      DEBUG(NAME_term, Cprintf("Moved %p\n", hr));
      hr->next = to->links;
      to->links = hr;
    } else if ( hr->start + hr->length > from->size )
    { rlc_add_link(to, hr->link, 0, hr->start + hr->length - from->size);
      hr->length = from->size - hr->start;
    }
  next_link:
    ;
  }

  DEBUG(NAME_term,
	Dprint_links(from, "From after move");
	Dprint_links(to, "To after move"));
}


static void
rlc_resize(RlcData b, int w, int h)
{ int i;

  if ( b->width == w && b->window_size == h )
    return;				/* no real change */

  DEBUG(NAME_term,
	Cprintf("Resizing %dx%d --> %dx%d\n",
		b->width, b->window_size, w, h));

  b->window_size = h;
  b->width = w;

  for(i = b->first; /*i != b->last*/; i = NextLine(b, i))
  { RlcTextLine tl = &b->lines[i];

    if ( tl->text && tl->adjusted == false )
      rlc_adjust_line(b, i);

    DEBUG(NAME_term, Cprintf("%03d: sz=%d %s\n", i, tl->size,
			     tl->softreturn ? "(soft)" : ""));
    if ( tl->size > w )
    { DEBUG(NAME_term, Cprintf("  Truncate\n"));
      if ( !tl->softreturn )		/* hard --> soft */
      { DEBUG(NAME_term,
	      Cprintf("    hard -> soft\n");
	      Dprint_lines(b, i, i));
	rlc_shift_lines_down(b, i);
	DEBUG(NAME_term,
	      Cprintf("b->first = %d, b->last = %d\n", b->first, b->last));
	RlcTextLine pl = &b->lines[PrevLine(b, i)]; /* this is the moved line */
	int moved = pl->size - w;
	tl->text = rlc_malloc(moved*sizeof(text_char));
	memmove(tl->text, &pl->text[w], moved*sizeof(text_char));
	DEBUG(NAME_term,
	      Cprintf("Copied %d chars from line %d to %d\n",
		      moved, pl - b->lines, i));
	tl->size = moved;
	tl->adjusted = true;
	tl->softreturn = false;
	pl->softreturn = true;
	pl->text = rlc_realloc(pl->text, w * sizeof(text_char));
	pl->size = w;
	pl->adjusted = true;
	i = (int)(pl - b->lines);
	update_links(pl, tl, moved);
//	DEBUG(Dprint_lines(b, b->first, b->last));
      } else				/* put in next line */
      { RlcTextLine nl;
	int move = tl->size - w;

	DEBUG(NAME_term, Cprintf("    soft\n"));
	if ( i == b->last )
	  rlc_add_line(b);
	nl = &b->lines[NextLine(b, i)];
	nl->text = rlc_realloc(nl->text, (nl->size + move)*sizeof(text_char));
	memmove(&nl->text[move], nl->text, nl->size*sizeof(text_char));
	memmove(nl->text, &tl->text[w], move*sizeof(text_char));
	nl->size += move;
	tl->size = w;
	move_link_positions(nl, move);
	move_links_soft(tl, nl);
      }
    } else if ( tl->text && tl->softreturn && tl->size < w )
    { RlcTextLine nl;

      DEBUG(NAME_term, Cprintf("  Merge\n"));
      if ( i == b->last )
	rlc_add_line(b);
      nl = &b->lines[NextLine(b, i)];

      nl->text = rlc_realloc(nl->text,
			     (nl->size + tl->size)*sizeof(text_char));
      memmove(&nl->text[tl->size], nl->text, nl->size*sizeof(text_char));
      move_link_positions(nl, tl->size);
      memmove(nl->text, tl->text, tl->size*sizeof(text_char));
      move_links(tl, nl);

      nl->size += tl->size;
      nl->adjusted = true;
      rcl_check_links(nl);

      rlc_shift_lines_up(b, i);
    }

    if ( i == b->last )
      break;
  }

  for(i = NextLine(b, i); i != b->first; i = NextLine(b, i))
    rlc_free_line(b, i);

  if ( rlc_count_lines(b, b->first, b->last) < h )
    b->window_start = b->first;
  else
    b->window_start = rlc_add_lines(b, b->last, -(h-1));

  b->caret_y = b->last;
  b->caret_x = b->lines[b->last].size;

  b->changed |= CHG_CARET|CHG_CHANGED|CHG_CLEAR;

  rlc_check_assertions(b);
}


static void
rlc_reinit_line(RlcData b, int line)
{ RlcTextLine tl = &b->lines[line];
  memset(tl, 0, sizeof(*tl));
  tl->line_no = line;
  tl->adjusted = true;
}

static void
rlc_free_link(href *hr)
{ rlc_free(hr->link);
  rlc_free(hr);
}

static void
rlc_free_links(href *links)
{ href *next;

  for(; links; links=next)
  { next = links->next;
    rlc_free_link(links);
  }
}

static href*
rlc_copy_links(const href *links)
{ href *copy = NULL;
  href **tail = &copy;

  for( ; links; links = links->next )
  { href *new = rlc_malloc(sizeof(href));
    *tail = new;
    new->next = NULL;
    tail = &new->next;
    new->start = links->start;
    new->length = links->length;
    new->link = rlc_malloc((ucslen(links->link)+1)*sizeof(*new->link));
    ucscpy(new->link, links->link);
  }

  return copy;
}


static void
rlc_free_line(RlcData b, int line)
{ RlcTextLine tl = &b->lines[line];
  if ( tl->text )
  { rlc_free(tl->text);
    rlc_reinit_line(b, line);
  }
  href *links = tl->links;
  if ( links )
  { tl->links = NULL;
    rlc_free_links(links);
  }
}


static void
rlc_adjust_line(RlcData b, int line)
{ RlcTextLine tl = &b->lines[line];

  if ( tl->text && !tl->adjusted )
  { tl->text = rlc_realloc(tl->text, tl->size == 0
				? sizeof(text_char)
				: tl->size * sizeof(text_char));
    tl->adjusted = true;
  }
}


static void
rlc_unadjust_line(RlcData b, int line)
{ RlcTextLine tl = &b->lines[line];

  if ( tl->text )
  { if ( tl->adjusted )
    { tl->text = rlc_realloc(tl->text, (b->width + 1)*sizeof(text_char));
      tl->adjusted = false;
    }
  } else
  { tl->text = rlc_malloc((b->width + 1)*sizeof(text_char));
    tl->adjusted = false;
    tl->size = 0;
  }
}


static void
rlc_open_line(RlcData b)
{ int i = b->last;

  if ( i == b->sel_start_line )
    rlc_set_selection(b, 0, 0, 0, 0);	/* clear the selection */
  if ( i == b->first )
  { rlc_free_line(b, b->first);
    b->first = NextLine(b, b->first);
  }

  b->lines[i].text       = rlc_malloc((b->width + 1)*sizeof(text_char));
  b->lines[i].adjusted   = false;
  b->lines[i].size       = 0;
  b->lines[i].softreturn = false;
  b->lines[i].line_no    = i;
}


static void
rlc_add_line(RlcData b)
{ b->last = NextLine(b, b->last);
  rlc_open_line(b);
}

		 /*******************************
		 *	   CALCULATIONS		*
		 *******************************/

static int
rlc_count_lines(RlcData b, int from, int to)
{ if ( to >= from )
    return to-from;

  return to + b->height - from;
}


static int
rlc_add_lines(RlcData b, int here, int add)
{ here += add;
  while ( here < 0 )
    here += b->height;
  while ( here >= b->height )
    here -= b->height;

  return here;
}


		 /*******************************
		 *    ANSI SEQUENCE HANDLING	*
		 *******************************/

static void
rlc_need_arg(RlcData b, int arg, int def)
{ if ( b->argc < arg )
  { b->argv[arg-1] = def;
    b->argc = arg;
  }
}


static void
rlc_caret_up(RlcData b, int arg)
{ while(arg-- > 0 && b->caret_y != b->first)
    b->caret_y = PrevLine(b, b->caret_y);

  b->changed |= CHG_CARET;
}


/** Move caret up.  When already on first line, push screen down
 *  one line and clean first line.  Bound to `\eM`
 */

static void
rlc_reverse_index(RlcData b)
{ if ( b->caret_y == b->window_start )
  { int lines = rlc_count_lines(b, b->window_start, b->last)+1;
    int bottom;
    if ( lines > b->window_size )
    { bottom = rlc_add_lines(b, b->window_start, b->window_size-1);
    } else
    { bottom = NextLine(b, b->last);
      b->last = bottom;
    }

    rlc_free_line(b, bottom);
    while(bottom != b->window_start)
    { int before = PrevLine(b, bottom);
      b->lines[bottom] = b->lines[before];
      b->lines[bottom].changed |= CHG_CHANGED;
      b->lines[bottom].line_no = bottom;
      bottom = before;
    }
    rlc_reinit_line(b, bottom);
    b->lines[bottom].changed |= CHG_CHANGED;

    b->changed |= CHG_CARET|CHG_CLEAR|CHG_CHANGED;
  } else
  { b->caret_y = PrevLine(b, b->caret_y);
    b->changed |= CHG_CARET;
  }
}


static void
rlc_caret_down(RlcData b, int arg)
{ while ( arg-- > 0 )
  { if ( b->caret_y == b->last )
      rlc_add_line(b);
    b->caret_y = NextLine(b, b->caret_y);
    b->lines[b->caret_y].softreturn = false; /* ? why not only on open? */
  }
  b->changed |= CHG_CARET;
					/* scroll? */
  int row = rlc_count_lines(b, b->window_start, b->caret_y);
  if ( row >= b->window_size )
  { if ( b->saved.lines )
    { rlc_shift_up(b, row-(b->window_size-1));
      b->caret_y = rlc_add_lines(b, b->window_start, b->window_size-1);
    } else
    { b->window_start = rlc_add_lines(b, b->caret_y, -(b->window_size-1));
      b->changed |= CHG_CHANGED|CHG_CLEAR;
    }
  }

  rlc_check_assertions(b);
}


static void
rlc_caret_forward(RlcData b, int arg)
{ while(arg-- > 0)
  { if ( ++b->caret_x >= b->width )
    { b->lines[b->caret_y].softreturn = true;
      b->caret_x = 0;
      rlc_caret_down(b, 1);
    }
  }

  b->changed |= CHG_CARET;
}


static void
rlc_caret_backward(RlcData b, int arg)
{ while(arg-- > 0)
  { if ( b->caret_x-- == 0 )
    { rlc_caret_up(b, 1);
      b->caret_x = b->width-1;
    }
  }

  b->changed |= CHG_CARET;
}


static void
rlc_cariage_return(RlcData b)
{ b->caret_x = 0;

  b->changed |= CHG_CARET;
}


static void
rlc_tab(RlcData b)
{ RlcTextLine tl = &b->lines[b->caret_y];

  do
  { rlc_caret_forward(b, 1);
  } while( (b->caret_x % 8) != 0 );

  if ( tl->size < b->caret_x )
  { rlc_unadjust_line(b, b->caret_y);

    while ( tl->size < b->caret_x )
    { text_char *tc = &tl->text[tl->size++];

      tc->code = ' ';
      tc->flags = b->sgr_flags;
    }
  }

  b->changed |= CHG_CARET;
}


/**
 * @param x is the 0-based column
 * @param y is the 0-based row
 */

static void
rlc_set_caret(RlcData b, int x, int y)
{ int cy = rlc_count_lines(b, b->window_start, b->caret_y);

  y = Bounds(y, 0, b->window_size);

  if ( y < cy )
    b->caret_y = rlc_add_lines(b, b->window_start, y);
  else
    rlc_caret_down(b, y-cy);

  b->caret_x = Bounds(x, 0, b->width-1);

  b->changed |= CHG_CARET;
}


static void
rlc_set_caret_x(RlcData b, int x)
{ b->caret_x = Bounds(x-1, 0, b->width-1);

  b->changed |= CHG_CARET;
}


static void
rlc_save_caret_position(RlcData b)
{ b->scaret_y = rlc_count_lines(b, b->window_start, b->caret_y);
  b->scaret_x = b->caret_x;
}


static void
rlc_restore_caret_position(RlcData b)
{ rlc_set_caret(b, b->scaret_x, b->scaret_y);
}


static void
rlc_erase_saved_lines(RlcData b)
{ if ( b->last == b->window_start )
    b->window_start = b->first = b->last = 0;
  else
    b->first = b->window_start;
}

static void
rlc_erase_display(RlcData b)
{ if ( b->first == b->window_start ) /* no saved lines */
    b->window_start = b->first = b->last = 0;

  RlcTextLine tl = &b->lines[b->window_start];
  tl->size = 0;
  tl->adjusted = false;
  b->last = b->window_start;
  b->changed |= CHG_CHANGED|CHG_CLEAR|CHG_CARET;

  rlc_set_caret(b, 0, 0);
}


static void
rlc_erase_line(RlcData b)
{ RlcTextLine tl = &b->lines[b->caret_y];

  tl->size = b->caret_x;
  tl->changed |= CHG_CHANGED|CHG_CLEAR;
}

static void
rlc_clear_from_cursor(RlcData b)
{ rlc_erase_line(b);
  b->last = b->caret_y;
  b->changed |= CHG_CHANGED|CHG_CLEAR|CHG_CARET;
}

static void
rlc_sgr(RlcData b, int sgr)
{ if ( sgr == 0 )
  { b->sgr_flags = TF_DEFAULT;
  } else if ( sgr >= 30 && sgr <= 39 )
  { b->sgr_flags = TF_SET_FG(b->sgr_flags,
			     sgr == 39 ? ANSI_COLOR_DEFAULT : sgr-30);
  } else if ( sgr >= 40 && sgr <= 49 )
  { b->sgr_flags = TF_SET_BG(b->sgr_flags,
			     sgr == 49 ? ANSI_COLOR_DEFAULT : sgr-40);
  } else if ( sgr >= 90 && sgr <= 99 )
  { b->sgr_flags = TF_SET_FG(b->sgr_flags,
			     sgr == 99 ? ANSI_COLOR_DEFAULT : sgr-90+8);
  } else if ( sgr >= 100 && sgr <= 109 )
  { b->sgr_flags = TF_SET_BG(b->sgr_flags,
			     sgr == 109 ? ANSI_COLOR_DEFAULT : sgr-100+8);
  } else if ( sgr == 1 )
  { b->sgr_flags = TF_SET_BOLD(b->sgr_flags, 1);
  } else if ( sgr == 4 )
  { b->sgr_flags = TF_SET_UNDERLINE(b->sgr_flags, 1);
  } else if ( sgr == 7 )
  { b->sgr_flags = TF_SET_INVERSE(b->sgr_flags, 1);
  } else if ( sgr == 22 )	/* also clears "faint" */
  { b->sgr_flags = TF_SET_BOLD(b->sgr_flags, 0);
  } else if ( sgr == 24 )
  { b->sgr_flags = TF_SET_UNDERLINE(b->sgr_flags, 0);
  } else if ( sgr == 27 )
  { b->sgr_flags = TF_SET_INVERSE(b->sgr_flags, 0);
  }
}

static RlcTextLine
rlc_prepare_line(RlcData b, int y)
{ RlcTextLine tl = &b->lines[b->caret_y];
  text_char *tc;

  rlc_unadjust_line(b, b->caret_y);
  while( tl->size < b->caret_x )
  { tc = &tl->text[tl->size++];

    tc->code  = ' ';
    tc->flags = b->sgr_flags;
  }

  return tl;
}


static void
rlc_put(RlcData b, int chr)
{ RlcTextLine tl = rlc_prepare_line(b, b->caret_y);
  text_char *tc = &tl->text[b->caret_x];
  tc->code = chr;
  tc->flags = b->sgr_flags;
  if ( tl->size <= b->caret_x )
    tl->size = b->caret_x + 1;
  tl->changed |= CHG_CHANGED;

  rlc_caret_forward(b, 1);
}

static void
rlc_insert(RlcData b, int chr)
{ RlcTextLine tl = rlc_prepare_line(b, b->caret_y);
  if ( tl->size < b->width )
    tl->size++;
  for(int i=tl->size-1; i>b->caret_x; i--)
    tl->text[i] = tl->text[i-1];
  text_char *tc = &tl->text[b->caret_x];
  tc->code = chr;
  tc->flags = b->sgr_flags;
  tl->changed |= CHG_CHANGED;
}

static void
rlc_delete_chars(RlcData b, int count)
{ RlcTextLine tl = rlc_prepare_line(b, b->caret_y);
  if ( count > tl->size - b->caret_x )
    count = tl->size - b->caret_x;
  tl->size -= count;
  for(int i=b->caret_x; i<tl->size; i++)
    tl->text[i] = tl->text[i+count];
  tl->changed |= CHG_CHANGED;
}

static void
rcl_check_links(RlcTextLine tl)
{
#if _DEBUG
  int links = 0;

  for(int x=0; x<tl->size; x++)
  { if ( TF_LINK(tl->text[x].flags) )
    { int start = x;
      links++;
      for(; x<tl->size && TF_LINK(tl->text[x].flags); x++)
	;
      int len = x-start;
      href *hr;
      for(hr = tl->links; hr; hr=hr->next)
      { if ( hr->start == start && hr->length == len )
	  break;
      }
      if ( !hr )
      { Cprintf("CHECK: %03d could not find href for %d(%d); got: ",
		tl->line_no, start, len);
	for(hr = tl->links; hr; hr=hr->next)
	  Dprintf(_T(" %d(%d)"), hr->start, hr->length);
	Dprintf(_T("\n"));
      }
    }
  }

  int refs = 0;
  for(href *hr = tl->links; hr; hr=hr->next)
    refs++;
  if ( refs != links )
  { Dprintf("CHECK: %03d found %d links; expected %d\n",
	    tl->line_no, links, refs);
    Dprint_line(tl, true);
  }
#endif
}

static href *
rlc_add_link(RlcTextLine tl, const uchar_t *link, int start, int len)
{ href *hr = rlc_malloc(sizeof(*hr));

  hr->link = rlc_malloc((ucslen(link)+1)*sizeof(*link));
  ucscpy(hr->link, link);
  hr->start = start;
  hr->length = len;
  hr->next = tl->links;
  tl->links = hr;
  return hr;
}

static href *
rlc_register_link(RlcData b, const uchar_t *link, size_t len)
{ RlcTextLine tl = &b->lines[b->caret_y];
  return rlc_add_link(tl, link, b->caret_x, len);
}

		 /*******************************
		 *     SAVED SCREEN SUPPORT     *
		 *******************************/

/**
 * Handle scrolling up on a cursor down in saved-screen-mode.  In that
 * case we  do not push  the lines to the  saved lines, but  we simply
 * remove them.
 */

static void
rlc_shift_up(RlcData b, int shift)
{ int line = b->window_start;
  int last = rlc_add_lines(b, b->window_start, b->window_size-1);
  int src = rlc_add_lines(b, line, shift);

  if ( shift < b->window_size )
  { int done = 0;
    for(;;)
    { if ( done++ < shift )
	rlc_free_line(b, line);
      b->lines[line] = b->lines[src];
      b->lines[line].changed = CHG_CHANGED;
      if ( src == last )
	break;
      line = NextLine(b, line);
      src  = NextLine(b, src);
    }
  }

  do
  { line = NextLine(b, line);
    rlc_reinit_line(b, line);
  } while( line != last );

  b->changed |= CHG_CHANGED|CHG_CLEAR;
}


static void
rlc_destroy_saved_line(RlcTextLine tl)
{ if ( tl->text )
    rlc_free(tl->text);
  href *links = tl->links;
  if ( links )
    rlc_free_links(links);
}

static void
rlc_destroy_saved_screen(RlcData b)
{ RlcTextLine tls = b->saved.lines;

  if ( tls )
  { int count = b->saved.height;
    b->saved.lines = NULL;
    b->saved.height = 0;
    for(int i=0; i<count; i++)
      rlc_destroy_saved_line(&tls[i]);
    rlc_free(tls);
  }
}

static void
rlc_copy_line(RlcTextLine dst, const RlcTextLine src)
{ memset(dst, 0, sizeof(*dst));
  if ( src->size )
  { size_t bytes = src->size * sizeof(text_char);
    dst->text = rlc_malloc(bytes);
    dst->size = src->size;
    memcpy(dst->text, src->text, bytes);
    dst->changed = false;
    dst->softreturn = src->softreturn;
  }
  if ( src->links )
    dst->links = rlc_copy_links(src->links);
}

static void
rlc_save_screen(RlcData b)
{ rlc_destroy_saved_screen(b);
  int lines = rlc_count_lines(b, b->window_start, b->last);
  if ( lines > b->window_size )
    lines = b->window_size;
  b->saved.height = lines;
  b->saved.lines = rlc_malloc(sizeof(rlc_text_line) * lines);
  b->saved.caret_x = b->caret_x;
  b->saved.caret_y = rlc_count_lines(b, b->window_start, b->caret_y);
  int src = b->window_start;
  for(int i=0; i<lines; i++)
  { rlc_copy_line(&b->saved.lines[i], &b->lines[src]);
    src = NextLine(b, src);
  }
}


static void
rlc_restore_screen(RlcData b)
{ RlcTextLine tls = b->saved.lines;

  if ( tls )
  { int line = b->window_start;
    int count = b->saved.height;
    b->saved.lines = NULL;
    b->saved.height = 0;

    for(int i=0; i<count; i++)
    { if ( i < b->window_size )
      { rlc_free_line(b, line);
	RlcTextLine nl = &b->lines[line];
	*nl = tls[i];
	nl->line_no = line;
	nl->changed = CHG_CHANGED;
	if ( nl->size > b->width ) /* TODO: we should re-flush */
	{ nl->size = b->width;
	  nl->softreturn = false;
	  rlc_adjust_line(b, i);
	}
	line = NextLine(b, line);
      } else
      { rlc_destroy_saved_line(&tls[i]);
      }
    }
    rlc_free(tls);

    if ( rlc_count_lines(b, b->window_start, b->last) < count )
    { b->last = rlc_add_lines(b, b->window_start, count);
      rlc_free_line(b, b->last);
    }
    b->caret_x = Bounds(b->saved.caret_x, 0, b->width-1);
    b->caret_y = rlc_add_lines(b, b->window_start,
			       Bounds(b->saved.caret_y, 0, b->window_size));
  }
}

		 /*******************************
		 *      DEC PRIVATE MODES       *
		 *******************************/

/** Set/clear DEC primate modes.
 */
static void
rlc_set_dec_mode(RlcData b, int mode)
{ switch(mode)
  { case 1:
      b->app_escape = true;
      break;
    case 12:
      DEBUG(NAME_term, Cprintf("stub: enable blinking cursor\n"));
      break;
    case 25:
      b->hide_caret = false;
      changed_caret(b);
      break;
    case 1004:
      b->focus_inout_events = true;
      break;
    case 1049:
      rlc_save_screen(b);
      rlc_erase_display(b);
      break;
    case 2004:
      b->bracketed_paste_mode = true;
      break;
    default:
      Cprintf("Set unknown DEC private mode %d\n", mode);
  }
}

static void
rlc_clear_dec_mode(RlcData b, int mode)
{ switch(mode)
  { case 1:
      b->app_escape = false;
      break;
    case 12:
      DEBUG(NAME_term, Cprintf("stub: enable static cursor\n"));
      break;
    case 25:
      b->hide_caret = true;
      changed_caret(b);
      break;
    case 1004:
      b->focus_inout_events = false;
      break;
    case 1049:
      rlc_erase_display(b);
      rlc_restore_screen(b);
      break;
    case 2004:
      b->bracketed_paste_mode = false;
      break;
    default:
      Cprintf("Clear unknown DEC private mode %d\n", mode);
  }
}

static void
osc_command(RlcData b, int param, const uchar_t *link)
{ switch(param)
  { case 0:			/* should set title and icon name */
    case 2:			/* only sets title */
    { AnswerMark mark;
      StringObj s = TCHAR2String(link);
      markAnswerStack(mark);
      addCodeReference(s);
      send(b->object, NAME_windowLabel, s, EAV);
      delCodeReference(s);
      freeableObj(s);
      rewindAnswerStack(mark, NIL);
      break;
    }
    default:
      Cprintf("Unknown OSC command: %d\n", param);
  }
}

/** The "ST" sequence is either \e\\ or \a
 */
static bool
osc8_end(RlcData b)
{ const char *end1 = "\e]8;;\a";	/* new OSC 8 standard */
  const char *end2 = "\e]8;;\e\\";	/* old */
  const size_t end1l = strlen(end1);
  const size_t end2l = strlen(end2);

  int chr = b->link[b->link_len-1];
  if ( chr == '\a' &&
       b->link_len >= end1l &&
       cuncmp(end1, &b->link[b->link_len-end1l], end1l) == 0 )
  { b->link_len -= end1l;
    return true;
  }
  if ( chr == '\\' &&
       b->link_len >= end2l &&
       cuncmp(end2, &b->link[b->link_len-end2l], end2l) == 0 )
  { b->link_len -= end2l;
    return true;
  }

  return false;
}


static void
rlc_put_link(RlcData b, const uchar_t *label, const uchar_t *link)
{ text_flags flags0 = b->sgr_flags;
  rlc_sgr(b, 34);	/* blue */
  rlc_sgr(b, 4);	/* underline */
  int y = b->caret_y;
  href *hr = rlc_register_link(b, link, ucslen(label));
  b->sgr_flags = TF_SET_LINK(b->sgr_flags, true);
  for( ; *label; label++)
  { rlc_put(b, *label);
    if ( b->caret_y != y )	/* moved to next line */
    { size_t left = ucslen(label)-1;
      hr->length -= left;
      rcl_check_links(&b->lines[y]);
      hr = rlc_register_link(b, link, left);
      y = b->caret_y;
    }
  }
  rcl_check_links(&b->lines[y]);
  b->sgr_flags = flags0;
}

#ifdef _DEBUG
#define CMD(c) do {cmd = _T(#c); c;} while(0)
#else
#define CMD(c) do {c;} while(0)
#endif

static void
rlc_putansi(RlcData b, int chr)
{
#ifdef _DEBUG
  uchar_t *cmd;
#endif

  switch(b->cmdstat)
  { case CMD_INITIAL:
      switch(chr)
      { case '\b':
	  CMD(rlc_caret_backward(b, 1));
	  break;
        case 0x7:
	  //MessageBeep(MB_ICONEXCLAMATION);
	  send(b->object, NAME_flash, EAV);
	  break;
	case '\r':
	  CMD(rlc_cariage_return(b));
	  break;
	case '\n':
	  CMD(rlc_caret_down(b, 1));
	  break;
	case '\t':
	  CMD(rlc_tab(b));
	  break;
	case 27:			/* ESC */
	  b->cmdstat = CMD_ESC;
	  break;
	case 0x0F:			/* SI (Shift In) */
	  b->shift_in = true;
	  break;
	case 0x0E:			/* SO (Shift Out */
	  b->shift_in = false;
	  break;
	default:
	  CMD(rlc_put(b, chr));
	  break;
      }
      break;
    case CMD_ESC:
      switch(chr)
      { case '[':
	  b->cmdstat = CMD_ANSI;
	  b->argc    = 0;
	  b->argstat = 0;		/* no arg */
	  break;
	case ']':
	  b->cmdstat = CMD_OSC;
	  break;
	case '(':
	  b->cmdstat = CMD_G0;
	  break;
	case ')':
	  b->cmdstat = CMD_G1;
	  break;
	case 'M':
	  CMD(rlc_reverse_index(b));
	  b->cmdstat = CMD_INITIAL;
	  break;
	case '=':
	  b->app_keypad_mode = true;
	  b->cmdstat = CMD_INITIAL;
	  break;
	case '>':
	  b->app_keypad_mode = true;
	  b->cmdstat = CMD_INITIAL;
	  break;
	default:
	  Cprintf("ESC%c\n", chr);
	  b->cmdstat = CMD_INITIAL;
	  break;
      }
      break;
    case CMD_G0:
      switch(chr)
      { case 'B':
	  b->G0 = G_ASCII;
	  break;
	case '0':
	  b->G0 = G_GRAPHICS;
	  break;
      }
      b->cmdstat = CMD_INITIAL;
      break;
    case CMD_G1:
      switch(chr)
      { case 'B':
	  b->G1 = G_ASCII;
	  break;
	case '0':
	  b->G1 = G_GRAPHICS;
	  break;
      }
      b->cmdstat = CMD_INITIAL;
      break;
    case CMD_OSC:
      switch ( chr )
      { case '8':
	{ b->must_see = ";;";
	  b->cmdstat = CMD_LINK;
	  break;
	}
	default:
	{ if ( chr >= '0' && chr <= '9' )
	  { b->cmdstat = CMD_OSCARG;
	    b->argc    = 0;
	    b->argstat = 1;
	    b->argv[b->argc] = (chr - '0');
	    break;
	  }
	  b->cmdstat = CMD_INITIAL;
	}
      }
      break;
    case CMD_OSCTEXT:
      if ( b->link_len < ANSI_MAX_LINK-1 )
      { bool end = false;
	b->link[b->link_len++] = chr;
	b->link[b->link_len] = 0;
	if ( chr == '\a' )
	{ b->link_len--;
	  end = true;
	} else if ( chr == '\\' && b->link_len >= 2 &&
		    b->link[b->link_len-2] == '\e' )
	{ b->link_len -= 2;
	  end = true;
	}
	if ( end )
	{ b->link[b->link_len] = 0;
	  osc_command(b, b->argv[0], b->link);
	  b->cmdstat = CMD_INITIAL;
	}
	break;
      } else			/* too long, emit as text */
      { b->cmdstat = CMD_INITIAL;
	b->link[b->link_len] = 0;
	for(uchar_t *split=b->link; *split; split++)
	  rlc_put(b, *split);
      }
      break;
    case CMD_LINK:
      if ( b->must_see && b->must_see[0] == chr )
      { b->must_see++;
	if ( !b->must_see[0] )
	{ b->cmdstat = CMD_LINKARG;
	  b->link_len = 0;
	  b->must_see = NULL;
	}
	break;
      } else
      { b->cmdstat = CMD_INITIAL;
	break;
      }
    case CMD_LINKARG:
      if ( b->link_len < ANSI_MAX_LINK-1 )
      { const char *sep1   = "\a"; /* OSC8 "ST" sequence */
	const size_t sep1l = strlen(sep1);
	const char *sep2   = "\e\\";
	const size_t sep2l = strlen(sep2);

	b->link[b->link_len++] = chr;
	b->link[b->link_len] = 0;
	if ( osc8_end(b) )
	{ uchar_t *link;

	  b->cmdstat = CMD_INITIAL;
	  b->link[b->link_len] = 0;

	  uchar_t *label = ucstr(b->link, sep1);
	  if ( label )
	  { *label = 0;
	    label += sep1l;
	    link = b->link;
	  } else if ( (label=ucstr(b->link, sep2)) )
	  { *label = 0;
	    label += sep2l;
	    link = b->link;
	  } else
	  { label = b->link;
	    link = NULL;
	  }
	  DEBUG(NAME_term,
		Cprintf("Link: \"%ls\", Label \"%ls\"\n", link, label));
	  if ( link )
	  { rlc_put_link(b, label, link);
	  } else
	  { for( ; *label; label++)
	      rlc_put(b, *label);
	  }
	}
	break;
      } else			/* too long; process as text */
      { b->cmdstat = CMD_INITIAL;
	b->link[b->link_len] = 0;
	for(uchar_t *split=b->link; *split; split++)
	  rlc_put(b, *split);
	break;
      }
    case CMD_ANSI:			/* ESC [ */
    case CMD_DEC_PRIVATE:		/* ESC [ ? */
    case CMD_OSCARG:			/* ESC ] <digit> */
      if ( chr >= '0' && chr <= '9' )
      { if ( !b->argstat )
	{ b->argv[b->argc] = (chr - '0');
	  b->argstat = 1;		/* positive */
	} else
	{ b->argv[b->argc] = b->argv[b->argc] * 10 + (chr - '0');
	}

	break;
      }
      if ( !b->argstat && chr == '-' )
      { b->argstat = -1;		/* negative */
	break;
      }
      if ( b->argstat )
      { b->argv[b->argc] *= b->argstat;
	if ( b->argc < (ANSI_MAX_ARGC-1) )
	  b->argc++;			/* silently discard more of them */
	b->argstat = 0;
      }
      switch(chr)
      { case ';':
	  if ( b->cmdstat == CMD_OSCARG )
	  { b->cmdstat = CMD_OSCTEXT;
	    b->link_len = 0;
	  }
	  return;			/* wait for more args */
	case 'H':
	case 'f':
	{ rlc_need_arg(b, 1, 1); /* row */
	  rlc_need_arg(b, 2, 1); /* col */
	  int row = Bounds(b->argv[0], 1, b->window_size)-1;
	  int col = Bounds(b->argv[1], 1, b->width)-1;
	  CMD(rlc_set_caret(b, col, row));
	  break;
	}
	case 'd':
	{ rlc_need_arg(b, 1, 1); /* row */
	  int row = Bounds(b->argv[0], 1, b->window_size)-1;
	  int col = b->caret_x;
	  CMD(rlc_set_caret(b, col, row));
	  break;
	}
	case 'r':
	  if ( b->argc == 0 )
	  { DEBUG(NAME_term, Cprintf("Unlimit scroll\n"));
	  } else
	  { rlc_need_arg(b, 1, 1); /* row */
	    rlc_need_arg(b, 2, 1); /* col */
	    DEBUG(NAME_term, Cprintf("Limit scroll\n"));
	  }
	  break;
	case 'A':
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_caret_up(b, b->argv[0]));
	  break;
	case 'B':
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_caret_down(b, b->argv[0]));
	  break;
	case 'C':
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_caret_forward(b, b->argv[0]));
	  break;
	case 'D':
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_caret_backward(b, b->argv[0]));
	  break;
	case 'G':
	  rlc_need_arg(b, 1, 0);
	  CMD(rlc_set_caret_x(b, b->argv[0]));
	  break;
	case 's':
	  CMD(rlc_save_caret_position(b));
	  break;
	case 'u':
	  CMD(rlc_restore_caret_position(b));
	  break;
	case 'J':
	  rlc_need_arg(b, 1, 0);
	  if ( b->argv[0] == 0 )
	    CMD(rlc_clear_from_cursor(b));
	  else if ( b->argv[0] == 2 )
	    CMD(rlc_erase_display(b));
	  else if ( b->argv[0] == 3 )
	    CMD(rlc_erase_saved_lines(b));
	  else
	    Dprint_csi(b, chr);
	  break;
	case 'K':
	  CMD(rlc_erase_line(b));
	  break;
	case 'm':
	  { int i;
	    rlc_need_arg(b, 1, 0);

	    for(i=0; i<b->argc; i++)
	      CMD(rlc_sgr(b, b->argv[i]));
	    break;
	  }
	case '@':		/* \e[<n>@: insert <n> spaces */
	  rlc_need_arg(b, 1, 0);
	  for(int i=0; i<b->argv[0]; i++)
	    rlc_insert(b, ' ');
	  break;
	case 'P':		/* CSI Ps P — Delete Character(s) (DCH) */
	  rlc_need_arg(b, 1, 1);
	  CMD(rlc_delete_chars(b, b->argv[0]));
	  break;
	case '?':
	case '>':
	  b->cmdstat = CMD_DEC_PRIVATE;
	  return;
	case 'l':
	  if ( b->cmdstat == CMD_DEC_PRIVATE )
	  { rlc_need_arg(b, 1, 0);
	    rlc_clear_dec_mode(b, b->argv[0]);
	  }
	  break;
	case 'h':
	  if ( b->cmdstat == CMD_DEC_PRIVATE )
	  { rlc_need_arg(b, 1, 0);
	    rlc_set_dec_mode(b, b->argv[0]);
	  }
	  break;
	case 't':
	  if ( b->argv[0] == 22 )
	  { DEBUG(NAME_term, Cprintf("ESC[22;0;0t: un-minimize\n"));
	  } else if ( b->argv[0] == 23 )
	  { DEBUG(NAME_term, Cprintf("ESC[23;0;0t: minimize\n"));
	  } else if ( b->argv[0] == 8 )
	  { DEBUG(NAME_term, Cprintf("ESC[8;rows;colst: set size\n"));
	  } else
	    Dprint_csi(b, chr);
	  break;
	case 'n':
	  /* \e[6n: report row and column */
	  if ( b->argc == 1 && b->argv[0] == 6 )
	  { int row = rlc_count_lines(b, b->window_start, b->caret_y)+1;
	    int col = b->caret_x+1;
	    char buf[100];
	    snprintf(buf, sizeof(buf), "\e[%d;%dR", row, col);
	    rlc_send(b, buf, strlen(buf));
	  } else
	  { Dprint_csi(b, chr);
	  }
	  break;
	case 'c':		/* Identify as VT100+ANSI */
	{ const char *id = "\e[?1;2c";
	  rlc_send(b, id, strlen(id));
	  break;
	}
	default:
	  Dprint_csi(b, chr);
      }
      b->cmdstat = CMD_INITIAL;
  }

  rlc_check_assertions(b);
#ifdef _DEBUG
  (void)cmd;
#endif
}


		 /*******************************
		 *	LINE-READ SUPPORT	*
		 *******************************/

void
rlc_get_mark(rlc_console c, RlcMark m)
{ RlcData b = rlc_get_data(c);

  m->mark_x = b->caret_x;
  m->mark_y = b->caret_y;
}


void
rlc_goto_mark(rlc_console c, RlcMark m, const uchar_t *data, size_t offset)
{ RlcData b = rlc_get_data(c);

  b->caret_x = m->mark_x;
  b->caret_y = m->mark_y;

  for( ; offset-- > 0; data++ )
  { switch(*data)
    { case '\t':
	rlc_tab(b);
	break;
      case '\n':
	b->caret_x = 0;
        rlc_caret_down(b, 1);
	break;
      default:
	rlc_caret_forward(b, 1);
    }
  }
}


void
rlc_erase_from_caret(rlc_console c)
{ RlcData b = rlc_get_data(c);
  int i = b->caret_y;
  int x = b->caret_x;
  int last = rlc_add_lines(b, b->window_start, b->window_size);

  do
  { RlcTextLine tl = &b->lines[i];

    if ( tl->size != x )
    { tl->size = x;
      tl->changed |= CHG_CHANGED|CHG_CLEAR;
    }

    i = NextLine(b, i);
    x = 0;
  } while ( i != last );
}


void
rlc_putchar(rlc_console c, int chr)
{ RlcData b = rlc_get_data(c);

  rlc_putansi(b, chr);
}


uchar_t *
rlc_read_screen(rlc_console c, RlcMark f, RlcMark t)
{ RlcData b = rlc_get_data(c);
  uchar_t *buf;

  buf = rlc_read_from_window(b, f->mark_y, f->mark_x, t->mark_y, t->mark_x);

  return buf;
}


static void
rlc_update(rlc_console c)
{ RlcData b = rlc_get_data(c);

  if ( !rlc_normalise(b) )
    rlc_request_redraw(b);
}

static status
processClientOutputTerminalImage(TerminalImage ti,
			   const char *buf, ssize_t count)
{ RlcData b = ti->data;
  bool debug = false;
  DEBUG(NAME_term, debug = true);

  if ( count > 0 )
  { const char *i = buf;
    bool debug = false;
    if ( debug ) Cprintf("Received (%d bytes): ", count);
    while( i < &buf[count] )
    { int chr;
      i = utf8_get_char(i, &chr);
      rlc_putansi(b, chr);
      if ( debug ) Dprint_chr(chr);
    }
    if ( debug ) Cprintf("\n");
    rlc_update(b);
    succeed;
  } else if ( count == 0 )
  { if ( debug )
      Cprintf("%s: EOF\n", pp(ti));
  } else
  { if ( debug )
      Cprintf("%s: Error\n", pp(ti));
  }

  rlc_close_connection(b);
  fail;
}

/**
 * Prepare  the terminal's  client  streams by  setting the  encoding,
 * buffering and claim them to be tty streams.
 */

static void
set_stream_flags(IOSTREAM *s, unsigned int flags)
{ const unsigned int SIO_ABUF = (SIO_FBUF|SIO_LBUF|SIO_NBUF);
  s->flags &= ~SIO_ABUF;
  s->flags |= flags;
}

static void
set_stream_properties(IOSTREAM *i, IOSTREAM *o, IOSTREAM *e)
{ i->encoding = ENC_UTF8;
  o->encoding = ENC_UTF8;
  e->encoding = ENC_UTF8;

  i->flags &= ~(SIO_BOM);
  i->flags |= SIO_ISATTY;

  set_stream_flags(o, SIO_LBUF|SIO_ISATTY);
  set_stream_flags(e, SIO_NBUF|SIO_ISATTY);
}

#if HAVE_POSIX_OPENPT
		 /*******************************
		 *           PTY CODE           *
		 *******************************/

#include <fcntl.h>
#include <termios.h>
#include <sys/ioctl.h>

/**
 * Establish  a pty  pair between  the xpce  terminal and  the client.
 * Normally,  the client  is a  Prolog  thread, but  this design  also
 * allows  forking and  attaching  an arbitrary  process  to our  xpce
 * terminal.
 */

static bool
rlc_open_pty_pair(RlcData b, int cols, int rows)
{ memset(&b->pty, 0, sizeof(b->pty));

  b->pty.master_fd = posix_openpt(O_RDWR | O_NOCTTY);
  if ( b->pty.master_fd < 0 )
    return errorPce(b->object, NAME_cannotOpenPty);

  if ( grantpt(b->pty.master_fd) < 0 )
  { close(b->pty.master_fd);
    return errorPce(b->object, NAME_cannotGrantPty);
  }

  if ( unlockpt(b->pty.master_fd) < 0 )
  { close(b->pty.master_fd);
    return errorPce(b->object, NAME_cannotUnlockPty);
  }

  char *slave = ptsname(b->pty.master_fd);
#if 0
  if ( !slave )
  { close(b->pty.master_fd);
    return errorPce(b->object, NAME_cannotPtsname);
  }
#endif

  strncpy(b->pty.slave_name, slave, sizeof(b->pty.slave_name) - 1);
  b->pty.slave_fd = open(b->pty.slave_name, O_RDWR | O_NOCTTY);
  if ( b->pty.slave_fd < 0 )
  { close(b->pty.master_fd);
    return errorPce(b->object, NAME_cannotOpenPty);
  }
  b->pty.open = true;
  b->pty.watch = add_fd_to_watch(b->pty.master_fd, FD_READY_TERMINAL, b->object);

  return true;
}

static void
rlc_close_connection(RlcData b)
{ if ( b->pty.open )
  { if ( b->pty.watch )
    { remove_fd_watch(b->pty.watch);
      b->pty.watch = NULL;
    }
    if ( b->pty.master_fd >= 0 )
    { close(b->pty.master_fd);
      b->pty.master_fd = -1;
    }
    if ( b->pty.slave_fd >= 0 )	/* leave to the client? */
    { close(b->pty.slave_fd);
      b->pty.slave_fd = -1;
    }
    b->pty.open = false;
  }
}

static ssize_t
rlc_send(RlcData b, const char *buffer, size_t count)
{ if ( b->pty.master_fd )
  { return write(b->pty.master_fd, buffer, count);
  } else
  { Cprintf("Nowhere to send data\n");
    return -1;
  }
}

status
receiveTerminalImage(TerminalImage ti)
{ char buf[4096];
  RlcData b = ti->data;

  ssize_t count = read(b->pty.master_fd, buf, sizeof(buf));
  return processClientOutputTerminalImage(ti, buf, count);
}

static void
rlc_resize_pty(RlcData b, int cols, int rows)
{ if ( b->pty.open && b->pty.slave_name[0] )
  { int fd = open(b->pty.slave_name, O_RDWR|O_NOCTTY);
    if ( fd >= 0 )
    { struct winsize ws = {
        .ws_row = rows,
        .ws_col = cols,
        .ws_xpixel = valInt(b->object->area->w),
        .ws_ypixel = valInt(b->object->area->h)
      };
      if ( ioctl(fd, TIOCSWINSZ, &ws) == 0 )
      { DEBUG(NAME_term, Cprintf("Updated size to %dx%d\n", cols, rows));
      }
      close(fd);
    }
  }
}

bool
getPrologStreamTerminalImage(Any obj,
			     IOSTREAM **in, IOSTREAM **out, IOSTREAM **err)
{ TerminalImage ti = obj;

  if ( instanceOfObject(ti, ClassTerminalImage) )
  { RlcData b = ti->data;
    IOSTREAM *i = NULL;
    IOSTREAM *o = NULL;
    IOSTREAM *e = NULL;

    if ( !b->pty.open &&
	 !rlc_open_pty_pair(b, 80, 25) )
      return false;

    i = Sopen_file(b->pty.slave_name, "r");
    o = Sopen_file(b->pty.slave_name, "w");
    e = Sopen_file(b->pty.slave_name, "w");

    if ( i && o && e )
    { set_stream_properties(i,o,e);

      *in = i; *out = o; *err = e;
      return true;
    }

    if ( i ) Sclose(i);
    if ( o ) Sclose(o);
    if ( e ) Sclose(e);
  }

  return false;
}



#elif __WINDOWS__
#include <msw/mswin.h>
#define PIPE_SIZE 8192

static bool
rlc_create_pipes(RlcData b)
{ if ( b->ptycon.hIn && b->ptycon.hOut )
    return true;

  if ( !CreatePipeEx(&b->ptycon.hIn, &b->ptycon.hTaskOut, NULL,
		     PIPE_SIZE, 0, FILE_FLAG_OVERLAPPED) ||
       !CreatePipeEx(&b->ptycon.hTaskIn, &b->ptycon.hOut,  NULL,
		     PIPE_SIZE, FILE_FLAG_OVERLAPPED, 0) )
  { Cprintf("Failed to create ptyCon pipes\n");
    return false;
  }

  b->ptycon.watchIn  = add_pipe_to_watch(b->ptycon.hIn, FD_READY_TERMINAL,
					 b->object);
  b->ptycon.watchOut = add_out_pipe_to_watch(b->ptycon.hOut);

  return true;
}

static bool
rlc_open_pty_pair(RlcData b, int cols, int rows)
{ if ( !rlc_create_pipes(b) )
    return false;

  COORD size = { cols, rows };
  if ( CreatePseudoConsole(size, b->ptycon.hTaskIn, b->ptycon.hTaskOut,
			   0, &b->ptycon.hPC) != S_OK )
  { Cprintf("Failed to create PtyCon\n");
    return false;
  }

  DEBUG(NAME_term, Cprintf("Created PtyCon for %s\n", pp(b->object)));
  return true;
}

/**
 * Get the Prolog streams for the terminal I/O.
 */

bool
getPrologStreamTerminalImage(Any obj,
			     IOSTREAM **in, IOSTREAM **out, IOSTREAM **err)
{ TerminalImage ti = obj;

  if ( instanceOfObject(ti, ClassTerminalImage) )
  { RlcData b = ti->data;
    IOSTREAM *i = NULL;
    IOSTREAM *o = NULL;
    IOSTREAM *e = NULL;

    if ( !rlc_create_pipes(b) )
      return false;

    if ( !b->ptycon.hTaskError )
    { if ( !DuplicateHandle(GetCurrentProcess(), b->ptycon.hTaskOut,
			    GetCurrentProcess(), &b->ptycon.hTaskError,
			    0, TRUE, DUPLICATE_SAME_ACCESS) )
      { Cprintf("Failed to dup out to error\n");
	return false;
      }

      i = Swin_open_handle(b->ptycon.hTaskIn,    "r");
      o = Swin_open_handle(b->ptycon.hTaskOut,   "w");
      e = Swin_open_handle(b->ptycon.hTaskError, "w");

      if ( i && o && e )
      { set_stream_properties(i,o,e);
	b->ptycon.pl_streams[0] = i;
	b->ptycon.pl_streams[1] = o;
	b->ptycon.pl_streams[2] = e;

	*in = i; *out = o; *err = e;
	return true;
      }
    }

    if ( i ) Sclose(i);
    if ( o ) Sclose(o);
    if ( e ) Sclose(e);
  }

  return false;
}


static void
closeHandlePtr(HANDLE *ph)
{ HANDLE h = *ph;
  if ( h )
  { *ph = NULL;
    CloseHandle(h);
  }
}

static void
rlc_close_connection(RlcData b)
{ HANDLE h;
  if ( b->ptycon.watchIn )
  { remove_fd_watch(b->ptycon.watchIn);
    b->ptycon.watchIn = NULL;
  }
  if ( b->ptycon.watchOut )
  { remove_fd_watch(b->ptycon.watchOut);
    b->ptycon.watchOut = NULL;
  }
  if ( (h=b->ptycon.hPC) )
  { b->ptycon.hPC = NULL;
    ClosePseudoConsole(h);
  }
  closeHandlePtr(&b->ptycon.hIn);
  closeHandlePtr(&b->ptycon.hOut);
  closeHandlePtr(&b->ptycon.hTaskIn);
  closeHandlePtr(&b->ptycon.hTaskOut);
  closeHandlePtr(&b->ptycon.hTaskError);
  b->ptycon.pl_streams[0] = NULL;
  b->ptycon.pl_streams[1] = NULL;
  b->ptycon.pl_streams[2] = NULL;
}

static ssize_t
rlc_send(RlcData b, const char *buffer, size_t count)
{ DEBUG(NAME_term, Cprintf("Sending %zd bytes to %p\n",
			   count, b->ptycon.hOut));
  if ( b->ptycon.hOut )
  { return write_watch(b->ptycon.watchOut, buffer, count);
  } else
  { Cprintf("%s: nowhere to send data\n", pp(b->object));
    return -1;
  }
}

status
receiveTerminalImage(TerminalImage ti)
{ char buf[PIPE_READ_CHUNK];
  RlcData b = ti->data;
  ssize_t rc = read_watch(b->ptycon.watchIn, buf, sizeof(buf));

  return processClientOutputTerminalImage(ti, buf, rc);
}

static void
rlc_resize_pty(RlcData b, int cols, int rows)
{ if ( b->ptycon.hPC )
  { COORD size = { cols, rows };
    ResizePseudoConsole(b->ptycon.hPC, size);
  } else
  { if ( b->ptycon.pl_streams[1] )
      Ssetttysize(b->ptycon.pl_streams[1], cols, rows);
    DEBUG(NAME_term, Cprintf("%s: resized to %dx%d\n",
			     pp(b->object), cols, rows));
  }
}

static status
launchTerminalImage(TerminalImage ti, CharArray cmdline)
{ RlcData b = ti->data;
  STARTUPINFOEXW siEx = { .StartupInfo = { .cb = sizeof(siEx) } };
  SIZE_T attrSize = 0;

  if ( !b->ptycon.hPC &&
       !rlc_open_pty_pair(b, 80, 25) )
    fail;

  InitializeProcThreadAttributeList(NULL, 1, 0, &attrSize);
  siEx.lpAttributeList = HeapAlloc(GetProcessHeap(), 0, attrSize);
  if ( !InitializeProcThreadAttributeList(siEx.lpAttributeList,
					  1, 0, &attrSize) )
  { Cprintf("Failed to create thread attribute list\n");
    fail;
  }
  if ( !UpdateProcThreadAttribute(
	 siEx.lpAttributeList,
	 0,
	 PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
	 b->ptycon.hPC,
	 sizeof(b->ptycon.hPC),
	 NULL,
	 NULL))
  { Cprintf("Failed to set hPC in thread attribute list\n");
    fail;
  }

  PROCESS_INFORMATION piClient;
  BOOL ok = CreateProcessW(
    NULL,
    charArrayToWC(cmdline, NULL),
    NULL, NULL, TRUE,
    EXTENDED_STARTUPINFO_PRESENT,
    NULL, NULL,
    &siEx.StartupInfo,
    &piClient);

  DeleteProcThreadAttributeList(siEx.lpAttributeList);
  DEBUG(NAME_term, Cprintf("Started process %s on %s\n",
			   pp(cmdline), pp(ti)));
  return ok;
}

#endif

		 /*******************************
		 *         DEBUG PRINT          *
		 *******************************/

static void
Dprint_links(RlcTextLine tl, const char *msg)
{ if ( tl->links )
  { Cprintf("%03d %s:", tl->line_no, msg);
    for(href *hr = tl->links; hr; hr=hr->next)
      Cprintf(" %p = %d(%d) -> %ls", hr, hr->start, hr->length, hr->link);
    Cprintf("\n");
  }
}

static void
Dprint_line(RlcTextLine tl, bool links)
{ char buf[4096];
  char *o = buf;

  for(int x=0; x<tl->size && o<buf+sizeof(buf)-1; x++)
    o = utf8_put_char(o, tl->text[x].code);
  *o = EOS;
  Cprintf("%03d: (%03d) \"%s\"\n", tl->line_no, tl->size, buf);
  if ( links && tl->links )
    Dprint_links(tl, "  links");
}

static void
Dprint_lines(RlcData b, int from, int to)
{ for( ; ; from = NextLine(b, from))
  { Dprint_line(&b->lines[from], true);

    if ( from == to )
      break;
  }
}

static void
Dprint_chr(int chr)
{ if ( chr >= ' ' && chr <= 127 )
    Cprintf("%c", chr);
  else if ( chr == 27 )
    Cprintf("\\\\e");
  else if ( chr == 13 )
    Cprintf("\\\\r");
  else if ( chr == 10 )
    Cprintf("\\\\n");
  else if ( chr == 9 )
    Cprintf("\\\\t");
  else
    Cprintf("\\\\u%04x", chr);
}

static void
Dprint_csi(RlcData b, int chr)
{ Cprintf("Unknown ANSI CSI: \\\\e[");
  for(int i=0; i<b->argc; i++)
    Cprintf("%s%d", i==0?"":";", b->argv[i]);
  Cprintf("%c\n", chr);
}
