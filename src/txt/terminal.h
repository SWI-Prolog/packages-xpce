/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2025, University of Amsterdam
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

#ifndef _TERMINAL_H_INCLUDED
#define _TERMINAL_H_INCLUDED
#include "../sdl/sdlcolour.h"

typedef uint32_t uchar_t;

/* Marks a location in the line buffer
 */
typedef struct
{ int		mark_x;
  int		mark_y;
} rlc_mark, *RlcMark;

typedef struct rlc_data * rlc_console; /* they are the same; rename! */

typedef void	(*RlcUpdateHook)(void);	/* Graphics update hook */
typedef void	(*RlcTimerHook)(int);	/* Timer fireing hook */
typedef void	(*RlcRenderAllHook)(void); /* Render all formats */
typedef int	(*RlcMain)(rlc_console c, int, uchar_t**); /* main() */
typedef void	(*RlcInterruptHook)(rlc_console, int); /* Hook for Control-C */
typedef void	(*RlcResizeHook)(int, int); /* Hook for window change */
typedef void	(*RlcMenuHook)(rlc_console, const uchar_t *id); /* Hook for menu-selection */
typedef void	(*RlcFreeDataHook)(uintptr_t data); /* release data */
typedef bool	(*RlcLinkHook)(rlc_console, const uchar_t *); /* link href */

RlcUpdateHook	rlc_update_hook(RlcUpdateHook updatehook);
RlcTimerHook	rlc_timer_hook(RlcTimerHook timerhook);
#if TODO
RlcRenderHook   rlc_render_hook(RlcRenderHook renderhook);
#endif
RlcRenderAllHook rlc_render_all_hook(RlcRenderAllHook renderallhook);
RlcInterruptHook rlc_interrupt_hook(RlcInterruptHook interrupthook);
RlcResizeHook	rlc_resize_hook(RlcResizeHook resizehook);
RlcMenuHook	rlc_menu_hook(RlcMenuHook menuhook);
RlcLinkHook	rlc_link_hook(RlcLinkHook linkhook);
int		rlc_copy_output_to_debug_output(int docopy);

void		rlc_yield(void);
void		rlc_word_char(int chr, int isword);

size_t		rlc_read(rlc_console c, uchar_t *buf, size_t cnt);
size_t		rlc_write(rlc_console c, uchar_t *buf, size_t cnt);
int		rlc_close(rlc_console c);
int		rlc_flush_output(rlc_console c);

wchar_t	       *rlc_clipboard_text(rlc_console c);

int		getch(rlc_console c);
int		getche(rlc_console c);
int		getkey(rlc_console c);
int		kbhit(rlc_console c);
void		ScreenGetCursor(rlc_console c, int *row, int *col);
void		ScreenSetCursor(rlc_console c, int row, int col);
int		ScreenCols(rlc_console c);
int		ScreenRows(rlc_console c);

		 /*******************************
		 *	 LINE EDIT STUFF	*
		 *******************************/

/* Represent the line currently being typed in "cooked" mode
 */
typedef struct _line
{ rlc_mark	origin;			/* origin of edit */
  size_t	point;			/* location of the caret */
  size_t	size;			/* # characters in buffer */
  size_t	allocated;		/* # characters allocated */
  size_t	change_start;		/* start of change */
  int		complete;		/* line is completed */
  int		reprompt;		/* repeat the prompt */
  uchar_t	       *data;			/* the data (malloc'ed) */
  rlc_console	console;		/* console I belong to */
} rlc_line, *RlcLine;

#define COMPLETE_MAX_WORD_LEN 256
#define COMPLETE_MAX_MATCHES 100

#define COMPLETE_INIT	   0
#define COMPLETE_ENUMERATE 1
#define COMPLETE_CLOSE	   2

struct _complete_data;

typedef int (*RlcCompleteFunc)(struct _complete_data *);

typedef struct _complete_data
{ RlcLine	line;			/* line we are completing */
  int		call_type;		/* COMPLETE_* */
  int		replace_from;		/* index to start replacement */
  int		quote;			/* closing quote */
  int		case_insensitive;	/* if true: insensitive match */
  uchar_t		candidate[COMPLETE_MAX_WORD_LEN];
  uchar_t		buf_handle[COMPLETE_MAX_WORD_LEN];
  RlcCompleteFunc function;		/* function for continuation */
  void	       *ptr_handle;		/* pointer handle for client */
  intptr_t	num_handle;		/* numeric handle for client */
} rlc_complete_data, *RlcCompleteData;

RlcCompleteFunc rlc_complete_hook(RlcCompleteFunc func);

uchar_t	*read_line(rlc_console console);
int	rlc_complete_file_function(RlcCompleteData data);
void	rlc_init_history(rlc_console c, int size);
void	rlc_add_history(rlc_console c, const uchar_t *line);
bool	rlc_bind(int chr, const char *fname);
int	rlc_for_history(
		    rlc_console b,
		    int (*handler)(void *ctx, int no, const uchar_t *line),
		    void *ctx);

		 /*******************************
		 *	       HISTORY		*
		 *******************************/

/* a ring buffer that stores the history of commands typed into
 * the terminal
 */
typedef struct _history
{ int		size;			/* size of the history */
  int		tail;			/* oldest position */
  int		head;			/* newest position */
  int		current;		/* for retrieval */
  uchar_t **	lines;			/* the lines */
} history, *History;


		 /*******************************
		 *	    TERMINAL DATA	*
		 *******************************/

#define ANSI_MAX_ARGC     10		/* Ansi-escape sequence argv */
#define ANSI_MAX_LINK	4096		/* 4-K max URL length */
#define MAXPROMPT         80		/* max size of prompt */
#define OQSIZE		4096		/* output queue size */
#define MAX_USER_VALUES	  10		/* max user data-handles */

typedef struct lqueued
{ uchar_t *	  line;			/* Lines in queue */
  struct lqueued* next;			/* Next in queue */
} lqueued, *LQueued;

typedef unsigned short text_flags;

#define ANSI_COLOR_DEFAULT 31

/* Encode color and attributes in a 16 bit entity:
 *   - 5 bit (0..4) foreground color
 *   - 5 bit (5..9) background color
 *   - 1 bit (10)   bold
 *   - 1 bit (11)   underline
 *   - 1 bit (12)   inverse video
 *   - 1 bit (13)   (hyper)link
 */
#define TF_FG(f)	((f)&0x1f)	/* foreground */
#define TF_BG(f)	(((f)>>5)&0x1f)	/* background */
#define TF_BOLD(f)	((f)&(1<<10))	/* bold */
#define TF_UNDERLINE(f)	((f)&(1<<11))	/* underline */
#define TF_INVERSE(f)	((f)&(1<<12))	/* inverse video */
#define TF_LINK(f)	((f)&(1<<13))	/* inside a link */

#define TF_DEFAULT (ANSI_COLOR_DEFAULT | ANSI_COLOR_DEFAULT<<5)

#define TF_SET_FG(f,c)		(((f)&~0x1f)|(c))
#define TF_SET_BG(f,c)		(((f)&~(0x1f<<5))|((c)<<5))
#define TF_SET_BOLD(f,v)	(((f)&~(1<<10))|((v)<<10))
#define TF_SET_UNDERLINE(f,v)	(((f)&~(1<<11))|((v)<<11))
#define TF_SET_INVERSE(f,v)	(((f)&~(1<<12))|((v)<<12))
#define TF_SET_LINK(f,v)	(((f)&~(1<<13))|((v)<<13))

typedef struct
{ uchar_t		 code;			/* character code */
  text_flags	 flags;			/* flags for the text */
} text_char;

typedef struct href
{ uchar_t		*link;			/* Hyperlink target */
  int		 start;			/* start of label */
  int		 length;		/* #chars of label */
  struct href   *next;			/* Next in chain */
} href;

typedef struct
{ text_char     *text;			/* the storage */
  href          *links;			/* Hyperlinks */
  unsigned short size;			/* #characters in line */
  unsigned	 adjusted : 1;		/* line has been adjusted? */
  unsigned	 changed : 1;		/* line needs redraw */
  unsigned	 softreturn : 1;	/* wrapped line */
  int		 line_no;		/* The number of the line */
} rlc_text_line, *RlcTextLine;

typedef struct
{ uintptr_t	data;			/* the data itself */
  RlcFreeDataHook hook;			/* call when destroying console */
} user_data;

typedef enum
{ CMD_INITIAL = 0,
  CMD_ESC,
  CMD_G0,
  CMD_G1,
  CMD_ANSI,
  CMD_OSC,			/* \e] */
  CMD_OSCARG,			/* \e]<digit> */
  CMD_OSCTEXT,			/* \e]param; */
  CMD_LINK,			/* \e]8 */
  CMD_LINKARG,
  CMD_DEC_PRIVATE
} ansi_state;

typedef enum
{ G_ASCII = 0,
  G_GRAPHICS
} G_state;

#define RLC_MAGIC	0x3b75df1e	/* magic number to verify */

/* This struct holds all data related to the terminal image, i.e.,
   the lines, selection, etc.
 */
typedef struct rlc_data
{ int		magic;
  TerminalImage object;			/* Pointer back to object */
  int		height;			/* number of lines in buffer */
  int		width;			/* #characters ler line */
  int		first;			/* first line of ring */
  int		last;			/* last line of ring */
  int		caret_x;		/* cursor's x-position */
  int		caret_y;		/* its line */
  int		window_start;		/* start line of the window */
  int		window_size;		/* #lines on the window */
  RlcTextLine	lines;			/* the actual lines */
  struct				/* ESC ? 1049 [hl] */
  { RlcTextLine	lines;			/* The saved lines */
    int		height;			/* # lines saved */
    int		caret_x;		/* Caret location */
    int		caret_y;
  } saved;
  int		sel_unit;		/* SEL_CHAR, SEL_WORD, SEL_LINE */
  int		sel_org_line;		/* line origin of the selection */
  int		sel_org_char;		/* char origin of the selection */
  int		sel_start_line;		/* starting line for selection */
  int		sel_start_char;		/* starting char for selection */
  int		sel_end_line;		/* ending line for selection */
  int		sel_end_char;		/* ending char for selection */
  bool		app_escape;		/* Send ESC 0 instead of ESC [ */
  bool		app_keypad_mode;	/* Send ESC <N> p from keypad */
  ansi_state    cmdstat;		/* for parsing ANSI escape */
  int		argstat;		/* argument status ANSI */
  char const   *must_see;		/* \e]8;; link decoding */
  int		argc;			/* argument count for ANSI */
  int		argv[ANSI_MAX_ARGC];	/* argument vector for ANSI */
  uchar_t		link[ANSI_MAX_LINK];	/* Max URL length */
  bool		shift_in;		/* select G1 */
  G_state	G0;			/* Character set slot 0 */
  G_state	G1;			/* Character set slot 1 */
  int		link_len;		/* # chars in `link` */
  int		scaret_x;		/* saved-caret X */
  int		scaret_y;		/* saved-caret Y */
  bool		has_focus;		/* Application has the focus */
  bool		fixedfont;		/* Font is fixed */
  COLORRGBA	foreground;		/* Foreground (text) color */
  COLORRGBA	background;		/* Background color */
  COLORRGBA	sel_foreground;		/* Selection foreground */
  COLORRGBA	sel_background;		/* Selection background */
  COLORRGBA	ansi_color[16];		/* ANSI colors (8 normal + 8 bright) */
  text_flags	sgr_flags;		/* Current SGR flags */
  double	cw;			/* character width */
  int		ch;			/* character height */
  int		cb;			/* baseline */
  int		changed;		/* changes to the whole screen */
  int		sb_lines;		/* #lines the scrollbar thinks */
  int		sb_start;		/* start-line scrollbar thinks */
  bool		caret_is_shown;		/* is caret in the window? */
  bool		hide_caret;		/* DEC Private Mode 25 */
  bool		bracketed_paste_mode;	/* DEC Private Mode 2004 */
  bool		focus_inout_events;	/* Dec Private Mode 1004 */
  int		caret_px;		/* Position of the caret in pixels */
  int		caret_py;		/* Position of the caret in pixels */
#ifdef HAVE_POSIX_OPENPT
  struct
  { bool open;
    int  master_fd;			/* Terminal side */
    int  slave_fd;			/* Client side */
    char slave_name[128];		/* PTY name */
    FDWatch *watch;			/* Watch for write to terminal */
  } pty;
#else
  struct
  { HANDLE hPC;				/* The pseudo console */
    HANDLE hIn;				/* For reading from the process */
    HANDLE hOut;			/* For writing to the process */
    HANDLE hTaskIn;			/* The client read handle */
    HANDLE hTaskOut;			/* The client output handle */
    HANDLE hTaskError;			/* The client error handle */
    FDWatch *watchIn;			/* Watch on hIn */
    FDWatch *watchOut;			/* Watch on hOut rlc_send() */
    IOSTREAM *pl_streams[3];		/* Stdin/Stdout/Stderr */
  } ptycon;
#endif
} rlc_data, *RlcData;


		 /*******************************
		 *	    FUNCTIONS		*
		 *******************************/

int		rlc_at_head_history(RlcData b);
const uchar_t *	rlc_bwd_history(RlcData b);
const uchar_t *	rlc_fwd_history(RlcData b);
void		rlc_get_mark(rlc_console c, RlcMark mark);
void		rlc_goto_mark(rlc_console c, RlcMark mark,
			      const uchar_t *data, size_t offset);
void		rlc_erase_from_caret(rlc_console c);
void		rlc_putchar(rlc_console c, int chr);
uchar_t *		rlc_read_screen(rlc_console c,
				RlcMark from, RlcMark to);
const uchar_t *	rlc_prompt(rlc_console c, const uchar_t *prompt);
void		rlc_clearprompt(rlc_console c);


		 /*******************************
		 *	 INLINE FUNCTIONS	*
		 *******************************/

static __inline RlcData
rlc_get_data(rlc_console c)
{ if ( c )
  { RlcData b = c;

    assert(b->magic == RLC_MAGIC);
    if ( b->magic == RLC_MAGIC )
      return b;
  }

  return NULL;
}


#endif /* _TERMINAL_H_INCLUDED */
