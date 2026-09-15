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

#ifndef UCHAR_T_DEFINED
#define UCHAR_T_DEFINED
typedef uint32_t uchar_t;
#endif

typedef struct rlc_data * rlc_console; /* they are the same; rename! */

		 /*******************************
		 *	    TERMINAL DATA	*
		 *******************************/

#define ANSI_MAX_ARGC     10		/* Ansi-escape sequence argv */
#define ANSI_MAX_LINK	4096		/* 4-K max URL length */

/* Packed per-cell metadata (32 bits).  Indices `fg` and `bg` reference
 * the per-buffer color palette (see `palette` in rlc_data).  The sentinel
 * PAL_DEFAULT means "use the terminal/style default for this slot".
 *
 *   - width      : 2   display width (0=combining, 1=normal, 2=wide)
 *   - bold       : 1
 *   - underline  : 1
 *   - inverse    : 1
 *   - link       : 1   inside a hyperlink
 *   - strike     : 1   strikethrough (SGR 9 / crossed_out)
 *   - italic     : 1   slanted (SGR 3 / sitm)
 *   - fg         : 12  palette index, 4096 entries
 *   - bg         : 12  palette index, 4096 entries
 */
typedef union text_flags
{ uint32_t raw;				/* fast equality / memcpy */
  struct
  { unsigned width     : 2;
    unsigned bold      : 1;
    unsigned underline : 1;
    unsigned inverse   : 1;
    unsigned link      : 1;
    unsigned strike    : 1;
    unsigned italic    : 1;
    unsigned fg        : 12;
    unsigned bg        : 12;
  };
} text_flags;

#define PAL_DEFAULT 4095		/* sentinel: use default fg/bg */
#define PAL_LIMIT   4095		/* exclusive upper bound: valid 0..4094 */
#define PAL_ANSI_RESERVED 16		/* slots 0..15 mirror ti->ansi_colours */

#define TF_DEFAULT ((text_flags){ .fg = PAL_DEFAULT, .bg = PAL_DEFAULT })

typedef struct
{ uchar_t	 code;			/* character code */
  text_flags	 flags;			/* width + style + palette indices */
} text_char;

typedef struct href
{ uchar_t	*link;			/* Hyperlink target */
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
  unsigned	 eol_erased : 1;	/* paint the tail using eol_flags */
  unsigned	 folded : 1;		/* inside a closed fold: not painted */
  unsigned	 fold_head : 1;		/* the line that carries its marker */
  text_flags	 eol_flags;		/* background colour erase (bce) */
  int		 line_no;		/* The number of the line */
} rlc_text_line, *RlcTextLine;

/* Where the OSC 133 marks of one command landed.  A position is a ring
 * line and a cell on it; -1 for a mark that has not arrived.  This hangs
 * off a terminal_block (see ../h/text.h) rather than living in the ring,
 * because it must outlive the lines being rewrapped under it: rlc_resize()
 * carries it across as an rlc_textpos, exactly as it does the caret and
 * the selection.
 */

typedef struct rlc_anchors
{ int	prompt_line, prompt_char;	/* `A': the prompt starts here */
  int	input_line,  input_char;	/* `B': and the line the user edits */
  int	output_line, output_char;	/* `C': it was entered; output follows */
  int	end_line,    end_char;		/* `D': and the output ends here */
  bool	continued;			/* `A;k=s': typed over several lines */
  int	cont_char;			/* where its later lines start */
  int	hidden_lines;			/* rows the fold hides, if folded */
} rlc_anchors, *RlcAnchors;

typedef enum
{ CMD_INITIAL = 0,
  CMD_ESC,
  CMD_G0,
  CMD_G1,
  CMD_ANSI,
  CMD_OSC,			/* \e] */
  CMD_OSCARG,			/* \e]<digit> */
  CMD_OSCTEXT,			/* \e]param; */
  CMD_DEC_PRIVATE,
  CMD_CSI_INTERMEDIATE,		/* CSI param ... <intermediate 0x20-0x2F>+ */
  CMD_DCS,			/* \eP <body> ST */
  CMD_DCS_ESC			/* saw ESC inside DCS, waiting for '\\' */
} ansi_state;

typedef enum
{ G_ASCII = 0,
  G_GRAPHICS
} G_state;

#define RLC_MAGIC	0x3b75df1e	/* magic number to verify */
#define MAX_INCOMPLETE	5		/* Max buffered chars */

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
  int		scroll_top;		/* DECSTBM: first row of the */
  int		scroll_bottom;		/* scrolling region and its last */
  RlcTextLine	lines;			/* the actual lines */
  struct				/* ESC ? 1049 [hl] */
  { RlcTextLine	lines;			/* The saved lines */
    int		height;			/* # lines saved */
    int		caret_x;		/* Caret location */
    int		caret_y;
  } saved;
  int		sel_unit;		/* SEL_CHAR, SEL_WORD, SEL_LINE */
  bool		sel_word;		/* Match this selection as a word */
  int		sel_org_line;		/* line origin of the selection */
  int		sel_org_char;		/* char origin of the selection */
  int		sel_start_line;		/* starting line for selection */
  int		sel_start_char;		/* starting char for selection */
  int		sel_end_line;		/* ending line for selection */
  int		sel_end_char;		/* ending char for selection */
  int		drag_x;			/* last pointer position of a */
  int		drag_y;			/* selection drag */
  struct				/* Incremental search; the hit is */
  { int	origin_line;			/* the selection, so all we keep is */
    int	origin_char;			/* where the search started ... */
    int	base_line;			/* ... where changing the string */
    int	base_char;			/* looks from ... */
    int	window_start;			/* ... and the view it started from */
    bool seeded;			/* Started from a selection, which */
    int	held_start_line;		/* ^G gives back along with the */
    int	held_start_char;		/* view */
    int	held_end_line;
    int	held_end_char;
  } isearch;
  bool		app_escape;		/* Send ESC 0 instead of ESC [ */
  bool		app_keypad_mode;	/* Send ESC <N> p from keypad */
  bool		insert_mode;		/* ANSI mode 4 (IRM) */
  bool		autowrap;		/* DEC Private Mode 7 (DECAWM) */
  int		last_char;		/* last printed char, for REP */
  ansi_state    cmdstat;		/* for parsing ANSI escape */
  int		argstat;		/* argument status ANSI */
  int		csi_intermediate;	/* CSI intermediate byte, 0 if none */
  int		csi_private;		/* CSI private marker ? or >, 0 if none */
  int		argc;			/* argument count for ANSI */
  int		argv[ANSI_MAX_ARGC];	/* argument vector for ANSI */
  uchar_t		link[ANSI_MAX_LINK];	/* Max URL length */
  uchar_t      *link_url;		/* Open OSC 8 hyperlink or NULL */
  href	       *armed_href;		/* href the mouse is hovering, or NULL */
  bool		shift_in;		/* select G1 */
					/* HTS/TBC tab stops.  Columns past
					   the map (a line holds at most
					   MAXLINE of them) stop every 8 */
  unsigned char	tabs[128];
  G_state	G0;			/* Character set slot 0 */
  G_state	G1;			/* Character set slot 1 */
  int		link_len;		/* # chars in `link` */
  int		scaret_x;		/* saved-caret X (CSI s/u) */
  int		scaret_y;		/* saved-caret Y */
  struct				/* ESC 7 / ESC 8 (DECSC/DECRC) */
  { int		x;			/* visual column */
    int		y;			/* row in the window */
    text_flags	sgr;			/* SGR in effect */
    G_state	G0;			/* character set slots */
    G_state	G1;
    bool	shift_in;		/* G1 selected */
    bool	saved;			/* ESC 7 was seen */
  } cursor;
  bool		has_focus;		/* Application has the focus */
  COLORRGBA    *palette;		/* per-buffer color palette */
  struct colour **palette_obj;		/* owned locked Colour per slot >=16 */
  uint32_t	palette_size;		/* live entries (>= PAL_ANSI_RESERVED) */
  uint32_t	palette_alloc;		/* capacity */
  struct pal_hash *palette_hash;	/* COLORRGBA -> index for intern */
  bool		palette_full;		/* sticky once nearest-fallback engages */
  text_flags	sgr_flags;		/* Current SGR flags */
  double	cw;			/* character width */
  int		ch;			/* character height */
  int		cb;			/* baseline */
  int		changed;		/* changes to the whole screen */
  bool		caret_is_shown;		/* is caret in the window? */
  bool		hide_caret;		/* DEC Private Mode 25 */
  bool		bracketed_paste_mode;	/* DEC Private Mode 2004 */
  bool		prompt_marks;		/* client marks its prompts */
  bool		input_active;		/* OSC 133: between B and C */
  bool		input_continued;	/* OSC 133: the last A was k=s */
  int		input_line;		/* OSC 133 B: where the input */
  int		input_char;		/* the user edits starts */
  int		next_block_id;		/* <-id of the next terminal_block */
  int		folds;			/* # closed folds; 0 is the fast path */
  bool		focus_inout_events;	/* Dec Private Mode 1004 */
  bool		alt_scroll;		/* DEC Private Mode 1007 */
  int		mouse_tracking;		/* DEC Private Mode 9/1000/1002/1003 */
  int		mouse_encoding;		/* DEC Private Mode 1005/1006/1015 */
  int		mouse_col;		/* cell the last motion reported */
  int		mouse_row;
  int		caret_px;		/* Position of the caret in pixels */
  int		caret_py;		/* Position of the caret in pixels */
  unsigned char	incomplete_cnt;		/* # incomplete chars */
  char		incomplete[MAX_INCOMPLETE]; /* Incomplete sequences */
#ifdef HAVE_POSIX_OPENPT
  struct
  { bool      open;
    int       master_fd;		/* Terminal side */
    int       slave_fd;			/* Client side */
    char      slave_name[128];		/* PTY name */
    int	      client_fd[3];		/* Client stdin/stdout/stderr */
    FDWatch  *watch;			/* Watch for write to terminal */
    bool      has_client_thread;
    pthread_t client_thread;		/* Thread that opened this terminal */
  } pty;
#else
  struct
  { HANDLE hPC;				/* The pseudo console */
    int	   hPC_refs;			/* Clients that claimed hPC */
    bool   hPC_ours;			/* ... and we made it for them */
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
