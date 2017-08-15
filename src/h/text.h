/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

#ifndef _PCE_TXT_INCLUDED
#define _PCE_TXT_INCLUDED

#include <h/graphics.h>

		/********************************
		*          TEXT CLASSES		*
		********************************/


#define TXT_X_MARGIN 5		/* Margin between text and box of textimage */
#define TXT_Y_MARGIN 2

#define TXT_UNDERLINED	0x1	/* underlined character */
#define TXT_HIGHLIGHTED	0x2	/* highlighted character (inverse video) */
#define TXT_GREYED	0x4	/* greyed character */
#define TXT_BOLDEN	0x8	/* bold character */
#define TXT_HIDDEN	0x10	/* invisible fragment */

NewClass(text_buffer)
  ABSTRACT_SOURCE_SINK
  Fragment	first_fragment;		/* first fragment */
  Fragment	last_fragment;		/* last fragment */
  Chain		editors;		/* editors associated buffer */
  BoolObj	modified;		/* has textbuffer been modified? */
  Int		undo_buffer_size;	/* Size of the undo-buffer */
  SyntaxTable	syntax;			/* Syntax description */
  BoolObj	indent_tabs;		/* Indent with tabs? */
  Int		generation;		/* Increments on each change */
					/* start private data */
  intptr_t	changed_start;		/* start of changed region */
  intptr_t	changed_end;		/* end of changed region */
  intptr_t	gap_start;		/* first location of the gap */
  intptr_t	gap_end;		/* last location of the gap */
  intptr_t	size;			/* # characters in buffer */
  intptr_t	lines;			/* total number of lines */
  intptr_t	allocated;		/* allocated size */
  UndoBuffer	undo_buffer;		/* Undo log */
  string	buffer;			/* Actual buffer (with gap) */
End;

#define tb_bufferA buffer.text_union.textA
#define tb_bufferW buffer.text_union.textW

#define FRAG_INCLUDES_START	0x1	/* <-start is included */
#define FRAG_INCLUDES_END	0x2	/* <-end is included */

NewClass(fragment)
  TextBuffer	textbuffer;		/* text buffer fragment associated */
  Fragment	next;			/* next fragment */
  Fragment	prev;			/* previous fragment */
  Name		style;			/* style of fragment (via editor) */
  intptr_t	start;			/* start of fragment */
  intptr_t	length;			/* length of fragment (> 0) */
  intptr_t	attributes;		/* FRAG_... */
End;

NewClass(style)
  FontObj	font;			/* font of fragment */
  Colour	colour;			/* colour of fragment */
  Any		background;		/* Background for drawing */
  Image		icon;			/* margin marker */
  Int		left_margin;		/* left margin in pixels */
  Int		right_margin;		/* right margin in pixels */
  intptr_t	attributes;		/* style attributes */
End;

typedef struct
{ enum
  { TXT_FRAGMENT_START,
    TXT_FRAGMENT_END,
    TXT_FRAGMENT_CHAR
  } type;

  union
  { Fragment fragment;
    int	     character;
  } value;
} text_event;

typedef int (*TextEventFunction)(text_event *event);


typedef struct fragment_cache *FragmentCache;
typedef struct isearch_cache  *ISearchCache;

NewClass(editor)
  ABSTRACT_DEVICE			/* abstract super class device */
  TextBuffer	text_buffer;		/* Buffer editor operates on */
  TextImage	image;			/* The text area */
  ScrollBar	scroll_bar;		/* The scrollbar */
  TextMargin	margin;			/* The margin */
  TextCursor	text_cursor;		/* The cursor */
  TextObj	label_text;		/* Text for the label */
  FontObj	font;			/* editors default font */
  Size		size;			/* Size in characters */
  Int		caret;			/* position of the caret */
  Int		mark;			/* position of the marker */
  Name		mark_status;		/* active,inactive,highlight */
  Vector	mark_ring;		/* ring of old marks */
  Int		tab_distance;		/* distance between tabs */
  Style		selection_style;	/* style for the <-selection */
  Fragment	selected_fragment;	/* Currently selected fragment */
  Style		selected_fragment_style; /* style of selected_fragment */
  Sheet		styles;			/* Style-name --> Style-object */
  KeyBinding	bindings;		/* Key bindings */
  Name		focus_function;		/* Function in focus */
  BoolObj	fill_mode;		/* Auto fill */
  BoolObj	exact_case;		/* Search and replace do exact case */
  Name		kill_location;		/* Processing kill sequences */
  Name		search_direction;	/* direction of the search */
  StringObj	search_string;		/* Target of search */
  Int		search_origin;		/* Incremental search started here */
  Int		search_base;		/* Currently searching from here */
  Name		search_wrapped;		/* Whether search is wrapped */
  BoolObj	search_wrapped_warned;	/* ISearch hit end of buffer */
  Name		selection_unit;		/* Selection unit (char, word, line) */
  Name		selection_origin;	/* Original start of selection */
  BoolObj	editable;		/* Text may be changed by user */
  Code		error_message;		/* Forward error messages */
  Code		modified_message;	/* Forward <->modified changed */
  Int		left_margin;		/* Left margin indentation */
  Int		right_margin;		/* Right margin */
  Int		indent_increment;	/* Steps taken by region in/undent1 */
  BoolObj	auto_newline;		/* Auto newline on ->append */
  SourceSink	file;			/* Name of file or NIL */
  Name		dabbrev_target;		/* Base of the dabbrev expansion */
  Chain		dabbrev_reject;		/* Hits rejected by dabbrev */
  Int		dabbrev_pos;		/* Current search position */
  Int		dabbrev_origin;		/* Start of dabbrev word */
  Name		dabbrev_mode;		/* Current dabbrev mode */
  Chain		dabbrev_candidates;	/* Dabbrev user candidates */
					/* Private data */
  intptr_t	internal_mark;		/* Internally used mark */
  FragmentCache fragment_cache;		/* Cache to compute frament overlap */
  ISearchCache  isearch_cache;		/* Cache for highlighting search hits */
End;

NewClass(text_cursor)
  ABSTRACT_GRAPHICAL			/* Abstract class graphical */
  Name		style;			/* Block, arrow, bitmap */
  Image		image;			/* If there is an image; this is it */
  Point		hot_spot;		/* Hot spot of the bitmap image */
End;

NewClass(text_margin)
  ABSTRACT_GRAPHICAL			/* Abstract class graphical */
  Editor	editor;			/* Editor we are associated with */
  Size		gap;			/* X and Y distance between icons */
  Any		background;		/* background of the margin */
End;


		/********************************
		*            TEXTIMAGE		*
		********************************/

#define EOB	(-1)			/* end-of-buffer */

#define TEXT_SCAN_FOR	0
#define TEXT_SKIP_OVER	1

typedef struct text_screen	* TextScreen;
typedef struct text_char	* TextChar;
typedef struct text_line	* TextLine;

typedef void (*SeekFunction)(Any, long);
typedef long (*ScanFunction)(Any, long, int, int, int, int *);
typedef long (*FetchFunction)(Any, TextChar);
typedef void (*MarginFunction)(Any, int *, int*);
typedef void (*RewindFunction)(Any);

#define CHAR_ASCII	(0)		/* ASCII character */
#define CHAR_GRAPHICAL	(1)		/* graphical object */
#define CHAR_IMAGE	(2)		/* image object */

struct text_char
{ union
  { int		c;			/* character at pos */
    Graphical	graphical;		/* graphical at pos */
    Image	image;			/* image at pos */
  } value;
  FontObj	font;			/* Font of this character */
  Colour	colour;			/* Colour of this character */
  Any		background;		/* Background for the characters */
  intptr_t	index;			/* Index in line (relative) */
  short		x;			/* X-position in line (pixels) */
  unsigned char attributes;		/* Its attributes */
  unsigned	type : 2;		/* type of character */
};

struct text_line
{ intptr_t	start;			/* Start index (relative) */
  intptr_t	end;			/* Last index (relative) */
  short		y;			/* Y-position in pixels */
  short		h;			/* Height in pixels */
  short		w;			/* Width of displayed text */
  short		base;			/* Baseline (relative to y) */
  short		length;			/* Number of characters displayed */
  short		allocated;		/* Size of chars array */
  int		changed;		/* Line has been changed? */
  int		ends_because;		/* END_WRAP; END_EOF; END_NL */
  TextChar	chars;			/* Character descriptions */
};

struct text_screen
{ short		skip;			/* Skip this many screen lines */
  short		length;			/* Number of lines displayed */
  short		allocated;		/* Allocated entries of the array */
  TextLine	lines;			/* The actual line structure */
};


NewClass(text_image)			/* TBD: subclass of bitmap? */
  ABSTRACT_GRAPHICAL
  Any		text;			/* Text we are operation on */
  Any		background;		/* Background of text */
  Int		start;			/* Start offset */
  Int		end;			/* First non-visible character */
  Name		wrap;			/* Wrap mode in effect */
  Int		tab_distance;		/* Tab distance in pixels */
  Vector	tab_stops;		/* Vector of tab-stops (pixels) */
  Graphical	pointed;		/* Graphical under the pointer */
  BoolObj	eof_in_window;		/* EOF is in the window */
  Elevation	elevation;		/* Box elevation */
					/* start private data */
  intptr_t	w;			/* Used width in pixels */
  intptr_t	h;			/* Used height in pixels */
  intptr_t	change_start;		/* Start of changes */
  intptr_t	change_end;		/* End of changes */
  intptr_t	inserted;		/* Number of chars inserted/deleted */
  SeekFunction  seek;			/* Seek to position */
  ScanFunction	scan;			/* Scan for character type */
  FetchFunction fetch;			/* Function to fetch characters */
  MarginFunction margin;		/* Function to fetch margins */
  RewindFunction rewind;		/* Rewind (prepare) input */
  TextScreen	map;			/* Describes the text object */
End;

#endif /* _PCE_TXT_INCLUDED */
