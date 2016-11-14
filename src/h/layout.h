/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1998-2011, University of Amsterdam
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

#ifndef LAYOUT_H_INCLUDED
#define LAYOUT_H_INCLUDED
#include <h/graphics.h>

GLOBAL Class ClassLayoutManager;
GLOBAL Class ClassLayoutInterface;
GLOBAL Class ClassTable;
GLOBAL Class ClassTableSlice;
GLOBAL Class ClassTableRow;
GLOBAL Class ClassTableColumn;
GLOBAL Class ClassTableCell;

typedef struct layout_manager	*LayoutManager;
typedef struct layout_interface *LayoutInterface;
typedef struct table		*Table;
typedef struct table_slice	*TableSlice;
typedef struct table_row	*TableRow;
typedef struct table_column	*TableColumn;
typedef struct table_cell	*TableCell;
#ifndef BOXES_H_INCLUDED
typedef struct rubber		*Rubber;
#endif

#define ABSTRACT_LAYOUT_MANAGER \
    Device	device;			/* Device managed */ \
    Any		request_compute;	/* Layout needs recomputed? */

#define ABSTRACT_LAYOUT_INTERFACE \
    LayoutManager layout_manager;	/* The layout manager */ \
    Graphical	  image;		/* Graphical managed */

NewClass(layout_manager)
    ABSTRACT_LAYOUT_MANAGER
End;

NewClass(layout_interface)
    ABSTRACT_LAYOUT_INTERFACE
End;

NewClass(table)
    ABSTRACT_LAYOUT_MANAGER
    Vector	rows;			/* Vector of holding the rows */
    Vector	columns;		/* Vector holding column info */
    Int		border;			/* border around cells */
    Name	frame;			/* Parts of the frame painted */
    Name	rules;			/* Which rules are painted */
    Size	cell_padding;		/* default padding around cells */
    Size	cell_spacing;		/* Space between cells */
    Point	current;		/* Current X-Y location */
    Int		width;			/* Total width (or @default) */
					/* internal stuff */
    Area	area;			/* Total occupied area */
    BoolObj	changed;		/* layout-changing action */
End;

NewClass(table_cell)
    ABSTRACT_LAYOUT_INTERFACE
    Int		column;			/* X-location in table */
    Int		row;			/* Y-location in table */
    Name	halign;			/* left, right, center, ref, stretch */
    Name	valign;			/* top, bottom, center, ref, stretch */
    Rubber	hrubber;		/* Horizonal stretchability */
    Rubber	vrubber;		/* Vertical stretchability */
    Int		col_span;		/* number of columns spanned */
    Int		row_span;		/* number of rows spanned */
    Size	cell_padding;		/* Padding for this cell (default) */
    BoolObj	selected;		/* Cell is selected */ \
    Any		background;		/* Background colour in */
    Image	note_mark;		/* Mark for (foot-) notes */
End;

#define ABSTRACT_TABLE_SLICE \
    ABSTRACT_VECTOR			/* vector attributes */ \
    Table	table;			/* Table I belong to */ \
    Any		background;		/* Background colour in */ \
    BoolObj	selected;		/* Default <-selected of cells */ \
    Name	alignment;		/* halign,valign */ \
    BoolObj	end_group;		/* Slice ends a (row/column) group */ \
    Name	name;			/* Name of the slice */ \
    Int		index;			/* nth row/column */ \
    BoolObj	fixed;			/* Width/reference is fixed */ \
    Int		width;			/* width/height of the row/column */ \
    Int		reference;		/* position of reference-aligned */ \
    Int		position;		/* Offset of row/column */ \
    Rubber	rubber;			/* Stretch/Shrinkability */ \
    BoolObj	displayed;		/* @off: slice is hidden */

NewClass(table_slice)
    ABSTRACT_TABLE_SLICE
End;

NewClass(table_row)
    ABSTRACT_TABLE_SLICE
End;

NewClass(table_column)
    ABSTRACT_TABLE_SLICE
End;

typedef struct _table_cell_dimensions
{ int	x;				/* X-location */
  int	y;				/* Y-location */
  int	w;				/* Width of the cell (outer) */
  int	h;				/* height */
  int	rx;				/* reference-X */
  int	ry;				/* reference-Y */
  int	px;				/* H-padding */
  int	py;				/* V-padding */
} table_cell_dimensions, *TableCellDimensions;

#include <fmt/proto.h>			/* function prototypes */

#endif /*LAYOUT_H_INCLUDED*/
