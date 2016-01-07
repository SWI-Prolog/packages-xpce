/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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

#ifndef BOXES_H_INCLUDED
#define BOXES_H_INCLUDED

typedef struct hbox	*HBox;
typedef struct tbox	*TBox;
typedef struct grbox	*GrBox;
typedef struct parbox	*ParBox;
typedef struct rubber	*Rubber;
typedef struct lbox     *LBox;

#include <h/kernel.h>
#include <h/graphics.h>
#include <h/text.h>

#include "proto.h"

#ifdef GLOBALS_HERE
#undef GLOBAL
#define GLOBAL
#endif

GLOBAL Class ClassHBox;
GLOBAL Class ClassRubber;
GLOBAL Class ClassTBox;
GLOBAL Class ClassGrBox;
GLOBAL Class ClassParBox;
GLOBAL Class ClassLBox;

#define ABSTRACT_HBOX \
  Int		width;			/* total width */ \
  Int		ascent;			/* height above baseline */ \
  Int		descent;		/* depth below baseline */ \
  Rubber	rubber;			/* h/v stretchability */


NewClass(hbox)
  ABSTRACT_HBOX
End;


NewClass(tbox)
  ABSTRACT_HBOX
  CharArray	text;			/* represented text */
  Style		style;			/* used style parameters */
End;


NewClass(grbox)
  ABSTRACT_HBOX
  Graphical	graphical;		/* Held in-line graphical */
  Any		alignment;		/* top, bottom, center, left, right */
End;


NewClass(rubber)
  Int		stretch;		/* Get bigger */
  Int		shrink;			/* Get smaller */
  Int		level;			/* hfil/hfill/hfilll (1/2/3) */
  Int		natural;		/* Natural desired size */
  Int		minimum;		/* Minimum size */
  Int		maximum;		/* Maximum size */
  Name		linebreak;		/* @nil, allow, force */
End;


NewClass(parbox)
  ABSTRACT_DEVICE			/* graphical device */
  Int		line_width;		/* Max width of a line */
  Vector	content;		/* Contained hboxes */
  Name		alignment;		/* left,right,center,justify */
  BoolObj		auto_crop;		/* Crop content */
End;


NewClass(lbox)
  ABSTRACT_DEVICE
  Int		left_margin;		/* width of left margin */
  Int		right_margin;		/* with of right margin */
  Int		top_sep;		/* separation above  */
  Int		item_sep;		/* separation between items */
  Int		label_sep;		/* label to item distance */
  Int		label_width;		/* Width of label box */
End;


#endif /*BOXES_H_INCLUDED*/
