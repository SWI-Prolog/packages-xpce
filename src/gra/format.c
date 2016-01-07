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

#include <h/kernel.h>
#include <h/graphics.h>

static status
initialiseFormat(Format l, Name direction, Int width, BoolObj columns)
{ assign(l, direction, isDefault(direction) ? NAME_horizontal : direction);
  assign(l, width, isDefault(width) ? ONE : width);
  assign(l, columns, isDefault(columns) ? ON : columns);
  assign(l, column_sep, toInt(10));
  assign(l, row_sep, toInt(10));
  assign(l, adjustment, NIL);

  succeed;
}


status
makeClassFormat(Class class)
{ sourceClass(class, makeClassFormat, __FILE__, "$Revision$");

  localClass(class, NAME_direction, NAME_orientation,
	     "{horizontal,vertical}", NAME_both,
	     "horizontal (rows) or vertical (columns)");
  localClass(class, NAME_width, NAME_dimension, "int", NAME_both,
	     "Width in columns/rows or pixels");
  localClass(class, NAME_columns, NAME_table, "bool", NAME_both,
	     "Use columns/rows");
  localClass(class, NAME_columnSep, NAME_table, "int", NAME_both,
	     "Distance between columns/rows");
  localClass(class, NAME_rowSep, NAME_table, "int", NAME_both,
	     "Distance between rows/columns");
  localClass(class, NAME_adjustment, NAME_table, "vector*", NAME_both,
	     "left,center,right alignment");

  termClass(class, "format", 3, NAME_direction, NAME_width, NAME_columns);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "orientation=[{horizontal,vertical}]",
	     "width=[1..]", "columns=[bool]",
	     "Create from direction, width and columns",
	     initialiseFormat);

  succeed;
}

