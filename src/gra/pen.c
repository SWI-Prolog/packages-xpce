/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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
initialisePen(Pen p, Int thickness, Name texture, Any colour)
{ if ( isDefault(thickness) )
    thickness = ONE;
  if ( isDefault(texture) )
    texture = NAME_none;

  assign(p, thickness, thickness);
  assign(p, texture,   texture);
  assign(p, colour,    colour);

  succeed;
}


static Pen
getConvertPen(Class class, Int thickness)
{ answer(newObject(ClassPen, thickness, EAV));
}


status
makeClassPen(Class class)
{ sourceClass(class, makeClassPen, __FILE__, "$Revision$");

  localClass(class, NAME_thickness, NAME_dimension, "0..", NAME_both,
	     "Thickness of the line (pixels)");
  localClass(class, NAME_texture, NAME_appearance, "texture_name", NAME_both,
	     "Dash pattern");
  localClass(class, NAME_colour, NAME_appearance, "[colour]", NAME_both,
	     "Colour of the line");

  termClass(class, "pen", 3, NAME_thickness, NAME_texture, NAME_colour);

  sendMethod(class, NAME_initialise, DEFAULT, 3,
	     "thickness=[0..]",
	     "[texture=texture_name]",
	     "colour=[colour|pixmap]",
	     "Create pen from thickness, texture and colour",
	     initialisePen);

  getMethod(class, NAME_convert, DEFAULT, "pen", 1, "0..",
	    "Convert to pen of indicated thickness",
	    getConvertPen);

  succeed;
}
