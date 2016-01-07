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

static status
initialiseSize(Size s, Int w, Int h)
{ if ( isDefault(w) ) w = ZERO;
  if ( isDefault(h) ) h = ZERO;
  assign(s, w, w);
  assign(s, h, h);

  succeed;
}


static Size
getConvertSize(Class class, Name name)
{ int w, h;

  if ( isstrA(&name->data) &&
       (sscanf((char *)name->data.s_textA, "%dx%d", &w, &h) == 2 ||
       (syntax.uppercase && sscanf((char *)name->data.s_textA, "%dX%d", &w, &h) == 2)))
    answer(newObject(ClassSize, toInt(w), toInt(h), EAV));

  fail;
}


static StringObj
getPrintNameSize(Size s)
{ char buf[200];

  sprintf(buf, INTPTR_FORMAT "x" INTPTR_FORMAT, valInt(s->w), valInt(s->h));
  answer(CtoString(buf));
}


status
equalSize(Size s, Size s2)
{ if (s->w == s2->w && s->h == s2->h)
    succeed;
  fail;
}


static status
unionSize(Size s, Size s2)
{ if (valInt(s->w) < valInt(s2->w))
    assign(s, w, s2->w);
  if (valInt(s->h) < valInt(s2->h))
    assign(s, h, s2->h);
  succeed;
}


status
copySize(Size s, Size s2)
{ assign(s, w, s2->w);
  assign(s, h, s2->h);

  succeed;
}


Size
getCopySize(Size s)
{ answer(answerObject(s->class, s->w, s->h, EAV));
}


status
setSize(Size s, Int w, Int h)
{ if ( notDefault(w) ) assign(s, w, w);
  if ( notDefault(w) ) assign(s, h, h);

  succeed;
}


static status
offsetSize(Size s, Int w, Int h)
{ assign(s, w, add(w, s->w));
  assign(s, h, add(h, s->h));

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_widthADintD_heightADintD[] =
        { "width=[int]", "height=[int]" };
static char *T_offset[] =
        { "width=int", "height=int" };

/* Instance Variables */

static vardecl var_size[] =
{ IV(NAME_width, "int", IV_BOTH,
     NAME_dimension, "Width (W) of the size"),
  IV(NAME_height, "int", IV_BOTH,
     NAME_dimension, "Height (H) of the size")
};

/* Send Methods */

static senddecl send_size[] =
{ SM(NAME_initialise, 2, T_widthADintD_heightADintD, initialiseSize,
     DEFAULT, "Create size from width and height"),
  SM(NAME_offset, 2, T_offset, offsetSize,
     NAME_calculate, "Add 1st argument to W, 2nd to H"),
  SM(NAME_set, 2, T_widthADintD_heightADintD, setSize,
     NAME_calculate, "Set W and H from arguments"),
  SM(NAME_union, 1, "size", unionSize,
     NAME_calculate, "set W and H to maximum of the two"),
  SM(NAME_equal, 1, "size", equalSize,
     NAME_compare, "Test if equal to argument"),
  SM(NAME_copy, 1, "size", copySize,
     NAME_copy, "Copy W and H from argument")
};

/* Get Methods */

static getdecl get_size[] =
{ GM(NAME_convert, 1, "size", "name", getConvertSize,
     NAME_textual, "Convert text `WxH'"),
  GM(NAME_printName, 0, "string", NULL, getPrintNameSize,
     NAME_textual, "Printed representation as %dx%d")
};

/* Resources */

#define rc_size NULL
/*
static classvardecl rc_size[] =
{
};
*/

/* Class Declaration */

static Name size_termnames[] = { NAME_width, NAME_height };

ClassDecl(size_decls,
          var_size, send_size, get_size, rc_size,
          2, size_termnames,
          "$Rev$");


status
makeClassSize(Class class)
{ return declareClass(class, &size_decls);
}
