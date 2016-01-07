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

CPointer
CtoCPointer(void *ptr)
{ CPointer p = answerObjectv(ClassCPointer, 0, NULL);

  p->pointer = ptr;

  return p;
}


static status
initialiseCPointer(CPointer p, CPointer value)
{ if ( notDefault(value) )
    p->pointer = value->pointer;

  succeed;
}


static StringObj
getPrintNameCPointer(CPointer p)
{ char buf[20];

  sprintf(buf, "%p", p->pointer);
  answer(CtoString(buf));
}


static status
equalCPointer(CPointer p1, CPointer p2)
{ if ( p1->pointer == p2->pointer )
    succeed;
  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_cPointer[] =
{ IV(NAME_pointer, "alien:void *", IV_NONE,
     NAME_storage, "Address of the pointer")
};

/* Send Methods */

static senddecl send_cPointer[] =
{ SM(NAME_initialise, 1, "[c_pointer]", initialiseCPointer,
     DEFAULT, "Create c_pointer from other c_pointer"),
  SM(NAME_equal, 1, "to=c_pointer", equalCPointer,
     NAME_compare, "Test if argument is same position")
};

/* Get Methods */

static getdecl get_cPointer[] =
{ GM(NAME_printName, 0, "string", NULL, getPrintNameCPointer,
     NAME_textual, "Printed representation as 0x%lx")
};

/* Resources */

#define rc_cPointer NULL
/*
static classvardecl rc_cPointer[] =
{
};
*/

/* Class Declaration */

static Name cPointer_termnames[] = { NAME_printName };

ClassDecl(cPointer_decls,
          var_cPointer, send_cPointer, get_cPointer, rc_cPointer,
          1, cPointer_termnames,
          "$Rev$");


status
makeClassCPointer(Class class)
{ return declareClass(class, &cPointer_decls);
}
