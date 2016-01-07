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


status
initialiseTuple(Tuple t, Any first, Any second)
{ assign(t, first, first);
  assign(t, second, second);

  succeed;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "first=any", "second=any" };

/* Instance Variables */

static vardecl var_tuple[] =
{ IV(NAME_first, "any", IV_BOTH,
     NAME_storage, "First of the tuple"),
  IV(NAME_second, "any", IV_BOTH,
     NAME_storage, "Second of the tuple")
};

/* Send Methods */

static senddecl send_tuple[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseTuple,
     DEFAULT, "Create tuple from first and second")
};

/* Get Methods */

#define get_tuple NULL
/*
static getdecl get_tuple[] =
{
};
*/

/* Resources */

#define rc_tuple NULL
/*
static classvardecl rc_tuple[] =
{
};
*/

/* Class Declaration */

static Name tuple_termnames[] = { NAME_first, NAME_second };

ClassDecl(tuple_decls,
          var_tuple, send_tuple, get_tuple, rc_tuple,
          2, tuple_termnames,
          "$Rev$");


status
makeClassTuple(Class class)
{ return declareClass(class, &tuple_decls);
}

