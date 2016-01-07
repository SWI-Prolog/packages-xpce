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
initialiseQuoteFunction(QuoteFunction q, Function f)
{ assign(q, function, f);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_quoteFunction[] =
{ IV(NAME_function, "function", IV_BOTH,
     NAME_storage, "Quoted function")
};

/* Send Methods */

static senddecl send_quoteFunction[] =
{ SM(NAME_initialise, 1, "function=function", initialiseQuoteFunction,
     DEFAULT, "Initialise")
};

/* Get Methods */

#define get_quoteFunction NULL
/*
static getdecl get_quoteFunction[] =
{
};
*/

/* Resources */

#define rc_quoteFunction NULL
/*
static classvardecl rc_quoteFunction[] =
{
};
*/

/* Class Declaration */

static Name quoteFunction_termnames[] = { NAME_function };

ClassDecl(quoteFunction_decls,
          var_quoteFunction, send_quoteFunction,
	  get_quoteFunction, rc_quoteFunction,
          1, quoteFunction_termnames,
          "$Rev$");

status
makeClassQuoteFunction(Class class)
{ declareClass(class, &quoteFunction_decls);
  delegateClass(class, NAME_function);

  succeed;
}

