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
initialiseBlockv(Block b, int argc, Any *argv)
{ int n;

  initialiseCode((Code) b);
  assign(b, members, newObject(ClassChain, EAV));

  for(n=0; n<argc; n++)
  { if ( instanceOfObject(argv[n], ClassVar) )
    { if ( isNil(b->parameters) )
	assign(b, parameters, newObjectv(ClassCodeVector, 1, &argv[n]));
      else
	appendVector(b->parameters, 1, &argv[n]);
    } else
      break;
  }

  for( ; n < argc; n++ )
    appendChain(b->members, argv[n]);

  succeed;
}


static Int
getArityBlock(Block b)
{ int n = (isNil(b->parameters) ? 0 : valInt(getArityVector(b->parameters)));

  n += valInt(getArityChain(b->members));

  answer(toInt(n));
}


static Any
getArgBlock(Block b, Int n)
{ if ( isNil(b->parameters) )
    answer(getArgChain(b->members, n));
  else
  { int s = valInt(getArityVector(b->parameters));

    if ( valInt(n) <= s )
      answer(getArgVector(b->parameters, n));
    else
      answer(getArgChain(b->members, toInt(valInt(n)-s)));
  }
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_block[] =
{ IV(NAME_parameters, "code_vector*", IV_BOTH,
     NAME_argument, "Vector with formal parameters")
};

/* Send Methods */

static senddecl send_block[] =
{ SM(NAME_initialise, 1, "var|code ...", initialiseBlockv,
     DEFAULT, "Create from parameters and statements"),
  SM(NAME_forward, 1, "any ...", forwardBlockv,
     NAME_execute, "Push <-parameters, @arg1 ... and execute")
};

/* Get Methods */

static getdecl get_block[] =
{ GM(NAME_Arg, 1, "code", "int", getArgBlock,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityBlock,
     DEFAULT, "Arity for term description")
};

/* Resources */

#define rc_block NULL
/*
static classvardecl rc_block[] =
{
};
*/

/* Class Declaration */

ClassDecl(block_decls,
          var_block, send_block, get_block, rc_block,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassBlock(Class class)
{ return declareClass(class, &block_decls);
}

