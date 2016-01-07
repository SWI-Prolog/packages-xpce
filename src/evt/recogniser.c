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

status
initialiseRecogniser(Recogniser r)
{ assign(r, active, ON);

  succeed;
}


static status
eventRecogniser(Recogniser r, EventObj ev)
{ fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_recogniser[] =
{ IV(NAME_active, "bool", IV_BOTH,
     NAME_status, "Ignore events when @off")
};

/* Send Methods */

static senddecl send_recogniser[] =
{ SM(NAME_initialise, 0, NULL, initialiseRecogniser,
     DEFAULT, "Create new recogniser"),
  SM(NAME_event, 1, "event", eventRecogniser,
     NAME_event, "Process an event (fails)")
};

/* Get Methods */

#define get_recogniser NULL
/*
static getdecl get_recogniser[] =
{
};
*/

/* Resources */

#define rc_recogniser NULL
/*
static classvardecl rc_recogniser[] =
{
};
*/

/* Class Declaration */

ClassDecl(recogniser_decls,
          var_recogniser, send_recogniser, get_recogniser, rc_recogniser,
          ARGC_INHERIT, NULL,
          "$Rev$");


status
makeClassRecogniser(Class class)
{ return declareClass(class, &recogniser_decls);
}

