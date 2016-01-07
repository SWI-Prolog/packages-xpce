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

#include <h/kernel.h>
#include <h/unix.h>

static status
initialiseRC(RC rc, Name name, Class rc_class)
{ if ( !initialiseSourceSink((SourceSink)rc) )
    fail;

  assign(rc, name,     name);
  assign(rc, rc_class, rc_class);

  if ( TheCallbackFunctions.getHostContext )
  { Any context = (*TheCallbackFunctions.getHostContext)(HOST);

    assign(rc, context, context);
  }

  succeed;
}


static RC
getConvertRC(Class class, Name name)
{ answer(answerObject(ClassRC, name, EAV));
}


		 /*******************************
		 *	     PROPERTIES		*
		 *******************************/

static status
existsRC(RC rc)
{ IOSTREAM *s;

  catchErrorPce(PCE, NAME_openFile);
  s = Sopen_object(rc, "rbr");
  catchPopPce(PCE);

  if ( s )
  { Sclose(s);
    succeed;
  }

  fail;
}


static status
accessRC(RC rc, Name mode)
{ if ( mode == NAME_read )
    return existsRC(rc);

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "name=name", "class=[name]" };

/* Instance Variables */

static vardecl var_rc[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_name, "Name of the resource"),
  IV(NAME_class, "[name]", IV_BOTH,
     NAME_name, "Class of the resource (@default: don't care)"),
  IV(NAME_context, "any*", IV_GET,
     NAME_storage, "Host context information")
};

/* Send Methods */

static senddecl send_rc[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseRC,
     DEFAULT, "Create from name and class"),
  SM(NAME_access, 1, "mode={read,write,append,execute}", accessRC,
     NAME_test, "Test if resource has access"),
  SM(NAME_exists, 0, NULL, existsRC,
     NAME_test, "Test if resource exists")
};

/* Get Methods */

static getdecl get_rc[] =
{ GM(NAME_convert, 1, "resource", "name=name", getConvertRC,
     DEFAULT, "Convert name to resource")
};

/* Resources */

#define rc_rc NULL
/*
static classvardecl rc_rc[] =
{
};
*/

/* Class Declaration */

static Name rc_termnames[] = { NAME_name, NAME_class };

ClassDecl(rc_decls,
          var_rc, send_rc, get_rc, rc_rc,
          2, rc_termnames,
          "$Rev$");


status
makeClassRC(Class class)
{ return declareClass(class, &rc_decls);
}

