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
initialiseLink(Link link, Name from, Name to, Line line, Class cl)
{ if ( isDefault(from) )
    from = NAME_link;

  assign(link, from, from);
  assign(link, to, isDefault(to) ? from : to);
  assign(link, line, isDefault(line) ? (Line) newObject(ClassLine, EAV) :	line);
  assign(link, connection_class, cl);

  succeed;
}


		 /*******************************
		 *	     CONNECTION		*
		 *******************************/

					/* lazy binding: fixes save/load! */

static Connection
getConnectionLink(Link link, Graphical gr, Graphical gr2, Name from, Name to)
{ if ( !instanceOfObject(link->connection_class, ClassClass) )
    assign(link, connection_class, ClassConnection);

  return newObject(link->connection_class, gr, gr2, link, from, to, EAV);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_connection[] =
        { "from=graphical", "to=graphical", "from_handle=[name]", "to_handle=[name]" };
static char *T_initialise[] =
        { "handle_kind1=[name]", "handle_kind2=[name]", "line=[line]", "connection_class=[class]" };

/* Instance Variables */

static vardecl var_link[] =
{ IV(NAME_line, "line", IV_GET,
     NAME_appearance, "Line (with pen, arrows, etc.)"),
  IV(NAME_from, "name", IV_BOTH,
     NAME_relation, "Name of valid handles at `from' side"),
  IV(NAME_to, "name", IV_BOTH,
     NAME_relation, "Name of valid handles at `to' side"),
  IV(NAME_connectionClass, "[class]", IV_BOTH,
     NAME_relation, "Class used by <-connection")
};

/* Send Methods */

static senddecl send_link[] =
{ SM(NAME_initialise, 4, T_initialise, initialiseLink,
     DEFAULT, "Create from handle names, line")
};

/* Get Methods */

static getdecl get_link[] =
{ GM(NAME_connection, 4, "connection", T_connection, getConnectionLink,
     NAME_relation, "Instantiate the link by creating a connection")
};

/* Resources */

#define rc_link NULL
/*
static classvardecl rc_link[] =
{
};
*/

/* Class Declaration */

static Name link_termnames[] = { NAME_from, NAME_to, NAME_line };

ClassDecl(link_decls,
          var_link, send_link, get_link, rc_link,
          3, link_termnames,
          "$Rev$");


status
makeClassLink(Class class)
{ declareClass(class, &link_decls);
  delegateClass(class, NAME_line);

  succeed;
}

