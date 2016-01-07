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

static status	rootEventTree(EventTreeObj t, EventNodeObj n);

static status
initialiseEventTree(EventTreeObj t, EventNodeObj n)
{ assign(t, root,  NIL);
  assign(t, table, newObject(ClassHashTable, toInt(101), EAV));

  if ( notDefault(n) )
    rootEventTree(t, n);

  succeed;
}


static status
rootEventTree(EventTreeObj t, EventNodeObj n)
{ if ( notNil(t->root) )
    return errorPce(t, NAME_alreadyHasRoot);

  assign(t, root,   n);
  assign(n, parent, t);

  addNodeEventTree(t, n);

  succeed;
}


status
addNodeEventTree(EventTreeObj t, EventNodeObj n)
{ return appendHashTable(t->table, n->value, n);
}


EventNodeObj
getNodeEventTree(EventTreeObj t, Any value)
{ return getMemberHashTable(t->table, value);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_eventTree[] =
{ SV(NAME_root, "event_node", IV_GET|IV_STORE, rootEventTree,
     NAME_hierarchy, "Root node of the hierarchy"),
  IV(NAME_table, "hash_table", IV_NONE,
     NAME_hashing, "Hashtable to find nodes by value")
};

/* Send Methods */

static senddecl send_eventTree[] =
{ SM(NAME_initialise, 1, "root=[event_node]", initialiseEventTree,
     DEFAULT, "Create from root node")
};

/* Get Methods */

static getdecl get_eventTree[] =
{ GM(NAME_node, 1, "event_node", "name|int", getNodeEventTree,
     NAME_lookup, "Find a node from it's associated value")
};

/* Resources */

#define rc_eventTree NULL
/*
static classvardecl rc_eventTree[] =
{
};
*/

/* Class Declaration */

static Name eventTree_termnames[] = { NAME_root };

ClassDecl(eventTree_decls,
          var_eventTree, send_eventTree, get_eventTree, rc_eventTree,
          1, eventTree_termnames,
          "$Rev$");


status
makeClassEventTree(Class class)
{ return declareClass(class, &eventTree_decls);
}

