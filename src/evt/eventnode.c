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

static EventTreeObj	getTreeEventNode(EventNodeObj n);
static status		sonEventNode(EventNodeObj n, EventNodeObj son);

static status
initialiseEventNode(EventNodeObj n, Any value, EventNodeObj parent)
{ if ( isDefault(parent) )
    parent = NIL;

  assign(n, value,  value);

  if ( isName(parent) )
  { EventNodeObj p;

    if ( !EventTree )
      realiseClass(ClassEvent);

    if ( !(p=getNodeEventTree(EventTree, parent)) )
      return errorPce(EventTree, NAME_noEvent, parent);
    parent = p;
  }

  if ( notNil(parent) )
    sonEventNode(parent, n);

  succeed;
}


static status
sonEventNode(EventNodeObj n, EventNodeObj son)
{ if ( notNil(son->parent) )
    return errorPce(son, NAME_alreadyHasParent);

  if ( isNil(n->sons) )
    assign(n, sons, newObject(ClassChain, EAV));

  appendChain(n->sons, son);
  son->parent = n;
  addNodeEventTree(getTreeEventNode(n), son);

  succeed;
}


status
isAEventNode(EventNodeObj sb, EventNodeObj super)
{ do
  { if ( sb == super )
      succeed;
    sb = sb->parent;
  } while( isObject(sb) );

  fail;
}


static EventTreeObj
getTreeEventNode(EventNodeObj n)
{ while( instanceOfObject(n->parent, ClassEventNode) )
    n = n->parent;

  if ( instanceOfObject(n->parent, ClassEventTree) )
    answer((EventTreeObj) n->parent);

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "value=name", "parent=[name|event_node]*" };

/* Instance Variables */

static vardecl var_eventNode[] =
{ IV(NAME_value, "event_id", IV_GET,
     NAME_value, "Value of the node"),
  IV(NAME_parent, "event_node|event_tree", IV_GET,
     NAME_hierarchy, "Direct parent of the node"),
  IV(NAME_sons, "chain*", IV_GET,
     NAME_hierarchy, "Chain of direct sons")
};

/* Send Methods */

static senddecl send_eventNode[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseEventNode,
     DEFAULT, "Create from value and parent"),
  SM(NAME_son, 1, "event_node", sonEventNode,
     NAME_edit, "Add a son to the node"),
  SM(NAME_isA, 1, "event_node", isAEventNode,
     NAME_test, "Test if node is a subnode of argument")
};

/* Get Methods */

static getdecl get_eventNode[] =
{ GM(NAME_tree, 0, "event_tree", NULL, getTreeEventNode,
     NAME_organisation, "The tree holding this event_node")
};

/* Resources */

#define rc_eventNode NULL
/*
static classvardecl rc_eventNode[] =
{
};
*/

/* Class Declaration */

static Name eventNode_termnames[] = { NAME_value, NAME_parent };

ClassDecl(eventNode_decls,
          var_eventNode, send_eventNode, get_eventNode, rc_eventNode,
          2, eventNode_termnames,
          "$Rev$");

status
makeClassEventNode(Class class)
{ return declareClass(class, &eventNode_decls);
}

