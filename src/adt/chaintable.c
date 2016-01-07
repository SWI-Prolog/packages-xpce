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
appendChainTable(ChainTable ct, Any name, Any value)
{ Chain ch;

  if ( (ch = getMemberHashTable((HashTable) ct, name)) )
    appendChain(ch, value);
  else
    appendHashTable((HashTable) ct, name, newObject(ClassChain, value, EAV));

  succeed;
}


status
addChainTable(ChainTable ct, Any name, Any value)
{ Chain ch;

  if ( (ch = getMemberHashTable((HashTable) ct, name)) )
    addChain(ch, value);
  else
    appendHashTable((HashTable) ct, name, newObject(ClassChain, value, EAV));

  succeed;
}


static status
prependChainTable(ChainTable ct, Any name, Any value)
{ Chain ch;

  if ( (ch = getMemberHashTable((HashTable) ct, name)) )
    prependChain(ch, value);
  else
    appendHashTable((HashTable) ct, name, newObject(ClassChain, value, EAV));

  succeed;
}


static status
deleteChainTable(ChainTable ct, Any name, Any value)
{ Chain ch;

  if ( isDefault(value) )
    return deleteHashTable((HashTable)ct, name);

  if ( (ch = getMemberHashTable((HashTable) ct, name)) )
  { TRY(deleteChain(ch, value));
    if ( emptyChain(ch) )
      deleteHashTable((HashTable) ct, name);

    succeed;
  }

  fail;
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_delete[] =
        { "key=any", "value=[any]" };
static char *T_keyAany_valueAany[] =
        { "key=any", "value=any" };

/* Instance Variables */

#define var_chainTable NULL
/*
static vardecl var_chainTable[] =
{
};
*/

/* Send Methods */

static senddecl send_chainTable[] =
{ SM(NAME_append, 2, T_keyAany_valueAany, appendChainTable,
     DEFAULT, "Append association to table"),
  SM(NAME_add, 2, T_keyAany_valueAany, addChainTable,
     NAME_add, "Add association to table"),
  SM(NAME_prepend, 2, T_keyAany_valueAany, prependChainTable,
     NAME_add, "Prepend association to table"),
  SM(NAME_delete, 2, T_delete, deleteChainTable,
     NAME_delete, "Delete all matching symbols")
};

/* Get Methods */

#define get_chainTable NULL
/*
static getdecl get_chainTable[] =
{
};
*/

/* Resources */

#define rc_chainTable NULL
/*
static classvardecl rc_chainTable[] =
{
};
*/

/* Class Declaration */

static Name chainTable_termnames[] = { NAME_buckets };

ClassDecl(chainTable_decls,
          var_chainTable, send_chainTable, get_chainTable, rc_chainTable,
          1, chainTable_termnames,
          "$Rev$");


status
makeClassChainTable(Class class)
{ return declareClass(class, &chainTable_decls);
}
