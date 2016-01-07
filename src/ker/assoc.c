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

#define INLINE_UTILITIES 1
#include <h/kernel.h>
#include <h/interface.h>

static int	host_handles;		/* # handles */
static long	itf_symbols;		/* # symbols */

#define SizeOfSymbol ((size_t)(intptr_t)(&(((PceITFSymbol)NULL)->handle[host_handles])))

		/********************************
		*            SYMBOLS		*
		********************************/

PceITFSymbol
newSymbol(Any obj, Name name)
{ PceITFSymbol s = alloc(SizeOfSymbol);
  int n;

  s->object = obj;
  s->name   = name;

  for( n=0; n < host_handles; n++ )
    s->handle[n] = NULL;

  itf_symbols++;

  return s;
}


		/********************************
		*         CREATE/DELETE		*
		********************************/

void
deleteAssoc(Any obj)
{ if ( isObject(obj) && onFlag(obj, F_ASSOC) )
  { PceITFSymbol symbol = getMemberHashTable(ObjectToITFTable, obj);
    if ( symbol )
    { symbol->object = NULL;
      deleteHashTable(ObjectToITFTable, obj);
      clearFlag(obj, F_ASSOC);
    }
  }
}


void
newAssoc(Name name, Any obj)
{ PceITFSymbol symbol;
  Any old;

  if ( (old = getObjectAssoc(name)) )
    deleteAssoc(old);
  deleteAssoc(obj);

  if ( onFlag(name, F_ITFNAME) )
  { symbol = getMemberHashTable(NameToITFTable, name);
    symbol->object = obj;
    appendHashTable(ObjectToITFTable, obj, symbol);
    setFlag(obj, F_ASSOC);
  } else
  { symbol = newSymbol(obj, name);

    setFlag(name, F_ITFNAME);
    if ( isObject(obj) )
      setFlag(obj, F_ASSOC);

    appendHashTable(ObjectToITFTable, obj,  symbol);
    appendHashTable(NameToITFTable,   name, symbol);
  }

  if ( isObject(obj) )
    lockObj(obj);
}


Any
getObjectAssoc(Name name)
{ if ( onFlag(name, F_ITFNAME) )
  { PceITFSymbol symbol = getMemberHashTable(NameToITFTable, name);
    return symbol->object;
  }

  fail;
}


Name
getNameAssoc(Any obj)
{ if ( (isObject(obj) && onFlag(obj, F_ASSOC)) )
  { PceITFSymbol symbol = getMemberHashTable(ObjectToITFTable, obj);
    return symbol->name;
  }

  fail;
}


status
renameAssoc(Name old, Name new)
{ Any obj = getObjectAssoc(old);

  if ( obj != FAIL )
  { newAssoc(new, obj);
    succeed;
  }

  fail;
}


status
forSomeAssoc(Code code)
{ for_hash_table(ObjectToITFTable, s,
		 { PceITFSymbol symbol = s->value;

		   if ( symbol->object )
		     forwardCodev(code, 1, (Any *) (&symbol->name));
		 });
  succeed;
}


		/********************************
		*            SPECIALS           *
		*********************************/

void
initAssoc(int handles)
{ int n;

  host_handles = handles;

  ObjectToITFTable = createHashTable(toInt(1024), NAME_none);
  NameToITFTable   = createHashTable(toInt(1024), NAME_none);

  newAssoc(NAME_ObjectToItfTable, ObjectToITFTable);
  newAssoc(NAME_NameToItfTable,   NameToITFTable);

  for(n=0; n<host_handles; n++)
    HandleToITFTables[n] = createHashTable(toInt(64), NAME_none);
}


