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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TBD:	handle destruction of these objects.  Not that important as they
	are generally not destroyed and only a bit of memory is wasted
	if they are.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define XREF_TABLESIZE 		256
#define HashValue(obj)		(int)(((uintptr_t)(obj) & (XREF_TABLESIZE-1)))

static Xref XrefTable[XREF_TABLESIZE];


WsRef
getXrefObject(Any obj, DisplayObj d)
{ int v = HashValue(obj);
  Xref r;

  XrefsResolved++;

  for( r = XrefTable[v]; r != NULL; r = r->next)
    if ( r->object == obj && r->display == d )
    { DEBUG(NAME_getXref, Cprintf("getXrefObject(%s, %s) --> %p\n",
				  pp(obj), pp(d), r->xref));
      return r->xref;
    }

  if ( openDisplay(d) == SUCCEED )
  { if ( send(obj, NAME_Xopen, d, EAV) == SUCCEED )
    { for( r = XrefTable[v]; r != NULL; r = r->next)
	if ( r->object == obj && r->display == d )
	{ DEBUG(NAME_getXref, Cprintf("getXrefObject(%s, %s) --> %p\n",
				      pp(obj), pp(d), r->xref));
	  return r->xref;
	}
    }
  }

  XrefsResolved--;

  errorPce(obj, NAME_xOpen, d);

  return NULL;
}


WsRef
getExistingXrefObject(Any obj, DisplayObj d)
{ int v = HashValue(obj);
  Xref r;


  for( r = XrefTable[v]; r != NULL; r = r->next)
    if ( r->object == obj && r->display == d )
    { XrefsResolved++;
      return r->xref;
    }

  return NULL;
}


status
registerXrefObject(Any obj, DisplayObj d, WsRef xref)
{ Xref *R = &XrefTable[HashValue(obj)];
  Xref r, new;

  DEBUG(NAME_xref, Cprintf("registerXrefObject(%s, %s, %p)\n",
			   pp(obj), pp(d), xref));

  for( r = *R; r != NULL; r = r->next)
    if ( r->object == obj && r->display == d )
    { r->xref = xref;
      succeed;
    }

  new = alloc(sizeof(struct xref));
  new->object = obj;
  new->display = d;
  new->xref = xref;
  new->next = *R;
  *R = new;

  succeed;
}


Xref
unregisterXrefObject(Any obj, DisplayObj d)
{ Xref *R = &XrefTable[HashValue(obj)];
  Xref r = *R;
  static struct xref old;

  for( ; r != NULL; R = &r->next, r = *R )
  { if ( r->object == obj && (r->display == d || isDefault(d)) )
    { *R = r->next;

      DEBUG(NAME_xref, Cprintf("unregisterXrefObject(%s, %s)\n",
			       pp(obj), pp(r->display)));
      old = *r;
      unalloc(sizeof(struct xref), r);
      return &old;
    }
  }

  fail;
}


void
closeAllXrefs()
{ int i;

  for(i=0; i<XREF_TABLESIZE; i++)
  { Xref r = XrefTable[i];
    Xref nr;

    for(; r; r = nr)
    { nr = r->next;

      send(r->object, NAME_Xclose, r->display, EAV);
    }
  }
}


#if KEEP
static void
unregisterAllXrefsObject(Any obj)
{ Xref *R = &XrefTable[HashValue(obj)];
  Xref r = *R;
  WsRef old;

  for( ; r != NULL; R = &r->next, r = *R )
  { if ( r->object == obj )
    { *R = r->next;
      R = &r->next;

      DEBUG(NAME_xref, Cprintf("unregisterXrefObject(%s, %s)\n",
			       pp(obj), pp(r->display)));
      old = r->xref;
      unalloc(sizeof(struct xref), r);

    } else
      R = &r->next;
  }
}
#endif

