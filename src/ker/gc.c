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

#undef DEBUG				/* only if needed on this module */
#define DEBUG(t, g)

static struct to_cell AnswerStackBaseCell;

void
pushAnswerObject(Any obj)
{ if ( isVirginObj(obj) )
  { ToCell c = alloc(sizeof(struct to_cell));

    DEBUG(NAME_gc, Cprintf("pushAnswerObject(%s)\n", pp(obj)));
    setAnswerObj(obj);
    c->value = obj;
    c->index = AnswerStack->index + 1;
    c->next  = AnswerStack;
    AnswerStack = c;
  }
}


void
deleteAnswerObject(Any obj)
{ if ( isAnswerObj(obj) )
  { ToCell c;

    DEBUG(NAME_gc, Cprintf("deleteAnswerObject(%s)\n", pp(obj)));

    c = AnswerStack;

    if ( c->value == obj )
    { AnswerStack = c->next;
      unalloc(sizeof(struct to_cell), c);
    } else
    { ToCell p = c;

      for( c = c->next; c; p = c, c = c->next)
      { if ( c->value == obj )
	{ p->next = c->next;
	  unalloc(sizeof(struct to_cell), c);
	  break;				/* can only be there once! */
	}
      }
    }

    clearAnswerObj(obj);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: deletion of the  head  is  avoided   to  ensure  this  routine  is
reentrant. This may be  necessary  if   unlinking  an  object causes new
mark/rewind actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

export void
_rewindAnswerStack(AnswerMark *mark, Any obj)
{ ToCell c = AnswerStack;
  long index = *mark;

  if ( c->index > index )
  { ToCell n, preserve = NULL;
    int freehead = FALSE;

    for( ; c->index > index; c = n )
    { n = c->next;
      DEBUG(NAME_gc, Cprintf("Cell at 0x%lx\n", (unsigned long)c));
      if ( c->value )
      { if ( c->value != obj )
	{ Any o = c->value;

	  if ( noRefsObj(o) && !onFlag(o, F_LOCKED|F_PROTECTED) )
	  { DEBUG(NAME_gc,
		  Cprintf("Removing %s from AnswerStack\n", pp(o)));
	    clearAnswerObj(c->value);
	    freeObject(o);
	  }
	  if ( c != AnswerStack )
	    unalloc(sizeof(struct to_cell), c);
	  else
	    freehead = TRUE;
	} else
	  preserve = c;
      } else
      { if ( c != AnswerStack )
	  unalloc(sizeof(struct to_cell), c);
	else
	  freehead = TRUE;
      }
    }

    if ( freehead )
      unalloc(sizeof(struct to_cell), AnswerStack);

    AnswerStack = c;

    if ( preserve )
    { preserve->next  = AnswerStack;
      preserve->index = AnswerStack->index + 1;
      AnswerStack     = preserve;
    }
  }
}


void
initAnswerStack(void)
{ AnswerStack = &AnswerStackBaseCell;
  AnswerStack->index = 1;
  AnswerStack->value = 0;
  AnswerStack->next  = NULL;
}


void
resetAnswerStack(void)
{ ToCell c, n;

  for( c = AnswerStack; c != &AnswerStackBaseCell; c = n )
  { if ( c->value )
    { clearAnswerObj(c->value);
      /*freeableObj(c->value);		May be handy to leave the
					object for debugging */
    }
    n = c->next;
    unalloc(sizeof(struct to_cell), c);
  }

  initAnswerStack();
}


Int
countAnswerStack()
{ ToCell c;
  int n = 0;

  for( c = AnswerStack; c != &AnswerStackBaseCell; c = c->next )
    n++;

  answer(toInt(n));
}
