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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The AnswerStack holds  objects that have been created,  but that nobody
has claimed yet.   It is a per-thread  stack of cells.  Each  cell links
down (next)  and up (above),  and a  pointer hash table maps  the object
to  its cell,  so  deleteAnswerObject() unlinks  a  cell without  scanning
the stack.

Cell indices increase towards the top.   They come from a counter rather
than from the top  cell, so a cell pushed after a mark  has a higher index
than the  mark, also  if the  cell that was  on top  when the  mark was
taken has been deleted since.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static inline size_t
answer_hash(Any obj, size_t size)
{ uintptr_t k = (uintptr_t)obj >> 3;

  k ^= k >> 15;
  k *= (uintptr_t)0x2c1b3c6dU;
  k ^= k >> 12;

  return (size_t)k & (size-1);
}


static void
answer_table_resize(answer_table *t, size_t size)
{ ToCell *old = t->cells;
  size_t osize = t->size;
  size_t i;

  t->cells = alloc(size * sizeof(ToCell));
  memset(t->cells, 0, size * sizeof(ToCell));
  t->size = size;

  for(i=0; i<osize; i++)
  { ToCell c = old[i];

    if ( c )
    { size_t j = answer_hash(c->value, size);

      while( t->cells[j] )
	j = (j+1) & (size-1);
      t->cells[j] = c;
    }
  }

  if ( old )
    unalloc(osize * sizeof(ToCell), old);
}


static void
answer_table_add(answer_table *t, ToCell c)
{ size_t i;

  if ( (t->count+1)*2 > t->size )
    answer_table_resize(t, t->size ? t->size*2 : 64);

  i = answer_hash(c->value, t->size);
  while( t->cells[i] )
    i = (i+1) & (t->size-1);
  t->cells[i] = c;
  t->count++;
}


static size_t
answer_table_slot(answer_table *t, Any obj)
{ if ( t->size )
  { size_t i = answer_hash(obj, t->size);
    ToCell c;

    while( (c=t->cells[i]) )
    { if ( c->value == obj )
	return i;
      i = (i+1) & (t->size-1);
    }
  }

  return (size_t)-1;
}


/* Delete slot i, moving later cells of the probe sequence back so that
   lookups need no tombstones.
*/

static void
answer_table_delete_slot(answer_table *t, size_t i)
{ size_t mask = t->size-1;
  size_t j = i;

  for(;;)
  { ToCell c;
    size_t h;

    j = (j+1) & mask;
    if ( !(c=t->cells[j]) )
      break;
    h = answer_hash(c->value, t->size);
    if ( i <= j ? (i < h && h <= j) : (i < h || h <= j) )
      continue;				/* c cannot move to i */
    t->cells[i] = c;
    i = j;
  }

  t->cells[i] = NULL;
  t->count--;
}


static ToCell
answer_table_remove(answer_table *t, Any obj)
{ size_t i = answer_table_slot(t, obj);

  if ( i != (size_t)-1 )
  { ToCell c = t->cells[i];

    answer_table_delete_slot(t, i);
    return c;
  }

  return NULL;
}


static void
unlink_answer_cell(ThreadData td, ToCell c)
{ if ( c->above )
    c->above->next = c->next;
  else
    td->answer_stack = c->next;
  c->next->above = c->above;		/* the base cell is never unlinked */

  unalloc(sizeof(struct to_cell), c);
}


void
pushAnswerObject(Any obj)
{ if ( isVirginObj(obj) )
  { ThreadData td = TheThreadData();
    ToCell c = alloc(sizeof(struct to_cell));

    setAnswerObj(obj);
    c->value = obj;
    c->index = ++td->answer_high;
    c->above = NULL;
    c->next  = td->answer_stack;
    td->answer_stack->above = c;
    td->answer_stack = c;
    answer_table_add(&td->answer_table, c);
  }
}


void
deleteAnswerObject(Any obj)
{ if ( isAnswerObj(obj) )
  { ThreadData td = TheThreadData();
    ToCell c;

    if ( (c=answer_table_remove(&td->answer_table, obj)) )
      unlink_answer_cell(td, c);
    clearAnswerObj(obj);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Drop all  cells above the mark  and free their objects  if nobody claimed
them.  `obj` is preserved: it stays an answer object, on top of the stack.

Cells are popped one at  a time from the top, so this is  reentrant: if
freeing  an object  deletes other  cells or  pushes new  ones, the  loop
simply sees the stack as it is now.  Objects pushed above the mark while
we free are reclaimed as well.

NOTE: F_ANSWER must be cleared for every  cell we drop, not only when we
also free the object.  An object that acquired a reference while it was
on the  stack survives the  rewind, but its  cell is gone:  leaving the
flag  set makes  isVirginObj() false  forever, so  freeableObj() can never
reclaim it once the last reference disappears.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

export void
_rewindAnswerStack(AnswerMark *mark, Any obj)
{ ThreadData td = TheThreadData();
  long index = *mark;
  bool preserve = false;

  if ( td->answer_stack->index <= index )
    return;

  if ( isObject(obj) && isAnswerObj(obj) )
  { size_t i = answer_table_slot(&td->answer_table, obj);

    if ( i != (size_t)-1 && td->answer_table.cells[i]->index > index )
    { ToCell c = td->answer_table.cells[i];

      answer_table_delete_slot(&td->answer_table, i);
      unlink_answer_cell(td, c);
      clearAnswerObj(obj);
      addCodeReference(obj);		/* freeing the others may not free it */
      preserve = true;
    }
  }

  while( td->answer_stack->index > index )
  { ToCell c = td->answer_stack;
    Any o = c->value;

    answer_table_remove(&td->answer_table, o);
    unlink_answer_cell(td, c);
    clearAnswerObj(o);			/* the cell is dropped */

    if ( noRefsObj(o) && !onFlag(o, F_LOCKED|F_PROTECTED) )
      freeObject(o);
  }
  td->answer_high = index;

  if ( preserve )
  { ((Instance)obj)->references -= ONE_CODE_REF;
    if ( isFreedObj(obj) )
    { checkDeferredUnalloc(obj);
    } else
    { pushAnswerObject(obj);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Called at the host boundary, where no goal is running.  Nothing on the C
stack can then hold  an object that nobody claimed, so anything left on
the AnswerStack  was created outside a  mark/rewind pair and would never
be reclaimed.  Use debugpce(gc) to list these objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
rewindAnswerStackToBase(void)
{ ThreadData td = TheThreadData();

  if ( td->answer_stack != &td->answer_stack_base_cell )
  { AnswerMark mark = td->answer_stack_base_cell.index;

#ifndef O_RUNTIME
    if ( DEBUGGING(NAME_gc) )
    { ToCell c;

      Cprintf("Reclaiming %zd unclaimed answer objects:\n",
	      td->answer_table.count);
      for(c=td->answer_stack; c != &td->answer_stack_base_cell; c=c->next)
	Cprintf("\t%s\n", pp(c->value));
    }
#endif

    _rewindAnswerStack(&mark, NIL);
  }
}


void
initAnswerStack(void)
{ ThreadData td = TheThreadData();

  td->answer_stack = &td->answer_stack_base_cell;
  td->answer_stack->index = 1;
  td->answer_stack->value = 0;
  td->answer_stack->next  = NULL;
  td->answer_stack->above = NULL;
  td->answer_high = 1;
}


/* Drop all cells without touching the objects.  The table remains
   allocated.
*/

static void
clearAnswerStack(ThreadData td, bool clear_flags)
{ ToCell c, n;

  for(c = td->answer_stack; c != &td->answer_stack_base_cell; c = n)
  { n = c->next;
    if ( clear_flags )
      clearAnswerObj(c->value);
    unalloc(sizeof(struct to_cell), c);
  }

  if ( td->answer_table.size )
    memset(td->answer_table.cells, 0, td->answer_table.size*sizeof(ToCell));
  td->answer_table.count = 0;

  initAnswerStack();
}


void
resetAnswerStack(void)
{ clearAnswerStack(TheThreadData(), true);
}


/* Called when the thread exits.  The objects may be gone already.
*/

void
destroyAnswerStack(void)
{ ThreadData td = TheThreadData();

  clearAnswerStack(td, false);
  if ( td->answer_table.cells )
  { unalloc(td->answer_table.size*sizeof(ToCell), td->answer_table.cells);
    td->answer_table.cells = NULL;
    td->answer_table.size  = 0;
  }
}


Int
countAnswerStack(void)
{ answer(toInt(TheThreadData()->answer_table.count));
}
