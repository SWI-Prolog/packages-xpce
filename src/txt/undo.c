/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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
#include <h/text.h>

static void	resetUndoBuffer(UndoBuffer ub);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			  TEXTBUFFER UNDO

This module implements multi-action undo  for textbuffers.  Only three
basic operations are  defined on textbuffers: deleting, inserting  and
change.  For any of these, a record is made that describes the change.
For   insertions,  this just  defines the  place   and the size.   For
deletions  and changes the  old text  is stored  as well.  Undo simply
implies to walk back this change and perform the inverse operations.

The records are stored in an `undo buffer'.  This buffer operates as a
cycle.  New records  are appended to the end.   If the buffer is full,
old records are destroyed at  the start,  just until enough  space  is
available to  store  the new record.   The  elements are linked  in  a
double  linked chain.   The  backward links are used  to   perform the
incremental  undo's, the forward  links are used  to  destroy old undo
records.  The head and tail members point to the  last inserted, resp.
oldest cell  in the chain.  The free  member points ahead  of the head
and describes where to allocate  the next cell.  The current describes
the  cell to be undone on  the next ->undo  message.  While performing
undos this pointer goes backwards in the list.  On any other operation
it is placed at the head of the list.

Finally, the checkpoint member describes  the head cell  at the moment
->modified @off was send to the textbuffer  the last time.  If undoing
hits this cell it will again mark the buffer as not modified.

__Change forwarding__

To accommodate LSP (Language Service Protocol), we need to be able to
send incremental updates.  An LSP change consists of

  - A range, expressed as `start` and `end`, where positions are 0-based
    line and line position, where the line position is counted in
    UTF-16 entities ...
  - Length of the range in UTF-16 entities
  - Replacement text

This maps as follows to our three undo cell types:

  - An _insert_ has the same `start` and `end` and a text to be inserted.
  - A _delete_ uses the range to describe the deleted material, using
    no replacement text.
  - A _change_ uses the range and the replacement text.  Our replaces
    always replace with the same number of characters (Unicode code
    points).

We  must generate  a list  of the  above change  descriptions for  the
changes that result from processing an event.  At the end of the event
markUndoTextBuffer() is  called.  While  collecting the  changes, undo
cells are extended on adjacent changes, creating a new undo cell for a
new series or if the next action is not adjacent or of different type.
Thus, we can

  - If we create a new undo cell, push the previous cell on the change
    list.
  - On markUndoTextBuffer(), push the previous cell on the change list.
    if the change list is not empty, dispatch it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_CHANGESET_SIZE 10000

typedef struct undo_cell	* UndoCell;
typedef struct undo_delete	* UndoDelete;
typedef struct undo_insert	* UndoInsert;
typedef struct undo_change	* UndoChange;

#define UNDO_DELETE 0		/* action types */
#define UNDO_INSERT 1
#define UNDO_CHANGE 2

#define NOCHECKPOINT		((UndoCell) -1)

#define COMMON_CELL \
  UndoCell	previous;	/* previous cell */ \
  UndoCell	next;		/* next in chain */ \
  size_t	size;		/* size in chars */ \
  bool		marked;		/* marked as interactive cell */ \
  char		type;		/* type of action (UNDO_*) */ \
  lsp_pos	lpos;		/* LSP line/position notation */ \
  ssize_t	where;		/* start address of delete */ \
  ssize_t	len;		/* number of characters deleted there */

struct undo_cell
{ COMMON_CELL
};

struct undo_delete
{ COMMON_CELL
  bool		iswide;
  unsigned	u16_len;	/* Maintain range length */
  lsp_pos	epos;		/* End position */
  union
  { charA	A[1];		/* ISO Latin-1 text */
    charW	W[1];		/* Wide character text */
  } text;
};

struct undo_insert
{ COMMON_CELL
};

struct undo_change
{ COMMON_CELL
  bool		iswide;
  unsigned	u16_len;	/* Maintain range length */
  lsp_pos	epos;		/* End position */
  union
  { charA	A[1];		/* ISO Latin-1 text */
    charW	W[1];		/* Wide character text */
  } text;
};

struct undo_buffer
{ TextBuffer	client;		/* so we know whom to talk to */
  size_t	size;		/* size of buffer in chars */
  size_t	changeset_size;	/* Total size of pending changes */
  bool		undone;		/* last action was an undo */
  bool		aborted;	/* sequence was too big, aborted */
  UndoCell	current;	/* current undo cell for undos */
  UndoCell	checkpoint;	/* non-modified checkpoint */
  UndoCell	lastmark;	/* last marked cell */
  UndoCell	head;		/* first cell */
  UndoCell	tail;		/* last cell */
  UndoCell	free;		/* allocate next one here */
  UndoCell	buffer;		/* buffer storage */
};

#define istbA(tb)		((tb)->buffer.iswide == 0)
#define Round(n, r)		(((n)+(r)-1) & (~((r)-1)))
#define AllocRound(s)		Round(s, sizeof(UndoCell))
#define Distance(p1, p2)	((char *)(p1) - (char *)(p2))
#define SizeAfter(ub, size)	((size) <= ub->size - \
					   Distance(ub->free, ub->buffer))
#define UndoDeleteSize(len) ((unsigned)(uintptr_t) &((UndoDelete)NULL)->text.A[len])
#define UndoChangeSize(len) ((unsigned)(uintptr_t) &((UndoChange)NULL)->text.A[len])

static void	pushLspChanges(UndoBuffer ub);
static Int	getSizeTextChange(TextChange tc);

static UndoBuffer
createUndoBuffer(size_t size)
{ UndoBuffer ub = alloc(sizeof(struct undo_buffer));

  ub->size    = AllocRound(size);
  ub->buffer  = alloc(ub->size);
  ub->aborted = FALSE;
  ub->client  = NIL;
  resetUndoBuffer(ub);

  return ub;
}


static void
resetUndoBuffer(UndoBuffer ub)
{ ub->current = ub->lastmark = ub->head = ub->tail = NULL;
  ub->checkpoint = NOCHECKPOINT;
  ub->free = ub->buffer;
}


void
destroyUndoBuffer(UndoBuffer ub)
{ if ( ub->buffer != NULL )
  { unalloc(ub->size, ub->buffer);
    ub->buffer = NULL;
  }

  unalloc(sizeof(struct undo_buffer), ub);
}


/* Undo a command, returning the new caret position
 */

Int
getUndoTextBuffer(TextBuffer tb)
{ long caret = -1;

  if ( tb->undo_buffer != NULL )
  { UndoBuffer ub = tb->undo_buffer;
    UndoCell cell;

    if ( (cell = ub->current) == NULL )	/* No further undo's */
      fail;

    while(cell != NULL)
    { DEBUG(NAME_undo, Cprintf("Undo using cell %d: ",
			       Distance(cell, ub->buffer)));
      switch( cell->type )
      { case UNDO_DELETE:
	{ UndoDelete d = (UndoDelete) cell;
	  string s;

	  s.s_size = d->len;
	  s.s_iswide = d->iswide;
	  if ( d->iswide )
	    s.s_textA = d->text.A;
	  else
	    s.s_textW = d->text.W;

	  DEBUG(NAME_undo, Cprintf("Undo delete at %ld, len=%ld\n",
				   d->where, d->len));
	  insert_textbuffer(tb, d->where, 1, &s);
	  caret = max(caret, d->where + d->len);
	  break;
	}
	case UNDO_INSERT:
	{ UndoInsert i = (UndoInsert) cell;
	  DEBUG(NAME_undo, Cprintf("Undo insert at %ld, len=%ld\n",
				   i->where, i->len));
	  delete_textbuffer(tb, i->where, i->len);
	  caret = max(caret, i->where);
	  break;
	}
	case UNDO_CHANGE:
	{ UndoChange c = (UndoChange) cell;
	  string s;

	  s.s_size = c->len;
	  s.s_iswide = c->iswide;
	  if ( c->iswide )
	    s.s_textA = c->text.A;
	  else
	    s.s_textW = c->text.W;

	  DEBUG(NAME_undo, Cprintf("Undo change at %ld, len=%ld\n",
				   c->where, c->len));

	  change_textbuffer(tb, c->where, &s);
	  caret = max(caret, c->where + c->len);
	  break;
	}
      }

      cell = cell->previous;
      if ( cell == NULL || cell->marked == true )
      {	ub->current = cell;

	if ( cell == ub->checkpoint )	/* reached non-modified checkpoint */
	{ DEBUG(NAME_undo, Cprintf("Reset modified to @off\n"));
	  CmodifiedTextBuffer(tb, OFF);
	}

        changedTextBuffer(tb);
	ub->undone = true;

	answer(toInt(caret));
      }
    }
  }

  fail;
}

status
undoTextBuffer(TextBuffer tb)
{ return getUndoTextBuffer(tb) ? SUCCEED : FAIL;
}

static UndoBuffer
getUndoBufferTextBuffer(TextBuffer tb)
{ if ( tb->undo_buffer != NULL )
    return tb->undo_buffer;

  if ( isDefault(tb->undo_buffer_size) )
    assign(tb, undo_buffer_size,
	   getClassVariableValueObject(tb, NAME_undoBufferSize));

  if ( tb->undo_buffer_size != ZERO )
  { tb->undo_buffer = createUndoBuffer(valInt(tb->undo_buffer_size));
    tb->undo_buffer->client = tb;
  }

  return tb->undo_buffer;
}


status
undoBufferSizeTextBuffer(TextBuffer tb, Int size)
{ if ( tb->undo_buffer_size != size )
  { if ( tb->undo_buffer != NULL )
    { destroyUndoBuffer(tb->undo_buffer);
      tb->undo_buffer = NULL;
    }

    assign(tb, undo_buffer_size, size);
  }

  succeed;
}


status
markUndoTextBuffer(TextBuffer tb)
{ UndoBuffer ub;

  if ( (ub = getUndoBufferTextBuffer(tb)) )
  { DEBUG(NAME_undo, Cprintf("markUndoTextBuffer(%s)\n", pp(tb)));

    pushLspChanges(ub);

    if ( ub->head )
    { ub->head->marked = true;
      ub->lastmark = ub->head;
    }

    if ( ub->undone == false )
      ub->current = ub->head;

    ub->undone = false;
    ub->aborted = false;
  }

  succeed;
}


status
resetUndoTextBuffer(TextBuffer tb)
{ if ( tb->undo_buffer != NULL )
  { destroyUndoBuffer(tb->undo_buffer);
    tb->undo_buffer = NULL;
  }

  succeed;
}


status
checkpointUndoTextBuffer(TextBuffer tb)
{ UndoBuffer ub;

  if ( (ub = getUndoBufferTextBuffer(tb)) )
    ub->checkpoint = ub->head;

  succeed;
}


static void
destroy_oldest_undo(UndoBuffer ub)
{ if ( ub->tail != NULL )
    ub->tail->marked = FALSE;

  while( ub->tail != NULL && ub->tail->marked == FALSE )
  { if ( ub->tail == ub->current )
      ub->current = NULL;
    if ( ub->tail == ub->checkpoint )
      ub->checkpoint = NOCHECKPOINT;
    if ( ub->tail == ub->head )
    { resetUndoBuffer(ub);
      return;
    }
    if ( ub->tail->next )
      ub->tail->next->previous = NULL;
    ub->tail = ub->tail->next;
  }

  if ( ub->tail == NULL )
    resetUndoBuffer(ub);
}


		/********************************
		*           ALLOCATION          *
		*********************************/

static int
Between(UndoBuffer ub, UndoCell new, UndoCell old)
{ if ( new > old )
    return Distance(new, old);

  return ub->size - Distance(old, new);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Allocate a new undo size with the requested size in bytes
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void *
new_undo_cell(UndoBuffer ub, size_t size)
{ UndoCell new;

  if ( ub->aborted )
    return NULL;

  size = AllocRound(size);

  if ( size > ub->size/2 )		/* Too big: destroy all */
  { errorPce(ub->client, NAME_undoOverflow);

    ub->aborted = TRUE;
    resetUndoBuffer(ub);
    return NULL;
  }

  pushLspChanges(ub);

  for(;;)
  { if ( ub->head == NULL )		/* Empty: new cell is head & tail */
      break;

    if ( ub->tail < ub->free )
    { if ( SizeAfter(ub, size) )
	break;
      ub->free = ub->buffer;
    } else if ( (int)size <= Distance(ub->tail, ub->free) )
      break;

    destroy_oldest_undo(ub);
  }

  if ( ub->lastmark && Between(ub, ub->free, ub->lastmark) >= (int)ub->size/2 )
  { errorPce(ub->client, NAME_undoOverflow);

    ub->aborted = TRUE;
    resetUndoBuffer(ub);
    return NULL;
  }

  new = ub->free;
  new->size = size;
  new->marked = FALSE;
  new->next = NULL;
  new->previous = ub->head;
  if ( ub->head == NULL )		/* empty */
  { ub->tail = new;
    ub->lastmark = new;
  } else
    ub->head->next = new;
  ub->head = new;
  ub->free = (UndoCell) ((char *)new + size);

  DEBUG(NAME_undo, Cprintf("Cell at %d size=%d: ",
			   Distance(new, ub->buffer), new->size));

  return new;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resize the current undo-cell to be at least <size> characters.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
resize_undo_cell(UndoBuffer ub, UndoCell cell, size_t size)
{ size = AllocRound(size);
  assert(cell == ub->head);

  if ( cell->size == size )
    return true;

  while( ub->tail > cell && (int)size > Distance(ub->tail, cell) && ub->head )
    destroy_oldest_undo(ub);

  if ( ub->head &&
       ((ub->tail >  cell && size < Distance(ub->tail, cell)) ||
	(ub->tail <= cell && SizeAfter(ub, size))) )
  { cell->size = size;
    ub->free = (UndoCell) ((char *) cell + size);

    DEBUG(NAME_undo, Cprintf("Resized cell at %d size=%d\n",
			     Distance(cell, ub->buffer), cell->size));
    return true;
  }

  DEBUG(NAME_undo,
	if ( !ub->head )
	  Cprintf("**** UNDO buffer overflow ****\n");
	else
	  Cprintf("**** UNDO buffer circle ****\n"));

  return false;
}


static TextChange
get_text_change_undo_cell(TextBuffer tb, UndoCell c)
{ TextChange tc = NULL;

  switch(c->type)
  { case UNDO_DELETE:
    { UndoDelete ud  = (UndoDelete)c;

      tc = newObject(ClassTextChange,
		     toInt(ud->lpos.line), toInt(ud->lpos.pos),
		     toInt(ud->epos.line), toInt(ud->epos.pos),
		     toInt(ud->u16_len), EAV);
      break;
    }
    case UNDO_INSERT:
    { UndoInsert ui  = (UndoInsert)c;
      if ( ui->len > MAX_CHANGESET_SIZE ) return NULL;
      StringObj repl = getContentsTextBuffer(tb, toInt(ui->where), toInt(ui->len));

      tc = newObject(ClassTextChange,
		     toInt(ui->lpos.line), toInt(ui->lpos.pos),
		     DEFAULT, DEFAULT, DEFAULT, repl, EAV);
      break;
    }
    case UNDO_CHANGE:
    { UndoChange uc  = (UndoChange)c;
      if ( uc->len > MAX_CHANGESET_SIZE ) return NULL;
      StringObj repl = getContentsTextBuffer(tb, toInt(uc->where), toInt(uc->len));

      tc = newObject(ClassTextChange,
		     toInt(uc->lpos.line), toInt(uc->lpos.pos),
		     toInt(uc->epos.line), toInt(uc->epos.pos),
		     toInt(uc->u16_len), repl, EAV);
      break;
    }
    default:
      assert(0);
      fail;
  }

  return tc;
}

/* Push a change to the changeset.   This cancels collecting changeset
 * data if the collective size of the changes exceeds MAX_CHANGESET_SIZE
 */

static void
pushLspChanges(UndoBuffer ub)
{ TextBuffer tb = ub->client;

  if ( !isNil(tb->lsp_changes) )
  { UndoCell uc = ub->head;
    if ( uc && !uc->marked )
    { TextChange tc = get_text_change_undo_cell(tb, uc);
      if ( tc )
      { size_t sz = valInt(getSizeTextChange(tc));

	if ( isDefault(tb->lsp_changes) )
	{ assign(tb, lsp_changes, newObject(ClassChain, tc, EAV));
	  ub->changeset_size = sz;
	} else
	{ appendChain(tb->lsp_changes, tc);
	  ub->changeset_size += sz;
	}
	if ( ub->changeset_size > MAX_CHANGESET_SIZE )
	{ assign(tb, lsp_changes, NIL);
	}
      } else
      { assign(tb, lsp_changes, NIL);
      }
    }
  }
}

static void
init_lsp_pos(TextBuffer tb, UndoCell c)
{ if ( tb->lsp_changes != NIL )
  { memset(&c->lpos, 0, sizeof(c->lpos));
    update_lsp_pos_text_buffer(tb, 0, c->where, &c->lpos);
  }
}

static void
move_lsp_pos(TextBuffer tb, UndoCell c, size_t to)
{ if ( tb->lsp_changes != NIL )
  { update_lsp_pos_text_buffer(tb, c->where, to, &c->lpos);
    c->where = to;
  }
}

/* Called _before_ the text is inserted
*/

void
register_insert_textbuffer(TextBuffer tb, size_t where, size_t len)
{ UndoBuffer ub;

  if ( len > 0 && (ub = getUndoBufferTextBuffer(tb)) != NULL )
  { UndoInsert i = (UndoInsert) ub->head;

    if ( i != NULL && i->type == UNDO_INSERT && i->marked == false )
    { if ( i->where + i->len == where )
      { i->len += len;
	DEBUG(NAME_undo, Cprintf("Insert at %zd right-grown %zd bytes\n",
				 i->where, i->len));
        return;
      } else if ( where + len == i->where )
      { i->len += len;
	move_lsp_pos(tb, (UndoCell)i, where);
	DEBUG(NAME_undo, Cprintf("Insert at %zd left-grown %zd bytes\n",
				 i->where, i->len));
        return;
      }
    }

    if ( (i = new_undo_cell(ub, sizeof(struct undo_insert))) == NULL )
      return;

    i->type  = UNDO_INSERT;
    i->where = where;
    i->len   = len;
    init_lsp_pos(tb, (UndoCell)i);
    DEBUG(NAME_undo, Cprintf("New Insert at %ld, %ld bytes\n",
			     i->where, i->len));
  }
}


static void
copy_undo_del(TextBuffer tb, long from, long len, UndoDelete udc, long offset)
{ if ( udc->iswide )
  { charW *to = &udc->text.W[offset];

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  } else
  { charA *to = &udc->text.A[offset];

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  }
}


/* Called before the text is actually being deleted
*/

void
register_delete_textbuffer(TextBuffer tb, size_t where, size_t len)
{ UndoBuffer ub;
  long i;
  int need_wide = FALSE;
  int cell_size;

  for(i=where; i<where+len; i++)
  { wint_t c = fetch_textbuffer(tb, i);

    if ( tisendsline(tb->syntax, c) )
      tb->lines--;

    if ( c > 0xff )
      need_wide = TRUE;
  }

  if ( len > 0 && (ub = getUndoBufferTextBuffer(tb)) != NULL )
  { UndoDelete udc = (UndoDelete) ub->head;

    if ( udc != NULL && udc->type == UNDO_DELETE &&
	 udc->marked == FALSE &&
	 tb->buffer.s_iswide == udc->iswide )
    { if ( where == udc->where )	/* forward delete */
      { cell_size = len+udc->len;
	if ( udc->iswide )
	  cell_size *= sizeof(charW);

	if ( resize_undo_cell(ub, (UndoCell)udc, UndoDeleteSize(cell_size)) )
	{ copy_undo_del(tb, where, len, udc, udc->len);
	  udc->len += len;
	  udc->u16_len += u16_range_length(tb, where, len);
	  update_lsp_pos_text_buffer(tb, udc->where+udc->len, where+len, &udc->epos);
	  DEBUG(NAME_undo, Cprintf("Delete at %ld grown forward %ld bytes\n",
				   udc->where, udc->len));
	}

        return;
      }

      if ( where + len == udc->where )	/* backward delete */
      { cell_size = len+udc->len;
	if ( udc->iswide )
	  cell_size *= sizeof(charW);

	if ( resize_undo_cell(ub, (UndoCell) udc, UndoDeleteSize(cell_size)) )
	{ if ( udc->iswide )
	  { memmove(&udc->text.W[len], &udc->text.W, udc->len*sizeof(charW));
	  } else
	  { memmove(&udc->text.A[len], &udc->text.A, udc->len);
	  }

	  copy_undo_del(tb, where, len, udc, 0);
	  update_lsp_pos_text_buffer(tb, udc->where, where, &udc->lpos);
	  udc->len += len;
	  udc->where -= len;
	  udc->u16_len = u16_range_length(tb, where, len);
	  DEBUG(NAME_undo, Cprintf("Delete at %ld grown backward %ld bytes\n",
				   udc->where, udc->len));
	}
	return;
      }
    }

    cell_size = need_wide ? len*(int)sizeof(charW) : len;
    if ( (udc = new_undo_cell(ub, UndoDeleteSize(cell_size))) == NULL )
      return;
    udc->type    = UNDO_DELETE;
    udc->where   = where;
    udc->len     = len;
    udc->iswide  = need_wide;
    udc->u16_len = u16_range_length(tb, where, len);
    copy_undo_del(tb, where, len, udc, 0);
    init_lsp_pos(tb, (UndoCell)udc);
    udc->epos    = udc->lpos;
    update_lsp_pos_text_buffer(tb, where, where+len, &udc->epos);
    DEBUG(NAME_undo, Cprintf("New delete at %ld, %ld bytes\n",
			     udc->where, udc->len));
  }
}


static void
copy_undo_chg(TextBuffer tb, long from, long len, UndoChange uc, long offset)
{ if ( uc->iswide )
  { charW *to = &uc->text.W[offset];

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  } else
  { charA *to = &uc->text.A[offset];

    for( ; len > 0; len--, from++ )
      *to++ = fetch_textbuffer(tb, from);
  }
}


void
register_change_textbuffer(TextBuffer tb, size_t where, size_t len)
{ UndoBuffer ub;
  int need_wide = FALSE;
  int cell_size;
  long i;

  for(i=where; i<where+len; i++)
  { wint_t c = fetch_textbuffer(tb, i);

    if ( c > 0xff )
      need_wide = TRUE;
  }

  if ( len > 0 && (ub = getUndoBufferTextBuffer(tb)) != NULL )
  { UndoChange uc = (UndoChange) ub->head;

    if ( uc != NULL && uc->type == UNDO_CHANGE &&
	 uc->marked == FALSE &&
	 tb->buffer.s_iswide == uc->iswide )
    { if ( where == uc->where + uc->len )	/* forward change */
      {	cell_size = len+uc->len;
	if ( uc->iswide )
	  cell_size *= sizeof(charW);

	if ( resize_undo_cell(ub, (UndoCell)uc,
			      UndoChangeSize(cell_size)) )
	{ uc->u16_len += u16_range_length(tb, where, len);
	  update_lsp_pos_text_buffer(tb, where, where+len, &uc->epos);
	  copy_undo_chg(tb, where, len, uc, uc->len);
	  uc->len += len;
	  DEBUG(NAME_undo,
		Cprintf("Change at %ld grown forward to %ld bytes\n",
			uc->where, uc->len));
	}
	return;
      }

      if ( where + len == uc->where )		/* backward change */
      { cell_size = len+uc->len;
	if ( uc->iswide )
	  cell_size *= sizeof(charW);

	if ( resize_undo_cell(ub, (UndoCell)uc,
			      UndoChangeSize(cell_size)) )
	{ if ( uc->iswide )
	  { memmove(&uc->text.W[len], &uc->text.W, uc->len*sizeof(charW));
	  } else
	  { memmove(&uc->text.A[len], &uc->text.A, uc->len);
	  }

	  copy_undo_chg(tb, where, len, uc, 0);
	  update_lsp_pos_text_buffer(tb, uc->where, where, &uc->lpos);
	  uc->len += len;
	  uc->where -= len;
	  uc->u16_len = u16_range_length(tb, where, len);
	  DEBUG(NAME_undo,
		Cprintf("Change at %ld grown backward to %ld bytes\n",
			uc->where, uc->len));
	}
	return;
      }
    }

    cell_size = need_wide ? len*(int)sizeof(charW) : len;
    if ( (uc = new_undo_cell(ub, UndoChangeSize(cell_size))) == NULL )
      return;
    uc->type   = UNDO_CHANGE;
    uc->where  = where;
    uc->len    = len;
    uc->iswide = need_wide;
    uc->u16_len = u16_range_length(tb, where, len);
    init_lsp_pos(tb, (UndoCell)uc);
    uc->epos   = uc->lpos;
    update_lsp_pos_text_buffer(tb, where, where+len, &uc->epos);
    copy_undo_chg(tb, where, len, uc, 0);
    DEBUG(NAME_undo, Cprintf("New change at %ld, %ld bytes\n",
			     uc->where, uc->len));
  }
}

		/*******************************
		*         TEXT CHANGE          *
		*******************************/

static status
initialiseTextChange(TextChange tc,
		     Int sl, Int sp, Int el, Int ep, Int len,
		     StringObj repl)
{ if ( isDefault(el)   ) el=sl;
  if ( isDefault(ep)   ) ep=sp;
  if ( isDefault(len)  ) len=ZERO;
  if ( isDefault(repl) ) repl=NIL;

  assign(tc, start_line,     sl);
  assign(tc, start_position, sp);
  assign(tc, end_line,       el);
  assign(tc, end_position,   ep);
  assign(tc, length,         len);
  assign(tc, replacement,    repl);

  succeed;
}

static Int
getSizeTextChange(TextChange tc)
{ size_t sz = sizeof(*tc);
  if ( notNil(tc->replacement) )
  { StringObj s = tc->replacement;

    sz += sizeof(*s);
    sz += s->data.s_size * (s->data.s_iswide ? sizeof(*s->data.s_textW)
					     : sizeof(*s->data.s_textA));
  }

  return toInt(sz);
}


static char *T_initialise[] =
{ "start_line=int", "start_position=int", "end_line=[int]", "end_position=[int]",
  "length=[int]", "replacement=[string*]"
};

static vardecl var_text_change[] =
{ IV(NAME_startLine, "int", IV_GET,
     NAME_range, "0-based line where change starts"),
  IV(NAME_startPosition, "int", IV_GET,
     NAME_range, "0-based line position in UTF-16 units"),
  IV(NAME_endLine, "int", IV_GET,
     NAME_range, "0-based line where change ends"),
  IV(NAME_endPosition, "int", IV_GET,
     NAME_range, "0-based line position in UTF-16 units"),
  IV(NAME_length, "int", IV_GET,
     NAME_range, "Length of region in UTF-16 units"),
  IV(NAME_replacement, "string*", IV_GET,
     NAME_range, "Text used to replace the region")
};

static senddecl send_text_change[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseTextChange,
     DEFAULT, "Create text_change from range and replacement"),
};

static getdecl get_text_change[] =
{ GM(NAME_size, 0, "int", NULL, getSizeTextChange,
     NAME_memory, "Size in bytes for this change")
};

#define rc_text_change NULL
/*
static classvardecl rc_text_change[] =
{
};
*/

static Name text_change_termnames[] =
	{ NAME_startLine, NAME_startPosition, NAME_endLine, NAME_endPosition,
	  NAME_replacement
	};

ClassDecl(text_change_decls,
          var_text_change, send_text_change, get_text_change,
	  rc_text_change,
          5, text_change_termnames,
          "$Rev$");

status
makeClassTextChange(Class class)
{ declareClass(class, &text_change_decls);

  succeed;
}
