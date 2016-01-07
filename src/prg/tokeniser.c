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
#include <h/unix.h>
#include <h/lang.h>
#include <math.h>

#define A_NONE		0		/* no input */
#define A_FILE		1		/* input is a file */
#define A_CHAR_ARRAY	2		/* input is a char_array */
#define A_TEXT_BUFFER	3		/* input in a text_buffer */

NewClass(tokeniser)
  SyntaxTable	syntax;			/* syntax declarations */
  Any		source;			/* input device */
  Chain		stack;			/* push-back */
  HashTable	symbols;		/* (Partial) symbols */
  int		line;			/* current line-no */
  int		access;			/* access-functions */
  int		caret;			/* current location */
End;

static status	closeTokeniser(Tokeniser);

#define IsEof(c)	((c) == EOF)

static status
initialiseTokeniser(Tokeniser t, SyntaxTable syntax)
{ assign(t, syntax,  syntax);
  assign(t, symbols, newObject(ClassHashTable, EAV));

  t->access = A_NONE;
  t->line = t->caret = 0;

  succeed;
}


static status
cloneTokeniser(Tokeniser t, Tokeniser clone)
{ clonePceSlots(t, clone);
  assign(clone, source, NIL);
  clone->access = A_NONE;
  clone->line = clone->caret = 0;

  succeed;
}

		 /*******************************
		 *	 READ ALIEN SLOTS	*
		 *******************************/

static Int
getLineTokeniser(Tokeniser t)
{ answer(toInt(t->line));
}


static Int
getCaretTokeniser(Tokeniser t)
{ answer(toInt(t->caret));
}


		 /*******************************
		 *	 HANLING <-SOURCE	*
		 *******************************/

Tokeniser
getOpenTokeniser(Tokeniser t, Any source)
{ if ( notNil(t->source) )
  { t = getCloneObject(t);
    assert(t);
  }

  assign(t, source, source);
  t->line  = 1;
  t->caret = 0;

  if ( instanceOfObject(source, ClassFile) )
  { if ( !send(t->source, NAME_open, NAME_read, EAV) )
    { assign(t, source, NIL);
      fail;
    }
    t->access = A_FILE;
  } else if ( instanceOfObject(source, ClassCharArray) )
  { t->access = A_CHAR_ARRAY;
  } else if ( instanceOfObject(source, ClassTextBuffer) )
  { t->access = A_TEXT_BUFFER;
  }

  answer(t);
}


static status
closeTokeniser(Tokeniser t)
{ switch(t->access)
  { case A_FILE:
      send(t->source, NAME_close, EAV);
  }

  assign(t, source, NIL);
  t->access = A_NONE;

  succeed;
}


static int
GETC(Tokeniser t)
{ int c;

  switch(t->access)
  { case A_FILE:
    { FileObj f = t->source;
      c = Sgetcode(f->fd);
      break;
    }
    case A_CHAR_ARRAY:
    { CharArray ca = t->source;
      String s = &ca->data;

      c = (t->caret < s->s_size ? (int)str_fetch(&ca->data, t->caret) : EOF);
      break;
    }
    case A_TEXT_BUFFER:
    { TextBuffer tb = t->source;
      c = fetch_textbuffer(tb, t->caret);
    }
    default:
      return EOF;
  }

  if ( tisendsline(t->syntax, c) )
    t->line++;
  t->caret++;

  return c;
}


static int
PEEKC(Tokeniser t)
{ int c;

  switch(t->access)
  { case A_FILE:
    { FileObj f = t->source;
      c = Speekcode(f->fd);
      break;
    }
    case A_CHAR_ARRAY:
    { CharArray ca = t->source;
      String s = &ca->data;

      c = (t->caret < s->s_size ? (int)str_fetch(&ca->data, t->caret) : EOF);
      break;
    }
    case A_TEXT_BUFFER:
    { TextBuffer tb = t->source;
      c = fetch_textbuffer(tb, t->caret);
    }
    default:
      return EOF;
  }

  return c;
}


static void
UNGETC(Tokeniser t, int c)		/* Only used for tokenizing numbers */
{ if ( t->caret > 0 )
  { switch(t->access)
    { case A_FILE:
      { FileObj f = t->source;

	assert(c<128);
	Sungetc(c, f->fd);
      }
    }

    if ( tisendsline(t->syntax, c) )
      t->line--;
    t->caret--;
  }
}


static Int
getCharacterTokeniser(Tokeniser t)
{ int c = GETC(t);

  if ( !IsEof(c) )
    answer(toInt(c));

  fail;
}


Int
getPeekTokeniser(Tokeniser t)
{ int c = PEEKC(t);

  if ( !IsEof(c) )
    answer(toInt(c));

  fail;
}

		 /*******************************
		 *	    SYMBOLS		*
		 *******************************/

status
symbolTokeniser(Tokeniser t, Name symb)	/* only need 2++ characters!? */
{ String s = &symb->data;
  int size = s->s_size;

  if ( size > 1 )
  { int i;

    for(i=0; i<size; i++)
    { if ( !tisalnum(t->syntax, str_fetch(s, i)) )
      { string s2;

	str_cphdr(&s2, s);
	s2.s_text = s->s_text;
	for(i=1; i<=size; i++)
	{ s2.s_size = i;
	  appendHashTable(t->symbols, StringToName(&s2), ON);
	}

	succeed;
      }
    }
  }

  succeed;
}


		 /*******************************
		 *	    SYNTAX ERRORS	*
		 *******************************/

static status
syntaxErrorTokeniser(Tokeniser t, CharArray msg)
{ errorPce(t, NAME_sourceError, t->source, toInt(t->line), msg);

  succeed;
}


static Any
getReportToTokeniser(Tokeniser t)
{ if ( notNil(t->source) )
    answer(t->source);

  fail;
}


		 /*******************************
		 *	    TOKENISER		*
		 *******************************/


static status
tokenTokeniser(Tokeniser t, Any token)
{ if ( isNil(t->stack) )
    assign(t, stack, newObject(ClassChain, EAV));

  return appendChain(t->stack, token);
}


static Any
getTokenTokeniser(Tokeniser t)
{ int c;
  SyntaxTable s = t->syntax;

  if ( notNil(t->stack) && !emptyChain(t->stack) )
  { Any token = getDeleteHeadChain(t->stack);

    answer(token);
  }

  if ( isNil(t->source) )
  { errorPce(t, NAME_notOpen);
    fail;
  }

					/* skip whitespace and comment */
  for(;;)
  { do
    { c = GETC(t);
    } while(tislayout(s, c));

    if ( tiscommentstart(s, c) )	/* 1 character comment */
    { do
      { if ( IsEof(c = GETC(t)) )
	{ send(t, NAME_syntaxError, CtoName("End of file in comment"), EAV);
	  fail;
	}
      } while( !tiscommentend(s, c) );

      continue;
    } else if ( tiscommentstart1(s, c) ) /* 2 character comment */
    { int c2 = PEEKC(t);

      if ( tiscommentstart2(s, c2) )
      { int c1, c2;

	GETC(t);			/* get peeked char */
	c1 = GETC(t);
	c2 = GETC(t);

	while( !tiscommentend1(s, c1) || !tiscommentend2(s, c2) )
	{ c1 = c2;
	  if ( IsEof(c2 = GETC(t)) )
	  { send(t, NAME_syntaxError, CtoName("End of file in comment"), EAV);
	    fail;
	  }
	}

	continue;
      }
    }

    break;
  }


  DEBUG(NAME_tokeniser, Cprintf("Found char = %c at %d\n", c, t->caret));

  if ( IsEof(c) )
    return EndOfFile;

  if ( tisquote(s, c) )			/* strings */
  { char buf[LINESIZE];
    char *q = buf;
    int open = c;

    for(;;)
    { if ( IsEof(c = GETC(t)) )
      { send(t, NAME_syntaxError, CtoName("End of file in string"), EAV);
	fail;
      }

      if ( tisstringescape(s, open, c) )
      { if ( c == open )		/* escape as double "" or '' */
	{ int c2 = PEEKC(t);

	  if ( c2 == open )
	  { GETC(t);			/* get peeked */
	    *q++ = c;
	    continue;
	  } else
	  { *q = EOS;
	    answer(CtoString(buf));
	  }
	} else
	{ int c2;

	  if ( IsEof(c2 = GETC(t)) )
	  { send(t, NAME_syntaxError, CtoName("End of file in string"), EAV);
	    fail;
	  }
	  if ( c2 != open )
	    *q++ = c;
	  *q++ = c2;
	  continue;
	}
      }

      if ( c == open )
      { *q = EOS;
        answer(CtoString(buf));
      } else
	*q++ = c;
    }
  } else if ( tisdigit(s, c) || c == '-' ) /* int, real */
  { char buf[LINESIZE];
    char *q = buf;
    int is_int = TRUE;

    if ( c == '-' )
    { int c2 = PEEKC(t);

      if ( !tisdigit(s, c2) )
	goto nonum;
      *q++ = c;
      c = GETC(t);
    }

    do
    { *q++ = c;

      c = GETC(t);
    } while ( tisdigit(s, c) );

    if ( c == '.' )
    { int c2 = PEEKC(t);

      if ( tisdigit(s, c2) )
      { *q++ = c;
        c = GETC(t);
        is_int = FALSE;
	do
	{ *q++ = c;
	  c = GETC(t);
	} while( tisdigit(s, c) );
      } else
      { goto num_out;
      }
    }

    if ( c == 'e' || c == 'E' )
    { int c2 = PEEKC(t);

      if ( tisdigit(s, c2) )
      { *q++ = c;
        c = GETC(t);
        is_int = FALSE;
	do
	{ *q++ = c;
	  c = GETC(t);
	} while( tisdigit(s, c) );
      } else
      { goto num_out;
      }
    }
  num_out:
    UNGETC(t, c);

    *q = EOS;
    if ( is_int )
    { char *e;
      long f = strtol(buf, &e, 10);
      if ( e != q )
      { DEBUG(NAME_tokeniser,
	      Cprintf("Num = '%s' (%ld), e = %d, q = %d\n",
		      buf, f, e-buf, q-buf));
	send(t, NAME_syntaxError, CtoName("Illegal number"), EAV);
	fail;
      }
      answer(toInt(f));
    } else
    { char *e;
      double f = cstrtod(buf, &e);
      if ( e != q )
      { DEBUG(NAME_tokeniser,
	      Cprintf("Num = '%s' (%f), e = %d, q = %d\n",
		      buf, f, e-buf, q-buf));
	send(t, NAME_syntaxError, CtoName("Illegal number"), EAV);
	fail;
      }
      answer(CtoReal(f));
    }
  }

nonum:
  if ( tisalnum(s, c) )		/* atom */
  { char buf[LINESIZE];
    char *q = buf;

    *q++ = c;
    for(;;)
    { c = PEEKC(t);
      if ( !tisalnum(s, c) )
	break;
      *q++ = GETC(t);
    }
    *q = EOS;

    return CtoKeyword(buf);		/* uppercase conversion! */
  } else				/* singleton */
  { char buf[LINESIZE];
    char *s = buf;
    Name symb, symbol;

    *s++ = c;
    *s = EOS;
    symb = CtoName(buf);

    if ( isNil(t->symbols) || !getMemberHashTable(t->symbols, symb) )
      answer(symb);

    for(;;)
    { symbol = symb;
      c = PEEKC(t);
      *s++ = c;
      *s   = EOS;
      if ( !tischtype(t->syntax, c, PU) )
	break;
      symb = CtoName(buf);
      DEBUG(NAME_token, Cprintf("trying symbol %s\n", pp(symb)));
      if ( !getMemberHashTable(t->symbols, symb) )
	break;
      c = GETC(t);
    }

    answer(symbol);
  }
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */


/* Instance Variables */

static vardecl var_tokeniser[] =
{ IV(NAME_syntax, "syntax_table", IV_BOTH,
     NAME_syntax, "Syntax used"),
  IV(NAME_source, "file|char_array|text_buffer*", IV_GET,
     NAME_input, "Input source"),
  IV(NAME_stack, "chain*", IV_GET,
     NAME_readAhead, "Stack of pushed-back tokens"),
  IV(NAME_symbols, "hash_table", IV_GET,
     NAME_syntax, "Table with punctuation-character symbols"),
  IV(NAME_access, "alien:int", IV_NONE,
     NAME_input, "Internal access context"),
  IV(NAME_line, "alien:int", IV_NONE,
     NAME_report, "Current line number"),
  IV(NAME_caret, "alien:int", IV_NONE,
     NAME_input, "Point for random_access devices")
};

/* Send Methods */

static senddecl send_tokeniser[] =
{ SM(NAME_initialise, 1, "syntax=[syntax_table]", initialiseTokeniser,
     DEFAULT, "Create from syntax"),
  SM(NAME_close, 0, NULL, closeTokeniser,
     NAME_input, "Close source"),
  SM(NAME_token, 1, "token=any", tokenTokeniser,
     NAME_readAhead, "Push back a token"),
  SM(NAME_syntaxError, 1, "message=char_array", syntaxErrorTokeniser,
     NAME_report, "Generate syntax-error warning"),
  SM(NAME_symbol, 1, "symbol=name", symbolTokeniser,
     NAME_syntax, "Declare name to be a symbol")
};

/* Get Methods */

static getdecl get_tokeniser[] =
{ GM(NAME_open, 1, "tokeniser", "file|char_array|text_buffer*", getOpenTokeniser,
     NAME_input, "Open input for tokenising"),
  GM(NAME_character, 0, "char", NULL, getCharacterTokeniser,
     NAME_parse, "Read next character"),
  GM(NAME_peek, 0, "char", NULL, getPeekTokeniser,
     NAME_parse, "Peek at next character"),
  GM(NAME_token, 0, "token=any", NULL, getTokenTokeniser,
     NAME_parse, "Read next token"),
  GM(NAME_caret, 0, "int", NULL, getCaretTokeniser,
     NAME_report, "Current character-index"),
  GM(NAME_line, 0, "int", NULL, getLineTokeniser,
     NAME_report, "Current line-number (1-based)"),
  GM(NAME_reportTo, 0, "any", NULL, getReportToTokeniser,
     NAME_report, "Report errors to the <-source")
};

/* Resources */

#define rc_tokeniser NULL
/*
static classvardecl rc_tokeniser[] =
{
};
*/

/* Class Declaration */

static Name tokeniser_termnames[] = { NAME_source, NAME_syntax };

ClassDecl(tokeniser_decls,
          var_tokeniser, send_tokeniser, get_tokeniser, rc_tokeniser,
          2, tokeniser_termnames,
          "$Rev$");

status
makeClassTokeniser(Class class)
{ declareClass(class, &tokeniser_decls);

  setCloneFunctionClass(class, cloneTokeniser);
  cloneStyleVariableClass(class, NAME_syntax,  NAME_reference);
  cloneStyleVariableClass(class, NAME_symbols, NAME_reference);
  cloneStyleVariableClass(class, NAME_source,  NAME_reference);
  cloneStyleVariableClass(class, NAME_stack,   NAME_nil);

  EndOfFile = globalObject(NAME_endOfFile, ClassConstant,
			   NAME_endOfFile,
			   CtoString("End-of-file marker"),
			   EAV);

  succeed;
}
