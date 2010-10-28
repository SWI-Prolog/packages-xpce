/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <h/kernel.h>
#include <h/unix.h>

static status	appendString(StringObj, CharArray);
static status	setString(StringObj str, String s);

StringObj
create_string_from_str(String s, int tmp)
{ string s2;
  CharArray c;
  StringObj str;
  charA *do_free = NULL;

  if ( s->iswide )
  { const charW *txt = s->s_textW;
    const charW *end = &txt[s->size];
    charA *p;

    for( ; txt < end; txt++ )
    { if ( *txt > 0xff )
	goto canonical;
    }

    str_inithdr(&s2, FALSE);
    s2.size = s->size;
    if ( !(s2.s_textA = alloca(s->size)) )
    { s2.s_textA = pceMalloc(s->size);
      do_free = s2.s_textA;
    }
    for(txt = s->s_textW, p = s2.s_textA; txt < end; )
      *p++ = *txt++;

    s = &s2;
  }

canonical:
  c = StringToScratchCharArray(s);
  if ( tmp )
    str = tempObject(ClassString, name_procent_s, c, EAV);
  else
    str = answerObject(ClassString, name_procent_s, c, EAV);
  doneScratchCharArray(c);

  if ( do_free )
    pceFree(do_free);

  return str;
}


StringObj
StringToString(String s)
{ return create_string_from_str(s, FALSE);
}


StringObj
StringToTempString(String s)
{ return create_string_from_str(s, TRUE);
}


StringObj
CtoString(const char *s)
{ CharArray c = CtoScratchCharArray(s);
  StringObj str =  answerObject(ClassString, name_procent_s, c, EAV);
  doneScratchCharArray(c);

  return str;
}


StringObj
staticCtoString(const char *s)
{ CharArray c = CtoScratchCharArray(s);
  StringObj str;

  c->data.readonly = TRUE;
  str = answerObject(ClassString, name_procent_s, c, EAV);
  doneScratchCharArray(c);

  return str;
}


StringObj
CtoTempString(char *s)
{ CharArray c = CtoScratchCharArray(s);
  StringObj str =  tempObject(ClassString, name_procent_s, c, EAV);
  doneScratchCharArray(c);

  return str;
}


static StringObj
getModifyString(StringObj str, CharArray value)
{ answer(answerObject(classOfObject(str), name_procent_s, value, EAV));
}


static void
prepareWriteString(StringObj s)
{ if ( s->data.readonly )
    setString(s, &s->data);
}


static void
promoteString(StringObj s)
{ if ( !s->data.iswide )
  { string ws;
    const charA *f = s->data.s_textA;
    const charA *e = &f[s->data.size];
    charW *t;

    str_inithdr(&ws, TRUE);
    ws.size = s->data.size;
    str_alloc(&ws);

    for(t=ws.s_textW; f<e;)
      *t++ = *f++;

    s->data = ws;
  }
}


status
initialiseStringv(StringObj str, CharArray fmt, int argc, Any *argv)
{ if ( isDefault(fmt) )
  { str_inithdr(&str->data, FALSE);
    str->data.size = 0;
    str_alloc(&str->data);
  } else if ( (Name) fmt == name_procent_s &&
	      argc == 1 && instanceOfObject(argv[0], ClassCharArray) )
  { CharArray v = argv[0];

    str_cphdr(&str->data, &v->data);
    if ( v->data.readonly )
    { str->data.s_textA = v->data.s_textA;

      DEBUG(NAME_readOnly, Cprintf("Shared %s\n", pp(str)));
    } else
    { str_alloc(&str->data);
      memcpy(str->data.s_textA, v->data.s_textA, str_datasize(&v->data));
    }
  } else
    TRY(str_writefv(&str->data, fmt, argc, argv));

  succeed;
}


static StringObj
getCopyString(StringObj s)
{ answer(answerObject(classOfObject(s), name_procent_s, s, EAV));
}


static StringObj
convertString(Class class, Any obj)
{ if ( instanceOfObject(obj, ClassString) )
    answer((StringObj) obj);
  else if ( instanceOfObject(obj, ClassCharArray) )
    answer(answerObject(ClassString, name_procent_s, obj, EAV));
  else
  { char *s = toCharp(obj);

    if ( s != NULL )
      answer(CtoString(s));
    else
      fail;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Load/store a string to/from file. Format:

<string>	::= <charp>
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeString(StringObj s, FileObj file)
{ TRY(storeSlotsObject(s, file));

  return storeStringFile(file, &s->data);
}


static status
loadString(StringObj s, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(s, fd, def));

  return loadStringFile(fd, &s->data);
}


static status
formatString(StringObj s, CharArray fmt, int argc, Any *argv)
{ prepareWriteString(s);

  str_unalloc(&s->data);
  str_writefv(&s->data, fmt, argc, argv);

  return setString(s, &s->data);
}


status
valueString(StringObj s1, CharArray s2)
{ if ( equalCharArray((CharArray) s1, s2, OFF) )
    succeed;

  return setString(s1, &s2->data);
}


static status
prependString(StringObj s1, StringObj s2)
{ return str_insert_string(s1, ZERO, &s2->data);
}


static status
ensureNlString(StringObj s1, CharArray s2)
{ if ( s1->data.size > 0 && str_fetch(&s1->data, s1->data.size-1) != '\n' )
    str_insert_string(s1, DEFAULT, str_nl(&s1->data));

  if ( notDefault(s2) )
    return appendString(s1, s2);

  succeed;
}


static status
ensureSuffixString(StringObj s, CharArray suff, BoolObj ign_case)
{ if ( !suffixCharArray((CharArray) s, suff, ign_case) )
    appendString(s, suff);

  succeed;
}


static status
newlineString(StringObj s, Int times)
{ int tms;

  if ( isDefault(times) )
    times = ONE;
  tms = valInt(times);

  { String nl = str_nl(&s->data);
    LocalString(buf, s->data.iswide, nl->size * tms);
    int i;

    for(i=0; i<tms; i++)
      str_ncpy(buf, i * nl->size, nl, 0, nl->size);
    buf->size = nl->size * tms;

    return str_insert_string(s, DEFAULT, buf);
  }
}


status
insertCharacterString(StringObj str, Int chr, Int where, Int times)
{ int tms = isDefault(times) ? 1 : valInt(times);
  wint_t c = valInt(chr);
  int iswide = (c <= 0xff);
  LocalString(buf, iswide, tms);
  int i;

  for(i=0; i<tms; i++)
    str_store(buf, i, c);
  buf->size = tms;
  str_insert_string(str, where, buf);

  succeed;
}


static status
appendString(StringObj s1, CharArray s2)
{ return str_insert_string(s1, DEFAULT, &s2->data);
}


static status
stripString(StringObj str, Name where)
{ String s = &str->data;
  int size = s->size;
  int from = 0;
  int to = size;
  string buf;

  if ( where != NAME_trailing )
  { while( from < size && iswspace(str_fetch(s, from)))
      from++;
  }

  if ( where != NAME_leading )
  { while( to > from && iswspace(str_fetch(s, to-1)) )
      to--;
  }

  str_cphdr(&buf, s);
  buf.s_text = str_textp(s, from);
  buf.size = to - from;

  return setString(str, &buf);
}


static status
untabifyString(StringObj str, Any tabs)
{ Int n;

  if ( isDefault(tabs) )
    tabs = toInt(8);

  if ( instanceOfObject(tabs, ClassVector) )
  { int size = valInt(((Vector)tabs)->size);
    Any *elements = ((Vector)tabs)->elements;
    int maxtab = -1;
    int n;

    for(n = 0; n<size; n++)
    { if ( !isInteger(elements[n]) )
	return errorPce(elements[n], NAME_unexpectedType, TypeInt);
      if ( n <= maxtab )
	return errorPce(str, NAME_badTabStopVector);
      maxtab = n;
    }

    { int size = str->data.size;
      String s = &str->data;
      LocalString(buf, s->iswide, size + maxtab);
      int i=0, o=0, col=0;

      for( ; i < size; i++ )
      { wint_t c = str_fetch(s, i);

	if ( c == '\t' )
	{ int destcol = col+1;

	  for(n=0; n<size; n++)
	  { if ( valInt(elements[n]) >= destcol )
	    { destcol = valInt(elements[n]);
	      break;
	    }
	  }

	  do
	  { str_store(buf, o++, ' ');
	    col++;
	  } while ( col != destcol );
	} else
	{ str_store(buf, o++, c);
	  if ( c == '\n' )
	    col = 0;
	  else
	    col++;
	}
      }
      buf->size = o;

      return setString(str, buf);
    }
  } else if ( (n = checkType(tabs, TypeInt, NIL)) )
  { int size = str->data.size;
    int d = valInt(n);
    String s = &str->data;
    int tabs = str_count_chr(s, 0, size, '\t');
    LocalString(buf, s->iswide, size + d * tabs);
    int i=0, o=0, col=0;

    for( ; i < size; i++ )
    { wint_t c = str_fetch(s, i);

      if ( c == '\t' )
      { do
	{ str_store(buf, o++, ' ');
	  col++;
	} while ( col % d );
      } else
      { str_store(buf, o++, c);
	if ( c == '\n' )
	  col = 0;
	else
	  col++;
      }
    }
    buf->size = o;

    return setString(str, buf);
  }

  fail;
}


status
upcaseString(StringObj s)
{ prepareWriteString(s);

  str_upcase(&s->data, 0, s->data.size);
  return setString(s, &s->data);
}


static status
downcaseString(StringObj s)
{ prepareWriteString(s);

  str_downcase(&s->data, 0, s->data.size);
  return setString(s, &s->data);
}


static status
truncateString(StringObj s, Int n)
{ return deleteString(s, n, DEFAULT);
}


static status
translateString(StringObj str, Int c1, Int c2)
{ wint_t f = valInt(c1);
  int changed = 0;
  String s = &str->data;
  int size = s->size;
  int i = 0;

  if ( notNil(c2) )
  { wint_t t = valInt(c2);

    if ( t > 0xff )
      promoteString(str);
    else
      prepareWriteString(str);

    for(;;)
    { if ( (i = str_next_index(s, i, f)) >= 0 )
      { str_store(s, i++, t);
	changed++;
      } else
	break;
    }

    if ( changed )
      setString(str, &str->data);	/* forward changes */
  } else				/* delete c1's */
  { LocalString(buf, s->iswide, size);
    int o = 0;

    for(;;)
    { int ni;

      if ( (ni = str_next_index(s, i, f)) >= 0 )
      { str_ncpy(buf, o, s, i, ni-i);
	o += ni-i;
	i = ni+1;
	changed++;
      } else
	break;
    }
    if ( changed )
    { str_ncpy(buf, o, s, i, size-i);
      o += size-i;
      buf->size = o;

      setString(str, buf);
    }
  }

  succeed;
}


static status
characterString(StringObj str, Int index, Int chr)
{ int i = valInt(index);
  wint_t c = valInt(chr);

  if ( i <  0 || i >= str->data.size )
    fail;

  if ( str_fetch(&str->data, i) != c )
  { if ( c > 0xff && !str->data.iswide )
      promoteString(str);
    else
      prepareWriteString(str);
    str_store(&str->data, i, c);
    setString(str, &str->data);
  }

  succeed;
}


status
deleteString(StringObj str, Int start, Int length)
{ String s = &str->data;
  int size = s->size;
  int f = valInt(start);
  int e = (isDefault(length) ? size : valInt(length)) + f - 1;
  int d;

  if ( f <  0    ) s = 0;
  if ( f >= size ) succeed;
  if ( e <  f    ) succeed;
  if ( e >= size )
    e = size - 1;
  d = e - f + 1;

  { LocalString(buf, s->iswide, size-d);

    str_ncpy(buf, 0, s, 0, f);
    str_ncpy(buf, f, s, e+1, size - (e+1));
    buf->size = size-d;

    setString(str, buf);
  }

  succeed;
}


status
insertString(StringObj s1, Int n, CharArray s2)
{ return str_insert_string(s1, n, &s2->data);
}


 		/********************************
		*    NON-PCE-TYPE MANIPULATION  *
		*********************************/

static status
setString(StringObj str, String s)
{ Class class = classOfObject(str);

  if ( str->data.s_text != s->s_text ||
       str_allocsize(&str->data) != str_allocsize(s) ||
       str->data.readonly )
  { string s2 = *s;

    DEBUG(NAME_readOnly,
	  if ( str->data.readonly )
	    Cprintf("Copying %s", pp(str)));

    str_alloc(&s2);
    memcpy(s2.s_textA, s->s_textA, str_datasize(s));
    str_unalloc(&str->data);
    str->data = s2;
  } else
    str->data = *s;

  if ( notNil(class->changed_messages) )
    changedObject(str, NAME_text, EAV);

  succeed;
}


status
str_insert_string(StringObj str, Int where, String s)
{ int sz = str->data.size;
  int iswide = (str->data.iswide || s->iswide);
  LocalString(buf, iswide, sz + s->size);
  int p = (isDefault(where) ? sz : valInt(where));

  if ( p < 0  ) p = 0;
  if ( p > sz ) p = sz;

  str_ncpy(buf, 0, &str->data, 0, p);
  str_ncpy(buf, p, s, 0, s->size);
  str_ncpy(buf, p+s->size, &str->data, p, str->data.size - p);
  buf->size = sz + s->size;

  return setString(str, buf);
}


					/* used in text <-selected */
StringObj
getSubString(StringObj n, Int start, Int end)
{ string s;
  int x, y;
  int len = n->data.size;

  x = valInt(start);
  y = (isDefault(end) ? len : valInt(end));
  if ( x < 0 || y > len || x > y )
    fail;

  str_cphdr(&s, &n->data);
  s.size = y-x;
  if ( isstrA(&n->data) )
    s.s_textA = &n->data.s_textA[x];
  else
    s.s_textW = &n->data.s_textW[x];

  answer(StringToString(&s));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_insert[] =
        { "at=[int]", "text=char_array" };
static char *T_character[] =
        { "at=int", "char=char" };
static char *T_insertCharacter[] =
        { "char=char", "at=[0..]", "times=[0..]" };
static char *T_format[] =
        { "format=[char_array]", "argument=any ..." };
static char *T_translate[] =
        { "from=char", "into=char*" };
static char *T_delete[] =
        { "from=int", "length=[int]" };
static char *T_ensureSuffix[] =
	{ "text=char_array", "ignore_case=[bool]" };

/* Instance Variables */

#define var_string NULL
/*
vardecl var_string[] =
{
};
*/

/* Send Methods */

static senddecl send_string[] =
{ SM(NAME_initialise, 2, T_format, initialiseStringv,
     DEFAULT, "Create a string, initialise as ->format"),
  SM(NAME_downcase, 0, NULL, downcaseString,
     NAME_case, "Change all letters in string to lower case"),
  SM(NAME_upcase, 0, NULL, upcaseString,
     NAME_case, "Change all letters in string to upper case"),
  SM(NAME_append, 1, "text=char_array", appendString,
     NAME_content, "Append to the string"),
  SM(NAME_character, 2, T_character, characterString,
     NAME_content, "Change character at 0-based index"),
  SM(NAME_delete, 2, T_delete, deleteString,
     NAME_content, "Delete range from 0-based start and length"),
  SM(NAME_ensureNl, 1, "[char_array]", ensureNlString,
     NAME_content, "Ensure string has trailing newline [and append string]"),
  SM(NAME_ensureSuffix, 2, T_ensureSuffix, ensureSuffixString,
     NAME_content, "Ensure string has indicated suffix"),
  SM(NAME_insert, 2, T_insert, insertString,
     NAME_content, "Insert string at 0-based index"),
  SM(NAME_insertCharacter, 3, T_insertCharacter, insertCharacterString,
     NAME_content, "Insert times character(s) at location"),
  SM(NAME_newline, 1, "times=[0..]", newlineString,
     NAME_content, "Append a newline to string"),
  SM(NAME_prepend, 1, "char_array", prependString,
     NAME_content, "Add argument at the beginning"),
  SM(NAME_strip, 1, "[{leading,trailing}]", stripString,
     NAME_content, "Strip leading/trailing blanks"),
  SM(NAME_translate, 2, T_translate, translateString,
     NAME_content, "Map occurrences of 1-st arg into 2-nd arg"),
  SM(NAME_truncate, 1, "int", truncateString,
     NAME_content, "Truncate string to argument characters"),
  SM(NAME_value, 1, "text=char_array", valueString,
     NAME_copy, "Set the contents of the string"),
  SM(NAME_format, 2, T_format, formatString,
     NAME_format, "Format (like printf) in string"),
  SM(NAME_untabify, 1, "tabs=[int|vector]", untabifyString,
     NAME_indentation, "Replace tab characters by spaces")
};

/* Get Methods */

static getdecl get_string[] =
{ GM(NAME_convert, 1, "string", "any", convertString,
     DEFAULT, "Convert name, int, real, etc."),
  GM(NAME_modify, 1, "string", "char_array", getModifyString,
     DEFAULT, "Make modified version"),
  GM(NAME_copy, 0, "string", NULL, getCopyString,
     NAME_copy, "Copy with the same text")
};

/* Resources */

#define rc_string NULL
/*
static classvardecl rc_string[] =
{
};
*/

/* Class Declaration */

static Name string_termnames[] = { NAME_value };

ClassDecl(string_decls,
          var_string, send_string, get_string, rc_string,
          1, string_termnames,
          "$Rev$");

status
makeClassString(Class class)
{ declareClass(class, &string_decls);
  setLoadStoreFunctionClass(class, loadString, storeString);

  succeed;
}

