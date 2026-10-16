/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

/* Class `markup' is a string that holds Pango markup, e.g., "Some
 * <b>bold</b> text".  Being a string, it can be used wherever text is
 * accepted.  Its class tells receivers that the text is markup: a
 * label renders it, while targets that cannot render markup use
 * <-text.
 *
 * Formatting a markup (->initialise with arguments and ->format)
 * makes the format markup and escapes the text of the arguments,
 * unless an argument is itself a markup.  See str_writefv_markup().
 */

static status
validateMarkup(StringObj m)
{ StringObj error;

  if ( !ws_parse_markup((CharArray)m, NULL, &error) )
    return errorPce(m, NAME_syntaxError, error);

  succeed;
}


static status
initialiseMarkupv(StringObj m, CharArray fmt, int argc, Any *argv)
{ TRY(initialiseStringv(m, fmt, argc, argv));

  return validateMarkup(m);
}


static StringObj
getConvertMarkup(Class class, CharArray text)
{ answer(answerObject(ClassMarkup, name_procent_s, text, EAV));
}


static StringObj
getTextMarkup(StringObj m)
{ StringObj plain;

  if ( ws_parse_markup((CharArray)m, &plain, NULL) )
    answer(plain);

  fail;
}


/* The text of formatting the markup `fmt', without the markup.  If the
 * result is not valid markup, it is used as is.  This is how
 * str_writefv() formats a markup.
 */

status
str_writefv_markup_text(PceString s, CharArray fmt,
			int argc, const Any *argv)
{ string m;
  StringObj plain = NULL;

  TRY(str_writefv_markup(&m, fmt, argc, argv));
  CharArray ca = StringToScratchCharArray(&m);
  ws_parse_markup(ca, &plain, NULL);
  Any text = (plain ? (Any)plain : (Any)ca);
  status rc = str_writefv(s, (CharArray)name_procent_s, 1, &text);
  doneScratchCharArray(ca);
  if ( plain )
    doneObject(plain);
  str_unalloc(&m);

  return rc;
}


/* The text `text' as markup that shows it as is.  The result is a
 * string rather than a markup, so a receiver that displays its label
 * with a markup_text can tell an escaped plain label from a markup.
 */

StringObj
escapedMarkupString(CharArray text)
{ string s;
  Any av[1] = { text };
  StringObj str;

  if ( !str_writefv_markup(&s, (CharArray)name_procent_s, 1, av) )
    return NULL;
  str = StringToString(&s);
  str_unalloc(&s);

  return str;
}


/* The markup for displaying `label' with a markup_text: a markup as
 * is, other text escaped.
 */

CharArray
labelMarkup(CharArray label)
{ if ( instanceOfObject(label, ClassMarkup) )
    return label;

  return (CharArray)escapedMarkupString(label);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

static char *T_initialise[] =
	{ "format=[char_array]", "argument=any ..." };

static senddecl send_markup[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseMarkupv,
     DEFAULT, "Create from markup or ->format with escaped arguments")
};

static getdecl get_markup[] =
{ GM(NAME_convert, 1, "markup", "text=char_array", getConvertMarkup,
     NAME_conversion, "Convert text to markup, escaping it"),
  GM(NAME_text, 0, "string", NULL, getTextMarkup,
     NAME_contents, "The text without the markup")
};

ClassDecl(markup_decls,
	  NULL, send_markup, get_markup, NULL,
	  ARGC_INHERIT, NULL);


status
makeClassMarkup(Class class)
{ return declareClass(class, &markup_decls);
}
