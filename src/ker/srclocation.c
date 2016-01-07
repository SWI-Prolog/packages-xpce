/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2013, University of Amsterdam
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
initialiseSourceLocation(SourceLocation loc, Name file, Int line)
{ if ( isDefault(line) )
    line = (Int) NIL;

  assign(loc, file_name, file);
  assign(loc, line_no,   line);

  succeed;
}


static status
copySourceLocation(SourceLocation loc1, SourceLocation loc2)
{ assign(loc1, file_name, loc2->file_name);
  assign(loc1, line_no,   loc2->line_no);

  succeed;
}


static SourceLocation
getConvertSourceLocation(SourceLocation loc, Any spec)
{ if ( instanceOfObject(spec, ClassFile) )
  { FileObj f = spec;
    Name abs = get(f, NAME_absolutePath, EAV);

    if ( abs )
      answer(newObject(ClassSourceLocation, abs, EAV));
  } else /* if ( instanceOfObject(spec, ClassCharArray) */
  { CharArray ca = spec;
    String s = &ca->data;
    int i;

    if ( (i=str_rindex(s, ':')) > 0 )
    { char lb[20];
      size_t j;
      string fn;

      i++;
      for(j=0; j<sizeof(lb)-1 && i+j<s->s_size; j++)
      { int c = str_fetch(s, i+j);

	if ( isdigit(c) )
	  lb[j] = c;
	else
	  goto noline;
      }
      if ( j == 0 || j >= sizeof(lb)-1 )
	goto noline;

      lb[j] = EOS;
      fn = *s;
      fn.s_size = i-1;
      answer(newObject(ClassSourceLocation,
		       StringToName(&fn), toInt(atol(lb)), EAV));
    }

    noline:
    answer(newObject(ClassSourceLocation, spec, EAV));
  }

  fail;
}


static Name
getPathSourceLocation(SourceLocation loc)
{ char *name = strName(loc->file_name);

  if ( *name == '/' || *name == '.' )
    answer(loc->file_name);
  else
  { Name home;
    char buf[LINESIZE];

    TRY(home = get(PCE, NAME_home, EAV));
    sprintf(buf, "%s/src/%s", strName(home), name);

    answer(CtoName(buf));
  }
}


static StringObj
getPrintNameSourceLocation(SourceLocation loc)
{ static Name fmt = NULL;

  if ( !fmt )
    fmt = CtoName("%s:%d");

  answer(answerObject(ClassString, fmt, loc->file_name, loc->line_no, EAV));
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "file=name", "line=[int]*" };

/* Instance Variables */

static vardecl var_sourceLocation[] =
{ IV(NAME_fileName, "name", IV_BOTH,
     NAME_location, "Name of the file in which the source resides"),
  IV(NAME_lineNo, "int*", IV_BOTH,
     NAME_location, "Starting line number of the source")
};

/* Send Methods */

static senddecl send_sourceLocation[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseSourceLocation,
     DEFAULT, "Create from file_name and line_no"),
  SM(NAME_copy, 1, "source_location", copySourceLocation,
     NAME_copy, "Copy file_name and line_no from argument")
};

/* Get Methods */

static getdecl get_sourceLocation[] =
{ GM(NAME_convert, 1, "source_location", "char_array|file", getConvertSourceLocation,
     DEFAULT, "Convert `file', `path-name' and `path-name:line-no'"),
  GM(NAME_path, 0, "name", NULL, getPathSourceLocation,
     NAME_path, "Get absolute path-name of source file"),
  GM(NAME_printName, 0, "string", NULL, getPrintNameSourceLocation,
     NAME_textual, "Printed representation as %s:%d")
};

/* Resources */

#define rc_sourceLocation NULL
/*
static classvardecl rc_sourceLocation[] =
{
};
*/

/* Class Declaration */

static Name sourceLocation_termnames[] = { NAME_fileName, NAME_lineNo };

ClassDecl(sourceLocation_decls,
          var_sourceLocation, send_sourceLocation,
	  get_sourceLocation, rc_sourceLocation,
          2, sourceLocation_termnames,
          "$Rev$");

status
makeClassSourceLocation(Class class)
{ return declareClass(class, &sourceLocation_decls);
}

