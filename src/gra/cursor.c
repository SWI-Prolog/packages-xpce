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

static status XcloseCursor(CursorObj, DisplayObj);

static status
initialiseCursor(CursorObj c, Name name, Image image, Point hot)
{ assign(c, name, name);

  if ( isDefault(image) )
  { if ( !ws_cursor_font_index(name) )
      return errorPce(NAME_noNamedCursor, name);
  } else
  { if ( isDefault(hot) )
    { hot = newObject(ClassPoint, EAV);
      if ( notNil(image->hot_spot) )
	copyPoint(hot, image->hot_spot);
    }

    assign(c, image,      image);
    assign(c, hot_spot,   hot);
  }

  if ( notNil(name) )
  { Name assoc = getAppendName(c->name, NAME_Cursor);

    protectObject(c);
    newAssoc(assoc, c);

    appendHashTable(CursorTable, c->name, c);
  }

  succeed;
}


static status
unlinkCursor(CursorObj c)
{ XcloseCursor(c, DEFAULT);

  succeed;
}


static CursorObj
getLookupCursor(Class class, Name name)
{ answer(getMemberHashTable(CursorTable, name));
}


static status
XopenCursor(CursorObj c, DisplayObj d)
{ return ws_create_cursor(c, d);
}


static status
XcloseCursor(CursorObj c, DisplayObj d)
{ ws_destroy_cursor(c, d);

  succeed;
}


static CursorObj
getConvertCursor(Class class, Name name)
{ CursorObj c;

  if ( (c = getMemberHashTable(CursorTable, name)) )
    answer(c);
  if ( syntax.uppercase &&
       (c = getMemberHashTable(CursorTable, CtoKeyword(strName(name)))) )
    answer(c);

  return answerObject(ClassCursor, name, EAV);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "name=name*", "image=[image]", "hot_spot=[point]" };

/* Instance Variables */

static vardecl var_cursor[] =
{ IV(NAME_name, "name*", IV_GET,
     NAME_name, "Name of the cursor"),
  IV(NAME_image, "image*", IV_GET,
     NAME_appearance, "User-defined image"),
  IV(NAME_hotSpot, "point*", IV_GET,
     NAME_appearance, "User-defined hot spot"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_storage, "Window System Reference")
};

/* Send Methods */

static senddecl send_cursor[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseCursor,
     DEFAULT, "Create from name or name or image & hot_spot"),
  SM(NAME_unlink, 0, NULL, unlinkCursor,
     DEFAULT, "Destroy the cursor"),
  SM(NAME_Xclose, 1, "display", XcloseCursor,
     NAME_x, "Destroy X-cursor on display"),
  SM(NAME_Xopen, 1, "display", XopenCursor,
     NAME_x, "Create X-cursor on display")
};

/* Get Methods */

static getdecl get_cursor[] =
{ GM(NAME_convert, 1, "cursor", "name", getConvertCursor,
     NAME_conversion, "Convert cursor-name to cursor"),
  GM(NAME_lookup, 1, "cursor", "name", getLookupCursor,
     NAME_oms, "Lookup from @cursors table")
};

/* Resources */

#define rc_cursor NULL
/*
static classvardecl rc_cursor[] =
{
};
*/

/* Class Declaration */

static Name cursor_termnames[] = { NAME_name };

ClassDecl(cursor_decls,
          var_cursor, send_cursor, get_cursor, rc_cursor,
          1, cursor_termnames,
          "$Rev$");


status
makeClassCursor(Class class)
{ declareClass(class, &cursor_decls);

  cloneStyleClass(class, NAME_none);
  CursorTable = globalObject(NAME_cursors, ClassHashTable, toInt(32), EAV);
  ws_init_cursor_font();

  succeed;
}
