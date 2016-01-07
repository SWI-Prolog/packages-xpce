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

forwards status XcloseCursor(CursorObj, DisplayObj);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Creating cursors.

In X, cursors can be created two ways: from the cursor  font  and from
two pixmaps.  PCE  supports both ways  to create a   cursor.  For this
reason various instantiation patterns for cursors exist.

?- new(C, cursor(Name))
	Create cursor from cursor font
?- new(C, cursor(Name, Source, [Mask], [X, Y]
	Create cursor from an image.  If Mask is not supplied it defaults
	to Source.  If X and Y are not supplied they default to (0,0) This
	function is in the first place meant to maintain compatibility with
	the SunView version of cursors.

Cursors from now on are shared objects like fonts.  That  is, a second
`new' to a  cursor of the  same name returns  the same  cursor object.
This because they are limited resources on the X-server.
-  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseCursor(CursorObj c, Name name,
		 Image image, Image mask,
		 Point hot,
		 Colour foreground, Colour background)
{ assign(c, name, name);

  if ( isDefault(image) )
  { if ( !ws_cursor_font_index(name) )
      return errorPce(NAME_noNamedCursor, name);

    assign(c, font_id, DEFAULT);
  } else
  { if ( isDefault(mask) )
    { if ( notNil(image->mask) )
	mask = image->mask;
      else
	mask = image;
    }
    if ( isDefault(hot) )
      hot  = newObject(ClassPoint, EAV);
    if ( notNil(image->hot_spot) )
      copyPoint(hot, image->hot_spot);

    assign(c, image,      image);
    assign(c, mask,       mask);
    assign(c, hot_spot,   hot);
    assign(c, foreground, foreground);
    assign(c, background, background);
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
        { "name=name*", "image=[image]", "mask=[image]", "hot_spot=[point]", "foreground=[colour]", "background=[colour]" };

/* Instance Variables */

static vardecl var_cursor[] =
{ IV(NAME_name, "name*", IV_GET,
     NAME_name, "Name of the cursor"),
  IV(NAME_fontId, "[int]*", IV_GET,
     NAME_appearance, "Id in X-cursor font"),
  IV(NAME_image, "image*", IV_GET,
     NAME_appearance, "User-defined image"),
  IV(NAME_mask, "image*", IV_GET,
     NAME_appearance, "User-defined mask"),
  IV(NAME_hotSpot, "point*", IV_GET,
     NAME_appearance, "User-defined hot spot"),
  IV(NAME_foreground, "[colour]*", IV_GET,
     NAME_appearance, "Foreground colour of the cursor"),
  IV(NAME_background, "[colour]*", IV_GET,
     NAME_appearance, "Background colour of the cursor")
};

/* Send Methods */

static senddecl send_cursor[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseCursor,
     DEFAULT, "Create from name or name, image, mask, hot_spot"),
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


