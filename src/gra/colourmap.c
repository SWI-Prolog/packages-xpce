/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1996-2011, University of Amsterdam
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

static HashTable ColourMaps;		/* name --> maps */

static status
initialiseColourMap(ColourMap cm, Name name, Vector colours)
{ if ( isDefault(name) )
    name = NAME_system;
  if ( isDefault(colours) )
    colours = NIL;

  assign(cm, name,      name);
  assign(cm, colours,   colours);
  assign(cm, read_only, OFF);

  succeed;
}


static status
unlinkColourMap(ColourMap cm)
{ ws_unlink_colour_map(cm);

  succeed;
}


static ColourMap
getConvertColourMap(Class class, Name name)
{ ColourMap cm;
  int size;

  if ( ColourMaps && (cm = getMemberHashTable(ColourMaps, name)) )
    answer(cm);

  if ( isstrA(&name->data) &&
       sscanf(strName(name), "colour_cube_%d", &size) == 1 )
  { cm = newObject(ClassColourMap, name, NIL, EAV);
    lockObject(cm, ON);

    ws_colour_cube(cm, size);
    assign(cm, read_only, ON);
    answer(cm);
  }

  fail;
}


static ColourMap
getLookupColourMap(Class class, Name name)
{ return getConvertColourMap(class, name);
}


static Vector
getColoursColourMap(ColourMap cm)
{ if ( isNil(cm->colours) )
    ws_colour_map_colours(cm);

  if ( notNil(cm->colours) )
    answer(cm->colours);

  fail;
}


/* Type declaractions */

static char *T_initialise[] = { "name=[name]*", "colours=[vector]*" };

/* Instance Variables */

static vardecl var_colour_map[] =
{ IV(NAME_name, "name*", IV_GET,
     NAME_name, "Name (for lookup) of the colourmap"),
  IV(NAME_colours, "vector*", IV_NONE,
     NAME_storage, "Vector of colours defining the map"),
  IV(NAME_readOnly, "bool", IV_NONE,
     NAME_storage, "If @on, map cannot be changed"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_storage, "Window system handle")
};

/* Send Methods */

static senddecl send_colour_map[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseColourMap,
     DEFAULT, "Create from name and colours"),
  SM(NAME_unlink, 0, NULL, unlinkColourMap,
     DEFAULT, "Destroy system resources")
};

/* Get Methods */

static getdecl get_colour_map[] =
{ GM(NAME_lookup, 1, "colour_map", "name", getLookupColourMap,
     NAME_oms, "Reuse existing named colour_map"),
  GM(NAME_convert, 1, "colour_map", "name", getConvertColourMap,
     DEFAULT, "Allow using name to specify a map"),
  GM(NAME_colours, 0, "vector*", NULL, getColoursColourMap,
     DEFAULT, "Get the colours of the map")
};

/* Resources */

#define rc_colour_map NULL
/*
static classvardecl rc_colour_map[] =
{
};
*/

/* Class Declaration */

static Name colour_map_termnames[] = { NAME_name, NAME_colours };

ClassDecl(colour_map_decls,
          var_colour_map, send_colour_map, get_colour_map, rc_colour_map,
          2, colour_map_termnames,
          "$Rev$");

status
makeClassColourMap(Class class)
{ declareClass(class, &colour_map_decls);

  ColourMaps = globalObject(NAME_colourMaps, ClassHashTable, toInt(8), EAV);

  succeed;
}
