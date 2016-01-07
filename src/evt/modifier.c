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

static HashTable ModifierTable;

static status
initialiseModifier(Modifier m, Name shift, Name ctrl, Name meta)
{ assign(m, shift,   shift);
  assign(m, control, ctrl);
  assign(m, meta,    meta);

  succeed;
}


static Modifier
getConvertModifier(Class class, Name name)
{ Modifier m;

  if ( (m = getMemberHashTable(ModifierTable, name)) )
    answer(m);
  else
  { String s = &name->data;
    int i, size = s->s_size;
    Name shift   = NAME_up;
    Name control = NAME_up;
    Name meta    = NAME_up;

    for(i=0; i<size; i++)
    { wint_t c = str_fetch(s, i);

      switch(towlower(c))
      { case 's':
	  shift = NAME_down;
	  break;
	case 'c':
	  control = NAME_down;
	  break;
	case 'm':
	  meta = NAME_down;
	  break;
	default:
	  fail;
      }
    }

    m = answerObject(ClassModifier, shift, control, meta, EAV);
    protectObject(m);
    appendHashTable(ModifierTable, name, m);

    answer(m);
  }
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "shift=[{up,down}]", "control=[{up,down}]", "meta=[{up,down}]" };

/* Instance Variables */

static vardecl var_modifier[] =
{ IV(NAME_shift, "[{up,down}]", IV_BOTH,
     NAME_modifier, "Condition on shift"),
  IV(NAME_control, "[{up,down}]", IV_BOTH,
     NAME_modifier, "Condition on control"),
  IV(NAME_meta, "[{up,down}]", IV_BOTH,
     NAME_modifier, "Condition on meta")
};

/* Send Methods */

static senddecl send_modifier[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseModifier,
     DEFAULT, "Create from shift, control and meta")
};

/* Get Methods */

static getdecl get_modifier[] =
{ GM(NAME_convert, 1, "modifier", "name", getConvertModifier,
     NAME_conversion, "Convert name, consisting of {s|m|c}")
};

/* Resources */

#define rc_modifier NULL
/*
static classvardecl rc_modifier[] =
{
};
*/

/* Class Declaration */

static Name modifier_termnames[] = { NAME_shift, NAME_control, NAME_meta };

ClassDecl(modifier_decls,
          var_modifier, send_modifier, get_modifier, rc_modifier,
          3, modifier_termnames,
          "$Rev$");

status
makeClassModifier(Class class)
{ declareClass(class, &modifier_decls);

  MODIFIER_shift   = globalObject(NAME_ModifierShift, ClassModifier,
				  NAME_down, NAME_up, NAME_up, EAV);
  MODIFIER_control = globalObject(NAME_ModifierControl, ClassModifier,
				  NAME_up, NAME_down, NAME_up, EAV);
  MODIFIER_allup   = globalObject(NAME_ModifierAllUp, ClassModifier,
				  NAME_up, NAME_up, NAME_up, EAV);
  ModifierTable = globalObject(NAME_modifiers, ClassHashTable, EAV);

  succeed;
}

