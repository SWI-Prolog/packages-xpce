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

static void initVars(void);
static VarBinding findVarEnvironment(VarEnvironment ev, Var v);

static HashTable VarTable;

static status
initialiseVar(Var v, Type type, Name name, Any value)
{ if ( isDefault(type) )
    type = TypeUnchecked;
  if ( isDefault(name) )		/* local var */
    name = NIL;

  assign(v, name, name);
  assign(v, type, type);
  assign(v, global_value, value);

  if ( notNil(name) )
  { if ( getMemberHashTable(VarTable, name) )
      errorPce(v, NAME_redeclaredVar);
    appendHashTable(VarTable, name, v);
    protectObject(v);
  }

  return initialiseFunction((Function) v);
}


static status
unlinkVar(Var v)
{ VarEnvironment ev = varEnvironment;

  for(; ev; ev = ev->parent)
  { VarBinding b;

    if ( (b = findVarEnvironment(ev, v)) )
      b->variable = NULL;
  }

  succeed;
}


static Var
getConvertVar(Class class, Any name)
{ answer(getMemberHashTable(VarTable, name));
}


/*				see assignVar()
static status
valueVar(Var v, Any value)
{ if ( v->value != value )
  { if ( isObject(v->value) )
      delCodeReference(v->value);
    v->value = value;
    if ( isObject(value) )
      addCodeReference(value);
  }

  succeed;
}
*/

Any
getValueVar(const Var v)
{ VarEnvironment ev = varEnvironment;

  for(; ev; ev = ev->parent)
  { VarBinding b = findVarEnvironment(ev, v);

    if ( b )
      return b->value;
  }

  return v->global_value;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
        { "type=[type]", "name=[name]", "value=[any]" };
static char *T_assign[] =
        { "value=any", "scope=[{local,outer,global}]" };

/* Instance Variables */

static vardecl var_var[] =
{ IV(NAME_Name, "name*", IV_GET,
     NAME_name, "Name of the var"),
  IV(NAME_Type, "type", IV_BOTH,
     NAME_type, "Type of the <-_value"),
  IV(NAME_GlobalValue, "any", IV_GET,
     NAME_abort, "Global value of the var")
};

/* Send Methods */

static senddecl send_var[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseVar,
     DEFAULT, "Create var from name and value"),
  SM(NAME_unlink, 0, NULL, unlinkVar,
     DEFAULT, "Release code reference of value"),
  SM(NAME_assign, 2, T_assign, assignVar,
     NAME_value, "Assign value to variable (with scope)")
};

/* Get Methods */

static getdecl get_var[] =
{ GM(NAME_convert, 1, "var", "name", getConvertVar,
     NAME_conversion, "Converts name to var from @variables"),
  GM(NAME_Execute, 0, "unchecked", NULL, getValueVar,
     NAME_execute, "Current value of the variable"),
  GM(NAME_Value, 0, "unchecked", NULL, getValueVar,
     NAME_value, "Current value of the variable")
};

/* Resources */

#define rc_var NULL
/*
static classvardecl rc_var[] =
{
};
*/

/* Class Declaration */

static Name var_termnames[] = { NAME_Value };

ClassDecl(var_decls,
          var_var, send_var, get_var, rc_var,
          1, var_termnames,
          "$Rev$");

status
makeClassVar(Class class)
{ declareClass(class, &var_decls);
  saveStyleClass(class, NAME_external);

  VarTable = globalObject(NAME_variables, ClassHashTable, EAV);
  initVars();

  succeed;
}


static Var
initVar(Name name, char *type, Any value)
{ return globalObject(name, ClassVar, CtoType(type), name, value, EAV);
}


static Var
initGrVar(Name ref, Name name)
{ return globalObject(ref, ClassVar, TypeInt, name, ZERO, EAV);
}


static void
initVars(void)
{ int n;

  RECEIVER       = initVar(NAME_receiver,	"object*", NIL);
  RECEIVER_CLASS = initVar(NAME_receiverClass,	"class*",  NIL);
  EVENT		 = initVar(NAME_event,		"event*",  NIL);
  SELECTOR	 = initVar(NAME_selector,	"name*",   NIL);
  REPORTEE	 = initVar(NAME_reportee,	"chain*",  NIL);

  VarX		 = initGrVar(NAME_xVar, NAME_x);
  VarY		 = initGrVar(NAME_yVar, NAME_y);
  VarW		 = initGrVar(NAME_wVar, NAME_w);
  VarH		 = initGrVar(NAME_hVar, NAME_h);
  VarW2		 = initGrVar(NAME_w2Var, NAME_w2);
  VarH2		 = initGrVar(NAME_h2Var, NAME_h2);
  VarXref	 = initGrVar(NAME_xrefVar, NAME_xref);
  VarYref	 = initGrVar(NAME_yrefVar, NAME_yref);

  for(n = 1; n <= FWD_PCE_MAX_ARGS; n++)
  { char varname[100];

    sprintf(varname, "arg%d", n);
    Arg(n) = initVar(CtoName(varname), "unchecked", DEFAULT);
  }
}


		/********************************
		*          ENVIRONMENTS		*
		********************************/

#define sizeofVarExtension(n) ((int)(intptr_t)(&((VarExtension)NULL)->bindings[n]))

#define EXTBLOCKSIZE 8

static VarBinding
findVarEnvironment(const VarEnvironment ev, const Var v)
{ int i;
  VarBinding b;

  b = ev->bindings; i = 0;
  while( i < ev->size )
  { if ( b->variable == v )
      return b;

    if ( ++i == BINDINGBLOCKSIZE && ev->extension )
      b = ev->extension->bindings;
    else
      b++;
  }

  return NULL;
}


static VarExtension
expandVarExtension(VarExtension ext, int size)
{ if ( ext == NULL )
  { ext = alloc(sizeofVarExtension(EXTBLOCKSIZE));
    ext->allocated = EXTBLOCKSIZE;
    return ext;
  } else if ( size > ext->allocated )
  { int a = ((size + EXTBLOCKSIZE - 1) / EXTBLOCKSIZE) * EXTBLOCKSIZE;
    int i;

    VarExtension new = alloc(sizeofVarExtension(a));
    new->allocated = a;
    for(i=0; i<ext->allocated; i++)
      new->bindings[i] = ext->bindings[i];
    unalloc(sizeofVarExtension(ext->allocated), ext);

    return new;
  } else

    return ext;
}


static VarBinding
appendVarEnvironment(VarEnvironment ev, Var v, Any value)
{ VarBinding b;

  DEBUG(NAME_var, Cprintf("Appending %s to env %p\n", pp(v), ev));

  if ( ev->size < BINDINGBLOCKSIZE )
  { b = &ev->bindings[ev->size++];
  } else
  { int ext =  ev->size - BINDINGBLOCKSIZE;

    ev->extension = expandVarExtension(ev->extension, ext+1);
    b = &ev->extension->bindings[ext];
  }

  b->variable = v;
  b->value = value;
  if ( isObject(b->value) )
    addCodeReference(b->value);

  return b;
}


void
popVarEnvironment(void)
{ int i;
  VarBinding b;
  VarEnvironment ev = varEnvironment;

  b = ev->bindings; i = 0;
  while( i < ev->size )
  { if ( isObject(b->value) )
      delCodeReference(b->value);

    DEBUG(NAME_var, Cprintf("Restoring %s to %s\n",
			    pp(b->variable), pp(b->value)));

    if ( ++i == BINDINGBLOCKSIZE && ev->extension )
      b = ev->extension->bindings;
    else
      b++;
  }

  if ( ev->extension )
    unalloc(sizeofVarExtension(ev->extension->allocated), ev->extension);

  varEnvironment = ev->parent;
}


static void
valueVarBinding(VarBinding b, Any value)
{ b->value = value;
  if ( isObject(b->value) )
    addCodeReference(b->value);
}


status
assignVar(Var v, Any value, Name scope)
{ if ( isDefault(scope) || scope == NAME_local )
  { if ( varEnvironment )
    { VarBinding b = findVarEnvironment(varEnvironment, v);
      if ( b )
	valueVarBinding(b, value);
      else
	appendVarEnvironment(varEnvironment, v, value);
    } else
    { assign(v, global_value, value);
    }
  } else if ( scope == NAME_outer )
  { VarBinding b;

    if ( varEnvironment )
    { if ( !(b = findVarEnvironment(varEnvironment, v)) )
	b = appendVarEnvironment(varEnvironment, v, value);

      valueVarBinding(b, value);
    }
  } else /* if ( scope == NAME_global ) */
  { VarEnvironment ev = varEnvironment;

    for(; ev; ev = ev->parent)
    { VarBinding b;

      if ( (b = findVarEnvironment(ev, v)) )
	valueVarBinding(b, value);
    }
    assign(v, global_value, value);
  }

  DEBUG(NAME_var, Cprintf("assignVar(%s) --> %s\n",
			  pp(v), pp(value)));

  succeed;
}
