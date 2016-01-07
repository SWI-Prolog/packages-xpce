/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2005-2011, University of Amsterdam
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

#define XPCE_PUBLIC_IMPL 1
#include <h/XPCE.h>

int
XPCE_define_classes(const XPCE_class_definition_t *classes)
{ for(; classes->name; classes++)
  { Class class = defineClass(CtoName(classes->name),
			      CtoName(classes->super),
			      staticCtoString(classes->summary),
			      classes->makefunction);

    if ( classes->global )
      *classes->global = class;
  }

  numberTreeClass(ClassObject, 0);

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE_declare_class() calls declareClass() after internalising all names,
so we can load the class without assigning all names at compiletime.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
charpToName(Name *n)
{ if ( !n )
    return;

  *n = CtoName((char *)*n);
}

static void
groupToName(Name *n)
{ if ( !n )
    *n = DEFAULT;			/* meaning inherit from superclass */

  *n = CtoName((char *)*n);
}

#define ToName(a)  charpToName(&a)
#define GToName(a) groupToName(&a)

static void
intern_vardef(vardecl *vd)
{ ToName(vd->name);
  GToName(vd->group);
}


static void
intern_send(senddecl *sd)
{ ToName(sd->name);
  GToName(sd->group);
}


static void
intern_get(getdecl *gd)
{ ToName(gd->name);
  GToName(gd->group);
}


static void
intern_cvardef(classvardecl *cvd)
{ ToName(cvd->name);
}


static void
intern_term_name(Name *np)
{ charpToName(np);
}


int
XPCE_declare_class(Class class, classdecl *decl)
{ int i;

  for(i=0; i<decl->nvar; i++)
    intern_vardef(&decl->variables[i]);
  for(i=0; i<decl->nsend; i++)
    intern_send(&decl->send_methods[i]);
  for(i=0; i<decl->nget; i++)
    intern_get(&decl->get_methods[i]);
  for(i=0; i<decl->nclassvars; i++)
    intern_cvardef(&decl->class_variables[i]);
  for(i=0; i<decl->term_arity; i++)
    intern_term_name(&decl->term_names[i]);

  return declareClass(class, decl);
}


void
XPCE_assignField(Instance instance, Any *field, Any value)
{ assignField(instance, field, value);
}


Any
XPCE_constant(xpce_constant_id id)
{ static Any constants[] =
  { NIL,
    DEFAULT,
    CLASSDEFAULT,
    ON,
    OFF
  };

  return constants[id];
}


int
initPublicInterface()
{ succeed;
}
