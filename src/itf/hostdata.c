/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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
#define QUICK_AND_DIRTY 1

		 /*******************************
		 *	 PUBLIC INTERFACE	*
		 *******************************/

HostData
CtoHostData(Class class, void *h, int flags)
{ HostData hd;
#ifdef QUICK_AND_DIRTY
  hd = allocObject(class, TRUE);

  hd->handle = h;
  setFlag(hd, F_ISHOSTDATA|F_NOTANY);
  incrInt(class->no_created);
  clearCreatingObj(hd);
#else
  hd = newObjectv(class, 1, &h);
#endif

  if ( flags & PCE_ANSWER )
    pushAnswerObject(hd);

  return hd;
}


void
setHostDataHandle(HostData hd, void *h)
{ hd->handle = h;
}


void *
getHostDataHandle(HostData hd)
{ if ( isHostData(hd) )
    return hd->handle;

  return NULL;
}


int
freeHostData(HostData hd)
{
  if ( refsObject(hd) == 0 )
  {
#ifdef QUICK_AND_DIRTY
    Class class = classOfObject(hd);

    if ( !onFlag(hd, F_FREED) )
    { incrInt(class->no_freed);
      unalloc(valInt(class->instance_size), hd);
    }
#else
    freeObject(hd);
#endif
    succeed;
  }

  fail;
}


void
makeAnyHostData(HostData hd)
{ clearFlag(hd, F_NOTANY);
}




		 /*******************************
		 *	      CLASS		*
		 *******************************/


static status
initialiseHostData(HostData hd, void *h)
{
#ifdef QUICK_AND_DIRTY
  return errorPce(classOfObject(hd), NAME_cannotCreateInstances);
#else
  hd->handle = h;
  setFlag(hd, F_ISHOSTDATA|F_NOTANY);

  succeed;
#endif
}


static StringObj
getPrintNameHostData(HostData hd)
{ char tmp[25];

  sprintf(tmp, "@" INTPTR_FORMAT "/%s",
	  valInt(PointerToInt(hd)),
	  strName(classOfObject(hd)->name));

  return CtoString(tmp);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

/* Instance Variables */

static vardecl var_host_data[] =
{ IV(NAME_handle, "alien:void *", IV_NONE,
     NAME_storage, "Foreign-data handle")
};

/* Send Methods */

static senddecl send_host_data[] =
{ SM(NAME_initialise, 1, "alien:void *", initialiseHostData,
     DEFAULT, "Create from handle")
};

/* Get Methods */

static getdecl get_host_data[] =
{ GM(NAME_printName, 0, "text=string", NULL, getPrintNameHostData,
     DEFAULT, "Returns string holding @<ref>/<class>")
};

/* Resources */

#define rc_host_data NULL
/*
static classvardecl rc_host_data[] =
{
};
*/

/* Class Declaration */

static Name host_data_termnames[] = { NAME_handle };

ClassDecl(host_data_decls,
          var_host_data, send_host_data, get_host_data, rc_host_data,
          1, host_data_termnames,
          "$Rev$");


status
makeClassHostData(Class class)
{ return declareClass(class, &host_data_decls);
}

