/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1998-2011, University of Amsterdam
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


status
initialiseLayoutInterface(Any obj, Graphical image)
{ LayoutInterface itf = obj;

  assign(itf,   image,            image);

  return qadSendv(image, NAME_layoutInterface, 1, &obj);
}


status
unlinkLayoutInterface(Any obj)
{ LayoutInterface itf = obj;

  if ( notNil(itf->image) && !isFreedObj(itf->image) )
  { Any nil = NIL;

    return qadSendv(itf->image, NAME_layoutInterface, 1, &nil);
  }

  succeed;
}


static LayoutInterface
getConvertLayoutInterface(Any context, Graphical image)
{ fail;
}


status
changedAreaLayoutInterface(LayoutInterface itf)
{ if ( notNil(itf->layout_manager) &&
       itf->layout_manager->request_compute != NAME_computing )
    return requestComputeLayoutManager(itf->layout_manager, DEFAULT);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

/* Instance Variables */

static vardecl var_layoutitf[] =
{ IV(NAME_layoutManager, "layout_manager*", IV_GET,
     NAME_organisation, "My manager"),
  IV(NAME_image, "graphical", IV_GET,
     NAME_appearance, "Image managed")
};

/* Send Methods */

static senddecl send_layoutitf[] =
{ SM(NAME_initialise, 1, "graphical", initialiseLayoutInterface,
     DEFAULT, "Initialise abstract instance"),
  SM(NAME_unlink, 0, NULL, unlinkLayoutInterface,
     DEFAULT, "Unlink from <-image")
};

/* Get Methods */

static getdecl get_layoutitf[] =
{ GM(NAME_convert, 1, "layout_interface", "graphical",
     getConvertLayoutInterface,
     DEFAULT, "Convert graphical object")
};

/* Resources */

#define rc_layoutitf NULL
/*
static classvardecl rc_layoutitf[] =
{
};
*/

/* Class Declaration */

static Name layoutitf_termnames[] = { NAME_image };

ClassDecl(layoutitf_decls,
          var_layoutitf, send_layoutitf, get_layoutitf, rc_layoutitf,
          1, layoutitf_termnames,
          "$Rev$");

status
makeClassLayoutInterface(Class class)
{ return declareClass(class, &layoutitf_decls);
}

