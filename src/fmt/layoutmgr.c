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
initialiseLayoutManager(Any obj)
{ /*LayoutManager mgr = obj;*/

/*assign(mgr, device,		NIL);
  assign(mgr, request_compute,  NIL);
*/

  succeed;
}


static status
unlinkLayoutManager(LayoutManager mgr)
{ if ( notNil(mgr->device) )
    assign(mgr->device, layout_manager, NIL);

  succeed;
}


static status
attachLayoutManager(LayoutManager mgr, Device dev)
{ assign(mgr, device, dev);

  succeed;
}


static status
detachLayoutManager(LayoutManager mgr)
{ if ( notNil(mgr->device) )
    assign(mgr, device, NIL);

  succeed;
}


status
requestComputeLayoutManager(LayoutManager mgr, Any val)
{ if ( isNil(mgr) ||
       isFreeingObj(mgr) ||		/* not needed */
       (notNil(mgr->request_compute) && isDefault(val)) ||
       mgr->request_compute == val )
    succeed;

  if ( isDefault(val) )
    val = ON;

  assign(mgr, request_compute, val);

  if ( notNil(mgr->device) )
    requestComputeGraphical(mgr->device, DEFAULT);

  succeed;
}


static status
computeLayoutManager(LayoutManager mgr)
{ assign(mgr, request_compute, NIL);

  succeed;
}


status
ComputeLayoutManager(Any obj)
{ LayoutManager mgr = obj;

  if ( notNil(mgr->request_compute) )
    qadSendv(mgr, NAME_compute, 0, NULL);

  succeed;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

/* Instance Variables */

static vardecl var_layoutmgr[] =
{ IV(NAME_device, "device*", IV_GET,
     NAME_organisation, "Device on which I manage graphicals"),
  SV(NAME_requestCompute, "any*", IV_GET|IV_STORE, requestComputeLayoutManager,
     NAME_compute, "Indicates layout needs to be recomputed")
};

/* Send Methods */

static senddecl send_layoutmgr[] =
{ SM(NAME_initialise, 0, NULL, initialiseLayoutManager,
     DEFAULT, "Initialise abstract instance"),
  SM(NAME_unlink, 0, NULL, unlinkLayoutManager,
     DEFAULT, "Detach from <-device"),
  SM(NAME_attach, 1, "graphical", attachLayoutManager,
     NAME_layout, "Attach manager to device"),
  SM(NAME_detach, 0, NULL, detachLayoutManager,
     NAME_layout, "Detach manager from current device"),
  SM(NAME_compute, 0, NULL, computeLayoutManager,
     NAME_compute, "Recompute the layout (virtual)"),
  SM(NAME_computeBoundingBox, 0, NULL, failObject,
     NAME_compute, "Compute the bounding box (virtual)"),
  SM(NAME_RedrawArea, 1, "area", succeedObject,
     NAME_redraw, "Paint (background) objects")
};

/* Get Methods */

#define get_layoutmgr NULL
/*
static getdecl get_layoutmgr[] =
{
};
*/

/* Resources */

#define rc_layoutmgr NULL
/*
static classvardecl rc_layoutmgr[] =
{
};
*/

/* Class Declaration */

ClassDecl(layoutmgr_decls,
          var_layoutmgr, send_layoutmgr, get_layoutmgr, rc_layoutmgr,
          0, NULL,
          "$Rev$");

status
makeClassLayoutManager(Class class)
{ return declareClass(class, &layoutmgr_decls);
}

