/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
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


static status
initialiseTileAdjuster(TileAdjuster p, TileObj t)
{ Image img = getClassVariableValueObject(p, NAME_image);
  Size size;
  CursorObj crs;
  BitmapObj bm;

  if ( isNil(t->super) )
    return errorPce(p, NAME_noSubTile, t);

  if ( t->super->orientation == NAME_horizontal )
  { img = getClassVariableValueObject(p, NAME_himage);
    crs = getClassVariableValueObject(p, NAME_horizontalResizeCursor);
  } else
  { img = getClassVariableValueObject(p, NAME_vimage);
    crs = getClassVariableValueObject(p, NAME_verticalResizeCursor);
  }

  size = getCopySize(img->size);
  initialiseWindow((PceWindow) p, NAME_adjuster, size, DEFAULT);
  assign(p, pen, ZERO);
  assign(p, cursor, crs);
  assign(p, orientation, t->super->orientation);

  send(p, NAME_display, bm=newObject(ClassBitmap, img, EAV), EAV);
/*send(bm, NAME_cursor, crs, EAV);*/

  assign(t, adjuster, p);
  assign(p, client, t);

  succeed;
}


static status
unlinkTileAdjuster(TileAdjuster adj)
{ if ( notNil(adj->client) )
    assign(adj->client, adjuster, NIL);

  return unlinkWindow((PceWindow) adj);
}

		 /*******************************
		 *	      EVENTS		*
		 *******************************/

static Int
getEventOffsetTileAdjuster(TileAdjuster adj, EventObj ev)
{ Int x, y;

  TRY(get_xy_event(ev, adj->frame, OFF, &x, &y));

  if ( adj->orientation == NAME_horizontal )
    answer(sub(x, adj->client->area->x));
  else
    answer(sub(y, adj->client->area->y));
}


static status
forwardTileAdjuster(TileAdjuster adj, EventObj ev)
{ Int offset;

  if ( (offset = getEventOffsetTileAdjuster(adj, ev)) )
  { Name dim = ( adj->orientation == NAME_horizontal
		 ? NAME_width : NAME_height );

    if ( valInt(offset) < 1 )
      offset = ONE;

    send(adj->client, dim, offset, EAV);
  }

  succeed;
}


static status
eventTileAdjuster(TileAdjuster adj, EventObj ev)
{ Int offset;

  if ( postEventWindow((PceWindow) adj, ev) )
    succeed;

  if ( isDownEvent(ev) && (offset = getEventOffsetTileAdjuster(adj, ev)) )
  { send(adj, NAME_focus, adj, DEFAULT, adj->cursor, getButtonEvent(ev), EAV);

    assign(adj, down_offset, offset);
    succeed;
  } else if ( notNil(adj->offset) )
  { if ( isDragEvent(ev) )
    { DisplayObj d = getDisplayEvent(ev);

      if ( d && ws_events_queued_display(d) )
	succeed;

      forwardTileAdjuster(adj, ev);
    } else if ( isUpEvent(ev) )
    { forwardTileAdjuster(adj, ev);

      assign(adj, down_offset, NIL);
    }

    succeed;
  }

  fail;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

/* Instance Variables */

vardecl var_tile_adjuster[] =
{  IV(NAME_client,      "tile",
      IV_GET, NAME_tile, "Tile I adjust"),
   IV(NAME_orientation, "{horizontal,vertical}",
      IV_GET, NAME_tile, "Horizontal or vertical resize"),
   IV(NAME_downOffset,  "int*",
      IV_GET, NAME_event, "Initial offset")
};

/* Send Methods */

static senddecl send_tile_adjuster[] =
{ SM(NAME_initialise, 1, "tile", initialiseTileAdjuster,
     DEFAULT, "Create for tile"),
  SM(NAME_unlink, 0, NULL, unlinkTileAdjuster,
     DEFAULT, "Detach from <-client"),
  SM(NAME_event, 1, "event", eventTileAdjuster,
     NAME_event, "Handle event")
};

/* Get Methods */

#define get_tile_adjuster NULL
/*
static getdecl get_tile_adjuster[] =
{
};
*/

/* Resources */

static classvardecl rc_tile_adjuster[] =
{ RC(NAME_himage, "[image]", "@hadjust_tile_image",
     "Default displayed image"),
  RC(NAME_vimage, "[image]", "@vadjust_tile_image",
     "Default displayed image"),
  RC(NAME_horizontalResizeCursor, "cursor",
     UXWIN("sb_h_double_arrow", "win_sizewe"),
     "Cursor for horizontally resizing tile"),
  RC(NAME_verticalResizeCursor, "cursor",
     UXWIN("sb_v_double_arrow", "win_sizens"),
     "Cursor for vertically resizing tile")
};

/* Class Declaration */

static Name tile_adjuster_termnames[] = { NAME_tile };

ClassDecl(tile_adjuster_decls,
	  var_tile_adjuster,
	  send_tile_adjuster,
	  get_tile_adjuster,
	  rc_tile_adjuster,
          1, tile_adjuster_termnames,
          "$Rev$");

status
makeClassTileAdjuster(Class class)
{ return declareClass(class, &tile_adjuster_decls);
}

