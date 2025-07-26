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
#include <h/graphics.h>
					/* generated from Makefile */

static status	backgroundDisplay(DisplayObj, Colour);
static status	foregroundDisplay(DisplayObj d, Colour c);
static void	attach_font_families(Class class);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a display.  The display is not yet opened.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
initialiseDisplay(DisplayObj d, Name name, Area a)
{ DisplayManager dm = TheDisplayManager();

  assign(d, name,		name);
  assign(d, primary,		DEFAULT);
  assign(d, area,		a);
  assign(d, removed,		OFF);
  assign(d, frames,		newObject(ClassChain, EAV));
  assign(d, inspect_handlers,	newObject(ClassChain, EAV));
  assign(d, display_manager,	dm);
  assign(d, busy_locks,		ZERO);
  obtainClassVariablesObject(d);

  appendDisplayManager(dm, d);

  succeed;
}


static DisplayObj
getConvertDisplay(Class class, Any obj)
{ DisplayObj d;

  if ( (d = getMemberDisplayManager(TheDisplayManager(), obj)) )
    answer(d);

  if ( isDefault(obj) )
    answer(CurrentDisplay(obj));

  if ( instanceOfObject(obj, ClassVisual) )
    answer(get(obj, NAME_display, EAV));

  fail;
}


static status
unlinkDisplay(DisplayObj d)
{ deleteDisplayManager(d->display_manager, d);
  ws_close_display(d);

  succeed;
}


static status
removedDisplay(DisplayObj d)
{ return freeObject(d);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Open a display.  If necessary, the X toolkit is initialised first and
a context for the application is created.

As PCE  normally manages a  collection of main  windows an application
shell  widget is created to  serve as root for  all  the other (popup)
shells.  This widget is never realised (page 35 of Xt manual).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
openDisplay(DisplayObj d)
{ succeed;
}


static status
foregroundDisplay(DisplayObj d, Colour c)
{ assign(d, foreground, c);
  ws_foreground_display(d, c);

  succeed;
}


static status
backgroundDisplay(DisplayObj d, Colour c)
{ assign(d, background, c);
  ws_background_display(d, c);

  succeed;
}


static status
eventQueuedDisplay(DisplayObj d)
{ RedrawDisplayManager(d->display_manager);

  return ws_events_queued_display(d);
}


status
dispatchDisplay(DisplayObj d)
{ answer(dispatchDisplayManager(d->display_manager, DEFAULT, DEFAULT));
}


static status
screenSaverDisplay(DisplayObj d, BoolObj val)
{ openDisplay(d);

  if ( val == ON )
    ws_activate_screen_saver(d);
  else
    ws_deactivate_screen_saver(d);

  succeed;
}


status
bellDisplay(DisplayObj d, Int vol)
{ openDisplay(d);

  if ( isDefault(vol) )
    vol = (Int) getClassVariableValueObject(d, NAME_volume);

  ws_bell_display(d, valInt(vol));

  succeed;
}


/* See whether we can open graphics.  We try to avoid this if not
 * really necessary for compiling xpce sources to .qlf
 */
static int
hasDisplay(void)
{
#if defined(__WINDOWS__) || defined(__APPLE__)
  return TRUE;
#else
  char *dsp = getenv("DISPLAY");
  if ( dsp && dsp[0] )
    return TRUE;
  dsp = getenv("WAYLAND_DISPLAY");
  if ( dsp && dsp[0] )
    return TRUE;
#endif
  return FALSE;
}

Size
getSizeDisplay(DisplayObj d)
{ answer(getSizeArea(d->area));
}


Int
getWidthDisplay(DisplayObj d)
{ answer(d->area->w);
}


Int
getHeightDisplay(DisplayObj d)
{ answer(d->area->h);
}


static Int
getDepthDisplay(DisplayObj d)
{ answer(toInt(ws_depth_display(d)));
}

static Name
getSystemThemeDisplay(DisplayObj d)
{ TRY(openDisplay(d));

  answer(ws_get_system_theme_display(d));
}

static Name
getThemeDisplay(DisplayObj d)
{ Name theme;

  if ( (theme=getClassVariableValueObject(d, NAME_theme)) &&
       notDefault(theme) )
    return theme;

  return getSystemThemeDisplay(d);
}


Size
getDPIDisplay(DisplayObj d)
{ int rx, ry;

  if ( instanceOfObject(d->dpi, ClassSize) )
    answer(d->dpi);
  if ( isInteger(d->dpi) )
  { assign(d, dpi, newObject(ClassSize, d->dpi, d->dpi, EAV));
    answer(d->dpi);
  }
  Any rc = getClassVariableValueObject(d, NAME_dpi);
  if ( rc && !isDefault(rc) )
  { if ( instanceOfObject(rc, ClassSize) )
      assign(d, dpi, rc);
    else
      assign(d, dpi, newObject(ClassSize, rc, rc, EAV));
    answer(d->dpi);
  }

  if ( hasDisplay() )
    TRY(openDisplay(d));
  if ( instanceOfObject(d->dpi, ClassSize) )
    answer(d->dpi);
  if ( ws_resolution_display(d, &rx, &ry) )
  { assign(d, dpi, newObject(ClassSize, toInt(rx), toInt(ry), EAV));
    answer(d->dpi);
  }

  assign(d, dpi, newObject(ClassSize, toInt(96), toInt(96), EAV));
  answer(d->dpi);
}

int
DPI(Any gr)
{ DisplayObj d = CurrentDisplay(gr ? gr : NIL);

  if ( d )
  { Size sz = getDPIDisplay(d);

    return (int)((valInt(sz->w) + valInt(sz->h) + 1)/2);
  } else
  { return 96;
  }
}

double
dpi_scale(Any gr, double px)
{ return px;
}

static status
DPIDisplay(DisplayObj d, Any arg)
{ if ( instanceOfObject(arg, ClassSize) )
    assign(d, dpi, arg);
  else
    assign(d, dpi, newObject(ClassSize, arg, arg, EAV));

  succeed;
}


status
hasVisibleFramesDisplay(DisplayObj d)
{ if ( notNil(d->frames) )
  { Cell cell;

    for_cell(cell, d->frames)
    { FrameObj fr = cell->value;
      if ( !onFlag(fr, F_FREED|F_FREEING) )
      { if ( fr->status != NAME_unmapped && fr->status != NAME_hidden )
	  succeed;
      }
    }
  }

  fail;
}


		 /*******************************
		 *	SELECTION INTERFACE	*
		 *******************************/

static Any
getSelectionDisplay(DisplayObj d, Name which, Name target, Type type)
{ Any sel;

  TRY(openDisplay(d));

  if ( isDefault(which) )  which  = NAME_primary;
  if ( isDefault(target) ) target = NAME_text;
  if ( isDefault(type) )   type   = nameToType(NAME_string);

  if ( (sel = ws_get_selection(d, which, target)) )
    answer(checkType(sel, type, NIL));

  fail;
}

		 /*******************************
		 *  SIMPLE SELECTION INTERFACE	*
		 *******************************/

static status
selectionDisplay(DisplayObj d, Name which, StringObj data)
{ return ws_selection_display(d, which, data);
}


static status
copyDisplay(DisplayObj d, StringObj data)
{ int rval = (send(d, NAME_selection, NAME_primary, data, EAV) |
	      send(d, NAME_selection, NAME_clipboard, data, EAV));


  return rval ? SUCCEED : FAIL;
}


static StringObj
getPasteDisplay(DisplayObj d, Name which)
{ if ( isDefault(which) )
    which = NAME_clipboard;

  return getSelectionDisplay(d, which, DEFAULT, DEFAULT);
}


		/********************************
		*        CONFIRM/INFORM		*
		********************************/

static status
create_confirmer(DisplayObj d)
{ Any p, m, h;

  if ( getAttributeObject(d, NAME_confirmer) )
    succeed;

  TRY( p = newObject(ClassWindow, DEFAULT, DEFAULT, d, EAV) );
  TRY( m = newObject(ClassText, CtoName(""), NAME_center, EAV) );
  TRY( h = newObject(ClassText, CtoName(""), NAME_center, EAV) );

  send(m, NAME_font, getClassVariableValueObject(d, NAME_labelFont), EAV);
  send(h, NAME_font, getClassVariableValueObject(d, NAME_valueFont), EAV);
  send(p, NAME_display, m, EAV);
  send(p, NAME_display, h, EAV);
  send(p, NAME_kind, NAME_popup, EAV);
  send(p, NAME_cursor, newObject(ClassCursor, NAME_mouse, EAV), EAV);
  send(p, NAME_border, toInt(3), EAV);
  send(p, NAME_pen, toInt(3), EAV);
  send(p, NAME_create, EAV);
  send(get(p, NAME_frame, EAV), NAME_border, ONE, EAV);

  send(p, NAME_recogniser,
          newObject(ClassHandler, NAME_button,
		    newObject(ClassMessage,
			      d, NAME_ConfirmPressed, Arg(1), EAV),
		    EAV),
       EAV);

  attributeObject(d, NAME_SeenDown, OFF);
  attributeObject(d, NAME_confirmer, p);
  attributeObject(p, NAME_helpText, h);
  attributeObject(p, NAME_messageText, m);

  succeed;
}


static status
ConfirmPressedDisplay(DisplayObj d, EventObj ev)
{ if ( isDownEvent(ev) )
    send(d, NAME_SeenDown, ON, EAV);
  else if ( isUpEvent(ev) )
  { if ( get(d, NAME_SeenDown, EAV) == ON )
    { Name code = getButtonEvent(ev);

      send(get(d, NAME_confirmer, EAV), NAME_return, code, EAV);
    } else
    { send(get(d, NAME_confirmer, EAV), NAME_grabPointer, OFF, EAV); /* HACK */
      send(get(d, NAME_confirmer, EAV), NAME_grabPointer, ON, EAV);
    }
  }

  succeed;
}


static Name
display_help(DisplayObj d, StringObj hlp, Name msg)
{ Any p;
  TextObj hlp_text, msg_text;
  int fx, fy, fw, fh, tx, ty;
  Name rval;

  create_confirmer(d);
  TRY( p        = getAttributeObject(d, NAME_confirmer) );
  TRY( hlp_text = getAttributeObject(p, NAME_helpText));
  TRY( msg_text = getAttributeObject(p, NAME_messageText));

  send(hlp_text, NAME_string, hlp, EAV);
  send(msg_text, NAME_string, msg, EAV);
  send(p, NAME_compute, EAV);

  fw = max(valInt(hlp_text->area->w), valInt(msg_text->area->w)) + 40;
  fh = valInt(hlp_text->area->h) + valInt(msg_text->area->h) + 50;
  getSizeDisplay(d);			/* initialise size argument */
  fx = (valInt(d->area->w) - fw) / 2;
  fy = (valInt(d->area->h) - fh) / 2;

  tx = (fw - 12 - valInt(hlp_text->area->w)) / 2;
  send(hlp_text, NAME_set, toInt(tx), toInt(20), DEFAULT, DEFAULT, EAV);
  tx = (fw - 12 - valInt(msg_text->area->w)) / 2;
  ty = valInt(hlp_text->area->h) + 30;
  send(msg_text, NAME_set, toInt(tx), toInt(ty), DEFAULT, DEFAULT, EAV);

  send(get(p, NAME_frame, EAV), NAME_set, toInt(fx), toInt(fy),
					toInt(fw), toInt(fh), EAV);

  send(d, NAME_SeenDown, OFF, EAV);
  send(p, NAME_show, ON, EAV);
  send(p, NAME_grabPointer, ON, EAV);
  rval = get(p, NAME_confirm, DEFAULT, ON, EAV);
  send(p, NAME_grabPointer, OFF, EAV);
  send(p, NAME_show, OFF, EAV);

  return rval;
}


status
confirmDisplay(DisplayObj d, Any client, CharArray title,
	       CharArray fmt, int argc, Any *argv)
{ StringObj message;
  ArgVector(av, argc+1);
  int i;
  Name button;

  av[0] = (Any) fmt;
  for(i=0; i<argc; i++)
    av[i+1] = argv[i];

  TRY(message = answerObjectv(ClassString, argc+1, av));

  switch( ws_message_box(client, title, (CharArray)message, MBX_CONFIRM) )
  { case MBX_OK:
      succeed;
    case MBX_CANCEL:
      fail;
    default:
    { Name msg;

      msg = CtoName("Press LEFT button to confirm, RIGHT button to cancel");
      TRY(button = display_help(d, message, msg));
      doneObject(message);

      if ( button == NAME_left )
	succeed;
    }
  }

  fail;
}


status
informDisplay(DisplayObj d, Any client, CharArray title,
	      CharArray fmt, int argc, Any *argv)
{ StringObj message;
  ArgVector(av, argc+1);
  int i;
  Name button;
  status rc = SUCCEED;

  av[0] = (Any) fmt;
  for(i=0; i<argc; i++)
    av[i+1] = argv[i];

  TRY(message = answerObjectv(ClassString, argc+1, av));

  switch( ws_message_box(client, title, (CharArray)message, MBX_INFORM) )
  { case MBX_NOTHANDLED:
    { Name msg;

      msg = CtoName("Press any button to remove message");
      if ( !(button = display_help(d, message, msg)) )
	rc = FAIL;
    }
  }
  doneObject(message);

  return rc;
}


static status
reportDisplay(DisplayObj d, Name kind, CharArray fmt, int argc, Any *argv)
{ status rc = SUCCEED;

  if ( kind == NAME_error || kind == NAME_inform )
  { ArgVector(av, argc+1);
    StringObj str;

    av[0] = isDefault(fmt) ? (CharArray) CtoName("") : fmt;
    copyArgs(argc, argv, &av[1]);
    TRY(str = answerObjectv(ClassString, argc+1, av));
    if ( kind == NAME_error )
      alertReporteeVisual(d);

    switch( ws_message_box(DEFAULT, DEFAULT, (CharArray)str, MBX_ERROR) )
    { case MBX_NOTHANDLED:
      { Name msg, button;

	msg = CtoName("Press any button to remove message");
	if ( !(button = display_help(d, str, msg)) )
	  rc = FAIL;
      }
    }
    doneObject(str);
  } else if ( kind == NAME_warning )
    alertReporteeVisual(d);

  return rc;
}


		 /*******************************
		 *		BUSY		*
		 *******************************/

status
busyCursorDisplay(DisplayObj d, CursorObj c, BoolObj block_events)
{ if ( !instanceOfObject(d, ClassDisplay) )
    succeed;

  if ( notNil(c) )
  { assign(d, busy_locks, add(d->busy_locks, ONE));

    if ( d->busy_locks == ONE )
    { Cell cell;

      for_cell(cell, d->frames)
	busyCursorFrame(cell->value, c, block_events);
    }
  } else
  { assign(d, busy_locks, sub(d->busy_locks, ONE));

    if ( valInt(d->busy_locks) < 0 )
      assign(d, busy_locks, ZERO);

    if ( d->busy_locks == ZERO )
    { Cell cell;

      for_cell(cell, d->frames)
	busyCursorFrame(cell->value, c, block_events);
    }
  }

  succeed;
}


		/********************************
		*          DEBUGGING		*
		********************************/

static status
inspectHandlerDisplay(DisplayObj d, Handler h)
{ return addChain(d->inspect_handlers, h);
}


status
inspectDisplay(DisplayObj d, Graphical gr, EventObj ev)
{ Handler h;

  for_chain(d->inspect_handlers, h,
	    { if ( isAEvent(ev, h->event) &&
		   forwardReceiverCode(h->message, gr, gr, ev, EAV) )
	      { DEBUG(NAME_inspect, Cprintf("Inspect %s succeeded on %s\n",
					    pp(ev->id), pp(h)));
		succeed;
	      }
	    })

  fail;
}

		/********************************
		*          FONT TABLES		*
		********************************/

static status
loadFontFamilyDisplay(DisplayObj d, Name fam)
{ Class class = classOfObject(d);

  if ( !getClassVariableClass(class, fam) )
    attach_class_variable(class, fam, "chain", "[]", "Font family set");

  if ( !getClassVariableValueObject(d, fam) )
    return errorPce(d, NAME_noFontsInFamily, fam);

  succeed;
}


static status
loadFontsDisplay(DisplayObj d)
{ Chain fams;
  static int done = FALSE;

  if ( done )
    succeed;
  done = TRUE;

  if ( (fams = getClassVariableValueObject(d, NAME_fontFamilies)) )
  { Cell cell;

    for_cell(cell, fams)
      send(d, NAME_loadFontFamily, cell->value, EAV);
  }

  succeed;
}


static status
loadFontAliasesDisplay(DisplayObj d, Name res)
{ Chain ch = getClassVariableValueObject(d, res);

  if ( ch )
  { Cell cell;
    Type type_font = nameToType(NAME_font);

    for_cell(cell, ch)
    { Name name;
      FontObj font;
      Any n, f;

      if ( instanceOfObject(cell->value, ClassBinding) )
      { Binding b = cell->value;
	n = b->name;
	f = b->value;
      } else if ( instanceOfObject(cell->value, ClassTuple) )
      { Tuple t = cell->value;
	n = t->first;
	f = t->second;
      } else if ( instanceOfObject(cell->value, ClassAttribute) )
      { Attribute a = cell->value;
	n = a->name;
	f = a->value;
      } else
      { errorPce(cell->value, NAME_unexpectedType,
		 CtoType(":=|tuple|attribute"));
	continue;
      }

      if ( !(name = checkType(n, TypeName, d)) ||
	   !(font = checkType(f, type_font, d)) )
	errorPce(d, NAME_badFontAlias, n, f);
      else
	send(d, NAME_fontAlias, name, font, EAV);
    }

    succeed;
  }

  fail;
}


static status
fontAliasDisplay(DisplayObj d, Name name, FontObj font, BoolObj force)
{ if ( force == ON || !getMemberHashTable(FontAliasTable, name) )
    appendHashTable(FontAliasTable, name, font);

  succeed;
}


static FontObj
getFontAliasDisplay(DisplayObj d, Name name)
{ FontObj f;

  if ( (f = getMemberHashTable(FontAliasTable, name)) )
    answer(f);

  makeBuiltinFonts();

  answer(getMemberHashTable(FontAliasTable, name));
}


static status
listFontsDisplay(DisplayObj d, BoolObj mono)
{ return ws_list_fonts(d, mono);
}


		/********************************
		*             VISUAL		*
		********************************/

static Chain
getContainsDisplay(DisplayObj d)
{ answer(d->frames);
}


static Any
getContainedInDisplay(DisplayObj d)
{ answer(d->display_manager);
}

		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_initialise[] =
	{ "name=name", "area=area" };
static char *T_busyCursor[] =
        { "cursor=[cursor]*", "block_input=[bool]" };
static char *T_fontAlias[] =
        { "name=name", "font=font", "force=[bool]" };
static char *T_inform[] =
        { "for=[visual]", "title=[char_array]", "message=char_array", "any ..." };
static char *T_getSelection[] =
        { "which=[name]", "target=[name]", "type=[type]" };
static char *T_selection[] =
        { "which=[name]", "value=char_array" };
#ifdef WIN32_GRAPHICS
extern Name getWinFileNameDisplay(DisplayObj obj,
				  Name mode,
				  Chain filters,
				  CharArray title,
				  CharArray file,
				  Directory dir,
				  Any owner,
				  Chain flags);
static char *T_win_file_name[] =
	{ "mode={open,save}",
	  "filters=[chain]",
	  "title=[char_array]",
	  "default=[char_array]",
	  "directory=[directory]",
	  "owner=[frame|int]",
	  "options=[chain]"
	};
extern Name getWinDirectoryDisplay(DisplayObj d,
				   CharArray title,
				   Directory dir,
				   Any owner);
static char *T_win_directory[] =
	{ "title=[char_array]",
	  "directory=[directory]",
	  "owner=[frame|int]"
	};
#endif

/* Instance Variables */

static vardecl var_display[] =
{ IV(NAME_name, "name", IV_BOTH,
     NAME_name, "Human name of the display"),
  IV(NAME_primary, "bool", IV_BOTH,
     NAME_organisation, "@on if this is the primary display"),
  IV(NAME_area, "area", IV_GET,
     NAME_dimension, "Area occupied by this display"),
  IV(NAME_workArea, "area", IV_GET,
     NAME_dimension, "Area available for applications"),
  IV(NAME_removed, "bool", IV_NONE,
     NAME_organisation, "Display is removed, but not yet empty"),
  IV(NAME_dpi, "[size|int]", IV_NONE,
     NAME_dimension, "Resolution (dots per inch)"),
  IV(NAME_frames, "chain", IV_GET,
     NAME_organisation, "Frames displayed on this display"),
  IV(NAME_inspectHandlers, "chain", IV_GET,
     NAME_event, "Chain of handlers to support inspector tools"),
  SV(NAME_foreground, "colour", IV_GET|IV_STORE, foregroundDisplay,
     NAME_appearance, "Windows default foreground colour"),
  SV(NAME_background, "colour", IV_GET|IV_STORE, backgroundDisplay,
     NAME_appearance, "Windows default background colour"),
  IV(NAME_displayManager, "display_manager", IV_GET,
     NAME_organisation, "The global display manager (@display_manager)"),
  IV(NAME_busyLocks, "0..", IV_NONE,
     NAME_event, "Lock count for ->busy_cursor"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_windowSystem, "Window-System reference")
};

/* Send Methods */

static senddecl send_display[] =
{ SM(NAME_initialise, 2, T_initialise, initialiseDisplay,
     DEFAULT, "Create from name and area"),
  SM(NAME_unlink, 0, NULL, unlinkDisplay,
     DEFAULT, "Remove from display_manager"),
  SM(NAME_removed, 0, NULL, removedDisplay,
     NAME_oms, "Hotplug display was removed"),
  SM(NAME_busyCursor, 2, T_busyCursor, busyCursorDisplay,
     NAME_event, "Define (temporary) cursor for all frames on the display"),
  SM(NAME_dispatch, 0, NULL, dispatchDisplay,
     NAME_event, "Dispatch events for 1/4th second"),
  SM(NAME_eventQueued, 0, NULL, eventQueuedDisplay,
     NAME_event, "Test if there are X-events waiting"),
  SM(NAME_inspectHandler, 1, "handler", inspectHandlerDisplay,
     NAME_event, "Register handler for inspect tool"),
  SM(NAME_fontAlias, 3, T_fontAlias, fontAliasDisplay,
     NAME_font, "Define a logical name for a font"),
  SM(NAME_loadFontAliases, 1, "set=name", loadFontAliasesDisplay,
     NAME_font, "Load font aliases from named class-variable"),
  SM(NAME_loadFontFamily, 1, "family=name", loadFontFamilyDisplay,
     NAME_font, "Create predefined fonts from family"),
  SM(NAME_loadFonts, 0, NULL, loadFontsDisplay,
     NAME_font, "Create predefined font set from defaults"),
  SM(NAME_listFonts, 1, "[bool]", listFontsDisplay,
     NAME_font, "List available fonts to console"),
  SM(NAME_ConfirmPressed, 1, "event", ConfirmPressedDisplay,
     NAME_internal, "Handle confirmer events"),
  SM(NAME_open, 0, NULL, openDisplay,
     NAME_open, "Prepare display for graphics operations"),
  SM(NAME_bell, 1, "volume=[int]", bellDisplay,
     NAME_report, "Ring the bell at volume"),
  SM(NAME_confirm, 4, T_inform, confirmDisplay,
     NAME_report, "Test if the user confirms string"),
  SM(NAME_inform, 4, T_inform, informDisplay,
     NAME_report, "Inform the user of something"),
  SM(NAME_report, 3, T_report, reportDisplay,
     NAME_report, "Report message using ->inform"),
  SM(NAME_selection, 2, T_selection, selectionDisplay,
     NAME_selection, "Set the (textual) selection"),
  SM(NAME_copy, 1, "char_array", copyDisplay,
     NAME_selection, "Copy to selection and cut_buffer"),
  SM(NAME_screenSaver, 1, "bool", screenSaverDisplay,
     NAME_x, "Activate (@on) or deactivate (@off) screensaver"),
  SM(NAME_dpi, 1, "size|int", DPIDisplay,
     NAME_dimension, "Resolution in dots per inch"),
  SM(NAME_hasVisibleFrames, 0, NULL, hasVisibleFramesDisplay,
     NAME_organisation, "True if there is at least one visible frame")
};

/* Get Methods */

static getdecl get_display[] =
{ GM(NAME_containedIn, 0, "display_manager", NULL, getContainedInDisplay,
     DEFAULT, "Display manager"),
  GM(NAME_contains, 0, "chain", NULL, getContainsDisplay,
     DEFAULT, "Chain with frames contained"),
  GM(NAME_convert, 1, "display", "any", getConvertDisplay,
     DEFAULT, "Convert graphical or `host:display[.screen]'"),
  GM(NAME_depth, 0, "bits_per_pixel=int", NULL, getDepthDisplay,
     NAME_colour, "Number of bits/pixel"),
  GM(NAME_systemTheme, 0, "{light,dark}",
     NULL, getSystemThemeDisplay,
     NAME_colour, "The OS system theme"),
  GM(NAME_theme, 0, "name", NULL, getThemeDisplay,
     NAME_colour, "Use specified or OS theme"),
  GM(NAME_height, 0, "int", NULL, getHeightDisplay,
     NAME_dimension, "Height of the display in pixels"),
  GM(NAME_size, 0, "size", NULL, getSizeDisplay,
     NAME_dimension, "Size of the display"),
  GM(NAME_width, 0, "int", NULL, getWidthDisplay,
     NAME_dimension, "Width of the display in pixels"),
  GM(NAME_dpi, 0, "size", NULL, getDPIDisplay,
     NAME_dimension, "Resolution in dots per inch"),
  GM(NAME_fontAlias, 1, "font", "name=name", getFontAliasDisplay,
     NAME_font, "Lookup logical name"),
  GM(NAME_selection, 3, "any", T_getSelection, getSelectionDisplay,
     NAME_selection, "Query value of the X-window selection"),
  GM(NAME_paste, 1, "string", "which=[{primary,clipboard}]", getPasteDisplay,
     NAME_selection, "Simple interface to get clipboard value"),
#ifdef WIN32_GRAPHICS
  GM(NAME_winFileName, 7, "name", T_win_file_name, getWinFileNameDisplay,
     NAME_prompt, "Ask for a filename using Windows standard dialog"),
  GM(NAME_winDirectory, 3, "name", T_win_directory, getWinDirectoryDisplay,
     NAME_prompt, "Ask for a directory (folder) using Windows standard dialog"),
#endif
};

/* Resources */

static classvardecl rc_display[] =
{ RC(NAME_dpi, "[size|int]", "@default",
     "Screen resolution in Dots Per Inch"),
  RC(NAME_theme, "[name]", "@default",
     "SWI-Prolog theme library to load"),
  RC(NAME_background, "colour", "white",
     "Default background for windows"),
  RC(NAME_foreground, "colour", "black",
     "Default foreground for windows"),
  RC(NAME_labelFont, "font", "bold",
     "Label font for confirm/inform"),
  RC(NAME_systemFonts, "chain",
     "[ normal    := font(helvetica, roman, 12),\n"
     "  bold      := font(helvetica, bold, 12),\n"
     "  italic    := font(helvetica, oblique, 12),\n"
     "  small     := font(helvetica, roman, 10),\n"
     "  large     := font(helvetica, roman, 14),\n"
     "  boldlarge := font(helvetica, bold, 14),\n"
     "  huge      := font(helvetica, roman, 18),\n"
     "  boldhuge  := font(helvetica, bold, 18),\n"
     "  fixed     := font(courier, roman, 12),\n"
     "  tt        := font(courier, roman, 12),\n"
     "  boldtt    := font(courier, bold, 12)\n"
     "]",
     "Predefined font-aliases"),
  RC(NAME_noFont, "font", "normal",
     "Replacement for undefined fonts"),
  RC(NAME_valueFont, "font", "normal",
     "Text font for confirm/inform"),
  RC(NAME_volume, "int", "0",
     "Default volume of ->bell"),
  RC(NAME_windowManager, "[name]", "@default",
     "Window manager running on this display")
};

/* Class Declaration */

static Name display_termnames[] = { NAME_name };

ClassDecl(display_decls,
          var_display, send_display, get_display, rc_display,
          1, display_termnames,
          "$Rev$");

status
makeClassDisplay(Class class)
{ declareClass(class, &display_decls);
  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  /* @colour_display is always true now */
  globalObject(NAME_colourDisplay, ClassAnd, EAV);

  attach_font_families(class);

  succeed;
}


#define PFONT(n, p, x) { n, p, XNAME(x) }
#define ENDFONTLIST    { NULL, 0, NULL }

typedef struct
{ Name style;
  int  points;
  char *xname;
} fontdef, *FontDef;

#if defined(WIN32_GRAPHICS) || defined(USE_XFT)
#define XNAME(x) NULL
#else
#define XNAME(x) x
#endif

#ifndef FIXED_FAMILY
#define FIXED_FAMILY "*"
#endif

static fontdef screen_fonts[] =
{ PFONT(NAME_roman, 10,
	"-" FIXED_FAMILY "-fixed-medium-r-normal--10-*-*-*-*-*-iso10646-*"),
  PFONT(NAME_roman, 12,
	"-" FIXED_FAMILY "-fixed-medium-r-normal--12-*-*-*-*-*-iso10646-*"),
  PFONT(NAME_roman, 14,
	"-" FIXED_FAMILY "-fixed-medium-r-normal--14-*-*-*-*-*-iso10646-*"),
  PFONT(NAME_roman, 16,
	"-" FIXED_FAMILY "-fixed-medium-r-normal--16-*-*-*-*-*-iso10646-*"),
  PFONT(NAME_bold, 10,
	"-" FIXED_FAMILY "-fixed-bold-r-normal--10-*-*-*-*-*-iso10646-*"),
  PFONT(NAME_bold, 12,
	"-" FIXED_FAMILY "-fixed-bold-r-normal--12-*-*-*-*-*-iso10646-*"),
  PFONT(NAME_bold, 14,
	"-" FIXED_FAMILY "-fixed-bold-r-normal--14-*-*-*-*-*-iso10646-*"),
  PFONT(NAME_bold, 16,
	"-" FIXED_FAMILY "-fixed-bold-r-normal--16-*-*-*-*-*-iso10646-*"),
  ENDFONTLIST
};

#undef XNAME
#if defined(WIN32_GRAPHICS) || defined(USE_XFT)
#define XNAME(x) NULL
#else
#define XNAME(x) x
#endif

static fontdef courier_fonts[] =
{ PFONT(NAME_roman, 10,
	"-*-courier new-medium-r-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 12,
	"-*-courier new-medium-r-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 14,
	"-*-courier new-medium-r-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 18,
	"-*-courier new-medium-r-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 24,
	"-*-courier new-medium-r-normal--24-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 10,
	"-*-courier new-bold-r-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 12,
	"-*-courier new-bold-r-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 14,
	"-*-courier new-bold-r-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 18,
	"-*-courier new-bold-r-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 24,
	"-*-courier new-bold-r-normal--24-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 10,
	"-*-courier new-medium-o-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 12,
	"-*-courier new-medium-o-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 14,
	"-*-courier new-medium-o-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 18,
	"-*-courier new-medium-o-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 24,
	"-*-courier new-medium-o-normal--24-*-*-*-*-*-*-*"),
  ENDFONTLIST
};


static fontdef helvetica_fonts[] =
{ PFONT(NAME_bold, 10,
	"-*-helvetica-bold-r-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 12,
	"-*-helvetica-bold-r-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 14,
	"-*-helvetica-bold-r-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 18,
	"-*-helvetica-bold-r-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 24,
	"-*-helvetica-bold-r-normal--24-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 10,
	"-*-helvetica-medium-r-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 12,
	"-*-helvetica-medium-r-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 14,
	"-*-helvetica-medium-r-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 18,
	"-*-helvetica-medium-r-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 24,
	"-*-helvetica-medium-r-normal--24-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 10,
	"-*-helvetica-medium-o-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 12,
	"-*-helvetica-medium-o-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 14,
	"-*-helvetica-medium-o-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 18,
	"-*-helvetica-medium-o-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_oblique, 24,
	"-*-helvetica-medium-o-normal--24-*-*-*-*-*-*-*"),
  ENDFONTLIST
};


static fontdef times_fonts[] =
{ PFONT(NAME_roman, 10,
	"-*-times-medium-r-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 12,
	"-*-times-medium-r-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 14,
	"-*-times-medium-r-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 18,
	"-*-times-medium-r-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_roman, 24,
	"-*-times-medium-r-normal--24-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 10,
	"-*-times-bold-r-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 12,
	"-*-times-bold-r-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 14,
	"-*-times-bold-r-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 18,
	"-*-times-bold-r-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_bold, 24,
	"-*-times-bold-r-normal--24-*-*-*-*-*-*-*"),
  PFONT(NAME_italic, 10,
	"-*-times-medium-i-normal--10-*-*-*-*-*-*-*"),
  PFONT(NAME_italic, 12,
	"-*-times-medium-i-normal--12-*-*-*-*-*-*-*"),
  PFONT(NAME_italic, 14,
	"-*-times-medium-i-normal--14-*-*-*-*-*-*-*"),
  PFONT(NAME_italic, 18,
	"-*-times-medium-i-normal--18-*-*-*-*-*-*-*"),
  PFONT(NAME_italic, 24,
	"-*-times-medium-i-normal--24-*-*-*-*-*-*-*"),
  ENDFONTLIST
};


static char *
default_font_list(Name fam, FontDef defs)
{ char buf[10240];
  char *s = buf;

#define LEFT() (sizeof(buf)-(s-buf)-1)

  *s++ = '[';

  while(defs->style)
  {
    if ( defs->xname )
    { snprintf(s, LEFT(),
	       "font(%s, %s, %d, \"%s\")",
	      strName(fam),
	      strName(defs->style),
	      defs->points,
	      defs->xname);
    } else
    { snprintf(s, LEFT(),
	      "font(%s, %s, %d)",
	      strName(fam),
	      strName(defs->style),
	      defs->points);
    }
    s += strlen(s);
    defs++;
    if ( defs->style && LEFT() >= 2 )
      strcpy(s, ",\n");
    s += strlen(s);
  }

  if ( LEFT() > 1 )
    *s++ = ']';
  *s = EOS;

  assert(LEFT() > 0);

  return save_string(buf);
}


static void
attach_fonts(Class class, char *res, Name fam, FontDef defs)
{ attach_class_variable(class, CtoName(res), "chain",
			default_font_list(fam, defs),
			"Font family set");
}


static void
attach_font_families(Class class)
{ attach_class_variable(class, NAME_fontFamilies,  "chain",
			"[screen_fonts,courier_fonts,"
			"helvetica_fonts,times_fonts]",
			"Predefined font families");

  attach_fonts(class, "courier_fonts",	 NAME_courier,	 courier_fonts);
  attach_fonts(class, "helvetica_fonts", NAME_helvetica, helvetica_fonts);
  attach_fonts(class, "times_fonts",	 NAME_times,	 times_fonts);
  attach_fonts(class, "screen_fonts",	 NAME_screen,	 screen_fonts);
}
