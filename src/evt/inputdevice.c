/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

/* An input_device describes the device that generated an event: the
 * mouse, a pen, a touch screen, a gamepad or the keyboard.  Devices the
 * window system reports are registered in InputDevices, so the same
 * physical device is represented by the same object.  See
 * findInputDevice().
 */

static Chain InputDevices;

static void
init_input_devices(void)
{ if ( !InputDevices )
    realiseClass(ClassInputDevice);
}

static status
initialiseInputDevice(InputDevice d, Name kind, Int id, Name name)
{ assign(d, kind, kind);
  assign(d, id,   isDefault(id) ? ZERO : id);
  assign(d, name, isDefault(name) ? kind : name);
  d->pointer = NULL;

  succeed;
}


static status
unlinkInputDevice(InputDevice d)
{ freePointerStateDevice(d);

  succeed;
}


/**
 * Find the registered device of `kind` with window system identifier
 * `id`.  If there is no such device, create and register one, using
 * `name` (UTF-8, may be NULL) as its name.
 */

InputDevice
findInputDevice(Name kind, intptr_t id, const char *name)
{ Int iid = toInt(id);
  Cell cell;

  init_input_devices();

  for_cell(cell, InputDevices)
  { InputDevice d = cell->value;

    if ( d->kind == kind && d->id == iid )
      return d;
  }

  InputDevice d = newObject(ClassInputDevice, kind, iid,
			    name ? (Name)UTF8ToName(name) : (Name)DEFAULT,
			    EAV);
  if ( d )
    appendChain(InputDevices, d);

  return d;
}


Chain
getInputDevicesDisplay(DisplayObj d)
{ init_input_devices();

  answer(InputDevices);
}


/* Type declarations */

static char *T_initialise[] =
        { "kind={mouse,pen,touch,gamepad,keyboard}", "id=[int]",
	  "name=[name]" };

/* Instance Variables */

static vardecl var_input_device[] =
{ IV(NAME_kind, "{mouse,pen,touch,gamepad,keyboard}", IV_GET,
     NAME_classify, "Kind of device"),
  IV(NAME_id, "int", IV_GET,
     NAME_name, "Window system identifier of the device"),
  IV(NAME_name, "name", IV_GET,
     NAME_name, "Human readable name of the device"),
  IV(NAME_pointer, "alien:PointerState", IV_NONE,
     NAME_event, "Defaults and multiclick state for its events")
};

/* Send Methods */

static senddecl send_input_device[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseInputDevice,
     DEFAULT, "Create from kind, id and name"),
  SM(NAME_unlink, 0, NULL, unlinkInputDevice,
     DEFAULT, "Free the event state")
};

/* Get Methods */

#define get_input_device NULL
/*
static getdecl get_input_device[] =
{
};
*/

/* Resources */

#define rc_input_device NULL
/*
static classvardecl rc_input_device[] =
{
};
*/

/* Class Declaration */

static Name input_device_termnames[] = { NAME_kind, NAME_id, NAME_name };

ClassDecl(input_device_decls,
          var_input_device, send_input_device, get_input_device,
	  rc_input_device,
          3, input_device_termnames);

status
makeClassInputDevice(Class class)
{ declareClass(class, &input_device_decls);

  InputDevices = globalObject(NAME_inputDevices, ClassChain, EAV);
  DEVICE_mouse = globalObject(NAME_mouseDevice, ClassInputDevice,
			      NAME_mouse, ZERO, EAV);
  DEVICE_keyboard = globalObject(NAME_keyboardDevice, ClassInputDevice,
				 NAME_keyboard, ZERO, EAV);
  appendChain(InputDevices, DEVICE_mouse);
  appendChain(InputDevices, DEVICE_keyboard);

  succeed;
}


		 /*******************************
		 *	   EVENT DETAIL		*
		 *******************************/

/* An event_detail holds the data that is specific for the kind of
 * event, e.g., the pressure of a pen.  Class event delegates to its
 * <-detail, so the methods of the detail are available on the event.
 * This class only defines the common super class.
 */

ClassDecl(event_detail_decls, NULL, NULL, NULL, NULL,
          ARGC_INHERIT, NULL);

status
makeClassEventDetail(Class class)
{ declareClass(class, &event_detail_decls);

  succeed;
}


		 /*******************************
		 *	     DROP DATA		*
		 *******************************/

/* Data of the drop_* events: the dropped file or text and the
 * application that sent it.  The getters fail if the drop does not
 * provide the value, e.g., <-path for a drop_text event.
 */

static status
initialiseDropData(DropData dd, Name path, StringObj text, Name source)
{ assign(dd, path,   isDefault(path)   ? NIL : path);
  assign(dd, text,   isDefault(text)   ? NIL : text);
  assign(dd, source, isDefault(source) ? NIL : source);

  succeed;
}


static Name
getPathDropData(DropData dd)
{ if ( notNil(dd->path) )
    answer(dd->path);

  fail;
}


static StringObj
getTextDropData(DropData dd)
{ if ( notNil(dd->text) )
    answer(dd->text);

  fail;
}


static Name
getSourceDropData(DropData dd)
{ if ( notNil(dd->source) )
    answer(dd->source);

  fail;
}


/* Type declarations */

static char *T_initialiseDropData[] =
        { "path=[name]", "text=[string]", "source=[name]" };

/* Instance Variables */

static vardecl var_drop_data[] =
{ IV(NAME_path, "name*", IV_NONE,
     NAME_drop, "Dropped file (drop_file)"),
  IV(NAME_text, "string*", IV_NONE,
     NAME_drop, "Dropped text (drop_text)"),
  IV(NAME_source, "name*", IV_NONE,
     NAME_drop, "Application that sent the drop")
};

/* Send Methods */

static senddecl send_drop_data[] =
{ SM(NAME_initialise, 3, T_initialiseDropData, initialiseDropData,
     DEFAULT, "Create from path, text and source")
};

/* Get Methods */

static getdecl get_drop_data[] =
{ GM(NAME_path, 0, "name", NULL, getPathDropData,
     NAME_drop, "Dropped file; fails if not a file drop"),
  GM(NAME_text, 0, "string", NULL, getTextDropData,
     NAME_drop, "Dropped text; fails if not a text drop"),
  GM(NAME_source, 0, "name", NULL, getSourceDropData,
     NAME_drop, "Application that sent the drop; fails if unknown")
};

/* Class Declaration */

static Name drop_data_termnames[] = { NAME_path, NAME_text, NAME_source };

ClassDecl(drop_data_decls,
          var_drop_data, send_drop_data, get_drop_data, NULL,
          3, drop_data_termnames);

status
makeClassDropData(Class class)
{ declareClass(class, &drop_data_decls);

  succeed;
}


		 /*******************************
		 *	     WHEEL DATA		*
		 *******************************/

/* Data of the wheel and horizontal_wheel events.  Unlike
 * event<-rotation, which reports whole notches in the dominant
 * direction, this holds the precise scroll distance of both directions
 * since the previous wheel event, as reported by trackpads.
 */

static status
initialiseWheelData(WheelData wd, Int dx, Int dy, BoolObj flipped)
{ assign(wd, dx,      dx);
  assign(wd, dy,      dy);
  assign(wd, flipped, isDefault(flipped) ? OFF : flipped);

  succeed;
}


/* Type declarations */

static char *T_initialiseWheelData[] =
        { "dx=num", "dy=num", "flipped=[bool]" };

/* Instance Variables */

static vardecl var_wheel_data[] =
{ IV(NAME_dx, "num", IV_GET,
     NAME_scroll, "Horizontal scroll in notches; positive is to the right"),
  IV(NAME_dy, "num", IV_GET,
     NAME_scroll, "Vertical scroll in notches; positive is up"),
  IV(NAME_flipped, "bool", IV_GET,
     NAME_scroll, "@on if the system uses natural scrolling")
};

/* Send Methods */

static senddecl send_wheel_data[] =
{ SM(NAME_initialise, 3, T_initialiseWheelData, initialiseWheelData,
     DEFAULT, "Create from dx, dy and flipped")
};

/* Class Declaration */

static Name wheel_data_termnames[] = { NAME_dx, NAME_dy, NAME_flipped };

ClassDecl(wheel_data_decls,
          var_wheel_data, send_wheel_data, NULL, NULL,
          3, wheel_data_termnames);

status
makeClassWheelData(Class class)
{ declareClass(class, &wheel_data_decls);

  succeed;
}
