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
     NAME_name, "Human readable name of the device")
};

/* Send Methods */

static senddecl send_input_device[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseInputDevice,
     DEFAULT, "Create from kind, id and name")
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
 * event, e.g., the pressure of a pen.  This class only defines the
 * common super class.
 */

ClassDecl(event_detail_decls, NULL, NULL, NULL, NULL,
          ARGC_INHERIT, NULL);

status
makeClassEventDetail(Class class)
{ declareClass(class, &event_detail_decls);

  succeed;
}
