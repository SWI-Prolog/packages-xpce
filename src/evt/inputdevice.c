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

  InputDevice d = newObject(kind == NAME_gamepad ? ClassGamepad
						  : ClassInputDevice,
			    kind, iid,
			    name ? (Name)UTF8ToName(name) : (Name)DEFAULT,
			    EAV);
  if ( d )
    appendChain(InputDevices, d);

  return d;
}


/**
 * The window system reported that `d` was removed.  Remove it from the
 * registry.  The object remains valid for as long as it is referenced,
 * e.g., by an event, but no longer generates events.
 */

void
removeInputDevice(InputDevice d)
{ init_input_devices();

  addCodeReference(d);
  deleteChain(InputDevices, d);
  freePointerStateDevice(d);
  ws_input_device_removed(d);
  delCodeReference(d);
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


		 /*******************************
		 *	      PEN STATE		*
		 *******************************/

/* The state of a pen when it generated the event: the axes reported by
 * the tablet and the buttons on its barrel.  The names follow SDL,
 * except for `hover_distance` and `barrel_rotation`, as an event
 * already has a <-distance and a <-rotation.  Axes the pen does not
 * have are 0.
 */

static Int
num_default(Int v)
{ return isDefault(v) ? ZERO : v;
}

static status
initialisePenState(PenState ps, Int pressure, Int xtilt, Int ytilt,
		   Int hover_distance, Int barrel_rotation, Int slider,
		   Int tangential_pressure, BoolObj eraser,
		   Int barrel_buttons)
{ assign(ps, pressure,		  num_default(pressure));
  assign(ps, xtilt,		  num_default(xtilt));
  assign(ps, ytilt,		  num_default(ytilt));
  assign(ps, hover_distance,	  num_default(hover_distance));
  assign(ps, barrel_rotation,	  num_default(barrel_rotation));
  assign(ps, slider,		  num_default(slider));
  assign(ps, tangential_pressure, num_default(tangential_pressure));
  assign(ps, eraser,		  isDefault(eraser) ? OFF : eraser);
  assign(ps, barrel_buttons,	  num_default(barrel_buttons));

  succeed;
}


/* Type declarations */

static char *T_initialisePenState[] =
        { "pressure=[num]", "xtilt=[num]", "ytilt=[num]",
	  "hover_distance=[num]", "barrel_rotation=[num]", "slider=[num]",
	  "tangential_pressure=[num]", "eraser=[bool]",
	  "barrel_buttons=[int]" };

/* Instance Variables */

static vardecl var_pen_state[] =
{ IV(NAME_pressure, "num", IV_GET,
     NAME_pen, "Pressure of the tip, 0..1"),
  IV(NAME_xtilt, "num", IV_GET,
     NAME_pen, "Horizontal tilt, -90..90 degrees (left to right)"),
  IV(NAME_ytilt, "num", IV_GET,
     NAME_pen, "Vertical tilt, -90..90 degrees (top to bottom)"),
  IV(NAME_hoverDistance, "num", IV_GET,
     NAME_pen, "Distance of the tip to the surface, 0..1"),
  IV(NAME_barrelRotation, "num", IV_GET,
     NAME_pen, "Rotation of the barrel, -180..180 degrees, clockwise"),
  IV(NAME_slider, "num", IV_GET,
     NAME_pen, "Position of the finger wheel or slider, 0..1"),
  IV(NAME_tangentialPressure, "num", IV_GET,
     NAME_pen, "Pressure from squeezing the barrel, 0..1"),
  IV(NAME_eraser, "bool", IV_GET,
     NAME_pen, "@on if the eraser end of the pen is used"),
  IV(NAME_barrelButtons, "int", IV_GET,
     NAME_pen, "Mask of the barrel buttons that are down (1: first, ...)")
};

/* Send Methods */

static senddecl send_pen_state[] =
{ SM(NAME_initialise, 9, T_initialisePenState, initialisePenState,
     DEFAULT, "Create from the axes and buttons")
};

/* Class Declaration */

static Name pen_state_termnames[] =
{ NAME_pressure, NAME_xtilt, NAME_ytilt, NAME_hoverDistance,
  NAME_barrelRotation, NAME_slider, NAME_tangentialPressure,
  NAME_eraser, NAME_barrelButtons
};

ClassDecl(pen_state_decls,
          var_pen_state, send_pen_state, NULL, NULL,
          9, pen_state_termnames);

status
makeClassPenState(Class class)
{ declareClass(class, &pen_state_decls);

  succeed;
}


		 /*******************************
		 *	     TOUCH POINT	*
		 *******************************/

/* A finger on a touch screen.  The first finger that touches the
 * screen generates the ms_left_* events, the others the touch_* events.
 * <-finger identifies the finger during the time it touches the
 * screen.
 */

static status
initialiseTouchPoint(TouchPoint tp, Int finger, Int pressure,
		     Int dx, Int dy)
{ assign(tp, finger,   finger);
  assign(tp, pressure, num_default(pressure));
  assign(tp, dx,       num_default(dx));
  assign(tp, dy,       num_default(dy));

  succeed;
}


/* Type declarations */

static char *T_initialiseTouchPoint[] =
        { "finger=int", "pressure=[num]", "dx=[num]", "dy=[num]" };

/* Instance Variables */

static vardecl var_touch_point[] =
{ IV(NAME_finger, "int", IV_GET,
     NAME_touch, "Identifies the finger while it touches the screen"),
  IV(NAME_pressure, "num", IV_GET,
     NAME_touch, "Pressure, 0..1"),
  IV(NAME_dx, "num", IV_GET,
     NAME_touch, "Horizontal motion in pixels"),
  IV(NAME_dy, "num", IV_GET,
     NAME_touch, "Vertical motion in pixels")
};

/* Send Methods */

static senddecl send_touch_point[] =
{ SM(NAME_initialise, 4, T_initialiseTouchPoint, initialiseTouchPoint,
     DEFAULT, "Create from finger, pressure and motion")
};

/* Class Declaration */

static Name touch_point_termnames[] =
{ NAME_finger, NAME_pressure, NAME_dx, NAME_dy };

ClassDecl(touch_point_decls,
          var_touch_point, send_touch_point, NULL, NULL,
          4, touch_point_termnames);

status
makeClassTouchPoint(Class class)
{ declareClass(class, &touch_point_decls);

  succeed;
}


		 /*******************************
		 *	     PINCH DATA		*
		 *******************************/

/* Data of the pinch_* events: the change of scale since the previous
 * pinch_update.  A scale above 1 zooms in.
 */

static status
initialisePinchData(PinchData pd, Int scale)
{ assign(pd, scale, isDefault(scale) ? ONE : scale);

  succeed;
}


/* Instance Variables */

static vardecl var_pinch_data[] =
{ IV(NAME_scale, "num", IV_GET,
     NAME_pinch, "Change of scale since the last update; > 1 zooms in")
};

/* Send Methods */

static senddecl send_pinch_data[] =
{ SM(NAME_initialise, 1, "scale=[num]", initialisePinchData,
     DEFAULT, "Create from scale")
};

/* Class Declaration */

static Name pinch_data_termnames[] = { NAME_scale };

ClassDecl(pinch_data_decls,
          var_pinch_data, send_pinch_data, NULL, NULL,
          1, pinch_data_termnames);

status
makeClassPinchData(Class class)
{ declareClass(class, &pinch_data_decls);

  succeed;
}


		 /*******************************
		 *	      GAMEPAD		*
		 *******************************/

/* A gamepad (game controller).  Its buttons and axes generate the
 * gamepad_* events, delivered to the window with the keyboard focus.
 * It can also be polled, e.g., in a game loop, and it may be able to
 * rumble or show a colour.  The names of the controls follow the
 * position of the buttons rather than their label: `south` is the
 * bottom face button (A on an Xbox controller and the cross on a
 * PlayStation controller).
 */

static status
initialiseGamepad(Gamepad g, Name kind, Int id, Name name)
{ TRY(initialiseInputDevice((InputDevice)g, kind, id, name));
  g->ws_ref = NULL;

  succeed;
}


static BoolObj
getConnectedGamepad(Gamepad g)
{ answer(g->ws_ref ? ON : OFF);
}


static BoolObj
getButtonGamepad(Gamepad g, Name button)
{ bool down;

  if ( ws_gamepad_button(g, button, &down) )
    answer(down ? ON : OFF);

  fail;
}


static Int
getAxisGamepad(Gamepad g, Name axis)
{ double value;

  if ( ws_gamepad_axis(g, axis, &value) )
    answer(toNum(value));

  fail;
}


static Name
getTypeGamepad(Gamepad g)
{ answer(ws_gamepad_type(g));
}


static Int
getPlayerIndexGamepad(Gamepad g)
{ int i = ws_gamepad_player_index(g);

  if ( i >= 0 )
    answer(toInt(i));

  fail;
}


static status
rumbleGamepad(Gamepad g, Int low, Int high, Int ms)
{ return ws_gamepad_rumble(g, valNum(low), valNum(high), valInt(ms));
}


static status
rumbleTriggersGamepad(Gamepad g, Int left, Int right, Int ms)
{ return ws_gamepad_rumble_triggers(g, valNum(left), valNum(right),
				    valInt(ms));
}


static status
ledGamepad(Gamepad g, Colour c)
{ return ws_gamepad_led(g,
			valInt(getRedColour(c)),
			valInt(getGreenColour(c)),
			valInt(getBlueColour(c)));
}


/* Type declarations */

static char *T_rumble[] =
        { "low=0.0..1.0", "high=0.0..1.0", "milliseconds=int" };
static char *T_rumbleTriggers[] =
        { "left=0.0..1.0", "right=0.0..1.0", "milliseconds=int" };

/* Instance Variables */

static vardecl var_gamepad[] =
{ IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "SDL_Gamepad handle")
};

/* Send Methods */

static senddecl send_gamepad[] =
{ SM(NAME_initialise, 3, T_initialise, initialiseGamepad,
     DEFAULT, "Create from kind, id and name"),
  SM(NAME_rumble, 3, T_rumble, rumbleGamepad,
     NAME_output, "Rumble the low and high frequency motors"),
  SM(NAME_rumbleTriggers, 3, T_rumbleTriggers, rumbleTriggersGamepad,
     NAME_output, "Rumble the motors in the triggers"),
  SM(NAME_led, 1, "colour", ledGamepad,
     NAME_output, "Set the colour of the LED")
};

/* Get Methods */

static getdecl get_gamepad[] =
{ GM(NAME_connected, 0, "bool", NULL, getConnectedGamepad,
     NAME_status, "@off if the gamepad was removed"),
  GM(NAME_button, 1, "bool", "name", getButtonGamepad,
     NAME_poll, "@on if the named button is down"),
  GM(NAME_axis, 1, "num", "name", getAxisGamepad,
     NAME_poll, "Position of the named axis, -1..1 or 0..1 (trigger)"),
  GM(NAME_type, 0, "name", NULL, getTypeGamepad,
     NAME_name, "Type of controller, e.g., xbox360, ps5"),
  GM(NAME_playerIndex, 0, "int", NULL, getPlayerIndexGamepad,
     NAME_name, "Player index or fail if not set")
};

/* Class Declaration */

ClassDecl(gamepad_decls,
          var_gamepad, send_gamepad, get_gamepad, NULL,
          3, input_device_termnames);

status
makeClassGamepad(Class class)
{ declareClass(class, &gamepad_decls);

  succeed;
}


Chain
getGamepadsDisplay(DisplayObj d)
{ Chain ch = answerObject(ClassChain, EAV);
  Cell cell;

  init_input_devices();
  ws_init_gamepads();
  for_cell(cell, InputDevices)
  { InputDevice dev = cell->value;

    if ( dev->kind == NAME_gamepad )
      appendChain(ch, dev);
  }

  answer(ch);
}


		 /*******************************
		 *	   GAMEPAD INPUT	*
		 *******************************/

/* Data of the gamepad_* events: the name of the button or axis and its
 * value: 1 or 0 for a button and the position for an axis.
 */

static status
initialiseGamepadInput(GamepadInput gi, Name control, Int value)
{ assign(gi, control, control);
  assign(gi, value,   value);

  succeed;
}


/* Type declarations */

static char *T_initialiseGamepadInput[] =
        { "control=name", "value=num" };

/* Instance Variables */

static vardecl var_gamepad_input[] =
{ IV(NAME_control, "name", IV_GET,
     NAME_gamepad, "Name of the button or axis"),
  IV(NAME_value, "num", IV_GET,
     NAME_gamepad, "1 or 0 for a button, the position of an axis")
};

/* Send Methods */

static senddecl send_gamepad_input[] =
{ SM(NAME_initialise, 2, T_initialiseGamepadInput, initialiseGamepadInput,
     DEFAULT, "Create from control and value")
};

/* Class Declaration */

static Name gamepad_input_termnames[] = { NAME_control, NAME_value };

ClassDecl(gamepad_input_decls,
          var_gamepad_input, send_gamepad_input, NULL, NULL,
          2, gamepad_input_termnames);

status
makeClassGamepadInput(Class class)
{ declareClass(class, &gamepad_input_decls);

  succeed;
}
