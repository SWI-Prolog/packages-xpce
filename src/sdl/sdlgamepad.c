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


/* Gamepads (game controllers).  The SDL gamepad subsystem is started
 * by ws_init_gamepads(), which display<-gamepads calls: starting it may
 * be slow and, on some systems, ask the user for permission, so we
 * only do so if the application wants gamepads.
 *
 * Controls are named after their position, as the SDL enum constants:
 * `south` is the bottom face button, labelled A on an Xbox controller
 * and with a cross on a PlayStation controller.
 */

static bool gamepads_initialised = false;

static const char *button_names[] =
{ "south", "east", "west", "north",
  "back", "guide", "start",
  "left_stick", "right_stick",
  "left_shoulder", "right_shoulder",
  "dpad_up", "dpad_down", "dpad_left", "dpad_right",
  "misc1",
  "right_paddle1", "left_paddle1", "right_paddle2", "left_paddle2",
  "touchpad",
  "misc2", "misc3", "misc4", "misc5", "misc6"
};

static const char *axis_names[] =
{ "left_x", "left_y", "right_x", "right_y",
  "left_trigger", "right_trigger"
};

#define NBUTTONS (int)(sizeof(button_names)/sizeof(button_names[0]))
#define NAXES    (int)(sizeof(axis_names)/sizeof(axis_names[0]))

static Name
button_name(int button)
{ if ( button >= 0 && button < NBUTTONS )
    return CtoName(button_names[button]);

  return NULL;
}

static int
button_index(Name name)
{ for(int i=0; i<NBUTTONS; i++)
  { if ( streq(strName(name), button_names[i]) )
      return i;
  }

  return -1;
}

static Name
axis_name(int axis)
{ if ( axis >= 0 && axis < NAXES )
    return CtoName(axis_names[axis]);

  return NULL;
}

static int
axis_index(Name name)
{ for(int i=0; i<NAXES; i++)
  { if ( streq(strName(name), axis_names[i]) )
      return i;
  }

  return -1;
}

/* Sticks are -1..1, triggers 0..1 */

static double
axis_value(Sint16 v)
{ double d = (double)v/32767.0;

  return d < -1.0 ? -1.0 : d;
}


static Gamepad
open_gamepad(SDL_JoystickID id)
{ Gamepad g = (Gamepad)findInputDevice(NAME_gamepad, (intptr_t)id,
				       SDL_GetGamepadNameForID(id));

  if ( g && !g->ws_ref )
  { g->ws_ref = SDL_OpenGamepad(id);
    if ( !g->ws_ref )
      Cprintf("Could not open gamepad %d: %s\n", (int)id, SDL_GetError());
  }

  return g;
}


void
ws_init_gamepads(void)
{ if ( gamepads_initialised )
    return;

  if ( !SDL_InitSubSystem(SDL_INIT_GAMEPAD) )
  { Cprintf("Could not initialise gamepads: %s\n", SDL_GetError());
    return;
  }
  gamepads_initialised = true;

  int count = 0;
  SDL_JoystickID *ids = SDL_GetGamepads(&count);
  for(int i=0; i<count; i++)
    open_gamepad(ids[i]);
  SDL_free(ids);
}


void
ws_input_device_removed(InputDevice d)
{ if ( instanceOfObject(d, ClassGamepad) )
  { Gamepad g = (Gamepad)d;

    if ( g->ws_ref )
    { SDL_CloseGamepad(g->ws_ref);
      g->ws_ref = NULL;
    }
  }
  ws_forget_pointer(d);
}


		 /*******************************
		 *	      EVENTS		*
		 *******************************/

/* Gamepads that are added or removed.  Returns true if `event` was
 * handled here.
 */

bool
sdl_gamepad_hotplug_event(SDL_Event *event)
{ switch(event->type)
  { case SDL_EVENT_GAMEPAD_ADDED:
    { Gamepad g = open_gamepad(event->gdevice.which);

      if ( g )
	sdl_post_device_event(NAME_deviceAdded, (InputDevice)g);
      return true;
    }
    case SDL_EVENT_GAMEPAD_REMOVED:
    { InputDevice d = findInputDevice(NAME_gamepad,
				      (intptr_t)event->gdevice.which, NULL);

      if ( d )
      { addCodeReference(d);
	removeInputDevice(d);
	sdl_post_device_event(NAME_deviceRemoved, d);
	delCodeReference(d);
	freeableObj(d);
      }
      return true;
    }
    default:
      return false;
  }
}


/* Translate the button and axis events of a gamepad.  Fails if this is
 * not such an event or it cannot be translated.
 */

bool
sdl_gamepad_input_event(SDL_Event *event, Any *device, Any *name,
			Any *detail, unsigned int *time)
{ Name control;
  double value;
  SDL_JoystickID which;

  switch(event->type)
  { case SDL_EVENT_GAMEPAD_BUTTON_DOWN:
    case SDL_EVENT_GAMEPAD_BUTTON_UP:
      if ( !(control = button_name(event->gbutton.button)) )
	return false;
      value = event->gbutton.down ? 1.0 : 0.0;
      which = event->gbutton.which;
      *time = event->gbutton.timestamp/1000000;
      *name = ( event->gbutton.down ? NAME_gamepadButtonDown
				    : NAME_gamepadButtonUp );
      break;
    case SDL_EVENT_GAMEPAD_AXIS_MOTION:
      if ( !(control = axis_name(event->gaxis.axis)) )
	return false;
      value = axis_value(event->gaxis.value);
      which = event->gaxis.which;
      *time = event->gaxis.timestamp/1000000;
      *name = NAME_gamepadAxis;
      break;
    default:
      return false;
  }

  if ( !(*device = open_gamepad(which)) )
    return false;
  *detail = answerObject(ClassGamepadInput, control, toNum(value), EAV);

  return true;
}


		 /*******************************
		 *	  POLLING & OUTPUT	*
		 *******************************/

bool
ws_gamepad_button(Gamepad g, Name button, bool *down)
{ int i = button_index(button);

  if ( !g->ws_ref || i < 0 )
    return false;
  *down = SDL_GetGamepadButton(g->ws_ref, (SDL_GamepadButton)i);

  return true;
}


bool
ws_gamepad_axis(Gamepad g, Name axis, double *value)
{ int i = axis_index(axis);

  if ( !g->ws_ref || i < 0 )
    return false;
  *value = axis_value(SDL_GetGamepadAxis(g->ws_ref, (SDL_GamepadAxis)i));

  return true;
}


Name
ws_gamepad_type(Gamepad g)
{ const char *s;

  if ( g->ws_ref &&
       (s = SDL_GetGamepadStringForType(SDL_GetGamepadType(g->ws_ref))) )
    return CtoName(s);

  return NAME_unknown;
}


int
ws_gamepad_player_index(Gamepad g)
{ return g->ws_ref ? SDL_GetGamepadPlayerIndex(g->ws_ref) : -1;
}


static Uint16
motor(double v)
{ return (Uint16)(v*65535.0 + 0.5);
}

bool
ws_gamepad_rumble(Gamepad g, double low, double high, int ms)
{ return g->ws_ref && SDL_RumbleGamepad(g->ws_ref, motor(low), motor(high),
					  (Uint32)ms);
}


bool
ws_gamepad_rumble_triggers(Gamepad g, double left, double right, int ms)
{ return g->ws_ref && SDL_RumbleGamepadTriggers(g->ws_ref,
						 motor(left), motor(right),
						 (Uint32)ms);
}


bool
ws_gamepad_led(Gamepad g, int r, int gr, int b)
{ return g->ws_ref && SDL_SetGamepadLED(g->ws_ref,
					 (Uint8)r, (Uint8)gr, (Uint8)b);
}


		 /*******************************
		 *	  TESTING SUPPORT	*
		 *******************************/

/* A virtual gamepad for testing (see display->sdl_test_gamepad).  The
 * events SDL generates for it are real gamepad events.  Rumble and LED
 * output is recorded, so tests can check it.
 */

static SDL_JoystickID test_gamepad_id = 0;
static SDL_Joystick  *test_gamepad = NULL;
static double test_output[5];		/* low, high, red, green, blue */

static bool SDLCALL
test_rumble(void *userdata, Uint16 low, Uint16 high)
{ test_output[0] = low/65535.0;
  test_output[1] = high/65535.0;
  return true;
}

static bool SDLCALL
test_set_led(void *userdata, Uint8 r, Uint8 g, Uint8 b)
{ test_output[2] = r;
  test_output[3] = g;
  test_output[4] = b;
  return true;
}

status
ws_test_gamepad(DisplayObj d, Name action, Name control, Int value)
{ ws_init_gamepads();

  if ( action == NAME_attach )
  { SDL_VirtualJoystickDesc desc;

    if ( test_gamepad )
      succeed;
    SDL_INIT_INTERFACE(&desc);
    desc.type	     = SDL_JOYSTICK_TYPE_GAMEPAD;
    desc.naxes	     = NAXES;
    desc.nbuttons    = NBUTTONS;
    desc.button_mask = (Uint32)((1ULL<<NBUTTONS)-1);
    desc.axis_mask   = (Uint32)((1ULL<<NAXES)-1);
    desc.name	     = "xpce test gamepad";
    desc.Rumble	     = test_rumble;
    desc.SetLED	     = test_set_led;
    if ( !(test_gamepad_id = SDL_AttachVirtualJoystick(&desc)) ||
	 !(test_gamepad = SDL_OpenJoystick(test_gamepad_id)) )
    { Cprintf("Could not attach virtual gamepad: %s\n", SDL_GetError());
      fail;
    }
    memset(test_output, 0, sizeof(test_output));
    succeed;
  } else if ( action == NAME_detach )
  { if ( test_gamepad )
    { SDL_CloseJoystick(test_gamepad);
      SDL_DetachVirtualJoystick(test_gamepad_id);
      test_gamepad = NULL;
      test_gamepad_id = 0;
    }
    succeed;
  }

  if ( !test_gamepad || isDefault(control) )
    fail;
  if ( action == NAME_button )
  { int i = button_index(control);

    return i >= 0 &&
	   SDL_SetJoystickVirtualButton(test_gamepad, i,
					isDefault(value) || valNum(value) != 0);
  } else if ( action == NAME_axis )
  { int i = axis_index(control);
    double v = isDefault(value) ? 0.0 : valNum(value);

    return i >= 0 &&
	   SDL_SetJoystickVirtualAxis(test_gamepad, i, (Sint16)(v*32767.0));
  }

  fail;
}

Int
ws_test_gamepad_output(DisplayObj d, Name what)
{ static const char *names[] =
    { "rumble_low", "rumble_high", "red", "green", "blue" };

  for(int i=0; i<5; i++)
  { if ( streq(strName(what), names[i]) )
      return toNum(test_output[i]);
  }

  fail;
}
