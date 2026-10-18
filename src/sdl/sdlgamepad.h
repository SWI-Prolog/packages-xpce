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

#ifndef SDLGAMEPAD_H
#define SDLGAMEPAD_H
#include <SDL3/SDL.h>

void	ws_init_gamepads(void);
void	ws_input_device_removed(InputDevice d);
bool	sdl_gamepad_hotplug_event(SDL_Event *event);
bool	sdl_gamepad_input_event(SDL_Event *event, Any *device, Any *name,
				Any *detail, unsigned int *time);
bool	ws_gamepad_button(Gamepad g, Name button, bool *down);
bool	ws_gamepad_axis(Gamepad g, Name axis, double *value);
Name	ws_gamepad_type(Gamepad g);
int	ws_gamepad_player_index(Gamepad g);
bool	ws_gamepad_rumble(Gamepad g, double low, double high, int ms);
bool	ws_gamepad_rumble_triggers(Gamepad g, double left, double right,
				   int ms);
bool	ws_gamepad_led(Gamepad g, int r, int gr, int b);
status	ws_test_gamepad(DisplayObj d, Name action, Name control, Int value);
Int	ws_test_gamepad_output(DisplayObj d, Name what);

#endif /*SDLGAMEPAD_H*/
