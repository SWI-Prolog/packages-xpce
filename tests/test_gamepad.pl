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

:- module(test_gamepad, [test_gamepad/0]).

/** <module> Gamepads

Uses a virtual gamepad provided by SDL (see `display->sdl_test_gamepad`),
for which SDL generates the same events as for a real gamepad.

Run with:

    swipl -g test_gamepad -t halt packages/xpce/tests/test_gamepad.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_gamepad :-
    run_tests([ gamepad ]).

:- dynamic
    seen/1.                             % seen(Event)

:- pce_begin_class(gamepad_window, window,
                   "Window that records gamepad and device events").

event(_W, Ev:event) :->
    "Record a copy of gamepad and device events"::
    (   ( send(Ev, is_a, gamepad) ; send(Ev, is_a, device) )
    ->  get(Ev, clone, Copy),
        send(Copy, lock_object, @on),
        assertz(seen(Copy))
    ;   fail
    ).

:- pce_end_class(gamepad_window).

%!  open_window(-Window) is det.
%
%   Open a window that records events.  Gamepad input is only delivered
%   while one of our windows has the keyboard focus, which the window
%   system may give it a little later (e.g., Wine under load), so we wait
%   for that.

open_window(W) :-
    new(W, gamepad_window('', size(200, 200))),
    get(W, frame, F),
    send(F, open),
    send(F, expose),                    % gives it the keyboard focus
    ignore(pump(get(F, input_focus, @on))),
    retractall(seen(_)).

close_window(W) :-
    get(W, frame, F),
    send(F, destroy),
    retractall(seen(_)).

%!  pump(:Goal) is semidet.
%
%   Dispatch events until Goal succeeds, for at most 2 seconds.

pump(Goal) :-
    get_time(T0),
    repeat,
    (   call(Goal)
    ->  !
    ;   get_time(T),
        T - T0 > 2
    ->  !, fail
    ;   ignore(send(@display, dispatch)),
        fail
    ).

seen_id(Id, E) :-
    seen(E),
    get(E, id, Id).

gp(Action) :-
    send(@display, sdl_test_gamepad, Action).
gp(Action, Control, Value) :-
    send(@display, sdl_test_gamepad, Action, Control, Value).

%!  detach_pad is det.
%
%   Detach the virtual gamepad and wait until xpce has removed it.

detach_pad :-
    gp(detach),
    ignore(pump(\+ test_pad(_))).

test_pad(Pad) :-
    get(@display, gamepads, Chain),
    chain_list(Chain, Pads),
    member(Pad, Pads),
    get(Pad, name, 'xpce test gamepad'),
    !.

:- begin_tests(gamepad, [ setup(get(@display, gamepads, _)),
                          cleanup(detach_pad)
                        ]).

test(hotplug, [Kind-Connected-Removed == gamepad-(@on)-(@off)]) :-
    open_window(W),
    gp(attach),
    pump(seen_id(device_added, _)),
    seen_id(device_added, E),
    get(E, device, Pad),
    get(Pad, kind, Kind),
    test_pad(Pad),
    get(Pad, connected, Connected),
    gp(detach),
    pump(seen_id(device_removed, _)),
    get(Pad, connected, Removed),
    \+ test_pad(_),
    close_window(W).
test(button_event, [Ids-Controls-Values ==
                    [gamepad_button_down, gamepad_button_up]-
                    [south, south]-[1, 0]]) :-
    open_window(W),
    gp(attach),
    pump(seen_id(device_added, _)),
    gp(button, south, 1),
    pump(seen_id(gamepad_button_down, _)),
    gp(button, south, 0),
    pump(seen_id(gamepad_button_up, _)),
    findall(E, (seen(E), send(E, is_a, gamepad)), Es),
    maplist([E,I]>>get(E, id, I), Es, Ids),
    maplist([E,C]>>get(E, control, C), Es, Controls),
    maplist([E,V]>>get(E, value, V), Es, Values),
    detach_pad,
    close_window(W).
test(axis_event, [C-V == left_x-0.5]) :-
    open_window(W),
    gp(attach),
    pump(seen_id(device_added, _)),
    gp(axis, left_x, 0.5),
    pump(seen_id(gamepad_axis, _)),
    seen_id(gamepad_axis, E),
    get(E, control, C),
    get(E, value, V0),
    V is round(V0*100)/100.0,
    detach_pad,
    close_window(W).
test(poll, [B-A == (@on)-(-0.25)]) :-
    open_window(W),
    gp(attach),
    pump(seen_id(device_added, _)),
    gp(button, east, 1),
    gp(axis, right_y, -0.25),
    pump(seen_id(gamepad_axis, _)),
    test_pad(Pad),
    get(Pad, button, east, B),
    get(Pad, axis, right_y, A0),
    A is round(A0*100)/100.0,
    detach_pad,
    close_window(W).
test(rumble_and_led, [Out == [0.5, 1.0, 255.0, 128.0, 0.0]]) :-
    open_window(W),
    gp(attach),
    pump(seen_id(device_added, _)),
    test_pad(Pad),
    send(Pad, rumble, 0.5, 1.0, 100),
    send(Pad, led, colour(@default, 255, 128, 0)),
    findall(V, ( member(What, [rumble_low, rumble_high, red, green, blue]),
                 get(@display, sdl_test_gamepad, What, V0),
                 V is round(V0*100)/100.0
               ), Out),
    detach_pad,
    close_window(W).

:- end_tests(gamepad).
