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

:- module(test_sdl_input, [test_sdl_input/0]).

/** <module> Input as reported by SDL

Feeds SDL events to the event handling of xpce using
`frame->sdl_test_event`, which allows testing input devices that are
not available on the test machine, such as pens and touch screens.

Run with:

    swipl -g test_sdl_input -t halt \
          packages/xpce/tests/test_sdl_input.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(pan_zoom)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_sdl_input :-
    run_tests([ sdl_wheel,
                sdl_emulation,
                sdl_pen,
                sdl_touch,
                sdl_pinch,
                sdl_keyboard,
                pan_zoom
              ]).

:- dynamic
    seen/1.                             % seen(Event)

:- pce_begin_class(recording_window, window,
                   "Window that records the events it receives").

event(_P, Ev:event) :->
    "Record a copy of the event"::
    get(Ev, id, Id),
    (   Id == loc_still
    ->  true
    ;   get(Ev, clone, Copy),
        send(Copy, lock_object, @on),
        assertz(seen(Copy))
    ).

:- pce_end_class(recording_window).

:- pce_begin_class(focus_window, recording_window,
                   "Recording window to give the keyboard focus").
:- pce_end_class(focus_window).

:- pce_begin_class(recording_text_item, text_item,
                   "Text item that records the events it receives").

event(TI, Ev:event) :->
    "Record a copy of the event and handle it"::
    get(Ev, clone, Copy),
    send(Copy, lock_object, @on),
    assertz(seen(Copy)),
    send_super(TI, event, Ev).

:- pce_end_class(recording_text_item).

%!  open_window(-Frame, -Window) is det.
%
%   Open a frame with a recording window of 200x200.  Clear the
%   recorded events.  The frame is destroyed by close_windows/0, the
%   cleanup of the test units.

open_window(F, W) :-
    close_windows,
    new(W, recording_window('', size(200, 200))),
    get(W, frame, F),
    send(F, open),
    retractall(seen(_)).

close_windows :-
    retractall(seen(_)),
    get(@display, frames, Frames),
    chain_list(Frames, List),
    forall(( member(Fr, List),
             get(Fr, members, Ms),
             send(Ms, find, message(@arg1, instance_of, recording_window))
           ),
           send(Fr, destroy)).

%!  sdl(+Frame, +Type, +Fields) is det.
%
%   Process an SDL event of Type with Fields, a list of Name=Value.

sdl(F, Type, Fields) :-
    new(S, sheet),
    forall(member(N=V, Fields), send(S, value, N, V)),
    send(F, sdl_test_event, Type, S).

%!  events(-Ids) is det.
%!  events(+Kind, -Events) is det.
%
%   Recorded event ids, or recorded events whose id is_a Kind.

events(Ids) :-
    findall(Id, (seen(E), get(E, id, Id)), Ids).

events(Kind, Events) :-
    findall(E, (seen(E), send(E, is_a, Kind)), Events).

%!  pump_for(+Seconds) is det.
%
%   Dispatch events for Seconds, e.g., to let a timer fire.

pump_for(Seconds) :-
    get_time(T0),
    repeat,
    ignore(send(@display, dispatch)),
    get_time(T),
    T - T0 > Seconds,
    !.

%!  key_down(+Frame, +Key, +Scancode, +Mod) is det.

key_down(F, Key, Scancode, Mod) :-
    sdl(F, key_down, [key=Key, scancode=Scancode, mod=Mod]).

key_up(F, Key, Scancode) :-
    sdl(F, key_up, [key=Key, scancode=Scancode]).

%!  keys(-Keys) is det.
%
%   The recorded key events as Id-Key-Repeat.

keys(Keys) :-
    findall(Id-Key-Repeat,
            ( seen(E),
              get(E, id, Id),
              get(E, key, Key),
              get(E, repeat, Repeat)
            ),
            Keys).

%!  receivers(-Pairs) is det.
%
%   The recorded key events as Id-ReceiverClass.

receivers(Pairs) :-
    findall(Id-Class,
            ( seen(E),
              get(E, key, _),
              get(E, id, Id),
              get(E?receiver, class_name, Class)
            ),
            Pairs).

:- begin_tests(sdl_keyboard, [cleanup(close_windows)]).

%       A window without text input gets the characters of the keys
%       from the key-down events, as soon as they arrive.  The keycode
%       of SDL is the unshifted key; the keymap gives the shifted one.

test(plain_window, Ids == [0'a, 0'A, 0'!, 'RET', 1]) :-
    open_window(F, _),
    key_down(F, 0'a, 4, 0),             % a
    key_down(F, 0'a, 4, 0x1),           % Shift-a
    key_down(F, 0'1, 30, 0x1),          % Shift-1
    key_down(F, 13, 40, 0),             % Return
    key_down(F, 0'a, 4, 0x40),          % Control-a
    events(Ids).

%       With text input the character comes from the text input event:
%       the key-down that has none must not type it as well.  We only
%       look at the key events, as pumping also delivers the events of
%       the real pointer if it is over the window.

test(text_input, Keys-Value == []-'') :-
    open_window(F, W),
    send(W, display, new(TI, text_item(name, ''))),
    send(W, keyboard_focus, TI),
    key_down(F, 0'a, 4, 0),
    pump_for(0.2),
    keys(Keys),
    get(TI, selection, Value).

%       The release of a key is a key_up event with the key of its
%       key-down, also if a modifier was released first.  An auto-repeat
%       of a key-down says so.  Keys that gave no key-down event, such as
%       Shift, give no key_up either.

test(release, Keys == [ 0'a-a-(@off), 0'a-a-(@on), key_up-a-(@off),
                        0'A-'A'-(@off), key_up-'A'-(@off),
                        'RET'-'RET'-(@off), key_up-'RET'-(@off)
                      ]) :-
    open_window(F, _),
    key_down(F, 0'a, 4, 0),
    sdl(F, key_down, [key=0'a, scancode=4, repeat=1]),
    key_up(F, 0'a, 4),
    key_down(F, 0x400000e1, 225, 0x1),  % Left Shift
    key_down(F, 0'a, 4, 0x1),
    key_up(F, 0x400000e1, 225),
    key_up(F, 0'a, 4),
    key_down(F, 13, 40, 0),
    key_up(F, 13, 40),
    keys(Keys).

%       With text input the key-down gives its character through the
%       text input event.  Its release goes to the keyboard focus, as the
%       key-down did, which does not type anything for it.

test(release_with_text_input,
     Keys-Pairs-Value == [ 0'a-a-(@off), key_up-a-(@off) ] -
                         [ 0'a-recording_text_item,
                           key_up-recording_text_item
                         ] - a) :-
    open_window(F, W),
    send(W, display, new(TI, recording_text_item(name, ''))),
    send(W, keyboard_focus, TI),
    retractall(seen(_)),
    key_down(F, 0'a, 4, 0),
    sdl(F, text_input, [char=0'a]),
    key_up(F, 0'a, 4),
    keys(Keys),
    receivers(Pairs),
    get(TI, selection, Value).

%       The release goes to the window with the keyboard focus, not to
%       the one under the pointer.

test(release_to_focus, Pairs == [0'a-focus_window, key_up-focus_window]) :-
    close_windows,
    new(F, frame),
    send(F, append, new(W1, recording_window('', size(200, 200)))),
    send(new(W2, focus_window('', size(200, 200))), below, W1),
    send(F, open),
    send(F, keyboard_focus, W2),
    sdl(F, mouse_motion, [x=50, y=50]),  % pointer in W1
    key_down(F, 0'a, 4, 0),
    key_up(F, 0'a, 4),
    receivers(Pairs).

:- end_tests(sdl_keyboard).

:- begin_tests(sdl_wheel, [cleanup(close_windows)]).

test(whole_notches, [R-DY == 15-0.75]) :-
    open_window(F, _),
    sdl(F, mouse_wheel, [x=50, y=50, dy=0.25]),
    sdl(F, mouse_wheel, [x=50, y=50, dy=0.5, integer_y=1]),
    events(wheel, [E]),
    get(E, rotation, R),
    get(E, dy, DY).
test(smooth, [Rs == [3.75, 7.5]]) :-
    open_window(F, W),
    send(W, smooth_scroll, @on),
    sdl(F, mouse_wheel, [x=50, y=50, dy=0.25]),
    sdl(F, mouse_wheel, [x=50, y=50, dy=0.5, integer_y=1]),
    events(wheel, Es),
    maplist([E,R]>>get(E, rotation, R), Es, Rs).
test(position, [WX-WY == MX-MY]) :-
    open_window(F, W),
    sdl(F, mouse_motion, [x=50, y=60]),
    sdl(F, mouse_wheel, [x=50, y=60, dy=1, integer_y=1]),
    events(loc_move, [M]),
    events(wheel, [E]),
    get(M, position, W, point(MX, MY)),
    get(E, position, W, point(WX, WY)).

:- end_tests(sdl_wheel).

:- begin_tests(sdl_emulation, [cleanup(close_windows)]).

test(mouse, Ids == [area_enter, loc_move]) :-
    open_window(F, _),
    sdl(F, mouse_motion, [x=50, y=50]),
    events(Ids).
test(pen_mouse_ignored, Ids == []) :-
    open_window(F, _),
    sdl(F, mouse_motion, [x=50, y=50, which= -2]),
    events(Ids).
test(touch_mouse_ignored, Ids == []) :-
    open_window(F, _),
    sdl(F, mouse_button_down, [x=50, y=50, button=1, which= -1]),
    events(Ids).

:- end_tests(sdl_emulation).

%!  pen_state(+Flags, -State) is det.
%
%   SDL_PenInputFlags for a list of flags.

pen_state(Flags, State) :-
    foldl(pen_flag, Flags, 0, State).

pen_flag(down,     S0, S) :- S is S0 \/ 0x1.
pen_flag(button_1, S0, S) :- S is S0 \/ 0x2.
pen_flag(button_2, S0, S) :- S is S0 \/ 0x4.

%!  pen(+Frame, +Type, +Fields) is det.
%
%   An event of pen 7.

pen(F, Type, Fields) :-
    sdl(F, Type, [which=7|Fields]).

kinds(Events, Kinds) :-
    maplist([E,K]>>(get(E, device, D), get(D, kind, K)), Events, Kinds).

:- begin_tests(sdl_pen, [cleanup(close_windows)]).

test(stroke, [Ids-Kinds == [area_enter, ms_left_down, obtain_focus,
                            ms_left_drag, ms_left_up, release_focus]-
                           [pen, pen, pen, pen, pen, pen]]) :-
    open_window(F, _),
    pen_state([down], Down),
    pen(F, pen_down,   [x=50, y=50, state=Down]),
    pen(F, pen_motion, [x=60, y=60, state=Down]),
    pen(F, pen_up,     [x=60, y=60]),
    findall(E, seen(E), Es),
    events(Ids),
    kinds(Es, Kinds).
test(hover, Ids == [area_enter, loc_move]) :-
    open_window(F, _),
    pen(F, pen_motion, [x=50, y=50]),
    events(Ids).
test(device_id, [Kind-Id == pen-7]) :-
    open_window(F, _),
    pen(F, pen_motion, [x=50, y=50]),
    events(loc_move, [E]),
    get(E, device, D),
    get(D, kind, Kind),
    get(D, id, Id).
test(pressure, [P == 0.5]) :-
    open_window(F, _),
    pen_state([down], Down),
    pen(F, pen_down,   [x=50, y=50, state=Down]),
    pen(F, pen_axis,   [x=50, y=50, state=Down, axis=0, value=0.5]),
    pen(F, pen_motion, [x=55, y=50, state=Down]),
    events(ms_left_drag, [E]),
    get(E, pressure, P),
    pen(F, pen_up, [x=55, y=50]).
test(tilt, [XT-YT == 30-(-15)]) :-
    open_window(F, _),
    pen(F, pen_axis,   [x=50, y=50, axis=1, value=30]),
    pen(F, pen_axis,   [x=50, y=50, axis=2, value= -15]),
    pen(F, pen_motion, [x=50, y=50]),
    events(loc_move, [E]),
    get(E, xtilt, XT),
    get(E, ytilt, YT).
test(axis_event, [Id-V == pen_axis-0.25]) :-
    open_window(F, _),
    pen(F, pen_axis, [x=50, y=50, axis=5, value=0.25]),
    events(pen, [E]),
    get(E, id, Id),
    get(E, slider, V).
test(eraser, E == @on) :-
    open_window(F, _),
    pen_state([down], Down),
    pen(F, pen_down, [x=50, y=50, state=Down, eraser=1]),
    events(ms_left_down, [Ev]),
    get(Ev, eraser, E),
    pen(F, pen_up, [x=50, y=50]).
test(barrel_button, [Ids-Bs == [ms_right_down, ms_right_up]-1]) :-
    open_window(F, _),
    pen_state([button_1], B1),
    pen(F, pen_button_down, [x=50, y=50, button=1, state=B1]),
    pen(F, pen_button_up,   [x=50, y=50, button=1]),
    events(button, Es),
    maplist([E,I]>>get(E, id, I), Es, Ids),
    Es = [Down|_],
    get(Down, barrel_buttons, Bs).
test(proximity, Ids == [pen_proximity_in, area_enter, loc_move,
                        area_exit, pen_proximity_out]) :-
    open_window(F, _),
    pen(F, pen_proximity_in, []),
    pen(F, pen_motion, [x=50, y=50]),
    pen(F, pen_proximity_out, []),
    events(Ids).
test(mouse_unaffected, D == @mouse_device) :-
    open_window(F, _),
    pen(F, pen_motion, [x=50, y=50]),
    sdl(F, mouse_motion, [x=70, y=70]),
    events(loc_move, [_, E]),
    get(E, device, D).

:- end_tests(sdl_pen).

%!  finger(+Frame, +Type, +Finger, +X, +Y) is det.
%!  finger(+Frame, +Type, +Finger, +X, +Y, +Fields) is det.
%
%   An event of Finger on touch screen 5 at (X,Y), normalised to 0..1.

finger(F, Type, Finger, X, Y) :-
    finger(F, Type, Finger, X, Y, []).
finger(F, Type, Finger, X, Y, Fields) :-
    sdl(F, Type, [touch_id=5, finger_id=Finger, x=X, y=Y|Fields]).

ids(Events, Ids) :-
    maplist([E,I]>>get(E, id, I), Events, Ids).

:- begin_tests(sdl_touch, [cleanup(close_windows)]).

test(tap, [Ids-Kinds == [area_enter, ms_left_down, obtain_focus,
                         ms_left_up, release_focus]-
                        [touch, touch, touch, touch, touch]]) :-
    open_window(F, _),
    finger(F, finger_down, 1, 0.25, 0.25),
    finger(F, finger_up,   1, 0.25, 0.25),
    findall(E, seen(E), Es),
    events(Ids),
    kinds(Es, Kinds).
test(position, [TX-TY == MX-MY]) :-
    open_window(F, W),
    get(F, area, area(_, _, FW, FH)),
    X is FW*0.25, Y is FH*0.5,
    sdl(F, mouse_motion, [x=X, y=Y]),
    finger(F, finger_down, 1, 0.25, 0.5),
    events(loc_move, [M]),
    events(ms_left_down, [T]),
    get(M, position, W, point(MX, MY)),
    get(T, position, W, point(TX, TY)),
    finger(F, finger_up, 1, 0.25, 0.5).
test(second_finger, [Ids == [touch_down, touch_motion, touch_up]]) :-
    open_window(F, _),
    finger(F, finger_down,   1, 0.25, 0.25),
    finger(F, finger_down,   2, 0.5, 0.5),
    finger(F, finger_motion, 2, 0.6, 0.5),
    finger(F, finger_up,     2, 0.6, 0.5),
    finger(F, finger_up,     1, 0.25, 0.25),
    events(touch, Es),
    ids(Es, Ids).
test(finger_detail, [Fs-P == [2, 2]-0.75]) :-
    open_window(F, _),
    finger(F, finger_down, 1, 0.25, 0.25),
    finger(F, finger_down, 2, 0.5, 0.5, [pressure=0.75]),
    finger(F, finger_up,   2, 0.5, 0.5),
    finger(F, finger_up,   1, 0.25, 0.25),
    events(touch, Es),
    maplist([E,Fi]>>get(E, finger, Fi), Es, Fs),
    Es = [Down|_],
    get(Down, pressure, P).
test(no_new_primary, Ids == [ms_left_down, touch_down, ms_left_up,
                             touch_down, touch_up, touch_up,
                             ms_left_down, ms_left_up]) :-
    open_window(F, _),
    finger(F, finger_down, 1, 0.25, 0.25),   % primary
    finger(F, finger_down, 2, 0.5, 0.5),
    finger(F, finger_up,   1, 0.25, 0.25),   % primary lifts
    finger(F, finger_down, 3, 0.6, 0.6),     % 2 still down: not primary
    finger(F, finger_up,   2, 0.5, 0.5),
    finger(F, finger_up,   3, 0.6, 0.6),
    finger(F, finger_down, 4, 0.5, 0.5),     % all lifted: primary
    finger(F, finger_up,   4, 0.5, 0.5),
    findall(I, ( seen(E), get(E, id, I),
                 memberchk(I, [ms_left_down, ms_left_up,
                               touch_down, touch_up])
               ), Ids).
test(cancel_primary, Ids == [ms_left_down, ms_left_up]) :-
    open_window(F, _),
    finger(F, finger_down,     1, 0.25, 0.25),
    finger(F, finger_canceled, 1, 0.25, 0.25),
    events(button, Es),
    ids(Es, Ids).

:- end_tests(sdl_touch).

%   SDL added pinch events in 3.4

:- begin_tests(sdl_pinch, [ condition(sdl_version(30400)),
                            cleanup(close_windows)
                          ]).

test(pinch, [Ids-Scales-Kinds == [pinch_begin, pinch_update, pinch_end]-
                                 [1, 1.25, 1]-[mouse, mouse, mouse]]) :-
    open_window(F, _),
    sdl(F, mouse_motion, [x=50, y=50]), % a pinch is where the mouse is
    sdl(F, pinch_begin,  [scale=1]),
    sdl(F, pinch_update, [scale=1.25]),
    sdl(F, pinch_end,    [scale=1]),
    events(pinch, Es),
    ids(Es, Ids),
    maplist([E,S]>>get(E, scale, S), Es, Scales),
    kinds(Es, Kinds).

:- end_tests(sdl_pinch).

sdl_version(Min) :-
    get(@pce, window_system_version, V),
    V >= Min.

%!  zoom_figure(-Window, -Figure) is det.
%
%   A window holding a figure with @pan_zoom_recogniser at (0,0),
%   holding a box.

zoom_figure(W, Fig) :-
    new(W, window('', size(200, 200))),
    new(Fig, figure),
    send(Fig, display, box(100, 100)),
    send(Fig, recogniser, @pan_zoom_recogniser),
    send(W, display, Fig, point(0, 0)),
    send(W, open).

close_zoom_windows :-
    get(@display, frames, Frames),
    chain_list(Frames, List),
    forall(( member(Fr, List),
             get(Fr, label, '')
           ),
           send(Fr, destroy)).

%!  post(+Figure, +Id, +X, +Y, +Detail) is det.
%
%   Post an event at (X,Y) of the window of Figure to Figure.

post(Fig, Id, X, Y, Detail) :-
    get(Fig, window, W),
    new(E, event(Id, W, X, Y)),
    send(E, slot, detail, Detail),
    ignore(send(E, post, Fig)).

transform(Fig, XX-TX) :-
    get(Fig, transform, T),
    get(T, xx, XX),
    get(T, tx, TX).

:- begin_tests(pan_zoom, [cleanup(close_zoom_windows)]).

test(pinch_zooms_around_pointer, T == 2-(-20)) :-
    zoom_figure(_, Fig),
    post(Fig, pinch_update, 20, 20, pinch_data(2)),
    transform(Fig, T).
test(pinch_begin_and_end, T == @nil) :-
    zoom_figure(_, Fig),
    post(Fig, pinch_begin, 20, 20, pinch_data(1)),
    post(Fig, pinch_end,   20, 20, pinch_data(1)),
    get(Fig, transform, T).
test(wheel_still_zooms, T == 0.85-3) :-
    zoom_figure(_, Fig),
    new(E, event(wheel, Fig?window, 20, 20)),
    send(E, slot, rotation, 15),
    send(E, post, Fig),
    transform(Fig, T).

:- end_tests(pan_zoom).
