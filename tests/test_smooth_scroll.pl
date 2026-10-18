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

:- module(test_smooth_scroll, [test_smooth_scroll/0]).

/** <module> Wheel events of a fraction of a notch

A window with `smooth_scroll` @on receives a wheel event for every
scroll step of a trackpad, whose <-rotation is a fraction of a notch
(15).  The built-in handlers add these up: a window scrolls in whole
pixels, a list_browser in whole lines and a cycle menu in whole
notches.

Run with:

    swipl -g test_smooth_scroll -t halt \
          packages/xpce/tests/test_smooth_scroll.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_smooth_scroll :-
    run_tests([ smooth_scroll ]).

%!  wheel_event(+Window, +Rotation, -Event) is det.

wheel_event(W, Rot, E) :-
    new(E, event(wheel, W, 20, 20)),
    send(E, slot, rotation, Rot).

%!  window_wheel(+Window, +Rotation) is det.
%
%   Deliver a wheel event to Window as the window system does.

window_wheel(W, Rot) :-
    wheel_event(W, Rot, E),
    ignore(send(W, post_event, E)).

%!  scrolled_picture(-Picture) is det.
%
%   A picture with a tall graphical, scrolled down to y=500.

scrolled_picture(P) :-
    new(P, picture('', size(200, 200))),
    send(P, display, box(50, 2000)),
    send(P, open),
    send(P, scroll_to, point(0, 500)).

view_y(P, Y) :-
    get(P, visible, A),
    get(A, y, Y).

:- begin_tests(smooth_scroll).

test(default_off, Value == @off) :-
    new(P, picture),
    get(P, smooth_scroll, Value),
    send(P, free).
test(window_notch, Y == 560) :-
    scrolled_picture(P),
    window_wheel(P, -15),                % a notch is 3 lines of 20 pixels
    view_y(P, Y).
test(window_pixels, [Ys == [504, 508, 528]]) :-
    scrolled_picture(P),
    window_wheel(P, -1), view_y(P, Y1),  % 1/15 notch: 4 pixels
    window_wheel(P, -1), view_y(P, Y2),
    window_wheel(P, -5), view_y(P, Y3),  % 1/3 notch: a line
    Ys = [Y1, Y2, Y3].
test(window_fraction_of_pixel, [Ys == [500, 501]]) :-
    scrolled_picture(P),
    window_wheel(P, -0.125), view_y(P, Y1),  % 1/2 pixel
    window_wheel(P, -0.125), view_y(P, Y2),
    Ys = [Y1, Y2].
test(window_direction_resets, [Y == 500]) :-
    scrolled_picture(P),
    window_wheel(P, -0.125),            % 1/2 pixel down
    window_wheel(P, 0.125),             % 1/2 pixel up: no scrolling
    window_wheel(P, -0.125),
    view_y(P, Y).
test(list_browser_lines, [Starts == [1, 2, 2, 3]]) :-
    new(LB, list_browser(@default, 20, 10)),
    forall(between(1, 100, I), send(LB, append, I)),
    new(D, dialog),
    send(D, append, LB),
    send(D, open),
    findall(S,
            ( member(R, [-5, -5, -2.5, -2.5]),
              wheel_event(D, R, E),
              ignore(send(LB, event, E)),
              get(LB, start, S)
            ),
            Starts).
test(cycle_menu_notches, [Vs == [a, a, b]]) :-
    new(D, dialog),
    send(D, append, new(M, menu(m, cycle))),
    send_list(M, append, [a, b, c]),
    send(D, open),
    findall(V,
            ( member(R, [-5, -5, -5]),
              wheel_event(D, R, E),
              ignore(send(M, event, E)),
              get(M, selection, V)
            ),
            Vs).

:- end_tests(smooth_scroll).
