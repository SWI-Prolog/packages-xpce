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
:- module(test_plot_axis, [test_plot_axis/0]).

/** <module> Tests for library(plot/axis)

Run with:

    swipl -g test_plot_axis -t halt packages/xpce/tests/test_plot_axis.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(plot/plotter)).

test_plot_axis :-
    run_tests([ plot_axis
              ]).

%   A plotter with a 300 pixel X axis and a 200 pixel Y axis, both
%   drawing their support lines across the other.

plot(P) :-
    new(P, plotter),
    send(P, axis, plot_axis(x, 0, 100, 10, 300, point(50, 250))),
    send(P, axis, plot_axis(y, 0, 100, 10, 200, point(50, 250))),
    send(P, compute).

%   The lines of an axis run across the range of the other, so they
%   follow when the other gets shorter.

:- begin_tests(plot_axis).

test(lines_follow_other_length, true(H < 100)) :-
    plot(P),
    get(P, axis, y, Y),
    send(Y, length, 50),
    send(P, compute),
    get(P, height, H),
    free(P).
test(lines_follow_other_origin, Tops == [100]) :-
    plot(P),
    get(P, axis, y, Y),
    send(Y, origin, point(50, 300)),
    send(P, compute),
    findall(Top, grid_line_top(P, Top), Tops0),
    sort(Tops0, Tops),
    free(P).

%   Top is the upper end of a dotted line across the Y range.

grid_line_top(P, Top) :-
    get(P, graphicals, Grs),
    chain_list(Grs, List),
    member(L, List),
    get(L, class_name, line),
    get(L, texture, dotted),
    get(L, start_x, X),
    get(L, end_x, X),
    get(L, start_y, Y0),
    get(L, end_y, Y1),
    Top is min(Y0, Y1).

:- end_tests(plot_axis).
