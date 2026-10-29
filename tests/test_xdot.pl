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
:- module(test_xdot, [test_xdot/0]).
:- encoding(utf8).

/** <module> Test library(xdot)

Run with:

    swipl -g test_xdot -t halt packages/xpce/tests/test_xdot.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(xdot)).

test_xdot :-
    run_tests([ xdot_labels
              ]).

:- begin_tests(xdot_labels, [condition(has_dot)]).

%   Graphviz places an edge label beside the centre line of the edge,
%   regardless of the width of the line and its arrows.  The label is
%   drawn last, on a rounded, translucent box of the background, so
%   they cannot hide it.

test(label_on_top, Last == "10×") :-
    graph("digraph { a -> b [label=\"10×\" penwidth=5] }", W, E),
    get(E?graphicals, tail, T),
    get(T, string, S),
    get(S, value, Last0),
    atom_string(Last0, Last),
    free(W).
test(label_background, [ RGB == WRGB, Alpha == 160, true(Radius > 0),
                         Behind == true, Covers == true ]) :-
    graph("digraph { a -> b [label=\"10×\"] }", W, E),
    label(E, T),
    label_box(E, B),
    get(B, fill, BG),
    rgb(BG, RGB),
    get(BG, alpha, Alpha),
    get(B, radius, Radius),
    get(W, background, WBG),
    rgb(WBG, WRGB),
    (   get(E?graphicals, index, B, IB),
        get(E?graphicals, index, T, IT),
        IB =:= IT - 1
    ->  Behind = true
    ;   Behind = false
    ),
    (   get(B?area, union, T?area, U),
        send(U, equal, B?area)
    ->  Covers = true
    ;   Covers = false
    ),
    free(W).
test(graph_background, RGB == YRGB) :-
    graph("digraph { bgcolor=yellow; a -> b [label=\"10×\"] }", W, E),
    label_box(E, B),
    get(B, fill, BG),
    rgb(BG, RGB),
    get(@pce, convert, yellow, colour, Y),
    rgb(Y, YRGB),
    free(W).
test(transparent_labels, fail) :-
    get(class(xdot), class_variable, edge_label_alpha, CV),
    get(CV, value, Old),
    setup_call_cleanup(
        send(CV, value, 0),
        ( graph("digraph { a -> b [label=\"10×\"] }", W, E),
          call_cleanup(label_box(E, _), free(W))
        ),
        send(CV, value, Old)).

:- end_tests(xdot_labels).

has_dot :-
    absolute_file_name(path(dot), _, [access(execute), file_errors(fail)]).

%!  graph(+Dot, -Window, -Edge) is det.
%
%   Show the graph Dot in a new xdot_window and return its only edge.

graph(Dot, W, E) :-
    new(W, xdot_window),
    get(W, xdot, X),
    send(X, load, string(Dot)),
    get(X?graphicals, find, message(@arg1, instance_of, xdot_edge), E).

label(E, T) :-
    get(E?graphicals, find, message(@arg1, instance_of, text), T).

label_box(E, B) :-
    get(E?graphicals, find, message(@arg1, instance_of, box), B).

rgb(Colour, rgb(R,G,B)) :-
    get(Colour, red, R),
    get(Colour, green, G),
    get(Colour, blue, B).
