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


:- module(test_terminal_graphical,
          [ test_terminal_graphical/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> A terminal_image is an ordinary graphical

A terminal_image may share its window with other graphicals and must
only paint its own area.  The window of the SDL dummy driver paints
nothing, so we draw the terminal into an image.
*/

test_terminal_graphical :-
    run_tests([ terminal_graphical
              ]).

%   pixel_after_drawing(+X, +Y, -Name)
%
%   Draw a terminal at 10,150 in a red image and get the colour at X,Y.

pixel_after_drawing(X, Y, Name) :-
    new(I, image(@nil, 400, 300, pixmap)),
    send(I, fill, red),
    new(T, terminal_image(380, 96)),
    send(T, insert, 'one\r\ntwo\r\nthree\r\n'),
    send(I, draw_in, T, point(10, 150)),
    get(I, pixel, X, Y, C),
    get(C, name, Name),
    free(T),
    free(I).

:- begin_tests(terminal_graphical).

test(above_is_untouched, Name == '#ff0000') :-
    %  Clearing below the text used to clear upwards if the terminal is
    %  not at the top of its window
    pixel_after_drawing(20, 100, Name).
test(below_is_untouched, Name == '#ff0000') :-
    pixel_after_drawing(20, 280, Name).
test(inside_is_painted, true(Name \== '#ff0000')) :-
    pixel_after_drawing(20, 200, Name).

:- end_tests(terminal_graphical).
