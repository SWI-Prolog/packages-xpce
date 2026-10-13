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

:- module(test_graphics_state, [test_graphics_state/0]).

/** <module> graphical ->save_graphics_state and ->restore_graphics_state

->restore_graphics_state must reset the pen, texture and colours to the
values saved by ->save_graphics_state.

Run with:

    swipl -g test_graphics_state -t halt \
          packages/xpce/tests/test_graphics_state.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_graphics_state :-
    run_tests([ graphics_state ]).

%   A graphical that draws a thick, dotted red line between saving and
%   restoring the graphics state and a line using the restored state
%   below it.

:- pce_begin_class(tgs_shape, graphical,
                   "Draw using a saved graphics state").

'_redraw_area'(G, _A:area) :->
    get(G, area, area(X, Y, W, _)),
    send(G, graphics_state, 1, none, black),
    send(G, save_graphics_state),
    send(G, graphics_state, 5, dotted, red),
    send(G, draw_line, X, Y+5, X+W, Y+5),
    send(G, restore_graphics_state),
    send(G, draw_line, X, Y+20, X+W, Y+20).

:- pce_end_class(tgs_shape).

%!  row_colours(+Image, +Y, -Colours) is det.
%
%   Colours is the sorted set of RGBA values of row Y of Image, leaving
%   out the anti-aliased ends.

row_colours(Image, Y, Colours) :-
    get(Image, size, size(W, _)),
    X1 is W-3,
    findall(RGBA,
            ( between(2, X1, X),
              get(Image, pixel, X, Y, C),
              get(C, rgba, RGBA)
            ),
            List),
    sort(List, Colours).

red(0xffff0000).

rendered(Image) :-
    new(G, tgs_shape),
    send(G, set, 0, 0, 40, 30),
    new(Image, image(@nil, 40, 30)),
    send(Image, draw_in, G),
    send(G, free).

background(Image, Background) :-
    get(Image, pixel, 0, 0, C),         % nothing is drawn here
    get(C, rgba, Background).

:- begin_tests(graphics_state).

test(saved_state_used) :-
    rendered(Image),
    red(Red),
    assertion(( between(3, 7, Y),
                row_colours(Image, Y, Colours),
                memberchk(Red, Colours)
              )),
    free(Image).
test(converted_colour_kept) :-       % red is freed after ->graphics_state
    red(Red),
    forall(between(1, 5, _),
           ( rendered(Image),
             assertion(( between(3, 7, Y),
                         row_colours(Image, Y, Colours),
                         memberchk(Red, Colours)
                       )),
             free(Image)
           )).
test(colour_and_texture_restored) :-
    rendered(Image),
    background(Image, Background),
    row_colours(Image, 20, Colours),
    red(Red),
    assertion(Colours = [_]),           % solid, so not dotted
    assertion(Colours \== [Background]), % the line is drawn
    assertion(\+ memberchk(Red, Colours)),
    free(Image).
test(pen_restored) :-
    rendered(Image),
    background(Image, Background),
    row_colours(Image, 17, Above),
    row_colours(Image, 22, Below),
    assertion(Above == [Background]),
    assertion(Below == [Background]),
    free(Image).

:- end_tests(graphics_state).
