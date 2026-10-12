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

:- module(test_cairo_render, [test_cairo_render/0]).

/** <module> Tests for what the Cairo drawing primitives put in pixels

Each test draws a graphical into an image and reads pixels back, so it
needs no window and runs under SDL's dummy video driver.

Run with:

    swipl -g test_cairo_render -t halt \
          packages/xpce/tests/test_cairo_render.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_cairo_render :-
    run_tests([ cairo_render
              ]).

%!  pixel_after_drawing(+Graphical, +Size, +X, +Y, -RGBA) is det.
%
%   Draw Graphical into a fresh image of Size x Size pixels and
%   return the RGBA value of the pixel at X,Y.

pixel_after_drawing(Gr, Size, X, Y, RGBA) :-
    new(I, image(@nil, Size, Size)),
    send(I, draw_in, Gr),
    get(I, pixel, X, Y, C),
    get(C, rgba, RGBA),
    free(I).

%!  uniform_gradient(+Colour, -Gradient) is det.
%
%   Gradient is a linear gradient that has Colour all along.

uniform_gradient(Colour, G) :-
    new(G, gradient(linear, point(0,0), point(20,0))),
    send(G, add_stop, 0, Colour),
    send(G, add_stop, 1, Colour).

%   A graphical that fills its area using ->draw_fill.

:- pce_begin_class(tcr_filler, graphical,
                   "Fill the area using ->draw_fill").

variable(fill, any, both, "Fill passed to ->draw_fill").

initialise(G, Fill:any) :->
    send_super(G, initialise, 0, 0, 20, 20),
    send(G, slot, fill, Fill).

'_redraw_area'(G, _A:area) :->
    get(G, area, area(X, Y, W, H)),
    get(G, fill, Fill),
    send(G, draw_fill, X, Y, W, H, Fill).

:- pce_end_class(tcr_filler).

%   A graphical that draws a filled square with a thick black outline
%   using ->draw_poly.

:- pce_begin_class(tcr_poly, graphical,
                   "Draw a filled polygon using ->draw_poly").

initialise(G) :->
    send_super(G, initialise, 0, 0, 20, 20).

'_redraw_area'(G, _A:area) :->
    send(G, graphics_state, 4, none, black),
    send(G, draw_poly,
         chain(point(2,2), point(18,2), point(18,18), point(2,18)),
         @on, colour(red)).

:- pce_end_class(tcr_poly).

filled_box(Fill, B) :-
    new(B, box(20,20)),
    send(B, pen, 0),
    send(B, fill, Fill).

:- begin_tests(cairo_render).

test(gradient_stops_are_exact, RGBA == 0xffff0000) :-
    uniform_gradient(colour(red), G),
    filled_box(G, B),
    pixel_after_drawing(B, 20, 10, 10, RGBA).

test(draw_fill_colour, RGBA == 0xffff0000) :-
    new(G, tcr_filler(colour(red))),
    pixel_after_drawing(G, 20, 10, 10, RGBA).
test(draw_fill_gradient, RGBA == 0xffff0000) :-
    uniform_gradient(colour(red), Gradient),
    new(G, tcr_filler(Gradient)),
    pixel_after_drawing(G, 20, 10, 10, RGBA).
test(figure_gradient_background, RGBA == 0xffff0000) :-
    uniform_gradient(colour(red), Gradient),
    new(F, figure),
    send(F, background, Gradient),
    send(F, display, new(B, box(20, 20))),
    send(B, pen, 0),
    pixel_after_drawing(F, 20, 10, 10, RGBA).

test(draw_poly_strokes_over_fill, Inner-Outline == 0xffff0000-0xff000000) :-
    new(G, tcr_poly),
    pixel_after_drawing(G, 20, 10, 10, Inner),
    pixel_after_drawing(G, 20, 3, 10, Outline).

:- end_tests(cairo_render).
