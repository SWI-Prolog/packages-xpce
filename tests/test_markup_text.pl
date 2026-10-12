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

:- module(test_markup_text, [test_markup_text/0]).

/** <module> Tests for class markup_text and graphical->draw_markup

Run with:

    swipl -g test_markup_text -t halt \
          packages/xpce/tests/test_markup_text.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_markup_text :-
    run_tests([ markup_text
              ]).

red(0xffff0000).
white(0xffffffff).

%   Markup for a solid red block: spaces with a red background.

red_block("<span background='#ff0000'>      </span>").

%!  pixel(+Graphical, +W, +H, +X, +Y, -RGBA) is det.

pixel(Gr, W, H, X, Y, RGBA) :-
    new(I, image(@nil, W, H)),
    send(I, draw_in, Gr),
    get(I, pixel, X, Y, C),
    get(C, rgba, RGBA),
    free(I).

%!  pixels(+Graphical, +W, +H, -Set) is det.
%
%   Set is the set of all RGBA values in the drawing.

pixels(Gr, W, H, Set) :-
    new(I, image(@nil, W, H)),
    send(I, draw_in, Gr),
    W1 is W-1, H1 is H-1,
    findall(RGBA, ( between(0, W1, X), between(0, H1, Y),
                    get(I, pixel, X, Y, C), get(C, rgba, RGBA)
                  ), List),
    sort(List, Set),
    free(I).

size(Gr, W-H) :-
    get(Gr?area, width, W),
    get(Gr?area, height, H).

%   A graphical that draws a red block using ->draw_markup.

:- pce_begin_class(tmt_drawer, graphical,
                   "Draw markup using ->draw_markup").

initialise(G) :->
    send_super(G, initialise, 0, 0, 100, 40).

'_redraw_area'(G, _A:area) :->
    red_block(M),
    get(G?area, x, X), get(G?area, y, Y),
    send(G, draw_markup, M, font(sans, normal, 12), X, Y).

:- pce_end_class(tmt_drawer).

:- begin_tests(markup_text).

test(plain_text, Text == "Some bold text") :-
    new(T, markup_text("Some <b>bold</b> text")),
    get(T, text, S),
    get(S, value, A),
    atom_string(A, Text).
test(invalid_markup_is_rejected, fail) :-
    new(_, markup_text("bad <b>markup")).
test(invalid_markup_keeps_the_old, Markup == "<b>ok</b>") :-
    new(T, markup_text("<b>ok</b>")),
    \+ send(T, markup, "bad <b>markup"),
    get(T, markup, M),
    get(M, value, A),
    atom_string(A, Markup).
test(draws_markup_colours, RGBA == Red) :-
    red(Red),
    red_block(M),
    new(T, markup_text(M)),
    pixel(T, 100, 40, 5, 5, RGBA).
test(uses_graphical_colour, true(memberchk(0xff0000ff, Set))) :-
    new(T, markup_text("<span size='xx-large'><b>MW</b></span>")),
    send(T, colour, blue),
    size(T, W-H),
    pixels(T, W, H, Set).
test(wrapping_makes_it_higher, true(WH > H)) :-
    Text = "one two three four five six seven eight",
    new(T1, markup_text(Text)),
    new(T2, markup_text(Text, 60)),
    size(T1, _-H),
    size(T2, _-WH).
test(wrapped_width, W == 300) :-
    new(T, markup_text("short", 300)),
    size(T, W-_).
test(right_alignment, [Left, Right] == [White, Red]) :-
    red(Red), white(White),
    red_block(M),
    new(T, markup_text(M, 200)),
    send(T, alignment, right),
    pixel(T, 200, 40, 5, 5, Left),
    pixel(T, 200, 40, 195, 5, Right).
test(ellipsize_keeps_one_line, H == H1) :-
    Text = "one two three four five six seven eight",
    new(T1, markup_text("one")),
    new(T, markup_text(Text, 60)),
    send(T, ellipsize, end),
    size(T1, _-H1),
    size(T, _-H).
test(line_spacing, true(H2 > H1)) :-
    Text = "line one\nline two",
    new(T1, markup_text(Text)),
    new(T2, markup_text(Text)),
    send(T2, line_spacing, 2),
    size(T1, _-H1),
    size(T2, _-H2).
test(geometry_sets_width, W == 120) :-
    new(T, markup_text("text")),
    send(T, set, 10, 10, 120),
    get(T, width, W).
test(draw_markup, RGBA == Red) :-
    red(Red),
    new(G, tmt_drawer),
    pixel(G, 100, 40, 5, 5, RGBA).

:- end_tests(markup_text).
