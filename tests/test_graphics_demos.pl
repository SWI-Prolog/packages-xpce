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

:- module(test_graphics_demos, [test_graphics_demos/0]).

/** <module> The graphics demos load and build their windows

Each demo is started and its frame destroyed.  This guards the demos of
paths, paint, markup text, compositing and the canvas against changes
in the features they show.

Run with:

    swipl -g test_graphics_demos -t halt \
          packages/xpce/tests/test_graphics_demos.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_graphics_demos :-
    run_tests([ graphics_demos
              ]).

%!  run_demo(+File, +Goal) is semidet.
%
%   Load the demo File, run Goal and destroy the frame it opened.

run_demo(File, Goal) :-
    use_module(demo(File)),
    get(@display, frames, Frames0),
    chain_list(Frames0, Before),
    call(Goal),
    get(@display, frames, Frames),
    chain_list(Frames, After),
    subtract(After, Before, New),
    New \== [],
    forall(member(F, New), send(F, destroy)).

:- begin_tests(graphics_demos).

test(paths)   :- run_demo(paths, paths_demo:paths_demo).
test(paint)   :- run_demo(paint, paint_demo:paint_demo).
test(compose) :- run_demo(compose, compose_demo:compose_demo).
test(canvas)  :- run_demo(canvas_demo, canvas_demo:canvas_demo).
test(blend_menu) :-
    use_module(demo(compose)),
    compose_demo:compose_demo,
    get(@display, frames, Frames),
    get(Frames, tail, F),
    get(F, member, picture, P),
    compose_demo:set_blend(P, screen),
    get(P?graphicals, find, @arg1?name == blend_figure, Fig),
    get(Fig, blend, Blend),
    send(F, destroy),
    assertion(Blend == screen).

:- end_tests(graphics_demos).
