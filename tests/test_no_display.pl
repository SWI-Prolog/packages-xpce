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

:- module(test_no_display,
          [ test_no_display/0
          ]).
%  Before library(pce): pce.pl passes this flag on to SDL, and SDL picks
%  its video driver when it initialises, which happens as xpce loads.
:- set_prolog_flag('SDL_VIDEODRIVER', 'no-such-video-driver').
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test xpce on a machine that has no display

Loading xpce where the window system cannot be initialised -- a headless
machine, or no DISPLAY -- leaves it without a display object.  Everything
that needs one must then say so and fail, rather than read the display
that is not there: that crashed the process.  Asking for a video driver
that does not exist puts SDL_Init() in the same position on any machine,
which is how these tests reach it.

Drawing that does not need a window still works headless: an image is an
offscreen surface and text is measured by Pango, so both go on without a
display.  The tests below hold xpce to that as well.
*/

test_no_display :-
    run_tests([ no_display
              ]).

%!  fails_cleanly(:Goal) is semidet.
%
%   True when Goal fails or raises an error.  Which of the two is not the
%   point: the point is that it returns at all.

fails_cleanly(Goal) :-
    \+ catch(Goal, _, fail).

:- begin_tests(no_display).

test(no_display_object) :-
    assertion(\+ object(@display)).

test(frame) :-
    assertion(fails_cleanly(new(_, frame(no_display)))).

test(window) :-
    assertion(fails_cleanly(( new(W, window(no_display)),
                              send(W, open)
                            ))).

test(dialog) :-
    assertion(fails_cleanly(( new(D, dialog(no_display)),
                              send(D, open)
                            ))).

test(picture) :-
    assertion(fails_cleanly(( new(P, picture(no_display)),
                              send(P, open)
                            ))).

test(terminal) :-
    assertion(fails_cleanly(( new(TI, terminal_image(200, 100)),
                              new(W, window(no_display)),
                              send(W, display, TI),
                              send(W, open)
                            ))).

%  An image is an offscreen cairo surface, so it does not need a display
%  and must not start needing one.

test(image_can_be_drawn) :-
    new(I, image(@nil, 10, 10)),
    send(I, fill, colour(red)),
    get(I, pixel(5, 5), C),
    get(C, red, R),
    assertion(R == 255),
    free(I).

%  Pango measures text, not the window system.

test(font_has_metrics) :-
    get(font(mono, normal, 12), width, W),
    assertion(integer(W)),
    assertion(W > 0).

:- end_tests(no_display).
