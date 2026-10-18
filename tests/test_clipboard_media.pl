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

:- module(test_clipboard_media, [test_clipboard_media/0]).

/** <module> Images and graphicals on the clipboard

Without a window system, SDL keeps the clipboard in the process, which
allows for a round trip.

Run with:

    swipl -g test_clipboard_media -t halt \
          packages/xpce/tests/test_clipboard_media.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_clipboard_media :-
    run_tests([ clipboard_media ]).

targets(Targets) :-
    get(@display, selection_targets, Chain),
    chain_list(Chain, Targets).

:- begin_tests(clipboard_media).

test(image, [Targets-W-H-R == ['image/png']-20-10-255]) :-
    new(I, image(@nil, 20, 10, pixmap)),
    send(I, fill, colour(red)),
    send(@display, selection, clipboard, I),
    targets(Targets),
    get(@display, selection, clipboard, image, I2),
    get(I2, size, size(W, H)),
    get(I2, pixel, 5, 5, C),
    get(C, red, R).
test(graphical, [Targets-W-H-SVG == ['image/svg+xml', 'image/png']-
                                    30-20-true]) :-
    send(@display, copy, box(30, 20)),
    targets(Targets),
    get(@display, selection, clipboard, image, I),
    get(I, size, size(W, H)),
    get(@display, selection, clipboard, svg, S),
    ( send(S, sub, '<svg') -> SVG = true ; SVG = false ).
test(text_still_works, Text == "hello") :-
    send(@display, copy, hello),
    get(@display, paste, S),
    get(S, value, V),
    atom_string(V, Text).
test(no_image, fail) :-
    send(@display, copy, hello),
    get(@display, selection, clipboard, image, _).
test(no_primary_image, fail) :-
    send(@display, selection, primary, image(@nil, 2, 2, pixmap)).

:- end_tests(clipboard_media).
