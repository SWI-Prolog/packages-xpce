/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/packages/xpce/
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

:- module(test_terminal_palette,
          [ test_terminal_palette/0
          ]).
:- use_module(library(debug)).

setup_headless :-
    debugging(xpce(gui)),
    !.
setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- initialization(setup_headless, now).

:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test the palette Colours of the xpce terminal

A terminal resolves the 256 colour cube and the 24 bit colours of its
client to xpce Colour objects and holds on to them between paints.  The
Colours are interned: every terminal that resolves an RGBA value is
handed the same object, so holding it must be counted.  It was a flag
(->lock_object), and the first terminal to let go took the protection of
all the others with it: deleting a second Epilog window left the first
painting with a freed Colour.

The Colours are resolved while painting, so these tests paint.
*/

test_terminal_palette :-
    run_tests([ terminal_palette
              ]).

                /*******************************
                *            HARNESS           *
                *******************************/

%!  terminal(-Terminal, -Window) is det.
%
%   A terminal image of 1000x500 pixels in an open window.

terminal(TI, W) :-
    new(TI, terminal_image(1000, 500)),
    new(W, window('test_terminal_palette')),
    send(W, display, TI),
    send(W, size, size(1000, 500)),
    send(W, open),
    send(W, wait).

%!  paint(+Window) is det.
%
%   Let the window paint what was written to it.  Nothing resolves a
%   palette slot to a Colour until the cells using it are painted.

paint(W) :-
    dispatch(0.3),
    get(W, frame, F),
    get(F, image, Image),
    get(Image, size, _).

dispatch(Seconds) :-
    get_time(Start),
    Deadline is Start+Seconds,
    dispatch_until(Deadline).

dispatch_until(Deadline) :-
    get_time(Now),
    (   Now >= Deadline
    ->  true
    ;   pce_principal:pce_dispatch(-1, 0.05),
        dispatch_until(Deadline)
    ).

%!  paint_rgb(+Terminal, +Window, +RGB) is det.
%
%   Write text on a 24 bit background and paint it, which is what makes
%   the terminal resolve that colour.

paint_rgb(TI, W, rgb(R,G,B)) :-
    format(atom(Text), '\e[48;2;~w;~w;~wmXXXX\e[0m\n', [R,G,B]),
    send(TI, insert, Text),
    paint(W).

%!  colour_alive(+RGB) is semidet.
%
%   True when the Colour for RGB is still registered.  A Colour takes
%   its @colours entry out when it goes.  Look it up only after the
%   test has done what it is about: the lookup itself hands Prolog a
%   reference, which would keep the Colour alive.

colour_alive(rgb(R,G,B)) :-
    format(atom(Name), '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+', [R,G,B]),
    get(@colours, member, Name, _).

                /*******************************
                *             TESTS            *
                *******************************/

:- begin_tests(terminal_palette).

test(colour_survives_a_second_terminal) :-
    %  Both terminals paint the same colour, so both are handed the same
    %  Colour object.  Destroying one must not take it away from the
    %  other, which still has it in its palette and paints with it.
    RGB = rgb(12, 34, 56),
    terminal(T1, W1),
    terminal(T2, W2),
    paint_rgb(T1, W1, RGB),
    paint_rgb(T2, W2, RGB),
    send(W2, destroy),
    assertion(colour_alive(RGB)),
    paint_rgb(T1, W1, RGB),
    send(W1, destroy).

test(colour_goes_with_the_last_terminal) :-
    %  ... and the other way around: the terminal owns the Colour, so
    %  the last one to let go takes it with it.
    RGB = rgb(13, 35, 57),
    terminal(T, W),
    paint_rgb(T, W, RGB),
    send(W, destroy),
    assertion(\+ colour_alive(RGB)).

:- end_tests(terminal_palette).
