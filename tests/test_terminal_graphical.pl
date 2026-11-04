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
    run_tests([ terminal_graphical,
                terminal_damage
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

%       Painting a part of the terminal must not make it forget the
%       changes of the lines outside that part.  The terminal turns its
%       changes into damage of the window when it has handled a chunk of
%       output.  While handling it, ->prompt_mark calls Prolog, which
%       releases the xpce lock, so the terminal may be painted before
%       its changes are damage.  The prompt was then not painted.
%
%       A terminal_image, unlike the window of the SDL dummy driver,
%       paints when drawn into an image.  An image of one pixel high
%       paints a part of it.

:- pce_begin_class(test_partial_paint_terminal, terminal_image,
                   "Paints a part of itself when the input starts").

prompt_mark(T, Kind:{prompt,input,output,end}, Cont:[bool]) :->
    send_super(T, prompt_mark, Kind, Cont),
    (   Kind == input
    ->  new(I, image(@nil, 380, 1, pixmap)),
        send(I, draw_in, T),
        free(I)
    ;   true
    ).

:- pce_end_class(test_partial_paint_terminal).

%!  damage_window(-Window, -Terminal) is det.
%
%   A window with a terminal that shows two lines and has nothing to
%   repaint.  The window of the dummy driver is not displayed: say it
%   is, or changes do not reach it.

damage_window(W, T) :-
    new(W, window('', size(400, 200))),
    new(T, test_partial_paint_terminal(380, 150)),
    send(W, display, T, point(0, 0)),
    send(W, open),
    send(W, displayed, @on),
    send(W, compute),
    send(T, insert, 'one\r\ntwo\r\n'),
    send(W, flush).

%!  damage(+Window, -Damage) is det.
%
%   Damage is y(Top, Bottom), the vertical extent of the pending damage
%   of Window in pixels, or `none`.

damage(W, Damage) :-
    (   get(W, changes_area, A)
    ->  get(A, y, Y), get(A, height, H),
        Bottom is Y+H,
        Damage = y(Y, Bottom)
    ;   Damage = none
    ).

%!  row(+Terminal, +Row, -Top, -Bottom) is det.
%
%   Pixels of text row Row.

row(T, Row, Top, Bottom) :-
    get(T?font, height, H),
    Top is Row*H,
    Bottom is Top+H.

:- begin_tests(terminal_damage, [cleanup(free_damage_windows)]).

free_damage_windows :-
    forall(( get(@display, frames, Fs), chain_list(Fs, L), member(F, L),
             get(F, member, window, W),
             get(W, member, test_partial_paint_terminal, _)
           ),
           free(F)).

test(prompt_painted_after_partial_paint,
     [ Row == 2, true(Damage = y(Y0, Y1)), true(Y0 =< Top),
       true(Y1 >= Bottom) ]) :-
    damage_window(W, T),
    send(T, insert, '\e]133;A\e\\PROMPT> \e]133;B\e\\'),
    get(T, cursor_position, P), get(P, y, Row),
    row(T, Row, Top, Bottom),
    damage(W, Damage).
test(full_paint_forgets_changes,
     [ true(Damage = y(Y0, Y1)), true(Y1-Y0 < 2*(Bottom-Top)) ]) :-
    %  A paint of all of the terminal must forget all its changes, or
    %  every change after it repaints all of the terminal.  The window
    %  of the dummy driver does not paint, so we paint into an image.
    damage_window(W, T),
    forall(between(1, 10, I), send(T, insert, string('line %d\r\n', I))),
    new(I, image(@nil, 380, 150, pixmap)),
    send(I, draw_in, T),
    free(I),
    send(W, flush),
    send(T, insert, x),
    row(T, 0, Top, Bottom),
    damage(W, Damage).

:- end_tests(terminal_damage).
