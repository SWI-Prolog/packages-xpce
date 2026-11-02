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


:- module(test_scroll_bar, [test_scroll_bar/0]).

/** <module> Test class scroll_bar

Run with:

    swipl -g test_scroll_bar -t halt packages/xpce/tests/test_scroll_bar.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_scroll_bar :-
    run_tests([ scroll_bar
              ]).

%   press_top(+Arrows, -Unit, -Direction)
%
%   Press the left button at the top of a vertical scroll bar in the
%   middle of a long document, with the class variable `arrows` set to
%   Arrows.  Unit and Direction tell what the press scrolls.

press_top(Arrows, Unit, Direction) :-
    get(@pce, convert, scroll_bar, class, Class),
    get(Class, class_variable, arrows, CV),
    get(CV, value, Old),
    setup_call_cleanup(
        send(Class, class_variable_value, arrows, Arrows),
        press_top(Unit, Direction),
        send(Class, class_variable_value, arrows, Old)).

%   thumb(+MinLength, +Start, -Thumb)
%
%   Thumb is Start-Length of the thumb of a 200 pixel vertical scroll
%   bar over a document of a million lines with the view at Start, with
%   the class variable `min_thumb_length` set to MinLength.

thumb(MinLength, Start, Thumb) :-
    get(@pce, convert, scroll_bar, class, Class),
    get(Class, class_variable, min_thumb_length, CV),
    get(CV, value, Old),
    setup_call_cleanup(
        send(Class, class_variable_value, min_thumb_length, MinLength),
        thumb(Start, Thumb),
        send(Class, class_variable_value, min_thumb_length, Old)).

thumb(Start, ThumbStart-Length) :-
    new(SB, scroll_bar(@nil, vertical, @nil)),
    send(SB, height, 200),
    send(SB, bubble, 1000000, Start, 50),
    send(SB, compute),
    get(SB, slot, bubble_start, ThumbStart),
    get(SB, slot, bubble_length, Length),
    free(SB).

press_top(Unit, Direction) :-
    new(P, picture),
    send(P, display, new(SB, scroll_bar(@nil, vertical, @nil)), point(0, 0)),
    send(SB, height, 200),
    send(SB, bubble, 1000, 500, 100),
    new(Ev, event(ms_left_down, P, 5, 2, 0x10, 1000)),
    send(SB, event, Ev),
    get(SB, slot, unit, Unit),
    get(SB, slot, direction, Direction),
    send(P, destroy).

:- begin_tests(scroll_bar).

test(no_arrows_by_default, A == @off) :-
    new(SB, scroll_bar(@nil, vertical, @nil)),
    get(SB, class_variable_value, arrows, A),
    free(SB).
test(top_without_arrows_is_page_up, U-D == page-backwards) :-
    press_top(@off, U, D).
test(top_with_arrows_is_line_up, U-D == line-backwards) :-
    press_top(@on, U, D).

%   The width is a class variable in mm, which is not a whole number of
%   pixels.  The bar has whole pixels, so W - Width is an integer.

test(width_is_whole_pixels, W =:= round(W)) :-
    new(SB, scroll_bar(@nil, vertical, @nil)),
    get(SB, width, W),
    free(SB).

%   On a huge document the thumb keeps its minimal length and the
%   thumb of a view at the end ends flush with the end of the bar.

test(min_thumb_default, L =:= 2*W) :-
    new(SB, scroll_bar(@nil, vertical, @nil)),
    get(SB, width, W),
    free(SB),
    thumb(@default, 0, _-L).
test(min_thumb_class_variable, T == 0-50) :-
    thumb(50, 0, T).
test(min_thumb_at_end, T == 150-50) :-
    thumb(50, 999950, T).

:- end_tests(scroll_bar).
