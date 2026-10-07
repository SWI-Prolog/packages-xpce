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

:- module(test_text_cursor, [test_text_cursor/0]).

/** <module> Test class text_cursor

Run with:

    swipl -g test_text_cursor -t halt packages/xpce/tests/test_text_cursor.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_text_cursor :-
    run_tests([ text_cursor,
                text_cursor_blink
              ]).

%   area(+Style, -Area)
%
%   Area of a caret with Style for the character box at (10,20) of
%   8x16 pixels with the baseline 12 pixels below the top.

area(Style, area(X,Y,W,H)) :-
    new(C, text_cursor),
    send(C, style, Style),
    send(C, set, 10, 20, 8, 16, 12),
    get(C, area, area(X,Y,W,H)),
    free(C).

%   caret_width(+Text, +Caret, -Width)
%
%   Width of a block caret at Caret in an editor holding Text in a
%   proportional font.

caret_width(Text, Caret, W) :-
    new(V, view),
    send(V, open),
    get(V, editor, E),
    send(E, font, normal),              % ->font sets the style
    send(E?text_cursor, style, block),
    send(V, contents, Text),
    send(V, caret, Caret),
    send(V, compute),
    get(E?text_cursor, width, W),
    send(V, destroy).

:- begin_tests(text_cursor).

test(bar, A == area(9, 20, 2, 16)) :-
    area(bar, A).
test(block, A == area(10, 20, 8, 16)) :-
    area(block, A).
test(underline, A == area(10, 33, 8, 2)) :-
    area(underline, A).
test(xpce, A == area(4.5, 31, 11, 11)) :-
    area(xpce, A).
test(image_needs_an_image, error(_)) :-
    new(C, text_cursor),
    send(C, style, image).
test(class_variable_has_no_image, [Image, Block] == [false, true]) :-
    get(class(text_cursor), class_variable, fixed_font_style, CV),
    get(CV, type, Type),
    ( send(Type, validate, image) -> Image = true ; Image = false ),
    ( send(Type, validate, block) -> Block = true ; Block = false ).
test(block_has_the_width_of_the_character, true(WW > WI)) :-
    caret_width("iW", 0, WI),
    caret_width("iW", 1, WW).
test(block_at_the_end_of_a_line, true(W > 0)) :-
    caret_width("i\nW", 1, W).

test(xpce_is_the_default, [Fixed, Proportional] == [xpce, xpce]) :-
    program_default(fixed_font_style, Fixed),
    program_default(proportional_font_style, Proportional).
test(style_follows_the_font, [Fixed, Proportional] == [block, bar],
     [ setup(set_cvs([fixed_font_style-block, proportional_font_style-bar],
                     Old)),
       cleanup(set_cvs(Old, _))
     ]) :-
    new(C, text_cursor(font(mono, normal, 12))),
    get(C, style, Fixed),
    send(C, font, font(sans, normal, 12)),
    get(C, style, Proportional),
    free(C).

:- end_tests(text_cursor).

:- begin_tests(text_cursor_blink,
               [ setup(set_cvs([blink-(@on), blink_interval-100000,
                                blink_timeout-1], Old)),
                 cleanup(set_cvs(Old, _))
               ]).

test(active_caret_blinks, [Owner, Status] == [C, repeat]) :-
    caret(V, C),
    send(C, active, @on),
    get(@caret_blink_owner, head, Owner),
    get(@caret_blink_timer, status, Status),
    send(V, destroy).
test(inactive_caret_does_not, [Empty, Status] == [true, idle]) :-
    caret(V, C),
    send(C, active, @on),
    send(C, active, @off),
    ( send(@caret_blink_owner, empty) -> Empty = true ; Empty = false ),
    get(@caret_blink_timer, status, Status),
    send(V, destroy).
test(stops_after_the_timeout, [Before, After] == [repeat, idle]) :-
    caret(V, C),
    send(C, active, @on),
    send(@caret_blinker, blink),        % hide
    get(@caret_blink_timer, status, Before),
    send(@caret_blinker, blink),        % show; 2*100000ms >= 1s
    get(@caret_blink_timer, status, After),
    send(V, destroy).
test(typing_restarts_blinking, Status == repeat) :-
    caret(V, C),
    send(C, active, @on),
    send(@caret_blinker, blink),
    send(@caret_blinker, blink),        % timed out
    send(V, caret, 2),
    send(V, compute),
    get(@caret_blink_timer, status, Status),
    send(V, destroy).
test(destroyed_owner, Status == idle) :-
    caret(V, C),
    send(C, active, @on),
    send(V, destroy),
    send(@caret_blinker, blink),
    get(@caret_blink_timer, status, Status).

:- end_tests(text_cursor_blink).

%   caret(-View, -TextCursor)
%
%   An opened view with some text and its caret.

caret(V, C) :-
    new(V, view),
    send(V, contents, "Hello world"),
    send(V, open),
    send(V, compute),
    get(V?editor, text_cursor, C).

program_default(Name, Value) :-
    get(class(text_cursor), class_variable, Name, CV),
    get(CV, default, Default),
    (   atom(Default)
    ->  Value = Default
    ;   get(Default, value, Value)
    ).

%   set_cvs(+NameValues, -OldNameValues)
%
%   Set class variables of text_cursor, returning the old values.

set_cvs(NameValues, Old) :-
    maplist(set_cv, NameValues, Old).

set_cv(Name-Value, Name-Old) :-
    get(class(text_cursor), class_variable, Name, CV),
    get(CV, value, Old),
    send(CV, value, Value).
