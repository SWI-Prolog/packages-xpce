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

:- module(test_text_style, [test_text_style/0]).

/** <module> Test text->style

A text with a style takes its font, colour, background and underline
from the style if the style defines them.  Changing these through the
text changes the style.

Run with:

    swipl -g test_text_style -t halt packages/xpce/tests/test_text_style.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_text_style :-
    run_tests([text_style]).

styled(T, S, Attributes) :-
    Term =.. [style|Attributes],
    new(S, Term),
    new(T, text(hello)),
    send(T, style, S).

points(T, P) :-
    get(T?font, points, P).

:- begin_tests(text_style).

test(font_from_style, [P == 20]) :-
    styled(T, _, [font := font(sans, normal, 20)]),
    points(T, P).
test(own_font_without_style_font, [F == F0]) :-
    new(T0, text(hello)),
    get(T0, font, F0),
    styled(T, _, [colour := red]),
    get(T, font, F).
test(bold_italic, [W-St == bold-italic]) :-
    styled(T, _, [bold := @on, italic := @on]),
    get(T?font, weight, W),
    get(T?font, style, St).
test(colour_from_style, [C == red]) :-
    styled(T, _, [colour := red]),
    get(T?colour, name, C).
test(background_from_style, [C == yellow]) :-
    styled(T, _, [background := yellow]),
    get(T?background, name, C).
test(underline_from_style, [U == @on]) :-
    styled(T, _, [underline := @on]),
    get(T, underline, U).
test(strikethrough_from_style, [S == @on]) :-
    styled(T, _, [strikethrough := @on]),
    get(T, strikethrough, S).
test(no_strikethrough, [S == @off]) :-
    new(T, text(hello)),
    get(T, strikethrough, S).
test(texture_underline, [U == dotted]) :-
    styled(T, _, [underline := dotted]),
    get(T, underline, U).
test(set_font_changes_style, [SP-H1 == 30-true]) :-
    styled(T, S, [font := font(sans, normal, 12)]),
    get(T?area, height, H0),
    send(T, font, font(sans, normal, 30)),
    get(S?font, points, SP),
    get(T?area, height, H),
    (H > H0 -> H1 = true ; H1 = false).
test(set_colour_changes_style, [C == blue]) :-
    styled(T, S, [colour := red]),
    send(T, colour, blue),
    get(S?colour, name, C).
test(style_changed, [H1 == true]) :-
    styled(T, S, [font := font(sans, normal, 12)]),
    get(T?area, height, H0),
    send(S, font, font(sans, normal, 30)),
    send(T, style_changed),
    get(T?area, height, H),
    (H > H0 -> H1 = true ; H1 = false).
test(remove_style, [P == P0]) :-
    new(T0, text(hello)),
    points(T0, P0),
    styled(T, _, [font := font(sans, normal, 30)]),
    send(T, style, @nil),
    points(T, P).

:- end_tests(text_style).
