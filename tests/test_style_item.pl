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

:- module(test_style_item, [test_style_item/0]).

/** <module> Test class style_item and the style editor

Run with:

    swipl -g test_style_item -t halt packages/xpce/tests/test_style_item.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_style_item)).

test_style_item :-
    run_tests([style_item]).

item(Item, Initial, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append,
         new(Item, style_item(s, Initial, message(Log, append, @arg1)))),
    send(D, open).

editor(Style, E) :-
    new(E, style_editor(Style, @nil)).

:- begin_tests(style_item).

test(sample_uses_style, [C == red]) :-
    item(I, style(colour := red), _),
    get(I, member, style_sample, S),
    get(S, member, text, T),
    get(T?colour, name, C).
test(edit_creates_new_style, [true(New \== Old), It-Bold == @on - @on]) :-
    item(I, style(bold := @on), _),
    get(I, selection, Old),
    new(E, style_editor(Old, message(I, user_selection, @arg1))),
    get(E, item, italic, Italic),
    send(Italic, selection, @on),
    send(E, ok),
    get(I, selection, New),
    get(New, italic, It),
    get(New, bold, Bold).
test(message, [N == 1]) :-
    item(I, new(style), Log),
    send(I, user_selection, style(bold := @on)),
    get(Log, size, N).
test(default_tick, [Colour == @default]) :-
    editor(style(colour := red), E),
    get(E, item, colour_default, Tick),
    send(Tick, selection, @on),
    get(E, current_style, S),
    get(S, colour, Colour).
test(keep_line_colour, [U == C]) :-
    new(C, colour(blue)),
    editor(style(underline := C), E),
    get(E, current_style, S),
    get(S, underline, U).
test(line_off, [U == @off]) :-
    editor(style(underline := @on), E),
    get(E, item, underline, M),
    send(M, selection, @off),
    get(E, current_style, S),
    get(S, underline, U).
test(keep_elevation, [B == E]) :-
    new(E, elevation(@nil, 1)),
    editor(style(background := E), Ed),
    get(Ed, current_style, S),
    get(S, background, B).
test(style_term, [T == style(bold := @on, left_margin := 4)]) :-
    style_term(style(bold := @on, left_margin := 4), T).

test(no_item_for_margin, [fail]) :-
    editor(new(style), E),
    get(E, item, left_margin, _).
test(keeps_unedited, [M-G == 4 - @on]) :-
    editor(style(left_margin := 4, grey := @on), E),
    get(E, current_style, S),
    get(S, left_margin, M),
    get(S, grey, G).

test(line_texture, [U == dashed]) :-
    editor(style(underline := dashed), E),
    get(E, current_style, S),
    get(S, underline, U).
test(line_item_values, [Vs == [@off, @on, dotted, red]]) :-
    maplist(line_value, [@off, @on, dotted, colour(red)], Vs).
test(line_item_no_default, [fail]) :-
    new(I, line_decoration_item(u, @off)),
    get(I, member, kind, M),
    get(M, member, default, _).
test(line_item_colour_active, [A0-A1 == @off - @on]) :-
    new(I, line_decoration_item(u, @on)),
    get(I, member, colour, CI),
    get(CI, active, A0),
    send(I, selection, colour(blue)),
    get(CI, active, A1).

line_value(V0, V) :-
    new(I, line_decoration_item(u, V0)),
    get(I, selection, V1),
    (   send(V1, instance_of, colour)
    ->  get(V1, name, V)
    ;   V = V1
    ).

test(line_item_kinds, [Kinds == [default,off,on,dotted,dashed,dashdot,
                                  dashdotted,longdash,colour]]) :-
    new(I, line_decoration_item(u, @off, @default, @on)),
    get(I, member, kind, M),
    get(M?members, map, @arg1?value, KindChain),
    chain_list(KindChain, Kinds).

:- end_tests(style_item).
