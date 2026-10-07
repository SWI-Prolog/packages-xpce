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

:- module(test_cursor_item, [test_cursor_item/0]).

/** <module> Test class cursor_item

Run with:

    swipl -g test_cursor_item -t halt packages/xpce/tests/test_cursor_item.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- pce_autoload(cursor_item, library(pce_cursor_item)).

test_cursor_item :-
    run_tests([cursor_item]).

item(Item, Initial, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append,
         new(Item, cursor_item(c, Initial, message(Log, append, @arg1)))),
    send(D, open).

shown(Item, Value) :-
    get(Item, member, cursor_name, Menu),
    get(Menu, selection, Value).

:- begin_tests(cursor_item).

test(native, [V == text]) :-
    item(I, text, _),
    shown(I, V).
test(alias, [V-N == pointer-hand2]) :-
    item(I, hand2, _),
    shown(I, V),
    get(I?selection, name, N).
test(user_selection, [Names == [move]]) :-
    item(I, default, L),
    send(I, user_selection, move),
    get(L, map, @arg1?name, NC),
    chain_list(NC, Names).
test(image_cursor, [V == image_cursor]) :-
    new(Img, image(@nil, 16, 16)),
    new(C, cursor(@nil, Img, point(0,0))),
    item(I, C, _),
    shown(I, V),
    get(I, selection, C).
test(preview_cursor, [P == C]) :-
    item(I, ns_resize, _),
    get(I, selection, C),
    get(I, member, cursor_preview, Try),
    get(Try, cursor, P).

:- end_tests(cursor_item).
