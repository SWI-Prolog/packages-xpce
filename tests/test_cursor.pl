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

:- module(test_cursor, [test_cursor/0]).

/** <module> Test class cursor

Run with:

    swipl -g test_cursor -t halt packages/xpce/tests/test_cursor.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_cursor :-
    run_tests([cursor_kind, cursor_summary]).

kind(Name, Kind) :-
    get(@pce, convert, Name, cursor, Cursor),
    get(Cursor, kind, Kind).

%   predefined(+Kind, -Names)
%
%   Names are the names in @cursor_names of the given Kind.

predefined(Kind, Names) :-
    get(@cursor_names, attribute_names, Chain),
    chain_list(Chain, All),
    include([N]>>kind(N, Kind), All, Names).

:- begin_tests(cursor_kind).

test(standard, Kind == standard) :-
    kind(pointer, Kind).
test(x11, Kind == x11) :-
    kind(hand2, Kind).
test(none, Kind == standard) :-
    kind(none, Kind).
test(none_on_window, Name == none) :-
    new(W, window),
    send(W, cursor, none),
    get(W?cursor, name, Name),
    send(W, cursor, @nil),
    free(W).
test(image, Kind == image) :-
    new(C, cursor(@nil, @pce_image, point(0,0))),
    get(C, kind, Kind),
    free(C).
test(all_predefined_have_a_kind, Kinds == [standard, x11]) :-
    get(@cursor_names, attribute_names, Chain),
    chain_list(Chain, All),
    maplist(kind, All, Kinds0),
    sort(Kinds0, Kinds).
test(alias_maps_to_standard, true) :-
    predefined(x11, Aliases),
    assertion(Aliases \== []),
    predefined(standard, Standard),
    forall(member(A, Aliases),
           ( get(@cursor_names, value, A, Id),
             once(( member(S, Standard),
                    get(@cursor_names, value, S, Id)
                  ))
           )).

:- end_tests(cursor_kind).

summary(Name, Summary) :-
    get(@pce, convert, Name, cursor, Cursor),
    get(Cursor, summary, S),
    (   S == @nil
    ->  Summary = S
    ;   get(S, value, Summary)
    ).

:- begin_tests(cursor_summary).

test(standard, S == 'Pointing hand, e.g., for a link') :-
    summary(pointer, S).
test(x11_alias, S == S0) :-
    summary(pointer, S0),
    summary(hand2, S).
test(all_standard, Missing == []) :-
    predefined(standard, Names),
    include([N]>>summary(N, @nil), Names, Missing).
test(image, S0-S == @nil-'Prolog') :-
    new(C, cursor(@nil, @pce_image, point(0,0))),
    get(C, summary, S0),
    send(C, summary, 'Prolog'),
    get(C?summary, value, S),
    free(C).

:- end_tests(cursor_summary).
