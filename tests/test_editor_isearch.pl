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

:- module(test_editor_isearch, [test_editor_isearch/0]).

/** <module> Tests for incremental search in an editor

Run with:

    swipl -g test_editor_isearch -t halt \
          packages/xpce/tests/test_editor_isearch.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).

test_editor_isearch :-
    run_tests([ editor_isearch
              ]).

:- pce_begin_class(isearch_test_editor, editor,
                   "Editor that records its warnings").

:- dynamic warning/1.

report(_E, Kind:name, Fmt:[char_array], _Args:any ...) :->
    (   Kind == warning
    ->  failing_report(Fmt, Report),
        assertz(warning(Report))
    ;   true
    ).

:- pce_end_class(isearch_test_editor).

%!  isearch(+Text, +Keys, -Report) is det.
%
%   Run an incremental search for Keys in an editor holding Text.
%   Report is the last warning: failing(Text, Matched, Failed),
%   where Matched and Failed are the contents of the two spans.

isearch(Text, Keys, Report) :-
    retractall(warning(_)),
    new(E, isearch_test_editor),
    send(E, contents, Text),
    send(E, caret, 0),
    send(E, isearch_forward),
    new(W, window),
    forall(member(K, Keys), type_key(E, W, K)),
    free(E),
    free(W),
    findall(R, warning(R), Warnings),
    last(Warnings, Report).

%   type_key(+Editor, +Window, +Key)
%
%   Type Key, a character code or the name of a function key, as f3.
%   A function key is typed as an event, as only that maps to the key
%   name, e.g. `<f3>`, of the key binding.

type_key(E, _, Key) :-
    integer(Key),
    !,
    send(E, typed, Key).
type_key(E, W, Key) :-
    new(Ev, event(Key, W)),
    send(E, typed, Ev),
    free(Ev).

failing_report(Fmt, failing(Plain, Matched, Failed)) :-
    get(Fmt, class_name, markup),
    get(Fmt, text, PlainS),
    get(PlainS, value, Plain),
    get(Fmt, value, Markup),
    atomic_list_concat(Parts, '</span>', Markup),
    Parts = [P1, P2, ''],
    span_text(P1, Matched),
    span_text(P2, Failed).

span_text(Part, Text) :-
    sub_atom(Part, _, 2, A, '">'),
    sub_atom(Part, _, A, 0, Text),
    \+ sub_atom(Text, _, _, _, '">'),
    !.

:- begin_tests(editor_isearch).

test(partial, R == failing('Failing ISearch: barz', bar, z)) :-
    isearch('foo(X) :- bar(X).', `barz`, R).
test(no_match, R == failing('Failing ISearch: q', '', q)) :-
    isearch('foo(X) :- bar(X).', `q`, R).
test(stays_failing, R == failing('Failing ISearch: barzy', bar, zy)) :-
    isearch('foo(X) :- bar(X).', `barzy`, R).
test(escaped, R == failing('Failing ISearch: a<c', 'a&lt;', c)) :-
    isearch('a<b', `a<c`, R).
% Searching again finds no next hit.  F3 searches again in all key
% binding styles; Control-S saves in the `cua` style.

test(no_next, R == failing('Failing ISearch: bar', bar, '')) :-
    isearch('foo(X) :- bar(X).', [0'b, 0'a, 0'r, f3], R).

:- end_tests(editor_isearch).
