/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(test_filter_item, [test_filter_item/0]).

/** <module> Tests for class filter_item

A filter_item calls its message with a regex or @nil as the text
changes, whether by typing or by the clear button at its right.

Run with:

    swipl -g test_filter_item -t halt \
          packages/xpce/tests/test_filter_item.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_filter_item), []).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(apply), [maplist/3]).

test_filter_item :-
    run_tests([ filter_item
              ]).

%!  filter_item(-Item, -Calls) is det.
%
%   A filter_item that appends what its message is called with to the
%   chain Calls.

filter_item(Item, Calls) :-
    new(Calls, chain),
    new(Item, filter_item(filter, message(Calls, append, @arg1))).

%!  calls(+Calls, -Filters) is det.
%
%   What the message was called with: the pattern of each regex, or
%   `nil`.

calls(Calls, Filters) :-
    chain_list(Calls, List),
    maplist(call_pattern, List, Filters).

call_pattern(@nil, nil) :- !.
call_pattern(Regex, Pattern) :-
    get(Regex, pattern, String),
    get(String, value, Pattern).

type(Item, Text) :-
    string_codes(Text, Codes),
    forall(member(C, Codes), send(Item, typed, C)).

:- begin_tests(filter_item).

test(typing_filters_on_every_key, Filters == [a, ab]) :-
    filter_item(Item, Calls),
    type(Item, "ab"),
    calls(Calls, Filters).

test(clearing_removes_the_filter, Filters == [a, nil]) :-
    filter_item(Item, Calls),
    type(Item, "a"),
    send(Item, clear),                  % what the clear button sends
    calls(Calls, Filters).

test(erasing_the_text_removes_the_filter, Filters == [a, nil]) :-
    filter_item(Item, Calls),
    type(Item, "a"),
    send(Item, typed, 8),               % backspace
    calls(Calls, Filters).

test(an_incomplete_expression_does_not_filter, Filters == [a]) :-
    filter_item(Item, Calls),
    type(Item, "a["),
    calls(Calls, Filters).

test(a_subclass_may_filter_itself, Filters == [a]) :-
    new(Item, test_own_filter_item),
    type(Item, "a"),
    get(Item, calls, Calls),
    calls(Calls, Filters).

:- end_tests(filter_item).

:- pce_begin_class(test_own_filter_item, filter_item,
                   "Filter item that overrules ->filter").

variable(calls, chain, get, "What ->filter was sent").

initialise(FI) :->
    send_super(FI, initialise),
    send(FI, slot, calls, new(chain)).

filter(FI, Filter:regex*) :->
    send(FI?calls, append, Filter).

:- pce_end_class(test_own_filter_item).
