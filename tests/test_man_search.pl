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


:- module(test_man_search,
          [ test_man_search/0
          ]).

%   The manual opens frames that save their geometry.

:- use_module(xpce_test_sandbox).
:- initialization(sandbox_app_config(_), now).

:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(plunit)).
:- use_module(library(pce_manual)).

/** <module> Test the search tool of the xpce manual

The word list of the search tool searches for the word typed in the
search field.  This needs the index of the manual, which is only
available if the documentation was built.
*/

test_man_search :-
    run_tests([ man_search
              ]).

search_tool(T) :-
    manpce,
    new(T, man_search_tool).

has_words :-
    catch(search_tool(T), _, fail),
    get(T, member, wordlist, WL),
    get(WL?dict?members, size, N),
    send(T, destroy),
    N > 0.

%   type(+Tool, +Keys) is det.
%
%   Type Keys in the search field of Tool.  Keys is a list of character
%   codes and event names such as 'BS'.

type(T, Keys) :-
    search_item(T, I),
    get(I, window, W),
    forall(member(Key, Keys),
           ( new(Ev, event(Key, W)),
             send(I, typed, Ev)
           )).

search_item(T, I) :-
    get(T, member, dialog, D),
    get(D, member, search, I).

%   texts(+Tool, -Field, -Search)
%
%   Field is the text of the search field and Search the search of the
%   word list.

texts(T, Field, Search) :-
    search_item(T, I),
    get(I?value_text?string, value, Field),
    get(T, member, wordlist, WL),
    get(WL, list_browser, LB),
    get(LB, slot, search_string, S),
    (   S == @nil
    ->  Search = ''
    ;   get(S, value, Search)
    ).

:- begin_tests(man_search, [condition(has_words)]).

test(rejected_character, [Field-Search == app-app,
                          cleanup(send(T, destroy))]) :-
    %  The list rejects `x`.  It must follow the field after deleting it
    search_tool(T),
    type(T, [0'a, 0'p, 0'x, 'BS', 0'p]),
    texts(T, Field, Search).
test(last_word, [Field-Search == 'appen me'-me,
                 cleanup(send(T, destroy))]) :-
    search_tool(T),
    type(T, `appen me`),
    texts(T, Field, Search).

:- end_tests(man_search).
