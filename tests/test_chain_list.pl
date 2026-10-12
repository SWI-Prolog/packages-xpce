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

:- module(test_chain_list, [test_chain_list/0]).

/** <module> Test chain_list/2 and member_chain/2

Both walk a snapshot of the chain, so they are not affected by changes
to the chain, by elements being freed, or by other threads.

Run with:

    swipl -g test_chain_list -t halt packages/xpce/tests/test_chain_list.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- if(current_prolog_flag(threads, true)).
:- use_module(library(thread)).
:- endif.

test_chain_list :-
    run_tests([ chain_list,
                member_chain,
                chain_snapshot_threads
              ]).

:- begin_tests(chain_list).

test(elements, L == [a, 1, b]) :-
    new(C, chain(a, 1, b)),
    chain_list(C, L).
test(empty, L == []) :-
    new(C, chain),
    chain_list(C, L).
test(nil, L == []) :-
    chain_list(@nil, L).
test(term, L == [a, b]) :-
    chain_list(chain(a, b), L).
test(objects, Classes == [point, size]) :-
    new(C, chain(point(1,2), size(3,4))),
    chain_list(C, L),
    maplist([O,Cl]>>get(O, class_name, Cl), L, Classes).
test(from_list, L == [x, y]) :-
    chain_list(C, [x, y]),
    chain_list(C, L).
test(current_untouched, Cur == b) :-
    new(C, chain(a, b, c)),
    send(C, current, b),
    chain_list(C, _),
    get(C, current, Cur).
test(not_a_chain, fail) :-
    chain_list(point(1,2), _).
test(function, L == [a, b]) :-
    new(O, chain(chain(a, b))),
    chain_list(O?head, L).
test(code_references, After == Before) :-
    new(P, point),
    new(C, chain(P)),
    get(P, code_references, Before),
    chain_list(C, _),
    get(P, code_references, After).

:- end_tests(chain_list).

:- begin_tests(member_chain).

test(all, L == [a, 1, b]) :-
    new(C, chain(a, 1, b)),
    findall(X, member_chain(X, C), L).
test(last_is_deterministic, true) :-
    new(C, chain(a, b)),
    member_chain(b, C).
test(bound, true) :-
    new(C, chain(a, b, c)),
    member_chain(b, C),
    !.
test(not_member, fail) :-
    new(C, chain(a, b)),
    member_chain(c, C).
test(nil, fail) :-
    member_chain(_, @nil).
test(empty, fail) :-
    new(C, chain),
    member_chain(_, C).
test(not_a_chain, fail) :-
    member_chain(_, point(1,2)).
test(function, L == [a, b]) :-
    new(O, chain(chain(a, b))),
    findall(X, member_chain(X, O?head), L).
test(modified, L == [a, b, c]) :-
    new(C, chain(a, b, c)),
    findall(X, ( member_chain(X, C),
                 ignore(send(C, delete, c)),
                 send(C, append, d)
               ), L).
test(freed_skipped, L == [P1]) :-
    new(P1, point(1,1)),
    new(P2, point(2,2)),
    new(C, chain(P1, P2)),
    findall(X, ( member_chain(X, C),
                 ( X == P1 -> free(P2) ; true )
               ), L).
test(code_references_after_cut, After == Before) :-
    new(P, point),
    new(C, chain(P, a, b)),
    get(P, code_references, Before),
    once(member_chain(_, C)),
    get(P, code_references, After).
test(code_references_after_exception, After == Before) :-
    new(P, point),
    new(C, chain(P, a, b)),
    get(P, code_references, Before),
    catch(( member_chain(X, C), X == a, throw(stop) ), stop, true),
    get(P, code_references, After).
test(code_references_after_failure, After == Before) :-
    new(P, point),
    new(C, chain(P, a, b)),
    get(P, code_references, Before),
    \+ ( member_chain(X, C), X == c ),
    get(P, code_references, After).

:- end_tests(member_chain).

%   Threads walk a chain that a writer modifies: it appends the next
%   integer and deletes the first.  Each walk must see a consistent
%   snapshot: increasing integers without gaps.

consecutive([]).
consecutive([_]) :- !.
consecutive([A,B|T]) :-
    B =:= A+1,
    consecutive([B|T]).

writer(C, N, Max) :-
    (   N > Max
    ->  true
    ;   send(C, append, N),
        send(C, delete_head),
        N1 is N+1,
        writer(C, N1, Max)
    ).

walker(C, How, Bad) :-
    walker(C, How, 0, 300, Bad).

walker(_, _, Bad, 0, Bad) :- !.
walker(C, How, Bad0, N, Bad) :-
    walk(How, C, L),
    (   consecutive(L)
    ->  Bad1 = Bad0
    ;   Bad1 is Bad0+1
    ),
    N1 is N-1,
    walker(C, How, Bad1, N1, Bad).

walk(chain_list, C, L) :-
    chain_list(C, L).
walk(member_chain, C, L) :-
    findall(X, member_chain(X, C), L).

:- begin_tests(chain_snapshot_threads,
               [ condition(current_prolog_flag(threads, true))
               ]).

test(concurrent, Bad == [0, 0, 0, 0]) :-
    numlist(1, 50, Init),
    chain_list(C, Init),
    thread_create(writer(C, 51, 5000), W),
    concurrent_maplist(walker(C),
                       [chain_list, member_chain, chain_list, member_chain],
                       Bad),
    thread_join(W).

:- end_tests(chain_snapshot_threads).
