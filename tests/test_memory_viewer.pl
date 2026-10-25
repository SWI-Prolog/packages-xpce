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

:- module(test_memory_viewer, [test_memory_viewer/0]).

/** <module> Tests for library(swi/pce_memory)

Run with:

    swipl -g test_memory_viewer -t halt \
          packages/xpce/tests/test_memory_viewer.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(swi/pce_memory)).

test_memory_viewer :-
    run_tests([ memory_tree,
                memory_viewer
              ]).

:- dynamic   mv_fact/1, mv_indexed/2, mv_many/1.
:- dynamic   mv_multi/1.
:- dynamic   mv_multi1/1.
:- multifile mv_multi/1, mv_multi1/1.

mv_two(1).
mv_two(2).

mv_multi(from_file).
:- thread_local mv_local/1.
:- table mv_shared/1 as shared.

:- dynamic([mv_dep/1], [incremental(true)]).
:- table mv_inc/1 as incremental.

mv_dep(1).
mv_dep(2).

mv_inc(X) :- mv_dep(X).

%   A predicate of `user`, i.e., non-modular code, defined in this file.

user:mv_user_pred(1).

mv_shared(X) :- between(1, 50, X).

:- table mv_private/2.

mv_private(N, X) :- between(1, N, X).

%   25 private tables of mv_private/2; the larger N, the more space.

fill_private :-
    forall(between(1, 25, I),
           ( N is I*100,
             forall(mv_private(N, _), true)
           )).

:- pce_begin_class(mv_object, object, "Count the instances of me").
:- pce_end_class(mv_object).

:- pce_begin_class(mv_unused, object, "I have no instances").
:- pce_end_class(mv_unused).

%   child(+Term, +Label, -Child)

child(node(_, _, Children, _), Label, Child) :-
    member(Child, Children),
    Child = node(Label, _, _, _),
    !.

path(Term, [], Term).
path(Term, [L|Ls], Node) :-
    child(Term, L, Child),
    path(Child, Ls, Node).

total(Term, Total) :-
    pce_memory:term_total(Term, Total).

%   The tests run in a module of their own, so the database is changed
%   here.

fill_facts :-
    retractall(mv_fact(_)),
    garbage_collect_clauses,
    forall(between(1, 10, I), assertz(mv_fact(I))).

%   with_erased_facts(:Goal)
%
%   Run Goal while the clauses of mv_fact/1 are erased, but cannot be
%   reclaimed as mv_fact/1 is running.

with_erased_facts(Goal) :-
    fill_facts,
    once(( mv_fact(_),
           retractall(mv_fact(_)),
           call(Goal)
         )).

fill_many :-
    retractall(mv_many(_)),
    forall(between(1, 20, I), assertz(mv_many(I))).

fill_indexed :-
    retractall(mv_indexed(_, _)),
    forall(between(1, 100, I), assertz(mv_indexed(x, I))),
    forall(mv_indexed(_, 50), true).

fill_one_source :-
    retractall(mv_multi1(_)),
    forall(between(1, 20, I), assertz(mv_multi1(I))).

fill_multi :-
    retractall(mv_multi(asserted)),
    forall(between(1, 10, _), assertz(mv_multi(asserted))).

fill_local :-
    forall(between(1, 100, I), assertz(mv_local(I))).

:- begin_tests(memory_tree, [condition(current_prolog_flag(threads, true))]).

test(program_predicate, [Total == Size]) :-
    fill_facts,
    predicate_property(test_memory_viewer:mv_fact(_), size(Size)),
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_fact/1'], P),
    total(P, Total).

%   A predicate with a few clauses shows them, with their size and
%   source.

test(program_clauses, [Labels-Value == ['clause 1', 'clause 2']-ClauseSize,
                       true(sub_atom(Title, _, _, _, 'test_memory_viewer.pl'))]) :-
    nth_clause(test_memory_viewer:mv_two(_), 1, Ref),
    clause_property(Ref, size(ClauseSize)),
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_two/1'], node(_, _, Children, _)),
    findall(L, member(node(L, _, _, _), Children), Labels),
    path(T, [test_memory_viewer, 'mv_two/1', 'clause 1'],
         node(_, Value, _, Opts)),
    memberchk(title(Title), Opts).
test(program_many_clauses, [Children == []]) :-
    fill_many,
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_many/1'], node(_, _, Children, _)).
test(program_index, [Value == Size]) :-
    fill_indexed,
    predicate_property(test_memory_viewer:mv_indexed(_,_), indexed(Indexes)),
    member(Index, Indexes),
    Index.arguments == [2],
    !,
    Size = Index.size,
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_indexed/2', 'index 2'],
         node(_, Value, _, _)).
%   Clauses of a multifile predicate from a single file are not split.

test(program_one_source, [Children == []]) :-
    fill_one_source,
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_multi1/1'], node(_, _, Children, _)).
test(program_clause_count, [Title == 'test_memory_viewer:mv_two/1: 2 clauses']) :-
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_two/1'], node(_, _, _, Opts)),
    memberchk(title(Title), Opts).
test(program_sources, [Files == ['<asserted>', 'test_memory_viewer.pl']]) :-
    fill_multi,
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_multi/1'], node(_, _, Children, _)),
    findall(L, member(node(L, _, _, _), Children), Files0),
    msort(Files0, Files).
%   A file shows the number of its clauses; editing it edits the first.

test(program_source_node, [ Data == clause(Ref),
                            true(sub_atom(Title, _, _, 0, ': 1 clause')) ]) :-
    fill_multi,
    nth_clause(test_memory_viewer:mv_multi(_), 1, Ref),
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_multi/1', 'test_memory_viewer.pl'],
         node(_, _, _, Opts)),
    memberchk(title(Title), Opts),
    memberchk(data(Data), Opts).

%   Erased clauses that cannot be reclaimed show below their predicate;
%   the total stays that of the predicate.

test(program_erased, [Total-Erased == Size-ErasedSpace]) :-
    with_erased_facts(
        ( predicate_property(test_memory_viewer:mv_fact(_), size(Size)),
          predicate_property(test_memory_viewer:mv_fact(_),
                             erased_clause_space(ErasedSpace)),
          memory_tree(program, T)
        )),
    path(T, [test_memory_viewer, 'mv_fact/1'], P),
    total(P, Total),
    path(P, ['erased clauses'], node(_, Erased, _, _)).

%   Thread-local predicates are not part of the program, but of the
%   thread that has a copy.

test(program_no_thread_local, fail) :-
    memory_tree(program, T),
    path(T, [test_memory_viewer, 'mv_local/1'], _).
test(thread_local, [Value == Size]) :-
    thread_create(( fill_local,
                    thread_get_message(done)
                  ), Id, [alias(mv_thread)]),
    wait_local(Id),
    '$thread_local_predicate_size'(test_memory_viewer:mv_local(_), Id,
                                   Size),
    memory_tree(threads, T),
    thread_send_message(Id, done),
    thread_join(Id),
    path(T, [ mv_thread, 'thread-local data',
              'test_memory_viewer:mv_local/1'
            ], node(_, Value, _, _)).
test(thread_total, [Total == Size]) :-
    thread_self(Me),
    thread_property(Me, size(Size0)),
    memory_tree(threads, T),
    thread_property(Me, size(Size1)),
    thread_property(Me, id(Id)),
    T = node(_, _, Threads, _),
    member(Th, Threads),
    Th = node(_, _, _, Opts),
    memberchk(data(thread(Id)), Opts),
    !,
    total(Th, Total),
    (   Total == Size0
    ->  Size = Size0
    ;   Size = Size1
    ).
test(thread_stacks, [true(Used > 0)]) :-
    thread_self(Me),
    thread_property(Me, id(Id)),
    memory_tree(threads, node(_, _, Threads, _)),
    member(Th, Threads),
    Th = node(_, _, _, Opts),
    memberchk(data(thread(Id)), Opts),
    !,
    path(Th, [stacks, global, used], node(_, Used, _, _)).
test(shared_tables, [Variants == 1, true(Total >= Pool)]) :-
    abolish_all_tables,
    forall(mv_shared(_), true),
    statistics(shared_table_space_used, Pool),
    memory_tree(tables, T),
    path(T, ['shared tables', 'test_memory_viewer:mv_shared/1'], P),
    P = node(_, _, VNodes, _),
    length(VNodes, Variants),
    path(T, ['shared tables'], S),
    total(S, Total).
test(top_variants, [ Labels == [ 'mv_private(2400,A)', 'mv_private(2500,A)' ],
                     Sizes == Descending,
                     Count == 11,
                     Last == '15 other variants',
                     Total == Sum
                   ]) :-
    abolish_all_tables,
    fill_private,
    memory_tree(tables, T),
    path(T, [main, 'test_memory_viewer:mv_private/2'], P),
    P = node(_, _, VNodes, _),
    length(VNodes, Count),
    VNodes = [node(L1, _, _, _), node(L2, _, _, _)|_],
    sort([L1, L2], Labels),             % 2400 and 2500 use the same space
    once(append(Top, [_Other], VNodes)),
    findall(Size, member(node(_, Size, _, _), Top), Sizes),
    sort(0, @>=, Sizes, Descending),
    last(VNodes, node(Last, _, _, _)),
    total(P, Total),
    aggregate_all(sum(S),
                  ( pce_memory:'$tbl_local_variant_table'(VT),
                    trie_gen(VT, test_memory_viewer:mv_private(_, _), Trie),
                    pce_memory:answer_table(Trie, _, S, _, _)
                  ), Sum),
    abolish_all_tables.
test(table_counts,
     Title == 'test_memory_viewer:mv_private/2: 25 tables, 32,500 answers') :-
    abolish_all_tables,
    fill_private,
    memory_tree(tables, T),
    path(T, [main, 'test_memory_viewer:mv_private/2'],
         node(_, _, _, Options)),
    memberchk(title(Title), Options),
    abolish_all_tables.
%   A completed table that is used often is compiled and its answer
%   trie is discarded; the compiled clause is the space of the table.

test(compiled_table, [ true(Total >= Compiled), Title == Expected ]) :-
    abolish_all_tables,
    current_prolog_flag(table_compile_threshold, Threshold),
    forall(between(0, Threshold, _),
           forall(mv_private(2000, _), true)),
    '$tbl_local_variant_table'(VT),
    trie_gen(VT, test_memory_viewer:mv_private(2000, _), Trie),
    '$trie_property'(Trie, compiled_size(Compiled)),
    memory_tree(tables, T),
    path(T, [ main, 'test_memory_viewer:mv_private/2',
              'mv_private(2000,A)'
            ], Node),
    Node = node(_, Total, _, Options),
    memberchk(title(Title), Options),
    Expected = 'mv_private(2000,A): 2,000 answers, compiled',
    abolish_all_tables.
%   Describing the tables of a thread is expensive, so the description
%   is kept while the space of its tables stays the same.

%   The variants of an incremental dynamic predicate in the variant
%   table are nodes of the dependency graph, not tables.

test(dependency_nodes,
     Title == 'test_memory_viewer:mv_dep/1: incremental dynamic, 3 dependency nodes') :-
    abolish_all_tables,
    forall(mv_inc(_), true),
    ignore(mv_inc(1)),
    ignore(mv_inc(2)),
    memory_tree(tables, T),
    path(T, [main, 'test_memory_viewer:mv_dep/1'], node(_, _, _, Options)),
    memberchk(title(Title), Options),
    abolish_all_tables.
test(thread_tables_cached, Tree == cached) :-
    thread_self(Me),
    thread_create(( fill_private,
                    thread_send_message(Me, filled),
                    thread_get_message(done)
                  ), Id, [alias(mv_cached)]),
    thread_get_message(filled),
    memory_tree(threads, _),
    pce_memory:thread_key(Id, Key),
    retract(pce_memory:thread_tables_cache(Key, Space, _)),
    assertz(pce_memory:thread_tables_cache(Key, Space,
                                           node(cached, 0, [], []))),
    memory_tree(threads, T),
    thread_send_message(Id, done),
    thread_join(Id),
    retractall(pce_memory:thread_tables_cache(Key, _, _)),
    path(T, [mv_cached, cached], node(Tree, _, _, _)).
test(thread_tables, Count == 11) :-
    thread_self(Me),
    thread_create(( fill_private,
                    thread_send_message(Me, filled),
                    thread_get_message(done)
                  ), Id, [alias(mv_tabler)]),
    thread_get_message(filled),
    memory_tree(threads, T),
    thread_send_message(Id, done),
    thread_join(Id),
    path(T, [ mv_tabler, 'private tables',
              'test_memory_viewer:mv_private/2'
            ], node(_, _, VNodes, _)),
    length(VNodes, Count).
%   The tables view shows the shared tables and the private tables of
%   each thread that has them.

test(tables_per_thread, [ Count-HasIdle == 11-false ]) :-
    abolish_all_tables,
    thread_self(Me),
    thread_create(( fill_private,
                    thread_send_message(Me, filled),
                    thread_get_message(done)
                  ), Id, [alias(mv_table_owner)]),
    thread_create(thread_get_message(done), Idle, [alias(mv_no_tables)]),
    thread_get_message(filled),
    memory_tree(tables, T),
    thread_send_message(Id, done),
    thread_send_message(Idle, done),
    thread_join(Id),
    thread_join(Idle),
    path(T, [ mv_table_owner, 'test_memory_viewer:mv_private/2' ],
         node(_, _, VNodes, _)),
    length(VNodes, Count),
    (   path(T, [mv_no_tables], _)
    ->  HasIdle = true
    ;   HasIdle = false
    ).
%   The application view shows the modules of the application, split
%   into the static program and the dynamic data.  A dynamic predicate
%   is data if it has clauses, none of which are loaded from a file.
%   The `user` module is split by file.

test(application_parts, Labels == ['static program', 'dynamic data']) :-
    memory_tree(application, node(_, _, Parts, _)),
    findall(L, member(node(L, _, _, _), Parts), Labels).
test(application_no_library, fail) :-
    memory_tree(application, T),
    path(T, ['static program', lists], _).
test(application_no_hook_module, fail) :-   % only hooks, no file
    memory_tree(application, T),
    path(T, ['static program', prolog], _).
test(application_static, true) :-
    memory_tree(application, T),
    path(T, ['static program', test_memory_viewer, 'mv_two/1'], _).
test(application_data, [Static-Data == false-true]) :-
    fill_facts,
    memory_tree(application, T),
    truth(path(T, ['static program', test_memory_viewer, 'mv_fact/1'], _),
          Static),
    truth(path(T, ['dynamic data', test_memory_viewer, 'mv_fact/1'], _),
          Data).
test(application_loaded_dynamic, [Static-Data == true-false]) :-
    memory_tree(application, T),
    truth(path(T, ['static program', test_memory_viewer, 'mv_multi/1'], _),
          Static),
    truth(path(T, ['dynamic data', test_memory_viewer, 'mv_multi/1'], _),
          Data).
test(application_user_files, true) :-
    memory_tree(application, T),
    path(T, [ 'static program', user, 'test_memory_viewer.pl',
              'mv_user_pred/1'
            ], _).
test(application_user_no_system, Files == ['test_memory_viewer.pl']) :-
    memory_tree(application, T),
    path(T, ['static program', user], node(_, _, Children, _)),
    findall(F, ( member(node(F, _, _, Opts), Children),
                 memberchk(data(file(Path)), Opts),
                 sub_atom(Path, _, _, _, '/xpce/tests/')
               ), Files0),
    sort(Files0, Files),
    current_prolog_flag(home, Home),
    \+ ( member(node(_, _, _, Opts), Children),
          memberchk(data(file(Path)), Opts),
          sub_atom(Path, 0, _, _, Home)
        ).
test(application_user_no_hooks, fail) :-     % declared, no clauses
    memory_tree(application, T),
    path(T, ['static program', user], node(_, _, Files, _)),
    member(File, Files),
    child(File, 'message_hook/3', _).
test(data_view, true) :-
    fill_facts,
    memory_tree(data, T),
    path(T, [test_memory_viewer, 'mv_fact/1'], _).
test(file_labels, Labels == ['a/init.pl', 'b/init.pl', 'x.pl']) :-
    Files = ['/p/a/init.pl', '/p/b/init.pl', '/p/x.pl'],
    maplist(pce_memory:file_label(Files), Files, Labels).

truth(Goal, Truth) :-
    (   Goal
    ->  Truth = true
    ;   Truth = false
    ).

test(overview, [Labels = [ program, threads, 'shared tables', xpce,
                           atoms, functors | _ ]]) :-
    memory_tree(overview, node(_, _, Children, _)),
    findall(L, member(node(L, _, _, _), Children), Labels).
%   A class is below its super class; its size is that of the
%   instances that exist.

test(xpce_class, [Own == Expected]) :-
    findall(O, (between(1, 5, _), new(O, mv_object)), Objects),
    get(class(mv_object), instance_size, Size),
    Expected is 5*Size,
    memory_tree(xpce, T),
    maplist(free, Objects),
    path(T, [object, mv_object], node(_, Own, _, _)).
test(xpce_no_empty_class, fail) :-
    new(O, mv_unused),                  % realise the class
    free(O),
    memory_tree(xpce, T),
    path(T, [object, mv_unused], _).
test(edit_targets, [Targets == [true, true, true, false, true]]) :-
    nth_clause(test_memory_viewer:mv_two(_), 1, Ref),
    assertz(mv_fact(0), Asserted),
    maplist([Data,R]>>( pce_memory:edit_target(Data, Target),
                        (pce_memory:has_source(Target) -> R = true ; R = false)
                      ),
            [ module(test_memory_viewer),
              predicate(test_memory_viewer:mv_two/1),
              clause(Ref),
              clause(Asserted),
              table(test_memory_viewer:mv_private(10, _))
            ], Targets),
    erase(Asserted).
test(bad_view, error(domain_error(memory_view, nonsense))) :-
    memory_tree(nonsense, _).

wait_local(Id) :-
    (   '$thread_local_clause_count'(test_memory_viewer:mv_local(_), Id, 100)
    ->  true
    ;   sleep(0.01),
        wait_local(Id)
    ).

:- end_tests(memory_tree).

%   wait_for(:Get, :Test, +Timeout)
%
%   Dispatch events until Test is true after Get.

wait_for(Get, Test, Timeout) :-
    get_time(T0),
    repeat,
      ignore(send(@display, dispatch)),
      call(Get),
      (   call(Test)
      ->  !
      ;   get_time(Now),
          Now-T0 > Timeout
      ->  !, fail
      ;   sleep(0.05),
          fail
      ).

:- begin_tests(memory_viewer, [condition(current_prolog_flag(threads, true))]).

%   Refresh keeps the node zoomed into.

test(refresh_keeps_focus, [Label == test_memory_viewer]) :-
    new(MV, prolog_memory_viewer),
    send(MV, view, program),
    send(MV, refresh, @on),
    get(MV, chart_window, W),
    get(W, root, Root),
    get(Root?children, find, @arg1?label == test_memory_viewer, M),
    send(W, focus_node, M),
    send(MV, refresh, @on),
    get(W?focus_node?label, value, Label),
    free(MV).

%   A refresh computes the tree in a thread of its own; the GUI shows
%   it when it arrives.  A tree that arrives after a newer refresh
%   started is dropped.

test(refresh_in_thread, [Label == threads]) :-
    new(MV, prolog_memory_viewer),
    send(MV, view, threads),
    wait_for(get(MV, busy, Busy), Busy == @off, 10),
    get(MV?chart_window?root?label, value, Label),
    free(MV).
test(stale_refresh_dropped, [Label == program]) :-
    new(MV, prolog_memory_viewer),
    send(MV, view, program),
    send(MV, refresh, @on),
    get(MV, generation, Gen),
    Stale is Gen-1,
    send(MV, show_tree, Stale, node(stale, 1, [], []), 0),
    get(MV?chart_window?root?label, value, Label),
    free(MV).
%   The viewer only refreshes on request, unless the auto refresh menu
%   says every so many seconds.

test(no_auto_refresh, [Timer-Selection == @nil-0]) :-
    new(MV, prolog_memory_viewer),
    get(MV, timer, Timer),
    get(MV, menu, auto_refresh, AM),
    get(AM, selection, Selection),
    free(MV).
test(auto_refresh, [Interval-Selection =@= 5-5]) :-
    new(MV, prolog_memory_viewer),
    get(MV, menu, auto_refresh, AM),
    get(AM, message, Message),
    send(Message, forward, 5),          % as choosing "5 sec"
    get(MV?timer, interval, Interval0),
    Interval is integer(Interval0),
    get(AM, selection, Selection),
    free(MV).
test(auto_refresh_off, [Timer == @nil]) :-
    new(MV, prolog_memory_viewer),
    send(MV, auto_refresh, 5),
    send(MV, auto_refresh, 0),
    get(MV, timer, Timer),
    free(MV).
test(view_menu, [Selection == threads]) :-
    new(MV, prolog_memory_viewer),
    send(MV, view, threads),
    get(MV, menu, view, Menu),
    get(Menu, selection, Selection),
    free(MV).

:- end_tests(memory_viewer).
