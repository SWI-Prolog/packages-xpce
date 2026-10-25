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

:- module(test_sdg_viewer, [test_sdg_viewer/0]).

/** <module> Tests for library(swi/pce_sdg_viewer)

Run with:

    swipl -g test_sdg_viewer -t halt packages/xpce/tests/test_sdg_viewer.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

%   The IDE writes the window arrangements and geometry to the user's
%   xpce configuration.  Use a configuration of our own.

:- use_module(xpce_test_sandbox).
:- initialization(sandbox_app_config(_), now).

:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(swi_ide)).
:- use_module(library(pce_util), [member_chain/2]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(thread)).
:- use_module(library(swi/pce_sdg_viewer)).

test_sdg_viewer :-
    run_tests([ sdg_graph,
                sdg_viewer,
                sdg_ide,
                sdg_monitor
              ]).

%   sv_p/1 and sv_q/1 form a positive loop that calls the negative loop
%   of sv_r/0 and sv_s/0, which is a component of its own.  take/0
%   stores the SDG while they are incomplete.

:- table sv_p/1, sv_q/1, sv_r/0, sv_s/0.

sv_p(X) :- sv_q(X), sv_r.
sv_p(1).
sv_q(X) :- sv_p(X).
sv_q(2) :- sv_r.

sv_r :- tnot(sv_s).
sv_s :- tnot(sv_r), take.
sv_s :- tnot(sv_r).

:- dynamic snapshot/1.

take :-
    '$tbl_sdg'(Cs, Ts, Es),
    assertz(snapshot(sdg(Cs, Ts, Es))).

sdg(SDG) :-
    retractall(snapshot(_)),
    abolish_all_tables,
    ignore(sv_p(_)),
    snapshot(SDG),
    !,
    retractall(snapshot(_)).

%   nodes(+Items, -Ids)
%
%   Ids are the ids of the nodes, also those inside clusters, with the
%   tables as their variants.

nodes(SDG, Items, Ids) :-
    findall(Id, item_node(SDG, Items, Id), Ids0),
    msort(Ids0, Ids).

item_node(SDG, Items, Id) :-
    member(Item, Items),
    (   Item = node(Id0, _),
        id_term(SDG, Id0, Id)
    ;   Item = cluster(_, _, Children),
        item_node(SDG, Children, Id)
    ).

id_term(sdg(_, Ts, _), table(Trie), Variant) :-
    !,
    memberchk(table(Trie, Variant0, _, _, _), Ts),
    copy_term(Variant0, Variant),
    numbervars(Variant, 0, _).
id_term(_, Id, Id).

%   clusters(+Items, -Tree)
%
%   The nesting of the clusters: a list of Kind-SubTree.

clusters(Items, Tree) :-
    findall(Kind-Sub,
            ( member(cluster(Id, _, Children), Items),
              functor(Id, Kind, _),
              clusters(Children, Sub)
            ), Tree).

has_dot :-
    absolute_file_name(path(dot), _, [access(execute), file_errors(fail)]).

:- begin_tests(sdg_graph).

test(variants, Ids == [ test_sdg_viewer:sv_r,
                        test_sdg_viewer:sv_s,
                        test_sdg_viewer:sv_p('$VAR'(0)),
                        test_sdg_viewer:sv_q('$VAR'(0))
                      ]) :-
    sdg(SDG),
    sdg_graph(SDG, [], graph(Items, _)),
    nodes(SDG, Items, Ids).
test(nested_components, Tree == [scc-[scc-[]]]) :-
    sdg(SDG),
    sdg_graph(SDG, [level(variants)], graph(Items, _)),
    clusters(Items, Tree).
test(leaders, Leaders == [test_sdg_viewer:sv_r,
                          test_sdg_viewer:sv_p('$VAR'(0))]) :-
    sdg(SDG),
    sdg_graph(SDG, [], graph(Items, _)),
    findall(V, ( item_node(SDG, Items, V),
                 sub_item(Items, node(_, table(V0, _, _, true))),
                 copy_term(V0, V), numbervars(V, 0, _)
               ), Leaders0),
    sort(Leaders0, Leaders).
test(predicates, [ Ids-Clusters ==
                   [ pred(test_sdg_viewer:sv_p/1), pred(test_sdg_viewer:sv_q/1),
                     pred(test_sdg_viewer:sv_r/0), pred(test_sdg_viewer:sv_s/0)
                   ]-[]
                 ]) :-
    sdg(SDG),
    sdg_graph(SDG, [level(predicates)], graph(Items, _)),
    nodes(SDG, Items, Ids),
    clusters(Items, Clusters).
test(negative_edges, Negative == [ edge(pred(test_sdg_viewer:sv_r/0),
                                        pred(test_sdg_viewer:sv_s/0), 1),
                                   edge(pred(test_sdg_viewer:sv_s/0),
                                        pred(test_sdg_viewer:sv_r/0), 2)
                                 ]) :-
    sdg(SDG),
    sdg_graph(SDG, [level(predicates)], graph(_, Edges)),
    findall(edge(F, T, C), member(edge(F, T, negative, C), Edges), Negative0),
    msort(Negative0, Negative).
test(expanded, [ Ids-Clusters ==
                 [ pred(test_sdg_viewer:sv_q/1), pred(test_sdg_viewer:sv_r/0),
                   pred(test_sdg_viewer:sv_s/0), test_sdg_viewer:sv_p('$VAR'(0))
                 ]-[pred-[]]
               ]) :-
    sdg(SDG),
    sdg_graph(SDG, [level(predicates), expanded([test_sdg_viewer:sv_p/1])],
              graph(Items, _)),
    nodes(SDG, Items, Ids),
    clusters(Items, Clusters).
%   The edges of an expanded predicate go to its tables.

test(expanded_edges, Edge == edge(pred(test_sdg_viewer:sv_q/1), p, positive, 1)) :-
    sdg(SDG),
    sdg_graph(SDG, [level(predicates), expanded([test_sdg_viewer:sv_p/1])],
              graph(_, Edges)),
    once(member(edge(F, table(_), positive, C), Edges)),
    Edge = edge(F, p, positive, C).
test(dot, [condition(has_dot), true(Problems == [@nil, @nil, @nil])]) :-
    sdg(SDG),
    findall(Problem,
            ( member(Options, [ [level(variants)],
                                [level(predicates)],
                                [level(predicates),
                                 expanded([test_sdg_viewer:sv_s/0])]
                              ]),
              sdg_graph(SDG, Options, Graph),
              sdg_dot(Graph, Dot, _),
              new(X, xdot),
              send(X, load, Dot),
              get(X, problem, Problem),
              free(X)
            ), Problems).
test(dot_negative, true(sub_string(Dot, _, _, _, "style=dashed, color=\"#c04040\""))) :-
    sdg(SDG),
    sdg_graph(SDG, [], Graph),
    sdg_dot(Graph, Dot, _).

:- end_tests(sdg_graph).

sub_item(Items, Item) :-
    member(I, Items),
    (   I = Item
    ;   I = cluster(_, _, Children),
        sub_item(Children, Item)
    ).

%   The viewer shows the SDG of a thread that waits inside a table.

:- table sv_w/1.

sv_w(X) :- sv_w(X).
sv_w(1) :- thread_get_message(go).

%   waiting(-Thread)
%
%   Thread runs sv_w/1 and waits for `go`, at most 5 seconds.

waiting(Id) :-
    thread_create(sv_w(_), Id),
    wait_for(call_in_thread(Id, '$tbl_sdg'(_, Ts, _)), Ts \== [], 5).

done(Id) :-
    thread_send_message(Id, go),
    thread_join(Id).

%   wait_for(:Get, :Test, +Timeout)
%
%   Dispatch events until Test is true after Get, which may fail.

wait_for(Get, Test, Timeout) :-
    get_time(T0),
    repeat,
      ignore(send(@display, dispatch)),
      (   call(Get),
          call(Test)
      ->  !
      ;   get_time(Now),
          Now-T0 > Timeout
      ->  !, fail
      ;   sleep(0.05),
          fail
      ).

status(V, Status) :-
    get(V?dialog, member, status, L),
    get(L, selection, S),
    get(S, value, Status).

names(V, Names) :-
    get(V?graph_window, names, Names).

viewer(Id, V) :-
    new(V, prolog_sdg_viewer(Id)),
    wait_for(status(V, S), sub_atom(S, _, _, _, incomplete), 10).

click(V, Id) :-
    get(V, graph_window, W),
    names(V, Names),
    memberchk(Name-Id, Names),
    get(W?xdot?graphicals, find, @arg1?name == Name, Node),
    send(W, clicked, Node).

:- begin_tests(sdg_viewer, [ condition((current_prolog_flag(threads, true),
                                        has_dot))
                           ]).

test(status, Status == '1 incomplete table in 1 SCC') :-
    waiting(Id),
    viewer(Id, V),
    status(V, Status),
    free(V),
    done(Id).
test(variants, Ids == [table]) :-
    waiting(Id),
    viewer(Id, V),
    names(V, Names),
    findall(K, (member(_-I, Names), functor(I, K, _)), Ids),
    free(V),
    done(Id).
test(predicates, Ids-Selection == [pred(test_sdg_viewer:sv_w/1)]-predicates) :-
    waiting(Id),
    viewer(Id, V),
    send(V, graph_level, predicates),
    names(V, Names),
    findall(I, member(_-I, Names), Ids),
    get(V?dialog, member, level, Menu),
    get(Menu, selection, Selection),
    free(V),
    done(Id).
test(click_expands, [ Expanded-Kinds == [test_sdg_viewer:sv_w/1]-[pred, table]
                    ]) :-
    waiting(Id),
    viewer(Id, V),
    send(V, graph_level, predicates),
    click(V, pred(test_sdg_viewer:sv_w/1)),
    get(V, expanded, Expanded),
    names(V, Names),
    findall(K, (member(_-I, Names), functor(I, K, _)), Kinds0),
    msort(Kinds0, Kinds),
    free(V),
    done(Id).
test(click_collapses, Expanded == []) :-
    waiting(Id),
    viewer(Id, V),
    send(V, graph_level, predicates),
    click(V, pred(test_sdg_viewer:sv_w/1)),
    click(V, pred(test_sdg_viewer:sv_w/1)),       % the cluster
    get(V, expanded, Expanded),
    free(V),
    done(Id).
%   A thread that ended has a name a slot can hold, so the viewer can say
%   it is gone.

test(ended_thread, true(atom(Name))) :-
    thread_create(true, T),
    thread_join(T),
    pce_sdg_viewer:thread_name(T, Name).

%   A thread without incomplete tables has an empty SDG.

test(other_thread, Status == '0 incomplete tables in 0 SCCs') :-
    waiting(Id),
    viewer(Id, V),
    thread_create(thread_get_message(stop), Idle),
    send(V, thread, Idle),
    wait_for(status(V, Status), sub_atom(Status, 0, _, _, '0 '), 10),
    thread_send_message(Idle, stop),
    thread_join(Idle),
    free(V),
    done(Id).

:- end_tests(sdg_viewer).

%   The IDE shows the SDG viewer from the Debug/Tabling menu, the thread
%   monitor and when a break starts in a thread with incomplete tables.

no_frames :-
    get(@prolog_ide, members, Members),
    forall(( member_chain(F, Members),
             send(F, instance_of, pane_frame)
           ),
           send(F, destroy)).

ide_viewer(V) :-
    get(@prolog_ide, tool, prolog_sdg_viewer, V).

%   sv_b/1 breaks while sv_a/1 and sv_b/1 are incomplete.  in_break/1
%   runs a goal in a thread whose break waits for `go`.  The first break
%   of a thread without a toplevel is silent (level 0), so it starts a
%   second one.

:- table sv_a/1, sv_b/1.

sv_a(X) :- sv_b(X).
sv_b(X) :- sv_a(X).
sv_b(1) :- break.

in_break(Goal) :-
    setup_call_cleanup(
        ( open_string("break.\nthread_get_message(go).\n", In),
          open_null_stream(Null)
        ),
        ( set_stream(In, alias(user_input)),
          set_stream(Null, alias(user_output)),
          set_stream(Null, alias(user_error)),
          call(Goal)
        ),
        ( close(In),
          close(Null) )).

:- begin_tests(sdg_ide, [ condition((current_prolog_flag(threads, true),
                                     has_dot)),
                          cleanup(no_frames)
                        ]).

test(open_and_reuse, [ Thread1-Thread2-Same == Id1-Id2-true,
                       cleanup(no_frames) ]) :-
    waiting(T1), thread_property(T1, id(Id1)),
    waiting(T2), thread_property(T2, id(Id2)),
    send(@prolog_ide, sdg_viewer, Id1),
    ide_viewer(V1),
    get(V1, thread, Thread1),
    send(@prolog_ide, sdg_viewer, Id2),
    ide_viewer(V2),
    get(V2, thread, Thread2),
    (V1 == V2 -> Same = true ; Same = false),
    done(T1), done(T2).
test(break_opens, [ Thread-Status == Id-'2 incomplete tables in 1 SCC',
                    cleanup(no_frames) ]) :-
    setup_call_cleanup(
        thread_create(in_break(sv_a(_)), T),
        ( thread_property(T, id(Id)),
          wait_for(ide_viewer(V), true, 10),
          wait_for(status(V, Status), sub_atom(Status, 0, _, _, '2 '), 10),
          get(V, thread, Thread)
        ),
        ( thread_send_message(T, go),
          thread_join(T) )).
test(no_tables_no_viewer, [ fail, cleanup(no_frames) ]) :-
    thread_create(in_break(break), T),
    thread_send_message(T, go),
    thread_join(T),
    ignore(wait_for(true, fail, 0.3)),
    ide_viewer(_).

:- end_tests(sdg_ide).

%   The thread monitor samples the incomplete tables of a thread and
%   the SCCs per incomplete table.

:- use_module(library(swi/thread_monitor), []).

:- begin_tests(sdg_monitor, [ condition(current_prolog_flag(threads, true))
                            ]).

test(sample, Incomplete-Ratio == 1-100) :-
    waiting(Id),
    thread_property(Id, id(TID)),               % the monitor knows ids
    new(S, thread_statistics(TID, 0)),
    get(S, incomplete, Incomplete),
    get(S, scc_ratio, Ratio),
    free(S),
    done(Id).
test(sample_none, Incomplete-Ratio == 0-0) :-
    new(S, thread_statistics(main, 0)),
    get(S, incomplete, Incomplete),
    get(S, scc_ratio, Ratio),
    free(S).
test(colours, true) :-
    pce_thread_monitor:graph_colour(incomplete, C1),
    pce_thread_monitor:graph_colour(scc_ratio, C2),
    send(C1, instance_of, colour),
    send(C2, instance_of, colour).

:- end_tests(sdg_monitor).
