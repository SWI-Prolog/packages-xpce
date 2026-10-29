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

:- module(test_profiler, [test_profiler/0]).
:- encoding(utf8).

/** <module> Test the profiler GUI

Tests library(swi/pce_profile) on profile data made up here rather than
collected, so the numbers are known.

Run with:

    swipl -g test_profiler -t halt packages/xpce/tests/test_profiler.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(swi/pce_profile)).
:- use_module(library(pce_class_variable_editor), []).

test_profiler :-
    run_tests([ profiler
              ]).

%!  profile(-Data) is det.
%
%   A profile of 100 ticks in the shape of profile_data/1.

profile(profile{summary:summary{samples:100, ticks:100, accounting:0,
                                time:0.5, nodes:10, sample_period:5000,
                                ports:true},
                nodes:[ node{predicate:user:p/1,
                             ticks_self:10, ticks_siblings:90,
                             call:5, redo:0, exit:5,
                             callers:[node(user:q/1,0,10,80,5,0,5),
                                      node(user:q/1,1,10,70,3,0,3),
                                      node('<recursive>',0,0,0,3,0,3),
                                      node('<recursive>',1,0,0,4,0,4)],
                             callees:[node(user:q/1,0,10,80,5,0,5),
                                      node(user:r/1,0,5,0,2,0,2)]},
                        node{predicate:user:q/1,
                             ticks_self:10, ticks_siblings:80,
                             call:5, redo:2, exit:4,
                             callers:[node(user:p/1,0,10,80,5,0,5)],
                             callees:[node(user:p/1,0,10,80,5,0,5)]},
                        node{predicate:user:r/1,
                             ticks_self:5, ticks_siblings:0,
                             call:2, redo:0, exit:2,
                             callers:[node(user:p/1,0,5,0,2,0,2),
                                      node('<recursive>',0,0,0,6,0,6)],
                             callees:[]}
                      ]}).

%!  profiler(-Profiler) is det.
%!  profiler(+Ports, -Profiler) is det.
%
%   A profiler holding profile/1, as recorded with the ports option
%   Ports.  It is loaded without `prof_frame->load_profile', which
%   reports on the window it is in.

profiler(F) :-
    profiler(true, F).

profiler(Ports, F) :-
    profile(Data0),
    profiler(Ports, Data0, F).

profiler(Ports, Data0, F) :-
    Data = Data0.put(summary/ports, Ports),
    Summary = Data.summary,
    new(F, prof_frame),
    send(F, slot, samples, Summary.samples),
    send(F, slot, ticks, Summary.ticks),
    send(F, slot, accounting_ticks, Summary.accounting),
    send(F, slot, time, Summary.time),
    send(F, slot, nodes, Summary.nodes),
    send(F, slot, ports, Summary.ports),
    send(F, slot, clock, Summary.get(clock, cpu)),
    send(F, slot, exact, Summary.get(exact, false)),
    get(F, window, prof_browser, B),
    send(B, load_profile, Data.nodes).

%!  call_graph(+Profiler, +Pred, -Edges) is det.
%
%   The edges of the call graph around Pred with the default limits.

call_graph(F, Pred, Edges) :-
    get(F, node_data, Pred, Data),
    pce_profile:call_graph(F, Data, limits(2, 2, 6, 12, 30), Edges).

%!  tooltip(+Profiler, +Pred, -Lines) is det.
%
%   The lines of the tooltip of the node of Pred in the call graph.

tooltip(F, Pred, Lines) :-
    pce_profile:node_attrs(F, Pred, Pred, 100, Attrs),
    memberchk(tooltip=Tooltip, Attrs),
    split_string(Tooltip, "\n", "", Lines).

browser_labels(F, Labels) :-
    get(F, window, prof_browser, B),
    get(B?members, map, @arg1?label?value, Chain),
    chain_list(Chain, Labels).

:- begin_tests(profiler, [condition(current_prolog_flag(threads, true))]).

%   The status line describes how the profile was collected.  Exact
%   profiles do not sample and exclude the profiler's overhead.

test(summary_sampled,
     Text == '100 samples of CPU time in 0.50 sec; 3 predicates; \c
              10 nodes in call-graph; distortion 0%') :-
    profiler(F),
    get(F, statistics_summary, Str),
    get(Str, value, Text).
test(summary_exact,
     Text == 'Exact wall time: 0.400 sec in predicates; 3 predicates; \c
              10 nodes in call-graph; \c
              profiler overhead 0.100 sec (excluded)') :-
    profile(Data0),
    Data = Data0.put(summary, Data0.summary.put(_{clock:wall, exact:true,
                                                  ticks:500 000 000,
                                                  accounting:100 000 000})),
    profiler(true, Data, F),
    get(F, statistics_summary, Str),
    get(Str, value, Text).

%   Sorted on the time of the children, the time is shown as such
%   rather than as a number of ticks.

test(sort_by_children,
     Labels == ['p/1\t90.0%', 'q/1\t80.0%', 'r/1\t0.0%']) :-
    profiler(F),
    send(F, sort_by, ticks_siblings),
    browser_labels(F, Labels),
    free(F).

%   The recursive calls are the sum over the cycles p/1 is part of.

test(recursive_calls, Calls == 7) :-
    profiler(F),
    call_graph(F, user:p/1, Edges),
    memberchk(edge(user:p/1, user:p/1, _, Calls), Edges),
    free(F).

%   The tooltip of a node shows how the calls ended, if the profile
%   recorded the ports.

test(tooltip_ports,
     Ports == "Call: 5, redo: 2, exit: 4, fail: 3") :-
    profiler(F),
    tooltip(F, user:q/1, [_, _, Ports]),
    free(F).
test(tooltip_no_ports, Ports == "Call: 5") :-
    profiler(false, F),
    tooltip(F, user:q/1, [_, _, Ports]),
    free(F).

%   The recursion of a callee shows, although its own callers are not
%   walked.

test(recursive_callee, Calls == 6) :-
    profiler(F),
    call_graph(F, user:p/1, Edges),
    memberchk(edge(user:r/1, user:r/1, _, Calls), Edges),
    free(F).

%   The calls in all cycles are counted, but the time is that of the
%   outermost cycle, as it includes that of the cycles further in.  An
%   edge shows the calls only.

test(cycle_edge, [Ticks-Calls == 90-8, Label == "8×"]) :-
    profiler(F),
    call_graph(F, user:p/1, Edges),
    memberchk(edge(user:q/1, user:p/1, Ticks, Calls), Edges),
    pce_profile:edge_attrs(Ticks, Calls, 100, Attrs),
    memberchk(label=Label, Attrs),
    free(F).

%   Changing a limit of the call graph in the class variable editor
%   draws the graph again.  It is not shown, so it is only marked to
%   be drawn when it is.

test(limit_changed, Stale == @on) :-
    profiler(F),
    get(F, window, prof_graph, G),
    get(F, node_data, user:p/1, Data),
    send(G, slot, node, Data),
    send(G, slot, stale, @off),
    get(class(prof_graph), class_variable, max_nodes, CV),
    get(CV, value, Old),
    New is Old+1,
    setup_call_cleanup(
        pce_class_variable_editor:apply_class_variable(
                                      max_nodes, Old, New, class(prof_graph)),
        get(G, stale, Stale),
        send(CV, value, Old)),
    free(F).

%   The call tree below p/1.  Ticks are 5 ms.  q/1 spends 10 ticks
%   itself and 80 below it on behalf of p/1, all in p/1, which is not
%   expanded again as it is on the path.  r/1 calls nothing.

test(call_tree, [ Tree == node('p/1', 0.05,
                               [ node('q/1', 0.05, [node('p/1', 0.45, [])]),
                                 node('r/1', 0.025, [])
                               ]),
                  Total =:= 0.575
                ]) :-
    profiler(F),
    get(F, window, prof_tree, T),
    get(F, node_data, user:p/1, Data),
    send(T, render, Data),
    get(T, root, Root),
    get(Root, total, Total),
    tree_term(Root, Tree),
    free(F).

%   ev/1 and od/1 call each other; each level of the recursion is a
%   cycle of its own in the profile.  The calls of ev/1 to od/1 are
%   nested: the outer one includes the inner one.  Its calls to work/0
%   at the two levels are distinct.  The tree and the graph have one
%   node for each.

recursion_profile(
    profile{summary:summary{samples:100, ticks:100, accounting:0,
                            time:0.5, nodes:10, sample_period:5000,
                            ports:true},
            nodes:[ node{predicate:user:ev/1,
                         ticks_self:0, ticks_siblings:100,
                         call:2, redo:0, exit:2,
                         callers:[node(user:od/1,1,0,60,1,0,1)],
                         callees:[node(user:work/0,0,10,0,1,0,1),
                                  node(user:od/1,0,0,80,1,0,1),
                                  node(user:work/0,1,10,0,1,0,1),
                                  node(user:od/1,1,0,60,1,0,1)]},
                    node{predicate:user:od/1,
                         ticks_self:0, ticks_siblings:80,
                         call:2, redo:0, exit:2,
                         callers:[node(user:ev/1,0,0,80,1,0,1),
                                  node(user:ev/1,1,0,60,1,0,1)],
                         callees:[node(user:ev/1,1,0,70,1,0,1),
                                  node(user:work/0,0,10,0,1,0,1)]},
                    node{predicate:user:work/0,
                         ticks_self:30, ticks_siblings:0,
                         call:3, redo:0, exit:3,
                         callers:[node(user:ev/1,0,10,0,1,0,1),
                                  node(user:ev/1,1,10,0,1,0,1),
                                  node(user:od/1,0,10,0,1,0,1)],
                         callees:[]}
                  ]}).

test(tree_merges_cycles,
     [ Children == [ node('work/0', 0.1, []),
                     node('od/1', 0, [ node('ev/1', 0.35, []),
                                       node('work/0', 0.05, [])
                                     ])
                   ]
     ]) :-
    recursion_profile(Data),
    profiler(true, Data, F),
    get(F, window, prof_tree, T),
    get(F, node_data, user:ev/1, EvData),
    send(T, render, EvData),
    get(T, root, Root),
    tree_term(Root, node(_, _, Children0)),
    msort(Children0, Sorted),
    reverse(Sorted, Children),
    free(F).
test(graph_merges_cycles, [ Work-Od == 20-80 ]) :-
    recursion_profile(Data),
    profiler(true, Data, F),
    call_graph(F, user:ev/1, Edges),
    memberchk(edge(user:ev/1, user:work/0, Work, _), Edges),
    memberchk(edge(user:ev/1, user:od/1, Od, _), Edges),
    free(F).
test(components, [ Same-Other == true-false ]) :-
    recursion_profile(Data),
    profiler(true, Data, F),
    get(F, components, Cs),
    truth(pce_profile:same_component(user:ev/1, user:od/1, Cs), Same),
    truth(pce_profile:same_component(user:ev/1, user:work/0, Cs), Other),
    free(F).

%   Clicking a node of the call tree, also one with children, makes its
%   predicate the current node of the profiler, which all tabs show.

test(tree_click, [Current-Graph == (user:q/1)-(user:q/1)]) :-
    profiler(F),
    get(F, window, prof_tree, T),
    get(F, node_data, user:p/1, Data),
    send(F, details, Data),
    send(T, render, Data),
    get(T, root, Root),
    get(Root?children, find, @arg1?label == 'q/1', Q),
    send(Q, has_children),
    send(T?chart, clicked, Q),
    get(T, node, TData),
    Current = TData.predicate,
    get(F, window, prof_graph, G),
    get(G, node, GData),
    Graph = GData.predicate,
    free(F).

%   The popup of the tree edits the predicate it was opened on.

test(tree_popup_edit, [CanEdit-Recursive == true-false]) :-
    profiler(F),
    get(F, window, prof_tree, T),
    get(F, node_data, user:p/1, Data),
    send(T, render, Data),
    get(T, root, Root),
    send(T, slot, popup_node, Root),
    truth(send(T, can_edit), CanEdit),
    new(R, hierarchy_node('<recursive>', 1, '<recursive>')),
    send(T, slot, popup_node, R),
    truth(send(T, can_edit), Recursive),
    free(F).

%   As the boxes of the call graph, the call tree leaves out the module,
%   which its balloon shows.

test(tree_labels, [Label-Title == 'append/3'-'lists:append/3']) :-
    pce_profile:tree_labels(lists:append/3, Label0, Title0),
    maplist(text_atom, [Label0, Title0], [Label, Title]).

text_atom(Text, Atom) :-
    (   atom(Text)
    ->  Atom = Text
    ;   get(Text, value, Atom)
    ).

truth(G, true) :- call(G), !.
truth(_, false).

:- end_tests(profiler).

tree_term(Node, node(Label, Value, Children)) :-
    get(Node?label, value, Label),
    get(Node, value, Value0),
    Value is round(Value0*1000)/1000,
    (   get(Node, children, Chain),
        Chain \== @nil
    ->  chain_list(Chain, Nodes),
        maplist(tree_term, Nodes, Children)
    ;   Children = []
    ).
