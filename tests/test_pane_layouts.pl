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

:- module(test_pane_layouts, [test_pane_layouts/0]).
:- encoding(utf8).

/** <module> Tests for library(pane_layouts)

Where a new pane goes, read off the arrangements of windows holding those
kinds of pane.  All of this is plain Prolog over terms: no window is made
and nothing here needs a display.

Run with:

    swipl -g test_pane_layouts -t halt \
          packages/xpce/tests/test_pane_layouts.pl
*/

:- use_module(library(plunit)).
:- use_module(library(pane_layouts)).
:- use_module(library(lists), [member/2]).

test_pane_layouts :-
    run_tests([ pane_layouts_kinds,
                pane_layouts_defaults,
                pane_layouts_reading,
                pane_layouts_ranking
              ]).

                 /*******************************
                 *            KINDS             *
                 *******************************/

:- begin_tests(pane_layouts_kinds).

test(the_kinds_of_an_arrangement_are_a_sorted_set,
     Kinds == [editor, prolog_navigator, terminal]) :-
    arrangement_kinds(
        pane_frame([], [tab([], horizontal([0.2-prolog_navigator,
                                            0.8-vertical([0.7-editor,
                                                          0.3-terminal])]))]),
        Kinds).

test(a_kind_that_carries_options_is_still_that_kind,
     Kinds == [editor, terminal]) :-
    arrangement_kinds(
        pane_frame([], [tab([], vertical([0.7-editor([file('x.pl')]),
                                          0.3-terminal([profile(prolog)])]))]),
        Kinds).

test(the_kinds_of_every_tab_are_counted_once,
     Kinds == [editor, terminal]) :-
    arrangement_kinds(
        pane_frame([], [ tab([], editor),
                         tab([], vertical([0.5-editor, 0.5-terminal]))
                       ]),
        Kinds).

test(a_window_of_one_pane_has_one_kind, Kinds == [prolog_debugger]) :-
    arrangement_kinds(pane_frame([], [tab([], prolog_debugger)]), Kinds).

:- end_tests(pane_layouts_kinds).

                 /*******************************
                 *        WHAT IT SHIPS         *
                 *******************************/

/* The arrangements the system comes with say the things the old single
   setting could not: which edge, of what, and how big.
*/

:- begin_tests(pane_layouts_defaults).

test(a_navigator_goes_down_the_left_of_everything,
     Rule = split(Kinds, left, Share)) :-
    pane_placement(prolog_navigator, [editor, terminal], Rule),
    Rule = split(Kinds, left, Share),
    assertion(Kinds == [editor, terminal]),
    assertion(abs(Share-0.2) < 0.001).

test(a_terminal_goes_below_the_editor,
     Rule = split([editor], below, Share)) :-
    pane_placement(terminal, [editor], Rule),
    Rule = split(_, _, Share),
    assertion(abs(Share-0.3) < 0.001).

test(and_an_editor_above_the_terminal,
     Rule = split([terminal], above, Share)) :-
    pane_placement(editor, [terminal], Rule),
    Rule = split(_, _, Share),
    assertion(abs(Share-0.7) < 0.001).

test(a_debugger_gets_a_window_of_its_own, Rule == window) :-
    pane_placement(prolog_debugger, [editor, terminal], Rule).

%       A kind nothing has been arranged with has no answer here, and the
%       caller falls back on the setting -- which is what the IDE has
%       always done with it.

test(a_kind_nothing_says_anything_about_has_no_answer, fail) :-
    pane_placement(prolog_thread_monitor, [editor, terminal], _).

test(and_a_window_holding_nothing_has_none_either, fail) :-
    pane_placement(prolog_navigator, [], _).

:- end_tests(pane_layouts_defaults).

                 /*******************************
                 *           READING            *
                 *******************************/

/* Reading a rule out of one arrangement: which edge, of which panes, at
   what share.  These use arrangements of their own, added through the
   hook the library declares for a user's init file or a project.
*/

:- multifile pane_layouts:default_arrangement/1.

pane_layouts:default_arrangement(
    pane_frame([], [tab([], horizontal([1-tl_left,
                                        2-vertical([1-tl_top,
                                                    1-tl_bottom])]))])).
pane_layouts:default_arrangement(
    pane_frame([], [ tab([], tl_alone),
                     tab([], tl_left)
                   ])).
pane_layouts:default_arrangement(
    pane_frame([], [tab([], tl_only)])).

:- begin_tests(pane_layouts_reading).

%       Shares are relative: 1 and 2 in a row mean a third and two
%       thirds.

test(a_share_is_its_part_of_the_row, true(Share =:= 1/3)) :-
    pane_placement(tl_left, [tl_top, tl_bottom], split(_, _, Share)).

test(the_neighbour_is_the_subtree_beside_it,
     Kinds == [tl_bottom, tl_top]) :-
    pane_placement(tl_left, [tl_top, tl_bottom], split(Kinds, _, _)).

test(and_only_the_panes_of_it_that_are_there, Kinds == [tl_top]) :-
    pane_placement(tl_left, [tl_top], split(Kinds, _, _)).

test(a_pane_before_its_neighbour_goes_on_the_near_side, Side == left) :-
    pane_placement(tl_left, [tl_top], split(_, Side, _)).

test(and_one_after_it_on_the_far_side, Side == right) :-
    pane_placement(tl_top, [tl_left], split(_, Side, _)).

test(panes_stacked_are_read_as_above_and_below, Side == below) :-
    pane_placement(tl_bottom, [tl_top], split(_, Side, _)).

test(a_pane_that_is_a_tab_of_its_own_asks_for_a_tab, Rule == tab) :-
    pane_placement(tl_alone, [tl_left], Rule).

test(a_pane_that_is_a_whole_window_asks_for_a_window, Rule == window) :-
    pane_placement(tl_only, [tl_left], Rule).

%       An arrangement that names none of the panes the window holds says
%       nothing about it.

test(an_arrangement_with_no_neighbour_in_view_is_passed_over, fail) :-
    pane_placement(tl_left, [tl_nothing_like_it], _).

:- end_tests(pane_layouts_reading).

                 /*******************************
                 *           RANKING            *
                 *******************************/

/* Which arrangement is read when several hold the kind that is wanted:
   the one that looks most like the window as it will be.
*/

pane_layouts:default_arrangement(
    pane_frame([], [tab([], horizontal([1-tr_pane, 1-tr_one]))])).
pane_layouts:default_arrangement(
    pane_frame([], [tab([], vertical([1-tr_two, 1-tr_pane]))])).

:- begin_tests(pane_layouts_ranking).

%       Two arrangements hold tr_pane.  With tr_two in the window the
%       second is exactly that window and is the one read; with tr_one it
%       is the first.

test(the_arrangement_that_is_this_window_is_the_one_read,
     Kinds-Side == [tr_two]-below) :-
    pane_placement(tr_pane, [tr_two], split(Kinds, Side, _)).

test(and_the_other_window_reads_the_other, Kinds-Side == [tr_one]-left) :-
    pane_placement(tr_pane, [tr_one], split(Kinds, Side, _)).

%       Holding both, the exact match beats the overlap: with tr_one and
%       tr_two on the screen neither arrangement is exact, and the one
%       that shares as much and adds as little scores the same, so what
%       matters is that an answer comes out at all and names a pane that
%       is there.

test(with_both_in_view_an_answer_still_names_a_pane_that_is_there) :-
    pane_placement(tr_pane, [tr_one, tr_two], split(Kinds, _, _)),
    assertion(( member(K, Kinds), member(K, [tr_one, tr_two]) )).

:- end_tests(pane_layouts_ranking).
