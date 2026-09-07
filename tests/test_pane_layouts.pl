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
:- use_module(library(apply), [maplist/3]).
:- use_module(library(readutil), [read_file_to_terms/3]).

%       An arrangements file of their own: these must neither read nor
%       write the arrangements of whoever runs them.

:- multifile pane_layouts:arrangements_file/1.

pane_layouts:arrangements_file(File) :-
    current_prolog_flag(tmp_dir, Tmp),
    atom_concat(Tmp, '/test_pane_layouts_store', File).

test_pane_layouts :-
    run_tests([ pane_layouts_kinds,
                pane_layouts_defaults,
                pane_layouts_reading,
                pane_layouts_ranking,
                pane_layouts_stripping,
                pane_layouts_learning,
                pane_layouts_log
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


                 /*******************************
                 *          STRIPPING           *
                 *******************************/

/* An arrangement is what a window says of itself with the content taken
   out: the kinds of pane, how they are tiled and their share of the room.
*/

:- begin_tests(pane_layouts_stripping).

test(the_content_of_a_pane_is_left_out,
     Arrangement == pane_frame([], [tab([], vertical([0.7-editor,
                                                      0.3-terminal]))])) :-
    arrangement_of(
        pane_frame([name(main)],
                   [tab([label('foo.pl'), current(true)],
                        vertical([0.7-current(editor([file('foo.pl'),
                                                      line(120)])),
                                  0.3-terminal([profile(shell)])]))]),
        Arrangement).

test(how_big_the_window_was_is_kept,
     Options == [geometry('1200x800+40+40')]) :-
    arrangement_of(pane_frame([name(main), geometry('1200x800+40+40')],
                              [tab([], editor)]),
                   pane_frame(Options, _)).

%       A share is rounded, so that pulling a pane an inch wider is the
%       same arrangement rather than a new one, and clamped, because a
%       pane dragged nearly shut is not an arrangement worth learning.

test(shares_are_rounded, Shares == [0.7, 0.3]) :-
    arrangement_of(pane_frame([], [tab([], vertical([0.7003-editor,
                                                     0.2997-terminal]))]),
                   pane_frame(_, [tab(_, vertical(Pairs))])),
    findall(S, member(S-_, Pairs), Shares).

test(and_clamped, Shares == [0.95, 0.05]) :-
    arrangement_of(pane_frame([], [tab([], vertical([0.99-editor,
                                                     0.01-terminal]))]),
                   pane_frame(_, [tab(_, vertical(Pairs))])),
    findall(S, member(S-_, Pairs), Shares).

:- end_tests(pane_layouts_stripping).

                 /*******************************
                 *           LEARNING           *
                 *******************************/

/* What an arrangement earns is the time it is lived in, and it decays.
   These write to a file of their own: the hook says where, so that they
   leave the arrangements of whoever runs them alone.
*/

%       Two arrangements to learn, differing only in which way the
%       navigator sits beside the terminal.  They are named here rather
%       than inside a unit, because both units below use them.

right(pane_frame([], [tab([], horizontal([0.8-terminal,
                                          0.2-prolog_navigator]))])).
below(pane_frame([], [tab([], vertical([0.8-terminal,
                                        0.2-prolog_navigator]))])).

:- begin_tests(pane_layouts_learning,
               [ setup(forget_arrangements),
                 cleanup(forget_arrangements)
               ]).

test(an_arrangement_lived_in_is_learned, Side == right) :-
    right(A),
    record_arrangement(A, 7200),
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)),
    forget_arrangements.

%       The steps on the way to an arrangement -- merge the tab back in,
%       drag the pane across, pull it to the width it should have -- last
%       seconds each.  Only what is then worked in is learned.

test(and_one_that_lasted_seconds_is_not, Side == left) :-
    right(A),
    too_short(Short),
    record_arrangement(A, Short),
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)),
    forget_arrangements.

test(the_one_lived_in_longest_wins, Side == below) :-
    right(A), below(B),
    record_arrangement(A, 600),
    record_arrangement(B, 7200),
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)),
    forget_arrangements.

%       An arrangement returned to keeps what it earned before, so a habit
%       builds up rather than being replaced each time.

test(what_an_arrangement_earns_adds_up, Side == right) :-
    right(A), below(B),
    record_arrangement(A, 4000),
    record_arrangement(B, 7000),
    record_arrangement(A, 4000),
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)),
    forget_arrangements.

%       And it fades: the same time spent a year ago counts for almost
%       nothing beside ten minutes yesterday.  The half-life is thirty
%       days.

test(what_was_earned_long_ago_has_faded, Side == below) :-
    right(A), below(B),
    record_arrangement(A, 100000),
    age_arrangements(365*24*3600),
    record_arrangement(B, 600),
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)),
    forget_arrangements.

%       Pulling a pane a little wider is the same arrangement, not a new
%       one, so what it has earned is not split between the two.

test(the_same_shape_at_another_size_is_the_same_arrangement, Shares == 1) :-
    right(A),
    A = pane_frame(_, [tab(_, horizontal([S1-T, _-N]))]),
    Wider = pane_frame([], [tab([], horizontal([S1-T, 0.3-N]))]),
    record_arrangement(A, 600),
    record_arrangement(Wider, 600),
    findall(x, pane_layouts:stored(_, _, _, _), Xs),
    length(Xs, Shares),
    forget_arrangements.

test(forgetting_puts_back_the_way_it_comes, Side == left) :-
    right(A),
    record_arrangement(A, 7200),
    forget_arrangements,
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)).

:- end_tests(pane_layouts_learning).

                 /*******************************
                 *            THE LOG           *
                 *******************************/

/* What is learned is appended to the log as it is learned, rather than
   written out at the end of the session: two instances of the IDE running
   at once must both be able to add to it, and a hard crash must not cost
   what was learned before it.
*/

:- begin_tests(pane_layouts_log,
               [ setup(forget_arrangements),
                 cleanup(forget_arrangements)
               ]).

test(what_is_learned_is_written_down_at_once, Records == 2) :-
    right(A), below(B),
    forget_arrangements,
    record_arrangement(A, 600),
    record_arrangement(B, 600),
    log_records(Log),
    length(Log, Records).

test(and_nothing_that_barely_lasted_is, Log == []) :-
    right(A),
    forget_arrangements,
    too_short(Short),
    record_arrangement(A, Short),
    log_records(Log).

%       Written by the other instance of the IDE while this one was
%       running: the store is read from the log when it is asked for, so
%       there is nothing to restart.

test(what_another_instance_wrote_is_picked_up, Side == right) :-
    right(A),
    get_time(Now),
    write_log([used(A, 7200, Now)]),
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)).

%       A log that has grown long says the same in one record per
%       arrangement: a record and a summary both say what an arrangement
%       was worth at the moment they were written.

test(a_long_log_is_summarised, [Records, Side] == [1, right]) :-
    right(A),
    get_time(Now),
    findall(used(A, 60, Now), between(1, 210, _), Log),
    write_log(Log),
    record_arrangement(A, 600),
    log_records(Summary),
    length(Summary, Records),
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)).

test(and_keeps_what_was_learned, true(Seconds > 13000)) :-
    right(A),
    get_time(Now),
    findall(used(A, 60, Now), between(1, 210, _), Log),
    write_log(Log),
    record_arrangement(A, 600),
    log_records([used(_, Seconds, _)]).

%       A record from a later version, or a hand-edited file gone wrong,
%       costs its own record and no more.

test(a_record_that_means_nothing_costs_itself, Side == right) :-
    right(A),
    get_time(Now),
    assertz(pane_layouts:complained(unknown_term(nonsense))),
    write_log([nonsense, used(A, 7200, Now)]),
    pane_placement(prolog_navigator, [terminal], split(_, Side, _)).

test(forgetting_empties_the_log, Log == []) :-
    right(A),
    record_arrangement(A, 600),
    forget_arrangements,
    log_records(Log).

:- end_tests(pane_layouts_log).

%!  age_arrangements(+Seconds) is det.
%
%   Pretend everything learned so far was learned that long ago, by
%   putting the clock of the log itself back.  The store is read from the
%   log whenever it is asked for, so this is all it takes.

age_arrangements(Expr) :-
    Seconds is Expr,
    log_records(Records),
    maplist(age_record(Seconds), Records, Aged),
    write_log(Aged).

age_record(Seconds, used(A, S, At), used(A, S, Then)) :-
    Then is At-Seconds.

%!  too_short(-Seconds) is det.
%
%   A time too short to be credited, whatever the least that is ever
%   credited has been set to.

too_short(Seconds) :-
    pane_layouts:worth_recording(Least),
    Seconds is Least/2.

%!  log_records(-Records) is det.
%!  write_log(+Records) is det.
%
%   The log as it stands, and the log as another instance of the IDE --
%   or the user with an editor -- might leave it.

log_records(Records) :-
    pane_layouts:store_file(File),
    (   exists_file(File)
    ->  read_file_to_terms(File, Records, [])
    ;   Records = []
    ).

write_log(Records) :-
    pane_layouts:store_file(File),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        forall(member(Record, Records),
               format(Out, '~q.~n', [Record])),
        close(Out)).
