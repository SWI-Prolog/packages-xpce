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


:- module(test_bookmark_store, [test_bookmark_store/0]).
:- encoding(utf8).

/** <module> Tests for library(emacs/bookmark_store)

The bookmarks of PceEmacs, kept as an append-only log so that several
instances of the IDE do not overwrite each other's.  All of this is plain
Prolog over a file: no window is made and nothing here needs a display.

Run with:

    swipl -g test_bookmark_store -t halt \
          packages/xpce/tests/test_bookmark_store.pl
*/

:- use_module(library(plunit)).
:- use_module(library(emacs/bookmark_store)).
:- use_module(library(lists), [member/2, nth0/3]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(readutil), [read_file_to_terms/3]).

%       A bookmarks file of their own: these must neither read nor write
%       the bookmarks of whoever runs them.

:- multifile emacs_bookmark_store:bookmarks_file/1.

emacs_bookmark_store:bookmarks_file(File) :-
    current_prolog_flag(tmp_dir, Tmp),
    atom_concat(Tmp, '/test_bookmark_store', File).

test_bookmark_store :-
    run_tests([ bookmark_store_log,
                bookmark_store_sharing,
                bookmark_store_legacy,
                bookmark_store_tidying
              ]).

%!  empty_store is det.
%!  log_records(-Records) is det.
%!  write_log(+Records) is det.
%
%   The log as it stands, and the log as another instance of PceEmacs --
%   or the user with an editor -- might leave it.

empty_store :-
    bookmark_store_file(File),
    (   exists_file(File)
    ->  delete_file(File)
    ;   true
    ),
    bookmark_store_load(_).

log_records(Records) :-
    bookmark_store_file(File),
    (   exists_file(File)
    ->  read_file_to_terms(File, Records, [])
    ;   Records = []
    ).

write_log(Records) :-
    bookmark_store_file(File),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        forall(member(Record, Records),
               format(Out, '~q.~n', [Record])),
        close(Out)),
    bookmark_store_load(_).

%!  mark(+Id, -Bookmark) is det.

mark(Id, bookmark(Id, '/tmp/a.pl', 10, 0, 3, "foo(x)", 1750000000, "")).

mark(Id, Line, bookmark(Id, '/tmp/a.pl', Line, 0, 3, "foo(x)", 1750000000, "")).

ids(Bookmarks, Ids) :-
    maplist([BM,Id]>>arg(1, BM, Id), Bookmarks, Ids).

                 /*******************************
                 *            THE LOG           *
                 *******************************/

:- begin_tests(bookmark_store_log, [setup(empty_store)]).

test(a_bookmark_is_written_down_at_once, Ids == [one]) :-
    empty_store,
    mark(one, BM),
    bookmark_store_save(BM),
    bookmark_store_load(Marks),
    ids(Marks, Ids).

test(and_read_back_as_it_was, Read == BM) :-
    empty_store,
    mark(one, BM),
    bookmark_store_save(BM),
    bookmark_store_load([Read]).

test(a_bookmark_that_moves_takes_the_place_of_itself,
     [Ids, Lines] == [[one, two], [99, 20]]) :-
    empty_store,
    mark(one, 10, A), mark(two, 20, B), mark(one, 99, Moved),
    maplist(bookmark_store_save, [A, B, Moved]),
    bookmark_store_load(Marks),
    ids(Marks, Ids),
    maplist([M,L]>>arg(3, M, L), Marks, Lines).

test(a_bookmark_thrown_away_is_gone, Ids == [two]) :-
    empty_store,
    mark(one, A), mark(two, B),
    maplist(bookmark_store_save, [A, B]),
    bookmark_store_forget(one),
    bookmark_store_load(Marks),
    ids(Marks, Ids).

test(and_saying_so_twice_is_no_worse, Marks == []) :-
    empty_store,
    mark(one, A),
    bookmark_store_save(A),
    bookmark_store_forget(one),
    bookmark_store_forget(one),
    bookmark_store_load(Marks).

test(every_bookmark_is_named_apart, [true(A \== B)]) :-
    bookmark_store_id(A),
    bookmark_store_id(B).

:- end_tests(bookmark_store_log).

                 /*******************************
                 *          SIDE BY SIDE        *
                 *******************************/

/* Written by the other instance of PceEmacs while this one was running:
   the store is read from the log when it is asked for, so there is
   nothing to restart, and adding ours does not drop theirs.
*/

:- begin_tests(bookmark_store_sharing, [setup(empty_store)]).

test(what_another_instance_wrote_is_picked_up, Ids == [theirs]) :-
    empty_store,
    mark(theirs, Theirs),
    write_log([Theirs]),
    bookmark_store_load(Marks),
    ids(Marks, Ids).

test(and_is_not_overwritten_by_what_we_add, Ids == [theirs, ours]) :-
    empty_store,
    mark(theirs, Theirs), mark(ours, Ours),
    write_log([Theirs]),
    bookmark_store_save(Ours),
    bookmark_store_load(Marks),
    ids(Marks, Ids).

test(nor_by_what_we_throw_away, Ids == [theirs]) :-
    empty_store,
    mark(theirs, Theirs), mark(ours, Ours),
    bookmark_store_save(Ours),
    write_log([Theirs, Ours]),
    bookmark_store_forget(ours),
    bookmark_store_load(Marks),
    ids(Marks, Ids).

:- end_tests(bookmark_store_sharing).

                 /*******************************
                 *          WHAT WAS THERE      *
                 *******************************/

/* Files written before bookmarks had names: the whole tree, written out
   at the end of a session.
*/

:- begin_tests(bookmark_store_legacy, [setup(empty_store)]).

test(an_old_file_is_read_as_it_stands, [Lines, Notes] == [[10, 20], ["a", "b"]]) :-
    empty_store,
    write_log([ bookmark('/tmp/a.pl', 10, 0, 3, "foo(x)", 1750000000, "a"),
                bookmark('/tmp/b.pl', 20, 0, 3, "bar(y)", 1750000001, "b")
              ]),
    bookmark_store_load(Marks),
    maplist([M,L]>>arg(3, M, L), Marks, Lines),
    maplist([M,N]>>arg(8, M, N), Marks, Notes).

test(an_older_one_too, Lines == [10]) :-
    empty_store,
    write_log([bookmark('/tmp/a.pl', 10, "foo(x)", 1750000000, "a")]),
    bookmark_store_load(Marks),
    maplist([M,L]>>arg(3, M, L), Marks, Lines).

%       Nothing told them apart but the file and the second they were
%       made in, and two made in the same second are not the same
%       bookmark.

test(two_old_ones_made_in_the_same_second_both_survive, Lines == [10, 11]) :-
    empty_store,
    write_log([ bookmark('/tmp/a.pl', 10, 0, 3, "foo(x)", 1750000000, ""),
                bookmark('/tmp/a.pl', 11, 0, 3, "foo(y)", 1750000000, "")
              ]),
    bookmark_store_load(Marks),
    maplist([M,L]>>arg(3, M, L), Marks, Lines).

test(and_are_named_the_same_way_every_time, Ids == Again) :-
    empty_store,
    write_log([ bookmark('/tmp/a.pl', 10, 0, 3, "foo(x)", 1750000000, ""),
                bookmark('/tmp/a.pl', 11, 0, 3, "foo(y)", 1750000000, "")
              ]),
    bookmark_store_load(Marks),
    ids(Marks, Ids),
    bookmark_store_load(Marks2),
    ids(Marks2, Again).

%       An old file names its bookmarks the first time it is tidied, and
%       from then on they are named in the file itself.

test(tidying_an_old_file_names_them, Named == 2) :-
    empty_store,
    write_log([ bookmark('/tmp/a.pl', 10, 0, 3, "foo(x)", 1750000000, ""),
                bookmark('/tmp/b.pl', 20, 0, 3, "bar(y)", 1750000001, "")
              ]),
    bookmark_store_tidy,
    log_records(Records),
    aggregate_all(count, ( member(R, Records),
                           functor(R, bookmark, 8)
                         ), Named).

:- end_tests(bookmark_store_legacy).

                 /*******************************
                 *           TIDYING            *
                 *******************************/

:- begin_tests(bookmark_store_tidying, [setup(empty_store)]).

test(a_long_log_says_the_same_in_one_record_each,
     [Records, Ids] == [1, [one]]) :-
    empty_store,
    findall(BM, ( between(1, 600, N),
                  mark(one, N, BM)
                ), Log),
    write_log(Log),
    mark(one, 601, Last),
    bookmark_store_save(Last),
    log_records(Summary),
    length(Summary, Records),
    bookmark_store_load(Marks),
    ids(Marks, Ids).

test(tidying_drops_what_was_thrown_away, Records == []) :-
    empty_store,
    mark(one, A),
    bookmark_store_save(A),
    bookmark_store_forget(one),
    bookmark_store_tidy,
    log_records(Records).

test(a_record_that_means_nothing_costs_itself, Ids == [one]) :-
    empty_store,
    mark(one, A),
    write_log([nonsense, A]),
    bookmark_store_load(Marks),
    ids(Marks, Ids).

:- end_tests(bookmark_store_tidying).
