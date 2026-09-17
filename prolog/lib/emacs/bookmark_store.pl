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


:- module(emacs_bookmark_store,
          [ bookmark_store_load/1,      % -Bookmarks
            bookmark_store_save/1,      % +Bookmark
            bookmark_store_forget/1,    % +Id
            bookmark_store_tidy/0,
            bookmark_store_id/1,        % -Id
            bookmark_store_file/1       % -File (semidet)
          ]).
:- use_module(library(pairs), [pairs_values/2]).
:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(random), [random_between/3]).
:- use_module(library(log_store),
              [ with_log_store/2, read_log_store/3,
                append_log_store/3, rewrite_log_store/3
              ]).

/** <module> Where the bookmarks of PceEmacs are kept

The bookmarks live in `xpce/emacs_bookmarks' of the config directory,
kept as a log -- see library(log_store).  A bookmark that is made,
annotated, moved or thrown away is written there and then, so that
several instances of PceEmacs running at once each add what the user
did in them instead of the last one to leave overwriting the rest.

A bookmark is the term

    bookmark(Id, File, Line, LinePos, Length, Title, Stamp, Note)

with Id naming it for as long as it lasts.  A later record for the same
Id is the bookmark as it now stands, and `deleted(Id)' is the bookmark
thrown away; the state is what playing the log back leaves.  A log that
has grown long is rewritten as one record per bookmark, which says the
same thing.

Older versions wrote the whole tree out at the end of a session as
`bookmark/7' (or `bookmark/5' before that), with nothing to name a
bookmark by.  Those are read as they stand and given an Id made from the
file and the moment they were created, so that reading the same file
twice names them the same way; they are written out as `bookmark/8' the
first time the log is tidied.

@see library(emacs/bookmarks) for the tool over this.
*/

:- dynamic
    stored/3,                           % Seq, Id, Bookmark
    events/1.                           % records in the log as last read

max_events(500).                        % a longer log is tidied

%!  bookmarks_file(-File) is nondet.
%
%   Hook.  Where the bookmarks are kept.  The first clause wins, so a
%   project that wants bookmarks of its own -- or a test that must not
%   touch the user's -- says so:
%
%   ```
%   :- multifile emacs_bookmark_store:bookmarks_file/1.
%   emacs_bookmark_store:bookmarks_file('/path/of/my/project/bookmarks').
%   ```

:- multifile
    bookmarks_file/1.                   % -File

%!  bookmark_store_file(-File) is semidet.
%
%   The file the bookmarks are kept in, whether or not it exists yet.
%   Fails if there is no config directory, for example because `HOME`
%   does not exist.  The bookmarks then live as long as the process.

bookmark_store_file(File) :-
    bookmarks_file(File),
    !.
bookmark_store_file(File) :-
    absolute_file_name(user_app_config('xpce/emacs_bookmarks'), File,
                       [ access(none), solutions(first), file_errors(fail) ]).

%!  bookmark_store_load(-Bookmarks) is det.
%
%   All the bookmarks there are, in the order they were first written.
%   A file that cannot be read leaves the tool with no bookmarks, which
%   is a working state.

bookmark_store_load(Bookmarks) :-
    (   bookmark_store_file(File),
        exists_file(File)
    ->  with_log_store(File, read_log)
    ;   clear_store
    ),
    live_bookmarks(Bookmarks).

live_bookmarks(Bookmarks) :-
    findall(Seq-BM, stored(Seq, _Id, BM), Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Bookmarks).

clear_store :-
    retractall(stored(_,_,_)),
    retractall(events(_)),
    assertz(events(0)).

read_log(File) :-
    clear_store,
    read_log_store(File, replay, N),
    retractall(events(_)),
    assertz(events(N)).

%       A record that means nothing to us costs itself and no more: a log
%       written by a later version, or edited by hand into something
%       else, still gives up everything else it holds.

replay(bookmark(Id, File, Line, LinePos, Length, Title, Stamp, Note)) =>
    keep(bookmark(Id, File, Line, LinePos, Length, Title, Stamp, Note)).
replay(bookmark(File, Line, LinePos, Length, Title, Stamp, Note)) =>
    legacy_id(File, Stamp, Id),
    keep(bookmark(Id, File, Line, LinePos, Length, Title, Stamp, Note)).
replay(bookmark(File, Line, Title, Stamp, Note)) =>
    replay(bookmark(File, Line, 0, 0, Title, Stamp, Note)).
replay(deleted(Id)) =>
    retractall(stored(_, Id, _)).
replay(Term) =>
    print_message(warning, emacs_bookmark_store(unknown_term(Term))).

%       Keyed by the Id and kept where it first appeared: a bookmark that
%       has been annotated or has moved is the same bookmark, and the
%       tree it is read into should not shuffle under the user because a
%       note was typed into one of them.

keep(Bookmark) :-
    arg(1, Bookmark, Id),
    (   retract(stored(Seq, Id, _))
    ->  true
    ;   next_seq(Seq)
    ),
    assertz(stored(Seq, Id, Bookmark)).

next_seq(Seq) :-
    (   aggregate_all(max(S), stored(S, _, _), Highest)
    ->  Seq is Highest+1
    ;   Seq = 1
    ).

%!  legacy_id(+File, +Stamp, -Id) is det.
%
%   Name a bookmark from a file written before bookmarks had names.  The
%   file and the moment it was created tell nearly all of them apart, and
%   the ones they do not are counted off in the order they are read, so
%   that reading the same file twice names them the same way.

legacy_id(File, Stamp, Id) :-
    format(atom(Base), '~w@~0f', [File, Stamp]),
    unique_id(Base, 1, Id).

unique_id(Base, N, Id) :-
    (   N == 1
    ->  Candidate = Base
    ;   format(atom(Candidate), '~w#~d', [Base, N])
    ),
    (   stored(_, Candidate, _)
    ->  N1 is N+1,
        unique_id(Base, N1, Id)
    ;   Id = Candidate
    ).

%!  bookmark_store_id(-Id) is det.
%
%   A name for a new bookmark: the moment it was made and a random
%   number, which no other bookmark of this user's is going to carry.

bookmark_store_id(Id) :-
    get_time(Now),
    Millis is round(Now*1000),
    random_between(0, 0xffffff, Random),
    format(atom(Id), 'bm-~d-~d', [Millis, Random]).

%!  bookmark_store_save(+Bookmark) is det.
%!  bookmark_store_forget(+Id) is det.
%
%   Write down a bookmark as it now stands, or that it is gone.  Both
%   take effect at once: nothing waits for the end of the session.

bookmark_store_save(Bookmark) :-
    Bookmark = bookmark(_,_,_,_,_,_,_,_),
    add_record(Bookmark).

bookmark_store_forget(Id) :-
    add_record(deleted(Id)).

%       Under the lock: play back what the others have written since we
%       last looked, add ours, and either append it or -- if the log has
%       grown long -- write the whole store back as one record per
%       bookmark, which says the same thing.

add_record(Record) :-
    bookmark_store_file(File),
    !,
    with_log_store(File, add_record(Record)).
add_record(Record) :-
    replay(Record).

add_record(Record, File) :-
    (   exists_file(File)
    ->  read_log(File)
    ;   clear_store
    ),
    replay(Record),
    events(N),
    max_events(Max),
    (   N >= Max
    ->  summarise_log(File)
    ;   append_log_store(File, write_header, Record),
        retract(events(N)),
        N1 is N+1,
        assertz(events(N1))
    ).

%!  bookmark_store_tidy is det.
%
%   Write the log back as one record per bookmark, dropping everything
%   that has been thrown away along the way.

bookmark_store_tidy :-
    (   bookmark_store_file(File),
        exists_file(File)
    ->  with_log_store(File, tidy_log)
    ;   true
    ).

tidy_log(File) :-
    read_log(File),
    summarise_log(File).

summarise_log(File) :-
    live_bookmarks(Records),
    rewrite_log_store(File, write_header, Records),
    read_log(File).

write_header(Out) :-
    format(Out, '/*  The bookmarks of PceEmacs.~n~n', []),
    format(Out, '    A `bookmark\' record is a place you marked, as it~n', []),
    format(Out, '    stood when it was written; a later one naming the~n', []),
    format(Out, '    same bookmark takes its place.  A `deleted\' record~n', []),
    format(Out, '    is a bookmark you threw away.~n~n', []),
    format(Out, '    Edit it as you like.~n', []),
    format(Out, '*/~n~n', []).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(emacs_bookmark_store(unknown_term(Term))) -->
    [ 'Bookmarks: ignored ~p'-[Term] ].
