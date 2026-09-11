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


:- module(log_store,
          [ with_log_store/2,           % +File, :Goal
            read_log_store/3,           % +File, :OnRecord, -Count
            append_log_store/3,         % +File, :Header, +Record
            rewrite_log_store/3         % +File, :Header, +Records
          ]).
:- use_module(library(lists), [member/2]).
:- use_module(library(filesex), [make_directory_path/1]).

/** <module> A file of Prolog terms kept as an append-only log

Several instances of the IDE run at once, and each of them learns things
the user would like to keep: how windows have been arranged, where the
bookmarks are.  A file written out in full when a session ends cannot
hold that, as the last instance to leave overwrites what the others
learned, and a session that never gets to leave writes nothing at all.

A _log store_ is such a file kept the other way round: what has just
happened is appended to it there and then, one term to a line, and the
state is what playing the log back leaves.  Nothing waits for the end of
the session, so instances running side by side each add what they have
without treading on the others, and a hard crash costs at most what was
on the screen at the time.

A log that has grown long is rewritten as a summary -- the same state in
as few records as say it -- and playing *that* back gives the same state
again.  When to do so, and what a summary is, belong to whoever keeps the
log; this module holds the file, the lock and the terms.

Everything goes through a lock file beside the store, so that two
instances appending, or one summarising while another reads, cannot tread
on each other.  The log is read whenever it is asked for rather than held
over the session, which is what lets one instance pick up what another
has just written.

@see library(pane_layouts) and library(emacs/bookmark_store) keep their
     state this way.
*/

:- meta_predicate
    with_log_store(+, 1),
    read_log_store(+, 1, -),
    append_log_store(+, 1, +),
    rewrite_log_store(+, 1, +).

:- dynamic
    complained/1.                       % what has been warned about

%!  with_log_store(+File, :Goal) is det.
%
%   Run call(Goal, File) with the store to ourselves.  The lock is on a
%   file beside it rather than on the store, because the store is renamed
%   over when it is summarised while the lock file stays what it is.
%
%   A store that cannot be read or written leaves the caller with
%   whatever state it had; it is said once and not again.

with_log_store(File, Goal) :-
    catch(ignore(locked_store(File, Goal)), Error,
          complain(no_file(File, Error))).

locked_store(File, Goal) :-
    file_directory_name(File, Dir),
    make_directory_path(Dir),
    atom_concat(File, '.lock', Lock),
    setup_call_cleanup(
        open(Lock, append, Stream, [lock(exclusive)]),
        call(Goal, File),
        close(Stream)).

complain(Message) :-
    (   complained(Message)
    ->  true
    ;   assertz(complained(Message)),
        print_message(warning, log_store(Message))
    ).

%!  read_log_store(+File, :OnRecord, -Count) is det.
%
%   Play the log back: call(OnRecord, Term) for each term in File, in the
%   order they were written.  Count is how many there were, which is what
%   says whether the log has grown long enough to summarise.  A term
%   OnRecord fails on is one that meant nothing to the caller; it costs
%   itself and no more.

read_log_store(File, OnRecord, Count) :-
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        read_records(In, OnRecord, 0, Count),
        close(In)).

read_records(In, OnRecord, Count0, Count) :-
    read_term(In, Term, []),
    (   Term == end_of_file
    ->  Count = Count0
    ;   ignore(call(OnRecord, Term)),
        Count1 is Count0+1,
        read_records(In, OnRecord, Count1, Count)
    ).

%!  append_log_store(+File, :Header, +Record) is det.
%
%   Add Record to the log.  call(Header, Stream) writes whatever should
%   stand at the head of the file, and is called only when the file is
%   made.

append_log_store(File, Header, Record) :-
    (   exists_file(File)
    ->  Fresh = false
    ;   Fresh = true
    ),
    setup_call_cleanup(
        open(File, append, Out, [encoding(utf8)]),
        (   (   Fresh == true
            ->  call(Header, Out)
            ;   true
            ),
            write_record(Out, Record)
        ),
        close(Out)).

%!  rewrite_log_store(+File, :Header, +Records) is det.
%
%   Replace the log with Records.  Written beside the log and renamed over
%   it, so that the log is either the old one or the new one and never
%   half of either.  Call it holding the lock, so that nobody has the old
%   one open to append to.

rewrite_log_store(File, Header, Records) :-
    atom_concat(File, '.new', New),
    setup_call_cleanup(
        open(New, write, Out, [encoding(utf8)]),
        (   call(Header, Out),
            forall(member(Record, Records),
                   write_record(Out, Record))
        ),
        close(Out)),
    rename_file(New, File).

write_record(Out, Record) :-
    format(Out, '~q.~n', [Record]).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(log_store(no_file(File, Error))) -->
    [ 'Cannot use ~w: ~p'-[File, Error] ].
