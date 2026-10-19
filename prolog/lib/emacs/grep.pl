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

:- module(emacs_grep, []).
:- use_module(library(pce)).
:- use_module(library(dcg/basics), [string//1, digits//1]).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(bookmarks, []).

/** <module> Show the hits of grep in a hit list

Runs grep, git grep or any command that prints lines as
=|File:Line:Text|=, and puts each line it prints as a bookmark in a hit
list, the pane that also shows the references found by
`find_references`.

If the command colours its output, such as `grep --color=always`, the
colour of the match in Text tells us where the match is, which is
highlighted in the hit list and in the editor.  Without colours, a hit
is the whole line.  Output that is not a hit, such as an error message,
is reported when the command is done.
*/

%   Lines longer than this are cut in the hit list.

max_title_length(500).

:- pce_begin_class(emacs_grep_process, process,
                   "Process whose output lines are hits in a hit list").

variable(hits,     int := 0,     get, "Number of hits found").
variable(messages, chain*,       get, "Output lines that are not hits").
variable(done,     bool := @off, get, "All output has been processed").
variable(reported, bool := @off, get, "The outcome has been reported").

%!  run(+P, +Editor, +Dir)
%
%   Run the command in Dir, adding the hits to Editor.  A command
%   still running for Editor is killed.

run(P, BM:emacs_bookmark_editor, Dir:directory) :->
    "Run, adding the hits to the hit list"::
    ignore(send(BM, send_hyper, grep_process, kill_if_running)),
    send(BM, forget_report),            % the outcome of the previous run
    send(BM, delete_hypers, grep_process),
    new(_, hyper(BM, P, grep_process, editor)),
    send(P, slot, messages, new(chain)),
    send(P, directory, Dir),
    send(P, input_message, message(P, hit_line, @arg1)),
    send(P, terminate_message, message(P, report_outcome)),
    send(P, open),
    send(BM, open),
    send(BM, report, progress, 'Searching ...').

%   Errors of the process, e.g., that the command cannot be started,
%   are reported in the editor rather than on the console.

report_to(P, BM:emacs_bookmark_editor) :<-
    "Report to the hit list"::
    get(P, hypered, editor, BM).

%   A non-zero exit status need not be an error: grep and git grep exit
%   with status 1 if nothing matches.  The status is part of the outcome
%   reported by ->report_outcome.

error(P, Error:error, Context:any ...) :->
    "Ignore the exit status: ->report_outcome reports it"::
    (   get(Error, id, process_exit_status)
    ->  true
    ;   Msg =.. [error, Error|Context],
        send_super(P, Msg)
    ).

kill_if_running(P) :->
    "Kill me if I am running"::
    (   get(P, status, running)
    ->  ignore(send(P, kill))
    ;   true
    ).

hit_line(P, Line:string) :->
    "Add Line to the hit list, if it is a hit"::
    (   get(P, hypered, editor, BM)
    ->  get(Line, value, Text0),
        (   grep_line(Text0, File, LineNo, Pos, Len, Text)
        ->  get(P, directory, Dir),
            get(Dir, path, DirPath),
            add_hit(BM, DirPath, File, LineNo, Pos, Len, Text),
            get(P, hits, Hits0),
            Hits is Hits0+1,
            send(P, slot, hits, Hits)
        ;   strip_line(Text0, Plain, _),
            Plain \== ""
        ->  send(P?messages, append, string(Plain))
        ;   true
        )
    ;   send(P, kill_if_running)          % my hit list is gone
    ).

end_of_file(P) :->
    "Sort the hits and report"::
    send_super(P, end_of_file),
    send(P, slot, done, @on),
    (   get(P, hypered, editor, BM)
    ->  send(BM, sort)
    ;   true
    ),
    send(P, report_outcome).

%!  report_outcome(+P)
%
%   Report the outcome if all output is processed and the process has
%   exited, which may happen in either order.  If there are no hits, the
%   first line of output that is not a hit is the most useful message,
%   e.g., git's "fatal: ..." line.  An exit status other than 0 or 1
%   (nothing matches) without such a line is an error of its own.

report_outcome(P) :->
    "Report the hits or the error"::
    (   get(P, done, @on),
        get(P, status, exited),
        get(P, reported, @off),
        get(P, hypered, editor, BM)
    ->  send(P, slot, reported, @on),
        get(P, hits, Hits),
        get(P, messages, Messages),
        get(P, code, Code),
        (   Hits > 0
        ->  send(BM, report, status, 'Found %d matches', Hits)
        ;   get(Messages, head, First)
        ->  send(BM, report, warning, '%s', First)
        ;   integer(Code), Code > 1
        ->  send(BM, report, warning, 'Exit status %d', Code)
        ;   send(BM, report, status, 'No matches')
        )
    ;   true
    ).

:- pce_end_class(emacs_grep_process).

%!  add_hit(+BM, +Dir, +File, +LineNo, +Pos, +Len, +Text) is det.
%
%   Add a bookmark for a hit  to  BM.   The  bookmarks  are not sorted;
%   `emacs_grep_process ->end_of_file` sorts them all at once.

add_hit(BM, Dir, File, LineNo, Pos, Len, Text) :-
    absolute_file_name(File, Path, [relative_to(Dir)]),
    new(Title, string('%s', Text)),
    send(Title, translate, '\t', ' '),
    new(Mark, emacs_bookmark(Path, LineNo, Pos, Len, Title)),
    send(BM, bookmark, Mark, @off),
    (   object(@emacs),
        get(@emacs, file_buffer, Path, Buffer)
    ->  send(Mark, link, Buffer)
    ;   true
    ).

%!  grep_line(+Line, -File, -LineNo, -Pos, -Len, -Text) is semidet.
%
%   True when Line is a line =|File:LineNo:Text|= as printed by grep
%   -n.  Pos and Len are the start and length  of the first coloured
%   part of Text, the match, as  marked by ANSI escape sequences.  If
%   Text has no colours, Pos is 0 and Len is `@default`, which makes
%   the whole line the hit.  Text has the escape sequences removed.

grep_line(Line, File, LineNo, Pos, Len, Text) :-
    strip_line(Line, Plain, Marked),
    string_codes(Plain, Codes),
    phrase(hit(FileCodes, LineNo), Codes, TextCodes),
    atom_codes(File, FileCodes),
    length(Codes, PlainLen),
    length(TextCodes, TextLen),
    TextStart is PlainLen-TextLen,
    max_title_length(Max),
    (   TextLen > Max
    ->  length(TitleCodes, Max),
        append(TitleCodes, _, TextCodes),
        TitleLen = Max
    ;   TitleCodes = TextCodes,
        TitleLen = TextLen
    ),
    string_codes(Text, TitleCodes),
    (   member(S-E, Marked),
        S >= TextStart,
        E-TextStart =< TitleLen
    ->  Pos is S-TextStart,
        Len is E-S
    ;   Pos = 0,
        Len = @default
    ).

hit(File, LineNo) -->
    string(File),
    { File \== [] },
    ":",
    digits(Digits),
    { Digits \== [] },
    ":",
    !,
    { number_codes(LineNo, Digits) }.

%!  strip_line(+Line, -Plain, -Marked) is det.
%
%   Plain is Line without the trailing newline and ANSI escape sequences.
%   Marked is a list of Start-End character ranges of Plain that are
%   coloured.

strip_line(Line, Plain, Marked) :-
    string_codes(Line, Codes0),
    strip_newline(Codes0, Codes),
    plain_codes(Codes, 0, plain, PlainCodes, Marked),
    string_codes(Plain, PlainCodes).

strip_newline(Codes0, Codes) :-
    append(Codes1, [C], Codes0),
    (   C == 0'\n
    ;   C == 0'\r
    ),
    !,
    strip_newline(Codes1, Codes).
strip_newline(Codes, Codes).

%   plain_codes(+Codes, +Index, +State, -Plain, -Marked)
%
%   State is `plain` or colour(Start), where Start is the index in
%   Plain where the current colour started.

plain_codes([], I, State, [], Marked) :-
    end_colour(State, I, Marked, []).
plain_codes([0'\e, 0'[|T0], I, State0, Plain, Marked) :-
    csi(T0, Params, Final, T),
    !,
    (   Final == 0'm
    ->  sgr_colour(Params, Colour),
        colour_state(Colour, State0, I, State, Marked, Marked1)
    ;   State = State0,
        Marked = Marked1
    ),
    plain_codes(T, I, State, Plain, Marked1).
plain_codes([C|T], I, State, [C|Plain], Marked) :-
    I1 is I+1,
    plain_codes(T, I1, State, Plain, Marked).

%   A control sequence is ESC [, parameter bytes and a final byte in
%   the range @..~.

csi(Codes, Params, Final, Rest) :-
    append(Params, [Final|Rest], Codes),
    Final >= 0'@, Final =< 0'~,
    !,
    forall(member(C, Params), csi_param(C)).

csi_param(C) :- code_type(C, digit), !.
csi_param(0';).

%   SGR without parameters or with only 0 resets the colour.

sgr_colour(Params, Colour) :-
    (   (   Params == []
        ;   forall(member(C, Params), C == 0'0)
        )
    ->  Colour = false
    ;   Colour = true
    ).

colour_state(true,  plain,     I, colour(I), M, M).
colour_state(true,  colour(S), _, colour(S), M, M).
colour_state(false, plain,     _, plain,     M, M).
colour_state(false, colour(S), I, plain,     M0, M) :-
    end_colour(colour(S), I, M0, M).

end_colour(colour(S), I, [S-I|M], M) :-
    I > S,
    !.
end_colour(_, _, M, M).
