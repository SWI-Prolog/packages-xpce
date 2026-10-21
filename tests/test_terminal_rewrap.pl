/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(test_terminal_rewrap,
          [ test_terminal_rewrap/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).

/** <module> Test rewrapping the terminal buffer on a resize

Changing the width of the terminal rewraps every line it holds, which
needs more lines of the ring than it had when the window narrows.  The
ring has a fixed size, so the oldest text scrolls off to pay for them.

The case this pins is the one where there is nothing left to pay with:
the buffer is full and the line to be wrapped is the oldest one there
is.  Making room by dropping the oldest line would drop the very line
being wrapped, which used to leave the wrap copying from a line that no
longer held its text -- a segmentation fault on narrowing a full console
(https://swi-prolog.discourse.group/t/9090/170).
*/

test_terminal_rewrap :-
    run_tests([ terminal_rewrap,
                terminal_save_lines
              ]).

                 /*******************************
                 *            HARNESS           *
                 *******************************/

%!  terminal(+SaveLines, -TI) is det.
%
%   A terminal image with a scrollback of SaveLines lines, created with
%   a ring of that size rather than resized to it using ->save_lines.

terminal(SaveLines, TI) :-
    get(@pce, convert, terminal_image, class, Class),
    get(Class, class_variable, save_lines, CV),
    send(CV, value, SaveLines),
    new(TI, terminal_image(1000, 400)).     % pixels

%!  fill(+TI, +Count, +Len) is det.
%
%   Write Count hard lines of Len characters, each starting with its own
%   number, so that a line can be told apart from its neighbours.

fill(TI, Count, Len) :-
    fill(TI, 1, Count, Len).

fill(TI, From, To, Len) :-
    forall(between(From, To, I),
           ( line_text(I, Len, Text),
             atom_concat(Text, '\r\n', Line),
             send(TI, insert, Line)
           )).

line_text(I, Len, Text) :-
    format(atom(Nr), '~w', [I]),
    atom_length(Nr, NrLen),
    Pad is Len-NrLen,
    length(Codes, Pad),
    maplist(=(0'x), Codes),
    atom_codes(Filler, Codes),
    atom_concat(Nr, Filler, Text).

%!  visible(+TI, -Text) is det.
%
%   The text of the visible rows, joined.  Wrapping a line splits it
%   over rows, so the join is what the buffer holds regardless of the
%   width it is shown at.

visible(TI, Text) :-
    get(TI, rows, Rows),
    Last is Rows-1,
    findall(Row,
            ( between(0, Last, I),
              ( get(TI, row, I, Str)
              -> get(Str, value, Row)
              ;  Row = ''
              )
            ),
            Lines),
    atomic_list_concat(Lines, Text).

%!  ends_with_lines(+TI, +Numbers, +Len) is semidet.
%
%   The visible text ends with the lines Numbers, in order.

ends_with_lines(TI, Numbers, Len) :-
    visible(TI, Text),
    findall(L, (member(I, Numbers), line_text(I, Len, L)), Lines),
    atomic_list_concat(Lines, Tail),
    atom_concat(_, Tail, Text).

                 /*******************************
                 *            TESTS             *
                 *******************************/

:- begin_tests(terminal_rewrap).

% Narrowing a full buffer.  The lines are short enough to have been
% written without wrapping, so the oldest line of the ring is a hard one
% and narrowing must wrap it -- the case that has no line to wrap into.

test(narrow_full_buffer, [cleanup(free(TI))]) :-
    terminal(200, TI),
    fill(TI, 300, 60),
    get(TI, columns, Cols0),
    assertion(Cols0 > 60),                  % written without wrapping
    send(TI, width, 200),
    get(TI, columns, Cols),
    assertion(Cols < 60),                   % and wrapped now
    assertion(ends_with_lines(TI, [299,300], 60)).

% Again with lines that were wrapped as they were written, so the oldest
% line of the ring is a soft one.

test(narrow_full_buffer_soft, [cleanup(free(TI))]) :-
    terminal(200, TI),
    fill(TI, 300, 100),
    send(TI, width, 200),
    assertion(ends_with_lines(TI, [299,300], 100)).

% And the same when the ring still has free lines: there the wrap can
% take one of those and nothing is lost.

test(narrow_partial_buffer, [cleanup(free(TI))]) :-
    terminal(2000, TI),
    fill(TI, 100, 60),
    send(TI, width, 200),
    assertion(ends_with_lines(TI, [99,100], 60)).

% Narrowing and widening again over a full buffer.  The text that is
% still there must survive every rewrap unchanged.

test(round_trip, [cleanup(free(TI))]) :-
    terminal(200, TI),
    fill(TI, 300, 60),
    forall(member(W, [1000, 600, 300, 200, 300, 600, 1000]),
           ( send(TI, width, W),
             assertion(ends_with_lines(TI, [299,300], 60))
           )).

:- end_tests(terminal_rewrap).

/** <module> Test changing ->save_lines of a live terminal

Changing ->save_lines resizes the ring.  Growing it keeps everything,
shrinking it drops the oldest lines, and in both cases the terminal goes
on where it was: the caret, the selection and the window follow their
text.
*/

:- begin_tests(terminal_save_lines).

%!  numbers(+TI, -Numbers) is det.
%
%   The numbers of the lines the buffer holds, oldest first.

numbers(TI, Numbers) :-
    get(TI, contents, Str),
    get(Str, value, Text),
    split_string(Text, "\n", "", Lines),
    convlist([L,N]>>( L \== "",
                      split_string(L, "x", "", [NS|_]),
                      number_string(N, NS)
                    ),
             Lines, Numbers).

consecutive([]).
consecutive([_]).
consecutive([A,B|T]) :-
    B =:= A+1,
    consecutive([B|T]).

test(grow, [ cleanup(free(TI)),
             [Save, First, Last] == [1000, First0, 800]
           ]) :-
    terminal(200, TI),
    fill(TI, 300, 60),
    numbers(TI, [First0|_]),
    send(TI, save_lines, 1000),
    get(TI, save_lines, Save),
    fill(TI, 301, 800, 60),                 % fits in what was added
    numbers(TI, Numbers),
    assertion(consecutive(Numbers)),
    Numbers = [First|_],
    last(Numbers, Last).

test(shrink, [ cleanup(free(TI)),
               [Save, Last1, Last2] == [100, 500, 600]
             ]) :-
    terminal(1000, TI),
    fill(TI, 500, 60),
    send(TI, save_lines, 100),
    get(TI, save_lines, Save),
    numbers(TI, Numbers1),
    assertion(consecutive(Numbers1)),
    length(Numbers1, Kept),
    assertion(Kept < 100),
    last(Numbers1, Last1),
    assertion(ends_with_lines(TI, [499,500], 60)),
    fill(TI, 501, 600, 60),                 % output goes on at the caret
    numbers(TI, Numbers2),
    assertion(consecutive(Numbers2)),
    last(Numbers2, Last2),
    assertion(ends_with_lines(TI, [599,600], 60)).

% The ring holds at least the window, so that what is shown survives.

test(minimum, [cleanup(free(TI))]) :-
    terminal(1000, TI),
    fill(TI, 100, 60),
    send(TI, save_lines, 1),
    get(TI, save_lines, Save),
    get(TI, rows, Rows),
    assertion(Save > Rows),
    assertion(ends_with_lines(TI, [99,100], 60)).

test(selection, [ cleanup(free(TI)),
                  [Grown, Shrunk] == [Selected, Selected]
                ]) :-
    terminal(1000, TI),
    fill(TI, 500, 60),
    get(TI, contents, Str),
    get(Str, size, Size),
    From is Size-200,
    To is Size-100,
    send(TI, selection, From, To),
    get(TI, selected, S0), get(S0, value, Selected),
    send(TI, save_lines, 2000),
    get(TI, selected, S1), get(S1, value, Grown),
    send(TI, save_lines, 100),
    get(TI, selected, S2), get(S2, value, Shrunk).

test(selection_dropped, [cleanup(free(TI)), fail]) :-
    terminal(1000, TI),
    fill(TI, 500, 60),
    send(TI, selection, 0, 100),
    send(TI, save_lines, 100),
    get(TI, selected, S),
    get(S, size, Size),
    Size > 0.

:- end_tests(terminal_save_lines).
