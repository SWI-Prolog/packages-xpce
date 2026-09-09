/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/projects/xpce/
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
:- module(test_terminal_margin,
          [ test_terminal_margin/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(debug), [assertion/1]).

/** <module> Test what the terminal does at the right margin

A character written in the last column does not take the caret to the
next row.  The caret stays where it is and the wrap happens when the
next character arrives -- that is what keeps a line which exactly fills
the width from costing a blank row after it, and every terminal works
that way.  What it leaves behind is a state of its own: the caret is on
the last column and a wrap is owed.

We leave the caret one column past the margin to say so, which lets a
combining mark still find the base it belongs to (see `rlc_put').  What
must not follow is that the rest of the terminal reads that as a place
the caret really is.  A cursor motion cancels the owed wrap: `CSI C' at
the margin stays on the last column rather than stepping over it, and
`CSI A' and `CSI B' land on the last column of the row above or below
rather than on the first column of the row after that.  A client that
draws a rule across the screen and then moves back up over its own
output -- which is every full-screen program there is -- counts its rows
wrong the moment we hand it one more row than it drew.

The other half is that a cell is not a column.  A combining mark is a
cell of its own and no column at all, so on a line carrying one the cell
index runs ahead of the column, and whatever passes a caret column on --
`CSI d', the cursor position report -- has to say which of the two it
means.

The answers here are the ones xterm and tmux give.

@see test_terminal_bce.pl for the erase and background tests, and
     test_terminal.pl for the suite that drives a terminal for real.
*/

test_terminal_margin :-
    run_tests([ terminal_margin
              ]).

                /*******************************
                *            HARNESS           *
                *******************************/

%!  terminal(-TI) is det.
%
%   A terminal in an open window.  Nothing here reads pixels, but the
%   window is opened all the same: the terminal takes its width in
%   columns from the size it is given.

terminal(TI) :-
    new(TI, terminal_image(800, 300)),
    new(W, window('test_terminal_margin')),
    send(W, display, TI),
    send(W, size, size(800, 300)),
    send(W, open),
    send(W, wait).

destroy_terminal(TI) :-
    get(TI, window, W),
    send(W, destroy).

%!  row(+Terminal, +Row, -Text) is det.

row(TI, Row, Text) :-
    get(TI, row, Row, String),
    get(String, value, Text).

%!  full_line(+Terminal) is det.
%
%   Clear the screen and fill the top row to the last column, leaving
%   the caret with a wrap owed.

full_line(TI) :-
    get(TI, columns, Cols),
    length(Codes, Cols),
    maplist(=(0'x), Codes),
    atom_codes(Full, Codes),
    send(TI, insert, '\e[2J\e[H'),
    send(TI, insert, Full).

%!  landed(+Terminal, +Sequence, -Row, -Column) is semidet.
%
%   Fill the top row, send Sequence, and say where the character after
%   it lands.  Where the next character goes is the whole of what a
%   client can see of the caret, and all this suite asks about.

landed(TI, Sequence, Row, Column) :-
    full_line(TI),
    send(TI, insert, Sequence),
    send(TI, insert, 'Z'),
    between(0, 4, Row),
    row(TI, Row, Text),
    sub_atom(Text, Column, _, _, 'Z'),
    !.

%!  last_column(+Terminal, -Column) is det.

last_column(TI, Column) :-
    get(TI, columns, Cols),
    Column is Cols-1.

                /*******************************
                *             TESTS            *
                *******************************/

:- begin_tests(terminal_margin).

% A row filled to the last column is one row, and the character after it
% is what starts the next.

test(a_full_row_is_one_row,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    full_line(TI),
    row(TI, 1, Next),
    assertion(Next == '').

test(the_owed_wrap_is_paid_by_the_next_character,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 1-0)]) :-
    landed(TI, '', Row, Column),
    Where = Row-Column.

% Cursor forward at the margin has nowhere to go: ECMA-48 does not let
% CUF pass the last column, and the owed wrap goes with the motion.
% Stepping over it put the next character on a row of its own.

test(forward_at_the_margin_stays_on_the_last_column,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 0-Last)]) :-
    last_column(TI, Last),
    landed(TI, '\e[C', Row, Column),
    Where = Row-Column.

test(forward_by_more_stays_there_too,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 0-Last)]) :-
    last_column(TI, Last),
    landed(TI, '\e[8C', Row, Column),
    Where = Row-Column.

% Up and down move within a column, and the column they are in is the
% last one.  Leaving the wrap owed spent it on the row they landed on,
% which cost a row and put the text at the left margin instead.

test(up_at_the_margin_keeps_the_last_column,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 0-Last)]) :-
    last_column(TI, Last),
    landed(TI, '\e[A', Row, Column),        % already on the top row
    Where = Row-Column.

test(down_at_the_margin_keeps_the_last_column,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 1-Last)]) :-
    last_column(TI, Last),
    landed(TI, '\e[B', Row, Column),
    Where = Row-Column.

% Backwards, the caret one past the margin is the answer already: the
% first step back is the last column, and a second one is the column
% before it.  This is what the fix above must not overshoot.

test(back_at_the_margin_is_the_last_column,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 0-Last)]) :-
    last_column(TI, Last),
    landed(TI, '\b', Row, Column),
    Where = Row-Column.

test(back_twice_is_the_column_before_it,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 0-Before)]) :-
    last_column(TI, Last),
    Before is Last-1,
    landed(TI, '\e[2D', Row, Column),
    Where = Row-Column.

% An index is not a cursor motion and does not cancel the owed wrap:
% xterm and tmux both pay it on the row below, at the left margin.

test(an_index_at_the_margin_still_owes_the_wrap,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 2-0)]) :-
    landed(TI, '\eD', Row, Column),
    Where = Row-Column.

% Nor does a colour change, which moves nothing at all.

test(a_colour_change_at_the_margin_still_owes_the_wrap,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 1-0)]) :-
    landed(TI, '\e[1m', Row, Column),
    Where = Row-Column.

% A carriage return has a column of its own and cancels the wrap by
% saying where the caret is.

test(a_carriage_return_at_the_margin_cancels_the_wrap,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Where == 0-0)]) :-
    landed(TI, '\r', Row, Column),
    Where = Row-Column.

% A cell is not a column.  Three combining sequences are six cells and
% three columns, so a caret carried to another row by its cell index
% lands three columns too far right.

test(a_line_position_is_a_column_not_a_cell,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)),
      true(Column == 4)]) :-
    atom_codes(Decomposed, [0'e,0x301,0'e,0x301,0'e,0x301,0'a,0'b,0'c]),
    send(TI, insert, '\e[2J\e[H'),
    send(TI, insert, Decomposed),
    send(TI, insert, '\e[5G'),          % the fifth column, which is `b'
    send(TI, insert, '\e[2d'),          % VPA: the row below, same column
    send(TI, insert, 'Z'),
    row(TI, 1, Text),
    sub_atom(Text, Column, _, _, 'Z').

:- end_tests(terminal_margin).
