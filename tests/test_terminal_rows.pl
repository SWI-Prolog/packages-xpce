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

:- module(test_terminal_rows,
          [ test_terminal_rows/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test the number of rows the xpce terminal fits in its height

The xpce caret is a triangle hanging from the baseline that is taller
than the descent of the font.  On the last row it is truncated unless
the terminal leaves room below that row.  These tests check that the
rows leave that room, and no more than needed.

Run with:

    swipl -g test_terminal_rows -t halt \
          packages/xpce/tests/test_terminal_rows.pl
*/

test_terminal_rows :-
    run_tests([ terminal_rows
              ]).

%!  with_caret_style(+Style, :Goal)
%
%   Run Goal with text_cursor.fixed_font_style set to Style.

with_caret_style(Style, Goal) :-
    get(class(text_cursor), class_variable, fixed_font_style, CV),
    get(CV, value, Old),
    setup_call_cleanup(send(CV, value, Style),
                       Goal,
                       send(CV, value, Old)).

%!  rows(+Height, -Rows, -CellHeight, -Ascent) is det.
%
%   Rows of a terminal that is Height pixels high.

rows(Height, Rows, CellHeight, Ascent) :-
    new(T, terminal_image(400, Height)),
    get(T, rows, Rows),
    get(T, font, Font),
    get(Font, height, CellHeight),
    get(Font, ascent, Ascent),
    free(T).

%!  cells(+Cols0, +Rows0, -Cols, -Rows) is det.
%
%   Cells of a terminal sized by <-cells_size for Cols0 by Rows0.

cells(Cols0, Rows0, Cols, Rows) :-
    new(T0, terminal_image),
    get(T0, cells_size, Cols0, Rows0, size(W, H)),
    free(T0),
    new(T, terminal_image(W, H)),
    get(T, columns, Cols),
    get(T, rows, Rows),
    free(T).

caret_size(Size) :-
    get(class(text_cursor), class_variable, height, CV),
    get(CV, value, Size).

%   The xpce caret of the last row starts one pixel above the baseline
%   of that row.  One pixel is left for anti-aliasing.

xpce_caret_fits(Height) :-
    rows(Height, Rows, CH, Ascent),
    caret_size(Size),
    Bottom is (Rows-1)*CH + Ascent - 1 + Size,
    Bottom + 1 =< Height,
    \+ ( Bottom + CH + 1 =< Height ).

:- begin_tests(terminal_rows).

test(xpce_caret_fits, [forall(between(60, 200, Height))]) :-
    with_caret_style(xpce, xpce_caret_fits(Height)).
test(block_caret_uses_all_rows,
     [forall(between(60, 200, Height)), Rows =:= Height // CH]) :-
    with_caret_style(block, rows(Height, Rows, CH, _)).
test(cells_size_gives_the_cells,
     [ forall(( member(Style, [xpce, block]),
                between(5, 30, Rows0)
              )),
       [Cols, Rows] == [80, Rows0]
     ]) :-
    %  A window sized for 25 rows must get 25 rows, not one less for
    %  the room the caret needs below the last.
    with_caret_style(Style, cells(80, Rows0, Cols, Rows)).

:- end_tests(terminal_rows).
