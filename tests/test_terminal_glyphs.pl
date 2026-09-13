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
:- module(test_terminal_glyphs,
          [ test_terminal_glyphs/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(debug), [assertion/1]).

/** <module> Test how the xpce terminal fits a glyph to its cell

A font lays its glyphs out in its own em box, which is not the cell: the
box is placed by the baseline and is shorter than the line height a
terminal gives a row.  Drawn from the font, U+2588 FULL BLOCK therefore
leaves a strip of the cell background above it and a quadrant sits below
the top of its cell -- so a picture built out of these characters comes
out of the screen in stripes, with the rows detached from one another.

These tests write blocks that must meet and read the pixels back to see
that they do.  They ask nothing about where the cell is: a shape drawn
from the font is not a little wrong but visibly broken, and what says so
is the background showing through a seam that should not exist.

The em box is too narrow as often as it is too short.  The last test
takes the other side of the same question: a glyph the font draws wider
than the columns Unicode gives the character has to be made to fit,
because the width belongs to the client (see test_terminal_width.pl).
*/

test_terminal_glyphs :-
    run_tests([ terminal_glyphs
              ]).

                /*******************************
                *            HARNESS           *
                *******************************/

%!  terminal(-TI) is det.
%
%   A terminal image in an open window, so that there is something to
%   read pixels from.

terminal(TI) :-
    new(TI, terminal_image(600, 200)),
    new(W, window('test_terminal_glyphs')),
    send(W, display, TI),
    send(W, size, size(600, 200)),
    send(W, open),
    send(W, wait).

destroy_terminal(TI) :-
    get(TI, window, W),
    send(W, destroy).

%!  settle(+Terminal) is det.
%
%   Let the window paint what was inserted: the text of a row is there
%   the moment it is written, the pixels are painted from an event.

settle(_TI) :-
    get_time(Now),
    Deadline is Now+0.3,
    settle_until(Deadline).

settle_until(Deadline) :-
    pce_principal:pce_dispatch(-1, 0.05),
    get_time(Now),
    (   Now >= Deadline
    ->  true
    ;   settle_until(Deadline)
    ).

%!  ink(-Escape) is det.
%!  paper(-Escape) is det.
%
%   The two colours the tests draw with, as far apart as colours get so
%   that classify/2 cannot mistake one for the other.

ink('\e[38;2;255;0;0m').                        % red foreground
paper('\e[48;2;0;0;255m').                      % blue background

%!  write_blocks(+Terminal, +Rows) is det.
%
%   Clear the screen and write Rows, each a text of block characters,
%   red on blue.  The caret ends up on the row below them, where it
%   cannot colour a pixel these tests read.

write_blocks(TI, Rows) :-
    ink(Ink), paper(Paper),
    findall(Text, ( member(Row, Rows),
                    atomic_list_concat([Row, '\r\n'], Text)
                  ), Texts),
    atomic_list_concat([ '\e[2J\e[H', Ink, Paper | Texts ], Screen),
    atomic_list_concat([Screen, '\e[0m'], Full),
    send(TI, insert, Full),
    settle(TI).

%!  scan_x(+Terminal, +Y, -Classes) is det.
%!  scan_y(+Terminal, +X, -Classes) is det.
%
%   Classify a row or a column of pixels of the window, left to right
%   and top to bottom.

scan_x(TI, Y, Classes) :-
    image_of(TI, Img, W, _H),
    Last is W-1,
    findall(C, ( between(0, Last, X),
                 class_at(Img, X, Y, C)
               ), Classes).

scan_y(TI, X, Classes) :-
    image_of(TI, Img, _W, H),
    Last is H-1,
    findall(C, ( between(0, Last, Y),
                 class_at(Img, X, Y, C)
               ), Classes).

image_of(TI, Img, W, H) :-
    get(TI, window, Win),
    get(Win, image, Img),
    get(Img, size, size(W, H)).

class_at(Img, X, Y, Class) :-
    get(Img, pixel(X, Y), Colour),
    get(Colour, red, R),
    get(Colour, green, G),
    get(Colour, blue, B),
    classify(R, G, B, Class).

classify(R, G, B, ink) :-
    R > G, R > B,
    !.
classify(R, G, B, paper) :-
    B > R, B > G,
    !.
classify(_, _, _, other).

%!  solid(+Classes) is semidet.
%
%   True when the ink in Classes is one unbroken run: no paper between
%   the first ink and the last.  This is the whole of what these tests
%   ask, and what a glyph that does not fill its cell breaks.  A pixel
%   that is neither is let through: the edge of a glyph is not what is
%   being looked at.

solid(Classes) :-
    append(_, [ink|Rest], Classes),
    !,
    no_ink_after_paper(Rest, false).

no_ink_after_paper([], _).
no_ink_after_paper([ink|T], SawPaper) :-
    SawPaper == false,
    no_ink_after_paper(T, false).
no_ink_after_paper([paper|T], _) :-
    no_ink_after_paper(T, true).
no_ink_after_paper([other|T], SawPaper) :-
    no_ink_after_paper(T, SawPaper).

%!  ink_length(+Classes, -N) is det.

ink_length(Classes, N) :-
    aggregate_all(count, member(ink, Classes), N).

%!  glyph_ink_columns(+Terminal, +Char, -Xs) is det.
%
%   The x coordinates at which Char, written alone on an otherwise
%   empty screen, puts ink.  Only the left of the window is looked at:
%   the question is always about the first cell or two, and a pixel
%   read is not cheap.

glyph_ink_columns(TI, Char, Xs) :-
    write_blocks(TI, [Char]),
    image_of(TI, Img, W, H),
    LastX is min(W, 120)-1,
    LastY is H-1,
    findall(X, ( between(0, LastX, X),
                 between(0, LastY, Y),
                 class_at(Img, X, Y, ink)
               ), Inked),
    sort(Inked, Xs).

%!  inked_row(+Terminal, -Y) is semidet.
%!  inked_column(+Terminal, -X) is semidet.
%
%   A row or column of pixels that the blocks reach, so that a scan
%   across them has something to say.  Found rather than computed: the
%   cell size and the margins of the window are not what these tests
%   are about.

inked_row(TI, Y) :-
    image_of(TI, _Img, _W, H),
    Last is H-1,
    between(0, Last, Y),
    scan_x(TI, Y, Classes),
    memberchk(ink, Classes),
    !.

inked_column(TI, X) :-
    image_of(TI, _Img, W, _H),
    Last is W-1,
    between(0, Last, X),
    scan_y(TI, X, Classes),
    memberchk(ink, Classes),
    !.

                /*******************************
                *             TESTS            *
                *******************************/

:- begin_tests(terminal_glyphs).

full_row('████████████████████').

%   The sample says something at all: a row of upper halves has ink
%   over paper, and the scan sees both.

test(the_sample_sees_both_colours,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    write_blocks(TI, ['\u2580\u2580\u2580\u2580\u2580\u2580\u2580\u2580']),
    inked_column(TI, X),
    scan_y(TI, X, Column),
    assertion(memberchk(ink, Column)),
    assertion(memberchk(paper, Column)).

test(full_blocks_meet_across_a_row_boundary,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  Two rows of full blocks are one solid area.  Drawn from the
    %  font they were two bands with the cell background between them
    %  -- the dark line Claude Code's logo grew above its head.
    full_row(Full),
    write_blocks(TI, [Full, Full]),
    inked_column(TI, X),
    scan_y(TI, X, Column),
    assertion(solid(Column)).

test(half_blocks_meet_across_a_row_boundary,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  A lower half over an upper half is the same solid area, one
    %  cell high.  This is how a picture puts a row of feet under a
    %  body; they used to hang below it with a gap.
    write_blocks(TI, ['▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄',
                      '▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀']),
    inked_column(TI, X),
    scan_y(TI, X, Halves),
    assertion(solid(Halves)),
    ink_length(Halves, Half),
    %  And really halves: half of what two rows of full blocks cover.
    full_row(Full),
    write_blocks(TI, [Full, Full]),
    scan_y(TI, X, Whole),
    ink_length(Whole, Cells),
    assertion(abs(Cells - 2*Half) =< 1).

test(half_blocks_meet_across_a_column_boundary,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  The same the other way round: a right half against the left
    %  half of the cell beside it covers one whole cell, unbroken.
    write_blocks(TI, ['█']),
    inked_row(TI, Y),
    scan_x(TI, Y, One),
    ink_length(One, Cell),
    write_blocks(TI, ['▐▌']),
    scan_x(TI, Y, Pair),
    assertion(solid(Pair)),
    ink_length(Pair, Width),
    assertion(Width =:= Cell).

test(quadrants_reach_the_top_of_their_cell,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  The upper quadrants tile the top half of their row, so under a
    %  row of full blocks they leave no seam at any column.
    full_row(Full),
    write_blocks(TI, [Full, '▘▝▘▝▘▝▘▝▘▝▘▝▘▝▘▝▘▝▘▝']),
    inked_column(TI, X0),
    forall(( between(0, 19, I),
             X is X0+I
           ),
           ( scan_y(TI, X, Column),
             assertion(solid(Column))
           )).

test(a_glyph_the_font_draws_too_wide_stays_in_its_cell,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  A full block is painted as the rectangle of its cell, so its ink
    %  is exactly one cell wide: the boundary no one-column character
    %  may cross.
    glyph_ink_columns(TI, '\u2588', Cell),
    last(Cell, CellRight),
    %  U+23BF LEFT PARENTHESIS LOWER HOOK is one column to every
    %  wcwidth, and on macOS comes from a fallback face that draws it
    %  at about 1.6 cells.  We may not answer 2 columns for it -- the
    %  client counts 1 either way -- so it has to be condensed into the
    %  one column it has.
    glyph_ink_columns(TI, '\u23bf', Hook),
    assertion(Hook \== []),
    last(Hook, HookRight),
    assertion(HookRight =< CellRight).

:- end_tests(terminal_glyphs).
