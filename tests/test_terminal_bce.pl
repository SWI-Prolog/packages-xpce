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

:- module(test_terminal_bce,
          [ test_terminal_bce/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test background colour erase of the xpce terminal

`ESC [ K` (_Erase in Line_) paints the cells it erases using the current
background colour, which is how a coloured block is made to extend to the
right margin.  These cells are not stored in the line: the terminal holds
lines of the length they were written to, so that copying a row does not
pick up the blanks to the right of its text.  These tests verify that
contract: after an erase with a background colour the text of the row and
the selection must still end at the last character written.
*/

test_terminal_bce :-
    run_tests([ terminal_bce
              ]).

                /*******************************
                *            HARNESS           *
                *******************************/

terminal(TI) :-
    new(TI, terminal_image(1000, 500)),
    new(W, window('test_terminal_bce')),
    send(W, display, TI),
    send(W, size, size(1000, 500)),	% so the whole terminal is painted
    send(W, open),
    send(W, wait).

destroy_terminal(TI) :-
    get(TI, window, W),
    send(W, destroy).

%!  row(+Terminal, +Row, -Text) is det.
%
%   Text of Row, _without_ removing trailing blanks: that is what these
%   tests are about.

row(TI, Row, Text) :-
    get(TI, row, Row, String),
    get(String, value, Text).

%!  selected(+Terminal, -Text) is det.
%
%   Text of the selection over the whole buffer, i.e., what a copy from
%   the terminal yields.

selected(TI, Text) :-
    get(TI, length, Len),
    send(TI, selection, 0, Len),
    (   get(TI, selected, String),
        String \== @nil
    ->  get(String, value, Text)
    ;   Text = ''
    ).

bg(Seq) :-                                      % a soft dark background
    Seq = '\e[48;5;235m'.

%!  settle(+Terminal) is det.
%
%   Let the window paint what was inserted.  Only the tests that read
%   pixels need this: the text of a row is there the moment it is
%   written, but what is on the screen is painted from an event.

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

%!  tail_colours(+Terminal, -Colours) is det.
%
%   Colours of a column of pixels halfway across the window, top down,
%   as `rgb(R,G,B)' terms.  That column crosses the tail of the top row,
%   which is the only place the background colour erase shows: the cells
%   are not in the line (see above), so nothing but the screen itself
%   can say whether they were painted.

tail_colours(TI, Colours) :-
    get(TI, window, W),
    get(W, frame, F),
    get(F, image, Img),
    get(Img, size, size(Width, Height)),
    X is Width//2,
    findall(rgb(R,G,B),
            ( between(0, 15, I),
              Y is I*4,
              Y < Height,
              get(Img, pixel(X, Y), Colour),
              get(Colour, red, R),
              get(Colour, green, G),
              get(Colour, blue, B)
            ),
            Colours).

%!  column_colours(+Terminal, -Colours) is det.
%
%   Colours of every other pixel of the column halfway across the
%   window, top to bottom.  Unlike tail_colours/2 this reaches the
%   bottom row, which is where a line feed scrolls a line in.

column_colours(TI, Colours) :-
    get(TI, window, W),
    get(W, frame, F),
    get(F, image, Img),
    get(Img, size, size(Width, Height)),
    X is Width//2,
    Last is Height-1,
    findall(rgb(R,G,B),
            ( between(0, Last, Y),
              Y mod 2 =:= 0,
              get(Img, pixel(X, Y), Colour),
              get(Colour, red, R),
              get(Colour, green, G),
              get(Colour, blue, B)
            ),
            Colours).

%!  painted(+Terminal) is semidet.
%
%   True if the background of bg/1 shows somewhere in the middle
%   column of the window.

painted(TI) :-
    settle(TI),
    column_colours(TI, Colours),
    memberchk(rgb(38,38,38), Colours).

%!  to_last_row(+Terminal) is det.
%
%   Put the caret on the last row of an empty window.  The terminal
%   counts its rows from its size only once that changes, and until
%   then it has more of them than the window shows: the line feed would
%   scroll in a line below the bottom of the window.

to_last_row(TI) :-
    send(TI, height, 480),
    get(TI, rows, Rows),
    N is Rows-1,
    length(NLs, N),
    maplist(=('\r\n'), NLs),
    atomic_list_concat(['\e[2J\e[H'|NLs], Seq),
    send(TI, insert, Seq).

                /*******************************
                *             TESTS            *
                *******************************/

:- begin_tests(terminal_bce).

% The canonical case: the toplevel writes an answer and erases to the end
% of the line to extend the background to the right margin.

test(row_keeps_its_length,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    atomic_list_concat([Bg, 'X = 1 ', '\e[K'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == 'X = 1 ').

test(copy_has_no_trailing_blanks,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    atomic_list_concat([Bg, 'X = 1 ', '\e[K'], Text),
    send(TI, insert, Text),
    selected(TI, Selected),
    assertion(Selected == 'X = 1 ').

% The prompt erases the line first and writes over it afterwards.

test(erase_then_write,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    atomic_list_concat([Bg, '\e[K', '?- '], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == '?- ').

% Erasing the whole line (EL 2) must not leave blanks behind either.

test(erase_whole_line,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    atomic_list_concat([Bg, 'hello', '\e[2K'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == '').

% An erase without a background colour behaves as it always did.

test(erase_without_background,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    send(TI, insert, 'abcdef\e[3G\e[K'),
    row(TI, 0, Row),
    assertion(Row == 'ab').

% A line that scrolled up keeps its text, not the painted tail.

test(next_line_unaffected,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    atomic_list_concat([Bg, 'first', '\e[K', '\r\n', 'second'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row0),
    row(TI, 1, Row1),
    assertion(Row0 == 'first'),
    assertion(Row1 == 'second').

% The tail painted by an erase belongs to the line rather than to its
% text, so saving the screen for the alternate screen and putting it back
% has to carry it along.  help/1 leaves through the alternate screen, and
% the input lines came back with a background behind their text only.

test(background_survives_the_alternate_screen,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    atomic_list_concat([Bg, 'text', '\e[0m\r\n.\r\n'], Plain),
    atomic_list_concat([Bg, 'text', '\e[K\e[0m\r\n.\r\n'], Erased),
    send(TI, insert, Plain),
    settle(TI),
    tail_colours(TI, NotPainted),
    send(TI, insert, '\e[2J\e[H'),
    send(TI, insert, Erased),
    settle(TI),
    tail_colours(TI, Painted),
    %  Without this the rest says nothing: it is what shows that the
    %  sample tells a painted tail from a plain one at all.
    assertion(NotPainted \== Painted),
    send(TI, insert, '\e[?1049h\e[HALT'),
    send(TI, insert, '\e[?1049l'),
    settle(TI),
    tail_colours(TI, Restored),
    assertion(Restored == Painted).

% The painted tail belongs to the line, so letting go of the line must
% let go of it too.  A line opened by an insert (IL) has no text, and
% erasing it with a background colour left that erase in the slot after
% the display was cleared: the next line written there came up with the
% old background behind it.  This is the input box of a full screen
% client showing up at rows it no longer occupies.

test(erase_does_not_outlive_the_line,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    send(TI, insert, 'text\r\n.\r\n'),
    settle(TI),
    tail_colours(TI, Plain),
    atomic_list_concat(['\e[2J\e[H\e[L', Bg, '\e[2K\e[0m'], Erase),
    send(TI, insert, Erase),
    send(TI, insert, '\e[2J\e[H'),
    send(TI, insert, 'text\r\n.\r\n'),
    settle(TI),
    tail_colours(TI, Reused),
    assertion(Reused == Plain).

% The lines a scroll brings in are erased, and so take the background
% colour as an erase does.  This is what xterm does.

test(inserted_line_takes_background,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    send(TI, insert, 'text\r\n.\r\n\e[H'),
    atomic_list_concat([Bg, '\e[L\e[0m'], Seq),
    send(TI, insert, Seq),
    painted(TI).

test(region_scroll_takes_background,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    atomic_list_concat(['a\r\nb\r\nc\e[1;3r\e[3H', Bg, '\n\e[0m'], Seq),
    send(TI, insert, Seq),
    painted(TI).

test(line_feed_scrolls_in_background,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    bg(Bg),
    to_last_row(TI),
    atomic_list_concat([Bg, '\r\n\e[0m'], Seq),
    send(TI, insert, Seq),
    painted(TI).

% Only a line that scrolls in: a line feed onto a row the window has
% room for leaves it as it is, as the row exists in xterm already.

test(line_feed_in_window_keeps_default,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)), fail]) :-
    bg(Bg),
    atomic_list_concat([Bg, '\r\n\r\n\e[0m'], Seq),
    send(TI, insert, Seq),
    painted(TI).

test(plain_line_feed_keeps_default,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)), fail]) :-
    to_last_row(TI),
    send(TI, insert, '\r\n'),
    painted(TI).

:- end_tests(terminal_bce).
