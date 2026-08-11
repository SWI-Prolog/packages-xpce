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

:- module(test_terminal_osc,
          [ test_terminal_osc/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test OSC 8 hyperlinks of the xpce terminal

A hyperlink is a state: everything between `ESC ] 8 ; <params> ; <URL>
ST` and the same sequence with an empty URL is the label, escape
sequences included.  Clients colour the label (ripgrep does), so the
label must go through the normal escape processing rather than be
copied to the screen as text.
*/

test_terminal_osc :-
    run_tests([ terminal_osc8
              ]).

                /*******************************
                *            HARNESS           *
                *******************************/

%!  terminal(-Terminal) is det.
%
%   Create a terminal image of 80x25 cells in an open window.

terminal(TI) :-
    new(TI, terminal_image(1000, 500)),
    new(W, window('test_terminal_osc')),
    send(W, display, TI),
    send(W, open),
    send(W, wait).

%!  destroy_terminal(+Terminal) is det.

destroy_terminal(TI) :-
    get(TI, window, W),
    send(W, destroy).

%!  row(+Terminal, +Row, -Text) is det.
%
%   Text of Row, with trailing blanks removed.

row(TI, Row, Text) :-
    get(TI, row, Row, String),
    get(String, value, Atom),
    normalize_space(atom(Text), Atom).

%!  link_at(+Terminal, +Column, +Row, -URL) is semidet.
%
%   URL of the hyperlink displayed at cell Column,Row.

link_at(TI, Column, Row, URL) :-
    get(TI, font, Font),
    get(Font, avg_char_width, CW),
    get(Font, height, CH),
    X is round((Column+1)*CW + CW/2),   % centre of the cell; one cell
    Y is round(Row*CH + CH/2),          % of left margin
    get(TI, link, point(X, Y), URL).

%!  osc8(+URL, -Sequence) is det.
%!  st(-ST) is det.
%
%   Building blocks for OSC 8 sequences.  osc8('') closes the link.

osc8(URL, Seq) :-
    osc8(URL, '', Seq).

osc8(URL, Params, Seq) :-
    st(ST),
    atom_codes(Osc, [0'\e, 0']]),
    atomic_list_concat([Osc, '8;', Params, ';', URL, ST], Seq).

st(ST) :-
    atom_codes(ST, [0'\e, 0'\\]).

sgr(Code, Seq) :-
    atomic_list_concat(['\e[', Code, m], Seq).

                /*******************************
                *             TESTS            *
                *******************************/

:- begin_tests(terminal_osc8).

test(plain, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    osc8('file:///tmp/a.txt', Open),
    osc8('', Close),
    atomic_list_concat([Open, '/tmp/a.txt', Close, ':hello'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == '/tmp/a.txt:hello'),
    assertion(link_at(TI, 0, 0, 'file:///tmp/a.txt')),
    assertion(link_at(TI, 9, 0, 'file:///tmp/a.txt')),
    assertion(\+ link_at(TI, 11, 0, _)).

% As emitted by rg --hyperlink-format=...: the label is coloured, so it
% contains SGR sequences.  These must be processed rather than be added
% to the screen as text.

test(coloured_label, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    osc8('file:///tmp/a.txt#L1:1', Open),
    osc8('', Close),
    sgr(0, Sgr0),
    sgr(35, Sgr35),
    atomic_list_concat([Open, Sgr0, Sgr35, '/tmp/a.txt', Sgr0, Close,
                        ':hello'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == '/tmp/a.txt:hello'),
    assertion(link_at(TI, 0, 0, 'file:///tmp/a.txt#L1:1')),
    assertion(link_at(TI, 9, 0, 'file:///tmp/a.txt#L1:1')),
    assertion(\+ link_at(TI, 10, 0, _)).

% The label may be terminated using BEL rather than ST and the sequence
% may carry parameters, e.g., `id=`.

test(bel_and_params, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    atom_codes(Open, [0'\e, 0'], 0'8, 0';, 0'i, 0'd, 0'=, 0'1, 0';,
                      0'h, 0't, 0't, 0'p, 0':, 0'/, 0'/, 0'x, 0'/, 0'\a]),
    atom_codes(Close, [0'\e, 0'], 0'8, 0';, 0';, 0'\a]),
    atomic_list_concat([Open, label, Close, '.'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == 'label.'),
    assertion(link_at(TI, 0, 0, 'http://x/')),
    assertion(\+ link_at(TI, 5, 0, _)).

% A label that does not fit on the line continues on the next one, where
% it must be a link as well.

test(wrap, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    osc8('file:///tmp/wrap', Open),
    osc8('', Close),
    get(TI, columns, Columns),
    Len is Columns+20,
    length(Codes, Len),
    maplist(=(0'x), Codes),
    atom_codes(Label, Codes),
    atomic_list_concat([Open, Label, Close], Text),
    send(TI, insert, Text),
    Last is Columns-1,
    assertion(link_at(TI, Last, 0, 'file:///tmp/wrap')),
    assertion(link_at(TI, 0, 1, 'file:///tmp/wrap')),
    assertion(link_at(TI, 19, 1, 'file:///tmp/wrap')),
    assertion(\+ link_at(TI, 21, 1, _)).

% Every pixel of a cell must find the link of that cell, including the
% first and last one.  Translating a click used to be off by one cell,
% so the last character of a link was not clickable.

test(cell_pixels, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    numlist(0, 4, Cells),
    maplist(one_cell_link, Cells, Texts),
    atomic_list_concat(Texts, Text),
    send(TI, insert, Text),
    get(TI, font, Font),
    get(Font, avg_char_width, CW0),
    CW is truncate(CW0),
    forall(member(Cell, Cells),
           ( atom_concat(u, Cell, URL),
             Left  is (Cell+1)*CW,      % first pixel of the cell
             Right is (Cell+2)*CW-1,    % last pixel of the cell
             assertion(link_pixel(TI, Left, URL)),
             assertion(link_pixel(TI, Right, URL)),
             assertion(link_at(TI, Cell, 0, URL))
           )).

one_cell_link(Cell, Text) :-
    atom_concat(u, Cell, URL),
    osc8(URL, Open),
    osc8('', Close),
    atomic_list_concat([Open, x, Close], Text).

%!  link_pixel(+Terminal, +X, ?URL) is semidet.
%
%   URL of the hyperlink at pixel X of the first row.

link_pixel(TI, X, URL) :-
    get(TI, link, point(X, 1), URL).

:- end_tests(terminal_osc8).
