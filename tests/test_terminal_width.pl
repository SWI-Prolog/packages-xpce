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

:- module(test_terminal_width,
          [ test_terminal_width/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test the column width the xpce terminal gives a character

The column grid is a contract with whatever writes to the terminal.  A
full-screen program lays its output out with its own wcwidth() -- East
Asian W and F are two columns, every other printable one -- and then
places the cursor by counting those columns.  It cannot see our font, so
neither may our answer: a character we make one column wider than the
client thinks puts us ahead of it for the rest of the line, and the
frame it redraws over that lands on the wrong rows.

That is not hypothetical.  The terminal used to promote a character to
two columns when the font drew its glyph more than 1.5 cells wide, which
on macOS caught U+23BF -- resolved from a symbol face that draws it at
about 1.6 cells -- and U+23BF opens every tool-result line of at least
one widely used terminal program.

So these tests pin <-cwidth to the static classification, and pin the
one property the font-measuring version could not have: that the answer
is the same in every font.
*/

test_terminal_width :-
    run_tests([ terminal_width
              ]).

                /*******************************
                *            HARNESS           *
                *******************************/

%!  terminal(-TI) is det.
%
%   A terminal image in an open window.  <-cwidth needs the cell
%   metrics, which the terminal only has once it has been computed.

terminal(TI) :-
    new(TI, terminal_image(600, 200)),      % pixels
    new(W, window('test_terminal_width')),
    send(W, display, TI),
    send(W, size, size(600, 200)),
    send(W, open),
    send(W, wait).

destroy_terminal(TI) :-
    get(TI, window, W),
    send(W, destroy).

%!  unicode_width(+Code, -Columns, -Comment) is nondet.
%
%   The width every wcwidth() agrees on, and that a client therefore
%   assumes.  The comments name where the character turns up, because
%   what makes a wrong answer expensive is not the character but the
%   program that draws with it.

unicode_width(0x0041, 1, 'A').
unicode_width(0x00B7, 1, 'MIDDLE DOT (East Asian Ambiguous)').
unicode_width(0x2026, 1, 'HORIZONTAL ELLIPSIS (Ambiguous)').
unicode_width(0x2500, 1, 'BOX DRAWINGS LIGHT HORIZONTAL (Ambiguous)').
unicode_width(0x2502, 1, 'BOX DRAWINGS LIGHT VERTICAL (Ambiguous)').
unicode_width(0x25CF, 1, 'BLACK CIRCLE (Ambiguous)').
unicode_width(0x2588, 1, 'FULL BLOCK (Ambiguous)').
unicode_width(0x2713, 1, 'CHECK MARK (Dingbats)').
unicode_width(0x2714, 1, 'HEAVY CHECK MARK (Dingbats)').
unicode_width(0x273B, 1, 'TEARDROP-SPOKED ASTERISK (Dingbats)').
unicode_width(0x2699, 1, 'GEAR (Misc Symbols)').
unicode_width(0x23BF, 1, 'LEFT PARENTHESIS LOWER HOOK (Misc Technical)').
unicode_width(0x23FA, 1, 'BLACK CIRCLE FOR RECORD (Misc Technical)').
unicode_width(0x2937, 1, 'ARROW DOWN THEN CURVING RIGHT').
unicode_width(0x231B, 2, 'HOURGLASS (East Asian Wide)').
unicode_width(0x2705, 2, 'WHITE HEAVY CHECK MARK (Wide)').
unicode_width(0x274C, 2, 'CROSS MARK (Wide)').
unicode_width(0x4E00, 2, 'CJK UNIFIED IDEOGRAPH-4E00 (Wide)').
unicode_width(0xFF21, 2, 'FULLWIDTH LATIN CAPITAL A (Fullwidth)').
unicode_width(0x1F600, 2, 'GRINNING FACE (Wide)').
unicode_width(0x0301, 0, 'COMBINING ACUTE ACCENT').
unicode_width(0xFE0F, 0, 'VARIATION SELECTOR-16').

                /*******************************
                *            TESTS             *
                *******************************/

:- begin_tests(terminal_width).

test(cwidth_is_the_unicode_width,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)), Wrong == []]) :-
    findall(Code-Comment-Expected-Got,
            ( unicode_width(Code, Expected, Comment),
              get(TI, cwidth, Code, Got),
              Got \== Expected
            ),
            Wrong).

%   The regression the font-measuring classification could not survive:
%   the client picks its width without seeing the font, so we must too.

test(cwidth_does_not_depend_on_the_font,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)), Differ == []]) :-
    findall(Code-Comment-Small-Large,
            ( unicode_width(Code, _, Comment),
              cwidth_in(TI, font(mono, normal, 10), Code, Small),
              cwidth_in(TI, font(mono, normal, 20), Code, Large),
              Small \== Large
            ),
            Differ).

%   <-cwidth is only worth anything if it is what the screen does.

test(the_caret_advances_by_cwidth,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI)), Wrong == []]) :-
    findall(Code-Comment-Expected-Got,
            ( unicode_width(Code, Expected, Comment),
              Expected > 0,             % a lone combining mark is dropped
              caret_after(TI, Code, Got),
              Got \== Expected
            ),
            Wrong).

:- end_tests(terminal_width).

%!  cwidth_in(+TI, +Font, +Code, -Columns) is det.

cwidth_in(TI, Font, Code, Columns) :-
    send(TI, font, Font),
    get(TI, cwidth, Code, Columns).

%!  caret_after(+TI, +Code, -Column) is det.
%
%   The column the caret sits in after writing Code at the start of an
%   empty screen, which is the number of columns the grid gave it.

caret_after(TI, Code, Column) :-
    char_code(Char, Code),
    atomic_list_concat(['\e[2J\e[H', Char], Text),
    send(TI, insert, Text),
    get(TI, cursor_position, Pos),
    get(Pos, x, Column).
