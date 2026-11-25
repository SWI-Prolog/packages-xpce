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

:- module(test_emacs_align, [test_emacs_align/0]).
:- encoding(utf8).

/** <module> Tests for aligning text in PceEmacs

Tests for editor->align and the alignment commands of the PceEmacs
language modes.

Run with:

    swipl -g test_emacs_align -t halt \
          packages/xpce/tests/test_emacs_align.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- set_prolog_flag(emacs_server, false).

:- use_module(xpce_test_sandbox).
:- initialization(sandbox_app_config(_), now).

:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(plunit)).
:- use_module(library(pce_emacs)).

test_emacs_align :-
    run_tests([ editor_align,
                line_region,
                align_regexp,
                align_comment,
                comment_column,
                align
              ]).

                 /*******************************
                 *            HELPERS           *
                 *******************************/

%!  editor_text(+Text, -Editor) is det.

editor_text(Text, E) :-
    new(E, editor),
    send(E?text_buffer, insert, 0, Text).

%!  editor_contents(+Editor, -Text) is det.

editor_contents(E, Text) :-
    get(E?text_buffer, contents, String),
    get(String, value, Text).

%!  mode(+ModeName, +Text, -Mode) is det.
%
%   A PceEmacs mode over a scratch buffer holding Text.  The buffers of
%   a run share @emacs_buffers, so each gets a name of its own.

:- dynamic buffer_count/1.

mode(ModeName, Text, M) :-
    start_emacs,
    (   retract(buffer_count(N0))
    ->  N is N0+1
    ;   N = 1
    ),
    assertz(buffer_count(N)),
    format(atom(Name), '*test-emacs-align-~d*', [N]),
    new(B, emacs_buffer(@nil, Name)),
    send(B, mode, ModeName),
    send(B, insert, 0, Text),
    new(V, emacs_view(B)),
    get(V, mode, M).

%!  region_mode(+ModeName, +Text, -Mode) is det.
%
%   As mode/3, with the whole buffer as region.

region_mode(ModeName, Text, M) :-
    mode(ModeName, Text, M),
    atom_length(Text, Len),
    send(M, mark, 0),
    send(M, caret, Len).

%!  contents(+Mode, -Text) is det.

contents(M, Text) :-
    get(M, text_buffer, TB),
    get(TB, contents, String),
    get(String, value, Text).

                 /*******************************
                 *          ->ALIGN             *
                 *******************************/

:- begin_tests(editor_align).

%   Text at the start of the buffer is at the start of a line, so
%   aligning it to column 0 must not insert a space.

test(start_of_buffer_to_column_0, Text == 'x = 1') :-
    editor_text('x = 1', E),
    send(E, align, 0, 0),
    editor_contents(E, Text).

test(start_of_line_to_column_0, Text == 'a\nx = 1') :-
    editor_text('a\nx = 1', E),
    send(E, align, 0, 2),
    editor_contents(E, Text).

test(after_text_keeps_one_space, Text == 'x = 1') :-
    editor_text('x   = 1', E),
    send(E, align, 0, 4),
    editor_contents(E, Text).

test(after_text_to_column, Text == 'x     = 1') :-
    editor_text('x = 1', E),
    send(E?text_buffer, indent_tabs, @off),
    send(E, align, 6, 2),
    editor_contents(E, Text).

:- end_tests(editor_align).

                 /*******************************
                 *         <-LINE_REGION        *
                 *******************************/

%   The lines of a region are those it touches; a region that ends at
%   the start of a line does not touch that line.

:- begin_tests(line_region).

test(ends_at_start_of_line, R == 4-8) :-
    mode(fundamental, 'aaa\nbbb\nccc\n', M),
    get(M, line_region, 5, 8, tuple(S, E)),
    R = S-E.
test(ends_inside_a_line, R == 4-12) :-
    mode(fundamental, 'aaa\nbbb\nccc\n', M),
    get(M, line_region, 5, 9, tuple(S, E)),
    R = S-E.
test(ends_inside_the_last_line, R == 0-7) :-
    mode(fundamental, 'aaa\nbbb', M),
    get(M, line_region, 1, 6, tuple(S, E)),
    R = S-E.

:- end_tests(line_region).

                 /*******************************
                 *          ALIGN_REGEXP        *
                 *******************************/

%   The expected results are those of GNU Emacs 31 `align-regexp` with
%   indent-tabs-mode nil.

:- begin_tests(align_regexp).

test(first_match, Text == 'x      = 1;\nlonger =2;\nyy     =   3;\n') :-
    region_mode(prolog, 'x = 1;\nlonger=2;\nyy  =   3;\n', M),
    send(M, align_regexp_region, 0, 30, regex('(\\s*)=')),
    contents(M, Text).
test(repeat, Text == 't(a,   1,   x).\nt(bbb, 22,  yy).\nt(c,   333, z).\n') :-
    region_mode(prolog, 't(a, 1, x).\nt(bbb, 22, yy).\nt(c,333,z).\n', M),
    send(M, align_regexp_region, 0, 40, regex(',(\\s*)'), 1, 1, @on),
    contents(M, Text).
test(operator, Text == '  p(a)     :- q.\n  pp(a, b) :- q.\n') :-
    region_mode(prolog, '  p(a) :- q.\n  pp(a, b) :- q.\n', M),
    send(M, align_regexp_region, 0, 30, regex('(\\s*):-')),
    contents(M, Text).
test(spacing, Text == 'x   = 1;\nyy  = 2;\n') :-
    region_mode(prolog, 'x = 1;\nyy = 2;\n', M),
    send(M, align_regexp_region, 0, 16, regex('(\\s*)='), 1, 2),
    contents(M, Text).
test(not_in_strings, Text == 'x("=") =1;\nlo     =2;\n') :-
    region_mode(prolog, 'x("=")=1;\nlo=2;\n', M),
    send(M, align_regexp_region, 0, 15, regex('(\\s*)=')),
    contents(M, Text).
test(tabs, Text == 'x\t= 1;\nlonger\t=2;\n') :-
    region_mode(prolog, 'x = 1;\nlonger=2;\n', M),
    send(M, indent_tabs, @on),
    send(M, tab_distance, 8),
    send(M, align_regexp_region, 0, 16, regex('(\\s*)=')),
    contents(M, Text).

%   M-x align-regexp without prefix: the regex is the text after the
%   white space to change.

test(command, Text == 'x      = 1;\nlonger =2;\n') :-
    region_mode(prolog, 'x = 1;\nlonger=2;\n', M),
    send(M, align_regexp, @default, "="),
    contents(M, Text).

:- end_tests(align_regexp).

                 /*******************************
                 *          ALIGN_COMMENT       *
                 *******************************/

:- begin_tests(align_comment).

%   All lines, including the last; full-line comments stay where they
%   are; a % in a string is not a comment.

test(region, Text == Expected) :-
    region_mode(prolog,
                'foo(a). % c1\n% full\nx("a%b"). % c2\ny. % c3\n', M),
    send(M, comment_column, 16),
    send(M, align_comment),
    contents(M, Text),
    Expected = 'foo(a).         % c1\n% full\n\c
                x("a%b").       % c2\ny.              % c3\n'.

%   At least one space after the longest code

test(right_of_code, Text == 'abcdef. % c1\ny.      % c2\n') :-
    region_mode(prolog, 'abcdef. % c1\ny. % c2\n', M),
    send(M, comment_column, 4),
    send(M, align_comment),
    contents(M, Text).

%   C declares // using line_comment/1 of its syntax rather than as a
%   comment_start/comment_end pair.

test(c_line_comment, Text == 'int x = 1;\t// one\nlong yy = 22;\t// two\n') :-
    region_mode(c, 'int x = 1; // one\nlong yy = 22; // two\n', M),
    send(M, comment_column, 16),
    send(M, align_comment),
    contents(M, Text).

:- end_tests(align_comment).

                 /*******************************
                 *        COMMENT COLUMN        *
                 *******************************/

:- begin_tests(comment_column).

test(set, Col == 6) :-
    mode(prolog, 'foo(a).\n', M),
    send(M, caret, 6),
    send(M, comment_set_column),
    get(M, comment_column, Col).
test(from_previous, Text == 'a.    % c1\nb.    % \n') :-
    mode(prolog, 'a.    % c1\nb.\n', M),
    send(M, caret, 12),
    send(M, comment_set_column, 4),
    contents(M, Text).
test(kill, Text == 'a.\n% full\n') :-
    mode(prolog, 'a. % c1\n% full\n', M),
    send(M, caret, 0),
    send(M, kill_comment),
    contents(M, Text).
test(kill_lines, Text == 'a.\nb.\nc. % c3\n') :-
    mode(prolog, 'a. % c1\nb. % c2\nc. % c3\n', M),
    send(M, caret, 0),
    send(M, kill_comment, 2),
    contents(M, Text).

%   M-; on a line without comment, but a % in a string

test(insert_not_in_string, Text == 'x("a%b").       % \n') :-
    mode(prolog, 'x("a%b").\n', M),
    send(M, comment_column, 16),
    send(M, caret, 0),
    send(M, insert_line_comment),
    contents(M, Text).

%   C-x C-; (comment_line) toggles the comment of the line, or of
%   Times lines, and moves to the line after them.

test(comment_line, [Text, Caret] == ['%a.\nb.\n', 4]) :-
    mode(prolog, 'a.\nb.\n', M),
    send(M, caret, 1),
    send(M, comment_line),
    contents(M, Text),
    get(M, caret, Caret).
test(uncomment_lines, [Text, Caret] == ['a.\nb.\n%c.\n', 6]) :-
    mode(prolog, '%a.\n%b.\n%c.\n', M),
    send(M, caret, 0),
    send(M, comment_line, 2),
    contents(M, Text),
    get(M, caret, Caret).
test(comment_line_region, Text == '%a.\n%b.\nc.\n') :-
    mode(prolog, 'a.\nb.\nc.\n', M),
    send(M, selection, 0, 6, active),
    send(M, comment_line),
    contents(M, Text).
test(comment_line_c, Text == '//int x;\n') :-
    mode(c, 'int x;\n', M),
    send(M, caret, 0),
    send(M, comment_line),
    contents(M, Text).

:- end_tests(comment_column).

                 /*******************************
                 *             ALIGN            *
                 *******************************/

:- begin_tests(align).

test(arguments, Text == 't(a,   1,   x).\nt(bbb, 22,  yy).\nt(c,   333, z).\n') :-
    region_mode(prolog, 't(a, 1, x).\nt(bbb, 22, yy).\nt(c,333,z).\n', M),
    send(M, align_region),
    contents(M, Text).
test(items, Text == '  p(a)     :- q.\n  pp(a, b) :- q.\n') :-
    region_mode(prolog, '  p(a) :- q.\n  pp(a, b) :- q.\n', M),
    send(M, align_region),
    contents(M, Text).
test(no_columns, Text == Orig) :-
    Orig = '[ a-1,\n  bbb-22,\n  cc-3\n]\n',
    region_mode(prolog, Orig, M),
    send(M, align_region),
    contents(M, Text).
test(quoted, Text == 't("a,b", 1).\nt(x,     2).\nt(0\',,   3).\n') :-
    region_mode(prolog, 't("a,b", 1).\nt(x, 2).\nt(0\',, 3).\n', M),
    send(M, align_region),
    contents(M, Text).
test(sections, Text == 't(a,   1).\nt(bbb, 2).\n\nt(cc, 3).\nt(d,  4).\n') :-
    region_mode(prolog, 't(a, 1).\nt(bbb, 2).\n\nt(cc, 3).\nt(d, 4).\n', M),
    send(M, align_region),
    contents(M, Text).
test(current, Text == 't(a, 1).\nt(bbb, 2).\n\nt(cc, 3).\nt(d,  4).\n') :-
    mode(prolog, 't(a, 1).\nt(bbb, 2).\n\nt(cc, 3).\nt(d, 4).\n', M),
    send(M, caret, 22),
    send(M, align_current),
    contents(M, Text).
test(comments, Text == 't(a,   1). % a\nt(bbb, 2). % b\n') :-
    region_mode(prolog, 't(a, 1). % a\nt(bbb, 2). % b\n', M),
    send(M, comment_column, 4),
    send(M, align_region),
    contents(M, Text).
test(tabularise, Text == 't(a,   1).\nt(bbb, 2).\n') :-
    region_mode(prolog, 't(a, 1).\nt(bbb, 2).\n', M),
    send(M, tabularise),
    contents(M, Text).

:- end_tests(align).
