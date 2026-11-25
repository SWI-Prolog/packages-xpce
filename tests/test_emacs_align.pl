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

test_emacs_align :-
    run_tests([ editor_align
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
