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

:- module(test_emacs_lsp_editor, [test_emacs_lsp_editor/0]).

/** <module> PceEmacs editor features for LSP

Test the parts of library(emacs/lsp_highlight) and
library(emacs/lsp_diagnostics) that do not need a server: turning
semantic tokens and diagnostics into fragments, the diagnostic styles
and margin, and moving between diagnostics.

Run with:

    swipl -g test_emacs_lsp_editor -t halt packages/xpce/tests/test_emacs_lsp_editor.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(pce_emacs)).
:- use_module(library(emacs/lsp_client)).

test_emacs_lsp_editor :-
    run_tests([ emacs_lsp_editor ]).

:- multifile
    emacs_lsp_test_mode:style/2,        % as emacs_begin_mode/5 declares
    emacs_lsp_test_mode:def_style/2,
    lsp_diagnostics:suppress_diagnostic/5.

emacs_lsp_test_mode:style(lsp(type), [bold(true)]).

lsp_diagnostics:suppress_diagnostic(_Buffer, _LSP, _Start, _Len, Diagnostic) :-
    Diagnostic.get(message) == "suppress me".

%   "/* 😀 */ int foo;": the code point above U+FFFF is two UTF-16
%   units, so `int` is at character 9 but index 8.

text("/* \x1F600\ */ int foo;\nint bar;\n").

fragments(TB, Class, Fragments) :-
    get(TB, find_all_fragments, message(@arg1, instance_of, Class), Chain),
    chain_list(Chain, List),
    maplist(fragment, List, Fragments).

fragment(F, Start-Len-Style) :-
    get(F, start, Start),
    get(F, length, Len),
    get(F, style, Style).

diagnostic(Line0-Char0, Line1-Char1, Severity, Message,
           #{ range: #{ start: #{line:Line0, character:Char0},
                        end:   #{line:Line1, character:Char1}
                      },
              severity: Severity,
              message: Message
            }).

%!  with_editor(-Editor, :Goal)
%
%   Run Goal with a C mode editor on a file holding text/1.

:- meta_predicate with_editor(-, 0).

with_editor(E, Goal) :-
    text(Text),
    tmp_file_stream(File, Out, [extension(c), encoding(utf8)]),
    write(Out, Text),
    close(Out),
    setup_call_cleanup(
        true,
        ( start_emacs,
          new(B, emacs_buffer(File)),
          send(B, open),
          get(B?editors, head, E),
          Goal
        ),
        delete_file(File)).

selection(E, Start-End) :-
    get(E, selection, P),
    object(P, point(Start, End)).

:- begin_tests(emacs_lsp_editor,
               [ condition(current_prolog_flag(threads, true)),
                 setup((set_prolog_flag(clangd, false), start_emacs)),
                 cleanup(set_prolog_flag(clangd, true))
               ]).

test(token_utf16, Fragments == [8-3-'lsp(type)', 17-3-'lsp(type)']) :-
    text(Text),
    new(TB, text_buffer(Text)),
    Styles = styles('lsp(type)', -),    % type 0 is styled, type 1 not
    lsp_highlight:highlight_tokens([0,9,3,0,0,   0,4,3,1,0,
                                    1,0,3,0,0,   0,4,3,1,0],
                                   Styles, @nil, TB, 0, 0, 0, Count),
    assertion(Count == 2),
    fragments(TB, emacs_lsp_fragment, Fragments).
test(token_styles, Styles == styles('lsp(type)', -)) :-
    LSP = test_lsp,
    setup_call_cleanup(
        ( assertz(lsp_client:token_type(LSP, 0, type)),
          assertz(lsp_client:token_type(LSP, 1, variable))
        ),
        lsp_highlight:token_styles(LSP, lsp_test, Styles),
        retractall(lsp_client:token_type(LSP, _, _))).
test(diagnostic_utf16, Fragments == [12-3-lsp_diag_error]) :-
    text(Text),
    new(TB, text_buffer(Text)),
    diagnostic(0-13, 0-16, 1, "bad foo", D),
    lsp_diagnostics:show_diagnostic(TB, @nil, 0, D),
    fragments(TB, emacs_lsp_diagnostic, Fragments).
test(diagnostic_line_offset, Fragments == [21-3-lsp_diag_warning]) :-
    text(Text),
    new(TB, text_buffer(Text)),
    diagnostic(0-4, 0-7, 2, "bad bar", D),   % line 0 of a region at line 1
    lsp_diagnostics:show_diagnostic(TB, @nil, 1, D),
    fragments(TB, emacs_lsp_diagnostic, Fragments).
test(diagnostic_suppressed, Fragments == []) :-
    text(Text),
    new(TB, text_buffer(Text)),
    diagnostic(0-13, 0-16, 1, "suppress me", D),
    lsp_diagnostics:show_diagnostic(TB, @nil, 0, D),
    fragments(TB, emacs_lsp_diagnostic, Fragments).
test(margin_and_styles, [Width-HasStyle == 22-true]) :-
    with_editor(E,
                ( get(E, mode, M),
                  get(E, text_buffer, TB),
                  diagnostic(0-13, 0-16, 1, "bad foo", D),
                  lsp_diagnostics:show_diagnostic(TB, @nil, 0, D),
                  send(M, lsp_enable_margin),
                  get(E, margin_width, Width),
                  get(E, styles, Styles),
                  (   get(Styles, value, lsp_diag_error, _)
                  ->  HasStyle = true
                  ;   HasStyle = false
                  )
                )).
test(no_margin_without_diagnostics, Width == 0) :-
    with_editor(E,
                ( get(E, mode, M),
                  send(M, lsp_enable_margin),
                  get(E, margin_width, Width)
                )).
test(goto_error, Selections == [(12-15)-(21-24), (21-24)-(12-15)]) :-
    with_editor(E,
                ( get(E, mode, M),
                  get(E, text_buffer, TB),
                  diagnostic(0-13, 0-16, 1, "bad foo", D1),
                  diagnostic(1-4, 1-7, 2, "bad bar", D2),
                  lsp_diagnostics:show_diagnostic(TB, @nil, 0, D1),
                  lsp_diagnostics:show_diagnostic(TB, @nil, 0, D2),
                  send(E, caret, 0),
                  send(M, goto_next_error), selection(E, S1),
                  send(M, goto_next_error), selection(E, S2),
                  send(E, caret, 30),
                  send(M, goto_prev_error), selection(E, S3),
                  send(M, goto_prev_error), selection(E, S4),
                  Selections = [S1-S2, S3-S4]
                )).

:- end_tests(emacs_lsp_editor).
