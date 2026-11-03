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

:- module(test_emacs_c_mode, [test_emacs_c_mode/0]).

/** <module> PceEmacs C and C++ mode

Test the parts of the C and C++ modes that do not need an LSP server:
syntax colouring, keywords, the styles C++ inherits from C, and the
commands that fall back to tags if there is no server.  The tests run
with the Prolog flag `clangd` set to `false`.

Run with:

    swipl -g test_emacs_c_mode -t halt packages/xpce/tests/test_emacs_c_mode.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(pce_emacs)).
:- use_module(library(emacs_extend), [emacs_mode_style/3]).

test_emacs_c_mode :-
    run_tests([ emacs_c_mode ]).

:- multifile
    emacs_cpp_mode:style/2.

emacs_cpp_mode:style(lsp(macro), [colour(green)]).

source(c,   "// line\n/* block */ static int f(void) { return 0; }\n").
source(cpp, "// line\nnamespace n { class C { public: bool b; }; }\n").

%!  with_editor(+Ext, -Editor, :Goal)
%
%   Run Goal with an editor on a file with extension Ext holding the
%   source/2 for Ext.

:- meta_predicate with_editor(+, -, 0).

with_editor(Ext, E, Goal) :-
    source(Ext, Text),
    tmp_file_stream(File, Out, [extension(Ext), encoding(utf8)]),
    write(Out, Text),
    close(Out),
    call_cleanup(
        ( new(B, emacs_buffer(File)),
          send(B, open),
          get(B?editors, head, E),
          Goal
        ),
        delete_file(File)).

syntax_fragments(E, Pairs) :-
    get(E, mode, M),
    send(M, colourise_buffer),
    get(E, text_buffer, TB),
    get(TB, find_all_fragments, @arg1?class_name == emacs_colour_fragment, Ch),
    chain_list(Ch, L),
    findall(S-St, ( member(F, L),
                    get(F, style, St),
                    get(F?string, value, S)
                  ), Pairs).

has_style(E, Name) :-
    get(E, styles, Styles),
    get(Styles, value, Name, _).

:- begin_tests(emacs_c_mode,
               [ condition(current_prolog_flag(threads, true)),
                 setup((set_prolog_flag(clangd, false), start_emacs)),
                 cleanup(set_prolog_flag(clangd, true))
               ]).

test(c_mode, Mode == c) :-
    with_editor(c, E, get(E?mode, name, Mode)).
test(cpp_mode, Mode == cpp) :-
    with_editor(cpp, E, get(E?mode, name, Mode)).
test(c_syntax, Pairs == [ '// line'-comment, '/* block */'-comment,
                          static-qualifier, int-type, void-type,
                          return-control ]) :-
    with_editor(c, E, syntax_fragments(E, Pairs)).
test(cpp_syntax, Pairs == [ '// line'-comment, namespace-definition,
                            class-definition, (public)-qualifier,
                            bool-type ]) :-
    with_editor(cpp, E, syntax_fragments(E, Pairs)).
test(cpp_keyword_type, Types == [definition, type, control]) :-
    with_editor(cpp, E,
                ( get(E, mode, M),
                  findall(T, ( member(K, [class, int, return]),
                               get(M, keyword_type, K, T)
                             ), Types)
                )).
test(cpp_inherits_styles, A == [bold(true)]) :-
    emacs_mode_style(cpp, lsp(function), A).
test(cpp_overrides_style, CA-CppA == [colour('#006e6e')]-[colour(green)]) :-
    emacs_mode_style(c, lsp(macro), CA),
    emacs_mode_style(cpp, lsp(macro), CppA).
test(cpp_style_enum, Macros == [[colour(green)]]) :- % the override only
    findall(A, ( emacs_mode_style(cpp, Class, A),
                 Class == lsp(macro)
               ), Macros).
test(c_editor_styles) :-
    with_editor(c, E,
                ( assertion(has_style(E, comment)),
                  assertion(has_style(E, 'lsp(function)'))
                )).
test(cpp_editor_styles) :-
    with_editor(cpp, E,
                ( assertion(has_style(E, definition)),
                  assertion(has_style(E, 'lsp(class)'))
                )).
test(no_server_fallback,
     [ condition(\+ current_module(lsp_client)),
       true(\+ current_module(lsp_client))
     ]) :-
    with_editor(c, E,
                ( get(E, mode, M),
                  assertion(\+ get(M, lsp_client, symbol, _)),
                  send(M, find_references)
                )).

:- end_tests(emacs_c_mode).
