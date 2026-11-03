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

:- module(test_emacs_lsp, [test_emacs_lsp/0]).

/** <module> The LSP server registry of PceEmacs

library(emacs/lsp_registry) is loaded with PceEmacs.  It tells which
LSP servers serve a mode and loads library(emacs/lsp_client) only if a
buffer is opened in such a mode.

Run with:

    swipl -g test_emacs_lsp -t halt packages/xpce/tests/test_emacs_lsp.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(pce_emacs)).
:- use_module(library(emacs/lsp_registry)).

test_emacs_lsp :-
    run_tests([ emacs_lsp ]).

:- multifile
    lsp_registry:lsp_server/2,
    lsp_registry:lsp_argument/3.

lsp_registry:lsp_server(test_ls,
                        #{ executable: path(true),
                           modes:      [lsp_test_mode],
                           roles:      [spelling]
                         }).
lsp_registry:lsp_argument(test_ls, Root, Arg) :-
    format(atom(Arg), '--root=~w', [Root]).

%!  with_flag(+Flag, +Value, :Goal)
%
%   Run Goal with Flag set to Value.  A flag that did not exist is set
%   to `true` afterwards, which the registry handles as absent.

:- meta_predicate with_flag(+, +, 0).

with_flag(Flag, Value, Goal) :-
    (   current_prolog_flag(Flag, Old)
    ->  true
    ;   Old = true
    ),
    setup_call_cleanup(set_prolog_flag(Flag, Value),
                       Goal,
                       set_prolog_flag(Flag, Old)).

%!  with_path(+Dir, :Goal)
%
%   Run Goal with PATH set to Dir.

:- meta_predicate with_path(+, 0).

with_path(Dir, Goal) :-
    getenv('PATH', Old),
    setup_call_cleanup(setenv('PATH', Dir),
                       Goal,
                       setenv('PATH', Old)).

%!  with_dir(-Dir, :Goal)

:- meta_predicate with_dir(-, 0).

with_dir(Dir, Goal) :-
    tmp_directory(emacs_lsp, Dir),
    call_cleanup(Goal, delete_directory_and_contents(Dir)).

%!  touch(+Dir, +Rel, +Time)
%
%   Create Dir/Rel with modification time Time.

touch(Dir, Rel, Time) :-
    directory_file_path(Dir, Rel, File),
    file_directory_name(File, FileDir),
    make_directory_path(FileDir),
    setup_call_cleanup(open(File, write, Out), true, close(Out)),
    set_time_file(File, _, [modified(Time)]).

:- begin_tests(emacs_lsp, [condition(current_prolog_flag(threads, true))]).

test(mode_server, Ids == [test_ls]) :-
    findall(Id, lsp_mode_server(lsp_test_mode, Id), Ids).
test(no_mode_server, fail) :-
    lsp_mode_server(lsp_no_such_mode, _).
test(role_server, Id == test_ls) :-
    lsp_role_server(lsp_test_mode, spelling, Id).
test(role_server_default, Id == test_ls) :-
    lsp_role_server(lsp_test_mode, @default, Id).
test(role_server_other_role, fail) :-
    lsp_role_server(lsp_test_mode, highlight, _).
test(arguments, Args == ['--root=/some/root']) :-
    lsp_server(test_ls, '/some/root', Config),
    get_dict(arguments, Config, Args).
test(clangd_flag, fail) :-
    with_flag(clangd, false, lsp_mode_server(c, clangd)).
test(clangd_not_in_path, fail) :-
    with_dir(Dir, with_path(Dir, lsp_mode_server(c, clangd))).
test(compile_commands_newest, File == Expected) :-
    with_dir(Dir,
             ( touch(Dir, 'compile_commands.json', 1000),
               touch(Dir, 'build/compile_commands.json', 2000),
               touch(Dir, 'build.x/compile_commands.json', 3000),
               directory_file_path(Dir, 'build.x/compile_commands.json',
                                   Expected),
               lsp_registry:compile_commands_file(Dir, File)
             )).
test(compile_commands_root, File == Expected) :-
    with_dir(Dir,
             ( touch(Dir, 'compile_commands.json', 3000),
               touch(Dir, 'build/compile_commands.json', 2000),
               directory_file_path(Dir, 'compile_commands.json', Expected),
               lsp_registry:compile_commands_file(Dir, File)
             )).
test(compile_commands_not_cwd, fail) :-
    with_dir(Dir,
             with_dir(Cwd,
                      ( touch(Cwd, 'compile_commands.json', 1000),
                        setup_call_cleanup(
                            working_directory(Old, Cwd),
                            lsp_registry:compile_commands_file(Dir, _),
                            working_directory(_, Old))
                      ))).
test(no_client_without_server,
     [ condition(\+ current_module(lsp_client)),
       true(\+ current_module(lsp_client))
     ]) :-
    start_emacs,
    with_dir(Dir,
             ( directory_file_path(Dir, 'test.txt', File),
               touch(Dir, 'test.txt', 1000),
               new(_, emacs_buffer(File)) % broadcasts pce_emacs(opened(B))
             )).

:- end_tests(emacs_lsp).
