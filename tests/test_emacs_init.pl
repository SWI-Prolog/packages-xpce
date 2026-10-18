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

:- module(test_emacs_init, [test_emacs_init/0]).

/** <module> The init file of PceEmacs

PceEmacs loads user_app_config('xpce/emacs/init'), i.e., init.pl in
the xpce/emacs directory of the user's configuration, and tells users
who still have the old ~/.pceemacsrc or pceemacs.ini to move it.

Run with:

    swipl -g test_emacs_init -t halt packages/xpce/tests/test_emacs_init.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(filesex)).
:- use_module(library(emacs/application)).

test_emacs_init :-
    run_tests([ emacs_init ]).

%!  with_dir(-Dir, :Goal)
%
%   Run Goal with a new temporary directory Dir.

:- meta_predicate with_dir(-, 0).

with_dir(Dir, Goal) :-
    tmp_file(emacs_init, Dir),
    make_directory(Dir),
    call_cleanup(Goal, delete_directory_and_contents(Dir)).

%!  with_path(+Alias, +Dir, :Goal)
%
%   Run Goal with Dir as the first directory of Alias.

:- meta_predicate with_path(+, +, 0).

with_path(Alias, Dir, Goal) :-
    setup_call_cleanup(
        asserta(user:file_search_path(Alias, Dir), Ref),
        Goal,
        erase(Ref)).

touch(File) :-
    file_directory_name(File, Dir),
    make_directory_path(Dir),
    setup_call_cleanup(open(File, write, Out),
                       format(Out, '% init~n', []),
                       close(Out)).

:- begin_tests(emacs_init).

test(init_file, Found == Expected) :-
    with_dir(Dir,
             ( directory_file_path(Dir, 'xpce/emacs/init.pl', Expected),
               touch(Expected),
               with_path(user_app_config, Dir,
                         emacs_application:emacs_init_file(Found))
             )).
test(no_init_file, fail) :-
    with_dir(Dir,
             with_path(user_app_config, Dir,
                       ( emacs_application:emacs_init_file(File),
                         sub_atom(File, 0, _, _, Dir)
                       ))).
test(old_init_file, Found == Expected) :-
    with_dir(Dir,
             ( directory_file_path(Dir, '.pceemacsrc', Expected),
               touch(Expected),
               with_path(user_profile, Dir,
                         emacs_application:old_emacs_init_file(Found))
             )).
test(old_init_file_message, true(sub_string(Msg, _, _, _, "Please move"))) :-
    message_to_codes(emacs(old_init_file('/old/.pceemacsrc')), Msg).

:- end_tests(emacs_init).

message_to_codes(Term, String) :-
    phrase(prolog:message(Term), Lines),
    with_output_to(string(String), print_message_lines(current_output, '', Lines)).
