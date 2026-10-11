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


:- module(xpce_test_sandbox,
          [ sandbox_app_config/1,       % -Dir
            sandbox_dir/2,              % +Prefix, -Dir
            remove_sandbox_at_exit/0
          ]).
:- use_module(library(filesex), [directory_file_path/3,
                                  make_directory_path/1,
                                  delete_directory_and_contents/1]).
:- use_module(library(lists), [member/2]).
:- autoload(library(pce), [send/3]).

/** <module> Keep tests out of the user's xpce configuration

Tests that open PceEmacs, the IDE or a persistent_frame write bookmarks,
window arrangements and geometry to the xpce configuration.  A test
calls sandbox_app_config/1 before xpce is initialised to use a
temporary configuration instead, and remove_sandbox_at_exit/0 right
after loading library(pce).
*/

:- dynamic
    created_dir/1.

%!  sandbox_app_config(-Dir) is det.
%
%   Use a new temporary directory Dir as `user_app_config`.  This must
%   be called before xpce is initialised.  The `xpce` subdirectory is
%   created, as an existing config directory is preferred.

sandbox_app_config(Dir) :-
    sandbox_dir(xpce_config, Dir),
    directory_file_path(Dir, xpce, XpceDir),
    make_directory_path(XpceDir),
    asserta(user:file_search_path(user_app_config, Dir)).

%!  sandbox_dir(+Prefix, -Dir) is det.
%
%   Dir is a new temporary directory that is removed when xpce exits.

sandbox_dir(Prefix, Dir) :-
    tmp_file(Prefix, Dir),
    make_directory(Dir),
    assertz(created_dir(Dir)).

%!  remove_sandbox_at_exit is det.
%
%   Delete the directories of this module when xpce exits.  xpce calls
%   its exit messages in the reverse order of registration and after
%   the at_halt/1 hooks.  Call this right after loading library(pce),
%   so the directories are removed after the exit messages that save,
%   such as the one that saves the geometry of a persistent_frame.

remove_sandbox_at_exit :-                 % @(...): no pce operators
    send(@(pce), exit_message,
         message(@(prolog), call, prolog(xpce_test_sandbox:remove_dirs))).

:- public remove_dirs/0.

remove_dirs :-
    forall(created_dir(Dir),
           catch(delete_directory_and_contents(Dir), _, true)).
