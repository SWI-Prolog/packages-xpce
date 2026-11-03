/*  Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025-2026, SWI-Prolog Solutions b.v.
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

:- module(lsp_registry,
          [ lsp_server/3,               % ?Id, +Root, -Config
            lsp_mode_server/2,          % ?Mode, ?Id
            lsp_role_server/3,          % +Mode, ?Role, -Id
            project_root/3              % +File, -Root, +Options
          ]).
:- use_module(library(pce)).
:- autoload(library(option), [option/3]).
:- autoload(library(process), [process_create/3]).
:- autoload(library(broadcast), [listen/3, unlisten/1]).
:- use_module(library(debug), [debugging/1]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(filesex), [directory_file_path/3]).
:- autoload(library(lists), [member/2]).
:- autoload(library(emacs/lsp_client), [lsp_buffer_opened/1]).

:- multifile
    lsp_server/2,                       % ?LSPId, -Config
    lsp_argument/3.                     % ?LSPId, +Root, -Arg

/** <module> Reason about available LSP servers

This module is the registry of LSP (Language Server Protocol) servers
for PceEmacs.  It is loaded with PceEmacs and is small: if a buffer is
opened in a mode for which a server is registered, it loads
library(emacs/lsp_client), which connects the buffer to the server(s).

The registry is the multifile predicate lsp_server/2.  For example,
to use `vale-ls` for Markdown files, add this to the PceEmacs
initialization file (see `Help/Customise`):

```
:- multifile lsp_registry:lsp_server/2.

lsp_registry:lsp_server('vale-ls',
                        #{ executable: path('vale-ls'),
                           modes:      [markdown],
                           roles:      [spelling]
                         }).
```
*/

:- initialization
    pce_define_type(lsp_tag, name).     % symbol, completed using the server

:- initialization
    ( unlisten(lsp_registry),
      listen(lsp_registry, pce_emacs(opened(Buffer)), lsp_opened(Buffer))
    ).

%!  lsp_opened(+Buffer) is semidet.
%
%   Called if PceEmacs opens Buffer.  Loads the LSP client and connects
%   Buffer to the LSP servers for its mode, if there are any.

lsp_opened(Buffer) :-
    get(Buffer, mode, Mode),
    lsp_mode_server(Mode, _),
    !,
    lsp_buffer_opened(Buffer).

%!  lsp_server(?Id, +Root, -Config) is nondet.
%
%   True when Id is the identifier for a n LSP server to be started in a
%   workspace at Root with given configuration Config.  Config is the
%   configuration from lsp_server/2 with `arguments` set to the list
%   of arguments from lsp_argument/3.

lsp_server(Id, Root, Config) :-
    lsp_server(Id, Config0),
    findall(Arg, lsp_argument(Id, Root, Arg), Argv),
    Config = Config0.put(arguments, Argv).

%!  lsp_mode_server(?Mode, ?Id) is nondet.
%
%   True when LSP server Id serves files in Mode.  This does not depend
%   on the project, so it can be tested cheaply before finding the
%   project root.

lsp_mode_server(Mode, Id) :-
    lsp_server(Id, Config),
    memberchk(Mode, Config.modes).

%!  lsp_role_server(+Mode, ?Role, -Id) is nondet.
%
%   True when LSP server Id plays Role for files in Mode.  If Role is
%   unbound or `@default`, any server for Mode qualifies.

lsp_role_server(Mode, Role, Id) :-
    (   var(Role)
    ;   Role == @default
    ),
    !,
    lsp_mode_server(Mode, Id).
lsp_role_server(Mode, Role, Id) :-
    lsp_server(Id, Config),
    memberchk(Mode, Config.modes),
    memberchk(Role, Config.get(roles, [])).

%!  lsp_server(?Id, -Config) is nondet.
%
%   Registry of LSP servers.  This predicate is multifile.  Config is a
%   dict with the keys below.  Arguments that depend on the project are
%   provided by lsp_argument/3.
%
%     - executable
%       File specification of the server, e.g., path(clangd).
%     - modes
%       List of PceEmacs modes the server serves.
%     - roles
%       List of roles the server plays, e.g. `highlight`, `symbol`,
%       `complete` or `spelling`.  A mode asks for a server by role
%       (see `emacs_language_mode<-lsp_client`).  The class variable
%       `lsp_roles` of a mode overrules this.
%     - change
%       If `1`, only send full changes to the server rather than
%       incremental ones.
%
%   PceEmacs registers `clangd` for C and C++ if it can be found in
%   `PATH`, unless the Prolog flag `clangd` is `false`.

lsp_server(clangd,
           #{ executable: path(clangd),
              modes:      [c,cpp],
              roles:      [highlight, symbol, complete]
            }) :-
    \+ current_prolog_flag(clangd, false),
    absolute_file_name(path(clangd), _,
                       [ access(execute),
                         file_errors(fail)
                       ]).

%!  lsp_argument(+Id, +Root, -Arg) is nondet.
%
%   Provide additional arguments for LSP server Id to run in a workspace
%   rooted by Root.  This predicate is multifile.

lsp_argument(clangd, Root, Arg) :-
    compile_commands_file(Root, CompileCommandsFile),
    file_directory_name(CompileCommandsFile, Dir),
    format(atom(Arg), '--compile-commands-dir=~w', [Dir]).
lsp_argument(clangd, _, Arg) :-
    (   debugging(lsp(log(clangd, Level))),
        atom(Level)
    ->  must_be(oneof([error,info,verbose]), Level)
    ;   Level = error
    ),
    format(atom(Arg), '--log=~w', [Level]).


                /*******************************
                *           PROJECT            *
                *******************************/

%!  project_root(+File, -Root, +Options) is det.
%
%   Find the project root given a specific file.  Options:
%
%     - from(?Source)
%       Source is the way the root was found.  Currently supported
%       ways:
%
%         - git
%           Find the git root directory.  Finds the toplevel project
%           when inside a submodule.
%         - file
%           Last resort, returning the directory of File.
%     - mode(+Mode)
%       Edit mode we will use for File.

:- discontiguous
    project_root/4.

:- det(project_root/3).
project_root(File, Root, Options) :-
    option(from(Source), Options, _),
    project_root(Source, File, Root, Options),
    !.

%!  project_root(+From, +File, -Root, +Options) is semidet.
%
%   Find the project root using some technique.

                                        % GIT support
project_root(git, File, Root, Options) :-
    catch(project_root_(git, File, Root, Options),
          error(_,_),
          fail).

project_root_(git, File, Root, _Options) :-
    (   dir_from_git(File, '--show-superproject-working-tree', Root0)
    ->  Root = Root0
    ;   dir_from_git(File, '--show-toplevel', Root)
    ).

dir_from_git(File, How, Root) :-
    file_directory_name(File, Dir),
    setup_call_cleanup(
        process_create(path(git),
                       [ '-C', Dir, 'rev-parse', How ],
                       [ stdout(pipe(Out)),
                         stderr(null)
                       ]),
        read_string(Out, _, Result),
        close(Out)),
    split_string(Result, "\n", "\n\r\t\s", [Line]),
    Line \== "",
    atom_string(Root, Line).

                                        % Last resort: from file.
project_root(file, File, Root, _Options) :-
    file_directory_name(File, Root).


                /*******************************
                *        CMAKE SUPPORT         *
                *******************************/

%!  compile_commands_file(+Root, -File) is semidet.
%
%   True when File is the absolute file path for the most recently
%   modified `compile_commands.json` in Root or one of its `build*`
%   subdirectories.

compile_commands_file(Root, File) :-
    findall(Time-F,
            ( member(Rel, ['compile_commands.json',
                           'build*/compile_commands.json']),
              directory_file_path(Root, Rel, Pattern),
              expand_file_name(Pattern, Files),
              member(F, Files),
              exists_file(F),
              time_file(F, Time)
            ),
            Pairs),
    sort(1, >=, Pairs, [_-File|_]).
