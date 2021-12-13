#!/usr/bin/env swipl

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This script may be used to open   edit windows on an existing SWI-Prolog
instance that has the built-in editor PceEmacs open. It works similar to
`emacsclient` for GNU-Emacs.

To make use of this to the max on Linux systems we must:

  - enable terminal hyperlinks using this in `~/.config/swi-prolog/init.pl`

       :- set_prolog_flag(hyperlink_term, true).

  - Create a desktop application link by adding a file to
    `~/.local/share/applications`, e.g. `edit.desktop`. This file
    contains something like below.  The ``%u`` ensures the entire
    URL from the hyperlink is passed

```
[Desktop Entry]
Exec=edit --no-wait %u
MimeType=application/x-perl;
Name=PceEmacs
Encoding=UTF-8
Type=Application
Terminal=false
Icon=/home/janw/.local/share/applications/swipl.png
Categories=Utility;Development;TextEditor;
Keywords=Text;Editor;

```

  - Install this script in ``$PATH`` as `edit` and make it executable

  - Add entries to the ``~/.config/mimeapps.list`` for the mime-types
    you want handled by this editor.  To the minimum add `x-perl` for
    *.pl files (or update the mime database to change .pl into
    ``application/x-prolog``).

        application/x-perl=edit.desktop

After these changes, errors and warnings from SWI-Prolog in the terminal
become clickable.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- use_module(library(socket)).
:- use_module(library(dcg/basics)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(socket)).
:- use_module(library(uri)).

:- initialization(main,main).

main(Argv) :-
    argv_options(Argv, Positional, Options),
    (   Positional = [Spec]
    ->  target(Spec, File, FOptions),
        absolute_file_name(File, AbsFile),
        append(Options, FOptions, EOptions),
        call_editor(AbsFile, EOptions)
    ;   argv_usage(debug),
        halt(1)
    ).

opt_type(wait, wait, boolean).
opt_help(wait, "When false, do not wait").
opt_help(help(usage), "edit [--no-wait] Spec").

target(Spec, File, Options) :-
    sub_atom(Spec, 0, _, _, 'file://'),
    !,
    uri_file_name(Spec, File),
    uri_components(Spec, Components),
    uri_data(fragment, Components, Fragment),
    (   nonvar(Fragment)
    ->  atom_codes(Fragment, Codes),
        phrase(fragment_line(Options), Codes)
    ;   Options = []
    ).
target(Spec, File, Options) :-
    split_string(Spec, ":", "", Parts),
    (   append(Pre, [LS,CS], Parts),
        number_string(Line, LS),
        number_string(Column, CS)
    ->  atomic_list_concat(Pre, :, File),
        Options = [line(Line), column(Column)]
    ;   append(Pre, [LS], Parts),
        number_string(Line, LS)
    ->  atomic_list_concat(Pre, :, File),
        Options = [line(Line)]
    ;   File = Spec,
        Options = []
    ).

fragment_line([line(Line)|Extra]) -->
    opt_L,
    integer(Line),
    (   ":"
    ->  integer(Column),
        {Extra = [column(Column)]}
    ;   {Extra = []}
    ).

opt_L -->
    "L",
    !.
opt_L -->
    "".

call_editor(File, Options) :-
    pce_emacs_command(File, Options, Command),
    server_socket(SocketFile),
    unix_domain_socket(Socket),
    catch(tcp_connect(Socket, SocketFile),
          _,
          (tcp_close_socket(Socket), fail)),
    !,
    tcp_open_socket(Socket, Stream),
    format(Stream, '~q~n', [Command]),
    flush_output(Stream),
    (   option(wait(true), Options, true)
    ->  copy_stream_data(Stream, user_error)
    ;   true
    ).
call_editor(File, Options) :-
    (   option(line(Line), Options)
    ->  atom_concat(+, Line, Av1),
        Argv = [Av1|Argv1]
    ;   Argv = Argv1
    ),
    Argv1 = [File],
    process_create(path(emacs), Argv, []).


pce_emacs_command(File, EOptions, edit(File, Line, Column, Wait)) :-
    option(line(Line), EOptions, []),
    option(column(Column), EOptions, []),
    (   option(wait(false), EOptions)
    ->  Wait = nowait
    ;   Wait = wait
    ).

server_socket(Socket) :-
    getenv('DISPLAY', Display),
    split_string(Display, ":", "", ["",NS]),
    !,
    server_base(Base),
    format(atom(Socket), '~w.~w', [Base, NS]).
server_socket(Socket) :-
    server_base(Socket).

server_base(Socket) :-
    absolute_file_name(app_config(xpce), Dir,
                       [ file_type(directory),
                         access(read)
                       ]),
    directory_file_path(Dir, emacs_server, Socket).
