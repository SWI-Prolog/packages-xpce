/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(test_theme_file,
          [ test_theme_file/0
          ]).

/** <module> Test loading and saving theme files

Tests library(pce_theme_file).  The theme files are written to the
library directory of a sandbox configuration directory.
*/

:- use_module(library(filesex), [directory_file_path/3,
                                  make_directory_path/1]).
:- use_module(xpce_test_sandbox).

%   The sandbox has no lib/theme directory.  A second configuration
%   directory with lower priority has one.  New themes must be saved
%   in the sandbox.

sandbox_app_configs :-
    sandbox_app_config(Dir),
    asserta(sandbox(Dir)),
    sandbox_dir(xpce_other_config, Other),
    directory_file_path(Other, 'lib/theme', OtherThemeDir),
    make_directory_path(OtherThemeDir),
    assertz(user:file_search_path(user_app_config, Other)).

:- dynamic sandbox/1.
:- initialization(sandbox_app_configs, now).

:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(pce_theme)).
:- use_module(library(pce_theme_file)).

test_theme_file :-
    run_tests([ theme_file_dir,
                theme_file_read,
                theme_file_edit,
                theme_file_new
              ]).

fixture_text(
":- module(prolog_theme_test_fixture, []).
:- multifile
    prolog:theme/1,
    prolog:theme_base/2,
    prolog:console_color/3,
    pce_theme:colour/3.

prolog:theme(test_fixture).
prolog:theme_base(test_fixture, dark).

% Console colours

prolog:console_color(test_fixture, Class, Attributes) :-
    console_color(Class, Attributes).

console_color(prompt,       [bold]).            % the prompt
console_color(goal(_, odd), [fg(red)]).

% IDE colours

pce_theme:colour(test_fixture, syntax_comment, green).
pce_theme:colour(test_fixture, syntax_string, blue).
pce_theme:colour(test_fixture, test_no_such_colour, red).
").

fixture_file(File) :-
    sandbox(Dir),
    directory_file_path(Dir, 'lib/theme', ThemeDir),
    make_directory_path(ThemeDir),
    directory_file_path(ThemeDir, 'test_fixture.pl', File).

write_fixture(File) :-
    fixture_file(File),
    fixture_text(Text),
    setup_call_cleanup(open(File, write, Out),
                       write(Out, Text),
                       close(Out)).

remove_fixture :-
    forall(edited_theme(T, _), end_theme_edit(T)),
    apply_theme(light),
    fixture_file(File),
    (   source_file(File)
    ->  unload_file(File)
    ;   true
    ),
    delete_file(File).

file_text(Theme, Text) :-
    theme_file(Theme, File),
    read_file_to_string(File, Text, []).

contains(Text, Sub) :-
    sub_string(Text, _, _, _, Sub),
    !.

:- multifile user:message_hook/3.
:- dynamic   quiet/0.

%   The fixture has an invalid colour.  Do not print the warning.

user:message_hook(error(existence_error(semantic_colour, _), _), warning, _) :-
    quiet.

:- begin_tests(theme_file_dir).

test(user_dir, true(sub_atom(ThemeDir, 0, _, _, Dir))) :-
    %  A new theme is saved in the first user configuration directory,
    %  also if another one has a lib/theme directory.
    sandbox(Dir),
    pce_theme_file:user_theme_dir(ThemeDir).

:- end_tests(theme_file_dir).

:- begin_tests(theme_file_read,
               [ setup(write_fixture(_)),
                 cleanup(remove_fixture)
               ]).

test(read, [ Base == dark,
             Settings =@= [ console(prompt, [bold]),
                            console(goal(_, odd), [fg(red)]),
                            colour(syntax_comment, green),
                            colour(syntax_string, blue),
                            colour(test_no_such_colour, red)
                          ]
           ]) :-
    theme_file(test_fixture, File),
    read_theme_file(File, Base, Settings).
test(editable, true) :-
    editable_theme(test_fixture).
test(system_not_editable, fail) :-
    editable_theme(dark).

:- end_tests(theme_file_read).

:- begin_tests(theme_file_edit,
               [ setup(( load_theme_libraries,
                         write_fixture(File),
                         use_module(File),
                         asserta(quiet),
                         edit_theme(test_fixture),
                         retractall(quiet)
                       )),
                 cleanup(remove_fixture)
               ]).

test(edited, [Base,Modified] == [dark,false]) :-
    edited_theme(test_fixture, Base),
    ( theme_modified(test_fixture) -> Modified = true ; Modified = false ).
test(file_unloaded, true) :-
    %  The settings of the file are replaced by those of the edited theme
    findall(V, pce_theme:colour(test_fixture, syntax_comment, V), [green]).
%   Saving replaces the value and keeps the comment and alignment,
%   inserts a new fact after a fact of the same kind, deletes a removed
%   fact and keeps a fact that could not be loaded.  The online help
%   has no facts, so a section for it is added at the end.

test(save, [Missing,Present] == [[],[]]) :-
    set_theme_setting(test_fixture, colour(syntax_comment, red)),
    set_theme_setting(test_fixture, console(prompt, [underline])),
    reset_theme_setting(test_fixture, console(goal(_, odd), _)),
    set_theme_setting(test_fixture, console(input, [bg(blue)])),
    set_theme_setting(test_fixture, pldoc(code, true, [color(red)])),
    assertion(theme_modified(test_fixture)),
    save_theme(test_fixture),
    assertion(\+ theme_modified(test_fixture)),
    file_text(test_fixture, Text),
    exclude(contains(Text),
            [ "console_color(prompt,       [underline]).            \c
               % the prompt\nconsole_color(input,        [bg(blue)]).\n",
              "pce_theme:colour(test_fixture, syntax_comment, red).",
              "pce_theme:colour(test_fixture, test_no_such_colour, red).",
              ":- multifile pldoc_style:theme/4.\n\n\c
               pldoc_style:theme(test_fixture, Element, Condition, CSS) :-\n\c
               \x20   pldoc_theme(Element, Condition, CSS).\n\n\c
               pldoc_theme(code, true, [color(red)]).\n",
              "% IDE colours"
            ], Missing),
    include(contains(Text), ["goal(_, odd)"], Present).
test(end_edit, [ [Edited,Comment,Input] == [false,red,[bg(blue)]] ]) :-
    end_theme_edit(test_fixture),
    ( edited_theme(test_fixture, _) -> Edited = true ; Edited = false ),
    once(pce_theme:colour(test_fixture, syntax_comment, Comment)),
    once(prolog:console_color(test_fixture, input, Input)).
test(edit_again, Settings =@= [ console(prompt, [underline]),
                                console(input, [bg(blue)]),
                                colour(syntax_comment, red),
                                colour(syntax_string, blue),
                                pldoc(code, true, [color(red)])
                              ]) :-
    asserta(quiet),
    edit_theme(test_fixture),
    retractall(quiet),
    findall(S, theme_setting(test_fixture, S), Settings).
test(revert, Value == red) :-
    set_theme_setting(test_fixture, colour(syntax_comment, blue)),
    revert_theme(test_fixture),
    theme_setting(test_fixture, colour(syntax_comment, Value)).
test(unlink, [ Linked-Classes == false-[] ]) :-
    %  Without console colours, the clause that calls the local
    %  predicate is deleted, so the hook does not call an undefined
    %  predicate
    reset_theme_setting(test_fixture, console(prompt, _)),
    reset_theme_setting(test_fixture, console(input, _)),
    save_theme(test_fixture),
    file_text(test_fixture, Text),
    (   contains(Text, "console_color(Class, Attributes)")
    ->  Linked = true
    ;   Linked = false
    ),
    end_theme_edit(test_fixture),
    findall(C, prolog:console_color(test_fixture, C, _), Classes).

:- end_tests(theme_file_edit).

remove_new_themes :-
    forall(edited_theme(T, _), end_theme_edit(T)),
    apply_theme(light),
    forall(member(T, [test_new, test_new2]),
           (   absolute_file_name(user_app_config(lib/theme/T), File,
                                  [ file_type(prolog), access(exist),
                                    file_errors(fail)
                                  ])
           ->  (   source_file(File)
               ->  unload_file(File)
               ;   true
               ),
               delete_file(File)
           ;   true
           )).

:- begin_tests(theme_file_new,
               [ setup(load_theme_libraries),
                 cleanup(remove_new_themes)
               ]).

test(new, [ [Modified,Saved,Value,Base] == [true,false,red,dark] ]) :-
    create_theme(test_new, dark, [colour(syntax_comment, red)]),
    ( theme_modified(test_new) -> Modified = true ; Modified = false ),
    save_theme(test_new),
    ( theme_modified(test_new) -> Saved = true ; Saved = false ),
    end_theme_edit(test_new),
    apply_theme(test_new),
    theme_colour_value(test_new, syntax_comment, Value, _),
    once(prolog:theme_base(test_new, Base)).
test(new_sections, [Missing-Present == []-[]]) :-
    %  A new file only has a section for the kinds of settings
    file_text(test_new, Text),
    exclude(contains(Text),
            [ "IDE TOOLS",
              "pce_theme:colour(test_new, Name, Value) :-"
            ], Missing),
    include(contains(Text),
            [ "dynamic", "PROLOG CONSOLE", "ONLINE HELP",
              "console_color/3", "pldoc_style:theme/4"
            ], Present).
test(available, true) :-
    available_theme(test_new).
test(in_sandbox, true(sub_atom(File, 0, _, _, Dir))) :-
    sandbox(Dir),
    theme_file(test_new, File).
test(save_as, [ [Old,New,Edited] == [red,blue,false] ]) :-
    edit_theme(test_new),
    set_theme_setting(test_new, colour(syntax_comment, blue)),
    save_theme_as(test_new, test_new2),
    once(pce_theme:colour(test_new, syntax_comment, Old)),
    once(theme_setting(test_new2, colour(syntax_comment, New))),
    ( edited_theme(test_new, _) -> Edited = true ; Edited = false ).
test(save_as_existing, error(permission_error(create, theme, test_new))) :-
    save_theme_as(test_new2, test_new).

:- end_tests(theme_file_new).
