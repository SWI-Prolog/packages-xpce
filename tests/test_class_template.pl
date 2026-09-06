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


:- module(test_class_template, [test_class_template/0]).
:- encoding(utf8).

/** <module> A class template and the .qlf file of a class that uses one

`use_class_template/1' copies the methods of a template into the class
that uses it, at the time that class is compiled.  A .qlf file holding
such a class therefore holds a copy of the template as it was, and has to
be recompiled when the file the template comes from changes.  Nothing in
the .pl file says so, which is what `prolog:qlf_dependency/2' of
library(pce_expansion) is for.

Run with:

    swipl -g test_class_template -t halt \
          packages/xpce/tests/test_class_template.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(prolog_qlfmake), []).
:- use_module(library(filesex), [directory_file_path/3]).
:- use_module(library(lists), [memberchk/2]).

test_class_template :-
    run_tests([ class_template ]).

:- begin_tests(class_template).

test(the_file_a_template_comes_from_is_a_dependency,
     [ setup(compiled_user(Pl, Qlf, Template)),
       cleanup(remove_files([Pl, Qlf, Template]))
     ]) :-
    '$qlf_sources'(Qlf, Sources),
    memberchk(dependency(Template, Hash), Sources),
    Hash =\= 0.

test(so_a_change_to_the_template_needs_a_rebuild,
     [ setup(compiled_user(Pl, Qlf, Template)),
       cleanup(remove_files([Pl, Qlf, Template]))
     ]) :-
    \+ prolog_qlfmake:qlf_needs_rebuild(Pl),
    write_file(Template, template_source(43)),
    touch(Template, Qlf),
    prolog_qlfmake:qlf_needs_rebuild(Pl).

%       The pane template of library(pane_frame) is used by the tools of
%       the IDE, which is what this is really about.

test(the_tools_of_the_ide_depend_on_the_pane_template,
     [ setup(compiled_pane(Pl, Qlf)),
       cleanup(remove_files([Pl, Qlf]))
     ]) :-
    absolute_file_name(library(pane_frame), PaneFrame,
                       [ file_type(prolog), access(read) ]),
    '$qlf_sources'(Qlf, Sources),
    memberchk(dependency(PaneFrame, _), Sources).

%       An _aggregate_ .qlf file holds the files it loads -- the one of
%       library(emacs/emacs) holds the editor, which uses the pane
%       template -- so the file the template comes from is a dependency
%       of the file that was compiled, not of the one holding the class.

test(and_so_does_a_file_that_only_loads_one,
     [ setup(compiled_aggregate(Pl, Qlf, Part)),
       cleanup(remove_files([Pl, Qlf, Part]))
     ]) :-
    absolute_file_name(library(pane_frame), PaneFrame,
                       [ file_type(prolog), access(read) ]),
    '$qlf_sources'(Qlf, Sources),
    memberchk(dependency(PaneFrame, _), Sources).

:- end_tests(class_template).

%!  compiled_user(-PlFile, -QlfFile, -TemplateFile) is det.
%
%   A template of our own and a class that uses it, compiled to a .qlf
%   file.  Both are in the temporary directory, so that the test can
%   change the template.

compiled_user(Pl, Qlf, Template) :-
    tmp_file_name('test_template_source.pl', Template),
    tmp_file_name('test_template_user.pl', Pl),
    file_name_extension(Base, pl, Pl),
    file_name_extension(Base, qlf, Qlf),
    write_file(Template, template_source(42)),
    write_file(Pl, user_source(Template)),
    remove_files([Qlf]),
    ensure_loaded(user:Template),
    qcompile(Pl),
    unload_file(Pl).

%!  compiled_pane(-PlFile, -QlfFile) is det.
%
%   A class using the pane template of library(pane_frame), compiled to
%   a .qlf file.

compiled_pane(Pl, Qlf) :-
    tmp_file_name('test_template_pane.pl', Pl),
    file_name_extension(Base, pl, Pl),
    file_name_extension(Base, qlf, Qlf),
    write_file(Pl, pane_source(test_template_pane)),
    remove_files([Qlf]),
    qcompile(Pl),
    unload_file(Pl).

%!  compiled_aggregate(-PlFile, -QlfFile, -PartFile) is det.
%
%   A file that loads another which uses the pane template, compiled to
%   a .qlf file.  This is the shape of an aggregate .qlf file.

compiled_aggregate(Pl, Qlf, Part) :-
    tmp_file_name('test_template_part.pl', Part),
    tmp_file_name('test_template_aggregate.pl', Pl),
    file_name_extension(Base, pl, Pl),
    file_name_extension(Base, qlf, Qlf),
    write_file(Part, pane_source(test_template_part)),
    write_file(Pl, aggregate_source(Part)),
    remove_files([Qlf]),
    qcompile(Pl),
    unload_file(Pl),
    unload_file(Part).

tmp_file_name(Base, Path) :-
    current_prolog_flag(tmp_dir, Tmp),
    directory_file_path(Tmp, Base, Path).

write_file(File, Content) :-
    setup_call_cleanup(
        open(File, write, Out),
        write_content(Content, Out),
        close(Out)).

write_content(template_source(Answer), Out) :-
    format(Out, '~q.~n', [:- module(test_template_source, [])]),
    format(Out, '~q.~n', [:- use_module(library(pce))]),
    format(Out, '~q.~n',
           [:- pce_begin_class(test_template_source, template)]),
    format(Out, 'answer(_P, A:int) :<-~n    "The answer"::~n    A = ~q.~n',
           [Answer]),
    format(Out, '~q.~n', [:- pce_end_class]).
write_content(user_source(Template), Out) :-
    format(Out, '~q.~n', [:- module(test_template_user, [])]),
    format(Out, '~q.~n', [:- use_module(library(pce))]),
    format(Out, '~q.~n', [:- ensure_loaded(Template)]),
    format(Out, '~q.~n',
           [:- pce_begin_class(test_template_user, object, "Uses one")]),
    format(Out, '~q.~n', [:- use_class_template(test_template_source)]),
    format(Out, '~q.~n', [:- pce_end_class]).
write_content(pane_source(Class), Out) :-
    format(Out, '~q.~n', [:- use_module(library(pce))]),
    format(Out, '~q.~n', [:- use_module(library(pane_frame))]),
    format(Out, '~q.~n',
           [:- pce_begin_class(Class, window, "A pane")]),
    format(Out, '~q.~n', [:- use_class_template(pane)]),
    format(Out, '~q.~n', [:- pce_end_class]).
write_content(aggregate_source(Part), Out) :-
    format(Out, '~q.~n', [:- module(test_template_aggregate, [])]),
    format(Out, '~q.~n', [:- use_module(library(pce))]),
    format(Out, '~q.~n', [:- consult(Part)]).       % as emacs.pl loads its parts

%!  touch(+File, +Reference) is det.
%
%   Give File a modification time a second after that of Reference, so
%   that the test does not have to wait for the clock.

touch(File, Reference) :-
    time_file(Reference, Time),
    New is Time+1,
    set_time_file(File, _, [modified(New)]).

remove_files(Files) :-
    forall(member(File, Files),
           catch(delete_file(File), _, true)).
