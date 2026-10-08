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

:- module(test_class_doc, [test_class_doc/0]).
:- encoding(utf8).

/** <module> PlDoc comments on the members of Prolog defined classes

A structured comment whose first line is `->Sel`, `<-Sel`, `<->Var`,
`-Var` or `.Var` documents the member of the class being compiled, and
a block comment starting with `<class> Title` documents the class itself.  The comments become
PlDoc objects xpce(Class, Kind, Name), both when PlDoc collects comments
while loading and when the cross-referencer processes the file.

Run with:

    swipl -g test_class_doc -t halt packages/xpce/tests/test_class_doc.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(strings), [string/4]).          % {|string||...|}
:- use_module(library(pldoc), [doc_collect/1]).
:- use_module(library(pldoc/doc_process),
              [doc_comment/4, doc_signature/2]).
:- use_module(library(pldoc/doc_html), [doc_for_file/2]).
:- use_module(library(pce_class_doc),
              [xpce_doc_comment/5, xpce_doc_dom/3]).
:- use_module(library(prolog_xref),
              [ xref_source/1, xref_clean/1,
                xref_object_comment/4, xref_object_signature/3,
                xref_comment/3
              ]).

test_class_doc :-
    run_tests([ class_doc ]).

:- dynamic
    fixture/1,                          % File
    recording/0,
    warning/1.                          % Message

:- multifile
    user:thread_message_hook/3.

user:thread_message_hook(Message, warning, _) :-
    recording,
    assertz(warning(Message)).

:- begin_tests(class_doc, [ setup(loaded_fixture),
                            cleanup(remove_fixture)
                          ]).

test(class, [nondet, Summary == "A class to test documentation"]) :-
    doc_comment(test_doc_class:xpce(test_doc_class, class, test_doc_class), _,
                Summary, _).
test(send, [nondet, [Summary, Sig] ==
     [ "Open FileName in Mode.",
       "test_doc_class->open: file_name=file, mode=[{read,write}], column=int"
     ]]) :-
    doc_comment(test_doc_class:xpce(test_doc_class, send, open), _, Summary, _),
    doc_signature(test_doc_class:xpce(test_doc_class, send, open), Sig).
test(send_no_args, [nondet, Sig == "test_doc_class->reset"]) :-
    doc_signature(test_doc_class:xpce(test_doc_class, send, reset), Sig).
test(get, [nondet, Sig == "test_doc_class<-size: scale=int -> int"]) :-
    doc_signature(test_doc_class:xpce(test_doc_class, get, size), Sig).
test(get_no_args, [nondet, Sig == "test_doc_class<-count: -> any"]) :-
    doc_signature(test_doc_class:xpce(test_doc_class, get, count), Sig).
test(variable, [nondet, Sig == "test_doc_class<->label: name"]) :-
    doc_signature(test_doc_class:xpce(test_doc_class, both, label), Sig).
test(class_variable, [nondet, Sig == "test_doc_class.tab_width: int = 8"]) :-
    doc_signature(test_doc_class:xpce(test_doc_class, classvar, tab_width), Sig).
test(no_member_warns, Name == missing) :-
    warning(pce_doc(no_member(_, '->', Name))).
test(no_invalid_comments, Invalid == []) :-
    findall(M, (warning(M), M = pldoc(_)), Invalid).
test(no_signature_if_not_followed_by_definition, fail) :-
    doc_signature(test_doc_class:xpce(test_doc_class, send, missing), _).
test(xref, [Summary, Sig] ==
     [ "Distance between tab stops.",
       "test_doc_class.tab_width: int = 8"
     ]) :-
    fixture(File),
    setup_call_cleanup(
        xref_source(File),
        ( xref_object_comment(File, xpce(test_doc_class, classvar, tab_width),
                              Summary, _),
          xref_object_signature(File, xpce(test_doc_class, classvar, tab_width),
                                Sig)
        ),
        xref_clean(File)).
test(xref_class_is_not_module_comment, fail) :-
    fixture(File),
    setup_call_cleanup(
        xref_source(File),
        xref_comment(File, _Title, _Comment),
        xref_clean(File)).

test(markdown, Markdown == "Distance between tab stops.") :-
    xpce_doc_comment(xpce(test_doc_class, classvar, tab_width), _, _,
                     Markdown, _).
test(member_dom, Dt == ["test_doc_class<-size: scale=int -> int"]) :-
    xpce_doc_dom(xpce(test_doc_class, get, size), _,
                 [element(dt, _, Dt), element(dd, _, _)]).
test(class_dom, [Heading, Members] ==
     ["class test_doc_class: A class to test documentation", 7]) :-
    xpce_doc_dom(xpce(test_doc_class, class, test_doc_class), _,
                 [element(h2, _, H2), _P, element(dl, _, Items)]),
    dom_text(H2, Heading),
    aggregate_all(count, member(element(dt, _, _), Items), Members).

test(file_page, [Heading, Members] == [1, 7]) :-
    fixture(File),
    with_output_to(string(HTML), doc_for_file(File, [files([])])),
    aggregate_all(count, sub_string(HTML, _, _, _, "<h2 class=\"wiki\">"),
                  Heading),
    aggregate_all(count, sub_string(HTML, _, _, _, "<dt class=\"xpce-member\">"),
                  Members).

:- end_tests(class_doc).

%   The text of a DOM.  Once the class is known, the class name in the
%   heading is rendered as code.

dom_text(DOM, Text) :-
    phrase(dom_strings(DOM), Strings),
    atomic_list_concat(Strings, Atom),
    atom_string(Atom, Text).

dom_strings([]) --> [].
dom_strings([H|T]) --> dom_strings(H), dom_strings(T).
dom_strings(element(_, _, Content)) --> dom_strings(Content).
dom_strings(String) --> { string(String) }, [String].

%!  loaded_fixture is det.
%
%   Write the fixture class to a temporary file and load it while PlDoc
%   collects comments.  Warnings are recorded in warning/1.

loaded_fixture :-
    tmp_file_name('test_doc_class.pl', File),
    fixture_source(Source),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        write(Out, Source),
        close(Out)),
    asserta(fixture(File)),
    setup_call_cleanup(
        ( asserta(recording),
          doc_collect(true)
        ),
        load_files(File, [silent(true)]),
        ( doc_collect(false),
          retractall(recording)
        )).

remove_fixture :-
    forall(retract(fixture(File)),
           delete_file(File)),
    retractall(warning(_)).

tmp_file_name(Base, File) :-
    tmp_file(doc, Tmp),
    atomic_list_concat([Tmp, '_', Base], File).

fixture_source({|string||
:- module(test_doc_class, []).
:- use_module(library(pce)).

:- pce_begin_class(test_doc_class, object, "Test class").

/** <class> A class to test documentation

This class tests PlDoc comments on xpce classes.
*/

%!  <->label
%
%   The label.

variable(label, name, both, "The label").

%!  .tab_width
%
%   Distance between tab stops.

class_variable(tab_width, int, 8, "Tab distance").

%!  ->open
%
%   Open FileName in Mode.

open(_D, FileName:file, Mode:[{read,write}], _:column=int) :->
    format("~w ~w~n", [FileName, Mode]).

%!  ->reset
%
%   Reset.

reset(_D) :->
    true.

%!  <-size
%
%   Size of the thing.

size(_D, Scale:int, Size:int) :<-
    Size is 10*Scale.

%!  <-count
%
%   Count.

count(_D, Count) :<-
    Count = 42.

%!  ->missing
%
%   Not followed by its definition.

foo(_) :->
    true.

:- pce_end_class(test_doc_class).
|}).
