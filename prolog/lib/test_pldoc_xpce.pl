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

:- module(test_pldoc_xpce,
          [ test_pldoc_xpce/0
          ]).

:- use_module(library(plunit)).
:- use_module(library(pldoc/doc_wiki), [wiki_codes_to_dom/3]).
:- use_module(library(doc_latex), []).
:- use_module(library(pce)).
:- use_module(pldoc_xpce).

%   Per the xpce test convention (see CLAUDE.md): module name = file
%   name, exports a single predicate that succeeds silently on pass.

test_pldoc_xpce :-
    run_tests(pldoc_xpce).

%   Test inputs are deliberately short -- we assert against the DOM
%   shape produced by wiki_codes_to_dom/3, so each test pins down one
%   pattern at a time. The `[w(Foo), ...]` boilerplate around the
%   single inline term is the standard PlDoc wrapping of a paragraph.

:- begin_tests(pldoc_xpce).

face(In, Face) :-
    string_codes(In, Codes),
    wiki_codes_to_dom(Codes, [], [p(Tokens)]),
    member(Face, Tokens),
    Face = \(_:_),
    !.

test(method_send) :-
    face("button->event",
         \(pldoc_xpce:xpce(method, [button, ->, event]))).

test(method_get) :-
    face("button<-label",
         \(pldoc_xpce:xpce(method, [button, <-, label]))).

test(method_both) :-
    face("button<->status",
         \(pldoc_xpce:xpce(method, [button, <->, status]))).

test(local_send) :-
    face("Then ->run",
         \(pldoc_xpce:xpce(method, ['', ->, run]))).

test(local_get) :-
    face("Then <-name",
         \(pldoc_xpce:xpce(method, ['', <-, name]))).

test(classvar) :-
    face("dialog.label_suffix",
         \(pldoc_xpce:xpce(classvar, [dialog, label_suffix]))).

test(objref) :-
    face("see @default",
         \(pldoc_xpce:xpce(objref, [default]))).

test(errref) :-
    face("see !no_behaviour",
         \(pldoc_xpce:xpce(errref, [no_behaviour]))).

test(instvar_known_class) :-
    face("see button-label",
         \(pldoc_xpce:xpce(instvar, [button, label]))).

test(instvar_unknown_class_skipped) :-
    string_codes("high-level concept", Codes),
    wiki_codes_to_dom(Codes, [], [p(Tokens)]),
    \+ ( member(\(pldoc_xpce:xpce(instvar, _)), Tokens) ).

test(classref_known) :-
    face("see class button",
         \(pldoc_xpce:xpce(classref, [button]))).

test(classref_unknown_skipped) :-
    %  "thing" is not an xpce class -- must NOT be matched as a
    %  classref so we don't mangle "class is", "class of", etc.
    string_codes("the class thing matters", Codes),
    wiki_codes_to_dom(Codes, [], [p(Tokens)]),
    \+ ( member(\(pldoc_xpce:xpce(classref, _)), Tokens) ).

%   Regression: a @see block must terminate at the next section
%   header rather than swallowing it into the last tag's content.
%   Tests the pldoc_wiki:tags/3 fix.

test(tag_block_closes_at_heading) :-
    Text = "\n@see foo\n@see bar\n\n## Next\n\nMore.\n",
    string_codes(Text, Codes),
    wiki_codes_to_dom(Codes, [], DOM),
    memberchk(\tags(_), DOM),
    memberchk(h2(_, _), DOM).

%   LaTeX backend: each face produces the matching xpce.sty macro.

latex_render(In, Substring) :-
    string_codes(In, Codes),
    with_xpce_backend(latex,
        ( wiki_codes_to_dom(Codes, [], DOM),
          phrase(pldoc_latex:latex(DOM), Tokens, [])
        )),
    with_output_to(string(Out),
                   pldoc_latex:print_latex(current_output, Tokens, [])),
    once(sub_string(Out, _, _, _, Substring)).

test(latex_method_send) :-
    latex_render("see button->event", "\\classsend{button}{event}").
test(latex_method_get) :-
    latex_render("see button<-label", "\\classget{button}{label}").
test(latex_method_both) :-
    latex_render("see button<->status", "\\classboth{button}{status}").
test(latex_local_send) :-
    latex_render("Then ->run", "\\send{run}").
test(latex_classvar) :-
    latex_render("dialog.size",  "\\classvar{dialog}{size}").
test(latex_instvar) :-
    latex_render("button-label", "\\classinstvar{button}{label}").
test(latex_objref) :-
    latex_render("see @default", "\\objectname{default}").
test(latex_errref) :-
    latex_render("see !no_behaviour", "\\errid{no_behaviour}").
test(latex_classref) :-
    latex_render("see class button", "class \\class{button}").

:- end_tests(pldoc_xpce).
