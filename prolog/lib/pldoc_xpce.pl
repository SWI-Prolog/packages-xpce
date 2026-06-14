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

:- module(pldoc_xpce,
          [ with_xpce_backend/2,        % +Backend, :Goal
            with_xpce_chapter_class/2,  % +Class, :Goal
            xpce_dom_transform/2,       % +DOM0, -DOM
            xpce_dom_extract_index/3    % +Class, +DOM, -Entries
          ]).

:- use_module(library(pldoc/doc_wiki), []).
:- use_module(library(doc_latex), []).
:- use_module(library(pce),
              [ op(_, _, _),
                object/1,
                get/3, get/4, get/5
              ]).
:- use_module(library(lists), [append/3, last/2, member/2, reverse/2]).
:- use_module(library(apply), [maplist/3]).

%   Suppress =|PlDoc: unknown tag @<refname>|= warnings during the
%   .md -> .tex pass when the tag names a live xpce global object
%   (=|@arg1|=, =|@receiver|=, =|@event_tree|=, =|@nil|=, ...). Such
%   refs trip PlDoc's tag-name check at the head of a paragraph; the
%   parser falls back and the line renders as plain text either way,
%   so the warning is pure noise. Genuinely unknown tags still get
%   reported.

:- multifile user:message_hook/3.
user:message_hook(pldoc(unknown_tag(Name)), warning, _) :-
    xpce_global_ref(Name),
    !.

xpce_global_ref(Name) :-
    well_known_global(Name),
    !.
xpce_global_ref(Name) :-
    catch(object(@Name), _, fail).

%   The xpce engine isn't always fully booted when the manual renders
%   (=|gen_refman.pl|= runs swipl with a freshly-built tree where the
%   pl2xpce foreign plugin may not yet be loadable, and the lazy
%   C-side singletons like @event_tree only spring into existence on
%   first send). In either case =|object/1|= reports the ref as
%   absent, so list the known built-in singletons explicitly.

well_known_global(arg1).        well_known_global(arg2).
well_known_global(arg3).        well_known_global(arg4).
well_known_global(arg5).        well_known_global(arg6).
well_known_global(arg7).        well_known_global(arg8).
well_known_global(arg9).        well_known_global(arg10).
well_known_global(classes).
well_known_global(default).     well_known_global(display).
well_known_global(display_manager).
well_known_global(error_database).
well_known_global(event).       well_known_global(event_tree).
well_known_global(grabbed_windows).
well_known_global(nil).
well_known_global(off).         well_known_global(on).
well_known_global(pce).         well_known_global(prolog).
well_known_global(receiver).    well_known_global(receiver_class).
well_known_global(reporter).    well_known_global(reportee).
well_known_global(selector).
well_known_global(types).

:- meta_predicate
    with_xpce_backend(+, 0).

/** <module> PlDoc Markdown extensions for xpce documentation

Loading this module installs clauses on the multifile DCG hook
=|prolog:doc_wiki_face//2|= so the PlDoc wiki/markdown parser
recognises the xpce surface idiom in bare prose (no surrounding
backticks needed):

  * =|Class->name|=    -- send-method reference
  * =|Class<-name|=    -- get-method reference
  * =|Class<->name|=   -- variable, both access
  * =|->name|= / =|<-name|= / =|<->name|= -- method reference without
    a class prefix (resolved against the surrounding card's context
    by the renderer)
  * =|Class.classvar|= -- class-variable reference
  * =|Class-name|=     -- instance-variable reference (only when
    =Class= is a known xpce class)
  * =|@reference|=     -- xpce global object reference
  * =|!errid|=         -- xpce error reference
  * =|class <name>|=   -- class reference (only when =name= is a
    known xpce class -- avoids matching common English like
    "class is", "class of")

Each match is replaced by =|\xpce(Kind, Args)|= in the DOM, where
=Kind= is one of =method=, =classvar=, =instvar=, =objref=, =errref=,
=classref=. The default rendering is plain inline code; the multifile
hook =|pldoc_xpce:xpce_link(+Kind, +Args, -URL)|= can be defined to
turn matches into hyperlinks at HTML-render time. The "known xpce
class" check is also a multifile hook -- =|pldoc_xpce:xpce_known_class/1|=
-- with a default clause that consults the live =|@classes|= chain.

Loading is global: once the module is loaded the patterns are
recognised in *all* PlDoc text in the running image. The
=|Class.classvar|= pattern in particular will match unrelated dotted
constructs (filenames, version numbers); load only where xpce
documentation is being processed.

@see library(pldoc/doc_wiki) -- the parser hooked here.
*/

:- multifile
    prolog:doc_wiki_face//2,
    pldoc_xpce:xpce_link/3,         % +Kind, +Args, -URL
    pldoc_xpce:xpce_known_class/1.  % +ClassName

%   Default class-existence check: consult the live xpce class system.
%   Falls through quietly if xpce isn't loaded or @classes isn't an
%   object, so this module is safe to load standalone.

pldoc_xpce:xpce_known_class(Name) :-
    catch(get(@classes, member, Name, _), _, fail).


                 /*******************************
                 *           PATTERNS           *
                 *******************************/

% Tokens from doc_wiki: w(W) for words, single-char atoms for
% punctuation, ' ' for whitespace runs. Order matters here -- longest
% method-arrow form first so Class<->name does not get consumed as
% Class<-name followed by junk.

%   Backtick-wrapped xpce references. PlDoc's built-in =|`...`|=
%   parser only handles single-token Prolog terms, so the common
%   authoring form =|`error <-id`|= (space inside) falls through as
%   literal backticks. Try matching the content as an xpce reference
%   first; if no xpce shape applies, fail and let PlDoc's own
%   backtick rules (predref / code-term) take over.
prolog:doc_wiki_face(Face, _) -->
    ['`'], xpce_backtick_inside(Face), ['`'], !.

prolog:doc_wiki_face(Face, _) -->
    [w(Class), <, -, >], underscored_word(Sel), !,
    { method_face(Class, '<->', Sel, Face) }.
prolog:doc_wiki_face(Face, _) -->
    [w(Class), -, >], underscored_word(Sel), !,
    { method_face(Class, '->', Sel, Face) }.
prolog:doc_wiki_face(Face, _) -->
    [w(Class), <, -], underscored_word(Sel), !,
    { method_face(Class, '<-', Sel, Face) }.

% Local-method forms without a class prefix. The renderer needs to
% know the enclosing class to resolve these; emit the term and let
% the backend handle it.
prolog:doc_wiki_face(Face, _) -->
    [<, -, >], underscored_word(Sel), !,
    { method_face('', '<->', Sel, Face) }.
prolog:doc_wiki_face(Face, _) -->
    [-, >], underscored_word(Sel), !,
    { method_face('', '->', Sel, Face) }.
prolog:doc_wiki_face(Face, _) -->
    [<, -], underscored_word(Sel), !,
    { method_face('', '<-', Sel, Face) }.

%   Selectors that start with `_` (private / internal methods like
%   `editor->_dabbrev_expand`) come through the wiki tokenizer as
%   =|_, w(Word)|= because `_` ranks as punctuation rather than a
%   word character. Stitch them back together so the face hook sees
%   the selector as a single atom.

underscored_word(Sel) --> ['_', w(Word)], !,
    { atom_concat('_', Word, Sel) }.
underscored_word(Sel) --> [w(Sel)].

%   xpce reference shapes recognised inside backtick code spans.
%   Allow optional whitespace around the operator -- the common
%   authoring form puts a space between the class name and the
%   arrow (=|`Class <-method`|=).

xpce_backtick_inside(Face) -->
    optional_space, [w(Class)], optional_space,
    [<, -, >],
    optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { method_face(Class, '<->', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [w(Class)], optional_space,
    [-, >],
    optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { method_face(Class, '->', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [w(Class)], optional_space,
    [<, -],
    optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { method_face(Class, '<-', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [<, -, >],
    optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { method_face('', '<->', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [-, >],
    optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { method_face('', '->', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [<, -],
    optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { method_face('', '<-', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [w(ClassIn)], optional_space, ['.'],
    optional_space, underscored_word(Var), optional_space,
    { normalize_class(ClassIn, Class),
      Face = \(pldoc_xpce:xpce(classvar, [Class, Var])) }.
xpce_backtick_inside(Face) -->
    optional_space, [w(ClassIn)], optional_space, [-],
    optional_space, underscored_word(Var), optional_space,
    { normalize_class(ClassIn, Class),
      live_has_instance_variable(Class, Var) },
    { Face = \(pldoc_xpce:xpce(instvar, [Class, Var])) }.
%   =|`@receiver <-method`|= and =|`@receiver Class<-method`|= --
%   compound references that call a method on a named global object.
%   When the receiver's class can be resolved (live xpce lookup), the
%   method link targets that class. Otherwise the receiver's chapter
%   class is used as a fallback.

xpce_backtick_inside(Face) -->
    optional_space, [@, w(Ref)], optional_space,
    [<, -, >], optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { compound_method_face(Ref, '<->', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [@, w(Ref)], optional_space,
    [-, >], optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { compound_method_face(Ref, '->', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [@, w(Ref)], optional_space,
    [<, -], optional_space, underscored_word(Sel),
    optional_space, optional_args(_),
    { compound_method_face(Ref, '<-', Sel, Face) }.
xpce_backtick_inside(Face) -->
    optional_space, [@, w(Ref)], optional_space,
    { Face = \(pldoc_xpce:xpce(objref, [Ref])) }.
xpce_backtick_inside(Face) -->
    optional_space, [!, w(Id)], optional_space,
    { Face = \(pldoc_xpce:xpce(errref, [Id])) }.

%   Render =|@Ref <op>Sel|= as the receiver's class member, falling
%   back to the chapter class when the receiver class can't be
%   resolved live.

compound_method_face(Ref, Arrow, Sel, Face) :-
    (   ref_class_name(Ref, Class)
    ->  true
    ;   Class = ''
    ),
    method_face(Class, Arrow, Sel, Face).

ref_class_name(Ref, Class) :-
    catch(get(@Ref, class_name, Class), _, fail).

optional_space --> [' '], !.
optional_space --> [].

%   The selector inside a backtick reference can carry the call args
%   (=|`Class <-method: arg1, arg2`|=). Swallow them so the closing
%   backtick still matches -- we drop the arg text since the manual
%   already prints it next to each method definition.

optional_args([]) --> [].
optional_args([T|Ts]) --> [T], { T \== '`' }, !, optional_args(Ts).

%   Last-resort backtick handler. Runs after all xpce-shape rules
%   above. If the content does not parse as a Prolog term (the
%   condition under which PlDoc would have produced a code face
%   itself) emit a plain =|code(Text)|= so the rendered output
%   shows the content verbatim instead of leaving the literal
%   backticks behind as left-single-quotes.

prolog:doc_wiki_face(code(Text), _) -->
    ['`'], backtick_words(Words), ['`'],
    { \+ parsable_as_term(Words),
      atomic_list_concat(Words, Text),
      Text \== ''
    }.

parsable_as_term(Words) :-
    atomic_list_concat(Words, Text),
    E = error(_, _),
    catch(atom_to_term(Text, _, _), E, fail).

backtick_words([]) --> [].
backtick_words([Word|T]) --> [w(Word)], !, backtick_words(T).
backtick_words([Punct|T]) --> [Punct], { atomic(Punct), Punct \== '`' }, !,
    backtick_words(T).

prolog:doc_wiki_face(Face, _) -->
    [w(ClassIn), '.'], underscored_word(Var), !,
    { normalize_class(ClassIn, Class),
      Face = \(pldoc_xpce:xpce(classvar, [Class, Var])) }.

% Instance variable: gated on "Class actually has the instance_variable"
% so we don't mangle English compounds like "Object-level" or
% "object-reference" where `object' is a known class but `level' /
% `reference' aren't ivars on it.
prolog:doc_wiki_face(Face, _) -->
    [w(ClassIn), -], underscored_word(Var),
    { normalize_class(ClassIn, Class),
      live_has_instance_variable(Class, Var) },
    !,
    { Face = \(pldoc_xpce:xpce(instvar, [Class, Var])) }.

prolog:doc_wiki_face(Face, _) -->
    [@, w(Ref)],
    { \+ pldoc_tag_name(Ref) },
    !,
    { Face = \(pldoc_xpce:xpce(objref, [Ref])) }.

prolog:doc_wiki_face(Face, _) -->
    [!, w(Id)], !,
    { Face = \(pldoc_xpce:xpce(errref, [Id])) }.

%   PlDoc tag names that look like xpce object refs (=|@see|=,
%   =|@param|=, ...). When they appear inside a bullet body the
%   wiki parser doesn't lift them out into the surrounding tags
%   block (that's done only at paragraph level), so they reach the
%   face hooks. Refuse to absorb them as objrefs; let them through
%   as plain text so the embedded class reference still renders.

pldoc_tag_name(see).
pldoc_tag_name(param).
pldoc_tag_name(arg).
pldoc_tag_name(return).
pldoc_tag_name(author).
pldoc_tag_name(version).
pldoc_tag_name(deprecated).
pldoc_tag_name(throws).
pldoc_tag_name(compat).
pldoc_tag_name(tbd).
pldoc_tag_name(bug).

% Bare class reference: gated on a known xpce class so we don't match
% "class is", "class of", "class definitions", etc. Whitespace between
% the =|class|= word and the class name may be a literal space or the
% newline the wiki tokenizer emits when the phrase straddles a wrapped
% source line. Authors also capitalise the lead word at the start of a
% sentence ("Class picture is the ...") -- accept that form too.
prolog:doc_wiki_face(Face, _) -->
    [w(Lead)], { class_lead(Lead) },
    wiki_ws, [w(ClassIn)],
    { normalize_class(ClassIn, ClassName),
      pldoc_xpce:xpce_known_class(ClassName) },
    !,
    { Face = \(pldoc_xpce:xpce(classref, [Lead, ClassName])) }.

% The same phrase with the class word *before* "object" / "objects"
% ("Graphical objects inform their device ..."). Class may be
% capitalised at the start of a sentence. The trailer word stays
% lowercase by convention; only the leading class word is folded.
prolog:doc_wiki_face(Face, _) -->
    [w(ClassIn)], wiki_ws, [w(Trailer)],
    { class_trailer(Trailer),
      normalize_class(ClassIn, ClassName),
      pldoc_xpce:xpce_known_class(ClassName) },
    !,
    { Face = \(pldoc_xpce:xpce(classref_suffix, [ClassIn, ClassName, Trailer])) }.

class_lead(class).
class_lead('Class').

class_trailer(object).
class_trailer(objects).

wiki_ws --> [' '], !.
wiki_ws --> ['\n'].

method_face(ClassIn, Arrow, Sel,
            \(pldoc_xpce:xpce(method, [Class, Arrow, Sel]))) :-
    normalize_class(ClassIn, ClassNorm),
    defining_class(ClassNorm, Arrow, Sel, Class).

%   Methods inherited from a super class are documented there, not on
%   the receiving class. Ask xpce live where the named member is
%   actually defined and use that class as the link's target so a
%   prose mention of =|frame->free|= lands on the documented
%   =|object->free|= chunk rather than producing a dead link to a
%   =|class-frame-send-free|= anchor that does not exist.

defining_class('', _, _, '') :- !.
defining_class(ClassIn, Arrow, Sel, DefClass) :-
    catch(get(@pce, convert, ClassIn, class, CObj), _, fail),
    behaviour_lookup(Arrow, CObj, Sel, Member),
    catch(get(Member, context, DefCObj), _, fail),
    catch(get(DefCObj, name, DefName), _, fail),
    !,
    DefClass = DefName.
defining_class(C, _, _, C).

behaviour_lookup('->',  CObj, Sel, M) :-
    catch(get(CObj, send_method, Sel, M), _, fail).
behaviour_lookup('<-',  CObj, Sel, M) :-
    catch(get(CObj, get_method, Sel, M), _, fail).
behaviour_lookup('<->', CObj, Sel, M) :-
    catch(get(CObj, instance_variable, Sel, M), _, fail).

live_has_instance_variable(Class, Var) :-
    catch(get(@pce, convert, Class, class, CObj), _, fail),
    catch(get(CObj, instance_variable, Var, _), _, fail).

%   Class identifiers in xpce are all lowercase, but authors capitalise
%   them at the start of a sentence ("=|`Object ->unlink`|= is often
%   defined..."). If the lowercase version names a known xpce class,
%   use it so the resulting cross-reference resolves to the real class
%   chapter; otherwise keep the original (the author may be referring
%   to something else).

normalize_class('', '') :- !.
normalize_class(In, Out) :-
    atom(In),
    downcase_atom(In, Down),
    (   In == Down
    ->  Out = In
    ;   pldoc_xpce:xpce_known_class(Down)
    ->  Out = Down
    ;   Out = In
    ).
normalize_class(X, X).


                 /*******************************
                 *         BACKEND DISPATCH     *
                 *******************************/

%   The =xpce/2= face DCG is called by both the html//1 backend
%   (=html_write:do_expand/4= for the =|\(Module:Goal)|= form) and the
%   doc_latex.pl backend (=latex(\Cmd, ...)= clause), but each expects
%   tokens of its own shape. We dispatch on a thread-local flag set by
%   =with_xpce_backend/2=; the default is HTML so direct =|html//1|=
%   use needs no setup.

:- thread_local
    pldoc_xpce_backend/1,
    current_xpce_chapter_class/1,
    current_member_section/1.        % ivars | sends | gets | classvars | none

%!  with_xpce_chapter_class(+Class, :Goal) is nondet.
%
%   Tell the converter the enclosing class while Goal runs. Used by
%   gen_refman.pl when rendering each per-class .md so that
%   local-form references (=|->fill|=, =|<-area|=, =|<->name|=)
%   resolve to =|\classsend{Class}{...}|= (with a real anchor) rather
%   than =|\send{...}|= (plain bold-code without a link).

with_xpce_chapter_class(Class, Goal) :-
    setup_call_cleanup(
        asserta(current_xpce_chapter_class(Class), Ref),
        Goal,
        erase(Ref)).

%!  with_xpce_backend(+Backend, :Goal) is nondet.
%
%   Set the backend selector to =Backend= (=html= or =latex=) for the
%   duration of Goal. Goal would typically wrap a call to
%   =|phrase(latex(DOM), Tokens)|= or =|phrase(html(DOM), Tokens)|=
%   so that the =\xpce(...)= DOM terms render into the matching
%   token form.

with_xpce_backend(Backend, Goal) :-
    setup_call_cleanup(
        asserta(pldoc_xpce_backend(Backend), Ref),
        Goal,
        erase(Ref)).

current_xpce_backend(Backend) :-
    (   pldoc_xpce_backend(B) -> Backend = B ; Backend = html ).


                 /*******************************
                 *      HTML RENDERING          *
                 *******************************/

%!  xpce(+Kind, +Args)// is det.
%
%   Backend dispatcher invoked from a =|\(pldoc_xpce:xpce(...))|=
%   term in the DOM. The HTML branch emits a hyperlink when the
%   multifile hook =|pldoc_xpce:xpce_link/3|= yields a URL,
%   otherwise inline code. The LaTeX branch emits the matching
%   xpce.sty macros via =|pldoc_latex:latex/3|=.

:- use_module(library(http/html_write), [html/3]).

xpce(Kind, Args) -->
    { current_xpce_backend(Backend) },
    xpce_in(Backend, Kind, Args).

xpce_in(html, Kind, Args) -->
    { face_text(Kind, Args, Text),
      (   catch(pldoc_xpce:xpce_link(Kind, Args, URL), _, fail)
      ->  Mode = link(URL, Text)
      ;   Mode = code(Text)
      )
    },
    html_render(Kind, Mode).
xpce_in(latex, Kind, Args) -->
    { latex_terms(Kind, Args, Terms) },
    pldoc_latex:latex(Terms).

html_render(classref, link(URL, ClassName)) -->
    !,
    html(['class ', a(href=URL, code(ClassName))]).
html_render(classref, code(ClassName)) -->
    !,
    html(['class ', code(ClassName)]).
html_render(classref_suffix, link(URL, Text)) -->
    !,
    html([a(href=URL, code(Text))]).
html_render(classref_suffix, code(Text)) -->
    !,
    html([code(Text)]).
html_render(_, link(URL, Text)) -->
    html(a(href=URL, code(Text))).
html_render(_, code(Text)) -->
    html(code(Text)).

face_text(method, [Class, Arrow, Sel], Text) :-
    format(atom(Text), '~w~w~w', [Class, Arrow, Sel]).
face_text(classvar, [Class, Var], Text) :-
    format(atom(Text), '~w.~w', [Class, Var]).
face_text(instvar, [Class, Var], Text) :-
    format(atom(Text), '~w-~w', [Class, Var]).
face_text(objref, [Ref], Text) :-
    atom_concat('@', Ref, Text).
face_text(errref, [Id], Text) :-
    atom_concat('!', Id, Text).
face_text(classref, [Class], Class).
face_text(classref, [_Lead, Class], Class).
face_text(classref_suffix, [ClassIn, _Class, Trailer], Text) :-
    format(atom(Text), '~w ~w', [ClassIn, Trailer]).


                 /*******************************
                 *      LATEX RENDERING         *
                 *******************************/

%!  latex_terms(+Kind, +Args, -Terms) is det.
%
%   Map an xpce face term to a list of doc_latex DOM elements (atoms
%   for literal text, =cmd(Cmd(Args...))= for macros). The macros
%   targeted are the ones in =|packages/xpce/TeX/xpce.sty|= that the
%   hand-written UserGuide already uses; =classinstvar/2= and
%   =errid/1= are introduced in Phase 2b.

latex_terms(method, ['',  '->',  S], [cmd(classsend(DefC, S))]) :-
    current_xpce_chapter_class(C),
    defining_class(C, '->', S, DefC), !.
latex_terms(method, ['',  '<-',  S], [cmd(classget(DefC, S))]) :-
    current_xpce_chapter_class(C),
    defining_class(C, '<-', S, DefC), !.
latex_terms(method, ['',  '<->', S], [cmd(classboth(DefC, S))]) :-
    current_xpce_chapter_class(C),
    defining_class(C, '<->', S, DefC), !.
latex_terms(method, ['',  '->',  S], [cmd(send(S))]).
latex_terms(method, ['',  '<-',  S], [cmd(get(S))]).
latex_terms(method, ['',  '<->', S], [cmd(both(S))]).
latex_terms(method, [C,   '->',  S], [cmd(classsend(C, S))]).
latex_terms(method, [C,   '<-',  S], [cmd(classget(C, S))]).
latex_terms(method, [C,   '<->', S], [cmd(classboth(C, S))]).
latex_terms(classvar, [C, V],        [cmd(classvar(C, V))]).
latex_terms(instvar,  [C, V],        [cmd(classinstvar(C, V))]).
latex_terms(objref,   [R],           [cmd(objectname(R))]).
latex_terms(errref,   [I],           [cmd(errid(I))]).
latex_terms(classref, [C],           ['class ', cmd(class(C))]).
latex_terms(classref, [Lead, C],     [LeadStr, cmd(class(C))]) :-
    atom_concat(Lead, ' ', LeadStr).
latex_terms(classref_suffix, [ClassIn, C, Trailer],
            [cmd(classsuffix(C, ClassIn)), TrailerStr]) :-
    atom_concat(' ', Trailer, TrailerStr).


                 /*******************************
                 *     DOM TRANSFORMATION       *
                 *******************************/

%!  xpce_dom_transform(+DOM0, -DOM) is det.
%
%   Walk the parsed PlDoc DOM looking for =|ul([li(Tokens), ...])|=
%   lists whose every item starts with an xpce class-member shape:
%
%     - =|<Class>-><sel>|= (send method)
%     - =|<Class><-<sel>|= (get method)
%     - =|<Class><-><sel>|= (instance variable, "both" access)
%     - =|<Class>-<name>|=  (instance variable, plain dash form)
%     - =|<Class>.<name>|=  (class variable)
%
%   Such lists are rewritten into
%   =|\(pldoc_xpce:xpce_member_list(Members))|= so the LaTeX/HTML
%   backends can drop each member into a =description= environment
%   via the matching =|\sendmethod|=/=|\getmethod|=/=|\bothmethod|=/
%   etc. macros from xpce.sty -- giving the dense definition-list
%   layout the hand-written UserGuide already uses, instead of one
%   subsubsection per member. Non-member uls and other DOM shapes
%   pass through unchanged.

xpce_dom_transform(DOM0, DOM) :-
    is_list(DOM0),
    !,
    collapse_see_paragraphs(DOM0, DOM1),
    walk_with_section(DOM1, DOM).
xpce_dom_transform(ul(LIs), \(pldoc_xpce:xpce_member_list(Members))) :-
    is_list(LIs),
    maplist(xpce_li_to_member, LIs, Members),
    !.
xpce_dom_transform(dl(class(termlist), Items),
                   \(pldoc_xpce:xpce_member_list(Members))) :-
    is_list(Items),
    dt_dd_pairs(Items, Pairs),
    maplist(termlist_pair_to_member, Pairs, Members),
    !.
xpce_dom_transform(Term, Out) :-
    compound(Term),
    \+ skip_compound(Term),
    !,
    Term =.. [Functor|Args],
    maplist(xpce_dom_transform, Args, NewArgs),
    Out =.. [Functor|NewArgs].
xpce_dom_transform(X, X).

%!  collapse_see_paragraphs(+DOM, -DOM2) is det.
%
%   Walk a DOM list and fold runs of =|p([\xpce(objref, see), ...])|=
%   paragraphs into a single =|\tags([\tag(see, [Ref1, Ref2, ...])])|=
%   term so PlDoc's emitter renders them as a single "See also:"
%   section. The wiki parser would normally collect =|@see|= lines at
%   tag-parse time, but inside a description-list item's body it
%   leaves them as ordinary paragraphs.

%   Replace consecutive =|p|= paragraphs whose body starts with
%   =|@see ...|= with a single =|\tags([\tag(see, [Ref1, Ref2, ...])])|=
%   block, and also crack open a single =|p|= that contains a
%   newline-separated run of =|@see <ref>|= lines (the common shape
%   when the .doc body emitted multiple consecutive =|@see|= tags --
%   PlDoc parses them all into one paragraph because the wiki tag
%   parser doesn't recurse into description-list bodies).

collapse_see_paragraphs([], []).
collapse_see_paragraphs(In, [SeeBlock|RestOut]) :-
    see_run(In, Refs, Rest),
    Refs \== [],
    !,
    SeeBlock = \tags([\tag(see, Refs)]),
    collapse_see_paragraphs(Rest, RestOut).
collapse_see_paragraphs([p(Tokens)|T], [p(Prose), SeeBlock|RestOut]) :-
    split_trailing_see(Tokens, Prose, Refs),
    Refs \== [],
    !,
    SeeBlock = \tags([\tag(see, Refs)]),
    collapse_see_paragraphs(T, RestOut).
collapse_see_paragraphs([H|T], [H|RestOut]) :-
    collapse_see_paragraphs(T, RestOut).

%   Find a run of =|@see <ref>|= lines at the tail of a paragraph and
%   peel them off. Returns the prose-only Prefix and the list of
%   refs the run carried, so the renderer can emit the prose as a
%   plain paragraph and the refs as a "See also" tag block.

split_trailing_see(Tokens, Prose, Refs) :-
    append(Prose0, ['\n'|Tail], Tokens),
    take_at_see(Tail, AfterSee),
    extract_see_refs([@, see|AfterSee], Refs),
    Refs \== [],
    %   Drop trailing whitespace at the end of the prose so the
    %   rendered paragraph doesn't leave a dangling space.
    trim_trailing_ws(Prose0, Prose).

trim_trailing_ws(Ts0, Ts) :-
    reverse(Ts0, R0),
    skip_leading_ws(R0, R1),
    reverse(R1, Ts).

see_run([P|Rest], Refs, Tail) :-
    see_paragraph_refs(P, Refs0),
    Refs0 \== [],
    !,
    see_run(Rest, MoreRefs, Tail),
    append(Refs0, MoreRefs, Refs).
see_run(Tail, [], Tail).

%   Pull every @see reference out of a paragraph. Returns the list of
%   references the @see directives carried, where each reference is
%   the token list between this @see and the next newline (or end).

see_paragraph_refs(p(Tokens), Refs) :-
    is_list(Tokens),
    starts_with_at_see(Tokens),
    !,
    extract_see_refs(Tokens, Refs).
see_paragraph_refs(_, []).

extract_see_refs(Tokens, [Ref|More]) :-
    take_at_see(Tokens, AfterSee),
    !,
    take_until_newline(AfterSee, Ref0, Rest),
    trim_ws(Ref0, Ref),
    extract_see_refs(Rest, More).
extract_see_refs(_, []).

%   Recognise both the old =|\xpce(objref, [see])|= face-term (kept
%   for backwards compatibility) and the plain =|[@, see, ' ']|=
%   token run that the parser now leaves in place after the
%   =|pldoc_tag_name/1|= exclusion in =|prolog:doc_wiki_face|=.

starts_with_at_see(Tokens) :-
    skip_leading_ws(Tokens, Rest),
    take_at_see(Rest, _).

take_at_see(Tokens, AfterSee) :-
    skip_leading_ws(Tokens, [\(_:xpce(objref, [see]))|AfterSee0]),
    !,
    skip_leading_ws(AfterSee0, AfterSee).
take_at_see(Tokens, AfterSee) :-
    skip_leading_ws(Tokens, [@, see|AfterSee0]),
    skip_leading_ws(AfterSee0, AfterSee).

take_until_newline([], [], []).
take_until_newline(['\n'|T], [], T) :- !.
take_until_newline([H|T], [H|TakenT], Rest) :-
    take_until_newline(T, TakenT, Rest).

skip_leading_ws([H|T], Rest) :- ws_token(H), !, skip_leading_ws(T, Rest).
skip_leading_ws(L, L).

trim_ws(Ts0, Ts) :-
    skip_leading_ws(Ts0, Ts1),
    reverse(Ts1, R0),
    skip_leading_ws(R0, R1),
    reverse(R1, Ts).

ws_token(' ').
ws_token('\n').

%   PlDoc's wiki parser turns =|- Term\n    body|= bullet items with
%   indented bodies into =|dl(class(termlist), [dt, dd, ...])|=, not
%   into a =|ul|=. Walk the dt/dd alternation and reshape each pair
%   into the same =|member(Kind, Class, Name, Args, Descr)|= records
%   the ul branch produces, so the downstream emitters see one
%   uniform shape regardless of which branch the parser picked.

dt_dd_pairs([], []).
dt_dd_pairs([dt(_, \term(Src, _, _)), dd(Descr)|T], [Src-Descr|Rest]) :-
    dt_dd_pairs(T, Rest).

termlist_pair_to_member(SrcAtom-Descr0,
                        member(Kind, Class, Name, Args, Descr)) :-
    split_dt_src(SrcAtom, HeadCodes, Args),
    head_codes_to_member(HeadCodes, Kind, Class, Name),
    %   xpce_dom_transform also collapses @see paragraphs (via
    %   collapse_see_paragraphs on every list it walks) and
    %   recurses into nested ul/li bodies so @see runs inside
    %   nested bullet lists get folded into "See also" tag blocks.
    xpce_dom_transform(Descr0, Descr).

%   Split the dt source text =|'Class<op>Name [: Args] .\n'|= into the
%   head bytes and the args atom. PlDoc appends =|' .\n'|= to make the
%   source readable as a Prolog term; strip that, then take the first
%   =|: |= as the args separator.

split_dt_src(SrcAtom, HeadCodes, ArgsAtom) :-
    atom_codes(SrcAtom, Codes0),
    strip_trailing_period(Codes0, Codes),
    (   append(Head, [0':, 0' |Rest], Codes)
    ->  HeadCodes = Head,
        string_codes(ArgsStr, Rest),
        atom_string(ArgsAtom, ArgsStr)
    ;   HeadCodes = Codes,
        ArgsAtom = ''
    ).

strip_trailing_period(Codes, Stripped) :-
    append(Stripped, [0' , 0'., 0'\n], Codes), !.
strip_trailing_period(Codes, Stripped) :-
    append(Stripped, [0' , 0'.], Codes), !.
strip_trailing_period(Codes, Codes).

%   Match =|Class<op>Name|= against the four xpce arrow forms and the
%   instance- / class-variable dash and dot forms. Longest arrow first
%   so =|Class<->Name|= isn't read as =|Class<-Name|=. After the raw
%   match, =classify_member/4= asks the live xpce class whether the
%   member is an instance_variable; if so the kind is rewritten to
%   =|ivar(Access)|= so the index records it under the ivar bucket
%   and the renderer keeps the side-effect-free access info that the
%   plain =|Class-Name|= notation would otherwise drop.

head_codes_to_member(Codes, Kind, Class, Name) :-
    head_codes_to_member_raw(Codes, RawKind, Class, Name),
    classify_member(Class, Name, RawKind, Kind).

head_codes_to_member_raw(Codes, both_method, Class, Name) :-
    sep_split(Codes, [0'<, 0'-, 0'>], L, R), !,
    member_atoms(L, R, Class, Name).
head_codes_to_member_raw(Codes, send_method, Class, Name) :-
    sep_split(Codes, [0'-, 0'>], L, R), !,
    member_atoms(L, R, Class, Name).
head_codes_to_member_raw(Codes, get_method,  Class, Name) :-
    sep_split(Codes, [0'<, 0'-], L, R), !,
    member_atoms(L, R, Class, Name).
head_codes_to_member_raw(Codes, classvar,    Class, Name) :-
    sep_split(Codes, [0'.], L, R), !,
    member_atoms(L, R, Class, Name).
head_codes_to_member_raw(Codes, instvar,     Class, Name) :-
    sep_split(Codes, [0'-], L, R), !,
    member_atoms(L, R, Class, Name).

%!  classify_member(+Class, +Name, +RawKind, -Kind) is det.
%
%   Refine the parser's notation-derived RawKind by consulting the
%   live xpce class. The bullet is reclassified to =|ivar(Access)|=
%   only when we are inside the chapter's =|## Instance variables|=
%   section (so a same-named hand-written =|send_method|= or
%   =|get_method|= documented in its own section keeps its kind).
%   Outside any known member section -- e.g. an inline xpce face in
%   prose, or while loading a card outside the .md walker -- the
%   ivar reclassification still runs as a best-effort default.

classify_member(Class, Name, _, ivar(Access)) :-
    member_section_allows_ivar,
    live_ivar_access(Class, Name, Access),
    !.
classify_member(_, _, K, K).

member_section_allows_ivar :-
    (   current_member_section(Sec)
    ->  memberchk(Sec, [ivars, none])
    ;   true ).

%   The ivar's own =|access|= slot records which side-effect-free
%   accessors xpce auto-generates for the variable. A class can also
%   define explicit =|send_method|= or =|get_method|= with the same
%   name; those are distinct documented entities and must NOT be
%   conflated with the ivar's auto-accessor here -- they live in
%   their own =|## Send methods|= / =|## Get methods|= sections.

live_ivar_access(Class, Name, Access) :-
    catch(get(@pce, convert, Class, class, ClassObj), _, fail),
    catch(get(ClassObj, instance_variable, Name, IVar), _, fail),
    !,
    catch(get(IVar, access, Access), _, Access = none).

%!  walk_with_section(+List, -List) is det.
%
%   Walk the DOM list in order, updating =current_member_section/1=
%   when a member-section =|h2|= heading goes by. Each child still
%   passes through =xpce_dom_transform/2= for the usual rewrites.

walk_with_section([], []).
walk_with_section([H|T], [H1|T1]) :-
    update_section(H),
    xpce_dom_transform(H, H1),
    walk_with_section(T, T1).

update_section(H) :-
    heading_content(H, Content),
    !,
    (   heading_section_kind(Content, Sec) -> true ; Sec = none ),
    retractall(current_member_section(_)),
    assertz(current_member_section(Sec)).
update_section(_).

heading_content(H, C) :- H =.. [h1, _, C].
heading_content(H, C) :- H =.. [h2, _, C].
heading_content(H, C) :- H =.. [h3, _, C].
heading_content(H, C) :- H =.. [h4, _, C].

heading_section_kind(Content, ivars) :-
    heading_text(Content, "Instance variables"), !.
heading_section_kind(Content, sends) :-
    heading_text(Content, "Send methods"), !.
heading_section_kind(Content, gets) :-
    heading_text(Content, "Get methods"), !.
heading_section_kind(Content, classvars) :-
    heading_text(Content, "Class variables"), !.

heading_text(Content, Match) :-
    is_list(Content),
    with_output_to(string(S),
                   forall(member(T, Content), write_heading_tok(T))),
    sub_string(S, _, _, _, Match).

write_heading_tok(T) :- atom(T), !, write(T).
write_heading_tok(T) :- string(T), !, write(T).
write_heading_tok(_) :- write(' ').

%   Reject obvious prose-style "Foo --> Bar" lines (typed by the user
%   to describe a relationship, not to name a method). xpce class
%   identifiers are always lower-case, single-word identifiers; the
%   member name follows the same convention. No leading or trailing
%   whitespace either -- that signals a split inside the operator
%   character run.

member_atoms(L, R, Class, Name) :-
    L = [C0|_], C0 >= 0'a, C0 =< 0'z,
    \+ memberchk(0' , L),
    R = [N0|_], lower_or_underscore(N0),
    \+ memberchk(0' , R),
    atom_codes(Class, L),
    atom_codes(Name, R).

lower_or_underscore(C) :- C >= 0'a, C =< 0'z, !.
lower_or_underscore(0'_).

sep_split(Codes, Sep, Left, Right) :-
    append(Left, Tail, Codes),
    Left = [_|_],
    append(Sep, Right, Tail),
    Right = [_|_],
    !.

%   Don't recurse into face terms / \-marker terms -- their arg
%   shapes aren't DOM nodes.

skip_compound(\_).
skip_compound(:(_, _)).

xpce_li_to_member(li(Tokens), member(Kind, Class, Name, Args, Descr)) :-
    phrase(xpce_member_li(Kind, Class, Name, Args, Descr0), Tokens),
    xpce_dom_transform(Descr0, Descr).

xpce_member_li(Kind, Class, Name, Args, Descr) -->
    xpce_member_head(Kind, Class, Name),
    xpce_member_args(Args),
    !,
    descr_tokens(Descr).

%   By the time wiki_codes_to_dom finishes, the face hooks installed
%   for Class->name / Class<-name / Class-name / Class.name have
%   already turned the head tokens into a single \xpce(Kind, Args)
%   term sitting at the front of the li token list. Match those.

xpce_member_head(Kind, C, N) -->
    [\(pldoc_xpce:xpce(method, [C, '->', N]))],
    { C \== '', classify_member(C, N, send_method, Kind) }.
xpce_member_head(Kind, C, N) -->
    [\(pldoc_xpce:xpce(method, [C, '<-', N]))],
    { C \== '', classify_member(C, N, get_method, Kind) }.
xpce_member_head(Kind, C, N) -->
    [\(pldoc_xpce:xpce(method, [C, '<->', N]))],
    { C \== '', classify_member(C, N, both_method, Kind) }.
xpce_member_head(Kind, C, N) -->
    [\(pldoc_xpce:xpce(instvar, [C, N]))],
    { C \== '', classify_member(C, N, instvar, Kind) }.
xpce_member_head(classvar, C, N) -->
    [\(pldoc_xpce:xpce(classvar, [C, N]))],
    { C \== '' }.

%   Argument spec: optional " : <tokens-up-to-newline>". The body
%   description starts at the next token (the \n is consumed; the
%   tokens after it form the dd content).

%   Args end at either a \n (consumed) or at any compound token --
%   PlDoc wraps continuation paragraphs into pre(...) / p(...) /
%   similar terms with no explicit \n separator. Either way the args
%   tokens are everything atomic between ": " and the next non-atomic
%   token (or \n).

xpce_member_args(Args) -->
    [:, ' '],
    args_tokens(Tks),
    optional_nl,
    !,
    { detokenize(Tks, Args) }.
xpce_member_args('') -->
    ['\n'], !.
xpce_member_args('') -->
    [].

args_tokens([T|Ts]) -->
    [T],
    { atom(T), T \== '\n' },
    !,
    args_tokens(Ts).
args_tokens([]) --> [].

optional_nl --> ['\n'], !.
optional_nl --> [].

descr_tokens(Tokens, Tokens, []).

%   Reassemble an args token list back into a flat atom so it can be
%   handed verbatim to \sendmethod{C}{S}{ArgSpec}. Atoms/words come
%   out as-is; the special " ' '" whitespace token becomes one space.

detokenize(Tokens, Atom) :-
    with_output_to(string(Str),
                   forall(member(T, Tokens), write_token_for_args(T))),
    atom_string(Atom, Str).

write_token_for_args(' ') :- !, write(' ').
write_token_for_args(T) :- write(T).


                 /*******************************
                 *       INDEX EXTRACTION       *
                 *******************************/

%!  xpce_dom_extract_index(+Class, +DOM, -Entries) is det.
%
%   Walk a class chapter's DOM (post-transform) collecting one entry
%   per documented member, in the form
%
%       entry(Kind, Class, Name, AnchorSlug, Summary)
%
%   where Kind is =send=, =get=, =both= (instance variable) or
%   =classvar=, AnchorSlug is the ltx2htm-side anchor that
%   sty_xpce.pl's =sendmethod=/=getmethod=/=bothmethod= rule
%   generated (=|class-<C>-<kind>-<S>|=, underscores stripped), and
%   Summary is a one-line synopsis taken from the first sentence /
%   first 200 characters of the member's prose body.

xpce_dom_extract_index(Class, DOM, Entries) :-
    walk_index(DOM, Class, [], Entries).

walk_index([], _, T, T).
walk_index([H|Tail], Class, E0, E) :-
    walk_index_one(H, Class, E0, E1),
    walk_index(Tail, Class, E1, E).

walk_index_one(\(pldoc_xpce:xpce_member_list(Members)), Class, E0, E) :-
    !,
    foldl(member_to_entry(Class), Members, E0, E).
walk_index_one(Term, Class, E0, E) :-
    compound(Term),
    \+ skip_index_compound(Term),
    !,
    Term =.. [_|Args],
    walk_args_index(Args, Class, E0, E).
walk_index_one(_, _, E, E).

walk_args_index([], _, E, E).
walk_args_index([A|T], Class, E0, E) :-
    (   is_list(A)
    ->  walk_index(A, Class, E0, E1)
    ;   walk_index_one(A, Class, E0, E1)
    ),
    walk_args_index(T, Class, E1, E).

skip_index_compound(\_).
skip_index_compound(:(_, _)).

member_to_entry(_DefaultClass,
                member(KindIn, Class, Name, _ArgSpec, Descr),
                E0, [entry(Kind, Class, Name, Anchor, Summary)|E0]) :-
    kind_label(KindIn, Kind),
    member_anchor(Class, Kind, Name, Anchor),
    summary_of_descr(Descr, Summary).

kind_label(send_method, send).
kind_label(get_method,  get).
kind_label(both_method, both).
kind_label(ivar(_),     ivar).
%   Notation fallback when xpce can't confirm an ivar (class not
%   loaded). The plain =|Class-Name|= form was authored as an ivar,
%   so route it into the ivar bucket with unknown access.
kind_label(instvar,     ivar).
kind_label(classvar,    classvar).

%   Strip underscores from the class slug (so the file name stays
%   stable: =|class-window_decorator.html|= → =|class-windowdecorator.html|=
%   to match the PlDoc/doc_latex convention for the chapter heading
%   label). Preserve them in the selector slug so members that differ
%   only in a leading underscore (=|object->free|= vs
%   =|object->_free|=) get distinct anchors.

member_anchor(C, K, S, Anchor) :-
    safe_anchor(C, C1),
    format(atom(Anchor), 'class-~w-~w-~w', [C1, K, S]).

safe_anchor(In, Out) :-
    atom_chars(In, Chars),
    delete(Chars, '_', Safe),
    atom_chars(Out, Safe).

%   First sentence / first 200 characters of the prose body. The body
%   is the DOM-token list under each dl item; pull text content, trim,
%   cap. Returns '' for empty bodies.

summary_of_descr(Descr, Summary) :-
    descr_text(Descr, Text),
    summarise(Text, Summary).

descr_text(Tokens, Text) :-
    with_output_to(string(Text), forall(member(T, Tokens), write_descr(T))).

write_descr(T) :- atom(T), !, write(T).
write_descr(T) :- string(T), !, write(T).
write_descr(T) :- number(T), !, write(T).
write_descr(\(_M:Term)) :-
    !,
    write_face_text(Term).
write_descr(code(X))  :- !, write(X).
write_descr(b(X))     :- !, descr_text([X], _).
write_descr(i(X))     :- !, descr_text([X], _).
write_descr(strong(X)):- !, descr_text([X], _).
write_descr(_) :- write(' ').

write_face_text(xpce(method, [C, A, S])) :- !, format("~w~w~w", [C, A, S]).
write_face_text(xpce(classvar, [C, V]))  :- !, format("~w.~w", [C, V]).
write_face_text(xpce(instvar,  [C, V]))  :- !, format("~w-~w", [C, V]).
write_face_text(xpce(objref,   [R]))     :- !, format("@~w", [R]).
write_face_text(xpce(errref,   [I]))     :- !, format("!~w", [I]).
write_face_text(xpce(classref, [C]))     :- !, format("class ~w", [C]).
write_face_text(xpce(classref, [Lead, C])) :- !, format("~w ~w", [Lead, C]).
write_face_text(_) :- write(' ').

%   Trim leading/trailing whitespace, cut at first period (or 200
%   characters), normalise internal whitespace.

summarise(Text, Summary) :-
    string_codes(Text, Codes),
    phrase(trimmed(Trim), Codes),
    string_codes(StrTrim, Trim),
    (   sub_string(StrTrim, B, _, _, "."),
        B > 0,
        B < 200
    ->  sub_string(StrTrim, 0, B, _, S0),
        atom_string(Summary, S0)
    ;   string_length(StrTrim, L),
        Cap is min(L, 200),
        sub_string(StrTrim, 0, Cap, _, S0),
        atom_string(Summary, S0)
    ).

trimmed(Out) -->
    leading_ws,
    body_chars(Out).

leading_ws --> [C], { code_type(C, space) }, !, leading_ws.
leading_ws --> [].

body_chars([C|T]) --> [C], { \+ code_type(C, space) }, !, body_chars(T).
body_chars([0' |T]) --> [C], { code_type(C, space) }, !, ws_run, body_chars(T).
body_chars([]) --> [].

ws_run --> [C], { code_type(C, space) }, !, ws_run.
ws_run --> [].


                 /*******************************
                 *       MEMBER-LIST EMIT       *
                 *******************************/

%!  xpce_member_list(+Members)// is det.
%
%   Render the rewritten DOM block. LaTeX branch: wrap in a
%   description environment and emit a \sendmethod/\getmethod/... per
%   item, followed by the member's prose. HTML branch: defer to the
%   default \xpce_member rendering through html//1 (sty_xpce.pl will
%   pick up the LaTeX-side \sendmethod calls when invoked via
%   ltx2htm).

:- use_module(library(http/html_write), [html/3]).

xpce_member_list(Members) -->
    { current_xpce_backend(Backend) },
    member_list_in(Backend, Members).

member_list_in(latex, Members) -->
    pldoc_latex:latex(cmd(begin(description))),
    member_items_latex(Members),
    pldoc_latex:latex(cmd(end(description))).
member_list_in(html, Members) -->
    html(dl(class='xpce-members', \member_items_html(Members))).

%   Members with an empty Descr followed by a sibling with a non-empty
%   Descr form a bundle that shares one description. Mirror the
%   =|\nodescription|= convention pldoc uses for bundled predicate
%   modes: emit =|\nodescription|= before each header in the bundle
%   except the last (which carries the body). At LaTeX render time
%   =|pl.sty|='s =|\definition|= macro picks the header-only branch
%   when =|\@nodescription|= is set; ltx2htm's HTML writer
%   (=|empty_dd/2|= in =|latex2html.pl|=) collapses the empty
%   =|<dd>|= that the bundled headers would otherwise open, leaving
%   the manindex parser to merge all headers under the next
%   non-empty =|<dd>|='s summary.

member_items_latex([]) --> [].
member_items_latex([M|Ms]) -->
    { is_empty_descr(M), Ms = [_|_] }, !,
    member_item_head_latex(M),
    pldoc_latex:latex(cmd(nodescription)),
    [nl_exact(1)],
    member_items_latex(Ms).
member_items_latex([M|Ms]) -->
    member_item_latex(M),
    member_items_latex(Ms).

%   Header-only emit for a bundled member: just the =|\cmd|= for the
%   header followed by a single newline (no blank line). The next
%   member's =|\cmd|= follows immediately so ltx2htm's =|empty_dd/2|=
%   sees no intervening paragraph token and can collapse the empty
%   =|<dd>|= the header opens.

member_item_head_latex(member(send_method, C, S, A, _)) -->
    pldoc_latex:latex(cmd(sendmethod(C, S, A))),
    [nl_exact(1)].
member_item_head_latex(member(get_method,  C, S, A, _)) -->
    { split_get_args(A, Args, Ret) },
    pldoc_latex:latex(cmd(getmethod(C, S, Args, Ret))),
    [nl_exact(1)].
member_item_head_latex(member(both_method, C, V, A, _)) -->
    pldoc_latex:latex(cmd(bothmethod(C, V, A))),
    [nl_exact(1)].
member_item_head_latex(member(instvar,     C, V, A, _)) -->
    pldoc_latex:latex(cmd(bothmethod(C, V, A))),
    [nl_exact(1)].
member_item_head_latex(member(ivar(Access), C, V, A, _)) -->
    { ivar_latex_cmd(Access, Cmd),
      Term =.. [Cmd, C, V, A] },
    pldoc_latex:latex(cmd(Term)),
    [nl_exact(1)].
member_item_head_latex(member(classvar,    C, V, A, _)) -->
    pldoc_latex:latex(cmd(classvarmethod(C, V, A))),
    [nl_exact(1)].

is_empty_descr(member(_, _, _, _, Descr)) :- empty_descr(Descr).

empty_descr([]) :- !.
empty_descr([T]) :- atom(T), atom_length(T, 0).
empty_descr([T]) :- string(T), string_length(T, 0).

member_item_latex(member(send_method, C, S, A, Descr)) -->
    pldoc_latex:latex(cmd(sendmethod(C, S, A))),
    [nl(1)],
    pldoc_latex:latex(Descr),
    [nl_exact(2)].
member_item_latex(member(get_method,  C, S, A, Descr)) -->
    { split_get_args(A, Args, Ret) },
    pldoc_latex:latex(cmd(getmethod(C, S, Args, Ret))),
    [nl(1)],
    pldoc_latex:latex(Descr),
    [nl_exact(2)].
member_item_latex(member(both_method, C, V, A, Descr)) -->
    pldoc_latex:latex(cmd(bothmethod(C, V, A))),
    [nl(1)],
    pldoc_latex:latex(Descr),
    [nl_exact(2)].
member_item_latex(member(instvar,     C, V, A, Descr)) -->
    %   Notation-only fallback when xpce couldn't confirm the ivar.
    %   Render as the legacy =|class-V|= form via bothmethod for
    %   visual continuity.
    pldoc_latex:latex(cmd(bothmethod(C, V, A))),
    [nl(1)],
    pldoc_latex:latex(Descr),
    [nl_exact(2)].
member_item_latex(member(ivar(Access), C, V, A, Descr)) -->
    { ivar_latex_cmd(Access, Cmd),
      Term =.. [Cmd, C, V, A] },
    pldoc_latex:latex(cmd(Term)),
    [nl(1)],
    pldoc_latex:latex(Descr),
    [nl_exact(2)].
member_item_latex(member(classvar,    C, V, A, Descr)) -->
    pldoc_latex:latex(cmd(classvarmethod(C, V, A))),
    [nl(1)],
    pldoc_latex:latex(Descr),
    [nl_exact(2)].

ivar_latex_cmd(both, ivarbothmethod).
ivar_latex_cmd(get,  ivargetmethod).
ivar_latex_cmd(send, ivarsendmethod).
ivar_latex_cmd(none, ivarnonemethod).

%   For "Class<-name: args -> ret", split off the trailing " -> ret".
%   Handle "-> ret" (ret-only, no args before) and "args -> ret".

split_get_args(A, '', Ret) :-
    sub_atom(A, 0, 3, _, '-> '),
    !,
    sub_atom(A, 3, _, 0, Ret).
split_get_args(A, Args, Ret) :-
    sub_atom(A, B, _, _, ' -> '),
    !,
    sub_atom(A, 0, B, _, Args),
    Pos is B + 4,
    sub_atom(A, Pos, _, 0, Ret).
split_get_args(A, A, '').

member_items_html([]) --> [].
member_items_html([M|Ms]) -->
    member_item_html(M),
    member_items_html(Ms).

member_item_html(member(Kind, C, N, A, Descr)) -->
    { member_html_signature(Kind, C, N, A, Sig) },
    html([ dt(class='xpce-member', Sig),
           dd(class='xpce-member', \dom_to_html(Descr)) ]).

member_html_signature(send_method, C, N, '', S) :-
    !,
    format(atom(S), '~w->~w', [C, N]).
member_html_signature(send_method, C, N, A,  S) :-
    format(atom(S), '~w->~w: ~w', [C, N, A]).
member_html_signature(get_method,  C, N, '', S) :-
    !,
    format(atom(S), '~w<-~w', [C, N]).
member_html_signature(get_method,  C, N, A,  S) :-
    format(atom(S), '~w<-~w: ~w', [C, N, A]).
member_html_signature(both_method, C, V, '', S) :-
    !,
    format(atom(S), '~w<->~w', [C, V]).
member_html_signature(both_method, C, V, A,  S) :-
    format(atom(S), '~w<->~w: ~w', [C, V, A]).
member_html_signature(instvar,     C, V, '', S) :-
    !,
    format(atom(S), '~w-~w', [C, V]).
member_html_signature(instvar,     C, V, A,  S) :-
    format(atom(S), '~w-~w: ~w', [C, V, A]).
member_html_signature(ivar(Access), C, V, '', S) :-
    !,
    access_arrow(Access, Arrow),
    format(atom(S), '~w~w~w', [C, Arrow, V]).
member_html_signature(ivar(Access), C, V, A,  S) :-
    access_arrow(Access, Arrow),
    format(atom(S), '~w~w~w: ~w', [C, Arrow, V, A]).
member_html_signature(classvar,    C, V, '', S) :-
    !,
    format(atom(S), '~w.~w', [C, V]).
member_html_signature(classvar,    C, V, A,  S) :-
    format(atom(S), '~w.~w: ~w', [C, V, A]).

access_arrow(both, '<->').
access_arrow(get,  '<-').
access_arrow(send, '->').
access_arrow(none, '-').

dom_to_html(Tokens) -->
    html(\(html_write:html(Tokens))).
