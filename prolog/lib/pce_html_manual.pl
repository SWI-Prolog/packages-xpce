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

:- module(pce_html_manual,
          [ man_html_object/1            % +xpce-object
          ]).
:- use_module(library(pce)).
:- use_module(library(doc/load)).
:- use_module(library(doc/browser)).
:- use_module(library(doc/window)).
:- use_module(library(pldoc/man_index), [manual_object/5]).

/** <module> Open the generated XPCE reference manual in a doc_browser

A lightweight companion to =|library(pce_manual)|=. Used by the
=|manpce|= frame to show the per-card HTML page (or an embedded
HTML chunk via =|man_html_card|=) for a live xpce class, method,
instance variable or global object.

Anchor slugs strip underscores from class and selector names so they
line up with the labels =|ltx2htm|= writes (which mirror PlDoc's
=|delete_unsafe_label_chars/2|=).
*/

%!  man_html_object(+Object) is det.
%
%   Show the manual page for a live xpce class, send/get method or
%   instance variable. Used by the manpce frame's request_selection
%   path so double-clicking an entry in the class browser opens the
%   HTML page rather than the saved-card editor.

man_html_object(Obj) :-
    object_url(Obj, URL),
    !,
    open_doc_browser(URL).
man_html_object(Obj) :-
    print_message(warning, pce_html_manual(no_help(Obj))).

object_url(Obj, URL) :-
    object_spec(Obj, Spec),
    spec_url(Spec, URL).

%   Card-side wrapper used by manpce for globals (=|@display|=, ...).
%   manpce/1 passes a plain Prolog =|man_global(Ref)|= compound while
%   the Object Browser uses live =|man_global|= xpce instances; cover
%   both.
object_spec(man_global(Ref),    object(Ref)) :- !.
object_spec(man_global(Ref, _), object(Ref)) :- !.
object_spec(Obj, object(Ref)) :-
    object(Obj),
    class_loaded(man_global),
    send(Obj, instance_of, man_global),
    !,
    get(Obj, reference, Ref).
object_spec(Obj, error(Id)) :-
    object(Obj),
    send(Obj, instance_of, error),
    !,
    get(Obj, id, Id).
object_spec(Obj, example(Name)) :-
    object(Obj),
    class_loaded(man_example_card),
    send(Obj, instance_of, man_example_card),
    !,
    get(Obj, name, Name).

object_spec(Obj, Class) :-
    send(Obj, instance_of, class),
    !,
    get(Obj, name, Class).
object_spec(Obj, ->(Class, Name)) :-
    send(Obj, instance_of, send_method),
    !,
    behaviour_class_name(Obj, Class, Name).
object_spec(Obj, <-(Class, Name)) :-
    send(Obj, instance_of, get_method),
    !,
    behaviour_class_name(Obj, Class, Name).
object_spec(Obj, <->(Class, Name)) :-
    send(Obj, instance_of, variable),
    !,
    behaviour_class_name(Obj, Class, Name).
object_spec(Obj, xpce(classvar, Class, Name)) :-
    send(Obj, instance_of, class_variable),
    !,
    behaviour_class_name(Obj, Class, Name).

class_loaded(Name) :-
    catch(get(@pce, convert, Name, class, _), _, fail).

behaviour_class_name(Obj, Class, Name) :-
    get(Obj, name, Name),
    get(Obj, context, ContextClass),
    get(ContextClass, name, Class).

spec_url(Spec, URL) :-
    spec_anchor(Spec, ClassFile, FragmentOrEmpty),
    man_html_url(ClassFile, Base),
    (   FragmentOrEmpty == ''
    ->  URL = Base
    ;   atomic_list_concat([Base, '#', FragmentOrEmpty], URL)
    ).

%!  spec_anchor(+Spec, -ClassFile, -Fragment) is semidet.
%
%   ClassFile is the basename =|class-<safe>.html|= (or one of the
%   non-class top-level chapters); Fragment is the in-page anchor
%   (=|''|= when none).

spec_anchor(Class, File, '') :-
    atom(Class),
    !,
    class_html_file(Class, File).
spec_anchor(object(Ref), File, Anchor) :-
    global_section_file(Ref, Path),
    !,
    file_base_name(Path, File),
    section_anchor_id(Ref, Anchor).
spec_anchor(error(Id), File, Anchor) :-
    error_section_file(Id, Path),
    !,
    file_base_name(Path, File),
    error_section_id(Id, Anchor).
spec_anchor(example(Name), File, Anchor) :-
    example_section_file(Name, Path),
    !,
    file_base_name(Path, File),
    example_section_id(Name, Anchor).
spec_anchor(->(Class, Sel), File, Anchor) :-
    member_anchor(Class, send, Sel, File, Anchor).
spec_anchor(<-(Class, Sel), File, Anchor) :-
    member_anchor(Class, get, Sel, File, Anchor).
spec_anchor(<->(Class, Var), File, Anchor) :-
    member_anchor(Class, both, Var, File, Anchor).
spec_anchor(-(Class, Var), File, Anchor) :-
    member_anchor(Class, both, Var, File, Anchor).
%   Class-variable specs (=|Class.Var|=) can't be written literally
%   because SWI-Prolog reads =|.|= as the dict accessor; the wrapper
%   xpce(classvar, Class, Var) lets callers reach the same page.
spec_anchor(xpce(classvar, Class, Var), File, Anchor) :-
    member_anchor(Class, classvar, Var, File, Anchor).

member_anchor(Class, Kind, Selector, File, Anchor) :-
    atom(Class),
    atom(Selector),
    class_html_file(Class, File),
    safe_id(Class, C),
    atomic_list_concat(['class-', C, '-', Kind, '-', Selector], Anchor).

class_html_file(Class, File) :-
    safe_id(Class, Safe),
    atomic_list_concat(['class-', Safe, '.html'], File).

safe_id(In, Out) :-
    atom_chars(In, Chars),
    delete(Chars, '_', Safe),
    atom_chars(Out, Safe).

%!  man_html_url(+BaseName, -URL) is det.
%
%   Locate =|<swi-home>/xpce/man/refmanual/<BaseName>|= (the
%   swi_man_xpce file_search_path) and wrap it as a =|file:///...|=
%   URL.

man_html_url(BaseName, URL) :-
    absolute_file_name(swi_man_xpce(BaseName), Path,
                       [ access(read),
                         file_errors(fail)
                       ]),
    atom_concat('file://', Path, URL).

                 /*******************************
                 *           BROWSER            *
                 *******************************/

:- pce_global(@man_html_browser, new(doc_browser)).

open_doc_browser(URL) :-
    in_pce_thread(
        (   send(@man_html_browser, expose),
            send(@man_html_browser, url, URL)
        )).

                 /*******************************
                 *      EMBEDDED CHUNK VIEW     *
                 *******************************/

:- discontiguous load_member_dom/3.

%!  load_member_dom(+Obj, -Path, -DOM) is semidet.
%
%   Load the =|<dt> ... <dd>|= chunk PlDoc indexes under the
%   xpce(C,K,N) compound for Obj. The DOM is the standard
%   =|element(Tag, Attrs, Content)|= shape consumed by
%   library(doc/html).

load_member_dom(Obj, Path, DOM) :-
    object_spec(Obj, Spec),
    spec_to_xpce_term(Spec, xpce(Class, Kind, Name)),
    resolve_member_kind(Class, Kind, Name, ResolvedKind, Path),
    !,
    load_html(Path, HTML, [cdata(string), max_errors(-1), syntax_errors(quiet)]),
    member_anchor_slug(Class, ResolvedKind, Name, Slug),
    find_member_chunk(HTML, Slug, DOM).

%   The live xpce class system exposes an instance variable as both
%   a send_method and a get_method (where access permits). The
%   manual records the underlying ivar once under
%   =|xpce(C, ivar, N)|=, so a lookup for any of send/get/both falls
%   back through =|both|= (legacy both_method index) and finally to
%   =|ivar|= so the per-member card resolves regardless of how the
%   click site classified the member.

resolve_member_kind(Class, Kind, Name, Kind, Path) :-
    manual_object(xpce(Class, Kind, Name), _, Path, _, _),
    !.
resolve_member_kind(Class, Kind, Name, both, Path) :-
    ( Kind == send ; Kind == get ),
    manual_object(xpce(Class, both, Name), _, Path, _, _),
    !.
resolve_member_kind(Class, Kind, Name, ivar, Path) :-
    ( Kind == send ; Kind == get ; Kind == both ),
    manual_object(xpce(Class, ivar, Name), _, Path, _, _).
%   Class chapters have no per-member <dt> chunk -- fall back to the
%   whole class-<name>.html (or, eventually, the class intro section).

load_member_dom(Obj, Path, []) :-
    object_spec(Obj, Class),
    atom(Class),
    spec_url(Class, URL),
    atom_concat('file://', Path, URL).
load_member_dom(Obj, Path, DOM) :-
    object_spec(Obj, object(Ref)),
    global_section_file(Ref, Path),
    !,
    load_html(Path, HTML, [cdata(string), max_errors(-1), syntax_errors(quiet)]),
    section_anchor_id(Ref, SecId),
    find_section_chunk(HTML, SecId, DOM).
load_member_dom(Obj, Path, DOM) :-
    object_spec(Obj, error(Id)),
    error_section_file(Id, Path),
    !,
    load_html(Path, HTML, [cdata(string), max_errors(-1), syntax_errors(quiet)]),
    error_section_id(Id, SecId),
    find_section_chunk(HTML, SecId, DOM).
load_member_dom(Obj, Path, DOM) :-
    object_spec(Obj, example(Name)),
    example_section_file(Name, Path),
    !,
    load_html(Path, HTML, [cdata(string), max_errors(-1), syntax_errors(quiet)]),
    example_section_id(Name, SecId),
    find_section_chunk(HTML, SecId, DOM).

%   Globals are kept in the auto-numbered manual sections, e.g.
%   =|sec-3.1.html#sec:@display|=. We look the global up by its
%   man-section id (=|sec:@<Ref>|=, which the PlDoc indexer records
%   straight from the H2 title text) and return the file the
%   manindex points us at.

global_section_file(Ref, Path) :-
    section_anchor_id(Ref, SecId),
    manual_object(section(_, _, SecId, _), _, Path, _, _).

section_anchor_id(Ref, SecId) :-
    safe_id(Ref, Safe),
    atom_concat('sec:object-', Safe, SecId).

error_section_file(Id, Path) :-
    error_section_id(Id, SecId),
    manual_object(section(_, _, SecId, _), _, Path, _, _).

error_section_id(Id, SecId) :-
    safe_id(Id, Safe),
    atom_concat('sec:error-', Safe, SecId).

example_section_file(Name, Path) :-
    example_section_id(Name, SecId),
    manual_object(section(_, _, SecId, _), _, Path, _, _).

%   Mirror export_md.pl example_anchor_slug/2: drop spaces from
%   the example's display name to build a slug.

example_section_id(Name, SecId) :-
    atom_codes(Name, Codes),
    replace_spaces(Codes, Cleaned),
    safe_id_codes(Cleaned, Safe),
    atom_codes(Bare, Safe),
    atom_concat('sec:example-', Bare, SecId).

replace_spaces([], []).
replace_spaces([0' |T], [0'-|T2]) :- !, replace_spaces(T, T2).
replace_spaces([H|T], [H|T2]) :- replace_spaces(T, T2).

safe_id_codes([], []).
safe_id_codes([0'_|T], T2) :- !, safe_id_codes(T, T2).
safe_id_codes([H|T], [H|T2]) :- safe_id_codes(T, T2).

spec_to_xpce_term(->(C, N),                xpce(C, send,    N)).
spec_to_xpce_term(<-(C, N),                xpce(C, get,     N)).
spec_to_xpce_term(<->(C, N),               xpce(C, both,    N)).
spec_to_xpce_term(-(C, N),                 xpce(C, both,    N)).
spec_to_xpce_term(xpce(classvar, C, N),    xpce(C, classvar, N)).

%!  member_anchor_slug(+Class, +Kind, +Name, -Slug) is det.
%
%   Mirror the =|class-C-K-S|= anchor sty_xpce writes -- underscores
%   stripped from the class name (so the chapter filename matches
%   PlDoc's =|delete_unsafe_label_chars|=) but kept in the selector
%   so =|object->free|= and =|object->_free|= produce distinct ids.

member_anchor_slug(Class, Kind, Name, Slug) :-
    safe_id(Class, C),
    atomic_list_concat(['class-', C, '-', Kind, '-', Name], Slug).

%!  find_member_chunk(+HTML, +Slug, -Chunk) is semidet.
%
%   Walk the parsed HTML looking for a =|<dt>|= containing an
%   anchor with =|id=Slug|=. Return =|[Dt, Dd]|= where Dd is the
%   first =|<dd>|= that follows.

find_member_chunk(HTML, Slug, [Dt, Dd]) :-
    walk_dl_lists(HTML, Slug, Dt, Dd).

%   Deterministic DOM walk: at each list of nodes try the head; if
%   the head doesn't contain (or isn't) the target dt, move to the
%   tail. Cuts after each shape match keep this from blowing up on
%   prose-heavy chunks like frame->report's See-also paragraph.

walk_dl_lists([H|T], Slug, Dt, Dd) :-
    (   walk_one(H, Slug, Dt, Dd)
    ->  true
    ;   walk_dl_lists(T, Slug, Dt, Dd)
    ).

walk_one(element(dl, _, Items), Slug, Dt, Dd) :-
    !,
    dl_chunk(Items, Slug, Dt, Dd).
walk_one(element(_, _, Children), Slug, Dt, Dd) :-
    !,
    walk_dl_lists(Children, Slug, Dt, Dd).
walk_one(_, _, _, _) :- fail.

dl_chunk([Dt|Rest], Slug, Dt, Dd) :-
    Dt = element(dt, _, DtContent),
    dt_has_id(DtContent, Slug),
    !,
    next_dd(Rest, Dd).
dl_chunk([_|Rest], Slug, Dt, Dd) :-
    dl_chunk(Rest, Slug, Dt, Dd).

dt_has_id(Content, Slug) :-
    member(element(a, AAttr, _), Content),
    memberchk(id=Slug, AAttr), !.

next_dd([element(dd, A, C)|_], element(dd, A, C)) :- !.
next_dd([_|Rest], Dd) :- next_dd(Rest, Dd).

%!  find_section_chunk(+HTML, +SecId, -Chunk) is semidet.
%
%   For top-level chapters (=|objects|=, =|errors|=, ...) the
%   per-entry =|<h3>|= section headings carry the manindex id.
%   Walk the parsed document looking for that heading; the chunk
%   is the heading itself plus the sibling content up to the
%   next =|<h3>|=, =|<h2>|=, or end-of-block.

find_section_chunk(HTML, SecId, Chunk) :-
    walk_sections(HTML, SecId, Chunk).

walk_sections([H|T], SecId, Chunk) :-
    (   section_block(H, T, SecId, Chunk)
    ->  true
    ;   walk_one_section(H, SecId, Chunk)
    ->  true
    ;   walk_sections(T, SecId, Chunk)
    ).

walk_one_section(element(_, _, Children), SecId, Chunk) :-
    walk_sections(Children, SecId, Chunk).

%   The heading we want is some =|element(hN, _, Content)|= whose
%   own attributes (or a nested =|<a>|=) carry =|id=SecId|=.

section_block(Heading, Rest, SecId, [Heading|Body]) :-
    is_heading(Heading, HLevel, Attrs, Content),
    (   memberchk(id=SecId, Attrs)
    ;   heading_has_id(Content, SecId)
    ),
    !,
    take_until_heading(Rest, HLevel, Body).

is_heading(element(h1, A, C), 1, A, C).
is_heading(element(h2, A, C), 2, A, C).
is_heading(element(h3, A, C), 3, A, C).
is_heading(element(h4, A, C), 4, A, C).

heading_has_id(Content, SecId) :-
    member(element(a, A, _), Content),
    memberchk(id=SecId, A),
    !.

take_until_heading([], _, []).
take_until_heading([H|_], Level, []) :-
    is_heading(H, HLevel, _, _),
    HLevel =< Level,
    !.
take_until_heading([H|T], Level, [H|Rest]) :-
    take_until_heading(T, Level, Rest).

%   Keep only element/3 nodes -- inter-element whitespace produced
%   by the SGML parser confuses the definition_list parbox (which
%   has no ->cdata method).

%   Member chunks (=|[dt, dd]|=) need a synthetic =|<dl>|= context
%   for the renderer; section chunks (=|[h2|...]|=) are already
%   self-contained and shouldn't be wrapped.

wrap_chunk(DOM, Wrapped) :-
    DOM = [First|_],
    is_section_head(First),
    !,
    include(is_element, DOM, Wrapped).
wrap_chunk(DOM, [element(dl, [class=latex], OnlyElems)]) :-
    include(is_element, DOM, OnlyElems).

is_section_head(element(Tag, _, _)) :-
    memberchk(Tag, [h1, h2, h3, h4]).

is_element(element(_,_,_)).

%!  show_chunk(+MHC, +Path, +DOM) is det.
%
%   Common rendering path used both by selection/1 (initial load)
%   and goto_url/2 (link follow-up). Sets the doc_window's URL
%   slot to the source file and configures a doc_mode with a
%   matching base_url so relative links resolve correctly.

show_chunk(MHC, Path, DOM) :-
    atom_concat('file://', Path, URL),
    send(MHC, slot, url, URL),
    new(Mode, doc_mode),
    send(Mode, base_url, URL),
    wrap_chunk(DOM, Wrapped),
    send(MHC, show, Wrapped, Mode).

%!  resolve_url(+RelURL, +BaseURL, -AbsURL) is det.
%
%   Apply BaseURL to RelURL. RelURL may be absolute (kept as is) or
%   a =|name.html#anchor|= relative reference (re-rooted against
%   BaseURL's directory).

resolve_url(URL, _Base, URL) :-
    has_scheme(URL),
    !.
resolve_url(Rel, Base, Abs) :-
    sub_atom(Rel, 0, 1, _, '#'),
    !,
    split_url_anchor(Base, BaseFile, _),
    atom_concat(BaseFile, Rel, Abs).
resolve_url(Rel, Base, Abs) :-
    file_directory_url(Base, BaseDir),
    atom_concat(BaseDir, Rel, Abs).

%   "Has a scheme" = matches =|alpha+://|=. Plain check: split on
%   the first =|:|= and look for the =|//|= that follows.

has_scheme(URL) :-
    sub_atom(URL, B, 3, _, '://'),
    B > 0,
    sub_atom(URL, 0, B, _, Scheme),
    atom_codes(Scheme, Codes),
    forall(member(C, Codes),
           (   (C >= 0'a, C =< 0'z)
           ;   (C >= 0'A, C =< 0'Z)
           )),
    !.

file_directory_url(URL, Dir) :-
    atom_codes(URL, Codes),
    last_slash(Codes, 0, -1, Idx),
    Idx >= 0,
    !,
    Take is Idx + 1,
    sub_atom(URL, 0, Take, _, Dir).
file_directory_url(URL, URL).

last_slash([], _, Idx, Idx).
last_slash([0'/|T], I, _, Idx) :-
    !,
    I1 is I + 1,
    last_slash(T, I1, I, Idx).
last_slash([_|T], I, Last, Idx) :-
    I1 is I + 1,
    last_slash(T, I1, Last, Idx).

%!  url_to_chunk(+AbsURL, -Path, -DOM) is semidet.
%
%   For a =|file:///.../class-<C>.html#<slug>|= URL, parse the
%   target HTML and return the dt/dd chunk anchored on =|<slug>|=.

url_to_chunk(AbsURL, Path, DOM) :-
    split_url_anchor(AbsURL, FileURL, Anchor),
    Anchor \== '',
    atom_concat('file://', Path0, FileURL),
    Path = Path0,
    exists_file(Path),
    load_html(Path, HTML, [cdata(string), max_errors(-1), syntax_errors(quiet)]),
    (   find_member_chunk(HTML, Anchor, DOM)
    ->  true
    ;   find_section_chunk(HTML, Anchor, DOM)
    ).

split_url_anchor(URL, FileURL, Anchor) :-
    sub_atom(URL, B, _, _, '#'),
    !,
    sub_atom(URL, 0, B, _, FileURL),
    Skip is B + 1,
    sub_atom(URL, Skip, _, 0, Anchor).
split_url_anchor(URL, URL, '').

%!  pce_begin_class(+ClassName, +Super, +Comment).
%
%   A doc_window subclass with a =|->selection(Obj)|= method that
%   renders just the indexed chunk for Obj. Used by =|man_card_editor|=
%   to replace the legacy =|man_editor|= text panel.

:- pce_begin_class(man_html_card, doc_window,
                   "Render HTML chunk for a selected manual entry").

variable(selection, object*, get, "Current selected manual object").

initialise(MHC) :->
    "Create with empty content"::
    send_super(MHC, initialise).

selection(MHC, Obj:object*) :->
    "Display HTML for the selected object"::
    send(MHC, slot, selection, Obj),
    (   Obj == @nil
    ->  send(MHC, clear)
    ;   load_member_dom(Obj, Path, DOM),
        DOM \== []
    ->  show_chunk(MHC, Path, DOM)
    ;   object_spec(Obj, Class),
        atom(Class),
        spec_url(Class, URL)
    ->  send(MHC, slot, url, @nil),   % force reload if same URL
        send(MHC, url, URL)
    ;   send(MHC, show, [element(p, [], ['No manual entry indexed.'])])
    ).

%   Intercept link clicks inside the rendered chunk: if the URL
%   resolves to a =|class-<C>.html#class-<C>-<K>-<S>|= entry that
%   we have indexed, swap the chunk in-place instead of fully
%   loading the target file (which is what doc_window's default
%   ->url path does).

goto_url(MHC, URLSpec:name, _Dir:[{forward,backward}]) :->
    "Follow a link inside the current chunk"::
    get(MHC, url, Base),
    (   Base == @nil
    ->  AbsURL = URLSpec
    ;   resolve_url(URLSpec, Base, AbsURL)
    ),
    (   url_to_chunk(AbsURL, Path, DOM)
    ->  show_chunk(MHC, Path, DOM)
    ;   send_super(MHC, goto_url, URLSpec)
    ),
    notify_link_followed(MHC, AbsURL).

%!  notify_link_followed(+MHC, +AbsURL) is det.
%
%   When the click resolves to a live xpce object, update the
%   selection slot and tell the enclosing frame so the navigation
%   history can record the new location. Silent when the URL can't
%   be reversed (e.g. external sections).

notify_link_followed(MHC, AbsURL) :-
    get(MHC, frame, Frame),
    send(Frame, add_history, AbsURL).

:- pce_end_class.

:- multifile prolog:message//1.

prolog:message(pce_html_manual(no_help(Spec))) -->
    [ 'No HTML manual entry for ~p'-[Spec] ].
