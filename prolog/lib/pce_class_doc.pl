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

:- module(pce_class_doc,
          [ xpce_doc_comment/5,         % +Object, -Signature, -Summary,
                                        % -Markdown, -File
            xpce_doc_dom/3,             % +Object, -File, -DOM
            xpce_doc_html//3            % +Object, +Pairs, +Options
          ]).
:- use_module(library(pce)).
:- autoload(library(apply), [maplist/3, foldl/4, exclude/3]).
:- autoload(library(lists), [append/3, min_list/2]).
:- use_module(library(pldoc), []).     % must be loaded before doc_process
:- use_module(library(pldoc/doc_process), [doc_comment/4, doc_signature/2]).
:- autoload(library(prolog_xref),
            [ xref_source/2,
              xref_object_comment/4,
              xref_object_signature/3
            ]).
:- autoload(library(pldoc/doc_wiki),
            [ indented_lines/3,
              wiki_codes_to_dom/3
            ]).
:- autoload(library(http/html_write), [html/3, print_html/1]).
%   PlDoc's wiki DOM may contain \term(...) and \tags(...)
:- use_module(library(pldoc/doc_html), [term//3, tags//1]).
:- autoload(library(sgml), [load_html/3]).
%   Not autoloaded: library(pldoc_xpce) must be loaded before parsing
%   the Markdown as it adds the xpce notations to the PlDoc wiki parser.
:- use_module(library(pldoc_xpce),
              [ with_xpce_backend/2,
                with_xpce_chapter_class/2,
                xpce_dom_transform/2
              ]).

/** <module> PlDoc documentation of Prolog defined classes

Classes defined in Prolog are documented using PlDoc comments on their
members.  These comments are compiled into the PlDoc objects
xpce(Class, Kind, Name), where Kind is one of `send`, `get`, `both`,
`ivar`, `classvar` or `class`.  See prolog:doc_compile_comment/6 in
library(pce_expansion).

This library finds the documentation of a member, either from the PlDoc
database if PlDoc collected the comments while loading, or by
cross-referencing the source of the class.  The latter reflects the
current version of the file.  The documentation is provided as Markdown
in the format of the reference manual, as HTML DOM for the doc_window
based manual card and as HTML for the PlDoc web pages.
*/

%!  xpce_doc_comment(+Object, -Signature, -Summary, -Markdown,
%!                   -File) is semidet.
%
%   Find the documentation for Object, a term xpce(Class, Kind, Name).
%   Signature is the signature in the notation of the reference manual
%   (e.g., `editor->align: column=int`) or `""` if it is unknown.
%   Markdown is the comment without the header line and indentation and
%   File is the file holding the comment.

xpce_doc_comment(Object, Signature, Summary, Markdown, File) :-
    Object = xpce(_Class, _Kind, _Name),
    once(class_doc_comment(Object, Signature, Summary, Comment, File)),
    comment_markdown(Comment, Markdown).

%!  class_doc_comment(?Object, -Signature, -Summary, -Comment,
%!                    -File) is nondet.
%
%   Enumerate the documented members of a class.  The class of Object
%   must be known.  If PlDoc collected the comments while loading, they
%   are in the database.  Otherwise we cross-reference the source of
%   the class.

class_doc_comment(Object, Signature, Summary, Comment, File) :-
    Object = xpce(Class, _, _),
    (   loaded_class_docs(Class)
    ->  loaded_doc_comment(Object, Signature, Summary, Comment, File)
    ;   class_source(Class, File),
        xref_doc_comment(File, Object, Signature, Summary, Comment)
    ).

loaded_class_docs(Class) :-
    doc_comment(_:xpce(Class, _, _), _, _, _),
    !.

loaded_doc_comment(Object, Signature, Summary, Comment, File) :-
    doc_comment(_:Object, File:_Line, Summary, Comment),
    (   doc_signature(_:Object, Signature0)
    ->  Signature = Signature0
    ;   Signature = ""
    ).

xref_doc_comment(File, Object, Signature, Summary, Comment) :-
    xref_source(File, [silent(true)]),
    xref_object_comment(File, Object, Summary, Comment),
    (   xref_object_signature(File, Object, Signature0)
    ->  Signature = Signature0
    ;   Signature = ""
    ).

class_source(ClassName, File) :-
    get(@pce, convert, ClassName, class, Class),
    get(Class, source, source_location(File, _Line)).

%!  comment_markdown(+Comment:string, -Markdown:string) is det.
%!  comment_markdown(+Comment:string, -Header:string,
%!                   -Markdown:string) is det.
%
%   Remove the comment delimiters, the header line and the common
%   indentation from Comment.  Header is the header line without the
%   leading `!` of a `%!` comment.

comment_markdown(Comment, Markdown) :-
    comment_markdown(Comment, _Header, Markdown).

comment_markdown(Comment, Header, Markdown) :-
    comment_prefixes(Comment, Prefixes),
    string_codes(Comment, Codes),
    indented_lines(Codes, Prefixes, [_-HeaderCodes|Lines]),
    string_codes(Header0, HeaderCodes),
    split_string(Header0, "", "! \t", [Header]),
    exclude(empty_line, Lines, NonEmpty),
    (   NonEmpty == []
    ->  Markdown = ""
    ;   maplist(line_indent, NonEmpty, Indents),
        min_list(Indents, Min),
        maplist(dedent_line(Min), Lines, Strings),
        atomic_list_concat(Strings, '\n', Text),
        split_string(Text, "", "\n ", [Markdown])
    ).

comment_prefixes(Comment, ["%"]) :-
    sub_string(Comment, 0, _, _, "%"),
    !.
comment_prefixes(_, ["/**", " *"]).

empty_line(_-[]).

line_indent(Indent-_, Indent).

dedent_line(_, _-[], "") :-
    !.
dedent_line(Min, Indent-Codes, String) :-
    Spaces is Indent - Min,
    length(Pre, Spaces),
    maplist(=(0' ), Pre),
    append(Pre, Codes, All),
    string_codes(String, All).

%!  xpce_doc_dom(+Object, -File, -DOM) is semidet.
%
%   DOM is the documentation of Object as a list of HTML element/3
%   terms.  For a member this is the =|<dt>|= and =|<dd>|= of the
%   member.  For xpce(Class, class, Class) this is a heading, the
%   description of the class and the list of its documented members.

xpce_doc_dom(xpce(Class, class, Class), File, DOM) :-
    !,
    xpce_doc_comment(xpce(Class, class, Class), _, Title, Markdown, File),
    class_members(File, Class, Members),
    format(string(Head), "## class ~w: ~w~n~n~s~n~n", [Class, Title, Markdown]),
    foldl(member_markdown, Members, Head, Text),
    markdown_dom(Class, Text, DOM).
xpce_doc_dom(xpce(Class, Kind, Name), File, DOM) :-
    xpce_doc_comment(xpce(Class, Kind, Name), _, _, _, File),
    member_markdown(xpce(Class, Kind, Name), "", Text),
    markdown_dom(Class, Text, HTML),
    member_dl(HTML, DOM).

member_dl([element(dl, _, Items)|_], Items) :-
    !.
member_dl([_|T], Items) :-
    member_dl(T, Items).

%   The members of Class that have documentation.

class_members(File, Class, Members) :-
    findall(xpce(Class, Kind, Name),
            ( class_doc_comment(xpce(Class, Kind, Name), _, _, _, File),
              Kind \== class
            ),
            Members0),
    sort(Members0, Members).

%   Add a member to the Markdown text in the format of the reference
%   manual: "- Signature" followed by the indented description.

member_markdown(Object, Text0, Text) :-
    xpce_doc_comment(Object, Signature, _, Markdown, _),
    member_item(Object, Signature, Markdown, Text0, Text).

member_item(Object, Signature0, Markdown, Text0, Text) :-
    (   Signature0 == ""
    ->  Object = xpce(Class, Kind, Name),
        kind_arrow(Kind, Arrow),
        format(string(Signature), '~w~w~w', [Class, Arrow, Name])
    ;   Signature = Signature0
    ),
    split_string(Markdown, "\n", "", Lines),
    maplist(indent_line, Lines, Indented),
    atomic_list_concat(Indented, '\n', Body),
    format(string(Text), "~s- ~w~n~w~n~n", [Text0, Signature, Body]).

indent_line("", "") :-
    !.
indent_line(Line, Indented) :-
    string_concat("    ", Line, Indented).

kind_arrow(send,     '->').
kind_arrow(get,      '<-').
kind_arrow(both,     '<->').
kind_arrow(ivar,     '-').
kind_arrow(classvar, '.').

%!  xpce_doc_html(+Object, +Pairs, +Options)// is semidet.
%
%   Emit HTML for the PlDoc comments Pairs (a list Pos-Comment) on
%   Object, a term xpce(Class, Kind, Name), possibly qualified with a
%   module.  This implements prolog:doc_object//3 for the PlDoc web
%   pages.  A member is emitted as in the reference manual.  The class
%   itself is emitted as a heading with the description of the class.

xpce_doc_html(Object, [(File:_Line)-Comment|_], _Options) -->
    { strip_module(Object, _, xpce(Class, Kind, Name)),
      object_markdown(xpce(Class, Kind, Name), File, Comment, Text),
      markdown_tokens(Class, Text, Tokens)
    },
    tokens(Tokens).

object_markdown(xpce(Class, class, Class), _File, Comment, Text) :-
    !,
    comment_markdown(Comment, Header, Markdown),
    (   string_concat("<class>", Title0, Header)
    ->  normalize_space(string(Title), Title0)
    ;   Title = ""
    ),
    format(string(Text), "## class ~w: ~w~n~n~s~n", [Class, Title, Markdown]).
object_markdown(Object, File, Comment, Text) :-
    comment_markdown(Comment, Markdown),
    (   doc_signature(_:Object, Signature)
    ->  true
    ;   xref_object_signature(File, Object, Signature)
    ->  true
    ;   Signature = ""
    ),
    member_item(Object, Signature, Markdown, "", Text).

tokens(Tokens, List, Tail) :-
    append(Tokens, Tail, List).

%!  markdown_dom(+Class, +Markdown, -DOM) is det.
%
%   Render Markdown using PlDoc and the xpce extensions and parse the
%   result into a list of HTML element/3 terms.

markdown_dom(Class, Markdown, DOM) :-
    markdown_tokens(Class, Markdown, Tokens),
    with_output_to(string(HTML), print_html(Tokens)),
    setup_call_cleanup(
        open_string(HTML, In),
        load_html(In, DOM0, [cdata(string)]),
        close(In)),
    include_elements(DOM0, DOM).

%!  markdown_tokens(+Class, +Markdown, -Tokens) is det.
%
%   Render Markdown using PlDoc and the xpce extensions into HTML
%   tokens as produced by html//1.

markdown_tokens(Class, Markdown, Tokens) :-
    string_codes(Markdown, Codes),
    wiki_codes_to_dom(Codes, [], WikiDOM0),
    xpce_dom_transform(WikiDOM0, WikiDOM),
    with_xpce_backend(
        html,
        with_xpce_chapter_class(
            Class,
            phrase(html(WikiDOM), Tokens))).

include_elements([], []).
include_elements([H|T0], [H|T]) :-
    H = element(_,_,_),
    !,
    include_elements(T0, T).
include_elements([_|T0], T) :-
    include_elements(T0, T).
