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

:- module(man_idx,
          [ man_index_build/0,    % build @man_index from manindex.db
            xpce_manual_object/2  % +ManualObj, -XPCEObj
          ]).

:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(library(pldoc/man_index), [manual_object/5]).

/** <module> Build the xpce manual search index from manindex.db

This module populates @man_index (a =|chain_table|=) by walking the
rows =|library(pldoc/man_index)|= keeps for the xpce package: every
class, send / get / both method, class variable, global object and
error. The chain at each word holds the resolved xpce side object
(e.g. a class, a method, a man_global), not an encoded id atom -- the
search tool uses them as-is, the card viewer renders them through the
HTML manual machinery.

The Search tool used to deserialise the chain_table from
=|$PCEHOME/man/reference/index.obj|= and fall back to walking every
serialised =|.doc|= card; that tree was retired in Phase 8.
*/

:- pce_global(@man_index, new(chain_table)).

%!  man_index_build is det.
%
%   Populate @man_index. Idempotent: if the table already has entries
%   we leave it alone. Callers may =|send(@man_index, clear)|= first
%   to force a rebuild after the manindex changes.

man_index_build :-
    get(@man_index, size, N),
    N > 0,
    !.
man_index_build :-
    forall(xpce_manual_entry(XPCEObj, Words),
           index_words(Words, XPCEObj)).

index_words([], _).
index_words([W|T], Obj) :-
    (   skip_word(W)
    ->  true
    ;   send(@man_index, append, W, Obj)
    ),
    index_words(T, Obj).

%   Noise-word filter matching the old =|man/man_index.pl|=. Pruning
%   these keeps the wordlist from being dominated by stop words.

skip_word(W) :- noindex(W).

noindex(a).
noindex(also).
noindex(an).
noindex(and).
noindex(are).
noindex(be).
noindex(by).
noindex(class).
noindex(from).
noindex(if).
noindex(in).
noindex(is).
noindex(it).
noindex(method).
noindex(not).
noindex(object).
noindex(of).
noindex(or).
noindex(see).
noindex(that).
noindex(the).
noindex(this).
noindex(to).
noindex(used).
noindex(using).
noindex(was).
noindex(when).
noindex(will).
noindex(with).

%!  xpce_manual_entry(-XPCEObj, -Words:list(atom)) is nondet.
%
%   For every xpce-package row in =|manual_object/5|=, resolve the
%   row's Object term to a live xpce side object and return the words
%   to index against it. Rows that can't be resolved (e.g. a method
%   on a class that isn't loaded, a typo in a manual entry) are
%   silently dropped.

xpce_manual_entry(XPCEObj, Words) :-
    manual_object(ManObj, Summary, File, packages, _Off),
    is_xpce_file(File),
    xpce_manual_object(ManObj, XPCEObj),
    entry_words(ManObj, Summary, Words).

is_xpce_file(File) :-
    sub_atom(File, _, _, _, '/xpce/man/refmanual/').

%!  xpce_manual_object(+ManObj, -XPCEObj) is semidet.
%
%   Resolve a =|manual_object/5|= row key to a live xpce object the
%   manual GUI can display:
%
%       | =|xpce(C, classvar, V)|= | =|Class<-class_variable(V)|=    |
%       | =|xpce(C, send, M)|=     | =|Class<-send_method(M)|=       |
%       | =|xpce(C, get, M)|=      | =|Class<-get_method(M)|=        |
%       | =|xpce(C, both, V)|=     | =|Class<-instance_variable(V)|= |
%       | =|xpce(C, ivar, V)|=     | =|Class<-instance_variable(V)|= |
%       | =|c(Class)|=             | the =|class|= itself            |
%       | =|o(Ref)|=               | =|man_global(Ref)|=             |
%       | =|error(Id)|=            | the =|error|= itself            |
%
%   Anything else (sections, predicates, etc.) fails silently.

xpce_manual_object(xpce(C, classvar, V), Obj) :-
    !,
    get(@pce, convert, C, class, Class),
    get(Class, class_variable, V, Obj).
xpce_manual_object(xpce(C, send, M), Obj) :-
    !,
    get(@pce, convert, C, class, Class),
    get(Class, send_method, M, Obj).
xpce_manual_object(xpce(C, get, M), Obj) :-
    !,
    get(@pce, convert, C, class, Class),
    get(Class, get_method, M, Obj).
xpce_manual_object(xpce(C, both, V), Obj) :-
    !,
    get(@pce, convert, C, class, Class),
    get(Class, instance_variable, V, Obj).
xpce_manual_object(xpce(C, ivar, V), Obj) :-
    !,
    get(@pce, convert, C, class, Class),
    get(Class, instance_variable, V, Obj).
xpce_manual_object(c(C), Obj) :-
    !,
    get(@pce, convert, C, class, Obj).
xpce_manual_object(o(Ref), Obj) :-
    !,
    new(Obj, man_global(Ref)).
xpce_manual_object(error(Id), Obj) :-
    !,
    get(@pce, convert, Id, error, Obj).

                 /*******************************
                 *            WORDS             *
                 *******************************/

%   Tokens to index for each row. For an =|xpce(C, _, S)|= row we
%   surface both the class and member selector so a search for
%   =|frame|= surfaces every frame member, not just the ones whose
%   summary happens to mention it. For =|c|=, =|o|= and =|error|=
%   rows we just use the row's own name.

entry_words(xpce(C, _K, S), Summary, Words) :-
    !,
    text_words(C, CW),
    text_words(S, SW),
    summary_text(Summary, SumText),
    text_words(SumText, SumWords),
    append([CW, SW, SumWords], All),
    list_to_set(All, Words).
entry_words(ManObj, Summary, Words) :-
    obj_name(ManObj, ObjName),
    text_words(ObjName, NameWords),
    summary_text(Summary, SumText),
    text_words(SumText, SumWords),
    append(NameWords, SumWords, All),
    list_to_set(All, Words).

obj_name(c(C),     C) :- !.
obj_name(o(R),     R) :- !.
obj_name(error(E), E) :- !.

summary_text(S, T) :-
    (   string(S) ; atom(S) ),
    !,
    T = S.
summary_text(_, '').

%!  text_words(+Text, -Words:list(atom)) is det.
%
%   Tokenise Text into a list of lower-case atoms suitable for the
%   search index. Splits on whitespace, the usual punctuation and
%   xpce's class-member operators (=|->|=, =|<-|=, =|<->|=, =|.|=,
%   =|-|=, =|_|=); also emits the joined form alongside the
%   underscore-/hyphen-split parts (so a search for =|pce_global|=
%   still hits while =|global|= also resolves), and emits the lower-
%   case word-pieces of a CamelCase token (so =|ScrollBar|= surfaces
%   as =|scrollbar|=, =|scroll|= and =|bar|=). Tokens shorter than
%   2 characters are dropped.

text_words(Text, Words) :-
    (   atom(Text)   -> atom_string(Text, Str)
    ;   string(Text) -> Str = Text
    ;   atom_string(Text, Str)
    ),
    %   Eat the class-member arrows whole so they don't leave a
    %   stray =|-|= or =|>|= mangling the word on either side.
    string_replace_arrows(Str, Cleaned),
    split_string(Cleaned, " \t\n\r,.;:!?()[]{}<>`/\\\"'",
                 " \t\n\r", Tops),
    expand_tokens(Tops, Words0),
    list_to_set(Words0, Words).

string_replace_arrows(S0, S) :-
    %   Order matters: =|<->|= first, then =|->|=, then =|<-|=.
    string_replace(S0, "<->", " ", S1),
    string_replace(S1, "->",  " ", S2),
    string_replace(S2, "<-",  " ", S).

string_replace(S0, From, To, S) :-
    split_string(S0, "", "", _),  % normalise to a string
    atomic_list_concat(Parts, From, S0),
    atomic_list_concat(Parts, To, S).

expand_tokens([], []).
expand_tokens([T|Ts], Words) :-
    token_words(T, TW),
    expand_tokens(Ts, More),
    append(TW, More, Words).

%   For each whitespace-delimited token, return the lower-case forms
%   to index against it. We keep the joined form (=|pce_global|=) and
%   add the underscore-/hyphen-split parts and CamelCase parts.

token_words(Token, Words) :-
    string_lower(Token, Lower),
    split_string(Lower, "", "-", [Trimmed]),
    atom_string(LowerAtom, Trimmed),
    findall(W,
            ( token_part(Token, P),
              atom_string(W, P),
              good_word(W)
            ),
            Parts),
    (   good_word(LowerAtom)
    ->  Words = [LowerAtom|Parts]
    ;   Words = Parts
    ).

token_part(Token, Part) :-
    split_string(Token, "_-.", "", Pieces),
    member(Piece, Pieces),
    Piece \== "",
    string_lower(Piece, Part).
token_part(Token, Part) :-
    camel_split(Token, Pieces),
    Pieces = [_,_|_],         % only if we found a split
    member(Piece, Pieces),
    string_lower(Piece, Part).

%   CamelCase split. Pieces are alternations between:
%
%       - a lower-case run                 ( =|scroll|= )
%       - an upper-case run followed by 0+ lower-case  ( =|Bar|=, =|HTTP|=)
%       - non-alpha runs                   ( digits etc. )
%
%   For =|HTTPRequest|= we want [HTTP, Request]; the trick is that
%   when an upper-case run is followed by another upper-case letter
%   that itself starts a lower-case suffix, the last upper belongs to
%   the following piece. We greedy-match an upper run, then peek: if
%   only one upper follows + a lower, hand it back via push-back.

camel_split(Token, Pieces) :-
    string_codes(Token, Codes),
    phrase(camel(Pieces), Codes),
    !.

camel([Piece|Pieces]) -->
    camel_piece(Codes),
    { Codes \== [], string_codes(Piece, Codes) },
    camel(Pieces).
camel([]) --> [].

camel_piece(Codes) -->
    upper_run(URs), !,
    ( { URs = [_,_|_] }
    ->  %  Pull the last upper back if a lower follows -- it starts
        %  the next CamelCase piece.
        peek_lower_next,
        { append(Hd, [Last], URs) },
        push_back([Last]),
        { Codes = Hd }
    ;   lowers(Lowers),
        { append(URs, Lowers, Codes) }
    ).
camel_piece([C|Cs]) -->
    [C], { is_lower(C) },
    lowers(Cs).
camel_piece([C|Cs]) -->
    [C], { \+ is_alpha(C) },
    others(Cs).

upper_run([C|Cs]) --> [C], { is_upper(C) }, upper_run(Cs).
upper_run([])     --> [].

lowers([C|Cs]) --> [C], { is_lower(C) }, !, lowers(Cs).
lowers([])     --> [].

others([C|Cs]) --> [C], { \+ is_alpha(C) }, !, others(Cs).
others([])     --> [].

peek_lower_next(In, In) :- In = [C|_], is_lower(C).

push_back(L, S0, S) :- append(L, S0, S).

is_upper(C) :- code_type(C, upper).
is_lower(C) :- code_type(C, lower).
is_alpha(C) :- code_type(C, alpha).

good_word(W) :-
    \+ sub_atom(W, _, _, _, '-'),
    atom_length(W, L),
    L >= 2.
