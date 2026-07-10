/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2014-2026, SWI-Prolog Solutions b.v.
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

:- module(graphviz_term,
          [ term_to_dot/2               % +Out, +Term
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(gensym)).

/** <module> View complex Prolog terms using Graphviz

This library translates  complex  Prolog   terms  into  Graphviz (`dot`)
output for graphical rendering.

__History__: This module was initially named `gvterm.pl` and part of the
_pack_ `gvterm`, using Python `xdot` to show the result.

@see library(xdot) provides XPCE classes and   utilities  to display the
generated dot.
*/

%!  term_to_dot(+Out:stream, @Term) is det.
%
%   Emit a dot representation for Term  to   the  stream Out. The stream
%   should allow for Unicode encoding of   Term holds Unicode characters
%   in atoms, strings or functor names.

:- det(term_to_dot/2).
term_to_dot(Out, Term) :-
    \+ \+ term_to_dot_(Out, Term).

term_to_dot_(Out, Term) :-
    numbervars(Term, 0, _, [singletons(true), attvar(skip)]),
    '$factorize_term'(Term, Skel, Subst),
    label_factors(Subst),
    phrase(struct0(Skel), Codes),
    format(Out,
           'digraph structs {\n  \c
              node [shape=plaintext fontname="Helvetica" fontsize=11];\n  \c
              edge [color="#546E7A" arrowsize=0.7];\n\c
              ~s}\n',
           [Codes]).

label_factors([]) =>
    true.
label_factors([V='$VAR'(X)|T]) =>
    V = '$VAR'(X),
    label_factors(T).
label_factors([V=C|T]), var(V) =>
    V = '$SKEL'(_Done,C),
    label_factors(T).

%!  struct0(+Term)//
%
%   Deal with the outer term.  Labels are emitted as Graphviz HTML
%   labels (`label=< ... >`); each argument becomes its own coloured
%   cell.

struct0(Prim) -->
    { primitive(Prim),
      !,
      prim_type(Prim, Type),
      type_color(Type, Color),
      primitive_codes(Prim, Codes)
    },
    "  n0 [label=<", table_open, "<TR>",
    plain_cell(Color, Codes),
    "</TR>", table_close, ">];\n".
struct0(Term) -->
    struct(Term, -(_), Links, []),
    links(Links).

%!  struct(+Term, -Link, -Links, ?RestLinks)//
%
%   Deal with compound and inner terms.  At a top position (`-(Id)`)
%   the term becomes its own `structN` node; at a child position
%   (`Id-N`) it becomes one `<TD PORT="aN">` cell inside the parent's
%   HTML table.

struct('$SKEL'(Done, C), -(Id), Links, LinksT) -->
    { var(Done),
      !,
      Done = top(Id)
    },
    struct(C, -(Id), Links, LinksT).
struct('$SKEL'(Done, C), Id-N, [link_c(Id-N, Id2, C)|LinkT], LinkT) -->
    { var(Done),
      !,
      Done = id(Id2)
    },
    link_cell(N).
struct('$SKEL'(top(TopId), _), Id-N,
       [link(Id-N, TopId)|LinksT], LinksT) -->
    !,
    link_cell(N).
struct('$SKEL'(id(Id2), _), Id-N, [link(Id-N, Id2)|LinkT], LinkT) -->
    !,
    link_cell(N).
struct(Prim, _Id-N, Links, Links) -->
    { primitive(Prim),
      !,
      prim_type(Prim, Type),
      type_color(Type, Color),
      primitive_codes(Prim, Codes)
    },
    port_cell(N, Color, Codes).
struct(Compound, -(Id), Links, LinkT) -->
    !,
    { compound_name_arguments(Compound, F, Args),
      gensym(struct, Id),
      functor_codes(F, Args, FCodes)
    },
    "  ", atom(Id), " [label=<", table_open, "<TR>",
    functor_cell(FCodes),
    gv_args(Args, 0, Id, Links, LinkT),
    "</TR>", table_close, ">];\n".
struct(Compound, Id-N, [link_c(Id-N, _, Compound)|LinkT], LinkT) -->
    link_cell(N).

gv_args([], _, _, Links, Links) --> [].
gv_args([H|T], N, Id, Links, LinksT) -->
    struct(H, Id-N, Links, LT0),
    { N2 is N + 1 },
    gv_args(T, N2, Id, LT0, LinksT).

links(Links) -->
    { \+ memberchk(link_c(_,_,_), Links)
    },
    !,
    "\n",
    link_f(Links).
links(Links) -->
    link_c(Links, RestLinks, []),
    links(RestLinks).

link_c([], Links, Links) --> [].
link_c([link_c(Id-Arg, Id2, Compound)|T0],
       [link(Id-Arg, Id2)|LinksT0], LinkT) -->
    !,
    struct(Compound, -(Id2), LinksT0, LinkT1),
    link_c(T0, LinkT1, LinkT).
link_c([H|T0], [H|T], Links) -->
    link_c(T0, T, Links).

link_f([]) --> [].
link_f([link(Id-Arg, Id2)|T]) -->
    "  ", atom(Id), ":a", integer(Arg), " -> ", atom(Id2), ":f;\n",
    link_f(T).


primitive('$VAR'(_)) =>
    true.
primitive(X) =>
    \+ compound(X).

primitive_codes(Prim, Codes) :-
    format(codes(Codes), '~W',
           [ Prim,
             [ numbervars(true),
               quoted(true),
               max_text(20)
             ]
           ]).

functor_codes(F, [], Codes) :-
    !,
    format(codes(Codes), '~q()', [F]).
functor_codes(F, _, Codes) :-
    format(codes(Codes), '~q', [F]).

%!  prim_type(+Prim, -Type) is det.
%
%   Classify a primitive so we can pick the right theme colour.
%   Order matters: `integer` before `rational` (SWI integers satisfy
%   `rational/1`) and every clause matches at most once.

prim_type('$VAR'(_), var)  :- !.
prim_type(X, integer)      :- integer(X), !.
prim_type(X, rational)     :- rational(X), !.
prim_type(X, float)        :- float(X), !.
prim_type(X, string)       :- string(X), !.
prim_type(X, atom)         :- atom(X), !.
prim_type(_, atom).

%!  type_color(?Type, ?Color) is nondet.
%
%   Soft-pastel palette used for the cell backgrounds.

type_color(functor,  '#B0C4DE').        % steel blue
type_color(integer,  '#FFF9C4').        % pale yellow
type_color(float,    '#DCEDC8').        % pale green
type_color(rational, '#FFCCBC').        % peach
type_color(atom,     '#F5F5DC').        % beige
type_color(string,   '#E1BEE7').        % lavender
type_color(var,      '#ECEFF1').        % pale gray
type_color(link,     '#FFCDD2').        % pale pink (shared / cycle back-ref)

%!  table_open//
%!  table_close//
%!  functor_cell(+FCodes)//
%!  port_cell(+N, +Color, +Codes)//
%!  plain_cell(+Color, +Codes)//
%!  link_cell(+N)//
%
%   Building blocks for the Graphviz HTML label of a single node.

table_open -->
    "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">".
table_close -->
    "</TABLE>".

functor_cell(FCodes) -->
    { type_color(functor, Color) },
    "<TD PORT=\"f\" BGCOLOR=\"", atom(Color), "\"><B>",
    hstring(FCodes),
    "</B></TD>".

port_cell(N, Color, Codes) -->
    "<TD PORT=\"a", integer(N), "\" BGCOLOR=\"", atom(Color), "\">",
    hstring(Codes),
    "</TD>".

plain_cell(Color, Codes) -->
    "<TD BGCOLOR=\"", atom(Color), "\">",
    hstring(Codes),
    "</TD>".

link_cell(N) -->
    { type_color(link, Color) },
    "<TD PORT=\"a", integer(N), "\" BGCOLOR=\"", atom(Color), "\">.</TD>".

%!  hstring(+Codes)//
%
%   Emit Codes escaped for a Graphviz HTML label.

hstring([]) --> [].
hstring([H|T]) -->
    (   hchar(H)
    ->  []
    ;   [H]
    ),
    hstring(T).

hchar(0'<)  --> "&lt;".
hchar(0'>)  --> "&gt;".
hchar(0'&)  --> "&amp;".
hchar(0'")  --> "&quot;".
hchar(0'\n) --> "<BR/>".
