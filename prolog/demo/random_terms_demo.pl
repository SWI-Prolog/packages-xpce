/*  Part of SWI-Prolog

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

:- module(random_terms_demo,
          [ random_terms_demo/0
          ]).
:- use_module(library(random_terms)).
:- use_module(library(xdot), []).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- use_module(library(graphviz_term)).
:- use_module(library(debug)).

/** <module> Demo generating and visualizing random Prolog terms

This module defines the class `random_terms`,   which provides a gui for
random_term/2  from  library(random_terms)  and  uses  library(xdot)  to
render the generated terms using Graphviz.
*/

random_terms_demo :-
    send(new(random_terms), open).

%!  widget_spec(?Key, ?Widget, ?Default) is nondet.
%
%   Declares one control per random_term/2 option.
%
%     * `slider(Lo, Hi)` — numeric slider (int if Lo/Hi are ints,
%       float if either is a float).
%     * `list_item`       — text_item holding a Prolog list literal.

widget_spec(depth,        slider(0,   20),   5).
widget_spec(max_arity,    slider(0,   8),    3).
widget_spec(max_int,      slider(0,   1000), 100).
widget_spec(depth_decay,  slider(0.0, 1.0),  0.5).
widget_spec(p_zero_arity, slider(0.0, 1.0),  0.10).
widget_spec(functors,     list_item,         [f,g,h,p,q]).
widget_spec(atoms,        list_item,         [a,b,c]).
widget_spec(w_leaf,       slider(0, 100),    10).
widget_spec(w_share,      slider(0, 100),    30).
widget_spec(w_copy,       slider(0, 100),    15).
widget_spec(w_cycle,      slider(0, 100),    15).
widget_spec(w_compound,   slider(0, 100),    50).
widget_spec(w_var,        slider(0,   100),  10).
widget_spec(w_atom,       slider(0, 100),    10).
widget_spec(w_int,        slider(0, 100),    10).
widget_spec(w_float,      slider(0, 100),    10).
widget_spec(w_rational,   slider(0, 100),    10).
widget_spec(w_string,     slider(0, 100),    10).

slider_order([ depth, depth_decay,
               w_leaf, w_compound,
               w_share, w_copy, w_cycle,
               p_zero_arity, max_arity, max_int, w_var,
               w_atom, w_int, w_float, w_rational, w_string
             ]).

:- pce_begin_class(random_terms, frame,
                   "Show random terms").

initialise(F) :->
    send_super(F, initialise, "Random term demo"),
    send(F, append, new(D, dialog)),
    send(F, fill_dialog),
    send(new(XDot, xdot_window), below, D),
    send(XDot, natural_zoom, 1.5),
    send(new(report_dialog), below, XDot).

fill_dialog(F) :->
    get(F, member, dialog, D),
    slider_order(Sliders),
    place_grid(D, Sliders, 3, 0),
    place_item(D, functors, next_row),
    place_item(D, atoms, right),
    send(D, append, button(generate, message(F, generate)), next_row).

%!  place_grid(+D, +Keys, +Cols, +Index) is det.
%
%   Append widgets to D in a grid Cols columns wide, using
%   `next_row` for row breaks and `right` between columns.

place_grid(_, [], _, _).
place_grid(D, [K|Ks], Cols, I) :-
    (   I =:= 0
    ->  place_item(D, K, first)
    ;   I mod Cols =:= 0
    ->  place_item(D, K, next_row)
    ;   place_item(D, K, right)
    ),
    I1 is I + 1,
    place_grid(D, Ks, Cols, I1).

place_item(D, Key, Where) :-
    widget_spec(Key, Widget, Default),
    make_widget(Widget, Key, Default, W),
    (   Where == first
    ->  send(D, append, W)
    ;   send(D, append, W, Where)
    ).

make_widget(slider(Lo, Hi), Key, Default, S) :-
    slider_args(Lo, Hi, Default, LoA, HiA, DefA),
    new(S, slider(Key, LoA, HiA, DefA)),
    send(S, show_value, @on),
    (   LoA = real(_)
    ->  send(S, format, '%.1f')
    ;   true
    ).
make_widget(list_item, Key, Default, TI) :-
    format(string(Text), '~q', [Default]),
    new(TI, text_item(Key, Text)),
    send(TI, length, 30).

%   xpce demotes whole-valued Prolog floats (e.g. 1.0) to PCE ints,
%   which turns a slider with 0.0..100.0 bounds into an int slider.
%   Wrap float bounds/default in real/1 to force float behaviour.

slider_args(Lo, Hi, Def, real(Lo), real(Hi), real(Def)) :-
    (   float(Lo)
    ;   float(Hi)
    ), !.
slider_args(Lo, Hi, Def, Lo, Hi, Def).

xdot(F, XDot:xdot) :<-
    "Get the xdot figure"::
    get(F, member, xdot_window, W),
    get(W, xdot, XDot).

generate(F) :->
    get(F, member, dialog, D),
    collect_options(D, Options),
    debug(random_term, 'Options: ~p', [Options]),
    random_term(Term, Options),
    with_output_to(
        string(DOT),
        term_to_dot(current_output, Term)),
    send(F?xdot, load, DOT),
    get(F, member, xdot_window, W),
    send(W, fit).

collect_options(D, Options) :-
    findall(Key=Value,
            (   widget_spec(Key, Widget, _),
                get(D, member, Key, Item),
                get(Item, selection, Sel),
                widget_value(Widget, Sel, Value)
            ),
            Options).

widget_value(slider(_, _), Sel, Sel).
widget_value(list_item, Sel, V) :-
    item_string(Sel, Str),
    term_string(V, Str),
    is_list(V).

item_string(Sel, Str) :-
    (   string(Sel) -> Str = Sel
    ;   atom(Sel)   -> atom_string(Sel, Str)
    ;   format(string(Str), '~w', [Sel])
    ).

:- pce_end_class.


loop :-
    random_term(Term, []),
    recorda(random_term, Term),
    with_output_to(string(S),
                   term_to_dot(current_output, Term)),
    assertion(\+ sub_atom(S, _, _, _, '$SKEL')),
    put_char(user_error, '.'),
    loop.

term_to_dot_file(Term, File) :-
    setup_call_cleanup(
        tmp_file_stream(utf8, File, Out),
        term_to_dot(Out, Term),
        close(Out)).
