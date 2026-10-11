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


:- module(test_doc_render,
          [ test_doc_render/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(doc/load)).
:- use_module(doc(html)).
:- use_module(doc(vfont)).

/** <module> Test the HTML renderer of library(doc/load)
*/

test_doc_render :-
    run_tests([ doc_render
              ]).

%   boxes(+HTML, -Boxes)
%
%   Boxes describes the content of a pbox that shows HTML: text(Text)
%   for a word and space for a space.  Other boxes, such as the start
%   and end of a link, are left out.

boxes(HTML, Boxes) :-
    new(PB, pbox(400, left)),
    send(PB, show, HTML),
    get(PB, content, Content),
    get(Content, size, N),
    findall(Box, ( between(1, N, I),
                   get(Content, element, I, B),
                   box(B, Box)
                 ), Boxes),
    send(PB, destroy).

box(B, text(Text)) :-
    send(B, instance_of, tbox),
    !,
    get(B, text, Text).
box(B, space) :-
    send(B, instance_of, hbox),
    get(B, width, W),
    W > 0.

font_props(VF, Family-Style-Weight-Points) :-
    get(VF, font, Font),
    get(Font, family, Family),
    get(Font, style, Style),
    get(Font, weight, Weight),
    get(Font, points, Points).

:- begin_tests(doc_render).

test(space_after_group,
     true(append(_, [text(a), space, text(=), space, text(b)|_], Boxes))) :-
    %  A paragraph that starts with an inline element keeps the blank
    %  after it
    boxes([element(p, [], [element(code, [], [a]), ' = b'])], Boxes).
test(space_after_link,
     true(append(_, [text(a), space, text(b)|_], Boxes))) :-
    boxes([element(p, [], [element(a, [href=x], [a]), ' b'])], Boxes).
test(leading_space, true(\+ append(_, [space, text(a)|_], Boxes))) :-
    boxes([element(p, [], [' a b'])], Boxes).
test(vfont_fixed, [Family-Points == mono-NormalPoints]) :-
    %  Fixed-width text has the size of the text around it
    get(@pce, convert, normal, font, Normal),
    get(Normal, points, NormalPoints),
    new(VF, vfont),
    send(VF, fixed, @on),
    font_props(VF, Family-_-_-Points).
test(vfont_style, [Props == sans-italic-bold-Points]) :-
    get(@pce, convert, normal, font, Normal),
    get(Normal, points, NormalPoints),
    Points is round(NormalPoints*1.5),
    new(VF, vfont),
    send(VF, slant, i),
    send(VF, weight, bold),
    send(VF, font_size, 3),
    font_props(VF, Props).

:- end_tests(doc_render).
