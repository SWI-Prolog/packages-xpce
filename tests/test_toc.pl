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

:- module(test_toc, [test_toc/0]).

/** <module> Tests for library(pce_toc)

Run with:

    swipl -g test_toc -t halt packages/xpce/tests/test_toc.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_toc)).

test_toc :-
    run_tests([ toc ]).

%   toc_window(-Window, -Box)
%
%   Window has an expanded root with one son whose identifier is Box,
%   a graphical displayed elsewhere.  The visual hierarchy tool uses
%   graphicals as identifiers this way.

toc_window(W, B) :-
    new(W, toc_window),
    new(B, box(10, 10)),
    new(P, picture),
    send(P, display, B),
    send(W, root, toc_folder(root, root)),
    send(W, son, root, toc_file(box, B)),
    get(W, node, root, Root),
    send(Root, collapsed, @off).

:- begin_tests(toc).

test(select_graphical_id, [NodeSel-BoxSel == @on-(@off)]) :-
    toc_window(W, B),
    send(W, selection, B),
    get(W, node, B, Node),
    get(Node, image, Image),
    get(Image, selected, NodeSel),
    get(B, selected, BoxSel),
    get(B, device, P),
    send(P, free),
    send(W, free).

:- end_tests(toc).
