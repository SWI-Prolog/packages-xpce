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


:- module(test_focus_border, [test_focus_border/0]).

/** <module> Test ->focus_border of editor and list_browser

An editor or list_browser has the look of a text entry field (rounded,
an accent border when it has the focus), unless it fills a window:
view and browser switch ->focus_border off.

Run with:

    swipl -g test_focus_border -t halt packages/xpce/tests/test_focus_border.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_focus_border :-
    run_tests([ focus_border
              ]).

:- begin_tests(focus_border).

test(editor, FB == @on) :-
    new(E, editor),
    get(E, focus_border, FB),
    free(E).
test(list_browser, FB == @on) :-
    new(LB, list_browser),
    get(LB, focus_border, FB),
    free(LB).
test(view, FB == @off) :-
    new(V, view),
    get(V?editor, focus_border, FB),
    free(V).
test(browser, FB == @off) :-
    new(B, browser),
    get(B?list_browser, focus_border, FB),
    free(B).
test(view_editor_replaced, FB == @off) :-
    new(V, view),
    send(V, editor, new(E, editor)),
    get(E, focus_border, FB),
    free(V).

:- end_tests(focus_border).
