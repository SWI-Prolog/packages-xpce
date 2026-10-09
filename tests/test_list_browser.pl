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


:- module(test_list_browser, [test_list_browser/0]).

/** <module> Test class list_browser

Run with:

    swipl -g test_list_browser -t halt packages/xpce/tests/test_list_browser.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_list_browser :-
    run_tests([ list_browser_scroll
              ]).

%   list(+Items, -ListBrowser)
%
%   An open browser of 5 lines holding the numbers 1..Items.

list(Items, LB) :-
    new(B, browser(@default, size(20, 5))),
    forall(between(1, Items, I), send(B, append, I)),
    send(B, open),
    get(B, list_browser, LB).

done(LB) :-
    get(LB, window, B),
    send(B, destroy).

%   scrolled(+LB, +How, -Start)
%
%   Start is the first item shown after scrolling from the top as How
%   says.

scrolled(LB, How, Start) :-
    send(LB, scroll_to, 0),
    How =.. [Sel|Args],
    Msg =.. [send, LB, Sel|Args],
    call(Msg),
    get(LB, start, Start).

:- begin_tests(list_browser_scroll).

%   Scrolling stops when the last item is on the bottom line.

test(scrolling_stops_with_the_last_item_at_the_bottom, Starts == [45,45,45,45,45]) :-
    list(50, LB),
    findall(S, ( member(How, [ scroll_vertical(forwards, page, 10000),
                               scroll_vertical(forwards, line, 1000),
                               scroll_vertical(goto, file, 1000),
                               scroll_to(@default),
                               scroll_to(49)
                             ]),
                 scrolled(LB, How, S)
               ), Starts),
    done(LB).
test(a_short_list_does_not_scroll, Start == 0) :-
    list(3, LB),
    scrolled(LB, scroll_vertical(forwards, page, 1000), Start),
    done(LB).
test(scrolling_back_is_free, Start == 10) :-
    list(50, LB),
    send(LB, scroll_to, 10),
    get(LB, start, Start),
    done(LB).

:- end_tests(list_browser_scroll).
