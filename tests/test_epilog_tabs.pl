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

:- module(test_epilog_tabs, [test_epilog_tabs/0]).
:- encoding(utf8).

/** <module> Tests for the tabs of an Epilog window

An Epilog window holds its terminals in tabs, and a tab holds one or more
of them side by side (see library(tab_frame)).  These test that the tabs
are made and named, that splitting stays inside a tab, and that closing a
terminal, a tab and the last tab do the right thing.

The frames are never opened: a terminal connects to a Prolog thread when
it is created, and none of this needs one.

Run with:

    swipl -g test_epilog_tabs -t halt \
          packages/xpce/tests/test_epilog_tabs.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(epilog)).
:- use_module(library(pce_util), [chain_list/2]).

test_epilog_tabs :-
    run_tests([ epilog_tabs,
                epilog_split,
                epilog_move,
                epilog_close
              ]).

                 /*******************************
                 *            HELPERS           *
                 *******************************/

%!  epilog(-Frame, -Window) is det.
%
%   An Epilog frame with one tab holding one terminal.

epilog(F, W) :-
    epilog_frame(@default, @default, @default, @off, @default, F),
    get(F, current_pane, W).

%!  tabs(+Frame, -Tabs) is det.
%
%   Label and terminal count of every tab, in order.

tabs(F, Tabs) :-
    get(F, tabs, TW),
    get(TW, tabs, Chain),
    chain_list(Chain, List),
    findall(Label-Count,
            ( member(Tab, List),
              get(Tab, label, Label),
              get(Tab?windows, size, Count)
            ),
            Tabs).

%!  terminals(+Frame, -Count) is det.

terminals(F, Count) :-
    get(F?panes, size, Count).

%!  at_edge(+Window, +Where, -Pos) is det.
%
%   A point just inside an edge of Window, in the coordinates its tab lays
%   its windows out in: what a drop there would be told.

at_edge(W, Where, point(X, Y)) :-
    (   get(W, decoration, D),
        D \== @nil
    ->  Decor = D
    ;   Decor = W
    ),
    get(Decor, area, area(AX, AY, AW, AH)),
    edge(Where, AX, AY, AW, AH, X, Y).

edge(left,  AX, AY, _AW, AH, X, Y) :- X is AX+5,    Y is AY+AH//2.
edge(right, AX, AY, AW,  AH, X, Y) :- X is AX+AW-5, Y is AY+AH//2.

%!  split_orientation(+Window, -Orientation) is semidet.
%
%   A terminal split off below its neighbour sits in a vertical tile, one
%   split off beside it in a horizontal one.

split_orientation(W, Orientation) :-
    get(W, tile, T),
    get(T, super, Super),
    Super \== @nil,
    get(Super, orientation, Orientation).


:- begin_tests(epilog_tabs).

test(a_window_starts_with_one_tab_named_after_its_profile) :-
    epilog(F, _W),
    tabs(F, ['Prolog'-1]).

%   A terminal is a subwindow of the tabbed window now, and a subwindow
%   is created with the window it is displayed on (see window ->create).
%   The dummy driver paints nothing, so no test here reaches that call:
%   check that the override takes the argument.

test(create_takes_the_parent_window) :-
    get(class(epilog_window), send_method, create, Method),
    get(Method?types, size, 1).

test(a_new_tab_gets_a_label_of_its_own) :-
    epilog(F, W),
    send(W, new_tab),
    tabs(F, ['Prolog'-1, 'Prolog 2'-1]).

test(a_new_tab_can_take_another_profile) :-
    epilog(F, _W),
    send(F, new_pane, shell),
    tabs(F, ['Prolog'-1, 'OS shell'-1]).

test(terminal_windows_counts_over_all_tabs) :-
    epilog(F, W),
    send(W, split, horizontally),
    send(W, new_tab),
    terminals(F, 3),
    tabs(F, ['Prolog'-2, 'Prolog 2'-1]).

:- end_tests(epilog_tabs).


:- begin_tests(epilog_split).

test(split_horizontally_adds_a_terminal_below) :-
    epilog(F, W),
    send(W, split, horizontally),
    tabs(F, ['Prolog'-2]),
    split_orientation(W, vertical).

test(split_vertically_adds_a_terminal_beside) :-
    epilog(F, W),
    send(W, split, vertically),
    tabs(F, ['Prolog'-2]),
    split_orientation(W, horizontal).

test(a_split_terminal_continues_the_one_it_came_from) :-
    epilog(F, W),
    send(W?terminal, profile, shell),
    send(W?terminal, goal, prolog),
    send(W, split, horizontally),
    get(F, current_pane, New),
    New \== W,
    get(New?terminal, profile, shell),
    get(New?terminal, goal, prolog).

test(splitting_stays_in_the_tab) :-
    epilog(F, W),
    send(W, split, horizontally),
    get(F, current_pane, New),
    send(New, split, vertically),
    tabs(F, ['Prolog'-3]).

:- end_tests(epilog_split).


:- begin_tests(epilog_move).

%   A terminal carries the grip of library(tab_frame), so it can be taken
%   to another tab or another Epilog window.  The terminal fills the
%   window, so the grip is drawn over it, clear of the scrollbar.

%   A client that asks for no title at all is asking for the name the tab
%   was given back, not for a blank tab.

test(an_empty_title_puts_the_tab_name_back) :-
    epilog(F, W),
    send(W, new_tab),
    get(F, current_pane, New),
    get(New, container, tab_frame, Tab),
    get(Tab, name, Name),
    send(Tab, window_label, 'claude'),
    get(Tab, label, claude),
    send(Tab, window_label, ''),
    get(Tab, label, Name),
    get(Tab, window_label, @nil).

test(a_terminal_tab_carries_a_close_button) :-
    epilog(F, W),
    send(W, new_tab),
    get(W, container, tab_frame, Tab),
    get(Tab, closable, @on),
    get(Tab, hypered, close_button, B),
    get(B, area, area(X, Y, BW, BH)),
    CX is X+BW//2,
    CY is Y+BH//2,
    get(B?device, window, Window),
    send(event(ms_left_down, Window, CX, CY), post, B),
    send(event(ms_left_up, Window, CX, CY), post, B),
    terminals(F, 1),                    % it closed the terminal with it
    tabs(F, ['Prolog 2'-1]).

test(the_tab_bar_offers_a_new_terminal) :-
    epilog(F, W),
    send(W, new_tab),
    get(F, tabs, TW),
    get(TW, new_tab_message, Message),
    Message \== @nil,
    send(Message, forward),
    terminals(F, 3).

test(a_terminal_tab_can_be_renamed) :-
    epilog(F, W),
    send(W, new_tab),                   % a lone tab shows no label
    get(W, container, tab_frame, Tab),
    get(Tab, editable_label, @on),
    send(Tab, edit_label),
    get(Tab, device, Stack),
    get(Stack, member, tab_label_item, Item),
    send(Item, selection, 'build log'),
    send(Item, execute),
    get(Tab, label, 'build log'),
    tabs(F, ['build log'-1, 'Prolog 2'-1]).

test(a_terminal_carries_a_grip) :-
    epilog(_F, W),
    get(W, member, split_handle, H),
    get(H, help_message, tag, Tag),
    get(Tag, size, Len),
    Len > 0.

test(a_terminal_moves_to_another_tab) :-
    epilog(F, W),
    send(W, new_tab),
    get(F, current_pane, Other),
    get(Other, container, tab_frame, Tab),
    at_edge(Other, right, Pos),
    send(Tab, drop, W, Pos),
    %  The tab it left was emptied, and the one it arrived in is named
    %  after it: a tab carries the name of the pane the user is working
    %  in, so that a tab holding two of them says which.
    tabs(F, ['Prolog'-2]),
    terminals(F, 2),
    get(W, tile_manager, Tab).

test(a_terminal_moves_to_another_window) :-
    epilog(F1, W1),
    epilog(_F2, W2),
    get(W1, container, tab_frame, Tab),
    at_edge(W1, right, Pos),
    send(Tab, drop, W2, Pos),
    tabs(F1, ['Prolog'-2]),
    terminals(F1, 2),
    get(W2, frame, F1).

test(the_terminals_of_a_moved_window_still_answer) :-
    epilog(F1, W1),
    epilog(_F2, W2),
    send(W2?terminal, profile, shell),
    get(W1, container, tab_frame, Tab),
    at_edge(W1, right, Pos),
    send(Tab, drop, W2, Pos),
    get(F1, panes, Chain),
    send(Chain, member, W2),
    get(W2?terminal, profile, shell).

:- end_tests(epilog_move).


:- begin_tests(epilog_close).

test(closing_one_terminal_leaves_the_others) :-
    epilog(F, W),
    send(W, split, horizontally),
    send(W?terminal, close),
    tabs(F, ['Prolog'-1]),
    terminals(F, 1).

test(closing_a_tab_closes_its_terminals) :-
    epilog(F, W),
    send(W, split, horizontally),
    send(W, new_tab),
    get(F, current_pane, Other),
    get(W, container, tab_frame, Tab),
    send(Tab, close),
    tabs(F, ['Prolog 2'-1]),
    get(F, current_pane, Other).

test(close_other_tabs_leaves_just_this_one) :-
    epilog(F, W),
    send(W, split, horizontally),
    send(W, new_tab),
    send(F, new_pane, shell),
    tabs(F, ['Prolog'-2, 'Prolog 2'-1, 'OS shell'-1]),
    get(W, container, tab_frame, Tab),
    send(Tab, close_other_tabs),
    tabs(F, ['Prolog'-2]).

test(closing_the_last_tab_ends_the_window) :-
    epilog(F, W),
    get(W, container, tab_frame, Tab),
    send(Tab, close),
    \+ object(F).

test(closing_the_last_terminal_ends_the_window) :-
    epilog(F, W),
    send(W?terminal, close),
    \+ object(F).

:- end_tests(epilog_close).
