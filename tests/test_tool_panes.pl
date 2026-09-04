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


:- module(test_tool_panes, [test_tool_panes/0]).
:- encoding(utf8).

/** <module> An IDE tool as a pane

The thread monitor used to be a window of its own: a browser, a graph
window, a menu bar and a reporter in a frame.  It is a pane now, so it
can sit in a tab of any window of the IDE beside a terminal, an editor
or another tool.

These check that it goes where it should, that there is only ever one,
that its two windows are tiled inside it and that its menu reaches the
bar of whatever window it lands in.

Run with:

    swipl -g test_tool_panes -t halt \
          packages/xpce/tests/test_tool_panes.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(swi_ide)).
:- use_module(library(epilog)).
:- use_module(library(swi/thread_monitor), []).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(lists), [member/2]).

test_tool_panes :-
    run_tests([ tool_panes ]).

%!  classes(+Frame, -Classes) is det.

classes(F, Classes) :-
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    findall(C, (member(P, Panes), get(P, class_name, C)), Classes).

%!  menus(+Frame, -Names) is det.

menus(F, Names) :-
    get(F, menu_bar, MB),
    get(MB, buttons, Chain),
    chain_list(Chain, Buttons),
    findall(N, (member(B, Buttons), get(B, name, N)), Names).

%!  monitor(-Monitor) is det.
%
%   The one thread monitor there is, made if there is none.

monitor(TM) :-
    get(@prolog_ide, show_tool, prolog_thread_monitor, @default, TM).

%!  no_monitor is det.
%
%   Take away the one there is.  There is only ever one, so a test that
%   wants to watch one being made has to start without it.

no_monitor :-
    (   get(@prolog_ide, tool, prolog_thread_monitor, TM)
    ->  send(TM, destroy)
    ;   true
    ).


:- begin_tests(tool_panes).

test(a_tool_opens_in_a_window_of_the_ide, Classes == [prolog_thread_monitor]) :-
    no_monitor,
    monitor(TM),
    get(TM, frame, F),
    send(F, instance_of, pane_frame),
    classes(F, Classes).

test(it_goes_into_a_window_that_is_already_open,
     Classes == [epilog_window, prolog_thread_monitor]) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, thread_monitor),
    classes(F, Classes).

test(there_is_only_ever_one, true(Again == TM)) :-
    monitor(TM),
    send(@prolog_ide, thread_monitor),
    get(@prolog_ide, tool, prolog_thread_monitor, Again).

%       A pane that shows more than one window is a tabbed_window holding
%       a single tab_frame, which lays them out with a tile.

test(its_two_windows_are_tiled_inside_it,
     Names == [thread_browser, thread_window]) :-
    monitor(TM),
    get(TM, members, Chain),
    chain_list(Chain, Windows),
    findall(N, (member(W, Windows), get(W, class_name, N)), Names).

test(its_menu_reaches_the_bar_of_the_window_it_is_in) :-
    monitor(TM),
    get(TM, frame, F),
    send(F, current_pane, TM),
    menus(F, Menus),
    memberchk(threads, Menus).

%       Which window a tool lands in is <-current_frame's to say, and
%       with no window manager to give one the focus that is whichever
%       was made first.  So ask the monitor which window it is in rather
%       than assuming.

test(and_goes_again_when_another_pane_has_the_focus) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, thread_monitor),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    get(TM, frame, MF),
    send(MF, current_pane, TM),
    menus(MF, WithMonitor),
    memberchk(threads, WithMonitor),
    get(MF, panes, Chain), chain_list(Chain, Panes),
    member(Other, Panes), Other \== TM, !,
    send(MF, current_pane, Other),
    menus(MF, WithOther),
    \+ memberchk(threads, WithOther).

test(the_window_is_named_after_it, true(Label == 'SWI-Prolog -- Threads')) :-
    monitor(TM),
    get(TM, frame, F),
    send(F, current_pane, TM),
    get(F, label, Label).

%       The browser drives the graph window through the pane they share,
%       not through <-frame: the frame is the window of the IDE now.

test(selecting_a_thread_draws_its_graph) :-
    monitor(TM),
    send(TM, selection, main),
    get(TM, graph_window, GW),
    get(GW, member, thread_diagram, _).

%       A tool is a pane, not only a tab: it can sit beside a terminal or
%       an editor, and be dragged to another window like any other pane.

test(it_can_be_asked_for_beside_what_is_already_there,
     Classes == [epilog_window, prolog_thread_monitor]) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_thread_monitor, split),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    get(TM, container, tab_frame, Tab),
    get(Tab, windows, Chain),
    chain_list(Chain, Panes),
    findall(C, (member(P, Panes), get(P, class_name, C)), Classes).

test(its_windows_follow_the_size_it_is_given, true(Fits == true)) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_thread_monitor, split),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    get(TM, area, area(_, _, _, PaneH)),
    get(TM, window, thread_browser, TB),
    get(TB, area, area(_, _, _, BrowserH)),
    (   BrowserH =:= PaneH ->  Fits = true ;  Fits = false ).

test(it_carries_a_grip_to_drag_it_by) :-
    monitor(TM),
    get(TM, grip, _).

%       A window has a surface of its own, so a grip displayed on the
%       pane behind them would be covered.  It goes on the window that is
%       in the corner, and still moves the whole tool.

test(the_grip_is_on_one_of_its_windows, true(OnAWindow == true)) :-
    monitor(TM),
    get(TM, grip, Handle),
    get(Handle, device, D),
    get(TM, members, Chain),
    chain_list(Chain, Windows),
    (   memberchk_eq(D, Windows) ->  OnAWindow = true ;  OnAWindow = false ).

test(but_it_moves_the_whole_tool, true(Moves == TM)) :-
    monitor(TM),
    get(TM, grip, Handle),
    get(Handle, pane, Moves).

test(and_sits_in_the_top_right_of_the_pane, true(NearRight == true)) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_thread_monitor, split),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    get(TM, size, size(PaneW, _)),
    get(TM, grip, Handle),
    get(Handle, device, D),
    (   get(D, decoration, Dec), Dec \== @nil ->  Placed = Dec ;  Placed = D ),
    get(Placed, area, area(DX, _, _, _)),
    get(Handle, area, area(HX, HY, HW, _)),
    Right is DX+HX+HW,
    %  within a scrollbar's width of the right edge, and at the top
    (   PaneW-Right < 24, HY < 8 ->  NearRight = true ;  NearRight = false ).

%!  memberchk_eq(+X, +List) is semidet.

memberchk_eq(X, [Y|T]) :-
    (   X == Y ->  true ;  memberchk_eq(X, T) ).

test(dragging_it_to_another_window_takes_it_there,
     true(Where == [[epilog_window], [epilog_window, prolog_thread_monitor]])) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F1),
    send(F1, open),
    send(@prolog_ide, show_tool, prolog_thread_monitor, split),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    get(TM, frame, From),
    epilog_frame(@default, @default, @default, @off, @default, F2),
    send(F2, open),
    get(F2, current_pane, Target),
    get(Target, container, tab_frame, Tab2),
    get(Target, area, area(AX, AY, AW, AH)),
    X is AX+AW-5, Y is AY+AH//2,
    send(Tab2, drop, TM, point(X, Y)),
    classes(From, Left),
    classes(F2, Arrived),
    Where = [Left, Arrived].

test(and_the_window_it_left_stops_saying_its_name,
     Label == 'SWI-Prolog -- Prolog') :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F1),
    send(F1, open),
    send(@prolog_ide, show_tool, prolog_thread_monitor, split),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    get(TM, frame, From),
    epilog_frame(@default, @default, @default, @off, @default, F2),
    send(F2, open),
    get(F2, current_pane, Target),
    get(Target, container, tab_frame, Tab2),
    get(Target, area, area(AX, AY, AW, AH)),
    X is AX+AW-5, Y is AY+AH//2,
    send(Tab2, drop, TM, point(X, Y)),
    get(From, label, Label).

:- end_tests(tool_panes).
