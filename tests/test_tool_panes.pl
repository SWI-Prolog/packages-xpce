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

The thread monitor and the source navigator used to be windows of their
own, each holding its windows, a menu bar and a reporter in a frame.
They are panes now, so they can sit in a tab of any window of the IDE
beside a terminal, an editor or another tool.

These check that a tool goes where it should, that there is only ever
one, that its windows are tiled inside it and that what it has to say
reaches the bar of whatever window it lands in.

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
:- use_module(library(trace/browse), []).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(lists), [member/2]).

test_tool_panes :-
    run_tests([ tool_panes,
                navigator_pane
              ]).

%!  classes(+Frame, -Classes) is det.

classes(F, Classes) :-
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    findall(C, (member(P, Panes), get(P, class_name, C)), Classes).

%!  no_frames is det.
%
%   Take away every window of the IDE, so that a test that watches a
%   tool being placed knows which window it is placed in.

no_frames :-
    get(@prolog_ide, members, Members),
    chain_list(Members, List),
    forall(( member(F, List),
             send(F, instance_of, pane_frame)
           ),
           send(F, destroy)).

%!  side_of(+Graphical, +Relative, -Side) is det.
%
%   Which side of Relative Graphical landed on.  The tile places a
%   window's decoration, not the window, so that is what says where it
%   went.  See `placed_area/2' in library(pane_frame).

side_of(Gr, Relative, Side) :-
    placed_position(Gr, X, Y),
    placed_position(Relative, RX, RY),
    (   Y > RY
    ->  Side = below
    ;   Y < RY
    ->  Side = above
    ;   X > RX
    ->  Side = right
    ;   X < RX
    ->  Side = left
    ;   Side = nowhere
    ).

placed_position(W, X, Y) :-
    (   get(W, decoration, Decor),
        Decor \== @nil
    ->  Placed = Decor
    ;   Placed = W
    ),
    get(Placed, area, area(X, Y, _, _)).

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

%       Where a tool goes is one setting on the application, so it can be
%       said once in a Defaults file and hold for every tool.  Which
%       window it lands in is <-current_frame's to say and depends on
%       which one was last worked in, so these assert the shape of the
%       window the tool ends up in rather than which window that is.

%!  with_placement(+Where, :Goal) is det.

:- meta_predicate with_placement(+, 0).

with_placement(Where, Goal) :-
    get(@pce, convert, prolog_ide, class, Class),
    get(Class, class_variable, tool_placement, Var),
    get(Var, value, Old),
    setup_call_cleanup(
        send(Class, class_variable_value, tool_placement, Where),
        Goal,
        send(Class, class_variable_value, tool_placement, Old)).

%!  monitor_shape(-Tabs, -PanesInItsTab) is det.
%
%   A window of terminals is open, then the monitor is asked for.

monitor_shape(Tabs, InItsTab) :-
    no_monitor,

    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, thread_monitor),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    get(TM?frame?tabs?tabs, size, Tabs),
    get(TM, container, tab_frame, Tab),
    get(Tab?windows, size, InItsTab).

test(a_tool_takes_a_tab_by_default, true(InItsTab == 1)) :-
    with_placement(tab, monitor_shape(Tabs, InItsTab)),
    Tabs > 1.                           % beside the terminal's tab

test(it_can_be_asked_to_sit_beside_what_is_there, true(InItsTab == 2)) :-
    with_placement(split, monitor_shape(_Tabs, InItsTab)).

test(or_to_open_a_window_of_its_own, true(Shape == [1, 1])) :-
    with_placement(frame, monitor_shape(Tabs, InItsTab)),
    Shape = [Tabs, InItsTab].           % one tab, one pane: only the tool

test(and_the_caller_can_overrule_the_setting, true(InItsTab == 2)) :-
    with_placement(tab,
                   ( no_monitor,
                     epilog_frame(@default, @default, @default, @off,
                                  @default, F),
                     send(F, open),
                     send(@prolog_ide, show_tool, prolog_thread_monitor, split),
                     get(@prolog_ide, tool, prolog_thread_monitor, TM),
                     get(TM, container, tab_frame, Tab),
                     get(Tab?windows, size, InItsTab)
                   )).

%       Which side of what is there it lands on is the tool's own say, so
%       a navigator can go down the left and a monitor along the bottom.

%!  with_pane_side(+Side, :Goal) is det.

:- meta_predicate with_pane_side(+, 0).

with_pane_side(Side, Goal) :-
    get(@pce, convert, prolog_thread_monitor, class, Class),
    get(Class, class_variable, pane_side, Var),
    get(Var, value, Old),
    setup_call_cleanup(
        send(Class, class_variable_value, pane_side, Side),
        Goal,
        send(Class, class_variable_value, pane_side, Old)).

%!  monitor_side(+Side, -Landed) is det.
%
%   Ask for the monitor beside a terminal, with the tool saying it wants
%   Side, and report where it actually landed.

monitor_side(Side, Landed) :-
    with_pane_side(
        Side,
        with_placement(
            split,
            ( no_frames,                % whichever window is there is the
              no_monitor,               % one the tool is put in
              epilog_frame(@default, @default, @default, @off, @default, F),
              send(F, open),
              send(@prolog_ide, thread_monitor),
              get(@prolog_ide, tool, prolog_thread_monitor, TM),
              get(TM, container, tab_frame, Tab),
              get(Tab, windows, Chain),
              chain_list(Chain, Windows),
              member(Other, Windows),
              Other \== TM,
              !,
              side_of(TM, Other, Landed)
            ))).

test(a_tool_says_which_side_it_goes_on, true(Landed == left)) :-
    monitor_side(left, Landed).

test(and_is_believed_whichever_side_that_is, true(Landed == above)) :-
    monitor_side(above, Landed).

test(the_monitor_asks_for_the_bottom, true(Side == below)) :-
    no_monitor,
    new(TM, prolog_thread_monitor),
    get(TM, pane_side, Side),
    send(TM, destroy).

%       And it is on the Settings menu, so it can be changed without
%       editing a Defaults file.

test(the_setting_is_on_the_settings_menu,
     Items == [frame, tab, split]) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    get(F, menu_bar, MB),
    get(MB, member, settings, Settings),
    get(Settings, member, new_tools_open, Item),
    get(Item, popup, Popup),
    get(Popup, members, Chain),
    chain_list(Chain, Members),
    findall(V, (member(MI, Members), get(MI, value, V)), Items).

test(it_shows_which_one_is_in_force, true(Ticked == [split])) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    get(F, menu_bar, MB),
    get(MB, member, settings, Settings),
    get(Settings, member, new_tools_open, Item),
    get(Item, popup, Popup),
    with_placement(split,
                   ( send(Popup, update, F),
                     get(Popup, members, Chain),
                     chain_list(Chain, Members),
                     findall(V,
                             ( member(MI, Members),
                               get(MI, selected, @on),
                               get(MI, value, V)
                             ),
                             Ticked)
                   )).

:- end_tests(tool_panes).


                 /*******************************
                 *          THE NAVIGATOR       *
                 *******************************/

/* The source navigator is the second tool to become a pane.  It carries
three windows rather than two -- a tool bar, the filter and the tree --
and it asks for the left rather than the bottom, which is what a
navigator is for.
*/

%!  navigator(-Navigator) is det.
%
%   The one navigator there is, made if there is none.

navigator(SB) :-
    get(@prolog_ide, show_tool, prolog_navigator, @default, SB).

no_navigator :-
    (   get(@prolog_ide, tool, prolog_navigator, SB)
    ->  send(SB, destroy)
    ;   true
    ).

:- begin_tests(navigator_pane).

test(it_opens_in_a_window_of_the_ide, Classes == [prolog_navigator]) :-
    no_frames,
    no_navigator,
    navigator(SB),
    get(SB, frame, F),
    send(F, instance_of, pane_frame),
    classes(F, Classes).

test(there_is_only_ever_one, true(Again == SB)) :-
    navigator(SB),
    send(@prolog_ide, open_navigator),
    get(@prolog_ide, tool, prolog_navigator, Again).

test(its_three_windows_are_tiled_inside_it,
     Names == [tool_dialog, sb_filter_dialog, prolog_source_structure]) :-
    navigator(SB),
    get(SB, members, Chain),
    chain_list(Chain, Windows),
    findall(N, (member(W, Windows), get(W, class_name, N)), Names).

test(the_tab_is_named_after_it, true(Label == 'SWI-Prolog -- Navigator')) :-
    navigator(SB),
    get(SB, frame, F),
    send(F, current_pane, SB),
    get(F, label, Label).

test(it_asks_for_the_left, true(Side == left)) :-
    no_navigator,
    new(SB, prolog_navigator),
    get(SB, pane_side, Side),
    send(SB, destroy).

%       The tool bar acts on the tree and the filter dialog reads it, and
%       both used to find it through <-frame.  The frame is a window of
%       the IDE now, so they go through the pane they share.

test(the_tool_bar_is_found_through_the_pane) :-
    navigator(SB),
    get(SB, tool_bar, TB),
    get(TB, client, Client),
    get(SB, tree, Client).

test(and_so_is_the_tree_the_filter_controls, true(Tree == Mine)) :-
    navigator(SB),
    get(SB, window, sb_filter_dialog, FD),
    get(FD, tree, Tree),
    get(SB, tree, Mine).

test(so_the_filter_can_still_say_what_to_show, true(Content == all)) :-
    navigator(SB),
    get(SB, window, sb_filter_dialog, FD),
    send(FD, content, all),
    get(SB?tree, content, Content).

%       A pane has no reporter of its own: it reports on the bar of the
%       window it is in, which grows one the first time anything asks.

test(what_it_has_to_say_grows_a_status_bar, true(Class == pane_status_dialog)) :-
    no_navigator,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_navigator, split),
    get(@prolog_ide, tool, prolog_navigator, SB),
    \+ get(SB?frame, status_dialog, _),
    send(SB, report, status, 'scanning'),
    get(SB?frame, status_dialog, SD),
    get(SD, class_name, Class).

test(and_it_lands_on_the_left_of_what_was_there, true(Landed == left)) :-
    no_frames,
    no_navigator,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_navigator, split),
    get(@prolog_ide, tool, prolog_navigator, SB),
    get(SB, container, tab_frame, Tab),
    get(Tab, windows, Chain),
    chain_list(Chain, Windows),
    member(Other, Windows), Other \== SB, !,
    side_of(SB, Other, Landed).

:- end_tests(navigator_pane).
