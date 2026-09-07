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

The thread monitor, the source navigator, the debug monitor, the debugger
status, the cross-referencer, the profiler, the exception editor, the
source-level debugger and the tools of the XPCE manual used to be windows
of their own, each holding its windows, a menu bar and a reporter in a
frame.  They are panes now, so they can sit in a tab of any window of the
IDE beside a terminal, an editor or another tool.

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
:- use_module(library(swi/pce_debug_monitor), []).
:- use_module(library(trace/status), []).
:- use_module(library(pce_xref), []).
:- use_module(library(pce_manual), []).
:- use_module(library(pce_html_manual), []).  % defines swi_man_xpce
:- use_module(library(swi/pce_profile), []).
:- use_module(library(trace/exceptions), []).
:- use_module(library(trace/viewterm), [view_term/2]).
:- use_module(library(trace/trace), []).   % loads trace/gui, which does
                                          % not survive a use_module of
                                          % its own
:- use_module(library(apply), [maplist/3]).
:- use_module(library(prolog_debug), [spy/1, nospy/1]).
:- use_module(library(debug), [debug/1, debug/3, nodebug/1]).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(pane_frame), [pane_kind/2]).

%       An arrangements file of their own: what these check is where a
%       pane goes with the arrangements the system comes with, which the
%       arrangements of whoever runs them must not colour.

:- multifile pane_layouts:arrangements_file/1.

pane_layouts:arrangements_file(File) :-
    current_prolog_flag(tmp_dir, Tmp),
    atom_concat(Tmp, '/test_tool_panes_store', File).
:- use_module(library(lists), [member/2]).

test_tool_panes :-
    run_tests([ tool_panes,
                navigator_pane,
                debug_monitor_pane,
                debug_status_pane,
                xref_pane,
                profiler_pane,
                exception_editor_pane,
                debugger_pane,
                term_viewer_pane,
                manual_tool_panes,
                tool_pane_placement
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

%!  placed_area(+Window, -Area) is det.

placed_area(W, area(X, Y, Width, Height)) :-
    (   get(W, decoration, Decor),
        Decor \== @nil
    ->  Placed = Decor
    ;   Placed = W
    ),
    get(Placed, area, area(X, Y, Width, Height)).

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

%       What the monitor says a thread is.  A thread that cannot be
%       debugged used to be called a system thread, which is a class it
%       may well not be in: thread_create/3 makes a user thread, and it
%       is debuggable or not as the thread that made it is.

test(the_monitor_says_what_a_thread_is,
     Says == [console, console, system, 'no debug', debug, '']) :-
    findall(Postfix,
            ( member(Class-Debug, [console-(@nil), console-(@on),
                                   system-(@nil),
                                   user-(@nil), user-(@on), user-(@off)]),
              pce_thread_monitor:label_postfix(Class, Debug, Postfix)
            ),
            Says).

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

%       A tool pane takes a drop like any other pane: the tab it is in
%       splits it and the dropped pane takes the half the pointer is
%       nearest.

test(a_tool_takes_a_drop_and_is_split_by_it,
     true(Classes == [epilog_window, prolog_thread_monitor, picture])) :-
    no_frames,
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_thread_monitor, split),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    send(F, resize),
    get(TM, container, tab_frame, Tab),
    placed_position(TM, X, Y),
    get(TM, size, size(W, H)),
    PX is X+W-10,                       % the right edge of the tool
    PY is Y+H//2,
    get(Tab, drop_target, point(PX, PY), TM),
    send(Tab, drop, new(P, picture), point(PX, PY)),
    get(P, tile_manager, Tab),
    get(Tab, windows, Chain),
    chain_list(Chain, Windows),
    findall(C, (member(Win, Windows), get(Win, class_name, C)), Classes).

%       The outline that says where a drop would go is displayed on the
%       window it covers.  A tool pane is drawn first and its windows
%       over it, so an outline on the pane itself would be covered: it
%       goes on the windows it reaches.  ->create is what puts a window
%       in the <-subwindows of the one it is drawn over, and it does not
%       run with no display, so the chain is filled in here by hand.

test(the_outline_of_a_drop_on_a_tool_goes_on_its_windows,
     true(On == [thread_window])) :-
    no_frames,
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_thread_monitor, split),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    send(F, resize),
    as_subwindows(TM),
    get(TM, container, tab_frame, Tab),
    send(Tab, drop_feedback, TM, right),
    get(Tab, drop_feedback, Boxes),
    chain_list(Boxes, List),
    findall(N, (member(B, List), get(B?device, class_name, N)), On).

test(and_is_taken_away_again_when_the_pointer_moves_on, [fail]) :-
    no_frames,
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_thread_monitor, split),
    get(@prolog_ide, tool, prolog_thread_monitor, TM),
    send(F, resize),
    as_subwindows(TM),
    get(TM, container, tab_frame, Tab),
    send(Tab, drop_feedback, TM, right),
    get(Tab, drop_feedback, Boxes),
    chain_list(Boxes, List),
    send(Tab, clear_drop_feedback),
    ( get(Tab, drop_feedback, Left), Left \== @nil
    ; member(Box, List), object(Box)             % nothing left behind
    ).

%!  as_subwindows(+Pane) is det.
%
%   Say that the windows of Pane are drawn over it, as `window ->create'
%   does when there is a display to create them on.

as_subwindows(Pane) :-
    get(Pane, members, Chain),
    chain_list(Chain, Windows),
    send(Pane, slot, subwindows, new(Subs, chain)),
    forall(member(W, Windows), send(Subs, append, W)).

%       And it is on the Settings menu, so it can be changed without
%       editing a Defaults file.

test(the_setting_is_on_the_settings_menu,
     Items == [as_arranged, frame, tab, split]) :-
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

%       It says the sources too, since edit/1 goes by it; see
%       `prolog_ide <-source_placement'.

test(and_says_that_it_is_about_sources_as_well,
     true(Label == 'New tools and sources open')) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    get(F, menu_bar, MB),
    get(MB, member, settings, Settings),
    get(Settings, member, new_tools_open, Item),
    get(Item, label, Label).

test(the_setting_says_where_a_source_opens,
     true(Places == [window, tab, split])) :-
    findall(Where,
            ( member(Placement, [frame, tab, split]),
              with_placement(Placement,
                             get(@prolog_ide, source_placement, Where))
            ),
            Places).

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


                 /*******************************
                 *        THE DEBUG MONITOR     *
                 *******************************/

/* The debug monitor shows the debug topics beside the messages they
printed.  Its menu used to be a bar of its own; it goes on the bar of
whatever window it lands in now, under one popup of its own rather than
spread over the File and Settings menus of that window.
*/

%!  debug_monitor(-Monitor) is det.
%
%   The one debug monitor there is, made if there is none.

debug_monitor(M) :-
    get(@prolog_ide, show_tool, prolog_debug_monitor, @default, M).

no_debug_monitor :-
    (   get(@prolog_ide, tool, prolog_debug_monitor, M)
    ->  send(M, destroy)
    ;   true
    ).

:- begin_tests(debug_monitor_pane).

test(it_opens_in_a_window_of_the_ide, Classes == [prolog_debug_monitor]) :-
    no_frames,
    no_debug_monitor,
    debug_monitor(M),
    get(M, frame, F),
    send(F, instance_of, pane_frame),
    classes(F, Classes).

test(there_is_only_ever_one, true(Again == M)) :-
    debug_monitor(M),
    send(@prolog_ide, debug_monitor),
    get(@prolog_ide, tool, prolog_debug_monitor, Again).

test(its_two_windows_are_tiled_inside_it,
     Names == [prolog_debug_browser, prolog_debug_view]) :-
    debug_monitor(M),
    get(M, members, Chain),
    chain_list(Chain, Windows),
    findall(N, (member(W, Windows), get(W, class_name, N)), Names).

test(the_tab_is_named_after_it, true(Label == 'SWI-Prolog -- Debug monitor')) :-
    debug_monitor(M),
    get(M, frame, F),
    send(F, current_pane, M),
    get(F, label, Label).

%       One popup of its own, on the bar of the window it is in.

test(its_menu_reaches_the_bar_of_the_window_it_is_in,
     Items == [clear, refresh, save_as, disable_all, enable_all, help]) :-
    debug_monitor(M),
    get(M, frame, F),
    send(F, current_pane, M),
    menus(F, Menus),
    memberchk(debug_monitor, Menus),
    get(F, menu_bar, MB),
    get(MB, member, debug_monitor, Popup),
    get(Popup, members, Chain),
    chain_list(Chain, Members),
    findall(V, (member(MI, Members), get(MI, value, V)), Items).

%       An item without a message of its own goes to the pane the user is
%       working in, which is how the actions used to reach the frame.

test(and_its_items_act_on_it, true(Cleared == 0)) :-
    debug_monitor(M),
    get(M, frame, F),
    send(F, current_pane, M),
    setup_call_cleanup(
        debug(test_tool_panes_topic),
        ( debug(test_tool_panes_topic, 'a message', []),
          get(M?view?text_buffer, size, Size),
          Size > 0,
          get(F, menu_dialog, MD),
          get(MD, client, M),           % an item goes to the pane in view
          send(MD, action, clear),
          get(M?view?text_buffer, size, Cleared)
        ),
        nodebug(test_tool_panes_topic)).

%       The browser drives the view through the pane they share, not
%       through <-frame: the frame is a window of the IDE now.

test(selecting_a_topic_highlights_its_messages) :-
    debug_monitor(M),
    setup_call_cleanup(
        debug(test_tool_panes_topic),
        ( send(M, refresh),
          get(M, browser, B),
          get(B, members, Chain),
          chain_list(Chain, Items),
          member(DI, Items),
          get(DI, object, test_tool_panes_topic),
          !,
          send(B, selected, DI),
          get(M?view, styles, Styles),
          get(Styles, value, test_tool_panes_topic, _)
        ),
        nodebug(test_tool_panes_topic)).

test(what_it_has_to_say_grows_a_status_bar, true(Class == pane_status_dialog)) :-
    no_frames,
    no_debug_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_debug_monitor, split),
    get(@prolog_ide, tool, prolog_debug_monitor, M),
    \+ get(M?frame, status_dialog, _),
    send(M, report, status, 'saved'),
    get(M?frame, status_dialog, SD),
    get(SD, class_name, Class).

test(and_it_lands_along_the_bottom, true(Landed == below)) :-
    no_frames,
    no_debug_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_debug_monitor, split),
    get(@prolog_ide, tool, prolog_debug_monitor, M),
    get(M, container, tab_frame, Tab),
    get(Tab, windows, Chain),
    chain_list(Chain, Windows),
    member(Other, Windows), Other \== M, !,
    side_of(M, Other, Landed).

:- end_tests(debug_monitor_pane).


                 /*******************************
                 *       THE DEBUGGER STATUS    *
                 *******************************/

/* The debugger status is one dialog: the spy, trace and break points
with the buttons that set and clear them.  It was a frame holding that
dialog and a reporter; the dialog is the pane now, with no tool_pane
around it -- that is for a tool that shows more than one window.
*/

%!  debug_status(-Pane) is det.
%
%   The one debugger status pane there is, made if there is none.

debug_status(D) :-
    get(@prolog_ide, show_tool, prolog_debug_status, @default, D).

no_debug_status :-
    (   get(@prolog_ide, tool, prolog_debug_status, D)
    ->  send(D, destroy)
    ;   true
    ).

%!  grip(+Pane, -Handle) is semidet.
%
%   The grip Pane is dragged by, on its fixed layer.

grip(Pane, Handle) :-
    get(Pane, fixed_graphicals, Chain),
    Chain \== @nil,
    get(Chain, find, message(@arg1, instance_of, split_handle), Handle).

:- begin_tests(debug_status_pane).

test(it_opens_in_a_window_of_the_ide, Classes == [prolog_debug_status]) :-
    no_frames,
    no_debug_status,
    debug_status(D),
    get(D, frame, F),
    send(F, instance_of, pane_frame),
    classes(F, Classes).

test(there_is_only_ever_one, true(Again == D)) :-
    debug_status(D),
    send(@prolog_ide, open_debug_status),
    get(@prolog_ide, tool, prolog_debug_status, Again).

%       One dialog, not a tool_pane: there is only one window to show.

test(it_is_a_dialog_of_its_own) :-
    debug_status(D),
    send(D, instance_of, dialog),
    \+ send(D, instance_of, tool_pane).

test(the_tab_is_named_after_it, true(Label == 'SWI-Prolog -- Debugging')) :-
    debug_status(D),
    get(D, frame, F),
    send(F, current_pane, D),
    get(F, label, Label).

test(it_carries_a_grip_to_drag_it_by) :-
    debug_status(D),
    grip(D, _).

%       The grip is drawn over whatever the dialog lays out, so ->layout
%       keeps the corner it sits in clear.  ->layout runs on every resize,
%       so making room must not be something that accumulates: `graphical
%       ->right_side' sets the right edge by changing the width, and
%       asking for one further left made the menu narrower every time.
%       The menu is as wide as its label and items ask for -- how wide
%       that is depends on the font, so the test asks that it does not
%       change rather than what it is.

test(and_the_layout_keeps_the_corner_clear,
     [ forall(member(Width-Times, [600-1, 600-2, 600-3, 800-1, 800-3])),
       true(Kept-Clear == true-true)
     ]) :-
    debug_status(D),
    get(D, member, mode, Mode),
    get(Mode, width, W0),
    forall(between(1, Times, _),
           ( send(D, size, size(Width, 300)),
             send(D, layout, size(Width, 300)) )),
    get(Mode, width, W),
    (   W == W0
    ->  Kept = true
    ;   Kept = W0-W
    ),
    grip(D, H),
    send(H, compute),
    get(H, area, area(GX, _, _, _)),
    get(Mode, right_side, Right),
    (   Right =< GX
    ->  Clear = true
    ;   Clear = Right-GX
    ).

test(it_lists_what_is_being_debugged, true(Listed == ['append/3'])) :-
    debug_status(D),
    setup_call_cleanup(
        spy(lists:append/3),
        ( send(D, update),
          get(D, member, list_browser, LB),
          get(LB, members, Chain),
          chain_list(Chain, Items),
          findall(N, (member(I, Items), get(I, key, N)), Listed)
        ),
        nospy(lists:append/3)).

%       A pane that sizes itself to what it holds -- the debugger status
%       is a dialog -- says it can neither give nor take space, and then
%       `tile <-can_resize' answers @off and the gap beside it cannot be
%       dragged at all.  Docking one makes it as willing as the panes it
%       lands among.

test(the_gap_beside_it_can_be_dragged_once_it_is_docked,
     true(Gaps == 1)) :-
    no_frames,
    no_debug_status,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_debug_status, split),
    get(@prolog_ide, tool, prolog_debug_status, D),
    send(F, resize),
    get(D, container, tab_frame, Tab),
    get(Tab, root_tile, Tile),
    get(Tile, resize_areas, Areas),
    get(Areas, size, Gaps).

test(what_it_has_to_say_grows_a_status_bar, true(Class == pane_status_dialog)) :-
    no_frames,
    no_debug_status,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    send(@prolog_ide, show_tool, prolog_debug_status, split),
    get(@prolog_ide, tool, prolog_debug_status, D),
    \+ get(D?frame, status_dialog, _),
    send(D, report, warning, 'No predicate'),
    get(D?frame, status_dialog, SD),
    get(SD, class_name, Class).

:- end_tests(debug_status_pane).


                 /*******************************
                 *      THE CROSS-REFERENCER    *
                 *******************************/

/* The cross-referencer carries three windows: the filter across the top,
the browsers on the left and the workspaces on the right.  The last two
are tabbed windows of their own, so a tool pane can hold tabs without
being confused with the tabs of the window it is in.
*/

%!  xref(-Tool) is det.
%
%   The one cross-referencer there is, made if there is none.  It is not
%   asked to analyse anything: that reads every source file of the
%   program, in a thread of its own.

xref(F) :-
    get(@prolog_ide, show_tool, xref_tool, @default, F).

no_xref :-
    (   get(@prolog_ide, tool, xref_tool, F)
    ->  send(F, destroy)
    ;   true
    ).

:- begin_tests(xref_pane).

test(it_opens_in_a_window_of_the_ide, Classes == [xref_tool]) :-
    no_frames,
    no_xref,
    xref(F),
    get(F, frame, Frame),
    send(Frame, instance_of, pane_frame),
    classes(Frame, Classes).

test(there_is_only_ever_one, true(Again == F)) :-
    xref(F),
    send(@prolog_ide, xref),
    get(@prolog_ide, tool, xref_tool, Again).

test(its_three_windows_are_tiled_inside_it,
     Names == [filter_dialog, browsers, workspaces]) :-
    xref(F),
    get(F, members, Chain),
    chain_list(Chain, Windows),
    findall(N, (member(W, Windows), get(W, name, N)), Names).

test(the_tab_is_named_after_it,
     true(Label == 'SWI-Prolog -- Cross-referencer')) :-
    xref(F),
    get(F, frame, Frame),
    send(Frame, current_pane, F),
    get(Frame, label, Label).

%       Its two tabbed windows are told apart by name: <-window would ask
%       by class and both are a plain tabbed_window, and <-member on the
%       tool answers the window of one of *its* tabs.

test(the_browsers_are_reached_through_the_pane,
     Names == [xref_file_tree, xref_predicate_browser]) :-
    xref(F),
    get(F, browser, files, Tree),
    get(F, browser, predicates, Predicates),
    findall(N,
            ( member(W, [Tree, Predicates]),
              get(W, class_name, N)
            ),
            Names).

test(and_so_are_the_workspaces, true(Class == prolog_file_info)) :-
    xref(F),
    get(F, workspace, file_info, @on, @on, WS),
    get(WS, class_name, Class).

%       A graphical deep in the tool used to reach it with <-frame.

test(a_window_of_the_tool_reaches_the_tool, true(Reached == F)) :-
    xref(F),
    get(F, browser, files, Tree),
    get(Tree, container, xref_tool, Reached).

test(and_so_does_an_item_of_its_filter, true(Reached == F)) :-
    xref(F),
    get(F, window, xref_filter_dialog, FD),
    get(FD, member, filter_on_filename, Item),
    get(Item, container, xref_tool, Reached).

%       Its menu is one popup of its own on the bar of the window it is
%       in, with the settings as a pull-right that fills itself in.

test(its_menu_reaches_the_bar_of_the_window_it_is_in,
     Items == [refresh, settings, about]) :-
    xref(F),
    get(F, frame, Frame),
    send(Frame, current_pane, F),
    menus(Frame, Menus),
    memberchk(xref, Menus),
    get(Frame, menu_bar, MB),
    get(MB, member, xref, Popup),
    get(Popup, members, Chain),
    chain_list(Chain, Members),
    findall(V, (member(MI, Members), get(MI, value, V)), Items).

test(the_settings_show_what_is_in_force,
     true(Ticked == [@off, @on, @on])) :-
    xref(F),
    get(F, frame, Frame),
    send(Frame, current_pane, F),
    get(Frame, menu_bar, MB),
    get(MB, member, xref, Popup),
    get(Popup, member, settings, Item),
    get(Item, popup, Settings),
    send(F, update_setting_menu, Settings),   % what its update_message does
    findall(On,
            ( member(Name, [warn_autoload,   % false by default; the other
                            warn_not_called, % two are true
                            hide_system_files]),
              get(Settings, member, Name, MI),
              get(MI, selected, On)
            ),
            Ticked).

test(what_it_has_to_say_grows_a_status_bar, true(Class == pane_status_dialog)) :-
    no_frames,
    no_xref,
    epilog_frame(@default, @default, @default, @off, @default, Frame),
    send(Frame, open),
    send(@prolog_ide, show_tool, xref_tool, split),
    get(@prolog_ide, tool, xref_tool, F),
    \+ get(F?frame, status_dialog, _),
    send(F, report, progress, 'XREF %s', 'somewhere.pl'),
    get(F?frame, status_dialog, SD),
    get(SD, class_name, Class).

:- end_tests(xref_pane).


                 /*******************************
                 *       THE MANUAL TOOLS       *
                 *******************************/

/* Every tool of the XPCE manual is a man_frame, and man_frame is a
tool_pane: the one class turned all fourteen of them into panes at once.
What a tool says to build itself -- ->append, <-member, <-<name>_member,
->label, ->open, ->keyboard_focus -- man_frame answers over the pane, so
the tools themselves did not have to change.  What did change is that a
window inside a tool reaches it with <-container(man_frame) rather than
with <-frame, which is a window of the IDE now.
*/

manual_tool(class_browser,    man_class_browser(@manual)).
manual_tool(class_hierarchy,  man_class_hierarchy(@manual)).
manual_tool(search,           man_search_tool(@manual)).
manual_tool(topics,           man_topic_browser(@manual)).
manual_tool(card_viewer,      man_card_editor(@manual)).
manual_tool(statistics,       man_statistics(@manual)).
manual_tool(inspector,        isp_frame(@manual)).
manual_tool(visual_hierarchy, vis_frame(@manual)).
manual_tool(global_objects,   man_object_browser(@manual)).
manual_tool(errors,           man_error_browser(@manual)).
manual_tool(group_overview,   man_group_browser(@manual, groups,
                                                'Group Browser')).
manual_tool(examples,         man_module_browser(@manual, examples,
                                                 man_example_card,
                                                 'XPCE Examples')).
manual_tool(event_viewer,     man_event_viewer(@manual)).

%!  open_manual_tool(+Name, -Tool) is det.
%
%   Make the named tool and show it.  Not `@manual <-start_tool', which
%   tells the user with a modal dialog when a tool cannot be made: with
%   no display there is nobody to answer it.

open_manual_tool(Name, Tool) :-
    manual_tool(Name, Term),
    new(Tool, Term),
    send(Tool, open).

:- begin_tests(manual_tool_panes).

test(every_tool_opens_in_a_window_of_the_ide,
     [ forall(manual_tool(Name, _)),
       true(Class == pane_frame)
     ]) :-
    no_frames,
    open_manual_tool(Name, Tool),
    get(Tool, frame, Frame),            % a pane in no window would be
    get(Frame, class_name, Class).      % given a plain frame of its own

test(and_is_named_on_its_tab, true(Label == 'Class Hierarchy')) :-
    open_manual_tool(class_hierarchy, Tool),
    get(Tool, pane_label, Label).

%       A tool that renames itself -- the class browser says which class
%       it shows -- used to put that on the title of its frame.

test(a_tool_that_renames_itself_renames_its_tab,
     true(Label == 'Class object')) :-
    no_frames,
    open_manual_tool(class_browser, Tool),
    get(Tool, frame, Frame),
    send(Frame, current_pane, Tool),
    get(Frame, tab, Tab),
    get(Tab, label, Label).

%       What a tool says to build itself.

test(a_tool_adds_a_window_with_append, true(Names == [dialog, picture])) :-
    open_manual_tool(statistics, Tool),
    send(Tool, append, new(P, picture)),
    send(P, name, picture),
    get(Tool, members, Chain),
    chain_list(Chain, Windows),
    findall(N, (member(W, Windows), get(W, name, N)), Names).

test(and_finds_it_back_by_name, true(Found == P)) :-
    open_manual_tool(statistics, Tool),
    send(Tool, append, new(P, picture)),
    send(P, name, picture),
    get(Tool, member, picture, Found).

test(and_by_the_shorthand_class_frame_offered, true(Found == P)) :-
    open_manual_tool(statistics, Tool),
    send(Tool, append, new(P, picture)),
    send(P, name, picture),
    get(Tool, picture_member, Found).

%       And what a window inside a tool says to reach it.

test(a_window_of_a_tool_reaches_the_tool, true(Reached == Tool)) :-
    open_manual_tool(class_hierarchy, Tool),
    get(Tool, member, dialog, Dialog),
    get(Dialog, container, man_frame, Reached).

%       And through the tool, whatever the tool knows: the visual
%       hierarchy asks its manual to inspect what is selected in it.

test(and_through_it_whatever_the_tool_knows, true(M == @manual)) :-
    open_manual_tool(visual_hierarchy, Tool),
    get(Tool, window, W),
    get(W, container, man_frame, T),
    get(T, manual, M).

%       A window inside a tool asks the tool for what the tool answers --
%       show this class, show its source, select the card for it.  They
%       used to send those to themselves and let `window ->catch_all'
%       hand them to <-frame; the frame is a window of the IDE now and
%       knows none of them.

test(a_window_of_a_tool_asks_the_tool_to_show_something) :-
    no_frames,
    open_manual_tool(class_hierarchy, Tool),
    get(Tool, member, man_class_hierarchy_window, Window),
    get(@pce, convert, pane_frame, class, Class),
    send(Window, select_node, Class),
    send(Window, open_node, Class).

test(and_so_does_the_browser_of_a_summary, true(Asked == Tool)) :-
    no_frames,
    open_manual_tool(class_browser, Tool),
    get(Tool, member, man_summary_browser, Browser),
    get(Browser, select_message, Message),
    get(Message, receiver, Receiver),   % ?(browser, container, man_frame)
    get(Receiver, execute, Asked).

test(the_tool_is_not_the_window_it_is_in) :-
    open_manual_tool(class_hierarchy, Tool),
    get(Tool, frame, Frame),
    Frame \== Tool,
    send(Frame, instance_of, pane_frame).

%       A card viewer keeps a history of what it showed, and the window
%       that shows a card tells it -- `doc_window <-history_holder'.  It
%       used to tell <-frame, which was the tool; the frame is a window
%       of the IDE now and answers no ->add_history, and that took the
%       whole of `doc_window ->url' down with it: no card appeared.

test(the_history_of_a_card_is_kept_by_the_tool, true(Holder == CE)) :-
    no_frames,
    open_manual_tool(card_viewer, CE),
    get(CE, member, html_card, HC),
    get(HC, history_holder, Holder).

%!  reference_manual is semidet.
%
%   True when the HTML reference manual a card shows was built.

reference_manual :-
    catch(absolute_file_name(swi_man_xpce('class-frame.html'), _,
                             [ access(read), file_errors(fail) ]),
          _, fail).

test(and_a_card_shown_is_a_card_it_remembers,
     [ condition(reference_manual),
       true(Recorded == URL)
     ]) :-
    no_frames,
    open_manual_tool(card_viewer, CE),
    get(CE, member, html_card, HC),
    get(@pce, convert, frame, class, Class),
    send(HC, selection, Class),
    get(HC, url, URL),
    get(CE?history, current, Recorded).

test(and_so_is_a_link_followed_in_it,
     [ condition(reference_manual),
       true(Anchor == 'class-frame-get-confirm_centered')
     ]) :-
    no_frames,
    open_manual_tool(card_viewer, CE),
    get(CE, member, html_card, HC),
    get(@pce, convert, frame, class, Class),
    send(HC, selection, Class),
    send(HC, goto_url,
         'class-frame.html#class-frame-get-confirm_centered'),
    get(CE?history, current, Recorded),
    atomic_list_concat([_, Anchor], '#', Recorded).

%       man_frame answers a handful of frame methods over the pane, and a
%       method there must keep the shape class window gives it: ->create
%       takes the window to create the pane inside -- see createWindow()
%       in src/win/window.c -- and one of another shape is not an override
%       but a clash, which leaves the pane uncreated.

test(the_frame_methods_a_tool_uses_keep_their_shape,
     [ forall(member(Selector-Args, [create-1, open-2, open_centered-3,
                                     keyboard_focus-1, label-1])),
       true(Types == Args)
     ]) :-
    get(@pce, convert, man_frame, class, Class),
    get(Class, send_method, Selector, Method),
    get(Method?types, size, Types).

%       A tool bar acts on <-client, and a tool_dialog made without one
%       falls back to <-frame -- a window of the IDE now, which knows
%       nothing of ->grab or ->clear.  The tools that carry one say who
%       their buttons act on.

test(a_tool_bar_acts_on_the_tool_that_carries_it,
     [ forall(member(Term, [isp_frame(@manual), vis_frame(@manual)])),
       true(Client == Tool)
     ]) :-
    new(Tool, Term),
    send(Tool, open),
    get(Tool, member, tool_dialog, TD),
    get(TD, tool_bar, TB),
    get(TB, client, Client).

%       A tool with a menu of its own puts it on the bar of the window it
%       is in, like any other pane.

test(a_tool_with_a_menu_puts_it_on_the_bar_of_its_window) :-
    no_frames,
    open_manual_tool(event_viewer, Tool),
    get(Tool, frame, Frame),
    send(Frame, current_pane, Tool),
    menus(Frame, Menus),
    memberchk(events, Menus).

test(what_a_tool_has_to_say_grows_a_status_bar,
     true(Class == pane_status_dialog)) :-
    no_frames,
    open_manual_tool(statistics, Tool),
    get(Tool, frame, Frame),
    \+ get(Frame, status_dialog, _),
    send(Tool, report, status, 'busy'),
    get(Frame, status_dialog, SD),
    get(SD, class_name, Class).

:- end_tests(manual_tool_panes).


                 /*******************************
                 *         THE PROFILER         *
                 *******************************/

/* The profiler shows the predicates that were sampled beside the details
of the one selected.  Unlike the other tools it is not a singleton: every
profile opens a pane of its own, as it opened a frame of its own before.
*/

%!  profiler(-Tool) is det.
%
%   Profile something small and show the result.

profiler(F) :-
    profile(numlist(1, 10000, _), [time(cpu)]),
    profile_data(Data),
    pce_profile:show_profile(Data),
    get(@prolog_ide, tool, prof_frame, F).

:- begin_tests(profiler_pane).

test(it_opens_in_a_window_of_the_ide, Classes == [prof_frame]) :-
    no_frames,
    profiler(F),
    get(F, frame, Frame),
    send(Frame, instance_of, pane_frame),
    classes(Frame, Classes).

test(its_two_windows_are_tiled_inside_it,
     Names == [prof_browser, prof_details]) :-
    no_frames,
    profiler(F),
    get(F, members, Chain),
    chain_list(Chain, Windows),
    findall(N, (member(W, Windows), get(W, class_name, N)), Names).

test(the_tab_is_named_after_it, true(Label == 'SWI-Prolog -- Profile')) :-
    no_frames,
    profiler(F),
    get(F, frame, Frame),
    send(Frame, current_pane, F),
    get(Frame, label, Label).

test(its_menu_reaches_the_bar_of_the_window_it_is_in,
     Items == [sort_by, show_time_as, help]) :-
    no_frames,
    profiler(F),
    get(F, frame, Frame),
    send(Frame, current_pane, F),
    menus(Frame, Menus),
    memberchk(profile, Menus),
    get(Frame, menu_bar, MB),
    get(MB, member, profile, Popup),
    get(Popup, members, Chain),
    chain_list(Chain, Members),
    findall(V, (member(MI, Members), get(MI, value, V)), Items).

test(it_lists_what_was_sampled, true(Listed == true)) :-
    no_frames,
    profiler(F),
    get(F, window, prof_browser, B),
    get(B?dict?members, size, N),
    (   N > 0
    ->  Listed = true
    ;   Listed = N
    ).

%       Its browser, its details and the texts in them used to reach it
%       with <-frame; the frame is a window of the IDE now.

test(the_browser_reaches_the_tool_to_show_details) :-
    no_frames,
    profiler(F),
    get(F, window, prof_browser, B),
    get(B?dict?members, head, Item),
    send(Item, details),
    get(F, window, prof_details, W),
    get(W, tabular, Tabular),
    get(Tabular?graphicals, size, N),
    N > 0.

test(and_so_does_a_text_of_the_details) :-
    no_frames,
    profiler(F),
    get(F, window, prof_browser, B),
    get(B?dict?members, head, Item),
    send(Item, details),
    get(F, window, prof_details, W),
    get(W?tabular, graphicals, Chain),
    chain_list(Chain, Graphicals),
    member(Text, Graphicals),
    send(Text, instance_of, prof_node_text),
    !,
    send(Text, details).

%       The details window carries no label of its own: a label puts a row
%       on the window_decorator it is held in, and the grip that drags the
%       profiler around would land in that row rather than in the corner
%       of the window.

test(the_details_carry_no_label_of_their_own, [fail]) :-
    no_frames,
    profiler(F),
    get(F, window, prof_browser, B),
    get(B?dict?members, head, Item),
    send(Item, details),                % this used to name the predicate
    get(F, window, prof_details, W),    % on the label
    get(W, label, _).

test(so_the_grip_sits_in_the_corner_of_the_window_it_is_on) :-
    no_frames,
    profiler(F),
    get(F, frame, Frame),
    send(Frame, resize),
    get(F, corner_window, W),
    get(W, class_name, prof_details),
    get(F, grip, Grip),
    send(F, place_grip),
    send(Grip, compute),
    get(Grip, area, area(_, GY, _, _)),
    GY < 8.

test(how_the_times_are_read_is_the_tools_to_say) :-
    no_frames,
    profiler(F),
    send(F, time_view, seconds),
    send(F, time_view, percentage),
    send(F, sort_by, ticks, normal).

%       Not a singleton: a second profile is a second pane.

test(every_profile_opens_a_pane_of_its_own, true(Panes == 2)) :-
    no_frames,
    profiler(_),
    profiler(_),
    get(@prolog_ide, members, Chain),
    chain_list(Chain, Frames),
    findall(P,
            ( member(Frame, Frames),
              send(Frame, instance_of, pane_frame),
              get(Frame, panes, Ps),
              chain_list(Ps, PL),
              member(P, PL),
              send(P, instance_of, prof_frame)
            ),
            Profilers),
    length(Profilers, Panes).

test(what_it_has_to_say_grows_a_status_bar, true(Class == pane_status_dialog)) :-
    no_frames,
    profiler(F),
    get(F, frame, Frame),
    send(F, report, status, 'loading'),
    get(Frame, status_dialog, SD),
    get(SD, class_name, Class).

:- end_tests(profiler_pane).


                 /*******************************
                 *      THE EXCEPTION EDITOR    *
                 *******************************/

/* The exception editor says which exceptions stop the debugger.  There
is one of it, @prolog_exception_window: the hook that records an
exception refreshes it by that name, so it is not made afresh the way the
profiler is.
*/

editor(W) :-
    send(@prolog_ide, open_exceptions),
    W = @prolog_exception_window.

:- begin_tests(exception_editor_pane).

test(it_opens_in_a_window_of_the_ide, Classes == [prolog_trace_exception]) :-
    no_frames,
    editor(W),
    get(W, frame, Frame),
    send(Frame, instance_of, pane_frame),
    classes(Frame, Classes).

test(there_is_one_of_it, true(Again == W)) :-
    editor(W),
    editor(Again).

test(the_tab_is_named_after_it, true(Label == 'SWI-Prolog -- Exceptions')) :-
    no_frames,
    editor(W),
    get(W, frame, Frame),
    send(Frame, current_pane, W),
    get(Frame, label, Label).

test(its_menu_reaches_the_bar_of_the_window_it_is_in,
     Items == [clear_all,
               'New (error, first)', 'New (general, first)',
               'New (error, last)', 'New (general, last)',
               debug_mode, nodebug_mode]) :-
    no_frames,
    editor(W),
    get(W, frame, Frame),
    send(Frame, current_pane, W),
    menus(Frame, Menus),
    memberchk(exceptions, Menus),
    get(Frame, menu_bar, MB),
    get(MB, member, exceptions, Popup),
    get(Popup, members, Chain),
    chain_list(Chain, Members),
    findall(V, (member(MI, Members), get(MI, value, V)), Items).

%       The table it shows is the pane's own content now, not a window
%       inside a frame.

test(what_it_shows_is_a_table_of_its_own) :-
    editor(W),
    get(W, table, Table),
    send(Table, instance_of, tabular),
    get(Table, device, W).

test(and_a_new_exception_reaches_it) :-
    editor(W),
    setup_call_cleanup(
        send(W, clear_all),
        ( send(W, new, prolog(error(type_error(_, _), _))),
          get(W, table, Table),
          get(Table?graphicals, size, Rows),
          Rows > 0
        ),
        send(W, clear_all)).

%       The columns of that table are rubber, so left to itself it asks
%       for the width its widest exception term would like -- about 1900
%       pixels.  A frame of its own remembered a size and hid that; a pane
%       hands its wish to the window it is docked in, which would open
%       that wide.

test(it_asks_for_a_width_a_window_can_live_with, true(Sane == true)) :-
    new(W, prolog_trace_exception),
    get(W, class_variable_value, size, size(Wanted, _)),
    setup_call_cleanup(
        send(W, new, prolog(error(type_error(a_long_type_name_indeed,
                                             a_long_culprit_as_well), _))),
        ( send(W, refresh),
          send(W, '_compute_desired_size'),
          get(W, table, Table),
          get(Table, area, area(_, _, Width, _)),
          (   Width =< Wanted
          ->  Sane = true
          ;   Sane = Width-Wanted
          )
        ),
        send(W, clear_all)),
    send(W, destroy).

test(what_it_has_to_say_grows_a_status_bar, true(Class == pane_status_dialog)) :-
    no_frames,
    editor(W),
    get(W, frame, Frame),
    \+ get(Frame, status_dialog, _),
    send(W, report, status, 'saved'),
    get(Frame, status_dialog, SD),
    get(SD, class_name, Class).

:- end_tests(exception_editor_pane).


                 /*******************************
                 *          THE DEBUGGER        *
                 *******************************/

/* The source-level debugger carries the buttons, the bindings, the call
stack and the source.  There is one per traced thread and break level, so
like the profiler it is not a singleton.
*/

%!  debugger(-Tool) is det.
%
%   A debugger for the main thread at break level 0, placed.  Not through
%   the tracer: that would want a goal to trace and somebody to answer at
%   every port.

debugger(F) :-
    new(F, prolog_debugger(0, main)),
    send(F, open).

%!  own_grip(+Window, -Handle) is semidet.
%
%   The grip Window displays on itself, if it has one.

own_grip(W, Handle) :-
    get(W, fixed_graphicals, Graphicals),
    get(Graphicals, find, message(@arg1, instance_of, split_handle), Handle).

:- begin_tests(debugger_pane).

test(it_opens_in_a_window_of_the_ide, Classes == [prolog_debugger]) :-
    no_frames,
    debugger(F),
    get(F, frame, Frame),
    send(Frame, instance_of, pane_frame),
    classes(Frame, Classes).

test(its_four_windows_are_tiled_inside_it,
     Names == [buttons, bindings, stack, prolog_source_view]) :-
    no_frames,
    debugger(F),
    get(F, members, Chain),
    chain_list(Chain, Windows),
    findall(N, (member(W, Windows), get(W, name, N)), Names).

%       Its windows are named, and it used to find them back with
%       `frame <-member'.

test(and_it_finds_them_back_by_name,
     Classes == [prolog_button_dialog, prolog_bindings_view,
                 prolog_stack_view]) :-
    no_frames,
    debugger(F),
    findall(C,
            ( member(Name, [buttons, bindings, stack]),
              get(F, member, Name, W),
              get(W, class_name, C)
            ),
            Classes).

%       The tab says which thread it traces; the label it used to put on
%       its frame is what the window makes its title from.

test(the_tab_says_which_thread_it_traces,
     true(Label == 'SWI-Prolog -- Debugger')) :-
    no_frames,
    debugger(F),
    get(F, frame, Frame),
    send(Frame, current_pane, F),
    get(Frame, label, Label).

test(and_names_the_thread_when_it_is_not_the_main_one,
     true(Label == 'Debugger [worker]')) :-
    new(F, prolog_debugger(0, worker)),
    get(F, pane_label, Label),
    send(F, destroy).

test(its_menu_reaches_the_bar_of_the_window_it_is_in,
     Items == [settings, clear_source_cache,
               breakpoints, exceptions,
               toggle_edit_mode, copy_goal,
               view, make, help_on_debugger, quit]) :-
    no_frames,
    debugger(F),
    get(F, frame, Frame),
    send(Frame, current_pane, F),
    menus(Frame, Menus),
    memberchk(debugger, Menus),
    get(Frame, menu_bar, MB),
    get(MB, member, debugger, Popup),
    get(Popup, members, Chain),
    chain_list(Chain, Members),
    findall(V, (member(MI, Members), get(MI, value, V)), Items).

%       The tracer waits in <-confirm for an action and ->return_action
%       gives it one.  Which answer belongs to which debugger is kept per
%       pane: two threads can be traced at once into one window.

test(the_action_the_user_picks_is_kept_per_debugger,
     true(Answers == [creep, leap])) :-
    no_frames,
    debugger(One),
    new(Two, prolog_debugger(1, main)),
    send(One, return_action, creep),
    send(Two, return_action, leap),
    get(One, return_value, A),
    get(Two, return_value, B),
    Answers = [A, B],
    send(Two, destroy).

%       The bindings and the call stack share the row under the buttons
%       and the source runs the whole width below them.  Which is what
%       the order of ->right and ->below in ->initialise is for: pair the
%       two before either is taken in, or the stack ends up beside the
%       source as well.

test(the_source_runs_below_the_bindings_and_the_stack) :-
    no_frames,
    debugger(F),
    get(F, frame, Frame),
    send(Frame, resize),
    get(F, member, bindings, V),
    get(F, member, stack, S),
    get(F, source, Src),
    placed_area(V,   area(VX, VY, VW, VH)),
    placed_area(S,   area(SX, SY,  _,  _)),
    placed_area(Src, area( _, CY, CW,  _)),
    SY =:= VY,                          % the stack shares the row
    SX >= VX+VW,                        % to the right of the bindings
    CY >= VY+VH,                        % the source is below them
    CW > VW.                            % and wider than the bindings

%       A button and a key in the source both tell the tracer what the
%       user picked; ->return used to reach the debugger because a dialog
%       hands it to <-frame, which was the debugger.

test(a_button_tells_the_tracer_what_was_picked, true(Picked == creep)) :-
    no_frames,
    debugger(F),
    send(F, mode, wait_user),
    send(F, slot, return_value, @nil),
    get(F, member, buttons, D),
    get(D, member, tool_bar, TB),
    get(TB, member, creep, Button),
    send(Button?message, forward),
    get(F, return_value, Picked).

test(and_so_does_a_key_in_the_source, true(Picked == creep)) :-
    no_frames,
    debugger(F),
    send(F, mode, wait_user),
    send(F, slot, return_value, @nil),
    get(F, source, Src),
    send(Src, post_event, event(32, Src, 10, 10)),   % SPC: creep
    get(F, return_value, Picked).

test(what_it_has_to_say_grows_a_status_bar, true(Class == pane_status_dialog)) :-
    no_frames,
    debugger(F),
    get(F, frame, Frame),
    \+ get(Frame, status_dialog, _),
    send(F, report, status, 'Call: foo/1'),
    get(Frame, status_dialog, SD),
    get(SD, class_name, Class).

%       The grip is the handle the whole debugger is dragged by, and it
%       goes on the window in its top right corner: the dialog along the
%       top.

test(the_grip_is_on_the_dialog_along_the_top, true(Name == buttons)) :-
    no_frames,
    debugger(F),
    get(F, frame, Frame),
    send(Frame, resize),
    send(F, place_grip),
    get(F, grip, Handle),
    send(Handle, compute),
    get(Handle, device, Device),
    get(Device, name, Name),
    get(Handle, displayed, @on).

%       The source carries a grip of its own -- every emacs_view does,
%       and that is how PceEmacs moves an editor about.  Here it is not a
%       pane of the window but a window of the debugger, so dragging it
%       would take the source out of the tool.  It hides itself.

test(the_source_hides_the_grip_it_carries_itself, true(Shown == @off)) :-
    no_frames,
    debugger(F),
    get(F, frame, Frame),
    send(Frame, resize),
    get(F, source, Src),
    own_grip(Src, Handle),
    send(Handle, compute),
    get(Handle, displayed, Shown).

%       Typing in the debugger is typing in the source: the keys it does
%       not use itself are the tracer's actions (see ->post_event above).
%       So the source takes the keyboard when the debugger becomes the
%       pane in use, and its caret is drawn as having the focus.

test(the_source_takes_the_keyboard, true(Focus == @on)) :-
    no_frames,
    debugger(F),
    send(F, input_focus, @on),
    get(F, source, Src),
    get(Src, input_focus, Focus).

test(and_it_can_be_closed_when_nobody_is_waiting, true(Close == @on)) :-
    no_frames,
    debugger(F),
    send(F, mode, query_finished),
    get(F, can_close, Close).

:- end_tests(debugger_pane).


                 /*******************************
                 *         TERM VIEWER          *
                 *******************************/

/* The window a value is shown in.

Clicking a variable in the bindings of the debugger shows its value here
-- see `prolog_bindings_view ->details'.  It used to be a frame of its
own, titled with the variable it was showing; a pane says that on its tab
and on the status bar of the window it lands in.
*/

:- begin_tests(term_viewer_pane).

viewer(TV) :-
    view_term(foo(bar, [1,2,3]),
              [ label('Variable X'),
                comment('Variable X of frame at level 3 running foo/2')
              ]),
    get(@prolog_ide, tool, term_viewer, TV).

test(it_opens_in_a_window_of_the_ide, Classes == [term_viewer]) :-
    no_frames,
    viewer(TV),
    get(TV, frame, Frame),
    send(Frame, instance_of, pane_frame),
    classes(Frame, Classes).

test(its_windows_are_the_controls_and_the_term,
     Names == [dialog, view]) :-
    no_frames,
    viewer(TV),
    get(TV, members, Chain),
    chain_list(Chain, Windows),
    findall(N, (member(W, Windows), get(W, class_name, N)), Names).

test(and_the_term_is_written_in_the_view, true(Text == 'foo(bar,[1,2,3])')) :-
    no_frames,
    viewer(TV),
    get(TV, text_buffer, TB),
    get(TB?contents, value, Text).

%       The tab used to be the title of a frame, which had room for a
%       sentence.  The caller gives a name that fits a tab and says the
%       sentence on the status bar instead.

test(its_tab_is_named_by_whoever_asked_for_it, true(Label == 'Variable X')) :-
    no_frames,
    viewer(TV),
    get(TV, pane_label, Label).

test(and_a_viewer_nobody_named_is_just_a_term, true(Label == 'Term')) :-
    no_frames,
    view_term(foo, []),
    get(@prolog_ide, tool, term_viewer, TV),
    get(TV, pane_label, Label).

test(what_it_is_showing_goes_on_the_status_bar,
     true(Class == pane_status_dialog)) :-
    no_frames,
    viewer(TV),
    get(TV, frame, Frame),
    get(Frame, status_dialog, SD),
    get(SD, class_name, Class).

%       An unpinned viewer takes the next value; a pinned one is left
%       where it is and the next value opens a viewer of its own.

test(the_next_value_takes_an_unpinned_viewer_over,
     true(Panes == [term_viewer])) :-
    no_frames,
    viewer(TV),
    view_term(other(term), [label('Variable Y')]),
    get(TV, pane_label, 'Variable Y'),
    get(TV, frame, Frame),
    classes(Frame, Panes).

test(and_a_pinned_one_is_left_showing_what_it_shows,
     true(Panes-Label == [term_viewer,term_viewer]-'Variable X')) :-
    no_frames,
    viewer(TV),
    send(TV, pinned, @on),
    view_term(other(term), [label('Variable Y')]),
    get(TV, pane_label, Label),
    get(TV, frame, Frame),
    classes(Frame, Panes).

:- end_tests(term_viewer_pane).


                 /*******************************
                 *          PLACEMENT           *
                 *******************************/

/* Where a new pane lands when the setting is left at `as_arranged': the
   IDE reads the arrangements of library(pane_layouts) rather than putting
   everything in a tab.  A navigator belongs down the left of the panes
   there are, at a fifth of the width; a tool nothing has been arranged
   with still takes a tab.
*/

:- begin_tests(tool_pane_placement).

%!  console(-Frame) is det.
%
%   An open window of the IDE holding a terminal.

console(F) :-
    no_monitor,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open).

%!  tab_shape(+Frame, -Shape) is det.
%
%   The panes of the tab in view, by kind, with the shares dropped.

tab_shape(F, Shape) :-
    get(F, tab, Tab),
    get(Tab, window_tree, Tree),
    tree_shape(Tree, Shape).

tree_shape(Tree, Kind) :-
    object(Tree),
    !,
    pane_kind(Tree, Kind).
tree_shape(Tree, Shape) :-
    Tree =.. [Orientation, Shares],
    findall(S, ( member(Share, Shares),
                 tree_content(Share, Content),
                 tree_shape(Content, S)
               ), Subs),
    Shape =.. [Orientation, Subs].

tree_content(_-Content, Content) :- !.
tree_content(Content, Content).

%!  tab_share(+Frame, +Kind, -Share) is semidet.

tab_share(F, Kind, Share) :-
    get(F, tab, Tab),
    get(Tab, window_tree, Tree),
    Tree =.. [_, Shares],
    member(Share-Content, Shares),
    object(Content),
    pane_kind(Content, Kind),
    !.

test(a_navigator_goes_down_the_left_of_what_is_there,
     Shape == horizontal([prolog_navigator, terminal])) :-
    console(F),
    send(@prolog_ide, place_pane, new(_N, prolog_navigator), F),
    tab_shape(F, Shape).

test(and_takes_the_share_it_was_arranged_at) :-
    console(F),
    send(@prolog_ide, place_pane, new(_N, prolog_navigator), F),
    tab_share(F, prolog_navigator, Share),
    assertion(abs(Share-0.2) < 0.03).

%       Nothing has been arranged with a thread monitor, so it falls back
%       on what the IDE has always done with a tool: a tab.

test(a_tool_nothing_says_anything_about_takes_a_tab, Tabs == 2) :-
    console(F),
    send(@prolog_ide, place_pane, new(_M, prolog_thread_monitor), F),
    get(F, tabs, TW),
    get(TW, tabs, Chain),
    get(Chain, size, Tabs).

%       And the fixed answers still hold when the user pins one.

test(the_setting_still_overrules, Tabs == 2) :-
    console(F),
    send(@prolog_ide, place_pane, new(_N, prolog_navigator), F, tab),
    get(F, tabs, TW),
    get(TW, tabs, Chain),
    get(Chain, size, Tabs).

test(and_a_window_of_its_own_is_a_window_of_its_own, Panes == 1) :-
    console(F),
    send(@prolog_ide, place_pane, new(N, prolog_navigator), F, frame),
    get(N, frame, Own),
    assertion(Own \== F),
    get(Own, panes, Chain),
    get(Chain, size, Panes).

:- end_tests(tool_pane_placement).
