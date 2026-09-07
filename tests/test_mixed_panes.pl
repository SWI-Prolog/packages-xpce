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


:- module(test_mixed_panes, [test_mixed_panes/0]).
:- encoding(utf8).

/** <module> A terminal and an editor in one window

This is what library(pane_frame) exists for: an Epilog terminal and a
PceEmacs editor as panes of the same window, with the menu bar following
whichever of them the user is working in.

The frames are never opened; a terminal connects to a Prolog thread when
it is created and none of this needs one.

Run with:

    swipl -g test_mixed_panes -t halt \
          packages/xpce/tests/test_mixed_panes.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(epilog)).
:- use_module(library(swi_ide)).
:- use_module(library(emacs/emacs)).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(lists), [member/2, subtract/3, length/2]).
:- use_module(library(filesex), [directory_file_path/3]).

test_mixed_panes :-
    run_tests([ mixed_panes ]).

%       PceEmacs must not take the address of the PceEmacs of whoever
%       runs the tests, nor leave one behind.

emacs :-
    (   object(@emacs)
    ->  true
    ;   set_prolog_flag(emacs_server, false),
        start_emacs
    ).

%!  classes(+Frame, -Classes) is det.

classes(F, Classes) :-
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    findall(C, (member(P, Panes), get(P, class_name, C)), Classes).

%!  menus(+Frame, -Names) is det.
%
%   The menus on the bar, in the order they are drawn.

menus(F, Names) :-
    get(F, menu_bar, MB),
    get(MB, buttons, Chain),
    chain_list(Chain, Buttons),
    findall(N, (member(B, Buttons), get(B, name, N)), Names).

%!  editor(+Frame, -View) is det.
%!  terminal(+Frame, -Window) is det.

editor(F, V) :-
    get(F, panes, Chain), chain_list(Chain, Panes),
    member(V, Panes), send(V, instance_of, emacs_view), !.

terminal(F, W) :-
    get(F, panes, Chain), chain_list(Chain, Panes),
    member(W, Panes), send(W, instance_of, epilog_window), !.


%       The warning the search used to give -- <-text_buffer sent to a
%       terminal -- costs nothing but noise, so no outcome betrays it.  A
%       pane that says when it is asked does.

:- dynamic asked/1.

%!  source_of_our_own(-File) is det.
%
%   A Prolog file of its own to open, with a body indented by four, so
%   that PceEmacs works the layout out and says so.

:- dynamic
    source_count/1.

source_of_our_own(File) :-
    (   retract(source_count(N0))
    ->  N is N0+1
    ;   N = 1
    ),
    assertz(source_count(N)),
    current_prolog_flag(tmp_dir, Tmp),
    format(atom(Base), 'test_mixed_source_~d.pl', [N]),
    directory_file_path(Tmp, Base, File),
    setup_call_cleanup(
        open(File, write, Out),
        format(Out, 'answer(X) :-~n    X = 42.~n', []),
        close(Out)).

%!  with_placement(+Placement, :Goal) is semidet.
%
%   Run Goal with the IDE set to open new things in Placement.

:- meta_predicate with_placement(+, 0).

with_placement(Placement, Goal) :-
    get(@pce, convert, prolog_ide, class, Class),
    get(Class, class_variable, tool_placement, Var),
    get(Var, value, Old),
    setup_call_cleanup(
        send(Class, class_variable_value, tool_placement, Placement),
        Goal,
        send(Class, class_variable_value, tool_placement, Old)).

:- pce_begin_class(nosy_pane, window,
                   "A pane that records being asked for a buffer").

text_buffer(P, TB:text_buffer) :<-
    "I have none, but I remember the question"::
    assertz(test_mixed_panes:asked(P)),
    fail,
    TB = @nil.                          % never reached; types the method

:- pce_end_class(nosy_pane).


:- begin_tests(mixed_panes).

no_frames :-
    get(@prolog_ide, members, Members),
    chain_list(Members, List),
    forall(( member(F, List),
             send(F, instance_of, pane_frame)
           ),
           send(F, destroy)).

test(an_epilog_window_takes_an_editor, Classes == [epilog_window, emacs_view]) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F),
    classes(F, Classes).

test(a_pcemacs_window_takes_a_terminal, Classes == [emacs_view, epilog_window]) :-
    emacs,
    new(B, emacs_buffer(@nil, '*mixed-1*')),
    get(@emacs, frame, B, F),
    send(@prolog_ide, new_terminal, F),
    classes(F, Classes).

%       Both panes are dragged by a grip of their own.  An editor
%       displays one wherever it is, and inside a tool it hides it (see
%       `split_handle ->update_displayed'); as a pane of a window it is
%       the thing that moves, so it shows it.

test(both_panes_show_the_grip_they_are_dragged_by,
     true(Shown == [@on, @on])) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F),
    send(F, resize),
    editor(F, V),
    terminal(F, T),
    findall(Displayed,
            ( member(W, [V, T]),
              get(W, fixed_graphicals, Graphicals),
              get(Graphicals, find,
                  message(@arg1, instance_of, split_handle), Handle),
              send(Handle, compute),
              get(Handle, displayed, Displayed)
            ),
            Shown).

%       A source the user asks to see -- edit/1 -- goes in the window
%       they are in, even when that window holds no editor: a console can
%       take a tab or a split like any other.  It used to want a window
%       with an editor in it and made one of its own when there was none.

test(a_source_opens_in_the_console_the_user_is_in,
     Classes == [epilog_window, emacs_view]) :-
    no_frames,
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    source_of_our_own(File),
    with_placement(tab, send(@emacs, goto_source_location,
                             source_location(File, 1))),
    classes(F, Classes).

test(and_beside_the_terminal_when_that_is_the_setting,
     true(Tabs-Panes == 1-2)) :-
    no_frames,
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    source_of_our_own(File),
    with_placement(split, send(@emacs, goto_source_location,
                               source_location(File, 1))),
    get(F?tabs?tabs, size, Tabs),
    classes(F, Cs),
    length(Cs, Panes).

%       The layout PceEmacs works out from the file is a remark: it goes
%       on the status bar of the window when there is one to say it on,
%       and is dropped when there is not -- a view is made before the
%       window it goes in.  As `inform' it was a message box that had to
%       be clicked away before the file appeared, and this test hung.

test(what_it_makes_of_the_layout_does_not_stop_for_an_answer,
     true(Indentation-Tabs == 4-(@off))) :-
    no_frames,
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    source_of_our_own(File),
    with_placement(tab, send(@emacs, goto_source_location,
                             source_location(File, 1))),
    editor(F, View),
    get(View, mode, Mode),
    get(Mode, body_indentation, Indentation),
    get(View?editor, indent_tabs, Tabs).

%       Where the user asked from is remembered, so that `Back' returns
%       to it.  Where they were is an editor, and the pane they are in
%       need not be one: a terminal has no editor to remember a place in,
%       nor has a tool of the IDE, and asking one for it lost the place
%       -- and said so.  ->open_file works from the same view.
%
%       The one place remembered is the caret the user left: arriving at
%       the head of a file is not worth remembering -- see `emacs_mode
%       ->history_not_interesting'.

test(where_a_source_was_asked_for_from_is_remembered,
     true(Places == 1)) :-
    no_frames,
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    source_with_room(Asking),
    with_placement(tab, send(@emacs, goto_source_location,
                             source_location(Asking, 1))),
    editor(F, View),
    get(View, editor, Editor),           % away from the place arriving
    get(Editor, scan, 0, line, 60, start, Caret),  % there remembered
    send(Editor, caret, Caret),
    terminal(F, T),
    send(F, current_pane, T),            % the pane the user is in is not
    source_of_our_own(Asked),            % the one that holds the source
    with_placement(tab, send(@emacs, goto_source_location,
                             source_location(Asked, 1))),
    history_places(Asking, Places).

%!  source_with_room(-File) is det.
%
%   A source long enough to move the caret away in: a place close to the
%   last one remembered is not worth remembering again.

source_with_room(File) :-
    source_of_our_own(File),
    setup_call_cleanup(
        open(File, write, Out),
        forall(between(1, 60, I),
               format(Out, 'answer(~d, X) :-~n    X = ~d.~n~n', [I, I])),
        close(Out)).

%!  history_places(+File, -Count) is det.
%
%   How many places PceEmacs remembers in File.

history_places(File, Count) :-
    get(@emacs?history, backward_list, Chain),
    chain_list(Chain, Entries),
    findall(HE,
            ( member(HE, Entries),
              get(HE, get_hyper, fragment, text_buffer, TB),
              get(TB, file, PceFile),
              PceFile \== @nil,
              get(PceFile, name, File)
            ),
            Ours),
    length(Ours, Count).

test(the_mode_menus_come_and_go_with_the_editor) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    menus(F, WithTerminal),
    send(@prolog_ide, new_editor, F),
    menus(F, WithEditor),
    subtract(WithEditor, WithTerminal, Added),
    Added \== [],                        % the mode brought menus of its own
    terminal(F, T),
    send(F, current_pane, T),
    menus(F, Back),
    Back == WithTerminal.                % and took them away again

%       What the window offers, the mode does not offer again: editing
%       breakpoints and exceptions and viewing the threads and the debug
%       messages are on the Tools menu of every window of the IDE, and an
%       editor is a pane of one.

test(the_mode_leaves_the_ide_tools_to_the_window,
     [ forall(member(Item, [edit_breakpoints, edit_exceptions,
                            view_threads, view_debug_messages])),
       fail
     ]) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F),
    get(F, menu_bar, MB),
    get(MB, member, prolog, Popup),
    get(Popup, member, Item, _).

test(and_the_window_offers_two_of_them_on_its_tools_menu) :-
    epilog_frame(@default, @default, @default, @off, @default, F),
    get(F, menu_bar, MB),
    get(MB, member, tools, Tools),
    get(Tools, member, edit_breakpoints, _),
    get(Tools, member, edit_exceptions, _).

test(a_terminal_carries_its_own_menus_into_a_pcemacs_window) :-
    emacs,
    new(B, emacs_buffer(@nil, '*mixed-2*')),
    get(@emacs, frame, B, F),
    send(@prolog_ide, new_terminal, F),
    terminal(F, T),
    send(F, current_pane, T),
    menus(F, Menus),
    memberchk(debug, Menus).            % the Debug menu is the terminal's

test(both_kinds_of_pane_side_by_side_in_one_tab, true(N == 2)) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F, @on),  % split, rather than a tab of its own
    editor(F, V),
    get(V, container, tab_frame, Tab),
    get(Tab?windows, size, N).

test(the_menu_bar_follows_the_focus_inside_one_tab) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F, @on),
    editor(F, V),
    terminal(F, T),
    send(F, keyboard_focus, V),
    menus(F, WithEditor),
    send(F, keyboard_focus, T),
    menus(F, WithTerminal),
    WithEditor \== WithTerminal.

%       The title is made out of the label of the tab in view and a
%       format.  The pane the user is working in may say what that format
%       is, so an editor still names its window PceEmacs's wherever it
%       sits, and a terminal leaves the name to the application.

test(an_editor_names_the_window_it_is_in, true(Label == Expected)) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F),
    get(F, tab_label, TabLabel),
    get(string('PceEmacs -- %s', TabLabel), value, Expected),
    get(F, label, Label).

test(and_a_terminal_leaves_it_to_the_application, true(Label == Expected)) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F),
    terminal(F, T),
    send(F, current_pane, T),
    get(F, tab_label, TabLabel),
    get(string('SWI-Prolog -- %s', TabLabel), value, Expected),
    get(F, label, Label).

%       The point of one application for the whole IDE: dropping a
%       terminal onto an editor and dropping an editor onto a terminal
%       must leave the same window.

test(the_two_ways_round_give_the_same_menus,
     true(FromTerminal == FromEditor)) :-
    emacs,
    %  a window that started as a terminal and gained an editor
    epilog_frame(@default, @default, @default, @off, @default, F1),
    send(@prolog_ide, new_editor, F1),
    %  and one that started as an editor and gained a terminal
    new(B, emacs_buffer(@nil, '*symmetry*')),
    get(@emacs, frame, B, F2),
    send(@prolog_ide, new_terminal, F2),
    %  compared with the terminal current in both
    terminal(F1, T1), send(F1, current_pane, T1), menus(F1, FromTerminal),
    terminal(F2, T2), send(F2, current_pane, T2), menus(F2, FromEditor).

%       Both editors are made the same way, so that the mode menus they
%       bring are the same and any difference left is the window's.

test(and_the_same_menus_with_the_editor_current,
     true(FromTerminal == FromEditor)) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F1),
    send(@prolog_ide, new_editor, F1),
    get(F1, current_pane, V1),
    new(B, emacs_buffer(@nil, '*symmetry-2*')),
    get(@emacs, frame, B, F2),
    send(@prolog_ide, new_editor, F2),
    get(F2, current_pane, V2),
    send(F1, current_pane, V1), menus(F1, FromTerminal),
    send(F2, current_pane, V2), menus(F2, FromEditor).

test(a_window_that_started_as_a_terminal_grows_a_bar_for_its_editor,
     true(Bar == yes)) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    \+ get(F, status_dialog, _),        % a window of terminals has none
    send(@prolog_ide, new_editor, F),
    send(F, show_line_number, 3),       % what an editor does as you type
    (   get(F, status_dialog, _) ->  Bar = yes ;  Bar = no ).

%       Every window of the IDE belongs to @prolog_ide, so being a member
%       no longer says a window is one PceEmacs can put a buffer in.
%       Holding an editor does.

test(a_buffer_is_not_shown_in_a_window_of_terminals, true(Landed == own)) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, FT),
    new(B, emacs_buffer(@nil, '*not-here*')),
    get(B, open, tab, V),
    get(V, frame, F),
    (   F == FT ->  Landed = terminal_window ;  Landed = own ).

test(and_a_window_of_terminals_is_not_the_current_frame, [fail]) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, FT),
    get(@emacs, current_frame, FT).

%       Opening a buffer in a window that holds a terminal as well as an
%       editor.  Each of the three routes has to pick the editor out of
%       the panes rather than take the first one it finds.

test(a_buffer_opens_in_a_window_that_also_holds_a_terminal,
     true(Landed == same_window)) :-
    emacs,
    mixed_window(F),
    new(B, emacs_buffer(@nil, '*mixed-open*')),
    get(B, open, tab, V),
    get(V, frame, In),
    (   In == F ->  Landed = same_window ;  Landed = elsewhere ).

test(and_asking_twice_goes_back_to_the_view_it_made, true(Views == 1)) :-
    emacs,
    mixed_window(F),
    new(B, emacs_buffer(@nil, '*mixed-twice*')),
    get(B, open, tab, V),
    get(B, open, tab, V),                % the same view, in the same frame
    get(V, frame, F),
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    aggregate_all(count,
                  ( member(P, Panes),
                    send(P, instance_of, emacs_view),
                    get(P, text_buffer, TB),
                    TB == B
                  ),
                  Views).

test(and_here_uses_the_editor_even_from_the_terminal, true(TB == B)) :-
    emacs,
    mixed_window(F),
    terminal(F, T),
    send(F, current_pane, T),           % the terminal has the focus
    new(B, emacs_buffer(@nil, '*mixed-here*')),
    send(@emacs, show_buffer, F, B, here),
    editor(F, V),
    get(V, text_buffer, TB).

test(only_editors_are_asked_which_buffer_they_hold, true(Asked == [])) :-
    emacs,
    mixed_window(F),
    send(F, append_pane, new(nosy_pane), nosy, @off),
    retractall(test_mixed_panes:asked(_)),
    new(B, emacs_buffer(@nil, '*mixed-nosy*')),
    send(@emacs, show_buffer, F, B, tab),
    findall(P, test_mixed_panes:asked(P), Asked).

%       Dropping a pane onto another gives it the focus: that is where
%       the mouse is.  The pane it came from has to lose it in the same
%       breath.  ->input_focus is edge-triggered, so a pane left switched
%       on can never be switched on again -- what the user sees is that
%       clicking either pane does nothing and only leaving the application
%       and coming back repairs it.  That symptom is a ws_enable_text_input
%       that is never re-issued and cannot be seen from here; that exactly
%       one pane holds the focus is the invariant behind it.

test(a_dropped_pane_takes_the_focus, true(Focused == [V])) :-
    emacs,
    tabbed_window_pair(F, _T, V),
    send(F, input_focus, @on),          % as if the window manager had
    drop_onto(F, V),
    focused(F, Focused).

test(and_the_focus_follows_a_click_afterwards, true(Focused == [T])) :-
    emacs,
    tabbed_window_pair(F, T, V),
    send(F, input_focus, @on),
    drop_onto(F, V),
    ignore(send(T, post_event, event(ms_left_down, T, 20, 20))),
    ignore(send(T, post_event, event(ms_left_up, T, 20, 20))),
    focused(F, Focused).

%       edit/1 in a window where an editor sits beside a terminal.  The
%       buffer opens in a tab of its own, and that tab has to stay in
%       front: the terminal is told it has the keyboard as its own tab
%       goes away, and used to answer by pulling it back.

test(edit_opens_a_tab_and_it_stays_in_front, true(Pane == view)) :-
    emacs,
    mixed_window(F),
    terminal(F, T),
    send(F, current_pane, T),
    new(B, emacs_buffer(@nil, '*edit*')),
    send(@emacs, show_buffer, F, B, tab),
    get(F, current_pane, Current),
    (   send(Current, instance_of, emacs_view)
    ->  Pane = view
    ;   Pane = terminal
    ).

test(and_the_editor_it_opens_is_the_one_it_asked_for, true(TB == B)) :-
    emacs,
    mixed_window(F),
    terminal(F, T),
    send(F, current_pane, T),
    new(B, emacs_buffer(@nil, '*edit2*')),
    send(@emacs, show_buffer, F, B, tab),
    get(F, current_pane, View),
    get(View, text_buffer, TB).

%!  tabbed_window_pair(-Frame, -Terminal, -View) is det.
%
%   A window with a terminal and an editor in tabs of their own, the
%   terminal current.  This is `swipl-win' plus edit/1.

tabbed_window_pair(F, T, V) :-
    no_frames,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(F, open),
    new(B, emacs_buffer(@nil, '*dropped*')),
    send(@emacs, show_buffer, F, B, tab),
    editor(F, V),
    terminal(F, T),
    send(F, current_pane, T).

%!  drop_onto(+Frame, +Window) is det.
%
%   Drag Window onto the bottom half of the pane that is current.

drop_onto(F, Window) :-
    get(F, current_pane, Target),
    get(Target, container, tab_frame, Tab),
    get(Target, size, size(W, H)),
    X is W//2,
    Y is H-10,
    send(Tab, drop, Window, point(X, Y)).

%!  focused(+Frame, -Panes) is det.
%
%   The panes of Frame that hold the keyboard focus.  Exactly one should.

focused(F, Panes) :-
    get(F, panes, Chain),
    chain_list(Chain, All),
    findall(P,
            ( member(P, All),
              get(P, input_focus, @on)
            ),
            Panes).

%!  mixed_window(-Frame) is det.
%
%   A window with a terminal and an editor below it.

mixed_window(F) :-
    no_frames,                          % it is the window PceEmacs finds
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F, @on),
    send(F, open).

:- end_tests(mixed_panes).
