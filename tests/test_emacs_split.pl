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

:- module(test_emacs_split, [test_emacs_split/0]).
:- encoding(utf8).

/** <module> Tests for splitting windows in PceEmacs

An Emacs tab holds its views in a tab_frame (see library(tab_frame)), so
that a tab can show more than one view.  These test the window commands
over those views and the rule that the label of the tab and of the frame
follows the view that has the focus.

Run with:

    swipl -g test_emacs_split -t halt \
          packages/xpce/tests/test_emacs_split.pl
*/

%  Both flags are set before PceEmacs is loaded: the driver is picked
%  when the display is initialised and the server when @emacs is made.
%  A test may not take the server address of the PceEmacs of whoever
%  runs it, nor leave one behind.

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- set_prolog_flag(emacs_server, false).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_emacs)).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(swi_ide), []).
:- use_module(library(lists), [member/2, length/2]).

test_emacs_split :-
    run_tests([ emacs_menu,
                emacs_tabs,
                emacs_split,
                emacs_move,
                emacs_labels,
                emacs_placement
              ]).

                 /*******************************
                 *            HELPERS           *
                 *******************************/

:- dynamic buffer_count/1.

%!  scratch(-Buffer) is det.
%
%   A buffer of its own, named apart from every other one: the buffers
%   of a run share @emacs_buffers.

scratch(B) :-
    (   retract(buffer_count(N0))
    ->  N is N0+1
    ;   N = 1
    ),
    assertz(buffer_count(N)),
    format(atom(Name), '*test-~d*', [N]),
    new(B, emacs_buffer(@nil, Name)).

%!  emacs(-Frame, -View) is det.
%
%   A PceEmacs frame of its own, holding one tab with one view.  The
%   frame is leaked: destroying it is unreliable on dummy-SDL.

emacs(F, V) :-
    start_emacs,
    scratch(B),
    get(@emacs, frame, B, F),
    get(F, current_pane, V).

%!  views(+Frame, -Views) is det.
%
%   The views of the tab that has the focus, in layout order.

views(F, Views) :-
    get(F, current_pane, V),
    get(V, container, tab_frame, TF),
    get(TF, windows, Chain),
    chain_list(Chain, Views).

%!  frame_label(+Frame, +TabLabel) is semidet.
%
%   True when the title of Frame is what @emacs makes of TabLabel.  The
%   frame composes its title out of the label of the tab in view and
%   <-label_format of its application; see library(pane_frame).

frame_label(F, TabLabel) :-
    get(F, current_pane, Pane),
    get(Pane, title_format, Format),    % an editor names its own window
    get(F, label, Label),
    get(string(Format, TabLabel), value, Expected),
    Label == Expected.

%!  tabs(+Frame, -Count) is det.

tabs(F, Count) :-
    get(F, tabs, TW),
    get(TW?tabs, size, Count).

%!  split_orientation(+View, -Orientation) is semidet.
%
%   How the tile holding View divides its space.  A view that has been
%   split off below its neighbour sits in a vertical tile, one split off
%   to the right in a horizontal one.

split_orientation(V, Orientation) :-
    get(V, tile, T),
    get(T, super, Super),
    Super \== @nil,
    get(Super, orientation, Orientation).

mode(F, M) :-
    get(F?current_pane, mode, M).

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


%       A menu item is chosen in the menu bar, not in the editor, so
%       @emacs_mode cannot be the mode of the window the event came from.

:- begin_tests(emacs_menu).

test(the_current_mode_is_found_with_no_event_at_all, Name == fundamental) :-
    emacs(_F, _V),
    get(@emacs_mode, name, Name).

test(a_pullright_fills_itself, true(Entries > 0)) :-
    emacs(F, _V),
    get(F, menu_dialog, MD),
    get(MD, menu_bar, @on, MB),
    get(MB, member, file, File),
    get(File, member, switch_to_buffer, Item),
    get(Item, popup, Popup),
    send(Popup, update, MB),
    get(Popup?members, size, Entries).

:- end_tests(emacs_menu).


:- begin_tests(emacs_tabs).

test(a_tab_holds_its_views_in_a_tab_frame) :-
    emacs(_F, V),
    get(V, container, tab_frame, _).

test(a_new_tab_is_a_tab_of_its_own) :-
    emacs(F, _V),
    tabs(F, 1),
    scratch(B),
    send(@emacs, show_buffer, F, B, tab),
    tabs(F, 2),
    views(F, [_]).

:- end_tests(emacs_tabs).


:- begin_tests(emacs_split).

test(split_window_adds_a_view_below) :-
    emacs(F, V),
    mode(F, M),
    send(M, split_window),
    views(F, Views),
    Views = [_,_],
    split_orientation(V, vertical).

test(split_window_right_adds_a_view_beside) :-
    emacs(F, V),
    mode(F, M),
    send(M, split_window_right),
    views(F, Views),
    Views = [_,_],
    split_orientation(V, horizontal).

test(splitting_keeps_the_focus_and_the_tab) :-
    emacs(F, V),
    mode(F, M),
    send(M, split_window),
    tabs(F, 1),
    get(F, current_pane, V).                    % point stays where it was

test(other_window_moves_the_focus_and_wraps) :-
    emacs(F, V1),
    mode(F, M),
    send(M, split_window),
    views(F, [V1, V2]),
    send(M, other_window),
    get(F, current_pane, V2),
    mode(F, M2),
    send(M2, other_window),
    get(F, current_pane, V1).

test(delete_window_leaves_the_others) :-
    emacs(F, V1),
    mode(F, M),
    send(M, split_window),
    views(F, [V1, V2]),
    send(M, delete_window),
    views(F, [V2]),
    get(F, current_pane, V2).                   % the focus went with it

test(only_window_leaves_just_this_one) :-
    emacs(F, V1),
    mode(F, M),
    send(M, split_window),
    mode(F, M2),
    send(M2, split_window_right),
    views(F, [_,_,_]),
    get(F, current_pane, V1),
    mode(F, M3),
    send(M3, only_window),
    views(F, [V1]).

test(the_only_view_of_a_tab_is_kept) :-
    emacs(F, V),
    mode(F, M),
    send(M, delete_window),
    views(F, [V]),
    mode(F, M2),
    send(M2, only_window),
    views(F, [V]).

test(split_shows_the_buffer_beside_the_view) :-
    emacs(F, V1),
    scratch(B),
    send(@emacs, show_buffer, F, B, split),
    views(F, [V1, V2]),
    tabs(F, 1),
    get(F, current_pane, V2),                   % the new view has the focus
    get(V2, text_buffer, B).

test(a_buffer_asked_for_opens_a_tab_when_not_split) :-
    emacs(F, V),
    scratch(B),
    mode(F, M),
    send(M, show_buffer, B),            % what C-x C-f and C-x b do
    tabs(F, 2),
    views(F, [_]),
    get(V, text_buffer, B0),
    B0 \== B.                           % the old view kept its buffer

test(a_buffer_asked_for_reuses_the_pane_when_split) :-
    emacs(F, V1),
    mode(F, M),
    send(M, split_window),
    views(F, [V1, V2]),
    scratch(B),
    get(B, name, Name),
    mode(F, M2),
    send(M2, show_buffer, B),
    tabs(F, 1),                         % no tab of its own
    views(F, [V1, V2]),                 % and no view of its own
    get(F, current_pane, V1),
    get(V1, text_buffer, B),
    frame_label(F, Name).               % the label came along

test(open_split_splits_the_current_frame) :-
    emacs(_F, _V),                      % <-open picks <-current_frame,
    scratch(B),                         % which need not be that one
    get(B, open, split, V),
    get(V, text_buffer, B),
    get(V, container, tab_frame, TF),
    get(TF?windows, size, N),
    N >= 2.

:- end_tests(emacs_split).


:- begin_tests(emacs_move).

%   A view carries the grip of library(tab_frame), so it can be taken to
%   another tab or another PceEmacs window.  The editor fills the view, so
%   the grip is drawn over it, clear of the scrollbar.

%   A buffer names its own tab, so PceEmacs does not offer to rename it.

test(a_view_tab_carries_a_close_button) :-
    emacs(F, V),
    scratch(B),
    send(@emacs, show_buffer, F, B, tab),
    get(V, container, tab_frame, Tab),
    get(Tab, closable, @on),
    get(Tab, hypered, close_button, Button),
    get(Button, area, area(X, Y, BW, BH)),
    CX is X+BW//2,
    CY is Y+BH//2,
    get(Button?device, window, Window),
    send(event(ms_left_down, Window, CX, CY), post, Button),
    send(event(ms_left_up, Window, CX, CY), post, Button),
    \+ object(Tab),
    tabs(F, 1).

test(a_view_tab_is_not_renamed_by_hand) :-
    emacs(_F, V),
    get(V, container, tab_frame, Tab),
    get(Tab, editable_label, @off),
    \+ send(Tab, edit_label).

test(a_view_carries_a_grip) :-
    emacs(_F, V),
    get(V, member, split_handle, H),
    get(H, help_message, tag, Tag),
    get(Tag, size, Len),
    Len > 0.

test(a_view_moves_to_another_tab) :-
    emacs(F, V1),
    scratch(B),
    send(@emacs, show_buffer, F, B, tab),
    get(F, current_pane, V2),
    V2 \== V1,
    get(V2, container, tab_frame, Tab),
    at_edge(V2, right, Pos),
    send(Tab, drop, V1, Pos),
    get(Tab?windows, size, 2),
    send(Tab?windows, member, V1),
    tabs(F, 1).                         % the tab it left was emptied

test(a_view_moves_to_another_window) :-
    emacs(F1, V1),
    emacs(_F2, V2),
    get(V1, container, tab_frame, Tab),
    at_edge(V1, right, Pos),
    send(Tab, drop, V2, Pos),
    get(Tab?windows, size, 2),
    get(V2, frame, F1).

%   ->drop makes what it dropped the current window of the tab, and
%   pane_frame ->keyboard_focus exposes a pane, so the frame follows it.

test(the_window_follows_the_view_that_arrives) :-
    emacs(F1, _V1),
    emacs(_F2, V2),
    get(V2, label, Label),
    get(F1, current_pane, Before),
    Before \== V2,
    get(Before, container, tab_frame, Tab),
    at_edge(Before, right, Pos),
    send(Tab, drop, V2, Pos),
    get(F1, current_pane, V2),
    frame_label(F1, Label).

:- end_tests(emacs_move).


:- begin_tests(emacs_labels).

test(the_label_follows_the_view_that_has_the_focus) :-
    emacs(F, V1),
    get(V1?text_buffer, name, Name1),
    mode(F, M),
    send(M, split_window),
    views(F, [V1, _V2]),
    send(M, other_window),              % focus the new view
    scratch(B2),
    get(B2, name, Name2),
    send(@emacs, show_buffer, F, B2, here),                % show another buffer there
    frame_label(F, Name2),
    get(V1, container, tab_frame, TF),
    get(TF, label, Name2),
    mode(F, M2),
    send(M2, other_window),             % and back
    frame_label(F, Name1),
    get(TF, label, Name1).

test(the_label_follows_a_view_that_goes_away) :-
    emacs(F, V1),
    mode(F, M),
    send(M, split_window),
    views(F, [V1, _V2]),
    send(M, other_window),
    scratch(B2),
    get(B2, name, Name2),
    send(@emacs, show_buffer, F, B2, here),
    frame_label(F, Name2),
    mode(F, M2),
    send(M2, delete_window),            % the view showing B2 goes
    get(V1?text_buffer, name, Name1),
    frame_label(F, Name1),
    get(V1, container, tab_frame, TF),
    get(TF, label, Name1).

%       Renaming a buffer used to put its name straight on the title of
%       every frame holding a view on it, which is the label of whatever
%       tab is in view now.

test(renaming_a_hidden_buffer_leaves_the_title_alone) :-
    emacs(F, V1),
    get(V1?text_buffer, name, Name1),
    scratch(B2),
    send(@emacs, show_buffer, F, B2, tab),
    send(F, current_pane, V1),          % B2 is in a tab that is not in view
    send(B2, name, '*renamed*'),
    frame_label(F, Name1),
    get(B2?editors, head, E2),
    get(E2, container, tab, Tab2),
    get(Tab2, label, '*renamed*').      % its own tab did follow

test(renaming_the_buffer_in_view_retitles_its_frame) :-
    emacs(F, V),
    get(V, text_buffer, B),
    send(B, name, '*retitled*'),
    frame_label(F, '*retitled*').

test(a_view_of_its_own_labels_its_frame) :-
    start_emacs,
    scratch(B),
    get(B, name, Name),
    get(B, open, window, V),
    get(V, frame, F),
    frame_label(F, Name),
    tabs(F, 1).

:- end_tests(emacs_labels).


                 /*******************************
                 *          PLACEMENT           *
                 *******************************/

/* Where edit/1 puts the file it opens.

It used to be a tab, always.  A window of the IDE says on its Settings
menu where new things are to go -- in a window of their own, in a tab or
beside what is there -- and a source the user asks to see goes by that,
like a tool.  `emacs/1' of library(pce_emacs), which is what edit/1 ends
up in, asks `prolog_ide <-source_placement'.
*/

:- begin_tests(emacs_placement).

test(the_setting_says_where_a_source_opens,
     true(Places == [window, tab, split])) :-
    findall(Where,
            ( member(Placement, [frame, tab, split]),
              with_placement(Placement, start_emacs:source_placement(Where))
            ),
            Places).

test(and_a_tab_is_what_it_asks_for_by_default,
     true(Where == tab)) :-
    start_emacs:source_placement(Where).

%       What each answer does with a source, which is `emacs_buffer
%       <-open's to do: edit/1 hands it the word and no more.

test(a_source_asked_for_in_a_tab_opens_in_one) :-
    emacs(_F, _V),
    get(@emacs, current_frame, Frame),  % which window is not this test's
    tabs(Frame, Tabs0),                 % business; see <-current_frame
    scratch(B),
    send(B, open, tab),
    tabs(Frame, Tabs),
    Tabs =:= Tabs0+1.

test(and_one_asked_for_beside_what_is_there_splits) :-
    emacs(_F, _V),
    get(@emacs, current_frame, Frame),
    tabs(Frame, Tabs0),
    views(Frame, Views0),
    length(Views0, Panes0),
    scratch(B),
    send(B, open, split),
    tabs(Frame, Tabs0),                 % beside what is there, not a tab
    views(Frame, Views),
    length(Views, Panes),
    Panes =:= Panes0+1.

test(and_one_asked_for_in_a_window_of_its_own_gets_one) :-
    emacs(F, _V),
    scratch(B),
    get(B, open, window, View),
    get(View, frame, Other),
    Other \== F.

:- end_tests(emacs_placement).

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
