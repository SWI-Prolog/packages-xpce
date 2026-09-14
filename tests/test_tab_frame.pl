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

:- module(test_tab_frame, [test_tab_frame/0]).
:- encoding(utf8).

/** <module> Tests for class tab_frame

A tab_frame is a tab that lays its windows out with a tile hierarchy, the
way class frame does for its members.   These tests check the layout, the
splitting and removal of windows and the drag-to-resize gesture.  None of
that needs pixels, so they run against the headless SDL driver.

Run with:

    swipl -g test_tab_frame -t halt \
          packages/xpce/tests/test_tab_frame.pl
*/

%  Set before library(pce) is loaded: the driver is picked when the
%  display is initialised, which loading xpce already does.

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(tabbed_window)).
:- use_module(library(tab_frame)).

test_tab_frame :-
    run_tests([ tab_frame_layout,
                tab_frame_single_tab,
                tab_frame_label,
                tab_frame_buttons,
                tab_frame_members,
                tab_frame_resize,
                tab_frame_manager,
                tab_frame_drop,
                tab_frame_window_tree,
                tab_frame_window_group
              ]).

                 /*******************************
                 *            HELPERS           *
                 *******************************/

%!  tabbed(-TabbedWindow, -TabFrame, -Window) is det.
%
%   An open tabbed window holding one tab_frame with one picture in it.
%   The frame is leaked: destroying it is unreliable on dummy-SDL.

tabbed(TW, TF, P) :-
    new(TW, tabbed_window('Test')),
    send(TW, tab, new(TF, tab_frame(new(P, picture), one))),
    send(TW, open),
    send(TW, resize).

%!  tabbed_at(+X, +Y, -TabbedWindow, -TabFrame, -Window) is det.
%
%   As tabbed/3, at a place of its own on the display, so that two of them
%   can be told apart.

tabbed_at(X, Y, TW, TF, P) :-
    new(TW, tabbed_window('Test', size(500,300))),
    send(TW, tab, new(TF, tab_frame(new(P, picture), one))),
    send(TW, open, point(X, Y)),
    send(TW, resize).

%!  geometry(+Window, -Area) is det.
%
%   Area of Window as it is placed by the tile.  A window that carries a
%   label or scrollbars is wrapped in a window_decorator and it is the
%   decorator that the tile positions.

geometry(W, area(X,Y,Width,Height)) :-
    (   get(W, decoration, D),
        D \== @nil
    ->  Decor = D
    ;   Decor = W
    ),
    get(Decor, area, area(X,Y,Width,Height)).

%!  two_tabs(-TabbedWindow, -Tab1, -Tab2) is det.
%
%   An open tabbed window with two tabs, so that both carry a label.

two_tabs(TW, TF1, TF2) :-
    new(TW, tabbed_window('Test', size(400,300))),
    send(TW, tab, new(TF1, tab_frame(new(_P1, picture), one))),
    send(TW, tab, new(TF2, tab_frame(new(_P2, picture), two))),
    send(TW, open),
    send(TW, resize).

%!  post_grabbed(+Window, +Id, +Tab, +X, +Y) is det.
%
%   Post an event to Window the way the window system delivers one while
%   Window holds the pointer grab: whatever frame it arrived on, it is
%   handed to the grabbing window, naming that frame and a position in it.
%   X,Y are in the content coordinates of Tab.

post_grabbed(W, Id, Tab, X, Y) :-
    get(Tab, display_position, point(TX, TY)),
    get(Tab, offset, point(OX, OY)),
    get(Tab, frame, Frame),
    get(Frame, area, area(FX, FY, _, _)),
    EX is TX+OX+X-FX,
    EY is TY+OY+Y-FY,
    new(Ev, event(Id, W, EX, EY)),
    send(Ev, slot, frame, Frame),
    ignore(send(W, post_event, Ev)).

%!  event_at(+TabFrame, +Id, +X, +Y, -Event) is det.
%
%   An event of type Id at X,Y in the content coordinates of TabFrame,
%   i.e. the coordinate system its windows are laid out in.

event_at(TF, Id, CX, CY, Ev) :-
    get(TF, area, area(AX,AY,_,_)),
    get(TF, offset, point(OX,OY)),
    get(TF, frame, Frame),
    get(Frame, member, tabbed_window, TW),
    X is AX+OX+CX,
    Y is AY+OY+CY,
    new(Ev, event(Id, TW, X, Y)).


:- begin_tests(tab_frame_layout).

test(single_window_fills_the_tab) :-
    tabbed(_TW, TF, P),
    get(TF, content_size, size(CW, CH)),
    geometry(P, area(X,Y,W,H)),
    get(TF?root_tile, border_root, B),
    X =:= B, Y =:= B,
    W =:= CW-2*B, H =:= CH-2*B.

%   A tab wants its windows out to its own edges, which is what
%   <-border_root is for: the border between the tiles stays.

%   ->border used to be the only border a tile had, and code that wants
%   none of it -- a popup frame, a balloon -- still says so that way.

test(setting_the_border_sets_the_one_around_the_root) :-
    tabbed(_TW, TF, _P),
    get(TF, root_tile, Tile),
    send(Tile, border, 7),
    get(Tile, border, 7),
    get(Tile, border_root, 7),
    send(Tile, border_root, 0),          % and this is what tells them apart
    get(Tile, border, 7),
    get(Tile, border_root, 0).

test(the_tab_asks_for_no_border_around_its_tile) :-
    tabbed(_TW, TF, P),
    get(TF?root_tile, border_root, 0),
    geometry(P, area(0, 0, CW, CH)),
    get(TF, content_size, size(CW, CH)).

test(the_border_survives_a_split_making_a_new_root) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(_P2, picture), P1, vertically),
    get(TF?tile, super, @nil),          % the split made a new root
    get(TF?root_tile, border_root, 0),
    geometry(P1, area(0, 0, _, _)).

test(the_border_survives_losing_a_window) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    send(TF, delete, P2),
    get(TF?root_tile, border_root, 0),
    geometry(P1, area(0, 0, CW, CH)),
    get(TF, content_size, size(CW, CH)).

test(vertical_split_divides_the_width) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    geometry(P1, area(X1,Y1,W1,_H1)),
    geometry(P2, area(X2,Y2,_W2,_H2)),
    Y1 =:= Y2,
    X2 > X1+W1.

test(horizontal_split_divides_the_height) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),
    geometry(P1, area(X1,Y1,_W1,H1)),
    geometry(P2, area(X2,Y2,_W2,_H2)),
    X1 =:= X2,
    Y2 > Y1+H1.

test(split_of_a_split_nests) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    send(TF, split, new(P3, picture), P2, horizontally),
    geometry(P1, area(_X1,_Y1,_W1,H1)),
    geometry(P2, area(X2,_Y2,_W2,H2)),
    geometry(P3, area(X3,_Y3,_W3,_H3)),
    X2 =:= X3,                          % P2 and P3 share the right column
    H2 < H1.                            % and split its height

test(separator_per_resizable_gap) :-
    tabbed(_TW, TF, P1),
    get(TF?separators, size, 0),
    send(TF, split, new(P2, picture), P1, vertically),
    get(TF?separators, size, 1),
    send(TF, split, new(_P3, picture), P2, horizontally),
    get(TF?separators, size, 2).

:- end_tests(tab_frame_layout).


:- begin_tests(tab_frame_single_tab).

%   A stack told to hide the label of a lone tab hands that tab the strip
%   the label would take, and takes it back as soon as there is a second
%   tab to tell it from.

single(TW, TF, P) :-
    new(TW, tabbed_window('Test', size(400,300))),
    send(TW, hide_single_label, @on),
    send(TW, tab, new(TF, tab_frame(new(P, picture), one))),
    send(TW, open),
    send(TW, resize).

test(a_lone_tab_keeps_its_label_by_default) :-
    tabbed(TW, TF, _P),
    get(TW, hide_single_label, @off),
    get(TF, label_height, LH),
    LH > 0.

test(a_lone_tab_can_drop_its_label) :-
    single(_TW, TF, _P),
    get(TF, label_height, 0).

test(the_content_gets_the_room_the_label_would_take) :-
    single(TW, TF, _P),
    get(TF, content_size, size(_, H1)),
    send(TW, tab, new(TF2, tab_frame(new(_P2, picture), two))),
    send(TW, resize),
    get(TF, label_height, LH),
    LH > 0,
    get(TF, content_size, size(_, H2)),
    H1 =:= H2+LH,                       % the label is back and paid for
    send(TF2, destroy),
    send(TW, resize),
    get(TF, label_height, 0),
    get(TF, content_size, size(_, H3)),
    H3 =:= H1.

test(the_window_fills_the_tab_either_way) :-
    single(TW, TF, P),
    get(TF, content_size, size(CW, CH)),
    geometry(P, area(0, 0, CW, CH)),
    send(TW, tab, new(_TF2, tab_frame(new(_P2, picture), two))),
    send(TW, resize),
    get(TF, content_size, size(CW2, CH2)),
    geometry(P, area(0, 0, CW2, CH2)).

test(the_tab_sits_at_the_top_either_way) :-
    single(TW, TF, _P),
    get(TF, area, area(_, 0, _, _)),
    send(TW, tab, new(_TF2, tab_frame(new(_P2, picture), two))),
    send(TW, resize),
    get(TF, area, area(_, 0, _, _)).

test(turning_it_on_later_takes_effect) :-
    tabbed(TW, TF, _P),
    get(TF, label_height, LH),
    LH > 0,
    send(TW, hide_single_label, @on),
    get(TF, label_height, 0),
    send(TW, hide_single_label, @off),
    get(TF, label_height, LH).

:- end_tests(tab_frame_single_tab).


:- begin_tests(tab_frame_label).

%   The label of a tab is drawn by the tab, not by a graphical of its own,
%   so what can be done to it answers on class tab.  Editing it is off
%   unless the tab says otherwise.

editor(TF, Item) :-
    get(TF, device, Stack),
    get(Stack, member, tab_label_item, Item).

%!  label_click(+TabbedWindow, +Tab, -Event) is det.
%
%   A left click in the middle of the label of Tab.  A label is drawn
%   above the tab, so it lies at a negative y.

label_click(TW, TF, Ev) :-
    label_event(TW, TF, ms_left_down, 10, Ev).

%!  label_event(+TabbedWindow, +Tab, +Kind, +Into, -Event) is det.
%
%   An event Into pixels along Tab's label.  `postNamedEvent()' hands
%   ->label_event the tab as <-receiver, so say so here as well.

label_event(TW, TF, Kind, Into, Ev) :-
    get(TF, label_offset, X),
    get(TF, label_height, H),
    EX is X+Into,
    EY is -(H//2),
    new(Ev, event(Kind, TW, EX, EY)),
    send(Ev, slot, receiver, TF).

test(a_label_is_not_editable_unless_asked) :-
    two_tabs(_TW, TF, _TF2),
    get(TF, editable_label, @off),
    \+ send(TF, edit_label).

test(an_editable_label_opens_over_itself) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    send(TF, edit_label),
    editor(TF, Item),
    get(Item, selection, Label),
    send(Label, equal, TF?label),
    get(TF, label_offset, X),
    get(TF, label_height, H),
    get(TF?label_size, width, W),
    get(Item, area, area(X, 0, IW, IH)),
    IH >= H,
    IW >= W.                            % a label is narrow to type in

%       The name a tab has is what you are usually replacing, not what you
%       want to edit a letter of, so it starts out selected.

test(and_starts_out_selected_so_typing_replaces_the_name) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    send(TF, edit_label),
    editor(TF, Item),
    send(Item, typed, 0'x),
    get(Item, selection, Typed),
    send(Typed, equal, x).

%       Room to type in, which is more than the label takes to draw --
%       there is a whole name to put there.

test(and_is_wider_than_the_label) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    send(TF, edit_label),
    editor(TF, Item),
    get(TF?label_size, width, W),
    get(Item?area, width, IW),
    IW > W.

test(but_not_wider_than_the_row_it_is_in) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    send(TF, edit_label),
    editor(TF, Item),
    get(TF?device?area, width, RowW),
    get(TF, label_offset, X),
    get(Item?area, width, IW),
    X+IW =< RowW.

%       Escape and Return arrive as names from the window system and as
%       character codes from a program.  Both have to work, or Escape does
%       nothing where it counts.

test(escape_by_name_leaves_the_label_alone) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    get(TF, label, Was),
    send(TF, edit_label),
    editor(TF, Item),
    send(Item, selection, nope),
    send(Item, typed, 'ESC'),
    \+ editor(TF, _),
    get(TF, label, Was).

test(return_that_changes_nothing_closes_the_editor) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    get(TF, label, Was),
    send(TF, edit_label),
    editor(TF, Item),
    send(Item, typed, 'RET'),
    \+ editor(TF, _),
    get(TF, label, Was).

test(while_return_after_typing_takes_what_was_typed) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    send(TF, edit_label),
    editor(TF, Item),
    send(Item, selection, renamed),
    send(Item, modified, @on),
    send(Item, typed, 'RET'),
    \+ editor(TF, _),
    get(TF, label, renamed).

test(what_is_typed_becomes_the_label) :-
    two_tabs(_TW, TF, TF2),
    send(TF, editable_label, @on),
    send(TF, edit_label),
    editor(TF, Item),
    send(Item, selection, renamed),
    send(Item, execute),
    get(TF, label, renamed),
    \+ editor(TF, _),
    get(TF2, label_offset, X2),          % the labels were laid out again
    get(TF?label_size, width, W1),
    X2 =:= W1.

test(an_empty_name_is_no_name) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    get(TF, label, Was),
    send(TF, edit_label),
    editor(TF, Item),
    send(Item, selection, ''),
    send(Item, execute),
    get(TF, label, Was).

test(escape_leaves_the_label_alone) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, editable_label, @on),
    get(TF, label, Was),
    send(TF, edit_label),
    editor(TF, Item),
    send(Item, selection, nope),
    send(Item, typed, 27),
    \+ editor(TF, _),
    get(TF, label, Was).

%       The editor takes the frame's focus from the window that had it,
%       so that one stops showing a cursor, and gives it back when done.

test(editing_a_label_takes_the_focus_and_gives_it_back) :-
    two_tabs(TW, TF, _TF2),
    send(TF, editable_label, @on),
    get(TW, frame, Frame),
    get(TF, current, P),
    send(Frame, keyboard_focus, P),
    send(TF, edit_label),
    get(Frame, keyboard_focus, TW),
    editor(TF, Item),
    send(Item, typed, 'ESC'),
    get(Frame, keyboard_focus, P).

test(and_after_renaming_as_well) :-
    two_tabs(TW, TF, _TF2),
    send(TF, editable_label, @on),
    get(TW, frame, Frame),
    get(TF, current, P),
    send(Frame, keyboard_focus, P),
    send(TF, edit_label),
    editor(TF, Item),
    send(Item, selection, renamed),
    send(Item, execute),
    get(Frame, keyboard_focus, P).

%   ->label_event has to raise the tab itself: class tab defines it, so
%   the one in library(tabbed_window) replaces it rather than adding to it.

test(a_click_on_a_label_raises_its_tab) :-
    two_tabs(TW, TF1, TF2),
    get(TF1, status, on_top),           % the first one appended
    label_click(TW, TF2, Ev),
    send(TF2, label_event, Ev),
    get(TF2, status, on_top),
    get(TF1, status, hidden).

test(a_click_on_the_label_of_an_inactive_tab_does_nothing) :-
    two_tabs(TW, TF1, TF2),
    send(TF2, active, @off),
    label_click(TW, TF2, Ev),
    \+ send(TF2, label_event, Ev),
    get(TF1, status, on_top).

test(a_double_click_on_the_label_starts_an_edit) :-
    two_tabs(TW, TF, _TF2),
    send(TF, editable_label, @on),
    get(TF, label_offset, X),
    get(TF, label_height, H),
    EX is X+10,
    EY is -(H//2),
    new(_First, event(ms_left_down, TW, EX, EY)),
    new(Ev, event(ms_left_down, TW, EX, EY)),
    get(Ev, multiclick, double),
    send(TF, label_event, Ev),
    editor(TF, _).

%   ->label_event and <-label_popup used to sit on window_tab, which left
%   a tab_frame without either.

test(a_tab_frame_reaches_the_label_popup) :-
    two_tabs(TW, TF, _TF2),
    send(TW, label_popup, new(P, popup)),
    get(TF, label_popup, P).

%       Dragging a label puts the tab somewhere else in the row.  The
%       labels are laid out in the order the stack holds the tabs in, so
%       the order of <-tabs is the order they are drawn in.

test(a_tab_can_be_moved_along_the_row, true(Names == [two, one, three])) :-
    three_tabs(TW, TF1, TF2, _TF3),
    get(TF2, device, TS),
    send(TS, move_tab, TF2, TF1),
    tab_names(TW, Names).

test(and_back_again, true(Names == [one, two, three])) :-
    three_tabs(TW, TF1, TF2, _TF3),
    get(TF1, device, TS),
    send(TS, move_tab, TF2, TF1),
    send(TS, move_tab, TF1, TF2),
    tab_names(TW, Names).

test(a_label_says_which_tab_is_at_a_place, true(Name == two)) :-
    three_tabs(TW, _TF1, TF2, _TF3),
    get(TF2, device, TS),
    get(TF2, label_offset, X),
    get(TS, tab_at, X+2, Tab),
    get(Tab, name, Name).

test(and_nothing_past_the_end_of_the_row, [fail]) :-
    three_tabs(_TW, _TF1, _TF2, TF3),
    get(TF3, device, TS),
    get(TF3, label_offset, X),
    get(TF3?label_size, width, W),
    get(TS, tab_at, X+W+10, _).

test(dragging_a_label_over_another_moves_the_tab,
     true(Names == [two, one, three])) :-
    three_tabs(TW, TF1, TF2, _TF3),
    drag_label(TW, TF2, TF1),
    tab_names(TW, Names).

test(and_dragging_it_back_puts_it_where_it_was,
     true(Names == [one, two, three])) :-
    three_tabs(TW, TF1, TF2, _TF3),
    drag_label(TW, TF2, TF1),
    drag_label(TW, TF2, TF1),           % TF1 is on the right of it now
    tab_names(TW, Names).

test(while_dragging_a_label_over_itself_changes_nothing,
     true(Names == [one, two, three])) :-
    three_tabs(TW, _TF1, TF2, _TF3),
    drag_label(TW, TF2, TF2),
    tab_names(TW, Names).

%!  drag_label(+TabbedWindow, +Tab, +Onto) is det.
%
%   Press Tab's label and drag it onto Onto's, the way the window system
%   delivers it: to the window, in its coordinates.

drag_label(TW, Tab, Onto) :-
    get(Tab, device, TS),
    get(TS, area, area(SX, SY, _, _)),
    get(Tab, label_height, H),
    Y is SY + H//2,
    get(Tab, label_offset, From),
    get(Onto, label_offset, To),
    DownX is SX+From+5,
    DragX is SX+To+2,
    ignore(send(TW, post_event, event(ms_left_down, TW, DownX, Y))),
    ignore(send(TW, post_event, event(ms_left_drag, TW, DragX, Y))),
    ignore(send(TW, post_event, event(ms_left_up,   TW, DragX, Y))).

%!  three_tabs(-TabbedWindow, -One, -Two, -Three) is det.

three_tabs(TW, TF1, TF2, TF3) :-
    new(TW, tabbed_window('Test', size(400,300))),
    send(TW, tab, new(TF1, tab_frame(new(_P1, picture), one))),
    send(TW, tab, new(TF2, tab_frame(new(_P2, picture), two))),
    send(TW, tab, new(TF3, tab_frame(new(_P3, picture), three))),
    send(TW, open),
    send(TW, resize).

tab_names(TW, Names) :-
    get(TW, tabs, Chain),
    chain_list(Chain, Tabs),
    findall(N, (member(T, Tabs), get(T, name, N)), Names).

:- end_tests(tab_frame_label).


:- begin_tests(tab_frame_buttons).

%   A label is drawn by its tab, so the buttons on the tab bar are
%   displayed on the stack at the place the label was given.  The stack
%   therefore holds graphicals that are not tabs, which the rest of it has
%   to allow for.

close_button(TF, B) :-
    get(TF, hypered, close_button, B).

new_tab_button(TF, B) :-
    get(TF?device, hypered, new_tab_button, B).

click(B) :-
    get(B, area, area(X, Y, W, H)),
    CX is X+W//2,
    CY is Y+H//2,
    get(B?device, window, Window),
    send(event(ms_left_down, Window, CX, CY), post, B),
    send(event(ms_left_up, Window, CX, CY), post, B).

test(a_tab_carries_no_close_button_unless_asked) :-
    two_tabs(_TW, TF, _TF2),
    get(TF, closable, @off),
    \+ close_button(TF, _).

test(a_closable_tab_makes_room_for_its_button) :-
    two_tabs(_TW, TF, _TF2),
    get(TF?label_size, width, W0),
    get(TF, label_height, LH),
    send(TF, closable, @on),
    get(TF?label_size, width, W1),
    W1 > W0,                            % the button goes beside the text
    W1 < W0+LH,                         % but is smaller than the label
    close_button(TF, B),
    get(B, area, area(BX, _, BW, _)),
    get(TF, label_offset, LX),
    BX >= LX+W0,                        % and lands in the room made
    BX+BW =< LX+W1.

%   The cross sits on the baseline of the text it belongs to, and is drawn
%   smaller than the label is tall.

%   A tab that is given no label at all shrinks to its minimum rather than
%   keeping the box of a label it no longer has, which used to leave a
%   wide empty tab with its close button next to the new-tab button.

test(an_empty_label_gives_the_room_back) :-
    two_tabs(_TW, TF, TF2),
    send(TF, label, 'a very long tab title indeed'),
    send(TF?device, layout_labels),
    get(TF?label_size, width, W1),
    get(TF, label_height, H1),
    send(TF, label, ''),
    send(TF?device, layout_labels),
    get(TF?label_size, width, W2),
    get(TF, label_height, H2),
    W2 < W1,
    H2 =:= H1,                          % but the bar keeps its height
    get(TF2, label_offset, W2).         % and the next tab moves up

test(the_close_button_sits_on_the_text_baseline) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, closable, @on),
    get(TF, label_height, LH),
    get(TF, close_button_area, area(_, Y, S, S)),
    S < LH,
    get(TF?label_font, ascent, Ascent),
    Y+S =< LH,                          % inside the label
    Y+S >= Ascent.

test(the_close_button_closes_the_tab) :-
    two_tabs(TW, TF, TF2),
    send(TF, closable, @on),
    close_button(TF, B),
    click(B),
    \+ object(TF),
    get(TW?tabs, size, 1),
    send(TW?tabs, member, TF2).

%   A button belongs to the tab it acts on, so it has to go when that tab
%   does -- clicking it included.  It used to be left behind on the bar,
%   where it read as a second close button.

%   The stack holds the buttons among its tabs, so what follows a tab in
%   its chain need not be one: closing the tab on top used to hand
%   ->on_top whatever came next, which was a button, and a freed one.

test(closing_the_tab_on_top_raises_another_tab) :-
    two_tabs(TW, TF1, TF2),
    send(TF1, closable, @on),
    send(TF2, closable, @on),
    send(TW, new_tab_message, message(@pce, succeed)),
    get(TF1, status, on_top),
    send(TF1, destroy),
    get(TW?tabs, size, 1),
    get(TF2, status, on_top).

test(closing_the_last_tab_on_top_raises_the_one_before_it) :-
    two_tabs(TW, TF1, TF2),
    send(TF1, closable, @on),
    send(TF2, closable, @on),
    send(TW, new_tab_message, message(@pce, succeed)),
    send(TF2?device, on_top, TF2),      % ->on_top of a tabbed_window takes
    get(TF2, status, on_top),           % a name or a window, not a tab
    send(TF2, destroy),
    get(TW?tabs, size, 1),
    get(TF1, status, on_top).

test(a_closed_tab_takes_its_button_with_it,
     [forall(member(How, [click, destroy]))]) :-
    two_tabs(TW, TF, TF2),
    send(TF, closable, @on),
    send(TF2, closable, @on),
    close_buttons(TW, 2),
    close_button(TF, B),
    (   How == click
    ->  click(B)
    ;   send(TF, destroy)
    ),
    \+ object(B),
    close_buttons(TW, 1).

close_buttons(TW, Count) :-
    get(TW, tabs, Tabs),
    get(Tabs, head, Tab),
    get(Tab?device, graphicals, Graphicals),
    chain_list(Graphicals, List),
    findall(G, ( member(G, List),
                 \+ send(G, instance_of, tab),
                 get(G, name, close_tab)
               ), Buttons),
    length(Buttons, Count).

test(there_is_no_new_tab_button_unless_asked) :-
    two_tabs(_TW, TF, _TF2),
    \+ new_tab_button(TF, _).

test(the_new_tab_button_runs_what_it_was_given) :-
    two_tabs(TW, TF, TF2),
    send(TW, new_tab_message, message(new(C, number(0)), plus, 1)),
    new_tab_button(TF, B),
    get(TF2, label_offset, LX),         % it follows the last label
    get(TF2?label_size, width, LW),
    get(B, area, area(BX, _, _, _)),
    BX >= LX+LW,
    click(B),
    get(C, value, 1).

test(taking_the_message_away_takes_the_button_away) :-
    two_tabs(TW, TF, _TF2),
    send(TW, new_tab_message, message(@pce, succeed)),
    new_tab_button(TF, _),
    send(TW, new_tab_message, @nil),
    \+ new_tab_button(TF, _).

%   Everything that walks the stack has to skip what is not a tab.

test(the_buttons_are_not_taken_for_tabs) :-
    two_tabs(TW, TF, _TF2),
    send(TF, closable, @on),
    send(TW, new_tab_message, message(@pce, succeed)),
    get(TW?tabs, size, 2),
    send(TW, resize),
    get(TF, label_height, LH),
    get(TF, content_size, size(_, CH)),
    get(TW, area, area(_, _, _, WH)),
    CH =:= WH-LH,                       % a button did not count as a label
    get(TW, members, Members),
    get(Members, size, 2).

%       The close button sits over the label, so the editor would cover it
%       and a click meant for the editor would close the tab instead.

test(the_buttons_go_away_while_a_label_is_edited) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, closable, @on),
    send(TF, editable_label, @on),
    close_button(TF, _),
    send(TF, edit_label),
    \+ close_button(TF, _).

test(and_come_back_when_the_edit_ends) :-
    two_tabs(_TW, TF, _TF2),
    send(TF, closable, @on),
    send(TF, editable_label, @on),
    send(TF, edit_label),
    send(TF, end_label_edit),
    close_button(TF, _).

:- end_tests(tab_frame_buttons).


:- begin_tests(tab_frame_members).

test(windows_lists_all_of_them) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    send(TF, split, new(P3, picture), P2, horizontally),
    get(TF, windows, Windows),
    get(Windows, size, 3),
    send(Windows, member, P1),
    send(Windows, member, P2),
    send(Windows, member, P3).

test(delete_gives_the_space_to_the_sibling) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    geometry(P1, area(_,_,W0,_)),
    send(TF, delete, P2),
    get(TF?windows, size, 1),
    geometry(P1, area(_,_,W1,_)),
    W1 > W0.

test(destroy_is_seen_as_a_delete) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    send(P2, destroy),
    get(TF?windows, size, 1),
    get(TF?separators, size, 0).

test(deleted_window_survives_and_can_move) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    send(TF, delete, P2),
    send(TF, append, P2, P1, below),
    get(TF?windows, size, 2),
    geometry(P1, area(X1,_,_,_)),
    geometry(P2, area(X2,_,_,_)),
    X1 =:= X2.

test(tabbed_window_sees_windows_of_every_tab) :-
    tabbed(TW, TF, P1),
    send(TF, split, new(_P2, picture), P1, vertically),
    send(TW, append, new(_V, view), plain),     % a classic window_tab
    get(TW, members, Members),
    get(Members, size, 3).

test(on_top_selects_the_window_in_the_tab) :-
    tabbed(TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    send(TW, on_top, P2),
    get(TF, current, P2),
    send(TW, on_top, P1),
    get(TF, current, P1).

:- end_tests(tab_frame_members).


:- begin_tests(tab_frame_manager).

%   `window ->below' and friends work on whatever manages the tile
%   hierarchy the target is part of: a frame, or a tab_frame.  See tile
%   <-manager and window <-tile_manager.

test(a_tab_window_is_managed_by_its_tab) :-
    tabbed(_TW, TF, P1),
    get(P1, tile_manager, TF).

test(a_frame_member_is_managed_by_its_frame) :-
    new(F, frame('Test')),
    send(F, append, new(P, picture)),
    send(F, open),
    get(P, tile_manager, F).

test(relating_adds_to_the_tab) :-
    tabbed(_TW, TF, P1),
    send(new(P2, picture), right, P1),
    get(TF?windows, size, 2),
    get(TF?separators, size, 1),
    get(P2, tile_manager, TF),
    geometry(P1, area(X1,_,W1,_)),
    geometry(P2, area(X2,_,_,_)),
    X2 > X1+W1.

test(relating_below_adds_to_the_tab) :-
    tabbed(_TW, TF, P1),
    send(new(P2, picture), below, P1),
    get(TF?windows, size, 2),
    geometry(P1, area(_,Y1,_,H1)),
    geometry(P2, area(_,Y2,_,_)),
    Y2 > Y1+H1.

%       A window that carries scrollbars or a label is held through a
%       window_decorator and is displayed *on* that decorator.  Taking it
%       from whoever holds it must take the decorator out of that, and
%       must leave the window where it is: erased from its own decorator
%       it is never created, and all that is drawn is the decorator's
%       bare ground.

test(a_window_that_carries_a_label_stays_in_its_decorator) :-
    new(F, frame('Own')),
    send(F, append, new(P, picture('Canvas'))),
    get(P, decoration, Decor),
    Decor \== @nil,
    get(P, device, Decor).

test(and_so_it_does_when_it_moves_to_another_frame) :-
    new(F1, frame('One')),
    send(F1, append, new(P, picture('Canvas'))),
    new(F2, frame('Two')),
    send(F2, append, P),
    get(P, decoration, Decor),
    get(P, device, Decor),
    get(Decor, frame, F2).

test(and_when_it_moves_into_a_tab) :-
    tabbed(_TW, TF, P1),
    new(F, frame('Own')),
    send(F, append, new(P2, picture('Canvas'))),
    send(TF, append, P2, P1, right),
    get(P2, decoration, Decor),
    get(P2, device, Decor),
    get(P2, tile_manager, TF).

test(moving_a_pane_into_a_frame_takes_it_out_of_the_tab) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    new(F, frame('Own')),
    send(F, append, P2),
    send(F, open),
    get(TF?windows, size, 1),
    get(TF?separators, size, 0),
    get(P2, tile_manager, F).

test(moving_a_window_into_a_tab_takes_it_out_of_the_frame) :-
    tabbed(_TW, TF, P1),
    new(F, frame('Own')),
    send(F, append, new(P2, picture)),
    send(F, open),
    send(TF, append, P2, P1, right),
    get(TF?windows, size, 2),
    get(P2, tile_manager, TF),
    get(F?members, size, 0).

%       A window may be related to one that is in no tile manager yet, and
%       both arrive when that one is taken in.  Class frame walks the tile
%       tree of the window it takes -- see frameWindow() in
%       src/win/window.c -- and a tab has to do the same, or the windows
%       hanging off it are laid out but never displayed.

test(a_window_related_before_its_neighbour_comes_along_with_it,
     true(Windows == [one, two, three])) :-
    tabbed(_TW, TF, P1),
    send(P1, name, one),
    new(P2, picture), send(P2, name, two),
    new(P3, picture), send(P3, name, three),
    send(P3, below, P2),                % P2 is in no tab yet: P3 hangs on
    send(P2, right, P1),                % it until P2 is taken in
    get(TF, windows, Chain),
    chain_list(Chain, List),
    findall(N, (member(W, List), get(W, name, N)), Windows).

test(and_is_laid_out_where_the_tile_says, true(Below == true)) :-
    tabbed(_TW, TF, P1),
    new(P2, picture),
    new(P3, picture),
    send(P3, below, P2),
    send(P2, right, P1),
    send(TF, layout),
    geometry(P2, area(_, Y2, _, H2)),
    geometry(P3, area(_, Y3, _, _)),
    (   Y3 >= Y2+H2
    ->  Below = true
    ;   Below = Y2-H2-Y3
    ).

test(relating_to_a_tile_works_too) :-      % as class epilog_window does
    tabbed(_TW, TF, P1),
    get(P1, tile, T1),
    send(new(P2, picture), right, T1),
    get(TF?windows, size, 2),
    get(P2, tile_manager, TF).

test(the_manager_follows_a_new_root) :-
    tabbed(_TW, TF, P1),
    get(P1?tile, root, Root0),
    send(TF, split, new(_P2, picture), P1, horizontally),
    get(P1?tile, root, Root),
    Root \== Root0,                     % the split introduced a super
    get(Root, manager, TF).

test(current_never_answers_a_destroyed_window) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    send(TF, current, P2),
    send(P2, destroy),
    get(TF, current, P1).

:- end_tests(tab_frame_manager).


:- begin_tests(tab_frame_resize).

%!  gap_x(+TabFrame, +LeftWindow, -X) is det.
%
%   X in the middle of the gap right of LeftWindow.

gap_x(TF, Left, X) :-
    geometry(Left, area(LX,_,LW,_)),
    get(TF?tile, border, B),
    X is LX+LW+B//2.

%!  drag(+TabbedWindow, +TabFrame, +X0, +Y0, +X1, +Y1) is det.
%
%   Drag the gap at X0,Y0 to X1,Y1, in the content coordinates of the tab.

drag(TW, TF, X0, Y0, X1, Y1) :-
    event_at(TF, ms_left_down, X0, Y0, Down),
    send(TW, event, Down),
    event_at(TF, ms_left_drag, X1, Y1, Drag),
    send(TW, event, Drag),
    event_at(TF, ms_left_up, X1, Y1, Up),
    send(TW, event, Up).

test(cursor_follows_the_gap) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(_P2, picture), P1, vertically),
    gap_x(TF, P1, GapX),
    event_at(TF, loc_move, GapX, 50, Ev1),
    send(TF, update_cursor, Ev1),
    get(TF, cursor, C1),
    get(C1, name, ew_resize),
    event_at(TF, loc_move, 10, 50, Ev2),
    send(TF, update_cursor, Ev2),
    get(TF, cursor, @nil).

test(dragging_the_gap_redistributes_the_space) :-
    tabbed(TW, TF, P1),
    send(TF, split, new(P2, picture), P1, vertically),
    geometry(P1, area(X1,_,W1,_)),
    geometry(P2, area(_,_,W2,_)),
    gap_x(TF, P1, GapX),
    Target is GapX-60,
    event_at(TF, ms_left_down, GapX, 50, Down),
    send(TW, event, Down),
    event_at(TF, ms_left_drag, Target, 50, Drag),
    send(TW, event, Drag),
    event_at(TF, ms_left_up, Target, 50, Up),
    send(TW, event, Up),
    geometry(P1, area(_,_,NW1,_)),
    geometry(P2, area(_,_,NW2,_)),
    NW1 =:= Target-X1,                  % the gap went where we dropped it
    NW1+NW2 =:= W1+W2.                  % and the total is unchanged

%   A pane the user has resized holds its size through a zero stretch,
%   which used to be inherited by the tile a later split of that pane
%   introduces: the pair then took the new window's ideal size, squeezing
%   whatever was next to it, and could no longer be resized at all.

test(splitting_a_resized_pane_leaves_the_rest_where_it_was) :-
    tabbed(TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),
    geometry(P1, area(_,Y1,_,H1)),
    Gap is Y1+H1+2,
    Target is Gap-40,
    drag(TW, TF, 100, Gap, 100, Target),
    geometry(P1, area(_,_,_,RH1)),
    geometry(P2, area(_,RY2,_,RH2)),
    send(TF, split, new(_P3, picture), P1, vertically),
    geometry(P1, area(_,_,_,RH1)),      % the resized pane keeps its height
    geometry(P2, area(_,RY2,_,RH2)),    % and its neighbour does not move
    get(TF?separators, size, 2).

test(the_separator_of_a_resized_pane_can_still_be_dragged) :-
    tabbed(TW, TF, P1),
    send(TF, split, new(_P2, picture), P1, horizontally),
    geometry(P1, area(_,Y1,_,H1)),
    Gap is Y1+H1+2,
    Target is Gap-40,
    drag(TW, TF, 100, Gap, 100, Target),
    send(TF, split, new(_P3, picture), P1, vertically),
    geometry(P1, area(_,NY1,_,NH1)),
    NewGap is NY1+NH1+2,
    event_at(TF, loc_move, 100, NewGap, Ev),
    get(TF, resize_tile, Ev, _).

%   <-can_resize is derived and cached, as it is asked for on every
%   pointer move.  Relating or dropping a tile changes the answer.

test(can_resize_is_worked_out_again_after_a_split) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),
    get(P2?tile, can_resize, @off),     % last of the column: nothing to give
    send(TF, split, new(_P3, picture), P2, vertically),
    get(P2?tile, can_resize, @on),      % but P3 is beside it now
    get(TF?separators, size, 2).

test(can_resize_is_worked_out_again_after_a_delete) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),
    get(P1?tile, can_resize, @on),
    send(TF, delete, P2),
    get(P1?tile, can_resize, @off).     % on its own again

test(a_resized_pane_is_still_resizable) :-
    tabbed(TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),
    geometry(P1, area(_,Y1,_,H1)),
    Gap is Y1+H1+2,
    Target is Gap-40,
    drag(TW, TF, 100, Gap, 100, Target),
    send(TF, split, new(_P3, picture), P2, vertically),  % re-derives
    get(P1?tile, can_resize, @on).

test(no_gesture_inside_a_window) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(_P2, picture), P1, vertically),
    event_at(TF, ms_left_down, 10, 50, Down),
    send(Down, slot, receiver, TF),
    \+ send(@tab_frame_resize_gesture, event, Down).

%       What the tiles have to give up between them is shared out in
%       proportion to what they are, so a small window keeps a small share
%       rather than being asked for as many pixels as a large one and
%       ending with none.

test(a_small_window_keeps_a_share_of_a_space_too_small_for_all) :-
    crowded(_TF, Windows),
    forall(member(W, Windows),
           ( geometry(W, area(_, _, _, H)),
             H > 0
           )).

test(and_they_fit_in_the_tab_they_are_in) :-
    crowded(TF, [A, B, C]),
    get(TF?tile, border, Border),
    get(TF, content_size, size(_, Room)),
    geometry(A, area(_,_,_,HA)),
    geometry(B, area(_,_,_,HB)),
    geometry(C, area(_,_,_,HC)),
    HA+HB+HC + 2*Border =< Room.

test(and_the_small_one_shrinks_in_proportion) :-
    crowded(_TF, [Big, Small|_]),
    get(Big?tile, ideal_height, BI),
    get(Small?tile, ideal_height, SI),
    geometry(Big, area(_,_,_,BH)),
    geometry(Small, area(_,_,_,SH)),
    abs(BH*SI - SH*BI) =< BI+SI.        % the same fraction, give or take

%!  crowded(-TabFrame, -Windows) is det.
%
%   A tab holding a tall window, a short one and another tall one, in a
%   window with room for none of them at their ideal size.  This is a
%   terminal, a tool beside it and the terminal split.

crowded(TF, Windows) :-
    crowded(_TW, TF, Windows).

crowded(TW, TF, [Big, Small, Big2]) :-
    new(TW, tabbed_window('Test', size(400,300))),
    new(Big, picture(big, size(400, 500))),
    send(TW, tab, new(TF, tab_frame(Big, one))),
    send(TW, open),
    send(TW, resize),
    send(TF, split, new(Small, picture(small, size(200, 100))), Big, horizontally),
    send(TF, split, new(Big2, picture(big2, size(400, 500))), Big, horizontally),
    send(TW, resize).

%       Dragging the gap above the small one makes it taller.  The tiles
%       above the gap hold on to the size they have, which is not the size
%       they asked for once anything has had to give way.

test(dragging_a_gap_gives_the_pane_below_it_the_room) :-
    crowded(TW, TF, [_Big, Small, _Big2]),
    geometry(Small, area(_, SY, _, H0)),
    get(TF?tile, border, B),
    GapY is SY - B//2 - 1,
    drag(TW, TF, 50, GapY, 50, GapY-60),
    geometry(Small, area(_, _, _, H1)),
    H1 > H0.

test(and_dragging_it_back_takes_it_away_again) :-
    crowded(TW, TF, [_Big, Small, _Big2]),
    get(TF?tile, border, B),
    geometry(Small, area(_, SY0, _, _)),
    G0 is SY0 - B//2 - 1,
    drag(TW, TF, 50, G0, 50, G0-60),
    geometry(Small, area(_, SY1, _, H1)),
    G1 is SY1 - B//2 - 1,
    drag(TW, TF, 50, G1, 50, G1+40),
    geometry(Small, area(_, _, _, H2)),
    H2 < H1.

test(and_none_of_them_collapses_along_the_way) :-
    crowded(TW, TF, Windows),
    Windows = [_Big, Small, _Big2],
    get(TF?tile, border, B),
    geometry(Small, area(_, SY, _, _)),
    GapY is SY - B//2 - 1,
    drag(TW, TF, 50, GapY, 50, GapY-60),
    forall(member(W, Windows),
           ( geometry(W, area(_, _, _, H)),
             H > 0
           )).

%!  stacked(-TabbedWindow, -TabFrame, -Top, -Bottom) is det.
%
%   A window of a known size holding two panes above one another.

stacked(TW, TF, P1, P2) :-
    new(TW, tabbed_window('Test')),
    send(TW, tab, new(TF, tab_frame(new(P1, picture(a, size(400,200))), one))),
    send(TW, open),
    window_height(TW, 600),
    send(TF, split, new(P2, picture(b, size(400,200))), P1, horizontally),
    send(TW, resize).

%!  window_height(+TabbedWindow, +Height) is det.
%
%   Give the window a new height, the way the window manager does.

window_height(TW, H) :-
    get(TW, area, area(_,_,W,_)),
    send(TW, size, size(W, H)),
    send(TW, resize).

%!  drag_gap(+TabbedWindow, +TabFrame, +Top, +Delta) is det.
%
%   Drag the gap below Top over Delta pixels.

drag_gap(TW, TF, Top, Delta) :-
    geometry(Top, area(_, Y, _, H)),
    get(TF?tile, border, B),
    Gap is Y+H+B//2,
    drag(TW, TF, 50, Gap, 50, Gap+Delta).

%!  same_share(+H1, +H2, +NH1, +NH2) is semidet.
%
%   The two panes hold the same fraction of the room they share.  It is a
%   fraction rather than a ratio: rounding and the minimum size a pane is
%   laid out at (see MIN_TILE_SIZE) make it approximate.

same_share(H1, H2, NH1, NH2) :-
    Share0 is H1/(H1+H2),
    Share  is NH1/(NH1+NH2),
    abs(Share-Share0) < 0.02.

%       A hand resize says what the panes should be, not merely what they
%       happen to be: ->rebalance turns the sizes the drag left behind
%       into the wish, so resizing the window afterwards keeps the panes
%       at the relative sizes the user gave them.  Before, everything up
%       to the dragged edge held on to its size and the pane below it took
%       all of what a resize brought or gave all of what it took away.

test(a_hand_resize_keeps_its_share_when_the_window_grows) :-
    stacked(TW, TF, P1, P2),
    drag_gap(TW, TF, P1, -100),
    geometry(P1, area(_,_,_,H1)),
    geometry(P2, area(_,_,_,H2)),
    window_height(TW, 900),
    geometry(P1, area(_,_,_,NH1)),
    geometry(P2, area(_,_,_,NH2)),
    NH1 > H1,                           % it grew along
    same_share(H1, H2, NH1, NH2).

test(a_hand_resize_keeps_its_share_when_the_window_shrinks) :-
    stacked(TW, TF, P1, P2),
    drag_gap(TW, TF, P1, -100),
    geometry(P1, area(_,_,_,H1)),
    geometry(P2, area(_,_,_,H2)),
    window_height(TW, 300),
    geometry(P1, area(_,_,_,NH1)),
    geometry(P2, area(_,_,_,NH2)),
    NH1 < H1,                           % it gave along
    same_share(H1, H2, NH1, NH2).

%       What ->rebalance asks for is the size the pane has now.

test(a_hand_resize_becomes_the_size_the_panes_ask_for) :-
    stacked(TW, TF, P1, _P2),
    drag_gap(TW, TF, P1, -100),
    geometry(P1, area(_,_,_,H1)),
    get(P1?tile, ideal_height, H1).

%       A tile that never stretched is fixed by whoever built it -- a menu
%       bar is as high as its buttons and no more -- and a rebalance may
%       not turn it into a pane that takes its share.

test(a_pane_that_was_never_resizable_is_not_made_one) :-
    stacked(_TW, TF, P1, P2),
    send(P2?tile, ver_stretch, 0),
    send(P2?tile, ver_shrink, 0),
    send(TF?tile, rebalance),
    get(P2?tile, ver_stretch, 0),
    get(P2?tile, ver_shrink, 0),
    get(P1?tile, ver_stretch, S),
    S > 0.

:- end_tests(tab_frame_resize).


:- begin_tests(tab_frame_drop).

%   A window is moved by dragging its split_handle onto another window;
%   the receiver splits and the dropped window takes the half the pointer
%   is nearest.  The drag itself cannot be driven here: the driver these
%   tests run against paints nothing, so no subwindow is ever created and
%   the pointer cannot be resolved to one (see event <-inside_sub_window).
%   What ->drop does once a target is found is what is checked.

%!  at(+Window, +Where, -Point) is det.
%
%   A point just inside the Where edge of Window, in the coordinates the
%   tab lays its windows out in.

at(W, Where, point(X, Y)) :-
    geometry(W, area(AX, AY, AW, AH)),
    edge(Where, AX, AY, AW, AH, X, Y).

edge(left,   AX, AY, _AW, AH, X, Y) :- X is AX+5,       Y is AY+AH//2.
edge(right,  AX, AY, AW,  AH, X, Y) :- X is AX+AW-5,    Y is AY+AH//2.
edge(above,  AX, AY, AW, _AH, X, Y) :- X is AX+AW//2,   Y is AY+5.
edge(below,  AX, AY, AW,  AH, X, Y) :- X is AX+AW//2,   Y is AY+AH-5.

test(the_side_follows_the_nearest_edge, [forall(member(Where,
                                                [left,right,above,below]))]) :-
    tabbed(_TW, TF, P1),
    at(P1, Where, Pos),
    get(TF, drop_side, Pos, Where).

test(the_sides_get_more_of_the_window_than_the_top_and_bottom) :-
    tabbed(_TW, TF, P1),
    geometry(P1, area(AX, AY, AW, AH)),
    AW > AH,                            % a quarter in and a fifth down is
    X is AX+AW//4,                      % nearer the top edge in pixels, but
    Y is AY+AH//5,                      % still well inside the left zone
    get(TF, drop_side, point(X, Y), left).

test(the_target_is_the_window_under_the_pointer) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),
    at(P1, left, Pos1),
    get(TF, drop_target, Pos1, P1),
    at(P2, left, Pos2),
    get(TF, drop_target, Pos2, P2).

test(a_point_outside_every_window_is_no_target) :-
    tabbed(_TW, TF, _P1),
    \+ get(TF, drop_target, point(-20, -20), _).

test(preview_shows_and_takes_away_the_outline) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),
    at(P1, right, Pos),
    send(TF, preview_drop, P2, Pos),
    get(TF, drop_feedback, Box),
    Box \== @nil,
    send(TF, preview_drop, @nil),
    get(TF, drop_feedback, @nil).

test(dropping_beside_a_window_rearranges_the_tab) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),   % stacked
    geometry(P1, area(_,Y1,_,H1)),
    geometry(P2, area(_,Y2,_,_)),
    Y2 > Y1+H1,
    at(P1, right, Pos),
    send(TF, drop, P2, Pos),
    geometry(P1, area(X1,_,W1,_)),                         % now beside
    geometry(P2, area(X2,_,_,_)),
    X2 > X1+W1,
    get(TF?windows, size, 2).

test(dropping_from_another_tab_moves_the_window) :-
    tabbed(TW, TF, P1),
    send(TW, tab, new(TF2, tab_frame(new(P2, picture), two))),
    get(TW?tabs, size, 2),
    at(P1, below, Pos),
    send(TF, drop, P2, Pos),
    get(TF?windows, size, 2),
    send(TF?windows, member, P2),
    get(P2, tile_manager, TF),
    \+ object(TF2),                    % the tab it left was empty
    get(TW?tabs, size, 1).

test(dropping_a_window_on_itself_does_nothing) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),
    at(P2, right, Pos),
    send(TF, drop, P2, Pos),
    get(TF?windows, size, 2),
    get(TF?separators, size, 1).

test(dragging_the_grip_onto_another_window_moves_it) :-
    tabbed(_TW, TF, P1),
    send(TF, split, new(P2, picture), P1, horizontally),   % P2 below P1
    send(P2, display, new(H, split_handle)),
    send(H, place, P2),
    get(H, area, area(HX, HY, HW, HH)),
    DownX is HX+HW//2,
    DownY is HY+HH//2,
    geometry(P1, area(AX, AY, AW, AH)),
    geometry(P2, area(BX, BY, _, _)),
    UpX is AX+AW-20-BX,                 % the right edge of P1, in the
    UpY is AY+AH//2-BY,                 % coordinates of P2
    send(P2, post_event, event(ms_left_down, P2, DownX, DownY)),
    send(P2, post_event, event(ms_left_drag, P2, UpX, UpY)),
    get(TF, drop_feedback, Box),
    Box \== @nil,                       % outlined while the pointer is there
    send(P2, post_event, event(ms_left_up, P2, UpX, UpY)),
    get(TF, drop_feedback, @nil),
    geometry(P1, area(NX1, _, NW1, _)),
    geometry(P2, area(NX2, _, _, _)),
    NX2 > NX1+NW1.                      % beside P1 now, not below it

%   A class variable whose default cannot be converted to its type is
%   only found out when something asks for it, and the cursor asks in the
%   middle of a drag.

test(dragging_the_grip_onto_another_frame_moves_the_window) :-
    tabbed_at(0, 0, _TW1, TF1, P1),
    tabbed_at(700, 0, _TW2, TF2, P2),
    send(P1, display, new(H, split_handle)),
    send(H, place, P1),
    get(P1, display_position, point(D1X, D1Y)),
    get(P2, display_position, point(D2X, D2Y)),
    geometry(P2, area(_, _, W2, H2)),
    get(H, area, area(HX, HY, HW, HH)),
    DownX is HX+HW//2,
    DownY is HY+HH//2,
    UpX is D2X+W2-30-D1X,               % the right edge of P2, over in the
    UpY is D2Y+H2//2-D1Y,               % other frame, relative to P1
    send(P1, post_event, event(ms_left_down, P1, DownX, DownY)),
    send(P1, post_event, event(ms_left_drag, P1, UpX, UpY)),
    get(TF2, drop_feedback, Box),
    Box \== @nil,                       % outlined in the frame it is over
    send(P1, post_event, event(ms_left_up, P1, UpX, UpY)),
    get(TF2?windows, size, 2),
    send(TF2?windows, member, P1),
    get(P1, tile_manager, TF2),
    \+ object(TF1).                     % the tab it left was empty

%   Clicking the grip picks the window up instead of dragging it: the
%   pointer then carries it until a click says where it goes.  That works
%   across frames on every window system, as the pointer is delivered to
%   whatever window it is over rather than to the one it was pressed on.

test(clicking_the_grip_picks_the_window_up) :-
    tabbed_at(0, 0, _TW, TF, P1),
    send(TF, split, new(_P2, picture), P1, horizontally),
    send(P1, display, new(H, split_handle)),
    send(H, place, P1),
    get(H, area, area(HX, HY, HW, HH)),
    X is HX+HW//2,
    Y is HY+HH//2,
    send(P1, post_event, event(ms_left_down, P1, X, Y)),
    send(P1, post_event, event(ms_left_up, P1, X, Y)),
    get(@split_move, source, P1),
    send(@split_move, cancel),
    get(@split_move, source, @nil).

test(the_pointer_puts_it_down_in_another_frame) :-
    tabbed_at(0, 0, _TW1, TF1, P1),
    tabbed_at(700, 0, _TW2, TF2, P2),
    send(P1, display, new(H, split_handle)),
    send(H, place, P1),
    get(H, area, area(HX, HY, HW, HH)),
    DownX is HX+HW//2,
    DownY is HY+HH//2,
    send(P1, post_event, event(ms_left_down, P1, DownX, DownY)),
    send(P1, post_event, event(ms_left_up, P1, DownX, DownY)),
    get(@split_move, source, P1),       % picked up
    geometry(P2, area(BX, BY, BW, BH)),
    OverX is BX+BW-20,                  % the right half of the window in
    OverY is BY+BH//2,                  % the other frame
    post_grabbed(P1, loc_move, TF2, OverX, OverY),
    get(TF2, drop_feedback, Box),
    Box \== @nil,                       % outlined over there
    post_grabbed(P1, ms_left_up, TF2, OverX, OverY),
    get(TF2?windows, size, 2),
    send(TF2?windows, member, P1),
    get(P1, tile_manager, TF2),
    get(@split_move, source, @nil),     % and put down again
    \+ object(TF1).

test(the_grip_fades_until_the_pointer_is_on_it) :-
    tabbed(_TW, _TF, P1),
    send(P1, display, new(H, split_handle)),
    send(H, place, P1),
    get(H, area, area(HX, HY, HW, HH)),
    X is HX+HW//2,
    Y is HY+HH//2,
    get(H, class_variable_value, dim_opacity, Dim),
    Dim < 1.0,
    get(H, opacity, Dim),
    send(event(area_enter, P1, X, Y), post, H),
    get(H, opacity, Full),
    Full =:= 1.0,                       % num gives back a plain 1
    send(event(area_exit, P1, X, Y), post, H),
    get(H, opacity, Dim).

%   Which gesture the grip names depends on the window system: dragging
%   needs to know where the windows are and Wayland does not say.

test(the_grip_names_the_gesture_that_works_here) :-
    tab_frame:move_gesture(How),
    memberchk(How, [drag, click]),
    tab_frame:handle_help(drag, Drag),
    tab_frame:handle_help(click, Click),
    Drag \== Click,
    tab_frame:handle_help(How, Here),
    tabbed(_TW, _TF, P1),
    send(P1, display, new(H, split_handle)),
    get(H, help_message, tag, Tag),
    send(Tag, equal, Here).

test(the_grip_shows_its_picture_at_its_size) :-
    tabbed(_TW, _TF, P1),
    send(P1, display, new(H, split_handle)),
    get(H, class_variable_value, handle_size, Size),
    get(H?graphicals, find, message(@arg1, instance_of, bitmap), Bitmap),
    send(Bitmap?image?size, equal, Size),
    send(H?size, equal, Size).

%   The grip is one of these; a pane may want more of them, as the demo
%   does for throwing a pane away.

test(a_pane_handle_takes_any_picture_and_a_tip) :-
    tabbed(_TW, _TF, P1),
    send(P1, display,
         new(H, pane_handle('tool/trashcan.svg', 'Delete this pane'))),
    get(H, class_variable_value, handle_size, Size),
    send(H?size, equal, Size),
    get(H, help_message, tag, Tag),
    send(Tag, equal, 'Delete this pane').

test(the_class_variables_resolve,
     [forall(member(Class-Var, [split_handle        - handle_size,
                                split_handle        - grip_image,
                                split_handle        - dim_opacity,
                                tab_frame           - split_bias,
                                split_handle_gesture- cursor,
                                split_handle_gesture- cursor_size,
                                split_handle_gesture- cursor_border]))]) :-
    get(@pce, convert, Class, class, TheClass),
    get(TheClass, class_variable, Var, ClassVariable),
    get(ClassVariable, value, _).

test(the_handle_offers_a_window_and_a_tab_of_its_own_on_a_popup,
     Items == [move_to_new_window, move_to_new_tab,
               move_to_previous_tab, move_to_next_tab, close]) :-
    new(P, picture),
    send(P, display, new(H, split_handle)),
    get(H, all_recognisers, Recognisers),
    chain_list(Recognisers, Rs),
    member(R, Rs),
    send(R, instance_of, popup_gesture),
    !,
    get(R, popup, Popup),
    get(Popup, members, Chain),
    chain_list(Chain, Members),
    findall(V, (member(MI, Members), get(MI, value, V)), Items).

test(the_handle_drags_the_window_it_is_displayed_on) :-
    new(P, picture),
    send(P, display, new(H, split_handle)),
    get(H, window, P),                  % what the gesture takes as source
    get(H, all_recognisers, Recognisers),
    get(Recognisers, find,
        message(@arg1, instance_of, drag_and_drop_gesture), _).

:- end_tests(tab_frame_drop).


                 /*******************************
                 *         WINDOW TREE          *
                 *******************************/

/* <-window_tree writes down how the windows of a tab are tiled and
   ->window_tree arranges them that way, both with the windows themselves
   as the leaves.  What is checked here is the shape, the order in which
   the splits have to be made and the arithmetic of the shares.
*/

:- begin_tests(tab_frame_window_tree).

%!  named(-TabbedWindow, -TabFrame, -Windows) is det.
%
%   An open tab_frame holding three named pictures, side by side.

named(TW, TF, [A,B,C]) :-
    new(TW, tabbed_window('Test', size(600,400))),
    send(TW, tab, new(TF, tab_frame(new(A, picture), one))),
    send(A, name, a),
    send(TW, open),
    send(TW, resize),
    send(TF, append, new(B, picture), A, right), send(B, name, b),
    send(TF, append, new(C, picture), B, right), send(C, name, c).

%!  shape(+Tree, -Shape) is det.
%
%   Tree with its windows written as their names and its shares dropped,
%   so that a test can compare the shape alone.

shape(Tree, Name) :-
    object(Tree),
    !,
    get(Tree, name, Name).
shape(Tree, Shape) :-
    Tree =.. [Orientation, Shares],
    findall(S, (member(Share, Shares), share_shape(Share, S)), Subs),
    Shape =.. [Orientation, Subs].

share_shape(_-Content, Shape) :-
    !,
    shape(Content, Shape).
share_shape(Content, Shape) :-
    shape(Content, Shape).

%!  share_of(+Tree, +Name, -Share) is semidet.
%
%   The share the window called Name has of the row it is in.

share_of(Tree, Name, Share) :-
    Tree =.. [_, Shares],
    (   member(Share0-Content, Shares),
        object(Content),
        get(Content, name, Name)
    ->  Share = Share0
    ;   member(_-Content, Shares),
        \+ object(Content),
        share_of(Content, Name, Share)
    ).

test(one_window_is_its_own_tree, Shape == a) :-
    tabbed(_TW, TF, P),
    send(P, name, a),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(a_row_reads_back_as_horizontal, Shape == horizontal([a,b,c])) :-
    named(_TW, TF, _),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(a_column_reads_back_as_vertical, Shape == vertical([a,b])) :-
    tabbed(_TW, TF, A),
    send(A, name, a),
    send(TF, append, new(B, picture), A, below),
    send(B, name, b),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(a_nested_tree_reads_back_nested,
     Shape == horizontal([vertical([a,c]),b])) :-
    tabbed(_TW, TF, A),
    send(A, name, a),
    send(TF, append, new(B, picture), A, right), send(B, name, b),
    send(TF, append, new(C, picture), A, below), send(C, name, c),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

%       The one that pins the construction order down.  A tree is built
%       from the outside in: making the inner split first would leave the
%       last window beside one of the pair rather than below both.

test(the_outermost_split_is_made_first,
     Shape == vertical([horizontal([a,b]),c])) :-
    named(_TW, TF, [A,B,C]),
    send(TF, window_tree, vertical([0.5-horizontal([0.5-A, 0.5-B]),
                                    0.5-C])),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(the_shape_asked_for_is_the_shape_made,
     Shape == horizontal([a,vertical([b,c])])) :-
    named(_TW, TF, [A,B,C]),
    send(TF, window_tree, horizontal([0.5-A,
                                      0.5-vertical([0.5-B, 0.5-C])])),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(shares_are_applied) :-
    named(_TW, TF, [A,B,C]),
    send(TF, window_tree, horizontal([0.5-A, 0.25-B, 0.25-C])),
    send(TF, window_shares, horizontal([0.5-A, 0.25-B, 0.25-C])),
    get(TF, window_tree, Tree),
    share_of(Tree, a, ShareA),
    share_of(Tree, b, ShareB),
    assertion(abs(ShareA-0.5) < 0.02),
    assertion(abs(ShareB-0.25) < 0.02).

%       A share is a wish rather than merely a size: what the tree asks for
%       is kept when the window holding it is resized.

test(the_shares_a_tree_asks_for_survive_a_resize) :-
    named(TW, TF, [A,B,C]),
    send(TF, window_shares, horizontal([0.5-A, 0.25-B, 0.25-C])),
    get(TW, area, area(_,_,_,H)),
    send(TW, size, size(900, H)),
    send(TW, resize),
    get(TF, window_tree, Tree),
    share_of(Tree, a, ShareA),
    share_of(Tree, b, ShareB),
    assertion(abs(ShareA-0.5) < 0.02),
    assertion(abs(ShareB-0.25) < 0.02).

%       A share says how much of the row a window takes, so it is what it
%       is relative to the others: [2,1,1] and [0.5,0.25,0.25] ask for the
%       same thing.

test(shares_are_relative) :-
    named(_TW, TF, [A,B,C]),
    send(TF, window_shares, horizontal([2-A, 1-B, 1-C])),
    get(TF, window_tree, Tree),
    share_of(Tree, a, ShareA),
    assertion(abs(ShareA-0.5) < 0.02).

test(shares_read_back_as_they_were_set) :-
    named(_TW, TF, [A,B,C]),
    send(TF, window_shares, horizontal([0.5-A, 0.3-B, 0.2-C])),
    get(TF, window_tree, Tree),
    send(TF, window_shares, Tree),
    get(TF, window_tree, Again),
    assertion(Tree == Again).

%       A window that carries a label or a scroll bar of its own sits in a
%       window_decorator, and it is the decorator the tile places.  The
%       tree must answer the window all the same.

test(a_decorated_window_is_unwrapped, Shape == horizontal([a,b])) :-
    tabbed(_TW, TF, A),
    send(A, name, a),
    send(TF, append, new(B, picture), A, right),
    send(B, name, b),
    send(B, label, 'Has a label'),
    get(B, decoration, Decor),
    assertion(Decor \== @nil),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

:- end_tests(tab_frame_window_tree).


                 /*******************************
                 *         WINDOW GROUP         *
                 *******************************/

/* A window can be put beside a *group* of windows rather than beside one:
   a navigator belongs down the left of the editor and the terminal
   together, not to the left of whichever of them is current.  These check
   that, the flattening that keeps the tree free of nodes that say nothing,
   and giving the newcomer its share of the row without disturbing the
   rest.
*/

:- begin_tests(tab_frame_window_group).

%!  column(-TabbedWindow, -TabFrame, -Windows) is det.
%
%   An open tab_frame holding two pictures, one above the other.

column(TW, TF, [A,B]) :-
    new(TW, tabbed_window('Test', size(900,700))),
    send(TW, tab, new(TF, tab_frame(new(A, picture), one))),
    send(A, name, a),
    send(TW, open),
    send(TW, resize),
    send(TF, append, new(B, picture), A, below),
    send(B, name, b).

%!  nested(-TabbedWindow, -TabFrame, -Windows) is det.
%
%   `a' on the left, `b' over `c' on the right.

nested(TW, TF, [A,B,C]) :-
    new(TW, tabbed_window('Test', size(900,700))),
    send(TW, tab, new(TF, tab_frame(new(A, picture), one))),
    send(A, name, a),
    send(TW, open),
    send(TW, resize),
    send(TF, append, new(B, picture), A, right), send(B, name, b),
    send(TF, append, new(C, picture), B, below), send(C, name, c).

%!  pane(+Name, -Window) is det.

pane(Name, W) :-
    new(W, picture),
    send(W, name, Name).

%       The shares are dropped: what these tests are about is the shape.

shape(Tree, Name) :-
    object(Tree),
    !,
    get(Tree, name, Name).
shape(Tree, Shape) :-
    Tree =.. [Orientation, Shares],
    findall(S, ( member(Share, Shares),
                 share_of(Share, Content),
                 shape(Content, S)
               ), Subs),
    Shape =.. [Orientation, Subs].

share_of(_-Content, Content) :- !.
share_of(Content, Content).

%!  share_named(+Tree, +Name, -Share) is semidet.

share_named(Tree, Name, Share) :-
    Tree =.. [_, Shares],
    (   member(Share0-Content, Shares),
        object(Content),
        get(Content, name, Name)
    ->  Share = Share0
    ;   member(_-Content, Shares),
        \+ object(Content),
        share_named(Content, Name, Share)
    ).

%       The case the whole thing exists for.  Beside one window the new
%       one joins that window's row; beside all of them it goes down the
%       edge of the lot.

test(beside_a_window_is_beside_that_window,
     Shape == vertical([horizontal([nav,a]),b])) :-
    column(_TW, TF, [A,_B]),
    pane(nav, Nav),
    send(TF, append, Nav, A, left),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(beside_them_all_is_down_the_edge,
     Shape == horizontal([nav,vertical([a,b])])) :-
    column(_TW, TF, [A,B]),
    pane(nav, Nav),
    send(TF, append, Nav, chain(A,B), left),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(beside_a_group_is_beside_the_group,
     Shape == horizontal([a,nav,vertical([b,c])])) :-
    nested(_TW, TF, [_A,B,C]),
    pane(nav, Nav),
    send(TF, append, Nav, chain(B,C), left),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(a_group_of_one_is_that_window,
     Shape == horizontal([a,vertical([horizontal([b,nav]),c])])) :-
    nested(_TW, TF, [_A,B,_C]),
    pane(nav, Nav),
    send(TF, append, Nav, chain(B), right),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

%       A row that already runs the way the new window is to be added is
%       joined rather than wrapped: wrapping lays out the same but reads
%       back as a tree with a node in it that says nothing.

test(a_row_that_runs_that_way_is_joined,
     Shape == horizontal([nav,a,vertical([b,c])])) :-
    nested(_TW, TF, [A,B,C]),
    pane(nav, Nav),
    send(TF, append, Nav, chain(A,B,C), left),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

test(and_a_row_that_does_not_is_wrapped,
     Shape == vertical([horizontal([a,vertical([b,c])]),bar])) :-
    nested(_TW, TF, [A,B,C]),
    pane(bar, Bar),
    send(TF, append, Bar, chain(A,B,C), below),
    get(TF, window_tree, Tree),
    shape(Tree, Shape).

%       Windows that are not a subtree of their own are refused, which is
%       how a caller finds out that what it asked for cannot be done here.

test(a_group_that_is_not_a_subtree_is_refused, fail) :-
    nested(_TW, TF, [A,_B,C]),
    get(TF, window_group, chain(A,C), _).

test(a_group_of_windows_of_another_tab_is_refused, fail) :-
    nested(_TW, TF, [A,_B,_C]),
    column(_TW2, _TF2, [A2,_B2]),
    get(TF, window_group, chain(A,A2), _).

test(the_whole_tab_is_a_group) :-
    nested(_TW, TF, [A,B,C]),
    get(TF, window_group, chain(A,B,C), Tile),
    assertion(get(Tile, super, @nil)).       % the root

                 /*******************************
                 *            SHARES            *
                 *******************************/

test(the_newcomer_gets_the_share_it_asked_for) :-
    nested(_TW, TF, [A,B,C]),
    get(TF, window_tree, Was),
    pane(nav, Nav),
    send(TF, append, Nav, chain(A,B,C), left),
    send(TF, window_share, Nav, 0.2, Was),
    get(TF, window_tree, Tree),
    share_named(Tree, nav, Share),
    assertion(abs(Share-0.2) < 0.01).

%       Adding a window moves the ideal sizes about at every level, not
%       only in the row it lands in.  What the rest of the window looked
%       like has to come back.

test(the_other_rows_keep_the_shares_they_had) :-
    nested(_TW, TF, [_A,B,C]),
    get(TF, window_tree, Was),
    share_named(Was, a, WasA),
    pane(nav, Nav),
    send(TF, append, Nav, chain(B,C), above),
    send(TF, window_share, Nav, 0.3, Was),
    get(TF, window_tree, Tree),
    share_named(Tree, a, Share),
    share_named(Tree, nav, ShareNav),
    assertion(abs(Share-WasA) < 0.01),
    assertion(abs(ShareNav-0.3) < 0.01).

test(the_others_in_the_row_keep_their_proportions) :-
    nested(_TW, TF, [A,B,C]),
    get(TF, window_tree, Was),
    pane(nav, Nav),
    send(TF, append, Nav, chain(A,B,C), left),
    send(TF, window_share, Nav, 0.2, Was),
    get(TF, window_tree, Tree),
    share_named(Tree, a, ShareA),
    assertion(abs(ShareA-0.4) < 0.01).       % half of what is left

:- end_tests(tab_frame_window_group).
