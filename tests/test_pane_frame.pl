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


:- module(test_pane_frame, [test_pane_frame/0]).
:- encoding(utf8).

/** <module> Tests for library(pane_frame)

A pane_frame holds tools in tabs and panes: a menu bar on top, the panes
in the middle and optionally a report/prompt bar at the bottom.  These
check the frame it builds, the protocol it asks of the panes, and that a
pane answering none of that protocol still works.

The frames are never opened: nothing here needs pixels, and the fading of
an inactive pane can only be checked by its <-opacity, as the headless
SDL driver paints nothing.

Run with:

    swipl -g test_pane_frame -t halt \
          packages/xpce/tests/test_pane_frame.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pane_frame)).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(lists), [member/2, memberchk/2, reverse/2]).
:- use_module(library(pane_layouts), [forget_arrangements/0]).

%       An arrangements file of their own: the tests must neither read nor
%       write the arrangements of whoever runs them.

:- multifile pane_layouts:arrangements_file/1.

pane_layouts:arrangements_file(File) :-
    current_prolog_flag(tmp_dir, Tmp),
    atom_concat(Tmp, '/test_pane_frame_store', File).

test_pane_frame :-
    run_tests([ pane_frame_structure,
                pane_frame_menu_bar,
                pane_frame_label,
                pane_frame_focus,
                pane_frame_move,
                pane_frame_opacity,
                pane_frame_panes,
                pane_frame_plain_pane,
                pane_frame_minimum,
                pane_frame_term,
                pane_frame_split_beside,
                pane_frame_arranged
              ]).

                 /*******************************
                 *          TEST CLASSES        *
                 *******************************/

%       A pane that answers the whole protocol.  <-menu_bar_key is its
%       <-kind, so that two panes of this class can still ask for
%       different menu bars.

:- pce_begin_class(tp_pane, window, "Pane answering the pane protocol").
:- use_class_template(pane).

variable(kind,     name := plain, both, "Which menu bar I ask for").
variable(closable, bool := @on,   both, "Do I agree to be closed?").
variable(exposed,  int := 0,      both, "Times I was told I am current").
variable(setting,  name := none,  both, "Something to write down").

class_variable(inactive_opacity, num, 0.6,
               "Fade me while another pane has the focus").

variable(own_title_format, name*, both,
         "What a window showing me is called; @nil: not my business").

title_format(P, Format:name) :<-
    get(P, own_title_format, Format),
    Format \== @nil.

menu_bar_key(P, Key:name) :<-
    get(P, kind, Key).

fill_menu_bar(P, MD:tool_dialog) :->
    get(MD, popup, edit, @on, Popup),
    send(Popup, append, menu_item(P?kind)).

pane_exposed(P) :->
    get(P, exposed, N0),
    N is N0+1,
    send(P, exposed, N).

can_close(P, Reply:bool) :<-
    get(P, closable, Reply).

sibling(P, New:window) :<-
    new(New, tp_pane),
    send(New, kind, P?kind).

%       The pair a pane answers to be written down and built back.

pane_term(P, Options:prolog) :<-
    get(P, setting, Setting),
    (   Setting == none
    ->  Options = []
    ;   Options = [setting(Setting)]
    ).

pane_term(P, Options:prolog) :->
    (   memberchk(setting(Setting), Options)
    ->  send(P, setting, Setting)
    ;   true
    ).

:- pce_end_class(tp_pane).

%       A pane whose class insists on an argument, as `prolog_debugger'
%       does: it is made for something live and cannot come from a term.

:- pce_begin_class(tp_needs, window, "Pane that needs an argument").

initialise(P, Level:int) :->
    send_super(P, initialise),
    send(P, name, Level).

:- pce_end_class(tp_needs).

%       A pane that answers none of it.

:- pce_begin_class(tp_bare, window, "Pane answering nothing at all").
:- pce_end_class(tp_bare).

:- pce_begin_class(tp_app, application, "Application of a test frame").

label_format(_App, Fmt:name) :<-
    Fmt = 'Test -- %s'.

variable(emptied, frame*, both, "Frame whose last pane went").

frame_empty(App, F:frame) :->
    "Remember rather than destroy, so a test can see it"::
    send(App, emptied, F).

fill_menu_bar(_App, MD:tool_dialog, _F:frame) :->
    get(MD, popup, file, @on, Popup),
    send(Popup, append, menu_item(quit)).

:- pce_end_class(tp_app).

                 /*******************************
                 *            HELPERS           *
                 *******************************/

%!  frame(-Frame, -Application, -Pane) is det.
%
%   A pane_frame with one tp_pane named `one'.  The frames are not opened
%   and not destroyed: tearing one down is unreliable on dummy-SDL, and
%   nothing here outlives the process.

frame(F, App, P) :-
    frame(F, App, P, @off).

frame(F, App, P, Bar) :-
    new(App, tp_app(test)),
    new(P, tp_pane),
    send(P, name, one),
    send(P, kind, alpha),
    new(F, pane_frame(App, @default, P, Bar)).

%!  pane(+Name, +Kind, -Pane) is det.

pane(Name, Kind, P) :-
    new(P, tp_pane),
    send(P, name, Name),
    send(P, kind, Kind).

%!  menus(+Frame, -Menus:list) is det.
%
%   The menu bar as Name-Items pairs, in the order it carries them.

menus(F, Menus) :-
    get(F, menu_bar, MB),
    get(MB, members, Popups),
    chain_list(Popups, List),
    findall(Name-Items,
            ( member(Popup, List),
              get(Popup, name, Name),
              items(Popup, Items)
            ),
            Menus).

items(Popup, Items) :-
    get(Popup, members, Chain),
    chain_list(Chain, List),
    findall(V, (member(MI, List), get(MI, value, V)), Items).

%!  opacities(+Frame, -Pairs:list) is det.
%
%   <-opacity of every pane of the tab in view, by name.

opacities(F, Pairs) :-
    get(F, tab, Tab),
    get(Tab, windows, Chain),
    chain_list(Chain, Panes),
    findall(Name-Opacity,
            ( member(P, Panes),
              get(P, name, Name),
              get(P, opacity, Opacity)
            ),
            Pairs).

%!  faded(+Frame, -Names:list) is det.
%
%   The panes of the tab in view that are faded.

faded(F, Names) :-
    opacities(F, Pairs),
    findall(N, (member(N-O, Pairs), O < 1.0), Names).

                 /*******************************
                 *           STRUCTURE          *
                 *******************************/

:- begin_tests(pane_frame_structure).

test(three_rows_when_a_status_bar_is_asked_for, Names == [pane_menu_dialog,
                                                          pane_tabbed_window,
                                                          pane_status_dialog]) :-
    frame(F, _App, _P, @on),
    get(F, members, Chain),
    chain_list(Chain, Members),
    findall(N, (member(M, Members), get(M, class_name, N)), Names).

test(two_rows_when_it_is_not, Names == [pane_menu_dialog,
                                        pane_tabbed_window]) :-
    frame(F, _App, _P),
    get(F, members, Chain),
    chain_list(Chain, Members),
    findall(N, (member(M, Members), get(M, class_name, N)), Names).

test(a_frame_without_a_status_bar_has_no_status_dialog, [fail]) :-
    frame(F, _App, _P),
    get(F, status_dialog, _).

%       A window that never prompts stays bare; one that does grows a bar
%       the first time it is wanted, which is what lets an editor dropped
%       into a window of terminals prompt on one.

test(a_bar_is_grown_when_it_is_first_wanted, true(Names == [pane_menu_dialog,
                                                           pane_tabbed_window,
                                                           pane_status_dialog])) :-
    frame(F, _App, _P),
    get(F, ensure_status_dialog, _),
    get(F, members, Chain),
    chain_list(Chain, Members),
    findall(N, (member(M, Members), get(M, class_name, N)), Names).

test(and_only_one_is_ever_grown, true(SD1 == SD2)) :-
    frame(F, _App, _P),
    get(F, ensure_status_dialog, SD1),
    get(F, ensure_status_dialog, SD2).

test(showing_no_line_number_does_not_grow_one, [fail]) :-
    frame(F, _App, _P),
    send(F, show_line_number, @nil),
    get(F, status_dialog, _).

test(showing_one_does, true(Shown == 'Line: 42')) :-
    frame(F, _App, _P),
    send(F, show_line_number, 42),
    get(F, status_dialog, SD),
    get(SD, member, line, Text),
    get(Text?string, value, Shown).

test(the_first_pane_is_the_current_one, true(P == P0)) :-
    frame(F, _App, P0),
    get(F, current_pane, P).

test(the_tab_in_view_is_the_one_holding_it, true(W == P)) :-
    frame(F, _App, P),
    get(F, tab, Tab),
    get(Tab, current, W).

test(a_pane_reaches_its_frame_through_the_template, true(Frame == F)) :-
    frame(F, _App, P),
    get(P, pane_frame, Frame).

:- end_tests(pane_frame_structure).

                 /*******************************
                 *           MENU BAR           *
                 *******************************/

:- begin_tests(pane_frame_menu_bar).

test(the_application_and_the_pane_both_fill_it,
     Menus == [file-[quit], edit-[alpha]]) :-
    frame(F, _App, _P),
    menus(F, Menus).

test(switching_tabs_rebuilds_it_for_the_new_pane,
     Menus == [file-[quit], edit-[beta]]) :-
    frame(F, _App, _P),
    pane(two, beta, P2),
    send(F, append_pane, P2, @default, @on),
    menus(F, Menus).

test(switching_back_rebuilds_it_again,
     Menus == [file-[quit], edit-[alpha]]) :-
    frame(F, _App, P1),
    pane(two, beta, P2),
    send(F, append_pane, P2, @default, @on),
    send(F, current_pane, P1),
    menus(F, Menus).

test(moving_the_focus_inside_a_tab_rebuilds_it,
     Menus == [file-[quit], edit-[gamma]]) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    menus(F, Menus).

%       The bar is rebuilt when <-menu_bar_key changes, and only then:
%       that is what keeps a click from rebuilding a native menu bar.

test(a_pane_asking_for_the_same_bar_does_not_rebuild_it, true(MB1 == MB2)) :-
    frame(F, _App, P1),
    pane(three, alpha, P3),             % same kind, same key
    send(F, split, P3, P1, vertically),
    get(F, menu_bar, MB),
    get(MB, member, edit, Popup1),
    get(Popup1, members, C1), get(C1, head, MB1),
    send(F, keyboard_focus, P1),
    get(MB, member, edit, Popup2),
    get(Popup2, members, C2), get(C2, head, MB2).

test(a_pane_asking_for_another_bar_does_rebuild_it, true(MB1 \== MB2)) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    get(F, menu_bar, MB),
    get(MB, member, edit, Popup1),
    get(Popup1, members, C1), get(C1, head, MB1),
    send(F, keyboard_focus, P1),
    get(MB, member, edit, Popup2),
    get(Popup2, members, C2), get(C2, head, MB2).

test(an_extension_is_put_on_at_once,
     Menus == [file-[quit], edit-[alpha], tools-[extra]]) :-
    frame(F, _App, _P),
    send(F, extend_menu_bar,
         message(@arg1, append, create(menu_item, extra), tools)),
    menus(F, Menus).

test(an_extension_survives_a_rebuild,
     Menus == [file-[quit], edit-[gamma], tools-[extra]]) :-
    frame(F, _App, P1),
    send(F, extend_menu_bar,
         message(@arg1, append, create(menu_item, extra), tools)),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    menus(F, Menus).

test(a_menu_item_without_a_message_goes_to_the_current_pane,
     true(Client == P)) :-
    frame(F, _App, P),
    get(F, menu_dialog, MD),
    get(MD, client, Client).

%       Alt-<char> to open a popup and the same again to run an item only
%       works on a bar XPCE draws itself, where the character is
%       underlined.  A popup has to be able to say which bar it is on --
%       through however many pull-rights -- for the assignment to know.

test(a_popup_on_the_bar_knows_the_bar, true(Bar == MB)) :-
    frame(F, _App, _P),
    get(F, menu_bar, MB),
    get(MB, member, file, Popup),
    get(Popup, menu_bar, Bar).

test(and_so_does_a_pull_right_under_it, true(Bar == MB)) :-
    frame(F, _App, _P),
    get(F, menu_bar, MB),
    get(MB, member, file, Popup),
    send(Popup, append, menu_item(more)),
    get(Popup, member, more, Item),
    send(Item, popup, new(Sub, popup(more))),
    get(Sub, menu_bar, Bar).

test(while_a_popup_on_no_bar_says_nothing, [fail]) :-
    new(P, popup(loose)),
    get(P, menu_bar, _).

test(a_popup_of_the_bar_leaves_its_items_alone, true(Accelerators == [@default])) :-
    frame(F, _App, _P),
    get(F, menu_bar, MB),
    get(MB, member, file, Popup),
    send(Popup, append, menu_item(alpha)),
    send(Popup, assign_accelerators),
    get(Popup, member, alpha, Item),
    get(Item, accelerator, A),
    Accelerators = [A].

:- end_tests(pane_frame_menu_bar).

                 /*******************************
                 *            LABEL             *
                 *******************************/

:- begin_tests(pane_frame_label).

test(the_application_says_how_the_title_is_made,
     Label == 'Test -- One') :-
    frame(F, _App, _P),
    get(F, label, Label).

test(the_title_follows_the_tab_in_view, Label == 'Test -- Two') :-
    frame(F, _App, _P),
    pane(two, beta, P2),
    send(F, append_pane, P2, @default, @on),
    get(F, label, Label).

test(renaming_a_tab_renames_the_frame, Label == 'Test -- Renamed') :-
    frame(F, _App, _P),
    get(F, tab, Tab),
    send(Tab, label, 'Renamed'),
    get(F, label, Label).

test(a_frame_of_its_own_overrules_the_application,
     Label == 'Just One') :-
    frame(F, _App, _P),
    send(F, label_format, 'Just %s'),
    get(F, label, Label).

test(no_format_at_all_is_the_bare_tab_label, Label == 'One') :-
    frame(F, _App, _P),
    send(F, label_format, @nil),
    get(F, label, Label).

%       A tab holding two panes is named after the one the user is
%       working in, and the title follows it.

test(the_tab_is_named_after_the_pane_with_the_focus, Label == 'Test -- Three') :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    get(F, label, Label).

test(and_follows_the_focus_back, Label == 'Test -- One') :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    send(F, keyboard_focus, P1),
    get(F, label, Label).

test(a_pane_may_say_what_a_window_showing_it_is_called,
     Label == 'Pane -- Three') :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(P3, own_title_format, 'Pane -- %s'),
    send(F, split, P3, P1, vertically),
    get(F, label, Label).

test(and_the_application_has_it_back_when_the_focus_moves_on,
     Label == 'Test -- One') :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(P3, own_title_format, 'Pane -- %s'),
    send(F, split, P3, P1, vertically),
    send(F, keyboard_focus, P1),
    get(F, label, Label).

test(a_tab_the_user_renamed_keeps_the_name_they_gave_it,
     Label == 'Test -- Mine') :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    get(F, tab, Tab),
    send(Tab, label_edited, 'Mine'),    % what the label editor sends
    send(F, keyboard_focus, P1),
    get(F, label, Label).

:- end_tests(pane_frame_label).


                 /*******************************
                 *             FOCUS            *
                 *******************************/

%       One class variable says whether the pointer entering a pane is
%       enough to give it the focus, for every pane of every window.

:- begin_tests(pane_frame_focus).

%!  enter(+Pane) is det.
%
%   Post the event a pointer entering Pane would deliver.

%   An event nothing acts on answers failure, which is not the same as
%   something going wrong, so the answer is not the point here.

enter(P) :-
    get(P, area, area(X, Y, _, _)),
    ignore(send(event(area_enter, P, X, Y), post, P)).

%!  with_focus_on_enter(+Bool, :Goal) is det.

:- meta_predicate with_focus_on_enter(+, 0).

with_focus_on_enter(Bool, Goal) :-
    get(@pce, convert, pane_frame, class, Class),
    get(Class, class_variable, focus_on_enter, Var),
    get(Var, value, Old),
    setup_call_cleanup(
        send(Class, class_variable_value, focus_on_enter, Bool),
        Goal,
        send(Class, class_variable_value, focus_on_enter, Old)).

test(the_pointer_alone_does_not_move_the_focus, true(Current == P1)) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    send(F, keyboard_focus, P1),
    with_focus_on_enter(@off, enter(P3)),
    get(F, current_pane, Current).

test(unless_it_is_asked_to, true(Current == P3)) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    send(F, keyboard_focus, P1),
    with_focus_on_enter(@on, enter(P3)),
    get(F, current_pane, Current).

:- end_tests(pane_frame_focus).


                 /*******************************
                 *          MOVING A PANE       *
                 *******************************/

%       A pane lives on a device, so it can change frame without its
%       `frame' slot ever changing and without any frame being told it
%       lost a member.  The frame it leaves has to let go of it, or it
%       stays that frame's keyboard focus and keeps the ->input_focus it
%       had -- which is edge triggered, so it would never be armed again
%       in the frame it moved to.

:- begin_tests(pane_frame_move).

%!  two_frames(-A, -B, -Pane) is det.
%
%   Two frames of two panes each, with Pane the current one of A and A
%   holding the focus, as if the window system had given it.

two_frames(A, B, Pane) :-
    frame(A, App, _P1),
    pane(second, alpha, P2),
    send(A, append_pane, P2, @default, @on),
    new(B, pane_frame(App, @default, new(P3, tp_pane))),
    send(P3, name, other),
    get(A, current_pane, Pane),
    send(A, input_focus, @on),
    send(A, keyboard_focus, Pane).

%!  move(+Pane, +Frame) is det.
%
%   Put Pane beside the current pane of Frame, as ->drop does.

move(Pane, F) :-
    get(F, current_pane, Rel),
    get(Rel, container, tab_frame, Tab),
    send(Tab, append, Pane, Rel, right),
    send(Tab, current, Pane).

test(the_frame_it_leaves_stops_naming_it, [fail]) :-
    two_frames(A, B, Pane),
    move(Pane, B),
    get(A, hypered, keyboard_focus, Pane).

test(the_pane_is_disarmed_so_that_it_can_be_armed_again,
     true(Focus == @off)) :-
    two_frames(_A, B, Pane),
    move(Pane, B),
    get(Pane, input_focus, Focus).

test(the_frame_it_arrives_in_names_it, true(Named == Pane)) :-
    two_frames(_A, B, Pane),
    move(Pane, B),
    get(B, hypered, keyboard_focus, Named).

%       `second' is the pane that moves: two_frames/3 exposed it last, so
%       it is the current one of A.

test(both_frames_keep_a_sensible_current_pane, true(Names == [one, second])) :-
    two_frames(A, B, Pane),
    move(Pane, B),
    get(A, current_pane, CA), get(CA, name, NA),
    get(B, current_pane, CB), get(CB, name, NB),
    CB == Pane,                         % the moved one is current in B
    Names = [NA, NB].

%       Dragging the grip moves a pane onto another one and clicking it
%       picks the pane up, so a window of its own is offered on a popup.

test(detaching_takes_the_pane_out_of_the_window_it_was_in,
     Names == [one]) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P2, detach),
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    findall(N, (member(P, Panes), get(P, name, N)), Names).

test(and_gives_it_a_window_of_its_own, true(Alone == [second])) :-
    frame(F, _App, _P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P2, detach),
    get(P2, frame, New),
    New \== F,
    get(New, panes, Chain),
    chain_list(Chain, Panes),
    findall(N, (member(P, Panes), get(P, name, N)), Alone).

test(the_new_window_belongs_to_the_same_application, true(App2 == App)) :-
    frame(F, App, _P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P2, detach),
    get(P2?frame, application, App2).

test(a_pane_that_is_already_alone_has_nowhere_to_go, [fail]) :-
    frame(_F, _App, P1),
    send(P1, detach).

%       And a pane sharing a tab with another can be given a tab of its
%       own, which is the other half of the same offer.

test(a_pane_in_a_split_can_be_given_a_tab_of_its_own,
     true(Sizes == 1-2)) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, split, P2, P1, vertically),
    send(P2, move_to_tab),
    get(P2?pane_tab?windows, size, InItsTab),
    get(F?tabs?tabs, size, Tabs),
    Sizes = InItsTab-Tabs.

test(and_stays_in_the_window_it_was_in, true(F2 == F)) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, split, P2, P1, vertically),
    send(P2, move_to_tab),
    get(P2, frame, F2).

test(while_a_pane_that_has_a_tab_already_has_nowhere_to_go, [fail]) :-
    frame(F, _App, _P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P2, move_to_tab).

%       And the way back: a pane that has a tab to itself folds into the
%       tab beside it, taking its own tab away with it.

test(a_lone_pane_moves_into_the_tab_before_its_own, true(Sizes == 2-1)) :-
    frame(F, _App, _P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P2, move_to_neighbour_tab, previous),
    get(P2?pane_tab?windows, size, InItsTab),
    get(F?tabs?tabs, size, Tabs),
    Sizes = InItsTab-Tabs.

test(and_shares_the_tab_of_the_pane_that_was_there) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P2, move_to_neighbour_tab, previous),
    get(P1, pane_tab, Tab),
    get(P2, pane_tab, Tab).

test(and_is_the_one_the_user_is_working_in, true(Current == P2)) :-
    frame(F, _App, _P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P2, move_to_neighbour_tab, previous),
    get(F, current_pane, Current).

test(the_tab_after_mine_works_the_same_way) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P1, move_to_neighbour_tab, next),
    get(P1, pane_tab, Tab),
    get(P2, pane_tab, Tab).

test(the_first_tab_has_none_before_it, [fail]) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P1, move_to_neighbour_tab, previous).

test(the_last_tab_has_none_after_it, [fail]) :-
    frame(F, _App, _P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    send(P2, move_to_neighbour_tab, next).

test(a_pane_sharing_a_tab_stays_where_it_is, [fail]) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, split, P2, P1, vertically),
    pane(third, alpha, P3),
    send(F, append_pane, P3, @default, @on),
    send(P2, move_to_neighbour_tab, next).

%       Each move is offered only where it changes something; closing is
%       always on offer.  What the popup shows is what the grip's own menu
%       shows: the same object.

test(the_only_pane_of_a_window_is_offered_no_move, true(Offered == [close])) :-
    frame(_F, _App, P1),
    grip_offers(P1, Offered).

test(a_pane_in_a_tab_of_its_own_is_offered_a_window_and_the_tab_before_it,
     true(Offered == [move_to_new_window, move_to_previous_tab, close])) :-
    frame(F, _App, _P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    grip_offers(P2, Offered).

test(a_lone_pane_with_a_tab_on_either_side_is_offered_both,
     true(Offered == [move_to_new_window,
                      move_to_previous_tab,
                      move_to_next_tab,
                      close])) :-
    frame(F, _App, _P1),
    pane(second, alpha, P2),
    send(F, append_pane, P2, @default, @on),
    pane(third, alpha, P3),
    send(F, append_pane, P3, @default, @on),
    grip_offers(P2, Offered).

test(a_pane_sharing_a_tab_is_offered_both,
     true(Offered == [move_to_new_window, move_to_new_tab, close])) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, split, P2, P1, vertically),
    grip_offers(P2, Offered).

test(and_closing_takes_it_out_of_the_window, Names == [one]) :-
    frame(F, _App, P1),
    pane(second, alpha, P2),
    send(F, split, P2, P1, vertically),
    send(P2, close_pane),
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    findall(N, (member(P, Panes), get(P, name, N)), Names).

%!  grip_offers(+Pane, -Items) is det.
%
%   What the popup on Pane's grip offers, once its conditions have run.

grip_offers(Pane, Items) :-
    new(H, split_handle),
    send(H, pane, Pane),
    send(@split_handle_popup, update, H),
    get(@split_handle_popup, members, Chain),
    chain_list(Chain, Members),
    findall(V,
            ( member(MI, Members),
              get(MI, active, @on),
              get(MI, value, V)
            ),
            Items),
    send(H, destroy).

:- end_tests(pane_frame_move).

                 /*******************************
                 *           OPACITY            *
                 *******************************/

:- begin_tests(pane_frame_opacity).

test(a_lone_pane_is_not_faded, Faded == []) :-
    frame(F, _App, _P),
    faded(F, Faded).

test(the_pane_without_the_focus_is_faded, Faded == [one]) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    faded(F, Faded).

test(the_focus_moving_back_swaps_them_round, Faded == [three]) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    send(F, keyboard_focus, P1),
    faded(F, Faded).

test(a_pane_that_says_nothing_takes_the_frames_word_for_it,
     Opacity =:= 1.0) :-
    frame(F, _App, P1),
    new(P3, tp_bare),
    send(P3, name, bare),
    send(F, split, P3, P1, vertically),
    send(F, keyboard_focus, P1),        % so that P3 is the faded one
    get(P3, opacity, Opacity).          % pane_frame says 1.0, i.e. off

:- end_tests(pane_frame_opacity).

                 /*******************************
                 *            PANES             *
                 *******************************/

:- begin_tests(pane_frame_panes).

test(a_pane_is_told_when_it_becomes_current, true(N >= 1)) :-
    frame(F, _App, P),
    get(F, current_pane, _),
    get(P, exposed, N).

test(a_tab_is_named_after_the_pane_it_holds, Label == 'One') :-
    frame(F, _App, _P),
    get(F, tab, Tab),
    get(Tab, label, Label).

test(all_panes_over_all_tabs, Names == [one, three, two]) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    pane(two, beta, P2),
    send(F, append_pane, P2, @default, @off),
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    findall(N, (member(P, Panes), get(P, name, N)), Names0),
    msort(Names0, Names).

test(deleting_a_pane_leaves_the_others, Names == [one]) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    send(F, delete_pane, P3, @on),
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    findall(N, (member(P, Panes), get(P, name, N)), Names).

%       What the last pane going means is the application's to say --
%       Epilog warns when the main console is closed with other windows
%       open.  A frame whose application says nothing is destroyed.

test(the_application_is_asked_what_an_empty_frame_means, true(Gone == F)) :-
    frame(F, App, P1),
    send(F, delete_pane, P1, @on),
    get(App, emptied, Gone).

test(and_a_frame_whose_application_says_nothing_is_destroyed, [fail]) :-
    new(P, tp_bare),
    new(F, pane_frame(@default, @default, P)),
    send(F, delete_pane, P, @on),
    object(F).

test(every_frame_gets_a_name_of_its_own, true(N1 \== N2)) :-
    frame(F1, _A1, _P1),
    frame(F2, _A2, _P2),
    get(F1, name, N1),
    get(F2, name, N2).

test(the_template_splits_a_pane_beside_itself, true(Panes == [P1, New])) :-
    frame(F, _App, P1),
    send(P1, split, vertically),
    get(F, tab, Tab),
    get(Tab, windows, Chain),
    chain_list(Chain, Panes),
    get(F, current_pane, New).          % the new one takes the focus

test(the_template_gives_a_pane_a_tab_of_its_own, true(Tabs == 2)) :-
    frame(F, _App, P1),
    send(P1, new_tab),
    get(F, tabs, TW),
    get(TW?tabs, size, Tabs).

test(the_template_closes_a_pane, Names == [one]) :-
    frame(F, _App, P1),
    pane(three, gamma, P3),
    send(F, split, P3, P1, vertically),
    send(P3, close_pane),
    get(F, panes, Chain),
    chain_list(Chain, Panes),
    findall(N, (member(P, Panes), get(P, name, N)), Names).

test(a_pane_that_refuses_to_close_stops_the_frame, true(Reply == @off)) :-
    frame(F, _App, P),
    send(P, closable, @off),
    get(F, can_close, Reply).

test(closing_a_frame_with_one_tab_destroys_it, [fail]) :-
    frame(F, _App, _P),
    send(F, close),
    object(F).

test(a_pane_that_refuses_keeps_the_frame_alive, true(Alive == true)) :-
    frame(F, _App, P),
    send(P, closable, @off),
    send(F, close),
    (   object(F) ->  Alive = true ;  Alive = false ).

test(a_pane_that_says_nothing_agrees_to_close, true(Reply == @on)) :-
    new(App, tp_app(test)),
    new(P, tp_bare),
    new(F, pane_frame(App, @default, P)),
    get(F, can_close, Reply).

:- end_tests(pane_frame_panes).

                 /*******************************
                 *          PLAIN PANE          *
                 *******************************/

%       Every message of the protocol is optional.  A window that answers
%       none of it must still make a usable frame.

:- begin_tests(pane_frame_plain_pane).

test(a_frame_is_built_around_it, true(Pane == P)) :-
    new(App, tp_app(test)),
    new(P, tp_bare),
    send(P, name, bare),
    new(F, pane_frame(App, @default, P)),
    get(F, current_pane, Pane).

test(the_menu_bar_is_the_applications_alone, Menus == [file-[quit]]) :-
    new(App, tp_app(test)),
    new(P, tp_bare),
    new(F, pane_frame(App, @default, P)),
    menus(F, Menus).

test(its_name_is_its_tab_label_and_its_title, Label == 'Test -- Bare') :-
    new(App, tp_app(test)),
    new(P, tp_bare),
    send(P, name, bare),
    new(F, pane_frame(App, @default, P)),
    get(F, label, Label).

test(a_frame_without_an_application_at_all, Label == 'SWI-Prolog -- Bare') :-
    new(P, tp_bare),
    send(P, name, bare),
    new(F, pane_frame(@default, @default, P)),
    get(F, label, Label).

:- end_tests(pane_frame_plain_pane).


                 /*******************************
                 *           MINIMUM            *
                 *******************************/

/* How small a pane may be made.

A window made full screen and shrunk back used to leave the pane at the
bottom below the edge of the window, where it could not be reached: a tab
with less room than its windows want left them where they were, and the
share-out of what there was drove a small pane to nothing.  A pane is
kept visible (`MIN_TILE_SIZE' of tile.c) while there is room for that,
and inside the window when there is not.
*/

:- begin_tests(pane_frame_minimum).

test(a_small_pane_is_not_shrunk_away, true(Kept == [20,20,20])) :-
    frame(F, _App, One, @off),
    pane(two, beta, Two),
    send(F, split, Two, One, below),
    resize(F, 600, 800),
    small_pane(Two, 40),                        % dragged down to 40 pixels
    findall(H,
            ( member(Height, [400, 200, 120]),
              resize(F, 600, Height),
              pane_area(Two, area(_,_,_,H))
            ),
            Kept),
    send(F, destroy).

test(and_the_one_beside_it_gets_what_is_left) :-
    frame(F, _App, One, @off),
    pane(two, beta, Two),
    send(F, split, Two, One, below),
    resize(F, 600, 800),
    small_pane(Two, 40),
    resize(F, 600, 800),
    pane_area(One, area(_,_,_,H0)),
    resize(F, 600, 200),
    pane_area(One, area(_,_,_,H)),
    H < H0,                                     % it gives what the other
    send(F, destroy).                           % may not

%       And when even that will not fit, a pane is squeezed to nothing
%       rather than left where it was, which is over whatever the window
%       is drawn beside.  A pane with nothing to show is not "outside":
%       it draws nowhere.

test(a_pane_that_shows_anything_is_inside_the_window,
     true(Outside == [])) :-
    frame(F, _App, One, @off),
    pane(two, beta, Two),
    send(F, split, Two, One, below),
    findall(Name-Area,
            ( member(Height, [800, 400, 200, 120, 60, 40, 20, 10, 800]),
              resize(F, 600, Height),
              member(P, [One, Two]),
              pane_area(P, area(_, Y, _, H)),   % the pattern is xpce's to
              H > 0,                            % fill in, not =/2's
              Y+H > Height,
              get(P, name, Name),
              Area = Y-H
            ),
            Outside),
    send(F, destroy).

test(and_comes_back_when_there_is_room_again) :-
    frame(F, _App, One, @off),
    pane(two, beta, Two),
    send(F, split, Two, One, below),
    resize(F, 600, 800),
    pane_area(Two, area(_,_,_,H0)),
    resize(F, 600, 40),                         % too small for both
    resize(F, 600, 800),
    pane_area(Two, area(_,_,_,H)),
    H =:= H0,                                   % as it was
    send(F, destroy).

:- end_tests(pane_frame_minimum).

%!  resize(+Frame, +W, +H) is det.
%
%   Lay Frame out at that size.  ->size asks the window system, which has
%   nothing to say headless; the area is what ->resize works from.

resize(F, W, H) :-
    get(F, area, Area),
    send(Area, set, @default, @default, W, H),
    send(F, resize).

%!  small_pane(+Pane, +Height) is det.
%
%   Make Pane as high as dragging the separator between it and the one
%   above would.

small_pane(P, Height) :-
    (   get(P, decoration, D), D \== @nil
    ->  Placed = D
    ;   Placed = P
    ),
    get(Placed, tile, Tile),
    send(Tile, height, Height).

%!  pane_area(+Pane, -Area) is det.

pane_area(P, Area) :-
    (   get(P, decoration, D), D \== @nil
    ->  Placed = D
    ;   Placed = P
    ),
    get(Placed, area, Area).


                 /*******************************
                 *          PANE TERM           *
                 *******************************/

/* A window can be written down as a Prolog term and built back from it.
   These check what goes into the term, that a window built from a term
   gives the same term again, and that a term naming something that
   cannot be made restores everything else all the same.
*/

:- begin_tests(pane_frame_term).

%!  described(-Frame, -Application) is det.
%
%   A frame of two tabs: the first holds a tp_pane beside a tp_pane over a
%   tp_bare, the second a tp_pane that has something to say about itself.

described(F, App) :-
    frame(F, App, A),
    send(A, setting, alpha),
    send(F, split, new(B, tp_pane), A, right),
    send(B, name, two),
    send(F, split, new(C, tp_bare), A, below),
    send(C, name, three),
    send(F, append_pane, new(D, tp_pane), second, @off),
    send(D, name, four),
    send(D, setting, delta).

test(a_pane_that_says_nothing_is_its_class_name, Content == tp_bare) :-
    frame(F, _App, _P),
    send(F, append_pane, new(Bare, tp_bare), bare, @off),
    send(Bare, name, bare),
    get(F, pane_term, pane_frame(_, [_, tab(_, Content)])).

test(a_pane_that_says_something_carries_its_options,
     Options == [setting(alpha)]) :-
    frame(F, _App, P),
    send(P, setting, alpha),
    get(F, pane_term, pane_frame(_, [tab(_, tp_pane(Options))])).

test(a_tab_with_one_pane_has_no_split, Content == tp_pane) :-
    frame(F, _App, _P),
    get(F, pane_term, pane_frame(_, [tab(_, Content)])).

test(a_split_tab_is_a_tree) :-
    described(F, _App),
    get(F, pane_term, pane_frame(_, [tab(_, Content)|_])),
    assertion(Content = horizontal([_-vertical([_-tp_pane([setting(alpha)]),
                                                _-current(tp_bare)]),
                                    _-tp_pane])).

test(the_tab_in_view_is_marked) :-
    described(F, _App),
    get(F, pane_term, pane_frame(_, [tab(First, _), tab(Second, _)])),
    assertion(memberchk(current(true), First)),
    assertion(\+ memberchk(current(true), Second)).

test(a_renamed_tab_says_so) :-
    frame(F, _App, P),
    get(P, container, tab_frame, Tab),
    send(Tab, rename, 'By hand'),
    get(F, pane_term, pane_frame(_, [tab(Options, _)])),
    assertion(memberchk(renamed(true), Options)),
    assertion(memberchk(label('By hand'), Options)).

test(a_window_of_its_own_label_format_says_so) :-
    frame(F, _App, _P),
    get(F, pane_term, pane_frame(Plain, _)),
    assertion(\+ memberchk(label_format(_), Plain)),
    send(F, label_format, 'Mine -- %s'),
    get(F, pane_term, pane_frame(Own, _)),
    assertion(memberchk(label_format('Mine -- %s'), Own)).

%       A window that was never opened has no geometry worth writing:
%       <-geometry answers 0x0+0+0 for one, and reading that back would
%       ask for a window of no size at all.

test(a_window_that_was_never_opened_writes_no_geometry) :-
    frame(F, _App, _P),
    get(F, pane_term, pane_frame(Options, _)),
    assertion(\+ memberchk(geometry(_), Options)).

test(describing_a_window_built_from_a_term_gives_the_same_term) :-
    described(F, App),
    get(F, pane_term, Term),
    open_pane_frame(Term, F2, [application(App), open(false)]),
    get(F2, pane_term, Again),
    assertion(Term == Again).

test(a_term_written_by_hand_is_enough,
     Shape == vertical([tp_pane,tp_pane])) :-
    new(App, tp_app(test)),
    open_pane_frame(pane_frame([], [tab([], vertical([tp_pane, tp_pane]))]),
                    F, [application(App), open(false)]),
    get(F, pane_term, pane_frame(_, [tab(_, Content)])),
    pane_term_shape(Content, Shape).

test(a_renamed_tab_keeps_its_name_when_it_is_built_back) :-
    frame(F, App, P),
    get(P, container, tab_frame, Tab),
    send(Tab, rename, 'By hand'),
    get(F, pane_term, Term),
    open_pane_frame(Term, F2, [application(App), open(false)]),
    get(F2, pane_term, pane_frame(_, [tab(Options, _)])),
    assertion(memberchk(label('By hand'), Options)),
    assertion(memberchk(renamed(true), Options)).

test(the_marked_tab_is_the_one_in_view) :-
    described(F, App),
    get(F, pane_term, pane_frame(FrameOptions, [First, Second])),
    open_pane_frame(pane_frame(FrameOptions, [Second, First]), F2,
                    [application(App), open(false)]),
    get(F2, pane_term, pane_frame(_, [tab(_, _), tab(Options, _)])),
    assertion(memberchk(current(true), Options)).

%       A class that insists on arguments is made for something live --
%       the debugger for a break level -- and cannot be built from a term.
%       What is around it is built all the same.

test(a_pane_that_cannot_be_made_is_left_out, Shape == tp_pane) :-
    new(App, tp_app(test)),
    pane_term_messages(
        open_pane_frame(pane_frame([], [tab([], vertical([tp_needs,
                                                          tp_pane]))]),
                        F, [application(App), open(false)]),
        Messages),
    get(F, pane_term, pane_frame(_, [tab(_, Content)])),
    pane_term_shape(Content, Shape),
    assertion(memberchk(pane_frame(needs_arguments(tp_needs)), Messages)).

test(a_kind_that_is_no_class_is_left_out) :-
    new(App, tp_app(test)),
    pane_term_messages(
        open_pane_frame(pane_frame([], [tab([], vertical([no_such_pane,
                                                          tp_pane]))]),
                        F, [application(App), open(false)]),
        Messages),
    get(F, pane_term, pane_frame(_, [tab(_, tp_pane)])),
    assertion(memberchk(pane_frame(no_class(no_such_pane)), Messages)).

test(a_term_with_nothing_in_it_opens_no_window, fail) :-
    new(App, tp_app(test)),
    pane_term_messages(
        open_pane_frame(pane_frame([], [tab([], no_such_pane)]),
                        _F, [application(App), open(false)]),
        _Messages).

%       A term sent to a window replaces what it held: the window ends up
%       holding what the term says and nothing else, and it is still the
%       window it was.

test(a_term_sent_to_a_window_replaces_what_it_held) :-
    described(F, _App),
    send(F, pane_term, pane_frame([], [tab([], tp_pane)])),
    get(F, pane_term, pane_frame(_, Tabs)),
    assertion(Tabs = [tab(_, tp_pane)]),
    get(F, panes, Panes),
    assertion(get(Panes, size, 1)).

test(a_pane_that_refuses_to_close_stops_the_replacement, fail) :-
    frame(F, _App, P),
    send(P, closable, @off),
    send(F, pane_term, pane_frame([], [tab([], tp_pane)])).

:- end_tests(pane_frame_term).

%!  pane_term_shape(+Content, -Shape) is det.
%
%   The content of a tab with the shares dropped, so that a test can
%   compare the shape alone.

pane_term_shape(current(Content), Shape) :-
    !,
    pane_term_shape(Content, Shape).
pane_term_shape(Content, Shape) :-
    compound(Content),
    Content =.. [Orientation, Shares],
    pane_term_orientation(Orientation),
    !,
    findall(S, ( member(Share, Shares),
                 pane_term_share(Share, Sub),
                 pane_term_shape(Sub, S)
               ), Subs),
    Shape =.. [Orientation, Subs].
pane_term_shape(Content, Content).

pane_term_orientation(horizontal).
pane_term_orientation(vertical).

pane_term_share(_-Content, Content) :- !.
pane_term_share(Content, Content).

%!  pane_term_messages(:Goal, -Messages) is semidet.
%
%   Run Goal with the messages it prints collected rather than printed.

:- meta_predicate pane_term_messages(0, -).

pane_term_messages(Goal, Messages) :-
    nb_setval(pane_term_messages, []),
    setup_call_cleanup(
        asserta((user:message_hook(Term, _, _) :-
                     pane_term_message(Term)), Ref),
        Goal,
        ( erase(Ref),
          nb_getval(pane_term_messages, Collected),
          reverse(Collected, Messages)
        )).

pane_term_message(Term) :-
    nb_getval(pane_term_messages, Old),
    nb_setval(pane_term_messages, [Term|Old]).


                 /*******************************
                 *         SPLIT BESIDE         *
                 *******************************/

/* A tool that belongs down an edge is put beside a group of panes at a
   share of their room, rather than beside whichever pane is current at
   half of it.
*/

:- begin_tests(pane_frame_split_beside).

%!  column(-Frame, -Panes) is det.
%
%   A frame whose tab holds one pane above another.

column(F, [A,B]) :-
    frame(F, _App, A),
    send(A, name, one),
    send(F, split, new(B, tp_pane), A, below),
    send(B, name, two).

pane_shape(Tree, Name) :-
    object(Tree),
    !,
    get(Tree, name, Name).
pane_shape(Tree, Shape) :-
    Tree =.. [Orientation, Shares],
    findall(S, ( member(Share, Shares),
                 pane_term_share(Share, Content),
                 pane_shape(Content, S)
               ), Subs),
    Shape =.. [Orientation, Subs].

test(a_pane_can_be_put_beside_them_all,
     Shape == horizontal([nav,vertical([one,two])])) :-
    column(F, [A,B]),
    new(Nav, tp_pane),
    send(Nav, name, nav),
    send(F, split_beside, Nav, chain(A,B), left),
    get(A, container, tab_frame, Tab),
    get(Tab, window_tree, Tree),
    pane_shape(Tree, Shape).

test(and_takes_the_share_it_was_given) :-
    column(F, [A,B]),
    new(Nav, tp_pane),
    send(Nav, name, nav),
    send(F, split_beside, Nav, chain(A,B), left, 0.2),
    get(A, container, tab_frame, Tab),
    get(Tab, window_tree, Tree),
    Tree = horizontal([Share-_|_]),
    assertion(abs(Share-0.2) < 0.02).

test(the_new_pane_gets_the_keyboard) :-
    column(F, [A,B]),
    new(Nav, tp_pane),
    send(F, split_beside, Nav, chain(A,B), left, 0.2),
    assertion(get(F, keyboard_focus, Nav)).

:- end_tests(pane_frame_split_beside).


                 /*******************************
                 *       ARRANGED BY HAND       *
                 *******************************/

/* A window only teaches the IDE anything once the user has arranged it by
   hand.  What the IDE does of its own accord must not count, or it would
   learn its own guesses back.
*/

:- begin_tests(pane_frame_arranged,
               [ setup(forget_arrangements),
                 cleanup(forget_arrangements)
               ]).

test(a_window_starts_saying_nothing, Arranged == @off) :-
    frame(F, _App, _P),
    get(F, arranged, Arranged).

%       And it holds no arrangement and no clock until it does.  A slot of
%       type `prolog' takes a Prolog term, so `none' rather than @nil says
%       "nothing yet": @nil is not one, and declaring it as the default
%       costs a warning for every window ever made.

test(and_holds_no_arrangement_and_no_clock, Slots == [none, none]) :-
    frame(F, _App, _P),
    get(F, slot, arrangement, Arrangement),
    get(F, slot, arranged_since, Since),
    Slots = [Arrangement, Since].

test(and_a_pane_the_ide_puts_there_says_nothing_either, Arranged == @off) :-
    frame(F, _App, _P),
    send(F, append_pane, new(_, tp_pane), @default, @on),
    get(F, arranged, Arranged).

test(nor_does_one_it_puts_beside_what_is_there, Arranged == @off) :-
    frame(F, _App, A),
    send(F, split, new(_, tp_pane), A, below),
    get(F, arranged, Arranged).

test(a_pane_split_by_hand_counts, Arranged == @on) :-
    frame(F, _App, P),
    send(P, split, vertically),
    get(F, arranged, Arranged).

test(a_pane_closed_by_hand_counts, Arranged == @on) :-
    frame(F, _App, A),
    send(F, split, new(B, tp_pane), A, below),
    send(B, close_pane),
    get(F, arranged, Arranged).

test(a_pane_moved_into_a_tab_of_its_own_counts, Arranged == @on) :-
    frame(F, _App, A),
    send(F, split, new(B, tp_pane), A, below),
    send(B, move_to_tab),
    get(F, arranged, Arranged).

test(and_a_tab_tells_the_window_it_is_in, Arranged == @on) :-
    frame(F, _App, P),
    get(P, container, tab_frame, Tab),
    send(Tab, arranged),
    get(F, arranged, Arranged).

%       What is learned is the time an arrangement is lived in.  The clock
%       is wound back rather than waited on.

test(an_arrangement_lived_in_reaches_the_store) :-
    frame(F, _App, A),
    send(F, split, new(_B, tp_pane), A, below),
    send(F, arranged),
    get_time(Now),
    Then is Now-7200,
    send(F, slot, arranged_since, Then),
    send(F, record_arrangement),
    assertion(pane_layouts:stored(_, _, Earned, _)),
    pane_layouts:stored(_, _, Earned, _),
    assertion(Earned > 7000),
    forget_arrangements.

test(and_one_that_lasted_seconds_does_not, fail) :-
    frame(F, _App, A),
    send(F, split, new(_B, tp_pane), A, below),
    send(F, arranged),
    send(F, record_arrangement),
    pane_layouts:stored(_, _, _, _).

:- end_tests(pane_frame_arranged).
