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
:- use_module(library(lists), [member/2]).

test_pane_frame :-
    run_tests([ pane_frame_structure,
                pane_frame_menu_bar,
                pane_frame_label,
                pane_frame_focus,
                pane_frame_move,
                pane_frame_opacity,
                pane_frame_panes,
                pane_frame_plain_pane
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

:- pce_end_class(tp_pane).

%       A pane that answers none of it.

:- pce_begin_class(tp_bare, window, "Pane answering nothing at all").
:- pce_end_class(tp_bare).

:- pce_begin_class(tp_app, application, "Application of a test frame").

label_format(_App, Fmt:name) :<-
    Fmt = 'Test -- %s'.

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

test(deleting_the_last_pane_takes_the_frame_with_it, [fail]) :-
    frame(F, _App, P1),
    send(F, delete_pane, P1, @on),
    object(F).

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
