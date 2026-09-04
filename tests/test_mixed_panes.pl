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
:- use_module(library(lists), [member/2, subtract/3]).

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
    get(B, open, tab, F),
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
    get(B, open, tab, In),
    (   In == F ->  Landed = same_window ;  Landed = elsewhere ).

test(and_asking_twice_goes_back_to_the_view_it_made, true(Views == 1)) :-
    emacs,
    mixed_window(F),
    new(B, emacs_buffer(@nil, '*mixed-twice*')),
    get(B, open, tab, F),
    get(B, open, tab, F),
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

%!  mixed_window(-Frame) is det.
%
%   A window with a terminal and an editor below it.

mixed_window(F) :-
    no_frames,                          % it is the window PceEmacs finds
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F, @on),
    send(F, open).

:- end_tests(mixed_panes).
