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


:- begin_tests(mixed_panes).

test(an_epilog_window_takes_an_editor, Classes == [epilog_window, emacs_view]) :-
    emacs,
    epilog_frame(@default, @default, @default, @off, @default, F),
    send(@prolog_ide, new_editor, F),
    classes(F, Classes).

test(a_pcemacs_window_takes_a_terminal, Classes == [emacs_view, epilog_window]) :-
    emacs,
    new(B, emacs_buffer(@nil, '*mixed-1*')),
    get(@emacs, frame, B, F),
    send(@emacs, new_terminal, F),
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
    send(@emacs, new_terminal, F),
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

:- end_tests(mixed_panes).
