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


:- module(test_button, [test_button/0]).

/** <module> Test class button: buttons with a popup

Run with:

    swipl -g test_button -t halt packages/xpce/tests/test_button.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_button :-
    run_tests([ button_popup,
                button_keyboard
              ]).

%   popup_buttons(-Dialog, -Split, -Menu, -Log)
%
%   An open dialog with a split button `more`, which has a message and
%   a popup, and a menu button `actions`, which only has a popup.  Log
%   collects the message calls.

popup_buttons(D, Split, MenuB, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append, new(Split, button(more, message(Log, append, more)))),
    send(Split, popup, new(P1, popup(more, message(Log, append, @arg1)))),
    send_list(P1, append, [first, second]),
    send(D, append, new(MenuB, button(actions, @nil)), right),
    send(MenuB, popup, new(P2, popup(actions, message(Log, append, @arg1)))),
    send_list(P2, append, [copy, paste]),
    send(D, open).

%   press(+Dialog, +Button, +Where)
%
%   Press the left mouse button on the label or the popup marker of
%   Button.  The popup opens when the button is released (see
%   class popup_gesture), so pressing tests the decision to open it
%   without needing a popup window, which the dummy video driver used
%   for the tests cannot create.

press(D, B, Where) :-
    position(B, Where, X, Y),
    post(D, ms_left_down, X, Y, 0x10, 1000).

click(D, B, Where) :-
    position(B, Where, X, Y),
    post(D, ms_left_down, X, Y, 0x10, 1000),
    post(D, ms_left_up, X, Y, 0, 1100).

position(B, Where, X, Y) :-
    get(B, area, area(X0, Y0, W, H)),
    (   Where == marker
    ->  X is X0+W-6
    ;   X is X0+8
    ),
    Y is Y0 + H//2.

post(D, Id, X, Y, Buttons, T) :-
    new(Ev, event(Id, D, X, Y, Buttons, T)),
    ignore(send(D, post_event, Ev)).

%   gesture(-Status, -Popup)
%
%   Status of the popup gesture and the popup it is working on.  Resets
%   the gesture.

gesture(Status, Popup) :-
    (   object(@'_popup_gesture')
    ->  G = @'_popup_gesture',
        get(G, slot, status, Status),
        get(G, slot, current, Popup),
        send(G, slot, status, inactive),
        send(G, slot, current, @nil),
        send(G, slot, context, @nil)
    ;   Status = inactive,
        Popup = @nil
    ).

%   popup_windows
%
%   True if popups can be opened: not so with the dummy video driver.

popup_windows :-
    \+ getenv('SDL_VIDEODRIVER', dummy).

dismiss(D) :-
    post(D, ms_left_down, 2, 2, 0x10, 5000),
    post(D, ms_left_up, 2, 2, 0, 5100).

%   below(+Button, -Offset)
%
%   Offset is the position of the popup relative to the bottom-left
%   corner of Button.  Where popups have a drop shadow (Windows, MacOS,
%   Wayland and X11 with a compositor), the window of the popup has room
%   for it and the popup is displayed inside it at <-position.

below(B, DX-DY) :-
    get(B, popup, P),
    get(P?window?frame, position, point(FX, FY)),
    get(P, position, point(PX, PY)),
    get(B, display_position, point(BX, BY)),
    get(B, height, H),
    DX is FX+PX-BX,
    DY is FY+PY-(BY+H).

:- begin_tests(button_popup).

test(label_runs_message, [Calls-Status == [more]-inactive]) :-
    popup_buttons(D, Split, _, Log),
    click(D, Split, label),
    chain_list(Log, Calls),
    gesture(Status, _),
    send(D, destroy).
test(marker_activates_popup, [Status == active, true(Popup == SP)]) :-
    popup_buttons(D, Split, _, _),
    press(D, Split, marker),
    gesture(Status, Popup),
    get(Split, popup, SP),
    send(D, destroy).
test(menu_button_label_activates_popup, [Status == active, true(Popup == MP)]) :-
    popup_buttons(D, _, MenuB, _),
    press(D, MenuB, label),
    gesture(Status, Popup),
    get(MenuB, popup, MP),
    send(D, destroy).
test(popup_below_button, [condition(popup_windows), Offset == 0-0]) :-
    popup_buttons(D, _, MenuB, _),
    click(D, MenuB, label),
    below(MenuB, Offset),
    dismiss(D),
    send(D, destroy).
test(popup_as_wide_as_button, [condition(popup_windows), true(PW >= BW)]) :-
    popup_buttons(D, _, MenuB, _),
    click(D, MenuB, label),
    get(MenuB, width, BW),
    get(MenuB?popup?window?frame, area, area(_, _, PW, _)),
    dismiss(D),
    send(D, destroy).

:- end_tests(button_popup).

%   key(+Dialog, +Id)
%
%   Type a key without modifiers.  SDL names Return, Escape and Tab
%   `RET`, `ESC` and `TAB`.

key(D, Id) :-
    post(D, Id, 0, 0, 0, 2000).

%   pressed_dialog(-Dialog, -Button, -Log)
%
%   A dialog with a button `run` and a default button `ok`.

pressed_dialog(D, Run, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append, new(Run, button(run, message(Log, append, run)))),
    send(D, append, new(Ok, button(ok, message(Log, append, ok))), right),
    send(Ok, default_button, @on),
    send(D, open),
    send(D, keyboard_focus, Run).

menu_bar_dialog(D, TI, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File, popup(file, message(Log, append, @arg1)))),
    send_list(File, append, [open, save]),
    send(D, append, new(TI, text_item(name, ''))),
    send(D, open),
    send(D, keyboard_focus, TI).

:- begin_tests(button_keyboard).

test(return_presses_focussed, Calls == [run]) :-
    pressed_dialog(D, _, Log),
    key(D, 'RET'),
    chain_list(Log, Calls),
    send(D, destroy).
test(space_presses_focussed, Calls == [run]) :-
    pressed_dialog(D, _, Log),
    key(D, 0' ),
    chain_list(Log, Calls),
    send(D, destroy).
test(space_opens_menu_button, [condition(popup_windows), true(Shown == @on)]) :-
    popup_buttons(D, _, MenuB, _),
    send(D, keyboard_focus, MenuB),
    key(D, 0' ),
    get(MenuB?popup, displayed, Shown),
    key(D, 'ESC'),
    send(D, destroy).
test(select_from_menu_button, [condition(popup_windows),
                               Calls-Status == [paste]-inactive]) :-
    popup_buttons(D, _, MenuB, Log),
    send(D, keyboard_focus, MenuB),
    key(D, 'RET'),
    key(D, cursor_down),
    key(D, 'RET'),
    chain_list(Log, Calls),
    gesture(Status, _),
    send(D, destroy).
test(down_opens_split_popup, [condition(popup_windows), Calls == [first]]) :-
    popup_buttons(D, Split, _, Log),
    send(D, keyboard_focus, Split),
    key(D, cursor_down),
    key(D, 'RET'),
    chain_list(Log, Calls),
    send(D, destroy).
test(escape_cancels_popup, [condition(popup_windows),
                            Calls-Open-Shown-Status ==
                            []-(@on)-(@off)-inactive]) :-
    popup_buttons(D, _, MenuB, Log),
    send(D, keyboard_focus, MenuB),
    key(D, 0' ),
    get(MenuB?popup, displayed, Open),
    key(D, 'ESC'),
    chain_list(Log, Calls),
    get(MenuB?popup, displayed, Shown),
    gesture(Status, _),
    send(D, destroy).
test(menu_bar_keyboard, [condition(popup_windows), Calls == [save]]) :-
    menu_bar_dialog(D, _, Log),
    post(D, 0'f, 0, 0, 0x4, 2000),      % Alt-F
    key(D, cursor_down),
    key(D, 'RET'),
    chain_list(Log, Calls),
    send(D, destroy).
test(escape_runs_cancel, [condition(native_dialog_style), Calls == [cancel]]) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append, new(TI, text_item(name, ''))),
    send(D, append, button(ok, message(Log, append, ok))),
    send(D, append, button(cancel, message(Log, append, cancel))),
    send(D, open),
    send(D, keyboard_focus, TI),
    key(D, 'ESC'),
    chain_list(Log, Calls),
    send(D, destroy).
test(f10_opens_menu, [condition(popup_windows), Calls == [open]]) :-
    menu_bar_dialog(D, _, Log),
    key(D, f10),
    key(D, 'RET'),
    chain_list(Log, Calls),
    send(D, destroy).
test(cues_while_alt, Mode == alt) :-
    get(class(dialog_item), class_variable, accelerator_cues, Var),
    get(Var, value, Mode).
test(keyboard_popup_shows_cues, [condition(popup_windows),
                                 Open-Closed == true-false]) :-
    menu_bar_dialog(D, _, _),
    post(D, 0'f, 0, 0, 0x4, 2000),      % Alt-F
    get(D, member, menu_bar, MB),
    get(MB, current, P),
    truth(get(P, attribute, keyboard, _), Open),
    key(D, 'ESC'),
    truth(get(P, attribute, keyboard, _), Closed),
    send(D, destroy).
test(plain_letter_in_popup, [condition(popup_windows), Calls == [save]]) :-
    menu_bar_dialog(D, _, Log),
    post(D, 0'f, 0, 0, 0x4, 2000),      % Alt-F
    key(D, 0's),                        % s, without Alt
    chain_list(Log, Calls),
    send(D, destroy).
test(f10_before_editor, [condition(popup_windows), Calls == [save]]) :-
    editor_frame(F, V, Log),
    key(V, f10),                        % the editor would take F10
    key(V, cursor_down),
    key(V, 'RET'),
    chain_list(Log, Calls),
    send(F, destroy).
test(f10_disabled, [condition(popup_windows),
                    setup(set_menu_bar_key(@nil, Old)),
                    cleanup(set_menu_bar_key(Old, _)),
                    Current == @nil]) :-
    editor_frame(F, V, _),
    key(V, f10),
    get(F?members?head, member, menu_bar, MB),
    get(MB, current, Current),
    send(F, destroy).
test(key_shows_mnemonics, [condition(popup_windows),
                           Before-After == false-true]) :-
    popup_buttons(D, _, MenuB, _),
    click(D, MenuB, label),             % opened with the mouse
    get(MenuB, popup, P),
    truth(get(P, attribute, keyboard, _), Before),
    key(D, cursor_down),
    truth(get(P, attribute, keyboard, _), After),
    key(D, 'ESC'),
    send(D, destroy).
test(submenu_select, [condition(popup_windows), Calls == [b]]) :-
    submenu_dialog(D, Log),
    post(D, 0'f, 0, 0, 0x4, 2000),      % Alt-F: File, at `open`
    key(D, cursor_down),                % recent
    key(D, cursor_right),               % opens the submenu, at a
    key(D, cursor_down),                % b
    key(D, 'RET'),
    chain_list(Log, Calls),
    send(D, destroy).
test(submenu_left_closes, [condition(popup_windows),
                           Open-Closed-Current == true-true-file]) :-
    submenu_dialog(D, _),
    post(D, 0'f, 0, 0, 0x4, 2000),
    key(D, cursor_down),
    key(D, cursor_right),
    file_popup(D, P),
    truth(get(P, slot, pullright, @nil), NotOpen),
    negate(NotOpen, Open),
    key(D, cursor_left),                % closes the submenu only
    truth(get(P, slot, pullright, @nil), Closed),
    get(D, member, menu_bar, MB),
    get(MB?current, name, Current),
    key(D, 'ESC'),
    send(D, destroy).
test(submenu_escape_closes, [condition(popup_windows),
                             Closed-Shown == true-(@on)]) :-
    submenu_dialog(D, _),
    post(D, 0'f, 0, 0, 0x4, 2000),
    key(D, cursor_down),
    key(D, cursor_right),
    key(D, 'ESC'),                      % closes the submenu only
    file_popup(D, P),
    truth(get(P, slot, pullright, @nil), Closed),
    get(P, displayed, Shown),
    key(D, 'ESC'),
    send(D, destroy).
test(click_after_keyboard_escape, [condition(popup_windows),
                                   Shown == @on]) :-
    menu_bar_dialog(D, _, _),
    post(D, 0'f, 0, 0, 0x4, 2000),      % Alt-F
    key(D, 'ESC'),
    click_menu_bar(D, Shown),
    key(D, 'ESC'),
    send(D, destroy).
test(click_after_keyboard_select, [condition(popup_windows),
                                   Calls-Shown == [save]-(@on)]) :-
    menu_bar_dialog(D, _, Log),
    post(D, 0'f, 0, 0, 0x4, 2000),
    key(D, cursor_down),
    key(D, 'RET'),
    chain_list(Log, Calls),
    click_menu_bar(D, Shown),
    key(D, 'ESC'),
    send(D, destroy).
test(menu_bar_escape, [condition(popup_windows),
                       Calls-Text == []-x]) :-
    menu_bar_dialog(D, TI, Log),
    post(D, 0'f, 0, 0, 0x4, 2000),
    key(D, 'ESC'),
    key(D, 0'x),                        % goes to the text_item again
    chain_list(Log, Calls),
    get(TI, selection, Text),
    send(D, destroy).

:- end_tests(button_keyboard).

native_dialog_style :-
    get(@pce, convert, key_binding, class, Class),
    get(Class, class_variable, dialog_style, Var),
    get(Var, value, Style),
    Style \== emacs.

truth(Goal, true) :- call(Goal), !.
truth(_, false).

%   editor_frame(-Frame, -View, -Log)
%
%   A frame with a dialog holding a menu_bar above a view, whose editor
%   has the keyboard focus.  The editor binds keys it does not know to
%   `undefined`, as PceEmacs does, which takes F10.

editor_frame(F, V, Log) :-
    new(Log, chain),
    new(F, frame),
    send(F, append, new(D, dialog)),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File, popup(file, message(Log, append, @arg1)))),
    send_list(File, append, [open, save]),
    send(new(V, view), below, D),
    send(V?editor, key_binding, f10, undefined),
    send(F, open),
    send(F, keyboard_focus, V).

set_menu_bar_key(New, Old) :-
    get(class(frame), class_variable, menu_bar_key, Var),
    get(Var, value, Old),
    send(Var, value, New).

%   submenu_dialog(-Dialog, -Log)
%
%   A dialog whose menu bar has File with `open`, `recent`, which has a
%   submenu holding a and b, and `quit`.

submenu_dialog(D, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File, popup(file, message(Log, append, @arg1)))),
    send(File, append, open),
    send(File, append, new(Recent, menu_item(recent))),
    send(Recent, popup, new(Sub, popup(recent))),
    send_list(Sub, append, [a, b]),
    send(File, append, quit),
    send(D, open).

file_popup(D, P) :-
    get(D, member, menu_bar, MB),
    get(MB, member, file, P).

negate(true, false).
negate(false, true).

%   click_menu_bar(+Dialog, -Shown)
%
%   Click (press and release quickly) on the first menu of the menu bar
%   of Dialog.  Shown is the <-displayed of its popup afterwards: a
%   click opens a menu that stays up.

click_menu_bar(D, Shown) :-
    get(D, member, menu_bar, MB),
    get(MB, area, area(X0, Y0, _, H)),
    X is X0+10,
    Y is Y0+H//2,
    post(D, ms_left_down, X, Y, 0x10, 5000),
    post(D, ms_left_up, X, Y, 0, 5100),
    get(MB, member, file, P),
    get(P, displayed, Shown).
