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
    run_tests([ button_popup
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
    send(Split, popup, new(P1, popup(more))),
    send_list(P1, append, [first, second]),
    send(D, append, new(MenuB, button(actions, @nil)), right),
    send(MenuB, popup, new(P2, popup(actions))),
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
%   Offset is the position of the popup's frame relative to the
%   bottom-left corner of Button.

below(B, DX-DY) :-
    get(B, popup, P),
    get(P?window?frame, position, point(FX, FY)),
    get(B, display_position, point(BX, BY)),
    get(B, height, H),
    DX is FX-BX,
    DY is FY-(BY+H).

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
