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

:- module(test_tray_icon, [test_tray_icon/0]).

/** <module> Icons in the system tray

Creating a tray icon would show it on the desktop of the user running
the tests.  Therefore we disconnect from the desktop before SDL is
initialised: creating a tray icon must then fail.  On Windows and MacOS
the tray is part of the system, so we cannot disconnect and do not try
to create a tray icon.

The other tests only run if the environment variable `XPCE_TEST_TRAY`
is set.  Run them in an isolated session, e.g.

    Xvfb :99 &
    XPCE_TEST_TRAY=1 DISPLAY=:99 dbus-run-session -- \
        swipl -g test_tray_icon -t halt \
              packages/xpce/tests/test_tray_icon.pl
*/

isolated :-
    \+ getenv('XPCE_TEST_TRAY', _).

can_disconnect :-
    \+ current_prolog_flag(windows, true),
    \+ current_prolog_flag(apple, true).

%   GLib uses $XDG_RUNTIME_DIR/bus if there is no session bus address
%   and GTK finds the Wayland socket there, so remove that as well.

disconnect_desktop :-
    (   isolated
    ->  unsetenv('DISPLAY'),
        unsetenv('WAYLAND_DISPLAY'),
        unsetenv('XDG_RUNTIME_DIR'),
        setenv('DBUS_SESSION_BUS_ADDRESS', 'unix:path=/nonexistent')
    ;   true
    ).
setup_headless :-
    (   isolated
    ->  set_prolog_flag('SDL_VIDEODRIVER', dummy)
    ;   true
    ).

:- initialization(disconnect_desktop, now).   % before SDL copies the env
:- initialization(setup_headless, now).       % before SDL is initialised

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_tray_icon :-
    run_tests([ tray_icon ]).

:- dynamic
    clicked/1.

user:test_tray_clicked(X) :-
    assertz(test_tray_icon:clicked(X)).

clear :-
    retractall(clicked(_)).

clicks(Xs) :-
    findall(X, clicked(X), Xs).

:- meta_predicate
    pump(0).

pump(Goal) :-
    get_time(T0),
    repeat,
    (   call(Goal)
    ->  !
    ;   get_time(T),
        T - T0 > 3
    ->  !, fail
    ;   ignore(send(@display, dispatch)),
        fail
    ).

icon(I) :-
    new(I, image(@nil, 16, 16, pixmap)),
    send(I, fill, colour(blue)).

%!  menu(-Popup) is det.
%
%   A popup with two items and a sub popup with two items.

menu(P) :-
    new(P, popup(tray_menu)),
    send_list(P, append,
              [ menu_item(open, message(@prolog, test_tray_clicked, open)),
                menu_item(quit, message(@prolog, test_tray_clicked, quit))
              ]),
    new(Sub, popup(more, message(@prolog, test_tray_clicked, @arg1))),
    send_list(Sub, append, [menu_item(alpha), menu_item(beta)]),
    new(MI, menu_item(more)),
    send(MI, popup, Sub),
    send(P, append, MI).

click(T, Label, Clicks) :-
    clear,
    send(T, sdl_test_click, Label),
    pump(clicks([_|_])),
    clicks(Clicks).

:- begin_tests(tray_icon).

test(no_desktop, [condition((isolated, can_disconnect)), fail]) :-
    icon(I),
    new(_, tray_icon(I, 'Test')).
test(click, [condition(\+ isolated), Clicks == [quit]]) :-
    icon(I),
    menu(P),
    new(T, tray_icon(I, 'Test', P)),
    click(T, 'Quit', Clicks),
    send(T, free).
test(submenu, [condition(\+ isolated), Clicks == [beta]]) :-
    icon(I),
    menu(P),
    new(T, tray_icon(I, 'Test', P)),
    click(T, 'Beta', Clicks),
    send(T, free).
test(update, [condition(\+ isolated), Clicks == [new]]) :-
    icon(I),
    menu(P),
    new(T, tray_icon(I, 'Test', P)),
    send(P, append,
         menu_item(new, message(@prolog, test_tray_clicked, new))),
    send(T, update),
    click(T, 'New', Clicks),
    send(T, free).

:- end_tests(tray_icon).
