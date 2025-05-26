/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(epilog,
          [ epilog/1,                 % +Title
            epilog/0,
            win_window_color/2,       % +Which, +Color
            window_title/1
          ]).
:- use_module(library(pce)).
:- use_module(library(threadutil), []).
:- use_module(library(edit)).
:- use_module(library(pce_util)).
:- use_module(library(uri)).
:- use_module(library(www_browser)).
:- use_module(library(gensym)).

/** <module> XPCE Embedded terminals

This module implements embedded terminals   for XPCE. Embedded terminals
replace `swipl-win`, both the native Win32 version and the Qt version.

@tbd   The   current   version   leans   on    the   console   code   of
library(thread_util). Eventually, this should be properly merged.
@tbd Add a frame with menu bar and tabbed windows for the terminals. Or,
    split horizontal/vertical, like terminator.
*/

%!  epilog is det.
%!  epilog(+Title) is det.
%
%   Create a new terminal manager and open it.

epilog :-
    epilog(@default).
epilog(Title) :-
    send(new(epilog(Title)), open).

:- dynamic
    current_prolog_terminal/2.          % Thread, Terminal

:- pce_begin_class(prolog_terminal, terminal_image,
                   "Terminal for running a Prolog thread").

variable(goal_init,     prolog := version, both, "Goal to run for init").
variable(goal,          prolog := prolog,  both, "Main goal").
variable(popup,         popup*,         get,     "Terminal popup").
variable(popup_gesture, popup_gesture*, none,    "Gesture to show menu").

initialise(PT) :->
    "Create Prolog terminal"::
    send_super(PT, initialise),
    send(PT, name, terminal),
    send(PT, link_message, message(@receiver, open_link, @arg1)),
    send(PT, popup, new(P, popup)),
    Terminal = @event?receiver,
    send_list(P, append,
              [ menu_item(copy,
                          message(Terminal, copy),
                          condition := message(Terminal, has_selection),
                          accelerator := 'Ctrl+C'),
                menu_item(paste,
                          message(Terminal, paste),
                          end_group := @on,
                          accelerator := 'Ctrl+V'),
                menu_item(split_horizontally,
                          message(Terminal, split, horizontally),
                          accelerator := 'Shift+ctrl+O'),
                menu_item(split_vertically,
                          message(Terminal, split, vertically),
                          accelerator := 'Shift+ctrl+E')
              ]).

unlink(PT) :->
    (   retract(current_prolog_terminal(Thread, PT))
    ->  catch(terminate_thread(Thread), error(_,_), true)
    ;   true
    ),
    send_super(PT, unlink).

terminate_thread(Thread) :-
    thread_signal(Thread, clean_exit).

%!  clean_exit
%
%   Make the console thread exit immediately. Various things can happen,
%   depending in the timing. By setting   `debug_on_error`  to false, we
%   instruct the system not only to stop invoking the debugger, but also
%   to suppress print_message/2 reports if   the  `user_error` stream is
%   lost.

clean_exit :-
    set_prolog_flag(debug_on_error, false),
    thread_exit(console).

%!  terminated
%
%   Called from at_exit(Goal) option of the created thread.

terminated :-
    thread_self(Me),
    (   retract(current_prolog_terminal(Me, PT))
    ->  in_pce_thread(send(PT?frame, delete_epilog, PT?window))
    ;   true
    ),
    close_io.

close_io :-
    (   current_predicate(el_unwrap/1)
    ->  catch(el_unwrap(user_input), error(_,_), true)
    ;   true
    ),
    close(user_input, [force(true)]),
    close(user_output, [force(true)]),
    close(user_error, [force(true)]).

interrupt(PT) :->
    "Interrupt client Prolog process"::
    current_prolog_terminal(Thread, PT),
    current_signal(int, SIGINT, debug),
    thread_signal(Thread, SIGINT).

open_link(_T, Href:name) :->
    "Open a clicked hyperlink"::
    tty_link(Href).


connect(PT) :->
    "Connect a Prolog thread to the terminal"::
    (   current_prolog_terminal(_Thread, PT)
    ->  true
    ;   get(PT, goal_init, Init),
        get(PT, goal, Goal),
        connect(PT, Init, Goal, _Title)
    ).

:- pce_group(event).

event(T, Ev:event) :->
    "Handle popup"::
    (   send_super(T, event, Ev)
    ->  true
    ;   send(Ev, is_a, ms_right_down)
    ->  send(T, show_popup, Ev)
    ).

typed(T, Ev:event) :->
    (   get(Ev, id, Id),
        typed_epilog(Id, T, Ev)
    ->  true
    ;   send_super(T, typed, Ev)
    ).

typed_epilog(15, T, Ev) :-              % Shift+Ctrl+o
    send(Ev, has_modifier, sc),
    send(T, split, horizontally).
typed_epilog(5, T, Ev) :-              % Shift+Ctrl+e
    send(Ev, has_modifier, sc),
    send(T, split, vertically).

copy(T) :->
    "Copy selection to clipboard"::
    send(T?display, copy, T?selected).

split(T, Dir:{horizontally,vertically}) :->
    "Split this terminal"::
    send(T?window, split, Dir).

popup(T, Popup:popup*) :->
    "Associate a menu"::
    send(T, slot, popup, Popup),
    (   Popup == @nil
    ->  send(T, slot, popup_gesture, @nil)
    ;   send(T, slot, popup_gesture, popup_gesture(Popup))
    ).

show_popup(T, Ev:event) :->
    "Open popup if this is defined"::
    (   get(T, slot, popup_gesture, G),
        G \== @nil
    ->  send(G, event, Ev)
    ).


                /*******************************
                *     MANAGE PROLOG THREAD     *
                *******************************/

%!  connect(+TI, :Init, :Goal, -Title) is det.

connect(TI, Init, Goal, Title) :-
    gensym(con, Alias),
    get(TI, pty_name, PTY),
    thread_self(Me),
    thread_create(thread_run_interactor(Me, PTY, Init, Goal, Title),
                  Thread,
                  [ detached(true),
                    alias(Alias),
                    at_exit(terminated)
                  ]),
    asserta(current_prolog_terminal(Thread, TI)),
    thread_get_message(Msg),
    (   Msg = title(Title0)
    ->  Title = Title0
    ;   Msg = throw(Error)
    ->  throw(Error)
    ;   Msg = false
    ->  fail
    ).

thread_run_interactor(Creator, PTY, Init, Goal, Title) :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    Error = error(Formal,_),
    (   catch(attach_terminal(PTY, Title), Error, true)
    ->  (   var(Formal)
        ->  thread_send_message(Creator, title(Title)),
            call(Init),
            call(Goal)
        ;   thread_send_message(Creator, throw(Error))
        )
    ;   thread_send_message(Creator, false)
    ).

attach_terminal(PTY, _Title) :-
    exists_source(library(editline)),
    use_module(library(editline)),
    !,
    open(PTY, read,  In,  [encoding(utf8), bom(false)]),
    open(PTY, write, Out, [encoding(utf8)]),
    open(PTY, write, Err, [encoding(utf8)]),
    set_stream(In,  file_name('')),     % kill source_location/2
    set_stream(Out, buffer(line)),
    set_stream(Err, buffer(false)),
    set_stream(In,  eof_action(reset)),
    set_stream(In,  tty(true)),
    set_stream(Out, tty(true)),
    set_stream(Err, tty(true)),
    set_stream(In,  alias(user_input)),
    set_stream(Out, alias(user_output)),
    set_stream(Err, alias(user_error)),
    set_stream(In,  alias(current_input)),
    set_stream(Out, alias(current_output)),
    call(el_wrap).
attach_terminal(PTY, _Title) :-
    open(PTY, read,  In,  [encoding(utf8), bom(false)]),
    open(PTY, write, Out, [encoding(utf8)]),
    open(PTY, write, Err, [encoding(utf8)]),
    set_stream(In,  file_name('')),
    set_prolog_IO(In, Out, Err).

%!  tty_link(+Link) is det.
%
%   Handle a terminal hyperlink to ``file://`` links

tty_link(Link) :-
    uri_file_name(Link, File),
    !,
    uri_components(Link, Components),
    uri_data(fragment, Components, Fragment),
    fragment_location(Fragment, File, Location),
    call(edit(Location)).
tty_link(URL) :-
    call(www_open_url(URL)).

fragment_location(Fragment, File, file(File)) :-
    var(Fragment),
    !.
fragment_location(Fragment, File, File:Line:Column) :-
    split_string(Fragment, ":", "", [LineS,ColumnS]),
    !,
    number_string(Line, LineS),
    number_string(Column, ColumnS).
fragment_location(Fragment, File, File:Line) :-
    atom_number(Fragment, Line).

:- pce_end_class(prolog_terminal).


                /*******************************
                *           TERMINAL           *
                *******************************/

:- pce_begin_class(epilog_window, window, "Implement an embedded terminal").

variable(terminal, prolog_terminal, get, "The terminal_image").
delegate_to(terminal).

initialise(T, Title:title=[name],
           Width:width=[integer], Height:height=[integer]) :->
    "Create from title, width and height"::
    default(Title, "SWI-Prolog console", TheTitle),
    default(Width, 80, TheWidth),
    default(Height, 25, TheHeight),
    new(TI, prolog_terminal),
    get(TI, class_variable_value, font, Font),
    send(TI, scroll_bar, new(SB, scroll_bar(TI, vertical))),
    get(Font, height, FH),
    get(Font, width, m, EM),
    get(SB, width, SBW),
    WH is round(TheHeight*FH),
    WW is round((TheWidth+2)*EM+SBW),
    send_super(T, initialise, TheTitle, size(WW,WH)),
    send(T, slot, terminal, TI),
    send(T, display, SB),
    send(T, display, TI),
    send(T, keyboard_focus, TI).

resize(T) :->
    "Place terminal and scrollbar"::
    get(T, size, size(TW, TH)),
    get(T, member, scroll_bar, SB),
    get(SB, width, SBW),
    send(SB, set, TW-SBW, 0, @default, TH),
    get(T, member, terminal, TI),
    send(TI, set, 0, 0, TW-SBW, TH).

create(T) :->
    "Create the terminal and attach a Prolog thread to it"::
    send_super(T, create),
    get(T, member, terminal, TI),
    send(TI, connect).

split(T, Dir:{horizontally,vertically}) :->
    "Add a new terminal below me"::
    new(W, epilog_window),
    send(W, goal_init, true),
    get(T, tile, Tile),
    send(Tile, can_resize, @on),
    (   Dir == horizontally
    ->  send(W, below, Tile)
    ;   send(W, right, Tile)
    ).

:- pce_end_class(epilog_window).

                /*******************************
                *            EPILOG            *
                *******************************/

:- pce_begin_class(epilog, frame,
                   "Multiple terminals and menu").

initialise(T, Title:title=[name],
           Width:width=[integer], Height:height=[integer]) :->
    default(Title, "SWI-Prolog console", TheTitle),
    send_super(T, initialise, TheTitle),
    send(T, append, new(D, epilog_dialog)),
    send(epilog_window(@default, Width, Height), below, D).

quit(T) :->
    "Quit this terminal.  Optionally should terminate Prolog"::
    send(T, destroy).

delete_epilog(T, W:window) :->
    "Remove an individual terminal"::
    (   send(W, instance_of, epilog_window),
        get(T?members, find_all,
            message(@arg1, instance_of, epilog_window),
            Terminals),
        send(Terminals, delete, W),
        \+ send(Terminals, empty)
    ->  send(T, delete, W)
    ;   send(T, destroy)
    ).

:- pce_end_class(epilog).

:- pce_begin_class(epilog_dialog, dialog, "Prolog terminator menu").

initialise(D) :->
    send_super(D, initialise),
    send(D, gap, size(0,0)),
    send(D, pen, 0),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File, popup(file))),
    Epilog = @event?receiver?frame,
    send_list(File, append,
              [ menu_item(quit,
                          message(Epilog, quit))
              ]).

:- pce_end_class(epilog_dialog).

                /*******************************
                *        COMPATIBILITY         *
                *******************************/

%!  win_window_color(+Which, +Color) is det.
%
%   Set console colours.

win_window_color(Which, Color) :-
    pce_colour(Color, Object),
    terminal(Term),
    set_colour(Which, Term, Object).

pce_colour(rgb(R,G,B), Name) =>
    format(atom(Name), '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+',
           [R,G,B]).
pce_colour(Atom, Name), atom(Atom) =>
    Name = Atom.

terminal(Term) :-
    thread_self(Me),
    current_prolog_terminal(Me, Term).

set_colour(foreground, Term, Color) =>
    send(Term, colour, Color).
set_colour(background, Term, Color) =>
    send(Term, background, Color).
set_colour(selection_foreground, Term, Color) =>
    get(Term, selection_style, Style),
    get(Style, clone, NewStyle),
    send(NewStyle, colour, Color),
    send(Term, selection_style, NewStyle).
set_colour(selection_background, Term, Color) =>
    get(Term, selection_style, Style),
    get(Style, clone, NewStyle),
    send(NewStyle, background, Color),
    send(Term, selection_style, NewStyle).

%!  window_title(+Title) is det.

window_title(Title) :-
    terminal(Term),
    send(Term?frame, label, Title).
