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

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).


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
    setup_history,
    send(new(epilog(Title)), open).

%!  setup_history
%
%   Whether or not to transfer the history.

setup_history :-
    current_prolog_terminal(_,_),
    !.
setup_history :-
    set_prolog_flag(save_history, false).

:- dynamic
    current_prolog_terminal/2,  % ?Thread, ?TerminalObject
    terminal_input/6.           % TerminalObject, PTY, In, Out, Error,
                                % EditLine

:- pce_begin_class(prolog_terminal, terminal_image,
                   "Terminal for running a Prolog thread").

variable(goal_init,     prolog := version,    both, "Goal to run for init").
variable(goal,          prolog := prolog,     both, "Main goal").
variable(popup,         popup*,               get,  "Terminal popup").
variable(popup_gesture, popup_gesture*,       none, "Gesture to show menu").
variable(history,       {on,off,copy} := off, none, "Support history").
variable(save_history,  bool := @off,         none, "Save history on exit").

initialise(PT) :->
    "Create Prolog terminal"::
    send_super(PT, initialise),
    send(PT, name, terminal),
    send(PT, link_message, message(@receiver, open_link, @arg1)),
    send(PT, popup, new(P, epilog_popup)),
    Terminal = @event?receiver,
    send_list(P, append,
              [ menu_item(copy,
                          message(Terminal, copy),
                          condition := message(Terminal, has_selection),
                          accelerator := 'Ctrl+C'),
                menu_item(paste,
                          message(Terminal, paste),
                          accelerator := 'Ctrl+V'),
                menu_item(paste_quoted,
                          message(Terminal, paste_quoted),
                          end_group := @on,
                          accelerator := 'Ctrl+Y'),
                menu_item(split_horizontally,
                          message(Terminal, split, horizontally),
                          accelerator := 'Shift+ctrl+O'),
                menu_item(split_vertically,
                          message(Terminal, split, vertically),
                          end_group := @on,
                          accelerator := 'Shift+ctrl+E'),
                menu_item(interrupt,
                          message(Terminal, interrupt),
                          accelerator := 'Ctrl+C')
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
    delete_window,
    close_io.

delete_window :-
    thread_self(Me),
    retract(current_prolog_terminal(Me, PT)),
    !,
    save_history(PT),
    in_pce_thread(send(PT?frame, delete_epilog, PT?window)).
delete_window.

close_io :-
    (   current_predicate(el_unwrap/1)
    ->  catch(el_unwrap(user_input), error(_,_), true)
    ;   true
    ),
    close(user_input, [force(true)]),
    close(user_output, [force(true)]),
    close(user_error, [force(true)]).

open_link(_T, Href:name) :->
    "Open a clicked hyperlink"::
    tty_link(Href).

:- pce_group(prolog).

connect(PT) :->
    "Connect a Prolog thread to the terminal"::
    (   current_prolog_terminal(_Thread, PT)
    ->  true
    ;   connect(PT, _Title)
    ).

interrupt(PT) :->
    "Interrupt client Prolog process"::
    current_prolog_terminal(Thread, PT),
    current_signal(int, SIGINT, debug),
    thread_signal(Thread, SIGINT).

save_history(PT) :-
    retract(terminal_input(PT, _PTY, _In, _Out, _Err, EditLine)),
    EditLine == true,
    !,
    prolog_history(save).               % If loaded and `save_history` is `true`
save_history(_).

history_events(PT, Events:prolog) :<-
    "Get the CLI history of this terminal"::
    terminal_input(PT, _PTY, In, _Out, _Err, true),
    stream_property(In, file_no(Fd)),
    el_history_events(Fd, Events).

history_events(PT, Events:prolog) :->
    "Insert history events"::
    history_events(PT, Events).

%!  history_events(+PT, +History) is det.
%
%   Activate the history of the new thread.  History is one of
%
%     - []
%       No history
%     - load
%       Load the saved history for this directory
%     - list(Events)
%       Start with a list of events.  This is used if we split a window
%       to copy the contents of the parent.
%
%    Note that this runs in the newly   created thread and cannot invoke
%    methods on XPCE as that will deadlock.

history_events(_PT, []) :-
    !.
history_events(_PT, load) :-
    !,
    prolog_history(enable),
    '$load_history'.
history_events(PT, Events) :-
    terminal_input(PT, _PTY, In, _Out, _Err, true),
    stream_property(In, file_no(Fd)),
    reverse(Events, OldFirst),
    forall(member(_N-Line, OldFirst),
           el_add_history(Fd, Line)),
    editline:load_history_events(Events).

history(PT, Enabled:enable={on,off,copy}, Save:save=[bool]) :->
    "Enable/disable history"::
    default(Enabled, on, TheEnabled),
    default(Save, @off, TheSave),
    send(PT, slot, history, TheEnabled),
    send(PT, slot, save_history, TheSave),
    ignore(activate_history(PT)).

activate_history(PT) :-
    terminal_input(PT, _PTY, _In, _Out, _Err, _EditLine),
    get(PT, slot, history, Enabled),
    get(PT, slot, save_history, Save),
    (   Enabled == off
    ->  prolog_history(disable)
    ;   Enabled == on
    ->  prolog_history(enable),
        '$load_history',
        (   Save \== @on
        ->  set_prolog_flag(save_history, false)
        ;   true
        )
    ;   true                            % e.g., `copy`
    ).

parent_history(PT, Events) :-
    get(PT, slot, history, copy),
    get(PT?window, hypered, parent, Parent),
    get(Parent, history_events, Events),
    !.
parent_history(PT, Events) :-
    get(PT, slot, history, on),
    Events = load.
parent_history(_PT, []).

paste_quoted(PT) :->
    "Paste as quoted material"::
    send(PT, send, "\u0019").         % Ctrl-Y

:- pce_group(event).

event(T, Ev:event) :->
    "Handle popup"::
    (   send_super(T, event, Ev)
    ->  (   send(Ev, is_a, activate_keyboard_focus)
        ->  send(T?frame, current_terminal, T)
        ;   true
        )
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

%!  connect(+PT, -Title) is det.

connect(PT, Title) :-
    get(PT, goal_init, Init),
    get(PT, goal, Goal),
    gensym(con, Alias),
    send(PT?window, name, Alias),
    get(PT, pty_name, PTY),
    thread_self(Me),
    parent_history(PT, Events),
    thread_create(thread_run_interactor(PT, Me, PTY, Init, Goal, Title,
                                        Events),
                  Thread,
                  [ detached(true),
                    alias(Alias),
                    at_exit(terminated)
                  ]),
    asserta(current_prolog_terminal(Thread, PT)),
    thread_get_message(Msg),
    (   Msg = title(Title0)
    ->  Title = Title0
    ;   Msg = throw(Error)
    ->  throw(Error)
    ;   Msg = false
    ->  fail
    ).

thread_run_interactor(PT, Creator, PTY, Init, Goal, Title, History) :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    Error = error(Formal,_),
    (   catch(attach_terminal(PT, PTY, Title, History), Error, true)
    ->  (   var(Formal)
        ->  thread_send_message(Creator, title(Title)),
            call(Init),
            call(Goal)
        ;   thread_send_message(Creator, throw(Error))
        )
    ;   thread_send_message(Creator, false)
    ).

attach_terminal(PT, PTY, _Title, History) :-
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
    call(el_wrap),
    register_input(PT, PTY, true, History).
attach_terminal(PT, PTY, _Title, History) :-
    open(PTY, read,  In,  [encoding(utf8), bom(false)]),
    open(PTY, write, Out, [encoding(utf8)]),
    open(PTY, write, Err, [encoding(utf8)]),
    set_stream(In,  file_name('')),
    set_prolog_IO(In, Out, Err),
    register_input(PT, PTY, false, History).

register_input(PT, PTY, EditLine, History) :-
    stream_property(In, alias(user_input)),
    stream_property(Out, alias(user_output)),
    stream_property(Err, alias(user_error)),
    asserta(terminal_input(PT, PTY, In, Out, Err, EditLine)),
    history_events(PT, History).


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
    get(Font, advance, m, EM),
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
    new(_, hyper(W, T, parent, child)),
    send(W, history, copy),
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

variable(current_window, name*, both, "Name of the current window").

initialise(T, Title:title=[name],
           Width:width=[integer], Height:height=[integer]) :->
    default(Title, "SWI-Prolog console", TheTitle),
    send_super(T, initialise, TheTitle),
    send(T, append, new(D, epilog_dialog)),
    new(W, epilog_window(@default, Width, Height)),
    (   current_prolog_terminal(_, _)
    ->  true
    ;   send(W, history, on)            % Use history on the first
    ),
    send(W, below, D).

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

:- pce_group(actions).

% Run several actions.  These should have output redirected to the
% current terminal.  How to do that?

current_terminal(Epilog, Terminal:prolog_terminal) :->
    "Set the current terminal"::
    send(Epilog, current_window, Terminal?window?name).

current_terminal(Epilog, Terminal:prolog_terminal) :<-
    "Get the current terminal of this frame"::
    (   get(Epilog, current_window, WindowName),
        WindowName \== @nil,
        get(Epilog, member, WindowName, Window)
    ->  true
    ;   get(Epilog?members, find,
            message(@arg1, instance_of, epilog_window), Window)
    ),
    get(Window, terminal, Terminal).

inject(Epilog, Command:prolog) :->
    "Inject a command into the current terminal"::
    get(Epilog, current_terminal, Term),
    format(string(Cmd), '~q.\r', [Command]),
    send(Term, send, Cmd).

consult(T) :->
    "Ask for a file and consult it"::
    findall(Ext, user:prolog_file_type(Ext, prolog), Exts),
    chain_list(Filter, Exts),
    get(@finder, file, open, tuple('Prolog file', Filter), File),
    send(T, inject, consult(File)).

edit_file(_T) :->
    "Ask for a file and edit it"::
    findall(Ext, user:prolog_file_type(Ext, source), Exts),
    chain_list(Filter, Exts),
    get(@finder, file, open, tuple('Source', Filter), File),
    edit(file(File)).

new_file(_T) :->
    "Ask for a file and create it"::
    findall(Ext, user:prolog_file_type(Ext, source), Exts),
    chain_list(Filter, Exts),
    get(@finder, file, save, tuple('Source', Filter), File),
    edit(file(File)).

make(T) :->
    "Run make/0"::
    send(T, inject, make).

quit(T) :->
    "Quit this terminal.  Optionally should terminate Prolog"::
    send(T, destroy).

interrupt(Epilog) :->
    "Interrupt running thread"::
    get(Epilog, current_terminal, Term),
    send(Term, interrupt).

ide(_T, Tool:name) :->
    "Open an IDE tool"::
    call(user:prolog_ide(Tool)).

preferences(_T, Which:{prolog,xpce}) :->
    "Edit Prolog or GUI preferences"::
    call(prolog_edit_preferences(Which)).

open_url(_T, URL:name) :->
    "Open a URL"::
    www_open_url(URL).

:- pce_end_class(epilog).

:- pce_begin_class(epilog_dialog, dialog, "Prolog terminator menu").

initialise(D) :->
    send_super(D, initialise),
    send(D, gap, size(0,0)),
    send(D, pen, 0),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File,     popup(file))),
    send(MB, append, new(Settings, popup(settings))),
    send(MB, append, new(Tools,    popup(tools))),
    send(MB, append, new(Help,     popup(help))),
    Epilog = @event?receiver?frame,
    send_list(File, append,
              [ menu_item(consult,
                          message(Epilog, consult)),
                menu_item(edit,
                          message(Epilog, edit_file)),
                menu_item(new,
                          message(Epilog, new_file),
                          end_group := @on),
                menu_item(reload_modified_files,
                          message(Epilog, make),
                          end_group := @on),
                menu_item(quit,
                          message(Epilog, quit))
              ]),
    send_list(Settings, append,
              [ menu_item(user_init_file,
                          message(Epilog, preferences, prolog)),
                menu_item('GUI_preferences',
                          message(Epilog, preferences, xpce))
              ]),
    send_list(Tools, append,
              [ menu_item(navigator,
                          message(Epilog, ide, open_navigator)),
                menu_item(view_threads,
                          message(Epilog, ide, thread_monitor)),
                menu_item(debug_messages,
                          message(Epilog, ide, debug_monitor)),
                menu_item(cross_referencer,
                          message(Epilog, ide, xref))
              ]),
    send_list(Help, append,
              [ menu_item('SWI-Prolog documentation',
                          message(Epilog, open_url,
                                  'https://www.swi-prolog.org')),
                menu_item('SWI-Prolog Discourse forum',
                          message(Epilog, open_url,
                                  'https://swi-prolog.discourse.group/'),
                          end_group := @on),
                menu_item('XPCE on SDL+Cairo',
                          message(Epilog, open_url,
                                  'https://github.com/SWI-Prolog/packages-xpce/wiki'))
              ]).

:- pce_end_class(epilog_dialog).

:- pce_begin_class(epilog_popup, popup, "Epilog styled popup").

class_variable(accelerator_font, font, small).

assign_accelerators(_) :->
    "Accelerators are defined by the window"::
    true.

:- pce_end_class(epilog_popup).



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
