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
          [ ep_main/0,
            epilog/1,                 % :Options
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
:- use_module(library(editline),
              [ el_unwrap/1, el_history_events/2,
                el_add_history/2, el_wrap/1
              ]).
:- use_module(library(lists), [reverse/2, member/2]).
:- use_module(library(option), [meta_options/3, option/3, option/2]).
:- use_module(library(prolog_history), [prolog_history/1]).
:- use_module(library(swi_preferences), [prolog_edit_preferences/1]).

:- meta_predicate
    epilog(:).

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

%!  ep_main
%
%   Run epilog as main goal

:- dynamic quit_requested/0.

ep_main :-
    epilog,
    ep_wait.

ep_wait :-
    repeat,
      pce_principal:pce_dispatch(-1, 0.25),         % fd, timeout
      ep_main_end,
    !,
    halt.

ep_main_end :-
    \+ send(@display_manager, has_visible_frames),
    !.
ep_main_end :-
    quit_requested.

%!  epilog is det.
%!  epilog(:Options) is det.
%
%   Create a new terminal and open it.  Options:
%
%     - title(+Title)
%     - rows(+Rows)
%       Height of the initial terminal in lines (default 25)
%     - cols(+Cols)
%       Width of the initial terminal in characters (default 80)
%     - init(:Goal)
%       Run Goal as initialization goal. Default is `version` for the
%       first and `true` for subsequent terminals.
%     - goal(:Goal)
%       Run Goal as REPL loop.  Default is `prolog`.
%     - main(+Bool)
%       If `true`, act as main window.

epilog :-
    epilog([]).

epilog(Options0) :-
    meta_options(is_meta, Options0, Options),
    fix_term,
    setup_history,
    option(title(Title), Options, @default),
    option(rows(Height), Options, @default),
    option(cols(Width), Options, @default),
    new(Epilog, epilog(Title, Width, Height)),
    get(Epilog, current_terminal, PT),
    (   option(init(Init), Options)
    ->  send(PT, goal_init, Init)
    ;   true
    ),
    (   option(goal(Goal), Options)
    ->  send(PT, goal, Goal)
    ;   true
    ),
    send(Epilog, open),
    (   option(main(true), Options)
    ->  ep_wait
    ;   true
    ).

is_meta(goal).
is_meta(init).

%!  fix_term
%
%   Ensure a sensible ``TERM`` setting. We  have   a  problem  if we use
%   `swipl` in a terminal that is not compatible with `xterm`.

fix_term :-
    current_prolog_flag(windows, true),
    !.
fix_term :-
    \+ current_prolog_flag(epilog, true),
    getenv('TERM', _),
    !.
fix_term :-
    setenv('TERM', xterm).

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
    terminal_input/6,           % TerminalObject, PTY, In, Out, Error,
                                % EditLine
    closed_epilog/2.            % Frame, Time

:- pce_begin_class(prolog_terminal, terminal_image,
                   "Terminal for running a Prolog thread").

variable(goal_init,     prolog := version,    both, "Goal to run for init").
variable(goal,          prolog := prolog,     both, "Main goal").
variable(popup,         popup*,               get,  "Terminal popup").
variable(popup_gesture, popup_gesture*,       none, "Gesture to show menu").
variable(history,       {on,off,copy} := off, none, "Support history").
variable(save_history,  bool := @off,         none, "Save history on exit").

%!  binding(?Key, ?Method)
%
%   Epilog specific bindings. These are  used   to  create  the `epilog`
%   key_binding object.

binding('\\C-y',     paste_quoted).
binding('\\C-\\S-o', split_horizontally). % Terminator compatibility
binding('\\C-\\S-e', split_vertically).
binding('\\C-\\S-i', new_window).
binding('\\C-\\S-k', clear_screen).       % Gnome terminal
binding('\\C-\\S-w', close).
:- if(current_prolog_flag(apple, true)).
binding(Key, Method) :-
    pce_keybinding:binding(apple, epilog, Bindings),
    member(Key = Method, Bindings).
:- endif.

key_binding(KB) :-
    get(@key_bindings, member, epilog, KB),
    !.
key_binding(KB) :-
    new(KB, key_binding(epilog, terminal)),
    forall(binding(Key, Method),
           send(KB, function, Key, Method)).

:- multifile pce_keybinding:alt_binding_function/2.

pce_keybinding:alt_binding_function(copy,      copy_or_interrupt).
pce_keybinding:alt_binding_function(interrupt, copy_or_interrupt).

epilog_accelerators(Popup, KeyBinding) :-
    get_chain(Popup, members, Items),
    (   member(Item, Items),
        get(Item, value, MethodName),
        get(KeyBinding, accelerator_label, MethodName, Accell),
        send(Item, accelerator, Accell),
        fail
    ;   true
    ).

initialise(PT) :->
    "Create Prolog terminal"::
    send_super(PT, initialise),
    key_binding(KB),
    send(PT, bindings, KB),
    send(PT, name, terminal),
    send(PT, link_message, message(@receiver, open_link, @arg1)),
    send(PT, popup, new(P, epilog_popup)),
    Terminal = @event?receiver,
    send_list(P, append,
              [ menu_item(copy,
                          message(Terminal, copy),
                          condition := message(Terminal, has_selection)),
                menu_item(paste,
                          message(Terminal, paste)),
                menu_item(paste_quoted,
                          message(Terminal, paste_quoted)),
                menu_item(select_all,
                          message(Terminal, select_all),
                          end_group := @on),
                menu_item(clear_screen,
                          message(Terminal, clear_screen),
                          end_group := @on),
                menu_item(split_horizontally,
                          message(Terminal, split_horizontally)),
                menu_item(split_vertically,
                          message(Terminal, split_vertically)),
                menu_item(new_window,
                          message(Terminal, new_window),
                          end_group := @on),
                menu_item(interrupt,
                          message(Terminal, interrupt),
                          end_group := @on),
                menu_item(close,
                          message(Terminal, close))
              ]),
    epilog_accelerators(P, KB).

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

log(Fmt, Argv) :-
    setup_call_cleanup(
        open('epilog.log', append, Out),
        format(Out, Fmt, Argv),
        close(Out)).

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
    (   '$run_state'(normal)
    ->  in_pce_thread(send(PT?frame, delete_epilog, PT?window))
    ;   true
    ).
delete_window.

close_io :-
    (   current_predicate(el_unwrap/1),
        '$run_state'(normal)            % hangs in el_end() on MacOS
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

clear_screen(PT) :->
    "Clean all output (cls)"::
    send(PT, insert, "\e[3J\e[H\e[2J\e[3J\r"),
    send(PT, send, "\f").               % Ctrl-L: re-prompt.

interrupt(PT) :->
    "Interrupt client Prolog process"::
    current_prolog_terminal(Thread, PT),
    current_signal(int, SIGINT, debug),
    thread_signal(Thread, SIGINT).

close(PT) :->
    "Close this Prolog shell"::
    get(PT, window, Window),
    get(PT, frame, Epilog),
    send(Epilog, delete_epilog, Window, @on),
    get_time(Now),
    asserta(closed_epilog(Epilog, Now)).

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

%!  parent_thread(+PT, -Thread) is det.
%
%   Find the thread of the terminal splitted.   We use this to clone its
%   Prolog flags.

parent_thread(PT, Thread) :-
    get(PT?window, hypered, parent, ParentEpilog),
    get(ParentEpilog, terminal, ParentPT),
    current_prolog_terminal(Thread, ParentPT),
    !.
parent_thread(_, main).


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

split(T, Dir:{horizontally,vertically}) :->
    "Split this terminal"::
    send(T?window, split, Dir).

split_horizontally(T) :->
    "Split terminal horizontally"::
    send(T, split, horizontally).

split_vertically(T) :->
    "Split terminal vertically"::
    send(T, split, vertically).

new_window(_T) :->
    "Open a new window"::
    epilog.

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
    get(PT, pty_name, PTY),             % /dev/pty* on Unix, @nil on Windows
    thread_self(Me),
    parent_history(PT, Events),
    parent_thread(PT, Parent),
    thread_create(thread_run_interactor(PT, Me, PTY, Init, Goal, Title,
                                        Events),
                  Thread,
                  [ inherit_from(Parent),
                    detached(true),
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

%!  thread_run_interactor(+PrologTerminal, +CreatorThread, +PTY, +Init,
%!                        +Goal, +Title, +History) is det.
%
%   Run the Prolog terminal main thread. Note that this code cannot talk
%   to xpce as  it  will  deadlock.  That   is  why  all  relevant  xpce
%   interaction is done in connect/2 above.

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
    pce_open_terminal_image(PT, In, Out, Err),
    set_stream(In,  eof_action(reset)),
    set_stream(In,  alias(user_input)),
    set_stream(Out, alias(user_output)),
    set_stream(Err, alias(user_error)),
    set_stream(In,  alias(current_input)),
    set_stream(Out, alias(current_output)),
    set_prolog_flag(tty_control, true),
    call(el_wrap([pipes(true)])),        % Option only for Windows
    register_input(PT, PTY, true, History).
attach_terminal(PT, PTY, _Title, History) :-
    pce_open_terminal_image(PT, In, Out, Err),
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
           Width:width=[int], Height:height=[int]) :->
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
           Width:width=[int], Height:height=[int]) :->
    default(Title, "SWI-Prolog console", TheTitle),
    send_super(T, initialise, TheTitle),
    send(T, done_message, message(@receiver, wm_close_requested)),
    send(T, append, new(D, epilog_dialog)),
    new(W, epilog_window(@default, Width, Height)),
    send(T, current_window, W?name),
    (   current_prolog_terminal(_, _)
    ->  true
    ;   send(W, history, on)            % Use history on the first
    ),
    send(W, below, D).

% ->wm_close_requested
%
% This  is  a  hack  around  MacOS,    where   Command-W  also  triggers
% SDL_EVENT_WINDOW_CLOSE_REQUESTED

wm_close_requested(T) :->
    "Handle close-request"::
    (   retract(closed_epilog(T, Time)),
        get_time(Now),
        Now-Time < 0.5
    ->  true
    ;   send(T, destroy)
    ).

delete_epilog(T, W:window, Destroy:[bool]) :->
    "Remove an individual terminal"::
    (   send(W, instance_of, epilog_window),
        get(T?members, find_all,
            message(@arg1, instance_of, epilog_window),
            Terminals),
        send(Terminals, delete, W),
        \+ send(Terminals, empty)
    ->  send(T, delete, W),
        (   Destroy == @on
        ->  send(W, destroy)
        ;   true
        )
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

quit(T, Prolog:prolog=[bool]) :->
    "Quit this terminal.  Optionally should terminate Prolog"::
    send(T, destroy),
    (   Prolog == @on
    ->  assert(quit_requested)
    ;   true
    ).

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
                          message(Epilog, quit)),
                menu_item(quit_prolog,
                          message(Epilog, quit, @on))
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
