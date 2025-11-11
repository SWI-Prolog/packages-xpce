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
          [ epilog/0,
            epilog/1,                  % :Options
            epilog_attach/1,           % +Options
            ep_main/0,
            ep_has_console/1,          % +Thread
                                       % Adjust window
            set_epilog/1,	       % +Option
                                       % Misc helpers
            run_in_help_epilog/1       % :Goal
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
:- use_module(library(pce_openframes), [confirm_open_frames/1]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(error), [existence_error/2]).
:- use_module(library(prolog_code), [pi_head/2]).
:- use_module(library(thread), [call_in_thread/2, call_in_thread/3]).

:- meta_predicate
    epilog(:),
    set_epilog(:),
    run_in_help_epilog(0),
    win_insert_menu_item(+, +, +, 0).

:- pce_global(@epilog, new(epilog)).


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
    set_thread(self, debug(false)),
    capture_messages,
    ep_wait_.

ep_wait_ :-
    E = error(Formal,_),
    catch_with_backtrace(ep_wait__, E, true),
    (   var(Formal)
    ->  true
    ;   print_message(warning, E),
        ep_wait_
    ).

ep_wait__ :-
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
%       If `true`, act as main window.   In this case epilog/1
%       runs the main thread and returns after all windows have
%       been closed.

epilog :-
    epilog([]).

epilog(Options0) :-
    meta_options(is_meta, Options0, Options),
    fix_term,
    setup_history,
    option(name(Name),   Options, @default),
    option(title(Title), Options, @default),
    option(rows(Height), Options, @default),
    option(cols(Width),  Options, @default),
    option(main(IsMain), Options, @off),
    new(Epilog, epilog_frame(Name, Title, Width, Height, IsMain)),
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
    (   get(Epilog, main, @on)
    ->  ep_wait
    ;   true
    ).

is_meta(goal).
is_meta(init).

%!  epilog_attach(+Options) is det.
%
%   Attach an epilog window to the currently running thread.

epilog_attach(_Options) :-
    thread_self(Thread),
    current_prolog_terminal(Thread, PT),
    !,
    print_message(informational, epilog(already_attached(Thread, PT))).
epilog_attach(Options) :-
    thread_self(Thread),
    thread_property(Thread, id(TID)),
    fix_term,
    detach_context(RestoreContext),
    set_prolog_flag(save_history, false),
    in_pce_thread(create_epilog(TID, Options)),
    thread_get_message('$epilog'(PT, PTY)),
    prolog_listen(this_thread_exit, terminated),
    set_prolog_flag(query_debug_settings, debug(false, false)),
    set_prolog_flag(hyperlink_term, true),
    set_prolog_flag(color_term, true),
    attach_terminal(PT, PTY, _Title, []),
    asserta(current_prolog_terminal(Thread, PT)),
    asserta(attached_terminal(PT, RestoreContext)).

create_epilog(TID, Options) :-
    option(name(Name),   Options, @default),
    option(title(Title), Options, @default),
    option(rows(Height), Options, @default),
    option(cols(Width),  Options, @default),
    new(Epilog, epilog_frame(Name, Title, Width, Height, @off, TID)),
    send(Epilog, open).

detach_context(ctx(In,Out,Err)) :-
    stream_property(In, alias(user_input)),
    stream_property(Out, alias(user_output)),
    stream_property(Err, alias(user_error)).


restore_io(ctx(OIn,OOut,OErr)) :-
    dbg_format("Calling restore_io~n", []),
    unwrap_editline,
    stream_property(CIn, alias(user_input)),
    stream_property(COut, alias(user_output)),
    stream_property(CErr, alias(user_error)),
    dbg_format("Current streams: ~p~n", [t(CIn,COut,CErr)]),
    set_std_streams(OIn, OOut, OErr),
    close(CIn, [force(true)]),
    close(COut, [force(true)]),
    close(CErr, [force(true)]).

dbg_format(Fmt, Args) :-
    setup_call_cleanup(
        open("/proc/self/fd/2", write, Out),
        format(Out, Fmt, Args),
        close(Out)).


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

%!  ep_has_console(?Thread)
%
%   True when Thread has an Epilog console.

ep_has_console(Thread) :-
    current_prolog_terminal(Thread, _PT).

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
    attached_terminal/2,        % TerminalObject, RestoreInfo
                                % EditLine
    closed_epilog/2,            % Frame, Time
    active_terminal/1.          % TerminalObject

:- pce_begin_class(prolog_terminal, terminal_image,
                   "Terminal for running a Prolog thread").

variable(goal_init,     prolog := version,    both, "Goal to run for init").
variable(goal,          prolog := prolog,     both, "Main goal").
variable(popup,         popup*,               get,  "Terminal popup").
variable(popup_gesture, popup_gesture*,       none, "Gesture to show menu").
variable(history,       {on,off,copy} := off, none, "Support history").
variable(save_history,  bool := @off,         none, "Save history on exit").
variable(current_link,	name*,                get,  "Link under popup").

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
binding('\\C-\\S-w', close).
binding('\\C-\\S-m', make).
binding('\\C--',     font_reduce).
binding('\\C-=',     font_default).
binding('<f5>',      trace_mode).
binding('\\S-<f5>',  debug_mode).
binding('\\C-<f5>',  gui_debug).
binding('<f6>',      debugging).
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
    send(P, update_message, message(PT, update_popup, @receiver, @event)),
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
                menu_item(consult_linked_file,
                          message(Terminal, consult_link),
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
    catch(unlink_terminal_thread(PT), error(_,_), true),
    send_super(PT, unlink).

unlink_terminal_thread(PT) :- % Epilog attached to a running thread
    retract(attached_terminal(PT, RestoreContext)),
    retract(current_prolog_terminal(Thread, PT)),
    !,
    call_in_thread(Thread, restore_io(RestoreContext)).
unlink_terminal_thread(PT) :- % Normal Epilog window
    retract(current_prolog_terminal(Thread, PT)),
    !,
    thread_signal(Thread, clean_exit).
unlink_terminal_thread(_).


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

delete_window :-            % Epilog attached to running thread
    thread_self(Me),
    thread_property(Me, id(Id)),
    current_prolog_terminal(Me, PT),
    attached_terminal(PT, _RestoreContext),
    !,
    get_time(Now),
    format_time(string(T), '%+', Now),
    ansi_format(comment, '~N<thread ~w finished at ~s>~n', [Id, T]).
delete_window :-            % Normal Epilog
    thread_self(Me),
    current_prolog_terminal(Me, PT),
    !,
    save_history(PT),
    retractall(terminal_input(PT, _Pty, _In, _Out, _Error, _Edit)),
    retractall(current_prolog_terminal(Me, PT)),
    (   '$run_state'(normal)
    ->  in_pce_thread(send(PT?frame, delete_epilog, PT?window))
    ;   true
    ).
delete_window.

close_io :-
    unwrap_editline,
    close(user_input, [force(true)]),
    close(user_output, [force(true)]),
    close(user_error, [force(true)]).

unwrap_editline :-
    current_predicate(el_unwrap/1),
    '$run_state'(normal),            % hangs in el_end() on MacOS
    !,
    catch(el_unwrap(user_input), error(_,_), true).
unwrap_editline.

open_link(_T, Href:name) :->
    "Open a clicked hyperlink"::
    tty_link(Href).

:- pce_group(prolog).

connect(PT, TID:[name|int]) :->
    "Connect a Prolog thread to the terminal"::
    (   current_prolog_terminal(_Thread, PT)
    ->  true
    ;   connect(PT, TID, _Title)
    ).

update_popup(PT, P:popup, Ev:event) :->
    "Update the popup"::
    get(P, member, consult_linked_file, Item),
    (   get(PT, link, Ev, Link),
        link_file_location(Link, File, _Location)
    ->  send(Item, active, @on),
        send(PT, slot, current_link, Link),
        file_base_name(File, Base),
        send(Item, label, string('Consult %s', Base))
    ;   send(Item, active, @off),
        send(PT, slot, current_link, @nil),
        send(Item, label, 'Consult linked file')
    ).

consult_link(PT) :->
    "Consult linked file"::
    get(PT, current_link, Link),
    link_file_location(Link, File, _Location),
    send(PT, inject, consult(File)).

inject(PT, Command:prolog) :->
    "Inject Prolog goal in commandline"::
    plain_command(Command, Plain),
    format(string(Cmd), '~q.\r', [Plain]),
    send(PT, send, Cmd).

plain_command(_M:Command, Command) :-
    pi_head(PI, Command),
    current_predicate(user:PI),
    !.
plain_command(Command, Command).

clear_screen(PT) :->
    "Clean all output (cls)"::
    send(PT, insert, "\e[3J\e[H\e[2J\e[3J\r"),
    send(PT, send, "\f").               % Ctrl-L: re-prompt.

interrupt(PT) :->
    "Interrupt client Prolog process"::
    current_prolog_terminal(Thread, PT),
    current_signal(int, SIGINT, debug),
    thread_signal(Thread, SIGINT).

debug_mode(PT) :->
    "Toggle Prolog debug mode"::
    (   terminal_prolog_flag(PT, query_debug_settings,
                             debug(Debugging, _Tracing), -)
    ->  debug_toggle_command(Debugging, Negate),
        send(PT, inject, Negate)
    ;   true
    ).

debug_toggle_command(true, nodebug).
debug_toggle_command(false, debug).

trace_mode(PT) :->
    "Toggle Prolog trace mode"::
    (   terminal_prolog_flag(PT, query_debug_settings,
                             debug(_Debugging, Tracing), -)
    ->  trace_toggle_command(Tracing, Negate),
        send(PT, inject, Negate)
    ;   true
    ).

trace_toggle_command(true, notrace).
trace_toggle_command(false, trace).

debugging(PT) :->
    "Show debugging status"::
    send(PT, inject, debugging).

gui_debug(PT) :->
    "Toggle Prolog GUI tracer"::
    (   terminal_prolog_flag(PT, gui_tracer, GuiDebug, false)
    ->  gui_debug_toggle_command(GuiDebug, Negate),
        send(PT, inject, Negate)
    ;   true
    ).

gui_debug_toggle_command(true,  noguitracer).
gui_debug_toggle_command(false, guitracer).

make(PT) :->
    "Inject make/0"::
    send(PT, inject, make).

close(PT) :->
    "Close this Prolog shell"::
    get(PT, window, Window),
    get(PT, frame, Epilog),
    send(Epilog, delete_epilog, Window, @on),
    get_time(Now),
    asserta(closed_epilog(Epilog, Now)).

%!  save_history(+PrologTerminal) is det.
%
%   Save the history for PrologTerminal.

save_history(PT) :-
    terminal_input(PT, _PTY, _In, _Out, _Err, true),
    current_prolog_terminal(Thread, PT),
    !,
    call_in_thread(Thread,
                   catch(prolog_history(save), error(_,_), true),
                   [ timeout(0.1),
                     on_timeout(true)
                   ]).
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
    prolog_history(enable).
history_events(PT, Events) :-
    terminal_input(PT, _PTY, In, _Out, _Err, true),
    stream_property(In, file_no(Fd)),
    reverse(Events, OldFirst),
    forall(member(_N-Line, OldFirst),
           el_add_history(Fd, Line)).

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
        ;   send(Ev, is_a, 'RET')
        ->  retractall(active_terminal(_)),
            asserta(active_terminal(T))
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

:- pce_group(font).

resize_font(T, Factor:int) :->
    "Resize font to percentage, keep size in chars"::
    get(T, font, Font),
    get(Font, rescale, Factor, NewFont),
    send(T, font, NewFont),
    get(NewFont, points, Points),
    get(T, class_variable_value, font, DefaultFont),
    get(DefaultFont, points, DefPoints),
    Perc is round(Points*100/DefPoints),
    send(T, report, status, 'Resized to %d percent', Perc).

font_magnify(T) :->
    "Increase font 10%"::
    send(T, resize_font, 1.1).

font_reduce(T) :->
    "Decrease font 10%"::
    F is 1/1.1,
    send(T, resize_font, F).

font_default(T) :->
    "Use default font (size)"::
    get(T, class_variable_value, font, DefaultFont),
    send(T, font, DefaultFont),
    send(T, report, status, 'Resized to 100 percent').


                /*******************************
                *     MANAGE PROLOG THREAD     *
                *******************************/

%!  connect(+PT, +TID, -Title) is det.

connect(PT, @default, Title) =>
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
connect(PT, TID, _Title) =>
    thread_property(Thread, id(TID)),
    get(PT, pty_name, PTY),
    thread_send_message(Thread, '$epilog'(PT, PTY)).

%!  thread_run_interactor(+PrologTerminal, +CreatorThread, +PTY, +Init,
%!                        +Goal, +Title, +History) is det.
%
%   Run the Prolog terminal main thread. Note that this code cannot talk
%   to xpce as  it  will  deadlock.  That   is  why  all  relevant  xpce
%   interaction is done in connect/2 above.
%
%   Q: Will this still deadlock after changes to the XPCE "GIL"?

thread_run_interactor(PT, Creator, PTY, Init, Goal, Title, History) :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    set_prolog_flag(hyperlink_term, true),
    set_prolog_flag(color_term, true),
    set_prolog_flag(console_menu, true),
    Error = error(Formal,_),
    (   catch(attach_terminal(PT, PTY, Title, History), Error, true)
    ->  (   var(Formal)
        ->  thread_send_message(Creator, title(Title)),
            call(Init),
            ignore(Goal)
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
    set_std_streams(In, Out, Err),
    set_prolog_flag(tty_control, true),
    call(el_wrap([pipes(true)])),        % Option only for Windows
    register_input(PT, PTY, true, History).
attach_terminal(PT, PTY, _Title, History) :-
    pce_open_terminal_image(PT, In, Out, Err),
    set_prolog_IO(In, Out, Err),
    register_input(PT, PTY, false, History).

set_std_streams(In, Out, Err) :-
    set_stream(In,  alias(user_input)),
    set_stream(Out, alias(user_output)),
    set_stream(Err, alias(user_error)),
    set_stream(In,  alias(current_input)),
    set_stream(Out, alias(current_output)).

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
    link_file_location(Link, _File, Location),
    !,
    call(edit(Location)).
tty_link(URL) :-
    call(www_open_url(URL)).

link_file_location(Link, File, Location) :-
    uri_file_name(Link, File),
    !,
    uri_components(Link, Components),
    uri_data(fragment, Components, Fragment),
    fragment_location(Fragment, File, Location).

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
variable(tid,      [name|int],      get, "Attached thread").
delegate_to(terminal).

initialise(T, Title:title=[name],
           Width:width=[int], Height:height=[int], TID:[name|int]) :->
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
    send(T, slot, tid, TID),
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
    get(T, tid, TID),
    send(TI, connect, TID).

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

save_history(EW) :->
    "Save the commandline history"::
    get(EW, terminal, PT),
    save_history(PT).

:- pce_end_class(epilog_window).


                /*******************************
                *            EPILOG            *
                *******************************/

:- pce_begin_class(epilog_frame, frame,
                   "Multiple terminals and menu").

variable(current_window, name*,        both, "Name of the current window").
variable(main,		 bool := @off, both, "True if this is the main window").

initialise(T, Name:[name], Title:title=[name],
           Width:width=[int], Height:height=[int],
           Main:main=[bool], TID:[name|int]) :->
    default(Title, "SWI-Prolog console", TheTitle),
    send_super(T, initialise, TheTitle),
    default(Main, @off, IsMain),
    send(T, slot, main, IsMain),
    epilog_name(Name, IsMain, TheName),
    send(T, name, TheName),
    send(T, application, @epilog),
    send(T, done_message, message(@receiver, wm_close_requested)),
    send(T, append, new(D, epilog_dialog)),
    new(W, epilog_window(@default, Width, Height, TID)),
    send(T, current_window, W?name),
    (   current_prolog_terminal(_, _)
    ->  true
    ;   send(W, history, on)            % Use history on the first
    ),
    send(W, below, D).

epilog_name(@default, @on, main) :-
    !.
epilog_name(@default, _, Name) :-
    gensym(epilog, Name).
epilog_name(Name, _, Name).

destroy(Epilog) :->
    "Destroy the Epilog terminal"::
    send(Epilog, save_history),
    send_super(Epilog, destroy).

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
    ->  send(W, save_history),
        send(T, delete, W),
        (   Destroy == @on
        ->  send(W, destroy)
        ;   true
        )
    ;   send(T, terminate)
    ).

terminate(T) :->
    "Destroy this Epilog window"::
    (   get(T, main, @on)
    ->  send(T, destroy),
        confirm_open_frames(
            [ message("The main Prolog console was closed\n\c
                       while there are open windows")
            ])
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
    send(Term, inject, Command).

consult(T) :->
    "Ask for a file and consult it"::
    source_file_filter(Filter),
    working_directory(CWD, CWD),
    get(T?frame, open_file,
        filters := Filter,
        default := CWD,
        allow_many := @on, FileChain),
    chain_list(FileChain, Files),
    send(T, inject, consult(Files)).

edit_file(T) :->
    "Ask for a file and edit it"::
    source_file_filter(Filter),
    (   current_prolog_flag(associated_file, Default)
    ->  true
    ;   working_directory(Default, Default)
    ),
    get(T?frame, open_file,
        filters := Filter,
        default := Default,
        File),
    edit(file(File)).

new_file(T) :->
    "Ask for a file and create it"::
    source_file_filter(Filter),
    working_directory(CWD, CWD),
    get(T?frame, save_file,
        filters := Filter,
        default := CWD,
        File0),
    ensure_prolog_extension(File0, File),
    edit(file(File)).

source_file_filter(Filter) :-
    findall(Ext, user:prolog_file_type(Ext, source), Exts),
    chain_list(ExtChain, Exts),
    new(Filter, chain(tuple('Source', ExtChain))).

%!  ensure_prolog_extension(+File0, -File) is det.
%
%   Ensure File has a Prolog extension.

ensure_prolog_extension(File0, File) :-
    file_name_extension(_, Ext, File0),
    user:prolog_file_type(Ext, prolog),
    !,
    File = File0.
ensure_prolog_extension(File0, File) :-
    file_name_extension(File0, pl, File).

make(T) :->
    "Run make/0"::
    send(T, inject, make).

close(T, Prolog:prolog=[bool]) :->
    "Close this terminal.  Optionally terminates Prolog"::
    send(T, destroy),
    (   Prolog == @on
    ->  assert(quit_requested)
    ;   true
    ).

save_history(Epilog) :->
    "Save pending history"::
    (   terminal_input(PT, _PTY, _In, _Out, _Err, true),
        get(PT, frame, Epilog),
        save_history(PT),
        fail
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

debug_mode(Frame) :->
    "Toggle Prolog debug mode"::
    get(Frame, current_terminal, Term),
    send(Term, debug_mode).

trace_mode(Frame) :->
    "Toggle Prolog trace mode"::
    get(Frame, current_terminal, Term),
    send(Term, trace_mode).

debugging(Frame, MI:menu_item) :->
    "Toggle Prolog debug mode"::
    get(Frame, current_terminal, Term),
    send(Term, debugging),
    send(MI, selected, @off).

gui_debug(Frame) :->
    "Toggle GUI tracer"::
    get(Frame, current_terminal, Term),
    send(Term, gui_debug).

update_debug_mode(Frame, MI:menu_item) :->
    "Updated the current debug mode"::
    (   epilog_prolog_flag(Frame, query_debug_settings,
                           debug(Debugging, _Tracing), -)
    ->  send(MI, selected, Debugging)
    ;   true
    ).

update_trace_mode(Frame, MI:menu_item) :->
    "Updated the current trace mode"::
    (   epilog_prolog_flag(Frame, query_debug_settings,
                           debug(_Debugging, Tracing), -)
    ->  send(MI, selected, Tracing)
    ;   true
    ).

update_gui_debug(Frame, MI:menu_item) :->
    (   epilog_prolog_flag(Frame, gui_tracer, GuiTracer, false)
    ->  send(MI, selected, GuiTracer)
    ;   true
    ).

epilog_prolog_flag(Frame, Flag, Value, Default) :-
    get(Frame, current_terminal, Term),
    terminal_prolog_flag(Term, Flag, Value, Default).

%!  terminal_prolog_flag(+Term, +Flag, -Value, +Default) is semidet.
%
%   Get the Prolog flat Flag for the toplevel thread running in Term. If
%   the flag is not defined, unify   Value  with Default. This predicate
%   uses a timeout of 0.1 seconds,   returning  Default on timeout. This
%   guarantees that the console will not  freeze   if  the thread is not
%   responsive.

terminal_prolog_flag(Term, Flag, Value, Default) :-
    current_prolog_terminal(Thread, Term),
    (   catch(call_in_thread(Thread,
                             current_prolog_flag(Flag, Value),
                             [ timeout(0.1),
                               on_timeout(fail)
                             ]),
              error(Formal,_),
              true)
    ->  var(Formal)
    ;   Value = Default
    ).


:- pce_end_class(epilog_frame).

:- pce_begin_class(epilog_dialog, dialog, "Prolog terminator menu").

initialise(D) :->
    send_super(D, initialise),
    send(D, gap, size(0,0)),
    send(D, pen, 0),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File,     epilog_popup(file))),
    send(MB, append, new(Settings, popup(settings))),
    send(MB, append, new(Tools,    popup(tools))),
    send(MB, append, new(Debug,    epilog_popup(debug))),
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
                          accelerator := 'Shift-Ctrl-M',
                          end_group := @on),
                menu_item(close,
                          message(Epilog, close),
                          accelerator := 'Shift-Ctrl-W'),
                menu_item(halt_prolog,
                          message(Epilog, close, @on))
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
                          message(Epilog, ide, xref),
                          end_group := @on),
                menu_item('Inspect GUI hierarchy',
                          message(Epilog, ide, visual_hierarchy),
                          end_group := @on)
              ]),
    send_list(Debug, append,
              [ new(TraceMode,
                    menu_item(trace_mode,
                              message(Epilog, trace_mode),
                              accelerator := 'F5')),
                new(DebugMode,
                    menu_item(debug_mode,
                              message(Epilog, debug_mode),
                              accelerator := 'Shift-F5')),
                new(GuiDebug,
                    menu_item('GUI_debugger',
                              message(Epilog, gui_debug),
                              accelerator := 'Ctrl-F5',
                              end_group := @on)),
                menu_item(show_debug_status,
                          message(Epilog, debugging, @arg1),
                          accelerator := 'F6')
              ]),
    send_list(Help, append,
              [ menu_item('SWI-Prolog documentation',
                          message(Epilog, open_url,
                                  'https://www.swi-prolog.org')),
                menu_item('SWI-Prolog Discourse forum',
                          message(Epilog, open_url,
                                  'https://swi-prolog.discourse.group/'),
                          end_group := @on),
                menu_item('SWI-Prolog GUI tools',
                          message(Epilog, open_url,
                                  'https://github.com/SWI-Prolog/packages-xpce/wiki'))
              ]),
    send(Debug, show_current, @on),
    send(Debug, multiple_selection, @on),
    send(DebugMode, condition, message(Epilog, update_debug_mode, DebugMode)),
    send(TraceMode, condition, message(Epilog, update_trace_mode, TraceMode)),
    send(GuiDebug,  condition, message(Epilog, update_gui_debug, GuiDebug)).

:- pce_end_class(epilog_dialog).

:- pce_begin_class(epilog_popup, popup, "Epilog styled popup").

class_variable(accelerator_font, font, small).

assign_accelerators(_) :->
    "Accelerators are defined by the window"::
    true.

:- pce_end_class(epilog_popup).


:- pce_begin_class(epilog, application,
                   "The Epilog terminal application").

initialise(E) :->
    send_super(E, initialise, epilog).

:- pce_end_class(epilog).



                /*******************************
                *     XPCE CONSOLE OUTPUT      *
                *******************************/

%!  capture_messages
%
%   Capture messages from XPCE's main thread in an Epilog console.

capture_messages :-
    asserta(( user:thread_message_hook(Term,Kind,Lines) :-
                 xpce_message(Term,Kind,Lines))).

:- dynamic in_interrupt_handler/0.

:- public xpce_message/3.
xpce_message(interrupt(begin), _, _) =>
    asserta(in_interrupt_handler),
    fail.
xpce_message(interrupt(end), _, _) =>
    retractall(in_interrupt_handler),
    fail.
xpce_message(_Term, Kind, Lines), Kind \== silent =>
    \+ in_interrupt_handler,
    xpce_epilog_console(_In,_Out,Error),
    print_message_lines(Error, kind(Kind), Lines).

%!  pce:xpce_console(-In,-Out,-Error) is semidet.
%
%   Tell xpce where to write console output.   If  this fails, output is
%   written to the `user_output` of the   calling  thread or the process
%   `stdout`.

:- multifile
    pce:xpce_console/3.

pce:xpce_console(In,Out,Error) :-
    current_prolog_flag(epilog, true),
    xpce_epilog_console(In,Out,Error),
    !.

xpce_epilog_console(In,Out,Error) :-
    thread_self(Me),
    current_prolog_terminal(Me, TerminalImage),
    terminal_input(TerminalImage, _PTY, In,Out,Error, _EditLine),
    !.
xpce_epilog_console(In,Out,Error) :-
    active_terminal(TerminalImage),
    terminal_input(TerminalImage,_PTY,In,Out,Error,_EditLine),
    !.
xpce_epilog_console(In,Out,Error) :-
    terminal_input(_Obj,_PTY,In,Out,Error,_EditLine).


                /*******************************
                *     TOPLEVEL INTEGRATION     *
                *******************************/

%!  prolog:set_app_file_config(+Files) is nondet.
%
%   Executed as forall(prolog:set_app_file_config(Files), true) to allow
%   the GUI to update.   This implementation sets the title.

:- multifile
    prolog:set_app_file_config/1.       % +Files

prolog:set_app_file_config([File|More]) :-
    (   More == []
    ->  Extra = []
    ;   Extra = ['...']
    ),
    atomic_list_concat(['SWI-Prolog --', File | Extra], ' ', Title),
    current_prolog_terminal(_, Term),
    send(Term?frame, label, Title).


                /*******************************
                *      DEDICATED WINDOWS       *
                *******************************/

%!  run_in_help_epilog(:Goal)
%
%   Run Goal in the `help` epilog frame.  Create this frame
%   if necessary.

run_in_help_epilog(Goal) :-
    get(@epilog, member, help, Epilog),
    !,
    send(Epilog, expose),
    send(Epilog, inject, Goal).
run_in_help_epilog(Goal) :-
    epilog([ title('SWI-Prolog -- help'),
             name(help),
             init(true)
           ]),
    get(@epilog, member, help, Epilog),
    !,
    send(Epilog, inject, Goal).


                /*******************************
                *              API             *
                *******************************/

%!  set_epilog(:Option) is det.
%
%   Modify the Epilog console attached to the calling thread. Option is
%   one of:
%
%     - title(+Title)
%     - foreground(+Color)
%     - background(+Color)
%     - selection_foreground(+Color)
%     - selection_background(+Color)
%     - menu(+Label, +Before)
%       Add a new popup to the Epilog   menu.  The popus is added before
%       Before. If Before is `-`, the new popup is added to the right.
%     - menu_item(+PopupName, +Item, +Before, :Goal)
%       Insert an item in the  Epilog   console  menu.  PopupName is the
%       popup in which to insert the item. Item  is the name for the new
%       item. If Item is `--`, a _separator_  is inserted. Before is the
%       name of the item before which to insert the new item. If this is
%       `-`, the item is appended.
%
%       Goal is _injected_ into  the  current   terminal  of  the Epilog
%       window. This implies that we assume  that the console is waiting
%       for the user. Eventually,  we  probably   want  a  more flexible
%       solution.
%
%   @error existence_error(epilog, Thread) if the   (calling) thread has
%   no attached Epilog window.

set_epilog(_:title(Title)) =>
    window_title(Title).
set_epilog(_:foreground(Color)) =>
    win_window_color(foreground, Color).
set_epilog(_:background(Color)) =>
    win_window_color(background, Color).
set_epilog(_:selection_foreground(Color)) =>
    win_window_color(selection_foreground, Color).
set_epilog(_:selection_background(Color)) =>
    win_window_color(selection_background, Color).
set_epilog(_:menu(Label, Before)) =>
    win_insert_menu(Label, Before).
set_epilog(M:menu_item(PopupName, Item, Before, Goal)) =>
    win_insert_menu_item(PopupName, Item, Before, M:Goal).

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
    current_prolog_terminal(Me, Term),
    !.
terminal(_) :-
    thread_self(Me),
    existence_error(epilog, Me).

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

%!  win_insert_menu(+Label, +Before) is det.
%
%   Add a new popup to  the  Epilog   menu.  The  popus  is added before
%   Before. If Before is `-`, the new popup is added to the right.

win_insert_menu(Label, Before) :-
    terminal(Term),
    win_insert_menu(Term, Label, Before).

win_insert_menu(Term, Label, Before) :-
    get(Term, frame, Epilog),
    get(Epilog, member, epilog_dialog, Dialog),
    get(Dialog, member, menu_bar, MB),
    mb_insert_menu(MB, Label, Before).

mb_insert_menu(MB, Label, '-') =>
    send(MB, append, popup(Label)).
mb_insert_menu(MB, Label, Before) =>
    send(MB, append, popup(Label), before := Before).


%!  win_insert_menu_item(+PopupName, +Item, +Before, :Goal) is det.
%
%   Insert an item in the Epilog console menu. PopupName is the popup in
%   which to insert the item. Item is the name for the new item. If Item
%   is `--`, a _separator_ is inserted. Before   is the name of the item
%   before which to insert the new item.  If   this  is `-`, the item is
%   appended.
%
%   Goal is _injected_ into the current   terminal of the Epilog window.
%   This implies that we assume that  the   console  is  waiting for the
%   user. Eventually, we probably want a more flexible solution.

win_insert_menu_item(PopupName, Item, Before, Goal) :-
    terminal(Term),
    win_insert_menu_item(Term, PopupName, Item, Before, Goal).

win_insert_menu_item(Term, PopupName, Item, Before, Goal) :-
    get(Term, frame, Epilog),
    get(Epilog, member, epilog_dialog, Dialog),
    get(Dialog, member, menu_bar, MB),
    get(MB, member, PopupName, Popup),
    insert_in_popup(Epilog, Popup, Item, Before, Goal).

insert_in_popup(_Epilog, Popup, '--', '-', _Goal) =>
    send(Popup, append, gap).
insert_in_popup(Epilog, Popup, Item, '-', Goal) =>
    message_to_prolog(Epilog, Goal, Msg),
    send(Popup, append, menu_item(Item, Msg)).
insert_in_popup(_Epilog, Popup, '--', Before, _Goal) =>
    get(Popup, member, Before, BeforeItem),
    send(BeforeItem, end_group, @on).
insert_in_popup(Epilog, Popup, Item, Before, Goal) =>
    message_to_prolog(Epilog, Goal, Msg),
    send(Popup, insert_before, Before, menu_item(Item, Msg)).

message_to_prolog(Epilog, Goal, Msg) :-
    new(Msg, message(Epilog, inject, prolog(Goal))).
