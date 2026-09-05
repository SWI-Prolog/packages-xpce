/*  Part of SWI-Prolog

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

:- module(epilog,
          [ epilog/0,
            epilog/1,                  % :Options
            epilog_attach/1,           % +Options
            ep_main/0,
            epilog_frame/6,            % +Title,+W,+H,+Main,+TID,-Frame
            ep_has_console/1,          % +Thread
                                       % Adjust window
            set_epilog/1,	       % +Option
                                       % Misc helpers
            run_in_help_epilog/1       % :Goal
          ]).
:- use_module(library(pce)).
:- use_module(library(tabbed_window)).
:- use_module(library(tab_frame)).
:- use_module(library(pane_frame), [pane_frame_closed_tab/1]).
:- use_module(library(swi_ide), []).
:- use_module(library(pce_template)).
:- use_module(library(toolbar), []).
:- pce_autoload(partof_hyper, library(hyper)).
:- use_module(library(threadutil), []).
:- use_module(library(edit)).
:- use_module(library(pce_util)).
:- use_module(library(uri)).
:- use_module(library(www_browser)).
:- use_module(library(gensym)).
:- use_module(library(editline),
              [el_unwrap/1, el_history_events/2, el_add_history/2, el_wrap/1]).
:- use_module(library(solution_sequences), [distinct/2]).
:- use_module(library(lists), [reverse/2, member/2, memberchk/2, nth0/3]).
:- use_module(library(option),
              [meta_options/3, option/3, option/2, merge_options/3,
               select_option/3]).
:- use_module(library(prolog_history), [prolog_history/1]).
:- use_module(library(swi_preferences), [prolog_edit_preferences/1]).
:- use_module(library(pce_openframes), [confirm_open_frames/1]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(error),
              [existence_error/2, must_be/2, permission_error/3]).
:- use_module(library(prolog_code), [pi_head/2]).
:- use_module(library(thread), [call_in_thread/2, call_in_thread/3]).
:- autoload(library(pce_symbol_picker), [symbol_picker/1]).
:- autoload(library(pce_drop_target),
            [drop_target_event/4, drop_target_show_rejected/3]).
:- autoload(library(desktop), [desktop_open/1]).
:- autoload(library(process), [process_create/3, process_wait/2]).
:- autoload(library(shell), [shell_command/1]).

:- meta_predicate
    epilog(:),
    epilog_tab(+, :),
    set_epilog(:),
    run_in_help_epilog(0),
    win_insert_menu_item(+, +, +, 0).

:- multifile
    tty_link_hook/2,                                % +PT, +URL
    profile/2.                                      % +Profile, -Options



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

:- dynamic ep_main_running/0.

ep_main :-
    epilog([object(Epilog)]),
    ep_wait(Epilog).

ep_wait(Epilog) :-
    set_thread(self, debug(false)),
    get(Epilog, current_terminal, PT),
    capture_messages(PT),
    setup_call_cleanup(
        asserta(ep_main_running),
        ep_wait_,
        retractall(ep_main_running)).

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
    \+ ep_main_running.

%!  epilog is det.
%!  epilog(:ProfileOrOptions) is det.
%
%   Create a new terminal and open it. If  the argument is an atom it is
%   taken as a _profile name_  and   options  are  taken from profile/2.
%   Options:
%
%     - title(+Title)
%     - rows(+Rows)
%       Height of the initial terminal in lines (default 25)
%     - cols(+Cols)
%       Width of the initial terminal in characters (default 80)
%     - profile(+Name)
%       Take the default options from the profile Name.  See profile/2.
%       A profile/2 clause may use this option itself to refine another
%       profile.
%     - init(:Goal)
%       Run Goal as initialization goal. Default is `version` for the
%       first and `true` for subsequent terminals.
%     - goal(:Goal)
%       Run Goal as REPL loop.  Default is `prolog`.
%     - cwd(+Directory)
%       Directory in which to start an OS shell using shell/0.
%     - inject(+Spec)
%       Type Spec at the terminal once it is connected.  Spec is an item
%       or a list of items.  A string is typed as a line of text, any
%       other item as a Prolog goal (see ->inject).  Unlike init(:Goal),
%       which runs in the Prolog thread, this is read by whoever reads
%       the terminal, e.g., the shell of goal(shell).
%     - background(+Colour)
%       Background colour for the terminal.
%     - main(+Bool)
%       If `true`, act as main window.   In this case epilog/1
%       runs the main thread and returns after all windows have
%       been closed.
%     - object(-Epilog)
%       Get the xpce object reference for the created terminal.

epilog :-
    epilog([]).

epilog(M:Profile) :-
    atom(Profile),
    !,
    epilog(M:[profile(Profile)]).
epilog(M:Options0) :-
    option(profile(Profile), Options0, prolog),
    profile_options(Profile, ProfileOptions),
    merge_options(Options0, ProfileOptions, Options1),
    meta_options(is_meta, M:Options1, Options),
    fix_term,
    setup_history,
    option(name(Name),   Options, @default),
    option(title(Title), Options, @default),
    option(rows(Height), Options, @default),
    option(cols(Width),  Options, @default),
    option(main(IsMain), Options, @off),
    epilog_frame(Title, Width, Height, IsMain, @default, Epilog),
    epilog_name(Name, IsMain, TheName),
    send(Epilog, name, TheName),
    get(Epilog, current_terminal, PT),
    configure_terminal(PT, Profile, Options),
    ignore(send(PT?window, pane_exposed)),  % the tab was named before this
    ignore(option(object(Epilog), Options)),

    send(Epilog, open),
    (   get(Epilog, attribute, main, @on)
    ->  ep_wait(Epilog)
    ;   true
    ).

is_meta(goal).
is_meta(init).

%!  epilog_frame(+Title, +Width, +Height, +Main, +TID, -Frame) is det.
%
%   A window of the IDE running one terminal, not yet open.  Every window
%   belongs to @prolog_ide, whichever tool opened it, so that one holding
%   a terminal and an editor behaves the same however it was made.

epilog_frame(Title, Width, Height, Main, TID, F) :-
    new(W, epilog_window(@default, Width, Height, TID)),
    (   current_prolog_terminal(_, _)
    ->  true
    ;   send(W, history, on)            % use history on the first
    ),
    new(F, pane_frame(@prolog_ide, 'SWI-Prolog')),
    (   is_true(Main)
    ->  send(F, attribute, main, @on)
    ;   true
    ),
    send(F, append_terminal, W, @on),
    (   Title == @default
    ->  true
    ;   send(F, tab_label, Title)
    ).

%!  is_true(+Bool) is semidet.
%
%   True for either way of saying yes.  This used to be a method, whose
%   `bool' argument XPCE converted for us; epilog/1 takes main(true) from
%   an option list -- that is how boot/toplevel.pl asks for the console
%   that must not return to the toplevel -- while a caller inside XPCE
%   says @on.

is_true(@on).
is_true(true).

%!  epilog_name(+Spec, +IsMain, -Name) is det.
%
%   <-name of an Epilog window.  The main console is `main'; the rest are
%   numbered, so that every window can be found back by name.

epilog_name(@default, @on, main) :-
    !.
epilog_name(@default, _, Name) :-
    gensym(epilog, Name).
epilog_name(Name, _, Name).

%!  epilog_tab(+Frame, :Spec) is det.
%
%   Open a new tab in Frame.  Spec is a profile name or a list of options
%   as epilog/1 takes them; it is the same terminal in another place.

epilog_tab(Frame, M:Profile) :-
    atom(Profile),
    !,
    epilog_tab(Frame, M:[profile(Profile)]).
epilog_tab(Frame, M:Options0) :-
    option(profile(Profile), Options0, prolog),
    profile_options(Profile, ProfileOptions),
    merge_options(Options0, ProfileOptions, Options1),
    meta_options(is_meta, M:Options1, Options),
    new(W, epilog_window),
    get(W, terminal, PT),
    configure_terminal(PT, Profile, Options),
    send(Frame, append_terminal, W, @on),
    send(Frame, keyboard_focus, W).

%!  configure_terminal(+Terminal, +Profile, +Options) is det.
%
%   Set Terminal up for Profile.  Shared by the window epilog/1 opens and
%   the tab epilog_tab/2 adds to one.

configure_terminal(PT, Profile, Options) :-
    send(PT, profile, Profile),
    on_option(init(Init),            Options, send(PT, goal_init, Init)),
    on_option(goal(Goal),            Options, send(PT, goal, Goal)),
    on_option(cwd(CWD),              Options, send(PT, process_cwd, CWD)),
    on_option(inject(Text),          Options, set_inject(PT, Text)),
    on_option(background(Colour),    Options, send(PT, background, Colour)).

%!  on_option(+Option, +Options, :Goal) is det.
%
%   Run Goal if Option appears in Options.

:- meta_predicate
    on_option(+, +, 0).

on_option(Opt, Options, Goal) :-
    (   option(Opt, Options)
    ->  call(Goal)
    ;   true
    ).

%!  epilog_attach(+Options) is det.
%
%   Attach an epilog window to the currently running thread.

epilog_attach(_Options) :-
    thread_self(Thread),
    current_prolog_terminal(Thread, PT),
    !,
    print_message(informational, epilog(already_attached(Thread, PT))).
epilog_attach(_Options) :-
    thread_self(Thread),
    pce_thread(Thread),
    !,
    print_message(warning, epilog(cannot_attach(Thread))).
epilog_attach(Options) :-
    thread_self(Thread),
    thread_property(Thread, id(TID)),
    fix_term,
    detach_context(RestoreContext),
    set_thread(Thread, class(console)),
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
    epilog_frame(Title, Width, Height, @off, TID, Epilog),
    epilog_name(Name, @off, TheName),
    send(Epilog, name, TheName),
    send(Epilog, open).

detach_context(ctx(In,Out,Err,Class)) :-
    stream_property(In, alias(user_input)),
    stream_property(Out, alias(user_output)),
    stream_property(Err, alias(user_error)),
    thread_self(Self),
    ignore(thread_property(Self, class(Class))).


restore_io(ctx(OIn,OOut,OErr,Class)) :-
    dbg_format("Calling restore_io~n", []),
    unwrap_editline,
    stream_property(CIn, alias(user_input)),
    stream_property(COut, alias(user_output)),
    stream_property(CErr, alias(user_error)),
    dbg_format("Current streams: ~p~n", [t(CIn,COut,CErr)]),
    set_std_streams(OIn, OOut, OErr),
    close(CIn, [force(true)]),
    close(COut, [force(true)]),
    close(CErr, [force(true)]),
    (   atom(Class)
    ->  thread_self(Self),
        set_thread(Self, class(Class))
    ;   true
    ).

dbg_format(Fmt, Args) :-
    setup_call_cleanup(
        open("/proc/self/fd/2", write, Out),
        format(Out, Fmt, Args),
        close(Out)).


%!  fix_term
%
%   Ensure a sensible ``TERM`` setting. We  have   a  problem  if we use
%   `swipl` in a terminal that is not compatible with `xterm`.
%
%   We claim ``xterm-256color``: the  terminal   implements  the  string
%   capabilities of that description  and  the   colours  it  adds, from
%   ANSI 16 through the 256 colour cube  to   24  bit  RGB. What it does
%   ignore  are  the  attributes  it  cannot  draw  (dim, slow blink and
%   invisible), printer control and left/right margins.
%
%   ``TERM_PROGRAM`` must be updated along  with   it.  We  are not the
%   terminal we were started from, and  a   stale  value makes programs
%   apply that terminal's quirks to ours.   Notably ansi_get_color/2 does
%   not query a terminal that says it is `Apple_Terminal`, which would
%   leave every program started from an Epilog window unable to find out
%   the colours of the window it runs in.

fix_term :-
    current_prolog_flag(windows, true),
    !.
fix_term :-
    \+ current_prolog_flag(epilog, true),
    getenv('TERM', _),
    !.
fix_term :-
    setenv('TERM', 'xterm-256color'),
    setenv('TERM_PROGRAM', 'Epilog').

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
    active_terminal/1.          % TerminalObject

%!  current_profile(-Name, -Label) is nondet.
%
%   True when Name is a known profile name to be announced using Label.

current_profile(Name, Label) :-
    distinct(Name, (default_profile(Name,_) ; profile(Name,_))),
    Error = error(_,_),
    catch(profile_options(Name, Options), Error,
          ( print_message(warning, Error),   % a broken profile may not
            fail                             % take the others with it
          )),
    option(label(Label), Options, Name).

%!  profile_options(+Name, -Options) is det.
%
%   True when Options are the options to   create a new Epilog window in
%   the profile Name.  A  profile  that   names  another  in  a profile/1
%   option refines that one: its own options win.

profile_options(Name, Options) :-
    profile_options(Name, [], Options).

profile_options(Name, Seen, _) :-
    memberchk(Name, Seen),
    !,
    permission_error(inherit, epilog_profile, Name).
profile_options(Name, Seen, Options) :-
    own_profile_options(Name, Options0),
    profile_label(Name, Options0, Options1),
    (   select_option(profile(Super), Options1, Options2)
    ->  profile_options(Super, [Name|Seen], SuperOptions),
        merge_options(Options2, SuperOptions, Options)
    ;   Options = Options1
    ).

%!  profile_label(+Name, +Options0, -Options) is det.
%
%   Give a profile a label of its own before  it takes any from the one
%   it refines: a profile is announced by  its label, else its title, and
%   else its name.  Inheriting the label  would   announce  it as the very
%   profile it refines.

profile_label(_, Options0, Options0) :-
    option(label(_), Options0),
    !.
profile_label(Name, Options0, [label(Label)|Options0]) :-
    option(title(Label), Options0, Name).

own_profile_options(Name, Options) :-
    default_profile(Name, Opts0),
    profile(Name, UserOpts),
    !,
    merge_options(UserOpts, Opts0, Options).
own_profile_options(Name, Options) :-
    profile(Name, Options),
    !.
own_profile_options(Name, Options) :-
    default_profile(Name, Options),
    !.
own_profile_options(Name, _) :-
    existence_error(epilog_profile, Name).

%!  profile(?Name, ?Options) is nondet.
%
%   Multifile hook that  defines  available   profiles  for  new  Epilog
%   windows. Name is the name of the  profile   and Options is an option
%   list for epilog/1. Options defined  here   overrule those of a built
%   in profile with the same name.  A   profile/1  option in Options is a
%   profile to refine, taking its options for  the ones we do not give
%   ourselves.  The label is the exception:  a   profile  is announced by
%   its own label, else its own title,  and else its name.  Note that
%   goals in Options are qualified in the module that calls epilog/1.
%
%   For example, a shell in a fixed place to build in:
%
%       ```
%       epilog:profile(build,
%                      [ profile(shell),
%                        label('Build'),
%                        cwd('/home/me/src/swipl-devel/build'),
%                        inject(["ninja"])
%                      ]).
%       ```

%!  default_profile(?Name, ?Options) is nondet.
%
%   Built-in profiles: `prolog` runs the  Prolog   top level and `shell`
%   runs the user's shell. A profile/2 clause with the same name
%   overrules the options below.

default_profile(prolog, [ label('Prolog')
                        ]).
default_profile(shell,  [ title('SWI-Prolog OS shell'),
                          label('OS shell'),
                          init(true),
                          goal(shell)
                        ]).


                /*******************************
                *           CLASSES            *
                *******************************/

:- pce_begin_class(prolog_terminal, terminal_image,
                   "Terminal for running a Prolog thread").

variable(goal_init,     prolog := version,    both, "Goal to run for init").
variable(goal,          prolog := prolog,     both, "Main goal").
variable(profile,       name := prolog,       both, "Profile used to create").
variable(process_cwd,   [name]*,              both, "Directory for processes").
variable(inject_items,  prolog := '',         both, "Lines/goals to type when connected").
variable(popup,         popup*,               get,  "Terminal popup").
variable(popup_gesture, popup_gesture*,       none, "Gesture to show menu").
variable(block_popup,   popup*,               get,  "Popup for a block marker").
variable(block_gesture, popup_gesture*,       none, "Gesture to show that").
variable(history,       {on,off,copy} := off, none, "Support history").
variable(save_history,  bool := @off,         none, "Save history on exit").
variable(fold_previous, bool,                 both,
         "Fold the previous command when one is entered").
variable(current_link,	name*,                get,  "Link under popup").
variable(current_block,	terminal_block*,      get,  "Block under popup").

class_variable(inactive_opacity, num, 0.5,
               "Opacity if terminal does not have the focus").
class_variable(inject_tries, int, [windows(5), unix(20)],
               "Times to look for a reader of inject(Spec) text").
class_variable(fold_previous, bool, @off,
               "Fold the output of the previous command when one is entered").

%!  binding(?Key, ?Method)
%
%   Epilog specific bindings. These are  used   to  create  the `epilog`
%   key_binding object.

binding('\\C-y',     paste_quoted).
binding('\\C-x',     prefix_or_cut).  % cut when there is a selection
binding('\\C-x8',    prefix).
binding('\\C-x8RET', insert_symbol).
binding('\\C-x8s',   insert_symbol).
binding('\\C-\\S-o', split_horizontally). % Terminator compatibility
binding('\\C-\\S-e', split_vertically).
binding('\\C-\\S-i', new_window).
binding('\\C-\\S-t', new_tab).
binding('\\C-\\S-k', clear_screen).       % Gnome terminal
binding('\\C-\\S-w', close).
binding('\\C-\\S-m', make).
binding('\\C-\\S-v', paste).
binding('\\C-\\S-c', copy).
binding('\\C-\\S-x', cut).           % only inside the input
binding('\\C--',     font_reduce).
binding('\\C-=',     font_default).
binding('\\C-\\S-h', toggle_fold).       % hide the output
binding('\\C-\\S-<cursor_up>',   previous_prompt).
binding('\\C-\\S-<cursor_down>', next_prompt).
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
    send(PT, popup, new(P, pane_popup)),
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
                menu_item(insert_symbol,
                          message(Terminal, insert_symbol)),
                menu_item(select_all,
                          message(Terminal, select_all),
                          end_group := @on),
                menu_item(consult_linked_file,
                          message(Terminal, consult_link),
                          end_group := @on),
                menu_item(copy_command,
                          message(Terminal, copy_block, command)),
                menu_item(copy_output,
                          message(Terminal, copy_block, output)),
                menu_item(fold_output,
                          message(Terminal, fold_block),
                          end_group := @on),
                menu_item(clear_screen,
                          message(Terminal, clear_screen),
                          end_group := @on),
                menu_item(split_horizontally,
                          message(Terminal, split_horizontally)),
                menu_item(split_vertically,
                          message(Terminal, split_vertically)),
                menu_item(new_tab,
                          message(Terminal, new_tab)),
                menu_item(new_window,
                          message(Terminal, new_window),
                          end_group := @on),
                menu_item(interrupt,
                          message(Terminal, interrupt),
                          end_group := @on),
                menu_item(close,
                          message(Terminal, close))
              ]),
    epilog_accelerators(P, KB),
    block_popup(PT, Terminal).

%!  block_popup(+Terminal, +Receiver) is det.
%
%   The menu of the triangle in the margin, which is about the one
%   command it stands beside rather than about the terminal.

block_popup(PT, Terminal) :-
    send(PT, block_popup, new(BP, pane_popup)),
    send(BP, update_message,
         message(PT, update_block_popup, @receiver, @event)),
    send_list(BP, append,
              [ menu_item(fold_output,
                          message(Terminal, fold_block)),
                menu_item(hide_all_outputs,
                          message(Terminal, fold_all_blocks, @on)),
                menu_item(show_all_outputs,
                          message(Terminal, fold_all_blocks, @off)),
                menu_item(remove_command,
                          message(Terminal, remove_block),
                          end_group := @on),
                menu_item(copy_command,
                          message(Terminal, copy_block, command)),
                menu_item(copy_output,
                          message(Terminal, copy_block, output)),
                menu_item(copy_both,
                          message(Terminal, copy_block, all)),
                menu_item(select_output,
                          message(Terminal, select_block, output),
                          end_group := @on),
                menu_item(repeat_command,
                          message(Terminal, repeat_block))
              ]).

unlink(PT) :->
    catch(unlink_terminal_thread(PT), error(_,_), true),
    uncapture_messages(PT),
    send_super(PT, unlink).

unlink_terminal_thread(PT) :- % Epilog attached to a running thread
    retract(attached_terminal(PT, RestoreContext)),
    retract(current_prolog_terminal(Thread, PT)),
    !,
    call_in_thread(Thread, restore_io(RestoreContext)).
unlink_terminal_thread(PT) :- % Normal Epilog window
    retract(current_prolog_terminal(Thread, PT)),
    !,
    send(PT, send, '\u0004'),
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
    ->  in_pce_thread(send(PT?frame, delete_pane, PT?window))
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

open_link(PT, Href:name) :->
    "Open a clicked hyperlink"::
    tty_link(PT, Href).

:- pce_group(prolog).

thread(PT, Thread:prolog) :<-
    "Prolog thread connected to this terminal"::
    current_prolog_terminal(Thread, PT).

connect(PT, TID:[name|int]) :->
    get(PT, connect, TID, _Title).

connect(PT, TID:[name|int], Title:[name]) :<-
    "Connect a Prolog thread to the terminal"::
    (   current_prolog_terminal(_Thread, PT)
    ->  Title = @default
    ;   connect(PT, TID, Title)
    ).

update_popup(PT, P:popup, Ev:event) :->
    "Update the popup"::
    update_block_items(PT, P, Ev),
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

:- pce_group(blocks).

prompt_mark(PT, Kind:{prompt,input,output,end}, Cont:[bool]) :->
    "Fold the command before this one when one is entered"::
    send_super(PT, prompt_mark, Kind, Cont),
    (   Kind == output,                 % the line was entered
        get(PT, fold_previous, @on),
        get(PT, blocks, Blocks),
        get(Blocks, tail, Current),
        get(Blocks, previous, Current, Previous)
    ->  ignore(send(Previous, fold))
    ;   true
    ).


%       The OSC 133 marks of the commandline editor divide the window
%       into blocks: a prompt, the line the user entered and the output
%       it produced.  The terminal keeps them as `terminal_block`
%       objects; what follows is the commands that act on the one the
%       mouse or the caret is on.

update_block_popup(PT, P:popup, Ev:event) :->
    "Update the menu of a block marker"::
    (   get(PT, fold_at, Ev, Block)
    ->  send(PT, slot, current_block, Block)
    ;   get(PT, current_block, Block),
        Block \== @nil
    ),
    (   get(Block, folded, @on)
    ->  send(P?members, for_all, message(@arg1, active, @on)),
        set_label(P, fold_output, 'Show output')
    ;   send(P?members, for_all, message(@arg1, active, @on)),
        set_label(P, fold_output, 'Hide output')
    ),
    get(P, member, repeat_command, Repeat),
    (   at_prompt(PT)
    ->  send(Repeat, active, @on)
    ;   send(Repeat, active, @off)   % typing would go to what is running
    ),
    get(P, member, remove_command, Remove),
    (   get(Block, end, _)           % it has finished writing
    ->  send(Remove, active, @on)
    ;   send(Remove, active, @off)
    ).

set_label(P, Name, Label) :-
    get(P, member, Name, Item),
    send(Item, label, Label).

%!  at_prompt(+Terminal) is semidet.
%
%   True while the client is asking for a line rather than running
%   something, which is when text sent to it is typed rather than fed to
%   what it is doing.

at_prompt(PT) :-
    get(PT, blocks, Chain),
    get(Chain, tail, Last),
    Last \== @nil,
    \+ get(Last, output, _).

select_block(PT, What:[{command,output,all}]) :->
    "Make the block the selection"::
    get(PT, current_block, Block),
    Block \== @nil,
    send(Block, select, What).

remove_block(PT) :->
    "Take the block out of the buffer"::
    get(PT, current_block, Block),
    Block \== @nil,
    send(Block, remove),
    send(PT, slot, current_block, @nil).

repeat_block(PT) :->
    "Type the command of the block again"::
    get(PT, current_block, Block),
    Block \== @nil,
    get(Block, content, command, String),
    get(String, value, Text0),
    Text0 \== '',
    %  <-content hands over the command without the return that entered
    %  it, so what arrives is typed and left for the user to enter.  The
    %  lines of one collected over several do need their returns, and a
    %  line the client reads ends the way a keyboard ends one.
    atomic_list_concat(Lines, '\n', Text0),
    atomic_list_concat(Lines, '\r', Text),
    send(PT, send, Text).

update_block_items(PT, P, Ev) :-
    (   get(PT, block_at, Ev, Block)
    ->  send(PT, slot, current_block, Block),
        (   get(Block, folded, @on)
        ->  Label = 'Show output'
        ;   Label = 'Hide output'
        ),
        Active = @on
    ;   send(PT, slot, current_block, @nil),
        Label = 'Hide output',
        Active = @off
    ),
    get(P, member, fold_output, Fold),
    send(Fold, label, Label),
    forall(member(Name, [copy_command, copy_output, fold_output]),
           ( get(P, member, Name, Item),
             send(Item, active, Active) )).

copy_block(PT, What:[{command,output,all}]) :->
    "Copy the command or its output to the clipboard"::
    get(PT, current_block, Block),
    Block \== @nil,
    send(Block, copy, What, clipboard).

fold_block(PT) :->
    "Hide the output of the block the popup was on, or show it"::
    get(PT, current_block, Block),
    Block \== @nil,
    send(Block, toggle_fold).

fold_all_blocks(PT, Fold:[bool]) :->
    "Hide the output of all blocks"::
    default(Fold, @on, Value),
    %  Not ->for_all over <-blocks: folding a command that printed
    %  nothing, or one still running, fails, and this would report the
    %  work it did as a failure.  ->fold_all passes over those.
    (   Value == @on
    ->  send(PT, fold_all)
    ;   send(PT, unfold_all)
    ).

toggle_fold(PT) :->
    "Hide the output of the command being edited, or show it again"::
    get(PT, blocks, Blocks),
    enum_backwards(Blocks, Block),
    send(Block, toggle_fold),
    !.

enum_backwards(Chain, Member) :-
    get(Chain, tail, Last),
    enum_backwards(Chain, Last, Member).

enum_backwards(_, Current, Current).
enum_backwards(Chain, Current, Member) :-
    get(Chain, previous, Current, Prev),
    enum_backwards(Chain, Prev, Member).

previous_prompt(PT) :->
    "Scroll to the prompt before the topmost one in view"::
    prompt_step(PT, -1).

next_prompt(PT) :->
    "Scroll to the prompt after the topmost one in view"::
    prompt_step(PT, 1).

%!  prompt_step(+Terminal, +Dir) is semidet.
%
%   Scroll to the block Dir away from the one the top of the window is
%   in, which is what makes repeating the command walk the history
%   rather than return to where it started.

prompt_step(PT, Dir) :-
    get(PT, blocks, Chain),
    chain_list(Chain, Blocks),
    Blocks \== [],
    (   get(PT, block_at, point(0, 0), Here),
        nth0(At, Blocks, Here)
    ->  true
    ;   At = 0
    ),
    To is At+Dir,
    nth0(To, Blocks, Block),
    send(Block, scroll_to).

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

undo(PT) :->
    "Undo the last change to the line being edited"::
    %  libedit binds undo to ^_ (and to ^Z while it is reading a line).
    %  Command-Z is the MacOS way to ask for it, so hand the program on
    %  the terminal the key it understands.
    send(PT, send, "\u001f").

interrupt(PT) :->
    "Interrupt the process running in this terminal"::
    (   send_super(PT, interrupt)       % external process: the tty signals it
    ->  true
    ;   current_prolog_terminal(Thread, PT),
        current_signal(int, SIGINT, debug),
        thread_signal(Thread, SIGINT)
    ).

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
    pane_frame_closed_tab(Epilog),
    send(Epilog, delete_pane, Window, @on).

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

insert_symbol(PT) :->
    "Open the Unicode symbol picker, targeting this terminal"::
    symbol_picker(PT).

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
    "Handle popup and drag-and-drop"::
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
    ;   drop_target_event(T, Ev,
                          'Drop Prolog source file(s) to consult',
                          epilog_consult_drop)
    ).

input_focus(PT, Focus:bool) :->
    "We gained/lost the focus"::
    send_super(PT, input_focus, Focus),
    (   Focus == @on
    ->  Opacity = 1
    ;   get(PT, class_variable_value, inactive_opacity, Opacity)
    ),
    send(PT, opacity, Opacity).

split(T, Dir:{horizontally,vertically}) :->
    "Split this terminal"::
    send(T?window, split, Dir).

split_horizontally(T) :->
    "Split terminal horizontally"::
    send(T, split, horizontally).

split_vertically(T) :->
    "Split terminal vertically"::
    send(T, split, vertically).

new_tab(T) :->
    "Open a new terminal in a tab of this window"::
    send(T?window, new_tab).

new_window(T) :->
    "Open a new window"::
    get(T, goal, Goal),
    get(T, background, BG),
    get(T, working_directory, WDir),
    get(T, profile, Profile),
    epilog([ profile(Profile),
             cwd(WDir),
             init(true),
             goal(Goal),
             background(BG)
           ]).

popup(T, Popup:popup*) :->
    "Associate a menu"::
    send(T, slot, popup, Popup),
    (   Popup == @nil
    ->  send(T, slot, popup_gesture, @nil)
    ;   send(T, slot, popup_gesture, popup_gesture(Popup))
    ).

block_popup(T, Popup:popup*) :->
    "Associate a menu with the fold marker of a block"::
    send(T, slot, block_popup, Popup),
    (   Popup == @nil
    ->  send(T, slot, block_gesture, @nil)
    ;   send(T, slot, block_gesture, popup_gesture(Popup))
    ).

show_popup(T, Ev:event) :->
    "Open the popup for what the event is on"::
    (   get(T, fold_at, Ev, Block),      % the marker of a block
        get(T, slot, block_gesture, G),
        G \== @nil
    ->  send(T, slot, current_block, Block)
    ;   get(T, slot, popup_gesture, G),
        G \== @nil
    ),
    send(G, event, Ev).

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
%
%   Connect to a Prolog thread. If TID  = @default, create a new thread,
%   else connect to the  already  existing   thread.  Title  carries the
%   thread alias or `'Thead <id>'`.

connect(PT, @default, Title) =>
    get(PT, goal_init, Init),
    get(PT, goal, Goal),
    get(PT, process_cwd, CWD),
    gensym(con, Alias),
    send(PT?window, name, Alias),
    get(PT, pty_name, PTY),             % /dev/pty* on Unix, @nil on Windows
    thread_self(Me),
    parent_history(PT, Events),
    parent_thread(PT, Parent),
    thread_create(thread_run_interactor(PT, Me, PTY, Init, Goal, CWD, Title,
                                        Events),
                  Thread,
                  [ inherit_from(Parent),
                    detached(true),
                    alias(Alias),
                    at_exit(terminated),
                    class(console)
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
connect(PT, TID, Title) =>
    thread_property(Thread, id(TID)),
    get(PT, pty_name, PTY),
    thread_title(Title),
    thread_send_message(Thread, '$epilog'(PT, PTY)).

%!  set_inject(+PT, +Spec) is det.
%
%   Realise the inject(Spec) option of epilog/1.  As we cannot tell what
%   will run the terminal, the caller does:   a string is a line of text,
%   anything else a Prolog goal, typed by ->inject.

set_inject(PT, Spec) :-
    (   is_list(Spec)
    ->  Items = Spec
    ;   Items = [Spec]
    ),
    maplist(must_be_inject_item, Items),
    send(PT, inject_items, Items).

must_be_inject_item(Item) :-
    (   string(Item)
    ->  true
    ;   must_be(callable, Item)
    ).

inject_pending(PT) :->
    "Type the inject(Spec) items once a client reads them"::
    %  Text that arrives while a client is still setting the terminal up
    %  is displayed twice: the client reads and echoes it during its
    %  setup, and its line editor draws it again at the prompt.  We
    %  therefore wait for inject_ready/1, and type anyway when it does
    %  not come.  First called from thread_run_interactor/8, i.e., after
    %  the client ran its init goal, and by the timer below after that.
    (   get(PT, inject_items, Items),
        is_list(Items)                          % `` when there is nothing
    ->  (   inject_ready(PT)
        ->  inject_now(PT, Items)
        ;   get(PT, hypered, inject_timer, Timer)
        ->  (   get(Timer, times, 0)            % we just used our last
            ->  inject_now(PT, Items)           % try; type it blind
            ;   true                            % the timer calls us again
            )
        ;   wait_for_reader(PT)
        )
    ;   true
    ).

inject_now(PT, Items) :-
    send(PT, inject_items, ''),                 % once
    (   get(PT, hypered, inject_timer, Timer)
    ->  send(Timer, stop)
    ;   true
    ),
    maplist(inject_item(PT), Items).

%!  wait_for_reader(+PT) is det.
%
%   Have the terminal ask itself again every  50ms, `inject_tries` times.
%   The timer is a part of PT, so that   closing the terminal takes it
%   with it, and it reaches PT back over that same hyper.
%
%   Windows waits fewer times: it cannot  tell   a  reader from a client
%   still setting up (see inject_ready/1),   so the tries are a delay we
%   always pay rather than a bound we rarely reach.

wait_for_reader(PT) :-
    get(PT, class_variable_value, inject_tries, Tries),
    new(Timer, timer(0.05, message(@receiver, send_hyper,
                                   terminal, inject_pending))),
    new(_, partof_hyper(PT, Timer, inject_timer, terminal)),
    send(Timer, start, repeat, Tries).

%!  inject_ready(+PT) is semidet.
%
%   True when a client stopped the tty  from   echoing, which says a line
%   editor took the terminal over and  displays   what  it reads.  We do
%   not use <-foreground_process: a  shell   owns  the  tty from the ~exec
%   on, well before its editor is up.
%
%   Fails while nothing edits, e.g., a  plain   `sh' or a Prolog toplevel
%   without library(editline), and on a platform  that cannot tell.  The
%   tty then echoes our text itself and  nothing repeats it, so the wait
%   only costs us the timeout.

inject_ready(PT) :-
    get(PT, tty_echo, @off).

inject_item(PT, Item) :-
    string(Item),
    !,
    atomics_to_string([Item, "\r"], Line),
    send(PT, send, Line).
inject_item(PT, Goal) :-
    send(PT, inject, Goal).

%!  thread_run_interactor(+PrologTerminal, +CreatorThread, +PTY, +Init,
%!                        +Goal, +CWD, +Title, +History) is det.
%
%   Run the Prolog terminal main thread. Note that this code cannot talk
%   to xpce as  it  will  deadlock.  That   is  why  all  relevant  xpce
%   interaction is done in connect/2 above.
%
%   Q: Will this still deadlock after changes to the XPCE "GIL"?

thread_run_interactor(PT, Creator, PTY, Init, Goal, CWD, Title, History) :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    set_prolog_flag(hyperlink_term, true),
    set_prolog_flag(color_term, true),
    set_prolog_flag(console_menu, true),
    Error = error(Formal,_),
    (   catch(attach_terminal(PT, PTY, History), Error, true)
    ->  (   var(Formal)
        ->  thread_title(Title),
            thread_send_message(Creator, title(Title)),
            set_process_working_directory(CWD),
            call(Init),
            in_pce_thread(send(PT, inject_pending)),
            ignore(epilog_run(PT, Goal))
        ;   thread_send_message(Creator, throw(Error))
        )
    ;   thread_send_message(Creator, false)
    ).

attach_terminal(PT, PTY, History) :-
    exists_source(library(editline)),
    use_module(library(editline)),
    !,
    pce_open_terminal_image(PT, In, Out, Err),
    set_stream(In,  eof_action(reset)),
    set_std_streams(In, Out, Err),
    set_prolog_flag(tty_control, true),
    call(el_wrap([pipes(true)])),        % Option only for Windows
    register_input(PT, PTY, true, History).
attach_terminal(PT, PTY, History) :-
    pce_open_terminal_image(PT, In, Out, Err),
    set_prolog_IO(In, Out, Err),
    register_input(PT, PTY, false, History).

thread_title(Title) :-
    thread_self(Me),
    (   atom(Me)
    ->  Title = Me
    ;   thread_property(Me, id(Id)),
        format(atom(Title), 'Thread ~w', [Id])
    ).

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


%!  editline:el_wcwidth(+Code, -Columns) is semidet.
%
%   Hook for library(editline).  Report the number of terminal columns
%   occupied by Code when rendered by this thread's Epilog terminal.
%
%   The default libedit column tracker uses PL_wcwidth(), which reads
%   the static Unicode tables.  Those disagree with us for the symbol
%   and emoji code points an emoji-presenting font draws twice as wide.
%   We therefore ask the terminal itself using <-cwidth, keeping the
%   renderer as the single point of truth.  Failing here (no terminal,
%   or no cell metrics yet) makes libedit fall back to PL_wcwidth().

:- multifile
    editline:el_wcwidth/2.

editline:el_wcwidth(Code, Columns) :-
    thread_self(Thread),
    current_prolog_terminal(Thread, PT),
    object(PT),                         % terminal may already be gone
    get(PT, cwidth, Code, Columns).


%!  tty_link_hook(+Terminal, +Link) is semidet.
%
%   Multifile hook to open an  OSC8   embedded  hyperlink.  The link was
%   clicked in Terminal, an  instance   of  the class `prolog_terminal`.
%   Link is an atom representing the clicked link.

%!  tty_link(+Terminal, +Link) is det.
%
%   Handle clicking a terminal hyperlink click.

tty_link(PT, Link) :-
    tty_link_hook(PT, Link),
    !.
tty_link(_PT, Link) :-
    link_file_location(Link, _File, Location),
    !,
    tty_open(Location).
tty_link(_PT, URL) :-
    call(www_open_url(URL)).

%!  tty_open(+Location) is det.
%
%   Open a file link.  A location that   carries a line number is opened
%   using edit/1.  A plain file is  opened   using  the editor unless we
%   know it is not a text file,  e.g., an image or PDF document.

tty_open(file(File)) :-
    !,
    (   (   exists_directory(File)
        ;   binary_file(File)
        )
    ->  desktop_open(File)
    ;   call(edit(file(File)))
    ).
tty_open(Location) :-
    call(edit(Location)).

%!  binary_file(+File) is semidet.
%
%   True when we know File cannot be edited as text.  Note that
%   file_mime_type/2 returns `application/unknown` if it does not know
%   the extension, so we must ask for known binary types rather than
%   for known text types.

:- if(exists_source(library(http/mimetype))).
:- autoload(library(http/mimetype), [file_mime_type/2]).

binary_file(File) :-
    file_mime_type(File, Type),
    binary_type(Type).
:- else.
binary_file(_) :-               % library(http) is not installed and we
    fail.                       % cannot tell.  Assume text.
:- endif.

binary_type(image/_).
binary_type(audio/_).
binary_type(video/_).
binary_type(application/pdf).
binary_type(application/msword).
binary_type(application/zip).
binary_type(application/wasm).
binary_type(application/'x-gzip').
binary_type(application/'x-gtar').
binary_type(application/'x-java-archive').
binary_type(application/'octet-stream').


link_file_location(Link, File, Location) :-
    uri_file_name(Link, File),
    !,
    uri_components(Link, Components),
    uri_data(fragment, Components, Fragment),
    fragment_location(Fragment, File, Location).

%!  fragment_location(?Fragment, +File, -Location) is semidet.
%
%   Translate the fragment of a `file://` link  into a location for
%   edit/1.  The fragment holds the line  and optionally the column,
%   both counting from 1 as in edit/1.  The line may be prefixed with
%   ``L``, the form used by GitHub and e.g., ``rg
%   --hyperlink-format=file://{path}#L{line}:{column}``.

fragment_location(Fragment, File, file(File)) :-
    var(Fragment),
    !.
fragment_location(Fragment, File, File:Line:Column) :-
    split_string(Fragment, ":", "L", [LineS,ColumnS]),
    number_string(Line, LineS),
    number_string(Column, ColumnS),
    !.
fragment_location(Fragment, File, File:Line) :-
    split_string(Fragment, "", "L", [LineS]),
    number_string(Line, LineS).

:- pce_group(drop).

%!  epilog_consult_drop(+Terminal, +Paths) is det.
%
%   Drop-target callback: consult dropped Prolog files; briefly flag
%   any non-Prolog files in red.

epilog_consult_drop(Terminal, Paths) :-
    split_dropped_files(Paths, PrologOS, OtherOS),
    (   PrologOS \== []
    ->  prolog_path_list(PrologOS, PrologFiles),
        send(Terminal, inject, consult(PrologFiles))
    ;   OtherOS \== []
    ->  rejection_text(OtherOS, Msg),
        drop_target_show_rejected(Terminal, Msg, 1.5)
    ;   true
    ).

split_dropped_files([], [], []).
split_dropped_files([P|T], [P|PR], O) :-
    file_name_extension(_, Ext, P),
    user:prolog_file_type(Ext, prolog),
    !,
    split_dropped_files(T, PR, O).
split_dropped_files([P|T], PR, [P|O]) :-
    split_dropped_files(T, PR, O).

prolog_path_list([], []).
prolog_path_list([OS|TOS], [Pl|TPl]) :-
    prolog_to_os_filename(Pl, OS),
    prolog_path_list(TOS, TPl).

rejection_text([_], 'Ignored: not a Prolog source file') :- !.
rejection_text(Files, Msg) :-
    length(Files, N),
    format(string(Msg), 'Ignored ~d files: not Prolog source', [N]).

:- pce_group(process).

working_directory(PT, CWD:name) :<-
    "Get the directory for running commands"::
    (   get_super(PT, working_directory, CWD),  % OSC 7 or 9
        CWD \== @nil
    ->  true
    ;   get(PT, foreground_directory, CWD),     % POSIX PTY/Process inspection
        CWD \== @nil
    ->  true
    ;   working_directory(CWD, CWD)             % Prolog default
    ).

consult(PT) :->
    "Ask for a file and consult it"::
    source_file_filter(Filter),
    working_directory(CWD, CWD),
    get(PT?frame, open_file,
        filters := Filter,
        default := CWD,
        allow_many := @on, FileChain),
    chain_list(FileChain, Files),
    send(PT, inject, consult(Files)).

edit_file(PT) :->
    "Ask for a file and edit it"::
    source_file_filter(Filter),
    (   current_prolog_flag(associated_file, Default)
    ->  true
    ;   working_directory(Default, Default)
    ),
    get(PT?frame, open_file,
        filters := Filter,
        default := Default,
        File),
    edit(file(File)).

new_file(PT) :->
    "Ask for a file and create it"::
    source_file_filter(Filter),
    working_directory(CWD, CWD),
    get(PT?frame, save_file,
        filters := Filter,
        default := CWD,
        File0),
    ensure_prolog_extension(File0, File),
    edit(file(File)).

toggle_fold_previous(PT) :->
    "Toggle folding the previous command"::
    get(PT, fold_previous, Old),
    (   Old == @on
    ->  New = @off
    ;   New = @on
    ),
    send(PT, fold_previous, New).

:- pce_group(menu).

%       The menu bar asks the terminal it acts on what its items should
%       look like, through `menu_item <-condition'.  Each reads a flag of
%       the Prolog thread this terminal runs, which may be busy, so
%       terminal_prolog_flag/4 gives up after a moment.

update_debug_mode(PT, MI:menu_item) :->
    "Tick the item while the thread is in debug mode"::
    (   terminal_prolog_flag(PT, query_debug_settings,
                             debug(Debugging, _Tracing), -)
    ->  send(MI, selected, Debugging)
    ;   true
    ).

update_trace_mode(PT, MI:menu_item) :->
    "Tick the item while the thread is tracing"::
    (   terminal_prolog_flag(PT, query_debug_settings,
                             debug(_Debugging, Tracing), -)
    ->  send(MI, selected, Tracing)
    ;   true
    ).

update_gui_debug(PT, MI:menu_item) :->
    "Tick the item while the thread uses the GUI tracer"::
    (   terminal_prolog_flag(PT, gui_tracer, GuiTracer, false)
    ->  send(MI, selected, GuiTracer)
    ;   true
    ).

update_fold_previous(PT, MI:menu_item) :->
    "Tick the item while the previous command is folded"::
    (   get(PT, fold_previous, Bool)
    ->  send(MI, selected, Bool)
    ;   true
    ).

:- pce_end_class(prolog_terminal).

%!  source_file_filter(-Filter) is det.
%
%   Chain the file finder takes, offering the Prolog source extensions.

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

%!  terminal_prolog_flag(+Term, +Flag, -Value, +Default) is semidet.
%
%   Get the Prolog flag Flag for the toplevel thread running in Term. If
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


                /*******************************
                *           TERMINAL           *
                *******************************/

:- pce_begin_class(epilog_window, window, "Implement an embedded terminal").
:- use_class_template(pane).

variable(terminal, prolog_terminal, get, "The terminal_image").
variable(tab_label,    name*, get, "Name my tab was given").
variable(window_label, name*, get, "Title a client asked for").
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
    send(T, display, new(Bar, epilog_report)),  % after TI: it draws on top
    send(Bar, displayed, @off),                 % ->display turned it on
    send(Bar, client, TI),
    send(T, display_fixed, new(split_handle)),  % puts itself in the corner
    send(T, keyboard_focus, TI).

resize(T) :->
    "Place terminal and scrollbar"::
    get(T, size, size(TW, TH)),
    get(T, member, scroll_bar, SB),
    get(SB, width, SBW),
    send(SB, set, TW-SBW, 0, @default, TH),
    get(T, member, terminal, TI),
    send(TI, set, 0, 0, TW-SBW, TH),
    get(T, member, epilog_report, Bar),
    send(Bar, place, 0, 0, TW-SBW, TH).

create(T, Parent:[window]) :->
    "Create the terminal and attach a Prolog thread to it"::
    send_super(T, create, Parent),      % a subwindow is created with the
    get(T, member, terminal, TI),       % window it is displayed on
    get(T, tid, TID),
    get(TI, connect, TID, Title),
    (   Title == @default               % it was connected already
    ->  true
    ;   ignore(send(T, thread_connected, Title))
    ).

%       A tab is named when it is added, which is before its terminal has
%       a thread to be named after, so the name it starts with says what
%       it runs.  Once the thread is there, a Prolog toplevel goes by its
%       alias -- con1, con2 -- and the user renames the thread to rename
%       the tab.  Anything else keeps the name of what it runs: a shell
%       has a thread too, and `con3' would say nothing about it.

thread_connected(T, Thread:name) :->
    "Name my tab after the thread that has just been connected"::
    get(T, terminal, PT),
    get(PT, profile, prolog),
    send(T, retitle_tab, Thread).

pane_exposed(T) :->
    "Take the name of what I run, which I may only know now"::
    get(T, terminal, PT),
    terminal_base_label(PT, Base),
    ignore(send(T, retitle_tab, Base)).

retitle_tab(T, Base:name) :->
    "Put Base on my tab, made unique, unless the user named it"::
    get(T, pane_frame, F),
    get(T, pane_tab, Tab),
    get(Tab, renamed, @off),            % the user named it themselves
    get(Tab, label, Now),
    \+ says_the_same(Now, Base),        % renaming would only bump the
    unique_tab_label(F, Base, 1, Label), % number after it
    send(T, tab_label, Label),
    send(Tab, label, Label).

sibling(T, W:epilog_window) :<-
    "A new terminal window that continues mine"::
    new(W, epilog_window),
    new(_, hyper(W, T, parent, child)),
    send(W, history, copy),
    get(W, terminal, PT),
    get(T, working_directory, WDir),
    send(PT, process_cwd, WDir),
    send(PT, goal_init, true),
    get(T, goal, Goal),
    send(PT, goal, Goal),
    send(PT, profile, T?profile),
    send(PT, background, T?terminal?background).

new_tab(T) :->
    "Add a new terminal in a tab of its own"::
    get(T, sibling, W),
    get(T, frame, Frame),
    send(Frame, append_terminal, W, @on),
    send(Frame, keyboard_focus, W).

pane_label(T, Label:name) :<-
    "What my tab is called"::
    (   get(T, slot, window_label, L),          % a client asked for a title
        L \== @nil
    ->  Label = L
    ;   get(T, slot, tab_label, L),             % the name my tab was given
        L \== @nil
    ->  Label = L
    ;   get(T, terminal, PT),                   % failing both, what I run
        terminal_base_label(PT, Label)
    ).

tab_label(T, Label:name) :->
    "Remember the name my tab was given, so that I can put it back"::
    send(T, slot, tab_label, Label).

menu_bar_key(_T, Key:name) :<-
    "Every terminal asks for the same menu bar"::
    Key = epilog.

unlink(T) :->
    "Save the command line history of my terminal"::
    ignore(send(T, save_history)),
    send_super(T, unlink).

%       What a terminal puts on the menu bar of whatever window it is in.
%       The frame has already put the menus of its application there, so
%       the File items go in front of that application's own, and the
%       Debug menu is made if it is not there.
%
%       `Term' is read when an item is chosen or a menu opens, so it is
%       whichever terminal is current then rather than the one that built
%       the bar.

fill_menu_bar(_T, MD:tool_dialog) :->
    "Put the menus of a terminal on the bar"::
    Term = @event?receiver?frame?current_terminal,
    %  Each goes immediately before the item the application put first,
    %  so they keep the order they are written in here.
    forall(member(Item,
                  [ menu_item(consult,
                              message(Term, consult)),
                    menu_item(edit,
                              message(Term, edit_file)),
                    menu_item(new_prolog_file,
                              message(Term, new_file),
                              end_group := @on),
                    menu_item(reload_modified_files,
                              message(Term, make),
                              accelerator := 'Shift-Ctrl-M',
                              end_group := @on)
                  ]),
           send(MD, append, Item, file, editor_in_a_new_tab)),
    send(MD, append, new(NewTab, menu_item(new_tab_with_profile)),
         file, editor_in_a_new_tab),
    send(MD, append, new(NewWindow, menu_item(new_window_with_profile)),
         file, editor_in_a_new_tab),
    send(NewTab, popup,
         new(NewTabPopup, popup(new_tab,
                                message(@prolog, epilog_tab_with_profile,
                                        @event?receiver?frame, @arg1)))),
    send(NewTabPopup, update_message,
         message(@prolog, epilog_profile_menu, @receiver)),
    send(NewWindow, popup,
         new(NewWindowPopup, popup(new_window,
                                   message(@prolog, epilog_with_profile,
                                           @arg1)))),
    send(NewWindowPopup, update_message,
         message(@prolog, epilog_profile_menu, @receiver)),
    get(MD, popup, settings, @on, Settings),
    send(Settings, append,
         new(FoldPrevious,
             menu_item(fold_previous_command,
                       message(Term, toggle_fold_previous)))),
    send(FoldPrevious, condition,
         message(Term, update_fold_previous, FoldPrevious)),
    debug_popup(MD, Debug),
    send_list(Debug, append,
              [ new(TraceMode,
                    menu_item(trace_mode,
                              message(Term, trace_mode),
                              accelerator := 'F5')),
                new(DebugMode,
                    menu_item(debug_mode,
                              message(Term, debug_mode),
                              accelerator := 'Shift-F5')),
                new(GuiDebug,
                    menu_item('GUI_debugger',
                              message(Term, gui_debug),
                              accelerator := 'Ctrl-F5',
                              end_group := @on)),
                menu_item(show_debug_status,
                          message(Term, debugging),
                          accelerator := 'F6')
              ]),
    send(Debug, show_current, @on),
    send(Debug, multiple_selection, @on),
    send(DebugMode, condition, message(Term, update_debug_mode, DebugMode)),
    send(TraceMode, condition, message(Term, update_trace_mode, TraceMode)),
    send(GuiDebug,  condition, message(Term, update_gui_debug, GuiDebug)).

window_label(T, Label:char_array) :->
    "Show the title a client asked for on my tab"::
    (   send(Label, equal, '')          % no title: my tab gets its own
    ->  send(T, slot, window_label, @nil)
    ;   send(T, slot, window_label, Label)
    ),
    (   get(T, container, tab_frame, Tab)
    ->  send(Tab, window_label, Label)
    ;   send_super(T, window_label, Label)
    ).

save_history(EW) :->
    "Save the commandline history"::
    get(EW, terminal, PT),
    save_history(PT).

report(T, Type:name, Fmt:[char_array], Args:any ...) :->
    "Show short messages on the bar over the terminal"::
    get(T, member, epilog_report, Bar),
    (   report_on_bar(Type),
        \+ get(Bar, placement, none)
    ->  (   (Fmt == @default ; Fmt == '')
        ->  send(Bar, hide)
        ;   Format =.. [format, Fmt|Args],
            new(S, string),
            send(S, Format),
            %  A message from something that holds the keyboard -- an
            %  incremental search -- is its prompt, not a remark: it
            %  stays until that something says it is done.
            get(T, member, terminal, TI),
            (   get(TI, focus_function, @nil)
            ->  Transient = @on
            ;   Transient = @off
            ),
            send(Bar, show, Type, S, Transient),
            %  The boxes say what counts as a match, which is what a
            %  search looks for and, when one is up, what the other
            %  occurrences of the selection are.  ->show takes them
            %  away again, so they go back after it.
            (   search_options(TI)
            ->  send(Bar, search_options,
                     TI?exact_case, TI?match_word)
            ;   true
            )
        )
    ;   Report =.. [report, Type, Fmt|Args],
        send_super(T, Report)
    ).

%!  report_on_bar(+Type) is semidet.
%
%   Message kinds the bar takes.  The ones that ask something of the
%   user, or that must not be missed, keep the dialog they had.

report_on_bar(status).
report_on_bar(warning).

%!  search_options(+Terminal) is semidet.
%
%   True when the terminal is matching something, so that the two boxes
%   saying what counts as a match are worth showing: an incremental
%   search is running, or a selection whose other occurrences are
%   highlighted.

search_options(TI) :-
    (   \+ get(TI, focus_function, @nil)
    ;   \+ get(TI, selection_string, @nil)
    ),
    !.

:- pce_end_class(epilog_window).

%!  epilog_profile_menu(+Popup) is det.
%!  epilog_tab_with_profile(+Frame, +Profile) is det.
%!  epilog_with_profile(+Profile) is det.
%
%   The profile pullrights of the File menu.  They belong to the terminal
%   rather than to the IDE: nothing but a terminal has a profile.

:- public
    epilog_profile_menu/1,
    epilog_tab_with_profile/2,
    epilog_with_profile/1.

epilog_profile_menu(Popup) :-
    send(Popup, clear),
    forall(current_profile(Name, Label),
           send(Popup, append, menu_item(Name, label := Label))).

epilog_tab_with_profile(Frame, Profile) :-
    epilog_tab(Frame, Profile).

epilog_with_profile(Profile) :-
    epilog(Profile).

%!  debug_popup(+MenuDialog, -Popup) is det.
%
%   The Debug menu, made if it is not there yet.  In a window of Epilog's
%   own it goes where it has always been, before the GUI menu; in a window
%   belonging to something else it goes at the end.

debug_popup(MD, Debug) :-
    get(MD, menu_bar, @on, MB),
    (   get(MB, member, debug, Debug)
    ->  true
    ;   new(Debug, pane_popup(debug)),
        (   get(MB, member, 'GUI', _)
        ->  send(MB, append, Debug, @default, 'GUI')
        ;   send(MB, append, Debug)
        )
    ).


%!  set_process_working_directory(+Dir) is det.
%
%   Initialisation goal for a  terminal  that   must  run  its external
%   processes in Dir.  We cannot use  working_directory/2 as the Prolog
%   working directory is  shared  by  all   threads.   Instead  we  let
%   run_shell/0 pass Dir to process_create/3.  Nothing needs to be done
%   if Dir is already the current directory.

:- public set_process_working_directory/1.

set_process_working_directory(Dir) :-
    (   atom(Dir),
        exists_directory(Dir),
        \+ same_file(Dir, '.')
    ->  nb_setval(epilog_process_working_directory, Dir)
    ;   true
    ).

%!  epilog_run(+Terminal, :Goal)
%
%   Run Goal under Epilog.  Redefines shell/0

epilog_run(PT, _:shell) :-
    !,
    run_shell,
    send(PT, window_label, 'Shell').
epilog_run(_, Goal) :-
    call(Goal).

%!  run_shell is det.
%
%   Run an interactive shell  in  this   terminal.  Unlike  shell/0, we
%   use process_create/3 to run the shell   in  the directory the user
%   was in when this terminal was created.
%
%   We wait using process_wait/2 rather   than leaving the waiting to
%   process_create/3, as the latter turns a  non-zero exit status into
%   an exception.  As for shell/0, the exit status is not our business.

run_shell :-
    findall(Option, shell_process_option(Option), Options),
    shell_command(Shell),
    shell_prog_argv(Shell, Prog, Argv),
    process_create(Prog, Argv, [process(PID)|Options]),
    process_wait(PID, _Status).

%!  shell_process_option(-Option) is nondet.
%
%   Options for the process we run in this terminal.

shell_process_option(cwd(Dir)) :-
    nb_current(epilog_process_working_directory, Dir).
shell_process_option(environment(Env)) :-
    terminal_environment(Env).

%!  terminal_environment(-Env:list) is semidet.
%
%   Environment variables that describe _this_ terminal.  A process we
%   start must be told about the terminal it talks to rather than
%   inherit the description of the terminal Epilog itself was started
%   from, which is what it finds in the environment if Epilog was
%   started from one.  See fix_term/0, which does the same for the
%   processes started from the Prolog top level of an Epilog window.
%
%   Fails on Windows, where process_create/3 handles environment/1 as
%   env/1 and the process would lose the rest of its environment.

terminal_environment(Env) :-
    \+ current_prolog_flag(windows, true),
    current_prolog_flag(version_data, swi(Major,Minor,Patch,_)),
    atomic_list_concat([Major,Minor,Patch], '.', Version),
    Env = [ 'TERM'            = 'xterm-256color',
            'TERM_PROGRAM'    = 'Epilog',
            'TERM_PROGRAM_VERSION' = Version
          ].

%!  shell_prog_argv(+Shell, -Prog, -Argv) is det.
%
%   Split the shell command into a  program and its arguments.  Shell is
%   either the executable or  a  term   Exe(Arg...),  e.g.,  bash('-l').

shell_prog_argv(Shell, Prog, Argv) :-
    must_be(callable, Shell),
    (   atom(Shell)
    ->  Name = Shell,
        Argv = []
    ;   compound_name_arguments(Shell, Name, Argv)
    ),
    process_prog(Name, Prog).

%!  process_prog(+Shell, -Prog) is det.
%
%   Turn the shell command into a  specification for process_create/3,
%   searching $PATH if Shell has no directory component.

process_prog(Name, Prog) :-
    (   file_base_name(Name, Name)
    ->  Prog = path(Name)
    ;   Prog = Name
    ).

                /*******************************
                *          REPORT BAR          *
                *******************************/

:- pce_begin_class(epilog_report, device,
                   "Transient bar for messages over the terminal").

/*  A place for `->report' messages that costs the terminal nothing
    when there is nothing to say.

    It has to be an overlay rather than a row of its own: the terminal
    derives its size in characters from its size in pixels, so a bar
    that took a row would resize the terminal, rewrap the whole
    scroll-back, drop the scrolling region and send the process on it a
    SIGWINCH -- on every keystroke of an incremental search.

    Covering a line of the terminal is a nuisance if that is the line
    being edited, so the `placement' class variable says which line we
    cover.  By default we go over the last one, unless that is where
    the user is looking -- see at_top/2 -- in which case we go over the
    first.  With

        epilog_report.placement: bottom

    in ~/.xpce/Defaults we always take the last line, `top' always
    takes the first and with `none' the messages go to the normal XPCE
    reporting instead.
*/

variable(timer,  timer*,          get, "Hides us again").
variable(client, terminal_image*, both, "Terminal whose search we show").
variable(placement, {top,bottom,smart,none}, get,
         "Where I appear over the terminal").
variable(covers,    area*, get, "Terminal area I am placed in").

class_variable(background,  colour, '#0008',  "Colour behind the message").
class_variable(colour,      colour, white,    "Colour of the message").
class_variable(hide_after,  int,    5,        "Seconds a message stays up").
class_variable(placement,   {top,bottom,smart,none}, smart,
               "Show messages at the top, at the bottom, out of the \c
                way (`smart') or not at all").

initialise(R) :->
    send_super(R, initialise),
    get(R, class_variable_value, background, Background),
    get(R, class_variable_value, colour, Colour),
    send(R, display, new(B, box(100, 20))),
    send(B, pen, 0),
    send(B, fill, Background),
    send(R, display, new(Text, text('', left)), point(6, 3)),
    send(Text, colour, Colour),
    send(R, display, new(Menu, menu(search_options, marked,
                                    message(R, option, @arg1, @arg2)))),
    send(Menu, multiple_selection, @on),
    send(Menu, label, 'Match:'),
    send(Menu, layout, horizontal),
    send(Menu, gap, size(10, 0)),
    send(Menu, colour, Colour),
    send_list(Menu, append,
              [ menu_item(exact_case,  @default, 'Case'),
                menu_item(search_word, @default, 'Word')
              ]),
    send(Menu, displayed, @off),
    %  The box must hold the text: my <-height is the union of the two,
    %  and ->layout uses it to put me on an edge of the terminal.
    get(Text, height, TextHeight),
    send(B, height, TextHeight+6).

option(R, Which:name, Value:bool) :->
    "A box was clicked; the box is named after what it sets"::
    get(R, client, Client),
    (   Client == @nil
    ->  true
    ;   send(Client, Which, Value)
    ).

unlink(R) :->
    send(R, stop_timer),
    send_super(R, unlink).

place(R, X:int, Y:int, Width:int, Height:int) :->
    "Cover the given area of the terminal"::
    get(R, covers, Old),
    (   Old == @nil
    ->  send(R, slot, covers, area(X, Y, Width, Height))
    ;   send(Old, set, X, Y, Width, Height)
    ),
    send(R, layout).

placement(R, Placement:{top,bottom,smart,none}) :->
    "Show me at the top, at the bottom or not at all"::
    send(R, slot, placement, Placement),
    (   Placement == none
    ->  send(R, hide)
    ;   send(R, layout)
    ).

layout(R) :->
    "Put me on the edge of the area I cover named by <-placement"::
    get(R, covers, Area),
    (   Area == @nil
    ->  true
    ;   object(Area, area(X, Y, Width, Height)),
        get(R, member, box, Box),
        send(Box, width, Width),
        get(R, member, search_options, Menu),
        send(Menu, compute),
        get(Menu, width, MW),
        get(Box, height, BH),
        get(Menu, height, MH),
        send(Menu, set, Width-MW-6, (BH-MH)/2),
        get(R, height, H),
        (   at_top(R, H)
        ->  send(R, set, X, Y)
        ;   send(R, set, X, Y+Height-H)
        )
    ).

%!  at_top(+Report, +BarHeight) is semidet.
%
%   True when the bar goes over the  first line of the terminal rather
%   than over the last.  With `smart' that is where it goes as soon as
%   the lines it would cover hold what the user is looking at: the
%   caret, or an end of the selection, which is also the hit of a
%   running incremental search.

at_top(R, _BarHeight) :-
    get(R, placement, top),
    !.
at_top(R, BarHeight) :-
    get(R, placement, smart),
    get(R, client, TI),
    TI \== @nil,
    get(TI, rows, Rows),
    get(TI, font, Font),
    get(Font, height, FontHeight),      % may be fractional
    Covered is max(1, ceiling(BarHeight/FontHeight)),
    First is Rows-Covered,
    watched_row(TI, Row),
    Row >= First,
    Row < Rows,
    !.

%!  watched_row(+Terminal, -Row) is nondet.
%
%   Rows of the terminal window the user has an eye on.  <-rows counts
%   from the top of the window, so a  row outside 0..<-rows-1 is one
%   scrolled out of view.

watched_row(TI, Row) :-
    get(TI, cursor_position, point(_, Row)).
watched_row(TI, Row) :-
    (   get(TI, selection_start, point(_, Row))
    ;   get(TI, selection_end, point(_, Row))
    ).

search_options(R, Case:bool, Word:bool) :->
    "Show the boxes and what they stand at"::
    get(R, member, search_options, Menu),
    send(Menu, selected, exact_case, Case),
    send(Menu, selected, search_word, Word),
    send(Menu, displayed, @on),
    send(R, layout).

show(R, _Type:name, Message:string, Transient:[bool]) :->
    "Show Message, and take it away again unless it is a prompt"::
    get(R, member, text, Text),
    send(Text, string, Message),
    get(R, member, search_options, Menu),
    send(Menu, displayed, @off),        % a search turns them back on
    send(R, layout),                    % `smart' looks where we are now
    send(R, displayed, @on),
    send(R, expose),
    send(R, stop_timer),
    (   Transient == @off
    ->  true                            % it stays until its mode is done
    ;   get(R, class_variable_value, hide_after, Seconds),
        send(R, slot, timer, new(Timer, timer(Seconds, message(R, hide)))),
        send(Timer, start, once)
    ).

hide(R) :->
    "Take the message away"::
    send(R, stop_timer),
    get(R, member, search_options, Menu),
    send(Menu, displayed, @off),
    send(R, displayed, @off).

stop_timer(R) :->
    "Forget the timer that would hide us"::
    get(R, timer, Timer),
    (   Timer == @nil
    ->  true
    ;   send(Timer, stop),
        send(R, slot, timer, @nil),
        free(Timer)
    ).

:- pce_end_class(epilog_report).



                 /*******************************
                 *        TERMINALS IN IT       *
                 *******************************/

%       A window of terminals.  ->append_terminal names the tab after the
%       thread or the profile the terminal runs; a tab is found back by
%       its label (see `tabbed_window ->on_top'), so no two of them may
%       carry the same one.

:- pce_extend_class(pane_frame).

append_terminal(F, W:epilog_window, Expose:[bool]) :->
    "Add a terminal in a tab of its own, named after its profile"::
    get(W, terminal, PT),
    terminal_base_label(PT, Base),
    unique_tab_label(F, Base, 1, Label),
    send(W, tab_label, Label),
    send(F, append_pane, W, Label, Expose).

tab_label(F, Label:name) :->
    "Rename the tab in view"::
    get(F, tab, Tab),
    send(Tab, label, Label).

current_terminal(F, Terminal:prolog_terminal) :<-
    "The terminal the user is working in"::
    get(F, current_pane, Window),
    get(Window, terminal, Terminal).

%       A terminal is told it has the keyboard whenever the window it is
%       in is activated -- including while the frame is telling everyone
%       that another tab has come to the front, at which point the window
%       being activated is the one on its way out.  Bringing its tab back
%       is the last thing wanted, so all I do here is pick the terminal
%       out of a tab that holds more than one pane.

current_terminal(F, Terminal:prolog_terminal) :->
    "Make Terminal the one the user is working in"::
    get(Terminal, window, Window),
    (   get(F, current_pane, Window)
    ->  true                            % it already is
    ;   in_view(Window)
    ->  send(F, current_pane, Window)
    ;   true
    ).

%!  in_view(+Window) is semidet.
%
%   True when Window is in the tab that is in front.  A window on its way
%   out may be half taken apart by then, and not being able to tell is
%   the same answer as no.


in_view(Window) :-
    catch(( get(Window, container, tab, Tab),
            get(Tab, status, on_top)
          ), _, fail).

inject(F, Command:prolog) :->
    "Inject a command into the terminal in view"::
    get(F, current_terminal, Term),
    send(Term, inject, Command).

:- pce_end_class.

%!  terminal_base_label(+PrologTerminal, -Label) is det.
%
%   What a tab holding PrologTerminal is called, before it is made
%   unique within its window.

terminal_base_label(PT, Label) :-
    get(PT, profile, prolog),
    current_prolog_terminal(Thread, PT),
    atom(Thread),
    !,
    Label = Thread.
terminal_base_label(PT, Label) :-
    get(PT, profile, Profile),
    (   current_profile(Profile, Label)
    ->  true
    ;   Label = Profile
    ).

%!  unique_tab_label(+Frame, +Base, +N, -Label) is det.
%
%   Base, or Base with a number after it, such that no tab of Frame
%   carries it already.

%!  says_the_same(+Label, +Base) is semidet.
%
%   Label is Base, or Base with a number after it as `unique_tab_label/4'
%   writes them.  Either way it already says what Base says.

says_the_same(Label, Base) :-
    (   Label == Base
    ->  true
    ;   atom_concat(Base, Rest, Label),
        atom_concat(' ', Digits, Rest),
        atom_number(Digits, _)
    ).

unique_tab_label(F, Base, N, Label) :-
    (   N == 1
    ->  Try = Base
    ;   format(atom(Try), '~w ~d', [Base, N])
    ),
    (   tab_labelled(F, Try)
    ->  N2 is N+1,
        unique_tab_label(F, Base, N2, Label)
    ;   Label = Try
    ).

%!  tab_labelled(+Frame, +Label) is semidet.
%
%   A tab of Frame carries Label.  Not `tabbed_window <-tab', which finds
%   a tab by its name: a tab keeps the name it was made with however it
%   is labelled afterwards, and it is the label that has to be unique.

tab_labelled(F, Label) :-
    get(F, tabs, TW),
    get(TW, tabs, Chain),
    chain_list(Chain, Tabs),
    member(Tab, Tabs),
    get(Tab, label, Label),
    !.



                /*******************************
                *     XPCE CONSOLE OUTPUT      *
                *******************************/

%!  capture_messages(+PrologTerminal) is det.
%!  uncapture_messages(?PrologTerminal) is det.
%
%   Capture messages from XPCE's main thread in an Epilog console.

:- dynamic
    capturing/3.                        % PrologTerminal, Stdout, Stderr

capture_messages(PrologTerminal) :-
    thread_self(main),
    terminal_input(PrologTerminal, _PTY, _In,Out,Error, _EditLine),
    stream_property(Stdout, alias(user_output)),
    stream_property(Stderr, alias(user_error)),
    Stdout \== Out,
    Stderr \== Error,
    !,
    set_stream(Out, alias(current_output)),
    set_stream(Out, alias(user_output)),
    set_stream(Error, alias(user_error)),
    retractall(capturing(_,_,_)),
    asserta(capturing(PrologTerminal,Stdout,Stderr)).
capture_messages(_).

uncapture_messages(PrologTerminal) :-
    thread_self(main),
    retract(capturing(PrologTerminal,Stdout,Stderr)),
    !,
    set_stream(Stdout, alias(current_output)),
    set_stream(Stdout, alias(user_output)),
    set_stream(Stderr, alias(user_error)).
uncapture_messages(_).


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
    send(Term, window_label, Title).


                /*******************************
                *      DEDICATED WINDOWS       *
                *******************************/

%!  run_in_help_epilog(:Goal)
%
%   Run Goal in the `help` epilog frame.  Create this frame
%   if necessary.

run_in_help_epilog(Goal) :-
    get(@prolog_ide, member, help, Epilog),
    !,
    send(Epilog, expose),
    send(Epilog, inject, Goal).
run_in_help_epilog(Goal) :-
    epilog([ title('SWI-Prolog -- help'),
             name(help),
             init(true)
           ]),
    get(@prolog_ide, member, help, Epilog),
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
%
%   Ask for a title for the terminal of the calling thread.  Where it
%   lands is up to the window the terminal is on: a tab of its own puts
%   it on the tab and, while that tab is the one in view, on the frame.
%   This is the road a client that writes an OSC 0 takes as well.

window_title(Title) :-
    terminal(Term),
    send(Term, window_label, Title).

%!  win_insert_menu(+Label, +Before) is det.
%
%   Add a new popup to  the  Epilog   menu.  The  popus  is added before
%   Before. If Before is `-`, the new popup is added to the right.

win_insert_menu(Label, Before) :-
    terminal(Term),
    win_insert_menu(Term, Label, Before).

%       A menu added at runtime cannot simply be put on the bar: the bar
%       is rebuilt whenever the pane the user is working in changes, and
%       the next rebuild would take it away again.  It is registered with
%       the frame instead and replayed after every rebuild.

win_insert_menu(Term, Label, Before) :-
    get(Term, frame, Epilog),
    send(Epilog, extend_menu_bar,
         message(@prolog, ep_insert_menu, @arg1, Label, Before)).

:- public
    ep_insert_menu/3,
    ep_insert_menu_item/5.

ep_insert_menu(MD, Label, Before) :-
    get(MD, menu_bar, @on, MB),
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
    send(Epilog, extend_menu_bar,
         message(@prolog, ep_insert_menu_item, @arg1,
                 PopupName, Item, Before, prolog(Goal))).

ep_insert_menu_item(MD, PopupName, Item, Before, Goal) :-
    get(MD, menu_bar, @on, MB),
    get(MB, member, PopupName, Popup),
    get(MD, frame, Epilog),
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

                /*******************************
                *           MESSAGES           *
                *******************************/

:- multifile prolog:message//1.

prolog:message(epilog(Message)) -->
    epilog_message(Message).

epilog_message(already_attached(Thread, _PT)) -->
    [ 'Thread ~p already has a console'-[Thread] ].
epilog_message(cannot_attach(Thread)) -->
    [ 'Can not attach a console to thread ~p'-[Thread] ].
