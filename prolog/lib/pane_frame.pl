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


:- module(pane_frame,
          [ pane_frame_closed_tab/1     % +Frame
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(pce_template)).
:- use_module(library(tabbed_window), []).
:- use_module(library(tab_frame), []).
:- use_module(library(toolbar), []).
:- use_module(library(lists), [member/2]).

/** <module> One main window holding tools in tabs and panes

A pane_frame is the window a whole application lives in: a menu bar on
top, a set of tabs below it that each hold one or more panes side by
side, and optionally a bar at the bottom that reports and prompts.  It
is what lets a terminal, an editor and, in time, the navigator and the
thread monitor share one window.

The frame itself knows nothing about any of them.  What it does know is
when the pane the user is working in changes, and it then asks that pane
to say what the menu bar and the window title should be.  Everything an
application differs in lives in two places:

  - its `application` object, which fills the menus every pane shares
    and makes new panes;
  - the panes, which add their own menus and answer for their label.

Every message of the pane protocol is optional: the frame asks with
->has_send_method before sending, so a plain window is a usable pane.
See the `pane' template for the parts a pane would otherwise repeat.

@see library(tab_frame) for the tab that holds the panes.
*/

                 /*******************************
                 *            FRAME             *
                 *******************************/

:- pce_begin_class(pane_frame, frame,
                   "Main window holding tools in tabs and panes").

variable(menu_key,        name*  := @nil, get,
         "Identity of the menu bar now in place").
variable(menu_extensions, chain,          get,
         "Codes run after every rebuild of the menu bar").
variable(label_format,    'name*',   none,
         "Format for my label; %s is the label of the tab in view").
variable(updating,        bool := @off,   none,
         "->pane_changed is running").

class_variable(label_format, 'name*', 'SWI-Prolog -- %s',
               "Frame label; %s is the label of the tab in view").
class_variable(inactive_opacity, num, 1.0,
               "Opacity of a pane that has not got the focus").
class_variable(prompt_style, {status_bar,dialog}, dialog,
               "Ask for one value at a time, or all of them in a dialog").

:- pce_global(@pane_tab_popup, make_pane_tab_popup).

make_pane_tab_popup(P) :-
    new(P, popup),
    Tab = @arg1,
    Cond = (Tab?device?tabs?size \== 1),
    send_list(P, append,
              [ menu_item(close_tab,
                          message(Tab, close_tab)),
                menu_item(close_other_tabs,
                          message(Tab, close_other_tabs),
                          condition := Cond),
                menu_item(move_to_new_window,
                          message(Tab, untab),
                          condition := Cond)
              ]).

initialise(F, App:application=[application],
              Label:label=[name],
              Pane:pane=[window],
              Status:status_bar=[bool]) :->
    "Create from an application and a first pane"::
    send_super(F, initialise, Label, @default, @default, App),
    send(F, slot, menu_extensions, new(chain)),
    ignore(send(F, label_format_from_application)),
    send(F, done_message, message(F, close)),
    send(F, append, new(MD, pane_menu_dialog)),
    get(MD, menu_bar, @on, _),          % there is always a bar to fill
    send(new(TW, pane_tabbed_window), below, MD),
    (   get(F, application, App0),
        App0 \== @nil,
        send(App0, has_send_method, new_pane)
    ->  send(TW, new_tab_message, message(TW, new_pane))
    ;   true                            % no button: nothing to make
    ),
    (   status_bar(F, Status)
    ->  send(new(pane_status_dialog), below, TW)
    ;   true
    ),
    (   Pane == @default
    ->  true
    ;   send(F, append_pane, Pane, @default, @on)
    ),
    ignore(send(F, pane_changed)).      % nothing has moved the focus yet

%!  status_bar(+Frame, +Argument) is semidet.
%
%   True when Frame is to have a bar at the bottom.  The application
%   decides unless the caller said.

status_bar(_F, @on) :- !.
status_bar(_F, @off) :- !, fail.
status_bar(F, _) :-
    get(F, application, App),
    App \== @nil,
    send(App, has_get_method, status_bar),
    get(App, status_bar, @on).

                 /*******************************
                 *           STRUCTURE          *
                 *******************************/

menu_dialog(F, MD:pane_menu_dialog) :<-
    "The dialog carrying my menu bar"::
    get(F, member, pane_menu_dialog, MD).

menu_bar(F, MB:menu_bar) :<-
    "My menu bar"::
    get(F, menu_dialog, MD),
    get(MD, menu_bar, @on, MB).

tabs(F, TW:pane_tabbed_window) :<-
    "The tabbed window holding my panes"::
    get(F, member, pane_tabbed_window, TW).

status_dialog(F, SD:pane_status_dialog) :<-
    "The bar at my bottom; fails if I have none"::
    get(F, member, pane_status_dialog, SD).

tab(F, Tab:pane_tab) :<-
    "The tab in view"::
    get(F, tabs, TW),
    get(TW, on_top, Tab).

panes(F, Panes:chain) :<-
    "New chain holding my panes, over all tabs"::
    get(F, tabs, TW),
    get(TW, members, Panes).

current_pane(F, Pane:window) :<-
    "The pane the user is working in"::
    get(F, tabs, TW),
    get(TW, current, Pane).

current_pane(F, Pane:window) :->
    "Make Pane the one the user is working in"::
    get(F, tabs, TW),
    send(TW, current, Pane).

                 /*******************************
                 *            PANES             *
                 *******************************/

append_pane(F, Pane:window, Label:[name], Expose:[bool]) :->
    "Add Pane in a tab of its own"::
    get(F, tabs, TW),
    send(TW, append, Pane, Label, Expose).

split(F, Pane:window,
         Relative:relative_to=[window],
         Direction:direction=[{horizontally,vertically}]) :->
    "Add Pane beside Relative, in the tab Relative is in"::
    (   Relative == @default
    ->  get(F, current_pane, Rel)
    ;   Rel = Relative
    ),
    get(Rel, container, tab_frame, Tab),
    send(Tab, split, Pane, Rel, Direction),
    send(F, keyboard_focus, Pane).

delete_pane(F, Pane:window, Destroy:[bool]) :->
    "Take Pane out of its tab; destroy me if it was my last"::
    get(F, panes, Panes),
    (   get(Panes, size, Size),
        Size > 1
    ->  get(Pane, container, tab_frame, Tab),
        send(Tab, delete, Pane),
        (   Destroy == @on
        ->  send(Pane, destroy)
        ;   true
        ),
        send(F, pane_changed)
    ;   send(F, empty)
    ).

new_pane(F, Kind:[name]) :->
    "Add a pane of the kind my application makes"::
    get(F, application, App),
    App \== @nil,
    send(App, has_send_method, new_pane),
    send(App, new_pane, F, Kind).

empty(F) :->
    "My last pane is gone"::
    send(F, destroy).

                 /*******************************
                 *           CLOSING            *
                 *******************************/

:- dynamic
    closed_tab/2.                       % Frame, Time

%!  pane_frame_closed_tab(+Frame) is det.
%
%   Remember that a tab of Frame was just closed.  On MacOS, Command-W
%   closes the tab *and* asks the frame to close, and the frame must let
%   that second request go.  Exported, so that a key binding that closes
%   a tab itself can say so.

pane_frame_closed_tab(Frame) :-
    get_time(Now),
    forget_closed_tabs(Now),
    asserta(closed_tab(Frame, Now)).

closed_tab_just_now(Frame) :-
    retract(closed_tab(Frame, Time)),
    get_time(Now),
    Now-Time < 0.5.

%!  forget_closed_tabs(+Now) is det.
%
%   Drop what is too old to be the second half of a Command-W.  A frame
%   that is closed without one leaves its note behind otherwise.

forget_closed_tabs(Now) :-
    forall(( closed_tab(F, Time),
             Now-Time >= 0.5
           ),
           retractall(closed_tab(F, Time))).

close(F) :->
    "The user asked to close me"::
    (   closed_tab_just_now(F)
    ->  true
    ;   get(F, tabs, TW),
        get(TW?tabs, size, Count),
        (   Count > 1,
            \+ send(F, confirm, 'Close %d tabs?', Count)
        ->  true
        ;   get(F, can_close, @on)
        ->  send(F, destroy)
        ;   true
        )
    ).

can_close(F, Reply:bool) :<-
    "@on if every pane of mine agrees to be closed"::
    get(F, panes, Panes),
    chain_list(Panes, List),
    (   member(P, List),
        send(P, has_get_method, can_close),
        \+ get(P, can_close, @on)
    ->  Reply = @off
    ;   Reply = @on
    ).

confirm(F, Format:char_array, Args:any...) :->
    "Ask the user a yes/no question, centred on me"::
    new(D, dialog('Confirm action')),
    String =.. [string, Format | Args],
    send(D, append, label(message, String)),
    send(D, append, button(ok, message(D, return, ok))),
    send(D, append, button(cancel, message(D, return, cancel))),
    send(D, modal, transient),
    get(D, confirm_centered, F, Rval),
    send(D, destroy),
    Rval == ok.

                 /*******************************
                 *           THE HOOK           *
                 *******************************/

%       ->pane_changed is the one place that answers "the user is now
%       working somewhere else".  It is sent from ->keyboard_focus, which
%       is what a click on a pane reaches (see postEventWindow()), from
%       the tabbed window when a tab is raised, and when a pane is taken
%       away.  Those overlap -- raising a tab moves the focus as well --
%       so it guards against running inside itself.

pane_changed(F) :->
    "The pane the user is working in changed"::
    (   get(F, slot, updating, @on)
    ->  true
    ;   setup_call_cleanup(
            send(F, slot, updating, @on),
            ignore(send(F, do_pane_changed)),
            send(F, slot, updating, @off))
    ).

do_pane_changed(F) :->
    "Follow the current pane; see ->pane_changed"::
    get(F, current_pane, Pane),
    send(F?menu_dialog, client, Pane),
    ignore(send(F, update_menu_bar)),
    ignore(send(F, update_label)),
    ignore(send(F, update_opacity)),
    (   send(Pane, has_send_method, pane_exposed)
    ->  ignore(send(Pane, pane_exposed))
    ;   true
    ).

input_focus(F, Val:bool) :->
    "The window manager gave me the focus, or took it away"::
    send_super(F, input_focus, Val),
    (   \+ send(F, unlinking),
        get(F, current_pane, Pane),
        send(Pane, has_send_method, frame_active)
    ->  ignore(send(Pane, frame_active, Val))
    ;   true
    ).

keyboard_focus(F, W:[window]*) :->
    "Follow the focus as it moves between my panes"::
    (   get(F, prompter, _),
        get(F, status_dialog, SD),
        W \== SD
    ->  send_super(F, keyboard_focus, SD)
    ;   send_super(F, keyboard_focus, W),
        (   send(W, instance_of, window)
        ->  send(F, pane_changed)
        ;   true
        )
    ).

fit(F) :->
    "Fit around my contents, but resize rather than refit"::
    (   get(F, attribute, fitted, @on)
    ->  send(F, resize)
    ;   send_super(F, fit),
        send(F, attribute, fitted, @on)
    ).

on_current_desktop(F) :->
    "True if I am for more than half on the desktop in view"::
    (   get(@pce, window_system, sdl)
    ->  true
    ;   get(F, area, FArea),
        (   object(FArea, area(-32000, -32000, _, _))
        ->  true                        % MS-Windows iconized
        ;   get(F?display, size, size(DW,DH)),
            get(FArea, intersection, area(0,0,DW,DH), Intersection),
            get(FArea, measure, MA),
            get(Intersection, measure, IA),
            IA > MA/2
        )
    ).

                 /*******************************
                 *           MENU BAR           *
                 *******************************/

menu_bar_key(F, Key:name) :<-
    "Identity of the menu bar my current pane would build"::
    get(F, current_pane, Pane),
    (   send(Pane, has_get_method, menu_bar_key)
    ->  get(Pane, menu_bar_key, Key)
    ;   get(Pane, class_name, Key)      % one bar per pane class
    ).

fill_menu_bar(F, MD:tool_dialog) :->
    "Put the menus every pane of mine shares on the bar"::
    get(F, application, App),
    App \== @nil,
    send(App, has_send_method, fill_menu_bar),
    send(App, fill_menu_bar, MD, F).

%       The bar is cleared and rebuilt rather than patched: that is what
%       PceEmacs has always done for its mode menus, and it is the only
%       way a bar assembled from two sides stays in the right order.
%       <-menu_bar_key is what keeps it from happening on every click:
%       the *state* of the menus -- which items are ticked or greyed --
%       needs no rebuild, as `menu_item ->condition' and `popup
%       ->update_message' are run when a menu opens.

update_menu_bar(F, Force:[bool]) :->
    "Rebuild the menu bar if the current pane wants a different one"::
    (   Force \== @on,
        get(F, menu_bar_key, Key),
        get(F, menu_key, Key)
    ->  true
    ;   (   get(F, menu_bar_key, Key)
        ->  true
        ;   Key = @nil
        ),
        send(F, slot, menu_key, Key),
        get(F, menu_dialog, MD),
        send(MD?menu_bar, clear),
        ignore(send(F, fill_menu_bar, MD)),
        (   get(F, current_pane, Pane),
            send(Pane, has_send_method, fill_menu_bar)
        ->  ignore(send(Pane, fill_menu_bar, MD))
        ;   true
        ),
        send(F?menu_extensions, for_all, message(@arg1, forward, MD))
    ).

%       A menu somebody added at runtime -- see Epilog's win_insert_menu/2
%       -- cannot simply be put on the bar: the next pane switch would
%       take it away again.  It is registered here instead and replayed
%       after every rebuild.

extend_menu_bar(F, Code:code) :->
    "Run Code over my menu dialog after every rebuild, and now"::
    send(F?menu_extensions, append, Code),
    send(F, update_menu_bar, @on).

                 /*******************************
                 *            LABEL             *
                 *******************************/

%       The title runs one way: the pane says what it is called, that is
%       the label of its tab, and the frame makes its title out of the
%       label of the tab in view.  ->update_label is the only thing that
%       writes the title, so there is one place that decides what a
%       window of this application is called.

label_format(F, Fmt:'[name]*') :->
    "Set the format my label is made with"::
    send(F, slot, label_format, Fmt),
    ignore(send(F, update_label)).

label_format(F, Fmt:'name*') :<-
    "The format my label is made with"::
    get(F, slot, label_format, Fmt).

%       Three say what a window of this application is called, the later
%       overruling the earlier: the class variable, so that a user can set
%       it in their Defaults; the application, so that PceEmacs and Epilog
%       differ; and ->label_format, for one frame on its own.  The first
%       two are settled here, once, as the slot starts out holding the
%       class variable.

label_format_from_application(F) :->
    "Take the format my application asks for"::
    get(F, application, App),
    App \== @nil,
    send(App, has_get_method, label_format),
    get(App, label_format, Fmt),
    send(F, slot, label_format, Fmt).

tab_label(F, Label:name) :<-
    "The label of the tab in view"::
    get(F, tab, Tab),
    get(Tab, label, Label).

update_label(F) :->
    "Make my title out of the label of the tab in view"::
    get(F, tab_label, TabLabel),
    get(F, label_format, Fmt),
    (   Fmt == @nil
    ->  send(F, label, TabLabel)
    ;   send(F, label, string(Fmt, TabLabel))
    ).

                 /*******************************
                 *           OPACITY            *
                 *******************************/

%       A tab may show several panes at once and only one of them has the
%       keyboard.  Fading the others says which, without taking room for
%       a border or a title.  It is off by default: set `inactive_opacity'
%       on the pane class, or on pane_frame for all of them.

update_opacity(F) :->
    "Fade every pane of the tab in view but the current one"::
    get(F, tab, Tab),
    get(F, current_pane, Current),
    get(Tab, windows, Chain),
    chain_list(Chain, Panes),
    forall(member(P, Panes),
           set_opacity(F, P, Current)).

set_opacity(F, P, Current) :-
    (   P == Current
    ->  Opacity = 1.0
    ;   get(F, pane_opacity, P, Opacity)
    ),
    send(P, opacity, Opacity).          % ->opacity itself ignores a no-op

pane_opacity(F, Pane:window, Opacity:num) :<-
    "How far Pane is faded while another pane has the focus"::
    (   get(Pane, class_variable_value, inactive_opacity, Opacity)
    ->  true
    ;   get(F, class_variable_value, inactive_opacity, Opacity)
    ).

                 /*******************************
                 *      REPORT AND PROMPT       *
                 *******************************/

%       These four are what an editor pane asks of its frame.  Each of
%       them must do something harmless when there is no bar at the
%       bottom: a caller guards with ->has_send_method, which says the
%       method is there, not that it can work.

prompter(F, Prompter:dialog_item) :<-
    "The item the bar at my bottom is prompting with"::
    get(F, status_dialog, SD),
    get(SD, prompter, Prompter),
    Prompter \== @nil.

prompt_using(F, Item:dialog_item, Rval:unchecked) :<-
    "Prompt for a value using Item"::
    (   get(F, status_dialog, SD)
    ->  get(F, prompt_in_status_dialog, SD, Item, Rval)
    ;   new(D, dialog),
        send(D, transient_for, F),
        send(D, modal, transient),
        send(D, append, Item),
        get(D, confirm_centered, Rval)
    ).

:- pce_global(@pane_prompt_recogniser, make_pane_prompt_recogniser).

make_pane_prompt_recogniser(G) :-
    new(G, key_binding(pane_frame_prompter, text_item)),
    send(G, function, 'TAB',  complete),
    send(G, function, 'SPC',  insert_self),
    send(G, function, 'RET',  if(message(@receiver, apply, @on))),
    send(G, function, '\\C-g', and(message(@receiver, keyboard_quit),
                                   message(@receiver?frame, return,
                                           canceled))).

prompt_in_status_dialog(F, SD:pane_status_dialog,
                           Item:dialog_item, Rval:unchecked) :<-
    "Prompt for a value on the bar at my bottom"::
    get(F, current_pane, Pane),
    get(F, menu_bar, MB),
    send(MB, active, @off),

    send(SD, client, Pane),
    send(SD, prompter, Item),
    send(Item, message, message(F, return, ok)),
    (   send(Item, instance_of, text_item)
    ->  send(Item, recogniser, @pane_prompt_recogniser),
        send(Item, value_font, fixed)
    ;   true
    ),
    send(F, keyboard_focus, SD),
    get(F, confirm, Return),
    object(F),                          % may be freed!

    (   Return == ok
    ->  get(Item, selection, Rval)
    ;   true
    ),

    send(Item, message, @nil),
    send(Item, lock_object, @on),       % the lifetime of Item is the
    send(SD, prompter, @nil),           % caller's, not ours
    get(Item, unlock, Item),
    get(F, current_pane, Pane2),
    send(F, keyboard_focus, Pane2),
    send(MB, active, @on),
    Return == ok.

reset(F) :->
    "Take a prompter away after an abort"::
    send_super(F, reset),
    (   get(F, status_dialog, SD)
    ->  send(SD, prompter, @nil)
    ;   true
    ),
    (   get(F, menu_bar, MB)
    ->  send(MB, active, @on)
    ;   true
    ).

editor_event(F, Ev:event) :->
    "Give a key typed in a pane to the prompter"::
    get(F, status_dialog, SD),
    send(SD, editor_event, Ev).

show_line_number(F, Line:'int|{too_expensive}*') :->
    "Show the line the caret is on"::
    (   get(F, status_dialog, SD)
    ->  send(SD, show_line_number, Line)
    ;   true
    ).

:- pce_end_class(pane_frame).


                 /*******************************
                 *          MENU DIALOG         *
                 *******************************/

:- pce_begin_class(pane_menu_dialog, tool_dialog,
                   "Dialog carrying the menu bar of a pane_frame").

initialise(MD, Client:[object]) :->
    "Create without a border"::
    send_super(MD, initialise, Client),
    send(MD, gap, size(0,0)),
    send(MD, border, size(0,0)),
    send(MD, pen, 0).

client(MD, Client:[object]) :->
    "Say which object a menu item without a message goes to"::
    send(MD, slot, client, Client).

menu_bar(MD, Create:[bool], MB:menu_bar) :<-
    "Get (or create) the menu bar"::
    (   get(MD, member, menu_bar, MB)
    ->  true
    ;   Create == @on
    ->  (   get(MD, tool_bar, TB)
        ->  send(new(MB, pane_menu_bar), above, TB)
        ;   send_super(MD, append, new(MB, pane_menu_bar))
        )
    ).

assign_accelerators(_) :->
    "Accelerators are defined by the panes"::
    true.

:- pce_end_class(pane_menu_dialog).


:- pce_begin_class(pane_menu_bar, menu_bar,
                   "Menu bar of a pane_frame").

initialise(MB) :->
    "Create empty, under the name my dialog looks me up by"::
    send_super(MB, initialise),
    send(MB, name, menu_bar).

assign_accelerators(_) :->
    "Accelerators are defined by the panes"::
    true.

:- pce_end_class(pane_menu_bar).


:- pce_begin_class(pane_popup, popup,
                   "Popup of a pane_frame menu bar").

class_variable(accelerator_font, font, small,
               "Font the accelerator is written in").

assign_accelerators(_) :->
    "Accelerators are defined by the panes"::
    true.

:- pce_end_class(pane_popup).


                 /*******************************
                 *             TABS             *
                 *******************************/

:- pce_begin_class(pane_tabbed_window, tabbed_window,
                   "The tabs of a pane_frame").

initialise(TW, Label:label=[name], Size:size=[size],
               Display:display=[display]) :->
    "Create with a popup on the labels and a new-tab button"::
    send_super(TW, initialise, Label, Size, Display),
    send(TW, hide_single_label, @on),   % one tab needs no name
    send(TW, label_popup, @pane_tab_popup).

new_pane(TW) :->
    "The new-tab button was pressed"::
    send(TW?frame, new_pane).

new_tab(_TW, Window:window, Label:[name], Tab:tab) :<-
    "A tab of mine holds one or more panes"::
    (   Label == @default,
        send(Window, has_get_method, pane_label),
        get(Window, pane_label, TheLabel)
    ->  true
    ;   TheLabel = Label
    ),
    new(Tab, pane_tab(Window, TheLabel)),
    %  A pane that keeps pushing a label out -- an editor showing a
    %  buffer -- would take a hand-typed one straight back off again.
    (   send(Window, has_get_method, tab_editable_label)
    ->  send(Tab, editable_label, Window?tab_editable_label)
    ;   true
    ).

%       Not `get_super(TW, member, tab_stack, TS)': in a subclass the
%       super is class tabbed_window, whose <-member answers the *window*
%       of a named tab.  The tabs are what I have to hand anyway.

on_top(TW, Tab:tab) :<-
    "The tab in view"::
    get(TW, tabs, Tabs),
    get(Tabs, find, @arg1?status == on_top, Tab).

current(TW, Window:window) :->
    "Make Window the current pane and tell the frame"::
    send_super(TW, current, Window),
    (   get(TW, frame, Frame),
        Frame \== @nil,
        send(Frame, has_send_method, pane_changed)
    ->  send(Frame, pane_changed)
    ;   true
    ).

empty(TW) :->
    "My last tab was closed"::
    send(TW?frame, empty).

frame_window(_TW, Window:window, _Name:name, _Rank:'1..', Frame:frame) :<-
    "After un-tabbing, give the pane a frame of its own"::
    get(Window, frame, Old),
    get(Old, application, App),
    new(Frame, pane_frame(App, @default, Window)).

:- pce_end_class(pane_tabbed_window).


:- pce_begin_class(pane_tab, tab_frame,
                   "Tab of a pane_frame, holding one or more panes").

class_variable(editable_label, bool, @on,
               "A tab is named by the user, so let them").
class_variable(closable,       bool, @on,
               "A tab carries a button to close it").

%       tab_frame ->status only tells the tabbed window when the stack is
%       displayed, which is not yet so while a frame is being built.  The
%       frame wants to know either way: its menu bar and its title come
%       from the tab in view.  ->pane_changed costs nothing when nothing
%       has really changed.

status(Tab, Status:{on_top,hidden}) :->
    "Tell my frame when I come into view"::
    send_super(Tab, status, Status),
    (   Status == on_top,
        get(Tab, frame, Frame),
        Frame \== @nil,
        send(Frame, has_send_method, pane_changed)
    ->  send(Frame, pane_changed)
    ;   true
    ).

close_tab(Tab) :->
    "Close my panes, which takes me with them"::
    (   get(Tab, frame, Frame),
        Frame \== @nil
    ->  pane_frame_closed_tab(Frame)
    ;   true
    ),
    send(Tab, close).

close(Tab) :->
    "Close my panes"::
    get(Tab, windows, Chain),
    chain_list(Chain, Panes),
    (   member(P, Panes),
        send(P, has_get_method, can_close),
        \+ get(P, can_close, @on)
    ->  true
    ;   forall(member(P, Panes), close_pane(P))
    ).

close_pane(P) :-
    (   send(P, has_send_method, close_pane)
    ->  send(P, close_pane)
    ;   send(P, destroy)
    ).

close_other_tabs(Tab) :->
    "Close the panes of every other tab"::
    get(Tab?device, tabs, Chain),
    chain_list(Chain, Tabs),
    forall(( member(Other, Tabs),
             Other \== Tab,
             send(Other, instance_of, pane_tab)
           ),
           send(Other, close)).

:- pce_end_class(pane_tab).


                 /*******************************
                 *          STATUS BAR          *
                 *******************************/

/** The bar at the bottom of a pane_frame.

It does three things at once, as the minibuffer of an editor does: it
carries the reporter every ->report of the frame ends up on, it is where
a pane prompts for a value, and it shows the line the caret is on at the
right.  A frame may be built without one; everything that uses it copes
with its absence.

The label on it is *named* `reporter', which is what makes `dialog
<-report_to' hand reports to it.
*/

:- pce_begin_class(pane_status_dialog, dialog,
                   "Report and prompt bar at the bottom of a pane_frame").

variable(prompter,     dialog_item*, get,  "Item being prompted with").
variable(report_count, number,       get,  "Count down to erasing a report").
variable(report_type,  name*,        both, "Kind of the last report").

initialise(D) :->
    "Create the reporter and the line counter"::
    send_super(D, initialise),
    send(D, slot, report_count, number(0)),
    send(D, gap, size(10, 2)),
    send(D, pen, 0),
    send(D, display, new(R, label(reporter)), point(0, 2)),
    send(R, wrap, clip),
    send(D, display, new(T, text('', right, normal)), point(100, 2)),
    send(T, name, line),
    get(text_item(''), height, MH),
    send(D, height, MH).

resize(D) :->
    "Keep the line counter against my right edge"::
    get(D, member, line, Text),
    get(D?area, width, W),
    get(Text, width, TW),
    send(Text, x, W-TW-16).

'_compute_desired_size'(_) :->
    "I have a fixed height"::
    true.

geometry(D, X:[int], Y:[int], W:[int], H:[int]) :->
    "Change size, centring my contents vertically"::
    send_super(D, geometry, X, Y, W, H),
    get(D, height, DH),
    DH2 is round(DH/2),
    send(D?graphicals, for_all,
         message(@arg1, center_y, DH2)).

client(D, Client:window) :->
    "Register the pane I am prompting for"::
    send(D, delete_hypers, client),
    new(_, hyper(D, Client, client, status_dialog)).

client(D, Client:window) :<-
    "The pane I am prompting for"::
    get(D, hypered, client, Client).

                 /*******************************
                 *            REPORT            *
                 *******************************/

report(D, Type:name, Fmt:[char_array], Args:any...) :->
    "Show a message on my reporter"::
    (   get(D, report_type, ReportType),
        ok_to_overrule(Type, ReportType)
    ->  send(D, report_type, Type),
        get(D, member, reporter, Label),
        get(D, report_count, RC),
        (   Fmt == '', Type == status       % clear
        ->  send(Label, clear),
            send(RC, value, 0)
        ;   (   get(D, prompter, Prompter), Prompter \== @nil
            ->  send(Label, x, Prompter?width + 10)
            ;   send(Label, x, 0)
            ),
            send(Label, displayed, @on),
            Msg =.. [report, Type, Fmt|Args],
            send(Label, Msg),
            send(RC, value, 10)
        )
    ;   true
    ).

%!  ok_to_overrule(+New, +Standing) is semidet.
%
%   True when a report of kind New may take the place of one of kind
%   Standing.  A warning does not push aside an error.

ok_to_overrule(_, @nil).
ok_to_overrule(_, status).
ok_to_overrule(_, progress).
ok_to_overrule(_, done).
ok_to_overrule(warning, warning).
ok_to_overrule(inform, _).
ok_to_overrule(error, _).

show_line_number(D, Line:'int|{too_expensive}*') :->
    "Show the line the caret is on"::
    get(D, member, line, Text),
    (   Line == @nil
    ->  send(Text, string, '')
    ;   Line == too_expensive
    ->  send(Text, string, 'Line: ?')
    ;   send(Text, string, string('Line: %d', Line))
    ).

                 /*******************************
                 *            PROMPT            *
                 *******************************/

prompter(D, Prompter:dialog_item*) :->
    "Display Prompter, or take the one I have away"::
    get(D, member, reporter, Reporter),
    (   get(D, prompter, OldPrompter), OldPrompter \== @nil
    ->  send(D, erase, OldPrompter)
    ;   true
    ),
    (   Prompter == @nil
    ->  send(Reporter, clear),
        send(Reporter, displayed, @on)
    ;   send(Reporter, displayed, @off),
        get(Prompter, height, H),
        get(D, height, DH),
        PY is (DH-H)/2,
        send(D, display, Prompter, point(25, PY)),
        (   DH < H
        ->  send(D, height, H)
        ;   true
        )
    ),
    send(D, slot, prompter, Prompter).

:- pce_global(@pane_status_bindings, make_pane_status_bindings).

make_pane_status_bindings(B) :-
    new(B, key_binding(pane_status_dialog)),
    send(B, function, '\\en', m_x_next),
    send(B, function, '\\ep', m_x_previous).

editor_event(D, Ev:event) :->
    "Take a key typed in a pane while I am prompting"::
    (   get(D, prompter, Prompter),
        Prompter \== @nil
    ->  (   send(@pane_status_bindings, event, Ev)
        ->  true
        ;   get(D, member, reporter, Reporter),
            send(Reporter, displayed, @off),
            ignore(send(Ev, post, Prompter))
        )
    ;   send(D, report_type, @nil),
        get(D, report_count, RC),
        send(RC, minus, 1),
        (   send(RC, equal, 0)
        ->  send(D, report, status, '')
        ),
        fail
    ).

event(D, Ev:event) :->
    "While I am prompting, every key is the prompter's"::
    (   get(D, prompter, Prompter),
        Prompter \== @nil
    ->  send(D, editor_event, Ev)
    ;   send_super(D, event, Ev)
    ).

%       M-n and M-p walk the history of whatever the pane is prompting
%       for.  Only a pane that keeps one answers; the rest put the
%       default back.

m_x_next(D) :->
    "Show the next value from the history"::
    (   history_source(D, Source),
        get(Source, m_x_next, NewDefault)
    ->  send(D?prompter, displayed_value, NewDefault?print_name)
    ;   send(D?prompter, restore)
    ).

m_x_previous(D) :->
    "Show the previous value from the history"::
    history_source(D, Source),
    get(Source, m_x_previous, NewDefault),
    send(D?prompter, displayed_value, NewDefault?print_name).

%!  history_source(+StatusDialog, -Source) is semidet.
%
%   The object that keeps the history of the pane being prompted for.
%   An editor pane keeps it on its mode; anything else answers for
%   itself, or not at all.

history_source(D, Source) :-
    get(D, client, Client),
    (   send(Client, has_get_method, mode),
        get(Client, mode, Mode),
        send(Mode, has_get_method, m_x_previous)
    ->  Source = Mode
    ;   send(Client, has_get_method, m_x_previous),
        Source = Client
    ).

:- pce_end_class(pane_status_dialog).


                 /*******************************
                 *         PANE TEMPLATE        *
                 *******************************/

/** What a pane does not have to write out for itself.

A pane is any window in a pane_tab, and none of the pane protocol is
compulsory.  This template carries the parts that would otherwise be
repeated by every pane that does want them: reaching my frame and my
tab, splitting, opening a new tab or window, and closing.

Use it as:

```
:- pce_begin_class(my_pane, window, "...").
:- use_class_template(pane).
```

A class that uses it must answer `<-sibling' with a new pane of its own
kind for ->split and ->new_tab to have anything to put there.
*/

:- pce_begin_class(pane, template,
                   "Common behaviour of a window in a pane_frame").

pane_label(P, Label:name) :<-
    "What my tab is called; my name unless I say otherwise"::
    get(P, name, Label).

pane_frame(P, Frame:pane_frame) :<-
    "The frame I am a pane of"::
    get(P, frame, Frame),
    send(Frame, instance_of, pane_frame).

pane_tab(P, Tab:tab_frame) :<-
    "The tab I am in"::
    get(P, container, tab_frame, Tab).

split(P, Direction:[{horizontally,vertically}]) :->
    "Put a new pane like me beside me"::
    get(P, sibling, New),
    get(P, pane_tab, Tab),
    send(Tab, split, New, P, Direction),
    (   get(P, pane_frame, Frame)
    ->  send(Frame, keyboard_focus, New)
    ;   true
    ).

new_tab(P) :->
    "Put a new pane like me in a tab of its own"::
    get(P, sibling, New),
    get(P, pane_frame, Frame),
    send(Frame, append_pane, New, @default, @on),
    send(Frame, keyboard_focus, New).

new_window(P) :->
    "Put a new pane like me in a window of its own"::
    get(P, sibling, New),
    (   get(P, pane_frame, Frame)
    ->  get(Frame, application, App)
    ;   App = @default
    ),
    send(new(pane_frame(App, @default, New)), open).

close_pane(P) :->
    "Close me; my frame goes with me if I was its last pane"::
    (   get(P, pane_frame, Frame)
    ->  send(Frame, delete_pane, P, @on)
    ;   send(P, destroy)
    ).

place_pane_handle(P, Inset:[int]) :->
    "Put my split_handle back in my corner"::
    (   get(P, member, split_handle, Handle)
    ->  send(Handle, place, P, Inset)
    ;   true                            % still being built
    ).

:- pce_end_class(pane).
