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
          [ pane_frame_closed_tab/1,    % +Frame
            show_pane/1,                % +Pane
            pane_status_bar/1,          % +Pane
            open_pane_frame/2,          % +Term, -Frame
            open_pane_frame/3,          % +Term, -Frame, +Options
            pane_kind/2,                % +Pane, -Kind
            pane_tree_term/4,           % :Leaf, +Tree, +Current, -Content
            build_pane_tree/5           % :MakeLeaf, +Content, -Tree,
          ]).                           %   -First, -Current
:- use_module(library(pce)).
:- use_module(library(swi_ide), []).    % get @prolog_ide application
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(gensym), [gensym/2]).
:- use_module(library(pce_template)).
:- use_module(library(tabbed_window), []).
:- use_module(library(tab_frame), []).
:- use_module(library(toolbar), []).
:- use_module(library(lists), [member/2, max_list/2, nth1/3, last/2]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(pane_layouts),
              [ arrangement_of/2, record_arrangement/2,
                remember_arrangement/1
              ]).

:- meta_predicate
    pane_tree_term(2, +, +, -),
    share_term(2, +, +, -),
    build_pane_tree(2, +, -, -, -),
    build_shares(2, +, -).

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

A pane need not be one window.  A `pane_stack' holds its windows in tabs
of its own: `tool_pane' uses a single tab to lay a tool's windows out
side by side, and `emacs_pane' (see library(emacs/window)) uses many, one
per source.  A stack with tabs is a *group*: the frame looks inside it
for the pane the user is really working in -- see <-current_pane -- and
<-pane_group is the way back out to the pane its tab tiles.

A window can be written down as a Prolog term saying what it holds and
how it is laid out, and built back from one:

```
?- get(F, pane_term, Term).
Term = pane_frame([geometry('1200x800+40+40')],
                  [ tab([current(true)],
                        vertical([ 0.7-current(editor([file('foo.pl'),
                                                       line(120)])),
                                   0.3-terminal([profile(shell)]) ]))
                  ]).
?- open_pane_frame(Term, _F).
```

See the PANE TERM section below for what a term may say, and
`open_pane_frame/2' for making a window out of one.  A term can be
written by hand: everything in it is optional.

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
variable(own_label_format, [name]* := @default, none,
         "Format asked for on me alone; @default: ask elsewhere").
variable(updating,        bool := @off,   none,
         "->pane_changed is running").
variable(arranged,        bool := @off,   get,
         "The user has arranged my panes by hand").
variable(arrangement,     prolog := none, none,
         "The arrangement they last left me in; none: not yet").
variable(arranged_since,  prolog := none, none,
         "When they left me in it; none: not yet").

class_variable(label_format, 'name*', 'SWI-Prolog -- %s',
               "Frame label; %s is the label of the tab in view").
class_variable(inactive_opacity, num, 1.0,
               "Opacity of a pane that has not got the focus").
class_variable(prompt_style, {status_bar,dialog}, dialog,
               "Ask for one value at a time, or all of them in a dialog").
class_variable(focus_on_enter, bool, @off,
               "Give a pane the focus when the pointer enters it").

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
    name_frame(F),
    send(F, slot, own_label_format, @default),  % a slot declared `:= @default'
                                                % is @nil until it is told

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
    (   status_bar(Status)
    ->  send(new(pane_status_dialog), below, TW)
    ;   true
    ),
    (   Pane == @default
    ->  true
    ;   send(F, append_pane, Pane, @default, @on)
    ),
    ignore(send(F, pane_changed)).      % nothing has moved the focus yet

%!  show_pane(+Pane) is det.
%
%   Put Pane in a window of the IDE, or bring the window it is in up with
%   Pane in view.  This is what ->open means for a pane: it has no window
%   of its own to open.  The IDE is asked for at need, so that a tool of
%   XPCE's own does not load the Prolog IDE to be able to run.

show_pane(Pane) :-
    (   get(Pane, pane_tab, _)          % a pane of a window already.  Not
    ->  send(@prolog_ide, expose_tool, Pane)  % <-frame: a window that is
    ;   send(@prolog_ide, place_tool, Pane, @default)  % in none gets one
    ).

%!  pane_status_bar(+Pane) is det.
%
%   Make sure the window Pane is in has a bar to report on.  A pane that
%   has something to say calls this first: a window grows its status bar
%   the first time anything wants one -- see <-ensure_status_dialog --
%   and a pane that is in no window of the IDE has nowhere to grow one.

pane_status_bar(Pane) :-
    (   get(Pane, frame, Frame),
        Frame \== @nil,
        send(Frame, has_get_method, ensure_status_dialog)
    ->  ignore(get(Frame, ensure_status_dialog, _))
    ;   true
    ).

%!  name_frame(+Frame) is det.
%
%   Give Frame a name of its own.  A frame is named after its class, so
%   every pane_frame would be called `pane_frame' and `application
%   <-member(Name)' -- how a tool asks whether its window is already open
%   -- could not tell two of them apart.  A caller that has a name in
%   mind sets it afterwards, as epilog/1 does with `main' and `help'.

name_frame(F) :-
    gensym(pane_frame, Name),
    send(F, name, Name).

%!  placed_area(+Window, -Area) is det.
%
%   Where a window sits among the ones it is tiled with.  A window that
%   carries a label or scrollbars of its own is wrapped in a
%   window_decorator, and it is the decorator the tile places.

placed_area(W, Area) :-
    (   get(W, decoration, Decor),
        Decor \== @nil
    ->  get(Decor, area, Area)
    ;   get(W, area, Area)
    ).

%!  modal_transient(+Frame) is semidet.
%
%   True while a transient window of Frame is up.  The focus must not be
%   taken from under it.

modal_transient(F) :-
    get(F, transients, Transients),
    Transients \== @nil,
    get(Transients, find, @arg1?modal == transient, _).

%!  status_bar(+Argument) is semidet.
%
%   True when a frame is to be built with a bar at the bottom.  Only a
%   caller who says so: a window that never prompts should not carry one,
%   and one that does grows it when it is first wanted -- see
%   <-ensure_status_dialog.  A terminal reports on a bar of its own, over
%   its own text, and an editor wants a minibuffer, so this cannot be
%   settled per application once a window may hold either.

status_bar(@on).

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

%       A window grows a bar the first time a pane wants to prompt on one
%       or to say which line the caret is on.  A window of terminals never
%       asks, and stays as bare as it always was; drop an editor into one
%       and the bar appears.

ensure_status_dialog(F, SD:pane_status_dialog) :<-
    "The bar at my bottom, made if I have none"::
    (   get(F, member, pane_status_dialog, SD)
    ->  true
    ;   get(F, tabs, TW),
        send(new(SD, pane_status_dialog), below, TW),
        (   get(F, status, unmapped)
        ->  true                        % not open yet: it lays out on ->open
        ;   send(F, resize)
        )
    ).

tab(F, Tab:pane_tab) :<-
    "The tab in view"::
    get(F, tabs, TW),
    get(TW, on_top, Tab).

panes(F, Panes:chain) :<-
    "New chain holding my panes, over all tabs"::
    get(F, tabs, TW),
    get(TW, members, Panes).

%       A pane may be a *group*: a pane_stack holding a tab per source,
%       as PceEmacs does.  Everything I ask of a pane -- what it is
%       called, which menu bar it wants, that it has come into view -- is
%       answered by the window inside it that the user is really working
%       in, and so is every `get(Frame, current_pane, View)' in PceEmacs.
%       So <-current_pane looks inside, and <-pane_group is the way back
%       out for the few things that are about the pane my *tab* tiles:
%       putting another pane beside it, fading it, writing it down.
%
%       Looking inside is by asking: only a class that answers
%       <-current_pane is looked into.  A tool is its windows and answers
%       none, and stays the one pane it is.

current_pane(F, Pane:window) :<-
    "The pane the user is working in, inside a group if it is one"::
    get(F, tabs, TW),
    get(TW, current, Outer),
    inner_pane(Outer, Pane).

current_pane(F, Pane:window) :->
    "Make Pane the one the user is working in"::
    pane_group(Pane, Group),
    (   Group == Pane
    ->  true
    ;   send(Group, current, Pane)      % the tab of the group holding it
    ),
    get(F, tabs, TW),
    send(TW, current, Group).

pane_group(_F, Pane:window, Group:window) :<-
    "The pane my tab tiles that holds Pane"::
    pane_group(Pane, Group).

%!  inner_pane(+Outer, -Pane) is det.
%
%   The window the user is working in, inside however many groups.

inner_pane(P0, P) :-
    (   send(P0, has_get_method, current_pane),
        get(P0, current_pane, P1),
        P1 \== @nil,
        P1 \== P0
    ->  inner_pane(P1, P)
    ;   P = P0
    ).

%!  pane_group(+Pane, -Group) is det.
%
%   The other way: climb out of the groups Pane is in until the tab
%   holding it is a tab of a window, which is the one my tabbed window
%   tiles.  A pane that is in no group at all is its own group.

pane_group(Pane, Group) :-
    (   get(Pane, container, tab_frame, Tab),
        \+ send(Tab, instance_of, pane_tab),
        get(Tab, container, tabbed_window, Outer)
    ->  pane_group(Outer, Group)
    ;   Group = Pane
    ).

                 /*******************************
                 *            PANES             *
                 *******************************/

append_pane(F, Pane:window, Label:[name], Expose:[bool]) :->
    "Add Pane in a tab of its own"::
    get(F, tabs, TW),
    send(TW, append, Pane, Label, Expose).

split(F, Pane:window,
         Relative:relative_to=[window],
         Direction:direction=[{horizontally,vertically,
                               above,below,left,right}]) :->
    "Add Pane beside Relative, in the tab Relative is in"::
    (   Relative == @default
    ->  get(F, current_pane, Rel0)
    ;   Rel0 = Relative
    ),
    pane_group(Rel0, Rel),              % beside the group, not inside it
    get(Rel, container, tab_frame, Tab),
    send(Tab, split, Pane, Rel, Direction),
    send(F, keyboard_focus, Pane).

%       A tool that belongs down an edge -- the navigator down the left of
%       the editor and the terminal together -- is put beside a *group* of
%       panes rather than beside the one the user happens to be in, and at
%       a share of the room rather than half of it.  See `tab_frame
%       ->append' for the group and `->window_share' for the share.

split_beside(F, Pane:window,
                Relatives:chain,
                Side:{above,below,left,right},
                Share:[real]) :->
    "Add Pane beside those panes, taking that share of their room"::
    get(Relatives, head, First0),
    pane_group(First0, First),
    get(First, container, tab_frame, Tab),
    get(Tab, window_tree, Was),
    send(Tab, append, Pane, Relatives, Side),
    (   Share == @default
    ->  true
    ;   send(Tab, window_share, Pane, Share, Was)
    ),
    send(F, keyboard_focus, Pane).

                 /*******************************
                 *       ARRANGED BY HAND       *
                 *******************************/

/* Which windows the IDE learns from, and for how long.

A window the IDE placed and the user never touched teaches nothing: the
IDE would only be learning back its own guesses.  So a window starts
saying nothing, and the first time the user moves, splits, resizes,
re-tabs or closes a pane in it by hand it begins to count.

What is counted is the time an arrangement is *lived in*, because putting
a window right takes several steps -- merge the tab back in, drag the pane
across, pull it to the width it should have -- and only the state that is
then worked in means anything.  Each of those steps closes off the one
before it; the steps themselves are over in moments and are thrown away
by library(pane_layouts), which credits nothing that barely lasted.
*/

arranged(F) :->
    "Note that the user has just arranged my panes"::
    (   get(F, arranged, @on)
    ->  send(F, record_arrangement)     % what was there until now
    ;   send(F, slot, arranged, @on)
    ),
    start_arrangement(F).

start_arrangement(F) :-
    (   frame_arrangement(F, Arrangement)
    ->  get_time(Now),
        send(F, slot, arrangement, Arrangement),
        send(F, slot, arranged_since, Now)
    ;   true
    ).

frame_arrangement(F, Arrangement) :-
    get(F, pane_term, Term),
    arrangement_of(Term, Arrangement).

%       What the user asks the Settings menu to keep.  Not the
%       arrangement <-arranged has been timing -- that is the one I was in
%       when they last moved something -- but the one I am in now.

remember_arrangement(F) :->
    "Keep the way I am arranged for windows holding these panes"::
    frame_arrangement(F, Arrangement),
    remember_arrangement(Arrangement).

record_arrangement(F) :->
    "Credit the arrangement I am in with the time it has been"::
    get(F, arranged, @on),
    get(F, slot, arrangement, Arrangement),
    Arrangement \== none,
    get(F, slot, arranged_since, Since),
    Since \== none,
    get_time(Now),
    Seconds is Now-Since,
    send(F, slot, arranged_since, Now),
    record_arrangement(Arrangement, Seconds).

%!  pane_arranged(+Pane) is det.
%
%   The user has just moved Pane by hand.  Called by the gestures of the
%   `pane' template; the plumbing they use is not marked, because a
%   program placing a pane goes through the same plumbing.

pane_arranged(Pane) :-
    (   get(Pane, pane_frame, Frame)
    ->  ignore(send(Frame, arranged))
    ;   true
    ).

%       A pane inside a group is taken out of the group: it is the group
%       my tab tiles, and the group looks after itself when its last
%       window goes -- see `pane_stack ->empty'.  Only the last *group*
%       is the last of me.

delete_pane(F, Pane:window, Destroy:[bool]) :->
    "Take Pane out of its tab; destroy me if it was my last"::
    (   last_pane(F, Pane)
    ->  send(F, empty)
    ;   get(Pane, container, tab_frame, Tab),
        send(Tab, delete, Pane),
        (   Destroy == @on
        ->  send(Pane, destroy)
        ;   true
        ),
        send(F, pane_changed)
    ).

%!  last_pane(+Frame, +Pane) is semidet.
%
%   True when taking Pane away would leave Frame with nothing.  A pane
%   inside a group is never the last: the group looks after itself when
%   its last window goes -- see `pane_stack ->empty' -- and asks me to
%   take *it* away, which is when the count applies.

last_pane(F, Pane) :-
    pane_group(Pane, Pane),
    get(F, panes, Panes),
    get(Panes, size, 1).

new_pane(F, Kind:[name]) :->
    "Add a pane of the kind my application makes"::
    get(F, application, App),
    App \== @nil,
    send(App, has_send_method, new_pane),
    send(App, new_pane, F, Kind).

empty(F) :->
    "My last pane is gone"::
    (   get(F, application, App),
        App \== @nil,
        send(App, has_send_method, frame_empty)
    ->  send(App, frame_empty, F)
    ;   send(F, destroy)
    ).

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
        ->  ignore(send(F, record_arrangement)),
            send(F, destroy)
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
    ignore(send(F, clear_status)),      % what the pane before had to say
    ignore(send(F, update_menu_bar)),   % is not about this one
    ignore(send(F, update_tab_label)),
    ignore(send(F, update_label)),
    ignore(send(F, update_opacity)),
    (   send(Pane, has_send_method, pane_exposed)
    ->  ignore(send(Pane, pane_exposed))
    ;   true
    ).

input_focus(F, Val:bool) :->
    "The window manager gave me the focus, or took it away"::
    send_super(F, input_focus, Val),
    (   Val == @on,
        \+ send(F, unlinking),
        get(F, application, App),
        App \== @nil
    ->  ignore(send(App, first, F))     % the most recently worked in
    ;   true
    ),
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

%       A pane tells me the pointer entered it and I decide whether that
%       is enough to give it the focus.  PceEmacs used to do this itself,
%       unconditionally; a terminal never did.  One class variable now says
%       which it is for every pane of every window.

focus_on_enter(F, Pane:window) :->
    "Give Pane the focus, if the pointer entering one is enough"::
    get(F, class_variable_value, focus_on_enter, @on),
    \+ modal_transient(F),
    send(F, keyboard_focus, Pane).

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
        ignore(send(MD, clear_tool_bar)),
        ignore(send(F, fill_menu_bar, MD)),
        (   get(F, current_pane, Pane),
            send(Pane, has_send_method, fill_menu_bar)
        ->  ignore(send(Pane, fill_menu_bar, MD))
        ;   true
        ),
        send(F?menu_extensions, for_all, message(@arg1, forward, MD)),
        ignore(send(MD, lay_out_bars))
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

%       Four say what a window is called, the earlier overruling the
%       later: ->label_format, for one window on its own; <-title_format
%       of the pane the user is working in, so that an editor may still
%       call its window something else; the application; and the class
%       variable, so that a user can say in their Defaults what all of
%       them are called.  The pane is asked for <-title_format rather
%       than <-label_format: the latter is text alignment on anything
%       descended from dialog_item, which a pane may well be.
%
%       It is asked at every ->update_label rather than settled once, or
%       a window would keep the name of whichever tool happened to make
%       it however its panes changed afterwards.

label_format(F, Fmt:'[name]*') :->
    "Set the format my label is made with"::
    send(F, slot, own_label_format, Fmt),
    ignore(send(F, update_label)).

label_format(F, Fmt:'name*') :<-
    "The format my label is made with"::
    (   get(F, slot, own_label_format, Fmt0),
        Fmt0 \== @default
    ->  Fmt = Fmt0
    ;   get(F, current_pane, Pane),
        send(Pane, has_get_method, title_format),
        get(Pane, title_format, Fmt1)
    ->  Fmt = Fmt1
    ;   get(F, application, App),
        App \== @nil,
        send(App, has_get_method, label_format),
        get(App, label_format, Fmt2)
    ->  Fmt = Fmt2
    ;   get(F, class_variable_value, label_format, Fmt)
    ).

tab_label(F, Label:name) :<-
    "The label of the tab in view"::
    get(F, tab, Tab),
    get(Tab, label, Label).

%       A tab is named after the pane the user is working in, so that a
%       tab holding two of them says which.  A tab the user has renamed by
%       hand keeps the name they gave it.

update_tab_label(F) :->
    "Put the label of the current pane on its tab"::
    get(F, current_pane, Pane),
    send(Pane, has_get_method, pane_label),
    get(Pane, pane_label, Label),
    get(F, tab, Tab),
    get(Tab, renamed, @off),
    send(Tab, label, Label).

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
    get(F, current_pane, Current0),
    pane_group(Current0, Current),      % my tab tiles groups, not views
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
    (   get(F, ensure_status_dialog, SD)
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

%       The bar says what the pane in view has to say, and a pane that
%       comes into view has said nothing yet: the message the pane before
%       left is not about this one.  A pane that has something to put back
%       -- an editor says which line the caret is on -- does it from
%       ->pane_exposed, which runs after this.

clear_status(F) :->
    "Take away what the pane before had to say"::
    (   get(F, status_dialog, SD)
    ->  send(SD, clear)
    ;   true                            % no bar: nothing to take away
    ).

show_line_number(F, Line:'int|{too_expensive}*') :->
    "Show the line the caret is on"::
    (   Line == @nil                    % nothing to say: do not grow a bar
    ->  (   get(F, status_dialog, SD)
        ->  send(SD, show_line_number, @nil)
        ;   true
        )
    ;   get(F, ensure_status_dialog, SD)
    ->  send(SD, show_line_number, Line)
    ;   true
    ).

                 /*******************************
                 *          PANE TERM           *
                 *******************************/

/* What I hold, and how it is laid out, as a Prolog term.

    pane_frame(FrameOptions, [Tab, ...])
    Tab      := tab(TabOptions, Content)
    Content  := Pane | horizontal([Share-Content, ...])
                     | vertical([Share-Content, ...])
    Pane     := Kind | Kind(PaneOptions) | current(Pane)

A pane is written by its <-pane_kind, which is its class name unless it
says otherwise, and by the <-pane_term it answers -- a tool that has
nothing to say about itself is simply its name.  Shares are relative:
[2-A, 1-B] and [0.667-A, 0.333-B] say the same thing.  Everything is
optional, so `pane_frame([], [tab([], epilog_window)])' is a complete
description and a term can be written by hand.

Reading a frame never fails.  Building one restores what can be restored
and reports the rest: a source that has gone, a tool whose class will not
load, a profile that is no longer defined.
*/

pane_term(F, Term:prolog) :<-
    "The term that says what I hold and how it is laid out"::
    settle_room(F),                     % see settle_room/1
    frame_term_options(F, Options),
    get(F, tabs, TW),
    get(TW, tabs, Chain),
    chain_list(Chain, Tabs),
    maplist(tab_term, Tabs, TabTerms),
    Term = pane_frame(Options, TabTerms).

pane_term(F, Term:prolog) :->
    "Hold what Term says, in place of what I hold now"::
    Term = pane_frame(Options, Tabs),
    get(F, can_close, @on),             % refused: leave me as I was
    get(F, tabs, TW),
    get(TW, tabs, Chain),
    chain_list(Chain, Old),
    build_tabs(F, Tabs, Built),
    Built \== [],                       % nothing came of it: leave me as
    forall(member(Tab, Old),            % I was rather than empty
           send(Tab, close)),
    apply_frame_options(F, Options),
    expose_current_tab(F, Built),
    settle_room(F),                     % see settle_room/1
    share_room(Built).

%!  settle_room(+Frame) is det.
%
%   Hand the panes the room the frame has, so that there is room to
%   divide and shares to read off.  A window that has not been opened
%   has none: it has only been fitted -- every pane laid out at the
%   size it asks for, which in a tab is the least it will take -- and
%   shares of that are not shares of anything.  The tab has exactly the
%   room its panes insist on, no arrangement of it is possible, and the
%   term it gives back is neither the one it was built from nor the one
%   the same window gives once it is opened.  ->resize is what the
%   window system sends when a window is given its size; sending it
%   here lays the panes out in the frame as it stands.  An open window
%   was given its room when it opened and is left alone: laying it out
%   again from here puts its panes where they were before its bars took
%   theirs.
%
%   Both directions of <-/->pane_term settle the room: the shares one
%   writes are pixels of it, and the shares the other reads are pixels
%   of it too, so reading a window that was built but never opened has
%   to divide the same room the building did.

settle_room(F) :-
    (   get(F, status, unmapped)
    ->  send(F, resize)
    ;   true
    ).

%!  share_room(+Built) is det.
%
%   Give the windows of every tab their share of it, once all the
%   rest is in place.  A share is worked out in pixels of the room
%   there is, and `tab_frame ->layout_natural' takes the ideal sizes
%   back off the windows at every layout, so this has to be the last
%   thing done.

share_room(Built) :-
    forall(member(built_tab(Tab, _, Tree), Built),
           send(Tab, window_shares, Tree)).

%!  frame_term_options(+Frame, -Options) is det.
%
%   What is worth saying about the window itself.  The menu bar and the
%   label are left out: both are rebuilt from the pane in view at every
%   ->pane_changed, so neither is state.

frame_term_options(F, Options) :-
    findall(O, frame_term_option(F, O), Options).

frame_term_option(F, name(Name)) :-
    get(F, name, Name),
    \+ gensym_name(Name).
frame_term_option(F, main(true)) :-
    get(F, attribute, main, @on).
frame_term_option(F, label_format(Format)) :-
    get(F, slot, own_label_format, Fmt),
    Fmt \== @default,
    (   Fmt == @nil
    ->  Format = none
    ;   Format = Fmt
    ).
frame_term_option(F, status_bar(true)) :-
    get(F, status_dialog, _).
frame_term_option(F, geometry(Geometry)) :-
    \+ get(F, status, unmapped),        % a window that was never opened
    get(F, geometry, Geometry).         % answers 0x0+0+0

%!  gensym_name(+Name) is semidet.
%
%   True for the name `name_frame/1' gives a window that was not named by
%   its caller.  Writing it down would be noise, and reading it back
%   would clash with the window that has it now.

gensym_name(Name) :-
    atom_concat(pane_frame, Rest, Name),
    atom_number(Rest, _).

apply_frame_options(F, Options) :-
    forall(member(Option, Options),
           ignore(apply_frame_option(F, Option))).

apply_frame_option(F, name(Name)) :-
    (   free_frame_name(F, Name)
    ->  send(F, name, Name)
    ;   get(F, name, Now),
        print_message(informational, pane_frame(name_taken(Name, Now)))
    ).
apply_frame_option(F, main(true)) :-
    (   main_frame(F)
    ->  print_message(informational, pane_frame(main_frame_exists))
    ;   send(F, attribute, main, @on)
    ).
apply_frame_option(F, label_format(none)) :-
    !,
    send(F, label_format, @nil).
apply_frame_option(F, label_format(Format)) :-
    send(F, label_format, Format).
apply_frame_option(F, status_bar(true)) :-
    get(F, ensure_status_dialog, _).
apply_frame_option(F, geometry(Geometry)) :-
    send(F, geometry, Geometry).

%!  main_frame(+Frame) is semidet.
%
%   True when another window of the same application is the main one.

main_frame(F) :-
    get(F, application, App),
    App \== @nil,
    get(App, members, Members),
    chain_list(Members, List),
    member(Other, List),
    Other \== F,
    get(Other, attribute, main, @on).

free_frame_name(F, Name) :-
    (   get(F, application, App),
        App \== @nil
    ->  \+ ( get(App, member, Name, Other),
             Other \== F
           )
    ;   true
    ).

                 /*******************************
                 *             TABS             *
                 *******************************/

%!  tab_term(+Tab, -Term) is det.
%
%   A tab is its panes and what it is called.  The label is written only
%   when it is not the one its pane would give it anyway, and `renamed'
%   with it when the user typed it: without that, ->update_tab_label
%   takes a hand-typed name straight back off again.

tab_term(Tab, tab(Options, Content)) :-
    findall(O, tab_term_option(Tab, O), Options),
    get(Tab, window_tree, Tree),
    tab_current(Tab, Current),
    tree_term(Tree, Current, Content).

%!  tab_current(+Tab, -Current) is det.
%
%   The pane of a tab that has the keyboard, or @nil when saying so
%   would mean nothing: a tab holding one pane has no choice.

tab_current(Tab, Current) :-
    get(Tab, windows, Windows),
    get(Windows, size, Size),
    (   Size > 1
    ->  get(Tab, current, Current)
    ;   Current = @nil
    ).

tab_term_option(Tab, label(Label)) :-
    get(Tab, label, Label),
    (   get(Tab, renamed, @on)
    ->  true
    ;   get(Tab, current, Window),
        Window \== @nil,
        \+ ( send(Window, has_get_method, pane_label),
             get(Window, pane_label, Label)
           )
    ).
tab_term_option(Tab, renamed(true)) :-
    get(Tab, renamed, @on).
tab_term_option(Tab, current(true)) :-
    get(Tab, status, on_top).

%!  tree_term(+Tree, +Current, -Content) is det.
%
%   Turn the window tree of a tab into the term for it: `tab_frame
%   <-window_tree' answers a tree of windows, and each of them is written
%   as the pane term for it.

tree_term(Tree, Current, Content) :-
    pane_tree_term(pane_term_of, Tree, Current, Content).

%!  pane_tree_term(:Leaf, +Tree, +Current, -Content) is det.
%
%   The same over any tab that tiles windows, with Leaf saying how one
%   window is written down: call(Leaf, Window, Term).  A group of panes
%   with tabs of its own uses this to write what it holds -- see
%   `emacs_pane <-pane_term' in library(emacs/window).

pane_tree_term(Leaf, Window, Current, Term) :-
    object(Window),
    !,
    call(Leaf, Window, Term0),
    (   Window == Current
    ->  Term = current(Term0)
    ;   Term = Term0
    ).
pane_tree_term(Leaf, Node, Current, Term) :-
    Node =.. [Orientation, Shares],
    maplist(share_term(Leaf, Current), Shares, Terms),
    Term =.. [Orientation, Terms].

share_term(Leaf, Current, Share-Content, Share-Term) :-
    pane_tree_term(Leaf, Content, Current, Term).

%!  pane_term_of(+Pane, -Term) is det.
%
%   How a pane is written down.  Both halves of the protocol are
%   optional: a pane that answers neither is its class name.

pane_term_of(Pane, Term) :-
    pane_kind(Pane, Kind),
    (   send(Pane, has_get_method, pane_term),
        get(Pane, pane_term, Options),
        Options \== []
    ->  Term =.. [Kind, Options]
    ;   Term = Kind
    ).

%!  pane_kind(+Pane, -Kind) is det.
%
%   What a pane is called in a description of a window: what it says of
%   itself, and otherwise its class name.

pane_kind(Pane, Kind) :-
    (   send(Pane, has_get_method, pane_kind)
    ->  get(Pane, pane_kind, Kind)
    ;   get(Pane, class_name, Kind)
    ).

                 /*******************************
                 *           BUILDING           *
                 *******************************/

%!  build_tabs(+Frame, +Tabs, -Built) is det.
%
%   Add a tab for each term that has anything in it.  A tab whose panes
%   could none of them be made is left out rather than added empty.

build_tabs(_, [], []).
build_tabs(F, [Term|Terms], Built) :-
    build_tab(F, Term, Built, Rest),
    build_tabs(F, Terms, Rest).

build_tab(F, tab(Options, Content), Built, Rest) :-
    !,
    (   build_content(F, Content, Tree, First, Current),
        add_tab(F, Options, Tree, First, Current, Tab)
    ->  Built = [built_tab(Tab, Options, Tree)|Rest]
    ;   print_message(warning, pane_frame(empty_tab(Options))),
        Built = Rest
    ).
build_tab(_, Term, Rest, Rest) :-
    print_message(warning, pane_frame(not_a_tab(Term))).

add_tab(F, Options, Tree, First, Current, Tab) :-
    tab_label_option(Options, Label),
    send(F, append_pane, First, Label, @off),
    get(First, container, tab_frame, Tab),
    send(Tab, window_tree, Tree),
    apply_tab_options(Tab, Options),
    (   Current == @default
    ->  true
    ;   send(Tab, current, Current)
    ).

tab_label_option(Options, Label) :-
    (   memberchk(label(Label0), Options)
    ->  Label = Label0
    ;   Label = @default
    ).

apply_tab_options(Tab, Options) :-
    (   memberchk(label(Label), Options),
        memberchk(renamed(true), Options)
    ->  send(Tab, rename, Label)
    ;   true
    ).

%!  expose_current_tab(+Frame, +Built) is det.
%
%   Bring the tab the term marked up, with its own pane in view.  One
%   ->current_pane does both: it raises the tab the pane is in and tells
%   me to rebuild my menu bar and my label around it.

expose_current_tab(F, Built) :-
    (   member(built_tab(Tab, Options, _), Built),
        memberchk(current(true), Options)
    ->  true
    ;   Built = [built_tab(Tab, _, _)|_]
    ),
    get(Tab, current, Pane),
    Pane \== @nil,
    !,
    send(F, current_pane, Pane).
expose_current_tab(_, _).

%!  build_content(+Frame, +Content, -Tree, -First, -Current) is semidet.
%
%   Make the panes of one tab and give back the tree `tab_frame
%   ->window_tree' takes, the pane the tab is opened on and the pane that
%   is to have the keyboard.  A pane that cannot be made is dropped and a
%   split left holding one pane collapses onto it, so a term naming a
%   source that has gone still restores everything else.

build_content(F, Content, Tree, First, Current) :-
    build_pane_tree(build_pane(F), Content, Tree, First, Current).

%!  build_pane_tree(:MakeLeaf, +Content, -Tree, -First, -Current) is semidet.
%
%   The same over any tab that tiles windows, with MakeLeaf saying how one
%   window is made: call(MakeLeaf, Term, Window), which may fail.  See
%   `emacs_pane ->pane_term' in library(emacs/window).

build_pane_tree(Make, current(Term), Window, Window, Window) :-
    !,
    build_pane_tree(Make, Term, Window, Window, _).
build_pane_tree(Make, Node, Tree, First, Current) :-
    Node =.. [Orientation, Shares],
    split_orientation(Orientation),
    !,
    build_shares(Make, Shares, Built),
    Built \== [],
    (   Built = [built(_, Tree, First, Current)]   % one left: no split
    ->  true
    ;   maplist(built_share, Built, Subs),
        Tree =.. [Orientation, Subs],
        Built = [built(_, _, First, _)|_],
        (   member(built(_, _, _, Current0), Built),
            Current0 \== @default
        ->  Current = Current0
        ;   Current = @default
        )
    ).
build_pane_tree(Make, Term, Window, Window, @default) :-
    call(Make, Term, Window).

build_shares(_, [], []).
build_shares(Make, [Share|Shares], Built) :-
    share_parts(Share, Weight, Content),
    (   build_pane_tree(Make, Content, Tree, First, Current)
    ->  Built = [built(Weight, Tree, First, Current)|Rest]
    ;   Built = Rest
    ),
    build_shares(Make, Shares, Rest).

built_share(built(Weight, Tree, _, _), Weight-Tree).

share_parts(Weight-Content, Weight, Content) :-
    number(Weight),
    !.
share_parts(Content, 1, Content).

split_orientation(horizontal).
split_orientation(vertical).

%!  build_pane(+Frame, +Term, -Pane) is semidet.
%
%   Make one pane and tell it what it is to hold.  A class is created
%   with no arguments at all -- every pane that can be restored has an
%   ->initialise that takes none, and one that insists on arguments is
%   bound to something live that a term cannot bring back.

build_pane(F, Term, Pane) :-
    kind_options(Term, Kind, Options),
    pane_class(F, Kind, Class),
    (   is_class(Class)
    ->  true
    ;   print_message(warning, pane_frame(no_class(Kind))),
        fail
    ),
    (   creatable(Class)
    ->  true
    ;   print_message(warning, pane_frame(needs_arguments(Kind))),
        fail
    ),
    catch(new(Pane0, Class), E,
          ( print_message(warning, pane_frame(pane_failed(Kind, E))),
            fail
          )),
    (   send(Pane0, instance_of, window)
    ->  Pane = Pane0
    ;   send(Pane0, destroy),
        print_message(warning, pane_frame(not_a_pane(Kind))),
        fail
    ),
    (   Options == []
    ->  true
    ;   send(Pane, has_send_method, pane_term)
    ->  (   catch(send(Pane, pane_term, Options), E2,
                  ( print_message(warning,
                                  pane_frame(pane_failed(Kind, E2))),
                    fail
                  ))
        ->  true
        ;   print_message(warning, pane_frame(pane_failed(Kind, failed)))
        )
    ;   print_message(warning, pane_frame(no_options(Kind, Options)))
    ).

kind_options(Term, Kind, Options) :-
    compound(Term),
    Term =.. [Kind, Options],
    is_list(Options),
    !.
kind_options(Kind, Kind, []) :-
    atom(Kind).

%!  pane_class(+Frame, +Kind, -Class) is semidet.
%
%   The class that makes a pane of that kind.  An application that knows
%   its tools says so -- `prolog_ide <-pane_class' loads PceEmacs for an
%   editor and Epilog for a terminal -- and anything else is a class name
%   that pce_autoload/2 can find.

pane_class(F, Kind, Class) :-
    (   get(F, application, App),
        App \== @nil,
        send(App, has_get_method, pane_class),
        catch(get(App, pane_class, Kind, Class0), _, fail)
    ->  Class = Class0                  % the library of a tool that is
    ;   Class = Kind                    % not in this build will not load
    ).

%!  is_class(+Name) is semidet.
%
%   True when Name names an XPCE class, loading it if pce_autoload/2 knows
%   where it lives.

is_class(Name) :-
    catch(get(@pce, convert, Name, class, _), _, fail).

%!  creatable(+Class) is semidet.
%
%   True when `new(X, Class)' can be done: every argument of its
%   ->initialise takes @default.  A class that insists on one is made for
%   something live -- the debugger for a break level and a thread -- and
%   is not something a term can bring back.

creatable(Class) :-
    catch(get(@pce, convert, Class, class, TheClass), _, fail),
    (   get(TheClass, send_method, initialise, Method)
    ->  get(Method, types, Types),
        get(Types, size, Size),
        forall(between(1, Size, I),
               ( get(Types, element, I, Type),
                 send(Type, validate, @default)
               ))
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

%       The bar is built into a dialog that was laid out long ago, and a
%       bar built afterwards is not placed until the dialog lays itself
%       out again: the tool bar `tool_dialog <-tool_bar' puts below the
%       menu bar sat in the corner the menus are in, drawn over them.
%       ->height asks my tile for the room -- see requestGeometryWindow()
%       in src/win/window.c -- which is what makes the strip grow for a
%       second row and shrink back when there is nothing on it.

lay_out_bars(MD) :->
    "Place my bars and take the room they need"::
    send(MD, layout),
    send(MD, place_bars),
    get(MD, border, size(_, BH)),
    get(MD, graphicals, Chain),
    chain_list(Chain, Bars),
    findall(Bottom,
            ( member(Bar, Bars),
              get(Bar, displayed, @on),
              get(Bar, area, area(_, Y, _, H)),
              Bottom is Y+H
            ),
            Bottoms),
    Bottoms \== [],
    max_list(Bottoms, Deepest),
    Height is Deepest+2*BH,
    (   get(MD, height, Height)
    ->  true
    ;   send(MD, height, Height)
    ).

%       The row runs menus at the left and buttons at the right, both
%       against the top.  The dialog lays its items out one after the
%       other and lines them up on their baselines, which puts the
%       buttons hard against the last menu and the menus low in the row.
%       ->layout_dialog runs whenever the strip is laid out afresh -- on
%       a resize as well as on a rebuild -- so this holds.

layout_dialog(MD, Gap:[size], Size:[size], Border:[size]) :->
    "Lay my bars out, then put them where they belong"::
    send_super(MD, layout_dialog, Gap, Size, Border),
    send(MD, place_bars).

resize(MD) :->
    "The right edge moved; the buttons go with it"::
    send_super(MD, resize),
    send(MD, place_bars).

place_bars(MD) :->
    "Menus at the left, buttons at the right, both at the top"::
    (   get(MD, member, menu_bar, MB),
        get(MB, displayed, @on)
    ->  send(MB, set, 0, 0)
    ;   true
    ),
    (   get(MD, member, tool_bar, TB),
        get(TB, displayed, @on)
    ->  get(MD, width, Width),
        get(TB, width, BW),
        X is max(0, Width-BW),
        send(TB, set, X, 0)
    ;   true
    ).

menu_bar(MD, Create:[bool], MB:menu_bar) :<-
    "Get (or create) the menu bar"::
    (   get(MD, member, menu_bar, MB)
    ->  true
    ;   Create == @on
    ->  (   get(MD, tool_bar, TB)
        ->  send(new(MB, pane_menu_bar), left, TB)
        ;   send_super(MD, append, new(MB, pane_menu_bar))
        )
    ).

%       The buttons of the pane in view go at the right of the row the
%       menus are in, not on a row of their own: two of them are not
%       worth a strip across the window.  ->place_bars is what puts them
%       against the right edge; appending them here only says they share
%       the row with the menus.

tool_bar(MD, Create:[bool], TB:tool_bar) :<-
    "Get (or create) the tool bar, at the right of the menus"::
    (   get(MD, member, tool_bar, TB)
    ->  true
    ;   Create == @on
    ->  (   get(MD, client, Client),
            Client \== @default
        ->  true
        ;   get(MD, frame, Client)
        ),
        get(MD, menu_bar, @on, MB),
        send(new(TB, tool_bar(Client)), right, MB)
    ).

%       What is on the tool bar belongs to the pane in view, as the menus
%       do, so it is taken away and put back at every ->update_menu_bar
%       and a pane that has nothing to put there leaves the row to the
%       menus.  The bar is hidden rather than emptied: the buttons are
%       made once and kept, and destroying them here would take the
%       keyboard focus with them -- ->erase on a window clears its
%       <-keyboard_focus and its <-focus (see eraseWindow() in
%       src/win/window.c), and ->update_menu_bar runs in the middle of
%       ->pane_changed, which is where the focus is being handed over.

clear_tool_bar(MD) :->
    "Take my tool bar away until the pane in view asks for it"::
    (   get(MD, member, tool_bar, TB)
    ->  send(TB, displayed, @off)
    ;   true
    ).

%       A popup on my bar is a pane_popup: the bar is assembled from two
%       sides, so a menu one of them makes must be one the other can add
%       to.  Class tool_dialog would make a plain popup.

popup(MD, Name:name, Create:[bool], Popup:pane_popup) :<-
    "Find the named popup or create it"::
    get(MD, menu_bar, Create, MB),
    (   get(MB, member, Name, Popup)
    ->  true
    ;   Create == @on
    ->  send(MB, append, new(Popup, pane_popup(Name))),
        send(Popup, message, message(MD, action, @arg1))
    ).

assign_accelerators(_) :->
    "Accelerators are defined by the panes"::
    true.

:- pce_end_class(pane_menu_dialog).


:- pce_begin_class(pane_menu_bar, menu_bar,
                   "Menu bar of a pane_frame").

%       The bar is assembled from two sides -- the application first,
%       then the pane in view, then whatever `->extend_menu_bar' added --
%       and neither can know what the other will put on.  So the bar says
%       where a menu goes rather than the order in which it happens to
%       arrive: `->append' looks the name up in <-menu_order and passes
%       the menu it must come before to the super.  A name that is not in
%       the list takes the place of `*': that is a mode's own menu
%       (prolog, sgml, LaTeX, ...) or a tool pane's (xref, threads, ...).

class_variable(menu_order, chain,
               chain(file, settings, tools, debug, 'GUI',
                     edit, browse, compile, '*', help),
               "Order the pulldown menus appear in").

initialise(MB) :->
    "Create empty, under the name my dialog looks me up by"::
    send_super(MB, initialise),
    send(MB, name, menu_bar).

rank(MB, Name:name, Rank:int) :<-
    "Where a menu of this name belongs on me"::
    menu_order(MB, Order),
    (   nth1(Rank, Order, Name)
    ->  true
    ;   nth1(Rank, Order, '*')
    ).

%!  menu_order(+MenuBar, -Order) is semidet.
%
%   The names in <-menu_order, as a list.  A name written between quotes
%   in a Defaults file -- and 'GUI' and '*' have to be -- is read as a
%   string rather than as a name, so the elements are converted rather
%   than compared as they come.

menu_order(MB, Order) :-
    get(MB, class_variable_value, menu_order, Chain),
    chain_list(Chain, List),
    maplist(element_name, List, Order).

element_name(Elem, Name) :-
    get(@pce, convert, Elem, name, Name).

%       Strictly greater: two menus that both take the place of `*' keep
%       the order in which they were appended.

before(MB, Name:name, Before:name) :<-
    "The menu on me the named one must come before"::
    get(MB, rank, Name, Rank),
    get(MB, members, Chain),
    chain_list(Chain, Popups),
    member(Popup, Popups),
    get(Popup, name, Before),
    get(MB, rank, Before, OtherRank),
    OtherRank > Rank,
    !.

%       A caller who names the menu to come before knows better, and so
%       does one who says `right': that means "after them all", which is
%       what win_insert_menu/2 promises for a menu added with `-'.  Both
%       go through unranked.

append(MB, Popup:'member=popup', Alignment:'[{left,right}]',
           Before:'[name|popup]') :->
    "Append a popup at the place its name asks for"::
    (   Before \== @default
    ->  send_super(MB, append, Popup, Alignment, Before)
    ;   Alignment == right
    ->  send_super(MB, append, Popup, Alignment)
    ;   get(MB, before, Popup?name, TheBefore)
    ->  send_super(MB, append, Popup, Alignment, TheBefore)
    ;   send_super(MB, append, Popup, Alignment)
    ).

assign_accelerators(_) :->
    "Accelerators are defined by the panes"::
    true.

:- pce_end_class(pane_menu_bar).


:- pce_begin_class(pane_popup, popup,
                   "Popup of a pane_frame menu bar").

class_variable(accelerator_font, font, small,
               "Font the accelerator is written in").

%       The items of a menu arrive from two sides, as the menus of the
%       bar do: the application puts `close window' and `quit' on the
%       File menu, and the pane in view puts what it can do with a file
%       there afterwards.  So the menu says where an item goes rather
%       than the order it happens to arrive in -- `->append' looks the
%       name up in <-item_order and hands the super the item it must come
%       before.  Only the ones that belong at the end need naming: every
%       other name takes the place of `*' and keeps the order it came in.

class_variable(item_order, chain,
               chain('*', close_window, halt_prolog),
               "Order the items of a menu appear in").

rank(P, Name:name, Rank:int) :<-
    "Where an item of this name belongs on me"::
    item_order(P, Order),
    (   nth1(Rank, Order, Name)
    ->  true
    ;   nth1(Rank, Order, '*')
    ).

%!  item_order(+Popup, -Order) is semidet.
%
%   The names in <-item_order, as a list.  As with `pane_menu_bar
%   <-menu_order', `*' has to be written between quotes in a Defaults
%   file and comes back as a string, so the elements are converted.

item_order(P, Order) :-
    get(P, class_variable_value, item_order, Chain),
    chain_list(Chain, List),
    maplist(element_name, List, Order).

before(P, Name:name, Before:menu_item) :<-
    "The item on me the named one must come before"::
    get(P, rank, Name, Rank),
    get(P, members, Chain),
    chain_list(Chain, Items),
    member(Before, Items),
    get(Before, value, ItsName),
    get(P, rank, ItsName, ItsRank),
    ItsRank > Rank,
    !.

append(P, Item:'menu_item|{gap}') :->
    "Append an item at the place its name asks for"::
    (   Item == gap
    ->  (   get(P, last_free_item, MI)
        ->  send(MI, end_group, @on)
        ;   true
        )
    ;   get(P, before, Item?value, Before)
    ->  send(P, insert_before, Item, Before)
    ;   send_super(P, append, Item)
    ).

%       `tool_dialog ->append' puts an item on a named menu by naming the
%       item it must come before -- see `prolog_terminal ->fill_menu_bar'
%       -- and that item belongs to another pane as often as not.  The
%       super appends at the end when it cannot find the name, which is
%       how what a pane adds ends up below `close window'; ask for the
%       place the name deserves instead.

insert_before(P, Item:menu_item, Before:[name|menu_item]) :->
    "Insert before the named item, or where <-item_order says"::
    (   Before \== @default,
        get(P, member, Before, Other)
    ->  send_super(P, insert_before, Item, Other)
    ;   send(P, append, Item)
    ).

%       A gap ends the group of the item it follows, and that is the last
%       item appended rather than the last item on the menu: the ones the
%       application put at the end are ranked past it.

last_free_item(P, MI:menu_item) :<-
    "The item a gap appended now would follow"::
    get(P, members, Chain),
    chain_list(Chain, Items),
    free_items(P, Items, Free),
    last(Free, MI).

%!  free_items(+Popup, +Items, -Free) is det.
%
%   The leading Items that no rank of their own puts at the end.  The
%   items sit in rank order, so this is a prefix of the list.

free_items(P, Items, Free) :-
    get(P, rank, '*', FreeRank),
    free_items_(Items, P, FreeRank, Free).

free_items_([], _, _, []).
free_items_([MI|T], P, FreeRank, Free) :-
    get(MI, value, Name),
    get(P, rank, Name, Rank),
    (   Rank =< FreeRank
    ->  Free = [MI|Free1],
        free_items_(T, P, FreeRank, Free1)
    ;   Free = []
    ).

%       A line separates what the application put at the end from the
%       items above it.  This waits until the menu opens: everybody who
%       fills the bar has had a turn by then, so the item the line goes
%       under is the one it will still be under when the menu is drawn.
%
%       The tick a command is left wearing goes at the same moment.  A
%       menu that holds settings as well as commands takes a set of them
%       -- <-multiple_selection is @on, so that any number can be on at
%       once -- and `popup ->execute' toggles whatever was picked in such
%       a menu, which leaves a tick beside a command that nothing ever
%       takes off again.  What says an item is a setting is its
%       <-condition: that is what the super runs to say whether the
%       setting is on.  An item without one is a command and wears no
%       tick.

update(P, Context:any) :->
    "Close the group above the items ranked at the end"::
    (   get(P, last_free_item, MI),
        get(P?members, tail, Tail),
        Tail \== MI
    ->  send(MI, end_group, @on)
    ;   true
    ),
    send(P, untick_commands),
    send_super(P, update, Context).

untick_commands(P) :->
    "Take the tick off every item of mine that is a command"::
    get(P, members, Chain),
    chain_list(Chain, Items),
    forall(( member(Item, Items),
             get(Item, condition, @nil)
           ),
           send(Item, selected, @off)).

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

variable(renamed, bool := @off, get,
         "The user typed my label; it is not the pane's to set").

label_edited(Tab, Label:name) :->
    "Take the label typed into the editor, and keep it"::
    send(Tab, slot, renamed, @on),
    send_super(Tab, label_edited, Label).

rename(Tab, Label:name) :->
    "Give me a label of my own, as if the user had typed it"::
    send(Tab, slot, renamed, @on),
    send(Tab, label, Label).

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
    ->  pane_frame_closed_tab(Frame),
        ignore(send(Frame, arranged))   % closing a tab by hand arranges too
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

%       A pane shares the window with the other panes, and the gaps
%       between them are dragged to redistribute the space.  A window that
%       sizes itself to what it holds -- class dialog does -- says it can
%       neither give nor take, and then `tile <-can_resize' answers @off
%       and the gap beside it cannot be dragged at all.  Docking one makes
%       it as willing as the panes it lands among; a pane that says how
%       much it wants to give is left alone.  It has to be said before the
%       tab lays itself out: `tile <-can_resize' is worked out once and
%       kept until the hierarchy changes again.

attach_window(Tab, Window:window) :->
    "Take a pane in, and let it be resized"::
    ignore(pane_resizable(Window)),     % before the layout: `tile
    send_super(Tab, attach_window, Window).

pane_resizable(Window) :-
    (   get(Window, decoration, Decor),
        Decor \== @nil
    ->  Placed = Decor
    ;   Placed = Window
    ),
    get(Placed, tile, Tile),
    Tile \== @nil,
    forall(member(Attribute, [hor_stretch, hor_shrink,
                              ver_stretch, ver_shrink]),
           (   get(Tile, Attribute, Old),
               Old > 0
           ->  true
           ;   send(Tile, Attribute, 100)
           )).

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

/* The bar at the bottom of a pane_frame.

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

clear(D) :->
    "Take away whatever is on me"::
    get(D, member, reporter, Label),
    send(Label, clear),
    send(D, report_type, @nil),
    send(D?report_count, value, 0),
    send(D, show_line_number, @nil).

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

/* What a pane does not have to write out for itself.

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
    get(P?name, label_name, Label).   % as class tab would have written it

%       A pane has no window of its own, so ->open means "put me in one
%       and bring it up".  It keeps the signature class window gives it:
%       the window system sends ->open to a window with a position and a
%       display, and a method of another shape would be a clash rather
%       than an override -- see `man_frame ->open_centered'.

open(P, _:[point], _:[display]) :->
    "Show me in a window of the IDE"::
    show_pane(P).

expose(P) :->
    "Bring the window I am in up, with me in view"::
    show_pane(P).

%       What a pane has to say goes on the bar of the window it is in,
%       which grows one the first time anything asks.  A pane that has a
%       place of its own to report -- a terminal writes over its own text
%       -- says so with a ->report of its own, which takes the place of
%       this one.
%
%       There is one bar and it belongs to the pane in view.  A tool that
%       keeps itself up to date whether or not anybody is looking -- the
%       thread monitor says what it found on every update -- wrote over
%       what the pane the user was working in had to say, from a tab they
%       could not even see.

report(P, Kind:name, Fmt:[char_array], Args:any ...) :->
    "Report on the bar of the window I am in, if I am the pane in view"::
    (   pane_in_view(P)
    ->  pane_status_bar(P),
        Msg =.. [report, Kind, Fmt|Args],
        send_super(P, Msg)
    ;   true
    ).

%!  pane_in_view(+Pane) is semidet.
%
%   True when Pane is the one the user is working in, or is in no window
%   of the IDE at all and so shares a bar with nobody.

pane_in_view(P) :-
    (   get(P, pane_frame, Frame)
    ->  get(Frame, current_pane, P)
    ;   true
    ).

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
    ),
    pane_arranged(P).

new_tab(P) :->
    "Put a new pane like me in a tab of its own"::
    get(P, sibling, New),
    get(P, pane_frame, Frame),
    send(Frame, append_pane, New, @default, @on),
    send(Frame, keyboard_focus, New),
    pane_arranged(New).

new_window(P) :->
    "Put a new pane like me in a window of its own"::
    get(P, sibling, New),
    (   get(P, pane_frame, Frame)
    ->  get(Frame, application, App)
    ;   App = @default
    ),
    send(new(pane_frame(App, @default, New)), open),
    pane_arranged(New).                 % a window of its own is an
                                        % arrangement to learn as well


move_to_tab(P) :->
    "Move me out of a split, into a tab of my own"::
    get(P, pane_frame, F),
    get(P, pane_tab, Tab),
    get(Tab, windows, Windows),
    get(Windows, size, Size),
    Size > 1,                           % a tab of my own already
    send(Tab, delete, P),               % take me out without destroying me
    send(F, append_pane, P, @default, @on),
    pane_arranged(P).

%       The other way round: a pane that has a tab to itself is put into
%       the tab beside it, which takes its own tab away.  ->append moves
%       the pane out of the tab it is in, so there is nothing to undo
%       here; the tab left empty destroys itself.

neighbour_tab(P, Where:{previous,next}, Tab:tab_frame) :<-
    "The tab before or after mine; fails if there is none"::
    get(P, pane_tab, Mine),
    get(Mine, device, Stack),
    get(Stack, tabs, Tabs),
    get(Tabs, index, Mine, Rank),
    (   Where == previous
    ->  N is Rank-1
    ;   N is Rank+1
    ),
    N >= 1,
    get(Tabs, nth1, N, Tab).

can_move_to_neighbour_tab(P, Where:{previous,next}) :->
    "True if my tab holds nothing but me and there is one beside it"::
    get(P, pane_tab, Mine),
    get(Mine, windows, Windows),
    get(Windows, size, 1),              % sharing: ->move_to_tab is the way
    get(P, neighbour_tab, Where, _).

move_to_neighbour_tab(P, Where:{previous,next}) :->
    "Move me into the tab before or after mine"::
    send(P, can_move_to_neighbour_tab, Where),
    get(P, pane_frame, F),
    get(P, neighbour_tab, Where, Tab),
    pane_side(P, Side),
    send(Tab, append, P, @default, Side),
    send(F, current_pane, P),
    send(F, keyboard_focus, P),
    pane_arranged(P).

%       Same rule as `prolog_ide <-pane_side': a tool says which side of
%       what is there it belongs on, anything else goes below.

pane_side(P, Side) :-
    send(P, has_get_method, pane_side),
    get(P, pane_side, Side),
    !.
pane_side(_, below).

detach(P) :->
    "Move me into a window of my own"::
    get(P, pane_frame, F),
    get(F, panes, Panes),
    get(Panes, size, Size),
    Size > 1,                           % alone already: nothing to do
    (   get(F, application, App0),
        App0 \== @nil
    ->  App = App0
    ;   App = @default
    ),
    get(P, display_position, point(X, Y)),
    ignore(send(F, arranged)),          % the window I am leaving changed too
    send(F, delete_pane, P, @off),      % take me out without destroying me
    new(New, pane_frame(App, @default, P)),
    send(New, open, point(X, Y+20)),
    pane_arranged(P).

close_pane(P) :->
    "Close me; my frame goes with me if I was its last pane"::
    (   get(P, pane_frame, Frame)
    ->  send(Frame, delete_pane, P, @on),
        (   object(Frame)               % it went with its last pane
        ->  ignore(send(Frame, arranged))
        ;   true
        )
    ;   send(P, destroy)
    ).

event(P, Ev:event) :->
    "Let my frame decide whether entering me gives me the focus"::
    (   send(Ev, is_a, area_enter),
        get(P, pane_frame, Frame),
        send(Frame, focus_on_enter, P)
    ->  true
    ;   send_super(P, event, Ev)
    ).

:- pce_end_class(pane).


                 /*******************************
                 *          PANE STACK          *
                 *******************************/

/* A pane that holds its windows in tabs.

Most panes are one window: a terminal, a single editor.  Some are not: a
tool is usually several windows -- the thread monitor is a list of
threads beside a graph -- that have to be laid out against one another
and to travel together, and PceEmacs wants a tab per source with the
tabs belonging to the editor rather than to the window it is in.

A tabbed_window holding tab_frames does both.  A tab_frame lays windows
out with a tile the way class frame does for its members, so the windows
of a tab can be arranged and the gaps between them dragged; a lone tab
shows no label; and to everything outside it is one window, so it drops
into a tab of a window of the IDE like any other pane.

A subclass holding a single tab is a tool -- see tool_pane below.  One
holding many is a *group*: the frame looks inside it for the pane the
user is really working in, which is what `pane_frame <-current_pane'
does with the optional <-current_pane below.  See `emacs_pane' in
library(emacs/window).
*/

:- pce_begin_class(pane_stack, tabbed_window,
                   "A pane of the IDE that holds its windows in tabs").
:- use_class_template(pane).

variable(grip, split_handle*, get, "The grip I am dragged by").

%       Where I belong in a window that already has something in it: a
%       navigator down the left, a monitor along the bottom.  It is a
%       class variable, so a tool says where it goes by declaring one of
%       its own, and the user overrules that from a Defaults file:
%
%           prolog_thread_monitor.pane_side: right

class_variable(pane_side, {above,below,left,right}, below,
               "Which side of what is there I am added on").

%       The windows in me carry grips of their own -- every emacs_view
%       does -- but a grip shows itself only on a window its frame calls
%       a pane, and the pane here is me.  See `split_handle
%       ->update_displayed': they hide themselves and mine is the one
%       that is seen.

initialise(TP, Label:[name]) :->
    "Create empty, with a grip to drag me by"::
    send_super(TP, initialise, Label),
    send(TP, slot, grip, new(H, split_handle)),
    send(H, pane, TP).                  % it moves me, not the window it
                                        % is on

pane_side(TP, Side:{above,below,left,right}) :<-
    "Which side of what is there I am added on"::
    get(TP, class_variable_value, pane_side, Side).

%       Not <-member: on a tabbed_window that answers the window of a
%       named tab.  The windows of a tool are told apart by their class.

window(TP, Class:name, W:window) :<-
    "A window of mine of the given class"::
    get(TP, members, Windows),
    get(Windows, find, message(@arg1, instance_of, Class), W).

resize(TP, Tab:[tab]) :->
    "Keep the grip on the window in my corner"::
    send_super(TP, resize, Tab),
    ignore(send(TP, place_grip)).

%       Each of my windows has a surface of its own, so a grip displayed
%       on me is covered by whichever of them is over it.  It goes on the
%       fixed layer of the window that is in my corner instead, and moves
%       house when the layout changes which window that is.  Where in that
%       window it sits is the grip's own business -- see `split_handle
%       ->compute'.

place_grip(TP) :->
    "Put the grip on the window in my top right corner"::
    get(TP, grip, Handle),
    Handle \== @nil,
    get(TP, corner_window, W),
    (   get(Handle, device, W)
    ->  true
    ;   send(W, display_fixed, Handle)
    ).

corner_window(TP, W:window) :<-
    "The window of mine at my top right"::
    get(TP, current_tab, Tab),
    get(Tab, windows, Chain),
    chain_list(Chain, Windows),
    Windows \== [],
    get(TP, size, size(PW, _)),
    Right is PW-1,
    (   member(W, Windows),
        placed_area(W, area(X, Y, AW, AH)),
        Right >= X, Right =< X+AW,
        0 >= Y, 0 =< Y+AH
    ->  true
    ;   Windows = [W|_]
    ).

current_tab(TP, Tab:tab_frame) :<-
    "The tab of mine that is in view"::
    (   get(TP, on_top, Tab0)
    ->  Tab = Tab0
    ;   get(TP, tabs, Tabs),
        get(Tabs, head, Tab)
    ).

on_top(TP, Tab:tab_frame) :<-
    "The tab of mine that is on top; fails while there is none"::
    get(TP, tabs, Tabs),
    get(Tabs, find, @arg1?status == on_top, Tab).

%       Switching between my tabs moves the pane the user is working in
%       without touching the frame's tabs, so the frame has to be told:
%       its menu bar, its title and its bar at the bottom all follow the
%       pane in view.  Cf. `pane_tabbed_window ->current'.

current(TP, Window:window) :->
    "Make Window the one I show and tell the frame"::
    send_super(TP, current, Window),
    (   get(TP, pane_frame, Frame),
        send(Frame, has_send_method, pane_changed)
    ->  send(Frame, pane_changed)
    ;   true
    ).

%       What is asked of a pane and answered by the windows in it.  My
%       last tab going takes me with it; being asked to close closes them
%       rather than me, and I go when the last of them does.

empty(TP) :->
    "My last tab was closed"::
    (   get(TP, pane_frame, Frame)
    ->  send(Frame, delete_pane, TP, @on)
    ;   send(TP, destroy)
    ).

can_close(TP, Reply:bool) :<-
    "@on if every window of mine agrees to be closed"::
    get(TP, members, Windows),
    chain_list(Windows, List),
    (   member(W, List),
        send(W, has_get_method, can_close),
        \+ get(W, can_close, @on)
    ->  Reply = @off
    ;   Reply = @on
    ).

close_pane(TP) :->
    "Close my windows, which takes me with them"::
    get(TP, members, Windows),
    chain_list(Windows, List),
    forall(member(W, List), close_pane(W)).

:- pce_end_class(pane_stack).


                 /*******************************
                 *          TOOL PANE           *
                 *******************************/

/* A pane that shows more than one window.

A tool is one pane made of several windows, and thus a pane_stack with a
single tab: a lone tab shows no label, so the tool looks like the one
pane it is.

    :- pce_begin_class(my_tool, tool_pane, "...").

    initialise(T) :->
        send_super(T, initialise, my_tool),
        send(T, append_window, new(B, my_browser)),
        send(T, append_window, new(my_view), B, right).
*/

:- pce_begin_class(tool_pane, pane_stack,
                   "A pane of the IDE showing more than one window").

initialise(TP, Label:[name]) :->
    "Create empty, showing no tab strip"::
    send_super(TP, initialise, Label),
    send(TP, hide_single_label, @on).   % I am one pane, not a tab strip

append_window(TP, Window:window,
                  Relative:relative_to=[window],
                  Where:where=[{above,below,left,right}]) :->
    "Add a window beside the ones I have"::
    (   get(TP, content, Tab)
    ->  send(Tab, append, Window, Relative, Where)
    ;   send(TP, tab, tab_frame(Window, TP?name))
    ).

content(TP, Tab:tab_frame) :<-
    "The tab my windows are tiled in"::
    get(TP, tabs, Tabs),
    get(Tabs, head, Tab).

:- pce_end_class(tool_pane).


                 /*******************************
                 *        OPEN FROM A TERM      *
                 *******************************/

%!  open_pane_frame(+Term, -Frame) is semidet.
%!  open_pane_frame(+Term, -Frame, +Options) is semidet.
%
%   Open a window of the IDE holding what Term says.  Term is what
%   `pane_frame <-pane_term' writes; see the PANE TERM section above for
%   what it looks like.  Fails, after reporting, when nothing in Term
%   could be restored.
%
%   Options:
%
%     - application(+Application)
%       Application the window belongs to.  Default is @prolog_ide, or
%       none when library(swi_ide) cannot be loaded.
%     - open(+Bool)
%       Open the window (default `true').  A test that only wants the
%       structure says `false' and never touches the window system.

open_pane_frame(Term, Frame) :-
    open_pane_frame(Term, Frame, []).

open_pane_frame(Term, Frame, Options) :-
    frame_application(Options, App),
    new(Frame, pane_frame(App)),
    (   memberchk(open(false), Options)
    ->  true
    ;   send(Frame, open)          % before it is filled: a share is
    ),                             % pixels of the room there is
    (   send(Frame, pane_term, Term)
    ->  true
    ;   send(Frame, destroy),
        fail
    ).

frame_application(Options, App) :-
    (   memberchk(application(App0), Options)
    ->  App = App0
    ;   catch(use_module(user:library(swi_ide), []), _, fail)
    ->  App = @prolog_ide
    ;   App = @default
    ).


                 /*******************************
                 *          AT THE END          *
                 *******************************/

%!  record_open_arrangements is det.
%
%   Credit every window that the user arranged with the time it has been
%   as they left it.  A window closed by hand has already been credited by
%   `pane_frame ->close'; this is for the ones that are still open when
%   Prolog halts.  What is credited is written down as it is credited, so
%   there is nothing else left to do here.

record_open_arrangements :-
    (   object(@display)
    ->  send(@display?frames, for_all,
             if(message(@arg1, instance_of, pane_frame),
                if(message(@arg1, record_arrangement))))
    ;   true
    ).

:- initialization
   send(@pce, exit_message, message(@prolog, record_open_arrangements)).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(pane_frame(Message)) -->
    pane_frame_message(Message).

pane_frame_message(name_taken(Wanted, Got)) -->
    [ 'Pane frame: a window is called ~w already; using ~w'-[Wanted, Got] ].
pane_frame_message(main_frame_exists) -->
    [ 'Pane frame: there is a main window already' ].
pane_frame_message(empty_tab(Options)) -->
    { tab_name(Options, Name) },
    [ 'Pane frame: nothing of the tab ~w could be restored'-[Name] ].
pane_frame_message(not_a_tab(Term)) -->
    [ 'Pane frame: not a tab: ~p'-[Term] ].
pane_frame_message(needs_arguments(Kind)) -->
    [ 'Pane frame: ~w cannot be restored: it is made for something live'-
      [Kind] ].
pane_frame_message(no_such_file(Path)) -->
    [ 'Pane frame: ~w is not there; opening it as a new file'-[Path] ].
pane_frame_message(no_such_profile(Asked, Instead)) -->
    [ 'Pane frame: there is no profile ~w; running ~w'-[Asked, Instead] ].
pane_frame_message(no_class(Kind)) -->
    [ 'Pane frame: there is no pane of kind ~w'-[Kind] ].
pane_frame_message(not_a_pane(Kind)) -->
    [ 'Pane frame: ~w is not a window'-[Kind] ].
pane_frame_message(no_options(Kind, Options)) -->
    [ 'Pane frame: ~w has nothing to do with ~p'-[Kind, Options] ].
pane_frame_message(pane_failed(Kind, Reason)) -->
    [ 'Pane frame: ~w could not be set up (~p)'-[Kind, Reason] ].

tab_name(Options, Name) :-
    (   memberchk(label(Name), Options)
    ->  true
    ;   Name = ''
    ).
