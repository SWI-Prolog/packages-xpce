/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2003-2011, University of Amsterdam
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

:- module(tabbed_window, []).
:- use_module(library(pce)).
:- use_module(library(hyper)).
:- use_module(library(help_message), []).
:- use_module(library(pce_icon_button), []).
:- use_module(library(pce_util), [chain_list/2]).
:- use_module(library(lists), [member/2, last/2]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This class creates a tabbed window:  a   window  displaying  a number of
tabs, each displaying a window.   Here is some simple code using it:

test :-
        new(TW, tabbed_window('Nice tabs')),
        send(TW, append, new(P, picture)),
        send(P, display, box(200, 200), point(50,50)),
        send(TW, append, new(view)),
        send(TW, append, new(D, dialog)),
        send(D, append, text_item(name)),
        send(D, append, button(quit, message(TW, destroy))),
        send(TW, open).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(tabbed_window, dialog,
                   "Resizeable window holding set of tabs").

variable(label_popup,   popup*, both, "Popup shown on labels").
variable(new_tab_message, code*, get,
         "Run by the new-tab button; @nil: no such button").

initialise(W, Label:label=[name], Size:size=[size],
           Display:display=[display]) :->
    send_super(W, initialise, Label, Size, Display),
    send(W, hor_stretch, 100),
    send(W, ver_stretch, 100),
    send(W, hor_shrink, 100),
    send(W, ver_shrink, 100),
    send(W, pen, 0),
    send(W, border, size(0,0)),
    send_super(W, append, new(tab_stack)).

resize(W, Tab:[tab]) :->
    "Resize member tabs to fit the dialog"::
    get_super(W, member, tab_stack, TS),
    get(W, area, area(_,_,Width, Height)),
    get(TS, tabs, Tabs),
    new(LabelH, number(0)),
    send(Tabs, for_all,           % 0 while a lone tab drops its label
         message(LabelH, maximum, @arg1?label_height)),
    get(LabelH, value, LH),
    TabH is Height - LH,
    (   Tab == @default
    ->  send(Tabs, for_all,
             message(@arg1, size, size(Width,TabH)))
    ;   send(Tab, size, size(Width,TabH))
    ).

layout_dialog(W, _Gap:[size], _Size:[size], _Border:[size]) :->
    "Overrule to deal with nested tabbed windows"::
    new(S0, size(0,0)),
    send_super(W, layout_dialog, S0, S0, S0).

%       ->resize is what fits the tabs to the window, and the window
%       system only sends it once the window has a surface to draw on
%       (ws_geometry_window(), src/sdl/sdlwindow.c).  A tabbed window
%       placed by a tile before it is created -- which is what happens
%       when one is used as a pane -- would keep the size it asked for
%       rather than the size it was given.  Being placed is enough.

geometry(W, X:[int], Y:[int], Width:[int], Height:[int]) :->
    "Fit my tabs to the size I am given"::
    send_super(W, geometry, X, Y, Width, Height),
    send(W, resize).

new_tab_message(W, Message:'code*') :->
    "What the new-tab button is to do; @nil takes the button away"::
    send(W, slot, new_tab_message, Message),
    get_super(W, member, tab_stack, TS),
    send(TS, layout_labels).

hide_single_label(W, Hide:bool) :->
    "Give a lone tab the room its label would take"::
    get_super(W, member, tab_stack, TS),
    send(TS, hide_single_label, Hide).

hide_single_label(W, Hide:bool) :<-
    "Does a lone tab drop its label?"::
    get_super(W, member, tab_stack, TS),
    get(TS, hide_single_label, Hide).

:- pce_group(stack).

on_top(W, Top:'name|window') :->
    "Put the named tab or tab containing Window on top"::
    get_super(W, member, tab_stack, TS),
    (   atom(Top)
    ->  (   get(TS, member, Top, Tab)
        ->  send(TS, on_top, Tab)
        ;   get(W, hypered, tab, @arg3?name == Top, Window)
        ->  send(Window, expose)
        )
    ;   get(Top, container, tab, Tab)
    ->  make_current(Tab, Top),
        send(TS, on_top, Tab)
    ).

current(W, Window:window) :<-
    "Window of currently selected tab"::
    get_super(W, member, tab_stack, TS),
    get(TS, on_top, Tab),
    get(Tab, window, Window).

current(W, Window:window) :->
    "Window of currently selected tab"::
    get(Window, container, tab, Tab),
    make_current(Tab, Window),
    (   get(Tab, status, on_top)
    ->  send(W, resize, Tab)
    ;   get_super(W, member, tab_stack, TS),
        send(TS, on_top, Tab)
    ).

%       make_current(+Tab, +Window)
%
%       If Tab holds more than one window (see class tab_frame), tell it
%       which of them the request is about.

make_current(Tab, Window) :-
    (   send(Tab, has_send_method, current)
    ->  send(Tab, current, Window)
    ;   true
    ).

%       Losing the focus has to reach the window that has it, and by the
%       time it is taken away <-current is usually the window it is being
%       given to -- a frame moves its `input_window\' by activating the new
%       one, which makes it current, and only then deactivating the old.
%       So I hand the focus on to a window I remember, not to whichever is
%       current at the moment I am told.

variable(focus_window, window*, none, "The window I passed the focus to").

input_focus(W, Focus:bool) :->
    send_super(W, input_focus, Focus),
    (   Focus == @on
    ->  (   get(W, current, Current)
        ->  send(W, slot, focus_window, Current),
            send(Current, input_focus, @on)
        ;   true
        )
    ;   get(W, slot, focus_window, Old),
        Old \== @nil
    ->  send(W, slot, focus_window, @nil),
        send(Old, input_focus, @off)
    ;   true
    ).

:- pce_group(members).

%       ->append: Window, Label, [Expose]
%
%       Append a new tab using Window with the given tab label.
%
%       The call to ->'_compute_desired_size' should be properly delayed
%       until the tabbed window is actually   created,  but this doesn't
%       appear to work properly. If Expose == @on the tab is immediately
%       brought to the top.

append(W, Window:window=window, Label:name=[name], Expose:expose=[bool]) :->
    "Append a window to the tabs"::
    send(Window, '_compute_desired_size'),
    get(W, new_tab, Window, Label, Tab),
    send(W, tab, Tab),
    (   Expose == @on
    ->  send(W, resize, Tab),
        get_super(W, member, tab_stack, TS),
        send(TS, on_top, Tab)
    ;   true
    ).

new_tab(_W, Window:window, Label:[name], Tab:tab) :<-
    "Create the tab that is to hold Window"::
    new(Tab, window_tab(Window, Label)).

tabs(W, Tabs:chain) :<-
    "New chain holding my tabs"::
    get_super(W, member, tab_stack, TS),
    get(TS, tabs, Tabs).

member(W, Name:name, Window:window) :<-
    "Get named window from tabbed window"::
    get_super(W, member, tab_stack, TS),
    get(TS, member, Name, Tab),
    get(Tab, window, Window).

members(W, Windows:chain) :<-
    "New chain with member windows"::
    new(Windows, chain),
    get_super(W, member, tab_stack, TS),
    send(TS?tabs, for_all,
         message(Windows, merge, @arg1?windows)),
    (   get(W, all_hypers, Hypers)
    ->  send(Hypers, for_all,
             if(@arg1?forward_name == toplevel,
                message(Windows, append, @arg1?to)))
    ;   true
    ).

clear(W) :->
    "Remove all member tabs"::
    get_super(W, member, tab_stack, TS),
    send(TS, clear).

tab(W, Tab:tab) :->
    "Add normal tab"::
    get_super(W, member, tab_stack, TS),
    send(TS, append, Tab),
    (   get(W, is_displayed, @on)
    ->  send(W, resize, Tab)
    ;   true
    ).

tab(W, Name:name, Tab:tab) :<-
    "Find named tab"::
    get_super(W, member, tab_stack, TS),
    get(TS, member, Name, Tab).

empty(_W) :->
    "Abstract method.  Called if last window disappears"::
    true.

:- pce_group(frame).

frame_window(TW, Window:window, Name:name, Rank:'1..', Frame:frame) :<-
    "After un-tabbing, give the window a new frame"::
    new(Frame, window_tab_frame(Window, Name, Rank)),
    new(_, partof_hyper(TW, Window, toplevel, tab)).

:- pce_end_class(tabbed_window).


                 /*******************************
                 *          TAB LABELS          *
                 *******************************/

%       The label of a tab is drawn by the tab itself rather than being a
%       graphical of its own, so what can be done to it is answered here
%       rather than by something sitting on it.  This is on class tab, so
%       that it holds for a window_tab and for a tab_frame alike.

:- pce_extend_class(tab).

label_popup(Tab, Popup:popup) :<-
    "Popup of the tabbed_window I am in"::
    get(Tab?device, window, TabbedWindow),
    send(TabbedWindow, has_get_method, label_popup),
    get(TabbedWindow, label_popup, Popup),
    Popup \== @nil.

:- pce_global(@tab_label_recogniser,
              new(popup_gesture(@receiver?label_popup))).

%       ->send_super is no use here: class tab defines ->label_event
%       itself, so the one below replaces it rather than adding to it and
%       the super is class dialog_group, which has none.  Raising the tab
%       on a left click is therefore repeated here, from labelEventTab().

label_event(T, Ev:event) :->
    "Raise on a click, rename on a double one, popup on the right button"::
    (   send(Ev, is_a, ms_left_down),
        get(T, active, Active),
        Active \== @off
    ->  (   get(T, editable_label, @on),
            get(Ev, multiclick, double)
        ->  send(T, edit_label)
        ;   send(T?device, on_top, T)
        )
    ;   send(@tab_label_recogniser, event, Ev)
    ).

edit_label(T) :->
    "Put an editor over my label"::
    get(T, editable_label, @on),
    get(T, label_height, H),
    H > 0,                              % a lone tab may show no label
    get(T, device, Stack),
    send(T, end_label_edit),
    get(T?label_size, width, W),
    get(T, label_offset, X),
    send(Stack, display, new(TI, tab_label_item(T)), point(X, 0)),
    send(TI, set, X, 0, W, H),
    send(Stack?window, keyboard_focus, TI).

close_tab(T) :->
    "Close me; what that means is up to what I hold"::
    send(T, destroy).

end_label_edit(T) :->
    "Take the editor away, if there is one"::
    get(T, device, Stack),
    (   get(Stack, member, tab_label_item, TI)
    ->  send(Stack?window, keyboard_focus, @nil),
        send(TI, destroy)
    ;   true
    ).

label_edited(T, Label:name) :->
    "Take the label typed into the editor"::
    send(T, end_label_edit),
    (   Label == ''
    ->  true
    ;   send(T, label, Label),
        send(T, compute),               % the label box has a new width
        send(T?device, layout_labels)
    ).

:- pce_end_class.


                 /*******************************
                 *          TAB BUTTONS         *
                 *******************************/

%       The labels are drawn by the tabs themselves, so anything to click
%       on them is displayed on the stack instead, at the place the label
%       was given.  tab_stack ->labels_laid_out is sent whenever those
%       places change, which is the one moment the buttons have to follow.

:- pce_extend_class(tab_stack).

tabs(TS, Tabs:chain) :<-
    "My tabs, in order, without the buttons on the label row"::
    get(TS?graphicals, find_all, message(@arg1, instance_of, tab), Tabs).

labels_laid_out(TS) :->
    "Put the buttons back where the labels are now"::
    ignore(send(TS, update_tab_buttons)).

update_tab_buttons(TS) :->
    "A close button per closable tab, and one to add a tab at the end"::
    get(TS, graphicals, Graphicals),
    chain_list(Graphicals, List),
    tab_list(List, Tabs),
    forall(member(T, Tabs),
           send(TS, place_close_button, T)),
    send(TS, place_new_tab_button, Tabs).

place_close_button(TS, T:tab) :->
    "Give T a close button, or take away the one it has"::
    (   get(T, close_button_area, area(BX, BY, W, H)),   % says class tab
        get(T, area, area(TX, TY, _, _))
    ->  get(TS, tab_button, T, close_button, size(W, H),
            close_tab, message(T, close_tab), 'Close this tab', B),
        X is TX+BX,
        Y is TY+BY,
        send(B, set, X, Y)
    ;   send(TS, forget_tab_button, T, close_button)
    ).

place_new_tab_button(TS, Tabs:prolog) :->
    "Put the new-tab button after the last label"::
    (   new_tab_message(TS, Message),
        last(Tabs, Last),
        get(Last, label_button_area, area(BX, BY, S, S)),
        get(Last, area, area(TX, TY, _, _)),
        get(Last, label_offset, LX),
        get(Last?label_size, width, LW)
    ->  Gap is LX+LW-BX-S,              % the same room it leaves on a label
        X is TX+LX+LW+Gap,
        Y is TY+BY,
        get(TS, tab_button, TS, new_tab_button, size(S, S),
            new_tab, Message, 'Open a new tab', B),
        send(B, set, X, Y)
    ;   send(TS, forget_tab_button, TS, new_tab_button)
    ).

tab_button(TS, Owner:object, Role:name, Size:size, Which:name,
           Message:code, Help:name, Button:icon_button) :<-
    "The button Owner holds under Role, made if it has none of that size"::
    get(Size, width, W),
    get(Size, height, H),
    (   get(Owner, hypered, Role, B),
        get(B, size, size(W, H))
    ->  Button = B
    ;   send(TS, forget_tab_button, Owner, Role),
        tab_button_image(Which, File),
        new(Button, icon_button(File, size(W, H))),
        send(Button, name, Which),
        send(Button, recogniser,
             click_gesture(left, '', single, Message)),
        send(Button, help_message, tag, Help),
        send(TS, display, Button),
        new(_, partof_hyper(Owner, Button, Role, tab_button))
                                        % so that it goes with what it
                                        % belongs to, tab or stack
    ).

forget_tab_button(_TS, Owner:object, Role:name) :->
    "Take away the button Owner holds under Role, if any"::
    (   get(Owner, hypered, Role, B)
    ->  send(B, destroy)
    ;   true
    ).

:- pce_end_class.

%!  new_tab_message(+TabStack, -Message) is semidet.
%
%   What the new-tab button is to do, as the tabbed_window says.  There is
%   no button while it says nothing.

new_tab_message(TS, Message) :-
    get(TS, window, TW),
    TW \== @nil,
    send(TW, has_get_method, new_tab_message),
    get(TW, new_tab_message, Message),
    Message \== @nil.

tab_button_image(close_tab, 'tool/close-tab.svg').
tab_button_image(new_tab,   'tool/new-tab.svg').

tab_list([], []).
tab_list([G|T0], Tabs) :-
    (   send(G, instance_of, tab)
    ->  Tabs = [G|T]
    ;   Tabs = T
    ),
    tab_list(T0, T).


:- pce_begin_class(tab_label_item, text_item,
                   "Editor over the label of a tab").

initialise(TI, Tab:tab) :->
    "Edit the label of Tab"::
    get(Tab, label, Label),
    send_super(TI, initialise, tab_label_item, Label,
               message(Tab, label_edited, @arg1)),
    send(TI, show_label, @off),
    new(_, hyper(Tab, TI, label_item, tab)).

tab(TI, Tab:tab) :<-
    "The tab I am editing"::
    get(TI, hypered, tab, Tab).

typed(TI, Id:event_id) :->
    "Escape puts the old label back"::
    (   Id == 27
    ->  get(TI, tab, Tab),
        send(Tab, end_label_edit)
    ;   send_super(TI, typed, Id)
    ).

:- pce_end_class(tab_label_item).


                 /*******************************
                 *           WINDOW TAB         *
                 *******************************/


:- pce_begin_class(window_tab(name), tab,
                   "Tab displaying a window").

variable(window,        window*,      get, "Displayed window").
variable(closing,       bool := @off, get, "We are about to close").
delegate_to(window).

initialise(T, Window:window=[window], Name:name=[name]) :->
    "Create from window and name"::
    (   Window == @default
    ->  new(W, picture)
    ;   W = Window
    ),
    (   Name == @default
    ->  get(W, name, TheName)
    ;   TheName = Name
    ),
    (   get(W, decoration, Decor),
        Decor \== @nil
    ->  true
    ;   Decor = Window
    ),
    send(Decor, lock_object, @on),
    (   get(Decor, slot, frame, Frame),
        Frame \== @nil
    ->  send(Frame, delete, Decor)
    ;   true
    ),
    send(Decor, slot, tile, @nil),
    send_super(T, initialise, TheName),
    send(T, border, size(0,0)),
    send_super(T, display, Decor),
    get(Decor, unlock, _),
    send(T, slot, window, W),
    new(_, mutual_dependency_hyper(T, W, window, tab)).

windows(T, Windows:chain) :<-
    "New chain holding my (single) window"::
    get(T, window, Window),
    new(Windows, chain(Window)).

unlink(Tab) :->
    "Trap if I'm the last tab"::
    (   get(Tab, device, Dev),
        Dev \== @nil
    ->  get(Dev?tabs, size, Count),
        (   Count == 1
        ->  get(Tab, container, tabbed_window, TabbedWindow),
            send_super(Tab, unlink),
            send(TabbedWindow, empty)
        ;   send_super(Tab, unlink)
        )
    ;   send_super(Tab, unlink)
    ).

:- pce_group(resize).

%       ->size
%
%       This method must update the size of  the window. For some, to me
%       unknown,  reason  this  does  not    work  correctly  when  done
%       immediately.  Possibly  this  has  something   to  do  with  X11
%       synchronisation. We use the hack   in_pce_thread/1 to reschedule
%       the window resize in the event loop.

size(T, Size:size) :->
    "Adjust size of tab and window"::
    (   get(T, closing, @on)
    ->  true
    ;   in_pce_thread(resize_window(T)),
        send_super(T, size, Size)
    ).

resize_window(T) :-
    (   object(T)                             % but the window may be gone
    ->  send(T, resize_window)
    ;   true
    ).

resize_window(T) :->
    get(T, size, size(W, H)),
    get(T, window, Window),
    (   get(Window, decoration, Decor),
        Decor \== @nil
    ->  Resize = Decor
    ;   Resize = Window
    ),
    send(Resize, do_set, 0,0,W,H).

:- pce_group(event).

status(T, Status:{on_top,hidden}) :->
    send_super(T, status, Status),
    (   Status == on_top,
        get(T, is_displayed, @on),
        get(T, container, tabbed_window, TabbedWindow)
    ->  send(TabbedWindow, current, T?window)
    ;   true
    ).

:- pce_group(delegate).

display(T, Gr:graphical, Pos:[point]) :->
    "Delegate to window"::
    get(T, window, Window),
    send(Window, display, Gr, Pos).

append(T, Item:graphical, RelPos:[{below,right,next_row}]) :->
    "Delegate to window"::
    get(T, window, Window),
    send(Window, append, Item, RelPos).

:- pce_group(frame).

rank(Tab, Rank:'1..') :<-
    "Get position number of the tab"::
    get(Tab, device, Stack),
    get(Stack?tabs, index, Tab, Rank).

rank(Tab, Rank:'1..') :->
    "Move tab in rank"::
    get(Tab, device, Stack),
    get(Stack?tabs, index, Tab, Rank0),
    (   Rank == Rank0
    ->  true
    ;   (   Rank > Rank0
        ->  Rank1 is Rank+1
        ;   Rank1 = Rank
        ),
        (   Rank1 == 1
        ->  send(Tab, hide)
        ;   Before is Rank1 - 1,
            get(Stack?tabs, nth1, Before, BeforeGr)
        ->  send(Tab, expose, BeforeGr)
        ;   send(Tab, expose)               % make last one
        ),
        send(Stack, layout_labels)
    ).

untab(Tab, W:window) :<-
    "Remove a tab from the tabbed window and return the window"::
    get(Tab, window, W),
    send(W, lock_object, @on),
    send(Tab, delete_hypers, window),
    free(Tab),
    get(W, unlock, _).

untab(Tab) :->
    "Turn the window into a toplevel window"::
    get(Tab, rank, Rank),
    get(Tab, name, Name),
    get(Tab, container, dialog, TabbedWindow),
    get(Tab, display_position, point(X, Y)),
    get(Tab, untab, Window),
    get(TabbedWindow, frame_window, Window, Name, Rank, Frame),
    send(Frame, open, point(X, Y+20)).

%       ->close_other_tabs
%
%       Close all tabs but me. To work   around scheduled resize for the
%       subwindows we first indicate we are about to close the tabs. See
%       also ->size.

close_other_tabs(Tab) :->
    "Destroy all tabs except for me"::
    get(Tab, device, Stack),
    get(Stack, tabs, Tabs),
    send(Tabs, for_all,
         if(@arg1 \== Tab,
            message(@arg1, slot, closing, @on))),
    send(Tabs, for_all,
         if(@arg1 \== Tab,
            message(@arg1, destroy))).

:- pce_end_class(window_tab).


:- pce_begin_class(window_tab_frame, frame,
                   "Temporary frame for an untabbed window").

variable(rank, '1..', get, "Saved position in tabbed window").

initialise(F, Window:window, Name:name, Rank:'1..') :->
    send(F, slot, rank, Rank),
    send_super(F, initialise, Name?label_name),
    send(F, append, Window),
    send(F, done_message, message(F, retab)).


window(F, Window:window) :<-
    "Get the un-tabbed window"::
    get(F?members, head, Window).

retab(F) :->
    "Bring the window back to its tab"::
    get(F, window, Window),
    get(Window, hypered, tab, TabbedWindow),
    get(F, rank, Rank),
    send(F, delete, Window),
    send(Window, delete_hypers, tab),
    send(TabbedWindow, append, Window),
    get(Window, container, tab, Tab),
    send(Tab, rank, Rank),
    send(F, destroy).

contained_in(F, TabbedWindow:tabbed_window) :<-
    "An untabbed window is consider part of the tab"::
    get(F, window, Window),
    get(Window, hypered, tab, TabbedWindow).

:- pce_end_class(window_tab_frame).
