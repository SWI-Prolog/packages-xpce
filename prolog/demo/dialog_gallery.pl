/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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

:- module(dialog_gallery,
          [ dialog_gallery/0
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_tick_box)).
:- use_module(library(pce_float_item)).
:- use_module(library(password_item)).
:- use_module(library(file_item)).
:- use_module(library(pce_colour_item)).
:- use_module(library(pce_font_item)).
:- use_module(library(pce_style_item)).
:- use_module(library(pce_cursor_item)).

/** <module> Gallery of dialog items

Opens a dialog that shows the dialog items (controllers) of XPCE, one
tab per category:

  * Buttons    -- buttons and labels
  * Text       -- text_item, combo box, int_item, float_item,
                  password_item, file_item and an editor
  * Choices    -- the kinds of class menu, bool_item and tick_box
  * Ranges     -- slider and list_browser
  * Pickers    -- colour, font, style and cursor items
  * Groups     -- dialog_group as a box and as a group

The menu bar holds popup menus.  Each item reports the value its
message receives in the status line at the bottom.

Run with:

    ?- dialog_gallery.
*/

dialog_gallery :-
    new(D, dialog('Dialog item gallery')),
    send(D, append, new(MB, menu_bar)),
    menu_bar(D, MB),
    send(D, append,
         new(_, tab_stack(new(Buttons, tab(buttons)),
                          new(Text,    tab(text)),
                          new(Choices, tab(choices)),
                          new(Ranges,  tab(ranges)),
                          new(Pickers, tab(pickers)),
                          new(Groups,  tab(groups))))),
    buttons_tab(D, Buttons),
    text_tab(D, Text),
    choices_tab(D, Choices),
    ranges_tab(D, Ranges),
    pickers_tab(D, Pickers),
    groups_tab(D, Groups),
    send(D, append, new(Status, label(reporter))),
    send(Status, width, 60),
    send(D, report, status, 'Use the items; their values show here'),
    send(D, open).

%!  show_value(+Dialog, +Item, +Value) is det.
%
%   Show the value an item's message received in the status line.

show_value(D, Item, Value) :-
    value_text(Value, Text),
    format(string(Msg), '~w: ~w', [Item, Text]),
    send(D, report, status, Msg).

value_text(Value, Text) :-
    object(Value),
    send(Value, instance_of, chain),
    !,
    chain_list(Value, List),
    maplist(value_text, List, Texts),
    atomic_list_concat(Texts, ', ', Joined),
    format(string(Text), '[~w]', [Joined]).
value_text(Value, Text) :-
    object(Value),
    get(Value, print_name, Name),
    !,
    get(Name, value, Text).
value_text(Value, Text) :-
    format(string(Text), '~p', [Value]).

%!  msg(+Dialog, +Item, -Message) is det.
%
%   Message that reports the value of Item.  Note that a dialog with a
%   button named `apply` would defer these messages until the user
%   presses it; see `dialog ->modified_item`.

msg(D, Item, message(@prolog, show_value, D, Item, @arg1)).

%!  show_toggle(+Dialog, +Item, +Value, +Selected) is det.
%
%   Show the item of a multiple selection popup the user toggled.

show_toggle(D, Item, Value, Selected) :-
    (   Selected == @on
    ->  State = on
    ;   State = off
    ),
    format(string(Msg), '~w: ~w ~w', [Item, Value, State]),
    send(D, report, status, Msg).

%!  menu_bar(+Dialog, +MenuBar) is det.
%
%   The message of a popup receives the value of the selected item.
%   A menu_item can have its own message (`quit`), which receives the
%   context of the popup instead.

menu_bar(D, MB) :-
    msg(D, file, FMsg),
    send(MB, append, new(File, popup(file, FMsg))),
    send_list(File, append,
              [ open,
                menu_item(save, end_group := @on),
                menu_item(quit, message(D, destroy))
              ]),
    send(MB, append,
         new(View, popup(view,
                         message(@prolog, show_toggle, D, view,
                                 @arg1, @arg2)))),
    send(View, multiple_selection, @on),
    send(View, show_current, @on),
    send_list(View, append, [toolbar, status_line]).

%!  buttons_tab(+Dialog, +Tab) is det.

buttons_tab(D, T) :-
    send(T, append, label(label, 'A label shows text or an image')),
    send(T, append, label(image, image('pce.svg'))),
    BMsg = message(@prolog, show_value, D, button, @receiver?name),
    send(T, append, button(run, BMsg)),
    send(T, append, new(Default, button(ok, BMsg)), right),
    send(Default, default_button, @on),
    send(T, append, new(Off, button(inactive, BMsg)), right),
    send(Off, active, @off),
    send(T, append, new(Split, button(more, BMsg)), right),
    msg(D, more, MoreMsg),              % @arg1 is the selected item
    send(Split, popup, new(P, popup(more, MoreMsg))),
    send_list(P, append, [first, second]),
    send(T, append, new(MenuButton, button(actions, @nil)), right),
    msg(D, actions, ActionsMsg),
    send(MenuButton, popup, new(P2, popup(actions, ActionsMsg))),
    send_list(P2, append, [ copy, paste,
                            menu_item(delete, end_group := @on),
                            select_all ]).

%!  text_tab(+Dialog, +Tab) is det.

text_tab(D, T) :-
    msg(D, text_item, TMsg),
    send(T, append, text_item(text_item, 'Some text', TMsg)),
    msg(D, combo_box, CMsg),
    send(T, append, new(Combo, text_item(combo_box, apple, CMsg))),
    send(Combo, value_set, chain(apple, banana, cherry, date)),
    msg(D, int_item, IMsg),
    send(T, append, int_item(int_item, 42, IMsg, 0, 100)),
    msg(D, float_item, FMsg),
    send(T, append, float_item(float_item, 3.14, FMsg)),
    msg(D, password_item, PMsg),
    send(T, append, password_item(password, PMsg)),
    msg(D, file_item, FiMsg),
    send(T, append, file_item(file, '', FiMsg)),
    send(T, append, new(LB, label_box(editor))),
    send(LB, append, new(E, editor(@default, 40, 4))),
    send(E, contents, 'An editor holds\nmultiple lines\nof text.').

%!  choices_tab(+Dialog, +Tab) is det.

choices_tab(D, T) :-
    choice_menu(D, T, cycle,  cycle,  @off, [north, east, south, west]),
    choice_menu(D, T, marked, marked, @off, [small, medium, large]),
    choice_menu(D, T, toggle, toggle, @on,  [bold, italic, underline]),
    choice_menu(D, T, choice, choice, @off, [left, center, right]),
    choice_menu(D, T, choice_multiple, choice, @on, [mon, tue, wed, thu, fri]),
    msg(D, vertical, Msg),
    send(T, append, new(V, menu(vertical, marked, Msg))),
    send(V, layout, vertical),
    send_list(V, append, [one, two, three]),
    send(V, off, three),
    msg(D, bool_item, BMsg),
    send(T, append, bool_item(bool_item, @on, BMsg)),
    msg(D, tick_box, TbMsg),
    send(T, append, new(TB, tick_box(tick_box, @off, TbMsg))),
    send(TB, align_with, value).

choice_menu(D, T, Name, Kind, Multiple, Values) :-
    msg(D, Name, Msg),
    send(T, append, new(M, menu(Name, Kind, Msg))),
    send(M, multiple_selection, Multiple),
    send_list(M, append, Values),
    Values = [First|_],
    send(M, selected, First, @on).

%!  ranges_tab(+Dialog, +Tab) is det.

ranges_tab(D, T) :-
    msg(D, slider, SMsg),
    send(T, append, slider(slider, 0, 100, 25, SMsg)),
    msg(D, real_slider, RMsg),
    send(T, append, slider(real_slider, -1.5, 1.5, 0.5, RMsg)),
    send(T, append, new(LB, list_browser(@default, 30, 6))),
    msg(D, list_browser, LMsg),
    send(LB, select_message, LMsg),
    forall(member(X, [alpha, beta, gamma, delta, epsilon, zeta, eta, theta]),
           send(LB, append, X)).

%!  pickers_tab(+Dialog, +Tab) is det.

pickers_tab(D, T) :-
    msg(D, colour_item, CMsg),
    send(T, append, colour_item(colour, colour(steelblue), CMsg)),
    msg(D, font_item, FMsg),
    send(T, append, font_item(font, normal, FMsg)),
    msg(D, style_item, SMsg),
    send(T, append, style_item(style, @default, SMsg)),
    msg(D, cursor_item, CuMsg),
    send(T, append, cursor_item(cursor, @default, CuMsg)).

%!  groups_tab(+Dialog, +Tab) is det.

groups_tab(D, T) :-
    send(T, append, new(Box, dialog_group(address, box))),
    msg(D, street, SMsg),
    send(Box, append, text_item(street, '', SMsg)),
    msg(D, city, CMsg),
    send(Box, append, text_item(city, '', CMsg)),
    send(T, append, new(Group, dialog_group(options, group)), right),
    msg(D, options, OMsg),
    send(Group, append, new(M, menu(options, toggle, OMsg))),
    send(M, layout, vertical),
    send_list(M, append, [verbose, quiet, debug]).
