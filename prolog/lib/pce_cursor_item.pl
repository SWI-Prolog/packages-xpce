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

:- module(pce_cursor_item, []).
:- use_module(library(pce)).
:- use_module(library(help_message)).
:- use_module(library(apply)).
:- use_module(library(lists)).

/** <module> Dialog item to select a cursor

Class cursor_item selects one of the system cursors from a menu.  Next
to the menu is an area that shows the selected cursor when the pointer
is moved over it.  System cursors have no image we can draw, so this is
how the user can see the cursor.

The menu shows the native names of the system cursors.  The old (X11)
names that are aliases of these are shown under the native name.  A
cursor defined from an image is shown as an extra menu entry.
*/

%!  cursor_name(?Name, ?Summary) is nondet.
%
%   The native names of the system cursors, in the order of the menu.

cursor_name(default,     'Normal pointer').
cursor_name(pointer,     'Pointing hand, e.g., for a link').
cursor_name(text,        'Text insertion (I-beam)').
cursor_name(crosshair,   'Crosshair for precise selection').
cursor_name(move,        'Move something').
cursor_name(wait,        'Busy').
cursor_name(progress,    'Busy in the background').
cursor_name(not_allowed, 'Operation not allowed').
cursor_name(ew_resize,   'Resize horizontally').
cursor_name(ns_resize,   'Resize vertically').
cursor_name(nwse_resize, 'Resize diagonally (\\)').
cursor_name(nesw_resize, 'Resize diagonally (/)').
cursor_name(n_resize,    'Resize at the top').
cursor_name(e_resize,    'Resize at the right').
cursor_name(s_resize,    'Resize at the bottom').
cursor_name(w_resize,    'Resize at the left').
cursor_name(ne_resize,   'Resize at the top-right corner').
cursor_name(nw_resize,   'Resize at the top-left corner').
cursor_name(se_resize,   'Resize at the bottom-right corner').
cursor_name(sw_resize,   'Resize at the bottom-left corner').

%!  canonical_cursor_name(+Name, -Native) is semidet.
%
%   Native is the native name of the system cursor Name, which may be
%   an old alias.

canonical_cursor_name(Name, Name) :-
    cursor_name(Name, _),
    !.
canonical_cursor_name(Name, Native) :-
    get(@cursor_names, value, Name, Id),
    cursor_name(Native, _),
    get(@cursor_names, value, Native, Id),
    !.

:- pce_begin_class(cursor_item, label_box,
                   "Select a system cursor").

variable(selection, cursor, get, "Current cursor").

initialise(CI, Name:[name], Selection:[cursor], Msg:[code]*) :->
    "Create from label, initial cursor and message"::
    default(Name, cursor, Nm),
    send_super(CI, initialise, Nm, Msg),
    send(CI, gap, size(8,0)),
    send(CI, append,
         new(M, menu(cursor_name, cycle, message(CI, user_selection, @arg1)))),
    send(M, show_label, @off),
    forall(cursor_name(CN, Summary),
           ( send(M, append, menu_item(CN, @default, CN)),
             get(M, member, CN, MI),
             send(MI, help_message, tag, Summary)
           )),
    send(CI, append, new(Try, cursor_preview), right),
    send(Try, help_message, tag, 'Point here to see the cursor'),
    default(Selection, default, Initial),
    send(CI, selection, Initial).

selection(CI, Cursor:cursor) :->
    "Set the cursor"::
    send(CI, slot, selection, Cursor),
    get(CI, member, cursor_name, Menu),
    (   get(Cursor, image, Image), Image \== @nil
    ->  image_entry(Menu, Cursor, Value)
    ;   get(Cursor, name, Name),
        (   canonical_cursor_name(Name, Value)
        ->  true
        ;   image_entry(Menu, Cursor, Value)
        )
    ),
    send(Menu, selection, Value),
    get(CI, member, cursor_preview, Try),
    send(Try, cursor, Cursor),
    send(CI, modified, @off).

%   image_entry(+Menu, +Cursor, -Value)
%
%   Make sure Menu has an entry for Cursor, which is not a system
%   cursor.

image_entry(Menu, Cursor, Value) :-
    get(Cursor, name, Name),
    (   Name == @nil
    ->  Value = image_cursor
    ;   Value = Name
    ),
    (   get(Menu, member, Value, _)
    ->  true
    ;   send(Menu, append, menu_item(Value, @default, Value))
    ),
    get(Menu, member, Value, MI),
    send(MI, attribute, cursor, Cursor).

user_selection(CI, Value:name) :->
    "The user selected a cursor from the menu"::
    get(CI, member, cursor_name, Menu),
    get(Menu, member, Value, MI),
    (   get(MI, attribute, cursor, Cursor)
    ->  true
    ;   get(@pce, convert, Value, cursor, Cursor)
    ),
    send(CI, selection, Cursor),
    send(CI, forward).

forward(CI) :->
    send(CI, modified, @on),
    (   send(CI?device, modified_item, CI, @on)
    ->  true
    ;   ignore(send(CI, apply))         % no message is fine
    ).

modified_item(_CI, _Gr:graphical, _Modified:bool) :->
    fail.

clear(_CI) :->
    true.

active(CI, Val:bool) :->
    send_super(CI, active, Val),
    send(CI?graphicals, for_all, message(@arg1, active, Val)).

:- pce_end_class(cursor_item).


:- pce_begin_class(cursor_preview, device,
                   "Area that shows its cursor").

initialise(P) :->
    send_super(P, initialise),
    send(P, name, cursor_preview),
    send(P, display, new(T, text('Point here'))),
    send(T, colour, ui_dialog_foreground),
    get(T, area, A),
    get(A, width, TW),
    get(A, height, TH),
    W is TW + 12,
    H is TH + 4,
    send(P, display, new(B, box(W, H)), point(0, 0)),
    send(B, radius, 4),
    send(B, colour, grey50),
    send(B, texture, dashed),
    send(T, center, B?center),
    send(B, hide).

reference(P, Ref:point) :<-
    "Baseline of the text"::
    get(P?graphicals, find, message(@arg1, instance_of, text), T),
    get(T?font, ascent, A),
    get(T, y, Y),
    RY is Y + A,
    new(Ref, point(0, RY)).

:- pce_end_class(cursor_preview).
