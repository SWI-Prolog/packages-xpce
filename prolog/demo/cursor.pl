/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1995-2026, University of Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(pce_cursor,
          [ cursor_demo/0
          ]).

:- use_module(library(pce)).
:- use_module(library(help_message)).

/** <module> Cursor demo

Show the available cursors.  Selecting an item, by clicking or using the
arrow keys, sets the cursor of the browser.  The menu selects the
standard (SDL system) cursors, the names that are kept for compatibility
with the X11 cursor font, or cursors made from an image.
*/

cursor_demo :-
    send(new(cursor_demo), open).

:- pce_begin_class(cursor_demo, frame,
                   "Browse the available cursors").

variable(browser, cursor_browser, get, "Browser showing the cursors").

initialise(F) :->
    send_super(F, initialise, 'Cursor Demo'),
    send(F, append, new(D, dialog)),
    send(D, append,
         new(M, menu(kind, marked, message(F, fill_browser, @arg1)))),
    forall(kind(Kind, Label, Tip),
           ( send(M, append, new(MI, menu_item(Kind, @default, Label))),
             send(MI, help_message, tag, Tip)
           )),
    send(M, label, 'Cursors'),
    send(new(B, cursor_browser), below, D),
    send(F, slot, browser, B),
    send(F, fill_browser, standard).

fill_browser(F, Kind:{standard,x11,image}) :->
    "Fill the browser with the cursors of Kind"::
    get(F, browser, B),
    send(B, clear),
    forall(cursor_item(Kind, Name, Label, Cursor),
           ( send(B, append, new(DI, dict_item(Name, Label, Cursor))),
             get(Cursor, summary, Summary),
             (   Summary == @nil
             ->  true
             ;   send(DI, help_message, tag, Summary)
             )
           )),
    send(F, keyboard_focus, B).

kind(standard, 'Standard', 'The cursors provided by the system').
kind(x11,      'X11',      'Names kept for compatibility with the X11 \c
                            cursor font.  They map to a standard cursor').
kind(image,    'Image',    'Cursors made from an image').

%   cursor_item(+Kind, -Name, -Label, -Cursor) is nondet.
%
%   Cursor is a cursor of Kind, to be shown as Label.  The predefined
%   cursors are in @cursor_names; cursor <-kind tells where they
%   come from.

cursor_item(image, pce_image, '@pce_image', Cursor) :-
    !,
    get(@pce_image, scale, size(32,32), Image),
    new(Cursor, cursor(@nil, Image, point(16,16))),
    send(Cursor, summary, 'Made from @pce_image, scaled to 32x32').
cursor_item(Kind, Name, Name, Cursor) :-
    get(@cursor_names, attribute_names, Names),
    chain_list(Names, List),
    member(Name, List),
    get(@pce, convert, Name, cursor, Cursor),
    get(Cursor, kind, Kind).

:- pce_end_class(cursor_demo).


:- pce_begin_class(cursor_browser, browser,
                   "Browser that shows the cursor of the selection").

initialise(B) :->
    send_super(B, initialise, @default, size(30, 20)).

selection_changed(B) :->
    "Set my cursor from the selected item"::
    (   get(B, selection, DI)
    ->  get(DI, object, Cursor),
        send(B, cursor, Cursor)
    ;   send(B, cursor, @nil)
    ).

:- pce_end_class(cursor_browser).
