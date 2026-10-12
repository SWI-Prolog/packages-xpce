/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
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

:- module(man_error_browser, []).

:- use_module(library(pce)).
:- pce_autoload(filter_item, library(pce_filter_item)).
:- use_module(library(help_message), []).

:- pce_begin_class(man_error_browser, man_frame,
                   "Error browser").

variable(errors,        man_module,             get).
%   Not `current`: that is the window of my tab (`tabbed_window
%   <-current`), which gets the input focus.

variable(current_error, error*,                 get,
         "The selected error").
variable(original_kinds, hash_table := new(hash_table), get,
         "Error -> its <-kind before the user changed it").

initialise(EB, Manual:man_manual) :->
    "Create from Manual"::
    send(EB, send_super, initialise, Manual, 'Error Browser'),

    get(Manual, module, errors, @on, Module),
    send(EB, slot, errors, Module),

    new(B, man_summary_browser(man_summary, size(100, 15))),
    error_tab_stops(B, Stops),
    send(B?text_image, tab_stops, Stops),
    send(B, name, browser),
    dialog(Dialog),

    send(EB, append, B),
    send(Dialog, below, B),
    send(EB, fill, @nil),

    send(EB, open).


%   error_tab_stops(+Browser, -Stops)
%
%   Tab stops for the columns of <-man_summary of an error: kind
%   marker, id, book, kind and message.  They are computed from the
%   widest id and kind in the font of Browser.

error_tab_stops(B, vector(Id, Book, Kind, Message)) :-
    get(B, font, Font),
    new(Ids, chain),
    send(@errors, for_all, message(Ids, append, @arg1)),
    max_width(Font, Ids, IdW),
    get(class(error), instance_variable, kind, Var),
    get(Var?type, context, Kinds),
    max_width(Font, Kinds, KindW),
    get(Font, width, '\x1F4D6\', BookW),
    Id = 15,
    Book is Id+IdW+10,
    Kind is Book+BookW+10,
    Message is Kind+KindW+10.

max_width(Font, Texts, Max) :-
    new(M, number(0)),
    send(Texts, for_all, message(M, maximum, ?(Font, width, @arg1))),
    get(M, value, Max).

                /********************************
                *            DIALOG             *
                ********************************/

dialog(D) :-
    new(D, dialog),
    new(EB, ?(D, container, man_frame)),
    send(D, append,
         filter_item(search, message(EB, fill, @arg1), 'Search')),
    send(D, append,
         new(T, menu(change_kind, cycle, message(EB, change_kind, @arg1))),
         right),
    get(class(error), instance_variable, kind, Var),
    get(Var, type, Type),
    get(Type, context, TypeNames),
    send(TypeNames, for_all, message(T, append, @arg1)),
    send(T, active, @off),
    send(T, help_message, tag,
         'How the selected error is reported.  For debugging, make a\n\c
          warning an error to raise an exception, or make an error ignored.\n\c
          The change lasts until Prolog is restarted.'),
    send(D, append, button(quit, message(EB, quit)), right).

%   ->change_kind is called from the Kind menu.  It says what changed,
%   as it changes how xpce behaves until Prolog is restarted.

change_kind(EB, Kind:name) :->
    "Change the <-kind of the selected error"::
    get(EB, current_error, Error),
    Error \== @nil,
    get(Error, kind, Old),
    get(EB, original_kinds, Originals),
    (   get(Originals, member, Error, Original)
    ->  true
    ;   Original = Old,
        send(Originals, append, Error, Old)
    ),
    send(Error, kind, Kind),
    get(EB, member, browser, B),
    send(B, update, Error),
    get(Error, id, Id),
    (   Kind == Original
    ->  send(EB, report, inform, '%s is reported as %s again', Id, Kind)
    ;   send(EB, report, inform,
             '%s is reported as %s instead of %s until Prolog is restarted',
             Id, Kind, Original)
    ).


                /********************************
                *             FILL              *
                ********************************/

%   The search field calls this as the user types.

fill(EB, Pattern:regex*) :->
    "Fill with all errors matching pattern (@nil: all)"::
    get(EB, member, browser, B),
    send(B, clear),
    new(Chain, chain),
    (   Pattern == @nil
    ->  send(@errors, for_all, message(Chain, append, @arg2))
    ;   send(@errors, for_all,
             if(message(Pattern, search, @arg2?man_summary),
                message(Chain, append, @arg2)))
    ),
    send(Chain, sort, ?(@arg1?id, compare, @arg2?id)),
    send(Chain, for_all, message(B, append_card, @arg1)),
    ignore(send(EB, selected, EB?current_error)).


                /********************************
                *          COMMUNICATION        *
                ********************************/

change_kind_menu(EB, KindMenu:menu) :<-
    "Get the change kind menu"::
    get(?(EB, member, dialog), member, change_kind, KindMenu).

selected(EB, Obj:object*) :->
    "Set the selection"::
    send(Obj, instance_of, error),
    send(EB, slot, current_error, Obj),
    get(EB, change_kind_menu, KindMenu),
    send(KindMenu, selection, Obj?kind),
    send(KindMenu, active, @on),
    get(EB, member, browser, B),
    send(B, selected, Obj).


release_selection(EB) :->
    "Clear the selection"::
    get(EB, member, browser, B),
    send(B, release_selection),
    send(EB?change_kind_menu, active, @off).

:- pce_end_class.

