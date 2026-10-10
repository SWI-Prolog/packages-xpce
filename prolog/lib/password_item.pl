/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
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

:- module(pce_password_item,
          [
          ]).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This class realises a GUI password  item, visualising the typed password
as a row of bullets. The returned value   is  an XPCE string to avoid the
password entering the XPCE symbol table where it would be much easier to
find.

The password is edited in an invisible  shadow text_item.  The visible
item shows the bullets and does  everything else: Return applies it, Tab
advances and the clear icon clears it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


                 /*******************************
                 *       CLASS PASSWD_ITEM      *
                 *******************************/

:- pce_begin_class(password_item, text_item, "text-item for entering a passwd").

variable(shadow,        text_item,      get, "The real (invisible) item").

initialise(I, Name:[name], Message:[message]) :->
    default(Name, password, TheName),
    send_super(I, initialise, TheName, string(''), Message),
    send(I, slot, shadow, text_item(TheName, string(''))).


unlink(I) :->
    get(I, shadow, Shadow),
    free(Shadow),
    send_super(I, unlink).


%   Keys that edit the text go to the invisible shadow.  Other keys
%   (Return, Tab) are for the visible item.  So are mouse events, as
%   that is where they happened; the shadow follows its caret.  The
%   shadow is not displayed, so it cannot locate a mouse event.  Both
%   items get the other events, notably those about the keyboard focus.

event(I, Ev:event) :->
    get(I, shadow, Shadow),
    (   send(Ev, is_a, keyboard)
    ->  (   item_key(Ev)
        ->  send_super(I, event, Ev)
        ;   copy_key(Ev)
        ->  send(I, alert)              % never put the password on the
        ;   send(Shadow, event, Ev),    % clipboard
            send(I, update)
        )
    ;   send(Ev, is_a, mouse)
    ->  (   send_super(I, event, Ev)
        ->  Done = true
        ;   Done = false
        ),
        get(I, caret, Caret),
        send(Shadow, caret, Caret),
        Done == true
    ;   ignore(send(Shadow, event, Ev)),    % focus events
        send_super(I, event, Ev)
    ).

item_key(Ev) :-
    key_function(Ev, Function),
    memberchk(Function, [enter, next, previous]).

copy_key(Ev) :-
    key_function(Ev, Function),
    memberchk(Function, [copy, cut, prefix_or_copy, prefix_or_cut]).

key_function(Ev, Function) :-
    get(key_binding(text_item), function, Ev, Function).


update(I) :->
    "Update visual representation"::
    get(I, shadow, Shadow),
    get(Shadow, displayed_value, String),  % <-selection resets <-modified
    get(Shadow, caret, Caret),
    get(String, size, Size),
    bullet_string(Size, Bullets),
    send_super(I, displayed_value, Bullets),
    send(I, caret, Caret),
    copy_selection(Shadow, I),
    (   get(Shadow, modified, @on),
        get(I, device, Dev),
        Dev \== @nil
    ->  ignore(send(Dev, modified_item, I, @on))
    ;   true
    ).


%   The bullets are one per character, so the selection of the shadow
%   (e.g., after select-all or Shift-Left) is shown at the same place.

copy_selection(From, To) :-
    get(From, value_text, FT),
    get(To, value_text, TT),
    (   get(FT, selection, point(S, E))
    ->  send(TT, selection, S, E)
    ;   send(TT, selection, @nil)
    ).


selection(I, Passwd:string) :<-
    get(I, shadow, Shadow),
    get(Shadow, selection, Passwd).

selection(I, Passwd:string) :->
    get(I, shadow, Shadow),
    send(Shadow, selection, Passwd),
    get(Passwd, size, Size),
    bullet_string(Size, Bullets),
    send_super(I, selection, Bullets),
    send(I, update).

modified(I, Modified:bool) :<-
    "True if the password was edited"::
    get(I, shadow, Shadow),
    get(Shadow, modified, Modified).

modified(I, Modified:bool) :->
    get(I, shadow, Shadow),
    send(Shadow, modified, Modified).

apply(I, Always:[bool]) :->
    "Send the message with the password if it was edited"::
    get(I, message, Msg),
    send(Msg, instance_of, code),
    (   Always == @on
    ->  true
    ;   get(I, modified, @on)
    ),
    get(I, selection, Passwd),
    send(Msg, forward_receiver, I, Passwd),
    send(I, modified, @off).

clear(I) :->
    "Clear the password"::
    get(I, shadow, Shadow),
    send(Shadow, clear),
    send_super(I, clear),
    send(I, update).

bullet_string(Size, S) :-
    new(S, string),
    forall(between(1, Size, _), send(S, append, '\u25CF')).

:- pce_end_class(password_item).
