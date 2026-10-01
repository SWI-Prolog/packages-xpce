/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(pce_filter_item, []).
:- use_module(library(pce)).
:- autoload(library(pce_util), [default/3, pce_text_to_regex/2]).

/** <module> Filter as you type

Class `filter_item` is a text_item in which the user types a regular
expression that filters what some other window shows.  The filter is
applied as the text changes, so the user sees the effect of every key
they type, and of clearing the field using the button at its right.

    send(D, append,
         filter_item(filter,
                     message(Browser, filter, @arg1),
                     'Filter predicates'))

The message is called with @arg1 bound to a `regex` or, if the field
is empty, @nil.  While the text is not a valid regular expression, as
it is most of the time while typing ``[a-z]``, the message is not
called and the item reports ``Incomplete expression``.
*/

:- pce_begin_class(filter_item, text_item,
                   "Filter something as the user types").

variable(filter_message, code*, both,
         "Called with the regex or @nil when the text changes").

initialise(FI, Name:name=[name], Message:message=[code]*,
           Placeholder:placeholder=[char_array]) :->
    "Create from name, filter message and placeholder"::
    default(Name, filter, TheName),
    default(Message, @nil, TheMessage),
    send_super(FI, initialise, TheName),
    send(FI, filter_message, TheMessage),
    (   Placeholder == @default
    ->  true
    ;   send(FI, placeholder, Placeholder)
    ).

typed(FI, Id:'event|event_id') :->
    "Apply the filter after the key is processed"::
    send_super(FI, typed, Id),
    send(FI, apply_filter).

%       The clear button at my right sends ->clear, not ->typed.

clear(FI) :->
    "Clear the field and remove the filter"::
    send_super(FI, clear),
    send(FI, apply_filter).

%       <-displayed_value is the text of the field itself, which goes
%       on changing as the user types.  A regex made from that would
%       change with it, so it is made from a copy.

apply_filter(FI) :->
    "Call <-filter_message with what is typed"::
    get(FI?displayed_value, value, Text),
    (   Text == ''
    ->  send(FI, filter, @nil)
    ;   pce_text_to_regex(Text, Filter)
    ->  send(FI, filter, Filter)
    ;   ignore(send(FI, report, status, 'Incomplete expression'))
    ).

filter(FI, Filter:regex*) :->
    "Filter on Filter; @nil shows all"::
    (   get(FI, filter_message, Message),
        Message \== @nil
    ->  send(Message, forward, Filter)
    ;   true
    ).

:- pce_end_class(filter_item).
