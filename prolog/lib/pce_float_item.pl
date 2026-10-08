/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2002-2011, University of Amsterdam
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

:- module(pce_float_item, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Text-item specialised for editing floating   point (real) numbers. Usage
should be quite straigh-forward.

See also the built-in class int_item.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(float_item, text_item).

variable(low,           num*,         both, "Lowest value").
variable(high,          num*,         both, "highest value").
variable(format,        name, get,  "How values are formatted").
variable(allow_default, bool := @off, get,  "'' <-> @default").
variable(step,          num*,         get,  "Step for up/down").
variable(apply_step,    bool := @on,  both, "Apply stepping immediately").

class_variable(format, name,  '%g').
class_variable(step,   num*,  @nil).

initialise(RI, Label:label=name, Default:default=[num], Msg:message=[code]*,
           Low:low=[num], High:high=[num]*) :->
    send_super(RI, initialise, Label, '', Msg),
    send(RI, type, num),
    send(RI, length, 10),
    send(RI, style, normal),
    (   Low \== @default
    ->  send(RI, low, Low)
    ;   true
    ),
    (   High \== @default
    ->  send(RI, high, High)
    ;   true
    ),
    (   Default \== @default
    ->  send(RI, selection, Default)
    ;   true
    ),
    (   get(RI, step, Step),
        Step \== @nil
    ->  send(RI, step, Step)
    ;   true
    ).


format(RI, Fmt:name) :->
    send(RI, slot, format, Fmt),
    (   get(RI, slot, selection, Sel),
        number(Sel)
    ->  send(RI, selection, Sel)
    ;   true
    ).


allow_default(RI, Val:bool) :->
    send(RI, slot, allow_default, Val),
    (   Val == @on
    ->  send(RI, type, '[num]')
    ;   send(RI, type, num)
    ).


selection(RI, Sel:[num]) :<-
    get(RI?value_text, string, Text),
    (   text_float(Text, Sel)
    ->  get(RI, low, Low),
        get(RI, high, High),
        (   (   Low == @nil
            ->  true
            ;   Sel >= Low
            ),
            (   High == @nil
            ->  true
            ;   Sel =< High
            )
        ->  true
        ;   (   High == @nil
            ->  send(RI, report, error, 'Minimum value is %g', Low)
            ;   send(RI, report, error,
                     'Value out of range (%g .. %g)', Low, High)
            ),
            fail
        )
    ;   get(RI, allow_default, @on),
        new(S, string('%s', Text)),
        send(S, strip),
        get(S, size, 0)
    ->  Sel = @default
    ;   send(RI, report, error,
             'Please enter a valid number'),
        fail
    ).


clear(RI) :->
    send(RI, string, ''),
    send(RI, slot, selection, @default).


value(RI, Value:num) :->
    "Set the displayed value"::
    get(RI, format, Fmt),
    send(RI, string, string(Fmt, Value)).


value(RI, Value:num) :<-
    "The displayed value"::
    get(RI, string, Text),
    text_float(Text, Value).

%   text_float(+Text, -Float) is semidet.
%
%   Float is the number Text holds as a float.  The item edits a real
%   number, also if it is integral.

text_float(Text, Float) :-
    get(@pce, convert, Text, num, Num),
    Float is float(Num).


selection(RI, Sel:[num]) :->
    (   Sel == @default
    ->  send(RI, clear)
    ;   get(RI, format, Fmt),
        send(RI, string, string(Fmt, Sel)),
        send(RI, modified, @off),
        send(RI, slot, selection, Sel)
    ).

type(RI, Type:type) :->
    "Set type, updating <-low and <-high"::
    (   (   get(Type, kind, real_range)
        ->  get(Type, context, tuple(Low, High)),
            send(RI, low, Low),
            send(RI, high, High)
        ;   (   send(Type, includes, num)
            ;   send(Type, includes, real)
            )
        ->  send(RI, low, @nil),
            send(RI, high, @nil)
        )
    ->  send_super(RI, type, Type)
    ;   send(Type, error, domainError, num)
    ),
    (   get(RI, step, @nil)
    ->  send(RI, style, normal)
    ;   send(RI, style, stepper)
    ).

step(RI, Step:num*, Apply:[bool]) :->
    send(RI, slot, step, Step),
    (   Step == @nil
    ->  send(RI, style, normal)
    ;   send(RI, style, stepper)
    ),
    (   Apply \== @default
    ->  send(RI, apply_step, Apply)
    ;   true
    ).


increment(RI) :->
    "Handle stepper"::
    get(RI, step, Step),
    Step \== @nil,
    get(RI, value, Now),
    (   get(RI, high, High), High \== @nil
    ->  NewVal is min(High, Now+Step)
    ;   NewVal is Now+Step
    ),
    send(RI, value, NewVal),
    (   get(RI, apply_step, @on)
    ->  send(RI, apply)
    ;   true
    ).

decrement(RI) :->
    "Handle stepper"::
    get(RI, step, Step),
    Step \== @nil,
    get(RI, value, Now),
    (   get(RI, low, Low), Low \== @nil
    ->  NewVal is max(Low, Now-Step)
    ;   NewVal is Now-Step
    ),
    send(RI, value, NewVal),
    (   get(RI, apply_step, @on)
    ->  send(RI, apply)
    ;   true
    ).


:- pce_end_class(float_item).


