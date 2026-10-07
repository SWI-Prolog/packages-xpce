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

:- module(pce_colour_editor, []).
:- use_module(library(pce)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(help_message)).

/** <module> Edit a colour using the HSV or RGB model

Class colour_editor is a dialog to edit a colour.  It shows the colour
as hue, saturation and value as well as red, green and blue sliders,
together with the named colours closest to it.  If it has a message,
OK and Apply call it with the colour.

```
?- new(E, colour_editor(red, message(@prolog, writeln, @arg1))),
   send(E, open).
```
*/

:- pce_begin_class(colour_editor, dialog,
                   "Edit a colour in the HSV or RGB model").

variable(current_colour, colour,  get, "Current colour value").
variable(message,        code*,   both, "Called with the colour on OK/Apply").

item('H', hue,         0-360).
item('S', saturation, 0-100).
item('V', value,       0-100).
item('R', red,         0-255).
item('G', green,       0-255).
item('B', blue,        0-255).

initialise(D, Init:[colour], Message:[code]*) :->
    send_super(D, initialise, 'Edit colour'),
    default(Message, @nil, Msg),
    send(D, message, Msg),
    send(D, append, colour_candidate(hex, ' Exact: ')),
    send(D, append, colour_candidate(named_1, ' Named 1: ')),
    send(D, append, colour_candidate(named_2, ' Named 2: ')),
    send(D, append, colour_candidate(named_3, ' Named 3: ')),
    forall(item(Label, Selector, Low-High),
           append_slider(D, Label, Selector, Low, High)),
    (   Msg == @nil
    ->  send(D, append, button(close))
    ;   send(D, append, button(ok)),
        send(D, append, button(apply)),
        send(D, append, button(cancel))
    ),
    send(D, resize_message, message(D, layout, @arg2)),
    (   Init \== @default
    ->  send(D, current_colour, Init)
    ;   send(D, current_colour, @display?foreground)
    ).

%   slider_help(?Selector, ?Help)
%
%   Help text (tooltip) for the slider that sets Selector.

slider_help(hue,        'Hue: the colour on the colour wheel (0-360 degrees)').
slider_help(saturation, 'Saturation: from grey (0) to the pure colour (100)').
slider_help(value,      'Value: from black (0) to full brightness (100)').
slider_help(red,        'Red component (0-255)').
slider_help(green,      'Green component (0-255)').
slider_help(blue,       'Blue component (0-255)').

append_slider(D, Label, Selector, Low, High) :-
    send(D, append,
         new(Slider, slider(Label, Low, High, Low,
                            message(D, Selector, @arg1)))),
    send(Slider, drag, @on),
    send(Slider, attribute, hor_stretch, 100),
    send(Slider, width, 300),
    slider_help(Selector, Help),
    send(Slider, help_message, tag, Help).

close(D) :->
    "Close the editor"::
    send(D, destroy).

cancel(D) :->
    "Close the editor without applying"::
    send(D, destroy).

apply(D) :->
    "Call <-message with the current colour"::
    get(D, current_colour, C),
    get(D, message, Msg),
    forward_colour(Msg, C).

forward_colour(@nil, _) :- !.
forward_colour(Msg, C) :-
    ignore(send(Msg, forward, C)).

ok(D) :->
    "Close and call <-message with the current colour"::
    get(D, current_colour, C),
    get(D, message, Msg),
    send(D, destroy),
    forward_colour(Msg, C).

:- pce_group(update).

current_colour(D, C:colour, From:[{rgb,hsv}]) :->
    "Set the current colour, updating all items"::
    send(D, slot, current_colour, C),
    (   From \== hsv
    ->  update(D, 'H', C, hue),
        update(D, 'S', C, saturation),
        update(D, 'V', C, value)
    ;   true
    ),
    (   From \== rgb
    ->  update(D, 'R', C, red),
        update(D, 'G', C, green),
        update(D, 'B', C, blue)
    ;   true
    ),
    send(D, show, hex, C),
    send(D, show_named, C).

show(D, As:name, C:colour) :->
    "Show colour in item named As"::
    get(D, member, As, Item),
    send(Item, value, C).

show_named(D, C:colour) :->
    "Show close named colour"::
    closest_named_colours(C, 3, [C1,C2,C3]),
    send(D, show, named_1, C1),
    send(D, show, named_2, C2),
    send(D, show, named_3, C3).

update(D, Name, Colour, Selector) :-
    get(Colour, Selector, Value),
    get(D, member, Name, Item),
    send(Item, selection, Value).

value(D, Selector:name, Val) :<-
    "Get value from corresponding item"::
    item(ItemName, Selector, _),
    get(D, member, ItemName, Item),
    get(Item, selection, Val).

:- pce_group(hsv).

hue(D, H:'0..360') :->
    H2 is min(H, 359),
    get(D, value, saturation, S),
    get(D, value, value, V),
    send(D, current_colour, colour(@default, H2, S, V, model := hsv), hsv).

saturation(D, S:'0..100') :->
    get(D, value, hue, H),
    get(D, value, value, V),
    send(D, current_colour, colour(@default, H, S, V, model := hsv), hsv).

value(D, V:'0..100') :->
    get(D, value, hue, H),
    get(D, value, saturation, S),
    send(D, current_colour, colour(@default, H, S, V, model := hsv), hsv).

:- pce_group(rgb).

red(D, R:'0..255') :->
    get(D, value, green, G),
    get(D, value, blue, B),
    send(D, current_colour, colour(@default, R, G, B), rgb).

green(D, G:'0..255') :->
    get(D, value, red, R),
    get(D, value, blue, B),
    send(D, current_colour, colour(@default, R, G, B), rgb).

blue(D, B:'0..255') :->
    get(D, value, red, R),
    get(D, value, green, G),
    send(D, current_colour, colour(@default, R, G, B), rgb).

:- pce_end_class(colour_editor).


:- pce_begin_class(colour_candidate, dialog_group,
                   "Show a colour with its name").

variable(candidate_colour, colour*, get, "Colour shown").

%   The exact colour can be copied as a name.  The named colours close
%   to it can be selected, after which OK and Apply pass the named
%   colour.

initialise(Candidate, Name:name, Label:name) :->
    send_super(Candidate, initialise, Name, group),
    send(Candidate, slot, candidate_colour, @nil),
    (   Name == hex
    ->  send(Candidate, append, new(Btn, button(copy))),
        send(Btn, label, image('tool/copy.svg')),
        send(Btn, help_message, tag, 'Copy the name of the colour')
    ;   send(Candidate, append, new(Btn, button(use))),
        send(Btn, label, image('tool/pipette.svg')),
        send(Btn, help_message, tag, 'Use this named colour')
    ),
    send(Candidate, append, new(Nme, text('#xxxxxx')), right),
    send(Candidate, append, new(Txt,  text(Label)), right),
    send(Candidate, append, new(Box,  box(100, 20)), right),
    send(Box, alignment, right),
    send(Btn, alignment, left),
    send(Nme, alignment, left),
    send(Nme, name, colour_name),
    send(Txt, alignment, right),
    send(Txt, name, label),
    send(Txt, background, @display?background),
    send(Candidate, attribute, hor_stretch, 100).

value(Candidate, Value:colour) :->
    "Set the selected colour"::
    send(Candidate, slot, candidate_colour, Value),
    get(Candidate, member, box, Box),
    send(Box, fill, Value),
    get(Candidate, member, colour_name, Text1),
    send(Text1, string, Value?name),
    get(Candidate, member, label, Text2),
    send(Text2, colour, Value).

copy(Candidate) :->
    "Copy current selection as colour name"::
    get(Candidate, member, colour_name, Text),
    send(@display, copy, Text?string).

use(Candidate) :->
    "Make our (named) colour the current colour of the editor"::
    get(Candidate, candidate_colour, Colour),
    Colour \== @nil,
    send(Candidate?device, current_colour, Colour).

:- pce_end_class(colour_candidate).

%!  closest_named_colours(+Colour, +Count, -Names) is det.
%
%   Names are the Count named colours closest to Colour.

closest_named_colours(From, Count, Closest) :-
    chain_list(@colour_list, Names),
    maplist(colour_distance(From), Names, Distances),
    pairs_keys_values(Pairs, Distances, Names),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, ByDistance),
    length(Closest, Count),
    append(Closest, _, ByDistance).

colour_distance(Colour, Name, Distance) :-
    get(@colour_names, member, Name, RGB),
    get(Colour, distance, RGB, Distance).
