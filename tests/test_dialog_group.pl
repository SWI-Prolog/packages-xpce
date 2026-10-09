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


:- module(test_dialog_group, [test_dialog_group/0]).

/** <module> Test the layout of class dialog_group

Run with:

    swipl -g test_dialog_group -t halt packages/xpce/tests/test_dialog_group.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_dialog_group :-
    run_tests([ dialog_group
              ]).

%   box_dialog(-Dialog, -Group, -Item)
%
%   A dialog holding a box group `address` with a text item.

box_dialog(D, G, I) :-
    new(D, dialog),
    send(D, append, new(G, dialog_group(address, box))),
    send(G, append, new(I, text_item(street))),
    send(D, layout).

%   item_below_label(+Group, +Item, -Space)
%
%   Space is the room between the bottom of the label of Group and the
%   top of Item.

item_below_label(G, I, Space) :-
    get(G, position, point(_, GY)),
    get(I, absolute_position, G?device, point(_, IY)),
    get(G, label_font, Font),
    get(Font, height, LH),
    Space is IY - (GY + LH).

:- begin_tests(dialog_group).

test(label_above_by_default, Format == above) :-
    box_dialog(D, G, _),
    get(G, label_format, Format),
    send(D, destroy).
test(item_clear_of_label, true(Space >= 10)) :-
    box_dialog(D, G, I),
    item_below_label(G, I, Space),
    send(D, destroy).
test(group_has_no_label_room, true(IY - GY < 3)) :-
    new(D, dialog),
    send(D, append, new(G, dialog_group(options, group))),
    send(G, append, new(I, text_item(street))),
    send(D, layout),
    get(G, position, point(_, GY)),
    get(I, absolute_position, D, point(_, IY)),
    send(D, destroy).

:- end_tests(dialog_group).
