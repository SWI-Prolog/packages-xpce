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

:- module(test_text_item, [test_text_item/0]).
:- encoding(utf8).

/** <module> Tests for the clear button of a text_item

A text_item with a <-clear_image shows it at the right while there is
text to clear, and reserves the room for it whether or not it is showing
so that the field does not reflow as the user types.  A field that is not
<-editable cannot be cleared, so it neither shows it nor keeps the room.

The icon itself cannot be seen here -- the tests run against the SDL
dummy driver, which paints nothing -- so this reads the room it takes.

Run with:

    swipl -g test_text_item -t halt \
          packages/xpce/tests/test_text_item.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(pce_float_item)).
:- use_module(library(apply)).

test_text_item :-
    run_tests([ text_item_clear_button,
                text_item_step,
                float_item
              ]).

%       A subclass of our own: class text_item is what draws the field,
%       so anything built on it answers on the same terms.

:- pce_begin_class(test_clearable_item, text_item,
                   "A text_item that adds nothing").
:- pce_end_class(test_clearable_item).

%!  item_width(+Item, -Width) is det.
%
%   The width Item asks for.  ->compute rather than <-width alone: the
%   width is settled when the item is computed, and what asks for that
%   here is the test rather than a dialog laying itself out.

item_width(Item, Width) :-
    send(Item, compute),
    get(Item, width, Width).

:- begin_tests(text_item_clear_button).

test(a_field_reserves_room_for_the_button, Reserved =:= ImageWidth + Gap) :-
    new(T, text_item(name, 'some text')),
    item_width(T, Editable),
    send(T, editable, @off),
    item_width(T, ReadOnly),
    Reserved is Editable - ReadOnly,
    get(T, clear_image, Image),
    get(Image?size, width, ImageWidth),
    Gap is Reserved - ImageWidth,
    assertion(Gap > 0).                 % one ex between text and button

test(and_a_read_only_one_reserves_none, ReadOnly == Without) :-
    new(T, text_item(name, 'some text')),
    send(T, editable, @off),
    item_width(T, ReadOnly),
    new(T2, text_item(name, 'some text')),
    send(T2, clear_image, @nil),
    item_width(T2, Without).

%       Room is kept while the field is empty, so that the field does not
%       reflow when the first character arrives.

test(the_room_is_kept_while_there_is_nothing_to_clear, W == WithText) :-
    new(T, text_item(name, '')),
    item_width(T, W),
    new(T2, text_item(name, 'some text')),
    item_width(T2, WithText).

test(and_comes_back_when_the_field_may_be_typed_in_again, Again == Editable) :-
    new(T, text_item(name, 'some text')),
    item_width(T, Editable),
    send(T, editable, @off),
    item_width(T, _),
    send(T, editable, @on),
    item_width(T, Again).

test(a_subclass_answers_the_same_way) :-
    new(T, test_clearable_item(name, 'some text')),
    item_width(T, Editable),
    send(T, editable, @off),
    item_width(T, ReadOnly),
    assertion(Editable > ReadOnly).

:- end_tests(text_item_clear_button).

%!  steps(+Item, +Keys, -Values) is det.
%
%   Open a dialog holding Item (an object or a term to create one), type
%   Keys and collect the selection after each.

steps(Spec, Keys, Values) :-
    (   object(Spec)
    ->  Item = Spec
    ;   new(Item, Spec)
    ),
    new(D, dialog),
    send(D, append, Item),
    send(D, open),
    send(D, keyboard_focus, Item),
    foldl(step_key(D, Item), Keys, Values, []),
    send(D, destroy).

step_key(D, Item, Id, [V|T], T) :-
    new(Ev, event(Id, D, 0, 0, 0, 1000)),
    send(D, post_event, Ev),
    get(Item, selection, V).

:- begin_tests(text_item_step).

test(int_item, Values-Calls == [6,7,6,16,6,0]-[6,7,6,16,6,0]) :-
    new(Log, chain),
    steps(int_item(n, 5, message(Log, append, @arg1), 0, 100),
          [cursor_up, cursor_up, cursor_down, page_up, page_down, page_down],
          Values),
    chain_list(Log, Calls).
test(float_item_step, Values == [0.6, 0.5, 0.4, 1.4]) :-
    new(F, float_item(f, 0.5)),
    send(F, step, 0.1),
    steps(F, [cursor_up, cursor_down, cursor_down, page_up], Values).
test(float_item_back_to_start, Count == 2) :-
    new(Log, chain),
    new(F, float_item(f, 0.5, message(Log, append, @arg1))),
    send(F, step, 0.1),
    steps(F, [cursor_up, cursor_down], _),
    get(Log, size, Count).
test(float_item_range, Values == [0.52, 0.72]) :-
    steps(float_item(r, 0.5, @default, 0.0, 2.0), [cursor_up, page_up], Values).
test(float_item_limit, Values == [2.0, 2.0]) :-
    steps(float_item(r, 1.9, @default, 0.0, 2.0), [page_up, page_up], Values).
test(float_item_no_step, Values == [0.5]) :-
    steps(float_item(x, 0.5), [cursor_up], Values).
test(text_item_unchanged, Values == [abc]) :-
    steps(text_item(t, abc), [cursor_up], Values).

:- end_tests(text_item_step).

:- begin_tests(float_item).

test(exact_selection, V == 0.6) :-
    new(F, float_item(f, 0.6)),
    get(F, selection, V).

:- end_tests(float_item).
