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

:- module(test_colour,
          [ test_colour/0
          ]).
:- use_module(library(pce)).
:- use_module(library(plunit)).

/** <module> Test the colour tables of xpce

A Colour registers itself in @colours, by name, and in @rgba, by its
encoded RGBA value.  Both tables hold their members with `refer' none, so
neither keeps the Colour alive and a Colour that goes away has to take
its entries out itself.  An entry left behind hands the next lookup an
object that is no longer there: `<-lookup', and with it `new/2' on a
colour, `Image <-pixel' and the terminal palette all read these tables.
*/

test_colour :-
    run_tests([ colour
              ]).

:- begin_tests(colour).

test(free_clears_the_tables) :-
    new(C, colour(@default, 1, 2, 3)),
    get(C, name, Name),
    get(C, rgba, Rgba),
    assertion(get(@colours, member, Name, C)),
    assertion(get(@rgba, member, Rgba, C)),
    free(C),
    assertion(\+ get(@colours, member, Name, _)),
    assertion(\+ get(@rgba, member, Rgba, _)).

test(lookup_after_free) :-
    %  The reverse table used to keep the freed Colour, so this handed
    %  the lookup an object whose memory had been handed out again.
    new(C, colour(@default, 1, 2, 3)),
    free(C),
    forall(between(1, 1000, I),
           ( Red is I mod 200 + 10,
             new(X, colour(@default, Red, 7, 9)),
             free(X)
           )),
    new(C2, colour(@default, 1, 2, 3)),
    get(C2, red, R),
    get(C2, green, G),
    get(C2, blue, B),
    assertion(rgb(R,G,B) == rgb(1,2,3)),
    free(C2).

test(system_colours) :-
    %  The sys_* names are defined on all platforms; see the userguide,
    %  section "System colours".
    get(@pce, convert, white, colour, _),       % load the name table
    forall(sys_colour(Name),
           assertion(get(@colour_names, member, Name, _))).

test(reload_system_colours) :-
    %  ->system_colours_changed restores the table entry and updates
    %  the existing Colour object, which is what drawing uses.
    get(@pce, convert, sys_accent, colour, C),
    get(C, rgba, Orig),
    send(@colour_names, append, sys_accent, 12345),
    send(C, slot, rgba, 12345),
    send(@display_manager, system_colours_changed),
    get(C, rgba, New),
    get(@colour_names, member, sys_accent, InTable),
    assertion(New == Orig),
    assertion(InTable == Orig).

test(set_rgba, RGB == rgb(255,0,0)) :-
    new(C, colour(test_colour_set, 1, 2, 3)),
    send(C, access, both),
    send(C, rgba, red),
    colour_rgb(C, RGB),
    free(C).
test(set_rgba_from_int, RGB == rgb(4,5,6)) :-
    new(C, colour(test_colour_set_int, 1, 2, 3)),
    send(C, access, both),
    get(@pce, convert, '#040506', colour, From),
    get(From, rgba, Rgba),
    send(C, rgba, Rgba),
    colour_rgb(C, RGB),
    free(C).
test(set_rgba_read_only, [RGB == rgb(1,2,3), Access == read]) :-
    new(C, colour(test_colour_read_only, 1, 2, 3)),
    get(C, access, Access),
    \+ pce_catch_error(read_only, send(C, rgba, red)),
    colour_rgb(C, RGB),
    free(C).
test(read_only_in_reverse_table, Same == true) :-
    new(C, colour(test_colour_read_only_rev, 11, 12, 13)),
    new(Lookup, colour(@default, 11, 12, 13)),
    same(Lookup, C, Same),
    free(C).
test(read_write_not_in_reverse_table, Same == false) :-
    %  A read/write colour may change its value, so it must not be the
    %  answer to looking up a colour from its RGBA.
    new(C, colour(test_colour_read_write, 14, 15, 16)),
    send(C, access, both),
    new(Lookup, colour(@default, 14, 15, 16)),
    same(Lookup, C, Same),
    free(C).
test(set_rgba_leaves_reverse_table, [OldSame == false, NewSame == false]) :-
    new(C, colour(@default, 21, 22, 23)),
    send(C, access, both),
    get(C, rgba, OldRgba),
    send(C, rgba, colour(@default, 24, 25, 26)),
    get(C, rgba, NewRgba),
    ( get(@rgba, member, OldRgba, Old) -> true ; Old = none ),
    ( get(@rgba, member, NewRgba, New) -> true ; New = none ),
    same(Old, C, OldSame),
    same(New, C, NewSame),
    free(C).
test(read_only_again_in_reverse_table, Same == true) :-
    new(C, colour(test_colour_back_to_read, 17, 18, 19)),
    send(C, access, both),
    send(C, access, read),
    get(C, rgba, Rgba),
    get(@rgba, member, Rgba, Found),
    same(Found, C, Same),
    free(C).
test(locked_colour_survives, [Unlocked == gone, Locked == alive]) :-
    %  A colour created as part of a graphical is freed with it, unless
    %  it is locked.  Theme colours are locked.
    gc_colour(test_colour_unlocked, false, Unlocked),
    gc_colour(test_colour_locked, true, Locked),
    get(@colours, member, test_colour_locked, C),
    send(C, lock_object, @off),
    free(C).
test(system_colours_message, Called == true) :-
    nb_setval(test_colour_called, false),
    setup_call_cleanup(
        send(@display_manager, system_colours_message,
             message(@prolog, nb_setval, test_colour_called, true)),
        send(@display_manager, system_colours_changed),
        send(@display_manager, system_colours_message, @nil)),
    nb_getval(test_colour_called, Called).

same(X, Y, Same) :-
    (   X == Y
    ->  Same = true
    ;   Same = false
    ).

colour_rgb(C, rgb(R,G,B)) :-
    get(C, red, R),
    get(C, green, G),
    get(C, blue, B).

gc_colour(Name, Lock, State) :-
    new(Box, box),
    send(Box, colour, colour(Name, 1, 2, 3)),
    (   Lock == true
    ->  get(Box, colour, C),
        send(C, lock_object, @on)
    ;   true
    ),
    free(Box),
    (   get(@colours, member, Name, _)
    ->  State = alive
    ;   State = gone
    ).

sys_colour(sys_window_background).
sys_colour(sys_window_foreground).
sys_colour(sys_dialog_background).
sys_colour(sys_dialog_foreground).
sys_colour(sys_button_background).
sys_colour(sys_button_foreground).
sys_colour(sys_button_pressed).
sys_colour(sys_selection_background).
sys_colour(sys_selection_foreground).
sys_colour(sys_tooltip_background).
sys_colour(sys_tooltip_foreground).
sys_colour(sys_inactive).
sys_colour(sys_link).
sys_colour(sys_accent).
sys_colour(sys_separator).
sys_colour(sys_shadow).

:- end_tests(colour).
