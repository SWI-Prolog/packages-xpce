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

test(named_rgb_in_reverse_table, Same == true) :-
    new(C, colour(test_colour_named_rgb, 11, 12, 13)),
    new(Lookup, colour(@default, 11, 12, 13)),
    same(Lookup, C, Same),
    free(C).
test(theme_colour, [RGB == rgb(255,0,0), Kind == theme]) :-
    new(C, theme_colour(test_colour_theme, red)),
    colour_rgb(C, RGB),
    get(C, kind, Kind).
test(theme_colour_by_name, Same == true) :-
    new(C, theme_colour(test_colour_by_name, red)),
    get(@pce, convert, test_colour_by_name, colour, C2),
    same(C, C2, Same).
test(theme_colour_derived_from, RGB == rgb(0,0,255)) :-
    new(C, theme_colour(test_colour_derived_from, red)),
    colour_rgb(C, _),
    send(C, derived_from, blue),
    colour_rgb(C, RGB).
test(theme_colour_lookup, [Same == true, RGB == rgb(0,255,0)]) :-
    %  Creating an existing theme colour changes its value
    new(C, theme_colour(test_colour_lookup, red)),
    new(C2, theme_colour(test_colour_lookup, '#00ff00')),
    same(C, C2, Same),
    colour_rgb(C, RGB).
test(theme_colour_alias_first, [RGB1 == rgb(1,2,3), RGB2 == rgb(4,5,6)]) :-
    %  The value is resolved when needed, so an alias may be created
    %  before its target and follows changes of the target.
    new(A, theme_colour(test_colour_alias, test_colour_target)),
    new(T, theme_colour(test_colour_target, '#010203')),
    colour_rgb(A, RGB1),
    send(T, derived_from, '#040506'),
    colour_rgb(A, RGB2).
test(theme_colour_not_in_reverse_table, Same == false) :-
    new(C, theme_colour(test_colour_reverse, '#0e0f10')),
    colour_rgb(C, _),
    new(Lookup, colour(@default, 14, 15, 16)),
    same(Lookup, C, Same).
test(theme_colour_cycle, RGB == rgb(127,127,127)) :-
    new(C, theme_colour(test_colour_cycle1, test_colour_cycle2)),
    new(_, theme_colour(test_colour_cycle2, test_colour_cycle1)),
    pce_catch_error(cyclic_theme_colour, colour_rgb(C, RGB)).
test(theme_colour_follows_system, RGB == Orig) :-
    %  A theme colour derived from a system colour follows a reload
    new(C, theme_colour(test_colour_sys, sys_accent)),
    colour_rgb(C, Orig),
    get(@pce, convert, sys_accent, colour, Sys),
    get(Sys, rgba, SysRgba),
    send(@colour_names, append, sys_accent, 12345),
    send(Sys, slot, rgba, 12345),
    send(C, derived_from, white),       % resolve again from the
    send(C, derived_from, sys_accent),  % changed system colour
    colour_rgb(C, Stale),
    assertion(Stale \== Orig),
    send(@display_manager, system_colours_changed),
    colour_rgb(C, RGB),
    get(Sys, rgba, SysRgba).
test(mix, [Half == rgb(128,128,128), Same == rgb(0,0,0),
            Other == rgb(255,255,255)]) :-
    get(@pce, convert, black, colour, Black),
    get(@pce, convert, white, colour, White),
    get(Black, mix, White, Mid), colour_rgb(Mid, Half),
    get(Black, mix, White, 0.0, C0), colour_rgb(C0, Same),
    get(Black, mix, White, 1.0, C1), colour_rgb(C1, Other).
test(mix_down, RGB == rgb(229,229,229)) :-
    %  Mixing a light colour towards a dark one darkens it
    get(@pce, convert, white, colour, White),
    get(@pce, convert, black, colour, Black),
    get(White, mix, Black, 0.1, C),
    colour_rgb(C, RGB).
test(locked_colour_survives, [Plain == gone, Theme == alive]) :-
    %  A colour created as part of a graphical is freed with it.  A
    %  theme colour is locked and survives.
    gc_colour(colour(test_colour_plain, 1, 2, 3), test_colour_plain, Plain),
    gc_colour(theme_colour(test_colour_locked, red), test_colour_locked,
              Theme).
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

gc_colour(Term, Name, State) :-
    new(Box, box),
    send(Box, colour, Term),
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
