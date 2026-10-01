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
