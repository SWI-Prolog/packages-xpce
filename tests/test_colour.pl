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
    run_tests([ colour,
                colour_palette_item,
                theme_colour_summary,
                colour_item,
                ansi_colours_item
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
test(theme_colour_hsv, [H == 240, S == 100, V == 100]) :-
    %  colour<-value is the HSV value, also for a theme colour
    new(C, theme_colour(test_colour_hsv, blue)),
    get(C, hue, H0), H is round(H0),
    get(C, saturation, S0), S is round(S0),
    get(C, value, V0), V is round(V0).
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

test(colour_names_protected, [R == 250]) :- % kernel keeps pointers
    \+ free(@colour_names),
    \+ free(@colour_list),
    new(C, colour(salmon)),
    get(C, red, R).

:- end_tests(colour).


                 /*******************************
                 *     COLOUR PALETTE ITEM      *
                 *******************************/

:- pce_autoload(colour_palette_item, library(pce_colour_item)).
:- pce_autoload(colour_set_item, library(pce_colour_item)).
:- pce_autoload(colour_item, library(pce_colour_item)).
:- pce_autoload(ansi_colours_item, library(pce_colour_item)).
:- pce_autoload(colour_editor, library(pce_colour_editor)).

slider_values(Item, RGB) :-
    maplist(slider_value(Item), [red,green,blue], RGB).

slider_value(Item, Name, Value) :-
    get(Item, member, Name, Slider),
    get(Slider, selection, Value).

:- begin_tests(colour_palette_item).

test(sliders, [RGB == [255,128,0]]) :-
    new(I, colour_palette_item(c, colour(@default, 255, 128, 0))),
    slider_values(I, RGB).
test(sliders_drive_selection, [RGB == [10,20,30]]) :-
    new(I, colour_palette_item(c, red)),
    get(I, member, red, R), send(R, selection, 10),
    get(I, member, green, G), send(G, selection, 20),
    get(I, member, blue, B), send(B, selection, 30),
    send(I, slider_dragged),
    get(I, selection, C),
    maplist([S,V]>>get(C, S, V), [red,green,blue], RGB).
test(hex_shows_name, [Name == red]) :-
    new(I, colour_palette_item(c, colour(@default, 255, 0, 0))),
    get(I, member, name, NI),
    get(NI?value_text?string, value, Name).
test(set_item, [Names == [red,green]]) :-
    new(I, colour_set_item(p, chain(red, green))),
    get(I, selection, Chain),
    get(Chain, map, @arg1?name, NameChain),
    chain_list(NameChain, Names).

:- end_tests(colour_palette_item).

:- begin_tests(theme_colour_summary).

test(builtin, [true(sub_string(S, _, _, _, "focus"))]) :-
    get(@pce, convert, ui_accent, colour, C),
    get(C?summary, value, S0),
    atom_string(S0, S).
test(new, [S == 'Test role']) :-
    new(C, theme_colour(test_summary_colour, red, 'Test role')),
    get(C?summary, value, S).
test(kept, [S == 'Test role']) :-
    new(_, theme_colour(test_summary_colour2, red, 'Test role')),
    new(C, theme_colour(test_summary_colour2, blue)),
    get(C?summary, value, S).
test(none, [S == @nil]) :-
    new(C, theme_colour(test_summary_colour3, red)),
    get(C, summary, S).

:- end_tests(theme_colour_summary).


                 /*******************************
                 *          COLOUR ITEM         *
                 *******************************/

%   colour_item(-Item, +Initial, -Log)
%
%   Item in an open dialog.  Log collects the colours passed to the
%   message.

colour_item(Item, Initial, Log) :-
    new(Log, chain),
    new(D, dialog),
    send(D, append,
         new(Item, colour_item(c, Initial, message(Log, append, @arg1)))),
    send(D, open).

shown_name(Item, Name) :-
    get(Item, member, colour_name, Label),
    get(Label, selection, Name0),
    get(Name0, value, Name).

:- begin_tests(colour_item).

test(initial, [Name-Shown == ui_accent-ui_accent]) :-
    colour_item(I, ui_accent, _),
    get(I?selection, name, Name),
    shown_name(I, Shown).
test(selection_no_message, [Log == []]) :-
    colour_item(I, red, L),
    send(I, selection, blue),
    chain_list(L, Log).
test(user_selection_message, [Names == [blue]]) :-
    colour_item(I, red, L),
    send(I, user_selection, blue),
    get(L, map, @arg1?name, NC),
    chain_list(NC, Names).
test(theme_chooser, [Name == ui_link]) :-
    colour_item(I, ui_accent, _),
    new(Ch, pce_colour_item:theme_colour_chooser(
                I?selection, message(I, user_selection, @arg1))),
    get(Ch, member, browser, B),
    send(B, selection, ui_link),
    send(Ch, ok),
    get(I?selection, name, Name).
test(chooser_selects_current, [Key == ui_link]) :-
    get(@pce, convert, ui_link, colour, C),
    new(Ch, pce_colour_item:theme_colour_chooser(C, @nil)),
    get(Ch, member, browser, B),
    get(B?selection, key, Key),
    send(Ch, destroy).
test(editor_ok, [RGB == [0,128,255]]) :-
    new(L, chain),
    new(E, colour_editor(red, message(L, append, @arg1))),
    send(E, current_colour, colour(@default, 0, 128, 255)),
    send(E, ok),
    get(L, head, C),
    maplist([S,V]>>get(C, S, V), [red,green,blue], RGB).

test(editor_ok_closes, [Gone-Name == true-blue]) :-
    new(D, dialog),                     % as in the class variable editor
    send(D, append, new(G, dialog_group(row, group))),
    send(G, append, new(CI, colour_item(c, red))),
    send(D, open),
    new(E, colour_editor(red, message(CI, user_selection, @arg1))),
    send(E, open),
    send(E, current_colour, blue),
    send(E, ok),
    (object(E) -> Gone = false ; Gone = true),
    get(CI?selection, name, Name).

test(editor_use_named, [Name-Kind == salmon-named]) :-
    new(L, chain),
    new(E, colour_editor(colour(@default, 250, 130, 110),
                         message(L, append, @arg1))),
    get(E, member, named_1, Candidate),
    send(Candidate, use),
    send(E, ok),
    get(L, head, C),
    get(C, name, Name),
    get(C, kind, Kind).

:- end_tests(colour_item).


%   ansi_colours_item edits terminal_image<-ansi_colours, a vector of 16
%   colours.  @nil means the default (theme) colours.

colour_name(Item, Index, Name) :-
    get(Item, colour, Index, Colour),
    get(Colour, name, Name).

:- begin_tests(ansi_colours_item).

test(defaults, [Squares-First-Last == 16-ansi_black-ansi_bright_white]) :-
    new(I, ansi_colours_item(ansi, @nil)),
    get(I?graphicals, find_all,
        message(@arg1, instance_of, ansi_colour_swatch), Swatches),
    get(Swatches, size, Squares),
    colour_name(I, 1, First),
    colour_name(I, 16, Last).
test(tooltip, Tip == "Bright red (default ansi_bright_red)") :-
    new(I, ansi_colours_item(ansi, @nil)),
    get(I?graphicals, find, @arg1?index == 10, Swatch),
    get(Swatch, help_message, tag, String),
    get(String, value, Tip0),
    atom_string(Tip0, Tip).
test(edit, [Size-Edited-Other-Sent == 16-orange-ansi_red-orange]) :-
    new(Log, chain),
    new(I, ansi_colours_item(ansi, @nil,
                             message(Log, append, @arg1))),
    send(I, user_colour, 3, colour(orange)),
    get(I, selection, V),
    get(V, size, Size),
    colour_name(I, 3, Edited),
    colour_name(I, 2, Other),
    get(Log, head, Sent0),
    get(Sent0, element, 3, SentColour),
    get(SentColour, name, Sent).

:- end_tests(ansi_colours_item).
