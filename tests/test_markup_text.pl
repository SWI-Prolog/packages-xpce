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

:- module(test_markup_text, [test_markup_text/0]).

/** <module> Tests for class markup_text, graphical->draw_markup and class markup

Run with:

    swipl -g test_markup_text -t halt \
          packages/xpce/tests/test_markup_text.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_markup_text :-
    run_tests([ markup_text,
                markup,
                label_markup,
                list_browser_markup,
                decorator_markup,
                editor_markup
              ]).

red(0xffff0000).
white(0xffffffff).

%   Markup for a solid red block: spaces with a red background.

red_block("<span background='#ff0000'>      </span>").

%!  pixel(+Graphical, +W, +H, +X, +Y, -RGBA) is det.

pixel(Gr, W, H, X, Y, RGBA) :-
    new(I, image(@nil, W, H)),
    send(I, draw_in, Gr),
    get(I, pixel, X, Y, C),
    get(C, rgba, RGBA),
    free(I).

%!  pixels(+Graphical, +W, +H, -Set) is det.
%
%   Set is the set of all RGBA values in the drawing.

pixels(Gr, W, H, Set) :-
    new(I, image(@nil, W, H)),
    send(I, draw_in, Gr),
    W1 is W-1, H1 is H-1,
    findall(RGBA, ( between(0, W1, X), between(0, H1, Y),
                    get(I, pixel, X, Y, C), get(C, rgba, RGBA)
                  ), List),
    sort(List, Set),
    free(I).

size(Gr, W-H) :-
    get(Gr?area, width, W),
    get(Gr?area, height, H).

%   A graphical that draws a red block using ->draw_markup.

:- pce_begin_class(tmt_drawer, graphical,
                   "Draw markup using ->draw_markup").

initialise(G) :->
    send_super(G, initialise, 0, 0, 100, 40).

'_redraw_area'(G, _A:area) :->
    red_block(M),
    get(G?area, x, X), get(G?area, y, Y),
    send(G, draw_markup, M, font(sans, normal, 12), X, Y).

:- pce_end_class(tmt_drawer).

:- begin_tests(markup_text).

test(plain_text, Text == "Some bold text") :-
    new(T, markup_text("Some <b>bold</b> text")),
    get(T, text, S),
    get(S, value, A),
    atom_string(A, Text).
test(invalid_markup_is_rejected, fail) :-
    new(_, markup_text("bad <b>markup")).
test(invalid_markup_keeps_the_old, Markup == "<b>ok</b>") :-
    new(T, markup_text("<b>ok</b>")),
    \+ send(T, markup, "bad <b>markup"),
    get(T, markup, M),
    get(M, value, A),
    atom_string(A, Markup).
test(draws_markup_colours, RGBA == Red) :-
    red(Red),
    red_block(M),
    new(T, markup_text(M)),
    pixel(T, 100, 40, 5, 5, RGBA).
test(uses_graphical_colour, true(memberchk(0xff0000ff, Set))) :-
    new(T, markup_text("<span size='xx-large'><b>MW</b></span>")),
    send(T, colour, blue),
    size(T, W-H),
    pixels(T, W, H, Set).
test(wrapping_makes_it_higher, true(WH > H)) :-
    Text = "one two three four five six seven eight",
    new(T1, markup_text(Text)),
    new(T2, markup_text(Text, 60)),
    size(T1, _-H),
    size(T2, _-WH).
test(wrapped_width, W == 300) :-
    new(T, markup_text("short", 300)),
    size(T, W-_).
test(right_alignment, [Left, Right] == [White, Red]) :-
    red(Red), white(White),
    red_block(M),
    new(T, markup_text(M, 200)),
    send(T, alignment, right),
    pixel(T, 200, 40, 5, 5, Left),
    pixel(T, 200, 40, 195, 5, Right).
test(ellipsize_keeps_one_line, H == H1) :-
    Text = "one two three four five six seven eight",
    new(T1, markup_text("one")),
    new(T, markup_text(Text, 60)),
    send(T, ellipsize, end),
    size(T1, _-H1),
    size(T, _-H).
test(line_spacing, true(H2 > H1)) :-
    Text = "line one\nline two",
    new(T1, markup_text(Text)),
    new(T2, markup_text(Text)),
    send(T2, line_spacing, 2),
    size(T1, _-H1),
    size(T2, _-H2).
test(geometry_sets_width, W == 120) :-
    new(T, markup_text("text")),
    send(T, set, 10, 10, 120),
    get(T, width, W).
test(theme_colour, [RGBA1, RGBA2] == [Red, Blue]) :-
    red(Red),
    Blue = 0xff0000ff,
    setup_call_cleanup(
        new(C, theme_colour(tmt_block, red)),
        ( new(T, markup_text("<span background='tmt_block'>      </span>")),
          pixel(T, 100, 40, 5, 5, RGBA1),
          send(C, derived_from, blue),
          pixel(T, 100, 40, 5, 5, RGBA2)
        ),
        free(C)).
test(xpce_colour_name, RGBA == 0xffafeeee) :-
    new(T, markup_text("<span background='pale_turquoise'>      </span>")),
    pixel(T, 100, 40, 5, 5, RGBA).
test(unknown_colour_is_rejected, fail) :-
    new(_, markup_text("<span background='no_such_colour'>x</span>")).
test(draw_markup, RGBA == Red) :-
    red(Red),
    new(G, tmt_drawer),
    pixel(G, 100, 40, 5, 5, RGBA).
% Map mono to the family xpce uses for sans: the Pango alias `sans` need
% not be that family, e.g., on Windows.

test(tt_is_xpce_mono, [TT-Family == W-W,
                       cleanup(set_font_cv(pango_families, Old))]) :-
    get(@font_class, class_variable, pango_families, CV),
    get(CV, value, Families),
    get(Families, find, @arg1?name == sans, Binding),
    get(Binding, value, Sans),
    set_font_cv(pango_families, chain(mono := Sans, sans := Sans), Old),
    Text = "iiiiMMMM",
    tt_width("<tt>~s</tt>", Text, TT),
    tt_width("<span font_family='mono'>~s</span>", Text, Family),
    tt_width("<span font_family='sans'>~s</span>", Text, W).

test(tt_background, RGBA == Red) :-
    red(Red),
    get(@pce, convert, ui_code_background, colour, C),
    get(C, derived_from, Old),
    setup_call_cleanup(
        send(C, derived_from, red),
        ( new(T, markup_text("<tt>      </tt>")),
          pixel(T, 100, 40, 5, 5, RGBA)
        ),
        send(C, derived_from, Old)).

tt_width(Fmt, Text, W) :-
    format(string(M), Fmt, [Text]),
    new(T, markup_text(M)),
    size(T, W-_).

set_font_cv(Name, Value, Old) :-
    get(@font_class, class_variable, Name, CV),
    get(CV, value, Old),
    set_font_cv(Name, Value).

set_font_cv(Name, Value) :-
    send(@font_class, class_variable_value, Name, Value),
    send(@display_manager, fonts_changed).

:- end_tests(markup_text).

value(Obj, Value) :-
    get(Obj, value, A),
    atom_string(A, Value).

:- begin_tests(markup).

test(escapes_arguments, V == "<b>a&lt;b&amp;&quot;c&#39;</b> 42 &lt;") :-
    new(M, markup("<b>%s</b> %d %c", 'a<b&"c\'', 42, 0'<)),
    value(M, V).
test(markup_argument_as_is, V == "<b><i>x</i></b>") :-
    new(M, markup("<b>%s</b>", markup("<i>x</i>"))),
    value(M, V).
test(no_arguments_is_not_a_format, V == "100% <b>%s</b>") :-
    new(M, markup("100% <b>%s</b>")),
    value(M, V).
test(format_escapes, V == "<i>&lt;</i>") :-
    new(M, markup("<b>x</b>")),
    send(M, format, "<i>%s</i>", '<'),
    value(M, V).
test(convert_escapes, [Class, V] == [markup, "a &lt; b"]) :-
    get(@pce, convert, 'a < b', markup, M),
    get(M, class_name, Class),
    value(M, V).
test(text, V == "a&b") :-
    new(M, markup("<b>a&amp;b</b>")),
    get(M, text, T),
    value(T, V).
test(invalid_markup_is_rejected, fail) :-
    new(_, markup("<b>bad")).
test(plain_format_of_markup, V == "a<b") :-
    new(S, string(markup("<b>%s</b>"), 'a<b')),
    value(S, V).

:- end_tests(markup).

:- begin_tests(label_markup).

test(format, [Class, V] == [markup, "<b>a&lt;b</b>"]) :-
    new(L, label(l, '')),
    send(L, format, markup("<b>%s</b>"), 'a<b'),
    get(L, selection, S),
    get(S, class_name, Class),
    value(S, V).
test(report_done, [Class, V] == [markup, "<b>go</b> ok&amp;"]) :-
    new(L, label(l, '')),
    send(L, report, progress, markup("<b>%s</b>"), go),
    send(L, report, done, 'ok&'),
    get(L, selection, S),
    get(S, class_name, Class),
    value(S, V).
test(draws_markup, RGBA == Red) :-
    red(Red),
    red_block(M),
    new(L, label(l, markup(M))),
    send(L, border, 0),
    pixel(L, 100, 40, 10, 5, RGBA).
test(inactive_drops_foreground, [Active, Inactive] == [true, false]) :-
    Markup = "<span size='xx-large' foreground='#ff0000'><b>MW</b></span>",
    new(L, label(l, markup(Markup))),
    size(L, W-H),
    pixels(L, W, H, Set1),
    send(L, active, @off),
    pixels(L, W, H, Set2),
    red(Red),
    truth(memberchk(Red, Set1), Active),
    truth(memberchk(Red, Set2), Inactive).

%   A clipped label draws a one-line copy of its text, which is not
%   0-terminated.  Showing a long text first leaves its tail behind
%   the copy of the short one.
test(clip_after_longer_text, Width == Fresh) :-
    Short = "<b>x</b>",
    length(Xs, 200), maplist(=(0'x), Xs),
    format(string(Long), "<b>~s</b>", [Xs]),
    new(L, label(l, markup(Long))),
    send(L, wrap, clip),
    send(L, length, 0),
    get(L, width, _),
    send(L, selection, markup(Short)),
    get(L, width, Width),
    new(L2, label(l, markup(Short))),
    send(L2, wrap, clip),
    send(L2, length, 0),
    get(L2, width, Fresh).

truth(G, true) :- call(G), !.
truth(_, false).

:- end_tests(label_markup).


:- begin_tests(list_browser_markup).

markup_browser(Labels, LB) :-
    new(LB, list_browser(@default, 20, 5)),
    forall(nth1(I, Labels, Label),
           send(LB, append, dict_item(I, Label))).

browser_pixels(LB, Set) :-
    size(LB, W-H),
    pixels(LB, W, H, Set).

key(DI, Key) :-
    get(DI, key, Key).

test(item_draws_markup, true(memberchk(Red, Set))) :-
    red(Red),
    red_block(M),
    markup_browser([markup(M)], LB),
    browser_pixels(LB, Set).
test(string_item_shows_tags, true(\+ memberchk(Red, Set))) :-
    red(Red),
    red_block(M),
    markup_browser([M], LB),
    browser_pixels(LB, Set).
test(selection_overrides_markup, true(\+ memberchk(Red, Set))) :-
    red(Red),
    red_block(M),
    markup_browser([markup(M)], LB),
    send(LB, selection, 1),
    browser_pixels(LB, Set).
test(find_prefix_ignores_markup, Key == 2) :-
    markup_browser([markup("<b>a</b>bc"), markup("<i>x</i>yz")], LB),
    get(LB?dict, find_prefix, "xy", DI),
    key(DI, Key).
test(match_ignores_markup, Keys == [1]) :-
    markup_browser([markup("<b>a</b>bc"), markup("<i>x</i>yz")], LB),
    get(LB?dict, match, "ab", Chain),
    chain_list(Chain, DIs),
    maplist(key, DIs, Keys).
test(sort_ignores_markup, Keys == [2, 1]) :-
    markup_browser([markup("<b>z</b>a"), "b"], LB),
    send(LB, sort),
    get(LB?dict?members, map, @arg1?key, Chain),
    chain_list(Chain, Keys).
test(search_ignores_markup) :-
    markup_browser([markup("<b>foo</b>")], LB),
    send(LB, insert_self, 1, 0'f).
test(search_does_not_find_tags, fail) :-
    markup_browser([markup("<b>foo</b>")], LB),
    send(LB, insert_self, 1, 0'<).
test(label_markup, [Class, TextClass, V] == [markup, markup_text, "<b>x</b>"]) :-
    new(LB, list_browser),
    send(LB, label, markup("<b>x</b>")),
    get(LB, label, L),
    get(L, class_name, Class),
    get(LB?label_text, class_name, TextClass),
    value(L, V).
test(label_plain, L == 'a<b&c') :-
    new(LB, list_browser),
    send(LB, label, 'a<b&c'),
    get(LB, label, L).
test(label_draws_markup, true(memberchk(Red, Set))) :-
    red(Red),
    red_block(M),
    new(LB, list_browser),
    send(LB, label, markup(M)),
    browser_pixels(LB, Set).

%   The label of an item with an icon starts <-icon_gap pixels after
%   the icon.  The icon is a red square.  The text starts at the first
%   pixel right of the icon, in the rows of the icon, that is not
%   white.

text_start(LB, X) :-
    size(LB, W-H),
    new(I, image(@nil, W, H)),
    send(I, draw_in, LB),
    red(Red), white(White),
    W1 is W-1, H1 is H-1,
    findall(X0-Y, ( between(0, H1, Y), between(0, W1, X0),
                    rgba(I, X0, Y, Red)
                  ), Icon),
    aggregate_all(max(X0), member(X0-_, Icon), Right),
    aggregate_all(min(X0),
                  ( member(_-Y, Icon), between(Right, W1, X0),
                    rgba(I, X0, Y, RGBA),
                    RGBA \== Red, RGBA \== White
                  ),
                  X),
    free(I).

rgba(I, X, Y, RGBA) :-
    get(I, pixel, X, Y, C),
    get(C, rgba, RGBA).

test(icon_gap, [Gap, D] == [4, 7]) :-
    new(Icon, image(@nil, 8, 8)),
    send(Icon, fill, red),
    new(LB, list_browser),
    send(LB, style, icon, style(icon := Icon)),
    send(LB, append, dict_item(x, 'MMM', style := icon)),
    get(LB, icon_gap, Gap),
    send(LB, icon_gap, 0),
    text_start(LB, X0),
    send(LB, icon_gap, 7),
    text_start(LB, X7),
    D is X7 - X0.

:- end_tests(list_browser_markup).


:- begin_tests(decorator_markup).

decorated_label(Label, Class, Value) :-
    get(Label, class_name, Class),
    value(Label, Value).

test(markup_label, [Class, V, TextClass] ==
                   [markup, "<b>a&lt;b</b>", markup_text]) :-
    new(P, picture),
    send(P, label, markup("<b>%s</b>"), 'a<b'),
    get(P, decoration, D),
    get(D, label, L),
    decorated_label(L, Class, V),
    get(D?label_text, class_name, TextClass).
test(plain_label, [Class, V] == [string, "a<b 42&"]) :-
    new(P, picture),
    send(P, label, 'a<b %d&', 42),
    get(P?decoration, label, L),
    decorated_label(L, Class, V).
test(label_above_window, true(Y >= H)) :-
    new(P, picture),
    send(P, label, markup("<big>Big</big> title")),
    get(P?decoration, label_text, T),
    get(T?area, height, H),
    get(P?area, y, Y).
test(label_width, W == DW) :-
    new(P, picture),
    send(P, label, markup("<b>title</b>")),
    get(P, decoration, D),
    send(D, set, width := 300),
    get(D?label_text?area, width, W),
    get(D?area, width, DW).

:- end_tests(decorator_markup).


:- begin_tests(editor_markup).

test(label_markup, [Class, TextClass, V] == [markup, markup_text, "<b>x</b>"]) :-
    new(E, editor),
    send(E, label, markup("<b>x</b>")),
    get(E, label, L),
    get(L, class_name, Class),
    get(E?label_text, class_name, TextClass),
    value(L, V).
test(label_plain, L == 'a<b&c') :-
    new(E, editor),
    send(E, label, 'a<b&c'),
    get(E, label, L).
test(label_draws_markup, true(memberchk(Red, Set))) :-
    red(Red),
    red_block(M),
    new(E, editor),
    send(E, label, markup(M)),
    size(E, W-H),
    pixels(E, W, H, Set).

:- end_tests(editor_markup).
