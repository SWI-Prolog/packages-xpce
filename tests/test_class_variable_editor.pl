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

:- module(test_class_variable_editor, [test_class_variable_editor/0]).

/** <module> Test the class variable editor

Tests library(pce_class_variable_editor): mapping types to items,
writing values as Defaults text, updating a Defaults file and applying
a change to the live objects.

Run with:

    swipl -g test_class_variable_editor -t halt \
          packages/xpce/tests/test_class_variable_editor.pl
*/

%   The editor is a persistent_frame: keep what it saves out of the
%   configuration of the user running the tests.  Must be done before
%   xpce is initialised.

sandbox_app_config :-                   % an existing dir is preferred
    tmp_file(xpce_config, Dir),
    directory_file_path(Dir, xpce, XpceDir),
    make_directory_path(XpceDir),
    asserta(user:file_search_path(user_app_config, Dir)).

:- initialization(sandbox_app_config, now).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(readutil)).
:- use_module(library(pce_class_variable_editor)).

test_class_variable_editor :-
    run_tests([ class_variable_editor_types,
                class_variable_editor_text,
                class_variable_editor_file,
                class_variable_editor_live,
                class_variable_editor_doc
              ]).

:- pce_begin_class(tcve_item, object).
class_variable(i,      int,       3).
class_variable(r,      '1..10',   5).
class_variable(o,      '0.0..1.0', 0.5).
class_variable(p,      '0..',     2).
class_variable(q,      '0.0..',   2.0).
class_variable(n,      'int*',    @nil).
class_variable(b,      bool,      @on).
class_variable(e,      '{left,right}', left).
class_variable(f,      font,      normal).
class_variable(m,      mono_font, fixed).
class_variable(c,      colour,    red).
class_variable(t,      colour,    ui_accent).
class_variable(u,      cursor,    hand2).
class_variable(v,      style,     style(colour := red, bold := @on)).
class_variable(s,      size,      size(10,20)).
class_variable(d,      '[colour]', @default).
:- pce_end_class.

:- pce_begin_class(tcve_box, box).
class_variable(colour, colour, red).
:- pce_end_class.
:- pce_begin_class(tcve_sub_box, tcve_box).
:- pce_end_class.

cv(Name, CV) :-
    get(class(tcve_item), class_variable, Name, CV).

config_type(Name, Type-Nullable) :-
    cv(Name, CV),
    get(CV, type, T),
    pce_class_variable_editor:cv_config_type(T, Type, Nullable).

round_trip(Name, Text) :-
    cv(Name, CV),
    get(CV, value, Value),
    pce_class_variable_editor:defaults_text(CV, Value, Text),
    get(CV, convert_string, Text, Value2),
    pce_class_variable_editor:value_to_defaults_text(Value2, Text).

:- begin_tests(class_variable_editor_types).

test(int, T == int-[]) :-
    config_type(i, T).
test(int_range, T == between(1,10)-[]) :-
    config_type(r, T).
test(real_range, T == between(0.0,1.0)-[]) :-
    config_type(o, T).
test(open_int_range, T == between(0,inf)-[]) :-
    config_type(p, T).
test(open_real_range, T == between(0.0,inf)-[]) :-
    config_type(q, T).
test(real_range_slider, [Class-Low-Sel == slider-0.0-0.5]) :-
    new(Row, pce_class_variable_editor:cv_row(class(tcve_item),
                                              class(tcve_item), o)),
    get(Row, item, Item),
    get(Item, class_name, Class),
    get(Item, low, Low),
    get(Item, selection, Sel).
test(nullable, T == int-[none]) :-
    config_type(n, T).
test(bool, T == bool-[]) :-
    config_type(b, T).
test(name_of, T == {left,right}-[]) :-
    config_type(e, T).
test(font, T == font-[]) :-
    config_type(f, T).
test(mono_font, T == mono_font-[]) :-
    config_type(m, T).
test(mono_font_families, [Fs == [mono]]) :-
    cv(m, CV),
    get(CV, value, V),
    pce_class_variable_editor:cv_item(mono_font, m, CV, V, Item),
    get(Item, member, family, Menu),
    get(Menu?members, map, @arg1?value, C),
    chain_list(C, Fs).
test(mono_font_family_inactive, [A0-A1 == @off - @off]) :-
    cv(m, CV),
    get(CV, value, V),
    pce_class_variable_editor:cv_item(mono_font, m, CV, V, Item),
    get(Item, member, family, Menu),
    get(Menu, active, A0),
    send(Item, active, @on),            % e.g., a Default tick box
    get(Menu, active, A1).
test(colour, T == colour-[]) :-
    config_type(c, T).
test(generic, T == generic-[]) :-
    config_type(s, T).
test(cursor, T == cursor-[]) :-
    config_type(u, T).
test(style, T == style-[]) :-
    config_type(v, T).
test(default, T == colour-[default]) :-
    config_type(d, T).
test(ordered_by_type, Names == [b,c,d,t,u,f,i,n,p,r,m,o,q,s,v,e]) :-
    new(Seen, chain),
    pce_class_variable_editor:declared_class_variables(
                                  class(tcve_item), Seen, CVs),
    get(CVs, map, @arg1?name, NameChain),
    chain_list(NameChain, Names).
test(seen_skipped, Names == [b,c]) :-
    new(Seen, chain(d,e,f,i,m,n,o,p,q,r,s,t,u,v)),
    pce_class_variable_editor:declared_class_variables(
                                  class(tcve_item), Seen, CVs),
    get(CVs, map, @arg1?name, NameChain),
    chain_list(NameChain, Names).

:- end_tests(class_variable_editor_types).

:- begin_tests(class_variable_editor_text).

test(int, T == "3") :-
    round_trip(i, T).
test(real, T == "0.5") :-
    round_trip(o, T).
test(bool, T == "@on") :-
    round_trip(b, T).
test(name, T == "left") :-
    round_trip(e, T).
test(font, T == "font(sans, normal, 12)") :-
    round_trip(f, T).
test(colour, T == "colour(red)") :-
    round_trip(c, T).
test(theme_colour, T == "ui_accent") :-
    round_trip(t, T).
test(cursor, T == "hand2") :-
    round_trip(u, T).
test(style, T == "style(colour:=colour(red), bold:= @on)") :-
    round_trip(v, T).
test(default, T == "@default") :-
    round_trip(d, T).
test(size, T == "size(10, 20)") :-
    round_trip(s, T).
test(size_rounded, T == "size(14.4, 7.6)") :-
    pce_class_variable_editor:value_to_defaults_text(
        size(14.362204724409448, 7.559055118110237), T).
test(number_not_rounded, T == "0.05") :-
    pce_class_variable_editor:value_to_defaults_text(0.05, T).

:- end_tests(class_variable_editor_text).

:- begin_tests(class_variable_editor_file,
               [ setup(tmp_file(defaults, File)),
                 cleanup(catch(delete_file(File), _, true))
               ]).

lines(File, Lines) :-
    read_file_to_string(File, String, []),
    split_string(String, "\n", "", Lines).

save(File, Key, Text) :-
    pce_class_variable_editor:save_defaults(File, Key, Text).

test(create_from_template, [First-Last == "!"-["a.x:\t1", ""]]) :-
    tmp_file(defaults, File),
    save(File, 'a.x', "1"),
    lines(File, Lines),
    delete_file(File),
    Lines = [Line1|_],
    sub_string(Line1, 0, 1, _, First),
    append(_, Last, Lines),
    length(Last, 2),
    !.
test(replace_and_append,
     Lines == [ "! comment",
                "!a.x: 0",
                "a.x:\t2",
                "b.y: 1",
                "b.z:\t3",
                ""
              ]) :-
    tmp_file(defaults, File),
    setup_call_cleanup(
        open(File, write, Out),
        format(Out, "! comment~n!a.x: 0~na.x: [ 1, \\~n      2 ]~nb.y: 1~n", []),
        close(Out)),
    save(File, 'a.x', "2"),
    save(File, 'b.z', "3"),
    lines(File, Lines),
    delete_file(File).

test(delete_entry, [Lines == ["! comment", "b.y: 1", ""]]) :-
    tmp_file(defaults, File),
    setup_call_cleanup(
        open(File, write, Out),
        format(Out, "! comment~na.x: [ 1, \\~n      2 ]~nb.y: 1~n", []),
        close(Out)),
    pce_class_variable_editor:delete_defaults(File, 'a.x'),
    lines(File, Lines),
    delete_file(File).

:- end_tests(class_variable_editor_file).

:- begin_tests(class_variable_editor_live).

%   "Show all class variables" shows the same sections, with all class
%   variables of their class rather than the specified ones.

:- multifile pce_preferences:preferences/2.

pce_preferences:preferences(tcve_pref_box, [ tcve_pref_box - [colour] ]).

:- pce_begin_class(tcve_pref_box, box).
:- pce_end_class.

show_all_rows(ShowAll, Sections, Rows) :-
    new(P, picture),
    send(P, display, new(B, tcve_pref_box(10,10))),
    send(P, open),
    new(E, class_variable_editor),
    get(E, member, top_dialog, Top),
    get(Top, member, show_all, Switch),
    send(Switch, selection, ShowAll),
    send(E, edit, B),
    get(E, component, D),
    get(D?graphicals, find_all,
        and(message(@arg1, instance_of, label), @arg1?name == class),
        Labels),
    chain_list(Labels, LabelList),
    maplist(section_title, LabelList, Sections),
    get(E?rows, size, Rows),
    send(E, destroy),
    send(P, destroy).

section_title(Label, Title) :-
    get(Label, selection, Title0),
    (   atom(Title0)
    ->  Title = Title0
    ;   get(Title0, value, Title)
    ).

%   The top line shows the containment path, from the frame down to the
%   object.  The current object is a label, the others link to them.

path_items(E, Items) :-
    get(E, member, top_dialog, Top),
    get(Top, member, path, G),
    get(G?graphicals, find_all, @arg1?name \== separator, Chain),
    chain_list(Chain, List),
    maplist(path_item, List, Items).

path_item(Item, Kind-Text) :-
    get(Item, name, Kind),
    get(Item, selection, Text0),
    (   atom(Text0)
    ->  Text = Text0
    ;   get(Text0, value, Text)
    ).

test(path, [Before-After == [link-picture, current-tcve_pref_box]-
                            [current-picture, link-tcve_pref_box]]) :-
    new(P, picture),
    send(P, display, new(B, tcve_pref_box(10,10))),
    send(P, open),
    new(E, class_variable_editor),
    send(E, edit, B),
    path_items(E, Items0),
    last2(Items0, Before),
    send(E, select_path, P),
    path_items(E, Items1),
    last2(Items1, After),
    send(E, destroy),
    send(P, destroy).

last2(List, Last2) :-
    append(_, Last2, List),
    length(Last2, 2),
    !.

test(preferences, [Sections-Rows == [tcve_pref_box]-1]) :-
    show_all_rows(@off, Sections, Rows).
test(show_all_keeps_sections, [Sections == [tcve_pref_box], true(Rows > 1)]) :-
    show_all_rows(@on, Sections, Rows).

colour_name(Gr, Name) :-
    get(Gr, colour, Colour),
    get(Colour, name, Name).

test(apply_refresh, [Parent-Sub-Created-ParentCV == red-blue-blue-red]) :-
    new(P, picture),
    send(P, display, new(B1, tcve_box(10,10))),
    send(P, display, new(B2, tcve_sub_box(10,10)), point(20,0)),
    send(P, open),
    new(Row, pce_class_variable_editor:cv_row(class(tcve_sub_box),
                                              class(tcve_box), colour)),
    send(Row, value, colour(blue)),
    send(Row, apply),
    colour_name(B1, Parent),
    colour_name(B2, Sub),
    new(B3, tcve_sub_box(5,5)),
    colour_name(B3, Created),
    get(class(tcve_box), class_variable, colour, CV),
    get(CV, value, CVColour),
    get(CVColour, name, ParentCV),
    send(P, destroy).

%   The class variables of text_cursor are not copied into a slot.  An
%   editor sets the style of its caret from them when its font is set.

test(apply_caret_style, [Style == block,
                         cleanup(send(class(text_cursor), class_variable_value,
                                      fixed_font_style, Old))]) :-
    get(class(text_cursor), class_variable, fixed_font_style, CV),
    get(CV, value, Old),
    new(V, view),
    send(V, font, font(mono, normal, 12)),
    send(V, open),
    new(Row, pce_class_variable_editor:cv_row(class(text_cursor),
                                              class(text_cursor),
                                              fixed_font_style)),
    send(Row, value, block),
    send(Row, apply),
    get(V?editor?text_cursor, style, Style),
    send(V, destroy).

%   A value is applied as soon as the user enters it.

test(apply_on_change, [Colour == green,
                       cleanup(send(class(tcve_box), class_variable_value,
                                    colour, colour(red)))]) :-
    new(Row, pce_class_variable_editor:cv_row(class(tcve_box),
                                              class(tcve_box), colour)),
    send(Row, value, colour(green)),
    send(Row, item_changed),            % as the item's message
    new(B, tcve_box(5,5)),
    colour_name(B, Colour).

%   A default declared in Prolog need not be in Defaults syntax.

test(program_default_plain_text, Default == 'SWI-Prolog -- %s') :-
    use_module(library(pane_frame)),
    pce_class_variable_editor:program_default(class(pane_frame),
                                              label_format, Default).

%   The tab stack of the IDE hides the label of a single tab, using a
%   class variable that the user may change.

test(hide_single_label, [Class-Before-After == pane_tab_stack-(@on)-(@off),
                         cleanup(send(class(pane_tab_stack),
                                      class_variable_value,
                                      hide_single_label, @on))]) :-
    use_module(library(pane_frame)),
    new(TW, pane_tabbed_window),
    send(TW, open),
    get(TW?graphicals, find, @arg1?name == tab_stack, TS),
    get(TS, class_name, Class),
    get(TS, hide_single_label, Before),
    new(Row, pce_class_variable_editor:cv_row(class(pane_tab_stack),
                                              class(pane_tab_stack),
                                              hide_single_label)),
    send(Row, value, @off),
    send(Row, item_changed),
    get(TS, hide_single_label, After),
    send(TW, destroy).

%   Tiles are not graphicals, but hold their border.  Applying the
%   class variable updates the tiles of open frames and lays them out.

apply_row(Class, Name, Value) :-
    new(Row, pce_class_variable_editor:cv_row(class(Class), class(Class),
                                              Name)),
    send(Row, value, Value),
    send(Row, item_changed).

test(tile_border, [ [Border, Gap] == [10, 10],
                    cleanup(apply_row(tile, border, 4))
                  ]) :-
    new(F, frame),
    send(F, append, new(P1, picture)),
    send(new(P2, picture), below, P1),
    send(F, open),
    apply_row(tile, border, 10),
    get(P1?tile, border, Border),
    get(P1?tile?area, bottom_side, B1),   % the tile has the position
    get(P2?tile?area, top_side, T2),
    Gap is T2-B1,
    send(F, destroy).

%   Only a gap that can be dragged has the tile border.  A dialog that
%   has its own size cannot be resized, so the gap below it is the
%   (smaller) tile.fixed_border.

test(fixed_border, [ [DialogGap, PictureGap] == [0, 4] ]) :-
    new(F, frame),
    send(F, append, new(D, dialog)),
    send(D, append, button(hello)),
    send(new(P1, picture), below, D),
    send(new(P2, picture), below, P1),
    send(F, open),
    gap(D, P1, DialogGap),
    gap(P1, P2, PictureGap),
    send(F, destroy).

gap(W1, W2, Gap) :-
    get(W1?tile?area, bottom_side, B1),
    get(W2?tile?area, top_side, T2),
    Gap is T2-B1.

%   The separators of a tab_frame use the class variables of tile.

test(separator_pen, [ [Before, After] == [1, 0],
                      cleanup(apply_row(tile, separator_pen, 1))
                    ]) :-
    use_module(library(tab_frame)),
    use_module(library(tabbed_window)),
    new(TW, tabbed_window(test)),
    send(TW, tab, new(TF, tab_frame(new(P1, picture), one))),
    send(TF, split, new(_P2, picture), P1, vertically),
    send(TW, open),
    get(TF?separators, size, Before),
    apply_row(tile, separator_pen, 0),
    get(TF?separators, size, After),
    send(TW, destroy).

%   Fonts are shared objects: applying font.scale reloads them.  The
%   Pango families are edited using pango_families_item.

test(font_scale, [ true(H1 > H0),
                   cleanup(( send(@font_class, class_variable_value,
                                  scale, 1.0),
                             send(@display_manager, fonts_changed) ))
                 ]) :-
    new(F, font(sans, normal, 12)),
    get(F, height, H0),
    new(Row, pce_class_variable_editor:cv_row(class(font), class(font),
                                              scale)),
    send(Row, value, 2),
    send(Row, item_changed),
    get(F, height, H1).
test(pango_families_item, [Class-Fields-Revert ==
                           pango_families_item-[mono,sans,serif]-(@off)]) :-
    new(Row, pce_class_variable_editor:cv_row(class(font), class(font),
                                              pango_families)),
    get(Row?item, class_name, Class),
    get(Row?item?graphicals, find_all,
        message(@arg1, instance_of, menu), Items),
    get(Items, map, @arg1?name, Names),
    chain_list(Names, Fields),
    revert_shown(Row, Revert).
test(pango_mono_offers_monospace, Offered == Mono) :-
    new(Row, pce_class_variable_editor:cv_row(class(font), class(font),
                                              pango_families)),
    get(Row?item, member, mono, Menu),
    get(@font_class, font_families, @on, Sheet),
    get(Sheet?attribute_names, size, Mono),
    get(Menu?members, find_all,
        message(Sheet, is_attribute, @arg1?value), Installed),
    get(Installed, size, Offered).
test(pango_selection_in_own_font, Font == Preview) :-
    new(PI, pango_families_item(pango_families,
                                chain(sans := "Not Installed,sans"))),
    get(PI, member, sans, Menu),
    get(Menu, member, 'Not Installed,sans', MI),
    get(MI, font, Font),
    get(MI, attribute, preview_font, Preview).
test(font_scale_slider, Class == slider) :-
    new(Row, pce_class_variable_editor:cv_row(class(font), class(font),
                                              scale)),
    get(Row?item, class_name, Class).

revert_shown(Row, Shown) :-
    (   send(Row?revert, shown)
    ->  Shown = @on
    ;   Shown = @off
    ).

test(revert_link, [S0-S1-S2-C == @off - @on - @off - red]) :-
    new(Row, pce_class_variable_editor:cv_row(class(tcve_box),
                                              class(tcve_box), colour)),
    revert_shown(Row, S0),              % program default: no link
    send(Row, value, colour(blue)),
    revert_shown(Row, S1),
    send(Row, revert_to_default),
    revert_shown(Row, S2),
    get(Row?value, name, C).
test(revert_help, [true(sub_string(H, _, _, _, "colour(red)"))]) :-
    new(Row, pce_class_variable_editor:cv_row(class(tcve_box),
                                              class(tcve_box), colour)),
    get(Row?revert, help_message, tag, H0),
    get(H0, value, H1),
    atom_string(H1, H).
test(save_reverted_deletes, [Lines == ["! comment", ""]]) :-
    tmp_file(defaults, File),
    setup_call_cleanup(
        open(File, write, Out),
        format(Out, "! comment~ntcve_box.colour: colour(blue)~n", []),
        close(Out)),
    new(Row, pce_class_variable_editor:cv_row(class(tcve_box),
                                              class(tcve_box), colour)),
    send(Row, saved, colour(blue)),     % as if read from File
    send(Row, revert_to_default),
    send(Row, save, File),
    read_file_to_string(File, String, []),
    split_string(String, "\n", "", Lines),
    delete_file(File).
test(switch_special, [S0-V0-A0-S1-V1 == @on-5-(@on) - @off-(@nil)]) :-
    new(Row, pce_class_variable_editor:cv_row(class(tcve_item),
                                              class(tcve_item), n)),
    send(Row, value, 5),
    get(Row?switch, selection, S0),
    get(Row, value, V0),
    get(Row?item, active, A0),
    send(Row?switch, selection, @off),
    send(Row, switched, @off),
    get(Row?switch, selection, S1),
    get(Row, value, V1).
test(switch_inactive, [A == @off]) :-
    new(Row, pce_class_variable_editor:cv_row(class(tcve_item),
                                              class(tcve_item), i)),
    get(Row?switch, active, A).
test(switch_shows_special, [S-A == @off - @off]) :-
    new(Row, pce_class_variable_editor:cv_row(class(tcve_item),
                                              class(tcve_item), d)),
    get(Row?switch, selection, S),     % d is @default
    get(Row?item, active, A).
test(scope, [Keys-Targets == ['tcve_sub_box.colour', 'tcve_box.colour', '*.colour']-[tcve_sub_box, tcve_box, tcve_box]]) :-
    new(Row, pce_class_variable_editor:cv_row(class(tcve_sub_box),
                                              class(tcve_box), colour)),
    maplist(scope_key(Row), [tcve_sub_box, tcve_box, *], Keys, Targets).

scope_key(Row, Scope, Key, Target) :-
    send(Row?scope, selection, Scope),
    get(Row, key, Key),
    get(Row?target, name, Target).

%   The general preferences are in tabs after the Components tab.  The
%   containment path only applies to the Components tab.  A tab without
%   preferences of a loaded class is not shown.

:- multifile
    pce_preferences:general_tab/3,
    pce_preferences:general/2.

pce_preferences:general_tab(tcve_empty, 'Empty', 99).
pce_preferences:general(tcve_empty, [no_such_class-[x]]).

tab_rows(E, Tab, Names) :-
    get(E?general, find, @arg1?name == Tab, D),
    get(D?rows, map, @arg1?name, Chain),
    chain_list(Chain, Names).

test(general_tabs, [ Labels == ['Components', 'Text', 'Theme', 'Layout',
                                'IDE'] ]) :-
    use_module(library(pane_frame)),    % adds the IDE tab
    new(E, class_variable_editor),
    get(E, member, tabs, Tabs),
    get(Tabs?graphicals, find, @arg1?name == tab_stack, TS),
    get(TS?graphicals, map, @arg1?label, Chain),
    chain_list(Chain, Labels),
    send(E, destroy).
test(general_heading, [ Sections = [_,_] ]) :-
    pce_preferences:general_preferences(Tabs),
    memberchk(tab(theme, _, Theme), Tabs),
    memberchk(heading('Bell', Sections), Theme).
test(general_tab_without_classes, [ fail ]) :-
    pce_preferences:general_preferences(Tabs),
    memberchk(tab(tcve_empty, _, _), Tabs).
test(general_rows, [ true(subset([scale, pango_families, blink], Text)),
                     true(subset([theme, visual_bell, volume, bell_pitch,
                                  bell_duration], Theme)),
                     true(subset([border, gap_colour], Layout))
                   ]) :-
    new(E, class_variable_editor),
    tab_rows(E, text, Text),
    tab_rows(E, theme, Theme),
    tab_rows(E, layout, Layout),
    send(E, destroy).
test(general_show_all, [ true(All > Basic) ]) :-
    new(E, class_variable_editor),
    tab_rows(E, layout, Rows0),
    length(Rows0, Basic),
    get(E, member, top_dialog, Top),
    get(Top, member, show_all, Switch),
    send(Switch, selection, @on),
    send(E, refresh),
    tab_rows(E, layout, Rows1),
    length(Rows1, All),
    send(E, destroy).
test(path_on_components_only, [ Shown == [@on, @off, @on] ]) :-
    new(E, class_variable_editor),
    send(E, open),
    get(E, member, top_dialog, Top),
    get(Top, member, path, Path),
    get(Path, displayed, S0),
    get(E?general, head, Text),
    send(E, tab, Text),
    get(Path, displayed, S1),
    send(E, tab, E?component),
    get(Path, displayed, S2),
    Shown = [S0, S1, S2],
    send(E, destroy).

%   The Theme tab selects the theme, the class variable display.theme.
%   @default follows the desktop.

test(theme_item, [ Values == [@default, dark, my_theme] ]) :-
    new(I, theme_item(theme, @default)),
    get(I, selection, V0),
    send(I, selection, dark),
    get(I, selection, V1),
    send(I, selection, my_theme),       % not available: added
    get(I, selection, V2),
    Values = [V0, V1, V2],
    free(I).
test(theme_row, [ [Class, Value, Selection] == [theme_item, dark, dark],
                  cleanup(( send(class(display), class_variable_value,
                                 theme, @default),
                            pce_theme:select_theme(system)
                          ))
                ]) :-
    new(Row, pce_class_variable_editor:cv_row(class(display), class(display),
                                              theme)),
    get(Row?item, class_name, Class),
    send(Row, value, dark),
    send(Row, item_changed),
    get(class(display), class_variable, theme, CV),
    get(CV, value, Value),
    pce_theme:current_theme_selection(Selection).

test(theme_revert, [ [Value, Selection] == [@default, system],
                      cleanup(( send(class(display), class_variable_value,
                                     theme, @default),
                                pce_theme:select_theme(system)
                              ))
                    ]) :-
    new(Row, pce_class_variable_editor:cv_row(class(display), class(display),
                                              theme)),
    send(Row, value, dark),
    send(Row, item_changed),
    send(Row, revert_to_default),
    get(class(display), class_variable, theme, CV),
    get(CV, value, Value),
    pce_theme:current_theme_selection(Selection).

%   A row shows a change made elsewhere: by another row for the same
%   class variable or outside the editor, e.g., the theme from the menu
%   of the IDE.

test(sync_other_row, [ Shown == green,
                       cleanup(send(class(tcve_box), class_variable_value,
                                    colour, red))
                     ]) :-
    new(R1, pce_class_variable_editor:cv_row(class(tcve_box),
                                             class(tcve_box), colour)),
    new(R2, pce_class_variable_editor:cv_row(class(tcve_box),
                                             class(tcve_box), colour)),
    send(R1, value, colour(green)),
    send(R1, item_changed),
    send(R2, sync),
    get(R2, value, Colour),
    get(Colour, name, Shown).
test(sync_on_focus, [ Shown == dark,
                      cleanup(( send(class(display), class_variable_value,
                                     theme, @default),
                                pce_theme:select_theme(system)
                              ))
                    ]) :-
    new(E, class_variable_editor),
    send(E, open),
    pce_theme:select_theme(dark),       % as the menu of the IDE
    send(E, input_focus, @on),
    get(E?general, find, @arg1?name == theme, D),
    get(D?rows, find, @arg1?name == theme, Row),
    get(Row, value, Shown),
    send(E, destroy).

%   The scope menu is only active if it offers a choice: a class
%   variable that only display has can only be set for display.

test(scope_single_class, [ Active == [@off, @on] ]) :-
    new(Volume, pce_class_variable_editor:cv_row(class(display),
                                                 class(display), volume)),
    new(Style, pce_class_variable_editor:cv_row(class(editor),
                                                class(editor),
                                                selection_style)),
    get(Volume?scope, active, A0),
    get(Style?scope, active, A1),
    Active = [A0, A1].

%   The audible bell is only used if the visual bell is off.  The rows
%   that configure it are inactive otherwise.

test(requires, [ Active == [@off, @on, @on, @off],
                 cleanup(send(class(graphical), class_variable_value,
                              visual_bell, @on))
               ]) :-
    send(class(graphical), class_variable_value, visual_bell, @on),
    new(Volume, pce_class_variable_editor:cv_row(class(display),
                                                 class(display), volume)),
    new(Flash, pce_class_variable_editor:cv_row(
                   class(graphical), class(graphical),
                   visual_bell_duration)),
    get(Volume?item, active, A0),
    get(Flash?item, active, A1),
    send(class(graphical), class_variable_value, visual_bell, @off),
    send(Volume, sync),
    send(Flash, sync),
    get(Volume?item, active, A2),
    get(Flash?item, active, A3),
    Active = [A0, A1, A2, A3].

%   A tool in the IDE is a pane_stack.  Where it is added is shown with
%   the preferences of any object in it.

test(tool_pane_side, [ Names == [pane_side] ]) :-
    use_module(library(pane_frame)),
    new(PS, pane_stack(tool)),
    send(PS, append, new(P, picture)),
    send(PS, open),
    new(E, class_variable_editor),
    send(E, edit, P),
    get(E?rows, find_all, @arg1?class?name == pane_stack, Rows),
    get(Rows, map, @arg1?name, Chain),
    chain_list(Chain, Names),
    send(E, destroy),
    send(PS, destroy).

%   PceEmacs shows the preferences of its buffer.

test(emacs_buffer, [ true(memberchk(unicode_encoding, Names)) ]) :-
    use_module(library(emacs/emacs)),
    new(V, emacs_view),
    send(V, open),
    new(E, class_variable_editor),
    send(E, edit, V?editor),
    get(E?rows, find_all, @arg1?class?name == emacs_buffer, Rows),
    get(Rows, map, @arg1?name, Chain),
    chain_list(Chain, Names),
    send(E, destroy),
    free(V).

%   The profiler shows the preferences of its panes, also of those in
%   its tabs.

test(profiler, [ Rows == [ prof_frame-auto_reset,
                           prof_browser-max_width,
                           prof_graph-caller_depth,
                           prof_graph-callee_depth,
                           prof_graph-prune_above,
                           prof_graph-max_relatives,
                           prof_graph-max_nodes,
                           prof_graph-natural_zoom,
                           prof_details-header_colour,
                           prof_details-header_background
                         ]
               ]) :-
    use_module(library(swi/pce_profile)),
    new(P, prof_frame),
    get(P, contained, class(prof_browser), B),
    new(E, class_variable_editor),
    send(E, edit, B),
    get(E?rows, map, @arg1?class?name, ClassChain),
    get(E?rows, map, @arg1?name, NameChain),
    chain_list(ClassChain, Classes),
    chain_list(NameChain, Names),
    pairs_keys_values(Rows, Classes, Names),
    send(E, destroy),
    free(P).

%   The settings of the cross-referencer are class variables, so the
%   editor shows them with the colours it uses.

test(xref, [ [Rows, Setting] ==
             [ [ xref_tool-warn_autoload,
                 xref_tool-warn_not_called,
                 xref_tool-hide_system_files,
                 xref_tool-hide_profile_files,
                 xref_predicate_text-colour,
                 xref_predicate_text-colour_autoload,
                 xref_predicate_text-colour_global,
                 xref_predicate_text-colour_undefined,
                 xref_predicate_text-colour_not_called,
                 xref_file_graph_node-background,
                 xref_file_graph_node-colour,
                 xref_file_graph_node-font,
                 prolog_file_info-header_colour,
                 prolog_file_info-header_background
               ],
               true
             ],
             cleanup(send(class(xref_tool), class_variable_value,
                          warn_autoload, @off))
           ]) :-
    use_module(library(pce_xref)),
    new(X, xref_tool),
    send(X, setting, warn_autoload, @on),       % as the Settings menu
    pce_xref_gui:setting(warn_autoload, Setting),
    new(E, class_variable_editor),
    send(E, edit, X),
    get(E?rows, map, @arg1?class?name, ClassChain),
    get(E?rows, map, @arg1?name, NameChain),
    chain_list(ClassChain, Classes),
    chain_list(NameChain, Names),
    pairs_keys_values(Rows, Classes, Names),
    send(E, destroy),
    free(X).

%   The preferences of the debugger are class variables of
%   prolog_debug_settings.  A change in the editor reaches
%   library(portray_text) through pce_preferences:class_variable_changed/3
%   and the settings of an older config('Tracer.cnf') are applied.

test(debugger, [ true(subset([ prolog_debug_settings-show_unbound,
                               prolog_debug_settings-stack_depth,
                               prolog_debug_settings-other_threads,
                               prolog_bindings_view-font
                             ], Rows))
               ]) :-
    use_module(library(trace/util)),
    use_module(library(trace/gui)),
    once(pce_preferences:preferences(prolog_debugger, Spec)),
    pce_preferences:preference_sections(@nil, Spec, Sections),
    findall(Class-Name,
            ( member(section(C, Names), Sections),
              get(C, name, Class),
              member(Name, Names),
              new(_, pce_class_variable_editor:cv_row(C, C, Name))
            ),
            Rows).
test(debugger_portray_text, [ Len == 42,
                              cleanup(( send(class(prolog_debug_settings),
                                             class_variable_value,
                                             portray_text_length, Old),
                                        set_portray_text(ellipsis, _, Old)
                                      ))
                            ]) :-
    use_module(library(trace/util)),
    set_portray_text(ellipsis, Old, Old),
    new(Row, pce_class_variable_editor:cv_row(
                 class(prolog_debug_settings), class(prolog_debug_settings),
                 portray_text_length)),
    send(Row, value, 42),
    send(Row, item_changed),
    set_portray_text(ellipsis, Len, Len).
test(debugger_migrate, [ Depth == 17,
                         cleanup(( send(class(prolog_debug_settings),
                                        class_variable_value,
                                        stack_depth, Old),
                                   ignore(delete_file(File))
                                 ))
                       ]) :-
    use_module(library(trace/util)),
    prolog_trace_utils:setting(stack_depth, Old),
    absolute_file_name(config('Tracer.cnf'), File,
                       [ access(write) ]),
    setup_call_cleanup(open(File, write, Out),
                       format(Out, '~q.~n', [setting(stack_depth, 17)]),
                       close(Out)),
    prolog_trace_utils:migrate_trace_settings,
    prolog_trace_utils:setting(stack_depth, Depth).

%   A view gives its font to its editor.  Changing the font of the
%   view updates the editor; the bindings view of the debugger also
%   updates its tab stops.

test(view_font, [ [Points, TabChanged] == [20, true],
                  cleanup(( send(class(prolog_bindings_view),
                                 class_variable_value, font, Old),
                            send(F, destroy)
                          ))
                ]) :-
    use_module(library(trace/gui)),
    get(class(prolog_bindings_view), class_variable, font, CV),
    get(CV, value, Old),
    new(F, frame),
    send(F, append, new(B, prolog_bindings_view)),
    send(F, open),
    get(B?text_image?tab_stops, element, 1, Tab0),
    new(Row, pce_class_variable_editor:cv_row(
                 class(prolog_bindings_view), class(prolog_bindings_view),
                 font)),
    send(Row, value, font(mono, normal, 20)),
    send(Row, item_changed),
    get(B?editor?font, points, Points),
    get(B?text_image?tab_stops, element, 1, Tab1),
    (   Tab1 =\= Tab0
    ->  TabChanged = true
    ;   TabChanged = Tab0-Tab1
    ).

%   The styles of the debugger ports are class variables of the source
%   view.  A change applies to the open source views, which keep the
%   margin icon of the port.

test(port_style, [ [Colour, Icon] == [red, port_call],
                   cleanup(( send(class(prolog_source_view),
                                  class_variable_value, call_style, Old),
                             send(F, destroy)
                           ))
                 ]) :-
    use_module(library(trace/gui)),
    get(class(prolog_source_view), class_variable, call_style, CV),
    get(CV, value, Old),
    new(F, frame),
    send(F, append, new(V, prolog_source_view)),
    send(F, open),
    new(Row, pce_class_variable_editor:cv_row(
                 class(prolog_source_view), class(prolog_source_view),
                 call_style)),
    send(Row, value, style(background := red)),
    send(Row, item_changed),
    get(V?editor?styles, value, call, Style),
    get(Style?background, name, Colour),
    get(Style?icon, name, Icon).

%   The thread monitor: the colours and pen of the graphs apply to an
%   open diagram, the graphs are edited using a toggle menu and the
%   update interval has a slider and a switch to turn updating off.

test(thread_monitor,
     [ [Colour, Pen, Item, Switch, Interval] == [red, 2.5, menu, @on, 1],
       cleanup(( forall(member(N-V, Old),
                        send(class(thread_diagram), class_variable_value,
                             N, V)),
                 send(class(prolog_thread_monitor), class_variable_value,
                      update_interval, OldInterval),
                 send(F, destroy)
               ))
     ]) :-
    use_module(library(swi/thread_monitor)),
    findall(N-V, ( member(N, [local_colour, graph_pen]),
                   get(class(thread_diagram), class_variable, N, CV),
                   get(CV, value, V)
                 ), Old),
    get(class(prolog_thread_monitor), class_variable, update_interval, ICV),
    get(ICV, value, OldInterval),
    new(F, frame),
    send(F, append, new(TM, prolog_thread_monitor)),
    send(F, open),
    send(TM, selection, main),
    get(@thread_diagrams, head, TD),
    new(R1, pce_class_variable_editor:cv_row(
                class(thread_diagram), class(thread_diagram), local_colour)),
    send(R1, value, colour(red)),
    send(R1, item_changed),
    new(R2, pce_class_variable_editor:cv_row(
                class(thread_diagram), class(thread_diagram), graph_pen)),
    send(R2, value, 2.5),
    send(R2, item_changed),
    get(TD, member, local, Graph),
    get(Graph?colour, name, Colour),
    get(Graph, pen, Pen),
    new(R3, pce_class_variable_editor:cv_row(
                class(prolog_thread_monitor), class(prolog_thread_monitor),
                graphs)),
    get(R3?item, class_name, Item),
    new(R4, pce_class_variable_editor:cv_row(
                class(prolog_thread_monitor), class(prolog_thread_monitor),
                update_interval)),
    get(R4?switch, active, Switch),
    send(R4, value, 1),
    send(R4, item_changed),
    get(TM, update_interval, Interval).

%   The refresh timer of an open navigator follows auto_refresh.

test(navigator, [ Interval == 5,
                  cleanup(( send(class(prolog_source_structure),
                                 class_variable_value, auto_refresh, Old),
                            send(F, destroy)
                          ))
                ]) :-
    use_module(library(trace/browse)),
    get(class(prolog_source_structure), class_variable, auto_refresh, CV),
    get(CV, value, Old),
    new(F, frame),
    send(F, append, new(SB, prolog_navigator)),
    send(F, open),
    new(Row, pce_class_variable_editor:cv_row(
                 class(prolog_source_structure),
                 class(prolog_source_structure), auto_refresh)),
    send(Row, value, 5),
    send(Row, item_changed),
    get(SB, tree, Tree),
    get(Tree?refresh_timer, interval, Interval).

test(rows_unique, [Unique == true]) :-
    new(D, dialog),
    send(D, append, new(B, button(hello))),
    send(D, open),
    new(E, class_variable_editor),
    send(E, edit, B),
    get(E?rows, map, @arg1?name, NameChain),
    chain_list(NameChain, Names),
    (   sort(Names, Sorted), length(Sorted, N), length(Names, N)
    ->  Unique = true
    ;   Unique = Names
    ),
    send(E, destroy),
    send(D, destroy).

:- end_tests(class_variable_editor_live).

%   The help links need the reference manual in Markdown.  It is not
%   available if the documentation is not built.

has_manual :-
    absolute_file_name(swi('xpce/man/refmanual/md'), _,
                       [ file_type(directory), access(read),
                         file_errors(fail)
                       ]).

doc(Class, Name, Markdown) :-
    pce_class_variable_editor:class_variable_doc(class(Class), Name,
                                                 Markdown).

:- begin_tests(class_variable_editor_doc, [condition(has_manual)]).

test(class_variable, true(sub_string(Doc, _, _, _, "Font used to display"))) :-
    doc(editor, font, Doc).
test(instance_variable, true(sub_string(Doc, _, _, _, "case"))) :-
    doc(editor, exact_case, Doc).
test(super_class, true(sub_string(Doc, _, _, _, "colour"))) :-
    doc(circle, colour, Doc).
test(no_entry, fail) :-
    doc(button, label_font, _).
test(entry_class_variable, [Class, Context] == [class_variable, graphical]) :-
    entry(box, selection_handles, Class, Context).
test(entry_instance_variable, [Class, Context] == [variable, editor]) :-
    entry(editor, exact_case, Class, Context).
test(entry_super_class, [Class, Context] == [variable, graphical]) :-
    entry(circle, colour, Class, Context).

entry(Class, Name, EntryClass, Context) :-
    pce_class_variable_editor:class_variable_entry(class(Class), Name,
                                                   Entry, _),
    get(Entry, class_name, EntryClass),
    get(Entry?context, name, Context).

%   A row without a manual entry hides its "?" link.  The link must keep
%   its width, or the item right of it moves.

test(hidden_link_keeps_width, Hidden == Shown) :-
    new(L, pce_class_variable_editor:link_label(help, '?')),
    get(L, width, Shown),
    send(L, show, @off),
    get(L, width, Hidden),
    free(L).

%   Sections are titled by the summary of the class.

test(class_summary, Summary == "EMACS look-alike text editor") :-
    pce_class_variable_editor:class_summary(class(editor), Summary).
test(no_class_summary, fail) :-
    pce_class_variable_editor:class_summary(class(tcve_box), _).

%   Descriptions of the values of a class variable come from a list of
%   values in the manual.  Here the entry refers to text_cursor<-style.

test(value_docs, true(sub_string(Doc, _, _, _, "vertical bar"))) :-
    pce_class_variable_editor:class_variable_value_docs(
        class(text_cursor), fixed_font_style, Docs),
    memberchk(bar-Doc, Docs).

%   The geometry of a window is not a preference that belongs in the
%   default view.

test(geometry_not_basic, fail) :-
    pce_class_variable_editor:basic_class_variable_(class(frame), geometry).

:- end_tests(class_variable_editor_doc).
