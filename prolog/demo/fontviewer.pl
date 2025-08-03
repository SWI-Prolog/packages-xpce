/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

:- module(pce_fontviewer,
          [ fontviewer/0
          ]).
:- use_module(library(pce)).
:- use_module(library('unicode/blocks')).
:- use_module(library(apply)).
:- use_module(library(pce_util)).
:- use_module(library(pce_report)).

:- require([ between/3
           ]).

fontviewer :-
    send(new(font_browser), open).

:- pce_begin_class(font_browser, frame,
                   "Browse available fonts").

variable(fonts,     sheet, get, "All available fonts").
variable(selection, font*, get, "Currently selected font").

initialise(FB) :->
    send_super(FB, initialise, 'Font Browser'),
    get(@pce, convert, normal, font, _Normal), % Initialise class font.
    get(class(font), font_families, Fonts),
    send(FB, slot, fonts, Fonts),
    send(FB, append, new(D, dialog)),
    send(new(P, window(size := size(350,350))), below, D),
    send(P, name, preview),
    send(new(report_dialog), below, P),

    new(FontMsg, message(FB, font_selected)),
    new(StyleMsg, message(FB, style_selected)),
    new(FilterMsg, message(FB, filter)),
    new(ChartMsg,  message(FB, show_block, @arg1)),
    new(AliasMsg,  message(FB, alias_selected, @arg1)),

    send(D, append, new(_Filter, text_item(filter, '', FilterMsg))),
    send(D, append, new(Type,   menu(type,   cycle, FilterMsg)), right),
    send(D, append, new(Alias,  menu(alias,  cycle, AliasMsg)), next_row),
    send(D, append, new(ALBL,   label(alias_msg, "Show font from alias")), right),
    send(D, append, new(Family, menu(family, cycle, FontMsg)), next_row),
    send(D, append, new(Style,  menu(style,  cycle, StyleMsg)),  right),
    send(D, append, new(Weight, menu(weight, cycle, FontMsg)), right),
    send(D, append, new(Points, menu(points, cycle, FontMsg)), right),
    send(new(ChartMenu, menu(unicode_block, cycle, ChartMsg)), below, Family),
    send_list([Type, Style,Weight,Points,ALBL], alignment, left),

    send_list(Type, append, [any, proportional, monospace]),
    font_aliases(Aliases),
    set_menu_options(Alias, Aliases),
    font_families(FB, new(and), Families),
    set_menu_options(Family, Families),
    set_menu_options(Style, [normal,italic,oblique,bold]),
    findall(W, weight(W), Weights),
    set_menu_options(Weight, Weights),
    set_menu_options(Points, [8,9,10,12,14,18,24]),
    forall(unicode_block(Name, _, _),
           send(ChartMenu, append, menu_item(Name, @default, Name))),

    send(FB, alias_selected, normal).

%!  set_menu_options(+Menu, +Options:list) is det.
%
%   Fill a menu without invoking the default label transformation.

set_menu_options(Menu, Options) :-
    forall(member(Opt, Options),
           send(Menu, append, menu_item(Opt, @default, Opt))).

font_families(FB, Cond, Families) :-
    get(FB, fonts, Fonts),
    new(Ch, chain),
    send(Fonts, for_all,
         if(Cond, message(Ch, append, @arg1?name))),
    chain_list(Ch, Families).


% Filter available fonts on a string and type condition.

filter(FB) :->
    "Filter the families"::
    get(FB, member, dialog, D),
    get(D, member, filter, FilterItem), get(FilterItem, selection, Text),
    get(D, member, type, TypeItem), get(TypeItem, selection, Type),
    filter_condition(Text, Type, Cond),
    get(D, member, family, FamItem),
    ignore(get(FamItem, selection, Fam0)),
    send(FamItem, clear),
    font_families(FB, Cond, Families),
    set_menu_options(FamItem, Families),
    (   nonvar(Fam0),
        get(FamItem, member, Fam0, Item0)
    ->  send(FamItem, selection, Item0)
    ;   true
    ),
    get(FamItem?members, size, MatchCount),
    send(D, report, status, '%d matches', MatchCount),
    send(FB, font_selected).

filter_condition('', any, Cond) =>
    new(Cond, and).
filter_condition('', monospace, Cond) =>
    new(Cond, message(@arg1?value, is_attribute, monospace)).
filter_condition('', proportional, Cond) =>
    new(Cond, not(message(@arg1?value, is_attribute, monospace))).
filter_condition(Text, any, Cond) =>
    new(Re, regex(Text, @off)),
    new(Cond, message(Re, search, @arg1?name)).
filter_condition(Text, Type, Cond) =>
    filter_condition('', Type, TypeCond),
    filter_condition(Text, any, TextCond),
    new(Cond, and(TypeCond, TextCond)).

style_selected(FB) :->
    "User changed style"::
    get(FB, member, dialog, D),
    get(D, member, weight,        WgtItem),
    get(D, member, style,         StyItem), get(StyItem, selection, Style),
    (   Style == bold
    ->  send(WgtItem, selection, bold),
        send(WgtItem, active, @off)
    ;   send(WgtItem, selection, normal),
        send(WgtItem, active, @on)
    ),
    send(FB, font_selected).

%!  font_aliases(-Aliases:list(atom)) is det.
%
%   Extract all defined font aliases, preserving the order as much as
%   possible.

font_aliases(Aliases) :-
    font_aliases(system_fonts, SystemAliases),
    (   font_aliases(user_fonts, UserAliases)
    ->  subtract(UserAliases, SystemAliases, NewAliases),
        append(SystemAliases, NewAliases, ResourceAliases)
    ;   ResourceAliases = SystemAliases
    ),
    hashed_aliases(Hashed),
    subtract(Hashed, ResourceAliases, Added),
    sort(Added, SortedAdded),
    append(ResourceAliases, SortedAdded, Aliases).

font_aliases(Resource, Aliases) :-
    get(class(font), class_variable, Resource, SF),
    get(SF, value, Chain),
    get_chain(Chain, map(@arg1?name), Aliases).

hashed_aliases(Hashed) :-
    new(Ch, chain),
    send(@font_aliases, for_all, message(Ch, append, @arg1)),
    chain_list(Ch, Hashed).

alias_selected(FB, Alias:name) :->
    "Show details on an alias"::
    get(FB, member, dialog, D),
    get(D, member, family, Family),
    get(D, member, style,  Style),
    get(D, member, weight, Weight),
    get(D, member, points, Points),

    get(@pce, convert, Alias, font, Font),

    ignore(send(Style,  selection, Font?style)),
    ignore(send(Weight, selection, Font?weight)),
    ignore(send(Points, selection, Font?points)),
    ignore((get(Font, pango_property, family, DefFam),
            send(Family, selection, DefFam))),
    send(FB, font_selected).

font_selected(FB) :->
    "One of the font selection menus changed"::
    get(FB, member, dialog, D),
    get(D, member, family,        FamItem), get(FamItem, selection, Family),
    get(D, member, style,         StyItem), get(StyItem, selection, Style),
    get(D, member, weight,        WgtItem), get(WgtItem, selection, Weight),
    get(D, member, points,        PtsItem), get(PtsItem, selection, Points),
    new(Font, font(Family,Style,Points,Weight)),
    send(FB, slot, selection, Font),
    send(FB, update_block_menu, Font),
    get(D, member, unicode_block, CrtItem), get(CrtItem, selection, Chart),
    send(FB, show_font, Font, Chart).


update_block_menu(FB, Font:font) :->
    "Update block menu with pages covered by the font"::
    get(FB, member, dialog, D),
    get(D, member, unicode_block, CrtItem), get(CrtItem, selection, Chart0),
    send(CrtItem, clear),
    forall(font_supports_unicode_block(Font, Page),
           send(CrtItem, append, Page)),
    (   get(CrtItem, member, Chart0, Item)
    ->  send(CrtItem, selection, Item)
    ;   true
    ).

show_block(FB, Chart:name) :->
    "Show Unicode named page"::
    get(FB, selection, Font),
    send(FB, show_font, Font, Chart).

show_font(FB, Font:font, Chart:name) :->
    "Show a particular Unicode page for a font"::
    send(FB, report_font, Font),
    get(FB, member, preview, P),
    unicode_block(Chart, From, To),
    send(P, clear),
    new(F, format(horizontal, 2, @on)),
    send(F, row_sep, 5),
    send(P, format, F),
    new(A, string(x)),
    MaxRow is ((To-From)//15)-1,
    (   between(0, MaxRow, Y),
        I is Y*16+From,
        has_chars_in_row(Font, I),
        send(P, display,
             text(string('0x%02x/%03d:', I, I), left, fixed)),
        new(S, string),
        (   between(0, 15, X),
            C is I+X,
            C \== 0, C \== 9, C \== 10, C \== 13,
            (   send(Font, member, C)
            ->  send(A, character, 0, C),
                send(S, append, A)
            ;   true
            ),
            fail
        ;   send(P, display, font_text(S, left, Font))
        ),
        fail
    ;   true
    ).

has_chars_in_row(Font, From) :-
    between(0, 15, X),
    C is From+X,
    \+ hide_char(C),
    send(Font, member, C), !.

hide_char(C) :- code_type(C, white), !.
hide_char(0x00A0).               % non-breaking space
hide_char(0x200b).               % zero-width space

report_font(FB, Font:font) :->
    "Report font in xpce notation"::
    get(Font, family, Family),
    get(Font, style, Style),
    get(Font, weight, Weight),
    get(Font, points, Points),
    font_term(Family, Style, Weight, Points, Term),
    term_string(Term, String),
    send(@display, copy, String),
    send(FB, report, status, 'Copied: %s', String).

font_term(Family, bold, bold, Points, Term) =>
    Term = font(Family, bold, Points).
font_term(Family, Style, normal, Points, Term) =>
    Term = font(Family, Style, Points).
font_term(Family, Style, Weight, Points, Term) =>
    Term = font(Family, Style, Points, Weight).

:- pce_end_class.

:- pce_begin_class(font_text, text,
                   "Show current character").

event(FT, Ev:event) :->
    (   send_super(FT, event, Ev)
    ->  true
    ;   send(Ev, is_a, area_exit)
    ->  send(FT, report, status, '')
    ;   get(FT, pointed, Ev, Index),
        End is Index+1,
        get(FT?string, sub, Index, End, S),
        get(FT?string, character, Index, C),
        (   send(Ev, is_a, ms_left_up),
            send(@display, copy, S)
        ->  Msg = "Copied "
        ;   Msg = ""
        ),
        send(FT, report, status, '%s%s = 0x%02x/%03d', Msg, S, C, C)
    ).

:- pce_end_class.

%!  font_supports_unicode_block(+Font, ?Block) is nondet.

font_supports_unicode_block(Font, Block) :-
    var(Block),
    !,
    get(Font, domain, tuple(DS, DE)),
    unicode_block(Block, Start, End),
    DS =< End,
    DE >= Start,
    (   between(Start, End, Char),
        \+ hide_char(Char),
        send(Font, member, Char)
    ->  true
    ).
font_supports_unicode_block(Font, Block) :-
    unicode_block(Block, Start, End),
    between(Start, End, Char),
    send(Font, member, Char),
    \+ hide_char(Char),
    !.

weight(thin).
weight(ultralight).
weight(light).
weight(semilight).
weight(book).
weight(normal).
weight(medium).
weight(bold).
weight(ultrabold).
weight(heavy).
weight(ultraheavy).
