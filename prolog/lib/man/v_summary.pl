/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2026, University of Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(man_summary_browser, []).

:- use_module(library(pce)).
:- use_module(util).
:- use_module(library(help_message)).
:- require([ forall/2
           , send_list/3
           ]).


:- pce_begin_class(man_summary_browser(displayed_attribute, size), browser).

class_variable(header_font,     font,   bold).
class_variable(list_font,       font,   normal).

variable(displayed_attribute, name, get,
         "Get method for getting string").
variable(dynamic_columns, bool := @on, get,
         "Adapt the name column to the width of the window").

                /********************************
                *            CREATE             *
                ********************************/

initialise(S, Att:name, Size:size) :->
    "Create from displayed attribute and size"::
    DI = @arg1,
    new(Tool, ?(S, container, man_frame)),
    new(Obj, DI?object),

    get(S, list_font, ListFont),
    get(S, header_font, HeaderFont),

    send(S, send_super, initialise, @default, Size),
    send(S, slot, displayed_attribute, Att),
    send(S, font, ListFont),
    send(S, tab_stops, vector(20, 300, 325)),  % kind, name, book, summary
                                        % see ->update_columns
    send_list(S, [hor_stretch, hor_shrink], 100),
    send_list(S, [ver_stretch, ver_shrink], 1000),
    send(S, style, header, style(font := HeaderFont)),

    Ask = ?(S, container, man_frame),   % not <-frame: that is a window
    send(S, select_message,             % of the IDE now
         message(Ask, request_selection, Obj, @off)),
    send(S, open_message,
         message(Ask, request_selection, Obj, @on)),

    send(S, popup, new(P, popup(view, @nil))),
    send_list(P, append,
              [ menu_item(select,
                          message(Ask, request_selection, Obj, @on))
              , menu_item(class_browser,
                          message(Tool, request_tool_focus, Obj, @on),
                          @default, @on,
                          DI?style \== header)
              , menu_item(source,
                          message(Tool, request_source, Obj),
                          @default, @off,
                          and(message(Obj, has_send_method, has_source),
                              message(Obj, has_source)))
              , menu_item(spy,
                          message(Obj, break, full, @on),
                          @default, @off,
                          and(message(Obj, has_send_method, break),
                              Obj?break == @off))
              , menu_item(nospy,
                          message(Obj, break, full, @off),
                          @default, @off,
                          and(message(Obj, has_send_method, break),
                              Obj?break == @on))
              , menu_item(trace,
                          message(Obj, trace, full, @on),
                          @default, @off,
                          and(message(Obj, has_send_method, trace),
                              Obj?trace == @off))
              , menu_item(notrace,
                          message(Obj, trace, full, @off),
                          @default, @on,
                          and(message(Obj, has_send_method, trace),
                              Obj?trace == @on))
              ]).


dynamic_columns(S, Dynamic:bool) :->
    "Use fixed ->tab_stops (@off) or adapt them to the width"::
    send(S, slot, dynamic_columns, Dynamic),
    (   Dynamic == @off
    ->  send(S?dict, for_all, message(S, unfit_item, @arg1))
    ;   send(S, update_columns)
    ).


                /********************************
                *            COLUMNS            *
                ********************************/

%   The lines of the browser are "Kind\tName\tBook\tSummary".  The name
%   column is as wide as the widest name, but at most half the window.
%   Longer names are truncated with an ellipsis in the item's ->label,
%   keeping the full text in its <-key and showing the full name as
%   its tooltip.

name_stop(20).                          % left of the name column
column_gap(10).                         % white space after a column
max_name_fraction(0.5).                 % of the window width
min_name_width(150).

resize(S) :->
    "Adapt the columns to the new width"::
    send_super(S, resize),
    send(S, update_columns).

update_columns(S) :->
    "Set the tab stops and truncate names to the window width"::
    (   get(S, dynamic_columns, @on)
    ->  get(S, font, Font),
        get(S?text_image, width, Width),
        new(Widest, number(0)),
        send(S?dict, for_all,
             message(Widest, maximum, ?(S, name_width, @arg1, Font))),
        get(Widest, value, NameW),
        name_stop(Name),
        column_gap(Gap),
        max_name_fraction(Fraction),
        min_name_width(MinW),
        Avail is max(MinW, min(NameW, round(Width*Fraction)-Name-Gap)),
        Book is Name+Avail+Gap,
        get(Font, width, '\x1F4D6\', BookW),
        Summary is Book+BookW+Gap,
        send(S, tab_stops, vector(Name, Book, Summary)),
        send(S?dict, for_all, message(S, fit_item, @arg1, Font, Avail))
    ;   true
    ).

name_width(_S, DI:dict_item, Font:font, W:int) :<-
    "Width of the name column of DI in Font (0 for headers)"::
    (   summary_fields(DI, _Kind, Name, _Rest)
    ->  get(Font, width, Name, W)
    ;   W = 0
    ).

fit_item(_S, DI:dict_item, Font:font, Avail:int) :->
    "Truncate the name of DI with an ellipsis if wider than Avail"::
    (   summary_fields(DI, Kind, Name, Rest)
    ->  (   get(Font, width, Name, W),
            W =< Avail
        ->  Label = @default,
            Tip = @nil
        ;   ellipsis(Font, Name, Avail, Short),
            atomic_list_concat([Kind,Short|Rest], '\t', Text),
            new(Label, string('%s', Text)),
            new(Tip, string('%s', Name))
        ),
        get(DI, slot, label, Old),
        (   same_label(Old, Label)
        ->  true
        ;   send(DI, label, Label),
            set_tooltip(DI, Tip)
        )
    ;   true
    ).

unfit_item(_S, DI:dict_item) :->
    "Show the full name of DI"::
    (   summary_fields(DI, _, _, _)
    ->  send(DI, label, @default),
        set_tooltip(DI, @nil)
    ;   true
    ).

summary_fields(DI, Kind, Name, Rest) :-
    \+ get(DI, style, header),
    get(DI, key, Key),
    send(Key, instance_of, char_array),
    get(Key, value, Text),
    split_string(Text, "\t", "", [Kind, Name|Rest]).

set_tooltip(DI, @nil) :-
    !,
    (   get(DI, help_message, tag, _)
    ->  send(DI, help_message, tag, @nil)
    ;   true
    ).
set_tooltip(DI, Tip) :-
    send(DI, help_message, tag, Tip).

same_label(@default, @default) :- !.
same_label(L1, L2) :-
    L1 \== @default, L2 \== @default,
    send(L1, equal, L2).

%   ellipsis(+Font, +Text, +Avail, -Short)
%
%   Short is the longest prefix of Text followed by "…" that fits in
%   Avail pixels.

ellipsis(Font, Text, Avail, Short) :-
    string_length(Text, Len),
    longest_fit(Font, Text, Avail, 0, Len, L),
    sub_string(Text, 0, L, _, Prefix0),
    split_string(Prefix0, "", " ,", [Prefix]),
    string_concat(Prefix, "…", Short).

longest_fit(_, _, _, Lo, Hi, Lo) :-
    Hi =< Lo,
    !.
longest_fit(Font, Text, Avail, Lo, Hi, L) :-
    Mid is (Lo+Hi+1)//2,
    sub_string(Text, 0, Mid, _, Prefix),
    string_concat(Prefix, "…", Try),
    get(Font, width, Try, W),
    (   W =< Avail
    ->  longest_fit(Font, Text, Avail, Mid, Hi, L)
    ;   Hi1 is Mid-1,
        longest_fit(Font, Text, Avail, Lo, Hi1, L)
    ).


                /********************************
                *         COMMUNICATION         *
                ********************************/

selected(S, Obj:object*) :->
    "Set selection to requested object"::
    (   Obj == @nil
    ->  send(S, selection, @nil)
    ;   get(S?dict, find, @arg1?object == Obj, Di)
    ->  send(S, selection, Di),
        send(S, normalise, Di)
    ;   send(S, selection, @nil)
    ).


release_selection(S) :->
    "Unselect all objects"::
    send(S, selection, @nil).


                /********************************
                *        FILL THE BROWSER       *
                ********************************/

append_card(S, What:object) :->
    "Append a single card"::
    send(S, append,
         dict_item(What?(S?displayed_attribute), @default, What)).


append_group(S, Index:int, Group:name, Members:chain) :->
    "Append group of cards"::
    (   get(@manual, module, groups, @on, Module),
        get(Module?id_table, member, Group, GroupCard),
        get(GroupCard, summary, Summary),
        Summary \== @nil
    ->  new(Label, string('%d\t%s\t%s', Index, Group, Summary))
    ;   new(Label, string('%d\t%s', Index, Group))
    ),
    send(S, append,
         dict_item(Group, Label, style := header, object := Members)),
    send(Members, for_all,
         message(S, append_card, @arg1)).


members(S, Matches:chain, First:[object]) :->
    "Display chain of cards, First (a member) on top"::
    send(S, clear),
    (   send(Matches, empty)
    ->  send(S, report, warning, 'No match')
    ;   send(S, report, progress, 'Grouping ...'),
        group_objects(Matches, Groups),
        (   First == @default
        ->  true
        ;   put_first(Groups, First)
        ),
        send(S, report, done),
        new(Index, number(1)),
        send(Groups, for_all,
             and(message(S, append_group, Index, @arg1?name, @arg1?value),
                 message(Index, plus, 1))),
        send(S, update_columns),
        (   get(S, container, man_frame, Tool),
            get(Tool, manual, Manual),
            Manual \== @nil
        ->  send(S, selected, Manual?selection)
        ;   true
        )
    ).


%   put_first(+Groups, +Card)
%
%   Move the group that holds Card to the front and Card to the front
%   of this group.

put_first(Groups, Card) :-
    get(Groups, members, Attributes),
    (   get(Attributes, find, message(@arg1?value, member, Card), Group)
    ->  get(Group, value, Cards),
        send(Cards, delete, Card),
        send(Cards, prepend, Card),
        send(Attributes, delete, Group),
        send(Attributes, prepend, Group)
    ;   true
    ).


update(S, What:object) :->
    "Update selection for object"::
    get(S, displayed_attribute, Att),
    get(S?members, find_all, @arg1?object == What, DictItems),
    send(DictItems, for_all, message(@arg1, key, @arg1?object?Att)),
    send(S, update_columns).


                /********************************
                *            CHANGES            *
                ********************************/

update_card(S, Obj:object) :->
    "Update the value for specified card"::
    (   get(S?dict, find, @arg1?object == Obj, Di)
    ->  send(Di, key, Obj?(S?displayed_attribute)),
        send(S, update_columns)
    ;   true
    ).


:- pce_end_class.
