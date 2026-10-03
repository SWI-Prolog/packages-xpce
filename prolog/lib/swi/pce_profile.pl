/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2003-2019, University of Amsterdam
                              VU University Amsterdam
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

:- module(pce_profile,
          [ pce_show_profile/0
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_theme), [theme_colours/1]).

:- theme_colours([ prof_header_background = khaki1,
                   prof_node              = blue
                 ]).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(yall)).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(pane_frame)).
:- use_module(library(toolbar)).
:- use_module(library(tabular)).
:- use_module(library(prolog_predicate)).
:- use_module(library(tabbed_window), []).
:- use_module(library(xdot), []).
:- use_module(library(pce_filter_item), []).

:- require([ auto_call/1,
	     reset_profiler/0,
	     is_dict/1,
	     profile_data/1,
	     www_open_url/1,
	     pi_head/2,
	     predicate_label/2,
	     predicate_sort_key/2,
	     get_chain/3,
	     send_list/3
	   ]).

/** <module> GUI frontend for the profiler

This module hooks into profile/1 and  provides   a  graphical UI for the
profiler output.
*/

%!  pce_show_profile is det.
%
%   Show already collected profile using a graphical browser.

pce_show_profile :-
    profile_data(Data),
    in_pce_thread(show_profile(Data)).

show_profile(Data) :-
    send(new(F, prof_frame), open),
    send(F, load_profile, Data).

%!  prof_tool(+Object, -Tool) is semidet.
%
%   The profiler a window or graphical of it belongs to.  They used to
%   reach it with <-frame; the frame is a window of the IDE now and the
%   profiler is the pane in it.

prof_tool(Obj, Tool) :-
    get(Obj, container, prof_frame, Tool).


                 /*******************************
                 *             FRAME            *
                 *******************************/

/* The profiler as a pane.

It used to be a frame of its own, holding the list of predicates, the
details, a menu bar and a reporter.  It is a `tool_pane' now -- see
library(pane_frame) -- so it sits in a tab of a window of the IDE beside
a terminal, an editor or another tool, and what it has to say goes on the
status bar of that window.  Every profile still opens one of its own, as
it did when each was a frame.
*/

:- pce_begin_class(prof_frame, tool_pane,
                   "Show Prolog profile data").

variable(samples,          int,  get, "Total # samples").
variable(ticks,            int,  get, "Total # ticks").
variable(accounting_ticks, int,  get, "# ticks while accounting").
variable(time,             real, get, "Total time").
variable(nodes,            int,  get, "Nodes created").
variable(ports,            {true,false,classic},  get, "Port mode").
variable(time_view,        {percentage,seconds} := percentage,
                                 get, "How time is displayed").

class_variable(auto_reset, bool, @on, "Reset profiler after collecting").

initialise(F) :->
    send_super(F, initialise, profiler),
    send(F, append_window, new(B, prof_browser)),
    send(F, append_window, new(prof_tabs), B, right),
    send(F, append_window, new(prof_filter_dialog), B, above).

                 /*******************************
                 *             PANE             *
                 *******************************/

pane_label(_F, Label:name) :<-
    "What my tab is called"::
    Label = 'Profile'.

menu_bar_key(_F, Key:name) :<-
    "Every profile asks for the same menu bar"::
    Key = profiler.

%       One popup of my own rather than items on the menus of the window:
%       how the profile is sorted and how its times are read are about
%       the profile, not about the window it happens to be in.

fill_menu_bar(F, MD:tool_dialog) :->
    "Put my menu on the bar of the window I am in"::
    get(MD, popup, profile, @on, Popup),
    send(Popup, append, new(Sort, popup(sort_by))),
    forall(sort_by(Label, Field, Order),
           send(Sort, append,
                menu_item(Label, message(F, sort_by, Field, Order)))),
    send(Popup, append, new(Time, popup(show_time_as))),
    get(F?class, instance_variable, time_view, TV),
    get(TV, type, Type),
    get_chain(Type, value_set, Values),
    forall(member(TimeView, Values),
           send(Time, append,
                menu_item(TimeView, message(F, time_view, TimeView)))),
    send(Popup, append, menu_item(help, message(F, help))).


load_profile(F, ProfData0:[prolog]) :->
    "Load stored profile from the Prolog database"::
    (   is_dict(ProfData0)
    ->  ProfData = ProfData0
    ;   profile_data(ProfData)
    ),
    Summary = ProfData.summary,
    send(F, slot, samples, Summary.samples),
    send(F, slot, ticks, Summary.ticks),
    send(F, slot, accounting_ticks, Summary.accounting),
    send(F, slot, time, Summary.time),
    send(F, slot, nodes, Summary.nodes),
    send(F, slot, ports, Summary.ports),
    get(F, window, prof_browser, B),
    send(F, report, progress, 'Loading profile data ...'),
    send(B, load_profile, ProfData.nodes),
    send(B, fit_width),
    send(F, report, done),
    send(F, show_statistics),
    send(B, select_interesting),
    (   get(F, auto_reset, @on)
    ->  reset_profiler
    ;   true
    ).


show_statistics(F) :->
    "Show basic statistics on profile"::
    get(F, samples, Samples),
    get(F, ticks, Ticks),
    get(F, accounting_ticks, Account),
    get(F, time, Time),
    get(F, slot, nodes, Nodes),
    get(F, window, prof_browser, B),
    get(B?all_items, size, Predicates),
    (   Ticks == 0
    ->  Distortion = 0.0
    ;   Distortion is 100.0*(Account/Ticks)
    ),
    send(F, report, inform,
         '%d samples in %.2f sec; %d predicates; \c
              %d nodes in call-graph; distortion %.0f%%',
         Samples, Time, Predicates, Nodes, Distortion).


details(F, From:prolog) :->
    "Show details on node or predicate"::
    (   is_dict(From)
    ->  Node = From
    ;   get(F, node_data, From, Node)
    ),
    get(F, window, prof_browser, B),
    send(B, current, Node),
    get(F, window, prof_details, W),
    send(W, node, Node),
    get(F, window, prof_graph, G),
    send(G, node, Node).

node_data(F, Pred:prolog, Node:prolog) :<-
    "The profile data of a predicate; fails if it was not sampled"::
    get(F, window, prof_browser, B),
    get(B?all_items, find,
        message(@arg1, has_predicate, prolog(Pred)),
        DI),
    get(DI, data, Node).

sort_by(F, SortBy:name, Order:[{normal,reverse}]) :->
    "Define the key for sorting the flat profile"::
    get(F, window, prof_browser, B),
    send(B, sort_by, SortBy, Order).

time_view(F, TV:name) :->
    send(F, slot, time_view, TV),
    get(F, window, prof_browser, B),
    get(F, window, prof_details, W),
    get(F, window, prof_graph, G),
    send(B, update_labels),
    send(W, refresh),
    send(G, refresh).

render_time(F, Ticks:int, Rendered:any) :<-
    "Render a time constant"::
    get(F, time_view, View),
    (   View == percentage
    ->  get(F, ticks, Total),
        get(F, accounting_ticks, Accounting),
        (   Total-Accounting =:= 0
        ->  Rendered = '0.0%'
        ;   Percentage is 100.0 * (Ticks/(Total-Accounting)),
            new(Rendered, string('%.1f%%', Percentage))
        )
    ;   View == seconds
    ->  get(F, ticks, Total),
        (   Total == 0
        ->  Rendered = '0.0 s.'
        ;   get(F, time, TotalTime),
            Time is TotalTime*(Ticks/float(Total)),
            new(Rendered, string('%.2f s.', Time))
        )
    ).

help(_) :->
    "Open help (web site)"::
    www_open_url('https://github.com/SWI-Prolog/packages-xpce/wiki/profiler').

:- pce_end_class(prof_frame).


                 /*******************************
                 *            FILTER            *
                 *******************************/

:- pce_begin_class(prof_filter_dialog, dialog,
                   "Filter the predicates of the flat profile").

class_variable(border, size, size(0,0)).

initialise(D) :->
    send_super(D, initialise),
    send(D, gap, size(5, 2)),
    send(D, pen, 0),
    send(D, hor_stretch, 100),          % the browser decides the width
    send(D, hor_shrink, 100),
    send(D, append,
         new(F, filter_item(filter, message(D, filter, @arg1),
                            "Filter predicates"))),
    send(F, show_label, @off).

resize(D) :->
    send(D, layout, D?visible?size).

filter(D, Filter:regex*) :->
    "Only show the predicates that match Filter"::
    prof_tool(D, Tool),
    get(Tool, window, prof_browser, B),
    send(B, filter, Filter).

:- pce_end_class(prof_filter_dialog).


                 /*******************************
                 *     FLAT PROFILE BROWSER     *
                 *******************************/

/* The browser holds all predicates in <-all_items, sorted, and shows
those that match <-filter.  Lookups by predicate go through
<-all_items, so the details and the call graph reach predicates that
the filter hides.
*/

:- pce_begin_class(prof_browser, browser,
                   "Show flat profile in browser").

class_variable(size, size, size(40,20)).
class_variable(max_width, '1..100', 60,
               "Most percentage of the profiler ->fit_width takes").

variable(sort_by,   name := ticks, get, "How the items are sorted").
variable(all_items, chain,         get, "All items, shown or not").
variable(filter,    regex*,        get, "Only show items matching this").
variable(current,   prolog*,       get, "Node shown by the details").

initialise(B) :->
    send_super(B, initialise),
    send(B, slot, all_items, new(chain)),
    send(B, update_label),
    send(B, select_message, message(@arg1, details)).

resize(B) :->
    send_super(B, resize),
    get(B?text_image, width, W),
    get(B?font, width, '100.0%', ColW),
    send(B, tab_stops, vector(W-ColW-15)).

%       The column holding me and the filter above me is made as wide as
%       the longest predicate, a space and the widest time, "100.0%".
%       Not wider than max_width of the profiler, though: one long name
%       would push the details off the window.

fit_width(B) :->
    "Make my column as wide as my longest predicate"::
    get(B, font, Font),
    new(Max, number(0)),
    send(B?all_items, for_all,
         message(Max, maximum, ?(Font, width, @arg1?key))),
    get(Max, value, KeyW),
    get(Font, width, ' 100.0%', ColW),
    get(B?scroll_bar, width, SBW),      % <-text_image is not laid out yet
    Wanted is KeyW + ColW + 15 + SBW + 10,
    (   get(B, tile, Tile),
        column_tile(Tile, Column, Row)
    ->  get(Row?area, width, RowW),
        get(B, class_variable_value, max_width, MaxPct),
        Width is min(Wanted, round(RowW*MaxPct/100)),
        send(Column, width, Width)
    ;   send(B, width, Wanted)
    ).

%   column_tile(+Tile, -Column, -Row) is semidet.
%
%   Column is the tile that holds Tile and sits in the horizontal Row.

column_tile(Tile, Column, Row) :-
    get(Tile, super, Super),
    Super \== @nil,
    (   get(Super, orientation, horizontal)
    ->  Column = Tile,
        Row = Super
    ;   column_tile(Super, Column, Row)
    ).

load_profile(B, Nodes:prolog) :->
    "Load stored profile from the Prolog database"::
    prof_tool(B, Frame),
    get(B, sort_by, SortBy),
    get(B, all_items, All),
    forall(member(Node, Nodes),
           send(All, append, prof_dict_item(Node, SortBy, Frame))),
    send(B, sort).

select_interesting(B) :->
    "Select the most interesting predicate and show its details"::
    get(B, sort_by, SortBy),
    get_chain(B?dict, members, Items),
    prof_tool(B, F),
    (   interesting_item(SortBy, Items, F, DI)
    ->  send(DI, details)               % selects DI; see ->current
    ;   true
    ).

%   interesting_item(+SortBy, +Items, +Frame, -Item) is semidet.
%
%   In a cumulative profile the top is a chain of predicates that are
%   active (nearly) all the time, such as the goal being profiled.  The
%   interesting one is the first below that, which we take to be the
%   first using less than 90% of the time.  Otherwise it is the first.

interesting_item(SortBy, Items, F, DI) :-
    cumulative_key(SortBy),
    !,
    get(F, ticks, Total),
    get(F, accounting_ticks, Accounting),
    Limit is 0.9*(Total-Accounting),
    (   member(DI, Items),
        get(DI, value, SortBy, Ticks),
        Ticks < Limit
    ->  true
    ;   Items = [DI|_]
    ).
interesting_item(_, [DI|_], _, DI).

cumulative_key(ticks).
cumulative_key(ticks_siblings).

update_label(B) :->
    get(B, sort_by, Sort),
    sort_by(Human, Sort, _How),
    send(B, label, Human?label_name).

sort_by(B, SortBy:name, Order:[{normal,reverse}]) :->
    "Define key on which to sort"::
    send(B, slot, sort_by, SortBy),
    send(B, update_label),
    send(B, sort, Order),
    send(B, update_labels).

sort(B, Order:[{normal,reverse}]) :->
    get(B, sort_by, Sort),
    (   Order == @default
    ->  sort_by(_, Sort, TheOrder)
    ;   TheOrder = Order
    ),
    get(B, all_items, All),
    send(All, sort, ?(@arg1, compare, @arg2, Sort, TheOrder)),
    send(B, show_items).

filter(B, Filter:regex*) :->
    "Only show the predicates whose label matches Filter"::
    send(B, slot, filter, Filter),
    send(B, show_items).

%       The items stay in <-all_items, so taking them out of the
%       dictionary does not destroy them.

show_items(B) :->
    "Show the items of <-all_items that match <-filter"::
    send(B?dict, clear),
    get(B, all_items, All),
    get(B, filter, Filter),
    (   Filter == @nil
    ->  send(All, for_all, message(B, append, @arg1))
    ;   send(All, for_all,
             if(message(Filter, search, @arg1?key),
                message(B, append, @arg1)))
    ),
    get(B, current, Node),
    send(B, current, Node).

%       The selection follows the current node, wherever it was made
%       current: here, in the details or in the call graph.  If the
%       filter hides it, nothing is selected.

current(B, Node:prolog*) :->
    "Make Node current and select it if it is shown"::
    send(B, slot, current, Node),
    (   Node \== @nil,
        get(B?dict, find, message(@arg1, is_node, prolog(Node)), DI)
    ->  send(B, selection, DI),
        send(B, normalise, DI)
    ;   send(B, selection, @nil)
    ).

update_labels(B) :->
    "Update labels of predicates"::
    get(B, sort_by, SortBy),
    prof_tool(B, F),
    send(B?all_items, for_all, message(@arg1, update_label, SortBy, F)).

:- pce_end_class(prof_browser).

:- pce_begin_class(prof_dict_item, dict_item,
                   "Show entry of Prolog flat profile").

variable(data,         prolog, get, "Predicate data").

initialise(DI, Node:prolog, SortBy:name, F:prof_frame) :->
    "Create from predicate head"::
    send(DI, slot, data, Node),
    pce_predicate_label(Node.predicate, Key),
    send_super(DI, initialise, Key),
    send(DI, update_label, SortBy, F).

value(DI, Name:name, Value:prolog) :<-
    "Get associated value"::
    get(DI, data, Data),
    value(Name, Data, Value).

is_node(DI, Node:prolog) :->
    "True if I show Node"::
    get(DI, data, Data),
    Data.predicate == Node.predicate.

has_predicate(DI, Test:prolog) :->
    get(DI, data, Data),
    same_pred(Test, Data.predicate).

same_pred(X, X) :- !.
same_pred(QP1, QP2) :-
    unqualify(QP1, P1),
    unqualify(QP2, P2),
    same_pred_(P1, P2).

unqualify(user:X, X) :- !.
unqualify(X, X).

same_pred_(X, X) :- !.
same_pred_(Head, Name/Arity) :-
    pi_head(Name/Arity, Head).
same_pred_(Head, user:Name/Arity) :-
    pi_head(Name/Arity, Head).

compare(DI, DI2:prof_dict_item,
        SortBy:name, Order:{normal,reverse},
        Result:name) :<-
    "Compare two predicate items on given key"::
    get(DI, value, SortBy, K1),
    get(DI2, value, SortBy, K2),
    (   Order == normal
    ->  get(K1, compare, K2, Result)
    ;   get(K2, compare, K1, Result)
    ).

update_label(DI, SortBy:name, F:prof_frame) :->
    "Update label considering sort key and frame"::
    get(DI, key, Key),
    (   SortBy == name
    ->  send(DI, update_label, ticks_self, F)
    ;   get(DI, value, SortBy, Value),
        (   time_key(SortBy)
        ->  get(F, render_time, Value, Rendered)
        ;   Rendered = Value
        ),
        send(DI, label, string('%s\t%s', Key, Rendered))
    ).

time_key(ticks).
time_key(ticks_self).
time_key(ticks_children).

details(DI) :->
    "Show details"::
    get(DI, data, Data),
    prof_tool(DI?dict?browser, Tool),
    send(Tool, details, Data).

:- pce_end_class(prof_dict_item).


                 /*******************************
                 *         DETAIL WINDOW        *
                 *******************************/

:- pce_begin_class(prof_details, window,
                   "Table showing profile details").

variable(tabular, tabular, get, "Displayed table").
variable(node,    prolog,  get, "Currently shown node").

class_variable(background,        colour, ui_dialog_background).
class_variable(header_colour,     colour, black,  "Predicate header colour").
class_variable(header_background, colour, prof_header_background,
               "Predicate header background").

%       No label: a label puts a row of its own on the window_decorator I
%       am held in, and the grip that drags the profiler around lands in
%       it.  The predicate the details are about is the row the table
%       writes in bold -- see ->show_predicate -- so nothing is lost.

initialise(W) :->
    send_super(W, initialise),
    send(W, pen, 0),
    send(W, scrollbars, vertical),
    send(W, restrict_scroll, @on),
    send(W, display, new(T, tabular)),
    send(T, rules, all),
    send(T, cell_spacing, -1),
    send(W, slot, tabular, T).

resize(W) :->
    send_super(W, resize),
    get(W?visible, width, Width),
    send(W?tabular, table_width, Width-3).

title(W) :->
    "Show title-rows"::
    get(W, class_variable_value, header_colour, HC),
    get(W, class_variable_value, header_background, HBG),
    get(W, tabular, T),
    BG = (background := HBG),
    FG = (colour := HC),
    send(T, append, 'Time',   bold, center, colspan := 2, BG, FG),
    (   prof_tool(W, Tool), get(Tool, ports, false)
    ->  send(T, append, '# Calls', bold, center, colspan := 1,
             valign := center, BG, FG, rowspan := 2)
    ;   send(T, append, 'Port',    bold, center, colspan := 4, BG, FG)
    ),
    send(T, append, 'Predicate', bold, center,
         valign := center, BG, FG,
         rowspan := 2),
    send(T, next_row),
    send(T, append, 'Self',   bold, center, BG, FG),
    send(T, append, 'Children',   bold, center, BG, FG),
    (   prof_tool(W, Tool), get(Tool, ports, false)
    ->  true
    ;   send(T, append, 'Call',   bold, center, BG, FG),
        send(T, append, 'Redo',   bold, center, BG, FG),
        send(T, append, 'Exit',   bold, center, BG, FG),
        send(T, append, 'Fail',   bold, center, BG, FG)
    ),
    send(T, next_row).

cluster_title(W, Cycle:int) :->
    get(W, tabular, T),
    (   prof_tool(W, Tool), get(Tool, ports, false)
    ->  Colspan = 4
    ;   Colspan = 7
    ),
    send(T, append, string('Cluster <%d>', Cycle),
         bold, center, colspan := Colspan,
         background := navyblue, colour := yellow),
    send(T, next_row).

refresh(W) :->
    "Refresh to accomodate visualisation change"::
    (   get(W, node, Data),
        Data \== @nil
    ->  send(W, node, Data)
    ;   true
    ).

node(W, Data:prolog) :->
    "Visualise a node"::
    send(W, slot, node, Data),
    send(W?tabular, clear),
    send(W, scroll_to, point(0,0)),
    send(W, title),
    clusters(Data.callers, CallersCycles),
    clusters(Data.callees, CalleesCycles),
    (   CallersCycles = [_]
    ->  show_clusters(CallersCycles, CalleesCycles, Data, 0, W)
    ;   show_clusters(CallersCycles, CalleesCycles, Data, 1, W)
    ).

show_clusters([], [], _, _, _) :- !.
show_clusters([P|PT], [C|CT], Data, Cycle, W) :-
    show_cluster(P, C, Data, Cycle, W),
    Next is Cycle+1,
    show_clusters(PT, CT, Data, Next, W).
show_clusters([P|PT], [], Data, Cycle, W) :-
    show_cluster(P, [], Data, Cycle, W),
    Next is Cycle+1,
    show_clusters(PT, [], Data, Next, W).
show_clusters([], [C|CT], Data, Cycle, W) :-
    show_cluster([], C, Data, Cycle, W),
    Next is Cycle+1,
    show_clusters([], CT, Data, Next, W).


show_cluster(Callers, Callees, Data, Cycle, W) :-
    (   Cycle == 0
    ->  true
    ;   send(W, cluster_title, Cycle)
    ),
    sort_relatives(Callers, Callers1),
    show_relatives(Callers1, parent, W),
    ticks(Callers1, Self, Children, Call, Redo, Exit),
    send(W, show_predicate, Data, Self, Children, Call, Redo, Exit),
    sort_relatives(Callees, Callees1),
    reverse(Callees1, Callees2),
    show_relatives(Callees2, child, W).

ticks(Callers, Self, Children, Call, Redo, Exit) :-
    ticks(Callers, 0, Self, 0, Children, 0, Call, 0, Redo, 0, Exit).

ticks([], Self, Self, Sibl, Sibl, Call, Call, Redo, Redo, Exit, Exit).
ticks([H|T],
      Self0, Self, Sibl0, Sibl, Call0, Call, Redo0, Redo, Exit0, Exit) :-
    arg(1, H, '<recursive>'),
    !,
    ticks(T, Self0, Self, Sibl0, Sibl, Call0, Call, Redo0, Redo, Exit0, Exit).
ticks([H|T], Self0, Self, Sibl0, Sibl, Call0, Call, Redo0, Redo, Exit0, Exit) :-
    arg(3, H, ThisSelf),
    arg(4, H, ThisSibings),
    arg(5, H, ThisCall),
    arg(6, H, ThisRedo),
    arg(7, H, ThisExit),
    Self1 is ThisSelf + Self0,
    Sibl1 is ThisSibings + Sibl0,
    Call1 is ThisCall + Call0,
    Redo1 is ThisRedo + Redo0,
    Exit1 is ThisExit + Exit0,
    ticks(T, Self1, Self, Sibl1, Sibl, Call1, Call, Redo1, Redo, Exit1, Exit).


%       clusters(+Relatives, -Cycles)
%
%       Organise the relatives by cluster.

clusters(Relatives, Cycles) :-
    clusters(Relatives, 0, Cycles).

clusters([], _, []).
clusters(R, C, [H|T]) :-
    cluster(R, C, H, T0),
    C2 is C + 1,
    clusters(T0, C2, T).

cluster([], _, [], []).
cluster([H|T0], C, [H|TC], R) :-
    arg(2, H, C),
    !,
    cluster(T0, C, TC, R).
cluster([H|T0], C, TC, [H|T]) :-
    cluster(T0, C, TC, T).

%       sort_relatives(+Relatives, -Sorted)
%
%       Sort relatives in ascending number of calls.

sort_relatives(List, Sorted) :-
    key_with_calls(List, Keyed),
    keysort(Keyed, KeySorted),
    unkey(KeySorted, Sorted).

key_with_calls([], []).
key_with_calls([H|T0], [0-H|T]) :-      % get recursive on top
    arg(1, H, '<recursive>'),
    !,
    key_with_calls(T0, T).
key_with_calls([H|T0], [K-H|T]) :-
    arg(4, H, Calls),
    arg(5, H, Redos),
    K is Calls+Redos,
    key_with_calls(T0, T).

unkey([], []).
unkey([_-H|T0], [H|T]) :-
    unkey(T0, T).

%       show_relatives(+Relatives, +Rolw, +Window)
%
%       Show list of relatives as table-rows.

show_relatives([], _, _) :- !.
show_relatives([H|T], Role, W) :-
    send(W, show_relative, H, Role),
    show_relatives(T, Role, W).

show_predicate(W, Data:prolog,
               Ticks:int, ChildTicks:int,
               Call:int, Redo:int, Exit:int) :->
    "Show the predicate we have details on"::
    get(W, class_variable_value, header_colour, HC),
    get(W, class_variable_value, header_background, HBG),
    BG = (background := HBG),
    FG = (colour := HC),
    Pred = Data.predicate,
    prof_tool(W, Frame),
    get(Frame, render_time, Ticks, Self),
    get(Frame, render_time, ChildTicks, Children),
    get(W, tabular, T),
    Fail is Call+Redo-Exit,
    send(T, append, Self, halign := right, BG, FG),
    send(T, append, Children, halign := right, BG, FG),
    (   prof_tool(W, Tool), get(Tool, ports, false)
    ->  send(T, append, Call, halign := right, BG, FG)
    ;   send(T, append, Call, halign := right, BG, FG),
        send(T, append, Redo, halign := right, BG, FG),
        send(T, append, Exit, halign := right, BG, FG),
        send(T, append, Fail, halign := right, BG, FG)
    ),
    (   object(Pred)
    ->  new(Txt, prof_node_text(Pred, self))
    ;   new(Txt, prof_predicate_text(Pred, self))
    ),
    send(T, append, Txt, BG, FG),
    send(T, next_row).

show_relative(W, Caller:prolog, Role:name) :->
    Caller = node(Pred, _Cluster, Ticks, ChildTicks, Calls, Redos, Exits),
    get(W, tabular, T),
    prof_tool(W, Frame),
    (   Pred == '<recursive>'
    ->  send(T, append, new(graphical), colspan := 2),
        send(T, append, Calls, halign := right),
        (   prof_tool(W, Tool), get(Tool, ports, false)
        ->  true
        ;   send(T, append, new(graphical), colspan := 3)
        ),
        send(T, append, Pred, italic)
    ;   get(Frame, render_time, Ticks, Self),
        get(Frame, render_time, ChildTicks, Children),
        send(T, append, Self, halign := right),
        send(T, append, Children, halign := right),
        (   prof_tool(W, Tool), get(Tool, ports, false)
        ->  send(T, append, Calls, halign := right)
        ;   Fails is Calls+Redos-Exits,
            send(T, append, Calls, halign := right),
            send(T, append, Redos, halign := right),
            send(T, append, Exits, halign := right),
            send(T, append, Fails, halign := right)
        ),
        (   Pred == '<spontaneous>'
        ->  send(T, append, Pred, italic)
        ;   object(Pred)
        ->  send(T, append, prof_node_text(Pred, Role))
        ;   send(T, append, prof_predicate_text(Pred, Role))
        )
    ),
    send(T, next_row).


:- pce_end_class(prof_details).


                 /*******************************
                 *        RIGHT-HAND TABS       *
                 *******************************/

/* The details and the call graph are two views of the same node, so
they share the right of the profiler as tabs.  The graph is drawn by
graphviz, which takes a process, so it is only drawn when it can be
seen: a node that is selected while the details are on top only marks
it stale, and it is drawn when its tab comes up.
*/

:- pce_begin_class(prof_tabs, tabbed_window,
                   "Details and call graph of the current node").

initialise(W) :->
    send_super(W, initialise),
    send(W, append, new(prof_details), details),
    send(W, append, new(prof_graph), call_graph).

new_tab(_W, Window:window, Label:[name], Tab:tab) :<-
    "Create the tab that is to hold Window"::
    new(Tab, prof_tab(Window, Label)).

:- pce_end_class(prof_tabs).

:- pce_begin_class(prof_tab, window_tab,
                   "Tab of the details or call graph").

status(T, Status:{on_top,hidden}) :->
    "Draw the graph when its tab comes on top"::
    send_super(T, status, Status),
    (   Status == on_top,
        get(T, window, Window),
        send(Window, instance_of, prof_graph)
    ->  send(Window, update)
    ;   true
    ).

:- pce_end_class(prof_tab).


                 /*******************************
                 *          CALL GRAPH          *
                 *******************************/

/* A call graph around the current node, as kcachegrind shows it: the
node in the middle, its callers above and its callees below, each box
tinted by the time spent in it and its children, and each arrow as
thick as the time that flows along it.

A graph is only of use as long as it can be read, so it is pruned once
it grows, but no sooner.  The callers or callees of a predicate are all
shown as long as there are at most `prune_above' of them.  Of more, only
those that take at least half their fair share of the time spent in
them are, and at most `max_relatives': ten callees that take about the
same time all stay, while of a callee that takes most of the time and
twenty that take next to nothing, only the first does.  Beyond the
direct relatives of the node in the middle, the graph grows level by
level, the heaviest calls first, up to `max_nodes'.  The details list
all relatives.  The times on a call further out are
those of all its calls, not only of the ones made on behalf of the node
in the middle: the profile does not tell those apart.
*/

:- pce_begin_class(prof_graph, xdot_window,
                   "Call graph around the current node").

variable(node,  prolog,       get, "Currently shown node").
variable(stale, bool := @off, get, "The node changed since it was drawn").
variable(ids,   prolog,       get, "Id-Predicate pairs of the graph nodes").
variable(fitted, prolog,      none, "Transform left by ->fit, or `none'").

class_variable(caller_depth, '1..', 2,   "Levels of callers shown").
class_variable(callee_depth, '1..', 2,   "Levels of callees shown").
class_variable(natural_zoom, num, 1.5,
               "Zoom of a graph that fits at this scale").
class_variable(prune_above,  '1..', 6,
               "Callers or callees that are all shown").
class_variable(max_relatives, '1..', 12,
               "Most callers or callees shown").
class_variable(max_nodes,    '1..', 30,
               "Nodes beyond which no further levels are added").

initialise(W) :->
    send_super(W, initialise, @default, call_graph, size(300,200)),
    send(W, slot, fitted, none),
    get(W, xdot, X),
    send(X, node_clicked, message(W, clicked, @arg1)),
    new(P, popup),
    send_list(P, append,
              [ menu_item(details, message(W, clicked, @arg1)),
                menu_item(edit, message(W, edit, @arg1))
              ]),
    send(X, node_popup, P).

node(W, Data:prolog) :->
    "Show the call graph around a node"::
    send(W, slot, node, Data),
    send(W, refresh).

refresh(W) :->
    "Draw again, now if I can be seen, else when I can"::
    send(W, slot, stale, @on),
    send(W, update).

colours_changed(W) :->
    "The graph uses the colours of the window: draw it again"::
    send(W, refresh).

update(W) :->
    "Draw the graph if it changed and I can be seen"::
    (   get(W, stale, @on),
        get(W, node, Data),
        Data \== @nil,
        get(W, container, tab, Tab),
        get(Tab, status, on_top)
    ->  send(W, slot, stale, @off),
        send(W, render, Data)
    ;   true
    ).

render(W, Data:prolog) :->
    "Lay out and display the call graph around Data"::
    prof_tool(W, Tool),
    get(W, class_variable_value, caller_depth, CallerDepth),
    get(W, class_variable_value, callee_depth, CalleeDepth),
    get(W, class_variable_value, prune_above, PruneAbove),
    get(W, class_variable_value, max_relatives, MaxRelatives),
    get(W, class_variable_value, max_nodes, MaxNodes),
    get(Tool, ticks, Ticks),
    get(Tool, accounting_ticks, Accounting),
    Total is max(1, Ticks-Accounting),
    call_graph(Tool, Data,
               limits(CallerDepth, CalleeDepth,
                      PruneAbove, MaxRelatives, MaxNodes),
               Edges),
    graph_preds(Data.predicate, Edges, Preds),
    pairs_keys_values(Ids, IdList, Preds),
    numbered_ids(Preds, 0, IdList),
    send(W, slot, ids, Ids),
    graph_colours(W, Colours),
    phrase(dot_graph(Tool, Data.predicate, Total, Colours, Ids, Edges),
           Codes),
    string_codes(Dot, Codes),
    debug(profile(graph), '~s', [Dot]),
    get(W, xdot, X),
    send(X, load, Dot),                 % explains itself if dot fails
    send(W, star),
    send(W, fit).

%       A new size fits the graph again, unless the user moved or
%       zoomed it since it was fitted: the scrollbars that come and go as
%       they drag it past my edges change my size as well, and fitting
%       then undoes what they just did.

fit(W) :->
    "Fit the graph and remember the transform that left"::
    send_super(W, fit),
    get(W?xdot, transform, T),
    transform_state(T, State),
    send(W, slot, fitted, State).

resize(W) :->
    "Fit the graph to the new size if the user did not move it"::
    send_super(W, resize),
    (   get(W, stale, @off),
        get(W, slot, fitted, State),
        State \== none,
        get(W?xdot, transform, T),
        transform_state(T, State)
    ->  send(W, fit)
    ;   true
    ).

transform_state(@nil, identity) :- !.
transform_state(T, State) :-
    get(T, xx, XX), get(T, xy, XY),
    get(T, yx, YX), get(T, yy, YY),
    get(T, tx, TX), get(T, ty, TY),
    (   XX =:= 1, XY =:= 0, YX =:= 0, YY =:= 1, TX =:= 0, TY =:= 0
    ->  State = identity                % a click makes one; see pan_zoom
    ;   State = t(XX,XY,YX,YY,TX,TY)
    ).

%       The current node stands out by a star behind it, as in
%       kcachegrind.  Its spikes reach out from an ellipse through the
%       corners of the box.

star(W) :->
    "Put a star behind the current node"::
    get(W, xdot, X),
    (   get(X, member, n0, Node)
    ->  get(Node, area, area(NX, NY, NW, NH)),
        CX is NX + NW/2,
        CY is NY + NH/2,
        IRX is NW/2*sqrt(2) + 2,
        IRY is NH/2*sqrt(2) + 2,
        Spike = 12,
        Points = 14,
        new(Star, path),
        send(Star, closed, @on),
        Last is 2*Points-1,
        forall(between(0, Last, I),
               ( A is I*pi/Points - pi/2,
                 (   I mod 2 =:= 0
                 ->  RX = IRX+Spike, RY = IRY+Spike
                 ;   RX = IRX, RY = IRY
                 ),
                 PX is round(CX + RX*cos(A)),
                 PY is round(CY + RY*sin(A)),
                 send(Star, append, point(PX, PY))
               )),
        send(Star, pen, 0),
        send(Star, fill, colour(@default, 128, 128, 128, 80)),
        send(X, display, Star),
        send(Star, hide, Node)          % in front of the graph's background
    ;   true
    ).

pred(W, Node:xdot_node, Pred:prolog) :<-
    "Predicate shown by a node of the graph"::
    get(W, ids, Ids),
    get(Node, name, Id),
    memberchk(Id-Pred, Ids).

clicked(W, Node:xdot_node) :->
    "Make the clicked predicate the current one"::
    get(W, pred, Node, Pred),
    get(W, node, Data),
    (   Pred == Data.predicate
    ->  true
    ;   prof_tool(W, Tool),
        send(Tool, details, Pred)
    ).

edit(W, Node:xdot_node) :->
    "Edit the predicate of a node"::
    get(W, pred, Node, Pred),
    (   object(Pred)
    ->  send(Pred, edit)
    ;   new(PP, prolog_predicate(Pred)),
        send(PP, edit)
    ).

:- pce_end_class(prof_graph).

numbered_ids([], _, []).
numbered_ids([_|T0], N, [Id|T]) :-
    atom_concat(n, N, Id),
    N1 is N+1,
    numbered_ids(T0, N1, T).

%!  graph_colours(+Window, -Colours) is det.
%
%   The colours of the arrows and their labels follow those of the
%   window, so the graph reads in a dark theme as well.  The boxes are
%   tinted by themselves and keep black text.

graph_colours(W, colours(FG)) :-
    get(W, foreground, Colour0),
    (   send(Colour0, instance_of, colour)
    ->  Colour = Colour0
    ;   get(@display, foreground, Colour)
    ),
    colour_hex(Colour, FG).

colour_hex(Colour, Hex) :-
    get(Colour, red, R0),
    get(Colour, green, G0),
    get(Colour, blue, B0),
    maplist(byte, [R0,G0,B0], [R,G,B]),
    format(atom(Hex), '#~|~`0t~16r~2+~|~`0t~16r~2+~|~`0t~16r~2+', [R,G,B]).

byte(V, B) :-
    (   V > 255
    ->  B is V >> 8
    ;   B = V
    ).

%!  call_graph(+Tool, +Data, +Limits, -Edges) is det.
%
%   Edges is a list of edge(Caller, Callee, Ticks, Calls) around the node
%   Data.  Limits is a term
%
%       limits(CallerDepth, CalleeDepth, PruneAbove, MaxRelatives, MaxNodes)
%
%   The direct callers and callees of Data are there as far as
%   pred_edges/5 keeps them.  From those it walks CallerDepth-1 levels
%   up and CalleeDepth-1 levels down, while the graph has fewer than
%   MaxNodes nodes.  A call is there once, as found first.

call_graph(Tool, Data, Limits, Edges) :-
    Limits = limits(CallerDepth, CalleeDepth, _, _, _),
    Pred = Data.predicate,
    pred_edges(callers, Pred, Data, Limits, Callers),
    pred_edges(callees, Pred, Data, Limits, Callees),
    edge_ends(callers, Callers, Up),
    edge_ends(callees, Callees, Down),
    append([[Pred], Up, Down], Seen0),
    list_to_set(Seen0, Seen),
    append(Callers, Callees, Direct),
    MoreUp is CallerDepth-1,
    MoreDown is CalleeDepth-1,
    walk(MoreUp-Up, MoreDown-Down, Tool, Limits, Seen, Direct, Edges1),
    unique_edges(Edges1, Edges).

%!  walk(+Up, +Down, +Tool, +Limits, +Seen, +Edges0, -Edges) is det.
%
%   Add a level of callers above the predicates in Up and of callees
%   below those in Down, both Depth-Frontier pairs.  The calls of a
%   level are added heaviest first; a call to a predicate that is not
%   yet in the graph is only added while there is room for it.

walk(UpDepth-Ups, DownDepth-Downs, Tool, Limits, Seen, Edges0, Edges) :-
    candidates(callers, UpDepth, Ups, Tool, Limits, Above),
    candidates(callees, DownDepth, Downs, Tool, Limits, Below),
    append(Above, Below, Candidates),
    (   Candidates == []
    ->  Edges = Edges0
    ;   map_list_to_pairs(candidate_ticks, Candidates, Keyed),
        sort(1, @>=, Keyed, Sorted),
        pairs_values(Sorted, Ordered),
        Limits = limits(_, _, _, _, MaxNodes),
        admit(Ordered, MaxNodes, Seen, Seen1, New, NewUps, NewDowns),
        append(Edges0, New, Edges1),
        UpDepth1 is UpDepth-1,
        DownDepth1 is DownDepth-1,
        walk(UpDepth1-NewUps, DownDepth1-NewDowns, Tool, Limits, Seen1,
             Edges1, Edges)
    ).

candidates(_, Depth, _, _, _, []) :-
    Depth =< 0,
    !.
candidates(Dir, _, Frontier, Tool, Limits, Candidates) :-
    findall(Dir-E,
            ( member(P, Frontier),
              get(Tool, node_data, P, Data),
              pred_edges(Dir, P, Data, Limits, PEdges),
              member(E, PEdges)
            ),
            Candidates).

candidate_ticks(_-edge(_, _, Ticks, _), Ticks).

admit([], _, Seen, Seen, [], [], []).
admit([Dir-E|T], Max, Seen0, Seen, Edges, Ups, Downs) :-
    new_end(Dir, E, End),
    (   memberchk(End, Seen0)
    ->  Edges = [E|Edges1],
        admit(T, Max, Seen0, Seen, Edges1, Ups, Downs)
    ;   length(Seen0, Nodes),
        Nodes < Max
    ->  Edges = [E|Edges1],
        (   Dir == callers
        ->  Ups = [End|Ups1], Downs = Downs1
        ;   Downs = [End|Downs1], Ups = Ups1
        ),
        admit(T, Max, [End|Seen0], Seen, Edges1, Ups1, Downs1)
    ;   admit(T, Max, Seen0, Seen, Edges, Ups, Downs)
    ).

new_end(callers, edge(End, _, _, _), End).
new_end(callees, edge(_, End, _, _), End).

%!  pred_edges(+Dir, +Pred, +Data, +Limits, -Edges) is det.
%
%   The calls to (callers) or from (callees) Pred, as far as
%   prune_relatives/3 keeps them.  The relatives of a predicate are
%   split by the cycles it is part of; the calls between the same two
%   predicates are summed here.  Recursion shows as one call of Pred to
%   itself, taken from the callers only, as that is where the profile
%   lists it.

pred_edges(Dir, Pred, Data, Limits, Edges) :-
    findall(Other-t(Ticks,Calls),
            ( member(node(Other, _Cycle, Self, Children, Calls, _, _),
                     Data.Dir),
              Other \== '<recursive>',
              Ticks is Self+Children
            ),
            Pairs0),
    keysort(Pairs0, Pairs),
    group_pairs_by_key(Pairs, Grouped),
    findall(r(Ticks, Calls, Other),
            ( member(Other-Ts, Grouped),
              sum_ticks(Ts, Ticks, Calls)
            ),
            Relatives),
    prune_relatives(Relatives, Limits, Kept),
    findall(Edge,
            ( member(r(Ticks, Calls, Other), Kept),
              edge(Dir, Pred, Other, Ticks, Calls, Edge)
            ),
            Edges0),
    (   Dir == callers,
        member(node('<recursive>', _, _, _, RCalls, _, _), Data.callers)
    ->  Edges = [edge(Pred, Pred, 0, RCalls)|Edges0]
    ;   Edges = Edges0
    ).

%!  prune_relatives(+Relatives, +Limits, -Kept) is det.
%
%   Up to PruneAbove relatives are all kept.  Of more, keep those that
%   take at least half their fair share, Total/N, of the time, at most
%   MaxRelatives of them, heaviest first.  Relatives that take no time
%   go with that, unless none of them takes any.

prune_relatives(Relatives, limits(_, _, PruneAbove, MaxRelatives, _),
                Kept) :-
    length(Relatives, N),
    (   N =< PruneAbove
    ->  Kept = Relatives
    ;   foldl([r(T,_,_), S0, S]>>(S is S0+T), Relatives, 0, Total),
        include(fair_share(N, Total), Relatives, Fair),
        sort(0, @>=, Fair, Heaviest),
        max_prefix(Heaviest, MaxRelatives, Kept)
    ).

fair_share(N, Total, r(Ticks, _, _)) :-
    Ticks*2*N >= Total.

max_prefix(List, Max, Prefix) :-
    length(List, Len),
    Len > Max,
    !,
    length(Prefix, Max),
    append(Prefix, _, List).
max_prefix(List, _, List).

sum_ticks(Ts, Ticks, Calls) :-
    foldl([t(T,C), T0-C0, T1-C1]>>(T1 is T0+T, C1 is C0+C),
          Ts, 0-0, Ticks-Calls).

edge(callers, Pred, Other, Ticks, Calls, edge(Other, Pred, Ticks, Calls)).
edge(callees, Pred, Other, Ticks, Calls, edge(Pred, Other, Ticks, Calls)).

edge_ends(callers, Edges, Ends) :-
    findall(P, (member(edge(P,To,_,_), Edges), P \== To), Ends).
edge_ends(callees, Edges, Ends) :-
    findall(P, (member(edge(From,P,_,_), Edges), P \== From), Ends).

unique_edges(Edges, Unique) :-
    unique_edges(Edges, [], Unique).

unique_edges([], _, []).
unique_edges([E|T0], Seen, T) :-
    E = edge(From, To, _, _),
    memberchk(From-To, Seen),
    !,
    unique_edges(T0, Seen, T).
unique_edges([E|T0], Seen, [E|T]) :-
    E = edge(From, To, _, _),
    unique_edges(T0, [From-To|Seen], T).

%!  graph_preds(+Pred, +Edges, -Preds) is det.
%
%   The predicates in the graph, Pred first.

graph_preds(Pred, Edges, [Pred|Preds]) :-
    findall(P, ( member(edge(From,To,_,_), Edges),
                 ( P = From ; P = To ),
                 P \== Pred
               ),
            Preds0),
    list_to_set(Preds0, Preds).

%!  dot_graph(+Tool, +Pred, +Total, +Colours, +Ids, +Edges)//
%
%   The call graph in the dot language.

dot_graph(Tool, Pred, Total, colours(FG), Ids, Edges) -->
    "digraph calls {\n",
    "  graph [rankdir=TB nodesep=0.25 ranksep=0.45];\n",
    "  node [shape=box style=filled fontname=\"Helvetica\" \c
     fontsize=10 fontcolor=\"black\" color=\"#606060\"];\n",
    "  edge [fontname=\"Helvetica\" fontsize=9 arrowsize=0.7 ",
    "color=", dot_string(FG), " fontcolor=", dot_string(FG), "];\n",
    dot_nodes(Ids, Tool, Pred, Total),
    dot_edges(Edges, Tool, Total, Ids),
    "}\n".

dot_nodes([], _, _, _) --> [].
dot_nodes([Id-P|T], Tool, Pred, Total) -->
    { node_attrs(Tool, P, Pred, Total, Attrs) },
    "  ", atom(Id), " [", dot_attrs(Attrs), "];\n",
    dot_nodes(T, Tool, Pred, Total).

node_attrs(Tool, P, Pred, Total, Attrs) :-
    pred_label(P, Name),
    short_pred_label(P, Short),
    (   get(Tool, node_data, P, Data)
    ->  Self = Data.ticks_self,
        Incl is Self + Data.ticks_siblings,
        time_text(Tool, Incl, InclText),
        time_text(Tool, Self, SelfText),
        format(string(Label), '~w\n~w (self ~w)', [Short, InclText, SelfText]),
        format(string(Tooltip),
               '~w\nTime: ~w, self ~w\nCall: ~D, redo: ~D, exit: ~D',
               [ Name, InclText, SelfText,
                 Data.call, Data.redo, Data.exit
               ]),
        heat_colour(Incl, Total, Fill)
    ;   Label = Short,
        Tooltip = Name,
        Fill = '#f0f0f0'
    ),
    (   P == Pred
    ->  Extra = [penwidth=2.5, color='#202020',
                 fontname='Helvetica-Bold']
    ;   Extra = []
    ),
    Attrs = [label=Label, tooltip=Tooltip, fillcolor=Fill|Extra].

pred_label(P, Label) :-
    atom(P),
    sub_atom(P, 0, _, _, <),
    !,
    Label = P.
pred_label(P, Label) :-
    pce_predicate_label(P, Label0),
    (   atom(Label0)
    ->  Label = Label0
    ;   get(Label0, value, Label)
    ).

%   The boxes leave out the module, which the tooltip still shows: it
%   takes room and is rarely what tells the predicates apart.

short_pred_label(_:PI, Label) :-
    !,
    pred_label(PI, Label).
short_pred_label(P, Label) :-
    pred_label(P, Label).

time_text(Tool, Ticks, Text) :-
    get(Tool, render_time, Ticks, Rendered),
    (   atom(Rendered)
    ->  Text = Rendered
    ;   get(Rendered, value, Text)
    ).

%!  heat_colour(+Ticks, +Total, -Colour) is det.
%
%   From pale yellow for nothing to orange for all of the time.

heat_colour(Ticks, Total, Colour) :-
    F is min(1.0, Ticks/float(Total)),
    G is round(250 - F*110),
    B is round(215 - F*165),
    format(atom(Colour), '#ff~|~`0t~16r~2+~|~`0t~16r~2+', [G, B]).

dot_edges([], _, _, _) --> [].
dot_edges([edge(From,To,Ticks,Calls)|T], Tool, Total, Ids) -->
    { memberchk(FromId-From, Ids),
      memberchk(ToId-To, Ids),
      edge_attrs(Tool, Ticks, Calls, Total, Attrs)
    },
    "  ", atom(FromId), " -> ", atom(ToId), " [", dot_attrs(Attrs), "];\n",
    dot_edges(T, Tool, Total, Ids).

edge_attrs(Tool, Ticks, Calls, Total, [label=Label, penwidth=Width]) :-
    (   Ticks > 0
    ->  time_text(Tool, Ticks, TimeText),
        format(string(Label), '~w\n~D×', [TimeText, Calls])
    ;   format(string(Label), '~D×', [Calls])
    ),
    Width is round(10*(1 + 4*min(1.0, Ticks/float(Total))))/10.0.

dot_attrs([]) --> [].
dot_attrs([Name=Value|T]) -->
    atom(Name), "=", dot_value(Value),
    (   { T == [] }
    ->  []
    ;   " ",
        dot_attrs(T)
    ).

dot_value(Value) -->
    { number(Value) },
    !,
    number(Value).
dot_value(Value) -->
    dot_string(Value).

dot_string(Text) -->
    { atom_codes(Text, Codes) },
    "\"", dot_chars(Codes), "\"".

dot_chars([]) --> [].
dot_chars([H|T]) -->
    dot_char(H),
    dot_chars(T).

dot_char(0'")  --> !, "\\\"".
dot_char(0'\\) --> !, "\\\\".
dot_char(0'\n) --> !, "\\n".
dot_char(C)    --> [C].


:- pce_begin_class(prof_node_text, text,
                   "Show executable object").

variable(context,   any,                 get, "Represented executable").
variable(role,      {parent,self,child}, get, "Represented role").

class_variable(colour, colour, prof_node).

initialise(T, Context:any, Role:{parent,self,child}, Cycle:[int]) :->
    send(T, slot, context, Context),
    send(T, slot, role, Role),
    get(T, label, Label),
    (   (   Cycle == 0
        ;   Cycle == @default
        )
    ->  TheLabel = Label
    ;   N is Cycle+1,               % people like counting from 1
        TheLabel = string('%s <%d>', Label, N)
    ),
    send_super(T, initialise, TheLabel),
    get(T, class_variable_value, colour, FG),
    send(T, colour, FG),
    send(T, underline, @on),
    (   Role == self
    ->  send(T, font, bold)
    ;   true
    ).


label(T, Label:char_array) :<-
    get(T?context, print_name, Label).


:- free(@prof_node_text_recogniser).
:- pce_global(@prof_node_text_recogniser,
              make_prof_node_text_recogniser).

make_prof_node_text_recogniser(G) :-
    Text = @arg1,
    Pred = @arg1?context,
    new(P, popup),
    send_list(P, append,
              [ menu_item(details,
                          message(Text, details),
                          condition := Text?role \== self),
                menu_item(edit,
                          message(Pred, edit),
                          condition := Pred?source),
                menu_item(documentation,
                          message(Pred, help),
                          condition := message(Text, has_help))
              ]),
    new(C, click_gesture(left, '', single,
                         message(@receiver, details))),
    new(G, handler_group(C, popup_gesture(P))).


event(T, Ev:event) :->
    (   send_super(T, event, Ev)
    ->  true
    ;   send(@prof_node_text_recogniser, event, Ev)
    ).

has_help(T) :->
    get(T, context, Ctx),
    (   send(Ctx, instance_of, method) % hack
    ->  auto_call(manpce)
    ;   true
    ),
    send(Ctx, has_send_method, has_help),
    send(Ctx, has_help).

details(T) :->
    "Show details of clicked predicate"::
    get(T, context, Context),
    prof_tool(T, Tool),
    send(Tool, details, Context).

:- pce_end_class(prof_node_text).


:- pce_begin_class(prof_predicate_text, prof_node_text,
                   "Show a predicate").

initialise(T, Pred:prolog, Role:{parent,self,child}, Cycle:[int]) :->
    send_super(T, initialise, prolog_predicate(Pred), Role, Cycle).

details(T) :->
    "Show details of clicked predicate"::
    get(T?context, pi, @on, Head),
    prof_tool(T, Tool),
    send(Tool, details, Head).

:- pce_end_class(prof_predicate_text).


                 /*******************************
                 *              UTIL            *
                 *******************************/

value(name, Data, Name) :-
    !,
    predicate_sort_key(Data.predicate, Name).
value(label, Data, Label) :-
    !,
    pce_predicate_label(Data.predicate, Label).
value(ticks, Data, Ticks) :-
    !,
    Ticks is Data.ticks_self + Data.ticks_siblings.
value(Name, Data, Value) :-
    Value = Data.Name.

sort_by(cumulative_profile_by_time,          ticks,          reverse).
sort_by(flat_profile_by_time_self,           ticks_self,     reverse).
sort_by(cumulative_profile_by_time_children, ticks_siblings, reverse).
sort_by(flat_profile_by_number_of_calls,     call,           reverse).
sort_by(flat_profile_by_number_of_redos,     redo,           reverse).
sort_by(flat_profile_by_name,                name,           normal).


%!  pce_predicate_label(+PI, -Label)
%
%   Label is the human-readable identification   for Head. Calls the
%   hook user:prolog_predicate_name/2.

pce_predicate_label(Obj, Label) :-
    object(Obj),
    !,
    get(Obj, print_name, Label).
pce_predicate_label(PI, Label) :-
    predicate_label(PI, Label).
