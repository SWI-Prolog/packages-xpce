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
:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(assoc),
              [list_to_assoc/2, get_assoc/3, put_assoc/4, empty_assoc/1]).
:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(pane_frame)).
:- use_module(library(toolbar)).
:- use_module(library(tabular)).
:- use_module(library(prolog_predicate)).
:- use_module(library(tabbed_window), []).
:- use_module(library(xdot), []).
:- use_module(library(pce_filter_item), []).
:- use_module(library(plot/hierarchy_chart),
              [hierarchy_from_term/2, format_value/3]).
:- autoload(library(edit), [edit/1]).
:- autoload(library(option), [option/2, option/3]).
:- autoload(library(pce_html_manual), [man_documented/1, man_html_object/1]).

:- require([ reset_profiler/0,
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

class_variable(pane_icon, name*, 'ide/tools/profiler.svg',
               "Image shown on my tab").

variable(samples,          int,  get, "Total # samples").
variable(ticks,            int,  get, "Total # ticks").
variable(accounting_ticks, int,  get, "# ticks while accounting").
variable(time,             num,  get, "Total time").
variable(nodes,            int,  get, "Nodes created").
variable(ports,            {true,false,classic},  get, "Port mode").
variable(clock,            {cpu,wall} := cpu, get, "Time profiled").
variable(exact,            bool := @off, get, "Exact (not sampled) timing").
variable(time_view,        {percentage,seconds} := percentage,
                                 get, "How time is displayed").
variable(measure,          {time,memory} := time,
                                 get, "What the profile shows").
variable(allocated,        int := 0, get, "Global stack allocated").
variable(discarded,        int := 0, get, "Global stack discarded").
variable(gc_reclaimed,     int := 0, get, "Global stack reclaimed by GC").
variable(gc_count,         int := 0, get, "Garbage collections").
variable(components,       prolog, none,
         "Cached strongly connected components of the call graph").

%!  .auto_reset
%
%   If @on (default), the profiler forgets what it collected as soon as
%   it is shown here, so the next profile starts afresh.  If @off, a
%   next profile adds to it, which accumulates several runs into one
%   report.

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
    forall(sort_kind(Kind, Name, _),
           ( findall(menu_item(Key, message(F, sort_by, Key), Label),
                     sort_key(Kind, Key, Label, _),
                     Items),
             choice_popup(Popup, Name, Items, @default,
                          message(F, update_sort_menu, @receiver), Sort),
             send(Sort, multiple_selection, @on)
           )),
    findall(menu_item(M, @default, MLabel), measure(M, MLabel), MItems),
    choice_popup(Popup, measure, MItems,
                 message(F, measure, @arg1),
                 message(@receiver, selection, F?measure), _),
    findall(menu_item(TV, @default, TLabel), time_view(TV, TLabel), TItems),
    choice_popup(Popup, show_values_as, TItems,
                 message(F, time_view, @arg1),
                 message(@receiver, selection, F?time_view), _),
    send(Popup, append, menu_item(help, message(F, help))).

%   choice_popup(+Popup, +Name, +Items, +Message, +Update, -Sub)
%
%   Append a popup Sub named Name to Popup holding Items, of which one
%   is the current setting and is ticked.  Update makes it so when the
%   menu opens.  The current sort key is in one of the two sort popups,
%   so these allow for none to be ticked using `multiple_selection`: a
%   single selection menu always ticks an item.

choice_popup(Popup, Name, Items, Message, Update, Sub) :-
    send(Popup, append, new(Sub, popup(Name, Message))),
    send_list(Sub, append, Items),
    send(Sub, show_current, @on),
    send(Sub, update_message, Update).

update_sort_menu(F, Popup:popup) :->
    "Tick the current sort key if it is in Popup"::
    get(F, window, prof_browser, B),
    get(B, sort_by, Key),
    (   get(Popup, member, Key, Item)
    ->  send(Popup, selection, chain(Item))
    ;   send(Popup, selection, new(chain))
    ).


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
    send(F, slot, clock, Summary.get(clock, cpu)),
    send(F, slot, exact, Summary.get(exact, false)),
    send(F, slot, allocated, Summary.get(allocated, 0)),
    send(F, slot, discarded, Summary.get(discarded, 0)),
    send(F, slot, gc_reclaimed, Summary.get(gc_reclaimed, 0)),
    send(F, slot, gc_count, Summary.get(gc_count, 0)),
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
    get(F, statistics_summary, Text),
    send(F, report, inform, '%s', Text).

statistics_summary(F, Text:string) :<-
    "Text describing the overall statistics of the profile"::
    get(F, slot, nodes, Nodes),
    get(F, window, prof_browser, B),
    get(B?all_items, size, Predicates),
    get(F, measure, Measure),
    measure_summary(Measure, F, Predicates, Nodes, Text).

%   measure_summary(+Measure, +Frame, +Predicates, +Nodes, -Text)
%
%   The status line describes the measure shown.  For memory, the
%   number of predicates and nodes are left out: they are about the
%   profile rather than the memory.

measure_summary(memory, F, _Predicates, _Nodes, Text) :-
    !,
    get(F, allocated, Allocated),
    get(F, discarded, Discarded),
    get(F, gc_reclaimed, Reclaimed),
    get(F, gc_count, Count),
    maplist(format_value(bytes), [Allocated, Discarded, Reclaimed],
            [AS, DS, RS]),
    format_value(count, Count, CS),
    new(Text, string('Global stack: allocated %s, discarded %s, \c
                      reclaimed by GC %s in %s collections',
                     AS, DS, RS, CS)).
measure_summary(time, F, Predicates, Nodes, Text) :-
    get(F, samples, Samples),
    get(F, ticks, Ticks),
    get(F, accounting_ticks, Account),
    get(F, time, Time),
    get(F, clock, Clock),
    clock_label(Clock, Label),
    (   get(F, exact, @on)
    ->  Net is (Ticks-Account)/1e9,
        Overhead is Account/1e9,
        new(Text, string('Exact %s time: %.3f sec in predicates; \c
                          %d predicates; %d nodes in call-graph; \c
                          profiler overhead %.3f sec (excluded)',
                         Label, Net, Predicates, Nodes, Overhead))
    ;   (   Ticks == 0
        ->  Distortion = 0.0
        ;   Distortion is 100.0*(Account/Ticks)
        ),
        new(Text, string('%d samples of %s time in %.2f sec; \c
                          %d predicates; %d nodes in call-graph; \c
                          distortion %.0f%%',
                         Samples, Label, Time, Predicates, Nodes, Distortion))
    ).

clock_label(cpu,  'CPU').
clock_label(wall, wall).


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
    send(G, node, Node),
    get(F, window, prof_tree, T),
    send(T, node, Node).

components(F, Components:prolog) :<-
    "Assoc mapping a predicate to its component in the call graph"::
    (   get(F, slot, components, Components),
        Components \== @nil,
        Components \== none
    ->  true
    ;   get(F, window, prof_browser, B),
        chain_list(B?all_items, Items),
        maplist(item_data, Items, Nodes),
        call_graph_components(Nodes, Components),
        send(F, slot, components, Components)
    ).

item_data(DI, Data) :-
    get(DI, data, Data).

node_data(F, Pred:prolog, Node:prolog) :<-
    "The profile data of a predicate; fails if it was not sampled"::
    get(F, window, prof_browser, B),
    get(B?all_items, find,
        message(@arg1, has_predicate, prolog(Pred)),
        DI),
    get(DI, data, Node).

sort_by(F, SortBy:name, Order:[{normal,reverse}]) :->
    "Define the key for sorting the profile"::
    get(F, window, prof_browser, B),
    send(B, sort_by, SortBy, Order).

time_view(F, TV:name) :->
    "Show values as a percentage of the total or absolute"::
    send(F, slot, time_view, TV),
    send(F, refresh_views).

%!  measure(+Measure)
%
%   Show the time (`time`) or the global stack allocated (`memory`).  The
%   flat profile is sorted on the same key of the new measure.

measure(F, Measure:{time,memory}) :->
    "Show the time or the memory of the profile"::
    send(F, slot, measure, Measure),
    get(F, window, prof_browser, B),
    get(B, sort_by, Key0),
    (   measure_key(Key0, Measure, Key)
    ->  send(B, sort_by, Key)
    ;   true
    ),
    send(F, refresh_views),
    (   get(F, pane_frame, _)           % else ->report pops up a window
    ->  send(F, show_statistics)
    ;   true
    ).

refresh_views(F) :->
    "Show all views again after the time view or measure changed"::
    get(F, window, prof_browser, B),
    get(F, window, prof_details, W),
    get(F, window, prof_graph, G),
    get(F, window, prof_tree, T),
    send(B, update_labels),
    send(W, refresh),
    send(G, refresh),
    send(T, refresh).

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
        ->  Time = 0
        ;   get(F, time, TotalTime),
            Time is TotalTime*(Ticks/float(Total))
        ),
        format_value(seconds, Time, Rendered)
    ).

render_cost(F, Value:int, Rendered:any) :<-
    "Render a value of the current <-measure"::
    get(F, measure, Measure),
    get(F, render_measure, Measure, Value, Rendered).

render_measure(F, Measure:{time,memory}, Value:int, Rendered:any) :<-
    "Render a time (ticks) or memory (bytes) value"::
    (   Measure == time
    ->  get(F, render_time, Value, Rendered)
    ;   get(F, time_view, percentage)
    ->  get(F, allocated, Total),
        (   Total =:= 0
        ->  Rendered = '0.0%'
        ;   Percentage is 100.0 * (Value/Total),
            new(Rendered, string('%.1f%%', Percentage))
        )
    ;   format_value(bytes, Value, Rendered)
    ).

total_cost(F, Total:int) :<-
    "Total of the current <-measure, at least 1"::
    (   get(F, measure, time)
    ->  get(F, ticks, Ticks),
        get(F, accounting_ticks, Accounting),
        Total is max(1, Ticks-Accounting)
    ;   get(F, allocated, Allocated),
        Total is max(1, Allocated)
    ).

help(_) :->
    "Open help (web site)"::
    www_open_url('https://github.com/SWI-Prolog/packages-xpce/wiki/profiler').

:- pce_end_class(prof_frame).


                 /*******************************
                 *          PREFERENCES         *
                 *******************************/

:- multifile
    pce_preferences:preferences/2,
    pce_preferences:edit_type/3,
    pce_preferences:class_variable_changed/3.

%   The preferences a user may want to change for the profiler.  See
%   library(pce_preferences) and the class variable editor.

pce_preferences:preferences(prof_frame,
    [ prof_frame   - [ auto_reset ],
      prof_browser - [ max_width ],
      prof_graph   - [ caller_depth, callee_depth, prune_above,
                       max_relatives, max_nodes, max_edge_width,
                       natural_zoom
                     ],
      prof_tree    - [ view, min_share, max_depth, group_small ],
      prof_details - [ header_colour, header_background ]
    ]).

pce_preferences:edit_type(prof_graph, caller_depth,  '1..5').
pce_preferences:edit_type(prof_graph, callee_depth,  '1..5').
pce_preferences:edit_type(prof_graph, prune_above,   '1..20').
pce_preferences:edit_type(prof_graph, max_relatives, '1..30').
pce_preferences:edit_type(prof_graph, max_nodes,     '10..100').
pce_preferences:edit_type(prof_graph, max_edge_width, '1.0..5.0').
pce_preferences:edit_type(prof_graph, natural_zoom,  '0.5..3.0').
pce_preferences:edit_type(prof_tree,  min_share,     '0.0..0.1').
pce_preferences:edit_type(prof_tree,  max_depth,     '5..100').

%   The call graphs are drawn again when one of their limits changes.

pce_preferences:class_variable_changed(prof_graph, _, _) :-
    send(@prof_graphs, for_all, message(@arg1, refresh)).


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
%!  .max_width
%
%   The list of predicates at the left is made as wide as the longest
%   predicate name, but no wider than this percentage of the window, so
%   a single long name does not push the other panes aside.

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
    time_width(B?font, ColW),
    send(B, tab_stops, vector(W-ColW-15)).

%   The width of the widest value, as a percentage, in seconds or in
%   bytes.

time_width(Font, Width) :-
    aggregate_all(max(W),
                  ( member(Text, ['100.0%', '999.9 µs', '999 bytes']),
                    get(Font, width, Text, W)
                  ),
                  Width).

%       The column holding me and the filter above me is made as wide as
%       the longest predicate, a space and the widest time.
%       Not wider than max_width of the profiler, though: one long name
%       would push the details off the window.

fit_width(B) :->
    "Make my column as wide as my longest predicate"::
    get(B, font, Font),
    new(Max, number(0)),
    send(B?all_items, for_all,
         message(Max, maximum, ?(Font, width, @arg1?key))),
    get(Max, value, KeyW),
    get(Font, width, ' ', SpaceW),
    time_width(Font, TimeW),
    ColW is SpaceW + TimeW,
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
    send(Frame, slot, components, none),
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
    get(F, total_cost, Total),
    Limit is 0.9*Total,
    (   member(DI, Items),
        get(DI, value, SortBy, Ticks),
        Ticks < Limit
    ->  true
    ;   Items = [DI|_]
    ).
interesting_item(_, [DI|_], _, DI).

cumulative_key(Key) :-
    sort_key(cumulative, Key, _, _).

update_label(B) :->
    get(B, sort_by, Sort),
    sort_label(Sort, Label),
    send(B, label, Label).

sort_by(B, SortBy:name, Order:[{normal,reverse}]) :->
    "Define key on which to sort"::
    send(B, slot, sort_by, SortBy),
    (   measure_key(SortBy, Measure, SortBy),
        prof_tool(B, F),
        \+ get(F, measure, Measure)
    ->  send(F, measure, Measure)       % sorts again
    ;   true
    ),
    send(B, update_label),
    send(B, sort, Order),
    send(B, update_labels).

sort(B, Order:[{normal,reverse}]) :->
    get(B, sort_by, Sort),
    (   Order == @default
    ->  sort_key(_, Sort, _, TheOrder)
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
same_pred_(Head, PI) :-                 % not an xpce method
    callable(Head),
    same_head(Head, PI).

same_head(Head, Name/Arity) :-
    pi_head(Name/Arity, Head).
same_head(Head, user:Name/Arity) :-
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
    ->  get(F, measure, Measure),
        measure_key(ticks_self, Measure, Self),
        send(DI, update_label, Self, F)
    ;   get(DI, value, SortBy, Value),
        (   measure_key(SortBy, Measure, SortBy)
        ->  get(F, render_measure, Measure, Value, Rendered)
        ;   Rendered = Value
        ),
        send(DI, label, string('%s\t%s', Key, Rendered))
    ).

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
%!  .header_colour
%
%   Colour of the text of the row in the details that shows the current
%   predicate, between its callers above and its callees below.

class_variable(header_colour,     colour, black,  "Predicate header colour").

%!  .header_background
%
%   Background colour of the row in the details that shows the current
%   predicate.  The default follows the theme.

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
    prof_tool(W, Tool),
    get(Tool, measure, Measure),
    BG = (background := HBG),
    FG = (colour := HC),
    measure_title(Measure, Title),
    send(T, append, Title, bold, center, colspan := 2, BG, FG),
    (   Measure == memory
    ->  send(T, append, 'Discarded', bold, center,
             valign := center, BG, FG, rowspan := 2)
    ;   true
    ),
    (   get(Tool, ports, false)
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
    (   get(Tool, ports, false)
    ->  true
    ;   send(T, append, 'Call',   bold, center, BG, FG),
        send(T, append, 'Redo',   bold, center, BG, FG),
        send(T, append, 'Exit',   bold, center, BG, FG),
        send(T, append, 'Fail',   bold, center, BG, FG)
    ),
    send(T, next_row).

measure_title(time,   'Time').
measure_title(memory, 'Allocated').

cluster_title(W, Cycle:int) :->
    get(W, tabular, T),
    get(W, columns, Columns),
    send(T, append, string('Cluster <%d>', Cycle),
         bold, center, colspan := Columns,
         background := navyblue, colour := yellow),
    send(T, next_row).

columns(W, Columns:int) :<-
    "Number of columns of the table"::
    prof_tool(W, Tool),
    (   get(Tool, ports, false)
    ->  Ports = 1
    ;   Ports = 4
    ),
    (   get(Tool, measure, memory)
    ->  Measure = 3
    ;   Measure = 2
    ),
    Columns is Measure+Ports+1.

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
    prof_tool(W, Tool),
    get(Tool, measure, Measure),
    sort_relatives(Callers, Callers1),
    show_relatives(Callers1, parent, W),
    sum_relatives(Measure, Callers1, Sum),
    send(W, show_predicate, Data, Sum),
    sort_relatives(Callees, Callees1),
    reverse(Callees1, Callees2),
    show_relatives(Callees2, child, W).

%   sum_relatives(+Measure, +Callers, -Sum)
%
%   Sum is a term sum(Self, Children, Discarded, Call, Redo, Exit) that
%   sums the cost of Measure and the ports over the Callers, which
%   together are all calls of the predicate in a cluster.

sum_relatives(Measure, Callers, Sum) :-
    foldl(add_relative(Measure), Callers, sum(0,0,0,0,0,0), Sum).

add_relative(_, Rel, Sum, Sum) :-
    arg(1, Rel, '<recursive>'),
    !.
add_relative(Measure, Rel,
             sum(S0, C0, D0, Call0, Redo0, Exit0),
             sum(S,  C,  D,  Call,  Redo,  Exit)) :-
    rel_cost(Measure, Rel, ThisS, ThisC),
    rel_discarded(Rel, ThisD),
    arg(5, Rel, ThisCall),
    arg(6, Rel, ThisRedo),
    arg(7, Rel, ThisExit),
    S is S0+ThisS,
    C is C0+ThisC,
    D is D0+ThisD,
    Call is Call0+ThisCall,
    Redo is Redo0+ThisRedo,
    Exit is Exit0+ThisExit.


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
    arg(5, H, Calls),
    arg(6, H, Redos),
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

show_predicate(W, Data:prolog, Sum:prolog) :->
    "Show the predicate we have details on"::
    get(W, class_variable_value, header_colour, HC),
    get(W, class_variable_value, header_background, HBG),
    BG = (background := HBG),
    FG = (colour := HC),
    Pred = Data.predicate,
    get(W, tabular, T),
    send(W, show_numbers, Sum, [BG, FG]),
    (   object(Pred)
    ->  new(Txt, prof_node_text(Pred, self))
    ;   new(Txt, prof_predicate_text(Pred, self))
    ),
    send(T, append, Txt, BG, FG),
    send(T, next_row).

%   ->show_numbers appends the cells before the predicate: the cost of
%   the measure and the ports.  Attrs are additional attributes of the
%   cells.

show_numbers(W, Sum:prolog, Attrs:prolog) :->
    "Append the cells for the numbers of a row"::
    Sum = sum(Self, Children, Discarded, Call, Redo, Exit),
    get(W, tabular, T),
    prof_tool(W, Tool),
    get(Tool, measure, Measure),
    get(Tool, render_cost, Self, SelfText),
    get(Tool, render_cost, Children, ChildrenText),
    Cells0 = [SelfText, ChildrenText],
    (   Measure == memory
    ->  format_value(bytes, Discarded, DiscardedText),
        append(Cells0, [DiscardedText], Cells1)
    ;   Cells1 = Cells0
    ),
    (   get(Tool, ports, false)
    ->  append(Cells1, [Call], Cells)
    ;   Fail is Call+Redo-Exit,
        append(Cells1, [Call, Redo, Exit, Fail], Cells)
    ),
    forall(member(Cell, Cells),
           ( Msg =.. [append, Cell, halign := right|Attrs],
             send(T, Msg)
           )).

show_relative(W, Rel:prolog, Role:name) :->
    "Show a caller or callee as a row"::
    arg(1, Rel, Pred),
    get(W, tabular, T),
    (   Pred == '<recursive>'
    ->  arg(5, Rel, Calls),
        prof_tool(W, Tool),
        (   get(Tool, measure, memory)
        ->  Empty = 3
        ;   Empty = 2
        ),
        send(T, append, new(graphical), colspan := Empty),
        send(T, append, Calls, halign := right),
        (   get(Tool, ports, false)
        ->  true
        ;   send(T, append, new(graphical), colspan := 3)
        ),
        send(T, append, Pred, italic)
    ;   prof_tool(W, Tool),
        get(Tool, measure, Measure),
        sum_relatives(Measure, [Rel], Sum),
        send(W, show_numbers, Sum, []),
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
    send(W, append, new(prof_graph), call_graph),
    send(W, append, new(prof_tree), call_tree).

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
        (   send(Window, instance_of, prof_graph)
        ;   send(Window, instance_of, prof_tree)
        )
    ->  send(Window, update)
    ;   true
    ).

:- pce_end_class(prof_tab).


                 /*******************************
                 *          CALL GRAPH          *
                 *******************************/

/* A call graph around the current node, as kcachegrind shows it: the
node in the middle, its callers above and its callees below, each box
tinted by the time spent in it and its children, and each arrow
labelled with the number of calls and as thick as the time that flows
along it, up to `max_edge_width'.  Set that to 1 for all edges to be
thin.

A graph is only of use as long as it can be read, so it is pruned once
it grows, but no sooner.  The callers or callees of a predicate are all
shown as long as there are at most `prune_above' of them.  Of more, only
those that take at least half their fair share of the time spent in
them are, and at most `max_relatives': ten callees that take about the
same time all stay, while of a callee that takes most of the time and
twenty that take next to nothing, only the first does.  Beyond the
direct relatives of the node in the middle, the graph grows level by
level, the heaviest calls first, up to `max_nodes'.  The callers or
callees that are left out are shown as a dashed "… N more" node.
Clicking it adds them to the graph, connected to the predicates already
there, until another predicate becomes the current one.  The details
list all relatives.  The times on a call further out are
those of all its calls, not only of the ones made on behalf of the node
in the middle: the profile does not tell those apart.
*/

:- pce_begin_class(prof_graph, xdot_window,
                   "Call graph around the current node").

variable(node,  prolog,       get, "Currently shown node").
variable(stale, bool := @off, get, "The node changed since it was drawn").
variable(ids,   prolog,       get, "Id-Predicate pairs of the graph nodes").
variable(expanded, prolog,    get, "Dir-Pred of the \"… N more\" clicked").

%!  .caller_depth
%
%   How many levels of callers the call graph shows above the current
%   predicate: 1 shows its direct callers, 2 also their callers, etc.
%   Levels beyond the first are only added while the graph has fewer
%   than `max_nodes` nodes.

class_variable(caller_depth, '1..', 2,   "Levels of callers shown").

%!  .callee_depth
%
%   How many levels of callees the call graph shows below the current
%   predicate: 1 shows the predicates it calls, 2 also the predicates
%   those call, etc.  Levels beyond the first are only added while the
%   graph has fewer than `max_nodes` nodes.

class_variable(callee_depth, '1..', 2,   "Levels of callees shown").

%!  .natural_zoom
%
%   Scale at which a call graph that fits the window is shown.  A graph
%   that does not fit at this scale is scaled down until it does.

class_variable(natural_zoom, num, 1.5,
               "Zoom of a graph that fits at this scale").

%!  .prune_above
%
%   A predicate with at most this many callers (or callees) has all of
%   them shown.  Of a predicate with more, only those that take a fair
%   share of the time are shown, at most `max_relatives`.  The others
%   are combined into a dashed "… N more" node; clicking it adds them
%   to the graph.  The details always list all of them.

class_variable(prune_above,  '1..', 6,
               "Callers or callees that are all shown").

%!  .max_relatives
%
%   The largest number of callers (or callees) of a predicate the call
%   graph shows if it has more than `prune_above`, the heaviest first.
%   Those left out are combined into a dashed "… N more" node; clicking
%   it adds them to the graph.

class_variable(max_relatives, '1..', 12,
               "Most callers or callees shown").

%!  .max_nodes
%
%   The call graph stops adding levels of callers and callees once it
%   has this many nodes, so it stays readable.  The direct callers and
%   callees of the current predicate are always shown.

class_variable(max_nodes,    '1..', 30,
               "Nodes beyond which no further levels are added").

%!  .max_edge_width
%
%   Line width of an arrow in the call graph along which all of the
%   time is spent.  Arrows are drawn thinner in proportion to the time
%   spent in the calls they stand for.  Use 1 to draw all arrows thin.

class_variable(max_edge_width, num, 5,
               "Width of an edge that takes all of the time").

%!  .renderer
%
%   How the call graph is drawn.  `xdot` draws what graphviz draws,
%   including the text.  `graphicals` makes the nodes and edge labels
%   as XPCE graphicals and uses graphviz only to place them and route
%   the edges.

class_variable(renderer, {xdot,graphicals}, graphicals,
               "Draw the graph as graphviz does or with own graphicals").

initialise(W) :->
    send_super(W, initialise, @default, call_graph, size(300,200)),
    send(W, slot, expanded, []),
    send(@prof_graphs, append, W),
    get(W, xdot, X),
    send(X, node_clicked, message(W, clicked, @arg1)),
    new(P, popup),
    send_list(P, append,
              [ menu_item(edit, message(W, edit, @arg1)),
                menu_item(info, message(W, info, @arg1),
                          condition := message(W, has_info, @arg1))
              ]),
    send(X, node_popup, P).

unlink(W) :->
    send(@prof_graphs, delete_all, W),
    send_super(W, unlink).

%   The "… N more" nodes that were clicked stay open while the graph
%   shows the same predicate.

node(W, Data:prolog) :->
    "Show the call graph around a node"::
    (   get(W, node, Old),
        is_dict(Old),
        Old.predicate == Data.predicate
    ->  true
    ;   send(W, slot, expanded, [])
    ),
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
    get(W, class_variable_value, max_edge_width, MaxWidth),
    get(Tool, total_cost, Total),
    call_graph(Tool, Data,
               limits(CallerDepth, CalleeDepth,
                      PruneAbove, MaxRelatives, MaxNodes),
               Edges0),
    get(W, expanded, Expanded),
    expand_hidden(Tool, Data, Expanded, Edges0, Edges1),
    hidden_edges(Tool, Data, Edges1, Hidden),
    append(Edges1, Hidden, Edges),
    graph_preds(Data.predicate, Edges, Preds),
    pairs_keys_values(Ids, IdList, Preds),
    numbered_ids(Preds, 0, IdList),
    send(W, slot, ids, Ids),
    graph_colours(W, Colours),
    get(W, xdot, X),
    (   send(X, instance_of, graphviz_figure)
    ->  graph_graphicals(X, Tool, Data.predicate, Total, Colours, MaxWidth,
                         Ids, Edges)
    ;   phrase(dot_graph(Tool, Data.predicate, Total, Colours, MaxWidth,
                         Ids, Edges),
               Codes),
        string_codes(Dot, Codes),
        debug(profile(graph), '~s', [Dot]),
        send(X, load, Dot)              % explains itself if dot fails
    ),
    send(W, star),
    send(W, fit).

%       The current node stands out by a star behind it, as in
%       kcachegrind.

star(W) :->
    "Put a star behind the current node"::
    get(W, xdot, X),
    ignore(send(X, star, n0)).

%   A node that stands for the hidden relatives of a predicate acts as
%   that predicate, except that clicking it adds them to the graph.

pred(W, Node:xdot_node, Pred:prolog) :<-
    "Predicate shown by a node of the graph"::
    get(W, ids, Ids),
    get(Node, name, Id),
    memberchk(Id-P, Ids),
    (   P = more(_, Pred, _)
    ->  true
    ;   Pred = P
    ).

clicked(W, Node:xdot_node) :->
    "Make the clicked predicate the current one or show hidden ones"::
    get(W, ids, Ids),
    get(Node, name, Id),
    memberchk(Id-P, Ids),
    get(W, node, Data),
    (   P = more(Dir, Pred, _)
    ->  send(W, expand, Dir, Pred)
    ;   P == Data.predicate
    ->  true
    ;   prof_tool(W, Tool),
        send(Tool, details, P)
    ).

expand(W, Dir:{callers,callees}, Pred:prolog) :->
    "Add all callers or callees of Pred to the graph"::
    get(W, expanded, Expanded),
    send(W, slot, expanded, [Dir-Pred|Expanded]),
    send(W, refresh).

edit(W, Node:xdot_node) :->
    "Edit the predicate of a node"::
    get(W, pred, Node, Pred),
    edit_predicate(Pred).

has_info(W, Node:xdot_node) :->
    "True if the predicate of a node has an info sheet"::
    get(W, pred, Node, Pred),
    predicate_has_info(Pred).

info(W, Node:xdot_node) :->
    "Show the info sheet of the predicate of a node"::
    get(W, pred, Node, Pred),
    predicate_info(Pred).

:- pce_end_class(prof_graph).

edit_predicate(Pred) :-
    (   object(Pred)
    ->  edit(Pred)
    ;   new(PP, prolog_predicate(Pred)),
        send(PP, edit)
    ).

%!  predicate_has_info(+Pred) is semidet.
%!  predicate_info(+Pred) is det.
%
%   The info sheet of a predicate of the profile, as the Info item of
%   PceEmacs shows it (`prolog_predicate ->info`).  Pred is what the
%   profile holds: a predicate indicator, or an xpce method, which has
%   no sheet.

predicate_has_info(Pred) :-
    info_predicate(Pred, PP),
    get(PP, head, Head),
    predicate_property(Head, _),
    !.

predicate_info(Pred) :-
    info_predicate(Pred, PP),
    send(PP, info).

info_predicate(Pred, PP) :-
    object(Pred),
    !,
    send(Pred, instance_of, prolog_predicate),
    PP = Pred.
info_predicate(Pred, PP) :-
    Pred \= '<recursive>',
    Pred \= more(_),
    new(PP, prolog_predicate(Pred)).


                 /*******************************
                 *           CALL TREE          *
                 *******************************/

/* Where the time below the current node goes, as rings or a treemap
(see library(plot/hierarchy_chart)).  The profile is a call graph, not
a tree, so the tree is made by expanding the callees of the current
node.  A callee gets the time spent in it on behalf of its caller, and
its own callees share the time spent below it on behalf of that caller
in proportion to all the time spent below it: the profile does not tell
the calls made on behalf of one caller apart.  A callee is not expanded
if it takes less than `min_share' of the current node, or below
`max_depth'.  If `group_small' is @on, the callees of a node that are
not expanded because they take less than `min_share' are combined into
a single "… N more" node, so their time stays visible in the chart.

The predicates of a recursion (a strongly connected component of the
call graph) are shown once each, as a tree of the calls by which they
are first reached from the predicate through which the recursion is
entered.  Each gets the time it spent itself and in the calls it made
out of the recursion.  The time of a call into the recursion includes
that of the recursive calls inside it, so following those would count
it again.

As the call graph, the tree is only built when its tab can be seen.
*/

:- pce_begin_class(prof_tree, hierarchy_window,
                   "Call tree below the current node").

variable(node,  prolog,       get, "Currently shown node").
variable(stale, bool := @off, get, "The node changed since it was drawn").

%!  .min_share
%
%   A callee in the call tree that takes less than this fraction of the
%   time of the current predicate is not opened up: it shows as a
%   single segment, with its own callees included.  The default 0.002
%   is 0.2%.  See also `group_small`.

class_variable(min_share, num,   0.002,
               "Callees taking less of the time are not expanded").

%!  .max_depth
%
%   The call tree is not opened up further than this many levels below
%   the current predicate.

class_variable(max_depth, '1..', 40, "Deepest level built").

%!  .group_small
%
%   If @on, the callees of a predicate in the call tree that take less
%   than `min_share` are combined into a single grey "… N more"
%   segment.  The chart leaves out segments that are too small to see,
%   so with many small callees this shows where their time went.  If
%   @off (default), each callee has a segment of its own.

class_variable(group_small, bool, @off,
               "Combine callees below min_share into one node").

initialise(W) :->
    send_super(W, initialise, @nil),
    send(W, name, call_tree),
    send(W, unit, seconds),
    send(W?chart, click_message, message(W, clicked, @arg1)),
    get(W, popup, P),
    send(P, insert_before,
         menu_item(edit,
                   message(W, edit),
                   condition := message(W, can_edit)),
         zoom_out),
    send(P, insert_before,
         menu_item(info,
                   message(W, info),
                   condition := message(W, has_info),
                   end_group := @on),
         zoom_out).

%   The tree shows the current node of the profiler.  Clicking a node
%   makes it the current node, which shows its own tree here and its
%   details and call graph in the other tabs.  The popup acts on the
%   node it was opened on.

can_edit(W) :->
    "True if the popup is on a predicate"::
    get(W, popup_node, Node),
    Node \== @nil,
    get(Node, data, Pred),
    Pred \= '<recursive>',
    Pred \= more(_).

edit(W) :->
    "Edit the predicate the popup is on"::
    get(W, popup_node, Node),
    get(Node, data, Pred),
    edit_predicate(Pred).

has_info(W) :->
    "True if the predicate the popup is on has an info sheet"::
    get(W, popup_node, Node),
    Node \== @nil,
    get(Node, data, Pred),
    predicate_has_info(Pred).

info(W) :->
    "Show the info sheet of the predicate the popup is on"::
    get(W, popup_node, Node),
    get(Node, data, Pred),
    predicate_info(Pred).

node(W, Data:prolog) :->
    "Show the call tree below a node"::
    send(W, slot, node, Data),
    send(W, slot, stale, @on),
    send(W, update).

update(W) :->
    "Build the tree if it changed and I can be seen"::
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
    "Build the call tree below Data"::
    prof_tool(W, Tool),
    get(Tool, window, prof_browser, B),
    chain_list(B?all_items, Items),
    findall(P-D, ( member(DI, Items),
                   get(DI, data, D),
                   P = D.predicate
                 ), Pairs),
    sort(1, @<, Pairs, Unique),
    list_to_assoc(Unique, Nodes),
    get(Tool, measure, Measure),
    (   Measure == time
    ->  get(Tool, ticks, Ticks),
        get(Tool, time, Time),
        TickTime is Time/max(1, Ticks)
    ;   TickTime = 1                    % bytes are shown as they are
    ),
    node_cost(Measure, Data, Self, Children),
    Total is Self + Children,
    get(W, class_variable_value, min_share, MinShare),
    get(W, class_variable_value, max_depth, MaxDepth),
    get(W, class_variable_value, group_small, GroupSmall),
    MinTicks is Total*MinShare,
    get(Tool, components, Components),
    Ctx = tree(Nodes, Components, TickTime, MinTicks, MaxDepth, GroupSmall,
               Measure),
    pred_tree(Ctx, Data, Self, Children, [Data.predicate], Term),
    hierarchy_from_term(Term, Root),
    send(W, time_view),
    send(W, root, Root).

%   As the other tabs, the tree shows times in seconds or as a
%   percentage of the time of the profile, as `Show values as' says.

time_view(W) :->
    "Show values as the profiler's <-time_view and <-measure say"::
    prof_tool(W, Tool),
    get(Tool, time_view, View),
    get(Tool, measure, Measure),
    (   View == percentage
    ->  (   Measure == time
        ->  get(Tool, ticks, Ticks),
            get(Tool, accounting_ticks, Accounting),
            get(Tool, time, Time),
            Reference is (Ticks-Accounting)*Time/max(1, Ticks)
        ;   get(Tool, allocated, Reference)
        ),
        send(W, reference, Reference),
        send(W, unit, percentage)
    ;   Measure == time
    ->  send(W, unit, seconds)
    ;   send(W, unit, bytes)
    ).

%   The unit changes at once; the tree of another measure is built as
%   soon as it can be seen.

refresh(W) :->
    "Build the tree again, now if I can be seen, else when I can"::
    send(W, time_view),
    send(W, slot, stale, @on),
    send(W, update).

%   pred_tree(+Ctx, +Data, +Self, +Below, +Path, -Term)
%
%   Term is the tree for the predicate of Data, called such that it
%   spent Self ticks itself and Below in its callees.

pred_tree(Ctx, Data, Self, Below, Path, Term) :-
    Ctx = tree(_, Components, _, _, _, _, Measure),
    Pred = Data.predicate,
    (   recursive(Pred, Data.callees, Components)
    ->  Edge is Self+Below,
        component_tree(Ctx, Pred, Edge, Path, Term)
    ;   node_cost(Measure, Data, _, Children),
        (   Children > 0
        ->  Scale is Below/Children
        ;   Scale = 0
        ),
        call_tree(Ctx, Pred, Self, Data.callees, Scale, Path, Term)
    ).

recursive(Pred, Callees, Components) :-
    member(Rel, Callees),
    arg(1, Rel, Callee),
    Callee \== '<recursive>',
    same_component(Pred, Callee, Components),
    !.

%   call_tree(+Ctx, +Pred, +Self, +Callees, +Scale, +Path, -Term)
%
%   Term is the tree for Pred, which spent Self ticks itself.  Callees
%   are its callees, whose ticks are multiplied by Scale.

call_tree(Ctx, Pred, Self, Callees, Scale, Path,
          node(Label, Seconds, Children, [data(Pred), title(Title)])) :-
    Ctx = tree(_, Components, TickTime, _, _, _, Measure),
    tree_labels(Pred, Label, Title),
    Seconds is Self*TickTime,
    merged_relatives(Measure, Pred, Callees, Components, Merged),
    callee_trees(Ctx, Pred, Merged, Scale, Path, Children).

%   callee_trees(+Ctx, +Pred, +Callees, +Scale, +Path, -Children)
%
%   Children are the trees for Callees of Pred, a list Callee-t(Self,
%   Children, Calls) whose ticks are multiplied by Scale.  If small
%   callees are grouped and there are at least two of them, they are
%   combined into one node.

callee_trees(Ctx, Pred, Callees, Scale, Path, Children) :-
    Ctx = tree(_, _, _, MinTicks, _, GroupSmall, _),
    findall(c(Callee, Self, Below),
            ( member(Callee-t(T, CT, _), Callees),
              Self is T*Scale,
              Below is CT*Scale,
              Self+Below > 0
            ), Cs),
    (   GroupSmall == @on,
        partition(small_callee(MinTicks), Cs, Small, Large),
        Small = [_,_|_]
    ->  maplist(callee_tree(Ctx, Path), Large, Children0),
        more_tree(Ctx, Pred, Small, More),
        append(Children0, [More], Children)
    ;   maplist(callee_tree(Ctx, Path), Cs, Children)
    ).

small_callee(MinTicks, c(_, Self, Below)) :-
    Self+Below < MinTicks.

more_tree(Ctx, Pred, Small, node(Label, Seconds, [],
                                 [ data(more(Pred)), title(Title),
                                   colour(grey75)
                                 ])) :-
    Ctx = tree(_, _, TickTime, _, _, _, _),
    length(Small, N),
    aggregate_all(sum(S+B), member(c(_, S, B), Small), Ticks),
    Seconds is Ticks*TickTime,
    format(atom(Label), '\u2026 ~D more', [N]),
    pred_label(Pred, Name),
    format(string(Title), '~D small callees of ~w', [N, Name]).

callee_tree(Ctx, Path, c(Callee, Self, Below), Term) :-
    callee_tree(Ctx, Callee, Self, Below, Path, Term).

callee_tree(Ctx, Callee, Self, Below, Path, Term) :-
    Ctx = tree(Nodes, _, TickTime, MinTicks, MaxDepth, _, Measure),
    Edge is Self+Below,
    (   Below > 0,
        Edge >= MinTicks,
        \+ memberchk(Callee, Path),
        length(Path, Depth),
        Depth < MaxDepth,
        get_assoc(Callee, Nodes, Data),
        node_cost(Measure, Data, _, Children),
        Children > 0
    ->  pred_tree(Ctx, Data, Self, Below, [Callee|Path], Term)
    ;   tree_labels(Callee, Label, Title),
        Seconds is Edge*TickTime,
        Term = node(Label, Seconds, [], [data(Callee), title(Title)])
    ).

%   component_tree(+Ctx, +Entry, +Edge, +Path, -Term)
%
%   Term is the tree for the recursion entered through Entry by a call
%   that took Edge ticks.  The members of the recursion form a tree
%   by the calls through which they are first reached from Entry.  Each
%   owns the time it spent itself and in its calls out of the
%   recursion.  These are scaled such that they add up to Edge.

component_tree(Ctx, Entry, Edge, Path, Term) :-
    Ctx = tree(Nodes, Components, _, _, _, _, Measure),
    component_members([Entry-[]], [Entry], Measure, Components, Nodes,
                      Members),
    aggregate_all(sum(Own), member(m(_, _, _, _, Own), Members), Total),
    (   Total > 0
    ->  Scale is Edge/Total
    ;   Scale = 0
    ),
    maplist(arg(1), Members, Preds),
    append(Preds, Path, Path1),
    member_tree(Ctx, Entry, Members, Scale, Path1, Term).

%   component_members(+Queue, +Seen, +Measure, +Components, +Nodes,
%                     -Members)
%
%   Queue holds Pred-Parent pairs.  Members is a list m(Pred, Parent, Self, Out, Own) for the members
%   of the recursion, found breadth first.  Parent is the member from
%   which Pred was first reached (`[]` for the entry), Out are the
%   calls out of the recursion as Callee-t(Self, Children, Calls) and
%   Own is the time spent in Pred itself and in Out.

component_members([], _, _, _, _, []).
component_members([P-Parent|Queue], Seen, Measure, Components, Nodes,
                  [m(P, Parent, Self, Out, Own)|Members]) :-
    (   get_assoc(P, Nodes, Data)
    ->  node_cost(Measure, Data, Self, _),
        merged_relatives(Measure, P, Data.callees, Components, Merged)
    ;   Self = 0,
        Merged = []
    ),
    partition(in_component(P, Components), Merged, In, Out),
    aggregate_all(sum(T+CT), member(_-t(T, CT, _), Out), OutTicks),
    Own is Self+OutTicks,
    findall(Q, ( member(Q-_, In), \+ memberchk(Q, Seen) ), New),
    append(Seen, New, Seen1),
    findall(Q-P, member(Q, New), Children),
    append(Queue, Children, Queue1),
    component_members(Queue1, Seen1, Measure, Components, Nodes, Members).

in_component(P, Components, Q-_) :-
    same_component(P, Q, Components).

member_tree(Ctx, P, Members, Scale, Path,
            node(Label, Seconds, Children, [data(P), title(Title)])) :-
    Ctx = tree(_, _, TickTime, _, _, _, _),
    memberchk(m(P, _, Self, Out, _), Members),
    tree_labels(P, Label, Title),
    Seconds is Self*Scale*TickTime,
    findall(Child,
            ( member(m(Q, P, _, _, _), Members),
              Q \== P,
              member_tree(Ctx, Q, Members, Scale, Path, Child)
            ), InChildren),
    callee_trees(Ctx, P, Out, Scale, Path, OutChildren),
    append(InChildren, OutChildren, Children).

%   As the boxes of the call graph, the segments leave out the module.
%   The balloon shows it.

tree_labels(Pred, Label, Title) :-
    short_pred_label(Pred, Label),
    pred_label(Pred, Title).

clicked(W, Node:hierarchy_node) :->
    "Make the predicate of a node the current one"::
    get(Node, data, Pred),
    get(W, node, Data),
    (   Pred == Data.predicate
    ->  true
    ;   prof_tool(W, Tool),
        get(Tool, node_data, Pred, _)
    ->  send(Tool, details, Pred)
    ;   true
    ).

:- pce_end_class(prof_tree).

%   The call graphs, so they can be drawn again if one of the class
%   variables that limit them changes.

:- pce_global(@prof_graphs, new(chain)).

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
    pred_edges(callers, Tool, Pred, Data, Limits, Callers),
    pred_edges(callees, Tool, Pred, Data, Limits, Callees),
    edge_ends(callers, Callers, Up),
    edge_ends(callees, Callees, Down),
    append([[Pred], Up, Down], Seen0),
    list_to_set(Seen0, Seen),
    append(Callers, Callees, Direct),
    MoreUp is CallerDepth-1,
    MoreDown is CalleeDepth-1,
    walk(MoreUp-Up, MoreDown-Down, Tool, Limits, Seen, Direct, Edges1),
    unique_edges(Edges1, Edges2),
    graph_preds(Pred, Edges2, Preds),
    foldl(recursion_edge(Tool, Data), Preds, Edges, Edges2).

%!  expand_hidden(+Tool, +Data, +Expanded, +Edges0, -Edges) is det.
%
%   Add all callers or callees of the predicates in Expanded, a list of
%   Dir-Pred whose "… N more" node was clicked, to the graph Edges0 and
%   connect them to the predicates that are already there.  An added
%   predicate may have been expanded itself, so this repeats until
%   nothing is added.

expand_hidden(Tool, Data, Expanded, Edges0, Edges) :-
    Pred = Data.predicate,
    graph_preds(Pred, Edges0, Shown),
    get(Tool, components, Components),
    get(Tool, measure, Measure),
    findall(Edge,
            ( member(Dir-P, Expanded),
              memberchk(P, Shown),
              pred_data(Tool, Data, P, PData),
              merged_relatives(Measure, P, PData.Dir, Components, Merged),
              member(Other-t(Self, Children, Calls), Merged),
              \+ memberchk(Other, Shown),
              Ticks is Self+Children,
              edge(Dir, P, Other, Ticks, Calls, Edge)
            ), Opened),
    (   Opened == []
    ->  Edges = Edges0
    ;   findall(Other, ( member(edge(F, T, _, _), Opened),
                         ( Other = F ; Other = T ),
                         \+ memberchk(Other, Shown)
                       ), New0),
        list_to_set(New0, New),
        append(Shown, New, Shown1),
        findall(Edge,
                ( member(N, New),
                  pred_data(Tool, Data, N, NData),
                  member(Dir, [callers, callees]),
                  merged_relatives(Measure, N, NData.Dir, Components,
                                   Merged),
                  member(Other-t(Self, Children, Calls), Merged),
                  Other \== N,
                  memberchk(Other, Shown1),
                  Ticks is Self+Children,
                  edge(Dir, N, Other, Ticks, Calls, Edge)
                ), Connections),
        foldl(recursion_edge(Tool, Data), New, Recursion, []),
        append([Edges0, Opened, Connections, Recursion], Edges1),
        unique_edges(Edges1, Edges2),
        expand_hidden(Tool, Data, Expanded, Edges2, Edges)
    ).

pred_data(_, Data, P, Data) :-
    P == Data.predicate,
    !.
pred_data(Tool, _, P, Data) :-
    get(Tool, node_data, P, Data).

%!  hidden_edges(+Tool, +Data, +Edges, -Hidden) is det.
%
%   Hidden are edges to or from a node more(Dir, P, N) that stands for
%   the N callers or callees (Dir) of P that are not in the graph of
%   Edges, with their summed time and calls.  The node in the middle and
%   the predicates above it get one for their callers, the node in the
%   middle and those below it one for their callees.

hidden_edges(Tool, Data, Edges, Hidden) :-
    Pred = Data.predicate,
    graph_preds(Pred, Edges, Shown),
    reachable(callers, Edges, [Pred], Above),
    reachable(callees, Edges, [Pred], Below),
    get(Tool, components, Components),
    get(Tool, measure, Measure),
    findall(Edge,
            ( (   Dir = callers, member(P, Above)
              ;   Dir = callees, member(P, Below)
              ),
              pred_data(Tool, Data, P, PData),
              merged_relatives(Measure, P, PData.Dir, Components, Merged),
              findall(Ticks-Calls,
                      ( member(Other-t(Self, Children, Calls), Merged),
                        \+ memberchk(Other, Shown),
                        Ticks is Self+Children
                      ), Pairs),
              Pairs \== [],
              length(Pairs, N),
              aggregate_all(sum(T), member(T-_, Pairs), Ticks),
              aggregate_all(sum(C), member(_-C, Pairs), Calls),
              edge(Dir, P, more(Dir, P, N), Ticks, Calls, Edge)
            ), Hidden).

%   reachable(+Dir, +Edges, +Preds, -Reached)
%
%   Reached are Preds and the predicates reached from them by following
%   the callers or callees (Dir) in Edges.

reachable(Dir, Edges, Preds, Reached) :-
    reachable(Preds, Dir, Edges, Preds, Reached).

reachable([], _, _, Reached, Reached).
reachable([P|Queue], Dir, Edges, Seen, Reached) :-
    findall(Q, ( edge(Dir, P, Q, _, _, E),
                 member(E, Edges),
                 \+ memberchk(Q, Seen)
               ), New0),
    list_to_set(New0, New),
    append(Seen, New, Seen1),
    append(Queue, New, Queue1),
    reachable(Queue1, Dir, Edges, Seen1, Reached).

%   Recursion shows as one call of a predicate to itself.  That is for
%   every predicate in the graph, also those of which the relatives are
%   not walked.

recursion_edge(Tool, Data, P, [edge(P, P, 0, Calls)|Edges], Edges) :-
    (   P == Data.predicate
    ->  PData = Data
    ;   get(Tool, node_data, P, PData)
    ),
    recursive_calls(PData, Calls),
    Calls > 0,
    !.
recursion_edge(_, _, _, Edges, Edges).

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
              pred_edges(Dir, Tool, P, Data, Limits, PEdges),
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

%!  pred_edges(+Dir, +Tool, +Pred, +Data, +Limits, -Edges) is det.
%
%   The calls to (callers) or from (callees) Pred, as far as
%   prune_relatives/3 keeps them, merged over the cycles by
%   merged_relatives/4.  Recursion is added by call_graph/4.

pred_edges(Dir, Tool, Pred, Data, Limits, Edges) :-
    get(Tool, components, Components),
    get(Tool, measure, Measure),
    merged_relatives(Measure, Pred, Data.Dir, Components, Merged),
    findall(r(Ticks, Calls, Other),
            ( member(Other-t(Self, Children, Calls), Merged),
              Ticks is Self+Children
            ),
            Relatives),
    prune_relatives(Relatives, Limits, Kept),
    findall(Edge,
            ( member(r(Ticks, Calls, Other), Kept),
              edge(Dir, Pred, Other, Ticks, Calls, Edge)
            ),
            Edges).

%!  recursive_calls(+Data, -Calls) is det.
%
%   The calls of the predicate of Data to itself.  The profile lists them
%   per cycle the predicate is part of.

recursive_calls(Data, Calls) :-
    aggregate_all(sum(C),
                  ( member(Rel, Data.callers),
                    arg(1, Rel, '<recursive>'),
                    arg(5, Rel, C)
                  ),
                  Calls).

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
    ;   aggregate_all(sum(T), member(r(T,_,_), Relatives), Total),
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

%!  merged_relatives(+Measure, +Pred, +Relatives, +Components,
%!                   -Merged) is det.
%
%   Merge the callers or callees of Pred, which the profile splits by
%   the cycles (recursion levels) of Pred, into a list Other-t(Self,
%   Children, Calls) with one element per predicate.  Self and Children
%   are the cost of Measure (see rel_cost/4).  The calls are
%   summed.  The time depends on whether Other is part of the same
%   recursion as Pred, i.e., in the same strongly connected component
%   of the call graph:
%
%     - If so, the call at a deeper level is made from within the call
%       at the level above it, so the time of the outermost cycle
%       includes that of all others.
%     - If not, the calls at the various levels are distinct, and their
%       times are summed.

merged_relatives(Measure, Pred, Relatives, Components, Merged) :-
    findall(Other-r(Cycle, Self, Children, Calls),
            ( member(Rel, Relatives),
              arg(1, Rel, Other),
              Other \== '<recursive>',
              arg(2, Rel, Cycle),
              rel_cost(Measure, Rel, Self, Children),
              arg(5, Rel, Calls)
            ),
            Pairs0),
    keysort(Pairs0, Pairs),
    group_pairs_by_key(Pairs, Grouped),
    maplist(merge_relative(Pred, Components), Grouped, Merged).

merge_relative(Pred, Components, Other-Rs, Other-t(Self, Children, Calls)) :-
    aggregate_all(sum(C), member(r(_,_,_,C), Rs), Calls),
    (   same_component(Pred, Other, Components)
    ->  msort(Rs, [r(_, Self, Children, _)|_])
    ;   aggregate_all(sum(S), member(r(_,S,_,_), Rs), Self),
        aggregate_all(sum(Ch), member(r(_,_,Ch,_), Rs), Children)
    ).

same_component(P, P, _) :- !.
same_component(P1, P2, Components) :-
    get_assoc(P1, Components, C),
    get_assoc(P2, Components, C).

%!  call_graph_components(+Nodes, -Components) is det.
%
%   Components is an assoc that maps each predicate of the profile Nodes
%   to the strongly connected component of the call graph it is part of,
%   computed using Tarjan's algorithm.

call_graph_components(Nodes, Components) :-
    findall(P-Callees,
            ( member(N, Nodes),
              P = N.predicate,
              findall(Q, ( member(Rel, N.callees),
                           arg(1, Rel, Q),
                           Q \== '<recursive>'
                         ), Qs0),
              sort(Qs0, Callees)
            ), Pairs),
    list_to_assoc(Pairs, Graph),
    pairs_keys(Pairs, Vertices),
    empty_assoc(E),
    foldl(scc_root(Graph), Vertices, scc(0, E, E, [], E, 0), State),
    State = scc(_, _, _, _, Components, _).

%   The state is scc(Index, Indices, Lows, Stack, Components, NextComp).
%   A vertex is on the stack if it has an index but no component.

scc_root(Graph, V, S0, S) :-
    S0 = scc(_, Indices, _, _, _, _),
    (   get_assoc(V, Indices, _)
    ->  S = S0
    ;   strong_connect(Graph, V, S0, S)
    ).

strong_connect(Graph, V, S0, S) :-
    S0 = scc(I, Indices0, Lows0, Stack0, Comps, NC),
    put_assoc(V, Indices0, I, Indices),
    put_assoc(V, Lows0, I, Lows),
    I1 is I+1,
    (   get_assoc(V, Graph, Ws)
    ->  true
    ;   Ws = []
    ),
    foldl(scc_edge(Graph, V), Ws,
          scc(I1, Indices, Lows, [V|Stack0], Comps, NC), S1),
    S1 = scc(I2, Indices2, Lows2, Stack2, Comps2, NC2),
    (   get_assoc(V, Lows2, Low),
        Low =:= I
    ->  pop_component(Stack2, V, NC2, Comps2, Stack3, Comps3),
        NC3 is NC2+1,
        S = scc(I2, Indices2, Lows2, Stack3, Comps3, NC3)
    ;   S = S1
    ).

scc_edge(Graph, V, W, S0, S) :-
    S0 = scc(_, Indices, _, _, Comps, _),
    (   \+ get_assoc(W, Indices, _)
    ->  strong_connect(Graph, W, S0, S1),
        S1 = scc(I, Ix, Lows1, St, Cs, NC),
        get_assoc(W, Lows1, LW),
        lower(V, LW, Lows1, Lows),
        S = scc(I, Ix, Lows, St, Cs, NC)
    ;   \+ get_assoc(W, Comps, _)     % on the stack
    ->  get_assoc(W, Indices, IW),
        S0 = scc(I, Ix, Lows0, St, Cs, NC),
        lower(V, IW, Lows0, Lows),
        S = scc(I, Ix, Lows, St, Cs, NC)
    ;   S = S0
    ).

lower(V, L, Lows0, Lows) :-
    get_assoc(V, Lows0, LV),
    (   L < LV
    ->  put_assoc(V, Lows0, L, Lows)
    ;   Lows = Lows0
    ).

pop_component([W|Stack0], V, NC, Comps0, Stack, Comps) :-
    put_assoc(W, Comps0, NC, Comps1),
    (   W == V
    ->  Stack = Stack0,
        Comps = Comps1
    ;   pop_component(Stack0, V, NC, Comps1, Stack, Comps)
    ).

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

%!  dot_graph(+Tool, +Pred, +Total, +Colours, +MaxWidth, +Ids, +Edges)//
%
%   The call graph in the dot language.  MaxWidth is the width of an
%   edge along which all of the time flows.

dot_graph(Tool, Pred, Total, colours(FG), MaxWidth, Ids, Edges) -->
    "digraph calls {\n",
    "  graph [rankdir=TB nodesep=0.25 ranksep=0.45];\n",
    "  node [shape=box style=filled fontname=\"Helvetica\" \c
     fontsize=10 fontcolor=\"black\" color=\"#606060\"];\n",
    "  edge [fontname=\"Helvetica\" fontsize=9 arrowsize=0.7 ",
    "color=", dot_string(FG), " fontcolor=", dot_string(FG), "];\n",
    dot_nodes(Ids, Tool, Pred, Total),
    dot_edges(Edges, Tool, Total, MaxWidth, Ids),
    "}\n".

%!  graph_graphicals(+Figure, +Tool, +Pred, +Total, +Colours, +MaxWidth,
%!                   +Ids, +Edges) is det.
%
%   The call graph as XPCE graphicals in Figure, a graphviz_figure.
%   The nodes and edges are described by the same attributes as for
%   dot_graph//7, with the same defaults, but they are drawn here.
%   Graphviz only places them.

graph_graphicals(X, Tool, Pred, Total, colours(FG), MaxWidth, Ids, Edges) :-
    send(X, clear, destroy),
    forall(member(Id-P, Ids),
           ( node_attrs(Tool, P, Pred, Total, Attrs),
             node_graphical(Id, Attrs, Node),
             send(X, display, Node)
           )),
    forall(member(Edge, Edges),
           ( Edge = edge(From, To, _, _),
             memberchk(FromId-From, Ids),
             memberchk(ToId-To, Ids),
             edge_attrs(Tool, Edge, Total, MaxWidth, Attrs),
             edge_graphical(FromId, ToId, Attrs, FG, GE),
             send(X, display, GE)
           )),
    send(X, lay_out, [rankdir='TB', nodesep=0.25, ranksep=0.45]).

%   A box around the label, with the margins graphviz gives a box: 8
%   points to the sides and 4 above and below.

node_graphical(Id, Attrs, Node) :-
    new(Node, graphviz_node(Id)),
    option(label(Label), Attrs),
    option(fontcolor(FontColour), Attrs, black),
    option(color(PenColour), Attrs, '#606060'),
    option(penwidth(Pen), Attrs, 1),
    option(style(Style), Attrs, filled),
    (   option(fontname(Face), Attrs),
        sub_atom(Face, _, _, _, 'Bold')
    ->  Font = bold
    ;   Font = normal
    ),
    new(T, text(Label, center, Font)),
    send(T, colour, FontColour),
    get(T, size, size(TW, TH)),
    BW is TW+16,
    BH is TH+8,
    new(B, box(BW, BH)),
    send(B, pen, Pen),
    send(B, colour, PenColour),
    (   sub_atom(Style, _, _, _, rounded)
    ->  send(B, radius, 6)
    ;   true
    ),
    (   sub_atom(Style, _, _, _, dashed)
    ->  send(B, texture, dashed)
    ;   true
    ),
    (   option(fillcolor(Fill), Attrs)
    ->  send(B, fill, Fill)
    ;   true
    ),
    send(Node, display, B),
    send(Node, display, T),
    send(T, center, B?center),
    graphical_tooltip(Node, Attrs).

edge_graphical(FromId, ToId, Attrs, FG, Edge) :-
    option(label(Label), Attrs),
    option(color(Colour), Attrs, FG),
    option(penwidth(Pen), Attrs, 1),
    new(T, text(Label, center, small)),
    send(T, colour, FG),
    new(Edge, graphviz_edge(FromId, ToId, T,
                            [arrowsize=0.7, penwidth=Pen])),
    send(Edge, line_width, Pen),
    send(Edge, line_colour, Colour),
    (   option(style(dashed), Attrs)
    ->  send(Edge, line_texture, dashed)
    ;   true
    ),
    graphical_tooltip(Edge, Attrs).

graphical_tooltip(Gr, Attrs) :-
    (   option(tooltip(Tip), Attrs)
    ->  send(Gr, help_message, tag, string('%s', Tip))
    ;   true
    ).

dot_nodes([], _, _, _) --> [].
dot_nodes([Id-P|T], Tool, Pred, Total) -->
    { node_attrs(Tool, P, Pred, Total, Attrs) },
    "  ", atom(Id), " [", dot_attrs(Attrs), "];\n",
    dot_nodes(T, Tool, Pred, Total).

node_attrs(_Tool, more(Dir, P, N), _Pred, _Total, Attrs) :-
    !,
    format(string(Label), '\u2026 ~D more', [N]),
    relatives_text(Dir, N, What),
    pred_label(P, Name),
    format(string(Tooltip), '~D more ~w of ~w\n(click to show them)',
           [N, What, Name]),
    Attrs = [ label=Label, tooltip=Tooltip, style='dashed,rounded',
              color='#909090', fontcolor='#909090'
            ].
node_attrs(Tool, P, Pred, Total, Attrs) :-
    pred_label(P, Name),
    short_pred_label(P, Short),
    (   get(Tool, node_data, P, Data)
    ->  get(Tool, measure, Measure),
        node_cost(Measure, Data, Self, Children),
        Incl is Self + Children,
        time_text(Tool, Incl, InclText),
        time_text(Tool, Self, SelfText),
        format(string(Label), '~w\n~w (self ~w)', [Short, InclText, SelfText]),
        port_text(Tool, Data, PortText),
        measure_title(Measure, Title),
        discarded_text(Measure, Data, DiscardedText),
        format(string(Tooltip), '~w\n~w: ~w, self ~w~w\n~w',
               [Name, Title, InclText, SelfText, DiscardedText, PortText]),
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

%   The calls to a predicate, and how they ended if the profile
%   recorded the ports.

port_text(Tool, Data, Text) :-
    (   get(Tool, ports, false)
    ->  format(string(Text), 'Call: ~D', [Data.call])
    ;   Fail is Data.call + Data.redo - Data.exit,
        format(string(Text), 'Call: ~D, redo: ~D, exit: ~D, fail: ~D',
               [Data.call, Data.redo, Data.exit, Fail])
    ).

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

%   discarded_text(+Measure, +Data, -Text)
%
%   For memory, the line telling the memory the predicate discarded.

discarded_text(memory, Data, Text) :-
    !,
    format_value(bytes, Data.get(discarded_self, 0), Discarded),
    format(string(Text), '\nDiscarded: ~w', [Discarded]).
discarded_text(_, _, "").

%   time_text(+Tool, +Value, -Text)
%
%   Text renders Value, a time or memory value of the current measure.

time_text(Tool, Value, Text) :-
    get(Tool, render_cost, Value, Rendered),
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

dot_edges([], _, _, _, _) --> [].
dot_edges([Edge|T], Tool, Total, MaxWidth, Ids) -->
    { Edge = edge(From, To, _, _),
      memberchk(FromId-From, Ids),
      memberchk(ToId-To, Ids),
      edge_attrs(Tool, Edge, Total, MaxWidth, Attrs)
    },
    "  ", atom(FromId), " -> ", atom(ToId), " [", dot_attrs(Attrs), "];\n",
    dot_edges(T, Tool, Total, MaxWidth, Ids).

%   An edge is labelled with the number of calls only.  The time of a
%   call into a cycle does not add up to anything that reads well: the
%   time of a call at an outer level includes that of the calls further
%   in.  It does set the width of the edge, and the tooltip tells it.

edge_attrs(Tool, edge(From, To, Ticks, Calls), Total, MaxWidth, Attrs) :-
    format(string(Label), '~D×', [Calls]),
    Share = min(1.0, Ticks/float(Total)),
    Width is round(10*(1 + (max(1, MaxWidth)-1)*Share))/10.0,
    edge_tooltip(Tool, From, To, Ticks, Calls, Tooltip),
    Attrs0 = [label=Label, tooltip=Tooltip, penwidth=Width],
    (   ( From = more(_, _, _) ; To = more(_, _, _) )
    ->  append(Attrs0, [style=dashed, color='#909090'], Attrs)
    ;   Attrs = Attrs0
    ).

measure_word(Tool, Word) :-
    get(Tool, measure, Measure),
    measure_word_(Measure, Word).

measure_word_(time,   time).
measure_word_(memory, allocated).

relatives_text(callers, 1, caller) :- !.
relatives_text(callers, _, callers).
relatives_text(callees, 1, callee) :- !.
relatives_text(callees, _, callees).

edge_tooltip(Tool, more(callers, P, N), P, Ticks, Calls, Tooltip) :-
    !,
    relatives_text(callers, N, What),
    pred_label(P, Name),
    time_text(Tool, Ticks, Time),
    measure_word(Tool, Word),
    format(string(Tooltip), '~D more ~w → ~w\nCalls: ~D, ~w: ~w',
           [N, What, Name, Calls, Word, Time]).
edge_tooltip(Tool, P, more(callees, P, N), Ticks, Calls, Tooltip) :-
    !,
    relatives_text(callees, N, What),
    pred_label(P, Name),
    time_text(Tool, Ticks, Time),
    measure_word(Tool, Word),
    format(string(Tooltip), '~w → ~D more ~w\nCalls: ~D, ~w: ~w',
           [Name, N, What, Calls, Word, Time]).
edge_tooltip(_Tool, P, P, _Ticks, Calls, Tooltip) :-
    !,
    pred_label(P, Name),
    format(string(Tooltip), '~w calls itself\nCalls: ~D', [Name, Calls]).
edge_tooltip(Tool, From, To, Ticks, Calls, Tooltip) :-
    pred_label(From, FromName),
    pred_label(To, ToName),
    time_text(Tool, Ticks, Time),
    measure_word(Tool, Word),
    format(string(Tooltip), '~w → ~w\nCalls: ~D, ~w: ~w',
           [FromName, ToName, Calls, Word, Time]).

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
    new(P, popup),
    send_list(P, append,
              [ menu_item(details,
                          message(Text, details),
                          condition := Text?role \== self),
                menu_item(edit,
                          message(Text, edit),
                          condition := message(Text, has_source)),
                menu_item(info,
                          message(Text, info),
                          condition := message(Text, has_info)),
                menu_item(documentation,
                          message(Text, help),
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

%       The context is a prolog_predicate or an xpce method.  A method
%       is looked up in the HTML manual and edited using edit/1, which
%       also finds the C source of a built-in method.  The manual tools
%       of manpce/0 are not involved: loading them opens their window.

has_source(T) :->
    "Succeed if the source of my context can be edited"::
    get(T, context, Ctx),
    (   send(Ctx, instance_of, method)
    ->  method_has_source(Ctx)
    ;   get(Ctx, source, _)
    ).

method_has_source(M) :-
    get(M, slot, source, Loc),
    Loc \== @nil,
    !.
method_has_source(M) :-
    get(M, slot, function, Address),
    Address \== 0.

edit(T) :->
    "Edit the source of my context"::
    get(T, context, Ctx),
    (   send(Ctx, instance_of, method)
    ->  edit(Ctx)
    ;   send(Ctx, edit)
    ).

has_info(T) :->
    "Succeed if my context has an info sheet"::
    get(T, context, Ctx),
    predicate_has_info(Ctx).

info(T) :->
    "Show the info sheet of my context"::
    get(T, context, Ctx),
    predicate_info(Ctx).

has_help(T) :->
    "Succeed if the manual documents my context"::
    get(T, context, Ctx),
    (   send(Ctx, instance_of, method)
    ->  man_documented(Ctx)
    ;   send(Ctx, has_send_method, has_help),
        send(Ctx, has_help)
    ).

help(T) :->
    "Show the documentation of my context"::
    get(T, context, Ctx),
    (   send(Ctx, instance_of, method)
    ->  man_html_object(Ctx)
    ;   send(Ctx, help)
    ).

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
    object(Data.predicate),
    !,
    get(Data.predicate, name, Name).
value(name, Data, Name) :-
    !,
    predicate_sort_key(Data.predicate, Name).
value(label, Data, Label) :-
    !,
    pce_predicate_label(Data.predicate, Label).
value(ticks, Data, Ticks) :-
    !,
    Ticks is Data.ticks_self + Data.ticks_siblings.
value(alloc, Data, Bytes) :-
    !,
    node_cost(memory, Data, Self, Children),
    Bytes is Self+Children.
value(Name, Data, Value) :-
    Value = Data.get(Name, 0).

%   sort_kind(?Kind, ?Name, ?Label)
%   sort_key(?Kind, ?Key, ?Label, ?Order)
%
%   The keys the predicates can be sorted on, grouped by whether the
%   profile is cumulative (including the callees) or flat.

sort_kind(cumulative, sort_cumulative, 'Cumulative').
sort_kind(flat,       sort_flat,       'Flat').

sort_key(cumulative, ticks,          'Time',             reverse).
sort_key(cumulative, alloc,          'Memory',           reverse).
sort_key(flat,       ticks_self,     'Time',             reverse).
sort_key(flat,       alloc_self,     'Memory',           reverse).
sort_key(flat,       discarded_self, 'Discarded memory', reverse).
sort_key(flat,       call,           'Number of calls',  reverse).
sort_key(flat,       redo,           'Number of redos',  reverse).

%   sort_label(+Key, -Label)
%
%   Label tells how the predicates are sorted, e.g., "Flat profile by
%   number of calls".

sort_label(Key, Label) :-
    sort_key(Kind, Key, KeyLabel, _),
    !,
    sort_kind(Kind, _, KindLabel),
    downcase_atom(KeyLabel, KL),
    format(atom(Label), '~w profile by ~w', [KindLabel, KL]).

measure(time,   'Time').
measure(memory, 'Global stack memory').

time_view(percentage, 'Percentage').
time_view(seconds,    'Seconds or bytes').

%!  measure_key(+Key, ?Measure, -MeasureKey) is semidet.
%
%   MeasureKey is the sort key of Measure that corresponds to Key, a
%   sort key of either measure.  Fails if Key is not about a measure,
%   e.g., the number of calls.

measure_key(Key, Measure, MeasureKey) :-
    key_pair(Time, Memory),
    (   Key == Time
    ;   Key == Memory
    ),
    !,
    (   Measure = time,
        MeasureKey = Time
    ;   Measure = memory,
        MeasureKey = Memory
    ).

key_pair(ticks,          alloc).
key_pair(ticks_self,     alloc_self).
key_pair(ticks_self,     discarded_self).

%!  node_cost(+Measure, +Data, -Self, -Children) is det.
%
%   The time (ticks) or global stack allocated (bytes) of the profile
%   node Data, by the predicate itself and by its callees.

node_cost(time, Data, Self, Children) :-
    Self = Data.ticks_self,
    Children = Data.ticks_siblings.
node_cost(memory, Data, Self, Children) :-
    Self = Data.get(alloc_self, 0),
    Children = Data.get(alloc_siblings, 0).

%!  rel_cost(+Measure, +Relative, -Self, -Children) is det.
%
%   As node_cost/4, for a caller or callee, a term node/8 (see
%   profile_data/1).  An older node/7 has no memory.

rel_cost(time, Rel, Self, Children) :-
    arg(3, Rel, Self),
    arg(4, Rel, Children).
rel_cost(memory, Rel, Self, Children) :-
    rel_mem(Rel, mem(Self, Children, _, _)).

rel_discarded(Rel, Discarded) :-
    rel_mem(Rel, mem(_, _, Discarded, _)).

rel_mem(Rel, Mem) :-
    (   arg(8, Rel, Mem0)
    ->  Mem = Mem0
    ;   Mem = mem(0, 0, 0, 0)
    ).


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
