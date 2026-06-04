/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(tube_gui,
          [ london_tube/0
          ]).
:- encoding(utf8).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pce_util)).

:- use_module(tube).
:- use_module(library(pan_zoom)).

/** <module> Interactive London tube map

An xpce GUI for exploring the London tube network. Launch with

    ?- london_tube.

# Window layout

A single frame holds four sibling windows arranged as:

  - `tube_dialog` (top): live `live_text_item` station search, plus
    *Select all* / *Clear all* buttons.
  - `line_browser` (left of the map): one row per tube line, each
    coloured in the line's official colour with a luminance-chosen
    foreground. Clicking flips a ☑/☐ glyph and adds or removes that
    line's `line_segment` graphicals on the map.
  - `tube_window` (the picture itself): hosts the `network` figure
    that displays stations and the currently shown line segments.
  - `report_dialog` (bottom): receives status messages bubbled up
    from any graphical via `->report`.

# Class architecture

  - tube_window — a `picture` subclass. Knows how to add a station
    (projecting `<-lat`/`<-lon` to pixels via station_position/7)
    and to recenter on a station or on the overall bounding box.
  - network — a `figure` hosting stations and line_segments. It is
    only a container of graphics: pan/zoom is delegated to a
    `pan_zoom_recogniser` (see pan_zoom.pl) attached at
    `->initialise`. Public methods split into three concerns:
      * graph editing: `->add_line`, `->remove_line`
      * highlight policy: `->light_lines`, `->unlight_lines`,
        `->highlight_matches`
      * viewport: `->zoom_to_matches`, `->reset_view`, plus the
        `<-matching` query.
  - line_segment — a `connection` between two stations. Knows
    which `<-line` it belongs to. Hovering it lights its whole
    line (delegates to `network->light_lines`).
  - station — a `figure` with N/S/E/W `handle/4` declarations so
    line_segments anchor cleanly. Hovering it lights every line
    passing through. `->matches/1` is the substring predicate used
    by `<-matching` on the network.
  - live_text_item — a `text_item` whose `->typed` re-applies on
    every keystroke instead of waiting for Enter.
  - line_browser — a `browser` showing tube lines. Selection state
    lives on each `dict_item` as a `selected` attribute (xpce
    `@on`/`@off`); the leading glyph is rendered from that state.
  - tube_dialog — a `dialog` whose `->fill_dialog` populates the
    search field and the two batch buttons.

# Data sources

The dynamic facts station/8, connection/4 and line/4 (see tube.pl)
are loaded once from `tube.json` by `load_tube/0`. The convenience
accessors station/1 and station_display_name/2 are exported from
tube.pl; the GUI only adds the pixel projection station_position/7
and its tabled range/3 helper.

@see pan_zoom.pl for the pan/zoom recogniser.
@see tube.pl for the data layer.
*/

%!  london_tube is det.
%
%   Open the tube map. Equivalent to `gui(_)`.

london_tube :-
    gui(_).

%!  gui(-Frame) is det.
%
%   Build the four-window frame and load the data. Frame is the
%   tube_window (the picture) — the frame owning all four siblings
%   can be reached via `<-frame`.

gui(W) :-
    load_tube,
    new(W, tube_window),
    get(W, member, network, N),
    new(LB, line_browser(N)),
    new(D, tube_dialog),
    send(D, above, LB),
    send(LB, left, W),
    send(D, fill_dialog, LB, N),
    send(new(report_dialog), below, W),
    send(W, open),
    forall(station(Name), send(W, add_station(Name))),
    send(W, recenter, 'Holborn').

%!  search_callback(+TI, +N, +Pattern) is det.
%
%   Run a search interactively. Wired as the `live_text_item` message
%   so it fires per keystroke. Highlights the matching stations on N,
%   reports the count, fits the view when there are hits, resets the
%   view when Pattern is empty, and recolours the typed text red when
%   the (non-empty) pattern matches nothing.

search_callback(TI, N, Pattern) :-
    get(N, matching, Pattern, Matches),
    send(N, highlight_matches, Matches),
    get(Matches, size, Count),
    (   Pattern == ''
    ->  send(N, reset_view),
        send(N, report, status, ''),
        send(TI?value_text, colour, black)
    ;   send(N, report, status, '%d matches', Count),
        (   Count > 0
        ->  send(N, zoom_to_matches, Matches),
            send(TI?value_text, colour, black)
        ;   send(TI?value_text, colour, red)
        )
    ).

:- pce_begin_class(tube_window, picture).

class_variable(size,     size, size(800,500)).
class_variable(map_size, size, size(12000,7000)).

initialise(W) :->
    send_super(W, initialise, 'London Tube Network'),
    send(W, display, new(network)).

add_station(Win, Name:name=name) :->
    "Add named station"::
    get(Win, class_variable_value, map_size, size(W,H)),
    get(Win, member, network, N),
    new(S, station(Name)),
    get(S, size, size(SW,SH)),
    station_position(Name, SW, SH, W, H, X, Y),
    send(N, display, S, point(X,Y)).

station(Win, Name:name=name, Station:station) :<-
    "Find station by name"::
    get(Win, member, network, N),
    get(N, member, Name, Station).

recenter(Win, StationName:station=[name]) :->
    "Center contents"::
    get(Win, size, size(W,H)),
    (   StationName == @default
    ->  get(Win?bounding_box, center, point(X,Y))
    ;   get(Win, station, StationName, Station),
        get(Station, center, point(X,Y))
    ),
    send(Win, scroll_to, point(X-W/2, Y-H/2)).

:- pce_end_class(tube_window).


:- pce_begin_class(network, figure).

initialise(N) :->
    send_super(N, initialise),
    send(N, recogniser, @pan_zoom_recogniser).

add_line(N, LineName:name) :->
    "Display connections for the named tube line"::
    line(LineId, LineName, _Colour, _Stripe),
    forall(connection(S1Id, S2Id, LineId, _Time),
           add_connection(N, S1Id, S2Id, LineName)).

remove_line(Network, LineName:name) :->
    "Remove connections for the named tube line"::
    send(Network?graphicals, for_all,
         if(and(message(@arg1, instance_of, line_segment),
                @arg1?line == LineName),
            message(@arg1, free))).

matching(N, Pattern:name, Matches:chain) :<-
    "Chain of stations whose ->matches(Pattern) succeeds"::
    get(N?graphicals, find_all,
        and(message(@arg1, instance_of, station),
            message(@arg1, matches, Pattern)),
        Matches).

highlight_matches(N, Matches:chain) :->
    "Clear all station match marks, then mark every member of Matches"::
    send(N?graphicals, for_all,
         if(message(@arg1, instance_of, station),
            message(@arg1, matched, @off))),
    send(Matches, for_all, message(@arg1, matched, @on)).

zoom_to_matches(N, Matches:chain) :->
    "Fit the view to the bounding box of Matches; no-op when empty"::
    (   send(Matches, empty)
    ->  true
    ;   fit_view(N, Matches)
    ).

reset_view(N) :->
    "Restore identity transform and recenter the picture"::
    send(N, translate, 0, 0),
    get(N, transform, T),
    send(T, set, 1, 0, 0, 1, 0, 0),
    send(N, transform, T),
    send(N?window, recenter).

light_lines(N, Lines:chain) :->
    "Expose every segment whose <-line is in Lines, mark endpoints hovered"::
    send(N?graphicals, for_all,
         if(and(message(@arg1, instance_of, line_segment),
                message(Lines, member, @arg1?line)),
            and(message(@arg1,      expose),
                message(@arg1?from, expose),
                message(@arg1?to,   expose),
                message(@arg1?from, hovered, @on),
                message(@arg1?to,   hovered, @on)))).

unlight_lines(N, Lines:chain) :->
    "Clear hovered state on endpoint stations of those segments"::
    send(N?graphicals, for_all,
         if(and(message(@arg1, instance_of, line_segment),
                message(Lines, member, @arg1?line)),
            and(message(@arg1?from, hovered, @off),
                message(@arg1?to,   hovered, @off)))).

fit_view(N, Stations) :-
    bbox_of_chain(Stations, BX, BY, BW, BH),
    get(N, window, Win),
    get(Win, size, size(W, H)),
    Margin = 20,
    AvailW is max(1, W - 2*Margin),
    AvailH is max(1, H - 2*Margin),
    (   BW =< AvailW, BH =< AvailH
    ->  S = 1.0
    ;   S is min(AvailW/BW, AvailH/BH)
    ),
    CX is BX + BW/2,
    CY is BY + BH/2,
    TX is W/2 - S*CX,
    TY is H/2 - S*CY,
    send(N, translate, 0, 0),
    get(N, transform, T),
    send(T, set, S, 0, 0, S, TX, TY),
    send(N, transform, T),
    send(Win, scroll_to, point(0, 0)).

bbox_of_chain(Chain, BX, BY, BW, BH) :-
    new(Union, area(0, 0, 0, 0)),
    send(Chain, for_all, message(Union, union, @arg1?area)),
    object(Union, area(BX, BY, BW, BH)),
    send(Union, free).

add_connection(N, S1Id, S2Id, Link) :-
    station(S1Id, S1Name, _, _, _, _, _, _),
    station(S2Id, S2Name, _, _, _, _, _, _),
    get(N, member, S1Name, S1),
    get(N, member, S2Name, S2),
    !,
    new(_, line_segment(S1, S2, Link)).
add_connection(_, _, _, _).

:- pce_end_class(network).

:- pce_begin_class(line_segment, connection,
                   "Line between two stations").

class_variable(pen, num, 7, "Pen width").

variable(line, name, get, "Tube line I belong to").

initialise(LS, S1:station, S2:station, LineName:name) :->
    "Create connection for line"::
    get(LS, class_variable_value, pen, Pen),
    line_link(LineName, Pen, Link),
    send_super(LS, initialise, S1, S2, Link),
    send(LS, slot, line, LineName).

line_link(LineName, Pen, Link) :-
    line(_LineId, LineName, Colour, _Stripe),
    new(L, line),
    send(L, pen, Pen),
    send(L, colour, Colour),
    new(Link, link(link, link, L)).

event(LS, Ev:event) :->
    "Hover: expose + highlight on enter, unhighlight on exit"::
    (   send(Ev, is_a, area_enter)
    ->  send(LS, hover, @on)
    ;   send(Ev, is_a, area_exit)
    ->  send(LS, hover, @off)
    ;   send_super(LS, event, Ev)
    ).

hover(LS, On:bool) :->
    "Light or unlight this segment's line and report its name"::
    get(LS, line, LineName),
    get(LS, device, N),
    new(Lines, chain(LineName)),
    (   On == @on
    ->  send(N, light_lines, Lines),
        send(LS, report, status, '%s', LineName)
    ;   send(N, unlight_lines, Lines),
        send(LS, report, status, '')
    ),
    send(Lines, free).

:- pce_end_class(line_segment).


:- pce_begin_class(station, figure).

handle(w/2, 0,   link, north).
handle(w/2, h,   link, south).
handle(0,   h/2, link, west).
handle(w,   h/2, link, east).

initialise(S, Name:name) :->
    send_super(S, initialise),
    send(S, name, Name),
    station_display_name(Name, DispName),
    send(S, display, text(DispName, center)),
    send(S, hovered, @off),
    send(S, matched, @off),
    send(S, radius, 10),
    send(S, border, 3).

matches(S, Pattern:name) :->
    "True iff every whitespace-separated word of Pattern occurs in <-name"::
    Pattern \== '',
    get(S, name, Name),
    downcase_atom(Name, NameLow),
    downcase_atom(Pattern, PatLow),
    split_string(PatLow, " \t", " \t", Words0),
    exclude(=(""), Words0, Words),
    Words \= [],
    forall(member(W, Words),
           sub_atom(NameLow, _, _, _, W)).

matched(S, On:bool) :->
    "Search-match marker: thicker red outline"::
    (   On == @on
    ->  send(S, pen, 7), send(S, colour, red)
    ;   send(S, pen, 1), send(S, colour, black)
    ).

hovered(S, On:bool) :->
    "Hover marker: distinct background"::
    hover_background(On, BG),
    send(S, background, BG).

hover_background(@off, colour(@default,  50,205, 50, 128)).
hover_background(@on,  colour(@default, 218,112,214, 128)).

event(S, Ev:event) :->
    "Hover: expose + highlight every line touching this station"::
    (   send(Ev, is_a, area_enter)
    ->  send(S, hover, @on)
    ;   send(Ev, is_a, area_exit)
    ->  send(S, hover, @off)
    ;   send_super(S, event, Ev)
    ).

hover(S, On:bool) :->
    "Light or unlight every line touching S; report its name and lines"::
    get(S, device, N),
    new(Lines, chain),
    send(N?graphicals, for_all,
         if(and(message(@arg1, instance_of, line_segment),
                or(@arg1?from == S, @arg1?to == S)),
            message(Lines, append, @arg1?line))),
    (   On == @on
    ->  send(N, light_lines, Lines),
        chain_list(Lines, LineList),
        sort(LineList, Uniq),
        atomic_list_concat(Uniq, ', ', LineStr),
        send(S, report, status, '%s: %s', S?name, LineStr)
    ;   send(N, unlight_lines, Lines),
        send(S, report, status, '')
    ),
    send(Lines, free).

:- pce_end_class(station).


:- pce_begin_class(live_text_item, text_item,
                   "Text item that fires its message on every keystroke").

typed(TI, Id:'event|event_id') :->
    "Process key, then apply unconditionally"::
    send_super(TI, typed, Id),
    send(TI, apply, @on).

:- pce_end_class(live_text_item).


:- pce_begin_class(line_browser, browser,
                   "Browser listing tube lines; click toggles selection").

variable(network, network, get, "Network the browser drives").

initialise(B, N:network) :->
    send_super(B, initialise),
    send(B, slot, network, N),
    send(B, label, 'Tube lines'),
    send(B, selection_style, new(style)),   % suppress default highlight
    send(B, select_message, message(B, toggle, @arg1)),
    load_tube,
    findall(Name-line(Id, Name, Hex),
            line(Id, Name, Hex, _Stripe), Pairs),
    keysort(Pairs, Sorted),
    forall(member(_-line(Id, Name, Hex), Sorted),
           send(B, add_line, Id, Name, Hex)).

add_line(B, Id:int, Name:name, Hex:name) :->
    "Append a coloured line entry, initially unselected"::
    get(@pce, convert, Hex, colour, BG),
    contrast_colour(BG, FG),
    atom_concat(line_, Id, Style),
    send(B, style, Style, style(colour := FG, background := BG)),
    new(Item, dict_item(Name, @default, @default, Style)),
    send(Item, attribute, selected, @off),
    render_label(Item),
    send(B, append, Item).

toggle(B, Item:dict_item) :->
    "Flip `selected`, re-render label, add/remove the line on the network"::
    get(Item, attribute, selected, Sel0),
    flip_bool(Sel0, Sel),
    send(Item, attribute, selected, Sel),
    render_label(Item),
    send(B, selection, @nil),
    get(Item, key, LineName),
    get(B, network, N),
    (   Sel == @on
    ->  send(N, add_line, LineName)
    ;   send(N, remove_line, LineName)
    ).

select_all(B) :->
    "Select every line"::
    send(B, ensure_all, @on).

clear_all(B) :->
    "Deselect every line"::
    send(B, ensure_all, @off).

ensure_all(B, Wanted:bool) :->
    "Bring every line item to the requested state"::
    get(B, dict, D),
    chain_list(D?members, Items),
    forall(member(Item, Items),
           ensure_state(B, Item, Wanted)).

:- pce_end_class(line_browser).


:- pce_begin_class(tube_dialog, dialog,
                   "Top control dialog: select/clear buttons and search").

fill_dialog(D, LB:line_browser, N:network) :->
    "Populate the dialog with the select/clear buttons and search field"::
    send(D, append,
         new(TI, live_text_item(search, '',
                                message(@prolog, search_callback,
                                        @receiver, N, @arg1)))),
    send(TI, show_label, @off),
    send(TI, placeholder, "Search stations"),
    send(D, append, button(select_all, message(LB, select_all))),
    send(D, append, button(clear_all,  message(LB, clear_all))).

:- pce_end_class(tube_dialog).


%!  ensure_state(+B, +Item, +Wanted) is det.
%
%   Toggle Item via B unless it is already in the Wanted (@on/@off)
%   selection state.

ensure_state(B, Item, Wanted) :-
    get(Item, attribute, selected, Current),
    (   Current == Wanted
    ->  true
    ;   send(B, toggle, Item)
    ).

%!  render_label(+Item) is det.
%
%   Rebuild Item's `->label` from its `<-key` and the `selected`
%   attribute, prefixing the line name with a check or cross glyph.

render_label(Item) :-
    get(Item, attribute, selected, Sel),
    get(Item, key, LineName),
    selection_glyph(Sel, Mark),
    format(string(Label), '~w ~w', [Mark, LineName]),
    send(Item, label, Label).

selection_glyph(@on,  '✅').
selection_glyph(@off, '❌').

flip_bool(@on,  @off).
flip_bool(@off, @on).

                /*******************************
                *           COLOURS            *
                *******************************/

%!  contrast_colour(+Colour, -FG) is det.
%
%   FG is `white` if Colour is dark and `black` otherwise, decided
%   from xpce's perceptual `<-intensity` (0..255).

contrast_colour(Colour, FG) :-
    get(Colour, intensity, I),
    (   I < 128
    ->  FG = white
    ;   FG = black
    ).


                /*******************************
                *         PROJECTION           *
                *******************************/

%!  station_position(+Name, +SW, +SH, +W, +H, -X, -Y) is det.
%
%   X,Y is the top-left pixel position for a station of size SW×SH on
%   a map of size W×H, computed from the station's geographic
%   coordinates linearly stretched to fit the map.

station_position(Name, SW,SH, W,H, X,Y) :-
    range(lat, LatMin, LatMax),
    range(lon, LonMin, LonMax),
    station(_Id, Name, _DispName, _Zone, _Lines, _Rail, Lat, Lon),
    X is (Lon-LonMin) * (W-SW) / (LonMax-LonMin),
    Y is (Lat-LatMin) * (H-SH) / (LatMax-LatMin).

:- table range/3.

range(lat, Low, High) :-
    aggregate_all(min(Lat)+max(Lat),
                  station(_Id, _Name, _DispName, _Zone, _Lines, _Rail, Lat, _Lon),
                  Low+High).
range(lon, Low, High) :-
    aggregate_all(min(Lon)+max(Lon),
                  station(_Id, _Name, _DispName, _Zone, _Lines, _Rail, _Lat, Lon),
                  Low+High).

