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

:- module(pce_symbol_picker,
          [ symbol_picker/0,
            symbol_picker/1,            % +Client
            pick_symbol/1               % -Code
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- autoload(library(unicode/blocks), [unicode_block/3]).
:- autoload(library(apply), [include/3]).
:- autoload(library(lists), [append/3, member/2, delete/3, reverse/2, nth0/3]).
:- autoload(library(pce_util), [chain_list/2]).
:- autoload(library(readutil), [read_file_to_terms/3]).
:- autoload(library(solution_sequences), [distinct/2]).

:- multifile code_range/3.              % Name, Ranges, Sample

:- pce_autoload(font_item, library(pce_font_item)).

/** <module> Pick a Unicode symbol

This library defines a graphical picker for Unicode symbols. The
picker shows a row of recently picked symbols, a list browser to
select a named code range and a grid of all printable symbols in
the selected range.

Two entry points are provided:

  * symbol_picker/0 and symbol_picker/1 open or expose the non-modal
    singleton window (@symbol_picker).  Clicking a symbol types it
    into the application window that has the keyboard focus (falling
    back to the clipboard if there is no such window) and remembers
    it in the recents list (which is persisted across sessions).
    symbol_picker/1 takes a _Client_ object (e.g. an editor) that
    should initially receive the typed symbols.

  * pick_symbol/1 opens a modal window. It succeeds with the code
    of the picked symbol or fails when the user closes the window
    without picking.

Applications can extend the set of named ranges by adding clauses
to the multifile predicate code_range/3.  User defined ranges appear
before the built-in Unicode blocks and override any block with the
same name:

  ==
  :- multifile pce_symbol_picker:code_range/3.

  pce_symbol_picker:code_range('My math', [0x2200-0x22FF], _).
  pce_symbol_picker:code_range('Brackets',
                               [ 0x28/0x29,            % ( )
                                 0x5B/0x5D,            % [ ]
                                 0x2018/0x2019         % ‘ ’
                               ], _).
  ==

The default set of ranges is the Unicode blocks from
library(unicode/blocks).
*/

%!  code_range(?Name, ?Members, ?Sample) is nondet.
%
%   Multifile hook defining a named set of symbols shown in the
%   picker.  _Members_ is a list whose elements are:
%
%     * an integer character code
%     * From-To, an inclusive range of code points
%     * Open/Close, a matching pair (e.g. brackets/quotes).  The two
%       characters are shown adjacently; picking emits _Open_,
%       _Close_ and a backward-character (^B), leaving the caret
%       between them.
%
%   _Sample_ is the list of code points (or text) shown next to the
%   name in the browser.  Leave it unbound to derive a sample
%   automatically.

:- pce_global(@symbol_picker, new(symbol_picker)).

%!  symbol_picker is det.
%!  symbol_picker(+Client) is det.
%
%   Open or expose the non-modal singleton symbol picker. Picking a
%   symbol types it into the window that has keyboard focus and adds
%   it to the recents list. _Client_, when given, is the object (e.g.
%   an editor) that should initially receive the typed symbols.

symbol_picker :-
    send(@symbol_picker, show).

symbol_picker(Client) :-
    send(@symbol_picker, show, Client).

%!  pick_symbol(-Code) is semidet.
%
%   Open a modal symbol picker. Succeeds with the code of the picked
%   symbol or fails if the user closes the picker.

pick_symbol(Code) :-
    new(SP, symbol_picker),
    (   get(SP, pick, Code0)
    ->  send(SP, destroy),
        Code = Code0
    ;   send(SP, destroy),
        fail
    ).


		 /*******************************
		 *           RECENTS            *
		 *******************************/

%!  recent(?Action) is nondet.
%
%   Recently picked actions (emit(Code) or pair(Open,Close)), in
%   most-recent-first order. Persisted
%   to the xpce per-user application data directory using an
%   append-only log so that multiple sessions can contribute. The
%   log is compacted on load when it grows too large compared to the
%   set of distinct recents.

:- dynamic recent/1.
:- dynamic last_range/1.
:- dynamic recents_loaded/0.
:- volatile recents_loaded/0.

max_recents(48).
%   File is compacted on load when it has more than this many entries
%   and more than twice as many entries as distinct recents.
recents_compact_threshold(200).

%!  add_recent(+Action) is det.
%
%   Action is emit(Code) or pair(Open,Close).  Kept as a unit so a
%   recent pair re-emits the pair (not its two characters separately).

add_recent(Action0) :-
    (   norm_action(Action0, Action)
    ->  true
    ;   Action = Action0
    ),
    retractall(recent(Action)),
    asserta(recent(Action)),
    cap_recents,
    append_recent(Action).

cap_recents :-
    max_recents(Max),
    findall(C, recent(C), Codes),
    length(Codes, Len),
    (   Len > Max
    ->  length(Keep, Max),
        append(Keep, _Drop, Codes),
        retractall(recent(_)),
        forall(member(C, Keep), assertz(recent(C)))
    ;   true
    ).

recents_file(File) :-
    get(@pce, application_data, AppDir),
    (   send(AppDir, exists)
    ->  true
    ;   send(AppDir, make)
    ),
    get(AppDir, path, Dir),
    atomic_list_concat([Dir, '/symbol-recents.pl'], File).

load_recents :-
    recents_loaded,
    !.
load_recents :-
    assertz(recents_loaded),
    catch(do_load_recents, E,
          print_message(warning, E)).

do_load_recents :-
    recents_file(File),
    exists_file(File),
    !,
    read_file_to_terms(File, Terms, [encoding(utf8)]),
    apply_terms(Terms, state([], _), state(Recents0, LR)),
    max_recents(Max),
    truncate_list(Recents0, Max, Recents),
    forall(member(C, Recents), assertz(recent(C))),
    (   ground(LR)
    ->  retractall(last_range(_)),
        assertz(last_range(LR))
    ;   true
    ),
    maybe_compact(File, Terms, Recents, LR).
do_load_recents.

apply_terms([], S, S).
apply_terms([recent(C0)|Ts], state(R0, LR0), Final) :-
    norm_action(C0, A), !,
    delete(R0, A, R1),
    apply_terms(Ts, state([A|R1], LR0), Final).
apply_terms([last_range(N)|Ts], state(R, _), Final) :-
    atom(N), !,
    apply_terms(Ts, state(R, N), Final).
apply_terms([_|Ts], S, Final) :-
    apply_terms(Ts, S, Final).

%!  norm_action(+Stored, -Action) is semidet.
%
%   Normalise a stored recent: an action term, or a bare integer
%   (legacy on-disk format) treated as emit/1.

norm_action(emit(C),     emit(C))     :- integer(C), !.
norm_action(pair(O,Cl),  pair(O,Cl))  :- integer(O), integer(Cl), !.
norm_action(C,           emit(C))     :- integer(C), !.

truncate_list(L, Max, T) :-
    length(L, N),
    (   N =< Max
    ->  T = L
    ;   length(T, Max),
        append(T, _, L)
    ).

append_recent(Action) :-
    catch(do_append_recent(Action), E,
          print_message(warning, E)).

do_append_recent(Action) :-
    recents_file(File),
    setup_call_cleanup(
        open(File, append, Out, [encoding(utf8)]),
        write_recent_term(Out, Action),
        close(Out)).

write_recent_term(Out, emit(C)) :-
    format(Out, 'recent(emit(0x~|~`0t~16r~4+)).~n', [C]).
write_recent_term(Out, pair(O,Cl)) :-
    format(Out, 'recent(pair(0x~|~`0t~16r~4+,0x~|~`0t~16r~4+)).~n',
           [O, Cl]).

append_last_range(Name) :-
    catch(do_append_last_range(Name), E,
          print_message(warning, E)).

do_append_last_range(Name) :-
    recents_file(File),
    setup_call_cleanup(
        open(File, append, Out, [encoding(utf8)]),
        format(Out, 'last_range(~q).~n', [Name]),
        close(Out)).

maybe_compact(File, Terms, Recents, LastRange) :-
    length(Terms, NT),
    length(Recents, NR),
    recents_compact_threshold(Th),
    (   NT > Th,
        NT > 2 * NR
    ->  catch(rewrite_recents_file(File, Recents, LastRange), E,
              print_message(warning, E))
    ;   true
    ).

%!  rewrite_recents_file(+File, +Recents, +LastRange) is det.
%
%   Replace File with a compact log: oldest first, newest last, so a
%   subsequent append-driven load reconstructs the same order.
%   LastRange may be unbound, in which case no last_range entry is
%   written.

rewrite_recents_file(File, Recents, LastRange) :-
    reverse(Recents, Chrono),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        ( format(Out, '% Symbol picker recents.  Auto-generated.~n', []),
          (   ground(LastRange)
          ->  format(Out, 'last_range(~q).~n', [LastRange])
          ;   true
          ),
          forall(member(A, Chrono),
                 write_recent_term(Out, A))
        ),
        close(Out)).


                /*******************************
                *        DEFAULT RANGES        *
                *******************************/

builtin_code_range('Parenthesis', Ranges, `()⌈⌉⟪⟫`) :-
    findall(Open/Close, code_type(Open, paren(Close)), Ranges).
builtin_code_range('Quotes',      Ranges, '""«»⸉⸊') :-
    findall(Open/Close, code_type(Open, quote(Close)), Ranges).


		 /*******************************
		 *            RANGES            *
		 *******************************/

%!  range_def(?Name, -Members, -Sample) is nondet.
%
%   Raw definition of a named range: the user defined code_range/3
%   clauses followed by the Unicode blocks from library(unicode/blocks)
%   (as a single From-To member, surrogates suppressed).  A user
%   definition hides the block with the same name.

range_def(Name, Members, Sample) :-
    distinct(Name, range_def_(Name, Members, Sample)).


range_def_(Name, Members, Sample) :-
    code_range(Name, Members, Sample).
range_def_(Name, Members, Sample) :-
    builtin_code_range(Name, Members, Sample).
range_def_(Name, [From-To], _) :-
    unicode_block(Name, From, To),
    \+ surrogate_block(From).

surrogate_block(From) :-
    From >= 0xD800,
    From =< 0xDFFF.

%!  range_cells(+Name, -Cells) is semidet.
%
%   Expand a range's Members into an ordered list of cells, each
%   emit(Code) or pair(Open,Close).

range_cells(Name, Cells) :-
    range_def(Name, Members, _),
    members_cells(Members, Cells, []).

members_cells([], T, T).
members_cells([M|Ms], Cells, T) :-
    member_cells(M, Cells, T1),
    members_cells(Ms, T1, T).

member_cells(Code, [emit(Code)|T], T) :-
    integer(Code), !.
member_cells(Open/Close, [pair(Open,Close)|T], T) :-
    integer(Open), integer(Close), !.
member_cells(From-To, Cells, T) :-
    integer(From), integer(To), !,
    emit_range(From, To, Cells, T).

emit_range(From, To, T, T) :-
    From > To, !.
emit_range(From, To, [emit(From)|Cs], T) :-
    F1 is From+1,
    emit_range(F1, To, Cs, T).

%!  range_sample(+Name, +Font, -Codes) is det.
%
%   Up to four representative code points for the browser label.
%   Uses an explicit Sample when given, else scans the members
%   (bounded, so huge blocks stay cheap).

range_sample(Name, Font, Codes) :-
    range_def(Name, Members, Sample),
    (   is_list(Sample)
    ->  include(ok_sample(Font), Sample, Codes)
    ;   members_sample(Members, Font, 4, Codes)
    ).

members_sample(_, _, 0, []) :- !.
members_sample([], _, _, []) :- !.
members_sample([M|Ms], Font, N, Codes) :-
    item_sample(M, Font, N, Got),
    length(Got, G),
    append(Got, Rest, Codes),
    N1 is N - G,
    (   N1 =< 0
    ->  Rest = []
    ;   members_sample(Ms, Font, N1, Rest)
    ).

item_sample(Code, Font, _, Got) :-
    integer(Code), !,
    (   ok_sample(Font, Code)
    ->  Got = [Code]
    ;   Got = []
    ).
item_sample(Open/Close, Font, _, Got) :-
    integer(Open), !,
    include(ok_sample(Font), [Open,Close], Got).
item_sample(From-To, Font, N, Got) :-
    integer(From), !,
    scan_sample(From, To, Font, N, 256, Got).

ok_sample(Font, C) :-
    printable_char(C),
    send(Font, member, C).

scan_sample(From, To, _, _, _, []) :- From > To, !.
scan_sample(_, _, _, 0, _, []) :- !.
scan_sample(_, _, _, _, 0, []) :- !.
scan_sample(From, To, Font, N, B, Out) :-
    (   ok_sample(Font, From)
    ->  Out = [From|O],
        N1 is N-1
    ;   Out = O, N1 = N
    ),
    F1 is From+1,
    B1 is B-1,
    scan_sample(F1, To, Font, N1, B1, O).

take_at_most(0, _, []) :- !.
take_at_most(_, [], []) :- !.
take_at_most(N, [X|Xs], [X|Ys]) :-
    N1 is N-1,
    take_at_most(N1, Xs, Ys).

initial_range_name(Name) :-
    last_range(Name),
    range_def(Name, _, _),
    !.
initial_range_name(Name) :-
    once(range_def(Name, _, _)).

%!  focus_widget_font(+Graphical, -Font) is semidet.
%
%   Font of the focused widget: <-font (editor/view) or <-value_font
%   (text_item).  Fails if the widget has no usable font.

focus_widget_font(Gr, Font) :-
    (   send(Gr, has_get_method, value_font)
    ->  get(Gr, value_font, F)
    ;   send(Gr, has_get_method, font)
    ->  get(Gr, font, F)
    ),
    F \== @nil,
    Font = F.


		 /*******************************
		 *            FRAME             *
		 *******************************/

:- pce_begin_class(symbol_picker, frame,
                   "Pick a Unicode symbol from a code range").

variable(pick_mode,
         {type,clipboard,return} := type, get,
         "What to do when a symbol is clicked").
variable(target_frame, frame*, both,
         "Frame that receives typed symbols").
variable(saved_focus_message, code*, both,
         "Previous @display_manager focus_message, restored on close").
variable(symbol_font, font, both,
         "Font used to display symbols").
variable(range_name, name*, get,
         "Name of the selected range").

class_variable(symbol_font, font, font(sans, normal, 16)).

initialise(SP) :->
    load_recents,
    send_super(SP, initialise, 'Symbol picker'),
    send(SP, done_message, message(SP, destroy)),

    new(FilterMsg, message(SP, filter)),
    new(RangeMsg,  message(SP, range_selected, @arg1?key)),
    new(FontMsg,   message(SP, choose_font)),
    new(CloseMsg,  message(SP, destroy)),
    new(ClearMsg,  message(SP, clear_recents)),

    send(SP, append, new(D, dialog)),
    send(D, name, controls),
    send(D, gap, size(8,4)),
    send(D, append, new(Filter, sp_live_text_item(filter, '', FilterMsg))),
    send(Filter, length, 24),
    send(Filter, placeholder, "Filter code blocks"),
    send(D, append, button(font, FontMsg), right),
    send(D, append, button(clear_recents, ClearMsg), right),
    send(D, append, button(close, CloseMsg), right),

    send(new(R, picture(recents, size(450, 60))), below, D),
    send(R, name, recents),
    send(R, ver_shrink, 0),
    send(R, ver_stretch, 0),
    send(R, scrollbars, none),

    new(B, browser('Code range', size(28, 10))),
    send(B, name, ranges),
    send(B, select_message, RangeMsg),
    send(B, open_message,   RangeMsg),
    get(SP, symbol_font, Font),
    get(Font, width, 'WWWWW', Tab),
    send(B, tab_stops, vector(Tab)),

    format(string(Row), '~20c', [0'W]),
    get(Font, width, Row, GridW),
    send(new(G, picture(grid, size(GridW, 100))), right, B),
    send(G, name, grid),
    send(G, scrollbars, both),

    send(G, below, R),
    send(new(report_dialog), below, G),

    send(SP, fill_ranges, ''),
    send(SP, update_recents),
    (   initial_range_name(Init)
    ->  send(SP, select_range, Init)
    ;   send(SP, report, warning, 'No code ranges available')
    ),
    send(SP, capture_target),
    get(@display_manager, focus_message, Old),
    send(SP, slot, saved_focus_message, Old),
    send(@display_manager, focus_message,
         message(SP, on_focus, @arg1)).

unlink(SP) :->
    "Restore the previous focus_message and clear the reference"::
    get(SP, saved_focus_message, Old),
    send(@display_manager, focus_message, Old),
    send_super(SP, unlink).

show(SP, Client:[object]*) :->
    "Open or expose the picker; Client initially receives symbols"::
    (   (   Client == @default
        ;   Client == @nil
        )
    ->  true
    ;   send(SP, client, Client)
    ),
    (   get(SP, status, unmapped)
    ->  send(SP, open)
    ;   send(SP, expose)
    ).

client(SP, Client:object) :->
    "Set the frame that receives typed symbols"::
    (   send(Client, instance_of, frame)
    ->  Fr = Client
    ;   send(Client, has_get_method, frame)
    ->  get(Client, frame, Fr)
    ;   Fr = @nil
    ),
    (   Fr == @nil
    ->  true
    ;   send(SP, slot, target_frame, Fr)
    ).

pick(SP, Code:int) :<-
    "Modally pick a symbol; fails when cancelled"::
    send(SP, slot, pick_mode, return),
    get(SP, confirm_centered, Reply),
    Reply \== @nil,
    Code = Reply.

on_focus(SP, Fr:frame) :->
    "Track the application frame that just gained keyboard focus"::
    (   object(Fr),
        Fr \== SP,
        \+ send(Fr, instance_of, symbol_picker)
    ->  send(SP, slot, target_frame, Fr),
        send(SP, adopt_target_font)
    ;   true
    ).

adopt_target_font(SP) :->
    "Adopt the focused widget's font family, keeping our style/size"::
    (   get(SP, target_frame, Fr), Fr \== @nil, object(Fr),
        catch(get(Fr, keyboard_focus, Win), _, fail), Win \== @nil,
        catch(get(Win, keyboard_focus, Gr), _, fail), Gr \== @nil,
        focus_widget_font(Gr, TFont),
        get(TFont, family, Fam),
        get(SP, symbol_font, Cur),
        get(Cur, family, CurFam),
        Fam \== CurFam
    ->  get(Cur, style, Style),
        get(Cur, points, Points),
        Pt is integer(Points),
        send(SP, font, font(Fam, Style, Pt))
    ;   true
    ).

capture_target(SP) :->
    "Remember the application frame that currently has keyboard focus"::
    get(@display, frames, Frames),
    chain_list(Frames, List),
    (   member(Fr, List),
        Fr \== SP,
        get(Fr, input_focus, @on)
    ->  send(SP, slot, target_frame, Fr),
        send(SP, adopt_target_font)
    ;   true
    ).


		 /*******************************
		 *        RANGE BROWSER         *
		 *******************************/

fill_ranges(SP, Filter:name) :->
    "Populate the list browser; show only ranges matching Filter"::
    get(SP, member, ranges, LB),
    (   get(LB, selection, OldSel),
        OldSel \== @nil
    ->  get(OldSel, key, OldName)
    ;   OldName = @nil
    ),
    send(LB, clear),
    get(SP, symbol_font, Font),
    forall(( matching_range(Filter, Name),
             range_label(Name, Font, Label)
           ),
           send(LB, append, dict_item(Name, Label))),
    (   OldName \== @nil,
        get(LB?dict, member, OldName, _)
    ->  send(LB, selection, OldName)
    ;   true
    ).

matching_range(Filter, Name) :-
    range_def(Name, _, _),
    matches(Filter, Name).

%!  range_label(+Name, +Font, -Label) is det.
%
%   "samples\tName": up to four representative characters followed by
%   the range name.

range_label(Name, Font, Label) :-
    range_sample(Name, Font, Codes),
    Codes \== [],
    format(string(Label), '~s\t~w', [Codes, Name]).

matches('', _) :- !.
matches(Filter, Name) :-
    sub_atom_icasechk(Name, _, Filter).

filter(SP) :->
    "Apply the current filter expression"::
    get(SP, member, controls, D),
    get(D, member, filter, Item),
    get(Item, selection, Text),
    send(SP, fill_ranges, Text),
    get(SP, member, ranges, LB),
    get(LB?dict?members, size, N),
    send(SP, report, status, '%d matches', N).

range_selected(SP, Name:name) :->
    "User clicked a range name"::
    send(SP, select_range, Name).

select_range(SP, Name:name) :->
    "Display the named range in the grid"::
    (   range_cells(Name, Cells)
    ->  send(SP, slot, range_name, Name),
        get(SP, member, ranges, LB),
        (   get(LB?dict, member, Name, _)
        ->  send(LB, selection, Name)
        ;   true
        ),
        send(SP, fill_grid),
        update_last_range(Name),
        length(Cells, Count),
        send(SP, report, status, '%s  (%d items)', Name, Count)
    ;   send(SP, report, warning, 'Unknown range: %s', Name)
    ).

%   Remember the named range as the most-recent selection.  Only writes
%   when the value actually changes to avoid spurious file growth.
update_last_range(Name) :-
    (   last_range(Name)
    ->  true
    ;   retractall(last_range(_)),
        assertz(last_range(Name)),
        append_last_range(Name)
    ).


		 /*******************************
		 *           SYMBOLS            *
		 *******************************/

fill_grid(SP) :->
    "Re-draw the symbols of the current range"::
    get(SP, member, grid, G),
    send(G, clear),
    new(Fmt, format(horizontal, 1, @on)),
    send(Fmt, column_sep, 6),
    send(Fmt, row_sep, 2),
    send(G, format, Fmt),
    get(SP, range_name, Name), Name \== @nil,
    get(SP, symbol_font, Font),
    range_cells(Name, Cells0),
    renderable_cells(Cells0, Font, Cells),
    fill_grid_rows(G, Font, Cells).

%   Drop emit cells the font cannot show; keep curated pairs as-is.

renderable_cells([], _, []).
renderable_cells([emit(C)|T], Font, Out) :-
    !,
    (   printable_char(C),
        send(Font, member, C)
    ->  Out = [emit(C)|O]
    ;   Out = O
    ),
    renderable_cells(T, Font, O).
renderable_cells([pair(O,C)|T], Font, [pair(O,C)|Os]) :-
    !,
    renderable_cells(T, Font, Os).
renderable_cells([_|T], Font, Out) :-
    renderable_cells(T, Font, Out).

fill_grid_rows(_, _, []) :- !.
fill_grid_rows(G, Font, Cells) :-
    take_at_most(16, Cells, Row),
    length(Row, Len),
    Len > 0,
    !,
    row_visual(Row, S, Actions),
    new(Cell, picker_cell(S, Font)),
    send(Cell, slot, actions, Actions),
    send(G, display, Cell),
    list_drop(Len, Cells, Rest),
    fill_grid_rows(G, Font, Rest).
fill_grid_rows(_, _, _).

list_drop(0, L, L) :- !.
list_drop(_, [], []) :- !.
list_drop(N, [_|T], R) :-
    N1 is N-1,
    list_drop(N1, T, R).

%   Build the row's display string and a per-display-character action
%   list (a pair occupies two display characters, both mapping to the
%   same pair action).

row_visual(Cells, S, Actions) :-
    new(S, string),
    row_visual_(Cells, S, Actions).

row_visual_([], _, []).
row_visual_([emit(C)|T], S, [emit(C)|A]) :-
    send(S, append, string('%c', C)),
    row_visual_(T, S, A).
row_visual_([pair(O,C)|T], S, [pair(O,C),pair(O,C)|A]) :-
    send(S, append, string('%c', O)),
    send(S, append, string('%c', C)),
    row_visual_(T, S, A).
row_visual_([_|T], S, A) :-             % skip anything unexpected
    row_visual_(T, S, A).

printable_char(C) :-
    code_type(C, width(W)),
    W > 0.


		 /*******************************
		 *           RECENTS            *
		 *******************************/

update_recents(SP) :->
    "Re-draw the row of recently picked symbols"::
    get(SP, member, recents, R),
    send(R, clear),
    new(Fmt, format(horizontal, 1, @on)),
    send(Fmt, row_sep, 2),
    send(R, format, Fmt),
    get(SP, symbol_font, Font),
    findall(A, ( recent(A0), norm_action(A0, A) ), Actions),
    (   Actions == []
    ->  send(R, display,
             text('(no recently picked symbols)',
                  left, font(helvetica, oblique, 11)))
    ;   row_visual(Actions, S, Map),
        new(Cell, picker_cell(S, Font)),
        send(Cell, slot, actions, Map),
        send(R, display, Cell)
    ).

clear_recents(SP) :->
    "Forget the recents list"::
    retractall(recent(_)),
    catch(( recents_file(File),
            ( last_range(LR) -> true ; LR = _ ),
            rewrite_recents_file(File, [], LR)
          ), _, true),
    send(SP, update_recents),
    send(SP, report, status, 'Cleared recents').


		 /*******************************
		 *             FONT             *
		 *******************************/

choose_font(SP) :->
    "Let the user pick a new display font"::
    get(SP, symbol_font, Old),
    new(Dlg, dialog('Symbol picker font')),
    send(Dlg, append, new(FI, font_item(font, Old))),
    send(Dlg, append, button(ok,
                             message(Dlg, return, FI?selection))),
    send(Dlg, append, button(cancel,
                             message(Dlg, return, @nil))),
    send(Dlg, default_button, ok),
    (   get(Dlg, confirm_centered, SP?area?center, Reply),
        Reply \== @nil
    ->  send(SP, font, Reply)
    ;   true
    ),
    send(Dlg, destroy).

font(SP, Font:font) :->
    "Change the display font and refresh"::
    send(SP, slot, symbol_font, Font),
    send(SP, fill_grid),
    send(SP, update_recents).


		 /*******************************
		 *            PICK              *
		 *******************************/

pick_action(SP, Action:prolog) :->
    "Dispatch a cell action chosen in the grid or recents"::
    (   Action = emit(Code)
    ->  send(SP, pick_code, Code)
    ;   Action = pair(Open, Close)
    ->  send(SP, pick_pair, Open, Close)
    ;   true
    ).

pick_pair(SP, Open:int, Close:int) :->
    "Pick a matching pair: emit Open, Close and a backward-character"::
    get(SP, pick_mode, Mode),
    (   Mode == return
    ->  send(SP, return, Open)
    ;   Mode == type,
        send(SP, type_symbol, Open),
        send(SP, type_symbol, Close),
        send(SP, type_ctrl, 'B')        % ^B: leave caret between
    ->  add_recent(pair(Open,Close)),
        send(SP, update_recents),
        send(SP, report, status, 'Typed %c%c', Open, Close)
    ;   atom_codes(A, [Open,Close]),
        send(@display, copy, A),
        add_recent(pair(Open,Close)),
        send(SP, update_recents),
        send(SP, report, status, 'Copied %s', A)
    ).

pick_code(SP, Code:int) :->
    "Called by picker_cell when the user clicks a symbol"::
    get(SP, pick_mode, Mode),
    (   Mode == return
    ->  send(SP, return, Code)
    ;   Mode == type,
        send(SP, type_symbol, Code)
    ->  add_recent(emit(Code)),
        send(SP, update_recents),
        send(SP, report, status, 'Typed %c  (U+%04X)', Code, Code)
    ;   atom_codes(A, [Code]),
        send(@display, copy, A),
        add_recent(emit(Code)),
        send(SP, update_recents),
        (   Mode == type
        ->  send(SP, report, status,
                 'No target window; copied %c  (U+%04X)', Code, Code)
        ;   send(SP, report, status, 'Copied %s  (U+%04X)', A, Code)
        )
    ).

type_symbol(SP, Code:int) :->
    "Post a keyboard event for Code to the target frame"::
    get(SP, target_frame, Fr),
    Fr \== @nil,
    object(Fr),
    Fr \== SP,
    new(Ev, event(Code, Fr)),
    send(Fr, post_event, Ev).

type_ctrl(SP, Char:name) :->
    "Post a control-modified key (e.g. ^B) to the target frame"::
    get(SP, target_frame, Fr),
    Fr \== @nil,
    object(Fr),
    Fr \== SP,
    ctrl(Char, Ctrl),
    new(Ev, event(Ctrl, Fr, button_mask := 1)), % 1 = BUTTON_control
    send(Fr, post_event, Ev).

ctrl(Char, Ctrl) :-
    char_code(Char, Code),
    Ctrl is Code - 0'@.

:- pce_end_class(symbol_picker).


		 /*******************************
		 *         PICKER CELL          *
		 *******************************/

:- pce_begin_class(picker_cell, text,
                   "Clickable row of symbols").

variable(actions, prolog, get,
         "Action per display-character position").

initialise(C, S:string, Font:font) :->
    send_super(C, initialise, S, left, Font),
    send(C, slot, actions, []).

event(C, Ev:event) :->
    (   send_super(C, event, Ev)
    ->  true
    ;   send(Ev, is_a, area_exit)
    ->  send(C, report, status, '')
    ;   get(C, pointed, Ev, @off, Index),
        Index >= 0,
        get(C, actions, Actions),
        nth0(Index, Actions, Action)
    ->  (   send(Ev, is_a, ms_left_up)
        ->  send(C?frame, pick_action, Action)
        ;   send(C?frame, capture_target),
            action_status(Action, Status),
            send(C, report, status, Status)
        )
    ;   true
    ).

action_status(emit(Code), Status) :-
    new(Status, string('%c  U+%04X / %d', Code, Code, Code)).
action_status(pair(Open,Close), Status) :-
    new(Status, string('%c%c  pair', Open, Close)).

:- pce_end_class(picker_cell).


		 /*******************************
		 *      LIVE FILTER ITEM        *
		 *******************************/

:- pce_begin_class(sp_live_text_item, text_item,
                   "text_item that fires its message on each keystroke").

typed(TI, Ev:'event|event_id') :->
    "Forward typed event, then fire <-message with new selection"::
    send_super(TI, typed, Ev),
    get(TI, message, M),
    (   M \== @nil, M \== @default
    ->  get(TI, selection, V),
        ignore(send(M, forward, V))
    ;   true
    ).

:- pce_end_class(sp_live_text_item).
