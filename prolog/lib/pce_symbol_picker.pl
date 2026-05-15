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
            pick_symbol/1,              % -Code
            code_range/3                % ?Name, ?From, ?To
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_report)).
:- use_module(library('unicode/blocks')).
:- use_module(library(lists)).

:- pce_autoload(font_item, library(pce_font_item)).

/** <module> Pick a Unicode symbol

This library defines a graphical picker for Unicode symbols. The
picker shows a row of recently picked symbols, a list browser to
select a named code range and a grid of all printable symbols in
the selected range.

Two entry points are provided:

  * symbol_picker/0 opens a non-modal singleton window. Clicking
    a symbol types it into the application window that has the
    keyboard focus (falling back to the clipboard if there is no
    such window) and remembers it in the recents list (which is
    persisted across sessions).

  * pick_symbol/1 opens a modal window. It succeeds with the code
    of the picked symbol or fails when the user closes the window
    without picking.

Applications can extend the set of named ranges by adding clauses
to the multifile predicate code_range/3.  User defined ranges appear
before the built-in Unicode blocks and override any block with the
same name:

  ==
  :- multifile pce_symbol_picker:code_range/3.

  pce_symbol_picker:code_range('My math', 0x2200, 0x22FF).
  ==

The default set of ranges is the Unicode blocks from
library(unicode/blocks).
*/

%!  code_range(?Name, ?From, ?To) is nondet.
%
%   Multifile hook to define named code  ranges shown in the picker's
%   list browser. _From_ and _To_ are inclusive code points.

:- multifile code_range/3.

%!  symbol_picker is det.
%
%   Open a non-modal singleton symbol picker. Picking a symbol types
%   it into the focused application window and adds it to the recents
%   list.

symbol_picker :-
    load_recents,
    (   object(@symbol_picker)
    ->  send(@symbol_picker, expose)
    ;   send(new(@symbol_picker, symbol_picker), open)
    ).

%!  pick_symbol(-Code) is semidet.
%
%   Open a modal symbol picker. Succeeds with the code of the picked
%   symbol or fails if the user closes the picker.

pick_symbol(Code) :-
    load_recents,
    new(SP, symbol_picker),
    send(SP, slot, pick_mode, return),
    (   get(SP, confirm_centered, Reply),
        Reply \== @nil
    ->  Code = Reply,
        send(SP, destroy)
    ;   send(SP, destroy),
        fail
    ).


		 /*******************************
		 *           RECENTS            *
		 *******************************/

%!  recent(?Code) is nondet.
%
%   Recently picked symbols, in most-recent-first order. Persisted
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

add_recent(Code) :-
    retractall(recent(Code)),
    asserta(recent(Code)),
    cap_recents,
    append_recent(Code).

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
    read_recents_terms(File, Terms),
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

read_recents_terms(File, Terms) :-
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        read_all_terms(In, Terms),
        close(In)).

read_all_terms(In, Terms) :-
    read_term(In, T, []),
    (   T == end_of_file
    ->  Terms = []
    ;   Terms = [T|Rest],
        read_all_terms(In, Rest)
    ).

apply_terms([], S, S).
apply_terms([recent(C)|Ts], state(R0, LR0), Final) :-
    integer(C), !,
    delete(R0, C, R1),
    apply_terms(Ts, state([C|R1], LR0), Final).
apply_terms([last_range(N)|Ts], state(R, _), Final) :-
    atom(N), !,
    apply_terms(Ts, state(R, N), Final).
apply_terms([_|Ts], S, Final) :-
    apply_terms(Ts, S, Final).

truncate_list(L, Max, T) :-
    length(L, N),
    (   N =< Max
    ->  T = L
    ;   length(T, Max),
        append(T, _, L)
    ).

append_recent(Code) :-
    catch(do_append_recent(Code), E,
          print_message(warning, E)).

do_append_recent(Code) :-
    recents_file(File),
    setup_call_cleanup(
        open(File, append, Out, [encoding(utf8)]),
        format(Out, 'recent(0x~|~`0t~16r~4+).~n', [Code]),
        close(Out)).

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
          forall(member(C, Chrono),
                 format(Out, 'recent(0x~|~`0t~16r~4+).~n', [C]))
        ),
        close(Out)).


		 /*******************************
		 *            RANGES            *
		 *******************************/

%!  effective_range(?Name, ?From, ?To) is nondet.
%
%   All code ranges shown in the list browser: first the user defined
%   ranges (code_range/3) followed by the Unicode blocks from
%   library(unicode/blocks), with surrogate blocks suppressed. Names
%   already defined by the user are not added again from the defaults.

effective_range(Name, From, To) :-
    code_range(Name, From, To).
effective_range(Name, From, To) :-
    unicode_block(Name, From, To),
    \+ surrogate_block(From),
    \+ code_range(Name, _, _).

surrogate_block(From) :-
    From >= 0xD800,
    From =< 0xDFFF.

initial_range_name(Name) :-
    last_range(Name),
    effective_range(Name, _, _),
    !.
initial_range_name(Name) :-
    once(effective_range(Name, _, _)).

%!  focus_widget_font(+Graphical, -Font) is semidet.
%
%   Font of the focused widget: <-font (editor/view) or <-value_font
%   (text_item).  Fails if the widget has no usable font.

focus_widget_font(Gr, Font) :-
    (   send(Gr, has_get_method, font)
    ->  get(Gr, font, F)
    ;   send(Gr, has_get_method, value_font)
    ->  get(Gr, value_font, F)
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
variable(range_from, int*, get,
         "First code of selected range").
variable(range_to,   int*, get,
         "Last code of selected range").

class_variable(symbol_font, font, font(sans, normal, 16)).

initialise(SP) :->
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
    forall(( matching_range(Filter, Name, From, To),
             range_label(Name, From, To, Font, Label)
           ),
           send(LB, append, dict_item(Name, Label))),
    (   OldName \== @nil,
        get(LB?dict, member, OldName, _)
    ->  send(LB, selection, OldName)
    ;   true
    ).

matching_range(Filter, Name, From, To) :-
    effective_range(Name, From, To),
    matches(Filter, Name).

%!  range_label(+Name, +From, +To, +Font, Label) is semidet.
%
%   Build "Name  abcd" where abcd are up to 4 representative printable
%   characters from the range, rendered with the list browser's font.

range_label(Name, From, To, Font, Label) :-
    range_samples(From, To, Font, 4, Samples),
    length(Samples, 4),
    format(string(Label), '~s\t~w', [Samples, Name]).

range_samples(From, To, _, _, []) :-
    From > To, !.
range_samples(_, _, _, 0, []) :- !.
range_samples(From, To, Font, N, [From|T]) :-
    printable_char(From),
    send(Font, member, From),
    !,
    F1 is From + 1, N1 is N - 1,
    range_samples(F1, To, Font, N1, T).
range_samples(From, To, Font, N, T) :-
    F1 is From + 1,
    range_samples(F1, To, Font, N, T).

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
    (   effective_range(Name, From, To)
    ->  send(SP, slot, range_from, From),
        send(SP, slot, range_to,   To),
        get(SP, member, ranges, LB),
        (   get(LB?dict, member, Name, _)
        ->  send(LB, selection, Name)
        ;   true
        ),
        send(SP, fill_grid),
        update_last_range(Name),
        send(SP, report, status, '%s  (U+%04X..U+%04X)', Name, From, To)
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
    get(SP, range_from, From), From \== @nil,
    get(SP, range_to,   To),   To   \== @nil,
    get(SP, symbol_font, Font),
    Start is (From // 16) * 16,
    fill_grid_rows(G, Font, Start, From, To).

fill_grid_rows(_, _, RowStart, _, To) :-
    RowStart > To,
    !.
fill_grid_rows(G, Font, RowStart, From, To) :-
    row_string(Font, RowStart, From, To, S, FirstCode),
    (   FirstCode == none
    ->  true
    ;   /*send(G, display,
             text(string('U+%04X', RowStart), left, fixed)),*/
        send(G, display,
             picker_cell(RowStart, S, Font))
    ),
    Next is RowStart + 16,
    fill_grid_rows(G, Font, Next, From, To).

row_string(Font, RowStart, From, To, S, FirstCode) :-
    new(S, string),
    fill_row(0, 16, Font, RowStart, From, To, S, none, FirstCode).

fill_row(I, N, _, _, _, _, _, FirstCode, FirstCode) :-
    I >= N,
    !.
fill_row(I, N, Font, RowStart, From, To, S, FC0, FirstCode) :-
    Code is RowStart + I,
    (   Code >= From,
        Code =< To,
        printable_char(Code),
        send(Font, member, Code)
    ->  send(S, append, string('%c', Code)),
        (   FC0 == none
        ->  FC1 = Code
        ;   FC1 = FC0
        )
    ;   FC1 = FC0
    ),
    I1 is I + 1,
    fill_row(I1, N, Font, RowStart, From, To, S, FC1, FirstCode).

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
    findall(C, recent(C), Codes),
    (   Codes == []
    ->  send(R, display,
             text('(no recently picked symbols)',
                  left, font(helvetica, oblique, 11)))
    ;   new(S, string),
        forall(member(C, Codes),
               send(S, append, string('%c', C))),
        Codes = [First|_],
        send(R, display, picker_cell(First, S, Font))
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

pick_code(SP, Code:int) :->
    "Called by picker_cell when the user clicks a symbol"::
    get(SP, pick_mode, Mode),
    (   Mode == return
    ->  send(SP, return, Code)
    ;   Mode == type,
        send(SP, type_symbol, Code)
    ->  add_recent(Code),
        send(SP, update_recents),
        send(SP, report, status, 'Typed %c  (U+%04X)', Code, Code)
    ;   atom_codes(A, [Code]),
        send(@display, copy, A),
        add_recent(Code),
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

:- pce_end_class(symbol_picker).


		 /*******************************
		 *         PICKER CELL          *
		 *******************************/

:- pce_begin_class(picker_cell, text,
                   "Clickable row of symbols").

variable(start_code, int, get,
         "Code of the first symbol in this cell").

initialise(C, Start:int, S:string, Font:font) :->
    send_super(C, initialise, S, left, Font),
    send(C, slot, start_code, Start).

event(C, Ev:event) :->
    (   send_super(C, event, Ev)
    ->  true
    ;   send(Ev, is_a, area_exit)
    ->  send(C, report, status, '')
    ;   get(C, pointed, Ev, @off, Index),
        Index >= 0,
        get(C?string, size, Sz),
        Index < Sz
    ->  get(C?string, character, Index, Code),
        (   send(Ev, is_a, ms_left_up)
        ->  send(C?frame, pick_code, Code)
        ;   send(C?frame, capture_target),
            send(C, report, status, '%c  U+%04X / %d',
                 Code, Code, Code)
        )
    ;   true
    ).

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
