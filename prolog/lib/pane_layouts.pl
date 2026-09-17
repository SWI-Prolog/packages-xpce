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

:- module(pane_layouts,
          [ pane_placement/3,           % +Kind, +LiveKinds, -Rule
            arrangement_kinds/2,        % +Arrangement, -Kinds
            arrangement_of/2,           % +PaneTerm, -Arrangement
            record_arrangement/2,       % +Arrangement, +Seconds
            remember_arrangement/1,     % +Arrangement
            forget_arrangements/0
          ]).
:- use_module(library(lists),
              [ member/2, nth0/3, reverse/2,
                sum_list/2, append/3
              ]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(aggregate), [aggregate_all/3]).
:- use_module(library(log_store),
              [ with_log_store/2, read_log_store/3,
                append_log_store/3, rewrite_log_store/3
              ]).

/** <module> Where a new pane goes, from how windows have been arranged

The IDE has to decide where a tool, a source or a terminal it was asked
for should appear: in a window of its own, in a tab, or beside what is
there -- and if beside, on which edge of what, and how big.  One setting
for all of it (`prolog_ide.tool_placement') cannot say any of the last
three, so the first placement is usually wrong and the user reshuffles.

An _arrangement_ is a window written down with the content left out: the
kinds of pane in it, how they are tiled, and their share of the room.  It
is what `pane_frame <-pane_term' writes, stripped:

```
pane_frame([], [tab([], horizontal([ 0.2-prolog_navigator,
                                     0.8-vertical([ 0.7-editor,
                                                    0.3-terminal ]) ]))])
```

Given the kinds a window holds now and the kind of the pane to add,
pane_placement/3 reads the best arrangement that has something to say and
answers one of

  - `window`, a window of its own;
  - `tab`, a tab of the window the user is in;
  - `split(Kinds, Side, Share)`, beside the panes of those kinds, on that
    edge, taking that share of their room.

It answers in _kinds_ rather than in panes so that all of this is plain
Prolog that can be read and tested without a window on the screen; the
caller turns the kinds into the panes it has.

@see library(pane_frame) for the windows this describes.
*/

:- multifile
    default_arrangement/1.              % -Arrangement

                 /*******************************
                 *         ARRANGEMENTS         *
                 *******************************/

%       The arrangements the system comes with.  They say the things that
%       have no good single answer today: a navigator or a list of
%       buffers belongs down the left at a fifth of the width, a terminal
%       or a list of bookmarks below the editor at a third of the height,
%       a debugger in a tab beside the terminal it is debugging, a symbol
%       picker in a window of its own, being a thing you pick from while
%       typing elsewhere.  Anything not mentioned falls through to a tab,
%       which is what the IDE has always done.
%
%       They are clauses rather than data in a file so that a user's init
%       file or a project can add its own.

default_arrangement(
    pane_frame([], [tab([], vertical([0.7-editor, 0.3-terminal]))])).
default_arrangement(
    pane_frame([], [tab([], horizontal([0.2-prolog_navigator, 0.8-editor]))])).
default_arrangement(
    pane_frame([], [tab([], horizontal([0.2-prolog_navigator, 0.8-terminal]))])).
default_arrangement(
    pane_frame([], [tab([], horizontal([0.2-prolog_navigator,
                                        0.8-vertical([0.7-editor,
                                                      0.3-terminal])]))])).
default_arrangement(
    pane_frame([], [tab([], horizontal([0.2-emacs_buffer_menu, 0.8-editor]))])).
default_arrangement(
    pane_frame([], [tab([], horizontal([0.8-editor, 0.2-emacs_bookmark_editor]))])).
default_arrangement(
    pane_frame([], [ tab([], terminal),
                     tab([], prolog_debugger)
                   ])).
default_arrangement(
    pane_frame([], [tab([], symbol_picker)])).

%!  arrangement(-Arrangement, -Tier, -Priority) is nondet.
%
%   Every arrangement that could be used, with what it has earned.  An
%   arrangement the user asked to keep -- see remember_arrangement/1 --
%   is in a tier of its own above everything else, so that no amount of
%   working in another one displaces it; within that tier the one that
%   fits the window best is still the one read, which is what its
%   priority of 1 leaves to say.  The ones the system comes with carry a
%   nominal five minutes, so that any arrangement the user has really
%   worked in outranks them and they answer when nothing else does.

arrangement(Arrangement, 1, 1) :-
    load_arrangements,
    kept(_Kinds, Arrangement, _At).
arrangement(Arrangement, 0, Priority) :-
    load_arrangements,
    get_time(Now),
    stored(_Shape, Arrangement, Earned, At),
    decayed(Earned, At, Now, Priority).
arrangement(Arrangement, 0, 300) :-
    default_arrangement(Arrangement).

                 /*******************************
                 *          WHAT IT EARNS       *
                 *******************************/

/* An arrangement earns the time it is lived in.

Rearranging a window takes several steps -- merge the tab back in, drag
the pane where it belongs, pull it to the width it should have -- and only
the state that is then worked in means anything.  An arrangement is
therefore credited with the time it was on the screen, and an interval
too short to have been worked in is not credited at all, which is what
leaves the steps on the way out of it.

What is earned decays, so that an arrangement made once and never returned
to fades rather than having to be unlearned, and a habit that changes
re-ranks itself.  A priority is thus "seconds of recent use", and two of
them can simply be compared.
*/

half_life(2592000).                     % thirty days, in seconds
worth_recording(10).                    % less was a step on the way

%!  decayed(+Earned, +At, +Now, -Priority) is det.

decayed(Earned, At, Now, Priority) :-
    half_life(Half),
    Priority is Earned * 2 ** (-(Now-At)/Half).

%!  arrangement_shape(+Arrangement, -Shape) is det.
%
%   An arrangement with the sizes taken out: what tells one arrangement
%   from another.

arrangement_shape(pane_frame(_, Tabs), pane_frame([], Shapes)) :-
    !,
    maplist(tab_shape, Tabs, Shapes).
arrangement_shape(Content, Shape) :-
    content_shape(Content, Shape).

tab_shape(tab(_, Content), tab([], Shape)) :-
    content_shape(Content, Shape).

content_shape(Content, Content) :-
    atom(Content),
    !.
content_shape(Content, Shape) :-
    Content =.. [Orientation, Shares],
    orientation(Orientation),
    !,
    maplist(share_content, Shares, Contents),
    maplist(content_shape, Contents, Subs),
    Shape =.. [Orientation, Subs].
content_shape(Content, Kind) :-
    functor(Content, Kind, 1).

                 /*******************************
                 *          STRIPPING           *
                 *******************************/

%!  arrangement_of(+PaneTerm, -Arrangement) is det.
%
%   The arrangement a window is in: what `pane_frame <-pane_term' writes,
%   with everything about the content taken out.  What is left is the
%   kinds of pane, how they are tiled, their share of the room and how big
%   the window is.

arrangement_of(pane_frame(Options, Tabs), pane_frame(Kept, Stripped)) :-
    !,
    findall(O, (member(O, Options), kept_frame_option(O)), Kept),
    maplist(strip_tab, Tabs, Stripped).
arrangement_of(Content, Arrangement) :-
    strip_content(Content, Arrangement).

kept_frame_option(geometry(_)).

strip_tab(tab(_, Content), tab([], Stripped)) :-
    strip_content(Content, Stripped).

strip_content(current(Content), Stripped) :-
    !,
    strip_content(Content, Stripped).
strip_content(Content, Content) :-
    atom(Content),
    !.
strip_content(Content, Stripped) :-
    Content =.. [Orientation, Shares],
    orientation(Orientation),
    !,
    maplist(strip_share, Shares, Pairs),
    Stripped =.. [Orientation, Pairs].
strip_content(Content, Kind) :-
    functor(Content, Kind, 1).

strip_share(Share-Content, Rounded-Stripped) :-
    number(Share),
    !,
    clamped(Share, Rounded),
    strip_content(Content, Stripped).
strip_share(Content, Stripped) :-
    strip_content(Content, Stripped).

%       A pane dragged nearly shut is not an arrangement worth learning,
%       and one dragged shut altogether cannot be laid out again: a tile
%       is never given less than MIN_TILE_SIZE.  Two decimals, because a
%       share is read back and compared.

clamped(Share, Rounded) :-
    Clamped is min(0.95, max(0.05, Share)),
    Rounded is round(Clamped*100)/100.0.

                 /*******************************
                 *           THE STORE          *
                 *******************************/

/* The arrangements live in a file of their own in the XPCE config
   directory, kept as a log -- see library(log_store) for why, and for
   the file and the lock on it.

   Reading the store plays the log back: each record credits the
   arrangement it names as of when it was written, and the store is where
   that leaves things.  A record and a summary say the same thing -- this
   arrangement was worth that many seconds at that moment -- so a log that
   has grown long can be rewritten as one record per arrangement, and
   playing *that* back gives the same store again.
*/

:- dynamic
    stored/4,                           % Shape, Arrangement, Earned, At
    kept/3,                             % Kinds, Arrangement, At
    events/1,                           % records in the log as last read
    complained/1.                       % what has been warned about

max_events(200).                        % a longer log is summarised

%!  arrangements_file(-File) is nondet.
%
%   Hook.  Where the arrangements are kept.  The first clause wins, so a
%   project that wants arrangements of its own -- or a test that must not
%   touch the user's -- says so:
%
%   ```
%   :- multifile pane_layouts:arrangements_file/1.
%   pane_layouts:arrangements_file('/path/of/my/project/layouts').
%   ```
%
%   With no clause they live beside the other XPCE settings, in
%   `xpce/pane_layouts' of the config directory.

:- multifile
    arrangements_file/1.                % -File

store_file(File) :-
    arrangements_file(File),
    !.
store_file(File) :-
    absolute_file_name(user_app_config('xpce/pane_layouts'), File,
                       [ access(none), solutions(first), file_errors(fail) ]).

%!  load_arrangements is det.
%
%   Bring the store up to date with the log.  A file that cannot be read
%   leaves the system with the arrangements it comes with, which is a
%   working state.

load_arrangements :-
    (   store_file(File),
        exists_file(File)
    ->  with_log_store(File, read_log)
    ;   clear_store
    ).

clear_store :-
    retractall(stored(_,_,_,_)),
    retractall(kept(_,_,_)),
    retractall(events(_)),
    assertz(events(0)).

read_log(File) :-
    clear_store,
    read_log_store(File, replay, N),
    retractall(events(_)),
    assertz(events(N)).

count_record :-
    retract(events(N0)),
    N is N0+1,
    assertz(events(N)).

%       A record that means nothing to us costs itself and no more: a log
%       written by a later version, or edited by hand into something else,
%       still gives up everything else it holds.

replay(used(Arrangement, Seconds, At)) =>
    credit(Arrangement, Seconds, At).
replay(kept(Arrangement, At)) =>
    keep(Arrangement, At).
replay(Term) =>
    complain(unknown_term(Term)).

%!  credit(+Arrangement, +Seconds, +At) is det.
%
%   Add to what an arrangement has earned, as of the moment the record was
%   written.  An arrangement is known by its *shape* -- its panes and how
%   they are tiled, without their sizes -- so that pulling a pane an inch
%   wider does not fork the record and split what it has earned.  The
%   sizes kept are the ones last seen.

credit(Arrangement, Seconds, At) :-
    arrangement_shape(Arrangement, Shape),
    (   retract(stored(Shape, _, Earned0, At0))
    ->  decayed(Earned0, At0, At, Was)
    ;   Was = 0
    ),
    Earned is Was+Seconds,
    assertz(stored(Shape, Arrangement, Earned, At)).

%!  record_arrangement(+Arrangement, +Seconds) is det.
%
%   Credit an arrangement with the time it has just been lived in, and
%   write that down at once.

record_arrangement(Arrangement, Seconds) :-
    worth_recording(Least),
    Seconds >= Least,
    !,
    get_time(Now),
    add_record(used(Arrangement, Seconds, Now)).
record_arrangement(_, _).

%!  remember_arrangement(+Arrangement) is det.
%
%   Keep Arrangement for as long as the store lasts.  It answers over
%   anything that has been learned, however long that has been worked in,
%   and the log being summarised does not drop it.
%
%   One is kept for each set of pane kinds, so asking again for a window
%   holding the same tools replaces what was kept for them: there is one
%   answer to "where do these go", and it is the last one given.

remember_arrangement(Arrangement) :-
    get_time(Now),
    add_record(kept(Arrangement, Now)).

%!  keep(+Arrangement, +At) is det.
%
%   Replay of a `kept' record.  Keyed by the kinds rather than by the
%   shape: what the user asked to keep is where these panes go, so a new
%   answer for the same panes takes the place of the old one however
%   differently they are tiled in it.

keep(Arrangement, At) :-
    arrangement_kinds(Arrangement, Kinds),
    retractall(kept(Kinds, _, _)),
    assertz(kept(Kinds, Arrangement, At)).

%       Under the lock: play back what the others have written since we
%       last looked, add ours, and either append it or -- if the log has
%       grown long -- write the whole store back as a summary, which says
%       the same in one record per arrangement.  Without a config
%       directory the record lives in memory only.

add_record(Record) :-
    store_file(File),
    !,
    with_log_store(File, add_record(Record)).
add_record(Record) :-
    replay(Record).

add_record(Record, File) :-
    (   exists_file(File)
    ->  read_log(File)
    ;   clear_store
    ),
    replay(Record),
    events(N),
    max_events(Max),
    (   N >= Max
    ->  summarise_log(File)
    ;   append_log_store(File, write_header, Record),
        count_record
    ).

%!  summarise_log(+File) is det.
%
%   Write the log back as one record per arrangement, dropping the ones
%   that have faded: an arrangement worth less than the least that is ever
%   credited can never outrank anything again.

summarise_log(File) :-
    get_time(Now),
    worth_recording(Least),
    findall(kept(Arrangement, At),
            kept(_Kinds, Arrangement, At),
            Keeps),
    findall(used(Arrangement, Rounded, Now),
            ( stored(_Shape, Arrangement, Earned, At),
              decayed(Earned, At, Now, Priority),
              Priority >= Least,
              Rounded is round(Priority*10)/10.0
            ),
            Used),
    append(Keeps, Used, Records),
    rewrite_log(File, Records).

rewrite_log(File, Records) :-
    rewrite_log_store(File, write_header, Records),
    read_log(File).

write_header(Out) :-
    format(Out, '/*  How you have arranged the windows of the IDE.~n~n', []),
    format(Out, '    A `used\' record is a window with the content~n', []),
    format(Out, '    left out and the seconds it was worked in, as~n', []),
    format(Out, '    they stood at that moment.  They are added up~n', []),
    format(Out, '    as the file is read, the older ones counting~n', []),
    format(Out, '    for less.~n~n', []),
    format(Out, '    A `kept\' record is a window you asked to keep.~n', []),
    format(Out, '    It does not fade and answers over the rest,~n', []),
    format(Out, '    until you keep another holding the same panes.~n~n', []),
    format(Out, '    Edit it as you like.~n', []),
    format(Out, '*/~n~n', []).

%!  forget_arrangements is det.
%
%   Throw away everything that has been learned, and everything the user
%   asked to keep: the menu item says "forget how I arranged windows",
%   and an arrangement that went on answering after that would be a
%   puzzle rather than a help.

forget_arrangements :-
    retractall(stored(_,_,_,_)),
    retractall(kept(_,_,_)),
    (   store_file(File),
        exists_file(File)
    ->  with_log_store(File, forget_log)
    ;   clear_store
    ).

forget_log(File) :-
    rewrite_log(File, []).

                 /*******************************
                 *           MESSAGES           *
                 *******************************/

%       Said once and not again: a log that has something in it we do not
%       understand has it on every read.

complain(Message) :-
    (   complained(Message)
    ->  true
    ;   assertz(complained(Message)),
        print_message(warning, pane_layouts(Message))
    ).

:- multifile
    prolog:message//1.

prolog:message(pane_layouts(unknown_term(Term))) -->
    [ 'Window arrangements: ignored ~p'-[Term] ].

%!  arrangement_kinds(+Arrangement, -Kinds) is det.
%
%   The sorted set of pane kinds in an arrangement.  Multiplicity is
%   dropped: what matters is where a kind lives, not how many there are.

arrangement_kinds(pane_frame(_, Tabs), Kinds) :-
    !,
    findall(K, ( member(tab(_, Content), Tabs),
                 content_kind(Content, K)
               ), Ks),
    sort(Ks, Kinds).
arrangement_kinds(Content, Kinds) :-
    findall(K, content_kind(Content, K), Ks),
    sort(Ks, Kinds).

%!  content_kind(+Content, -Kind) is nondet.
%
%   Each pane kind in the content of a tab, in the order it appears.

content_kind(Content, Kind) :-
    atom(Content),
    !,
    Kind = Content.
content_kind(Content, Kind) :-
    Content =.. [Orientation, Shares],
    orientation(Orientation),
    !,
    member(Share, Shares),
    share_content(Share, Sub),
    content_kind(Sub, Kind).
content_kind(Content, Kind) :-
    compound(Content),                  % a kind that carries options
    functor(Content, Kind, 1).

share_content(_-Content, Content) :-
    !.
share_content(Content, Content).

share_weight(Weight-_, Weight) :-
    number(Weight),
    !.
share_weight(_, 1).

orientation(horizontal).
orientation(vertical).

                 /*******************************
                 *          THE ORACLE          *
                 *******************************/

%!  pane_placement(+Kind, +LiveKinds, -Rule) is semidet.
%
%   Where a new pane of Kind goes in a window that holds LiveKinds.  Rule
%   is `window', `tab' or `split(Kinds, Side, Share)'.  Fails when no
%   arrangement has anything to say, and the caller falls back on the
%   setting.
%
%   Arrangements are tried best first: one holding exactly what the window
%   will hold, then the one that shares most of it and adds least, then
%   the one that has earned most.  The first that yields a rule wins; an
%   arrangement yields nothing when none of the kinds beside Kind in it is
%   in the window.

pane_placement(Kind, LiveKinds, Rule) :-
    sort([Kind|LiveKinds], Want),
    findall(Tier-Score-Arrangement,
            ( arrangement(Arrangement, Tier, Priority),
              arrangement_kinds(Arrangement, Kinds),
              memberchk(Kind, Kinds),
              score(Kinds, Kind, Want, Priority, Score)
            ),
            Scored),
    sort(1, @>=, Scored, Best),
    member(_-Arrangement, Best),
    arrangement_rule(Arrangement, Kind, LiveKinds, Rule),
    !.

%!  score(+Kinds, +Kind, +Want, +Priority, -Score) is det.
%
%   How well an arrangement answers for a window that is to hold Want.
%   The factor is 1 when the arrangement is exactly that window, so an
%   exact match wins between equals while an arrangement that has been
%   lived in far longer can still win over one that has hardly been used.
%
%   An arrangement holding nothing but the kind being placed is the
%   window that pane would have to itself, and is read against *that*
%   window rather than against the one the user is working in: what it
%   says is "one of these lives alone", and the panes it does not hold
%   are the whole of its point rather than a poor fit.  Without this a
%   tool dragged out to a window of its own could never say so, as
%   sharing one kind out of three is all such an arrangement can ever do.

score([Kind], Kind, _Want, Priority, Priority) :-
    !.
score(Kinds, _Kind, Want, Priority, Score) :-
    intersection_count(Kinds, Want, Shared),
    union_count(Kinds, Want, Total),
    Score is Priority*Shared/Total.

intersection_count(Kinds, Want, Count) :-
    aggregate_all(count, (member(K, Kinds), memberchk(K, Want)), Count).

union_count(Kinds, Want, Count) :-
    append(Kinds, Want, All),
    sort(All, Union),
    length(Union, Count).

%!  arrangement_rule(+Arrangement, +Kind, +LiveKinds, -Rule) is semidet.
%
%   What an arrangement says about where a pane of Kind goes.

arrangement_rule(pane_frame(_, Tabs), Kind, LiveKinds, Rule) :-
    !,
    tabs_rule(Tabs, Kind, LiveKinds, Rule).
arrangement_rule(Content, Kind, LiveKinds, Rule) :-
    tabs_rule([tab([], Content)], Kind, LiveKinds, Rule).

tabs_rule(Tabs, Kind, _LiveKinds, window) :-
    Tabs = [tab(_, Content)],
    Content == Kind,
    !.                                  % the whole window is this pane
tabs_rule(Tabs, Kind, LiveKinds, tab) :-
    member(tab(_, Content), Tabs),
    Content == Kind,                    % a tab of its own, beside tabs
    member(tab(_, Other), Tabs),        % that are on the screen
    Other \== Kind,
    arrangement_kinds(Other, Kinds),
    member(K, Kinds),
    memberchk(K, LiveKinds),
    !.
tabs_rule(Tabs, Kind, LiveKinds, Rule) :-
    member(tab(_, Content), Tabs),
    content_rule(Content, Kind, LiveKinds, Rule),
    !.

%!  content_rule(+Content, +Kind, +LiveKinds, -Rule) is semidet.
%
%   Find Kind in the tiling of one tab and read off which of the panes
%   beside it it is to be put next to, on which edge and at what share.
%
%   Read from the inside out.  A pane is put beside what is next to it,
%   but the window may hold none of that: an arrangement of a navigator
%   left of an editor over a terminal says where the terminal goes in a
%   window that holds only the navigator, and it says it one row further
%   out.  Each row from the pane's own outwards is tried, and what it
%   gives is that row's share -- the room the whole of that side takes.

content_rule(Content, Kind, LiveKinds, Rule) :-
    kind_path(Content, Kind, Path),
    reverse(Path, Inside_out),
    member(node(Node, Index), Inside_out),
    node_rule(Node, Index, LiveKinds, Rule),
    !.

%!  kind_path(+Content, +Kind, -Path) is nondet.
%
%   The rows from the outside in that hold Kind, each with the place in
%   it of the one below.

kind_path(Content, Kind, Path) :-
    compound(Content),
    Content =.. [Orientation, Shares],
    orientation(Orientation),
    maplist(share_content, Shares, Contents),
    nth0(Index, Contents, Sub),
    (   is_kind(Sub, Kind)
    ->  Path = [node(Content, Index)]
    ;   kind_path(Sub, Kind, Rest),
        Path = [node(Content, Index)|Rest]
    ).

%!  node_rule(+Node, +Index, +LiveKinds, -Rule) is semidet.
%
%   What one row says about the pane that sits at Index in it.

node_rule(Node, Index, LiveKinds, split(Neighbours, Side, Share)) :-
    Node =.. [Orientation, Shares],
    maplist(share_content, Shares, Contents),
    maplist(share_weight, Shares, Weights),
    sum_list(Weights, Total),
    Total > 0,
    nth0(Index, Weights, Weight),
    Share is Weight/Total,
    neighbours(Contents, Index, LiveKinds, Neighbours, Before),
    side(Orientation, Before, Side).

is_kind(Sub, Kind) :-
    (   atom(Sub)
    ->  Sub == Kind
    ;   compound(Sub),
        functor(Sub, Name, 1),
        Name == Kind
    ).

%!  neighbours(+Contents, +I, +LiveKinds, -Neighbours, -Before) is semidet.
%
%   The panes the new one is put beside: the nearest thing next to it in
%   the row that is on the screen.  Nearest first, on either side, and
%   failing that everything on either side -- an arrangement that only
%   names panes the window does not hold says nothing about it.

neighbours(Contents, I, LiveKinds, Neighbours, Before) :-
    before_after(Contents, I, Befores, Afters),
    (   candidate(Befores, LiveKinds, Neighbours)
    ->  Before = before
    ;   candidate(Afters, LiveKinds, Neighbours)
    ->  Before = after
    ;   live_kinds(Befores, LiveKinds, Neighbours),
        Neighbours \== []
    ->  Before = before
    ;   live_kinds(Afters, LiveKinds, Neighbours),
        Neighbours \== [],
        Before = after
    ).

candidate(Contents, LiveKinds, Neighbours) :-
    member(Content, Contents),          % nearest first
    live_kinds([Content], LiveKinds, Neighbours),
    Neighbours \== [],
    !.

live_kinds(Contents, LiveKinds, Kinds) :-
    findall(K, ( member(Content, Contents),
                 content_kind(Content, K),
                 memberchk(K, LiveKinds)
               ), Ks),
    sort(Ks, Kinds).

%!  before_after(+Contents, +I, -Befores, -Afters) is det.
%
%   What comes before and after the new pane in its row, each nearest
%   first.

before_after(Contents, I, Befores, Afters) :-
    length(Prefix, I),
    append(Prefix, [_|Suffix], Contents),
    reverse(Prefix, Befores),
    Afters = Suffix.

%       A pane put beside what comes before it in the row goes on the far
%       side of it, and the other way round.

side(horizontal, before, right).
side(horizontal, after,  left).
side(vertical,   before, below).
side(vertical,   after,  above).
