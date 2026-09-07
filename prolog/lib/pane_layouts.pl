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
            save_arrangements/0,
            forget_arrangements/0
          ]).
:- use_module(library(lists),
              [ member/2, memberchk/2, nth0/3, reverse/2,
                sum_list/2, append/3
              ]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(filesex), [make_directory_path/1]).

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
%       have no good single answer today: a navigator belongs down the
%       left at a fifth of the width, a terminal below the editor at a
%       third of the height, a debugger in a window of its own.  Anything
%       not mentioned falls through to a tab, which is what the IDE has
%       always done.
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
    pane_frame([], [tab([], prolog_debugger)])).

%!  arrangement(-Arrangement, -Priority) is nondet.
%
%   Every arrangement that could be used, with what it has earned.  The
%   ones the system comes with carry a nominal five minutes, so that any
%   arrangement the user has really worked in outranks them and they
%   answer when nothing else does.

arrangement(Arrangement, Priority) :-
    load_arrangements,
    get_time(Now),
    stored(_Shape, Arrangement, Earned, At),
    decayed(Earned, At, Now, Priority).
arrangement(Arrangement, 300) :-
    default_arrangement(Arrangement).

                 /*******************************
                 *          WHAT IT EARNS       *
                 *******************************/

/* An arrangement earns the time it is lived in.

Rearranging a window takes several steps -- merge the tab back in, drag
the pane where it belongs, pull it to the width it should have -- and only
the state that is then worked in means anything.  An arrangement is
therefore credited with the time it was on the screen, and an interval
shorter than a minute is not credited at all, which is what leaves the
steps on the way out of it.

What is earned decays, so that an arrangement made once and never returned
to fades rather than having to be unlearned, and a habit that changes
re-ranks itself.  A priority is thus "seconds of recent use", and two of
them can simply be compared.
*/

half_life(2592000).                     % thirty days, in seconds
worth_recording(60).                    % a minute: less was a step on the way

%!  decayed(+Earned, +At, +Now, -Priority) is det.

decayed(Earned, At, Now, Priority) :-
    half_life(Half),
    Priority is Earned * 2 ** (-(Now-At)/Half).

%!  record_arrangement(+Arrangement, +Seconds) is det.
%
%   Credit an arrangement with the time it has just been lived in.  An
%   arrangement is known by its *shape* -- its panes and how they are
%   tiled, without their sizes -- so that pulling a pane an inch wider
%   does not fork the record and split what it has earned.  The sizes kept
%   are the ones last seen.

record_arrangement(Arrangement, Seconds) :-
    worth_recording(Least),
    Seconds >= Least,
    arrangement_shape(Arrangement, Shape),
    !,
    load_arrangements,
    get_time(Now),
    (   retract(stored(Shape, _, Earned0, At))
    ->  decayed(Earned0, At, Now, Was)
    ;   Was = 0
    ),
    Earned is Was+Seconds,
    assertz(stored(Shape, Arrangement, Earned, Now)),
    set_modified.
record_arrangement(_, _).

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

/* The arrangements are kept in a file of their own in the XPCE config
   directory, one term to a line, because they are meant to be read and
   edited by hand.  It is written when Prolog halts and read the first
   time anything asks.
*/

:- dynamic
    stored/4,                           % Shape, Arrangement, Earned, At
    loaded/0,
    modified/0.

set_modified :-
    (   modified
    ->  true
    ;   assertz(modified)
    ).

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
                       [ access(none), solutions(first) ]).

%!  load_arrangements is det.
%
%   Read the arrangements, once.  A file that cannot be read leaves the
%   system with the arrangements it comes with, which is a working state.

load_arrangements :-
    loaded,
    !.
load_arrangements :-
    assertz(loaded),
    store_file(File),
    (   exists_file(File)
    ->  catch(read_arrangements(File), E,
              print_message(warning, pane_layouts(no_file(File, E))))
    ;   true
    ).

read_arrangements(File) :-
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        read_terms(In),
        close(In)).

read_terms(In) :-
    read_term(In, Term, []),
    (   Term == end_of_file
    ->  true
    ;   read_arrangement(Term),
        read_terms(In)
    ).

%       An unknown term costs its own record and no more: a file written
%       by a later version, or edited by hand into something else, still
%       gives up everything else it holds.

read_arrangement(arrangement(Arrangement, Earned, At)) =>
    arrangement_shape(Arrangement, Shape),
    retractall(stored(Shape, _, _, _)),
    assertz(stored(Shape, Arrangement, Earned, At)).
read_arrangement(Term) =>
    print_message(warning, pane_layouts(unknown_term(Term))).

%!  save_arrangements is det.
%
%   Write the arrangements out, dropping the ones that have faded: an
%   arrangement worth less than the least that is ever credited can never
%   outrank anything again.

save_arrangements :-
    modified,
    !,
    retractall(modified),
    store_file(File),
    catch(write_arrangements(File), E,
          print_message(warning, pane_layouts(no_file(File, E)))).
save_arrangements.

write_arrangements(File) :-
    file_directory_name(File, Dir),
    make_directory_path(Dir),
    get_time(Now),
    worth_recording(Least),
    findall(arrangement(Arrangement, Rounded, Now),
            ( stored(_Shape, Arrangement, Earned, At),
              decayed(Earned, At, Now, Priority),
              Priority >= Least,
              Rounded is round(Priority*10)/10.0
            ),
            Records),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        write_records(Out, Records),
        close(Out)).

write_records(Out, Records) :-
    write_header(Out),
    forall(member(Record, Records),
           format(Out, '~q.~n', [Record])).

write_header(Out) :-
    format(Out, '/*  How you have arranged the windows of the IDE.~n', []),
    format(Out, '~n', []),
    format(Out, '    Each term is a window with the content left out and~n', []),
    format(Out, '    the seconds of recent use it has earned.  Written when~n', []),
    format(Out, '    Prolog halts; edit it as you like.~n', []),
    format(Out, '*/~n~n', []).

%!  forget_arrangements is det.
%
%   Throw away everything that has been learned.

forget_arrangements :-
    retractall(stored(_, _, _, _)),
    assertz(loaded),
    set_modified.

                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(pane_layouts(no_file(File, Error))) -->
    [ 'Window arrangements: cannot use ~w: ~p'-[File, Error] ].
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
    findall(Score-Arrangement,
            ( arrangement(Arrangement, Priority),
              arrangement_kinds(Arrangement, Kinds),
              memberchk(Kind, Kinds),
              score(Kinds, Want, Priority, Score)
            ),
            Scored),
    sort(1, @>=, Scored, Best),
    member(_-Arrangement, Best),
    arrangement_rule(Arrangement, Kind, LiveKinds, Rule),
    !.

%!  score(+Kinds, +Want, +Priority, -Score) is det.
%
%   How well an arrangement answers for a window that is to hold Want.
%   The factor is 1 when the arrangement is exactly that window, so an
%   exact match wins between equals while an arrangement that has been
%   lived in far longer can still win over one that has hardly been used.

score(Kinds, Want, Priority, Score) :-
    intersection_count(Kinds, Want, Shared),
    union_count(Kinds, Want, Total),
    Score is Priority*Shared/Total.

intersection_count(Kinds, Want, Count) :-
    findall(K, (member(K, Kinds), memberchk(K, Want)), Shared),
    length(Shared, Count).

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
