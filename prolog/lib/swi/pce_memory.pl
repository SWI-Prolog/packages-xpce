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

:- module(pce_memory,
          [ memory_tree/2,                      % +View, -Term
            memory_tree/3                       % +View, -Term, +Options
          ]).
:- use_module(library(pce)).
:- use_module(library(pane_frame)).
:- use_module(library(plot/hierarchy_chart)).
:- autoload(library(apply),
            [maplist/3, foldl/4, partition/4, exclude/3, convlist/3]).
:- autoload(library(lists), [sum_list/2, append/3, append/2]).
:- autoload(library(edit), [edit/1]).
:- autoload(library(pairs),
            [ group_pairs_by_key/2, map_list_to_pairs/3, pairs_keys/2,
              pairs_values/2, pairs_keys_values/3
            ]).
:- autoload(library(aggregate), [aggregate_all/3]).
:- autoload(library(thread), [call_in_thread/3]).
:- autoload(library(option), [option/2]).

/** <module> Show how Prolog uses its memory

This library defines the class prolog_memory_viewer, a pane of the IDE
that shows the memory used by Prolog as rings or a treemap (see
library(plot/hierarchy_chart)).  There are several views:

  - `overview`
    The process: the program, the threads, the shared tables, atoms
    and functors.  If the memory allocator tells how much is in use,
    what is not accounted for by these is shown as well.
  - `application`
    The modules of the application, i.e., not those of the system or a
    library, split into the `static program` and the `dynamic data`.
    The dynamic data are the dynamic predicates that have clauses, none
    of which are loaded from a file.  The module `user` holds the
    non-modular code, so its program is split into the files that
    define it.  Modules that are not loaded from a file, such as
    `prolog` that holds hooks of the libraries, only have dynamic data.
  - `data`
    The dynamic data of all modules, including the system and
    libraries.
  - `program`
    The modules and their predicates.  Below a predicate are its
    indexes, its clauses if there are at most 10, or else, for a
    multifile predicate whose clauses come from several files, its
    clauses per file, and erased clauses that are not yet reclaimed.
    What remains is the administration of the predicate (or all its
    clauses).  The balloon of a predicate tells its number of clauses.
    Thread-local predicates are shown with the threads.
  - `threads`
    Per thread its stacks, its copies of thread-local predicates and
    its private tables.  Only a thread itself can get its variant
    table, so a thread that has tables is asked for it using
    call_in_thread/3; the tables are inspected from the outside.  If
    the thread does not answer within 5 seconds, only their total is
    shown.  The description is kept as long as the space used by the
    tables of the thread does not change.
  - `tables`
    The shared tables and the private tables of each thread that has
    them.  Tables are shown as in table_statistics_by_predicate/1
    with `order_by(space)` and `top(10)`: the ten predicates whose
    tables use most space and below each the ten variants using most
    space.  The others are summed in one node.  The balloons tell the
    number of tables and answers.  The space of a table that is
    compiled to a clause (see the flag `table_compile_threshold`) is
    that of the clause, which is not part of the table space of
    statistics/2.
  - `xpce`
    The memory of xpce's own allocator, by the class hierarchy.  The
    size of a class is the number of its instances that exist times
    the size of an instance.  What instances refer to, such as the text
    of a string or the cells of a chain, is in `other xpce data`.
    Images, fonts and windows use memory of the graphics libraries,
    which is not included.

The numbers are those of predicate_property/2 (`size`), module_property/2
(`size`), thread_property/2 (`size`), thread_statistics/3,
statistics/2 and, for xpce, of `@pce<-core_usage` and the classes.
The popup of the chart edits the module, predicate or clause below the
pointer; for the clauses from a file, the first of them, and for a table
its predicate.

By default the viewer only refreshes on request.  The menu below the
Refresh button makes it refresh every so many seconds; the class
variable `refresh_interval` sets the initial interval.  The tree is
computed in a thread of its own, so the GUI keeps responding.  If
computing takes long, the next refresh waits at least four times as
long.
*/

%!  memory_tree(+View, -Term) is det.
%!  memory_tree(+View, -Term, +Options) is det.
%
%   Term describes how memory is used according to View as a term for
%   hierarchy_from_term/2.  View is one of `overview`, `program`,
%   `threads`, `tables` or `xpce`.  The values are in bytes.  The data
%   of a node is one of module(M), predicate(PI), clause(Ref),
%   thread(Id), table(M:Variant), class(Name) or a plain atom for a node
%   that groups others.  Options:
%
%     - thread(+Thread)
%     - variant_table(+Trie)
%       Trie is the variant table of Thread.  A thread can only get its
%       own, so the variant table of another thread is fetched using
%       call_in_thread/3.  These options avoid that, e.g., for the
%       thread of a GUI that computes the tree in a thread of its own.

memory_tree(View, Term) :-
    memory_tree(View, Term, []).

%   If the allocator tells how much memory is in use (see `heapused`
%   of statistics/2), `other` is what is not accounted for, such as the
%   memory of foreign libraries (including that of this GUI), records
%   and source administration.

memory_tree(overview, node(Label, 0, Children, [data(process)]), Options) :-
    !,
    memory_tree(program, Program, Options),
    memory_tree(threads, Threads, Options),
    shared_tables(Shared),
    memory_tree(xpce, XPCE, Options),
    statistics(atom_space, Atoms),
    statistics(functor_space, Functors),
    Known = [ Program, Threads, Shared, XPCE,
              node(atoms, Atoms, [], [data(atoms)]),
              node(functors, Functors, [], [data(functors)])
            ],
    maplist(term_total, Known, Totals),
    sum_list(Totals, Accounted),
    statistics(heapused, Heap),
    (   Heap > 0
    ->  thread_stacks(Stacks),
        Other is max(0, Heap+Stacks-Accounted),
        append(Known, [node('other (not accounted for)', Other, [],
                            [data(other)])], Children)
    ;   Children = Known
    ),
    current_prolog_flag(pid, Pid),
    format(atom(Label), 'Process ~w', [Pid]).
memory_tree(program, node(program, 0, Modules, [data(program)]), _) :-
    !,
    findall(M, current_module(M), Ms0),
    sort(Ms0, Ms),
    findall(T, ( member(M, Ms),
                 module_tree(M, T)
               ), Modules).
memory_tree(application, node(application, 0, [Static, Data],
                             [data(application)]), _) :-
    !,
    application_modules(Ms),
    findall(T, ( member(M, Ms),
                 program_module(M),
                 static_module_tree(M, T)
               ), StaticMs),
    Static = node('static program', 0, StaticMs, [data(static)]),
    findall(T, ( member(M, Ms),
                 data_module_tree(M, T)
               ), DataMs),
    Data = node('dynamic data', 0, DataMs, [data(dynamic)]).
memory_tree(data, node('dynamic data', 0, DataMs, [data(dynamic)]), _) :-
    !,
    findall(M, current_module(M), Ms0),
    sort(Ms0, Ms),
    findall(T, ( member(M, Ms),
                 data_module_tree(M, T)
               ), DataMs).
memory_tree(threads, node(threads, 0, Threads, [data(threads)]), Options) :-
    !,
    forget_thread_tables,
    thread_local_predicates(TLPreds),
    findall(T, ( thread_property(Id, size(Size)),
                 catch(thread_tree(Id, Size, TLPreds, Options, T),
                       error(_,_), fail)
               ), Threads).
memory_tree(tables, node(tables, 0, [Shared|Private], [data(tables)]),
            Options) :-
    !,
    shared_tables(Shared),
    findall(T, ( thread_property(Id, size(_)),
                 catch(thread_table_tree(Id, Options, T), error(_,_), fail)
               ), Private).
memory_tree(xpce, node(xpce, 0, [Classes, Other], [data(xpce)]), _) :-
    !,
    class_tree(class(object), Classes),
    term_total(Classes, Instances),
    get(@pce, core_usage, Core),
    OtherSize is max(0, Core-Instances),
    Other = node('other xpce data', OtherSize, [], [data(xpce_data)]).
memory_tree(View, _, _) :-
    domain_error(memory_view, View).

term_total(node(_, Own, Children, _), Total) :-
    maplist(term_total, Children, Totals),
    sum_list(Totals, Sum),
    Total is Own+Sum.

thread_stacks(Total) :-
    aggregate_all(sum(S),
                  ( thread_property(Id, size(_)),
                    catch(stacks_allocated(Id, S), error(_,_), fail)
                  ),
                  Total).

stacks_allocated(Id, Size) :-
    thread_statistics(Id, local, L),
    thread_statistics(Id, global, G),
    thread_statistics(Id, trail, T),
    Size is L+G+T.


                 /*******************************
                 *            PROGRAM           *
                 *******************************/

%   The part of the size of a module that is not used by its predicates
%   is that of the module itself.  The size of the module includes the
%   copies of its thread-local predicates in the calling thread; these
%   are shown with the threads.

module_tree(M, node(M, Own, Preds, [data(module(M))])) :-
    module_property(M, size(Size)),
    findall(P, local_predicate(M, P), Ps),
    foldl(predicate_size(M), Ps, 0-[], Sum-Preds0),
    reverse(Preds0, Preds),
    Own is max(0, Size-Sum).

local_predicate(M, Name/Arity) :-
    current_predicate(Name, M:Head),
    \+ predicate_property(M:Head, imported_from(_)),
    functor(Head, Name, Arity).

predicate_size(M, PI, Sum0-Preds0, Sum-Preds) :-
    PI = Name/Arity,
    functor(Head, Name, Arity),
    (   predicate_property(M:Head, size(Size))
    ->  true
    ;   Size = 0
    ),
    Sum is Sum0+Size,
    (   predicate_property(M:Head, thread_local)
    ->  Preds = Preds0
    ;   predicate_tree(M:Head, PI, Size, Tree),
        Preds = [Tree|Preds0]
    ).

                 /*******************************
                 *   APPLICATION AND DATA       *
                 *******************************/

%   The application is the modules that are not part of the system or
%   a library.  Its predicates are split into the static program and the
%   dynamic data: dynamic predicates with no clauses loaded from a file.
%   The module `user` holds the non-modular code, which is split into
%   the files that define it.  Thread-local predicates are shown with
%   the threads.

application_modules(Ms) :-
    findall(M, ( current_module(M),
                 module_property(M, class(user))
               ), Ms0),
    sort(Ms0, Ms).

%   A module of the application that is not loaded from a file is
%   mostly one that only holds hooks, e.g., `prolog` for the multifile
%   predicates of the libraries.  Such a module has no program of its
%   own, but its dynamic data, e.g., facts asserted into it, is part of
%   the application.

program_module(user) :-
    !.
program_module(M) :-
    module_property(M, file(_)).

%   module_predicates(+M, -Preds, -Sum)
%
%   Preds is a list of Kind-pred(PI, Head, Size) for the local
%   predicates of M, where Kind is one of `static`, `data` or
%   `thread_local`.  Sum is the size of all of them.

module_predicates(M, Preds, Sum) :-
    findall(Kind-pred(PI, Head, Size),
            ( local_predicate(M, PI),
              PI = Name/Arity,
              functor(Head, Name, Arity),
              (   predicate_property(M:Head, size(Size))
              ->  true
              ;   Size = 0
              ),
              predicate_kind(M:Head, Kind)
            ), Preds),
    aggregate_all(sum(S), member(_-pred(_, _, S), Preds), Sum).

predicate_kind(Pred, Kind) :-
    (   predicate_property(Pred, thread_local)
    ->  Kind = (thread_local)
    ;   dynamic_data(Pred)
    ->  Kind = data
    ;   Kind = static
    ).

dynamic_data(Pred) :-
    predicate_property(Pred, dynamic),
    \+ predicate_property(Pred, file(_)),
    predicate_property(Pred, number_of_clauses(N)),
    N > 0.

%   The module administration is part of the static program.

static_module_tree(M, node(M, Own, Children, [data(module(M))])) :-
    module_property(M, size(Size)),
    module_predicates(M, Preds, Sum),
    Own is max(0, Size-Sum),
    (   M == user
    ->  findall(File-Tree,
                ( member(static-pred(PI, Head, PSize), Preds),
                  predicate_file(M:Head, File),
                  \+ library_file(File),
                  \+ hook_declaration(M:Head, File),
                  predicate_tree(M:Head, PI, PSize, Tree)
                ), Pairs),
        file_nodes(Pairs, Children)
    ;   findall(Tree, ( member(static-pred(PI, Head, PSize), Preds),
                        predicate_tree(M:Head, PI, PSize, Tree)
                      ), Children)
    ).

%   The system declares hooks in `user`, e.g., term_expansion/2, that
%   have no clauses unless the program adds them.

hook_declaration(Pred, '<no file>') :-
    predicate_property(Pred, number_of_clauses(0)).

predicate_file(Pred, File) :-
    (   predicate_property(Pred, file(File0))
    ->  File = File0
    ;   File = '<no file>'
    ).

%   library_file(+File)
%
%   File is part of the system or a library: it is below the home
%   directory of Prolog (as for the class of a module) or it defines a
%   module of the system or a library.  These add clauses to multifile
%   predicates of `user`, e.g., portray/1.

library_file(File) :-
    current_prolog_flag(home, Home),
    atom_concat(Home, /, Prefix),
    sub_atom(File, 0, _, _, Prefix),
    !.
library_file(File) :-
    source_file_property(File, module(M)),
    module_property(M, class(Class)),
    library_class(Class),
    !.

library_class(library).
library_class(system).

%   file_nodes(+Pairs, -FileNodes)
%
%   Group the File-Tree pairs of `user` by file.  A node is labelled
%   with the base name of the file, or its last two parts if another
%   file has the same base name.

file_nodes(Pairs, Nodes) :-
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, ByFile),
    pairs_keys(ByFile, Files),
    maplist(file_node(Files), ByFile, Nodes).

file_node(_, '<no file>'-Trees,
          node('<no file>', 0, Trees,
               [ data(no_file),
                 title('Predicates of user that are not defined in a file')
               ])) :-
    !.
file_node(Files, File-Trees,
          node(Label, 0, Trees, [data(file(File)), title(Title)])) :-
    file_label(Files, File, Label),
    length(Trees, Count),
    counted(Count, predicate, CountText),
    format(atom(Title), '~w: ~w', [File, CountText]).

file_label(Files, File, Label) :-
    file_base_name(File, Base),
    (   member(Other, Files),
        Other \== File,
        file_base_name(Other, Base)
    ->  file_directory_name(File, Dir),
        file_base_name(Dir, DirBase),
        atomic_list_concat([DirBase, Base], /, Label)
    ;   Label = Base
    ).

data_module_tree(M, node(M, 0, Trees, [data(module(M))])) :-
    module_predicates(M, Preds, _),
    findall(Tree, ( member(data-pred(PI, Head, PSize), Preds),
                    predicate_tree(M:Head, PI, PSize, Tree)
                  ), Trees),
    Trees \== [].

%   predicate_tree(+Pred, +PI, +Size, -Tree)
%
%   The size of a predicate is that of its clauses, its indexes and its
%   own administration.  The clauses are shown if there are only a few
%   (see max_clauses/1) and else per source file for a multifile
%   predicate whose clauses come from several files.  Erased clauses
%   that are not yet reclaimed are shown separately.

predicate_tree(Pred, PI, Size,
               node(Label, Own, Children,
                    [data(predicate(M:PI)), title(Title)])) :-
    format(atom(Label), '~q', [PI]),
    Pred = M:_,
    (   predicate_property(Pred, number_of_clauses(Count))
    ->  true
    ;   Count = 0
    ),
    counted(Count, clause, CountText),
    format(atom(Title), '~q: ~w', [M:PI, CountText]),
    index_nodes(Pred, Indexes),
    clause_nodes(Pred, PI, Clauses),
    (   predicate_property(Pred, erased_clause_space(Erased))
    ->  Gone = [node('erased clauses', Erased, [], [data(erased)])]
    ;   Gone = []
    ),
    append([Indexes, Clauses, Gone], Children),
    maplist(term_total, Children, Totals),
    sum_list(Totals, Parts),
    Own is max(0, Size-Parts).

%   index_nodes(+Pred, -Nodes)
%
%   A node for each index of Pred (see `indexed` of predicate_property/2).

index_nodes(Pred, Nodes) :-
    (   predicate_property(Pred, indexed(Indexes))
    ->  convlist(index_node, Indexes, Nodes)
    ;   Nodes = []
    ).

%   An index is labelled with the path to the argument it is on: `1`
%   for the first argument, `2/1` for the first argument of the
%   compound in the second (a deep index) and `1+2` for an index on
%   the combination of two arguments.

index_node(Index, node(Label, Size, [], [data(index), title(Title)])) :-
    is_dict(Index, Kind),
    _{size:Size, arguments:Args, position:Position,
      buckets:Buckets, speedup:Speedup} :< Index,
    Size > 0,
    atomic_list_concat(Args, +, ArgsLabel),
    append(Position, [ArgsLabel], Path),
    atomic_list_concat(Path, /, PathLabel),
    format(atom(Label), 'index ~w', [PathLabel]),
    format(atom(Title), '~w index on argument ~w: ~D buckets, speedup ~1f',
           [Kind, PathLabel, Buckets, Speedup]).

%   clause_nodes(+Pred, +PI, -Nodes)
%
%   A node per clause if there are at most max_clauses/1 of them.  The
%   clauses of a multifile predicate with more clauses are shown per
%   source file if they come from more than one.

max_clauses(10).

clause_nodes(Pred, PI, Nodes) :-
    (   predicate_property(Pred, number_of_clauses(Count)),
        max_clauses(Max),
        Count =< Max
    ->  findall(Node, clause_node(Pred, PI, Node), Nodes)
    ;   predicate_property(Pred, multifile)
    ->  source_nodes(Pred, Nodes)
    ;   Nodes = []
    ).

clause_node(Pred, PI, node(Label, Size, [], [data(clause(Ref)), title(Title)])) :-
    nth_clause(Pred, N, Ref),
    clause_property(Ref, size(Size)),
    format(atom(Label), 'clause ~d', [N]),
    (   clause_property(Ref, file(File)),
        clause_property(Ref, line_count(Line))
    ->  format(atom(Title), 'clause ~d of ~q at ~w:~d', [N, PI, File, Line])
    ;   format(atom(Title), 'clause ~d of ~q', [N, PI])
    ).

%   A node per source file holds the clauses from that file.  Its data
%   is the first of them, so editing it goes there.  keysort/2 is
%   stable, so the clauses of a file stay in their order.

source_nodes(Pred, Nodes) :-
    findall(Source-(Size-Ref),
            ( nth_clause(Pred, _, Ref),
              clause_property(Ref, size(Size)),
              (   clause_property(Ref, source(Source))
              ->  true
              ;   Source = '<asserted>'
              )
            ), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, BySource),
    (   BySource = [_,_|_]
    ->  maplist(source_node, BySource, Nodes)
    ;   Nodes = []
    ).

source_node(Source-Clauses,
            node(Label, Size, [], [data(Data), title(Title)])) :-
    pairs_keys_values(Clauses, Sizes, [First|_]),
    sum_list(Sizes, Size),
    length(Clauses, Count),
    counted(Count, clause, Text),
    format(atom(Title), '~w: ~w', [Source, Text]),
    (   Source == '<asserted>'
    ->  Label = Source,
        Data = asserted
    ;   file_base_name(Source, Label),
        Data = clause(First)
    ).

                 /*******************************
                 *            THREADS           *
                 *******************************/

thread_local_predicates(Preds) :-
    findall(M:Head,
            ( current_predicate(_, M:Head),
              \+ predicate_property(M:Head, imported_from(_)),
              predicate_property(M:Head, thread_local)
            ), Preds).

thread_tree(Id, Size, TLPreds, Options,
            node(Label, Other, [Stacks, Local, Tables], [data(thread(TID))])) :-
    thread_label(Id, Label, TID),
    stacks_tree(Id, Stacks),
    findall(node(PLabel, S, [], [data(predicate(M:Name/Arity))]),
            ( member(M:Head, TLPreds),
              '$thread_local_predicate_size'(M:Head, Id, S),
              functor(Head, Name, Arity),
              format(atom(PLabel), '~q', [M:Name/Arity])
            ), LocalPreds),
    Local = node('thread-local data', 0, LocalPreds, [data(thread_local)]),
    thread_statistics(Id, table_space_used, TableSpace),
    thread_tables(Id, TableSpace, Options, Tables),
    maplist(term_total, [Stacks, Local, Tables], Totals),
    sum_list(Totals, Accounted),
    Other is max(0, Size-Accounted).

%   thread_tables(+Id, +Space, +Options, -Tree)
%
%   Tree describes the private tables of a thread, which use Space.
%   Only a thread itself can get its variant table, so that of another
%   thread is fetched using call_in_thread/3; the tables are inspected
%   by the calling thread.  If the thread does not answer in time, only
%   the total is known.  Describing hundreds of thousands of tables
%   takes a second or more, so the description is kept as long as the
%   space of the tables of the thread does not change.

:- dynamic
    thread_tables_cache/3.              % Key, Space, Tree

thread_tables(Id, Space, Options, Tree) :-
    thread_key(Id, Key),
    (   Space > 0,
        (   thread_tables_cache(Key, Space, Tree0)
        ->  true
        ;   thread_variant_table(Id, Options, VT)
        ->  table_tree('private tables', VT, Space, Tree0),
            retractall(thread_tables_cache(Key, _, _)),
            assertz(thread_tables_cache(Key, Space, Tree0))
        )
    ->  Tree = Tree0
    ;   Tree = node('private tables', Space, [], [data(private_tables)])
    ).

%   thread_table_tree(+Id, +Options, -Tree)
%
%   Tree describes the private tables of a thread that has them, as a
%   node labelled with the thread.

thread_table_tree(Id, Options,
                  node(Label, Own, Children, [data(thread(TID))])) :-
    thread_statistics(Id, table_space_used, Space),
    Space > 0,
    thread_label(Id, Label, TID),
    thread_tables(Id, Space, Options, node(_, Own, Children, _)).

thread_variant_table(Id, Options, VT) :-
    (   thread_self(Me),
        same_thread(Id, Me)
    ->  '$tbl_local_variant_table'(VT)
    ;   option(thread(Thread), Options),
        same_thread(Id, Thread),
        option(variant_table(VT0), Options)
    ->  VT = VT0
    ;   catch(call_in_thread(Id, '$tbl_local_variant_table'(VT),
                             [timeout(5)]),
              _, fail)
    ).

%   thread_key(+Thread, -Key)
%
%   Key identifies Thread.  Thread ids are reused, so it includes the
%   id of the thread of the operating system.

thread_key(Thread, TID-SysId) :-
    thread_property(Thread, id(TID)),
    (   thread_property(Thread, system_thread_id(SysId0))
    ->  SysId = SysId0
    ;   SysId = 0
    ).

%   forget_thread_tables
%
%   Forget the descriptions of the tables of threads that are gone.

forget_thread_tables :-
    forall(( thread_tables_cache(Key, _, _),
             \+ ( Key = TID-_,
                   catch(thread_key(TID, Key), error(_,_), fail)
                 )
           ),
           retractall(thread_tables_cache(Key, _, _))).

same_thread(T1, T2) :-
    thread_property(T1, id(Id)),
    thread_property(T2, id(Id)).

thread_label(Id, Label, TID) :-
    thread_property(Id, id(TID)),
    (   thread_property(Id, alias(Alias))
    ->  Label = Alias
    ;   format(atom(Label), 'thread ~d', [TID])
    ).

stacks_tree(Id, node(stacks, 0, Stacks, [data(stacks)])) :-
    maplist(stack_tree(Id), [global, local, trail], Stacks).

stack_tree(Id, Stack, node(Stack, 0, [Used, Free], [data(stack(Stack))])) :-
    atom_concat(Stack, used, UsedKey),
    thread_statistics(Id, Stack, Allocated),
    thread_statistics(Id, UsedKey, InUse),
    FreeBytes is max(0, Allocated-InUse),
    Used = node(used, InUse, [], [data(used)]),
    Free = node(free, FreeBytes, [], [data(free)]).


                 /*******************************
                 *            TABLES            *
                 *******************************/

%   The tables in a pool: the predicates using most space and below
%   each the variants using most space, at most max_tables/1 of each;
%   the others are summed in one node.  The part of the pool that is not
%   used by the answer tries is that of the variant table and the
%   administration of the tables.

shared_tables(Tree) :-
    statistics(shared_table_space_used, Pool),
    (   '$tbl_global_variant_table'(VT)
    ->  true
    ;   VT = @none
    ),
    table_tree('shared tables', VT, Pool, Tree).

max_tables(10).

%   A completed table that is used often is compiled to a clause and
%   its answer trie is discarded.  The clause is not part of the pool,
%   but its space is that of the table.

table_tree(Label, VT, Pool, node(Label, Own, Preds, [data(tables)])) :-
    table_predicates(VT, Preds, InPool),
    Own is max(0, Pool-InPool).

table_predicates(@none, [], 0) :-
    !.
table_predicates(VT, Nodes, InPool) :-
    findall(PI-(Size-table(M:Variant, Answers, Compiled, TrieSize)),
            ( trie_gen(VT, M:Variant, Trie),
              answer_table(Trie, TrieSize, Size, Answers, Compiled),
              functor(Variant, Name, Arity),
              PI = M:Name/Arity
            ), Tables),
    aggregate_all(sum(S), member(_-(_-table(_, _, _, S)), Tables), InPool),
    keysort(Tables, Sorted),
    group_pairs_by_key(Sorted, ByPred),
    maplist(predicate_space, ByPred, Spaced),
    top_nodes(Spaced, pred_tables, predicates, Nodes).

predicate_space(PI-Variants, Space-(PI-Variants)) :-
    pairs_keys(Variants, Sizes),
    sum_list(Sizes, Space).

%   top_nodes(+Pairs, :ToNode, +What, -Nodes)
%
%   Nodes are the nodes for the max_tables/1 Space-Item pairs with the
%   most space, followed by a node for the other ones.

top_nodes(Pairs, ToNode, What, Nodes) :-
    sort(1, @>=, Pairs, Ordered),
    max_tables(Max),
    length(Ordered, Count),
    (   Count =< Max
    ->  maplist(ToNode, Ordered, Nodes)
    ;   length(Top, Max),
        append(Top, Rest, Ordered),
        maplist(ToNode, Top, TopNodes),
        pairs_keys(Rest, RestSizes),
        sum_list(RestSizes, RestSpace),
        length(Rest, RestCount),
        format(atom(Label), '~D other ~w', [RestCount, What]),
        append(TopNodes, [node(Label, RestSpace, [], [data(other)])], Nodes)
    ).

%   answer_table(+Trie, -TrieSize, -Size, -Answers, -Compiled)
%
%   TrieSize is the space of the answer trie and its dependency graph;
%   Size adds the clause the table is compiled to.  Compiled is `true`
%   if there is such a clause.

answer_table(Trie, TrieSize, Size, Answers, Compiled) :-
    '$trie_property'(Trie, size(S0)),
    '$trie_property'(Trie, value_count(Answers)),
    (   '$trie_property'(Trie, idg_size(S1))
    ->  true
    ;   S1 = 0
    ),
    (   '$trie_property'(Trie, compiled_size(S2))
    ->  Compiled = true
    ;   S2 = 0,
        Compiled = false
    ),
    TrieSize is S0+S1,
    Size is TrieSize+S2.

pred_tables(_Space-(PI-Variants),
            node(Label, 0, Nodes, [data(predicate(PI)), title(Title)])) :-
    format(atom(Label), '~q', [PI]),
    length(Variants, Tables),
    aggregate_all(sum(N),
                  member(_-table(_, N, _, _), Variants),
                  Answers),
    counted(Tables, table, TablesText),
    counted(Answers, answer, AnswersText),
    format(atom(Title), '~q: ~w, ~w', [PI, TablesText, AnswersText]),
    top_nodes(Variants, variant_table, variants, Nodes).

variant_table(Size-table(M:Variant, Answers, Compiled, _),
              node(Label, Size, [], [data(table(M:Variant)), title(Title)])) :-
    variant_label(Variant, Label),
    counted(Answers, answer, AnswersText),
    (   Compiled == true
    ->  format(atom(Title), '~w: ~w, compiled', [Label, AnswersText])
    ;   format(atom(Title), '~w: ~w', [Label, AnswersText])
    ).

%   counted(+Count, +What, -Text)
%
%   Text is Count followed by What, in the plural unless Count is 1.

counted(1, What, Text) :-
    !,
    format(atom(Text), '1 ~w', [What]).
counted(N, What, Text) :-
    format(atom(Text), '~D ~ws', [N, What]).

variant_label(Variant, Label) :-
    copy_term(Variant, Copy),
    numbervars(Copy, 0, _),
    format(atom(Label), '~W',
           [ Copy,
             [ max_depth(4), quoted(true), portray(true), numbervars(true) ]
           ]).


                 /*******************************
                 *             XPCE             *
                 *******************************/

%   class_tree(+Class, -Tree)
%
%   Tree holds the instances of Class and its subclasses that exist.
%   Subclasses without instances are left out.

class_tree(Class, node(Name, Own, Subs, [data(class(Name)), title(Title)])) :-
    get(Class, name, Name),
    get(Class, no_created, Created),
    get(Class, no_freed, Freed),
    get(Class, instance_size, InstanceSize),
    Count is Created-Freed,
    Own is Count*InstanceSize,
    (   get(Class, sub_classes, SubClasses)
    ->  chain_list(SubClasses, Classes),
        maplist(class_tree, Classes, Subs0),
        exclude(empty_tree, Subs0, Subs)
    ;   Subs = []
    ),
    format(atom(Title), 'class ~w: ~D instances of ~D bytes',
           [Name, Count, InstanceSize]).

empty_tree(Tree) :-
    term_total(Tree, 0).


                 /*******************************
                 *             PANE             *
                 *******************************/

/* The memory viewer as a pane of the IDE (see library(pane_frame)).
The chart is computed again on ->refresh, keeping the node zoomed into if
it still exists.  Computing it walks all predicates and tables, so by
default it is only refreshed on request; the Refresh group has a menu
to refresh it every so many seconds.
*/

:- pce_begin_class(prolog_memory_viewer, tool_pane,
                   "Show how Prolog uses its memory").

class_variable(view, {overview,application,program,data,threads,tables,xpce}, overview,
               "View shown initially").

class_variable(refresh_interval, num*, @nil,
               "Seconds between refreshes; @nil: only on request").

variable(view, {overview,application,program,data,threads,tables,xpce}, get, "Current view").
variable(timer, timer*, get, "Timer for the live refresh").
variable(refresh_interval, num*, get, "Seconds between refreshes").
variable(generation, int := 0, get, "Number of the last refresh started").
variable(busy, bool := @off, get, "A refresh is being computed").

initialise(MV) :->
    send_super(MV, initialise, memory_viewer),
    send(MV, append_window, new(W, hierarchy_window)),
    send(MV, append_window, new(VD, dialog), W, left),
    send(MV, append_window, new(CD, dialog), W, right),
    send(W, unit, bytes),
    get(W, popup, P),
    send(P, insert_before,
         menu_item(edit,
                   message(MV, edit_node),
                   condition := message(MV, can_edit_node),
                   end_group := @on),
         zoom_out),
    get(MV, class_variable_value, view, View),
    send(MV, slot, view, View),
    send(VD, gap, size(5, 3)),
    send(CD, gap, size(5, 3)),
    new(VM, menu(view, choice, message(MV, view, @arg1))),
    send_list(VM, append,
              [overview, application, program, data, threads, tables, xpce]),
    send(VM, layout, vertical),
    send(VM, selection, View),
    new(CM, menu(chart, choice, message(W, view, @arg1))),
    send_list(CM, append, [rings, treemap]),
    send(CM, layout, vertical),
    send(CM, selection, W?view),
    send(W, view_message, message(CM, selection, @arg1)),
    new(AM, menu(auto_refresh, cycle, message(MV, auto_refresh, @arg1))),
    forall(auto_refresh(Secs, Label),
           send(AM, append, menu_item(Secs, @default, Label))),
    send(VD, append, labelled_group(view, [VM])),
    send_list(CD, append,
              [ labelled_group(chart, [CM]),
                labelled_group(refresh,
                               [ button(refresh_now, message(MV, refresh)),
                                 AM
                               ])
              ]),
    send(MV, refresh),
    get(MV, class_variable_value, refresh_interval, Interval),
    send(MV, refresh_interval, Interval).

auto_refresh(0,   off).
auto_refresh(1,   '1 sec').
auto_refresh(2,   '2 sec').
auto_refresh(5,   '5 sec').
auto_refresh(10,  '10 sec').
auto_refresh(20,  '20 sec').
auto_refresh(50,  '50 sec').
auto_refresh(100, '100 sec').

auto_refresh(MV, Secs:int) :->
    "Refresh every Secs seconds; 0: only on request"::
    (   Secs == 0
    ->  send(MV, refresh_interval, @nil)
    ;   send(MV, refresh_interval, Secs)
    ).

menu(MV, Name:name, Menu:menu) :<-
    "Menu of one of the dialogs by name"::
    get(MV?members, find,
        and(message(@arg1, instance_of, dialog),
            ?(@arg1?graphicals, find,
              and(message(@arg1, instance_of, labelled_group),
                  ?(@arg1, member, Name)))),
        D),
    get(D?graphicals, find,
        and(message(@arg1, instance_of, labelled_group),
            ?(@arg1, member, Name)),
        G),
    get(G, member, Name, Menu).

unlink(MV) :->
    send(MV, refresh_interval, @nil),
    send_super(MV, unlink).

refresh_interval(MV, Interval:num*) :->
    "Refresh every Interval seconds; @nil: only on request"::
    send(MV, slot, refresh_interval, Interval),
    (   get(MV, menu, auto_refresh, AM)
    ->  (   Interval == @nil
        ->  send(AM, selection, 0)
        ;   Secs is round(Interval),
            ignore(send(AM, selection, Secs))
        )
    ;   true
    ),
    (   get(MV, timer, Old), Old \== @nil
    ->  free(Old),
        send(MV, slot, timer, @nil)
    ;   true
    ),
    (   Interval == @nil
    ->  true
    ;   send(MV, slot, timer,
             new(T, timer(Interval, message(MV, live_refresh)))),
        send(T, service, @on),          % hide from the debugger
        send(T, start)
    ).

live_refresh(MV) :->
    "Refresh by the timer, unless a refresh is running"::
    (   get(MV, busy, @on)
    ->  true
    ;   send(MV, refresh)
    ).

chart_window(MV, W:hierarchy_window) :<-
    "The window showing the chart"::
    get(MV, window, hierarchy_window, W).

pane_label(_MV, Label:name) :<-
    "What my tab is called"::
    Label = 'Memory'.

menu_bar_key(_MV, Key:name) :<-
    "Every memory viewer asks for the same menu bar"::
    Key = memory_viewer.

fill_menu_bar(MV, MD:tool_dialog) :->
    "Put my menu on the bar of the window I am in"::
    get(MD, popup, memory, @on, Popup),
    send_list(Popup, append,
              [ menu_item(refresh, message(MV, refresh)),
                menu_item(collect_garbage, message(MV, collect_garbage))
              ]).

view(MV, View:{overview,application,program,data,threads,tables,xpce}) :->
    "Switch to another view"::
    send(MV, slot, view, View),
    get(MV, menu, view, Menu),
    send(Menu, selection, View),
    get(MV, chart_window, W),
    send(W, root, @nil),
    send(MV, refresh).

/* Computing the tree can take a second or more, for example to
describe a thread with many tables, so it is computed in a thread of
its own and the GUI shows the result when it arrives.  The private
tables of the thread of the GUI are in the `tables` view; only that
thread can get their variant table, so it passes it.  A result that
arrives after a newer refresh started is dropped.  The live refresh
waits at least four times as long as computing took, so it does not
keep a CPU busy.
*/

refresh(MV, Wait:[bool]) :->
    "Compute the chart again, keeping the node zoomed into"::
    get(MV, view, View),
    get(MV, generation, Gen0),
    Gen is Gen0+1,
    send(MV, slot, generation, Gen),
    thread_self(Me),
    (   '$tbl_local_variant_table'(VT)
    ->  Options = [thread(Me), variant_table(VT)]
    ;   Options = [thread(Me)]
    ),
    (   Wait == @on
    ->  memory_tree(View, Term, Options),
        send(MV, show_tree, Gen, Term, 0)
    ;   send(MV, slot, busy, @on),
        thread_create(refresh_tree(MV, Gen, View, Options), _,
                      [detached(true)])
    ).

refresh_tree(MV, Gen, View, Options) :-
    get_time(T0),
    catch(memory_tree(View, Term, Options), E, true),
    get_time(T1),
    Time is T1-T0,
    (   var(E)
    ->  in_pce_thread(show_tree(MV, Gen, Term, Time))
    ;   in_pce_thread(refresh_failed(MV, Gen, E))
    ).

%   The viewer may have been closed while the tree was computed.

show_tree(MV, Gen, Term, Time) :-
    (   viewer(MV)
    ->  send(MV, show_tree, Gen, Term, Time)
    ;   true
    ).

refresh_failed(MV, Gen, E) :-
    (   viewer(MV),
        get(MV, generation, Gen)
    ->  send(MV, slot, busy, @off),
        send(MV, report, error, 'Refresh failed'),
        print_message(error, E)
    ;   true
    ).

viewer(MV) :-
    object(MV),
    send(MV, instance_of, prolog_memory_viewer).

show_tree(MV, Gen:int, Term:prolog, Time:num) :->
    "Show the tree computed by refresh Gen in Time seconds"::
    (   get(MV, generation, Gen)
    ->  send(MV, slot, busy, @off),
        get(MV, view, View),
        get(MV, chart_window, W),
        focus_labels(W, Path),
        hierarchy_from_term(Term, Root),
        send(W, root, Root),
        (   Path = [_|Below],
            follow_labels(Below, Root, Focus)
        ->  send(W, focus_node, Focus)
        ;   true
        ),
        get(Root, total, Total),
        get(W?chart, format_value, Total, Size),
        send(MV, report, status, '%s: %s', View, Size),
        send(MV, live_interval, Time)
    ;   true
    ).

live_interval(MV, Time:num) :->
    "Wait at least four times as long as the last refresh took"::
    get(MV, refresh_interval, Interval),
    get(MV, timer, Timer),
    (   Interval \== @nil,
        Timer \== @nil
    ->  Wait is max(Interval, 4*Time),
        send(Timer, interval, Wait)
    ;   true
    ).

%   The popup acts on the node below the pointer when it opened.  A
%   module, predicate or clause can be edited if we know where it is
%   defined.

can_edit_node(MV) :->
    "True if the popup is on something with a source"::
    get(MV, popup_target, Target),
    has_source(Target).

edit_node(MV) :->
    "Edit the module, predicate or clause the popup is on"::
    get(MV, popup_target, Target),
    edit(Target).

popup_target(MV, Target:prolog) :<-
    "What edit/1 opens for the node the popup is on"::
    get(MV?chart_window, popup_node, Node),
    Node \== @nil,
    get(Node, data, Data),
    edit_target(Data, Target).

collect_garbage(MV) :->
    "Run the garbage collectors and show the result"::
    garbage_collect,
    garbage_collect_atoms,
    garbage_collect_clauses,
    trim_stacks,
    send(MV, refresh).

:- pce_end_class(prolog_memory_viewer).


%   A group of dialog items with its label above them.  The items are
%   in a narrow column, so menus do not show their own label.

:- pce_begin_class(labelled_group, dialog_group,
                   "Dialog items with a label above them").

initialise(G, Name:name, Items:prolog) :->
    send_super(G, initialise, Name, group),
    get(Name, label_name, Label),
    send(G, label, Label),
    send(G, label_format, above),
    forall(member(Item, Items),
           ( (   send(Item, instance_of, menu)
             ->  send(Item, show_label, @off)
             ;   true
             ),
             send(G, append, Item)
           )).

:- pce_end_class(labelled_group).

edit_target(module(M),    module(M)).
edit_target(predicate(PI), PI).
edit_target(clause(Ref),   clause(Ref)).
edit_target(file(File),    file(File)).
edit_target(table(M:Variant), M:Name/Arity) :-
    functor(Variant, Name, Arity).

has_source(module(M)) :-
    module_property(M, file(_)).
has_source(M:Name/Arity) :-
    functor(Head, Name, Arity),
    predicate_property(M:Head, file(_)).
has_source(file(File)) :-
    exists_file(File).
has_source(clause(Ref)) :-
    clause_property(Ref, file(_)).

%   focus_labels(+Window, -Labels)
%
%   Labels are the labels of the path from the root to the focus.

focus_labels(W, Labels) :-
    get(W?chart, focus_node, Focus),
    Focus \== @nil,
    !,
    get(Focus, path, Chain),
    chain_list(Chain, Nodes),
    maplist(node_label, Nodes, Labels).
focus_labels(_, []).

node_label(Node, Label) :-
    get(Node?label, value, Label).

follow_labels([], Node, Node).
follow_labels([Label|Labels], Node, Focus) :-
    get(Node, children, Children),
    Children \== @nil,
    get(Children, find, @arg1?label == Label, Child),
    follow_labels(Labels, Child, Focus).
