/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(pce_debug,
        [ debugpce/0
        , debugpce/1
        , nodebugpce/0
        , nodebugpce/1
        , tracepce/1                    % Trace a pce method
        , notracepce/1                  % UnTrace a pce method
        , spypce/1                      % Trace a pce method
        , nospypce/1                    % UnTrace a pce method
        , checkpce/0                    % Check all global pce objects
        , show_slots/1                  % Show all pce slot-values
        , pcerefer/1                    % Print objects refering to me
        , pcerefer/2                    % Print objects refering to me
        , pce_global_objects/1          % -globals
        , pce_lost_objects/1            % +Top
        , pce_lost_instances/2          % +Class, -Objects
        ]).
:- use_module(library(pce)).
:- autoload(library(apply)).
:- autoload(library(edinburgh), [debug/0]).
:- autoload(library(pce_meta), [pce_to_method/2]).
:- autoload(library(pce_util), [chain_list/2]).
:- autoload(library(error), [existence_error/2]).
:- autoload(library(lists), [append/3, member/2, numlist/3, sum_list/2]).
:- autoload(library(pairs), [pairs_keys/2]).
:- use_module(library(debug), [debug/3]).

:- set_prolog_flag(generate_debug_info, false).
:- meta_predicate test(0,-).

%!  debugpce is det.
%!  nodebugpce is det.
%
%   Switch xpce debugging facilities.

debugpce :-
    send(@pce, debugging, @on).
nodebugpce :-
    send(@pce, debugging, @off).


%!   debugpce(+Subject) is det.
%!   nodebugpce(+Subject) is det.
%
%   Start/stop printing debugging messages on `Subject'. System maintenance
%   usage only.

debugpce(Subject) :-
    send(@pce, debug_subject, Subject).

nodebugpce(Subject) :-
    send(@pce, nodebug_subject, Subject).

%!  tracepce(+Method) is det.
%!  notracepce(+Method) is det.
%
%   Send a ->trace message to the refered method. This will cause PCE to
%   print the enters, exits or failures of this method. Prints the class
%   and selector on which the tracepoint is actually set (which might be
%   an inherited method).
%
%   @arg Method is one of `Class->Method` or `Class<-Method`.  Note that
%   the `<-` is declared as operator in library(pce).

tracepce(Spec) :-
    method(Spec, Method),
    send(Method, trace, full),
    trace_feedback('Tracing', Method).

notracepce(Spec) :-
    method(Spec, Method),
    send(Method, trace, full, @off),
    trace_feedback('Stopped tracing', Method).

%!  spypce(+Method) is det.
%!  nospypce(+Method) is det.
%
%   Put a spy-point on the Prolog implementation or XPCE method object.
%
%   @see tracepce/1.

spypce(Spec) :-
    method(Spec, Method),
    send(Method, break, full),
    (   prolog_method(Method)
    ->  debug
    ;   true
    ),
    trace_feedback('Spying', Method).

nospypce(Spec) :-
    method(Spec, Method),
    send(Method, break, full, @off),
    trace_feedback('Stopped spying', Method).

method(Spec, Method) :-
    pce_to_method(Spec, Method),
    send(Method, instance_of, behaviour).


%       succeed if the method is implemented in Prolog (dubious test).

prolog_method(Implementation) :-
    send(Implementation, instance_of, method),
    get(Implementation, message, Msg),
    send(Msg, instance_of, host_data).

trace_feedback(Action, Obj) :-
    (   prolog_method(Obj)
    ->  Type = 'Prolog implementation of'
    ;   get(Obj?class_name, label_name, Type)
    ),
    get(Obj?context, name, ClassName),
    get(Obj, name, Selector),
    get(Obj, access_arrow, Arrow),
    format('~w ~w: ~w ~w~w~n', [Action, Type, ClassName, Arrow, Selector]).


                /********************************
                *       CHECK PCE DATABASE      *
                ********************************/

%!  pce_global_objects(-ChainOfGlobalObjects)
%   Return a chain with all globally known objects.

pce_global_objects(Chain) :-
    new(Chain, chain),
    send(@pce, for_name_reference,
         message(@prolog, '_append_reference', Chain, @arg1)).

'_append_reference'(_, Name) :-
    non_object_reference(Name),
    !.
'_append_reference'(Chain, Name) :-
    send(Chain, '_append', @Name).

non_object_reference('_object_to_itf_table').
non_object_reference('_name_to_itf_table').
non_object_reference('_handle_to_itf_table').

%!  add_prolog_references(+Chain, -PrologRefs, -Reclaimed, -Freed) is
%!                        det.
%
%   Add to Chain the Prolog blob references that Prolog holds on to.
%   See held_reference/1.
%
%   @arg PrologRefs is the number of life Prolog blobs.
%   @arg Freed is the number of blobs that refer to freed PCE objects.

add_prolog_references(Chain, PrologRefs, Reclaimed, Freed) :-
    get(Chain, size, Size0),
    gc_pce_blobs(Reclaimed),
    State = freed(0),
    forall(( current_blob(Ref, pce),
             Ref \== Chain,
             held_reference(Ref),
             existing_object(Ref, State)
           ),
           send(Chain, '_append', Ref)),
    get(Chain, size, AllObjects),
    PrologRefs is AllObjects-Size0,
    arg(1, State, Freed).

gc_pce_blobs(Reclaimed) :-
    aggregate_all(count, current_blob(Ref, pce), Count0),
    garbage_collect_atoms,
    ignore(get(@pce, version, _)),      % hands the references back: see
                                        % pceDrainHostReferences()
    aggregate_all(count, current_blob(Ref, pce), Count1),
    Reclaimed is Count0 - Count1.


%!  held_reference(+Ref) is semidet.
%
%   True if something registered the blob Ref: a clause, a record or
%   foreign code.  A blob that is only on a Prolog stack is left out.
%   The atom GC scans the stacks conservatively: it marks what a thread
%   dropped long ago as well as what is above the top of its local
%   stack.  Such a stale blob keeps its object alive, and taken as a
%   root it shows objects that died with the window that held them as
%   freed objects inside life ones.  The price is that objects held
%   only by a running goal, a global variable or a message queue are
%   not checked.

held_reference(Ref) :-
    '$atom_references'(Ref, Count),
    Count > 0.

existing_object(Ref, _State) :-
    object(Ref),
    !,
    \+ send(Ref, '_instance_of', host_data).
existing_object(_, State) :-
    arg(1, State, Count0),
    Count is Count0+1,
    nb_setarg(1, State, Count),
    fail.

%!  checkpce is semidet.
%
%   Runs  a  recursive  '_check'  on  all  reachable  objects.  See  the
%   reference documentation of `Object ->_check' for details.

checkpce :-
    test(check_pce_database, Status),
    test(check_pce_types, Status),
    test(check_classes, Status),
    test(check_redefined_methods, Status),
    Status = yes.

check_classes :-
    (   pce_expansion:compiling(_, _)
    ->  forall(pce_expansion:compiling(Class, Path),
               ( file_base_name(Path, File),
                 send(@pce, format,
                      '[PCE: WARNING: definition of class \c
                          %s in ~s not closed]\n',
                      Class, File))),
        fail
    ;   true
    ).

check_redefined_methods :-
    findall(S, ( redefined_send_method(S),
                 \+ deliberately_redefined_method(S)), SL),
    maplist(report_redefined_method, SL),
    findall(G, ( redefined_get_method(G),
                 \+ deliberately_redefined_method(G)), GL),
    maplist(report_redefined_method, GL),
    SL == [],
    GL == [].

redefined_send_method(method(Class, Sel, B0, B1)) :-
    pce_principal:pce_lazy_send_method(Sel, Class, B1),
    (   pce_principal:pce_lazy_send_method(Sel, Class, B0)
    ->  B0 \== B1
    ;   fail
    ).
redefined_get_method(method(Class, Sel, B0, B1)) :-
    pce_principal:pce_lazy_get_method(Sel, Class, B1),
    (   pce_principal:pce_lazy_get_method(Sel, Class, B0)
    ->  B0 \== B1
    ;   fail
    ).

deliberately_redefined_method(method(_, _, B0, B1)) :-
    arg(1, B0, Id0),
    arg(1, B1, Id1),
    Id0 \== Id1.

report_redefined_method(method(Class, Sel, B0, B1)) :-
    describe_location(B1, Loc1),
    (   Loc1 = File:Line
    ->  Loc = file(File, Line)
    ;   true
    ),
    print_message(error,
                  error(pce(redefined_method(Class, Sel, B0, B1)),
                        Loc)).

describe_location(Binder, File:Line) :-
    arg(_, Binder, source_location(File, Line)),
    !.
describe_location(_, '<no source>').


%!  check_pce_database
%
%   Find a set of _root_ objects and recursively look for instances that
%   violate their slot type  as  well   as  freed  instances that appear
%   inside life instances.  The root objects are
%
%     - Global objects (e.g., `@display`)
%     - Objects reachable from Prolog _blobs_ of type `pce` that are
%       held by a clause, a record or foreign code.  See
%       held_reference/1.

%       ->free rather than ->done: Prolog holds a reference to a chain it
%       made, so ->done would leave it, and with it everything it
%       collected.  Those objects then show up as lost on the next run.

check_pce_database :-
    pce_root_objects(All),
    send(All, '_check'),
    send(All, free).

%!  pce_root_objects(-Chain) is det.
%
%   Chain holds the root objects used by check_pce_database/0.

pce_root_objects(All) :-
    create_check_objects,
    pce_global_objects(All),
    get(All, size, Globals),
    add_prolog_references(All, PrologRefs, Reclaimed, Freed),
    print_message(information,
                  pce(checking(Globals, PrologRefs, Reclaimed, Freed))).

%   Classes and methods are created lazily.  Create those used by the
%   check and pce_lost_instances/2 before collecting the roots.  Else
%   they are created during the check and reported as lost.

create_check_objects :-
    send(class(error), realise),        % creates @errors
    get(class(object), send_method, error, _),
    get(class(error), send_method, display, _),
    get(class(hash_table), send_method, for_all, _).


                /********************************
                *          LOST OBJECTS         *
                ********************************/

%!  pce_lost_objects(+Top) is det.
%
%   Check the object base from the roots used by checkpce/0 and print
%   the Top classes with the most _lost_ instances.  An instance is lost
%   if it exists (class `<-no_created` minus `<-no_freed`) but was not
%   reached by the check (class `<-no_reachable`).  Use
%   `send(class(Name), record_instances)` before creating the objects
%   and pce_lost_instances/2 to find the lost instances of a class.
%
%   Objects only held by a running goal, a global variable or a message
%   queue are not roots.  Classes of such objects show lost instances.

pce_lost_objects(Top) :-
    pce_classes(Classes),
    pce_root_objects(Roots),
    send(Roots, '_check', @on, @default, @on),  % silent: see pcerefer/1
    findall(Lost-lost(Name, Live, Reached),
            ( member(Class, Classes),
              get(Class, no_created, Created),
              get(Class, no_freed, Freed),
              get(Class, no_reachable, Reached),
              Live is Created-Freed,
              Lost is Live-Reached,
              Lost =\= 0,
              get(Class, name, Name)
            ),
            Pairs),
    send(Roots, free),                  % see check_pce_database/0
    sort(1, @>=, Pairs, Sorted),
    pairs_keys(Sorted, AllLost),
    sum_list(AllLost, Total),
    length(Sorted, Len),
    N is min(Top, Len),
    length(TopPairs, N),
    append(TopPairs, _, Sorted),
    format('~w~t~32|~t~w~10+~t~w~10+~t~w~10+~n',
           [class, live, reached, lost]),
    forall(member(Lost-lost(Name, Live, Reached), TopPairs),
           format('~w~t~32|~t~D~10+~t~D~10+~t~D~10+~n',
                  [Name, Live, Reached, Lost])),
    format('~w~t~32|~t~D~30+~n', ['Total lost', Total]).

%!  pce_lost_instances(+Class, -Objects:list) is det.
%
%   Objects is a list of instances of Class that are not reached from
%   the roots used by checkpce/0.  Requires Class to record its
%   instances using `send(class(Class), record_instances)` before the
%   leaking objects are created.  Only direct instances of Class are
%   considered.

%   The instances are collected in a chain and read from there rather
%   than asserted: asserting a blob registers a reference to it that
%   retracting does not give back (garbage_collect_clauses/0 does),
%   which would pin every object we report.

pce_lost_instances(ClassName, Lost) :-
    get(@pce, convert, ClassName, class, Class),
    get(Class, instances, Instances),
    (   Instances == @nil
    ->  existence_error(recorded_instances, ClassName)
    ;   true
    ),
    new(Checked, hash_table(1000, none)),
    pce_root_objects(Roots),
    send(Roots, '_check', @on, Checked, @on),   % silent: we only want
                                                % the objects it reached
    new(Recorded, chain),
    send(Instances, for_all,          % ->_member: the key is unchecked,
         if(not(message(Checked, '_member', @arg1)),   % a recorded
            message(Recorded, '_append', @arg1))),        % instance is any
    chain_list(Recorded, Lost),
    send(Recorded, free),              % see check_pce_database/0
    send(Roots, free),
    send(Checked, free).

check_pce_types :-
    get(@pce, unresolved_types, Types),
    get(Types, find_all,
        message(@prolog, no_autoload_class, @arg1?context?print_name),
        Unresolved),
    (   send(Unresolved, empty)
    ->  true
    ;   send(@pce, format,
             '[PCE: WARNING: The following type(s) have no associated class:\n'),
        send(Unresolved, for_all,
             message(@pce, format, '\t%N\n', @arg1)),
        send(@pce, format, ']\n')
    ).


no_autoload_class(ClassName) :-
    pce_prolog_class(ClassName), !, fail.
no_autoload_class(ClassName) :-
    pce_autoload:autoload_decl(ClassName, _), !, fail.
no_autoload_class(_).


%!  show_slots(+Reference)
%
%   Show  all   slots of the   named object.  Actually,  this is a
%   terminal version  of   the inspector  tool  provided  with the
%   manual.  Notably used by me if PCE is in such  a bad shape the
%   inspector won't run anymore

show_slots(X) :-
    get(X, '_class', Class),
    get(Class, slots, Slots),
    Max is Slots - 1,
    new(Header, string('%O', X)),
    get(Header, value, HeaderText),
    format('~w~n', [HeaderText]),
    between(0, Max, Slot),
        get(X, '_slot', Slot, Value),
        get(Class, instance_variable, Slot, Var),
        get(Var, name, Name),
        format('~t~8|~w~t~30|~p~n', [Name, Value]),
    fail ; true.


                /********************************
                *             REFER             *
                ********************************/

%!  pcerefer(+Obj) is det.
%
%   Print the objects that hold Obj as a tree.  Each holder is marked
%   `reachable` (from the roots used by checkpce/0), `lost` or `cycle`.
%   The holders of lost holders are printed below them, up to 10 levels.
%   Holders are found through slots, chain cells, vector elements, and
%   hash table values and keys.
%
%   The holders of a lost object are lost as well.  They can only be
%   found if their class records its instances.  To hunt for leaks, run
%   the following before reproducing the leak:
%
%       ?- send(class(object), record_instances, @on, @on).
%
%   A holder that is a function object (such as `?(...)`), a binding
%   (`Name := Value`) or host data is printed as `not followed`: passing
%   such an object as an argument to xpce evaluates it, uses it as a
%   named argument or raises a type error.
%
%   References the object base does not account for are Prolog's, one
%   for every blob.  These are printed as `Prolog reference', saying how
%   many of them are registered (`stack only' if none are).  A clause, a
%   record and foreign code register one, and so does a query that names
%   the object: the toplevel keeps the goals it read and the answers it
%   printed, so asking about an object here registers a reference of its
%   own and asking again adds another.

%   holder(Target, Holder, Kind, Where, Plain): Holder refers to Target.
%   Plain is @off if Holder cannot be passed as an argument.  The
%   holders live in a list rather than in clauses: asserting a blob
%   registers a reference to it that retracting does not give back
%   (garbage_collect_clauses/0 does), which would pin every object we
%   looked at and make each of them look permanently held.  A list on
%   the stack keeps them alive just as well.

pcerefer(Obj) :-
    get(Obj, '_references', Refs),
    format('~p has ~d references~n', [Obj, Refs]),
    recorded_tables(TableChain),
    pce_root_objects(Roots),          % collects the atoms first
    prolog_reference(Obj, ObjRef),    % before anything of ours pins it
    new(Checked, hash_table(1000, none)),
    send(Roots, '_check', @on, Checked, @on),   % silent: we only want
                                                % the objects it reached
    chain_list(TableChain, Tables),
    new(Targets, hash_table(16, none)),
    find_holders([Obj], 10, Roots, TableChain, Checked, Targets,
                 [Roots, Checked, TableChain, Targets|Tables], [Obj],
                 [], Holders),
    print_holders(Obj, Holders, Checked, [Obj], 1),
    include(holds(Obj), Holders, Direct),
    length(Direct, Found),
    print_prolog_reference(ObjRef, 1),
    format('Found ~D holders~n', [Found]),
    send(TableChain, free),            % see check_pce_database/0
    send(Targets, free),
    send(Roots, free),
    send(Checked, free).

holds(Target, holder(Target, _, _, _, _)).

%!  pcerefer(+From, +Obj) is det.
%
%   Print the objects reachable from From that hold Obj.

pcerefer(From, Obj) :-
    get(Obj, '_references', Refs),
    format('~p has ~d references~n', [Obj, Refs]),
    new(Targets, hash_table(4, none)),
    send(Targets, append, Obj, @on),
    new(Result, hash_table(16, none)),
    send(From, '_find_holders', Targets, Result, @on),
    collect_holders(Result, [Targets], [], Holders),
    forall(( member(holder(Obj, Holder, Kind, Where, _), Holders) ),
           print_holder(1, Holder, Kind, Where, '')),
    send(Result, free),
    send(Targets, free).

%!  pce_classes(-Classes:list) is det.
%
%   True when Classes is a list holding all classes.  @classes holds a
%   type object for a class that is only referred to, so the members are
%   sorted out before we read them.

pce_classes(Classes) :-
    new(Chain, chain),
    send(@classes, for_all,
         if(message(@arg2, instance_of, class),
            message(Chain, append, @arg2))),
    chain_list(Chain, Classes),
    send(Chain, free).                 % see check_pce_database/0

%!  recorded_tables(-Tables:chain) is det.
%
%   True when Tables is a chain holding all class<-instances tables.

recorded_tables(Tables) :-
    new(Tables, chain),
    send(@classes, for_all,
         if(and(message(@arg2, instance_of, class),
                @arg2?instances \== @nil),
            message(Tables, append, @arg2?instances))).

%   Breadth-first search for the holders of Frontier.  `->_find_holders`
%   walks the roots and the recorded instances in C and records the
%   references in Result.  Reachable holders and holders that are not
%   plain objects are not expanded.

find_holders([], _, _, _, _, _, _, _, Holders, Holders) :- !.
find_holders(_, 0, _, _, _, _, _, _, Holders, Holders) :- !.
find_holders(Frontier, Depth, Roots, TableChain, Checked, Targets, Ignored,
             Seen, Holders0, Holders) :-
    length(Frontier, Count),
    debug(pce_debug(pcerefer), 'Searching the holders of ~D objects',
          [Count]),
    send(Targets, clear),
    forall(member(T, Frontier), send(Targets, '_append', T, @on)),
    new(Result, hash_table(64, none)),
    send(Roots, '_find_holders', Targets, Result, @on, TableChain),
    collect_holders(Result, [Result|Ignored], Holders0, Holders1),
    send(Result, free),
    findall(H,
            ( member(T, Frontier),
              member(holder(T, H, _, _, @on), Holders1),
              \+ memberchk(H, Seen),
              \+ get(Checked, member, H, _)
            ),
            Hs0),
    sort(Hs0, Hs),
    append(Hs, Seen, Seen1),
    Depth1 is Depth-1,
    find_holders(Hs, Depth1, Roots, TableChain, Checked, Targets, Ignored,
                 Seen1, Holders1, Holders).

%   Result holds 5 entries per reference, keyed 0, 1, ...: holder, kind,
%   where, target and plain.  See `object->_find_holders`.

collect_holders(Result, Ignored, Holders0, Holders) :-
    get(Result, size, Size),
    Refs is Size // 5,
    findall(I, between(1, Refs, I), Is),        % [] if nothing was found
    foldl(collect_nth_holder(Result, Ignored), Is, Holders0, Holders).

collect_nth_holder(Result, Ignored, I, Holders0, Holders) :-
    Base is (I-1)*5,
    (   result_holder(Result, Base, Ignored, Holders0, Holder)
    ->  Holders = [Holder|Holders0]
    ;   Holders = Holders0
    ).

result_holder(Result, Base, Ignored, Holders0,
              holder(Target, Holder, Kind, Where, Plain)) :-
    result_entry(Result, Base, 0, Holder),
    \+ memberchk(Holder, Ignored),
    result_entry(Result, Base, 1, Kind),
    result_entry(Result, Base, 2, Where),
    result_entry(Result, Base, 3, Target),
    result_entry(Result, Base, 4, Plain),
    \+ memberchk(holder(Target, Holder, Kind, Where, _), Holders0).

result_entry(Result, Base, Offset, Value) :-
    Key is Base+Offset,
    get(Result, member, Key, Value).

print_holders(Target, Holders, Checked, Path, Indent) :-
    forall(member(holder(Target, Holder, Kind, Where, Plain), Holders),
           ( holder_state(Holder, Plain, Checked, Path, State),
             print_holder(Indent, Holder, Kind, Where, State),
             (   State == lost
             ->  Indent1 is Indent+1,
                 print_holders(Holder, Holders, Checked, [Holder|Path],
                               Indent1)
             ;   true
             )
           )).

%   Prolog holds a reference for every blob, and the object base knows
%   nothing about it.  We report how many of them are registered: by a
%   clause, a record, foreign code -- or by a query that named the
%   object, as the toplevel keeps the goals and answers it read.  The
%   rest sit on a stack and go away with the next atom garbage
%   collection.  This is read before we name the object ourselves.

prolog_reference(Obj, Registered) :-
    (   \+ object(Obj)
    ->  Registered = none
    ;   '$atom_references'(Obj, Registered)
    ).

print_prolog_reference(none, _) :- !.
print_prolog_reference(0, Indent) :-
    !,
    print_prolog_reference_(Indent, 'stack only').
print_prolog_reference(1, Indent) :-
    !,
    print_prolog_reference_(Indent, 'registered once').
print_prolog_reference(N, Indent) :-
    format(atom(Kind), 'registered ~D times', [N]),
    print_prolog_reference_(Indent, Kind).

print_prolog_reference_(Indent, Kind) :-
    Column is Indent*2,
    format('~*c~w~t~72|~w~n', [Column, 0'\s, 'Prolog reference', Kind]).

holder_state(Holder, _, _, Path, cycle) :-
    memberchk(Holder, Path),
    !.
holder_state(_, @off, _, _, 'not followed') :-
    !.
holder_state(Holder, _, Checked, _, reachable) :-
    get(Checked, member, Holder, _),
    !.
holder_state(_, _, _, _, lost).

print_holder(Indent, Holder, Kind, Where, State) :-
    Column is Indent*2,
    holder_location(Kind, Where, Location),
    format('~*c~w of ~p~t~72|~w~n',
           [Column, 0'\s, Location, Holder, State]).

holder_location(slot, Name, Name) :- !.
holder_location(key, _, value) :- !.    % Holder is a hash table
holder_location(name, _, key) :- !.     % Holder is a hash table
holder_location(Kind, Where, Location) :-
    format(atom(Location), '~w ~w', [Kind, Where]).


                /********************************
                *           UTILITIES           *
                ********************************/

test(Goal, _) :-
    Goal,
    !.
test(_, no).

                 /*******************************
                 *            MESSAGES          *
                 *******************************/


:- multifile
    prolog:message/3.

prolog:message(error(pce(redefined_method(Class, Sel, B0, B1)), _)) -->
    { describe_location(B0, Loc0),
      describe_location(B1, Loc1),
      (   functor(B0, bind_send, _)
      ->  Arrow = (->)
      ;   Arrow = (<-)
      )
    },
    [ '~w: ~w~w~w redefined'-[Loc1, Class, Arrow, Sel], nl,
      '\tFirst definition at ~w'-[Loc0]
    ].
prolog:message(pce(checking(AllObjects, PrologRefs, Reclaimed, Freed))) -->
    [ 'PCE: Checking ~D global objects, ~D Prolog references (~D freed).'-
      [AllObjects, PrologRefs, Freed], nl,
      'PCE: ~D Prolog references from stacks reclaimed'-[Reclaimed]
    ].
