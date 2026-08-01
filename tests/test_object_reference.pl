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

:- module(test_object_reference, [test_object_reference/0]).
:- encoding(utf8).

/** <module> Tests for XPCE object references

An anonymous reference is the blob <pce>(0xADDR,Class); a named reference
is the term @Name.  These tests cover the representation, the ownership it
carries (Prolog holding a reference keeps the object alive, dropping it
lets the object go) and the released state left behind by free/1.

Run with:

    swipl -g test_object_reference -t halt \
          packages/xpce/tests/test_object_reference.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_object_reference :-
    run_tests([ reference_representation,
                reference_identity,
                reference_conversion,
                reference_lifetime,
                reference_free,
                reference_as_xref_source,
                weak_reference
              ]).

%!  reclaim is det.
%
%   Run a full reclamation cycle: drop unreachable blobs, then give XPCE a
%   chance to drain the objects the atom garbage collector handed back.
%   The drain happens on entry to any send/2 or get/3.

reclaim :-
    garbage_collect,
    garbage_collect_atoms,
    ignore(get(@pce, version, _)).

%!  written(+Term, -Atom) is det.

written(Term, Atom) :-
    format(atom(Atom), '~q', [Term]).


:- begin_tests(reference_representation).

test(anonymous_is_a_blob, [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    assertion(blob(P, pce)),
    assertion(\+ atom(P)),              % as for any non-text blob
    assertion(atomic(P)),
    assertion(\+ compound(P)).

test(named_is_a_compound, [cleanup(free(@tor_named))]) :-
    new(@tor_named, point(1,2)),
    @tor_named = @Name,
    assertion(atom(Name)),
    assertion(compound(@tor_named)).

test(written_form, [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    written(P, Atom),
    assertion(sub_atom(Atom, 0, _, _, '<pce>(0x')),
    assertion(sub_atom(Atom, _, _, 0, ',point)')).

test(written_form_names_the_class,
     [setup(new(C, chain)), cleanup(free(C))]) :-
    written(C, Atom),
    assertion(sub_atom(Atom, _, _, 0, ',chain)')).

test(is_object_reference, [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    assertion(is_object_reference(P)),
    assertion(is_object_reference(@pce)),
    assertion(\+ is_object_reference(point(1,2))),
    assertion(\+ is_object_reference(foo)),
    assertion(\+ is_object_reference(@(1))).

:- end_tests(reference_representation).


:- begin_tests(reference_identity).

test(same_object_same_blob, [setup(new(C, chain)), cleanup(free(C))]) :-
    new(P, point(1,2)),
    send(C, append, P),
    get(C, head, P2),
    assertion(P == P2).                 % PL_BLOB_UNIQUE

test(distinct_objects_differ,
     [setup((new(A, point(1,2)), new(B, point(1,2)))),
      cleanup((free(A), free(B)))]) :-
    assertion(A \== B).

test(usable_as_a_key, [setup(new(C, chain)), cleanup(free(C))]) :-
    new(P, point(1,2)),
    send(C, append, P),
    get(C, head, P2),
    assertion(memberchk(P2, [P])),
    sort([P,P2], Sorted),
    assertion(Sorted == [P]).           % compare/3 agrees with ==/2

test(sorting_is_total, [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    msort([P, @pce, @nil, foo, 1], Sorted),
    assertion(length(Sorted, 5)),
    assertion(is_list(Sorted)).

:- end_tests(reference_identity).


:- begin_tests(reference_conversion).

%  An anonymous object has no identity besides its handle: there is no
%  integer to hand out, because an address can go stale.

test(anonymous_has_no_reference,
     [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    assertion(\+ object_reference(P, _)).

%  Nor can one be fabricated.  <-object_reference still answers the
%  XPCE-level integer, but @Integer is not a reference Prolog accepts.

test(integer_is_not_a_reference,
     [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    get(P, object_reference, Int),
    assertion(integer(Int)),
    assertion(\+ object(@Int)),
    assertion(\+ is_object_reference(@Int)),
    catch(get(@Int, x, _), error(Formal, _), true),
    assertion(Formal = type_error(pce(object), _)).

test(object_reference_of_named, [cleanup(free(@tor_conv))]) :-
    new(@tor_conv, point(1,2)),
    object_reference(@tor_conv, Ref),
    assertion(Ref == tor_conv).

test(round_trip_of_a_name, [cleanup(free(@tor_conv3))]) :-
    new(@tor_conv3, point(1,2)),
    object_reference(@tor_conv3, Ref),
    object_from_reference(Ref, Obj),
    assertion(Obj == @tor_conv3).

test(an_address_does_not_resolve,
     [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    get(P, object_reference, Int),
    assertion(\+ object_from_reference(Int, _)).

test(round_trip_named, [cleanup(free(@tor_conv2))]) :-
    new(@tor_conv2, point(1,2)),
    object_from_reference(tor_conv2, Obj),
    assertion(Obj == @tor_conv2).

test(blob_resolve, [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    written(P, Atom),
    term_string(Back, Atom, [blob(resolve)]),
    assertion(Back == P).

%  An address that resolves to nothing degrades to a dead blob.  It still
%  answers blob/2 with `pce', because that is read back from the text, so
%  it is still a reference: it just does not denote an object.

test(blob_resolve_of_stale_address) :-
    term_string(Dead, '<pce>(0xdeadbeef,point)', [blob(resolve)]),
    assertion(current_blob(Dead, unavailable)),
    assertion(\+ object(Dead)),
    assertion(\+ catch(object_reference(Dead, _), _, fail)).

test(send_and_get_accept_a_blob,
     [setup(new(P, point(1,2))), cleanup(free(P))]) :-
    send(P, x, 42),
    get(P, x, X),
    assertion(X == 42).

test(a_blob_is_a_legal_argument,
     [setup((new(C, chain), new(P, point(1,2)))),
      cleanup((free(C), free(P)))]) :-
    send(C, append, P),                 % not wrapped as a Prolog term
    get(C, head, H),
    assertion(H == P),
    assertion(object(H)).

:- end_tests(reference_conversion).


:- begin_tests(reference_lifetime).

%  Holding the blob is what keeps the object alive; the answer stack no
%  longer pins anything that escapes to Prolog.

test(answer_stack_does_not_grow) :-
    get(@pce, answer_stack_size, Before),
    forall(between(1, 1000, _), new(_, point(1,2))),
    get(@pce, answer_stack_size, After),
    assertion(After == Before).

test(held_object_survives_gc, [cleanup(free(P))]) :-
    new(P, point(1,2)),
    reclaim,
    assertion(object(P)),
    assertion(get(P, x, 1)).

test(slot_referenced_object_survives_gc, [cleanup(free(C))]) :-
    new(C, chain),
    make_and_append(C),
    reclaim,
    assertion(get(C, head, _)).

test(dropped_object_is_reclaimed) :-
    freed_points(F0),
    make_point,
    reclaim,
    freed_points(F1),
    assertion(F1 > F0).

freed_points(N) :-
    get(@pce, convert, point, class, Class),
    get(Class, no_freed, N).

test(transient_objects_do_not_accumulate) :-
    forall(between(1, 5000, _), new(_, point(1,2))),
    reclaim,
    aggregate_all(count, current_blob(_, pce), N),
    assertion(N =< 10).                 % only what this frame still holds

test(object_survives_done, [cleanup(free(P))]) :-
    new(P, point(1,2)),
    send(P, done),                      % would free a virgin object
    assertion(object(P)).

:- end_tests(reference_lifetime).

%  Helpers that let a reference go out of scope.  The blob must not be
%  reachable from the calling frame for the atom GC to collect it.

make_point :-
    new(_, point(1,2)).

make_and_append(C) :-
    new(P, point(1,2)),
    send(C, append, P).



:- begin_tests(reference_free).

test(free_releases_the_blob) :-
    new(P, point(1,2)),
    free(P),
    assertion(blob_released(P)),
    assertion(blob(P, pce)).

test(freed_reference_is_not_an_object) :-
    new(P, point(1,2)),
    free(P),
    assertion(\+ object(P)).

test(freed_reference_prints_as_freed) :-
    new(P, point(1,2)),
    free(P),
    written(P, Atom),
    assertion(Atom == '<pce>(freed)').

%  ->free() destroys the object without releasing the handle, so the two
%  routes leave different blob states.  They must still look the same: the
%  reference does not denote an object either way.

test(both_free_routes_look_alike) :-
    new(A, point(1,2)),
    send(A, free),
    written(A, ViaSend),
    new(B, point(1,2)),
    free(B),
    written(B, ViaFree),
    assertion(ViaSend == ViaFree),
    assertion(ViaSend == '<pce>(freed)'),
    assertion(\+ object(A)),
    assertion(\+ blob_released(A)),      % but the blob is still there
    assertion(blob_released(B)).

test(using_a_freed_reference_raises,
     [throws(error(existence_error(pce(object), _), _))]) :-
    new(P, point(1,2)),
    free(P),
    get(P, x, _).

test(free_is_idempotent) :-
    new(P, point(1,2)),
    free(P),
    free(P).

%  free/1 must test the association rather than resolve it: resolving an
%  undefined global runs the pce_global/2 trap, so freeing one that does
%  not exist would create it, breaking the ":- free(@name)" idiom used
%  throughout the library.  Compiling the library exercises that; it is not
%  reproducible here because pce_global/2 records the calling module, which
%  under plunit is the unit module rather than this one.

test(free_of_a_missing_object) :-
    free(@tor_no_such_object).


test(free_of_a_named_object) :-
    new(@tor_free, point(1,2)),
    free(@tor_free),
    assertion(\+ object(@tor_free)).

:- end_tests(reference_free).


%  An object reference is its own cross-referencer source identifier: it is
%  opaque, so it indexes as an atom does, and it cannot go stale the way the
%  integer it used to be reduced to could.

:- begin_tests(reference_as_xref_source).

:- use_module(library(pce_prolog_xref)).
:- use_module(library(prolog_xref)).

tor_xref_buffer(TB) :-
    new(TB, text_buffer),
    send(TB, insert, 0, ':- module(tor_m, [foo/0]).\nfoo :- bar.\nbar.\n').

test(object_is_its_own_identifier, [setup(tor_xref_buffer(TB)), cleanup(free(TB))]) :-
    prolog:xref_source_identifier(TB, Id),
    assertion(Id == TB).

test(hook_declines_other_sources) :-
    assertion(\+ prolog:xref_source_identifier('/tmp/tor.pl', _)),
    assertion(\+ prolog:xref_source_identifier(library(lists), _)),
    assertion(\+ prolog:xref_source_identifier(foo, _)).

test(xref_a_buffer, [setup(tor_xref_buffer(TB)), cleanup(free(TB))]) :-
    xref_source(TB, [silent(true)]),
    assertion(xref_current_source(TB)),
    assertion(xref_defined(TB, foo, _)),
    xref_clean(TB),
    assertion(\+ xref_current_source(TB)).

%  xref_clean/1 re-canonicalises, so it must still work once the buffer is
%  gone.  Guarding on object/1 rather than is_object_reference/1 left the
%  data unreclaimable.

test(clean_after_free) :-
    tor_xref_buffer(TB),
    xref_source(TB, [silent(true)]),
    free(TB),
    xref_clean(TB),
    assertion(\+ xref_current_source(TB)).

:- end_tests(reference_as_xref_source).


%  A tool that monitors the object base must be able to hold a handle
%  without extending the object's life.  hash_table(Buckets, none) is that
%  handle: it creates no reference, so the object stays collectable.

:- begin_tests(weak_reference).

freed_points(N) :-
    get(@pce, convert, point, class, Class),
    get(Class, no_freed, N).

add_point(Table) :-
    new(P, point(1,2)),
    send(Table, append, P, marker).

test(weak_table_does_not_own_its_entries) :-
    new(T, hash_table(@default, none)),
    freed_points(F0),
    add_point(T),
    garbage_collect, garbage_collect_atoms, ignore(get(@pce, version, _)),
    freed_points(F1),
    assertion(F1 > F0).

test(normal_table_owns_its_entries) :-
    new(T, hash_table),
    freed_points(F0),
    add_point(T),
    garbage_collect, garbage_collect_atoms, ignore(get(@pce, version, _)),
    freed_points(F1),
    assertion(F1 == F0).

test(weak_table_still_looks_up,
     [setup((new(T, hash_table(@default, none)), new(P, point(1,2)))),
      cleanup(free(P))]) :-
    send(T, append, P, marker),
    assertion(get(T, member, P, marker)),
    assertion(get(P, references, 1)).       % the Prolog handle only

test(default_table_is_referring) :-
    new(T, hash_table),
    assertion(get(T, slot, refer, both)).

:- end_tests(weak_reference).
