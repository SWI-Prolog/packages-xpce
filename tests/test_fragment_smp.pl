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

:- module(test_fragment_smp, [test_fragment_smp/0]).
:- encoding(utf8).

/** <module> Tests for fragment positions across supplementary-plane
    code points

Verifies that text_buffer indexing and fragment start/length values
remain in code-point units regardless of platform wchar_t width.  On
Windows this used to break the moment an SMP code point (emoji, CJK
Ext B, etc.) entered the buffer, because charW was a 16-bit UTF-16
unit and a single code point took two slots.

Run with:

    swipl -g test_fragment_smp -t halt \
          packages/xpce/tests/test_fragment_smp.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_fragment_smp :-
    run_tests([fragment_smp, fragment_save_load]).

%!  buffer_with(+Text, -TB)
%
%   Build a fresh text_buffer holding the given Prolog string.  Use
%   "..." with \uXXXX / \UXXXXXXXX escapes to get exact code points.

buffer_with(Text, TB) :-
    new(TB, text_buffer),
    send(TB, append, Text).

:- begin_tests(fragment_smp).

test(size_in_code_points) :-
    %   "ab🌏c"  →  4 code points (a b emoji c)
    buffer_with("ab\U0001F30Fc", TB),
    get(TB, size, 4).

test(character_at_smp_position) :-
    %   buffer:  a b 🌏 c
    %   index:   0 1 2  3
    buffer_with("ab\U0001F30Fc", TB),
    get(TB, character, 2, 0x1F30F).

test(character_after_smp) :-
    %   The slot AFTER the emoji must be 'c', not a surrogate trail.
    buffer_with("ab\U0001F30Fc", TB),
    get(TB, character, 3, 0'c).

test(fragment_start_after_smp) :-
    %   A fragment created with start=3, length=1 must point at 'c',
    %   not at a stray UTF-16 unit inside the emoji.
    buffer_with("ab\U0001F30Fcd", TB),
    new(F, fragment(TB, 3, 1)),
    get(F, start, 3),
    get(F, length, 1),
    get(F, string, S),
    get(S, value, c).

test(fragment_spans_smp) :-
    %   Fragment covering the emoji and a trailing ASCII char: start=2,
    %   length=2 → the emoji + 'c'.  Length is in code points.
    buffer_with("ab\U0001F30Fcd", TB),
    new(F, fragment(TB, 2, 2)),
    get(F, length, 2),
    get(F, string, S),
    get(S, size, 2).

test(fragment_after_cjk_ext_b) :-
    %   CJK Extension B: U+20000.  Same shape as the emoji case but a
    %   different SMP block.
    buffer_with("x\U00020000yz", TB),
    new(F, fragment(TB, 2, 2)),
    get(F, start, 2),
    get(F, string, S),
    get(S, value, yz).

test(fragment_shifts_on_insert) :-
    %   Insert two ASCII chars at offset 0: a fragment originally at 3
    %   must move to 5 — including across an emoji that came after.
    buffer_with("ab\U0001F30Fcd", TB),
    new(F, fragment(TB, 3, 1)),
    send(TB, insert, 0, "XY"),
    get(F, start, 5),
    get(F, length, 1).

test(fragment_shifts_when_smp_inserted_before) :-
    %   Inserting an SMP code point before the fragment should shift it
    %   by exactly 1 code point, not 2 (which is the UTF-16 unit count).
    buffer_with("abcd", TB),
    new(F, fragment(TB, 2, 1)),     % originally pointing at 'c'
    send(TB, insert, 0, "\U0001F30F"),
    get(F, start, 3),                % shifted by 1 code point
    get(F, string, S),
    get(S, value, c).

test(many_smp_fragments) :-
    %   Stress: 10 contiguous SMP code points with a fragment over
    %   each.  Validates that shift_fragments arithmetic is in
    %   code-point units.
    buffer_with("\U0001F300\U0001F301\U0001F302\U0001F303\U0001F304\c
                 \U0001F305\U0001F306\U0001F307\U0001F308\U0001F309",
                TB),
    forall(between(0, 9, I),
           ( new(F, fragment(TB, I, 1)),
             get(F, start, I),
             get(F, length, 1)
           )).

:- end_tests(fragment_smp).


:- begin_tests(fragment_save_load).

test(save_load_roundtrip_smp) :-
    %   Save a buffer with SMP content to a temp file, load it back,
    %   verify size and characters survive.
    tmp_file(smp_test, F),
    setup_call_cleanup(
        (   buffer_with("a\U0001F30Fb\U00020000c", TB),
            send(TB, save, file(F))
        ),
        (   new(TB2, text_buffer),
            send(TB2, insert_file, 0, file(F)),
            get(TB2, size, Size),
            get(TB2, character, 1, C1),
            get(TB2, character, 3, C3)
        ),
        catch(delete_file(F), _, true)),
    Size =:= 5,
    C1 =:= 0x1F30F,
    C3 =:= 0x20000.

:- end_tests(fragment_save_load).
