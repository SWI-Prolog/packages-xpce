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

:- module(test_lsp_offset, [test_lsp_offset/0]).

/** <module> text_buffer <-lsp_offset

LSP positions are a line and a character offset in UTF-16 units.
text_buffer<-lsp_offset translates them to an index.  It remembers the
line it found, so lookups that move towards the end of the buffer scan
each line once; changes before that line reset it.  These tests compare
it to a translation computed from the buffer contents, also while the
buffer is edited.

Run with:

    swipl -g test_lsp_offset -t halt packages/xpce/tests/test_lsp_offset.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(apply)).
:- use_module(library(yall)).

test_lsp_offset :-
    run_tests([ lsp_offset ]).

%!  ref_offset(+Codes, +Line, +Char, -Offset)
%
%   Reference translation: the start of Line (the end of the text if
%   there are fewer lines), advanced by Char UTF-16 units.

ref_offset(Codes, Line, Char, Offset) :-
    line_start(Codes, 0, Line, 0, Start, Rest),
    advance(Rest, Char, Start, Offset).

line_start(Codes, L, L, I, I, Codes) :- !.
line_start([], _, _, I, I, []) :- !.
line_start([0'\n|T], L0, L, I0, I, Rest) :- !,
    L1 is L0+1, I1 is I0+1,
    line_start(T, L1, L, I1, I, Rest).
line_start([_|T], L0, L, I0, I, Rest) :-
    I1 is I0+1,
    line_start(T, L0, L, I1, I, Rest).

advance(_, Units, I, I) :- Units =< 0, !.
advance([C|T], Units, I0, I) :-
    (   C > 0xffff -> U is Units-2 ; U is Units-1 ),
    I1 is I0+1,
    advance(T, U, I1, I).

%!  units(+Codes, -Units)
%
%   Length of Codes in UTF-16 units.

units(Codes, Units) :-
    foldl([C,U0,U]>>(C > 0xffff -> U is U0+2 ; U is U0+1), Codes, 0, Units).

lines(Codes, Lines) :-
    split_codes(Codes, Lines).

split_codes(Codes, [Line|More]) :-
    (   append(Line, [0'\n|Rest], Codes)
    ->  split_codes(Rest, More)
    ;   Line = Codes,
        More = []
    ).

%!  check(+TB, +Line, +Char) is semidet.
%
%   True if <-lsp_offset agrees with the reference.

check(TB, Line, Char) :-
    get(TB, contents, string(S)),
    string_codes(S, Codes),
    ref_offset(Codes, Line, Char, Expected),
    get(TB, lsp_offset, Line, Char, Offset),
    (   Offset == Expected
    ->  true
    ;   format(user_error, "lsp_offset(~w,~w): ~w, expected ~w~n",
               [Line, Char, Offset, Expected]),
        fail
    ).

%!  random_lookup(+TB) is semidet.
%
%   Look up a random position: mostly forward, sometimes backward or
%   beyond the last line.  Characters stay within the line.

random_lookup(TB) :-
    get(TB, contents, string(S)),
    string_codes(S, Codes),
    lines(Codes, Lines),
    length(Lines, NLines),
    random_between(0, NLines, Line),     % NLines: beyond the last line
    (   nth0(Line, Lines, LineCodes)
    ->  units(LineCodes, Max),
        random_between(0, Max, Char)
    ;   Char = 0
    ),
    check(TB, Line, Char).

%!  random_edit(+TB)
%
%   Insert or delete some text, possibly with newlines and characters
%   above U+FFFF.

random_edit(TB) :-
    get(TB, size, Size),
    random_between(0, Size, Where),
    (   maybe(0.5), Size > 0
    ->  Max is min(5, Size-Where),
        random_between(0, Max, Len),
        send(TB, delete, Where, Len)
    ;   random_member(Text, ["x", "\n", "a\nb", "\x1F600\", "\x1F600\\n", "\n\n"]),
        send(TB, insert, Where, Text)
    ).

random_text(Lines, Text) :-
    length(Ls, Lines),
    maplist(random_line, Ls),
    atomic_list_concat(Ls, '\n', Text).

random_line(Line) :-
    random_between(0, 12, Len),
    length(Cs, Len),
    maplist([C]>>random_member(C, [0'a, 0'b, 0' , 0x3B1, 0x1F600]), Cs),
    atom_codes(Line, Cs).

line_offset(TB, Line, Offset) :-
    get(TB, lsp_offset, Line, Offset).

char_offset(TB, Line, Char, Offset) :-
    get(TB, lsp_offset, Line, Char, Offset).

:- begin_tests(lsp_offset).

test(lines, Offsets == [0, 4, 8, 10]) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    maplist(line_offset(TB), [0,1,2,3], Offsets).
test(character, Offsets == [5, 6, 7]) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    maplist(char_offset(TB, 1), [1,2,3], Offsets).
test(utf16, Offsets == [4, 5, 6, 7]) :-
    new(TB, text_buffer("abc\n\x1F600\\x1F600\x\n")),
    maplist(char_offset(TB, 1), [0,2,4,5], Offsets).
test(backward, Offsets == [8, 4, 0, 8]) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    maplist(line_offset(TB), [2,1,0,2], Offsets).
test(beyond_last_line, Offsets == [10, 4]) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    get(TB, lsp_offset, 7, O1),
    get(TB, lsp_offset, 1, O2),
    Offsets = [O1, O2].
test(beyond_then_append, O == 15) :-  % past the end is not remembered
    new(TB, text_buffer("abc\ndef\ngh")),
    get(TB, lsp_offset, 7, _),
    send(TB, append, "\n\n\n\n\nxyz"),
    get(TB, lsp_offset, 7, O).
test(insert_before, O == 7) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    get(TB, lsp_offset, 2, _),
    send(TB, insert, 0, "x\ny"),        % adds a line before line 2
    get(TB, lsp_offset, 2, O).
test(delete_before, O == 7) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    get(TB, lsp_offset, 2, _),
    send(TB, delete, 3, 1),             % joins lines 0 and 1
    get(TB, lsp_offset, 1, O).
test(change_after, O == 8) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    get(TB, lsp_offset, 2, _),
    send(TB, insert, 9, "\n\n"),
    get(TB, lsp_offset, 2, O).
test(insert_at_line_start, O == 8) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    get(TB, lsp_offset, 2, _),
    send(TB, insert, 8, "x\n"),
    get(TB, lsp_offset, 2, O).
test(clear, O == 0) :-
    new(TB, text_buffer("abc\ndef\ngh")),
    get(TB, lsp_offset, 2, _),
    send(TB, clear),
    get(TB, lsp_offset, 2, O).
test(random) :-
    set_random(seed(42)),
    forall(between(1, 20, _),
           ( random_text(30, Text),
             new(TB, text_buffer(Text)),
             forall(between(1, 200, _),
                    (   maybe(0.3)
                    ->  random_edit(TB)
                    ;   assertion(random_lookup(TB))
                    )),
             free(TB)
           )).

:- end_tests(lsp_offset).
