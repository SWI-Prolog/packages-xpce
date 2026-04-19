/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce
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

:- module(test_terminal,
          [ test_terminal/0,
            test_terminal_random/2,              % +Sessions, +CommandsPerSession
            test_terminal_random/3               % +Sessions, +CommandsPerSession, +Options
          ]).
:- encoding(utf8).

/** <module> Integration tests for the xpce terminal + libedit

Drives an epilog terminal end-to-end through its public xpce methods:

    - ->send                injects keystrokes (UTF-8 bytes to the PTY)
    - <-cursor_position     point(col, row) in the visible window
    - <-row                 string content of a visible row

Tests create a fresh epilog frame, type and hit keys, drive the event
loop until output has settled, then assert the cursor position and row
contents.  Each test owns its frame and destroys it in its cleanup
clause.

Because the SWI-Prolog prompt includes the command number ("101 ?- "
rather than just "?- "), column assertions are expressed relative to
the prompt's width on that fresh line.  Each test captures the
prompt position at the start (the cursor's coordinates right after
wait_for_prompt/1) and phrases its expected columns as "P + N" where
N is the visual-column offset inside the input.

Run with:

    swipl -g test_terminal -t halt packages/xpce/tests/test_terminal.pl
*/

:- use_module(library(debug)).

setup_headless :-
    debugging(xpce(gui)),
    !.
setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- initialization(setup_headless, now).

:- use_module(library(plunit)).
:- use_module(library(pce)).
:- use_module(library(epilog)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(option)).
:- use_module(library(random)).

test_terminal :-
    run_tests([ terminal_basic,
                terminal_nfd,
                terminal_regression,
                terminal_wide,
                terminal_non_bmp,
                terminal_mixed,
                terminal_wrap
              ]).


		 /*******************************
		 *       SETUP / TEARDOWN       *
		 *******************************/

%   We create one epilog terminal per test UNIT (via begin_tests/2's
%   setup and cleanup options) and reuse it across the tests of the
%   unit.  Each test's own setup calls reset_input/1 to clear whatever
%   the previous test left on the command line, so tests see a fresh
%   empty prompt without the overhead of spawning a new window.

%!  start_terminal(-Frame, -Terminal) is det.
%
%   Create a fresh epilog terminal and wait for the initial prompt.
%   Frame is the epilog_frame; Terminal is its prolog_terminal.

start_terminal(Frame, Terminal) :-
    epilog([ object(Frame),
             title('test_terminal'),
             rows(25),
             cols(80)
           ]),
    get(Frame, current_terminal, Terminal),
    wait_for_prompt(Terminal).

%!  stop_terminal(+Frame) is det.

stop_terminal(Frame) :-
    (   object(Frame)
    ->  in_pce_thread(send(Frame, destroy))
    ;   true
    ).

%!  setup_unit is det.
%!  cleanup_unit is det.
%
%   Unit-level hooks: open/close the epilog terminal shared by all
%   tests in a PLUnit unit.  The frame and terminal references are
%   stashed in a non-backtrackable global so individual tests can
%   retrieve them through current_test_terminal/1.

setup_unit :-
    start_terminal(Frame, Terminal),
    nb_setval(terminal_test, Frame-Terminal).

cleanup_unit :-
    (   nb_current(terminal_test, Frame-_)
    ->  nb_delete(terminal_test),
        stop_terminal(Frame)
    ;   true
    ).

%!  current_test_terminal(-Terminal) is det.

current_test_terminal(Terminal) :-
    nb_getval(terminal_test, _-Terminal).

%!  test_begin(-Terminal) is det.
%
%   Per-test setup: retrieve the shared terminal and reset its input
%   line so the test starts at a clean empty prompt.

test_begin(Terminal) :-
    current_test_terminal(Terminal),
    reset_input(Terminal).

%!  reset_input(+Terminal) is det.
%
%   Clear the current libedit input line and redraw the prompt.  We
%   go to end-of-line first so ^U (kill-to-start) removes whatever is
%   in front of the cursor regardless of where the previous test
%   left it; ^L then triggers a fresh redisplay.

reset_input(Terminal) :-
    key(Terminal, ctrl_e),
    key(Terminal, ctrl_u),
    key(Terminal, ctrl_l),
    wait_for_prompt(Terminal).


		 /*******************************
		 *   xpce ↔ Prolog CONVERSION   *
		 *******************************/

%!  cursor(+Terminal, -Col, -Row) is det.
%
%   Read the logical cursor position.  <-cursor_position returns an
%   xpce Point object; unpack it into Prolog integers.

cursor(Terminal, Col, Row) :-
    get(Terminal, cursor_position, P),
    get(P, x, Col),
    get(P, y, Row).

%!  row_text(+Terminal, +Row, -Atom) is det.
%
%   Read the content of a visible row as a Prolog atom.  <-row
%   returns an xpce String; pull its value out.

row_text(Terminal, Row, Atom) :-
    get(Terminal, row, Row, Str),
    get(Str, value, Atom).


		 /*******************************
		 *       SYNCHRONISATION        *
		 *******************************/

%!  drive(+Seconds) is det.
%
%   Pump the xpce event loop for the given wall-clock time.  Every
%   helper that needs output to propagate into the terminal calls
%   this, so there is exactly one place that touches pce_dispatch/2.

drive(Seconds) :-
    get_time(Start),
    Deadline is Start + Seconds,
    drive_until(Deadline).

drive_until(Deadline) :-
    get_time(Now),
    (   Now >= Deadline
    ->  true
    ;   Remaining is max(0.01, Deadline-Now),
        Slice is min(0.05, Remaining),
        wait(Slice),
        drive_until(Deadline)
    ).

%!  wait_until(:Goal, +Timeout) is semidet.
%
%   Repeatedly dispatch events and test Goal until it succeeds or
%   Timeout seconds have elapsed.  Fails (does not throw) on timeout
%   so a PLUnit test reports a missed expectation instead of hanging.

:- meta_predicate wait_until(0, +).
wait_until(Goal, Timeout) :-
    get_time(Start),
    Deadline is Start + Timeout,
    wait_until_(Goal, Deadline).

wait_until_(Goal, Deadline) :-
    (   call(Goal)
    ->  true
    ;   get_time(Now),
        Now < Deadline,
        wait(0.05),
        wait_until_(Goal, Deadline)
    ).

%!  wait(+Time) is det.
%
%   Wait for an xpce event while dispatching input.

wait(Time) :-
    pce_principal:pce_dispatch(-1, Time).

%!  wait_for_prompt(+Terminal) is semidet.
%
%   Wait until the row the cursor is on ends with the SWI-Prolog
%   prompt string "?- ".  That is true as soon as Prolog reaches its
%   top-level and libedit has drawn its prompt.

wait_for_prompt(Terminal) :-
    wait_until(at_prompt(Terminal), 15).

at_prompt(Terminal) :-
    cursor(Terminal, _, Row),
    row_text(Terminal, Row, Line),
    atom(Line),
    sub_atom(Line, _, _, 0, '?- ').

%!  prompt_col(+Terminal, -Col) is det.
%
%   The column immediately after the prompt on the current input row
%   (equal to the cursor column right after wait_for_prompt/1).

prompt_col(Terminal, Col) :-
    cursor(Terminal, Col, _).


		 /*******************************
		 *          INPUT HELPERS       *
		 *******************************/

%!  type(+Terminal, +Text) is det.
%
%   Inject Text as if typed.  Drives the event loop briefly so the
%   terminal has a chance to echo.  Text may be an atom or a string.

type(Terminal, Text) :-
    send(Terminal, send, Text),
    drive(0.1).

%!  key(+Terminal, +Name) is det.
%
%   Send a symbolic key.  Uses the byte sequences libedit expects on a
%   VT-style terminal.

key(Terminal, Name) :-
    key_bytes(Name, Bytes),
    atom_codes(Atom, Bytes),
    send(Terminal, send, Atom),
    drive(0.05).

% ctrl bytes --------------------------------------------------------------
key_bytes(ctrl_a,         [0x01]).
key_bytes(ctrl_b,         [0x02]).
key_bytes(ctrl_d,         [0x04]).
key_bytes(ctrl_e,         [0x05]).
key_bytes(ctrl_f,         [0x06]).
key_bytes(ctrl_k,         [0x0B]).
key_bytes(ctrl_l,         [0x0C]).
key_bytes(ctrl_u,         [0x15]).
key_bytes(backspace,      [0x7F]).          % libedit treats DEL as backspace
key_bytes(enter,          [0'\r]).
key_bytes(tab,            [0'\t]).
% Meta = ESC prefix on VT terminals
key_bytes(meta_b,         [0'\e, 0'b]).
key_bytes(meta_f,         [0'\e, 0'f]).
key_bytes(meta_d,         [0'\e, 0'd]).
key_bytes(meta_backspace, [0'\e, 0x7F]).
% ANSI CSI sequences
key_bytes(home,           [0'\e, 0'[, 0'H]).
key_bytes(end,            [0'\e, 0'[, 0'F]).
key_bytes(cursor_up,      [0'\e, 0'[, 0'A]).
key_bytes(cursor_down,    [0'\e, 0'[, 0'B]).
key_bytes(cursor_right,   [0'\e, 0'[, 0'C]).
key_bytes(cursor_left,    [0'\e, 0'[, 0'D]).
key_bytes(delete,         [0'\e, 0'[, 0'3, 0'~]).


		 /*******************************
		 *          ASSERTIONS          *
		 *******************************/

%!  assert_cursor(+Terminal, +Col, +Row) is det.

assert_cursor(Terminal, ExpCol, ExpRow) :-
    cursor(Terminal, Col, Row),
    (   Col =:= ExpCol,
        Row =:= ExpRow
    ->  true
    ;   format(user_error,
               "cursor: expected (~w, ~w), got (~w, ~w)~n",
               [ExpCol, ExpRow, Col, Row]),
        assertion((Col =:= ExpCol, Row =:= ExpRow))
    ).

%!  assert_row(+Terminal, +Row, +Expected) is det.
%
%   Exact match of the row content (atom-compared).

assert_row(Terminal, Row, Expected) :-
    row_text(Terminal, Row, Line),
    (   Line == Expected
    ->  true
    ;   format(user_error,
               "row ~w: expected ~q, got ~q~n", [Row, Expected, Line]),
        assertion(Line == Expected)
    ).

%!  assert_input(+Terminal, +Row, +ExpectedInput) is det.
%
%   Strip the SWI-Prolog prompt "N ?- " from the start of the row, then
%   compare the remainder with ExpectedInput.  Works for any command
%   number N.

assert_input(Terminal, Row, ExpectedInput) :-
    row_text(Terminal, Row, Line),
    strip_prompt(Line, Input),
    (   Input == ExpectedInput
    ->  true
    ;   format(user_error,
               "input row ~w: expected ~q, got ~q (full: ~q)~n",
               [Row, ExpectedInput, Input, Line]),
        assertion(Input == ExpectedInput)
    ).

%!  strip_prompt(+Line, -Rest) is det.
%
%   Drop everything up to and including the "?- " prompt.  If the
%   line does not contain a prompt we return it unchanged — useful
%   for rows that ended up empty.

strip_prompt(Line, Rest) :-
    (   sub_atom(Line, Before, 3, _, '?- ')
    ->  After is Before + 3,
        sub_atom(Line, After, _, 0, Rest)
    ;   Rest = Line
    ).


		 /*******************************
		 *        TEST: BASIC           *
		 *******************************/

:- begin_tests(terminal_basic,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

test(type_ascii, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    type(T, abc),
    assert_input(T, R, abc),
    C is P + 3,
    assert_cursor(T, C, R).

test(home_end, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    type(T, hello),
    End is P + 5,
    assert_cursor(T, End, R),
    key(T, ctrl_a),
    assert_cursor(T, P, R),
    key(T, ctrl_e),
    assert_cursor(T, End, R).

test(kill_to_start, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    type(T, foo),
    key(T, ctrl_u),
    assert_input(T, R, ''),
    assert_cursor(T, P, R),
    type(T, bar),
    assert_input(T, R, bar).

:- end_tests(terminal_basic).


		 /*******************************
		 *        TEST: NFD TEXT        *
		 *******************************/

:- begin_tests(terminal_nfd,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  nfd_word(-Word) is det.
%
%   "àéîõü" in NFD: 5 grapheme clusters, 10 code points.

nfd_word(Word) :-
    atom_codes(Word,
               [ 0'a, 0x300,           % à
                 0'e, 0x301,           % é
                 0'i, 0x302,           % î
                 0'o, 0x303,           % õ
                 0'u, 0x308            % ü
               ]).

test(paste_nfd_renders_5_cols, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    End is P + 5,
    assert_cursor(T, End, R),
    assert_input(T, R, W).

test(cursor_right_steps_one_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    key(T, ctrl_a),
    assert_cursor(T, P, R),
    key(T, cursor_right),
    C1 is P + 1,
    assert_cursor(T, C1, R),           % past 'à'
    key(T, cursor_right),
    C2 is P + 2,
    assert_cursor(T, C2, R).           % past 'é'

test(forward_delete_first_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    key(T, ctrl_a),
    key(T, ctrl_d),
    drive(0.1),
    assert_cursor(T, P, R),
    atom_codes(Rest,
               [ 0'e, 0x301, 0'i, 0x302, 0'o, 0x303, 0'u, 0x308 ]),
    assert_input(T, R, Rest).

test(word_forward_spans_full_cluster_word, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    key(T, ctrl_a),
    key(T, meta_f),
    End is P + 5,
    assert_cursor(T, End, R).

test(delete_word_forward_removes_full_word, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    key(T, ctrl_a),
    key(T, meta_d),
    drive(0.1),
    assert_input(T, R, ''),
    assert_cursor(T, P, R).

:- end_tests(terminal_nfd).


		 /*******************************
		 *      TEST: REGRESSION        *
		 *******************************/

%   Minimised regressions found by test_terminal_random/2,3.  Each
%   test replays the shortest command sequence that reproduces the
%   bug and asserts the display state the model expected.

:- begin_tests(terminal_regression,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%   Inserting an NFD cluster at `home` when the buffer already contains
%   another NFD cluster drops one character from the re-rendered line.
%   Repro: type `abcỳ` (ỳ = 'y'+U+0300 in NFD), press Home, type
%   `à` (NFD a+U+0300).  Expected line: `àabcỳ`; without the fix,
%   libedit's refresh overwrites the 'b' and the line becomes `àacỳ`.

%   Forward-delete of a mid-line NFD cluster must drop the whole
%   cluster (base + combining marks), not just the base.  Before the
%   fix, libedit's re_update_line placed ofd on a combining mark, so
%   the resulting ANSI DCH targeted the NEXT base's column — the
%   preceding cluster's combiner stayed behind, attaching to whatever
%   cluster shifted into that column.
%   Repro: type `f̀fj̀jòǹàz̀` (NFD), home, cursor_right twice, insert
%   `z`, delete (which should remove `j̀`).  Without the fix the line
%   becomes `f̀fzj̀òǹàz̀` — the `j̀`'s combiner survives, attaches to
%   the plain `j`.

%   Forward-delete of a wide (emoji) cluster must drop exactly the 2
%   cells the cluster occupies, not 3.  Before the fix, rlc_delete_chars
%   decremented its column budget by the base cell's width (1) even for
%   a wide base, so a `count=2` DCH walked past the cluster and ate a
%   character from the next cluster.
%   Repro: type 🤩🤩🤩jj, home, cursor_right once, delete (to remove the
%   second 🤩).  Without the fix the line becomes 🤩🤩jnsh̀i instead of
%   🤩🤩jjnsh̀i — the `j` disappears.

%   Typing a character into the middle of a line whose cell count
%   already exceeds the visual width (because it's full of NFD
%   clusters) must not truncate the trailing combining mark.  Before
%   the fix, rlc_insert clamped tl->size at b->width, so inserting a
%   char into a line of 40 NFD clusters (= 80 cells) dropped the last
%   cell — the final cluster's combining mark.

%   When the buffer contains enough NFD combining marks that the
%   total code-point count past the cursor reaches the terminal width,
%   a wide (emoji) cluster after the cursor caused libedit's re_refresh
%   to bump the cursor to the next row — the wrap check compared the
%   code-point index against t_size.h instead of the visual column.
%   Repro: fill a line with ~45 narrow chars plus enough NFD combining
%   marks to push the code-point index past 80, insert an emoji, move
%   cursor back, then type another char to trigger re_refresh.  Before
%   the fix the cursor reported (0, R+1) instead of the correct (col, R).

test(refresh_wide_at_cursor_uses_visual_col, [setup(test_begin(T))]) :-
    cursor(T, _P, R),
    %  Build a buffer: 20 NFD clusters (40 cps, 20 vcols) + emoji (1
    %  cp, 2 vcols) + 20 narrow chars.  Total cps with prompt > 80,
    %  visual cols well under.
    make_nfd_codes(20, NFDCodes),
    append(NFDCodes, [0x1F929], MidCodes),       % NFD then 🤩
    length(Tail, 20), maplist(=(0'q), Tail),
    append(MidCodes, Tail, AllCodes),
    atom_codes(Buf, AllCodes),
    type(T, Buf),
    %  Move left past the trailing q's, stopping just past the emoji,
    %  so the NEXT char under libedit's cursor is the 🤩.  Then type
    %  one more letter to force a re_refresh.
    forall(between(1, 20, _), key(T, cursor_left)),
    type(T, 'k'),
    %  The cursor must still be on row R, not bumped to R+1.
    cursor(T, _, GotRow),
    assertion(GotRow =:= R).

test(insert_midline_preserves_trailing_combiner, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    %  Fill a line with NFD clusters up to just below visual width so
    %  the next insert definitely exceeds b->width in cells but still
    %  fits visually.
    Fill is 80 - P - 2,
    make_nfd_codes(Fill, TypedCodes),
    atom_codes(Typed, TypedCodes),
    type(T, Typed),
    key(T, home),
    key(T, cursor_right),
    type(T, 'z'),
    TailN is Fill - 1,
    make_nfd_codes(TailN, TailCodes),
    atom_codes(Tail, TailCodes),
    atom_codes(OneCluster, [0'a, 0x300]),
    atom_concat(OneCluster, 'z', Prefix),
    atom_concat(Prefix, Tail, Expected),
    assert_input(T, R, Expected).

make_nfd_codes(0, []) :- !.
make_nfd_codes(N, [0'a, 0x300 | T]) :-
    N > 0,
    N1 is N - 1,
    make_nfd_codes(N1, T).

test(delete_wide_cluster_midline, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    atom_codes(Buf, [0x1F929, 0x1F929, 0x1F929,
                     0'j, 0'j, 0'n, 0's]),
    type(T, Buf),
    key(T, home),
    key(T, cursor_right),
    ColBefore is P + 2,
    assert_cursor(T, ColBefore, R),
    key(T, delete),
    assert_cursor(T, ColBefore, R),
    atom_codes(Expected, [0x1F929, 0x1F929,
                          0'j, 0'j, 0'n, 0's]),
    assert_input(T, R, Expected).

test(delete_nfd_cluster_midline, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    atom_codes(Buf, [ 0'f, 0x300, 0'f,
                      0'j, 0x300, 0'j,
                      0'o, 0x300,
                      0'n, 0x300,
                      0'a, 0x300,
                      0'z, 0x300 ]),
    type(T, Buf),
    key(T, home),
    key(T, cursor_right),
    key(T, cursor_right),
    type(T, 'z'),
    ColBeforeDel is P + 3,
    assert_cursor(T, ColBeforeDel, R),
    key(T, delete),
    assert_cursor(T, ColBeforeDel, R),
    atom_codes(Expected, [ 0'f, 0x300, 0'f, 0'z,
                           0'j,
                           0'o, 0x300,
                           0'n, 0x300,
                           0'a, 0x300,
                           0'z, 0x300 ]),
    assert_input(T, R, Expected).

test(insert_nfd_at_home_with_nfd_buffer, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    atom_codes(Ygrave, [0'y, 0x300]),
    atom_codes(Agrave, [0'a, 0x300]),
    atom_concat('abc', Ygrave, Buffer),
    type(T, Buffer),
    Col0 is P + 4,
    assert_cursor(T, Col0, R),
    key(T, home),
    assert_cursor(T, P, R),
    type(T, Agrave),
    Col1 is P + 1,
    assert_cursor(T, Col1, R),
    atom_concat(Agrave, Buffer, Expected),
    assert_input(T, R, Expected).

:- end_tests(terminal_regression).


		 /*******************************
		 *     TEST: WIDE / EMOJI       *
		 *******************************/

:- begin_tests(terminal_wide,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  emoji(-Emoji) is det.
%
%   "🤩️" = U+1F929 + VS-16 (emoji presentation selector), 2 visual cols.

emoji(E) :-
    atom_codes(E, [0x1F929, 0xFE0F]).

test(emoji_is_two_columns, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    emoji(E),
    type(T, E),
    End is P + 2,
    assert_cursor(T, End, R).

test(cursor_left_skips_emoji_as_cluster, [setup(test_begin(T))]) :-
    %  libedit's cursor-left steps by one grapheme cluster, so a wide
    %  emoji is crossed in a single hop (no stop on its right half).
    cursor(T, P, R),
    emoji(E),
    type(T, E),
    End is P + 2,
    assert_cursor(T, End, R),
    key(T, cursor_left),
    assert_cursor(T, P, R).                 % back to before the emoji

:- end_tests(terminal_wide).


		 /*******************************
		 *     TEST: NON-BMP (SMP)      *
		 *******************************/

%   Regression tests for supplementary-plane (U+10000+) code points on
%   Windows, where wchar_t is 16-bit UTF-16 and a single code point
%   occupies two wchar_t slots as a surrogate pair.  Linux stores the
%   same code point in one wchar_t, so these tests pin both platforms
%   to the same observable behaviour.

:- begin_tests(terminal_non_bmp,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  smp_char(-Code) is det.
%
%   Plain U+1F929 "star-struck" emoji, no VS-16 — a single non-BMP code
%   point that renders as two visual columns.

smp_char(0x1F929).

test(smp_types_two_columns, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    smp_char(C),
    atom_codes(A, [C]),
    type(T, A),
    End is P + 2,
    assert_cursor(T, End, R).

test(smp_backspace_removes_whole_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    smp_char(C),
    atom_codes(A, [C]),
    type(T, A),
    key(T, backspace),
    drive(0.1),
    assert_cursor(T, P, R),                 % 2 visual cols gone
    atom_codes(Empty, []),
    assert_input(T, R, Empty).              % buffer emptied

test(smp_cursor_left_is_one_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    smp_char(C),
    atom_codes(A, [C]),
    type(T, A),
    End is P + 2,
    assert_cursor(T, End, R),
    key(T, cursor_left),
    assert_cursor(T, P, R).                 % one hop across the pair

test(smp_delete_forward_is_one_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    smp_char(C),
    atom_codes(A, [C]),
    type(T, A),
    key(T, home),
    assert_cursor(T, P, R),
    key(T, delete),
    drive(0.1),
    assert_cursor(T, P, R),
    atom_codes(Empty, []),
    assert_input(T, R, Empty).

test(smp_midline_insert, [setup(test_begin(T))]) :-
    %   Type an ASCII context, step the cursor into the middle of it,
    %   then insert a non-BMP cluster.  Verifies the pair lands in the
    %   buffer as a single cluster and the display advances by two
    %   visual columns.
    cursor(T, P, R),
    type(T, abcd),
    End is P + 4,
    assert_cursor(T, End, R),
    key(T, cursor_left),                    % between c and d
    key(T, cursor_left),                    % between b and c
    Mid is P + 2,
    assert_cursor(T, Mid, R),
    smp_char(C),
    atom_codes(Emoji, [C]),
    type(T, Emoji),
    drive(0.1),
    After is P + 4,                         % cursor = Mid + 2 visual cols
    assert_cursor(T, After, R),
    atom_codes(Expected, [0'a, 0'b, C, 0'c, 0'd]),
    assert_input(T, R, Expected).

:- end_tests(terminal_non_bmp).


		 /*******************************
		 *       TEST: MIXED (GOLD)     *
		 *******************************/

:- begin_tests(terminal_mixed,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  mixed_line(-Atom) is det.
%
%   "àéîõü🤩️õàéîõü🤩️" — 15 visual columns.

mixed_line(Atom) :-
    atom_codes(Atom,
               [ 0'a, 0x300,           % à
                 0'e, 0x301,           % é
                 0'i, 0x302,           % î
                 0'o, 0x303,           % õ
                 0'u, 0x308,           % ü
                 0x1F929, 0xFE0F,      % 🤩️
                 0'o, 0x303,           % õ
                 0'a, 0x300,           % à
                 0'e, 0x301,           % é
                 0'i, 0x302,           % î
                 0'o, 0x303,           % õ
                 0'u, 0x308,           % ü
                 0x1F929, 0xFE0F       % 🤩️
               ]).

test(mixed_line_reports_15_cols, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    mixed_line(L),
    type(T, L),
    End is P + 15,
    assert_cursor(T, End, R).

test(cursor_left_from_end_lands_before_emoji, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    mixed_line(L),
    type(T, L),
    End is P + 15,
    assert_cursor(T, End, R),
    key(T, cursor_left),
    Before is P + 13,
    assert_cursor(T, Before, R),            % before final emoji
    key(T, cursor_left),
    BeforeU is P + 12,
    assert_cursor(T, BeforeU, R).           % before the preceding 'ü'

test(insert_before_final_emoji, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    mixed_line(L),
    type(T, L),
    key(T, cursor_left),                    % at col P+13 (before final emoji)
    type(T, z),
    drive(0.1),
    After is P + 14,                        % after the inserted 'z'
    assert_cursor(T, After, R),
    atom_codes(Expected,
               [ 0'a, 0x300, 0'e, 0x301, 0'i, 0x302, 0'o, 0x303,
                 0'u, 0x308, 0x1F929, 0xFE0F,
                 0'o, 0x303,
                 0'a, 0x300, 0'e, 0x301, 0'i, 0x302,
                 0'o, 0x303, 0'u, 0x308,
                 0'z,
                 0x1F929, 0xFE0F
               ]),
    assert_input(T, R, Expected).

:- end_tests(terminal_mixed).


		 /*******************************
		 *      TEST: WRAPPED INPUT     *
		 *******************************/

:- begin_tests(terminal_wrap,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%   Terminal width is 80 columns (see start_terminal/2).  With a prompt
%   of width P (captured per test), the first input row can hold
%   80 - P columns before wrapping to the next row.  When the cursor
%   reaches the edge it moves to column 0 of the next row (no
%   deferred-wrap position is observable through <-cursor_position).

%!  xs(+N, -Atom) is det.

xs(N, Atom) :-
    format(atom(Atom), '~*c', [N, 0'x]).

%!  wrap_emoji(-Emoji) is det.
%
%   Local copy of the emoji atom so this unit does not depend on
%   emoji/1 defined in the terminal_wide unit.

wrap_emoji(E) :-
    atom_codes(E, [0x1F929, 0xFE0F]).

%!  type_await(+Terminal, +Text, +ExpCol, +ExpRow) is det.
%
%   Inject Text and pump events until the cursor reaches
%   (ExpCol, ExpRow) or a timeout.  Succeeds even on timeout — the
%   following assert_cursor/3 will report the actual position for
%   diagnosis.

type_await(T, Text, ExpCol, ExpRow) :-
    type(T, Text),
    ignore(wait_until(cursor_at(T, ExpCol, ExpRow), 5)).

cursor_at(T, C, R) :-
    cursor(T, Cc, Rc),
    Cc =:= C, Rc =:= R.


test(input_fills_first_row_exactly, [setup(test_begin(T))]) :-
    %  Typing exactly 80-P narrow characters fills the row to its
    %  right edge.  On an xenl terminal (and xpce's delayed wrap) the
    %  cursor stays in the pending-wrap state at (80, R); the physical
    %  move to (0, R+1) only happens when the NEXT base arrives.
    cursor(T, P, R),
    Fill is 80 - P,
    xs(Fill, Xs),
    type_await(T, Xs, 80, R),
    assert_cursor(T, 80, R).

test(input_wraps_one_char_past_row, [setup(test_begin(T))]) :-
    %  One extra character past 80-P lands at column 1 of the next row.
    cursor(T, P, R),
    Fill is 80 - P + 1,
    xs(Fill, Xs),
    R2 is R + 1,
    type_await(T, Xs, 1, R2),
    assert_cursor(T, 1, R2).

test(input_wraps_multiple_cols, [setup(test_begin(T))]) :-
    %  Cursor lands at (N - (80 - P), R+1) after typing N chars.
    cursor(T, P, R),
    N = 100,
    xs(N, Xs),
    R2 is R + 1,
    ExpCol is N - (80 - P),
    type_await(T, Xs, ExpCol, R2),
    assert_cursor(T, ExpCol, R2).

test(home_end_on_wrapped_input, [setup(test_begin(T))]) :-
    %  ^A returns to the prompt column on the original row; ^E returns
    %  to the wrapped-line end on the next row.
    cursor(T, P, R),
    N = 100,
    xs(N, Xs),
    R2 is R + 1,
    ExpCol is N - (80 - P),
    type_await(T, Xs, ExpCol, R2),
    key(T, ctrl_a),
    assert_cursor(T, P, R),
    key(T, ctrl_e),
    assert_cursor(T, ExpCol, R2).

test(cursor_right_across_wrap, [setup(test_begin(T))]) :-
    %  Advancing one cursor-right at a time from the prompt: after
    %  (80 - P) right-moves we land at column 0 of the next row.
    cursor(T, P, R),
    Fill is 80 - P + 1,                      % one char beyond end of row
    xs(Fill, Xs),
    R2 is R + 1,
    type_await(T, Xs, 1, R2),
    key(T, ctrl_a),
    assert_cursor(T, P, R),
    Steps is 80 - P,
    %  Send all cursor_rights at once and drive once at the end — far
    %  faster than drive/1 after each individual key.
    key_bytes(cursor_right, Bytes),
    length(Runs, Steps),
    maplist(=(Bytes), Runs),
    append(Runs, All),
    atom_codes(Burst, All),
    send(T, send, Burst),
    drive(0.5),
    assert_cursor(T, 0, R2),
    key(T, cursor_right),
    assert_cursor(T, 1, R2).

test(wide_char_prewraps_at_row_edge, [setup(test_begin(T))]) :-
    %  Fill the row leaving exactly one column empty (cursor at col 79
    %  on row R), then type a wide emoji.  It does not fit in the
    %  remaining single column so it pre-wraps: the last cell of row R
    %  is padded with a space and the emoji lands at columns 0-1 of
    %  row R+1.
    cursor(T, P, R),
    Fill is 80 - P - 1,                      % leave 1 col empty on row R
    xs(Fill, Xs),
    At is P + Fill,
    type_await(T, Xs, At, R),
    assert_cursor(T, At, R),
    wrap_emoji(E),
    R2 is R + 1,
    type_await(T, E, 2, R2),
    assert_cursor(T, 2, R2).


%!  nfd_as(+N, -Atom) is det.
%
%   Atom of N 'à' grapheme clusters (each 'a'+U+0300 — 1 col, 2 cp).

nfd_as(N, Atom) :-
    nfd_codes(N, Codes),
    atom_codes(Atom, Codes).

nfd_codes(0, []) :- !.
nfd_codes(N, [0'a, 0x300 | T]) :-
    N > 0, N1 is N - 1,
    nfd_codes(N1, T).

test(nfd_fills_first_row_exactly,
     [ setup(test_begin(T))
     ]) :-
    %  Typing exactly (80-P) NFD clusters fills the row to its visual
    %  edge.  Same pending-wrap semantics as the narrow fill test:
    %  cursor waits at (80, R) until the next base arrives.  Before
    %  the wrap fix this cursor landed around column 40 because
    %  caret_x (cell index) hit the wrap threshold at the visual
    %  midpoint of the buffer.
    cursor(T, P, R),
    Fill is 80 - P,
    nfd_as(Fill, Atom),
    type_await(T, Atom, 80, R),
    assert_cursor(T, 80, R).

test(nfd_one_cluster_wraps_to_next_row,
     [ setup(test_begin(T))
     ]) :-
    %  (80-P)+1 NFD clusters: last one should land at column 0 of the
    %  next row, cursor at column 1.  Currently fails because the wrap
    %  triggers much earlier and far more than one cluster ends up on
    %  the wrapped row.
    cursor(T, P, R),
    Fill is 80 - P + 1,
    nfd_as(Fill, Atom),
    R2 is R + 1,
    type_await(T, Atom, 1, R2),
    assert_cursor(T, 1, R2).

test(nfd_cluster_kept_whole_at_wrap_boundary,
     [ setup(test_begin(T))
     ]) :-
    %  When typing one NFD cluster more than fits on row R, the extra
    %  cluster must appear as a complete `à` on row R+1 — not a bare
    %  base with a dropped combiner, and not a stray combiner orphaned
    %  at column 0.
    cursor(T, P, R),
    Fill is 80 - P + 1,
    nfd_as(Fill, Atom),
    R2 is R + 1,
    type_await(T, Atom, 1, R2),
    nfd_as(1, OneCluster),
    assert_row(T, R2, OneCluster).

test(cursor_left_across_wrap_nfd,
     [ setup(test_begin(T))
     ]) :-
    %  After filling row R with (80-P) clusters and wrapping one more
    %  onto R+1, two cursor-lefts should land on the last cluster of
    %  row R (column 79).  libedit currently mistracks the NFD column
    %  width after a wrap, so the reported column is P + (80-P)/2
    %  rather than 79.  This is a libedit bug, not an xpce one.
    cursor(T, P, R),
    Fill is 80 - P + 1,
    nfd_as(Fill, Atom),
    R2 is R + 1,
    type_await(T, Atom, 1, R2),
    key(T, cursor_left),
    assert_cursor(T, 0, R2),
    key(T, cursor_left),
    LastCol is 80 - 1,
    assert_cursor(T, LastCol, R).

:- end_tests(terminal_wrap).


		 /*******************************
		 *        RANDOM TESTING        *
		 *******************************/

/** <section> Random testing

    test_terminal_random(+N, +M) runs N independent sessions (each
    starting from a fresh empty input line) of M commands each.  A
    command is either typing one grapheme cluster or sending a libedit
    editing key.  After EVERY command we compare the terminal's reported
    cursor and the content of the input row against a pure-Prolog
    model of where they should be.

    v1 stays on a single input row: the random generator rejects any
    `type` command that would push the cursor past column W-1.  Wrap
    support is a follow-up.

    On the first divergence we print the seed, the full command
    history of the failing session, the expected state and the actual
    state, then throw.  The seed + history is the minimum information
    needed to reproduce or minimise the failure by hand.
*/

%!  test_terminal_random(+N, +M) is det.
%!  test_terminal_random(+N, +M, +Options) is det.
%
%   Run N random sessions of M commands each.  Options:
%
%     - seed(Seed)
%       A term acceptable to set_random/1 (e.g. `random` or an integer
%       seed).  Default: a fresh random state.
%     - verbose(Bool)
%       If true, log every command and verify outcome.
%
%   Throws `terminal_random_failure(Info)` on the first divergence.

test_terminal_random(N, M) :-
    test_terminal_random(N, M, []).

test_terminal_random(N, M, Options) :-
    must_be(positive_integer, N),
    must_be(nonneg, M),
    option(seed(Seed), Options, random),
    option(verbose(Verbose), Options, false),
    set_random(seed(Seed)),
    format("test_terminal_random: seed=~q sessions=~w commands=~w~n",
           [Seed, N, M]),
    setup_call_cleanup(setup_unit,
                       run_random_sessions(N, M, Verbose),
                       cleanup_unit).

run_random_sessions(0, _, _) :- !.
run_random_sessions(N, M, Verbose) :-
    N > 0,
    current_test_terminal(T),
    reset_input(T),
    cursor(T, P, R),
    row_text(T, R, PromptLine),
    State0 = state([], 0),
    (   Verbose == true
    ->  format("==== session ~w: prompt=~q at (~w,~w) ====~n",
               [N, PromptLine, P, R])
    ;   true
    ),
    random_commands(M, T, P, R, PromptLine, State0, [], Verbose),
    N1 is N - 1,
    run_random_sessions(N1, M, Verbose).

random_commands(0, _, _, _, _, _, _, _) :- !.
random_commands(K, T, P, R, Prompt, State0, History, Verbose) :-
    K > 0,
    random_command(State0, P, Cmd),
    apply_model(Cmd, State0, State1),
    History1 = [Cmd|History],
    apply_terminal(Cmd, T),
    wait_verified(T, P, R, Prompt, State1, Outcome),
    (   Outcome == ok
    ->  (   Verbose == true
        ->  format("  ok  ~q~n", [Cmd])
        ;   true
        ),
        K1 is K - 1,
        random_commands(K1, T, P, R, Prompt, State1, History1, Verbose)
    ;   reverse(History1, HistF),
        report_failure(T, P, R, Prompt, State1, HistF, Outcome),
        throw(terminal_random_failure(
                  info{ history: HistF,
                        expected: State1,
                        divergence: Outcome }))
    ).


		 /*******************************
		 *           MODEL              *
		 *******************************/

%   state(Clusters, Cursor):
%   - Clusters: list of cluster(Codes, VCols).  Codes is a list of
%     Unicode code points, VCols is the visual width (1 for narrow
%     and NFD, 2 for wide).
%   - Cursor: integer 0..length(Clusters) — the insertion point.

%!  apply_model(+Command, +State0, -State1) is det.

apply_model(type(Cluster),   state(Cs, I), state(Cs1, I1)) :-
    nth0_insert(I, Cluster, Cs, Cs1),
    I1 is I + 1.
apply_model(cursor_left,     state(Cs, I), state(Cs, I1)) :-
    I1 is max(0, I - 1).
apply_model(cursor_right,    state(Cs, I), state(Cs, I1)) :-
    length(Cs, Len),
    I1 is min(Len, I + 1).
apply_model(home,            state(Cs, _), state(Cs, 0)).
apply_model(end,             state(Cs, _), state(Cs, Len)) :-
    length(Cs, Len).
apply_model(backspace,       state(Cs, I), state(Cs1, I1)) :-
    (   I > 0
    ->  I1 is I - 1,
        nth0_delete(I1, Cs, Cs1)
    ;   Cs1 = Cs, I1 = I
    ).
apply_model(delete,          state(Cs, I), state(Cs1, I)) :-
    length(Cs, Len),
    (   I < Len
    ->  nth0_delete(I, Cs, Cs1)
    ;   Cs1 = Cs
    ).

nth0_insert(0, X, L, [X|L]) :- !.
nth0_insert(N, X, [H|T], [H|T1]) :-
    N > 0, N1 is N - 1,
    nth0_insert(N1, X, T, T1).

nth0_delete(0, [_|T], T) :- !.
nth0_delete(N, [H|T], [H|T1]) :-
    N > 0, N1 is N - 1,
    nth0_delete(N1, T, T1).


		 /*******************************
		 *          LAYOUT              *
		 *******************************/

%!  model_layout(+Clusters, +Cursor, +P, +R, +W,
%!               -CurCol, -CurRow, -RowGroups) is det.
%
%   Lay out Clusters starting at visual column P of row R on a
%   terminal of width W columns, then report where the cursor lands
%   (CurCol, CurRow) and group the clusters by the row they
%   eventually occupy (RowGroups is a list of Row-ClusterList pairs,
%   sorted by Row).
%
%   The cursor at index I sits AFTER the first I clusters — i.e. at
%   the position the (I+1)th cluster would occupy, or at the
%   post-placement tip when I == length(Clusters).  After exactly
%   filling a row the cursor stays at (W, R) pending-wrap; it only
%   physically moves to (0, R+1) once a further cluster actually
%   lands there.

model_layout(Clusters, Cursor, P, R, W, CurCol, CurRow, RowGroups) :-
    length(Before, Cursor),
    append(Before, _, Clusters),
    layout_end_pos(Before, P, R, W, CurCol, CurRow),
    layout_all(Clusters, P, R, W, Placements),
    group_pairs_by_key(Placements, RowGroups).

layout_end_pos([], Col, Row, _, Col, Row).
layout_end_pos([cluster(_, CW)|Cs], Col, Row, W, EndCol, EndRow) :-
    (   Col + CW > W
    ->  NewCol = CW, NewRow is Row + 1
    ;   NewCol is Col + CW, NewRow = Row
    ),
    layout_end_pos(Cs, NewCol, NewRow, W, EndCol, EndRow).

layout_all([], _, _, _, []).
layout_all([C|Cs], Col, Row, W, [PlaceRow-C|Ps]) :-
    C = cluster(_, CW),
    (   Col + CW > W
    ->  PlaceCol = 0, PlaceRow is Row + 1
    ;   PlaceCol = Col, PlaceRow = Row
    ),
    NewCol is PlaceCol + CW,
    layout_all(Cs, NewCol, PlaceRow, W, Ps).

sum_widths([], Acc, Acc).
sum_widths([cluster(_, W)|T], Acc, Sum) :-
    Acc1 is Acc + W,
    sum_widths(T, Acc1, Sum).

%!  model_row_text(+PromptLine, +PromptCol, +PromptRow, +RowNum,
%!                 +RowClusters, -Atom) is det.
%
%   Build the expected text for row RowNum.  For the prompt row we
%   prefix with the captured prompt (truncated to PromptCol chars —
%   what's visible before any input landed).  Other rows contain just
%   the concatenated cluster code points — libedit doesn't pad, and
%   xpce's <-row stops at tl->size, so we match that.

model_row_text(PromptLine, P, PromptRow, RowNum, Clusters, Atom) :-
    clusters_codes(Clusters, Codes),
    atom_codes(InputAtom, Codes),
    (   RowNum =:= PromptRow
    ->  (   sub_atom(PromptLine, 0, P, _, PromptPrefix)
        ->  true
        ;   PromptPrefix = PromptLine
        ),
        atom_concat(PromptPrefix, InputAtom, Atom)
    ;   Atom = InputAtom
    ).

clusters_codes([], []).
clusters_codes([cluster(CodePts, _)|T], Codes) :-
    append(CodePts, Rest, Codes),
    clusters_codes(T, Rest).


		 /*******************************
		 *        RANDOM COMMANDS       *
		 *******************************/

%!  random_command(+State, +PromptCol, -Command) is det.
%
%   Pick a random command.  Heavily biased toward typing so the buffer
%   grows.  Caps total content width at 900 visual columns so the
%   buffer (including prompt) never wraps off the bottom of a 25-row
%   terminal (~11 rows of content leaves plenty of margin).

random_command(state(Cs, Cursor), _P, Cmd) :-
    length(Cs, Len),
    sum_widths(Cs, 0, UsedW),
    MaxContent = 900,
    Remaining is MaxContent - UsedW,
    edit_weights(Len, Cursor, EditWeights),
    (   Remaining >= 2
    ->  Weights = [60-type|EditWeights]
    ;   Weights = EditWeights
    ),
    weighted_pick(Weights, Kind),
    (   Kind == type
    ->  random_typeable(Remaining, Cluster),
        Cmd = type(Cluster)
    ;   Cmd = Kind
    ).

%!  edit_weights(+BufLen, +Cursor, -Weights) is det.
%
%   Available edit commands, tagged with selection weights.  Commands
%   that would be no-ops in the current state are omitted so the pick
%   is more productive.

edit_weights(Len, Cursor, Ws) :-
    findall(Weight-Cmd, edit_candidate(Len, Cursor, Cmd, Weight), Ws0),
    (   Ws0 == []
    ->  Ws = [1-end]                   % always available
    ;   Ws = Ws0
    ).

edit_candidate(_,    Cursor, cursor_left,  10) :- Cursor > 0.
edit_candidate(Len,  Cursor, cursor_right, 10) :- Cursor < Len.
edit_candidate(_,    Cursor, home,          5) :- Cursor > 0.
edit_candidate(Len,  Cursor, end,           5) :- Cursor < Len.
edit_candidate(_,    Cursor, backspace,     8) :- Cursor > 0.
edit_candidate(Len,  Cursor, delete,        8) :- Cursor < Len.

%!  random_typeable(+RemainingCols, -Cluster) is det.

random_typeable(Remaining, Cluster) :-
    (   Remaining >= 2
    ->  Choices = [5-ascii, 3-nfd, 2-wide]
    ;   Choices = [5-ascii, 3-nfd]
    ),
    weighted_pick(Choices, Kind),
    make_cluster(Kind, Cluster).

make_cluster(ascii, cluster([Code], 1)) :-
    random_between(0'a, 0'z, Code).
make_cluster(nfd, cluster([Base, 0x300], 1)) :-
    random_between(0'a, 0'z, Base).
make_cluster(wide, cluster([0x1F929], 2)).  % 🤩

%!  weighted_pick(+Weights, -Choice) is det.
%
%   Weights is a list of Weight-Item pairs.  Pick Item with probability
%   Weight / Total.

weighted_pick(Weights, Choice) :-
    pairs_keys(Weights, Keys),
    sum_list(Keys, Total),
    Total > 0,
    R is random_float * Total,
    pick_weighted(Weights, R, Choice).

pick_weighted([W-Item|Rest], R, Out) :-
    (   R < W
    ->  Out = Item
    ;   R1 is R - W,
        pick_weighted(Rest, R1, Out)
    ).


		 /*******************************
		 *       APPLY TO TERMINAL      *
		 *******************************/

%!  apply_terminal(+Command, +Terminal) is det.

apply_terminal(type(cluster(Codes, _)), T) :-
    atom_codes(Atom, Codes),
    send(T, send, Atom).
apply_terminal(cursor_left,  T) :- send_key(T, cursor_left).
apply_terminal(cursor_right, T) :- send_key(T, cursor_right).
apply_terminal(home,         T) :- send_key(T, home).
apply_terminal(end,          T) :- send_key(T, end).
apply_terminal(backspace,    T) :- send_key(T, backspace).
apply_terminal(delete,       T) :- send_key(T, delete).

send_key(T, Name) :-
    key_bytes(Name, Bytes),
    atom_codes(Atom, Bytes),
    send(T, send, Atom).


		 /*******************************
		 *         VERIFICATION         *
		 *******************************/

%!  wait_verified(+T, +P, +R, +Prompt, +State, -Outcome) is det.
%
%   Drive the event loop in small slices until the terminal matches the
%   model, up to ~1 s.  Returns `ok` on match, or the last observed
%   divergence if libedit never settled to the expected state.

wait_verified(T, P, R, Prompt, State, ok) :-
    between(1, 100, _),
    drive(0.01),
    verify_state(T, P, R, Prompt, State, ok),
    !.
wait_verified(T, P, R, Prompt, State, Outcome) :-
    verify_state(T, P, R, Prompt, State, Outcome).


%!  verify_state(+T, +P, +R, +Prompt, +State, -Outcome) is det.
%
%   Binds Outcome to `ok` when the terminal matches the model, or to a
%   `diff{...}` dict describing what disagreed.  The caller reports and
%   throws on any non-`ok` outcome.

verify_state(T, P, R, Prompt, state(Cs, Cursor), Outcome) :-
    W = 80,
    model_layout(Cs, Cursor, P, R, W, ExpCol, ExpRow, RowGroups),
    cursor(T, Col, Row),
    (   Col =:= ExpCol, Row =:= ExpRow
    ->  verify_rows(T, P, R, Prompt, RowGroups, Outcome)
    ;   collect_row_diffs(T, P, R, Prompt, RowGroups, RowDiffs),
        Outcome = diff{ expected_cursor: (ExpCol,ExpRow),
                        got_cursor:      (Col,Row),
                        row_diffs:       RowDiffs }
    ).

%!  verify_rows(+T, +P, +R, +Prompt, +RowGroups, -Outcome) is det.
%
%   Walk each Row-Clusters pair in RowGroups.  Returns `ok` if every
%   row's rendered text matches the model; otherwise a diff dict.

verify_rows(_T, _P, _R, _Prompt, [], ok).
verify_rows(T, P, R, Prompt, [RowNum-Clusters|Groups], Outcome) :-
    model_row_text(Prompt, P, R, RowNum, Clusters, ExpText),
    row_text(T, RowNum, GotText),
    (   ExpText == GotText
    ->  verify_rows(T, P, R, Prompt, Groups, Outcome)
    ;   Outcome = diff{ row:          RowNum,
                        expected_row: ExpText,
                        got_row:      GotText }
    ).

collect_row_diffs(_T, _P, _R, _Prompt, [], []).
collect_row_diffs(T, P, R, Prompt, [RowNum-Clusters|Groups], Diffs) :-
    model_row_text(Prompt, P, R, RowNum, Clusters, ExpText),
    row_text(T, RowNum, GotText),
    (   ExpText == GotText
    ->  Rest = Diffs
    ;   Diffs = [row(RowNum, ExpText, GotText)|Rest]
    ),
    collect_row_diffs(T, P, R, Prompt, Groups, Rest).

%!  report_failure(+T, +P, +R, +Prompt, +State, +History, +Divergence) is det.

report_failure(_T, P, R, Prompt, state(Cs, Cursor),
               History, Divergence) :-
    length(Cs, Len),
    format(user_error, "~n*** test_terminal_random DIVERGENCE ***~n", []),
    format(user_error, "prompt at (col=~w, row=~w), prompt line = ~q~n",
           [P, R, Prompt]),
    format(user_error, "model state: cursor=~w of ~w clusters~n",
           [Cursor, Len]),
    format(user_error, "clusters: ~q~n", [Cs]),
    (   get_dict(expected_cursor, Divergence, EC),
        get_dict(got_cursor,      Divergence, GC)
    ->  format(user_error, "expected cursor: ~q   got: ~q~n", [EC, GC])
    ;   true
    ),
    (   get_dict(row,             Divergence, RN),
        get_dict(expected_row,    Divergence, ER),
        get_dict(got_row,         Divergence, GR)
    ->  format(user_error, "row ~w expected: ~q~n", [RN, ER]),
        format(user_error, "row ~w got:      ~q~n", [RN, GR])
    ;   true
    ),
    (   get_dict(row_diffs, Divergence, RowDiffs), RowDiffs \== []
    ->  forall(member(row(RN, ER, GR), RowDiffs),
               ( format(user_error, "row ~w expected: ~q~n", [RN, ER]),
                 format(user_error, "row ~w got:      ~q~n", [RN, GR])
               ))
    ;   true
    ),
    format(user_error, "command history (in order):~n", []),
    forall(member(C, History),
           format(user_error, "    ~q~n", [C])).
