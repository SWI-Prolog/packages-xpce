/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org/projects/xpce/
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

:- module(test_terminal_osc,
          [ test_terminal_osc/0
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(yall)).

/** <module> Test OSC 8 hyperlinks of the xpce terminal

A hyperlink is a state: everything between `ESC ] 8 ; <params> ; <URL>
ST` and the same sequence with an empty URL is the label, escape
sequences included.  Clients colour the label (ripgrep does), so the
label must go through the normal escape processing rather than be
copied to the screen as text.
*/

test_terminal_osc :-
    run_tests([ terminal_osc8,
                terminal_osc133
              ]).

                /*******************************
                *            HARNESS           *
                *******************************/

%!  terminal(-Terminal) is det.
%
%   Create a terminal image of 80x25 cells in an open window.

terminal(TI) :-
    new(TI, terminal_image(1000, 500)),
    new(W, window('test_terminal_osc')),
    send(W, display, TI),
    send(W, open),
    send(W, wait).

%!  destroy_terminal(+Terminal) is det.

destroy_terminal(TI) :-
    get(TI, window, W),
    send(W, destroy).

%!  row(+Terminal, +Row, -Text) is det.
%
%   Text of Row, with trailing blanks removed.

row(TI, Row, Text) :-
    get(TI, row, Row, String),
    get(String, value, Atom),
    normalize_space(atom(Text), Atom).

%!  link_at(+Terminal, +Column, +Row, -URL) is semidet.
%
%   URL of the hyperlink displayed at cell Column,Row.

link_at(TI, Column, Row, URL) :-
    get(TI, font, Font),
    get(Font, avg_char_width, CW),
    get(Font, height, CH),
    X is round((Column+1)*CW + CW/2),   % centre of the cell; one cell
    Y is round(Row*CH + CH/2),          % of left margin
    get(TI, link, point(X, Y), URL).

%!  osc8(+URL, -Sequence) is det.
%!  st(-ST) is det.
%
%   Building blocks for OSC 8 sequences.  osc8('') closes the link.

osc8(URL, Seq) :-
    osc8(URL, '', Seq).

osc8(URL, Params, Seq) :-
    st(ST),
    atom_codes(Osc, [0'\e, 0']]),
    atomic_list_concat([Osc, '8;', Params, ';', URL, ST], Seq).

st(ST) :-
    atom_codes(ST, [0'\e, 0'\\]).

sgr(Code, Seq) :-
    atomic_list_concat(['\e[', Code, m], Seq).

%!  osc133(+Letter, -Sequence) is det.
%
%   One of the OSC 133 semantic prompt marks.

osc133(Letter, Seq) :-
    st(ST),
    atom_codes(Osc, [0'\e, 0']]),
    atomic_list_concat([Osc, '133;', Letter, ST], Seq).

%!  command(+N, -Text) is det.
%
%   A marked prompt `?- goalN.' with three lines of output under it.

command(N, Text) :-
    command(N, 3, Text).

command(N, Lines, Text) :-
    maplist(osc133, ['A','B','C','D'], [A,B,C,D]),
    format(atom(Goal), 'goal~w.', [N]),
    findall(L, (between(1, Lines, I),
                format(atom(L), 'out~w-~w\r\n', [N,I])), Ls),
    atomic_list_concat(Ls, Out),
    atomic_list_concat([A, '?- ', B, Goal, '\r\n', C, Out, D], Text).

%!  continued(+N, +Lines, -Text) is det.
%
%   A command typed over Lines lines: the first prompt is a primary one
%   and the rest are marked `A;k=s', as a client does that is still
%   collecting one input.  Three lines of output under it.

continued(N, Lines, Text) :-
    maplist(osc133, ['A','C','D'], [A,C,D]),
    osc133('A;k=s', A2),
    osc133('B', B),
    findall(P, ( between(1, Lines, I),
                 ( I == 1 -> Mark = A, Pr = '?- ' ; Mark = A2, Pr = '|    ' ),
                 format(atom(Goal), 'part~w(~w),', [N, I]),
                 atomic_list_concat([Mark, Pr, B, Goal, '\r\n', C, D], P) ),
            Ps),
    atomic_list_concat(Ps, Head),
    findall(L, (between(1, 3, I),
                format(atom(L), 'out~w-~w\r\n', [N,I])), Ls),
    atomic_list_concat(Ls, Out),
    %  the output really comes after the last line was entered
    atomic_list_concat([Head, Out, D], Text).

%!  prompt(-Text) is det.
%
%   A marked prompt with nothing entered at it yet.

prompt(Text) :-
    maplist(osc133, ['A','B'], [A,B]),
    atomic_list_concat([A, '?- ', B], Text).

%!  session(+Terminal, +N) is det.
%
%   Run N marked commands and leave the terminal at a fresh prompt.

session(TI, N) :-
    forall(between(1, N, I), (command(I, T), send(TI, insert, T))),
    prompt(P),
    send(TI, insert, P).

%!  blocks(+Terminal, -Blocks) is det.

blocks(TI, Blocks) :-
    get(TI, blocks, Chain),
    chain_list(Chain, Blocks).

%!  screen(+Terminal, -Rows) is det.
%
%   The non-blank rows of the window, whitespace normalised.

screen(TI, Rows) :-
    get(TI, rows, N),
    End is N-1,
    findall(R, (between(0, End, I),
                get(TI, row, I, S),
                get(S, value, A),
                normalize_space(atom(R), A)), All),
    exclude(==(''), All, Rows).

%!  row_text(+Terminal, +Row, -Text) is det.
%
%   Text of one row of the window, whitespace normalised.

row_text(TI, Row, Text) :-
    get(TI, row, Row, S),
    get(S, value, A),
    normalize_space(atom(Text), A).

%!  row_codes(+Terminal, +Row, -Codes) is det.
%
%   Codes of one row of the window, trailing blanks removed.  Unlike
%   row_text/3 this shows anything on the screen that normalize_space/2
%   would hide.

row_codes(TI, Row, Codes) :-
    get(TI, row, Row, S),
    get(S, value, A),
    atom_codes(A, All),
    append(Codes, Blanks, All),
    maplist(==(0' ), Blanks),
    \+ append(_, [0' ], Codes),
    !.

%!  fill_window(+Terminal, -Blocks) is det.
%
%   Run enough commands to more than fill the window and leave the
%   terminal at a prompt on its bottom row.

fill_window(TI, Blocks) :-
    get(TI, rows, Rows),
    N is Rows//4 + 2,                   % four rows to a command
    session(TI, N),
    blocks(TI, Blocks).

%!  text(+Object, -Atom) is det.

text(Obj, Atom) :-
    get(Obj, value, Atom).

                /*******************************
                *             TESTS            *
                *******************************/

:- begin_tests(terminal_osc8).

test(plain, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    osc8('file:///tmp/a.txt', Open),
    osc8('', Close),
    atomic_list_concat([Open, '/tmp/a.txt', Close, ':hello'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == '/tmp/a.txt:hello'),
    assertion(link_at(TI, 0, 0, 'file:///tmp/a.txt')),
    assertion(link_at(TI, 9, 0, 'file:///tmp/a.txt')),
    assertion(\+ link_at(TI, 11, 0, _)).

% As emitted by rg --hyperlink-format=...: the label is coloured, so it
% contains SGR sequences.  These must be processed rather than be added
% to the screen as text.

test(coloured_label, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    osc8('file:///tmp/a.txt#L1:1', Open),
    osc8('', Close),
    sgr(0, Sgr0),
    sgr(35, Sgr35),
    atomic_list_concat([Open, Sgr0, Sgr35, '/tmp/a.txt', Sgr0, Close,
                        ':hello'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == '/tmp/a.txt:hello'),
    assertion(link_at(TI, 0, 0, 'file:///tmp/a.txt#L1:1')),
    assertion(link_at(TI, 9, 0, 'file:///tmp/a.txt#L1:1')),
    assertion(\+ link_at(TI, 10, 0, _)).

% The label may be terminated using BEL rather than ST and the sequence
% may carry parameters, e.g., `id=`.

test(bel_and_params, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    atom_codes(Open, [0'\e, 0'], 0'8, 0';, 0'i, 0'd, 0'=, 0'1, 0';,
                      0'h, 0't, 0't, 0'p, 0':, 0'/, 0'/, 0'x, 0'/, 0'\a]),
    atom_codes(Close, [0'\e, 0'], 0'8, 0';, 0';, 0'\a]),
    atomic_list_concat([Open, label, Close, '.'], Text),
    send(TI, insert, Text),
    row(TI, 0, Row),
    assertion(Row == 'label.'),
    assertion(link_at(TI, 0, 0, 'http://x/')),
    assertion(\+ link_at(TI, 5, 0, _)).

% A label that does not fit on the line continues on the next one, where
% it must be a link as well.

test(wrap, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    osc8('file:///tmp/wrap', Open),
    osc8('', Close),
    get(TI, columns, Columns),
    Len is Columns+20,
    length(Codes, Len),
    maplist(=(0'x), Codes),
    atom_codes(Label, Codes),
    atomic_list_concat([Open, Label, Close], Text),
    send(TI, insert, Text),
    Last is Columns-1,
    assertion(link_at(TI, Last, 0, 'file:///tmp/wrap')),
    assertion(link_at(TI, 0, 1, 'file:///tmp/wrap')),
    assertion(link_at(TI, 19, 1, 'file:///tmp/wrap')),
    assertion(\+ link_at(TI, 21, 1, _)).

% Every pixel of a cell must find the link of that cell, including the
% first and last one.  Translating a click used to be off by one cell,
% so the last character of a link was not clickable.

test(cell_pixels, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    numlist(0, 4, Cells),
    maplist(one_cell_link, Cells, Texts),
    atomic_list_concat(Texts, Text),
    send(TI, insert, Text),
    get(TI, font, Font),
    get(Font, avg_char_width, CW0),
    CW is truncate(CW0),
    forall(member(Cell, Cells),
           ( atom_concat(u, Cell, URL),
             Left  is (Cell+1)*CW,      % first pixel of the cell
             Right is (Cell+2)*CW-1,    % last pixel of the cell
             assertion(link_pixel(TI, Left, URL)),
             assertion(link_pixel(TI, Right, URL)),
             assertion(link_at(TI, Cell, 0, URL))
           )).

one_cell_link(Cell, Text) :-
    atom_concat(u, Cell, URL),
    osc8(URL, Open),
    osc8('', Close),
    atomic_list_concat([Open, x, Close], Text).

%!  link_pixel(+Terminal, +X, ?URL) is semidet.
%
%   URL of the hyperlink at pixel X of the first row.

link_pixel(TI, X, URL) :-
    get(TI, link, point(X, 1), URL).

:- end_tests(terminal_osc8).

/** <module> Test the OSC 133 semantic prompt marks

The marks divide the window into blocks: a prompt, the line the user
entered and the output it produced.  The terminal keeps them as
`terminal_block` objects, which is what lets a command be copied, jumped
to or folded away long after it was printed.
*/

:- begin_tests(terminal_osc133).

test(one_per_command, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 3),
    blocks(TI, Blocks),
    length(Blocks, 4),                  % three commands and the prompt
    maplist([B,C]>>(get(B, content, command, S), text(S, C)),
            Blocks, Commands),
    assertion(Commands == ['goal1.', 'goal2.', 'goal3.', '']).

test(output, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 2),
    blocks(TI, [B1,B2|_]),
    get(B1, content, output, S1), text(S1, O1),
    get(B2, content, output, S2), text(S2, O2),
    assertion(O1 == 'out1-1\nout1-2\nout1-3\n'),
    assertion(O2 == 'out2-1\nout2-2\nout2-3\n').

%  Bash turns the readline markers \[ and \] of PS0 into \001 and \002
%  and writes PS0 verbatim, so the C mark arrives wrapped in them.  A
%  terminal drops C0 controls it has no meaning for; printing them would
%  put a glyph in front of every command's output.

test(a_c0_control_is_not_printed,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    maplist(osc133, ['A','B','C','D'], [A,B,C,D]),
    atomic_list_concat([A, '?- ', B, 'goal.\r\n',
                        '\u0001', C, '\u0002', 'hello\r\n', D], Text),
    send(TI, insert, Text),
    assertion(row_codes(TI, 1, `hello`)),
    blocks(TI, [Block]),
    get(Block, content, output, S),
    assertion(text(S, 'hello\n')).

test(the_marks_are_a_method_that_can_be_sent,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  osc_command() decodes the letter and the `k=' parameter and sends
    %  ->prompt_mark, so the state machine can be driven without any
    %  escape sequences at all -- and a subclass can refine it.
    send(TI, prompt_mark, prompt),
    send(TI, insert, '?- '),
    send(TI, prompt_mark, input),
    send(TI, insert, 'sent.\r\n'),
    send(TI, prompt_mark, output),
    send(TI, insert, 'out-a\r\nout-b\r\n'),
    send(TI, prompt_mark, end),
    blocks(TI, [B1]),
    get(B1, content, command, S1), text(S1, Cmd),
    get(B1, content, output, S2), text(S2, Out),
    assertion(Cmd == 'sent.'),
    assertion(Out == 'out-a\nout-b\n'),
    assertion(get(B1, running, @off)),
    assertion(send(B1, fold)).

test(a_sent_continuation_keeps_one_block,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    send(TI, prompt_mark, prompt),
    send(TI, insert, '?- '),
    send(TI, prompt_mark, input),
    send(TI, insert, 'first(\r\n'),
    send(TI, prompt_mark, output),
    send(TI, prompt_mark, end),
    send(TI, prompt_mark, prompt, @on),   % a secondary prompt
    send(TI, insert, '|  '),
    send(TI, prompt_mark, input),
    send(TI, insert, 'second).\r\n'),
    send(TI, prompt_mark, output),
    send(TI, insert, 'out-c\r\n'),
    send(TI, prompt_mark, end),
    blocks(TI, Blocks),
    assertion(length(Blocks, 1)),
    Blocks = [B1|_],
    get(B1, content, command, S), text(S, Cmd),
    assertion(Cmd == 'first(\nsecond).').

test(erasing_the_display_lets_go_of_what_it_erased,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  ED 2 destroys the lines a block was made of.  A mark that was
    %  recorded and has since gone is not a command still running: the
    %  block must say nothing rather than reach to where the client is
    %  writing, or folding it would swallow whatever came after.
    session(TI, 2),
    blocks(TI, Before),
    assertion(Before \== []),
    send(TI, insert, '\e[2J\e[H'),      % erase the display, caret home
    blocks(TI, After),
    assertion(After == []),             % nothing of them is left to name
    %  and what comes next is neither swallowed by a stale fold nor
    %  reported as part of a command that has gone
    session(TI, 1),
    screen(TI, Rows),
    assertion(memberchk('out1-1', Rows)),
    blocks(TI, [Fresh|_]),
    get(Fresh, content, output, S), text(S, Out),
    assertion(Out == 'out1-1\nout1-2\nout1-3\n').

test(erasing_below_the_caret_lets_go_of_what_it_erased,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  ED 0 from the top of the window destroys the same lines as ED 2,
    %  but leaves the caret's own line in the buffer: a block can keep
    %  its prompt and lose the mark that closed it.  It cannot say how
    %  far it reaches any more, so it must go with the rest.  This is
    %  the `cl' of a client on Windows, where libedit's built-in termcap
    %  clears the screen with CUP home + ED 0.
    session(TI, 2),
    blocks(TI, Before),
    assertion(Before \== []),
    send(TI, insert, '\e[H\e[J'),       % caret home, erase below it
    blocks(TI, After),
    assertion(After == []),
    session(TI, 1),
    screen(TI, Rows),
    assertion(memberchk('out1-1', Rows)),
    blocks(TI, [Fresh|_]),
    get(Fresh, content, output, S), text(S, Out),
    assertion(Out == 'out1-1\nout1-2\nout1-3\n').

test(a_command_does_not_include_the_return_that_entered_it,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 1),
    blocks(TI, [B1|_]),
    get(B1, content, command, S), text(S, Cmd),
    assertion(Cmd == 'goal1.'),
    %  and the output starts on the line below, where it was printed
    get(B1, content, output, S2), text(S2, Out),
    assertion(Out == 'out1-1\nout1-2\nout1-3\n').

test(a_copy_leaves_out_the_continuation_prompts,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  ->copy hands over the command, ->select marks the screen region
    %  it was typed in -- which still has the prompts on it.
    continued(1, 3, Text),
    send(TI, insert, Text),
    prompt(P), send(TI, insert, P),
    blocks(TI, [B1|_]),
    get(B1, content, command, S), text(S, Cmd),
    assertion(\+ sub_atom(Cmd, _, _, _, '|')),
    assertion(\+ sub_atom(Cmd, _, 1, 0, '\n')),
    get(B1, content, all, S2), text(S2, All),
    assertion(sub_atom(All, _, _, _, '|')).

test(indices_compose, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  What a block reports is an index into the same flat character
    %  space <-contents addresses, so the two must agree.
    session(TI, 2),
    blocks(TI, [B1|_]),
    get(B1, output, From),
    get(B1, end, To),
    Size is To-From,
    get(TI, contents, From, Size, S), text(S, Text),
    get(B1, content, output, S2), text(S2, Text2),
    assertion(Text == Text2).

test(a_redrawn_prompt_is_the_same_block,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  `A' and `B' live inside the prompt string of the client, so it
    %  emits them again every time it redraws its prompt.  A repeat says
    %  where the prompt has moved to; it does not start a command.
    session(TI, 1),
    blocks(TI, Before),
    prompt(P),
    forall(between(1, 5, _), send(TI, insert, P)),
    blocks(TI, After),
    assertion(length(Before, 2)),
    assertion(length(After, 2)).

test(a_command_typed_over_several_lines_is_one_block,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  Each line is asked for separately, but all but the first are
    %  marked `A;k=s' -- a secondary prompt -- so they are one command.
    continued(1, 3, Text),
    send(TI, insert, Text),
    prompt(P), send(TI, insert, P),
    blocks(TI, Blocks),
    length(Blocks, 2),                  % the command and the prompt after it
    Blocks = [B1|_],
    get(B1, content, command, S), text(S, Cmd),
    assertion(sub_atom(Cmd, _, _, _, 'part1(1),')),   % the first line
    assertion(sub_atom(Cmd, _, _, _, 'part1(3),')),   % and the last
    get(B1, content, output, S2), text(S2, Out),
    assertion(Out == 'out1-1\nout1-2\nout1-3\n').

test(the_marker_of_a_continued_command_is_on_its_first_line,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    continued(1, 3, Text),
    send(TI, insert, Text),
    prompt(P), send(TI, insert, P),
    blocks(TI, [B1|_]),
    gutter_click(TI, 0),                % the line the command starts on
    assertion(get(B1, folded, @on)),
    gutter_click(TI, 0),
    assertion(get(B1, folded, @off)),
    gutter_click(TI, 2),                % not the one it ends on
    assertion(get(B1, folded, @off)).

test(a_silent_command_does_not_swallow_the_next,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  A shell command that printed nothing leaves its block's output
    %  empty and the next prompt standing exactly where it ended.  That
    %  is not a continuation: without `k=s' the two stay apart.
    maplist(osc133, ['A','B','C','D'], [A,B,C,D]),
    atomic_list_concat([A,'$ ',B,'touch x','\r\n',C,D,
                        A,'$ ',B,'ls','\r\n',C,'x\r\ny\r\n',D,
                        A,'$ ',B], Text),
    send(TI, insert, Text),
    blocks(TI, [_Touch,Ls|_]),
    gutter_click(TI, 0),                % `$ touch x' has nothing to fold
    assertion(get(Ls, folded, @off)),
    gutter_click(TI, 1),                % `$ ls' does
    assertion(get(Ls, folded, @on)).

test(running, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    maplist(osc133, ['A','B','C','D'], [A,B,C,D]),
    atomic_list_concat([A, '?- ', B, 'slow.\r\n', C, 'partial\r\n'], Start),
    send(TI, insert, Start),
    blocks(TI, [Blk]),
    assertion(get(Blk, running, @on)),
    assertion(\+ get(Blk, end, _)),     % it has not ended yet
    assertion(\+ send(Blk, fold)),      % and so cannot be folded
    send(TI, insert, D),
    assertion(get(Blk, running, @off)),
    assertion(get(Blk, end, _)).

test(the_block_at_a_place, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 3),
    blocks(TI, [B1,B2|_]),
    get(B1, output, I1),
    get(B2, output, I2),
    assertion(get(TI, block_at, I1, B1)),
    assertion(get(TI, block_at, I2, B2)),
    get(B2, id, Id),
    assertion(get(TI, block, Id, B2)).

test(fold_hides_the_output,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 3),
    blocks(TI, [B1|_]),
    screen(TI, Before),
    assertion(memberchk('out1-1', Before)),
    send(B1, fold),
    screen(TI, Folded),
    assertion(\+ memberchk('out1-1', Folded)),
    assertion(memberchk('?- goal1.', Folded)),   % the command stays
    assertion(memberchk('out2-1', Folded)),      % and the others do
    send(B1, unfold),
    screen(TI, After),
    assertion(After == Before).

test(a_fold_hides_nothing_from_the_buffer,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  Folding is a property of the view.  What it takes off the display
    %  is still there to be searched and copied.
    session(TI, 3),
    blocks(TI, [B1|_]),
    get(TI, length, Len),
    send(B1, fold),
    assertion(get(TI, length, Len)),
    assertion(get(TI, find, 0, 'out1-2', _)),
    get(B1, content, output, S), text(S, Out),
    assertion(Out == 'out1-1\nout1-2\nout1-3\n').

test(fold_all, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 3),
    send(TI, fold_all),
    screen(TI, Rows),
    assertion(Rows == ['?- goal1.', '?- goal2.', '?- goal3.', '?-']),
    send(TI, unfold_all),
    screen(TI, Back),
    assertion(memberchk('out2-2', Back)).

test(a_click_on_the_marker_folds,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  The marker sits in the one column of margin left of the text.  It
    %  is ours rather than the client's, so a plain click is enough --
    %  a hyperlink needs Control.
    session(TI, 3),
    blocks(TI, [B1|_]),
    gutter_click(TI, 0),
    assertion(get(B1, folded, @on)),
    gutter_click(TI, 0),
    assertion(get(B1, folded, @off)).

test(no_marker_no_fold, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  A command that printed nothing has nothing to hide, so its
    %  margin is empty and a click there does nothing.
    maplist(osc133, ['A','B','C','D'], [A,B,C,D]),
    atomic_list_concat([A, '?- ', B, 'quiet.\r\n', C, D], Text),
    send(TI, insert, Text),
    blocks(TI, [B1|_]),
    assertion(\+ send(B1, fold)),
    gutter_click(TI, 0),
    assertion(get(B1, folded, @off)).

%!  gutter_click(+Terminal, +Row) is det.
%
%   Click the left margin of Row, where the fold marker is drawn.

gutter_click(TI, Row) :-
    get(TI, font, Font),
    get(Font, avg_char_width, CW),
    get(Font, height, CH),
    X is max(0, truncate(CW/2)),
    Y is truncate(Row*CH + CH/2),
    send(TI, event, new(_, event(ms_left_down, TI, X, Y, 0, 0))),
    send(TI, event, new(_, event(ms_left_up,   TI, X, Y, 0, 0))).

test(folding_keeps_the_end_on_the_bottom_row,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  Closing a fold takes rows out of the window and everything under
    %  it moves up.  Without pulling the window back, the last line walks
    %  off the bottom row and the blank rest of the buffer follows it.
    fill_window(TI, Blocks),
    get(TI, rows, Rows),
    Bottom is Rows-1,
    assertion(row_text(TI, Bottom, '?-')),
    reverse(Blocks, [_Prompt,Last|_]),
    send(Last, fold),
    assertion(row_text(TI, Bottom, '?-')),
    send(Last, unfold),
    assertion(row_text(TI, Bottom, '?-')).

test(scrolling_to_the_end_stops_at_the_end,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  There is nothing under the last line to show, so scrolling past it
    %  only walks the text off the top; far enough and the window is
    %  empty.  Every way of asking must stop at the same place.
    fill_window(TI, _),
    screen(TI, AtEnd),
    assertion(AtEnd \== []),
    forall(member(Ask, [ scroll_vertical(forwards, page, 10000),
                         scroll_vertical(forwards, line, 10000),
                         scroll_vertical(goto, file, 1000)
                       ]),
           ( Ask =.. [Sel|Args],
             Msg =.. [send, TI, Sel|Args],
             call(Msg),
             screen(TI, Rows),
             assertion(Rows == AtEnd)
           )).

test(scrolling_to_the_end_stops_at_the_end_with_a_fold,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    fill_window(TI, Blocks),
    Blocks = [B1|_],
    send(B1, fold),
    screen(TI, AtEnd),
    send(TI, scroll_vertical, forwards, page, 10000),
    assertion(screen_is(TI, AtEnd)),
    send(TI, scroll_vertical, goto, file, 1000),
    assertion(screen_is(TI, AtEnd)).

screen_is(TI, Rows) :-
    screen(TI, Now),
    Now == Rows.

test(folding_while_scrolled_back_leaves_the_view_alone,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  Keeping the end in view is for a window that was showing it.  One
    %  the user has scrolled away from stays where they put it.
    fill_window(TI, Blocks),
    send(TI, scroll_vertical, backwards, page, 1000),
    row_text(TI, 0, Top),
    assertion(Top == '?- goal1.'),
    Blocks = [First|_],
    send(First, fold),
    assertion(row_text(TI, 0, Top)).

test(remove_closes_the_gap, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 3),
    get(TI, length, Len0),
    blocks(TI, [_,B2|_]),
    assertion(send(B2, remove)),
    screen(TI, Rows),
    assertion(Rows == ['?- goal1.','out1-1','out1-2','out1-3',
                       '?- goal3.','out3-1','out3-2','out3-3','?-']),
    get(TI, length, Len1),
    assertion(Len1 < Len0),
    assertion(get(B2, terminal, @nil)),
    assertion(\+ get(TI, find, 0, 'out2-2', _)).

test(remove_leaves_its_neighbours_saying_the_same,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  `D' lands on the line the prompt after it goes on, so the block
    %  before the one removed had a mark inside the range.
    session(TI, 3),
    blocks(TI, [B1,B2,B3|_]),
    send(B2, remove),
    get(B1, content, output, S1), text(S1, O1),
    get(B3, content, output, S3), text(S3, O3),
    assertion(O1 == 'out1-1\nout1-2\nout1-3\n'),
    assertion(O3 == 'out3-1\nout3-2\nout3-3\n'),
    get(B1, content, command, C1), text(C1, Cmd1),
    assertion(Cmd1 == 'goal1.').

test(a_running_command_cannot_be_removed,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    maplist(osc133, ['A','B','C'], [A,B,C]),
    atomic_list_concat([A,'?- ',B,'slow.\r\n',C,'partial\r\n'], Text),
    send(TI, insert, Text),
    blocks(TI, [Blk]),
    assertion(\+ send(Blk, remove)).

test(remove_keeps_the_folds_of_the_others,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 3),
    blocks(TI, [B1,B2,B3|_]),
    send(B3, fold),
    send(B2, remove),
    assertion(get(B3, folded, @on)),
    screen(TI, Rows),
    assertion(\+ memberchk('out3-2', Rows)),
    assertion(memberchk('?- goal3.', Rows)),
    assertion(memberchk('out1-2', Rows)),
    send(B3, unfold),
    screen(TI, Back),
    assertion(memberchk('out3-2', Back)).

test(remove_ends_a_search_that_may_be_looking_at_it,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 3),
    blocks(TI, [_,B2|_]),
    send(TI, selection, 0, 10),
    assertion(send(TI, has_selection)),
    send(B2, remove),
    assertion(\+ send(TI, has_selection)).

test(scroll_to_opens_a_fold,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 3),
    blocks(TI, [B1|_]),
    get(B1, output, I),
    send(B1, fold),
    assertion(get(B1, folded, @on)),
    send(TI, scroll_to, I),
    assertion(get(B1, folded, @off)).

test(select_and_copy, [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    session(TI, 2),
    blocks(TI, [B1|_]),
    send(B1, select, output),
    get(TI, selected, S), text(S, Sel),
    normalize_space(atom(Flat), Sel),
    assertion(Flat == 'out1-1 out1-2 out1-3').

test(marks_survive_a_rewrap,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  A resize breaks the lines up differently, so a block anchored to
    %  ring positions has to be carried across as the caret is.
    session(TI, 2),
    blocks(TI, [B1|_]),
    get(B1, content, output, S0), text(S0, Before),
    send(TI, size, size(400, 500)),
    send(TI, compute),
    get(B1, content, output, S1), text(S1, After),
    assertion(After == Before).

test(a_block_dies_with_its_lines,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    %  The anchors of a block are lines of the scroll-back.  Once they
    %  have been pushed out of it there is nothing left to point at.
    session(TI, 1),
    blocks(TI, [B1|_]),
    assertion(get(B1, terminal, TI)),
    get(TI, save_lines, Save),
    Lines is Save+10,
    forall(between(1, Lines, _), send(TI, insert, 'x\r\n')),
    assertion(get(B1, terminal, @nil)),
    assertion(\+ get(B1, content, output, _)),
    blocks(TI, Left),
    assertion(\+ memberchk(B1, Left)).

%!  input(+Terminal, -Text, -Tail) is semidet.
%
%   <-input as Prolog data.

input(TI, Text, Tail) :-
    get(TI, input, Tuple),
    get(Tuple, first, S), text(S, Text),
    get(Tuple, second, Tail).

test(input_of_the_first_line,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    send(TI, prompt_mark, prompt),
    send(TI, insert, '?- '),
    send(TI, prompt_mark, input),
    assertion(input(TI, '', 0)),
    send(TI, insert, 'foo(X'),
    assertion(input(TI, 'foo(X', 0)),
    send(TI, insert, '\e[2D'),          % caret two to the left
    assertion(input(TI, 'foo(X', 2)).

test(no_input_while_not_reading,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    assertion(\+ get(TI, input, _)),    % nothing marked
    send(TI, prompt_mark, prompt),
    send(TI, insert, '?- '),
    send(TI, prompt_mark, input),
    send(TI, insert, 'sleep(5).\r\n'),
    send(TI, prompt_mark, output),
    assertion(\+ get(TI, input, _)).    % a command runs

test(no_input_at_a_continuation,
     [setup(terminal(TI)), cleanup(destroy_terminal(TI))]) :-
    send(TI, prompt_mark, prompt),
    send(TI, insert, '?- '),
    send(TI, prompt_mark, input),
    send(TI, insert, 'read(X).\r\n'),
    send(TI, prompt_mark, output),
    send(TI, prompt_mark, prompt, @on), % read/1 at `|: '
    send(TI, insert, '|: '),
    send(TI, prompt_mark, input),
    assertion(\+ get(TI, input, _)),
    send(TI, insert, 'a.\r\n'),
    send(TI, prompt_mark, output),
    send(TI, prompt_mark, end),
    send(TI, prompt_mark, prompt),      % the next query
    send(TI, insert, '?- '),
    send(TI, prompt_mark, input),
    assertion(input(TI, '', 0)).

:- end_tests(terminal_osc133).
