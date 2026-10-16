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

:- module(test_emacs_grep, [test_emacs_grep/0]).

/** <module> Tests for showing the hits of grep in a hit list

Run with:

    swipl -g test_emacs_grep -t halt packages/xpce/tests/test_emacs_grep.pl
*/

%       The hit list is opened in a window, which needs a display.

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

%   Opening the hit list writes the window arrangements and geometry to
%   the user's xpce configuration.  Use a configuration of our own.

:- use_module(xpce_test_sandbox).
:- initialization(sandbox_app_config(_), now).

:- use_module(library(pce)).
:- initialization(remove_sandbox_at_exit, now).
:- use_module(library(plunit)).
:- use_module(library(emacs/bookmarks)).
:- use_module(library(emacs/grep)).
:- use_module(library(lists), [member/2]).
:- use_module(library(process), [process_which/2]).

test_emacs_grep :-
    run_tests([ grep_line,
                grep_hit_list,
                hit_list_filter,
                hit_title
              ]).

%!  grep_line(+Line, -File, -LineNo, -Pos, -Len, -Text) is semidet
%
%   Testing a private predicate. Using module override to get it defined
%   here.

grep_line(Line, File, LineNo, Pos, Len, Text) :-
    emacs_grep:grep_line(Line, File, LineNo, Pos, Len, Text).

:- begin_tests(grep_line).

test(coloured, [File-Line-Pos-Len-Text == 'a.pl'-12-4-3-"one foo bar"]) :-
    grep_line("\e[35ma.pl\e[m\e[36m:\e[m\e[32m12\e[m\e[36m:\e[m\c
               one \e[1;31mfoo\e[m bar\n",
              File, Line, Pos, Len, Text).
test(gnu_coloured, [Pos-Len == 2-3]) :-
    grep_line("\e[1;35ma.pl\e[m\e[36m:\e[m\e[1;32m1\e[m\e[36m:\e[m\c
               ab\e[m\e[1;31m\e[Kfoo\e[m\e[K\e[m\n",
              _, _, Pos, Len, _).
test(plain, [File-Line-Pos-Len-Text == 'a.pl'-3-0-(@default)-"\tfoo"]) :-
    grep_line("a.pl:3:\tfoo\n", File, Line, Pos, Len, Text).
test(colon_in_file, [File-Line == 'a:b.pl'-7]) :-
    grep_line("a:b.pl:7:x", File, Line, _, _, _).
test(text_with_line_number, [File-Line-Text == 'a.pl'-7-"8:x"]) :-
    grep_line("a.pl:7:8:x", File, Line, _, _, Text).
test(message, fail) :-
    grep_line("grep: x: Is a directory\n", _, _, _, _, _).
test(long_line, [TextLen-Pos-Len == 500-0-(@default)]) :-
    length(Codes, 600),
    maplist(=(0'x), Codes),
    string_codes(Long, Codes),
    format(string(Line), "a.pl:1:~s\e[31mfoo\e[m", [Long]),
    grep_line(Line, _, _, Pos, Len, Text),
    string_length(Text, TextLen).

:- end_tests(grep_line).

%   These run the grep program, which need not be installed, e.g., on
%   Windows.

has_grep :-
    process_which(path(grep), _).

:- begin_tests(grep_hit_list, [ condition(has_grep),
                                setup(make_tree),
                                cleanup(remove_tree)
                              ]).

:- dynamic test_dir/1.

make_tree :-
    tmp_file(grep, Dir),
    make_directory(Dir),
    asserta(test_dir(Dir)),
    directory_file_path(Dir, sub, Sub),
    make_directory(Sub),
    write_file(Dir, 'a.txt', "one foo\ntwo\n\tthree foo bar\n"),
    write_file(Sub, 'b.txt', "foo at start\n").

remove_tree :-
    retract(test_dir(Dir)),
    delete_directory_and_contents(Dir).

write_file(Dir, Name, Content) :-
    directory_file_path(Dir, Name, Path),
    setup_call_cleanup(open(Path, write, Out),
                       write(Out, Content),
                       close(Out)).

%!  grep(+Args, -Hits, -Marks) is det.
%
%   Run grep with Args in the test directory and wait for it.  Marks
%   is a list of File-Line-Pos-Len, where File is relative to the
%   test directory.

grep(Args, Hits, Marks) :-
    test_dir(Dir),
    new(BM, emacs_bookmark_editor("grep test", @off)),
    Term =.. [emacs_grep_process, grep|Args],
    new(P, Term),
    send(P, run, BM, directory(Dir)),
    await_done(P),
    get(P, hits, Hits),
    get(BM?tree?root, bookmarks, Chain),
    chain_list(Chain, List),
    findall(Mark, (member(B, List), mark(Dir, B, Mark)), Marks),
    send(BM, free).

mark(Dir, B, Rel-Line-Pos-Len) :-
    get(B, file_name, File),
    relative_file_name(File, Dir, Rel0),
    atom_string(Rel0, Rel),
    get(B, line_no, Line),
    get(B, line_pos, Pos),
    get(B, length, Len).

await_done(P) :-
    get_time(T0),
    repeat,
      (   get(P, done, @on)
      ->  !
      ;   get_time(T),
          T-T0 > 10
      ->  !,
          fail
      ;   ignore(send(@display, dispatch)),
          fail
      ).

test(hits, [ Hits-Marks == 3-[ "a.txt"-1-4-3,
                               "a.txt"-3-7-3,
                               "sub/b.txt"-1-0-3
                             ]
           ]) :-
    grep(['-rn', '--color=always', foo, '.'], Hits, Marks0),
    msort(Marks0, Marks).
test(no_colour, [ Marks == ["sub/b.txt"-1-0-(@nil)] ]) :-
    grep(['-rn', '--color=never', start, '.'], _, Marks).
test(no_match, [Hits-Marks == 0-[]]) :-
    grep(['-rn', nomatch, '.'], Hits, Marks).

:- end_tests(grep_hit_list).

:- begin_tests(hit_list_filter, [ setup(make_hits(BM)),
                                  cleanup(send(BM, free))
                                ]).

:- dynamic hit_list/1.

%   The files need not exist: the tree is built from their names.

make_hits(BM) :-
    new(BM, emacs_bookmark_editor("filter test", @off)),
    retractall(hit_list(_)),
    asserta(hit_list(BM)),
    add_hit(BM, 'a.pl', 1, "foo bar"),
    add_hit(BM, 'a.pl', 2, "baz"),
    add_hit(BM, 'sub/b.pl', 1, "qux").

add_hit(BM, File, Line, Title) :-
    working_directory(CWD, CWD),
    directory_file_path(CWD, File, Path),
    send(BM, bookmark, emacs_bookmark(Path, Line, 0, 0, Title), @on, @off).

hit_list(BM, Titles) :-
    hit_list(BM),
    get(BM?tree?root, bookmarks, Chain),
    chain_list(Chain, Marks),
    findall(Title, ( member(M, Marks),
                     get(M?title, value, Title)
                   ), Titles0),
    msort(Titles0, Titles).

typed(BM, Text) :-
    get(BM, window, dialog, D),
    get(D, member, filter, FI),
    send(FI, selection, Text),
    send(FI, apply_filter).

test(title, [Titles == ['foo bar']]) :-
    hit_list(BM),
    typed(BM, foo),
    hit_list(BM, Titles).
test(file_name, [Titles == [qux]]) :-
    hit_list(BM),
    typed(BM, 'b\\.pl'),
    hit_list(BM, Titles).
test(regex, [Titles == [baz, qux]]) :-
    hit_list(BM),
    typed(BM, '^(baz|q)'),
    hit_list(BM, Titles).
test(incomplete, [Titles == [baz]]) :-
    hit_list(BM),
    typed(BM, baz),
    typed(BM, 'ba[z'),
    hit_list(BM, Titles).
test(empty, [Titles == [baz, 'foo bar', qux]]) :-
    hit_list(BM),
    typed(BM, foo),
    typed(BM, ''),
    hit_list(BM, Titles).
test(add_filtered, [Shown-Size == ['foo bar', 'foo zz']-5]) :-
    hit_list(BM),
    typed(BM, foo),
    add_hit(BM, 'c.pl', 1, "foo zz"),
    add_hit(BM, 'c.pl', 2, "zz"),
    hit_list(BM, Shown),
    get(BM?marks, size, Size),
    get(BM?marks, find_all, message(@arg1?title, sub, zz), Added),
    send(BM?marks, subtract, Added),
    typed(BM, '').
test(delete_filtered, [Titles == [baz, qux]]) :-
    hit_list(BM),
    typed(BM, foo),
    get(BM?tree?root, bookmarks, Chain),
    get(Chain, head, Mark),
    get(Mark, node, Node),
    send(BM, delete_node, Node),
    typed(BM, ''),
    hit_list(BM, Titles).
test(ignore_case, [Titles == [baz]]) :-
    hit_list(BM),
    typed(BM, 'BaZ'),
    hit_list(BM, Titles).
test(node_filter, [Filter == Editor]) :-
    hit_list(BM),
    typed(BM, baz),
    get(BM?tree?root, bookmarks, Chain),
    get(Chain, head, Mark),
    get(Mark, node, Node),
    emacs_bookmarks:node_filter(Node, Filter),
    get(BM, filter, Editor).
test(label, [Texts == [baz]]) :-
    hit_list(BM),
    typed(BM, 'Z$'),
    typed(BM, 'BAZ'),
    get(BM?tree?root, bookmarks, Chain),
    get(Chain, head, Mark),
    get(Mark, node, Node),
    get(Node, class_variable_value, style_filter, Style),
    get(Node, label_text, Label),
    get(Label, content, Boxes),
    get(Boxes, low_index, Low),
    get(Boxes, high_index, High),
    findall(Box, ( between(Low, High, I),
                   get(Boxes, element, I, Box)
                 ), List),
    findall(Text, ( member(Box, List),
                    send(Box, instance_of, tbox),
                    get(Box, style, Style),
                    get(Box?text, value, Text)
                  ), Texts).
test(clear, [Filter-Size-Text == (@nil)-0-'']) :-
    hit_list(BM),
    typed(BM, foo),
    send(BM, clear),
    get(BM, filter, Filter),
    get(BM?marks, size, Size),
    get(BM, window, dialog, D),
    get(D, member, filter, FI),
    get(FI?selection, value, Text).

:- end_tests(hit_list_filter).

:- begin_tests(hit_title).

%!  title(+Title, +Pos, +Len, +Pattern, -Segments) is det.
%
%   Segments is a list of Text-Style for the label of a hit on Title,
%   where the hit is at Pos with length Len and the filter is Pattern
%   (`none` for no filter).  The styles are `s` (plain), `h` (the hit)
%   and `f` (the filter).

title(Title, Pos, Len, Pattern, Segments) :-
    new(BM, emacs_bookmark('/x/a.pl', 1, Pos, Len, Title)),
    (   Pattern == none
    ->  Filter = @nil
    ;   new(Filter, regex(Pattern)),
        send(Filter, ignore_case, @on)
    ),
    emacs_bookmarks:bm_title(BM, Filter, s, h, f, Boxes),
    findall(Text-Style,
            ( member(tbox(T, Style), Boxes),
              get(T, value, Text)
            ),
            Segments).

test(plain, [Segs == ['foo bar'-s]]) :-
    title("foo bar", 0, 0, none, Segs).
test(hit, [Segs == ['foo '-s, bar-h]]) :-
    title("foo bar", 4, 3, none, Segs).
test(filter, [Segs == [f-s, oo-f, ' bar'-s]]) :-
    title("foo bar", 0, 0, 'o+', Segs).
test(all_matches, [Segs == [a-f, ' b '-s, a-f]]) :-
    title("a b a", 0, 0, a, Segs).
test(ignore_case, [Segs == ['FOO'-f, ' bar'-s]]) :-
    title("FOO bar", 0, 0, foo, Segs).
test(overlap, [Segs == ['foo '-s, b-h, ar-f]]) :-
    title("foo bar", 4, 3, ar, Segs).
test(empty_match, [Segs == ['foo bar'-s]]) :-
    title("foo bar", 0, 0, 'x*', Segs).

:- end_tests(hit_title).
