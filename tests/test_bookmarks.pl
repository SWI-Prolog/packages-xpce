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


:- module(test_bookmarks, [test_bookmarks/0]).
:- encoding(utf8).

/** <module> Tests for the bookmark editor of PceEmacs

What the tool does with the store under it -- see test_bookmark_store.pl
for the store itself -- and when the note being typed is put away.  No
window is opened: a pane and the windows in it are made and talked to
where they stand.

Run with:

    swipl -g test_bookmarks -t halt \
          packages/xpce/tests/test_bookmarks.pl
*/

%       A pane is made and talked to, which needs a display even though
%       no window of it is ever opened.

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(plunit)).
:- use_module(library(pce)).
:- use_module(library(emacs/bookmarks)).
:- use_module(library(emacs/bookmark_store)).
:- use_module(library(lists), [member/2]).

%       Bookmarks of their own: these must neither read nor write the
%       bookmarks of whoever runs them.

:- multifile emacs_bookmark_store:bookmarks_file/1.

emacs_bookmark_store:bookmarks_file(File) :-
    current_prolog_flag(tmp_dir, Tmp),
    atom_concat(Tmp, '/test_bookmarks_store', File).

test_bookmarks :-
    run_tests([ bookmark_note_field,
                bookmark_note_saving,
                bookmark_tool_store
              ]).

%!  tool(-Pane) is det.
%
%   A bookmark pane with an empty store behind it.

tool(BM) :-
    emacs_bookmark_store:bookmark_store_file(File),
    (   exists_file(File)
    ->  delete_file(File)
    ;   true
    ),
    new(BM, emacs_bookmark_editor("Test bookmarks", @on)).

mark(BM, File, Line, Mark) :-
    send(BM, bookmark, new(Mark, emacs_bookmark(File, Line, 0, 3, "foo(x)"))).

note_editor(BM, Editor) :-
    get(BM, view, View),
    get(View, editor, Editor).

records(Records) :-
    emacs_bookmark_store:bookmark_store_file(File),
    (   exists_file(File)
    ->  read_file_to_terms(File, Records, [])
    ;   Records = []
    ).

note_of(Mark, Text) :-
    get(Mark, note, Note),
    Note \== @nil,
    get(Note, value, Text).

                 /*******************************
                 *          THE FIELD           *
                 *******************************/

/* A note belongs to the bookmark it is about, so there is nowhere to put
   one while no bookmark is selected.
*/

:- begin_tests(bookmark_note_field).

test(nothing_selected_leaves_no_room_to_write, Editable == @off) :-
    tool(BM),
    note_editor(BM, Editor),
    get(Editor, editable, Editable),
    send(BM, destroy).

test(selecting_a_bookmark_makes_room, Editable == @on) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    send(BM, current, Mark),
    note_editor(BM, Editor),
    get(Editor, editable, Editable),
    send(BM, destroy).

test(and_selecting_a_folder_takes_it_away_again, Editable == @off) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    send(BM, current, Mark),
    get(BM, window, emacs_bookmark_window, W),
    get(W?tree, root, Root),
    send(W, select_node, Root?identifier),
    note_editor(BM, Editor),
    get(Editor, editable, Editable),
    send(BM, destroy).

test(as_does_cutting_the_one_that_was_selected, Editable == @off) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    send(BM, current, Mark),
    get(BM, window, emacs_bookmark_window, W),
    send(W, selection, Mark?node),
    send(BM, cut),
    note_editor(BM, Editor),
    get(Editor, editable, Editable),
    send(BM, destroy).

:- end_tests(bookmark_note_field).

                 /*******************************
                 *        PUTTING IT AWAY       *
                 *******************************/

/* The user is done writing when the caret leaves the note or another
   bookmark is selected, and that is when the note is the bookmark's.
*/

:- begin_tests(bookmark_note_saving).

test(a_note_is_put_away_when_the_caret_leaves_it, Text == 'why this matters') :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    send(BM, current, Mark),
    type(BM, "why this matters"),
    send(BM?view, input_focus, @off),
    note_of(Mark, Text),
    send(BM, destroy).

test(and_when_another_bookmark_is_selected, [Mine, Theirs] == ['mine', @nil]) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    mark(BM, '/tmp/b.pl', 20, Other),
    send(BM, current, Mark),
    type(BM, "mine"),
    send(BM, current, Other),
    note_of(Mark, Mine),
    get(Other, note, Theirs),
    send(BM, destroy).

test(and_when_the_pane_is_told_to_save, Text == 'mine') :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    send(BM, current, Mark),
    type(BM, "mine"),
    send(BM, save),
    note_of(Mark, Text),
    send(BM, destroy).

%       Anything typed at all makes an editor `modified', a word put there
%       and taken away again included, and the note is put away every time
%       the caret leaves.

test(a_note_typed_away_to_nothing_writes_nothing, Records == Before) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    send(BM, current, Mark),
    records(Before),
    type(BM, "oops"),
    type(BM, ""),
    send(BM?view, input_focus, @off),
    records(Records),
    send(BM, destroy).

test(and_a_note_left_as_it_was_writes_nothing, Records == Before) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    send(BM, current, Mark),
    type(BM, "keep me"),
    send(BM?view, input_focus, @off),
    records(Before),
    type(BM, "keep me"),
    send(BM?view, input_focus, @off),
    records(Records),
    send(BM, destroy).

:- end_tests(bookmark_note_saving).

%!  type(+Pane, +Text) is det.
%
%   Write Text into the note as the user would.

type(BM, Text) :-
    note_editor(BM, Editor),
    send(Editor, contents, Text),
    send(Editor, modified, @on).

                 /*******************************
                 *           THE STORE          *
                 *******************************/

:- begin_tests(bookmark_tool_store).

test(a_bookmark_is_written_down_as_it_is_made, Files == ['/tmp/a.pl']) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, _),
    bookmark_store_load(Marks),
    findall(F, member(bookmark(_,F,_,_,_,_,_,_), Marks), Files),
    send(BM, destroy).

test(and_taken_out_again_when_it_is_cut, Marks == []) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, Mark),
    get(BM, window, emacs_bookmark_window, W),
    send(W, selection, Mark?node),
    send(BM, cut),
    bookmark_store_load(Marks),
    send(BM, destroy).

%       Cutting the folder cuts what is under it.

test(cutting_a_folder_takes_out_what_is_below_it, Marks == []) :-
    tool(BM),
    mark(BM, '/tmp/a.pl', 10, _),
    mark(BM, '/tmp/b.pl', 20, _),
    get(BM, window, emacs_bookmark_window, W),
    get(W?tree, root, Root),
    send(W, selection, Root),
    send(BM, cut),
    bookmark_store_load(Marks),
    send(BM, destroy).

%       A list of hits is not the user's bookmarks and has no note field
%       of its own; nothing it holds reaches the store.

test(a_hit_list_leaves_the_store_alone, Marks == []) :-
    tool(BM),
    send(BM, destroy),
    find_references_editor("foo/1", Refs),
    send(Refs, bookmark, emacs_bookmark('/tmp/a.pl', 10, 0, 3, "foo(x)")),
    bookmark_store_load(Marks),
    send(Refs, destroy).

:- end_tests(bookmark_tool_store).
