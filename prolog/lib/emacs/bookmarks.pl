/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1999-2025, University of Amsterdam
                              CWI, Amsterdam
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

:- module(emacs_bookmarks,
          [ find_references_editor/2             % +Title, -Editor
          ]).
:- use_module(library(pce)).
:- use_module(library(swi_ide), []).
:- use_module(library(toolbar)).
:- use_module(library(pce_toc)).
:- use_module(library(pane_frame)).
:- use_module(library(debug)).
:- use_module(library(pce_util)).
:- use_module(library(emacs/bookmark_store),
              [ bookmark_store_load/1, bookmark_store_save/1,
                bookmark_store_forget/1, bookmark_store_tidy/0,
                bookmark_store_id/1, bookmark_store_file/1
              ]).

:- require([ file_directory_name/2,
	     absolute_file_name/3,
	     default/3,
	     member/2,
	     send_list/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module provides the first  definition   of  an advanced bookmarking
system for PceEmacs.  Bookmarks can  be   annotated  and are timestamped.
They are kept by library(emacs/bookmark_store), which writes each one
down as it is made, annotated or thrown  away, so that several instances
of PceEmacs running at once do not overwrite each other's.

The bookmark mechanism is available through the Browse menu of PceEmacs.

Some issues to consider:

        * Save `collapsed' status of nodes?
        * Selective expansion
        * Search
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_global(@emacs_mark_list,
              new(emacs_bookmark_editor("PceEmacs bookmarks", @on))).

:- dynamic references_editor/1 as volatile.

resource(save,   image, image('tool/save.svg')).
resource(cut,    image, image('tool/cut.svg')).
resource(open,   image, image('tool/edit.svg')).
resource(pin,    image, image('pin.png')).
resource(pinned, image, image('pinned.png')).

/* Bookmarks and hits as a pane.

It used to be a frame of its own, holding a tool bar, the tree of
bookmarks, the note being written and a reporter.  It is a `tool_pane'
now -- see library(pane_frame) -- so it drops into a tab of any window of
the IDE or, being a list of places to go to in what is being edited,
along the bottom of the editor.  What it has to say goes on the status
bar of the window it ends up in, so it carries no reporter of its own.

There is more than one: `@emacs_mark_list' holds the bookmarks the user
keeps, and each unpinned hit list of `find_references_editor/2' is
another.  They are told apart by <-persists and by <-label, which is what
their tabs are called.
*/

:- pce_begin_class(emacs_bookmark_editor, tool_pane,
                   "PceEmacs bookmark administration and viewing").

class_variable(pane_side, {above,below,left,right}, below,
               "A list of places to go to is added below the editor").

variable(persists,     bool,         get, "Bookmarks are persistent").
variable(exit_message, code*,        get, "Registered exit message").
variable(pinned,       bool := @off, get, "Pin: do not reuse for the next query").
variable(label,        name* := @nil, get, "What my tab is called").

initialise(BM,
           Title:title=[string],
           Persist:persists=[bool],
           Notes:notes=[bool]) :->
    default(Title, "PceEmacs bookmarks", TheTitle),
    default(Persist, @off, ThePersist),
    send_super(BM, initialise, bookmarks),
    send(BM, slot, persists, ThePersist),
    (   ThePersist == @off
    ->  assert(references_editor(BM))
    ;   true
    ),
    send(BM, append_window, new(D, dialog)),
    send(BM, fill_dialog),
    initial_directory(Dir),
    send(BM, append_window, new(W, emacs_bookmark_window(Dir, cwd)), D, below),
    (   (Persist == @on; Notes == @on)
    ->  send(BM, append_window,
             new(V, emacs_bookmark_note(size := size(40,4))), W, below),
        send(V, placeholder, "Make notes here"),
        send(V, font, normal),          % four lines, not eight: a pane
        send(V, ver_stretch, 0)         % is a strip, and the tree is what
    ;   true                            % the room in it is for
    ),
    send(BM, label, TheTitle),
    (   Persist == @on
    ->  send(@pce, exit_message, new(Msg, message(BM, save))),
        send(BM, slot, exit_message, Msg),
        ignore(send(BM, load))
    ;   true
    ),
    send(BM, current, @nil).            % nothing selected: no note to write

initial_directory(Dir) :-
    working_directory(CWD, CWD),
    (   atom_concat(Dir, /, CWD),
        Dir \== ''
    ->  true
    ;   Dir = CWD
    ).

%       A pane is closed the way every other pane of a window is, and
%       `@emacs_mark_list' makes itself again the next time anybody asks
%       for it.  What it holds is written out first: ->unlink is too late
%       for that, as <-tree is a window of mine and `pane_frame
%       ->delete_pane' has taken me out of my window by then.

close_pane(BM) :->
    "Write out what I hold, then close as any pane does"::
    (   get(BM, persists, @on)
    ->  ignore(send(BM, save))
    ;   true
    ),
    send_super(BM, close_pane).

close(BM) :->
    "Take me out of the window I am in"::
    send(BM, close_pane).

%       What my tab is called.  It is set rather than taken from my name,
%       as a hit list is called after what was searched for; see
%       find_references_editor/2.

label(BM, Label:name) :->
    "Name me and the tab I am in"::
    send(BM, slot, label, Label),
    (   get(BM, pane_frame, Frame)
    ->  ignore(send(Frame, update_tab_label)),
        ignore(send(Frame, update_label))
    ;   true
    ).

pane_label(BM, Label:name) :<-
    "What my tab is called"::
    get(BM, slot, label, Label),
    Label \== @nil.

fill_dialog(BM) :->
    get(BM, window, dialog, D),
    send(D, pen, 0),
    send(D, gap, size(0, 5)),
    send(BM, add_pin),
    send(D, append, new(TB, tool_bar(BM)), right),
    (   get(BM, persists, @on)
    ->  send_list(TB, append,
                  [ tool_button(save, resource(save), 'Save bookmarks'),
                    gap
                  ])
    ;   true
    ),
    send_list(TB, append,
              [ tool_button(goto, resource(open), 'Open editor'),
                tool_button(cut,  resource(cut),  'Delete selection')
              ]),
    send(D, resize_message, message(D, layout, @arg2)).

add_pin(BM) :->
    (   get(BM, persists, @off)
    ->  get(BM, window, dialog, D),
        send(D, append, new(Pin, bitmap(image(resource(pin)))), right),
        send(Pin, name, pin),
        get(@pce, convert, normal, font, Font),
        get(Font, ascent, RefH),
        send(Pin, reference, point(0, RefH)),
        send(Pin, recogniser,
             click_gesture(left, '', single, message(BM, toggle_pinned))),
        send(Pin, help_message, tag,
             'Pin: keep this view and open next result in a new window')
    ;   true
    ).

unlink(BM) :->
    retractall(references_editor(BM)),
    get(BM, exit_message, Msg),
    (   Msg == @nil
    ->  true
    ;   get(@pce, exit_messages, Chain),
        send(Chain, delete_all, Msg),
        ignore(send(BM, save))          % a ->unlink that fails leaves the
    ),                                  % window half taken apart
    send_super(BM, unlink).

toggle_pinned(BM) :->
    "Toggle the pinned state"::
    get(BM, pinned, P0),
    get(P0, negate, P1),
    send(BM, pinned, P1).

pinned(BM, Pinned:bool) :->
    "Set pinned state and update pin button icon"::
    send(BM, slot, pinned, Pinned),
    get(BM, window, dialog, D),
    get(D, member, pin, Btn),
    (   Pinned == @off
    ->  send(Btn, image, image(resource(pin)))
    ;   send(Btn, image, image(resource(pinned)))
    ).

clear(BM) :->
    "Remove all bookmarks from the tree"::
    get(BM, window, emacs_bookmark_window, BW),
    send(BM, current, @nil),
    send(BW, clear),
    initial_directory(CWD),
    send(BW, root, emacs_toc_bookmark_folder(CWD, directory)).

tree(BM, Tree:toc_tree) :<-
    get(BM, window, emacs_bookmark_window, W),
    get(W, tree, Tree).

view(BM, V:emacs_bookmark_note) :<-
    "View for annotations"::
    get(BM, window, emacs_bookmark_note, V).

selection(BM, Sel:'name|emacs_bookmark') :<-
    get(BM, window, emacs_bookmark_window, W),
    get(W, selection, Sel0),
    get(Sel0, map, @arg1?identifier, Sel1),
    get(Sel1, head, Sel).

:- pce_group(edit).

goto(BM) :->
    "Edit current selection"::
    get(BM, window, emacs_bookmark_window, W),
    (   get(BM, selection, Sel),
        send(Sel, instance_of, emacs_bookmark)
    ->  send(W, open_node, Sel)
    ;   send(BM, report, warning, 'No selection')
    ).

cut(BM) :->
    "Delete selected nodes"::
    get(BM, window, emacs_bookmark_window, W),
    (   get(W, selection, Nodes),
        \+ send(Nodes, empty)
    ->  send(BM, current, @nil),
        get(Nodes, copy, Copy),         % deleting takes them out of it
        send(Copy, for_all, message(BM, delete_node, @arg1))
    ;   send(BM, report, warning, 'No selection')
    ).

delete_node(BM, Node:toc_node) :->
    "Delete Node, forgetting the bookmarks below it"::
    (   get(BM, persists, @on)
    ->  get(Node, bookmarks, Marks),
        send(Marks, for_all, message(BM, forget, @arg1))
    ;   true
    ),
    send(Node, delete_tree).

forget(_BM, Mark:emacs_bookmark) :->
    "Take Mark out of the store"::
    get(Mark, id, Id),
    bookmark_store_forget(Id).

:- pce_group(interface).

bookmark(F, BM:emacs_bookmark, Sort:[bool], Store:[bool]) :->
    "Append a bookmark"::
    (   get(F, persists, @on)
    ->  send(BM, slot, persists, @on)
    ;   true
    ),
    get(BM, file_name, FileName),
    get(F, tree, Tree),
    (   between(1, 1000, _),
        get(Tree, root, Root),
        get(Root, identifier, RootPath),
        (   (   RootPath == @nil
            ;   send(FileName, prefix, RootPath)
            )
        ->  !
        ;   parent_directory(RootPath, Parent)
        ->  send(Tree, root,
                 emacs_toc_bookmark_folder(Parent),
                 @on),
            fail
        ;   %  The root is a root of the file system and the bookmark
            %  is not below it: on Windows it is on another drive.
            %  What holds them all is the root with no path of its own.
            send(Tree, root,
                 emacs_toc_bookmark_folder(/),
                 @on),
            !
        )
    ),
    send(Tree?root, append, BM, Sort),
    (   Store == @off
    ->  true
    ;   send(BM, store)
    ).

%!  parent_directory(+Dir, -Parent) is semidet.
%
%   The directory holding Dir, failing when Dir is a root of the file
%   system.  file_directory_name/2 does not say so itself: it answers
%   "C:" for "C:/" and then "." for "C:", neither of which is a
%   directory above the one asked about.

parent_directory(Dir, Parent) :-
    file_directory_name(Dir, Parent),
    Parent \== Dir,
    send(Dir, prefix, Parent).

append_hit(F, Buffer:emacs_buffer, Start:int, End0:[int]) :->
    "Add bookmark for indicated line"::
    (   End0 == @default
    ->  get(Buffer, scan, Start, line, 0, end, End)
    ;   End = End0
    ),
    get(Buffer, scan, Start, line, 0, start, SOL),
    get(Buffer, scan, Start, line, 0, end,  EOL),
    get(Buffer, contents, SOL, EOL-SOL, Title),
    send(Title, translate, '\t', ' '),
    get(Buffer, line_number, SOL, Line),
    LinePos is Start-SOL,
    Length is End-Start,
    debug(bookmark,
          'Created bookmark ~p[~d]: ~p/~p/~p~n',
          [Start, End-Start, Line, LinePos, Length]),
    (   get(Buffer, file, File),
        File \== @nil,
        get(File, absolute_path, FileName)
    ->  true
    ;   send(Buffer, report, warning, 'No associated file'),
        fail
    ),
    send(F, bookmark,
         new(BM, emacs_bookmark(FileName, Line, LinePos, Length,
                                Title))),
    send(BM, link, Buffer),
    send(F, open).

%   ->lsp_add
%
%   Add  a  hit  from  an  LSP  server.  If  the  file  is  loaded  used
%   ->append_hit, else create the hit as a non-loaded file.
%
%   `@emacs' rather than my <-application: every window of the IDE belongs
%   to @prolog_ide, and which buffers are open is PceEmacs's to answer.

lsp_add(F, File:name, LSPRange:prolog, Title:'[string]*') :->
    "Add an LSP position"::
    #{start:RangeStart, end:RangeEnd} :< LSPRange,
    #{line:Line, character:LinePos} :< RangeStart,
    #{line:EndLine, character:EndPos} :< RangeEnd,
    Line1 is Line+1,
    (   get(@emacs, file_buffer, File, Buffer)
    ->  get(Buffer, lsp_offset, Line, LinePos, Start),
        get(Buffer, lsp_offset, EndLine, EndPos, End),
        send(F, append_hit, Buffer, Start, End)
    ;   Length is EndPos-LinePos,
        absolute_file_name(File, FileName),
        (   Title == @default
        ->  file_line(File, Line, TheTitle),
            send(TheTitle, translate, '\t', ' ')
        ;   Title == @nil
        ->  TheTitle = ""
        ;   TheTitle = Title
        ),
        send(F, bookmark,
             emacs_bookmark(FileName, Line1, LinePos, Length,
                            TheTitle))
    ).

file_line(File, LineNo, Line) :-
    setup_call_cleanup(
        open(File, read, In),
        ( forall(between(1,LineNo,_), skip(In, 0'\n)),
          read_string(In, "\n", "\r", _Sep, Line)
        ),
        close(In)).

loaded_buffer(F, TB:emacs_buffer) :->
    "PceEmacs has loaded a file"::
    get(F, tree, Tree),
    send(Tree?root, loaded_buffer, TB).

%       PceEmacs re-colours a buffer whenever it falls idle, and that
%       moves the bookmarks in it; writing each of those down would fill
%       the log while the user types.  A bookmark says where it is in the
%       file on disk, so what is worth writing down is a buffer saved.

update_bookmarks(F, TB:emacs_buffer) :->
    "PceEmacs has saved this buffer"::
    (   get(F, persists, @on)
    ->  Then = message(@arg1, send_hyper, bookmark, store)
    ;   Then = @default
    ),
    send(TB, for_all_fragments,
         if(message(@arg1, send_hyper, bookmark, update), Then)).


current(F, BM:emacs_bookmark*, UpdateSelection:[bool]) :->
    "Make this bookmark the current one"::
    (   UpdateSelection \== @off,
        get(F, window, emacs_bookmark_window, BW)
    ->  send(BW, selection, BM)
    ;   true
    ),
    (   get(F, view, View)
    ->  send(View, save_note),
        send(View, delete_hypers, bookmark),
        get(View, editor, Editor),
        get(Editor, text_image, TextImage),
        (   BM == @nil
        ->  send(Editor, clear),
            send(Editor, editable, @off),
            send(Editor, placeholder, "Select a bookmark to annotate"),
            send(TextImage, background, grey80)
        ;   new(_, hyper(View, BM, bookmark, editor)),
            send(Editor, placeholder, "Make notes here"),
            (   get(BM, note, Note),
                Note \== @nil
            ->  send(Editor, contents, Note),
                send(Editor, modified, @off)
            ;   true
            ),
            send(Editor, editable, @on),
            get(TextImage, class_variable_value, background, BG),
            send(TextImage, background, BG)
        )
    ;   true
    ).

:- pce_group(file).

%       A bookmark is written down the moment it is made, annotated,
%       moved or thrown away -- see library(emacs/bookmark_store) -- so
%       there is nothing left to write out here.  What ->save still has
%       to do is put away the note the user is typing, which belongs to
%       the bookmark it is about only once the caret leaves it, and tidy
%       the log while we are at it.

save(BM) :->
    "Put away the note being typed and tidy the store"::
    send(BM, current, @nil),
    (   get(BM, persists, @on),
        bookmark_store_file(File)
    ->  bookmark_store_tidy,
        send(BM, report, status, 'Saved bookmarks to %s', File)
    ;   true
    ).

load(BM) :->
    "Load bookmarks from the store"::
    bookmark_store_load(Bookmarks),
    forall(member(Bookmark, Bookmarks),
           load_bookmark(Bookmark, BM)).

load_bookmark(bookmark(Id, File0, Line, Pos, Len, Title, Stamp, Note), BM) :-
    (   absolute_file_name(File0,
                           [ access(read),
                             file_errors(fail)
                           ],
                           File)
    ->  new(Created, date),
        FStamp is float(Stamp),             % avoid overflow
        send(Created, posix_value, FStamp),
        new(M, emacs_bookmark(File, Line, Pos, Len, Title,
                              Created, Note)),
        send(M, slot, id, Id),
        send(BM, bookmark, M,
             @off,                  % do not sort
             @off),                 % and do not write it back
        (   get(@emacs, file_buffer, File, Buffer)
        ->  send(M, link, Buffer)
        ;   true
        )
    ;   true
    ).

:- pce_end_class(emacs_bookmark_editor).

%!  find_references_editor(+Title, -BM) is det.
%
%   True when BM is an emacs_bookmark_editor ready to display references
%   with the given Title. Reuses an   existing unpinned editor (clearing
%   it first); creates a fresh one when none exists or all are pinned.

find_references_editor(Title, BM) :-
    references_editor(BM),
    get(BM, pinned, @off),
    !,
    send(BM, clear),
    send(BM, label, string('References to %s', Title)),
    send(BM, expose).
find_references_editor(Title, BM) :-
    new(BM, emacs_bookmark_editor(string('References to %s', Title), @off)).

:- pce_begin_class(emacs_bookmark_window, toc_window).

initialise(BW, Root:[name], Kind:[{directory,cwd,file}]) :->
    default(Root, /, TheRoot),
    send_super(BW, initialise),
    send(BW, root, emacs_toc_bookmark_folder(TheRoot, Kind)).

open_node(BW, Id:any) :->
    "Open bookmark on double-click"::
    (   send(Id, instance_of, emacs_bookmark)
    ->  (   send(Id, exists)
        ->  ignore(send(Id, update)),
            send(@emacs, goto_source_location, Id)
        ;   (   get(Id, file_name, File),
                send(@display, confirm, BW, "PceEmacs",
                     'Marked file "%s" does not exist.\nDelete bookmark?',
                     File)
            ->  get(BW, node, Id, Node),
                send(BW?editor, delete_node, Node)
            ;   true
            )
        )
    ).

%       Not <-frame: that is the window of the IDE I am a pane of now.

editor(BW, BM:emacs_bookmark_editor) :<-
    "The pane I am part of"::
    get(BW, container, emacs_bookmark_editor, BM).

select_node(BW, Id:any) :->
    "User selected a node"::
    (   send(Id, instance_of, emacs_bookmark)
    ->  send(BW?editor, current, Id)
    ;   send(BW?editor, current, @nil, @off)
    ).

selection(BW, Sel:any*) :->
    (   Sel == @nil
    ->  send(BW?editor, current, @nil, @off)
    ;   true
    ),
    send_super(BW, selection, Sel).

:- pce_end_class.

/* The note on the bookmark that is selected.

A note is the user's to write and the bookmark's to keep, and the two are
only brought together when the writing stops.  That is when the caret
leaves the note -- ->input_focus below -- and when another bookmark is
selected, which is `emacs_bookmark_editor ->current'.  Closing the pane
and leaving PceEmacs both go through ->current as well, so a note is
never left only on the screen.
*/

:- pce_begin_class(emacs_bookmark_note, view,
                   "Annotation on the selected bookmark").

input_focus(V, Focus:bool) :->
    "Put the note away when the caret leaves me"::
    (   Focus == @off
    ->  ignore(send(V, save_note))
    ;   true
    ),
    send_super(V, input_focus, Focus).

save_note(V) :->
    "Give what has been typed to the bookmark it is about"::
    (   get(V, modified, @on),
        get(V, hypered, bookmark, BM)
    ->  send(BM, note, V?contents),
        send(V?editor, modified, @off)
    ;   true
    ).

:- pce_end_class(emacs_bookmark_note).

:- pce_begin_class(emacs_toc_bookmark_folder, toc_folder,
                   "Represent directory in bookmarks").

initialise(F, Path:name, Kind:[{directory,cwd,file}]) :->
    (   Path == /
    ->  (   has_drives
        ->  RootName = 'My Computer'
        ;   RootName = '/'
        ),
        send_super(F, initialise, RootName, @nil)
    ;   get(file(Path), base_name, BaseName),
        (   Kind == file
        ->  send_super(F, initialise,
                       text(BaseName, left, bold),
                       Path,
                       resource(file),
                       resource(file))
        ;   Kind == cwd
        ->  send_super(F, initialise,
                       new(T, text(BaseName,left,bold)), Path),
            send(T, colour, darkgreen)
        ;   send_super(F, initialise, BaseName, Path)
        )
    ).

collapsed(F, Val:bool*) :->
    "Disable toc_window expansion mechanism"::
    send_class(F, node, collapsed(Val)).

append(F, BM:emacs_bookmark, Sort:[bool]) :->
    "Append a bookmrk to a folder node"::
    get(BM, file_name, FileName),
    get(F, identifier, Path),
    (   Path == @nil                % this is the root
    ->  true
    ;   send(FileName, prefix, Path)
    ),
    get(F, sons, Sons),
    (   get(Sons, find, message(@arg1, append, BM), _)
    ->  true
    ;   sub_directory(Path, FileName, SubPath),
        (   SubPath == FileName
        ->  Kind = file
        ;   Kind = directory
        ),
        send(F, collapsed, @off),
        send(F, son, new(S, emacs_toc_bookmark_folder(SubPath, Kind))),
        send(S, append, BM),
        (   Sort \== @off
        ->  send(F, sort)
        ;   true
        )
    ;   send_class(F, node, collapsed(@off)),
        send(F, son, new(emacs_toc_bookmark(BM))),
        (   Sort \== @off
        ->  send(F, sort)
        ;   true
        )
    ).

sort(F) :->
    "Sort the nodes"::
    send(F, sort_sons, ?(@arg1, compare, @arg2)).

compare(F, N:toc_node, Diff:{smaller,equal,larger}) :<-
    "Put folders before files, otherwise alphabetical"::
    (   send(N, instance_of, toc_folder)
    ->  get(F, member, text, T0),
        get(N, member, text, T1),
        get(T0?string, compare, T1?string, Diff)
    ;   Diff = smaller
    ).

%!  sub_directory(+Dir, +File, -SubDir) is semidet.
%
%   Extend Dir with one segment "in the direction" of File. Fails if Dir
%   is not a prefix of File.

sub_directory(@nil, File, SubPath) :-
    !,
    (   has_drives
    ->  new(Re, regex('[a-zA-Z]:'))
    ;   new(Re, regex('/[^/]*'))
    ),
    send(Re, match, File),
    get(Re, register_value, File, 0, name, SubPath).
sub_directory(Path, File, SubPath) :-
    send(File, prefix, Path),
    file_directory_name(File, FileDir),
    (   same_file(FileDir, Path)
    ->  SubPath = File
    ;   sub_directory(Path, FileDir, SubPath)
    ).


bookmarks(F, Marks:chain) :<-
    "The bookmarks below me"::
    new(Marks, chain),
    send(F?sons, for_all, message(Marks, merge, @arg1?bookmarks)).

loaded_buffer(F, TB:emacs_buffer) :->
    "PceEmacs has loaded this buffer"::
    (   get(TB, file, File),
        File \== @nil,
        get(File, absolute_path, Path),
        get(F, identifier, Id),
        (   Id == @nil
        ->  true
        ;   send(Path, prefix, Id)
        )
    ->  send(F?sons, for_all,
             message(@arg1, loaded_buffer, TB))
    ;   true
    ).

:- pce_end_class(emacs_toc_bookmark_folder).

:- pce_begin_class(emacs_toc_bookmark, toc_file,
                   "Represent a bookmark").

class_variable(style_line, style,
               style(background := grey90,
                     colour := grey20)).
class_variable(style_title, style,
               style(font := fixed)).
class_variable(style_hit, style,
               style(font := fixed,
                     background := yellow)).

initialise(F, BM:emacs_bookmark) :->
    bookmark_label(F, BM, Label),
    send_super(F, initialise, Label, BM, @null_image),
    send(BM, slot, node, F).

bookmark_label(F, BM, Label) :-
    get(BM, line_no, Line),
    get(F, class_variable_value, style_line,  StyleLine),
    get(F, class_variable_value, style_title, StyleTitle),
    get(F, class_variable_value, style_hit,   StyleHit),
    bm_title(BM, StyleTitle, StyleHit, TitleBoxes),
    get(@pce, convert, normal, font, Font),
    get(Font, advance, 99999, LW),
    new(Label, parbox(10000, left,
                      grbox(parbox(LW, right,
                                   tbox(Line, StyleLine))),
                      hbox(5))),
    send_list(Label, append, TitleBoxes).

bm_title(BM, Style, StyleHit, Boxes) :-
    get(BM, title, Title),
    get(Title, size, TitleLen),
    get(BM, length, Len),
    Len \== @nil,
    Len > 0, Len < TitleLen,
    get(BM, line_pos, Start),
    Start \== @nil,
    !,
    End is Start+Len,
    get(Title, sub, 0,   Start, Pre),
    get(Title, sub, Start, End, Match),
    get(Title, sub, End,        Post),
    Boxes = [ tbox(Pre, Style),
              tbox(Match, StyleHit),
              tbox(Post, Style)
            ].
bm_title(BM, Style, _StyleHit, [tbox(Title, Style)]) :-
    get(BM, title, Title).

unlink(F) :->
    get(F, identifier, BM),
    send(BM, slot, node, @nil),
    send_super(F, unlink).

update(F) :->
    "Update label after changed bookmark"::
    get(F, identifier, BM),
    bookmark_label(F, BM, Label),
    send(F, label, Label).

append(_F, _BM:emacs_bookmark) :->
    "Can't append to a file"::
    fail.

bookmarks(F, Marks:chain) :<-
    "Just me"::
    get(F, identifier, BM),
    new(Marks, chain(BM)).

loaded_buffer(F, TB:emacs_buffer) :->
    "PceEmacs has loaded this buffer"::
    get(F, identifier, BM),
    get(BM, file_name, FileName),
    (   get(TB, file, File),
        File \== @nil,
        get(File, absolute_path, Path),
        Path == FileName
    ->  send(BM, link, TB)
    ;   true
    ).

compare(F, N2:toc_node, Diff:{smaller,equal,larger}) :<-
    (   send(N2, instance_of, emacs_toc_bookmark_folder)
    ->  Diff = larger
    ;   get(F, identifier, BM0),
        get(N2, identifier, BM1),
        (   get(BM0, file_name, F1),
            get(BM1, file_name, F2),
            get(file(F1), base_name, B1),
            get(file(F2), base_name, B2),
            get(B1, compare, B2, Diff),
            Diff \== equal
        ->  true
        ;   get(BM0, line_no, L0),
            get(BM1, line_no, L1),
            get(number(L0), compare, L1, Diff)
        )
    ).

:- pce_end_class(emacs_toc_bookmark).

:- pce_begin_class(emacs_bookmark, source_location,
                   "Bookmark in PceEmacs").

variable(title,    string,              get,  "Represented title").
variable(created,  date,                get,  "Date of creation").
variable(note,     string*,             get,  "Annotation").
variable(node,     emacs_toc_bookmark*, get,  "Visualiser").
variable(id,       name* := @nil,       none, "Name in the store").
variable(persists, bool := @off,        get,  "I am kept in the store").

initialise(BM,
           File:file=name, Line:line=int,
           LinePos:position=[int],
           Length:length=[int],
           Title:title=string,
           Created:created=[date], Note:note=[string]*) :->
    send_super(BM, initialise, File, Line, LinePos, Length),
    send(BM, slot, title, Title),
    (   Created == @default
    ->  send(BM, slot, created, new(date))
    ;   send(BM, slot, created, Created)
    ),
    default(Note, @nil, TheNote),
    send(BM, slot, note, TheNote).

%       <-term does not refresh me first.  What refreshes a bookmark is
%       ->update, and the moment worth writing one down is when the buffer
%       it is in has been saved -- see `emacs_bookmark_editor
%       ->update_bookmarks'.  Until then a bookmark says where it is in
%       the file on disk, which is where another session reading it back
%       will look.

term(BM, Term:prolog) :<-
    "Describe bookmark as a Prolog term"::
    get(BM, id, Id),
    get(BM, file_name, File),
    get(BM, line_no, Line),
    get(BM, line_pos, LinePos),
    get(BM, length, Length),
    get(BM?title, value, Title),
    get(BM?created, posix_value, Stamp),
    (   get(BM, note, Note),
        Note \== @nil
    ->  get(Note, value, NoteText)
    ;   NoteText = ''
    ),
    Term = bookmark(Id, File, Line, LinePos, Length, Title, Stamp, NoteText).

id(BM, Id:name) :<-
    "The name I am stored under, made when first asked for"::
    (   get(BM, slot, id, Id0),
        Id0 \== @nil
    ->  Id = Id0
    ;   bookmark_store_id(Id),
        send(BM, slot, id, Id)
    ).

note(BM, Note:string*) :->
    "Annotate me and write that down"::
    (   get(BM, note, Note0),
        same_note(Note0, Note)
    ->  true
    ;   send(BM, slot, note, Note),
        send(BM, store)
    ).

%       An editor is `modified' after anything at all has been typed in
%       it, a word put there and taken away again included, and the note
%       is put away whenever the caret leaves.  Comparing the text keeps
%       those out of the store.  A note typed away to nothing is no note,
%       or visiting a bookmark that has none would write a record.

same_note(A, B) :-
    note_text(A, Text),
    note_text(B, Text).

note_text(@nil, '') :- !.
note_text(Note, Text) :-
    get(Note, value, Text).

store(BM) :->
    "Write me to the store, if that is where I live"::
    (   get(BM, persists, @on)
    ->  get(BM, term, Term),
        bookmark_store_save(Term)
    ;   true
    ).

exists(BM) :->
    "Test whether associated file exists"::
    get(BM, file_name, File),
    send(file(File), exists).

:- pce_group(buffer).

link(BM, To:text_buffer) :->
    "Link the bookmark using a fragment"::
    get(BM, line_no, Line),
    get(To, scan, 0, line, Line-1, start, SOL),
    (   get(BM, line_pos, LinePos),
        LinePos \== @nil
    ->  Start is SOL+LinePos
    ;   Start = SOL
    ),
    (   get(BM, length, Length),
        Length \== @nil
    ->  true
    ;   get(To, scan, Start, line, 0, end, End),
        Length is End-Start
    ),
    debug(bookmark,
          'Created bookmark fragment on ~p ~p[~p]~n',
          [To, Start, Length]),
    new(_, emacs_bookmark_hyper(BM,
                                emacs_bookmark_fragment(To, Start, Length))).

update(BM) :->
    "If bookmark is linked, update <-line_no"::
    get(BM, hypered, fragment, Fragment),
    (   get(Fragment, text_buffer, TB),
        TB \== @nil
    ->  get(Fragment, start, Start),
        get(Fragment, length, Length),
        get(TB, line_number, Start, Line),
        get(TB, scan, Start, line, 0, start, SOL),
        LinePos is Start-SOL,
        get(TB, scan, Start, line, 0, end, EOL),
        get(TB, contents, SOL, EOL-SOL, Title),
        send(Title, translate, '\t', ' '),
        update(BM, line_no, Line, Modified),
        update(BM, line_pos, LinePos, Modified),
        update(BM, length, Length, Modified),
        update(BM, title, Title, Modified),
        (   Modified == true
        ->  send(BM, modified)
        ;   true
        )
    ;   true                        % destroy?
    ).

update(BM, title, Title, Modified) =>
    (   send(BM?title, equal, Title)
    ->  true
    ;   send(BM, slot, title, Title),
        Modified = true
    ).
update(BM, Slot, Value, Modified) =>
    (   get(BM, Slot, Value)
    ->  true
    ;   send(BM, slot, Slot, Value),
        debug(bookmark, 'Updated ~p of ~p to ~p~n', [BM, Slot, Value]),
        Modified = true
    ).

modified(BM) :->
    "Bookmark parameters have been modified"::
    get(BM, node, Node),
    (   Node == @nil                % Not associated to a current buffer
    ->  true
    ;   send(Node, update)
    ).

:- pce_end_class(emacs_bookmark).


:- pce_begin_class(emacs_bookmark_fragment, fragment).

initialise(F, TB:text_buffer, Start:int, Length:int) :->
    send_super(F, initialise, TB, Start, Length, bookmark),
    (   Length > 0
    ->  send(F, include, end)
    ;   true
    ).

:- pce_end_class(emacs_bookmark_fragment).

:- pce_begin_class(emacs_bookmark_hyper, hyper,
                   "Hyper from bookmark to fragment").

initialise(H, BM:emacs_bookmark, To:fragment) :->
    send_super(H, initialise, BM, To, fragment, bookmark).

unlink_from(H) :->
    "Bookmark is deleted"::
    get(H, to, Fragment),
    free(Fragment),
    free(H).

unlink_to(H) :->
    "Fragment is deleted, update line"::
    get(H, from, BM),
    send(BM, update),
    free(H).

:- pce_end_class.

                 /*******************************
                 *             MISC             *
                 *******************************/

:- dynamic
    has_drives/1.

%       See whether there is only one logical root in the filesystem or
%       there are multiple.

has_drives :-
    has_drives(True),
    !,
    True = true.
has_drives :-
    (   get(directory(.), roots, Roots),
        get(Roots, size, 1)
    ->  assert(has_drives(false)),
        fail
    ;   assert(has_drives(true))
    ).
