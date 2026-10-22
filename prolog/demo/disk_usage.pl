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

:- module(disk_usage_demo,
          [ disk_usage_demo/0,
            disk_usage_demo/1                   % +Directory
          ]).
:- use_module(library(pce)).
:- use_module(library(plot/hierarchy_chart)).
:- use_module(library(file_item)).          % directory_item
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(open_document)).

/** <module> Disk usage analyser

Scans a folder and shows how its disk space is distributed over its
sub folders, as rings or as a treemap, in the style of GNOME's _baobab_.
Hover a segment to see its size, click it to zoom in and click the
centre (or the top bar of the treemap) to zoom out.  The popup of the
chart opens the file or folder below the pointer, scans that folder or
the parent of the folder shown, and switches between the rings and the
treemap.

Scanning runs in a thread of its own, so the window stays responsive
and shows the progress.  The sizes are those of the files (as
size_file/2), not the blocks they use on disk.  Symbolic links are not
followed.  A file is shown on its own if it takes at least 1% of its
folder; the other files make up the size of the folder itself.

Run with:

    ?- disk_usage_demo.               % the working directory
    ?- disk_usage_demo('/usr/lib').
*/

disk_usage_demo :-
    working_directory(Dir, Dir),
    disk_usage_demo(Dir).

disk_usage_demo(Dir) :-
    new(F, disk_usage_frame),
    send(F, open),
    send(F, scan, Dir).


:- pce_begin_class(disk_usage_frame, frame,
                   "Show the disk usage of a folder").

variable(scanner, int*, get, "Number of the scan in progress, if any").

initialise(F) :->
    send_super(F, initialise, 'Disk usage'),
    send(F, append, new(D, dialog)),
    send(D, append,
         new(DI, directory_item(folder, '', message(F, scan, @arg1)))),
    send(DI, length, 40),
    send(D, append, button(scan, message(F, scan, DI?selection)), right),
    send(D, append,
         new(V, menu(view, choice, message(F?chart_window, view, @arg1))),
         right),
    send_list(V, append, [rings, treemap]),
    send(new(W, hierarchy_window), below, D),
    send(V, selection, W?view),
    send(W, view_message, message(V, selection, @arg1)),
    send(W, size, size(600, 600)),
    send(W, unit, bytes),
    send(W?chart, select_message, message(F, selected, @arg1)),
    get(W, popup, P),
    forall(popup_item(F, Item),
           send(P, insert_before, Item, zoom_out)),
    send(new(R, dialog), below, W),
    send(R, append, label(reporter)),
    send(R, gap, size(5, 2)).

popup_item(F, menu_item(open, message(F, open_node),
                        condition := message(F, has_popup_node))).
popup_item(F, menu_item(scan_folder, message(F, scan_node),
                        condition := message(F, can_scan_node))).
popup_item(F, menu_item(scan_parent, message(F, scan_parent),
                        condition := message(F, has_parent),
                        end_group := @on)).

unlink(F) :->
    send(F, stop_scan),
    send_super(F, unlink).

chart_window(F, W:hierarchy_window) :<-
    get(F, member, hierarchy_window, W).

scan(F, Dir:name) :->
    "Scan Dir in the background"::
    (   exists_directory(Dir)
    ->  true
    ;   send(F, report, error, 'No such folder: %s', Dir),
        fail
    ),
    send(F, stop_scan),
    absolute_file_name(Dir, Abs, [file_type(directory)]),
    get(F, member, dialog, D),
    get(D, member, folder, DI),
    send(DI, selection, Abs),
    send(F?chart_window, root, @nil),
    send(F, report, progress, 'Scanning %s ...', Abs),
    flag(disk_usage_scan, Id, Id+1),
    thread_create(scan_thread(F, Id, Abs), _, [detached(true)]),
    send(F, slot, scanner, Id).

stop_scan(F) :->
    "Stop a scan in progress"::
    get(F, scanner, Id),
    (   Id == @nil
    ->  true
    ;   send(F, slot, scanner, @nil),
        assertz(stopped(Id))
    ).

progress(F, Files:int, Bytes:int) :->
    "Report progress of the scan"::
    format_size(Bytes, Size),
    send(F, report, progress, 'Scanned %d files, %s', Files, Size).

scanned(F, Id:int, Term:prolog) :->
    "The scan is complete; show it"::
    (   get(F, scanner, Id)
    ->  send(F, slot, scanner, @nil),
        hierarchy_from_term(Term, Root),
        send(F?chart_window, root, Root),
        get(Root, total, Total),
        format_size(Total, Size),
        get(Root, data, Dir),
        send(F, report, status, '%s: %s', Dir, Size)
    ;   true                            % a scan that was stopped
    ).

scan_failed(F, Id:int, Message:string) :->
    "The scan raised an error"::
    (   get(F, scanner, Id)
    ->  send(F, slot, scanner, @nil),
        send(F, report, error, 'Scan failed: %s', Message)
    ;   true
    ).

%   The popup acts on the node below the pointer when it opened.  The
%   data of a node is the path of its file or folder.

popup_path(F, Path:name) :<-
    "Path of the node the popup is on"::
    get(F?chart_window, popup_node, Node),
    Node \== @nil,
    get(Node, data, Path).

has_popup_node(F) :->
    get(F, popup_path, _).

open_node(F) :->
    "Open the file or folder the popup is on"::
    get(F, popup_path, Path),
    open_document(Path).

can_scan_node(F) :->
    "True if the popup is on a folder below the top"::
    get(F, popup_path, Path),
    exists_directory(Path),
    \+ get(F?chart_window?root, data, Path).

scan_node(F) :->
    "Scan the folder the popup is on"::
    get(F, popup_path, Path),
    send(F, scan, Path).

parent_folder(F, Parent:name) :<-
    "Parent of the folder shown"::
    get(F, member, dialog, D),
    get(D, member, folder, DI),
    get(DI, selection, Dir),
    Dir \== '',
    file_directory_name(Dir, Parent),
    Parent \== Dir.

has_parent(F) :->
    get(F, parent_folder, _).

scan_parent(F) :->
    "Scan the parent of the folder shown"::
    get(F, parent_folder, Parent),
    send(F, scan, Parent).

selected(F, Node:hierarchy_node) :->
    "A file was clicked"::
    get(Node, data, Path),
    get(Node, total, Total),
    format_size(Total, Size),
    send(F, report, status, '%s: %s', Path, Size).

:- pce_end_class(disk_usage_frame).

format_size(Bytes, Text) :-
    hierarchy_chart:format_value(bytes, Bytes, Text).


                 /*******************************
                 *           SCANNING           *
                 *******************************/

%   scan_thread(+Frame, +Id, +Dir)
%
%   Scan Dir and pass the result to Frame.  Id identifies the scan.
%   Progress is reported every 0.2 seconds.  The frame stops the scan
%   by asserting stopped(Id), which the scan checks for each file.
%   Signalling the thread instead could interrupt it outside the
%   catch/3.  An error ends the scan and is reported in the frame.

:- dynamic stopped/1.

scan_thread(Frame, Id, Dir) :-
    get_time(T0),
    nb_setval(du_progress, p(Id, 0, 0, T0)),
    catch(scan_dir(Frame, Dir, Term, _), E, true),
    \+ retract(stopped(Id)),
    E \== stop_scan,
    !,
    (   var(E)
    ->  to_frame(Frame, scanned(Id, Term))
    ;   message_string(E, Message),
        to_frame(Frame, scan_failed(Id, Message))
    ).
scan_thread(_, Id, _) :-
    retractall(stopped(Id)).

message_string(E, String) :-
    '$messages':translate_message(E, Lines, []),
    with_output_to(string(Text),
                   print_message_lines(current_output, '', Lines)),
    normalize_space(string(String), Text).

%   scan_dir(+Frame, +Dir, -Term, -Total)
%
%   Term is node(Name, Own, Children, [data(Dir)]), where Children are
%   the terms for the sub folders and for the files that take at least
%   1% of Dir.  Own is the size of the other files and Total the size
%   of everything in Dir.

scan_dir(Frame, Dir, node(Name, Own, Children, [data(Dir)]), Total) :-
    file_base_name(Dir, Name0),
    (   Name0 == ''
    ->  Name = Dir                      % the root folder
    ;   Name = Name0
    ),
    catch(directory_files(Dir, Entries), _, Entries = []),
    foldl(scan_entry(Frame, Dir), Entries, [], Items),
    partition(is_dir_item, Items, Dirs, Files),
    maplist(item_size, Items, Sizes),
    sum_list(Sizes, Total),
    Min is Total/100,
    partition(large_file(Min), Files, Large, Small),
    maplist(item_size, Small, SmallSizes),
    sum_list(SmallSizes, Own),
    maplist(item_term, Dirs, DirTerms),
    maplist(item_term, Large, FileTerms),
    append(DirTerms, FileTerms, Children).

scan_entry(_, _, Entry, Items, Items) :-
    special(Entry),
    !.
scan_entry(Frame, Dir, Entry, Items0, [Item|Items0]) :-
    directory_file_path(Dir, Entry, Path),
    (   is_link(Path)                   % do not follow symbolic links
    ->  Item = file(Entry, Path, 0)
    ;   exists_directory(Path)
    ->  scan_dir(Frame, Path, Term, Size),
        Item = dir(Term, Size)
    ;   catch(size_file(Path, Size), _, Size = 0),
        progress(Frame, Size),
        Item = file(Entry, Path, Size)
    ),
    !.
scan_entry(_, _, _, Items, Items).

special(.).
special(..).

%   is_link(+Path)
%
%   True if Path is a symbolic link.  read_link/3 raises an error on a
%   link that loops, which is a link all the same.

is_link(Path) :-
    catch(read_link(Path, _, _), error(_, _), true).

is_dir_item(dir(_, _)).

item_size(dir(_, Size), Size).
item_size(file(_, _, Size), Size).

large_file(Min, file(_, _, Size)) :-
    Size > 0,
    Size >= Min.

item_term(dir(Term, _), Term).
item_term(file(Name, Path, Size), node(Name, Size, [], [data(Path)])).

%   to_frame(+Frame, +Message)
%
%   Send Message to Frame in the thread of xpce, unless the frame has
%   been destroyed in the meantime.

to_frame(Frame, Message) :-
    in_pce_thread(( object(Frame)
                  ->  send(Frame, Message)
                  ;   true
                  )).

progress(Frame, Size) :-
    nb_getval(du_progress, p(Id, N0, B0, T0)),
    (   stopped(Id)
    ->  throw(stop_scan)
    ;   true
    ),
    N is N0+1,
    B is B0+Size,
    get_time(Now),
    (   Now - T0 > 0.2
    ->  to_frame(Frame, progress(N, B)),
        nb_setval(du_progress, p(Id, N, B, Now))
    ;   nb_setval(du_progress, p(Id, N, B, T0))
    ).
