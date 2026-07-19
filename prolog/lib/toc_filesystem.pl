/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2012, University of Amsterdam
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

:- module(toc_filesystem, []).
:- use_module(library(pce)).
:- use_module(library(pce_toc)).
:- require([ default/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  library  refines   library(pce_toc)    with   filesystem  browsing
capabilities. It again is designed for  further subclassing to deal with
domain specific subclasses as demonstrated in the Prolog Navigator.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(toc_directory, toc_folder,
                   "Represent a directory").

variable(name,  name,   get, "Base name of the folder").

initialise(F, Dir:directory, Show:[name]) :->
    default(Show, name, Selector),
    get(Dir, Selector, Label),
    send_super(F, initialise, Label, Dir),
    get(Dir, name, BaseName),
    send(F, slot, name, BaseName).

show(F, Show:name) :->
    "Determine selector visualised"::
    get(F, identifier, Dir),
    get(Dir, Show, Label),
    send(F, label, Label).

expand(F) :->
    "Expand this directory"::
    send(F, update).

expand_all(F) :->
    "Expand this directory recursively"::
    send(F, collapsed, @off),
    (   object(F)                   % ->update deletes vanished directories
    ->  send(F?sons, for_all, message(@arg1, async_expand))
    ;   true
    ).

async_expand(F) :->
    "Run ->expand_all in the next GUI iteration"::
    Delay is random_float/5,
    new(T, timer(Delay, message(@receiver, send_hyper,
                                node, expand_all))),
    new(_, hyper(T, F, node, timer)),
    send(T, start, once).

refresh(F) :->
    "Update for possible changes"::
    get(F, identifier, Dir),
    (   send(Dir, modified)
    ->  send(F, update)
    ;   true
    ),
    (   object(F)                   % ->update deletes vanished directories
    ->  send(F?sons, for_all,
             if(message(@arg1, has_send_method, refresh),
                message(@arg1, refresh)))
    ;   true
    ).

update(F) :->
    "Really update"::
    get(F, identifier, Dir),
    (   send(Dir, exists)
    ->  get(F?tree, window, FB),
        (   send(FB, has_get_method, file_pattern)
        ->  get(FB, file_pattern, Regex)
        ;   Regex = @default
        ),
        new(SubDirNames, chain),
        new(SubFileNames, chain),
        send(Dir, scan, SubFileNames, SubDirNames, Regex),

        get(F?sons, map, @arg1?name, Labels), % delete removed ones
        send(Labels, subtract, SubFileNames),
        send(Labels, subtract, SubDirNames),
        send(F?sons, for_all,
             if(message(Labels, member, @arg1?name),
                message(@arg1, delete_tree))),

        (   send(SubDirNames, empty),
            send(SubFileNames, empty)
        ->  true
        ;   send(SubDirNames, for_all, message(F, ensure_dir, @arg1)),
            send(SubFileNames, for_all, message(F, ensure_file, @arg1)),
            send(F, sort_sons)
        )
    ;   send(F, delete_tree)
    ).


ensure_dir(F, SubDir:name) :->
    "Ensure we have a subdirectory with this name"::
    (   get(F?sons, find, @arg1?name == SubDir, Node)
    ->  (   send(Node, instance_of, toc_directory)
        ->  true
        ;   send(Node, delete_tree),
            send(F, make_dir, SubDir)
        )
    ;   send(F, make_dir, SubDir)
    ).

make_dir(F, Name:name) :->
    "Add a subdirectory"::
    get(F, identifier, Dir),
    get(Dir, directory, Name, SubDir),
    get(F?tree, window, FB),
    get(FB, make_dir_node, SubDir, Node),
    send(FB, son, F, Node).

ensure_file(F, File:name) :->
    "Ensure file is displayed"::
    (   get(F?sons, find, @arg1?name == File, Node)
    ->  (   send(Node, instance_of, toc_directory)
        ->  send(Node, delete_tree),
            send(F, make_file, File)
        ;   true
        )
    ;   send(F, make_file, File)
    ).

make_file(F, Name:name) :->
    "Add a subdirectory"::
    get(F, identifier, Dir),
    get(Dir, file, Name, File),
    get(F?tree, window, FB),
    get(FB, make_file_node, File, Node),
    send(FB, son, F, Node).

sort_sons(F) :->
    "Sort the sons"::
    send_super(F, sort_sons, ?(F, compare_sons, @arg1, @arg2)).

compare_sons(_, S1:node, S2:node, Diff:{smaller,equal,larger}) :<-
    "Directories above files, both in alpabetical order"::
    (   send(S1, instance_of, toc_directory)
    ->  (   send(S2, instance_of, toc_directory)
        ->  get(S1?name, compare, S2?name, Diff)
        ;   Diff = smaller
        )
    ;   (   send(S2, instance_of, toc_directory)
        ->  Diff = larger
        ;   get(S1?label, compare, S2?label, Diff)
        )
    ).

:- pce_end_class(toc_directory).


:- pce_begin_class(toc_roots, toc_folder,
                   "Virtual root above the file system roots").

initialise(TR, Label:[name]) :->
    "Create from label, default \"This computer\""::
    default(Label, 'This computer', TheLabel),
    send_super(TR, initialise, TheLabel).

expand(TR) :->
    "Expand into the file system roots"::
    send(TR, update).

expand_all(TR) :->
    "Expand recursively"::
    send(TR, collapsed, @off),
    send(TR?sons, for_all, message(@arg1, expand_all)).

show_all_files(TR) :->
    "Ensure all roots are shown"::
    send(TR, update).

refresh(TR) :->
    "Update for possible changes"::
    send(TR, update),
    send(TR?sons, for_all,
         if(message(@arg1, has_send_method, refresh),
            message(@arg1, refresh))).

update(TR) :->
    "Ensure a node for each file system root"::
    get(TR?tree, window, FB),
    get(FB, root_directories, Roots),
    send(Roots, for_all, message(TR, ensure_root, @arg1)),
    send(TR, sort_sons).

ensure_root(TR, Dir:directory) :->
    "Ensure a node for the root directory Dir"::
    get(TR?tree, window, FB),
    (   \+ send(Dir, exists)        % empty or disconnected drive
    ->  true
    ;   get(FB, existing_dir_node, Dir, _)
    ->  true
    ;   get(FB, make_dir_node, Dir, Node),
        send(Node, show, path),
        send(FB, son, TR, Node)
    ).

sort_sons(TR) :->
    "Sort the roots by path"::
    send_super(TR, sort_sons, ?(TR, compare_sons, @arg1, @arg2)).

compare_sons(_TR, S1:node, S2:node, Diff:{smaller,equal,larger}) :<-
    "Compare the labels of the roots"::
    get(S1?label, compare, S2?label, Diff).

:- pce_end_class(toc_roots).


:- pce_begin_class(toc_filesystem, toc_window,
                   "Table-of-content based on directories").

class_variable(auto_refresh, int*, 10,
               "Check directories for modifications after this interval").

variable(refresh_timer, timer*, get, "Timer for automatic refresh").

initialise(FB, Root:[directory]) :->
    "Create from initial dierctory"::
    send_super(FB, initialise),
    get(FB, make_root_node, Root, RootNode),
    send(FB, root, RootNode),
    send(FB, expand_root),
    (   get(FB, auto_refresh, Time),
        Time \== @nil
    ->  send(FB, auto_refresh, Time)
    ;   true
    ).

unlink(FB) :->
    send(FB, kill_timer),
    send_super(FB, unlink).

expand_node(FB, Id:any) :->
    "Expand a directory"::
    get(FB, node, Id, Node),
    send(Node, expand).

up(FB) :->
    "Provide the parent directory"::
    get(FB, root, RootNode),
    get(RootNode, identifier, RootDir),
    send(RootDir, instance_of, directory),
    (   get(RootDir, parent, Parent)
    ->  get(FB, make_dir_node, Parent, R),
        send(R, show, path),
        send(FB, root, R, @on),
        send(RootNode, show, name),
        send(R, update)
    ;   get(FB, root_directories, Roots),  % above a Windows drive
        get(Roots, size, Size),
        Size > 1
    ->  get(FB, make_roots_node, R),
        send(FB, root, R, @on),
        send(R, update)
    ).

:- pce_group(virtual).

make_root_node(_FB, Root:[directory], Node:toc_folder) :<-
    "Virtual: create the root node from the initial directory"::
    (   Root == @default
    ->  absolute_file_name('.', Path)
    ;   get(Root, path, Path0),
        absolute_file_name(Path0, Path)
    ),
    new(Node, toc_directory(directory(Path), path)).

make_roots_node(_FB, Node:toc_folder) :<-
    "Virtual: create a virtual root above the file system roots"::
    new(Node, toc_roots).

make_dir_node(_FB, Dir:directory, Node:toc_node) :<-
    "Virtual: create a node for a directory"::
    new(Node, toc_directory(Dir)).

root_directories(_FB, Roots:chain) :<-
    "Virtual: chain of file system roots (Windows drives)"::
    get(directory('.'), roots, Roots).

make_file_node(_FB, File:file, Node:toc_node) :<-
    "Virtual: create a node for a file"::
    get(File, base_name, Name),
    new(Node, toc_file(Name, File)),
    send(Node, name, Name).

:- pce_group(expand).

existing_dir_node(FB, Dir:directory, Node:toc_node) :<-
    "Node for Dir if it is already in the tree"::
    get(FB?tree, nodes, NodeTable),
    get(NodeTable, find_key,
        and(message(@arg1, instance_of, directory),
            message(@arg1, same, Dir)),
        NodeDir),
    get(NodeTable, member, NodeDir, Node).

sub_dir_node(_FB, Node:toc_node, Dir:directory, SubNode:toc_node) :<-
    "Son of Node representing Dir"::
    get(Node?sons, find,
        and(message(@arg1?identifier, instance_of, directory),
            message(@arg1?identifier, same, Dir)),
        SubNode).

dir_node(FB, Dir:directory, Create:[bool], Node:toc_node) :<-
    "Get node for directory, possibly add it to tree"::
    (   get(FB, existing_dir_node, Dir, Node)
    ->  true
    ;   Create == @on
    ->  get(Dir, parent, Parent),
        get(FB, dir_node, Parent, Create, ParentNode),
        send(ParentNode, collapsed, @off),
        get(FB, sub_dir_node, ParentNode, Dir, Node)
    ).


:- pce_group(refresh).

refresh(FB) :->
    "->refresh the root"::
    get(FB, root, RootNode),
    send(RootNode, refresh).

auto_refresh(FB, Interval:int*) :->
    "Associate an auto-refresh timer"::
    send(FB, kill_timer),
    (   Interval == @nil
    ->  true
    ;   send(FB, slot, refresh_timer,
             new(T, timer(Interval, message(FB, refresh)))),
        send(T, status, repeat)
    ).

kill_timer(FB) :->
    "Kill timer if we have one"::
    (   get(FB, slot, refresh_timer, Timer),
        Timer \== @nil
    ->  send(Timer, status, idle),
        free(Timer),
        send(FB, slot, refresh_timer, @nil)
    ;   true
    ).

:- pce_end_class(toc_filesystem).
