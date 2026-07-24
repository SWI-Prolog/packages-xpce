/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce/
    Copyright (c)  2001-2026, University of Amsterdam
                              VU University Amsterdam
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

:- module(prolog_navigator, []).
:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- use_module(library(toc_filesystem)).
:- use_module(library(pce_report)).
:- use_module(library(toolbar)).
% used in load hook: cannot be autoloaded.
:- use_module(library(trace/util),[canonical_source_file/2]).

:- autoload(browse_xref,
	    [ x_browse_info/2,
	      x_browse_analyse/1,
	      system_predicate/1,
	      global_predicate/1
	    ]).
:- use_module(library(debug),[debug/3]).
:- autoload(library(prolog_code), [head_name_arity/3]).
:- autoload(library(prolog_debug), [(nospy)/1, (spy)/1]).
:- autoload(library(edit),[edit/1]).
:- autoload(library(help),[help/1]).
:- autoload(library(lists),[member/2,subtract/3,append/3,last/2]).
:- autoload(library(pce_debug),
	    [nospypce/1,spypce/1,notracepce/1,tracepce/1]).
:- autoload(library(pce_image),[pce_image_directory/1]).
:- autoload(library(pce_manual),[manpce/1,manpce/0]).
:- autoload(library(pce_util),
	    [send_list/3,default/3,get_chain/3,chain_list/2,pce_text_to_regex/2]).
:- autoload(library(prolog_source),
	    [ prolog_open_source/2,
	      prolog_read_source_term/4,
	      prolog_close_source/1,
	      file_alias_path/2
	    ]).
:- autoload(library(prolog_trace),[trace/2,trace/1]).
:- autoload(library(swi_ide),[prolog_ide/1]).
:- autoload(library(apply), [convlist/3, include/3]).
:- if(exists_source(library(pldoc/man_index))).
:- autoload(library(pldoc/man_index),[man_object_property/2]).
:- endif.

:- pce_image_directory(library('trace/icons')).

:- dynamic
    prolog_overview_window/1.

resource(edit,        image, image('tool/edit.svg')).
resource(up,          image, image('tool/up.svg')).
resource(refresh,     image, image('tool/refresh.svg')).
resource(dbgsettings, image, image('dbgsettings.svg')).

:- pce_begin_class(prolog_navigator, persistent_frame,
                   "Prolog source navigator").

initialise(SB, Root:[directory]) :->
    send_super(SB, initialise, 'Prolog Navigator'),
    send(SB, append, new(D, dialog)),
    send(new(FD, sb_filter_dialog), below, D),
    send(new(W, prolog_source_structure(Root)), below, FD),
    send(D, append, new(tool_bar(W))),
    send(D, gap, size(0, 2)),
    send(D, pen, 0),
    send(SB, fill_tool_bar),
    send(new(report_dialog), below, W),
    send(FD, update_content).

tool_bar(SB, TB:tool_bar) :<-
    "Get the toolbar"::
    get(SB, member, dialog, D),
    get(D, member, tool_bar, TB).

fill_tool_bar(SB) :->
    "Fill the toolbar"::
    get(SB, tool_bar, TB),
    send_list(TB, append,
              [ tool_button(up,
                            resource(up),
                            'Up one level'),
                tool_button(refresh,
                            resource(refresh),
                            'Update view'),
                gap,
                tool_button(debug_settings,
                            resource(dbgsettings),
                            'Edit breakpoints'),
                tool_button(edit,
                            resource(edit),
                            'Open file in editor')
              ]).

goto(SB, File:file, Line:int) :->
    "Expand and highlight tree for given location"::
    get(SB, member, prolog_source_structure, FB),
    send(FB, goto, File, Line).

directory(SB, Dir:directory) :->
    "Make directory visible"::
    get(SB, member, prolog_source_structure, FB),
    send(FB, directory, Dir).

:- pce_end_class(prolog_navigator).


                 /*******************************
                 *            FILTER            *
                 *******************************/

:- pce_begin_class(sb_filter_dialog, dialog,
                   "Select what the tree shows").

class_variable(border, size, size(0,0)).

initialise(D) :->
    send_super(D, initialise),
    send(D, name, filter_dialog),
    send(D, gap, size(5, 2)),
    send(D, pen, 0),
    send(D, append, new(M, menu(content, cycle,
                                message(D, content, @arg1)))),
    send(M, label, 'Show:'),
    forall(content_mode(Mode, Label, _),
           send(M, append, menu_item(Mode, @default, Label))),
    send(D, append, new(sb_file_filter_item(filter)), right).

resize(D) :->
    send(D, layout, D?visible?size).

tree(D, Tree:prolog_source_structure) :<-
    "The tree we control"::
    get(D?frame, member, prolog_source_structure, Tree).

content(D, Content:{loaded,prolog,all}) :->
    "Set the content mode of the whole tree"::
    get(D, tree, Tree),
    send(Tree, content, Content).

update_content(D) :->
    "Show the content mode of the tree"::
    get(D, tree, Tree),
    get(Tree, content, Content),
    get(D, member, content, Menu),
    send(Menu, selection, Content).

:- pce_end_class(sb_filter_dialog).


:- pce_begin_class(sb_file_filter_item, text_item,
                   "Filter files as you type").

typed(FFI, Id:event_id) :->
    "Activate the filter"::
    send_super(FFI, typed, Id),
    get(FFI, displayed_value, Current),
    get(FFI?device, tree, Tree),
    (   send(Current, equal, '')
    ->  send(Tree, file_filter, @nil)
    ;   pce_text_to_regex(Current, Filter)
    ->  send(Tree, file_filter, Filter)
    ;   send(FFI, report, status, 'Incomplete expression')
    ).

:- pce_end_class(sb_file_filter_item).


:- pce_begin_class(prolog_source_structure, toc_filesystem,
                   "Browser for (prolog) source-files").

class_variable(background,   colour, white).
class_variable(colour,       colour, black).
class_variable(auto_refresh, int*, @nil).
class_variable(size,    size,   size(200, 500),
               "Intial window size").

variable(file_pattern,  regex, get, "Pattern of showed files").
variable(content, {loaded,prolog,all} := prolog, get,
         "Content mode for the directory nodes").
variable(file_filter, regex*, get,
         "If not @nil, only show files matching this").
variable(expanded_dirs, chain, get,
         "Paths of the directories the user expanded").
variable(extra_seeds, chain, get,
         "Additional directories to browse (see <-scan_seeds)").
variable(seed_cache, chain*, get,
         "Cached <-scan_seeds; @nil if it must be recomputed").

initialise(FB, Root:[directory]) :->
    source_pattern(Regex),
    send(FB, slot, file_pattern, Regex),
    send(FB, slot, expanded_dirs, new(chain)),
    send(FB, slot, extra_seeds, new(chain)),
    send_super(FB, initialise, Root),
    send(FB?frame, label, 'SWI-Prolog Navigator'),
    asserta(prolog_overview_window(FB)),
    (   Root == @default
    ->  true
    ;   send(FB, directory, Root)
    ).

source_pattern(Pat) :-
    findall(E, (user:prolog_file_type(E, prolog),
                \+ user:prolog_file_type(E, qlf)), Exts),
    (   Exts = [Ext]
    ->  format(atom(Pat), '.*\\.~w$', [Ext])
    ;   atomic_list_concat(Exts, '|', P1),
        format(atom(Pat), '.*\\.(~w)$', [P1])
    ).

unlink(FB) :->
    retractall(prolog_overview_window(FB)),
    send_super(FB, unlink).

make_root_node(FB, Root:[directory], Node:toc_folder) :<-
    "Root at the common directory of the loaded source files"::
    (   Root == @default
    ->  (   loaded_root_directory(Path)
        ->  send(FB, slot, content, loaded),
            new(Node, sb_prolog_directory(directory(Path), path, loaded))
        ;   loaded_file_directory(_)      % spread over multiple drives
        ->  send(FB, slot, content, loaded),
            get(FB, make_roots_node, Node)
        ;   absolute_file_name('.', Path),
            new(Node, sb_prolog_directory(directory(Path), path, prolog))
        )
    ;   get(Root, path, Path0),
        absolute_file_name(Path0, Path),
        new(Node, sb_prolog_directory(directory(Path), path, prolog))
    ).

make_roots_node(_FB, Node:sb_computer) :<-
    "Return a virtual root above the file system roots"::
    new(Node, sb_computer).

%!  loaded_root_directory(-Dir) is semidet.
%
%   Common root directory of all directories holding loaded files.  Fails
%   if they are spread over multiple roots (Windows drives).

loaded_root_directory(Dir) :-
    findall(D, loaded_file_directory(D), Ds0),
    sort(Ds0, [First|Rest]),
    last([First|Rest], Last),
    dir_segments(First, S1),
    dir_segments(Last, S2),
    common_prefix(S1, S2, Common),
    Common \== [],
    segments_dir(Common, Dir).

loaded_file_directory(Dir) :-
    source_file(File),
    file_directory_name(File, Dir).

dir_segments(Dir, Segments) :-
    atomic_list_concat(Segments, /, Dir).

segments_dir([''], /) :-
    !.
segments_dir([Drive], Dir) :-           % Windows drive
    sub_atom(Drive, _, _, 0, :),
    !,
    atom_concat(Drive, /, Dir).
segments_dir(Segments, Dir) :-
    atomic_list_concat(Segments, /, Dir).

common_prefix([H|T1], [H|T2], [H|T]) :-
    !,
    common_prefix(T1, T2, T).
common_prefix(_, _, []).

make_dir_node(FB, Dir:directory, Node:sb_prolog_directory) :<-
    "Return a directory node showing the current <-content"::
    get(FB, content, Content),
    new(Node, sb_prolog_directory(Dir, @default, Content)).

make_file_node(FB, File:file, Node:toc_node) :<-
    "Return a node for a source file or a plain file"::
    (   get(FB, file_pattern, Regex),
        get(File, base_name, Base),
        \+ send(Regex, search, Base)
    ->  new(Node, sb_file(File))       % not Prolog: cannot be expanded
    ;   new(Node, sb_prolog_file(File))
    ).

:- pce_group(navigate).

file_node(FB, File:name, Create:[bool], Node:toc_node) :<-
    "Get node for file, possibly add it to the tree"::
    canonical_source_file(File, Path),
    (   get(FB, node, Path, Node)
    ->  true
    ;   Create == @on
    ->  file_directory_name(Path, Dir),
        get(FB, dir_node, Dir, @on, DirNode),
        send(DirNode, collapsed, @off),
        (   get(FB, node, Path, Node)
        ->  true
        ;   send(DirNode, show_all_files),   % not a loaded file
            get(FB, node, Path, Node)
        )
    ).

dir_node(FB, Dir:directory, Create:[bool], Node:toc_node) :<-
    "Get node for directory, possibly add it to the tree"::
    (   get(FB, existing_dir_node, Dir, Node)
    ->  true
    ;   Create == @on
    ->  send(FB, ensure_below_root, Dir),
        (   get(Dir, parent, Parent)
        ->  get(FB, dir_node, Parent, @on, ParentNode)
        ;   get(FB, root, ParentNode),  % Dir is a file system root
            send(ParentNode, instance_of, toc_roots)
        ),
        send(ParentNode, show_all_files),
        send(ParentNode, collapsed, @off),
        get(FB, sub_dir_node, ParentNode, Dir, Node)
    ).

ensure_below_root(FB, Dir:directory) :->
    "Move the root up until Dir is inside the tree"::
    (   send(FB, below_root, Dir)
    ->  true
    ;   send(FB, up)                    % fails at the filesystem root
    ->  send(FB, ensure_below_root, Dir)
    ;   true
    ).

below_root(FB, Dir:directory) :->
    "True if Dir is the root directory or below it"::
    get(FB, root, Root),
    get(Root, identifier, RootDir),
    (   send(RootDir, instance_of, directory)
    ->  (   send(RootDir, same, Dir)
        ->  true
        ;   get(Dir, parent, Parent),
            send(FB, below_root, Parent)
        )
    ;   true                            % virtual root holds all roots
    ).

directory(FB, Dir:directory) :->
    "Make directory visible"::
    get(Dir, path, Path),
    send(FB, seed, Path),
    get(FB, dir_node, Dir, @on, Node),
    send(Node, show_all_files),
    send(Node, collapsed, @off).

                 /*******************************
                 *             SCOPE            *
                 *******************************/

/* Showing all files below the root is useless: the root is the common
   directory of the loaded files, which is easily "/".  Instead we only
   browse below a set of seed directories: the ones that hold loaded
   files, the library search path, the working directory and the ones
   the user visited.  Ancestors of a seed only show the children that
   lead to a seed; at or below a seed everything is shown.
*/

seed(FB, Dir:name) :->
    "Add Dir to the directories we browse"::
    absolute_file_name(Dir, Path),
    get(FB, extra_seeds, Seeds),
    (   send(Seeds, member, Path)
    ->  true
    ;   send(Seeds, append, Path),
        send(FB, invalidate_seeds),
        get(FB?tree, root, Root),
        refilter(Root)                  % the new seed widens the scope
    ).

invalidate_seeds(FB) :->
    "Force <-scan_seeds to be recomputed"::
    send(FB, slot, seed_cache, @nil).

refresh(FB) :->
    "Recompute the seeds and update the tree"::
    send(FB, invalidate_seeds),
    send_super(FB, refresh).

scan_seeds(FB, Seeds:chain) :<-
    "Directories below which we show all files"::
    (   get(FB, seed_cache, Seeds),
        Seeds \== @nil
    ->  true
    ;   get(FB, extra_seeds, Extra),
        chain_list(Extra, Visited),
        findall(D, seed_directory(D), Found),
        append(Visited, Found, All0),
        sort(All0, All),
        chain_list(Seeds, All),
        send(FB, slot, seed_cache, Seeds)
    ).

seed_directory(Dir) :-                  % holds a loaded file
    source_file(File),
    file_directory_name(File, Dir).
seed_directory(Dir) :-                  % on the library search path
    file_alias_path(library, Dir0),
    exists_directory(Dir0),
    absolute_file_name(Dir0, Dir).
seed_directory(Dir) :-                  % the project we started in
    working_directory(Dir, Dir).

%!  in_scan_scope(+Dir, +Seeds) is semidet.
%
%   True when Dir is a seed or on the way   to  one, or below a seed.  A
%   directory whose name starts with a dot   must  lead to a seed: we do
%   want ~/.local/share/swi-prolog/pack, but not the .git of a project.

in_scan_scope(Dir, Seeds) :-
    (   member(Seed, Seeds),
        below_or_same(Seed, Dir)        % Dir is a seed or above one
    ->  true
    ;   \+ hidden_dir(Dir),
        member(Seed, Seeds),
        below_or_same(Dir, Seed)        % Dir is below a seed
    ).

hidden_dir(Dir) :-
    file_base_name(Dir, Base),
    sub_atom(Base, 0, _, _, '.').

below_or_same(Path, Path) :-
    !.
below_or_same(Path, Dir) :-
    atom_concat(Dir, /, Prefix),
    sub_atom(Path, 0, _, _, Prefix).

content(FB, Content:{loaded,prolog,all}) :->
    "Set the content mode of all directory nodes"::
    send(FB, remember_expanded),
    send(FB, slot, content, Content),
    get(FB?tree, root, Root),
    set_content(Root, Content),
    get(FB, expanded_dirs, Chain),
    chain_list(Chain, Paths),
    restore_expanded(Root, Paths).

remember_expanded(FB) :->
    "Update <-expanded_dirs from the current state of the tree"::
    get(FB?tree, root, Root),
    findall(Path, dir_below(Root, Path), Present),
    findall(Path, expanded_dir(Root, Path), Expanded),
    get(FB, expanded_dirs, Chain0),
    chain_list(Chain0, Remembered),
    subtract(Remembered, Present, Kept),  % keep what we cannot see
    append(Kept, Expanded, Paths0),
    sort(Paths0, Paths),
    chain_list(Chain, Paths),
    send(FB, slot, expanded_dirs, Chain).

dir_below(Node, Path) :-
    send(Node, instance_of, toc_directory),
    dir_node_path(Node, Path).
dir_below(Node, Path) :-
    node_sons(Node, Sons),
    member(Son, Sons),
    dir_below(Son, Path).

expanded_dir(Node, Path) :-
    send(Node, instance_of, toc_directory),
    get(Node, collapsed, @off),
    dir_node_path(Node, Path).
expanded_dir(Node, Path) :-
    node_sons(Node, Sons),
    member(Son, Sons),
    expanded_dir(Son, Path).

%!  restore_expanded(+Node, +Paths) is det.
%
%   Expand the directories in Paths and collapse the others.  Switching
%   the content mode deletes and recreates nodes, and creating a node for
%   a directory holding loaded files expands it; without this the tree
%   reorganises itself under the user on every switch.

restore_expanded(Node, Paths) :-
    (   send(Node, instance_of, toc_directory),
        get(Node, collapsed, Collapsed),
        Collapsed \== @nil              % @nil: not expandable
    ->  dir_node_path(Node, Path),
        (   memberchk(Path, Paths)
        ->  Want = @off
        ;   Want = @on
        ),
        (   Collapsed == Want
        ->  true
        ;   send(Node, collapsed, Want)
        )
    ;   true
    ),
    (   object(Node),                   % ->collapsed may have deleted us
        get(Node, collapsed, @off),
        node_sons(Node, Sons)
    ->  forall(member(Son, Sons),
               (   object(Son)
               ->  restore_expanded(Son, Paths)
               ;   true
               ))
    ;   true
    ).

dir_node_path(Node, Path) :-
    get(Node, identifier, Id),
    (   send(Id, instance_of, directory)
    ->  get(Id, path, Path)
    ;   Path = Id
    ).

node_sons(Node, Sons) :-
    get(Node, sons, Chain),
    Chain \== @nil,
    chain_list(Chain, Sons).

file_filter(FB, Regex:regex*) :->
    "Only show the files whose base name matches Regex"::
    get(FB, file_filter, Old),
    (   Old == @nil, Regex \== @nil     % entering the filter: snapshot the
    ->  send(FB, remember_expanded)     % expansion to restore when cleared
    ;   true
    ),
    send(FB, slot, file_filter, Regex),
    get(FB?tree, root, Root),
    refilter(Root),
    (   Regex == @nil
    ->  get(FB, expanded_dirs, Chain),  % leaving the filter: restore the tree
        chain_list(Chain, Paths),
        restore_expanded(Root, Paths)
    ;   reveal_matches(Root)            % expand the paths down to the matches
    ),
    send(FB, scroll_to, point(0,0)),    % the tree shrunk: show the top
    (   Regex == @nil
    ->  send(FB, report, status, '')
    ;   get(FB, displayed_files, Count),
        send(FB, report, status,
             'Filter on file name: %d files shown', Count)
    ).

%!  reveal_matches(+Node) is det.
%
%   Expand every directory below Node that survived filtering.  With a
%   filter active each visible directory leads to a match, so expanding
%   them makes the matching files visible instead of leaving them buried
%   under collapsed nodes.  This mirrors loaded mode (see
%   ensure_loaded_dir/2).

reveal_matches(Node) :-
    (   send(Node, instance_of, toc_directory),
        get(Node, collapsed, @on)       % @nil: not expandable; @off: already
    ->  send(Node, collapsed, @off)     % triggers ->update, prunes the sons
    ;   true
    ),
    (   object(Node),
        node_sons(Node, Sons)
    ->  forall(member(Son, Sons),
               (   object(Son)
               ->  reveal_matches(Son)
               ;   true
               ))
    ;   true
    ).

displayed_files(FB, Count:int) :<-
    "Number of file nodes currently displayed"::
    get(FB?tree, root, Root),
    new(Cnt, number(0)),
    send(Root, for_all,
         if(not(or(message(@arg1, instance_of, toc_directory),
                   message(@arg1, instance_of, toc_roots))),
            message(Cnt, plus, 1))),
    get(Cnt, value, Count).

%!  refilter(+Node) is det.
%
%   Re-run ->update on the expanded directory nodes below Node, so they
%   pick up the new <-file_filter.  Parents first: updating a parent may
%   delete or create sons.

refilter(Node) :-
    (   send(Node, instance_of, toc_directory),
        get(Node, collapsed, @off)
    ->  send(Node, update)
    ;   true
    ),
    (   object(Node),
        node_sons(Node, Sons)
    ->  forall(member(Son, Sons),
               (   object(Son)
               ->  refilter(Son)
               ;   true
               ))
    ;   true
    ).

%!  set_content(+Node, +Content) is det.
%
%   Set the <-content of Node and all directory nodes below it.  Parents
%   are done first: ->content runs ->update, which may delete or create
%   sons, so the sons are collected only after the parent settled.

set_content(Node, Content) :-
    (   send(Node, instance_of, sb_prolog_directory)
    ->  send(Node, content, Content)
    ;   true
    ),
    (   object(Node),                   % ->content may have deleted us
        node_sons(Node, Sons)
    ->  forall(member(Son, Sons),
               (   object(Son)
               ->  set_content(Son, Content)
               ;   true
               ))
    ;   true
    ).

loaded_file(FB, File:name) :->
    "Add a file that was just loaded to the tree"::
    send(FB, invalidate_seeds),         % its directory is a new seed
    (   file_directory_name(File, Dir),
        loaded_dir_node(FB, Dir, Node)
    ->  send(Node, update)
    ;   true
    ).

%!  loaded_dir_node(+FB, +Dir, -Node) is semidet.
%
%   Node is the nearest node at or above  Dir that shows only the loaded
%   files and is expanded.  ->update on  it   re-reads  the set of loaded
%   files, creating the nodes below it as needed.  Fails if Dir is not in
%   the tree or the enclosing directory is not expanded, in which case
%   the file appears as soon as the user expands it.

loaded_dir_node(FB, Dir, Node) :-
    (   get(FB, existing_dir_node, Dir, Node0)
    ->  get(Node0, content, loaded),
        get(Node0, collapsed, @off),
        Node = Node0
    ;   file_directory_name(Dir, Parent),
        Parent \== Dir,
        loaded_dir_node(FB, Parent, Node)
    ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->goto: File, Line opens  the  tree   such  that  the indicated position
becomes visible and selects the entity   holding the specified location.
First it looks for the file, then it  assumes the sons are in file-order
and looks for the first son after the requested line. After finding this
it iterates on this.

This method is intended to synchronise with an editor.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

goto(FB, File:'file|node', Line:int) :->
    "Show indicated position"::
    (   send(File, instance_of, file)
    ->  get(FB, file_node, File?name, @on, Node)
    ;   Node = File
    ),
    (   (   get(Node, collapsed, @nil)
        ;   send(Node, instance_of, sb_predicate)
        )
    ->  send(FB?tree, selection, Node),
        send(FB, normalise, Node)
    ;   send(Node, collapsed, @off),
        new(N, var(value := @nil)),
        (   get(Node?sons, find,
                and(message(@arg1, has_get_method, line),
                    if(and(@arg1?line \== @nil,
                           @arg1?line > Line),
                       new(and),
                       and(assign(N, @arg1, global),
                           new(or)))),
                _)
        ->  get(N, '_value', N2),
            (   N2 == @nil
            ->  send(FB?tree, selection, Node),
                send(FB, normalise, Node)
            ;   send(FB, goto, N2, Line)
            )
        ;   get(Node?sons, tail, Last)
        ->  send(FB, goto, Last, Line)
        ;   send(FB?tree, selection, Node),
            send(FB, normalise, Node)
        )
    ).

:- pce_group(popup).

popup(FB, Id:any, Popup:popup) :<-
    "Return popup from current node"::
    get(FB, node, Id, Node),
    (   send(Node, has_get_method, popup),
        get(Node, popup, Popup)
    ->  true
    ;   send(Node, instance_of, sb_prolog_directory)
    ->  new(Popup, popup(options)),
        send(Popup, show_current, @on),
        get(Node, content, Current),
        forall(content_mode(Mode, Label, EndGroup),
               ( send(Popup, append,
                      new(MI, menu_item(Mode, message(Node, content, Mode),
                                        Label, EndGroup))),
                 (   Mode == Current
                 ->  send(MI, selected, @on)
                 ;   true
                 )
               )),
        send(Popup, append,
             menu_item(expand_all,
                       message(Node, expand_all)))
    ;   send(Node, instance_of, toc_directory)
    ->  new(Popup, popup(options)),
        send(Popup, append,
             menu_item(expand_all,
                       message(Node, expand_all)))
    ).

%!  content_mode(?Mode, ?Label, ?EndGroup) is nondet.
%
%   Modes for the <-content of a directory node, their menu labels and
%   whether they close the group of modes in a menu.

content_mode(loaded, 'Loaded files', @default).
content_mode(prolog, 'Prolog files', @default).
content_mode(all,    'All files',    @on).


:- pce_group(edit).

edit(FB) :->
    "Edit selected file"::
    (   get(FB, selection, Sel),
        get(Sel, head, FileNode),
        send(FileNode, has_send_method, edit),
        send(FileNode, edit)
    ;   send(FB, report, warning, 'No selected file'),
        fail
    ).

debug_settings(_FB) :->
    "Open debug-status editor"::
    prolog_ide(open_debug_status).

:- pce_group(event).

event(FB, Ev:event) :->
    "Deal with identifying nodes"::
    (   send_super(FB, event, Ev)
    ->  true
    ;   send(Ev, is_a, loc_move)
    ->  (   get(FB, hypered, current, Node)
        ->  (   send(Ev, inside, Node)
            ->  true
            ;   send(FB, delete_hypers, current),
                send(FB, report, status, '')
            )
        ;   get(FB, find, Ev, @arg1?node, Img),
            new(_, hyper(FB, Img, current, toc)),
            get(Img, node, Node),
            (   send(Node, has_send_method, identify)
            ->  send(Node, identify)
            ;   true
            )
        )
    ).

:- pce_end_class(prolog_source_structure).


:- pce_begin_class(sb_computer, toc_roots,
                   "Root above the file system roots").

update(C) :->
    "Show the roots, expanding those that hold loaded files"::
    send_super(C, update),
    send(C?sons, for_all, message(@arg1, show_loaded)).

:- pce_end_class(sb_computer).


:- pce_begin_class(sb_prolog_directory, toc_directory,
                   "Directory holding Prolog source files").

variable(content, {loaded,prolog,all} := prolog, get,
         "Show the loaded files, the Prolog files or all files").

initialise(D, Dir:directory, Show:[name], Content:[{loaded,prolog,all}]) :->
    send_super(D, initialise, Dir, Show),
    (   Content == @default
    ->  true
    ;   send(D, slot, content, Content)
    ).

file_pattern(D, Regex:[regex]) :<-
    "Show all files if <-content is `all'"::
    (   get(D, content, all)
    ->  Regex = @default
    ;   get_super(D, file_pattern, Regex)
    ).

filter_files(D, Names:chain) :->
    "Remove the files that do not match the filter"::
    (   get(D, file_filter, Filter),
        Filter \== @nil
    ->  chain_list(Names, List),
        include(matching_file(Filter), List, Matching),
        send(Names, clear),
        forall(member(Name, Matching),
               send(Names, append, Name))
    ;   true
    ).

scan_hidden(_D, Hidden:[bool]) :<-
    "Scan dot directories: the seeds include e.g. ~/.local/.../pack"::
    Hidden = @on.

filter_dirs(D, Names:chain) :->
    "Remove the subdirectories we do not browse"::
    (   get(D, window, FB),
        send(FB, has_get_method, scan_seeds)
    ->  get(D, identifier, Dir),
        get(Dir, path, Path),
        get(FB, scan_seeds, Chain),
        chain_list(Chain, Seeds),
        get(D, file_filter, Filter),
        get(D, file_pattern, Pattern),
        chain_list(Names, List),
        include(browsable_subdir(Path, Seeds, Pattern, Filter), List, InScope),
        send(Names, clear),
        forall(member(Name, InScope),
               send(Names, append, Name))
    ;   true
    ).

%!  browsable_subdir(+Parent, +Seeds, +Pattern, +Filter, +Name) is semidet.
%
%   True when subdirectory Name of Parent is in scan scope and, if a
%   name Filter is active, holds at least one matching file somewhere
%   below it.  This prunes directories with nothing to show, matching
%   the behaviour of loaded mode (see loaded_below/4).

browsable_subdir(Parent, Seeds, Pattern, Filter, Name) :-
    in_scope_below(Parent, Seeds, Name),
    (   Filter == @nil
    ->  true
    ;   dir_prefix(Parent, Prefix),
        atom_concat(Prefix, Name, Dir),
        subtree_has_match(Dir, Pattern, Filter, Seeds)
    ).

in_scope_below(Parent, Seeds, Name) :-
    dir_prefix(Parent, Prefix),         % Parent may be "/"
    atom_concat(Prefix, Name, Dir),
    in_scan_scope(Dir, Seeds).

%!  subtree_has_match(+Dir, +Pattern, +Filter, +Seeds) is semidet.
%
%   True as soon as Dir holds, at some level in scope, a non-hidden
%   file whose base name matches Filter and, unless Pattern is
%   @default, the mode Pattern.  Stops at the first match.

subtree_has_match(Dir, Pattern, Filter, Seeds) :-
    dir_prefix(Dir, Prefix),
    directory_files(Dir, Entries),
    member(Entry, Entries),
    \+ dot_entry(Entry),
    atom_concat(Prefix, Entry, Path),
    (   exists_directory(Path)
    ->  \+ hidden_dir(Path),
        in_scan_scope(Path, Seeds),
        subtree_has_match(Path, Pattern, Filter, Seeds)
    ;   matching_file(Filter, Entry),
        pattern_matches(Pattern, Entry)
    ),
    !.

dot_entry('.').
dot_entry('..').

pattern_matches(@default, _) :- !.
pattern_matches(Pattern, Name) :-
    send(Pattern, search, Name).

file_filter(D, Filter:regex*) :<-
    "Filter on file names from the window"::
    (   get(D, window, FB),
        send(FB, has_get_method, file_filter)
    ->  get(FB, file_filter, Filter)
    ;   Filter = @nil
    ).

%!  matching_file(+Filter, +Name) is semidet.
%
%   True when the base name Name passes Filter.

matching_file(Filter, Name) :-
    send(Filter, search, Name).

content(D, Content:{loaded,prolog,all}) :->
    "Switch between showing the loaded, the Prolog or all files"::
    (   get(D, content, Content)
    ->  true
    ;   send(D, slot, content, Content),
        (   get(D, collapsed, @off)
        ->  send(D, update)
        ;   true
        )
    ).

show_all_files(D) :->
    "Show all Prolog files in this directory"::
    send(D, content, prolog).

show_loaded(D) :->
    "Show the loaded files if this directory holds any"::
    get(D, identifier, Dir),
    get(Dir, path, Path),
    get(D, file_filter, Filter),
    (   loaded_below(Path, Filter, [], [])
    ->  true
    ;   send(D, content, loaded),
        (   object(D)               % ->content may have deleted us
        ->  send(D, collapsed, @off)
        ;   true
        )
    ).

expand_all(D) :->
    "Expand this directory recursively"::
    send(D, content, prolog),
    (   object(D)                   % ->update may have deleted us
    ->  send_super(D, expand_all)
    ;   true
    ).

refresh(D) :->
    "Update for possible changes"::
    (   get(D, content, loaded),
        get(D, collapsed, @off)
    ->  send(D, update),
        (   object(D)               % ->update may have deleted us
        ->  send(D?sons, for_all,
                 if(message(@arg1, has_send_method, refresh),
                    message(@arg1, refresh)))
        ;   true
        )
    ;   send_super(D, refresh)
    ).

update(D) :->
    "Really update"::
    (   get(D, content, loaded)
    ->  send(D, update_loaded)
    ;   send_super(D, update)
    ).

hide_sons(D) :->
    "Return to the default content when expanded again"::
    (   get(D, window, FB),
        send(FB, has_get_method, content)
    ->  get(FB, content, Content)
    ;   Content = prolog
    ),
    send(D, slot, content, Content),
    send_super(D, hide_sons).

update_loaded(D) :->
    "Show the loaded files and the directories leading to them"::
    get(D, identifier, Dir),
    (   send(Dir, exists)
    ->  get(Dir, path, Path),
        get(D, file_filter, Filter),
        loaded_below(Path, Filter, SubDirs, Files),
        chain_list(DirNames, SubDirs),
        chain_list(FileNames, Files),
        send(D?sons, for_all,             % delete the no longer loaded
             if(and(not(message(DirNames, member, @arg1?name)),
                    not(message(FileNames, member, @arg1?name))),
                message(@arg1, delete_tree))),
        send(DirNames, for_all, message(D, ensure_loaded_dir, @arg1)),
        send(FileNames, for_all, message(D, ensure_file, @arg1)),
        send(D, sort_sons)
    ;   send(D, delete_tree)
    ).

ensure_loaded_dir(D, Name:name) :->
    "Ensure an expanded subdirectory holding loaded files"::
    (   get(D?sons, find,
            and(message(@arg1, instance_of, toc_directory),
                @arg1?name == Name),
            _)
    ->  true
    ;   send(D, ensure_dir, Name),
        get(D?sons, find, @arg1?name == Name, Node),
        send(Node, slot, content, loaded),
        send(Node, collapsed, @off)
    ).

%!  loaded_below(+Dir, +Filter, -SubDirs, -Files) is det.
%
%   SubDirs are the subdirectories of Dir that hold loaded files at some
%   level below them; Files are the files loaded from Dir itself.  Filter
%   is @nil or a regex the base name must match.  Because a subdirectory
%   is only reported if it holds a matching file, filtering also prunes
%   the directories that have nothing left to show.

loaded_below(Dir, Filter, SubDirs, Files) :-
    dir_prefix(Dir, Prefix),
    findall(D, loaded_entry(Prefix, Filter, dir(D)), SubDirs0),
    sort(SubDirs0, SubDirs),
    findall(F, loaded_entry(Prefix, Filter, file(F)), Files0),
    sort(Files0, Files).

loaded_entry(Prefix, Filter, Entry) :-
    source_file(File),
    atom_concat(Prefix, Rest, File),
    (   Filter == @nil
    ->  true
    ;   file_base_name(File, Base),      % the file decides, also for dirs
        matching_file(Filter, Base)
    ),
    (   sub_atom(Rest, Before, _, _, /)
    ->  sub_atom(Rest, 0, Before, _, Name),
        Entry = dir(Name)
    ;   Entry = file(Rest)
    ).

dir_prefix(Dir, Dir) :-                 % "/" or Windows "c:/"
    sub_atom(Dir, _, _, 0, /),
    !.
dir_prefix(Dir, Prefix) :-
    atom_concat(Dir, /, Prefix).

:- pce_end_class(sb_prolog_directory).


:- pce_begin_class(sb_prolog_file, toc_folder,
                   "Display a Prolog file").

initialise(TF, File:file) :->
    get(File, name, FileName),
    canonical_source_file(FileName, Path),
    file_image(Path, ImgFile),
    file_to_image(ImgFile, Img),
    file_base_name(FileName, Base),
    send_super(TF, initialise, Base, Path, Img),
    send(TF, name, Base).

update_image(_TF) :->
    true.

loaded(TF) :->
    get(TF, identifier, Path),
    source_file(Path).

included(TF) :->
    get(TF, identifier, Path),
    included_file(Path).

file_image(Path, 'plloadedfile.svg') :-
    source_file(Path),
    !.
file_image(Path, 'plincludedfile.svg') :-
    included_file(Path),
    !.
file_image(_, 'plfile.svg').

included_file(Path) :-
    source_file_property(Path, included_in(_,_)).


module(TF, Module:name) :<-
    "Return module defined in this file"::
    get(TF, identifier, Path),
    (   x_browse_info(Path, entity(module(Module), _Line))
    ->  true
    ;   catch(module_of_path(Path, Module), _, fail)
    ).

module_of_path(Path, Module) :-
    catch(setup_call_cleanup(
              prolog_open_source(Path, Stream),
              prolog_read_source_term(Stream, Term, _, []),
              prolog_close_source(Stream)), _,
          fail),
    Term = (:- module(Module, _Public)).

hidden_entity(module(_)).

file_expansion_entity(Path, entity(module(Module), Line)) :-
    once(x_browse_info(Path, entity(module(Module), Line))).
file_expansion_entity(Path, entity(dynamic, -)) :-
    once(file_expansion_entity(Path, entity(dynamic(_), _))).
file_expansion_entity(Path, Entity) :-
    x_browse_info(Path, Entity),
    arg(1, Entity, Term),
    \+ hidden_entity(Term).

expand(TF) :->
    get(TF, identifier, Path),
    get(TF, window, TocWindow),
    x_browse_analyse(Path),
    (   file_expansion_entity(Path, entity(Info, Line)),
        make_file_toc_entry(Info, Path, Entry),
        send(TocWindow, son, TF, Entry),
        send(Entry, file_id, Path),
        integer(Line),
        send(Entry, line, Line),
        fail
    ;   true
    ).

expand_all(_TF) :->
    true.

async_expand(_TF) :->
    true.

split_head(M:Head, Name, Arity, M) :-
    !,
    callable(Head),
    head_name_arity(Head, Name, Arity).
split_head(Head, Name, Arity, @nil) :-
    callable(Head),
    head_name_arity(Head, Name, Arity).

make_file_toc_entry(predicate(Head), Key, TE) :-
    split_head(Head, Name, Arity, Module),
    new(TE, sb_predicate(Key, Name, Arity, Module)).
make_file_toc_entry(grammar_rule(Head), Key, TE) :-
    split_head(Head, Name, Arity, Module),
    new(TE, sb_predicate(Key, Name, Arity, Module)).
make_file_toc_entry(Term, _Key, TE) :-
    make_file_toc_entry(Term, TE).

make_file_toc_entry(xpce_class(Class, _Super, Doc), TE) :-
    to_summary(Doc, PceDoc),
    new(TE, toc_xpce_class(Class, PceDoc)).
make_file_toc_entry(xpce_class_extension(Class), TE) :-
    file_to_image('classext.svg', Image),
    new(TE, toc_xpce_class(Class, @default, Image)).
make_file_toc_entry(module(Module), TE) :-
    file_to_image('module.svg', Image),
    new(TE, toc_module(Module, @default, Image)).
make_file_toc_entry(dynamic, TE) :-
    file_to_image('mini-run.svg', Image),
    new(TE, sb_predicate_list(dynamic, @default, Image)).

to_summary(Doc, String) :-
    catch(string_codes(String, Doc), _, fail),
    !.
to_summary(_, @default).

local_predicate_name(M:Head, Label) :-
    !,
    callable(Head),
    head_name_arity(Head, Name, Arity),
    atomic_list_concat([M, :, Name, /, Arity], Label).
local_predicate_name(Head, Label) :-
    head_name_arity(Head, Name, Arity),
    atomic_list_concat([Name, /, Arity], Label).

identify(TF) :->
    "Identify myself"::
    get(TF, identifier, Path),
    (   send(TF, loaded)
    ->  send(TF, report, status, 'Loaded file %s', Path)
    ;   send(TF, included)
    ->  send(TF, report, status, 'Included file %s', Path)
    ;   send(TF, report, status, 'File %s', Path)
    ).

:- pce_group(popup).

:- free(@sb_file_popup).
:- pce_global(@sb_file_popup, make_sb_file_popup).

make_sb_file_popup(P) :-
    new(P, popup(source_options)),

    send_list(P, append,
              [ menu_item(edit,
                          message(@arg1, edit)),
                menu_item(consult,
                          message(@arg1, consult))
              ]).

popup(_, Popup:popup) :<-
    Popup = @sb_file_popup.

edit(TF) :->
    get(TF, identifier, Path),
    edit(file(Path)).

consult(TF) :->
    "Load into Prolog"::
    get(TF, identifier, Path),
    ensure_loaded(user:Path).

:- pce_end_class(sb_prolog_file).

:- pce_begin_class(sb_file, toc_file,
                   "Display a file that is not Prolog source").

initialise(TF, File:file) :->
    get(File, name, Path),
    file_base_name(Path, Base),
    send_super(TF, initialise, Base, Path),
    send(TF, name, Base).

identify(TF) :->
    "Identify myself"::
    get(TF, identifier, Path),
    send(TF, report, status, 'File %s', Path).

:- free(@sb_other_file_popup).
:- pce_global(@sb_other_file_popup, make_sb_other_file_popup).

make_sb_other_file_popup(P) :-
    new(P, popup(source_options)),
    send(P, append,
         menu_item(edit,
                   message(@arg1, edit))).

popup(_, Popup:popup) :<-
    Popup = @sb_other_file_popup.

edit(TF) :->
    get(TF, identifier, Path),
    edit(file(Path)).

:- pce_end_class(sb_file).

:- pce_begin_class(toc_source_folder, toc_folder,
                   "Representation of a source entity").

variable(file_id,       name,   both, "File it was loaded from").
variable(line,          int*,   both, "Line it is associated with").

update_image(_) :->
    true.

open(TE) :->
    "Synonym for ->edit"::
    send(TE, edit).

edit(TE) :->
    "Open definition in editor"::
    get(TE, file_id, File),
    get(TE, line, Line),
    (   integer(Line)
    ->  edit(file(File, line(Line)))
    ;   send(TE, report, error, 'No source')
    ).

has_source(TE) :->
    "Has associated source"::
    get(TE, line, Line),
    integer(Line).

status(TE, Status:{open,close}) :<-
    get(TE, collapsed, Val),
    (   Val == @on
    ->  Status = close
    ;   Status = open
    ).

:- pce_group(popup).

:- free(@source_popup).
:- pce_global(@source_popup, make_source_popup).

make_source_popup(P) :-
    new(P, popup(source_options)),

    send_list(P, append,
              [ menu_item(edit,
                          message(@arg1, edit),
                          condition := message(@arg1, has_source))
              ]).

popup(_, Popup:popup) :<-
    Popup = @source_popup.

:- pce_end_class.

:- pce_begin_class(toc_xpce_entity, toc_file,
                   "Representation of an XPCE source entity").

variable(file_id,       name,   both, "File it was loaded from").
variable(line,          int*,   both, "Line it is associated with").


open(TE) :->
    send(TE, edit).

edit(TE) :->
    "Edit the source"::
    get(TE, file_id, File),
    get(TE, line, Line),
    (   integer(Line)
    ->  edit(file(File, line(Line)))
    ;   send(TE, report, error, 'No source')
    ).

has_source(TE) :->
    "Has associated source"::
    get(TE, line, Line),
    integer(Line).

loaded(TE) :->
    "Test if class is loaded"::
    get(TE, identifier, Id),
    atomic_list_concat([_Type, _Name, Class], $, Id),
    pce_prolog_class(Class).

behaviour(TE, Behaviour:behaviour) :<-
    "Get behaviour (if loaded)"::
    get(TE, identifier, Id),
    atomic_list_concat([Type, Name, Class], $, Id),
    get(@pce, convert, Class, class, ClassObj),
    (   Type == send
    ->  get(ClassObj, send_method, Name, Behaviour)
    ;   Type == get
    ->  get(ClassObj, get_method, Name, Behaviour)
    ;   Type == var
    ->  get(ClassObj, instance_variable, Name, Behaviour)
    ;   Type == cvar
    ->  get(ClassObj, class_variable, Name, Behaviour)
    ).

spy(TE, Val:[bool]) :->
    "Set spy-point"::
    get(TE, behaviour, Behaviour),
    (   Val == @off
    ->  nospypce(Behaviour)
    ;   spypce(Behaviour)
    ).

trace(TE, Val:[bool]) :->
    "Set trace-point"::
    get(TE, behaviour, Behaviour),
    (   Val == @off
    ->  notracepce(Behaviour)
    ;   tracepce(Behaviour)
    ).

identify(TE) :->
    "Identify myself"::
    get(TE, identifier, Id),
    atomic_list_concat([Type, Name, Class], $, Id),
    identify_behaviour(Type, Name, Class, TE).

identify_behaviour(send, Name, Class, TE) :-
    send(TE, report, status, 'XPCE send method %s->%s', Class, Name).
identify_behaviour(get, Name, Class, TE) :-
    send(TE, report, status, 'XPCE get method %s<-%s', Class, Name).
identify_behaviour(var, Name, Class, TE) :-
    send(TE, report, status, 'XPCE instance variable %s-%s', Class, Name).
identify_behaviour(cvar, Name, Class, TE) :-
    send(TE, report, status, 'XPCE class variable %s.%s', Class, Name).

:- pce_group(popup).

:- free(@sb_xpce_behaviour_popup).
:- pce_global(@sb_xpce_behaviour_popup,
              make_sb_xpce_behaviour_popup).

make_sb_xpce_behaviour_popup(P) :-
    new(P, popup(predicate_options)),

    send_list(P, append,
              [ menu_item(edit,
                          message(@arg1, open),
                          condition := message(@arg1, has_source)),
                menu_item(spy,
                          message(@arg1, spy),
                          condition := message(@arg1, loaded)),
                menu_item(trace,
                          message(@arg1, trace),
                          condition := message(@arg1, loaded))
              ]).

popup(_, Popup:popup) :<-
    Popup = @sb_xpce_behaviour_popup.

:- pce_end_class(toc_xpce_entity).

:- pce_begin_class(toc_xpce_class, toc_source_folder,
                   "Representation of a class (or extension)").

variable(class_id,      name,    get, "Class it represents").
variable(summary,       string*, get, "Summary documentation").

initialise(CF, Class:name, Summary:[string], Image:[image]) :->
    "Create from ClassName, Summary and Image"::
    default(Summary, @nil, Sum),
    (   Image == @default
    ->  file_image('class.svg', Img)
    ;   Img = Image
    ),
    send_super(CF, initialise, Class, @default, Img),
    send(CF, slot, class_id, Class),
    send(CF, slot, summary, Sum).


expand(CF) :->
    get(CF, identifier, NodeId),
    get(CF, file_id, BrowseId),
    get(CF, class_id, Class),
    get(CF, window, TocWindow),
    (   file_expansion_entity(BrowseId, entity(Info, Line)),
        make_class_toc_enter(Info, Class, BrowseId, Entry),
        send(TocWindow, son, NodeId, Entry),
        send(Entry, file_id, BrowseId),
        integer(Line),
        send(Entry, line, Line),
        fail
    ;   true
    ).

make_class_toc_enter(xpce_class_local_predicate(Class,Head), Class, Key, TE) :-
    make_file_toc_entry(predicate(Head), Key, TE),
    !.
make_class_toc_enter(Term, Class, _Key, TE) :-
    make_class_toc_enter(Term, Class, TE).

make_class_toc_enter(xpce_method(send(Class, Name, _Doc)), Class, TE) :-
    atomic_list_concat([send, Name, Class], $, Id),
    file_image('send.svg', Img),
    new(TE, toc_xpce_entity(Name, Id, Img)).
make_class_toc_enter(xpce_method(get(Class, Name, _Doc)), Class, TE) :-
    atomic_list_concat([get, Name, Class], $, Id),
    file_image('get.svg', Img),
    new(TE, toc_xpce_entity(Name, Id, Img)).
make_class_toc_enter(xpce_variable(Class, Name, _Doc), Class, TE) :-
    atomic_list_concat([var, Name, Class], $, Id),
    file_image('ivar.svg', Img),
    new(TE, toc_xpce_entity(Name, Id, Img)).
make_class_toc_enter(xpce_class_variable(Class, Name, _Doc), Class, TE) :-
    atomic_list_concat([cvar, Name, Class], $, Id),
    file_image('classvar.svg', Img),
    new(TE, toc_xpce_entity(Name, Id, Img)).

identify(CF) :->
    "Report who I am"::
    get(CF, class_id, Class),
    (   get(CF, summary, Summary),
        Summary \== @nil
    ->  send(CF, report, status, 'XPCE class %s (%s)', Class, Summary)
    ;   send(CF, report, status, 'XPCE class %s', Class)
    ).

:- pce_group(popup).

:- free(@sb_xpce_class_popup).
:- pce_global(@sb_xpce_class_popup, make_sb_xpce_class_popup).

make_sb_xpce_class_popup(P) :-
    new(P, popup(source_options)),

    send_list(P, append,
              [ menu_item(edit,
                          message(@arg1, edit),
                          condition := message(@arg1, has_source),
                          end_group := @on),
                menu_item(class_details,
                          message(@arg1, class_details),
                          condition := message(@arg1, loaded)),
                menu_item(class_hierarchy,
                          message(@arg1, class_hierarchy),
                          condition := message(@arg1, loaded))
              ]).

popup(_, Popup:popup) :<-
    Popup = @sb_xpce_class_popup.

loaded(CF) :->
    "Test if class is loaded"::
    get(CF, class_id, Class),
    pce_prolog_class(Class).

class_details(CF) :->
    "Open ClassBrowser"::
    get(CF, class_id, Class),
    manpce(Class).

class_hierarchy(CF) :->
    "Open and direct ClassHierarchy"::
    get(CF, class_id, Class),
    manpce,
    get(@manual, start_tool, class_hierarchy, Tool),
    send(Tool, focus, Class).

:- pce_end_class(toc_xpce_class).

:- pce_begin_class(toc_module, toc_source_folder,
                   "Representation of the module").

expand(MF) :->
    get(MF, identifier, NodeId),
    get(MF, file_id, BrowseId),
    get(MF, window, TocWindow),
    (   x_browse_info(BrowseId, export(Head)),
        (   exported_definition(BrowseId, Head, Line)
        ->  make_file_toc_entry(predicate(Head), BrowseId, Entry),
            send(Entry, file_id, BrowseId),
            send(Entry, line, Line)
        ;   local_predicate_name(Head, Label),
            atom_concat('$export$', Label, Id),
            new(Entry, toc_file(Label, Id, 'pred.svg'))
        ),
        send(TocWindow, son, NodeId, Entry),
        fail
    ;   true
    ).

update_image(MF) :->
    get(MF, status, Status),
    image(module, Status, Image),
    send(MF, image, Image).

%!  exported_definition(+BrowseId, +Head, -Line) is semidet.
%
%   Line is the location where the exported Head is defined.  Exported
%   non-terminals are found as grammar_rule/1 entities.

exported_definition(BrowseId, Head, Line) :-
    (   x_browse_info(BrowseId, entity(predicate(Head), Line))
    ;   x_browse_info(BrowseId, entity(grammar_rule(Head), Line))
    ),
    !.

:- pce_end_class.

:- pce_begin_class(sb_predicate_list, toc_source_folder,
                   "Representation of predicate set").

variable(set,   name,   get, "Name of the represented set").

initialise(PL, Name:name, Id:any, Img:[image]) :->
    send(PL, send_super, initialise, Name, Id, Img),
    send(PL, slot, set, Name).

expand(MF) :->
    get(MF, identifier, NodeId),
    get(MF, set, Set),
    get(MF, file_id, BrowseId),
    get(MF, window, TocWindow),
    Term =.. [Set, Head],
    (   file_expansion_entity(BrowseId, entity(Term, Line)),
        make_file_toc_entry(predicate(Head), BrowseId, Entry),
        send(Entry, file_id, BrowseId),
        send(Entry, line, Line),
        send(TocWindow, son, NodeId, Entry),
        fail
    ;   true
    ).

update_image(MF) :->
    get(MF, set, Set),
    get(MF, status, Status),
    image(Set, Status, Image),
    send(MF, image, Image).

:- pce_end_class.

:- pce_begin_class(sb_predicate, toc_source_folder,
                   "Represents a predicate").

variable(name,           name,  get, "Name of the represented predicate").
variable(arity,          int,   get, "Arity of it").
variable(module,         name*, get, "Module (or local)").
variable(classification, name,  get, "Class of the predicate").

initialise(P, BrowseId:name, Name:name, Arity:int, Module:[name]*) :->
    default(Module, @nil, M),
    head_name_arity(Head0, Name, Arity),
    (   M == @nil
    ->  Head = Head0
    ;   Head = M:Head0
    ),
    local_predicate_name(Head, Label),
    classify_predicate(Head, BrowseId, Classification),
    send(P, slot, classification, Classification),
    image(predicate, Classification, Img),
    send(P, send_super, initialise, Label, @default, Img),
    send(P, slot, name, Name),
    send(P, slot, arity, Arity),
    send(P, slot, module, M),
    (   expandable(Head, Classification)
    ->  true
    ;   send(P, collapsed, @nil)
    ).

file_node(P, Node:sb_prolog_file) :<-
    "Find associated file-node"::
    file_node(P, Node).

file_node(Node, Node) :-
    send(Node, instance_of, sb_prolog_file),
    !.
file_node(Node, FileNode) :-
    get_chain(Node, parents, Parents),
    member(Parent, Parents),
    file_node(Parent, FileNode),
    !.

head(P, Qualify:[bool], Head:prolog) :<-
    "Get the head"::
    get(P, module, M),
    get(P, name, Name),
    get(P, arity, Arity),
    head_name_arity(Head0, Name, Arity),
    (   M == @nil
    ->  (   Qualify == @on
        ->  (   get(P, file_node, SbPrologFile),
                get(SbPrologFile, module, Module)
            ->  Head = Module:Head0
            ;   Head = user:Head0
            )
        ;   Head = Head0
        )
    ;   Head = M:Head0
    ).

expandable(Head, _) :-
    prolog_xbrowse:called(_, Head).

classify_predicate(Head, Key, dcg) :-
    x_browse_info(Key, entity(grammar_rule(Head), _)),
    !.
classify_predicate(Head, Key, dynamic) :-
    x_browse_info(Key, entity(dynamic(Head), _)),
    !.
classify_predicate(Head, _, imported) :-
    prolog_xbrowse:imported(Head),
    !.
classify_predicate(Head, Key, exported) :-
    x_browse_info(Key, export(Head)).
classify_predicate(Head, _, built_in) :-
    system_predicate(Head),
    !.
classify_predicate(Head, _, global) :-
    global_predicate(Head),
    !.
classify_predicate(Head, Key, incomplete) :-
    x_browse_info(Key, entity(unreferenced_call(Head, _), _)),
    !.
classify_predicate(Head, Key, unreferenced) :-
    x_browse_info(Key, entity(unreferenced_predicate(Head), _)),
    !.
classify_predicate(Head, Key, undefined) :-
    x_browse_info(Key, entity(unreferenced_call(_, To), _)),
    memberchk(Head, To),
    !.
classify_predicate(Head, _Key, fact) :-
    \+ prolog_xbrowse:called(_, Head),
    !.
classify_predicate(_, _, local).

expand(P) :->
    get(P, file_id, Key),
    get(P, head, Head),
    get(P, window, TocWindow),
    (   prolog_xbrowse:called(Called, Head),
        make_file_toc_entry(predicate(Called), Key, TE),
        send(TocWindow, son, P, TE),
        send(TE, slot, file_id, Key),
        predicate_location(Key, Called, Line),
        send(TE, slot, line, Line),
        fail
    ;   true
    ).

predicate_location(K, Called, Line) :-
    x_browse_info(K, entity(predicate(Called), Line)),
    !.
predicate_location(K, Called, Line) :-
    x_browse_info(K, entity(dynamic(Called), Line)),
    !.
predicate_location(K, Called, Line) :-
    x_browse_info(K, entity(grammar_rule(Called), Line)),
    !.
predicate_location(K, Called, Line) :-
    x_browse_info(K, entity(xpce_class_local_predicate(_, Called), Line)),
    !.

identify(P) :->
    "Identify myself as status"::
    get(P, classification, Class),
    get(P, name, Name),
    get(P, arity, Arity),
    identify_predicate(Class, Name/Arity, P).

identify_predicate(fact, Name/Arity, P) :-
    send(P, report, status,
         'Unit-clause predicate %s/%d', Name, Arity).
identify_predicate(Class, Name/Arity, P) :-
    send(P, report, status,
         '%s predicate %s/%d', Class?label_name, Name, Arity).

open(P) :->
    "Edit, manual or expand"::
    (   send(P, has_source)
    ->  send(P, edit)
    ;   send(P, has_manual)
    ->  send(P, manual)
    ;   send_super(P, open)
    ).

manual(P) :->
    get(P, name, Name),
    get(P, arity, Arity),
    (   help(Name/Arity)
    ->  true
    ;   send(P, report, warning, 'No help for %s/%d', Name, Arity)
    ).

has_manual(P) :->
    "Succeed if there is a manual-page"::
    get(P, name, Name),
    get(P, arity, Arity),
    man_predicate_summary(Name/Arity, _).

:- if(current_predicate(man_object_property/2)).
man_predicate_summary(PI, Summary) :-
    man_object_property(PI, Summary).
:- endif.
man_predicate_summary(_, _) :-
    fail.

built_in(P) :->
    "True is represented predicate is builtin"::
    get(P, head, Head),
    system_predicate(Head).

loaded(P) :->
    "Test if represented predicate is loaded"::
    get(P, file_node, Node),
    send(Node, loaded).

spy(P, Val:[bool]) :->
    "Switch spying on/off"::
    get(P, head, @on, Head),
    (   Val == @off
    ->  nospy(Head)
    ;   spy(Head)
    ).

trace(P, Val:[bool]) :->
    "Switch tracing on/off"::
    get(P, head, @on, Head),
    (   Val == @off
    ->  trace(Head, -all)
    ;   trace(Head)
    ).

:- free(@prolog_predicate_popup).
:- pce_global(@prolog_predicate_popup,
              make_prolog_predicate_popup).

make_prolog_predicate_popup(P) :-
    new(P, popup(predicate_options)),

    send_list(P, append,
              [ menu_item(edit,
                          message(@arg1, open),
                          condition := message(@arg1, has_source)),
                menu_item(spy,
                          message(@arg1, spy),
                          condition := message(@arg1, loaded)),
                menu_item(trace,
                          message(@arg1, trace),
                          condition := message(@arg1, loaded)),
                menu_item(manual,
                          message(@arg1, manual),
                          condition := message(@arg1, has_manual))
              ]).

popup(_, Popup:popup) :<-
    Popup = @prolog_predicate_popup.

:- pce_end_class(sb_predicate).


image(module,           open,           'openmodule.svg').
image(module,           closed,         'module.svg').

image(dynamic,          open,           'mini-run.svg').
image(dynamic,          closed,         'mini-run.svg').

image(predicate,        built_in,       'builtin.svg').
image(predicate,        global,         'mini-globe.svg').
image(predicate,        dynamic,        'mini-run.svg').
image(predicate,        imported,       'import.svg').
image(predicate,        exported,       'export.svg').
image(predicate,        incomplete,     'warnpred.svg').
image(predicate,        unreferenced,   'unrefpred.svg').
image(predicate,        undefined,      'undefpred.svg').
image(predicate,        fact,           'fact.svg').
image(predicate,        local,          'pred.svg').
image(predicate,        dcg,            'grammar.svg').



                 /*******************************
                 *             HOOK             *
                 *******************************/

:- multifile
    prolog:message_action/2.

image_of_load_state(start, _,       'loading.svg').
image_of_load_state(true,  load,    'plloadedfile.svg').
image_of_load_state(true,  include, 'plincludedfile.svg').
image_of_load_state(false, _,       'loadfailed.svg').

prolog:message_action(load_file(What), _Kind) :-
    loading(What, load).
prolog:message_action(include_file(What), _Kind) :-
    loading(What, include).

loading(What, How) :-
    load_info(What, File, Stage),
    prolog_overview_window(Win),
    (   file_name_extension(_, qlf, File)
    ->  debug(gtrace(qlf), 'Looking for ~q', [File]),
        '$qlf_sources'(File, Sources),
        convlist(qlf_part, Sources, Files),
        debug(gtrace(qlf), 'Contains ~p', [Files]),
        member(TheFile, Files)
    ;   TheFile = File
    ),
    image_of_load_state(Stage, How, Img),
    in_pce_thread(update_load_state(Win, TheFile, Img, Stage)).

qlf_part(source(File), File).

%!  update_load_state(+Win, +File, +Img, +Stage) is det.
%
%   Update the icon of the node for File.   If  the load completed, first
%   add File to the tree if it is not in there yet.

update_load_state(Win, File, Img, Stage) :-
    (   Stage == true
    ->  send(Win, loaded_file, File)
    ;   true
    ),
    (   get(Win, file_node, File, Node)
    ->  send(Node, image, Img)
    ;   true
    ).

load_info(start(_Level, file(_, Path)),
          Path, start).
load_info(failed(Spec),
          Path, false) :-
    absolute_file_name(Spec,
                       [ file_type(prolog),
                         access(read)
                       ],
                       Path).
load_info(done(_Level, file(_, Path), _, _, _, _),
          Path, true).
load_info(done(_Level, file(_, Path)),
          Path, true).

file_to_image(File, Img) :-
    icon_size(H),
    new(Img, image(File, H, H)).

icon_size(H) :-
    get(@pce, convert, normal, font, Font),
    get(Font, height, FH),
    H is round(FH*0.8).
