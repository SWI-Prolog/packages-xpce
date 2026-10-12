/*  Figures for toc.plx: "Library pce_toc: displaying hierarchies"
*/

:- module(fig_libtoc, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).
:- use_module(library(lists)).

:- multifile
    screenshot:figure/2.

%   \figref{libtoc}: the explorer of the section "Library pce_toc:
%   displaying hierarchies".  The code below is copied from the guide.
%   We explore the directory prolog/lib/doc of the xpce package, such
%   that the figure does not depend on the environment, expand the
%   root and open the README file.  The frame is made narrower to fit
%   the page.

screenshot:figure(libtoc, E) :-
    absolute_file_name('../../prolog/lib/doc', Dir,
                       [ file_type(directory),
                         access(read)
                       ]),
    new(E, explorer(directory(Dir))),
    send(E, open),
    send(E, size, size(720, 420)),
    get(E, member, view, View),         % cut long lines, as a fixed
    send(View, wrap, none),             % width editor would
    settle,
    get(E, member, directory_hierarchy, DH),
    send(DH, expand_root),
    get(DH, root, Root),
    get(Root, sons, Sons),
    chain_list(Sons, Nodes),
    member(Node, Nodes),
    get(Node?image, label, 'README'),
    !,
    send(Node, select),
    send(Node, open).

:- pce_autoload(toc_window, library(pce_toc)).
:- pce_autoload(report_dialog, library(pce_report)).

:- pce_begin_class(explorer, frame, "Explore the filesystem").

initialise(E, Dir:directory) :->
        "Explore from directory"::
        send_super(E, initialise, 'Simple explorer'),
        send(E, append, new(DH, directory_hierarchy(Dir))),
        send(new(view), right, DH),
        send(new(report_dialog), below, DH).

open_node(E, Node:file) :->
        "Show file content of opened node"::
        get(E, member, view, View),
        send(View, load, Node).

:- pce_end_class.


:- pce_begin_class(directory_hierarchy, toc_window,
                   "Browser for a directory-hierarchy").

initialise(FB, Root:directory) :->
        send(FB, send_super, initialise),
        get(Root, name, Name),
        send(FB, root, toc_folder(Name, Root)).

expand_node(FB, D:directory) :->
        "Called if a node is to be expanded"::
        new(SubDirsNames, chain),
        new(FileNames, chain),
        send(D, scan, FileNames, SubDirsNames),

        get(SubDirsNames, map, ?(D, directory, @arg1), SubDirs),
        send(SubDirs, for_all,
             message(FB, son, D,
                     create(toc_folder, @arg1?name, @arg1))),
        get(FileNames, map, ?(D, file, @arg1), SubFiles),
        send(SubFiles, for_all,
             message(FB, son, D,
                     create(toc_file, @arg1?base_name, @arg1))).

open_node(FB, Node:file) :->
        "Called if a file is double-clicked"::
        send(FB?frame, open_node, Node).

:- pce_end_class.
