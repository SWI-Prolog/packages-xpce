/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1995-2026, University of Amsterdam
                              VU University Amsterdam
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

:- module(pce_demo, []).
:- encoding(utf8).
:- use_module(library(pce)).
:- use_module(contrib(contrib)).
:- use_module(library(persistent_frame)).
:- autoload(library(process), [process_which/2]).
:- autoload(library(pce_emacs), [emacs/1]).
:- autoload(library(apply), [include/3]).
:- autoload(library(lists), [member/2]).

/** <module> XPCE demo starter

This file defines a demo-starting tool.  The demo's themselves should be
in the library files 'demo/<demo-file>.pl'. At the end of this file is a
list of available demo's.
*/

:- pce_begin_class(man_demo_browser, man_frame,
                   "Show demo programs").

initialise(F, M:man_manual) :->
    "Create tool for manual"::
    send_super(F, initialise, M, 'XPCE Demo Programs'),
    send(F, append, new(B, browser(@default, size(80,25)))),
    send(B, confirm_done, @off),
    get(B?font, width, "Drag-and-drop File   ", LeftWidth),
    send(B, tab_stops, vector(LeftWidth)),
    send(F, fill_browser, B),

    send(new(D, dialog), below, B),
    send(D, append, new(O, button(open, message(F, open_demo, B)))),
    send(D, append, button(source, message(F, view_source, B))),
    send(D, append, button(quit, message(F, destroy))),
    send(D, default_button, open),

    send(B, open_message, message(O, execute)),
    send(B, style, title, style(font := boldlarge)).

fill_browser(_F, B:browser) :->
    forall(demo_group(Title, Names),
           ( include(available_demo, Names, Available),
             Available \== []
           ->  send(B, append, dict_item(Title, style := title)),
               forall(member(Name, Available),
                      append_demo(B, Name))
           ;   true
           )),
    findall(Name, ( demo(Name, _, _, _),
                    \+ ( demo_group(_, Names), memberchk(Name, Names) )
                  ), Others0),
    sort(Others0, Others),
    (   Others \== []
    ->  send(B, append, dict_item('Other demos', style := title)),
        forall(member(Name, Others), append_demo(B, Name))
    ;   true
    ),
    send(B, append,
         dict_item('💙 Contributions 💙',
                   style := title)),
    forall(contribution(Name, Summary, _Author, _, _),
           send(B, append, dict_item(Name,
                                     string('%s\t%s', Name, Summary)))).

available_demo(Name) :-
    demo(Name, _, _, _),
    !.

append_demo(B, Name) :-
    demo(Name, Summary, _, _),
    !,
    send(B, append, dict_item(Name, string('%s\t%s', Name, Summary))).

open_demo(_F, Browser:browser) :->
    "Open a demo"::
    get(Browser, selection, DictItem),
    (   (   DictItem == @nil
        ;   get(DictItem, style, title)
        )
    ->  send(@display, inform, Browser, @default, 'First select a demo')
    ;   get(DictItem, key, Name),
        (   (   demo(Name, Summary, File, Predicate)
            ;   contribution(Name, Summary, _Author, File, Predicate)
            )
        ->  (   send(@pce, report, progress, 'Loading demo %s ...', Summary),
                use_module(File),
                send(@pce, report, done)
            ->  (   Predicate
                ->  true
                ;   send(@pce, inform, 'Failed to start %s demo', Name)
                )
            ;   send(@pce, inform, 'Can''t find demo sourcefile')
            )
        ;   send(Browser, report, error, 'No such demo: %s', Name)
        )
    ).

view_source(_F, Browser:browser) :->
    "View source for a demo program"::
    get(Browser, selection, DictItem),
    (   DictItem == @nil
    ->  send(@display, inform, Browser, @default, 'First select a demo')
    ;   get(DictItem, key, Name),
        (   demo(Name, _, File, _)
        ;   contribution(Name, _, _Author, File, _)
        ),
        (   locate_file(File, Path)
        ->  emacs(Path)
        ;   term_to_atom(File, FileAtom),
            send(Browser, report, error,
                 'Can''t find source from %s', FileAtom)
        )
    ).

locate_file(Base, File) :-
    absolute_file_name(Base,
                       [ file_type(prolog),
                         access(read)
                       ], File).

                /********************************
                *             DEMO'S            *
                ********************************/

%!  demo_group(?Title, ?Demos) is nondet.
%
%   The demo browser shows the demos in groups, in this order.  Demos
%   that are not in a group are shown under "Other demos".

demo_group('Applications and tools',
           [ 'PceDraw', 'ImageViewer', 'FontViewer', 'XMLView',
             'GraphViewer', xdot, 'Random Terms', 'London Tube',
             'Ispell', 'ChessTool'
           ]).
demo_group('Dialogs and windows',
           [ 'Dialog gallery', 'Tabs and splitting', 'Drag-and-drop File',
             'Cursors', 'EventHierarchy'
           ]).
demo_group('Graphics',
           [ 'Path gallery', 'Paint gallery', 'Text gallery', 'Arc gallery',
             'Chart gallery', 'Canvas', 'Transform', 'Opacity', 'Colours',
             'HSV Colours', 'Constraints'
           ]).
demo_group('Media and devices',
           [ 'Camera', 'Video player', 'Sound'
           ]).
demo_group('Games and animation',
           [ 'Asteroids', 'Animated images', 'Kangaroo', 'Juggler'
           ]).

demo('PceDraw',                         % Name
     'Drawing tool',                    % Summary
     library(pcedraw),                  % Sources
     pcedraw).                          % Toplevel predicate

demo('Ispell',
     'Graphical interface to ispell (requires ispell 3)',
     demo(ispell),
     ispell) :-
    send(@pce, has_feature, process),
    process_which(path(ispell), _Path).

demo('FontViewer',
     'Examine PCE predefined fonts',
     demo(fontviewer),
     fontviewer).

demo('Cursors',
     'Displays browser of available cursors',
     demo(cursor),
     cursor_demo).

demo('Colours',
     'Displays browser of named colours',
     demo(colour),
     colour_browser).

demo('HSV Colours',
     'Colour browser using Hue-Saturation-Value',
     demo(hsvcolour),
     hsv_browser).

demo('ImageViewer',
     'Examine image files in a directory',
     demo(imageviewer),
     image_viewer).

demo('EventHierarchy',
     'Display hierarchy of event-types',
     demo(event_hierarchy),
     event_hierarchy).

demo('Camera',
     'Show and capture the video of a camera',
     demo(camera_demo),
     camera_demo).

demo('Video player',
     'Play video files (MP4, MKV, WebM, ...) with sound',
     demo(video_player),
     video_player).

demo('Sound',
     'Record, replay and load sound (WAV, MP3, Ogg, FLAC, ...)',
     demo(sound_demo),
     sound_demo).

demo('Animated images',
     'Play animated PNG, GIF and WebP files frame by frame',
     demo(animation_demo),
     animation_demo).

demo('Asteroids',
     'A game like Asteroids: GPU sprites, keyboard polling, gamepad and sound',
     demo(asteroids),
     asteroids).

demo('Drag-and-drop File',
     'Receive files dropped from the desktop (drop_file/drop_text events)',
     demo(dragdrop_file),
     dragdrop_file_demo).

demo('Tabs and splitting',
     'Tabs holding split windows: split, resize, move panes around',
     demo(tabframe),
     tab_frame_demo).

demo('Transform',
     'Live figure->transform: rotate, scale and shear a small scene',
     demo(transform),
     transform_demo).

demo('Dialog gallery',
     'The dialog items: buttons, text, menus, sliders, pickers, groups',
     demo(dialog_gallery),
     dialog_gallery).

demo('Arc gallery',
     'A gallery of class arc usage',
     demo(arc),
     arc_demo).

demo('Opacity',
     'Per-graphical opacity: ramps, primitives, groups, overlays',
     demo(opacity),
     opacity_demo).

demo('Path gallery',
     'Curves, arcs, sub-paths and SVG path data of class path',
     demo(paths),
     paths_demo).

demo('Paint gallery',
     'Gradients, image fills, transparency and line styles',
     demo(paint),
     paint_demo).

demo('Text gallery',
     'Pango markup text, clip paths, blend modes and opacity',
     demo(compose),
     compose_demo).

demo('Canvas',
     'Drawing curves using Cairo-style operations on a canvas',
     demo(canvas_demo),
     canvas_demo).

demo('Chart gallery',
     'Curves, axes and bar charts from library(plot)',
     demo(charts),
     charts_demo).

demo('London Tube',
     'Interactive London tube map: zoom, pan, search and toggle lines',
     demo(tube_gui),
     london_tube).

demo('xdot',
     'Graphviz layout viewer: render .dot files (requires graphviz)',
     demo(xdot_demo),
     xdot_demo) :-
    process_which(path(dot), _Path).

demo('Random Terms',
     'Generate random Prolog terms and show as graph',
     demo(random_terms_demo),
     random_terms_demo) :-
    process_which(path(dot), _Path).

demo('GraphViewer',
     'Visualise a graph represented as Prolog facts',
     demo(graph),
     graph_viewer).

demo('XMLView',
     'Browse structure of HTML/SGML and XML files',
     library('doc/xml_browse'),
     send(new(xml_browser), open)).

demo('ChessTool',
     'Simple frontend for /usr/games/chess',
     demo(chess),
     chess) :-
    send(@pce, has_feature, process),
    absolute_file_name('/usr/games/chess', Path),
    send(file(Path), access, execute).

demo('Constraints',
     'Using constraints and relations',
     demo(constraint),
     constraint_demo).

demo('Kangaroo',
     'Jumping kangaroos demo',
     demo(kangaroo),
     kangaroo).

demo('Juggler',
     'Animation of a juggling creature',
     demo(juggler),
     juggle_demo).

:- pce_end_class.
