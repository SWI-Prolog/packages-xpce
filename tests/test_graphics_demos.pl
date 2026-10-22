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

:- module(test_graphics_demos, [test_graphics_demos/0]).

/** <module> The graphics demos load and build their windows

Each demo is started and its frame destroyed.  This guards the demos of
paths, paint, markup text, compositing and the canvas against changes
in the features they show.

Run with:

    swipl -g test_graphics_demos -t halt \
          packages/xpce/tests/test_graphics_demos.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_graphics_demos :-
    run_tests([ graphics_demos
              ]).

%!  run_demo(+File, +Goal) is semidet.
%
%   Load the demo File, run Goal and destroy the frame it opened.

run_demo(File, Goal) :-
    use_module(demo(File)),
    get(@display, frames, Frames0),
    chain_list(Frames0, Before),
    call(Goal),
    get(@display, frames, Frames),
    chain_list(Frames, After),
    subtract(After, Before, New),
    New \== [],
    forall(member(F, New), send(F, destroy)).

scan_tree(Dir) :-
    tmp_file(du, Dir),
    make_directory(Dir),
    directory_file_path(Dir, sub, Sub),
    make_directory(Sub),
    sized_file(Dir, 'a.txt', 100),
    sized_file(Sub, 'b.txt', 50),
    sized_file(Sub, 'c.txt', 3000),
    directory_file_path(Sub, loop, Loop),   % a symbolic link to itself
    catch(link_file(loop, Loop, symbolic), error(_, _), true).

sized_file(Dir, Name, Size) :-
    directory_file_path(Dir, Name, File),
    length(Codes, Size),
    maplist(=(0'x), Codes),
    setup_call_cleanup(open(File, write, Out),
                       format(Out, '~s', [Codes]),
                       close(Out)).

%   wait_for(:Get, :Test, +Timeout)
%
%   Dispatch events until Test is true after Get.

wait_for(Get, Test, Timeout) :-
    get_time(T0),
    repeat,
      ignore(send(@display, dispatch)),
      call(Get),
      (   call(Test)
      ->  !
      ;   get_time(Now),
          Now-T0 > Timeout
      ->  !, fail
      ;   sleep(0.05),
          fail
      ).

:- begin_tests(graphics_demos).

test(paths)   :- run_demo(paths, paths_demo:paths_demo).
test(paint)   :- run_demo(paint, paint_demo:paint_demo).
test(compose) :- run_demo(compose, compose_demo:compose_demo).
test(canvas)  :- run_demo(canvas_demo, canvas_demo:canvas_demo).
test(ripples) :- run_demo(ripples, ripples_demo:ripples_demo).
test(disk_usage) :-                     % destroyed while scanning
    module_property(test_graphics_demos, file(File)),
    file_directory_name(File, Dir),
    run_demo(disk_usage, disk_usage_demo:disk_usage_demo(Dir)).

%   Scan a folder with files of 100, 50 and 3000 bytes and a symbolic
%   link that loops.  The scan runs
%   in a thread that passes the result to the xpce thread, so we
%   dispatch events until it arrives.

test(disk_usage_scan, [ Total-Labels-SubLabels ==
                        3150-[sub, 'a.txt']-['b.txt', 'c.txt'],
                        setup(scan_tree(Dir)),
                        cleanup(delete_directory_and_contents(Dir))
                      ]) :-
    use_module(demo(disk_usage)),
    new(F, disk_usage_demo:disk_usage_frame),
    send(F, scan, Dir),
    get(F, chart_window, W),
    wait_for(get(W, root, Root), Root \== @nil, 10),
    get(Root, total, Total),
    child_labels(Root, Labels),
    get(Root?children, head, Sub),
    child_labels(Sub, SubLabels0),
    msort(SubLabels0, SubLabels),
    send(F, destroy).

%   The popup scans the parent of the folder shown and the folder it
%   is on.

test(disk_usage_parent, [ Labels1-Labels2 == [sub, 'a.txt']-['b.txt', 'c.txt'],
                          setup(scan_tree(Dir)),
                          cleanup(delete_directory_and_contents(Dir))
                        ]) :-
    use_module(demo(disk_usage)),
    new(F, disk_usage_demo:disk_usage_frame),
    directory_file_path(Dir, sub, Sub),
    get(F, chart_window, W),
    send(F, scan, Sub),
    wait_for(get(W, root, Root0), Root0 \== @nil, 10),
    send(F, scan_parent),
    wait_for(get(W, root, Root1), Root1 \== @nil, 10),
    child_labels(Root1, Labels1),
    get(Root1?children, find, @arg1?label == sub, SubNode),
    send(W, slot, popup_node, SubNode),
    send(F, can_scan_node),
    send(F, scan_node),
    wait_for(get(W, root, Root2), Root2 \== @nil, 10),
    child_labels(Root2, Labels0),
    msort(Labels0, Labels2),
    send(W, slot, popup_node, Root2),
    \+ send(F, can_scan_node),          % already shown
    send(F, destroy).

child_labels(Node, Labels) :-
    get(Node?children, map, @arg1?label?value, Ch),
    chain_list(Ch, Labels).

test(blend_menu) :-
    use_module(demo(compose)),
    compose_demo:compose_demo,
    get(@display, frames, Frames),
    get(Frames, tail, F),
    get(F, member, picture, P),
    compose_demo:set_blend(P, screen),
    get(P?graphicals, find, @arg1?name == blend_figure, Fig),
    get(Fig, blend, Blend),
    send(F, destroy),
    assertion(Blend == screen).

:- end_tests(graphics_demos).
