/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(test_redraw_reentrance, [test_redraw_reentrance/0]).
:- encoding(utf8).

/** <module> A redraw may not be re-entered from inside a redraw

Resizing an SDL window posts SDL_EVENT_WINDOW_RESIZED, and SDL hands an
event to the watches that are registered for it at SDL_PushEvent() time
-- on the thread that pushes it, before it is queued.  Our watch is
live_resize_watch() (packages/xpce/src/sdl/sdlframe.c), which lays the
frame out and paints it, because during the modal resize loop of MacOS
and Windows that is our only chance to draw.

So anything that calls into SDL while we are painting can arrive back in
the redraw machinery with a drawing context open: `frame ->size' from a
->compute or a ->_redraw_area is enough.  The window is then painted
again inside its own redraw, clipped to what the outer redraw was given,
and a window that the layout resizes has the backing store its open
context draws into destroyed under it.  This is what printed

    d_window(<pce>(...,pane_tabbed_window)): Context is already open

The test asks for the resize from inside a ->_redraw_area and checks
that the redraw is not re-entered, and that the resize still arrives
once the paint is over.

Run with:

    swipl -Dxpce_defaults=none -g test_redraw_reentrance -t halt \
          packages/xpce/tests/test_redraw_reentrance.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).

test_redraw_reentrance :-
    run_tests([ redraw_reentrance
              ]).

                 /*******************************
                 *            HARNESS           *
                 *******************************/

%       A box that resizes the frame it is painted in, once, and keeps
%       track of how deeply its own painting is nested.

:- pce_begin_class(reentrance_probe, box,
                   "Box that resizes its frame while it is painted").

variable(depth,     int  := 0,   both, "Nesting of ->_redraw_area").
variable(max_depth, int  := 0,   both, "Deepest nesting seen").
variable(fired,     bool := @off, both, "Asked for the resize already").

'_redraw_area'(B, A:area) :->
    "Paint, and resize my frame the first time"::
    get(B, depth, D0),
    D is D0+1,
    send(B, depth, D),
    get(B, max_depth, M),
    (   D > M
    ->  send(B, max_depth, D)
    ;   true
    ),
    send_super(B, '_redraw_area', A),
    (   get(B, fired, @off),
        get(B, frame, Frame),
        Frame \== @nil
    ->  send(B, fired, @on),
        send(Frame, size, size(500,400))
    ;   true
    ),
    send(B, depth, D0).

:- pce_end_class(reentrance_probe).

%!  dispatch(+Times) is det.
%
%   Give the event loop a chance to paint and to see the resize.

dispatch(0) :- !.
dispatch(N) :-
    ignore(send(@display, dispatch)),
    N1 is N-1,
    dispatch(N1).

probe(Frame, Window, Probe) :-
    new(Frame, frame('test_redraw_reentrance')),
    send(Frame, append, new(Window, window)),
    send(Window, display, new(Probe, reentrance_probe(100,100)), point(10,10)),
    send(Frame, open),
    dispatch(10).

                 /*******************************
                 *            TESTS             *
                 *******************************/

:- begin_tests(redraw_reentrance).

test(not_re_entered, [cleanup(send(Frame, destroy))]) :-
    probe(Frame, _Window, Probe),
    get(Probe, max_depth, Depth),
    assertion(Depth =< 1).

% And the resize is not lost: the event stays in the queue and the main
% loop acts on it as soon as the paint is over.

test(resize_arrives, [cleanup(send(Frame, destroy))]) :-
    probe(Frame, _Window, Probe),
    assertion(get(Probe, fired, @on)),
    dispatch(4),
    get(Frame, area, area(_,_,W,H)),
    assertion(W == 500),
    assertion(H == 400).

:- end_tests(redraw_reentrance).
