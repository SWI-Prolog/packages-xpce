/*  Figures for graphics.plx: "Graphics"
*/

:- module(fig_graphics, []).
:- use_module(library(pce)).

:- multifile
    screenshot:figure/2.

%   \figref{graphics}: the result of the queries 1-10 at the start of
%   the chapter.

screenshot:figure(graphics, @p) :-
    new(@p, picture('Demo Picture')),
    send(@p, open),
    send(@p, display,
         new(@bo, box(100,100))),
    send(@p, display,
         new(@ci, circle(50)), point(25,25)),
    send(@p, display,
         new(@bm, bitmap('32x32/books.png')), point(100,100)),
    send(@p, display,
         new(@tx, text('Hello')), point(120, 50)),
    send(@p, display,
         new(@bz, bezier_curve(point(50,100),
                               point(120,132),
                               point(50, 160),
                               point(120, 200)))),
    send(@bo, radius, 10),
    send(@ci, fill, colour(orange)),
    send(@tx, font, font(times, bold, 18)),
    send(@bz, arrows, both).

%   \figref{connect}: linked_box_demo/0 of the section "Connecting
%   graphical objects".

screenshot:figure(connect, P) :-
    linked_box_demo,
    get(@display, frames, Frames),
    get(Frames?tail, member, picture, P).

:- pce_global(@in_out_link, make_in_out_link).

make_in_out_link(L) :-
    new(L, link(in, out, line(arrows := second))).

linked_box_demo :-
    new(P, picture('Linked Box demo')),
    send(P, open),
    send(P, display, new(B1, box(50,50)), point(20,20)),
    send(P, display, new(B2, box(25,25)), point(100,100)),
    send(B1, handle, handle(w, h/2, in)),
    send(B2, handle, handle(w/2, 0, out)),
    send_list([B1, B2], recogniser, new(move_gesture)),
    send(B1, connect, B2, @in_out_link).
