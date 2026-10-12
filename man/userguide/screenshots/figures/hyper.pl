/*  Figures for hyper.plx: "Relating frames"
*/

:- module(fig_hyper, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).

:- multifile
    screenshot:figure/2.

%   \figref{linkhyper}: the classes link_demo and link_box of the
%   section "Hypers" with three connected boxes, after double-clicking
%   the red box, which runs `link_box->edit`.

screenshot:figure(linkhyper, [Frame-point(0,0), DF-point(DX,DY)]) :-
    new(P, link_demo),
    send(P, size, size(480, 240)),
    send(P, open),
    get(P, frame, Frame),
    settle,
    send(P, add_box, point(40, 30)),
    send(P, add_box, point(350, 30)),
    send(P, add_box, point(350, 150)),
    get(P, graphicals, Boxes),
    chain_list(Boxes, [R,G,B]),
    send(R, fill, colour(red)),
    send(G, fill, colour(green)),
    send(B, fill, colour(blue)),
    new(L, link(link)),                 % as used by connect_gesture
    send(R, connect, G, L),
    send(G, connect, B, L),
    send(R, connect, B, L),
    send(R, edit),
    get(R, hypered, dialog, D),
    get(D, frame, DF),
    get(R, display_position, point(BX, BY)),
    get(Frame, position, point(FX, FY)),
    DX is BX-FX+20,
    DY is BY-FY+100.

:- use_module(library(hyper)).

:- pce_begin_class(link_demo, picture).

initialise(P) :->
    send_super(P, initialise, 'Link Demo'),
    send(P, recogniser,
         click_gesture(left, '', single,
                       message(P, add_box, @event?position))).

add_box(P, At:point) :->
    send(P, display, new(link_box), At).

:- pce_end_class(link_demo).

:- pce_begin_class(link_box, box).

handle(w/2, 0, link, north).
handle(w/2, h, link, south).
handle(0, h/2, link, west).
handle(w, h/2, link, east).

initialise(B) :->
    send_super(B, initialise, 100, 50),
    send_list(B, recogniser,
              [ click_gesture(left, '', double,
                              message(B, edit)),
                new(connect_gesture),
                new(move_gesture)
              ]).

colour(red).
colour(green).
colour(blue).
colour(yellow).

edit(B) :->
    "Allow changing colour"::
    new(D, dialog('Select colour')),
    send(D, append, new(M, menu(colour, choice,
                                message(?(D, hypered, box),
                                        fill,
                                        @arg1)))),
    forall(colour(Colour),
           ( new(I, image(@nil, 32, 16)),
             send(I, fill, colour(Colour)),
             send(M, append,
                  menu_item(colour(Colour), @default, I))
           )),
    send(D, append, button(done, message(D, destroy))),
    new(_, partof_hyper(B, D, dialog, box)),
    get(B, display_position, PosB),
    get(PosB, plus, point(20,100), PosD),
    send(D, open, PosD).

:- pce_end_class(link_box).
