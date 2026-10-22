/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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



:- module(ripples_demo,
          [ ripples_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(random)).

/** <module> A ripple tank: water over a picture

Drag the mouse through the water to stir it, click to drop a stone,
which stays on the floor.  Using a pen, the pressure decides how deep
the waves are, and the barrel button drops a stone.  The waves refract the picture below and
catch the light.  The dialog toggles rain, sets the viscosity of the
water, runs the simulation as fast as possible and clears the floor.
Resize the window to enlarge the pool: the number of triangles grows
with its area.

The water surface is an sdl_mesh: a grid of vertices every few pixels,
two triangles per grid cell, showing the picture as its texture.  On
every tick, a wave equation moves the heights of the grid points in
Prolog.  Each vertex then shifts its position in the texture along the
slope of the water, which makes the picture refract, and its colour
shades the slope.  A second mesh without texture uses additive
blending to add glints where the water is steep.  pce_sdl_mesh_set/3
replaces both meshes on every tick.  The floor of the pool is a
figure, drawn into the texture.  A stone that is dropped is added to
the figure and drawn into the texture.

Normally a timer ticks every 16 milliseconds.  At maximum speed, each
tick starts the timer once with interval 0.  Its event is queued
behind the pending events, so xpce handles user input and draws the
window before the next tick, as JavaScript code does using
setTimeout(f, 0).

Run with:

    ?- ripples_demo.
*/

:- set_prolog_flag(optimise, true).	% compile arithmetic

ripples_demo :-
    new(F, ripples_frame),
    send(F, open).

:- pce_begin_class(ripples_frame, frame, "Ripple tank demo").

initialise(F) :->
    send_super(F, initialise, 'Ripples'),
    send(F, append, new(W, ripples_window(size(800, 560)))),
    send(new(D, dialog), below, W),
    send(D, append,
         new(R, menu(rain, toggle, message(W, rain, @arg2)))),
    send(R, append, rain),
    send(R, show_label, @off),
    send(D, append,
         new(S, slider(viscosity, 0, 20, 4, message(W, viscosity, @arg1))),
         right),
    send(S, width, 150),
    send(D, append,
         new(M, menu(max_speed, toggle, message(W, max_speed, @arg2))),
         right),
    send(M, append, max_speed),
    send(M, show_label, @off),
    send(D, append, button(calm, message(W, calm)), right),
    send(D, append, button(clear_floor, message(W, clear_floor)), right),
    send(D, append, button(quit, message(F, destroy)), right).

:- pce_end_class(ripples_frame).


%   The window holds the heights of the grid points as a list of rows,
%   each a list of floats, for the current and the previous tick.  The
%   wave equation needs both.

:- pce_begin_class(ripples_window(size), sdl_window,
                   "Window with a water surface").

variable(heights,   prolog, both, "Rows of heights of the grid points").
variable(previous,  prolog, both, "Heights at the previous tick").
variable(indices,   prolog, both, "Triangles of the grid").
variable(drops,     prolog, both, "Pending drops as d(X,Y,Radius,Depth)").
variable(rain,      bool,   both, "Drop random rain drops").
variable(damping,   num,    both, "Factor to damp the waves per 1/60 sec").
variable(timer,     timer*, get,  "Timer that drives the water").
variable(max_speed, bool,   get,  "Tick as often as possible").
variable(floor,     figure, get,  "The bottom of the pool").
variable(floor_size, size,  get,  "Part of the floor that has pebbles").
variable(pebbles,   bool,   get,  "Add pebbles where the floor grows").
variable(time,      num,    both, "Time of the last tick").
variable(dt,        num,    both, "Duration of the last step").
variable(fps,       num,    both, "Last measured frames per second").
variable(fps_time,  num,    both, "Start of the frames-per-second count").
variable(frames,    int,    both, "Ticks since fps_time").

class_variable(cell,       int, 8,   "Distance between grid points").
class_variable(refraction, num, 3.0, "Texture shift per unit of slope").

initialise(W, Size:size=size) :->
    send_super(W, initialise, 'Ripples', Size),
    send(W, background, colour(black)),
    send(W, slot, heights, []),
    send(W, slot, previous, []),
    send(W, slot, drops, []),
    send(W, slot, rain, @off),
    send(W, viscosity, 4),
    get_time(Now),
    send(W, slot, fps_time, Now),
    send(W, slot, time, Now),
    DT is 1/60.0,
    send(W, slot, dt, DT),
    send(W, slot, fps, 0),
    send(W, slot, frames, 0),
    send(W, slot, floor, new(Floor, figure)),
    send(W, slot, floor_size, size(0, 0)),
    send(W, slot, pebbles, @on),
    sign(Floor),
    send(W, append, new(Water, sdl_layer)),
    send(Water, mesh, new(sdl_mesh)),
    send(W, append, new(Glint, sdl_layer)),
    send(Glint, mesh, new(sdl_mesh)),
    send(Glint, blend, add),
    send(W, display, new(Status, text('', left, bold)), point(8, 8)),
    send(Status, name, status),
    send(Status, colour, colour(white)),
    send(W, surface),
    send(W, recogniser,
         handler(ms_left_down, message(W, press, @event))),
    send(W, recogniser,
         handler(ms_left_drag, message(W, stir, @event))),
    send(W, recogniser,
         handler(pen_axis, message(W, stir, @event))),
    send(W, recogniser,
         handler(ms_right_down, message(W, stone, @event?position))),
    send(W, slot, max_speed, @off),
    send(W, slot, timer, new(Timer, timer(0.016, message(W, tick)))),
    send(Timer, start).

unlink(W) :->
    get(W, timer, Timer),
    send(Timer, free),
    send_super(W, unlink).

resize(W) :->
    "Fit the pool to the new size"::
    send(W, surface).

surface(W) :->
    "Fit the grid and the floor to the size of the window"::
    get(W, size, size(SW, SH)),
    get(W, cell, Cell),
    Cols is (SW+Cell-1)//Cell + 1,	% cover the whole window
    Rows is (SH+Cell-1)//Cell + 1,
    get(W, heights, H0),
    get(W, previous, P0),
    fit_grid(H0, Cols, Rows, H),
    fit_grid(P0, Cols, Rows, P),
    send(W, heights, H),
    send(W, previous, P),
    grid_indices(Cols, Rows, Indices),
    send(W, indices, Indices),
    get(W, floor, Floor),
    get(W, floor_size, size(FW, FH)),
    get(W, pebbles, Pebbles),
    extend_floor(Floor, FW, FH, SW, SH, Pebbles, NW, NH),
    send(W, slot, floor_size, size(NW, NH)),
    get(Floor, member, sign, Sign),
    CX is SW//2, CY is SH//2,
    send(Sign, center, point(CX, CY)),
    send(W, paint_floor),
    send(W, show_status).

paint_floor(W) :->
    "Draw the floor into a new texture of the size of the window"::
    get(W, size, Size),
    new(Texture, sdl_texture(Size)),
    get(W, floor, Floor),
    send(Texture, draw_in, Floor),
    get(W?layers, head, Water),
    send(Water, texture, Texture).

clear_floor(W) :->
    "Remove all pebbles and stones"::
    get(W, floor, Floor),
    send(Floor?graphicals, for_all,
         if(@arg1?name == pebble, message(@arg1, destroy))),
    send(W, slot, pebbles, @off),
    send(W, paint_floor).

stone(W, At:point) :->
    "Drop a stone that stays on the floor"::
    send(W, drop, At, 3, 12),
    get(W, floor, Floor),
    get(At, x, X), get(At, y, Y),
    pebble(Floor, X, Y, Pebble),
    get(W?layers, head, Water),
    get(Water, texture, Texture),
    send(Texture, draw_in, Pebble).

max_speed(W, Max:bool) :->
    "Tick as often as possible or every 16 milliseconds"::
    send(W, slot, max_speed, Max),
    get(W, timer, Timer),
    (   Max == @on
    ->  send(Timer, interval, 0),
        send(Timer, start, once)
    ;   send(Timer, interval, 0.016),
        send(Timer, start)
    ).

press(W, Ev:event) :->
    "Drop a stone using the mouse or start stirring using a pen"::
    (   pen_event(Ev)
    ->  send(W, stir, Ev)
    ;   get(Ev, position, Pos),
        send(W, stone, Pos)
    ).

stir(W, Ev:event) :->
    "Stir the water.  The pressure of a pen sets the depth"::
    get(Ev, position, Pos),
    (   pen_event(Ev)
    ->  get(Ev, pressure, P),
        P > 0.02,			% not hovering
        Depth is 0.5 + 7.5*P,
        Radius is 1.2 + 2*P,
        send(W, drop, Pos, Radius, Depth)
    ;   send(W, drop, Pos, 2, 3)
    ).

viscosity(W, V:int) :->
    "Set the viscosity, 0..20"::
    Damping is 1 - V/1000.0,
    send(W, damping, Damping).

calm(W) :->
    "Make the water flat"::
    get(W, heights, H0),
    H0 = [Row|_],
    length(Row, Cols),
    length(H0, Rows),
    flat(Cols, Rows, H),
    send(W, heights, H),
    send(W, previous, H),
    send(W, drops, []).

drop(W, At:point, Radius:num, Depth:num) :->
    "Disturb the water at At"::
    get(W, cell, Cell),
    get(At, x, PX), get(At, y, PY),
    X is float(PX)/Cell, Y is float(PY)/Cell,
    get(W, drops, Drops),
    send(W, drops, [d(X,Y,Radius,Depth)|Drops]).

tick(W) :->
    "Move the water and rebuild the meshes"::
    rain(W),
    get(W, heights, H0),
    get(W, drops, Drops),
    send(W, drops, []),
    foldl(add_drop, Drops, H0, H1),
    get(W, previous, P),
    step_time(W, DT, Ratio),
    Speed is 0.5*(60*DT)**2,
    get(W, damping, Damping60),
    Damping is Damping60**(60*DT),
    step(H1, P, Ratio, Speed, Damping, H),
    send(W, previous, H1),
    send(W, heights, H),
    get(W, cell, Cell),
    get(W, refraction, K),
    get(W, size, size(SW, SH)),
    MaxU is SW-1, MaxV is SH-1,
    vertices(H, Cell, K, MaxU, MaxV, Water, Glints),
    get(W, indices, Indices),
    get(W?layers, head, WaterLayer),
    get(W?layers, tail, GlintLayer),
    get(WaterLayer, mesh, WaterMesh),
    get(GlintLayer, mesh, GlintMesh),
    pce_sdl_mesh_set(WaterMesh, Water, Indices),
    pce_sdl_mesh_set(GlintMesh, Glints, Indices),
    status(W),
    next_tick(W).

%!  step_time(+Window, -DT, -Ratio) is det.
%
%   DT is the time since the previous tick, at most 1/60 sec to keep the
%   wave equation stable.  Ratio is DT divided by the previous DT.

step_time(W, DT, Ratio) :-
    get_time(Now),
    get(W, time, T0),
    send(W, time, Now),
    DT is max(0.001, min(1/60.0, Now-T0)),
    get(W, dt, DT0),
    send(W, dt, DT),
    Ratio is DT/DT0.

%!  pen_event(+Event) is semidet.
%
%   True if Event comes from a pen, whose detail holds its pressure.

pen_event(Ev) :-
    get(Ev, detail, Detail),
    send(Detail, instance_of, pen_state).

%   At maximum speed, each tick starts the timer once with interval 0.
%   Its event is queued behind the events that are pending, so xpce
%   handles user input and redraws before the next tick.

next_tick(W) :-
    get(W, max_speed, @on),
    !,
    get(W, timer, Timer),
    send(Timer, start, once).
next_tick(_).

rain(W) :-				% about 18 drops per second
    get(W, rain, @on),
    get(W, dt, DT),
    random(R),
    R < 18*DT,
    !,
    get(W, size, size(SW, SH)),
    X is random(SW), Y is random(SH),
    send(W, drop, point(X, Y), 1.5, 4).
rain(_).

status(W) :-
    get(W, frames, F0),
    F is F0+1,
    get(W, fps_time, T0),
    get_time(Now),
    (   Now - T0 >= 1
    ->  FPS is F/(Now-T0),
        send(W, fps, FPS),
        send(W, show_status),
        send(W, fps_time, Now),
        send(W, frames, 0)
    ;   send(W, frames, F)
    ).

show_status(W) :->
    "Show the size, the number of triangles and the frame rate"::
    get(W, size, size(SW, SH)),
    get(W, indices, Indices),
    length(Indices, NI),
    NT is NI//3,
    get(W, fps, FPS),
    format(string(S), '~w x ~w pixels, ~D triangles, ~0f frames/second',
           [SW, SH, NT, FPS]),
    get(W, member, status, Status),
    send(Status, string, S).

:- pce_end_class(ripples_window).


		 /*******************************
		 *            WATER		*
		 *******************************/

%!  flat(+Cols, +Rows, -Heights) is det.
%
%   Heights is a flat water surface.

flat(Cols, Rows, Heights) :-
    length(Row, Cols),
    maplist(=(0.0), Row),
    length(Heights, Rows),
    maplist(=(Row), Heights).

%!  fit_grid(+Heights0, +Cols, +Rows, -Heights) is det.
%
%   Heights has the given size.  It keeps the heights of Heights0
%   that fit and adds flat water elsewhere.

fit_grid(H0, Cols, Rows, H) :-
    length(H, Rows),
    fit_rows(H, H0, Cols).

fit_rows([], _, _).
fit_rows([Row|Rows], H0, Cols) :-
    (   H0 = [Row0|T0]
    ->  true
    ;   Row0 = [], T0 = []
    ),
    length(Row, Cols),
    fit_row(Row, Row0),
    fit_rows(Rows, T0, Cols).

fit_row([], _).
fit_row([H|T], Row0) :-
    (   Row0 = [H|T0]
    ->  true
    ;   H = 0.0, T0 = []
    ),
    fit_row(T, T0).

%!  add_drop(+Drop, +Heights0, -Heights) is det.
%
%   Push the water down around a point, using a cosine shaped dent.
%   Rows outside the dent are shared with Heights0.

add_drop(d(X,Y,R,A), H0, H) :-
    add_drop_rows(H0, 0, X, Y, R, A, H).

add_drop_rows([], _, _, _, _, _, []).
add_drop_rows([Row0|Rows0], J, X, Y, R, A, [Row|Rows]) :-
    DY is J-Y,
    (   abs(DY) < R
    ->  add_drop_row(Row0, 0, X, DY, R, A, Row)
    ;   Row = Row0
    ),
    J1 is J+1,
    (   DY > R
    ->  Rows = Rows0
    ;   add_drop_rows(Rows0, J1, X, Y, R, A, Rows)
    ).

add_drop_row([], _, _, _, _, _, []).
add_drop_row([H0|T0], I, X, DY, R, A, [H|T]) :-
    D is sqrt((I-X)**2 + DY**2),
    (   D < R
    ->  H is H0 - A*(1+cos(pi*D/R))/2
    ;   H = H0
    ),
    I1 is I+1,
    add_drop_row(T0, I1, X, DY, R, A, T).

%!  step(+Heights, +Previous, +Ratio, +K, +Damping, -Next) is det.
%
%   One step of the wave equation.  The new height of a point is its
%   height plus its speed, Ratio times the change since the previous
%   step, plus K times the difference between its neighbours and the
%   point.  K is 0.5 for a step of 1/60 sec and scales with the square
%   of the step.  Points outside the grid have height 0, so the waves
%   reflect at the edges.

step(H, P, Ratio, K, Damping, N) :-
    H = [Row|_],
    flat_row(Row, Z),
    step_rows(H, P, Z, Z, Ratio, K, Damping, N).

flat_row([], []).
flat_row([_|T0], [0.0|T]) :-
    flat_row(T0, T).

step_rows([], [], _, _, _, _, _, []).
step_rows([Row|Rows], [PRow|PRows], Above, Z, Ratio, K, Damping,
          [NRow|NRows]) :-
    below(Rows, Z, Below),
    step_row(Row, Above, Below, PRow, 0.0, Ratio, K, Damping, NRow),
    step_rows(Rows, PRows, Row, Z, Ratio, K, Damping, NRows).

below([Below|_], _, Below) :- !.
below([], Z, Z).

step_row([], [], [], [], _, _, _, _, []).
step_row([C|Cs], [U|Us], [D|Ds], [P|Ps], L, Ratio, K, Damping, [N|Ns]) :-
    right(Cs, R),
    N is (C + (C-P)*Ratio + K*(L+R+U+D-4*C))*Damping,
    step_row(Cs, Us, Ds, Ps, C, Ratio, K, Damping, Ns).

right([R|_], R) :- !.
right([], 0.0).


		 /*******************************
		 *            MESHES		*
		 *******************************/

%!  vertices(+Heights, +Cell, +K, +MaxU, +MaxV, -Water, -Glints)
%
%   Compute the vertices of both meshes from the heights.  The slope at
%   a point is the difference of the heights of its neighbours.  For
%   the water, the slope shifts the position in the texture by K
%   pixels per unit and darkens or lightens the picture.  The glints
%   are white where the water is steep, and transparent elsewhere.

vertices(H, Cell, K, MaxU, MaxV, Water, Glints) :-
    H = [Row|_],
    flat_row(Row, Z),
    vertex_rows(H, Z, Z, 0, Cell, K, MaxU, MaxV, Water, Glints).

vertex_rows([], _, _, _, _, _, _, _, [], []).
vertex_rows([Row|Rows], Above, Z, J, Cell, K, MaxU, MaxV, Ws, Gs) :-
    below(Rows, Z, Below),
    Y is J*Cell,
    vertex_row(Row, Above, Below, 0.0, 0, Y, Cell, K, MaxU, MaxV,
               Ws, Ws1, Gs, Gs1),
    J1 is J+1,
    vertex_rows(Rows, Row, Z, J1, Cell, K, MaxU, MaxV, Ws1, Gs1).

vertex_row([], [], [], _, _, _, _, _, _, _, Ws, Ws, Gs, Gs).
vertex_row([C|Cs], [U|Us], [D|Ds], L, I, Y, Cell, K, MaxU, MaxV,
           [v(X,Y,TX,TY,WC)|Ws], Ws1, [v(X,Y,GC)|Gs], Gs1) :-
    right(Cs, R),
    X is I*Cell,
    SX is R-L,
    SY is D-U,
    TX is max(0, min(MaxU, X+K*SX)),
    TY is max(0, min(MaxV, Y+K*SY)),
    Light is -(SX+SY),			% light comes from the top-left
    B is max(0, min(255, round(220 + 8*Light))),
    WC is round(B*0.80)<<24 \/ round(B*0.92)<<16 \/ B<<8 \/ 0xff,
    G is max(0, min(255, round(30*(Light-1.5)))),
    GC is G<<24 \/ G<<16 \/ G<<8 \/ 0xff,
    I1 is I+1,
    vertex_row(Cs, Us, Ds, C, I1, Y, Cell, K, MaxU, MaxV, Ws, Ws1, Gs, Gs1).

%!  grid_indices(+Cols, +Rows, -Indices) is det.
%
%   Two triangles for each cell of the grid.

grid_indices(Cols, Rows, Indices) :-
    LastRow is Rows-2,
    LastCol is Cols-2,
    findall(I,
            ( between(0, LastRow, J),
              between(0, LastCol, C),
              K is J*Cols+C,
              K1 is K+1,
              KD is K+Cols,
              KD1 is KD+1,
              member(I, [K,K1,KD, K1,KD1,KD])
            ),
            Indices).


		 /*******************************
		 *           TEXTURE		*
		 *******************************/

%!  sign(+Floor) is det.
%
%   Add the sign to the floor.  It is moved to the centre of the
%   window by ->surface.

sign(Floor) :-
    send(Floor, display,
         new(T, text('SWI-Prolog', center, font(sans, bold, 64)))),
    send(T, name, sign),
    send(T, colour, colour('#f4e9c9')).

%!  extend_floor(+Floor, +FW, +FH, +W, +H, +Pebbles, -NW, -NH) is det.
%
%   Make the floor cover W x H.  The floor has pebbles on FW x FH.  If
%   the window is larger, the tiles are made anew and, if Pebbles is
%   @on, the area that is new gets pebbles.  NW x NH is the part of
%   the floor that is done.

extend_floor(_Floor, FW, FH, W, H, _, FW, FH) :-
    W =< FW, H =< FH,
    !.
extend_floor(Floor, FW, FH, W, H, Pebbles, NW, NH) :-
    NW is max(FW, W),
    NH is max(FH, H),
    send(Floor?graphicals, for_all,
         if(@arg1?name == grout, message(@arg1, destroy))),
    tiles(Floor, NW, NH),
    (   get(Floor, member, bottom, Bottom)
    ->  send(Bottom, size, size(NW, NH))
    ;   send(Floor, display, new(Bottom, box(NW, NH))),
        send(Bottom, name, bottom),
        send(Bottom, fill, colour('#2b8fb3')),
        send(Bottom, pen, 0)
    ),
    send(Bottom, hide),
    get(Floor, member, sign, Sign),
    (   Pebbles == @on
    ->  Count is round(60 * (NW*NH - FW*FH) / (800*560))
    ;   Count = 0
    ),
    forall(between(1, Count, _),
           ( new_point(FW, FH, NW, NH, X, Y),
             pebble(Floor, X, Y, Pebble),
             send(Pebble, hide, Sign)	% below the sign and stones
           )).

%!  new_point(+FW, +FH, +W, +H, -X, -Y) is det.
%
%   A random point in W x H that is not in FW x FH.

new_point(FW, FH, W, H, X, Y) :-
    repeat,
      X is random(W),
      Y is random(H),
      \+ ( X < FW, Y < FH ),
    !.

tiles(Floor, W, H) :-
    Size = 40,
    NX is W//Size,
    NY is H//Size,
    forall(between(0, NX, I),
           ( X is I*Size,
             grout(Floor, line(X, 0, X, H))
           )),
    forall(between(0, NY, I),
           ( Y is I*Size,
             grout(Floor, line(0, Y, W, Y))
           )).

grout(Floor, Line) :-
    send(Floor, display, new(L, Line)),
    send(L, name, grout),
    send(L, colour, colour('#7cc3dc')),
    send(L, pen, 2),
    send(L, hide).

%!  pebble(+Floor, +X, +Y, -Pebble) is det.
%
%   Add a pebble centred at X,Y.  A pebble is a figure holding its
%   shadow and its body.  The body is a smooth closed path through
%   points around an ellipse, moved a little at random and rotated.  A
%   radial gradient lights it from the top-left.  Some pebbles get
%   speckles.

pebble(Floor, X, Y, Pebble) :-
    RX is 5 + random_float*13,
    RY is RX*(0.55 + random_float*0.35),
    Phi is random_float*pi,
    outline(8, X, Y, RX, RY, Phi, Points),
    random_member(Name, [ '#8a8178', '#a39a8c', '#c8bca6', '#6e665e',
                          '#b4a48a', '#9b7b62', '#d9d2c5', '#5f646a' ]),
    new(Colour, colour(Name)),
    new(Pebble, figure),
    send(Pebble, name, pebble),
    shape(Points, Shadow),
    send(Shadow, fill, colour(@default, 0, 25, 40, 90)),
    send(Shadow, relative_move, point(2, 3)),
    send(Pebble, display, Shadow),
    shape(Points, Body),
    pebble_gradient(Colour, Gradient),
    send(Body, fill, Gradient),
    send(Pebble, display, Body),
    (   random(R), R < 0.4
    ->  speckles(Pebble, X, Y, RX, RY, Phi, Colour)
    ;   true
    ),
    send(Floor, display, Pebble).

outline(N, X, Y, RX, RY, Phi, Points) :-
    numlist(1, N, Is),
    maplist(outline_point(N, X, Y, RX, RY, Phi), Is, Points).

outline_point(N, X, Y, RX, RY, Phi, I, point(PX, PY)) :-
    A is I*2*pi/N,
    F is 0.9 + random_float*0.2,
    DX is RX*F*cos(A), DY is RY*F*sin(A),
    PX is round(X + DX*cos(Phi) - DY*sin(Phi)),
    PY is round(Y + DX*sin(Phi) + DY*cos(Phi)).

shape(Points, Path) :-
    new(Path, path(smooth)),
    send_list(Path, append, Points),
    send(Path, closed, @on),
    send(Path, pen, 0).

pebble_gradient(Colour, G) :-
    get(Colour, hilite, 0.5, Light),
    get(Colour, reduce, 0.45, Dark),
    new(G, gradient(radial, point(0.35, 0.3), point(0.45, 0.45),
                    0.0, 0.75, units := bounding_box)),
    send(G, add_stop, 0.0, Light),
    send(G, add_stop, 0.55, Colour),
    send(G, add_stop, 1.0, Dark).

speckles(Pebble, X, Y, RX, RY, Phi, Colour) :-
    get(Colour, reduce, 0.3, Dark),
    get(Colour, hilite, 0.3, Light),
    N is round(RX*RY/8),			% about one per 25 pixels
    forall(between(1, N, _),
           ( A is random_float*2*pi,
             F is sqrt(random_float)*0.75,	% spread evenly
             DX is RX*F*cos(A), DY is RY*F*sin(A),
             SX is round(X + DX*cos(Phi) - DY*sin(Phi)),
             SY is round(Y + DX*sin(Phi) + DY*cos(Phi)),
             random_member(C, [Dark, Light]),
             send(Pebble, display, new(D, box(1, 1)), point(SX, SY)),
             send(D, fill, C),
             send(D, pen, 0)
           )).
