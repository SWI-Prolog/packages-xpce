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


:- module(particles_demo,
          [ particles_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(library(apply)).
:- use_module(library(random)).

/** <module> Thousands of particles in one layer

A fountain of glowing particles follows the pointer; a click makes a
burst.  The slider sets the number of new particles per second.

All particles are drawn by a single sdl_layer that shows an sdl_mesh:
two triangles per particle, each mapping the same small texture of a
soft dot.  The layer uses additive blending, so particles glow where
they overlap.  On every tick, the particles move in Prolog and
pce_sdl_mesh_set/3 replaces the whole mesh in one call.  Drawing them
as a layer each would cost an xpce object per particle.

Run with:

    ?- particles_demo.
*/

particles_demo :-
    new(F, particles_frame),
    send(F, open).

:- pce_begin_class(particles_frame, frame, "Particles demo").

initialise(F) :->
    send_super(F, initialise, 'Particles'),
    send(F, append, new(W, particles_window(size(800, 600)))),
    send(new(D, dialog), below, W),
    send(D, append,
         new(S, slider(rate, 0, 10000, 2000, message(W, rate, @arg1)))),
    send(S, label, 'Particles/second'),
    send(S, width, 300),
    send(D, append, button(quit, message(F, destroy)), right).

:- pce_end_class(particles_frame).


%   The window holds the particles as a list of
%   p(X, Y, VX, VY, Age, Life) and moves them on every tick.

:- pce_begin_class(particles_window(size), sdl_window,
                   "Window with a fountain of particles").

variable(particles, prolog,   both, "List of p(X,Y,VX,VY,Age,Life)").
variable(emitter,   point,    get,  "Where new particles appear").
variable(rate,      int,      both, "New particles per second").
variable(spawn,     num,      both, "Particles due but not yet made").
variable(time,      num,      both, "Time of the last tick").
variable(timer,     timer*,   get,  "Timer that drives the particles").
variable(fps_time,  num,      both, "Start of the frames-per-second count").
variable(frames,    int,      both, "Ticks since fps_time").

class_variable(gravity, num, 300, "Downward acceleration in pixels/s^2").
class_variable(speed,   num, 250, "Maximal initial speed in pixels/s").
class_variable(life,    num, 2.0, "Maximal life time in seconds").

initialise(W, Size:size=size) :->
    send_super(W, initialise, 'Particles', Size),
    send(W, background, colour(black)),
    get(Size, width, SW), get(Size, height, SH),
    X is SW/2, Y is SH/3,
    send(W, slot, emitter, point(X, Y)),
    send(W, slot, particles, []),
    send(W, slot, rate, 2000),
    send(W, slot, spawn, 0.0),
    get_time(Now),
    send(W, slot, time, Now),
    send(W, slot, fps_time, Now),
    send(W, slot, frames, 0),
    glow_texture(T),
    send(W, append, new(L, sdl_layer(T))),
    send(L, mesh, new(sdl_mesh)),
    send(L, blend, add),
    send(W, display, new(Status, text('', left, bold)), point(8, 8)),
    send(Status, name, status),
    send(Status, colour, colour(grey80)),
    send(W, recogniser,
         handler(loc_move, message(W, slot, emitter, @event?position))),
    send(W, recogniser,
         handler(ms_left_down, message(W, burst, @event?position))),
    send(W, slot, timer, new(Timer, timer(0.016, message(W, tick)))),
    send(Timer, start).

unlink(W) :->
    get(W, timer, Timer),
    send(Timer, free),
    send_super(W, unlink).

burst(W, At:point) :->
    "Add many particles at once"::
    get(At, x, X), get(At, y, Y),
    get(W, particles, Ps0),
    new_particles(W, 500, X, Y, Ps0, Ps),
    send(W, particles, Ps).

tick(W) :->
    "Move the particles and rebuild the mesh"::
    get_time(Now),
    get(W, time, T0),
    Dt is min(0.05, Now-T0),
    send(W, time, Now),
    get(W, particles, Ps0),
    get(W, gravity, G),
    get(W?size, height, H),
    move(Ps0, Dt, G, H, Ps1),
    get(W, rate, Rate),
    get(W, spawn, Spawn0),
    Due is Spawn0 + Rate*Dt,
    N is floor(Due),
    Spawn is Due - N,
    send(W, spawn, Spawn),
    get(W, emitter, point(X, Y)),
    new_particles(W, N, X, Y, Ps1, Ps),
    send(W, particles, Ps),
    get(W?layers, head, L),
    get(L, mesh, M),
    mesh(Ps, Vertices, Indices),
    pce_sdl_mesh_set(M, Vertices, Indices),
    status(W, Ps, Now).

status(W, Ps, Now) :-
    get(W, frames, F0),
    F is F0+1,
    get(W, fps_time, T0),
    (   Now - T0 >= 1
    ->  FPS is F/(Now-T0),
        length(Ps, Count),
        format(string(S), '~D particles, ~0f frames/second', [Count, FPS]),
        get(W, member, status, Status),
        send(Status, string, S),
        send(W, fps_time, Now),
        send(W, frames, 0)
    ;   send(W, frames, F)
    ).

:- pce_end_class(particles_window).

%!  new_particles(+Window, +N, +X, +Y, +Ps0, -Ps) is det.
%
%   Add N new particles at X,Y that fly upwards in random directions.

new_particles(W, N, X, Y, Ps0, Ps) :-
    get(W, speed, Max),
    get(W, life, MaxLife),
    length(New, N),
    maplist(particle(X, Y, Max, MaxLife), New),
    append(New, Ps0, Ps).

particle(X, Y, Max, MaxLife, p(X, Y, VX, VY, 0.0, Life)) :-
    Dir is -pi/2 + (random_float-0.5)*pi*0.8,
    Speed is Max*(0.3+0.7*random_float),
    VX is cos(Dir)*Speed,
    VY is sin(Dir)*Speed,
    Life is MaxLife*(0.4+0.6*random_float).

%!  move(+Ps0, +Dt, +Gravity, +Height, -Ps) is det.
%
%   Move the particles and drop those that are too old or fell out of
%   the window.

move([], _, _, _, []).
move([p(X0,Y0,VX,VY0,Age0,Life)|T0], Dt, G, H, Ps) :-
    Age is Age0+Dt,
    Y is Y0+VY0*Dt,
    (   ( Age > Life ; Y > H+20 )
    ->  Ps = T
    ;   X is X0+VX*Dt,
        VY is VY0+G*Dt,
        Ps = [p(X,Y,VX,VY,Age,Life)|T]
    ),
    move(T0, Dt, G, H, T).

%!  mesh(+Particles, -Vertices, -Indices) is det.
%
%   Two triangles for each particle, showing the glow texture.  A
%   particle grows and turns from yellow to red as it ages, and fades
%   out.

mesh(Ps, Vertices, Indices) :-
    mesh(Ps, 0, Vertices, Indices).

mesh([], _, [], []).
mesh([p(X,Y,_,_,Age,Life)|T], I, Vs, Is) :-
    F is Age/Life,
    R is 3 + 9*F,
    G is round(230 - 170*F),
    A is round(255*(1-F)),
    C = rgba(255, G, 60, A),
    X0 is X-R, Y0 is Y-R, X1 is X+R, Y1 is Y+R,
    Vs = [ v(X0,Y0,0,0,C), v(X1,Y0,32,0,C),
           v(X1,Y1,32,32,C), v(X0,Y1,0,32,C) | VT ],
    I1 is I+1, I2 is I+2, I3 is I+3,
    Is = [I,I1,I2, I,I2,I3 | IT],
    In is I+4,
    mesh(T, In, VT, IT).

%!  glow_texture(-Texture) is det.
%
%   A white dot of 32x32 that fades out towards its edge, drawn as
%   stacked translucent circles.

:- pce_global(@particles_glow, make_glow_texture).

glow_texture(@particles_glow).

make_glow_texture(T) :-
    new(Fig, figure),
    send(Fig, display, new(B, box(32, 32))),
    send(B, pen, 0),
    forall(between(1, 8, I),
           ( D is 34 - I*4,
             send(Fig, display, new(C, circle(D))),
             send(C, pen, 0),
             send(C, fill, colour(@default, 255, 255, 255, 48)),
             send(C, center, point(16, 16))
           )),
    new(T, sdl_texture(Fig)),
    free(Fig).
