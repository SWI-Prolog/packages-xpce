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

:- module(asteroids,
          [ asteroids/0
          ]).
:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(random)).

/** <module> A game like Asteroids

Fly a ship through a field of asteroids and shoot them.  A large
asteroid breaks into two medium ones, a medium one into two small ones.
Clear the field to get to the next level, which has more asteroids.
The playing field follows the size of the window.

Controls: cursor left/right rotate, cursor up thrusts, space fires,
p pauses and Enter starts a new game.  A gamepad works as well: the
left stick rotates, the right trigger or `north` button thrusts and the
`south` button fires.

The game is driven by a timer.  On each tick the program polls the
keyboard (`keyboard<-key_down`) and the gamepad, updates the state of
the game, which is a Prolog term, and moves the graphicals.  The ship
and the asteroids are figures holding a path, which are moved and
rotated using `figure->transform`.  The sounds are generated as WAV
files when the game starts.

Run with:

    ?- asteroids.
*/

asteroids :-
    init_gamepad,
    new(F, asteroids_game),
    send(F, open).

%   Use the first gamepad that is connected when the game starts.

init_gamepad :-
    (   get(@display, gamepads, Pads),
        get(Pads, head, Pad)
    ->  true
    ;   Pad = none
    ),
    nb_setval(asteroids_gamepad, Pad).

		 /*******************************
		 *	      PARAMETERS	*
		 *******************************/

%!  field(-Width, -Height) is det.
%
%   The size of the playing field.  This follows the size of the window;
%   see resize/1.

field(W, H) :-
    (   nb_current(asteroids_field, W-H)
    ->  true
    ;   W = 800, H = 600
    ).
tick(0.02).				% seconds per tick

rotate_speed(270).			% degrees/second
thrust(350).				% pixels/second^2
max_speed(450).
friction(0.995).			% per tick
bullet_speed(550).
bullet_life(1.0).			% seconds
max_bullets(6).
fire_interval(0.18).			% seconds
ship_radius(12).
respawn_time(2.0).
safe_time(2.5).				% invulnerable after a respawn

asteroid_radius(large,  42).
asteroid_radius(medium, 24).
asteroid_radius(small,  13).

asteroid_score(large,   20).
asteroid_score(medium,  50).
asteroid_score(small,  100).

split(large,  medium).
split(medium, small).


		 /*******************************
		 *	       THE GAME		*
		 *******************************/

:- pce_begin_class(asteroids_game, frame, "A game like Asteroids").

variable(timer, timer*, get, "Timer that drives the game").

initialise(F) :->
    send_super(F, initialise, 'Asteroids'),
    nb_setval(asteroids_field, 800-600),
    field(W, H),
    send(F, append, new(P, asteroids_window(W, H))),
    send(P, name, field),
    send(F, slot, timer, timer(0.02, message(F, tick))),
    send(F, new_game),
    send(F?timer, start).

unlink(F) :->
    send(F?timer, stop),
    state_key(F, Key),
    nb_delete(Key),
    send_super(F, unlink).

new_game(F) :->
    "Start a new game"::
    get(F, member, field, P),
    send(P, clear),
    nb_setval(asteroids_burst, []),
    stars(P),
    make_hud(P),
    get_time(Now),
    State0 = game{ ship:none, asteroids:[], bullets:[], particles:[],
                   score:0, lives:3, level:0, mode:playing,
                   respawn_at:0, safe_until:0, last_fire:0,
                   time:Now, paused:false, pause_key:false
                 },
    new_level(P, State0, State1),
    spawn_ship(P, Now, State1, State),
    set_state(F, State),
    hud(P, State).

tick(F) :->
    "Advance the game one tick"::
    (   get_state(F, State0)
    ->  get(F, member, field, P),
        resize(P),
        get_time(Now),
        step(P, Now, State0, State),
        set_state(F, State)
    ;   true
    ).

:- pce_end_class(asteroids_game).

state_key(F, Key) :-
    format(atom(Key), 'asteroids_~w', [F]).

get_state(F, State) :-
    state_key(F, Key),
    nb_current(Key, State).

set_state(F, State) :-
    state_key(F, Key),
    nb_setval(Key, State).


%   The window swallows the keyboard events: the game polls the keyboard
%   and the cursor keys would otherwise scroll the window.  Enter
%   starts a new game.

:- pce_begin_class(asteroids_window, picture, "The field of the game").

initialise(P, W:int, H:int) :->
    send_super(P, initialise, 'Asteroids', size(W, H)),
    send(P, scrollbars, none),
    send(P, background, colour(black)).

event(P, Ev:event) :->
    (   send(Ev, is_a, keyboard)
    ->  (   get(Ev, id, Id),
            memberchk(Id, ['RET', 13])      % Enter
        ->  send(P?frame, new_game)
        ;   true
        )
    ;   send_super(P, event, Ev)
    ).

:- pce_end_class(asteroids_window).


		 /*******************************
		 *	      ONE STEP		*
		 *******************************/

step(P, Now, S0, S) :-
    Dt is min(0.05, Now - S0.time),
    pause(Now, S0, S1),
    (   S1.paused == true
    ->  S = S1.put(time, Now)
    ;   input(Input),
        S2 = S1.put(time, Now),
        ship_step(P, Input, Now, Dt, S2, S3),
        fire(P, Input, Now, S3, S4),
        bullets_step(P, Dt, S4, S5),
        asteroids_step(P, Dt, S5, S6),
        particles_step(P, Dt, S6, S7),
        collisions(P, Now, S7, S8),
        respawn(P, Now, S8, S9),
        next_level(P, Now, S9, S),
        hud(P, S)
    ).

%!  input(-Input) is det.
%
%   Poll the keyboard and the gamepad.  Input is a dict with the keys
%   rotate (-1..1), thrust and fire.

input(input{rotate:R, thrust:T, fire:Fire}) :-
    key(cursor_left, L), key(cursor_right, Rt),
    KR is Rt - L,
    pad_rotate(PR),
    R is max(-1, min(1, KR + PR)),
    (   ( key(cursor_up, 1) ; pad_thrust ) -> T = true ; T = false ),
    (   ( key(space, 1) ; pad_button(south) ) -> Fire = true ; Fire = false ).

key(Key, V) :-
    (   get(@keyboard_device, key_down, Key, @on) -> V = 1 ; V = 0 ).

%   The gamepad is only used if the user has one connected when the
%   game starts; see init_gamepad/0.

pad(Pad) :-
    nb_current(asteroids_gamepad, Pad),
    Pad \== none,
    get(Pad, connected, @on).

pad_rotate(R) :-
    (   pad(Pad), get(Pad, axis, left_x, X), abs(X) > 0.25
    ->  R = X
    ;   R = 0
    ).

pad_thrust :-
    pad(Pad),
    (   get(Pad, button, north, @on)
    ;   get(Pad, axis, right_trigger, T), T > 0.3
    ),
    !.

pad_button(B) :-
    pad(Pad),
    get(Pad, button, B, @on).

pause(_Now, S0, S) :-
    key(p, Down),
    (   Down == 1, S0.pause_key == false
    ->  ( S0.paused == true -> P = false ; P = true ),
        S = S0.put(_{paused:P, pause_key:true})
    ;   Down == 0
    ->  S = S0.put(pause_key, false)
    ;   S = S0
    ).


		 /*******************************
		 *	       THE SHIP		*
		 *******************************/

ship_figure(P, Fig) :-
    new(Fig, figure),
    Hull = [point(16, 0), point(-12, 10), point(-7, 0), point(-12, -10)],
    closed_path(Hull, Glow),                % a soft glow below the hull
    send(Glow, colour, colour(cyan)),
    send(Glow, pen, 7),
    send(Glow, opacity, 0.35),
    send(Fig, display, Glow),
    closed_path(Hull, Body),
    send(Body, colour, colour(white)),
    send(Body, fill, colour(midnight_blue)),
    send(Body, pen, 2),
    send(Fig, display, Body),
    new(Flame, path),
    send_list(Flame, append, [point(-9, 5), point(-20, 0), point(-9, -5)]),
    send(Flame, colour, colour(orange)),
    send(Flame, pen, 2),
    send(Flame, name, flame),
    send(Flame, displayed, @off),
    send(Fig, display, Flame),
    send(Fig, transform, new(transform)),
    send(P, display, Fig).

spawn_ship(P, Now, S0, S) :-
    field(W, H),
    ship_figure(P, Fig),
    X is W/2, Y is H/2,
    Ship = ship{x:X, y:Y, vx:0, vy:0, angle:(-90), figure:Fig},
    place(Fig, X, Y, -90),
    safe_time(Safe),
    SafeUntil is Now + Safe,
    S = S0.put(_{ship:Ship, safe_until:SafeUntil, mode:playing}).

ship_step(_, _, _, _, S, S) :-
    S.ship == none,
    !.
ship_step(_P, In, Now, Dt, S0, S) :-
    Ship0 = S0.ship,
    rotate_speed(RS), thrust(Th), max_speed(Max), friction(Fr),
    Angle is Ship0.angle + In.rotate*RS*Dt,
    Rad is Angle*pi/180,
    (   In.thrust == true
    ->  VX1 is Ship0.vx + cos(Rad)*Th*Dt,
        VY1 is Ship0.vy + sin(Rad)*Th*Dt,
        Flame = @on
    ;   VX1 = Ship0.vx, VY1 = Ship0.vy,
        Flame = @off
    ),
    Speed is sqrt(VX1*VX1+VY1*VY1),
    (   Speed > Max
    ->  VX2 is VX1*Max/Speed, VY2 is VY1*Max/Speed
    ;   VX2 = VX1, VY2 = VY1
    ),
    VX is VX2*Fr, VY is VY2*Fr,
    wrap(Ship0.x + VX*Dt, Ship0.y + VY*Dt, X, Y),
    Fig = Ship0.figure,
    get(Fig, member, flame, FlameGr),
    send(FlameGr, displayed, Flame),
    (   Flame == @on                     % flicker
    ->  random_member(FC, [orange, yellow, orange_red]),
        send(FlameGr, colour, colour(FC))
    ;   true
    ),
    (   Now < S0.safe_until             % blink while invulnerable
    ->  Blink is truncate(Now*8) mod 2,
        ( Blink == 0 -> send(Fig, displayed, @on)
        ; send(Fig, displayed, @off)
        )
    ;   send(Fig, displayed, @on)
    ),
    place(Fig, X, Y, Angle),
    S = S0.put(ship, Ship0.put(_{x:X, y:Y, vx:VX, vy:VY, angle:Angle})).


		 /*******************************
		 *	      BULLETS		*
		 *******************************/

fire(P, In, Now, S0, S) :-
    In.fire == true,
    Ship = S0.ship,
    Ship \== none,
    fire_interval(Int),
    Now - S0.last_fire >= Int,
    length(S0.bullets, NB),
    max_bullets(Max),
    NB < Max,
    !,
    bullet_speed(BS), bullet_life(Life),
    Rad is Ship.angle*pi/180,
    X is Ship.x + cos(Rad)*16, Y is Ship.y + sin(Rad)*16,
    VX is Ship.vx + cos(Rad)*BS, VY is Ship.vy + sin(Rad)*BS,
    new(C, circle(4)),
    send(C, fill, colour(white)),
    send(C, colour, colour(cyan)),
    send(P, display, C),
    move_circle(C, X, Y),
    Until is Now + Life,
    B = bullet{x:X, y:Y, vx:VX, vy:VY, until:Until, figure:C},
    play(fire),
    S = S0.put(_{bullets:[B|S0.bullets], last_fire:Now}).
fire(_, _, _, S, S).

bullets_step(_P, Dt, S0, S) :-
    Now = S0.time,
    foldl(bullet_step(Dt, Now), S0.bullets, [], Bullets),
    S = S0.put(bullets, Bullets).

bullet_step(_Dt, Now, B, Bs, Bs) :-
    Now > B.until,
    !,
    send(B.figure, destroy).
bullet_step(Dt, _Now, B0, Bs, [B|Bs]) :-
    wrap(B0.x + B0.vx*Dt, B0.y + B0.vy*Dt, X, Y),
    move_circle(B0.figure, X, Y),
    B = B0.put(_{x:X, y:Y}).

move_circle(C, X, Y) :-
    CX is round(X)-2, CY is round(Y)-2,
    send(C, set, CX, CY).


		 /*******************************
		 *	     ASTEROIDS		*
		 *******************************/

new_level(P, S0, S) :-
    Level is S0.level + 1,
    N is 3 + Level,
    numlist(1, N, Is),
    maplist(new_asteroid(P), Is, Asteroids),
    S = S0.put(_{level:Level, asteroids:Asteroids}).

%   A new large asteroid at the edge of the field, away from the ship.

new_asteroid(P, _, A) :-
    field(W, H),
    (   maybe
    ->  X is random_float*W, Y = 0
    ;   X = 0, Y is random_float*H
    ),
    random_velocity(large, VX, VY),
    make_asteroid(P, large, X, Y, VX, VY, A).

random_velocity(Size, VX, VY) :-
    (   Size == large -> Min = 30, Max = 70
    ;   Size == medium -> Min = 50, Max = 100
    ;   Min = 70, Max = 140
    ),
    Speed is Min + random_float*(Max-Min),
    Dir is random_float*2*pi,
    VX is cos(Dir)*Speed, VY is sin(Dir)*Speed.

make_asteroid(P, Size, X, Y, VX, VY, A) :-
    asteroid_radius(Size, R),
    new(Fig, figure),
    N = 11,
    findall(point(PX, PY),
            ( between(1, N, I),
              Ang is 2*pi*I/N,
              Rr is R*(0.75 + random_float*0.4),
              PX is round(cos(Ang)*Rr), PY is round(sin(Ang)*Rr)
            ), Outline),
    rock_colours(Light, Dark, Edge),
    closed_path(Outline, Glow),             % soft glow around the rock
    send(Glow, colour, colour(Edge)),
    send(Glow, pen, 8),
    send(Glow, opacity, 0.3),
    send(Fig, display, Glow),
    closed_path(Outline, Rock),             % the shaded rock
    new(Shade, gradient(radial, point(0.3, 0.3), point(0.5, 0.5),
                        0, 0.6, units := bounding_box)),
    send(Shade, add_stop, 0, colour(Light)),
    send(Shade, add_stop, 1, colour(Dark)),
    send(Rock, fill, Shade),
    send(Rock, colour, colour(Edge)),
    send(Rock, pen, 2),
    send(Fig, display, Rock),
    craters(Fig, R, Dark),
    send(Fig, transform, new(transform)),
    send(P, display, Fig),
    Spin is (random_float-0.5)*120,
    Angle is random_float*360,
    place(Fig, X, Y, Angle),
    A = asteroid{size:Size, r:R, x:X, y:Y, vx:VX, vy:VY,
                 angle:Angle, spin:Spin, figure:Fig}.

asteroids_step(_P, Dt, S0, S) :-
    maplist(asteroid_step(Dt), S0.asteroids, As),
    S = S0.put(asteroids, As).

asteroid_step(Dt, A0, A) :-
    wrap(A0.x + A0.vx*Dt, A0.y + A0.vy*Dt, X, Y),
    Angle is A0.angle + A0.spin*Dt,
    place(A0.figure, X, Y, Angle),
    A = A0.put(_{x:X, y:Y, angle:Angle}).


		 /*******************************
		 *	     COLLISIONS		*
		 *******************************/

collisions(P, Now, S0, S) :-
    bullet_hits(P, S0.bullets, S0.asteroids, Bullets, Asteroids, 0, Points),
    Score is S0.score + Points,
    S1 = S0.put(_{bullets:Bullets, asteroids:Asteroids, score:Score}),
    ship_hit(P, Now, S1, S).

%   Each bullet destroys at most one asteroid.

bullet_hits(_, [], As, [], As, Points, Points).
bullet_hits(P, [B|Bs0], As0, Bs, As, P0, Points) :-
    (   select(A, As0, As1),
        hit(B.x, B.y, 2, A)
    ->  send(B.figure, destroy),
        explode_asteroid(P, A, Pieces, Score),
        append(Pieces, As1, As2),
        P1 is P0 + Score,
        bullet_hits(P, Bs0, As2, Bs, As, P1, Points)
    ;   Bs = [B|Bs1],
        bullet_hits(P, Bs0, As0, Bs1, As, P0, Points)
    ).

hit(X, Y, R, A) :-
    DX is X - A.x, DY is Y - A.y,
    D is A.r + R,
    DX*DX + DY*DY < D*D.

explode_asteroid(P, A, Pieces, Score) :-
    send(A.figure, destroy),
    asteroid_score(A.size, Score),
    burst(P, A.x, A.y, 10),
    (   split(A.size, Smaller)
    ->  play(bang),
        findall(Piece,
                ( between(1, 2, _),
                  random_velocity(Smaller, VX, VY),
                  make_asteroid(P, Smaller, A.x, A.y, VX, VY, Piece)
                ), Pieces)
    ;   play(bang_small),
        Pieces = []
    ).

ship_hit(P, Now, S0, S) :-
    Ship = S0.ship,
    Ship \== none,
    Now >= S0.safe_until,
    ship_radius(R),
    member(A, S0.asteroids),
    hit(Ship.x, Ship.y, R, A),
    !,
    send(Ship.figure, destroy),
    burst(P, Ship.x, Ship.y, 30),
    play(explosion),
    Lives is S0.lives - 1,
    respawn_time(RT),
    At is Now + RT,
    (   Lives > 0 -> Mode = respawn ; Mode = game_over ),
    S = S0.put(_{ship:none, lives:Lives, respawn_at:At, mode:Mode}).
ship_hit(_, _, S, S).

respawn(P, Now, S0, S) :-
    S0.mode == respawn,
    Now >= S0.respawn_at,
    !,
    spawn_ship(P, Now, S0, S).
respawn(_, _, S, S).

next_level(P, Now, S0, S) :-
    S0.asteroids == [],
    S0.mode \== game_over,
    !,
    new_level(P, S0, S1),
    safe_time(Safe),
    SafeUntil is Now + Safe,
    S = S1.put(safe_until, SafeUntil).
next_level(_, _, S, S).


		 /*******************************
		 *	     PARTICLES		*
		 *******************************/

burst(P, X, Y, N) :-
    get_time(Now),
    (   nb_current(asteroids_burst, Bursts0)
    ->  true
    ;   Bursts0 = []
    ),
    findall(Part,
            ( between(1, N, _),
              Dir is random_float*2*pi,
              Speed is 40 + random_float*160,
              VX is cos(Dir)*Speed, VY is sin(Dir)*Speed,
              Until is Now + 0.4 + random_float*0.6,
              random_member(Colour, [white, yellow, gold, orange,
                                     orange_red, red]),
              new(C, circle(3)),
              send(C, fill, colour(Colour)),
              send(C, colour, colour(Colour)),
              send(P, display, C),
              move_circle(C, X, Y),
              Part = particle{x:X, y:Y, vx:VX, vy:VY, born:Now,
                              until:Until, figure:C}
            ), New),
    append(New, Bursts0, Bursts),
    nb_setval(asteroids_burst, Bursts).

%   New particles are collected in a global variable by burst/4, as
%   they are created deep inside the collision handling.

particles_step(_P, Dt, S0, S) :-
    (   nb_current(asteroids_burst, New) -> true ; New = [] ),
    nb_setval(asteroids_burst, []),
    append(New, S0.particles, Parts0),
    Now = S0.time,
    foldl(particle_step(Dt, Now), Parts0, [], Parts),
    S = S0.put(particles, Parts).

%   Particles fade out during their life.

particle_step(_Dt, Now, Pt, Ps, Ps) :-
    Now > Pt.until,
    !,
    send(Pt.figure, destroy).
particle_step(Dt, Now, Pt0, Ps, [Pt|Ps]) :-
    wrap(Pt0.x + Pt0.vx*Dt, Pt0.y + Pt0.vy*Dt, X, Y),
    move_circle(Pt0.figure, X, Y),
    Opacity is max(0, (Pt0.until-Now)/(Pt0.until-Pt0.born)),
    send(Pt0.figure, opacity, Opacity),
    Pt = Pt0.put(_{x:X, y:Y}).


		 /*******************************
		 *	      DRAWING		*
		 *******************************/

closed_path(Points, Path) :-
    new(Path, path),
    send_list(Path, append, Points),
    send(Path, closed, @on).

%!  rock_colours(-Light, -Dark, -Edge) is det.
%
%   A colour scheme for an asteroid: the lit and the dark side of the
%   rock and its glowing edge.

rock_colours(Light, Dark, Edge) :-
    random_member(Light-Dark-Edge,
                  [ burlywood-saddle_brown-orange,
                    light_steel_blue-dark_slate_blue-deep_sky_blue,
                    plum-dark_violet-violet,
                    light_salmon-firebrick-orange_red,
                    pale_green-dark_olive_green-lime_green
                  ]).

%   A few craters: darker, translucent ellipses on the rock.

craters(Fig, R, Dark) :-
    N is 2 + random(2),
    forall(between(1, N, _),
           ( D is max(4, round(R*(0.2 + random_float*0.2))),
             Ang is random_float*2*pi,
             Dist is R*0.35*random_float,
             CX is round(cos(Ang)*Dist - D/2),
             CY is round(sin(Ang)*Dist - D/2),
             DH is round(D*0.8),
             new(C, ellipse(D, DH)),
             send(C, fill, colour(Dark)),
             send(C, colour, colour(black)),
             send(C, opacity, 0.6),
             send(Fig, display, C, point(CX, CY))
           )).

%!  resize(+Picture) is det.
%
%   If the user resized the window, make the playing field the size of
%   the window.  The starfield is made anew for the new size.  Objects
%   outside a smaller field come back through the edges (see wrap/4).

resize(P) :-
    get(P, visible, Area),
    get(Area, width, W),
    get(Area, height, H),
    field(W0, H0),
    (   W-H == W0-H0
    ;   W < 100
    ;   H < 100
    ),
    !.
resize(P) :-
    get(P, visible, Area),
    get(Area, width, W),
    get(Area, height, H),
    nb_setval(asteroids_field, W-H),
    get(P?graphicals, find_all, @arg1?name == star, Old),
    send(Old, for_all, message(@arg1, destroy)),
    stars(P),
    get(P, member, message, Msg),
    CX is W//2, CY is H//2-60,
    send(Msg, center, point(CX, CY)).

%   A static starfield.  The stars are put below everything.  The number
%   of stars depends on the size of the field.

stars(P) :-
    field(W, H),
    N is W*H//5000,
    forall(between(1, N, _),
           ( X is random(W), Y is random(H),
             random_member(Colour, [grey40, grey60, grey80, white,
                                    light_sky_blue]),
             random_member(Size, [1, 1, 2, 2, 3]),
             new(Star, box(Size, Size)),
             send(Star, name, star),
             send(Star, pen, 0),
             send(Star, fill, colour(Colour)),
             send(P, display, Star, point(X, Y)),
             send(Star, hide)
           )).

%!  place(+Figure, +X, +Y, +Angle) is det.
%
%   Move the figure to X,Y and rotate it by Angle degrees.  We give the
%   figure a new transform rather than changing its transform, so the
%   figure knows the area it occupied, which must be repainted.

place(Fig, X, Y, Angle) :-
    Rad is Angle*pi/180,
    C is cos(Rad), S is sin(Rad), NS is -S,
    new(T, transform),
    send(T, set, C, NS, S, C, X, Y),
    send(Fig, transform, T).

wrap(X0, Y0, X, Y) :-
    field(W, H),
    X is X0 - W*floor(X0/W),
    Y is Y0 - H*floor(Y0/H).

make_hud(P) :-
    send(P, display, new(Score, text('', left, font(mono, bold, 16))), point(10, 8)),
    send(Score, name, score),
    send(Score, colour, colour(white)),
    send(P, display, new(Msg, text('', center, font(sans, bold, 28)))),
    send(Msg, name, message),
    send(Msg, colour, colour(white)).

hud(P, S) :-
    format(atom(Score), 'Score ~w   Lives ~w   Level ~w',
           [S.score, S.lives, S.level]),
    ignore(set_text(P, score, Score)),
    (   S.mode == game_over
    ->  Msg = 'GAME OVER\nPress Enter'
    ;   S.paused == true
    ->  Msg = 'PAUSED'
    ;   Msg = ''
    ),
    (   set_text(P, message, Msg)
    ->  field(W, H),
        CX is W//2, CY is H//2-60,
        get(P, member, message, MsgText),
        send(MsgText, center, point(CX, CY))
    ;   true
    ).

%!  set_text(+Picture, +Name, +Text) is semidet.
%
%   Change the text Name if it differs from Text.  Fails if the text
%   did not change, avoiding a repaint on each tick.

set_text(P, Name, Text) :-
    get(P, member, Name, T),
    get(T?string, value, Old),
    Old \== Text,
    send(T, string, Text).


		 /*******************************
		 *	       SOUNDS		*
		 *******************************/

%!  play(+Effect) is det.
%
%   Play a sound effect.  The sounds are generated as WAV files the
%   first time they are used.  Without audio, the game is silent.

play(Effect) :-
    (   catch(effect_sound(Effect, Sound), _, fail)
    ->  ignore(send(Sound, play, 0.4))
    ;   true
    ).

:- dynamic sound_cache/2.

effect_sound(Effect, Sound) :-
    sound_cache(Effect, Sound),
    !.
effect_sound(Effect, Sound) :-
    effect(Effect, Seconds, Generator),
    tmp_file_stream(binary, File, Out),
    write_wav(Out, Seconds, Generator),
    close(Out),
    new(Sound, sound(File)),
    send(Sound, lock_object, @on),
    delete_file(File),
    assertz(sound_cache(Effect, Sound)).

effect(fire,       0.12, chirp(1400, 500)).
effect(bang,       0.35, noise(0.6)).
effect(bang_small, 0.20, noise(0.4)).
effect(explosion,  0.9,  noise(0.9)).

rate(22050).

write_wav(Out, Seconds, Gen) :-
    rate(Rate),
    N is round(Rate*Seconds),
    DataLen is N*2,
    format(Out, "RIFF", []), le(Out, 36+DataLen, 4),
    format(Out, "WAVEfmt ", []), le(Out, 16, 4),
    le(Out, 1, 2), le(Out, 1, 2), le(Out, Rate, 4),
    le(Out, Rate*2, 4), le(Out, 2, 2), le(Out, 16, 2),
    format(Out, "data", []), le(Out, DataLen, 4),
    forall(between(0, N, I),
           ( sample(Gen, I, N, Rate, V),
             S is max(-32767, min(32767, round(V*32767))),
             le(Out, S, 2)
           )).

%   A tone sweeping from F0 to F1 and noise, both fading out.

sample(chirp(F0, F1), I, N, Rate, V) :-
    T is I/Rate,
    F is F0 + (F1-F0)*I/N,
    V is 0.5*sin(2*pi*F*T)*(1-I/N).
sample(noise(Level), I, N, _, V) :-
    Fade is (1-I/N)**2,
    V is Level*(random_float*2-1)*Fade.

le(Out, V0, Bytes) :-
    V is V0,
    Max is Bytes-1,
    forall(between(0, Max, I),
           ( B is (V >> (8*I)) /\ 0xff,
             put_byte(Out, B)
           )).
