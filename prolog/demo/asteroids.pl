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

Everything on the field is a graphical object that holds its own state:
the ship, the asteroids, the bullets, the particles of an explosion, the
stars and the texts of the scoreboard.  The objects that move keep their
position and velocity as floats, as rounding to pixels on every tick
would make slow objects stick.  The window holds the state of the game
as a whole: the score, the lives, the level and the duration of the
current tick.  The parameters of the game are class variables of the
classes they belong to, so they can be changed in the user's xpce
defaults file.  A class variable with the same name as a variable
provides the initial value of the variable.

The game is driven by a timer.  On each tick the window sends `tick` to
all its graphicals:

    send(Window?graphicals, for_all, message(@arg1, tick))

Each object then moves, checks whether it hits something and destroys
itself when its time is up.  `chain->for_all` works on a copy of the
chain and skips objects that are destroyed during the iteration, so a
tick may freely add and remove graphicals.  The ship gets its controls
from `window<-input`, which polls the keyboard (`keyboard<-key_down`)
and the gamepad.  The ship and the asteroids are figures holding a
path, which are moved and rotated using `figure->transform`.  The
sounds are computed as samples that are played using an audio_stream.

Run with:

    ?- asteroids.
*/

asteroids :-
    new(F, asteroids_game),
    send(F, open).

		 /*******************************
		 *	       THE GAME		*
		 *******************************/

:- pce_begin_class(asteroids_game, frame, "A game like Asteroids").

initialise(F) :->
    send_super(F, initialise, 'Asteroids'),
    send(F, append, new(P, asteroids_window(size(800, 600)))),
    send(P, new_game).

:- pce_end_class(asteroids_game).


%   The window holds the state of the game as a whole and drives the
%   game using a timer.  A key binding handles p (pause) and Enter (new
%   game).  The controls of the ship are polled on each tick.

:- pce_begin_class(asteroids_window(size), window,
                   "The field of the game").

variable(timer,        timer*, get, "Timer that drives the game").
variable(score,        int,    get, "Score of the player").
variable(lives,        int,    get, "Ships left").
variable(level,        int,    get, "Current level").
variable(mode,  {playing,respawn,game_over}, get, "State of the game").
variable(respawn,      num,    get, "Seconds until the next ship appears").
variable(level_break,  num,    get, "Seconds left of the break after a level").
variable(paused,       bool,   get, "Game is paused").
variable(time,         num,    get, "Time of the last tick").
variable(dt,           num,    get, "Seconds since the last tick").
variable(gamepad,      any*,   get, "Gamepad used to play").

class_variable(tick,           num, 0.02, "Seconds per tick").
class_variable(lives,          int, 3,    "Ships at the start of a game").
class_variable(respawn,        num, 2.0,  "Seconds until a new ship appears").
class_variable(level_break,    num, 2.0,  "Seconds between two levels").
class_variable(max_dt,         num, 0.05, "Longest tick in seconds").
class_variable(base_asteroids, int, 3,    "Asteroids are this + level").
class_variable(star_area,      int, 5000, "Square pixels per star").
class_variable(volume,         '0.0..1.0', 0.4, "Volume of the sound effects").

initialise(P, Size:size=size) :->
    send_super(P, initialise, 'Asteroids', Size),
    send(P, background, colour(black)),
    send(P, slot, paused, @off),
    get_time(Now),
    send(P, slot, time, Now),
    send(P, slot, dt, 0.0),
    (   get(@display, gamepads, Pads),  % the first connected gamepad
        get(Pads, head, Pad)
    ->  send(P, slot, gamepad, Pad)
    ;   true
    ),
    send(P, recogniser, new(KB, key_binding)),
    send(KB, function,  p,    message(@receiver, toggle_paused)),
    send(KB, function, 'RET', message(@receiver, new_game)).

unlink(P) :->
    "Stop the timer"::
    send(P?timer, stop),
    send_super(P, unlink).

input(P, In:prolog) :<-
    "Get action from the keyboard or gamepad"::
    (   get(P, input_focus, @on)
    ->  get(P, rotate, R),
        get(P, thrust, T),
        get(P, fire, Fire),
        In = #{rotate:R, thrust:T, fire:Fire}
    ;   In = #{rotate:0, thrust:false, fire:false}
    ).

rotate(P, R:num) :<-
    "Get desired ship rotation"::
    key(cursor_left, L), key(cursor_right, Rt),
    KR is Rt - L,
    pad_rotate(P, PR),
    R is max(-1, min(1, KR + PR)).

pad_rotate(P, R) :-
    (   pad(P, Pad),
        get(Pad, axis, left_x, X),
        abs(X) > 0.25
    ->  R = X
    ;   R = 0
    ).

thrust(P, Thrust:{true,false}) :<-
    "Should the ship accelerate?"::
    (   key_down(cursor_up)
    ->  Thrust = true
    ;   pad_thrust(P)
    ->  Thrust = true
    ;   Thrust = false
    ).

pad_thrust(P) :-
    pad(P, Pad),
    (   get(Pad, button, north, @on)
    ->  true
    ;   get(Pad, axis, right_trigger, T),
        T > 0.3
    ).

fire(P, Fire:{true,false}) :<-
    "Should the ship fire?"::
    (   key_down(space)
    ->  Fire = true
    ;   pad_button(P, south)
    ->  Fire = true
    ;   Fire = false
    ).

pad_button(P, B) :-
    pad(P, Pad),
    get(Pad, button, B, @on).

pad(P, Pad) :-
    get(P, gamepad, Pad),
    Pad \== @nil,
    get(Pad, connected, @on).

new_game(P) :->
    "Start a new game"::
    send(P, start_ticking),
    send(P, clear),
    send(P, slot, score, 0),
    get(P, class_variable_value, lives, Lives),
    send(P, slot, lives, Lives),
    send(P, slot, level, 0),
    send(P, slot, respawn, 0.0),
    send(P, slot, level_break, 0.0),
    send(P, slot, paused, @off),
    send(P, stars),
    send(P, display, new(asteroids_score), point(10, 8)),
    send(P, display, new(asteroids_banner)),
    send(P, new_level),
    send(P, spawn_ship).

start_ticking(P) :->
    "Start the game ticker"::
    (   get(P, timer, @nil)
    ->  get(P, tick, Tick),
        send(P, slot, timer,
             new(Timer, timer(Tick, message(P, tick)))),
        send(Timer, start)
    ;   true
    ).

tick(P) :->
    "Advance the game one tick"::
    get_time(Now),
    get(P, time, T0),
    get(P, max_dt, MaxDt),
    Dt is min(MaxDt, Now - T0),
    send(P, slot, time, Now),
    send(P, slot, dt, Dt),
    (   get(P, paused, @on)
    ->  get(P, member, banner, Banner),
        send(Banner, tick)
    ;   send(P?graphicals, for_all, message(@arg1, tick)),
        respawn(P, Dt),
        next_level(P)
    ).

new_level(P) :->
    "Start the next level with more asteroids"::
    get(P, level, Level0),
    Level is Level0 + 1,
    send(P, slot, level, Level),
    get(P, base_asteroids, Base),
    N is Base + Level,
    forall(between(1, N, _), new_asteroid(P)).

spawn_ship(P) :->
    "Put a new ship in the middle of the field"::
    get(P, width, W),
    get(P, height, H),
    X is W/2, Y is H/2,
    send(P, display, asteroids_ship(X, Y)),
    send(P, slot, mode, playing).

ship_lost(P) :->
    "The ship was destroyed"::
    get(P, lives, Lives0),
    Lives is Lives0 - 1,
    send(P, slot, lives, Lives),
    get(P, class_variable_value, respawn, RT),
    send(P, slot, respawn, RT),
    (   Lives > 0
    ->  send(P, slot, mode, respawn)
    ;   send(P, slot, mode, game_over)
    ).

add_score(P, Points:int) :->
    "Add Points to the score"::
    get(P, score, Score0),
    Score is Score0 + Points,
    send(P, slot, score, Score).

toggle_paused(P) :->
    "Pause the game on 'p'"::
    send(P, slot, paused, P?paused?negate).

respawn(P, Dt) :-
    get(P, mode, respawn),
    !,
    get(P, respawn, Left0),
    Left is Left0 - Dt,
    (   Left =< 0
    ->  send(P, spawn_ship)
    ;   send(P, slot, respawn, Left)
    ).
respawn(_, _).

%   The field is clear.  After a break of <-level_break seconds, during
%   which the banner congratulates the player, the next level starts.

next_level(P) :-
    \+ get(P, mode, game_over),
    \+ get(P?graphicals, find, message(@arg1, instance_of, asteroid), _),
    !,
    get(P, level_break, Left0),
    (   Left0 > 0                       % in the break
    ->  get(P, dt, Dt),
        Left is Left0 - Dt,
        (   Left > 0
        ->  send(P, slot, level_break, Left)
        ;   send(P, slot, level_break, 0.0),
            start_level(P)
        )
    ;   get(P, class_variable_value, level_break, Break),
        (   Break > 0
        ->  send(P, slot, level_break, Break)
        ;   start_level(P)
        )
    ).
next_level(_).

%   The new asteroids appear at the edge, so the ship gets a little time
%   to get out of their way.

start_level(P) :-
    send(P, new_level),
    (   get(P, member, ship, Ship)
    ->  get(Ship, class_variable_value, safe, Safe),
        send(Ship, slot, safe, Safe)
    ;   true
    ).

%   A new large asteroid at the edge of the field, away from the ship.

new_asteroid(P) :-
    get(P, width, W),
    get(P, height, H),
    (   maybe
    ->  X is random_float*W, Y = 0
    ;   X = 0, Y is random_float*H
    ),
    send(P, display, asteroid(large, X, Y)).

%!  ->resize
%
%   The user resized the window.  The starfield is made anew for the
%   new size.  Objects outside a smaller field come back through the
%   edges (see wrap/5).

resize(P) :->
    "Make a new starfield for the new size"::
    send(P?graphicals, for_all,
         if(message(@arg1, instance_of, asteroids_star),
            message(@arg1, destroy))),
    send(P, stars).

%!  ->stars
%
%   The stars are put below everything. The   number of stars depends on
%   the size of the field.

stars(P) :->
    "Add random stars below everything"::
    get(P, size, size(W,H)),
    get(P, star_area, Area),
    N is W*H//Area,
    forall(between(1, N, _),
           ( X is random(W), Y is random(H),
             random_member(Size, [1, 1, 2, 2, 3]),
             send(P, display, new(Star, asteroids_star(Size)), point(X, Y)),
             send(Star, hide)
           )).

%   Each play uses a new audio_stream that gets the samples of the
%   effect and is freed after it is played.  The samples are computed
%   the first time an effect is used.  Without an audio device, the
%   game is silent.

play(P, Effect:{fire,bang,bang_small,explosion}) :->
    "Play a sound effect"::
    (   get(@display_manager, audio_output, @on)
    ->  rate(Rate),
        effect_samples(Effect, Samples),
        get(P, volume, Volume),
        new(AS, audio_stream(s16, Rate, 1)),
        send(AS, volume, Volume),
        (   send(AS, append, Samples),
            send(AS, close),
            send(AS, play)
        ->  true
        ;   free(AS)
        )
    ;   true
    ).

effect(fire,       0.12, chirp(1400, 500)).
effect(bang,       0.35, noise(0.6)).
effect(bang_small, 0.20, noise(0.4)).
effect(explosion,  0.9,  noise(0.9)).

rate(22050).

%!  effect_samples(+Effect, -Samples:string) is det.
%
%   Samples are the 16-bit mono samples of Effect as a string of bytes.
%   audio_stream expects native byte order; we assume little endian.

:- table effect_samples/2.

effect_samples(Effect, Samples) :-
    effect(Effect, Seconds, Gen),
    rate(Rate),
    N is round(Rate*Seconds),
    findall(B,
            ( between(0, N, I),
              sample(Gen, I, N, Rate, V),
              S is max(-32767, min(32767, round(V*32767))) /\ 0xffff,
              (   B is S /\ 0xff
              ;   B is S >> 8
              )
            ), Bytes),
    string_codes(Samples, Bytes).

%   A tone sweeping from F0 to F1 and noise, both fading out.

sample(chirp(F0, F1), I, N, Rate, V) :-
    T is I/Rate,
    F is F0 + (F1-F0)*I/N,
    V is 0.5*sin(2*pi*F*T)*(1-I/N).
sample(noise(Level), I, N, _, V) :-
    Fade is (1-I/N)**2,
    V is Level*(random_float*2-1)*Fade.

:- pce_end_class(asteroids_window).


		 /*******************************
		 *	       THE SHIP		*
		 *******************************/

:- pce_begin_class(asteroids_ship(px, py), figure, "The ship of the player").

variable(px,     num, get, "X of the centre").
variable(py,     num, get, "Y of the centre").
variable(vx,     num, get, "Horizontal velocity").
variable(vy,     num, get, "Vertical velocity").
variable(angle,  num, get, "Heading in degrees").
variable(safe,   num, get, "Seconds the ship remains invulnerable").
variable(reload, num, get, "Seconds until the ship can fire again").

class_variable(safe,            num, 2.5,   "Seconds safe after a respawn").
class_variable(reload,          num, 0.18,  "Seconds between two shots").
class_variable(rotate_speed,    num, 270,   "Degrees/second").
class_variable(thrust,          num, 350,   "Pixels/second^2").
class_variable(max_speed,       num, 450,   "Pixels/second").
class_variable(friction,        num, 0.995, "Speed kept per tick").
class_variable(bullet_speed,    num, 550,   "Pixels/second, relative to ship").
class_variable(max_bullets,     int, 6,     "Bullets that can be in flight").
class_variable(hit_radius,      num, 12,    "Radius for collisions").
class_variable(burst_particles, int, 30,    "Particles of an explosion").

initialise(S, X:num, Y:num) :->
    send_super(S, initialise),
    send(S, name, ship),
    send(S, slot, px, X),
    send(S, slot, py, Y),
    set_velocity(S, 0.0, 0.0),
    send(S, slot, angle, -90.0),
    send(S, slot, reload, 0.0),
    Hull = [point(16, 0), point(-12, 10), point(-7, 0), point(-12, -10)],
    closed_path(Hull, Glow),                % a soft glow below the hull
    send(Glow, colour, colour(cyan)),
    send(Glow, pen, 7),
    send(Glow, opacity, 0.35),
    send(S, display, Glow),
    closed_path(Hull, Body),
    send(Body, colour, colour(white)),
    send(Body, fill, colour(midnight_blue)),
    send(Body, pen, 2),
    send(S, display, Body),
    new(Flame, path),
    send_list(Flame, append, [point(-9, 5), point(-20, 0), point(-9, -5)]),
    send(Flame, colour, colour(orange)),
    send(Flame, pen, 2),
    send(Flame, name, flame),
    send(Flame, displayed, @off),
    send(S, display, Flame),
    place(S, X, Y, -90).

tick(S) :->
    "Steer, move, fire and check for a collision"::
    get(S, device, P),
    get(P, dt, Dt),
    get(P, input, In),
    get(S, rotate_speed, RS),
    get(S, thrust, Th),
    get(S, max_speed, Max),
    get(S, friction, Fr),
    get(S, angle, Angle0),
    Angle is Angle0 + In.rotate*RS*Dt,
    send(S, slot, angle, Angle),
    get(S, vx, VX0), get(S, vy, VY0),
    (   In.thrust == true
    ->  Rad is Angle*pi/180,
        VX1 is VX0 + cos(Rad)*Th*Dt,
        VY1 is VY0 + sin(Rad)*Th*Dt
    ;   VX1 = VX0, VY1 = VY0
    ),
    Speed is sqrt(VX1*VX1+VY1*VY1),
    (   Speed > Max
    ->  VX2 is VX1*Max/Speed, VY2 is VY1*Max/Speed
    ;   VX2 = VX1, VY2 = VY1
    ),
    VX is VX2*Fr, VY is VY2*Fr,
    set_velocity(S, VX, VY),
    move(S, X, Y, Dt),
    place(S, X, Y, Angle),
    flame(S, In.thrust),
    blink(S, Dt),
    fire(S, In.fire, Dt),
    collide(S).

flame(S, Thrust) :-
    get(S, member, flame, Flame),
    (   Thrust == true
    ->  send(Flame, displayed, @on),
        random_member(FC, [orange, yellow, orange_red]),    % flicker
        send(Flame, colour, colour(FC))
    ;   send(Flame, displayed, @off)
    ).

%   Blink while invulnerable.

blink(S, Dt) :-
    get(S, safe, Safe0),
    (   Safe0 > 0
    ->  Safe is Safe0 - Dt,
        send(S, slot, safe, Safe),
        Blink is truncate(Safe*8) mod 2,
        (   Blink == 0
        ->  send(S, displayed, @on)
        ;   send(S, displayed, @off)
        )
    ;   send(S, displayed, @on)
    ).

fire(S, Fire, Dt) :-
    get(S, reload, Reload0),
    Reload is Reload0 - Dt,
    (   Fire == true,
        Reload =< 0,
        get(S, device, P),
        get(P?graphicals, find_all,
            message(@arg1, instance_of, asteroids_bullet), Bullets),
        get(Bullets, size, NB),
        get(S, max_bullets, Max),
        NB < Max
    ->  get(S, bullet_speed, BS),
        get(S, angle, Angle),
        get(S, px, X0), get(S, py, Y0),
        get(S, vx, VX0), get(S, vy, VY0),
        Rad is Angle*pi/180,
        X is X0 + cos(Rad)*16, Y is Y0 + sin(Rad)*16,
        VX is VX0 + cos(Rad)*BS, VY is VY0 + sin(Rad)*BS,
        send(P, display, asteroids_bullet(X, Y, VX, VY)),
        send(P, play, fire),
        get(S, class_variable_value, reload, Interval),
        send(S, slot, reload, Interval)
    ;   Left is max(0, Reload),
        send(S, slot, reload, Left)
    ).

collide(S) :-
    get(S, safe, Safe),
    Safe =< 0,
    get(S, px, X), get(S, py, Y),
    get(S, hit_radius, R),
    hit_asteroid(S, X, Y, R, _),
    !,
    get(S, device, P),
    get(S, burst_particles, NP),
    burst(P, X, Y, NP),
    send(P, play, explosion),
    send(S, destroy),
    send(P, ship_lost).
collide(_).

:- pce_end_class(asteroids_ship).


		 /*******************************
		 *	      BULLETS		*
		 *******************************/

:- pce_begin_class(asteroids_bullet(px, py, vx, vy), circle,
                   "A bullet fired by the ship").

variable(px,   num, get, "X of the centre").
variable(py,   num, get, "Y of the centre").
variable(vx,   num, get, "Horizontal velocity").
variable(vy,   num, get, "Vertical velocity").
variable(life, num, get, "Seconds until the bullet vanishes").

class_variable(life,       num, 1.0, "Seconds until the bullet vanishes").
class_variable(hit_radius, num, 2,   "Radius for collisions").

initialise(B, X:num, Y:num, VX:num, VY:num) :->
    send_super(B, initialise, 4),
    send(B, fill, colour(white)),
    send(B, colour, colour(cyan)),
    send(B, slot, px, X),
    send(B, slot, py, Y),
    set_velocity(B, VX, VY),
    move_circle(B, X, Y).

%   A bullet destroys at most one asteroid.

tick(B) :->
    "Move and check whether we hit an asteroid"::
    move(B, X, Y, Dt),
    get(B, life, Life0),
    Life is Life0 - Dt,
    (   Life < 0
    ->  send(B, destroy)
    ;   get(B, hit_radius, R),
        hit_asteroid(B, X, Y, R, A)
    ->  send(B, destroy),
        send(A, explode)
    ;   send(B, slot, life, Life),
        move_circle(B, X, Y)
    ).

:- pce_end_class(asteroids_bullet).


		 /*******************************
		 *	     ASTEROIDS		*
		 *******************************/

:- pce_begin_class(asteroid(size, px, py), figure,
                   "A rock floating through space").

variable(size,       {large,medium,small}, get, "Size class").
variable(hit_radius, int, get, "Radius for collisions").
variable(px,         num, get, "X of the centre").
variable(py,         num, get, "Y of the centre").
variable(vx,         num, get, "Horizontal velocity").
variable(vy,         num, get, "Vertical velocity").
variable(angle,      num, get, "Rotation in degrees").
variable(spin,       num, get, "Rotation speed in degrees/second").

class_variable(large_hit_radius,  int, 42,  "Radius of a large rock").
class_variable(medium_hit_radius, int, 24,  "Radius of a medium rock").
class_variable(small_hit_radius,  int, 13,  "Radius of a small rock").
class_variable(large_score,       int, 20,  "Points for a large rock").
class_variable(medium_score,      int, 50,  "Points for a medium rock").
class_variable(small_score,       int, 100, "Points for a small rock").
class_variable(large_min_speed,   num, 30,  "Minimal speed of a large rock").
class_variable(large_max_speed,   num, 70,  "Maximal speed of a large rock").
class_variable(medium_min_speed,  num, 50,  "Minimal speed of a medium rock").
class_variable(medium_max_speed,  num, 100, "Maximal speed of a medium rock").
class_variable(small_min_speed,   num, 70,  "Minimal speed of a small rock").
class_variable(small_max_speed,   num, 140, "Maximal speed of a small rock").
class_variable(max_spin,          num, 60,  "Maximal spin in degrees/second").
class_variable(corners,           int, 11,  "Corners of the outline").
class_variable(burst_particles,   int, 10,  "Particles of an explosion").

initialise(A, Size:{large,medium,small}, X:num, Y:num) :->
    "Create a rock of Size at X,Y with a random velocity"::
    send_super(A, initialise),
    size_property(A, Size, hit_radius, R),
    send(A, slot, size, Size),
    send(A, slot, hit_radius, R),
    send(A, slot, px, X),
    send(A, slot, py, Y),
    random_velocity(A, Size, VX, VY),
    set_velocity(A, VX, VY),
    get(A, max_spin, MaxSpin),
    Spin is (random_float*2-1)*MaxSpin,
    Angle is random_float*360,
    send(A, slot, spin, Spin),
    send(A, slot, angle, Angle),
    get(A, corners, N),
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
    send(A, display, Glow),
    closed_path(Outline, Rock),             % the shaded rock
    new(Shade, gradient(radial, point(0.3, 0.3), point(0.5, 0.5),
                        0, 0.6, units := bounding_box)),
    send(Shade, add_stop, 0, colour(Light)),
    send(Shade, add_stop, 1, colour(Dark)),
    send(Rock, fill, Shade),
    send(Rock, colour, colour(Edge)),
    send(Rock, pen, 2),
    send(A, display, Rock),
    craters(A, R, Dark),
    place(A, X, Y, Angle).

tick(A) :->
    "Float and spin"::
    move(A, X, Y, Dt),
    get(A, angle, Angle0),
    get(A, spin, Spin),
    Angle is Angle0 + Spin*Dt,
    send(A, slot, angle, Angle),
    place(A, X, Y, Angle).

hit(A, X:num, Y:num, R:num) :->
    "Test whether a circle at X,Y with radius R touches the rock"::
    get(A, px, AX), get(A, py, AY),
    get(A, hit_radius, AR),
    DX is X - AX, DY is Y - AY,
    D is AR + R,
    DX*DX + DY*DY < D*D.

explode(A) :->
    "Hit by a bullet: score, burst and split into two smaller rocks"::
    get(A, device, P),
    get(A, size, Size),
    get(A, px, X), get(A, py, Y),
    size_property(A, Size, score, Score),
    get(A, burst_particles, NP),
    send(A, destroy),
    send(P, add_score, Score),
    burst(P, X, Y, NP),
    (   split(Size, Smaller)
    ->  send(P, play, bang),
        forall(between(1, 2, _),
               send(P, display, asteroid(Smaller, X, Y)))
    ;   send(P, play, bang_small)
    ).

%!  size_property(+Asteroid, +Size, +Property, -Value) is det.
%
%   Value is the class variable <Size>_<Property> of Asteroid.

size_property(A, Size, Property, Value) :-
    atomic_list_concat([Size, Property], '_', Name),
    get(A, Name, Value).

split(large,  medium).
split(medium, small).

random_velocity(A, Size, VX, VY) :-
    size_property(A, Size, min_speed, Min),
    size_property(A, Size, max_speed, Max),
    Speed is Min + random_float*(Max-Min),
    Dir is random_float*2*pi,
    VX is cos(Dir)*Speed, VY is sin(Dir)*Speed.

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

:- pce_end_class(asteroid).


		 /*******************************
		 *	     PARTICLES		*
		 *******************************/

%   Particles fly from an explosion and fade out during their life.

:- pce_begin_class(asteroids_particle(px, py), circle,
                   "A spark of an explosion").

variable(px,       num, get, "X of the centre").
variable(py,       num, get, "Y of the centre").
variable(vx,       num, get, "Horizontal velocity").
variable(vy,       num, get, "Vertical velocity").
variable(life,     num, get, "Seconds until the particle vanishes").
variable(lifetime, num, get, "Total life time in seconds").

class_variable(min_speed,    num, 40,  "Minimal speed in pixels/second").
class_variable(max_speed,    num, 200, "Maximal speed in pixels/second").
class_variable(min_lifetime, num, 0.4, "Minimal life time in seconds").
class_variable(max_lifetime, num, 1.0, "Maximal life time in seconds").

initialise(Pt, X:num, Y:num) :->
    send_super(Pt, initialise, 3),
    random_member(Colour, [white, yellow, gold, orange, orange_red, red]),
    send(Pt, fill, colour(Colour)),
    send(Pt, colour, colour(Colour)),
    send(Pt, slot, px, X),
    send(Pt, slot, py, Y),
    Dir is random_float*2*pi,
    get(Pt, min_speed, MinSpeed),
    get(Pt, max_speed, MaxSpeed),
    Speed is MinSpeed + random_float*(MaxSpeed-MinSpeed),
    VX is cos(Dir)*Speed, VY is sin(Dir)*Speed,
    set_velocity(Pt, VX, VY),
    get(Pt, min_lifetime, MinLife),
    get(Pt, max_lifetime, MaxLife),
    Life is MinLife + random_float*(MaxLife-MinLife),
    send(Pt, slot, life, Life),
    send(Pt, slot, lifetime, Life),
    move_circle(Pt, X, Y).

tick(Pt) :->
    "Fly and fade"::
    move(Pt, X, Y, Dt),
    get(Pt, life, Life0),
    Life is Life0 - Dt,
    (   Life < 0
    ->  send(Pt, destroy)
    ;   send(Pt, slot, life, Life),
        move_circle(Pt, X, Y),
        get(Pt, lifetime, Total),
        Opacity is Life/Total,
        send(Pt, opacity, Opacity)
    ).

:- pce_end_class(asteroids_particle).


		 /*******************************
		 *	       STARS		*
		 *******************************/

%   A star twinkles now and then: it dims for a moment.

:- pce_begin_class(asteroids_star(width), box, "A star in the background").

variable(twinkle, num, get, "Seconds left of the current twinkle").

class_variable(twinkle,         num, 0.3,  "Duration of a twinkle in seconds").
class_variable(twinkle_odds,    int, 1000, "Twinkle once in so many ticks").
class_variable(twinkle_opacity, num, 0.3,  "Opacity of a twinkling star").

initialise(S, Size:int) :->
    send_super(S, initialise, Size, Size),
    random_member(Colour, [grey40, grey60, grey80, white, light_sky_blue]),
    send(S, pen, 0),
    send(S, fill, colour(Colour)),
    send(S, slot, twinkle, 0.0).

tick(S) :->
    "Maybe twinkle"::
    get(S, twinkle, T0),
    (   T0 > 0
    ->  get(S?device, dt, Dt),
        T is T0 - Dt,
        (   T > 0
        ->  send(S, slot, twinkle, T)
        ;   send(S, slot, twinkle, 0.0),
            send(S, opacity, 1)
        )
    ;   get(S, twinkle_odds, Odds),
        random(Odds) =:= 0
    ->  get(S, class_variable_value, twinkle, Time),
        get(S, twinkle_opacity, Opacity),
        send(S, slot, twinkle, Time),
        send(S, opacity, Opacity)
    ;   true
    ).

:- pce_end_class(asteroids_star).


		 /*******************************
		 *	    SCOREBOARD		*
		 *******************************/

:- pce_begin_class(asteroids_score, text, "Score, lives and level").

class_variable(font, font, font(mono, bold, 16)).

initialise(T) :->
    send_super(T, initialise, ''),
    send(T, colour, colour(white)).

tick(T) :->
    "Show the state of the game"::
    get(T, device, P),
    get(P, score, Score),
    get(P, lives, Lives),
    get(P, level, Level),
    format(string(String), 'Score ~w   Lives ~w   Level ~w',
           [Score, Lives, Level]),
    send(T, string, String).

:- pce_end_class(asteroids_score).

:- pce_begin_class(asteroids_banner, text,
                   "Message in the middle of the field").

class_variable(font,   font, font(sans, bold, 28)).
class_variable(format, name, center).

initialise(T) :->
    send_super(T, initialise, ''),
    send(T, name, banner),
    send(T, colour, colour(white)).

tick(T) :->
    "Show whether the game is over, paused or a level is done"::
    get(T, device, P),
    (   get(P, mode, game_over)
    ->  String = 'GAME OVER\nPress Enter'
    ;   get(P, paused, @on)
    ->  String = 'PAUSED'
    ;   get(P, level_break, Break),
        Break > 0
    ->  get(P, level, Level),
        format(string(String), 'LEVEL ~w CLEARED', [Level])
    ;   String = ''
    ),
    send(T, string, String),
    send(T, center, P?center).

:- pce_end_class(asteroids_banner).


		 /*******************************
		 *	   SHARED HELPERS	*
		 *******************************/

%!  wrap(+Graphical, +X0, +Y0, -X, -Y) is det.
%
%   Objects leaving the field on one side come back on the other.

wrap(Gr, X0, Y0, X, Y) :-
    get(Gr, device, P),
    get(P, width, W),
    get(P, height, H),
    X is X0 - W*floor(X0/W),
    Y is Y0 - H*floor(Y0/H).

%!  move(+Graphical, -X, -Y, -Dt) is det.
%
%   Move Graphical according to its velocity for the duration of this
%   tick.  Graphical has the slots `px`, `py`, `vx` and `vy`.

move(Gr, X, Y, Dt) :-
    get(Gr?device, dt, Dt),
    get(Gr, px, X0), get(Gr, py, Y0),
    get(Gr, vx, VX), get(Gr, vy, VY),
    wrap(Gr, X0 + VX*Dt, Y0 + VY*Dt, X, Y),
    send(Gr, slot, px, X),
    send(Gr, slot, py, Y).

set_velocity(Gr, VX, VY) :-
    send(Gr, slot, vx, VX),
    send(Gr, slot, vy, VY).

move_circle(C, X, Y) :-
    CX is round(X)-2, CY is round(Y)-2,
    send(C, set, CX, CY).

key_down(Key) :-
    get(@keyboard_device, key_down, Key, @on).

key(Key, V) :-
    (   key_down(Key)
    ->  V = 1
    ;   V = 0
    ).

%!  hit_asteroid(+Graphical, +X, +Y, +R, -Asteroid) is semidet.
%
%   True when a circle at X,Y with radius R on the device of Graphical
%   touches Asteroid.

hit_asteroid(Gr, X, Y, R, A) :-
    get(Gr?device?graphicals, find,
        and(message(@arg1, instance_of, asteroid),
            message(@arg1, hit, X, Y, R)),
        A).

burst(P, X, Y, N) :-
    forall(between(1, N, _),
           send(P, display, asteroids_particle(X, Y))).

closed_path(Points, Path) :-
    new(Path, path),
    send_list(Path, append, Points),
    send(Path, closed, @on).

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
