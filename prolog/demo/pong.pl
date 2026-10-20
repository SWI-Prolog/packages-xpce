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


:- module(pong,
          [ pong/0
          ]).
:- use_module(library(pce)).
:- use_module(library(random)).
:- autoload(library(pce_class_variable_editor), [class_variable_editor/1]).

/** <module> The good old Pong

Two paddles hit a ball back and forth.  A player who misses the ball
gives the other player a point.  The first to reach <-points_to_win
wins.  The ball speeds up on every hit and the angle at which it leaves
a paddle depends on where it hits the paddle: hitting it with the edge
of the paddle makes a steep shot.

Play alone against the computer or with two players.  Alone, you play
the left paddle using w/s or cursor up/down.  With two players, the
left player uses w/s and the right player cursor up/down.  Gamepads
work as well: the left stick or the dpad moves the paddle.  The first
gamepad is the left player, the second the right player.  p pauses,
Enter starts a new game or resumes a paused one and F1 shows the
controls.  Before the first game the computer plays against itself.

The game is built like the Asteroids demo (see demo(asteroids)).
The field is an sdl_window and everything that moves is a sprite: an
sdl_layer that holds its own state.  The paddles, the ball, its trail,
the sparks and the net are sprites; the scores, the banner and the
buttons are ordinary graphicals.  A timer sends `tick` to all layers
and graphicals of the window, which then move themselves.  The
parameters of the game are class variables, which can be edited using
the settings button.

Run with:

    ?- pong.
*/

pong :-
    new(F, pong_game),
    send(F, open).

		 /*******************************
		 *	       THE GAME		*
		 *******************************/

:- pce_begin_class(pong_game, frame, "The good old Pong").

initialise(F) :->
    send_super(F, initialise, 'Pong'),
    send(F, append, new(P, pong_window(size(800, 600)))),
    send(P, intro).

:- pce_end_class(pong_game).


%   The window holds the state of the game as a whole and drives the
%   game using a timer.  The paddles and the ball are layers that are
%   also remembered in slots of the window.  The controls of the
%   paddles are polled on each tick.

:- pce_begin_class(pong_window(size), sdl_window, "The field of the game").

variable(timer,       timer*,        get, "Timer that drives the game").
variable(mode,        {intro,playing,game_over}, get, "State of the game").
variable(players,     '1..2',        get, "Number of human players").
variable(left,        pong_paddle*,  get, "The left paddle").
variable(right,       pong_paddle*,  get, "The right paddle").
variable(ball,        pong_ball*,    get, "The ball").
variable(left_score,  int,           get, "Points of the left player").
variable(right_score, int,           get, "Points of the right player").
variable(winner,      {left,right}*, get, "Winner of the last game").
variable(serve,       num,           get, "Seconds until the next serve").
variable(serve_to,    {left,right},  get, "Side the next serve goes to").
variable(paused,      bool,          get, "Game is paused").
variable(help,        bool,          get, "Show the controls").
variable(time,        num,           get, "Time of the last tick").
variable(dt,          num,           get, "Seconds since the last tick").
variable(gamepads,    chain,         get, "Connected gamepads").

class_variable(tick,          num, 0.016, "Seconds per tick").
class_variable(max_dt,        num, 0.05,  "Longest tick in seconds").
class_variable(points_to_win, int, 11,    "Points needed to win a game").
class_variable(serve,         num, 1.0,   "Seconds before a serve").
class_variable(volume,        '0.0..1.0', 0.4, "Volume of the sound effects").

initialise(P, Size:size=size) :->
    send_super(P, initialise, 'Pong', Size),
    send(P, background, colour(black)),
    send(P, slot, players, 1),
    send(P, slot, paused, @off),
    send(P, slot, help, @off),
    get_time(Now),
    send(P, slot, time, Now),
    send(P, slot, dt, 0.0),
    get(@display, gamepads, Pads),
    send(P, slot, gamepads, Pads),
    send(P, recogniser, new(KB, key_binding)),
    send(KB, function,  p,    message(@receiver, toggle_paused)),
    send(KB, function, 'RET', message(@receiver, start)),
    send(KB, function,  f1,   message(@receiver, toggle_help)).

unlink(P) :->
    "Stop the timer"::
    send(P?timer, stop),
    send_super(P, unlink).

%!  <-input(+Side, -Move)
%
%   Move is the desired direction of the paddle at Side: -1 is up, 1 is
%   down.  Alone, the player may use the keys of either side.

input(P, Side:{left,right}, Move:num) :<-
    "Get the move of the paddle at Side from the keyboard or gamepad"::
    (   get(P, input_focus, @on)
    ->  get(P, players, Players),
        findall(Up-Down, side_keys(Players, Side, Up, Down), Keys),
        foldl(key_move, Keys, 0, KM),
        pad_move(P, Players, Side, PM),
        Move is max(-1, min(1, KM+PM))
    ;   Move = 0
    ).

side_keys(_, left,  w,         s).
side_keys(1, left,  cursor_up, cursor_down).
side_keys(2, right, cursor_up, cursor_down).

key_move(Up-Down, M0, M) :-
    key(Up, U),
    key(Down, D),
    M is M0 + D - U.

pad_move(P, Players, Side, Move) :-
    (   pad_index(Players, Side, I),
        get(P?gamepads, nth1, I, Pad),
        get(Pad, connected, @on)
    ->  get(Pad, axis, left_y, Y),
        pad_button(Pad, dpad_up, U),
        pad_button(Pad, dpad_down, D),
        (   abs(Y) > 0.2
        ->  Move is Y + D - U
        ;   Move is D - U
        )
    ;   Move = 0
    ).

pad_index(_, left,  1).
pad_index(2, right, 2).

pad_button(Pad, Button, V) :-
    (   get(Pad, button, Button, @on)
    ->  V = 1
    ;   V = 0
    ).

human(P, Side:{left,right}) :->
    "True if the paddle at Side is played by a human"::
    \+ get(P, mode, intro),
    (   Side == left
    ->  true
    ;   get(P, players, 2)
    ).

%!  ->intro
%
%   The computer plays against itself until a game is started.

intro(P) :->
    "Show the computer playing and the buttons to start a game"::
    send(P, reset),
    send(P, slot, mode, intro),
    send(P, panel).

new_game(P, Players:[1..2]) :->
    "Start a new game for the given number of players"::
    (   Players == @default
    ->  true
    ;   send(P, slot, players, Players)
    ),
    get(@display, gamepads, Pads),
    send(P, slot, gamepads, Pads),
    send(P, reset),
    send(P, slot, mode, playing),
    send(P, panel).

reset(P) :->
    "Empty the field and reset the state of the game"::
    send(P, start_ticking),
    send(P?layers, for_all, message(@arg1, free)),
    send(P, clear),
    send(P, slot, left_score, 0),
    send(P, slot, right_score, 0),
    send(P, slot, winner, @nil),
    send(P, slot, paused, @off),
    send(P, slot, help, @off),
    send(P, net),
    send(P, slot, left, new(L, pong_paddle(left))),
    send(P, slot, right, new(R, pong_paddle(right))),
    send(P, slot, ball, new(B, pong_ball)),
    send_list(P, append, [L, R, B]),
    random_member(Side, [left, right]),
    send(P, serve, Side),
    send(P, display, new(pong_score(left))),
    send(P, display, new(pong_score(right))),
    send(P, display, new(pong_banner)),
    send(P, display, new(pong_panel(P))),
    send(P, display, new(pong_help)).

playing(P) :->
    "True when the players are playing, i.e., not paused or game over"::
    get(P, mode, playing),
    get(P, paused, @off).

%!  ->panel
%
%   Show the buttons and the pointer if the game is not being played,
%   and hide them otherwise.  The help is only shown with the buttons.

panel(P) :->
    "Show or hide the buttons and the pointer"::
    get(P, member, panel, Panel),
    get(P, member, help, Help),
    (   send(P, playing)
    ->  send(Panel, displayed, @off),
        send(Help, displayed, @off),
        send(P, keyboard_focus, @nil),  % keys must reach the key binding
        send(P, cursor, none)
    ;   send(Panel, update),
        send(Panel, displayed, @on),
        send(Help, displayed, P?help),
        send(P, cursor, @nil)
    ).

start(P) :->
    "Resume a paused game or start a new one"::
    (   get(P, paused, @on)
    ->  send(P, toggle_paused)
    ;   send(P, new_game)
    ).

toggle_help(P) :->
    "Show or hide the controls"::
    (   send(P, playing)
    ->  send(P, toggle_paused)          % F1 while playing pauses
    ;   true
    ),
    send(P, slot, help, P?help?negate),
    send(P, panel).

toggle_paused(P) :->
    "Pause the game on 'p'"::
    (   get(P, mode, playing)
    ->  send(P, slot, paused, P?paused?negate),
        send(P, panel)
    ;   true
    ).

settings(P) :->
    "Edit the settings of the game"::
    class_variable_editor(P).

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
    ->  true
    ;   send(P?layers, for_all, message(@arg1, tick))
    ),
    send(P?graphicals, for_all, message(@arg1, tick)).

%!  ->serve(+Side)
%
%   Put the ball in the middle.  After <-serve seconds it starts moving
%   to Side.

serve(P, Side:{left,right}) :->
    "Prepare to serve the ball to Side"::
    get(P, class_variable_value, serve, Delay),
    send(P, slot, serve, Delay),
    send(P, slot, serve_to, Side),
    get(P?visible, center, point(X, Y)),
    send(P?ball, center, X, Y).

%!  ->point(+Side)
%
%   The player at Side scores as the other player missed the ball.  The
%   next serve goes to the player who missed.

point(P, Side:{left,right}) :->
    "The player at Side scores a point"::
    other_side(Side, Loser),
    (   get(P, mode, playing)
    ->  score_slot(Side, Slot),
        get(P, Slot, Score0),
        Score is Score0 + 1,
        send(P, slot, Slot, Score),
        get(P, points_to_win, Win),
        (   Score >= Win
        ->  send(P, slot, winner, Side),
            send(P, slot, mode, game_over),
            send(P?ball, displayed, @off),
            send(P, play, win),
            send(P, panel)
        ;   send(P, play, score),
            send(P, serve, Loser)
        )
    ;   send(P, serve, Loser)
    ).

score_slot(left,  left_score).
score_slot(right, right_score).

other_side(left, right).
other_side(right, left).

%!  ->resize
%
%   The user resized the window.  The net is made anew for the new
%   height.  The paddles find their place on the next tick.

resize(P) :->
    "Make a new net for the new size"::
    send(P?layers, for_all,
         if(message(@arg1, instance_of, pong_net),
            message(@arg1, free))),
    send(P, net).

%!  ->net
%
%   The dashed line in the middle of the field.  It is put below all
%   other layers.

net(P) :->
    "Add the net below everything"::
    get(P, size, size(W,H)),
    X is W//2 - 2,
    forall(( between(0, H, Y), Y mod 24 =:= 6 ),
           ( send(P, append, new(Dash, pong_net(X, Y))),
             send(Dash, hide)
           )).

%   Each play uses a new audio_stream that gets the samples of the
%   effect and is freed after it is played.  The samples are computed
%   the first time an effect is used.  Without an audio device, the
%   game is silent.

play(P, Effect:{paddle,wall,score,win}) :->
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

effect(paddle, 0.07, square(880, 880)).
effect(wall,   0.05, square(440, 440)).
effect(score,  0.40, square(500, 120)).
effect(win,    0.80, square(400, 1200)).

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

%   The beeps of the classic game: a square wave, sweeping from F0 to
%   F1 and fading out at the end.

sample(square(F0, F1), I, N, Rate, V) :-
    T is I/Rate,
    F is F0 + (F1-F0)*I/(2*N),          % phase of a linear sweep
    Fade is min(1, (N-I)/(N*0.2)),
    V is 0.3*sign(sin(2*pi*F*T))*Fade.

:- pce_end_class(pong_window).


		 /*******************************
		 *	      PADDLES		*
		 *******************************/

:- pce_begin_class(pong_paddle(side), sdl_layer, "The paddle of a player").

variable(side, {left,right}, get, "Side of the field").
variable(py,   num,          get, "Y of the centre").
variable(aim,  num,          get, "Where the computer aims, relative to the centre").

class_variable(length,   int, 80,  "Length of the paddle").
class_variable(width,    int, 12,  "Width of the paddle").
class_variable(margin,   int, 30,  "Distance between centre and edge of the field").
class_variable(speed,    num, 550, "Pixels/second").
class_variable(ai_speed, num, 330, "Pixels/second when played by the computer").
class_variable(ai_error, num, 0.8, "How far the computer may miss the middle").

initialise(Pd, Side:{left,right}) :->
    get(Pd, length, L),
    get(Pd, width, W),
    texture(paddle(W, L), Texture),
    send_super(Pd, initialise, Texture, @nil, none),
    send(Pd, slot, side, Side),
    send(Pd, slot, py, 300),
    send(Pd, slot, aim, 0.0).

px(Pd, X:num) :<-
    "X of the centre"::
    get(Pd, margin, M),
    (   get(Pd, side, left)
    ->  X = M
    ;   get(Pd?window, width, W),
        X is W - M
    ).

face(Pd, X:num) :<-
    "X of the side facing the ball"::
    get(Pd, px, PX),
    get(Pd, width, W),
    (   get(Pd, side, left)
    ->  X is PX + W/2
    ;   X is PX - W/2
    ).

tick(Pd) :->
    "Move the paddle"::
    get(Pd, window, P),
    get(P, dt, Dt),
    get(Pd, side, Side),
    get(Pd, py, Y0),
    (   send(P, human, Side)
    ->  get(P, input, Side, Move),
        get(Pd, speed, Speed),
        Y1 is Y0 + Move*Speed*Dt
    ;   get(Pd, target, Target),
        get(Pd, ai_speed, Speed),
        Step is Speed*Dt,
        Y1 is Y0 + max(-Step, min(Step, Target-Y0))
    ),
    get(P, height, H),
    get(Pd, length, L),
    Y is max(L/2, min(H-L/2, Y1)),
    send(Pd, slot, py, Y),
    get(Pd, px, X),
    place(Pd, X, Y).

%   The computer follows the ball when it comes to the paddle and
%   returns to the middle otherwise.

target(Pd, Y:num) :<-
    "Where the computer wants the paddle"::
    get(Pd, window, P),
    get(P, ball, B),
    (   get(P, serve, Serve),
        Serve =:= 0,
        get(B, displayed, @on),
        get(B, vx, VX),
        (   get(Pd, side, left)
        ->  VX < 0
        ;   VX > 0
        )
    ->  get(B, py, BY),
        get(Pd, aim, Aim),
        Y is BY - Aim
    ;   get(P, height, H),
        Y is H/2
    ).

new_aim(Pd) :->
    "Choose a new point to hit the ball with"::
    get(Pd, length, L),
    get(Pd, ai_error, Error),
    Aim is (random_float*2-1)*Error*L/2,
    send(Pd, slot, aim, Aim).

%!  hit(+Ball, +X0, +X1, +Y, -X) is semidet.
%
%   True when the ball moving from X0 to X1 hits the face of the paddle
%   at height Y.  Then the ball bounces off the paddle at an angle that
%   depends on where it hits the paddle.  X is the new position of the
%   ball, against the face.

hit(Pd, B:pong_ball, X0:num, X1:num, Y:num, X:num) :<-
    "Bounce B off the paddle if it hits it"::
    get(Pd, face, F),
    get(B, radius, R),
    (   get(Pd, side, left)
    ->  Dir = 1
    ;   Dir = -1
    ),
    Dir*(X0 - Dir*R - F) >= 0,          % was in front of the face
    Dir*(X1 - Dir*R - F) < 0,           % and is now behind it
    get(Pd, py, PY),
    get(Pd, length, L),
    abs(Y-PY) =< L/2 + R,
    X is F + Dir*R,
    Rel is max(-1, min(1, (Y-PY)/(L/2))),
    get(B, max_angle, MaxAngle),
    Angle is Rel*MaxAngle*pi/180,
    send(B, speed_up),
    get(B, speed, Speed),
    VX is Dir*Speed*cos(Angle),
    VY is Speed*sin(Angle),
    set_velocity(B, VX, VY).

:- pce_end_class(pong_paddle).


		 /*******************************
		 *	       THE BALL		*
		 *******************************/

:- pce_begin_class(pong_ball, sdl_layer, "The ball").

variable(px,    num, get, "X of the centre").
variable(py,    num, get, "Y of the centre").
variable(vx,    num, get, "Horizontal velocity").
variable(vy,    num, get, "Vertical velocity").
variable(speed, num, get, "Current speed").

class_variable(radius,      int,  7,    "Radius of the ball").
class_variable(serve_speed, num,  380,  "Pixels/second at a serve").
class_variable(speed_up,    num,  1.05, "Speed factor on each hit").
class_variable(max_speed,   num,  1100, "Maximal speed in pixels/second").
class_variable(max_angle,   num,  60,   "Steepest angle off a paddle in degrees").
class_variable(serve_angle, num,  30,   "Steepest angle of a serve in degrees").
class_variable(sparks,      int,  8,    "Sparks when hitting a paddle").
class_variable(trail,       bool, @on,  "Draw a fading trail").

initialise(B) :->
    get(B, radius, R),
    texture(ball(R), Texture),
    send_super(B, initialise, Texture, @nil, none),
    send(B, center, 0, 0),
    set_velocity(B, 0.0, 0.0),
    send(B, slot, speed, 0.0).

center(B, X:num, Y:num) :->
    "Put the ball at X,Y and stop it"::
    send(B, slot, px, X),
    send(B, slot, py, Y),
    set_velocity(B, 0.0, 0.0),
    place(B, X, Y).

speed_up(B) :->
    "Make the ball faster after a hit"::
    get(B, speed, S0),
    get(B, class_variable_value, speed_up, F),
    get(B, max_speed, Max),
    S is min(Max, S0*F),
    send(B, slot, speed, S).

tick(B) :->
    "Wait for the serve or move"::
    get(B, window, P),
    get(P, serve, Serve0),
    (   get(P, mode, game_over)
    ->  true
    ;   Serve0 > 0
    ->  get(P, dt, Dt),
        Serve is max(0.0, Serve0 - Dt),
        send(P, slot, serve, Serve),
        (   Serve =:= 0
        ->  send(B, displayed, @on),
            send(B, launch)
        ;   Blink is truncate(Serve*6) mod 2,
            (   Blink == 0
            ->  send(B, displayed, @on)
            ;   send(B, displayed, @off)
            )
        )
    ;   send(B, fly)
    ).

launch(B) :->
    "Serve the ball"::
    get(B?window, serve_to, Side),
    (   Side == left
    ->  Dir = -1
    ;   Dir = 1
    ),
    get(B, serve_speed, Speed),
    get(B, serve_angle, Max),
    Angle is (random_float*2-1)*Max*pi/180,
    send(B, slot, speed, Speed),
    VX is Dir*Speed*cos(Angle),
    VY is Speed*sin(Angle),
    set_velocity(B, VX, VY),
    send(B?window?left, new_aim),
    send(B?window?right, new_aim).

%   Move the ball, bounce off the walls and the paddles and see whether
%   a player missed the ball.

fly(B) :->
    "Move the ball for one tick"::
    get(B, window, P),
    get(P, dt, Dt),
    get(B, px, X0), get(B, py, Y0),
    get(B, vx, VX), get(B, vy, VY),
    get(B, radius, R),
    get(P, size, size(W, H)),
    X1 is X0 + VX*Dt,
    Y1 is Y0 + VY*Dt,
    wall(B, Y1, R, H, Y),
    (   VX < 0
    ->  get(P, left, Paddle)
    ;   get(P, right, Paddle)
    ),
    (   get(Paddle, hit, B, X0, X1, Y, X)
    ->  send(P, play, paddle),
        get(B, sparks, N),
        sparks(P, X, Y, N),
        send(P?left, new_aim),
        send(P?right, new_aim)
    ;   X = X1
    ),
    send(B, slot, px, X),
    send(B, slot, py, Y),
    place(B, X, Y),
    trail(B, X, Y),
    (   X < -R
    ->  send(P, point, right)
    ;   X > W+R
    ->  send(P, point, left)
    ;   true
    ).

%   Bounce off the top and bottom of the field.

wall(B, Y1, R, H, Y) :-
    (   Y1 < R
    ->  Y is 2*R - Y1
    ;   Y1 > H-R
    ->  Y is 2*(H-R) - Y1
    ),
    !,
    get(B, vx, VX),
    get(B, vy, VY0),
    VY is -VY0,
    set_velocity(B, VX, VY),
    send(B?window, play, wall).
wall(_, Y, _, _, Y).

trail(B, X, Y) :-
    (   get(B, trail, @on)
    ->  get(B, radius, R),
        send(B?window, append, new(T, pong_spark(trail(R), X, Y, 0, 0))),
        send(T, hide, B)
    ;   true
    ).

:- pce_end_class(pong_ball).


		 /*******************************
		 *	      SPARKS		*
		 *******************************/

%   Sparks fly from the ball when it hits a paddle and the ball leaves
%   a trail of sparks that do not move.  They fade out during their
%   life.

:- pce_begin_class(pong_spark, sdl_layer,
                   "A spark or a ghost of the ball").

variable(px,       num, get, "X of the centre").
variable(py,       num, get, "Y of the centre").
variable(vx,       num, get, "Horizontal velocity").
variable(vy,       num, get, "Vertical velocity").
variable(life,     num, get, "Seconds until the spark vanishes").
variable(lifetime, num, get, "Total life time in seconds").

class_variable(lifetime,       num, 0.4,  "Life time of a spark").
class_variable(trail_lifetime, num, 0.12, "Life time of the trail").

initialise(S, Look:prolog, X:num, Y:num, VX:num, VY:num) :->
    texture(Look, Texture),
    send_super(S, initialise, Texture, @nil, none),
    send(S, slot, px, X),
    send(S, slot, py, Y),
    set_velocity(S, VX, VY),
    (   Look = trail(_)
    ->  get(S, trail_lifetime, Life)
    ;   get(S, class_variable_value, lifetime, Max),
        Life is Max*(0.5+random_float/2)
    ),
    send(S, slot, life, Life),
    send(S, slot, lifetime, Life),
    send(S, alpha, 0.5),
    place(S, X, Y).

tick(S) :->
    "Fly and fade"::
    get(S?window, dt, Dt),
    get(S, life, Life0),
    Life is Life0 - Dt,
    (   Life < 0
    ->  send(S, free)
    ;   send(S, slot, life, Life),
        get(S, px, X0), get(S, py, Y0),
        get(S, vx, VX), get(S, vy, VY),
        X is X0 + VX*Dt, Y is Y0 + VY*Dt,
        send(S, slot, px, X),
        send(S, slot, py, Y),
        place(S, X, Y),
        get(S, lifetime, Total),
        Alpha is 0.5*Life/Total,
        send(S, alpha, Alpha)
    ).

:- pce_end_class(pong_spark).


		 /*******************************
		 *	        NET		*
		 *******************************/

:- pce_begin_class(pong_net, sdl_layer, "A dash of the net").

initialise(N, X:int, Y:int) :->
    texture(dash, Texture),
    send_super(N, initialise, Texture, @nil, none),
    place(N, X, Y).

tick(_) :->
    "The net does not move"::
    true.

:- pce_end_class(pong_net).


		 /*******************************
		 *	    SCOREBOARD		*
		 *******************************/

:- pce_begin_class(pong_score(side), text, "Points of a player").

variable(side, {left,right}, get, "Side of the player").

class_variable(font,   font, font(mono, bold, 48)).
class_variable(offset, int,  80, "Distance from the net").

initialise(T, Side:{left,right}) :->
    send_super(T, initialise, ''),
    send(T, slot, side, Side),
    send(T, colour, colour(grey80)).

tick(T) :->
    "Show the points and stay near the net"::
    get(T, device, P),
    get(T, side, Side),
    (   get(P, mode, intro)
    ->  String = ''
    ;   score_slot(Side, Slot),
        get(P, Slot, Score),
        format(string(String), '~w', [Score])
    ),
    send(T, string, String),
    get(P?visible, center, point(CX, _)),
    get(T, offset, Offset),
    (   Side == left
    ->  X is CX - Offset
    ;   X is CX + Offset
    ),
    send(T, center_x, X),
    send(T, y, 16).

:- pce_end_class(pong_score).

:- pce_begin_class(pong_banner, text, "Message in the middle of the field").

class_variable(font,   font, font(sans, bold, 28)).
class_variable(format, name, center).

initialise(T) :->
    send_super(T, initialise, ''),
    send(T, name, banner),
    send(T, colour, colour(white)).

tick(T) :->
    "Show whether the game is over or paused"::
    get(T, device, P),
    (   get(P, mode, intro)
    ->  String = 'PONG'
    ;   get(P, mode, game_over)
    ->  get(P, winner, Winner),
        get(P, players, Players),
        winner_message(Players, Winner, String)
    ;   get(P, paused, @on)
    ->  String = 'PAUSED'
    ;   String = ''
    ),
    send(T, string, String),
    get(P, member, panel, Panel),
    get(Panel, center, point(X, Y)),
    send(T, center, point(X, Y-60)).

winner_message(1, left,  'YOU WIN').
winner_message(1, right, 'THE COMPUTER WINS').
winner_message(2, left,  'LEFT PLAYER WINS').
winner_message(2, right, 'RIGHT PLAYER WINS').

:- pce_end_class(pong_banner).


%   The buttons are shown on start-up, after the game is over and while
%   the game is paused.

:- pce_begin_class(pong_panel, device,
                   "Buttons to play, change the settings and get help").

class_variable(gap, int, 12, "Pixels between the buttons").

initialise(D, Field:pong_window) :->
    send_super(D, initialise),
    send(D, name, panel),
    send(D, display, button(resume,      message(Field, toggle_paused))),
    send(D, display, button(one_player,  message(Field, new_game, 1))),
    send(D, display, button(two_players, message(Field, new_game, 2))),
    send(D, display, button(settings,    message(Field, settings))),
    send(D, display, button(help,        message(Field, toggle_help))),
    send(D?one_player_member, label, '1 Player'),
    send(D?two_players_member, label, '2 Players'),
    send(D, layout).

layout(D) :->
    "Put the displayed buttons next to each other"::
    get(D, gap, Gap),
    get(D?graphicals, find_all, @arg1?displayed == @on, Buttons),
    send(Buttons, for_all,
         message(D, place_button, @arg1, Buttons, Gap)).

place_button(_D, B:button, Buttons:chain, Gap:int) :->
    "Place B to the right of the previous button"::
    get(Buttons, index, B, I),
    (   I > 1
    ->  get(Buttons, nth1, I-1, Prev),
        get(Prev, right_side, R),
        send(B, x, R+Gap)
    ;   send(B, x, 0)
    ).

update(D) :->
    "Show the resume button only if the game is paused"::
    get(D, device, P),
    send(D?resume_member, displayed, P?paused),
    send(D, layout),
    send(D, tick).

tick(D) :->
    "Stay in the middle of the field, higher if the help is shown"::
    get(D, device, P),
    get(P, visible, Area),
    get(Area, center, point(X, Y)),
    (   get(P, member, help, Help),
        get(Help, displayed, @on)
    ->  get(Help, height, H),
        CY is Y - H//2
    ;   CY = Y
    ),
    send(D, center, point(X, CY)).

:- pce_end_class(pong_panel).


%   The controls, shown with the buttons by the help button or F1.

:- pce_begin_class(pong_help, figure, "The controls of the game").

class_variable(font, font, font(mono, normal, 14)).

initialise(H) :->
    send_super(H, initialise),
    send(H, name, help),
    send(H, displayed, @off),
    send(H, border, 12),
    send(H, radius, 8),
    send(H, background, colour(grey15)),
    send(H, pen, 1),
    send(H, colour, colour(grey50)),
    get(H, font, Font),
    findall(Line, help_line(Line), Lines),
    atomic_list_concat(Lines, '\n', String),
    new(T, text(String, left, Font)),
    send(T, colour, colour(white)),
    send(H, display, T).

help_line(Line) :-
    help(Key, Action),
    format(atom(Line), '~w~t~16|~w', [Key, Action]).

help('W/S',            'Move the left paddle').
help('Cursor up/down', 'Move the right paddle').
help('',               'or the left one when alone').
help('Gamepad',        'Left stick or dpad').
help('P',              'Pause or resume').
help('Enter',          'Play or resume').
help('F1',             'Show or hide this help').
help('Control-comma',  'Settings').

tick(H) :->
    "Stay below the buttons"::
    (   get(H, displayed, @on)
    ->  get(H, device, P),
        get(P, member, panel, Panel),
        get(Panel, center_x, X),
        get(Panel, bottom_side, Y),
        send(H, center_x, X),
        send(H, y, Y+20)
    ;   true
    ).

:- pce_end_class(pong_help).


		 /*******************************
		 *	      SETTINGS		*
		 *******************************/

%   The settings shown by the class variable editor.

:- multifile pce_preferences:preferences/2.

pce_preferences:preferences(pong_window,
    [ pong_window - [points_to_win, serve, volume],
      pong_paddle - [speed, ai_speed, ai_error],
      pong_ball   - [serve_speed, speed_up, max_speed, max_angle,
                     serve_angle, trail]
    ]).


		 /*******************************
		 *	   SHARED HELPERS	*
		 *******************************/

set_velocity(Gr, VX, VY) :-
    send(Gr, slot, vx, VX),
    send(Gr, slot, vy, VY).

key(Key, V) :-
    (   get(@keyboard_device, key_down, Key, @on)
    ->  V = 1
    ;   V = 0
    ).

sparks(P, X, Y, N) :-
    forall(between(1, N, _),
           ( Dir is random_float*2*pi,
             Speed is 60 + random_float*180,
             VX is cos(Dir)*Speed, VY is sin(Dir)*Speed,
             send(P, append, pong_spark(spark, X, Y, VX, VY))
           )).

%!  place(+Sprite, +X, +Y) is det.
%
%   Move the hot spot of Sprite to X,Y.  See the Asteroids demo.

place(Sprite, X, Y) :-
    get(Sprite, transform, T0),
    (   T0 == @nil
    ->  new(T, transform)
    ;   T = T0
    ),
    send(T, set, 1, 0, 0, 1, X, Y),
    send(Sprite, transform, T).

%!  texture(+Spec, -Texture) is det.
%
%   Texture is the sdl_texture of the sprites described by Spec.  The
%   textures are made from a figure when first needed and shared by all
%   sprites that look the same.

:- pce_global(@pong_textures, new(hash_table)).

texture(Spec, Texture) :-
    term_to_atom(Spec, Key),
    (   get(@pong_textures, member, Key, Texture)
    ->  true
    ;   texture_figure(Spec, Fig),
        new(Texture, sdl_texture(Fig)),
        free(Fig),
        send(@pong_textures, append, Key, Texture)
    ).

%   The paddle and the ball glow a little.

texture_figure(paddle(W, L), Fig) :-
    new(Fig, figure),
    new(Glow, box(W+6, L+6)),
    send(Glow, radius, 5),
    send(Glow, pen, 0),
    send(Glow, fill, colour(cyan)),
    send(Glow, opacity, 0.3),
    send(Glow, center, point(0, 0)),
    send(Fig, display, Glow),
    new(Body, box(W, L)),
    send(Body, radius, 3),
    send(Body, pen, 0),
    send(Body, fill, colour(white)),
    send(Body, center, point(0, 0)),
    send(Fig, display, Body).
texture_figure(ball(R), Fig) :-
    new(Fig, figure),
    D is 2*R,
    new(Glow, circle(D+8)),
    send(Glow, pen, 0),
    send(Glow, fill, colour(yellow)),
    send(Glow, opacity, 0.3),
    send(Glow, center, point(0, 0)),
    send(Fig, display, Glow),
    new(Ball, circle(D)),
    send(Ball, pen, 0),
    send(Ball, fill, colour(white)),
    send(Ball, center, point(0, 0)),
    send(Fig, display, Ball).
texture_figure(trail(R), Fig) :-
    D is 2*R,
    new(Fig, circle(D)),
    send(Fig, pen, 0),
    send(Fig, fill, colour(gold)),
    send(Fig, center, point(0, 0)).
texture_figure(spark, Fig) :-
    new(Fig, circle(3)),
    send(Fig, pen, 0),
    send(Fig, fill, colour(gold)),
    send(Fig, center, point(0, 0)).
texture_figure(dash, Fig) :-
    new(Fig, box(4, 12)),
    send(Fig, pen, 0),
    send(Fig, fill, colour(grey50)).
