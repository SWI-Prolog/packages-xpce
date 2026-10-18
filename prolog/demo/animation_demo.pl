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

:- module(animation_demo,
          [ animation_demo/0,
            animation_demo/1                    % +File
          ]).
:- use_module(library(pce)).
:- use_module(library(filesex), [directory_file_path/3]).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/** <module> Play animated images

Opens a window that plays an animated image (APNG, GIF, WebP or AVIF)
using class animation.  The controls play and stop the animation, step
to the next frame or select a frame using the slider.  The film strip
below shows all frames; clicking a frame shows it.

The default animation is the bouncing beach ball by Holger Will, the
classic example of the APNG format, which is in the public domain:
https://commons.wikimedia.org/wiki/File:Animated_PNG_example_bouncing_beach_ball.png

Run with:

    ?- animation_demo.
    ?- animation_demo('my_animation.gif').
*/

animation_demo :-
    beach_ball(File),
    animation_demo(File).

animation_demo(File) :-
    new(F, animation_demo),
    send(F, load, File),
    send(F, open).

beach_ball(File) :-
    source_file(beach_ball(_), Source),
    file_directory_name(Source, Dir),
    directory_file_path(Dir, 'beach_ball.png', File).

:- pce_begin_class(animation_demo, frame,
                   "Play an animated image").

variable(timer, timer*, get, "Keeps the controls up to date").

initialise(F) :->
    send_super(F, initialise, 'Animation'),
    send(F, append, new(S, picture('Stage', size(320, 240)))),
    send(S, name, stage),
    send(S, scrollbars, none),
    send(S, background, colour(grey90)),
    send(S, display, new(_, bitmap)),
    send(new(D, dialog), below, S),
    send(D, name, controls),
    send(F, fill_dialog, D),
    send(new(Strip, picture('Frames', size(320, 76))), below, D),
    send(Strip, name, strip),
    send(Strip, scrollbars, horizontal),
    send(F, slot, timer, new(T, timer(0.05, message(F, update)))),
    send(T, start).

fill_dialog(F, D:dialog) :->
    "Create the controls"::
    send(D, append, button(play, message(F, play_or_stop))),
    send(D, append, button(step, message(F, step)), right),
    send(D, append, button(open, message(F, open_file)), right),
    send(D, append, button(quit, message(F, destroy)), right),
    send(D, append, slider(frame, 1, 2, 1, message(F, show_frame, @arg1))),
    send(D, append, new(Info, label(info, ''))),
    send(Info, alignment, left).

unlink(F) :->
    "Stop the timer"::
    get(F, timer, T),
    send(T, stop),
    send_super(F, unlink).

bitmap(F, BM:bitmap) :<-
    "The bitmap that plays the animation"::
    get(F, member, stage, S),
    get(S?graphicals, head, BM).

load(F, File:file) :->
    "Load and play an animated image"::
    new(A, animation(File)),
    get(F, bitmap, BM),
    send(BM, animation, A),
    send(F, center),
    send(F, fill_strip, A),
    get(A, count, N),
    get(F, member, controls, D),
    get(D, frame_member, Slider),
    High is max(2, N),                  % a slider needs a range
    send(Slider, high, High),
    (   N > 1 -> Active = @on ; Active = @off ),
    send(Slider, active, Active),
    send(D?play_member, active, Active),
    send(D?step_member, active, Active),
    send(F, show_info, A),
    get(File, base_name, Base),
    send(F, label, string('Animation: %s', Base)),
    ignore(send(BM, play)).

center(F) :->
    "Center the bitmap on the stage"::
    get(F, bitmap, BM),
    get(F, member, stage, S),
    get(S?visible, size, size(SW, SH)),
    get(BM, size, size(W, H)),
    X is max(0, (SW-W)//2),
    Y is max(0, (SH-H)//2),
    send(BM, position, point(X, Y)).

show_info(F, A:animation) :->
    "Describe the animation"::
    get(A, count, N),
    get(A?size, width, W),
    get(A?size, height, H),
    get(A, loop_count, Loops),
    (   Loops == 0
    ->  Plays = 'loops forever'
    ;   format(atom(Plays), 'plays ~d times', [Loops])
    ),
    get(F, member, controls, D),
    send(D?info_member, format, '%d frames of %dx%d pixels, %s',
         N, W, H, Plays).

fill_strip(F, A:animation) :->
    "Show all frames, scaled to the height of the strip"::
    get(F, member, strip, Strip),
    send(Strip, clear),
    get(A, count, N),
    get(A?size, height, H0),
    Scale is min(1, 48/H0),
    forall(between(1, N, I),
           ( get(A, frame, I, Image),
             get(Image?size, width, W0),
             W is max(1, round(W0*Scale)),
             H is max(1, round(H0*Scale)),
             get(Image, scale, size(W, H), Thumb),
             X is 4 + (I-1)*(W+4),
             send(Strip, display, new(BM, bitmap(Thumb)), point(X, 6)),
             send(BM, recogniser,
                  click_gesture(left, '', single,
                                message(F, show_frame, I)))
           )),
    send(Strip, display, new(Box, box(0, 0))),
    send(Box, name, current),
    send(Box, pen, 2),
    send(Box, colour, colour(red)).

play_or_stop(F) :->
    "Play or stop the animation"::
    get(F, bitmap, BM),
    (   get(BM, playing, @on)
    ->  send(BM, stop)
    ;   send(BM, play)
    ).

step(F) :->
    "Stop and show the next frame"::
    get(F, bitmap, BM),
    send(BM, stop),
    get(BM, frame, I0),
    get(BM?animation, count, N),
    I is I0 mod N + 1,
    send(BM, frame, I).

show_frame(F, I:int) :->
    "Stop and show frame I"::
    get(F, bitmap, BM),
    send(BM, stop),
    send(BM, frame, I).

open_file(F) :->
    "Load another animated image"::
    get(@finder, file, open,
        tuple('Animated images', chain(png, gif, webp, avif)), File),
    send(F, load, File).

update(F) :->
    "Show the current frame in the controls and the film strip"::
    send(F, center),                    % the window may have been resized
    get(F, bitmap, BM),
    get(BM, frame, I),
    get(F, member, controls, D),
    (   get(BM, playing, @on)
    ->  send(D?play_member, label, 'Stop')
    ;   send(D?play_member, label, 'Play')
    ),
    get(D, frame_member, Slider),
    (   I > 0,
        \+ get(Slider, selection, I)
    ->  send(Slider, selection, I)
    ;   true
    ),
    send(F, mark_frame, I).

mark_frame(F, I:int) :->
    "Put the red box around frame I of the film strip and show it"::
    get(F, member, strip, Strip),
    (   get(Strip, member, current, Box),
        get(Strip?graphicals, nth1, I, Thumb)
    ->  get(Thumb, area, area(X, Y, W, H)),
        BX is X-2, BY is Y-2, BW is W+4, BH is H+4,
        send(Box, set, BX, BY, BW, BH),
        send(Strip, normalise, Box)
    ;   true
    ).

:- pce_end_class(animation_demo).
