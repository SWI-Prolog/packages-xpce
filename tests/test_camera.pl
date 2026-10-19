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

:- module(test_camera, [test_camera/0]).

/** <module> Cameras

Uses the dummy camera driver of SDL, which has no cameras, so the test
never opens a real camera.  `camera->sdl_test_frame` delivers frames.

Run with:

    swipl -g test_camera -t halt packages/xpce/tests/test_camera.pl
*/

%   SDL copies the environment when it is initialised, i.e., when
%   library(pce) is loaded.

:- initialization(setenv('SDL_CAMERA_DRIVER', dummy), now).

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_camera :-
    run_tests([ camera ]).

:- dynamic
    frame_seen/1.

user:test_camera_frame(Camera) :-
    assertz(test_camera:frame_seen(Camera)).

frames(Camera, N) :-
    aggregate_all(count, frame_seen(Camera), N).

%!  rgb(+Image, +X, +Y, -RGB) is det.
%
%   Colour of a pixel, rounded to multiples of 8 to hide the rounding
%   of the YUV conversion.

rgb(Image, X, Y, rgb(R, G, B)) :-
    get(Image, pixel, X, Y, Colour),
    get(Colour, red, R0), R is round(R0/8)*8,
    get(Colour, green, G0), G is round(G0/8)*8,
    get(Colour, blue, B0), B is round(B0/8)*8.

:- begin_tests(camera).

test(no_cameras, Names == []) :-
    get(@display, cameras, Chain),
    chain_list(Chain, Names).
test(initial_status, S == closed) :-
    new(C, camera),
    get(C, status, S).
test(open_without_camera, fail) :-
    new(C, camera),
    send(C, open).
test(frame, [W-H-RGB == 64-48-rgb(256, 0, 0)]) :-
    new(C, camera),
    send(C, sdl_test_frame, 64, 48, colour(red)),
    get(C, image, I),
    get(I, size, size(W, H)),
    rgb(I, 10, 10, RGB).
test(mjpg_frame, [W-H-RGB == 64-48-rgb(256, 0, 0)]) :-
    new(C, camera),                     % many cameras deliver JPEG
    send(C, sdl_test_frame, 64, 48, colour(red), mjpg),
    get(C, image, I),
    get(I, size, size(W, H)),
    rgb(I, 10, 10, RGB).
test(message, N == 2) :-
    new(C, camera),
    send(C, message, message(@prolog, test_camera_frame, @arg1)),
    send(C, sdl_test_frame, 16, 16, colour(blue)),
    send(C, sdl_test_frame, 16, 16, colour(blue)),
    frames(C, N).
test(bitmap_follows_size, [Sizes == [64-48, 32-24]]) :-
    new(C, camera),
    get(C, image, I),
    new(B, bitmap(I)),
    send(C, sdl_test_frame, 64, 48, colour(green)),
    get(B, size, size(W1, H1)),
    send(C, sdl_test_frame, 32, 24, colour(green)),
    get(B, size, size(W2, H2)),
    Sizes = [W1-H1, W2-H2].

test(texture, [W-H-Format-RGB-ImageW == 64-48-yuy2-rgb(256, 0, 0)-1]) :-
    new(C, camera),                     % frames keep the camera's format
    send(C, texture, new(T, sdl_texture)),
    send(C, sdl_test_frame, 64, 48, colour(red)),
    get(T, size, size(W, H)),
    get(T, format, Format),
    rgb(T?image, 10, 10, RGB),
    get(C?image, size, size(ImageW, _)).
test(texture_reused, [W-H-RGB == 32-24-rgb(0, 0, 256)]) :-
    new(C, camera),
    send(C, texture, new(T, sdl_texture)),
    send(C, sdl_test_frame, 64, 48, colour(red)),
    send(C, sdl_test_frame, 32, 24, colour(green)),
    send(C, sdl_test_frame, 32, 24, colour(blue)),
    get(T, size, size(W, H)),
    rgb(T?image, 10, 10, RGB).
test(texture_mjpg, [W-H-RGB == 64-48-rgb(256, 0, 0)]) :-
    new(C, camera),                     % JPEG is decoded
    send(C, texture, new(T, sdl_texture)),
    send(C, sdl_test_frame, 64, 48, colour(red), mjpg),
    get(T, size, size(W, H)),
    assertion(\+ get(T, format, mjpg)),
    rgb(T?image, 10, 10, RGB).
test(texture_rendered, [RGB == rgb(256, 0, 0)]) :-
    new(C, camera),                     % the renderer draws the YUY2
    send(C, texture, new(T, sdl_texture)),
    send(C, sdl_test_frame, 64, 48, colour(red)),
    new(W, sdl_window),
    send(W, size, size(64, 48)),
    send(W, append, sdl_layer(T, @default, stretch)),
    send(W, open),
    get(W?frame, sdl_test_image, I),
    get(W?area, x, X0), get(W?area, y, Y0),
    X is X0+10, Y is Y0+10,
    rgb(I, X, Y, RGB),
    send(W?frame, destroy).

:- end_tests(camera).
