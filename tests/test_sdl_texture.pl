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


:- module(test_sdl_texture, [test_sdl_texture/0]).

/** <module> SDL textures, layers and windows

Layers are drawn twice: by the renderer when the frame goes to the
screen and with Cairo for frame <-image.  frame <-sdl_test_image reads
back what the renderer draws; both should show the same.

Run with:

    swipl -g test_sdl_texture -t halt packages/xpce/tests/test_sdl_texture.pl
*/

%   SDL copies the environment when it is initialised, i.e., when
%   library(pce) is loaded.

:- initialization(setenv('SDL_CAMERA_DRIVER', dummy), now).

:- use_module(library(pce)).
:- use_module(library(plunit)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_sdl_texture :-
    run_tests([ sdl_texture,
                sdl_texture_image,
                sdl_layer,
                sdl_sprite,
                sdl_mesh,
                sdl_canvas
              ]).

%!  colour_image(+Colours, -Image) is det.
%
%   An image of 10x10 pixel blocks, one for each colour in Colours, from
%   left to right.

colour_image(Colours, Image) :-
    length(Colours, N),
    W is N*10,
    new(Image, image(@nil, W, 10, pixmap)),
    forall(nth0(I, Colours, C),
           ( X is I*10,
             send(Image, fill, colour(C), area(X, 0, 10, 10)) )).

name_of(Image, X, Y, Name) :-
    get(Image, pixel, X, Y, Colour),
    get(Colour, red, R), get(Colour, green, G), get(Colour, blue, B),
    rgb_name(R, G, B, Name).

rgb_name(R, G, B, Name) :-
    (   nearest(R-G-B, Name)
    ->  true
    ;   Name = rgb(R,G,B)
    ).

nearest(R-G-B, Name) :-
    member(Name-(R0-G0-B0),
           [ red-(255-0-0), green-(0-255-0), blue-(0-0-255),
             yellow-(255-255-0), white-(255-255-255), black-(0-0-0) ]),
    abs(R-R0) < 16, abs(G-G0) < 16, abs(B-B0) < 16,
    !.

%!  sdl_window(+Size, -Window) is det.
%
%   A blue sdl_window of Size x Size in a frame.

sdl_window(Size, W) :-
    new(W, sdl_window),
    send(W, size, size(Size, Size)),
    send(W, background, colour(blue)).

%!  pixels(+Window, +Points, -Names) is det.
%
%   Colours of Points (X-Y in window coordinates) as drawn by the
%   renderer and as drawn by Cairo for frame <-image.  Raises an
%   assertion error if the two differ.  Blended colours may differ by
%   rounding, e.g., Metal draws blue at 0.5 alpha as 127 where Cairo
%   draws 128.

pixels(W, Points, Names) :-
    send(W, open),
    get(W, frame, F),
    get(W?area, x, OX), get(W?area, y, OY),
    get(F, sdl_test_image, GPU),
    get(F, image, Cairo),
    maplist(pixel(GPU, OX, OY), Points, Names),
    maplist(pixel(Cairo, OX, OY), Points, CairoNames),
    send(F, destroy),
    assertion(maplist(same_pixel, Names, CairoNames)).

same_pixel(rgb(R1,G1,B1), rgb(R2,G2,B2)) =>
    abs(R1-R2) =< 2, abs(G1-G2) =< 2, abs(B1-B2) =< 2.
same_pixel(Name1, Name2) =>
    Name1 == Name2.

pixel(Image, OX, OY, X-Y, Name) :-
    IX is OX+X, IY is OY+Y,
    name_of(Image, IX, IY, Name).

filled_box(Size, Colour, Box) :-
    new(Box, box(Size, Size)),
    send(Box, pen, 0),
    send(Box, fill, Colour).

%!  round8(+RGB0, -RGB) is det.
%
%   Round the components of an interpolated colour to multiples of 8.

round8(rgb(R0,G0,B0), rgb(R,G,B)) :-
    round_to_8(R0, R), round_to_8(G0, G), round_to_8(B0, B).

round_to_8(V0, V) :-
    V is min(255, round(V0/8)*8).

:- begin_tests(sdl_texture).

test(empty, [W-H-Format == 0-0-(@nil)]) :-
    new(T, sdl_texture),
    get(T, size, size(W, H)),
    get(T, format, Format),
    free(T).
test(from_image, [W-H-Format-Name == 20-10-argb8888-green]) :-
    colour_image([red,green], I),
    new(T, sdl_texture(I)),
    get(T, size, size(W, H)),
    get(T, format, Format),
    get(T, image, I2),
    name_of(I2, 15, 5, Name),
    free(T).
test(from_size, [W-H-Alpha == 8-4-0]) :-
    new(T, sdl_texture(size(8,4))),
    get(T, size, size(W, H)),
    get(T?image, pixel, 1, 1, C),
    get(C, alpha, Alpha),
    free(T).
test(replace_image, [W-H-Name == 10-10-blue]) :-
    colour_image([red,green], I),
    new(T, sdl_texture(I)),
    colour_image([blue], I2),
    send(T, image, I2),
    get(T, size, size(W, H)),
    name_of(T?image, 5, 5, Name),
    free(T).

:- end_tests(sdl_texture).

:- begin_tests(sdl_texture_image).

rgba(Image, X, Y, rgba(R,G,B,A)) :-
    get(Image, pixel, X, Y, C),
    get(C, red, R), get(C, green, G), get(C, blue, B), get(C, alpha, A).

test(copy_into_image, [Same-W-H-Name == true-20-10-green]) :-
    colour_image([red,green], I0),
    new(T, sdl_texture(I0)),
    new(I, image(@nil, 4, 4, pixmap)),
    send(I, copy, T),
    get(I, size, size(W, H)),
    name_of(I, 15, 5, Name),
    get(I, self, I1),
    (I1 == I -> Same = true ; Same = false),
    free(T).
test(copy_resizes_bitmap, [W-H == 20-10]) :-
    colour_image([red,green], I0),
    new(T, sdl_texture(I0)),
    new(I, image(@nil, 4, 4, pixmap)),
    new(B, bitmap(I)),
    send(I, copy, T),
    get(B, size, size(W, H)),
    free(T).
test(copy_empty, fail) :-
    new(T, sdl_texture),
    new(I, image(@nil, 4, 4, pixmap)),
    send(I, copy, T).
test(translucent_round_trip, [RGBA1-RGBA2 == rgba(255,0,0,128)-RGBA1]) :-
    new(T, sdl_texture(size(4, 4))),
    filled_box(4, colour(@default, 255, 0, 0, 128), Box),
    send(T, draw_in, Box, point(0, 0)),
    get(T, image, I1),
    rgba(I1, 1, 1, RGBA1),
    new(T2, sdl_texture(I1)),           % and back via an image
    get(T2, image, I2),
    rgba(I2, 1, 1, RGBA2).
test(draw_in, [In-Out == rgba(255,0,0,255)-rgba(0,0,0,0)]) :-
    new(T, sdl_texture(size(20, 20))),
    filled_box(10, colour(red), Box),
    send(T, draw_in, Box, point(5, 5)),
    get(T, image, I),
    rgba(I, 8, 8, In),
    rgba(I, 2, 2, Out).
test(draw_in_rendered, [Names == [blue, red]]) :-
    new(T, sdl_texture(size(20, 20))),  % transparent where nothing is drawn
    filled_box(10, colour(red), Box),
    send(T, draw_in, Box, point(10, 0)),
    sdl_window(40, W),
    send(W, append, sdl_layer(T, area(0,0,20,20), stretch)),
    pixels(W, [5-5, 15-5], Names).
test(draw_in_converts, [Before-After-Names == yuy2-argb8888-[red, blue]]) :-
    new(C, camera),                     % YUV pixels become ARGB
    send(C, texture, new(T, sdl_texture)),
    send(C, sdl_test_frame, 8, 8, colour(blue)),
    get(T, format, Before),
    filled_box(4, colour(red), Box),
    send(T, draw_in, Box, point(0, 0)),
    get(T, format, After),
    get(T, image, I),
    name_of(I, 1, 1, N1),
    name_of(I, 6, 6, N2),
    Names = [N1, N2].

test(draw_in_twice, [RGBA == rgba(255,0,0,128)]) :-
    new(T, sdl_texture(size(8, 8))),    % drawing again keeps the pixels
    filled_box(4, colour(@default, 255, 0, 0, 128), Box1),
    send(T, draw_in, Box1, point(0, 0)),
    filled_box(2, colour(green), Box2),
    send(T, draw_in, Box2, point(6, 6)),
    get(T, image, I),
    rgba(I, 1, 1, RGBA).
test(translucent_rendered, [RGB == rgb(128,0,128)]) :-
    new(T, sdl_texture(size(10, 10))),  % half red over blue
    filled_box(10, colour(@default, 255, 0, 0, 128), Box),
    send(T, draw_in, Box, point(0, 0)),
    sdl_window(40, W),
    send(W, append, sdl_layer(T, @default, stretch)),
    send(W, open),
    get(W?frame, sdl_test_image, I),
    get(W?area, x, X0), get(W?area, y, Y0),
    X is X0+20, Y is Y0+20,
    get(I, pixel, X, Y, C),
    get(C, red, R0), get(C, green, G0), get(C, blue, B0),
    R is round(R0/8)*8, G is round(G0/8)*8, B is round(B0/8)*8,
    RGB = rgb(R,G,B),
    send(W?frame, destroy).

:- end_tests(sdl_texture_image).

:- begin_tests(sdl_layer).

test(defaults, [Scale-Flip-Win == fit-none-(@nil), Alpha =:= 1]) :-
    new(L, sdl_layer),
    get(L, scale, Scale),
    get(L, alpha, Alpha),
    get(L, flip, Flip),
    get(L, device, Win).
test(append_delete, [Before-W1-After-W2 == [L]-W-[]-(@nil)]) :-
    sdl_window(20, W),
    new(L, sdl_layer),
    send(W, append, L),
    get(L, device, W1),
    chain_list(W?layers, Before),
    send(W, delete, L),
    chain_list(W?layers, After),
    get(L, device, W2),
    free(W).
test(move_to_other_window, [L1-L2 == []-[L]]) :-
    sdl_window(20, W1),
    sdl_window(20, W2),
    new(L, sdl_layer),
    send(W1, append, L),
    send(W2, append, L),
    chain_list(W1?layers, L1),
    chain_list(W2?layers, L2),
    free(W1), free(W2).
test(free_layer, [Layers == []]) :-
    sdl_window(20, W),
    send(W, append, new(L, sdl_layer)),
    free(L),
    chain_list(W?layers, Layers),
    free(W).
test(stacking, [Top-Bottom-Above-Below == [B,C,A]-[A,B,C]-[A,C,B]-[C,A,B]]) :-
    sdl_window(20, W),
    send_list(W, append, [new(A, sdl_layer), new(B, sdl_layer),
                          new(C, sdl_layer)]),
    send(A, expose),
    chain_list(W?layers, Top),
    send(A, hide),
    chain_list(W?layers, Bottom),
    send(B, expose, C),
    chain_list(W?layers, Above),
    send(C, hide, A),
    chain_list(W?layers, Below),
    free(W).
test(stacking_not_shown, fail) :-
    new(L, sdl_layer),
    send(L, expose).
test(free_window, [Win == @nil]) :-
    sdl_window(20, W),
    send(W, append, new(L, sdl_layer)),
    free(W),
    get(L, device, Win).
test(background, [Names == [blue]]) :-
    sdl_window(40, W),
    pixels(W, [20-20], Names).
test(stretch, [Names == [red, green, blue]]) :-
    colour_image([red,green], I),
    new(T, sdl_texture(I)),
    sdl_window(60, W),
    send(W, append, sdl_layer(T, area(0,0,40,40), stretch)),
    pixels(W, [10-30, 30-30, 50-30], Names).
test(fit, [Names == [blue, red, green, blue]]) :-
    colour_image([red,green], I),       % 20x10 in 60x60: 60x30, centred
    new(T, sdl_texture(I)),
    sdl_window(60, W),
    send(W, append, sdl_layer(T)),
    pixels(W, [10-5, 10-30, 50-30, 50-55], Names).
test(fill, [Names == [green, green, green]]) :-
    colour_image([red,green,blue], I),  % 30x10 in 60x60: middle 10x10
    new(T, sdl_texture(I)),
    sdl_window(60, W),
    send(W, append, sdl_layer(T, @default, fill)),
    pixels(W, [5-5, 30-30, 55-55], Names).
test(flip, [Names == [green, red]]) :-
    colour_image([red,green], I),
    new(T, sdl_texture(I)),
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, area(0,0,40,20), stretch))),
    send(L, flip, horizontal),
    pixels(W, [10-10, 30-10], Names).
test(rotate, [Names == [red, green, blue]]) :-
    colour_image([red,green], I),       % 40x20 at 10,20 turned 90 degrees
    new(T, sdl_texture(I)),
    sdl_window(60, W),
    send(W, append, new(L, sdl_layer(T, area(10,20,40,20), stretch))),
    send(L, angle, 90),
    pixels(W, [30-15, 30-45, 5-30], Names).
test(source, [Names == [green, green]]) :-
    colour_image([red,green], I),
    new(T, sdl_texture(I)),
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, area(0,0,40,20), stretch))),
    send(L, source, area(10,0,10,10)),
    pixels(W, [5-10, 35-10], Names).
test(hidden, [Names == [blue]]) :-
    colour_image([red], I),
    new(T, sdl_texture(I)),
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, @default, stretch))),
    send(L, displayed, @off),
    pixels(W, [20-20], Names).
test(order, [Names == [green]]) :-
    colour_image([red], I1),
    colour_image([green], I2),
    sdl_window(40, W),
    send(W, append, sdl_layer(sdl_texture(I1), @default, stretch)),
    send(W, append, sdl_layer(sdl_texture(I2), @default, stretch)),
    pixels(W, [20-20], Names).
test(alpha, [RGB == rgb(128,128,0)]) :-
    colour_image([red], I1),            % premultiplied green at 0.5
    colour_image([green], I2),          % over red
    sdl_window(40, W),
    send(W, append, sdl_layer(sdl_texture(I1), @default, stretch)),
    send(W, append, new(L, sdl_layer(sdl_texture(I2), @default, stretch))),
    send(L, alpha, 0.5),
    pixels(W, [20-20], [RGB0]),
    round8(RGB0, RGB).
test(alpha_no_alpha_channel, [RGB == rgb(128,0,128)]) :-
    colour_image([red], I1),            % YUV camera frame at 0.5
    new(C, camera),
    send(C, texture, new(T, sdl_texture)),
    send(C, sdl_test_frame, 8, 8, colour(blue)),
    sdl_window(40, W),
    send(W, append, sdl_layer(sdl_texture(I1), @default, stretch)),
    send(W, append, new(L, sdl_layer(T, @default, stretch))),
    send(L, alpha, 0.5),
    pixels(W, [20-20], [RGB0]),
    round8(RGB0, RGB).
test(changed, [Before-After == red-green]) :-
    colour_image([red], I1),
    new(T, sdl_texture(I1)),
    sdl_window(40, W),
    send(W, append, sdl_layer(T, @default, stretch)),
    send(W, open),
    get(W, frame, F),
    get(W?area, x, X0), get(W?area, y, Y0),
    X is X0+20, Y is Y0+20,
    get(F, sdl_test_image, GPU1),
    name_of(GPU1, X, Y, Before),
    colour_image([green], I2),
    send(T, image, I2),
    get(F, sdl_test_image, GPU2),
    name_of(GPU2, X, Y, After),
    send(F, destroy).

%!  two_layers(+Below, +Above, +Blend, -Window) is det.
%
%   A 40x20 window showing the colour blocks Above over Below, both
%   stretched over the whole window.  Above is combined using Blend.

two_layers(Below, Above, Blend, W) :-
    colour_image(Below, I1),
    colour_image(Above, I2),
    new(W, sdl_window),
    send(W, size, size(40, 20)),
    send(W, append, sdl_layer(sdl_texture(I1), @default, stretch)),
    send(W, append, new(L, sdl_layer(sdl_texture(I2), @default, stretch))),
    send(L, blend, Blend).

test(blend_default, [Blend == blend]) :-
    new(L, sdl_layer),
    get(L, blend, Blend).
test(blend_add, [Names == [yellow, green]]) :-
    two_layers([red, green], [green, black], add, W),
    pixels(W, [10-10, 30-10], Names).
test(blend_mod, [Names == [red, black]]) :-
    two_layers([red, green], [white, black], mod, W),
    pixels(W, [10-10, 30-10], Names).
test(blend_mul, [Names == [red, black]]) :-
    two_layers([red, green], [white, black], mul, W),
    pixels(W, [10-10, 30-10], Names).
test(blend_none_ignores_alpha, [Names == [green]]) :-
    two_layers([red], [green], none, W),
    get(W?layers, tail, L),
    send(L, alpha, 0.5),
    pixels(W, [20-10], Names).
test(blend_add_alpha, [RGB == rgb(255,128,0)]) :-
    two_layers([red], [green], add, W),
    get(W?layers, tail, L),
    send(L, alpha, 0.5),
    pixels(W, [20-10], [RGB0]),
    round8(RGB0, RGB).

test(tint, [Names == [red, black]]) :-
    two_layers([blue], [white, green], blend, W),
    get(W?layers, tail, L),
    send(L, tint, colour(red)),
    pixels(W, [10-10, 30-10], Names).
test(tint_alpha, [RGB == rgb(128,0,128)]) :-
    two_layers([blue], [white], blend, W),
    get(W?layers, tail, L),
    send(L, tint, colour(red)),
    send(L, alpha, 0.5),
    pixels(W, [20-10], [RGB0]),
    round8(RGB0, RGB).
test(tint_camera, [RGB == rgb(0,0,128)]) :-
    new(C, camera),                     % no alpha, not premultiplied
    send(C, texture, new(T, sdl_texture)),
    send(C, sdl_test_frame, 8, 8, colour(blue)),
    sdl_window(40, W),
    send(W, background, colour(black)),
    send(W, append, new(L, sdl_layer(T, @default, stretch))),
    send(L, tint, colour(@default, 128, 128, 128)),
    pixels(W, [20-20], [RGB0]),
    round8(RGB0, RGB).

%!  scaled_up(+ScaleMode, -Window) is det.
%
%   A red|blue texture of 20x10 stretched to 200x20: the edge between
%   the colours is at x=100.

scaled_up(Mode, W) :-
    colour_image([red, blue], I),
    new(W, sdl_window),
    send(W, size, size(200, 20)),
    send(W, append, new(L, sdl_layer(sdl_texture(I), @default, stretch))),
    send(L, scale_mode, Mode).

test(scale_mode_default, [Mode == linear]) :-
    new(L, sdl_layer),
    get(L, scale_mode, Mode).
test(scale_mode_nearest, [Names == [red, blue]]) :-
    scaled_up(nearest, W),
    pixels(W, [98-10, 101-10], Names).
test(scale_mode_pixelart, [Names == [red, blue]]) :-
    scaled_up(pixelart, W),
    pixels(W, [97-10, 102-10], Names).
test(scale_mode_linear, true(Name \== red)) :-
    scaled_up(linear, W),               % compare only the renderer:
    send(W, open),                      % Cairo filters slightly different
    get(W, frame, F),
    get(W?area, x, X0), get(W?area, y, Y0),
    X is X0+98, Y is Y0+10,
    get(F, sdl_test_image, GPU),
    name_of(GPU, X, Y, Name),
    send(F, destroy).

%!  tiled(+Area, -Layer, -Window) is det.
%
%   A 60x60 window showing a red|green texture of 20x10 tiled over
%   Area.

tiled(Area, L, W) :-
    colour_image([red, green], I),
    sdl_window(60, W),
    send(W, append, new(L, sdl_layer(sdl_texture(I), Area, tile))).

test(tile, [Names == [red, green, red, green, red, blue]]) :-
    tiled(area(0,0,60,20), _, W),
    pixels(W, [5-5, 15-5, 25-5, 35-15, 45-15, 30-30], Names).
test(tile_offset, [Names == [green, red, green]]) :-
    tiled(area(0,0,60,20), L, W),       % first tile at -15
    send(L, offset, point(5, 0)),
    pixels(W, [2-5, 8-5, 18-5], Names).
test(tile_negative_offset, [Names == [red, green]]) :-
    tiled(area(0,0,60,20), L, W),       % first tile at -5: red until 5
    send(L, offset, point(-5, 0)),
    pixels(W, [2-5, 8-5], Names).
test(tile_source, [Names == [green, green, green]]) :-
    tiled(area(0,0,60,20), L, W),
    send(L, source, area(10, 0, 10, 10)),
    pixels(W, [5-5, 15-5, 55-15], Names).
test(tile_zoom, [Names == [red, green, red, blue]]) :-
    tiled(area(0,0,30,10), L, W),       % tiles of 40x20
    new(Tr, transform),
    send(Tr, set, 2, 0, 0, 2, 0, 0),
    send(L, transform, Tr),
    pixels(W, [10-10, 30-10, 50-10, 30-30], Names).
test(tile_flip, [Names == [red, green, red]]) :-
    tiled(area(0,0,25,10), L, W),       % unflipped: red, red, green
    send(L, flip, horizontal),
    pixels(W, [3-5, 8-5, 18-5], Names).
test(tile_rotate, [Names == [red, green, red, green, blue]]) :-
    tiled(area(0,0,60,20), L, W),       % around 30,10: x becomes y
    send(L, angle, 90),
    pixels(W, [30-5, 30-15, 30-25, 30-35, 5-10], Names).

%!  grid_image(+Rows, -Image) is det.
%
%   An image of 10x10 pixel blocks, Rows is a list of rows of colours.

grid_image(Rows, Image) :-
    Rows = [Row|_],
    length(Rows, NR), length(Row, NC),
    W is NC*10, H is NR*10,
    new(Image, image(@nil, W, H, pixmap)),
    forall(( nth0(J, Rows, Cols), nth0(I, Cols, C) ),
           ( X is I*10, Y is J*10,
             send(Image, fill, colour(C), area(X, Y, 10, 10)) )).

%!  sliced(+Rows, +Slices, +Area, +Scale, -Layer, -Window) is det.

sliced(Rows, Slices, Area, Scale, L, W) :-
    grid_image(Rows, I),
    sdl_window(100, W),
    send(W, append, new(L, sdl_layer(sdl_texture(I), Area, Scale))),
    send(L, scale_mode, nearest),
    send(L, slices, Slices).

frame_rows([[red, green, red], [green, white, green], [red, green, red]]).

test(slices, [Names == [red, green, green, red, green, white, red, blue]]) :-
    frame_rows(Rows),                   % corners stay 10x10
    sliced(Rows, area(10,10,10,10), area(0,0,100,60), stretch, _, W),
    pixels(W, [5-5, 15-5, 50-5, 95-5, 5-30, 50-30, 95-55, 50-70], Names).
test(slices_shrink, [Names == [red, red, red, blue]]) :-
    frame_rows(Rows),                   % no room: corners of 5x5
    sliced(Rows, area(10,10,10,10), area(0,0,10,10), stretch, _, W),
    pixels(W, [2-2, 4-4, 6-6, 15-15], Names).
test(slices_ignored_by_fit, [Names == [red, red]]) :-
    frame_rows(Rows),                   % fit: 60x60 at 20,0
    sliced(Rows, area(10,10,10,10), area(0,0,100,60), fit, _, W),
    pixels(W, [25-5, 35-15], Names).
test(slices_stretch, [Names == [red, green, green, green, blue, red]]) :-
    sliced([[red, green, blue, red]], area(10,0,20,10),
           area(0,0,100,20), stretch, _, W),
    pixels(W, [5-5, 15-5, 25-5, 35-15, 85-5, 95-15], Names).
test(slices_tile, [Names == [red, green, blue, green, blue, red]]) :-
    sliced([[red, green, blue, red]], area(10,0,20,10),
           area(0,0,100,20), tile, _, W),
    pixels(W, [5-5, 15-5, 25-5, 35-15, 85-5, 95-15], Names).
test(slices_rotate, [Names == [red, green, green, red, white]]) :-
    frame_rows(Rows),                   % 100x60 turned around 50,30:
    sliced(Rows, area(10,10,10,10), area(0,0,100,60), stretch, L, W),
    send(L, angle, 90),                 % X,Y shows 50+(Y-30),30-(X-50)
    pixels(W, [75-75, 75-65, 65-75, 25-75, 50-30], Names).

:- end_tests(sdl_layer).

%   Sprites: textures placed by their hot spot and mapped through a
%   transform, as figure ->transform does for graphicals.

:- begin_tests(sdl_sprite).

%!  red_green(-Texture) is det.
%
%   A 20x10 texture, red left and green right, with its hot spot in the
%   middle of the left edge.

red_green(T) :-
    colour_image([red,green], I),
    new(T, sdl_texture(I)),
    send(T, hot_spot, point(0, 5)).

%!  place(+X, +Y, +Angle, -Transform) is det.
%
%   Transform that rotates by Angle degrees and moves the origin to
%   X,Y, as the Asteroids demo places its figures.

place(X, Y, Angle, T) :-
    Rad is Angle*pi/180,
    C is cos(Rad), S is sin(Rad), NS is -S,
    new(T, transform),
    send(T, set, C, NS, S, C, X, Y).

test(hot_spot_placement, [Names == [blue, red, green, blue]]) :-
    red_green(T),                       % hot spot at 10,20
    sdl_window(40, W),
    send(W, append, sdl_layer(T, area(10, 20, 0, 0), none)),
    pixels(W, [5-20, 15-20, 25-20, 15-10], Names).
test(transform, [Names == [red, green, blue, blue]]) :-
    red_green(T),                       % turned 90 degrees at 30,10
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, @nil, none))),
    place(30, 10, 90, Tr),
    send(L, transform, Tr),
    pixels(W, [30-15, 30-25, 30-35, 20-15], Names).
test(angle_around_hot_spot, [Names == [red, green, blue]]) :-
    red_green(T),                       % as above, using <-angle
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, area(30, 10, 0, 0), none))),
    send(L, angle, 90),
    pixels(W, [30-15, 30-25, 20-15], Names).
test(zoom, [Names == [red, green, blue]]) :-
    red_green(T),                       % twice as large: 40x20
    sdl_window(60, W),
    send(W, append, new(L, sdl_layer(T, @nil, none))),
    new(Tr, transform),
    send(Tr, set, 2, 0, 0, 2, 10, 30),
    send(L, transform, Tr),
    pixels(W, [15-30, 45-30, 55-30], Names).
test(shear, [Names == [red, green, red, red, blue]]) :-
    red_green(T),                       % lines of the texture slant
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, @nil, none))),
    new(Tr, transform),
    send(Tr, set, 1, 0.5, 0, 1, 10, 10),
    send(L, transform, Tr),
    pixels(W, [15-10, 25-10, 21-14, 9-6, 5-10], Names).
test(translucent_shear, [RGB == rgb(128,0,128)]) :-
    new(T, sdl_texture(size(10, 10))), % drawn with triangles
    filled_box(10, colour(@default, 255, 0, 0, 128), Box),
    send(T, draw_in, Box, point(0, 0)),
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, @nil, none))),
    new(Tr, transform),
    send(Tr, set, 2, 0.5, 0, 2, 10, 10),
    send(L, transform, Tr),
    pixels(W, [20-20], [RGB0]),
    round8(RGB0, RGB).
test(singular_transform, [Names == [green]]) :-
    red_green(T),                       % shows nothing, but the next
    colour_image([green], I),           % layer is drawn
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, @nil, none))),
    new(Tr, transform),
    send(Tr, set, 0, 0, 0, 0, 10, 10),
    send(L, transform, Tr),
    send(W, append, sdl_layer(sdl_texture(I), @default, stretch)),
    pixels(W, [20-20], Names).
test(from_graphical, [W-H-HX-HY == 20-20-10-10]) :-
    new(Fig, figure),                   % a circle around the origin
    send(Fig, display, new(C, circle(20)), point(-10, -10)),
    send(C, pen, 0),
    send(C, fill, colour(red)),
    new(T, sdl_texture(Fig)),
    get(T, size, size(W, H)),
    get(T, hot_spot, point(HX, HY)).
test(graphical_sprite, [Names == [red, blue, blue]]) :-
    new(Fig, figure),                   % centred on its origin at 20,20
    send(Fig, display, new(C, circle(10)), point(-5, -5)),
    send(C, pen, 0),
    send(C, fill, colour(red)),
    new(T, sdl_texture(Fig)),
    sdl_window(40, W),
    send(W, append, new(L, sdl_layer(T, @nil, none))),
    place(20, 20, 45, Tr),
    send(L, transform, Tr),
    pixels(W, [20-20, 10-10, 30-30], Names).
test(graphical_no_origin, [HS == @nil]) :-
    new(B, box(10, 10)),                % (0,0) is outside the box
    send(B, position, point(20, 20)),
    new(T, sdl_texture(B)),
    get(T, hot_spot, HS).

:- end_tests(sdl_sprite).


%   Meshes: triangles drawn using SDL_RenderGeometry().  Without a
%   texture, the vertices have colours.  Texture coordinates are in
%   pixels of the texture.

:- begin_tests(sdl_mesh).

%!  mesh_window(+Vertices, +Indices, +Texture, -Layer, -Window) is det.
%
%   A blue 60x60 window showing a mesh.

mesh_window(Vertices, Indices, Texture, L, W) :-
    new(M, sdl_mesh),
    pce_sdl_mesh_set(M, Vertices, Indices),
    sdl_window(60, W),
    send(W, append, new(L, sdl_layer(Texture))),
    send(L, mesh, M).

%!  quad(+X, +Y, +W, +H, +UV, +Colour, -Vertices, -Indices) is det.
%
%   A rectangle as two triangles, showing the part UV = U0-V0-U1-V1 of
%   the texture.

quad(X, Y, W, H, U0-V0-U1-V1, C,
     [ v(X,Y,U0,V0,C), v(X2,Y,U1,V0,C), v(X2,Y2,U1,V1,C), v(X,Y2,U0,V1,C) ],
     [0,1,2, 0,2,3]) :-
    X2 is X+W, Y2 is Y+H.

red_green_texture(T) :-
    colour_image([red, green], I),
    new(T, sdl_texture(I)).

%!  rgb_pixels(+Window, +Points, -GPU, -Cairo) is det.
%
%   As pixels/3, but give the rgb(R,G,B) of the renderer and Cairo, for
%   interpolated colours that need not be exactly the same.

rgb_pixels(W, Points, GPU, Cairo) :-
    send(W, open),
    get(W, frame, F),
    get(W?area, x, OX), get(W?area, y, OY),
    get(F, sdl_test_image, GI),
    get(F, image, CI),
    maplist(rgb_at(GI, OX, OY), Points, GPU),
    maplist(rgb_at(CI, OX, OY), Points, Cairo),
    send(F, destroy).

rgb_at(Image, OX, OY, X-Y, rgb(R,G,B)) :-
    IX is OX+X, IY is OY+Y,
    get(Image, pixel, IX, IY, C),
    get(C, red, R), get(C, green, G), get(C, blue, B).

near(rgb(R1,G1,B1), rgb(R2,G2,B2)) :-
    abs(R1-R2) =< 24, abs(G1-G2) =< 24, abs(B1-B2) =< 24.

test(empty, [N == 0]) :-
    new(M, sdl_mesh),
    get(M, count, N).
test(append, [Names == [red, red, blue]]) :-
    new(M, sdl_mesh),
    forall(member(X-Y, [0-0, 40-0, 40-20, 0-20]),
           send(M, append, point(X, Y), @default, colour(red))),
    send(M, triangle, 0, 1, 2),
    send(M, triangle, 0, 2, 3),
    sdl_window(60, W),
    send(W, append, new(L, sdl_layer)),
    send(L, mesh, M),
    pixels(W, [5-15, 35-5, 50-50], Names).
test(count, [N == 4]) :-
    quad(0, 0, 10, 10, 0-0-0-0, rgba(255,0,0,255), Vs, Is),
    new(M, sdl_mesh),
    pce_sdl_mesh_set(M, Vs, Is),
    get(M, count, N).
test(no_indices, [Names == [green, blue]]) :-
    mesh_window([ v(0,0,0,0,rgba(0,255,0,255)), v(60,0,0,0,rgba(0,255,0,255)),
                  v(0,60,0,0,rgba(0,255,0,255)) ], [], @nil, _, W),
    pixels(W, [10-10, 50-50], Names).
test(gradient, [true(maplist(near, GPU, [rgb(255,0,0), rgb(85,85,85)])),
                true(maplist(near, Cairo, GPU))]) :-
    mesh_window([ v(0,0,0,0,rgba(255,0,0,255)), v(60,0,0,0,rgba(0,255,0,255)),
                  v(0,60,0,0,rgba(0,0,255,255)) ], [], @nil, _, W),
    rgb_pixels(W, [0-0, 20-20], GPU, Cairo).
test(textured, [Names == [red, green, blue]]) :-
    red_green_texture(T),
    quad(0, 0, 40, 20, 0-0-20-10, rgba(255,255,255,255), Vs, Is),
    mesh_window(Vs, Is, T, _, W),
    pixels(W, [10-10, 30-10, 50-50], Names).
test(uv_in_pixels, [Names == [red, red]]) :-
    red_green_texture(T),               % only the red half
    quad(0, 0, 40, 20, 0-0-10-10, rgba(255,255,255,255), Vs, Is),
    mesh_window(Vs, Is, T, _, W),
    pixels(W, [10-10, 30-10], Names).
test(vertex_colour_on_texture, [Names == [red, black]]) :-
    red_green_texture(T),               % multiplied by red
    quad(0, 0, 40, 20, 0-0-20-10, rgba(255,0,0,255), Vs, Is),
    mesh_window(Vs, Is, T, _, W),
    send(W, background, colour(black)),
    pixels(W, [10-10, 30-10], Names).
test(translucent_texture, [RGB == rgb(128,0,128)]) :-
    new(T, sdl_texture(size(10, 10))), % premultiplied half red
    filled_box(10, colour(@default, 255, 0, 0, 128), Box),
    send(T, draw_in, Box, point(0, 0)),
    quad(0, 0, 60, 60, 0-0-10-10, rgba(255,255,255,255), Vs, Is),
    mesh_window(Vs, Is, T, _, W),
    pixels(W, [30-30], [RGB0]),
    round8(RGB0, RGB).
test(vertex_alpha, [RGB == rgb(128,0,128)]) :-
    red_green_texture(T),
    quad(0, 0, 60, 60, 0-0-10-10, rgba(255,255,255,128), Vs, Is),
    mesh_window(Vs, Is, T, _, W),
    pixels(W, [30-30], [RGB0]),
    round8(RGB0, RGB).
test(layer_alpha, [RGB == rgb(128,0,128)]) :-
    quad(0, 0, 60, 60, 0-0-0-0, rgba(255,0,0,255), Vs, Is),
    mesh_window(Vs, Is, @nil, L, W),
    send(L, alpha, 0.5),
    pixels(W, [30-30], [RGB0]),
    round8(RGB0, RGB).
test(tint, [Names == [red]]) :-
    quad(0, 0, 60, 60, 0-0-0-0, rgba(255,255,255,255), Vs, Is),
    mesh_window(Vs, Is, @nil, L, W),
    send(W, background, colour(black)),
    send(L, tint, colour(red)),
    pixels(W, [30-30], Names).
test(blend_add, [Names == [rgb(255,0,255)]]) :-
    quad(0, 0, 60, 60, 0-0-0-0, rgba(255,0,0,255), Vs, Is),
    mesh_window(Vs, Is, @nil, L, W),
    send(L, blend, add),
    pixels(W, [30-30], Names).
test(transform, [Names == [blue, red]]) :-
    quad(0, 0, 20, 20, 0-0-0-0, rgba(255,0,0,255), Vs, Is),
    mesh_window(Vs, Is, @nil, L, W),
    new(Tr, transform),
    send(Tr, set, 1, 0, 0, 1, 30, 30),
    send(L, transform, Tr),
    pixels(W, [10-10, 40-40], Names).
test(changed, [Before-After == red-green]) :-
    quad(0, 0, 60, 60, 0-0-0-0, rgba(255,0,0,255), Vs, Is),
    mesh_window(Vs, Is, @nil, L, W),
    send(W, open),
    get(W, frame, F),
    get(W?area, x, X0), get(W?area, y, Y0),
    X is X0+30, Y is Y0+30,
    get(F, sdl_test_image, GPU1),
    name_of(GPU1, X, Y, Before),
    quad(0, 0, 60, 60, 0-0-0-0, rgba(0,255,0,255), Vs2, Is2),
    get(L, mesh, M),
    pce_sdl_mesh_set(M, Vs2, Is2),
    get(F, sdl_test_image, GPU2),
    name_of(GPU2, X, Y, After),
    send(F, destroy).
test(clear, [Names == [blue]]) :-
    quad(0, 0, 60, 60, 0-0-0-0, rgba(255,0,0,255), Vs, Is),
    mesh_window(Vs, Is, @nil, L, W),
    get(L, mesh, M),
    send(M, clear),
    pixels(W, [30-30], Names).
test(free_mesh, [Mesh == @nil]) :-
    new(M, sdl_mesh),
    new(L, sdl_layer),
    send(L, mesh, M),
    free(M),
    get(L, mesh, Mesh).
test(layers, [Before-After == [L]-[]]) :-
    new(M, sdl_mesh),
    new(L, sdl_layer),
    send(L, mesh, M),
    chain_list(M?layers, Before),
    send(L, mesh, @nil),
    chain_list(M?layers, After).
test(bad_index, error(domain_error(vertex_indices, _))) :-
    new(M, sdl_mesh),
    pce_sdl_mesh_set(M, [v(0,0), v(1,0), v(0,1)], [0,1,3]).
test(not_triangles, error(domain_error(triangles, _))) :-
    new(M, sdl_mesh),
    pce_sdl_mesh_set(M, [v(0,0), v(1,0)], []).
test(not_a_mesh, error(type_error(sdl_mesh, _))) :-
    new(B, box),
    pce_sdl_mesh_set(B, [], []).
test(bad_vertex, error(type_error(mesh_vertex, _))) :-
    new(M, sdl_mesh),
    pce_sdl_mesh_set(M, [p(0,0), v(1,0), v(0,1)], []).
test(triangle_bad_index, fail) :-
    new(M, sdl_mesh),
    send(M, append, point(0, 0)),
    catch(send(M, triangle, 0, 0, 1), _, fail).

:- end_tests(sdl_mesh).


%   Canvases: layers drawn into an off-screen texture that a layer
%   shows.  pixels/3 compares the renderer with Cairo, which reads the
%   canvas back from the renderer.

:- begin_tests(sdl_canvas).

%!  canvas_window(+Canvas, -Window) is det.
%
%   A blue 40x40 window showing Canvas stretched over the window.

canvas_window(C, W) :-
    sdl_window(40, W),
    send(W, append, sdl_layer(C, @default, stretch)).

%!  colour_layer(+Colours, -Layer) is det.
%
%   A layer showing colour blocks, stretched over its device.

colour_layer(Colours, L) :-
    colour_image(Colours, I),
    new(L, sdl_layer(sdl_texture(I), @default, stretch)).

frame_pixels(W, Points, Names) :-
    get(W, frame, F),
    get(W?area, x, OX), get(W?area, y, OY),
    get(F, sdl_test_image, GPU),
    maplist(pixel(GPU, OX, OY), Points, Names).

test(defaults, [Layers-Clear-BG == []-(@on)-(@nil)]) :-
    new(C, sdl_canvas(size(10, 10))),
    chain_list(C?layers, Layers),
    get(C, clear, Clear),
    get(C, background, BG).
test(device, [Dev == C]) :-
    new(C, sdl_canvas(size(10, 10))),
    send(C, append, new(L, sdl_layer)),
    get(L, device, Dev).
test(move_from_window, [WL-CL == []-[L]]) :-
    sdl_window(20, W),
    new(C, sdl_canvas(size(10, 10))),
    send(W, append, new(L, sdl_layer)),
    send(C, append, L),
    chain_list(W?layers, WL),
    chain_list(C?layers, CL),
    free(W).
test(stacking, [Layers == [B, A]]) :-
    new(C, sdl_canvas(size(10, 10))),
    send(C, append, new(A, sdl_layer)),
    send(C, append, new(B, sdl_layer)),
    send(A, expose),
    chain_list(C?layers, Layers).
test(free_canvas, [Dev == @nil]) :-
    new(C, sdl_canvas(size(10, 10))),
    send(C, append, new(L, sdl_layer)),
    free(C),
    get(L, device, Dev).
test(shows_layers, [Names == [red, green]]) :-
    new(C, sdl_canvas(size(20, 10))),
    colour_layer([red, green], L),
    send(C, append, L),
    canvas_window(C, W),
    pixels(W, [10-20, 30-20], Names).
test(background, [Names == [green]]) :-
    new(C, sdl_canvas(size(10, 10), colour(green))),
    canvas_window(C, W),
    pixels(W, [20-20], Names).
test(transparent, [Names == [blue]]) :-
    new(C, sdl_canvas(size(10, 10))),
    canvas_window(C, W),
    pixels(W, [20-20], Names).
test(translucent, [RGB == rgb(128,0,128)]) :-
    new(C, sdl_canvas(size(10, 10))),
    colour_layer([red], L),
    send(L, alpha, 0.5),
    send(C, append, L),
    canvas_window(C, W),
    pixels(W, [20-20], [RGB0]),
    round8(RGB0, RGB).
test(mesh, [Names == [red, blue]]) :-
    new(C, sdl_canvas(size(40, 40))),
    new(M, sdl_mesh),
    R = rgba(255,0,0,255),
    pce_sdl_mesh_set(M, [v(0,0,0,0,R), v(40,0,0,0,R), v(0,40,0,0,R)], []),
    send(C, append, new(L, sdl_layer)),
    send(L, mesh, M),
    canvas_window(C, W),
    pixels(W, [5-5, 35-35], Names).
test(nested, [Names == [red, green]]) :-
    new(Inner, sdl_canvas(size(20, 10))),
    colour_layer([red, green], L),
    send(Inner, append, L),
    new(Outer, sdl_canvas(size(20, 20))),
    send(Outer, append, sdl_layer(Inner, @default, stretch)),
    canvas_window(Outer, W),
    pixels(W, [10-20, 30-20], Names).
test(layer_changed, [Before-After == [red]-[green]]) :-
    new(C, sdl_canvas(size(10, 10))),
    colour_layer([red], L),
    send(C, append, L),
    canvas_window(C, W),
    send(W, open),
    frame_pixels(W, [20-20], Before),
    colour_image([green], I),
    send(L, texture, sdl_texture(I)),
    frame_pixels(W, [20-20], After),
    send(W?frame, destroy).
test(texture_changed, [Before-After == [red]-[green]]) :-
    new(C, sdl_canvas(size(10, 10))),
    colour_layer([red], L),
    send(C, append, L),
    new(Outer, sdl_canvas(size(10, 10))),
    send(Outer, append, sdl_layer(C, @default, stretch)),
    canvas_window(Outer, W),
    send(W, open),
    frame_pixels(W, [20-20], Before),
    colour_image([green], I),
    get(L, texture, T),
    send(T, image, I),
    frame_pixels(W, [20-20], After),
    send(W?frame, destroy).
test(keep_pixels, [Names == [red, red, blue]]) :-
    new(C, sdl_canvas(size(40, 40))),
    send(C, clear, @off),               % a trail
    colour_image([red], I),
    send(C, append, new(L, sdl_layer(sdl_texture(I), area(0,0,10,10),
                                      stretch))),
    canvas_window(C, W),
    send(W, open),
    frame_pixels(W, [5-5], _),
    send(L, area, area(20, 0, 10, 10)),
    frame_pixels(W, [5-5, 25-5, 35-35], Names),
    send(W?frame, destroy).
test(clear, [Names == [blue, red]]) :-
    new(C, sdl_canvas(size(40, 40))),
    colour_image([red], I),
    send(C, append, new(L, sdl_layer(sdl_texture(I), area(0,0,10,10),
                                      stretch))),
    canvas_window(C, W),
    send(W, open),
    frame_pixels(W, [5-5], _),
    send(L, area, area(20, 0, 10, 10)),
    frame_pixels(W, [5-5, 25-5], Names),
    send(W?frame, destroy).
test(resize, [Names == [red, green]]) :-
    new(C, sdl_canvas(size(10, 10))),
    colour_layer([red, green], L),
    send(C, append, L),
    canvas_window(C, W),
    send(W, open),
    frame_pixels(W, [20-20], _),
    send(C, size, size(20, 10)),
    pixels(W, [10-20, 30-20], Names).
test(image, [W-H-Names == 20-10-[red, green]]) :-
    new(C, sdl_canvas(size(20, 10))),
    colour_layer([red, green], L),
    send(C, append, L),
    canvas_window(C, Win),
    send(Win, open),
    get(C, image, I),
    get(I, size, size(W, H)),
    name_of(I, 5, 5, N1),
    name_of(I, 15, 5, N2),
    Names = [N1, N2],
    send(Win?frame, destroy).
test(cycle, [Names == [green]]) :-
    new(C, sdl_canvas(size(10, 10), colour(green))),
    send(C, append, sdl_layer(C, @default, stretch)),
    canvas_window(C, W),
    pixels(W, [20-20], Names).

:- end_tests(sdl_canvas).
