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

:- module(test_image_save, [test_image_save/0]).

/** <module> Saving images in various formats

Saves an image in each format image->save supports, loads it back and
checks the size, the colour and the signature of the file.

Run with:

    swipl -g test_image_save -t halt packages/xpce/tests/test_image_save.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(filesex)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_image_save :-
    tmp_file(image_save, Dir),
    make_directory(Dir),
    nb_setval(test_image_save_dir, Dir),
    call_cleanup(run_tests([ image_save ]),
                 delete_directory_and_contents(Dir)).

test_file(Name, Path) :-
    nb_getval(test_image_save_dir, Dir),
    directory_file_path(Dir, Name, Path).

%!  red(-Image) is det.
%
%   A 16x8 red image.

red(I) :-
    new(I, image(@nil, 16, 8, pixmap)),
    send(I, fill, colour(red)).

%!  noise(-Image) is det.
%
%   A 64x64 image of pseudo random pixels, which compresses badly.

noise(I) :-
    new(I, image(@nil, 64, 64, pixmap)),
    forall(( between(0, 63, X), between(0, 63, Y) ),
           ( V is (X*7919 + Y*104729 + X*Y*31) mod 256,
             send(I, pixel, X, Y, colour(@default, V, 255-V, V)))).

%!  saved(+Name, +Format, -W, -H, -Red) is det.
%
%   Save red/1 to Name, using Format if not `default`, and load it
%   back.  Red is true if the pixels are (about) red.

saved(Name, Format, W, H, Red) :-
    red(I),
    test_file(Name, File),
    (   Format == default
    ->  send(I, save, File)
    ;   send(I, save, File, Format)
    ),
    new(I2, image(File)),
    get(I2, size, size(W, H)),
    get(I2, pixel, 5, 5, C),
    get(C, red, R), get(C, green, G), get(C, blue, B),
    (   R > 240, G < 16, B < 16 -> Red = true ; Red = false ).

%!  signature(+Name, -Bytes) is det.
%
%   The first 4 bytes of the file Name.

signature(Name, Bytes) :-
    test_file(Name, File),
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        findall(B, (between(1, 4, _), get_byte(In, B)), Bytes),
        close(In)).

%!  can_save(+Format) is semidet.
%
%   True if SDL_image was built with support for writing Format.

can_save(Format) :-
    get(@display, image_save_formats, Chain),
    send(Chain, member, Format).

half_transparent(I) :-
    new(I, image(@nil, 4, 4, pixmap)),
    forall(( between(0, 3, X), between(0, 3, Y) ),
           send(I, pixel, X, Y, colour(@default, 200, 100, 50, 128))).

pixel_rgba(I, R-G-B-A) :-
    get(I, pixel, 1, 1, C),
    get(C, red, R), get(C, green, G), get(C, blue, B), get(C, alpha, A).

close_rgba(R1-G1-B1-A1, R2-G2-B2-A2) :-
    maplist([X,Y]>>(abs(X-Y) =< 3), [R1,G1,B1,A1], [R2,G2,B2,A2]).

file_size(Name, Size) :-
    test_file(Name, File),
    size_file(File, Size).

:- begin_tests(image_save).

test(png, W-H-Red == 16-8-true)  :- saved('a.png',  default, W, H, Red).
test(jpeg, W-H-Red == 16-8-true) :- saved('a.jpg',  default, W, H, Red).
test(webp, [condition(can_save(webp)), W-H-Red == 16-8-true]) :-
    saved('a.webp', default, W, H, Red).
test(gif, W-H-Red == 16-8-true)  :- saved('a.gif',  default, W, H, Red).
test(bmp, W-H-Red == 16-8-true)  :- saved('a.bmp',  default, W, H, Red).
test(tga, W-H-Red == 16-8-true)  :- saved('a.tga',  default, W, H, Red).
test(ico, W-H-Red == 16-8-true)  :- saved('a.ico',  default, W, H, Red).
test(avif, [condition(can_save(avif)), W-H-Red == 16-8-true]) :-
    saved('a.avif', default, W, H, Red).

%   The extension selects the format; the format argument overrules it
%   and an unknown extension gives PNG.

test(extension_selects_format, Sig == [0xFF, 0xD8, 0xFF, 0xE0]) :-
    saved('b.jpeg', default, _, _, _),
    signature('b.jpeg', Sig).
test(format_overrules_extension, Sig == [0x47, 0x49, 0x46, 0x38]) :-
    saved('c.png', gif, _, _, _),
    signature('c.png', Sig).
test(unknown_extension_is_png, Sig == [0x89, 0x50, 0x4E, 0x47]) :-
    saved('d.img', default, _, _, _),
    signature('d.img', Sig).

test(jpeg_quality, true(Small < Large)) :-
    noise(I),
    test_file('low.jpg', Low),
    test_file('high.jpg', High),
    send(I, save, Low, jpeg, 10),
    send(I, save, High, jpeg, 95),
    file_size('low.jpg', Small),
    file_size('high.jpg', Large).

test(save_formats, true(subset([png, jpeg, gif, bmp], Formats))) :-
    get(@display, image_save_formats, Chain),
    chain_list(Chain, Formats).

%   Images are stored with premultiplied alpha.  Setting and getting
%   pixels and saving must convert, or semi-transparent pixels come out
%   too dark.  8-bit premultiplied alpha loses some precision, so we
%   compare with a tolerance.

test(semi_transparent_pixel, true(close_rgba(RGBA, 200-100-50-128))) :-
    half_transparent(I),
    pixel_rgba(I, RGBA).
test(semi_transparent_save, true(close_rgba(RGBA, 200-100-50-128))) :-
    half_transparent(I),
    test_file('alpha.png', File),
    send(I, save, File),
    new(I2, image(File)),
    pixel_rgba(I2, RGBA).

:- end_tests(image_save).
