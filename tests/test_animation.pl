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

:- module(test_animation, [test_animation/0]).

/** <module> Animated images

Writes a small animated PNG (APNG) and a plain PNG, loads them as class
animation and plays them in a bitmap.

Run with:

    swipl -g test_animation -t halt packages/xpce/tests/test_animation.pl
*/

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(filesex)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_animation :-
    tmp_file(animation, Dir),
    make_directory(Dir),
    nb_setval(test_animation_dir, Dir),
    call_cleanup(run_tests([ animation ]),
                 delete_directory_and_contents(Dir)).

test_file(Name, Path) :-
    nb_getval(test_animation_dir, Dir),
    directory_file_path(Dir, Name, Path).

                 /*******************************
                 *          WRITING PNG         *
                 *******************************/

%   The test images are 4x4 pixels; each frame has a single colour.

frames([ red-50, lime-100, blue-150 ]).
loop_count(2).

rgb(red,  255, 0,   0).
rgb(lime, 0,   255, 0).
rgb(blue, 0,   0,   255).

%!  write_apng(+File) is det.
%
%   Write the frames of frames/1 as an animated PNG that plays
%   loop_count/1 times.

write_apng(File) :-
    frames(Frames),
    loop_count(Loops),
    length(Frames, Count),
    frame_chunks(Frames, 0, Chunks),
    setup_call_cleanup(
        open(File, write, Out, [type(binary)]),
        ( png_header(Out),
          chunk(Out, 'acTL', [int32(Count), int32(Loops)]),
          maplist(chunk(Out), Chunks)
        ),
        close(Out)),
    png_trailer(File).

%   The first frame is IDAT, as for viewers that ignore the animation;
%   the others are fdAT chunks.  fcTL and fdAT chunks are numbered.

frame_chunks([], _, []).
frame_chunks([Colour-Ms|T], Seq0, [Ctl, Data|Chunks]) :-
    Ctl = 'fcTL'-[ int32(Seq0), int32(4), int32(4), int32(0), int32(0),
                   int16(Ms), int16(1000), byte(0), byte(0) ],
    Seq1 is Seq0+1,
    image_data(Colour, Bytes),
    (   Seq0 == 0
    ->  Data = 'IDAT'-[bytes(Bytes)],
        Seq = Seq1
    ;   Data = 'fdAT'-[int32(Seq1), bytes(Bytes)],
        Seq is Seq1+1
    ),
    frame_chunks(T, Seq, Chunks).

%!  write_png(+File, +Colour) is det.

write_png(File, Colour) :-
    image_data(Colour, Bytes),
    setup_call_cleanup(
        open(File, write, Out, [type(binary)]),
        ( png_header(Out),
          chunk(Out, 'IDAT', [bytes(Bytes)])
        ),
        close(Out)),
    png_trailer(File).

png_header(Out) :-
    maplist(put_byte(Out), [137, 80, 78, 71, 13, 10, 26, 10]),
    chunk(Out, 'IHDR', [ int32(4), int32(4), byte(8), byte(6),
                         byte(0), byte(0), byte(0) ]).

png_trailer(File) :-
    setup_call_cleanup(
        open(File, append, Out, [type(binary)]),
        chunk(Out, 'IEND', []),
        close(Out)).

%   The zlib data of a 4x4 RGBA image of one colour: each row starts
%   with filter type 0 (none).  We use a stored, i.e., uncompressed,
%   deflate block.

image_data(Colour, ZLib) :-
    rgb(Colour, R, G, B),
    Row = [0, R,G,B,255, R,G,B,255, R,G,B,255, R,G,B,255],
    append([Row, Row, Row, Row], Raw),
    length(Raw, Len),
    NLen is Len xor 0xffff,
    adler32(Raw, Adler),
    int_bytes(16, Len, LenBE), reverse(LenBE, LenLE),
    int_bytes(16, NLen, NLenBE), reverse(NLenBE, NLenLE),
    int_bytes(32, Adler, AdlerBytes),
    append([[0x78, 0x01, 0x01], LenLE, NLenLE, Raw, AdlerBytes], ZLib).

chunk(Out, Type-Fields) :-
    chunk(Out, Type, Fields).

chunk(Out, Type, Fields) :-
    maplist(field_bytes, Fields, Parts),
    append(Parts, Data),
    atom_codes(Type, TypeCodes),
    length(Data, Len),
    append(TypeCodes, Data, Checked),
    crc32(Checked, CRC),
    int_bytes(32, Len, LenBytes),
    int_bytes(32, CRC, CRCBytes),
    append([LenBytes, Checked, CRCBytes], Bytes),
    maplist(put_byte(Out), Bytes).

field_bytes(byte(B), [B]).
field_bytes(int16(I), Bytes) :- int_bytes(16, I, Bytes).
field_bytes(int32(I), Bytes) :- int_bytes(32, I, Bytes).
field_bytes(bytes(Bytes), Bytes).

%!  int_bytes(+Bits, +Int, -Bytes) is det.
%
%   Bytes is Int as a big-endian sequence of Bits/8 bytes.

int_bytes(Bits, Int, Bytes) :-
    N is Bits // 8,
    numlist(1, N, Is),
    maplist([I,B]>>(B is (Int >> ((N-I)*8)) /\ 0xff), Is, Bytes).

adler32(Bytes, Adler) :-
    foldl([X,A0-B0,A-B]>>( A is (A0+X) mod 65521,
                           B is (B0+A) mod 65521 ),
          Bytes, 1-0, A1-B1),
    Adler is (B1 << 16) \/ A1.

crc32(Bytes, CRC) :-
    foldl(crc32_byte, Bytes, 0xffffffff, C),
    CRC is C xor 0xffffffff.

crc32_byte(Byte, C0, C) :-
    C1 is C0 xor Byte,
    numlist(1, 8, Bits),
    foldl([_,X0,X]>>( X0 /\ 1 =:= 1
                    -> X is (X0 >> 1) xor 0xedb88320
                    ;  X is X0 >> 1
                    ), Bits, C1, C).

                 /*******************************
                 *            HELPERS           *
                 *******************************/

apng(A) :-
    test_file('anim.png', File),
    write_apng(File),
    new(A, animation(File)).

%!  colour_of(+Image, -Colour) is det.
%
%   The colour name of frames/1 whose colour the image has.

colour_of(Image, Colour) :-
    get(Image, pixel, 1, 1, C),
    get(C, red, R), get(C, green, G), get(C, blue, B),
    once(rgb(Colour, R, G, B)).

vector_list(Vector, List) :-
    get(Vector, size, N),
    findall(E, ( between(1, N, I),
                 get(Vector, element, I, E)
               ), List).

shown(BM, Colour) :-
    get(BM, image, Image),
    colour_of(Image, Colour).

%!  wait_until(:Goal, +Seconds) is semidet.

wait_until(Goal, Max) :-
    get_time(T0),
    Deadline is T0+Max,
    repeat,
    (   call(Goal)
    ->  !
    ;   get_time(T),
        T > Deadline
    ->  !, fail
    ;   ignore(send(@display, dispatch)),
        fail
    ).

                 /*******************************
                 *             TESTS            *
                 *******************************/

:- begin_tests(animation).

test(frames, [N-W-H-Colours == 3-4-4-[red, lime, blue]]) :-
    apng(A),
    get(A, count, N),
    get(A?size, width, W),
    get(A?size, height, H),
    findall(C, ( between(1, N, I),
                 get(A, frame, I, Image),
                 colour_of(Image, C)
               ), Colours).
test(delays, Delays == [50, 100, 150]) :-
    apng(A),
    get(A, delays, Vector),
    vector_list(Vector, Delays).
test(loop_count, N == 2) :-
    apng(A),
    get(A, loop_count, N).
test(still_png, [N-C == 1-blue]) :-
    test_file('still.png', File),
    write_png(File, blue),
    new(A, animation(File)),
    get(A, count, N),
    get(A, frame, 1, Image),
    colour_of(Image, C).
test(bitmap_shows_first_frame, C == red) :-
    apng(A),
    new(BM, bitmap(A)),
    shown(BM, C).
test(select_frame, [F-C == 3-blue]) :-
    apng(A),
    new(BM, bitmap(A)),
    send(BM, frame, 3),
    get(BM, frame, F),
    shown(BM, C).
test(no_such_frame, fail) :-
    apng(A),
    new(BM, bitmap(A)),
    send(BM, frame, 4).
test(image_loads_first_frame, C == red) :-
    test_file('anim.png', File),
    write_apng(File),
    new(I, image(File)),
    colour_of(I, C).

%   Plays loop_count/1 times through 3 frames of 50, 100 and 150
%   milliseconds, i.e., 0.6 seconds, and stops at the last frame.

test(play, [Loops-F-C == 2-3-blue]) :-
    apng(A),
    new(BM, bitmap(A)),
    send(BM, play),
    get(BM, playing, @on),
    wait_until(shown(BM, lime), 2),
    wait_until(shown(BM, blue), 2),
    wait_until(get(BM, playing, @off), 3),
    get(BM, loops, Loops),
    get(BM, frame, F),
    shown(BM, C).
test(stop, [Playing == @off]) :-
    apng(A),
    new(BM, bitmap(A)),
    send(BM, play),
    send(BM, stop),
    get(BM, playing, Playing).
test(one_frame_does_not_play, fail) :-
    test_file('still.png', File),
    write_png(File, blue),
    new(A, animation(File)),
    new(BM, bitmap(A)),
    send(BM, play).

:- end_tests(animation).
