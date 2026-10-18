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

:- module(test_sound, [test_sound/0]).

/** <module> Playing and recording sound

Uses the `disk` audio driver of SDL, which writes the audio it plays
to a file and records from a file.

Run with:

    swipl -g test_sound -t halt packages/xpce/tests/test_sound.pl
*/

%   SDL takes a copy of the environment when it is initialised, which
%   happens when library(pce) is loaded.  So we must select the disk
%   driver and its files before that.

:- use_module(library(filesex)).

setup_audio :-
    tmp_file(sound, Dir),
    make_directory(Dir),
    directory_file_path(Dir, 'out.raw', Out),
    directory_file_path(Dir, 'in.raw', In),
    setenv('SDL_AUDIO_DRIVER', disk),
    setenv('SDL_AUDIO_DISK_OUTPUT_FILE', Out),
    setenv('SDL_AUDIO_DISK_INPUT_FILE', In),
    nb_setval(test_sound_dir, Dir).
:- initialization(setup_audio, now).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(aggregate)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_sound :-
    nb_getval(test_sound_dir, Dir),
    call_cleanup(run_tests([ sound ]),
                 delete_directory_and_contents(Dir)).

test_file(Name, Path) :-
    nb_getval(test_sound_dir, Dir),
    directory_file_path(Dir, Name, Path).

%!  write_tone(+File, +Seconds) is det.
%
%   Write a 440Hz tone as a mono 16-bit 8000Hz WAV file.

write_tone(File, Seconds) :-
    Rate = 8000,
    N is round(Rate*Seconds),
    DataLen is N*2,
    setup_call_cleanup(
        open(File, write, Out, [type(binary)]),
        ( format(Out, "RIFF", []), le(Out, 36+DataLen, 4),
          format(Out, "WAVEfmt ", []), le(Out, 16, 4),
          le(Out, 1, 2), le(Out, 1, 2), le(Out, Rate, 4),
          le(Out, Rate*2, 4), le(Out, 2, 2), le(Out, 16, 2),
          format(Out, "data", []), le(Out, DataLen, 4),
          forall(between(1, N, I),
                 ( V is round(10000*sin(2*pi*440*I/Rate)),
                   le(Out, V, 2)
                 ))
        ),
        close(Out)).

%!  write_au(+File, +Seconds) is det.
%
%   Write the tone of write_tone/2 as a Sun/NeXT .au file: a header and
%   big-endian 16-bit samples.  SDL itself only reads WAV files, so
%   loading this tests the decoders of SDL3_sound.

write_au(File, Seconds) :-
    Rate = 8000,
    N is round(Rate*Seconds),
    setup_call_cleanup(
        open(File, write, Out, [type(binary)]),
        ( format(Out, ".snd", []),
          be(Out, 24, 4), be(Out, N*2, 4), be(Out, 3, 4),
          be(Out, Rate, 4), be(Out, 1, 4),
          forall(between(1, N, I),
                 ( V is round(10000*sin(2*pi*440*I/Rate)),
                   be(Out, V, 2)
                 ))
        ),
        close(Out)).

be(Out, V0, Bytes) :-
    V is V0,
    Max is Bytes-1,
    forall(between(0, Max, I),
           ( B is (V >> (8*(Max-I))) /\ 0xff,
             put_byte(Out, B)
           )).

le(Out, V0, Bytes) :-
    V is V0,
    Max is Bytes-1,
    forall(between(0, Max, I),
           ( B is (V >> (8*I)) /\ 0xff,
             put_byte(Out, B)
           )).

:- meta_predicate
    pump(0).

%!  pump(:Goal) is semidet.
%
%   Dispatch events until Goal succeeds, for at most 5 seconds.

pump(Goal) :-
    get_time(T0),
    repeat,
    (   call(Goal)
    ->  !
    ;   get_time(T),
        T - T0 > 5
    ->  !, fail
    ;   ignore(send(@display, dispatch)),
        fail
    ).

%!  write_input(+Byte, +Size) is det.
%
%   Fill the file from which the disk driver records.

write_input(Byte, Size) :-
    test_file('in.raw', In),
    setup_call_cleanup(
        open(In, write, Out, [type(binary)]),
        forall(between(1, Size, _), put_byte(Out, Byte)),
        close(Out)).

nonzero_bytes(File) :-
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        nonzero_byte(In),
        close(In)).

nonzero_byte(In) :-
    get_byte(In, B),
    B \== -1,
    (   B =\= 0
    ->  true
    ;   nonzero_byte(In)
    ).

:- dynamic
    finished/1,
    level/1.

%   The unit tests use these helpers: a dynamic predicate used in a
%   test body would be local to the module of the unit.

clear :-
    retractall(finished(_)),
    retractall(level(_)).

finished_sound(Sound) :-
    finished(Sound).

levels(Levels) :-
    findall(L, level(L), Levels).

%   Called from the messages of the sounds and recorders.

user:test_sound_finished(Sound) :-
    assertz(test_sound:finished(Sound)).
user:test_sound_level(Level) :-
    assertz(test_sound:level(Level)).

:- begin_tests(sound).

test(load, [F-C-D == 8000-1-0.25]) :-
    test_file('tone.wav', File),
    write_tone(File, 0.25),
    new(S, sound(File)),
    get(S, frequency, F),
    get(S, channels, C),
    get(S, duration, D).
test(load_error, fail) :-
    test_file('nonexistent.wav', File),
    new(_, sound(File)).
test(play, [Playing-Done == (@on)-(@off)]) :-
    test_file('tone.wav', File),
    write_tone(File, 0.2),
    clear,
    new(S, sound(File)),
    send(S, message, message(@prolog, test_sound_finished, @arg1)),
    send(S, play),
    get(S, playing, Playing),
    pump(finished_sound(S)),
    get(S, playing, Done),
    test_file('out.raw', Out),
    nonzero_bytes(Out).
test(stop, [Playing-Finished == (@off)-false]) :-
    test_file('tone.wav', File),
    write_tone(File, 2),
    clear,
    new(S, sound(File)),
    send(S, message, message(@prolog, test_sound_finished, @arg1)),
    send(S, play),
    send(S, stop),
    get(S, playing, Playing),
    ( finished_sound(S) -> Finished = true ; Finished = false ).
test(record, [Recorded-Level == true-true]) :-
    write_input(0x40, 200000),
    clear,
    new(R, audio_recorder(message(@prolog, test_sound_level, @arg2))),
    send(R, record),
    pump((levels(Ls0), member(L, Ls0), L > 0)),
    send(R, stop),
    get(R, sound, S),
    get(S, duration, D),
    ( D > 0 -> Recorded = true ; Recorded = false ),
    ( levels(Ls), member(L1, Ls), L1 > 0 -> Level = true ; Level = false ).
test(silence, [Max == 0]) :-
    write_input(0, 200000),
    clear,
    new(R, audio_recorder(message(@prolog, test_sound_level, @arg2))),
    send(R, record),
    pump(levels([_|_])),
    send(R, stop),
    levels(Ls),
    max_list(Ls, Max0),
    Max is integer(Max0).
test(save_and_load, [F-C-Same == 44100-1-true]) :-
    write_input(0x40, 200000),
    new(R, audio_recorder),
    send(R, record),
    get(R, sound, S),
    pump((get(S, duration, D0), D0 > 0.1)),
    send(R, stop),
    get(S, duration, D),
    test_file('recorded.wav', File),
    send(S, save, File),
    new(S2, sound(File)),
    get(S2, frequency, F),
    get(S2, channels, C),
    get(S2, duration, D2),
    ( abs(D2-D) < 0.001 -> Same = true ; Same = false ).

test(peaks, [Peaks == [0.31, 0.31, 0.31, 0.31]]) :-
    test_file('tone.wav', File),        % amplitude 10000/32768
    write_tone(File, 0.4),
    new(S, sound(File)),
    get(S, peaks, 4, Vector),
    findall(P, ( between(1, 4, I),
                 get(Vector, element, I, P0),
                 P is round(P0*100)/100.0
               ), Peaks).
test(extensions, true(memberchk(wav, Exts))) :-
    get(@display, sound_file_extensions, Chain),
    chain_list(Chain, Exts).
test(au_file, [ condition(send(@pce, has_feature, sound_decoders)),
                F-C-D == 8000-1-0.25
              ]) :-
    test_file('tone.au', File),
    write_au(File, 0.25),
    new(S, sound(File)),
    get(S, frequency, F),
    get(S, channels, C),
    get(S, duration, D).

%   A tone fades in and out over 5 milliseconds, so the first of 50
%   parts of a 0.25 second tone (5 ms) is softer than the rest.

test(tone, [F-C-D-Soft-Loud == 44100-1-0.25-true-0.98]) :-
    new(S, sound),
    send(S, tone, 440, 0.25),
    get(S, frequency, F),
    get(S, channels, C),
    get(S, duration, D0),
    D is round(D0*100)/100.0,
    get(S, peaks, 50, V),
    get(V, element, 1, First),
    get(V, element, 25, Middle),
    ( First < Middle -> Soft = true ; Soft = false ),
    Loud is round(Middle*100)/100.0.

%   ->bell plays display<-bell_sound, by default a tone made from the
%   class variables bell_pitch and bell_duration.

test(bell_sound, [F-D == 44100-0.2]) :-
    get(@display, bell_sound, S),
    get(S, frequency, F),
    get(S, duration, D0),
    D is round(D0*100)/100.0.
test(bell, [Playing-Done == (@on)-(@off)]) :-
    send(@display, bell),
    get(@display, slot, bell_sound, S),     % the sound it plays
    get(S, playing, Playing),
    pump(get(S, playing, @off)),
    get(S, playing, Done).
test(bell_file, [ F-D == 8000-0.25,
                  cleanup(set_bell_sound(@nil))
                ]) :-
    test_file('bell.wav', File),
    write_tone(File, 0.25),
    set_bell_sound(File),
    get(@display, bell_sound, S),
    get(S, frequency, F),
    get(S, duration, D).
test(bell_file_change, [ D == 0.5,
                         cleanup(set_bell_sound(@nil))
                       ]) :-
    test_file('bell1.wav', File1),
    test_file('bell2.wav', File2),
    test_file('bell3.wav', File3),
    write_tone(File1, 0.25),
    write_tone(File2, 0.5),
    write_tone(File3, 0.5),
    set_bell_sound(File1),
    send(@display, bell, 0),
    set_bell_sound(File2),
    send(@display, bell, 0),
    set_bell_sound(File3),
    get(@display, bell_sound, S),
    get(S, duration, D).

:- end_tests(sound).

set_bell_sound(File) :-
    get(@display?class, class_variable, bell_sound, CV),
    send(CV, value, File).
