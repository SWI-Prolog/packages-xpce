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
:- use_module(library(process), [process_create/3, process_which/2]).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

test_sound :-
    nb_getval(test_sound_dir, Dir),
    call_cleanup(run_tests([ sound, audio_stream, audio_stream_encoded,
                             record_to_sink ]),
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
    pump(0),
    poll(0).

%!  poll(:Goal) is semidet.
%
%   As pump/1, but sleep rather than dispatch events, so other threads
%   can use xpce meanwhile.

poll(Goal) :-
    get_time(T0),
    repeat,
    (   call(Goal)
    ->  !
    ;   get_time(T),
        T - T0 > 5
    ->  !, fail
    ;   sleep(0.02),
        fail
    ).

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
test(load_garbage, fail) :-             % no extension: all decoders
    test_file(garbage, File),
    setup_call_cleanup(open(File, write, Out),
                       format(Out, 'this is no sound file', []),
                       close(Out)),
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

%!  write_pcm(+Out, +Seconds) is det.
%
%   Write Seconds of a 440Hz tone to Out as 16-bit stereo samples at
%   44100Hz, the format of audio_stream(s16, 44100, 2).

write_pcm(Out, Seconds) :-
    Rate = 44100,
    N is round(Rate*Seconds),
    forall(between(1, N, I),
           ( V is round(10000*sin(2*pi*440*I/Rate)),
             le(Out, V, 2),
             le(Out, V, 2)
           )).

pcm_stream(AS) :-
    new(AS, audio_stream(s16, 44100, 2)),
    send(AS, message, message(@prolog, test_sound_finished, @arg1)).

%   Write Seconds to the audio_stream AS using pce_open/3.  Closing
%   the stream ends the data.

write_stream(AS, Seconds) :-
    setup_call_cleanup(
        pce_open(AS, append, Out),
        write_pcm(Out, Seconds),
        close(Out)).

:- dynamic
    low_count/1.

reset_low_count :-
    retractall(low_count(_)),
    assertz(low_count(0)).

low_calls(N) :-
    low_count(N).

%   Called from <-low_water_message: write 0.1 seconds to the stream
%   opened by low_water_stream/1, up to 3 times.  The 4th time, closing
%   the stream ends the data.

low_water_stream(AS) :-
    reset_low_count,
    pce_open(AS, append, Out),
    nb_setval(test_stream_out, Out).

user:test_stream_low(_AS) :-
    retract(test_sound:low_count(N0)),
    N is N0+1,
    assertz(test_sound:low_count(N)),
    nb_getval(test_stream_out, Out),
    (   N < 4
    ->  write_pcm(Out, 0.1),
        flush_output(Out)
    ;   close(Out)
    ).

:- begin_tests(audio_stream).

test(play, [Playing-Done == (@on)-(@off)]) :-
    clear,
    pcm_stream(AS),
    write_stream(AS, 0.3),
    send(AS, play),
    get(AS, playing, Playing),
    played(AS, 0.3),
    get(AS, playing, Done).
%   SDL only takes whole sample frames (4 bytes here).  Data may be
%   written in any pieces.

%   A finished stream closed its device.  It plays again.

test(replay, [Played == true]) :-
    clear,
    pcm_stream(AS),
    write_stream(AS, 0.2),
    send(AS, play),
    pump(finished_sound(AS)),
    clear,
    write_stream(AS, 0.3),
    send(AS, play),
    ( played(AS, 0.3) -> Played = true ; Played = false ).
%   While playing, a write blocks if more than 2 seconds are queued.

test(write_blocks, [true(Time > 0.5)]) :-
    pcm_stream(AS),
    send(AS, play),
    get_time(T0),
    write_stream(AS, 3),
    get_time(T1),
    Time is T1-T0,
    send(AS, stop).
test(partial_frames, [Bytes == 8]) :-          % 2 frames
    pcm_stream(AS),
    send(AS, append, string(ab)),
    send(AS, append, string(cde)),
    send(AS, append, string(fgh)),
    get(AS, queued, Q),
    Bytes is round(Q*44100*4),
    free(AS).
test(queued, [Q == 0.5]) :-
    pcm_stream(AS),
    write_stream(AS, 0.5),
    get(AS, queued, Q0),
    Q is round(Q0*100)/100.0,
    free(AS).
test(low_water, [Count-Done == 4-true]) :-
    clear,
    pcm_stream(AS),
    low_water_stream(AS),
    send(AS, low_water, 0.05),
    send(AS, low_water_message, message(@prolog, test_stream_low, @arg1)),
    send(AS, play),
    ( pump(finished_sound(AS)) -> Done = true ; Done = false ),
    low_calls(Count).
test(stop, [Playing-Finished == (@off)-false, Q =:= 0]) :-
    clear,
    pcm_stream(AS),
    write_stream(AS, 2),
    send(AS, play),
    send(AS, stop),
    get(AS, playing, Playing),
    get(AS, queued, Q),
    ( finished_sound(AS) -> Finished = true ; Finished = false ),
    free(AS).
%   After ->stop, writes fail, so a producer such as copy_stream_data/2
%   ends.  ->play accepts data again.

test(write_after_stop, [Stopped-Again == error-true]) :-
    pcm_stream(AS),
    send(AS, play),
    send(AS, stop),
    catch(with_output_to(string(_),
                         ( catch(write_stream(AS, 0.1), _, fail)
                         ->  Stopped = written
                         ;   Stopped = error
                         )),
          _, Stopped = error),
    send(AS, play),
    ( catch(write_stream(AS, 0.1), _, fail) -> Again = true ; Again = false ),
    send(AS, stop),
    free(AS).
%   ->stop ends a producer thread that writes to the stream: its writes
%   raise an error.  We wait using sleep/1, as `display->dispatch` holds
%   the xpce lock while it waits for events, which blocks the producer.

test(stop_ends_producer, [true(Status = exception(_))]) :-
    pcm_stream(AS),
    send(AS, play),
    thread_create(with_output_to(string(_), write_stream(AS, 6)), Id, []),
    poll(( get(AS, queued, Q0), Q0 > 1.0 )),
    send(AS, stop),
    thread_join(Id, Status),
    free(AS).
test(drain, [Playing == @on, Q =:= 0]) :-
    pcm_stream(AS),
    pce_open(AS, append, Out),
    write_pcm(Out, 0.2),
    flush_output(Out),
    send(AS, play),
    send(AS, drain),                    % the data did not end
    get(AS, queued, Q),
    get(AS, playing, Playing),
    close(Out),
    free(AS).

:- end_tests(audio_stream).

%!  stream_file(+AS, +File, +Parts) is det.
%
%   Write the bytes of File to the audio_stream AS using pce_open/3.
%   Parts is a list of fractions of the file: after each but the last
%   part we dispatch events for 0.3 seconds, so the decoder waits for
%   the data.

stream_file(AS, File, Parts) :-
    read_file_to_codes(File, Codes, [type(binary)]),
    length(Codes, Len),
    setup_call_cleanup(
        pce_open(AS, write, Out),
        stream_parts(Parts, Codes, Len, Out),
        close(Out)).

stream_parts([], _, _, _).
stream_parts([F|T], Codes, Len, Out) :-
    (   T == []
    ->  Part = Codes, Rest = []
    ;   N is round(F*Len),
        length(Part, N),
        append(Part, Rest, Codes)
    ),
    forall(member(B, Part), put_byte(Out, B)),
    flush_output(Out),
    (   T == []
    ->  true
    ;   pause(0.3)
    ),
    stream_parts(T, Rest, Len, Out).

pause(Secs) :-
    get_time(T0),
    End is T0+Secs,
    repeat,
    ignore(send(@display, dispatch)),
    get_time(T),
    T >= End,
    !.

encoded_stream(AS) :-
    new(AS, audio_stream),
    send(AS, message, message(@prolog, test_sound_finished, @arg1)).

%   The disk driver writes silence while nothing is played, so the
%   size of its output tells little.  We count the frames that are not
%   silent.  A 440Hz tone is rarely zero.

played(AS, Seconds) :-
    pump(finished_sound(AS)),
    test_file('out.raw', Out),
    audible_seconds(Out, Played),
    (   Played >= 0.9*Seconds
    ->  true
    ;   file_bytes(Out, Bytes),
        length(Bytes, Len),
        audible_runs(Bytes, 0, none, Runs),
        print_message(error, format("Played ~3f of ~w seconds; \c
                                     ~D bytes; audible: ~p",
                                    [Played, Seconds, Len, Runs])),
        fail
    ).

%   Debugging: the audible parts as From-To in seconds, ignoring gaps
%   shorter than 10ms.

audible_runs([A,B,C,D|T], I, Run, Runs) :-
    !,
    I1 is I+1,
    (   (A\/B\/C\/D) =\= 0
    ->  (   Run = S-E, I-E < 441
        ->  audible_runs(T, I1, S-I, Runs)
        ;   Run = S-E
        ->  run_secs(S, E, R), Runs = [R|Rs],
            audible_runs(T, I1, I-I, Rs)
        ;   audible_runs(T, I1, I-I, Runs)
        )
    ;   audible_runs(T, I1, Run, Runs)
    ).
audible_runs(_, _, none, []) :- !.
audible_runs(_, _, S-E, [R]) :-
    run_secs(S, E, R).

run_secs(S, E, From-To) :-
    From is round(S/44.1)/1000,
    To is round(E/44.1)/1000.

%!  audible_seconds(+File, -Seconds) is det.
%
%   Seconds of 16-bit stereo 44100Hz audio in File that is not silent.

audible_seconds(File, Seconds) :-
    file_bytes(File, Bytes),
    audible_frames(Bytes, 0, N),
    Seconds is N/44100.

%   On Windows, we cannot open the file while SDL writes it, e.g., while
%   the sound of an earlier test is still playing.

file_bytes(File, Bytes) :-
    pump(catch(open(File, read, In, [type(binary)]), _, fail)),
    call_cleanup(read_stream_to_codes(In, Bytes), close(In)).

audible_frames([A,B,C,D|T], N0, N) :-
    !,
    (   (A\/B\/C\/D) =\= 0
    ->  N1 is N0+1
    ;   N1 = N0
    ),
    audible_frames(T, N1, N).
audible_frames(_, N, N).

%   A source_sink that collects the samples an audio_recorder writes

:- pce_begin_class(test_sample_sink, source_sink,
                   "Collect the recorded samples").

variable(bytes,  int := 0, get, "Bytes received").
variable(offset, int := 0, get, "Next expected <-write_as_file offset").
variable(closed, int := 0, get, "Times ->close_as_file was sent").

initialise(S) :->
    send_super(S, initialise),
    send(S, slot, encoding, octet).

write_as_file(S, At:[int], Data:char_array) :->
    get(S, offset, At),                 % fails if not contiguous
    get(Data, size, Len),
    get(S, bytes, B0), B is B0+Len, send(S, slot, bytes, B),
    Next is At+Len, send(S, slot, offset, Next).

close_as_file(S) :->
    get(S, closed, N0), N is N0+1, send(S, slot, closed, N).

:- pce_end_class(test_sample_sink).

:- begin_tests(record_to_sink).

test(format, [F-R-C == s16-44100-1]) :-
    new(Rec, audio_recorder),
    get(Rec, format, F),
    get(Rec, frequency, R),
    get(Rec, channels, C).
test(file, [ F-C-Loaded-Empty == 44100-1-true-true ]) :-
    write_input(0x40, 200000),
    clear,
    test_file('recorded.wav', File),
    new(R, audio_recorder(message(@prolog, test_sound_level, @arg2))),
    send(R, record, file(File)),
    pump((levels(Ls), member(L, Ls), L > 0)),
    send(R, stop),
    new(S, sound(File)),
    get(S, frequency, F),
    get(S, channels, C),
    get(S, duration, D),
    ( D > 0 -> Loaded = true ; Loaded = D ),
    get(R?sound, duration, RD),
    ( RD =:= 0 -> Empty = true ; Empty = RD ).
test(sink, [ Even-Closed-Level == true-1-true ]) :-
    write_input(0x40, 200000),
    clear,
    new(Sink, test_sample_sink),
    new(R, audio_recorder(message(@prolog, test_sound_level, @arg2))),
    send(R, record, Sink),
    pump((levels(Ls), member(L, Ls), L > 0)),
    pump((get(Sink, bytes, B0), B0 > 4410)),
    send(R, stop),
    get(Sink, bytes, Bytes),
    ( Bytes mod 2 =:= 0 -> Even = true ; Even = Bytes ),
    get(Sink, closed, Closed),
    ( levels(Ls1), member(L1, Ls1), L1 > 0 -> Level = true ; Level = false ).

:- end_tests(record_to_sink).

:- begin_tests(audio_stream_encoded,
               [ condition(send(@pce, has_feature, sound_decoders))
               ]).

test(wav, [F-C == 8000-1]) :-
    clear,
    test_file('stream.wav', File),
    write_tone(File, 0.3),
    encoded_stream(AS),
    send(AS, play),
    stream_file(AS, File, [1]),
    played(AS, 0.3),
    get(AS, frequency, F),
    get(AS, channels, C).
test(au, [F == 8000]) :-
    clear,
    test_file('stream.au', File),
    write_au(File, 0.3),
    encoded_stream(AS),
    send(AS, play),
    stream_file(AS, File, [1]),
    played(AS, 0.3),
    get(AS, frequency, F).
test(slow_writer) :-
    clear,
    test_file('slow.wav', File),
    write_tone(File, 0.5),
    encoded_stream(AS),
    send(AS, play),
    stream_file(AS, File, [0.3, 1]),
    played(AS, 0.5).
test(garbage, [Playing-Finished == (@off)-false]) :-
    clear,
    encoded_stream(AS),
    send(AS, play),
    catch(with_output_to(string(_),
                         ( send(AS, append, string('this is no sound file')),
                           send(AS, close),
                           pump(get(AS, playing, @off))
                         )),
          _, true),
    get(AS, playing, Playing),
    ( finished_sound(AS) -> Finished = true ; Finished = false ).
test(stop_waiting_decoder, [Playing-Fast == (@off)-true]) :-
    test_file('stop.wav', File),
    write_tone(File, 1),
    encoded_stream(AS),
    send(AS, play),
    read_file_to_codes(File, Codes, [type(binary)]),
    length(Half, 4000),
    append(Half, _, Codes),
    atom_codes(Data, Half),
    send(AS, append, string(Data)),
    pause(0.2),                         % the decoder waits for data
    get_time(T0),
    send(AS, stop),
    get_time(T1),
    get(AS, playing, Playing),
    ( T1-T0 < 0.5 -> Fast = true ; Fast = T1-T0 ),
    free(AS).

%   MP3 and FLAC decoders seek while they open the data.  We make the
%   files using ffmpeg, if it is installed.

test(mp3, [condition(has_ffmpeg)]) :-
    converted_stream(mp3).
test(flac, [condition(has_ffmpeg)]) :-
    converted_stream(flac).

:- end_tests(audio_stream_encoded).

has_ffmpeg :-
    process_which(path(ffmpeg), _).

converted_stream(Ext) :-
    clear,
    test_file('conv.wav', Wav),
    write_tone(Wav, 0.5),
    file_name_extension(conv, Ext, Base),
    test_file(Base, File),
    process_create(path(ffmpeg),
                   [ '-loglevel', error, '-y', '-i', Wav, File ],
                   []),
    encoded_stream(AS),
    send(AS, play),
    stream_file(AS, File, [1]),
    played(AS, 0.5).

set_bell_sound(File) :-
    get(@display?class, class_variable, bell_sound, CV),
    send(CV, value, File).
