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

:- module(test_video,
          [ test_video/0,
            test_video/1                         % +Unit
          ]).

/** <module> Playing and writing video

The tests write a short clip with class video_writer and play it with
class video.  The clip shows a red box that moves 8 pixels to the right
on a blue background in each frame.  The clips with audio have a tone.
The tests need a video backend, so they are skipped if xpce was built
without video support.  They use the dummy audio driver of SDL, which
plays in real time without making a sound.

Run with:

    swipl -g test_video -t halt packages/xpce/tests/test_video.pl
*/

%   SDL copies the environment when it is initialised, i.e., when
%   library(pce) is loaded.

:- initialization(setenv('SDL_AUDIO_DRIVER', dummy), now).

:- use_module(library(pce)).
:- use_module(library(pce_video)).
:- use_module(library(plunit)).
:- use_module(library(lists)).

setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).
:- initialization(setup_headless, now).

%   The units.  They wait for playback in real time rather than compute,
%   so CMakeLists.txt reads them from here and runs each in a process of
%   its own, in parallel.

video_test_unit(video_writer).
video_test_unit(video).
video_test_unit(video_audio).
video_test_unit(video_record).
video_test_unit(video_network).
video_test_unit(video_input).
video_test_unit(video_output).

test_video :-
    findall(Unit, video_test_unit(Unit), Units),
    run_tests(Units).

test_video(Unit) :-
    run_tests([Unit]).

video_support :-
    get(@display, video_encoders, Chain),
    \+ send(Chain, empty).

%!  clip(-File) is det.
%
%   File is a clip of 30 frames at 30 frames/second of 320x240 pixels,
%   written when first needed.

:- dynamic clip_file/1.

clip(File) :-
    clip_file(File),
    !.
clip(File) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    write_clip(File, size(320, 240), []),
    asserta(clip_file(File)).

cleanup_clips :-
    forall(retract(clip_file(File)),
           delete_file(File)).

write_clip(File, Size, Options) :-
    option(frames(N), Options, 30),
    option(codec(Codec), Options, @default),
    new(W, video_writer(File, Size, 30, Codec)),
    Last is N-1,
    forall(between(0, Last, I),
           ( frame_image(I, Img),
             send(W, append, Img),
             free(Img)
           )),
    send(W, close),
    free(W).

frame_image(I, Img) :-
    new(Img, image(@nil, 320, 240, pixmap)),
    send(Img, fill, colour(blue)),
    new(B, box(40, 40)),
    send(B, pen, 0),
    send(B, fill, colour(red)),
    X is I*8,
    send(Img, draw_in, B, point(X, 100)),
    free(B).

%!  colour_at(+Video, +X, +Y, -Name) is det.
%
%   Name is `red`, `blue` or `other` for the pixel at X,Y of the current
%   frame of Video.  The colours are not exact after encoding.

colour_at(V, X, Y, Name) :-
    get(V, image, Img),
    pixel_colour(Img, X, Y, Name),
    free(Img).

pixel_colour(Img, X, Y, Name) :-
    get(Img, pixel, X, Y, C),
    get(C, red, R), get(C, green, G), get(C, blue, B),
    (   R > 180, G < 80, B < 80
    ->  Name = red
    ;   B > 180, R < 80, G < 80
    ->  Name = blue
    ;   Name = other
    ).

%!  pump(+Seconds) is det.
%
%   Dispatch events for Seconds, so the media tick shows frames.

pump(Seconds) :-
    get_time(T0),
    repeat,
    ignore(send(@display, dispatch)),
    get_time(T),
    T - T0 > Seconds,
    !.

%!  wait_status(+Video, +Status, +Timeout) is semidet.

wait_status(V, Status, Timeout) :-
    get_time(T0),
    repeat,
    ignore(send(@display, dispatch)),
    get_time(T),
    (   get(V, status, Status)
    ->  !
    ;   T - T0 > Timeout
    ->  !, fail
    ;   fail
    ).

:- begin_tests(video_writer, [ condition(video_support),
                               cleanup(cleanup_clips)
                             ]).

test(write, [Frames-Codec == 30-true]) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(320, 240), 30)),
    forall(between(0, 29, I),
           ( frame_image(I, Img),
             send(W, append, Img),
             free(Img)
           )),
    get(W, frames, Frames),
    get(W, codec, Name),
    (atom(Name) -> Codec = true ; Codec = Name),
    send(W, close),
    free(W),
    delete_file(File).
test(scaled, [Size == size(160, 120)]) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(160, 120), 30)),
    frame_image(0, Img),
    send(W, append, Img),
    send(W, close),
    free(W),
    new(V, video(File)),
    get(V, size, size(SW, SH)),
    Size = size(SW, SH),
    free(V),
    delete_file(File).
test(odd_size, [Size == size(160, 120)]) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(161, 121), 30)),
    frame_image(0, Img),
    send(W, append, Img),
    send(W, close),
    free(W),
    new(V, video(File)),
    get(V, size, size(SW, SH)),
    Size = size(SW, SH),
    free(V),
    delete_file(File).
test(texture, [Size == size(320, 240)]) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(320, 240), 30)),
    frame_image(0, Img),
    new(T, sdl_texture(Img)),
    send(W, append, T),
    send(W, append, T),
    send(W, close),
    free(W),
    new(V, video(File)),
    get(V, size, size(SW, SH)),
    Size = size(SW, SH),
    free(V),
    delete_file(File).
test(closed, fail) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(320, 240), 30)),
    frame_image(0, Img),
    send(W, append, Img),
    send(W, close),
    delete_file(File),
    send(W, append, Img).
test(no_container, fail) :-
    new(W, video_writer('/nonexistent/clip.no_such_container',
                        size(320, 240), 30)),
    frame_image(0, Img),
    send(W, append, Img).

:- end_tests(video_writer).

:- begin_tests(video, [ condition(video_support),
                        cleanup(cleanup_clips)
                      ]).

test(info, [Size-Status-Duration-Rate == size(320, 240)-paused-10-30]) :-
    clip(File),
    new(V, video(File)),
    get(V, size, size(W, H)),
    Size = size(W, H),
    get(V, duration, D),
    Duration is round(D*10),            % tenths of a second
    get(V, frame_rate, R),
    Rate is round(R),
    get(V, status, Status),
    free(V).
test(first_frame, [Colours == [red, blue]]) :-
    clip(File),
    new(V, video(File)),
    send(V, open),
    pump(0.3),
    colour_at(V, 20, 120, C1),
    colour_at(V, 200, 120, C2),
    Colours = [C1, C2],
    free(V).
test(play_to_end, [Status-Ended-Colour == finished-(@on)-red]) :-
    clip(File),
    new(V, video(File)),
    send(V, message, message(@receiver, attribute, ended, @on)),
    send(V, play),
    wait_status(V, finished, 5),
    get(V, status, Status),
    get(V, attribute, ended, Ended),
    colour_at(V, 260, 120, Colour),     % frame 29: box at 232..272
    free(V).
test(seek, [Colours == [blue, red, blue]]) :-
    clip(File),
    new(V, video(File)),
    send(V, position, 0.5),             % frame 15: box at 120..160
    pump(0.3),
    colour_at(V, 100, 120, C1),
    colour_at(V, 140, 120, C2),
    colour_at(V, 180, 120, C3),
    Colours = [C1, C2, C3],
    free(V).
test(seek_back, [Colour == red]) :-
    clip(File),
    new(V, video(File)),
    send(V, position, 0.8),
    pump(0.2),
    send(V, position, 0.1),             % frame 3: box at 24..64
    pump(0.3),
    colour_at(V, 44, 120, Colour),
    free(V).
test(pause, [Status-Same == paused-true]) :-
    clip(File),
    new(V, video(File)),
    send(V, play),
    pump(0.3),
    send(V, pause),
    get(V, position, P1),
    pump(0.2),
    get(V, position, P2),
    (P1 =:= P2 -> Same = true ; Same = false),
    get(V, status, Status),
    free(V).
test(loop, [Status == playing]) :-
    clip(File),
    new(V, video(File)),
    send(V, loop, @on),
    send(V, play),
    pump(1.5),
    get(V, status, Status),
    free(V).
test(replay, [Status-Start == playing-true]) :-
    clip(File),
    new(V, video(File)),
    send(V, play),
    wait_status(V, finished, 5),
    send(V, play),
    get(V, status, Status),
    get(V, position, Pos),
    (Pos < 0.5 -> Start = true ; Start = false),
    free(V).
test(no_file, fail) :-
    new(V, video('/nonexistent/clip.mp4')),
    send(V, open).
test(image_at, [Colours == [red, blue, red]]) :-
    clip(File),
    new(V, video(File)),
    get(V, image, 0.5, I1),            % frame 15: box at 120..160
    pixel_colour(I1, 140, 120, C1),
    pixel_colour(I1, 100, 120, C2),
    get(V, image, 0, I2),              % frame 0: box at 0..40
    pixel_colour(I2, 20, 120, C3),
    Colours = [C1, C2, C3],
    free(V).

:- end_tests(video).


		 /*******************************
		 *	       AUDIO		*
		 *******************************/

%!  tone(+Seconds, +Rate, -Data) is det.
%
%   Data is a 440 Hz tone as 16-bit mono samples.

tone(Seconds, Rate, Data) :-
    N is round(Seconds*Rate),
    findall(B, ( between(1, N, I),
                 V is round(10000*sin(2*pi*440*I/Rate)) /\ 0xffff,
                 ( B is V /\ 0xff ; B is V >> 8 )
               ), Bytes),
    string_codes(Data, Bytes).

%!  av_clip(+VideoSeconds, +AudioSeconds, -File) is det.
%
%   File is a clip with frames for VideoSeconds and a tone for
%   AudioSeconds.

:- dynamic av_clip_file/3.

av_clip(VS, AS, File) :-
    av_clip_file(VS, AS, File),
    !.
av_clip(VS, AS, File) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(320, 240), 30)),
    send(W, audio, 48000, 2),
    tone(AS, 44100, Tone),
    send(W, append_audio, string(Tone), s16, 44100, 1),
    N is round(VS*30)-1,
    forall(between(0, N, I),
           ( frame_image(I, Img),
             send(W, append, Img),
             free(Img)
           )),
    send(W, close),
    free(W),
    asserta(av_clip_file(VS, AS, File)).

cleanup_av_clips :-
    forall(retract(av_clip_file(_, _, File)),
           delete_file(File)).

%!  play_time(+Video, -Seconds) is semidet.
%
%   Play Video to the end and tell how long that took.

play_time(V, Seconds) :-
    get_time(T0),
    send(V, play),
    wait_status(V, finished, 6),
    get_time(T1),
    Seconds is T1-T0.

:- begin_tests(video_audio, [ condition(video_support),
                              cleanup(cleanup_av_clips)
                            ]).

test(write, [Rate-Channels-Codec == 48000-2-true]) :-
    av_clip(1, 1, File),
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File2),
    new(W, video_writer(File2, size(320, 240), 30)),
    send(W, audio),
    frame_image(0, Img),
    send(W, append, Img),
    tone(0.1, 48000, Tone),
    send(W, append_audio, string(Tone), s16, 48000, 1),
    get(W, audio_frequency, Rate),
    get(W, audio_channels, Channels),
    get(W, audio_codec, C),
    (atom(C) -> Codec = true ; Codec = C),
    send(W, close),
    free(W),
    delete_file(File2),
    atom(File).
test(implicit_track, [Rate-Channels == 44100-1]) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(320, 240), 30)),
    tone(0.1, 44100, Tone),
    send(W, append_audio, string(Tone), s16, 44100, 1),
    frame_image(0, Img),
    send(W, append, Img),
    get(W, audio_frequency, Rate),
    get(W, audio_channels, Channels),
    send(W, close),
    free(W),
    delete_file(File).
test(track_too_late, fail) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(320, 240), 30)),
    frame_image(0, Img),
    send(W, append, Img),
    call_cleanup(send(W, audio),
                 ( send(W, close), free(W), delete_file(File) )).
test(no_track, fail) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    new(W, video_writer(File, size(320, 240), 30)),
    frame_image(0, Img),
    send(W, append, Img),
    tone(0.1, 44100, Tone),
    call_cleanup(send(W, append_audio, string(Tone)),
                 ( send(W, close), free(W), delete_file(File) )).
test(read, [HasAudio-Codec == (@on)-true]) :-
    av_clip(1, 1, File),
    new(V, video(File)),
    get(V, has_audio, HasAudio),
    get(V, audio_codec, C),
    (atom(C) -> Codec = true ; Codec = C),
    free(V).
test(no_audio, [HasAudio == @off]) :-
    av_clip(1, 1, File),
    new(V, video(File)),
    send(V, audio, @off),
    get(V, has_audio, HasAudio),
    free(V).
test(silent_clip, [HasAudio == @off]) :-
    clip(File),
    new(V, video(File)),
    get(V, has_audio, HasAudio),
    free(V).
test(play, [InTime == true]) :-
    av_clip(1, 1, File),
    new(V, video(File)),
    play_time(V, T),
    (T > 0.8, T < 1.5 -> InTime = true ; InTime = T),
    free(V).
test(short_audio, [InTime == true]) :-     % the clock goes on without audio
    av_clip(2, 1, File),
    new(V, video(File)),
    play_time(V, T),
    (T > 1.8, T < 2.5 -> InTime = true ; InTime = T),
    free(V).
test(long_audio, [InTime == true]) :-      % we wait for the audio
    av_clip(1, 2, File),
    new(V, video(File)),
    play_time(V, T),
    (T > 1.8, T < 2.5 -> InTime = true ; InTime = T),
    free(V).
test(seek, [InTime-Colour == true-red]) :-
    av_clip(2, 2, File),
    new(V, video(File)),
    send(V, position, 1.0),             % frame 30: box at 240..280
    pump(0.3),
    colour_at(V, 260, 120, Colour),
    play_time(V, T),
    (T > 0.8, T < 1.5 -> InTime = true ; InTime = T),
    free(V).
test(volume, [Volume =:= 0.5]) :-
    av_clip(1, 1, File),
    new(V, video(File)),
    send(V, open),
    send(V, volume, 0.5),
    get(V, volume, Volume),
    free(V).

:- end_tests(video_audio).


		 /*******************************
		 *	     RECORDING		*
		 *******************************/

tmp_video(File) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File).

:- begin_tests(video_record, [condition(video_support)]).

test(window, [Enough-Size-Recording == true-size(200, 100)-(@nil)]) :-
    tmp_video(File),
    new(Win, window('Recorded', size(200, 100))),
    send(Win, background, colour(red)),
    send(Win, open),
    new(W, video_writer(File, size(200, 100), 20)),
    send(W, record, Win),
    pump(0.5),
    send(W, stop),
    get(W, recording, Recording),
    get(W, frames, N),
    (N >= 5 -> Enough = true ; Enough = N),
    send(W, close),
    free(W),
    send(Win?frame, destroy),
    new(V, video(File)),
    get(V, size, size(SW, SH)),
    Size = size(SW, SH),
    free(V),
    delete_file(File).
test(camera, [Frames-Colour-Writer == 3-red-(@nil)]) :-
    tmp_video(File),
    new(W, video_writer(File, size(64, 48), 30)),
    new(C, camera),
    send(C, writer, W),
    forall(between(1, 3, _),
           ( send(C, sdl_test_frame, 64, 48, colour(red)),
             sleep(0.03)
           )),
    get(W, frames, Frames),
    send(C, writer, @nil),
    get(C, writer, Writer),
    send(W, close),
    free(W),
    free(C),
    new(V, video(File)),
    get(V, image, 0, Img),
    pixel_colour(Img, 30, 20, Colour),
    free(V),
    delete_file(File).
test(camera_texture, [Frames == 2]) :-
    tmp_video(File),
    new(W, video_writer(File, size(64, 48), 30)),
    new(C, camera),
    send(C, texture, new(sdl_texture)),
    send(C, writer, W),
    send(C, sdl_test_frame, 64, 48, colour(blue)),
    sleep(0.03),
    send(C, sdl_test_frame, 64, 48, colour(blue), mjpg),
    get(W, frames, Frames),
    send(W, close),
    free(W),
    free(C),
    delete_file(File).
test(recorder_track, [Rate-Channels == Rate0-Channels0]) :-
    tmp_video(File),
    new(W, video_writer(File, size(64, 48), 30)),
    new(R, audio_recorder),
    get(R, frequency, Rate0),
    get(R, channels, Channels0),
    ignore(send(R, record, W)),         % there may be no microphone
    send(R, stop),
    get(W, audio_frequency, Rate),
    get(W, audio_channels, Channels),
    free(R),
    send(W, close),
    free(W),
    (exists_file(File) -> delete_file(File) ; true).

:- end_tests(video_record).


		 /*******************************
		 *	      NETWORK		*
		 *******************************/

%   A local HTTP server serves clips as a file, which can be seeked, as
%   a live stream that arrives in real time and as a stream that never
%   sends data.  Only ffmpeg is known to handle all of these.

:- if(current_prolog_flag(threads, true)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files), [http_reply_file/3]).
:- endif.

:- dynamic http_port/1, ts_clip_file/1.

network_support :-
    current_prolog_flag(threads, true),
    video_support,
    get(@display, video_backend, ffmpeg).

%   The clips are made before the server starts: making them in a
%   handler would delay the response.

start_server :-
    clip(_),
    ts_clip(_),
    message_queue_create(_, [alias(video_stall)]),
    http_server(http_dispatch, [port(localhost:Port)]),
    asserta(http_port(Port)).

stop_server :-
    thread_send_message(video_stall, release),
    forall(retract(http_port(Port)),
           http_stop_server(localhost:Port, [])),
    message_queue_destroy(video_stall),
    forall(retract(ts_clip_file(File)),
           delete_file(File)),
    cleanup_clips.

url(Path, URL) :-
    http_port(Port),
    format(atom(URL), 'http://localhost:~w~w', [Port, Path]).

:- if(current_prolog_flag(threads, true)).
:- http_handler('/clip.mp4', serve_clip, []).
:- http_handler('/live.ts',  serve_live, []).
:- http_handler('/stall.ts', serve_stall, []).
:- endif.

serve_clip(Request) :-
    clip(File),
    http_reply_file(File, [unsafe(true)], Request).

%   Send a 2 second MPEG-TS clip as a chunked response that arrives in
%   about real time, as a live broadcast does.

serve_live(_Request) :-
    ts_clip(File),
    read_file_to_codes(File, Codes, [type(binary)]),
    format('Content-type: video/mp2t~n'),
    format('Transfer-encoding: chunked~n~n'),
    length(Codes, Len),
    Chunk is max(1, Len//20),
    send_chunks(Codes, Chunk).

send_chunks([], _) :- !.
send_chunks(Codes, Chunk) :-
    length(Codes, Len),
    N is min(Len, Chunk),
    length(Prefix, N),
    append(Prefix, Rest, Codes),
    format('~s', [Prefix]),
    flush_output,
    sleep(0.1),
    send_chunks(Rest, Chunk).

serve_stall(_Request) :-
    format('Content-type: video/mp2t~n'),
    format('Transfer-encoding: chunked~n~n'),
    flush_output,
    thread_get_message(video_stall, release, [timeout(5)]).

ts_clip(File) :-
    ts_clip_file(File),
    !.
ts_clip(File) :-
    tmp_file(video, Base),
    file_name_extension(Base, ts, File),
    write_clip(File, size(320, 240), [frames(60)]),
    asserta(ts_clip_file(File)).

%!  statuses(+Video, -Chain) is det.
%
%   Chain collects the statuses of Video.

statuses(V, Ch) :-
    new(Ch, chain),
    send(V, status_message, message(Ch, append, @arg2)).

:- begin_tests(video_network, [ condition(network_support),
                                setup(start_server),
                                cleanup(stop_server)
                              ]).

test(http_file, [Opening-Size-Live-Statuses ==
                 opening-size(320,240)-(@off)-[opening,playing,finished]]) :-
    url('/clip.mp4', URL),
    new(V, video(URL)),
    statuses(V, Ch),
    send(V, open),
    get(V, status, Opening),
    send(V, play),
    wait_status(V, finished, 10),
    get(V, size, size(W, H)),
    Size = size(W, H),
    get(V, live, Live),
    chain_list(Ch, Statuses),
    free(V).
test(http_live, [Live-Duration-Seek-Status == (@on)-0-fail-finished]) :-
    url('/live.ts', URL),
    new(V, video(URL)),
    send(V, play),
    wait_status(V, playing, 10),
    get(V, live, Live),
    get(V, duration, D),
    Duration is round(D),
    (send(V, position, 1.0) -> Seek = true ; Seek = fail),
    wait_status(V, finished, 10),
    get(V, status, Status),
    free(V).
test(http_stall, [Status-Quick == opening-true]) :-
    url('/stall.ts', URL),
    new(V, video(URL)),
    send(V, open),
    pump(0.3),
    get(V, status, Status),
    get_time(T0),
    free(V),                            % must interrupt the blocked open
    get_time(T1),
    (T1-T0 < 2 -> Quick = true ; Quick = T1-T0).

:- end_tests(video_network).


		 /*******************************
		 *	   PROLOG INPUT		*
		 *******************************/

%   A video without a source plays the data written to it.

%!  clip_codes(+Ext, +Frames, -Codes) is det.
%
%   Codes is a clip of Frames frames at 30 frames/second in the format
%   of Ext.

clip_codes(Ext, Frames, Codes) :-
    tmp_file(video, Base),
    file_name_extension(Base, Ext, File),
    write_clip(File, size(320, 240), [frames(Frames)]),
    read_file_to_codes(File, Codes, [type(binary)]),
    delete_file(File).

webm_support :-
    get(@display, video_encoders, Chain),
    ( send(Chain, member, vp9) ; send(Chain, member, av1) ),
    !.

played(Ext, Status-Size) :-
    clip_codes(Ext, 15, Codes),         % plays in real time: keep it short
    new(V, video),
    pce_open(V, write, Out),
    set_stream(Out, type(binary)),
    format(Out, '~s', [Codes]),
    close(Out),
    send(V, play),
    ignore(wait_status(V, finished, 6)),
    get(V, status, Status),
    get(V, size, size(W, H)),
    Size = size(W, H),
    free(V).

:- begin_tests(video_input, [condition(network_support)]).

test(ts, [Result == finished-size(320,240)]) :-
    played(ts, Result).
test(mp4, [Result == finished-size(320,240)]) :-
    played(mp4, Result).
test(webm, [condition(webm_support), Result == finished-size(320,240)]) :-
    played(webm, Result).
test(arrives_while_playing, [Before-After-Played == playing-finished-true]) :-
    clip_codes(ts, 60, Codes),
    length(Codes, Len),
    Half is Len//2,
    length(First, Half),
    append(First, Rest, Codes),
    new(V, video),
    string_codes(Data, First),
    send(V, append, string(Data)),      % unlike pce_open/3, no end
    send(V, play),
    pump(3.0),                          % waits for the rest
    get(V, status, Before),
    get_time(T0),
    pce_open(V, append, Out),
    set_stream(Out, type(binary)),
    format(Out, '~s', [Rest]),
    close(Out),
    ignore(wait_status(V, finished, 6)),
    get_time(T1),
    get(V, status, After),
    (T1-T0 > 0.7 -> Played = true ; Played = T1-T0), % the second second
    free(V).
test(no_data, [Status == closed]) :-
    new(V, video),
    send(V, open),
    send(V, close_as_file),
    pump(0.5),
    get(V, status, Status),
    free(V).
test(free_waiting, [Quick == true]) :-
    new(V, video),
    send(V, append, "partial"),
    pump(0.2),
    get_time(T0),
    free(V),
    get_time(T1),
    (T1-T0 < 1 -> Quick = true ; Quick = T1-T0).
test(has_source, fail) :-
    clip(File),
    new(V, video(File)),
    call_cleanup(send(V, append, "data"), free(V)).

:- end_tests(video_input).


		 /*******************************
		 *	      OUTPUT		*
		 *******************************/

%   A writer whose target is code gives it the encoded data.  We play the
%   data using a video without a source.

:- dynamic written/1.

collect(Data) :-
    get(Data, value, String),
    assertz(written(String)).

written_video(Format, Status-Size) :-
    retractall(written(_)),
    new(W, video_writer(message(@prolog, collect, @arg1),
                        size(320, 240), 30)),
    (Format == default -> true ; send(W, format, Format)),
    forall(between(0, 29, I),
           ( frame_image(I, Img),
             send(W, append, Img),
             free(Img)
           )),
    send(W, close),
    free(W),
    findall(S, retract(written(S)), Strings),
    atomic_list_concat(Strings, Data),
    new(V, video),
    send(V, append, string(Data)),
    send(V, close_as_file),
    send(V, play),
    ignore(wait_status(V, finished, 6)),
    get(V, status, Status),
    get(V, size, size(SW, SH)),
    Size = size(SW, SH),
    free(V).

:- begin_tests(video_output, [condition(network_support)]).

test(code_mp4, [Result == finished-size(320,240)]) :-
    written_video(default, Result).
test(code_mpegts, [Result == finished-size(320,240)]) :-
    written_video(mpegts, Result).
test(stream, [Result == finished-size(320,240)]) :-
    tmp_file(video, Base),
    file_name_extension(Base, mp4, File),
    open(File, write, Out, [type(binary)]),
    video_stream_writer(Out, size(320, 240), W, [format(mp4)]),
    forall(between(0, 29, I),
           ( frame_image(I, Img),
             send(W, append, Img),
             free(Img)
           )),
    send(W, close),
    free(W),
    close(Out),
    new(V, video(File)),
    send(V, play),
    ignore(wait_status(V, finished, 6)),
    get(V, status, Status),
    get(V, size, size(SW, SH)),
    Result = Status-size(SW, SH),
    free(V),
    delete_file(File).
test(live_default, [Live-File == (@on)-(@off)]) :-
    new(W1, video_writer('udp://127.0.0.1:1234', size(320, 240))),
    get(W1, live, Live),
    free(W1),
    new(W2, video_writer('clip.mp4', size(320, 240))),
    get(W2, live, File),
    free(W2).
test(setting_too_late, fail) :-
    retractall(written(_)),
    new(W, video_writer(message(@prolog, collect, @arg1), size(320, 240))),
    frame_image(0, Img),
    send(W, append, Img),
    call_cleanup(send(W, format, mpegts),
                 ( send(W, close), free(W), retractall(written(_)) )).
test(udp_live, [Status-Size-Live == playing-size(320,240)-(@on)]) :-
    Port is 20000+random(20000),
    format(atom(URL), 'udp://127.0.0.1:~w', [Port]),
    new(V, video(URL)),
    send(V, play),
    new(W, video_writer(URL, size(320, 240), 30)),
    get_time(T0),
    forall(between(0, 89, I),           % 3 seconds in real time
           ( frame_image(I, Img),
             send(W, append, Img),
             free(Img),
             get_time(T),
             Wait is T0 + (I+1)/30 - T,
             (Wait > 0 -> pump(Wait) ; true)
           )),
    send(W, close),
    free(W),
    get(V, status, Status),
    get(V?texture, size, size(SW, SH)),
    Size = size(SW, SH),
    get(V, live, Live),
    free(V).

:- end_tests(video_output).
