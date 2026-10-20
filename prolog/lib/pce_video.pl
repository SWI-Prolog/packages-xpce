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


:- module(pce_video,
          [ video_stream_writer/4       % +Stream, +Size, -Writer, +Options
          ]).
:- use_module(library(pce)).
:- use_module(library(option)).

/** <module> Video and Prolog streams

A video_writer writes encoded video to a file, a URL or a code object
that receives the data.  This library connects the latter to a Prolog
stream, e.g., the reply to an HTTP request.  To play encoded video
that arrives on a Prolog stream, write it to a video without a
source using pce_open/3:

    new(V, video),
    pce_open(V, write, Out),
    set_stream(Out, type(binary)),
    copy_stream_data(In, Out),
    close(Out).
*/

%!  video_stream_writer(+Stream, +Size, -Writer, +Options) is det.
%
%   Writer is a video_writer for frames of Size that writes the encoded
%   video to the binary Stream.  The data is written from the GUI
%   thread.  Options:
%
%     - frame_rate(+Rate)
%       Frames per second; default 30.
%     - format(+Format)
%       Container, e.g., `mp4` (default, fragmented, which allows
%       playing while the data arrives), `mpegts` or `webm`.
%     - codec(+Codec)
%     - quality(+Quality)
%     - live(+Bool)
%     - bitrate(+BitsPerSecond)
%       As the corresponding methods of class video_writer.
%
%   The stream is not closed when the writer is closed.

video_stream_writer(Stream, Size, Writer, Options) :-
    option(frame_rate(Rate), Options, 30),
    option(codec(Codec), Options, @default),
    option(quality(Quality), Options, @default),
    new(Writer, video_writer(message(@prolog, write_video_data,
                                     prolog(Stream), @arg1),
                             Size, Rate, Codec, Quality)),
    forall(setting(Name, Options, Value),
           send(Writer, Name, Value)).

setting(format,  Options, Value) :- option(format(Value), Options).
setting(live,    Options, Value) :- option(live(Value), Options).
setting(bitrate, Options, Value) :- option(bitrate(Value), Options).

write_video_data(Stream, Data) :-
    get(Data, value, String),
    write(Stream, String),
    flush_output(Stream).
