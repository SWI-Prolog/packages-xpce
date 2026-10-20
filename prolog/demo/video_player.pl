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


:- module(video_player,
          [ video_player/0,
            video_player/1                  % +File
          ]).
:- use_module(library(pce)).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/** <module> Play a video file

A simple video player.  Open a file using the Open button or by
dropping it on the window.  The menu of the Open button also opens a
network stream (http, HLS, RTSP, UDP, ...) by URL.  Play/Pause starts and stops the video, the
slider below the video shows the position and can be dragged to seek
and the volume slider sets the volume.  The space bar plays and pauses.
While the position slider is dragged, a thumbnail shows the frame at
the position of the slider.

The video delivers its frames to an sdl_texture, which an sdl_layer
shows in an sdl_window, scaled to fit the window.  The frames are not
converted on the CPU: the renderer converts and scales them.  The time
of the video is in the top-left corner: graphicals in an sdl_window are
drawn on top of the video.  The thumbnail is a second video of the same
file without sound, shown by a small layer on top of the first.  Its
frames are decoded by its own thread, so dragging stays smooth.

The sliders edit floats, so their bounds are real objects: in xpce,
0.0 is the integer 0.

Run with:

    ?- video_player.
    ?- video_player('movie.mp4').
*/

video_player :-
    new(F, video_player),
    send(F, open).

video_player(File) :-
    new(F, video_player),
    send(F, open),
    send(F, load, File).

:- pce_begin_class(video_player, frame, "Play a video file").

variable(video,	      video*,	  get, "The video being played").
variable(thumb,	      video*,	  get, "Video for the thumbnail").
variable(thumb_layer, sdl_layer*, get, "Shows the thumbnail").
variable(timer,	      timer,	  get, "Updates the position").

class_variable(thumb_width, int, 192, "Width of the thumbnail").

initialise(F) :->
    send_super(F, initialise, 'Video player'),
    send(F, append, new(W, sdl_window('Video', size(640, 360)))),
    send(W, name, screen),
    send(W, background, black),
    send(W, display, new(T, text('', left, bold)), point(8, 8)),
    send(T, name, time),
    send(T, colour, yellow),
    send(W, recogniser, handler(drop_file,
                                message(F, load, @event?path))),
    send(W, recogniser, new(KB, key_binding)),
    send(KB, function, ' ', message(F, toggle)),
    send(new(D, dialog), below, W),
    send(D, name, controls),
    send(F, fill_dialog, D),
    send(F, slot, timer, new(Timer, timer(0.2, message(F, update)))),
    send(Timer, start),
    send(F, update).

fill_dialog(F, D:dialog) :->
    "Create the controls"::
    send(D, append, new(O, button(open, message(F, open_file)))),
    send(O, popup, new(P, popup)),          % a split button
    send_list(P, append,
              [ menu_item(file, message(F, open_file), 'File ...'),
                menu_item(url,  message(F, open_url),  'URL ...')
              ]),
    send(D, append, new(B, button(play, message(F, toggle))), right),
    play_label(play, Label),
    send(B, label, Label),
    send(D, append, video_position_slider(message(F, scrub, @arg1)),
         right),
    send(D, append, new(V, slider(volume, real(0), real(1), real(1),
                                  message(F, volume, @arg1))), right),
    send(V, width, 80),
    send(V, show_value, @off),
    send(D, append, button(quit, message(F, destroy)), right).

unlink(F) :->
    "Stop the timer and close the video"::
    send(F?timer, stop),
    send(F, close_video),
    send_super(F, unlink).

open_file(F) :->
    "Ask for a video file and load it"::
    get(@finder, file, open,
        tuple('Video files', chain(mp4, mov, mkv, webm, avi, ts)), File),
    send(F, load, File).

open_url(F) :->
    "Ask for the URL of a network stream and load it"::
    new(D, dialog('Open URL')),
    send(D, append, new(TI, text_item(url, ''))),
    send(TI, width, 50),
    send(D, append, button(open, message(D, return, TI?selection))),
    send(D, append, button(cancel, message(D, return, @nil))),
    send(D, default_button, open),
    send(D, transient_for, F),
    get(D, confirm_centered, F?area?center, URL),
    send(D, destroy),
    URL \== @nil,
    get(URL, strip, Stripped),
    \+ get(Stripped, size, 0),
    send(F, load, Stripped).

%   A network stream is opened in the background.  When it is open, its
%   duration is known and we make the thumbnail video.

load(F, File:name) :->
    "Show the first frame of a video file or stream"::
    send(F, close_video),
    new(V, video(File)),
    send(V, status_message, message(F, video_status, @arg2)),
    get(F, member, controls, D),
    send(V, volume, D?volume_member?selection),
    send(F, slot, video, V),            % ->open may call ->video_status
    send(V, open),
    get(F, member, screen, W),
    send(W, append, sdl_layer(V?texture, @default, fit)),
    file_base_name(File, Base),
    send(F, label, string('Video player: %s', Base)),
    (   get(V, status, opening)
    ->  true
    ;   send(F, prepare)
    ),
    send(F, update).

video_status(F, Status:name) :->
    "The video changed status; prepare when it is open"::
    (   memberchk(Status, [paused, playing]),
        get(F, thumb, @nil)
    ->  send(F, prepare)
    ;   true
    ),
    send(F, update).

%   A live stream cannot seek, so it has no thumbnail.

prepare(F) :->
    "Set the range of the slider and make the thumbnail video"::
    get(F, video, V),
    get(F, member, controls, D),
    get(V, duration, Duration),
    get(D, position_member, P),
    High is max(Duration, 0.1),
    send(P, high, real(High)),
    (   get(V, live, @off)
    ->  get(V, source, File),
        new(Thumb, video(File)),
        send(Thumb, audio, @off),
        send(Thumb, open),
        send(F, slot, thumb, Thumb),
        get(F, member, screen, W),
        send(W, append,
             new(TL, sdl_layer(Thumb?texture, area(0,0,1,1), fit))),
        send(TL, displayed, @off),
        send(F, slot, thumb_layer, TL)
    ;   true
    ).

close_video(F) :->
    "Stop and close the current video"::
    get(F, video, V),
    (   V == @nil
    ->  true
    ;   send(F, slot, video, @nil),
        send(F, slot, thumb_layer, @nil),
        get(F, member, screen, W),
        send(W?layers, for_all, message(@arg1, free)),
        send(V, free),
        get(F, thumb, Thumb),
        send(F, slot, thumb, @nil),
        (   Thumb == @nil
        ->  true
        ;   send(Thumb, free)
        )
    ).

toggle(F) :->
    "Play or pause"::
    get(F, video, V),
    (   V == @nil
    ->  true
    ;   get(V, status, playing)
    ->  send(V, pause)
    ;   send(V, play)
    ),
    send(F, update).

%!  ->scrub: Seconds
%
%   The position slider was moved to Seconds.  Show the frame at that
%   time as a thumbnail above the slider.  The thumbnail video shows the
%   frame after it decoded it.

scrub(F, Seconds:num) :->
    "Show the thumbnail of the frame at Seconds"::
    get(F, thumb, Thumb),
    (   Thumb == @nil
    ->  true
    ;   get(F, thumb_layer, TL),
        get(F, member, screen, W),
        get(W, size, size(WW, WH)),
        get(F, thumb_width, TW),
        get(Thumb, size, size(VW, VH)),
        TH is round(TW*VH/max(1, VW)),
        get(F?video, duration, Duration),
        X is round(Seconds/max(0.1, Duration)*(WW-TW)),
        Y is WH - TH - 10,
        send(TL, area, area(X, Y, TW, TH)),
        send(TL, displayed, @on),
        send(Thumb, position, Seconds),
        send(F, show_time, Seconds)
    ).

%!  ->end_scrub
%
%   The position slider was released: hide the thumbnail and continue at
%   the position of the slider.

end_scrub(F) :->
    "Seek to the slider's position"::
    get(F, video, V),
    (   V == @nil
    ->  true
    ;   get(F, thumb_layer, TL),
        (   TL == @nil
        ->  true
        ;   send(TL, displayed, @off)
        ),
        get(F, member, controls, D),
        get(D?position_member, selection, Seconds),
        send(V, position, Seconds)
    ).

volume(F, Volume:num) :->
    "Set the volume"::
    get(F, video, V),
    (   V == @nil
    ->  true
    ;   send(V, volume, Volume)
    ).

update(F) :->
    "Show the position and the state of the video"::
    get(F, member, controls, D),
    get(F, member, screen, W),
    get(W, member, time, T),
    get(F, video, V),
    (   V == @nil
    ->  send(D?play_member, active, @off),
        send(D?position_member, active, @off),
        send(T, string, 'Open a video or drop it here')
    ;   send(D?play_member, active, @on),
        (   get(V, status, playing)
        ->  play_label(pause, Label)
        ;   play_label(play, Label)
        ),
        send(D?play_member, label, Label),
        (   get(V, status, opening)
        ->  send(D?position_member, active, @off),
            send(T, string, 'Opening ...')
        ;   get(V, live, @on)
        ->  send(D?position_member, active, @off),
            get(V, position, Pos),
            time_string(Pos, PS),
            send(T, string, string('LIVE %s', PS))
        ;   send(D?position_member, active, @on),
            (   get(F, thumb_layer, TL),
                TL \== @nil,
                get(TL, displayed, @on)
            ->  true                    % the user drags the slider
            ;   get(V, position, Pos),
                send(D?position_member, selection, real(Pos)),
                send(F, show_time, Pos)
            )
        )
    ).

show_time(F, Seconds:num) :->
    "Show the time and the duration"::
    get(F, member, screen, W),
    get(W, member, time, T),
    (   get(F?video, duration, Duration)
    ->  time_string(Seconds, PS),
        time_string(Duration, DS),
        send(T, string, string('%s / %s', PS, DS))
    ;   true
    ).

%   The labels of the play button are symbols, so the button does not
%   change its width.  The pause symbol U+23F8 is often missing in text
%   fonts and drawn as an emoji, so we use two heavy bars.

play_label(play,  '\u25B6').
play_label(pause, '\u275A\u275A').

time_string(Seconds, String) :-
    S is round(Seconds),
    M is S // 60,
    SS is S mod 60,
    format(string(String), '~d:~|~`0t~d~2+', [M, SS]).

:- pce_end_class(video_player).


%   The position slider runs its message while it is dragged and tells
%   the frame when it is released.

:- pce_begin_class(video_position_slider, slider,
                   "Slider for the position in a video").

initialise(S, Message:code) :->
    send_super(S, initialise, position, real(0), real(1), real(0), Message),
    send(S, drag, @on),
    send(S, width, 300),
    send(S, show_value, @off),
    send(S, show_label, @off).

event(S, Ev:event) :->
    "Tell the frame when the slider is released"::
    (   send_super(S, event, Ev)
    ->  Done = true
    ;   Done = false
    ),
    (   send(Ev, is_a, ms_left_up)
    ->  send(S?frame, end_scrub)
    ;   true
    ),
    Done == true.

:- pce_end_class(video_position_slider).
