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

:- module(camera_demo,
          [ camera_demo/0
          ]).
:- use_module(library(pce)).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/** <module> Show and capture the video of a camera

Opens a window that shows the video of a camera.  The menu selects
one of the cameras of `display<-cameras`.  Snapshot copies the current
frame into the snapshot view, from where it can be saved as PNG or
copied to the clipboard.  Record writes the video and the sound of the
microphone to an MP4 file until Stop recording is pressed: the camera
appends its frames to a video_writer and an audio_recorder its
samples.  Stream does the same, but sends them live to a URL, e.g.,
`udp://127.0.0.1:5000`, which the Video player demo can open, or an
`rtmp://` URL of a streaming service.  The status line shows the status of the
camera (e.g., waiting for permission), the frame size and the number
of frames per second.

The camera delivers its frames to an sdl_texture, which an sdl_layer
shows in an sdl_window.  The frames keep the format of the camera:
the renderer converts them to RGB and scales them, so they do not go
through Cairo.  Graphicals in this window are drawn on top of the
video, as the frames per second in the corner show.  The camera's
message counts the frames.

Run with:

    ?- camera_demo.
*/

camera_demo :-
    new(F, camera_demo),
    send(F, open).

:- pce_begin_class(camera_demo, frame,
                   "Show and capture the video of a camera").

variable(camera,   camera*, get, "The running camera").
variable(texture,  sdl_texture, get, "Receives the frames").
variable(frames,   int := 0, both, "Frames since the last status update").
variable(timer,    timer*,  get, "Timer to update the status").
variable(snapshot, image*,  get, "The last snapshot").
variable(writer,   video_writer*, get, "Writes the recording").
variable(recorder, audio_recorder*, get, "Records the microphone").

initialise(F) :->
    send_super(F, initialise, 'Camera'),
    send(F, append, new(V, sdl_window('Video', size(640, 480)))),
    send(V, name, video),
    send(V, background, black),
    send(F, slot, texture, new(T, sdl_texture)),
    send(V, append, sdl_layer(T, @default, fit)),
    send(V, display, new(FPS, text('', left, bold)), point(8, 8)),
    send(FPS, name, fps),
    send(FPS, colour, yellow),
    send(new(S, picture('Snapshot', size(320, 240))), right, V),
    send(S, name, snapshot),
    send(S, scrollbars, none),
    send(S, display, new(_, bitmap), point(0, 0)),
    send(new(D, dialog), below, V),
    send(D, name, controls),
    send(F, fill_dialog, D),
    send(F, slot, timer, new(Timer, timer(1, message(F, update_status)))),
    send(Timer, start),
    send(F, update_status).

fill_dialog(F, D:dialog) :->
    "Create the controls"::
    get(@display, cameras, Cameras),
    send(D, append, new(M, menu(camera, cycle, message(F, fill_sizes)))),
    send(D, append, menu(size, cycle), right),
    (   send(Cameras, empty)
    ->  send(M, append, menu_item(none, @default, 'No camera')),
        send(M, active, @off),
        send(D?size_member, active, @off)
    ;   send(Cameras, for_all, message(M, append, @arg1)),
        send(F, fill_sizes)
    ),
    send(D, append, button(start, message(F, start)), right),
    send(D, append, button(stop, message(F, stop)), right),
    send(D, append, button(snapshot, message(F, snapshot)), right),
    send(D, append, button(save, message(F, save_snapshot)), right),
    send(D, append, button(copy, message(F, copy_snapshot)), right),
    send(D, append, button(record, message(F, toggle_recording)), right),
    send(D, append, button(stream, message(F, toggle_streaming)), right),
    send(D, append, button(quit, message(F, destroy)), right),
    send(D, append, new(Mirror, menu(mirror, toggle,
                                     message(F, mirror, @arg2))), right),
    send(Mirror, append, mirror),
    send(Mirror, show_label, @off),
    send(D, append, new(Status, label(status, ''))),
    send(Status, alignment, left),
    (   send(Cameras, empty)
    ->  send(D?start_member, active, @off)
    ;   true
    ),
    send(F, update_buttons).

fill_sizes(F) :->
    "Fill the size menu with the sizes of the selected camera"::
    get(F, member, controls, D),
    get(D?camera_member, selection, Name),
    get(D, size_member, Menu),
    send(Menu, clear),
    get(new(camera(Name)), sizes, Sizes),
    send(Sizes, for_all,
         message(Menu, append,
                 create(menu_item, @arg1,
                        label := create(string, '%dx%d',
                                        @arg1?width, @arg1?height)))),
    (   get(Menu?members, find,
            and(@arg1?value?width == 640, @arg1?value?height == 480),
            Default)
    ->  send(Menu, selection, Default?value)
    ;   true
    ),
    send(D, layout).

unlink(F) :->
    "Stop the camera and the timer"::
    get(F, timer, T),
    send(T, stop),
    send(F, stop),
    send_super(F, unlink).

mirror(F, Mirror:bool) :->
    "Show the video mirrored, as in a mirror"::
    get(F, member, video, V),
    get(V?layers, head, L),
    (   Mirror == @on
    ->  send(L, flip, horizontal)
    ;   send(L, flip, none)
    ).

snapshot_bitmap(F, BM:bitmap) :<-
    "The bitmap that shows the snapshot"::
    get(F, member, snapshot, S),
    get(S?graphicals, head, BM).

start(F) :->
    "Start the camera selected in the menu"::
    send(F, stop),
    get(F, member, controls, D),
    get(D?camera_member, selection, Name),
    get(D?size_member, selection, Size),
    get(F, member, video, V),
    send(V, size, Size),                    % the video pane fits the frames
    new(C, camera(Name)),
    send(F, use_camera, C),
    send(C, open, Size).

use_camera(F, C:camera) :->
    "Show the video of C"::
    send(F, slot, camera, C),
    send(F, frames, 0),
    send(C, message, message(F, frames, F?frames+1)),
    send(C, status_message, message(F, update_status)),
    send(C, texture, F?texture),
    send(F, update_buttons),
    send(F, update_status).

stop(F) :->
    "Stop the camera"::
    send(F, stop_recording),
    get(F, camera, C),
    (   C \== @nil
    ->  send(F, slot, camera, @nil),
        send(C, free)                   % the texture keeps the last frame
    ;   true
    ),
    send(F, update_buttons),
    send(F, update_status).

snapshot(F) :->
    "Copy the current frame into the snapshot view"::
    get(F?texture, image, Frame),
    get(Frame, size, size(W, H)),
    new(Snap, image(@nil, W, H, pixmap)),
    send(Snap, copy, Frame),
    send(F, slot, snapshot, Snap),
    get(F, member, snapshot, View),         % show it scaled to the view
    get(View?visible, width, VW),
    TH is max(1, round(H*VW/W)),
    get(Snap, scale, size(VW, TH), Thumb),
    get(F, snapshot_bitmap, BM),
    send(BM, image, Thumb),
    send(F, update_buttons).

snapshot_image(F, Image:image) :<-
    "The snapshot; fails if there is none"::
    get(F, snapshot, Image),
    Image \== @nil.

save_snapshot(F) :->
    "Save the snapshot as a PNG file"::
    get(F, snapshot_image, Image),
    get(@finder, file, save, png, File),
    send(Image, save, File, png).

copy_snapshot(F) :->
    "Copy the snapshot to the clipboard"::
    get(F, snapshot_image, Image),
    send(@display, copy, Image),
    send(F, report, status, 'Copied the snapshot to the clipboard').

toggle_recording(F) :->
    "Start or stop recording"::
    (   get(F, writer, @nil)
    ->  send(F, start_recording)
    ;   send(F, stop_recording)
    ).

toggle_streaming(F) :->
    "Start or stop streaming"::
    (   get(F, writer, @nil)
    ->  send(F, start_streaming)
    ;   send(F, stop_recording)
    ).

start_recording(F) :->
    "Record the video and the sound to an MP4 file"::
    get(@finder, file, save, mp4, File),
    send(F, start_writer, File).

start_streaming(F) :->
    "Send the video and the sound live to a URL"::
    new(D, dialog('Stream to')),
    send(D, append, new(TI, text_item(url, 'udp://127.0.0.1:5000'))),
    send(TI, width, 50),
    send(D, append, button(stream, message(D, return, TI?selection))),
    send(D, append, button(cancel, message(D, return, @nil))),
    send(D, default_button, stream),
    send(D, transient_for, F),
    get(D, confirm_centered, F?area?center, URL),
    send(D, destroy),
    URL \== @nil,
    send(F, start_writer, URL).

%   The microphone is optional: without one, the video has no sound.  A
%   writer for a URL is live.

start_writer(F, Target:name) :->
    "Write the video and the sound to a file or URL"::
    get(F, camera, C),
    C \== @nil,
    get(F, member, controls, D),
    get(D?size_member, selection, Size),
    new(W, video_writer(Target, Size, 30)),
    new(R, audio_recorder),
    (   send(R, record, W)
    ->  send(F, slot, recorder, R)
    ;   free(R)
    ),
    send(F, slot, writer, W),
    send(C, writer, W),
    send(F, update_buttons).

stop_recording(F) :->
    "Stop recording and finish the file"::
    get(F, writer, W),
    (   W == @nil
    ->  true
    ;   get(F, camera, C),
        (   C \== @nil
        ->  send(C, writer, @nil)
        ;   true
        ),
        get(F, recorder, R),
        (   R \== @nil
        ->  send(R, stop),
            send(F, slot, recorder, @nil),
            free(R)
        ;   true
        ),
        send(F, slot, writer, @nil),
        get(W, target, Target),
        get(W, live, Live),
        send(W, close),
        free(W),
        (   Live == @on
        ->  send(F, report, status, 'Stopped streaming to %s', Target)
        ;   send(F, report, status, 'Saved the recording in %s', Target)
        )
    ),
    send(F, update_buttons).

update_buttons(F) :->
    "Make the buttons (in)active"::
    get(F, member, controls, D),
    get(F, camera, C),
    (   C == @nil -> Running = @off ; Running = @on ),
    send(D?stop_member, active, Running),
    send(D?snapshot_member, active, Running),
    get(F, writer, W),
    (   W == @nil
    ->  send(D?record_member, active, Running),
        send(D?stream_member, active, Running),
        send(D?record_member, label, 'Record'),
        send(D?stream_member, label, 'Stream')
    ;   get(W, live, @on)
    ->  send(D?record_member, active, @off),
        send(D?stream_member, label, 'Stop streaming')
    ;   send(D?stream_member, active, @off),
        send(D?record_member, label, 'Stop recording')
    ),
    (   get(F, snapshot_image, _) -> Snap = @on ; Snap = @off ),
    send(D?save_member, active, Snap),
    send(D?copy_member, active, Snap).

update_status(F) :->
    "Show the status, frame size and frames per second"::
    get(F, member, controls, D),
    get(D, status_member, L),
    get(F, member, video, V),
    get(V, member, fps, FPS),
    get(F, camera, C),
    (   C == @nil
    ->  (   get(@display, cameras, Cs), send(Cs, empty)
        ->  send(L, selection, 'No camera found')
        ;   send(L, selection, 'Stopped')
        ),
        send(FPS, string, '')
    ;   get(C, status, Status),
        get(F?texture, size, size(W, H)),
        get(F, frames, N),
        send(F, frames, 0),
        send(L, format, '%s, %dx%d, %d frames/second', Status, W, H, N),
        send(FPS, string, string('%d fps', N))
    ).

:- pce_end_class(camera_demo).
