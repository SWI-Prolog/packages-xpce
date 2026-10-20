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
:- use_module(library(help_message)).
:- use_module(demo_buttons).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/** <module> Show and capture the video of a camera

Opens a window that shows the video of a camera.  The menus select one
of the cameras of `display<-cameras` and the size of the frames, and
Start starts the camera.  Snapshot copies the current frame into the
snapshot view; its menu saves the snapshot as PNG or copies it to the
clipboard.  Record writes the video and the sound of the microphone to
an MP4 file until Stop is pressed: the camera appends its frames to a
video_writer and an audio_recorder its samples.  Its menu also streams
them live to a URL, e.g., `udp://127.0.0.1:5000`, which the Video player
demo can open, or an `rtmp://` URL of a streaming service.  The video
shows REC or LIVE while recording or streaming.  The status line shows
the status of the camera (e.g., waiting for permission), the frame size
and the number of frames per second.  All controls have a tooltip.

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
    send(V, display, new(Rec, text('', right, bold))),
    send(Rec, name, rec),
    send(Rec, colour, red),
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

%   The controls: the camera, its size and start/stop, the mirror toggle,
%   snapshot and record.  Snapshot and Record are split buttons: the
%   button does the common action, the menu of its arrow the others.

fill_dialog(F, D:dialog) :->
    "Create the controls"::
    get(@display, cameras, Cameras),
    send(D, append, new(M, menu(camera, cycle, message(F, fill_sizes)))),
    send(M, help_message, tag, 'Camera to use'),
    send(D, append, new(Size, menu(size, cycle)), right),
    send(Size, help_message, tag, 'Size of the frames to ask the camera for'),
    (   send(Cameras, empty)
    ->  send(M, append, menu_item(none, @default, 'No camera')),
        send(M, active, @off),
        send(Size, active, @off)
    ;   send(Cameras, for_all, message(M, append, @arg1)),
        send(F, fill_sizes)
    ),
    send(D, append, state_button(camera_on_off, message(F, toggle_camera)),
         right),
    send(D, append, new(Mirror, menu(mirror, toggle,
                                     message(F, mirror, @arg2))), right),
    send(Mirror, append, mirror),
    send(Mirror, show_label, @off),
    send(Mirror, help_message, tag, 'Show the video as in a mirror'),

    send(D, append, new(Snap, button(snapshot, message(F, snapshot))),
         right),
    send(Snap, help_message, tag,
         'Copy the current frame into the snapshot view.\n\c
          The menu saves or copies the snapshot'),
    send(Snap, popup, new(SP, popup)),
    send_list(SP, append,
              [ menu_item(save, message(F, save_snapshot),
                          'Save as PNG ...',
                          condition := message(F, has_snapshot)),
                menu_item(copy, message(F, copy_snapshot),
                          'Copy to the clipboard',
                          condition := message(F, has_snapshot))
              ]),
    send(D, append, new(Rec, state_button(record, message(F, record_button))),
         right),
    send(Rec, popup, new(RP, popup)),
    send_list(RP, append,
              [ menu_item(file, message(F, start_recording),
                          'Record to a file ...',
                          condition := message(F, can_record)),
                menu_item(url, message(F, start_streaming),
                          'Stream to a URL ...',
                          condition := message(F, can_record))
              ]),
    send(D, append, new(Q, button(quit, message(F, destroy))), right),
    send(Q, help_message, tag, 'Close this window'),
    send(D, append, new(Status, label(status, ''))),
    send(Status, alignment, left),
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

toggle_camera(F) :->
    "Start or stop the camera"::
    (   get(F, camera, @nil)
    ->  send(F, start)
    ;   send(F, stop)
    ).

has_snapshot(F) :->
    "True if there is a snapshot (condition of the snapshot menu)"::
    get(F, snapshot_image, _).

can_record(F) :->
    "True if the camera runs and we are not recording"::
    \+ get(F, camera, @nil),
    get(F, writer, @nil).

record_button(F) :->
    "Record to a file or stop recording or streaming"::
    (   get(F, writer, @nil)
    ->  send(F, start_recording)
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
    get(F, frame_size, Size),
    new(W, video_writer(Target, Size, 30)),
    new(R, audio_recorder),
    (   send(R, record, W)
    ->  send(F, slot, recorder, R)
    ;   free(R)
    ),
    send(F, slot, writer, W),
    send(C, writer, W),
    send(F, update_buttons).

%   The size of the frames the camera delivers, which may differ from
%   the size we asked for.

frame_size(F, Size:size) :<-
    "Size of the frames of the camera"::
    get(F?texture, size, Size0),
    (   get(Size0, width, W), W > 0
    ->  Size = Size0
    ;   get(F, member, controls, D),
        get(D?size_member, selection, Size)
    ).

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

%   The labels show what a button does now: start or stop the camera,
%   record or stop recording.  The tooltips change with them.

update_buttons(F) :->
    "Show the state in the labels, tooltips and REC/LIVE indicator"::
    get(F, member, controls, D),
    get(F, camera, C),
    get(@display, cameras, Cameras),
    get(D, camera_on_off_member, OnOff),
    (   C == @nil
    ->  Running = @off,
        send(OnOff, label, '\u25B6 Start'),
        send(OnOff, help_message, tag, 'Start the selected camera')
    ;   Running = @on,
        send(OnOff, label, '\u25A0 Stop'),
        send(OnOff, help_message, tag, 'Stop the camera')
    ),
    (   send(Cameras, empty)
    ->  send(OnOff, active, @off)
    ;   true
    ),
    send(D?camera_member, active, Running?negate),
    send(D?size_member, active, Running?negate),
    send(D?snapshot_member, active, Running),
    get(D, record_member, Rec),
    get(F, member, video, V),
    get(V, member, rec, Indicator),
    get(F, writer, W),
    (   W == @nil
    ->  send(Rec, active, Running),
        send(Rec, label, '\u25CF Record'),
        send(Rec, help_message, tag,
             'Record the video and the microphone to an MP4 file.\n\c
              The menu also streams them live to a URL'),
        send(Indicator, string, '')
    ;   send(Rec, label, '\u25A0 Stop'),
        (   get(W, live, @on)
        ->  send(Rec, help_message, tag, 'Stop streaming'),
            send(Indicator, string, '\u25CF LIVE')
        ;   send(Rec, help_message, tag, 'Stop recording'),
            send(Indicator, string, '\u25CF REC')
        )
    ),
    get(V?visible, width, VW),
    send(Indicator, position, point(VW-Indicator?width-8, 8)),
    send(D, layout).

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
