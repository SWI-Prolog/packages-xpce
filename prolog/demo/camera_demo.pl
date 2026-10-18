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
copied to the clipboard.  The status line shows the status of the
camera (e.g., waiting for permission), the frame size and the number
of frames per second.

The video is shown by displaying the camera's image using a bitmap:
the camera copies each frame into this image, after which the bitmap
shows the new frame.  The camera's message counts the frames.

Run with:

    ?- camera_demo.
*/

camera_demo :-
    new(F, camera_demo),
    send(F, open).

:- pce_begin_class(camera_demo, frame,
                   "Show and capture the video of a camera").

variable(camera,   camera*, get, "The running camera").
variable(frames,   int := 0, both, "Frames since the last status update").
variable(timer,    timer*,  get, "Timer to update the status").
variable(snapshot, image*,  get, "The last snapshot").

initialise(F) :->
    send_super(F, initialise, 'Camera'),
    send(F, append, new(V, picture('Video', size(640, 480)))),
    send(V, name, video),
    send(V, scrollbars, none),
    send(V, display, new(_, bitmap), point(0, 0)),
    send(new(S, picture('Snapshot', size(320, 240))), right, V),
    send(S, name, snapshot),
    send(S, scrollbars, none),
    send(S, display, new(_, bitmap), point(0, 0)),
    send(new(D, dialog), below, V),
    send(D, name, controls),
    send(F, fill_dialog, D),
    send(F, slot, timer, new(T, timer(1, message(F, update_status)))),
    send(T, start),
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
    send(D, append, button(quit, message(F, destroy)), right),
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

video_bitmap(F, BM:bitmap) :<-
    "The bitmap that shows the video"::
    get(F, member, video, V),
    get(V?graphicals, head, BM).

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
    get(F, video_bitmap, BM),
    send(BM, image, C?image),
    send(F, update_buttons),
    send(F, update_status).

stop(F) :->
    "Stop the camera"::
    get(F, camera, C),
    (   C \== @nil
    ->  send(F, slot, camera, @nil),
        send(C, free)                   % the bitmap keeps the last frame
    ;   true
    ),
    send(F, update_buttons),
    send(F, update_status).

snapshot(F) :->
    "Copy the current frame into the snapshot view"::
    get(F, video_bitmap, Video),
    get(Video, image, Frame),
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

update_buttons(F) :->
    "Make the buttons (in)active"::
    get(F, member, controls, D),
    get(F, camera, C),
    (   C == @nil -> Running = @off ; Running = @on ),
    send(D?stop_member, active, Running),
    send(D?snapshot_member, active, Running),
    (   get(F, snapshot_image, _) -> Snap = @on ; Snap = @off ),
    send(D?save_member, active, Snap),
    send(D?copy_member, active, Snap).

update_status(F) :->
    "Show the status, frame size and frames per second"::
    get(F, member, controls, D),
    get(D, status_member, L),
    get(F, camera, C),
    (   C == @nil
    ->  (   get(@display, cameras, Cs), send(Cs, empty)
        ->  send(L, selection, 'No camera found')
        ;   send(L, selection, 'Stopped')
        )
    ;   get(C, status, Status),
        get(C?image, size, size(W, H)),
        get(F, frames, N),
        send(F, frames, 0),
        send(L, format, '%s, %dx%d, %d frames/second', Status, W, H, N)
    ).

:- pce_end_class(camera_demo).
