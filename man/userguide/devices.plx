\chapter{Input devices and media}		\label{sec:devices}

The window system reports more than keys and a mouse: a graphics
tablet reports the pressure and tilt of a pen, a touch screen reports
several fingers, a trackpad reports precise scrolling and zoom gestures
and a game controller has its own buttons and sticks.  This chapter
describes how \product{} delivers these as events, so applications
that only use the mouse keep working, while applications that wish to
use the additional information can do so.  It also describes the
classes for other media and devices: \class{sound} and
\class{audio_recorder} for audio, \class{camera} for video, images on
the clipboard and \class{tray_icon} for the system tray.

The examples of this chapter are in the file
\file{examples/devices.pl} of the User Guide.


\section{Events and input devices}		\label{sec:inputdevices}

\index{event,device}\index{input device}%
Each \class{event} refers to the device that generated it using
`event <-device'.  This is an \class{input_device}, whose <-kind is
one of \const{mouse}, \const{pen}, \const{touch}, \const{gamepad} or
\const{keyboard}.  Events of the mouse refer to @mouse_device and
keyboard events to @keyboard_device.  A pen, a touch screen or a
gamepad is represented by its own \class{input_device} object, which is
created when the window system reports the device.  `display
<-input_devices' lists the known devices.

Data that is specific to the kind of event is held by `event
<-detail'.  This is an \class{event_detail} object or @nil.  The event
\jargon{delegates} to its detail: methods that class \class{event} does
not define are executed on the detail.  Thus, the pressure of a pen is
requested from the event itself:

\begin{code}
event(W, Ev:event) :->
    (   send(Ev, is_a, ms_left_drag),
        get(Ev, device, Device),
        get(Device, kind, pen)
    ->  get(Ev, pressure, Pressure),
        ...
    ;   send_super(W, event, Ev)
    ).
\end{code}

\tabref{eventdetail} lists the detail classes.  Note that a method of
the detail is only defined for the events that have this detail.
Calling, e.g., `event <-pressure' on a keyboard event raises an error,
so test the kind of event first, as above.

\begin{table}
\begin{center}
\begin{tabularlp}{\class{gamepad_input}}
\hline
\bf Class		& \bf Events and methods \\
\hline
\class{drop_data}	& drop_* events of files or text dropped from
			  other applications: <-path, <-text, <-source.
			  See \secref{dragdrop}. \\
\class{key_data}	& keyboard events and key_up: <-key and
			  <-repeat.  See \secref{keyup}. \\
\class{wheel_data}	& wheel and horizontal_wheel: precise scroll
			  distance <-dx and <-dy and <-flipped.  See
			  \secref{scrolling}. \\
\class{pen_state}	& events of a pen: <-pressure, <-xtilt, <-ytilt,
			  <-hover_distance, <-barrel_rotation,
			  <-slider, <-tangential_pressure, <-eraser,
			  <-barrel_buttons.  See \secref{pen}. \\
\class{touch_point}	& events of a touch screen: <-finger,
			  <-pressure, <-dx, <-dy.  See \secref{touch}. \\
\class{pinch_data}	& pinch_* events of a trackpad zoom gesture:
			  <-scale.  See \secref{touch}. \\
\class{gamepad_input}	& gamepad_* events: <-control, <-value.  See
			  \secref{gamepad}. \\
\hline
\end{tabularlp}
\end{center}
\caption{Classes of `event <-detail'}
\label{tab:eventdetail}
\end{table}

\index{device_added}\index{device_removed}%
When a device is added or removed, e.g., a gamepad is plugged in, the
window that has the keyboard focus in each frame receives a
\const{device_added} or \const{device_removed} event, whose `event
<-device' is the device concerned.

\index{key_up}\index{repeat,key}%
\label{sec:keyup}%
Releasing a key generates a \const{key_up} event.  It goes to the
keyboard focus, like the key-down, and `event <-key' is the key of
the key-down it releases.  A key_up is not a keyboard event, so key
bindings, text items and editors do not see it; test for it using
`event ->is_a: key_up'.  `event <-repeat' is @on for a key-down that
is auto-repeated because the key is held.  Together, these allow a
window to track which keys are down.  See \class{key_data}.

Each pointing device has its own state.  For example, if a pen and the
mouse are used at the same time, a click of the mouse between two
clicks of the pen does not make the second click of the pen a double
click.


\section{Positions and scrolling}		\label{sec:scrolling}

\index{sub-pixel}\index{subpixel_events}%
The window system reports positions as floating point numbers.  By
default, `event <-x', `event <-y', `event <-position', etc.\ return
whole pixels, as \product{} always did.  A window with `window
<->subpixel_events' set to @on gets the fractional position.  This
is useful for drawing with a pen or on a high-resolution screen,
where the fraction of a pixel matters.

\index{wheel}\index{trackpad}\index{smooth scrolling}%
A mouse wheel generates a \const{wheel} event for each notch, and
tilting the wheel or scrolling sideways on a trackpad a
\const{horizontal_wheel} event.  `event <-rotation' is 15 for each
notch.  Trackpads and high-resolution wheels scroll in smaller steps.
By default, these steps are added up and an event is generated for
each whole notch.  Its \class{wheel_data} detail holds the precise
distance in both directions as `event <-dx' and `event <-dy'.

A window with `window <->smooth_scroll' set to @on receives a wheel
event for every step.  The <-rotation of these events may be a
fraction of a notch.  The built-in handlers add up the fractions, so
a window scrolls by whole pixels, an editor or list browser by whole
lines and a cycle menu by whole notches.  Code that handles wheel
events itself in a window with smooth scrolling should do the same or
use the precise distance, e.g., to zoom smoothly.


\section{Pens, touch screens and gestures}	\label{sec:pen}

\index{pen}\index{graphics tablet}\index{stylus}%
A pen of a graphics tablet or touch screen acts as a mouse.  Its tip
generates \const{ms_left_down}, \const{ms_left_drag} and
\const{ms_left_up}, the first and second button on its barrel the
\const{ms_right_*} and \const{ms_middle_*} events and moving the pen
while it is close to the surface generates \const{loc_move}.  Thus,
gestures such as \class{click_gesture} and \class{move_gesture} work
with a pen.  The \class{pen_state} detail of these events holds the
state of the pen: the pressure of its tip, its tilt, whether its
eraser end is used, etc.  A pen also generates the events
\const{pen_proximity_in} and \const{pen_proximity_out} if it comes
near or leaves the surface and \const{pen_axis} if, e.g., the pressure
changes while the pen does not move.

The example below draws with a pen.  The width of the line follows the
pressure.  The mouse, which has no pressure, draws with a width of 2.

\begin{code}
:- pce_begin_class(sketch_pad, picture, "Draw using a pen").

event(P, Ev:event) :->
    (   send(Ev, is_a, ms_left_down)
    ->  get(Ev, position, P, point(X, Y)),
        send(P, attribute, last, point(X, Y))
    ;   send(Ev, is_a, ms_left_drag),
        get(P, attribute, last, point(X0, Y0))
    ->  get(Ev, position, P, point(X, Y)),
        stroke_width(Ev, W),
        new(L, line(X0, Y0, X, Y)),
        send(L, pen, W),
        send(P, display, L),
        send(P, attribute, last, point(X, Y))
    ;   send_super(P, event, Ev)
    ).

stroke_width(Ev, W) :-
    get(Ev, device, Dev),
    get(Dev, kind, pen),
    !,
    get(Ev, pressure, Pressure),
    W is max(1, round(Pressure*8)).
stroke_width(_, 2).

:- pce_end_class(sketch_pad).
\end{code}

\subsection{Touch screens}			\label{sec:touch}

\index{touch screen}\index{finger}%
The first finger that touches a touch screen also acts as the mouse:
it generates \const{ms_left_down}, \const{ms_left_drag} and
\const{ms_left_up}.  While it is down, other fingers generate
\const{touch_down}, \const{touch_motion}, \const{touch_up} and
\const{touch_cancel}.  These are delivered to the window of the first
finger.  The \class{touch_point} detail of all these events holds
`event <-finger', which identifies the finger while it touches the
screen, as well as its pressure and motion.  If the first finger is
lifted while other fingers are still on the screen, no finger acts as
the mouse until all fingers are lifted.

\subsection{Pinch gestures}

\index{pinch}\index{zoom}%
Moving two fingers apart or together on a trackpad generates
\const{pinch_begin}, a \const{pinch_update} for each step and
\const{pinch_end}.  These are mouse events at the position of the
mouse.  `event <-scale' of a pinch_update is the change of scale
since the previous update: above 1 zooms in.  The recogniser of the
library \pllib{pan_zoom} zooms a \class{figure} on a pinch as well as
on the mouse wheel and pans it when dragged with the left button:

\begin{code}
?- new(P, picture), send(P, open),
   new(F, figure), send(F, display, box(100, 100)),
   send(F, recogniser, @pan_zoom_recogniser),
   send(P, display, F, point(50, 50)).
\end{code}


\section{Gamepads}				\label{sec:gamepad}

\index{gamepad}\index{game controller}%
`display <-gamepads' enables the support for game controllers and
returns a chain holding the connected \class{gamepad} objects.  Until
it is called, \product{} does not use gamepads, as starting the
gamepad support may be slow and, on some systems, asks the user for
permission.  Gamepads that are connected later are added, and the
windows with the keyboard focus receive a \const{device_added} event
(see \secref{inputdevices}).

As keys, the buttons and sticks of a gamepad generate events for the
window with the keyboard focus, and only while the application has the
keyboard focus.  The events are \const{gamepad_button_down},
\const{gamepad_button_up} and \const{gamepad_axis}.  Their
\class{gamepad_input} detail holds `event <-control', the name of the
button or axis, and `event <-value': 1 or 0 for a button and the
position of an axis.  The names follow the position of the controls
rather than their label, so they are the same for controllers of
different brands: \const{south} is the bottom face button, labelled A
on an Xbox controller and with a cross on a PlayStation controller.
\tabref{gamepadcontrols} lists the names.

\begin{table}
\begin{center}
\begin{tabular}{|l|p{0.65\linewidth}|}
\hline
Face buttons	& \const{south}, \const{east}, \const{west}, \const{north} \\
Other buttons	& \const{back}, \const{guide}, \const{start},
		  \const{left_stick}, \const{right_stick},
		  \const{left_shoulder}, \const{right_shoulder},
		  \const{dpad_up}, \const{dpad_down}, \const{dpad_left},
		  \const{dpad_right}, \const{misc1} \ldots{} \const{misc6},
		  \const{left_paddle1}, \const{left_paddle2},
		  \const{right_paddle1}, \const{right_paddle2},
		  \const{touchpad} \\
Sticks (-1..1)	& \const{left_x}, \const{left_y}, \const{right_x},
		  \const{right_y}; positive is right and down \\
Triggers (0..1)	& \const{left_trigger}, \const{right_trigger} \\
\hline
\end{tabular}
\end{center}
\caption{Names of the buttons and axes of a gamepad}
\label{tab:gamepadcontrols}
\end{table}

A game typically polls the state of the controller from a
\class{timer} instead.  The same applies to the keyboard: `keyboard
<-key_down' tells whether a key is held down, e.g.,
\exam{get(@keyboard_device, key_down, cursor_left, @on)}.  `gamepad <-button' and `gamepad <-axis' return
the current state of a button or axis.  `gamepad ->rumble' makes the
controller vibrate and `gamepad ->led' sets the colour of its light,
if it has these.  The example below moves a box using the left stick
or the direction pad:

\begin{code}
gamepad_example :-
    get(@display, gamepads, Pads),
    (   get(Pads, head, Pad)
    ->  true
    ;   print_message(warning, format('No gamepad', [])), fail
    ),
    new(P, picture('Gamepad')),
    send(P, display, new(B, box(20, 20)), point(100, 100)),
    send(B, fill, colour(red)),
    send(P, open),
    new(T, timer(0.02, message(@prolog, move_box, Pad, B))),
    send(P, attribute, timer, T),           % freed with the picture
    send(T, start).

move_box(Pad, B) :-
    get(Pad, axis, left_x, X0),
    get(Pad, axis, left_y, Y0),
    dpad(Pad, dpad_left, dpad_right, X0, X),
    dpad(Pad, dpad_up, dpad_down, Y0, Y),
    DX is round(X*5), DY is round(Y*5),
    send(B, relative_move, point(DX, DY)).

dpad(Pad, Minus, Plus, V0, V) :-
    (   get(Pad, button, Minus, @on) -> V = -1
    ;   get(Pad, button, Plus, @on)  -> V = 1
    ;   V = V0
    ).
\end{code}


\section{Sound}					\label{sec:sound}

\index{sound}\index{audio}\index{WAV}\index{MP3}%
A \class{sound} holds audio, loaded from a sound file.  If \product{}
is built with SDL3_sound, it loads WAV, MP3, Ogg Vorbis, FLAC, AIFF
and various other formats.  Otherwise it only loads WAV files.
`display <-sound_file_extensions' lists the extensions of the files
that can be loaded.  `sound ->play'
plays it on the default audio output and returns immediately.  A sound
may be played several times at the same time, e.g., for the sound
effects of a game.  `sound ->stop' stops all plays of the sound and
`sound <->message' is executed when a play is finished:

\begin{code}
?- new(S, sound('click.wav')),
   send(S, message, message(@pce, write_ln, done)),
   send(S, play, 0.5).                  % at half volume
\end{code}

An \class{audio_recorder} records from the default audio input,
normally the microphone, into its <-sound.  While recording, `audio_recorder
<->level_message' is executed about 40 times per second with the
recorder and the input level, a number between 0 and 1.  The example
below shows the level using a \class{slider} and saves the recording
when the user presses \textsf{Stop}:

\begin{code}
record_example :-
    new(D, dialog('Recorder')),
    send(D, append, new(Level, slider(level, 0, 100, 0))),
    new(R, audio_recorder(
               message(Level, selection, @arg2*100))),
    send(D, append,
         button(stop, and(message(R, stop),
                          message(R?sound, save, 'recording.wav'),
                          message(D, destroy)))),
    send(D, open),
    send(R, record).
\end{code}

The system may ask the user for permission to use the microphone.
`sound <-peaks' returns the peak level of a given number of parts of
the sound, which is useful to draw its waveform.  The demo
\textsf{Sound} records, plays, opens and streams sound.


\section{Video from a camera}			\label{sec:camera}

\index{camera}\index{webcam}\index{video}%
A \class{camera} delivers the video of a camera, e.g., a webcam, as an
\class{image}.  `display <-cameras' lists the names of the available
cameras.  `camera ->open' starts the camera.  Without arguments, the
camera chooses the size of the frames, often its largest.  `camera
<-sizes' lists the supported sizes and `camera ->open' accepts the
desired size.  Each new frame is copied
into the same `camera <-image', so a \class{bitmap} that displays this
image shows the video, and `camera <->message' is executed for each
frame, e.g., to analyse it:

\begin{code}
camera_example :-
    new(C, camera),
    new(P, picture('Camera')),
    send(P, display, bitmap(C?image)),
    send(P, open),
    send(C, open, size(640, 480)),
    send(P, attribute, camera, C).          % freed with the picture
\end{code}

The user may have to give the application permission to use the
camera.  Until this is decided, `camera <-status' is \const{waiting};
it is \const{open} when the camera delivers frames and \const{denied}
if the user refused.  `camera <->status_message' is executed when the
status changes.  The demo \textsf{Camera} of the demo browser
(\exam{?- manpce.}, menu \textsf{File/Demo programs}) shows the video,
takes snapshots and saves them or copies them to the clipboard.

\subsection{Showing video without Cairo}	\label{sec:sdl-window}

\index{sdl_texture}\index{sdl_window}\index{GPU}%
Copying each frame into an \class{image} converts it to RGB and has
Cairo draw it, which is expensive for large frames.  If `camera
<->texture' is an \class{sdl_texture}, the frames go there instead,
keeping the format of the camera.  An \class{sdl_layer} shows the
texture in an \class{sdl_window}.  The SDL renderer of the frame then
converts and scales the video, on the GPU if it can.  The layers are
drawn below the graphicals of the window, so these can annotate the
video without being redrawn for each frame:

\begin{code}
sdl_camera_example :-
    new(C, camera),
    send(C, texture, new(T, sdl_texture)),
    new(W, sdl_window('Camera', size(640, 480))),
    send(W, background, black),
    send(W, append, new(L, sdl_layer(T))),  % fit the window
    send(L, flip, horizontal),              % as a mirror
    send(W, display, text('Live'), point(10, 10)),
    send(W, open),
    send(C, open, size(640, 480)),
    send(W, attribute, camera, C).
\end{code}

An \class{sdl_layer} places its texture in an area of the window and
can scale, rotate, flip and fade it.  `sdl_texture <-image' converts
the pixels to a new \class{image}, e.g., to save a snapshot, and
`image ->copy' copies them into an existing image, e.g., to analyse
each frame.  An \class{sdl_texture} can also be made from an image,
which allows for fast moving sprites, and `sdl_texture ->draw_in'
paints a graphical on its pixels, as `image ->draw_in' does.

\index{sprite}%
A layer becomes a \jargon{sprite} using scale \const{none} and a
\class{transform}: `sdl_layer <->transform' moves and rotates the
texture around its `sdl_texture <->hot_spot', as `figure ->transform'
does for a figure.  Creating an \class{sdl_texture} from a graphical
draws it once with Cairo and makes the origin of the graphical the hot
spot.  Moving many such sprites costs the renderer little, while
moving as many figures has Cairo draw each of them for every move.
Layers are drawn in the order of `sdl_window <-layers'; `sdl_layer
->expose' and `sdl_layer ->hide' move a layer to the top or the bottom,
as for graphicals.  The demo \textsf{Asteroids} draws its ship, rocks,
bullets and stars as sprites.

A layer can also combine its texture with what is below it in other
ways than alpha blending: `sdl_layer <->blend' \const{add} makes
sparks and light glow, \const{mod} and \const{mul} darken.
`sdl_layer <->tint' shows one texture in many colours and `sdl_layer
<->scale_mode' \const{nearest} or \const{pixelart} keeps pixel art
sharp.  Scale \const{tile} repeats the texture over the area, e.g., for
a background that scrolls by changing `sdl_layer <->offset', and
`sdl_layer <->slices' scales a texture as nine parts that keep the
size of its corners, as is common for the frames of buttons and panels.

\subsection{Playing and recording video}		\label{sec:video}

\index{video}\index{MP4}%
A \class{video} plays a video file or stream, e.g., an MP4, MKV or WebM
file, with its sound.  Its frames go into `video <-texture', an
\class{sdl_texture}, which is shown using an \class{sdl_layer} as
above.  `video ->play', `video ->pause' and `video ->stop' control it
and `video <->position' tells and changes the time.  The sound plays on
the default playback device and keeps the frames in time.  The example
plays a file in a window and plays or pauses when the window is
clicked:

\begin{code}
video_example(File) :-
    new(V, video(File)),
    new(W, sdl_window('Video', size(640, 360))),
    send(W, background, black),
    send(W, append, sdl_layer(V?texture)),  % fit the window
    send(W, recogniser,                     % click to play or pause
         click_gesture(left, '', single,
                       if(V?status == playing,
                          message(V, pause),
                          message(V, play)))),
    send(W, open),
    send(W, attribute, video, V),           % freed with the window
    send(V, play).
\end{code}

`video <-image' copies the current frame or, given a time, the frame at
that time, e.g., to make a thumbnail.  A \class{video_writer} writes a
video file from images, textures, frames and windows, by default as
H.264 in MP4.  `video_writer ->append' adds a frame and `video_writer
->append_audio' adds samples to its sound.  It can also record: `video_writer
->record' appends the image of a window or frame at its frame rate,
`camera <->writer' makes a camera append its frames and `audio_recorder
->record' with a \class{video_writer} adds the sound of the microphone.
The example starts recording a window.  `video_writer ->close' stops
recording and finishes the file:

\begin{code}
record_window_example(Window, File, Writer) :-
    get(Window, size, Size),
    new(Writer, video_writer(File, Size, 25)),
    send(Writer, record, Window).
\end{code}

Videos are decoded and encoded using ffmpeg on Linux, AVFoundation on
macOS and Media Foundation on Windows.  \product{} built with
\exam{-DXPCE_VIDEO=ffmpeg} uses ffmpeg on all platforms; it then
encodes using the encoders of the operating system.  `display
<-video_backend' tells which is used, and `display <-video_decoders'
and `display <-video_encoders' list the codecs that are available.  The demo \textsf{Video player} plays files and network
streams, and the demo \textsf{Camera} records the camera and the
microphone or streams them to a URL.

\subsection{Streaming video}			\label{sec:video-stream}

\index{streaming}\index{live video}%
The source of a \class{video} may be a URL, e.g., \exam{https://...},
an HLS playlist (\exam{.m3u8}), an IP camera (\exam{rtsp://...}) or
\exam{udp://...}.  A network stream is opened in the background:
`video <-status' is \const{opening} until it is open, which `video
<->status_message' tells.  A live stream (`video <-live') cannot seek;
if playing falls behind, frames are dropped so the delay stays small.
`video ->option' passes options to the backend, e.g., \exam{rtsp_transport
tcp} for an IP camera.

A \class{video} without a source plays the encoded data written to it,
e.g., using pce_open/3.  This plays video that arrives on a Prolog
stream, e.g., from an HTTP request or a WebSocket.  Formats such as
MPEG-TS, WebM and fragmented MP4 play while the data arrives.

The target of a \class{video_writer} may also be a URL, which streams
the video live: \exam{rtmp://...} for streaming services, and
\exam{srt://}, \exam{udp://}, \exam{tcp://} or \exam{rtp://} for
MPEG-TS.  A target ending in \exam{.m3u8} writes HLS segments that a
web server can serve to browsers.  Finally, the target may be code that
receives the encoded data.  library(pce_video) uses this to write to a
Prolog stream.  The example serves a window live over HTTP using the
HTTP server of SWI-Prolog.  The writer gives the data to a message
queue in the GUI thread; the thread of the HTTP request copies it to
the client.  \exam{video('http://localhost:8080/live.ts')}, VLC or ffplay
show it.

\begin{code}
stream_window(Window, Port) :-
    http_handler(root('live.ts'), send_window(Window), [spawn([])]),
    http_server(http_dispatch, [port(Port)]).

send_window(Window, _Request) :-
    message_queue_create(Queue),
    format('Content-type: video/mp2t~n'),
    format('Transfer-encoding: chunked~n~n'),
    in_pce_thread_sync(start_stream(Window, Queue, Writer)),
    call_cleanup(relay(Queue),
                 in_pce_thread(stop_stream(Writer, Queue))).

start_stream(Window, Queue, Writer) :-
    get(Window, size, Size),
    new(Writer, video_writer(message(@prolog, queue_data,
                                     prolog(Queue), @arg1),
                             Size, 25)),
    send(Writer, format, mpegts),
    send(Writer, live, @on),
    send(Writer, record, Window).

queue_data(Queue, Data) :-
    get(Data, value, String),
    thread_send_message(Queue, String).

relay(Queue) :-
    thread_get_message(Queue, String),
    write(String),
    flush_output,
    relay(Queue).

stop_stream(Writer, Queue) :-
    send(Writer, close),
    free(Writer),
    message_queue_destroy(Queue).
\end{code}

Streaming needs the ffmpeg backend.  Media Foundation plays files and
progressive downloads over http(s) and AVFoundation plays files, but
neither reads written data or writes to anything but a file.  See
\exam{-DXPCE_VIDEO=ffmpeg} above to stream on Windows and macOS.


\section{The desktop}				\label{sec:desktop}

This section describes how \product{} cooperates with the desktop and
other applications.  Drag-and-drop of files and text from other
applications is described in \secref{dragdrop}.

\subsection{Images on the clipboard}		\label{sec:clipboard}

\index{clipboard}\index{copy}\index{paste}%
`display ->copy' puts text on the clipboard.  It also accepts an
\class{image}, which other applications can paste as a PNG image, or
a \class{graphical}, which is offered as SVG and as PNG, so
applications can paste the format they prefer.  Using `graphical
->svg' as described in \secref{savedrawing}, the SVG holds vectors:

\begin{code}
?- send(@display, copy, Figure).
\end{code}

`display <-selection' with the target \const{image} returns an image
from a PNG, JPEG or other image format on the clipboard and with the
target \const{svg} the SVG text.  `display <-selection_targets'
returns the MIME types of the data on the clipboard, which allows a
paste command to decide what to paste:

\begin{code}
paste(P) :-
    get(@display, selection_targets, Targets),
    (   send(Targets, member, 'image/png'),
        get(@display, selection, clipboard, image, Image)
    ->  send(P, display, bitmap(Image))
    ;   get(@display, paste, Text)
    ->  send(P, display, text(Text))
    ).
\end{code}

\subsection{The system tray}			\label{sec:tray}

\index{tray}\index{notification area}%
A \class{tray_icon} shows an icon in the system tray, also known as the
notification area, e.g., for an application that runs in the
background.  The icon can have a tooltip and a menu, which is built
from a \class{popup}.  Selecting an entry of the menu executes the
popup as if the item was selected in the popup, with the
\class{tray_icon} as context.  Sub-popups become submenus.  After
changing the popup, use `tray_icon ->update' to rebuild the menu.
Freeing the tray_icon removes the icon.

\begin{code}
tray_example(Frame) :-
    new(P, popup(tray)),
    send_list(P, append,
              [ menu_item(show, message(Frame, expose)),
                menu_item(quit, message(Frame, destroy))
              ]),
    new(I, image(@nil, 16, 16, pixmap)),
    send(I, fill, colour(steel_blue)),
    new(T, tray_icon(I, 'My application', P)),
    send(Frame, attribute, tray, T).
\end{code}

On Linux, the system tray requires the AppIndicator library
(libayatana-appindicator) and a desktop that shows such icons.

\subsection{Power and language}

`display <-power_state' tells whether the system runs on a battery and
whether it is charging.  `display <-battery_percent' and `display
<-battery_seconds' tell how much charge is left.  An application may
use this, e.g., to do less work while running on a battery.  `display
<-preferred_locales' returns the languages the user prefers, most
preferred first, as names such as \const{en_US}.


\section{Testing input without the devices}	\label{sec:testdevices}

\index{testing,input devices}%
Tests should not depend on the devices of the machine that runs them.
\product{} provides methods for testing that make it process input as
if a device generated it:

\begin{itemize}
    \tick{`frame ->sdl_test_event'}
Builds an SDL event of a given type from a \class{sheet} with the
fields of the event and processes it as if SDL reported it.  This
allows testing mouse, wheel, pen, touch, pinch and keyboard input,
e.g.,
\begin{code}
new(S, sheet), send(S, value, x, 50), send(S, value, y, 50),
send(S, value, which, 7),               % pen 7
send(Frame, sdl_test_event, pen_motion, S)
\end{code}
    \tick{`display ->sdl_test_gamepad'}
Attaches a virtual gamepad and sets the state of its buttons and axes.
SDL generates the same events for it as for a real gamepad.
`display <-sdl_test_gamepad' returns the rumble and LED colour sent to
it.
    \tick{`camera ->sdl_test_frame'}
Delivers a frame of a given size and colour, as a camera does.
    \tick{`frame <-sdl_test_image'}
Reads back what the SDL renderer draws for the frame, including the
layers of an \class{sdl_window}.  `frame <-image' composites the
windows using Cairo instead.
    \tick{SDL drivers}
SDL selects its drivers from environment variables, which must be set
before \pllib{pce} is loaded.  The \const{disk} audio driver
(\verb$SDL_AUDIO_DRIVER=disk$) plays to and records from files and the
\const{dummy} camera driver (\verb$SDL_CAMERA_DRIVER=dummy$) has no
cameras.  See also \chapref{sdl-driver}.
\end{itemize}

The tests of \product{} in the directory \file{tests} of the package
use these methods and are examples of their use.
