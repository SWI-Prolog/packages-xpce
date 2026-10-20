/*  Examples of the chapter "Input devices and media" of the XPCE User
    Guide.

        ?- new(P, sketch_pad), send(P, open).
        ?- gamepad_example.
        ?- record_example.
        ?- camera_example.
        ?- sdl_camera_example.
        ?- video_example('movie.mp4').
        ?- record_window_example(@my_window, 'window.mp4', W).
        ?- send(W, close).
        ?- stream_window(@my_window, 8080).
        ?- new(F, frame('Tray example')), send(F, open), tray_example(F).
*/

:- module(devices_examples,
          [ stroke_width/2,
            gamepad_example/0,
            move_box/2,
            record_example/0,
            camera_example/0,
            sdl_camera_example/0,
            video_example/1,
            record_window_example/3,
            stream_window/2,
            paste/1,
            tray_example/1
          ]).
:- use_module(library(pce)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

		 /*******************************
		 *	       PENS		*
		 *******************************/

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

:- pce_end_class(sketch_pad).

%!  stroke_width(+Event, -Width) is det.
%
%   The width of the line follows the pressure of a pen.  The mouse,
%   which has no pressure, draws with a width of 2.

stroke_width(Ev, W) :-
    get(Ev, device, Dev),
    get(Dev, kind, pen),
    !,
    get(Ev, pressure, Pressure),
    W is max(1, round(Pressure*8)).
stroke_width(_, 2).


		 /*******************************
		 *	      GAMEPADS		*
		 *******************************/

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


		 /*******************************
		 *	       SOUND		*
		 *******************************/

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


		 /*******************************
		 *	       CAMERA		*
		 *******************************/

camera_example :-
    new(C, camera),
    new(P, picture('Camera')),
    send(P, display, bitmap(C?image)),
    send(P, open),
    send(C, open, size(640, 480)),
    send(P, attribute, camera, C).          % freed with the picture

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


		 /*******************************
		 *	       VIDEO		*
		 *******************************/

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

record_window_example(Window, File, Writer) :-
    get(Window, size, Size),
    new(Writer, video_writer(File, Size, 25)),
    send(Writer, record, Window).

%   Serve Window live as MPEG-TS at http://localhost:Port/live.ts.  The
%   writer gives the data to a message queue from the GUI thread; the
%   thread of the HTTP request copies it to the client until the client
%   disconnects.

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


		 /*******************************
		 *	     CLIPBOARD		*
		 *******************************/

%!  paste(+Picture) is semidet.
%
%   Paste an image or text from the clipboard into Picture.

paste(P) :-
    get(@display, selection_targets, Targets),
    (   send(Targets, member, 'image/png'),
        get(@display, selection, clipboard, image, Image)
    ->  send(P, display, bitmap(Image))
    ;   get(@display, paste, Text)
    ->  send(P, display, text(Text))
    ).


		 /*******************************
		 *	       TRAY		*
		 *******************************/

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
