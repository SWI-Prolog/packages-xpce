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

:- module(sound_demo,
          [ sound_demo/0
          ]).
:- use_module(library(pce)).
:- use_module(library(help_message)).
:- use_module(demo_buttons).
:- autoload(library(http/http_open), [http_open/3]).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/** <module> Record, replay and load sound

Opens a window to record sound from the microphone and play it back.
While recording, a meter shows the input level and the waveform grows.
The menu of Record records into a WAV file rather than into memory.
Open reads a sound file: WAV and, if xpce is built with SDL3_sound,
also MP3, Ogg Vorbis, FLAC, etc. (see `display<-sound_file_extensions`).
Save writes the sound as a WAV file.

The menu of Open streams a file or URL: it plays while it is read, using
an audio_stream.  A thread copies the data to the stream, which decodes
and plays it.  This works for WAV, MP3 and FLAC, but not for Ogg Vorbis.
Record and Play become Stop while recording and while playing or
streaming.  All controls have a tooltip.

The waveform is drawn from `sound<-peaks`, the peak level of each of a
number of parts of the sound.

Run with:

    ?- sound_demo.
*/

sound_demo :-
    new(F, sound_demo),
    send(F, open).

:- pce_begin_class(sound_demo, frame,
                   "Record, replay and load sound").

variable(sound,    sound*,          get, "The current sound").
variable(recorder, audio_recorder*, get, "Recorder while recording").
variable(stream,   audio_stream*,   get, "Stream while streaming").
variable(location, name*,           get, "File or URL being streamed").
variable(timer,    timer,           get, "Redraws the waveform while recording").

wave_width(600).
wave_height(160).

initialise(F) :->
    send_super(F, initialise, 'Sound'),
    wave_width(W), wave_height(H),
    send(F, append, new(Wave, picture('Waveform', size(W, H)))),
    send(Wave, name, waveform),
    send(Wave, scrollbars, none),
    send(Wave, background, colour(white)),
    send(Wave, help_message, tag, 'Waveform of the sound'),
    send(new(Meter, picture('Level', size(W, 14))), below, Wave),
    send(Meter, name, meter),
    send(Meter, scrollbars, none),
    send(Meter, display, new(Bar, box(0, 14))),
    send(Bar, pen, 0),
    send(new(D, dialog), below, Meter),
    send(D, name, controls),
    send(F, fill_dialog, D),
    send(F, slot, timer, timer(0.25, message(F, draw_waveform))),
    send(F, update).

%   Record and Open are split buttons: the button does the common
%   action, the menu of its arrow the others.  Record and Play turn into
%   Stop while they are busy.

fill_dialog(F, D:dialog) :->
    "Create the controls"::
    send(D, append, new(Rec, state_button(record, message(F, record_button)))),
    send(Rec, popup, new(RP, popup)),
    send_list(RP, append,
              [ menu_item(memory, message(F, record), 'Record',
                          condition := message(F, idle)),
                menu_item(file, message(F, record_to_file),
                          'Record to a WAV file ...',
                          condition := message(F, idle))
              ]),
    send(D, append, state_button(play, message(F, play_button)), right),
    send(D, append, new(V, slider(volume, 0, 100, 80,
                                  message(F, volume, @arg1))), right),
    send(V, width, 100),
    send(V, show_value, @off),
    send(V, help_message, tag, 'Volume of playing and streaming'),
    send(D, append, new(Open, button(open, message(F, load))), right),
    send(Open, help_message, tag,
         'Open a sound file.\nThe menu also streams a file or a URL'),
    send(Open, popup, new(OP, popup)),
    send_list(OP, append,
              [ menu_item(file, message(F, load), 'Open a file ...'),
                menu_item(stream_file, message(F, stream_file),
                          'Stream a file ...'),
                menu_item(stream_url, message(F, stream_url),
                          'Stream a URL ...')
              ]),
    send(D, append, new(Save, button(save, message(F, save))), right),
    send(Save, label, 'Save ...'),
    send(Save, help_message, tag, 'Save the sound as a WAV file'),
    send(D, append, new(Q, button(quit, message(F, destroy))), right),
    send(Q, help_message, tag, 'Close this window'),
    send(D, append, new(Status, label(status, ''))),
    send(Status, alignment, left).

unlink(F) :->
    "Stop recording, playing and the timer"::
    send(F?timer, stop),
    send(F, stop),
    send_super(F, unlink).

		 /*******************************
		 *	       ACTIONS		*
		 *******************************/

record(F) :->
    "Start recording"::
    send(F, stop),
    new(R, audio_recorder(message(F, level, @arg2))),
    send(F, slot, recorder, R),
    get(R, sound, S),
    send(F, slot, sound, S),
    send(R, record),
    send(F?timer, start),
    send(F, update).

stop(F) :->
    "Stop recording, playing or streaming"::
    get(F, recorder, R),
    (   R \== @nil
    ->  send(R, stop),
        send(F?timer, stop),
        send(F, slot, recorder, @nil),
        send(F, level, 0),
        get(R, sink, Sink),
        (   Sink \== @nil              % recorded to a file: show it
        ->  send(F, load_file, Sink)
        ;   send(F, draw_waveform)
        )
    ;   true
    ),
    get(F, stream, AS),
    (   AS \== @nil
    ->  send(AS, stop),             % ends the thread that writes
        send(F, slot, stream, @nil),
        send(F, slot, location, @nil)
    ;   true
    ),
    get(F, sound, S),
    (   S \== @nil
    ->  send(S, stop)
    ;   true
    ),
    send(F, update).

record_button(F) :->
    "Record into memory or stop recording"::
    (   get(F, recorder, @nil)
    ->  send(F, record)
    ;   send(F, stop)
    ).

play_button(F) :->
    "Play the sound or stop playing or streaming"::
    (   send(F, busy_playing)
    ->  send(F, stop)
    ;   send(F, play)
    ).

idle(F) :->
    "True if we are not recording (condition of the record menu)"::
    get(F, recorder, @nil).

busy_playing(F) :->
    "True while playing or streaming"::
    (   \+ get(F, stream, @nil)
    ->  true
    ;   get(F, sound, S),
        S \== @nil,
        get(S, playing, @on)
    ).

volume(F, Volume:int) :->
    "Change the volume of what is streaming"::
    get(F, stream, AS),
    (   AS == @nil
    ->  true
    ;   send(AS, volume, Volume/100)
    ).

play(F) :->
    "Play the current sound"::
    get(F, sound, S),
    S \== @nil,
    get(F, member, controls, D),
    get(D?volume_member, selection, Volume),
    V is Volume/100,
    send(S, message, message(F, update)),
    send(S, play, V),
    send(F, update).

record_to_file(F) :->
    "Record into a WAV file"::
    get(@finder, file, save, wav, File),
    send(F, stop),
    new(R, audio_recorder(message(F, level, @arg2))),
    send(F, slot, recorder, R),
    send(F, slot, sound, @nil),
    send(R, record, file(File)),
    send(F, draw_waveform),
    send(F, update).

stream_file(F) :->
    "Ask for a sound file and play it while reading it"::
    get(@display, sound_file_extensions, Exts),
    get(@finder, file, open, tuple('Sound files', Exts), File),
    send(F, stream, File).

stream_url(F) :->
    "Ask for a URL and play it while reading it"::
    new(D, dialog('Stream a URL')),
    send(D, append, new(TI, text_item(url, ''))),
    send(TI, width, 50),
    send(D, append, button(stream, message(D, return, TI?selection))),
    send(D, append, button(cancel, message(D, return, @nil))),
    send(D, default_button, stream),
    send(D, transient_for, F),
    get(D, confirm_centered, F?area?center, URL),
    send(D, destroy),
    URL \== @nil,
    get(URL, strip, Stripped),
    \+ get(Stripped, size, 0),
    send(F, stream, Stripped).

stream(F, Location:name) :->
    "Play Location, a URL or file, while reading it"::
    send(F, stop),
    get(F, member, controls, D),
    get(D?volume_member, selection, Volume),
    new(AS, audio_stream),
    send(AS, volume, Volume/100),
    send(AS, message, message(F, stream_done)),
    send(F, slot, stream, AS),
    send(F, slot, location, Location),
    send(AS, play),
    thread_create(stream_source(AS, Location), _, [detached(true)]),
    send(F, update).

stream_done(F) :->
    "The stream played all data"::
    send(F, slot, stream, @nil),
    send(F, slot, location, @nil),
    send(F, update).

load(F) :->
    "Load a sound file"::
    get(@display, sound_file_extensions, Exts),
    get(@finder, file, open, tuple('Sound files', Exts), File),
    send(F, load_file, File).

load_file(F, File:file) :->
    "Load the sound from File"::
    send(F, stop),
    new(S, sound(File)),
    send(F, slot, sound, S),
    send(F, draw_waveform),
    send(F, update).

save(F) :->
    "Save the sound as a WAV file"::
    get(@finder, file, save, wav, File),
    send(F, save_file, File).

save_file(F, File:file) :->
    "Save the sound to File"::
    get(F, sound, S),
    S \== @nil,
    send(S, save, File),
    send(F, report, status, 'Saved to %s', File?name).

		 /*******************************
		 *	       DISPLAY		*
		 *******************************/

level(F, Level:num) :->
    "Show the input level (0..1) on the meter"::
    get(F, member, meter, M),
    get(M?graphicals, head, Bar),
    wave_width(W),
    BW is round(Level*W),
    (   Level > 0.8
    ->  Colour = red
    ;   Colour = forest_green
    ),
    send(Bar, size, size(BW, 14)),
    send(Bar, fill, colour(Colour)),
    send(F, update).

draw_waveform(F) :->
    "Draw the waveform of the sound"::
    get(F, member, waveform, P),
    send(P, clear),
    get(F, sound, S),
    wave_width(W), wave_height(H),
    Mid is H//2,
    send(P, display, line(0, Mid, W, Mid)),
    (   S \== @nil,
        get(S, duration, D),
        D > 0
    ->  Step = 3,
        N is W//Step,
        get(S, peaks, N, Peaks),
        forall(between(1, N, I),
               ( get(Peaks, element, I, Peak),
                 Y is max(1, round(Peak*(Mid-4))),
                 X is (I-1)*Step,
                 send(P, display,
                      new(L, line(X, Mid-Y, X, Mid+Y))),
                 send(L, colour, colour(steel_blue)),
                 send(L, pen, 2)
               ))
    ;   true
    ),
    send(F, update).                    % restores the REC/PLAY indicator

update(F) :->
    "Update the buttons and the status"::
    get(F, member, controls, D),
    get(F, recorder, R),
    get(F, sound, S),
    get(F, stream, AS),
    (   R \== @nil -> Recording = true ; Recording = false ),
    (   AS \== @nil -> Streaming = true ; Streaming = false ),
    (   S \== @nil, get(S, duration, Dur), Dur > 0
    ->  HasSound = true
    ;   HasSound = false
    ),
    (   S \== @nil, get(S, playing, @on) -> Playing = true ; Playing = false ),
    get(D, record_member, Rec),
    (   Recording
    ->  send(Rec, label, '\u25A0 Stop'),
        send(Rec, help_message, tag, 'Stop recording')
    ;   send(Rec, label, '\u25CF Record'),
        send(Rec, help_message, tag,
             'Record from the microphone.\n\c
              The menu records into a WAV file')
    ),
    active(D, record, \+ (Playing ; Streaming)),
    get(D, play_member, Play),
    (   (Playing ; Streaming)
    ->  send(Play, label, '\u25A0 Stop'),
        send(Play, help_message, tag, 'Stop playing')
    ;   send(Play, label, '\u25B6 Play'),
        send(Play, help_message, tag, 'Play the sound')
    ),
    active(D, play, (Playing ; Streaming ; HasSound, \+ Recording)),
    active(D, open, \+ Recording),
    active(D, save, (HasSound, \+ Recording)),
    indicator(F, Recording, Playing, Streaming),
    get(D, status_member, L),
    (   Recording, get(R, sink, Sink), Sink \== @nil
    ->  get(Sink, name, Name),
        send(L, format, 'Recording to %s', Name)
    ;   Recording
    ->  get(S, duration, T),
        send(L, format, 'Recording %.1f s', T)
    ;   Streaming
    ->  get(F, location, Loc),
        send(L, format, 'Streaming %s', Loc)
    ;   Playing
    ->  send(L, format, 'Playing %.1f s', Dur)
    ;   HasSound
    ->  get(S, frequency, Hz),
        get(S, channels, Ch),
        (   Ch == 1 -> Chans = mono ; Ch == 2 -> Chans = stereo
        ;   format(atom(Chans), '~w channels', [Ch])
        ),
        send(L, format, '%.1f s, %d Hz, %s', Dur, Hz, Chans)
    ;   send(L, selection, 'No sound')
    ).

%   REC, PLAY or STREAM in the top-right of the waveform

indicator(F, Recording, Playing, Streaming) :-
    get(F, member, waveform, P),
    (   get(P, member, indicator, T)
    ->  true
    ;   send(P, display, new(T, text('', right, bold))),
        send(T, name, indicator)
    ),
    (   Recording
    ->  send(T, colour, red),
        send(T, string, '\u25CF REC')
    ;   Streaming
    ->  send(T, colour, forest_green),
        send(T, string, '\u25B6 STREAM')
    ;   Playing
    ->  send(T, colour, forest_green),
        send(T, string, '\u25B6 PLAY')
    ;   send(T, string, '')
    ),
    wave_width(W),
    send(T, position, point(W-T?width-8, 6)).

:- pce_end_class(sound_demo).

%   stream_source(+AudioStream, +Location)
%
%   Copy the data of Location, a URL or file, to AudioStream.  Runs in
%   a thread of its own: the writes wait while the stream is far ahead.
%   ->stop makes the writes fail, which ends the thread.

stream_source(AS, Location) :-
    catch(setup_call_cleanup(
              open_source(Location, In),
              setup_call_cleanup(
                  pce_open(AS, write, Out),
                  copy_stream_data(In, Out),
                  close(Out)),
              close(In)),
          E,
          stream_error(AS, E)).

open_source(Location, In) :-
    (   sub_atom(Location, 0, _, _, 'http://')
    ;   sub_atom(Location, 0, _, _, 'https://')
    ),
    !,
    http_open(Location, In, []),
    set_stream(In, type(binary)).
open_source(File, In) :-
    open(File, read, In, [type(binary)]).

%   An I/O error on the audio stream means it was stopped.  Other errors
%   are reported in the window.

stream_error(AS, E) :-
    (   E = error(io_error(write, _), _)
    ->  true
    ;   message_to_string(E, Msg),
        in_pce_thread(send(AS, report, error, '%s', Msg))
    ).

:- meta_predicate active(+, +, 0).

active(D, Button, Cond) :-
    get(D, member, Button, B),
    (   call(Cond) -> send(B, active, @on) ; send(B, active, @off) ).
