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
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/** <module> Record, replay and load sound

Opens a window to record sound from the microphone and play it back.
While recording, a meter shows the input level and the waveform grows.
Load reads a sound file: WAV and, if xpce is built with SDL3_sound,
also MP3, Ogg Vorbis, FLAC, etc. (see `display<-sound_file_extensions`).
Save writes the sound as a WAV file.

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

fill_dialog(F, D:dialog) :->
    "Create the controls"::
    send(D, append, button(record, message(F, record))),
    send(D, append, button(stop, message(F, stop)), right),
    send(D, append, button(play, message(F, play)), right),
    send(D, append, new(V, slider(volume, 0, 100, 80)), right),
    send(V, width, 100),
    send(D, append, button(load, message(F, load)), right),
    send(D, append, button(save, message(F, save)), right),
    send(D, append, button(quit, message(F, destroy)), right),
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
    "Stop recording or playing"::
    get(F, recorder, R),
    (   R \== @nil
    ->  send(R, stop),
        send(F?timer, stop),
        send(F, slot, recorder, @nil),
        send(F, level, 0),
        send(F, draw_waveform)
    ;   true
    ),
    get(F, sound, S),
    (   S \== @nil
    ->  send(S, stop)
    ;   true
    ),
    send(F, update).

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
    ).

update(F) :->
    "Update the buttons and the status"::
    get(F, member, controls, D),
    get(F, recorder, R),
    get(F, sound, S),
    (   R \== @nil -> Recording = true ; Recording = false ),
    (   S \== @nil, get(S, duration, Dur), Dur > 0
    ->  HasSound = true
    ;   HasSound = false
    ),
    (   S \== @nil, get(S, playing, @on) -> Playing = true ; Playing = false ),
    active(D, record, \+ Recording),
    active(D, stop, (Recording ; Playing)),
    active(D, play, (HasSound, \+ Recording)),
    active(D, save, (HasSound, \+ Recording)),
    get(D, status_member, L),
    (   Recording
    ->  get(S, duration, T),
        send(L, format, 'Recording %.1f s', T)
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

:- pce_end_class(sound_demo).

:- meta_predicate active(+, +, 0).

active(D, Button, Cond) :-
    get(D, member, Button, B),
    (   call(Cond) -> send(B, active, @on) ; send(B, active, @off) ).
