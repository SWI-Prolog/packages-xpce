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

#include <h/kernel.h>
#include <h/graphics.h>

		 /*******************************
		 *	       SOUND		*
		 *******************************/

/* A sound holds audio, loaded from a WAV file or recorded using an
 * audio_recorder.  ->play plays it on the default output device.  A
 * sound may be played several times at once.
 */

static status
initialiseSound(Sound s, FileObj file)
{ assign(s, file,      NIL);
  assign(s, frequency, ZERO);
  assign(s, channels,  ZERO);
  assign(s, duration,  ZERO);
  assign(s, message,   NIL);
  s->ws_ref = NULL;

  if ( notDefault(file) )
    return send(s, NAME_load, file, EAV);

  succeed;
}


static status
unlinkSound(Sound s)
{ ws_free_sound(s);

  succeed;
}


static status
loadSound(Sound s, FileObj file)
{ TRY(ws_load_sound(s, file));
  assign(s, file, file);

  succeed;
}


static status
saveSound(Sound s, FileObj file)
{ return ws_save_sound(s, file);
}


static status
playSound(Sound s, Int volume)
{ return ws_play_sound(s, isDefault(volume) ? 1.0 : valNum(volume));
}


static status
stopSound(Sound s)
{ return ws_stop_sound(s);
}


static BoolObj
getPlayingSound(Sound s)
{ answer(ws_playing_sound(s) ? ON : OFF);
}


static Vector
getPeaksSound(Sound s, Int parts)
{ int n = valInt(parts);
  double *peaks = alloca(n*sizeof(double));
  Any *argv = alloca(n*sizeof(Any));

  if ( !ws_sound_peaks(s, n, peaks) )
    fail;
  for(int i=0; i<n; i++)
    argv[i] = toNum(peaks[i]);

  answer(answerObjectv(ClassVector, n, argv));
}


/* Type declarations */

/* Instance Variables */

static vardecl var_sound[] =
{ IV(NAME_file, "file*", IV_GET,
     NAME_storage, "File the sound was loaded from"),
  IV(NAME_frequency, "int", IV_GET,
     NAME_format, "Samples per second"),
  IV(NAME_channels, "int", IV_GET,
     NAME_format, "Number of channels: 1 is mono, 2 stereo"),
  IV(NAME_duration, "num", IV_GET,
     NAME_format, "Length in seconds"),
  IV(NAME_message, "code*", IV_BOTH,
     NAME_action, "Executed with the sound when a play finished"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "Window system data")
};

/* Send Methods */

static senddecl send_sound[] =
{ SM(NAME_initialise, 1, "file=[file]", initialiseSound,
     DEFAULT, "Create, optionally loading a WAV file"),
  SM(NAME_unlink, 0, NULL, unlinkSound,
     DEFAULT, "Stop playing and free the audio"),
  SM(NAME_load, 1, "file", loadSound,
     NAME_storage, "Load audio from a WAV file"),
  SM(NAME_save, 1, "file", saveSound,
     NAME_storage, "Save the audio as a WAV file"),
  SM(NAME_play, 1, "volume=[0.0..1.0]", playSound,
     NAME_action, "Play on the default output device"),
  SM(NAME_stop, 0, NULL, stopSound,
     NAME_action, "Stop all plays of this sound")
};

/* Get Methods */

static getdecl get_sound[] =
{ GM(NAME_playing, 0, "bool", NULL, getPlayingSound,
     NAME_action, "@on if the sound is being played"),
  GM(NAME_peaks, 1, "vector", "parts=1..10000", getPeaksSound,
     NAME_analyse, "Peak level (0..1) of each of `parts` parts")
};

/* Class Declaration */

static Name sound_termnames[] = { NAME_file };

ClassDecl(sound_decls,
          var_sound, send_sound, get_sound, NULL,
          1, sound_termnames);

status
makeClassSound(Class class)
{ declareClass(class, &sound_decls);

  succeed;
}


		 /*******************************
		 *	   AUDIO RECORDER	*
		 *******************************/

/* An audio_recorder records from the default input device, normally
 * the microphone, into its <-sound.  While recording, <-level_message
 * is executed regularly with the recorder and the input level (0..1),
 * e.g., to show a level meter.
 */

static status
initialiseAudioRecorder(AudioRecorder r, Code msg)
{ assign(r, sound,	   newObject(ClassSound, EAV));
  assign(r, level_message, isDefault(msg) ? NIL : msg);
  assign(r, recording,	   OFF);
  r->ws_ref = NULL;

  succeed;
}


static status
unlinkAudioRecorder(AudioRecorder r)
{ ws_stop_recording(r);

  succeed;
}


static status
recordAudioRecorder(AudioRecorder r)
{ if ( r->recording == ON )
    succeed;
  TRY(ws_start_recording(r));
  assign(r, recording, ON);

  succeed;
}


static status
stopAudioRecorder(AudioRecorder r)
{ if ( r->recording == OFF )
    succeed;
  ws_stop_recording(r);
  assign(r, recording, OFF);

  succeed;
}


/* Instance Variables */

static vardecl var_audio_recorder[] =
{ IV(NAME_sound, "sound", IV_GET,
     NAME_storage, "The recorded sound"),
  IV(NAME_levelMessage, "code*", IV_BOTH,
     NAME_action, "Executed with the recorder and the input level"),
  IV(NAME_recording, "bool", IV_GET,
     NAME_action, "@on while recording"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "Window system data")
};

/* Send Methods */

static senddecl send_audio_recorder[] =
{ SM(NAME_initialise, 1, "level_message=[code]*", initialiseAudioRecorder,
     DEFAULT, "Create, optionally with a level message"),
  SM(NAME_unlink, 0, NULL, unlinkAudioRecorder,
     DEFAULT, "Stop recording"),
  SM(NAME_record, 0, NULL, recordAudioRecorder,
     NAME_action, "Start recording, replacing the audio of <-sound"),
  SM(NAME_stop, 0, NULL, stopAudioRecorder,
     NAME_action, "Stop recording")
};

/* Class Declaration */

static Name audio_recorder_termnames[] = { NAME_levelMessage };

ClassDecl(audio_recorder_decls,
          var_audio_recorder, send_audio_recorder, NULL, NULL,
          1, audio_recorder_termnames);

status
makeClassAudioRecorder(Class class)
{ declareClass(class, &audio_recorder_decls);

  succeed;
}
