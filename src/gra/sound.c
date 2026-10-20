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
toneSound(Sound s, Int frequency, Real duration)
{ TRY(ws_tone_sound(s, valInt(frequency), valReal(duration)));
  assign(s, file, NIL);

  succeed;
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

static char *T_tone[] =
	{ "frequency=20..20000", "duration=real" };

static senddecl send_sound[] =
{ SM(NAME_initialise, 1, "file=[file]", initialiseSound,
     DEFAULT, "Create, optionally loading a WAV file"),
  SM(NAME_unlink, 0, NULL, unlinkSound,
     DEFAULT, "Stop playing and free the audio"),
  SM(NAME_load, 1, "file", loadSound,
     NAME_storage, "Load audio from a WAV file"),
  SM(NAME_save, 1, "file", saveSound,
     NAME_storage, "Save the audio as a WAV file"),
  SM(NAME_tone, 2, T_tone, toneSound,
     NAME_storage, "Make a sine tone of frequency Hz for duration seconds"),
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
 * the microphone, into its <-sound or, if ->record is given a sink,
 * writes the samples to the sink while recording: a file becomes a WAV
 * file and any other source_sink gets the raw samples using
 * ->write_as_file.  While recording, <-level_message is executed
 * regularly with the recorder and the input level (0..1), e.g., to
 * show a level meter.
 */

static status
initialiseAudioRecorder(AudioRecorder r, Code msg)
{ assign(r, sound,	   newObject(ClassSound, EAV));
  assign(r, level_message, isDefault(msg) ? NIL : msg);
  assign(r, recording,	   OFF);
  assign(r, sink,	   NIL);
  r->ws_ref = NULL;

  succeed;
}


static status
unlinkAudioRecorder(AudioRecorder r)
{ ws_stop_recording(r);

  succeed;
}


/* Record into <-sound or `to`.  If `to` is a video_writer, it gets an
 * audio track for the recorded samples if it has none.
 */

static status
recordAudioRecorder(AudioRecorder r, Any to)
{ if ( r->recording == ON )
    succeed;
  if ( instanceOfObject(to, ClassVideoWriter) )
  { VideoWriter vw = to;

    if ( vw->status == NAME_new && isNil(vw->audio_frequency) )
      TRY(send(vw, NAME_audio, toInt(ws_recording_frequency()),
	       toInt(ws_recording_channels()), EAV));
  }
  assign(r, sink, isDefault(to) ? NIL : to);
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
  IV(NAME_sink, "source_sink|video_writer*", IV_GET,
     NAME_storage, "Where ->record writes the samples, or @nil"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "Window system data")
};

/* Send Methods */

static senddecl send_audio_recorder[] =
{ SM(NAME_initialise, 1, "level_message=[code]*", initialiseAudioRecorder,
     DEFAULT, "Create, optionally with a level message"),
  SM(NAME_unlink, 0, NULL, unlinkAudioRecorder,
     DEFAULT, "Stop recording"),
  SM(NAME_record, 1, "to=[source_sink|video_writer]", recordAudioRecorder,
     NAME_action, "Start recording into <-sound, a sink or a video"),
  SM(NAME_stop, 0, NULL, stopAudioRecorder,
     NAME_action, "Stop recording")
};

/* Class Declaration */

static Name audio_recorder_termnames[] = { NAME_levelMessage };

/* Get Methods */

static Name
getFormatAudioRecorder(AudioRecorder r)
{ answer(NAME_s16);
}


static Int
getFrequencyAudioRecorder(AudioRecorder r)
{ answer(toInt(ws_recording_frequency()));
}


static Int
getChannelsAudioRecorder(AudioRecorder r)
{ answer(toInt(ws_recording_channels()));
}

static getdecl get_audio_recorder[] =
{ GM(NAME_format, 0, "name", NULL, getFormatAudioRecorder,
     NAME_format, "Format of the recorded samples: s16"),
  GM(NAME_frequency, 0, "int", NULL, getFrequencyAudioRecorder,
     NAME_format, "Recorded samples per second"),
  GM(NAME_channels, 0, "int", NULL, getChannelsAudioRecorder,
     NAME_format, "Number of channels recorded")
};

ClassDecl(audio_recorder_decls,
          var_audio_recorder, send_audio_recorder, get_audio_recorder, NULL,
          1, audio_recorder_termnames);

status
makeClassAudioRecorder(Class class)
{ declareClass(class, &audio_recorder_decls);

  succeed;
}


		 /*******************************
		 *	    AUDIO STREAM	*
		 *******************************/

/* An audio_stream plays audio while it is written, e.g., by Prolog
 * using pce_open/3.  The data is raw samples in <-format, or an encoded
 * file (WAV, Ogg Vorbis, MP3, FLAC, ...) if <-format is `encoded`.
 * ->close_as_file, sent when the Prolog stream is closed, tells it the
 * data has ended.
 */

static status
initialiseAudioStream(AudioStream as, Name format, Int frequency,
		      Int channels)
{ assign(as, encoding,		NAME_octet);
  assign(as, format,		isDefault(format) ? NAME_encoded : format);
  assign(as, frequency,		isDefault(frequency) ? toInt(44100)
						     : frequency);
  assign(as, channels,		isDefault(channels) ? TWO : channels);
  assign(as, volume,		toNum(1.0));
  assign(as, low_water,		toNum(0.5));
  assign(as, low_water_message, NIL);
  assign(as, message,		NIL);
  assign(as, playing,		OFF);
  as->ws_ref = NULL;

  succeed;
}


static status
unlinkAudioStream(AudioStream as)
{ ws_free_audio_stream(as);

  succeed;
}


static status
writeAsFileAudioStream(AudioStream as, Int where, CharArray data)
{ return ws_write_audio_stream(as, data);
}


static status
appendAudioStream(AudioStream as, CharArray data)
{ return ws_write_audio_stream(as, data);
}


static status
truncateAsFileAudioStream(AudioStream as)
{ ws_clear_audio_stream(as);

  succeed;
}


static status
closeAudioStream(AudioStream as)
{ ws_close_audio_stream(as);

  succeed;
}


static status
playAudioStream(AudioStream as)
{ if ( as->playing == ON )
    succeed;

  return ws_play_audio_stream(as);
}


static status
stopAudioStream(AudioStream as)
{ ws_stop_audio_stream(as);

  succeed;
}


static status
volumeAudioStream(AudioStream as, Any volume)
{ assign(as, volume, volume);
  ws_volume_audio_stream(as);

  succeed;
}


/* Wait until everything written has been played */

static status
drainAudioStream(AudioStream as)
{ while( !onFlag(as, F_FREED|F_FREEING) &&
	 as->playing == ON && ws_queued_audio_stream(as) > 0.0 )
    dispatchDisplayManager(TheDisplayManager(), DEFAULT, DEFAULT);

  succeed;
}


static Any
getQueuedAudioStream(AudioStream as)
{ answer(toNum(ws_queued_audio_stream(as)));
}


/* Type declarations */

static char *T_initialiseAudioStream[] =
	{ "format=[{u8,s8,s16,s32,f32,encoded}]",
	  "frequency=[1..]", "channels=[1..8]" };
static char *T_writeAsFileAudioStream[] =
	{ "at=[int]", "data=char_array" };

/* Instance Variables */

static vardecl var_audio_stream[] =
{ IV(NAME_format, "{u8,s8,s16,s32,f32,encoded}", IV_GET,
     NAME_format, "Format of the samples written, or encoded data"),
  IV(NAME_frequency, "int", IV_GET,
     NAME_format, "Samples per second"),
  IV(NAME_channels, "int", IV_GET,
     NAME_format, "Number of channels: 1 is mono, 2 stereo"),
  SV(NAME_volume, "0.0..1.0", IV_GET|IV_STORE, volumeAudioStream,
     NAME_volume, "Gain applied when playing"),
  IV(NAME_lowWater, "num", IV_BOTH,
     NAME_action, "Seconds queued below which <-low_water_message runs"),
  IV(NAME_lowWaterMessage, "code*", IV_BOTH,
     NAME_action, "Executed with the stream if little is queued"),
  IV(NAME_message, "code*", IV_BOTH,
     NAME_action, "Executed when the data ended and is played"),
  IV(NAME_playing, "bool", IV_GET,
     NAME_action, "@on between ->play and the end or ->stop"),
  IV(NAME_wsRef, "alien:WsRef", IV_NONE,
     NAME_internal, "Window system data")
};

/* Send Methods */

static senddecl send_audio_stream[] =
{ SM(NAME_initialise, 3, T_initialiseAudioStream, initialiseAudioStream,
     DEFAULT, "Create for raw samples or encoded data"),
  SM(NAME_unlink, 0, NULL, unlinkAudioStream,
     DEFAULT, "Stop playing and free the audio"),
  SM(NAME_writeAsFile, 2, T_writeAsFileAudioStream, writeAsFileAudioStream,
     NAME_stream, "Queue data; supports pce_open/3"),
  SM(NAME_append, 1, "data=char_array", appendAudioStream,
     NAME_stream, "Queue data"),
  SM(NAME_truncateAsFile, 0, NULL, truncateAsFileAudioStream,
     NAME_stream, "Discard the queued data"),
  SM(NAME_closeAsFile, 0, NULL, closeAudioStream,
     NAME_stream, "The data has ended; sent by pce_open/3"),
  SM(NAME_close, 0, NULL, closeAudioStream,
     NAME_stream, "The data has ended"),
  SM(NAME_play, 0, NULL, playAudioStream,
     NAME_action, "Start playing the queued and future data"),
  SM(NAME_stop, 0, NULL, stopAudioStream,
     NAME_action, "Stop playing and discard the queued data"),
  SM(NAME_drain, 0, NULL, drainAudioStream,
     NAME_action, "Wait until the queued data is played")
};

/* Get Methods */

static getdecl get_audio_stream[] =
{ GM(NAME_queued, 0, "num", NULL, getQueuedAudioStream,
     NAME_action, "Seconds of audio queued for playing")
};

/* Class Declaration */

static Name audio_stream_termnames[] =
	{ NAME_format, NAME_frequency, NAME_channels };

ClassDecl(audio_stream_decls,
          var_audio_stream, send_audio_stream, get_audio_stream, NULL,
          3, audio_stream_termnames);

status
makeClassAudioStream(Class class)
{ declareClass(class, &audio_stream_decls);

  succeed;
}
