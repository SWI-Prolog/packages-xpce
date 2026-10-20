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

/* Class video plays an encoded video file or stream.  The frames go
 * into <-texture, an sdl_texture, so an sdl_layer shows them in an
 * sdl_window without using Cairo.  Class video_writer encodes frames
 * into a video file.  See sdl/sdlvideo.c.  The SDL resources are made
 * when first needed.
 */

		 /*******************************
		 *	       VIDEO		*
		 *******************************/

static status
initialiseVideo(Video v, Any source)
{ assign(v, encoding, NAME_octet);
  assign(v, source,  isDefault(source) ? NIL : source);
  assign(v, texture, newObject(ClassSdlTexture, EAV));
  assign(v, status,  NAME_closed);
  assign(v, loop,    OFF);
  assign(v, message, NIL);
  assign(v, audio,   ON);
  assign(v, volume,  toNum(1.0));
  assign(v, status_message, NIL);
  assign(v, options, NIL);
  v->ws = NULL;

  succeed;
}


/* Change <-status and tell <-status_message */

status
setStatusVideo(Video v, Name status)
{ if ( v->status != status )
  { assign(v, status, status);
    if ( notNil(v->status_message) && !onFlag(v, F_FREED|F_FREEING) )
      forwardReceiverCode(v->status_message, v, v, status, EAV);
  }

  succeed;
}


static status
unlinkVideo(Video v)
{ ws_close_video(v);

  succeed;
}


/* Open the video.  A network stream is `opening` until the reader
 * opened it.
 */

static status
openVideo(Video v)
{ if ( v->status != NAME_closed )
    succeed;

  TRY(ws_open_video(v));
  setStatusVideo(v, ws_video_opening(v) ? NAME_opening : NAME_paused);

  succeed;
}


static status
closeVideo(Video v)
{ ws_close_video(v);
  setStatusVideo(v, NAME_closed);

  succeed;
}


static status
playVideo(Video v)
{ TRY(openVideo(v));

  if ( v->status == NAME_finished )
    ws_seek_video(v, 0.0);
  ws_play_video(v);
  if ( v->status != NAME_opening )	/* plays when it is open */
    setStatusVideo(v, NAME_playing);

  succeed;
}


static status
pauseVideo(Video v)
{ ws_pause_video(v);
  if ( v->status == NAME_playing )
    setStatusVideo(v, NAME_paused);

  succeed;
}


static BoolObj getLiveVideo(Video v);

static status
stopVideo(Video v)
{ if ( v->status != NAME_closed && v->status != NAME_opening )
  { pauseVideo(v);
    if ( getLiveVideo(v) != ON )
      ws_seek_video(v, 0.0);
    setStatusVideo(v, NAME_paused);
  }

  succeed;
}


static status
positionVideo(Video v, Num pos)
{ TRY(openVideo(v));

  if ( getLiveVideo(v) == ON )
    return errorPce(v, NAME_cannotSeek);
  ws_seek_video(v, valNum(pos));
  if ( v->status == NAME_finished )
    setStatusVideo(v, NAME_paused);

  succeed;
}


/* A video without <-source plays the data written to it, e.g., using
 * pce_open/3.  ->close_as_file tells the data has ended.
 */

static status
appendVideo(Video v, CharArray data)
{ if ( notNil(v->source) )
    return errorPce(v, NAME_ioError, CtoName("the video has a source"));
  TRY(openVideo(v));

  return ws_write_video(v, data);
}


static status
writeAsFileVideo(Video v, Int where, CharArray data)
{ return appendVideo(v, data);
}


/* Start again with new data, as pce_open/3 does in mode `write` */

static status
truncateAsFileVideo(Video v)
{ if ( isNil(v->source) && v->status != NAME_closed )
  { ws_close_video(v);
    setStatusVideo(v, NAME_closed);
  }

  succeed;
}


static status
closeAsFileVideo(Video v)
{ if ( isNil(v->source) && v->status != NAME_closed )
    return ws_write_video(v, NULL);

  succeed;
}


/* Options for opening, e.g., `rtsp_transport` `tcp`.  They are passed
 * to the backend when the video is opened.
 */

static status
optionVideo(Video v, Name name, Any value)
{ if ( isNil(v->options) )
    assign(v, options, newObject(ClassSheet, EAV));

  return valueSheet(v->options, name, value);
}


static Num
getPositionVideo(Video v)
{ answer(toNum(ws_video_position(v)));
}


static status
volumeVideo(Video v, Num volume)
{ assign(v, volume, volume);
  ws_volume_video(v);

  succeed;
}


/* Information about the video, which is opened if needed */

static bool
info_video(Video v, video_info *info)
{ return openVideo(v) && ws_video_info(v, info);
}

static Size
getSizeVideo(Video v)
{ video_info info;

  if ( info_video(v, &info) )
    answer(newObject(ClassSize, toInt(info.width), toInt(info.height), EAV));
  fail;
}

static Num
getFrameRateVideo(Video v)
{ video_info info;

  if ( info_video(v, &info) )
    answer(toNum(info.frame_rate));
  fail;
}

static Num
getDurationVideo(Video v)
{ video_info info;

  if ( info_video(v, &info) )
    answer(toNum(info.duration));
  fail;
}

static Name
getCodecVideo(Video v)
{ video_info info;

  if ( info_video(v, &info) )
    answer(CtoName(info.codec));
  fail;
}

static Name
getAudioCodecVideo(Video v)
{ video_info info;

  if ( info_video(v, &info) && info.audio_codec )
    answer(CtoName(info.audio_codec));
  fail;
}

static BoolObj
getLiveVideo(Video v)
{ video_info info;

  if ( info_video(v, &info) )
    answer(info.live ? ON : OFF);
  fail;
}

static BoolObj
getHasAudioVideo(Video v)
{ video_info info;

  if ( info_video(v, &info) )
    answer(info.audio_codec ? ON : OFF);
  fail;
}


/* The current frame or, with `at`, the frame at a time */

static Image
getImageVideo(Video v, Num at)
{ if ( isDefault(at) )
    answer(ws_sdl_texture_image(v->texture));

  answer(ws_video_frame_image(v, valNum(at)));
}


/* Instance Variables */

static vardecl var_video[] =
{ IV(NAME_source, "file|name*", IV_GET,
     NAME_file, "File or URL of the video; @nil: written to it"),
  IV(NAME_texture, "sdl_texture", IV_GET,
     NAME_video, "Shows the current frame"),
  IV(NAME_status, "{closed,opening,paused,playing,finished}", IV_GET,
     NAME_status, "Whether the video is open and playing"),
  IV(NAME_loop, "bool", IV_BOTH,
     NAME_status, "Start again at the end"),
  IV(NAME_message, "code*", IV_BOTH,
     NAME_status, "Executed with the video when it ends"),
  IV(NAME_audio, "bool", IV_BOTH,
     NAME_sound, "Play the audio (when opened)"),
  SV(NAME_volume, "0.0..1.0", IV_GET|IV_STORE, volumeVideo,
     NAME_sound, "Gain applied to the audio"),
  IV(NAME_statusMessage, "code*", IV_BOTH,
     NAME_status, "Executed with the video and status if it changes"),
  IV(NAME_options, "sheet*", IV_GET,
     NAME_file, "Options for opening the source"),
  IV(NAME_ws, "alien:WsVideo*", IV_NONE,
     NAME_internal, "Reader and frame queue")
};

/* Send Methods */

static char *T_writeAsFileVideo[] =
	{ "at=[int]", "data=char_array" };
static char *T_optionVideo[] =
	{ "name=name", "value=char_array|int" };

static senddecl send_video[] =
{ SM(NAME_initialise, 1, "source=[file|name]*", initialiseVideo,
     DEFAULT, "Create to play a video file or URL or data written to it"),
  SM(NAME_append, 1, "data=char_array", appendVideo,
     NAME_stream, "Add encoded data (if <-source is @nil)"),
  SM(NAME_writeAsFile, 2, T_writeAsFileVideo, writeAsFileVideo,
     NAME_stream, "Add encoded data; supports pce_open/3"),
  SM(NAME_truncateAsFile, 0, NULL, truncateAsFileVideo,
     NAME_stream, "Start again with new data"),
  SM(NAME_closeAsFile, 0, NULL, closeAsFileVideo,
     NAME_stream, "The data has ended; sent by pce_open/3"),
  SM(NAME_unlink, 0, NULL, unlinkVideo,
     DEFAULT, "Close the video"),
  SM(NAME_open, 0, NULL, openVideo,
     NAME_status, "Open the video and show the first frame"),
  SM(NAME_close, 0, NULL, closeVideo,
     NAME_status, "Close the video, freeing its resources"),
  SM(NAME_play, 0, NULL, playVideo,
     NAME_status, "Start or continue playing"),
  SM(NAME_pause, 0, NULL, pauseVideo,
     NAME_status, "Stop playing at the current position"),
  SM(NAME_stop, 0, NULL, stopVideo,
     NAME_status, "Stop playing and go back to the start"),
  SM(NAME_position, 1, "seconds=num", positionVideo,
     NAME_status, "Go to a time in the video"),
  SM(NAME_option, 2, T_optionVideo, optionVideo,
     NAME_file, "Option for opening, e.g., rtsp_transport: tcp")
};

/* Get Methods */

static getdecl get_video[] =
{ GM(NAME_position, 0, "seconds=num", NULL, getPositionVideo,
     NAME_status, "Time of the current frame"),
  GM(NAME_size, 0, "size", NULL, getSizeVideo,
     NAME_video, "Size of the frames"),
  GM(NAME_frameRate, 0, "num", NULL, getFrameRateVideo,
     NAME_video, "Frames per second; 0 if unknown"),
  GM(NAME_duration, 0, "seconds=num", NULL, getDurationVideo,
     NAME_video, "Length of the video; 0 if unknown"),
  GM(NAME_codec, 0, "name", NULL, getCodecVideo,
     NAME_video, "Name of the decoder"),
  GM(NAME_audioCodec, 0, "name", NULL, getAudioCodecVideo,
     NAME_sound, "Name of the audio decoder; fails without audio"),
  GM(NAME_hasAudio, 0, "bool", NULL, getHasAudioVideo,
     NAME_sound, "Whether the video has audio we play"),
  GM(NAME_live, 0, "bool", NULL, getLiveVideo,
     NAME_status, "Whether this is a live stream that cannot seek"),
  GM(NAME_image, 1, "image", "at=[num]", getImageVideo,
     NAME_video, "Copy of the current frame or the frame at a time")
};

/* Class Declaration */

static Name video_termnames[] = { NAME_source };

ClassDecl(video_decls,
          var_video, send_video, get_video, NULL,
          1, video_termnames);

status
makeClassVideo(Class class)
{ return declareClass(class, &video_decls);
}


		 /*******************************
		 *	   VIDEO WRITER		*
		 *******************************/

/* A network URL is written live by default */

static bool
is_url(Any target)
{ return ( instanceOfObject(target, ClassCharArray) &&
	   strstr(charArrayToUTF8(target), "://") );
}

static status
initialiseVideoWriter(VideoWriter vw, Any target, Size size, Num frame_rate,
		      Name codec, Int quality)
{ assign(vw, target,	 target);
  assign(vw, size,	 newObject(ClassSize, size->w, size->h, EAV));
  assign(vw, frame_rate, isDefault(frame_rate) ? toNum(30.0) : frame_rate);
  assign(vw, codec,	 isDefault(codec) ? NIL : codec);
  assign(vw, quality,	 isDefault(quality) ? NIL : quality);
  assign(vw, frames,	 ZERO);
  assign(vw, audio_frequency, NIL);
  assign(vw, audio_channels,  NIL);
  assign(vw, audio_codec,     NIL);
  assign(vw, status,	 NAME_new);
  assign(vw, format,	 NIL);
  assign(vw, live,	 is_url(target) ? ON : OFF);
  assign(vw, bitrate,	 NIL);
  assign(vw, dropped,	 ZERO);
  assign(vw, recording,	 NIL);
  assign(vw, timer,	 NIL);
  assign(vw, record_start, toNum(0.0));
  vw->ws = NULL;

  succeed;
}


/* Give the video an audio track.  This must be done before the first
 * frame or samples are appended.
 */

static status
audioVideoWriter(VideoWriter vw, Int frequency, Int channels)
{ if ( vw->status != NAME_new )
    return errorPce(vw, NAME_cannotOpen,
		    CtoName("audio must be added before the first frame"));

  assign(vw, audio_frequency, isDefault(frequency) ? toInt(48000) : frequency);
  assign(vw, audio_channels,  isDefault(channels) ? toInt(2) : channels);

  succeed;
}


/* Settings that must be made before the first frame */

static status
settingVideoWriter(VideoWriter vw, Name slot, Any value)
{ if ( vw->status != NAME_new )
    return errorPce(vw, NAME_cannotOpen,
		    CtoName("must be set before the first frame"));

  return send(vw, NAME_slot, slot, value, EAV);
}

static status
formatVideoWriter(VideoWriter vw, Name format)
{ return settingVideoWriter(vw, NAME_format, format);
}

static status
liveVideoWriter(VideoWriter vw, BoolObj live)
{ return settingVideoWriter(vw, NAME_live, live);
}

static status
bitrateVideoWriter(VideoWriter vw, Int bitrate)
{ return settingVideoWriter(vw, NAME_bitrate, bitrate);
}


/* Seconds since some point in the past */

static double
now_seconds(void)
{ return (double)SDL_GetTicksNS()/1e9;
}


/* The time of the next frame at <-frame_rate */

static double
next_frame_time(VideoWriter vw)
{ return valInt(vw->frames)/valNum(vw->frame_rate);
}


/* Record a frame or window: append its <-image at <-frame_rate until
 * ->stop or ->close.  The frames get the time they were taken.
 */

static status
recordVideoWriter(VideoWriter vw, Any from)
{ double secs = 1.0/valNum(vw->frame_rate);

  if ( notNil(vw->timer) )
    stopTimer(vw->timer);
  assign(vw, recording, from);
  assign(vw, record_start, toNum(now_seconds() - next_frame_time(vw)));
  if ( isNil(vw->timer) )
    assign(vw, timer,
	   newObject(ClassTimer, toNum(secs),
		     newObject(ClassMessage, vw, NAME_recordFrame, EAV),
		     EAV));
  else
    intervalTimer(vw->timer, toNum(secs));

  TRY(send(vw, NAME_recordFrame, EAV));
  return startTimer(vw->timer, NAME_repeat, DEFAULT);
}


static status
stopVideoWriter(VideoWriter vw)
{ if ( notNil(vw->timer) )
    stopTimer(vw->timer);
  assign(vw, recording, NIL);

  succeed;
}


static status appendVideoWriter(VideoWriter vw, Any frame, Num at);

static status
recordFrameVideoWriter(VideoWriter vw)
{ Any from = vw->recording;

  if ( isNil(from) || isFreedObj(from) || vw->status == NAME_closed )
    return stopVideoWriter(vw);

  double at = now_seconds() - valNum(vw->record_start);

  if ( !appendVideoWriter(vw, from, toNum(at)) )
  { stopVideoWriter(vw);
    fail;
  }

  succeed;
}


static status
closeVideoWriter(VideoWriter vw)
{ stopVideoWriter(vw);

  if ( vw->status == NAME_open )
  { assign(vw, status, NAME_closed);
    return ws_close_video_writer(vw);
  }

  succeed;
}


static status
unlinkVideoWriter(VideoWriter vw)
{ return closeVideoWriter(vw);
}


static status openVideoWriter(VideoWriter vw);

/* Append samples.  They need not have the frequency and channels of
 * the video, which are those of the first samples if ->audio was not
 * used.
 */

static status
appendAudioVideoWriter(VideoWriter vw, CharArray data, Name format,
		       Int frequency, Int channels)
{ if ( vw->status == NAME_new && isNil(vw->audio_frequency) )
    TRY(audioVideoWriter(vw, frequency, channels));
  TRY(openVideoWriter(vw));

  if ( isDefault(format) )
    format = NAME_s16;
  if ( isDefault(frequency) )
    frequency = vw->audio_frequency;
  if ( isDefault(channels) )
    channels = vw->audio_channels;
  if ( isNil(frequency) )
    return errorPce(vw, NAME_ioError, CtoName("the video has no audio"));

  return ws_append_audio_video_writer(vw, data, format,
				      valInt(frequency), valInt(channels));
}


static status
openVideoWriter(VideoWriter vw)
{ if ( vw->status == NAME_open )
    succeed;
  if ( vw->status == NAME_closed )
    return errorPce(vw, NAME_cannotOpen, CtoName("already closed"));

  TRY(ws_open_video_writer(vw));
  assign(vw, status, NAME_open);

  succeed;
}


/* Add a frame.  Without `at`, frames follow each other at
 * <-frame_rate.
 */

static status
appendVideoWriter(VideoWriter vw, Any frame, Num at)
{ double pts = isDefault(at) ? next_frame_time(vw) : valNum(at);
  SDL_Surface *s = NULL;
  bool free_s = false;
  status rc;

  TRY(openVideoWriter(vw));

  if ( instanceOfObject(frame, ClassSdlTexture) )
  { if ( !(s = ((SdlTexture)frame)->surface) )
      return errorPce(frame, NAME_noData);
  } else
  { Image img = frame;

    if ( !instanceOfObject(frame, ClassImage) &&
	 !(img = get(frame, NAME_image, EAV)) )
      fail;
    if ( !(s = pceImage2SDL_Surface(img)) )
      return errorPce(img, NAME_noData);
    free_s = true;
  }

  rc = ws_append_video_writer(vw, s, pts);
  if ( free_s )
    SDL_DestroySurface(s);
  if ( rc )
    assign(vw, frames, add(vw->frames, ONE));

  return rc;
}


/* Instance Variables */

static vardecl var_video_writer[] =
{ IV(NAME_target, "file|name|code", IV_GET,
     NAME_file, "File or URL the video is written to, or code that gets it"),
  IV(NAME_size, "size", IV_GET,
     NAME_video, "Size of the frames; other frames are scaled"),
  IV(NAME_frameRate, "num", IV_GET,
     NAME_video, "Frames per second"),
  IV(NAME_codec, "name*", IV_GET,
     NAME_video, "Encoder; @nil: the best available"),
  IV(NAME_quality, "0..100*", IV_GET,
     NAME_video, "Quality; @nil: the default of the encoder"),
  IV(NAME_frames, "int", IV_GET,
     NAME_video, "Number of frames appended"),
  IV(NAME_audioFrequency, "int*", IV_GET,
     NAME_sound, "Samples per second of the audio; @nil: no audio"),
  IV(NAME_audioChannels, "int*", IV_GET,
     NAME_sound, "Number of channels of the audio"),
  IV(NAME_audioCodec, "name*", IV_GET,
     NAME_sound, "Encoder of the audio"),
  IV(NAME_status, "{new,open,closed}", IV_GET,
     NAME_status, "Whether frames can be appended"),
  SV(NAME_format, "name*", IV_GET|IV_STORE, formatVideoWriter,
     NAME_file, "Container, e.g., mp4, mpegts; @nil: from the target"),
  SV(NAME_live, "bool", IV_GET|IV_STORE, liveVideoWriter,
     NAME_video, "Low latency: drop frames rather than wait"),
  SV(NAME_bitrate, "1..*", IV_GET|IV_STORE, bitrateVideoWriter,
     NAME_video, "Bits/second; @nil: from <-quality"),
  IV(NAME_dropped, "int", IV_GET,
     NAME_video, "Frames dropped because the encoder fell behind"),
  IV(NAME_recording, "frame|window*", IV_GET,
     NAME_record, "Frame or window being recorded"),
  IV(NAME_timer, "timer*", IV_NONE,
     NAME_record, "Records frames"),
  IV(NAME_recordStart, "num", IV_NONE,
     NAME_record, "Time of frame 0 while recording"),
  IV(NAME_ws, "alien:WsVideoWriter*", IV_NONE,
     NAME_internal, "Encoder and frame queue")
};

/* Send Methods */

static char *T_initialiseVideoWriter[] =
	{ "target=file|name|code", "size=size", "frame_rate=[num]",
	  "codec=[name]", "quality=[0..100]" };
static char *T_appendVideoWriter[] =
	{ "frame=image|sdl_texture|frame|window", "at=[num]" };
static char *T_audioVideoWriter[] =
	{ "frequency=[1..]", "channels=[1..8]" };
static char *T_appendAudioVideoWriter[] =
	{ "data=char_array", "format=[{u8,s8,s16,s32,f32}]",
	  "frequency=[1..]", "channels=[1..8]" };

static senddecl send_video_writer[] =
{ SM(NAME_initialise, 5, T_initialiseVideoWriter, initialiseVideoWriter,
     DEFAULT, "Create to write a video file"),
  SM(NAME_unlink, 0, NULL, unlinkVideoWriter,
     DEFAULT, "Finish the file"),
  SM(NAME_append, 2, T_appendVideoWriter, appendVideoWriter,
     NAME_video, "Add a frame, at a time in seconds"),
  SM(NAME_audio, 2, T_audioVideoWriter, audioVideoWriter,
     NAME_sound, "Add an audio track (default 48000 Hz, stereo)"),
  SM(NAME_appendAudio, 4, T_appendAudioVideoWriter, appendAudioVideoWriter,
     NAME_sound, "Add samples to the audio"),
  SM(NAME_record, 1, "from=frame|window", recordVideoWriter,
     NAME_record, "Append the image of from at <-frame_rate"),
  SM(NAME_recordFrame, 0, NULL, recordFrameVideoWriter,
     NAME_record, "Append the image of <-recording"),
  SM(NAME_stop, 0, NULL, stopVideoWriter,
     NAME_record, "Stop ->record"),
  SM(NAME_close, 0, NULL, closeVideoWriter,
     NAME_status, "Encode the remaining frames and finish the file")
};

/* Get Methods */

#define get_video_writer NULL

/* Class Declaration */

static Name video_writer_termnames[] = { NAME_target, NAME_size };

ClassDecl(video_writer_decls,
          var_video_writer, send_video_writer, get_video_writer, NULL,
          2, video_writer_termnames);

status
makeClassVideoWriter(Class class)
{ return declareClass(class, &video_writer_decls);
}
