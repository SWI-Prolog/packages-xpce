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
#include <h/unix.h>
#include "sdluserevent.h"
#include <math.h>
#ifdef HAVE_SDL3_SOUND
#include <SDL3_sound/SDL_sound.h>
#endif

/* Audio for class sound and class audio_recorder.  SDL calls its audio
 * callbacks from its own thread, where we cannot run xpce code.
 * Instead, a timer pushes MY_EVENT_MEDIA while sounds are playing,
 * recorders are recording or cameras are open.  sdl_audio_event()
 * handles this event in the main loop: it finishes plays whose audio
 * was consumed, moves the recorded audio into the sound of each
 * recorder and lets sdl_camera_tick() deliver camera frames.
 */

#define MEDIA_TICK_MS 15		/* poll interval */

typedef struct ws_sound
{ SDL_AudioSpec spec;			/* format of data */
  Uint8	       *data;			/* samples (SDL_malloc()) */
  Uint32	len;			/* bytes in data */
  Uint32	allocated;		/* allocated bytes */
} ws_sound;

typedef struct play
{ struct play  *next;
  Sound		sound;			/* we hold a code reference */
  SDL_AudioStream *stream;
} play;

typedef struct recorder
{ struct recorder *next;
  AudioRecorder recorder;		/* we hold a code reference */
  SDL_AudioStream *stream;
  SDL_IOStream *file;			/* WAV file we record to */
  Uint64	written;		/* bytes written to the sink */
} recorder;


static play	*plays = NULL;
static recorder *recorders = NULL;
static struct astream *astreams = NULL;
static SDL_AtomicInt ticking;		/* the timer is running */

static bool
init_audio(void)
{ if ( !SDL_WasInit(SDL_INIT_AUDIO) &&
       !SDL_InitSubSystem(SDL_INIT_AUDIO) )
  { Cprintf("Could not initialise audio: %s\n", SDL_GetError());
    return false;
  }

  return true;
}

static Uint32 SDLCALL
media_tick(void *udata, SDL_TimerID id, Uint32 interval)
{ if ( !SDL_GetAtomicInt(&ticking) )
    return 0;

  SDL_Event ev;
  SDL_zero(ev);
  ev.type = MY_EVENT_MEDIA;
  SDL_PushEvent(&ev);

  return interval;
}

void
sdl_start_media_tick(void)
{ if ( SDL_CompareAndSwapAtomicInt(&ticking, 0, 1) )
    SDL_AddTimer(MEDIA_TICK_MS, media_tick, NULL);
}


		 /*******************************
		 *	       SOUND		*
		 *******************************/

static ws_sound *
sound_data(Sound s, bool create)
{ if ( !s->ws_ref && create )
  { ws_sound *wss = SDL_calloc(1, sizeof(*wss));

    s->ws_ref = wss;
  }

  return s->ws_ref;
}

static void
free_sound_data(ws_sound *wss)
{ if ( wss->data )
    SDL_free(wss->data);
  wss->data = NULL;
  wss->len = wss->allocated = 0;
}

/* Update the slots that describe the audio of `s` */

static void
update_sound(Sound s)
{ ws_sound *wss = s->ws_ref;

  if ( wss && wss->spec.freq > 0 )
  { int frame = SDL_AUDIO_FRAMESIZE(wss->spec);

    assign(s, frequency, toInt(wss->spec.freq));
    assign(s, channels,  toInt(wss->spec.channels));
    assign(s, duration,  toNum((double)wss->len/frame/wss->spec.freq));
  } else
  { assign(s, frequency, ZERO);
    assign(s, channels,  ZERO);
    assign(s, duration,  ZERO);
  }
}

#ifdef HAVE_SDL3_SOUND
/* Decode a sound file into memory using SDL_sound, which handles WAV,
 * MP3, Ogg Vorbis, FLAC, etc.  The decoder is chosen from the file
 * extension; if that fails, SDL_sound tries all decoders.
 */

static bool
init_sound_decoders(void)
{ static bool done = false;

  if ( !done )
    done = Sound_Init() != 0;

  return done;
}

static bool
decode_file(const char *fn, SDL_AudioSpec *spec, Uint8 **data, Uint32 *len,
	    const char **err)
{ if ( !init_sound_decoders() )
  { *err = Sound_GetError();
    return false;
  }

  Sound_Sample *smp = Sound_NewSampleFromFile(fn, NULL, 65536);
  if ( !smp )
  { *err = Sound_GetError();
    return false;
  }

  Uint32 n = Sound_DecodeAll(smp);
  bool rc = false;

  if ( (smp->flags & SOUND_SAMPLEFLAG_ERROR) || n == 0 )
  { *err = Sound_GetError();
  } else if ( (*data = SDL_malloc(n)) )
  { memcpy(*data, smp->buffer, n);
    *spec = smp->actual;
    *len  = n;
    rc = true;
  } else
    *err = "Not enough memory";
  Sound_FreeSample(smp);

  return rc;
}
#endif /*HAVE_SDL3_SOUND*/

status
ws_load_sound(Sound s, FileObj file)
{ char *fn = charArrayToFN((CharArray)getOsNameFile(file));
  SDL_AudioSpec spec;
  Uint8 *data;
  Uint32 len;

#ifdef HAVE_SDL3_SOUND
  const char *err = NULL;

  if ( !decode_file(fn, &spec, &data, &len, &err) )
    return errorPce(s, NAME_cannotLoad, file,
		    CtoName(err && err[0] ? err : "cannot decode"));
#else
  if ( !SDL_LoadWAV(fn, &spec, &data, &len) )
    return errorPce(s, NAME_cannotLoad, file, CtoName(SDL_GetError()));
#endif

  ws_stop_sound(s);
  ws_sound *wss = sound_data(s, true);
  free_sound_data(wss);
  wss->spec = spec;
  wss->data = data;
  wss->len = wss->allocated = len;
  update_sound(s);

  succeed;
}


static void
put_le(Uint8 *p, Uint32 v, int bytes)
{ for(int i=0; i<bytes; i++)
    p[i] = (Uint8)(v >> (8*i));
}

/* Fill the canonical 44-byte header of a WAV file holding `len` bytes
 * of samples in `spec`.  WAV is little endian; we assume the samples are
 * as well.
 */

#define WAV_HEADER_SIZE 44

static void
wav_header(Uint8 *hdr, const SDL_AudioSpec *spec, Uint32 len)
{ int bits     = SDL_AUDIO_BITSIZE(spec->format);
  int channels = spec->channels;
  int rate     = spec->freq;

  memcpy(hdr, "RIFF", 4);    put_le(hdr+4, 36+len, 4);
  memcpy(hdr+8, "WAVEfmt ", 8);
  put_le(hdr+16, 16, 4);			/* fmt chunk size */
  put_le(hdr+20, SDL_AUDIO_ISFLOAT(spec->format) ? 3 : 1, 2);
  put_le(hdr+22, channels, 2);
  put_le(hdr+24, rate, 4);
  put_le(hdr+28, rate*channels*bits/8, 4);	/* bytes per second */
  put_le(hdr+32, channels*bits/8, 2);	/* block align */
  put_le(hdr+34, bits, 2);
  memcpy(hdr+36, "data", 4); put_le(hdr+40, len, 4);
}


/* Write a WAV file */

status
ws_save_sound(Sound s, FileObj file)
{ ws_sound *wss = s->ws_ref;

  if ( !wss || wss->spec.freq == 0 )
    return errorPce(s, NAME_noData);

  char *fn = charArrayToFN((CharArray)getOsNameFile(file));
  SDL_IOStream *out = SDL_IOFromFile(fn, "wb");
  if ( !out )
    return errorPce(s, NAME_cannotSave, file, CtoName(SDL_GetError()));

  Uint8 hdr[WAV_HEADER_SIZE];

  wav_header(hdr, &wss->spec, wss->len);

  bool rc = ( SDL_WriteIO(out, hdr, sizeof(hdr)) == sizeof(hdr) &&
	      SDL_WriteIO(out, wss->data, wss->len) == wss->len );
  if ( !SDL_CloseIO(out) )
    rc = false;
  if ( !rc )
    return errorPce(s, NAME_cannotSave, file, CtoName(SDL_GetError()));

  succeed;
}


#define TONE_RATE 44100			/* samples per second */
#define TONE_FADE 0.005			/* seconds; avoids clicks */

/**
 * Make `s` a sine tone of `freq` Hz that lasts `secs` seconds.  It
 * fades in and out over a few milliseconds, as a tone that starts or
 * stops abruptly clicks.
 */
status
ws_tone_sound(Sound s, int freq, double secs)
{ int n = (int)(TONE_RATE*secs + 0.5);
  int fade = (int)(TONE_RATE*TONE_FADE);
  Sint16 *buf;

  if ( n <= 0 )
    return errorPce(s, NAME_noData);
  if ( fade > n/2 )
    fade = n/2;
  if ( !(buf = SDL_malloc((size_t)n*sizeof(Sint16))) )
    return errorPce(s, NAME_notEnoughMemory);

  for(int i=0; i<n; i++)
  { double env = 1.0;

    if ( i < fade )
      env = (double)i/fade;
    else if ( n-1-i < fade )
      env = (double)(n-1-i)/fade;
    buf[i] = (Sint16)(env * 32000.0 * sin(2.0*M_PI*freq*i/TONE_RATE));
  }

  ws_sound *wss = sound_data(s, true);
  free_sound_data(wss);
  wss->spec.format   = SDL_AUDIO_S16;
  wss->spec.channels = 1;
  wss->spec.freq     = TONE_RATE;
  wss->data	     = (Uint8 *)buf;
  wss->len	     = wss->allocated = (Uint32)(n*sizeof(Sint16));
  update_sound(s);

  succeed;
}


status
ws_play_sound(Sound s, double volume)
{ ws_sound *wss = s->ws_ref;

  if ( !wss || wss->len == 0 )
    return errorPce(s, NAME_noData);
  if ( !init_audio() )
    fail;

  SDL_AudioStream *stream =
    SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
			      &wss->spec, NULL, NULL);
  if ( !stream )
    return errorPce(s, NAME_cannotPlay, CtoName(SDL_GetError()));

  SDL_SetAudioStreamGain(stream, (float)volume);
  SDL_PutAudioStreamData(stream, wss->data, (int)wss->len);
  SDL_FlushAudioStream(stream);
  SDL_ResumeAudioStreamDevice(stream);

  play *p = SDL_calloc(1, sizeof(*p));
  p->sound  = s;
  p->stream = stream;
  p->next   = plays;
  plays     = p;
  addCodeReference(s);
  sdl_start_media_tick();

  succeed;
}


/* Remove *pp from the plays.  If `finished`, run the sound's message */

static void
end_play(play **pp, bool finished)
{ play *p = *pp;
  Sound s = p->sound;

  *pp = p->next;
  SDL_DestroyAudioStream(p->stream);
  SDL_free(p);

  if ( finished && notNil(s->message) && !onFlag(s, F_FREED|F_FREEING) )
    forwardReceiverCode(s->message, s, s, EAV);
  delCodeReference(s);
  freeableObj(s);
}


status
ws_stop_sound(Sound s)
{ for(play **pp = &plays; *pp; )
  { if ( (*pp)->sound == s )
      end_play(pp, false);
    else
      pp = &(*pp)->next;
  }

  succeed;
}


bool
ws_playing_sound(Sound s)
{ for(play *p = plays; p; p = p->next)
  { if ( p->sound == s )
      return true;
  }

  return false;
}


void
ws_free_sound(Sound s)
{ ws_sound *wss = s->ws_ref;

  ws_stop_sound(s);
  if ( wss )
  { free_sound_data(wss);
    SDL_free(wss);
    s->ws_ref = NULL;
  }
}


/* Lower-case extensions of the sound files we can load */

Chain
ws_sound_file_extensions(void)
{ Chain ch = answerObject(ClassChain, EAV);

#ifdef HAVE_SDL3_SOUND
  if ( init_sound_decoders() )
  { for(const Sound_DecoderInfo **d = Sound_AvailableDecoders(); *d; d++)
    { for(const char **e = (*d)->extensions; *e; e++)
      { char buf[32];
	int i;

	for(i=0; (*e)[i] && i < (int)sizeof(buf)-1; i++)
	  buf[i] = (char)tolower((unsigned char)(*e)[i]);
	buf[i] = 0;
	if ( strcmp(buf, "raw") != 0 )	/* needs the format given */
	  appendChain(ch, CtoName(buf));
      }
    }
  }
#else
  appendChain(ch, CtoName("wav"));
#endif

  return ch;
}


/* The peak level (0..1) of each of `n` equal parts of the sound, e.g.,
 * to draw its waveform.  The level is the largest absolute sample
 * value of all channels.
 */

bool
ws_sound_peaks(Sound s, int n, double *peaks)
{ ws_sound *wss = s->ws_ref;
  SDL_AudioSpec fspec;
  Uint8 *fdata = NULL;
  int flen = 0;

  for(int i=0; i<n; i++)
    peaks[i] = 0.0;
  if ( !wss || wss->len == 0 || n <= 0 )
    return true;

  fspec = wss->spec;
  fspec.format = SDL_AUDIO_F32;
  if ( !SDL_ConvertAudioSamples(&wss->spec, wss->data, (int)wss->len,
				&fspec, &fdata, &flen) )
    return false;

  const float *samples = (const float*)fdata;
  size_t nframes = (size_t)flen/sizeof(float)/fspec.channels;

  for(size_t f=0; f<nframes; f++)
  { int part = (int)((double)f*n/nframes);

    for(int c=0; c<fspec.channels; c++)
    { double v = fabs(samples[f*fspec.channels+c]);

      if ( v > peaks[part] )
	peaks[part] = v > 1.0 ? 1.0 : v;
    }
  }
  SDL_free(fdata);

  return true;
}


		 /*******************************
		 *	     RECORDING		*
		 *******************************/

static const SDL_AudioSpec record_spec = { SDL_AUDIO_S16, 1, 44100 };

int
ws_recording_frequency(void)
{ return record_spec.freq;
}

int
ws_recording_channels(void)
{ return record_spec.channels;
}


/* Open the file of a recorder that records to a file and write the
 * header.  ws_stop_recording() fills in the sizes.
 */

static SDL_IOStream *
open_record_file(AudioRecorder r, FileObj file)
{ char *fn = charArrayToFN((CharArray)getOsNameFile(file));
  SDL_IOStream *out = SDL_IOFromFile(fn, "wb");
  Uint8 hdr[WAV_HEADER_SIZE];

  if ( !out )
  { errorPce(r, NAME_cannotRecord, CtoName(SDL_GetError()));
    return NULL;
  }
  wav_header(hdr, &record_spec, 0);
  if ( SDL_WriteIO(out, hdr, sizeof(hdr)) != sizeof(hdr) )
  { errorPce(r, NAME_cannotRecord, CtoName(SDL_GetError()));
    SDL_CloseIO(out);
    return NULL;
  }

  return out;
}


status
ws_start_recording(AudioRecorder r)
{ SDL_IOStream *file = NULL;

  if ( !init_audio() )
    fail;
  if ( instanceOfObject(r->sink, ClassFile) &&
       !(file = open_record_file(r, (FileObj)r->sink)) )
    fail;

  SDL_AudioStream *stream =
    SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_RECORDING,
			      &record_spec, NULL, NULL);
  if ( !stream )
  { if ( file )
      SDL_CloseIO(file);
    return errorPce(r, NAME_cannotRecord, CtoName(SDL_GetError()));
  }

  if ( isNil(r->sink) )
  { Sound s = r->sound;
    ws_stop_sound(s);
    ws_sound *wss = sound_data(s, true);
    free_sound_data(wss);
    wss->spec = record_spec;
    update_sound(s);
  }
  SDL_ResumeAudioStreamDevice(stream);

  recorder *rec = SDL_calloc(1, sizeof(*rec));
  rec->recorder = r;
  rec->stream   = stream;
  rec->file     = file;
  rec->next     = recorders;
  recorders     = rec;
  r->ws_ref     = rec;
  addCodeReference(r);
  sdl_start_media_tick();

  succeed;
}


/* RMS level (0..1) of `len` bytes of recorded samples */

static double
recorded_level(const Uint8 *data, int len)
{ const Sint16 *samples = (const Sint16*)data;
  int n = len/(int)sizeof(Sint16);
  double sum = 0.0;

  for(int i=0; i<n; i++)
  { double v = samples[i]/32768.0;
    sum += v*v;
  }

  return n > 0 ? sqrt(sum/n) : 0.0;
}


/* Give `len` recorded bytes to the sink of the recorder */

static void
write_to_sink(recorder *rec, const Uint8 *data, int len)
{ AudioRecorder r = rec->recorder;

  if ( rec->file )
  { if ( SDL_WriteIO(rec->file, data, len) != (size_t)len )
    { errorPce(r, NAME_cannotRecord, CtoName(SDL_GetError()));
      return;
    }
  } else
  { string str;

    str_set_n_ascii(&str, len, (char*)data);
    CharArray ca = StringToScratchCharArray(&str);
    status rc = send(r->sink, NAME_writeAsFile,
		     toInt(rec->written), ca, EAV);
    doneScratchCharArray(ca);
    if ( !rc )
      return;
  }
  rec->written += len;
}


/* Move the audio recorded by `rec` into the sound of its recorder or
 * its sink and return the RMS level (0..1) of the new audio, or -1 if
 * there is none.
 */

static double
pull_recorded(recorder *rec)
{ AudioRecorder r = rec->recorder;
  int avail = SDL_GetAudioStreamAvailable(rec->stream);

  if ( avail <= 0 )
    return -1.0;

  if ( notNil(r->sink) )
  { Uint8 *buf = SDL_malloc(avail);
    int got;

    if ( !buf )
      return -1.0;
    got = SDL_GetAudioStreamData(rec->stream, buf, avail);
    double level = got > 0 ? recorded_level(buf, got) : -1.0;
    if ( got > 0 )
      write_to_sink(rec, buf, got);
    SDL_free(buf);

    return level;
  }

  Sound s = r->sound;
  ws_sound *wss = sound_data(s, true);

  if ( wss->len + avail > wss->allocated )
  { Uint32 size = wss->allocated ? wss->allocated : 65536;

    while( size < wss->len + avail )
      size *= 2;
    wss->data = SDL_realloc(wss->data, size);
    wss->allocated = size;
  }

  int got = SDL_GetAudioStreamData(rec->stream, wss->data+wss->len, avail);
  if ( got <= 0 )
    return -1.0;

  double level = recorded_level(wss->data+wss->len, got);
  wss->len += got;
  update_sound(s);

  return level;
}


/* Finish the sink: complete the WAV header of a file or tell another
 * sink the data ended.
 */

static void
close_sink(recorder *rec)
{ AudioRecorder r = rec->recorder;

  if ( rec->file )
  { Uint8 hdr[WAV_HEADER_SIZE];
    Uint32 len = rec->written > 0xffffffff-WAV_HEADER_SIZE
		   ? 0xffffffff-WAV_HEADER_SIZE : (Uint32)rec->written;

    wav_header(hdr, &record_spec, len);
    if ( SDL_SeekIO(rec->file, 0, SDL_IO_SEEK_SET) != 0 ||
	 SDL_WriteIO(rec->file, hdr, sizeof(hdr)) != sizeof(hdr) )
      errorPce(r, NAME_cannotRecord, CtoName(SDL_GetError()));
    if ( !SDL_CloseIO(rec->file) )
      errorPce(r, NAME_cannotRecord, CtoName(SDL_GetError()));
    rec->file = NULL;
  } else if ( notNil(r->sink) &&
	      hasSendMethodObject(r->sink, NAME_closeAsFile) )
    send(r->sink, NAME_closeAsFile, EAV);
}


void
ws_stop_recording(AudioRecorder r)
{ recorder *rec = r->ws_ref;

  if ( !rec )
    return;

  pull_recorded(rec);
  close_sink(rec);
  for(recorder **rp = &recorders; *rp; rp = &(*rp)->next)
  { if ( *rp == rec )
    { *rp = rec->next;
      break;
    }
  }
  SDL_DestroyAudioStream(rec->stream);
  SDL_free(rec);
  r->ws_ref = NULL;
  delCodeReference(r);
}


		 /*******************************
		 *	    AUDIO STREAM	*
		 *******************************/

/* An audio_stream feeds the data written to it into a device stream.
 * Raw samples go in directly.  Encoded data is decoded by a thread of
 * its own using SDL_sound: SDL_sound reads from an SDL_IOStream, which
 * we implement on a buffer of the data written.  Reading blocks until
 * more data is written or the data ends, so any decoder sees an
 * ordinary stream.  The thread never touches xpce objects: the tick
 * reports its state.
 *
 * A write blocks while the stream is playing and far ahead of what
 * is played, so a producer, e.g., copy_stream_data/2 from an HTTP
 * stream, cannot fill memory.
 */

#define DECODE_AHEAD	1.0		/* seconds the decoder runs ahead */
#define WRITE_AHEAD	2.0		/* min. seconds before a write blocks */
#define ENCODED_AHEAD	(1024*1024)	/* bytes before a write blocks */
#define ENCODED_KEEP	(256*1024)	/* bytes kept for seeking back */

typedef enum
{ DEC_NONE = 0,				/* no decoder thread */
  DEC_RUNNING,				/* decoding */
  DEC_DONE,				/* all data decoded */
  DEC_ERROR				/* failed; see errmsg */
} dec_state;

typedef struct decoder
{ SDL_Mutex    *lock;			/* guards the fields below */
  SDL_Condition *cond;			/* data, room or stop */
  Uint8	       *buf;			/* data not yet read (and some read) */
  Sint64	start;			/* offset of buf[0] in the data */
  size_t	len;			/* bytes in buf */
  size_t	allocated;		/* allocated bytes of buf */
  Sint64	pos;			/* read position in the data */
  bool		eof;			/* the data has ended */
  bool		stop;			/* the thread must stop */
  bool		ready;			/* a decoder was selected */
  SDL_AudioSpec actual;			/* format of the decoded data */
  SDL_Thread   *thread;
  SDL_AtomicInt state;			/* dec_state */
  char		errmsg[256];
} decoder;

typedef struct astream
{ struct astream *next;
  AudioStream	as;			/* code reference while playing */
  SDL_AudioStream *stream;		/* device stream, data goes in here */
  SDL_AudioSpec spec;			/* format of the raw data */
  Uint8		partial[64];		/* incomplete sample frame */
  int		partial_len;		/* bytes in partial */
  bool		encoded;		/* data is decoded by `dec` */
  decoder	dec;
  bool		closed;			/* the data has ended */
  bool		low;			/* low_water_message was sent */
  bool		referenced;		/* we hold a code reference */
  bool		stopped;		/* ->stop: writes fail */
  Uint64	ended_at;		/* ticks when all was consumed */
} astream;

static SDL_AudioFormat
audio_format(Name name)
{ if ( name == NAME_u8 )  return SDL_AUDIO_U8;
  if ( name == NAME_s8 )  return SDL_AUDIO_S8;
  if ( name == NAME_s32 ) return SDL_AUDIO_S32;
  if ( name == NAME_f32 ) return SDL_AUDIO_F32;
  return SDL_AUDIO_S16;
}


/* Seconds of audio in the device stream that are not yet played */

static double
queued_seconds(astream *a)
{ SDL_AudioSpec src;

  if ( !a->stream )
    return 0.0;

  int bytes = SDL_GetAudioStreamQueued(a->stream);

  if ( bytes <= 0 || !SDL_GetAudioStreamFormat(a->stream, &src, NULL) )
    return 0.0;

  int rate = SDL_AUDIO_FRAMESIZE(src)*src.freq;

  return rate > 0 ? (double)bytes/rate : 0.0;
}


#ifdef HAVE_SDL3_SOUND

		 /*******************************
		 *	      DECODER		*
		 *******************************/

/* Read as from a file: wait until `size` bytes are available, unless
 * the data ends.  A decoder that gets less returns partial sample
 * frames.
 */

static size_t SDLCALL
dec_read(void *udata, void *ptr, size_t size, SDL_IOStatus *status)
{ decoder *d = udata;
  size_t n = 0;

  SDL_LockMutex(d->lock);
  while( !d->stop && !d->eof &&
	 d->pos + (Sint64)size > d->start + (Sint64)d->len )
    SDL_WaitCondition(d->cond, d->lock);

  if ( d->stop )
  { *status = SDL_IO_STATUS_ERROR;
  } else
  { size_t avail = (size_t)(d->start + (Sint64)d->len - d->pos);

    if ( avail == 0 )
    { *status = SDL_IO_STATUS_EOF;
    } else
    { n = size < avail ? size : avail;
      memcpy(ptr, d->buf + (d->pos - d->start), n);
      d->pos += n;

      if ( d->ready )			/* drop what we will not seek to */
      { size_t done = (size_t)(d->pos - d->start);

	if ( done > 2*ENCODED_KEEP )
	{ size_t drop = done - ENCODED_KEEP;

	  memmove(d->buf, d->buf+drop, d->len-drop);
	  d->len   -= drop;
	  d->start += drop;
	}
      }
      SDL_BroadcastCondition(d->cond);	/* room for a blocked writer */
    }
  }
  SDL_UnlockMutex(d->lock);

  return n;
}


/* Seek within the data we have.  Seeking forward waits for the data.
 * The size of the data is unknown, so we cannot seek from the end.
 */

static Sint64 SDLCALL
dec_seek(void *udata, Sint64 offset, SDL_IOWhence whence)
{ decoder *d = udata;
  Sint64 to;

  SDL_LockMutex(d->lock);
  switch(whence)
  { case SDL_IO_SEEK_SET: to = offset; break;
    case SDL_IO_SEEK_CUR: to = d->pos + offset; break;
    default:
      if ( d->eof )
      { to = d->start + (Sint64)d->len + offset;
	break;
      }
      SDL_UnlockMutex(d->lock);
      return SDL_SetError("cannot seek from the end of a stream");
  }

  while( !d->stop && !d->eof && to > d->start + (Sint64)d->len )
    SDL_WaitCondition(d->cond, d->lock);

  if ( d->stop || to < d->start || to > d->start + (Sint64)d->len )
  { SDL_UnlockMutex(d->lock);
    return SDL_SetError("cannot seek to %" SDL_PRIs64, to);
  }
  d->pos = to;
  SDL_UnlockMutex(d->lock);

  return to;
}


static Sint64 SDLCALL
dec_size(void *udata)
{ (void)udata;

  return -1;				/* unknown */
}


static bool SDLCALL
dec_close(void *udata)
{ (void)udata;				/* the decoder is freed by us */

  return true;
}


static void
dec_fail(decoder *d, const char *msg)
{ SDL_strlcpy(d->errmsg, msg && msg[0] ? msg : "cannot decode",
	      sizeof(d->errmsg));
  SDL_SetAtomicInt(&d->state, DEC_ERROR);
}


/* SDL only accepts whole sample frames.  If a decoder returns part of
 * a frame anyway, we keep it for the next call.
 */

typedef struct frame_carry
{ Uint8	bytes[64];
  int	len;
} frame_carry;

static void
put_decoded(astream *a, frame_carry *c, int frame,
	    const Uint8 *data, size_t len)
{ if ( frame <= 0 || frame > (int)sizeof(c->bytes) )
    return;
  if ( c->len > 0 )
  { size_t n = frame - c->len;

    if ( n > len )
      n = len;
    memcpy(c->bytes+c->len, data, n);
    c->len += (int)n;
    data += n;
    len -= n;
    if ( c->len < frame )
      return;
    SDL_PutAudioStreamData(a->stream, c->bytes, frame);
    c->len = 0;
  }

  size_t whole = len - len%frame;

  if ( whole > 0 )
    SDL_PutAudioStreamData(a->stream, data, (int)whole);
  memcpy(c->bytes, data+whole, len-whole);
  c->len = (int)(len-whole);
}


static int SDLCALL
dec_thread(void *udata)
{ astream *a = udata;
  decoder *d = &a->dec;
  SDL_IOStreamInterface iface;

  SDL_INIT_INTERFACE(&iface);
  iface.size  = dec_size;
  iface.seek  = dec_seek;
  iface.read  = dec_read;
  iface.close = dec_close;

  SDL_IOStream *io = SDL_OpenIO(&iface, d);
  if ( !io )
  { dec_fail(d, SDL_GetError());
    return 0;
  }

  Sound_Sample *smp = Sound_NewSample(io, NULL, NULL, 16384);
  if ( !smp )				/* disposed of `io` */
  { if ( !d->stop )
      dec_fail(d, Sound_GetError());
    return 0;
  }

  SDL_SetAudioStreamFormat(a->stream, &smp->actual, NULL);
  SDL_LockMutex(d->lock);
  d->actual = smp->actual;
  d->ready  = true;
  SDL_UnlockMutex(d->lock);

  int frame = SDL_AUDIO_FRAMESIZE(smp->actual);
  frame_carry carry = {0};

  for(;;)
  { SDL_LockMutex(d->lock);		/* do not run too far ahead */
    while( !d->stop && queued_seconds(a) > DECODE_AHEAD )
      SDL_WaitConditionTimeout(d->cond, d->lock, 20);
    bool stop = d->stop;
    SDL_UnlockMutex(d->lock);
    if ( stop )
      break;

    Uint32 n = Sound_Decode(smp);

    if ( n > 0 )
      put_decoded(a, &carry, frame, smp->buffer, n);
    if ( (smp->flags & SOUND_SAMPLEFLAG_ERROR) )
    { if ( !d->stop )
	dec_fail(d, Sound_GetError());
      break;
    }
    if ( (smp->flags & SOUND_SAMPLEFLAG_EOF) )
    { SDL_FlushAudioStream(a->stream);
      SDL_SetAtomicInt(&d->state, DEC_DONE);
      break;
    }
  }

  Sound_FreeSample(smp);		/* also closes `io` */

  return 0;
}


static bool
start_decoder(astream *a)
{ decoder *d = &a->dec;

  if ( d->thread )
    return true;
  if ( !init_sound_decoders() )
  { dec_fail(d, Sound_GetError());
    return false;
  }
  if ( !d->lock )
  { d->lock = SDL_CreateMutex();
    d->cond = SDL_CreateCondition();
  }
  d->start = d->pos = 0;
  d->len   = 0;
  d->eof = d->stop = d->ready = false;
  SDL_SetAtomicInt(&d->state, DEC_RUNNING);
  if ( !(d->thread = SDL_CreateThread(dec_thread, "xpce-decoder", a)) )
  { dec_fail(d, SDL_GetError());
    return false;
  }

  return true;
}


/* Stop the decoder thread and discard the data that was written */

static void
stop_decoder(astream *a)
{ decoder *d = &a->dec;

  if ( d->thread )
  { SDL_LockMutex(d->lock);
    d->stop = true;
    SDL_BroadcastCondition(d->cond);
    SDL_UnlockMutex(d->lock);
    SDL_WaitThread(d->thread, NULL);
    d->thread = NULL;
  }
  d->len = 0;
  d->start = d->pos = 0;
  SDL_SetAtomicInt(&d->state, DEC_NONE);
}


static void
free_decoder(astream *a)
{ decoder *d = &a->dec;

  stop_decoder(a);
  if ( d->lock )
  { SDL_DestroyCondition(d->cond);
    SDL_DestroyMutex(d->lock);
    d->lock = NULL;
    d->cond = NULL;
  }
  SDL_free(d->buf);
  d->buf = NULL;
  d->allocated = 0;
}


/* Bytes written that the decoder did not read yet, or 0 if it does not
 * read any more.
 */

static Sint64
dec_unread(astream *a)
{ decoder *d = &a->dec;
  Sint64 n = 0;

  if ( d->thread && SDL_GetAtomicInt(&d->state) == DEC_RUNNING )
  { SDL_LockMutex(d->lock);
    n = d->start + (Sint64)d->len - d->pos;
    SDL_UnlockMutex(d->lock);
  }

  return n;
}


/* Add data for the decoder */

static bool
dec_write(astream *a, const Uint8 *bytes, size_t len)
{ decoder *d = &a->dec;

  if ( !start_decoder(a) )
    return false;

  SDL_LockMutex(d->lock);
  if ( d->len + len > d->allocated )
  { size_t size = d->allocated ? d->allocated : 65536;

    while( size < d->len + len )
      size *= 2;
    Uint8 *nbuf = SDL_realloc(d->buf, size);
    if ( !nbuf )
    { SDL_UnlockMutex(d->lock);
      return SDL_OutOfMemory();
    }
    d->buf = nbuf;
    d->allocated = size;
  }
  memcpy(d->buf+d->len, bytes, len);
  d->len += len;
  SDL_BroadcastCondition(d->cond);
  SDL_UnlockMutex(d->lock);

  return true;
}


static void
dec_end(astream *a)
{ decoder *d = &a->dec;

  if ( d->lock )
  { SDL_LockMutex(d->lock);
    d->eof = true;
    SDL_BroadcastCondition(d->cond);
    SDL_UnlockMutex(d->lock);
  }
}

#else /*HAVE_SDL3_SOUND*/

static bool
dec_write(astream *a, const Uint8 *bytes, size_t len)
{ return SDL_SetError("encoded data requires SDL3_sound");
}

static Sint64 dec_unread(astream *a) { return 0; }

static void stop_decoder(astream *a) { }
static void free_decoder(astream *a) { }
static void dec_end(astream *a) { }

#endif /*HAVE_SDL3_SOUND*/


/* The SDL data of `as`.  It lives as long as `as`.  The device stream
 * is opened by open_device() on the first write or ->play and closed
 * when the stream finished playing or is stopped.
 */

static astream *
astream_data(AudioStream as)
{ astream *a = as->ws_ref;

  if ( !a )
  { a = SDL_calloc(1, sizeof(*a));
    a->as      = as;
    a->encoded = (as->format == NAME_encoded);
    a->next    = astreams;
    astreams   = a;
    as->ws_ref = a;
  }

  return a;
}


/* Open the device stream of `a`, paused.  The stream of encoded data
 * takes the format the device prefers until the decoder sets the format
 * of the data.
 */

static bool
open_device(astream *a)
{ AudioStream as = a->as;
  SDL_AudioSpec spec;

  if ( a->stream )
    return true;
  if ( !init_audio() )
    return false;

  if ( a->encoded )
  { if ( !SDL_GetAudioDeviceFormat(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
				   &spec, NULL) )
      spec = (SDL_AudioSpec){ SDL_AUDIO_F32, 2, 48000 };
  } else
    spec = (SDL_AudioSpec){ audio_format(as->format),
			    valInt(as->channels), valInt(as->frequency) };

  SDL_AudioStream *stream =
    SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
			      &spec, NULL, NULL);
  if ( !stream )
  { errorPce(as, NAME_cannotPlay, CtoName(SDL_GetError()));
    return false;
  }
  SDL_SetAudioStreamGain(stream, (float)valNum(as->volume));
  a->stream   = stream;
  a->spec     = spec;
  a->ended_at = 0;

  return true;
}


/* Close the device stream and discard all data.  For encoded data, the
 * next write starts a new file.
 */

static void
close_device(astream *a)
{ free_decoder(a);
  if ( a->stream )
  { SDL_DestroyAudioStream(a->stream);
    a->stream = NULL;
  }
  a->partial_len = 0;
  a->closed = false;
  a->low    = false;
}


/* Get the bytes of `data`.  Data written using pce_open/3 on an
 * audio_stream is an 8-bit string.  A wide string must hold bytes.
 */

static const Uint8 *
data_bytes(CharArray data, size_t *len, Uint8 **tmp)
{ PceString str = &data->data;

  *len = str->s_size;
  *tmp = NULL;
  if ( isstrA(str) )
    return str->s_textA;

  Uint8 *buf = SDL_malloc(str->s_size ? str->s_size : 1);
  for(size_t i=0; i<str->s_size; i++)
  { int c = str_fetch(str, (int)i);

    if ( c > 0xff )
    { SDL_free(buf);
      return NULL;
    }
    buf[i] = (Uint8)c;
  }

  return *tmp = buf;
}


/* Put `len` bytes into the device stream.  SDL only accepts whole
 * sample frames, while data may be written in arbitrary pieces, so we
 * keep an incomplete frame for the next write.
 */

static bool
put_frames(astream *a, const Uint8 *bytes, size_t len)
{ int frame = SDL_AUDIO_FRAMESIZE(a->spec);

  if ( a->partial_len > 0 )
  { size_t n = frame - a->partial_len;

    if ( n > len )
      n = len;
    memcpy(a->partial+a->partial_len, bytes, n);
    a->partial_len += (int)n;
    bytes += n;
    len -= n;
    if ( a->partial_len < frame )
      return true;
    a->partial_len = 0;
    if ( !SDL_PutAudioStreamData(a->stream, a->partial, frame) )
      return false;
  }

  size_t whole = len - len%frame;

  if ( whole > 0 && !SDL_PutAudioStreamData(a->stream, bytes, (int)whole) )
    return false;
  memcpy(a->partial, bytes+whole, len-whole);
  a->partial_len = (int)(len-whole);

  return true;
}


/* True if a write must wait: the stream is playing and far ahead */

static bool
must_wait(astream *a)
{ AudioStream as = a->as;

  if ( as->playing != ON || !a->stream || a->stopped )
    return false;
  if ( a->encoded )
    return dec_unread(a) > ENCODED_AHEAD;

  double ahead = 2*valNum(as->low_water);

  if ( ahead < WRITE_AHEAD )
    ahead = WRITE_AHEAD;

  return queued_seconds(a) > ahead;
}


/* Wait until we may write.  We release the xpce lock while waiting, so
 * the GUI and the tick continue while a producer thread waits.  They
 * may stop the stream or close its device meanwhile.  The caller holds
 * a code reference to `as`, so `a` stays.
 */

static void
wait_for_room(astream *a)
{ while( must_wait(a) )
  { int locks = pceMTUnlockAll();

    SDL_Delay(10);
    pceMTRelock(locks);
  }
}


status
ws_write_audio_stream(AudioStream as, CharArray data)
{ astream *a = astream_data(as);
  size_t len;
  Uint8 *tmp;
  const Uint8 *bytes;
  bool rc;

  if ( a->stopped )			/* silent: stopping is normal */
    fail;
  if ( a->encoded && a->closed )
    return errorPce(as, NAME_cannotPlay, CtoName("the data has ended"));
  if ( !(bytes = data_bytes(data, &len, &tmp)) )
    return errorPce(as, NAME_cannotPlay, CtoName("data is not binary"));

  addCodeReference(as);
  wait_for_room(a);
  if ( a->stopped )
  { SDL_free(tmp);
    delCodeReference(as);
    fail;
  } else if ( !open_device(a) )
    rc = SDL_SetError("cannot open the audio device");
  else
  { a->low = false;			/* new data answers low_water_message */
    if ( a->encoded )
    { rc = dec_write(a, bytes, len);
    } else
    { a->closed = false;
      rc = put_frames(a, bytes, len);
    }
  }
  SDL_free(tmp);
  if ( !rc )
    errorPce(as, NAME_cannotPlay, CtoName(SDL_GetError()));
  delCodeReference(as);

  return rc ? SUCCEED : FAIL;
}


void
ws_close_audio_stream(AudioStream as)
{ astream *a = as->ws_ref;

  if ( a && a->stream )
  { a->closed = true;
    if ( a->encoded )
      dec_end(a);
    else
      SDL_FlushAudioStream(a->stream);
  }
}


/* Discard all data, as for a new file.  Sent by pce_open/3 for mode
 * `write`.
 */

void
ws_clear_audio_stream(AudioStream as)
{ astream *a = as->ws_ref;

  if ( a )
  { a->stopped = false;
    if ( a->encoded )
      stop_decoder(a);
    if ( a->stream )
      SDL_ClearAudioStream(a->stream);
    a->partial_len = 0;
    a->closed = false;
    a->low    = false;
  }
}


status
ws_play_audio_stream(AudioStream as)
{ astream *a = astream_data(as);

  a->stopped = false;
  if ( !open_device(a) )
    fail;
  SDL_ResumeAudioStreamDevice(a->stream);
  assign(as, playing, ON);
  a->low = false;
  if ( !a->referenced )
  { addCodeReference(as);
    a->referenced = true;
  }
  sdl_start_media_tick();

  succeed;
}


/* No longer playing.  We close the device, as a sound does after it is
 * played.  A new write or ->play opens it again.  May free `as` if we
 * held the last reference.
 */

static void
end_astream(AudioStream as, astream *a)
{ bool referenced = a->referenced;

  close_device(a);
  a->referenced = false;
  assign(as, playing, OFF);
  if ( referenced )
  { delCodeReference(as);
    freeableObj(as);
  }
}


/* Stop playing and discard the data.  Writes fail until ->play or
 * ->truncate_as_file, so a producer such as copy_stream_data/2 ends.
 */

void
ws_stop_audio_stream(AudioStream as)
{ astream *a = astream_data(as);

  a->stopped = true;
  end_astream(as, a);
}


void
ws_volume_audio_stream(AudioStream as)
{ astream *a = as->ws_ref;

  if ( a && a->stream )
    SDL_SetAudioStreamGain(a->stream, (float)valNum(as->volume));
}


double
ws_queued_audio_stream(AudioStream as)
{ astream *a = as->ws_ref;

  return a ? queued_seconds(a) : 0.0;
}


void
ws_free_audio_stream(AudioStream as)
{ astream *a = as->ws_ref;

  if ( a )
  { for(astream **ap = &astreams; *ap; ap = &(*ap)->next)
    { if ( *ap == a )
      { *ap = a->next;
	break;
      }
    }
    as->ws_ref = NULL;
    close_device(a);
    if ( a->referenced )
      delCodeReference(as);
    SDL_free(a);
  }
}


/* Milliseconds of audio the device buffers.  When the stream is
 * empty, the device still has to play this.
 */

static Uint64
device_latency_ms(astream *a)
{ SDL_AudioSpec spec;
  int frames;

  if ( SDL_GetAudioDeviceFormat(SDL_GetAudioStreamDevice(a->stream),
				&spec, &frames) &&
       spec.freq > 0 && frames > 0 )
    return (Uint64)frames*1000/spec.freq;

  return 50;
}


/* The data has ended and is played.  For encoded data, it has ended
 * when it is all decoded.  After the stream is empty, we wait for the
 * device to play what it buffered: closing it discards that.
 */

static bool
astream_finished(astream *a)
{ if ( a->encoded )
  { if ( SDL_GetAtomicInt(&a->dec.state) != DEC_DONE )
      return false;
  } else if ( !a->closed )
    return false;

  if ( SDL_GetAudioStreamQueued(a->stream) > 0 )
  { a->ended_at = 0;
    return false;
  }

  Uint64 now = SDL_GetTicks();

  if ( !a->ended_at )
    a->ended_at = now;

  return now >= a->ended_at + 2*device_latency_ms(a);
}


/* Once the decoder knows the format of the data, set <-frequency and
 * <-channels.
 */

static void
update_encoded_format(astream *a)
{ AudioStream as = a->as;
  SDL_AudioSpec src;
  bool ready;

  if ( !a->dec.lock )
    return;
  SDL_LockMutex(a->dec.lock);
  ready = a->dec.ready;
  src   = a->dec.actual;
  SDL_UnlockMutex(a->dec.lock);

  if ( ready )
  { if ( valInt(as->frequency) != src.freq )
      assign(as, frequency, toInt(src.freq));
    if ( valInt(as->channels) != src.channels )
      assign(as, channels, toInt(src.channels));
  }
}


/* Called from the tick for a playing stream.  Reports decoder errors,
 * sends low_water_message if the queue is below <-low_water while the
 * data has not ended, once until data is written or the queue was
 * above <-low_water, and <-message after the data ended and was
 * played.  Returns false if the stream is no longer
 * playing.
 */

static bool
tick_astream(astream *a)
{ AudioStream as = a->as;

  if ( as->playing != ON || !a->stream )
    return false;

  if ( a->encoded && SDL_GetAtomicInt(&a->dec.state) == DEC_ERROR )
  { char msg[sizeof(a->dec.errmsg)];

    SDL_strlcpy(msg, a->dec.errmsg, sizeof(msg));
    addCodeReference(as);
    ws_stop_audio_stream(as);
    errorPce(as, NAME_cannotPlay, CtoName(msg));
    delCodeReference(as);
    freeableObj(as);
    return false;
  }

  if ( a->encoded )
    update_encoded_format(a);

  if ( astream_finished(a) )
  { addCodeReference(as);
    assign(as, playing, OFF);
    if ( notNil(as->message) )
      forwardReceiverCode(as->message, as, as, EAV);
    if ( as->ws_ref == a )
      end_astream(as, a);
    delCodeReference(as);
    freeableObj(as);
    return false;
  }

  if ( !a->closed )
  { if ( queued_seconds(a) < valNum(as->low_water) )
    { if ( !a->low )
      { a->low = true;
	if ( notNil(as->low_water_message) )
	  forwardReceiverCode(as->low_water_message, as, as, EAV);
      }
    } else
      a->low = false;
  }

  return true;
}


		 /*******************************
		 *	      THE TICK		*
		 *******************************/

bool
sdl_audio_event(SDL_Event *event)
{ if ( event->type != MY_EVENT_MEDIA )
    return false;

  pceMTLock();
  for(play **pp = &plays; *pp; )
  { if ( SDL_GetAudioStreamQueued((*pp)->stream) == 0 )
      end_play(pp, true);		/* may run code that adds plays */
    else
      pp = &(*pp)->next;
  }

  for(recorder *rec = recorders; rec; )
  { recorder *next = rec->next;	/* level message may stop it */
    AudioRecorder r = rec->recorder;
    double level = pull_recorded(rec);

    if ( level >= 0.0 && notNil(r->level_message) )
      forwardReceiverCode(r->level_message, r, r, toNum(level), EAV);
    rec = next;
  }

  bool streaming = false;
  for(astream *a = astreams; a; )
  { astream *next = a->next;		/* messages may free `a` */

    if ( tick_astream(a) )
      streaming = true;
    a = next;
  }

  bool cameras = sdl_camera_tick();

  if ( !plays && !recorders && !streaming && !cameras )
    SDL_SetAtomicInt(&ticking, 0);
  pceMTUnlock();

  return true;
}
