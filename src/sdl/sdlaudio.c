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
} recorder;

static play	*plays = NULL;
static recorder *recorders = NULL;
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

status
ws_load_sound(Sound s, FileObj file)
{ char *fn = charArrayToFN((CharArray)getOsNameFile(file));
  SDL_AudioSpec spec;
  Uint8 *data;
  Uint32 len;

  if ( !SDL_LoadWAV(fn, &spec, &data, &len) )
    return errorPce(s, NAME_cannotLoad, file, CtoName(SDL_GetError()));

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

/* Write a canonical 44-byte header WAV file.  WAV is little endian; we
 * assume the samples are as well.
 */

status
ws_save_sound(Sound s, FileObj file)
{ ws_sound *wss = s->ws_ref;

  if ( !wss || wss->spec.freq == 0 )
    return errorPce(s, NAME_noData);

  char *fn = charArrayToFN((CharArray)getOsNameFile(file));
  SDL_IOStream *out = SDL_IOFromFile(fn, "wb");
  if ( !out )
    return errorPce(s, NAME_cannotSave, file, CtoName(SDL_GetError()));

  int bits     = SDL_AUDIO_BITSIZE(wss->spec.format);
  int channels = wss->spec.channels;
  int rate     = wss->spec.freq;
  Uint8 hdr[44];

  memcpy(hdr, "RIFF", 4);    put_le(hdr+4, 36+wss->len, 4);
  memcpy(hdr+8, "WAVEfmt ", 8);
  put_le(hdr+16, 16, 4);			/* fmt chunk size */
  put_le(hdr+20, SDL_AUDIO_ISFLOAT(wss->spec.format) ? 3 : 1, 2);
  put_le(hdr+22, channels, 2);
  put_le(hdr+24, rate, 4);
  put_le(hdr+28, rate*channels*bits/8, 4);	/* bytes per second */
  put_le(hdr+32, channels*bits/8, 2);	/* block align */
  put_le(hdr+34, bits, 2);
  memcpy(hdr+36, "data", 4); put_le(hdr+40, wss->len, 4);

  bool rc = ( SDL_WriteIO(out, hdr, sizeof(hdr)) == sizeof(hdr) &&
	      SDL_WriteIO(out, wss->data, wss->len) == wss->len );
  if ( !SDL_CloseIO(out) )
    rc = false;
  if ( !rc )
    return errorPce(s, NAME_cannotSave, file, CtoName(SDL_GetError()));

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


		 /*******************************
		 *	     RECORDING		*
		 *******************************/

static const SDL_AudioSpec record_spec = { SDL_AUDIO_S16, 1, 44100 };

status
ws_start_recording(AudioRecorder r)
{ if ( !init_audio() )
    fail;

  SDL_AudioStream *stream =
    SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_RECORDING,
			      &record_spec, NULL, NULL);
  if ( !stream )
    return errorPce(r, NAME_cannotRecord, CtoName(SDL_GetError()));

  Sound s = r->sound;
  ws_stop_sound(s);
  ws_sound *wss = sound_data(s, true);
  free_sound_data(wss);
  wss->spec = record_spec;
  update_sound(s);
  SDL_ResumeAudioStreamDevice(stream);

  recorder *rec = SDL_calloc(1, sizeof(*rec));
  rec->recorder = r;
  rec->stream   = stream;
  rec->next     = recorders;
  recorders     = rec;
  r->ws_ref     = rec;
  addCodeReference(r);
  sdl_start_media_tick();

  succeed;
}


/* Move the audio recorded by `rec` into the sound of its recorder and
 * return the RMS level (0..1) of the new audio, or -1 if there is none.
 */

static double
pull_recorded(recorder *rec)
{ Sound s = rec->recorder->sound;
  ws_sound *wss = sound_data(s, true);
  int avail = SDL_GetAudioStreamAvailable(rec->stream);

  if ( avail <= 0 )
    return -1.0;

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

  Sint16 *samples = (Sint16*)(wss->data+wss->len);
  int n = got/(int)sizeof(Sint16);
  double sum = 0.0;
  for(int i=0; i<n; i++)
  { double v = samples[i]/32768.0;
    sum += v*v;
  }
  wss->len += got;
  update_sound(s);

  return n > 0 ? sqrt(sum/n) : 0.0;
}


void
ws_stop_recording(AudioRecorder r)
{ recorder *rec = r->ws_ref;

  if ( !rec )
    return;

  pull_recorded(rec);
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

  bool cameras = sdl_camera_tick();

  if ( !plays && !recorders && !cameras )
    SDL_SetAtomicInt(&ticking, 0);
  pceMTUnlock();

  return true;
}
