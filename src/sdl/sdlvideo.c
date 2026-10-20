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
#include <math.h>
#include "sdlvideo.h"
#ifdef HAVE_VIDEO
#include "video_backend.h"
#endif

/* Playing and writing encoded video.  The backend (video_backend.h)
 * decodes and encodes; this file does the threads, the queues and the
 * timing, which are the same for all backends.
 *
 * Playing: a reader thread decodes frames ahead into a small queue and
 * puts the audio into an SDL audio stream of the playback device.  The
 * media tick (see sdlaudio.c) shows the newest frame whose time has
 * come by swapping it into the sdl_texture of the video.  The surface
 * that was shown goes back to the reader, which decodes the next frame
 * into it, so we do not allocate while playing.  The time comes from
 * the audio: the time of the last sample we gave the device minus what
 * it has not yet played.  Without audio, or if the audio runs dry, it
 * comes from the system clock.  Seeking flushes the queues and makes
 * the reader skip the frames and samples before the target.  The
 * worker threads never touch xpce objects.
 *
 * Writing: ->append converts a frame to the pixel format of the
 * encoder and queues it for a writer thread, so encoding does not block
 * the GUI unless the encoder falls behind.  ->append_audio converts
 * the samples to the format of the encoder and queues them as well.
 */

#ifdef HAVE_VIDEO

#define QUEUE_MIN   4			/* decoded frames ahead */
#define QUEUE_SIZE  16			/* at most, waiting for audio */
#define QUEUE_BYTES (64<<20)		/* at most in all frames */
#define POOL_SIZE   (QUEUE_SIZE+2)	/* recycled surfaces */
#define AUDIO_AHEAD 0.25		/* seconds of audio decoded ahead */
#define LIVE_AUDIO_MAX 0.5		/* live: drop audio beyond this */
#define WQUEUE_SIZE 32			/* frames and audio to encode */
#define INPUT_KEEP  (1024*1024)		/* bytes of input kept for seeking */

typedef struct qframe
{ SDL_Surface  *surface;		/* a frame or NULL */
  double	pts;			/* seconds */
  float	       *samples;		/* if not a frame: audio */
  int		nsamples;		/* sample frames in samples */
} qframe;

typedef struct surface_pool
{ SDL_Surface  *surfaces[POOL_SIZE];
  int		count;
} surface_pool;

static SDL_Surface *
take_pool(surface_pool *p)
{ return p->count > 0 ? p->surfaces[--p->count] : NULL;
}

static void
put_pool(surface_pool *p, SDL_Surface *s)
{ if ( s )
  { if ( p->count < POOL_SIZE )
      p->surfaces[p->count++] = s;
    else
      SDL_DestroySurface(s);
  }
}

static void
free_pool(surface_pool *p)
{ while(p->count > 0)
    SDL_DestroySurface(p->surfaces[--p->count]);
}


		 /*******************************
		 *	      PLAYING		*
		 *******************************/

typedef struct ws_video
{ struct ws_video *next;		/* list of open videos */
  Video		video;			/* the xpce object */
  vb_reader    *reader;
  vb_info	info;
  char	       *source;			/* file or URL */
  char	      **options;		/* name, value, ..., NULL */
  bool		want_audio;		/* play the audio if there is any */
  double	volume;			/* initial gain */
  SDL_AtomicInt interrupt;		/* abort blocking calls */
  struct video_input *input;		/* data written to the video */
  SDL_Thread   *thread;
  SDL_Mutex    *lock;			/* protects the fields below */
  SDL_Condition *cond;
  qframe	queue[QUEUE_SIZE];
  int		qhead;
  int		qcount;
  surface_pool	pool;
  bool		stop;			/* thread must stop */
  bool		opened;			/* reader opened the source */
  bool		open_failed;		/* it could not; see errmsg */
  bool		eof;			/* reader is at the end */
  bool		failed;			/* reader failed; see errmsg */
  bool		seek;			/* reader must seek to seek_to */
  double	seek_to;
  double	skip_before;		/* drop frames before this time */
  unsigned	generation;		/* incremented by each seek */
  int		queue_max;		/* frames we may queue */
  SDL_AudioStream *audio;		/* playback stream or NULL */
  double	audio_end;		/* time after the last sample */
  double	latency;		/* seconds the device buffers */
  char		errmsg[VB_ERRLEN];
					/* main thread only */
  bool		announced;		/* status tells it is open */
  bool		play_on_open;		/* ->play while opening */
  bool		playing;		/* clock is running */
  bool		show_next;		/* show the next frame now */
  bool		reported;		/* error was reported */
  Uint64	start_ns;		/* SDL_GetTicksNS() at start_pos */
  double	start_pos;		/* position when the clock started */
} ws_video;

static ws_video *videos = NULL;

/* The data written to a video whose <-source is @nil, e.g., by Prolog
 * using pce_open/3.  The reader reads it as a file: a read waits for
 * the data.  Once the video is open, the data more than INPUT_KEEP
 * bytes before the read position is dropped, so a short seek back,
 * which some formats need, still works.
 */

typedef struct video_input
{ SDL_Mutex    *lock;
  SDL_Condition *cond;
  Uint8	       *buf;
  size_t	len;			/* bytes in buf */
  size_t	allocated;
  Sint64	start;			/* position of buf[0] */
  Sint64	pos;			/* read position */
  bool		eof;			/* ->close_as_file */
  bool		stop;			/* the video is closed */
  bool		trim;			/* drop data we passed */
} video_input;

static video_input *
new_input(void)
{ video_input *in = SDL_calloc(1, sizeof(*in));

  if ( in )
  { in->lock = SDL_CreateMutex();
    in->cond = SDL_CreateCondition();
  }

  return in;
}

static void
free_input(video_input *in)
{ if ( in )
  { SDL_DestroyCondition(in->cond);
    SDL_DestroyMutex(in->lock);
    SDL_free(in->buf);
    SDL_free(in);
  }
}

static void
stop_input(video_input *in)
{ if ( in )
  { SDL_LockMutex(in->lock);
    in->stop = true;
    SDL_BroadcastCondition(in->cond);
    SDL_UnlockMutex(in->lock);
  }
}

static long
input_read(void *closure, void *ptr, size_t size)
{ video_input *in = closure;
  long n = 0;

  SDL_LockMutex(in->lock);
  while( !in->stop && !in->eof && in->pos >= in->start + (Sint64)in->len )
    SDL_WaitCondition(in->cond, in->lock);

  if ( in->stop )
  { n = -1;
  } else
  { size_t avail = (size_t)(in->start + (Sint64)in->len - in->pos);

    n = (long)(size < avail ? size : avail);	/* 0: end of the data */
    memcpy(ptr, in->buf + (in->pos - in->start), (size_t)n);
    in->pos += n;

    if ( in->trim )
    { size_t done = (size_t)(in->pos - in->start);

      if ( done > 2*INPUT_KEEP )
      { size_t drop = done - INPUT_KEEP;

	memmove(in->buf, in->buf+drop, in->len-drop);
	in->len   -= drop;
	in->start += drop;
      }
    }
  }
  SDL_UnlockMutex(in->lock);

  return n;
}

/* Seek within the data we have.  Seeking forward waits for the data */

static long long
input_seek(void *closure, long long offset, int whence)
{ video_input *in = closure;
  Sint64 to;

  SDL_LockMutex(in->lock);
  to = (whence == SEEK_CUR ? in->pos + offset : offset);
  while( !in->stop && !in->eof && to > in->start + (Sint64)in->len )
    SDL_WaitCondition(in->cond, in->lock);
  if ( in->stop || to < in->start || to > in->start + (Sint64)in->len )
    to = -1;
  else
    in->pos = to;
  SDL_UnlockMutex(in->lock);

  return to;
}

static bool
input_write(video_input *in, const Uint8 *data, size_t len)
{ bool ok = true;

  SDL_LockMutex(in->lock);
  if ( in->len + len > in->allocated )
  { size_t size = in->allocated ? in->allocated : 65536;

    while( size < in->len + len )
      size *= 2;
    Uint8 *buf = SDL_realloc(in->buf, size);
    if ( buf )
    { in->buf = buf;
      in->allocated = size;
    } else
      ok = false;
  }
  if ( ok )
  { memcpy(in->buf+in->len, data, len);
    in->len += len;
    SDL_BroadcastCondition(in->cond);
  }
  SDL_UnlockMutex(in->lock);

  return ok;
}

static void
flush_queue(ws_video *v)
{ while(v->qcount > 0)
  { put_pool(&v->pool, v->queue[v->qhead].surface);
    v->qhead = (v->qhead+1)%QUEUE_SIZE;
    v->qcount--;
  }
}

/* Seconds of audio the device has not yet played */

static double
audio_queued(ws_video *v)
{ int bytes = SDL_GetAudioStreamQueued(v->audio);
  int rate = v->info.audio_rate*v->info.audio_channels*(int)sizeof(float);

  return bytes > 0 && rate > 0 ? (double)bytes/rate : 0.0;
}

/* True if the reader has decoded enough.  It decodes at least
 * QUEUE_MIN frames and AUDIO_AHEAD seconds of audio ahead.  As the
 * audio of a frame may come later in the file, it may need more frames
 * to get the audio.
 */

static bool
reader_ahead(ws_video *v)
{ if ( v->info.live )			/* never block a live source */
    return false;
  if ( v->qcount >= v->queue_max )
    return true;
  if ( v->qcount < QUEUE_MIN )
    return false;

  return !v->audio || audio_queued(v) >= AUDIO_AHEAD;
}

/* Give the samples at pts to the device, dropping those before the
 * target of a seek
 */

static void
put_audio(ws_video *v, const vb_samples *s, double pts)
{ int ch = v->info.audio_channels;
  double rate = v->info.audio_rate;
  int skip = 0;

  if ( pts < 0 )			/* unknown */
    pts = v->audio_end;
  if ( pts < v->seek_to )
  { skip = (int)((v->seek_to-pts)*rate);
    if ( skip >= s->frames )
      return;
  }
  if ( v->info.live && audio_queued(v) > LIVE_AUDIO_MAX )
    SDL_ClearAudioStream(v->audio);	/* we fell behind: catch up */
  SDL_PutAudioStreamData(v->audio, s->data+(size_t)skip*ch,
			 (s->frames-skip)*ch*(int)sizeof(float));
  v->audio_end = pts + s->frames/rate;
}

static void open_audio(ws_video *v, double volume);

/* Open the source in the reader thread, as opening a network stream
 * may take a while
 */

static bool
open_reader(ws_video *v)
{ char errmsg[VB_ERRLEN];
  vb_source src = { .url       = v->source,
		    .options   = v->options,
		    .interrupt = &v->interrupt };

  if ( v->input )
  { src.read	= input_read;
    src.seek	= input_seek;
    src.closure = v->input;
  }
  vb_info info;
  vb_reader *r = vb_open_reader(&src, v->want_audio, &info, errmsg);

  if ( r )
  { size_t frame_bytes = (size_t)info.width*info.height*3/2;

    v->queue_max = frame_bytes ? (int)(QUEUE_BYTES/frame_bytes) : QUEUE_SIZE;
    if ( v->queue_max > QUEUE_SIZE )
      v->queue_max = QUEUE_SIZE;
    if ( v->queue_max < QUEUE_MIN )
      v->queue_max = QUEUE_MIN;
    v->info = info;
    if ( v->want_audio )
      open_audio(v, v->volume);
  }

  if ( r && v->input )			/* we will not seek back far */
  { SDL_LockMutex(v->input->lock);
    v->input->trim = true;
    SDL_UnlockMutex(v->input->lock);
  }

  SDL_LockMutex(v->lock);
  if ( r )
  { v->reader = r;
    v->opened = true;
  } else
  { strcpy(v->errmsg, errmsg);
    v->open_failed = true;
  }
  SDL_BroadcastCondition(v->cond);
  SDL_UnlockMutex(v->lock);

  return r != NULL;
}


static int SDLCALL
reader_thread(void *data)
{ ws_video *v = data;
  char errmsg[VB_ERRLEN];

  if ( !open_reader(v) )
    return 0;

  SDL_LockMutex(v->lock);
  for(;;)
  { while( !v->stop && !v->seek &&
	   (v->eof || v->failed || reader_ahead(v)) )
    { if ( v->audio && !v->eof && !v->failed )
	SDL_WaitConditionTimeout(v->cond, v->lock, 10); /* audio drains */
      else
	SDL_WaitCondition(v->cond, v->lock);
    }
    if ( v->stop )
      break;

    if ( v->seek )
    { double t = v->seek_to;

      v->seek = false;
      SDL_UnlockMutex(v->lock);
      bool ok = vb_seek(v->reader, t, errmsg);
      SDL_LockMutex(v->lock);
      if ( !ok )
      { strcpy(v->errmsg, errmsg);
	v->failed = true;
      }
      continue;
    }

    unsigned generation = v->generation;
    SDL_Surface *s = take_pool(&v->pool);
    vb_samples samples;
    double pts;

    SDL_UnlockMutex(v->lock);
    vb_status rc = vb_read(v->reader, &s, &samples, &pts, errmsg);
    SDL_LockMutex(v->lock);

    if ( generation != v->generation )	/* seek while decoding */
    { put_pool(&v->pool, s);
      continue;
    }
    switch(rc)
    { case VB_FRAME:
	if ( pts < v->skip_before )
	{ put_pool(&v->pool, s);
	} else
	{ if ( v->qcount >= v->queue_max )	/* live: drop the oldest */
	  { put_pool(&v->pool, v->queue[v->qhead].surface);
	    v->qhead = (v->qhead+1)%QUEUE_SIZE;
	    v->qcount--;
	  }

	  int i = (v->qhead+v->qcount)%QUEUE_SIZE;

	  v->queue[i].surface = s;
	  v->queue[i].pts = pts;
	  v->qcount++;
	}
	break;
      case VB_AUDIO:
	put_pool(&v->pool, s);
	if ( v->audio )
	  put_audio(v, &samples, pts);
	break;
      case VB_EOF:
	put_pool(&v->pool, s);
	v->eof = true;
	break;
      case VB_ERROR:
	put_pool(&v->pool, s);
	strcpy(v->errmsg, errmsg);
	v->failed = true;
	break;
    }
  }
  SDL_UnlockMutex(v->lock);

  return 0;
}


/* The time in the video.  While the device plays our audio, this is
 * the time of the sample being played: the time after the last sample
 * we gave it minus the samples it did not take and the samples it
 * buffers.  We remember it with the time
 * of the system clock, so we can continue from there if the audio runs
 * dry.  Must be called with v->lock held.
 */

static double
clock_position(ws_video *v)
{ if ( !v->playing )
    return v->start_pos;

  Uint64 now = SDL_GetTicksNS();
  double queued;

  if ( v->audio && (queued = audio_queued(v)) > 0.0 )
  { v->start_pos = v->audio_end - queued - v->latency;
    v->start_ns  = now;
    return v->start_pos;
  }

  return v->start_pos + (double)(now-v->start_ns)/1e9;
}

static double
video_position(ws_video *v)
{ SDL_LockMutex(v->lock);
  double pos = clock_position(v);
  SDL_UnlockMutex(v->lock);

  return pos;
}


/* Open the playback device for the audio of v.  Without a device, the
 * video plays without sound.
 */

static void
open_audio(ws_video *v, double volume)
{ SDL_AudioSpec spec = { SDL_AUDIO_F32, v->info.audio_channels,
			 v->info.audio_rate };

  if ( v->info.audio_rate > 0 && ws_has_audio() &&
       (v->audio = SDL_OpenAudioDeviceStream(
		     SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, &spec, NULL, NULL)) )
  { SDL_AudioSpec dev;
    int frames;

    SDL_SetAudioStreamGain(v->audio, (float)volume);
    if ( SDL_GetAudioDeviceFormat(SDL_GetAudioStreamDevice(v->audio),
				  &dev, &frames) &&
	 dev.freq > 0 && frames > 0 )
      v->latency = (double)frames/dev.freq;
  }
}


static char *
source_name(Any source)
{ if ( instanceOfObject(source, ClassFile) )
    return charArrayToFN((CharArray)getOsNameFile(source));

  return charArrayToUTF8(source);
}


static bool
is_local(const char *source)
{ return source && !strstr(source, "://");
}

/* The options of the video as name, value, ..., NULL */

static char **
copy_options(Video vid)
{ if ( isNil(vid->options) )
    return NULL;

  int n = valInt(vid->options->attributes->size);
  char **o = SDL_calloc(2*n+1, sizeof(char*));
  int i = 0;
  Cell cell;

  if ( !o )
    return NULL;
  for_cell(cell, vid->options->attributes)
  { Attribute a = cell->value;
    CharArray value = (isInteger(a->value)
		       ? (CharArray)getv(a->value, NAME_printName, 0, NULL)
		       : (CharArray)a->value);

    o[i++] = SDL_strdup(charArrayToUTF8(a->name));
    o[i++] = SDL_strdup(value ? charArrayToUTF8(value) : "");
  }

  return o;
}

static void
free_options(char **o)
{ for(char **p = o; p && *p; p++)
    SDL_free(*p);
  SDL_free(o);
}


/* Open the video.  The reader thread opens the source.  We wait for a
 * local file, so its size, etc. are known when we return.  A network
 * stream is <-status `opening` until sdl_video_tick() sees it is open.
 */

status
ws_open_video(Video vid)
{ if ( vid->ws )
    succeed;

  ws_video *v = SDL_calloc(1, sizeof(*v));

  if ( !v )
    return errorPce(vid, NAME_notEnoughMemory);
  v->video	 = vid;
  if ( isNil(vid->source) )
  { if ( !(v->input = new_input()) )
    { SDL_free(v);
      return errorPce(vid, NAME_notEnoughMemory);
    }
  } else
    v->source	 = SDL_strdup(source_name(vid->source));
  v->options	 = copy_options(vid);
  v->want_audio	 = vid->audio == ON && ws_has_audio();
  v->volume	 = valNum(vid->volume);
  v->lock	 = SDL_CreateMutex();
  v->cond	 = SDL_CreateCondition();
  v->show_next	 = true;		/* show the first frame */
  v->skip_before = -HUGE_VAL;
  if ( !(v->thread = SDL_CreateThread(reader_thread, "xpce-video", v)) )
  { free_options(v->options);
    free_input(v->input);
    SDL_free(v->source);
    SDL_DestroyCondition(v->cond);
    SDL_DestroyMutex(v->lock);
    SDL_free(v);
    return errorPce(vid, NAME_cannotOpen, CtoName(SDL_GetError()));
  }
  v->next = videos;
  videos  = v;
  vid->ws = v;
  sdl_add_media_source(sdl_video_tick);
  sdl_start_media_tick();

  if ( is_local(v->source) )
  { char errmsg[VB_ERRLEN];
    bool failed;

    SDL_LockMutex(v->lock);
    while( !v->opened && !v->open_failed )
      SDL_WaitCondition(v->cond, v->lock);
    failed = v->open_failed;
    strcpy(errmsg, v->errmsg);
    SDL_UnlockMutex(v->lock);

    if ( failed )
    { ws_close_video(vid);
      return errorPce(vid, NAME_cannotOpen, CtoName(errmsg));
    }
    v->announced = true;
  }

  succeed;
}


/* Add data written to the video, or tell it ended (data is NULL) */

status
ws_write_video(Video vid, CharArray data)
{ ws_video *v = vid->ws;
  video_input *in = v ? v->input : NULL;

  if ( !in )
    return errorPce(vid, NAME_notOpen);

  if ( data )
  { const Uint8 *bytes;
    Uint8 *tmp;
    size_t len;

    if ( !(bytes = sdl_data_bytes(data, &len, &tmp)) )
      return errorPce(vid, NAME_ioError, CtoName("data is not binary"));
    bool ok = input_write(in, bytes, len);
    SDL_free(tmp);
    if ( !ok )
      return errorPce(vid, NAME_notEnoughMemory);
  } else
  { SDL_LockMutex(in->lock);
    in->eof = true;
    SDL_BroadcastCondition(in->cond);
    SDL_UnlockMutex(in->lock);
  }

  succeed;
}


/* True if the reader is still opening the source */

bool
ws_video_opening(Video vid)
{ ws_video *v = vid->ws;

  return v && !v->announced;
}


void
ws_close_video(Video vid)
{ ws_video *v = vid->ws;

  if ( !v )
    return;

  for(ws_video **pp = &videos; *pp; pp = &(*pp)->next)
  { if ( *pp == v )
    { *pp = v->next;
      break;
    }
  }
  SDL_LockMutex(v->lock);
  v->stop = true;
  SDL_SetAtomicInt(&v->interrupt, 1);	/* stop network I/O */
  SDL_SignalCondition(v->cond);
  SDL_UnlockMutex(v->lock);
  stop_input(v->input);
  SDL_WaitThread(v->thread, NULL);
  free_input(v->input);

  flush_queue(v);
  free_pool(&v->pool);
  if ( v->audio )
    SDL_DestroyAudioStream(v->audio);
  if ( v->reader )
    vb_close_reader(v->reader);
  free_options(v->options);
  SDL_free(v->source);
  SDL_DestroyCondition(v->cond);
  SDL_DestroyMutex(v->lock);
  SDL_free(v);
  vid->ws = NULL;
}


void
ws_play_video(Video vid)
{ ws_video *v = vid->ws;

  if ( v && !v->announced )
  { v->play_on_open = true;
    return;
  }
  if ( v && !v->playing )
  { SDL_LockMutex(v->lock);
    v->start_ns = SDL_GetTicksNS();
    v->playing = true;
    SDL_UnlockMutex(v->lock);
    if ( v->audio )
      SDL_ResumeAudioStreamDevice(v->audio);
    sdl_start_media_tick();
  }
}


void
ws_pause_video(Video vid)
{ ws_video *v = vid->ws;

  if ( v )
    v->play_on_open = false;
  if ( v && v->playing )
  { if ( v->audio )
      SDL_PauseAudioStreamDevice(v->audio);
    SDL_LockMutex(v->lock);
    v->start_pos = clock_position(v);
    v->playing = false;
    SDL_UnlockMutex(v->lock);
  }
}


void
ws_volume_video(Video vid)
{ ws_video *v = vid->ws;

  if ( v && v->audio )
    SDL_SetAudioStreamGain(v->audio, (float)valNum(vid->volume));
}


/* Seek to `t`.  The reader seeks to a key frame before t and drops the
 * frames until the one that shows at t.  The next tick shows it.
 */

void
ws_seek_video(Video vid, double t)
{ ws_video *v = vid->ws;

  if ( !v )
    return;

  double half = v->info.frame_rate > 0 ? 0.5/v->info.frame_rate : 0.001;

  if ( t < 0 )
    t = 0;
  SDL_LockMutex(v->lock);
  flush_queue(v);
  if ( v->audio )
    SDL_ClearAudioStream(v->audio);
  v->seek	 = true;
  v->seek_to	 = t;
  v->skip_before = t - half;
  v->audio_end	 = t;
  v->generation++;
  v->eof	 = false;
  v->failed	 = false;
  v->start_pos	 = t;
  v->start_ns	 = SDL_GetTicksNS();
  SDL_SignalCondition(v->cond);
  SDL_UnlockMutex(v->lock);

  v->show_next = true;
  v->reported  = false;
  sdl_start_media_tick();
}


double
ws_video_position(Video vid)
{ ws_video *v = vid->ws;

  return v ? video_position(v) : 0.0;
}


bool
ws_video_info(Video vid, video_info *info)
{ ws_video *v = vid->ws;

  if ( !v || !v->announced )
    return false;
  info->live	       = v->info.live;
  info->width	       = v->info.width;
  info->height	       = v->info.height;
  info->frame_rate     = v->info.frame_rate;
  info->duration       = v->info.duration;
  info->codec	       = v->info.codec;
  info->audio_codec    = v->info.audio_codec;
  info->audio_rate     = v->info.audio_rate;
  info->audio_channels = v->info.audio_channels;
  info->playing_audio  = v->audio != NULL;

  return true;
}


/* The frame at `at` as an image, e.g., for a thumbnail.  This uses a
 * reader of its own, so it does not disturb playing.
 */

Image
ws_video_frame_image(Video vid, double at)
{ char errmsg[VB_ERRLEN];
  vb_info info;
  if ( isNil(vid->source) )
  { errorPce(vid, NAME_cannotOpen, CtoName("no thumbnails of written data"));
    fail;
  }

  vb_source src = { .url = source_name(vid->source) };
  vb_reader *r = vb_open_reader(&src, false, &info, errmsg);
  SDL_Surface *frame = NULL, *shown = NULL;
  Image image = NULL;

  if ( !r )
  { errorPce(vid, NAME_cannotOpen, CtoName(errmsg));
    fail;
  }

  double half = info.frame_rate > 0 ? 0.5/info.frame_rate : 0.001;
  vb_samples samples;
  double pts;
  vb_status rc = VB_EOF;

  if ( at <= 0 || vb_seek(r, at, errmsg) )
  { while( (rc=vb_read(r, &frame, &samples, &pts, errmsg)) == VB_FRAME )
    { SDL_Surface *tmp = shown;		/* keep the last frame */

      shown = frame;
      frame = tmp;
      if ( pts >= at - half )
	break;
    }
  } else
    rc = VB_ERROR;

  if ( rc == VB_ERROR )
    errorPce(vid, NAME_ioError, CtoName(errmsg));
  else if ( shown )
  { image = newObject(ClassImage, NIL, ONE, ONE, NAME_pixmap, EAV);
    if ( !ws_set_image_pixels(image, shown) )
    { doneObject(image);
      image = NULL;
    }
  }

  if ( frame )
    SDL_DestroySurface(frame);
  if ( shown )
    SDL_DestroySurface(shown);
  vb_close_reader(r);

  return image;
}


/* Show the frame that is due, if any.  Returns true if v needs more
 * ticks.
 */

static bool
tick_video(ws_video *v)
{ Video vid = v->video;
  SDL_Surface *show = NULL;
  bool ended, failed;
  char errmsg[VB_ERRLEN];

  if ( !v->announced )			/* opening a network stream */
  { bool opened, open_failed;

    SDL_LockMutex(v->lock);
    opened = v->opened;
    open_failed = v->open_failed;
    strcpy(errmsg, v->errmsg);
    SDL_UnlockMutex(v->lock);

    if ( open_failed )
    { ws_close_video(vid);		/* frees v */
      setStatusVideo(vid, NAME_closed);
      errorPce(vid, NAME_cannotOpen, CtoName(errmsg));
      return false;
    }
    if ( !opened )
      return true;

    v->announced = true;
    if ( v->play_on_open )
    { v->play_on_open = false;
      ws_play_video(vid);
      setStatusVideo(vid, NAME_playing);
    } else
      setStatusVideo(vid, NAME_paused);
  }

  SDL_LockMutex(v->lock);
  if ( v->show_next )
  { if ( v->qcount > 0 )
    { if ( v->info.live )		/* the clock starts at this frame */
      { v->start_pos = v->queue[v->qhead].pts;
	v->start_ns  = SDL_GetTicksNS();
      }
      show = v->queue[v->qhead].surface;
      v->qhead = (v->qhead+1)%QUEUE_SIZE;
      v->qcount--;
      v->show_next = false;
    }
  } else if ( v->playing )
  { double now;

    if ( !v->info.live && v->qcount == 0 && !v->eof && !v->failed &&
	 (!v->audio || audio_queued(v) == 0.0) )
      v->start_ns = SDL_GetTicksNS();	/* waiting for data: hold the clock */
    now = clock_position(v);

    while( v->qcount > 0 && v->queue[v->qhead].pts <= now )
    { put_pool(&v->pool, show);		/* late: drop */
      show = v->queue[v->qhead].surface;
      v->qhead = (v->qhead+1)%QUEUE_SIZE;
      v->qcount--;
    }
  }
  if ( show )
    SDL_SignalCondition(v->cond);
  ended	 = ( v->eof && v->qcount == 0 && !v->seek &&
	     (!v->audio || audio_queued(v) == 0.0) );
  failed = v->failed && v->qcount == 0 && !v->reported;
  if ( failed )
    strcpy(errmsg, v->errmsg);
  SDL_UnlockMutex(v->lock);

  if ( show )
  { SDL_Surface *old = ws_sdl_texture_swap_surface(vid->texture, show);

    SDL_LockMutex(v->lock);
    put_pool(&v->pool, old);
    SDL_UnlockMutex(v->lock);
  }

  if ( failed )
  { v->reported = true;
    v->playing = false;
    setStatusVideo(vid, NAME_paused);
    errorPce(vid, NAME_ioError, CtoName(errmsg));
  } else if ( ended && v->playing && !show )
  { if ( vid->loop == ON && !v->info.live )
    { ws_seek_video(vid, 0.0);
    } else
    { v->start_pos = video_position(v);
      v->playing = false;
      setStatusVideo(vid, NAME_finished);
      if ( notNil(vid->message) )
	forwardReceiverCode(vid->message, vid, vid, EAV);
    }
  }

  return v->playing || v->show_next || !v->announced;
}


bool
sdl_video_tick(void)
{ bool active = false;

  for(ws_video *v = videos; v; )
  { ws_video *next = v->next;		/* message may close it */
    Video vid = v->video;

    addCodeReference(vid);
    if ( tick_video(v) )
      active = true;
    delCodeReference(vid);
    freeableObj(vid);
    v = next;
  }

  return active;
}


		 /*******************************
		 *	      WRITING		*
		 *******************************/

typedef struct ws_video_writer
{ struct ws_video_writer *next;		/* writers with a code target */
  VideoWriter	video_writer;		/* the xpce object */
  vb_writer    *writer;
  bool		live;			/* drop frames rather than wait */
  int		width;			/* size of the frames */
  int		height;
  SDL_PixelFormat format;		/* format the encoder wants */
  SDL_Colorspace colorspace;
  SDL_AudioStream *conv;		/* converts audio; NULL: no audio */
  int		audio_channels;
  SDL_Thread   *thread;
  SDL_Mutex    *lock;			/* protects the fields below */
  SDL_Condition *cond;
  qframe	queue[WQUEUE_SIZE];
  int		qhead;
  int		qcount;
  surface_pool	pool;
  bool		closing;		/* no more frames */
  bool		done;			/* the thread finished the data */
  bool		failed;			/* see errmsg */
  char		errmsg[VB_ERRLEN];
  Uint8	       *out;			/* data for the code target */
  size_t	out_len;
  size_t	out_allocated;
} ws_video_writer;

static ws_video_writer *code_writers = NULL;

/* The encoded data for a code target.  The writer thread adds it; the
 * media tick gives it to the code.
 */

static bool
out_write(void *closure, const void *data, size_t len)
{ ws_video_writer *w = closure;
  bool ok = true;

  SDL_LockMutex(w->lock);
  if ( w->out_len + len > w->out_allocated )
  { size_t size = w->out_allocated ? w->out_allocated : 65536;

    while( size < w->out_len + len )
      size *= 2;
    Uint8 *out = SDL_realloc(w->out, size);
    if ( out )
    { w->out = out;
      w->out_allocated = size;
    } else
      ok = false;
  }
  if ( ok )
  { memcpy(w->out+w->out_len, data, len);
    w->out_len += len;
  }
  SDL_UnlockMutex(w->lock);

  return ok;
}

/* Run the code target with the data that is ready */

static void
deliver_out(ws_video_writer *w)
{ VideoWriter vw = w->video_writer;
  Uint8 *data;
  size_t len;

  SDL_LockMutex(w->lock);
  data = w->out;
  len = w->out_len;
  w->out = NULL;
  w->out_len = w->out_allocated = 0;
  SDL_UnlockMutex(w->lock);

  if ( len > 0 && instanceOfObject(vw->target, ClassCode) )
  { string str;

    str_set_n_ascii(&str, len, (char*)data);
    CharArray ca = StringToScratchCharArray(&str);
    forwardReceiverCode(vw->target, vw, ca, EAV);
    doneScratchCharArray(ca);
  }
  SDL_free(data);
}

static bool
sdl_video_writer_tick(void)
{ for(ws_video_writer *w = code_writers; w; )
  { ws_video_writer *next = w->next;	/* code may close it */
    VideoWriter vw = w->video_writer;

    addCodeReference(vw);
    deliver_out(w);
    delCodeReference(vw);
    freeableObj(vw);
    w = next;
  }

  return code_writers != NULL;
}

static int SDLCALL
writer_thread(void *data)
{ ws_video_writer *w = data;
  char errmsg[VB_ERRLEN];

  SDL_LockMutex(w->lock);
  for(;;)
  { while( w->qcount == 0 && !w->closing )
      SDL_WaitCondition(w->cond, w->lock);
    if ( w->qcount == 0 )		/* closing */
      break;

    qframe f = w->queue[w->qhead];
    bool ok = true;

    SDL_UnlockMutex(w->lock);
    if ( !w->failed )
    { if ( f.surface )
	ok = vb_write_frame(w->writer, f.surface, f.pts, errmsg);
      else
	ok = vb_write_audio(w->writer, f.samples, f.nsamples, errmsg);
    }
    SDL_free(f.samples);
    SDL_LockMutex(w->lock);
    w->qhead = (w->qhead+1)%WQUEUE_SIZE;
    w->qcount--;
    put_pool(&w->pool, f.surface);
    if ( !ok && !w->failed )
    { strcpy(w->errmsg, errmsg);
      w->failed = true;
    }
    SDL_SignalCondition(w->cond);
  }
  SDL_UnlockMutex(w->lock);

  bool ok = vb_close_writer(w->writer, errmsg);
  w->writer = NULL;
  SDL_LockMutex(w->lock);
  if ( !ok && !w->failed )
  { strcpy(w->errmsg, errmsg);
    w->failed = true;
  }
  w->done = true;
  SDL_UnlockMutex(w->lock);

  return 0;
}


status
ws_open_video_writer(VideoWriter vw)
{ if ( vw->ws )
    succeed;

  ws_video_writer *w = SDL_calloc(1, sizeof(*w));
  char errmsg[VB_ERRLEN];
  vb_wspec spec =
  { .width	= valInt(vw->size->w) & ~1, /* 4:2:0 needs even sizes */
    .height	= valInt(vw->size->h) & ~1,
    .frame_rate	= valNum(vw->frame_rate),
    .codec	= isNil(vw->codec) ? NULL : nameToUTF8(vw->codec),
    .quality	= isNil(vw->quality) ? -1 : valInt(vw->quality),
    .audio_rate = isNil(vw->audio_frequency) ? 0
					     : valInt(vw->audio_frequency),
    .audio_channels = isNil(vw->audio_channels) ? 0
						: valInt(vw->audio_channels),
    .format	= isNil(vw->format) ? NULL : nameToUTF8(vw->format),
    .live	= vw->live == ON,
    .bitrate	= isNil(vw->bitrate) ? 0 : valInt(vw->bitrate)
  };
  bool code = instanceOfObject(vw->target, ClassCode);

  if ( !w )
    return errorPce(vw, NAME_notEnoughMemory);
  if ( spec.width < 2 || spec.height < 2 )
  { SDL_free(w);
    return errorPce(vw, NAME_cannotOpen, CtoName("frames are too small"));
  }
  w->video_writer = vw;
  w->live = spec.live;
  w->lock = SDL_CreateMutex();
  if ( code )
  { spec.write = out_write;
    spec.closure = w;
  }
  if ( !(w->writer = vb_open_writer(code ? NULL : source_name(vw->target),
				    &spec, &w->format, &w->colorspace,
				    errmsg)) )
  { SDL_DestroyMutex(w->lock);
    SDL_free(w->out);
    SDL_free(w);
    return errorPce(vw, NAME_cannotOpen, CtoName(errmsg));
  }
  w->width  = spec.width;
  w->height = spec.height;
  w->cond   = SDL_CreateCondition();
  if ( spec.audio_rate > 0 )
  { SDL_AudioSpec to = { SDL_AUDIO_F32, spec.audio_channels,
			 spec.audio_rate };

    w->conv = SDL_CreateAudioStream(&to, &to);
    w->audio_channels = spec.audio_channels;
  }
  if ( !(w->thread = SDL_CreateThread(writer_thread, "xpce-video-writer", w)) )
  { vb_close_writer(w->writer, errmsg);
    SDL_DestroyCondition(w->cond);
    SDL_DestroyMutex(w->lock);
    SDL_free(w);
    return errorPce(vw, NAME_cannotOpen, CtoName(SDL_GetError()));
  }
  vw->ws = w;
  if ( code )
  { w->next = code_writers;
    code_writers = w;
    sdl_add_media_source(sdl_video_writer_tick);
    sdl_start_media_tick();
  }
  assign(vw, codec, UTF8ToName(spec.codec));
  if ( spec.audio_rate > 0 )
  { assign(vw, audio_frequency, toInt(spec.audio_rate));
    assign(vw, audio_channels, toInt(spec.audio_channels));
    assign(vw, audio_codec, UTF8ToName(spec.audio_codec));
  }

  succeed;
}


/* Add an item to the queue of the writer thread, waiting if it is full.
 * Called with w->lock held.
 */

static bool
enqueue_item(ws_video_writer *w, SDL_Surface *s, double pts,
	     float *samples, int nsamples)
{ while( w->qcount == WQUEUE_SIZE && !w->failed )
    SDL_WaitCondition(w->cond, w->lock); /* the encoder falls behind */
  if ( w->failed )
    return false;

  int i = (w->qhead+w->qcount)%WQUEUE_SIZE;
  w->queue[i].surface  = s;
  w->queue[i].pts      = pts;
  w->queue[i].samples  = samples;
  w->queue[i].nsamples = nsamples;
  w->qcount++;
  SDL_SignalCondition(w->cond);

  return true;
}


/* Queue the samples the converter has ready */

static bool
queue_converted(ws_video_writer *w)
{ int avail = SDL_GetAudioStreamAvailable(w->conv);
  int frame = w->audio_channels*(int)sizeof(float);

  if ( avail < frame )
    return true;

  float *samples = SDL_malloc(avail);
  int got;

  if ( !samples ||
       (got = SDL_GetAudioStreamData(w->conv, samples, avail)) < 0 )
  { SDL_free(samples);
    return false;
  }

  SDL_LockMutex(w->lock);
  bool ok = enqueue_item(w, NULL, 0.0, samples, got/frame);
  SDL_UnlockMutex(w->lock);
  if ( !ok )
    SDL_free(samples);

  return ok;
}


/* Add samples of `format` at `rate` with `channels` to the audio */

status
ws_append_audio_video_writer(VideoWriter vw, CharArray data,
			     Name format, int rate, int channels)
{ ws_video_writer *w = vw->ws;
  const Uint8 *bytes;
  Uint8 *tmp;
  size_t len;

  if ( !w )
    fail;
  if ( !w->conv )
    return errorPce(vw, NAME_ioError, CtoName("the video has no audio"));
  if ( !(bytes = sdl_data_bytes(data, &len, &tmp)) )
    return errorPce(vw, NAME_ioError, CtoName("data is not binary"));

  SDL_AudioSpec from = { sdl_audio_format(format), channels, rate };
  bool ok = ( SDL_SetAudioStreamFormat(w->conv, &from, NULL) &&
	      SDL_PutAudioStreamData(w->conv, bytes, (int)len) );
  SDL_free(tmp);
  if ( !ok )
    return errorPce(vw, NAME_ioError, CtoName(SDL_GetError()));
  if ( !queue_converted(w) )
    return errorPce(vw, NAME_ioError,
		    CtoName(w->failed ? w->errmsg : SDL_GetError()));

  succeed;
}


static bool
convert_frame(ws_video_writer *w, SDL_Surface *src, SDL_Surface *dst)
{ return SDL_ConvertPixelsAndColorspace(
	   w->width, w->height,
	   src->format, SDL_GetSurfaceColorspace(src), 0,
	   src->pixels, src->pitch,
	   dst->format, w->colorspace, 0,
	   dst->pixels, dst->pitch);
}

/* Make a surface of the size and pixel format of the encoder from
 * `src`.  A frame of another size is scaled.  SDL converts YUV to YUV
 * only if the colorspace is the same, e.g., not from the BT.709 of a
 * webcam to the BT.601 of a small video: such a frame goes through
 * RGB.
 */

static SDL_Surface *
encoder_frame(ws_video_writer *w, SDL_Surface *src, SDL_Surface *dst)
{ SDL_Surface *rgb = NULL;

  if ( dst && (dst->w != w->width || dst->h != w->height ||
	       dst->format != w->format) )
  { SDL_DestroySurface(dst);
    dst = NULL;
  }
  if ( !dst && !(dst = SDL_CreateSurface(w->width, w->height, w->format)) )
    return NULL;

  if ( src->w == w->width && src->h == w->height &&
       convert_frame(w, src, dst) )
    goto out;

  if ( !(rgb = SDL_CreateSurface(w->width, w->height,
				 SDL_PIXELFORMAT_ARGB8888)) )
    goto error;
  if ( src->w == w->width && src->h == w->height )
  { if ( !SDL_ConvertPixelsAndColorspace(
	    src->w, src->h,
	    src->format, SDL_GetSurfaceColorspace(src), 0,
	    src->pixels, src->pitch,
	    rgb->format, SDL_COLORSPACE_SRGB, 0,
	    rgb->pixels, rgb->pitch) )
      goto error;
  } else
  { SDL_Surface *tmp = src;

    if ( SDL_ISPIXELFORMAT_FOURCC(src->format) &&
	 !(tmp = SDL_ConvertSurface(src, SDL_PIXELFORMAT_ARGB8888)) )
      goto error;
    bool ok = SDL_BlitSurfaceScaled(tmp, NULL, rgb, NULL,
				    SDL_SCALEMODE_LINEAR);
    if ( tmp != src )
      SDL_DestroySurface(tmp);
    if ( !ok )
      goto error;
  }
  if ( !convert_frame(w, rgb, dst) )
    goto error;

out:
  if ( rgb )
    SDL_DestroySurface(rgb);
  SDL_SetSurfaceColorspace(dst, w->colorspace);
  return dst;

error:
  if ( rgb )
    SDL_DestroySurface(rgb);
  SDL_DestroySurface(dst);
  return NULL;
}


status
ws_append_video_writer(VideoWriter vw, SDL_Surface *src, double pts)
{ ws_video_writer *w = vw->ws;
  SDL_Surface *s;

  if ( !w )
    fail;

  SDL_LockMutex(w->lock);
  bool failed = w->failed;
  bool full = w->live && w->qcount == WQUEUE_SIZE;
  s = failed || full ? NULL : take_pool(&w->pool);
  SDL_UnlockMutex(w->lock);

  if ( failed )
    return errorPce(vw, NAME_ioError, CtoName(w->errmsg));
  if ( full )				/* live: drop rather than wait */
  { assign(vw, dropped, add(vw->dropped, ONE));
    succeed;
  }
  if ( !(s = encoder_frame(w, src, s)) )
    return errorPce(vw, NAME_ioError, CtoName(SDL_GetError()));

  SDL_LockMutex(w->lock);
  bool ok = enqueue_item(w, s, pts, NULL, 0);
  if ( !ok )
    put_pool(&w->pool, s);
  SDL_UnlockMutex(w->lock);

  if ( !ok )
    return errorPce(vw, NAME_ioError, CtoName(w->errmsg));

  succeed;
}


/* Encode the queued frames and finish the file */

status
ws_close_video_writer(VideoWriter vw)
{ ws_video_writer *w = vw->ws;

  if ( !w )
    succeed;

  if ( w->conv )			/* samples held back by resampling */
  { SDL_FlushAudioStream(w->conv);
    queue_converted(w);
    SDL_DestroyAudioStream(w->conv);
    w->conv = NULL;
  }
  SDL_LockMutex(w->lock);
  w->closing = true;
  SDL_SignalCondition(w->cond);
  SDL_UnlockMutex(w->lock);
  for(;;)				/* deliver the data while it ends */
  { bool done;

    deliver_out(w);
    SDL_LockMutex(w->lock);
    done = w->done;
    SDL_UnlockMutex(w->lock);
    if ( done )
      break;
    SDL_Delay(2);
  }
  SDL_WaitThread(w->thread, NULL);
  deliver_out(w);
  for(ws_video_writer **pp = &code_writers; *pp; pp = &(*pp)->next)
  { if ( *pp == w )
    { *pp = w->next;
      break;
    }
  }

  bool failed = w->failed;
  char errmsg[VB_ERRLEN];

  strcpy(errmsg, w->errmsg);
  free_pool(&w->pool);
  SDL_DestroyCondition(w->cond);
  SDL_DestroyMutex(w->lock);
  SDL_free(w->out);
  SDL_free(w);
  vw->ws = NULL;

  if ( failed )
    return errorPce(vw, NAME_ioError, CtoName(errmsg));

  succeed;
}


		 /*******************************
		 *	    CAPABILITIES	*
		 *******************************/

static void
add_codec(const char *name, void *closure)
{ appendChain(closure, CtoName(name));
}

Chain
ws_video_codecs(bool encode)
{ Chain ch = answerObject(ClassChain, EAV);

  vb_codecs(encode, add_codec, ch);

  return ch;
}

Name
ws_video_backend(void)
{
#if defined(HAVE_VIDEO_FFMPEG)
  return CtoName("ffmpeg");
#elif defined(HAVE_VIDEO_MF)
  return CtoName("media_foundation");
#else
  return CtoName("avfoundation");
#endif
}

#else /*HAVE_VIDEO*/

		 /*******************************
		 *	   NO VIDEO SUPPORT	*
		 *******************************/

static status
no_video(Any obj)
{ return errorPce(obj, NAME_cannotOpen,
		  CtoName("xpce was built without video support"));
}

status ws_open_video(Video vid)		{ return no_video(vid); }
status ws_write_video(Video vid, CharArray data) { return no_video(vid); }
bool   ws_video_opening(Video vid)	{ return false; }
Image  ws_video_frame_image(Video vid, double at)
{ no_video(vid);
  return NULL;
}
void   ws_close_video(Video vid)	{}
void   ws_play_video(Video vid)		{}
void   ws_pause_video(Video vid)	{}
void   ws_seek_video(Video vid, double t) {}
double ws_video_position(Video vid)	{ return 0.0; }
bool   ws_video_info(Video vid, video_info *info) { return false; }
void   ws_volume_video(Video vid)	{}
bool   sdl_video_tick(void)		{ return false; }
status ws_open_video_writer(VideoWriter vw) { return no_video(vw); }
status ws_append_video_writer(VideoWriter vw, SDL_Surface *src, double pts)
{ fail;
}
status ws_close_video_writer(VideoWriter vw) { succeed; }
status ws_append_audio_video_writer(VideoWriter vw, CharArray data,
				    Name format, int rate, int channels)
{ fail;
}
Chain  ws_video_codecs(bool encode)
{ return answerObject(ClassChain, EAV);
}
Name   ws_video_backend(void)		{ return NIL; }

#endif /*HAVE_VIDEO*/
