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

/* The ffmpeg backend for video (see video_backend.h).  We only use
 * libavformat, libavcodec and libavutil: SDL converts the pixels.
 */

#include <SDL3/SDL.h>
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/imgutils.h>
#include <libavutil/opt.h>
#include <libavutil/pixdesc.h>
#include <string.h>
#include "video_backend.h"

#define streq(s, t) (strcmp((s), (t)) == 0)

static void
av_error(char *errmsg, const char *what, int rc)
{ char buf[AV_ERROR_MAX_STRING_SIZE];

  av_strerror(rc, buf, sizeof(buf));
  snprintf(errmsg, VB_ERRLEN, "%s: %s", what, buf);
}

static void
vb_error(char *errmsg, const char *fmt, const char *arg)
{ snprintf(errmsg, VB_ERRLEN, fmt, arg);
}

/* We report errors ourselves.  The libraries print a lot otherwise,
 * also when a network stream is interrupted because it is closed.
 */

static void
init_ffmpeg(void)
{ static bool done = false;

  if ( !done )
  { av_log_set_level(AV_LOG_FATAL);
    if ( !SDL_getenv("SVT_LOG") )	/* SVT-AV1 logs itself */
      SDL_setenv_unsafe("SVT_LOG", "1", 0);
    done = true;
  }
}


		 /*******************************
		 *	       READING		*
		 *******************************/

struct vb_reader
{ AVFormatContext *fmt;
  AVCodecContext  *dec;			/* video decoder */
  AVCodecContext  *adec;		/* audio decoder or NULL */
  AVPacket	  *pkt;
  AVFrame	  *frame;
  int		   stream;		/* index of the video stream */
  int		   astream;		/* index of the audio stream or -1 */
  double	   time_base;		/* seconds per video pts unit */
  double	   atime_base;		/* seconds per audio pts unit */
  double	   start;		/* time of the start in seconds */
  bool		   eof;			/* demuxer is done */
  bool		   video_done;		/* decoders are drained */
  bool		   audio_done;
  float		  *samples;		/* converted audio */
  size_t	   asize;		/* allocated floats in samples */
  AVIOContext	  *avio;		/* custom I/O or NULL */
  vb_source	   source;		/* for the custom I/O */
};


static AVCodecContext *
open_decoder(AVStream *st, const AVCodec *codec, char *errmsg)
{ AVCodecContext *dec;
  int rc;

  if ( !(dec = avcodec_alloc_context3(codec)) ||
       avcodec_parameters_to_context(dec, st->codecpar) < 0 )
  { vb_error(errmsg, "%s", "cannot create decoder");
    avcodec_free_context(&dec);
    return NULL;
  }
  dec->thread_count = 0;		/* as many as useful */
  if ( (rc=avcodec_open2(dec, codec, NULL)) < 0 )
  { av_error(errmsg, codec->name, rc);
    avcodec_free_context(&dec);
    return NULL;
  }

  return dec;
}


static int
channel_count(const AVCodecContext *c)
{
#if LIBAVUTIL_VERSION_INT >= AV_VERSION_INT(57,24,100)
  return c->ch_layout.nb_channels;
#else
  return c->channels;
#endif
}


/* Blocking calls on the network poll this to see whether they must
 * give up, e.g., because the video is closed.
 */

static int
interrupt_cb(void *closure)
{ SDL_AtomicInt *flag = closure;

  return flag && SDL_GetAtomicInt(flag) != 0;
}

/* Custom I/O over the read and seek callbacks of a source */

#define AVIO_BUFSIZE 65536

static int
source_read(void *opaque, uint8_t *buf, int size)
{ vb_reader *r = opaque;
  long n = r->source.read(r->source.closure, buf, (size_t)size);

  return n > 0 ? (int)n : n == 0 ? AVERROR_EOF : AVERROR(EIO);
}

static int64_t
source_seek(void *opaque, int64_t offset, int whence)
{ vb_reader *r = opaque;

  if ( (whence & AVSEEK_SIZE) || !r->source.seek )
    return -1;				/* size is unknown */

  return r->source.seek(r->source.closure, offset, whence & ~AVSEEK_FORCE);
}

static AVDictionary *
options_dict(char **options)
{ AVDictionary *d = NULL;

  for(char **o = options; o && o[0] && o[1]; o += 2)
    av_dict_set(&d, o[0], o[1], 0);

  return d;
}


vb_reader *
vb_open_reader(const vb_source *src, bool audio, vb_info *info,
	       char *errmsg)
{ vb_reader *r = SDL_calloc(1, sizeof(*r));
  const AVCodec *codec = NULL;
  const char *source = src->url ? src->url : "stream";
  int rc;

  init_ffmpeg();
  if ( !r )
  { vb_error(errmsg, "%s", "out of memory");
    return NULL;
  }
  r->astream = -1;
  r->source = *src;
  if ( !(r->fmt = avformat_alloc_context()) )
  { vb_error(errmsg, "%s", "out of memory");
    goto error;
  }
  r->fmt->interrupt_callback.callback = interrupt_cb;
  r->fmt->interrupt_callback.opaque = src->interrupt;
  if ( !src->url )
  { uint8_t *buf = av_malloc(AVIO_BUFSIZE);

    if ( !buf ||
	 !(r->avio = avio_alloc_context(buf, AVIO_BUFSIZE, 0, r, source_read,
					NULL, src->seek ? source_seek : NULL)) )
    { av_free(buf);
      vb_error(errmsg, "%s", "out of memory");
      goto error;
    }
    if ( !src->seek )
      r->avio->seekable = 0;
    r->fmt->pb = r->avio;
    r->fmt->flags |= AVFMT_FLAG_CUSTOM_IO;
  }

  AVDictionary *opts = options_dict(src->options);
  rc = avformat_open_input(&r->fmt, src->url, NULL, &opts);
  av_dict_free(&opts);
  if ( rc < 0 )				/* frees r->fmt */
  { av_error(errmsg, source, rc);
    goto error;
  }
  if ( (rc=avformat_find_stream_info(r->fmt, NULL)) < 0 )
  { av_error(errmsg, source, rc);
    goto error;
  }
  if ( (rc=av_find_best_stream(r->fmt, AVMEDIA_TYPE_VIDEO,
			       -1, -1, &codec, 0)) < 0 )
  { av_error(errmsg, source, rc);
    goto error;
  }
  r->stream = rc;

  AVStream *st = r->fmt->streams[r->stream];
  if ( !(r->dec = open_decoder(st, codec, errmsg)) )
    goto error;
  r->pkt   = av_packet_alloc();
  r->frame = av_frame_alloc();
  r->time_base = av_q2d(st->time_base);
  r->start = ( r->fmt->start_time == AV_NOPTS_VALUE ? 0.0
	       : (double)r->fmt->start_time/AV_TIME_BASE );

  memset(info, 0, sizeof(*info));
  info->width  = r->dec->width;
  info->height = r->dec->height;
  AVRational fr = st->avg_frame_rate.num ? st->avg_frame_rate
					 : st->r_frame_rate;
  info->frame_rate = fr.num && fr.den ? av_q2d(fr) : 0.0;
  if ( st->duration != AV_NOPTS_VALUE )
    info->duration = st->duration*r->time_base;
  else if ( r->fmt->duration != AV_NOPTS_VALUE )
    info->duration = (double)r->fmt->duration/AV_TIME_BASE;
  info->codec = codec->name;
	/* A network stream we cannot seek or without an end, such as a
	 * live HLS stream, whose playlist may well be seekable.
	 */
  info->live = ( src->url && strstr(src->url, "://") &&
		 ( !r->fmt->pb ||
		   !(r->fmt->pb->seekable & AVIO_SEEKABLE_NORMAL) ||
		   ( st->duration == AV_NOPTS_VALUE &&
		     r->fmt->duration == AV_NOPTS_VALUE ) ) );
  if ( info->live )
    info->duration = 0.0;

  const AVCodec *acodec = NULL;
  if ( audio &&
       (rc=av_find_best_stream(r->fmt, AVMEDIA_TYPE_AUDIO,
			       -1, r->stream, &acodec, 0)) >= 0 )
  { AVStream *ast = r->fmt->streams[rc];
    char aerr[VB_ERRLEN];

    if ( (r->adec = open_decoder(ast, acodec, aerr)) )
    { r->astream = rc;			/* without audio if we cannot */
      r->atime_base = av_q2d(ast->time_base);
      info->audio_rate = r->adec->sample_rate;
      info->audio_channels = channel_count(r->adec);
      info->audio_codec = acodec->name;
    }
  }
  if ( r->astream < 0 )
    r->audio_done = true;

  return r;

error:
  vb_close_reader(r);
  return NULL;
}


void
vb_close_reader(vb_reader *r)
{ if ( r )
  { av_frame_free(&r->frame);
    av_packet_free(&r->pkt);
    avcodec_free_context(&r->dec);
    avcodec_free_context(&r->adec);
    avformat_close_input(&r->fmt);
    if ( r->avio )
    { av_freep(&r->avio->buffer);
      avio_context_free(&r->avio);
    }
    SDL_free(r->samples);
    SDL_free(r);
  }
}


/* The SDL colorspace of a frame.  Videos that do not say are assumed
 * to use BT.709 if they are HD and BT.601 otherwise.
 */

static SDL_Colorspace
frame_colorspace(const AVFrame *f)
{ bool full = ( f->color_range == AVCOL_RANGE_JPEG ||
		f->format == AV_PIX_FMT_YUVJ420P );

  switch(f->colorspace)
  { case AVCOL_SPC_BT709:
      return full ? SDL_COLORSPACE_BT709_FULL : SDL_COLORSPACE_BT709_LIMITED;
    case AVCOL_SPC_BT470BG:
    case AVCOL_SPC_SMPTE170M:
      return full ? SDL_COLORSPACE_JPEG : SDL_COLORSPACE_BT601_LIMITED;
    case AVCOL_SPC_BT2020_NCL:
    case AVCOL_SPC_BT2020_CL:
      return full ? SDL_COLORSPACE_BT2020_FULL : SDL_COLORSPACE_BT2020_LIMITED;
    default:
      if ( f->height >= 720 )
	return full ? SDL_COLORSPACE_BT709_FULL : SDL_COLORSPACE_BT709_LIMITED;
      return full ? SDL_COLORSPACE_JPEG : SDL_COLORSPACE_BT601_LIMITED;
  }
}


static void
copy_plane(Uint8 *dst, int dpitch, const Uint8 *src, int spitch,
	   int bytes, int rows)
{ for(int y=0; y<rows; y++)
    memcpy(dst+(size_t)y*dpitch, src+(size_t)y*spitch, bytes);
}


/* 10-bit planar 4:2:0 to P010: the U and V samples are interleaved and
 * all samples are shifted to the high bits.
 */

static void
copy_p010(SDL_Surface *s, const AVFrame *f)
{ int w = f->width, h = f->height;
  int cw = (w+1)/2, ch = (h+1)/2;
  Uint8 *pixels = s->pixels;

  for(int y=0; y<h; y++)
  { const Uint16 *in = (const Uint16*)(f->data[0]+(size_t)y*f->linesize[0]);
    Uint16 *out = (Uint16*)(pixels+(size_t)y*s->pitch);

    for(int x=0; x<w; x++)
      out[x] = in[x]<<6;
  }
  for(int y=0; y<ch; y++)
  { const Uint16 *u = (const Uint16*)(f->data[1]+(size_t)y*f->linesize[1]);
    const Uint16 *v = (const Uint16*)(f->data[2]+(size_t)y*f->linesize[2]);
    Uint16 *out = (Uint16*)(pixels+(size_t)(h+y)*s->pitch);

    for(int x=0; x<cw; x++)
    { out[2*x]   = u[x]<<6;
      out[2*x+1] = v[x]<<6;
    }
  }
}


static SDL_PixelFormat
sdl_format(int fmt)
{ switch(fmt)
  { case AV_PIX_FMT_YUV420P:
    case AV_PIX_FMT_YUVJ420P:
      return SDL_PIXELFORMAT_IYUV;
    case AV_PIX_FMT_NV12:
      return SDL_PIXELFORMAT_NV12;
    case AV_PIX_FMT_P010LE:
    case AV_PIX_FMT_YUV420P10LE:
      return SDL_PIXELFORMAT_P010;
    default:
      return SDL_PIXELFORMAT_UNKNOWN;
  }
}


/* Copy the decoded frame into *sp, which is reused if it has the same
 * size and format.  The planes of an SDL YUV surface follow each other:
 * for IYUV the U and V planes have half the pitch of the Y plane.
 */

static bool
frame_to_surface(const AVFrame *f, SDL_Surface **sp, char *errmsg)
{ SDL_PixelFormat fmt = sdl_format(f->format);
  SDL_Surface *s = *sp;
  int w = f->width, h = f->height;

  if ( fmt == SDL_PIXELFORMAT_UNKNOWN )
  { const char *name = av_get_pix_fmt_name(f->format);

    vb_error(errmsg, "Unsupported pixel format: %s", name ? name : "?");
    return false;
  }
  if ( s && (s->w != w || s->h != h || s->format != fmt) )
  { SDL_DestroySurface(s);
    *sp = s = NULL;
  }
  if ( !s && !(*sp = s = SDL_CreateSurface(w, h, fmt)) )
  { vb_error(errmsg, "%s", SDL_GetError());
    return false;
  }

  Uint8 *pixels = s->pixels;
  int ch = (h+1)/2;

  switch(f->format)
  { case AV_PIX_FMT_YUV420P:
    case AV_PIX_FMT_YUVJ420P:
    { int cpitch = (s->pitch+1)/2;
      Uint8 *u = pixels + (size_t)s->pitch*h;
      Uint8 *v = u + (size_t)cpitch*ch;

      copy_plane(pixels, s->pitch, f->data[0], f->linesize[0], w, h);
      copy_plane(u, cpitch, f->data[1], f->linesize[1], (w+1)/2, ch);
      copy_plane(v, cpitch, f->data[2], f->linesize[2], (w+1)/2, ch);
      break;
    }
    case AV_PIX_FMT_NV12:
      copy_plane(pixels, s->pitch, f->data[0], f->linesize[0], w, h);
      copy_plane(pixels+(size_t)s->pitch*h, s->pitch,
		 f->data[1], f->linesize[1], 2*((w+1)/2), ch);
      break;
    case AV_PIX_FMT_P010LE:
      copy_plane(pixels, s->pitch, f->data[0], f->linesize[0], 2*w, h);
      copy_plane(pixels+(size_t)s->pitch*h, s->pitch,
		 f->data[1], f->linesize[1], 4*((w+1)/2), ch);
      break;
    case AV_PIX_FMT_YUV420P10LE:
      copy_p010(s, f);
      break;
  }
  SDL_SetSurfaceColorspace(s, frame_colorspace(f));

  return true;
}


/* Sample `i` of channel `c` as a float */

static float
sample(const AVFrame *f, int planar, int bytes, int fmt, int i, int c,
       int channels)
{ const Uint8 *p = ( planar ? f->extended_data[c] + (size_t)i*bytes
		       : f->extended_data[0] + ((size_t)i*channels+c)*bytes );

  switch(fmt)
  { case AV_SAMPLE_FMT_U8:  return (*p-128)/128.0f;
    case AV_SAMPLE_FMT_S16: return *(const int16_t*)p/32768.0f;
    case AV_SAMPLE_FMT_S32: return *(const int32_t*)p/2147483648.0f;
    case AV_SAMPLE_FMT_FLT: return *(const float*)p;
    case AV_SAMPLE_FMT_DBL: return (float)*(const double*)p;
    default:		    return 0.0f;
  }
}

static bool
frame_to_samples(vb_reader *r, const AVFrame *f, vb_samples *out,
		 char *errmsg)
{ int channels = channel_count(r->adec);
  int fmt = av_get_packed_sample_fmt(f->format);
  int planar = av_sample_fmt_is_planar(f->format);
  int bytes = av_get_bytes_per_sample(f->format);
  size_t need = (size_t)f->nb_samples*channels;

  if ( fmt == AV_SAMPLE_FMT_S64 || bytes == 0 )
  { const char *name = av_get_sample_fmt_name(f->format);

    vb_error(errmsg, "Unsupported sample format: %s", name ? name : "?");
    return false;
  }
  if ( need > r->asize )
  { float *s = SDL_realloc(r->samples, need*sizeof(float));

    if ( !s )
    { vb_error(errmsg, "%s", "out of memory");
      return false;
    }
    r->samples = s;
    r->asize = need;
  }
  for(int i=0; i<f->nb_samples; i++)
  { for(int c=0; c<channels; c++)
      r->samples[(size_t)i*channels+c] =
	sample(f, planar, bytes, fmt, i, c, channels);
  }
  out->data = r->samples;
  out->frames = f->nb_samples;

  return true;
}


vb_status
vb_read(vb_reader *r, SDL_Surface **frame, vb_samples *samples,
	double *pts, char *errmsg)
{ for(;;)
  { int rc;

    if ( !r->video_done )
    { rc = avcodec_receive_frame(r->dec, r->frame);
      if ( rc == 0 )
      { int64_t t = r->frame->best_effort_timestamp;
	bool ok = frame_to_surface(r->frame, frame, errmsg);

	*pts = t == AV_NOPTS_VALUE ? 0.0 : t*r->time_base - r->start;
	av_frame_unref(r->frame);
	return ok ? VB_FRAME : VB_ERROR;
      }
      if ( rc == AVERROR_EOF )
	r->video_done = true;
      else if ( rc != AVERROR(EAGAIN) )
      { av_error(errmsg, "decode", rc);
	return VB_ERROR;
      }
    }

    if ( !r->audio_done )
    { rc = avcodec_receive_frame(r->adec, r->frame);
      if ( rc == 0 )
      { int64_t t = r->frame->best_effort_timestamp;
	bool ok = frame_to_samples(r, r->frame, samples, errmsg);

	*pts = t == AV_NOPTS_VALUE ? -1.0 : t*r->atime_base - r->start;
	av_frame_unref(r->frame);
	return ok ? VB_AUDIO : VB_ERROR;
      }
      if ( rc == AVERROR_EOF )
	r->audio_done = true;
      else if ( rc != AVERROR(EAGAIN) )
      { av_error(errmsg, "decode audio", rc);
	return VB_ERROR;
      }
    }

    if ( r->video_done && r->audio_done )
      return VB_EOF;
    if ( r->eof )			/* drained; should not happen */
      return VB_EOF;

    rc = av_read_frame(r->fmt, r->pkt);
    if ( rc == AVERROR_EOF )
    { r->eof = true;
      avcodec_send_packet(r->dec, NULL); /* flush the decoders */
      if ( r->adec )
	avcodec_send_packet(r->adec, NULL);
      continue;
    }
    if ( rc < 0 )
    { av_error(errmsg, "read", rc);
      return VB_ERROR;
    }
    if ( r->pkt->stream_index == r->stream )
      rc = avcodec_send_packet(r->dec, r->pkt);
    else if ( r->pkt->stream_index == r->astream )
      rc = avcodec_send_packet(r->adec, r->pkt);
    av_packet_unref(r->pkt);
    if ( rc < 0 && rc != AVERROR(EAGAIN) && rc != AVERROR_INVALIDDATA )
    { av_error(errmsg, "decode", rc);
      return VB_ERROR;
    }
  }
}


bool
vb_seek(vb_reader *r, double t, char *errmsg)
{ int64_t ts = (int64_t)((t + r->start)/r->time_base);
  int rc = av_seek_frame(r->fmt, r->stream, ts, AVSEEK_FLAG_BACKWARD);

  if ( rc < 0 )
  { av_error(errmsg, "seek", rc);
    return false;
  }
  avcodec_flush_buffers(r->dec);
  if ( r->adec )
    avcodec_flush_buffers(r->adec);
  r->eof = false;
  r->video_done = false;
  r->audio_done = (r->adec == NULL);

  return true;
}


		 /*******************************
		 *	       WRITING		*
		 *******************************/

struct vb_writer
{ AVFormatContext *fmt;
  AVCodecContext  *enc;
  AVStream	  *stream;
  AVFrame	  *frame;
  AVPacket	  *pkt;
  int64_t	   last_pts;
  int64_t	   frame_duration;	/* in time_base units */
  bool		   header;		/* header was written */
					/* audio */
  AVCodecContext  *aenc;		/* NULL if there is no audio */
  AVStream	  *astream;
  AVFrame	  *aframe;
  int		   channels;
  int		   frame_size;		/* samples per encoder frame */
  int64_t	   asamples;		/* samples encoded */
  float		  *pending;		/* samples not yet encoded */
  int		   npending;		/* sample frames in pending */
  int		   apending;		/* allocated sample frames */
  AVIOContext	  *avio;		/* custom output or NULL */
  bool		 (*write)(void *closure, const void *data, size_t len);
  void		  *closure;
};

/* Encoders we try if the user does not name one, best first.  We skip
 * hardware encoders, which need a device.
 */

/* After libx264, the encoders of the OS: an LGPL build of ffmpeg for
 * Windows or macOS has no libx264, but encodes H.264 and HEVC using
 * Media Foundation or VideoToolbox.
 */

static const char *default_encoders[] =
{ "libx264", "h264_videotoolbox", "h264_mf", "libopenh264",
  "libx265", "hevc_videotoolbox", "hevc_mf",
  "libsvtav1", "libaom-av1", "libvpx-vp9", "libvpx", "mpeg4", NULL
};

static const char *audio_encoders[] =
{ "aac_at", "aac", "libfdk_aac", "libopus", "libvorbis", "libmp3lame",
  "aac_mf", NULL
};

static const AVCodec *
find_encoder(const char *name, const AVOutputFormat *ofmt,
	     const char **candidates)
{ if ( name )
  { const AVCodec *c = avcodec_find_encoder_by_name(name);
    const AVCodecDescriptor *d;

    if ( !c && (d=avcodec_descriptor_get_by_name(name)) )
      c = avcodec_find_encoder(d->id);
    return c;
  }

  for(const char **n = candidates; *n; n++)
  { const AVCodec *c = avcodec_find_encoder_by_name(*n);

    if ( c && avformat_query_codec(ofmt, c->id, FF_COMPLIANCE_NORMAL) != 0 )
      return c;				/* < 0: the format does not know */
  }

  return NULL;
}


static bool
supports_pix_fmt(const AVCodec *c, enum AVPixelFormat fmt)
{ const enum AVPixelFormat *fmts = NULL;

#if LIBAVCODEC_VERSION_INT >= AV_VERSION_INT(61,13,100)
  avcodec_get_supported_config(NULL, c, AV_CODEC_CONFIG_PIX_FORMAT, 0,
			       (const void**)&fmts, NULL);
#else
  fmts = c->pix_fmts;
#endif
  if ( !fmts )
    return fmt == AV_PIX_FMT_YUV420P;
  for(; *fmts != AV_PIX_FMT_NONE; fmts++)
  { if ( *fmts == fmt )
      return true;
  }

  return false;
}

static bool
supports_sample_fmt(const AVCodec *c, enum AVSampleFormat fmt)
{ const enum AVSampleFormat *fmts = NULL;

#if LIBAVCODEC_VERSION_INT >= AV_VERSION_INT(61,13,100)
  avcodec_get_supported_config(NULL, c, AV_CODEC_CONFIG_SAMPLE_FORMAT, 0,
			       (const void**)&fmts, NULL);
#else
  fmts = c->sample_fmts;
#endif
  if ( !fmts )
    return false;
  for(; *fmts != AV_SAMPLE_FMT_NONE; fmts++)
  { if ( *fmts == fmt )
      return true;
  }

  return false;
}

/* The rate closest to `rate` that the encoder supports, preferring a
 * higher one
 */

static int
supported_rate(const AVCodec *c, int rate)
{ const int *rates = NULL;
  int best = 0;

#if LIBAVCODEC_VERSION_INT >= AV_VERSION_INT(61,13,100)
  avcodec_get_supported_config(NULL, c, AV_CODEC_CONFIG_SAMPLE_RATE, 0,
			       (const void**)&rates, NULL);
#else
  rates = c->supported_samplerates;
#endif
  if ( !rates )
    return rate;
  for(; *rates; rates++)
  { if ( *rates == rate )
      return rate;
    if ( !best ||
	 (*rates > rate && (best < rate || *rates < best)) ||
	 (*rates < rate && best < rate && *rates > best) )
      best = *rates;
  }

  return best;
}


/* Add the audio stream to w.  We give the encoder planar or interleaved
 * floats or 16-bit integers.
 */

static bool
open_audio(vb_writer *w, vb_wspec *spec, char *errmsg)
{ const AVCodec *c = find_encoder(NULL, w->fmt->oformat, audio_encoders);
  enum AVSampleFormat sfmt;
  int rc;

  if ( !c )
  { vb_error(errmsg, "No audio encoder for %s", w->fmt->oformat->name);
    return false;
  }
  if ( supports_sample_fmt(c, AV_SAMPLE_FMT_FLTP) )
    sfmt = AV_SAMPLE_FMT_FLTP;
  else if ( supports_sample_fmt(c, AV_SAMPLE_FMT_FLT) )
    sfmt = AV_SAMPLE_FMT_FLT;
  else if ( supports_sample_fmt(c, AV_SAMPLE_FMT_S16) )
    sfmt = AV_SAMPLE_FMT_S16;
  else
  { vb_error(errmsg, "No sample format for %s", c->name);
    return false;
  }

  if ( !(w->aenc = avcodec_alloc_context3(c)) )
  { vb_error(errmsg, "%s", "cannot create audio encoder");
    return false;
  }
  w->channels = spec->audio_channels;
  spec->audio_rate = supported_rate(c, spec->audio_rate);
  w->aenc->sample_fmt  = sfmt;
  w->aenc->sample_rate = spec->audio_rate;
  w->aenc->time_base   = (AVRational){1, spec->audio_rate};
  w->aenc->bit_rate    = 64000*w->channels;
#if LIBAVUTIL_VERSION_INT >= AV_VERSION_INT(57,24,100)
  av_channel_layout_default(&w->aenc->ch_layout, w->channels);
#else
  w->aenc->channels = w->channels;
  w->aenc->channel_layout = av_get_default_channel_layout(w->channels);
#endif
  if ( w->fmt->oformat->flags & AVFMT_GLOBALHEADER )
    w->aenc->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;
  if ( (rc=avcodec_open2(w->aenc, c, NULL)) < 0 )
  { av_error(errmsg, c->name, rc);
    return false;
  }
  if ( !(w->astream = avformat_new_stream(w->fmt, NULL)) ||
       avcodec_parameters_from_context(w->astream->codecpar, w->aenc) < 0 )
  { vb_error(errmsg, "%s", "cannot create audio stream");
    return false;
  }
  w->astream->time_base = w->aenc->time_base;
  w->frame_size = w->aenc->frame_size > 0 ? w->aenc->frame_size : 1024;
  w->aframe = av_frame_alloc();
  spec->audio_codec = c->name;

  return true;
}


/* Set the quality (0..100) using the options of the common encoders.
 * Without a quality, we use a fast preset, so recording keeps up.
 */

/* About 0.12 bit per pixel, for encoders without a quality setting */

static int64_t
default_bitrate(const AVCodecContext *enc)
{ double fps = enc->framerate.num ? av_q2d(enc->framerate) : 30.0;

  return (int64_t)(enc->width*enc->height*fps*0.12);
}

static void
set_quality(AVCodecContext *enc, const AVCodec *c, int quality, bool live,
	    int bitrate)
{ void *priv = enc->priv_data;
  const char *n = c->name;

  if ( live )				/* frames leave at once */
    enc->max_b_frames = 0;
  if ( bitrate > 0 )			/* about constant */
  { enc->bit_rate = bitrate;
    enc->rc_max_rate = bitrate;
    enc->rc_buffer_size = bitrate;
    quality = -1;
  }

  if ( streq(n, "libx264") || streq(n, "libx265") )
  { av_opt_set(priv, "preset", live ? "veryfast" : "fast", 0);
    if ( live )
      av_opt_set(priv, "tune", "zerolatency", 0);
    if ( quality >= 0 )
      av_opt_set_int(priv, "crf", 51*(100-quality)/100, 0);
  } else if ( streq(n, "libsvtav1") )
  { av_opt_set_int(priv, "preset", 10, 0);
    if ( quality >= 0 )
      av_opt_set_int(priv, "crf", 63*(100-quality)/100, 0);
  } else if ( streq(n, "libaom-av1") )
  { av_opt_set_int(priv, "cpu-used", 8, 0);
    av_opt_set(priv, "usage", "realtime", 0);
    if ( quality >= 0 )
      av_opt_set_int(priv, "crf", 63*(100-quality)/100, 0);
  } else if ( strstr(n, "_videotoolbox") )
  { av_opt_set_int(priv, "allow_sw", 1, 0); /* also without hardware */
    if ( live )
      av_opt_set_int(priv, "realtime", 1, 0);
    if ( quality >= 0 )
      enc->global_quality = quality > 0 ? quality : 1;	/* 1..100 */
    else if ( bitrate <= 0 )
      enc->bit_rate = default_bitrate(enc);
  } else if ( strstr(n, "_mf") )
  { if ( live )
    { av_opt_set(priv, "scenario", "live_streaming", 0);
      av_opt_set(priv, "rate_control", "cbr", 0);
    } else if ( quality >= 0 )
    { av_opt_set(priv, "rate_control", "quality", 0);
      av_opt_set_int(priv, "quality", quality, 0);
    } else if ( bitrate <= 0 )
      enc->bit_rate = default_bitrate(enc);
  } else if ( strncmp(n, "libvpx", 6) == 0 )
  { av_opt_set(priv, "deadline", "realtime", 0);
    av_opt_set_int(priv, "cpu-used", 8, 0);
    if ( bitrate <= 0 )
    { av_opt_set_int(priv, "crf",
		     quality >= 0 ? 4+59*(100-quality)/100 : 32, 0);
      enc->bit_rate = 0;
    }
  } else if ( bitrate <= 0 )
  { int q = quality >= 0 ? 2+29*(100-quality)/100 : 4;

    enc->flags |= AV_CODEC_FLAG_QSCALE;
    enc->global_quality = FF_QP2LAMBDA * q;
  }
}


/* The container for a target without an explicit format.  NULL
 * guesses it from the extension, e.g., .m3u8 is HLS.
 */

static const char *
target_format(const char *file, const vb_wspec *spec)
{ if ( spec->format )
    return spec->format;
  if ( !file )
    return "mp4";
  if ( strncmp(file, "rtmp", 4) == 0 )
    return "flv";
  if ( strncmp(file, "rtp://", 6) == 0 )
    return "rtp_mpegts";
  if ( strncmp(file, "srt://", 6) == 0 || strncmp(file, "udp://", 6) == 0 ||
       strncmp(file, "tcp://", 6) == 0 )
    return "mpegts";

  return NULL;
}

/* Output to the write callback */

#if LIBAVFORMAT_VERSION_MAJOR >= 61
static int
target_write(void *opaque, const uint8_t *buf, int size)
#else
static int
target_write(void *opaque, uint8_t *buf, int size)
#endif
{ vb_writer *w = opaque;

  return w->write(w->closure, buf, (size_t)size) ? size : AVERROR(EIO);
}

/* Options for the header.  An MP4 that is not written to a file is
 * fragmented, so it can be played while it arrives.  Live HLS keeps a
 * few short segments.
 */

static AVDictionary *
header_options(vb_writer *w, const char *file, const vb_wspec *spec)
{ AVDictionary *d = NULL;
  const char *name = w->fmt->oformat->name;
  bool stream = !file || strstr(file, "://");

  if ( (strstr(name, "mp4") || strstr(name, "mov")) && (stream || spec->live) )
    av_dict_set(&d, "movflags", "frag_keyframe+empty_moov+default_base_moof",
		0);
  if ( streq(name, "hls") )
  { av_dict_set(&d, "hls_time", "2", 0);
    if ( spec->live )
    { av_dict_set(&d, "hls_list_size", "6", 0);
      av_dict_set(&d, "hls_flags", "delete_segments", 0);
    } else
      av_dict_set(&d, "hls_list_size", "0", 0);
  }

  return d;
}


/* Create and open w->enc for codec.  An encoder of the OS may fail to
 * open, e.g., Media Foundation on a Windows N edition.
 */

static bool
open_video_encoder(vb_writer *w, const AVCodec *codec, const vb_wspec *spec,
		   double fps, enum AVPixelFormat *pixp, char *errmsg)
{ enum AVPixelFormat pix = ( supports_pix_fmt(codec, AV_PIX_FMT_NV12)
			     ? AV_PIX_FMT_NV12 : AV_PIX_FMT_YUV420P );
  bool hd = spec->height >= 720;
  int rc;

  if ( !(w->enc = avcodec_alloc_context3(codec)) )
  { vb_error(errmsg, "%s", "cannot create encoder");
    return false;
  }
  w->enc->width	       = spec->width;
  w->enc->height       = spec->height;
  w->enc->pix_fmt      = pix;
  w->enc->time_base    = (AVRational){1, 1000};
  w->enc->framerate    = av_d2q(fps, 100000);
  w->enc->gop_size     = (int)(fps*2);
  w->enc->color_range  = AVCOL_RANGE_MPEG;
  w->enc->colorspace   = hd ? AVCOL_SPC_BT709 : AVCOL_SPC_SMPTE170M;
  w->enc->color_primaries = hd ? AVCOL_PRI_BT709 : AVCOL_PRI_SMPTE170M;
  w->enc->color_trc    = hd ? AVCOL_TRC_BT709 : AVCOL_TRC_SMPTE170M;
  if ( w->fmt->oformat->flags & AVFMT_GLOBALHEADER )
    w->enc->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;

  int bitrate = spec->bitrate;
  if ( spec->live && bitrate <= 0 )	/* about 0.1 bits per pixel */
    bitrate = (int)(spec->width*spec->height*fps*0.1);
  set_quality(w->enc, codec, spec->quality, spec->live, bitrate);

  if ( (rc=avcodec_open2(w->enc, codec, NULL)) < 0 )
  { av_error(errmsg, codec->name, rc);
    avcodec_free_context(&w->enc);
    return false;
  }
  *pixp = pix;

  return true;
}


vb_writer *
vb_open_writer(const char *file, vb_wspec *spec,
	       SDL_PixelFormat *format, SDL_Colorspace *colorspace,
	       char *errmsg)
{ vb_writer *w = SDL_calloc(1, sizeof(*w));
  const char *target = file ? file : "stream";
  const AVCodec *codec;
  int rc;

  init_ffmpeg();
  if ( !w )
  { vb_error(errmsg, "%s", "out of memory");
    return NULL;
  }
  w->last_pts = AV_NOPTS_VALUE;
  if ( (rc=avformat_alloc_output_context2(&w->fmt, NULL,
					  target_format(file, spec),
					  file)) < 0 )
  { av_error(errmsg, target, rc);
    goto error;
  }
  if ( spec->live || !file )		/* do not keep data in a buffer */
    w->fmt->flush_packets = 1;
  double fps = spec->frame_rate > 0 ? spec->frame_rate : 30.0;
  bool hd = spec->height >= 720;
  enum AVPixelFormat pix;

  if ( spec->codec )
  { if ( !(codec = find_encoder(spec->codec, w->fmt->oformat, NULL)) )
    { vb_error(errmsg, "No encoder for %s", spec->codec);
      goto error;
    }
    if ( !open_video_encoder(w, codec, spec, fps, &pix, errmsg) )
      goto error;
  } else			/* the first of the candidates that opens */
  { codec = NULL;
    errmsg[0] = 0;
    for(const char **n = default_encoders; *n; n++)
    { const AVCodec *c = avcodec_find_encoder_by_name(*n);
      char err[VB_ERRLEN];

      if ( c && avformat_query_codec(w->fmt->oformat, c->id,
				     FF_COMPLIANCE_NORMAL) != 0 )
      { if ( open_video_encoder(w, c, spec, fps, &pix, err) )
	{ codec = c;
	  break;
	}
	if ( !errmsg[0] )		/* report the first failure */
	  strcpy(errmsg, err);
      }
    }
    if ( !codec )
    { if ( !errmsg[0] )
	vb_error(errmsg, "No encoder for %s", w->fmt->oformat->name);
      goto error;
    }
  }
  if ( !(w->stream = avformat_new_stream(w->fmt, NULL)) ||
       (rc=avcodec_parameters_from_context(w->stream->codecpar, w->enc)) < 0 )
  { vb_error(errmsg, "%s", "cannot create stream");
    goto error;
  }
  w->stream->time_base = w->enc->time_base;
  w->stream->avg_frame_rate = w->enc->framerate;
  w->frame_duration = llround(1000.0/fps);
  if ( spec->audio_rate > 0 && !open_audio(w, spec, errmsg) )
    goto error;
  if ( !file )
  { uint8_t *buf = av_malloc(AVIO_BUFSIZE);

    w->write = spec->write;
    w->closure = spec->closure;
    if ( !buf ||
	 !(w->avio = avio_alloc_context(buf, AVIO_BUFSIZE, 1, w, NULL,
					target_write, NULL)) )
    { av_free(buf);
      vb_error(errmsg, "%s", "out of memory");
      goto error;
    }
    w->avio->seekable = 0;
    w->fmt->pb = w->avio;
    w->fmt->flags |= AVFMT_FLAG_CUSTOM_IO;
  } else if ( !(w->fmt->oformat->flags & AVFMT_NOFILE) &&
	      (rc=avio_open(&w->fmt->pb, file, AVIO_FLAG_WRITE)) < 0 )
  { av_error(errmsg, file, rc);
    goto error;
  }

  AVDictionary *opts = header_options(w, file, spec);
  rc = avformat_write_header(w->fmt, &opts);
  av_dict_free(&opts);
  if ( rc < 0 )
  { av_error(errmsg, target, rc);
    goto error;
  }
  w->header = true;
  w->frame = av_frame_alloc();
  w->pkt   = av_packet_alloc();

  spec->codec = codec->name;
  *format = pix == AV_PIX_FMT_NV12 ? SDL_PIXELFORMAT_NV12
				   : SDL_PIXELFORMAT_IYUV;
  *colorspace = hd ? SDL_COLORSPACE_BT709_LIMITED
		   : SDL_COLORSPACE_BT601_LIMITED;

  return w;

error:
  vb_close_writer(w, errmsg);
  return NULL;
}


/* Write the packets the encoder has ready */

static bool
drain_encoder(vb_writer *w, AVCodecContext *enc, AVStream *st,
	      char *errmsg)
{ for(;;)
  { int rc = avcodec_receive_packet(enc, w->pkt);

    if ( rc == AVERROR(EAGAIN) || rc == AVERROR_EOF )
      return true;
    if ( rc < 0 )
    { av_error(errmsg, "encode", rc);
      return false;
    }
    av_packet_rescale_ts(w->pkt, enc->time_base, st->time_base);
    w->pkt->stream_index = st->index;
    rc = av_interleaved_write_frame(w->fmt, w->pkt);
    if ( rc < 0 )
    { av_error(errmsg, "write", rc);
      return false;
    }
  }
}


bool
vb_write_frame(vb_writer *w, SDL_Surface *s, double pts, char *errmsg)
{ AVFrame *f = w->frame;
  int64_t t = (int64_t)llround(pts*1000);
  int ch = (s->h+1)/2;

  if ( w->last_pts != AV_NOPTS_VALUE && t <= w->last_pts )
    t = w->last_pts+1;			/* pts must increase */
  w->last_pts = t;

  f->format = w->enc->pix_fmt;
  f->width  = s->w;
  f->height = s->h;
  f->pts    = t;
  f->duration = w->frame_duration;
  f->data[0]     = s->pixels;
  f->linesize[0] = s->pitch;
  if ( w->enc->pix_fmt == AV_PIX_FMT_NV12 )
  { f->data[1]     = (Uint8*)s->pixels + (size_t)s->pitch*s->h;
    f->linesize[1] = s->pitch;
  } else
  { int cpitch = (s->pitch+1)/2;

    f->data[1]     = (Uint8*)s->pixels + (size_t)s->pitch*s->h;
    f->data[2]     = f->data[1] + (size_t)cpitch*ch;
    f->linesize[1] = f->linesize[2] = cpitch;
  }

  int rc = avcodec_send_frame(w->enc, f);	/* copies the data */
  memset(f->data, 0, sizeof(f->data));
  if ( rc < 0 )
  { av_error(errmsg, "encode", rc);
    return false;
  }

  return drain_encoder(w, w->enc, w->stream, errmsg);
}


/* Encode the first n sample frames of pending */

static bool
encode_audio(vb_writer *w, int n, char *errmsg)
{ AVFrame *f = w->aframe;
  int rc;

  f->nb_samples  = n;
  f->format      = w->aenc->sample_fmt;
  f->sample_rate = w->aenc->sample_rate;
#if LIBAVUTIL_VERSION_INT >= AV_VERSION_INT(57,24,100)
  av_channel_layout_copy(&f->ch_layout, &w->aenc->ch_layout);
#else
  f->channels = w->channels;
  f->channel_layout = w->aenc->channel_layout;
#endif
  if ( (rc=av_frame_get_buffer(f, 0)) < 0 )
  { av_error(errmsg, "audio", rc);
    return false;
  }

  const float *in = w->pending;
  int ch = w->channels;

  switch(f->format)
  { case AV_SAMPLE_FMT_FLTP:
      for(int c=0; c<ch; c++)
      { float *out = (float*)f->extended_data[c];

	for(int i=0; i<n; i++)
	  out[i] = in[i*ch+c];
      }
      break;
    case AV_SAMPLE_FMT_FLT:
      memcpy(f->data[0], in, (size_t)n*ch*sizeof(float));
      break;
    case AV_SAMPLE_FMT_S16:
    { int16_t *out = (int16_t*)f->data[0];

      for(int i=0; i<n*ch; i++)
      { float v = in[i] < -1.0f ? -1.0f : in[i] > 1.0f ? 1.0f : in[i];

	out[i] = (int16_t)lrintf(v*32767.0f);
      }
      break;
    }
  }
  f->pts = w->asamples;
  w->asamples += n;

  rc = avcodec_send_frame(w->aenc, f);
  av_frame_unref(f);
  if ( rc < 0 )
  { av_error(errmsg, "encode audio", rc);
    return false;
  }
  w->npending -= n;
  memmove(w->pending, w->pending+(size_t)n*ch,
	  (size_t)w->npending*ch*sizeof(float));

  return drain_encoder(w, w->aenc, w->astream, errmsg);
}


bool
vb_write_audio(vb_writer *w, const float *samples, int frames, char *errmsg)
{ int ch = w->channels;

  if ( !w->aenc )
  { vb_error(errmsg, "%s", "the video has no audio");
    return false;
  }
  if ( w->npending+frames > w->apending )
  { int size = w->npending+frames+w->frame_size;
    float *p = SDL_realloc(w->pending, (size_t)size*ch*sizeof(float));

    if ( !p )
    { vb_error(errmsg, "%s", "out of memory");
      return false;
    }
    w->pending = p;
    w->apending = size;
  }
  memcpy(w->pending+(size_t)w->npending*ch, samples,
	 (size_t)frames*ch*sizeof(float));
  w->npending += frames;

  while( w->npending >= w->frame_size )
  { if ( !encode_audio(w, w->frame_size, errmsg) )
      return false;
  }

  return true;
}


bool
vb_close_writer(vb_writer *w, char *errmsg)
{ bool ok = true;

  if ( w->header )
  { if ( w->aenc )
    { if ( w->npending > 0 )		/* the last frame may be shorter */
	ok = encode_audio(w, w->npending, errmsg);
      avcodec_send_frame(w->aenc, NULL);
      ok = drain_encoder(w, w->aenc, w->astream, errmsg) && ok;
    }
    avcodec_send_frame(w->enc, NULL);
    ok = drain_encoder(w, w->enc, w->stream, errmsg) && ok;
    int rc = av_write_trailer(w->fmt);
    if ( ok && rc < 0 )
    { av_error(errmsg, "write", rc);
      ok = false;
    }
  }
  if ( w->avio )
  { avio_flush(w->avio);
    av_freep(&w->avio->buffer);
    avio_context_free(&w->avio);
    if ( w->fmt )
      w->fmt->pb = NULL;
  } else if ( w->fmt && !(w->fmt->oformat->flags & AVFMT_NOFILE) )
    avio_closep(&w->fmt->pb);
  av_frame_free(&w->frame);
  av_frame_free(&w->aframe);
  av_packet_free(&w->pkt);
  avcodec_free_context(&w->enc);
  avcodec_free_context(&w->aenc);
  SDL_free(w->pending);
  avformat_free_context(w->fmt);
  SDL_free(w);

  return ok;
}


		 /*******************************
		 *	    CAPABILITIES	*
		 *******************************/

static const enum AVCodecID common_codecs[] =
{ AV_CODEC_ID_H264, AV_CODEC_ID_HEVC, AV_CODEC_ID_AV1, AV_CODEC_ID_VP9,
  AV_CODEC_ID_VP8, AV_CODEC_ID_MPEG4, AV_CODEC_ID_MPEG2VIDEO,
  AV_CODEC_ID_PRORES, AV_CODEC_ID_MJPEG, AV_CODEC_ID_NONE
};

void
vb_codecs(bool encode, void (*f)(const char *name, void *closure),
	  void *closure)
{ for(const enum AVCodecID *id = common_codecs; *id != AV_CODEC_ID_NONE; id++)
  { bool ok = false;

    if ( encode )
    { for(const char **n = default_encoders; *n && !ok; n++)
      { const AVCodec *c = avcodec_find_encoder_by_name(*n);

	ok = ( c && c->id == *id );
      }
    } else
    { ok = avcodec_find_decoder(*id) != NULL;
    }
    if ( ok )
      f(avcodec_get_name(*id), closure);
  }
}
