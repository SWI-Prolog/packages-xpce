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

/* The Media Foundation backend for video on Windows (see
 * video_backend.h).  The source reader decodes to NV12 and float audio,
 * using the codecs of Windows, which are often hardware accelerated.
 * The sink writer encodes H.264 or HEVC with AAC audio into MP4.
 *
 * Media Foundation objects may be used from any thread.  Each thread
 * that uses them must have initialised COM, which ensure_com() does.
 * The GUI thread usually is a single-threaded apartment already, which
 * is fine.
 */

#define COBJMACROS
#undef _WIN32_WINNT			/* the helpers of mfapi.h need */
#define _WIN32_WINNT 0x0A00		/* Windows 7; we ask for 10 */
#undef WINVER
#define WINVER _WIN32_WINNT
#include <SDL3/SDL.h>
#include <windows.h>
#include <initguid.h>
#include <mfapi.h>
#include <mfidl.h>
#include <mfreadwrite.h>
#include <mferror.h>
#include <stdio.h>
#include <string.h>
#include "video_backend.h"

#define HNS_PER_SECOND 10000000.0	/* time unit: 100 nanoseconds */

static void
hr_error(char *errmsg, const char *what, HRESULT hr)
{ char buf[200];

  if ( !FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		       NULL, hr, 0, buf, sizeof(buf), NULL) )
    buf[0] = 0;
  else
  { size_t len = strlen(buf);		/* strip the newline */

    while( len > 0 && (buf[len-1] == '\n' || buf[len-1] == '\r') )
      buf[--len] = 0;
  }
  snprintf(errmsg, VB_ERRLEN, "%s: %s (0x%08lx)", what,
	   buf[0] ? buf : "error", (unsigned long)hr);
}

static void
vb_error(char *errmsg, const char *msg)
{ snprintf(errmsg, VB_ERRLEN, "%s", msg);
}

/* COM on this thread and Media Foundation for the process */

static bool
ensure_com(char *errmsg)
{ static SDL_InitState mf_init;
  HRESULT hr = CoInitializeEx(NULL, COINIT_MULTITHREADED);

  if ( FAILED(hr) && hr != RPC_E_CHANGED_MODE )
  { hr_error(errmsg, "CoInitializeEx", hr);
    return false;
  }
  if ( SDL_ShouldInit(&mf_init) )
  { hr = MFStartup(MF_VERSION, MFSTARTUP_FULL);
    SDL_SetInitialized(&mf_init, SUCCEEDED(hr));
    if ( FAILED(hr) )
    { hr_error(errmsg, "MFStartup", hr);
      return false;
    }
  }

  return true;
}

/* The UTF-16 name of a file or URL.  Media Foundation needs the
 * absolute name of a file.
 */

static wchar_t *
wide_name(const char *utf8)
{ int len = MultiByteToWideChar(CP_UTF8, 0, utf8, -1, NULL, 0);
  wchar_t *w = len > 0 ? SDL_malloc(len*sizeof(wchar_t)) : NULL;

  if ( !w )
    return NULL;
  MultiByteToWideChar(CP_UTF8, 0, utf8, -1, w, len);
  if ( strstr(utf8, "://") )
    return w;

  DWORD flen = GetFullPathNameW(w, 0, NULL, NULL);
  wchar_t *full = flen ? SDL_malloc(flen*sizeof(wchar_t)) : NULL;

  if ( full && GetFullPathNameW(w, flen, full, NULL) )
  { SDL_free(w);
    return full;
  }
  SDL_free(full);

  return w;
}

/* The attribute helpers of mfapi.h are only for C++.  A size or ratio is
 * a 64-bit attribute with the first value in the high 32 bits.
 */

static HRESULT
get_pair(IMFAttributes *a, REFGUID key, UINT32 *hi, UINT32 *lo)
{ UINT64 v;
  HRESULT hr = IMFAttributes_GetUINT64(a, key, &v);

  if ( SUCCEEDED(hr) )
  { *hi = (UINT32)(v>>32);
    *lo = (UINT32)v;
  }

  return hr;
}

static HRESULT
set_pair(IMFAttributes *a, REFGUID key, UINT32 hi, UINT32 lo)
{ return IMFAttributes_SetUINT64(a, key, ((UINT64)hi<<32)|lo);
}

static UINT32
get_uint32(IMFAttributes *a, REFGUID key, UINT32 def)
{ UINT32 v;

  return SUCCEEDED(IMFAttributes_GetUINT32(a, key, &v)) ? v : def;
}

#define MFGetAttributeSize(a, k, w, h)  get_pair(a, k, w, h)
#define MFGetAttributeRatio(a, k, n, d) get_pair(a, k, n, d)
#define MFSetAttributeSize(a, k, w, h)  set_pair(a, k, w, h)
#define MFSetAttributeRatio(a, k, n, d) set_pair(a, k, n, d)
#define MFGetAttributeUINT32(a, k, def) get_uint32(a, k, def)

#define RELEASE(p) do { if ( p ) { IUnknown_Release((IUnknown*)(p)); \
				   (p) = NULL; } } while(0)


		 /*******************************
		 *	       CODECS		*
		 *******************************/

typedef struct codec_name
{ const GUID   *subtype;
  const char   *name;
} codec_name;

static const codec_name video_codecs[] =
{ { &MFVideoFormat_H264,  "h264" },
  { &MFVideoFormat_HEVC,  "hevc" },
  { &MFVideoFormat_AV1,   "av1" },
  { &MFVideoFormat_VP90,  "vp9" },
  { &MFVideoFormat_VP80,  "vp8" },
  { &MFVideoFormat_MP4V,  "mpeg4" },
  { &MFVideoFormat_MPEG2, "mpeg2video" },
  { &MFVideoFormat_MJPG,  "mjpeg" },
  { &MFVideoFormat_WMV3,  "wmv3" },
  { NULL,		  NULL }
};

static const codec_name audio_codecs[] =
{ { &MFAudioFormat_AAC,   "aac" },
  { &MFAudioFormat_MP3,   "mp3" },
  { &MFAudioFormat_PCM,   "pcm" },
  { &MFAudioFormat_Float, "pcm_f32" },
  { &MFAudioFormat_WMAudioV8, "wma" },
  { NULL,		  NULL }
};

static const char *
codec_of(const codec_name *table, const GUID *subtype)
{ for(const codec_name *c = table; c->subtype; c++)
  { if ( IsEqualGUID(c->subtype, subtype) )
      return c->name;
  }

  return "unknown";
}


		 /*******************************
		 *	       READING		*
		 *******************************/

struct vb_reader
{ IMFSourceReader *reader;
  DWORD		  vstream;		/* index of the video stream */
  DWORD		  astream;		/* index of the audio stream */
  bool		  has_audio;
  bool		  video_done;
  bool		  audio_done;
  UINT32	  width;		/* size of the decoded frames */
  UINT32	  height;
  LONG		  stride;		/* bytes per row, 0: unknown */
  MFVideoArea	  aperture;		/* part of the frame shown */
  SDL_Colorspace  colorspace;
  int		  channels;
  float		 *samples;		/* audio of the last read */
  size_t	  asize;		/* allocated floats */
};


/* The index of the first stream of a major type, which we select */

static bool
find_stream(IMFSourceReader *r, const GUID *major, DWORD *index)
{ for(DWORD i=0; ; i++)
  { IMFMediaType *type = NULL;
    GUID m;
    HRESULT hr = IMFSourceReader_GetNativeMediaType(r, i, 0, &type);

    if ( FAILED(hr) )
      return false;			/* MF_E_INVALIDSTREAMNUMBER */
    hr = IMFMediaType_GetMajorType(type, &m);
    IMFMediaType_Release(type);
    if ( SUCCEEDED(hr) && IsEqualGUID(&m, major) )
    { *index = i;
      return true;
    }
  }
}


static const GUID *
native_subtype(IMFSourceReader *r, DWORD stream, GUID *buf)
{ IMFMediaType *type = NULL;
  const GUID *sub = NULL;

  if ( SUCCEEDED(IMFSourceReader_GetNativeMediaType(r, stream, 0, &type)) )
  { if ( SUCCEEDED(IMFMediaType_GetGUID(type, &MF_MT_SUBTYPE, buf)) )
      sub = buf;
    IMFMediaType_Release(type);
  }

  return sub;
}


/* Read the size, stride, visible area and colorspace of the decoded
 * video, also after the decoder changed them.
 */

static bool
video_format(vb_reader *r, char *errmsg)
{ IMFMediaType *type = NULL;
  HRESULT hr = IMFSourceReader_GetCurrentMediaType(r->reader, r->vstream,
						     &type);

  if ( FAILED(hr) )
  { hr_error(errmsg, "video format", hr);
    return false;
  }
  MFGetAttributeSize((IMFAttributes*)type, &MF_MT_FRAME_SIZE,
		     &r->width, &r->height);
  r->stride = (LONG)MFGetAttributeUINT32((IMFAttributes*)type,
					 &MF_MT_DEFAULT_STRIDE, 0);

  UINT32 size = 0;
  if ( FAILED(IMFMediaType_GetBlob(type, &MF_MT_MINIMUM_DISPLAY_APERTURE,
				   (UINT8*)&r->aperture, sizeof(r->aperture),
				   &size)) || size != sizeof(r->aperture) )
  { memset(&r->aperture, 0, sizeof(r->aperture));
    r->aperture.Area.cx = r->width;
    r->aperture.Area.cy = r->height;
  }

  UINT32 matrix = MFGetAttributeUINT32((IMFAttributes*)type, &MF_MT_YUV_MATRIX,
				       MFVideoTransferMatrix_Unknown);
  UINT32 range = MFGetAttributeUINT32((IMFAttributes*)type,
				      &MF_MT_VIDEO_NOMINAL_RANGE,
				      MFNominalRange_16_235);
  bool full = (range == MFNominalRange_0_255);
  bool hd = r->aperture.Area.cy >= 720;

  switch(matrix)
  { case MFVideoTransferMatrix_BT709:
      hd = true;
      break;
    case MFVideoTransferMatrix_BT601:
      hd = false;
      break;
    case MFVideoTransferMatrix_BT2020_10:
    case MFVideoTransferMatrix_BT2020_12:
      r->colorspace = full ? SDL_COLORSPACE_BT2020_FULL
			   : SDL_COLORSPACE_BT2020_LIMITED;
      IMFMediaType_Release(type);
      return true;
  }
  if ( hd )
    r->colorspace = full ? SDL_COLORSPACE_BT709_FULL
			 : SDL_COLORSPACE_BT709_LIMITED;
  else
    r->colorspace = full ? SDL_COLORSPACE_JPEG : SDL_COLORSPACE_BT601_LIMITED;

  IMFMediaType_Release(type);
  return true;
}


static bool
set_output_type(IMFSourceReader *r, DWORD stream, const GUID *major,
		const GUID *sub, char *errmsg)
{ IMFMediaType *type = NULL;
  HRESULT hr = MFCreateMediaType(&type);

  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetGUID(type, &MF_MT_MAJOR_TYPE, major);
  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetGUID(type, &MF_MT_SUBTYPE, sub);
  if ( SUCCEEDED(hr) )
    hr = IMFSourceReader_SetCurrentMediaType(r, stream, NULL, type);
  RELEASE(type);
  if ( FAILED(hr) )
  { hr_error(errmsg, "decoder", hr);
    return false;
  }

  return true;
}


vb_reader *
vb_open_reader(const vb_source *src, bool audio, vb_info *info,
	       char *errmsg)
{ const char *source = src->url;

  if ( !source )
  { vb_error(errmsg, "reading video from a stream is not supported");
    return NULL;
  }

  vb_reader *r = SDL_calloc(1, sizeof(*r));
  IMFAttributes *attrs = NULL;
  wchar_t *url = NULL;
  HRESULT hr;
  GUID sub;
  const GUID *subp;

  if ( !r )
  { vb_error(errmsg, "out of memory");
    return NULL;
  }
  if ( !ensure_com(errmsg) )
    goto error;
  if ( !(url = wide_name(source)) )
  { vb_error(errmsg, "out of memory");
    goto error;
  }

  hr = MFCreateAttributes(&attrs, 2);
  if ( SUCCEEDED(hr) )			/* convert to NV12 */
    hr = IMFAttributes_SetUINT32(attrs,
		&MF_SOURCE_READER_ENABLE_ADVANCED_VIDEO_PROCESSING, TRUE);
  if ( SUCCEEDED(hr) )
    hr = MFCreateSourceReaderFromURL(url, attrs, &r->reader);
  RELEASE(attrs);
  SDL_free(url);
  if ( FAILED(hr) )
  { hr_error(errmsg, source, hr);
    goto error;
  }

  if ( !find_stream(r->reader, &MFMediaType_Video, &r->vstream) )
  { vb_error(errmsg, "no video stream");
    goto error;
  }
  r->has_audio = audio && find_stream(r->reader, &MFMediaType_Audio,
				      &r->astream);

  IMFSourceReader_SetStreamSelection(r->reader,
				     (DWORD)MF_SOURCE_READER_ALL_STREAMS, FALSE);
  IMFSourceReader_SetStreamSelection(r->reader, r->vstream, TRUE);
  if ( !set_output_type(r->reader, r->vstream, &MFMediaType_Video,
			&MFVideoFormat_NV12, errmsg) ||
       !video_format(r, errmsg) )
    goto error;

  memset(info, 0, sizeof(*info));
  info->width  = r->aperture.Area.cx & ~1;
  info->height = r->aperture.Area.cy & ~1;
  subp = native_subtype(r->reader, r->vstream, &sub);
  info->codec = subp ? codec_of(video_codecs, subp) : "unknown";

  IMFMediaType *native = NULL;
  if ( SUCCEEDED(IMFSourceReader_GetNativeMediaType(r->reader, r->vstream, 0,
						    &native)) )
  { UINT32 num = 0, den = 0;

    if ( SUCCEEDED(MFGetAttributeRatio((IMFAttributes*)native,
				       &MF_MT_FRAME_RATE, &num, &den)) &&
	 den > 0 )
      info->frame_rate = (double)num/den;
    IMFMediaType_Release(native);
  }

  PROPVARIANT var;
  PropVariantInit(&var);
  if ( SUCCEEDED(IMFSourceReader_GetPresentationAttribute(
		   r->reader, (DWORD)MF_SOURCE_READER_MEDIASOURCE,
		   &MF_PD_DURATION, &var)) )
  { if ( var.vt == VT_UI8 )
      info->duration = var.uhVal.QuadPart/HNS_PER_SECOND;
    PropVariantClear(&var);
  }

  if ( r->has_audio )
  { IMFMediaType *type = NULL;

    IMFSourceReader_SetStreamSelection(r->reader, r->astream, TRUE);
    if ( set_output_type(r->reader, r->astream, &MFMediaType_Audio,
			 &MFAudioFormat_Float, errmsg) &&
	 SUCCEEDED(IMFSourceReader_GetCurrentMediaType(r->reader, r->astream,
						       &type)) )
    { info->audio_rate = MFGetAttributeUINT32((IMFAttributes*)type,
				&MF_MT_AUDIO_SAMPLES_PER_SECOND, 0);
      info->audio_channels = MFGetAttributeUINT32((IMFAttributes*)type,
				&MF_MT_AUDIO_NUM_CHANNELS, 0);
      r->channels = info->audio_channels;
      subp = native_subtype(r->reader, r->astream, &sub);
      info->audio_codec = subp ? codec_of(audio_codecs, subp) : "unknown";
      IMFMediaType_Release(type);
    }
    if ( info->audio_rate <= 0 || info->audio_channels <= 0 )
    { IMFSourceReader_SetStreamSelection(r->reader, r->astream, FALSE);
      r->has_audio = false;		/* play without sound */
      info->audio_rate = info->audio_channels = 0;
      info->audio_codec = NULL;
    }
  }
  r->audio_done = !r->has_audio;

  return r;

error:
  vb_close_reader(r);
  return NULL;
}


void
vb_close_reader(vb_reader *r)
{ if ( r )
  { RELEASE(r->reader);
    SDL_free(r->samples);
    SDL_free(r);
  }
}


/* Copy the visible part of an NV12 frame into *sp */

static bool
sample_to_surface(vb_reader *r, IMFSample *sample, SDL_Surface **sp,
		  char *errmsg)
{ IMFMediaBuffer *buf = NULL;
  IMF2DBuffer *buf2d = NULL;
  BYTE *data = NULL;
  LONG pitch = 0;
  DWORD len = 0;
  HRESULT hr = IMFSample_ConvertToContiguousBuffer(sample, &buf);
  bool ok = false;

  if ( FAILED(hr) )
  { hr_error(errmsg, "frame", hr);
    return false;
  }
  if ( SUCCEEDED(IMFMediaBuffer_QueryInterface(buf, &IID_IMF2DBuffer,
					       (void**)&buf2d)) &&
       SUCCEEDED(IMF2DBuffer_Lock2D(buf2d, &data, &pitch)) )
  { ;
  } else
  { RELEASE(buf2d);
    if ( FAILED(hr = IMFMediaBuffer_Lock(buf, &data, NULL, &len)) )
    { hr_error(errmsg, "frame", hr);
      goto out;
    }
    pitch = r->stride > 0 ? r->stride : (LONG)r->width;
  }

  int w = r->aperture.Area.cx & ~1;
  int h = r->aperture.Area.cy & ~1;
  int x0 = r->aperture.OffsetX.value & ~1;
  int y0 = r->aperture.OffsetY.value & ~1;
  SDL_Surface *s = *sp;

  if ( s && (s->w != w || s->h != h || s->format != SDL_PIXELFORMAT_NV12) )
  { SDL_DestroySurface(s);
    *sp = s = NULL;
  }
  if ( !s && !(*sp = s = SDL_CreateSurface(w, h, SDL_PIXELFORMAT_NV12)) )
  { vb_error(errmsg, SDL_GetError());
  } else
  { const BYTE *y  = data + (size_t)y0*pitch + x0;
    const BYTE *uv = data + (size_t)r->height*pitch + (size_t)(y0/2)*pitch + x0;
    Uint8 *out = s->pixels;

    for(int i=0; i<h; i++)
      memcpy(out+(size_t)i*s->pitch, y+(size_t)i*pitch, w);
    out += (size_t)s->pitch*h;
    for(int i=0; i<h/2; i++)
      memcpy(out+(size_t)i*s->pitch, uv+(size_t)i*pitch, w);
    SDL_SetSurfaceColorspace(s, r->colorspace);
    ok = true;
  }

  if ( buf2d )
    IMF2DBuffer_Unlock2D(buf2d);
  else
    IMFMediaBuffer_Unlock(buf);

out:
  RELEASE(buf2d);
  RELEASE(buf);
  return ok;
}


static bool
sample_to_samples(vb_reader *r, IMFSample *sample, vb_samples *out,
		  char *errmsg)
{ IMFMediaBuffer *buf = NULL;
  BYTE *data;
  DWORD len;
  HRESULT hr = IMFSample_ConvertToContiguousBuffer(sample, &buf);

  if ( SUCCEEDED(hr) )
    hr = IMFMediaBuffer_Lock(buf, &data, NULL, &len);
  if ( FAILED(hr) )
  { hr_error(errmsg, "audio", hr);
    RELEASE(buf);
    return false;
  }

  size_t floats = len/sizeof(float);
  bool ok = true;

  if ( floats > r->asize )
  { float *s = SDL_realloc(r->samples, floats*sizeof(float));

    if ( s )
    { r->samples = s;
      r->asize = floats;
    } else
    { vb_error(errmsg, "out of memory");
      ok = false;
    }
  }
  if ( ok )
  { memcpy(r->samples, data, floats*sizeof(float));
    out->data = r->samples;
    out->frames = (int)(floats/r->channels);
  }
  IMFMediaBuffer_Unlock(buf);
  RELEASE(buf);

  return ok;
}


vb_status
vb_read(vb_reader *r, SDL_Surface **frame, vb_samples *samples,
	double *pts, char *errmsg)
{ for(;;)
  { DWORD index, flags = 0;
    LONGLONG time = 0;
    IMFSample *sample = NULL;

    if ( r->video_done && r->audio_done )
      return VB_EOF;

    HRESULT hr = IMFSourceReader_ReadSample(
		   r->reader, (DWORD)MF_SOURCE_READER_ANY_STREAM, 0,
		   &index, &flags, &time, &sample);
    if ( FAILED(hr) )
    { hr_error(errmsg, "decode", hr);
      return VB_ERROR;
    }
    if ( flags & MF_SOURCE_READERF_ERROR )
    { vb_error(errmsg, "decode: stream error");
      RELEASE(sample);
      return VB_ERROR;
    }
    if ( (flags & MF_SOURCE_READERF_CURRENTMEDIATYPECHANGED) &&
	 index == r->vstream && !video_format(r, errmsg) )
    { RELEASE(sample);
      return VB_ERROR;
    }
    if ( flags & MF_SOURCE_READERF_ENDOFSTREAM )
    { if ( index == r->vstream )
	r->video_done = true;
      else if ( r->has_audio && index == r->astream )
	r->audio_done = true;
    }
    if ( !sample )
      continue;

    bool ok;
    vb_status rc;

    *pts = time/HNS_PER_SECOND;
    if ( index == r->vstream )
    { ok = sample_to_surface(r, sample, frame, errmsg);
      rc = VB_FRAME;
    } else if ( r->has_audio && index == r->astream )
    { ok = sample_to_samples(r, sample, samples, errmsg);
      rc = VB_AUDIO;
    } else
    { IMFSample_Release(sample);
      continue;
    }
    IMFSample_Release(sample);

    return ok ? rc : VB_ERROR;
  }
}


bool
vb_seek(vb_reader *r, double t, char *errmsg)
{ PROPVARIANT var;
  HRESULT hr;

  PropVariantInit(&var);
  var.vt = VT_I8;
  var.hVal.QuadPart = (LONGLONG)(t*HNS_PER_SECOND);
  static const GUID time_format = {0};	/* GUID_NULL: 100ns units */

  hr = IMFSourceReader_SetCurrentPosition(r->reader, &time_format, &var);
  PropVariantClear(&var);
  if ( FAILED(hr) )
  { hr_error(errmsg, "seek", hr);
    return false;
  }
  r->video_done = false;
  r->audio_done = !r->has_audio;

  return true;
}


		 /*******************************
		 *	       WRITING		*
		 *******************************/

struct vb_writer
{ IMFSinkWriter  *writer;
  DWORD		  vstream;
  DWORD		  astream;
  bool		  has_audio;
  bool		  writing;		/* BeginWriting() succeeded */
  int		  width;
  int		  height;
  LONGLONG	  frame_duration;	/* 100ns units */
  LONGLONG	  last_time;		/* time of the last frame */
  int		  channels;
  int		  rate;
  UINT64	  asamples;		/* samples written */
};


static HRESULT
video_type(IMFMediaType **tp, const GUID *sub, int w, int h, double fps,
	   UINT32 bitrate)
{ IMFMediaType *t = NULL;
  UINT32 num = (UINT32)(fps*1000+0.5), den = 1000;
  HRESULT hr = MFCreateMediaType(&t);

  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetGUID(t, &MF_MT_MAJOR_TYPE, &MFMediaType_Video);
  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetGUID(t, &MF_MT_SUBTYPE, sub);
  if ( SUCCEEDED(hr) && bitrate )
    hr = IMFMediaType_SetUINT32(t, &MF_MT_AVG_BITRATE, bitrate);
  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetUINT32(t, &MF_MT_INTERLACE_MODE,
				MFVideoInterlace_Progressive);
  if ( SUCCEEDED(hr) )
    hr = MFSetAttributeSize((IMFAttributes*)t, &MF_MT_FRAME_SIZE, w, h);
  if ( SUCCEEDED(hr) )
    hr = MFSetAttributeRatio((IMFAttributes*)t, &MF_MT_FRAME_RATE, num, den);
  if ( SUCCEEDED(hr) )
    hr = MFSetAttributeRatio((IMFAttributes*)t, &MF_MT_PIXEL_ASPECT_RATIO,
			     1, 1);
  if ( SUCCEEDED(hr) && h >= 720 )
    hr = IMFMediaType_SetUINT32(t, &MF_MT_YUV_MATRIX,
				MFVideoTransferMatrix_BT709);
  if ( FAILED(hr) )
    RELEASE(t);
  *tp = t;

  return hr;
}


/* The rate the AAC encoder supports that is closest to `rate` */

static int
aac_rate(int rate)
{ return rate <= 44100 ? 44100 : 48000;
}

static HRESULT
audio_type(IMFMediaType **tp, const GUID *sub, int rate, int channels)
{ IMFMediaType *t = NULL;
  HRESULT hr = MFCreateMediaType(&t);

  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetGUID(t, &MF_MT_MAJOR_TYPE, &MFMediaType_Audio);
  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetGUID(t, &MF_MT_SUBTYPE, sub);
  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetUINT32(t, &MF_MT_AUDIO_BITS_PER_SAMPLE, 16);
  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetUINT32(t, &MF_MT_AUDIO_SAMPLES_PER_SECOND, rate);
  if ( SUCCEEDED(hr) )
    hr = IMFMediaType_SetUINT32(t, &MF_MT_AUDIO_NUM_CHANNELS, channels);
  if ( SUCCEEDED(hr) && IsEqualGUID(sub, &MFAudioFormat_AAC) )
    hr = IMFMediaType_SetUINT32(t, &MF_MT_AUDIO_AVG_BYTES_PER_SECOND, 24000);
  if ( SUCCEEDED(hr) && IsEqualGUID(sub, &MFAudioFormat_PCM) )
  { hr = IMFMediaType_SetUINT32(t, &MF_MT_AUDIO_BLOCK_ALIGNMENT, 2*channels);
    if ( SUCCEEDED(hr) )
      hr = IMFMediaType_SetUINT32(t, &MF_MT_AUDIO_AVG_BYTES_PER_SECOND,
				  2*channels*rate);
  }
  if ( FAILED(hr) )
    RELEASE(t);
  *tp = t;

  return hr;
}


vb_writer *
vb_open_writer(const char *file, vb_wspec *spec,
	       SDL_PixelFormat *format, SDL_Colorspace *colorspace,
	       char *errmsg)
{
  if ( !file || (spec->format && strcmp(spec->format, "mp4") != 0 &&
		 strcmp(spec->format, "mov") != 0) )
  { vb_error(errmsg, "writing to a stream or this format is not supported");
    return NULL;
  }

  vb_writer *w = SDL_calloc(1, sizeof(*w));
  IMFAttributes *attrs = NULL;
  IMFMediaType *out = NULL, *in = NULL;
  wchar_t *url = NULL;
  const GUID *codec = &MFVideoFormat_H264;
  HRESULT hr;

  if ( !w )
  { vb_error(errmsg, "out of memory");
    return NULL;
  }
  if ( !ensure_com(errmsg) )
    goto error;
  if ( spec->codec )
  { if ( strcmp(spec->codec, "hevc") == 0 || strcmp(spec->codec, "h265") == 0 )
      codec = &MFVideoFormat_HEVC;
    else if ( strcmp(spec->codec, "h264") != 0 )
    { snprintf(errmsg, VB_ERRLEN, "No encoder for %s", spec->codec);
      goto error;
    }
  }
  if ( !(url = wide_name(file)) )
  { vb_error(errmsg, "out of memory");
    goto error;
  }

  double fps = spec->frame_rate > 0 ? spec->frame_rate : 30.0;
  double bpp = spec->quality >= 0 ? 0.03 + spec->quality*0.0027 : 0.12;
  UINT32 bitrate = (UINT32)(spec->width*spec->height*fps*bpp);

  w->width = spec->width;
  w->height = spec->height;
  w->frame_duration = (LONGLONG)(HNS_PER_SECOND/fps);
  w->last_time = -1;

  hr = MFCreateAttributes(&attrs, 1);
  if ( SUCCEEDED(hr) )
    hr = IMFAttributes_SetUINT32(attrs, &MF_READWRITE_ENABLE_HARDWARE_TRANSFORMS,
				 TRUE);
  if ( SUCCEEDED(hr) )
    hr = MFCreateSinkWriterFromURL(url, NULL, attrs, &w->writer);
  RELEASE(attrs);
  SDL_free(url);
  if ( FAILED(hr) )
  { hr_error(errmsg, file, hr);
    goto error;
  }

  if ( FAILED(hr = video_type(&out, codec, w->width, w->height, fps,
			      bitrate)) ||
       FAILED(hr = IMFSinkWriter_AddStream(w->writer, out, &w->vstream)) ||
       FAILED(hr = video_type(&in, &MFVideoFormat_NV12, w->width, w->height,
			      fps, 0)) ||
       FAILED(hr = IMFSinkWriter_SetInputMediaType(w->writer, w->vstream,
						    in, NULL)) )
  { hr_error(errmsg, "video encoder", hr);
    goto error;
  }
  RELEASE(out);
  RELEASE(in);

  if ( spec->audio_rate > 0 )
  { w->rate = aac_rate(spec->audio_rate);
    w->channels = spec->audio_channels > 2 ? 2 : spec->audio_channels;
    if ( FAILED(hr = audio_type(&out, &MFAudioFormat_AAC, w->rate,
				w->channels)) ||
	 FAILED(hr = IMFSinkWriter_AddStream(w->writer, out, &w->astream)) ||
	 FAILED(hr = audio_type(&in, &MFAudioFormat_PCM, w->rate,
				w->channels)) ||
	 FAILED(hr = IMFSinkWriter_SetInputMediaType(w->writer, w->astream,
						      in, NULL)) )
    { hr_error(errmsg, "audio encoder", hr);
      goto error;
    }
    RELEASE(out);
    RELEASE(in);
    w->has_audio = true;
    spec->audio_rate = w->rate;
    spec->audio_channels = w->channels;
    spec->audio_codec = "aac";
  }

  if ( FAILED(hr = IMFSinkWriter_BeginWriting(w->writer)) )
  { hr_error(errmsg, file, hr);
    goto error;
  }
  w->writing = true;

  spec->codec = codec == &MFVideoFormat_HEVC ? "hevc" : "h264";
  *format = SDL_PIXELFORMAT_NV12;
  *colorspace = w->height >= 720 ? SDL_COLORSPACE_BT709_LIMITED
				 : SDL_COLORSPACE_BT601_LIMITED;

  return w;

error:
  RELEASE(out);
  RELEASE(in);
  vb_close_writer(w, errmsg);
  return NULL;
}


static bool
write_buffer(vb_writer *w, DWORD stream, const void *data, size_t len,
	     LONGLONG time, LONGLONG duration, char *errmsg)
{ IMFMediaBuffer *buf = NULL;
  IMFSample *sample = NULL;
  BYTE *p;
  HRESULT hr = MFCreateMemoryBuffer((DWORD)len, &buf);

  if ( SUCCEEDED(hr) && SUCCEEDED(hr = IMFMediaBuffer_Lock(buf, &p, NULL,
							   NULL)) )
  { memcpy(p, data, len);
    IMFMediaBuffer_Unlock(buf);
    hr = IMFMediaBuffer_SetCurrentLength(buf, (DWORD)len);
  }
  if ( SUCCEEDED(hr) )
    hr = MFCreateSample(&sample);
  if ( SUCCEEDED(hr) )
    hr = IMFSample_AddBuffer(sample, buf);
  if ( SUCCEEDED(hr) )
    hr = IMFSample_SetSampleTime(sample, time);
  if ( SUCCEEDED(hr) )
    hr = IMFSample_SetSampleDuration(sample, duration);
  if ( SUCCEEDED(hr) )
    hr = IMFSinkWriter_WriteSample(w->writer, stream, sample);
  RELEASE(sample);
  RELEASE(buf);
  if ( FAILED(hr) )
  { hr_error(errmsg, "encode", hr);
    return false;
  }

  return true;
}


bool
vb_write_frame(vb_writer *w, SDL_Surface *s, double pts, char *errmsg)
{ int wd = w->width, h = w->height;
  size_t len = (size_t)wd*h*3/2;
  Uint8 *nv12 = SDL_malloc(len);	/* contiguous, pitch = width */
  LONGLONG time = (LONGLONG)(pts*HNS_PER_SECOND);
  bool ok;

  if ( !nv12 )
  { vb_error(errmsg, "out of memory");
    return false;
  }
  for(int y=0; y<h; y++)
    memcpy(nv12+(size_t)y*wd, (Uint8*)s->pixels+(size_t)y*s->pitch, wd);
  for(int y=0; y<h/2; y++)
    memcpy(nv12+(size_t)(h+y)*wd,
	   (Uint8*)s->pixels+(size_t)(h+y)*s->pitch, wd);

  if ( time <= w->last_time )		/* times must increase */
    time = w->last_time+1;
  w->last_time = time;
  ok = write_buffer(w, w->vstream, nv12, len, time, w->frame_duration,
		    errmsg);
  SDL_free(nv12);

  return ok;
}


bool
vb_write_audio(vb_writer *w, const float *samples, int frames, char *errmsg)
{ if ( !w->has_audio )
  { vb_error(errmsg, "the video has no audio");
    return false;
  }

  size_t n = (size_t)frames*w->channels;
  Sint16 *pcm = SDL_malloc(n*sizeof(Sint16));

  if ( !pcm )
  { vb_error(errmsg, "out of memory");
    return false;
  }
  for(size_t i=0; i<n; i++)
  { float v = samples[i] < -1.0f ? -1.0f : samples[i] > 1.0f ? 1.0f
							    : samples[i];
    pcm[i] = (Sint16)(v*32767.0f);
  }

  LONGLONG time = (LONGLONG)(w->asamples*HNS_PER_SECOND/w->rate);
  LONGLONG duration = (LONGLONG)(frames*HNS_PER_SECOND/w->rate);
  bool ok = write_buffer(w, w->astream, pcm, n*sizeof(Sint16), time,
			 duration, errmsg);

  w->asamples += frames;
  SDL_free(pcm);

  return ok;
}


bool
vb_close_writer(vb_writer *w, char *errmsg)
{ bool ok = true;

  if ( w->writing )
  { HRESULT hr = IMFSinkWriter_Finalize(w->writer);

    if ( FAILED(hr) )
    { hr_error(errmsg, "finish", hr);
      ok = false;
    }
  }
  RELEASE(w->writer);
  SDL_free(w);

  return ok;
}


		 /*******************************
		 *	    CAPABILITIES	*
		 *******************************/

/* True if there is a transform of `category` from or to `subtype` */

static bool
have_transform(const GUID *category, const GUID *subtype, bool encode)
{ MFT_REGISTER_TYPE_INFO info = { MFMediaType_Video, *subtype };
  IMFActivate **acts = NULL;
  UINT32 count = 0;
  HRESULT hr = MFTEnumEx(*category,
			 MFT_ENUM_FLAG_ALL & ~MFT_ENUM_FLAG_FIELDOFUSE,
			 encode ? NULL : &info, encode ? &info : NULL,
			 &acts, &count);

  if ( FAILED(hr) )
    return false;
  for(UINT32 i=0; i<count; i++)
    IMFActivate_Release(acts[i]);
  CoTaskMemFree(acts);

  return count > 0;
}

void
vb_codecs(bool encode, void (*f)(const char *name, void *closure),
	  void *closure)
{ char errmsg[VB_ERRLEN];

  if ( !ensure_com(errmsg) )
    return;

  for(const codec_name *c = video_codecs; c->subtype; c++)
  { if ( encode && !IsEqualGUID(c->subtype, &MFVideoFormat_H264) &&
		   !IsEqualGUID(c->subtype, &MFVideoFormat_HEVC) )
      continue;				/* we only write these */
    if ( have_transform(encode ? &MFT_CATEGORY_VIDEO_ENCODER
			       : &MFT_CATEGORY_VIDEO_DECODER,
			c->subtype, encode) )
      f(c->name, closure);
  }
}
