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

/* The AVFoundation backend for video on macOS (see video_backend.h).
 * AVAssetReader decodes to NV12 and float audio, using VideoToolbox,
 * which is usually hardware accelerated.  AVAssetWriter encodes H.264
 * or HEVC with AAC audio into MP4 or QuickTime.
 *
 * This file must not include xpce headers: in Objective-C `Class' is a
 * builtin type.  It is compiled without ARC.  The functions are called
 * from the worker threads of sdlvideo.c, so each wraps its body in an
 * @autoreleasepool.
 *
 * We use the synchronous property access of AVAsset, which is
 * deprecated since macOS 13, because we support macOS 10.15 and the
 * interface is synchronous anyway.
 */

#include <SDL3/SDL.h>
#import <AVFoundation/AVFoundation.h>
#import <CoreMedia/CoreMedia.h>
#import <CoreVideo/CoreVideo.h>
#import <VideoToolbox/VideoToolbox.h>
#include <stdio.h>
#include <string.h>
#include "video_backend.h"

#pragma clang diagnostic ignored "-Wdeprecated-declarations"

static void
vb_error(char *errmsg, const char *msg)
{ snprintf(errmsg, VB_ERRLEN, "%s", msg);
}

static void
ns_error(char *errmsg, const char *what, NSError *error)
{ if ( !error )
  { snprintf(errmsg, VB_ERRLEN, "%s: unknown error", what);
  } else
  { NSString *reason = error.localizedFailureReason;

    if ( reason )
      snprintf(errmsg, VB_ERRLEN, "%s: %s (%s)", what,
	       error.localizedDescription.UTF8String, reason.UTF8String);
    else
      snprintf(errmsg, VB_ERRLEN, "%s: %s", what,
	       error.localizedDescription.UTF8String);
  }
}

static NSURL *
source_url(const char *source)
{ NSString *s = [NSString stringWithUTF8String:source];

  if ( !s )
    return nil;
  if ( strstr(source, "://") )
    return [NSURL URLWithString:s];

  return [NSURL fileURLWithPath:s];
}

/* A FourCC as a 32-bit value, also usable in a switch */

#define FOURCC(a,b,c,d) (((FourCharCode)(a)<<24)|((FourCharCode)(b)<<16)| \
			 ((FourCharCode)(c)<<8)|(FourCharCode)(d))


		 /*******************************
		 *	       CODECS		*
		 *******************************/

static const char *
video_codec_name(FourCharCode fcc)
{ switch(fcc)
  { case FOURCC('a','v','c','1'):
    case FOURCC('a','v','c','3'):
      return "h264";
    case FOURCC('h','v','c','1'):
    case FOURCC('h','e','v','1'):
      return "hevc";
    case FOURCC('a','v','0','1'):
      return "av1";
    case FOURCC('v','p','0','9'):
      return "vp9";
    case FOURCC('m','p','4','v'):
      return "mpeg4";
    case FOURCC('j','p','e','g'):
      return "mjpeg";
  }
  if ( (fcc>>16) == ((FOURCC('a','p','4','x')>>16)) ||  /* ap4h, ap4x */
       (fcc>>8)  == ((FOURCC('a','p','c','n')>>8)) )    /* apcn, apch, ... */
    return "prores";

  return "unknown";
}

static const char *
audio_codec_name(FourCharCode fcc)
{ switch(fcc)
  { case kAudioFormatMPEG4AAC:
    case kAudioFormatMPEG4AAC_HE:
    case kAudioFormatMPEG4AAC_HE_V2:
      return "aac";
    case kAudioFormatMPEGLayer3:
      return "mp3";
    case kAudioFormatAppleLossless:
      return "alac";
    case kAudioFormatAC3:
      return "ac3";
    case kAudioFormatOpus:
      return "opus";
    case kAudioFormatLinearPCM:
      return "pcm";
  }

  return "unknown";
}

static FourCharCode
track_fourcc(AVAssetTrack *track)
{ NSArray *descs = track.formatDescriptions;

  if ( descs.count > 0 )
    return CMFormatDescriptionGetMediaSubType(
	     (CMFormatDescriptionRef)descs[0]);

  return 0;
}


		 /*******************************
		 *	       READING		*
		 *******************************/

struct vb_reader
{ AVURLAsset	   *asset;
  AVAssetTrack	   *vtrack;
  AVAssetTrack	   *atrack;		/* nil: no audio */
  AVAssetReader	   *reader;		/* nil: seeked beyond the end */
  AVAssetReaderTrackOutput *vout;
  AVAssetReaderTrackOutput *aout;
  OSType	    pixel_format;	/* NV12, video or full range */
  int		    width;		/* size of the frames */
  int		    height;
  int		    channels;		/* audio channels we ask for */
  int		    rate;		/* audio sample rate */
  CMSampleBufferRef vpending;		/* next video sample */
  CMSampleBufferRef apending;		/* next audio sample */
  bool		    video_done;
  bool		    audio_done;
  float		   *samples;		/* audio of the last read */
  size_t	    asize;		/* allocated bytes */
};


static void
release_pending(vb_reader *r)
{ if ( r->vpending )
  { CFRelease(r->vpending);
    r->vpending = NULL;
  }
  if ( r->apending )
  { CFRelease(r->apending);
    r->apending = NULL;
  }
}


static void
stop_reader(vb_reader *r)
{ release_pending(r);
  if ( r->reader )
  { if ( r->reader.status == AVAssetReaderStatusReading )
      [r->reader cancelReading];
    [r->reader release];
    [r->vout release];
    [r->aout release];
    r->reader = nil;
    r->vout = nil;
    r->aout = nil;
  }
}


/* Create a reader that starts at `t` seconds.  An AVAssetReader cannot
 * seek, so seeking creates a new one.
 */

static bool
start_reader(vb_reader *r, double t, char *errmsg)
{ NSError *error = nil;
  CMTime duration = r->asset.duration;
  CMTime start = CMTimeMakeWithSeconds(t > 0 ? t : 0, 90000);

  stop_reader(r);
  if ( t > 0 && CMTIME_IS_NUMERIC(duration) &&
       CMTimeCompare(start, duration) >= 0 )
  { r->video_done = r->audio_done = true; /* nothing left */
    return true;
  }

  r->reader = [[AVAssetReader alloc] initWithAsset:r->asset error:&error];
  if ( !r->reader )
  { ns_error(errmsg, "reader", error);
    return false;
  }
  if ( t > 0 )
    r->reader.timeRange = CMTimeRangeMake(start, kCMTimePositiveInfinity);

  NSDictionary *vsettings =
    @{ (id)kCVPixelBufferPixelFormatTypeKey: @(r->pixel_format) };
  r->vout = [[AVAssetReaderTrackOutput alloc] initWithTrack:r->vtrack
					      outputSettings:vsettings];
  r->vout.alwaysCopiesSampleData = NO;
  if ( ![r->reader canAddOutput:r->vout] )
  { vb_error(errmsg, "cannot decode the video");
    goto error;
  }
  [r->reader addOutput:r->vout];

  if ( r->atrack )
  { NSMutableDictionary *asettings =
      [NSMutableDictionary dictionaryWithDictionary:
	@{ AVFormatIDKey:		@(kAudioFormatLinearPCM),
	   AVLinearPCMBitDepthKey:	@32,
	   AVLinearPCMIsFloatKey:	@YES,
	   AVLinearPCMIsBigEndianKey:	@NO,
	   AVLinearPCMIsNonInterleaved: @NO,
	   AVSampleRateKey:		@(r->rate),
	   AVNumberOfChannelsKey:	@(r->channels)
	 }];

    if ( r->channels == 2 )		/* also downmixes surround sound */
    { AudioChannelLayout layout = {0};

      layout.mChannelLayoutTag = kAudioChannelLayoutTag_Stereo;
      asettings[AVChannelLayoutKey] = [NSData dataWithBytes:&layout
						     length:sizeof(layout)];
    }
    r->aout = [[AVAssetReaderTrackOutput alloc] initWithTrack:r->atrack
						outputSettings:asettings];
    r->aout.alwaysCopiesSampleData = NO;
    if ( ![r->reader canAddOutput:r->aout] )
    { vb_error(errmsg, "cannot decode the audio");
      goto error;
    }
    [r->reader addOutput:r->aout];
  }

  if ( ![r->reader startReading] )
  { ns_error(errmsg, "reader", r->reader.error);
    goto error;
  }
  r->video_done = false;
  r->audio_done = (r->atrack == nil);

  return true;

error:
  stop_reader(r);
  return false;
}


vb_reader *
vb_open_reader(const vb_source *src, bool audio, vb_info *info,
	       char *errmsg)
{ const char *source = src->url;

  if ( !source )
  { vb_error(errmsg, "reading video from a stream is not supported");
    return NULL;
  }

  @autoreleasepool
  { vb_reader *r = SDL_calloc(1, sizeof(*r));
    NSURL *url = source_url(source);

    if ( !r )
    { vb_error(errmsg, "out of memory");
      return NULL;
    }
    if ( !url )
    { snprintf(errmsg, VB_ERRLEN, "%s: invalid name", source);
      goto error;
    }

    r->asset = [[AVURLAsset alloc] initWithURL:url options:
		 @{ AVURLAssetPreferPreciseDurationAndTimingKey: @YES }];
    NSArray *vtracks = [r->asset tracksWithMediaType:AVMediaTypeVideo];
    if ( vtracks.count == 0 )
    { NSError *error = nil;

      if ( [r->asset statusOfValueForKey:@"tracks" error:&error] ==
	   AVKeyValueStatusFailed )
	ns_error(errmsg, source, error);
      else if ( !r->asset.readable )
	snprintf(errmsg, VB_ERRLEN, "%s: cannot read", source);
      else
	vb_error(errmsg, "no video stream");
      goto error;
    }
    r->vtrack = [vtracks[0] retain];

    memset(info, 0, sizeof(*info));
    CGSize size = r->vtrack.naturalSize;
    r->width  = info->width  = (int)size.width  & ~1;
    r->height = info->height = (int)size.height & ~1;
    info->frame_rate = r->vtrack.nominalFrameRate;
    CMTime duration = r->asset.duration;
    if ( CMTIME_IS_NUMERIC(duration) )
      info->duration = CMTimeGetSeconds(duration);
    info->codec = video_codec_name(track_fourcc(r->vtrack));

    r->pixel_format = kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange;
    NSArray *descs = r->vtrack.formatDescriptions;
    if ( descs.count > 0 )
    { CFTypeRef full = CMFormatDescriptionGetExtension(
			 (CMFormatDescriptionRef)descs[0],
			 kCMFormatDescriptionExtension_FullRangeVideo);
      if ( full && CFEqual(full, kCFBooleanTrue) )
	r->pixel_format = kCVPixelFormatType_420YpCbCr8BiPlanarFullRange;
    }

    if ( audio )
    { NSArray *atracks = [r->asset tracksWithMediaType:AVMediaTypeAudio];
      AVAssetTrack *atrack = atracks.count > 0 ? atracks[0] : nil;
      NSArray *adescs = atrack.formatDescriptions;
      const AudioStreamBasicDescription *asbd =
	adescs.count > 0
	  ? CMAudioFormatDescriptionGetStreamBasicDescription(
	      (CMAudioFormatDescriptionRef)adescs[0])
	  : NULL;

      if ( asbd && asbd->mSampleRate > 0 && asbd->mChannelsPerFrame > 0 )
      { r->atrack   = [atrack retain];
	r->rate     = (int)asbd->mSampleRate;
	r->channels = asbd->mChannelsPerFrame > 2 ? 2
						  : (int)asbd->mChannelsPerFrame;
	info->audio_rate     = r->rate;
	info->audio_channels = r->channels;
	info->audio_codec    = audio_codec_name(asbd->mFormatID);
      }					/* else play without sound */
    }

    if ( !start_reader(r, 0.0, errmsg) )
      goto error;

    return r;

  error:
    vb_close_reader(r);
    return NULL;
  }
}


void
vb_close_reader(vb_reader *r)
{ @autoreleasepool
  { if ( r )
    { stop_reader(r);
      [r->vtrack release];
      [r->atrack release];
      [r->asset release];
      SDL_free(r->samples);
      SDL_free(r);
    }
  }
}


/* The colorspace of a frame from its attachments.  Without, BT.709 is
 * used for HD and BT.601 for SD.
 */

static SDL_Colorspace
frame_colorspace(CVPixelBufferRef pb, int height)
{ CFTypeRef matrix = CVBufferGetAttachment(pb, kCVImageBufferYCbCrMatrixKey,
					   NULL);
  bool full = (CVPixelBufferGetPixelFormatType(pb) ==
	       kCVPixelFormatType_420YpCbCr8BiPlanarFullRange);
  bool hd = height >= 720;

  if ( matrix )
  { if ( CFEqual(matrix, kCVImageBufferYCbCrMatrix_ITU_R_709_2) )
      hd = true;
    else if ( CFEqual(matrix, kCVImageBufferYCbCrMatrix_ITU_R_601_4) ||
	      CFEqual(matrix, kCVImageBufferYCbCrMatrix_SMPTE_240M_1995) )
      hd = false;
    else if ( CFEqual(matrix, kCVImageBufferYCbCrMatrix_ITU_R_2020) )
      return full ? SDL_COLORSPACE_BT2020_FULL : SDL_COLORSPACE_BT2020_LIMITED;
  }
  if ( hd )
    return full ? SDL_COLORSPACE_BT709_FULL : SDL_COLORSPACE_BT709_LIMITED;

  return full ? SDL_COLORSPACE_JPEG : SDL_COLORSPACE_BT601_LIMITED;
}


/* Copy the two planes of an NV12 pixel buffer into *sp */

static bool
sample_to_surface(vb_reader *r, CMSampleBufferRef sample, SDL_Surface **sp,
		  char *errmsg)
{ CVPixelBufferRef pb = CMSampleBufferGetImageBuffer(sample);
  OSType fmt = CVPixelBufferGetPixelFormatType(pb);

  if ( fmt != kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange &&
       fmt != kCVPixelFormatType_420YpCbCr8BiPlanarFullRange )
  { vb_error(errmsg, "decoder did not produce NV12");
    return false;
  }

  int bw = (int)CVPixelBufferGetWidth(pb)  & ~1;
  int bh = (int)CVPixelBufferGetHeight(pb) & ~1;
  int w = bw < r->width  ? bw : r->width;
  int h = bh < r->height ? bh : r->height;
  SDL_Surface *s = *sp;

  if ( s && (s->w != w || s->h != h || s->format != SDL_PIXELFORMAT_NV12) )
  { SDL_DestroySurface(s);
    *sp = s = NULL;
  }
  if ( !s && !(*sp = s = SDL_CreateSurface(w, h, SDL_PIXELFORMAT_NV12)) )
  { vb_error(errmsg, SDL_GetError());
    return false;
  }
  if ( CVPixelBufferLockBaseAddress(pb, kCVPixelBufferLock_ReadOnly) !=
       kCVReturnSuccess )
  { vb_error(errmsg, "cannot access the frame");
    return false;
  }

  const Uint8 *y  = CVPixelBufferGetBaseAddressOfPlane(pb, 0);
  const Uint8 *uv = CVPixelBufferGetBaseAddressOfPlane(pb, 1);
  size_t ypitch  = CVPixelBufferGetBytesPerRowOfPlane(pb, 0);
  size_t uvpitch = CVPixelBufferGetBytesPerRowOfPlane(pb, 1);
  Uint8 *out = s->pixels;

  for(int i=0; i<h; i++)
    memcpy(out+(size_t)i*s->pitch, y+(size_t)i*ypitch, w);
  out += (size_t)s->pitch*h;
  for(int i=0; i<h/2; i++)
    memcpy(out+(size_t)i*s->pitch, uv+(size_t)i*uvpitch, w);
  CVPixelBufferUnlockBaseAddress(pb, kCVPixelBufferLock_ReadOnly);
  SDL_SetSurfaceColorspace(s, frame_colorspace(pb, h));

  return true;
}


static bool
sample_to_samples(vb_reader *r, CMSampleBufferRef sample, vb_samples *out,
		  char *errmsg)
{ CMBlockBufferRef bb = CMSampleBufferGetDataBuffer(sample);
  size_t len = bb ? CMBlockBufferGetDataLength(bb) : 0;

  if ( len > r->asize )
  { float *s = SDL_realloc(r->samples, len);

    if ( !s )
    { vb_error(errmsg, "out of memory");
      return false;
    }
    r->samples = s;
    r->asize = len;
  }
  if ( len > 0 &&
       CMBlockBufferCopyDataBytes(bb, 0, len, r->samples) != kCMBlockBufferNoErr )
  { vb_error(errmsg, "cannot access the audio");
    return false;
  }
  out->data = r->samples;
  out->frames = (int)(len/(sizeof(float)*r->channels));

  return true;
}


/* Make sure there is a pending sample for an output that is not done.
 * Samples without data, such as markers, are skipped.
 */

static bool
fill_pending(vb_reader *r, AVAssetReaderTrackOutput *output,
	     CMSampleBufferRef *pending, bool *done, bool video, char *errmsg)
{ while( !*done && !*pending )
  { CMSampleBufferRef s = [output copyNextSampleBuffer];

    if ( !s )
    { if ( r->reader.status == AVAssetReaderStatusFailed )
      { ns_error(errmsg, "decode", r->reader.error);
	return false;
      }
      *done = true;
    } else if ( video ? CMSampleBufferGetImageBuffer(s) != NULL
		      : CMSampleBufferGetNumSamples(s) > 0 )
    { *pending = s;
    } else
    { CFRelease(s);
    }
  }

  return true;
}


static double
sample_time(CMSampleBufferRef s)
{ CMTime t = CMSampleBufferGetPresentationTimeStamp(s);

  return CMTIME_IS_NUMERIC(t) ? CMTimeGetSeconds(t) : 0.0;
}


vb_status
vb_read(vb_reader *r, SDL_Surface **frame, vb_samples *samples,
	double *pts, char *errmsg)
{ @autoreleasepool
  { if ( !fill_pending(r, r->vout, &r->vpending, &r->video_done, true,
		       errmsg) ||
	 !fill_pending(r, r->aout, &r->apending, &r->audio_done, false,
		       errmsg) )
      return VB_ERROR;

    bool video;				/* the earliest of the two */

    if ( r->vpending && r->apending )
      video = sample_time(r->vpending) <= sample_time(r->apending);
    else if ( r->vpending )
      video = true;
    else if ( r->apending )
      video = false;
    else
      return VB_EOF;

    CMSampleBufferRef s = video ? r->vpending : r->apending;
    bool ok;

    *pts = sample_time(s);
    if ( video )
    { ok = sample_to_surface(r, s, frame, errmsg);
      r->vpending = NULL;
    } else
    { ok = sample_to_samples(r, s, samples, errmsg);
      r->apending = NULL;
    }
    CFRelease(s);

    return ok ? (video ? VB_FRAME : VB_AUDIO) : VB_ERROR;
  }
}


bool
vb_seek(vb_reader *r, double t, char *errmsg)
{ @autoreleasepool
  { return start_reader(r, t, errmsg);
  }
}


		 /*******************************
		 *	       WRITING		*
		 *******************************/

/* AVAssetWriter interleaves the tracks, so an input may refuse data
 * until the other input got enough.  As we are called synchronously
 * from a single thread, we cannot wait for that.  Therefore we keep a
 * queue for each input and only wait if the writer has data for both
 * inputs, or if a queue gets too long.
 */

#define VQUEUE_SIZE  48			/* frames */
#define AQUEUE_SIZE 256			/* audio buffers */
#define VQUEUE_SOFT   4			/* wait if the writer has all */
#define AQUEUE_SOFT  16
#define WAIT_TIMEOUT 20000		/* ms without progress */

typedef struct vframe
{ CVPixelBufferRef pb;
  CMTime	   time;
} vframe;

struct vb_writer
{ AVAssetWriter	   *writer;
  AVAssetWriterInput *vinput;
  AVAssetWriterInput *ainput;		/* nil: no audio */
  AVAssetWriterInputPixelBufferAdaptor *adaptor;
  CMAudioFormatDescriptionRef afmt;	/* of the samples we get */
  bool		    writing;		/* startWriting succeeded */
  int		    width;
  int		    height;
  bool		    hd;			/* BT.709 rather than BT.601 */
  int32_t	    timescale;		/* of the video */
  CMTime	    frame_duration;
  CMTime	    last_time;		/* of the last frame */
  int		    channels;
  int		    rate;
  int64_t	    asamples;		/* samples written */
  vframe	    vqueue[VQUEUE_SIZE];
  int		    vhead;
  int		    vcount;
  CMSampleBufferRef aqueue[AQUEUE_SIZE];
  int		    ahead;
  int		    acount;
};


/* Write what the inputs accept.  Sets *progress if it wrote something */

static bool
drain(vb_writer *w, bool *progress, char *errmsg)
{ while( w->vcount > 0 && w->vinput.readyForMoreMediaData )
  { vframe *f = &w->vqueue[w->vhead];
    BOOL ok = [w->adaptor appendPixelBuffer:f->pb
		       withPresentationTime:f->time];

    CVPixelBufferRelease(f->pb);
    w->vhead = (w->vhead+1)%VQUEUE_SIZE;
    w->vcount--;
    if ( !ok )
    { ns_error(errmsg, "encode", w->writer.error);
      return false;
    }
    *progress = true;
  }
  while( w->acount > 0 && w->ainput.readyForMoreMediaData )
  { CMSampleBufferRef s = w->aqueue[w->ahead];
    BOOL ok = [w->ainput appendSampleBuffer:s];

    CFRelease(s);
    w->ahead = (w->ahead+1)%AQUEUE_SIZE;
    w->acount--;
    if ( !ok )
    { ns_error(errmsg, "encode audio", w->writer.error);
      return false;
    }
    *progress = true;
  }
  if ( w->writer.status == AVAssetWriterStatusFailed )
  { ns_error(errmsg, "encode", w->writer.error);
    return false;
  }

  return true;
}


static bool
must_wait(vb_writer *w)
{ if ( w->vcount >= VQUEUE_SIZE || w->acount >= AQUEUE_SIZE )
    return true;			/* no room */
  if ( !w->ainput )
    return w->vcount > VQUEUE_SOFT;

  return ( (w->vcount > VQUEUE_SOFT && w->acount > 0) ||
	   (w->acount > AQUEUE_SOFT && w->vcount > 0) );
}


/* Write what we can and wait while the writer is busy */

static bool
flush_queues(vb_writer *w, char *errmsg)
{ Uint64 last = SDL_GetTicks();

  for(;;)
  { bool progress = false;

    if ( !drain(w, &progress, errmsg) )
      return false;
    if ( !must_wait(w) )
      return true;
    if ( progress )
      last = SDL_GetTicks();
    else if ( SDL_GetTicks() - last > WAIT_TIMEOUT )
    { vb_error(errmsg, "encode: the encoder does not accept data");
      return false;
    }
    SDL_Delay(1);
  }
}


static void
free_queues(vb_writer *w)
{ for(; w->vcount > 0; w->vcount--)
  { CVPixelBufferRelease(w->vqueue[w->vhead].pb);
    w->vhead = (w->vhead+1)%VQUEUE_SIZE;
  }
  for(; w->acount > 0; w->acount--)
  { CFRelease(w->aqueue[w->ahead]);
    w->ahead = (w->ahead+1)%AQUEUE_SIZE;
  }
}


/* The rate the AAC encoder supports that is closest to `rate` */

static int
aac_rate(int rate)
{ return rate <= 44100 ? 44100 : 48000;
}


static NSString *
file_type(const char *file)
{ const char *ext = strrchr(file, '.');

  if ( ext && !strchr(ext, '/') )
  { if ( SDL_strcasecmp(ext, ".mp4") == 0 )
      return AVFileTypeMPEG4;
    if ( SDL_strcasecmp(ext, ".mov") == 0 )
      return AVFileTypeQuickTimeMovie;
  }

  return nil;
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

  @autoreleasepool
  { vb_writer *w = SDL_calloc(1, sizeof(*w));
    NSString *type = file_type(file);
    NSString *path = [NSString stringWithUTF8String:file];
    AVVideoCodecType codec = AVVideoCodecTypeH264;
    NSError *error = nil;

    if ( !w )
    { vb_error(errmsg, "out of memory");
      return NULL;
    }
    if ( !type )
    { snprintf(errmsg, VB_ERRLEN, "%s: unsupported file type (use .mp4 or .mov)",
	       file);
      goto error;
    }
    if ( spec->codec )
    { if ( strcmp(spec->codec, "hevc") == 0 || strcmp(spec->codec, "h265") == 0 )
	codec = AVVideoCodecTypeHEVC;
      else if ( strcmp(spec->codec, "h264") != 0 )
      { snprintf(errmsg, VB_ERRLEN, "No encoder for %s", spec->codec);
	goto error;
      }
    }

    double fps = spec->frame_rate > 0 ? spec->frame_rate : 30.0;
    double bpp = spec->quality >= 0 ? 0.03 + spec->quality*0.0027 : 0.12;
    long bitrate = (long)(spec->width*spec->height*fps*bpp);

    w->width = spec->width;
    w->height = spec->height;
    w->hd = w->height >= 720;
    w->timescale = 90000;
    w->frame_duration = CMTimeMakeWithSeconds(1.0/fps, w->timescale);
    w->last_time = kCMTimeInvalid;

    NSURL *url = [NSURL fileURLWithPath:path];
    BOOL dir;				/* AVAssetWriter does not overwrite */
    if ( [[NSFileManager defaultManager] fileExistsAtPath:path
					      isDirectory:&dir] && !dir )
      [[NSFileManager defaultManager] removeItemAtURL:url error:NULL];

    w->writer = [[AVAssetWriter alloc] initWithURL:url fileType:type
					     error:&error];
    if ( !w->writer )
    { ns_error(errmsg, file, error);
      goto error;
    }

    NSDictionary *colour =
      w->hd ? @{ AVVideoColorPrimariesKey: AVVideoColorPrimaries_ITU_R_709_2,
		 AVVideoTransferFunctionKey: AVVideoTransferFunction_ITU_R_709_2,
		 AVVideoYCbCrMatrixKey: AVVideoYCbCrMatrix_ITU_R_709_2 }
	    : @{ AVVideoColorPrimariesKey: AVVideoColorPrimaries_SMPTE_C,
		 AVVideoTransferFunctionKey: AVVideoTransferFunction_ITU_R_709_2,
		 AVVideoYCbCrMatrixKey: AVVideoYCbCrMatrix_ITU_R_601_4 };
    NSDictionary *vsettings =
      @{ AVVideoCodecKey: codec,
	 AVVideoWidthKey: @(w->width),
	 AVVideoHeightKey: @(w->height),
	 AVVideoColorPropertiesKey: colour,
	 AVVideoCompressionPropertiesKey:
	   @{ AVVideoAverageBitRateKey: @(bitrate),
	      AVVideoExpectedSourceFrameRateKey: @(fps),
	      AVVideoMaxKeyFrameIntervalKey: @((int)(fps*2+0.5))
	    }
       };

    w->vinput = [[AVAssetWriterInput alloc] initWithMediaType:AVMediaTypeVideo
					       outputSettings:vsettings];
    w->vinput.expectsMediaDataInRealTime = NO;
    if ( ![w->writer canAddInput:w->vinput] )
    { vb_error(errmsg, "video encoder: cannot add the video");
      goto error;
    }
    [w->writer addInput:w->vinput];

    NSDictionary *pbattrs =
      @{ (id)kCVPixelBufferPixelFormatTypeKey:
	   @(kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange),
	 (id)kCVPixelBufferWidthKey: @(w->width),
	 (id)kCVPixelBufferHeightKey: @(w->height),
	 (id)kCVPixelBufferIOSurfacePropertiesKey: @{}
       };
    w->adaptor = [[AVAssetWriterInputPixelBufferAdaptor alloc]
		   initWithAssetWriterInput:w->vinput
		   sourcePixelBufferAttributes:pbattrs];

    if ( spec->audio_rate > 0 )
    { w->rate = aac_rate(spec->audio_rate);
      w->channels = spec->audio_channels > 2 ? 2 :
		    spec->audio_channels < 1 ? 1 : spec->audio_channels;

      NSDictionary *asettings =
	@{ AVFormatIDKey: @(kAudioFormatMPEG4AAC),
	   AVSampleRateKey: @(w->rate),
	   AVNumberOfChannelsKey: @(w->channels),
	   AVEncoderBitRateKey: @(96000*w->channels)
	 };
      w->ainput = [[AVAssetWriterInput alloc] initWithMediaType:AVMediaTypeAudio
						 outputSettings:asettings];
      w->ainput.expectsMediaDataInRealTime = NO;
      if ( ![w->writer canAddInput:w->ainput] )
      { vb_error(errmsg, "audio encoder: cannot add the audio");
	goto error;
      }
      [w->writer addInput:w->ainput];

      AudioStreamBasicDescription asbd =
      { .mSampleRate	   = w->rate,
	.mFormatID	   = kAudioFormatLinearPCM,
	.mFormatFlags	   = kAudioFormatFlagIsFloat|kAudioFormatFlagIsPacked,
	.mBytesPerPacket   = (UInt32)(sizeof(float)*w->channels),
	.mFramesPerPacket  = 1,
	.mBytesPerFrame	   = (UInt32)(sizeof(float)*w->channels),
	.mChannelsPerFrame = (UInt32)w->channels,
	.mBitsPerChannel   = 32
      };
      OSStatus rc = CMAudioFormatDescriptionCreate(NULL, &asbd, 0, NULL,
						   0, NULL, NULL, &w->afmt);
      if ( rc != noErr )
      { snprintf(errmsg, VB_ERRLEN, "audio format: error %d", (int)rc);
	goto error;
      }

      spec->audio_rate = w->rate;
      spec->audio_channels = w->channels;
      spec->audio_codec = "aac";
    }

    if ( ![w->writer startWriting] )
    { ns_error(errmsg, file, w->writer.error);
      goto error;
    }
    [w->writer startSessionAtSourceTime:kCMTimeZero];
    w->writing = true;

    spec->codec = codec == AVVideoCodecTypeHEVC ? "hevc" : "h264";
    *format = SDL_PIXELFORMAT_NV12;
    *colorspace = w->hd ? SDL_COLORSPACE_BT709_LIMITED
			: SDL_COLORSPACE_BT601_LIMITED;

    return w;

  error:
    vb_close_writer(w, errmsg);
    return NULL;
  }
}


bool
vb_write_frame(vb_writer *w, SDL_Surface *s, double pts, char *errmsg)
{ @autoreleasepool
  { CVPixelBufferPoolRef pool = w->adaptor.pixelBufferPool;
    CVPixelBufferRef pb = NULL;

    if ( !pool )
    { ns_error(errmsg, "encode", w->writer.error);
      return false;
    }
    if ( CVPixelBufferPoolCreatePixelBuffer(NULL, pool, &pb) !=
	 kCVReturnSuccess ||
	 CVPixelBufferLockBaseAddress(pb, 0) != kCVReturnSuccess )
    { CVPixelBufferRelease(pb);
      vb_error(errmsg, "encode: cannot create a frame");
      return false;
    }

    int wd = w->width, h = w->height;
    Uint8 *y  = CVPixelBufferGetBaseAddressOfPlane(pb, 0);
    Uint8 *uv = CVPixelBufferGetBaseAddressOfPlane(pb, 1);
    size_t ypitch  = CVPixelBufferGetBytesPerRowOfPlane(pb, 0);
    size_t uvpitch = CVPixelBufferGetBytesPerRowOfPlane(pb, 1);
    const Uint8 *in = s->pixels;

    for(int i=0; i<h; i++)
      memcpy(y+(size_t)i*ypitch, in+(size_t)i*s->pitch, wd);
    in += (size_t)s->pitch*h;
    for(int i=0; i<h/2; i++)
      memcpy(uv+(size_t)i*uvpitch, in+(size_t)i*s->pitch, wd);
    CVPixelBufferUnlockBaseAddress(pb, 0);

    CVBufferSetAttachment(pb, kCVImageBufferYCbCrMatrixKey,
			  w->hd ? kCVImageBufferYCbCrMatrix_ITU_R_709_2
				: kCVImageBufferYCbCrMatrix_ITU_R_601_4,
			  kCVAttachmentMode_ShouldPropagate);
    CVBufferSetAttachment(pb, kCVImageBufferColorPrimariesKey,
			  w->hd ? kCVImageBufferColorPrimaries_ITU_R_709_2
				: kCVImageBufferColorPrimaries_SMPTE_C,
			  kCVAttachmentMode_ShouldPropagate);
    CVBufferSetAttachment(pb, kCVImageBufferTransferFunctionKey,
			  kCVImageBufferTransferFunction_ITU_R_709_2,
			  kCVAttachmentMode_ShouldPropagate);

    CMTime time = CMTimeMakeWithSeconds(pts > 0 ? pts : 0, w->timescale);
    if ( CMTIME_IS_VALID(w->last_time) &&	/* times must increase */
	 CMTimeCompare(time, w->last_time) <= 0 )
      time = CMTimeAdd(w->last_time, CMTimeMake(1, w->timescale));
    w->last_time = time;

    int i = (w->vhead+w->vcount)%VQUEUE_SIZE;
    w->vqueue[i].pb = pb;
    w->vqueue[i].time = time;
    w->vcount++;

    return flush_queues(w, errmsg);
  }
}


bool
vb_write_audio(vb_writer *w, const float *samples, int frames, char *errmsg)
{ @autoreleasepool
  { if ( !w->ainput )
    { vb_error(errmsg, "the video has no audio");
      return false;
    }
    if ( frames <= 0 )
      return true;

    size_t len = (size_t)frames*w->channels*sizeof(float);
    CMBlockBufferRef bb = NULL;
    CMSampleBufferRef sb = NULL;
    OSStatus rc = CMBlockBufferCreateWithMemoryBlock(
		    NULL, NULL, len, NULL, NULL, 0, len,
		    kCMBlockBufferAssureMemoryNowFlag, &bb);

    if ( rc == noErr )
      rc = CMBlockBufferReplaceDataBytes(samples, bb, 0, len);
    if ( rc == noErr )
      rc = CMAudioSampleBufferCreateReadyWithPacketDescriptions(
	     NULL, bb, w->afmt, frames, CMTimeMake(w->asamples, w->rate),
	     NULL, &sb);
    if ( bb )
      CFRelease(bb);
    if ( rc != noErr )
    { snprintf(errmsg, VB_ERRLEN, "encode audio: error %d", (int)rc);
      return false;
    }
    w->asamples += frames;

    int i = (w->ahead+w->acount)%AQUEUE_SIZE;
    w->aqueue[i] = sb;
    w->acount++;

    return flush_queues(w, errmsg);
  }
}


/* Write the queues, marking an input as finished as soon as its queue is
 * empty, so the writer no longer waits for it.
 */

static bool
finish_inputs(vb_writer *w, char *errmsg)
{ bool vfinished = false, afinished = (w->ainput == nil);
  Uint64 last = SDL_GetTicks();

  for(;;)
  { bool progress = false;

    if ( !drain(w, &progress, errmsg) )
      return false;
    if ( !vfinished && w->vcount == 0 )
    { [w->vinput markAsFinished];
      vfinished = progress = true;
    }
    if ( !afinished && w->acount == 0 )
    { [w->ainput markAsFinished];
      afinished = progress = true;
    }
    if ( vfinished && afinished )
      return true;
    if ( progress )
      last = SDL_GetTicks();
    else if ( SDL_GetTicks() - last > WAIT_TIMEOUT )
    { vb_error(errmsg, "finish: the encoder does not accept data");
      return false;
    }
    SDL_Delay(1);
  }
}


bool
vb_close_writer(vb_writer *w, char *errmsg)
{ @autoreleasepool
  { bool ok = true;

    if ( w->writing )
    { if ( (ok = finish_inputs(w, errmsg)) )
      { CMTime end = kCMTimeZero;	/* the end of the last frame/sample */
	dispatch_semaphore_t done = dispatch_semaphore_create(0);

	if ( CMTIME_IS_VALID(w->last_time) )
	  end = CMTimeAdd(w->last_time, w->frame_duration);
	if ( w->ainput )
	  end = CMTimeMaximum(end, CMTimeMake(w->asamples, w->rate));
	[w->writer endSessionAtSourceTime:end];
	[w->writer finishWritingWithCompletionHandler:^{
	  dispatch_semaphore_signal(done);
	}];
	dispatch_semaphore_wait(done, DISPATCH_TIME_FOREVER);
	dispatch_release(done);
	if ( w->writer.status != AVAssetWriterStatusCompleted )
	{ ns_error(errmsg, "finish", w->writer.error);
	  ok = false;
	}
      } else
      { [w->writer cancelWriting];
      }
    }
    free_queues(w);
    if ( w->afmt )
      CFRelease(w->afmt);
    [w->adaptor release];
    [w->vinput release];
    [w->ainput release];
    [w->writer release];
    SDL_free(w);

    return ok;
  }
}


		 /*******************************
		 *	    CAPABILITIES	*
		 *******************************/

void
vb_codecs(bool encode, void (*f)(const char *name, void *closure),
	  void *closure)
{ @autoreleasepool
  { if ( encode )
    { f("h264", closure);
      f("hevc", closure);
    } else
    { f("h264", closure);
      f("hevc", closure);
      f("prores", closure);
      f("mpeg4", closure);
      if ( VTIsHardwareDecodeSupported(FOURCC('a','v','0','1')) )
	f("av1", closure);
      if ( VTIsHardwareDecodeSupported(FOURCC('v','p','0','9')) )
	f("vp9", closure);
    }
  }
}
