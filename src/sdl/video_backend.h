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

#ifndef VIDEO_BACKEND_H
#define VIDEO_BACKEND_H
#include <SDL3/SDL.h>
#include <stdbool.h>

/* The interface between the video classes (sdlvideo.c) and the library
 * that decodes and encodes video: ffmpeg, AVFoundation or Media
 * Foundation.  A backend is synchronous: sdlvideo.c calls it from its
 * worker threads and does the queueing, timing and seeking.  A backend
 * never touches xpce objects.
 *
 * Frames are SDL_Surface objects with contiguous pixels in a YUV
 * format the renderer understands (IYUV, NV12 or P010) and the
 * colorspace of the video.  The reader may reuse a surface it is
 * given if it has the right size and format.
 *
 * Audio is interleaved 32-bit float samples.  SDL converts it to the
 * format of the device or from the format of the user.
 */

#define VB_ERRLEN 256

typedef struct vb_reader vb_reader;	/* defined by the backend */
typedef struct vb_writer vb_writer;

typedef struct vb_info
{ int		width;			/* size of the frames */
  int		height;
  double	frame_rate;		/* frames/second, 0 if unknown */
  double	duration;		/* seconds, 0 if unknown */
  const char   *codec;			/* name of the video codec */
  int		audio_rate;		/* samples/second, 0: no audio */
  int		audio_channels;
  const char   *audio_codec;		/* name of the audio codec */
  bool		live;			/* network stream we cannot seek */
} vb_info;

/* Where a reader gets its data: a file or URL, or a read callback.
 * read() blocks until data is available and returns 0 at the end and
 * -1 on an error.  seek() is as fseek() with SEEK_SET or SEEK_CUR and
 * returns the new position or -1.  While *interrupt is non-zero,
 * blocking calls of the backend, e.g., on the network, must return.
 */

typedef struct vb_source
{ const char   *url;			/* file or URL, or NULL */
  long	      (*read)(void *closure, void *buf, size_t size);
  long long   (*seek)(void *closure, long long offset, int whence);
  void	       *closure;
  char	      **options;		/* name, value, ..., NULL */
  SDL_AtomicInt *interrupt;		/* may be NULL */
} vb_source;

typedef struct vb_samples
{ const float  *data;			/* interleaved; owned by the reader */
  int		frames;			/* sample frames in data */
} vb_samples;

typedef enum
{ VB_FRAME,				/* got a frame */
  VB_AUDIO,				/* got samples */
  VB_EOF,				/* no more frames */
  VB_ERROR				/* see errmsg */
} vb_status;

typedef struct vb_wspec
{ int		width;			/* size of the frames */
  int		height;
  double	frame_rate;		/* nominal frames/second */
  const char   *codec;			/* NULL: the best available */
  int		quality;		/* 0..100, -1: default */
  int		audio_rate;		/* 0: no audio */
  int		audio_channels;
  const char   *audio_codec;		/* set to the codec used */
  const char   *format;			/* container; NULL: from the target */
  bool		live;			/* low latency, constant bitrate */
  int		bitrate;		/* bits/second, 0: default */
					/* if the file is NULL */
  bool	      (*write)(void *closure, const void *data, size_t len);
  void	       *closure;
} vb_wspec;

		 /*******************************
		 *	       READING		*
		 *******************************/

/* Open source.  If audio is false, the audio is ignored */
vb_reader *vb_open_reader(const vb_source *source, bool audio,
			  vb_info *info, char *errmsg);
/* Decode the next frame into *frame, which may be reused, or the next
 * samples into *samples.  Set *pts to their time in seconds.
 */
vb_status  vb_read(vb_reader *r, SDL_Surface **frame, vb_samples *samples,
		   double *pts, char *errmsg);
/* Position before t, such that the next frames lead up to t */
bool	   vb_seek(vb_reader *r, double t, char *errmsg);
void	   vb_close_reader(vb_reader *r);

		 /*******************************
		 *	       WRITING		*
		 *******************************/

/* Open a writer for a file or URL, or, if file is NULL, for the write
 * callback of spec, which gets the data from the thread that calls
 * vb_write_frame() and vb_close_writer().  *format is set to the pixel
 * format the frames must have for vb_write_frame() and the codecs in
 * spec are set to the codecs that are used.  If spec asks for audio,
 * its audio_rate may be changed to a rate the encoder supports.
 */
vb_writer *vb_open_writer(const char *file, vb_wspec *spec,
			  SDL_PixelFormat *format, SDL_Colorspace *colorspace,
			  char *errmsg);
bool	   vb_write_frame(vb_writer *w, SDL_Surface *frame, double pts,
			  char *errmsg);
/* Append samples to the audio, which starts at 0 and has no gaps */
bool	   vb_write_audio(vb_writer *w, const float *samples, int frames,
			  char *errmsg);
/* Flush and close.  w is freed, also on failure */
bool	   vb_close_writer(vb_writer *w, char *errmsg);

		 /*******************************
		 *	    CAPABILITIES	*
		 *******************************/

/* Call f for each codec of a short list of common ones that we can
 * decode (encode=false) or encode.
 */
void	   vb_codecs(bool encode, void (*f)(const char *name, void *closure),
		     void *closure);

#endif /*VIDEO_BACKEND_H*/
