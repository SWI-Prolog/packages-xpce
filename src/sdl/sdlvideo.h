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

#ifndef SDLVIDEO_H
#define SDLVIDEO_H
#include <SDL3/SDL.h>

#if defined(HAVE_VIDEO_FFMPEG) || defined(HAVE_VIDEO_AVF) || defined(HAVE_VIDEO_MF)
#define HAVE_VIDEO 1
#endif

typedef struct video_info
{ int		width;
  int		height;
  double	frame_rate;
  double	duration;
  const char   *codec;
  const char   *audio_codec;		/* NULL: no audio */
  int		audio_rate;
  int		audio_channels;
  bool		playing_audio;		/* audio goes to a device */
  bool		live;			/* network stream we cannot seek */
} video_info;

status	ws_open_video(Video vid);
bool	ws_video_opening(Video vid);
status	ws_write_video(Video vid, CharArray data);
void	ws_close_video(Video vid);
void	ws_play_video(Video vid);
void	ws_pause_video(Video vid);
void	ws_seek_video(Video vid, double t);
double	ws_video_position(Video vid);
Image	ws_video_frame_image(Video vid, double at);
bool	ws_video_info(Video vid, video_info *info);
void	ws_volume_video(Video vid);
bool	sdl_video_tick(void);
status	ws_open_video_writer(VideoWriter vw);
status	ws_append_video_writer(VideoWriter vw, SDL_Surface *src, double pts);
status	ws_close_video_writer(VideoWriter vw);
status	ws_append_audio_video_writer(VideoWriter vw, CharArray data,
				     Name format, int rate, int channels);
Chain	ws_video_codecs(bool encode);
Name	ws_video_backend(void);

#endif /*SDLVIDEO_H*/
