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

#ifndef SDLAUDIO_H
#define SDLAUDIO_H
#include <SDL3/SDL.h>

status	ws_load_sound(Sound s, FileObj file);
status	ws_save_sound(Sound s, FileObj file);
status	ws_play_sound(Sound s, double volume);
status	ws_tone_sound(Sound s, int freq, double secs);
status	ws_stop_sound(Sound s);
bool	ws_playing_sound(Sound s);
void	ws_free_sound(Sound s);
status	ws_start_recording(AudioRecorder r);
int	ws_recording_frequency(void);
int	ws_recording_channels(void);
void	ws_stop_recording(AudioRecorder r);
status	ws_write_audio_stream(AudioStream as, CharArray data);
void	ws_close_audio_stream(AudioStream as);
void	ws_clear_audio_stream(AudioStream as);
status	ws_play_audio_stream(AudioStream as);
void	ws_stop_audio_stream(AudioStream as);
void	ws_volume_audio_stream(AudioStream as);
double	ws_queued_audio_stream(AudioStream as);
void	ws_free_audio_stream(AudioStream as);
bool	sdl_audio_event(SDL_Event *event);
void	sdl_start_media_tick(void);
Chain	ws_sound_file_extensions(void);
bool	ws_sound_peaks(Sound s, int n, double *peaks);

#endif /*SDLAUDIO_H*/
