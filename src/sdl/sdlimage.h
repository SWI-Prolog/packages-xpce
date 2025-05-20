/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

#ifndef RAYIMAGE_H
#define RAYIMAGE_H
#include <SDL3/SDL.h>
#include <cairo/cairo.h>

/* private interface (to sdldraw.c) */
cairo_surface_t *pceImage2CairoSurface(Image image);

/* Public interface to core */
void ws_init_image(Image image);
void ws_destroy_image(Image image);
status ws_store_image(Image image, FileObj file);
status loadXImage(Image image, IOSTREAM *fd);
status loadPNMImage(Image image, IOSTREAM *fd);
status ws_load_old_image(Image image, IOSTREAM *fd);
status ws_load_image_file(Image image);
status ws_create_image_from_xpm_data(Image image, char **data, DisplayObj d);
status ws_save_image_file(Image image, SourceSink into, Name fmt);
status ws_open_image(Image image, DisplayObj d, double scale);
void ws_close_image(Image image, DisplayObj d);
status ws_resize_image(Image image, Int w, Int h);
Image ws_scale_image(Image image, int w, int h);
Image ws_rotate_image(Image image, float angle);
Image ws_monochrome_image(Image image);
Image ws_grayscale_image(Image image);
void ws_postscript_image(Image image, Int depth, int iscolor);
status loadXliImage(Image image, FileObj file, Int bright);
void ws_create_image_from_x11_data(Image image, unsigned char *data, int w, int h);
ColourMap ws_colour_map_for_image(Image image);
void ws_system_images(void);
void ws_prepare_image_mask(Image image);
status ws_has_alpha_image(Image image);

#endif /* RAYIMAGE_H */
