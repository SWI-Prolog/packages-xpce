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

#ifndef RAYDRAW_H
#define RAYDRAW_H

void resetDraw(void);
void d_offset(int x, int y);
void r_offset(int x, int y);
void r_filloffset(Point offset, int x0, int y0, fill_state *state);
void r_fillrestore(fill_state *state);
DisplayObj d_display(DisplayObj d);
void d_ensure_display(void);
void d_flush(void);
status d_window(PceWindow sw, int x, int y, int w, int h, int clear, int limit);
void d_image(Image i, int x, int y, int w, int h);
void d_screen(DisplayObj d);
void d_frame(FrameObj fr, int x, int y, int w, int h);
void d_clip(int x, int y, int w, int h);
void d_done(void);
void d_clip_done(void);
void intersection_iarea(IArea a, IArea b);
void r_clear(int x, int y, int w, int h);
void r_complement(int x, int y, int w, int h);
void r_and(int x, int y, int w, int h, Image pattern);
void r_thickness(int pen);
int r_transformed(int val);
void r_dash(Name name);
void d_pen(Pen pen);
void r_fillpattern(Any fill, Name which);
void r_arcmode(Name mode);
void r_fix_colours(Any fg, Any bg, ColourContext ctx);
void r_unfix_colours(ColourContext ctx);
Any r_default_colour(Any c);
Any r_colour(Any c);
Any r_background(Any c);
void r_swap_background_and_foreground(void);
BoolObj r_subwindow_mode(BoolObj val);
void r_invert_mode(BoolObj val);
void r_translate(int x, int y, int *ox, int *oy);
void r_box(int x, int y, int w, int h, int r, Any fill);
void r_shadow_box(int x, int y, int w, int h, int r, int shadow, Image fill);
Any r_elevation_shadow(Elevation e);
void r_3d_segments(int n, ISegment s, Elevation e, int light);
void r_3d_box(int x, int y, int w, int h, int radius, Elevation e, int up);
void r_3d_line(int x1, int y1, int x2, int y2, Elevation e, int up);
void r_3d_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Elevation e, int up, int map);
void r_3d_diamond(int x, int y, int w, int h, Elevation e, int up);
void r_arc(int x, int y, int w, int h, int s, int e, Any fill);
void r_ellipse(int x, int y, int w, int h, Any fill);
void r_3d_ellipse(int x, int y, int w, int h, Elevation z, int up);
void r_line(int x1, int y1, int x2, int y2);
void r_polygon(IPoint pts, int n, int close);
void r_path(Chain points, int ox, int oy, int radius, int closed, Image fill);
void r_op_image(Image image, int sx, int sy, int x, int y, int w, int h, Name op);
void r_image(Image image, int sx, int sy, int x, int y, int w, int h, BoolObj transparent);
void r_fill(int x, int y, int w, int h, Any pattern);
void r_fill_polygon(IPoint pts, int n);
void r_caret(int cx, int cy, FontObj font);
void r_fill_triangle(int x1, int y1, int x2, int y2, int x3, int y3);
void r_triangle(int x1, int y1, int x2, int y2, int x3, int y3, Any fill);
void r_pixel(int x, int y, Any val);
void r_complement_pixel(int x, int y);
void d_modify(void);
int r_get_mono_pixel(int x, int y);
unsigned long r_get_pixel(int x, int y);
int s_has_char(FontObj f, unsigned int c);
void f_domain(FontObj f, Name which, int *x, int *y);
int s_default_char(FontObj font);
int s_ascent(FontObj f);
int s_descent(FontObj f);
int s_height(FontObj f);
int c_width(wint_t c, FontObj font);
int str_width(PceString s, int from, int to, FontObj f);
int str_advance(PceString s, int from, int to, FontObj f);
void s_printA(charA *s, int l, int x, int y, FontObj f);
void s_printW(charW *s, int l, int x, int y, FontObj f);
void s_print(PceString s, int x, int y, FontObj f);
void s_print_aligned(PceString s, int x, int y, FontObj f);
void str_size(PceString s, FontObj font, int *width, int *height);
void str_string(PceString s, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);
void str_selected_string(PceString s, FontObj font, int f, int t, Style style, int x, int y, int w, int h, Name hadjust, Name vadjust);
void ps_string(PceString s, FontObj font, int x, int y, int w, Name format, int flags);
void str_label(PceString s, int acc, FontObj font, int x, int y, int w, int h, Name hadjust, Name vadjust, int flags);

#endif /* RAYDRAW_H */
