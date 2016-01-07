/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

void	r_msarc(int x, int y, int w, int h,
		int sx, int sy,
		int ex, int ey,
		int large,
		Name close,
		Any fill);
void	ws_check_intr(void);
int	ws_free_file_descriptors(void);
int	ws_getpid(void);
void	ws_initialise(int argc, char **argv);
void	ws_sleep(int time);
void	ws_invalidate_window(PceWindow sw, Area a);
void	ws_scroll_window(PceWindow sw, int dx, int dy);
void	ws_redraw_window(PceWindow sw, IArea a, int clear);
int	ws_emulate_three_buttons(int time);
int	ws_mousebuttons(void);
void	ws_kill_process(Process p, int sig);
char *	ws_os(void);
Name 	ws_appdata(const char *sub);
Name	WinStrError(int error, ...);
status	makeClassWinMF(Class class);
status	makeClassWinPrinter(Class class);
void	ws_frame_border(FrameObj fr, int *xb, int *yb, int *ycap);
