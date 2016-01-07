/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

		/********************************
		*            HACKS ...		*
		********************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Who  the   hell is using  these!?   It doesn't  seem  to be  the X11R5
libraries.  It certainly ain't PCE itself.  Nevertheless someone seems
to refer  to them.  Unfortunately they only  in a dynamic library  and
thus cannot be loaded through many foreign  language interfaces.  What
to do????
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if !defined(HAVE_LIBDL) && defined(__sun__) && XT_REVISION == 5

void *
dlopen(char *path, int mode)
{ Cprintf("dlopen(%s, %d)\n", path, mode);

  return NULL;
}


void *
dlsym(void *handle, char *symbol)
{ Cprintf("dlsym(%p, %s)\n", handle, symbol);

  return NULL;
}


void *
dlclose(void *handle)
{ Cprintf("dlclose (%p)\n", handle);

  return NULL;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
More of this nonsens.  RS6000 this time ...

	nm -pgo /usr/lib/libX11.a | grep _iconv_open
	shr4.o:         U __iconv_open
	shr4.o:0000fc18 T .__iconv_open
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if _AIX
void *
__iconv_open()
{ Cprintf("_iconv_open() called\n");

  return NULL;
}
#endif
