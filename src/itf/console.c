/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2013, University of Amsterdam
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

#include <stdio.h>
#include <stdarg.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
XPCE Console IO is only used  for   debugging  purposes. On Unix systems
this IO will normally be bound to  Unix stdout/stdin. On strictly window
based systems you may redefine these functions to use a window.

    void
    vCprintf(const char *fmt, va_list args)
	Behaves like: vprintf(fmt, args);

    int
    Cputchar(int chr)
	Behaves like: putchar(chr);

    void
    Cflush(void)
	Behaves like fflush(stdout);

    char *
    Cgetline(char *buf, int size)
	Behaves like: fgets(buf, size, stdin);
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __WINDOWS__

static FILE *console_in = NULL;
static FILE *console_out = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Whenever a stand-alone XPCE/Something application  writes explicitely to
the console, this stub will allocate a console to write to. Note the use
of _IONBF, instead of _IOLBF  which  would   be  much  more  natural. It
doesn't appear to work however (Windows-NT 4.0, MSVC 4.2).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <windows.h>
#include <io.h>
#include <fcntl.h>

static int
ensure_console(void)
{ static int allocated = 0;

  if ( !allocated )
  { allocated++;
    if ( AllocConsole() )
    { HANDLE hin  = GetStdHandle(STD_INPUT_HANDLE);
      HANDLE hout = GetStdHandle(STD_OUTPUT_HANDLE);
      int in  = _open_osfhandle((intptr_t)hin, _O_RDONLY);
      int out = _open_osfhandle((intptr_t)hout, _O_APPEND);

      console_in  = _fdopen(in, "r");
      console_out = _fdopen(out, "w");

      setvbuf(console_in,  NULL, _IONBF, 256);
      setvbuf(console_out, NULL, _IONBF, 256);
     }
  }

  return 1;
}

#else /* ~__WINDOWS__ */

static FILE *console_in = NULL;
static FILE *console_out = NULL;

static int
ensure_console()
{ console_in = stdin;
  console_out = stdout;

  return 1;
}

#endif /*__WINDOWS__*/

void
Stub__vCprintf(const char *fmt, va_list args)
{ if ( ensure_console() )
    vfprintf(console_out, fmt, args);
}


int
Stub__Cputchar(int chr)
{ if ( ensure_console() )
    return fputc(chr, console_out);
  else
    return EOF;				/* signal error */
}


char *
Stub__Cgetline(char *line, int size)
{ if ( ensure_console() )
    return fgets(line, size, console_in);
  else
    return NULL;			/* signal error */
}


void
Stub__Cflush()
{ if ( ensure_console() )
    fflush(console_out);
}
