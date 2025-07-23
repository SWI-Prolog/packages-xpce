/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2007-2012, University of Amsterdam
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

#define SDL_GRAPHICS 1		/* Use SDL graphics API */
#define _REENTRANT 1			/* Only multi-threading support */
#define UXWIN(unx, win) win

#define HAVE_UXNT_H 1
#define O_XOS 1				/* specific hooks */
#define O_DOSFILENAMES 1
#define O_HASSHARES			/* UNC filenames: //host/share */
#define VARIABLE_POINTER_OFFSET 1	/* Tagged int <-> pointer (win32s) */
#define HAVE_WINSOCK 1			/* try using winsock.dll */
#define HAVE_FSTAT 1			/* fstat() function */
#define HAVE_STRTOUL 1			/* strtoul() function */
#define HAVE_LIBXPM 1
#define HAVE_LIBJPEG 1

#define SIZEOF_WCHAR_T 2

#define export __declspec(dllexport)

#ifdef _MSC_VER
#define ALLOCA_BUG			/* see ker/goodies.c. MSVC++ 2.0 bug */
#endif

#define O_CPLUSPLUS 1

/* Define if your processor stores words with the most significant
   byte first (like Motorola and SPARC, unlike Intel and VAX).  */
/* #undef WORDS_BIGENDIAN */

/* Define if using alloca.c.  */
#undef C_ALLOCA

/* Define if type char is unsigned and you are not using gcc.  */
#undef __CHAR_UNSIGNED__

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
#undef CRAY_STACKSEG_END

/* Define if you have alloca, as a function or macro.  */
#define HAVE_ALLOCA 1

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
#undef HAVE_ALLOCA_H

/* Define as __inline if that's what the C compiler calls it.  */
#undef inline

/* Define if you need to in order for stat and other things to work.  */
#undef _POSIX_SOURCE

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE void

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown
 */
#define STACK_DIRECTION -1

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#undef TIME_WITH_SYS_TIME

/* Define if you have BSD signals (i.e. signal handler does *not* reset */
#undef BSD_SIGNALS

/* Define if you have the nsl library (-lnsl).  */
#undef HAVE_LIBNSL

/* Define if you have the socket library (-lsocket).  */
#undef HAVE_LIBSOCKET

/* Define if wait() uses union wait* for the 2nd argument. */
#undef UNION_WAIT

/* Define if (type)pointer = value is allowed */
#undef TAGGED_LVALUE

/* Define if struct termios has a member c_line */
#undef TERMIOS_HAS_C_LINE

/* Define if data-pointer is in high memory */
#undef POINTER_OFFSET

/* Define if text-pointer is in high memory */
#undef TEXT_OFFSET

/* Define to "file.h" to include additional system prototypes */
#undef SYSLIB_H

/* Define if you have the XmIsMotifWMRunning function.  */
#undef HAVE_XMISMOTIFWMRUNNING

/* Define if you have the XtPopupSpringLoaded function.  */
#undef HAVE_XTPOPUPSPRINGLOADED

/* Define if you have the fork function.  */
#undef HAVE_FORK

/* Define if you have the ftime function.  */
#define HAVE_FTIME 1

/* Define if you have the getcwd function.  */
#define HAVE_GETCWD 1

/* Define if you have the rename function.  */
#define HAVE_RENAME 1

/* Define if you have the accessd function.  */
#define HAVE_ACCESS 1

/* Define if you have the getdtablesize function.  */
#undef HAVE_GETDTABLESIZE

/* Define if you have the grantpt function.  */
#undef HAVE_GRANTPT

/* Define if you have the memmove function.  */
#define HAVE_MEMMOVE 1

/* Define if you have the tempnam function */
#define HAVE_TEMPNAM 1

/* Define if you have the tmpnam function */
/*#define HAVE_TMPNAM 1*/

/* Define if you have the on_exit function.  */
#undef HAVE_ON_EXIT

/* Define if you have the atexit function.  */
#define HAVE_ATEXIT 1

/* Define if you have the popen function.  */
#undef HAVE_POPEN

/* Define if you have the select function.  */
#undef HAVE_SELECT

/* Define if you have the setsid function.  */
#undef HAVE_SETSID

/* Define if you have the socket function.  */
#undef HAVE_SOCKET

/* Define if you have the strerror function.  */
#define HAVE_STRERROR 1

/* Define if you have the timelocal function.  */
#undef HAVE_TIMELOCAL

/* Define if you have the timelocal function.  */
#define HAVE_MKTIME 1

/* Define if you have the vsscanf function.  */
#undef HAVE_VSSCANF

/* Define if you have the <dirent.h> header file.  */
#define HAVE_DIRENT_H 1

/* Define if you have the <frame.h> header file.  */
#undef HAVE_FRAME_H

/* Define if you have the <malloc.h> header file.  */
#define HAVE_MALLOC_H 1

/* Define if you have the <memory.h> header file.  */
#define HAVE_MEMORY_H 1

/* Define if you have the <ndir.h> header file.  */
#undef HAVE_NDIR_H

/* Define if you have the <pwd.h> header file.  */
#undef HAVE_PWD_H

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <stropts.h> header file.  */
#undef HAVE_STROPTS_H

/* Define if you have the <sys/dir.h> header file.  */
#undef HAVE_SYS_DIR_H

/* Define if you have the <sys/file.h> header file.  */
#undef HAVE_SYS_FILE_H

/* Define if you have the <sys/ndir.h> header file.  */
#undef HAVE_SYS_NDIR_H

/* Define if you have the <sys/param.h> header file.  */
#undef HAVE_SYS_PARAM_H

/* Define if you have the <sys/resource.h> header file.  */
#undef HAVE_SYS_RESOURCE_H

/* Define if you have the <sys/select.h> header file.  */
#undef HAVE_SYS_SELECT_H

/* Define if you have the <sys/time.h> header file.  */
#undef HAVE_SYS_TIME_H

/* Define if you have the <sys/timeb.h> header file.  */
#define HAVE_SYS_TIMEB_H 1

/* Define if you have the <sys/times.h> header file.  */
#undef HAVE_SYS_TIMES_H

/* Define if you have the <unistd.h> header file.  */
#undef HAVE_UNISTD_H
