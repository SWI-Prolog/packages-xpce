dnl Process this file with autoconf to produce a configure script.

AC_INIT(Pce.in)
AC_PREREQ([2.50])

AC_CONFIG_HEADER(config.h)
AC_SUBST(YACC)
AC_SUBST(NETLIBS)
AC_SUBST(XLIB)
AC_SUBST(XINCLUDES)
AC_SUBST(XLIBS)
AC_SUBST(XOSLIB)
AC_SUBST(PCEHOME)
AC_SUBST(buildsubdir)
AC_SUBST(VMAJOR)
AC_SUBST(VMINOR)
AC_SUBST(VPATCH)
AC_SUBST(LDCONFIG)
AC_SUBST(MAKESHELL)
AC_SUBST(WINDRES)
AC_SUBST(VWS)
AC_SUBST(DLLIBVAR)
AC_SUBST(SWIPL)
AC_SUBST(PLWINNAME)
AC_SUBST(PLWINARG)
AC_SUBST(WINEPATHEXE)

dnl Link to X11.  MinGW versions overrule this later

WITHX11=true
VWS=x11

PLPKGDIR=../..
m4_include([../../ac_swi_c.m4])

case "$PL" in
   *swipl.sh)
	SWIPL=$(grep ^PL= $PL | sed 's/^PL=//')
	;;
   *)
	SWIPL=$(basename $PL .exe)
	;;
esac

case "$PLARCH" in
   *-win32|*-win64)
	PLWINNAME=swipl-win
	PLWINARG="-F swipl-win"
	AC_CHECK_PROG(WINEPATHEXE, winepath, winepath, ":")
	;;
   *)
	PLWINNAME="$SWIPL"
	PLWINARG=""
	WINEPATHEXE=":"
	;;
esac

changequote(,)dnl keep [] in sed expressions
VMAJOR=$(sed 's/\([0-9]*\).*/\1/' ../VERSION)
VMINOR=$(sed 's/[0-9]*\.\([0-9]*\).*/\1/' ../VERSION)
VPATCH=$(sed 's/[0-9]*\.[0-9]*\.\([0-9]*\).*/\1/' ../VERSION)
changequote([,])dnl

AC_CHECK_TOOLS(WINDRES, windres, ":")
AC_CHECK_PROGS(MAKESHELL, bash sh, "/bin/sh")
AC_CHECK_PROGS(MAKE, gmake make, "make")
AC_CHECK_PROGS(ETAGS, emacs-etags etags ctags, ":")
AC_CHECK_PROGS(YACC, bison yacc, yacc)
if test $YACC = "bison"; then YACC="bison -y"; fi

AC_PROG_CC
AC_PROG_LN_S
AC_ISC_POSIX
AC_HEADER_STDC
AC_C_INLINE
AC_FUNC_ALLOCA
AC_C_BIGENDIAN

# Determine the C compiler and executable suffix on the build system.
SWI_PROG_CC_FOR_BUILD
SWI_PROG_EXEEXT_FOR_BUILD

# Sometimes this is wrong !
PWD=`pwd`

AC_MSG_CHECKING("Path to installation")
PCEHOME=`dirname $PWD`
AC_MSG_RESULT("$PCEHOME")

AC_MSG_CHECKING("Path relative to $PCEHOME")
buildsubdir=`basename $PWD`
AC_MSG_RESULT("$buildsubdir")

if test "$GCC" = "yes"; then
  AC_CACHE_CHECK(whether __attribute__((visibility())) is supported,
		 libc_cv_visibility_attribute,
                 [cat > conftest.c <<EOF
                  int foo __attribute__ ((visibility ("hidden"))) = 1;
EOF
                  libc_cv_visibility_attribute=no
                  if ${CC-cc} -Werror -S conftest.c -o conftest.s >/dev/null 2>&1; then
                    if grep -E "\.(private_extern|hidden).*foo" conftest.s >/dev/null; then
                      libc_cv_visibility_attribute=yes
                    fi
                  fi
                  rm -f conftest.[cs]
                 ])
  if test $libc_cv_visibility_attribute = yes; then
    AC_DEFINE(HAVE_VISIBILITY_ATTRIBUTE, 1,
	      [Define if __attribute__ visibility is supported])
  fi
fi


AC_CHECK_SIZEOF(void*, 4)
AC_CHECK_SIZEOF(int,  4)
AC_CHECK_SIZEOF(long,  4)
AC_CHECK_SIZEOF(double, 8)

AC_CHECK_FUNC(socket, [], [
	     unset ac_cv_func_socket
             AC_CHECK_LIB(socket, socket,
                      [NETLIBS="$NETLIBS -lsocket"; LIBS="$LIBS -lsocket"]
                      AC_DEFINE(HAVE_LIBSOCKET, 1,
                      "Define if you have the socket library (-lsocket)."))])
AC_CHECK_FUNC(gethostent, [], [
	     unset ac_check_func_gethostent
             AC_CHECK_LIB(nsl, gethostent,
                      [NETLIBS="$NETLIBS -lnsl"]
                      AC_DEFINE(HAVE_LIBNSL, 1,
                      "Define if you have the nsl library (-lnsl)."))])

AC_CHECK_HEADERS(unistd.h string.h memory.h sys/time.h sys/file.h pwd.h)
AC_CHECK_HEADERS(sys/select.h sys/param.h malloc.h sys/resource.h stropts.h)
AC_CHECK_HEADERS(frame.h sys/timeb.h sys/times.h siginfo.h bstring.h)
AC_CHECK_HEADERS(sys/socketvar.h conio.h sys/access.h termios.h termio.h)
AC_CHECK_HEADERS(sched.h crt_externs.h poll.h)

case "$PLARCH" in
    *-win32|*-win64)
        ocflags="$CFLAGS"
        ocppflags="$CPPFLAGS"
        CFLAGS="$CFLAGS -I$PLINCL"
        CPPFLAGS="$CPPFLAGS -I$PLINCL"
        AC_CHECK_HEADERS(uxnt.h, [CIFLAGS="$CIFLAGS -I$PLINCL"])
        CFLAGS="$ocflags"
        CPPFLAGS="$ocppflags"
        AC_DEFINE(WIN32_GRAPHICS, 1, [Define to use WIN32 graphics interface])
        AC_DEFINE(FOR_MSW, 1, [Define to use XPM without X11 (MinGW)])
        AC_DEFINE(O_XOS, 1, [Define to use specific hooks])
        AC_DEFINE(O_DOSFILENAMES, 1, [Define if system supports DOS filenames])
        AC_DEFINE(O_HASSHARES, 1, [Define if system supports UNC filenames])
	AC_DEFINE(_REENTRANT, 1, [Define to support multi-threaded access])
	XOSLIB=-luxnt
        WITHX11=false
        VWS=msw
esac

case "$PLARCH" in
    *-win32)
	AC_DEFINE(INTPTR_FORMAT, "%d", [printf format for intptr_t])
esac

AC_CHECK_FUNCS(vsscanf on_exit memmove select popen strerror timelocal)
AC_CHECK_FUNCS(getdtablesize socket fork ftime getpid select getlogin)
AC_CHECK_FUNCS(getcwd setsid grantpt gethostname fstat gettimeofday)
AC_CHECK_FUNCS(rename atexit on_exit tmpnam sysinfo sigaction getpwnam)
AC_CHECK_FUNCS(shutdown mkstemp sched_yield mktime timegm nanosleep)
AC_CHECK_FUNCS(clock_gettime tempnam _NSGetEnviron poll)
AC_HEADER_TIME
AC_HEADER_DIRENT
AC_C_CHAR_UNSIGNED

AC_CHECK_TYPES(socklen_t, [], [],
[
#include <sys/types.h>
#include <sys/socket.h>
])

if test "$ac_cv_func_vsscanf" = "yes"; then
AC_MSG_CHECKING("cast to va_list")
AC_TRY_RUN(
[
#include <stdarg.h>
main()
{ long v[2];
  void *ptr[2];

  ptr[0] = &v[0];
  ptr[1] = &v[1];
  vsscanf("34 45", "%d%d", (va_list)ptr);
  if ( v[0] == 34 && v[1] == 45 )
    exit(0);

  exit(1);
}], AC_DEFINE(HAVE_CAST_VA_LIST, 1,
	      "Define if we can cast to va_list (say no if you are not sure)")
    AC_MSG_RESULT("yes"),
    AC_MSG_RESULT("no"),
    AC_MSG_RESULT("assuming no"))
fi

if test "$ac_cv_func_ftime" != "yes"; then
AC_MSG_CHECKING("tm_gmtoff")
AC_TRY_COMPILE(
[
#include <sys/types.h>
#include <time.h>
],
[ struct tm *tm;
  int i;

  i = tm->tm_gmtoff;
], AC_DEFINE(HAVE_TM_GMTOFF, 1,
	     "Define if struct tm has tm_gmtoff field")
   AC_MSG_RESULT("yes"),
   AC_MSG_RESULT("no"))
fi

AC_TYPE_SIGNAL
AC_MSG_CHECKING("for BSD signal handlers")
AC_TRY_RUN(
[
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
static int signalled;
static RETSIGTYPE catch (s) int s; { signalled = 1; }
main()
{ signal(SIGINT, catch);
  kill(getpid(), SIGINT);
  while(!signalled)
    ;
  if ( signal(SIGINT, catch) == catch )
    exit(0);
  exit(1);
}], AC_DEFINE(BSD_SIGNALS, 1,
	      "Define if you have BSD signals (i.e. handler does not reset)")
    AC_MSG_RESULT("BSD signals"),
    AC_MSG_RESULT("Posix signals"),
    AC_MSG_RESULT("assuming Posix signals"))

dnl The code below checks whether we can use siginfo () to set a signal
dnl handler that is passed the child's pid.  This code works on Solaris,
dnl but on irix, it compiles, but the handler is not passed the proper
dnl information.  Therefore we need to run the program.  If this fails,
dnl the system will fall back to use signal() and waitpid(), but this
dnl is more clumsy, and fails to work properly on Solaris.

AC_MSG_CHECKING("for usable siginfo")
AC_TRY_RUN(
[
#include <stdio.h>
#include <signal.h>
#include <siginfo.h>
#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef SA_NOMASK
#define SA_NOMASK 0
#endif

int child;

static void
child_changed(int sig, siginfo_t *info, void *uctx)
{ fprintf(stderr, "child_changed() called, child = %d, info = %p\n",
	  child, info);
  if ( info && info->si_pid == child )
    exit(0);

  exit(1);
}

int
main()
{ struct sigaction action, oaction;

  memset((char *) &action, 0, sizeof(action));
  action.sa_sigaction = child_changed;
  action.sa_flags     = SA_SIGINFO|SA_NOMASK|SA_RESTART;
  sigaction(SIGCHLD, &action, &oaction);

  if ( (child = fork()) == 0 )
  { sleep(1);
    exit(0);
  }

  sleep(8);
  return 1;
}
], AC_DEFINE(USE_SIGINFO, 1,
	     "Define if sigaction() supports SA_SIGINFO and passes thechild pid in info")
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no),
   AC_MSG_RESULT(assuming no))


AC_MSG_CHECKING("for union wait")
if test "$GCC" = yes; then
    ac_oldcflags="$CFLAGS"
    CFLAGS="$CFLAGS -Werror"
fi
AC_TRY_COMPILE(
[
#include <sys/wait.h>
],
[ union wait status;
  waitpid(1, &status, WNOHANG);
], AC_DEFINE(UNION_WAIT, 1,
	     "Define if wait() uses union wait* for the 2nd argument.")
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))
if test "$GCC" = "yes"; then CFLAGS="$ac_oldcflags"; fi

AC_MSG_CHECKING("for tagged lvalues")
AC_TRY_COMPILE(
[
],
[ int x;
  *(char *)&x = 'a';
  exit(0);
], AC_DEFINE(TAGGED_LVALUE, 1,
	     "Define if (type)pointer = value is allowed")
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

AC_MSG_CHECKING("Checking struct termios.c_line")
AC_TRY_COMPILE(
[
#include <termios.h>
],
[ struct termios buf;
  buf.c_line = 0;
  exit(0);
], AC_DEFINE(TERMIOS_HAS_C_LINE, 1,
	     "Define if struct termios has a member c_line")
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))


AC_MSG_CHECKING('ioctl(line, I_PUSH, "ttcompat")')
AC_TRY_RUN(
[
#include <stdio.h>
#include <fcntl.h>
#include <stropts.h>
main()
{ int line;

  if ( (line = open("/dev/tty", O_RDWR)) >= 0 &&
       ioctl(line, I_PUSH, "ttcompat") == 0 )
    exit(0);
  exit(1);
}
], AC_DEFINE(HAVE_TTCOMPAT, 1,
	     "Define if iocll(tty, I_PUSH, "ttcompat") works on your machine")
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no),
   AC_MSG_RESULT(assuming no))

case "$PLARCH" in
    *-win32|*-win64)
	AC_CHECK_LIB(pthreadGC2, pthread_create,
                     [NETLIBS="$NETLIBS -lpthreadGC2"])
        if test ! "$ac_cv_lib_pthreadGC2_pthread_create" = "yes"; then
          AC_CHECK_LIB(pthreadGC, pthread_create,
                       [NETLIBS="$NETLIBS -lpthreadGC"])
          if test ! "$ac_cv_lib_pthreadGC_pthread_create" = "yes"; then
            AC_CHECK_LIB(pthread, pthread_create,
                         [NETLIBS="$NETLIBS -lpthread"])
          fi
        fi
        AC_CHECK_LIB(gdi32, main,
                     [NETLIBS="$NETLIBS -lgdi32"])
        AC_CHECK_LIB(comdlg32, main,
                     [NETLIBS="$NETLIBS -lcomdlg32"])
        AC_CHECK_LIB(ole32, main,
                     [NETLIBS="$NETLIBS -lole32"])
        AC_CHECK_LIB(shell32, main,
                     [NETLIBS="$NETLIBS -lshell32"])
        AC_CHECK_LIB(ws2_32, main,
                     [AC_DEFINE(HAVE_WINSOCK, 1, "Define if you have the Windows socket library")
                      AC_DEFINE(USE_WINSOCK2, 1, "Define if you have the winsock2 library")
                      NETLIBS="$NETLIBS -lws2_32"])
	;;
esac

dnl ****************************************************************
dnl * X11 configuration
dnl ****************************************************************

if test "$WITHX11" = true; then
BEFOREXLIBS="$LIBS"

AC_PATH_X

if test "$x_libraries" = "" -o "$x_libraries" = NONE; then
  AC_MSG_CHECKING("Checking for XQuartz");
  if test -r /opt/X11/lib/libX11.dylib; then
    XLIB=/opt/X11/lib
    AC_MSG_RESULT($XLIB);
  else
    AC_MSG_RESULT("no");
    XLIB=/usr/X11/lib
  fi
else
  XLIB="$x_libraries"
fi
XLIBS="-L$XLIB -lXt -lX11"

if test "$x_includes" = "" -o "$x_includes" = NONE; then
  if test -r /opt/X11/include/X11/Xlib.h; then
    XINCLUDES="-I/opt/X11/include"
  else
    XINCLUDES="-I/usr/include"
  fi
else
  XINCLUDES="-I$x_includes"
fi

dnl echo XLIBS: $XLIBS, XINCLUDES $XINCLUDES

ac_oldlibs="$LIBS"
if test ! -z "$x_libraries"; then
  LIBS="-L$x_libraries $LIBS"
fi
XSMLIBS=""
AC_CHECK_LIB(SM, main,
	     XSMLIBS="-lSM -lICE",
	     ,
	     $XLIBS -lICE)
XLIBS="$XLIBS $XSMLIBS"

if test ! -z "$x_libraries"; then
  LIBS="-L$x_libraries $XLIBS $ac_oldlibs"
else
  LIBS="$XLIBS $ac_oldlibs"
fi

dnl -lXext appears needed on Debian.  Wat does it do?
AC_CHECK_LIB(Xext, main, [XLIBS="-lXext $XLIBS"],)

AC_CHECK_LIB(Xinerama, main,
	     [XLIBS="-lXinerama $XLIBS"]
	     AC_DEFINE(HAVE_LIBXINERAMA, 1,
		       "Define if you have the Xinerama library"))
AC_CHECK_HEADERS(X11/extensions/Xinerama.h)

echo "	XLIBS=$XLIBS"

CPPFLAGS="$XINCLUDES ${CPPFLAGS}"
CIFLAGS="$XINCLUDES ${CIFLAGS}"

fi dnl WITHX11

dnl Check the critical but not very standard libXpm and libjpeg libraries
dnl and make sure they required headers can be found.
dnl
dnl NOTE: If we have to add a -L, it should be *before* the -l for some
dnl linkers

XLIBDIRS=""

AC_ARG_WITH(xpm,[  --with-xpm=<dir>	  Location of Xpm include/lib],
	[case $withval in
		no|yes)	echo 'Specify dir for XPM please';
			exit 1 ;;
		*)	XLIBDIRS="-L${withval}/lib $XLIBDIRS"
		        CFLAGS="-I${withval}/include ${CFLAGS}"
			CPPFLAGS="-I${withval}/include ${CPPFLAGS}"
			CIFLAGS="-I${withval}/include ${CIFLAGS}"
			echo "LIBS=${LIBS}"
	esac])
AC_ARG_WITH(jpeg,[  --with-jpeg=<dir>	  Location of JPEG include/lib],
	[case $withval in
		no|yes)	echo 'Specify dir for JPEG please';
			exit 1 ;;
		*)	XLIBDIRS="-L${withval}/lib $XLIBDIRS"
			CPPFLAGS="-I${withval}/include ${CPPFLAGS}"
			CIFLAGS="-I${withval}/include ${CIFLAGS}"
			echo "LIBS=${LIBS}"
	esac])

dnl Clear the cache if the answer is no so re-running configure
dnl after fixing the problem works fine without removing the cache file

if test "$ac_cv_lib_Xpm_main" != yes; then unset ac_cv_lib_Xpm_main; fi
if test "$ac_cv_lib_jpeg_main" != yes; then unset ac_cv_lib_jpeg_main; fi
if test "$ac_cv_header_X11_xpm_h" != yes; then unset ac_cv_header_X11_xpm_h; fi
if test "$ac_cv_header_jpeglib_h" != yes; then unset ac_cv_header_jpeglib_h; fi
if test "$ac_cv_header_jerror_h" != yes; then unset ac_cv_header_jerror_h; fi

LIBS="$XLIBDIRS $LIBS"

AC_CHECK_LIB(jpeg, main,
	     [XLIBS="-ljpeg $XLIBS"]
	     AC_DEFINE(HAVE_LIBJPEG, 1,
		       "Define if you have the jpeg library (-ljpeg)"))

AC_CHECK_LIB(Xpm, main,
	     [XLIBS="-lXpm $XLIBS"]
	     AC_DEFINE(HAVE_LIBXPM, 1,
		       "Define if you have the Xpm library (-lXpm)."))

XLIBS="$XLIBDIRS $XLIBS"

if test "$ac_cv_lib_Xpm_main" != "yes"; then
cat << _EOM_
*****************************************************************
* ERROR: Could not load/find library -lXpm.			*
* Please install the XPM library and rerun configure.		*
*								*
* Please check the following:					*
*								*
*	- Do you have the development version of the library?	*
*	- Is the library you have of the right architecture?	*
*	- Could it be that other parts of X11 are missing?	*
*	  (typically the case if -lXext is also missing)	*
*								*
*	Examine packages/xpce/src/config.log for details	*
*	Linux users: check README.linux for dependencies	*
*	(note that the exact package names vary between		*
*	distributions and versions).				*
*****************************************************************
_EOM_
exit 1
fi

AC_CHECK_HEADERS(X11/xpm.h)
if test "$ac_cv_header_X11_xpm_h" != yes; then
cat << _EOM_
*****************************************************************
* ERROR: Could not find <X11/xpm.h>				*
* Please install the XPM development files and rerun configure.	*
* The primary source for libXpm is at:				*
*								*
*     http://www-sop.inria.fr/koala/lehors/xpm.html		*
*****************************************************************
_EOM_
exit 1
fi

if test "$ac_cv_lib_jpeg_main" != "yes"; then
cat << _EOM_
*****************************************************************
* ERROR: Could not find library -ljpeg.				*
* Please install the JPEG library and rerun configure.		*
* The primary source for libjpeg is at:				*
*								*
*     http://www.ijg.org/					*
*****************************************************************
_EOM_
exit 1
fi

AC_CHECK_HEADERS(jpeglib.h jerror.h)
if test "$ac_cv_header_jpeglib_h" != yes -o \
        "$ac_cv_header_jerror_h" != yes; then
cat << _EOM_
*****************************************************************
* ERROR: Could not find <jpeglib.h> or <jerror.h>		*
* Please install the JPEG development files and rerun configure.*
* The primary source for libjpeg is at:				*
*								*
*     http://www.ijg.org/					*
*****************************************************************
_EOM_
exit 1
fi

if test "$WITHX11" = true; then
  AC_CHECK_FUNCS(XtPopupSpringLoaded XmIsMotifWMRunning XInitThreads)
  LIBS="$ac_oldlibs"
fi

dnl ****************************************************************
dnl * X11 extensions
dnl * XFT: The Xfree font management system
dnl ****************************************************************

if test "$WITHX11" = true; then
AC_CHECK_PROGS(XFTCONFIG, pkg-config xft-config, "false")
case "$XFTCONFIG" in
	pkg-config)
		if pkg-config --exists xft fontconfig; then
			AC_DEFINE(USE_XFT, 1,
				  "Define to use the Xft scalable and anti-aliasing font system")
			XLIBS="$XLIBS `pkg-config --libs xft fontconfig`"
			XINCLUDES="$XINCLUDES `pkg-config --cflags xft fontconfig`"
			ac_xft=true
		fi
		;;
	xft-config)
		AC_DEFINE(USE_XFT, 1,
			  "Define to use the Xft scalable and anti-aliasing font system")
		XLIBS="$XLIBS `$XFTCONFIG --libs`"
		XINCLUDES="$XINCLUDES `$XFTCONFIG --cflags`"
		ac_xft=true
		;;
	false)
		;;
esac

dnl XftNameUnparse() is not available in all versions
if test "$ac_xft" = true; then
    oldlibs="$LIBS"
    LIBS="$XLIBS $LIBS"
    AC_CHECK_FUNCS(XftNameUnparse)
    LIBS="$oldlibs"
else
cat << _EOM_
*****************************************************************
* ERROR: Could not load/find library -lXft.			*
* Please install the freetype libraries and rerun configure.	*
* The primary source for the freetype system is:		*
*								*
*	http://www.freetype.org/				*
*								*
* NOTE: On Unix systems, you typically need to install		*
*								*
*	pkg-config						*
*	freetype development packages				*
*	fontconfig development packages				*
*								*
* If all is installed properly, pkg-config should output	*
* something like this (the details may vary):			*
*								*
*	% pkg-config --libs xft					*
*	-lXft -lXrender -lfontconfig -lfreetype -lz -lX11	*
*								*
* Examine packages/xpce/src/config.log for details		*
* Linux users: check README.debian for dependencies		*
* (note that the exact package names vary between distributions)*
*****************************************************************
_EOM_
exit 1
fi
fi


dnl ****************************************************************
dnl * Final tests
dnl ****************************************************************

AC_MSG_CHECKING("memory model")
echo '#include "confdefs.h"' > conftest.c
cat $srcdir/test/m-model.c >> conftest.c
(eval $ac_link) 2>&5
if test -s conftest && eval `./conftest`; then
    msg=
    if test ! -z "$POINTER_OFFSET"; then
	AC_DEFINE_UNQUOTED(POINTER_OFFSET, $POINTER_OFFSET,
			   "Define if data-pointer is in high memory")
	msg="$msg POINTER_OFFSET=$POINTER_OFFSET"
    fi
    if test -z "$msg"; then msg="yes"; fi
    AC_MSG_RESULT("$msg")
else
    AC_MSG_RESULT("Could not compile m-model.c")
fi
rm -rf conftest*

dnl See which parts of the sources are there, and check the Makefiles we
dnl need to update.  You cannot call AC_OUTPUT more then once!

Makefiles="Makefile xpce.sh"
Makedirs="../quintus/src ../prolog/pwboot ../sicstus/src ../prolog/lib"
OptionalMakefiles="../swipl/Makefile.bin"
for d in $Makedirs; do
    if test -r $d/Makefile.in; then
	Makefiles="$Makefiles $d/Makefile"
    fi
done
for f in $OptionalMakefiles; do
    if test -r $f.in; then
	Makefiles="$Makefiles $f"
    fi
done

dnl autoconf 2.5.3 doesn't handle relative srcdir properly.  We'll hack
dnl around making this variable global.

srcdir=`cd $srcdir && pwd`

AC_CONFIG_COMMANDS([exe],
		   [ chmod +x xpce.sh
		   ])
AC_OUTPUT($Makefiles)

