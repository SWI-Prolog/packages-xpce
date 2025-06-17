include(Sockets)
include(CheckTypeSize)
include(TestBSDSignals)

string(TOLOWER ${CMAKE_HOST_SYSTEM_NAME} OS)

set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${GUI_LIBRARIES})
set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES} ${GUI_INCLUDE_DIRS})

check_type_size("int" SIZEOF_INT)
check_type_size("long" SIZEOF_LONG)
check_type_size("void *" SIZEOF_VOIDP)
check_type_size("double" SIZEOF_DOUBLE)

AC_CHECK_HEADERS(unistd.h string.h memory.h time.h sys/time.h sys/file.h pwd.h)
AC_CHECK_HEADERS(sys/select.h sys/param.h malloc.h sys/resource.h stropts.h)
AC_CHECK_HEADERS(frame.h sys/timeb.h sys/times.h siginfo.h bstring.h)
AC_CHECK_HEADERS(sys/socketvar.h conio.h sys/access.h termios.h termio.h)
AC_CHECK_HEADERS(sched.h crt_externs.h poll.h jerror.h sys/socket.h)
AC_CHECK_HEADERS(X11/extensions/Xinerama.h)
AC_CHECK_HEADERS(X11/extensions/Xrandr.h)

AC_CHECK_FUNCS(vsscanf on_exit memmove select popen strerror timelocal)
AC_CHECK_FUNCS(getdtablesize socket fork ftime getpid select getlogin)
AC_CHECK_FUNCS(getcwd setsid grantpt gethostname fstat gettimeofday)
AC_CHECK_FUNCS(rename atexit on_exit tmpnam sysinfo sigaction getpwnam)
AC_CHECK_FUNCS(shutdown mkstemp sched_yield mktime timegm nanosleep)
AC_CHECK_FUNCS(clock_gettime tempnam _NSGetEnviron poll signal sigaction)
AC_CHECK_FUNCS(pipe posix_openpt)

check_struct_has_member("struct tm" tm_gmtoff time.h HAVE_TM_GMTOFF)
check_struct_has_member("struct termios" c_line termios.h TERMIOS_HAS_C_LINE)

if(HAVE_SIGNAL AND NOT HAVE_SIGACTION)
  include(TestBSDSignals)
endif()

#FIXME usable siginfo
