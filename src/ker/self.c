/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@uva.nl
    WWW:	   http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 1985-2009, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#if !defined(__WINDOWS__) && !defined(__WINDOWS__)
#include <version.h>			/* get MACHINE and PCE_VERSION */
#endif
#include <h/kernel.h>
#include "alloc.h"
#include <h/interface.h>
#include <h/trace.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <time.h>
#include <h/graphics.h>
#include <h/unix.h>
#include <errno.h>

#ifdef __WINDOWS__
#undef MACHINE
#define MACHINE "i386"
#undef OS
#define OS ws_os()
#endif

#if (defined(__sun__) && !STDC_HEADERS)
extern int gethostname(char *__name, size_t __len);
#endif

static void	callExitMessagesPce(int stat, Pce pce);
static void	exit_pce(int);
#ifdef HAVE_ON_EXIT
static void	run_pce_onexit_hooks(int, void *);
#else
#ifdef HAVE_ATEXIT
static void	run_pce_atexit_hooks(void);
#endif
#endif

static int
setAppDataPce(Pce pce)
{ Name appdataname;

#ifdef __WINDOWS__
  if ( !(appdataname = ws_appdata("xpce")) )
    appdataname = CtoName("~/.xpce");
#else
  appdataname = CtoName("~/.xpce");
#endif
  assign(pce, application_data, newObject(ClassDirectory, appdataname, EAV));

  succeed;
}



/* The MacOS X hack.  The mac loader doesn't want to load ker/glob.o, from
   libXPCE.a as it only contains common variables.  This is fixed by adding
   a function and calling it from here.  See __APPLE__ below.
*/

static status
initialisePce(Pce pce)
{ if ( PCE && notNil(PCE) )
    return errorPce(classOfObject(pce), NAME_cannotCreateInstances);

  PCE = pce;

#ifdef __APPLE__
 { extern int IAmAGlobalFunctionToMakeMeLoad(void);

   (void)IAmAGlobalFunctionToMakeMeLoad();
 }
#endif

  assert( sizeof(string) == 2*sizeof(void*) );

#ifndef O_RUNTIME
  assign(pce, debugging,              OFF);
  assign(pce, trap_errors,	      ON);
#endif
  assign(pce, catched_errors,	      newObject(ClassChain, EAV));
  assign(pce, catch_error_signals,    OFF);

  assign(pce, exit_messages,	      newObject(ClassChain, EAV));
  assign(pce, exception_handlers,     newObject(ClassSheet, EAV));

  assign(pce, home,		      DEFAULT);
  assign(pce, defaults,		      CtoString("$PCEHOME/Defaults"));
  setAppDataPce(pce);
  assign(pce, version,                CtoName(PCE_VERSION));
  assign(pce, machine,                CtoName(MACHINE));
  assign(pce, operating_system,       CtoName(OS));
#ifdef WIN32_GRAPHICS
  assign(pce, window_system,	      NAME_windows);
#else
  assign(pce, window_system,	      CtoName("X"));
#endif
  assign(pce, window_system_version,  toInt(ws_version()));
  assign(pce, window_system_revision, toInt(ws_revision()));
  assign(pce, features,		      newObject(ClassChain, EAV));

  at_pce_exit(exit_pce, ATEXIT_FIFO);

  initPublicInterface();

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unicode note. Cprintf for  SWI-Prolog  maps   to  Svfprintf(),  which is
designed to accept the argument of %Us as a UTF-8 string.  Such a string
is produced by charArrayToUTF8() as well as pp().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
writePcev(Pce pce, int argc, Any *argv)
{ int i;

  for(i=0; i<argc; i++)
  { if ( i > 0 )
      Cputchar(' ');

    if ( instanceOfObject(argv[i], ClassCharArray) )
      Cprintf("%Us", charArrayToUTF8(argv[i]));
    else if ( isInteger(argv[i]) )
      Cprintf("%ld", valInt(argv[i]));
    else if ( instanceOfObject(argv[i], ClassReal) )
      Cprintf("%g", valReal(argv[i]));
    else
    { char *s = pp(argv[i]);
      Cprintf("%Us", s);
    }
  }

  succeed;
}


static status
writeLnPcev(Pce pce, int argc, Any *argv)
{ writePcev(pce, argc, argv);
  Cputchar('\n');

  succeed;
}


status
formatPcev(Pce pce, CharArray fmt, int argc, Any *argv)
{ string s;

  str_writefv(&s, fmt, argc, argv);
  Cputstr(&s);
  str_unalloc(&s);

  succeed;
}

		 /*******************************
		 *      CONSOLE OPERATIONS	*
		 *******************************/

static status
showConsolePce(Pce pce, Name how)
{ return ws_show_console(how);
}


static status
exposeConsolePce(Pce pce)
{ return showConsolePce(pce, NAME_open);
}


static status
iconifyConsolePce(Pce pce)
{ return showConsolePce(pce, NAME_iconic);
}


static status
consoleLabelPce(Pce pce, CharArray title)
{ ws_console_label(title);

  succeed;
}


		 /*******************************
		 *	  ERROR HANDLING	*
		 *******************************/

Name
getOsErrorPce(Pce pce)
{
#if HAVE_STRERROR
#ifdef __WINDOWS__
  return CtoName(strerror(_xos_errno()));
#else
  return CtoName(strerror(errno));
#endif
#else /*HAVE_STRERROR*/
  static char errmsg[64];
  extern int sys_nerr;
  extern char *sys_errlist[];
  extern int errno;

  if ( errno < sys_nerr )
    return CtoName(sys_errlist[errno]);

  sprintf(errmsg, "Unknown OS Error (%d)", errno);
  return CtoName(errmsg);
#endif /*HAVE_STRERROR*/
}

#ifndef O_RUNTIME
static Chain
getUnresolvedTypesPce(Pce pce)
{ Chain ch = answerObject(ClassChain, EAV);

  for_hash_table(TypeTable, s,
	         { Type t = s->value;
		   if ( t->kind == NAME_class )
		   { Class class = t->context;
		     if ( isNil(class->super_class) )
		       appendChain(ch, t);
		     if ( isName(class) )
		     { if ( (class = getMemberHashTable(classTable, class)) )
			 assign(t, context, class);
		       else
			 appendChain(ch, t);
		     }
		   }
		 });

  answer(ch);
}
#endif /*O_RUNTIME*/


static status
crashPce(Pce pce)
{ int *p = 0;
  *p = 1;
  fail;					/* 'ed to crash ... */
}


status
catchErrorPce(Pce pce, Any ids)
{ assign(pce, last_error, NIL);

  return prependChain(pce->catched_errors, ids);
}


status
catchPopPce(Pce pce)
{ return deleteHeadChain(pce->catched_errors);
}


status
catchedErrorPce(Pce pce, Name id)
{ Cell cell;

  for_cell(cell, pce->catched_errors)
  { if ( isDefault(cell->value) )
      succeed;				/* catch all of them */

    if ( (Name) cell->value == id )
      succeed;

    if ( instanceOfObject(cell->value, ClassChain) &&
	 memberChain(cell->value, id) )
      succeed;
  }

  fail;
}


static Name
getHomePce(Pce pce)
{ if ( isDefault(pce->home) )
  { char *h;

    if ( (h=getenv("PCEHOME")) )
      assign(pce, home, CtoName(h));
    else
      assign(pce, home, CtoName("/usr/local/lib/xpce"));
  }

  answer(pce->home);
}


#ifdef __WINDOWS__
#define vareq(s, q) (stricmp(s, q) == 0)
#else
#define vareq(s, q) (strcmp(s,q) == 0)
#endif

Name
getEnvironmentVariablePce(Pce pce, Name name)
{ char *s;

  if ( (s = getenv(strName(name))) )
    answer(CtoName(s));
  if ( vareq(strName(name), "PCEHOME") )
    answer(get(PCE, NAME_home, EAV));
  if ( vareq(strName(name), "PCEAPPDATA") )
  { Directory dir = get(PCE, NAME_applicationData, EAV);

    if ( dir )
      return get(dir, NAME_path, EAV);
  }

  fail;
}


static status
exitMessagePce(Pce pce, Code code)
{ return prependChain(pce->exit_messages, code);
}


static void
callExitMessagesPce(int stat, Pce pce)
{ static int done = 0;

  if ( !done++ && pce && notNil(pce) )
  { Cell cell, q;

    for_cell_save(cell, q, pce->exit_messages)
    { addCodeReference(cell->value);
      forwardCode(cell->value, toInt(stat), EAV);
    }
  }
}


static void
exit_pce(int rval)
{ callExitMessagesPce(rval, PCE);
}

#ifdef HAVE_ON_EXIT
static void
run_pce_onexit_hooks(int rval, void *context)
{ run_pce_exit_hooks(rval);
}
#else

#ifdef HAVE_ATEXIT
static void				/* for usage with ANSI atexit() */
run_pce_atexit_hooks(void)
{ run_pce_exit_hooks(0);
}
#endif
#endif


		/********************************
		*            DEBUGGING		*
		********************************/

static BoolObj
getIsRuntimeSystemPce(Pce pce)
{
#ifdef O_RUNTIME
  answer(ON);
#else
  answer(OFF);
#endif
}


static status
debuggingSubjectPce(Pce pce, Name what)
{
#ifndef O_RUNTIME
  if ( PCEdebugging && memberChain(PCEdebugSubjects, what) )
    succeed;
#endif
  fail;
}


#ifndef O_RUNTIME

static status
debugSubjectPce(Pce pce, Name what)
{ if ( !memberChain(PCEdebugSubjects, what) )
    appendChain(PCEdebugSubjects, what);

  return debuggingPce(pce, ON);
}


static status
nodebugSubjectPce(Pce pce, Name what)
{ deleteChain(PCEdebugSubjects, what);

  succeed;
}


status
debuggingPce(Pce pce, BoolObj val)
{ assign(pce, debugging, val);

  PCEdebugging = (PCE->debugging == ON);

  succeed;
}


static status
trapErrorsPce(Pce pce, BoolObj trap)
{ assign(pce, trap_errors, trap);

  succeed;
}


static status
printStackPce(Pce pce, Int depth)
{ int n = isDefault(depth) ? 5 : valInt(depth);

  pceBackTrace(NULL, n);

  succeed;
}
#endif /*O_RUNTIME*/


static status
maxGoalDepthPce(Pce pce, Int depth)
{ MaxGoalDepth = (isInteger(depth) ? valInt(depth) : NO_MAX_GOAL_DEPTH);

  succeed;
}


static Int
getMaxGoalDepthPce(Pce pce)
{ answer(MaxGoalDepth == NO_MAX_GOAL_DEPTH ? NIL : toInt(MaxGoalDepth));
}



		/********************************
		*           EXCEPTIONS		*
		********************************/

status
exceptionPcev(Pce pce, Name name, int argc, Any *argv)
{ Code msg;

  if ( (msg = checkType(getValueSheet(pce->exception_handlers, (Any)name),
			TypeCode, pce)) )
    return forwardCodev(msg, argc, argv);

  fail;
}


status
exceptionPce(Pce pce, Name kind, ...)
{ va_list args;
  Any argv[VA_PCE_MAX_ARGS];
  int argc = 0;

  va_start(args, kind);
  for(; (argv[argc] = va_arg(args, Any)) != NULL; argc++)
    assert(argc <= VA_PCE_MAX_ARGS);
  va_end(args);

  return exceptionPcev(pce, kind, argc, argv);
}


		/********************************
		*           STATISTICS		*
		********************************/

int
getFileDesCount(void)
{
#ifdef HAVE_GETDTABLESIZE
   return getdtablesize();
#else
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
  struct rlimit rlp;
  (void) getrlimit(RLIMIT_NOFILE, &rlp);

  return (rlp.rlim_cur);
#else
#ifdef OPEN_MAX
  return OPEN_MAX;
#else
#ifdef FOPEN_MAX
  return FOPEN_MAX;
#else
#ifdef _NFILE
  return _NFILE;
#endif
#endif
#endif
  return 32;				/* don't know */
#endif
#endif
}


static Int
getFdPce(Pce pce)
{
#if defined(HAVE_FSTAT) || defined(__linux)
  int i, cntr = 0;
  struct stat buf;
  int mx = getFileDesCount();

  for (i=0; i<mx; i++)
  { if (fstat(i, &buf) == -1)
    { cntr++;
      continue;
    }
  }
  answer(toInt(cntr));
#else
  return toInt(ws_free_file_descriptors());
#endif
}


static Int
getCoreUsagePce(Pce pce)
{ answer(toInt(allocbytes));
}


static Int
getWastedCorePce(Pce pce)
{ answer(toInt(wastedbytes));
}


static Int
getDeferredUnallocedPce(Pce pce)
{ answer(toInt(deferredUnalloced));
}


static Int
getAnswerStackSizePce(Pce pce)
{ answer(countAnswerStack());
}


static Int
getNoCreatedPce(Pce pce)
{ answer(getNoCreatedClass(ClassObject, ON));
}


static Int
getNoFreedPce(Pce pce)
{ answer(getNoFreedClass(ClassObject, ON));
}


static status
bannerPce(Pce pce)
{ Name host = get(HostObject(), NAME_system, EAV);

#ifdef __WINDOWS__
#ifdef WIN64
  writef("XPCE %s for %I%IWin64: XP 64-bit edition%I%I\n",
#else
  writef("XPCE %s for %I%IWin32: NT,2000,XP%I%I\n",
#endif
#else
  writef("XPCE %s for %s-%s and X%dR%d\n",
#endif
	 pce->version,
	 pce->machine,
	 pce->operating_system,
	 pce->window_system_version,
	 pce->window_system_revision);
  writef("Copyright (C) 1993-2009 University of Amsterdam.\n"
	 "XPCE comes with ABSOLUTELY NO WARRANTY. "
	 "This is free software,\nand you are welcome to redistribute it "
	 "under certain conditions.\n");

  if ( host != NAME_unknown )
    writef("The host-language is %s\n", host);

  succeed;
}


static Int
count_subclasses(Class class)
{ Int rval = ONE;
  Cell cell;

  if ( notNil(class->sub_classes) )
    for_cell(cell, class->sub_classes)
      rval = add(rval, count_subclasses(cell->value));

  return rval;
}


static status
infoPce(Pce pce)
{ int classes;

  classes = valInt(count_subclasses(ClassObject));

  writef("Version:\n");
  writef("	Release:            %s\n", pce->version);
  writef("	System:             %s\n", pce->machine);
  writef("	Operating System:   %s\n", pce->operating_system);
#ifdef __WINDOWS__				/* TBD: Cygwin? */
  writef("	Window System:      windows %s.%s\n",
	 pce->window_system_version,
	 pce->window_system_revision);
#else
  writef("	Window System:      X%sR%s\n",
	 pce->window_system_version,
	 pce->window_system_revision);
#endif
  writef("\n");
  writef("Memory allocation:\n");
  writef("	Core in use:        %d Bytes\n", getCoreUsagePce(pce));
  writef("	Core wasted:        %d Bytes\n", getWastedCorePce(pce));
  writef("	Objects allocated:  %d\n",       getNoCreatedPce(pce));
  writef("	Objects freed:	    %d\n",       getNoFreedPce(pce));
  writef("	Objects in use:	    %d\n",       sub(getNoCreatedPce(pce),
						     getNoFreedPce(pce)));
  writef("\n");
  writef("Other info:\n");
  writef("	Classes:            %d\n", toInt(classes));
  writef("\n");
  writef("Designed and implemented by:\n");
  writef("	Anjo Anjewierden\n");
  writef("	Jan Wielemaker\n");
  writef("\n");

  bannerPce(PCE);

  succeed;
}

#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>

static Real
getCpuTimePce(Pce pce, Name which)
{ struct tms buffer;
  float f;

  times(&buffer);
  if ( which == NAME_user )
    f = (float) buffer.tms_utime / 60.0;
  else if ( which == NAME_system )
    f = (float) buffer.tms_stime / 60.0;
  else
    f = (float) (buffer.tms_utime + buffer.tms_stime) / 60.0;

  answer(CtoReal(f));
}

#else /*HAVE_SYS_TIMES_H*/

/* DOS/Windows version */

static Real
getCpuTimePce(Pce pce, Name which)
{				/* TBD: warn on bad type? */
  return CtoReal((float) clock()/(float)CLOCKS_PER_SEC);
}

#endif /*HAVE_SYS_TIMES_H*/

static Int
getMaxIntegerPce(Pce pce)
{ answer(toInt(PCE_MAX_INT));
}


static Int
getMinIntegerPce(Pce pce)
{ answer(toInt(PCE_MIN_INT));
}


static status
featurePce(Pce pce, Any feature)
{ return appendChain(pce->features, feature);
}


static status
hasFeaturePce(Pce pce, Any feature)
{ return memberChain(pce->features, feature);
}


static StringObj
getDatePce(Pce pce)
{ time_t clock;
  char tmp[27];

  clock = time(0);
  strcpy(tmp, ctime(&clock));
  tmp[24] = '\0';
  answer(CtoString(tmp));
}


static Int
getMclockPce(Pce pce)
{ return toInt(mclock());
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Linux sysinfo() is something completely  different from Solaris sysinfo,
for which this code was designed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __linux__
#undef HAVE_SYSINFO
#endif

#if !defined(HAVE_GETHOSTNAME) || defined(HAVE_SYSINFO)
#undef gethostname
#define gethostname _emu_gethostname
#ifdef HAVE_SYSINFO			/* solaris */
#include <sys/systeminfo.h>
#endif

static int
_emu_gethostname(char *buf, int len)
{
#ifdef HAVE_SYSINFO
  return sysinfo(SI_HOSTNAME, buf, len) > 0 ? 0 : -1;
#else
  char *s;

  if ( (s = getenv("HOSTNAME")) != NULL )
    strcpy(buf, s);
  else
    strcpy(buf, "doshost");
#endif

  return 0;
}

#endif /*HAVE_GETHOSTNAME*/

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

static Name
getUserPce(Pce pce)
{ char *s;

  if ( (s = ws_user()) )
    answer(CtoName(s));
#if HAVE_PWD_H
  { struct passwd *pwd;

    if ( (pwd = getpwuid(getuid())) )
      answer(CtoName(pwd->pw_name));
  }
#endif

  answer(NAME_unknown);
}


static Any
getUserInfoPce(Pce pce, Name what, Name user)
{
#if HAVE_PWD_H
  struct passwd *pwd;

  if ( isDefault(user) )
    pwd = getpwuid(getuid());
  else
    pwd = getpwnam(strName(user));

  if ( pwd )
  { if ( what == NAME_name )
      answer(CtoName(pwd->pw_name));
    else if ( what == NAME_password )
      answer(CtoName(pwd->pw_passwd));
    else if ( what == NAME_userId )
      answer(toInt(pwd->pw_uid));
    else if ( what == NAME_groupId )
      answer(toInt(pwd->pw_gid));
    else if ( what == NAME_gecos )
      answer(CtoName(pwd->pw_gecos));
    else if ( what == NAME_home )
      answer(CtoName(pwd->pw_dir));
    else if ( what == NAME_shell )
      answer(CtoName(pwd->pw_shell));
  }
#endif /*HAVE_PWD_H*/

  fail;
}


Name
getHostnamePce(Pce pce)
{ char buf[LINESIZE];

  if ( gethostname(buf, LINESIZE) )
  { errorPce(pce, NAME_hostname, getOsErrorPce(pce));
    fail;
  }

  answer(CtoName(buf));
}


static Int
getPidPce(Pce pce)
{
#ifdef HAVE_GETPID
  answer(toInt(getpid()));
#else
  answer(toInt(ws_getpid()));
#endif
}

/* (JW)	When switched on pce will catch all normal error signals, print the
	C-stack and exit normally. Otherwise a core dump is produced.
 */

status
catchErrorSignalsPce(Pce pce, BoolObj val)
{ if ( pce->catch_error_signals != val )
  { assign(pce, catch_error_signals, val);
    catchErrorSignals(val);
  }

  succeed;
}



		/********************************
		*       DISPLAY MANAGEMENT	*
		********************************/

static status
informPce(Pce pce, CharArray fmt, int argc, Any *argv)
{ Any d = CurrentDisplay(NIL);

  if ( d != NULL && getOpenDisplay(d) == ON )
    return informDisplay(d, fmt, argc, argv);

  return formatPcev(pce, fmt, argc, argv);
}


static status
confirmPce(Pce pce, CharArray fmt, int argc, Any *argv)
{ Any d = CurrentDisplay(NIL);
  int try;

  if ( d != NULL && getOpenDisplay(d) == ON )
    return confirmDisplay(d, fmt, argc, argv);

  for(try = 0; try < 3; try++)
  { char line[256];

    formatPcev(pce, fmt, argc, argv);
    Cprintf(" (y/n) ? ");
    Cflush();

    if ( Cgetline(line, sizeof(line)) )
    { char *s = line;

      while( *s && isblank(*s) )
	s++;

      switch(*s)
      { case 'n':
	  fail;
	case 'y':
	  succeed;
	default:
	  writef("Please answer `y' or `n'\n");
      }

      continue;
    }

    break;
  }

  hostAction(HOST_HALT);
  exit(1);
  fail;					/* fool compiler */
}


		/********************************
		*         CASE HANDLING		*
		********************************/

static status
syntaxPce(Pce pce, Name casemap, Int ws)
{ Code msg;

					/* realise all classes */
  msg = answerObject(ClassMessage, Arg(2), NAME_realise, EAV);
  send(classTable, NAME_forAll, msg, EAV);
  doneObject(msg);

  if ( isDefault(ws) )
    ws = toInt('_');

  msg = answerObject(ClassMessage, Arg(1), NAME_syntax, casemap, ws, EAV);
  DEBUG(NAME_name, checkNames(1));
  TRY(forNamePce(pce, msg));
  DEBUG(NAME_name, checkNames(1));
  doneObject(msg);

  char_flags[(int)syntax.word_separator] = PU;
  char_flags[valInt(ws)] = WS;
  syntax.word_separator = valInt(ws);
  syntax.uppercase = (casemap == NAME_uppercase);

  succeed;
}


		/********************************
		*         MISCELLANEOUS		*
		********************************/

static status
failPce(Pce pce)
{ fail;
}


static status
succeedPce(Pce pce)
{ succeed;
}


#ifndef O_RUNTIME
static status
benchPce(Pce pce, Message msg, Int count, Name how)
{ int cnt = valInt(count);

  if ( how == NAME_forward )
  { while( cnt-- > 0 )
      forwardCodev((Code) msg, 0, NULL);
  } else if ( how == NAME_execute )
  { while( cnt-- > 0 )
      ExecuteMessage(msg);
  } else
  { Any receiver  = msg->receiver;
    Name selector = msg->selector;
    Any *argv;
    int argc;

    if ( msg->arg_count == ZERO )
    { argv = NULL;
      argc = 0;
    } else if ( msg->arg_count == ONE )
    { argv = (Any *)&msg->arguments;
      argc = 1;
    } else
    { argv = msg->arguments->elements;
      argc = valInt(msg->arguments->size);
    }

    if ( how == NAME_send )
    { while( cnt-- > 0 )
	sendv(receiver, selector, argc, argv);
    } else if ( how == NAME_qad )
    { while( cnt-- > 0 )
	qadSendv(receiver, selector, argc, argv);
    }
  }

  succeed;
}
#endif /*O_RUNTIME*/

status
resetPce(Pce pce)
{ Any dm;

  changedLevel = 0;
  resetDebugger();			/* these first, so the system */
  resetAnswerStack();			/* can push/pop goal again */
  resetMessageResolve();		/* and resolve methods */

  if ( notNil(pce) )
  {
#ifndef O_RUNTIME
    debuggingPce(pce, OFF);
#endif
    clearChain(pce->catched_errors);
  }

  resetTypes();
  resetVars();
  resetDraw();
  resetDispatch();

  resetApplications();
  if ( (dm = getObjectAssoc(NAME_displayManager)) )
    send(dm, NAME_reset, EAV);

  succeed;
}


static status
diePce(Pce pce, Int rval)
{ static int dying = FALSE;
  int rv = isDefault(rval) ? 0 : valInt(rval);

  if ( !dying++ )			/* avoid loops */
  { callExitMessagesPce(rv, pce);

    hostAction(HOST_HALT, rv);
					/* should not get here */
    killAllProcesses(rv);		/* should be done by above */
  }

  exit(rv);
  fail;					/* should not be reached */
}


static Any
getInstancePcev(Pce pce, Class class, int argc, Any *argv)
{ answer(answerObjectv(class, argc, argv));
}


static Any
getConvertPce(Pce pce, Any obj, Type type)
{ answer(checkType(obj, type, pce));
}


static status
makeClassUsingCode(Class class)
{ if ( notNil(class->make_class_message) )
    return forwardCode(class->make_class_message, class->name, EAV);

  fail;
}


static status
defineClassPce(Pce pce, Name name, Name super, StringObj summary, Code msg)
{ Class class;

  TRY(class = nameToTypeClass(name));

  if ( notDefault(class->realised) )	/* already existing */
  { Class superclass;

    TRY(superclass = nameToTypeClass(super));
    if ( notNil(class->super_class) && class->super_class->name != super )
      return errorPce(class, NAME_cannotChangeSuperClass);
  } else
  { class = defineClass(name, super, summary, makeClassUsingCode);
    assign(class, make_class_message, msg);
  }

  succeed;
}


		/********************************
		*           REFERENCES		*
		********************************/

Any
getObjectFromReferencePce(Pce pce, Any ref)
{ Any rval;

  if ( isInteger(ref) )
  { rval = IntToPointer(ref);

    if ( isProperObject(rval) && !isFreedObj(rval) )
      answer(rval);
  } else
  { assert(isName(ref));

    answer(findGlobal(ref));
  }

  fail;
}


static status
forNameReferencePce(Pce pce, Code msg)
{ return forSomeAssoc(msg);
}


static status
renameReferencePce(Pce pce, Name old, Name new)
{ return renameAssoc(old, new);
}


static Any
getVersionPce(Pce pce, Name how)
{ if ( isDefault(how) || how == NAME_string )
    answer(pce->version);
  if ( how == NAME_name )
  { char *s = strName(pce->version);
    char *q = s;
    char v[100];
    int n;

    for(n=0; n<3; n++)
    { while(*q && isdigit(*q))
	q++;
      if ( *q == '.' )
	q++;
    }
    if ( q > s && q[-1] == '.' )
      q--;
    assert(q+1-s < (long)sizeof(v));
    strncpy(v, s, q-s);
    v[q-s] = EOS;

    answer(CtoName(v));
  } else /* if ( how == NAME_number ) */
  { int major, minor, patchlevel;
    char *s = strName(pce->version);

    if ( sscanf(s, "%d.%d.%d", &major, &minor, &patchlevel) == 3 )
    { answer(toInt(major*10000+minor*100+patchlevel));
    }

    answer(toInt(-1));
  }
}

		 /*******************************
		 *	     THREADS		*
		 *******************************/


static BoolObj
getMultiThreadingPce(Pce pce)
{ answer(XPCE_mt == TRUE ? ON : OFF);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_instance[] =
        { "class=class", "argument=unchecked ..." };
#ifndef O_RUNTIME
static char *T_bench[] =
        { "message=message", "times=int",
	  "how={forward,execute,qad,send}" };
#endif /*O_RUNTIME*/
static char *T_userInfo[] =
        { "field={name,password,user_id,group_id,gecos,home,shell}",
	  "user=[name]" };
static char *T_formatAchar_array_argumentAany_XXX[] =
        { "format=char_array", "argument=any ..." };
static char *T_exception[] =
        { "identifier=name", "context=any ..." };
static char *T_defineClass[] =
        { "name=name", "super=name", "summary=[string]", "realise=code" };
static char *T_convert[] =
        { "object=unchecked", "type=type" };
static char *T_renameReference[] =
        { "old=name", "new=name" };
static char *T_syntax[] =
	{ "syntax={uppercase}", "word_separator=[char]" };

/* Instance Variables */

static vardecl var_pce[] =
{
#ifndef O_RUNTIME
  SV(NAME_debugging, "bool", IV_GET|IV_STORE, debuggingPce,
     NAME_debugging, "Add consistency checks"),
  SV(NAME_trapErrors, "bool", IV_GET|IV_STORE, trapErrorsPce,
     NAME_debugging, "Trap tracer on errors"),
#endif
  IV(NAME_lastError, "name*", IV_BOTH,
     NAME_exception, "Id of last occurred error"),
  IV(NAME_catchedErrors, "chain", IV_GET,
     NAME_exception, "Errors are expected by code"),
  SV(NAME_catchErrorSignals, "bool", IV_GET|IV_STORE, catchErrorSignalsPce,
     NAME_debugging, "Trap Unix signals to deal with errors"),
  IV(NAME_exitMessages, "chain", IV_GET,
     NAME_quit, "Executed when the process terminates"),
  IV(NAME_exceptionHandlers, "sheet", IV_GET,
     NAME_exception, "Exception-name -> handler mapping"),
  IV(NAME_home, "[name]", IV_SEND,
     NAME_environment, "PCE's home directory"),
  IV(NAME_defaults, "source_sink|char_array", IV_BOTH,
     NAME_environment, "File/rc from which to load defaults"),
  IV(NAME_applicationData, "directory", IV_BOTH,
     NAME_environment, "Directory for application data"),
  IV(NAME_version, "name", IV_NONE,
     NAME_version, "Version indication"),
  IV(NAME_machine, "name", IV_GET,
     NAME_version, "Name of this machine/architecture"),
  IV(NAME_operatingSystem, "name", IV_GET,
     NAME_version, "Name of operating system"),
  IV(NAME_windowSystem, "{X,windows}", IV_GET,
     NAME_version, "Basic window system used"),
  IV(NAME_windowSystemVersion, "int", IV_GET,
     NAME_version, "Version of Xt library used to compile xpce"),
  IV(NAME_windowSystemRevision, "int", IV_GET,
     NAME_version, "Revision of Xt library used to compile xpce"),
  IV(NAME_features, "chain", IV_GET,
     NAME_version, "List of installed features")
};

/* Send Methods */

static senddecl send_pce[] =
{ SM(NAME_initialise, 0, NULL, initialisePce,
     DEFAULT, "Create @pce (done only once)"),
  SM(NAME_syntax, 2, T_syntax, syntaxPce,
     NAME_host, "Specify language compatible syntax"),
  SM(NAME_defineClass, 4, T_defineClass, defineClassPce,
     NAME_class, "Declare a class without details"),
  SM(NAME_consoleLabel, 1, "char_array", consoleLabelPce,
     NAME_console, "Set the label for the console window"),
  SM(NAME_exposeConsole, 0, NULL, exposeConsolePce,
     NAME_console, "Expose the PCE/host console window"),
  SM(NAME_iconifyConsole, 0, NULL, iconifyConsolePce,
     NAME_console, "Make PCE/host console window an icon"),
  SM(NAME_showConsole, 1, "{open,full_screen,iconic,hidden}", showConsolePce,
     NAME_console, "Control visibility of the console window"),
  SM(NAME_fail, 0, NULL, failPce,
     NAME_control, "Simply fails"),
  SM(NAME_succeed, 0, NULL, succeedPce,
     NAME_control, "Simply succeeds"),
  SM(NAME_info, 0, NULL, infoPce,
     NAME_debugging, "Write statistics/info to terminal"),
  SM(NAME_maxGoalDepth, 1, "[int]*", maxGoalDepthPce,
     NAME_debugging, "Set maximum recursion level"),
#ifndef O_RUNTIME
  SM(NAME_listWastedCore, 1, "list_content=[bool]", listWastedCorePce,
     NAME_debugging, "List wasted core map"),
  SM(NAME_nodebugSubject, 1, "subject=name", nodebugSubjectPce,
     NAME_debugging, "Don't Report internal event on terminal"),
  SM(NAME_printStack, 1, "depth=[0..]", printStackPce,
     NAME_debugging, "Print PCE message stack to host-window"),
  SM(NAME_debugSubject, 1, "subject=name", debugSubjectPce,
     NAME_debugging, "Report internal event on terminal"),
  SM(NAME_bench, 3, T_bench, benchPce,
     NAME_statistics, "Benchmark for message passing"),
#endif
  SM(NAME_debuggingSubject, 1, "subject=name", debuggingSubjectPce,
     NAME_debugging, "Succeed if we are debugging this subject"),
  SM(NAME_crash, 0, NULL, crashPce,
     NAME_debugging, "Write in an illegal address to force a crash"),
  SM(NAME_catchError, 1, "identifier=[name|chain]", catchErrorPce,
     NAME_exception, "Indicate code is prepared to handle errors"),
  SM(NAME_catchPop, 0, NULL, catchPopPce,
     NAME_exception, "Pop pushed error handlers"),
  SM(NAME_catched, 1, "identifier=name", catchedErrorPce,
     NAME_exception, "Test if error_id is catched"),
  SM(NAME_exception, 2, T_exception, exceptionPcev,
     NAME_exception, "Raise an exception"),
  SM(NAME_banner, 0, NULL, bannerPce,
     NAME_initialise, "Write standard banner to terminal"),
  SM(NAME_forName, 1, "message=code", forNamePce,
     NAME_name, "Execute code on all defined names"),
  SM(NAME_die, 1, "status=[int]", diePce,
     NAME_quit, "Exit this (Unix) process with status"),
  SM(NAME_exitMessage, 1, "message=code", exitMessagePce,
     NAME_quit, "Execute code while dying"),
  SM(NAME_forNameReference, 1, "message=code", forNameReferencePce,
     NAME_reference, "Run code on all name references (global objects)"),
  SM(NAME_renameReference, 2, T_renameReference, renameReferencePce,
     NAME_reference, "Rename global reference"),
  SM(NAME_confirm, 2, T_formatAchar_array_argumentAany_XXX, confirmPce,
     NAME_report, "Test if the user confirms string"),
  SM(NAME_format, 2, T_formatAchar_array_argumentAany_XXX, formatPcev,
     NAME_report, "Formatted like C's printf"),
  SM(NAME_inform, 2, T_formatAchar_array_argumentAany_XXX, informPce,
     NAME_report, "Inform the user of something"),
  SM(NAME_write, 1, "argument=any ...", writePcev,
     NAME_report, "Write arguments, separated by spaces"),
  SM(NAME_writeLn, 1, "argument=any ...", writeLnPcev,
     NAME_report, "Write arguments, separated by spaces, add nl"),
  SM(NAME_feature, 1, "any", featurePce,
     NAME_version, "Define new feature"),
  SM(NAME_hasFeature, 1, "any", hasFeaturePce,
     NAME_version, "Test if feature is defined"),
  SM(NAME_loadDefaults, 1, "source_sink", loadDefaultsPce,
     NAME_default, "Load class variable defaults from file")
};

/* Get Methods */

static getdecl get_pce[] =
{ GM(NAME_home, 0, "name", NULL, getHomePce,
     DEFAULT, "Find XPCE's home directory"),
  GM(NAME_convert, 2, "converted=unchecked", T_convert, getConvertPce,
     NAME_conversion, "Convert anything to specified type"),
#ifndef O_RUNTIME
  GM(NAME_unresolvedTypes, 0, "chain", NULL, getUnresolvedTypesPce,
     NAME_debugging, "New chain with unresolved types"),
#endif
  GM(NAME_maxGoalDepth, 0, "int*", NULL, getMaxGoalDepthPce,
     NAME_debugging, "Maximum recursion level"),
  GM(NAME_environmentVariable, 1, "value=name", "name=name",
     getEnvironmentVariablePce,
     NAME_environment, "Unix environment variable (getenv)"),
  GM(NAME_hostname, 0, "host=name", NULL, getHostnamePce,
     NAME_environment, "Name of host on which PCE runs"),
  GM(NAME_user, 0, "user=name", NULL, getUserPce,
     NAME_environment, "Login name of user"),
  GM(NAME_userInfo, 2, "value=name|int", T_userInfo, getUserInfoPce,
     NAME_environment, "Get information on user (from the passwd file)"),
  GM(NAME_fd, 0, "number=int", NULL, getFdPce,
     NAME_file, "Number of free file descriptors"),
  GM(NAME_maxInteger, 0, "value=int", NULL, getMaxIntegerPce,
     NAME_limit, "Highest representable integer"),
  GM(NAME_minInteger, 0, "value=int", NULL, getMinIntegerPce,
     NAME_limit, "Lowest representable integer"),
  GM(NAME_instance, 2, "created=object|function", T_instance, getInstancePcev,
     NAME_oms, "Create instance of any class"),
  GM(NAME_objectFromReference, 1, "object=unchecked", "reference=int|name",
     getObjectFromReferencePce,
     NAME_oms, "Convert object-name or integer reference into object"),
  GM(NAME_pid, 0, "identifier=int", NULL, getPidPce,
     NAME_process, "Process id of this process"),
  GM(NAME_osError, 0, "identifier=name", NULL, getOsErrorPce,
     NAME_report, "Name of last operating system error"),
  GM(NAME_isRuntimeSystem, 0, "bool", NULL, getIsRuntimeSystemPce,
     NAME_runtime, "@on if this is the runtime library"),
  GM(NAME_answerStackSize, 0, "cells=int", NULL, getAnswerStackSizePce,
     NAME_statistics, "Number of cells (objects) in `answer' state"),
  GM(NAME_coreUsage, 0, "bytes=int", NULL, getCoreUsagePce,
     NAME_statistics, "Total core in active use"),
  GM(NAME_coreWasted, 0, "bytes=int", NULL, getWastedCorePce,
     NAME_statistics, "Core requested from system, but not in use"),
  GM(NAME_cpuTime, 1, "seconds=real", "kind=[{user,system}]", getCpuTimePce,
     NAME_statistics, "Total CPU time for this process"),
  GM(NAME_deferredUnalloced, 0, "number=int", NULL, getDeferredUnallocedPce,
     NAME_statistics, "# freed referenced objects"),
  GM(NAME_objectsAllocated, 0, "number=int", NULL, getNoCreatedPce,
     NAME_statistics, "Total number of objects created"),
  GM(NAME_objectsFreed, 0, "number=int", NULL, getNoFreedPce,
     NAME_statistics, "Total number of objects freed"),
  GM(NAME_date, 0, "string", NULL, getDatePce,
     NAME_time, "Unix's standard time string for now"),
  GM(NAME_mclock, 0, "int", NULL, getMclockPce,
     NAME_time, "#Elapsed milliseconds since XPCE was started"),
  GM(NAME_version, 1, "name|int", "how=[{string,name,number}]", getVersionPce,
     NAME_version, "Representation of the version number"),
  GM(NAME_multiThreading, 0, "bool", NULL, getMultiThreadingPce,
     NAME_thread, NULL)
};

/* Resources */

static classvardecl rc_pce[] =
{ RC(NAME_initialise, "code*",
     UXWIN(/*UNIX*/
	   "and(_dialog_bg        @= colour(grey80),\n"
	   "    _button_elevation @= elevation(button, 1, grey80,\n"
	   "				       grey95, grey50,\n"
	   "				      '3d', grey70),\n"
	   "    _mark_elevation   @= elevation(mark, 0),\n"
	   "    _win_pen	  @= number(0))",
	   /*__WINDOWS__*/
           "and(_dialog_bg     @= colour(win_btnface),\n"
	   "    _graph_bg      @= colour(win_window),\n"
	   "    _win_pen       @= number(1),\n"
	   "    _isearch_style @= style(background := green),\n"
	   "    _select_style  @= style(background := win_highlight,\n"
	   "                            colour     := win_highlighttext),\n"
	   "    _txt_height    @= elevation(@nil, 2, win_window))"),
     "Code object to run when initialising defaults")
};

/* Class Declaration */

static Name pce_termnames[] = { NAME_version };

ClassDecl(pce_decls,
          var_pce, send_pce, get_pce, rc_pce,
          1, pce_termnames,
          "$Rev$");

status
makeClassPce(Class class)
{ declareClass(class, &pce_decls);

  saveStyleClass(class, NAME_external);
  cloneStyleClass(class, NAME_none);

  PCE = globalObject(NAME_pce, ClassPce, EAV);
  protectObject(PCE);

  succeed;
}


		 /*******************************
		 *	  INITIALISATION	*
		 *******************************/

static HashTable
objectAttributeTable(Name name)
{ HashTable ht = globalObject(name, ClassHashTable, EAV);
  assign(ht, refer, NAME_value);

  return ht;
}


export status
pceInitialise(int handles, const char *home, int argc, char **argv)
{ AnswerMark mark;

  if ( XPCE_initialised )
    succeed;

  XPCE_initialised = TRUE;
  inBoot = TRUE;

  PCEargc = argc;
  PCEargv = argv;

  MaxGoalDepth = NO_MAX_GOAL_DEPTH;
  initAnswerStack();
  initMClock();

#ifndef O_RUNTIME
  PCEdebugging = FALSE;
  if ( getenv("PCEDEBUGBOOT") != NULL )
  { PCEdebugBoot = TRUE;
    DEBUG_BOOT(Cprintf("Debugging boot cycle\n"));
  } else
    PCEdebugBoot = FALSE;
#endif

  PCE = NIL;
  pceReset();				/* reset important globals */
  markAnswerStack(mark);
  syntax.word_separator = '_';

  ((Instance)NIL)->flags     = F_PROTECTED|OBJ_MAGIC;
  ((Instance)DEFAULT)->flags = F_PROTECTED|OBJ_MAGIC;
  ((Instance)ON)->flags      = F_PROTECTED|OBJ_MAGIC;
  ((Instance)OFF)->flags     = F_PROTECTED|OBJ_MAGIC;

  DEBUG_BOOT(Cprintf("Alloc ...\n"));
  pceInitAlloc();
  allocRange(&ConstantNil,          sizeof(struct constant));
  allocRange(&ConstantDefault,      sizeof(struct constant));
  allocRange(&ConstantClassDefault, sizeof(struct constant));
  allocRange(&BoolOff,              sizeof(struct bool));
  allocRange(&BoolOn,               sizeof(struct bool));
  initNamesPass1();
  DEBUG_BOOT(Cprintf("Types ...\n"));
  initTypes();
  DEBUG_BOOT(Cprintf("Names ...\n"));
  initCharArrays();
  initNamesPass2();
  DEBUG_BOOT(Cprintf("Name Assocs ...\n"));
  initAssoc(handles);

{ Type t;
  t = createType(CtoName("any ..."), NAME_any, NIL);
  vectorType(t, ON);
}

  /* Make instanceOfObject(impl, ClassMethod) work ... */
  ClassMethod->tree_index      = 1;
  ClassMethod->neighbour_index = 4;
  ClassSendMethod->tree_index  = 2;
  ClassGetMethod->tree_index   = 3;

  DEBUG_BOOT(Cprintf("Boot classes ...\n"));

  ClassObject =
    bootClass(NAME_object,		/* Name */
	      (Name) NIL,		/* SuperClass */
	      sizeof(struct object),	/* Instance size */
	      1,			/* # PCE typed slots */
	      initialiseObject,		/* Initialisation function */
	      0);

  ClassChain =
    bootClass(NAME_chain,
	      NAME_object,
	      sizeof(struct chain),
	      0,
	      initialiseChainv,
	      1, "any ...");

  ClassProgramObject =
    bootClass(NAME_programObject,
	      NAME_object,
	      sizeof(struct program_object),
	      1,
	      initialiseProgramObject,
	      0);

  ClassType =
    bootClass(NAME_type,
	      NAME_programObject,
	      sizeof(struct type),
	      6,
	      initialiseType,
	      4, "name", "name", "any", "any");	/* changed later!! */
  lookupBootClass(ClassType, (Func) getLookupType, 1, "name");

  ClassSourceLocation =
    bootClass(NAME_sourceLocation,
	      NAME_object,
	      sizeof(struct source_location),
	      2,
	      initialiseSourceLocation,
	      2, "name", "[int]*");

  ClassVector =
    bootClass(NAME_vector,
	      NAME_object,
	      sizeof(struct vector),
	      2,
	      initialiseVectorv,
	      1, "any ...");

  ClassHashTable =
    bootClass(NAME_hashTable,
	      NAME_object,
	      sizeof(struct hash_table),
	      1,
	      initialiseHashTable,
	      1, "[int]");

  ClassBehaviour =
    bootClass(NAME_behaviour,
	      NAME_programObject,
	      sizeof(struct behaviour),
	      2,
	      initialiseBehaviour,
	      0);

  ClassMethod =
    bootClass(NAME_method,
	      NAME_behaviour,
	      sizeof(struct method),
	      5,
	      initialiseMethod,
	      6, "name", "[vector]", "code|any", "[string]*",
	         "[source_location]*", "[name]*");

  ClassSendMethod =
    bootClass(NAME_sendMethod,
	      NAME_method,
	      sizeof(struct send_method),
	      0,
	      initialiseMethod,
	      6, "name", "[vector]", "code|any",
	         "[string]*", "[source_location]*", "[name]*");

  ClassGetMethod =
    bootClass(NAME_getMethod,
	      NAME_method,
	      sizeof(struct get_method),
	      0,
	      initialiseGetMethod,
	      7, "name", "[type]", "[vector]", "code|any",
	         "[string]*", "[source_location]*", "[name]*");

  ClassCharArray =
    bootClass(NAME_charArray,
	      NAME_object,
	      sizeof(struct char_array),
	      0,
	      initialiseCharArray,
	      1, "char_array");

  ClassName =
    bootClass(NAME_name,
	      NAME_charArray,
	      sizeof(struct name),
	      1,
	      initialiseName,
	      1, "char_array");

  ClassString =
    bootClass(NAME_string,
	      NAME_charArray,
	      sizeof(struct string),
	      0,
	      initialiseStringv,
	      2, "[name]", "any ...");

  ClassTuple =
    bootClass(NAME_tuple,
	      NAME_object,
	      sizeof(struct tuple),
	      2,
	      initialiseTuple,
	      2, "any", "any");

  DEBUG_BOOT(Cprintf("Initialised boot classes\n"));

  classTable		= globalObject(NAME_classes,       ClassHashTable, EAV);
#ifndef O_RUNTIME
  PCEdebugSubjects	= globalObject(NAME_DebugSubjects, ClassChain, EAV);
#endif
  initDebugger();

  TypeTable->class = ClassHashTable;
  newAssoc(NAME_types, TypeTable);
  createdClass(ClassHashTable, TypeTable, NAME_new);

  TypeExpression = newObject(ClassType, NAME_expression, NAME_compound, EAV);
  superType(TypeExpression, TypeInt);
  superType(TypeExpression, nameToType(NAME_function));
  superType(TypeExpression, nameToType(NAME_number));
  superType(TypeExpression, nameToType(NAME_real));
  superType(TypeExpression, nameToType(NAME_var));

  TypeCode     = nameToType(NAME_code);
  TypeImage    = nameToType(NAME_image);
  TypeColour   = nameToType(NAME_colour);
  TypeEquation = CtoType("=");

  ObjectConstraintTable = objectAttributeTable(NAME_objectConstraintTable);
  ObjectAttributeTable  = objectAttributeTable(NAME_objectAttributeTable);
  ObjectSendMethodTable = objectAttributeTable(NAME_objectSendMethodTable);
  ObjectGetMethodTable  = objectAttributeTable(NAME_objectGetMethodTable);
  ObjectRecogniserTable = objectAttributeTable(NAME_objectRecogniserTable);
  ObjectHyperTable      = objectAttributeTable(NAME_objectHyperTable);

  name_procent_s	= CtoName("%s");
  name_cxx		= CtoName("C++");
  name_nil		= CtoName("[]");
  name_space		= CtoName(" ");

  DEBUG_BOOT(Cprintf("Building class definitions\n"));
  initClassDefs();
  DEBUG_BOOT(Cprintf("Realising Boot classes ...\n"));
  realiseBootClass(ClassObject);
  realiseBootClass(ClassChain);
  realiseBootClass(ClassProgramObject);
  realiseBootClass(ClassType);
  realiseBootClass(ClassSourceLocation);
  realiseBootClass(ClassVector);
  realiseBootClass(ClassHashTable);
  realiseBootClass(ClassBehaviour);
  realiseBootClass(ClassMethod);
  realiseBootClass(ClassSendMethod);
  realiseBootClass(ClassGetMethod);
  realiseBootClass(ClassCharArray);
  realiseBootClass(ClassName);
  realiseBootClass(ClassString);
  realiseBootClass(ClassTuple);
  DEBUG_BOOT(Cprintf("Boot classes realised.\n"));
  initTypeAliases();

  { for_hash_table(classTable, s,
		   { Class class = s->value;
		     if ( class->no_created != class->no_freed &&
			  class->realised == OFF )
		       realiseClass(class);
		   });
  }

  realiseClass(ClassPce);		/* make @pce */
  realiseClass(ClassVar);		/* @arg1, ... */
  realiseClass(ClassConstant);		/* @default, @nil */
  realiseClass(ClassBool);		/* @on, @off */

  DEBUG_BOOT(Cprintf("Defining features\n"));

  featurePce(PCE, NAME_process);
#if defined(HAVE_SOCKET) || defined(HAVE_WINSOCK)
  featurePce(PCE, NAME_socket);
#endif

  DEBUG_BOOT(Cprintf("C/C++ global objects\n"));
  initCGlobals();
  if ( home )
    send(PCE, NAME_home, CtoName(home), EAV);

  rewindAnswerStack(mark, NIL);
  inBoot = FALSE;

  ws_initialise(argc, argv);
  if ( !hostAction(HOST_ATEXIT, run_pce_exit_hooks) )
  {
#ifdef HAVE_ON_EXIT
     on_exit(run_pce_onexit_hooks, NULL);
#else
#ifdef HAVE_ATEXIT
     atexit(run_pce_atexit_hooks);
#endif
#endif
  }

  DEBUG_BOOT(Cprintf("Initialisation complete.\n"));
  succeed;
}
