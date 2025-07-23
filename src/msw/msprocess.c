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

#include "h/kernel.h"
#include "mswin.h"
#include <h/unix.h>
#include <h/interface.h>

#ifndef MAXCMDLINE
#define MAXCMDLINE 10240
#endif

#define APIError() WinStrError(GetLastError())

static int
getEnvironmentSizeProcess(Process p)
{ int size = 0;
  Cell cell;

  if ( isNil(p->environment) )
    return 0;

  for_cell(cell, p->environment->attributes)
  { Attribute a = cell->value;

    size += valInt(getSizeCharArray(a->name)) +
	    valInt(getSizeCharArray(a->value)) + 2;
  }

  return size+1;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create a Win32 environment block as requested  by the Win32 API. This is
rather different from the Unix version,   which  expects a char** ending
with a NULL pointer. Win32  is  a   char  *  holding  name=value fields,
separated by '\0'. The last is terminated by a secondary '\0'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
createWin32EnvriomentBlock(Process p)
{ if ( notNil(p->environment) )
  { char *buf = pceMalloc(getEnvironmentSizeProcess(p));
    char *s = buf;
    Cell cell;

    if ( !buf )
    { errorPce(p, NAME_notEnoughMemory);
      return NULL;
    }

    for_cell(cell, p->environment->attributes)
    { Attribute a = cell->value;

      s = strcpyskip(s, toCharp(a->name));
      *s++ = '=';
      s = strcpyskip(s, toCharp(a->value));
      *s++ = '\0';
    }

    *s++ = '\0';
    return buf;
  }

  return NULL;
}


status
openProcess(Process p, CharArray cmd, int argc, CharArray *argv)
{ status rval = SUCCEED;

  if ( notDefault(cmd) )
  { if ( notNil(p->pid) )
      return errorPce(p, NAME_noChangeAfterOpen);

    assign(p, name, cmd);
    assign(p, arguments, newObjectv(ClassVector, argc, (Any *)argv));
  }

  if ( isNil(p->pid) )
  { HANDLE wrfd[2];			/* output to process */
    HANDLE rdfd[2];			/* input from process */
    STARTUPINFO startinfo;
    PROCESS_INFORMATION *processinfo;
    SECURITY_ATTRIBUTES sa;
    char cmdline[MAXCMDLINE];
    char dirbuf[PATH_MAX];
    char *cwd = NULL;
    Any a;
    char *env = createWin32EnvriomentBlock(p);

    if ( p->use_tty == ON )
    { Cprintf("%s: Warning: no TTY control in Win32\n", pp(p));
    }

    sa.nLength              = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle       = TRUE;

    if ( !CreatePipe(&wrfd[0], &wrfd[1], &sa, 0) ) /* read, write */
      return errorPce(p, NAME_noPipe, APIError());
    if ( !CreatePipe(&rdfd[0], &rdfd[1], &sa, 0) )
    { CloseHandle(wrfd[0]);
      CloseHandle(wrfd[1]);
      return errorPce(p, NAME_noPipe, APIError());
    }

    if ( notDefault(p->directory) )
    { _xos_limited_os_filename(strName(p->directory->path), dirbuf, PATH_MAX);
      cwd = dirbuf;
    }

    _xos_limited_os_filename(strName(p->name), cmdline, MAXCMDLINE);
    for_vector(p->arguments, a,
	       { strcat(cmdline, " ");
		 strcat(cmdline, toCharp(a));
	       });
    processinfo = alloc(sizeof(*processinfo));
    memset(&startinfo, 0, sizeof(startinfo));
    memset(processinfo, 0, sizeof(*processinfo));

    startinfo.cb	  = sizeof(startinfo);
    startinfo.dwFlags     = STARTF_USESTDHANDLES;
    startinfo.hStdInput   = wrfd[0];
    startinfo.hStdOutput  = rdfd[1];
    startinfo.hStdError   = rdfd[1];

    if ( CreateProcess(NULL,			/* executable */
		       cmdline,			/* cmdline */
		       NULL,			/* Process security */
		       NULL,			/* Thread security */
		       TRUE,			/* Inherited handles */
		       DETACHED_PROCESS|CREATE_NEW_PROCESS_GROUP,
		       env,			/* environment char ** */
		       cwd,			/* directory */
		       &startinfo,
		       processinfo) )
    { CloseHandle(processinfo->hThread);	/* don't need this */
      pidProcess(p, toInt(processinfo->dwProcessId));
      p->rdfd   = (intptr_t) rdfd[0];
      p->wrfd   = (intptr_t) wrfd[1];
      p->ws_ref = processinfo;
      assign(p, status, NAME_running);

      DEBUG(NAME_process,
	    Cprintf("%s: Created Process %d --> %s --> %d, id = %d\n",
		    pp(p), p->wrfd, cmdline, p->rdfd,
		    processinfo->dwProcessId));

      inputStream((Stream)p, DEFAULT);
    } else
    { errorPce(p, NAME_execError, APIError());
      unalloc(sizeof(*processinfo), processinfo);
      rval = FAIL;
    }

    if ( env )
      pceFree(env);

    CloseHandle(wrfd[0]);
    CloseHandle(rdfd[1]);
  }

  return rval;
}


void
ws_kill_process(Process p, int sig)
{ PROCESS_INFORMATION *pi = p->ws_ref;

  if ( sig == 1 ||
       sig == 9 ||
       sig == 15 )
  { TerminateProcess(pi->hProcess, sig);
  } else if ( sig == 3 )
  { GenerateConsoleCtrlEvent(CTRL_C_EVENT, pi->dwProcessId);
  } else
    Cprintf("%s: process->kill only supports INT, KILL\n", pp(p));
}


#if NOSTUB
void
ws_done_process(Process p)
{ PROCESS_INFORMATION *pi;

  if ( (pi=p->ws_ref) )
  { CloseHandle(pi->hProcess);
    unalloc(sizeof(*pi), pi);
    p->ws_ref = NULL;
  }
}
#endif
