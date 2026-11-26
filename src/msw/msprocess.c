/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
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
#include <txt/chararray.h>
#include <txt/i18n.h>
#include <unx/process.h>
#include <unx/stream.h>

#define PIPE_SIZE 8192

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
createWin32EnvironmentBlock(Process p)
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


/* Quoting arguments for Windows is shared with library(process) */

#include "win_cmdline.c"

/* Copy the text of ca into buf as an OS file name: wide and with
 * backslashes.  We copy because charArrayToWC() may return the text of
 * ca itself.
 */

static bool
os_file_nameW(CharArray ca, wchar_t *buf, size_t size)
{ size_t len;
  const wchar_t *s = charArrayToWC(ca, &len);

  if ( len+1 > size )
    return false;
  for(size_t i=0; i<len; i++)
    buf[i] = (s[i] == '/' ? '\\' : s[i]);
  buf[len] = 0;

  return true;
}

/* Build the command line of p in b.  The program is the first word, as
 * we let CreateProcessW() find it.  A batch file (.bat, .cmd) is run by
 * cmd.exe, which needs other quoting and cannot be passed all text.
 */

static status
command_line(Process p, wbuf *b)
{ wchar_t prog[PATH_MAX];
  bool batch;
  Vector args = p->arguments;

  if ( !os_file_nameW(p->name, prog, PATH_MAX) )
    return errorPce(p, NAME_execError, CtoName("program name too long"));
  batch = is_batch_file(prog);
  if ( !add_quoted_arg(b, prog, wcslen(prog), false) )
    return errorPce(p, NAME_notEnoughMemory);

  for(int i=0; i<valInt(args->size); i++)
  { CharArray a = args->elements[i];
    size_t len;
    const wchar_t *s = charArrayToWC(a, &len);

    if ( batch && !batch_safe_arg(s, len) )
      return errorPce(p, NAME_execError,
		      CtoName("argument cannot be passed to a batch file"));
    if ( !wbuf_add(b, ' ') ||
	 !add_quoted_arg(b, s, len, batch) )
      return errorPce(p, NAME_notEnoughMemory);
  }

  succeed;
}


/* Start the process.  Its output is read through the overlapped read
 * end of a pipe by the watcher thread (see ws_input_stream()) and its
 * termination is seen through its handle (see ws_watch_process()).
 * Only the ends of the pipes the child uses are inherited: if the child
 * inherits our ends, it holds its own input open and the pipes of other
 * processes we start.
 */

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
    STARTUPINFOW startinfo;
    PROCESS_INFORMATION processinfo;
    SECURITY_ATTRIBUTES sa;
    wbuf cmdline = {0};
    wchar_t dirbuf[PATH_MAX];
    wchar_t *cwd = NULL;
    char *env;

    if ( p->use_tty == ON )
    { Cprintf("%s: Warning: no TTY control in Win32\n", pp(p));
    }

    if ( !command_line(p, &cmdline) )
    { free(cmdline.base);
      fail;
    }

    sa.nLength              = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle       = true;

    if ( !CreatePipe(&wrfd[0], &wrfd[1], &sa, 0) ) /* read, write */
    { free(cmdline.base);
      return errorPce(p, NAME_noPipe, APIError());
    }
    if ( !CreatePipeEx(&rdfd[0], &rdfd[1], &sa, PIPE_SIZE,
		       FILE_FLAG_OVERLAPPED, 0) )
    { Name msg = APIError();
      CloseHandle(wrfd[0]);
      CloseHandle(wrfd[1]);
      free(cmdline.base);
      return errorPce(p, NAME_noPipe, msg);
    }
    SetHandleInformation(wrfd[1], HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(rdfd[0], HANDLE_FLAG_INHERIT, 0);

    if ( notDefault(p->directory) &&
	 os_file_nameW((CharArray)p->directory->path, dirbuf, PATH_MAX) )
      cwd = dirbuf;

    memset(&startinfo, 0, sizeof(startinfo));
    memset(&processinfo, 0, sizeof(processinfo));

    startinfo.cb	  = sizeof(startinfo);
    startinfo.dwFlags     = STARTF_USESTDHANDLES;
    startinfo.hStdInput   = wrfd[0];
    startinfo.hStdOutput  = rdfd[1];
    startinfo.hStdError   = rdfd[1];

    env = createWin32EnvironmentBlock(p);
    if ( CreateProcessW(NULL,			/* executable */
		       cmdline.base,		/* cmdline */
		       NULL,			/* Process security */
		       NULL,			/* Thread security */
		       true,			/* Inherited handles */
		       DETACHED_PROCESS|CREATE_NEW_PROCESS_GROUP,
		       env,			/* environment char ** */
		       cwd,			/* directory */
		       &startinfo,
		       &processinfo) )
    { CloseHandle(processinfo.hThread);	/* don't need this */
      pidProcess(p, toInt(processinfo.dwProcessId));
      p->rdfd   = (intptr_t) rdfd[0];
      p->wrfd   = (intptr_t) wrfd[1];
      assign(p, status, NAME_running);
      assign(p, code, NIL);		/* see ws_end_of_output_process() */
      ws_watch_process(p, processinfo.hProcess);

      DEBUG(NAME_process,
	    Cprintf("%s: Created Process %d --> %ls --> %d, id = %d\n",
		    pp(p), p->wrfd, cmdline.base, p->rdfd,
		    processinfo.dwProcessId));

      inputStream((Stream)p, DEFAULT);
    } else
    { DWORD error = GetLastError();

      if ( error == ERROR_FILE_NOT_FOUND || error == ERROR_PATH_NOT_FOUND )
	errorPce(p, NAME_execError, CtoName("command not found"));
      else
	errorPce(p, NAME_execError, WinStrError(error));
      CloseHandle(wrfd[1]);
      CloseHandle(rdfd[0]);
      rval = FAIL;
    }

    if ( env )
      pceFree(env);
    free(cmdline.base);

    CloseHandle(wrfd[0]);
    CloseHandle(rdfd[1]);
  }

  return rval;
}
