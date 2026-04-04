/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2025, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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

#define UNICODE 1
#define _UNICODE 1
#define PceHInstance ThePceHInstance
#include <h/kernel.h>
#include "mswin.h"
#include <h/interface.h>
#include <tchar.h>
#undef End
#include <shlobj.h>
#define End }

#ifdef UNICODE
#define nameToTCHAR(nm) nameToWC((Name)(nm), NULL)
#define TCHARToName(s)  WCToName(s, _tcslen(s))
#else
#define nameToTCHAR(nm) nameToMB((Name)(nm))
#define TCHARToName(s)  MBToName(s)
#endif


		 /*******************************
		 *	    DLL STUFF		*
		 *******************************/

HINSTANCE ThePceHInstance;		/* Global handle */
DWORD	  ThePceThread;			/* Dispatching thread */

BOOL WINAPI
DllMain(HINSTANCE instance, DWORD reason, LPVOID reserved)
{ switch(reason)
  { case DLL_PROCESS_ATTACH:
      ThePceHInstance = instance;
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
  }

  return TRUE;
}

		 /*******************************
		 *	      VERSIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get Windows Version/Revision info
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

os_platform
ws_platform(void)
{ static int done = FALSE;
  os_platform platform = WINUNKNOWN;

  if ( !done )
  { OSVERSIONINFO info;

    info.dwOSVersionInfoSize = sizeof(info);
    if ( GetVersionEx(&info) )
    { switch( info.dwPlatformId )
      { case VER_PLATFORM_WIN32s:
	  platform = WIN32S;
	  break;
	case VER_PLATFORM_WIN32_WINDOWS:
	  switch( info.dwMinorVersion )
	  { case 0:
	      platform = WIN95;
	      break;
	    case 10:
	      platform = WIN98;
	      break;
	    case 90:
	      platform = WINME;
	      break;
	    default:
	      platform = WINUNKNOWN;
	  }
	  break;
	case VER_PLATFORM_WIN32_NT:
	  platform = NT;
	  break;
      }
    } else
      platform = WINUNKNOWN;
  }

  return platform;
}

char *
ws_os(void)
{ switch(ws_platform())
  { case WINUNKNOWN:
      return "win32";
    case WIN32S:
      return "win32s";
    case WIN95:
    case WIN98:
    case WINME:
      return "win95";			/* doesn't really make a difference */
    case NT:
      return "winnt";
    default:
      return "winunknown";
  }
}


int
ws_getpid(void)
{ DEBUG(NAME_instance, Cprintf("HINSTANCE is %d\n", PceHInstance));

  return (int) GetCurrentProcessId();
}


Name
ws_appdata(const char *sub)
{ TCHAR buf[PATH_MAX];

  if ( SHGetSpecialFolderPath(0, buf, CSIDL_APPDATA, TRUE) )
  { wchar_t *p;

    for(p=buf; *p; p++)
    { if ( *p == '\\' )
	*p = '/';
    }
    if ( sub )
    { const char *s;

      *p++ = '/';
      for(s=sub; *s; )
	*p++ = *s++;
      *p = EOS;
    }

    return TCHARToName(buf);
  }

  fail;
}


int
ws_mousebuttons(void)
{ return GetSystemMetrics(SM_CMOUSEBUTTONS);
}


Int
ws_default_scrollbar_width(void)
{ int w = GetSystemMetrics(SM_CXHSCROLL);	/* Is this the right one? */

  return toInt(w);
}


#define MAXMESSAGE 1024

Name
WinStrError(int error, ...)
{ va_list args;
  TCHAR msg[MAXMESSAGE];

  va_start(args, error);
  if ( !FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		      NULL,
		      error,
		      GetUserDefaultLangID(),
		      msg,
		      sizeof(msg),
		      (char **)args) )
  { wsprintf(msg, _T("Unknown WINAPI error %d"), error);
  }
  va_end(args);

  return TCHARToName(msg);
}


int
get_logical_drive_strings(int bufsize, char *buf)
{ return GetLogicalDriveStringsA(bufsize, buf);
}

		 /*******************************
		 *            PIPES             *
		 *******************************/

/*
 * CreatePipeEx - create an anonymous-like pipe with optional overlapped support.
 * Works by creating a local named pipe with a unique name and connecting both ends.
 *
 * Parameters:
 *   hReadPipe    - Receives the read handle
 *   hWritePipe   - Receives the write handle
 *   lpPipeAttributes - Security attributes
 *   nSize        - Buffer size
 *   dwReadMode   - Flags for read handle (e.g., FILE_FLAG_OVERLAPPED)
 *   dwWriteMode  - Flags for write handle (e.g., FILE_FLAG_OVERLAPPED)
 *
 * Returns:
 *   TRUE on success, FALSE on failure (call GetLastError()).
 */

static volatile long pipe_gensym = 0;

BOOL
CreatePipeEx(PHANDLE hReadPipe,
	     PHANDLE hWritePipe,
	     LPSECURITY_ATTRIBUTES lpPipeAttributes,
	     DWORD nSize,
	     DWORD dwReadMode,
	     DWORD dwWriteMode)
{ BOOL result = FALSE;
  HANDLE hReadTmp = INVALID_HANDLE_VALUE, hWriteTmp = INVALID_HANDLE_VALUE;
  CHAR pipeName[MAX_PATH];

  // Generate a unique name using process ID + tick count
  sprintf(pipeName, "\\\\.\\Pipe\\anon.%lu.%lu",
	  GetCurrentProcessId(), InterlockedIncrement(&pipe_gensym));

  hReadTmp = CreateNamedPipeA(pipeName,
			      PIPE_ACCESS_INBOUND | dwReadMode,
			      PIPE_TYPE_BYTE | PIPE_WAIT,
			      1,           // max instances
			      nSize, nSize,
			      0,           // default timeout
			      lpPipeAttributes);
  if (hReadTmp == INVALID_HANDLE_VALUE)
    goto cleanup;

  hWriteTmp = CreateFileA(pipeName,
			  GENERIC_WRITE,
			  0,                      // no sharing
			  lpPipeAttributes,
			  OPEN_EXISTING,
			  FILE_ATTRIBUTE_NORMAL | dwWriteMode,
			  NULL);
  if (hWriteTmp == INVALID_HANDLE_VALUE)
    goto cleanup;

  *hReadPipe = hReadTmp;
  *hWritePipe = hWriteTmp;
  result = TRUE;
  hReadTmp = hWriteTmp = INVALID_HANDLE_VALUE; // ownership transferred

cleanup:
  if (hReadTmp != INVALID_HANDLE_VALUE)
    CloseHandle(hReadTmp);
  if (hWriteTmp != INVALID_HANDLE_VALUE)
    CloseHandle(hWriteTmp);

  return result;
}
