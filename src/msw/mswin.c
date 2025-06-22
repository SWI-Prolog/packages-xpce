/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2022, University of Amsterdam
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
#include "include.h"
#include "mswin.h"
#include <h/interface.h>
#include <tchar.h>

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


#if NOSTUB
int
pceMTdetach(void)
{
#if O_DEBUG_EXIT
  ServiceMode = PCE_EXEC_USER;
  PCEdebugging = TRUE;

  Cprintf("pceMTdetach() in user mode\n");
#endif

  DEBUG(NAME_thread,
	Cprintf("Detached thread 0x%x\n", GetCurrentThreadId()));
  destroyThreadWindows(ClassFrame);
  destroyThreadWindows(ClassWindow);

  return TRUE;
}
#endif


		 /*******************************
		 *	      VERSIONS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get Windows Version/Revision info
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if NOSTUB
int
ws_version(void)
{ DWORD dwv = GetVersion();

  return LOBYTE(LOWORD(dwv));
}


int
ws_revision(void)
{ DWORD dwv = GetVersion();

  return HIBYTE(LOWORD(dwv));
}
#endif

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


HWND
HostConsoleHWND()
{ PceCValue val;

  if ( hostQuery(HOST_CONSOLE, &val) )
    return (HWND) val.pointer;

  return NULL;
}


#if NOSTUB
status
ws_show_console(Name how)
{ HWND hwnd = HostConsoleHWND();

  if ( hwnd )
  { if ( how == NAME_open )
    { if ( IsIconic(hwnd) )
	ShowWindow(hwnd, SW_RESTORE);
      else
	ShowWindow(hwnd, SW_SHOW);
    } else if ( how == NAME_iconic )
      ShowWindow(hwnd, SW_SHOWMINIMIZED);
    else if ( how == NAME_hidden )
      ShowWindow(hwnd, SW_HIDE);
    else if ( how == NAME_fullScreen )
      ShowWindow(hwnd, SW_MAXIMIZE);

    succeed;
  }

  fail;
}


status
ws_console_label(CharArray label)
{ HWND hwnd = HostConsoleHWND();

  if ( hwnd )
    SetWindowText(hwnd, nameToTCHAR((Name)label));

  succeed;
}
#endif

void
ws_check_intr()
{ hostAction(HOST_CHECK_INTERRUPT);
}


void
ws_msleep(int time)
{ Sleep((DWORD) time);
}


int
ws_getpid()
{ DEBUG(NAME_instance, Cprintf("HINSTANCE is %d\n", PceHInstance));

  return (int) GetCurrentProcessId();
}

#if NOSTUB
char *
ws_user()
{ TCHAR buf[256];
  Name nm;
  DWORD len = sizeof(buf)/sizeof(TCHAR);

  if ( GetUserName(buf, &len) )
    return nameToFN(TCHARToName(buf));
  else if ( (nm = getEnvironmentVariablePce(PCE, CtoName("USER"))) )
    return nameToFN(nm);
  else
    return NULL;
}
#endif


#include <shlobj.h>

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
ws_mousebuttons()
{ return GetSystemMetrics(SM_CMOUSEBUTTONS);
}


#ifdef O_IMGLIB
void
remove_ilerrout(int status)
{ unlink("ilerr.out");
}
#endif


#if NOSTUB
void
ws_initialise(int argc, char **argv)
{ if ( ws_mousebuttons() == 2 )
    ws_emulate_three_buttons(100);
}
#endif


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
		 *      COMMON DIALOG STUFF	*
		 *******************************/

HWND
getHwndFrame(FrameObj fr)
{ Cprintf("stub: getHwndFrame()\n");
  return 0;
}

HWND
getHwndWindow(PceWindow sw)
{ Cprintf("stub: getHwndWindow()\n");
  return 0;
}

#define nameToFN(s) charArrayToFN((CharArray)(s))

#include <h/unix.h>

#define strapp(s, q) \
	{ size_t l = _tcslen(q); \
	  if ( s+l+2 > filter+sizeof(filter)/sizeof(TCHAR) ) \
	  { errorPce(filters, NAME_representation, NAME_nameTooLong); \
	    fail; \
	  } \
	  _tcscpy(s, q); \
	  s += l; \
	}

static int
allLetter(const TCHAR *s)
{ for(; *s && _istalpha(*s); s++)
    ;

  return *s ? FALSE : TRUE;
}


#ifndef IsDirSep
#define IsDirSep(c) ((c) == '/' || (c) == '\\')
#endif

static TCHAR *
baseNameW(TCHAR *name)
{ TCHAR *base;

  for(base=name; *name; name++)
  { if ( IsDirSep(*name) && name[1] )
      base = name;
  }

  return base;
}


typedef struct
{ char *name;
  DWORD flag;
} ofn_namedef;

static ofn_namedef ofn_namedefs[] =
{ { "allowmultiselect", OFN_ALLOWMULTISELECT },
  { "createprompt", OFN_CREATEPROMPT },
  { "filemustexist", OFN_FILEMUSTEXIST },
  { "hidereadonly", OFN_HIDEREADONLY },
  { "nodereferencelinks", OFN_NODEREFERENCELINKS  },
  { "nonetworkbutton", OFN_NONETWORKBUTTON },
  { "noreadonlyreturn ", OFN_NOREADONLYRETURN },
  { "notestfilecreate", OFN_NOTESTFILECREATE },
  { "overwriteprompt", OFN_OVERWRITEPROMPT },
  { "pathmustexist", OFN_PATHMUSTEXIST },
  { "readonly", OFN_READONLY },
  { "shareaware", OFN_SHAREAWARE },
  { NULL, 0 }
};


Name
getWinFileNameDisplay(DisplayObj d,
		      Name mode,	/* open, save */
		      Chain filters,	/* tuple(Name, Pattern) */
		      CharArray title,
		      CharArray file,	/* default file */
		      Directory dir,	/* initial dir */
		      Any owner,	/* owner window */
		      Chain options)	/* Flags */
{ OPENFILENAME ofn;
  HWND hwnd;
  Name rval = 0;
  EventObj ev = EVENT->value;
  TCHAR filter[1024], *ef = filter;
  TCHAR buffer[2048];
  TCHAR dirbuf[1024];
  BOOL tmpb;

  memset(&ofn, 0, sizeof(OPENFILENAME));
  ofn.lStructSize = sizeof(OPENFILENAME);

  if ( isInteger(owner) )
    ofn.hwndOwner = (void *)valInt(owner);
  else if ( instanceOfObject(owner, ClassFrame) )
    ofn.hwndOwner = getHwndFrame(owner);
  else if ( instanceOfObject(ev, ClassEvent) &&
	    (hwnd = getHwndWindow(ev->window)) )
    ofn.hwndOwner = hwnd;

  if ( isDefault(filters) )
  { Name nm = get((Any)NAME_allFiles, NAME_labelName, EAV);
    strapp(ef, nameToTCHAR(nm));
    *ef++ = L'\0';
    strapp(ef, _T("*.*"));
    *ef++ = L'\0';
  } else
  { Cell cell;

    for_cell(cell, filters)
    { if ( instanceOfObject(cell->value, ClassTuple) )
      { Tuple t = cell->value;
	CharArray s1 = t->first, s2 = t->second;

	if ( !instanceOfObject(s1, ClassCharArray) )
	{ errorPce(s1, NAME_unexpectedType, TypeCharArray);
	  fail;
	}
	if ( !instanceOfObject(s2, ClassCharArray) )
	{ errorPce(s2, NAME_unexpectedType, TypeCharArray);
	  fail;
	}
	strapp(ef, nameToTCHAR((Name)s1));
	*ef++ = L'\0';
	strapp(ef, nameToTCHAR((Name)s2));
	*ef++ = L'\0';
      } else if ( instanceOfObject(cell->value, ClassCharArray) )
      { StringObj s = cell->value;

	strapp(ef, nameToTCHAR((Name)s));
	*ef++ = L'\0';
	strapp(ef, nameToTCHAR((Name)s));
	*ef++ = L'\0';
      } else
      { errorPce(cell->value, NAME_unexpectedType, CtoType("char_array|tuple"));
	fail;
      }
    }
  }
  *ef = L'\0';
  ofn.lpstrFilter  = filter;
  ofn.nFilterIndex = 0;

  if ( isDefault(file) )
    buffer[0] = L'\0';
  else
  { const TCHAR *fn = nameToTCHAR(file);

    if ( _tcslen(fn) >= sizeof(buffer) )
    { errorPce(file, NAME_representation, NAME_nameTooLong);
      fail;
    }
    _tcscpy(buffer, fn);
  }

  ofn.lpstrFile    = buffer;
  ofn.nMaxFile     = (sizeof(buffer)/sizeof(TCHAR))-1;
  if ( notDefault(dir) )
  { ofn.lpstrInitialDir =
      _xos_os_filenameW(nameToUTF8(dir->path),
			dirbuf, sizeof(dirbuf)/sizeof(TCHAR));
  }
  if ( notDefault(title) )
  ofn.lpstrTitle = nameToTCHAR(title);

  ofn.Flags = OFN_NOCHANGEDIR;

  if ( notDefault(options) )
  { Cell cell;

    for_cell(cell, options)
    { if ( isName(cell->value) )
      { ofn_namedef *dp = ofn_namedefs;

	for(; dp->name; dp++)
	{ if ( streq(strName(cell->value), dp->name) )
	    ofn.Flags |= dp->flag;
	}
      }
    }
  }

  if ( mode == NAME_open )
  { ofn.Flags |= OFN_FILEMUSTEXIST;
    tmpb = GetOpenFileName(&ofn);
  } else
    tmpb = GetSaveFileName(&ofn);

  if ( !tmpb )
  { DWORD w;

    if ( !(w=CommDlgExtendedError()) )
      fail;				/* user canceled */

    Cprintf("Get{Open,Save}FileName() failed: %ld\n", w);
    fail;
  }

  if ( buffer[0] )
  { TCHAR *base = baseNameW(buffer);

    if ( !_tcschr(base, '.') && ofn.nFilterIndex > 0 )
    { TCHAR *pattern = filter;
      TCHAR *ext;
      int n;

      pattern = filter;
      pattern += _tcslen(pattern)+1;	/* first pattern */
      for(n=1; n<ofn.nFilterIndex; n++)
      { pattern += _tcslen(pattern)+1;
	pattern += _tcslen(pattern)+1;
      }

      if ( (ext = _tcsrchr(pattern, '.')) && allLetter(ext+1) )
	_tcscat(buffer, ext);
    }

#ifdef O_XOS				/* should always be true */
  { char buf[PATH_MAX];

    if ( !_xos_canonical_filenameW(buffer, buf, sizeof(buf), 0) )
    { errorPce(TCHARToName(buffer), NAME_representation, NAME_nameTooLong);
      fail;
    }
    rval = UTF8ToName(buf);
  }
#else
    rval = TCHARToName(buffer);
#endif
  }

  return rval;
}


#include <objbase.h>

static INT CALLBACK
BrowseCallbackProc(HWND hwnd,
		   UINT uMsg,
		   LPARAM lp,
		   LPARAM pData)
{ TCHAR szDir[PATH_MAX];

  switch(uMsg)
  { case BFFM_INITIALIZED:
    /* WParam is TRUE since you are passing a path.
       It would be FALSE if you were passing a pidl. */
      SendMessage(hwnd, BFFM_SETSELECTION, TRUE, pData);
      break;
   case BFFM_SELCHANGED:
   /* Set the status window to the currently selected path. */
      if (SHGetPathFromIDList((LPITEMIDLIST) lp, szDir))
      { SendMessage(hwnd,BFFM_SETSTATUSTEXT,0,(LPARAM)szDir);
      }
      break;
   }

   return 0;
}


Name
getWinDirectoryDisplay(DisplayObj d,
		       CharArray title,
		       Directory dir,	/* initial dir */
		       Any owner)	/* owner window */
{ BROWSEINFO bi;
  HWND hwnd;
  EventObj ev = EVENT->value;
  LPITEMIDLIST pidl;
  Name result = NULL;

  memset(&bi, 0, sizeof(bi));

  if ( isInteger(owner) )
    bi.hwndOwner = (void *)valInt(owner);
  else if ( instanceOfObject(owner, ClassFrame) )
    bi.hwndOwner = getHwndFrame(owner);
  else if ( instanceOfObject(ev, ClassEvent) &&
	    (hwnd = getHwndWindow(ev->window)) )
    bi.hwndOwner = hwnd;

  if ( isDefault(title) )
    bi.lpszTitle = L"Choose folder";
  else
    bi.lpszTitle = nameToTCHAR(title);
  bi.ulFlags = (BIF_RETURNONLYFSDIRS|BIF_USENEWUI);
  if ( notDefault(dir) )
  { wchar_t windir[PATH_MAX];

    bi.lParam = (LPARAM)_xos_os_filenameW(nameToFN(dir->path),
					  windir,
					  sizeof(windir)/sizeof(wchar_t));
    if ( bi.lParam )
      bi.lpfn = BrowseCallbackProc;
  }

  CoInitialize(NULL);

  if ( (pidl = SHBrowseForFolder(&bi)) )
  { TCHAR path[PATH_MAX];

    if ( SHGetPathFromIDList(pidl, path) )
    {
#ifdef O_XOS				/* should always be true */
      char buf[PATH_MAX];

      if ( _xos_canonical_filenameW(path, buf, sizeof(buf), 0) )
	result = UTF8ToName(buf);
      else
	errorPce(TCHARToName(path), NAME_representation, NAME_nameTooLong);
#else
      result = TCHARToName(path);
#endif
    }

#if 1
    CoTaskMemFree(pidl);
#else
  { IMalloc *im = NULL;
    if ( SHGetMalloc(&im) == NOERROR )
    { im->Free(pidl);			/* these are C++ methods! */
      im->Release();
    }
  }
#endif
  }

  CoUninitialize();

  return result;
}
