/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

#include <windows.h>
#include "xpcemh.h"

#ifndef DEBUGGING
#define DEBUGGING 2			/* 0: nodebug */
					/* 1: task-level */
					/* 2: window-level */
#endif

#if DEBUGGING
#include <SWI-Stream.h>
#endif

#define _export __declspec(dllexport)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The module ensures we get area enter/exit   messages in XPCE windows. It
should be installed as xpcemh.dll in one of your $PATH directories. This
module installs a global mouse hook-function   that monitors the current
window. If the current window changes, it  will send an area-exit to the
left and area-enter to the new current window.

This is in a dll because system-wide mouse hooks can only run in a dll.
Interface:

	DeleteWindow(HWND)	Window is deleted.  Don't inform exit.

See xpce/src/msw/display.c for how this dll is used.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define MAX_PCE_TASKS 10

static LRESULT CALLBACK	mouse_hook(int code, WPARAM wParam, LPARAM lParam);
static int		AddProcess(DWORD id);
static int		DeleteProcess(DWORD id);
static HHOOK		defhook;

static DWORD	process_list[MAX_PCE_TASKS];
static HWND	cwin;			/* current window */
static int	cwin_is_pce_window;
static int	cwin_deleted;		/* cwin has been deleted */
static int	count;			/* access counts */

BOOL WINAPI
init_mouse_hook(HINSTANCE hInstance, DWORD reason, LPVOID context)
{ if ( reason == DLL_PROCESS_ATTACH )
  { if ( count == 0 )
    { if ( !(defhook = SetWindowsHookEx(WH_MOUSE, mouse_hook, hInstance, 0)) )
	return FALSE;
    }

    AddProcess(GetCurrentProcessId());

    count++;
  } else if ( reason == DLL_PROCESS_DETACH )
  { DeleteProcess(GetCurrentProcessId());

    if ( --count == 0 )
      UnhookWindowsHookEx(defhook);
  }

  return TRUE;
}


static int
AddProcess(DWORD pid)
{ int i;
#if DEBUGGING > 0
  Sprintf("Added pid = %d\n", pid);
#endif

  for(i=0; i<MAX_PCE_TASKS; i++)
    if ( process_list[i] == pid )
      return 0;

  for(i=0; i<MAX_PCE_TASKS; i++)
  { if ( !process_list[i] )
    { process_list[i] = pid;
      return 0;
    }
  }

  return -1;
}


static int
DeleteProcess(DWORD pid)
{ int i;
#if DEBUGGING > 0
  Sprintf("Deleted pid = %d\n", pid);
#endif

  for(i=0; i<MAX_PCE_TASKS; i++)
  { if ( process_list[i] == pid )
    { process_list[i] = 0;
      return 0;
    }
  }

  return -1;
}


_export int
MouseHookDeleteWindow(HWND win)
{
#if DEBUGGING > 0
  Sprintf("Deleted window 0x%x\n", win);
#endif

  if ( win == cwin )
    cwin_deleted = 1;

  return 0;
}


static int
isPceWindow(HWND hwnd)
{ DWORD pid;
  int i;

  GetWindowThreadProcessId(hwnd, &pid);

  for(i=0; i<MAX_PCE_TASKS; i++)
    if ( process_list[i] == pid )
      return 1;

  return 0;
}


static LRESULT CALLBACK
mouse_hook(int code, WPARAM wParam, LPARAM lParam)
{ if ( code == HC_ACTION )
  { MOUSEHOOKSTRUCT *data = (MOUSEHOOKSTRUCT *)lParam;

    if ( data->hwnd != cwin )
    { int ispce = isPceWindow(data->hwnd);
#if DEBUGGING > 2
      Sprintf("Windows %s0x%x --> %s0x%x\n",
	      cwin && isPceWindow(cwin) ? "*" : "", cwin,
	      isPceWindow(data->hwnd) ? "*" : "", data->hwnd);
#endif

      if ( cwin && cwin_is_pce_window && !cwin_deleted )
      {
#if DEBUGGING > 1
	Sprintf("WM_WINEXIT to 0x%x\n", cwin);
#endif
	SendMessage(cwin, WM_WINEXIT, 0, 0L);
      }
      cwin               = data->hwnd;
      cwin_is_pce_window = ispce;
      cwin_deleted       = 0;
      if ( cwin && cwin_is_pce_window )
      {
#if DEBUGGING > 1
        Sprintf("WM_WINENTER to 0x%x\n", cwin);
#endif
	SendMessage(cwin, WM_WINENTER, 0, 0L);
      }
    }

    return 0;
  }

  return CallNextHookEx(defhook, code, wParam, lParam);
}
