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

#include "include.h"
#include <h/interface.h>
#include <h/unix.h>

/* Allow compilation with old SDKs, also allow working with old OS
   versions that do not provide the functions in user32.dll.
*/

#define COMPILE_MULTIMON_STUBS 1
#include "multimon.h"

void
ws_flush_display(DisplayObj d)
{ ws_synchronise_display(d);
}


void
ws_synchronise_display(DisplayObj d)
{ MSG msg;

  while ( PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) )
  { TranslateMessage(&msg);
    DispatchMessage(&msg);
  }
}


void
ws_bell_display(DisplayObj d, int volume)
{ MessageBeep(MB_ICONEXCLAMATION);
}


void
ws_get_size_display(DisplayObj d, int *w, int *h)
{ HDC  hdc = GetDC(NULL);

#if 0
  *w = GetDeviceCaps(hdc, HORZRES);
  *h = GetDeviceCaps(hdc, VERTRES);
#else
  *w = GetSystemMetrics(SM_CXVIRTUALSCREEN);
  *h = GetSystemMetrics(SM_CYVIRTUALSCREEN);
#endif

  ReleaseDC(NULL, hdc);
}


Name
ws_get_visual_type_display(DisplayObj d)
{ int depth = ws_depth_display(d);

  if ( depth == 1 )
    return NAME_monochrome;
  else if ( depth <= 8 )		/* test for colourmap? */
    return NAME_pseudoColour;
  else
    return NAME_trueColour;
}


int
ws_depth_display(DisplayObj d)
{ HDC  hdc = GetDC(NULL);
  int depth = GetDeviceCaps(hdc, BITSPIXEL);
  ReleaseDC(NULL, hdc);

  return depth;
}


int
ws_resolution_display(DisplayObj d, int *rx, int *ry)
{ HDC hdc = GetDC(NULL);

  *rx = GetDeviceCaps(hdc, LOGPIXELSX);
  *ry = GetDeviceCaps(hdc, LOGPIXELSY);

  ReleaseDC(NULL, hdc);

  succeed;
}


void
ws_activate_screen_saver(DisplayObj d)
{
}


void
ws_deactivate_screen_saver(DisplayObj d)
{
}


void
ws_init_display(DisplayObj d)
{
}


status
ws_legal_display_name(char *s)
{ succeed;
}


status
ws_opened_display(DisplayObj d)
{ if ( d->ws_ref )
    succeed;

  fail;
}


void
ws_open_display(DisplayObj d)
{ d->ws_ref = (WsRef) 1;	/* just flag; nothing to do yet */

  if ( isDefault(d->colour_map) )
  { if ( ws_has_colourmap(d) )
    { int depth = ws_depth_display(d);

      if ( depth == 8 )
      { send(d, NAME_colourMap,
	     newObject(ClassColourMap, CtoName("colour_cube_216"), EAV), EAV);
      }
    } else
      send(d, NAME_colourMap, NIL, EAV);
  }

  ws_init_loc_still_timer();
}


void
ws_quit_display(DisplayObj d)
{ exitDraw(0);
}


static BOOL CALLBACK
next_monitor(HMONITOR m, HDC hdc, LPRECT rect, LPARAM closure)
{ DisplayObj d = (DisplayObj)closure;
  MONITORINFOEX info;
  Any name;
  Monitor mon;

  memset(&info, 0, sizeof(info));
  info.cbSize = sizeof(info);
  if ( GetMonitorInfo(m, (MONITORINFO*)&info) )
  { name = CtoName(info.szDevice);
  } else
  { name = d->monitors->size;
  }

  appendChain(d->monitors,
	      mon=newObject(ClassMonitor,
			    name,
			    newObject(ClassArea,
				      toInt(rect->left),
				      toInt(rect->top),
				      toInt(rect->right - rect->left),
				      toInt(rect->bottom - rect->top),
				      EAV),
			    EAV));
  if ( isName(name) )
  { if ( info.dwFlags & MONITORINFOF_PRIMARY )
      assign(mon, primary, ON);
    assign(mon, work_area,
	   newObject(ClassArea,
		     toInt(info.rcWork.left),
		     toInt(info.rcWork.top),
		     toInt(info.rcWork.right - info.rcWork.left),
		     toInt(info.rcWork.bottom - info.rcWork.top),
		     EAV));
  }

  return TRUE;
}


status
ws_init_monitors_display(DisplayObj d)
{ assign(d, monitors, newObject(ClassChain, EAV));

  EnumDisplayMonitors(NULL, NULL, next_monitor, (LPARAM)d);

  succeed;
}


		 /*******************************
		 *	  MOUSE TRACKING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The %@^%#@& MS-Windows system does  not   tell  the application when the
mouse enters/leaves some window.  News articles   and many mails later I
was hardly any further.  Anyway, the following appears to work:

Use SetWindowsHookEx() to register a WH_MOUSE hook.  When the hwnd field
of the MOUSEHOOKSTRUCT changes, SendMessage()   a  user-defined event to
the window left and entered.  Now, if you do that in the app itself, you
will not see any result if the mouse leaves towards another application.

Therefore we first try to load the pcewh.dll module which does the same,
but in a dll, so it can do the   job system-wide.  If this fails we will
do the job locally, which in any case is better than not at all.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static HWND current_window;

void
PceWhDeleteWindow(HWND win)
{ if ( win == current_window )
    current_window = 0;
}


static void
send_message(HWND win, UINT msg, WPARAM wParam, LPARAM lParam)
{ DWORD owner = GetWindowThreadProcessId(win, NULL);

  if ( owner == GetCurrentThreadId() )
  { SendMessage(win, msg, wParam, lParam);
  } else
  { PostMessage(win, msg, wParam, lParam);
  }
}


void
PceEventInWindow(HWND win)
{ if ( win != current_window )
  { if ( current_window )
    { DEBUG(NAME_areaEnter,
	    Cprintf("Posting exit to %s\n",
		    pp(getObjectFromHWND(current_window))));
      send_message(current_window, WM_WINEXIT, 0, 0L);
    }
    if ( win )
    { DEBUG(NAME_areaEnter,
	    Cprintf("Posting enter to %s\n",
		    pp(getObjectFromHWND(win))));
      send_message(win, WM_WINENTER, 0, 0L);
    }

    current_window = win;
  }
}


static void
init_area_enter_exit_handling(DisplayObj d)
{
}


status
ws_init_graphics_display(DisplayObj d)
{ initDraw();

  init_area_enter_exit_handling(d);

  succeed;
}


void
ws_foreground_display(DisplayObj d, Colour c)
{
}


void
ws_background_display(DisplayObj d, Colour c)
{
}


void
ws_draw_in_display(DisplayObj d, Graphical gr, BoolObj invert, BoolObj subtoo)
{ d_screen(d);
  if ( invert == ON ) r_invert_mode(ON);
  if ( subtoo == ON ) r_subwindow_mode(ON);
  RedrawArea(gr, gr->area);
  r_invert_mode(OFF);
  r_subwindow_mode(OFF);
  d_done();
}


void
ws_grab_server(DisplayObj d)
{
}


void
ws_ungrab_server(DisplayObj d)
{
}


Int
ws_display_connection_number(DisplayObj d)
{ fail;
}


status
ws_events_queued_display(DisplayObj d)
{ return GetInputState() ? SUCCEED : FAIL;
}


status
ws_pointer_location_display(DisplayObj d, int *x, int *y)
{ POINT pt;

  if ( GetCursorPos(&pt) )
  { *x = (int)pt.x;
    *y = (int)pt.y;

    succeed;
  }

  fail;
}


		 /*******************************
		 *     SELECTION HANDLING	*
		 *******************************/

#define CLIPBOARDWIN	PceHiddenWindow()

static HGLOBAL
ws_string_to_global_mem(String s)
{ int size  = s->s_size;
  int extra = str_count_chr(s, 0, s->s_size, '\n');
  HGLOBAL mem;
  wchar_t *data;
  int i;

  if ( !(mem = GlobalAlloc(GMEM_MOVEABLE, (size+extra+1)*sizeof(wchar_t))) )
  { Cprintf("Cannot allocate\n");
    return 0;
  }

  data = GlobalLock(mem);

  if ( isstrA(s) )
  { charA *q;

    for(q=s->s_textA,i=0; i<size; i++)
    { if ( *q == '\n' )
	*data++ = '\r';
      *data++ = *q++;
    }
    *data = EOS;
  } else
  { charW *q;

    for(q=s->s_textW,i=0; i<size; i++)
    { if ( *q == '\n' )
	*data++ = '\r';
      *data++ = *q++;
    }
    *data = EOS;
  }

  GlobalUnlock(mem);

  return mem;
}


status
ws_set_cutbuffer(DisplayObj d, int n, String s)
{ if ( n == 0 )
  { HGLOBAL mem = ws_string_to_global_mem(s);

    OpenClipboard(PceHiddenWindow());
    EmptyClipboard();
    SetClipboardData(CF_UNICODETEXT, mem);
    CloseClipboard();

    succeed;
  }

  Cprintf("Cannot access cut-buffers other than 0\n");
  fail;
}


static Any
get_clipboard_data(DisplayObj d, Name type)
{ HGLOBAL mem;
  HENHMETAFILE hmf;
  Any rval = FAIL;

  OpenClipboard(CLIPBOARDWIN);
  if ( type != NAME_winMetafile && (mem = GetClipboardData(CF_UNICODETEXT)) )
  { wchar_t *data = GlobalLock(mem);
    wchar_t *copy, *q;

    q = copy = pceMalloc((wcslen(data)+1)*sizeof(wchar_t));

    for(; *data; data++)
    { if ( *data == '\r' && data[1] == '\n' )
      { data++;
	*q++ = '\n';
      } else
	*q++ = *data;
    }
    *q = EOS;
    rval = WCToString(copy, q-copy);
    pceFree(copy);
    GlobalUnlock(mem);
  } else if ( type != NAME_winMetafile && (mem = GetClipboardData(CF_TEXT)) )
  { char far *data = GlobalLock(mem);
    char *copy, *q;

    q = copy = pceMalloc(strlen(data));

    for(; *data; data++)
    { if ( *data == '\r' && data[1] == '\n' )
      { data++;
	*q++ = '\n';
      } else
	*q++ = *data;
    }
    *q = EOS;
    rval = CtoString(copy);
    pceFree(copy);
    GlobalUnlock(mem);
  } else if ( type != NAME_text && (hmf = GetClipboardData(CF_ENHMETAFILE)) )
  { HENHMETAFILE copy = CopyEnhMetaFile(hmf, NULL);
    if ( !copy )
    { errorPce(d, NAME_winMetafile, CtoName("CopyEnhMetaFile"), APIError());
      fail;
    }

    rval = CtoWinMetafile(copy);
    DeleteEnhMetaFile(hmf);
  }
  CloseClipboard();

  return rval;
}


StringObj
ws_get_cutbuffer(DisplayObj d, int n)
{ if ( n == 0 )
    return get_clipboard_data(d, NAME_text); /* DEFAULT? */

  Cprintf("Cannot access cut-buffers other than 0\n");
  fail;
}


unsigned long
ws_get_selection_timeout(void)
{ return 0L;
}


void
ws_set_selection_timeout(unsigned long time)
{
}


Any
ws_get_selection(DisplayObj d, Name which, Name target)
{ return get_clipboard_data(d, target);
}


void
ws_renderall(void)
{ HWND hwnd = CLIPBOARDWIN;

  OpenClipboard(hwnd);
  EmptyClipboard();
  CloseClipboard();
}


void
ws_disown_selection(DisplayObj d, Name selection)
{ ws_renderall();
}


int
ws_provide_selection(int format)
{ DisplayObj d = CurrentDisplay(NIL);
  Hyper h;
  Function msg;
  Name which     = NAME_primary;
  Name hypername = getAppendName(which, NAME_selectionOwner);
  Name type;

  if ( d && notNil(d) &&
       (h    = getFindHyperObject(d, hypername, DEFAULT)) &&
       (type = getAttributeObject(h, NAME_type)) &&
       (msg  = getAttributeObject(h, NAME_convertFunction)) &&
       (msg  = checkType(msg, TypeFunction, NIL)) )
  { Any val;

    DEBUG(NAME_selection, Cprintf("Provide %s selection of type %s\n",
				  pp(which), pp(type)));

    if ( !(val = getForwardReceiverFunction(msg, h->to, which, type, EAV)) )
      return FALSE;

    DEBUG(NAME_selection, Cprintf("Got %s\n", pp(val)));

    if ( type == NAME_text )
    { CharArray ca = checkType(val, TypeCharArray, NIL);

      if ( ca )
      { String s = &ca->data;
      	HGLOBAL mem = ws_string_to_global_mem(s);

	if ( mem )
	  SetClipboardData(CF_UNICODETEXT, mem);

	return TRUE;
      }
    } else if ( type == NAME_emf || type == NAME_wmf )
    { Any mf = checkType(val, nameToType(NAME_winMetafile), NIL);

      if ( mf )
      { DEBUG(NAME_selection, Cprintf("Providing win_metafile\n"));
	return ws_on_clipboard_metafile(mf, type);
      }
    } else
      return errorPce(d, NAME_noSelectionType, type);
  }

  return FALSE;
}


status
ws_own_selection(DisplayObj d, Name selection, Name type)
{ HWND hwnd = CLIPBOARDWIN;
  UINT format;

  if ( type == NAME_emf )
    format = CF_ENHMETAFILE;
  else if ( type == NAME_wmf )
    format = CF_METAFILEPICT;
  else if ( type == NAME_text)
    format = CF_UNICODETEXT;
  else
    return errorPce(d, NAME_noSelectionType, type);

  DEBUG(NAME_selection, Cprintf("%s becomes owner of %selection, type %s\n",
				pp(d), pp(selection), pp(type)));

  OpenClipboard(hwnd);
  EmptyClipboard();
  SetClipboardData(format, NULL);
  CloseClipboard();

  succeed;
}


Name
ws_window_manager(DisplayObj d)
{ answer(CtoName("windows"));
}


void
ws_synchronous(DisplayObj d)
{
}


void
ws_asynchronous(DisplayObj d)
{
}


status
ws_postscript_display(DisplayObj d, int iscolor)
{ int w = valInt(getWidthDisplay(d));
  int h = valInt(getHeightDisplay(d));
  HDC hdc = GetDC(NULL);
  int depth = GetDeviceCaps(hdc, BITSPIXEL);

  switch(depth)
  { case 1:
      break;
    case 2:
    case 4:
    case 8:				/* colour-mapped */
    case 16:
      depth = 4;			/* low-res true-color */
    case 24:
    case 32:
      depth = 8;			/* high-res true color */
  }

  ps_output("0 0 ~D ~D ~D ~N\n", w, h,
	    depth, iscolor ? NAME_rgbimage : NAME_greymap);
  postscriptDC(hdc, 0, 0, w, h, depth, iscolor);
  ps_output("\n");

  succeed;
}


Image
ws_grab_image_display(DisplayObj d, int x, int y, int width, int height)
{ HDC hdc = GetDC(NULL);
  RECT rect;
  Image image;
  int w, h;
  HBITMAP obm, bm;
  HDC hdcimg;
  Size size = getSizeDisplay(d);

  rect.left   = x;
  rect.top    = y;
  rect.right  = x + width;
  rect.bottom = y + height;
  if ( rect.left < 0 ) rect.left = 0;
  if ( rect.top < 0 )  rect.top  = 0;
  if ( rect.bottom > valInt(size->h) ) rect.bottom = valInt(size->h);
  if ( rect.right >  valInt(size->w) ) rect.right  = valInt(size->w);

  w = rect.right - rect.left;
  h = rect.bottom - rect.top;

  image = answerObject(ClassImage, NIL,
		       toInt(w), toInt(h), NAME_pixmap, EAV);
  assign(image, display, d);
  bm = ZCreateCompatibleBitmap(hdc, w, h);
  hdcimg = CreateCompatibleDC(hdc);
  obm = SelectObject(hdcimg, bm);

  BitBlt(hdcimg, 0, 0, w, h, hdc, rect.left, rect.top, SRCCOPY);

  SelectObject(hdcimg, obm);
  ZDeleteObject(hdcimg);
  ReleaseDC(NULL, hdc);

  registerXrefObject(image, image->display, (void *) bm);

  return image;
}
