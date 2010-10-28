/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

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

#define UNICODE 1
#define _UNICODE 1
#include "include.h"
#include <tchar.h>

static int WINAPI frame_wnd_proc(HWND win, UINT msg, UINT wP, LONG lP);
static status     keyboard_event_frame(FrameObj fr, Any id,
				       UINT wParam, LONG lParam,
				       unsigned long bmask);
static void       paint_icon(FrameObj fr);

#define MainWindow(fr)	     ( isNil(fr->members->head) ? (Any) fr : \
			       fr->members->head->value )

static FrameObj current_frame;		/* hack for timing problem */

#ifndef CS_DROPSHADOW			/* SDK constant */
#define CS_DROPSHADOW 0x00020000
#endif

static const TCHAR *
store_stringW(TCHAR *in)
{ size_t bytes = (_tcslen(in)+1)*sizeof(TCHAR);
  TCHAR *copy = pceMalloc(bytes);

  memcpy(copy, in, bytes);

  return copy;
}


static const TCHAR *
WinFrameClass()
{ static WNDCLASS wndClass;
  static const TCHAR *cname = NULL;

  if ( !cname )
  { TCHAR buf[50];

    wsprintf(buf, _T("PceFrame%ld"), (unsigned long)PceHInstance);
    cname = store_stringW(buf);

    wndClass.style		= 0;
    wndClass.lpfnWndProc	= (LPVOID) frame_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= PceHInstance;
    wndClass.hIcon		= NULL; /*LoadIcon(NULL, IDI_APPLICATION);*/
    wndClass.hCursor		= NULL;
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= cname;

    RegisterClass(&wndClass);
  }

  return cname;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: CS_DROPSHADOW is an XP extension.   On  older Windows versions the
class registration fails. We try the  first   time  to  find out whether
dropshadow is supported (reported as Bug#81).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const TCHAR *
WinPopupFrameClass()
{ static const TCHAR *cname = NULL;
  static WNDCLASS wndClass;
  static int style = (CS_SAVEBITS|CS_DROPSHADOW);

  if ( !cname )
  { TCHAR buf[50];

    wsprintf(buf, _T("PcePopupFrame%ld"), (unsigned long)PceHInstance);
    cname = store_stringW(buf);

  retry:
    wndClass.style		= style;
    wndClass.lpfnWndProc	= (LPVOID) frame_wnd_proc;
    wndClass.cbClsExtra		= 0;
    wndClass.cbWndExtra		= 0;
    wndClass.hInstance		= PceHInstance;
    wndClass.hIcon		= NULL; /*LoadIcon(NULL, IDI_APPLICATION);*/
    wndClass.hCursor		= LoadCursor(NULL, IDC_ARROW);
    wndClass.hbrBackground	= GetStockObject(WHITE_BRUSH);
    wndClass.lpszMenuName	= NULL;
    wndClass.lpszClassName	= cname;

    if ( !RegisterClass(&wndClass) && (style&CS_DROPSHADOW) )
    { style &= ~CS_DROPSHADOW;
      goto retry;
    }
  }

  return cname;
}


HPALETTE
frame_palette(FrameObj fr)
{ ColourMap cm = fr->colour_map;

  if ( isDefault(cm) && notNil(fr->display) )
    cm = fr->display->colour_map;

  if ( isNil(cm) || isDefault(cm) )
    return NULL;
  else
    return getPaletteColourMap(cm);
}


static int
do_frame_wnd_proc(FrameObj fr,
		  HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ DEBUG(NAME_event,
	Cprintf("%s(0x%04x): MS-Windows event 0x%04x with 0x%04x/0x%08lx\n",
		pp(fr), hwnd, message, wParam, lParam));

  switch(message)
  { case WM_CREATE:
    { DragAcceptFiles(hwnd, TRUE);
      break;
    }

#if 0					/* does not work in Windows 95! */
    case WM_WINDOWPOSCHANGED:
    { LPWINDOWPOS wpos = (LPWINDOWPOS) lParam;

      if ( (wpos->flags & SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER) ==
						   SWP_NOSIZE|SWP_NOMOVE )
	send(fr, NAME_exposed, EAV);
/*
      Cprintf("hwnd = 0x%x, insertAfter = 0x%x, flags = 0x%x\n",
	      wpos->hwnd, wpos->hwndInsertAfter,
	      wpos->flags);
*/
      break;
    }
#endif

    case WM_SIZE:			/* frame resized */
    { int w = LOWORD(lParam);
      int h = HIWORD(lParam);

      switch( wParam )
      { case SIZE_MINIMIZED:
	{ Cell cell;

	  DEBUG(NAME_frame, Cprintf("Minimized %s\n", pp(fr)));
	  SetWindowText(hwnd, nameToWC(getIconLabelFrame(fr), NULL));
	  assign(fr, status, NAME_iconic);
	  for_cell(cell, fr->members)
	    DisplayedGraphical(cell->value, OFF);
	  break;
	}
	case SIZE_RESTORED:
	{ WsFrame f = fr->ws_ref;

	  if ( f )
	  { RECT rect;

	    if ( GetWindowRect(hwnd, &rect) )
	    { f->bbx = rect.left;
	      f->bby = rect.top;
	      f->bbw = rect.right - rect.left;
	      f->bbh = rect.bottom - rect.top;
	      f->bb  = TRUE;
	    } else
	      f->bb = FALSE;
	  }
	}
	/*FALLTHROUGH*/
	case SIZE_MAXIMIZED:
	  DEBUG(NAME_frame, Cprintf("Resized %s to %d x %d\n", pp(fr), w, h));
	  assign(fr->area, w, toInt(w));
	  assign(fr->area, h, toInt(h));

	  if ( IsWindowVisible(hwnd) )
	  { Cell cell;

	    send(fr, NAME_resize, EAV);
	    SetWindowText(hwnd, nameToWC(fr->label, NULL));
	    assign(fr, status, wParam == SIZE_MAXIMIZED ? NAME_fullScreen
		   					: NAME_window);
	    for_cell(cell, fr->members)
	      DisplayedGraphical(cell->value, ON);
	  }
	  break;
      }

      goto repaint;
    }

    case WM_MOVE:			/* frame moved */
    { POINTS pt = MAKEPOINTS(lParam);

      DEBUG(NAME_frame, Cprintf("Moved %s to %d, %d\n", pp(fr), pt.x, pt.y));
      assign(fr->area, x, toInt(pt.x));
      assign(fr->area, y, toInt(pt.y));

      return 0;
    }

    case WM_SHOWWINDOW:
    { HWND hwnd;
      Cell cell;

      if ( !wParam && (hwnd = getHwndFrame(fr)) )
      { Cell cell;

	for_cell(cell, fr->members)
	{ HWND subhwnd = getHwndWindow(cell->value);

	  if ( subhwnd )
	    PceWhDeleteWindow(subhwnd);
	}

	PceWhDeleteWindow(hwnd);
      }


      if ( wParam )			/* show on */
      { for_cell(cell, fr->members)
	{ extern void unlink_changes_data_window(PceWindow sw);

	  send(cell->value, NAME_displayed, ON, EAV);
	  ComputeGraphical(cell->value);
	  unlink_changes_data_window(cell->value);
	}

	send(fr, NAME_mapped, ON, EAV);

	assign(fr, status, NAME_window); /* Or full_screen? */
	ws_set_icon_frame(fr);
      } else				/* show off */
      { for_cell(cell, fr->members)
	{ if ( !onFlag(cell->value, F_FREED|F_FREEING) )
	    send(cell->value, NAME_displayed, OFF, EAV);
	}

	if ( !isFreedObj(fr) || isFreeingObj(fr) )
	  send(fr, NAME_mapped, OFF, EAV);

	assign(fr, status, NAME_hidden);
      }

      goto repaint;
    }

    case WM_SETFOCUS:
    case WM_KILLFOCUS:
    { BoolObj val = (message == WM_SETFOCUS ? ON : OFF);

      send(fr, NAME_inputFocus, val, EAV);
      goto repaint;
    }

    case WM_QUERYNEWPALETTE:
    case_query:
    { HPALETTE hpal = frame_palette(fr);
      if ( hpal )
      { HDC hdc = GetDC(hwnd);
	int i = 0;

	hpal = SelectPalette(hdc, hpal, FALSE);
	i = RealizePalette(hdc);
	SelectPalette(hdc, hpal, TRUE);
	RealizePalette(hdc);
	ReleaseDC(hwnd, hdc);

	if ( i > 0 )
	{ forwardColourMapChangeFrame(fr);
	  return TRUE;
	}
      }

      return FALSE;
    }
    case WM_PALETTECHANGED:
      if ( (HWND)wParam != hwnd )
	goto case_query;

      return FALSE;

    case WM_ERASEBKGND:			/* TODO: Add colourmap code */
    { HDC hdc = (HDC) wParam;
      RECT rect;
      COLORREF rgb = (COLORREF) getXrefObject(fr->background, fr->display);
      HBRUSH hbrush;

      rgb = GetNearestColor(hdc, rgb);
      hbrush = ZCreateSolidBrush(rgb);
      GetClipBox(hdc, &rect);
      FillRect(hdc, &rect, hbrush);
      ZDeleteObject(hbrush);

      DEBUG(NAME_redraw, Cprintf("Cleared background %d %d %d %d of %s\n",
				 rect.left, rect.top,
				 rect.right - rect.left,
				 rect.bottom - rect.top,
				 pp(fr)));

      return 1;				/* non-zero: I've erased it */
    }

    case WM_PAINT:
      if ( IsIconic(hwnd) )
      { paint_icon(fr);
	return 0;
      } else
        goto win_default;

#ifdef WM_MOUSEWHEEL			/* distributed as key-event */
    case WM_MOUSEWHEEL:
      DEBUG(NAME_wheel, Cprintf("Got WM_MOUSEWHEEL on %s\n", pp(fr)));
#endif
    case WM_KEYDOWN:			/* Named keys */
    case WM_SYSCHAR:			/* ALT-commands */
    case WM_CHAR:			/* Printable keys */
    { unsigned long bmask;
      Any id = messageToKeyId(message, wParam, lParam, &bmask);

      DEBUG(NAME_wheel,
	    if ( id == NAME_wheel )
	      Cprintf("Translated to wheel-event\n"));

      if ( id && keyboard_event_frame(fr, id, wParam, lParam, bmask) )
	return 0;

      break;
    }
    case WM_SYSCOMMAND:			/* prevent loosing the mouse on ALT */
      if ( (wParam & 0xfff0) == SC_KEYMENU )
	return 0;
      break;

    case WM_CLOSE:
    { Code msg;

      if ( (msg = checkType(getValueSheet(fr->wm_protocols,
					  CtoName("WM_DELETE_WINDOW")),
			    TypeCode, fr)) )
      { DEBUG(NAME_close, Cprintf("Running WM_DELETE_WINDOW message %s\n",
				  pp(msg)));
	forwardReceiverCode(msg, fr, MainWindow(fr), EAV);
	DEBUG(NAME_close, Cprintf("Finished WM_DELETE_WINDOW. fr=%s, msg=%s\n",
				  pp(fr), pp(msg)));
      }

      return 0;
    }

    case WM_DESTROY:
    { HWND hwnd = getHwndFrame(fr);

      DEBUG(NAME_window, Cprintf("WM_DESTROY on %s, hwnd 0x%x\n",
				 pp(fr), hwnd));
      if ( hwnd )
      { DragAcceptFiles(hwnd, FALSE);
	setHwndFrame(fr, 0);
	assocObjectToHWND(hwnd, NIL);
	freeObject(fr);
      }

      return 0;
    }

    case WM_SETCURSOR:
    { if ( LOWORD(lParam) == HTCLIENT )
      { WsFrame f = fr->ws_ref;

	if ( f )
	{ if ( !f->hcursor )
	    f->hcursor = LoadCursor(NULL, IDC_ARROW);

	  ZSetCursor(f->hcursor);
	}

	return 1;
      }

      break;
    }

#if 0
    case WM_PARENTNOTIFY:
    { int  fwEvent = LOWORD(wParam);
      HWND child   = HIWORD(wParam);

      if ( fwEvent == WM_DESTROY )
      { DEBUG(NAME_window, Cprintf("%s: child 0x%x destroyed\n",
				   pp(fr), hwnd));
	return 0;
      }

      break;
    }
#endif
  }

  { EventObj ev;
    AnswerMark mark;
    status rval = FALSE;
    markAnswerStack(mark);

    if ( (ev = messageToEvent(hwnd, message, wParam, lParam)) )
    { if ( message != WM_WINENTER && message != WM_WINEXIT )
	PceEventInWindow(hwnd);

      addCodeReference(ev);
      rval = send(fr, NAME_event, ev, EAV);
      delCodeReference(ev);
      freeableObj(ev);
    }
    rewindAnswerStack(mark, NIL);

    if ( rval )
    { RedrawDisplayManager(TheDisplayManager());
      return 0;
    }
  }

repaint:
  RedrawDisplayManager(TheDisplayManager());

win_default:
  return DefWindowProc(hwnd, message, wParam, lParam);
}


static int
service_frame(FrameObj fr)
{ Application app = fr->application;

  return (notNil(app) && app->kind == NAME_service ? PCE_EXEC_SERVICE
						   : PCE_EXEC_USER);
}


static int WINAPI
frame_wnd_proc(HWND hwnd, UINT message, UINT wParam, LONG lParam)
{ FrameObj fr = getObjectFromHWND(hwnd);
  int rval;

  if ( !fr )
  { fr = current_frame;
    if ( !fr )
      return DefWindowProc(hwnd, message, wParam, lParam);
  }
  assert(isProperObject(fr));

  if ( InSendMessage() )
  { if ( pceMTTryLock(LOCK_PCE) )
    { goto has_lock;
    } else
    { FrameObj fr = getObjectFromHWND(hwnd);
      DEBUG(NAME_thread,
	    Cprintf("[Thread 0x%x] %s: message 0x%04x: could not lock\n",
		    GetCurrentThreadId(), pp(fr), message));
      return DefWindowProc(hwnd, message, wParam, lParam);
    }
  } else
  { pceMTLock(LOCK_PCE);
  has_lock:
    ServiceMode(service_frame(fr),
		rval = do_frame_wnd_proc(fr, hwnd, message, wParam, lParam));
    pceMTUnlock(LOCK_PCE);
  }

  return rval;
}


static void
paint_icon(FrameObj fr)
{ if ( notNil(fr->icon_image) )
  { HWND hwnd = getHwndFrame(fr);
    PAINTSTRUCT ps;
    HDC  hdc  = BeginPaint(hwnd, &ps);
    HBITMAP bm = (HBITMAP) getXrefObject(fr->icon_image, fr->display);
    Size is = fr->icon_image->size;
    HDC mhdc = CreateCompatibleDC(hdc);
    HBITMAP obm;

    obm = SelectObject(mhdc, bm);
    StretchBlt(hdc, 0, 0, valInt(fr->area->w), valInt(fr->area->h),
	       mhdc, 0, 0, valInt(is->w), valInt(is->h), SRCCOPY);
    SelectObject(mhdc, obm);
    DeleteDC(mhdc);

    EndPaint(hwnd, &ps);
  }
}


static void
get_point_frame(FrameObj fr, POINT *pt)
{ GetCursorPos(pt);
  pt->x -= valInt(fr->area->x);
  pt->y -= valInt(fr->area->y);
}


PceWindow
get_window_holding_point(FrameObj fr, POINT *pt)
{ HWND win;
  PceWindow sw;

  if ( (win = ChildWindowFromPoint(getHwndFrame(fr), *pt)) &&
       (sw  = getObjectFromHWND(win)) &&
       instanceOfObject(sw, ClassWindow) )
    return sw;

  fail;
}


static status
keyboard_event_frame(FrameObj fr, Any id,
		     UINT wParam, LONG lParam,
		     unsigned long bmask)
{ PceWindow sw;
  POINT pt;
  EventObj ev;
  AnswerMark mark;
  status rval = FALSE;
  Any receiver;
  unsigned long m;

  get_point_frame(fr, &pt);
  if ( !(sw = get_window_holding_point(fr, &pt)) &&
       !(sw = getKeyboardFocusFrame(fr)) )
    receiver = fr;
  else
    receiver = userWindow(sw);

  markAnswerStack(mark);

  if ( sw )
  { pt.x -= valInt(sw->area->x) + valInt(sw->pen);
    pt.y -= valInt(sw->area->y) + valInt(sw->pen);
  }
  ev = answerObject(ClassEvent, id, receiver, toInt(pt.x), toInt(pt.y), EAV);
  m = valInt(ev->buttons);
  m &= ~(BUTTON_shift|BUTTON_control|BUTTON_meta);
  m |= bmask;
  assign(ev, buttons, toInt(m));

  if ( id == NAME_wheel )
  { short a = (short)HIWORD(wParam);
    Any angle = toInt(a);

    attributeObject(ev, NAME_rotation, angle);
    DEBUG(NAME_wheel,
	  Cprintf("Posting wheel %s degrees to %s\n",
		  pp(angle), pp(receiver)));
  }

  addCodeReference(ev);
  rval = postNamedEvent(ev, receiver, DEFAULT, NAME_postEvent);
  delCodeReference(ev);
  freeableObj(ev);

  rewindAnswerStack(mark, NIL);

  RedrawDisplayManager(TheDisplayManager());

  succeed;
}


PceWindow
ws_window_holding_point_frame(FrameObj fr)
{ POINT pt;

  get_point_frame(fr, &pt);
  return userWindow(get_window_holding_point(fr, &pt));
}


status
ws_created_frame(FrameObj fr)
{ if ( getHwndFrame(fr) )
    succeed;

  fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ws_uncreate_frame(FrameObj fr) is called  by   `frame  ->uncreate'.   It
calls  DestroyWindow(),  which  in  turn   will  destroy  the  MS-Window
subwindows, causing the WM_DESTROY action on these windows.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_uncreate_frame(FrameObj fr)
{ HWND hwnd = getHwndFrame(fr);

  if ( hwnd )
  { Cell cell;

    setHwndFrame(fr, 0);
    assocObjectToHWND(hwnd, NIL);
    PceWhDeleteWindow(hwnd);

    for_cell(cell, fr->members)
    { HWND subhwnd = getHwndWindow(cell->value);
      if ( subhwnd )
	PceWhDeleteWindow(subhwnd);
    }

    DestroyWindow(hwnd);
  }
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NOTE: transients are a bit complicated. I'd   like to have a window with
just a title and a close-button. The  below appears to achieve that. The
icon_image must be set to @nil.

If we are a transient window we must set the parent handle. This ensures
we stay on top of the parent.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
ws_create_frame(FrameObj fr)
{ HWND ref;
  HWND owner = NULL;
  DWORD style = WS_CLIPCHILDREN;
  DWORD exstyle = 0;
  RECT rect;
  int w, h;

  if ( fr->kind == NAME_popup )
  { style |= WS_POPUP;
    if ( fr->border != ZERO )
      style |= WS_BORDER;
    exstyle |= WS_EX_TOOLWINDOW;
  } else
  { if ( fr->kind == NAME_toplevel )
    { style |= WS_OVERLAPPEDWINDOW;
    } else if ( fr->kind == NAME_transient )
    { int dec;

      dec = (getClassVariableValueObject(fr, NAME_decorateTransient) == ON);
      if ( notNil(fr->transient_for) )
	owner = getHwndFrame(fr->transient_for);

      { style = WS_POPUP;
	if ( dec )
	{ style	|= WS_POPUPWINDOW|WS_CAPTION;
	  exstyle |= WS_EX_DLGMODALFRAME;
	} else
	  style |= WS_DLGFRAME;
      }
    }

    if ( fr->can_resize == ON )
    { style |= (WS_MAXIMIZEBOX|WS_MINIMIZEBOX|WS_SIZEBOX);
    } else
    { style &= ~(WS_MAXIMIZEBOX|WS_MINIMIZEBOX|WS_SIZEBOX);
    }
  }

  if ( fr->sensitive == OFF )
    style |= WS_DISABLED;

  rect.left   = valInt(fr->area->x);
  rect.top    = valInt(fr->area->y);
  rect.right  = rect.left + valInt(fr->area->w);
  rect.bottom = rect.top  + valInt(fr->area->h);
  AdjustWindowRectEx(&rect, style, FALSE, exstyle);
  if ( rect.left < 0 )
  { rect.right -= rect.left;
    rect.left = 0;
  }
  if ( rect.top < 0 )
  { rect.bottom -= rect.top;
    rect.top = 0;
  }
  w = rect.right - rect.left;
  h = rect.bottom - rect.top;
  if ( w < 5 )
    w = 5;
  if ( h < 5 )
    h = 5;

  DEBUG(NAME_frame, Cprintf("Creating %s area(%d,%d,%d,%d)\n",
			    pp(fr), rect.left, rect.top, w, h));

  current_frame = fr;
  ref = CreateWindowEx(exstyle,
		       fr->kind == NAME_popup ? WinPopupFrameClass()
					      : WinFrameClass(),
		       nameToWC(fr->label, NULL),
		       style,
		       rect.left, rect.top, w, h,
		       owner,
		       NULL,		/* menu */
		       PceHInstance,
		       NULL);		/* Creation data */

  if ( !ref )
    return errorPce(fr, NAME_xOpen, fr->display);

  setHwndFrame(fr, ref);
  assocObjectToHWND(ref, fr);
  current_frame = NULL;

  { WsFrame f = fr->ws_ref;
    f->style = style;
    f->styleex = exstyle;
  }

  succeed;
}


void
ws_realise_frame(FrameObj fr)
{ Cell cell;

  for_cell(cell, fr->members)
  { PceWindow sw = cell->value;

    ShowWindow(getHwndWindow(sw), SW_SHOW);
  }
}

void
ws_frame_border(FrameObj fr, int *xb, int *yb, int *ycap)
{ *xb   = GetSystemMetrics(SM_CXBORDER);
  *yb   = GetSystemMetrics(SM_CYBORDER);
  *ycap = GetSystemMetrics(SM_CYCAPTION);
}

#define PLACE_MARGIN 30			/* don't place on the border */
#define PLACE_X_OFFSET 20		/* offsets */
#define PLACE_Y_OFFSET 30

static void
ws_place_frame(FrameObj fr)
{ static int last_x = 0, last_y = 0;
  static int placed = 0;
  Monitor mon;
  int mx, my, mw, mh;
  int fw = valInt(fr->area->w);
  int fh = valInt(fr->area->h);
  int xborder, yborder, ycap;

  if ( (mon=getMonitorDisplay(fr->display, DEFAULT)) )
  { mx = valInt(mon->area->x);
    my = valInt(mon->area->y);
    mw = valInt(mon->area->w);
    mh = valInt(mon->area->h);
  } else
  { mx = my = 0;
    mw = valInt(getWidthDisplay(fr->display));
    mh = valInt(getHeightDisplay(fr->display));
    mon = (Monitor)DEFAULT;
  }

  ws_frame_border(fr, &xborder, &yborder, &ycap);
  yborder += ycap;

  if ( !placed++ )
  { last_x = rand() % (mw-fw-2*PLACE_MARGIN);
    last_y = rand() % (mh-fh-2*PLACE_MARGIN);
  } else
  { last_x += PLACE_X_OFFSET;
    last_y += PLACE_Y_OFFSET;
  }

  if ( last_x + fw > mw - PLACE_MARGIN )
  { last_x = PLACE_MARGIN;
    if ( last_x + fw > mw )
      last_x = 0;
  }
  if ( last_y + fh > mh - PLACE_MARGIN )
  { last_y = PLACE_MARGIN;
    if ( last_y + fh > mh )
      last_y = 0;
  }

  last_x = max(xborder, last_x);
  last_y = max(yborder, last_y);

  send(fr, NAME_set, toInt(last_x), toInt(last_y), DEFAULT, DEFAULT, mon, EAV);
}


void
ws_raise_frame(FrameObj fr)
{ UINT flags = SWP_NOMOVE|SWP_NOSIZE;

  if ( fr->sensitive == OFF )
    flags |= SWP_NOACTIVATE;

  SetWindowPos(getHwndFrame(fr),
	       HWND_TOP,
	       0, 0, 0, 0,
	       flags);
}


void
ws_lower_frame(FrameObj fr)
{ SetWindowPos(getHwndFrame(fr),
	       HWND_BOTTOM,
	       0, 0, 0, 0,
	       SWP_NOMOVE|SWP_NOSIZE);
}


void
ws_topmost_frame(FrameObj fr, BoolObj topmost)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) )
  { SetWindowPos(hwnd,
		 topmost == ON ? HWND_TOPMOST : HWND_NOTOPMOST,
		 0, 0, 0, 0,
		 SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE);
  }
}


int
ws_enable_frame(FrameObj fr, int enable)
{ HWND hwnd;

  if ( (hwnd=getHwndFrame(fr)) )
  { if ( EnableWindow(hwnd, enable) )
      succeed;
  }

  fail;
}


status
ws_attach_wm_prototols_frame(FrameObj fr)
{ succeed;
}


status
ws_frame_bb(FrameObj fr, int *x, int *y, int *w, int *h)
{ HWND hwnd;

  if ( (hwnd=getHwndFrame(fr)) )
  { RECT rect;

    if ( fr->status == NAME_iconic )
    { WsFrame f = fr->ws_ref;

      if ( f->bb )
      { *x = f->bbx;
        *y = f->bby;
        *w = f->bbw;
        *h = f->bbh;

        succeed;
      }
    } else if ( GetWindowRect(hwnd, &rect) )
    { *x = rect.left;
      *y = rect.top;
      *w = rect.right - rect.left;
      *h = rect.bottom - rect.top;
      DEBUG(NAME_geometry, Cprintf("Got bb=%d %d %d %d from %p\n",
				   *x, *y, *w, *h, hwnd));

      succeed;
    }
  }

  fail;
}


#define MIN_VISIBLE 32			/* pixels that must be visible */

void
ws_x_geometry_frame(FrameObj fr, Name spec, Monitor mon)
{ char *e, *s = strName(spec);
  UINT flags = SWP_NOACTIVATE|SWP_NOZORDER;
  int x, y, w, h, w0, h0;
  char signx[10], signy[10];
  int ok=0;
  WsFrame f = fr->ws_ref;
  int ew, eh;
  int dx, dy, dw, dh;
  RECT rect;

  if ( isDefault(mon) && (e=strchr(s, '@')) )
  { int n = atoi(e+1);

    if ( !(mon = getNth0Chain(fr->display->monitors, toInt(n))) )
      mon = (Monitor)DEFAULT;
  }

  if ( instanceOfObject(mon, ClassMonitor) )
  { Area a = (notNil(mon->work_area) ? mon->work_area : mon->area);

    dx = valInt(a->x);
    dy = valInt(a->y);
    dw = valInt(a->w);
    dh = valInt(a->h);
  } else if ( SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0) )
  { dx = rect.left;
    dy = rect.top;
    dw = rect.right - rect.left;
    dh = rect.bottom - rect.top;
  } else
  { dx = dy = 0;
    dw = valInt(getWidthDisplay(fr->display));
    dh = valInt(getHeightDisplay(fr->display));
  }

  if ( !ws_frame_bb(fr, &x, &y, &w0, &h0) )
    return;
  w = w0;
  h = h0;
  ew = w - valInt(fr->area->w);		/* width/height of decorations */
  eh = h - valInt(fr->area->h);

  switch(sscanf(s, "%dx%d%[+-]%d%[+-]%d", &w, &h, signx, &x, signy, &y))
  { case 2:
      w += ew;
      h += eh;
      flags |= SWP_NOMOVE;
      ok++;
      break;
    case 6:
      w += ew;
      h += eh;
      if ( signx[1] == '-' )
	x = -x;
      if ( signy[1] == '-' )
	y = -y;
      if ( signx[0] == '-' )
	x = dw - x - w;
      if ( signy[0] == '-' )
	y = dh - y - h;
      ok++;
      break;
    default:				/* [<Sign>]X<Sign>Y */
      if ( sscanf(s, "%[+-]%d%[+-]%d", signx, &x, signy, &y) != 4 )
      { signx[0] = '+';
	if ( sscanf(s, "%d%[+-]%d", &x, signy, &y) != 3 )
	  break;
      }

      DEBUG(NAME_frame,
	    Cprintf("signx = %s, x = %d, signy = %s,"
		    "y = %d, w0 = %d, h0 = %d\n",
		    signx, x, signy, y, w0, h0));

      flags |= SWP_NOSIZE;
      if ( signx[1] == '-' )
	x = -x;
      if ( signy[1] == '-' )
	y = -y;
      if ( signx[0] == '-' )
	x = dw - x - w0;
      if ( signy[0] == '-' )
	y = dh - y - h0;
      ok++;
      break;
  }

  if ( f && ok )
  { int mw = (w < MIN_VISIBLE ? MIN_VISIBLE : w);

    if ( y < 0 )			/* above the screen */
      y = 0;
    if ( y > dh-MIN_VISIBLE )		/* below the screen */
      y = dh - MIN_VISIBLE;
    if ( x+mw < MIN_VISIBLE )		/* left of the screen */
      x = MIN_VISIBLE-mw;
    if ( x > dw-MIN_VISIBLE )		/* right of the screen */
      x = dw - MIN_VISIBLE;

    SetWindowPos(f->hwnd,
		 HWND_TOP,	   /* ignored */
		 dx+x, dy+y, w, h, /* Specifies outer area (with border) */
		 flags);

    f->placed = TRUE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resize/reposition the frame according to  the   (client)  <-area. The p*
arguments are provided to allow passing   no-move/no-resize flags to the
appropriate system calls.

Used by `frame->set'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_geometry_frame(FrameObj fr, Int px, Int py, Int pw, Int ph, Monitor mon)
{ WsFrame f = fr->ws_ref;

  if ( f && f->hwnd )
  { UINT flags = SWP_NOACTIVATE|SWP_NOZORDER;
    Area a = fr->area;
    RECT rect;
    int w, h;

    rect.left   = valInt(fr->area->x);
    rect.top    = valInt(fr->area->y);
    if ( notDefault(mon) )
    { rect.left += valInt(mon->area->x);
      rect.top  += valInt(mon->area->y);
    }
    rect.right  = rect.left + valInt(fr->area->w);
    rect.bottom = rect.top  + valInt(fr->area->h);


    AdjustWindowRectEx(&rect, f->style, FALSE, f->styleex);
    if ( rect.left < 0 )
    { rect.right -= rect.left;
      rect.left = 0;
    }
    if ( rect.top < 0 )
    { rect.bottom -= rect.top;
      rect.top = 0;
    }
    w = rect.right - rect.left;
    h = rect.bottom - rect.top;

    if ( isDefault(pw) && isDefault(ph) )
      flags |= SWP_NOSIZE;
    if ( isDefault(px) && isDefault(py) )
      flags |= SWP_NOMOVE;
    else
      f->placed = TRUE;

    SetWindowPos(f->hwnd,
		 HWND_TOP,		/* ignored */
		 rect.left, rect.top, w, h,
		 flags);
  }
}


void
ws_frame_background(FrameObj fr, Any c)
{ Cprintf("ws_frame_background(%s, %s)\n", pp(fr), pp(c));
}


void
ws_border_frame(FrameObj fr, int b)
{
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
I can find this nowhere,  but   calling  SetCursor() not always actually
changes the cursor, but sometimes  makes   it  disappear. Only after the
user moves the mouse the  cursor   re-appears.  Redrawing  a little area
around the cursor `fixes' this problem.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
set_cursor_now(HWND hwnd, HCURSOR cursor)
{ RECT rect;

  if ( GetWindowRect(hwnd, &rect) )
  { POINT pt;

    ZSetCursor(cursor);
    GetCursorPos(&pt);
    rect.left = pt.x - 8 - rect.left;
    rect.top  = pt.y - 8 - rect.top;
    if ( rect.left < 0 ) rect.left = 0;
    if ( rect.top  < 0 ) rect.top  = 0;
    rect.right = rect.left + 16;
    rect.bottom= rect.top  + 16;
    InvalidateRect(hwnd, &rect, FALSE);
  }
}


void
ws_busy_cursor_frame(FrameObj fr, CursorObj c)
{ WsFrame r = fr->ws_ref;

  if ( r )
  { POINT pt;
    HWND hwnd;
    Any obj;
    int setcursor;

    GetCursorPos(&pt);
    if ( (hwnd = WindowFromPoint(pt)) &&
	 (obj = getObjectFromHWND(hwnd)) &&
	 isProperObject(obj) &&
	 hasGetMethodObject(obj, NAME_frame) &&
	 (fr == qadGetv(obj, NAME_frame, 0, NULL)) )
    { DEBUG(NAME_busyCursor, Cprintf("Setting cursor for %s\n", pp(fr)));
      setcursor = TRUE;
    } else
    { obj = NULL;
      setcursor = FALSE;
    }

    if ( isNil(c) )
    { if ( setcursor )
      { WsWindow ref;

	if ( obj && instanceOfObject(obj, ClassWindow) &&
	     (ref = ((PceWindow)obj)->ws_ref) &&
	     ref->hcursor )
	{ set_cursor_now(hwnd, ref->hcursor);
	} else
	{ SetCursorPos(pt.x, pt.y);	/* this should call a WM_SETCURSOR */
	}
      }

      r->hbusy_cursor = NULL;
    } else
    { if ( isDefault(c) )
	c = getClassVariableValueObject(fr, NAME_busyCursor);

      if ( c )
      { HCURSOR hc = (HCURSOR)getXrefObject(c, fr->display);

	r->hbusy_cursor = hc;
	if ( setcursor )
	  set_cursor_now(hwnd, hc);
      }
    }
  }
}


void
ws_set_icon_frame(FrameObj fr)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) && fr->status != NAME_unlinking )
  { HICON icon;

    if ( notNil(fr->icon_image) )
    { if ( (icon = ws_icon_from_image(fr->icon_image)) )
      { SendMessage(hwnd,
		    WM_SETICON,
		    (WPARAM)ICON_SMALL,
		    (LPARAM)icon);
	SendMessage(hwnd,
		    WM_SETICON,
		    (WPARAM)ICON_BIG,
		    (LPARAM)icon);
      }
    } else
    { SendMessage(hwnd,
		  WM_SETICON,
		  (WPARAM)ICON_SMALL,
		  (LPARAM)NULL);
    }

    if ( IsIconic(hwnd) )
      InvalidateRect(hwnd, NULL, TRUE);
  }
}


void
ws_set_icon_label_frame(FrameObj fr)
{
}


void
ws_set_icon_position_frame(FrameObj fr, int x, int y)
{
}


status
ws_get_icon_position_frame(FrameObj fr, int *x, int *y)
{ fail;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Enable/disable the windows we are modal too.   This is not really ok. If
frame fr1 is application modal and  opens fr2 which is application-modal
too, releasing fr2 will enable the other windows ...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
ws_enable_modal(FrameObj fr, BoolObj val)
{ BOOL enable = (val == ON ? TRUE : FALSE);

  if ( fr->modal == NAME_transient && notNil(fr->transient_for) )
  { HWND owner = getHwndFrame(fr->transient_for);

    DEBUG(NAME_modal, Cprintf("ws_enable_modal() %s %s\n",
			      pp(fr), enable ? "TRUE" : "FALSE"));
    EnableWindow(owner, enable);
  } else if ( fr->modal == NAME_application && notNil(fr->application) )
  { Cell cell;

    for_cell(cell, fr->application->members)
    { FrameObj appfr = cell->value;
      HWND hwnd;

      if ( appfr != fr && (hwnd = getHwndFrame(appfr)) )
	EnableWindow(hwnd, enable);
    }
  }
}


void
ws_status_frame(FrameObj fr, Name stat)
{ WsFrame f = fr->ws_ref;

  if ( stat == NAME_window || stat == NAME_fullScreen )
  { int how;

    if ( !f->placed )
    { ws_place_frame(fr);
      f->placed = TRUE;
    }

    if ( fr->kind == NAME_popup || fr->sensitive == OFF )
    { DEBUG(NAME_sensitive, Cprintf("%s: using SW_SHOWNOACTIVATE\n", pp(fr)));
      how = SW_SHOWNOACTIVATE; /*SW_SHOWNA;*/
    } else
    { if ( stat == NAME_window )
	how = SW_RESTORE;
      else
	how = SW_MAXIMIZE;
    }

    ShowWindow(f->hwnd, how);
    UpdateWindow(f->hwnd);
    ws_enable_modal(fr, OFF);
  } else
  { if ( stat == NAME_iconic )
    { ShowWindow(f->hwnd, SW_MINIMIZE);
    } else if ( stat == NAME_hidden )
    { if ( f->hwnd )
	ShowWindow(f->hwnd, SW_HIDE);
    }
    ws_enable_modal(fr, ON);
  }
}


void
ws_set_label_frame(FrameObj fr)
{ HWND hwnd = getHwndFrame(fr);

  if ( hwnd )
    SetWindowText(hwnd, nameToWC(fr->label, NULL));
}


Image
ws_image_of_frame(FrameObj fr)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) )
  { HDC hdc = GetDC(NULL);
    RECT rect;
    Image image;
    int w, h;
    HBITMAP obm, bm;
    HDC hdcimg;
    Size size = getSizeDisplay(fr->display);

    GetWindowRect(hwnd, &rect);
    if ( rect.left < 0 ) rect.left = 0;
    if ( rect.top < 0 )  rect.top  = 0;
    if ( rect.bottom > valInt(size->h) ) rect.bottom = valInt(size->h);
    if ( rect.right >  valInt(size->w) ) rect.right  = valInt(size->w);

    w = rect.right - rect.left;
    h = rect.bottom - rect.top;

    DEBUG(NAME_image, Cprintf("hdc = %d, size = %dx%d\n", (int) hdc, w, h));
    image = answerObject(ClassImage, NIL,
			 toInt(w), toInt(h), NAME_pixmap, EAV);
    assign(image, display, fr->display);
    bm = ZCreateCompatibleBitmap(hdc, w, h);
    hdcimg = CreateCompatibleDC(hdc);
    obm = SelectObject(hdcimg, bm);

    BitBlt(hdcimg, 0, 0, w, h, hdc, rect.left, rect.top, SRCCOPY);

    SelectObject(hdcimg, obm);
    ZDeleteObject(hdcimg);
    ReleaseDC(hwnd, hdc);

    registerXrefObject(image, image->display, (void *) bm);
    return image;
  }

  fail;
}


void
ws_transient_frame(FrameObj fr, FrameObj fr2)
{
}


status
ws_postscript_frame(FrameObj fr, int iscolor)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) )
  { HDC hdc = GetDC(NULL);
    RECT rect;
    int w, h;
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

    GetWindowRect(hwnd, &rect);
    w = rect.right - rect.left;
    h = rect.bottom - rect.top;

    ps_output("0 0 ~D ~D ~D ~N\n", w, h,
	      depth,
	      iscolor ? NAME_rgbimage : NAME_greymap);
    postscriptDC(hdc, rect.left, rect.top, w, h, depth, iscolor);
    ps_output("\n");

    succeed;
  } else
    return errorPce(fr, NAME_mustBeOpenBeforePostscript);
}


void
ws_frame_cursor(FrameObj fr, CursorObj cursor)
{ WsFrame f = fr->ws_ref;

  if ( f )
  { if ( isDefault(cursor) )
      f->hcursor = LoadCursor(NULL, IDC_ARROW);
    else
      f->hcursor = (HCURSOR)getXrefObject(cursor, fr->display);

    ZSetCursor(f->hcursor);
  }
}


void
ws_grab_frame_pointer(FrameObj fr, BoolObj grab, CursorObj cursor)
{ HWND win;

  if ( (win = getHwndFrame(fr)) )
  { if ( grab == ON )
    { ws_frame_cursor(fr, cursor);
      SetCapture(win);
    } else
      ReleaseCapture();
  }
}


Int
ws_frame_thread(FrameObj fr)
{ HWND hwnd;

  if ( (hwnd = getHwndFrame(fr)) )
  { DWORD owner = GetWindowThreadProcessId(hwnd, NULL);

    return toInt(owner);
  }

  fail;
}


