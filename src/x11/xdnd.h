/*
   xdnd.c, xdnd.h - C program library for handling the Xdnd protocol

   Copyright (C) 1998  Paul Sheer

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02111 USA.

   http://www.cco.caltech.edu/~jafl/xdnd/

   Further info can also be obtained by emailing the author at,
       psheer@obsidian.co.za

   Released 1998-08-07
*/

#ifndef _X_DND_H
#define _X_DND_H

#ifdef  __cplusplus
extern "C" {
#endif

/* you can set this to either 2 (which support 0 and 1 as well) or 3 */
/* #define XDND_VERSION 2 */
#define XDND_VERSION 3


/* XdndEnter */
#define XDND_THREE 3
#define XDND_ENTER_SOURCE_WIN(e)	((e)->xclient.data.l[0])
#define XDND_ENTER_THREE_TYPES(e)	(((e)->xclient.data.l[1] & 0x1UL) == 0)
#define XDND_ENTER_THREE_TYPES_SET(e,b)	(e)->xclient.data.l[1] = ((e)->xclient.data.l[1] & ~0x1UL) | (((b) == 0) ? 0 : 0x1UL)
#define XDND_ENTER_VERSION(e)		((e)->xclient.data.l[1] >> 24)
#define XDND_ENTER_VERSION_SET(e,v)	(e)->xclient.data.l[1] = ((e)->xclient.data.l[1] & ~(0xFF << 24)) | ((v) << 24)
#define XDND_ENTER_TYPE(e,i)		((e)->xclient.data.l[2 + i])	/* i => (0, 1, 2) */

/* XdndPosition */
#define XDND_POSITION_SOURCE_WIN(e)	((e)->xclient.data.l[0])
#define XDND_POSITION_ROOT_X(e)		((e)->xclient.data.l[2] >> 16)
#define XDND_POSITION_ROOT_Y(e)		((e)->xclient.data.l[2] & 0xFFFFUL)
#define XDND_POSITION_ROOT_SET(e,x,y)	(e)->xclient.data.l[2]  = ((x) << 16) | ((y) & 0xFFFFUL)
#define XDND_POSITION_TIME(e)		((e)->xclient.data.l[3])
#define XDND_POSITION_ACTION(e)		((e)->xclient.data.l[4])

/* XdndStatus */
#define XDND_STATUS_TARGET_WIN(e)	((e)->xclient.data.l[0])
#define XDND_STATUS_WILL_ACCEPT(e)	((e)->xclient.data.l[1] & 0x1L)
#define XDND_STATUS_WILL_ACCEPT_SET(e,b) (e)->xclient.data.l[1] = ((e)->xclient.data.l[1] & ~0x1UL) | (((b) == 0) ? 0 : 0x1UL)
#define XDND_STATUS_WANT_POSITION(e)	((e)->xclient.data.l[1] & 0x2UL)
#define XDND_STATUS_WANT_POSITION_SET(e,b) (e)->xclient.data.l[1] = ((e)->xclient.data.l[1] & ~0x2UL) | (((b) == 0) ? 0 : 0x2UL)
#define XDND_STATUS_RECT_X(e)		((e)->xclient.data.l[2] >> 16)
#define XDND_STATUS_RECT_Y(e)		((e)->xclient.data.l[2] & 0xFFFFL)
#define XDND_STATUS_RECT_WIDTH(e)	((e)->xclient.data.l[3] >> 16)
#define XDND_STATUS_RECT_HEIGHT(e)	((e)->xclient.data.l[3] & 0xFFFFL)
#define XDND_STATUS_RECT_SET(e,x,y,w,h)	{(e)->xclient.data.l[2] = ((x) << 16) | ((y) & 0xFFFFUL); (e)->xclient.data.l[3] = ((w) << 16) | ((h) & 0xFFFFUL); }
#define XDND_STATUS_ACTION(e)		((e)->xclient.data.l[4])

/* XdndLeave */
#define XDND_LEAVE_SOURCE_WIN(e)	((e)->xclient.data.l[0])

/* XdndDrop */
#define XDND_DROP_SOURCE_WIN(e)		((e)->xclient.data.l[0])
#define XDND_DROP_TIME(e)		((e)->xclient.data.l[2])

/* XdndFinished */
#define XDND_FINISHED_TARGET_WIN(e)	((e)->xclient.data.l[0])

struct _DndCursor {
    int width, height;
    int x, y;
    unsigned char *image_data, *mask_data;
    char *_action;
    Pixmap image_pixmap, mask_pixmap;
    Cursor cursor;
    Atom action;
};

typedef struct _DndCursor DndCursor;
typedef struct _DndClass DndClass;

struct _DndClass {
/* insert chars sequentionally into the target widget, type will be the same as `desired_type'
   returned from widget_apply_position. This may be called several times in succession
   with sequention blocks of data. Must return non-zero on failure */
    int (*widget_insert_drop) (DndClass * dnd, unsigned char *data, int length, int remaining, Window into, Window from, Atom type);

/* In response to DELETE requests : FIXME - not yet used */
    int (*widget_delete_selection) (DndClass * dnd, Window window, Window from);

/* returns 1 if widget exists, zero otherwise. If this method is not
   set then the code assumes that no widgets have support for recieving drops.
   In this case none of the widget methods need be set. */
    int (*widget_exists) (DndClass * dnd, Window window);

/* must update the widgets border to its default appearance */
    void (*widget_apply_leave) (DndClass * dnd, Window widgets_window);

/* must update the widgets border to give the appearance of being able to recieve a drop,
   plus return all data to pointers. As per the protocol, if the widget cannot
   perform the action specified by `action' then it should return either XdndActionPrivate
   or XdndActionCopy into supported_action (leaving 0 supported_action unchanged is equivalent
   to XdndActionCopy). Returns 1 if ready to ok drop */
    int (*widget_apply_position) (DndClass * dnd, Window widgets_window, Window from,
		      Atom action, int x, int y, Time t, Atom * typelist,
	int *want_position, Atom * supported_action, Atom * desired_type,
				  XRectangle * rectangle);

/* returns drag data of the specified type. This will be one of `typelist' given to xdnd_drag */
    void (*widget_get_data) (DndClass * dnd, Window window, unsigned char **data, int *length, Atom type);

/* this is called from with the main event loop if an expose event is recieved and is optional */
    void (*handle_expose_events) (DndClass * dnd, XEvent * xevent);

/* creates a chooser dialog if the action is XdndActionAsk. Returns non-zero on cancel */
    int (*action_choose_dialog) (DndClass * dnd, char **descriptions, Atom * actions, Atom * result);

    void *pad1[8];

    DndCursor *cursors;

    Display *display;

    Atom XdndAware;
    Atom XdndSelection;
    Atom XdndEnter;
    Atom XdndLeave;
    Atom XdndPosition;
    Atom XdndDrop;
    Atom XdndFinished;
    Atom XdndStatus;
    Atom XdndActionCopy;
    Atom XdndActionMove;
    Atom XdndActionLink;
    Atom XdndActionAsk;
    Atom XdndActionPrivate;
    Atom XdndTypeList;
    Atom XdndActionList;
    Atom XdndActionDescription;

    Atom Xdnd_NON_PROTOCOL_ATOM;
    Atom version;

    Atom pad2[16];

    Window root_window;

#define XDND_DROP_STAGE_IDLE		0
#define XDND_DRAG_STAGE_DRAGGING	1
#define XDND_DRAG_STAGE_ENTERED		2
#define XDND_DROP_STAGE_CONVERTING	3
#define XDND_DROP_STAGE_ENTERED		4
    int stage;
    int dragging_version;
    int internal_drag;
    int want_position;
    int ready_to_drop;
    int will_accept;
    XRectangle rectangle;
    Window dropper_window, dragger_window;
    Atom *dragger_typelist;
    Atom desired_type;
    Atom supported_action;
    Time time;
/* drop position from last XdndPosition */
    int x, y;
    int pad3[16];

/* move euclidian pixels before considering this to be an actual drag */
    float drag_threshold;

/* block for only this many seconds on not receiving a XdndFinished from target, default : 10 */
    int time_out;

#define XDND_OPTION_NO_HYSTERESIS (1<<0)
    int options;

/* user hooks */
    void *user_hook1;
    void *user_hook2;
    void *user_hook3;
    void *pad4[16];
};

void xdnd_init (DndClass * dnd, Display * display);
void xdnd_shut (DndClass *dnd);
void xdnd_set_dnd_aware (DndClass * dnd, Window window, Atom * typelist);
int xdnd_is_dnd_aware (DndClass * dnd, Window window, int *version, Atom * typelist);
void xdnd_set_type_list (DndClass * dnd, Window window, Atom * typelist);
void xdnd_set_actions (DndClass * dnd, Window window, Atom * actions, char **descriptions);
int xdnd_get_actions (DndClass * dnd, Window window, Atom ** actions, char ***descriptions);
int xdnd_choose_action_dialog (DndClass * dnd, Atom * actions, char **descriptions, Atom * result);
void xdnd_send_enter (DndClass * dnd, Window window, Window from, Atom * typelist);
void xdnd_send_position (DndClass * dnd, Window window, Window from, Atom action, int x, int y, unsigned long etime);
void xdnd_send_status (DndClass * dnd, Window window, Window from, int will_accept,
	     int want_position, int x, int y, int w, int h, Atom action);
void xdnd_send_leave (DndClass * dnd, Window window, Window from);
void xdnd_send_drop (DndClass * dnd, Window window, Window from, unsigned long etime);
void xdnd_send_finished (DndClass * dnd, Window window, Window from, int error);
int xdnd_convert_selection (DndClass * dnd, Window window, Window requester, Atom type);
void xdnd_selection_send (DndClass * dnd, XSelectionRequestEvent * request, unsigned char *data, int length);
int xdnd_get_selection (DndClass * dnd, Window from, Atom property, Window insert);
Atom xdnd_drag (DndClass * dnd, Window from, Atom action, Atom * typelist);

/* Returns 1 if event is handled, This must be placed in the widget libraries main event
   loop and be called if the event type is ClientMessage or SelectionNotify */
int xdnd_handle_drop_events (DndClass * dnd, XEvent * xevent);
Atom xdnd_get_drop (Display * display, XEvent * xevent, Atom * typelist, Atom * actionlist,
	  unsigned char **data, int *length, Atom * type, int *x, int *y);

/* ADDED (JW) */
DndClass * xdnd_default_dnd(Display * display);

#ifdef  __cplusplus
}
#endif

#endif	/* !_X_DND_H */


