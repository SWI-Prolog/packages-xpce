/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

#ifndef _FrameP_h
#define _FrameP_h

#include "fshell.h"
/* include superclass private header file */
#include <X11/ShellP.h>

/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRFrameResource "FrameResource"

		/********************************
		*           TOPLEVEL		*
		********************************/

typedef struct
{ XtPointer extension;
} TopLevelFrameClassPart;

typedef struct _TopLevelFrameClassRec
{ CoreClassPart			core_class;
  CompositeClassPart		composite_class;
  ShellClassPart		shell_class;
  WMShellClassPart		wm_shell_class;
  VendorShellClassPart		vendor_shell_class;
  TopLevelShellClassPart	top_level_shell_class;
  TopLevelFrameClassPart	top_level_frame_class;
} TopLevelFrameClassRec;

extern TopLevelFrameClassRec topLevelFrameClassRec;

typedef struct
{   /* resources */
  XtCallbackList		event_callback;
  XtCallbackList		expose_callback;
    /* private state */
    /* (none) */
} TopLevelFramePart;

typedef struct _TopLevelFrameRec
{ CorePart	 		core;
  CompositePart 		composite;
  ShellPart 			shell;
  WMShellPart			wm;
  VendorShellPart		vendor;
  TopLevelShellPart		top_level_shell;
  TopLevelFramePart		top_level_frame;
} TopLevelFrameRec;

		/********************************
		*          OVERRIDE		*
		********************************/

typedef struct
{ XtPointer extension;
} OverrideFrameClassPart;

typedef struct _OverrideFrameClassRec
{ CoreClassPart			core_class;
  CompositeClassPart		composite_class;
  ShellClassPart		shell_class;
  OverrideShellClassPart	override_shell_class;
  OverrideFrameClassPart	override_frame_class;
} OverrideFrameClassRec;

extern OverrideFrameClassRec overrideFrameClassRec;

typedef struct
{   /* resources */
  XtCallbackList		event_callback;
  XtCallbackList		expose_callback;
    /* private state */
    /* (none) */
} OverrideFramePart;

typedef struct _OverrideFrameRec
{ CorePart 			core;
  CompositePart 		composite;
  ShellPart 			shell;
  TopLevelShellPart		override_shell;
  OverrideFramePart		override_frame;
} OverrideFrameRec;


		/********************************
		*          TRANSIENT		*
		********************************/

typedef struct
{ XtPointer extension;
} TransientFrameClassPart;

typedef struct _TransientFrameClassRec
{ CoreClassPart			core_class;
  CompositeClassPart		composite_class;
  ShellClassPart		shell_class;
  WMShellClassPart		wm_shell_class;
  VendorShellClassPart		vendor_shell_class;
  TransientShellClassPart	transient_shell_class;
  TransientFrameClassPart	transient_frame_class;
} TransientFrameClassRec;

extern TransientFrameClassRec transientFrameClassRec;

typedef struct
{   /* resources */
  XtCallbackList		event_callback;
  XtCallbackList		expose_callback;
    /* private state */
    /* (none) */
} TransientFramePart;

typedef struct _TransientFrameRec
{ CorePart 			core;
  CompositePart 		composite;
  ShellPart 			shell;
  WMShellPart			wm;
  VendorShellPart		vendor;
  TransientShellPart		transient;
  TransientFramePart		transient_frame;
} TransientFrameRec;

#endif /* _FrameP_h */

