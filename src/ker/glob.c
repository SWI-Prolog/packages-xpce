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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#define GLOBAL SO_LOCAL
#if defined(__WINDOWS__) || defined(__WINDOWS__)
#define PUBLIC_GLOBAL __declspec(dllexport)
#else
#define PUBLIC_GLOBAL
#endif

#include <h/kernel.h>
#include <h/trace.h>
#include <h/interface.h>
#include <h/arith.h>
#include <h/dialog.h>
#include <h/lang.h>
#include <ker/alloc.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Oops. The MacOS X (darwin) linker doesn't get a .o file for a library if
there are only common symbols in there. So,   we add a function and call
it from ker/self.c to force loading this file.

Looks like a bug to me, but   this  work-around will never cause trouble
and isn't too ugly.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef __APPLE__
int
IAmAGlobalFunctionToMakeMeLoad()
{ return 42;
}
#endif

