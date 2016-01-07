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

#ifndef PCE_TRACE_H
#define PCE_TRACE_H

GLOBAL PceGoal	CurrentGoal;		/* current active goal */
GLOBAL int	ServiceMode;		/* Running a service  call-back */
GLOBAL int	MaxGoalDepth;		/* maximum recursion level */

#define NO_MAX_GOAL_DEPTH INT_MAX 	/* Setting for MaxGoalDepth if unlimited */

#define DebuggingProgramObject(o, flags) \
	(PCEdebugging && (ServiceMode == PCE_EXEC_USER) && onDFlag((o), (flags)))

#define ServiceMode(mode, code) \
  { int _smode = ServiceMode; \
    ServiceMode = mode; \
    { code; } \
    ServiceMode = _smode; \
  }

COMMON(void)	writeGoal(PceGoal g);
COMMON(int)	isProperGoal(PceGoal g);
COMMON(void)	pceBackTrace(PceGoal g, int depth);

#endif /*PCE_TRACE_H*/
