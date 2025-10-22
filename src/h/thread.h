/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

#ifndef PCE_THREAD_INCLUDED
#define PCE_THREAD_INCLUDED 1

typedef struct
{ PceGoal		current_goal;	  /* See passing.c */
  VarEnvironment	var_environment;  /* see var.c */
  int			service_mode;	  /* PCE_EXEC_* */
  int			max_goal_depth;	  /* Max recursion level */
  int			bind_nesting;	  /* Nesting in message resolving */
  struct to_cell	answer_stack_base_cell; /* AnswerStack */
  ToCell		answer_stack;
} thread_data, *ThreadData;

#ifndef GLOBAL
__thread ThreadData pce_thread_data = NULL;
#else
GLOBAL __thread ThreadData pce_thread_data;
#endif

COMMON(ThreadData)  createPceThreadData(void);
COMMON(void)        destroyPceThreadData(void);

static inline ThreadData
TheThreadData(void)
{ if ( pce_thread_data )
    return pce_thread_data;
  return createPceThreadData();
}

#define varEnvironment      (TheThreadData()->var_environment)
#define CurrentGoal         (TheThreadData()->current_goal)
#define TheServiceMode      (TheThreadData()->service_mode)
#define MaxGoalDepth        (TheThreadData()->max_goal_depth)
#define BindNesting         (TheThreadData()->bind_nesting)
#define AnswerStackBaseCell (TheThreadData()->answer_stack_base_cell)
#define AnswerStack	    (TheThreadData()->answer_stack)

#endif /*PCE_THREAD_INCLUDED*/
