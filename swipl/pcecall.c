/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2011-2025, University of Amsterdam
                              VU University Amsterdam
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <assert.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <h/interface.h>

#include "pcecall.h"


		 /*******************************
		 *	       TYPES		*
		 *******************************/

typedef enum goal_state
{ G_WAITING,
  G_RUNNING,
  G_TRUE,
  G_FALSE,
  G_ERROR
} goal_state;

typedef struct
{ module_t	module;			/* module to call in */
  record_t	goal;			/* the term to call */
  record_t	result;			/* exception/variables */
  bool		acknowledge;		/* If set, wait ( */
  goal_state	state;			/* G_* */
} prolog_goal;


#ifdef O_PLMT
static int init_prolog_goal(prolog_goal *g, term_t goal, int acknowledge);
static void call_prolog_goal(prolog_goal *g);
#endif

#if SDL_GRAPHICS

static foreign_t
in_pce_thread(term_t goal)
{ prolog_goal *g = malloc(sizeof(*g));

  if ( !g )
    return PL_resource_error("memory");

  if ( !init_prolog_goal(g, goal, FALSE) )
  { free(g);
    return FALSE;
  }

  SDL_Event event = {0};
  event.type = MY_EVENT_CALL;
  event.user.code = CALL_MAGIC;
  event.user.data1 = g;
  SDL_PushEvent(&event);

  return true;
}

bool
sdl_call_event(SDL_Event *event)
{ if ( event->type == MY_EVENT_CALL )
  { assert(event->user.code == CALL_MAGIC);
    prolog_goal *g = (prolog_goal *)event->user.data1;

    call_prolog_goal(g);
    if ( !g->acknowledge )
      free(g);

    return true;
  }

  return false;
}


static foreign_t
in_pce_thread_sync2(term_t goal, term_t vars)
{ prolog_goal *g = malloc(sizeof(*g));

  if ( !g )
    return PL_resource_error("memory");

  if ( !init_prolog_goal(g, goal, TRUE) )
  { free(g);
    return FALSE;
  }

  SDL_Event event = {0};
  event.type = MY_EVENT_CALL;
  event.user.code = CALL_MAGIC;
  event.user.data1 = g;
  SDL_PushEvent(&event);

  bool rc = false;
  while(true)
  { pceDispatch(-1, 250);
    if ( PL_handle_signals() < 0 )
      return false;

    switch(g->state)
    { case G_TRUE:
      { term_t v = PL_new_term_ref();

	rc = PL_recorded(g->result, v) && PL_unify(vars, v);
	PL_erase(g->result);
        goto out;
      }
      case G_FALSE:
	goto out;
      case G_ERROR:
      { term_t ex = PL_new_term_ref();

	if ( PL_recorded(g->result, ex) )
	  rc = PL_raise_exception(ex);
	PL_erase(g->result);
	goto out;
      }
      default:
	continue;
    }
  }

out:
  free(g);
  return rc;
}

#endif

		 /*******************************
		 *	CREATE/EXECUTE GOAL	*
		 *******************************/

#if O_PLMT
static int
init_prolog_goal(prolog_goal *g, term_t goal, int acknowledge)
{ term_t plain = PL_new_term_ref();

  g->module	 = NULL;
  g->acknowledge = acknowledge;
  g->state       = G_WAITING;
  if ( !PL_strip_module(goal, &g->module, plain) )
    return FALSE;
  if ( !(PL_is_compound(plain) || PL_is_atom(plain)) )
    return PL_type_error("callable", goal);
  g->goal = PL_record(plain);

  return TRUE;
}


static void
call_prolog_goal(prolog_goal *g)
{ fid_t fid;
  static predicate_t pred = NULL;
  int rc;

  if ( !pred )
    pred = PL_predicate("call", 1, "user");

  if ( (fid = PL_open_foreign_frame()) )
  { term_t t = PL_new_term_ref();
    term_t vars;
    rc = PL_recorded(g->goal, t);
    PL_erase(g->goal);
    g->goal = 0;
    g->state = G_RUNNING;
    if ( rc )
    { qid_t qid;
      int flags = PL_Q_NORMAL;

      if ( g->acknowledge )
      { flags |= PL_Q_CATCH_EXCEPTION;
	vars = PL_new_term_ref();
	if ( !PL_get_arg(2, t, vars) ||		/* Goal-Vars */
	     !PL_get_arg(1, t, t) )
	{ PL_warning("ERROR: in_pce_thread: bad goal-vars term");
	}
      } else
      { vars = 0;
      }

      if ( (qid = PL_open_query(g->module, flags, pred, t)) )
      { rc = PL_next_solution(qid);

	if ( rc )
	{ g->state = G_TRUE;
	  if ( vars )
	    g->result = PL_record(vars);
	} else
	{ term_t ex;

	  if ( g->acknowledge && (ex=PL_exception(qid)) )
	  { g->result = PL_record(ex);
	    g->state = G_ERROR;
	  } else
	  { g->state = G_FALSE;
	  }
	}

	PL_cut_query(qid);
      } else
	PL_warning("ERROR: pce: out of global stack");
    }
    PL_discard_foreign_frame(fid);
  } else
    PL_warning("ERROR: pce: out of global stack");
}
#endif

static int sdl_thread = 0;

static foreign_t
set_pce_thread(void)
{ int tid = PL_thread_self();

  if ( sdl_thread && tid != sdl_thread )
  { term_t culprit = PL_new_term_ref();
    return ( PL_unify_term(culprit, PL_FUNCTOR_CHARS, "@", 1,
				      PL_CHARS, "pce") &&
	     PL_permission_error("modify", "pce_thread", culprit) );
    sdl_thread = tid;
  }

  return setPceThread();
}


static foreign_t
pl_pce_dispatch(void)
{ pceDispatch(-1, 250);

  if ( PL_handle_signals() == -1 || PL_exception(0) )
    return FALSE;

  return TRUE;
}



		 /*******************************
		 *	       INSTALL		*
		 *******************************/

install_t
install_pcecall(void)
{ PL_register_foreign("in_pce_thread",      1,
		      in_pce_thread, PL_FA_META, "0");
  PL_register_foreign("in_pce_thread_sync2", 2, in_pce_thread_sync2, 0);
  PL_register_foreign("set_pce_thread",      0, set_pce_thread,      0);
  PL_register_foreign("pce_dispatch",        0, pl_pce_dispatch,     0);
}
