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

#define INLINE_UTILITIES 1
#include <h/kernel.h>


static status
initialiseMessagev(Message msg, Any rec, Name sel, int argc, Any *argv)
{ assign(msg, receiver,  rec);
  assign(msg, selector,  sel);
  assign(msg, arg_count, toInt(argc));

  switch(argc)
  { case 0:
      break;
    case 1:
      assign(msg, arguments, argv[0]);
      break;
    default:
      assign(msg, arguments, newObjectv(ClassCodeVector, argc, argv));
      break;
  }

  if ( TheCallbackFunctions.getHostContext )
  { Any context = (*TheCallbackFunctions.getHostContext)(rec);

    assign(msg, context, context);
  }

  return initialiseCode((Code) msg);
}


static Int
getArityMessage(Message msg)
{ answer(add(TWO, msg->arg_count));
}


static Any
getArgMessage(Message msg, Int arg)
{ int n = valInt(arg);

  switch(n)
  { case 1:	answer(msg->receiver);
    case 2:	answer((Any) msg->selector);
    default:	if ( n < 1 || n > valInt(getArityMessage(msg)) )
		  fail;
    		if ( msg->arg_count == ONE )
		  answer(msg->arguments);
		else
		  answer(msg->arguments->elements[n-3]);
  }
}


static status
argumentMessage(Message msg, Int n, Any val)
{ int i = valInt(n);

  if ( i < 1 || i > valInt(getArityMessage(msg)) )
    fail;

  if ( msg->arg_count == ONE )
    assign(msg, arguments, val);

  return elementVector(msg->arguments, n, val);
}


static Any
getArgumentMessage(Message msg, Int n)
{ int i = valInt(n);

  if ( i < 1 || i > valInt(getArityMessage(msg)) )
    fail;

  if ( msg->arg_count == ONE )
    answer(msg->arguments);

   answer(msg->arguments->elements[i-1]);
}


status
ExecuteMessage(Message msg)
{ Any receiver;
  Any selector;
  Any savedcontext;
  status rval = FAIL;

  if ( notNil(msg->context) &&
       TheCallbackFunctions.setHostContext )
  { savedcontext =
	(*TheCallbackFunctions.setHostContext)(msg->context);
  } else
    savedcontext = NIL;

  if ( !(receiver = expandCodeArgument(msg->receiver)) )
    goto out;
  if ( !(selector = checkSelector(msg->selector)) )
    goto out;

  if ( msg->arg_count == ZERO )
  { return sendv(receiver, selector, 0, NULL);
  } else if ( msg->arg_count == ONE )
  { Any arg;

    if ( !(arg = expandCodeArgument(msg->arguments)) )
      goto out;

    rval = sendv(receiver, selector, 1, &arg);
  } else
  { int n;
    int argc = valInt(msg->arguments->size);
    ArgVector(argv, argc);
    Any *elms = msg->arguments->elements;
    Any *av = argv;

    for(n = argc; --n >=0 ; )
    { if ( !(*av++ = expandCodeArgument(*elms++)) )
	goto out;
    }

    rval = sendv(receiver, selector, argc, argv);
  }

out:
  if ( notNil(savedcontext) )
    (*TheCallbackFunctions.setHostContext)(savedcontext);

  return rval;
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declarations */

static char *T_argument[] =
        { "index=int", "value=any|function" };
static char *T_initialise[] =
        { "receiver=object|function", "selector=name|function",
	  "argument=any|function ..." };

/* Instance Variables */

static vardecl var_message[] =
{ IV(NAME_receiver, "object|function", IV_BOTH,
     NAME_storage, "Receiver of the operation"),
  IV(NAME_selector, "name|function", IV_BOTH,
     NAME_storage, "Name of the operation"),
  IV(NAME_argCount, "0..", IV_GET,
     NAME_storage, "Argument-count"),
  IV(NAME_arguments, "code_vector|any|function*", IV_GET,
     NAME_storage, "Vector of arguments"),
  IV(NAME_context, "any*", IV_GET,
     NAME_storage, "Host context information")
};

/* Send Methods */

static senddecl send_message[] =
{ SM(NAME_Execute, 0, NULL, ExecuteMessage,
     DEFAULT, "Send the message"),
  SM(NAME_initialise, 3, T_initialise, initialiseMessagev,
     DEFAULT, "Create from receiver, selector and args"),
  SM(NAME_argument, 2, T_argument, argumentMessage,
     NAME_argument, "Set nth-1 argument")
};

/* Get Methods */

static getdecl get_message[] =
{ GM(NAME_Arg, 1, "any|function", "int", getArgMessage,
     DEFAULT, "Nth-1 argument for term description"),
  GM(NAME_Arity, 0, "int", NULL, getArityMessage,
     DEFAULT, "Arity for term description"),
  GM(NAME_argument, 1, "value=any|function", "index=int", getArgumentMessage,
     NAME_argument, "Nth-1 argument")
};

/* Resources */

#define rc_message NULL
/*
static classvardecl rc_message[] =
{
};
*/

/* Class Declaration */

ClassDecl(message_decls,
          var_message, send_message, get_message, rc_message,
          ARGC_UNKNOWN, NULL,
          "$Rev$");

status
makeClassMessage(Class class)
{ return declareClass(class, &message_decls);
}
