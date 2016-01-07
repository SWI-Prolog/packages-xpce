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

extern "C" {
					/* C ---> XPCE */

void *	XPCE_to_string(char *text);
void *	XPCE_to_name(char *text);
void *	XPCE_to_integer(long value);
void *	XPCE_to_real(float value);
void *	XPCE_to_object(void *name);
void *	XPCE_to_class(void *name);

					/* XPCE ---> C */

char *	XPCE_charp_of(void *string);
long	XPCE_int_of(void *integer);
float	XPCE_float_of(void *real);

typedef int		XPCE_status;

					/* VMI */
int	XPCE_sendv(void * receiver, void * selector,
		   int argc, const void * argv[]);
void *	XPCE_getv(void * receiver, void * selector,
		  int argc, const void * argv[]);
void *	XPCE_newv(void * cl, const char *name,
		  int argc, const void * argv[]);

int	XPCE_free(void *);
}

class PceBase;

class PceBase
{
protected:
  void*		self;
};

class P :public PceBase;
class XPCE_Object :public PceBase;

#ifndef NULL				/* TBD */
#define NULL ((void *)0)
#endif

class P :public PceBase
{
public:

  P(char *text)
  { self = XPCE_to_name(text);
  }
  P(int i)
  { self =  XPCE_to_integer((long) i);
  }
  P(short i)
  { self = XPCE_to_integer((long) i);
  }
  P(long i)
  { self = XPCE_to_integer((long) i);
  }
  P(float f)
  { self = XPCE_to_real(f);
  }
  P(double f)
  { self = XPCE_to_real((float) f);
  }
  P(const PceBase &obj)
  { self = obj.self;
  }
  P(P &p)
  { self = p.self;
  }
  P& operator=(const P&q)
  { self = q.self;
    return *this;
  }
};


class XPCE_Object :public PceBase
{
private:
  XPCE_Object(void *p)
  { self = p;
  }

public:
					/* NEW */
  XPCE_Object(P cl)
    { self = XPCE_newv(cl.self, NULL, 0, NULL);
    }
  XPCE_Object(P cl, P a1)
    { self = XPCE_newv(cl.self, NULL, 1, &a1.self);
    }
  XPCE_Object(P cl, P a1, P a2)
    { void *av[2] = { a1.self, a2.self };
      self = XPCE_newv(cl.self, NULL, 2, av);
    }

  XPCE_status send(P sel)
    { return XPCE_sendv(self, sel.self, 0, NULL);
    }
  XPCE_status send(P sel, P a1)
    { return XPCE_sendv(self, sel.self, 1, &a1.self);
    }
  XPCE_status send(P sel, P a1, P a2)
    { void * av[2] = { a1.self, a2.self };
      return XPCE_sendv(self, sel.self, 2, av);
    }
  XPCE_status send(P sel, P a1, P a2, P a3)
    { void * av[3] = { a1.self, a2.self, a3.self };
      return XPCE_sendv(self, sel.self, 3, av);
    }
					/* GET */
  XPCE_Object get(P sel)
    { return XPCE_Object(XPCE_getv(self, sel.self, 0, NULL));
    }
  XPCE_Object get(P sel, P a1)
    { return XPCE_getv(self, sel.self, 1, &a1.self);
    }
  XPCE_Object get(P sel, P a1, P a2)
    { void * av[2] = { a1.self, a2.self };
      return XPCE_getv(self, sel.self, 2, av);
    }
  XPCE_Object get(P sel, P a1, P a2, P a3)
    { void * av[3] = { a1.self, a2.self, a3.self };
      return XPCE_getv(self, sel.self, 3, av);
    }
};
