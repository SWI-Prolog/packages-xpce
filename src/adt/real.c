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

#include <h/kernel.h>
#include <h/unix.h>
#include <math.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
New reals  can be created  using newObject(ClassReal, value,  EAV).  The
argument should be a proper PCE data type, which is converted into a C
double by initialiseReal.  When a Real should be created from a C double
use the function CtoReal().
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
setReal(Real r, double f)
{
#if REAL_IN_ONE
  r->value = f;
#else
  union
  { double f;
    unsigned long l[2];
  } v;

  v.f = f;
  r->value1 = v.l[0];
  r->value2 = v.l[1];
#endif
}


double
valReal(Real r)
{
#if REAL_IN_ONE
  return r->value;
#else
  union
  { double f;
    unsigned long l[2];
  } v;

  v.l[0] = r->value1;
  v.l[1] = r->value2;

  return v.f;
#endif
}


Real
CtoReal(double f)
{ Real r = answerObject(ClassReal, ZERO, EAV);
  setReal(r, f);

  return r;
}


static status
initialiseReal(Real r, Any arg)
{ double v;

  setFlag(r, F_ISREAL);

  if ( isInteger(arg) )
  { v = valNum(arg);
  } else if ( instanceOfObject(arg, ClassNumber) )
  { v = (double)((Number)arg)->value;
  } else if ( instanceOfObject(arg, ClassReal) )
  { Real a = arg;

    return valueReal(r, a);
  } else
    return errorPce(ClassReal, NAME_cannotConvert, arg);

  setReal(r, v);

  succeed;
}


Real
getConvertReal(Class class, Any obj)
{ char *s;

  if ( isInteger(obj) || instanceOfObject(obj, ClassNumber) )
    answer(answerObjectv(ClassReal, 1, &obj));
  else if ( (s = toCharp(obj)) && s[0] != EOS )
  { char *end, *es = s + strlen(s);
    double f;

    f = cstrtod(s, &end);
    if ( end == es )
      return CtoReal(f);
    f = strtod(s, &end);
    if ( end == es )
      return CtoReal(f);
  }

  fail;
}


static StringObj
getPrintNameReal(Real r)
{ char buf[100];

  sprintf(buf, "%g", valReal(r));

  answer(CtoString(buf));
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Store/load reals to/from file. Format:

<real>		::= <word>		(first part of double)
		    <word>		(second part of double)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static status
storeReal(Real r, FileObj file)
{ TRY(storeSlotsObject(r, file));

  return storeDoubleFile(file, valReal(r));
}


static status
loadReal(Real r, IOSTREAM *fd, ClassDef def)
{ TRY(loadSlotsObject(r, fd, def));

  setFlag(r, F_ISREAL);

  if ( restoreVersion < 16 )		/* saved as single */
  { union { long l; float f; } u;

    u.l = loadWord(fd);
    setReal(r, (double)u.f);
  } else
  { setReal(r, loadDouble(fd));
  }
  succeed;
}


static status
cloneReal(Real r1, Real r2)
{ clonePceSlots(r1, r2);
  setFlag(r2, F_ISREAL);

#if REAL_IN_ONE
  r2->value = r1->value;
#else
  r2->value1 = r1->value1;
  r2->value2 = r1->value2;
#endif

  succeed;
}


static status
equalReal(Real r, Real r2)
{
#ifdef REAL_IN_ONE
  if ( r->value == r2->value )
#else
  if ( r->value1 == r2->value1 &&
       r->value2 == r2->value2 )
#endif
    succeed;
  fail;
}


static status
notEqualReal(Real r, Real r2)
{
#ifdef REAL_IN_ONE
  if ( r->value == r2->value )
#else
  if ( r->value1 == r2->value1 &&
       r->value2 == r2->value2 )
#endif
    fail;
  succeed;
}


static status
smallerReal(Real r, Real r2)
{ if (valReal(r) < valReal(r2))
    succeed;
  fail;
}


static status
largerReal(Real r, Real r2)
{ if (valReal(r) > valReal(r2))
    succeed;
  fail;
}


static status
lessEqualReal(Real r, Real r2)
{ if (valReal(r) <= valReal(r2))
    succeed;
  fail;
}


static status
largerEqualReal(Real r, Real r2)
{ if (valReal(r) >= valReal(r2))
    succeed;
  fail;
}


static status
plusReal(Real r, Real r2)
{ setReal(r, valReal(r) + valReal(r2));
  succeed;
}


static status
minusReal(Real r, Real r2)
{ setReal(r, valReal(r) - valReal(r2));
  succeed;
}


static status
timesReal(Real r, Real r2)
{ setReal(r, valReal(r) * valReal(r2));
  succeed;
}


static status
divideReal(Real r, Real r2)
{ setReal(r, valReal(r) / valReal(r2));
  succeed;
}


static Name
getCompareReal(Real r1, Real r2)
{ if ( valReal(r1) > valReal(r2) )
    answer(NAME_larger);
  if ( valReal(r1) < valReal(r2) )
    answer(NAME_smaller);
  answer(NAME_equal);
}


static Real
getCatchAllReal(Real r, Name selector, int argc, Any *argv)
{ Real result = answerObject(classOfObject(r), r, EAV);

  TRY(sendv(result, selector, argc, argv));

  answer(result);
}


status
valueReal(Real r, Real v)
{
#if REAL_IN_ONE
  r->value = v->value;
#else
  r->value1 = v->value1;
  r->value2 = v->value2;
#endif

  succeed;
}


static Real
getValueReal(Real r)
{ answer(r);
}


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_catchAll[] =
        { "selector=name", "argument=unchecked ..." };

/* Instance Variables */

static vardecl var_real[] =
{
#if REAL_IN_ONE
  IV(NAME_value, "alien:double", IV_NONE,
     NAME_storage, "double-value")
#else
  IV(NAME_value1, "alien:double1", IV_NONE,
     NAME_storage, "1-st part of double-value"),
  IV(NAME_value2, "alien:double2", IV_NONE,
     NAME_storage, "2-nd part of double-value")
#endif
};

/* Send Methods */

static senddecl send_real[] =
{ SM(NAME_initialise, 1, "value=any", initialiseReal,
     DEFAULT, "Create real by converting argument"),
  SM(NAME_divide, 1, "real", divideReal,
     NAME_calculate, "Divide value by argument"),
  SM(NAME_minus, 1, "real", minusReal,
     NAME_calculate, "Subtract argument from value"),
  SM(NAME_plus, 1, "real", plusReal,
     NAME_calculate, "Add argument to value"),
  SM(NAME_times, 1, "real", timesReal,
     NAME_calculate, "Multiply value by argument"),
  SM(NAME_equal, 1, "real", equalReal,
     NAME_compare, "Test if equal to argument"),
  SM(NAME_larger, 1, "real", largerReal,
     NAME_compare, "Test if larger than argument"),
  SM(NAME_largerEqual, 1, "real", largerEqualReal,
     NAME_compare, "Test if larger-or-equal than argument"),
  SM(NAME_lessEqual, 1, "real", lessEqualReal,
     NAME_compare, "Test if less-or-equal than argument"),
  SM(NAME_notEqual, 1, "real", notEqualReal,
     NAME_compare, "Test if not-equal to argument"),
  SM(NAME_smaller, 1, "real", smallerReal,
     NAME_compare, "Test if less than argument"),
  SM(NAME_value, 1, "real", valueReal,
     NAME_value, "Set the value to argument")
};

/* Get Methods */

static getdecl get_real[] =
{ GM(NAME_convert, 1, "real", "any", getConvertReal,
     DEFAULT, "Converts int, number and char_array"),
  GM(NAME_compare, 1, "{smaller,equal,larger}", "real", getCompareReal,
     NAME_compare, "Compare with argument"),
  GM(NAME_catchAll, 2, "copy=real", T_catchAll, getCatchAllReal,
     NAME_copy, "Create copy and run method on it"),
  GM(NAME_printName, 0, "string", NULL, getPrintNameReal,
     NAME_textual, "Formatted version (%g) of value"),
  GM(NAME_value, 0, "real", NULL, getValueReal,
     NAME_value, "Returns itself")
};

/* Resources */

#define rc_real NULL
/*
static classvardecl rc_real[] =
{
};
*/

/* Class Declaration */

static Name real_termnames[] = { NAME_value };

ClassDecl(real_decls,
          var_real, send_real, get_real, rc_real,
          1, real_termnames,
          "$Rev$");


status
makeClassReal(Class class)
{ declareClass(class, &real_decls);

  setCloneFunctionClass(class, cloneReal);
  setLoadStoreFunctionClass(class, loadReal, storeReal);

  succeed;
}
