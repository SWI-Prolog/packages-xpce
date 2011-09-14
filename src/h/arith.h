/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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

#ifndef ARITH_H_INCLUDED
#define ARITH_H_INCLUDED

#define V_ERROR	      (-1)
#define V_INTEGER      (0)
#define V_DOUBLE       (1)

typedef struct
{ int type;				/* V_INTEGER or V_FLOAT */
  union
  { long	i;			/* integer value */
    double	f;			/* float value */
  } value;
} numeric_value, *NumericValue;

#include <ari/proto.h>
#endif /*ARITH_H_INCLUDED*/
