/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993-1997 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_RECOGNISER_H
#define _PCE_RECOGNISER_H

PceExternalClass(ClassRecogniser);
class PceRecogniser :public PceObject
{
public:
  PceRecogniser() :
    PceObject(ClassRecogniser)
  {
  }
};

inline PceRecogniser
AsRecogniser(PceArg a)
{ return *((PceRecogniser*) &a);
}

#endif /*!_PCE_RECOGNISER_H*/
