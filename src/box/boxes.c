/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2011, University of Amsterdam
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

#define GLOBALS_HERE 1
#include "boxes.h"

static struct class_definition boxdefs[] =
{ { NAME_rubber, NAME_object, makeClassRubber,
    &ClassRubber, "Ease to get bigger or smaller" },
  { NAME_hbox, NAME_object, makeClassHBox,
    &ClassHBox, "Typesetting: horizontal box" },
  { NAME_tbox, NAME_hbox, makeClassTBox,
    &ClassTBox, "Typesetting: horizontal box holding text" },
  { NAME_grbox, NAME_hbox, makeClassGrBox,
    &ClassGrBox, "Typesetting: horizontal box holding graphical" },
  { NAME_parbox, NAME_device, makeClassParBox,
    &ClassParBox, "Typesetting: paragraph" },
  { NAME_lbox, NAME_device, makeClassLBox,
    &ClassLBox, "Typesetting: list-box" },
  { NULL, NULL, NULL, NULL, NULL }
};


void
initBoxes()
{ defineClasses(boxdefs);
}
