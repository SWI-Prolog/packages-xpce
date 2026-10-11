/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2000-2011, University of Amsterdam
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

:- module(vfont, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Class vfont defines a `virtual' font:   a  collection of font attributes
that can be manipulated without having to  worry about the physical font
that is available for satisfying the   specification.  The method <-font
extracts the best match physical font.

The font is computed from these properties.  A virtual font may be
mapped to a specific font using the multifile predicate

        vfont:font_map(Family,          % an atom
                       Encoding,        % an atom
                       Slant,           % {r,i}
                       Weight,          % {medium, bold}
                       Fixed,           % @on, @off
                       size,            % 1..7
                       Font).           % Resulting font
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
    font_map/7.

                 /*******************************
                 *         CLASS VFONT          *
                 *******************************/

:- pce_begin_class(vfont, object, "Virtual font").

variable(family,   name,          both, "Basic font family").
variable(encoding, name,          both, "Character-set encoding used").
variable(slant,    {r,i},         both, "Slanted?").
variable(weight,   {medium,bold}, both, "Weight of the font").
variable(fixed,    bool,          both, "@off: proportional").
variable(size,     '1..7',        both, "Size index").

initialise(VF) :->
    "Create with default attributes"::
    send(VF, family,   sans),
    send(VF, encoding, latin1),
    send(VF, slant,    r),
    send(VF, weight,   medium),
    send(VF, fixed,    @off),
    send(VF, size,     3).

font(VF, Font:font) :<-
    "Get closest font"::
    get(VF, family, Fam),
    get(VF, encoding, Enc),
    get(VF, slant,  Slant),
    get(VF, weight, W),
    get(VF, fixed, Fixed),
    get(VF, size, Size),
    (   font_map(Fam, Enc, Slant, W, Fixed, Size, Font0),
        catch(get(@pce, convert, Font0, font, Font1), _, fail)
    ->  Font = Font1
    ;   font(Fam, Slant, W, Fixed, Size, Font)
    ).

font_size(VF, Diff:int) :->
    "Change relative size of font"::
    get(VF, size, Size0),
    Size is max(1, min(7, Size0+Diff)),
    send(VF, size, Size).

%   font(+Family, +Slant, +Weight, +Fixed, +Size, -Font)
%
%   Font is the xpce font for the virtual font.  Its size is the size of
%   the font `normal`, scaled for the size index.  A fixed-width font
%   uses the same size, so code has the size of the text around it.

font(Family0, Slant, Weight, Fixed, Size, Font) :-
    (   Fixed == @on
    ->  Family = mono
    ;   Family = Family0
    ),
    slant_style(Slant, Style),
    weight(Weight, FontWeight),
    get(@pce, convert, normal, font, Normal),
    get(Normal, points, Points0),
    size_scale(Size, Scale),
    Points is round(Points0*Scale),
    new(Font, font(Family, Style, Points, FontWeight)).

slant_style(r, normal).
slant_style(i, italic).

weight(medium, normal).
weight(bold,   bold).

%   size_scale(?Size, ?Scale)
%
%   Scale of the HTML font size index, where 3 is the normal size.

size_scale(1, 0.75).
size_scale(2, 0.85).
size_scale(3, 1.0).
size_scale(4, 1.17).
size_scale(5, 1.33).
size_scale(6, 1.5).
size_scale(7, 1.75).

:- pce_end_class.





