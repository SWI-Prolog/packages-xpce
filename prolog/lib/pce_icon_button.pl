/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2003-2011, University of Amsterdam
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


:- module(pce_icon_button, []).
:- use_module(library(pce)).
:- use_module(library(help_message), []).
:- autoload(library(readutil), [read_file_to_string/3]).

/** <module> Small picture that does something when it is clicked

An icon_button shows a picture drawn from  a file, kept out of the way at
<-dim_opacity until the pointer is on   it,  where it comes out fully. It
carries no  behaviour of its  own: give  it a recogniser  and a ->help_message.

Naming an SVG file rather than drawing   the picture is what makes one of
these worth having: the file is easier   to  say what you want with, it is
drawn at whatever size the button is given, and whoever does not like it
can point the class variable at another one.

An SVG file that draws in `currentColor` is a _symbolic_ icon: it is
drawn in the colour of the class variable `symbolic_colour`, by default
the text colour of dialogs.  It is drawn again when the theme changes,
so it shows on light and dark backgrounds alike.
*/

:- pce_begin_class(icon_button, figure,
                   "Small picture that does something when clicked").

class_variable(dim_opacity, num, 0.4,
               "Opacity while the pointer is elsewhere").
class_variable(symbolic_colour, colour, ui_dialog_foreground,
               "Colour for `currentColor` in a symbolic SVG icon").

variable(image_file, name, get, "File the picture is drawn from").

:- pce_global(@icon_button_hover, make_icon_button_hover).

make_icon_button_hover(G) :-
    new(G, handler_group(handler(area_enter,
                                 message(@receiver, hovered, @on)),
                         handler(area_exit,
                                 message(@receiver, hovered, @off)))).

initialise(B, Image:name, Size:size) :->
    "Show Image, drawn at Size"::
    send_super(B, initialise),
    send(B, slot, image_file, Image),
    icon_image(B, Image, Size, Bitmap),
    send(B, display, bitmap(Bitmap)),
    send(B, hovered, @off),
    send(B, recogniser, @icon_button_hover).

colours_changed(B) :->
    "Draw a symbolic icon again in the new colour"::
    get(B, image_file, File),
    (   symbolic_svg(File, _)
    ->  get(B, member, bitmap, BM),
        get(BM?image, size, Size),
        icon_image(B, File, Size, Image),
        send(BM, image, Image)
    ;   true
    ).

hovered(B, Hovered:bool) :->
    "Come out fully while the pointer is on me"::
    (   Hovered == @on
    ->  Opacity = 1.0
    ;   get(B, class_variable_value, dim_opacity, Opacity)
    ),
    send(B, opacity, Opacity).

:- pce_end_class(icon_button).

%!  icon_image(+Button, +File, +Size, -Image) is det.
%
%   The picture on a button, at Size.  An SVG is drawn at that size rather
%   than drawn and then scaled to it, which is the point of naming one: it
%   stays sharp whatever size the button is given.  A symbolic SVG is
%   drawn in the `symbolic_colour` of Button.

icon_image(B, File, Size, Image) :-
    symbolic_svg(File, SVG),
    !,
    get(B, class_variable_value, symbolic_colour, Colour),
    colour_hex(Colour, Hex),
    get(Size, width, W),
    get(Size, height, H),
    (   symbolic_image(File, W, H, Hex, Image)
    ->  true
    ;   atomic_list_concat(Parts, currentColor, SVG),
        atomic_list_concat(Parts, Hex, Text),
        new(TB, text_buffer),
        send(TB, insert, 0, Text),
        new(Image, image(TB, W, H)),
        send(Image, lock_object, @on),
        assertz(symbolic_image(File, W, H, Hex, Image))
    ).
icon_image(_B, File, Size, Image) :-
    get(Size, width, W),
    get(Size, height, H),
    new(Image0, image(File, W, H)),
    (   send(Image0?size, equal, Size)
    ->  Image = Image0
    ;   get(Image0, scale, Size, Image)      % not an SVG after all
    ).

:- dynamic
    symbolic_image/5,                   % File, W, H, Hex, Image
    symbolic_svg_cache/2.               % File, SVG

%!  symbolic_svg(+File, -SVG) is semidet.
%
%   True when File is an SVG image that draws in `currentColor`.  SVG is
%   its text.

symbolic_svg(File, SVG) :-
    symbolic_svg_cache(File, SVG0),
    !,
    SVG0 \== false,
    SVG = SVG0.
symbolic_svg(File, SVG) :-
    (   file_name_extension(_, svg, File),
        absolute_file_name(image(File), Path,
                           [ access(read),
                             file_errors(fail)
                           ]),
        read_file_to_string(Path, SVG0, []),
        sub_string(SVG0, _, _, _, "currentColor")
    ->  assertz(symbolic_svg_cache(File, SVG0)),
        SVG = SVG0
    ;   assertz(symbolic_svg_cache(File, false)),
        fail
    ).

colour_hex(Colour, Hex) :-
    get(Colour, red, R),
    get(Colour, green, G),
    get(Colour, blue, B),
    format(atom(Hex), '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+', [R, G, B]).
