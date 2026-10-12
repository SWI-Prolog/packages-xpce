/*  Figures for drawing.plx: "Paths, paint and text"

    The figures run the examples of examples/drawing.pl, the code shown
    in the chapter, on a picture of a fixed size.
*/

:- module(fig_drawing, []).
:- use_module(library(pce)).
:- use_module('../../examples/drawing').

:- multifile
    screenshot:figure/2.

screenshot:figure(drawpaths, P) :-
    example_picture('Paths', size(450, 140), paths_example, P).
screenshot:figure(drawpaint, P) :-
    example_picture('Paint', size(450, 140), paint_example, P).
screenshot:figure(drawlines, P) :-
    example_picture('Line styles', size(470, 160), lines_example, P).
screenshot:figure(drawmarkup, P) :-
    example_picture('Markup', size(470, 180), markup_example, P).
screenshot:figure(drawcompose, P) :-
    example_picture('Clipping and blending', size(320, 140),
                    compose_example, P).
screenshot:figure(drawcanvas, P) :-
    example_picture('Canvas', size(220, 180), canvas_example, P).

example_picture(Label, Size, Example, P) :-
    new(P, picture(Label, Size)),
    send(P, scrollbars, none),
    call(Example, P).
