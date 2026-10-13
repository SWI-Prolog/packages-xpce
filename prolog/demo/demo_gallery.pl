/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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


:- module(demo_gallery,
          [ gallery/4,                  % +Label, +Cols, +Rows, -Picture
            gallery_cell/5,             % +Picture, +Col, +Row, +Title, :Goal
            gallery_show/4,             % +Picture, +Path, +X, +Y
            gallery_save/3,             % +Graphical, +Base, +Format
            gallery_save_buttons/3      % +Dialog, +Graphical, +Base
          ]).
:- use_module(library(pce)).

/** <module> Layout for the demo galleries

A gallery is a frame holding a picture divided into labelled cells of
238x178 pixels and a dialog with buttons to save the picture as SVG
(if supported) or PDF.  Each cell shows one feature.  Used by the path, paint and text
galleries.
*/

cell_width(240).
cell_height(180).

%!  gallery(+Label, +Cols, +Rows, -Picture) is det.
%
%   Create the frame of a gallery with Cols x Rows cells.  The caller
%   fills the cells using gallery_cell/5 and opens the frame using
%   `send(Picture?frame, open)`.  The dialog is the member `dialog` of
%   the frame, so the caller can add items to it.

gallery(Label, Cols, Rows, P) :-
    cell_width(CW), cell_height(CH),
    W is Cols*CW, H is Rows*CH,
    new(F, frame(Label)),
    send(F, append, new(P, picture(Label, size(W, H)))),
    send(P, scrollbars, none),
    new(D, dialog),
    send(D, below, P),
    file_base(Label, Base),
    gallery_save_buttons(D, P, Base),
    send(D, append, button(quit, message(F, destroy)), right).

%!  gallery_cell(+Picture, +Col, +Row, +Title, :Goal) is det.
%
%   Draw a labelled cell at column Col and row Row (0-based) and call
%   Goal(Picture, X, Y) to fill it, where X,Y is the top-left corner of
%   the cell.

:- meta_predicate gallery_cell(+,+,+,+,3).

gallery_cell(P, Col, Row, Title, Goal) :-
    cell_width(CW), cell_height(CH),
    X is Col*CW, Y is Row*CH,
    BW is CW-2, BH is CH-2,
    send(P, display, new(B, box(BW, BH)), point(X, Y)),
    send(B, colour, colour(grey80)),
    LX is X+6, LY is Y+4,
    send(P, display, new(T, text(Title, left, font(sans, bold, 11))),
         point(LX, LY)),
    send(T, colour, colour(grey40)),
    call(Goal, P, X, Y).

%!  gallery_show(+Picture, +Path, +X, +Y) is det.
%
%   Display Path such that the origin of the coordinates of its points
%   is at X,Y.  Displaying at a point would place the top-left corner
%   of its area there, which depends on the shape and the pen.

gallery_show(P, Path, X, Y) :-
    PX is X, PY is Y,
    send(P, display, Path),
    get(Path?offset, x, OX),
    get(Path?offset, y, OY),
    DX is PX-OX, DY is PY-OY,
    send(Path, relative_move, point(DX, DY), offset).

%!  gallery_save_buttons(+Dialog, +Graphical, +Base) is det.
%
%   Append buttons to Dialog that save Graphical using gallery_save/3:
%   one for each format XPCE supports.  The first button goes below the
%   last item of Dialog, the others right of it.

gallery_save_buttons(D, Gr, Base) :-
    (   send(@pce, has_feature, svg)
    ->  Formats = [svg, pdf]
    ;   Formats = [pdf]
    ),
    save_buttons(Formats, D, Gr, Base, @default).

save_buttons([], _, _, _, _).
save_buttons([Format|T], D, Gr, Base, Where) :-
    atom_concat(save_, Format, Name),
    send(D, append,
         button(Name, message(@prolog, gallery_save, Gr, Base, Format)),
         Where),
    save_buttons(T, D, Gr, Base, right).

%!  gallery_save(+Graphical, +Base, +Format) is det.
%
%   Prompt for a file, proposing Base.Format, and save Graphical (a
%   window or any other graphical) as SVG or PDF.

gallery_save(Gr, Base, Format) :-
    (   Format == svg
    ->  Filter = tuple('SVG images', svg)
    ;   Filter = tuple('PDF documents', pdf)
    ),
    file_name_extension(Base, Format, Default),
    get(Gr?frame, save_file, chain(Filter), Default, File0),
    file_name_extension(Plain, _, File0),
    file_name_extension(Plain, Format, File),
    send(Gr, Format, File),
    send(Gr, report, status, 'Saved to %s', File).

file_base(Label, Base) :-
    downcase_atom(Label, Lower),
    atomic_list_concat(Parts, ' ', Lower),
    atomic_list_concat(Parts, '_', Base).
