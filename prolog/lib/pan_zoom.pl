/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(pan_zoom, []).
:- use_module(library(pce)).

/** <module> Pan and zoom recogniser for figures

A pan_zoom_recogniser handles mouse-wheel zoom around the cursor and
left-button drag panning by editing the receiver figure's `<-transform`.
Both operations write the same six coefficients so they compose cleanly.

Recognisers are attached per instance. The shared global
`@pan_zoom_recogniser` is stateless across calls (the drag anchor lives
on the figure as an attribute), so the same instance can be attached to
many figures, typically from the figure's `->initialise`:

    initialise(F) :->
        send_super(F, initialise),
        send(F, recogniser, @pan_zoom_recogniser).

The receiver of each event must respond to `->transform`, `->translate`
and `<-window`; xpce's figure class does.
*/

:- pce_global(@pan_zoom_recogniser, new(pan_zoom_recogniser)).

:- pce_begin_class(pan_zoom_recogniser, recogniser,
                   "Wheel zoom around cursor + left-drag pan").

event(R, Ev:event) :->
    "Dispatch wheel, ms_left_down and ms_left_drag; fail otherwise"::
    (   send(Ev, is_a, wheel)
    ->  send(R, zoom, Ev)
    ;   send(Ev, is_a, ms_left_down)
    ->  send(R, pan_start, Ev)
    ;   send(Ev, is_a, ms_left_drag)
    ->  send(R, pan_drag, Ev)
    ;   fail
    ).

zoom(_R, Ev:event) :->
    "Scale the receiver figure around the cursor"::
    get(Ev, receiver, F),
    get(F, window, Win),
    get(Ev, position, Win, point(WX, WY)),
    get(F, position, point(FX, FY)),
    X is WX - FX,                       % cursor in figure-origin coord
    Y is WY - FY,
    get(Ev, rotation, Angle),
    S is (100 - Angle)/100,
    ensure_transform(F, T),
    get(T, xx, A0), get(T, xy, B0),
    get(T, yx, C0), get(T, yy, D0),
    get(T, tx, TX0), get(T, ty, TY0),
    A  is S*A0,  B  is S*B0,
    C  is S*C0,  D  is S*D0,
    TX is S*TX0 + (1-S)*X,
    TY is S*TY0 + (1-S)*Y,
    send(T, set, A, B, C, D, TX, TY),
    send(F, transform, T).

pan_start(_R, Ev:event) :->
    "Record the cursor anchor for a panning drag"::
    get(Ev, receiver, F),
    get(F, window, Win),
    get(Ev, position, Win, point(MX, MY)),
    ensure_transform(F, T),
    get(T, tx, TX0), get(T, ty, TY0),
    AX is MX - TX0, AY is MY - TY0,
    send(F, attribute, pan_anchor, point(AX, AY)).

pan_drag(_R, Ev:event) :->
    "Update the receiver's transform translation to keep the anchor fixed"::
    get(Ev, receiver, F),
    get(F, attribute, pan_anchor, point(AX, AY)),
    get(F, window, Win),
    get(Ev, position, Win, point(MX, MY)),
    get(F, transform, T),
    get(T, xx, A), get(T, xy, B),
    get(T, yx, C), get(T, yy, D),
    TX is MX - AX, TY is MY - AY,
    send(T, set, A, B, C, D, TX, TY),
    send(F, transform, T).

:- pce_end_class.

%!  ensure_transform(+Figure, -Transform) is det.
%
%   Bind Transform to Figure's `<-transform`, lazily creating an
%   identity transform if the slot is still `@nil`.

ensure_transform(F, T) :-
    send(F, translate, 0, 0),
    get(F, transform, T).
