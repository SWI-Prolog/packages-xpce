/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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


:- module(test_text_image, [test_text_image/0]).
:- encoding(utf8).

/** <module> Tests for <-text_image and <-image

<-image is the pixels of a visual: a frame, a window.  The text a browser
or a view shows is <-text_image, which is what its editor or list_browser
answers, reached by delegation.  The two used to share the name <-image,
which meant a window could not have one of its own.

Run with:

    swipl -g test_text_image -t halt \
          packages/xpce/tests/test_text_image.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(swi/thread_monitor), []).

test_text_image :-
    run_tests([ text_image
              ]).

:- begin_tests(text_image).

test(an_editor_shows_its_text_in_a_text_image) :-
    new(E, editor),
    get(E, text_image, Image),
    send(Image, instance_of, text_image).

test(a_list_browser_too) :-
    new(LB, list_browser),
    get(LB, text_image, Image),
    send(Image, instance_of, text_image).

%   A view and a browser are windows holding one of those, and answer for
%   it: the code that wants tab stops or a recogniser on the text asks
%   them, not the editor.

test(a_view_answers_for_its_editor) :-
    new(V, view),
    get(V, text_image, Image),
    get(V?editor, text_image, Image).

test(a_browser_answers_for_its_list_browser) :-
    new(B, browser),
    get(B, text_image, Image),
    get(B?list_browser, text_image, Image).

%   Which is asked for in ->initialise, where a window has no pixels to
%   answer with: <-image on class window used to be reached instead, and
%   Tools/View threads could not be opened at all.

test(a_browser_answers_before_it_is_created) :-
    new(TB, thread_browser),
    get(TB, text_image, Image),
    send(Image, instance_of, text_image).

test(the_pixels_of_a_window_are_something_else) :-
    new(V, view),
    get(V, text_image, Text),
    catch(get(V, image, Pixels), _, Pixels = none),
    Pixels \== Text.                    % <-image is not the text image

:- end_tests(text_image).
