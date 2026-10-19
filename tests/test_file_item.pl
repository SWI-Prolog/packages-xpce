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


:- module(test_file_item, [test_file_item/0]).

/** <module> Tests for file_item and the choice of file dialog

A file_item shows an icon right of its field that runs ->browse.  @finder
uses the file dialog of the desktop if display.native_file_dialog is @on
and a frame is shown; these tests do not open dialogs: the dialog of the
desktop would appear on the user's screen.

Run with:

    swipl -g test_file_item -t halt packages/xpce/tests/test_file_item.pl
*/

:- set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- use_module(library(pce)).
:- use_module(library(plunit)).
:- use_module(library(file_item)).
:- use_module(library(find_file)).

test_file_item :-
    run_tests([ file_item_icon,
                finder_filters,
                finder_native
              ]).

:- begin_tests(file_item_icon).

test(browse_icon, [Message == browse]) :-
    new(FI, file_item(file)),
    get(FI, icon, Icon),
    assertion(send(Icon, instance_of, image)),
    get(FI?icon_message, selector, Message).
test(directory_item, [true(send(Icon, instance_of, image))]) :-
    new(DI, directory_item(dir)),
    get(DI, icon, Icon).
test(no_browse_image, [ Icon == @nil,
                        cleanup(send(class(file_item), class_variable_value,
                                     browse_image, 'tool/open.svg'))
                      ]) :-
    send(class(file_item), class_variable_value, browse_image, @nil),
    new(FI, file_item(file)),
    get(FI, icon, Icon).
test(not_in_find_file_dialog, [Icons == [@nil, @nil]]) :-
    new(D, find_file_dialog(open, 'Test')),
    get(D, member, file, File),
    get(D, member, look_in, LookIn),
    get(File, icon, I1),
    get(LookIn, icon, I2),
    Icons = [I1, I2],
    send(D, destroy).

:- end_tests(file_item_icon).

:- begin_tests(finder_filters).

test(extension, [F == chain(tuple('Prolog files', pl))]) :-
    pce_finder:sdl_filters(pl, F).
test(dot_extension, [F == chain(tuple('Prolog files', pl))]) :-
    pce_finder:sdl_filters('.pl', F).
test(unknown_extension, [F == chain(tuple('xyz files', xyz))]) :-
    pce_finder:sdl_filters(xyz, F).
test(tuple, [F == chain(tuple('Sound files', 'wav;mp3'))]) :-
    pce_finder:sdl_filters(tuple('Sound files', chain(wav, '.mp3')), F).
test(chain, [F == chain(tuple('Prolog files', pl), tuple('All files', *))]) :-
    pce_finder:sdl_filters(chain(pl, *), F).
test(default, [F == @default]) :-
    pce_finder:sdl_filters(@default, F).

:- end_tests(finder_filters).

:- begin_tests(finder_native,
               [ cleanup(send(class(display), class_variable_value,
                              native_file_dialog, @on))
               ]).

test(shown_frame, [ Native-Off == true-false ]) :-
    new(F, frame(test)),
    send(F, append, new(dialog)),
    send(F, open),
    send(class(display), class_variable_value, native_file_dialog, @on),
    ( send(@finder, native) -> Native = true ; Native = false ),
    send(class(display), class_variable_value, native_file_dialog, @off),
    ( send(@finder, native) -> Off = true ; Off = false ),
    send(class(display), class_variable_value, native_file_dialog, @on),
    send(F, destroy).

%   The dialog starts in the directory of the finder.  A method
%   <-directory once hid the slot, which recursed without end.

test(default_location, [Location == Path]) :-
    get(@finder, directory, Dir),
    get(Dir, path, Path),
    pce_finder:default_location(@finder, @default, @default, Location).
test(default_location_file, [Location == Expected]) :-
    get(@finder?directory, path, Path),
    directory_file_path(Path, 'x.wav', Expected),
    pce_finder:default_location(@finder, @default, 'x.wav', Location).

:- end_tests(finder_native).
