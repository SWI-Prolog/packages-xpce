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


:- module(test_emacs_chrome_server, [test_emacs_chrome_server/0]).

/** <module> Tests for library(emacs/emacs_chrome_server)

The server for the _Edit with Emacs_ browser extension listens on
localhost, where any web page the user visits can post to it.
*/

:- use_module(library(plunit)).

:- if((current_prolog_flag(threads, true),
       exists_source(library(http/thread_httpd)))).

:- use_module(library(emacs/emacs_chrome_server)).

test_emacs_chrome_server :-
    run_tests([ emacs_chrome_server
              ]).

:- begin_tests(emacs_chrome_server).

test(web_page, throws(http_reply(forbidden(edit)))) :-
    % Used to open an editor holding the posted text
    emacs_chrome_server:edit([ method(post),
                               origin('https://example.com')
                             ]).
test(null_origin, throws(http_reply(forbidden(edit)))) :-
    emacs_chrome_server:edit([ method(post),
                               origin(null)
                             ]).
test(extension, fail) :-
    member(Origin, [ 'chrome-extension://abcdef',
                     'moz-extension://0123-4567'
                   ]),
    emacs_chrome_server:web_page_request([origin(Origin)]).
test(no_origin, fail) :-
    emacs_chrome_server:web_page_request([method(post)]).

:- end_tests(emacs_chrome_server).

:- else.

test_emacs_chrome_server.

:- endif.
