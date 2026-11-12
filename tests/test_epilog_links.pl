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


:- module(test_epilog_links, [test_epilog_links/0]).

/** <module> Tests for opening hyperlinks in Epilog

A program running in Epilog decides  the  URL   of  a  link (OSC 8).
Clicking it must not ask the desktop to   run a handler for any scheme
it likes.
*/

:- use_module(library(plunit)).

:- if(exists_source(library(epilog))).	% not in single threaded builds
:- use_module(library(epilog)).

test_epilog_links :-
    run_tests([ epilog_links
              ]).

:- begin_tests(epilog_links).

test(web, true) :-
    forall(member(URL, [ 'https://www.swi-prolog.org',
                         'http://localhost:3020/pldoc/',
                         'HTTPS://example.com',
                         'mailto:someone@example.com'
                       ]),
           epilog:browser_url(URL)).
test(other_schemes, true) :-
    forall(member(URL, [ 'ms-msdt:/id PCWDiagnostic',
                         'search-ms:query=x',
                         'javascript:alert(1)',
                         'smb://host/share',
                         'no scheme'
                       ]),
           \+ epilog:browser_url(URL)).

:- end_tests(epilog_links).

:- else.

test_epilog_links.

:- endif.
