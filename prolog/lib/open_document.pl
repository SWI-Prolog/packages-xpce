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


:- module(open_document,
          [ open_document/1             % +File
          ]).
:- autoload(library(desktop), [desktop_open/1]).

/** <module> Open a file the way the user expects

Open a file in the editor if it is text and else using the application
the desktop associates with it.  Used by Epilog to open file links and
by tools that show files, such as demo(disk_usage).
*/

%!  open_document(+File) is det.
%
%   Open File.  A folder, image,  PDF   document,  etc.  is opened using
%   desktop_open/1.  Other files are  edited   as  text using edit/1.

open_document(File) :-
    (   (   exists_directory(File)
        ;   binary_file(File)
        )
    ->  desktop_open(File)
    ;   call(edit(file(File)))
    ).

%!  binary_file(+File) is semidet.
%
%   True when we know File cannot be edited as text.  Note that
%   file_mime_type/2 returns `application/unknown` if it does not know
%   the extension, so we must ask for known binary types rather than
%   for known text types.

:- if(exists_source(library(http/mimetype))).
:- autoload(library(http/mimetype), [file_mime_type/2]).

binary_file(File) :-
    file_mime_type(File, Type),
    binary_type(Type).
:- else.
binary_file(_) :-               % library(http) is not installed and we
    fail.                       % cannot tell.  Assume text.
:- endif.

binary_type(image/_).
binary_type(audio/_).
binary_type(video/_).
binary_type(application/pdf).
binary_type(application/msword).
binary_type(application/zip).
binary_type(application/wasm).
binary_type(application/'x-gzip').
binary_type(application/'x-gtar').
binary_type(application/'x-java-archive').
binary_type(application/'octet-stream').
