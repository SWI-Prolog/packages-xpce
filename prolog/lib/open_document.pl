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
          [ open_document/1,            % +File
            open_document_method/2      % +File, -Method
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
    open_document_method(File, Method),
    (   Method == edit
    ->  call(edit(file(File)))
    ;   desktop_open(File)
    ).

%!  open_document_method(+File, -Method) is det.
%
%   How open_document/1 opens File.  Method is one of
%
%     - edit
%       File is edited as text using edit/1.
%     - view
%       File is a folder, image, sound, video, PDF document or archive
%       that desktop_open/1 shows using the application the desktop
%       associates with it.
%     - application
%       As `view`, but the application may run code from File: a Java
%       archive or class, a Word document with macros, an unknown
%       binary or, on macOS, a folder with an extension such as an
%       application bundle (`.app`).  Tools may warn the user before
%       opening File this way.

open_document_method(File, Method) :-
    exists_directory(File),
    !,
    (   current_prolog_flag(apple, true),
        file_name_extension(_, Ext, File),
        Ext \== ''
    ->  Method = application
    ;   Method = view
    ).
open_document_method(File, Method) :-
    binary_file(File, Method),
    !.
open_document_method(_, edit).

%!  binary_file(+File, -Method) is semidet.
%
%   True when we know File cannot be edited as text and Method is
%   `view` or `application` as in open_document_method/2.  Note that
%   file_mime_type/2 returns `application/unknown` if it does not know
%   the extension, so we must ask for known binary types rather than
%   for known text types.

:- if(exists_source(library(http/mimetype))).
:- autoload(library(http/mimetype), [file_mime_type/2]).

binary_file(File, Method) :-
    file_mime_type(File, Type),
    binary_type(Type, Method).
:- else.
binary_file(_, _) :-            % library(http) is not installed and we
    fail.                       % cannot tell.  Assume text.
:- endif.

binary_type(image/_,                         view).
binary_type(audio/_,                         view).
binary_type(video/_,                         view).
binary_type(application/pdf,                 view).
binary_type(application/zip,                 view).
binary_type(application/'x-gzip',            view).
binary_type(application/'x-gtar',            view).
binary_type(application/msword,              application).
binary_type(application/wasm,                application).
binary_type(application/'x-java-archive',    application).
binary_type(application/'octet-stream',      application).
