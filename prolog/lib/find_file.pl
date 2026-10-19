/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(pce_finder, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines the driver for prompting files.  @finder uses the file
dialog of the desktop (`frame<-open_file`, `frame<-save_file` and
`frame<-open_directory`) if the class variable display.native_file_dialog
is @on (default) and there is a frame to show the dialog for.  Otherwise
it uses class find_file_dialog, which can also be embedded in other
windows.

Typical usage:

        :- pce_autoload(finder, library(find_file)).
        :- pce_global(@finder, new(finder)).

                ...,
                get(@finder, file, ...Options..., FileName),
                ....

Examples:

        get(@finder, file, save, pl, SaveFile)
        get(@finder, file, open, tuple('Source', chain(c,h)), SourceFile)

See <-file below for all optios.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


                 /*******************************
                 *     DEFAULT GLOBAL OBJECT    *
                 *******************************/

:- pce_global(@finder, new(finder)).

:- pce_autoload(find_file_dialog, find_file_dialog).


                 /*******************************
                 *           CLASS FINDER       *
                 *******************************/

:- pce_begin_class(finder, object,
                   "Find files on behalf of applications").

variable(directory,     directory,      both, "Current directory").
variable(label,         [char_array],   both, "Used label").

initialise(F) :->
    send_super(F, initialise),
    send(F, slot, directory, directory('.')).


file(F, Exists:exists=[bool|{open,save}], Ext0:extension='[name|chain|tuple]',
     Dir:directory=[directory], Default:default=[name], File:name) :<-
    "Get [existing] file with [extension]"::
    get_file(F, Exists, Ext0, Dir, Default, File).

get_file(F, Exists, Ext, Dir, Default, File) :-
    native_dialog_frame(Frame),
    !,
    mode(Exists, Mode),
    sdl_filters(Ext, Filters),
    default_location(F, Dir, Default, Location),
    (   Mode == save
    ->  get(Frame, save_file, Filters, Location, File)
    ;   get(Frame, open_file, Filters, Location, @off, File)
    ),
    file_directory_name(File, NewDir),
    send(F, slot, directory, NewDir).
get_file(F, Exists, Ext, Dir, Default, File) :-
    mode(Exists, Mode),
    get(F, label, Label),
    new(D, find_file_dialog(Mode, Label)),
    send(D, filter, Ext),
    (   Default \== @default
    ->  send(D, default_file, Default)
    ;   true
    ),
    (   Dir == @default
    ->  send(D, directory, F?directory)
    ;   send(D, directory, Dir)
    ),
    send(D, make_transient),
    send(D, message, message(D, return, @arg1)),
    get(D, confirm, File),
    send(F, slot, directory, D?directory),
    send(D, destroy).

native(_F) :->
    "True if the dialog of the desktop is used"::
    native_dialog_frame(_).

%   Not <-directory: that is the slot used by default_location/4.

select_directory(F, Dir:directory=[directory], Result:name) :<-
    "Ask for a directory using the dialog of the desktop"::
    native_dialog_frame(Frame),
    default_location(F, Dir, @default, Location),
    get(Frame, open_directory, Location, Result),
    send(F, slot, directory, Result).

%!  native_dialog_frame(-Frame) is semidet.
%
%   Frame is the frame to show the file dialog of the desktop for, if
%   display.native_file_dialog is @on.  This is the frame of the object
%   that receives the current event or else a frame that is shown.

native_dialog_frame(Frame) :-
    get(@display, class_variable_value, native_file_dialog, @on),
    (   send(@event, instance_of, event),
        get(@event, receiver, Receiver),
        send(Receiver, has_get_method, frame),
        get(Receiver, frame, Frame),
        shown_frame(Frame)
    ->  true
    ;   get(@display, frames, Frames),
        chain_list(Frames, List),
        member(Frame, List),
        shown_frame(Frame)
    ->  true
    ).

shown_frame(Frame) :-
    send(Frame, instance_of, frame),
    get(Frame, status, Status),
    memberchk(Status, [window, full_screen, open]).

%   default_location(+Finder, +Dir, +Default, -Location)
%
%   The file or directory the dialog starts with.

default_location(F, Dir, Default, Location) :-
    (   Dir == @default
    ->  get(F, directory, D)
    ;   D = Dir
    ),
    get(D, path, DirPath),
    (   Default == @default
    ->  Location = DirPath
    ;   directory_file_path(DirPath, Default, Location)
    ).

%!  sdl_filters(+Spec, -Filters)
%
%   Map the extension specification of <-file to the filters of
%   `frame<-open_file`: a chain of tuple(Label, Extensions), where
%   Extensions is separated by `;` and `*` matches any file.

sdl_filters(@default, @default) :- !.
sdl_filters(Spec, chain(Tuple)) :-
    atom(Spec),
    !,
    sdl_filter(Spec, Tuple).
sdl_filters(Spec, chain(Tuple)) :-
    send(Spec, instance_of, tuple),
    !,
    sdl_filter(Spec, Tuple).
sdl_filters(Chain, Filters) :-
    chain_list(Chain, Specs),
    maplist(sdl_filter, Specs, Tuples),
    Filters =.. [chain|Tuples].

sdl_filter(Tuple, tuple(Label, Pattern)) :-
    send(Tuple, instance_of, tuple),
    !,
    get(Tuple, first, Label),
    get(Tuple, second, Exts),
    sdl_pattern(Exts, Pattern).
sdl_filter(Ext0, tuple(Label, Ext)) :-
    sdl_pattern(Ext0, Ext),
    (   file_type(Ext, Label)
    ->  true
    ;   format(atom(Label), '~w files', [Ext])
    ).

sdl_pattern(Exts, Pattern) :-
    atom(Exts),
    !,
    (   atom_concat('.', Pattern, Exts)
    ->  true
    ;   Pattern = Exts
    ).
sdl_pattern(Chain, Pattern) :-
    chain_list(Chain, List),
    maplist(sdl_pattern, List, Patterns),
    atomic_list_concat(Patterns, ;, Pattern).

%!  mode(+Exists, -Mode)
%
%   Map old boolean mode to new named mode.

mode(@on,      open).
mode(@off,     save).
mode(@default, open).
mode(save,     save).
mode(open,     open).

%!  file_type(?Extension, ?Type) is nondet.
%
%   Allow the user to add rules to this predicate, showing proper
%   names to the user rather than patterns.  The collection here
%   is rather arbitrary ...  Maybe we should read the registery
%   for defined filetypes ...
%
%   @tbd    Merge with MIME-type library

:- multifile
    file_type/2.

file_type(pl,   'Prolog files').
file_type(c,    'C source files').
file_type(cc,   'C++ source files').
file_type(cpp,  'C++ source files').
file_type(cxx,  'C++ source files').
file_type(h,    'C header files').
file_type(pd,   'PceDraw files').
file_type(ps,   'PostScript files').
file_type(eps,  'Encapsulated PostScript files').
file_type(pdf,  'Portable Document Format files').
file_type(txt,  'Text files').
file_type(jpeg, 'JPEG images').
file_type(jpg,  'JPEG images').
file_type(gif,  'GIF images').
file_type(xpm,  'XPM images').
file_type(ico,  'Icon files').
file_type(cur,  'Cursor files').
file_type(html, 'HTML documents').
file_type(htm,  'HTML documents').
file_type(xml,  'XML documents').
file_type(sgml, 'SGML documents').
file_type(rdf,  'RDF files').
file_type(rdfs, 'RDF schema files').
file_type(tex,  'TeX or LaTeX files').
file_type(*,    'All files').

:- pce_end_class(finder).
