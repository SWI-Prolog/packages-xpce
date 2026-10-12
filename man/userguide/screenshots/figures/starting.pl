/*  Figures for starting.plx: "Getting started"
*/

:- module(fig_starting, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).

:- multifile
    screenshot:figure/2.

%   \figref{dialog}: the dialog created by typing the examples of
%   the section "Modifying object state: send".

screenshot:figure(dialog, D) :-
    new(D, dialog('Demo Window')),
    send(D, append(text_item(name))).

%   \figref{fileviewer}: fileviewer/1 of the section "Example: show
%   files in directory", showing the directory with the examples of
%   this guide after viewing examples/fileviewer.pl.

screenshot:figure(fileviewer, [Viewer-point(0,0), View-point(0,Y)]) :-
    examples_dir(Dir),
    fileviewer(Dir),
    last_frame(Viewer),
    settle,
    get(Viewer, member, browser, B),
    send(B, selection, 'fileviewer.pl'),
    get(Viewer, member, dialog, D),
    get(D, member, view, Button),
    send(Button, execute),
    last_frame(View),
    settle,
    get(View, member, view, V),
    send(V, size, size(60, 14)),
    send(V, caret, 0),
    get(Viewer?area, height, H),
    Y is H+22+20.

fileviewer(Dir) :-
    new(DirObj, directory(Dir)),
    new(F, frame('File Viewer')),
    send(F, append(new(B, browser))),
    send(new(D, dialog), below(B)),
    send(D, append(button(view,
                          message(@prolog, view,
                                  DirObj, B?selection?key)))),
    send(D, append(button(quit,
                          message(F, destroy)))),
    send(B, members(DirObj?files)),
    send(F, open).

% The guide's view/2 is called through @prolog, i.e., in module user.

user:view(DirObj, F) :-
    send(new(V, view(F)), open),
    get(DirObj, file(F), FileObj),
    send(V, load(FileObj)).

examples_dir(Dir) :-
    module_property(fig_starting, file(File)),
    file_directory_name(File, FigDir),
    file_directory_name(FigDir, ScreenDir),
    file_directory_name(ScreenDir, GuideDir),
    directory_file_path(GuideDir, examples, Dir).
