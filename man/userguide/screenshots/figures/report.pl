/*  Figures for report.plx: "Showing progress and errors"
*/

:- module(fig_report, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).

:- multifile
    screenshot:figure/2.

%   \figref{reportdemo}: reportdemo/0 of the section "Example" after
%   loading a file.  The guide's load/1 asks for the file using `frame
%   <-open_file`.  We cannot automate the file dialog, so load_file/2
%   below is load/1 using a fixed file: the example code of this
%   figure.

screenshot:figure(reportdemo, Frame) :-
    reportdemo,
    get(@display, frames, Frames),
    get(Frames, tail, Frame),
    settle,
    load_file(Frame, 'examples/report.pl').

reportdemo :-
    new(Frame, frame('Reporter demo')),
    new(B, browser),
    send(new(picture), right, B),
    send(new(MD, dialog), above, B),
    send(new(RD, dialog), below, B),
    send(Frame, append, B),

    send(MD, append, new(MB, menu_bar)),
    send(MD, pen, 0),
    send(MD, gap, size(0,0)),
    send(MB, append, new(File, popup(file))),
    send_list(File, append,
              [ menu_item(load,
                          message(@prolog, load, Frame),
                          end_group := @on),
                menu_item(quit, message(Frame, destroy))
              ]),

    send(RD, append, label(reporter)),
    send(RD, gap, size(5, 0)),
    send(Frame, open).

load_file(Frame, FileName) :-
    send(Frame, report, progress,
         'Loading %s ...', FileName),
    get(Frame, member, browser, Browser),
    new(File, file(FileName)),
    send(File, open, read),
    (   repeat,
            (   get(File, read_line, Line)
            ->  send(Line, strip),
                send(Browser, append, Line),
                fail
            ;   !,
                send(File, close)
            )
    ),
    send(Frame, report, done).
