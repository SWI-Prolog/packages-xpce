/*  Figures for libdia.plx: "Dialog support libraries"
*/

:- module(fig_libdia, []).
:- use_module(library(pce)).

:- multifile
    screenshot:figure/2.

%   \figref{libdia}: the application framework of the section
%   "Example".  The code below is copied from the guide.

screenshot:figure(libdia, MyApp) :-
    new(MyApp, myapp).

%       Pull in the classes

:- pce_autoload(report_dialog, library(pce_report)).
:- pce_autoload(tool_bar, library(toolbar)).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

%       Define icons as program resources

resource(printer,       image,  image('tool/print.svg')).
resource(floppy,        image,  image('tool/save.svg')).

%       Define the application as a subclass of frame.

:- pce_begin_class(myapp, frame,
                   "Frame representing the application").

initialise(MyApp) :->
        send_super(MyApp, initialise, 'My application'),
        send(MyApp, append, new(D, dialog)),
        send(D, pen, 0),
        send(D, gap, size(5, 5)),
        send(D, append, new(menu_bar)),
        send(D, append, new(tool_bar(MyApp))),
        send(MyApp, fill_menu_bar),
        send(MyApp, fill_tool_bar),
        send(new(W, myapp_workspace), below, D),
        send(new(report_dialog), below, W).

fill_menu_bar(F) :->
        get(F, member, dialog, D),
        get(D, member, menu_bar, MB),
        send_list(MB, append,
                  [ new(File, popup(file)),
                    new(_Edit, popup(edit))
                  ]),

        send_list(File, append,
                  [ menu_item(load,
                              message(F, load),
                              end_group := @on),
                    menu_item(print,
                              message(F, print))
                  ]).

fill_tool_bar(F) :->
        get(F, member, dialog, D),
        get(D, member, tool_bar, TB),
        send_list(TB, append,
                  [ tool_button(load,
                                resource(floppy),
                                load),
                    gap,                % skip a little
                    tool_button(print,
                                resource(printer),
                                print)
                  ]).

print(MyApp) :->
        "Print the document"::
        send(MyApp, report, progress, 'Printing ...'),
        get(MyApp, member, myapp_workspace, WS),
        send(WS, print),
        send(MyApp, report, progress, done).

load(MyApp) :->
        "Ask a file and load it"::
        get(@finder, file, @on, myp, File),
        get(MyApp, member, myapp_workspace, WS),
        send(WS, load, File).

:- pce_end_class(myapp).


%       dummy class for the work-area of your application.  The
%       print_graphics template defines ->print.

:- use_module(library(print_graphics)).

:- pce_begin_class(myapp_workspace, window).
:- use_class_template(print_graphics).

:- pce_end_class(myapp_workspace).
