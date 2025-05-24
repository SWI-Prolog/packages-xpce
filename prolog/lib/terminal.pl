:- module(terminal,
          [ terminal/1
          ]).
:- use_module(library(pce)).
:- use_module(library(threadutil)).
:- use_module(library(edit)).
:- use_module(library(pce_util)).
:- use_module(library(uri)).
:- use_module(library(www_browser)).
:- use_module(library(gensym)).
:- use_module(library(time)).

terminal(T) :-
    send(new(T, terminal), open).

:- dynamic
    current_prolog_terminal/2.          % Thread, Terminal

:- pce_begin_class(prolog_terminal, terminal_image,
                   "Terminal for running a Prolog thread").

initialise(PT) :->
    "Create Prolog terminal"::
    send_super(PT, initialise),
    send(PT, name, terminal),
    send(PT, link_message, message(@receiver, open_link, @arg1)).

unlink(PT) :->
    (   retract(current_prolog_terminal(Thread, PT))
    ->  catch(terminate_thread(Thread), error(_,_), true)
    ;   true
    ),
    send_super(PT, unlink).

terminate_thread(Thread) :-
    thread_signal(Thread, clean_exit).

%!  clean_exit
%
%   Make the console thread exit immediately. Various things can happen,
%   depending in the timing. By setting   `debug_on_error`  to false, we
%   instruct the system not only to stop invoking the debugger, but also
%   to suppress print_message/2 reports if   the  `user_error` stream is
%   lost.

clean_exit :-
    set_prolog_flag(debug_on_error, false),
    thread_exit(console).

%!  terminated
%
%   Called from at_exit(Goal) option of the created thread.

terminated :-
    thread_self(Me),
    (   retract(current_prolog_terminal(Me, PT))
    ->  in_pce_thread(send(PT?window, destroy))
    ;   true
    ),
    close_io.

close_io :-
    catch(thread_util:disable_line_editing(user_input, user_output, user_error),
          error(_,_), true),
    close(user_input, [force(true)]),
    close(user_output, [force(true)]),
    close(user_error, [force(true)]).

interrupt(PT) :->
    "Interrupt client Prolog process"::
    current_prolog_terminal(Thread, PT),
    current_signal(int, SIGINT, debug),
    thread_signal(Thread, SIGINT).

open_link(_T, Href:name) :->
    "Open a clicked hyperlink"::
    tty_link(Href).


                /*******************************
                *     MANAGE PROLOG THREAD     *
                *******************************/

connect(TI, Title) :-
    gensym(con, Alias),
    get(TI, pty_name, PTY),
    thread_self(Me),
    thread_create(thread_run_interactor(Me, PTY, Title), Thread,
                  [ detached(true),
                    alias(Alias),
                    at_exit(terminated)
                  ]),
    asserta(current_prolog_terminal(Thread, TI)),
    thread_get_message(Msg),
    (   Msg = title(Title0)
    ->  Title = Title0
    ;   Msg = throw(Error)
    ->  throw(Error)
    ;   Msg = false
    ->  fail
    ).

thread_run_interactor(Creator, PTY, Title) :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    Error = error(Formal,_),
    (   catch(attach_terminal(PTY, Title), Error, true)
    ->  (   var(Formal)
        ->  thread_send_message(Creator, title(Title)),
            print_message(banner, thread_welcome),
            prolog
        ;   thread_send_message(Creator, throw(Error))
        )
    ;   thread_send_message(Creator, false)
    ).

attach_terminal(PTY, _Title) :-
    open(PTY, read,  In,  [encoding(utf8), bom(false)]),
    open(PTY, write, Out, [encoding(utf8)]),
    open(PTY, write, Err, [encoding(utf8)]),
    set_stream(In,  file_name('')),     % kill source_location/2
    set_stream(Out, buffer(line)),
    set_stream(Err, buffer(false)),
    set_stream(In,  eof_action(reset)),
    set_stream(In,  tty(true)),
    set_stream(Out, tty(true)),
    set_stream(Err, tty(true)),
    set_stream(In,  alias(user_input)),
    set_stream(Out, alias(user_output)),
    set_stream(Err, alias(user_error)),
    set_stream(In,  alias(current_input)),
    set_stream(Out, alias(current_output)),
    thread_util:enable_line_editing(In,Out,Err).

%!  tty_link(+Link) is det.
%
%   Handle a terminal hyperlink to ``file://`` links

tty_link(Link) :-
    uri_file_name(Link, File),
    !,
    uri_components(Link, Components),
    uri_data(fragment, Components, Fragment),
    fragment_location(Fragment, File, Location),
    call(edit(Location)).
tty_link(URL) :-
    call(www_open_url(URL)).

fragment_location(Fragment, File, file(File)) :-
    var(Fragment),
    !.
fragment_location(Fragment, File, File:Line:Column) :-
    split_string(Fragment, ":", "", [LineS,ColumnS]),
    !,
    number_string(Line, LineS),
    number_string(Column, ColumnS).
fragment_location(Fragment, File, File:Line) :-
    atom_number(Fragment, Line).

:- pce_end_class(prolog_terminal).


                /*******************************
                *           TERMINAL           *
                *******************************/

:- pce_begin_class(terminal, window, "Implement an embedded terminal").

variable(thread_id, int, none, "Thread alias or integer id").

initialise(T, Title:title=[name],
           Width:width=[integer], Height:height=[integer]) :->
    "Create from title, width and height"::
    default(Title, "SWI-Prolog console", TheTitle),
    default(Width, 80, TheWidth),
    default(Height, 25, TheHeight),
    new(TI, prolog_terminal),
    get(TI, class_variable_value, font, Font),
    get(Font, height, FH),
    get(Font, width, m, EM),
    WH is round(TheHeight*FH),
    WW is round(TheWidth*EM),
    send_super(T, initialise, TheTitle, size(WW,WH)),
    send(T, display, new(SB, scroll_bar(TI, vertical))),
    send(T, display, TI),
    send(T, keyboard_focus, TI),
    send(TI, scroll_bar, SB).

resize(T) :->
    "Place terminal and scrollbar"::
    get(T, size, size(TW, TH)),
    get(T, member, scroll_bar, SB),
    get(SB, width, SBW),
    send(SB, set, TW-SBW, 0, @default, TH),
    get(T, member, terminal, TI),
    send(TI, set, 0, 0, TW-SBW, TH).

open(T) :->
    "Open the terminal and attach a Prolog thread to it"::
    send_super(T, open),
    get(T, member, terminal, TI),
    connect(TI, "Test").

:- pce_end_class(terminal).
