/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/projects/xpce
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

:- module(test_terminal,
          [ test_terminal/0,
            test_terminal/1,                     % +Backend
            test_terminal/2,                     % +Backend, +Unit
            terminal_test_unit/1,                % ?Unit
            test_terminal_random/2,              % +Sessions, +CommandsPerSession
            test_terminal_random/3               % +Sessions, +CommandsPerSession, +Options
          ]).
:- encoding(utf8).

/** <module> Integration tests for the terminal + libedit

Drives a terminal end-to-end through three primitives:

    - send a keystroke      (UTF-8 bytes towards the line editor)
    - read the cursor       point(col, row) in the visible window
    - read a row            text content of a visible row

Tests type and hit keys, drive the event loop until output has settled,
then assert the cursor position and row contents.

The three primitives are all the suite needs, so it can run against
more than one terminal.  A *backend* (see the BACKENDS section) says
which line editor produces the output and which screen we read back;
term_start/2 opens one and every test then works through the term_*
primitives rather than through xpce methods directly.  Supported:

    - epilog          the epilog terminal driven by a Prolog thread in
                      this process; libedit runs with the EPILOG flag
    - child(Profile)  a child `swipl` started with shell/1 from the
                      epilog thread, so it runs on the terminal's pty
                      with TERM taken from Profile.  This is the only
                      way to exercise libedit's ordinary termcap paths,
                      the ones every non-epilog terminal uses.

Profile is the name of a terminal description: `xterm', `screen',
`linux', `ansi', ... and `winconsole', which is not a stock one.
swipl.exe on a Windows console reads no description at all -- it links
the fake termcap in packages/libedit/libedit/src/win_ncurses.c -- so
swipl-winconsole.ti in this directory writes that table out as a
description, and running against it puts libedit through the same
decisions the Windows console does.  Both bugs the suite has found so
far were of that kind, which is the argument for keeping the two in
step.

The screen is the xpce terminal whichever profile is chosen, so this
tests what libedit does, not what a Windows console draws.  Nothing
here can stand in for reading back a real console.

Because the SWI-Prolog prompt includes the command number ("101 ?- "
rather than just "?- "), column assertions are expressed relative to
the prompt's width on that fresh line.  Each test captures the
prompt position at the start (the cursor's coordinates right after
wait_for_prompt/1) and phrases its expected columns as "P + N" where
N is the visual-column offset inside the input.

Run with:

    swipl -g test_terminal -t halt packages/xpce/tests/test_terminal.pl

or, for a single backend:

    swipl -g 'test_terminal(child(ansi))' -t halt packages/xpce/tests/test_terminal.pl
*/

:- use_module(library(debug)).

setup_headless :-
    debugging(xpce(gui)),
    !.
setup_headless :-
    set_prolog_flag('SDL_VIDEODRIVER', dummy).

:- initialization(setup_headless, now).

:- use_module(library(plunit)).
:- use_module(library(pce)).
:- use_module(library(epilog)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(option)).
:- use_module(library(random)).
:- use_module(library(aggregate)).
:- use_module(library(process)).
:- if(exists_source(library(win_console))).
:- use_module(library(win_console)).
:- endif.

test_terminal :-
    test_terminal(epilog).

%!  test_terminal(+Backend) is semidet.
%
%   Run the suite against Backend.  Units that need a capability the
%   backend lacks (mouse, selection, ...) are skipped rather than
%   failed; see term_capability/2.

test_terminal(Backend) :-
    findall(Unit, terminal_test_unit(Unit), Units),
    run_units(Backend, Units).

%!  test_terminal(+Backend, +Unit) is semidet.
%
%   Run one unit.  The build runs the suite this way, a process per unit
%   and per backend: the units are independent, and xpce is not -- it has
%   one thread that may touch an object, so two units cannot share a
%   process.  See tests/CMakeLists.txt.

test_terminal(Backend, Unit) :-
    run_units(Backend, [Unit]).

run_units(Backend, Units) :-
    ensure_terminfo(Backend),
    setup_call_cleanup(
        nb_setval(terminal_backend, Backend),
        run_tests(Units),
        nb_delete(terminal_backend)).

%!  terminal_test_unit(?Unit) is nondet.
%
%   The units of this suite, in one place.  tests/CMakeLists.txt reads
%   them from here, so adding a unit below is enough to have the build
%   run it; there is no second list to keep in step.  A unit the backend
%   cannot support skips itself, see term_capability/2.

terminal_test_unit(terminal_basic).
terminal_test_unit(terminal_screen).
terminal_test_unit(terminal_nfd).
terminal_test_unit(terminal_regression).
terminal_test_unit(terminal_wide).
terminal_test_unit(terminal_non_bmp).
terminal_test_unit(terminal_mixed).
terminal_test_unit(terminal_background).
terminal_test_unit(terminal_mouse).
terminal_test_unit(terminal_wheel).
terminal_test_unit(terminal_alt_scroll).
terminal_test_unit(terminal_mouse_reports).
terminal_test_unit(terminal_wrap).
terminal_test_unit(terminal_resize).
terminal_test_unit(terminal_control_keys).
terminal_test_unit(terminal_child_on_terminal).

%!  current_backend(-Backend) is det.
%
%   The backend the current run uses.  Defaults to `epilog` so that
%   running a single unit by hand needs no set-up.

current_backend(Backend) :-
    (   nb_current(terminal_backend, B)
    ->  Backend = B
    ;   Backend = epilog
    ).


		 /*******************************
		 *           BACKENDS           *
		 *******************************/

%   A terminal under test is a handle
%
%       terminal(Backend, Screen)
%
%   Backend says what produces the output:
%
%       epilog          - a Prolog thread in this process.  libedit is
%                         wrapped around the terminal's streams with
%                         the EPILOG flag set.
%       child(Profile)  - a child `swipl` running on the terminal's
%                         pty, started with shell/1 from the epilog
%                         thread.  libedit sees an ordinary terminal
%                         and takes its capabilities from TERM, which
%                         term_profile_term/2 derives from Profile.
%
%   Screen says what we read back:
%
%       xpce(Frame, TerminalImage)
%
%   Everything below this section goes through the term_* primitives;
%   xpce methods on the terminal appear only in this section.

%!  term_start(+Backend, -T) is det.
%!  term_stop(+T) is det.

term_start(epilog, terminal(epilog, xpce(Frame, TI))) :-
    !,
    epilog_screen(Frame, TI).
term_start(child(Profile), terminal(child(Profile), xpce(Frame, TI))) :-
    !,
    ensure_terminfo(child(Profile)),
    epilog_screen(Frame, TI),
    T0 = terminal(epilog, xpce(Frame, TI)),
    wait_for_prompt(T0),
    start_child(T0, Profile).
term_start(console, T) :-
    T = terminal(console, console),
    start_console(T).

epilog_screen(Frame, TI) :-
    epilog([ object(Frame),
             title('test_terminal'),
             rows(25),
             cols(80)
           ]),
    get(Frame, current_terminal, TI).

term_stop(terminal(console, console)) :-
    !,
    catch(win_console_close, _, true).
term_stop(terminal(Backend, xpce(Frame, TI))) :-
    (   Backend = child(_)
    ->  ignore(stop_child(terminal(Backend, xpce(Frame, TI))))
    ;   true
    ),
    %  Known wart: a terminal whose thread hosted an interactive child
    %  keeps that thread running after ->destroy, so a child(_) run
    %  ends with a list of console threads that "wouldn't die".  The
    %  threads are idle and the run is unaffected; waiting for them
    %  here only made the suite three times slower.
    (   object(Frame)
    ->  in_pce_thread(send(Frame, destroy))
    ;   true
    ).

%!  term_send(+T, +Text) is det.
%
%   Inject Text into the terminal as if typed.

term_send(terminal(_, console), Text) :-
    !,
    win_console_send(Text).
term_send(terminal(_, xpce(_, TI)), Text) :-
    send(TI, send, Text).

%!  term_output(+T, +Text) is det.
%
%   Write Text to the screen as a program running on the terminal
%   would, escape sequences and all.  Unlike term_send/2 this does not
%   go past the line editor: it tests what the screen makes of a
%   sequence, not what libedit does with it.

term_output(terminal(_, xpce(_, TI)), Text) :-
    send(TI, insert, Text).

%!  term_typed(+T, +Id, +Buttons) is det.
%!  term_press(+T, +Code) is det.
%!  term_type_keys(+T, +Text) is det.
%!  term_key_press(+T, +Key) is det.
%
%   Press a key at the window.  term_send/2 puts bytes on the terminal
%   instead, which skips both the key bindings and whatever ->typed makes
%   of the key -- Return is a CR there and a newline in a byte stream --
%   so only these say what a client really receives.
%
%   term_press/2 holds Control down, Code being the control character
%   that produces; it is BUTTON_control that makes the table look up
%   \C-<key> rather than <key>, see characterName() in
%   src/ker/goodies.c.  term_type_keys/2 types text a character at a
%   time, term_key_press/2 presses a named key such as 'RET'.

term_typed(terminal(_, xpce(Frame, TI)), Id, Buttons) :-
    send(TI, typed, new(event(Id, Frame, @default, @default, Buttons))).

term_press(T, Code) :-
    button_control(Control),
    term_typed(T, Code, Control).

term_type_keys(T, Text) :-
    atom_codes(Text, Codes),
    forall(member(Code, Codes),
           term_typed(T, Code, 0)).

term_key_press(T, Key) :-
    term_typed(T, Key, 0).

button_control(0x1).			% BUTTON_control, src/h/graphics.h
button_shift(0x2).			% BUTTON_shift, idem

%!  term_foreground_process(+T, -PID) is semidet.
%
%   Process group of the process running in the terminal, if any.

term_foreground_process(terminal(_, xpce(_, TI)), PID) :-
    get(TI, foreground_process, PID).

%!  term_cursor(+T, -Col, -Row) is det.
%
%   Read the logical cursor position: Col is a *visual* column, Row is
%   0-based from the top of the visible window.

term_cursor(terminal(_, console), Col, Row) :-
    !,
    win_console_cursor(Col, Row).
term_cursor(terminal(_, xpce(_, TI)), Col, Row) :-
    get(TI, cursor_position, P),
    get(P, x, Col),
    get(P, y, Row).

%!  term_row(+T, +Row, -Atom) is det.
%
%   Content of visible row Row.  Rows past the end of the screen model
%   read as '' rather than failing: a display bug should surface as a
%   mismatched row, not as a helper that quietly fails.

term_row(terminal(_, console), Row, Atom) :-
    !,
    win_console_row(Row, Atom).
term_row(terminal(_, xpce(_, TI)), Row, Atom) :-
    (   get(TI, row, Row, Str)
    ->  get(Str, value, Atom)
    ;   Atom = ''
    ).

%!  term_cols(+T, -Cols) is det.
%
%   Current width of the terminal in columns.

term_cols(terminal(_, console), Cols) :-
    !,
    win_console_size(Cols, _).
term_cols(terminal(_, xpce(_, TI)), Cols) :-
    get(TI, columns, Cols).

%!  term_rows(+T, -Rows) is det.
%
%   Height of the visible window in rows.

term_rows(terminal(_, console), Rows) :-
    !,
    win_console_size(_, Rows).
term_rows(terminal(_, xpce(_, TI)), Rows) :-
    get(TI, rows, Rows).

%!  term_resize(+T, +WantCols, -GotCols) is det.
%
%   Resize the terminal to WantCols columns.  GotCols is the width
%   actually achieved, which may differ: the xpce terminal derives its
%   column count from pixels and the font's cell width, so only
%   certain widths are reachable.

term_resize(T, WantCols, GotCols) :-
    T = terminal(_, console),
    !,
    term_rows(T, Rows),
    win_console_resize(WantCols, Rows),
    drive(0.2),
    term_cols(T, GotCols).
term_resize(T, WantCols, GotCols) :-
    T = terminal(_, xpce(_, TI)),
    xpce_cw(TI, CW),
    Pixels is round((WantCols + 2) * CW),
    send(TI, width, Pixels),
    drive(0.2),
    term_cols(T, GotCols).

%!  xpce_cw(+TerminalImage, -CW) is det.
%
%   Pixel width of one character cell.  The terminal keeps one cell of
%   margin on either side, hence the +2 (see rlc_resize_pixel_units in
%   packages/xpce/src/txt/terminal.c).

xpce_cw(TI, CW) :-
    get(TI, width, W),
    get(TI, columns, Cols),
    CW is W/(Cols+2).

%!  term_click(+T, +Col, +Row) is det.
%!  term_drag(+T, +Col1, +Row1, +Col2, +Row2) is det.
%
%   Synthesise a left-button click, and a press-move-release.

term_click(T, Col, Row) :-
    term_click(T, Col, Row, 0).

term_drag(T, Col1, Row1, Col2, Row2) :-
    term_drag(T, Col1, Row1, Col2, Row2, 0).

%!  term_click(+T, +Col, +Row, +Buttons) is det.
%!  term_drag(+T, +Col1, +Row1, +Col2, +Row2, +Buttons) is det.
%!  term_move(+T, +Col, +Row) is det.
%
%   As above, with the modifier mask spelled out, and a bare motion of
%   the pointer.

term_click(terminal(_, xpce(_, TI)), Col, Row, Buttons) :-
    cell_pixel(TI, Col, Row, X, Y),
    send(TI, event, new(_, event(ms_left_down, TI, X, Y, Buttons, 0))),
    drive(0.1),
    send(TI, event, new(_, event(ms_left_up, TI, X, Y, Buttons, 0))),
    drive(0.3).

term_drag(terminal(_, xpce(_, TI)), Col1, Row1, Col2, Row2, Buttons) :-
    cell_pixel(TI, Col1, Row1, X1, Y1),
    cell_pixel(TI, Col2, Row2, X2, Y2),
    send(TI, event, new(_, event(ms_left_down, TI, X1, Y1, Buttons, 0))),
    drive(0.1),
    send(TI, event, new(_, event(ms_left_drag, TI, X2, Y2, Buttons, 0))),
    drive(0.1),
    send(TI, event, new(_, event(ms_left_up, TI, X2, Y2, Buttons, 0))),
    drive(0.3).

%   A terminal that does not report the mouse has nothing to do with a
%   bare motion and says so by failing the event, which is not the
%   helper's business.

term_move(terminal(_, xpce(_, TI)), Col, Row) :-
    cell_pixel(TI, Col, Row, X, Y),
    ignore(send(TI, event, new(_, event(loc_move, TI, X, Y, 0, 0)))),
    drive(0.1).

%!  term_wheel(+T, +Col, +Row, +Ticks, +Buttons) is det.
%
%   Turn the wheel Ticks notches over cell (Col,Row), positive being
%   away from the user.  Buttons is the modifier mask.  Unlike a button
%   event, a wheel event carries how far it turned as an attribute
%   rather than as an initialisation argument, 15 degrees to the notch;
%   see mapWheelMouseEvent() in packages/xpce/src/evt/event.c.

term_wheel(terminal(_, xpce(_, TI)), Col, Row, Ticks, Buttons) :-
    cell_pixel(TI, Col, Row, X, Y),
    Rotation is Ticks*15,
    new(Ev, event(wheel, TI, X, Y, Buttons, 0)),
    send(Ev, attribute, rotation, Rotation),
    send(TI, event, Ev),
    drive(0.2).

%!  cell_pixel(+TerminalImage, +Col, +Row, -X, -Y) is det.
%
%   Pixel in the middle of a character cell.

cell_pixel(TI, Col, Row, X, Y) :-
    get(TI, height, H),
    get(TI, rows, Rows),
    xpce_cw(TI, CW),
    CH is H/Rows,
    X is integer(CW*(Col+1) + CW/2),
    Y is integer(CH*Row + CH/2).

%!  term_bubble(+T, -Length, -Start, -View) is semidet.
%
%   What the terminal tells its scroll bar: the total number of lines,
%   where the visible part starts and how long it is.  Fails if the
%   terminal has no scroll bar.

term_bubble(terminal(_, xpce(_, TI)), Length, Start, View) :-
    get(TI, scroll_bar, SB),
    SB \== @nil,
    send(TI, bubble_scroll_bar, SB),
    get(SB, length, Length),
    get(SB, start, Start),
    get(SB, view, View).

%!  term_select_all(+T) is det.
%!  term_selection(+T, -Atom) is det.
%!  term_has_selection(+T) is semidet.

term_select_all(terminal(_, xpce(_, TI))) :-
    send(TI, select_all).

term_selection(terminal(_, xpce(_, TI)), Atom) :-
    get(TI, selected, Sel),
    (   Sel == @nil
    ->  Atom = ''
    ;   get(Sel, value, Atom)
    ).

term_has_selection(terminal(_, xpce(_, TI))) :-
    send(TI, has_selection).

%!  term_capability(+T, ?Cap) is nondet.
%
%   True when the backend supports Cap:
%
%       mouse       - term_click/3 and term_drag/5 work
%       selection   - the terminal maintains a selection
%       combining   - the screen model can hold combining marks
%       non_bmp     - ... and characters outside the BMP
%       margin_past_last_column
%                   - a caret waiting to wrap is reported one column
%                     past the last, rather than on it
%       wcwidth_font  - the line editor and the screen agree on column
%                     widths because both ask the terminal's font
%       program_output
%                   - term_output/2 works, i.e. we can write to the
%                     screen without going through the line editor
%       pty_signals - the window runs its client on a pty whose
%                     foreground process group it can see, so a process
%                     started in it gets the control keys
%       child_on_terminal
%                   - a process the Prolog thread starts runs on this
%                     terminal: it writes to the screen and reads what
%                     is typed at the window

term_capability(terminal(Backend, _), Cap) :-
    backend_capability(Backend, Cap).

%!  magic_margins is semidet.
%
%   True when the line editor believes the terminal defers its wrap,
%   i.e. when the terminal description it reads has `xenl'.  A caret
%   the terminal's own reflow left on the right margin is the one thing
%   the redisplay still predicts from the description rather than
%   settling itself, so a test of that prediction only makes sense
%   where the description and the terminal agree.  The terminal is
%   always the xpce one, which does defer its wrap.

magic_margins :-
    current_backend(Backend),
    Backend \== console,       % a console wraps as the column is written
    (   Backend = child(Profile)
    ->  term_profile_term(Profile, TERM)
    ;   getenv('TERM', TERM)
    ),
    terminfo_flag(TERM, xenl).

backend_capability(epilog,   mouse).
backend_capability(epilog,   selection).
backend_capability(epilog,   combining).
backend_capability(epilog,   wcwidth_font).
backend_capability(epilog,   non_bmp).
backend_capability(epilog,   margin_past_last_column).
backend_capability(epilog,   program_output).
backend_capability(child(_), mouse).
backend_capability(child(_), selection).
backend_capability(child(_), combining).
backend_capability(child(_), non_bmp).
backend_capability(child(_), margin_past_last_column).
%  `pty_signals' is Unix-only: a Windows pseudo console has no
%  foreground process group to ask about.  It is epilog-only for a
%  different reason: a child backend would run the tests against a
%  grandchild, which is the same code path at three times the cost.

backend_capability(epilog, pty_signals) :-
    \+ current_prolog_flag(windows, true).

%  `child_on_terminal' holds on both platforms, but for different
%  reasons: POSIX hands the child the pty, Windows puts it on the
%  terminal's pseudo console.  Epilog only, as under a child backend
%  shell/1 would start a grandchild.
%
%  Not under Wine, which answers S_OK for a pseudo console and then does
%  not put the child on it: its output goes to whatever console the
%  process already had and nothing arrives on the handles passed in.

backend_capability(epilog, child_on_terminal) :-
    \+ current_prolog_flag(wine_version, _).

%  `program_output' is epilog-only although the screen is the same
%  object under a child: there the child owns the screen, so writing
%  behind its back races with its own redisplay.
%
%  The console has no mouse or selection we can drive, and a cell holds
%  one UTF-16 unit, so a base and its combining marks cannot both be
%  there to read back.  Nothing about `combining' is skipped because it
%  is hard; it cannot be represented.

%!  needs(+Caps) is semidet.
%
%   plunit condition: true when the backend of the current run has all
%   of Caps.  Used as `condition(needs([mouse]))` so a unit is skipped
%   rather than failed on a backend that cannot support it.

needs(Caps) :-
    current_backend(Backend),
    forall(member(Cap, Caps), backend_capability(Backend, Cap)).


		 /*******************************
		 *         CHILD BACKEND        *
		 *******************************/

%!  term_profile_term(+Profile, -TERM) is det.
%
%   TERM setting for a capability profile.  A profile names the
%   terminal description libedit will read, which is what decides
%   which redisplay strategy it uses.  Most profiles are simply the
%   name of a stock description; `winconsole' is one we bring along.

term_profile_term(winconsole, 'swipl-winconsole') :-
    !.
term_profile_term(Profile, Profile).

%!  profile_terminfo_source(+Profile, -File) is semidet.
%
%   Terminal description this suite ships for Profile.  There is one:
%   swipl.exe on a Windows console reads no description at all -- it
%   links the fake termcap in packages/libedit/libedit/src/win_ncurses.c
%   -- so to put libedit through the same decisions on a Unix pty we
%   have to hand it that table as a description of its own.

profile_terminfo_source(winconsole, File) :-
    source_file(term_profile_term(_,_), Here),
    file_directory_name(Here, Dir),
    directory_file_path(Dir, 'swipl-winconsole.ti', File).

%!  ensure_terminfo(+Backend) is det.
%
%   Compile the description a profile brings along, and point TERMINFO
%   at it.  Setting it in this process rather than only in the child's
%   command line means tput sees it too, so terminfo_string/3 and
%   magic_margins/0 answer for the same description libedit reads.
%   ncurses searches $TERMINFO first and the system database after, so
%   the stock profiles keep working.

:- dynamic terminfo_compiled/1.

ensure_terminfo(Backend) :-
    (   Backend = child(Profile),
        profile_terminfo_source(Profile, Source)
    ->  (   terminfo_compiled(Profile)
        ->  true
        ;   terminfo_scratch_dir(Dir),
            compile_terminfo(Source, Dir),
            setenv('TERMINFO', Dir),
            assertz(terminfo_compiled(Profile))
        )
    ;   true
    ).

terminfo_scratch_dir(Dir) :-
    tmp_file(terminfo, Base),
    atom_concat(Base, '.d', Dir),
    make_directory(Dir).

compile_terminfo(Source, Dir) :-
    process_create(path(tic), ['-o', file(Dir), file(Source)],
                   [ stdout(null),
                     stderr(null),
                     process(PID)
                   ]),
    process_wait(PID, Status),
    (   Status == exit(0)
    ->  true
    ;   throw(error(terminfo_compile_failed(Source, Status), _))
    ).

%!  child_done_marker(-Marker) is det.
%
%   Text the epilog thread prints once shell/1 has returned, i.e. once
%   the child is really gone.  Waiting for "a prompt" instead would
%   return at once, because the child's own prompt is still on screen;
%   we would then tear the terminal down with its thread still inside
%   shell/1.

child_done_marker('<<child-exited>>').

%!  start_child(+T, +Profile) is det.
%
%   Start a child `swipl` on the terminal's pty by asking the epilog
%   thread to run shell/1.  System() (src/os/pl-os.c) dups the calling
%   thread's user streams onto the child's 0/1/2, and in an epilog
%   thread those are the terminal's pty, so the child gets the
%   terminal.  TERM is set on the command line, so it applies to the
%   child only.
%
%   After the child's prompt appears we clear the screen, so the child
%   starts from row 0 exactly like the epilog backend does.

start_child(T, Profile) :-
    term_profile_term(Profile, TERM),
    current_prolog_flag(executable, Exe),
    child_done_marker(Marker),
    term_cursor(T, _, ParentRow),
    format(atom(Cmd),
           'shell("TERM=~w \'~w\' -q"), format("~~n~w~~n").\n',
           [TERM, Exe, Marker]),
    term_send(T, Cmd),
    (   wait_until(child_started(T, ParentRow), 30)
    ->  true
    ;   throw(error(terminal_child_failed(Profile), _))
    ),
    key(T, ctrl_l),
    wait_for_prompt(T).

%!  child_started(+T, +ParentRow) is semidet.
%
%   True once the child's own prompt is on screen.  The prompt we typed
%   the shell/1 goal into is still there, so wait for a prompt *below*
%   it rather than for "a prompt".

child_started(T, ParentRow) :-
    term_cursor(T, _, Row),
    Row > ParentRow,
    at_prompt(T).

%!  stop_child(+T) is semidet.
%
%   Halt the child and wait until the epilog thread reports that
%   shell/1 has returned.  Fails if the child does not go away, in
%   which case the terminal is torn down regardless -- there is
%   nothing better to do at cleanup time.

stop_child(T) :-
    child_done_marker(Marker),
    catch(( key(T, ctrl_e),           % whatever the test left on the
            key(T, ctrl_u),           % line must not swallow the halt
            term_send(T, 'halt.\n'),
            wait_until(marker_on_screen(T, Marker), 15)
          ), _, fail).

marker_on_screen(T, Marker) :-
    term_rows(T, Rows),
    between(0, Rows, Row),
    term_row(T, Row, Line),
    sub_atom(Line, _, _, _, Marker),
    !.


		 /*******************************
		 *        CONSOLE BACKEND       *
		 *******************************/

%!  start_console(+T) is det.
%
%   Open a Windows console, put a `swipl' on it and wait for its
%   prompt.  Unlike the other backends this one reads the screen the
%   user would be looking at -- conhost's own buffer -- rather than a
%   terminal of ours, so it is the only one that can show what the
%   console makes of what libedit writes.
%
%   The suite's own output must not be on that console; see
%   win_console_open/2.

start_console(T) :-
    current_prolog_flag(executable, Exe),
    %  A process has one console, so a test that wants a terminal of its
    %  own gets this one back rather than a second.
    catch(win_console_close, _, true),
    win_console_open(80, 25),
    format(atom(Cmd), '"~w" -q', [Exe]),
    win_console_spawn(Cmd),
    (   wait_until(at_prompt(T), 30)
    ->  true
    ;   throw(error(terminal_console_failed(Cmd), _))
    ),
    check_console_interprets_escapes(T),
    key(T, ctrl_l),
    wait_for_prompt(T).

%!  check_console_interprets_escapes(+T) is det.
%
%   The line editor writes cursor motion as escape sequences and hands
%   them to WriteConsole().  A console acts on those only with
%   ENABLE_VIRTUAL_TERMINAL_PROCESSING set, which the line editor turns
%   on for itself as it starts up.  Where that does not take, the
%   sequences land in the screen buffer as text.
%
%   Test the screen rather than the mode word: wine's console accepts
%   the flag and ignores it, so the flag says everything is fine while
%   every row reads back full of escapes.  An ESC anywhere on screen
%   means no assertion about content can hold, so say that once instead
%   of letting it look like forty failures.

check_console_interprets_escapes(T) :-
    term_rows(T, Rows),
    Last is Rows-1,
    (   between(0, Last, Row),
        term_row(T, Row, Line),
        sub_atom(Line, _, 1, _, '\e')
    ->  win_console_mode(_In, Out),
        throw(error(console_does_not_interpret_escapes(Row, Line, Out), _))
    ;   true
    ).


		 /*******************************
		 *       SETUP / TEARDOWN       *
		 *******************************/

%   We create one terminal per test UNIT (via begin_tests/2's setup
%   and cleanup options) and reuse it across the tests of the unit.
%   Each test's own setup calls reset_input/1 to clear whatever the
%   previous test left on the command line, so tests see a fresh empty
%   prompt without the overhead of spawning a new terminal.

%!  start_terminal(-Terminal) is det.
%
%   Open a terminal for the current backend and wait for the initial
%   prompt.

start_terminal(Terminal) :-
    current_backend(Backend),
    term_start(Backend, Terminal),
    wait_for_prompt(Terminal),
    wait_settled(Terminal).

%!  wait_settled(+Terminal) is det.
%
%   Wait until the screen stops changing.  Waiting for the prompt is not
%   enough: it is drawn while the rest of the banner is still on its way,
%   and on a machine running several of these at once the rest can arrive
%   after a test has painted the screen and overwrite what it painted.
%
%   Polls rather than sleeps, so it costs what it has to and no more, and
%   waits longer where waiting is needed.  Gives up rather than failing:
%   a terminal that never goes quiet is the test's problem to report.

wait_settled(Terminal) :-
    screen_signature(Terminal, Sig),
    wait_settled(Terminal, Sig, 0, 0).

wait_settled(_Terminal, _Prev, Still, _Polls) :-
    Still >= 5,                         % nothing moved for five rounds
    !.
wait_settled(_Terminal, _Prev, _Still, Polls) :-
    Polls >= 500,                       % ... or it never stops
    !.
wait_settled(Terminal, Prev, Still, Polls) :-
    wait(0.01),
    screen_signature(Terminal, Sig),
    (   Sig == Prev
    ->  Still1 is Still+1
    ;   Still1 = 0
    ),
    Polls1 is Polls+1,
    wait_settled(Terminal, Sig, Still1, Polls1).

screen_signature(Terminal, Rows) :-
    term_rows(Terminal, N),
    Last is N-1,
    findall(Row,
            ( between(0, Last, I),
              term_row(Terminal, I, Row)
            ),
            Rows).

%!  stop_terminal(+Terminal) is det.

stop_terminal(Terminal) :-
    term_stop(Terminal).

%!  setup_unit is det.
%!  cleanup_unit is det.
%
%   Unit-level hooks: open/close the terminal shared by all tests in a
%   PLUnit unit.  The handle is stashed in a non-backtrackable global
%   so individual tests can retrieve it through
%   current_test_terminal/1.

setup_unit :-
    start_terminal(Terminal),
    nb_setval(terminal_test, Terminal).

cleanup_unit :-
    (   nb_current(terminal_test, Terminal)
    ->  nb_delete(terminal_test),
        stop_terminal(Terminal)
    ;   true
    ).

%!  current_test_terminal(-Terminal) is det.

current_test_terminal(Terminal) :-
    nb_getval(terminal_test, Terminal).

%!  test_begin(-Terminal) is det.
%
%   Per-test setup: retrieve the shared terminal and reset its input
%   line so the test starts at a clean empty prompt.

test_begin(Terminal) :-
    current_test_terminal(Terminal),
    reset_input(Terminal).

%!  reset_input(+Terminal) is det.
%
%   Clear the current libedit input line and redraw the prompt.  We
%   go to end-of-line first so ^U (kill-to-start) removes whatever is
%   in front of the cursor regardless of where the previous test
%   left it; ^L then triggers a fresh redisplay.

reset_input(Terminal) :-
    key(Terminal, ctrl_e),
    key(Terminal, ctrl_u),
    key(Terminal, ctrl_l),
    wait_for_prompt(Terminal).

%!  rows_above(+Terminal, +N) is semidet.
%
%   Make sure the prompt has at least N rows above it by running
%   queries until it has moved far enough down.  How far down a
%   terminal starts out differs per backend, so a test that needs room
%   above the input line asks for it rather than assuming it.
%
%   Submitting an empty line would not do: the reader wants a term, so
%   it answers with the continuation prompt rather than a new query.

rows_above(Terminal, N) :-
    rows_above(Terminal, N, N).

rows_above(Terminal, N, Tries) :-
    cursor(Terminal, _, Row),
    (   Row >= N
    ->  true
    ;   Tries > 0,
        type(Terminal, 'true.'),
        key(Terminal, enter),
        wait_for_prompt(Terminal),
        Tries1 is Tries - 1,
        rows_above(Terminal, N, Tries1)
    ).


		 /*******************************
		 *          SHORTHANDS          *
		 *******************************/

%!  cursor(+Terminal, -Col, -Row) is det.
%!  row_text(+Terminal, +Row, -Atom) is det.
%
%   Shorthands for the two readback primitives, kept because the tests
%   below use them on nearly every line.

cursor(Terminal, Col, Row) :-
    term_cursor(Terminal, Col, Row).

row_text(Terminal, Row, Atom) :-
    term_row(Terminal, Row, Atom).

%!  out(+Terminal, +Text) is det.
%
%   Write to the screen as a program running on the terminal would,
%   escape sequences and all, and let the screen settle.  Text may be a
%   list of atomics, which saves the tests a format/3 to glue a
%   sequence and its payload together.

out(T, Parts) :-
    is_list(Parts),
    !,
    atomic_list_concat(Parts, Text),
    out(T, Text).
out(T, Text) :-
    term_output(T, Text),
    drive(0.05).

%!  alt_screen(+T, +Text) is det.
%!  normal_screen(+T) is det.
%
%   Enter the alternate screen (DEC private mode 1049) with Text on its
%   top row, and leave it again.

alt_screen(T, Text) :-
    out(T, ['\e[?1049h\e[H', Text]).

normal_screen(T) :-
    out(T, '\e[?1049l').


		 /*******************************
		 *       SYNCHRONISATION        *
		 *******************************/

%!  drive(+Seconds) is det.
%
%   Pump the xpce event loop for the given wall-clock time.  Every
%   helper that needs output to propagate into the terminal calls
%   this, so there is exactly one place that touches pce_dispatch/2.

drive(Seconds) :-
    get_time(Start),
    Deadline is Start + Seconds,
    drive_until(Deadline).

drive_until(Deadline) :-
    get_time(Now),
    (   Now >= Deadline
    ->  true
    ;   Remaining is max(0.01, Deadline-Now),
        Slice is min(0.05, Remaining),
        wait(Slice),
        drive_until(Deadline)
    ).

%!  wait_until(:Goal, +Timeout) is semidet.
%
%   Repeatedly dispatch events and test Goal until it succeeds or
%   Timeout seconds have elapsed.  Fails (does not throw) on timeout
%   so a PLUnit test reports a missed expectation instead of hanging.

:- meta_predicate wait_until(0, +).
wait_until(Goal, Timeout) :-
    get_time(Start),
    Deadline is Start + Timeout,
    wait_until_(Goal, Deadline).

wait_until_(Goal, Deadline) :-
    (   call(Goal)
    ->  true
    ;   get_time(Now),
        Now < Deadline,
        wait(0.05),
        wait_until_(Goal, Deadline)
    ).

%!  wait(+Time) is det.
%
%   Wait for an xpce event while dispatching input.

wait(Time) :-
    (   current_backend(console)
    ->  sleep(Time)
    ;   pce_principal:pce_dispatch(-1, Time)
    ).

%!  wait_for_prompt(+Terminal) is semidet.
%
%   Wait until the row the cursor is on ends with the SWI-Prolog
%   prompt string "?- ".  That is true as soon as Prolog reaches its
%   top-level and libedit has drawn its prompt.

wait_for_prompt(Terminal) :-
    wait_until(at_prompt(Terminal), 15).

at_prompt(Terminal) :-
    cursor(Terminal, _, Row),
    row_text(Terminal, Row, Line),
    atom(Line),
    (   sub_atom(Line, _, _, 0, '?- ')
    ->  true
    ;   %  A console pads every row to the full width, so the space
        %  after the prompt cannot be told from the padding and is
        %  trimmed away with it.
        sub_atom(Line, _, _, 0, '?-')
    ).

%!  prompt_col(+Terminal, -Col) is det.
%
%   The column immediately after the prompt on the current input row
%   (equal to the cursor column right after wait_for_prompt/1).

prompt_col(Terminal, Col) :-
    cursor(Terminal, Col, _).


		 /*******************************
		 *          INPUT HELPERS       *
		 *******************************/

%!  type(+Terminal, +Text) is det.
%
%   Inject Text as if typed.  Drives the event loop briefly so the
%   terminal has a chance to echo.  Text may be an atom or a string.

type(Terminal, Text) :-
    term_send(Terminal, Text),
    drive(0.1).

%!  key(+Terminal, +Name) is det.
%
%   Send a symbolic key.

key(terminal(console, _), Name) :-
    !,
    console_key(Name),
    drive(0.05).
key(Terminal, Name) :-
    key_bytes(Terminal, Name, Bytes),
    atom_codes(Atom, Bytes),
    term_send(Terminal, Atom),
    drive(0.05).

%!  console_key(+Name) is det.
%
%   Press a key on a Windows console.  The editor bindings are control
%   characters and go as themselves; the rest are keys the console
%   reports as key codes, and letting it turn those into the escape
%   sequence the editor reads is part of what a console run is for.

console_key(Name) :-
    editor_key_bytes(Name, Bytes),
    !,
    atom_codes(Atom, Bytes),
    win_console_send(Atom).
console_key(Name) :-
    win_console_key(Name).

%!  key_bytes(+Terminal, +Name, -Bytes) is det.
%
%   Byte sequence for a symbolic key.  Control and Meta keys are
%   bindings inside the line editor and mean the same on every
%   terminal.  The cursor and editing keys belong to the terminal, and
%   the terminal here is always the xpce one, which sends the VT
%   sequences below (see typedTerminalImage() in
%   packages/xpce/src/txt/terminal.c).
%
%   Whether the line editor makes anything of them is another matter:
%   it binds the keys its terminal description gives it, so on a
%   description that has no Delete key at all -- `ansi' and `vt100'
%   have none -- ESC [ 3 ~ is not a key press but four characters to
%   type.  Drive those operations through the editor binding that does
%   the same thing instead, so a test of the redisplay does not fail
%   over a key the terminal cannot report.

key_bytes(_Terminal, Name, Bytes) :-
    editor_key_bytes(Name, Bytes),
    !.
key_bytes(Terminal, Name, Bytes) :-
    terminal_key(Name, Cap, Sequence, Fallback),
    (   term_terminfo(Terminal, TERM),
        terminfo_string(TERM, Cap, _)
    ->  Bytes = Sequence
    ;   editor_key_bytes(Fallback, Bytes)
    ).

% Line editor bindings: the same bytes everywhere ------------------------
editor_key_bytes(ctrl_a,         [0x01]).
editor_key_bytes(ctrl_b,         [0x02]).
editor_key_bytes(ctrl_d,         [0x04]).
editor_key_bytes(ctrl_e,         [0x05]).
editor_key_bytes(ctrl_f,         [0x06]).
editor_key_bytes(ctrl_k,         [0x0B]).
editor_key_bytes(ctrl_l,         [0x0C]).
editor_key_bytes(ctrl_n,         [0x0E]).
editor_key_bytes(ctrl_p,         [0x10]).
editor_key_bytes(ctrl_u,         [0x15]).
editor_key_bytes(backspace,      [0x7F]).   % libedit treats DEL as backspace
editor_key_bytes(enter,          [0'\r]).
editor_key_bytes(tab,            [0'\t]).
% Meta = ESC prefix on VT terminals
editor_key_bytes(meta_b,         [0'\e, 0'b]).
editor_key_bytes(meta_f,         [0'\e, 0'f]).
editor_key_bytes(meta_d,         [0'\e, 0'd]).
editor_key_bytes(meta_backspace, [0'\e, 0x7F]).

% Keys of the terminal itself: name, the terminfo capability that says
% whether the line editor knows this key, the bytes the xpce terminal
% sends for it, and the editor binding for the same operation to fall
% back on when it does not.
terminal_key(cursor_up,    kcuu1, [0'\e, 0'[, 0'A],       ctrl_p).
terminal_key(cursor_down,  kcud1, [0'\e, 0'[, 0'B],       ctrl_n).
terminal_key(cursor_right, kcuf1, [0'\e, 0'[, 0'C],       ctrl_f).
terminal_key(cursor_left,  kcub1, [0'\e, 0'[, 0'D],       ctrl_b).
terminal_key(home,         khome, [0'\e, 0'[, 0'H],       ctrl_a).
terminal_key(end,          kend,  [0'\e, 0'[, 0'F],       ctrl_e).
terminal_key(delete,       kdch1, [0'\e, 0'[, 0'3, 0'~],  ctrl_d).

%!  terminfo_string(+TERM, +Cap, -Bytes) is semidet.
%
%   Value of a terminfo string capability, or failure when TERM has no
%   such capability.  Asks tput, and remembers the answer -- including
%   "no such capability", recorded as (-) -- so each is looked up once.

:- dynamic terminfo_cache/3.                % TERM, Cap, Bytes or (-)

terminfo_string(TERM, Cap, Bytes) :-
    (   terminfo_cache(TERM, Cap, Cached)
    ->  true
    ;   (   catch(tput(TERM, Cap, Found), _, fail)
        ->  Cached = Found
        ;   Cached = (-)
        ),
        assertz(terminfo_cache(TERM, Cap, Cached))
    ),
    Cached \== (-),
    Bytes = Cached.

%!  terminfo_flag(+TERM, +Cap) is semidet.
%
%   True when TERM has the boolean capability Cap.  tput reports those
%   in its exit status rather than on standard output.

terminfo_flag(TERM, Cap) :-
    (   terminfo_cache(TERM, Cap, Cached)
    ->  true
    ;   (   catch(tput_status(TERM, Cap), _, fail)
        ->  Cached = true
        ;   Cached = (-)
        ),
        assertz(terminfo_cache(TERM, Cap, Cached))
    ),
    Cached == true.

tput_status(TERM, Cap) :-
    process_create(path(tput), ['-T', TERM, Cap],
                   [ stdout(null),
                     stderr(null),
                     process(PID)
                   ]),
    process_wait(PID, Status),
    Status == exit(0).

tput(TERM, Cap, Bytes) :-
    process_create(path(tput), ['-T', TERM, Cap],
                   [ stdout(pipe(Out)),
                     stderr(null),
                     process(PID)
                   ]),
    setup_call_cleanup(
        read_string(Out, _, String),
        process_wait(PID, Status),
        close(Out)),
    Status == exit(0),
    String \== "",
    string_codes(String, Bytes).

%!  term_terminfo(+Terminal, -TERM) is semidet.
%
%   Name of the terminal description the line editor on the other end
%   is reading.

term_terminfo(terminal(child(Profile), _), TERM) :-
    !,
    term_profile_term(Profile, TERM).
term_terminfo(terminal(epilog, _), TERM) :-
    getenv('TERM', TERM).                   % as fix_term/0 left it


		 /*******************************
		 *         MOUSE HELPERS        *
		 *******************************/

%!  click(+Terminal, +Col, +Row) is det.
%!  drag(+Terminal, +Col1, +Row1, +Col2, +Row2) is det.
%
%   Synthesise a left-button click, and a press-move-release.

click(T, Col, Row) :-
    term_click(T, Col, Row).

click(T, Col, Row, Buttons) :-
    term_click(T, Col, Row, Buttons).

drag(T, Col1, Row1, Col2, Row2) :-
    term_drag(T, Col1, Row1, Col2, Row2).

move(T, Col, Row) :-
    term_move(T, Col, Row).

%!  wheel(+T, +Col, +Row, +Ticks) is det.
%!  wheel(+T, +Col, +Row, +Ticks, +Buttons) is det.
%
%   Turn the wheel over a cell; positive Ticks is up (away from the
%   user), which scrolls back.

wheel(T, Col, Row, Ticks) :-
    wheel(T, Col, Row, Ticks, 0).

wheel(T, Col, Row, Ticks, Buttons) :-
    term_wheel(T, Col, Row, Ticks, Buttons).


		 /*******************************
		 *      FOREGROUND CHILD        *
		 *******************************/

%!  start_foreground(+T, +Command) is det.
%!  stop_foreground(+T) is det.
%
%   Run Command in the terminal with shell/1 and wait until it owns the
%   pty.  stop_foreground/1 gets rid of it again and is a no-op when
%   the test already did.

start_foreground(T, Command) :-
    format(atom(Goal), 'shell("~w").\n', [Command]),
    term_send(T, Goal),
    (   wait_until(term_foreground_process(T, _), 15)
    ->  true
    ;   throw(error(terminal_no_foreground_process(Command), _))
    ).

stop_foreground(T) :-
    (   term_foreground_process(T, _)
    ->  press(T, ctrl_c),
        wait_until(\+ term_foreground_process(T, _), 15)
    ;   true
    ),
    wait_for_prompt(T).

%!  press(+T, +Key) is det.
%
%   Press a control key at the window and let the terminal settle.

press(T, Key) :-
    control_code(Key, Code),
    term_press(T, Code),
    drive(0.2).

control_code(ctrl_c, 0x03).
control_code(ctrl_x, 0x18).

%!  echo_client(-Command) is det.
%!  client_reads(+T, +Expected) is semidet.
%
%   What the terminal sends its client is otherwise invisible: nothing
%   on this side sees the bytes and the line editor is not reading
%   them.  echo_client/1 is a command to run in the terminal that reads
%   them and prints them back with the escapes made visible, and
%   client_reads/2 asks it what it got by typing a `#' and Return: the
%   line discipline hands the line over and the row that comes back is
%   what the terminal sent, `#' and all.
%
%   Expected == '' therefore says the terminal sent nothing, which
%   waiting for something not to appear cannot: that can only time out.

echo_client('stty -echo; cat -v').

client_reads(T, Expected) :-
    atom_concat(Expected, '#', Line),
    term_type_keys(T, '#'),
    key(T, enter),
    wait_until(row_on_screen(T, Line), 15).

%!  row_on_screen(+T, +Text) is semidet.
%
%   True when a visible row holds exactly Text.  marker_on_screen/2
%   matches a substring, which cannot tell `#' from `^[[B#'.

row_on_screen(T, Text) :-
    term_rows(T, Rows),
    between(0, Rows, Row),
    term_row(T, Row, Line),
    Line == Text,
    !.


		 /*******************************
		 *         RESIZE HELPERS       *
		 *******************************/

%!  filler(+N, -Atom) is det.
%
%   Build an atom of N printable ASCII chars drawn from a repeating
%   "abcdefghijklmnopqrstuvwxyz_" pattern — 26 letters followed by
%   an underscore marker every 27 characters.  The underscores make
%   it easy to see at a glance where any byte-offset sits inside a
%   wrapped or shifted row: the N-th underscore marks column 27*(N+1).
%
%   Shared between the terminal_wrap and terminal_resize units.
%   Defined outside any `begin_tests/end_tests` block because plunit
%   scopes predicates declared inside a unit to a per-unit
%   plunit_<unit> module.

filler(N, Atom) :-
    length(Codes, N),
    fill_codes(Codes, 0),
    atom_codes(Atom, Codes).

fill_codes([], _).
fill_codes([C|T], I) :-
    Mod is I mod 27,
    (   Mod =:= 26
    ->  C = 0'_
    ;   C is 0'a + Mod
    ),
    I1 is I + 1,
    fill_codes(T, I1).

%!  resize_cols(+Terminal, +WantCols, -GotCols) is det.
%
%   Resize the terminal to WantCols columns and pump the event loop so
%   the resize-driven libedit refresh lands before we read rows.
%   GotCols is the width actually achieved; see term_resize/3.

resize_cols(Terminal, WantCols, GotCols) :-
    term_resize(Terminal, WantCols, GotCols).

%!  rows_of(+Terminal, +FromRow, +Count, -Atoms) is det.
%
%   Read Count consecutive rows starting at FromRow as a list of atoms.

rows_of(_Terminal, _FromRow, 0, []) :- !.
rows_of(Terminal, FromRow, Count, [Atom|Rest]) :-
    row_text(Terminal, FromRow, Atom),
    Next is FromRow + 1,
    Count1 is Count - 1,
    rows_of(Terminal, Next, Count1, Rest).


		 /*******************************
		 *          ASSERTIONS          *
		 *******************************/

%!  assert_cursor(+Terminal, +Col, +Row) is det.

assert_cursor(Terminal, ExpCol, ExpRow) :-
    cursor(Terminal, Col, Row),
    (   Col =:= ExpCol,
        Row =:= ExpRow
    ->  true
    ;   format(user_error,
               "cursor: expected (~w, ~w), got (~w, ~w)~n",
               [ExpCol, ExpRow, Col, Row]),
        report_cursor_row(Terminal, Row),
        dump_screen(Terminal, 'caret is not where it should be'),
        assertion((Col =:= ExpCol, Row =:= ExpRow))
    ).

%!  margin_col(+Terminal, -Col) is det.
%
%   The column the caret reports once a row has been filled to its right
%   edge and the wrap is still pending.
%
%   A terminal with delayed wrap parks the caret one past the last
%   column and says so: on an 80-column xpce terminal, 80.  A console
%   holds the same state but has no column 80 to name it with -- its
%   columns are 0..79 -- and reports the last one instead.  The state is
%   the same either way, so ask the terminal what it calls it rather
%   than writing one terminal's answer into the tests.

margin_col(Terminal, Col) :-
    term_cols(Terminal, Cols),
    (   term_capability(Terminal, margin_past_last_column)
    ->  Col = Cols
    ;   Col is Cols-1
    ).

%!  prompt_prefix(+Line, +Width, -Prompt) is det.
%
%   The first Width characters of Line, padded with spaces when Line is
%   shorter than that.  On a console the space after the prompt cannot
%   be told from the padding of an otherwise empty row and is trimmed
%   away with it, leaving a row one character shorter than the column
%   the caret is in.

prompt_prefix(Line, Width, Prompt) :-
    atom_length(Line, Len),
    (   Len >= Width
    ->  sub_atom(Line, 0, Width, _, Prompt)
    ;   Pad is Width - Len,
        length(Spaces, Pad),
        maplist(=(0' ), Spaces),
        atom_codes(Padding, Spaces),
        atom_concat(Line, Padding, Prompt)
    ).

%!  dump_screen(+Terminal, +Tag) is det.
%
%   Print every non-empty row.  For a failure that is about what is
%   *not* on the screen, the screen is the evidence.

dump_screen(Terminal, Tag) :-
    term_rows(Terminal, Rows),
    Last is Rows-1,
    format(user_error, "    screen (~w):~n", [Tag]),
    forall(( between(0, Last, Row),
             row_text(Terminal, Row, Line),
             Line \== ''
           ),
           format(user_error, "      ~w: ~q~n", [Row, Line])).

%!  report_cursor_row(+Terminal, +Row) is det.
%
%   Print the row the caret is on and how wide its content is.  Where a
%   caret lands at the right margin, what settles whether the terminal
%   or the line editor is at fault is whether the last column was
%   written at all.

report_cursor_row(Terminal, Row) :-
    term_cols(Terminal, Cols),
    row_text(Terminal, Row, Line),
    atom_length(Line, Len),
    format(user_error, "    row ~w holds ~w of ~w columns: ~q~n",
           [Row, Len, Cols, Line]).

%!  assert_row(+Terminal, +Row, +Expected) is det.
%
%   Exact match of the row content (atom-compared).

assert_row(Terminal, Row, Expected) :-
    row_text(Terminal, Row, Line),
    (   Line == Expected
    ->  true
    ;   format(user_error, "row ~w: expected ~q, got ~q~n",
               [Row, Expected, Line]),
        report_codes(Expected, Line),
        assertion(Line == Expected)
    ).

%!  report_codes(+Expected, +Got) is det.
%
%   Print both as code points as well.  A terminal that cannot draw the
%   characters under test draws both sides as the same row of question
%   marks, which says nothing about how they differ.

report_codes(Expected, Got) :-
    atom_codes(Expected, EC),
    atom_codes(Got, GC),
    (   EC == GC
    ->  true
    ;   format(user_error, "    expected codes: ~w~n", [EC]),
        format(user_error, "         got codes: ~w~n", [GC])
    ).

%!  assert_input(+Terminal, +Row, +ExpectedInput) is det.
%
%   Strip the SWI-Prolog prompt "N ?- " from the start of the row, then
%   compare the remainder with ExpectedInput.  Works for any command
%   number N.

assert_input(Terminal, Row, ExpectedInput) :-
    row_text(Terminal, Row, Line),
    strip_prompt(Line, Input),
    (   Input == ExpectedInput
    ->  true
    ;   format(user_error,
               "input row ~w: expected ~q, got ~q (full: ~q)~n",
               [Row, ExpectedInput, Input, Line]),
        report_codes(ExpectedInput, Input),
        dump_screen(Terminal, 'input row does not match'),
        assertion(Input == ExpectedInput)
    ).

%!  strip_prompt(+Line, -Rest) is det.
%
%   Drop everything up to and including the "?- " prompt.  If the
%   line does not contain a prompt we return it unchanged — useful
%   for rows that ended up empty.

strip_prompt(Line, Rest) :-
    (   sub_atom(Line, Before, 3, _, '?- ')
    ->  After is Before + 3,
        sub_atom(Line, After, _, 0, Rest)
    ;   %  On a console the space after the prompt cannot be told from
        %  the padding of an otherwise empty row, and goes with it.
        sub_atom(Line, Before, 2, 0, '?-')
    ->  After is Before + 2,
        sub_atom(Line, After, _, 0, Rest)
    ;   Rest = Line
    ).


		 /*******************************
		 *        TEST: BASIC           *
		 *******************************/

:- begin_tests(terminal_basic,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

test(type_ascii, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    type(T, abc),
    assert_input(T, R, abc),
    C is P + 3,
    assert_cursor(T, C, R).

test(home_end, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    type(T, hello),
    End is P + 5,
    assert_cursor(T, End, R),
    key(T, ctrl_a),
    assert_cursor(T, P, R),
    key(T, ctrl_e),
    assert_cursor(T, End, R).

test(program_clears_the_screen, [setup(test_begin(T))]) :-
    %  A program that clears the screen with ESC [ 2 J must have it
    %  cleared.  On Windows the standard streams are wrapped by an
    %  emulation of our own, src/pl-ntconsole.c, which acted on SGR and
    %  quietly ate every other sequence -- so this cleared nothing and
    %  wrote nothing either.
    %
    %  ESC [ 2 J alone, without the ESC [ H that usually goes with it:
    %  moving the caret as well would leave the line editor painting
    %  from a position it did not choose, which is a different question
    %  from whether the screen was cleared.
    rows_above(T, 3),
    cursor(T, _, PromptRow),
    Above is PromptRow - 2,
    row_text(T, Above, Before),
    assertion(Before \== ''),
    type(T, 'format(user_error, "\\e[2J", []).'),
    key(T, enter),
    assertion(wait_for_prompt(T)),
    row_text(T, Above, After),
    (   After == ''
    ->  true
    ;   format(user_error,
               "row ~w was ~q before the clear and ~q after~n",
               [Above, Before, After]),
        assertion(After == '')
    ).

test(kill_to_start, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    type(T, foo),
    key(T, ctrl_u),
    assert_input(T, R, ''),
    assert_cursor(T, P, R),
    type(T, bar),
    assert_input(T, R, bar).

:- end_tests(terminal_basic).


		 /*******************************
		 *      TEST: SCREEN EDITS      *
		 *******************************/

%   Escape sequences that rearrange whole lines: IL (`ESC [ Ps L'),
%   DL (`ESC [ Ps M'), the reverse index (`ESC M') they share their
%   implementation with, and the scrolling region (`ESC [ Ps ; Ps r')
%   that bounds all three.  These write to the screen rather than to
%   the line editor, so they say what the terminal makes of the
%   sequence.

:- begin_tests(terminal_screen,
               [ condition(needs([program_output])),
                 setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  paint(+T, +Lines) is det.
%
%   Clear the screen and write Lines to it, one per row from the top.
%   The screen does not turn a newline into a carriage return, so the
%   lines carry their own.

paint(T, Lines) :-
    out(T, '\e[2J\e[H'),
    forall(member(Line, Lines),
           out(T, [Line, '\r\n'])).

%!  assert_rows(+T, +Expected) is det.
%
%   Expected holds the content of the rows from the top of the screen.

assert_rows(T, Expected) :-
    length(Expected, Len),
    rows_of(T, 0, Len, Rows),
    (   Rows == Expected
    ->  true
    ;   format(user_error, "rows ~q, expected ~q~n", [Rows, Expected]),
        assertion(Rows == Expected)
    ).

%!  numbered_lines(+From, +To, -Lines) is det.

numbered_lines(From, To, Lines) :-
    findall(Line,
            ( between(From, To, N),
              format(atom(Line), 'l~w', [N])
            ), Lines).

nine_lines(T) :-
    numbered_lines(1, 9, Lines),
    paint(T, Lines).

%!  full_screen(+T, -Painted) is det.
%
%   Write a numbered line to every row of the screen.  That is one line
%   more than fits: the last carriage return scrolls the screen, so
%   Painted, the content of the rows afterwards, runs from l2 to the
%   last line and ends in the caret's own empty row.

full_screen(T, Painted) :-
    term_rows(T, Rows),
    numbered_lines(1, Rows, Lines),
    paint(T, Lines),
    numbered_lines(2, Rows, Text),
    append(Text, [''], Painted),
    assert_rows(T, Painted).

test(delete_lines, [setup(current_test_terminal(T))]) :-
    nine_lines(T),
    out(T, '\e[3;1H\e[2M'),
    assert_rows(T, [l1,l2,l5,l6,l7,l8,l9,'','']),
    assert_cursor(T, 0, 2).

test(delete_lines_to_bottom, [setup(current_test_terminal(T))]) :-
    %  More lines than the screen holds: everything from the caret
    %  down goes, and the caret's own row is left empty.
    nine_lines(T),
    out(T, '\e[5;1H\e[99M'),
    assert_rows(T, [l1,l2,l3,l4,'','','','','']).

test(insert_lines, [setup(current_test_terminal(T))]) :-
    nine_lines(T),
    out(T, '\e[2;1H\e[3L'),
    assert_rows(T, [l1,'','','',l2,l3,l4,l5,l6,l7,l8,l9,'']),
    assert_cursor(T, 0, 1).

test(insert_lines_pushes_off_the_screen,
     [setup(current_test_terminal(T))]) :-
    %  A full screen has no room below, so what is pushed past the
    %  last row is lost rather than added to the scroll back.
    full_screen(T, Painted),
    out(T, '\e[1;1H\e[2L'),
    once(append(Kept, [_Last,_Empty], Painted)), % pushed off the bottom
    assert_rows(T, ['',''|Kept]).

test(delete_lines_in_scroll_region, [setup(current_test_terminal(T))]) :-
    %  What emacs sends to take a line out of a window: a scrolling
    %  region around the window, DL inside it, region back to the whole
    %  screen.  Without DECSTBM the delete pulled up everything below,
    %  taking the mode line and the echo area with it.
    full_screen(T, [Top,_Killed,Third|Below]),
    out(T, '\e[1;3r\e[2;1H\e[1M\e[1;25r'),
    assert_rows(T, [Top,Third,''|Below]).

test(line_feed_scrolls_the_region_only,
     [setup(current_test_terminal(T))]) :-
    %  A line feed on the last row of the region scrolls the region
    %  rather than the screen: the rows below it stay put and nothing
    %  goes to the scroll back.
    full_screen(T, [_Top,Second,Third|Below]),
    out(T, '\e[1;3r\e[3;1H\n\e[1;25r'),
    assert_rows(T, [Second,Third,''|Below]).

%!  one_line(+T, +Text) is det.
%
%   Clear the screen and write Text to the top row, leaving the caret
%   at its end.

one_line(T, Text) :-
    out(T, ['\e[2J\e[H', Text]).

test(erase_to_end_of_line, [setup(current_test_terminal(T))]) :-
    one_line(T, abcdef),
    out(T, '\e[1;4H\e[K'),
    assert_rows(T, [abc]).

test(erase_to_start_of_line, [setup(current_test_terminal(T))]) :-
    %  EL 1 erases up to and including the caret and leaves the rest of
    %  the row where it is.  The parameter was ignored, so this erased
    %  the other half of the line.
    one_line(T, abcdef),
    out(T, '\e[1;4H\e[1K'),
    assert_rows(T, ['    ef']).

test(erase_whole_line, [setup(current_test_terminal(T))]) :-
    one_line(T, abcdef),
    out(T, '\e[1;4H\e[2K'),
    assert_rows(T, ['']).

test(erase_characters, [setup(current_test_terminal(T))]) :-
    %  ECH blanks columns without moving what follows them.
    one_line(T, abcdef),
    out(T, '\e[1;3H\e[2X'),
    assert_rows(T, ['ab  ef']).

test(erase_above, [setup(current_test_terminal(T))]) :-
    paint(T, [l1,l2,l3]),
    out(T, '\e[2;2H\e[1J'),
    assert_rows(T, ['','  ',l3]).

test(scroll_up_and_down_in_region, [setup(current_test_terminal(T))]) :-
    %  SU and SD move the content of the region and leave the caret
    %  where it is.  Both stay inside the region.
    full_screen(T, [_Top,Second,Third|Below]),
    out(T, '\e[1;3r\e[2;1H\e[1S'),
    assert_rows(T, [Second,Third,''|Below]),
    assert_cursor(T, 0, 1),
    out(T, '\e[1T\e[1;25r'),
    assert_rows(T, ['',Second,Third|Below]).

test(scroll_up_without_a_region, [setup(current_test_terminal(T))]) :-
    %  Without a region the window scrolls as a whole, the way a line
    %  feed on the last row does.
    full_screen(T, Painted),
    out(T, '\e[3S'),
    append([_,_,_], Rest, Painted),
    append(Rest, ['','',''], Expected),
    assert_rows(T, Expected).

test(tab_stops, [setup(current_test_terminal(T))]) :-
    %  Tabs stop every eight columns until HTS (ESC H) says otherwise,
    %  and CBT walks the same stops backwards.
    out(T, '\ec'),                      % RIS: default stops again
    one_line(T, 'a\tb'),
    assert_rows(T, ['a       b']),
    assert_cursor(T, 9, 0),
    out(T, '\e[1;1H\e[2I'),             % two tabs forward, writing nothing
    assert_cursor(T, 16, 0),
    assert_rows(T, ['a       b']),
    out(T, '\e[3;7H\eH'),               % a stop on the 7th column
    out(T, '\e[3;1H\tb'),
    assert_rows(T, ['a       b','','      b']),
    out(T, '\e[3;13H\e[Z'),             % back to the stop on column 9
    assert_cursor(T, 8, 2),
    out(T, '\e[Z'),                     % and to the one HTS set
    assert_cursor(T, 6, 2).

test(clear_tab_stops, [setup(current_test_terminal(T))]) :-
    term_cols(T, Cols),
    Margin is Cols-1,
    out(T, '\ec\e[3g'),                 % no stops at all
    out(T, '\t'),
    assert_cursor(T, Margin, 0),        % a tab runs into the margin
    out(T, '\e[2;5H\eH\e[2;1H\t'),      % one stop, on the 5th column
    assert_cursor(T, 4, 1),
    out(T, '\e[2;5H\e[g\e[2;1H\t'),     % and away again
    assert_cursor(T, Margin, 1).

test(erase_display_drops_the_rows, [setup(current_test_terminal(T))]) :-
    %  A row the screen no longer reaches is still in the ring, and
    %  both the painter and <-row walk the ring: erasing the display
    %  has to let go of the text or it stays on the screen.
    paint(T, [l1,l2,l3,l4]),
    out(T, '\e[2J'),
    assert_rows(T, ['','','','']),
    paint(T, [l1,l2,l3,l4]),
    out(T, '\e[2;1H\e[J'),              % and the same from the caret down
    assert_rows(T, [l1,'','','']).

test(repeat_character, [setup(current_test_terminal(T))]) :-
    %  REP repeats the last character written.
    out(T, '\ec-\e[4b\e[2;1Hx\e[b'),
    assert_rows(T, ['-----',xx]).

test(autowrap_off, [setup(current_test_terminal(T))]) :-
    %  With DECAWM off the last column takes every further character
    %  and the caret stays with it.
    term_cols(T, Cols),
    Margin is Cols-1,
    numlist(1, Cols, Ns),
    findall(a, member(_, Ns), As),
    atomic_list_concat(As, Line),       % one full row of a's
    out(T, ['\ec\e[?7l', Line, bcd]),
    assert_cursor(T, Margin, 0),
    atom_concat(Head, a, Line),
    atom_concat(Head, d, Expected),     % b and c were overwritten
    assert_rows(T, [Expected]),
    out(T, ['\e[?7h\e[2;1H', Line, x]), % and wrapping again
    assert_rows(T, [Expected,Line,x]).

test(soft_reset, [setup(current_test_terminal(T))]) :-
    %  DECSTR is part of terminfo's is2, so it runs when a full screen
    %  application starts: it must put the scrolling region back.
    full_screen(T, [_Top,Second|Below]),
    out(T, '\e[1;3r\e[!p'),
    out(T, '\e[1;1H\e[1M'),             % a delete the region would bound
    assert_rows(T, [Second|Below]).

test(save_and_restore_cursor, [setup(current_test_terminal(T))]) :-
    %  DECSC/DECRC (ESC 7 / ESC 8) are what terminfo's sc/rc use; they
    %  carry the attributes along with the position.
    out(T, '\e[2J\e[H'),
    out(T, '\e[2;3H\e7\e[5;1Hlow\e8here'),
    assert_rows(T, ['','  here','','','low']),
    assert_cursor(T, 6, 1).

test(index_and_next_line, [setup(current_test_terminal(T))]) :-
    %  IND (ESC D) moves down in the column it is in, NEL (ESC E) moves
    %  down to the start of the next row.
    out(T, '\e[2J\e[Habc\eDd\eEe'),
    assert_rows(T, [abc,'   d',e]).

test(reverse_index, [setup(current_test_terminal(T))]) :-
    %  ESC M on the top row inserts a line there, the same operation
    %  IL performs.
    paint(T, [l1,l2,l3]),
    out(T, '\e[1;1H\eM'),
    assert_rows(T, ['',l1,l2,l3,'']).

test(alternate_screen_round_trip, [setup(current_test_terminal(T))]) :-
    paint(T, [l1,l2,l3]),
    alt_screen(T, 'ALT'),
    assert_rows(T, ['ALT','','']),
    normal_screen(T),
    assert_rows(T, [l1,l2,l3]).

test(alternate_screen_restores_the_caret,
     [ setup(current_test_terminal(T)),
       cleanup(normal_screen(T))
     ]) :-
    %  The caret comes back with the screen: what is written next -- the
    %  prompt, after a pager quits -- goes where it was, not to the top.
    paint(T, [l1,l2,l3]),
    out(T, '\e[2;3H'),
    cursor(T, C, R),
    alt_screen(T, 'ALT'),
    out(T, '\e[5;10Hx'),
    normal_screen(T),
    assert_cursor(T, C, R).

test(scrolling_region_after_the_alternate_screen,
     [ setup(current_test_terminal(T)),
       cleanup(normal_screen(T))
     ]) :-
    %  DECSTBM homes the caret, so a client that leaves a region behind
    %  must have it reset before the screen comes back, not after: that
    %  is the order end_console_session() sends them in.
    paint(T, [l1,l2,l3]),
    out(T, '\e[2;3H'),
    cursor(T, C, R),
    alt_screen(T, 'ALT'),
    out(T, '\e[1;5r'),                  % a region the client leaves set
    out(T, '\e[r\e[?1049l'),            % reset, then the normal screen
    assert_cursor(T, C, R).

test(leaving_an_alternate_screen_never_entered,
     [setup(current_test_terminal(T))]) :-
    %  A stray rmcup must not blank the window: there is nothing saved
    %  to bring back, so erasing first would leave it empty for good.
    %  One arrives out of a Windows pseudo console as its client goes.
    paint(T, [l1,l2,l3]),
    normal_screen(T),
    assert_rows(T, [l1,l2,l3]).

test(entering_the_alternate_screen_twice,
     [ setup(current_test_terminal(T)),
       cleanup(normal_screen(T))
     ]) :-
    %  The second smcup is ignored rather than saving the alternate
    %  screen over the normal one, which would be gone for good.
    paint(T, [l1,l2,l3]),
    alt_screen(T, 'ALT'),
    alt_screen(T, 'ALT-AGAIN'),
    normal_screen(T),
    assert_rows(T, [l1,l2,l3]).

:- end_tests(terminal_screen).


		 /*******************************
		 *        TEST: NFD TEXT        *
		 *******************************/

:- begin_tests(terminal_nfd,
               [ condition(needs([combining])),
                 setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  nfd_word(-Word) is det.
%
%   "àéîõü" in NFD: 5 grapheme clusters, 10 code points.

nfd_word(Word) :-
    atom_codes(Word,
               [ 0'a, 0x300,           % à
                 0'e, 0x301,           % é
                 0'i, 0x302,           % î
                 0'o, 0x303,           % õ
                 0'u, 0x308            % ü
               ]).

test(paste_nfd_renders_5_cols, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    End is P + 5,
    assert_cursor(T, End, R),
    assert_input(T, R, W).

test(cursor_right_steps_one_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    key(T, ctrl_a),
    assert_cursor(T, P, R),
    key(T, cursor_right),
    C1 is P + 1,
    assert_cursor(T, C1, R),           % past 'à'
    key(T, cursor_right),
    C2 is P + 2,
    assert_cursor(T, C2, R).           % past 'é'

test(forward_delete_first_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    key(T, ctrl_a),
    key(T, ctrl_d),
    drive(0.1),
    assert_cursor(T, P, R),
    atom_codes(Rest,
               [ 0'e, 0x301, 0'i, 0x302, 0'o, 0x303, 0'u, 0x308 ]),
    assert_input(T, R, Rest).

test(word_forward_spans_full_cluster_word, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    key(T, ctrl_a),
    key(T, meta_f),
    End is P + 5,
    assert_cursor(T, End, R).

test(delete_word_forward_removes_full_word, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    nfd_word(W),
    type(T, W),
    key(T, ctrl_a),
    key(T, meta_d),
    drive(0.1),
    assert_input(T, R, ''),
    assert_cursor(T, P, R).

:- end_tests(terminal_nfd).


		 /*******************************
		 *      TEST: REGRESSION        *
		 *******************************/

%   Minimised regressions found by test_terminal_random/2,3.  Each
%   test replays the shortest command sequence that reproduces the
%   bug and asserts the display state the model expected.

:- begin_tests(terminal_regression,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%   Inserting an NFD cluster at `home` when the buffer already contains
%   another NFD cluster drops one character from the re-rendered line.
%   Repro: type `abcỳ` (ỳ = 'y'+U+0300 in NFD), press Home, type
%   `à` (NFD a+U+0300).  Expected line: `àabcỳ`; without the fix,
%   libedit's refresh overwrites the 'b' and the line becomes `àacỳ`.

%   Forward-delete of a mid-line NFD cluster must drop the whole
%   cluster (base + combining marks), not just the base.  Before the
%   fix, libedit's re_update_line placed ofd on a combining mark, so
%   the resulting ANSI DCH targeted the NEXT base's column — the
%   preceding cluster's combiner stayed behind, attaching to whatever
%   cluster shifted into that column.
%   Repro: type `f̀fj̀jòǹàz̀` (NFD), home, cursor_right twice, insert
%   `z`, delete (which should remove `j̀`).  Without the fix the line
%   becomes `f̀fzj̀òǹàz̀` — the `j̀`'s combiner survives, attaches to
%   the plain `j`.

%   Forward-delete of a wide (emoji) cluster must drop exactly the 2
%   cells the cluster occupies, not 3.  Before the fix, rlc_delete_chars
%   decremented its column budget by the base cell's width (1) even for
%   a wide base, so a `count=2` DCH walked past the cluster and ate a
%   character from the next cluster.
%   Repro: type 🤩🤩🤩jj, home, cursor_right once, delete (to remove the
%   second 🤩).  Without the fix the line becomes 🤩🤩jnsh̀i instead of
%   🤩🤩jjnsh̀i — the `j` disappears.

%   Typing a character into the middle of a line whose cell count
%   already exceeds the visual width (because it's full of NFD
%   clusters) must not truncate the trailing combining mark.  Before
%   the fix, rlc_insert clamped tl->size at b->width, so inserting a
%   char into a line of 40 NFD clusters (= 80 cells) dropped the last
%   cell — the final cluster's combining mark.

%   When the buffer contains enough NFD combining marks that the
%   total code-point count past the cursor reaches the terminal width,
%   a wide (emoji) cluster after the cursor caused libedit's re_refresh
%   to bump the cursor to the next row — the wrap check compared the
%   code-point index against t_size.h instead of the visual column.
%   Repro: fill a line with ~45 narrow chars plus enough NFD combining
%   marks to push the code-point index past 80, insert an emoji, move
%   cursor back, then type another char to trigger re_refresh.  Before
%   the fix the cursor reported (0, R+1) instead of the correct (col, R).

test(refresh_wide_at_cursor_uses_visual_col, [setup(test_begin(T))]) :-
    cursor(T, _P, R),
    %  Build a buffer: 20 NFD clusters (40 cps, 20 vcols) + emoji (1
    %  cp, 2 vcols) + 20 narrow chars.  Total cps with prompt > 80,
    %  visual cols well under.
    make_nfd_codes(20, NFDCodes),
    append(NFDCodes, [0x1F929], MidCodes),       % NFD then 🤩
    length(Tail, 20), maplist(=(0'q), Tail),
    append(MidCodes, Tail, AllCodes),
    atom_codes(Buf, AllCodes),
    type(T, Buf),
    %  Move left past the trailing q's, stopping just past the emoji,
    %  so the NEXT char under libedit's cursor is the 🤩.  Then type
    %  one more letter to force a re_refresh.
    forall(between(1, 20, _), key(T, cursor_left)),
    type(T, 'k'),
    %  The cursor must still be on row R, not bumped to R+1.
    cursor(T, _, GotRow),
    assertion(GotRow =:= R).

test(insert_midline_preserves_trailing_combiner,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    cursor(T, P, R),
    %  Fill a line with NFD clusters up to just below visual width so
    %  the next insert definitely exceeds b->width in cells but still
    %  fits visually.
    Fill is 80 - P - 2,
    make_nfd_codes(Fill, TypedCodes),
    atom_codes(Typed, TypedCodes),
    type(T, Typed),
    key(T, home),
    key(T, cursor_right),
    type(T, 'z'),
    TailN is Fill - 1,
    make_nfd_codes(TailN, TailCodes),
    atom_codes(Tail, TailCodes),
    atom_codes(OneCluster, [0'a, 0x300]),
    atom_concat(OneCluster, 'z', Prefix),
    atom_concat(Prefix, Tail, Expected),
    assert_input(T, R, Expected).

make_nfd_codes(0, []) :- !.
make_nfd_codes(N, [0'a, 0x300 | T]) :-
    N > 0,
    N1 is N - 1,
    make_nfd_codes(N1, T).

test(delete_wide_cluster_midline,
     [ condition(needs([non_bmp])),
       setup(test_begin(T))
     ]) :-
    cursor(T, P, R),
    atom_codes(Buf, [0x1F929, 0x1F929, 0x1F929,
                     0'j, 0'j, 0'n, 0's]),
    type(T, Buf),
    key(T, home),
    key(T, cursor_right),
    ColBefore is P + 2,
    assert_cursor(T, ColBefore, R),
    key(T, delete),
    assert_cursor(T, ColBefore, R),
    atom_codes(Expected, [0x1F929, 0x1F929,
                          0'j, 0'j, 0'n, 0's]),
    assert_input(T, R, Expected).

test(delete_nfd_cluster_midline,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    cursor(T, P, R),
    atom_codes(Buf, [ 0'f, 0x300, 0'f,
                      0'j, 0x300, 0'j,
                      0'o, 0x300,
                      0'n, 0x300,
                      0'a, 0x300,
                      0'z, 0x300 ]),
    type(T, Buf),
    key(T, home),
    key(T, cursor_right),
    key(T, cursor_right),
    type(T, 'z'),
    ColBeforeDel is P + 3,
    assert_cursor(T, ColBeforeDel, R),
    key(T, delete),
    assert_cursor(T, ColBeforeDel, R),
    atom_codes(Expected, [ 0'f, 0x300, 0'f, 0'z,
                           0'j,
                           0'o, 0x300,
                           0'n, 0x300,
                           0'a, 0x300,
                           0'z, 0x300 ]),
    assert_input(T, R, Expected).

test(insert_nfd_at_home_with_nfd_buffer,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    cursor(T, P, R),
    atom_codes(Ygrave, [0'y, 0x300]),
    atom_codes(Agrave, [0'a, 0x300]),
    atom_concat('abc', Ygrave, Buffer),
    type(T, Buffer),
    Col0 is P + 4,
    assert_cursor(T, Col0, R),
    key(T, home),
    assert_cursor(T, P, R),
    type(T, Agrave),
    Col1 is P + 1,
    assert_cursor(T, Col1, R),
    atom_concat(Agrave, Buffer, Expected),
    assert_input(T, R, Expected).

test(delete_wide_before_nfd,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    %  Insert a wide character in front of NFD text and take it away
    %  again.  Found by test_terminal_random/2 and Windows-only: libedit
    %  removed the two columns as two CSI P sequences, and we delete
    %  whole grapheme clusters, so the second one ate the cluster behind
    %  the wide character -- 'ǹ' vanished from the display.  U+4E2D is
    %  wide in the BMP, so this exercises the cluster arithmetic without
    %  also involving surrogate pairs.
    cursor(T, P, R),
    atom_codes(Buffer, [0'q, 0x300, 0'n, 0x300, 0'o]),
    type(T, Buffer),
    Col0 is P + 3,
    assert_cursor(T, Col0, R),
    key(T, home),
    type(T, '中'),
    Col1 is P + 2,
    assert_cursor(T, Col1, R),
    key(T, backspace),
    assert_cursor(T, P, R),
    assert_input(T, R, Buffer).

:- end_tests(terminal_regression).


		 /*******************************
		 *     TEST: WIDE / EMOJI       *
		 *******************************/

:- begin_tests(terminal_wide,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  emoji(-Emoji) is det.
%
%   "🤩️" = U+1F929 + VS-16 (emoji presentation selector), 2 visual cols.

emoji(E) :-
    atom_codes(E, [0x1F929, 0xFE0F]).

test(emoji_is_two_columns, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    emoji(E),
    type(T, E),
    End is P + 2,
    assert_cursor(T, End, R).

test(cursor_left_skips_emoji_as_cluster, [setup(test_begin(T))]) :-
    %  libedit's cursor-left steps by one grapheme cluster, so a wide
    %  emoji is crossed in a single hop (no stop on its right half).
    cursor(T, P, R),
    emoji(E),
    type(T, E),
    End is P + 2,
    assert_cursor(T, End, R),
    key(T, cursor_left),
    assert_cursor(T, P, R).                 % back to before the emoji

:- end_tests(terminal_wide).


		 /*******************************
		 *     TEST: NON-BMP (SMP)      *
		 *******************************/

%   Regression tests for supplementary-plane (U+10000+) code points on
%   Windows, where wchar_t is 16-bit UTF-16 and a single code point
%   occupies two wchar_t slots as a surrogate pair.  Linux stores the
%   same code point in one wchar_t, so these tests pin both platforms
%   to the same observable behaviour.

:- begin_tests(terminal_non_bmp,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  smp_char(-Code) is det.
%
%   Plain U+1F929 "star-struck" emoji, no VS-16 — a single non-BMP code
%   point that renders as two visual columns.

smp_char(0x1F929).

test(smp_types_two_columns, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    smp_char(C),
    atom_codes(A, [C]),
    type(T, A),
    End is P + 2,
    assert_cursor(T, End, R).

test(smp_backspace_removes_whole_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    smp_char(C),
    atom_codes(A, [C]),
    type(T, A),
    key(T, backspace),
    drive(0.1),
    assert_cursor(T, P, R),                 % 2 visual cols gone
    atom_codes(Empty, []),
    assert_input(T, R, Empty).              % buffer emptied

test(smp_cursor_left_is_one_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    smp_char(C),
    atom_codes(A, [C]),
    type(T, A),
    End is P + 2,
    assert_cursor(T, End, R),
    key(T, cursor_left),
    assert_cursor(T, P, R).                 % one hop across the pair

test(smp_delete_forward_is_one_cluster, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    smp_char(C),
    atom_codes(A, [C]),
    type(T, A),
    key(T, home),
    assert_cursor(T, P, R),
    key(T, delete),
    drive(0.1),
    assert_cursor(T, P, R),
    atom_codes(Empty, []),
    assert_input(T, R, Empty).

test(smp_midline_insert,
     [ condition(needs([non_bmp])),
       setup(test_begin(T))
     ]) :-
    %   Type an ASCII context, step the cursor into the middle of it,
    %   then insert a non-BMP cluster.  Verifies the pair lands in the
    %   buffer as a single cluster and the display advances by two
    %   visual columns.
    cursor(T, P, R),
    type(T, abcd),
    End is P + 4,
    assert_cursor(T, End, R),
    key(T, cursor_left),                    % between c and d
    key(T, cursor_left),                    % between b and c
    Mid is P + 2,
    assert_cursor(T, Mid, R),
    smp_char(C),
    atom_codes(Emoji, [C]),
    type(T, Emoji),
    drive(0.1),
    After is P + 4,                         % cursor = Mid + 2 visual cols
    assert_cursor(T, After, R),
    atom_codes(Expected, [0'a, 0'b, C, 0'c, 0'd]),
    assert_input(T, R, Expected).

:- end_tests(terminal_non_bmp).


		 /*******************************
		 *       TEST: MIXED (GOLD)     *
		 *******************************/

:- begin_tests(terminal_mixed,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  mixed_line(-Atom) is det.
%
%   "àéîõü🤩️õàéîõü🤩️" — 15 visual columns.

mixed_line(Atom) :-
    atom_codes(Atom,
               [ 0'a, 0x300,           % à
                 0'e, 0x301,           % é
                 0'i, 0x302,           % î
                 0'o, 0x303,           % õ
                 0'u, 0x308,           % ü
                 0x1F929, 0xFE0F,      % 🤩️
                 0'o, 0x303,           % õ
                 0'a, 0x300,           % à
                 0'e, 0x301,           % é
                 0'i, 0x302,           % î
                 0'o, 0x303,           % õ
                 0'u, 0x308,           % ü
                 0x1F929, 0xFE0F       % 🤩️
               ]).

test(mixed_line_reports_15_cols, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    mixed_line(L),
    type(T, L),
    End is P + 15,
    assert_cursor(T, End, R).

test(cursor_left_from_end_lands_before_emoji, [setup(test_begin(T))]) :-
    cursor(T, P, R),
    mixed_line(L),
    type(T, L),
    End is P + 15,
    assert_cursor(T, End, R),
    key(T, cursor_left),
    Before is P + 13,
    assert_cursor(T, Before, R),            % before final emoji
    key(T, cursor_left),
    BeforeU is P + 12,
    assert_cursor(T, BeforeU, R).           % before the preceding 'ü'

test(insert_before_final_emoji,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    cursor(T, P, R),
    mixed_line(L),
    type(T, L),
    key(T, cursor_left),                    % at col P+13 (before final emoji)
    type(T, z),
    drive(0.1),
    After is P + 14,                        % after the inserted 'z'
    assert_cursor(T, After, R),
    atom_codes(Expected,
               [ 0'a, 0x300, 0'e, 0x301, 0'i, 0x302, 0'o, 0x303,
                 0'u, 0x308, 0x1F929, 0xFE0F,
                 0'o, 0x303,
                 0'a, 0x300, 0'e, 0x301, 0'i, 0x302,
                 0'o, 0x303, 0'u, 0x308,
                 0'z,
                 0x1F929, 0xFE0F
               ]),
    assert_input(T, R, Expected).

:- end_tests(terminal_mixed).


		 /*******************************
		 *      TEST: BACKGROUND IO     *
		 *******************************/

:- begin_tests(terminal_background,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  bg_row(+Terminal, +Text, -Row) is semidet.
%
%   Row is the first visible row whose content is exactly Text.

bg_row(T, Text, Row) :-
    between(0, 24, Row),
    row_text(T, Row, Line),
    atom(Line),
    atom_concat(Text, Padding, Line),
    \+ sub_atom(Padding, _, _, _, ' '),         % trailing blanks only
    !.

input_row_holds(T, Row, Input) :-
    row_text(T, Row, Line),
    strip_prompt(Line, Got),
    Got == Input.

test(thread_output_keeps_input_line, [setup(test_begin(T))]) :-
    %  Output from another thread while the user is typing must not be
    %  written into the input line.  libedit takes the line off the
    %  screen, lets the output through, and paints the line back below
    %  it -- without waiting for the next keystroke.
    type(T, 'thread_create((sleep(1),writeln(from_thread)),_,[detached(true)]).'),
    key(T, enter),
    assertion(wait_for_prompt(T)),
    prompt_col(T, P),
    Input = 'foo(Bar)',
    type(T, Input),
    (   wait_until(bg_row(T, from_thread, _), 15)
    ->  true
    ;   dump_screen(T, 'waiting for output from the other thread')
    ),
    assertion(bg_row(T, from_thread, _)),
    bg_row(T, from_thread, OutRow),
    InputRow is OutRow + 1,
    %  The line comes back on its own account, a moment after the output
    %  it was taken down for.  Wait for it rather than reading the screen
    %  the instant the output lands.
    (   wait_until(input_row_holds(T, InputRow, Input), 5)
    ->  true
    ;   dump_screen(T, 'input line did not come back below the output')
    ),
    assert_input(T, InputRow, Input),
    atom_length(Input, Len),
    ExpCol is P + Len,
    assert_cursor(T, ExpCol, InputRow).

:- end_tests(terminal_background).


		 /*******************************
		 *        TEST: MOUSE           *
		 *******************************/

:- begin_tests(terminal_mouse,
               [ condition(needs([mouse, selection])),
                 setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

test(click_moves_the_caret, [setup(test_begin(T))]) :-
    %  A click in the line being edited puts the caret there.  The
    %  terminal cannot place the caret itself -- the line belongs to
    %  the client -- so it asks, by sending as many cursor keys as
    %  there are grapheme clusters in between.  Assert the caret moves
    %  by the distance clicked rather than to an absolute column: the
    %  pixel-to-cell mapping is the terminal's business, and a
    %  synthesised event does not carry the offset a real one has.
    type(T, 'hello world, this is the input line'),
    drive(0.3),
    cursor(T, End, R),
    click(T, 12, R),
    cursor(T, C1, R1),
    assertion(R1 =:= R),
    assertion(C1 < End),
    click(T, 20, R),                    % eight cells further right
    cursor(T, C2, R2),
    assertion(R2 =:= R),
    (   C2 =:= C1+8
    ->  true
    ;   format(user_error,
               "caret went from ~w to ~w, expected ~w~n", [C1, C2, C1+8]),
        assertion(C2 =:= C1+8)
    ),
    click(T, 20, R),                    % clicking again changes nothing
    assert_cursor(T, C2, R).

test(click_outside_the_input_line, [setup(test_begin(T))]) :-
    %  Only the line being edited follows the mouse; a click anywhere
    %  else still just starts a selection.  Push the prompt down first
    %  so there is a row above it to click on: how far down a terminal
    %  starts out is a property of the backend, not of the behaviour
    %  under test.
    rows_above(T, 2),
    type(T, 'hello'),
    drive(0.3),
    cursor(T, C, R),
    Above is R-2,
    assertion(Above >= 0),
    click(T, 5, Above),
    assert_cursor(T, C, R).

test(drag_selects_and_leaves_the_caret, [setup(test_begin(T))]) :-
    type(T, 'hello world, this is the input line'),
    drive(0.3),
    cursor(T, C, R),
    drag(T, 10, R, 20, R),
    assert_cursor(T, C, R),
    assertion(term_has_selection(T)).

test(click_on_a_wrapped_row, [setup(test_begin(T))]) :-
    %  The input spans two rows; a click on the first row moves the
    %  caret back into it.
    filler(120, Xs),
    type(T, Xs),
    drive(0.4),
    cursor(T, _, LastRow),
    FirstRow is LastRow-1,
    click(T, 20, FirstRow),
    cursor(T, C, R),
    assertion(R =:= FirstRow),
    click(T, 28, FirstRow),
    cursor(T, C2, R2),
    assertion(R2 =:= FirstRow),
    assertion(C2 =:= C+8).

:- end_tests(terminal_mouse).


		 /*******************************
		 *        TEST: WHEEL           *
		 *******************************/

/** <section> What the wheel does

    Turning the wheel means different things depending on what runs on
    the terminal, and always scrolling our own scroll back is the one
    thing no other terminal does:

      - Nothing in particular: scroll the scroll back.
      - A full screen application, i.e. one on the alternate screen:
        there is no scroll back to scroll there.
      - An application that asked for mouse reports: the wheel is a
        button like any other and the application decides.
*/

:- begin_tests(terminal_wheel,
               [ condition(needs([mouse, program_output])),
                 setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%!  scrollback(+T, +N) is det.
%
%   Clear the screen and write N numbered lines.  N is more than the
%   screen holds, so the first ones end up in the scroll back.

scrollback(T, N) :-
    out(T, '\e[2J\e[H'),
    forall(between(1, N, I),
           out(T, ['line', I, '\r\n'])).

test(wheel_scrolls_the_scrollback, [setup(current_test_terminal(T))]) :-
    scrollback(T, 60),
    row_text(T, 0, Before),
    wheel(T, 10, 5, 3),
    row_text(T, 0, After),
    assertion(Before \== After).

test(alt_screen_has_no_scrollback,
     [ setup(current_test_terminal(T)),
       cleanup(( out(T, '\e[?1007h'), normal_screen(T) ))
     ]) :-
    %  The lines the alternate screen replaced belong to the normal
    %  screen: an application owns the window until it gives them back,
    %  and scrolling them into view is never what the wheel was for.
    %
    %  Alternate scroll off: this is about our own scroll back, not
    %  about the cursor keys the wheel otherwise sends the application
    %  -- which the client on this terminal would answer.
    scrollback(T, 60),
    alt_screen(T, 'ALT-SCREEN'),
    out(T, '\e[?1007l'),
    wheel(T, 10, 5, 3),
    assert_row(T, 0, 'ALT-SCREEN').

test(alt_screen_scrollbar_is_full,
     [ setup(current_test_terminal(T)),
       cleanup(normal_screen(T))
     ]) :-
    %  ... and the scroll bar must say so rather than offering a bubble
    %  that scrolls nowhere.
    scrollback(T, 60),
    alt_screen(T, 'ALT-SCREEN'),
    term_rows(T, Rows),
    term_bubble(T, Length, Start, View),
    assertion([Length,Start,View] == [Rows,0,Rows]).

:- end_tests(terminal_wheel).


		 /*******************************
		 *      TEST: ALT SCROLL        *
		 *******************************/

%   Alternate scroll (DEC private mode 1007): what the terminal sends
%   its client is otherwise invisible, so these tests run a client that
%   reads it back; see client_reads/2.

:- begin_tests(terminal_alt_scroll,
               [ condition(needs([mouse, program_output, pty_signals])),
                 setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

alt_scroll_begin(T) :-
    current_test_terminal(T),
    echo_client(Cmd),
    start_foreground(T, Cmd),
    alt_screen(T, '').

alt_scroll_end(T) :-
    normal_screen(T),
    stop_foreground(T).

test(wheel_down_sends_cursor_down,
     [ setup(alt_scroll_begin(T)),
       cleanup(alt_scroll_end(T))
     ]) :-
    %  What makes the wheel scroll `less' and `man': they never asked
    %  for a mouse, so the terminal turns the wheel into the keys they
    %  do read -- three lines to the notch, as everywhere else.
    wheel(T, 10, 5, -1),
    assertion(client_reads(T, '^[[B^[[B^[[B')).

test(wheel_up_sends_cursor_up,
     [ setup(alt_scroll_begin(T)),
       cleanup(alt_scroll_end(T))
     ]) :-
    wheel(T, 10, 5, 1),
    assertion(client_reads(T, '^[[A^[[A^[[A')).

test(application_cursor_keys,
     [ setup(alt_scroll_begin(T)),
       cleanup(alt_scroll_end(T))
     ]) :-
    %  DECCKM (mode 1) decides how a cursor key is spelled, and these
    %  are cursor keys like any other.
    out(T, '\e[?1h'),
    wheel(T, 10, 5, -1),
    assertion(client_reads(T, '^[OB^[OB^[OB')),
    out(T, '\e[?1l').

test(alt_scroll_can_be_switched_off,
     [ setup(alt_scroll_begin(T)),
       cleanup(( out(T, '\e[?1007h'), alt_scroll_end(T) ))
     ]) :-
    out(T, '\e[?1007l'),
    wheel(T, 10, 5, -1),
    assertion(client_reads(T, '')).

test(shift_wheel_is_not_the_applications,
     [ setup(alt_scroll_begin(T)),
       cleanup(alt_scroll_end(T))
     ]) :-
    %  Shift is the way out of whatever the application asked for; on
    %  the alternate screen that leaves the wheel with nothing to do.
    button_shift(Shift),
    wheel(T, 10, 5, -1, Shift),
    assertion(client_reads(T, '')).

test(normal_screen_wheel_is_ours,
     [ setup(( current_test_terminal(T),
               echo_client(Cmd),
               start_foreground(T, Cmd) )),
       cleanup(stop_foreground(T))
     ]) :-
    %  Off the alternate screen the wheel scrolls the scroll back and
    %  the client hears nothing of it, whatever it is running.
    wheel(T, 10, 5, 1),
    assertion(client_reads(T, '')).

:- end_tests(terminal_alt_scroll).


		 /*******************************
		 *      TEST: MOUSE REPORTS     *
		 *******************************/

%   Mouse reporting (DEC private modes 9, 1000, 1002 and 1003, encoded
%   as asked for by 1005, 1006 or 1015).  As with alternate scroll, the
%   reports are read back from a client that echoes what it is sent.
%
%   The cell clicked on is (10,5) throughout, which the wire format
%   counts from one as column 11, row 6.

:- begin_tests(terminal_mouse_reports,
               [ condition(needs([mouse, program_output, pty_signals])),
                 setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

reports_begin(T) :-
    current_test_terminal(T),
    echo_client(Cmd),
    start_foreground(T, Cmd).

reports_end(T) :-
    out(T, '\e[?1003l\e[?1002l\e[?1000l\e[?9l\e[?1006l\e[?1015l\e[?1005l'),
    stop_foreground(T).

%!  tracking(+T, +Modes) is det.
%
%   Ask for the mouse as an application would, Modes being the DEC
%   private modes it sets.

tracking(T, Modes) :-
    forall(member(Mode, Modes),
           out(T, ['\e[?', Mode, 'h'])).

test(sgr_reports_press_and_release,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    tracking(T, [1000, 1006]),
    click(T, 10, 5),
    assertion(client_reads(T, '^[[<0;11;6M^[[<0;11;6m')).

test(default_encoding_is_the_x10_one,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    %  Without 1006 the report is CSI M and three bytes, each 32 more
    %  than the number it stands for: button 0 is a space, column 11 a
    %  `+', row 6 an `&'.  A release says only that a button came up,
    %  which is button 3, a `#'.
    tracking(T, [1000]),
    click(T, 10, 5),
    assertion(client_reads(T, '^[[M +&^[[M#+&')).

test(urxvt_encoding,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    tracking(T, [1000, 1015]),
    click(T, 10, 5),
    assertion(client_reads(T, '^[[32;11;6M^[[35;11;6M')).

test(wheel_is_a_button,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    %  Buttons 64 and 65, one report per notch and no release.  This is
    %  what `less --mouse' and emacs read, and it is why they scroll in
    %  a terminal that reports and not in one that scrolls itself.
    tracking(T, [1000, 1006]),
    wheel(T, 10, 5, -1),
    assertion(client_reads(T, '^[[<65;11;6M')),
    wheel(T, 10, 5, 2),
    assertion(client_reads(T, '^[[<64;11;6M^[[<64;11;6M')).

test(drags_need_button_event_tracking,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    %  1000 reports the two ends of a drag but not the way there;
    %  1002 adds the motion, marked with the motion flag (32).
    tracking(T, [1000, 1006]),
    drag(T, 10, 5, 20, 5),
    assertion(client_reads(T, '^[[<0;11;6M^[[<0;21;6m')),
    tracking(T, [1002]),
    drag(T, 10, 5, 20, 5),
    assertion(client_reads(T, '^[[<0;11;6M^[[<32;21;6M^[[<0;21;6m')).

test(motion_needs_any_event_tracking,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    %  A pointer that moves with no button down is 1003's business
    %  alone, and only where it enters a new cell: button 3 (no
    %  button) plus the motion flag is 35.
    tracking(T, [1002, 1006]),
    move(T, 10, 5),
    assertion(client_reads(T, '')),
    tracking(T, [1003]),
    move(T, 11, 5),
    move(T, 11, 5),
    assertion(client_reads(T, '^[[<35;12;6M')).

test(x10_tracking_reports_presses_only,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    tracking(T, [9, 1006]),
    click(T, 10, 5),
    assertion(client_reads(T, '^[[<0;11;6M')).

test(modifiers_are_reported,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    button_control(Control),
    tracking(T, [1000, 1006]),
    click(T, 10, 5, Control),
    assertion(client_reads(T, '^[[<16;11;6M^[[<16;11;6m')).

test(shift_click_is_the_users,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    %  Shift is the way out of an application that took the mouse: it
    %  is never reported, so selecting and pasting keep working.
    button_shift(Shift),
    tracking(T, [1000, 1006]),
    click(T, 10, 5, Shift),
    assertion(client_reads(T, '')).

test(tracking_can_be_switched_off,
     [ setup(reports_begin(T)),
       cleanup(reports_end(T))
     ]) :-
    tracking(T, [1000, 1006]),
    out(T, '\e[?1000l'),
    click(T, 10, 5),
    assertion(client_reads(T, '')).

test(utf8_encoding,
     [ setup(reports_begin(T)),
       cleanup(( resize_cols(T, 80, _), reports_end(T) ))
     ]) :-
    %  1005 is the default encoding with the three numbers written as
    %  UTF-8 code points rather than as bytes, so it says nothing until
    %  a number passes 127 -- which takes a terminal wider than the 96th
    %  column.  `cat -v' spells the bytes out: the single 0x85 of the
    %  default is `M-^E', the two of its UTF-8 form are `M-BM-^E'.
    resize_cols(T, 120, Cols),
    assertion(Cols >= 101),
    tracking(T, [1000]),
    click(T, 100, 5),
    assertion(client_reads(T, '^[[M M-^E&^[[M#M-^E&')),
    tracking(T, [1005]),
    click(T, 100, 5),
    assertion(client_reads(T, '^[[M M-BM-^E&^[[M#M-BM-^E&')).

test(reporting_takes_the_wheel_from_alternate_scroll,
     [ setup(reports_begin(T)),
       cleanup(( normal_screen(T), reports_end(T) ))
     ]) :-
    %  Both could claim the wheel on the alternate screen.  An
    %  application that asked for reports gets them; alternate scroll
    %  is for the ones that did not.
    alt_screen(T, ''),
    tracking(T, [1000, 1006]),
    wheel(T, 10, 5, -1),
    assertion(client_reads(T, '^[[<65;11;6M')).

:- end_tests(terminal_mouse_reports).


		 /*******************************
		 *        TEST: RESIZE          *
		 *******************************/

%   These tests resize the terminal while a line is
%   being edited and check that libedit+xpce re-wrap the current input
%   correctly at the new column count.  The tests currently fail on
%   the known resize-while-editing bug (stale rows from the pre-resize
%   wrap leak into the post-resize display); they exist to pin that
%   bug down so the follow-up fix has a regression net.

:- begin_tests(terminal_resize,
               [ setup(setup_unit_resize),
                 cleanup(cleanup_unit_resize)
               ]).

%!  setup_unit_resize is det.
%!  cleanup_unit_resize is det.
%
%   Extend the shared terminal setup/cleanup so we also remember the
%   initial width of the terminal.  Each resize test restores that
%   width first (via resize_test_begin/1) so a previous test's resize
%   doesn't leak into the next one.

setup_unit_resize :-
    setup_unit,
    current_test_terminal(T),
    term_cols(T, Cols),
    nb_setval(terminal_resize_initial_cols, Cols).

cleanup_unit_resize :-
    (   nb_current(terminal_resize_initial_cols, _)
    ->  nb_delete(terminal_resize_initial_cols)
    ;   true
    ),
    cleanup_unit.

%!  resize_test_begin(-Terminal) is det.
%
%   Per-resize-test setup: restore the terminal to its initial width,
%   then delegate to the usual test_begin/1 (which clears the input
%   line).  Resizing first means the input line is cleared at the
%   original width, so every test starts from the same geometry.

resize_test_begin(T) :-
    current_test_terminal(T),
    (   nb_current(terminal_resize_initial_cols, Cols0),
        term_cols(T, Cols),
        Cols =\= Cols0
    ->  resize_cols(T, Cols0, _)
    ;   true
    ),
    reset_input(T).

%!  type_and_wait(+Terminal, +Text) is det.
%
%   Like type/2 but waits until libedit has echoed Text to the end.
%   drive(0.1) is not always enough for libedit to finish echoing a
%   large paste before we trigger a resize; without this wait the
%   resize sees a half-echoed line and the caret in a stale mid-line
%   position.  The expected final cursor position is derived from the
%   pre-type cursor, the input length, and the current column count;
%   at the wrap boundary (TotalCells a multiple of Cols) xpce reports
%   the "pending-wrap" column, hence the branch on LastCol+1.

type_and_wait(T, Text) :-
    cursor(T, Col0, Row0),
    atom_length(Text, Len),
    term_cols(T, Cols),
    TotalCells is Col0 + Len,
    LastCell is TotalCells - 1,
    LastRow is Row0 + LastCell // Cols,
    LastCol is LastCell mod Cols,
    NextCol is LastCol + 1,
    margin_col(T, Margin),
    (   NextCol < Cols
    ->  ExpCol = NextCol, ExpRow = LastRow
    ;   ExpCol = Margin,  ExpRow = LastRow
    ),
    term_send(T, Text),
    wait_until(cursor_at(T, ExpCol, ExpRow), 5).

cursor_at(T, Col, Row) :-
    cursor(T, C, R),
    C =:= Col, R =:= Row.

has_prompt(Line) :-
    atom(Line),
    sub_atom(Line, _, 3, _, '?- ').

trim_trailing_spaces(Atom, Trimmed) :-
    atom_codes(Atom, Codes),
    reverse(Codes, RCodes),
    drop_spaces(RCodes, RTrimmed),
    reverse(RTrimmed, TrimmedCodes),
    atom_codes(Trimmed, TrimmedCodes).

drop_spaces([0' |T], R) :- !, drop_spaces(T, R).
drop_spaces(L, L).

count_prompt_occurrences(Rows, N) :-
    aggregate_all(count, (member(L, Rows), has_prompt(L)), N).

assert_single_prompt(Rows) :-
    count_prompt_occurrences(Rows, N),
    (   N == 1
    ->  true
    ;   format(user_error,
               "expected exactly one prompt row, found ~w in ~q~n",
               [N, Rows]),
        assertion(N == 1)
    ).

test(resize_welcome_line, [setup(resize_test_begin(T))]) :-
    %  Reproduce the user's bug report as closely as possible on a
    %  fresh terminal: a long ASCII input, resized so the line needs
    %  exactly two rows at the new width.  The bug manifests as an
    %  extra (duplicated) prompt row appearing after the resize.
    cursor(T, P, R),
    Input = 'Welcome to SWI-Prolog (threaded, 64 bits, version 10.1.5-43-g7b3ac1193-DIRTY)',
    type_and_wait(T, Input),
    atom_length(Input, InputLen),
    %  Pick a width that gives NewCols such that the first row holds
    %  prompt + most of the input and the second row holds the tail.
    TargetCols = 73,
    resize_cols(T, TargetCols, NewCols),
    assertion(NewCols == TargetCols),
    %  First row: prompt + first (NewCols - P) chars of the input.
    HeadLen is NewCols - P,
    sub_atom(Input, 0, HeadLen, _, Head),
    row_text(T, R, Row0),
    strip_prompt(Row0, Row0Tail),
    assertion(Row0Tail == Head),
    %  Second row: rest of the input, no prompt.
    TailLen is InputLen - HeadLen,
    sub_atom(Input, HeadLen, TailLen, 0, Tail),
    R1 is R + 1,
    assert_row(T, R1, Tail),
    %  Third row must be empty — the pre-resize row that held the
    %  continuation "193-DIRTY)" should be cleared.
    R2 is R + 2,
    row_text(T, R2, Row2),
    assertion(\+ has_prompt(Row2)),
    rows_of(T, 0, 5, AllRows),
    assert_single_prompt(AllRows).

test(resize_ascii_shrink, [setup(resize_test_begin(T))]) :-
    %  Shrink the width enough that the 150-x line needs 3+ rows.  Each
    %  non-empty row must be at most NewCols wide, and concatenating
    %  the rows (stripping the prompt once) must reproduce the input
    %  exactly — so there can be no duplicated prefix.
    cursor(T, P, R),
    Len = 150,
    filler(Len, Xs),
    type_and_wait(T, Xs),
    TargetCols = 40,			% comfortably forces 3+ rows
    resize_cols(T, TargetCols, NewCols),
    assertion((NewCols >= 30, NewCols =< 50)),
    %  Number of rows the wrapped line occupies (prompt only counts on
    %  first row).
    RowsNeeded is (P + Len + NewCols - 1) // NewCols,
    rows_of(T, R, RowsNeeded, DisplayRows),
    %  Rows may include padding spaces; the *content* length (after
    %  trailing whitespace trim) should not exceed NewCols.
    forall(member(Row, DisplayRows),
           (   trim_trailing_spaces(Row, Trimmed),
               atom_length(Trimmed, L),
               (   L =< NewCols
               ->  true
               ;   format(user_error,
                          "row too wide: ~w chars (NewCols=~w): ~q~n",
                          [L, NewCols, Row]),
                   assertion(L =< NewCols)
               )
           )),
    assert_single_prompt(DisplayRows),
    %  Reconstruct: strip prompt from first row, trim trailing padding
    %  from each, concatenate; expect the original 150 x's.
    DisplayRows = [First|Tail],
    strip_prompt(First, FirstTail),
    maplist(trim_trailing_spaces, [FirstTail|Tail], Trimmed),
    atomic_list_concat(Trimmed, Joined),
    assertion(Joined == Xs).

test(resize_ascii_grow, [setup(resize_test_begin(T))]) :-
    cursor(T, P, R),
    filler(150, Xs),
    type_and_wait(T, Xs),
    WantCols is P + 158,
    resize_cols(T, WantCols, NewCols),
    assertion(NewCols >= P + 150),
    assert_input(T, R, Xs),
    rows_of(T, 0, 3, Rows),
    assert_single_prompt(Rows).

test(resize_to_exact_row_multiple,
     [ condition(magic_margins),
       setup(resize_test_begin(T))
     ]) :-
    %  Shrink to a width the input fills exactly: prompt + input is a
    %  whole number of rows, so the caret ends on the right margin.  A
    %  terminal with magic margins leaves it there rather than opening
    %  the row below, and libedit must rewind by one row less when it
    %  repaints.  It rewound one row too far and painted the input over
    %  the line above the prompt, eating it.
    %
    %  Needs a terminal description that says so: this is the one place
    %  where the redisplay predicts the caret from the description
    %  rather than settling it, so where the two disagree -- TERM=ansi
    %  on the xpce terminal -- libedit rewinds by the wrong amount and
    %  there is nothing it could have done about it.
    %  Run a goal first: its output gives us a known line above the
    %  prompt, which is what the bug ate.
    type(T, 'true.'),
    key(T, enter),
    assertion(wait_for_prompt(T)),
    cursor(T, P, R),
    assertion(R > 0),
    TargetCols = 54,
    Len is 2*TargetCols - P,            % exactly two rows after the resize
    filler(Len, Xs),
    type_and_wait(T, Xs),
    row_text(T, R, PromptLine),
    prompt_prefix(PromptLine, P, Prompt),
    R0 is R - 1,
    row_text(T, R0, Above0),
    resize_cols(T, TargetCols, NewCols),
    assertion(NewCols == TargetCols),
    %  The prompt row moved (the input needs one row more than it did
    %  at 80 columns), so find it by its prompt rather than from the
    %  caret: at the right margin the caret sits below the last row of
    %  the input.  The line above the prompt must still be there.
    prompt_row(T, Prompt, PromptRow),
    AboveRow is PromptRow - 1,
    row_text(T, AboveRow, Above1),
    (   Above1 == Above0
    ->  true
    ;   format(user_error,
               "line above the prompt: expected ~q, got ~q~n",
               [Above0, Above1]),
        assertion(Above1 == Above0)
    ),
    RowsNeeded is (P + Len + NewCols - 1) // NewCols,
    rows_of(T, PromptRow, RowsNeeded, DisplayRows),
    assert_single_prompt(DisplayRows),
    DisplayRows = [First|Rest],
    strip_prompt(First, FirstTail),
    maplist(trim_trailing_spaces, [FirstTail|Rest], Trimmed),
    atomic_list_concat(Trimmed, Joined),
    assertion(Joined == Xs).

%!  prompt_rows(+Terminal, +Prompt, -Rows) is det.
%!  prompt_row(+Terminal, +Prompt, -Row) is semidet.
%
%   All visible rows that start with Prompt, and the last of them.

prompt_rows(T, Prompt, Rows) :-
    atom_length(Prompt, PL),
    findall(I,
            ( between(0, 24, I),
              row_text(T, I, Line),
              sub_atom(Line, 0, PL, _, Prompt)
            ), Rows).

prompt_row(T, Prompt, Row) :-
    prompt_rows(T, Prompt, Rows),
    last(Rows, Row).

test(key_after_resize_uses_new_width,
     [ setup(resize_test_begin(T))
     ]) :-
    %  Resize, then press a key.  The resize must reach libedit before
    %  the key is acted on: ^A moves the caret up by as many rows as
    %  libedit believes the input occupies, and with the width it had
    %  before the resize that is the wrong row -- the repaint then
    %  leaves a copy of the first row behind.  On Windows nothing
    %  interrupts the read, so the size has to be polled after it.
    %  Needs a terminal of its own: a resize in an earlier test leaves
    %  libedit's size already in step.
    cursor(T, P, R),
    row_text(T, R, PromptLine),
    prompt_prefix(PromptLine, P, Prompt),
    TargetCols = 60,
    %  One character past four whole rows at the new width: the last
    %  row holds a single character, so the row count changes and the
    %  caret's row offset with it.
    Len is 4*TargetCols + 1 - P,
    filler(Len, Xs),
    type_and_wait(T, Xs),
    resize_cols(T, TargetCols, NewCols),
    key(T, ctrl_a),
    drive(0.2),
    %  Exactly one prompt on the screen: a repaint that started on the
    %  wrong row leaves a second copy of the first row above it.  The
    %  terminal is this test's own, so the prompt of the line being
    %  edited is the only one there is.
    prompt_rows(T, Prompt, PromptRows),
    last(PromptRows, PromptRow),
    (   PromptRows = [_]
    ->  true
    ;   format(user_error, "prompt on rows ~q, expected one~n", [PromptRows]),
        assertion(PromptRows = [_])
    ),
    assert_cursor(T, P, PromptRow),
    RowsNeeded is (P + Len + NewCols - 1) // NewCols,
    rows_of(T, PromptRow, RowsNeeded, DisplayRows),
    assert_single_prompt(DisplayRows),
    DisplayRows = [First|Rest],
    strip_prompt(First, FirstTail),
    maplist(trim_trailing_spaces, [FirstTail|Rest], Trimmed),
    atomic_list_concat(Trimmed, Joined),
    assertion(Joined == Xs).

test(shrink_move_caret_widen,
     [ setup(resize_test_begin(T))
     ]) :-
    %  Shrink, walk the caret to the start and back to the end, widen,
    %  then ^A.  Going to the end moves the caret down one row per
    %  wrapped row, and a terminal that took each of those moves for a
    %  line break turned every continuation of the input into a hard
    %  line: rewrapping on the next resize reflowed the pieces on their
    %  own and left parts of the old layout on the screen.
    cursor(T, P, R),
    row_text(T, R, PromptLine),
    prompt_prefix(PromptLine, P, Prompt),
    Len is 321 - P,
    filler(Len, Xs),
    type_and_wait(T, Xs),
    resize_cols(T, 39, _),
    key(T, ctrl_a),
    drive(0.2),
    key(T, ctrl_e),
    drive(0.2),
    resize_cols(T, 60, _),
    key(T, ctrl_a),
    drive(0.2),
    prompt_rows(T, Prompt, PromptRows),
    last(PromptRows, PromptRow),
    (   PromptRows = [_]
    ->  true
    ;   format(user_error, "prompt on rows ~q, expected one~n", [PromptRows]),
        assertion(PromptRows = [_])
    ),
    assert_cursor(T, P, PromptRow),
    RowsNeeded is (P + Len + 59) // 60,
    rows_of(T, PromptRow, RowsNeeded, DisplayRows),
    DisplayRows = [First|Rest],
    strip_prompt(First, FirstTail),
    maplist(trim_trailing_spaces, [FirstTail|Rest], Trimmed),
    atomic_list_concat(Trimmed, Joined),
    assertion(Joined == Xs).

test(selection_survives_resize,
     [ condition(needs([selection])),
       setup(resize_test_begin(T))
     ]) :-
    %  Rewrapping rebuilds the ring of lines the selection points into,
    %  so the anchors have to be carried across with the text.  They
    %  were not, and a selection made before a resize covered something
    %  else afterwards.
    type(T, 'true.'),
    key(T, enter),
    assertion(wait_for_prompt(T)),
    filler(200, Xs),
    type_and_wait(T, Xs),
    term_select_all(T),
    term_selection(T, Text0),
    resize_cols(T, 60, _),
    term_selection(T, Text1),
    (   Text1 == Text0
    ->  true
    ;   format(user_error,
               "selection changed over the resize:~n  before: ~q~n  after:  ~q~n",
               [Text0, Text1]),
        assertion(Text1 == Text0)
    ).

test(resize_wrapped_row_ending_in_a_space, [setup(resize_test_begin(T))]) :-
    %  Put a space in the last column of the first row.  libedit does
    %  not write trailing blanks, so unless it is made to, the row ends
    %  with the newline that moves to the next one rather than with a
    %  wrap -- and the terminal, which rewraps on resize, reads that as
    %  a hard line break and reflows the input into the wrong number of
    %  rows, leaving a copy of a row on the screen.
    type(T, 'true.'),
    key(T, enter),
    assertion(wait_for_prompt(T)),
    cursor(T, P, R),
    assertion(R > 0),
    row_text(T, R, PromptLine),
    prompt_prefix(PromptLine, P, Prompt),
    RowAbove is R-1,
    row_text(T, RowAbove, Above0),
    FirstCols = 70,                     % the space ends the second row
    HeadLen is 2*FirstCols - P - 1,     % once we are at FirstCols
    filler(HeadLen, Head),
    filler(30, Tail),
    atomic_list_concat([Head, ' ', Tail], Xs),
    type_and_wait(T, Xs),
    %  The first resize repaints, and the repaint is where libedit
    %  would drop the trailing space; the second one rewraps whatever
    %  structure that left behind.
    resize_cols(T, FirstCols, _),
    TargetCols = 68,
    resize_cols(T, TargetCols, NewCols),
    assertion(NewCols == TargetCols),
    prompt_row(T, Prompt, PromptRow),
    AboveRow is PromptRow - 1,
    row_text(T, AboveRow, Above1),
    (   Above1 == Above0
    ->  true
    ;   format(user_error,
               "line above the prompt: expected ~q, got ~q~n",
               [Above0, Above1]),
        assertion(Above1 == Above0)
    ),
    atom_length(Xs, Len),
    RowsNeeded is (P + Len + NewCols - 1) // NewCols,
    rows_of(T, PromptRow, RowsNeeded, DisplayRows),
    assert_single_prompt(DisplayRows),
    DisplayRows = [First|Rest],
    strip_prompt(First, FirstTail),
    maplist(trim_trailing_spaces, [FirstTail|Rest], Trimmed),
    atomic_list_concat(Trimmed, Joined),
    normalize_space(atom(JoinedN), Joined),
    normalize_space(atom(XsN), Xs),
    assertion(JoinedN == XsN).

test(edit_wrapped_input_after_resize, [setup(resize_test_begin(T))]) :-
    %  Resize the window, then edit an input line that wraps.  Every
    %  cursor motion libedit makes is computed from the column count it
    %  believes the terminal has, so if the resize never reached it the
    %  redraw lands on the wrong row: ^A repaints the head of the line
    %  over its last row instead of moving to the prompt.  Reported for
    %  Epilog on Windows, where a resize raises no SIGWINCH.
    cursor(T, P, R),
    TargetCols = 100,
    resize_cols(T, TargetCols, NewCols),
    assertion(NewCols == TargetCols),
    Len = 200,
    filler(Len, Xs),
    %  type_and_wait/2 waits for the cursor position it derives from
    %  the column count, which is what this test is checking; wait for
    %  the position we computed from NewCols instead: the last cell of
    %  the input is P+Len-1.
    LastCell is P + Len - 1,
    ExpRow is R + LastCell // NewCols,
    ExpCol is LastCell mod NewCols + 1,
    assertion(ExpCol < NewCols),
    type(T, Xs),
    wait_until(cursor_at(T, ExpCol, ExpRow), 5),
    %  Move to the start of the line: the cursor must land on the
    %  prompt row, not somewhere in the wrapped tail.
    key(T, ctrl_a),
    drive(0.2),
    assert_cursor(T, P, R),
    %  Replace the character at offset 5 and check the whole line.
    key(T, cursor_right),
    key(T, cursor_right),
    key(T, cursor_right),
    key(T, cursor_right),
    key(T, cursor_right),
    key(T, backspace),
    type(T, 'Z'),
    drive(0.2),
    sub_atom(Xs, 0, 4, _, Head),
    sub_atom(Xs, 5, _, 0, Tail),
    atomic_list_concat([Head, 'Z', Tail], Expected),
    RowsNeeded is (P + Len + NewCols - 1) // NewCols,
    rows_of(T, R, RowsNeeded, DisplayRows),
    assert_single_prompt(DisplayRows),
    DisplayRows = [First|Rest],
    strip_prompt(First, FirstTail),
    maplist(trim_trailing_spaces, [FirstTail|Rest], Trimmed),
    atomic_list_concat(Trimmed, Joined),
    assertion(Joined == Expected).

test(resize_below_window_scrolls, [setup(resize_test_begin(T))]) :-
    %   Type a long input and then shrink the terminal so the
    %   wrapped input needs more rows than the window holds.  The
    %   cursor is at the end of the input, so xpce must scroll the
    %   view down until the cursor lands on the last visible row.
    %   Any rows above the cursor that were part of the original
    %   unscrolled layout should have slid into scrollback.
    %
    %   Assertions are kept lenient because the exact row-0 offset
    %   depends on xpce's reflow vs. libedit's redraw interaction;
    %   the key properties are:
    %     - cursor's row is the last visible row,
    %     - the visible rows form a contiguous slice of the input,
    %     - that slice ends at the tail of the input,
    %     - no prompt is visible (it's scrolled off).
    cursor(T, P, _R),
    WindowSize = 25,
    TargetCols = 25,
    %   Pick total chars as an exact multiple of NewCols so the last
    %   visible row is either full (25 chars, pending-wrap cursor)
    %   or empty (auto-wrap cursor landed on a new line).
    TotalRows = 32,
    TotalChars is TotalRows * TargetCols,
    Len is TotalChars - P,
    filler(Len, Xs),
    type_and_wait(T, Xs),
    resize_cols(T, TargetCols, NewCols),
    assertion(NewCols == TargetCols),
    assertion(TotalRows > WindowSize),
    LastRow is WindowSize - 1,
    %   Cursor row is the last visible row (column may be 0 or
    %   NewCols depending on pending-wrap vs auto-wrap).
    cursor(T, _CursorCol, CursorRow),
    assertion(CursorRow =:= LastRow),
    %   The visible rows (trimmed, prompt-stripped if any) form a
    %   contiguous slice of the input that ends at the last input
    %   char.  We walk upward from the last non-empty row and
    %   accumulate rows into a concatenated atom; that atom must be
    %   a suffix of Xs.
    rows_of(T, 0, WindowSize, AllVisible),
    visible_suffix(AllVisible, Suffix),
    atom_length(Suffix, SuffixLen),
    %   Suffix must cover at least one full window's worth of input,
    %   demonstrating that we scrolled (not just truncated).
    ContentRowsAboveWindow is TotalRows - WindowSize,
    MinSuffix is ContentRowsAboveWindow * NewCols,
    assertion(SuffixLen >= MinSuffix),
    atom_length(Xs, XsLen),
    TailStart is XsLen - SuffixLen,
    sub_atom(Xs, TailStart, SuffixLen, 0, ExpectedTail),
    assertion(Suffix == ExpectedTail),
    %   The prompt scrolled off — none of the visible rows carry
    %   the "?- " marker.
    count_prompt_occurrences(AllVisible, NPrompts),
    assertion(NPrompts == 0).

%!  visible_suffix(+Rows, -Concatenated) is det.
%
%   Trim trailing empty rows, then concatenate the remaining rows
%   (each trimmed of trailing whitespace).  Used by the scroll test
%   to reconstruct the visible slice of input for comparison with
%   the typed content's tail.

visible_suffix(Rows, Concatenated) :-
    maplist(trim_trailing_spaces, Rows, Trimmed),
    exclude_trailing_empty(Trimmed, Kept),
    atomic_list_concat(Kept, Concatenated).

exclude_trailing_empty(List, Kept) :-
    reverse(List, R),
    drop_empty_prefix(R, RKept),
    reverse(RKept, Kept).

drop_empty_prefix([''|T], R) :- !, drop_empty_prefix(T, R).
drop_empty_prefix(L, L).

:- end_tests(terminal_resize).


		 /*******************************
		 *      TEST: WRAPPED INPUT     *
		 *******************************/

:- begin_tests(terminal_wrap,
               [ setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

%   Terminal width is 80 columns (see term_start/2).  With a prompt
%   of width P (captured per test), the first input row can hold
%   80 - P columns before wrapping to the next row.  When the cursor
%   reaches the edge it moves to column 0 of the next row (no
%   deferred-wrap position is observable through <-cursor_position).

%!  wrap_emoji(-Emoji) is det.
%
%   Local copy of the emoji atom so this unit does not depend on
%   emoji/1 defined in the terminal_wide unit.

wrap_emoji(E) :-
    atom_codes(E, [0x1F929, 0xFE0F]).

%!  type_await(+Terminal, +Text, +ExpCol, +ExpRow) is det.
%
%   Inject Text and pump events until the cursor reaches
%   (ExpCol, ExpRow) or a timeout.  Succeeds even on timeout — the
%   following assert_cursor/3 will report the actual position for
%   diagnosis.

type_await(T, Text, ExpCol, ExpRow) :-
    type(T, Text),
    ignore(wait_until(cursor_at(T, ExpCol, ExpRow), 5)).

cursor_at(T, C, R) :-
    cursor(T, Cc, Rc),
    Cc =:= C, Rc =:= R.


test(input_fills_first_row_exactly, [setup(test_begin(T))]) :-
    %  Typing exactly 80-P narrow characters fills the row to its
    %  right edge.  On an xenl terminal (and xpce's delayed wrap) the
    %  cursor stays in the pending-wrap state at (80, R); the physical
    %  move to (0, R+1) only happens when the NEXT base arrives.
    cursor(T, P, R),
    margin_col(T, Margin),
    Fill is 80 - P,
    filler(Fill, Xs),
    type_await(T, Xs, 80, R),
    assert_cursor(T, Margin, R).

test(input_wraps_one_char_past_row, [setup(test_begin(T))]) :-
    %  One extra character past 80-P lands at column 1 of the next row.
    cursor(T, P, R),
    Fill is 80 - P + 1,
    filler(Fill, Xs),
    R2 is R + 1,
    type_await(T, Xs, 1, R2),
    assert_cursor(T, 1, R2).

test(input_wraps_multiple_cols, [setup(test_begin(T))]) :-
    %  Cursor lands at (N - (80 - P), R+1) after typing N chars.
    cursor(T, P, R),
    N = 100,
    filler(N, Xs),
    R2 is R + 1,
    ExpCol is N - (80 - P),
    type_await(T, Xs, ExpCol, R2),
    assert_cursor(T, ExpCol, R2).

test(home_end_on_wrapped_input, [setup(test_begin(T))]) :-
    %  ^A returns to the prompt column on the original row; ^E returns
    %  to the wrapped-line end on the next row.
    cursor(T, P, R),
    N = 100,
    filler(N, Xs),
    R2 is R + 1,
    ExpCol is N - (80 - P),
    type_await(T, Xs, ExpCol, R2),
    key(T, ctrl_a),
    assert_cursor(T, P, R),
    key(T, ctrl_e),
    assert_cursor(T, ExpCol, R2).

test(cursor_right_across_wrap, [setup(test_begin(T))]) :-
    %  Advancing one cursor-right at a time from the prompt: after
    %  (80 - P) right-moves we land at column 0 of the next row.
    cursor(T, P, R),
    Fill is 80 - P + 1,                      % one char beyond end of row
    filler(Fill, Xs),
    R2 is R + 1,
    type_await(T, Xs, 1, R2),
    key(T, ctrl_a),
    assert_cursor(T, P, R),
    Steps is 80 - P,
    %  Send all cursor_rights at once and drive once at the end — far
    %  faster than drive/1 after each individual key.
    key_bytes(T, cursor_right, Bytes),
    length(Runs, Steps),
    maplist(=(Bytes), Runs),
    append(Runs, All),
    atom_codes(Burst, All),
    term_send(T, Burst),
    drive(0.5),
    assert_cursor(T, 0, R2),
    key(T, cursor_right),
    assert_cursor(T, 1, R2).

test(wide_char_prewraps_at_row_edge,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    %  Fill the row leaving exactly one column empty (cursor at col 79
    %  on row R), then type a wide emoji.  It does not fit in the
    %  remaining single column so it pre-wraps: the last cell of row R
    %  is padded with a space and the emoji lands at columns 0-1 of
    %  row R+1.
    cursor(T, P, R),
    Fill is 80 - P - 1,                      % leave 1 col empty on row R
    filler(Fill, Xs),
    At is P + Fill,
    type_await(T, Xs, At, R),
    assert_cursor(T, At, R),
    wrap_emoji(E),
    R2 is R + 1,
    type_await(T, E, 2, R2),
    assert_cursor(T, 2, R2).


%!  nfd_as(+N, -Atom) is det.
%
%   Atom of N 'à' grapheme clusters (each 'a'+U+0300 — 1 col, 2 cp).

nfd_as(N, Atom) :-
    nfd_codes(N, Codes),
    atom_codes(Atom, Codes).

nfd_codes(0, []) :- !.
nfd_codes(N, [0'a, 0x300 | T]) :-
    N > 0, N1 is N - 1,
    nfd_codes(N1, T).

test(nfd_fills_first_row_exactly,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    %  Typing exactly (80-P) NFD clusters fills the row to its visual
    %  edge.  Same pending-wrap semantics as the narrow fill test:
    %  cursor waits at (80, R) until the next base arrives.  Before
    %  the wrap fix this cursor landed around column 40 because
    %  caret_x (cell index) hit the wrap threshold at the visual
    %  midpoint of the buffer.
    cursor(T, P, R),
    Fill is 80 - P,
    nfd_as(Fill, Atom),
    type_await(T, Atom, 80, R),
    assert_cursor(T, 80, R).

test(nfd_one_cluster_wraps_to_next_row,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    %  (80-P)+1 NFD clusters: last one should land at column 0 of the
    %  next row, cursor at column 1.  Currently fails because the wrap
    %  triggers much earlier and far more than one cluster ends up on
    %  the wrapped row.
    cursor(T, P, R),
    Fill is 80 - P + 1,
    nfd_as(Fill, Atom),
    R2 is R + 1,
    type_await(T, Atom, 1, R2),
    assert_cursor(T, 1, R2).

test(nfd_cluster_kept_whole_at_wrap_boundary,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    %  When typing one NFD cluster more than fits on row R, the extra
    %  cluster must appear as a complete `à` on row R+1 — not a bare
    %  base with a dropped combiner, and not a stray combiner orphaned
    %  at column 0.
    cursor(T, P, R),
    Fill is 80 - P + 1,
    nfd_as(Fill, Atom),
    R2 is R + 1,
    type_await(T, Atom, 1, R2),
    nfd_as(1, OneCluster),
    assert_row(T, R2, OneCluster).

test(cursor_left_across_wrap_nfd,
     [ condition(needs([combining])),
       setup(test_begin(T))
     ]) :-
    %  After filling row R with (80-P) clusters and wrapping one more
    %  onto R+1, two cursor-lefts should land on the last cluster of
    %  row R (column 79).  libedit currently mistracks the NFD column
    %  width after a wrap, so the reported column is P + (80-P)/2
    %  rather than 79.  This is a libedit bug, not an xpce one.
    cursor(T, P, R),
    Fill is 80 - P + 1,
    nfd_as(Fill, Atom),
    R2 is R + 1,
    type_await(T, Atom, 1, R2),
    key(T, cursor_left),
    assert_cursor(T, 0, R2),
    key(T, cursor_left),
    LastCol is 80 - 1,
    assert_cursor(T, LastCol, R).

test(edit_at_right_margin_stays_on_row,
     [ setup(test_begin(T))
     ]) :-
    %  Fill the row exactly to the right margin, then replace the last
    %  character.  The replacement must appear on the input row.
    %
    %  This is the Windows-only regression reported on Discourse (thread
    %  "Progressing the SWI-Prolog environment", post 101): the new
    %  character showed up on the row *above* the line being edited, and
    %  from there on every redraw was anchored one row too high, so the
    %  screen filled up from the bottom.
    %
    %  xpce's terminal implements xterm's delayed wrap: the base that
    %  lands in the last column leaves the caret at column 80 with the
    %  wrap still pending.  libedit only resolves that pending wrap (by
    %  writing ' ' and backspacing over it) when the terminal
    %  description advertises `xn`.  The fake termcap libedit uses on
    %  Windows, packages/libedit/libedit/src/win_ncurses.c, reports `am`
    %  but not `xn`, so libedit assumed the terminal had wrapped by
    %  itself and every following cursor motion -- which cancels the
    %  pending wrap -- acted one row too high.  On Unix library(epilog)
    %  forces TERM=xterm, which has xenl, so this only ever failed on
    %  Windows.
    %
    %  test_begin/1 ends with ^L, which leaves the prompt on row 0.  Run
    %  a trivial goal first so that there *is* a row above the input row
    %  for a misplaced character to land on.
    type(T, 'true.'),
    key(T, enter),
    wait_for_prompt(T),
    cursor(T, P, R),
    assertion(R > 0),
    Fill is 80 - P,
    filler(Fill, Xs),
    type_await(T, Xs, 80, R),
    key(T, backspace),
    type(T, '1'),
    Head is Fill - 1,
    sub_atom(Xs, 0, Head, _, Prefix),
    atom_concat(Prefix, '1', Expected),
    assert_input(T, R, Expected),
    margin_col(T, Margin),
    assert_cursor(T, Margin, R).

:- end_tests(terminal_wrap).


		 /*******************************
		 *      CHILD ON THE TERMINAL   *
		 *******************************/

/** <section> A process started by the Prolog thread runs on the window

    shell/1 hands the child the terminal the calling thread runs on.  On
    POSIX that is the pty and the kernel does the rest.  Windows has no
    pty: the child is put on the pseudo console of the window instead,
    which the terminal hands out for as long as the child holds it.

    Which is why these tests are worth having on both platforms: they
    say nothing about how it is done, only that the child ends up on
    the window, and every Windows attempt at this failed in a way this
    unit would have caught.
*/

:- begin_tests(terminal_child_on_terminal,
               [ condition(needs([child_on_terminal])),
                 setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

test(child_writes_to_the_window, [setup(test_begin(T))]) :-
    echo_command('CHILD-STDOUT', Cmd),
    format(atom(Goal), 'shell("~w", _).\n', [Cmd]),
    term_send(T, Goal),
    assertion(wait_until(marker_on_screen(T, 'CHILD-STDOUT'), 30)),
    wait_for_prompt(T).

test(child_exit_status_reaches_prolog, [setup(test_begin(T))]) :-
    exit_command(3, Cmd),
    format(atom(Goal), 'shell("~w", St), format("STATUS=~~w~~n", [St]).\n',
           [Cmd]),
    term_send(T, Goal),
    assertion(wait_until(marker_on_screen(T, 'STATUS=3'), 30)),
    wait_for_prompt(T).

%  The direction that is easy to get wrong: what is typed at the window
%  must reach the child, not the Prolog thread that started it.  Echo is
%  off, so the text can only appear on the screen by going through the
%  child and coming back.

test(child_reads_what_is_typed, [setup(test_begin(T))]) :-
    start_interactive_shell(T),
    arithmetic_command(Cmd, Answer),
    term_type_keys(T, Cmd),
    term_key_press(T, 'RET'),
    assertion(wait_until(marker_on_screen(T, Answer), 30)),
    quit_interactive_shell(T).

%  What is typed must appear as it is typed.  A console shows it itself
%  and so does the line discipline on POSIX; a Windows terminal is
%  neither, so there the console the child runs on does it.  Nothing is
%  submitted, so only an echo can put it on the screen.

test(typing_is_echoed, [setup(test_begin(T))]) :-
    start_interactive_shell(T),
    term_type_keys(T, 'ECHOED-BACK'),
    assertion(wait_until(marker_on_screen(T, 'ECHOED-BACK'), 30)),
    term_key_press(T, 'RET'),		% let the shell make of it what it will
    wait(0.5),
    quit_interactive_shell(T).

%  The Prolog thread hands its terminal over for the child and must get
%  it back: on Windows the console it hands out reads the very pipe the
%  thread reads.  Run enough children that a stuck one shows up.

test(terminal_still_works_after_many_children, [setup(test_begin(T))]) :-
    echo_command('ROUND', Cmd),
    format(atom(Goal), 'shell("~w", _).\n', [Cmd]),
    forall(between(1, 20, _),
           ( term_send(T, Goal),
             assertion(wait_until(at_prompt(T), 30)) )),
    key(T, ctrl_l),
    wait_for_prompt(T),
    type(T, 'X is 6*7.'),
    key(T, enter),
    assertion(wait_until(marker_on_screen(T, 'X = 42'), 30)).

%!  echo_command(+Text, -Command) is det.
%!  exit_command(+Status, -Command) is det.
%
%   A shell/1 command that prints Text, respectively exits with Status.
%   shell/1 runs the POSIX shell on Unix and the command line as given
%   on Windows, so the two need different words for the same thing.

echo_command(Text, Command) :-
    (   current_prolog_flag(windows, true)
    ->  format(atom(Command), 'cmd /c echo ~w', [Text])
    ;   format(atom(Command), 'echo ~w', [Text])
    ).

exit_command(Status, Command) :-
    (   current_prolog_flag(windows, true)
    ->  format(atom(Command), 'cmd /c exit ~w', [Status])
    ;   format(atom(Command), 'exit ~w', [Status])
    ).

%!  start_interactive_shell(+T) is det.
%!  quit_interactive_shell(+T) is det.
%
%   Run an interactive shell on the terminal and leave it again.  A shell
%   rather than something like `sort' because it ends on a command: there
%   is no way to send end of input to a child on Windows, where the
%   terminal is a pipe and ^Z is a convention of the console rather than
%   something a pipe can carry.

start_interactive_shell(T) :-
    (   current_prolog_flag(windows, true)
    ->  Shell = cmd
    ;   Shell = sh
    ),
    format(atom(Goal), 'shell("~w", _).\n', [Shell]),
    term_send(T, Goal),
    wait(1).

quit_interactive_shell(T) :-
    term_type_keys(T, exit),
    term_key_press(T, 'RET'),
    assertion(wait_until(at_prompt(T), 30)).

%!  arithmetic_command(-Command, -Answer) is det.
%
%   A command whose answer appears nowhere in the command itself, so that
%   seeing the answer means the child read the line and ran it.  Echoing
%   what was typed, whoever does the echoing, cannot produce it.

arithmetic_command(Command, '56088') :-
    (   current_prolog_flag(windows, true)
    ->  Command = 'set /a 123*456'
    ;   Command = 'echo $((123*456))'
    ).

:- end_tests(terminal_child_on_terminal).


		 /*******************************
		 *        CONTROL KEYS          *
		 *******************************/

/** <section> Control keys and the process running in the terminal

    While a process group of another session owns the pty, the control
    keys belong to that process: ^C must reach it as an interrupt and
    ^X as input, rather than acting on the window.  With no such
    process the window keeps its own bindings, which is what lets ^C
    interrupt the Prolog thread that runs on this terminal.  See
    clientOwnsKeyTerminalImage() in packages/xpce/src/txt/terminal.c.

    These tests press keys with press/2 rather than type/2: only what
    goes through ->typed passes the key bindings, and the bindings are
    what is under test.
*/

:- begin_tests(terminal_control_keys,
               [ condition(needs([pty_signals])),
                 setup(setup_unit),
                 cleanup(cleanup_unit)
               ]).

test(toplevel_has_no_foreground_process, [setup(test_begin(T))]) :-
    \+ term_foreground_process(T, _).

test(child_becomes_the_foreground_process,
     [ setup(test_begin(T)),
       cleanup(stop_foreground(T))
     ]) :-
    start_foreground(T, 'sleep 30'),
    term_foreground_process(T, PID),
    assertion(integer(PID)).

test(control_c_interrupts_the_child, [setup(test_begin(T))]) :-
    start_foreground(T, 'sleep 30'),
    press(T, ctrl_c),
    assertion(wait_until(\+ term_foreground_process(T, _), 15)),
    wait_for_prompt(T).

test(control_x_reaches_the_child,
     [ setup(test_begin(T)),
       cleanup(stop_foreground(T))
     ]) :-
    echo_client(Cmd),
    start_foreground(T, Cmd),
    press(T, ctrl_x),
    key(T, enter),
    assertion(wait_until(marker_on_screen(T, '^X'), 15)).

:- end_tests(terminal_control_keys).


		 /*******************************
		 *        RANDOM TESTING        *
		 *******************************/

/** <section> Random testing

    test_terminal_random(+N, +M) runs N independent sessions (each
    starting from a fresh empty input line) of M commands each.  A
    command is either typing one grapheme cluster or sending a libedit
    editing key.  After EVERY command we compare the terminal's reported
    cursor and the content of the input row against a pure-Prolog
    model of where they should be.

    v1 stays on a single input row: the random generator rejects any
    `type` command that would push the cursor past column W-1.  Wrap
    support is a follow-up.

    On the first divergence we print the seed, the full command
    history of the failing session, the expected state and the actual
    state, then throw.  The seed + history is the minimum information
    needed to reproduce or minimise the failure by hand.
*/

%!  test_terminal_random(+N, +M) is det.
%!  test_terminal_random(+N, +M, +Options) is det.
%
%   Run N random sessions of M commands each.  Options:
%
%     - seed(Seed)
%       A term acceptable to set_random/1 (e.g. `random` or an integer
%       seed).  Default: a fresh random state.
%     - verbose(Bool)
%       If true, log every command and verify outcome.
%
%   Throws `terminal_random_failure(Info)` on the first divergence.

test_terminal_random(N, M) :-
    test_terminal_random(N, M, []).

test_terminal_random(N, M, Options) :-
    must_be(positive_integer, N),
    must_be(nonneg, M),
    option(seed(Seed), Options, random),
    option(verbose(Verbose), Options, false),
    option(backend(Backend), Options, epilog),
    set_random(seed(Seed)),
    format("test_terminal_random: seed=~q sessions=~w commands=~w backend=~q~n",
           [Seed, N, M, Backend]),
    setup_call_cleanup(
        nb_setval(terminal_backend, Backend),
        setup_call_cleanup(setup_unit,
                           run_random_sessions(N, M, Verbose),
                           cleanup_unit),
        nb_delete(terminal_backend)).

run_random_sessions(0, _, _) :- !.
run_random_sessions(N, M, Verbose) :-
    N > 0,
    current_test_terminal(T),
    reset_input(T),
    cursor(T, P, R),
    row_text(T, R, PromptLine),
    State0 = state([], 0),
    (   Verbose == true
    ->  format("==== session ~w: prompt=~q at (~w,~w) ====~n",
               [N, PromptLine, P, R])
    ;   true
    ),
    random_commands(M, T, P, R, PromptLine, State0, [], Verbose),
    N1 is N - 1,
    run_random_sessions(N1, M, Verbose).

random_commands(0, _, _, _, _, _, _, _) :- !.
random_commands(K, T, P, R, Prompt, State0, History, Verbose) :-
    K > 0,
    random_command(State0, P, Cmd),
    apply_model(Cmd, State0, State1),
    History1 = [Cmd|History],
    apply_terminal(Cmd, T),
    wait_verified(T, P, R, Prompt, State1, Outcome),
    (   Outcome == ok
    ->  (   Verbose == true
        ->  format("  ok  ~q~n", [Cmd])
        ;   true
        ),
        K1 is K - 1,
        random_commands(K1, T, P, R, Prompt, State1, History1, Verbose)
    ;   reverse(History1, HistF),
        report_failure(T, P, R, Prompt, State1, HistF, Outcome),
        throw(terminal_random_failure(
                  info{ history: HistF,
                        expected: State1,
                        divergence: Outcome }))
    ).


		 /*******************************
		 *           MODEL              *
		 *******************************/

%   state(Clusters, Cursor):
%   - Clusters: list of cluster(Codes, VCols).  Codes is a list of
%     Unicode code points, VCols is the visual width (1 for narrow
%     and NFD, 2 for wide).
%   - Cursor: integer 0..length(Clusters) — the insertion point.

%!  apply_model(+Command, +State0, -State1) is det.

apply_model(type(Cluster),   state(Cs, I), state(Cs1, I1)) :-
    nth0_insert(I, Cluster, Cs, Cs1),
    I1 is I + 1.
apply_model(cursor_left,     state(Cs, I), state(Cs, I1)) :-
    I1 is max(0, I - 1).
apply_model(cursor_right,    state(Cs, I), state(Cs, I1)) :-
    length(Cs, Len),
    I1 is min(Len, I + 1).
apply_model(home,            state(Cs, _), state(Cs, 0)).
apply_model(end,             state(Cs, _), state(Cs, Len)) :-
    length(Cs, Len).
apply_model(backspace,       state(Cs, I), state(Cs1, I1)) :-
    (   I > 0
    ->  I1 is I - 1,
        nth0_delete(I1, Cs, Cs1)
    ;   Cs1 = Cs, I1 = I
    ).
apply_model(delete,          state(Cs, I), state(Cs1, I)) :-
    length(Cs, Len),
    (   I < Len
    ->  nth0_delete(I, Cs, Cs1)
    ;   Cs1 = Cs
    ).

nth0_insert(0, X, L, [X|L]) :- !.
nth0_insert(N, X, [H|T], [H|T1]) :-
    N > 0, N1 is N - 1,
    nth0_insert(N1, X, T, T1).

nth0_delete(0, [_|T], T) :- !.
nth0_delete(N, [H|T], [H|T1]) :-
    N > 0, N1 is N - 1,
    nth0_delete(N1, T, T1).


		 /*******************************
		 *          LAYOUT              *
		 *******************************/

%!  model_layout(+Clusters, +Cursor, +P, +R, +W,
%!               -CurCol, -CurRow, -RowGroups) is det.
%
%   Lay out Clusters starting at visual column P of row R on a
%   terminal of width W columns, then report where the cursor lands
%   (CurCol, CurRow) and group the clusters by the row they
%   eventually occupy (RowGroups is a list of Row-ClusterList pairs,
%   sorted by Row).
%
%   The cursor at index I sits AFTER the first I clusters — i.e. at
%   the position the (I+1)th cluster would occupy, or at the
%   post-placement tip when I == length(Clusters).  After exactly
%   filling a row the cursor stays at (W, R) pending-wrap; it only
%   physically moves to (0, R+1) once a further cluster actually
%   lands there.

model_layout(Clusters, Cursor, P, R, W, CurCol, CurRow, RowGroups) :-
    length(Before, Cursor),
    append(Before, _, Clusters),
    layout_end_pos(Before, P, R, W, CurCol, CurRow),
    layout_all(Clusters, P, R, W, Placements),
    group_pairs_by_key(Placements, RowGroups).

layout_end_pos([], Col, Row, _, Col, Row).
layout_end_pos([cluster(_, CW)|Cs], Col, Row, W, EndCol, EndRow) :-
    (   Col + CW > W
    ->  NewCol = CW, NewRow is Row + 1
    ;   NewCol is Col + CW, NewRow = Row
    ),
    layout_end_pos(Cs, NewCol, NewRow, W, EndCol, EndRow).

layout_all([], _, _, _, []).
layout_all([C|Cs], Col, Row, W, [PlaceRow-C|Ps]) :-
    C = cluster(_, CW),
    (   Col + CW > W
    ->  PlaceCol = 0, PlaceRow is Row + 1
    ;   PlaceCol = Col, PlaceRow = Row
    ),
    NewCol is PlaceCol + CW,
    layout_all(Cs, NewCol, PlaceRow, W, Ps).

sum_widths([], Acc, Acc).
sum_widths([cluster(_, W)|T], Acc, Sum) :-
    Acc1 is Acc + W,
    sum_widths(T, Acc1, Sum).

%!  model_row_text(+PromptLine, +PromptCol, +PromptRow, +RowNum,
%!                 +RowClusters, -Atom) is det.
%
%   Build the expected text for row RowNum.  For the prompt row we
%   prefix with the captured prompt (truncated to PromptCol chars —
%   what's visible before any input landed).  Other rows contain just
%   the concatenated cluster code points — libedit doesn't pad, and
%   xpce's <-row stops at tl->size, so we match that.

model_row_text(PromptLine, P, PromptRow, RowNum, Clusters, Atom) :-
    clusters_codes(Clusters, Codes),
    atom_codes(InputAtom, Codes),
    (   RowNum =:= PromptRow
    ->  (   sub_atom(PromptLine, 0, P, _, PromptPrefix)
        ->  true
        ;   PromptPrefix = PromptLine
        ),
        atom_concat(PromptPrefix, InputAtom, Atom)
    ;   Atom = InputAtom
    ).

clusters_codes([], []).
clusters_codes([cluster(CodePts, _)|T], Codes) :-
    append(CodePts, Rest, Codes),
    clusters_codes(T, Rest).


		 /*******************************
		 *        RANDOM COMMANDS       *
		 *******************************/

%!  random_command(+State, +PromptCol, -Command) is det.
%
%   Pick a random command.  Heavily biased toward typing so the buffer
%   grows.  Caps total content width at 900 visual columns so the
%   buffer (including prompt) never wraps off the bottom of a 25-row
%   terminal (~11 rows of content leaves plenty of margin).

random_command(state(Cs, Cursor), _P, Cmd) :-
    length(Cs, Len),
    sum_widths(Cs, 0, UsedW),
    MaxContent = 900,
    Remaining is MaxContent - UsedW,
    edit_weights(Len, Cursor, EditWeights),
    (   Remaining >= 2
    ->  Weights = [60-type|EditWeights]
    ;   Weights = EditWeights
    ),
    weighted_pick(Weights, Kind),
    (   Kind == type
    ->  random_typeable(Remaining, Cluster),
        Cmd = type(Cluster)
    ;   Cmd = Kind
    ).

%!  edit_weights(+BufLen, +Cursor, -Weights) is det.
%
%   Available edit commands, tagged with selection weights.  Commands
%   that would be no-ops in the current state are omitted so the pick
%   is more productive.

edit_weights(Len, Cursor, Ws) :-
    findall(Weight-Cmd, edit_candidate(Len, Cursor, Cmd, Weight), Ws0),
    (   Ws0 == []
    ->  Ws = [1-end]                   % always available
    ;   Ws = Ws0
    ).

edit_candidate(_,    Cursor, cursor_left,  10) :- Cursor > 0.
edit_candidate(Len,  Cursor, cursor_right, 10) :- Cursor < Len.
edit_candidate(_,    Cursor, home,          5) :- Cursor > 0.
edit_candidate(Len,  Cursor, end,           5) :- Cursor < Len.
edit_candidate(_,    Cursor, backspace,     8) :- Cursor > 0.
edit_candidate(Len,  Cursor, delete,        8) :- Cursor < Len.

%!  random_typeable(+RemainingCols, -Cluster) is det.

random_typeable(Remaining, Cluster) :-
    (   Remaining >= 2
    ->  Choices = [5-ascii, 3-nfd, 2-wide]
    ;   Choices = [5-ascii, 3-nfd]
    ),
    weighted_pick(Choices, Kind),
    make_cluster(Kind, Cluster).

make_cluster(ascii, cluster([Code], 1)) :-
    random_between(0'a, 0'z, Code).
make_cluster(nfd, cluster([Base, 0x300], 1)) :-
    random_between(0'a, 0'z, Base).
make_cluster(wide, cluster([0x1F929], 2)).  % 🤩

%!  weighted_pick(+Weights, -Choice) is det.
%
%   Weights is a list of Weight-Item pairs.  Pick Item with probability
%   Weight / Total.

weighted_pick(Weights, Choice) :-
    pairs_keys(Weights, Keys),
    sum_list(Keys, Total),
    Total > 0,
    R is random_float * Total,
    pick_weighted(Weights, R, Choice).

pick_weighted([W-Item|Rest], R, Out) :-
    (   R < W
    ->  Out = Item
    ;   R1 is R - W,
        pick_weighted(Rest, R1, Out)
    ).


		 /*******************************
		 *       APPLY TO TERMINAL      *
		 *******************************/

%!  apply_terminal(+Command, +Terminal) is det.

apply_terminal(type(cluster(Codes, _)), T) :-
    atom_codes(Atom, Codes),
    term_send(T, Atom).
apply_terminal(cursor_left,  T) :- send_key(T, cursor_left).
apply_terminal(cursor_right, T) :- send_key(T, cursor_right).
apply_terminal(home,         T) :- send_key(T, home).
apply_terminal(end,          T) :- send_key(T, end).
apply_terminal(backspace,    T) :- send_key(T, backspace).
apply_terminal(delete,       T) :- send_key(T, delete).

send_key(T, Name) :-
    key_bytes(T, Name, Bytes),
    atom_codes(Atom, Bytes),
    term_send(T, Atom).


		 /*******************************
		 *         VERIFICATION         *
		 *******************************/

%!  wait_verified(+T, +P, +R, +Prompt, +State, -Outcome) is det.
%
%   Drive the event loop in small slices until the terminal matches the
%   model, up to ~1 s.  Returns `ok` on match, or the last observed
%   divergence if libedit never settled to the expected state.

wait_verified(T, P, R, Prompt, State, ok) :-
    between(1, 100, _),
    drive(0.01),
    verify_state(T, P, R, Prompt, State, ok),
    !.
wait_verified(T, P, R, Prompt, State, Outcome) :-
    verify_state(T, P, R, Prompt, State, Outcome).


%!  verify_state(+T, +P, +R, +Prompt, +State, -Outcome) is det.
%
%   Binds Outcome to `ok` when the terminal matches the model, or to a
%   `diff{...}` dict describing what disagreed.  The caller reports and
%   throws on any non-`ok` outcome.

verify_state(T, P, R, Prompt, state(Cs, Cursor), Outcome) :-
    term_cols(T, W),
    model_layout(Cs, Cursor, P, R, W, ExpCol, ExpRow, RowGroups),
    cursor(T, Col, Row),
    (   Col =:= ExpCol, Row =:= ExpRow
    ->  verify_rows(T, P, R, Prompt, RowGroups, Outcome)
    ;   collect_row_diffs(T, P, R, Prompt, RowGroups, RowDiffs),
        Outcome = diff{ expected_cursor: (ExpCol,ExpRow),
                        got_cursor:      (Col,Row),
                        row_diffs:       RowDiffs }
    ).

%!  verify_rows(+T, +P, +R, +Prompt, +RowGroups, -Outcome) is det.
%
%   Walk each Row-Clusters pair in RowGroups.  Returns `ok` if every
%   row's rendered text matches the model; otherwise a diff dict.

verify_rows(_T, _P, _R, _Prompt, [], ok).
verify_rows(T, P, R, Prompt, [RowNum-Clusters|Groups], Outcome) :-
    model_row_text(Prompt, P, R, RowNum, Clusters, ExpText),
    row_text(T, RowNum, GotText),
    (   ExpText == GotText
    ->  verify_rows(T, P, R, Prompt, Groups, Outcome)
    ;   Outcome = diff{ row:          RowNum,
                        expected_row: ExpText,
                        got_row:      GotText }
    ).

collect_row_diffs(_T, _P, _R, _Prompt, [], []).
collect_row_diffs(T, P, R, Prompt, [RowNum-Clusters|Groups], Diffs) :-
    model_row_text(Prompt, P, R, RowNum, Clusters, ExpText),
    row_text(T, RowNum, GotText),
    (   ExpText == GotText
    ->  Rest = Diffs
    ;   Diffs = [row(RowNum, ExpText, GotText)|Rest]
    ),
    collect_row_diffs(T, P, R, Prompt, Groups, Rest).

%!  report_failure(+T, +P, +R, +Prompt, +State, +History, +Divergence) is det.

report_failure(_T, P, R, Prompt, state(Cs, Cursor),
               History, Divergence) :-
    length(Cs, Len),
    format(user_error, "~n*** test_terminal_random DIVERGENCE ***~n", []),
    format(user_error, "prompt at (col=~w, row=~w), prompt line = ~q~n",
           [P, R, Prompt]),
    format(user_error, "model state: cursor=~w of ~w clusters~n",
           [Cursor, Len]),
    format(user_error, "clusters: ~q~n", [Cs]),
    (   get_dict(expected_cursor, Divergence, EC),
        get_dict(got_cursor,      Divergence, GC)
    ->  format(user_error, "expected cursor: ~q   got: ~q~n", [EC, GC])
    ;   true
    ),
    (   get_dict(row,             Divergence, RN),
        get_dict(expected_row,    Divergence, ER),
        get_dict(got_row,         Divergence, GR)
    ->  format(user_error, "row ~w expected: ~q~n", [RN, ER]),
        format(user_error, "row ~w got:      ~q~n", [RN, GR])
    ;   true
    ),
    (   get_dict(row_diffs, Divergence, RowDiffs), RowDiffs \== []
    ->  forall(member(row(RN, ER, GR), RowDiffs),
               ( format(user_error, "row ~w expected: ~q~n", [RN, ER]),
                 format(user_error, "row ~w got:      ~q~n", [RN, GR])
               ))
    ;   true
    ),
    format(user_error, "command history (in order):~n", []),
    forall(member(C, History),
           format(user_error, "    ~q~n", [C])).
