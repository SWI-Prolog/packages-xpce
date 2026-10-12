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

:- module(screenshot,
          [ settle/0,
            settle/1,                   % +Seconds
            last_frame/1                % -Frame
          ]).

% Make the screenshots independent of the preferences of the user who
% runs this.  These flags must be set before xpce is initialised.

:- set_prolog_flag(xpce_defaults, none).
:- set_prolog_flag(theme, light).

:- use_module(library(pce)).
:- use_module(library(main)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(option)).

/** <module> Create the screenshots of the XPCE User Guide

This program recreates the screenshots in `../figs` from the example
code in the User Guide.  Run it from the `man/userguide` directory as

    screenshots/shoot.sh swipl screenshots/screenshots.pl [Figure ...]

or using `make screenshots`.  Without arguments, all figures are made.
Use `--list` to list the known figures.  shoot.sh runs the program on
a private headless display.  If possible, this display has a scale of
2, such that the screenshots have twice the resolution of the logical
coordinates.

Figures are defined in the files in the subdirectory `figures`, using
the multifile predicate figure/2.  Each figure is written as a PDF
file that holds the image of the frame below a title bar showing the
label of the frame.  A logical pixel is printed as 0.6 point (see
pixel_size/1), so the text in the screenshots has about the size of the
running text of the guide.
*/

:- multifile
    figure/2,                       % +Name, -Visual
    figure_option/2.                % +Name, -Option

%!  figure(+Name, -Visual) is semidet.
%
%   Define a figure.  The clause body creates the GUI for the figure
%   and unifies Visual with a frame or a window of the frame to
%   capture.  The frame is opened by the driver if the body does not
%   open it.  If the figure needs interaction, such as selecting an
%   item, the body opens the frame itself, calls settle/0 and performs
%   the interaction.  After capturing, all frames are destroyed.
%   Visual can also be a list Frame-point(X,Y) to show multiple frames
%   in one figure.  See make_composite/2.

%!  figure_option(?Name, ?Option) is nondet.
%
%   Options for creating a figure.  Defined options are:
%
%     - title(Bool)
%       If `false`, do not add a title bar.  Default is `true`.
%     - title(Text)
%       Use Text as title rather than the label of the frame.
%     - settle(Seconds)
%       Time to process events after opening the frame.  Default 1.0.
%     - capture(Visual)
%       What to capture: `frame` (default) or `window`, the window
%       returned by the figure goal.

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, Positional, Options),
    load_figures,
    (   option(list(true), Options)
    ->  forall(figure_name(Name), writeln(Name))
    ;   (   Positional == []
        ->  findall(Name, figure_name(Name), Names)
        ;   Names = Positional
        ),
        foldl(make_figure, Names, 0, Errors),
        (   Errors =:= 0
        ->  true
        ;   format(user_error, '~D figures failed~n', [Errors]),
            halt(1)
        )
    ).

figure_name(Name) :-
    findall(Name, clause(figure(Name, _), _), Names0),
    sort(Names0, Names),
    member(Name, Names).

load_figures :-
    module_property(screenshot, file(File)),
    file_directory_name(File, Dir),
    directory_file_path(Dir, figures, FigDir),
    directory_file_path(FigDir, '*.pl', Pattern),
    expand_file_name(Pattern, Files),
    maplist(load_figure_file, Files).

load_figure_file(File) :-
    load_files(File, [if(not_loaded)]).

figs_dir(Dir) :-
    module_property(screenshot, file(File)),
    file_directory_name(File, ScreenDir),
    file_directory_name(ScreenDir, GuideDir),
    directory_file_path(GuideDir, figs, Dir).

%!  make_figure(+Name, +Errors0, -Errors) is det.

make_figure(Name, E0, E) :-
    format(user_error, '~w ... ', [Name]),
    (   catch(make_figure(Name), Error, true)
    ->  (   var(Error)
        ->  format(user_error, 'ok~n', []),
            E = E0
        ;   format(user_error, 'ERROR~n', []),
            print_message(error, Error),
            E is E0+1
        )
    ;   format(user_error, 'FAILED~n', []),
        E is E0+1
    ),
    cleanup_frames.

make_figure(Name) :-
    (   figure_name(Name)
    ->  true
    ;   existence_error(figure, Name)
    ),
    once(figure(Name, Visual)),
    (   is_list(Visual)
    ->  make_composite(Name, Visual)
    ;   make_single(Name, Visual)
    ).

make_single(Name, Visual) :-
    get(Visual, frame, Frame),
    (   get(Frame, status, unmapped)
    ->  send(Frame, open)
    ;   true
    ),
    option_value(Name, settle(Time), 1.0),
    settle(Time),
    option_value(Name, capture(What), frame),
    capture_image(What, Visual, Frame, Image),
    title(Name, Frame, Title),
    figs_dir(Dir),
    file_name_extension(Name, pdf, Base),
    directory_file_path(Dir, Base, File),
    get(Visual, area, area(_,_,LW,_)),
    (   What == window
    ->  LogicalWidth = LW
    ;   get(Frame, area, area(_,_,LogicalWidth,_))
    ),
    write_pdf(Image, LogicalWidth, Title, File).

option_value(Name, Option, _Default) :-
    figure_option(Name, Option),
    !.
option_value(_, Option, Default) :-
    arg(1, Option, Default).

title(Name, Frame, Title) :-
    (   figure_option(Name, title(T))
    ->  (   T == false
        ->  Title = @nil
        ;   T == true
        ->  get(Frame, label, Title)
        ;   Title = T
        )
    ;   get(Frame, label, Title)
    ).

capture_image(frame, _, Frame, Image) :-
    get(Frame, image, Image).
capture_image(window, Window, _, Image) :-
    get(Window, image, Image).

%!  make_composite(+Name, +Placements) is det.
%
%   Make a figure that shows multiple frames.  Placements is a list
%   of Frame-point(X,Y), where X,Y is the position of the top-left
%   corner of the frame (including its title bar) in the figure in
%   logical pixels.  Each frame gets a title bar with its label.
%   Frames later in the list are drawn on top of earlier ones.

make_composite(Name, Placements) :-
    forall(member(Frame-_, Placements),
           (   get(Frame, status, unmapped)
           ->  send(Frame, open)
           ;   true
           )),
    option_value(Name, settle(Time), 1.0),
    settle(Time),
    Placements = [Frame0-_|_],
    get(Frame0, image, Image0),
    get(Image0, size, size(IW0, _)),
    get(Frame0, area, area(_,_,FW0,_)),
    Scale is IW0/FW0,
    new(Dev, device),
    forall(member(Frame-point(X,Y), Placements),
           ( get(Frame, image, Image),
             get(Frame, label, Title),
             PX is round(X*Scale), PY is round(Y*Scale),
             new(FD, device),
             draw_frame(FD, Image, Title, Scale),
             send(Dev, display, FD, point(PX, PY))
           )),
    figs_dir(Dir),
    file_name_extension(Name, pdf, Base),
    directory_file_path(Dir, Base, File),
    pixel_size(PixelSize),
    Factor is PixelSize/Scale,
    send(Dev, pdf, file(File), Factor),
    send(Dev, destroy).

%!  settle is det.
%!  settle(+Seconds) is det.
%
%   Process events for Seconds (default 0.5), so the GUI is laid out
%   and painted.

settle :-
    settle(0.5).

settle(Time) :-
    get_time(T0),
    End is T0+Time,
    repeat,
       ignore(send(@display, dispatch)),
       get_time(T),
       T >= End,
    !.

%!  last_frame(-Frame) is semidet.
%
%   Frame is the most recently created frame.  Used to find the frame
%   of an example that does not return it.

last_frame(Frame) :-
    get(@display, frames, Frames),
    get(Frames, tail, Frame).

%!  cleanup_frames
%
%   Destroy all frames that are left after making a figure, so the next
%   figure starts in a clean state.

cleanup_frames :-
    get(@display_manager, members, Displays),
    chain_list(Displays, DL),
    forall(( member(D, DL),
             get(D, frames, Frames),
             chain_list(Frames, FL),
             member(F, FL)
           ),
           ( object(F) -> ignore(send(F, destroy)) ; true )),
    settle(0.2).


		 /*******************************
		 *          WRITE PDF           *
		 *******************************/

%!  pixel_size(-Points) is det.
%
%   Size of a logical pixel in the PDF, in points.  The default font of
%   xpce is 12 points at 96 DPI, i.e., 16 pixels.  At 0.6 points per
%   pixel its x-height is about that of the 11 point running text.

pixel_size(0.6).

%!  write_pdf(+Image, +LogicalWidth, +Title, +File) is det.
%
%   Write Image to File as PDF.  Image is a screenshot that may have a
%   higher resolution than the logical coordinates.  LogicalWidth is the
%   width of the captured area in logical pixels.  The PDF is scaled
%   such that a logical pixel is pixel_size/1 points.  If Title is not
%   @nil, a title bar is drawn above the image.

write_pdf(Image, LogicalWidth, Title, File) :-
    get(Image, size, size(IW, _)),
    Scale is IW/LogicalWidth,
    new(Dev, device),
    draw_frame(Dev, Image, Title, Scale),
    pixel_size(PixelSize),
    Factor is PixelSize/Scale,
    send(Dev, pdf, file(File), Factor),
    send(Dev, destroy).

%!  draw_frame(+Dev, +Image, +Title, +Scale) is det.
%
%   Draw Image below a title bar showing Title (unless @nil) and with
%   a border on Dev.

draw_frame(Dev, Image, Title, Scale) :-
    get(Image, size, size(IW, IH)),
    TitleH is round(22*Scale),
    (   Title == @nil
    ->  Y = 0
    ;   Y = TitleH,
        title_bar(Dev, IW, TitleH, Title, Scale)
    ),
    send(Dev, display, bitmap(Image), point(0, Y)),
    H is Y+IH,
    Pen is max(1, round(Scale)),
    send(Dev, display, new(B, box(IW, H))),
    send(B, pen, Pen),
    send(B, colour, grey60),
    send(B, fill, @nil).

title_bar(Dev, W, H, Title, Scale) :-
    send(Dev, display, new(B, box(W, H))),
    send(B, pen, 0),
    send(B, fill, colour(grey90)),
    Points is round(10*Scale),
    send(Dev, display, new(T, text(Title, center, font(sans, bold, Points)))),
    send(T, center, point(W/2, H/2)),
    send(Dev, display, new(L, line(0, H, W, H))),
    Pen is max(1, round(Scale)),
    send(L, pen, Pen),
    send(L, colour, grey60).
