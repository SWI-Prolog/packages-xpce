/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2002-2011, University of Amsterdam
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

:- module(pce_print_graphics, []).
:- use_module(library(pce)).
:- use_module(library(pce_template)).
:- use_module(library(pce_shell)).

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Public methods:

        ->print
        Prints the content of the Window as a single page

        ->save_pdf: [file], [directory]
        Save content of the Window as PDF
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(print_graphics, template,
                   "Template defining ->print").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Print  the image to the default  printer.  Also this  method should be
extended by requesting additional parameters from the user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print(Canvas) :->
    "Send to default printer"::
    print_canvas(Canvas).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Printing is done by rendering the window   to  a temporary PDF file and
handing this to the print spooler (see <-print_command_template).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_canvas(Canvas) :-
    get(Canvas, print_command, Command),
    setup_call_cleanup(
        new(PdfFile, file),             % ->initialise creates it open
        (   send(PdfFile, close),
            send(Canvas, pdf, PdfFile),
            get(PdfFile, absolute_path, File),
            get(string('%s "%s"', Command, File), value, ShellCommand),
            pce_shell_command('/bin/sh'('-c', ShellCommand))
        ),
        (   send(PdfFile, remove),
            send(PdfFile, done)
        )),
    send(Canvas, report, status, 'Sent to printer').


print_command(Canvas, Command:name) :<-
    "Ask the user for the command that prints the job"::
    get(Canvas, frame, Frame),
    get(Canvas, print_command_template, CmdTempl),
    print_cmd(CmdTempl, Cmd),
    new(D, dialog(print_command?label_name)),
    send(D, append, new(P, text_item(print_command, Cmd))),
    send(D, append, button(cancel, message(D, return, @nil))),
    send(D, append, button(ok, message(D, return, P?selection))),
    send(D, default_button, ok),
    send(D, transient_for, Frame),
    send(D, modal, transient),
    get(D, confirm_centered, Canvas?frame?area?center, Answer),
    send(D, destroy),
    Answer \== @nil,
    Command = Answer.

print_job_name(_, Job) :<-
    "Default name of the printer job"::
    Job = 'XPCE/SWI-Prolog'.

print_command_template(_, Command) :<-
    "Default command to send a job to the printer"::
    (   get(@pce, environment_variable, 'PRINTER', _)
    ->  Command = 'lpr -P%p'
    ;   Command = 'lpr'                 % lpr(1) uses the default queue
    ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
print_cmd(+Template, -Command) determines the shell command to execute in
order to get the job printed, replacing `%p' in Template by the printer
named by $PRINTER.  The substitutions are handled by a regex object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_cmd(Template, Cmd) :-
    new(S, string('%s', Template)),
    (   get(@pce, environment_variable, 'PRINTER', Printer)
    ->  substitute(S, '%p', Printer)
    ;   true
    ),
    get(S, value, Cmd),
    free(S).

substitute(S, F, T) :-
    new(R, regex(F)),
    send(R, for_all, S,
         message(@arg1, replace, @arg2, T)),
    free(R).


                 /*******************************
                 *              PDF             *
                 *******************************/


save_pdf(Canvas, File:file=[file], Directory:directory=[directory]) :->
    "Save content as PDF to File"::
    (   File == @default
    ->  get(@finder, file, save,
            chain(tuple('PDF', pdf)),
            Directory,
            FileName),
        new(PdfFile, file(FileName))
    ;   PdfFile = File           % <-file of @finder returns a name
    ),
    send(Canvas, pdf, PdfFile),
    send(Canvas, report, status, 'Saved PDF to %s', PdfFile).

:- pce_end_class(print_graphics).

