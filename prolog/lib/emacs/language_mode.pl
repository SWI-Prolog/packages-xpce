/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2025, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(emacs_language_mode, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend), []).
:- use_module(library(hyper)).
:- use_module(library(socket), [gethostname/1]).
:- use_module(library(debug)).
:- use_module(library(atom)).
:- autoload(library(epilog), [run_in_help_epilog/1]).
:- require([ auto_call/1,
             chain_list/2,
             default/3,
             foldl/4,
             ignore/1,
             member/2,
             pce_define_type/2,
             reverse/2
           ]).

:- emacs_begin_mode(language, fundamental,
                    "Edit (programming) languages",
        [ indent_line                   = key('TAB'),
          newline_and_indent		= key('\\ej'),
          backward_delete_char_untabify = key('BS'),
          align_close_bracket           = key(']') + key('}') + key(')'),
          insert_file_header            = key('\\C-c\\C-f'),
          insert_section_comment        = key('\\C-c\\C-h'),
          insert_long_comment           = key('\\C-c\\C-q'),
          insert_line_comment           = key('\\e;'),
          comment_set_column            = key('\\C-x;'),
          comment_line                  = key('\\C-x\\C-;'),
          close_block_comment           = key('/'),
          find_tag                      = key('\\e.') + button(browse),
          xref_go_back		= key('\\e,'),
          find_references               = key('\\e?') + button(browse),
          camelcase_word                = key('\\C-c\\C-c'),
          underscores_word              = key('\\C-c\\C--'),
          goto_next_error               = key('\\egn'),
          goto_prev_error               = key('\\egp'),

                                        % Commands for M-x
          align_comment                 = command,
          align_current                 = command,
          align_regexp                  = command,
          align_region                  = command,
          align_with_previous_line      = command,
          beginning_of_text_on_line     = command,
          comment_region                = command,
          kill_comment                  = command,
          prolog_manual                 = command,
          restyle_word                  = command,
          show_caret_line               = command,
          show_line_numbers             = command,
          tabularise                    = command,
          visit_tag_table               = command,
          what_syntax                   = command
        ],
        [ '"'  = string_quote('"'),
          '''' = string_quote(''''),
          paragraph_end(regex('\\s*\n|/\\* - - -|- - -.*\\*/\n'))
        ]).


variable(bracket_gen,           int*,  get,  "Generation we checked brackets").
variable(bracket_caret,         int*,  get,  "Location we checked brackets").
variable(comment_column,        int,   both, "Column for line comment").
variable(parameter_indentation, int,   both, "Indentation for parameters").
variable(show_line_numbers,     '0..',      get,
         "Show line numbers up to this character position (0: never)").

%!  .comment_column
%
%   Column at which a line comment is inserted after the code on a
%   line.  If a nearby line has a comment after its code that starts
%   between column 20 and 100, the comment is aligned with that one
%   instead.

class_variable(comment_column,        '0..',      48).

%!  .show_line_numbers
%
%   Show the line number of the caret in the status bar while the
%   caret is before this character position.  Further into the buffer
%   counting lines is considered too expensive.  If 0, line numbers
%   are never shown.

class_variable(show_line_numbers,     '0..',      250000).

%!  .parameter_indentation
%
%   Extra indentation for the arguments of a term if the line holding
%   the open bracket ends after the bracket.  The arguments are
%   indented relative to the start of the term.

class_variable(parameter_indentation, '0..',      4).

setup_mode(E) :->
    "Switch editor into fill-mode"::
    send_super(E, setup_mode),
    send(E, fill_mode, @on),
    send(E, style, matching_bracket, style(background := skyblue)).


                 /*******************************
                 *      COMMENT; HEADERS        *
                 *******************************/

:- pce_group(comment).

line_comment(E, CS:name) :<-
    "Fetch the line-comment start sequence"::
    get(E, syntax, Syntax),
    (   get(Syntax, line_comment, CS),
        CS \== @nil
    ->  true
    ;   member(CSlen, [1, 2]),
        get(Syntax, comment_start, CSlen, CS),
        get(Syntax, comment_end, CSlen, CE),
        send(CE, equal, '\n')
    ->  true
    ).

line_comment_column_in_context(E) :->
    get(E, line_comment_column_in_context, Col),
    send(E, report, inform, 'Column %d', Col).

line_comment_start(TB, Re, Caret, To, Start) :-
    send(Re, search, TB, Caret, To),
    get(Re, register_start, 1, Start).

line_comment_column_in_context(E, Col:int) :<-
    "Find the column for line comment nearby"::
    member(CSlen, [1, 2]),
    get(E?syntax, comment_start, CSlen, CS),
    !,
    get(E, caret, Caret),
    get(E, text_buffer, TB),
    get(TB, size, TBLen),
    get(regex(''), quote, CS, string(QCS)),
    format(string(ReS), '^\\s*\\S[^\n]*(~w)', [QCS]),
    new(Re, regex(ReS)),
    Start is max(Caret-10000, 0),
    End is min(Caret+10000, TBLen),
    ignore(line_comment_start(TB, Re, Caret, Start, M1)),
    ignore(line_comment_start(TB, Re, Caret, End,   M2)),
    (   nonvar(M1), nonvar(M2)
    ->  (   abs(M1-Caret) < abs(M2-Caret)
        ->  SCM = M1
        ;   SCM = M2
        )
    ;   nonvar(M1)
    ->  SCM = M1
    ;   nonvar(M2)
    ->  SCM = M2
    ),
    get(E, column, SCM, Col).

insert_line_comment(E, Arg:[int]) :->
    "Insert (line) comment; C-u: kill it"::
    (   Arg == 4
    ->  send(E, kill_comment)
    ;   insert_line_comment(E, Arg)
    ).

insert_line_comment(E, Arg) :-
    member(CSlen, [1, 2]),
    get(E?syntax, comment_start, CSlen, CS),
    !,
    get(E, caret, Caret),
    get(E, text_buffer, TB),
    get(TB, scan, Caret, line, 0, start, SOL),
    (   integer(Arg), Arg > 4
    ->  CC = Arg
    ;   Arg == @default,
        get(E, line_comment_column_in_context, CC),
        CC > 20, CC < 100
    ->  true
    ;   get(E, comment_column, CC)
    ),
    (   line_comment_in_code(E, SOL, CS, Start)
    ->  send(E, caret, Start),
        send(E, align, CC),
        send(E, forward_char, CSlen + 1)
    ;   send(E, end_of_line),
        send(E, just_one_space),
        send(E, align, CC),
        get(E?syntax, comment_end, CSlen, CE),
        (   send(CE, equal, '\n')
        ->  send(E, format, '%s ', CS)
        ;   send(E, format, '%s  %s', CS, CE),
            send(E, backward_char, CSlen + 1)
        )
    ).


comment_region(E) :->
    "Toggle-Comment the region using line-comments"::
    get(E, line_comment, Comment),
    get(E, region, tuple(Start, End)),
    get(E, text_buffer, TB),
    get(TB, scan, Start, line, 0, start, S0),
    comment_lines(TB, S0, End, Comment).

comment_line(E, Times:[int]) :->
    "Toggle-Comment the lines of an active region or Times lines"::
    (   \+ get(E, mark_status, inactive),
        get(E, region, _)
    ->  send(E, comment_region)
    ;   default(Times, 1, N),
        get(E, line_comment, Comment),
        get(E, caret, Caret),
        get(E, text_buffer, TB),
        get(TB, scan, Caret, line, 0, start, SOL),
        get(TB, scan, SOL, line, N, start, End),
        comment_lines(TB, SOL, max(End, SOL+1), Comment),
        get(TB, scan, SOL, line, N, start, Next),
        send(E, caret, Next)
    ).

comment_lines(_TB, S0, End, _Comment) :-
    S0 >= End,
    !.
comment_lines(TB, S0, End, Comment) :-
    atom_length(Comment, L),
    (   get(regex(''), quote, Comment, RE),
        send(regex(RE), match, TB, S0)
    ->  send(TB, delete, S0, L),
        S1 = S0,
        End1 is End - L
    ;   send(TB, insert, S0, Comment),
        S1 is S0 + L,
        End1 is End + L
    ),
    (   get(TB, scan, S1, line, 1, start, S2)
    ->  comment_lines(TB, S2, End1, Comment)
    ;   true
    ).


fill_comment_paragraph(M, Justify:justify=[bool|int], From:from=[int],
                       Lead0:lead=[char_array]) :->
    "Fill paragraph in (line) comment"::
    (   Lead0 == @default
    ->  (   get(M?syntax, comment_start, 1, CS)
        ->  Lead = CS
        ;   send(M, report, warning, 'No line-comment character defined'),
            fail
        )
    ;   Lead = Lead0
    ),
    new(Re, regex(string('^((%s)?[ \t]*$|[^%s])', Lead, Lead))),
    get(M, caret, Caret),
    get(M, text_buffer, TB),
    (   From \== @default
    ->  Start = From
    ;   get(Re, search, TB, Caret, 0, StartPar),
        get(TB, scan, StartPar, line, 1, start, Start)
    ->  true
    ;   Start = 0
    ),
    (   get(Re, search, TB, Start, End)
    ->  true
    ;   get(TB, size, End)
    ),
    free(Re),
    debug(fill(comment), '~p: filling ~d..~d', [M, Start, End]),
    (   new(LeadRe, regex(string('%s([^\n\t]*)\t[\t]*', Lead))),
        send(LeadRe, match, TB, Start),
        get(LeadRe, register_size, 1, Size),
        Size > 0
    ->  LeadCont = Lead
    ;   nonvar(CS)
    ->  new(LeadRe, regex(string('%s%s*[ \t]*', CS, CS))),
        LeadCont = @default
    ;   new(LeadRe, regex(string('%s[ \t]*', Lead))),
        LeadCont = @default
    ),
    send(M, fill_comment, Start, End, LeadRe, Justify, LeadCont),
    free(LeadRe).

%       ->fill_comment
%
%       Fill a region using a regex that defines leading comment

fill_comment(M,
             Start:from=int, End:to=int,
             Re:leading=regex, Justify:justify=[bool|int],
             LeadCont:lead_continuation=[char_array]) :->
    "Fill paragraph given `leading' regex"::
    (   (Justify == @default ; Justify == @off ; Justify == 0)
    ->  TheJustify = @off
    ;   TheJustify = @on
    ),
    get(M, text_buffer, TB),
    get(M, caret, Caret),
    new(CaretF, fragment(TB, Caret, 0)),
    new(EndF, fragment(TB, End, 0)),
    get(Re, match, TB, Start, LeadChars),
    get(TB, contents, Start, LeadChars, Lead),
    LeadEnd is Start + LeadChars,
    get(M, column, LeadEnd, LeadCol),
    uncomment(M, Re, LeadCol, Start, EndF),
    get(M, right_margin, RM0),
    RM is RM0 - LeadCol,
    get(EndF, start, NewEnd),
    debug(fill(comment), '->fill: ~d ~d', [Start, NewEnd]),
    send(M, fill, Start, NewEnd, 0, RM, TheJustify),
    (   LeadCont == @default
    ->  TheLeadCont = Lead
    ;   TheLeadCont = LeadCont
    ),
    comment(M, Start, EndF, Lead, TheLeadCont, LeadCol),
    send(M, caret, CaretF?start),
    free(EndF),
    free(CaretF).

uncomment(_M, _Re, _LeadCol, Here, EndF) :-
    get(EndF, start, End),
    Here >= End,
    !.
uncomment(M, Re, LeadCol, Here, EndF) :-
    get(M, text_buffer, TB),
    get(Re, match, TB, Here, Len),
    send(M, caret, Here),
    send(M, column, LeadCol),
    get(M, caret, AtLeadCol),
    DelLen is min(Len, AtLeadCol-Here),
    send(TB, delete, Here, DelLen),
    get(TB, scan, Here, line, 1, start, NextHere),
    uncomment(M, Re, LeadCol, NextHere, EndF).

comment(_, Here, EndF, _, _, _Col) :-
    get(EndF, start, End),
    Here >= End,
    !.
comment(M, Here, EndF, Lead, LeadCont, Col) :-
    get(M, text_buffer, TB),
    send(TB, insert, Here, Lead),
    get(Lead, size, Size),
    send(M, align, Col, Here+Size),
    get(TB, scan, Here, line, 1, start, NextHere),
    comment(M, NextHere, EndF, LeadCont, LeadCont, Col).


insert_long_comment(E) :->
    "Insert header/footer for long comment"::
    send(E, insert,
'/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
'),
    send(E, previous_line, 2).


insert_section_comment(E, Title:title=name) :->
    "Insert /*...*/ comment for a code section"::
    upcase_atom(Title, Upper),
    format(string(Line), '*~t~w~t*~32|~n', [Upper]),
    send(E, insert, '\n'),
    send(E, align, 16),
    send(E, insert, '/*******************************\n'),
    send(E, align, 16),
    send(E, insert, Line),
    send(E, align, 16),
    send(E, insert, '*******************************/\n\n').


                 /*******************************
                 *          FILE HEADER         *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->insert_file_header
        Inserts a .fileheader it finds in the current directory or one of
        its parent directories.  If no file it found it uses a default.

        Next it makes the substitutions from file_header_parameter/3 and
        finally, if the header contains %. it removes this and sets the
        caret at this position.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

insert_file_header(M) :->
    "Insert .fileheader or default file-header"::
    get(M, directory, Dir),
    find_file_header(Dir, Header),
    (   file_header_parameter(Escape, M, Value),
        substitute(Header, Escape, Value),
        fail
    ;   true
    ),
    (   new(Here, regex('%\\.')),
        send(Here, search, Header)
    ->  send(Here, register_value, Header, ''),
        get(Here, register_start, Offset),
        get(M, caret, Caret),
        send(M, insert, Header),
        send(M, caret, Caret+Offset)
    ;   send(M, insert, Header)
    ).


find_file_header(Dir, Header) :-
    get(Dir, file, '.fileheader', File),
    send(File, access, read),
    !,
    send(File, open, read),
    get(File, read, Header),
    send(File, close).
find_file_header(Dir, Header) :-
    get(Dir, parent, Parent),
    find_file_header(Parent, Header).
find_file_header(_, Header) :-
    new(Header, string),
    send_list(Header, append,
              [ '/*  File:    %F\n',
                '    Author:  %U\n',
                '    Created: %D\n',
                '    Purpose: %.\n',
                '*/\n\n'
              ]).

file_header_parameter('%Y', _,  Year) :-
    get(new(date), year, Year).
file_header_parameter('%F', M, FileName) :-
    get(M?file, base_name, FileName).
file_header_parameter('%U', _, UserName) :-
    get(@pce, user, User),
    (   get(@pce, user_info, gecos, User, UserName)
    ->  true
    ;   UserName = User
    ).
file_header_parameter('%E', _, Email) :-
    (   getenv('EMAIL', Email)
    ->  true
    ;   get(@pce, user, User),
        gethostname(Host),
        atomic_list_concat([User, Host], @, Email)
    ).
file_header_parameter('%D', _, Date) :-
    new(D, date),
    get(D, year, Year),
    get(D, month_name, @on, Month),
    get(D, day, Day),
    new(Date, string('%s %2d %d', Month, Day, Year)).

substitute(String, From, To) :-
    send(regex(From), for_all, String,
         message(@arg1, replace, @arg2, To)).


                 /*******************************
                 *             FILLING          *
                 *******************************/

:- pce_group(fill).


fill_paragraph(M, Justify:[int]) :->
    "Fill comment paragraph"::
    get(M, caret, Caret),
    (   in_fillable_comment(M, Caret, Lead)
    ->  (   get(M?syntax, comment_start, 1, Lead)
        ->  send(M, fill_comment_paragraph, Justify)
        ;   send(M, fill_comment_paragraph, Justify, lead := Lead)
        )
    ;   send_super(M, fill_paragraph, Justify)
    ).

%!  in_fillable_comment(+Mode, +Caret, -Lead) is semidet.
%
%   True when Caret is inside a comment starting at the beginning of a
%   line.

in_fillable_comment(M, Caret, Lead) :-
    get(M, scan_syntax, 0, Caret, tuple(comment, Start)),
    (   get(M?syntax, comment_start, 1, CS),
        get(M, column, Start, 0),
        send(M, looking_at, CS, Start)
    ->  Lead = CS
    ;   send(M, looking_at, '/\\*', Start),
        get(M, scan, Caret, line, 0, start, SOL),
        send(M, looking_at, '[ \t]*\\*[ \t]', SOL)
    ->  Lead = '[ \t]*\\*'
    ),
    !.
in_fillable_comment(M, Caret, CS) :-
    get(M, column, Caret, 0),
    get(M?syntax, comment_start, 1, CS),
    send(M, looking_at, CS, Caret).


justify_paragraph(M) :->
    "->fill_paragraph using right-margin"::
    send(M, fill_paragraph, 1).


auto_fill(M, From:[int], Regex:[regex]) :->
    "Auto fill in comment mode"::
    (   From == @default
    ->  get(M, caret, Caret)
    ;   Caret = From
    ),
    (   get(M, scan_syntax, 0, Caret, tuple(comment, Start)),
        get(M, scan, Caret, line, 0, start, SOL)
    ->  (   in_fillable_comment(M, Caret, Lead)
        ->  (   get(M?syntax, comment_start, 1, Lead)
            ->  send(M, fill_comment_paragraph, @off, SOL)
            ;   send(M, fill_comment_paragraph, @off, SOL, Lead)
            )
        ;   get(M, column, Start, 0),
            get(M, editor, Editor),
            send_class(Editor, editor, auto_fill(Caret, Regex))
        )
    ).


                 /*******************************
                 *          INDENTATION         *
                 *******************************/

:- pce_group(indent).

newline_and_indent(E, Arg:[int]) :->
    "Insert newline and indent as TAB"::
    send(E, newline, Arg),
    send(E, indent_line).


indent_line(E) :->
    "Indent current line"::
    send(E, beginning_of_text_on_line),
    (   send(E, indent_close_bracket_line)
    ;   send(E, indent_expression_line)
    ;   send(E, indent_comment_line)
    ;   send(E, align_with_previous_line)
    ).


indent_close_bracket_line(E, Brackets:[name], Base:[int]) :->
    "Indent a line holding a bracket"::
    default(Brackets, ')}]', B1),
    get(E, text_buffer, TB),
    get(E, caret, Caret),
    get(TB, character, Caret, Char),
    get(B1, index, Char, _),
    get(TB, matching_bracket, Caret, OpenPos),
    (   Base \== @default
    ->  OpenPos >= Base
    ;   true
    ),
    (   get(E, argument_indent, OpenPos, OpenPos, Col)
    ->  true
    ;   get(E, column, OpenPos, Col)
    ),
    send(E, align_line, Col).


%       ->indent_expression_line
%
%       This deal with the following layouts:
%
%         ==
%               [ aap,
%                 noot
%               | mies
%               ]
%
%         functor(arg1,
%                 arg2)
%
%         functor(
%            arg1,
%            arg2)
%         ==

indent_expression_line(E, Brackets:[name], Base:[int]) :->
    "Indent current line according to expression"::
    default(Brackets, ')}]', B1),
    atom_codes(B1, B2),
    get(E, text_buffer, TB),
    member(Bracket, B2),
        pce_catch_error(mismatched_bracket,
                        get(TB, matching_bracket, E?caret,
                            Bracket, OpenPos)),
        !,
        (   Base \== @default
        ->  OpenPos >= Base
        ;   true
        ),
        (   send(E, looking_at, '[,|]')     % line starts with , or |
        ->  get(E, column, OpenPos, Col)
        ;   get(E, argument_indent, OpenPos, Base, StartCol)
        ->  get(E, parameter_indentation, PI),
            Col is StartCol+PI
        ;   get(TB, scan, OpenPos, line, 0, end, EOL),
            get(TB, skip_comment, OpenPos+1, EOL, P1),
            get(E, column, P1, Col)
        ),
        send(E, align_line, Col),
    !.


argument_indent(E, OpenPos:int, After:[int], StartCol:int) :<-
    "Get column for indented arguments from OpenPos"::
    get(E, text_buffer, TB),
    get(TB, scan, OpenPos, line, 0, end, EOL),
    get(TB, skip_comment, OpenPos+1, EOL, P1),
    P1 == EOL,
    get(E, scan, OpenPos, term, -1, start, SOT),
    (   After \== @default
    ->  SOT > After
    ;   true
    ),
    get(E, column, SOT, StartCol).


%       ->indent_comment_line
%

indent_comment_line(M) :->
    "Copy leading comment of previous line"::
    get(M, text_buffer, TB),
    get(M, caret, Caret),
    get(M, scan, Caret, line, -1, start, SOPL),
    debug(indent(comment), 'Prev line at ~d', [SOPL]),
    (   get(TB?syntax, comment_start, 1, CS),
        new(LeadRe, regex(string('%s[ \t]*', CS))),
        get(LeadRe, match, TB, SOPL, Len),  % Previous holds comment
        CLine = string('%s?[ \t]*$', CS)
    ->  true
    ;   get(TB?syntax, comment_start, 2, '/*'),
        new(LeadRe, regex(string('[ \t]*(/\\*|\\*)[ \t]*'))),
        get(LeadRe, match, TB, SOPL, Len),
        debug(indent(comment), ' *-match', []),
        get(M, scan_syntax, 0, Caret, tuple(comment, StartComment)),
        send(M, looking_at, '/\\*', StartComment),
        CLine = ' *\\*?[ \t]*$',
        BlockComment = true
    ),
    get(M, scan, Caret, line, 0, start, SOL),
    send(M, looking_at, CLine, SOL),
    get(TB, contents, SOPL, Len, Lead),
    (   BlockComment == true
    ->  (   send(Lead, sub, /)
        ->  send(Lead, translate, /, ' '),
            send(Lead, append, ' ')
        ;   true
        )
    ;   true
    ),
    get(M, scan, SOL, line, 0, end, EOL),
    send(TB, delete, SOL, EOL-SOL),
    send(M, insert, Lead).


close_block_comment(M, Times:[int], Id:[event_id]) :->
    "Close a /** ... */ multiline comment"::
    (   Times == @default,
        get(M?syntax, comment_start, 2, '/*'),
        get(M, caret, Caret),
        get(M, scan, Caret, line, 0, start, SOL),
        send(M, looking_at, '[ \t]*\\*[ \t]*$', SOL)
    ->  send(M, just_one_space),
        send(M, backward_delete_char),
        send(M, insert, /)
    ;   send(M, insert_self, Times, Id)
    ).


                /********************************
                *           ALIGNMENT           *
                ********************************/

alignment_of_previous_line(E, Leading:[regex], Indent:int) :<-
    "Find the indentation of the previous line"::
    get(E, caret, Caret),
    get(E, scan, Caret, line, -1, start, LineStart),
    get(E, scan, Caret, term, -1, start, TermStart),
    (   TermStart < LineStart
    ->  get(E, indentation, TermStart, Leading, Indent)
    ;   get(E, indentation, LineStart, Leading, Indent)
    ).


align_with_previous_line(E, Leading:[regex]) :->
    "Align current_line with the one above"::
    get(E, alignment_of_previous_line, Leading, Indent),
    send(E, align_line, Indent).


align_close_bracket(E, Times:[int], Id:[event_id]) :->
    "Insert and align with matching open bracket"::
    send(E, insert_self, Times, Id),
    get(E, caret, Caret),
    get(E, scan, Caret, line, 0, start, SOL),
    get(E, scan, Caret, line, 0, end,   EOL),
    (   get(E, skip_comment, SOL, EOL, P0),
        Caret =:= P0+1
    ->  send(E, indent_line),
        send(E, forward_char)
    ;   true
    ).


                 /*******************************
                 *             ALIGN            *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Alignment, after GNU Emacs' align.el and newcomment.el:

  - align_region is `M-x align` and align_current `M-x align-current`.
    The lines between blank lines form sections that are aligned
    independently using ->align_section.  That aligns the columns that
    the mode defines using <-align_context and <-align_group (none by
    default) and the comments after the code.  This command cannot be
    called `align`, as that would hide editor->align.  tabularise is an
    alias.
  - align_regexp is `M-x align-regexp`.  Without a prefix argument it
    asks for a regex and aligns its first match on each line, changing
    the white space before it.  With a prefix argument the regex holds
    the group to change itself, and the command asks for the group, the
    minimal spacing and whether to align all matches on the line.
  - align_comment aligns the line comments after the code of the region
    on one column: comment_column, or right of the longest code.
  - comment_set_column (C-x ;) and kill_comment, the latter also as
    C-u M-;

Columns are rounded up to a tab stop if the buffer indents using tabs,
as Emacs' `align-to-tab-stop` does by default.  Matches and comment
starts inside a string or comment are ignored.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_group(align).

align_region(M, From:[int], To:[int]) :->
    "Align the region (Emacs M-x align)"::
    get(M, line_region, From, To, tuple(S, E)),
    align_sections(M, S, E).

align_current(M) :->
    "Align the section (lines between blank lines) holding the caret"::
    get(M, caret, Caret),
    section(M, Caret, S, E),
    send(M, align_section, S, E).

tabularise(M, From:[int], To:[int]) :->
    "Align the region (alias for ->align_region)"::
    send(M, align_region, From, To).

%   Align the sections of S..E, the last first, so aligning one does
%   not move the others.

align_sections(M, S, E) :-
    findall(SS-SE, region_section(M, S, E, SS, SE), Sections),
    reverse(Sections, Rev),
    forall(member(SS-SE, Rev), send(M, align_section, SS, SE)).

region_section(M, S, E, SS, SE) :-
    S < E,
    get(M, text_buffer, TB),
    (   section_separator(TB, S)
    ->  get(TB, scan, S, line, 1, start, S1),
        S1 > S,
        region_section(M, S1, E, SS, SE)
    ;   section_end(TB, S, E, SE0),
        (   SS = S, SE = SE0
        ;   region_section(M, SE0, E, SS, SE)
        )
    ).

section_end(TB, S, E, SE) :-
    get(TB, scan, S, line, 1, start, Next),
    (   Next >= E
    ->  SE = E
    ;   Next =< S
    ->  SE = E
    ;   section_separator(TB, Next)
    ->  SE = Next
    ;   section_end(TB, Next, E, SE)
    ).

%!  section_separator(+TB, +SOL) is semidet.
%
%   The line at SOL separates sections, as Emacs' align-region-separate:
%   it is blank, but for an optional brace.

section_separator(TB, SOL) :-
    get(TB, scan, SOL, line, 0, end, EOL),
    get(TB, contents, SOL, EOL-SOL, Line),
    get(Line, value, Text),
    split_string(Text, "", " \t", [Rest]),
    memberchk(Rest, ["", "{", "}"]).

%!  section(+M, +Pos, -S, -E) is semidet.
%
%   S..E are the lines between section separators around Pos.

section(M, Pos, S, E) :-
    get(M, text_buffer, TB),
    get(TB, scan, Pos, line, 0, start, SOL),
    \+ section_separator(TB, SOL),
    section_start(TB, SOL, S),
    get(TB, size, Size),
    section_end(TB, SOL, Size, E).

section_start(TB, SOL, S) :-
    (   SOL > 0,
        get(TB, scan, SOL, line, -1, start, Prev),
        Prev < SOL,
        \+ section_separator(TB, Prev)
    ->  section_start(TB, Prev, S)
    ;   S = SOL
    ).

align_section(M, S:int, E:int) :->
    "Align the lines S..E: columns (see <-align_context) and comments"::
    (   get(M, align_context, S, E, Ctx)
    ->  get(M, text_buffer, TB),
        get(TB, count_lines, S, E, Lines),
        align_columns(M, 1, S, Lines, mode_group(M, Ctx), 1, @on)
    ;   true
    ),
    (   get(M, line_comment, _)
    ->  send(M, align_comment, S, E)
    ;   true
    ).

align_context(_M, _S:int, _E:int, _Ctx:any) :<-
    "Context for <-align_group to align the columns of S..E"::
    fail.

align_group(_M, _Ctx:any, _SOL:int, _EOL:int, _N:int, _Group:tuple) :<-
    "White space GS..GE before column N of the line SOL..EOL"::
    fail.

align_regexp(M, Arg:[int], Regexp:regexp=name) :->
    "Align the first match of a regex on each line of the region"::
    (   Arg == @default
    ->  format(string(ReS), '(\\s*)~w', [Regexp]),
        Group = 1,
        Spacing = 1,
        Repeat = @off
    ;   ReS = Regexp,
        get(M, window, W),
        get(W, prompt, 'Parenthesis group to modify', 1, int, Group),
        get(W, prompt, 'Amount of spacing', 1, int, Spacing),
        get(W, prompt, 'Repeat for entire line', @off, bool, Repeat)
    ),
    get(M, region, tuple(From, To)),
    send(M, align_regexp_region, From, To, regex(ReS),
         Group, Spacing, Repeat).

align_regexp_region(M, From:int, To:int, Re:regex,
                    Group:[int], Spacing:[int], Repeat:[bool]) :->
    "Align group Group of matches of Re in the lines From..To"::
    default(Group, 1, G),
    default(Spacing, 1, Sp),
    default(Repeat, @off, Rep),
    (   G < 1
    ->  send(M, report, error, 'Justification (negative group) \c
                                is not supported'),
        fail
    ;   true
    ),
    get(M, line_region, From, To, tuple(S, E)),
    get(M, text_buffer, TB),
    get(TB, count_lines, S, E, Lines),
    align_columns(M, 1, S, Lines, regex_group(Re, G), Sp, Rep).

%!  align_columns(+M, +N, +S, +Lines, :Finder, +Spacing, +Repeat)
%
%   Align the Nth column of the Lines lines starting at S.  The column
%   starts where the white space before it, GS..GE, ends:
%
%       call(Finder, TB, SOL, EOL, N, GS, GE)
%
%   Columns are found again for each N, as aligning the previous ones
%   moved them.

align_columns(M, N, S, Lines, Finder, Sp, Rep) :-
    get(M, text_buffer, TB),
    findall(GS-GE, line_group(TB, S, Lines, Finder, N, GS, GE), Groups),
    Groups \== [],
    !,
    foldl(max_group_column(M), Groups, 0, MaxCol),
    tab_stop_column(M, MaxCol+Sp, Col),
    reverse(Groups, Rev),                       % later lines first
    forall(member(GS-GE, Rev), set_white_space(M, GS, GE, Col)),
    (   Rep == @on
    ->  N1 is N+1,
        align_columns(M, N1, S, Lines, Finder, Sp, Rep)
    ;   true
    ).
align_columns(_, _, _, _, _, _, _).

line_group(TB, S, Lines, Finder, N, GS, GE) :-
    Last is Lines-1,
    between(0, Last, I),
    get(TB, scan, S, line, I, start, SOL),
    get(TB, scan, SOL, line, 0, end, EOL),
    call(Finder, TB, SOL, EOL, N, GS, GE).

regex_group(Re, G, TB, SOL, EOL, N, GS, GE) :-
    nth_code_match(TB, Re, SOL, EOL, N, G, GS, GE).

mode_group(M, Ctx, _TB, SOL, EOL, N, GS, GE) :-
    get(M, align_group, Ctx, SOL, EOL, N, tuple(GS, GE)).

max_group_column(M, GS-_, Max0, Max) :-
    get(M, column, GS, Col),
    Max is max(Max0, Col).

set_white_space(M, GS, GE, Col) :-
    get(M, text_buffer, TB),
    send(TB, delete, GS, GE-GS),
    get(M, column, GS, Here),
    (   Col > Here
    ->  send(M, align, Col, GS)
    ;   true
    ).

%!  nth_code_match(+TB, +Re, +From, +EOL, +N, +G, -GS, -GE) is semidet.
%
%   Group G of the Nth match of Re in code of From..EOL spans GS..GE.

nth_code_match(TB, Re, From, EOL, N, G, GS, GE) :-
    From =< EOL,
    send(Re, search, TB, From, EOL),
    get(Re, register_start, 0, MS),
    get(Re, register_end, 0, ME),
    Next is max(ME, MS+1),
    (   in_code(TB, MS)
    ->  (   N == 1
        ->  get(Re, register_start, G, GS),
            get(Re, register_end, G, GE)
        ;   N1 is N-1,
            nth_code_match(TB, Re, Next, EOL, N1, G, GS, GE)
        )
    ;   nth_code_match(TB, Re, Next, EOL, N, G, GS, GE)
    ).

in_code(TB, Pos) :-
    get(TB, scan_syntax, 0, Pos, tuple(code, _)).

tab_stop_column(M, Col0, Col) :-
    (   get(M?text_buffer, indent_tabs, @on)
    ->  get(M, tab_distance, TD),
        Col is ((Col0+TD-1)//TD)*TD
    ;   Col is Col0
    ).

align_comment(M, From:[int], To:[int]) :->
    "Align the comments after the code in region"::
    get(M, line_comment, CS),
    get(M, line_region, From, To, tuple(S, E)),
    get(M, text_buffer, TB),
    get(TB, count_lines, S, E, Lines),
    findall(Start-CodeCol,
            line_code_comment(M, S, Lines, CS, Start, CodeCol),
            Comments),
    (   Comments == []
    ->  true
    ;   get(M, comment_column, CC),
        foldl(max_code_column, Comments, CC, MaxCol),
        tab_stop_column(M, MaxCol, Col),
        reverse(Comments, Rev),
        forall(member(Start-_, Rev), send(M, align, Col, Start))
    ).

max_code_column(_-CodeCol, Max0, Max) :-
    Max is max(Max0, CodeCol+1).

%!  line_code_comment(+M, +S, +Lines, +CS, -Start, -CodeCol) is nondet.
%
%   A line has a line comment at Start after code that ends at CodeCol.

line_code_comment(M, S, Lines, CS, Start, CodeCol) :-
    get(M, text_buffer, TB),
    Last is Lines-1,
    between(0, Last, I),
    get(TB, scan, S, line, I, start, SOL),
    line_comment_in_code(M, SOL, CS, Start),
    get(TB, skip_layout, Start, backward, @off, EOC),
    EOC > SOL,
    get(M, column, EOC, CodeCol).

%!  line_comment_in_code(+M, +Pos, +CS, -Start) is semidet.
%
%   Start is the start of the line comment on the line of Pos.

line_comment_in_code(M, Pos, CS, Start) :-
    get(M, text_buffer, TB),
    get(TB, scan, Pos, line, 0, start, SOL),
    get(TB, scan, Pos, line, 0, end, EOL),
    get(regex(''), quote, CS, QCS),
    new(Re, regex(QCS)),
    code_match_start(TB, Re, SOL, EOL, Start).

code_match_start(TB, Re, From, EOL, Start) :-
    From < EOL,
    send(Re, search, TB, From, EOL),
    get(Re, register_start, 0, MS),
    (   in_code(TB, MS)
    ->  Start = MS
    ;   Next is MS+1,
        code_match_start(TB, Re, Next, EOL, Start)
    ).

comment_set_column(M, Arg:[int]) :->
    "Set comment_column; C-u: from previous comment; C--: kill comment"::
    (   Arg == @default
    ->  get(M, column, Col),
        send(M, comment_column, Col),
        send(M, report, status, 'comment_column set to %d', Col)
    ;   Arg < 0
    ->  send(M, kill_comment)
    ;   get(M, line_comment, CS),
        get(M, caret, Caret),
        get(M, text_buffer, TB),
        get(TB, scan, Caret, line, 0, start, SOL),
        previous_comment(M, SOL, CS, Start)
    ->  get(M, column, Start, Col),
        send(M, comment_column, Col),
        send(M, report, status, 'comment_column set to %d', Col),
        send(M, insert_line_comment, Col)
    ;   send(M, report, warning, 'No previous comment')
    ).

previous_comment(M, Here, CS, Start) :-
    Here > 0,
    get(M, text_buffer, TB),
    get(TB, scan, Here, line, -1, start, SOL),
    SOL < Here,
    (   line_comment_in_code(M, SOL, CS, Start)
    ->  true
    ;   previous_comment(M, SOL, CS, Start)
    ).

kill_comment(M, Times:[int]) :->
    "Kill the line comment on this and Times-1 next lines"::
    default(Times, 1, N),
    get(M, line_comment, CS),
    get(M, caret, Caret),
    get(M, text_buffer, TB),
    get(TB, scan, Caret, line, 0, start, SOL),
    get(TB, scan, SOL, line, N, start, End),
    forall(previous_line_start(TB, SOL, End, LS),
           kill_line_comment(M, LS, CS)).

%   Line starts from End back to Start, so killing does not move them.

previous_line_start(TB, Start, End, LS) :-
    End > Start,
    get(TB, scan, End-1, line, 0, start, LS0),
    (   LS = LS0
    ;   previous_line_start(TB, Start, LS0, LS)
    ).

kill_line_comment(M, SOL, CS) :-
    (   line_comment_in_code(M, SOL, CS, Start)
    ->  get(M, text_buffer, TB),
        get(TB, scan, Start, line, 0, end, EOL),
        get(TB, skip_layout, Start, backward, @off, From),
        send(M, kill, From, EOL)
    ;   true
    ).


                 /*******************************
                 *           UNTABIFY           *
                 *******************************/

backward_delete_char_untabify(M, Times:[int]) :->
    "Delete characters backward"::
    get(M, caret, Caret),
    get(M, character, Caret-1, Char),
    (   send(M?syntax, has_syntax, Char, white_space)
    ->  get(M, column, Col),
        default(Times, 1, Tms),
        send(M, align, Col-Tms),
        (   get(M, column, Col)
        ->  send(M, backward_delete_char, Times)
        ;   true
        )
    ;   send(M, backward_delete_char, Times)
    ).


                 /*******************************
                 *            TAGS              *
                 *******************************/

:- initialization
   pce_define_type(emacs_tag, name).

visit_tag_table(M, Table:tag_file='file|directory') :->
    "Load specified GNU-Emacs (etags) tag-table"::
    (   send(Table, instance_of, directory)
    ->  find_tag_from_dir(Table, TagFile)
    ;   TagFile = Table
    ),
    get(TagFile, absolute_path, TagFileName),
    (   send(TagFile, access, read)
    ->  auto_call(emacs_init_tags(TagFileName)),
        send(M, report, status, 'Loaded TAG table %s', TagFileName)
    ;   send(M, report, warning, '%s: not accessible', TagFileName),
        fail
    ).

find_tag_from_dir(Dir, File) :-
    get(Dir, file, 'TAGS', File),
    send(File, exists),
    !.
find_tag_from_dir(Dir, File) :-
    get(Dir, parent, Parent),
    find_tag_from_dir(Parent, File).

ensure_loaded_tags(M) :->
    "Make sure we have a tag-table loaded"::
    (   auto_call(emacs_tag_file(_))
    ->  emacs_update_tags
    ;   get(M, directory, Dir),
        (   send(?(Dir, file, 'TAGS'), exists)
        ->  get(Dir, path, Path),
            auto_call(emacs_init_tags(Path))
        ;   send(M, noarg_call, visit_tag_table)
        )
    ).

default_tag(M, DefTag:name) :<-
    "Return default tag from current word"::
    (   get(M, word, Word),
        send(regex('[a-zA-Z0-9_]*$'), match, Word)
    ->  DefTag = Word
    ;   DefTag = ''
    ).


expand_tag(M, Tag:[name], TheTag:name) :<-
    "Expand tag using tag-table"::
    send(M, ensure_loaded_tags),
    (   Tag == @default
    ->  get(M, word, DefTag),
        new(I, emacs_tag_item('Find tag', DefTag)),
        get(M, prompt_using, I, TagString),
        get(TagString, value, TheTag)
    ;   TheTag = Tag
    ).


find_tag(M, Tag:emacs_tag, Where:[{as_arranged,here,tab,split,window}], Editor:editor) :<-
    "Jump to indicated tag entry"::
    (   get(M, directory, Dir),
        find_tag_from_dir(Dir, TagFile),
        get(TagFile, directory_name, TagDirName),
        (   SearchDir = TagDirName
        ;   true
        ),
        debug(emacs(tag), 'Search ~q from ~q', [Tag, SearchDir]),
        emacs_update_tags,
        auto_call(emacs_tag(Tag, SearchDir, File, Line))
    ->  new(B, emacs_buffer(File)),
        get(B, open, Where, View),
        get(View, editor, Editor),
        send(M, location_history),
        send(Editor, line_number, Line),
        adjust_tag(Editor, Tag),
        send(M, location_history, title := Tag)
    ;   send(M, report, warning, 'Cannot find tag %s', Tag),
        fail
    ).


find_tag(M, Tag:emacs_tag) :->
    "Jump to entry from TAG table"::
    ignore(get(M, find_tag, Tag, tab, _)). % avoid delegation to menu-bar

find_references(M, _For:any) :->
    "Find references (default: not supported in this mode)"::
    send(M, report, warning, "No reference finder available for this mode").


adjust_tag(E, Tag) :-
    get(E, text_buffer, TB),
    get(E, caret, Here),
    new(Re, regex('')),
    get(Re, quote, Tag, QTag),
    (   send(Re, pattern, string('\\\\y%s\\\\y', QTag))
    ;   send(Re, pattern, string('\\\\y%s', QTag))
    ),
    closest(Re, TB, Here, Pos),
    !,
    send(E, caret, Pos).


closest(Re, TB, Here, Pos) :-
    get(Re, search, TB, Here, P1),
    !,
    (   get(Re, search, TB, Here, 0, P2)
    ->  closest_element(P1, P2, Here, Pos)
    ;   Pos = P1
    ).
closest(Re, TB, Here, Pos) :-
    get(Re, search, TB, Here, 0, Pos).

closest_element(A, B, Here, A) :-
    abs(A-Here) =< abs(B-Here),
    !.
closest_element(A, B, Here, B) :-
    abs(B-Here) < abs(A-Here).

xref_go_back(_E) :->
    "Go back in the location history"::
    send(@emacs?history, backward).


                 /*******************************
                 *         MISCELLANEOUS        *
                 *******************************/

beginning_of_text_on_line(E) :->
    "Position caret at first non-white on line"::
    get(E, caret, Caret),
    get(E, scan, Caret, line, 0, start, SOL),
    (   get(E?syntax, comment_start, 1, CS),
        new(LeadRe, regex(string('%s[ \t]*', CS))),
        get(E, text_buffer, TB),
        get(LeadRe, match, TB, SOL, Len)
    ->  AtText is SOL+Len,
        send(E, caret, AtText)
    ;   get(E, scan, Caret, line, 0, end,   EOL),
        get(E, skip_comment, SOL, EOL, P0),
        send(E, caret, P0)
    ).


new_caret_position(M, Caret:int) :->
    "Update line number"::
    send_super(M, new_caret_position, Caret),
    send(M, show_caret_line, Caret),
    send(M, highlight_matching_bracket, Caret).

%       Also on its own, from `emacs_view ->pane_exposed': the bar of a
%       window says what the pane in view has to say, and a pane coming
%       into view finds it cleared.

show_caret_line(M, Caret:[int]) :->
    "Say on the bar of my window which line the caret is on"::
    (   Caret == @default
    ->  get(M, caret, At)
    ;   At = Caret
    ),
    (   get(M, frame, Frame),
        send(Frame, has_send_method, show_line_number)
    ->  get(M, show_line_numbers, Upto),
        (   Upto == 0                   % never
        ->  send(Frame, show_line_number, @nil)
        ;   At =< Upto                  % counting further is too expensive
        ->  get(M, line_number, Line),
            send(Frame, show_line_number, Line)
        ;   send(Frame, show_line_number, too_expensive)
        )
    ;   true
    ).


show_line_numbers(M, Upto:'0..') :->
    "Show line numbers up to this character position (0: never)"::
    send(M, slot, show_line_numbers, Upto),
    send(M, new_caret_position, M?caret).


what_syntax(E) :->
    "Find syntax at caret"::
    get(E, caret, C),
    get(E, scan_syntax, 0, C, tuple(Syntax, Start)),
    send(E, report, inform,
         'Syntax: "%s"; started at %d', Syntax, Start).


                 /*******************************
                 *       MATCHING BRACKETS      *
                 *******************************/

highlight_matching_bracket(M, CaretSpec:[int]) :->
    (   CaretSpec == @default
    ->  get(M, caret, Caret)
    ;   Caret = CaretSpec
    ),
    (   send(M, highlight_up_to_date, Caret)
    ->  true
    ;   get(M, text_buffer, TB),
        get(TB, syntax, SyntaxTable),
        (   (   Here = Caret,
                get(TB, character, Here, Char),
                send(SyntaxTable, has_syntax, Char, open_bracket)
            ;   Here is Caret - 1,
                get(TB, character, Here, Char),
                send(SyntaxTable, has_syntax, Char, close_bracket)
            )
        ->  get(TB, matching_bracket, Here, Match),
            send(M, show_matching_bracket_fragment, Match)
        ;   send(M, unshow_matching_bracket_fragment)
        )
    ;   true
    ).

highlight_up_to_date(M, Caret:int) :->
    "True if highlight indication is up-to-date"::
    get(M, text_buffer, TB),
    get(TB, generation, Gen),
    (   get(M, bracket_caret, Caret),
        get(M, bracket_gen, Gen)
    ->  true
    ;   send(M, slot, bracket_caret, Caret),
        send(M, slot, bracket_gen, Gen),
        fail
    ).

show_matching_bracket_fragment(M, At:int) :->
    "Show matching bracket at position"::
    get(M, text_buffer, TB),
    (   get(TB, hypered, matching_bracket_fragment, F)
    ->  send(F, start, At),
        send(F, length, 1)
    ;   new(_, partof_hyper(TB,
                            fragment(TB, At, 1, matching_bracket),
                            matching_bracket_fragment,
                            text_buffer))
    ).

unshow_matching_bracket_fragment(M) :->
    "Delete the matching bracket fragment"::
    get(M, text_buffer, TB),
    (   get(TB, hypered, matching_bracket_fragment, F)
    ->  free(F)
    ;   true
    ).


                 /*******************************
                 *      RESTYLE IDENTIFIERS     *
                 *******************************/

camelcase_word(M, Arg:[int]) :->
    "Change word to CamelCase"::
    send(M, restyle_word, 'OneTwo', Arg).

underscores_word(M, Arg:[int]) :->
    "Change word to undercore_mode"::
    send(M, restyle_word, one_two, Arg).

restyle_word(M, Style:{'OneTwo',oneTwo,one_two,'One_Two'}, Arg:[int]) :->
    "Restyle the current identifier word"::
    default(Arg, 1, Times),
    forall(between(1, Times, _),
           ( (   send(M, looking_at, '\\w')
             ->  get(M, caret, Here)
             ;   get(M, caret, Caret),
                 get(M, scan, Caret, word, 1, start, Here)
             ),
             get(M, scan, Here, word, 0, end, End),
             get(M, contents, Here, End-Here, string(Word)),
             restyle_identifier(Style, Word, NewWord),
             send(M, delete, Here, End),
             send(M, caret, Here),
             send(M, insert, NewWord))).


                 /*******************************
                 *            HELP              *
                 *******************************/

prolog_manual(M, On:[name]) :->
    "Open Prolog manual"::
    (   (   On == @default
        ->  prolog_manual(help)
        ;   prolog_manual(On)
        )
    ->  true
    ;   send(M, report, warning, "No console for running help/1")
    ).

%!  prolog_manual(+Topic)
%
%   Show     help     on     Topic.       When     running     PceEmacs,
%   library(emacs/help_buffer)      provides      a        hook      for
%   prolog_help:show_html_hook/1 that makes the help   text appear in an
%   Emacs buffer.

prolog_manual(Topic) :-
    help(Topic).


                 /*******************************
                 *         DIAGNOSTICS          *
                 *******************************/

%   Diagnostics are emacs_lsp_diagnostic fragments created from the
%   messages of an LSP server (see library(emacs/lsp_diagnostics)).

goto_next_error(M) :->
    "Go to the next diagnostic"::
    send(M, goto_diagnostic, next).

goto_prev_error(M) :->
    "Go to the previous diagnostic"::
    send(M, goto_diagnostic, prev).

goto_diagnostic(M, Dir:direction={next,prev}) :->
    "Select the next/previous diagnostic"::
    get(M, caret, Caret),
    (   get(@classes, member, emacs_lsp_diagnostic, _),
        Diag = message(@arg1, instance_of, emacs_lsp_diagnostic),
        (   Dir == next
        ->  get(M, find_fragment, and(Diag, @arg1?start > Caret), Fragment)
        ;   get(M, find_all_fragments, and(Diag, @arg1?end < Caret), All),
            get(All, tail, Fragment)
        )
    ->  get(Fragment, start, Start),
        get(Fragment, end, End),
        send(M, selection, End, Start, highlight)
    ;   send(M, report, status, "No further diagnostics")
    ).

:- emacs_end_mode.

