/*  Figures for tabular.plx: "Tabular layout"
*/

:- module(fig_tabular, []).
:- use_module(library(pce)).

:- multifile
    screenshot:figure/2.

%   \figref{tabular}: make_table/0 of the section "Using table using
%   the tabular library".  The code below is copied from the guide.

screenshot:figure(tabular, Frame) :-
    make_table,
    get(@display, frames, Frames),
    get(Frames, tail, Frame).

:- use_module(library(tabular)).
:- use_module(library(autowin)).

make_table :-
        new(P, auto_sized_picture('Table with merged cells')),
        send(P, display, new(T, tabular)),
        send(T, border, 1),
        send(T, cell_spacing, -1),
        send(T, rules, all),
        send_list(T,
                  [ append(new(graphical), rowspan := 2),
                    append('Length/mm', bold, center, colspan := 3),
                    next_row,
                    append('body', bold, center),
                    append('tail', bold, center),
                    append('ears', bold, center),
                    next_row,
                    append('males', bold),
                    append('31.4'),
                    append('23.7'),
                    append('3.8'),
                    next_row,
                    append('females', bold),
                    append('29.6'),
                    append('20.8'),
                    append('3.4')
                  ]),
        send(P, open).
