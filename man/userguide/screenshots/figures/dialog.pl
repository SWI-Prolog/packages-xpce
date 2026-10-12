/*  Figures for dialog.plx: "Dialog (controller) windows"
*/

:- module(fig_dialog, []).
:- use_module(library(pce)).
:- use_module('../screenshots', [settle/0, settle/1, last_frame/1]).
:- use_module(library(lists)).

:- multifile
    screenshot:figure/2,
    screenshot:figure_option/2.

%   \figref{employee}: ask_employee/0 of the section "An example",
%   with the fields filled in.

screenshot:figure(employee, Dialog) :-
    ask_employee,
    last_frame(Frame),
    get(Frame, member, dialog, Dialog),
    settle,
    send(Dialog?first_name_member, selection, 'Bob'),
    send(Dialog?family_name_member, selection, 'Worker'),
    send(Dialog?sex_member, selection, male),
    send(Dialog?age_member, selection, 42),
    send(Dialog?department_member, selection, research).

ask_employee :-
    new(Dialog, dialog('Define employee')),
    send_list(Dialog, append,
              [ new(N1, text_item(first_name)),
                new(N2, text_item(family_name)),
                new(S,  menu(sex)),
                new(A,  int_item(age, low := 18, high := 65)),
                new(D,  menu(department, cycle)),
                button(cancel, message(Dialog, destroy)),
                button(enter, and(message(@prolog,
                                          assert_employee,
                                          N1?selection,
                                          N2?selection,
                                          S?selection,
                                          A?selection,
                                          D?selection),
                                  message(Dialog, destroy)))
              ]),
    send_list(S, append, [male, female]),
    send_list(D, append, [research, development, marketing]),
    send(Dialog, default_button, enter),
    send(Dialog, open).

%   \figref{layoutdemo1}: layoutdemo1/0 of the section "Practical
%   usage and problems".

screenshot:figure(layoutdemo1, Frame) :-
    layoutdemo1,
    last_frame(Frame).

layoutdemo1 :-
    new(D, dialog('Layout Demo 1')),
    send(D, append,
         new(BTS, dialog_group(buttons, group))),
    send(BTS, gap, size(0, 30)),
    send(BTS, append, button(add)),
    send(BTS, append, button(rename), below),
    send(BTS, append, button(delete), below),
    send(BTS, layout_dialog),
    send(D, append, new(LB, list_browser), right),
    send(D, append, new(TI, text_item(name, ''))),
    send(LB, alignment, left),
    send(D, layout),
    send(LB, bottom_side, BTS?bottom_side),
    send(LB, right_side, TI?right_side),
    send(D, open).


%   \figref{wysiwyg}: the editor of the section "Example: a simple
%   editor for multiple fonts" (examples/wysiwyg.pl) showing the
%   README.md of xpce.  A style `title` was defined and styles have
%   been applied to some text.  The user is defining a new style.
%   ask_style/3 blocks in `frame <-confirm_centered`, so we create
%   its dialog here using the same code, but open it without waiting.

:- use_module('../../examples/wysiwyg').

screenshot:figure(wysiwyg, [Fr-point(0,0), D-point(DX,DY)]) :-
    xpce_dir(XpceDir),
    working_directory(Old, XpceDir),
    call_cleanup(wysiwyg('README.md'),
                 working_directory(_, Old)),
    last_frame(Fr),
    settle,
    wysiwyg:append_style(Fr, title,
                         style(font := font(sans, bold, 16))),
    forall(member(Text-Style,
                  [ '# Contents'-title,
                    '# Authors'-title,
                    '# Copyrights'-title,
                    '# Installation'-title,
                    'XPCE'-bold,
                    'XPCE/Prolog'-bold,
                    'Anjo Anjewierden'-italic,
                    'Jan\nWielemaker'-italic,
                    'SDL3'-bold,
                    'Cairo'-bold,
                    '"Simplified BSD License"'-italic
                  ]),
           apply_style(Fr, Text, Style)),
    get(Fr, member, view, V),
    send(V, caret, 0),
    send(Fr, size, size(640, 400)),
    settle,
    style_dialog(Fr, D),
    get(D, member, name, N),
    send(N, selection, underline),
    get(D, member, style, S),
    send(S, selection, style(underline := @on)),
    send(D, open_centered, Fr?area?center),
    settle,
    get(Fr?area, size, size(FW, FH)),
    get(D?area, size, size(DW, DH)),
    DX is (FW-DW)//2,
    DY is (FH-DH)//2.

%   The dialog of wysiwyg:ask_style/3.

style_dialog(Fr, D) :-
    new(D, dialog('Define Style')),
    send(D, append,
         new(_N, text_item(name, ''))),
    send(D, append,
         new(_S, style_item(style))),
    send(D, append,
         button(ok, message(D, return, ok))),
    send(D, append,
         button(cancel, message(D, return, cancel))),
    send(D, default_button, ok),
    send(D, transient_for, Fr).

%   Select the first occurrence of Text in the view and apply Style
%   using wysiwyg:set_style/2, which is called by the style menu.

apply_style(Fr, Text, Style) :-
    get(Fr, member, view, V),
    get(V, text_buffer, TB),
    get(TB, contents, String),
    get(String, value, Contents),
    sub_atom(Contents, Start, Len, _, Text),
    !,
    End is Start+Len,
    send(V, selection, Start, End, active),
    wysiwyg:set_style(Fr, Style),
    send(V, selection, 0, 0, inactive).

xpce_dir(Dir) :-
    module_property(fig_dialog, file(File)),
    file_directory_name(File, FigDir),
    atomic_list_concat(Parts, /, FigDir),
    append(XpceParts, [man, userguide, screenshots, figures], Parts),
    atomic_list_concat(XpceParts, /, Dir).

%   \figref{editgraphical}: attributedemo/0 of the section "Example:
%   editing attributes of a graphical" after changing the box and
%   double-clicking the ellipse.  The code of the guide is in
%   examples/editgraphics.pl.  It is loaded into `user` because the
%   gesture calls edit_graphical/1 through @prolog.

:- load_files(user:'../../examples/editgraphics', [if(not_loaded)]).

screenshot:figure(editgraphical,
                  [ Demo-point(0,0),
                    Edit-point(X,Y)
                  ]) :-
    user:attributedemo,
    last_frame(Demo),
    get(Demo, member, picture, P),
    get(P?graphicals, head, Box),
    get(P?graphicals, tail, Ellipse),
    send(Box, fill, colour(red)),
    send(Box, pen, 3),
    send(Ellipse, fill, colour(green)),
    settle,
    user:edit_graphical(Ellipse),
    last_frame(Edit),
    Edit \== Demo,
    get(Demo?area, size, size(W,H)),
    X is W-280,
    Y is H-80.
