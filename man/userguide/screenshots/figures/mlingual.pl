/*  Figures for mlingual.plx: "Multi-lingual applications"
*/

:- module(fig_mlingual, []).
:- use_module(library(pce)).

:- multifile
    screenshot:figure/2.

%   The figure `username` in the description of `dialog_item
%   <-label_name`, showing how text_item(user_name, '') renders.

screenshot:figure(username, D) :-
    new(D, dialog),
    send(D, append, text_item(user_name, '')).
