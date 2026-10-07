/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2001-2024, University of Amsterdam
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

:- module(prolog_trace_utils,
          [ trace_setting/2,            % ?Name, ?Value
            trace_setting/3,            % +Name, -Old, +New
            setting/2,                  % +Name, +Value

            canonical_source_file/2,    % +RawFile, -CanonicalFile

            find_source/3,              % +Head, -File|TextBuffer, -Line
            thread_self_id/1            % -Name|Int
          ]).
:- use_module(library(pce)).
:- use_module(library(debug), [debug/3]).
:- autoload(library(listing), [portray_clause/1]).
:- autoload(library(lists), [member/2]).
:- autoload(library(pce_class_variable_editor), [save_class_variable/3]).
:- autoload(library(portray_text), [portray_text/1, set_portray_text/3]).
:- autoload(library(prolog_clause), [predicate_name/2]).
:- autoload(library(readutil), [read_file_to_terms/3]).
:- use_module(library(pce_config), []).

:- meta_predicate
    find_source(:, -, -).


                 /*******************************
                 *           SETTINGS           *
                 *******************************/

/* The preferences of the debugger are class variables of the class
prolog_debug_settings.  They are edited in Settings/Debugger (see
library(trace/settings)) or the class variable editor and saved in the
user's Defaults file.  Older versions saved them in config('Tracer.cnf').
This file is read once into the Defaults file and then renamed.  The
other settings are state of the running session.
*/

:- pce_begin_class(prolog_debug_settings, object,
                   "Preferences of the graphical debugger").

class_variable(show_unbound,        bool, @off,
               "The bindings show unbound variables").
class_variable(cluster_variables,   bool, @on,
               "The bindings cluster variables with the same value").
class_variable(portray_text,        bool, @on,
               "Portray code lists as text").
class_variable(portray_text_length, '0..', 30,
               "Show an ellipsis for text that is longer").
class_variable(stack_depth,         '2..', 10,
               "Number of stack frames shown").
class_variable(choice_depth,        '0..', 10,
               "Number of choice points shown").
class_variable(list_max_clauses,    '2..', 25,
               "Most clauses decompiled when listing dynamic code").
class_variable(auto_raise,          bool, @on,
               "Raise the debugger when it is entered").
class_variable(auto_close,          bool, @on,
               "Close the debugger on nodebug and abort").
class_variable(use_pce_emacs,       bool, @on,
               "Use the built-in PceEmacs editor").
class_variable(other_threads,       {trace,nodebug,block}, block,
               "How to handle other threads that trap the debugger").

:- pce_end_class(prolog_debug_settings).

:- dynamic
    local_setting/2.

local_setting(active,          true).   % actually use this tracer
local_setting(term_depth,      2).      % nesting for printing terms
local_setting(console_actions, false).  % map actions from the console

%!  setting(?Name, ?Value) is nondet.
%
%   Value is the value of the debugger setting Name.  Booleans are
%   `true` or `false`.  The portray_text settings are those of
%   library(portray_text).

setting(Name, Value) :-
    atom(Name),
    !,
    (   preference(Name)
    ->  preference_value(Name, Value)
    ;   local_setting(Name, Value)
    ).
setting(Name, Value) :-
    (   preference(Name),
        preference_value(Name, Value)
    ;   local_setting(Name, Value)
    ).

%   preference(?Name) is nondet.
%
%   Name is a preference: a class variable of prolog_debug_settings.

preference(Name) :-
    atom(Name),
    !,
    get(class(prolog_debug_settings), class_variable, Name, _).
preference(Name) :-
    get(class(prolog_debug_settings), class_variables, CVs),
    chain_list(CVs, List),
    member(CV, List),
    get(CV, name, Name).

preference_value(portray_text, Enabled) :-
    !,
    set_portray_text(enabled, Enabled, Enabled).
preference_value(portray_text_length, Len) :-
    !,
    set_portray_text(ellipsis, Len, Len).
preference_value(Name, Value) :-
    get(class(prolog_debug_settings), class_variable, Name, CV),
    get(CV, value, PceValue),
    pce_value(PceValue, Value).

pce_value(@on,  true) :- !.
pce_value(@off, false) :- !.
pce_value(Value, Value).

trace_setting(Name, Value) :-
    setting(Name, Value).

%!  trace_setting(+Name, -Old, +New) is det.
%
%   Set the setting Name to New.  A preference is also saved in the
%   user's Defaults file.

trace_setting(Name, Old, New) :-
    setting(Name, Old),
    Old == New,
    !.
trace_setting(portray_codes, Old, New) :- % compatibility
    !,
    trace_setting(portray_text, Old, New).
trace_setting(Name, Old, New) :-
    preference(Name),
    !,
    setting(Name, Old),
    set_preference(Name, New),
    ignore(save_preference(Name, New)).
trace_setting(Name, Old, New) :-
    retract(local_setting(Name, Old)),
    !,
    assertz(local_setting(Name, New)),
    notify_gui.
trace_setting(Name, Old, _) :-
    setting(Name, Old).

set_preference(Name, Value) :-
    pce_value(PceValue, Value),
    send(class(prolog_debug_settings), class_variable_value, Name, PceValue),
    preference_changed(Name, Value).

save_preference(Name, Value) :-
    pce_value(PceValue, Value),
    save_class_variable(prolog_debug_settings, Name, PceValue).

%   preference_changed(+Name, +Value)
%
%   Act on a preference that changed in the running session.

preference_changed(portray_text, Enabled) :-
    !,
    portray_text(Enabled).
preference_changed(portray_text_length, Len) :-
    !,
    set_portray_text(ellipsis, _, Len).
preference_changed(_, _) :-
    notify_gui.

notify_gui :-
    (   current_predicate(prolog_gui:notify_gui/0)
    ->  prolog_gui:notify_gui
    ;   true
    ).

:- multifile
    pce_preferences:class_variable_changed/3.

pce_preferences:class_variable_changed(prolog_debug_settings, Name,
                                       PceValue) :-
    pce_value(PceValue, Value),
    preference_changed(Name, Value).

%   init_trace_settings
%
%   Hand the portray_text preferences to library(portray_text) if they
%   are not the defaults and migrate config('Tracer.cnf').

init_trace_settings :-
    forall(( member(Name, [portray_text, portray_text_length]),
             get(class(prolog_debug_settings), class_variable, Name, CV),
             get(CV, value, PceValue),
             get(CV, default, Default),
             \+ default_value(CV, Default, PceValue)
           ),
           ( pce_value(PceValue, Value),
             preference_changed(Name, Value)
           )),
    migrate_trace_settings.

default_value(CV, Default, Value) :-
    (   send(Default, instance_of, char_array)
    ->  get(CV, convert_string, Default, DefaultValue)
    ;   DefaultValue = Default
    ),
    DefaultValue == Value.

%   migrate_trace_settings
%
%   Apply the settings of config('Tracer.cnf'), written by older
%   versions.  If they can be saved in the user's Defaults file, the
%   file is renamed to Tracer.cnf.migrated, so this happens only once.

migrate_trace_settings :-
    absolute_file_name(config('Tracer.cnf'), File,
                       [ access(read),
                         file_errors(fail)
                       ]),
    read_file_to_terms(File, Terms, []),
    !,
    findall(Ok,
            ( member(setting(Name, Value), Terms),
              Name \== active,
              migrate_setting(Name, Value, Ok)
            ),
            Oks),
    (   \+ memberchk(false, Oks),
        get(@pce, user_defaults, UserDefaults),
        UserDefaults \== @nil
    ->  file_name_extension(File, migrated, Migrated),
        catch(rename_file(File, Migrated), _, true)
    ;   true
    ).
migrate_trace_settings.

migrate_setting(Name, Value, Ok) :-
    preference(Name),
    !,
    (   setting(Name, Value)
    ->  Ok = true
    ;   set_preference(Name, Value),
        (   save_preference(Name, Value)
        ->  Ok = true
        ;   Ok = false
        )
    ).
migrate_setting(Name, Value, true) :-
    trace_setting(Name, _, Value).

:- initialization init_trace_settings.


                 /*******************************
                 *      SOURCE LOCATIONS        *
                 *******************************/

%!  find_source(:HeadTerm, -File, -Line) is det.
%
%   Finds the source-location of the predicate.  If the predicate is
%   not    defined,    it    will    list     the    predicate    on
%   @dynamic_source_buffer and return this buffer.

find_source(Predicate, File, Line) :-
    predicate_property(Predicate, file(File)),
    predicate_property(Predicate, line_count(Line)),
    !.
find_source(Predicate, File, 1) :-
    debug(gtrace(source), 'No source for ~p', [Predicate]),
    File = @dynamic_source_buffer,
    send(File, clear),
    setup_call_cleanup(
        pce_open(File, write, Fd),
        with_output_to(Fd, list_predicate(Predicate)),
        close(Fd)).

list_predicate(Predicate) :-
    predicate_property(Predicate, foreign),
    !,
    predicate_name(user:Predicate, PrintName),
    send(@dynamic_source_buffer, attribute, comment,
         string('Can''t show foreign predicate %s', PrintName)).
list_predicate(Predicate) :-
    predicate_name(user:Predicate, PrintName),
    setting(list_max_clauses, Max),
    '$get_predicate_attribute'(Predicate, number_of_clauses, Num),
    (   Num > Max
    ->  Upto is Max - 1,
        list_clauses(Predicate, 1, Upto),
        Skipped is Num - Max,
        format('~n% <skipped ~d clauses>~n~n', [Skipped]),
        list_clauses(Predicate, Num, Num),
        send(@dynamic_source_buffer, attribute, comment,
             string('Partial decompiled listing of %s', PrintName))
    ;   list_clauses(Predicate, 1, Num),
        send(@dynamic_source_buffer, attribute, comment,
             string('Decompiled listing of %s', PrintName))
    ).


list_clauses(Predicate, From, To) :-
    between(From, To, Nth),
        nth_clause(Predicate, Nth, Ref),
        clause(RawHead, Body, Ref),
        strip_module(user:RawHead, Module, Head),
        tag_module(Module),
        portray_clause((Head :- Body)),
    fail.
list_clauses(_, _, _).

tag_module(Module) :-
    prolog_clause:hidden_module(Module),
    !. % dubious
tag_module(Module) :-
    format('~q:', Module).

                 /*******************************
                 *           SOURCE FILE        *
                 *******************************/

%!  canonical_source_file(+Raw, -Cononical)
%
%   Determine the internal canonical filename from a raw file.

canonical_source_file(Source, File) :-
    absolute_file_name(Source, Canonical),
    (   source_file(Canonical)
    ->  File = Canonical
    ;   file_base_name(Source, Base),
        source_file(File),
        file_base_name(File, Base),
        same_file(Source, File)
    ->  true
    ;   File = Source               % system source files
    ).

%!  thread_self_id(-Id)
%
%   Get the current thread as atom  or   integer.  This is needed to
%   pass the thread id through XPCE. If   the caller is an engine we
%   return the id of the calling thread.

thread_self_id(Id) :-
    thread_self(Self),
    real_thread(Self, Thread),
    debug(gtrace(thread), 'real_thread: ~p --> ~p', [Self, Thread]),
    (   atom(Thread)
    ->  Id = Thread
    ;   thread_property(Thread, id(Id))
    ).

:- if(current_predicate(engine_create/3)).
real_thread(Self, Thread) :-
    thread_property(Self, thread(Thread)),
    !.
:- endif.
real_thread(Thread, Thread).
