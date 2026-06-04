/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- module(tube,
          [ load_tube/0,
            station/8,
            station/1,
            station_display_name/2,
            connection/4,
            line/4
          ]).
:- use_module(library(json)).
:- use_module(library(apply)).
:- use_module(library(dicts)).
:- use_module(library(pcre)).

/** <module> London tube data

Loads the London tube network  from   the  bundled `tube.json` file into
dynamic facts station/8, connection/4 and line/4.   The data is a Kaggle
dataset; ids are small integers that join the three relations.
*/

:- dynamic
    station/8,
    connection/4,
    line/4.

%!  load_tube is det.
%
%   Load `tube.json` (relative to  this   source  file)  into station/8,
%   connection/4 and line/4.  Idempotent:  after   the  first  call  the
%   predicate returns immediately.

load_tube :-
    line(_,_,_,_),                              % Already loaded
    !.
load_tube :-
    load_tube_nc.

load_tube_nc :-
    retractall(station(_,_,_,_,_,_,_,_)),
    retractall(connection(_,_,_,_)),
    retractall(line(_,_,_,_)),
    source_file(load_tube, Source),
    absolute_file_name('tube.json', DataFile,
                       [ access(read),
                         relative_to(Source)
                       ]),
    setup_call_cleanup(
        open(DataFile, read, In),
        json_read_dict(In, Dict, [ value_string_as(atom) ]),
        close(In)),
    maplist(save_station, Dict.stations),
    maplist(save_connection, Dict.connections),
    maplist(save_line, Dict.lines).

%!  station(?Id, ?Name, ?DisplayName, ?Zone, ?Lines, ?Rail, ?Lat, ?Lon)
%
%   True when a station is known. Id is   the numeric station id used to
%   join with connection/4. Name is  the   unique  short name (an atom),
%   DisplayName a possibly multi-line label for   rendering. Zone is the
%   fare zone (1..9), Lines the number of   tube  lines through it, Rail
%   the count of  national-rail  interchanges.  Lat   and  Lon  are  the
%   geographic coordinates as floats.

save_station(Dict0) :-
    mapdict(to_numeric(Dict0), Dict0, Dict),
    Dict :< #{id:Id, latitude:Lat, longitude:Lon,
              name:Name, display_name:DispName,
              zone:Zone,
              total_lines:Lines,
              rail:Rail},
    assertz(station(Id, Name, DispName, Zone, Lines, Rail, Lat, Lon)).

to_numeric(All, display_name, Value0, Value) :-
    !,
    cvt_display_name(Value0, Value, All.name).
to_numeric(_, colour, Value0, Value) :-
    !,
    atom_concat(#, Value0, Value).
to_numeric(_, _, Value0, Value) :-
    atom_number(Value0, Value),
    !.
to_numeric(_, _, Value, Value).

cvt_display_name('NULL', Name, Name) :-
    !.
cvt_display_name(Value0, Value, _) :-
    re_replace('<br />'/g, '\n', Value0, String),
    atom_string(Value, String).

%!  connection(?Station1, ?Station2, ?Line, ?Time)
%
%   True when two stations  are  directly   connected  by  a  tube line.
%   Station1 and Station2 are station ids;  Line   is  the line id (join
%   keys to station/8 and line/4). Time is   the  segment travel time in
%   minutes.

save_connection(Dict0) :-
    mapdict(to_numeric(Dict0), Dict0, Dict),
    Dict :< #{station1:S1, station2:S2, line:Line, time:Time},
    assertz(connection(S1, S2, Line, Time)).

%!  line(?Id, ?Name, ?Colour, ?Stripe)
%
%   True when a tube line is known. Id  is the numeric line id, Name the
%   full name (e.g. 'Bakerloo Line'). Colour   is  a `#RRGGBB` hex atom.
%   Stripe is the secondary colour as a  bare 6-digit hex atom for lines
%   that have one, or the atom 'NULL' otherwise.

save_line(Dict0) :-
    mapdict(to_numeric(Dict0), Dict0, Dict),
    Dict :< #{line:Line, name:Name, colour:Colour, stripe:Stripe},
    assertz(line(Line, Name, Colour, Stripe)).


                /*******************************
                *           ACCESSORS          *
                *******************************/

%!  station(?Name) is nondet.
%
%   True when Name is the name of a station.

station(Name) :-
    station(_Id, Name, _DispName, _Zone, _Lines, _Rail, _Lat, _Lon).

%!  station_display_name(?Name, ?DisplayName) is nondet.
%
%   Map a station Name to its DisplayName (possibly multi-line).

station_display_name(Name, DispName) :-
    station(_Id, Name, DispName, _Zone, _Lines, _Rail, _Lat, _Lon).
