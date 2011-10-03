/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(emacs_tags,
	  [ emacs_tag/4,			% +Symbol, ?Dir, -File, -Line
	    emacs_tag_file/1,			% ?File
	    emacs_init_tags/1,			% +FileOrDir
	    emacs_update_tags/0,
	    emacs_complete_tag/3		% +Prefix, ?Dir, :Goal
	  ]).

:- meta_predicate emacs_complete_tag(+, ?, 1).

:- use_module(library(pce)).
:- require([ call/2
	   , forall/2
	   ]).


/** <module> Query GNU-Emacs TAGS files

Make Emacs tags (produced with etags) information available to Prolog.
Predicates:

emacs_init_tags(+Directory|+TagFile)
  Reads the tags into the Prolog database.

emacs_tag(+Symbol, -File, -LineNo)
  Lookup tag in loaded tag-table
*/

:- dynamic
	tag_string/2,		% XPCE string holding content of tag file
	tag_file/2.


:- pce_global(@emacs_tag_file_regex,
	      new(regex('\f\n([^,]+),\\d+\n'))).
:- pce_global(@emacs_tag_line_regex,
	      new(regex('[^\n]*\\D(\\d+),\\d+\n'))).

%%	emacs_tag(+Symbol, ?Dir, -File, -Line)
%
%	Symbol is defined in File at Line.
%
%	@param	Dir is the directory in which the tag-file resides,
%		represented as a canonical file-name.

emacs_tag(Name, Dir, File, LineNo) :-
	tag_string(String, Dir), !,
	new(Re, regex('')),
	get(Re, quote, Name, QName),
	(   send(Re, pattern, string('\\\\y%s\\\\y', QName))
	;   send(Re, pattern, string('\\\\y%s', QName))
	;   send(Re, pattern, string('\\\\y%s%c', QName, 1))
	),
	get(Re, search, String, Start), !,
	send(@emacs_tag_file_regex, search, String, Start, 0),
	get(@emacs_tag_file_regex, register_value, String, 1, FNS),
	new(S, string('%s', Dir)),
	send(S, ensure_suffix, /),
	send(S, append, FNS),
	get(S, value, File),
	send(@emacs_tag_line_regex, match, String, Start),
	get(@emacs_tag_line_regex, register_value, String, 1, LNS),
	get(@pce, convert, LNS, int, LineNo).


%%	emacs_complete_tag(+Prefix, ?Directory, :Goal) is semidet.
%
%	Call call(Goal, Symbol) for  each  symbol   that  has  the given
%	Prefix.
%
%	@see	Used by class emacs_tag_item (defined in this file)

emacs_complete_tag(Name, Dir, Goal) :-
	(   Name == ''
	->  new(Re, regex('\\y[a-zA-Z_]\\w*'))
	;   new(Re, regex('')),
	    get(Re, quote, Name, QName),
	    send(Re, pattern, string('\\\\y%s\\\\w*', QName))
	),
	forall(tag_string(String, Dir),
	       complete_from_tag_string(String, Re, Goal)).

complete_from_tag_string(String, Re, Goal) :-
	new(Here, number(0)),
	repeat,
	    (	send(Re, search, String, Here)
	    ->  get(Re, register_end, End),
		send(Here, value, End),
		get(Re, register_value, String, 0, name, Symbol),
		call(Goal, Symbol),
		fail
	    ;	!
	    ).


%%	emacs_init_tags(+FileOrDir) is semidet.
%
%	Load tags from the given GNU-Emacs TAGS  file. If FileOrDir is a
%	directory, see whether <dir>/TAGS exists.

emacs_init_tags(TagFile) :-
	send(file(TagFile), exists), !,
	(   get(file(TagFile), time, TagDate),
	    tag_file(TagFile, LoadedTagDate),
	    send(LoadedTagDate, equal, TagDate)
	->  true
	;   get(file(TagFile), directory_name, Dir),
	    forall(retract(tag_string(Str, Dir)), free(Str)),
	    retractall(tag_file(TagFile, _)),
	    get(file(TagFile), time, TagTime),
	    send(TagTime, lock_object, @on),
	    load_tags(TagFile, Dir),
	    assert(tag_file(TagFile, TagTime))
	).
emacs_init_tags(Dir) :-
	send(directory(Dir), exists), !,
	atom_concat(Dir, '/TAGS', TagFile),
	emacs_init_tags(TagFile).


load_tags(File, Dir) :-
	new(F, file(File)),
	send(F, open, read),
	get(F, read, TagString),
	send(TagString, lock_object, @on),
	asserta(tag_string(TagString, Dir)),
	send(F, close),
	send(F, free).

%%	emacs_update_tags is det.
%
%	Reload all modified tag-files.

emacs_update_tags :-
	forall(tag_file(TagFile, _),
	       emacs_init_tags(TagFile)).


%%	emacs_tag_file(?File) is nondet.
%
%	True if File is a loaded Emacs tag-file.

emacs_tag_file(File) :-
	tag_file(File, _).


		 /*******************************
		 *	COMPLETION SUPPORT	*
		 *******************************/

:- pce_begin_class(emacs_tag_item, text_item).

initialise(TI, Label:name, Default:[name], Message:[code]*) :->
	send_super(TI, initialise, Label, Default, Message),
	send(TI, style, combo_box).

completions(TI, Text:name, Symbols:chain) :<-
	"Complete symbol from current TAGS-table"::
	(   tag_string(_, _)
	->  new(Symbols, chain),
	    catch(emacs_complete_tag(Text, _, add_completion(Symbols)),
		  too_many_matches,
		  send(TI, report, error, 'Too many matches')),
	    send(Symbols, sort, unique := @on)
	;   send(TI, report, warning, 'No tag-table loaded'),
	    fail
	).

add_completion(Chain, _Match) :-
	get(Chain, size, 5000),
	send(Chain, sort, unique := @on),
	get(Chain, size, 5000),
	throw(too_many_matches).
add_completion(Chain, Match) :-
	send(Chain, append, Match).

:- pce_end_class.
