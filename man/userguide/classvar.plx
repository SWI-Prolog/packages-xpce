\chapter{Class Variables}			\label{sec:classvar}

Class variables act as storage for class-constants. They are normally
used for storing setting information, such as fonts, colours etc. For
this reason, the default value for a \class{class_variable} is defined
with the declaration of it, but this default may be overruled using a
\file{Defaults} file. \product{} reads two such files, both of which
are optional:

\begin{itemize}
    \item The site-wide file \file{Defaults} in the \product{} home
directory (`@pce <-home').  This file is not part of the distribution.
    \item The personal file \file{Defaults} in the \product{}
application data directory (`@pce <-application_data'), which is read
last and thus takes precedence.  On Unix systems this is normally
\file{~/.config/swi-prolog/xpce/Defaults}, on MacOS
\file{~/Library/Application Support/swi-prolog/xpce/Defaults} and on
Windows \file{\%APPDATA\%\bsl{}swi-prolog\bsl{}xpce\bsl{}Defaults}.
\end{itemize}

An application that must not depend on the preferences of its user can
set the Prolog flag \const{xpce_defaults} to \const{none} to avoid
loading the personal file, or to the name of a file to load instead.
The flag must be set before \pllib{pce} is loaded, e.g., using
\mbox{\tt swipl -Dxpce_defaults=none \ldots}

Class variables are typed and documented.  Many \product{} built-in
classes define class-variables.  These can be examined using the
\tool{ClassBrowser} (see \secref{classbrowser}) from the online manual
tools.  The class variables that apply to a graphical object can be
edited interactively using class_variable_editor/[0,1] from
\pllib{pce_class_variable_editor}, which is also available as
{\sf Settings/Edit GUI preferences} in the IDE.  This tool allows
picking an object from the screen, applies changes immediately to the
running application and saves them in the personal \file{Defaults}
file.  The appearance of \product{} applications is also determined by
the colour \jargon{theme}, see \secref{themes}.


\section{Accessing Class Variables}		\label{sec:classvaraccess}

Class variables define \jargon{get-}behaviour and can be accessed using
the normal get/[3-8] call. Class variables are the last type of
behaviour checked when resolving a get-method. Below are the most
commonly used methods to access class-variables.

\begin{description}
    \getmethod{object}{class_variable_value}{name}{any}
Return the value of the named class-variable.  Fails silently if the
class does not define the given class-variable.
    \getmethod{class}{class_variable}{name}{class_variable}
Return the \class{class_variable} object with the given name.  Fails
silently if the class does not define the given class-variable.
    \bothmethod{class_variable}{value}{any}
Reads or writes the class-variable value.  The argument is type-checked
using `class_variable<-type' if the value is written. Writing
class-variables should be handled with care, as existing instances
of the class are not notified of the change, and may not be prepared to
deal with changes of the class-variable value.  pce_image_directory/1
is an example of a predicate modifying the image.path class-variable.
\end{description}


\section{Class variable and instance variables}

Class-variables may be used as defaults for instance-variables that can
be modified through the \file{Defaults} file. For example, class
\class{text} defines both the instance- and class-variable {\em font}.
The class-variable defines the default font, while the font can be
modified explicitely at instance-initialisation, or using the ->font
method afterwards.

If a class defines both an instance- and a class-variable with the same
name, object-allocation will use the \class{constant} @class_default for
the initial value of the slot. First access will replace this value
using the value from the class-variable.

Instance-variables supercede class-variables for \jargon{get-}behaviour.
To access the class-variable explicitly, use the methods from
\secref{classvaraccess}.


\section{The `Defaults' file}

The file \file{Defaults.user} in the \product{} home directory is a
commented template for this file.  It is copied to the personal
\file{Defaults} file if this does not yet exist when the user opens it
for editing using the {\sf Edit defaults file} button of the class
variable editor.

The \file{Defaults} file consists of statements. Each statement is on a
separate line. A statement may be split over several lines by preceeding
the newline with a backslash (\verb$\$). The exclamation mark
(\const{!}) is the line-comment character. All input from the \const{!}
upto the following newline is replaced by a single space-character.
Empty lines or lines containing only blank space are ignored.

Default files may include other default files using the statement

\begin{quote}
{\tt \#include} <file>
\end{quote}

Default statements are of the form:

\begin{quote}
<class>.<class-variable>: <value>
\end{quote}

Where <class> denotes the name of the class, or \const{*} to indicate
the default applies for any class defining this class-variable.  If both
forms are found, the statement with the explicit class-name is used. 
<class-variable> denotes the class-variable name. <value> is the default
value of the class-variable.  The syntax for <value> is similar to the
arguments of send/[2-8]. The (informal) syntax for <value> is below.

\def\isa{}
\def\ora{}
\renewcommand{\isa}{&    ::= &}
\renewcommand{\ora}{& $\mid$ &}
\begin{quote}
\begin{tabular}{|lrl|}
\hline
<Any>			\isa <int>		\\
			\ora <float>		\\
			\ora <Name>		\\
			\ora @<Name>		\\
			\ora <Chain>		\\
			\ora <Object>		\\
\hline
<Chain>			\isa {\tt [} <Any> \{, <Any>\} {\tt ]} \\
			\ora {\tt [} {<Blank>} {\tt ]} \\
\hline
<Object>		\isa <ClassName>{\tt ()} \\
			\ora <ClassName>{\tt (} <Any> \{, <Any>\} {\tt )} \\
			\ora <PrefixOp> <Any> \\
			\ora <Any> <InfixOp> <Any> \\
			\ora <Any> <PostfixOp> \\
			\ora {\tt "} <String> {\tt "} \\
\hline
<String>		\isa \{<CharNotDoubleQuote>$|${\tt ""}\} \\
\hline
<Name>			\isa <Letter>\{<Letter>$|$\bnfmeta{Digit}\} \\
			\ora {\tt '} \{<CharNotSingleQuote>$|${\tt ''}\} {\tt '} \\
\hline
\end{tabular}
\end{quote}


\section{Class variables in User Defined Classes}


Class-variables are declared, similar to instance-variables. through
macro-expansion inside the :- pce_begin_class/[2,3] \ldots :-
pce_end_class/0 definition of a class.  The syntax is:

\begin{quote}
{\tt class_variable(}<name>{\tt ,}
		     <type>{\tt ,}
		     <default>{\tt ,}
		     [<summary>]{\tt )}.
\end{quote}

<default> defines the value if not overruled in the \file{Defaults}
file.  It is a Prolog term describing an object similar to the arguments
of send/[2-8].

In the following example. there is a class with the property
`expert_level'.  The program defines the default level to be novice.
The user may change that in his/her personal \file{Defaults} file or
change it while the application is running.  As the value may change
at runtime, there should be an instance- as well as a class-variable.
Below is the skeleton for this code:

\begin{code}
variable(expert_level, {novice,advanced,expert}, get,
	 "Experience level of the user").

class_variable(expert_level, {novice,advanced,expert}, novice).

expert_level(Obj, Level:{novice,advanced,expert}) :->
	send(Obj, slot, expert_level, Level),
	<handle changes>.

	...,
	(   get(Obj, expert_level, expert)
	->  ...
	;   ...
	),
	...
\end{code}

\mbox{}
