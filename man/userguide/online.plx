\chapter{Using the online manual}		\label{sec:online}

In the previous sections we have introduced \product{} using examples.
The language primitives of \product{} are simple, but \product{} as a
whole is a massive library of very diverse classes with many methods.
The current system contains about 190 built-in classes defining over
3000 methods.

To help you finding your way around in the package as well as in
libraries and private code loaded in \product{}, an integrated manual is
provided.  This manual extracts all information, except for a natural
language description of the class, method or slot from the system and
thus guarantees exact and consistent information on available classes,
methods, slots, types, inheritance in the system.  The natural language
descriptions are taken from the \product{} reference manual.  For
classes defined in Prolog, the PlDoc comments in the source are used.

The manual consists of a number of search tools using different entry
points to the material.  A successful query displays the summary
information for the relevant hyper-cards.  Clicking on the summary
displays the cards themselves and hyper-links between the cards aid
to quickly browse the environment of related material.

The manual is started using manpce/0, which opens a window with a menu
bar from which the tools described below can be started.  Like the
other development tools, each manual tool is a \jargon{pane} that
appears as a tab or beside other panes in a window of the development
environment.  The most commonly used tools may also be started from
the \menuref{GUI} menu of the Epilog console and the other windows of
the development environment.


\section{Overview}

The online manual consists of a large set of tools to examine
different aspects of the \productpl{} environment and to navigate
through the available material from different view-points.

\begin{description}
    \manualtool{The inheritance hierarchy}{Browsers/Class Hierarchy}
\index{class,hierarchy}%
The `Class Hierarchy' tool allows the user to examine \product{}'s class
hierarchy.  This tool reads the inheritance relations from the class
objects and thus also visualises application or library classes.
\Figref{pceclasshierarchy} is created using this tool.
    \manualtool{The structure of a class}{Browsers/Class Browser}
\index{class,structure}%
The most important tool is the `Class Browser'.  It provides the
user with a view of material related to a class.  As everything in
\product{} is an object and thus an instance of a class this tool provides
access to everything in \product{}, except for the Prolog interface.
    \manualtool{Search Tool}{Browsers/Search}
This tool provides full search capabilities on the entire manual contents,
including combined search specifications.
    \manualtool{Globally available object references}{Browsers/Global Objects}
\index{global objects}%
The \product{} environment provides predefined objects (@pce, @prolog, @arg1, etc.).
The tool allows the user to find these objects.
    \manualtool{Errors}{Browsers/Errors}
\index{errors,browsing}%
This tool lists the errors and warnings \product{} may raise.  It may
be used to examine an error and to change its <-kind, i.e., how the
system reacts if the error occurs.
    \manualtool{Instances}{Tools/Inspector}
\index{inspecting instances}\index{object,inspecting}%
This tool is part of the runtime support system.  It allows you to
inspect the persistent state associated with objects.
    \manualtool{Structure of User Interface}{Tools/Visual Hierarchy}
\index{UI,structure of}\index{consists-of}%
This tool provides a `consists-of' view of all displayed visual objects.
It provides a quick overview of the structure of an interface.  It is
a useful for finding object-references, examining the structure of
an unknown UI and verifying that your program created the expected
structure.
    \manualtool{GUI preferences}{Tools/Class Variables}
\index{preferences}\index{class variable,editing}%
This tool allows the user to pick an object from the screen and edit
the class variables (see \secref{classvar}) that determine its
appearance.  Changes apply immediately and may be saved to the user's
{\em Defaults} file.
    \manualtool{The manual itself (help)}{File/Help}
\index{help,on manual}%
The manual tools are documented by itself.  Most tools have a `Help'
button that documents the tool.
    \manualtool{\product{} Demo programs}{File/Demo Programs}
\index{demo programs}%

The `Demo Programs' entry of the `File' menu starts an overview of the
available demo programs. A demo can be started by double-clicking it.
The sources of the demos may be found in \metafile{<home>/prolog/demo},
where <home> refers to the \product{} installation directory, which may
be obtained using

\begin{code}
1 ?- get(@pce, home, Home).
Home = '/usr/local/lib/swipl/xpce'.
\end{code}

Note that the DemoBrowser allows to view the sources of the main file
of a demo application immediately. Also consider using the
VisualHierarchy and ClassBrowser to analyse the structure of the demo
programs.
\end{description}


\section{Notational conventions}

The text shown by the online manual uses some notational conventions.
The various overview tools indicate candidate documentation cards with
a {\em \idx{summary line}}.  This line is of the form:

\begin{quote}
{<Identifier> <Formal~Description> [``<Summary>'']}
\end{quote}

\noindent
The `Identifier' is a single letter indicating the nature of the
documentation card.  The defined identifiers are: {\bf B}rowser
(Manual Tool), {\bf C}lass, {\bf E}xample, {\bf K}eyword, {\bf M}ethod,
{\bf O}bject, {\bf P}redicate, {\bf R}esource, {\bf T}opic and
{\bf V}ariable (instance-variable).

The `Formal Description' is a short description derived from the
described object itself:

\begin{center}
\begin{tabularlp}{{\tt M} class <-selector: type ... -->type}
\hline
{\tt V} class \verb$ - $selector: type
	& Variable that cannot be accessed directly \\
{\tt V} class \verb$<- $selector: type
	& Variable that may be read, but not written \\
{\tt V} class \verb$<->$selector: type
	& Variable that may be read and written \\
{\tt V} class \verb$ ->$selector: type
	& Variable that may only be written \\
{\tt M} class \verb$ ->$selector: type ...
	& Send-Method with argument-types \\
{\tt M} class \verb$<- $selector: type ... \verb$-->$type
	& Get-Method with argument-types returning value of type \\
{\tt R} Class.attribute:   type &
	Class-variable with type \\
\hline
\end{tabularlp}
\end{center}

\noindent
The same notational conventions are used in the running text of a
card.  See \secref{card}.


\subsection{Argument types}		\label{sec:atype}

\index{argument,type}\index{type}
\product{} is a partially typed language.  Types may be defined for both
method arguments and instance variables.  A type is represented by an
instance of class \class{type}.  \product{} defines a conversion to create
type objects from a textual representation.  A full description of this
conversion may be found in the online manual (method `type <-convert').
In this document we will summarise the most important types:

\begin{itemize}
\setlength{\itemsep}{5pt}
    \tick{int}
\product{} integer datum.
    \tick{<low>..<high>, <low>.., ..<high>}
Range of integers (including <low> and <high>). The latter two
constructs indicate one-side-unbound integer. Both <low> and <high> can
also be floating point numbers, indication a `real-range'.
    \tick{any}
Both integers and objects.  Function objects will be evaluated.
See \secref{function}.
    \tick{<class-name>}
Any instance of this class or one of its sub-classes.  Class
\class{object} is the root of the inheritance hierarchy.  The type {\tt
object} is interpreted slightly different, as it does {\em not} accept
instances of class \class{function} or its subclasses.  This implies
that the type {\tt object} forces functions to be evaluated.
    \tick{{\tt [}<type>{\tt ]}}
Either this type or @default.  Trailing arguments to methods that
accept @default may be omitted.
    \tick{<type>{\tt *}}
Either this type or @nil.
    \tick{<type> {\tt ...}}
Methods with this type specification accept any number of arguments
that satisfy <type>.
    \tick{\{<atom1>,<atom2>,\ldots\}}
Any of these name objects.
\end{itemize}

\noindent
For example, the ->initialise method of a graphical text object has
type declaration:

\begin{code}
[char_array], [{left,center,right}], [font]
\end{code}

The first argument is an instance of class \class{char_array}, the
super-class of \class{name} and \class{string}.  The second argument
either `left', `center' or `right' and the last argument is a font
object.  All arguments are between square brackets and may thus be
omitted.


\section{Guided tour}

This section provides a `guided tour' through the manual system. If you
have \productpl{} at hand, please start it and try the examples. For each
of the central tools of the manual we will present a screendump in a
typical situation and explain the purpose and some common ways to use
the tool.


\subsection{Class browser}		\label{sec:classbrowser}

\postscriptfig[width=\textwidth]{classbrowser}{The Class Browser}

The ``Class Browser'' is the central tool of the online manual. It
provides an overview of the functionality of a class and options to
limit the displayed information. \Figref{classbrowser} shows this tool
in a typical situation. In this example the user is interested in
methods dealing with `caret' in an \class{editor}.

The dialog to the left-side of the tool specifies {\em what} information
is displayed. The top-right window displays the class together with the
initialisation arguments (the arguments needed to create an instance of
this class).  Double-left-click on this text will open the description
for ->initialise.

Below this text a hierarchy is displayed that indicates the place in the
inheritance hierarchy as well as the classes to which messages are {\em
delegated} (see \secref{delegation}).  The user can select multiple
classes only if there is delegation and the tree actually has branches.
Use class \class{editor} or class view to explore these facilities.
Clicking a class selects or deselects it, after which the class-browser
immediately searches for methods in all classes below the selected
classes.  If a method is found in multiple classes the class-browser
will automatically display only the one method that will actually be
used by this class.

The large right window displays a list of matching classes, variables,
methods and class-variables.  The class itself is always shown at the
top.  If an item is tagged with an open book, the reference manual
provides a description that may be read by (double-) clicking the card
to start the ``Card Viewer'' (see \secref{card}).

\subsubsection{The ClassBrowser dialog}

The {\bf Class} text_item (text-entry-field) may be used to  switch to
a new class. Note that this text_item implements \idx{completion} (bound
to the space-bar).

The {\bf Filter} menu filters the candidate objects according to their
categorisation.  Selecting {\sf all} switches off filtering, which is
often useful in combination with {\bf Search}.  Clicking {\sf all}
again switches back to the old selection of categories.  The meaning
of the categories is:

\begin{itemize}
\setlength{\itemsep}{5pt}
    \tick{Basic}
Principal methods that are used very often. This is, together with
{\em Application}, the default selection of this menu.
    \tick{Advanced}
Less often used and sometimes complicated methods.
    \tick{Rare}
Infrequently used methods.  Note that does not mean they are complicated
or do things you'd hardly ever want to use.  For example, most of the
caret-manipulation of class editor is in this category.  It is essential
and commonly used behaviour of the editor, but rarely used directly from
program-control.
    \tick{Internal}
Behaviour that is not directly intended for public usage.  It may be
useful to understand how other methods interact.  Try to avoid using
these methods or variables in your code.
    \tick{Basic OO}
Methods intended to be redefined in user-defined classes. See
\chapref{udc}.
    \tick{Advanced OO}
Methods that may be redefined in user-defined classes, but for which
this is far less common.
    \tick{Application}
Methods implemented in the host-language.
\end{itemize}


The {\bf Display} menu determines the objects searched for.  {\sf Sub
Class} refers to the direct sub classes of this class. The other fields
refer to instance-variables, methods with send- and get-access and
class-variables.

The {\bf Search} and {\bf ... In} controllers limit the displayed cards
to those that have a match for the specified regular expression in one
of the specified fields.  The list is updated while typing.  Searching
is case sensitive.  Searching in the {\sf Name} field is useful to find
a particular method if the name (or part of it) is known.

Changes to any of the controllers of the dialog are applied
immediately.


\subsubsection{Example queries to the classbrowser}

Below we illustrate how some commonly asked questions may be answered
with the class browser.

\begin{itemize}
    \tick{What are variables of a bitmap?}
Select {\sf Variable} in the {\bf Display} menu, clear {\bf Search},
and set {\bf Filter} to {\sf All}. Then type `bitmap' in {\bf Class} and
hit return.  Note that by clicking on class \class{graphical}
in the inheritance display not only the variables of class
\class{bitmap} itself are shown, but also those of class
\class{graphical}.
    \tick{How can I position the caret in an editor?} The caret can
only be changed using send-methods. Either the name or the summary is
likely to have `caret' as a substring.  Thus, {\bf Display} is set to
{\sf Send Method}, {\bf ... In} to {\sf Name} and {\sf Summary},
search `caret'.
\end{itemize}


\subsubsection{Methods with special meaning}

This section describes the role of the `special' methods.  These are
methods that are not used directly, but they define the behaviour of
new/2, type conversion, etc.  and knowing about them is therefore
essential for understanding an \product{} class.

\begin{description}
    \sendmethod{object}{initialise}{<Class-Defined>}
\index{objects,creating}%
The ->initialise method of a class defines what happens when an instance
of this class is created.  It may be compared to the \idx{constructor}
in C++.  Note that double-clicking the class description in the
class-browser (top-right window) opens the reference card for the
->initialise method.  See also new/2, \secref{new2}.
    \sendmethod{object}{unlink}{}
\index{object,remove}%
The ->unlink method describes what happens when an instance of this
class is removed from the object-base and may be compared to the C++
\idx{destructor}.
    \getmethod{object}{lookup}{<Class-Defined>}{object}
\index{reusability}%
If defined, this method describes the lookup an already
defined instance instead of object creation.  For example

\begin{code}
1 ?- new(X, font(mono, normal, 13)).
X = @mono_normal_13
2 ?- new(Y, font(mono, normal, 13)).
Y = @mono_normal_13
\end{code}

The same instance of the reusable font instance is returned on a second
attempt to create a font from the same parameters.  Examples of classes
with this capability are: \class{name}, \class{font}, \class{colour},
\class{image} and \class{cursor}.
    \getmethod{object}{convert}{<Class-Defined>}{object}
\index{type,conversion}%
Defines what can be converted into an instance of this type.  If an instance
of this class is requested by a type but another object is provided
\product{} will call this method to translate the given argument into an instance
of this class.
    \sendmethod{object}{catch_all}{<Class-Defined>}
\index{catch-all}%
The ->catch_all method defines what happens with messages invoked
on this object that are not implemented by any other method.
    \getmethod{object}{catch_all}{<Class-Defined>}{any}
As ->catch_all, but for get-operations.
\end{description}


\subsection{Reading cards}		\label{sec:card}

\postscriptfig[width=\textwidth]{card}{The Card Viewer}

The other tools of the manual allow the user to {\em find} cards with
documentation on the topic(s) the user is looking for.  The information
provided by the summary-lists often suffices for this purpose.  Whenever
a card is marked with an open book in the summary list it may be opened
by double-clicking it.  This starts the ``Card Viewer'' tool.
\Figref{card} is a screendump of this tool.

The ``Card Viewer'' displays the description of the displayed object
(method, variable, class, ...) from the reference manual, rendered as
HTML.  For classes defined in Prolog, the PlDoc comments of the class
and its members are shown.  References to other manual material are
hyper-links: clicking a link jumps to the related material.  The arrow
buttons navigate back and forward through the history of visited cards.
If a method or class variable is described by the instance variable with
the same name, the card says so and shows the description of the
variable.

The {\bf Goto} field allows for switching to a new card.  The syntax for
this field is similar to the manpce/1 and tracepce/1 predicates
description in \secref{debugging}.  It consists of a classname, followed
by \verb$->$ to indicate a send-method, \verb$<-$ for a get-method and
\verb$-$ to specify an instance-variable without considering associated
methods.

The item performs {\em completion} bound to the space-bar.  The first
word is completed to a class-name. The second to a send-method, variable
or get-method.  Method completion considers inheritance and delegation.%
    \footnote{Given the dynamic nature of delegation, the system cannot
	      possibly determine all methods available through
	      delegation. Consider a slot specified with type
	      \class{graphical}.  The system can infer it will surely
	      be able to use behaviour defined at class
	      \class{graphical}.  If at runtime, the slot is filled with
	      a \class{box}, all methods defined at class box will be
	      available too.}


\subsection{Search tool}		\label{sec:searchtool}

The search tool is shown in \figref{searchtool}. It allows the user to
search through all \product{} manual cards in an efficient manner with
queries similar to that what is found in \idx{WAIS} tools. A search
specification is an expression formed from the following primitives:

\begin{itemize}
\setlength{\itemsep}{5pt}
    \tick{Word}
Specifies all cards containing a word for which the search
specification is the {\em prefix}.  Case is ignored.
    \tick{{\tt\string<}Word{\tt\string>}}
Specifies all cards that contain the indicated word.  Case is ignored.
    \tick{Expr1 {\tt and} Expr2}
Specifies all cards satisfying both conditions.
    \tick{Expr1 {\tt or} Expr2}
Specifies all cards satisfying either condition.
\end{itemize}

\noindent
As a special shorthand, just specifying multiple words refers to all
cards containing all these words.

If the user stops typing for more than a second, the system will parse
the expression and display the number of matching cards.

The \class{browser} window on the left contains all words occurring
anywhere in the manual.  The window on the right is used to display the
card summaries of all matching cards.

\postscriptfig[width=\textwidth]{searchtool}{Manual search tool}

\subsection{Class hierarchy}		\label{sec:classhierarchy}

\postscriptfig[width=\textwidth]{classhierarchy}{Class Hierarchy Tool}

The ``Class Hierachy'' tool shown in \figref{classhierarchy} may be used
to get an overview of \product{}'s class hierarchy or to find the
(inheritance) relations of a particular class with other classes. Note
that \product{}'s inheritance hierarchy has a technical foundation
rather than a conceptual. Super-classes are motivated by the need for
code-sharing.

Typing a class name or a wildcard pattern in the {\bf Class} field
expands the tree to show the matching classes and selects them.
\Figref{classhierarchy} shows the place of the classes \class{browser}
and \class{list_browser}.  The popup menu on a class opens its
documentation or the class browser, expands or prunes the tree and,
for a user-defined class, shows the source.


\section{Summary}

The online manuals integrate visualisation of \product{}'s internal
structure with a hyper-text system.  This approach guarantees
consistency between the documentation and the actual system and
integrates overview and documentation of library and user-defined
classes in one system.

The online manual tools provides various entry-points (classes,
global objects, predicate overview, keywords, etc.) to obtain a list
of {\em card summaries}.  Cards may be opened from these summary
lists to examine its contents.
