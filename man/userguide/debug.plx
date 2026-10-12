\chapter{Development and debugging tools}	\label{sec:debugging}

This section describes various tools and techniques to help finding bugs
in \productpl{} code.  Most of the tracing is done in the hosting
Prolog language.  \product{} provides additional tools for tracing
the execution of certain methods, breaking on the implementation of
a Prolog-defined method, finding and inspecting objects.


\section{Object-base consistency}

Unlike Prolog, \product{} is not {\em secure}: if a Prolog environment traps
a fatal error there is almost always a bug in the Prolog system.
Except for violating system limits there is no Prolog program that can
make the Prolog environment crash.  For \product{} this is different.
Consider the following example:

\begin{code}
1 ?- new(@p, picture),
     send(@p, display, new(B, box(100,100))),
     get(B, area, Area),
     free(Area).
B = <pce>(0x7f8a12c5d0,box),
Area = <pce>(freed).
\end{code}

After these calls, the <-area attribute of the box has been destroyed,
but the box is not aware of this fact.  The utility predicate
checkpce/0 scans the \product{} object-base for various inconsistencies and
will report that the box contains a slot referring to a freed object.
For each problem, it prints the path from a global object to the
faulty object.  If problems are found, checkpce/0 fails.

\begin{code}
2 ?- checkpce.
% PCE: Checking 281 global objects, 1 Prolog references (1 freed).
[PCE warning: <pce>(0x7f8a12c5d0,box): Freed object in slot
  graphical -area: <pce>(0x7f8a12c400,area) (freed)]
        Path from checked object:
          ...
          @p/picture (refs=7, code_refs=0, locked) <-graphicals
          <pce>(0x7f8a12c600,chain) (refs=1, code_refs=0)[1]
          <pce>(0x7f8a12c5d0,box) (refs=3, code_refs=0) <-area
[PCE: Checked 6749 objects]
false.
\end{code}

\product{} uses heuristics trying to avoid that such problems actually
crash the system (in the example above execution continues
normally).

We advice using checkpce/0 regularly during program development to
verify your application is not violating object consistency.  Please
see \secref{whoiswho} and \secref{hyper} for techniques
to avoid `dangling' object references.


\section{Tracing methods}

It is often useful to inspect the execution of a particular method.
Suppose we want to monitor geometry-changes of graphical objects.  The
utility predicate tracepce/1 (see also \secref{interface}) may be used:

\begin{code}
1 ?- tracepce((graphical->geometry)).
Tracing Send method: graphical ->geometry
\end{code}

As \verb$->$ is a standard Prolog operator with priority over 1000,
tracepce/1 requires two brackets.  Get-methods may be specified in
a similar way.  \verb$<-$ is an infix operator with priority 250, so
no extra brackets are needed:

\begin{code}
2 ?- tracepce(graphical<-area).
Tracing Get method: graphical <-area
\end{code}

Instance variables may be specified as <class>-<variable>. A trace-point
on an instance variable makes both reading and writing the variable
visible.

The predicate notracepce/1 disables a tracepoint.

\section{Breaking (spy) on methods}

\index{spy}\index{break}%
Prolog-defined methods are all implemented using the same predicate,
which makes it hard to use spy/1 for starting the debugger on a method.
One way around is to stick a call to trace/0 into the method body and
recompile the file.  For those among us who dislike this idea there
is the possibility to use spypce/1.

\begin{description}
    \predicate{spypce}{1}{+Spec}
\arg{Spec} specifies a send- or get-method like tracepce/1.  If the
method is defined in Prolog \product{} calls trace/0 just before
starting the implementation of the method.
    \predicate{nospypce}{1}{+Spec}
Remove a spy point set with spypce/1.
\end{description}


\section{Visual hierarchy tool}		\label{sec:vishierarchy}

\index{hierarchy,of UI components}
The ``Visual Hierarchy'' tool provides an overview of the consists-of
relations between the various UI components in a tool.  It is part of
the online manual tools and may be started from the \menuref{Tools}
menu of the manual (see manpce/0) or using \menuref{GUI/Inspect GUI
hierarchy} from the Epilog console or the other windows of the
development environment.  \Figref{vishierarchy} shows this tool
examining the structure of the ``PceDraw'' demo program.

\postscriptfig{vishierarchy}{Visual Hierarchy Tool showing ``PceDraw''}

This tool is very useful to examine the structure of existing
applications (for example the various demo programs).  It may also
be used to find object-references and to launch the inspector
(\secref{inspector}) on a particular object.

There are three ways to expand the tree of the visual hierarchy
tool.  The first is to expand the tree from the initially displayed
root object.  The second is to type the reference in the ``Add''
text_item and press {\sc return}.  The most comfortable way is to
click the hand button on the tool bar and select the target object
on the screen.  The popup menu on a node of the tree allows for
showing the source of a user-defined class, examining the class in the
class browser and opening the inspector on the object.


\section{Inspector tool}		\label{sec:inspector}

The inspector provides a visual representation of all attributes of an
object.  It is a visual form of the debugging predicate show_slots/1
which dumps the class and slot value of the argument reference in
the Prolog window.  The inspector is started from the
\menuref{Tools} menu of the manual tools, using
\menuref{GUI/Inspect XPCE object} or from the popup menu of the visual
hierarchy tool.  An object can be added by typing its reference or by
selecting it on the screen using the hand button of the tool bar.

\postscriptfig{inspector}{Inspector Tool}
\index{object,inspecting}\index{inspector}

Each inspected object is shown as a card.  The title of the card shows
the object reference, its flags (e.g., {\tt L} if the object is locked)
and its reference count.  The rows show the slots of the object and
their values.  Clicking a value that is an object flashes it if it is
displayed, and double-clicking it adds a card for this object, linked
to the slot by an arrow.  \Figref{inspector} shows a chain holding a
point, an area and a name, where the point and area have been expanded.

