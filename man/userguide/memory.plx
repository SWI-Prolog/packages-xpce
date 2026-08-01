\chapter{Memory management}		\label{sec:memory}

This chapter describes the memory- and object-management aspects of PCE.


\section{Lifetime of an object}

\index{garbage collection}\index{object,removing}
Object lifetime management is a difficult issue in PCE/Prolog as PCE
cannot be aware of all references to PCE objects stored in Prolog.
Another complicating factor is that non-incremental garbage
collection as performed by most Lisp systems is undesirable because
they harm the interactive response of the system.  For these reasons
PCE performs {\em incremental} garbage collection.  It distinguishes
a number of prototypical `life-cycles' for objects.  Heuristics tell
the system which of these is applicable and when the object may be
deleted. 

PCE distinguishes between {\em global-}, {\em top level-}, {\em
support-}, {\em argument-} and {\em answer-} objects.  {\em Global}
objects are created and exist for the entire PCE session: @prolog,
class objects, etc.  {\em Top-level} objects are the principal objects
of the application.  They should exist even if no other PCE object
refers to them.  An example of a top level object is a frame or
hash_table representing a database in the application.  {\em Support}
objects only complete the definition of other objects.  If this
`other' object is removed, the support object may be removed as well.
An example is the area attribute of a graphical.  {\em Argument}
objects are objects created to serve as an argument to a message.  For
example a graphical may be moved to a position described by a point
object.  The point may be deleted when the message is completed.
Finally, {\em answer} objects are the result of some computation.  For
example `area <-size' returns a size object.  This object may be
deleted when the code that requested the value is done with it.

PCE maintains the following information on objects to support
garbage collection.  This information may be requested using the PCE
inspector (see \secref{inspector}).

\begin{description}
    \tick{Protect Flag}
This flag may be set using `object ->protect'.  When set, the object
can not be freed by any means.  This flag is set for most global and
reusable objects: @prolog, @pce, @display, names, classes, etc.
    \tick{Lock Flag}
This flag indicates that the object may not be removed by the garbage
collector.  Locked objects can only be freed by sending an explicit
`object ->free' message' or using the predicate free/1.  It is used to
avoid that `top level' objects such as frames are deleted by the
garbage collector.  It is also used to indicate that Prolog wants to
be responsible for destruction of the object rather than PCE's garbage
collector.  The lock flag is automatically set on
any object that has a named reference.  If Prolog wants to store
integer object references in the Prolog database locking is often
necessary to protect the object for the PCE garbage collector.  See
also \secref{pceprolog}.
    \tick{Answer Flag}
This flag indicates that the object has been created as an answer of
some computation or as a result of the Prolog predicate new/2.  The
{\em answer} status is cleared if the object is used to fill a slot
of another object%
    \footnote{PCE assumes the object has become a {\em support} object.
	      This is generally not correct for code objects.  Class
	      \class{code} therefore has `class <-un_answer: @off', which
	      implies that objects that fill a slot of a code object will
	      not loose their `answer' status.}
or `object ->done' is invoked on the object.
    \tick{Reference Count}
PCE maintains the number of other objects referring to this object.
When the reference count drops to zero and none of the protect, lock
or answer flags are set PCE assumes the object is garbage and removes
the object from the object base.  A reference held by Prolog counts:
see \secref{prologrefs}.
\end{description}


\section{References held by Prolog}
\label{sec:prologrefs}

\index{reference,Prolog}%
An anonymous object reference passed to Prolog is a {\em blob}: an opaque
handle written as \const{<pce>(0xADDR,ClassName)}.  Holding such a handle
is a reference to the object, exactly like a slot of another object
referring to it, and it is counted as one.  Consequently an object that
Prolog can still reach is never garbage collected, and an object that
Prolog can no longer reach is reclaimed once SWI-Prolog's atom garbage
collector discovers the handle has become unreachable.

Two properties follow, and both are relied upon:

\begin{itemize}
    \item A reference can not dangle.  \const{->free} on an object Prolog
still refers to marks the object destroyed but keeps its administration
alive, so a later use reports a clean error rather than silently denoting
whatever object was allocated in its place.
    \item Objects created and dropped in a loop do not accumulate.
\end{itemize}

Named references (\const{@name}) are unchanged: they are Prolog atoms
inside the term \functor{@}{1}, and a named object is locked, so it lives
until it is explicitly freed.

Do not take a reference apart with \exam{Obj = @Ref}: that only matches a
named reference.  Use is_object_reference/1 to test a term, and
object_reference/2 and object_from_reference/2 for the name of a named
object.  An anonymous object has no identity besides its handle: keep the
handle, compare it with ==/2 and index on it.

XPCE identifies an anonymous object internally by its address, which
\verb$object<-object_reference$ reports, but that number is not a
reference and cannot be turned back into an object.  It was one until
handles replaced it, written \exam{@\arg{Integer}}, and it was the reason
a reference could go stale.

\subsection{Porting code that inspects references}
\label{sec:refporting}

An anonymous reference used to be \exam{@Integer}, a compound term, so it
answered the Prolog type tests the same way a named reference does.  A
handle does not, and the difference is easy to overlook because the two
forms now disagree:

\begin{center}
\begin{tabular}{lll}
\hline
& \exam{@name} & anonymous \\
\hline
atom/1     & no  & no  \\
atomic/1   & no  & {\bf yes} \\
compound/1 & yes & {\bf no}  \\
\hline
\end{tabular}
\end{center}

The trap is atomic/1.  Code that asks ``is this a name or an object?'' by
testing atomic/1 now answers ``name'' for an anonymous object.  Where the
question is really ``is this a name'', use atom/1, which is false for both
kinds of reference.  Where it is ``is this a plain value rather than an
object'', use atomic/1 together with is_object_reference/1:

\begin{code}
    (   atomic(X),
        \+ is_object_reference(X)
    ->  ...                         % a name, number or string
    ;   ...                         % an object
    )
\end{code}

The same applies to matching a reference by its shape.  A clause head such
as \exam{emit([@Ref|T], ...)} no longer sees anonymous references; test
with is_object_reference/1 instead.

Finally, an object reference has never been convertible to text by writing
it, and that has not changed: \exam{format('~w', [Obj])} yields the
reference, not the object's contents.  Ask the object, for example with
\exam{get(Obj, value, Text)} for a \class{string}.  Code that wrote a
reference into text and read it back used to produce a term that parsed but
meant nothing; it now produces a syntax error.


\section{Practical considerations}

\index{object,management}%
The principal predicates new/2, send/[2-12] and get/[3-13] will destroy
all argument- and answer- objects created during their execution except
for the object created by new/2 and the object returned by get/[3-13].

An object created by new/2 with an anonymous object reference needs no
attention: Prolog holds a reference on it, so it survives as long as it is
reachable and is reclaimed when it is not.  Neither locking it nor
`object ->done' is required, and `object ->done' has no effect while
Prolog can still reach the object.  Use free/1 to destroy an object at a
known moment rather than leaving it to the garbage collector.

\begin{quote}
Before the introduction of blob references this was not so: an object
returned to Prolog stayed on the answer stack forever unless it was
attached, locked or explicitly finished with `object ->done'.  Code
written against that model still works, but the precautions are no
longer needed.
\end{quote}


\section{Memory usage of objects}

\index{object,memory usage}\index{memory usage}
Currently an object consists of an object-header and an array of
instance variables.  The object-header includes various flags, a
reference count and a pointer to the class.  The size of an object
header is 12 bytes.  Each instance variable consumes an additional
4 bytes. For example a point object has `x' and `y' instance
variables and thus consumes $12 + 2 * 4 = 20$ bytes.

The method `class <-instance_size' returns the size of an instance of
this class in bytes.  Note that the costs of supporting objects is not
considered in this value.  For example a box object has instance size:

\begin{code}
1 ?- get(class(box), instance_size, S).
S = 72
\end{code}

But a box has an <-area instance variable consuming an additional 28
bytes.
