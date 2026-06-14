% XPCE/Prolog predicates -- input for doc2tex/pldoc2tex.
%
% Manual entries for every public XPCE predicate. Originally
% maintained as predicates.md / predicates.doc; rewritten as a
% .plx file so the entries land in manindex.db (and are reachable
% from help/1) the same way the SWI-Prolog manual's predicates do.

\begin{description}
    \predicate{manpce}{0}{}
    \predicate{manpce}{1}{+What}
    \label{sec:xpce-manpce}
Start the XPCE online manual tools. Without arguments, this just
opens the \textit{PCE Manual} main window. If the argument is the
name of a class, it will start the ClassBrowser on this class. If
the argument describes a method or variable, it will open the
CardViewer on the described object. Syntax:

\begin{center}
\begin{tabular}{ll}
\verb$class -> send_method$ & Describes a send-method \\
\verb$class <- get_method$ & Describes a get-method \\
\verb$class - variable$ & Describes an instance variable \\
\end{tabular}
\end{center}

For example:

\begin{code}
?- manpce(button->message).
\end{code}

will start the CardViewer on this method. See also editpce/1.

    \predicate{new}{2}{?Ref, +Class}
    \label{sec:xpce-new}
Create an instance of \arg{Class}, using its arguments as
initialisation arguments. \arg{Ref} is unified with the newly
created object reference. See also pce_global/2.

    \predicate{send}{2}{+Object, +Message}
    \label{sec:xpce-send}
Invoke a send-method on \arg{Object}. \arg{Object} is processed as
the initialisation arguments described with new/2. This implies that
a complex term is translated into an object before the method is
invoked. An atom is translated into an XPCE name object. \arg{Message}
is an atom or complex term. The functor defines the \textit{selector}:
the name of the operation to invoke. The arguments are processed as
the initialisation arguments described with new/2.

Sending a message is composed of the following steps:

\begin{itemize}
    \item Resolve the receiving object. For this, \arg{Object} is
    translated into a true XPCE object.
    \item Resolve the selector. The functor of \arg{Message} is
    determined and translated into an XPCE name object.
    \item Resolve the implementation. This can be a send\_method
    object, a variable object or one of the defined object-level
    implementations. This step also resolves a description of the
    required arguments.
    \item Translate the arguments of the message to data that
    satisfies the resolved argument description.
    \item Invoke the implementation. If the implementation is defined
    in Prolog, the interface will make a direct call to it; otherwise
    it calls an XPCE interface function.
\end{itemize}

If all these steps succeed and the implementation succeeds, send/2
succeeds. If anything fails, an error is raised through
object->error. If the error has
\verb$error<-feedback: throw$, a Prolog exception is raised;
otherwise XPCE itself reports the error.

See also get/3, send_class/3, send_super/2, new/2 and free/1.

    \predicate{send_class}{3}{+Object, +Class, +Message}
    \label{sec:xpce-send_class}
Like send/2, but the implementation of the indicated \arg{Class}
(possibly inherited from an even higher class) is used rather than
the implementation of the class to which \arg{Object} belongs.

\arg{Class} must be a super-class of the class to which
\arg{Object} belongs.

This construct is normally not used directly. Instead, send_super/2
should be used, which is translated at compile time to a proper
send_class/3 call.

    \predicate{send_super}{2}{+Object, +Message}
    \label{sec:xpce-send_super}
The goal send_super/2 is actually not a predicate: it can be used
as such in the context of a method declaration (see :->/2) and is
expanded by the class-compiler into an appropriate send_class/3
call. Example:

\begin{code}
:- pce_begin_class(myapp, frame).

initialise(A) :->
    send_super(A, initialise, 'My 1st application'),
    send(A, append, new(D, dialog)),
    ...
\end{code}

See also send_class/3, send/2 and get_super/3.

    \predicate{send_list}{2}{+Ref, +Selector}
    \predicate{send_list}{3}{+Ref, +Selector, +Arg}
    \label{sec:xpce-send_list}
The utility predicate send_list/[2,3] is similar to send/[2,3], but
any of the arguments may be a list. For each member of the Cartesian
product a message is sent:

\begin{code}
send_list([Box,Circle], pen, 2).
\end{code}

is equivalent to

\begin{code}
send(Box, pen, 2),
send(Circle, pen, 2).
\end{code}

send_list/3 is often used to append menu items to a menu:

\begin{code}
send_list(Menu, append,
              [ menu_item(quit, message(@arg1, free))
              , menu_item(hide, message(@arg1, hide))
              ]).
\end{code}

    \predicate{get}{3}{+Object, +Message, -Answer}
    \label{sec:xpce-get}
Like send/2, but where send-behaviour acts as a boolean procedure,
get/3 acts as a function: it requests a value from the receiving
object.

In general, get-behaviour does not modify the object base, with the
possible exception of creating new instances that serve as the
answer to the query. Send-behaviour is intended to modify the
receiving object; nothing in the implementation enforces this, and
some get-behaviour has side-effects beyond creating the answer.

The answer of get-behaviour can be an attribute of an object (e.g.
graphical<-area returns the area object associated with the
graphical), or it can be a freshly created object (e.g.
graphical<-size returns a new size object reflecting the
size of the graphical). The documentation is the only source for
this detail.

Knowing this can be important. For example, modifying the size
object returned by graphical<-size is valid but does not
affect the graphical. Modifying the area returned by a graphical
however will leave the graphical in an incoherent state.

Under normal circumstances, there is no need to ->free the object
returned by a get-operation. If the object is not referred to, it
will automatically be discarded by the incremental garbage collector
once its scope of creation ends.

See also get_class/4, get_super/3, get_object/4 and get_chain/3.

    \predicate{get_class}{4}{+Object, +Class, +Message, -Answer}
    \label{sec:xpce-get_class}
Invoke get-methods inherited from \arg{Class} rather than the
implementation belonging to the class of \arg{Object}.

    \predicate{get_super}{3}{+Object, +Message, -Answer}
    \label{sec:xpce-get_super}
Like get/3, but uses the implementation of the super-class of the
class to which \arg{Object} belongs. Translated at compile time to
get_class/4.

    \predicate{get_chain}{3}{+Ref, +Selector, -List}
    \label{sec:xpce-get_chain}
Utility defined as

\begin{code}
get_chain(Ref, Selector, List) :-
    get(Ref, Selector, Chain),
    chain_list(Chain, List).
\end{code}

    \predicate{get_object}{4}{+Ref, +Selector, +Args, -Term}
    \label{sec:xpce-get_object}
The utility predicate get_object/[3-13] is similar to get/[3-13].
Where get/[3-13] normally returns an object reference,
get_object/[3-13] by default returns a term similar to object/2.
Used mostly for debugging from the interactive toplevel.

    \predicate{chain_list}{2}{+ChainRef, -List}
    \label{sec:xpce-chain_list}
Translate an XPCE chain into a Prolog list. This may be useful to
exploit Prolog's list-processing primitives. Note however that
XPCE chains define various operations that may be exploited to
avoid the translation. See chain->for_all,
chain<-find_all, etc.

    \predicate{free}{1}{+Ref}
    \label{sec:xpce-free}
Send ->free to \arg{Ref} if it is a valid reference. Defined as

\begin{code}
free(Ref) :- object(Ref), !, send(Ref, free).
free(_).
\end{code}

This definition implies free/1 only fails if the object may not be
freed (see object->protect).

    \predicate{object}{1}{+Reference}
    \predicate{object}{2}{+Reference, -Term}
    \label{sec:xpce-object}
The Prolog predicate object/1 is an extension of the Prolog
type-test predicates. It succeeds if \arg{Reference} refers to a
valid object reference -- a term of the form @/1 whose first
argument is an atom denoting the name of a global object, or an
integer referring to an existing object.

In all other cases this predicate fails silently. Note that, if the
term is of the form @<integer>, heuristics are used to determine
whether the indicated memory location refers to a valid object.

object/2 converts an object into a descriptive term. See the User
Guide for complete information.

See also \verb$@pce<-object_from_reference$ and
object->name_reference.

    \predicate{portray}{1}{+Ref}
    \label{sec:xpce-portray}
Like portray/2; prints on the terminal.

    \predicate{portray_object}{2}{+Ref, -Term}
    \label{sec:xpce-portray_object}
Like object/2, but heuristically expands arguments.

    \predicate{default}{3}{+Arg, +Default, -Value}
    \label{sec:xpce-default}
Handle default arguments in methods. default/3 is only used inside
a user-defined method declaration. It specifies the value of an
omitted (default) argument. If \arg{Arg} is not omitted its value
is returned in \arg{Value}; otherwise \arg{Value} is unified with
\arg{Default}.

\arg{Default} may take the form

\begin{code}
resource(Object, ResourceName)
\end{code}

in which case \arg{Value} will be bound to the result of
\verb$object<-resource_value: ResourceName$.

Note that the message-passing kernel passes omitted arguments as
\const{@default}. Example:

\begin{code}
image(R, I:[image]) :->
    "Set image of object"::
    default(I, image('pce.bm'), Image),
    ...
\end{code}

    \predicate{checkpce}{0}{}
    \label{sec:xpce-checkpce}
Validate the internal consistency of XPCE's database. It checks
for:

\begin{itemize}
    \item Consistency of the XPCE name (atom) table. Detected
    inconsistencies are reported as system errors. On successful
    completion statistics on the name-table are printed to the
    host-language window.
    \item Inconsistencies in the object base. It collects all
    objects with a global named reference in a chain and then
    runs object->_check on this chain. This finds all
    visual objects (via @display\_manager), all classes (via
    @classes) and all other globally defined objects as well as
    objects referred from them.
    \item \textit{Unresolved} types (see \verb$@pce<-unresolved_types$).
    Types with a Prolog pce_autoload/2 declaration are omitted.
\end{itemize}

The predicate succeeds if no errors were found and fails otherwise.
Error messages are written to the host-language window. See also
object->\_check.

    \predicate{pcerefer}{1}{+Ref}
    \label{sec:xpce-pcerefer}
Scan the XPCE object base for objects that have references to
\arg{Ref}. Most commonly used in combination with checkpce/0: if
checkpce/0 reports an object to be in an inconsistent state,
pcerefer/1 can analyse its context.

    \predicate{in_pce_thread}{1}{:Goal}
    \label{sec:xpce-in_pce_thread}
Post \arg{Goal} to the thread running XPCE. This supports XPCE
running in a foreground thread (handling the GUI and short
application-oriented actions) while one or more background threads
do computation and other background work. Background threads call
in_pce_thread(Goal) to insert a Prolog goal into the main thread's
event loop -- the goal is placed in the normal X11/Windows event
queue. in_pce_thread/1 returns immediately in the calling thread.

A typical setup has the background thread compute a result and
ask the GUI to present it. The result may be passed as an argument
to \arg{Goal} or held in dynamic predicates. Both threads run in
parallel, so update/read of shared dynamic data must be properly
coordinated. To wait for the GUI to complete the calling thread can
use a message queue:

\begin{code}
sync_call(Goal) :-
    thread_self(Me),
    in_pce_thread(make_sync_goal(Goal, Me)),
    thread_get_message(sync_goal(Result, Binding)),
    (   Result == true
    ->  Goal = Binding
    ;   Result == exception
    ->  throw(Binding)
    ).

make_sync_goal(Goal, Thread) :-
    (   catch(Goal, E, true)
    ->  (   var(E)
        ->  Msg = sync_goal(true, Goal)
        ;   Msg = sync_goal(exception, E)
        )
    ;   Msg = sync_goal(false, _)
    ),
    thread_send_message(Thread, Msg).
\end{code}

    \predicate{pce_open}{3}{+Object, +Mode, -Stream}
    \label{sec:xpce-pce_open}
Open an XPCE object as a Prolog stream so the normal Prolog I/O
predicates can read from or write to the object.

Works on any object that implements the \verb$*_as_file$ methods.
Currently those are the subclasses of class source\_sink:

\begin{itemize}
    \item source_sink<-read_as_file
    \item source_sink<-size_as_file
    \item source_sink->truncate_as_file
    \item source_sink->write_as_file
\end{itemize}

In addition, the following methods provide partial support:
char_array<-read_as_file,
char_array<-size_as_file and
stream->write_as_file.

The stream handle is discarded with Prolog's close/1 predicate.
For example, to write to a view:

\begin{code}
pce_open(View, append, Stream),
format(Stream, 'Hello World~n', []),
close(Stream),
\end{code}

See also text_buffer->format. Reading from a stream is used
by PceEmacs to verify the syntax of an entered clause.

    \predicate{pce_server}{1}{+Address}
    \label{sec:xpce-pce_server}
Create a socket-based server. This predicate is in
library(pce_server). It creates a server-socket object at the
specified address which other processes can connect to in order
to give Prolog commands to this XPCE image.

pce_server/1 implements a simple generic server. Its sources may
be used as a starting point to write dedicated servers. The Unix
command \verb$xpce-client$ may be used to connect to XPCE sockets.
Below is an example of using this library:

\begin{code}
% xpce
<banner>
?- pce_server(gnat).
yes

% on another terminal on the same machine
% xpce-client gnat
(pce) send(new(P, picture), open).
P = @364728
(pce) exit
%
\end{code}

The reserved command \const{exit} closes the server socket --
\textit{only} this connection.

    \predicate{pce_image_directory}{1}{+Spec}
    \label{sec:xpce-pce_image_directory}
Define an alternate directory containing images. Modifies the
\classvar{image}{path} class variable, adding the path \arg{Spec}
expanded through Prolog's path-specification mechanism to the
image search path. See also image->load,
image->initialise and image<-convert.

It also prepends \arg{Spec} to the file_search_path/2 specification
for \const{image}. Normally used as a directive.

    \predicate{pce_catch_errors}{2}{+Id, +Goal}
    \label{sec:xpce-pce_catch_errors}
Handle XPCE errors raised while \arg{Goal} is called. \arg{Id} is
either the id of an error object or a chain of such ids. If one of
the listed errors occurs the goal silently fails and
\verb$@pce<-last_error$ holds the id of the trapped error. Any
other error encountered while \arg{Goal} runs is handled by XPCE's
normal error-handling mechanism.

Exceptions are \textit{repairable} errors. Exception handlers may
be installed in \verb$@pce<-exception_handlers$.

    \predicate{pce_global}{2}{+Ref, +Pred}
    \label{sec:xpce-pce_global}
Declare a predicate that creates the object referred to as
\arg{Ref}. Part of XPCE's facility to create objects only when they
are actually needed: this keeps start-up time short for large
applications.

The first argument is the global reference being declared. The
second is either the name of a predicate capable of creating the
object, or a term wrapped in new/1 representing the object.

Examples:

\begin{code}
:- pce_global(@center,
              new(spatial(xref = x+w/2, yref = y+h/2,
                          xref = x+w/2, yref = y+h/2))).

:- pce_global(@pce_icon, make_pce_icon).

make_pce_icon(Ref) :-
    new(Ref, bitmap),
    send(Ref, load, 'pce.bm').
\end{code}

    \predicate{pce_autoload}{1}{+Class}
    \label{sec:xpce-pce_autoload}
Autoload \arg{Class} if it has been declared via the pce_autoload/2
directive. See also pce_autoload_all/0.

    \predicate{pce_autoload_all}{0}{}
    \label{sec:xpce-pce_autoload_all}
Autoload every class declared for autoloading. Normally used before
qsave_program/2 so the saved state includes those classes.

    \predicate{auto_call}{1}{+Goal}
    \label{sec:xpce-auto_call}
Make an \textit{autoload} call, even on Prolog systems that lack
this facility. Prolog systems with autoload define this using
call/1; others can use pce_require/1:

\begin{code}
auto_call(Goal) :-
    functor(Goal, Name, Arity),
    pce_require([Name/Arity]),
    call(Goal).
\end{code}

    \predicate{pce_require}{1}{+ListOfPreds}
    \label{sec:xpce-pce_require}
Declare that this module requires the predicates listed in
\arg{ListOfPreds} -- a list of \textit{Name}/\textit{Arity} pairs.
Together with auto_call/1 it is part of XPCE's portability layer.

Prolog systems with a library-predicate autoloader provide an empty
definition for this predicate. Other Prolog systems can search the
library index and call use_module/2 to import the predicates.

    \predicate{pce_begin_class}{2}{+Term, +Super}
    \predicate{pce_begin_class}{3}{+Term, +Super, +Doc}
    \label{sec:xpce-pce_begin_class}
Start declaration of a class. Together with pce_end_class/0 (or
pce_end_class/1) this directive brackets the definition of a
Prolog-defined class. A Prolog source module may contain any number
of these pairs, defining any number of XPCE classes.

\arg{Term} defines both the class name and the term structure when
an instance of the class is transformed into a Prolog term. The
term has the form

\begin{code}
+Class(...+Selector...)
\end{code}

where \arg{Class} denotes the class name and the optional list of
selectors gives the (argument-free) get-behaviours used to obtain
the arguments of the term representation. See
\verb$Object->Term$.

The description term may be prefixed with the meta-class to be
used. For example, the following declaration ensures the meta-class
is \verb$my_meta_class$:

\begin{code}
my_meta_class:myobject
\end{code}

\arg{Super} is the name of the super-class. It must be the name of
a defined XPCE class or a class declared via pce_autoload/2.

\arg{Doc} is an approximately 40-character summary documentation
string visible in the manual tool.

    \predicate{pce_extend_class}{1}{+ClassName}
    \label{sec:xpce-pce_extend_class}
Prepare for the declaration of methods on an already existing XPCE
class. Be careful when extending built-in classes: it is easy to
corrupt the internal consistency of the extended class or one of
its subclasses. The class extension is terminated with
pce_end_class/0.

    \predicate{pce_end_class}{0}{}
    \predicate{pce_end_class}{1}{+Class}
    \label{sec:xpce-pce_end_class}
End declaration of a class. pce_end_class/1 finishes the definition
of a class opened with pce_begin_class/[2,3] or pce_extend_class/1
in the same way as pce_end_class/0, but provides additional
documentation and error checking and is recommended over
pce_end_class/0.

    \predicate{pce_class_directive}{1}{+Goal}
    \label{sec:xpce-pce_class_directive}
Execute a directive on the constructed class. Used between
pce_begin_class/[2,3] and pce_end_class/0 to invoke additional
behaviour on the defined class.

XPCE classes are not created immediately when a Prolog file
holding a class definition is loaded. Instead, the specification
is translated into Prolog facts; if a reference is later made to
the class, the XPCE class counterpart is created. This means it is
not possible to invoke behaviour on the class being defined using
ordinary Prolog directives.

A goal declared with pce_class_directive/1 is stored with the
Prolog class definition. When the class object is actually created
the declared class-directives are executed. During the execution
\const{@class} is a var object referring to the class being built.

The preprocessor automatically tags directives of the form
\verb$:- send(@class, ...)$ with this directive.

    \predicate{pce_group}{1}{+GroupName}
    \label{sec:xpce-pce_group}
Start a functional group of methods. Used between
pce_begin_class/[2,3] and pce_end_class/0 while defining an XPCE
class.

Method declarations following this directive get the specified
group identifier, which gives a better overview of the class
through the manual tools (ClassBrowser).

The value \const{@default} (pushed by pce_begin_class/[2,3])
assigns a method to the same group as an inherited method of the
same name, or the group \const{miscellaneous} if no such method
exists. The directive has no semantic implications.

See also pce_begin_class/[2,3], pce_extend_class/1 and
method<-group.

    \predicate{class_variable}{3}{+Name, +Type, +Default}
    \predicate{class_variable}{4}{+Name, +Type, +Default, +Doc}
    \label{sec:xpce-class_variable}
Declare a class-variable. The term class_variable/[3,4] is expanded
by the XPCE class compiler and associates a class_variable object
with the current class.

This declaration supersedes resource/4. Unlike with resource/4,
\arg{Default} is a term that is translated as the arguments to
send/[2-12].

    \predicate{resource}{3}{:Name, +Class, +FileSpec}
    \label{sec:xpce-resource}
Define data for a resource. Used as a directive while declaring
a class.

    \predicate{variable}{3}{+Name, +Type, +Access}
    \predicate{variable}{4}{+Name, +Type, +Access, +Doc}
    \label{sec:xpce-variable}
Declare an instance variable. Normally placed immediately after the
pce_begin_class/[2,3] call to declare the additional slots
(attributes) of the class. The arguments are:

\begin{itemize}
    \item \arg{Name} -- Name of the instance variable. Must be
    unique for this class (it cannot already be used in the
    super-class).
    \item \arg{Type} -- Type specification of permitted values.
    See \textit{Types}.
    \item \arg{Access} -- Access for the \verb$send$ and \verb$get$
    operations. Valid values are:
    \begin{itemize}
        \item \const{none} -- no access (only via
            \verb$Object<->slot$).
        \item \const{get} -- may be fetched with get/3; can be set
            only via \verb$Object->slot$.
        \item \const{send} -- may be set with send/3; can be
            fetched only via \verb$Object<-slot$ (rarely useful).
        \item \const{both} -- may be set with send/3 and fetched
            with get/3.
    \end{itemize}
    \item \arg{Doc} -- optional summary documentation of the
    variable, visible in the manual tool.
\end{itemize}

See also class->instance_variable.

    \predicate{handle}{4}{+X, +Y, +Kind, +Name}
    \label{sec:xpce-handle}
Declare a handle for this graphical. Attaches a handle object at
the class level when between pce_begin_class/[2,3] and
pce_end_class/0. See also class->handle.

    \predicate{delegate_to}{1}{+Variable}
    \label{sec:xpce-delegate_to}
Add the indicated instance variable to the list of delegate
variables of the class (see class<->delegate). Expanded by
the XPCE/Prolog class compiler.

    \predicate{use_class_template}{1}{+TemplateClass}
    \label{sec:xpce-use_class_template}
Import methods from \arg{TemplateClass}. Provides a simple
mechanism for sharing method and variable definitions across
multiple classes.

Suppose an application requires specialisations of several
primitive graphical classes. Three ways are available: use
pce_expand_class/1 on one of the super-classes (unsafe, as the
methods become available to all users), make subclasses of every
class needing the extension and duplicate the source (bad: hurts
maintenance), or use a class template.

To use a template: first create a subclass of class \const{template}
and define all variables and methods that need to be attached to
the target class. Then write:

\begin{code}
:- pce_begin_class(mybox, box).
:- use_class_template(my_graphical_extensions).

:- pce_end_class.
\end{code}

This behaves exactly as if all text defining
\verb$my_graphical_extensions$ were duplicated at the
use_class_template/1 call. The method implementation (the Prolog
predicate) is shared between the classes that use the template.

Note that you can normally \textit{not} create instances of the
template class itself since it may lack methods that need to be
refined in the target classes. Note also that you \textit{cannot}
further refine a method imported from a template in the
\textit{same} class -- the import behaves like text duplication, so
a new definition simply overwrites the one from the template. You
\textit{can} refine the method in a subclass.

    \predicate{editpce}{1}{+What}
    \label{sec:xpce-editpce}
Start PceEmacs on a method or class. Works only for classes and
methods defined in Prolog, unless run from a system with the XPCE
C-sources installed. Syntax:

\begin{center}
\begin{tabular}{ll}
\verb$classname$ & Edit the class \\
class->send_method & Edit a send-method \\
class<-get_method & Edit a get-method \\
\verb$class-variable$ & Edit the class that defines the variable \\
\end{tabular}
\end{center}

For \verb$class-variable$, if the variable is inherited from the
specified class the editor opens on the class the variable is
inherited from.

    \predicate{tracepce}{1}{+Behaviour}
    \label{sec:xpce-tracepce}
Trace the indicated behaviour object. Puts a trace-point on the
behaviour so the system prints entries and completion of that
behaviour. \arg{Behaviour} is specified as
class->selector or class<-selector. Example:

\begin{code}
?- tracepce(box->event).
Trace method: graphical ->event
\end{code}

See also spypce/1 and breakpce/1.

    \predicate{notracepce}{1}{+Behaviour}
    \label{sec:xpce-notracepce}
Remove an XPCE trace-point. See tracepce/1 for details.

    \predicate{breakpce}{1}{+Behaviour}
    \label{sec:xpce-breakpce}
Put an XPCE break-point on a method or variable. Functionality is
like tracepce/1, but the system breaks into the interactive tracer
when the behaviour is called. See also program_object->break,
spypce/1 and nobreakpce/1.

    \predicate{nobreakpce}{1}{+Behaviour}
    \label{sec:xpce-nobreakpce}
Remove an XPCE break-point. See spypce/1 for details.

    \predicate{spypce}{1}{+Behaviour}
    \label{sec:xpce-spypce}
Put a Prolog spy-point on the implementation of an XPCE behaviour.
\arg{Behaviour} is of the form class->send_method or
class<-get_method. The predicate locates the method,
determines the Prolog predicate implementing it and calls spy/1 to
put a Prolog spy-point on that predicate.

Can only be used on methods defined using the Prolog class
definition mechanism realised with pce_begin_class/[2,3] and
pce_end_class/0. See also tracepce/1 and breakpce/1.
\end{description}
