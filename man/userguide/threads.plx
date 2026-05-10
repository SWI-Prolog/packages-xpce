\chapter{Using Prolog threads}			\label{sec:thread}

\index{thread}%
\product{}~6 and earlier used a single global lock to prevent access
from multiple Prolog threads.  \product{} itself by default ran in a
thread named \const{pce}.  As of version~7, this changed substantially

\begin{itemize}
    \item The \href{https://www.libsdl.org}{SDL} library restricts many
    operations to the process \jargon{main} thread. The exact
    restrictions depend on the platform.  To avoid problems, we run
    \product{} in the main thread.

    \item When SWI-Prolog reads from the terminal in the main thread,
    the system processes SDL events.   This implies that \product{}
    GUI components are responsive when SWI-Prolog waits for the user
    to enter queries.  \product{} \emph{is not responsive} while Prolog
    executes a query in this scenario.

    \item \product{} provides \textit{Epilog}, a \productpl{} class that
    provides a console (terminal) attached to a Prolog \jargon{thread}.
    Epilog is used by \program{swipl-win}, which creates an Epilog
    console providing the Prolog REPL loop based on a thread while
    the main thread processes SDL events while the Epilog console is
    active.   Multiple Epilog consoles may be opened simultaneously and
    an Epilog console can be split to accomodate multiple REPL loops,
    each in their own thread.
\end{itemize}

\product{} uses a single lock, preventing multiple threads from
accessing it concurrently. Where \product{}~6 applied this strict,
\product{}~7 releases the lock while \product{} calls out to Prolog.
This design is similar to the Python \jargon{GIL}.  Multiple Prolog
threads may interact with \product{}, but only one may actively run
\product{} code at any point in time.

\product{} operations that manage windows are handled by SDL and
most of them must run in the main thread.  Several \product{}
methods solve this problem by running the SDL steps in the main
thread while allowing non-SDL functionality to proceed in the
calling thread.  An example is `window->open`.  Code that manages
non-UI related objects can always operate from any thread.

Code that manages \jargon{graphical} objects displayed on a window such
as boxes, lines, text, etc., can be called from any thread. To
understand the consequences, we first present the overall control
loop of the main thread.

\begin{itemize}
  \item while(true)
  \begin{itemize}
    \item If the are pending changes to graphicals, for each window
    that has modified content do
    \begin{itemize}
      \item Use `graphical->compute` to update sizes and layout.
      While in progress, the \product{} lock is \emph{strict}, i.e., no
      Prolog thread is allowed to initiate methods on \product{}.
      \item Use	Cairo and Pango to update the Cairo \jargon{surface}
      acting as backing store for the Window.
      \item Use SDL to render the Cairo surface to the SDL window.
    \end{itemize}
    \item Wait for max 250ms for an SDL event
    \item Process the event
  \end{itemize}
\end{itemize}

The \textit{Process the event} step invokes methods on \product{} that
may in turn call Prolog.  The processing may create and destroy window
as well as make arbitrary many changes to the contents of windows. These
changes only become visible after processing the event completes.

If a thread other than the main thread modifies graphical objects, these
changes will be picked up by the main thread on an event or after the
250ms timeout elapses.  This can lead to multiple screen updates for a
single batch of modifications as well as delaying updates for up to
250ms.

It is typically good practice to run \product{} code that affects the
GUI in the main thread.  This can be accomplished using the predicates
below.  These predicates pose only a small overhead in case the calling
thread is the \product{} main thread.

\begin{description}
    \predicate[det]{in_pce_thread}{1}{:Goal}
Execute \arg{Goal} in the main (\product{}) thread.  This call always
succeeds immediately, pushing \arg{Goal} onto the SDL event loop.

    \predicate[semidet]{in_pce_thread_sync}{1}{:Goal}
Execute \arg{Goal} in the main (\product{}) thread. This pushes
\arg{Goal} onto the SDL event loop and waits until the event is
processed. Possible bindings resulting from executing \arg{Goal} in the
\product{} thread are propagated to this predicate. \arg{Goal} is
executed as once/1. If \arg{Goal} raises an exception, this is
propagated.
\end{description}
