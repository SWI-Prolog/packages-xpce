\section{Showing help-balloons}			\label{sec:balloon}

\index{balloon}%
The library \pllib{help_message} provides support for displaying
\jargon{balloons}.  After loading this library, a \const{loc_still}
event causes the system to look for a \class{graphical} implementing
the <-help_message method.  If <-help_message: tag yields a string,
the library shows a little window with the message.  Moving the pointer
out of the area of the graphical or pressing a button causes the
feedback window to disappear.

This technique is commonly used in modern interfaces to provide
feedback on the functions behind icons.  See also \secref{toolbar}.

In addition to registering the global event-handler, the library
defines ->help_message and <-help_message on the classes \class{visual}
and \class{dict_item}.  The classes \class{menu} and
\class{list_browser} redefine <-help_message to return the message
associated with the \class{menu_item} or \class{dict_item} below the
pointer.  If a graphical has no help message, the message of the
graphical it is contained in is used.

\begin{description}
    \sendmethod{visual}{help_message}{\{tag,summary\}, string*}
Register \arg{string} as \const{tag} (balloon) or extensive help
message (\const{summary}) for the receiving object.  The
\const{summary} message is displayed if the user presses the
\const{help} function key.  Using @nil deletes the message.

    \getmethod{visual}{help_message}{\{tag,summary\}, event}{string}
This message is defined to return the help-message registered using
->help_message.  User-defined classes may consider redefining this
method to generate the help-message on-the-fly.

    \sendmethod{visual}{tooltip}{string*}
    \getmethod{visual}{tooltip}{}{string}
Shorthand for ->help_message and <-help_message with \const{tag}.
These are also defined on \class{dict_item}.
\end{description}

A message may be an instance of \class{markup}, a string holding Pango
markup, which is rendered in the balloon:

\begin{pcecode}
	send(B, tooltip, markup('Print the <b>whole</b> document')),
\end{pcecode}

Here is a typical usage for this library.

\begin{pcecode}
:- use_module(library(help_message)).
resource(print, image, image('tool/print.svg')).

	...
	send(X, append, new(B, button(print))),
	send(B, label, image(resource(print))),
	send(B, tooltip, 'Print document'),
\end{pcecode}

% tja ...
