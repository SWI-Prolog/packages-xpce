\section{Drag and drop interface}		\label{sec:dragdrop}

\product{}'s drag-and-drop interface allows the user to drag-and-drop
\class{graphical} objects and \class{dict_item} objects. Drag-and-drop
is a common GUI technique to specify operations that require two objects
in a specific order. Moving files to another directory by dragging them
to the icon of the target directory is a common example.

It may also be used to specify operations on a single object, where the
operation is represented by an icon. Dragging files to a trash-bin is the
most commonly found example.

For the drag-and-drop interface to work, the programmer must connect
a \class{drag_and_drop_gesture} to the object to be dragged.%
    \footnote{Attach a \class{drag_and_drop_dict_item_gesture} from
	      the library \pllib{dragdict} to a \class{list_browser} to
	      enable dragging the items in the dictionary}.
A {\em Drop-zone}  defines the method ->drop and (not strictly
obligatory) ->preview_drop.  ->drop is called to actually perform the
associated operation, while ->preview_drop may be used to indicate
what will happen if the object is dropped now.

\input{dragdropdemo.tex}

\subsection{Related methods}

\begin{description}
    \sendmethod{drag_and_drop_gesture}{initialise}{Button, Modifier,
						   Warp, GetSource}
Initialises a new \class{drag_and_drop_gesture}.  {\em Button} is
the name of the pointer-button the gesture should be connected to
(left, middle or right).  {\em Modifier} is a \class{modifier}
description (see class \class{modifier}).  While dragging, the
pointer shows the \const{move} cursor (see the class-variable
\const{cursor} of \class{drag_and_drop_gesture}).  If this
class-variable is @default, a cursor is created from the image of the
dragged graphical.  In that case {\em Warp} (default @on) determines
whether the pointer is placed in the centre of this cursor.  Dragging
starts after the pointer has moved a few pixels with the button
depressed.
{\em GetSource} is a function used to fetch the object dragged from the
graphical representing it.  Suppose the graphical to which the gesture
is attached represents a database record.  In this case it is much more
natural to pass the identifier for the database record to the
->drop and ->preview_drop methods than to pass the icon representing
it.  {\em GetSource} is a function that is evaluated with @arg1 bound
to the graphical when the gesture is activated.  An example could be:

\begin{code}
drag_and_drop_gesture(left,
		      get_source :=
			     @arg1?db_record)
\end{code}
    \sendmethod{graphical}{drop}{Object:<Type> [, Pos:point]}
This method may be defined on any graphical object that is a drop-zone.
It will only be activated if the drag_and_drop_gesture can locate
the method and make the necessary type transformations.  Thus, if the
type is specified as {\tt file}, this method will only be activated
if the dragged object can be converted to a \class{file} object.  See
also the discussion about the get_source argument above.

If the method accepts a point for the second argument, a point
will be passed that represents the location of the pointer in the
coordinate system of the drop-zone, subtracted by the distance between
the top-left corner of the dragged graphical to the pointer at the
moment the button was depressed.  To get the position of the pointer
itself, just ask for the position of @event relative to the drop-zone.
    \sendmethod{graphical}{preview_drop}{Object:<Type>* [, Pos:[point]]}
Sent by the \class{drag_and_drop_gesture} to allow the drop-zone 
providing feedback.  The arguments and semantics are the same
as for ->drop, but the first argument can be @nil, indicating that the
mouse has left the drop-zone.  Under this condition, the position
argument is @default.

If a position argument is available, the \class{drag_and_drop_gesture}
will be activated on each {\tt drag} event.  Otherwise it is only
activated if the pointer enters the area of the drop-zone.
\end{description}

\subsection{Dropping files from other applications}

\index{drag-and-drop,files}%
The interface above handles drag-and-drop between objects of the same
\product{} process.  Files and text dragged from other applications,
such as the file manager of the desktop, are delivered as events.  A
drop sequence starts with a \const{drop_begin} event, followed by
\const{drop_position} events while the user moves the pointer over the
window, a \const{drop_file} or \const{drop_text} event for each dropped
item and finally a \const{drop_complete} event.  The dropped file or
text is available as `event <-path' or `event <-text', which the event
delegates to its \class{drop_data} <-detail (see
\secref{inputdevices}).  The library \pllib{pce_drop_target} provides
\predref{drop_target_event}{4} to handle this sequence from an ->event
method.  It shows a hint while the user drags over the target and calls
a predicate with the list of dropped files on completion.  See also the
demo \metafile{demo(dragdrop_file)}.

