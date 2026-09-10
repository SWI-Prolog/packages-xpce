\chapter{Panes: the windows of the IDE}		\label{sec:panes}

The tools of the SWI-Prolog development environment --- the Epilog
console, PceEmacs, the source navigator, the thread monitor, the
debugger --- do not each open a window of their own. They are
\jargon{panes}, and one window holds as many of them as you care to put
in it: side by side, above each other, or in tabs. This chapter
describes how such a window is put together, what you can do to it, how
the system decides where a new pane goes and which class variables
(\chapref{classvar}) change the defaults.

None of it is built into the kernel. \pllib{tab_frame} defines a tab
that tiles a set of windows, \pllib{pane_frame} the window that holds
such tabs, and \pllib{pane_layouts} decides where a new pane belongs.
An application of your own can use all three; only \secref{paneplacement}
and \secref{panearrange} are about the Prolog IDE in particular.


\section{Anatomy of a window}			\label{sec:paneanatomy}

A window of the IDE is an instance of class \class{pane_frame}. It holds
three things:

\begin{itemize}
    \item A \jargon{menu bar} (class \class{pane_menu_bar}) along the
top.
    \item A strip of \jargon{tabs} below it. Each tab is an instance of
class \class{pane_tab}, a subclass of \class{tab_frame}. Where an
ordinary tab holds exactly one window, a \class{tab_frame} keeps a whole
\class{tile} hierarchy alive, the way class \class{frame} does for its
members: the windows in it are laid out left/right and above/below and
the gaps between them can be dragged. A window showing a single tab
shows no tab label at all.
    \item Optionally a \jargon{status bar} at the bottom (class
\class{pane_status_dialog}), which reports messages and, if you ask it
to, prompts for values.
\end{itemize}

The windows tiled by a tab are the panes. Any window can be one: none of
the pane protocol is compulsory, as \class{pane_frame} asks with
->has_send_method before it sends. What a pane would otherwise have to
repeat --- reaching its frame and its tab, splitting, opening a new tab
or window, closing --- is in the \class{pane} template:

\begin{code}
:- pce_begin_class(my_pane, window, "A pane of my own").
:- use_class_template(pane).
\end{code}

A pane need not be a single window. Class \class{pane_stack} holds its
windows in tabs of its own: \class{tool_pane} uses one such tab to tile
the windows of a tool (the navigator is a tree and a dialog), and
\class{emacs_pane} uses one tab per source it shows. Such a pane is a
\jargon{group}: `pane_frame <-current_pane' looks inside it for the
window you are really working in and `pane_frame <-pane_group' is the way
back out to the pane the tab tiles.

The menu bar is assembled from two sides. The application fills in the
menus that every pane of the window shares (\const{File},
\const{Settings}, \const{Tools}, \ldots) and the pane in view adds its
own; whenever the pane in view changes, the bar is rebuilt. Neither side
can know what the other will put on, so the bar is not built in the order
the menus arrive: `pane_menu_bar <-menu_order' names the menus in the
order they belong and ->append works out from the name which menu the new
one has to come in front of. A menu whose name is not in the list takes
the place of \verb$*$, which is where a mode's own menu
(\const{Prolog}, \const{SGML}, \ldots) or a tool's ends up.


\section{Rearranging the panes}			\label{sec:panerearrange}

Every pane carries a small \jargon{grip} (class \class{split_handle}) in
its upper-right corner. It is the one thing a pane of any class is sure
to have, and everything you can do to a pane by hand starts there.

\begin{itemize}
    \item \textbf{Drag the grip onto another pane} to put this pane
beside that one. Which of the four sides it lands on is decided by the
edge of the target the pointer is nearest, and the half of the target
that the drop would take is outlined while you drag. On a window system
that does not tell a window where it is on the display --- Wayland ---
dragging across windows cannot work, and the grip picks the pane up on a
click instead: click it, then click where the pane is to go. The balloon
on the grip says which of the two applies.

    \item \textbf{Drag the grip onto the label of a tab} to move the pane
into that tab.

    \item \textbf{Click the grip with the right button} for a menu that
offers, where each applies: move to a window of its own, move to a tab of
its own, move into the tab before or after this one, and close.

    \item \textbf{Drag a gap} between two panes to redistribute the room.
A line is drawn in every gap that can be dragged. A layout you set by
hand is kept when the window itself is resized: the panes keep their
share rather than being put back where the system had them.

    \item \textbf{Double-click a tab label} to rename that tab, drag it to
reorder it, click the button on it to close it. Its right-button menu
also offers to close the other tabs and to move the tab to a window of
its own.
\end{itemize}

When a pane is dropped on another, the target is divided into a left, a
right, a top and a bottom \jargon{zone}, and the pointer lands in one of
them. The top and bottom zones are weighted by
`tab_frame <-split_bias': above one they are narrower and the left and
right zones wider, on the grounds that a left/right split is the commoner
one.

From a program the same moves are `pane_frame ->append_pane' (a tab of
its own), `pane_frame ->split' (beside a given pane) and
`pane_frame ->split_beside' (beside a set of panes, taking a share of
their room).


\section{Where a new pane goes}			\label{sec:paneplacement}

When you ask for a tool from the \const{Tools} menu, ask to see a source
with edit/1 or by clicking a location in a tool, or open a new terminal,
the IDE has to find a place for it. The class variable
`prolog_ide <-tool_placement' decides. It is also in the menu, under
\menu{Settings/New tools and sources open}{}, so you can try the four
answers out before writing one down:

\begin{description}
    \item[\const{as_arranged}]
The default. Read how you have arranged windows holding such panes
before, and put the new pane where you would have put it yourself. See
\secref{panearrange}. When nothing that has been learned has anything to
say about this pane, it falls back on \const{tab}.
    \item[\const{frame}]
Always in a window of its own.
    \item[\const{tab}]
Always in a tab of the window you are working in.
    \item[\const{split}]
Always beside the pane you are working in.
\end{description}

With \const{split} the side comes from the tool itself: a tool says where
it belongs by declaring a \const{pane_side} class variable of its own.
The navigator declares \const{left}; a tool that says nothing goes
\const{below}. The same answer is used when a pane is moved into the tab
beside it. Being a class variable, you can overrule it:

\begin{code}
prolog_navigator.pane_side:		left
prolog_thread_monitor.pane_side:	right
\end{code}


\section{Learning how you arrange your windows}	\label{sec:panearrange}

One setting cannot say much. Whether a new pane wants a window, a tab or
a split is only the first of the questions; if it is a split, then beside
\emph{which} of the panes already there, on which edge of them and taking
how much of their room? \pllib{pane_layouts} answers all four by
watching how you arrange your own windows.

An \jargon{arrangement} is a window written down with the content left
out: which kinds of pane are in it, how they are tiled, and their share
of the room. This is one:

\begin{code}
pane_frame([], [tab([], horizontal([ 0.2-prolog_navigator,
                                     0.8-vertical([ 0.7-editor,
                                                    0.3-terminal ]) ]))])
\end{code}

Not every window teaches something. A window the IDE placed and you never
touched teaches nothing --- the system would only be learning back its own
guesses --- so a window says nothing until the first time you move, split,
resize, re-tab or close a pane in it by hand. From then on what is
counted is the time an arrangement is \emph{lived in}. Putting a window
right takes several steps, and only the state you then work in means
anything; each step closes off the one before it, and a state that lasted
less than ten seconds is thrown away rather than credited.

What an arrangement has earned decays with a half-life of thirty days, so
an arrangement you made once and never went back to fades on its own and
a habit that changes re-ranks itself. A priority is thus ``seconds of
recent use'', and the arrangement that answers for a new pane is the one
that shares most of what the window will hold, weighted by that.

\subsection{The store}				\label{sec:panestore}

What has been learned lives in the file \file{xpce/pane_layouts} of your
application configuration directory, beside the \file{Defaults} file
described in \chapref{classvar}. It is a log rather than a snapshot: each
record credits an arrangement with the seconds it had earned at the
moment the record was written, and reading the file plays the log back.
Nothing waits for the end of the session, so several instances of the IDE
running at once each add what they learn, and a crash costs at most the
window that was on the screen. A log that has grown past two hundred
records is rewritten as one record per arrangement, which says the same
thing.

The file is plain Prolog with a header comment, and it may be edited by
hand. A record that means nothing to the system --- one written by a
later version, or edited into something else --- costs itself and no
more.

\menu{Settings/Forget how I arranged windows}{} throws the lot away;
forget_arrangements/0 does the same from a program.

\subsection{Saying it yourself}			\label{sec:panedefarrange}

The arrangements the system comes with are ordinary clauses of the
multifile predicate default_arrangement/1 in module \const{pane_layouts},
credited with a nominal five minutes so that any arrangement you have
really worked in outranks them. Your initialisation file, or a project, can add its own:

\begin{code}
:- multifile pane_layouts:default_arrangement/1.

pane_layouts:default_arrangement(
    pane_frame([], [tab([], horizontal([ 0.25-prolog_navigator,
                                        0.75-editor ]))])).
\end{code}

This says three things at once: that a navigator belongs beside an
editor, that it goes on the left, and that it takes a quarter of the
width. Ask for a navigator in a window showing an editor and that is
what you get.

The store itself can be moved with the multifile hook
arrangements_file/1, whose first clause wins. A project that wants
arrangements of its own, or a test that must not touch yours, says so:

\begin{code}
:- multifile pane_layouts:arrangements_file/1.
pane_layouts:arrangements_file('/path/of/my/project/layouts').
\end{code}

pane_placement/3 is the question the IDE asks. Given the kind of pane to
add and the kinds the window holds now it answers \const{window},
\const{tab} or \const{split(Kinds, Side, Share)}. It answers in kinds
rather than in panes, so it is plain Prolog that can be tested without a
window on the screen.


\section{Writing a window down}			\label{sec:paneterm}

A whole window --- its tabs, the panes in them, how they are tiled, their
share of the room and what each pane is showing --- can be read as a
Prolog term and built back from one. `pane_frame <-pane_term' writes it
and open_pane_frame/2 makes a window out of it:

\begin{code}
?- Term = pane_frame([geometry('1200x800+40+40')],
                     [ tab([current(true)],
                           vertical([ 0.7-current(editor([file('foo.pl'),
                                                          line(120)])),
                                      0.3-terminal([profile(shell)]) ]))
                     ]),
   open_pane_frame(Term, _Frame).
\end{code}

Everything in the term is optional, so one can be written by hand. Each
pane answers for its own part of it: an editor says which sources it
shows, a terminal says what it runs. \pllib{pane_layouts} strips the term
to the kinds of pane and their shares, which is what an arrangement is.


\section{Class variables}			\label{sec:panedefaults}

All of the below are class variables, set in the \file{Defaults} file as
described in \chapref{classvar}. The file \file{Defaults.user} that
ships with \product{} carries all of them as commented examples;
\menu{Settings/GUI preferences}{} opens your own copy of it.

\begin{description}
    \classvarmethod{prolog_ide}{tool_placement}{\{as_arranged,frame,tab,split\}}
Where a tool, a source or a terminal you ask for is put. See
\secref{paneplacement}. Default is \const{as_arranged}.
    \classvarmethod{pane_stack}{pane_side}{\{above,below,left,right\}}
Which side of what is already there a tool is added on when it is split
in. Declared again by the tools that do not want the default,
\const{below}; the navigator declares \const{left}.

    \classvarmethod{pane_frame}{label_format}{name*}
The title of the window, in which \verb$%s$ stands for the label of the
pane in view. \const{@nil} leaves the title to the window manager.
Default is \verb$'SWI-Prolog -- %s'$. A pane can overrule it for the
window it is in.
    \classvarmethod{pane_frame}{focus_on_enter}{bool}
When \const{@on}, a pane gets the keyboard focus as soon as the pointer
enters it rather than when it is clicked. Default is \const{@off}.
    \classvarmethod{pane_frame}{inactive_opacity}{num}
Opacity of the panes that have not got the keyboard focus, which is a way
to see at a glance which one the keyboard talks to. Default is
\const{1.0}, which fades nothing. A pane that declares an
\const{inactive_opacity} of its own overrules this for itself.
    \classvarmethod{pane_frame}{prompt_style}{\{status_bar,dialog\}}
Where the window asks for a value: all of them in a dialog
(\const{dialog}, the default) or one at a time on the status bar.

    \classvarmethod{pane_menu_bar}{menu_order}{chain}
The order the pulldown menus appear in; see \secref{paneanatomy}. A menu
whose name is not in the chain takes the place of \verb$*$. Note that
\const{GUI} and \verb$*$ have to be quoted:

\begin{code}
pane_menu_bar.menu_order:	[ file, settings, tools, debug, 'GUI',	  \
				  edit, browse, compile, '*', help	  \
				]
\end{code}

    \classvarmethod{pane_popup}{accelerator_font}{font}
Font the accelerator of a menu entry is written in. Default is
\const{small}.

    \classvarmethod{pane_tab}{editable_label}{bool}
Whether a tab can be renamed by double-clicking its label. Default is
\const{@on}.
    \classvarmethod{pane_tab}{closable}{bool}
Whether a tab carries a button to close it. Default is \const{@on}.
    \classvarmethod{tab}{edit_width}{int}
Room made to type a new tab label in, if the tab strip has it. Default is
\const{200}.

    \classvarmethod{tab_frame}{separator_colour}{colour*}
Colour of the lines drawn in the gaps that can be dragged. Default is
\const{@nil}, which draws them in the foreground colour.
    \classvarmethod{tab_frame}{tile_border_root}{int}
Margin between the tiled panes and the edges of the tab. Default is
\const{0}: out to the edges.
    \classvarmethod{tab_frame}{split_bias}{num}
Weight of the top and bottom zones when a pane is dropped on another; see
\secref{panerearrange}. Above one the left and right zones are wider.
Default is \const{2}.
    \classvarmethod{tab_frame}{horizontal_resize_cursor}{cursor}%
    \classvarmethod*{tab_frame}{vertical_resize_cursor}{cursor}
Cursors shown over a gap that can be dragged. Defaults are
\const{ew_resize} and \const{ns_resize}.

    \classvarmethod{pane_handle}{handle_size}{size}
Size the picture on the grip is drawn at. Default is
\const{size(16,16)}.
    \classvarmethod{split_handle}{grip_image}{name}
The picture on the grip. Default is \const{'tool/drag-pane.svg'}.
    \classvarmethod{split_handle_gesture}{cursor}{[cursor]}
Cursor while a pane is dragged. Default is \const{@default}: a picture of
the pane itself.
    \classvarmethod{split_handle_gesture}{cursor_size}{size}
Largest that picture is drawn. Default is \const{size(96,96)}.
    \classvarmethod{split_handle_gesture}{cursor_border}{[colour]*}
Border around it. \const{@default} (the default) draws it in the
foreground colour, \const{@nil} draws none.
\end{description}
