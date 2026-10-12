\chapter{\plainproduct{} status and plans}		\label{sec:xpce-status}

\product{} has been used by many projects and has been sold as a
commercial product. In addition it has been used to implement the
SWI-Prolog development tools, the PceEmacs editor, a source level
debugger, profiler front end and many more tools. Over the years,
interest for using \product{} for building GUIs faded away because the
system was barely maintained and claimed as \jargon{deprecated}.

\product{}~7 is a major rewrite of the lower layers of \product{}. It
makes \product{} future proof by building on widely supported portable
libraries rather than only providing bindings for X11 and Windows.
\product{}~8 and~9 modernised the appearance of the system and its
development tools.  Some major changes compared to \product{}~6 are:

\begin{itemize}
    \item Fonts, colours, cursors and images have been updated,
    removing many of the old legacy X11 features.  Fonts are managed by
    Pango.  Colours are RGBA values and no longer accept images
    (pixmaps).

    \item Colours may follow a \jargon{theme}.  The themes switch at
    runtime and, by default, follow the light or dark appearance of the
    desktop.  The \const{sys_*} colours provide the colours of the
    desktop.

    \item The dialog items have a flat, themed look and dialogs, menus
    and popups can be operated from the keyboard.

    \item Images are loaded using SDL_image, which supports the common
    image formats as well as SVG.  Most bundled images are SVG or PNG
    files.  `image->save' only saves in PNG format.  While images
    represent pixels using the 32-bit RGBA model, images can still be
    assigned the type \const{bitmap}.  Such images should only have
    black, white and transparent pixels.  Black pixels are rendered
    using the current foreground, white pixels using the current
    background and transparent pixels have no effect.

    \item While \product{}~6 supported primarily integers, \product{}~7
    generalizes non-object numbers to floating point numbers.  In many,
    but not all, classes floats are properly passed to the Cairo library
    which expresses all coordinates as doubles.  The current system
    supports a type \const{num} to represent such numbers.  Currently,
    \const{int} is an alias for \const{num}.  Passing floats to methods
    that require a strict integer causes silent rounding to an integer.
    Most methods and libraries that used to accept a \class{real} now
    use \const{num}.

    \item Host file dialogs are supported through SDL using the
    `frame<-open_file' and `frame<-save_file' methods.   The native
    xpce \jargon{finder} still works, but does not use the host
    functionality.

    \item The `->fill' method used to fill graphicals accepts a solid
    colour, a \class{gradient} or an \class{image}, which is repeated
    to fill the area.

    \item Where \product{}~6 provided access to multiple X11 displays,
    each of which could be divided into one or more monitors,
    \product{}~7 removed class \const{monitor}.  The system exposes
    one or more \class{display} instance as members of
    \class{display_manager}.  Unlike \product{}~6, \product{}~7 deals
    with hot plugging, removing and modifying the display configuration.
    @display refers to the current display.

    \item Generating PostScript from graphicals has been removed. The
    methods `graphical->pdf' and `window->pdf' use Cairo to write PDF to
    a file. The implementation is rather minimal. Notably producing
    multi-page documents is not yet supported.

    \item There is no support for printing.  The above mentioned PDF
    methods allow for generating PDF from graphicals that can then be
    printed.

    \item Files and text dropped from external applications are
    delivered as events (see \pllib{pce_drop_target}).  Dragging objects
    from \product{} to external applications is not supported.

    \item Support for Windows meta-files has been removed.

    \item The C and C++ embedding API (classes \const{c} and
    \const{c_pointer}) has been removed.

    \item The development tools are \jargon{panes} that can be placed
    in tabs or beside each other in the windows of the development
    environment (see \pllib{pane_frame}).

    \item The reference manual sources are markdown files.  The online
    manual renders them as HTML.
\end{itemize}

\section{The future for \product{}}			\label{sec:future}

The underlying libraries allow for many extensions. Notably SDL provides
APIs for dealing with video, audio and many input devices used for
gaming. Cairo supports a \jargon{transformation matrix}, which is
available for scaling, rotating and skewing the contents of a
\class{figure} using a \class{transform}. Partial transparency
(\jargon{alpha channel}) is supported in \class{colour} and through
`graphical->opacity'.  Transformations are not (yet) supported for
other graphicals.
