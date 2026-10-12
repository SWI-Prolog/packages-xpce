\section{Printing}				\label{sec:printing}

\product{} prints graphics by rendering them as PDF.  The PDF file may
be saved or handed to the printing facilities of the operating system.
Rendering uses Cairo, the same library that is used to paint on the
screen, and therefore the PDF output looks the same as the screen.
Text and line drawings are preserved as vectors, so the output is
scalable.

\begin{description}
    \sendmethod{graphical}{pdf}{File:file, Scale:[num]}
Write the graphical to File as a single page PDF document.  The page
has the size of the graphical, multiplied by Scale (default 1.0).  For
a \class{device}, all graphicals displayed on the device are included.
    \sendmethod{window}{pdf}{File:file, Scale:[num]}
Write the full contents of the window as a single page PDF document.
Unlike the screen, the output is not restricted to the visible part
of the window: the page covers the bounding box of all graphicals
displayed on the window.
\end{description}

The library \pllib{print_graphics} defines the class template
\class{print_graphics} that adds methods for printing and saving as
PDF to a window.  It is used by, for example, PceDraw.  The method
\send{print} prints the window on the default printer by passing the
PDF to the command defined by \get{print_command_template}
(\program{lpr} by default) and \send{save_pdf} prompts for a file
and saves the contents of the window as PDF.

\begin{code}
:- use_module(library(print_graphics)).

:- pce_begin_class(my_picture, picture).
:- use_class_template(print_graphics).
...
:- pce_end_class(my_picture).
\end{code}
