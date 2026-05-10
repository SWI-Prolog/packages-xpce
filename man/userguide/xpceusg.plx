\documentclass[11pt,twoside]{report}
\def\setuphyperref{\usepackage[pdftex,colorlinks=true,urlcolor=blue]{hyperref}}
\usepackage{pl}
\usepackage{html}
\usepackage{logo}
\usepackage{plpage}
\usepackage{xpce}
\usepackage{times}
\usepackage{fancyheadings}

\htmloutput{UserGuide}				% Output directory
\htmlmainfile{index}				% Main document file
\bodycolor{white}
\linkimage{home}{home.gif}			% Images for links
\linkimage{index}{yellow_pages.gif}
\linkimage{contents}{index.gif}
\linkimage{up}{up.gif}
\linkimage{previous}{prev.gif}
\linkimage{next}{next.gif}
\linkimage{summary}{info.gif}

\newcommand{\product}{{\sc xpce}}
\newcommand{\plainproduct}{XPCE}
\newcommand{\productpl}{{\sc xpce/p}rolog}
\newcommand{\productversion}{7.0.0}
\newcommand{\quintus}{Quintus}
\newenvironment{xpceonly}{}{}
\excludecomment{pwonly}
\newcommand{\productdate}{November 2025}

\renewcommand{\runningtitle}{\product{} \productversion}
\def\booktitle{Programming in XPCE/Prolog}
\author{\href{mailto:jan@swi-prolog.org}{Jan Wielemaker} \\
	Anjo Anjewierden\textsuperscript{\dag} \\
	SWI, University of Amsterdam
        SWI-Prolog Solutions b.v.}

\sloppy
%\psdraft

\makeindex

%\includeonly{}

\begin{document}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%		HTML TitlePage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{htmlonly}

\maketitle

\begin{abstract}
\productpl{} is a hybrid environment integrating logic programming and
object-oriented programming for Graphical User Interfaces. Applications
in \productpl{} are portable across the supported X11 and Win32
(Windows NT/2000/XP) platforms.

{\it Last updated November, 2025 for \product{} version
 \productversion}

Copyright \copyright\ 1992-2025 University of Amsterdam, SWI-Prolog
Solutions b.v.
\end{abstract}

\hr
\subsubsection*{About this document}

This document is maintained in the LaTeX document processing system. It
was converted to HTML format using a latex2html written in SWI-Prolog
which concentrates on extensibility, rather then making the HTML
document look as closely as possible the same as the LaTeX version. The
document is maintained using macros dedicated to describe its content.
These macros are defined both for LaTeX to arrive at a proper typeset
version, and for the latex2html translator to arrive at a proper
hypertext document. For further information, please contact \email[Jan
Wielemaker]{jan@swi-prolog.org}

Each section in the document is translated into an HTML page.  Each
page contains links to the \strong{table of contents}, \strong{Index},
and \strong{Class Summary}.  Subsections contain links to their
super-section, and sections that have subsections contain a
\strong{`local' index} at the end of the page.
\end{htmlonly}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%		Normal document titlepage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\pagestyle{empty}
\begin{xpceonly}
\begin{titlepage}
\mbox{}
\vfil
\centerline{\includegraphics[width=\textwidth]{title}}
\vfil\vfil\vfil
\newpage
%\titlepageheader
\vfil

\noindent
{\bf Programming in \productpl{}} \\[1cm]
\begin{tabular}{ll}
Jan Wielemaker   & \email{jan@swi-prolog.org} \\
Anjo Anjewierden\textsuperscript{\dag}
\end{tabular}

\vfil\noindent
\productpl{} is a hybrid environment integrating logic programming and
object-oriented programming for building Graphical User Interfaces.
Applications in \productpl{} are fully compatible across the supported
platforms.\\[10pt]

\vfil\noindent
This document applies to XPCE/Prolog \productversion{} distributed
as integrated package to SWI-Prolog.

\vfil\noindent

XPCE~7 is a major release. \textbf{This manual still reflects mostly
version 6.6.} \Secref{font} is updated and several new sections have
been added. See \appref{sdl-driver} (SDL backend), \appref{mswin}
(Windows notes), \secref{thread} (multi-threading) and
\appref{xpce-status} (status and future). The main changes for version~7
are below.

\begin{itemize}
    \item While version~6 has two \jargon{backends}, one for X11 and
    one for Windows, version~7 is based on portable libraries.   Notably
    \begin{itemize}
        \item \href{https://www.libsdl.org}{SDL3} is used for window
	management and accessing devices.
	\item \href{https://www.cairographics.org/}{Cairo} is used for
	2D graphics
	\item \href{https://www.gtk.org/docs/architecture/pango}{Pango}
	is used for rendering of text, with an emphasis on
	internationalization.
    \end{itemize}
    With this change of backend, \product{} now supports X11,
    \href{https://wayland.freedesktop.org/}{Wayland}, MacSOS
    \href{https://en.wikipedia.org/wiki/Cocoa_(API)}{Cocoa} and
    Windows.  Notably the support for Wayland and Cocoa rather
    than X11 makes \product{} future proof and provides a better
    user experience, notably on MacOS.

    \item The Windows \program{swipl-win.exe} and Qt \program{swipl-win}
    app used on Linux and MacOS have been replaced by terminal code
    added to \product{}, resulting in the \textit{Epilog} Prolog
    console that runs on all platforms.

    \item Font, colour and image management is modernized and synchronized
    over all platforms. Pango is used for font management. Colours use
    the 32-bit RGBA model (Red/Green/Blue/Alpha).  All images are RGBA
    images.  Loading images is done by the SDL_image library, supporting
    many more formats.

    \item \product{}~7 relaxes multi-threaded access.  When \product{}
    calls out to Prolog its global lock is released, to be re-acquired
    when Prolog has finished and control returns to \product{}.  This
    model is similar to the Python \jargon{GIL}.
\end{itemize}

\vfil\noindent
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
BSD-2 for more details.

\vfil\vfil
\noindent
Titlepage created using \product{} 4.8.10 on Windows-NT 4.0
\vfil
\noindent
Last updated February 2025 for \product{} version \productversion
\vfil
\noindent
Copyright \copyright\ 1992-2025 University of Amsterdam and SWI-Prolog
Solutions b.v.
\end{titlepage}
\end{xpceonly}

\input{body.tex}

\end{document}
