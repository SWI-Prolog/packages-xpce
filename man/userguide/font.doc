\section{Specifying fonts}			\label{sec:font}

\product{}'s font specification is a three-stage process based on the
\href{https://www.gtk.org/docs/architecture/pango}{Pango} library as
well as \product{}'s older legacy font handling.  An \product{} font
is represented by \class{font}, which is created from four parameters:

\begin{itemize}
    \item \textbf{family} is the logical font family.  The typical
    values are \const{sans}, \const{serif} and \const{mono}.  These
    are mapped to Pango font families based on
    \classvar{font}{pango_families}.  The default mapping depends on the
    platform.   It may be overruled in the \file{Defaults} file.  Below
    is an example.  See the Pango documentation for details.
\begin{code}
font.pango_families: [ sans  := "Noto Sans,sans", \
		       serif := "Noto Serif,serif", \
		       mono  := "Noto Sans Mono,monospace" \
		     ]
\end{code}

    \item \textbf{style} is one of \const{normal}, \const{bold},

    \const{italic} or \const{oblique}.  Note that \const{bold}
    is a compatibility shorthand for \const{normal} with the
    weight \const{bold}.

    \item \textbf{size} is the \jargon{point-size} of the font.

    \item \textbf{weight} is either a Pango defined symbolic
    weight or an integer in the range $100\ldots{}1000$.  Symbolic
    names are \const{thin}, \const{ultralight}, \const{light},
    \const{semilight}, \const{book}, \const{normal}, \const{medium},
    \const{bold}, \const{ultrabold}, \const{heavy}, or
    \const{ultraheavy}
\end{itemize}

\subsection{Logical fonts}		\label{sec:fontalias}.

It is not wise let your application code speak about physical fonts as
the user or interface guidelines may prefer using a different
font-palette. For this reason the display defines a mapping between
logical font names and \class{font} objects. Applications are encouraged
to use logical font names as much as possible and leave the assignment
to physical fonts to the users preferences. \product{} predefines the
following logical font-names. The value gives the default assignment for
these fonts.

\begin{itemize}
\newcommand{\fontalias}[2]{\tick{\makebox[1in][l]{#1}#2}}
    \fontalias{normal}{font(sans, normal, 12)}
The default font. Normally a proportional normal font. Should be easy to
read.
    \fontalias{bold}{font(sans, bold, 12)}
Bold version of the normal font.
    \fontalias{italic}{font(sans, italic, 12)}
Slanted version of the normal font.
    \fontalias{small}{font(sans, normal, 10)}
Small version of the normal font.  To be used in notes, subscripts, etc.
May not be so easy to read, so avoid using it for long texts.
    \fontalias{large}{font(sans, normal, 14)}
Slightly larger version of the normal font.
    \fontalias{boldlarge}{font(sans, bold, 14)}
Bold version of large.
    \fontalias{huge}{font(sans, normal, 18)}
Even larger font.  To be used for titles, etc.
    \fontalias{boldhuge}{font(sans, bold, 18)}
Bold version of huge.
    \fontalias{fixed}{font(mono, normal, 14)}
Terminal font. To be used for code fragments, code editors, etc. Should
be easy to read.
    \fontalias{tt}{font(mono, normal, 14)}
Same as \const{fixed}.
    \fontalias{boldtt}{font(mono, bold, 14)}
Bold terminal font.
\end{itemize}

The end-user of an \product{} application can define the class-variable
\classvar{font.system_fonts} to overrule fonts.

Class \class{font}'s predefined conversion will translate names to font
objects.  This implies that for any method expecting a font object the
programmer can specify the font-name instead.  In those (rare) cases
where a font needs to be passed, but the type-specification does not
require this, the conversion must be done explicitly.  The preferred
way to make the conversion is using the font type object:

\begin{code}
	...,
	get(type(font), check, bold, BoldFont),
	...,
\end{code}

\subsection{Unicode}

\product{} fully supports Unicode on non-Windows platforms. On Windows,
many parts of \product{} handle strings as UCS2 encoded, which implies
that only characters up to 0xffff are handled properly. On most
platforms Pango supports coloured glyphs, which allows \product{} to
use Unicode emoji symbols.
