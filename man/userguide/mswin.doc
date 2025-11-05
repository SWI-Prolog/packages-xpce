\chapter{Notes on XPCE for MS-Windows}		\label{sec:mswin}

\index{Windows}%
\product{} runs on Windows~10 and later. Its functionality is very close
to the non-Windows versions, making applications source-code compatible
between the two platforms.

\product{} does not build on top of the hosting window-systems GUI library.
Instead, the primitive windowing and graphics facilities of the host are
used to implement `\product{}'s Virtual Window System'. All of \product{}'s
graphical functionality is build on top of this `Virtual Window System'.
This approach guarantees full portability of applications between the
platforms.

The look-and-feel of \product{} may be tailored using the defaults file
located in \metafile{<pcehome>/Defaults}.


\section{Currently unsupported features in the Win32 version}

\begin{itemlist}
    \item [Class socket]
No support of file-based addressing (Unix domain sockets). Inet-domain
sockets are provided (interfacing to WinSock).
\end{itemlist}


\section{Interprocess communication, extensions and interaction}

\begin{itemlist}
    \item [DDE]		\index{DDE}%
Not (yet) supported by \product{}. \ifpw{}{SWI-Prolog supports it
though, making DDE a feasible interprocess communication approach.}
    \item [WinSock]		\index{WinSock}%
Provides standard TCP/IP communication, both server- and client-side.
    \item [Named Pipes]		\index{named pipes}%
Not (yet) supported.
    \item [Cut/Paste]		\index{cut-and-paste}%
Supported for exchanging text.
\end{itemlist}


\section{Accessing Windows Graphics Resources}

\product{} on Win32 defines the same cursor, colour and font-names as the
Unix/X11 version to guarantee portability. It is desirable to have
access to all the native Windows graphical resources. This allows the
application to maintain better look-and-feel compatibility to other
Win32 applications. Therefore the classes colour, cursor and font
provide access to related Window resources.

\strong{It is NOT advised to use these objects in your application code
directly as this will stop the application to run on the Unix/X11
version of \product{}. We advice using these objects in the \product{}
defaults file (\metafile{<pcehome>/Defaults}) only, or use conditional code
using `pce <-window_system'.}


\section{Accessing Windows Colours}

Colours may be created from their X11 names. To provide access to the
window-system colours as they can be obtained using the Win32 API
function GetSysColor(), \product{} binds these colours to named colour
objects. These colour objects are normally used in the \product{}
resource file (\metafile{<pcehome>/Defaults}) to colour \product{}'s
controller objects according to the user's preferences.

If the name of the Windows API colours are COLOR_SOMETHING, the \product{}
name is \const{win_something}.  The full list is in \tabref{wincolours}.

\begin{table}
\begin{center}
\begin{tabularlp}{\const{win_gradientinactivecaption}}
\hline
\const{win_3ddkshadow} & Dark shadow for three-dimensional display elements. \\
\const{win_3dface} & \\
\const{win_btnface} & Face color for three-dimensional display elements. \\
\const{win_3dhilight} & \\
\const{win_3dhighlight} & \\
\const{win_btnhilight} & \\
\const{win_btnhighlight} & Highlight color for three-dimensional display elements \\
\const{win_3dlight} & Light color for three-dimensional display elements \\
\const{win_3dshadow} & \\
\const{win_btnshadow} & Shadow color for three-dimensional display elements \\
\const{win_activeborder} & Active window border. \\
\const{win_activecaption} & Active window title bar. \\
\const{win_appworkspace} & Background color of MDI applications. \\
\const{win_background} & \\
\const{win_desktop} & Desktop. \\
\const{win_btntext} & Text on push buttons. \\
\const{win_captiontext} & Text in caption, size box, and scroll bar arrow box. \\
\const{win_gradientactivecaption} & Right side color of an active window's title bar. \\
\const{win_activecaption} & specifies the left side color. \\
\const{win_gradientinactivecaption} & Right side color of an inactive window's title bar. \\
\const{win_inactivecaption} & specifies the left side color. \\
\const{win_graytext} & Grayed (disabled) text. \\
\const{win_highlight} & Item(s) selected in a control. \\
\const{win_highlighttext} & Text of item(s) selected in a control. \\
\const{win_hotlight} & Color for a hot-tracked item. \\
\const{win_inactiveborder} & Inactive window border. \\
\const{win_inactivecaption} & Inactive window caption. \\
\const{win_inactivecaptiontext} & Color of text in an inactive caption. \\
\const{win_infobk} & Background color for tooltip controls. \\
\const{win_infotext} & Text color for tooltip controls. \\
\const{win_menu} & Menu background. \\
\const{win_menutext} & Text in menus. \\
\const{win_scrollbar} & Scroll bar gray area. \\
\const{win_window} & Window background. \\
\const{win_windowframe} & Window frame. \\
\const{win_windowtext} & Text in windows \\
\hline
\end{tabularlp}
\end{center}
\caption{Windows colour name mapping}
\label{tab:wincolours}
\end{table}

\section{Unicode text on Windows}

\product{} represents characters using the C type \const{wchar_t}. On
most platforms that is an unsigned 32-bit integer.  On Windows however
this is a 16 bit unsigned integer.  Windows uses UTF-16 encoding to
represent Unicode code points above 0xffff.   Support for UTF-16 in
\product{} is currently inconsistent.
