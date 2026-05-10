\chapter{SDL video drivers} \label{sec:sdl-driver}

\product{} relies on \href{https://www.libsdl.org}{SDL} to access host
windows, the keyboard, mouse and other devices. SDL is primarily a
library for implementing portable games. It is a good match for
\product{} because \product{} GUI widgets are implemented in \product{}
itself, providing maximal portability.

SDL provides multiple \jargon{drivers}, depending on how it is built and
the hosting operating system. The default driver also depends on how the
library is built as well as the OS.  Details on window behaviour and
event handling may be affected by the driver.   The current driver can
be retrieved using `pce<-window_system_driver`, e.g.,

\begin{code}
?- get(@pce, window_system_driver, Driver).
Driver = wayland.
\end{code}

If multiple drivers are supported, a specific driver may be selected
using the environment variable \const{SDL_VIDEODRIVER} or using the
SWI-Prolog flag \prologflag{'SDL_VIDEODRIVER'}.  The latter allows for
e.g.,

\begin{code}
swipl -DSDL_VIDEODRIVER=x11 ...
\end{code}

\index{headless}%
All SDL implementations support the driver \const{dummy}.  This allows
for using \product{} \jargon{headless}.  This is typically useful for
executing build steps that involve \product{}, both on systems that
always provide a GUI context (Windows, MacOS) to avoid opening windows
and on systems that may not provide a graphical context such as almost
all Unix-derived systems.

\index{Linux,X11}\index{Linux,Wayland}%
Today's Linux systems are migrating from X11 to Wayland. The default
used on Linux depends on how SDL was built, more and more favouring
Wayland as default. In general, Wayland is faster and more secure.
Security on Wayland is better than on X11 because Wayland restricts
control of absolute positions of windows, does not allow accessing the
display beyond windows owned by the application and restricts grabbing
the mouse and keyboard focus. While \product{} works with both drivers,
SDL may not honour various requests on Wayland.

