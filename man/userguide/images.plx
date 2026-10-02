\section{Using images and cursors}		\label{sec:images}

Many today graphical user interfaces extensively use (iconic) images.
There are many image formats, some for specific machines, some with
specific goals in mind, such as optimal compression at the loss of
accuracy or provide alternatives for different screen properties.

One of \product{}'s aim is to provide portability between the supported
platform. Therefore, we have chosen to support a few formats across all
platforms, in addition to the most popular formats for each individual
platform.


\subsection{Colour handling}			\label{sec:colour}

\index{colour,images}%
Colour handling is a hard task for todays computerprogrammer. There is a
large variety in techniques, each providing their own advantages and
disadvantages. \product{} doesn't aim for programmers that need to get
the best performance and best results rendering colours, but for the
programmer who wants a reasonable result at little effort.

As long as you are not using many colours, which is normally the case
as long as you do not handle full-colour images, there is no problem.
This is why this general topic is handled in the section on images.


\subsubsection{System colours}			\label{sec:syscolours}

\index{colour,system}\index{system colours}%
Besides the CSS and X11 colour names, \product{} defines colour names
that reflect the colours chosen by the user in the desktop settings,
such as the light or dark appearance, a high-contrast theme or the
accent colour.  There are two groups of system colours:

\begin{itemlist}
    \item [Common names]
The names starting with \const{sys_} are defined on all platforms.  They
denote a role, such as the background of a window or the colours of a
selection.  \product{}'s own defaults use these names, and portable
applications should use them if they want to follow the desktop
settings.  \Tabref{syscolours} shows how they are mapped on each
platform.
    \item [Platform specific names]
Colours that only make sense on one platform use a platform prefix:
\const{win_} on Windows, \const{mac_} on MacOS and \const{kde_} on
KDE.  On GNOME there are no platform specific names.  These names are
\emph{not} defined on the other platforms, so using them makes an
application non-portable.
\end{itemlist}

\begin{table}
\begin{center}
\begin{tabular}{|l|l|l|l|l|}
\hline
\bf Name & \bf Windows & \bf MacOS & \bf KDE & \bf Fallback \\
\hline
\const{sys_window_background}
	& \const{COLOR_WINDOW}
	& \const{textBackgroundColor}
	& \const{View} BackgroundNormal
	& white \\
\const{sys_window_foreground}
	& \const{COLOR_WINDOWTEXT}
	& \const{textColor}
	& \const{View} ForegroundNormal
	& black \\
\const{sys_dialog_background}
	& \const{COLOR_BTNFACE}
	& \const{windowBackgroundColor}
	& \const{Window} BackgroundNormal
	& grey80 \\
\const{sys_dialog_foreground}
	& \const{COLOR_BTNTEXT}
	& \const{labelColor}
	& \const{Window} ForegroundNormal
	& black \\
\const{sys_button_background}
	& \const{COLOR_BTNFACE}
	& \const{controlColor}
	& \const{Button} BackgroundNormal
	& grey80 \\
\const{sys_button_foreground}
	& \const{COLOR_BTNTEXT}
	& \const{controlTextColor}
	& \const{Button} ForegroundNormal
	& black \\
\const{sys_button_pressed}
	& \const{COLOR_3DLIGHT}
	& \const{selectedControlColor}
	& \const{Button} BackgroundAlternate
	& grey70 \\
\const{sys_selection_background}
	& \const{COLOR_HIGHLIGHT}
	& \const{selectedContentBackgroundColor}
	& \const{Selection} BackgroundNormal
	& black \\
\const{sys_selection_foreground}
	& \const{COLOR_HIGHLIGHTTEXT}
	& \const{alternateSelectedControlTextColor}
	& \const{Selection} ForegroundNormal
	& white \\
\const{sys_tooltip_background}
	& \const{COLOR_INFOBK}
	& \const{windowBackgroundColor}
	& \const{Tooltip} BackgroundNormal
	& burlywood1 \\
\const{sys_tooltip_foreground}
	& \const{COLOR_INFOTEXT}
	& \const{labelColor}
	& \const{Tooltip} ForegroundNormal
	& black \\
\const{sys_inactive}
	& \const{COLOR_GRAYTEXT}
	& \const{disabledControlTextColor}
	& \const{Window} ForegroundInactive
	& grey50 \\
\const{sys_link}
	& \const{COLOR_HOTLIGHT}
	& \const{linkColor}
	& \const{View} ForegroundLink
	& \#0000ee \\
\const{sys_accent}
	& \const{COLOR_HIGHLIGHT}
	& \const{controlAccentColor}
	& \const{General} AccentColor
	& dodger_blue \\
\const{sys_separator}
	& \const{COLOR_BTNSHADOW}
	& \const{separatorColor}
	& (derived)
	& grey50 \\
\const{sys_shadow}
	& \const{COLOR_BTNSHADOW}
	& \const{tertiaryLabelColor}
	& (derived)
	& grey50 \\
\hline
\end{tabular}
\end{center}
\caption[Mapping of the system colour names]{Mapping of the \const{sys_} colour names.  The Windows column
	 names the argument to GetSysColor(), the MacOS column the
	 NSColor class method and the KDE column the group (without
	 \const{Colors:}) and key in \file{kdeglobals}.  The Fallback
	 column is used on other platforms and if the platform does not
	 provide the colour.  GNOME is described separately in
	 \tabref{gnomecolours}.}
\label{tab:syscolours}
\end{table}

The following notes apply to the mapping:

\begin{itemlist}
    \item [Windows]
The \const{win_} names are documented in \secref{mswin}.  Windows does
not provide the accent colour through GetSysColor(), so
\const{sys_accent} is the same as \const{sys_selection_background}.
Windows dark mode does not change the colours of GetSysColor().  If
the user selected dark mode for applications and no contrast theme is
active, the \const{sys_} names therefore use a dark palette that
follows the Windows~11 dark appearance, while \const{sys_accent} and
\const{sys_selection_background} use the accent colour selected by
the user.  The \const{win_} names always use GetSysColor().
    \item [MacOS]
All \const{mac_} names are derived from an NSColor class method: the
method name without the \const{Color} suffix, converted to snake case
and prefixed with \const{mac_}.  For example,
\const{selectedContentBackgroundColor} becomes
\const{mac_selected_content_background}.  The names defined are
\const{mac_label},
\const{mac_secondary_label},
\const{mac_tertiary_label},
\const{mac_quaternary_label},
\const{mac_text},
\const{mac_placeholder_text},
\const{mac_selected_text},
\const{mac_text_background},
\const{mac_selected_text_background},
\const{mac_keyboard_focus_indicator},
\const{mac_unemphasized_selected_text},
\const{mac_unemphasized_selected_text_background},
\const{mac_link},
\const{mac_separator},
\const{mac_selected_content_background},
\const{mac_unemphasized_selected_content_background},
\const{mac_selected_menu_item_text},
\const{mac_grid},
\const{mac_header_text},
\const{mac_control_accent},
\const{mac_control},
\const{mac_control_background},
\const{mac_control_text},
\const{mac_disabled_control_text},
\const{mac_selected_control},
\const{mac_selected_control_text},
\const{mac_alternate_selected_control_text},
\const{mac_window_background},
\const{mac_window_frame_text},
\const{mac_under_page_background},
\const{mac_find_highlight},
\const{mac_highlight},
\const{mac_shadow},
the fill colours \const{mac_system_fill},
\const{mac_secondary_system_fill},
\const{mac_tertiary_system_fill},
\const{mac_quaternary_system_fill} and
\const{mac_quinary_system_fill}, and the adaptive colours
\const{mac_system_red},
\const{mac_system_orange},
\const{mac_system_yellow},
\const{mac_system_green},
\const{mac_system_mint},
\const{mac_system_teal},
\const{mac_system_cyan},
\const{mac_system_blue},
\const{mac_system_indigo},
\const{mac_system_purple},
\const{mac_system_pink},
\const{mac_system_brown} and
\const{mac_system_gray}.  Colours that are not provided by the running
version of MacOS are not defined.

MacOS colours depend on the appearance (light, dark or increased
contrast).  They are resolved using the appearance of the application.
Many of them are translucent.  These are composed over
\const{mac_window_background}, so all system colours are opaque.

MacOS has no tooltip colours.  The tooltip colours are therefore the
same as the dialog colours.
    \item [KDE]
On Unix systems other than MacOS, \product{} uses the KDE colour scheme
if the environment variable \env{XDG_CURRENT_DESKTOP} contains
\const{KDE} or \env{KDE_FULL_SESSION} is \const{true}.  The colour
scheme is read from the \file{kdeglobals} files in the directories of
\env{XDG_CONFIG_DIRS} (default \file{/etc/xdg}) and
\env{XDG_CONFIG_HOME} (default \file{\Stilde{}/.config}), where the latter
takes precedence.  This does not require the KDE or Qt libraries.

All colours of the groups \const{[Colors:\em Group]}, including
sub-groups such as \const{[Colors:Header][Inactive]}, and of the group
\const{[WM]} are defined as \const{kde_<group>_<key>}, converted to
snake case.  For example, \const{BackgroundNormal} in
\const{[Colors:View]} becomes \const{kde_view_background_normal} and
\const{activeBackground} in \const{[WM]} becomes
\const{kde_wm_active_background}.  \const{AccentColor} in
\const{[General]} becomes \const{kde_accent}.

If \file{kdeglobals} does not define a colour that is used for a
\const{sys_} name, \product{} uses the colour of the default KDE scheme,
Breeze Light.  If there is no \const{AccentColor}, \const{sys_accent} is
\const{DecorationFocus} of \const{[Colors:View]}.  KDE has no separator
and shadow colours.  These are computed by mixing
\const{sys_dialog_foreground} into \const{sys_dialog_background} for
20\% (separator) and 40\% (shadow).
    \item [GNOME]
GNOME does not publish its colours.  If \env{XDG_CURRENT_DESKTOP}
contains \const{GNOME} (and we are not running under KDE),
\product{} uses a built-in copy of the libadwaita palette.  It asks the
XDG Desktop Portal (namespace \const{org.freedesktop.appearance}) for
the following settings:

\begin{itemlist}
    \item [\const{color-scheme}]
If this is \const{1} (prefer dark), the dark palette is used.
Otherwise the light palette is used.
    \item [\const{contrast}]
If this is \const{1} (high contrast), the text colours are opaque and
the separator and shadow colours are stronger.
    \item [\const{accent-color}]
The accent colour, available since GNOME~47.  If it is missing or out
of range, \product{} uses the default libadwaita accent, \#3584e4.  The
text on a selection is white, unless the accent colour is light.
\end{itemlist}

\Tabref{gnomecolours} shows the result.  The \emph{ink} is the text
colour of libadwaita: \verb$rgba(0,0,6,0.8)$ in the light palette and
white in the dark palette.  A percentage means that this fraction of the
ink is composed over the dialog background, which is how libadwaita
defines these colours in its CSS.  The libadwaita window background is
almost white, which would make dialogs indistinguishable from the
content windows.  The dialog background is therefore the sidebar
background of libadwaita, which it uses for panels next to the
content.  There are no \const{gnome_} colour names.

The portal is accessed over D-Bus using GIO.  If \product{} was built
without GIO, or there is no portal, the light palette with the default
accent colour is used.

\begin{table}
\begin{center}
\begin{tabular}{|l|l|l|l|}
\hline
\bf Name & \bf Derived from & \bf Light & \bf Dark \\
\hline
\const{sys_window_background} & view background & \#ffffff & \#1d1d20 \\
\const{sys_window_foreground} & ink & \#333338 & \#ffffff \\
\const{sys_dialog_background} & sidebar background & \#ebebed & \#2e2e32 \\
\const{sys_dialog_foreground} & ink & \#2f2f34 & \#ffffff \\
\const{sys_button_background} & 10\% ink & \#d8d8db & \#434347 \\
\const{sys_button_foreground} & ink & \#2f2f34 & \#ffffff \\
\const{sys_button_pressed} & 30\% ink & \#b3b3b6 & \#6d6d70 \\
\const{sys_selection_background} & accent & \#3584e4 & \#3584e4 \\
\const{sys_selection_foreground} & white or black & \#ffffff & \#ffffff \\
\const{sys_tooltip_background} & 80\% \#000006 & \#2f2f34 & \#09090f \\
\const{sys_tooltip_foreground} & white & \#ffffff & \#ffffff \\
\const{sys_inactive} & 50\% ink & \#8d8d91 & \#979799 \\
\const{sys_link} & accent & \#3584e4 & \#3584e4 \\
\const{sys_accent} & accent & \#3584e4 & \#3584e4 \\
\const{sys_separator} & 15\% ink & \#cfcfd1 & \#4d4d51 \\
\const{sys_shadow} & 30\% ink & \#b3b3b6 & \#6d6d70 \\
\hline
\end{tabular}
\end{center}
\caption[The system colours on GNOME]{The \const{sys_} colours on GNOME, using the default accent colour
	 and normal contrast.}
\label{tab:gnomecolours}
\end{table}
    \item [Other platforms]
On other platforms, such as Linux running Xfce, the system colours have
fixed values that reproduce \product{}'s traditional look.  They do not
depend on the desktop settings.
\end{itemlist}

The system colours are determined when \product{} looks up a colour name
for the first time, normally while it starts up.  They are reloaded by
`display_manager ->system_colours_changed', which also redraws all
windows.  Named colour objects for the system colours, such as
\exam{colour(sys_dialog_background)}, are updated in place, so all
graphicals that use them get the new colour.  \product{} sends this
message to \exam{@display_manager} if it is told that the desktop
settings changed:

\begin{itemlist}
    \item [All platforms]
If the user switches between light and dark mode.
    \item [Windows]
Also if the user selects another contrast theme or accent colour.
    \item [MacOS]
Also if the user changes the accent colour, the highlight colour or the
contrast.
\end{itemlist}

Other changes, such as a new accent colour on KDE or GNOME
or another KDE colour scheme with the same brightness, are not noticed.
The application may send ->system_colours_changed itself, or
\product{} must be restarted.

After reloading the system colours, the theme colours that are derived
from them are resolved again (see \secref{themes}).  Next,
->system_colours_changed sends `display_manager
<-system_colours_message' if this is not \const{@nil}, which allows the
application to select another theme.  Finally, all windows are redrawn.


\subsubsection{Themes}				\label{sec:themes}

\index{theme}\index{dark theme}\index{theme colour}%
A \jargon{theme} defines the colours of the \product{} user interface
and the SWI-Prolog development tools, such as the colours for syntax
highlighting in PceEmacs.  \product{} itself only uses
\jargon{theme colours}: colours whose name describes their role rather
than their value.  For example, the background of windows is
\const{ui_window_background} and the colour of a comment in PceEmacs
is \const{syntax_comment}.  A theme maps these names to values.
Switching themes changes the values of the theme colours in place and
redraws all windows, so themes can be switched while \product{} is
running.

The library \pllib{pce_theme} manages themes.  The \const{light} theme
is the default.  The other themes are files in
\file{library(theme)} that define the colours for their theme, such as
\file{library(theme/dark)}.

\paragraph{Selecting a theme}

By default, \product{} follows the light or dark setting of the desktop
(see `display <-theme') and switches theme if the user changes this
setting.  This is overruled by, in this order:

\begin{itemlist}
    \item [The Settings/Theme menu]
The \emph{Settings} menu of the IDE tools, such as \program{swipl-win},
has a \emph{Theme} submenu to follow the desktop or use one of the
available themes.  This calls select_theme/1 and holds for the running
session.
    \item [The Prolog flag \prologflag{theme}]
For example, \exam{swipl -Dtheme=dark}.
    \item [The class variable \const{display.theme}]
Setting this in the Defaults file (see \secref{classvar}) makes the
choice permanent:

\begin{code}
display.theme: dark
\end{code}
\end{itemlist}

\paragraph{Theme colours}

A \idx{theme colour} is an instance of class \class{theme_colour}, a
subclass of \class{colour}.  It is created from a name that describes
its role and a value, e.g., \exam{theme_colour(syntax_comment,
dark_green)}.  The value is the name of another colour, which may be
another theme colour or a system colour, or a colour object.  The RGB
value is computed when it is needed by following the value through
other theme colours.  Changing the value of a theme colour using
`theme_colour ->value', or creating it again with another value, makes
all theme colours compute their RGB value again on their next use.
Theme colours may therefore refer to each other in any order.  Theme
colours are locked, so they are never garbage collected, and they are
never returned when looking up a colour from its RGB values.  After
changing theme colours, `display_manager ->colours_changed' redraws all
windows.  As graphicals refer to the colour object, either directly or
by name, they show the new value after the windows are redrawn.

The theme colours are named using a prefix that describes where they
are used:

\begin{itemlist}
    \item [\const{ui_}\arg{role}]
The basic colours of the user interface.  For each system colour
\const{sys_}\arg{role} (see \tabref{syscolours}) there is a theme
colour \const{ui_}\arg{role}, e.g., \const{ui_window_background}.
\product{} only uses the \const{ui_} names, such that a theme can
replace the system colours (see below).  Other \const{ui_} colours are
used for, e.g., the text selection
(\const{ui_text_selection_background}) and incremental search
(\const{ui_isearch_background}).  The \const{ui_} colours and their
values in the \const{light} theme are in the hash table
@theme_colour_defaults.
    \item [\const{ansi_}\arg{colour}]
The 16 ANSI colours of the terminal, from \const{ansi_black} to
\const{ansi_bright_white}.
    \item [\const{syntax_}\arg{class}]
The colours of PceEmacs syntax highlighting.  The name is derived from
the syntax class of library(prolog_colour): \const{syntax_} followed by
the name and the atomic arguments of the class, separated by
\const{_}.  For example, the class \exam{goal(built_in,_)} uses
\const{syntax_goal_built_in}.  A background colour gets the suffix
\const{_bg}.  The value in the \const{light} theme is the colour that
is defined for the class.
    \item [\const{debug_}, \const{prof_}, \const{xref_}, \ldots]
The colours of the graphical debugger, the profiler, the cross
referencer and other tools.
\end{itemlist}

\paragraph{Using the system colours}

The \const{ui_} colours are the system colours if the theme and the
desktop are both light or both dark.  This preserves the look of the
desktop.  Otherwise, for example when using the dark theme on a light
desktop, the theme replaces the system colours by colours of its own.
A theme is dark if its \const{ui_window_background} is dark.  The
\const{light} theme provides its own light colours for use on a dark
desktop.

\paragraph{Writing a theme}

A theme is a Prolog file \file{library(theme/}\arg{Name}\file{)} that
defines the values of the theme colours using the multifile predicate
pce_theme:colour/3.  It does not need to load \productpl{}.  The
library \file{library(theme/dark)} is a complete example.  The skeleton
is below.

\begin{code}
:- module(prolog_theme_mytheme, []).
:- multifile
    prolog:theme/1,
    pce_theme:colour/3.

prolog:theme(mytheme).

pce_theme:colour(mytheme, Name, Value) :-
    colour(Name, Value).

colour(ui_window_background, '#1e1e1e').
colour(ui_window_foreground, white).
...
colour(syntax_comment,       green).
...
\end{code}

A theme should define all theme colours.  Colours it does not define
use their value in the \const{light} theme.  The theme colours of the
\const{ui_} roles are only used if the theme does not match the
desktop.  The predicate check_theme/1 verifies a theme against the
theme colours of xpce and the development tools.  It reports theme
colours that are not defined, defined twice or unknown, and values that
are not colours.  After changing a theme file, use make/0 and
apply_theme/1 to see the result.

\paragraph{Theme colours of an application}

An application that wants its colours to follow the theme declares
theme colours with their value in the \const{light} theme and uses the
name of the theme colour wherever it needs the colour, for example in
a class variable:

\begin{code}
:- use_module(library(pce_theme)).
:- theme_colours([ myapp_highlight = khaki1 ]).

class_variable(highlight, colour, myapp_highlight).
\end{code}

A theme defines the value for the other themes:

\begin{code}
pce_theme:colour(dark, myapp_highlight, khaki4).
\end{code}

Graphicals that refer to a theme colour follow the theme by themselves.
Colours that are \emph{copied}, however, do not.  This applies to an
image into which graphicals are drawn using `image ->draw_in', to a
colour computed from another colour and to colours passed to an
external program such as Graphviz.  For these, a frame or graphical
class of the application defines ->colours_changed.  This message is
sent by `display_manager ->colours_changed' to all frames and graphicals
whose class defines it, before all windows are redrawn:

\begin{code}
colours_changed(Menu) :->
    "Draw the icons again in the new colours"::
    send(Menu, paint_icons).
\end{code}

\paragraph{Colours chosen by the user}

A colour that is chosen by the user, for example the background of an
Epilog profile, is defined using adaptive_colour/3.  The theme colour
is the given colour if this is as dark or as light as a reference, for
example \const{ui_window_background} for a background colour.  If not,
the lightness of the colour is mirrored, keeping its hue.  A light
yellow background thus becomes a dark olive one in the dark theme.  It
is also possible to give a value per theme.  The Epilog profile options
\term{background}{Colour} and \term{foreground}{Colour} and the colours
of set_epilog/1 use this:

\begin{code}
epilog:profile(shell, [ background(lightgoldenrodyellow) ]).
epilog:profile(build, [ background([ light = lightgoldenrodyellow,
                                     dark  = '#33301e'
                                   ])
                      ]).
\end{code}

\paragraph{Predicates}

The library \pllib{pce_theme} provides the following predicates.  See
the documentation of the library for details.

\begin{description}
    \predicate{apply_theme}{1}{+Theme}
Make \arg{Theme} the active theme and redraw all windows.  This loads
\file{library(theme/}\arg{Theme}\file{)} if it exists.
    \predicate{select_theme}{1}{+Theme}
Select \arg{Theme} or, if \arg{Theme} is \const{system}, follow the
desktop for the running session.  This is used by the Settings/Theme
menu.
    \predicate{current_theme}{1}{-Theme}
\arg{Theme} is the active theme.
    \predicate{theme_colours}{1}{+List}
Declare the theme colours of an application.  \arg{List} is a list of
\arg{Name} \const{=} \arg{Value}, where \arg{Value} is the value in the
\const{light} theme.
    \predicate{adaptive_colour}{3}{+Name, +Spec, +Reference}
Define the theme colour \arg{Name} from a colour chosen by the user.
    \predicate{check_theme}{1}{+Theme}
Report problems with the colours defined by \arg{Theme}.
\end{description}

The console colours of a theme, prolog:console_color/2, only apply
while the theme is active.  Text that is already written keeps its
colours.


\subsubsection{Display colour depth}		\label{sec:colourdepth}

Displays differ in the number of colours they can display simultaneously
and whether this set can be changed or not. X11 defines 6 types of
\idx{visuals}.  Luckily, these days only three models are popular.

\index{colour,256}\index{colour,16-bits}\index{colour,true}%
\begin{itemlist}
    \item [8-bit colour-mapped]
This is that hard one.  It allows displaying 256 colours at the same
time.  Applications have to negotiate with each others and the windowing
systems which colours are used at any given moment.

It is hard to do this without some advice from the user. On the other
hand, this format is popular because it leads to good graphical
performance.

    \item [16-bit `high-colour']
This schema is a low-colour-resolution version of true-colour, normally
using 5-bit on the red and blue channels and 6 on the green channel. It
is not very good showing perfect colours, nor showing colour gradients.

It is as easy for the programmer as true-colour and still fairly
efficient in terms of memory.

    \item [24/32 bit true-colour]
This uses the full 8-bit resolution supported by most hardware on all
three channels.  The 32-bit version wastes one byte for each pixel,
achieving comfortable alignment.  Depending on the hardware, 32 bit
colour is sometimes much faster.
\end{itemlist}


We will further discuss 8-bit colour below.  As handling this is totally
different in X11 and MS-Windows we do this in two separate sections.


\subsubsection{Colour-mapped displays on MS-Windows}

In MS-Windows one has the choice to stick with the reserved 20 colours
of the system palette or use a colourmap (palette, called by Microsoft).

If an application chooses to use a colourmap switching to this
application causes the entire screen to be repainted using the
application's colourmap.  The idea is that the active application looks
perfect and the other applications look a little distorted as they have
to do their job using an imperfect colourmap.

By default, \product{} makes a \class{colour_map} that holds a copy of
the reserved colours.  As colours are required they are added to this
map.  This schema is suitable for applications using (small) icons and
solid colours for graphics.  When loading large colourful images the
colourmap will get very big and optimising its mapping to the display
slow and poor.  In this case it is a good idea to use a fixed colourmap.
See class \class{colour_map} for details.

When using \product{} with many full-colour images it is advised to use
high-colour or true-colour modes.


\subsubsection{Colour-mapped displays on X11/Unix}

X11 provides colourmap sharing between applications. This avoids the
flickering when changing applications, but limits the number of
available colours.  Even worse, depending on the other applications
there can be a large difference in available colours.  The alternative
is to use a \idx{private colourmap}, but unlike MS-Windows the other
applications appear in totally random colours.  \product{} does not
support the use of private colourmaps therefore.

In practice, it is strongly advised to run X11 in 16, 24 or 32 bit mode
when running multiple applications presenting colourful images. For
example \idx{Netscape} insists creating its own colourmap and starting
Netscape after another application has consumed too many colours will
simply fail.


\subsection{Supported Image Formats}

The table below illustrates the image format capabilities of each of the
platforms. Shape support means that the format can indicate {\em
transparent} areas. If such an image file is loaded, the resulting
\class{image} object will have an `image <-mask' associated: a
monochrome image of the same side that indicates where paint is to be
applied. This is required for defining cursors (see `cursor
->initialise') from a single image file. {\em Hotspot} means the format
can specify a location.  If a Hotspot is found, the `image <-hot_spot'
attribute is filled with it.  A Hotspot is necessary for cursors, but
can also be useful for other images.

\begin{center}
\index{XPM,file format}%
\index{ICO,file format}%
\index{CUR,file format}%
\index{XBM,file format}%
\index{JPEG,file format}%
\index{GIF,file format}%
\index{BMP,file format}%
\index{PNM,file format}%
\index{image,file formats}%
\index{image,shape}%
\index{cursor}%
\index{icon}%
\begin{tabular}{|l|ccccccc|}
\hline
\bf Format & \bf Colour & \bf HotSpot & \bf Shape &
	     \multicolumn{2}{c}{\bf Unix/X11} &
	     \multicolumn{2}{c|}{\bf Win32} \\
	   & &&&   load	  & save	      & load & save \\
\hline
\multicolumn{8}{|c|}{Icons, Cursors and shaped images} \\
\hline
XPM	   & +&+&+&  +	  &  +		      &   +  &  +   \\
ICO	   & +&-&+&  -	  &  -		      &   +  &  -   \\
CUR	   & +&+&+&  -	  &  -		      &   +  &  -   \\
\hline
\multicolumn{8}{|c|}{Rectangular monochrome images} \\
\hline
XBM	   & -&-&-&  +	  &  +		      &   +  &  -   \\
\hline
\multicolumn{8}{|c|}{Large rectangular images} \\
\hline
JPEG	   & +&-&-&  +	  &  +		      &   +  &  +   \\
GIF	   & +&-&+&  +	  &  +		      &   +  &  +   \\
BMP	   & +&-&-&  -	  &  -		      &   +  &  -   \\
PNM	   & +&-&-&  +	  &  +		      &   +  &  +   \\
\hline
\end{tabular}
\end{center}

The XPM format ({\bf X} {\bf P}ix{\bf M}ap) is the preferred format for
platform-independent storage of images that are used by the application
for cursors, icons and other nice pictures. The XPM format and
supporting libraries are actively developed as a contributed package to
X11.  


\subsubsection{Creating XPM files}

\paragraph{Unix} There are two basic ways to create XPM files. One is to
convert from another format. On Unix, there are two popular conversion
tools. The \program{xv} program is a good interactive tool for format
conversion and applying graphical operations to images.

ImageMagic can be found at \url{http://www.simplesystems.org/ImageMagick/}
and provides a comprehensive toolkit for converting images.

The \program{pixmap} program is a comprehensive icon editor, supporting
all of XPM's features.  The image tools mentioned here, as well as the
XPM library sources and a FAQ dealing with XPM related issues can be
found at \url{ftp://swi.psy.uva.nl/xpce/util/images/}


\paragraph{Windows}

\product{} supports the Windows native \fileext{ICO}, \fileext{CUR} and
\fileext{BMP} formats. Any editor, such as the resource editors that
comes with most C(++) development environments can be used. When
portability of the application becomes an issue, simply load the icons
into \product{}, and write them in the XPM format using the `image
->save' method. See the skeleton below:

\begin{code}
to_xpm(In, Out) :-
	new(I, image(In)),
	send(I, save, Out, xpm),
	free(I).
\end{code}

Note that the above mentioned ImageMagick toolkit is also available for
MS-Windows.

\subsubsection{Using Images}

Images in any of the formats are recognised by many of \product{}'s GUI
classes.  Table \tabref{imageusage} provides a brief list:

\begin{table}
\begin{center}
\begin{tabularlp}{`menu_item ->selection'}
\hline
\class{bitmap}		& A \class{bitmap} converts an image into a first
			  class \class{graphical} object that can be
			  displayed anywhere. \\
\hline
\class{cursor}		& A \class{cursor} may be created of an image 
			  that has a mask and hot-spot. \\
\hline
`frame ->icon'		& Sets the icon of the frame. The visual result
			  depends on the window system and X11 window
			  manager used.  Using the Windows 95 or NT 4.0
			  shell, the image is displayed in the task-bar
			  and top-left of the window. \\
\hline
`dialog_item ->label'	& The label of all subclasses of class
			  \class{dialog_item} can be an image. \\
\hline
`label ->selection'	& A \class{label} can have an image as its
			  visualisation. \\
\hline
`menu_item ->selection'	& The items of a menu can be an image. \\
`style ->icon'		& Allows association of images to lines in
			  a \class{list_browser}, as well as marking
			  \classs{fragment} in an \class{editor}. \\
\hline
\end{tabularlp}
\end{center}
\caption{GUI classes using \class{image} objects}
\label{tab:imageusage}
\end{table}





