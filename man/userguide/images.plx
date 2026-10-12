\section{Using images and cursors}		\label{sec:images}

Many today graphical user interfaces extensively use (iconic) images.
There are many image formats, some for specific machines, some with
specific goals in mind, such as optimal compression at the loss of
accuracy or provide alternatives for different screen properties.

\product{} loads images using the SDL_image library and draws them using
Cairo.  As a result, the same image formats are supported on all
platforms, including the scalable SVG format.  See
\secref{imageformats}.


\subsection{Colour handling}			\label{sec:colour}

\index{colour,images}%
\product{} uses a simple colour model.  A \class{colour} is a 32-bit
RGBA value: 8 bits for each of the red, green and blue channels and 8
bits for the \jargon{alpha} channel that defines the opacity, where 255
is fully opaque.  The value is available as `colour <-rgba'.  Where a
colour is expected, it may be specified by name, such as \const{red}, or
in hexadecimal notation, \verb$'#RRGGBB'$ or \verb$'#RRGGBBAA'$.  A
colour can also be created from its components, e.g.,
\exam{colour(@default, 255, 0, 0, 128)}.  There are no colour maps:
all colours can be used at the same time.  Colours cannot be created
from images.  Use a \class{gradient} to fill an area with more than one
colour, or an \class{image} to fill it with a repeated pattern.  This topic is handled in the section on images for historical
reasons.


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
	& \const{windowBackgroundColor}$^*$
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
\const{sys_text_selection_background}
	& (derived)
	& \const{selectedTextBackgroundColor}
	& (derived)
	& (derived) \\
\const{sys_tooltip_background}
	& \const{COLOR_INFOBK}
	& \const{windowBackgroundColor}$^*$
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
    \item [Text selection]
\const{sys_text_selection_background} is the background of selected
text in, e.g., the editor and the terminal.  Selected text keeps its
colour, so syntax highlighting remains visible.  Except on MacOS it is
derived by composing 30\% of \const{sys_accent} over
\const{sys_window_background}.
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
Many of them are translucent.  The \const{mac_} colours are composed over
\const{mac_window_background} and the \const{sys_} colours over
\const{sys_dialog_background}, so all system colours are opaque.

$^*$Up to MacOS~15, \const{windowBackgroundColor} is a grey that differs
from the content background.  Since MacOS~26 both are the same, which
would make dialogs indistinguishable from content windows.  If they are
the same, \const{sys_dialog_background} is
\const{secondarySystemFillColor} composed over
\const{windowBackgroundColor} (\#ebebeb light, \#303030 dark).  This
is about the sidebar colour used on GNOME.

\const{disabledControlTextColor} is translucent and disabled text
appears on dialogs as well as on buttons.  \const{sys_inactive} is this
colour composed over either \const{sys_dialog_background} or
\const{sys_button_background}, whichever remains most visible on both.

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
\const{sys_text_selection_background} & 30\% accent & \#c2daf7 & \#243c5b \\
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

\paragraph{The theme editor}		\label{sec:themeeditor}

\index{theme editor}%
The theme editor creates and changes themes interactively.  It is
started from \emph{Settings/Theme/Edit theme \ldots} of the IDE tools
or using theme_editor/0 or theme_editor/1 from library
\pllib{pce_theme_editor}.  Without argument, it edits the active theme.
If this is a theme of the user, a file in the user configuration
directory, the editor edits it.  Otherwise, e.g., for the \const{light}
or \const{dark} theme that come with SWI-Prolog, it asks for the name of
a new theme that is based on the active theme (see
\secref{themevariations}).  \emph{New theme \ldots} creates a new theme
based on any of the available themes.

While editing, the theme exists in memory and is the active theme, so
all windows show the effect of every change immediately.  The theme
only becomes a file when it is saved.  The editor shows all
\jargon{settings} a theme can have in a list on the left:

\begin{itemlist}
    \item [Colours]
The theme colours (see below), grouped as \emph{User interface},
\emph{Terminal colours}, \emph{Development tools} and \emph{PceEmacs
syntax}.
    \item [Console]
The colours and style of the messages of the Prolog console by
message class, as defined by prolog:console_color/3.
    \item [Online help]
The styles of the online help (help/1) in the console, as defined by
the hook pldoc_style:theme/4.
\end{itemlist}

The menus above the list select a group and the settings that this
theme changes (\emph{Changed}) or that have an issue (\emph{Issues}).
The field next to them filters the list using a regular expression on
the name and description.  The status column shows a pencil for a
setting that the theme changes and a symbol for the most severe issue
of a colour: an error, a warning or information, e.g., the theme
defines a colour to the value it would have anyway.  The tooltip of a
row explains its status.

Selecting a setting shows it on the right.  For a colour, these are:

\begin{itemlist}
    \item [Value]
The \emph{Theme \ldots} button selects another theme colour, such that
the colour follows that colour.  \emph{Colour \ldots} opens the colour
editor, which shows a colour wheel, sliders for hue, saturation and
value and for red, green and blue, and the named colours closest to
the current colour.  Clicking a named colour uses it.  Every change is
shown in all windows immediately.  \emph{Ok} keeps the colour and
\emph{Cancel} restores the setting as it was.  \emph{Use inherited
value} removes the setting from the theme, such that it uses the value
of its base theme.
    \item [Derivation]
How the colour gets its value: the steps from the theme colour through
other theme colours to a system colour or an RGB value, and the theme
that defines each step.  Theme colours in the steps are links that
select that colour.
    \item [Used for]
Samples of the user interface that use the colour, such as a button, a
selected item of a list, a few lines of Prolog code with a syntax
colour or a terminal with a hyperlink, and the class variables whose
default refers to the colour.  Documented class variables link to their
description.  The samples change with the colour.
\end{itemlist}

The style of a console message class is edited using menus for the
foreground and background colour and toggles for bold, italic,
underline and faint, or by typing the list of attributes of
ansi_format/3.  \emph{Other \ldots} in the colour menus opens the
colour editor with the 256 colours of the terminal: selecting one of
these uses \term{fg8}{N} or \term{bg8}{N}, any other colour uses the
24-bit colour.  The styles of the online help are edited the same way,
but use the CSS properties of pldoc_style:theme/4.

\emph{Save} writes the theme to
\file{lib/theme/}\arg{Name}\file{.pl} in the user configuration
directory (see the Prolog file search path \const{user_app_config}),
which makes it available as any other theme.  The file only defines
the settings the theme changes and declares its base using
prolog:theme_base/2.  If the file exists, only the values that changed
are replaced and new settings are added after a similar one, such that
comments and the layout of the file are preserved.  \emph{Save as \ldots}
saves the theme under another name, \emph{Revert} undoes the changes
since it was saved and \emph{Edit theme file} opens the file in the
editor.  Closing the editor with unsaved changes asks to save them.
To use a saved theme in a new session, select it using the
\emph{Settings/Theme} menu, \exam{swipl -Dtheme=}\arg{Name} or
\const{display.theme} as described above.

\paragraph{Theme colours}

A \idx{theme colour} is an instance of class \class{theme_colour}, a
subclass of \class{colour}.  It is created from a name that describes
its role and the colour it is derived from, e.g.,
\exam{theme_colour(syntax_comment, dark_green)}.  This is the name of
another colour, which may be another theme colour or a system colour,
or a colour object.  The RGB value is computed when it is needed by
following `theme_colour <-derived_from' through other theme colours.
Changing what a theme colour is derived from using `theme_colour
->derived_from', or creating it again, makes all theme colours compute
their RGB value again on their next use.  Theme colours may therefore
refer to each other in any order.  Theme
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
replace the system colours (see below).  We call these the \jargon{roles}.
Other \const{ui_} colours are used for, e.g., incremental search
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

The roles, the \const{ui_}\arg{role} colours that have a system colour
\const{sys_}\arg{role}, are the system colours if the theme
\jargon{matches} the desktop.  This preserves the look of the desktop,
including colours the theme defines for the role.  If the theme does not
match, for example when using the dark theme on a light desktop, the
theme's own colours replace the system colours.  A role the theme does
not define remains the system colour.  All other theme colours, such as
\const{ui_isearch_background}, \const{ansi_}, \const{syntax_} and the
colours of the tools, always come from the theme.

The theme matches the desktop if both are light or both are dark.  The
theme is dark if its \const{ui_window_background} is dark and the
desktop is dark if \const{sys_window_background} is dark.  Only the
brightness is compared.  A theme that does not define
\const{ui_window_background} matches any desktop.  The \const{light}
theme provides its own light colours for use on a dark desktop.

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
use their value in the \const{light} theme, except for the roles: these
remain the system colour, also if the theme does not match the desktop.
A dark theme that does not define, e.g., \const{ui_tooltip_background}
thus uses the light tooltip colour of a light desktop.  The theme colours
of the roles are only used if the theme does not match the desktop.
The predicate check_theme/1 verifies a theme against the theme colours
of xpce and the development tools.  It reports theme colours that are
not defined, defined twice or unknown, and values that are not colours.
After changing a theme file, use make/0 and apply_theme/1 to see the
result.  A theme file may also define the colours of the Prolog console
using prolog:console_color/3, as \file{library(theme/dark)} does.

\paragraph{Variations of a theme}	\label{sec:themevariations}

A theme can be a variation of another theme, its \jargon{base}, using
the hook prolog:theme_base/2.  It uses the theme colours, console
colours and online help styles of its base that it does not define
itself.  A variation of the \const{light} or \const{dark} theme thus
defines only the colours it changes:

\begin{code}
:- module(prolog_theme_my_dark, []).
:- use_module(library(theme/dark)).
:- multifile
    prolog:theme/1,
    prolog:theme_base/2,
    pce_theme:colour/3.

prolog:theme(my_dark).
prolog:theme_base(my_dark, dark).

pce_theme:colour(my_dark, syntax_comment, '#6a9955').
\end{code}

The roles of a theme that is based on another theme always use the
value of the theme rather than the system colour.  Thus, if
\const{my_dark} defines \const{ui_window_background}, this colour is
also used on a dark desktop.  Roles that it inherits from \const{dark}
follow the desktop as described above.

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
Make \arg{Theme} the active theme and redraw all windows.  This sets
the Prolog flag \prologflag{theme}, which loads
\file{library(theme/}\arg{Theme}\file{)} if it exists.  Setting the flag
directly using set_prolog_flag/2 has the same effect.
    \predicate{select_theme}{1}{+Theme}
Select \arg{Theme} or, if \arg{Theme} is \const{system}, follow the
desktop for the running session.  This is used by the Settings/Theme
menu.
    \predicate{current_theme}{1}{-Theme}
\arg{Theme} is the active theme: the value of the Prolog flag
\prologflag{theme}, where \const{default} is \const{light}.
    \predicate{theme_colours}{1}{+List}
Declare the theme colours of an application.  \arg{List} is a list of
\arg{Name} \const{=} \arg{Value}, where \arg{Value} is the value in the
\const{light} theme.
    \predicate{adaptive_colour}{3}{+Name, +Spec, +Reference}
Define the theme colour \arg{Name} from a colour chosen by the user.
    \predicate{check_theme}{1}{+Theme}
Report problems with the colours defined by \arg{Theme}.
    \predicate{create_theme}{2}{+Theme, +Base}
Create \arg{Theme} in memory as a variation of \arg{Base}.  Its
settings are changed using set_theme_setting/2 and
reset_theme_setting/2.  This is used by the theme editor.
\end{description}

The console colours of a theme, prolog:console_color/3, follow the
active theme.  Text that is already written keeps its colours.


\subsection{Supported Image Formats}		\label{sec:imageformats}

\index{image,file formats}%
\index{PNG,file format}%
\index{SVG,file format}%
\index{JPEG,file format}%
\index{GIF,file format}%
\index{XPM,file format}%
\index{ICO,file format}%
\index{BMP,file format}%
\index{PNM,file format}%
Images are loaded using the SDL_image library.  The supported formats
thus depend on how this library was built.  Typically, these include
PNG, SVG, JPEG, GIF, WebP, AVIF, TIFF, BMP, ICO and CUR, PNM, TGA, QOI
and XPM.  The X11 bitmap format (XBM) is not supported.  Images can be
loaded from a file or any other \class{source_sink}, e.g., a
\class{resource} (see \secref{resources}).  The format is determined
from the content of the file.  The preferred formats for images that are
part of an application are PNG and SVG.

An SVG image is rendered when it is loaded.  If `image ->initialise'
specifies a width and/or height, it is rendered at this size, where a
missing dimension is computed from the aspect ratio.  Otherwise it is
rendered at the size specified in the SVG file.  For example:

\begin{code}
?- new(I, image('document.svg', 64)).
\end{code}

Each image is an array of RGBA pixels.  Transparent areas are defined by
the alpha channel.  The `image <-kind' is \const{pixmap} for a normal
image.  If all pixels of a loaded image are either opaque black or
transparent, the kind is \const{bitmap}.  When drawn, the black pixels of
a bitmap are painted in the current foreground colour, while the
transparent pixels remain transparent.  Such images can thus be used as
icons that follow the colour of, e.g., the text around them.

Images can be saved using `image ->save', which writes the image to a
file in PNG format.  This is the only format supported for saving.
Hot-spots stored in image files, e.g., in XPM or CUR files, are not
used: `image ->hot_spot' must be set explicitly.

\subsubsection{Using Images}

Images in any of the formats are recognised by many of \product{}'s GUI
classes.  Table \tabref{imageusage} provides a brief list:

\begin{table}
\begin{center}
\begin{tabularlp}{`dialog_item ->label'}
\hline
\class{bitmap}		& A \class{bitmap} converts an image into a first
			  class \class{graphical} object that can be
			  displayed anywhere. \\
\hline
\class{cursor}		& A \class{cursor} may be created from an
			  image and a hot-spot. \\
\hline
`dialog_item ->label'	& The label of all subclasses of class
			  \class{dialog_item} can be an image. \\
\hline
`label ->selection'	& A \class{label} can have an image as its
			  visualisation. \\
\hline
`menu_item ->label'	& The items of a menu can be an image. \\
\hline
`tab ->icon'		& An image shown in the label of a \class{tab}. \\
\hline
`style ->icon'		& Allows association of images to lines in
			  a \class{list_browser}, as well as marking
			  \classs{fragment} in an \class{editor}. \\
\hline
\end{tabularlp}
\end{center}
\caption{GUI classes using \class{image} objects}
\label{tab:imageusage}
\end{table}

\subsubsection{Cursors}			\label{sec:cursors}

\index{cursor}%
The mouse pointer of a graphical is set using `graphical ->cursor'.  A
\class{cursor} is either one of the standard cursors of the platform or
is created from an image.  The standard cursors are named by their role:
\const{default}, \const{pointer}, \const{text}, \const{crosshair},
\const{move}, \const{wait}, \const{progress}, \const{not_allowed} and
the resize cursors \const{ew_resize}, \const{ns_resize},
\const{nwse_resize}, \const{nesw_resize}, \const{n_resize},
\const{e_resize}, etc.  For compatibility, the most commonly used names
of the old X11 cursor font, such as \const{top_left_arrow},
\const{watch} or \const{xterm}, are mapped to the closest standard
cursor.  `cursor <-kind' is \const{standard}, \const{x11} or
\const{image} and `cursor <-summary' describes a standard cursor.  The
cursor demo shows all cursors.  A cursor is created from an image using
the image and its hot-spot, the point that is the actual pointer
location:

\begin{code}
?- new(C, cursor(@nil, image('my_cursor.png'), point(8,8))).
\end{code}





