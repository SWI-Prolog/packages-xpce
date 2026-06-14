\documentclass[11pt,twoside]{report}
%   pldoc.sty calls \setuphyperref if we predefine it; load hyperref
%   with the luatex driver so it doesn't expand pdftex-only primitives
%   like \pdfpagewidth (which recent LuaLaTeX no longer ships).
\def\setuphyperref{\usepackage[luatex,colorlinks=true,urlcolor=blue]{hyperref}}

%   luatex85 reintroduces pdfTeX primitive names that older style
%   files (pldoc.sty's \pdfcompresslevel, plpage.sty's page setup
%   macros, ...) still rely on. Must be loaded before any package
%   that uses them.
\RequirePackage{luatex85}

%   Loading order matters: sty_pldoc.pl and sty_xpce.pl both define
%   \class{...} for ltx2htm; whichever \usepackage{...} is loaded last
%   wins. Load pldoc first so the xpce variant (using sec:class-<name>
%   anchors that match the {#class-<name>} headings emitted by the
%   exporter) takes precedence.

\usepackage{pldoc}
\usepackage{html}
\usepackage{logo}
\usepackage{plpage}
\usepackage{xpce}
\usepackage{times}
\usepackage{fancyheadings}

\htmloutput{XPCERefMan}                 % Output directory
\htmlmainfile{index}                    % Main document file
\bodycolor{white}
\linkimage{home}{home.gif}
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
\newenvironment{xpceonly}{}{}
\excludecomment{pwonly}

%   fancyheadings + LuaLaTeX leaves \headwidth at an absurdly wide
%   default; pin it to the body text width so the chapter-heading
%   rule from fancychap.sty doesn't trigger a 32-inch overfull \hbox.
\setlength{\headwidth}{\textwidth}

\renewcommand{\runningtitle}{\product{} Reference Manual \productversion}
\title{XPCE Reference Manual}
\author{Generated from class .md files \\
        SWI-Prolog Solutions b.v.}
\date{\today}

\sloppy

\begin{document}

\maketitle
\tableofcontents

\input{chapters.tex}

\printindex

\end{document}
