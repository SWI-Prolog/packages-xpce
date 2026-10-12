\section{Library ``find_file''}		\label{sec:findfile}

\index{file,prompting for}%
The library \pllib{find_file} defines the class \class{finder},
representing a modal dialog window for entering a filename. This class
defines the method `finder <-file':

\begin{description}
    \getmethod{finder}{file}{Exists:[bool|\{open,save\}],
			     Ext:[name|chain|tuple],
			     Dir:[directory], Def:[name]}{name}
Ask the user for a file.  If \arg{Exists} is @on or \const{open}, only
existing files can be returned.  The value \const{save} (or @off)
prompts for a file to write.  \arg{Ext} is either an atom denoting a single
extension or a chain of allowed extensions.  Extensions can be
specified with or without the leading dot.  I.e.\ both \const{pl}
and \const{'.pl'} are valid specifications of files ending in
\const{'.pl'}.  \arg{Dir} is the directory to start from.  When omitted,
this is the last directory visited by this instance of \class{finder}
or the current working directory.  \arg{Def} is the default file-name
returned.  When omitted there is no default.

Below is the typical declaration and usage.  In this example we ask
for an existing Prolog file.

\begin{code}
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

	...,
	get(@finder, file, @on, pl, PlFile),
	...,
\end{code}
\end{description}

\index{file,OS file dialog}%
The class \class{finder} always uses the portable \product{} dialog
defined by the class \class{find_file_dialog} (library
\pllib{find_file_dialog}).  Applications that prefer the native file
dialog of the operating system can use `frame<-open_file' and
`frame<-save_file'.  These methods are realised using SDL3 and block
until the user has selected a file or cancelled the dialog (in which
case they fail).  The filters are a \class{chain} of \class{tuple}
objects holding a label and an extension or chain of extensions.  For
example:

\begin{code}
	...,
	get(Frame, open_file,
	    filters := chain(tuple('Prolog files', chain(pl, prolog))),
	    File),
	...,
\end{code}

`frame<-open_file' accepts \const{allow_many := @on}, in which case it
returns a \class{chain} of file names.

Names for the extensions (as `Prolog file' rather than '*.pl') can be
defined by extending the multifile predicate pce_finder:file_type/2.  See
the library source for the standard definition.
