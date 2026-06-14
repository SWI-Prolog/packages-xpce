# class source_location {#class-source_location}

A `source_location` denotes a location in a sourcefile.  They are
created by the compiler and used by this manual tool to find the sources
for classes and methods.

If possible, the filename must be absolute, so the sources can be found
regardless of the current directory.

A value @nil for the line_no implies the line number is not known.  The
Prolog defined class compiler uses the predicate source_location/2 to
find out about the location from which the last term presented to
term_expansion/2 was read.  The line-numbers for system defined classes
are determined via the EMACS tag-file TAGS in the PCE source directory.

@see method-source


## Get methods {#class-source_location-get}

- source_location<-path: -> name
    Used for the PCE sources itself.  If <-file_name is absolute, returns
    this value.  Otherwise prepend `pce <-home`/src/.

