# class directory {#class-directory}

A directory object is a wrapper around a directory on the
computers file-system. XPCE defines behaviour to handle files
and directories for two reasons.  First of all to support some
of its own facilities such as saving things to file or loading
things from file.  Secondly to allow the user to query the
file-system.  Normally the host provides similar facilities, but
notably for Prolog, not much of a standard exists.  Using XPCE's
primitives will make this part of your program better portable.

@see class file


## Instance variables {#class-directory-instvars}

- directory<-name: name
    Name of the directory.  This name is just the `file <-base_name`.
    <-path contains the full path name.

- directory<-path: name
    This variable contains the full path-name to this directory.  It is
    computed from the name argument when the directory is created.  See also
    `file <-absolute_path`.


## Send methods {#class-directory-send}

- directory->access: {read,write}
    Test if directory has access {read, write}.  If a directory has
    write access, new files and directories can be created in this
    directory.  See also ->exists.

    @see directory->exists
    @see file->access

- directory->cd
    Changes the working directory of the entire PCE/Prolog process to
    <-path.

    @see directory->push

- directory->exists
    Test if directory exists.  A new directory can be created using
    ->make.  A typical sequence to ensure the existence of a
    directory is:

    	...
    	new(D, directory(Path)),
    	(   send(D, exists)
    	->  true
    	;   send(D, make)
    	),
    	...

    See also ->access.

    @see directory->access

- directory->initialise: path=name
    Create a directory from the named path.  A leading ~ or ~<user> is
    expanded as are shell variables notated as $<name>.  <-path is filled
    with an absolute path to this directory, <-name with the basename of the
    directory.

    @see directory<-convert

- directory->make
    Create the OS counterpart (mkdir).  See also ->remove and
    ->exists.

- directory->modified
    Succeed if directory has changed since last call to ->modified.
    The first call just writes the current modification time in
    -modified and fails.  If the directory does not exist any
    longer, ->modified succeeds as well.

    This method is intended for periodic changes to the set of
    members in a visualised directory.  See also `file <-time`.

- directory->pop
    When @directory_stack is not empty this changes the working directory to
    the head of this chain and deletes the head of the chain.  Used in
    combination with `directory ->push` to temporary switch directories.

    @see directory->push

- directory->push
    Changes working directory like ->cd.  When successful, the old working
    directory is prepended to the chain @directory_stack as a name.  The
    method ->pop allows you to go back to the original directory.

    @see directory->pop
    @see directory->cd

- directory->remove
    Delete the OS counterpart.  This is equivalent to rmdir in both
    Unix and Windows.  Note that removing a directory is only
    possible if it is empty.  See also ->make and ->exists.

- directory->same: directory=directory
    Test if two paths refer to the same physical directory.  For
    details see `file->same`.

- directory->scan: files=chain*, directories=chain*, pattern=[regex], hidden_too=[bool]
    Scans the directory appending all files matching regex to the first
    chain and all directories to the second.  If the last argument is @on,
    files starting with a '.' are considered too.

    If the same chain is given for `files` and `directories`, ->scan will
    not determine the type.  Depending on your Unix version, this may
    be *much* faster.

    **Bugs**:
    Changes working directory while running.  This will not be undone when
    execution is aborted while scanning the directory.

    @see directory<-directories
    @see directory<-files


## Get methods {#class-directory-get}

- directory<-convert: name -> directory
    Convert a name to the corresponding directory object.  See
    `->initialise` for details.

    @see directory->initialise

- directory<-files: pattern=[regex], hidden_too=[bool] -> names=chain

- directory<-directories: pattern=[regex], hidden_too=[bool] -> names=chain
    Returns a chain with the names of all member-directories or -files of
    this directory that match the regular expression name (see class
    regex).  If pattern is omitted, all member-directories whose name does
    not start with a _._ are returned.  If hidden_too is @on, directories
    with names starting _._ are considered too.

    The names in the chain are alphabetically ordered.

    @see directory->scan
    @see directory<-files

- directory<-directory: name -> directory

- directory<-file: name -> file
    Return a file (or directory) object relative to the specified
    argument.  If `name` starts with a '/' or '~', these methods
    just return a new file or directory from `name`.  Otherwise
    the new object will be created from <-path, and `name`,
    separated with a '/'.

- directory<-file_name: name -> name
    Create path relative to directory.  If `name` denotes an
    absolute path it is returned unmodified.  See also <-file,
    <-directory and <-parent.

- directory<-parent: -> directory
    Returns a new directory object that points to the parent directory.
    There are two ways to find such a directory.  The first is to `pwd` on
    the entry _.._ and the second is to strip of the last part of the path
    by scanning for a '/'.  XPCE uses the latter approach.

    <-parent fails silently if the directory refers to the root of
    the file-system.

- directory<-roots: -> chain
    The method <-roots gathers the roots of the filesystem.  On
    Unix, there is only one root, so a chain holding only '/' is
    returned.  On windows, it returns the path-names of all
    defined logical drives.  This call is used by library(find_file) to
    show all defined drives after an `up` from a drive-root.

    The content of the directory object itself is not examined.  For
    example (Windows):

    	?- get(directory('.'), roots, Roots),
    	   chain_list(Roots, List).

    	List = ['a:/', 'c:/', 'd:/', 'e:/', 's:/']

