# class file {#class-file}

A file object is a wrapper around a Unix file.  Files are used
internally for the various disk read and write operations supported by
PCE.

File objects can be used to query the status of a Unix file, read/write
from/to it, rename it, remove it, etc.

A file is represented by its <-name, which may hold the Unix
constructs _~_, _~loginname_ and $var.  A file can be searched
in a `search-path` using ->find.  It may be translated to an
absolute filename using ->absolute_path, which writes the
variable <-path after successful translation.

Class file defines the `class->has_feature`s listed below.
These features are defined for the various MS-Windows
based versions of XPCE.  If XPCE is running on any of
the MS-Windows based platforms, case_sensitive yields
@off.  case_preserving is @on on -NT and '95 and @off
on win32s.  8plus3names is @on on win32s and @off on
the other systems.  All Unix versions are case_sensitive,
case_preserving have 8plus3names set to @off.

	case_sensitive		bool
	case_preserving		bool
	8plus3names			bool

See also class directory.

@see class directory
@see class date


## Instance variables {#class-file-instvars}

- file<->bom: [bool]
    BOM (Byte Order Mark) handling of the file.  Initial @default.
    If the file is opened for reading in text-mode and <-bom is
    not @off, the file is checked for a UNICODE BOM signature.
    If found, <-bom is set to @on and the <-encoding is set
    accordingly.

    If the file is opened for writing and <-bom = @on and the
    encoding is a UNICODE encoding, a BOM marker is written
    to the file.

    Note that this default behaviour returns consistent results
    on the sequence below.  If a BOM is detected on reading,
    one will be added on writing the file and the encoding of the
    file will not be changed.

    	send(View, load, File),
    	...
    	send(View, save).

- file-fd: alien:FILE *
    Pointer to the C stream associated to this file.

- file<->filter: command=name*
    Name of the filter used as input- or output filter.  See ->open for
    details.

    @see file->open

- file<-name: name=name
    Name of the (Unix) file.  Filled from the first argument of
    ->initialise .  When the name is changed and the file already
    exists, the Unix file is renamed accordingly.  ->name cannot
    move files across file-systems.

    @see file<-base_name
    @see file<-directory_name
    @see file->name

- file<->newline_mode: {posix,dos,detect}
    Defines newline representation similar to SWI-Prolog's newline
    property on streams.   In read mode, posix read untranslated.
    Both dos and detect delete \r from the input.  The detect mode
    sets the mode of the file to dos or posix.  In write mode posix
    does not perform translation, while dos emits \n as \r\n.

    BUG: detection is only provided by `text_buffer->insert_file`.

- file<->path: path=[name]
    This variable holds the full path name of the file after
    ->absolute_path or ->find.

    The Unix file opened is the name of this variable when
    not @default and  <-name after expansion of _~_ and
    $var constructs if this variable holds @default

- file<-status: {closed,read,write,tmp_write}
    Current status of the file:

    - closed
    	Currently not open (-fd is 0).  See ->close.

    - read
    	Currently open for reading.  See ->open.

    - write
    	Currently open for writing.  May be opened `write` or
    	`append`.  See ->open.


## Send methods {#class-file-send}

- file->absolute_path
    Determine the absolute-path described by <-name and store the
    result in <-path.  First expands _~_ and $var constructs.

- file->access: mode={read,write,append,execute}
    Test if the file has specified access.  Access `write` and `append` have
    the same meaning, but they allow programmers to test the access with the
    same argument as passed to ->open.

    **Diagnostics**: Fails silently when the user has not the tested access.

    @see file->exists
    @see file->open
    @see directory->access

- file->append: text=char_array
    Append the argument text to the file.  The argument text is not
    interpreted.   See also ->format.

    @see file->format

- file->backup: extension=[name]
    If the file ->exists, it is copied to a file named <-name concatenated
    with `extension` (default _~_).  The file object itself is not affected.

    This method may be used in combination with ->open: write or
    `object ->save_in_file`.

    	save_diagram(Picture, File) :-
    		send(File, backup),
    		send(Picture?graphicals, save_in_file, File).

    @see !backup_file

- file->close
    Close file when open.  Flushes possible pending output. Succeeds without
    side-effects if the file is not opened.

    @see file->open

- file->copy: from=file
    Copy the contents of the argument-file to this file.  Argument
    ordering may look strange, but is consistent to all other ->copy
    operations in XPCE.  The example below defines an obvious
    Prolog predicate using this method.

    	copy_file(From, To) :-
    		send(file(To), copy, from).

    **Bugs**: Does not yet copy the mode attributes.

- file->exists: must_be_file=[bool]
    Succeeds if the file exists.  Note that this does not imply the user has
    read or write permission for this file.  Use ->access to obtain this
    information.

    If must_be_file equals @on or @default this test only succeeds if the
    file exists *and* is a regular file or a link to a regular file.  If
    must_be_file equals @off this test succeeds if there is an entry in the
    file-system with this name regardless of the type of the object there.

    **Diagnostics**: Always fails silently if the file could not be located.

    @see file->access

- file->find: path=[char_array], access=[{read,write,append,execute}]
    Find a file in the specified search-path that has the specified access.
    The path is a colon (:) separated list of directories.

    When <-name does not start with a '.' or '/', ->find will loop through
    the directories of the path, append the <-name of the file and check
    whether the resulting file has ->access.  When successful, it will set
    ->name to the full pathname of the file.

    This method is used by `image ->load` and `bitmap ->load` to locate
    bitmap files.

    The example below is a PCE/Prolog implementation of the Unix utility
    which for locating executable files.

    	which(Name, File) :-
    		get(@pce, environment_variable, 'PATH', Path),
    		new(F, file(Name)),
    		send(F, find, Path, execute),
    		get(F, absolute_path, File).

    **Defaults**: The default access is `read`.  The default path is '.'

    **Diagnostics**: Fails silently if the file cannot be found in the provided path.

- file->flush
    Output to a file is normally buffered.  The buffer is automatically
    emptied by ->close.   If the buffer is to be written to file without
    closing the file, use ->flush.

- file->format: format=char_array, argument=any ...
    Create a string by formatting the arguments and ->append the result to
    the file.  See `string ->format` for a specification of the format.

    **Bugs**:
    If the total output exceeds FORMATSIZE (currently 10000) characters, PCE
    crashes.

    @see file->newline
    @see file->append

- file->initialise: path=[name], encoding=[{text,binary,iso_latin_1,utf8,unicode_be,unicode_le}]
    Create a file from a specification.  The given `path` is expanded using
    the following rules:

    	| ~user | Expands the the home directory of `user`     |
    	| ~     | Expands the the current user's home ($HOME)  |
    	| $var  | Expands to `pce <-environment_variable: var` |

    `pce <-environment_variable` is an interface to Unix getenv,
    but the variable PCEHOME is expanded to PCE's home directory
    even if it does not exists in the Unix environment.

    This class may be used to create a *temporary* file by omiting
    `path`.  On systems providing mkstemp(), this function is used
    to generate a unique file in $TMPDIR or /tmp that is immediately
    opened for write and not accessible for other users.  For
    compatibility, the file object gets the status `tmp_write`.  A
    subsequent ->open in mode `write` or `append` simply sets
    the status to `write`.  On other systems, tmpnam() is used to
    generate the filename.  The mkstemp() approach is preferred
    for security reasons as it avoids race-conditions and access
    by others than the current user.

    The encoding argument specifies whether the file contains an
    octed stream or a stream of character codes.  See
    `source_sink->encoding` for details on character encoding
    issues.

- file->is_absolute
    Test if <-name specifies an absolute path.  First variables
    and ~ in the name specification is expanded.  Next, for
    Win32 versions the path is considered absolute if it starts
    with <letter>:.  For Unix systems if it starts with '/'.

- file->name: name=name
    @see file-name

- file->newline
    Equivalent to ->format: '\n'.

    @see file->format

- file->open: mode={read,write,append}, filter=[name], extension=[char_array]
    Open the file for reading or writing.  Mode `append` is like `write`,
    but does not truncate the file to zero characters.

    If a `filter` is specified, ->open will not open the file itself,
    but read or write the file through the filter.  The argument is the Unix
    command string.  Example:

    	?- new(F, file('data.Z')),
    	   send(F, open, read, uncompress).

    This example opens the file data.Z using the Unix command

    	`uncompress < data.Z`

    and reads the output of this command.

    When extension is specified, ->open appends the extension to <-name
    before passing the name to Unix.  This implies the file above could also
    have been opened using:

    	?- new(F, file(data)),
    	   send(F, open, read, uncompress, '.Z').

    Class process provides another way to interact with Unix processes.

    @see class process
    @see file-filter
    @see file->close
    @see file->access

- file->remove
    Remove the file from the disk.  The file is first closed using ->close.

    **Diagnostics**:
    Fails silently if the file did not exist.  Fails with an error message
    if the file is still present after this operation.

- file->same: file=file
    Test if two paths refer to the same physical file.  Both the
    reason and implementation of this call is highly system
    dependent:

    - Unix
    	Multiple paths may refer to the same phyical objects
    	due to hard- and soft links.  Equivalence testing is
    	achieved comparing the the inode and device identifier
    	as returned by the stat() call.

    - WIndows
    	Without links life looks easier, but isn't.  Windows
    	files are case-insensitive, on "8+3" filesystems the
    	names are truncated and on VFAT/FAT32 systems
    	both the long and short version exist.

    	The handling of long and short filenames is a bit
    	buggy.  If someone knows an efficient and correct
    	solution to test equivalence, please let us know.

    See also `directory->same`.

- file->seek: byte=int, from=[{start,here,end}]
    Moves the file-pointer to the indicated location.  The argument is
    relative to the start of the file, the current position (<-index) or the
    end of the file. The file must be open and must be a regular file not
    opened through a <-filter.

    @see file<-index


## Get methods {#class-file-get}

- file<-base_name: -> name
    Returns <-name without the possible directory specification:

    	?- new(F, file('/staff/jan/src/xpce/src/unx-file.c')),
    	   get(F, base_name, Base).

    	F = @634787
    	Base = 'unx-file.c'

    @see file<-directory_name
    @see file-name

- file<-character: -> char
    Reads the next character and returns it as an integer between 0 and 255.

    **Diagnostics**:
    Fails with message on I/O error.  Fails silently on reaching
    end-of-file.

- file<-convert: path=name -> file
    Converts a name into a file with that name.  See ->initialise.

- file<-directory_name: -> name
    Returns a name representing the directory name for this file.  Computed
    by deleting all text after the last '/' including this '/'.

    @see file<-base_name
    @see file-name

- file<-index: -> byte=int
    Returns the current value of the read- or write-pointer.
    This pointer may be set using ->seek.

    **Diagnostics**: Gives a warning if the file is not opened.

    @see file->seek

- file<-read: count=[int] -> string
    Returns a new string object with the next <int> characters or as many as
    there are left in the file.

    **Defaults**: The default amount to read is the entire file.

    **Diagnostics**: Standard file diagnostics.

    **Bugs**:
    Unlike Unix read(), this function cannot deal with files holding 0 (EOS)
    characters as strings cannot represent such characters.

    @see file<-read_line

- file<-read_line: -> string
    Reads the next line of input from the file and returns this in a string
    object.  The string contains the trailing newline character of the line.
    See `string ->strip`.  If the last line is not closed with a newline, a
    string *without* trailing newline is returned.

    **Diagnostics**:
    Fails with error message on file errors.  Fails silently when the end of
    the file is reached.

    @see file<-read

- file<-size: -> bytes=int
    Returns the number of characters in the file.  The file needs not be
    opened.

    @see file<-time

- file<-time: which_time=[{modified,access}] -> date=date
    Returns a new date object holding the last access or modification time.
    Date objects may be converted to a string (`date <-string`) or compared.

    See also `directory ->modified`.

    **Defaults**: By default returns the time of last modification.

    @see file<-size

