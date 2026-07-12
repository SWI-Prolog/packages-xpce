# class date {#class-date}

Class date is a wrapper around Unix's notion of time.  It is intended
to communicate over creation, last-modification dates, etc.  Dates
might also be useful for agenda systems and systems that represent some
kind of database.  For example, this manual tool uses date objects to
keep track of the last-modification time of cards.

Unix time is measured in seconds from the Unix epoch: Jan 1, 1970.
Therefore, the granularity of a date object is a second.  The first date
that can be represented is Jan 1, 1970 and the latest date somewhere
in the year 2105 (2 to-the-power 32 - 1 seconds from the epoch).

The most important methods are:

- <-print_name and <-string
	which returns a textual representation of the date

- <-convert
	Convert a textual representation into a date object.

- ->current
	Update date object to reflect the current time.

- ->before, ->after, ->equal
	Comparison of dates

- <-compare
	Comparison of dates compatible with `chain ->sort`.

@see pce<-date
@see class timer
@see class file


## Send methods {#class-date-send}

- date->advance: with=int, unit=[{second,minute,hour,day,week}]
    Advance date object with the given amount of the specified unit.
    The default unit is `second`.  Please note that larger units are
    omitted due to their unclear semantics.   Units are converted to
    seconds.  Given that the fields are in local time, creating a
    time from fields, ->advance and get the fields may yield strange
    results.   For example:

    	?-	new(D, date(0, 0, 0, 30, 10, 2005)),
    		send(D, advance, 1, day),
    		get(D, string, string(Date))

    	Date = 'Sun Oct 30 23:00:00 2005'

    See also ->month, etc.

- date->after: date
    Test if date is after argument.  See also ->before and ->equal.

- date->before: date
    Test if date is before argument.  See also ->equal and ->after.

- date->convert: description=char_array
    Conversion of a textual representation of a date into a date object.
    These methods exploit the GNU-project get_date() library and convert
    various commonly used representations of a date.  Unfortunately this
    library is undocumented ...

- date->current
    Make the date object reflect the current time.

- date->equal: date
    Succeeds if both date objects represent the same point in time.
    See also ->before, ->after and <-difference.

- date->initialise: seconds=[0..59], minutes=[0..59], hours=[0..23], day=[1..31], month=[1..12], year=[1970..2050]
    Create a date object from its components.  Any defaulted value defaults
    to the current value.  Thus

    	?- new(D, date(0, 35, 14)).

    Represents 14.35h today.

    See ->set for details and limitations.

- date->posix_value: real
    Communicate the represented time-stamp as a POSIX time stamp,
    i.e. the number of seconds elapsed since the epoch, Jan 1, 1970.

    This value is passed as a float to avoid the limited range
    supported by the XPCE int type.

    *Inherits description from*: date<-posix_value

- date->set: seconds=[0..59], minutes=[0..59], hours=[0..23], day=[1..31], month=[1..12], year=[1970..2050]
    Set all non-default components.  This method is also used by
    ->initialise if arguments are provided.

    It uses the mktime() function which appears to have different
    behaviour on different systems on timestamps before the epoch
    at Jan. 1, 1970 UTC.  On some systems it returns negative
    values, while on others it returns the error code -1.  Note
    that even on systems with negative values the last second
    before the epoch cannot be represented.  An error is returned
    if mktime() returns -1.


## Get methods {#class-date-get}

- date<-compare: date -> {smaller,equal,larger}
    Compare to date objects and return one of smaller (before), equal (same
    time) or larger (after).  May be used by `chain ->sort`.  The following
    example sorts a chain of file objects according to their last
    modification time:

    	send(Files, sort,
    		 ?(@arg1?time, compare, @arg2?time))

- date<-convert: string -> date
    Convert a date from textual representation.  It uses the public
    domain getdate.y file converting many commonly used textual
    representations of date/time.

    Before using the getdate.y library it tries to convert the
    XML-Schema defined dateTime representation.  See also
    <-string, <-rfc_string and <-xml_string.

    *Inherits description from*: date->convert

- date<-difference: to=[date], unit=[{second,minute,hour,day,week,year}] -> units=int
    Difference relative to the argument date in the specified unit.  The
    value is always rounded downwards.  `to` defaults to the epoch.  units
    defaults to seconds.  Note that the result is returned as a PCE integer
    and the maximum value of a PCE integer is not sufficient to represent
    all time differences in second units.  An error (int_range) is generated
    on overflow.

    See also ->before, ->equal and ->after.

- date<-dst: -> bool
    `@on` if daylight-saving time is in effect at the represented instant,
    `@off` if standard time.  Fails if the system cannot determine this.

- date<-posix_value: -> real

- date<-print_name: -> string
    Equivalent to <-string.  The methods <-print_name and <-convert are used
    by class text_item to represent and modify objects.

    @see date<-string

- date<-rfc_string: -> string
    <-string in RFC 5322 compatible format, using a numeric timezone
    offset (e.g. `+0200`).  See also <-time_zone and <-xml_string.

- date<-string: -> string
    New string object representing date.  This method uses the C-library
    function ctime().   It's format is:

    	_Mon Sep 14 17:23:18 1992_

    See also <-rfc_string and <-xml_string for conversion to
    other popular representations.

    **Bugs**:
    There is no way to manipulate the printed representations of a date
    apart from redefining this method from scratch.

    @see date<-print_name
    @see pce<-date

- date<-time_zone: abbreviation=[bool] -> name
    Local timezone at the represented instant.  Returns the numeric offset
    (e.g. `+0200`, `-0330`, `+0530`) by default; with `@on` returns the
    zone abbreviation (e.g. `CEST`).  Fails if the platform does not
    provide the requested form.

- date<-xml_string: -> string
    <-string in the XML-Schema defined dateTime format.  The time is
    always represented in UTZ (ending with a Z).  See also
    <-rfc_string, <-string and <-convert.

    This format is defined as CCYY-MM-DDThh:mm:ss[Z|+-hh:mm].
    See the XML schema definition for details.

