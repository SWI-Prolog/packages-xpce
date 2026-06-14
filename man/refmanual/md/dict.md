# class dict {#class-dict}

A dict object (a dictionary) is a collection of dictionary items
(dict_item objects), each of which is a triple (key, label,
object).  The key must be unique over the dict.

Dict objects can be made visible by attaching them to a browser
object or list_browser object.  The browser can then be used to
select dict_item objects.

The access to a dict_item object from a key is implemented using a
hash_table object.

**Bugs**:

Class `dict` is very old and is based on some outdated ideas.  Currently
its primary use is to provide the storage facilities for a browser.

Its definition might be revised some day to be more in line with the
spirit of the current PCE system.  Notably try not to rely on ->format
and <-current.

@see class browser
@see class list_browser
@see class dict_item


## Class variables {#class-dict-classvars}

- dict.sort_ignore_blanks: bool = @off
    When @on, ignore leading and trailing blanks.  Map multiple blanks onto
    a single space.  See ->sort.

    @see dict.sort_ignore_case
    @see dict->sort

- dict.sort_ignore_case: bool = @off
    When @on, sort the dict case-insensitive on the `dict_item <-label`.

    @see dict<-find_prefix
    @see dict.sort_ignore_blanks
    @see dict->sort


## Instance variables {#class-dict-instvars}

- dict-browser: list_browser*
    When not @nil, this is the list_browser object that visualises this
    dictionary.  The slot `list_browser <-dict` contains the reverse
    pointer.

    When the dict is modified, it will send messages to the list_browser
    object:

    - `list_browser ->_delete_item: dict_item`
    	The indicated item has been deleted from the dict

    - `list_browser ->_insert_item: dict_item`
    	The indicated item has been inserted in the dict

    - `list_browser ->_clear`
    	The dict has been cleared.

    @see list_browser-dict
    @see class list_browser

- dict<-members: chain
    Chain with all the members.  Class dict stores its members twice: once
    in <-members to maintain ordering information (see ->sort and ->append)
    and once in the hash_table object <-table to get fast access from a key
    to a dict_item object.

    @see dict-table

- dict<-sort_by: [code]*
    The method <->sort_by defines the behaviour of ->insert as well
    as the default behaviour of ->sort.  If its value is @nil, no
    sorting is done.  ->insert and will behave as ->append.  Using
    @default, the arguments are sorted alphabetically on their
    printed representation.  ->insert will insert a new item
    according to this order.  If the <-sort_by contains a code
    object, this will be used for comparing the pairs of dict_item
    objects.  See also `chain ->sort`.  It <-sort_by is assigned
    another value than @nil, ->sort will be activated to establish
    the desired order.

    Using ->sort_by is a good technique to insert new items at the
    right place in the list.  It is not a good technique to build up
    a sorted list as this will result in quadratic behaviour for
    long lists, while ->sort uses the QuickSort algorithm.

- dict-table: hash_table*
    The mapping from `dict_item <-key` to the corresponding dict_item object
    is maintained by this hash_table object.


## Send methods {#class-dict-send}

- dict->append: item=dict_item
    Append the dict_item at the end (see <-members).  Note that class
    dict_item defines a conversion function to create a dict_item from a
    key.  See `dict_item <-convert`.

    See also ->insert_after and ->insert.

    **Bugs**:
    If there is already a dict_item with this `dict_item <-key`, the old
    entry is deleted from <-table.  A subsequent <-member wil this return
    the last appended dict_item with this <-key.

    @see dict->insert_after
    @see dict_item<-convert

- dict->clear
    Removes all dict_item objects from the dict.  Informs a possibly related
    <-browser.

    @see dict->delete

- dict->delete: any|dict_item
    Delete a dict_item object.  The dict_item to be deleted may be specified
    using its `dict_item <-key`.

    **Diagnostics**:
    Fails silently when the argument is not a member dict_item or and the
    key of a member dict_item.

    @see dict->clear

- dict->for_all: action=code, safe=[bool]
    Invokes `chain ->for_all` on the chain of <-members.

    @see dict<-find

- dict->for_some: action=code, safe=[bool]
    Invokes `chain ->for_some` on the chain of <-members.

    @see dict<-find

- dict->insert: item=dict_item
    Insert dict_item according to <-sort_by.  If <-sort_by is @nil,
    just ->append.  If <-sort_by is @default, insert the item before
    the first item that is alphabetically larger.  It <-sort_by
    contains a code object, insert it before the first item for
    which executing the code (if it is a function) return `larger`.

    See also `chain->sort`, ->insert_after, ->append, ->sort and
    ->sort_by.

- dict->insert_after: after=dict_item, item=any|dict_item*
    Add a dict_item object (like ->append) after the specified item (a
    dict_item or key).  If the second argument is @nil, the dict_item is
    inserted as the first item.

    @see dict->append

- dict->member: any|dict_item
    Test if the argument (a dict_item object or a `dict_item <-key`) is
    member of the is dict.

    **Bugs**: Redundant.  Use `dict <-member` instead.

    @see dict<-member

- dict->sort: [bool|code|function], ignore_blanks=[bool], reverse=[bool]
    Sort the contents of the dict.  The ordering information is kept in
    <-members.  There are two possibilities:

    - Alphabetical sorting
    	When the first argument is not a code object, the dict will be
    	sorted alphabetically on the `dict_item <-label`.   The first
    	argument requests to compare case-insensitive.  The second
    	to ignore leading and trailing blank space and to collapse
    	multiple blanks in the middle to a single space  character.
    	(see also `string ->strip`).

    - Sorting on a comparison function.
    	In this case the first argument is a code object.  This code
    	object is passed pairs of dict_item objects as @arg1 and @arg2.
    	See `chain ->sort` for details.

    If <-sort_by contains a code object, this will be used as the
    default.  See also <-sort_by to maintain a list of sorted
    objects.

    **Defaults**:
    	# case-insensitive:		Dict.sort_ignore_case
    - ignore blanks		Dict.sort_ignore_blanks

    @see dict.sort_ignore_case
    @see dict.sort_ignore_blanks


## Get methods {#class-dict-get}

- dict<-find: test=code -> dict_item
    Invokes `chain <-find` on the chain of <-members.

    @see dict<-find_all
    @see dict->for_some
    @see dict->for_all

- dict<-find_all: test=code -> chain
    Invokes `chain <-find_all` on the chain of <-members.

    @see dict<-match
    @see dict<-find

- dict<-find_prefix: for=string, from=[int], no_exact_case=[bool] -> dict_item
    Return the first dict_item object from <-members whose <-label (or <-key
    if the <-label is @default) has the specified prefix.  The search starts
    at the nth dict_item (0-based).  The last argument requests
    case-insensitive (@on) or case-sensitive (@off).

    **Diagnostics**:
    	# Start index:	0
    - Ignore case:	@off (match exact case).

    @see dict<-match
    @see dict.sort_ignore_case

- dict<-match: char_array -> chain
    Return a new string holding all dict_item objects in this dict whose
    `dict_item <-label` (or `dict_item <-key` when the label is @default)
    match (using `char_array ->sub`) the argument name.

    Note that more complicated searches may be performed using <-find_all.

    **Bugs**: Should allow for case-insensitive matching.

    @see dict<-find_all
    @see dict<-find_prefix
    @see dict<-member

- dict<-member: any|dict_item -> dict_item
    When the argument is a dict_item object, return it if `dict_item <-dict`
    equals this dict.  Otherwise, invoke `hash_table <-member` on <-table
    and return the result.

    Note that many of the methods of the class that require a member
    dict_item argument use <-member to find the dict_item.

    @see dict<-match

