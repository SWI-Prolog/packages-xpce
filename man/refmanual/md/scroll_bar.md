# class scroll_bar {#class-scroll_bar}

A scroll_bar object allows the user to examine and modify the
position of a view --provided by some window-- onto a larger
object.  Scrollbars are normally attached to windows, editors
and list_browsers.  Scrollbars probably are rarely used by
normal PCE application programmers.  They may be used in
combination with class figure using `figure ->clip_area` to
create a `mini-window` (note that windows may also be displayed
on other window objects).

A scroll_bar assumes there is an object of a certain size and a
<-view on this object that allows the user to see part of it.
This part is defined by a <-start position and a <-length.  See
->bubble' A scroll_bar is a normal graphical object.  To connect
it to some other object, two communications should be
established:

- Object --> scroll_bar
	Each time the view or viewed object changes such that
	the scrollbar needs to be updated, the scroll_bar should
	be sent a ->request_compute message.  When XPCE's
	redraw system is activated, ->compute will be called to
	update the scrollbar.  See ->compute for the methods
	that need to be provided by the scrolled <-object to
	make this connection work.

- Scroll_bar --> object
	When the user activates the scroll_bar this information
	is passed to the <-object as follows:

	If <-message equals @default, invoke ->scroll_vertical
	on <-object if <-orientation equals vertical and
	->scroll_horizontal if <-orientation equals horizontal.
	This method is passed three arguments: `direction`,
	`unit` and `amount`.

	If `direction` equals `forwards`, the view should
	move further down/right the object.  _Unit_ may be
	`page`, in which case `amount` is the permillage
	of page the view should be moved or `line`, in which
	case `amount` is the number of lines to move.

	If `direction` equals `backwards`, the same parameters
	are passed, but the view should be moved up/left.

	If `direction` equals `goto`, `amount` is a permillage
	indicating an absolute position in the file: _0_ means
	goto the top/left, _1000_ to the bottom/right.

@see class slider
@see window->scroll_horizontal
@see scroll_bar->bubble
@see editor-scroll_bar


## Class variables {#class-scroll_bar-classvars}

- scroll_bar.repeat_interval: real = 0.06
    If the scroll_bar object has a UI for repeated scrolling, this
    class-variable determines the speed.  See also
    scroll_bar.repeat_delay.


## Instance variables {#class-scroll_bar-instvars}

- scroll_bar-direction: {forwards,backwards,goto}
    Direction in which to scroll or jump.

- scroll_bar-unit: {line,page,file}
    Unit to scroll.

- scroll_bar-amount: int
    These slots represent the scroll-request.  See also ->event and
    <-message.  The values are:

    - -unit
    	Scroll to a position in the file (document), per page or
    	line.  The interpretation of these units is left to the
    	scrolled object.

    - -direction
    	Scroll forwards (towards the bottom/right) or backwards
    	(towards the top/left) of the document.  If the unit is
    	`file`, -direction is `goto`.

    - -amount
    	For -unit is `file` or `page`, this is a promilage.
    	Thus, {file, goto, 300} goes to 30% of the file,
    	{page, forwards, 500} goes half a page down.
    	For -unit is line, this is a line-count.

- scroll_bar<-distance: int
    Distance between the scrollbar and the object.  The default is
    -1, so the lines of the scrollbar and object scrolled overlap.
    This variable is used by ->place.

- scroll_bar<-length: int
    *Inherits description from*: scroll_bar->bubble

- scroll_bar<-look: {win,gtk}
    Look-and-feel.  Currently only `x`, which implies compatible with the
    scrollbars from the Xaw library.  Future version should offer Motif and
    OpenLook compatible versions of the scrollbar.

    *Inherits description from*: dialog_item-look

- scroll_bar<->message: [code]*
    Message executed when the user operates the scrollbar.  See the
    description of class scroll_bar for the forwarded values.

- scroll_bar<->object: graphical*
    Object scrolled.  This object should understand the following messages

    	| <-start  | Start of the visible part  |
    	| <-view   | Length of the view part    |
    	| <-length | Total length of the object |

    The returned values are integers.  The unit is not important as a
    scroll_bar only considers the relative values.

    @see window->scroll_horizontal

- scroll_bar<-orientation: {horizontal,vertical}
    Determines the layout of the scroll_bar as well as the default messages
    sent (see description of class scroll_bar).  If this value is changed by
    the send_method, the <-width and <-height are swapped.

- scroll_bar<-placement: chain
    Name consisting of `left or right` and `top` or bottom',
    separated by spaces.  It determines where the scroll_bar is
    placed using ->place.

- scroll_bar<-start: int
    *Inherits description from*: scroll_bar->bubble

- scroll_bar<-view: int
    *Inherits description from*: scroll_bar->bubble


## Send methods {#class-scroll_bar-send}

- scroll_bar->length: int
    Total length of object.

- scroll_bar->start: int
    Start of visible part.

- scroll_bar->view: int
    Length of visible part.

- scroll_bar->bubble: length=int, start=int, view=int
    The three parameters <-length, <-start and <-view tell the
    scrollbar which part of the object scrolled is currently in the
    window:

    	| ->length | Total size of the object  |
    	| ->start  | Start of the visible part |
    	| ->view   | Size of the visible part  |

    The method ->bubble allows the user to change all three
    parameters with one message.  See also ->compute.

    @see class scroll_bar

- scroll_bar->compute
    If <-request_compute is not @nil, this method will:

    1. test if the <-object has the send method
    	->bubble_scroll_bar.  If this is the case, this method
    	is called with this scrollbar as argument.  The
    	receiving object is supposed to activate ->bubble,
    	passing the current viewport settings to the scrollbar.

    2. If the <-object has all the methods <-start, <-view
    	and <-length, the scrollbar will invoke them and update
    	the viewport using the result.  Backward compatibility
     		only.

    If the scrolled object or the viewport is modified, the window or
    <-object should invoke ->request_compute on the scroll_bar to activate
    this mechanism.

- scroll_bar->event: event
    Operate the scrollbar The exact definition depends on <-look.
    After user-interaction that should affect the scrolled <-object,
    the scroll_bar object determines -direction, -unit and
    -amount.  Next:

    - Invoke ->scroll_vertical: -direction, -unit,
    	-amount on the associated <-object if <-orientation
    	is vertical and ->scroll_horizontal otherwise.  If this
    	method is understood and succeeds the event is
    	considered handled. See, for example, `window->scroll_vertical`

    - Else, forward using the associated <-message.

- scroll_bar->initialise: object=object, orientation={horizontal,vertical}, message=[code]*
    Create a scroll_bar to scroll `object` either horizontally or
    vertically.   See also ->message.

- scroll_bar->place: [graphical]
    Position scrollbar relative to the given object.  If no object
    is given, <-object is used.  Exploits the variables

    	| <-placement | to place left/right, top/bottom.         |
    	| <-distance  | to determine the distance to the object. |

    The scrollbar is displayed on the same <-device as the
    given graphical.
