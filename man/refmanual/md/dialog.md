# class dialog {#class-dialog}

A dialog is a window that is designed to be used for creating dialog
boxes.  A dialog performs automatic layout over its contents, thus
largely simplifying the definition of simple dialog windows and the
automatic generation of dialog windows.

Dialog windows normally only display instances of subclasses of class
dialog_item.  These are added to the dialog using the `dialog ->append`
method rather than `window ->display`.  `dialog ->append` places the
item relative to the other items in the dialog.  When the frame in which
the dialog resides is created, the final positions are automatically
calculated by `dialog ->layout`.

Dialog windows may be operated in two modes: each change to an
item in the dialog immediately sends a message or pressing an `apply`
button will apply all modifications made to the dialog.  The example
below illustrates a dialog of the latter type for editing some window
attributes:

	dialog(W) :-
		new(D, dialog('Edit Window Attributes')),
		send(D, append, label(reporter)),
		send(D, append,
		     text_item(frame_label,
			       W?frame?label,
			       message(W?frame, label, @arg1))),
		send(D, append,
		     slider(width, 50, 500, W?visible?width,
				    message(W, width, @arg1))),
		send(D, append,
		     slider(height, 50, 500, W?visible?height,
				    message(W, height, @arg1))),
		send(D, append, button(apply)),
		send(D, append, button(restore)),
		send(D, append, button(quit, message(D, destroy))),
		send(D, default_button, apply),
		send(D, open).

	?- new(@w, window),
	     send(@w, open),
	     dialog(@w).

This dialog may be converted into an `immediate` changing dialog by
deleting the apply button.  See ->apply, ->modified_item and ->restore
for details.

@see class dialog_item


## Instance variables {#class-dialog-instvars}

- dialog<->border: [size]
    Free area around contents.  If not specified this is the same as
    <-gap, the distance between items.

- dialog->gap: size
    @see dialog->layout

- dialog-size_given: {none,width,height,both}
    Set by ->size, ->width and ->height.  Used by ->_compute_desired_size
    to determine the dimensions that should not be altered.  Values:

    - none
	->_compute_desired_size sets <-width and <-height.

    - width
	->_compute_desired_size only sets <-width

    - height
	->_compute_desired_size only sets <-height

    - both
	->_compute_desired_size does not modify dimensions.


## Send methods {#class-dialog-send}

- dialog->_compute_desired_size
    This method is normally called from `frame->fit`.  It invokes the
    automatic ->layout, assigns the ->caret to the first text_item and
    (depending on -size_given) computes the desired dimensions of
    the dialog window.

    @see frame->fit
    @see dialog->layout

- dialog->active: bool
    *Inherits description from*: dialog<-active

- dialog->append: item=graphical, relative_to_last=[{below,right,next_row}]
    Append an instance of class dialog_item or a plain graphical
    object to the dialog object.  The appended item is placed
    according to `relative_to_last`:

	| below    | below the last one                       |
	| right    | right of the last one                    |
	| next_row | below the left-most item of previous row |

    If relative_to_last is omitted, the following defaults apply: if
    the last item *and* this item are instances of class button, the
    item is placed `right`.  Otherwise the item is placed
    `next_row`.

    @see dialog->layout
    @see dialog->display

- dialog->apply: always=[bool]
    Invoke the `dialog_item <-message` for each of the dialog_item's in
    <-graphicals.  If `always` equals @on the message will be invoked
    regardless whether the user has modified the item or not.  If `always`
    equals @off or @default the message will only be invoked if
    `dialog_item <-modified` yields @on.  If the dialog has a
    <-default_button, execute `button->active: @off`.

    See also ->restore.

- dialog->assign_accelerators
    Assign accelerators for the dialog_item objects.  This method
    collects the labels of all member <-graphicals, computes a
    set of accelerators to use for them and finally uses
    `dialog_item->accelerator` to assign them.

    See also `menu->assign_accelerators` and `dialog->assign_accelerators`.

- dialog->caret: member:graphical
    Assign the caret to the named text_item object.   See also ->advance and
    `text_item <-advance`.

- dialog->default_button: member:button*
    *Inherits description from*: dialog<-default_button

- dialog->display: graphical, at=[point]
    Display a graphical or dialog_item on a fixes position rather than
    exploiting automatic layout.  Equivalent to `device->display`, but
    invokes

	`dialog_item ->auto_align: @off`

    To inform ->layout not to move this object.

    @see dialog->append

- dialog->initialise: label=[name], size=[size], display=[display]
    Create an empty dialog window.  After creation it is normal to display
    instances of class dialog_item in the dialog using ->append.

- dialog->layout: size=[size]
    Align the dialog_item objects in the dialog.  This method is normally
    called from `dialog ->_compute_desired_size` when then dialog's
    frame is created.  See also `frame ->fit`.

    The layout process consists of the following stages:

    1. Place all items for which `dialog_item <-auto_align: @on` in
	a two-dimensional grid.

    2. Automatically align labels and values of items placed above
       each other depending on `dialog_item <-auto_label_align`,
       `dialog_item <-label_width`,  `dialog_item <-auto_value_align`
       and `dialog_item <-value_width`

    3. Items that have `dialog_item <-alignment` *not*
	`column`, but one of `left`, `center` or `right` are not
	horizontally with items above or below them, but
	combined with items to their left and right into
	sequences of items with the same alignment.  At the end
	of the layout process aligned such sequences are aligned
	in the available space.  Notably button objects have
	`button <-alignment: center`, so a row of buttons is
	normally centered in the dialog window.

    4. Compute the cell-size of the matrix and align the
       `dialog_item <-reference` in the two-dimensional grid.

    @see dialog_item-placed
    @see dialog_item-left
    @see dialog_item-label_width
    @see dialog_item-below
    @see dialog_item-auto_value_align
    @see dialog_item-auto_label_align
    @see dialog_item-align_in_column
    @see dialog_item-above
    @see dialog->_compute_desired_size
    @see dialog->append
    @see dialog-gap
    @see class dialog_item

- dialog->modified_item: item=graphical, modified=bool
    This message is normally invoked by one of the member dialog_item
    objects.  If the dialog has a <-default_button, this button will be
    activated using `button ->active: @on`.  Otherwise, this method
    fails.  Dialog-items will send their `dialog_item <-message`
    immediately if this message fails.  Otherwise they assume the
    default button will activate `dialog ->apply`.

    See also `dialog_item ->modified`, ->apply and ->restore.

- dialog->restore
    Restores all items in the dialog to their default state by invoking
    `dialog_item ->restore` to all <-graphicals.  If the dialog has
    a <-default_button, this is send `button->active: @off`.

    See also `dialog_item ->restore`, `dialog_item <-default` and
    `dialog_item ->modified`, `dialog_item ->apply` and ->apply.

- dialog->size: size
    Set the size of the window as `window->size` and sets -size_given to
    both to avoid overruling of the given size by the automatic
    layout system invoked by ->_compute_desired_size.  See also
    ->width and ->height

- dialog->height: 0..
    Give dialog an explicit height.

- dialog->width: 0..
    Defines the width of the dialog window.  If ->_compute_desired_size
    computes the size on the basis of the dialog's contents, the
    <-width will not be changed after this message.  Sets
    -size_given to `width` or `both`.  See also ->height and ->size.


## Get methods {#class-dialog-get}

- dialog<-active: -> bool
    Equivalent to `window <->sensitive`.  Both for consistency with
    class dialog_item and for backward compatibility.

- dialog<-border: -> size
    If the <-slot -border is not @default return it, else return
    <-gap.

- dialog<-default_button: -> button
    The default window of a dialog window is the button that has accelerator
    _RET_.  There is no default default_button.  See also ->modified_item
    and ->apply.

- dialog<-report_to: -> graphical|frame
    If there is a <-member dialog_item called `reporter`, this item is
    returned.   Otherwise `visual <-report_to` is invoked.

    This method is normally exploited by displaying a label object with
    name `reporter` on the dialog object.  This label will then display all
    error messages raised from operating the dialog item.  See
    `visual ->report`, `object ->report` and class error.

    @see visual<-report_to
    @see label->report

