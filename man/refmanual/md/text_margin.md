# class text_margin {#class-text_margin}

A text_margin object may be attached to an editor object using the
`editor ->margin_width` method.  The margin displays image objects for
fragment objects defined on the text_buffer object associated with the
editor and thus provides a way for annotating textual information.

Class text_margin is not generally usable; it's functionality is
closely related to class editor.  The user should not explicitly
create instances of this class.

@see class style
@see style-icon
@see editor->margin_width
@see editor-margin
@see class fragment


## Instance variables {#class-text_margin-instvars}

- text_margin<-editor: editor
    Editor I'm part of.  Should be the same as <->device.


## Send methods {#class-text_margin-send}

- text_margin->initialise: editor=editor, width=int, height=int
    Create from editor and dimensions.  The user should create and attach a
    text_margin object using `editor ->margin_width`.


## Get methods {#class-text_margin-get}

- text_margin<-fragment: event -> fragment
    Find a fragment (-icon) from an event object.  This method may be used
    to attach event-handling to the icons displayed on the margin.  For
    example, the following recogniser will select a fragment by clicking on
    the icon:

    	send(Editor?margin, recogniser,
    		 click_gesture(left, '', single,
    					   message(Editor, selected_fragment,
    							   ?(@event?receiver, fragment,
    								 @event)))).

