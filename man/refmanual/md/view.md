# class view {#class-view}

A view is a window-based version of an editor.  It is implemented as a
window that displays a single editor object and resizes this object if
the window is resized.  A view delegates to its editor object.

Inheritance precedes delegation in PCE's message passing primitives.
For many methods both defined on class window and class editor the
method defined on class editor describes the desired behaviour.
Most of the behaviour explicitly defined on class view overrules
methods inherited from window and passes them explicitly to the
related editor object.

@see class editor


## Instance variables {#class-view-instvars}

- view<-editor: editor
    Editor object displayed by the view.  A view delegates to this editor.
    Also, the view object is the `visual <-master` of the editor.  The
    displayed editor may be changed after the view has been created.  Note
    that an editor may also switch from displayed text using `editor
    ->text_buffer'.


## Send methods {#class-view-send}

- view->clear
    Remove all text from the editor.  See `editor ->clear`.

- view->format: char_array, any ...
    Format (as printf()) a string and insert it at the caret in the editor.
    See `editor ->format`.

- view->initialise: label=[name], size=[size], display=[display], editor=[editor]
    Create a view from the specified parameters.  The size is interpreted in
    character units.  The default is determined by view.size .  When
    no editor is specified, a default editor is created.  Using a
    user-defined editor is useful to create window versions for
    subclasses of class editor.  See also ->editor and
    <-create_editor.

- view->scroll_to: index=[int], screenline=[int]
    Overrule window behaviour.

- view->normalise: from=int, to=int
    Redefine window behaviour to call:

    	| `editor ->normalise` | Make range of characters visible |
    	| `editor ->scroll_to` | Scroll to character index        |

- view->selection: mark=[int], caret=[int], status=[{active,inactive,highlight}]
    Define the selected part of the text.  The implementation on
    class view invokes immediately the implementation on the
    associated <-editor.  See `editor->selection`.

    *Inherits description from*: view<-selected

- view->unlink
    Explicitly ->free's the related editor object, after which `window ->unlink`
    is invoked.


## Get methods {#class-view-get}

- view<-create_editor: size=[size] -> editor
    Called by ->initialise if no editor argument was specified.  May be
    redefined to create a suitable editor.

- view<-selection: -> point

- view<-selected: -> string
    Redefines the corresponding window behaviour and calls the selection
    mechanism of class editor.   See

    	| `editor <-selected`  | Get selected string             |
    	| `editor <-selection` | Point object with start and end |
    	| `editor ->selection` | Specify selection start end end |
