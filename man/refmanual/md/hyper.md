# class hyper {#class-hyper}

A hyper object is a named binary link between two objects.  Where
slot-references and attribute relations are uni-directed, a hyper may be
regarded a two-way `object ->attribute`.  Hypers are used for two
purposes:

- The creation of hyper-text networks

- An attribute is a useful instrument to register a `slave`
	object.  Because the value of the attribute does not know
	about the `main` object, hypers are a suitable solution for
	circumstances where this knowledge is required.

As an example of the second case, consider an application that creates a
secondary window to allow for editing some object of the main window.
If the user is allowed to work in both window and destroy both windows
separately, a hyper is a suitable way to maintain the relation between
both windows.

The most important behaviour dealing with hypers is defined on class
object:

	| `object ->send_hyper` | Broadcast a message over hypers |
	| `object <-get_hyper`  | Idem to get information         |
	| `object <-hypered`    | Find hyperlinks from object     |

@see class constraint
@see class interceptor
@see object<-all_hypers


## Instance variables {#class-hyper-instvars}

- hyper<->backward_name: name
    Hyper-name seen from the <-to object.  By default the name is the same
    in both directions.  See <-forward_name.

- hyper<->forward_name: name
    Name of the hyper as seen from object <-from.

- hyper<-from: object
    The variables <-from and <-to define the two objects related by the
    hyper-link.  `object <-hypered` may be used to find hyper-links from
    the related objects.

- hyper<-to: object
    *Inherits description from*: hyper-from


## Send methods {#class-hyper-send}

- hyper->_save_relation: file
    Consider saving relation (->save_in_file).  A hyper is saved
    only if both <-from and <-to are saved.

- hyper->initialise: from=object, to=object, forward=[name], backward=[name]
    Create a hyper-link from <-from to <-to.  Seen from <-from, the link is
    named <-forward_name, seen from <-to it is called <-backward_name.

    This method will invoke `object ->attach_hyper` on both objects to
    register the hyper.

- hyper->unlink
    Destroy the hyper object and disconnect it from the related objects
    using `object ->delete_hyper`.  See also ->unlink_from and
    ->unlink_to.

- hyper->unlink_to
    <-to side is being unlinked.

- hyper->unlink_from
    Called from the object-management system if the <-from
    (->unlink_from) or the <-to (->unlink_to) side of the hyper is
    being destroyed.  Both methods simply destroy the hyper object.

    These methods may be redefined, but always should destroy the
    hyper by calling free/1.  The example below defines a subclass
    between a `whole` and a `part`, where destruction of the whole
    automatically destroys the part.

    	:- pce_begin_class(part_hyper, hyper).

    	unlink_from(H) :->
    		get(H, to, Part),
    		free(Part),
    		free(H).

    	:- pce_end_class.

    A simple example using the code above:

    	?- send(new(V1, view(hello)), open),
    	   send(new(P1, picture(world)), open),
    	   new(_, part_hyper(V1, P1, part, whole)).

    	V1 = @435643
    	P1 = @465732

    Now the following call will delete both windows:

    	?- send(@435643, free).
