# class resource {#class-resource}

Class resource forms and interface to access data needed to get
a running application that is not easily expressed in the
programs source-code.  Examples are graphics file, help-files,
etc.

The following example declares and uses a resource object to
access the bitmap 'pce.bm' from the standard bitmap library:

	resource(pce, image, image('pce.bm')).

		...
		new(I, image(resource(pce, image))),
		...

The resource/3 clause is used by the runtime-generation system
to attach the image to the runtime executable.  Class resource
is used by `image ->load` to access the resource-data, forming
the proper image object.


## Instance variables {#class-resource-instvars}

- resource<->class: [name]
    Class of the resource (@default: don't care).  Resource classes
    are intended to separate the resource name-space into multiple
    name-spaces dealing with objects of the same type.
    Resource-data for image objects is generally deneoted with class
    `image`.   See also <-context.

- resource<-context: any*
    *Inherits description from*: message-context

- resource<->name: name
    Name of the resource.  The resource-name is obligatory.  See
    also <-class .and <-context.


## Send methods {#class-resource-send}

- resource->access: mode={read,write,append,execute}
    Test if resource has access.  Succeeds if mode equals `read`,
    and ->exists succeeds.  See also `file->access`.

- resource->exists
    Test if resource exists.  See also ->access.

- resource->initialise: name=name, class=[name]
    Create a resource object from its <-name  and <-class.


## Get methods {#class-resource-get}

- resource<-convert: name=name -> resource
    Convert name into named resource.  See also `source_sink<-convert`.

