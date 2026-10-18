# class drop_data {#class-drop-data}

A drop_data is the `event<-detail` of the drop_* events that the window
system generates when files or text are dragged onto a window from
another application.  As the event delegates to its detail, the methods
below are normally called on the event:

	drop_event(W, Ev) :-
	    send(Ev, is_a, drop_file),
	    get(Ev, path, Path),
	    ...

@see class event_detail
@see event<-detail


## Instance variables {#class-drop-data-instvars}

- drop_data-path: name*
    Dropped file of a drop_file event.

- drop_data-text: string*
    Dropped text of a drop_text event.

- drop_data-source: name*
    Name of the application that sent the drop, if known.


## Send methods {#class-drop-data-send}

- drop_data->initialise: path=[name], text=[string], source=[name]
    Create from the given values.  Omitted values are @nil.


## Get methods {#class-drop-data-get}

- drop_data<-path: -> name
    Dropped file.  Fails if the drop is not a file.

- drop_data<-text: -> string
    Dropped text.  Fails if the drop is not text.

- drop_data<-source: -> name
    Application that sent the drop.  Fails if this is unknown.
