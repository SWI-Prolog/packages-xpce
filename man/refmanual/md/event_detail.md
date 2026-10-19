# class event_detail {#class-event-detail}

An event_detail holds data that is specific to the device that
generated an event, such as the pressure and tilt of a pen.  It is
available as `event<-detail`, which is @nil if the event has no such
data.  The event delegates to its detail, so the methods of the detail
can be called on the event itself.  Class event_detail itself only
defines the common super class of the classes for the various kinds of
events.

@see event<-detail
@see class drop_data
@see class key_data
@see class input_device
