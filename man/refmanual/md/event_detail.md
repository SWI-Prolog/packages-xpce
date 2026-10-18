# class event_detail {#class-event-detail}

An event_detail holds data that is specific to the device that
generated an event, such as the pressure and tilt of a pen.  It is
available as `event<-detail`, which is @nil if the event has no such
data.  Class event_detail itself only defines the common super class
of the classes for the various devices.

@see event<-detail
@see class input_device
