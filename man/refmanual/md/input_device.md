# class input_device {#class-input-device}

An input_device describes the device that generated an event (see
`event<-device`).  Events from the mouse refer to @mouse_device and
keyboard events to @keyboard_device.  Pens, touch screens and gamepads
are represented by their own input_device, which is created when the
window system reports the device.  The same physical device is always
represented by the same object.  `display<-input_devices` lists the
known devices.

Applications normally only inspect devices, e.g., to distinguish pen
input from mouse input:

	get(@event, device, Device),
	get(Device, kind, pen)

@see event<-device
@see display<-input_devices
@see class event_detail


## Instance variables {#class-input-device-instvars}

- input_device<-kind: {mouse,pen,touch,gamepad,keyboard}
    The kind of device.

- input_device<-id: int
    Identifier of the device in the window system.  This is 0 for
    @mouse_device and @keyboard_device.

- input_device<-name: name
    Human readable name of the device.  If the window system does not
    provide a name, this is the <-kind.


## Send methods {#class-input-device-send}

- input_device->initialise: kind={mouse,pen,touch,gamepad,keyboard}, id=[int], name=[name]
    Create a device description.  Devices are normally created by
    xpce.  Creating one is only useful to create events for testing.
    Such a device is not added to `display<-input_devices`.
