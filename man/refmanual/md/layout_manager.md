# class layout_manager {#class-layout_manager}

A `layout_manager` is the object responsible for placing the
graphicals on a `device`.  The design mirrors `recogniser`: part of
the device's responsibilities (here, geometry) is delegated to an
associated object.

A layout_manager is attached to a device via
`device->layout_manager`.  The device asks it to recompute the
layout and to paint any background.  Conversely, the manager assigns
each managed graphical a `layout_interface` (via
`graphical ->layout_interface`) that intercepts geometry changes
and propagates them back so the manager knows when to recompute.

Class `layout_manager` itself is an almost-empty abstract base.
The first concrete realisation is `table`, implementing HTML-3-like
tables.  See also `layout_interface`.

@see class device
@see class layout_interface
@see class table


## Instance variables {#class-layout_manager-instvars}

- layout_manager<-device: device*
    Device on which this manager places graphicals.

- layout_manager<->request_compute: any*
    Set when the layout has become invalid and a `->compute` is
    pending; the device polls this slot before the next redraw.


## Send methods {#class-layout_manager-send}

- layout_manager->initialise
    Initialise an abstract instance.  Subclasses normally extend
    this with their own setup.

- layout_manager->attach: graphical
    Attach the manager to a device, taking responsibility for the
    layout of the device's graphicals.

- layout_manager->detach
    Detach from the current device.

- layout_manager->unlink
    Called when the manager is destroyed: cleanly detach from
    `<-device`.

- layout_manager->compute
    Virtual method: recompute the layout.  Subclasses override.

- layout_manager->compute_bounding_box
    Virtual method: compute the union bounding box of the laid-out
    graphicals.  The default implementation fails (no contribution).

- layout_manager->RedrawArea: area
    Paint background objects for the indicated area.  The default
    implementation succeeds without drawing.
