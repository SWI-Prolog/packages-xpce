# class resize_outline_gesture {#class-resize_outline_gesture}

Resize a graphical object by resizing an outline first.  Outline-based
resizing avoids expensive computation for objects that cannot be resized
easily, either due to their internal complexity or due to constraints.

@see class resize_gesture


## Instance variables {#class-resize_outline_gesture-instvars}

- resize_outline_gesture<-outline_gesture: resize_gesture
    Instance of class resize_gesture used to resize the <-outline.

