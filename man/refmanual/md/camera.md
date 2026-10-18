# class camera {#class-camera}

A camera delivers video from a camera, e.g., a webcam, as an image.
->open starts the camera.  Each new frame is copied into the same
<-image, so a bitmap that displays this image shows the video, and
<-message is executed:

	new(C, camera),
	send(Window, display, bitmap(C?image)),
	send(C, open).

The image is replaced by the frame, so its size is the size of the
frames.  The user may have to give permission to use the camera.  Until
this is decided, <-status is `waiting`.

@see display<-cameras
@see class image


## Instance variables {#class-camera-instvars}

- camera<-name: name*
    The name of the camera, as in `display<-cameras`.  @nil uses the
    first camera.

- camera<-image: image
    The last frame.

- camera<-status: {closed,waiting,open,denied}
    Whether the camera delivers frames: `waiting` while the user has
    to decide whether the application may use the camera and `denied`
    if the user refused.

- camera<->message: code*
    Executed with the camera as argument for each new frame.

- camera<->status_message: code*
    Executed with the camera and the new <-status when it changes.


## Send methods {#class-camera-send}

- camera->initialise: name=[name]
    Create for the named camera or, if omitted, the first camera.  The
    camera is not opened.

- camera->open
    Start delivering frames.  Raises an error if there is no such
    camera.

- camera->close
    Stop delivering frames.

- camera->sdl_test_frame: width=int, height=int, colour=colour
    For testing only.  Deliver a frame of the given size and colour,
    converted from the YUY2 format of many cameras.
