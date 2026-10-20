# class camera {#class-camera}

A camera delivers video from a camera, e.g., a webcam, as an image.
->open starts the camera.  Each new frame is copied into the same
<-image, so a bitmap that displays this image shows the video, and
<-message is executed:

	new(C, camera),
	send(Window, display, bitmap(C?image)),
	send(C, open).

The image is replaced by the frame, so its size is the size of the
frames.  If <-texture is an sdl_texture, the frames go there instead.
They keep the format of the camera and an sdl_layer shows them in an
sdl_window without converting them on the CPU, which is much cheaper
for large frames.  The user may have to give permission to use the camera.  Until
this is decided, <-status is `waiting`.

@see display<-cameras
@see class image
@see class sdl_texture


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

- camera<->texture: sdl_texture*
    If not @nil, the frames are copied into this texture rather than
    into <-image, keeping their format.  Motion JPEG frames are decoded.

- camera<->writer: video_writer*
    If not @nil, each frame is also appended to this video_writer.  The
    frames get the time of the camera, starting at the next frame of
    the writer.  The writer is reset to @nil if appending fails.


## Send methods {#class-camera-send}

- camera->initialise: name=[name]
    Create for the named camera or, if omitted, the first camera.  The
    camera is not opened.

- camera->open: size=[size]
    Start delivering frames.  If `size` is given, the camera uses the
    size it supports that is closest: the same size or else the
    smallest larger size, using the highest frame rate.  Otherwise the
    camera chooses, often its largest size.  Large frames take time to
    process.  Fails with a report if there is no such camera.

- camera->close
    Stop delivering frames.

- camera->sdl_test_frame: width=int, height=int, colour=colour, format=[{yuy2,mjpg}]
    For testing only.  Deliver a frame of the given size and colour in
    the YUY2 (default) or Motion JPEG format of cameras.


## Get methods {#class-camera-get}

- camera<-sizes: -> chain
    New chain with the sizes the camera supports, smallest first.
