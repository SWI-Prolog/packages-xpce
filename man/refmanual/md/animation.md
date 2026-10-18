# class animation {#class-animation}

An animation holds the frames of an animated image file as images,
together with the time each frame is shown.  It reads animated PNG
(APNG), GIF, WebP and AVIF files and Windows animated cursors (ANI).  A
file that is not animated becomes an animation of one frame.

An animation does not play itself.  A bitmap shows it:

	new(A, animation('spinner.png')),
	send(Window, display, new(BM, bitmap(A))),
	send(BM, play).

All frames are decoded when the animation is created, so a long
animation with large frames takes a lot of memory.  Loading the same
file as an image gives its first frame.

@see bitmap->play
@see class image


## Instance variables {#class-animation-instvars}

- animation<-file: source_sink
    Where the frames were loaded from.

- animation<-frames: vector
    The frames, as images.  All frames have the same <-size: the file
    describes the complete picture for each frame, also if it only
    stores what changed.

- animation<-delays: vector
    For each frame, the number of milliseconds it is shown.  Animations
    often use 0 (GIF) for "as fast as possible"; `bitmap->play` shows
    frames of 10 milliseconds or less for 100 milliseconds, as web
    browsers do.

- animation<-loop_count: int
    How many times the animation is played.  0 means forever.

- animation<-size: size
    The size of the frames.


## Send methods {#class-animation-send}

- animation->initialise: file=source_sink
    Load all frames of the image file.  A file is searched for in
    `image.path`, as for `image->initialise`.  Fails with a message if
    the file cannot be read or is not an image.


## Get methods {#class-animation-get}

- animation<-frame: index=int -> image
    The image of the frame at index, where the first frame is 1.

- animation<-count: -> int
    The number of frames.
