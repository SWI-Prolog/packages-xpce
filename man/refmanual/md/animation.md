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

An animation can also be made from images using ->append, or by
recording a window using ->record, and saved as an animated image file
using ->save.  For example, to record a window for 5 seconds as an
animated GIF:

	new(A, animation),
	send(A, record, Window, 0.1),
	...                          % 5 seconds later
	send(A, stop),
	send(A, save, 'window.gif').

@see bitmap->play
@see class image


## Instance variables {#class-animation-instvars}

- animation<-file: source_sink*
    Where the frames were loaded from, or @nil if the animation was
    made by appending frames.

- animation<-frames: vector
    The frames, as images.  All frames have the same <-size: the file
    describes the complete picture for each frame, also if it only
    stores what changed.

- animation<-delays: vector
    For each frame, the number of milliseconds it is shown.  Animations
    often use 0 (GIF) for "as fast as possible"; `bitmap->play` shows
    frames of 10 milliseconds or less for 100 milliseconds, as web
    browsers do.

- animation<->loop_count: int
    How many times the animation is played.  0 means forever.  ->save
    writes it.

- animation<->title: char_array*

- animation<->author: char_array*

- animation<->description: char_array*

- animation<->copyright: char_array*
    Text stored in the file, or @nil.  ->save writes them.  PNG and
    WebP files hold all of them; GIF files only the description.

- animation<-size: size
    The size of the frames.

- animation<-recording: frame|window*
    The frame or window ->record is recording, or @nil.


## Send methods {#class-animation-send}

- animation->initialise: file=[source_sink]
    Load all frames of the image file.  A file is searched for in
    `image.path`, as for `image->initialise`.  The format is found
    from the data, not the name of the file.  Fails with a message if
    the file cannot be read or is not an image.  Without a file, the
    animation has no frames: use ->append or ->record to add them.

- animation->append: image=image, delay=[int]
    Append a copy of the image as a frame that is shown for delay
    milliseconds (default 100).  The first frame sets <-size; images of
    another size are scaled to it.

- animation->clear
    Remove all frames.

- animation->record: from=frame|window, interval=[real]
    Append `from<-image` as a frame now and every interval seconds
    (default 0.1) until ->stop, or until `from` is destroyed.  Each
    frame is shown for the interval.

- animation->stop
    Stop recording.

- animation->save: in=file, format=[{png,gif,webp,avif,ani}], quality=[0..100]
    Save the frames as an animated image file.  `png` writes an
    animated PNG (APNG).  If `format` is omitted, the extension of the
    file selects it.  `quality` is used by WebP and AVIF.  <-loop_count
    and the texts such as <-title are saved with the frames, as far as
    the format can hold them.  Fails with a message if there are no
    frames or the format cannot be written: whether WebP and AVIF are
    available depends on how SDL_image was built.


## Get methods {#class-animation-get}

- animation<-frame: index=int -> image
    The image of the frame at index, where the first frame is 1.

- animation<-count: -> int
    The number of frames.
