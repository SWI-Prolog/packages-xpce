# class video {#class-video}

A video plays an encoded video file or stream, such as an MP4, MKV or
WebM file, with its sound.  The frames go into <-texture, an
sdl_texture, so an sdl_layer shows them in an sdl_window.  The frames
keep the format of the video: the renderer of the frame converts and
scales them, so they do not go through Cairo.  Graphicals displayed on
the window appear on top of the video:

	new(V, video('movie.mp4')),
	new(W, sdl_window('Movie', size(640, 360))),
	send(W, background, black),
	send(W, append, sdl_layer(V?texture)),	% fit the window
	send(W, open),
	send(V, play).

A thread decodes the video a few frames ahead.  The audio is played on
the default playback device and sets the time of the video: each frame
is shown when the sound of that moment plays.  Without audio, or after
it ended, the time comes from the system clock.

Videos are decoded by ffmpeg on Linux, AVFoundation on macOS and Media
Foundation on Windows.  SWI-Prolog configured with `-DXPCE_VIDEO=ffmpeg`
uses ffmpeg on all platforms, which is needed for streaming (see
below).  Its encoders then include those of the operating system, such
as Media Foundation and VideoToolbox, and display<-video_backend tells
which backend is used.  The codecs that are available
are in display<-video_decoders.  Methods that need the video, such as
<-size or ->play, open it if needed.

Class video is a source_sink: a video without a source plays the
encoded data written to it, e.g., by Prolog using pce_open/3.  This
allows playing video that arrives from, e.g., an HTTP request, a
WebSocket or a pipe:

	new(V, video),
	pce_open(V, write, Out),
	set_stream(Out, type(binary)),
	copy_stream_data(In, Out),
	close(Out).			% the data has ended

Formats that can be read from the start, such as MPEG-TS, WebM and
fragmented MP4, play while the data arrives.  If the data does not
arrive in time, playing waits for it.  Other formats, such as an MP4
file with its index at the end, play once the data is complete.

The source may also be a URL, e.g., `https://...`, an HLS playlist
(`.m3u8`) or `rtsp://...` of an IP camera, as far as the backend
supports it; ffmpeg supports all of these.  A network stream is opened
in the background: <-status is `opening` and methods such as <-size
fail until it is open, which <-status_message tells.  ->play while
opening plays as soon as the stream is open.  See <-live for streams
that are broadcast live.

@see class video_writer
@see class sdl_layer
@see display<-video_decoders


## Instance variables {#class-video-instvars}

- video<-source: file|name*
    File or URL of the video, or @nil if the encoded data is written to
    the video.

- video<-texture: sdl_texture
    Shows the current frame.  Display it using an sdl_layer.

- video<-status: {closed,opening,paused,playing,finished}
    `closed` before ->open and after ->close, `opening` while a network
    stream is being opened, `playing` while playing, `finished` after
    the last frame and `paused` otherwise.

- video<->status_message: code*
    Executed with the video and the new <-status when it changes, e.g.,
    when a network stream is open.

- video<->loop: bool
    If @on, the video starts again at the end.

- video<->message: code*
    Executed with the video when it reaches the end.

- video<->audio: bool
    If @off, the video is opened without its sound.  Changing it has
    effect the next time the video is opened.

- video<-volume: 0.0..1.0
    Gain applied to the sound.  Default is 1.0.

- video<-options: sheet*
    Options for opening the source.  See ->option.


## Send methods {#class-video-send}

- video->initialise: source=[file|name]*
    Create to play a file or URL.  The video is opened when needed.
    Without a source, the video plays the encoded data written to it.

- video->append: data=char_array
    Add encoded data to a video without <-source.  The bytes are the
    characters of data, which must be in the range 0..255.

- video->write_as_file: at=[int], data=char_array
    As ->append.  This allows writing to the video using pce_open/3.

- video->truncate_as_file
    Discard the data written so far and start again.  Sent by pce_open/3
    in mode `write`.

- video->close_as_file
    The data has ended.  Sent when the stream of pce_open/3 is closed.

- video->open
    Open the video and show the first frame.  Fails, after printing
    an error, if the source cannot be opened or decoded.

- video->close
    Close the video, freeing its resources.

- video->play
    Start or continue playing.  A video that has finished starts again
    from the beginning.

- video->pause
    Stop playing at the current position.

- video->stop
    Stop playing and go back to the start.

- video->position: seconds=num
    Go to a time in the video.  The frame shown at that time appears in
    <-texture, also if the video is paused.

- video->volume: 0.0..1.0
    Set the gain of the sound.

- video->option: name=name, value=char_array|int
    Set an option for opening the source.  The options are passed to the
    backend when the video is opened.  With ffmpeg these are the options
    of its protocols and formats, e.g., `rtsp_transport` `tcp` for an
    IP camera, `reconnect` 1 for HTTP or `rw_timeout` in microseconds.


## Get methods {#class-video-get}

- video<-position: -> seconds=num
    Time in the video, which advances while playing.

- video<-size: -> size
    Size of the frames.

- video<-frame_rate: -> num
    Frames per second, 0 if unknown.

- video<-duration: -> seconds=num
    Length of the video, 0 if unknown.

- video<-codec: -> name
    Name of the video decoder, e.g., `h264`.

- video<-audio_codec: -> name
    Name of the audio decoder, e.g., `aac`.  Fails if the video has no
    sound we play.

- video<-live: -> bool
    @on for a network stream that cannot seek, such as a live camera or
    broadcast.  A live video has no duration, ->position fails and
    <-loop is ignored.  It plays from the first frame it receives.  If
    playing falls behind, frames and sound are dropped, so the delay
    stays small.

- video<-has_audio: -> bool
    Whether the video has sound we play.

- video<-image: at=[num] -> image
    Copy of the current frame or, given a time, of the frame shown at
    that time, e.g., for a thumbnail.  The latter decodes the frame
    without disturbing playing.
