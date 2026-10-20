# class video_writer {#class-video-writer}

A video_writer encodes frames and sound into a video file.  The format
follows from the extension of the file: `.mp4` and `.mov` give H.264
video with AAC sound if available; ffmpeg (Linux) also writes, e.g.,
`.mkv` and `.webm`.  AVFoundation (macOS) writes only `.mp4` and
`.mov`.  Frames are images, sdl_texture objects (e.g., of a camera) or
the image of a frame or window:

	new(W, video_writer('clip.mp4', size(640, 480), 30)),
	forall(between(1, 90, I),
	       ( draw_frame(I, Image),
		 send(W, append, Image) )),
	send(W, close).

Frames of another size are scaled to <-size.  Encoding happens in a
thread: ->append converts the frame and queues it, so it only waits if
the encoder cannot keep up.  ->close encodes what is queued and
finishes the file.  The file is opened by the first ->append or
->append_audio.

There are three ways to record:

  - video_writer->record appends the image of a frame or window at
    <-frame_rate until ->stop.
  - camera<->writer makes a camera append its frames.
  - audio_recorder->record with a video_writer adds the sound of the
    microphone.

The target need not be a file.  With ffmpeg (Linux) it may be a URL,
which streams the video live:

  - `rtmp://...` sends FLV, e.g., to YouTube or Twitch;
  - `srt://...`, `udp://...` and `tcp://...` send MPEG-TS, and
    `rtp://...` RTP with MPEG-TS;
  - a file ending in `.m3u8` writes HLS: a playlist and 2-second
    segments next to it, which a web server can serve to browsers.

The target may also be code, which is executed with the writer and a
char_array holding the next encoded data.  Its characters are the bytes
(0..255).  The code runs in the GUI thread from the media tick, in the
order of the data.  An MP4 that is not written to a file is fragmented,
so it can be played while it arrives.  See video_stream_writer/4 of
library(pce_video) to write to a Prolog stream, e.g., the reply to an
HTTP request:

	new(W, video_writer(message(@prolog, send_data, @arg1),
			    size(640, 480), 30)),
	send(W, format, mpegts),

<-live is @on by default for a URL.  A live writer encodes with low
latency and a constant <-bitrate, and drops frames, counted in
<-dropped, rather than making ->append wait if the encoder or the
network cannot keep up.

The codecs that are available are in display<-video_encoders.

@see class video
@see display<-video_encoders


## Instance variables {#class-video-writer-instvars}

- video_writer<-target: file|name|code
    File or URL the video is written to, or code that is executed with
    the writer and each chunk of encoded data.

- video_writer<-size: size
    Size of the frames.  Sizes are rounded down to an even number of
    pixels, as most codecs require.

- video_writer<-frame_rate: num
    Frames per second.  Frames appended without a time follow each
    other at this rate.

- video_writer<-codec: name*
    The video encoder.  @nil until the file is opened, which uses the
    best encoder for the format unless one was given.

- video_writer<-quality: 0..100*
    Quality, where 100 is the best.  @nil uses a default of the encoder.

- video_writer<-frames: int
    Number of frames appended.

- video_writer<-audio_frequency: int*
    Samples per second of the sound, or @nil if the video has no sound.
    The encoder may change the frequency given to ->audio.

- video_writer<-audio_channels: int*
    Number of channels of the sound.

- video_writer<-audio_codec: name*
    The audio encoder, set when the file is opened.

- video_writer<-status: {new,open,closed}
    `new` before the first frame, `open` while frames can be appended
    and `closed` after ->close.

- video_writer<-format: name*
    The container, e.g., `mp4`, `mov`, `mpegts`, `webm`, `matroska`,
    `flv` or `hls`.  @nil selects it from the target: the extension of
    a file, the protocol of a URL and fragmented `mp4` for code.  Set
    it before the first frame.

- video_writer<-live: bool
    If @on, the video is encoded with low latency and a constant
    bitrate, and ->append drops frames rather than waiting if the
    encoder falls behind.  @on by default for a URL.  Set it before the
    first frame.

- video_writer<-bitrate: 1..*
    Bits per second of the video, or @nil to use <-quality.  A live
    writer without a bitrate uses about 0.1 bit per pixel.  Set it
    before the first frame.

- video_writer<-dropped: int
    Frames dropped by a live writer because the encoder fell behind.

- video_writer<-recording: frame|window*
    Frame or window recorded by ->record.


## Send methods {#class-video-writer-send}

- video_writer->initialise: target=file|name|code, size=size, frame_rate=[num], codec=[name], quality=[0..100]
    Create to write a video file, to stream to a URL or to give the data
    to code.  The frame rate is 30 by default.
    codec names an encoder (e.g., `libx264`) or a codec (e.g., `h264`).

- video_writer->append: frame=image|sdl_texture|frame|window, at=[num]
    Add a frame.  `at` is its time in seconds; without it, it follows
    the previous frame at <-frame_rate.

- video_writer->audio: frequency=[1..], channels=[1..8]
    Give the video sound with this frequency (default 48000) and number
    of channels (default 2).  This must be done before the first frame.

- video_writer->append_audio: data=char_array, format=[{u8,s8,s16,s32,f32}], frequency=[1..], channels=[1..8]
    Add samples to the sound, as audio_stream->append does.  The data
    holds samples of `format` (default `s16`) at `frequency` with
    `channels`, by default those of the sound of the video.  They are
    converted to the format of the video.  If ->audio was not used,
    the first samples set the format of the sound.  The sound starts at
    the start of the video and has no gaps.

- video_writer->record: from=frame|window
    Append the image of `from` at <-frame_rate until ->stop or ->close.
    The frames get the time they were taken.

- video_writer->record_frame
    Append the image of <-recording.  Used by ->record.

- video_writer->stop
    Stop ->record.

- video_writer->close
    Encode the remaining frames and sound and finish the file.
