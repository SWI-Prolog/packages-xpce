# class audio_stream {#class-audio-stream}

An audio_stream plays audio while it is written.  Its data is either
raw samples in <-format, <-frequency and <-channels, or, if <-format is
`encoded`, a sound file that SDL_sound decodes while it arrives.  This
works for WAV, MP3, FLAC and Sun/NeXT `.au` data, but not for decoders
that need the size of the data, notably Ogg Vorbis: load such a file
using class `sound`.

An audio_stream is a `source_sink` with encoding `octet`, so pce_open/3
opens a binary Prolog stream on it.  Closing that stream ends the data.
For example, to play an MP3 internet radio stream:

	new(AS, audio_stream),
	send(AS, play),
	pce_open(AS, write, Out),
	http_open(URL, In, []),
	copy_stream_data(In, Out).

A write waits while the stream is playing and has much more queued
than it needs, so copy_stream_data/2 does not read the whole source
into memory.  Because it waits, a producer that writes a lot should
run in a thread of its own.  The thread releases the xpce lock while it
waits.  After ->stop, writes fail silently, so the Prolog stream
raises an I/O error, which ends such a producer.

An application that produces the samples can instead act on
<-low_water_message, which asks for more data when less than
<-low_water seconds are queued:

	new(AS, audio_stream(s16, 44100, 2)),
	send(AS, low_water_message, message(@prolog, more_samples, @arg1)),
	send(AS, play).

<-message is executed after the data ended and everything is played.
The stream then closes the audio device; writing and ->play open it
again.

@see class sound
@see class audio_recorder


## Instance variables {#class-audio-stream-instvars}

- audio_stream<-format: {u8,s8,s16,s32,f32,encoded}
    Format of the samples written, or `encoded` for a sound file.
    Raw samples are in native byte order.

- audio_stream<-frequency: int
    Samples per second of raw data.  For encoded data, it is set when
    the decoder knows the format.

- audio_stream<-channels: int
    Number of channels of raw data: 1 is mono, 2 stereo.  For encoded
    data, it is set when the decoder knows the format.

- audio_stream<->volume: 0.0..1.0
    Gain applied when playing.

- audio_stream<->low_water: num
    Seconds of queued audio below which <-low_water_message is
    executed.  The default is 0.5.

- audio_stream<->low_water_message: code*
    Executed with the stream when less than <-low_water seconds are
    queued while the data did not end.  It is executed again after
    data is written or more was queued.

- audio_stream<->message: code*
    Executed with the stream after the data ended and everything is
    played.

- audio_stream<-playing: bool
    `@on` from ->play until the data ended and is played or ->stop.


## Send methods {#class-audio-stream-send}

- audio_stream->initialise: format=[{u8,s8,s16,s32,f32,encoded}], frequency=[1..], channels=[1..8]
    Create a stream.  The default format is `encoded`.  The defaults
    for raw data are 44100 samples per second and 2 channels.

- audio_stream->write_as_file: at=[int], data=char_array
    Queue data.  Used by pce_open/3.  Waits while the stream is
    playing far ahead.

- audio_stream->append: data=char_array
    Queue data, as ->write_as_file.

- audio_stream->truncate_as_file
    Discard the queued data.  pce_open/3 sends this when opening the
    stream in `write` mode.  For encoded data, the next write starts a
    new file.

- audio_stream->close_as_file
    The data has ended.  Sent when the Prolog stream opened using
    pce_open/3 is closed.

- audio_stream->close
    The data has ended.

- audio_stream->play
    Start playing the queued data and the data written later.

- audio_stream->stop
    Stop playing and discard the queued data.  <-message is not
    executed.  Writes fail silently until ->play or ->truncate_as_file,
    so a producer that writes to the stream ends.

- audio_stream->drain
    Process events until all queued data is played.


## Get methods {#class-audio-stream-get}

- audio_stream<-queued: -> num
    Seconds of decoded audio that is queued for playing.
