# class audio_recorder {#class-audio_recorder}

An audio_recorder records from the default audio input, normally the
microphone, into its <-sound, a mono 16-bit sound of 44100 samples per
second.  While recording, <-level_message is executed regularly (about
40 times per second) with the recorder and the input level, a number
between 0 and 1:

	new(R, audio_recorder(message(Meter, value, @arg2))),
	send(R, record),
	...
	send(R, stop),
	get(R, sound, Sound),
	send(Sound, save, 'recording.wav').

Given a sink, ->record writes the samples to it while recording rather
than into <-sound.  A `file` becomes a WAV file; any other
`source_sink` gets the raw samples (<-format, <-frequency and
<-channels) using `source_sink->write_as_file` and
`source_sink->close_as_file` when recording stops:

	send(R, record, file('recording.wav'))

The system may ask the user for permission to use the microphone.

@see class sound
@see class audio_stream


## Instance variables {#class-audio_recorder-instvars}

- audio_recorder<-sound: sound
    The recorded sound.

- audio_recorder<->level_message: code*
    Executed with the recorder and the level of the newly recorded
    audio (the root mean square of the samples, 0..1).

- audio_recorder<-recording: bool
    `@on` while recording.

- audio_recorder<-sink: source_sink|video_writer*
    Where ->record writes the samples, or `@nil` if it records into
    <-sound.


## Send methods {#class-audio_recorder-send}

- audio_recorder->initialise: level_message=[code]*
    Create a recorder.

- audio_recorder->record: to=[source_sink|video_writer]
    Start recording.  Without `to`, this replaces the audio of
    <-sound.  Otherwise the samples are written to `to` while
    recording, as described above.  If `to` is a video_writer, the
    samples are added to its audio using video_writer->append_audio.
    If the video_writer has no audio yet, it gets an audio track with
    the frequency and channels of the recorder.

- audio_recorder->stop
    Stop recording.


## Get methods {#class-audio_recorder-get}

- audio_recorder<-format: -> name
    Format of the recorded samples: `s16`, signed 16-bit in native
    byte order.

- audio_recorder<-frequency: -> int
    Recorded samples per second: 44100.

- audio_recorder<-channels: -> int
    Number of channels recorded: 1.
