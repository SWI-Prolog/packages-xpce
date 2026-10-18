# class audio_recorder {#class-audio-recorder}

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

The system may ask the user for permission to use the microphone.

@see class sound


## Instance variables {#class-audio-recorder-instvars}

- audio_recorder<-sound: sound
    The recorded sound.

- audio_recorder<->level_message: code*
    Executed with the recorder and the level of the newly recorded
    audio (the root mean square of the samples, 0..1).

- audio_recorder<-recording: bool
    `@on` while recording.


## Send methods {#class-audio-recorder-send}

- audio_recorder->initialise: level_message=[code]*
    Create a recorder.

- audio_recorder->record
    Start recording.  This replaces the audio of <-sound.

- audio_recorder->stop
    Stop recording.
