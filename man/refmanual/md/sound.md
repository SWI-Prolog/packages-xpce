# class sound {#class-sound}

A sound holds audio, loaded from a WAV file or recorded using an
audio_recorder.  ->play plays it on the default audio output.  A sound
may be played several times at once, e.g., for the sound effects of a
game.  When a play is finished, <-message is executed:

	new(S, sound('click.wav')),
	send(S, message, message(@prolog, clicked, @arg1)),
	send(S, play).

Only WAV files are supported.

@see class audio_recorder


## Instance variables {#class-sound-instvars}

- sound<-file: file*
    The file the sound was loaded from, or @nil.

- sound<-frequency: int
    The number of samples per second.

- sound<-channels: int
    The number of channels: 1 is mono, 2 stereo.

- sound<-duration: num
    The length in seconds.

- sound<->message: code*
    Executed with the sound as argument when a play is finished.  It
    is not executed for a play that is stopped using ->stop.


## Send methods {#class-sound-send}

- sound->initialise: file=[file]
    Create a sound, loading the WAV file if given.

- sound->load: file
    Load audio from a WAV file.  Raises an error if the file cannot be
    loaded.

- sound->save: file
    Save the audio as a WAV file.

- sound->play: volume=[0.0..1.0]
    Play the sound on the default audio output.  The default volume
    is 1.0.  This returns immediately.

- sound->stop
    Stop all plays of this sound.


## Get methods {#class-sound-get}

- sound<-playing: -> bool
    `@on` if the sound is being played.
