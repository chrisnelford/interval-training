# intervalTraining

Teaching yourself to recognize notes and intervals can be hard. This program is designed to help, by building short quizzes to help users recognize notes.

First, I'm going to use my laptop's speakers to play tones, and a CLI to answer questions about them.

In the future, I'm planning to integrate with my MIDI keyboard, so that it will play tones, and I'll have to play them back.

## Install

Currently only on Windows (although it should be possible to get this working on Linux using JACK and Fluidsynth).

Clone the repo and run `stack build`.

## TODOs

Add a free software license to the code.

Add MIDI function to work with my keyboard.

Suggest genericizing MonadRandom's `fromList` to other traversables.

Debug the unwanted text in stdout:

> pm_winmm_term called
> pm_winmm_term exiting

The issue seems to be that [PortMidi](https://github.com/PortMidi/PortMidi/blob/master/PortMidi.cabal) builds its Windows C libraries with `DEBUG` hard-coded
to `1`. Ideally, it would be possible to disable this as part of configuring the
Haskell package.

Add more quizzes:

* Both ascending and descending intervals
* Identify chords