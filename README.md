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

Write better tools for building up quizzes.

Suggest genericizing MonadRandom's `fromList` to other traversables.