module Lib
    ( run
    ) where

import System.Random
import Euterpea
import Control.Monad.Random

-- Randomness for Euterpea note-level contstructs
instance Random PitchClass where
    randomR (lower, upper) g = (toEnum randomInt, g')
        where (randomInt, g') = randomR (fromEnum lower, fromEnum upper) g
    random = randomR (minBound, maxBound)

randomOctave :: RandomGen g => Rand g Octave
randomOctave = getRandomR (0, 8)

randomPitch :: RandomGen g => Rand g Pitch
randomPitch = do
    pitchClass <- getRandom
    octave <- randomOctave
    return (pitchClass, octave)

randomInterval :: RandomGen g => Rand g (PitchClass, Octave, Int)
randomInterval = do
    (basePitchClass, baseOctave) <- randomPitch
    interval <- getRandomR (1, 12)
    return (basePitchClass, baseOctave, interval)

-- Contructing musical objects
buildInterval :: PitchClass -> Octave -> Int -> Music Pitch
buildInterval pitchClass octave interval = line [firstNote, shortRest, secondNote]
  where firstNote = note (1/4) (pitchClass, octave)
        shortRest = rest (1/8)
        secondNote = transpose interval firstNote

-- Entry Point
run :: IO ()
run = intervalQuiz

runQuiz :: Music Pitch -> String -> (String -> Bool) -> String -> String -> IO ()
runQuiz music prompt test successString failureString = do
    play music
    putStrLn prompt
    answer <- getLine
    putStrLn $ if test answer then successString else failureString

-- Types of Quiz
intervalQuiz :: IO ()
intervalQuiz = do
    (pitchClass, octave, interval) <- evalRandIO randomInterval
    let music = buildInterval pitchClass octave interval
    let prompt = "How many semitones?"
    let test = \x -> (read x) == interval
    let successString = "Correct"
    let failureString = "Wrong: that was " ++ show interval ++ " semitones."
    runQuiz music prompt test successString failureString

singleToneQuiz :: IO ()
singleToneQuiz = do
    (pitch, octave) <- evalRandIO randomPitch
    let music = note 1 (pitch, octave)
    let prompt = "What note did you hear?"
    let test = \x -> (absPitch (read x, octave) == absPitch (pitch, octave))
    let successString = "Correct"
    let failureString = "Wrong: that was a " ++ show pitch ++ "."
    runQuiz music prompt test successString failureString