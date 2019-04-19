module Lib
    ( runQuiz
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
runQuiz :: IO ()
runQuiz = intervalQuiz

-- Types of Quiz
intervalQuiz :: IO ()
intervalQuiz = do
    (pitchClass, octave, interval) <- evalRandIO randomInterval
    putStrLn $ show interval
    play $ buildInterval pitchClass octave interval
    putStrLn "How many semitones?"
    answer <- getLine
    let answerInterval = read answer :: Int
    if answerInterval == interval
        then putStrLn "Correct"
        else putStrLn $ "Wrong: that was " ++ show interval ++ " semitones."

singleToneQuiz :: IO ()
singleToneQuiz = do
    (pitch, octave) <- evalRandIO randomPitch
    play $ note 1 (pitch, octave)
    putStrLn "What note did you hear?"
    answer <- getLine
    let answerPitch = read answer :: PitchClass
    if absPitch (answerPitch, octave) == absPitch (pitch, octave)
        then putStrLn "Correct"
        else putStrLn $ "Wrong: that was a " ++ show pitch