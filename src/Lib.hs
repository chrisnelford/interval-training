module Lib
    ( runQuiz
    ) where

import System.Random
import Euterpea

instance Random PitchClass where
    randomR (lower, upper) g = (toEnum randomInt, g')
        where (randomInt, g') = randomR (fromEnum lower, fromEnum upper) g
    random = randomR (minBound, maxBound)

randomOctave :: RandomGen g => g -> (Octave, g)
randomOctave = randomR (-1, 9)

runQuiz :: IO ()
runQuiz = intervalQuiz

intervalQuiz :: IO ()
intervalQuiz = do
    gen <- getStdGen
    let (pitch, gen') = random gen :: (PitchClass, StdGen)
    let (octave, gen'') = randomOctave gen'
    let (interval, _) = randomR (1, 12) gen''
    let firstNote = note (1/4) (pitch, octave)
    play $ firstNote :+: rest (1/8) :+: transpose interval firstNote
    putStrLn "How many semitones?"
    answer <- getLine
    let answerInterval = read answer :: Int
    if answerInterval == interval
        then putStrLn "Correct"
        else putStrLn $ "Wrong: that was " ++ show interval ++ " semitones."

singleToneQuiz :: IO ()
singleToneQuiz = do
    gen <- getStdGen
    let (pitch, gen') = random gen
    let (octave, _) = randomOctave gen'
    play $ note 1 (pitch, octave)
    putStrLn "What note did you hear?"
    answer <- getLine
    let answerPitch = read answer :: PitchClass
    if absPitch (answerPitch, octave) == absPitch (pitch, octave)
        then putStrLn "Correct"
        else putStrLn $ "Wrong: that was a " ++ show pitch