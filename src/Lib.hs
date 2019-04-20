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

buildTone :: Pitch -> Music Pitch
buildTone = note 1

-- Entry Point
run :: IO ()
run = do
    quiz <- intervalQuiz
    runQuiz quiz

data Quiz = Quiz { music :: Music Pitch
                 , prompt :: String
                 , test :: (String -> Bool)
                 , successText :: String
                 , failureText :: String }

runQuiz :: Quiz -> IO ()
runQuiz quiz = do
    play $ music quiz
    putStrLn $ prompt quiz
    answer <- getLine
    putStrLn $ if (test quiz) answer
               then successText quiz
               else failureText quiz

-- Types of Quiz
correct :: String
correct = "Correct!"

samePitchClass :: PitchClass -> PitchClass -> Bool
samePitchClass pc1 pc2 = absPitch (pc1, 0) == absPitch (pc2, 0)

intervalQuiz :: IO Quiz
intervalQuiz = do
    (pitchClass, octave, interval) <- evalRandIO randomInterval
    return $ Quiz {
        music       = (buildInterval pitchClass octave interval),
        prompt      = "How many semitones?",
        test        = (== interval) . read,
        successText = correct,
        failureText = ("Wrong: that was " ++ show interval ++ " semitones.")
        }

singleToneQuiz :: IO Quiz
singleToneQuiz = do
    (pitchClass, octave) <- evalRandIO randomPitch
    return $ Quiz {
        music       = (buildTone (pitchClass, octave)),
        prompt      = "What note did you hear?",
        test        = (samePitchClass pitchClass) . read,
        successText = correct,
        failureText = ("Wrong: that was a " ++ show pitchClass ++ ".")
        }