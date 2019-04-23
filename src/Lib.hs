module Lib
    ( run
    ) where

import System.Random
import Euterpea
import Control.Monad
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

randomInterval :: RandomGen g => Rand g (Pitch, Int)
randomInterval = do
    basePitch <- randomPitch
    interval <- getRandomR (1, 12)
    return (basePitch, interval)

-- Contructing musical objects
buildInterval :: Pitch -> Int -> Music Pitch
buildInterval pitch interval = line [firstNote, shortRest, secondNote]
  where firstNote = note (1/4) pitch
        shortRest = rest (1/8)
        secondNote = transpose interval firstNote

buildTone :: Pitch -> Music Pitch
buildTone = note 1

-- Utilites for building test functions
samePitchClass :: PitchClass -> PitchClass -> Bool
samePitchClass pc1 pc2 = absPitch (pc1, 0) == absPitch (pc2, 0)

-- Entry Point
run :: IO ()
run = runRandomQuiz quiz

askQuestion :: Question -> IO()
askQuestion q = do
    play $ music q
    putStrLn $ prompt q
    answer <- getLine
    putStrLn $ if (test q) answer
               then successText q
               else failureText q

runQuiz :: Quiz -> IO()
runQuiz = sequence_ . (map askQuestion)

runRandomQuiz :: Rand StdGen Quiz -> IO ()
runRandomQuiz = evalRandIO >=> runQuiz

-- Utilites for building messages for users.
correct :: String
correct = "Correct!"

-- Types of Quiz
data Question = Question { music :: Music Pitch
                         , prompt :: String
                         , test :: (String -> Bool)
                         , successText :: String
                         , failureText :: String }

type Quiz = [Question]

quiz :: RandomGen g => Rand g Quiz
quiz = liftM2 (++) (replicateM 3 singleToneQuestion) (replicateM 2 intervalQuestion)

intervalQuestion :: RandomGen g => Rand g Question
intervalQuestion = do
    (pitch, interval) <- randomInterval
    return $ Question {
        music       = buildInterval pitch interval,
        prompt      = "How many semitones?",
        test        = (== interval) . read,
        successText = correct,
        failureText = "Wrong: that was " ++ show interval ++ " semitones."
        }

singleToneQuestion :: RandomGen g => Rand g Question
singleToneQuestion = do
    (pitchClass, octave) <- randomPitch
    return $ Question {
        music       = buildTone (pitchClass, octave),
        prompt      = "What note did you hear?",
        test        = (samePitchClass pitchClass) . read,
        successText = correct,
        failureText = "Wrong: that was a " ++ show pitchClass ++ "."
        }