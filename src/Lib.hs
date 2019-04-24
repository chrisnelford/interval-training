module Lib
    ( run
    ) where

import Control.Monad
import Text.Printf

import Euterpea

import Random
import Result

-- Entry Point
run :: IO ()
run = do
    result <- runRandomQuiz quiz
    putStrLn $ resultSummary result

askQuestion :: Question -> IO Result
askQuestion q = do
    play $ music q
    putStrLn $ prompt q
    answer <- getLine
    let (response, result) | (test q) answer = (successText q, success)
                           | otherwise       = (failureText q, failure)
    putStrLn response
    return result

runQuiz :: Quiz -> IO Result
runQuiz = foldMap askQuestion

runRandomQuiz :: Rand StdGen Quiz -> IO Result
runRandomQuiz = evalRandIO >=> runQuiz

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

-- Utilites for contructing musical objects
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

-- Utilites for interacting with users.
correct :: String
correct = "Correct!"

formatSimplePercent :: Double -> String
formatSimplePercent = printf "%.0g%%"

resultSummary :: Result -> String
resultSummary res = let successStr = show $ successes res
                        questionStr | successes res == 1 = "question"
                                    | otherwise          = "questions"
                        totalStr = show $ total res
                        percentStr = formatSimplePercent $ successPercent res
                    in mconcat ["You answered ", successStr, " ",
                                 questionStr, " correctly out of a total of ",
                                 totalStr, ".\n",
                                 "That's ", percentStr, "."]