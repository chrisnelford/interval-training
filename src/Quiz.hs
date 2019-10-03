module Quiz
    ( Quiz
    , next
    , add
    , build
    , Question(..)
    , intervalQuestion
    , singleToneQuestion
    ) where

import qualified Euterpea as E

import Data.List (uncons)
import qualified Control.Monad.Random as R

import Music ( buildTone
             , buildInterval
             , samePitchClass )

import Random ( randomIntervalFromMiddleC
              , randomPitchInMiddleOctave )

import qualified Result

-- A quiz is a source of questions.
type Quiz = [Question]

next :: Quiz -> Maybe (Question, Quiz)
next = uncons

add :: Question -> Quiz -> Quiz
add = (:)

build :: R.StdGen -> [R.Rand R.StdGen Question] -> Quiz
build gen questions = R.evalRand (sequence questions) gen

-- Data structures representing questions.
data Question = Question { music :: E.Music E.Pitch
                         , prompt :: String
                         , test :: (String -> Bool)
                         , successText :: String
                         , failureText :: String }

-- Types of Question
intervalQuestion :: R.MonadRandom m => m Question
intervalQuestion = do
    (pitch, interval) <- randomIntervalFromMiddleC
    return $ Question {
        music       = buildInterval pitch interval,
        prompt      = "How many semitones?",
        test        = (== interval) . read,
        successText = correct,
        failureText = "Wrong: that was " ++ show interval ++ " semitones."
    }

singleToneQuestion :: R.MonadRandom m => m Question
singleToneQuestion = do
    (pitchClass, octave) <- randomPitchInMiddleOctave
    return $ Question {
        music       = buildTone (pitchClass, octave),
        prompt      = "What note did you hear?",
        test        = (samePitchClass pitchClass) . read,
        successText = correct,
        failureText = "Wrong: that was a " ++ show pitchClass ++ "."
    }

correct :: String
correct = "Correct!"