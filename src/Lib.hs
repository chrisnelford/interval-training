module Lib
    ( run
    ) where

import System.Random
import Euterpea
import Control.Monad
import Control.Monad.Random
import Text.Printf

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
run = void $ runRandomQuiz quiz

askQuestion :: Question -> IO Result
askQuestion q = do
    play $ music q
    putStrLn $ prompt q
    answer <- getLine
    let (response, result) | (test q) answer = (successText q, success)
                           | otherwise       = (failureText q, failure)
    putStrLn response
    return result

runQuiz :: Quiz -> IO ()
runQuiz = evaluate >=> display
    where evaluate = (fmap mconcat) . sequence . (map askQuestion)
          display = putStrLn . resultSummary

runRandomQuiz :: Rand StdGen Quiz -> IO ()
runRandomQuiz = evalRandIO >=> runQuiz

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

-- Tracking score
data Result = Result { successes :: Int
                     , failures :: Int }

instance Semigroup Result where
    (<>) (Result s1 f1) (Result s2 f2) = Result (s1 + s2) (f1 + f2)

instance Monoid Result where
    mempty = Result { successes = 0, failures = 0 }

success, failure :: Result
success = Result { successes = 1, failures = 0 }
failure = Result { successes = 0, failures = 1 }

total :: Result -> Int
total (Result s f) = s + f

successRatio :: Result -> Double
successRatio res = (fromIntegral $ successes res) / (fromIntegral $ total res)

successPercent :: Result -> Double
successPercent = (100 *) . successRatio

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