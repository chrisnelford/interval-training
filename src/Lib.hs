module Lib
    ( run
    ) where

import Control.Monad
import Control.Monad.State
import Data.List (uncons)
import Data.Foldable (toList)  -- Needed because Quiz is a list
import Text.Printf

import Euterpea

import Random
import Result

-- Entry Point
run :: IO ()
run = do
    result <- runQuiz myQuiz
    putStrLn $ resultSummary result

myQuiz :: Quiz
myQuiz = buildQuiz (replicate 3 singleToneQuestion ++ replicate 2 intervalQuestion)

newtype QuizApp a = QuizApp (RandT StdGen (StateT RemainingQns IO) a)
  deriving (Functor, Applicative, Monad, MonadRandom, MonadState RemainingQns, MonadIO)

type Quiz = QuizApp ()

buildQuiz :: (MonadState RemainingQns m, Traversable t) => t (m Question) -> m ()
buildQuiz = sequence >=> put . toList

runQuiz :: Quiz -> IO Result
runQuiz quiz = runQuizApp (quiz >> askQuestions)

runQuizApp :: QuizApp a -> IO a
runQuizApp (QuizApp app) = getStdGen >>= (flip evalStateT) [] . evalRandT app

askQuestions :: QuizApp Result
askQuestions = do
    question <- pickQuestion
    case question of Nothing -> return mempty
                     Just q  -> liftM2 (<>) (liftIO $ askQuestion q) askQuestions

askQuestion :: Question -> IO Result
askQuestion q = do
    play $ music q
    putStrLn $ prompt q
    answer <- getLine
    let (response, result) | (test q) answer = (successText q, success)
                           | otherwise       = (failureText q, failure)
    putStrLn response
    return result

-- Ways of mutating the set of remaining questions.
-- Pick a "random" question by always returning the first one. We can do
-- better later.
pickQuestion :: (MonadRandom m, MonadState RemainingQns m) => m (Maybe Question)
pickQuestion = do
    currentQuiz <- get
    case uncons currentQuiz of
        Nothing -> return Nothing
        Just (question, rest) -> do
            put rest
            return $ Just question

putQuestion :: MonadState RemainingQns m => Question -> m ()
putQuestion q = modify (q:)

-- Data structures representing questions.
data Question = Question { music :: Music Pitch
                         , prompt :: String
                         , test :: (String -> Bool)
                         , successText :: String
                         , failureText :: String }

type RemainingQns = [Question]

-- Types of Question
intervalQuestion :: MonadRandom m => m Question
intervalQuestion = do
    (pitch, interval) <- randomInterval
    return $ Question {
        music       = buildInterval pitch interval,
        prompt      = "How many semitones?",
        test        = (== interval) . read,
        successText = correct,
        failureText = "Wrong: that was " ++ show interval ++ " semitones."
        }

singleToneQuestion :: MonadRandom m => m Question
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