module Lib
    ( run
    ) where

import Control.Monad
import Control.Monad.State
import Text.Printf

import Euterpea

import Deck (Deck, fromFoldable, insert, drawFrom, Location(..))
import Random
import Result
import qualified Question as Question
import qualified Quiz as Quiz

-- Entry Point
run :: IO ()
run = do
    result <- runQuiz myQuiz
    putStrLn $ resultSummary result

-- myQuiz :: OldQuiz
-- myQuiz = buildQuiz (replicate 3 singleToneQuestion ++ replicate 2 intervalQuestion)

newtype QuizApp a = QuizApp (RandT StdGen (StateT (Deck Question.Question) IO) a)
  deriving (Functor, Applicative, Monad, MonadRandom, MonadState (Deck Question.Question), MonadIO)

type OldQuiz = QuizApp ()

-- buildQuiz :: (MonadState (Deck Question) m, Traversable t) => t (m Question) -> m ()
-- buildQuiz = sequence >=> put . fromFoldable

runQuiz :: Quiz.Quiz -> IO Result
runQuiz = undefined

myQuiz :: Quiz.Quiz
myQuiz = undefined

-- -- TODO: Something's a bit off about the way this handles state. The set of questions
-- --       to run in a quiz and the app that runs the quiz seem coupled too tightly.
-- runQuizOld :: OldQuiz -> IO Result
-- runQuizOld quiz = runQuizApp (quiz >> askQuestions)

-- runQuizApp :: QuizApp a -> IO a
-- runQuizApp (QuizApp app) = getStdGen >>= (flip evalStateT) mempty . evalRandT app

maxQuestions :: Int
maxQuestions = 10

-- Run a quiz by asking the users all of the questions.
-- If the user gets a question wrong, put it back in the deck
-- and ask it again later.
askQuestions :: QuizApp Result
askQuestions = do
    question <- pickQuestion
    case question of Nothing -> return mempty
                     Just q  -> liftM2 (<>) (processQuestion q) askQuestions
         where processQuestion :: Question.Question -> QuizApp Result
               processQuestion q = do
                    result <- liftIO $ askQuestion q
                    if | total result >= maxQuestions -> put mempty
                       | result == failure -> putQuestion q
                       | otherwise -> return ()
                    return result

askQuestion :: Question.Question -> IO Result
askQuestion q = do
    play $ Question.music q
    putStrLn $ Question.prompt q
    answer <- getLine
    let (response, result) | (Question.test q) answer = (Question.successText q, success)
                           | otherwise       = (Question.failureText q, failure)
    putStrLn response
    return result

-- Ways of mutating the set of remaining questions.
pickQuestion :: (MonadRandom m, MonadState (Deck Question.Question) m) => m (Maybe Question.Question)
pickQuestion = do
    currentQuiz <- get
    choice <- drawFrom Random currentQuiz
    case choice of
        Nothing -> return Nothing
        Just (question, rest) -> do
            put rest
            return $ Just question

putQuestion :: MonadState (Deck Question.Question) m => Question.Question -> m ()
putQuestion q = do
    currentDeck <- get
    updatedDeck <- insert q currentDeck
    put updatedDeck

-- Types of Question
intervalQuestion :: MonadRandom m => m Question.Question
intervalQuestion = do
    (pitch, interval) <- randomIntervalFromMiddleC
    return $ Question.Question {
        Question.music       = buildInterval pitch interval,
        Question.prompt      = "How many semitones?",
        Question.test        = (== interval) . read,
        Question.successText = correct,
        Question.failureText = "Wrong: that was " ++ show interval ++ " semitones."
        }

singleToneQuestion :: MonadRandom m => m Question.Question
singleToneQuestion = do
    (pitchClass, octave) <- randomPitchInMiddleOctave
    return $ Question.Question {
        Question.music       = buildTone (pitchClass, octave),
        Question.prompt      = "What note did you hear?",
        Question.test        = (samePitchClass pitchClass) . read,
        Question.successText = correct,
        Question.failureText = "Wrong: that was a " ++ show pitchClass ++ "."
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