module Lib
    ( run
    ) where

import Control.Monad.State
import Text.Printf

import Euterpea

import Random
import Result
import qualified Quiz as Q

-- Entry Point
run :: IO ()
run = do
    gen <- getStdGen
    result <- runQuiz (myQuiz gen)
    putStrLn $ resultSummary result

-- myQuiz :: OldQuiz
-- myQuiz = buildQuiz (replicate 3 singleToneQuestion ++ replicate 2 intervalQuestion)

newtype QuizApp a = QuizApp ((StateT Q.Quiz IO) a)
  deriving (Functor, Applicative, Monad, MonadState (Q.Quiz), MonadIO)

runQuiz :: Q.Quiz -> IO Result
runQuiz quiz = runQuizApp quiz askQuestions

runQuizApp :: Q.Quiz -> QuizApp a -> IO a
runQuizApp quiz (QuizApp app) = evalStateT app quiz

myQuiz :: StdGen -> Q.Quiz
myQuiz gen = let questions = [ Q.singleToneQuestion
                             , Q.singleToneQuestion
                             , Q.singleToneQuestion
                             , Q.intervalQuestion
                             , Q.intervalQuestion ]
             in Q.build gen questions

-- Run a quiz by asking the users all of the questions.
-- If the user gets a question wrong, put it back in the deck
-- and ask it again later.
askQuestions :: QuizApp Result
askQuestions = do
    question <- pickQuestion
    case question of Nothing -> return mempty
                     Just q  -> liftM2 (<>) (processQuestion q) askQuestions
         where processQuestion :: Q.Question -> QuizApp Result
               processQuestion q = do
                    result <- liftIO $ askQuestion q
                    if | total result >= maxQuestions -> put mempty
                       | result == failure -> addQuestion q
                       | otherwise -> return ()
                    return result
               maxQuestions = 10

askQuestion :: Q.Question -> IO Result
askQuestion q = do
    play $ Q.music q
    putStrLn $ Q.prompt q
    answer <- getLine
    let (response, result) | (Q.test q) answer = (Q.successText q, success)
                           | otherwise       = (Q.failureText q, failure)
    putStrLn response
    return result

pickQuestion :: MonadState Q.Quiz m => m (Maybe Q.Question)
pickQuestion = do
    pick <- gets Q.next
    case pick of Just (question, updatedQuiz) -> do
                     put updatedQuiz
                     return $ Just question
                 Nothing -> return Nothing

addQuestion :: MonadState (Q.Quiz) m => Q.Question -> m ()
addQuestion question = modify (Q.add question)

-- Utilites for interacting with users.
formatSimplePercent :: Double -> String
formatSimplePercent = printf "%.0g%%"

resultSummary :: Result.Result -> String
resultSummary res = let successStr = show $ Result.successes res
                        questionStr | Result.successes res == 1 = "question"
                                    | otherwise          = "questions"
                        totalStr = show $ Result.total res
                        percentStr = formatSimplePercent $ Result.successPercent res
                    in mconcat ["You answered ", successStr, " ",
                                 questionStr, " correctly out of a total of ",
                                 totalStr, ".\n",
                                 "That's ", percentStr, "."]