module Quiz
    ( Quiz
    , next
    ) where

import qualified System.Random as R
import qualified Question as Question

-- What is a Quiz?
--
-- * It's a source of questions.
-- * It may rely on randomness to generate the questions.
data Quiz = Quiz

next :: R.RandomGen r => r -> Quiz -> Question.Question
next = undefined