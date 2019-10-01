module Question
    ( Question(..)
    ) where

import Euterpea

-- Data structures representing questions.
data Question = Question { music :: Music Pitch
                         , prompt :: String
                         , test :: (String -> Bool)
                         , successText :: String
                         , failureText :: String }
