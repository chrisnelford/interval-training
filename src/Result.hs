module Result
    ( Result(..)
    , success
    , failure
    , total
    , successPercent
    ) where

-- Keeping track of score as a quiz runs.
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
