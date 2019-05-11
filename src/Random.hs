module Random
    ( randomOctave
    , randomPitch
    , randomInterval
    , consRandom
    , unconsRandom
    , module Control.Monad.Random
    ) where

import System.Random

import Control.Monad.Random
import Euterpea

-- Randomness for Euterpea note-level contstructs

-- We want a uniform distribution of all of the notes. Rather than
-- mixing up different names for the same note, we follow Euterpea's
-- lead and use sharps and naturals as the canonical pitch classes for
-- each tone.
canonicalNotes :: [PitchClass]
canonicalNotes = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]

instance Random PitchClass where
    randomR (lower, upper) g = runRand (fromList distribution) g
        where distribution = weighted $ filter inRange canonicalNotes
              inRange x = (x >= lower) && (x <= upper)
              weighted = map (\x -> (x, 1))
    random = randomR (minBound, maxBound)

-- On a piano, 0 is the lowest octave, and 8 is the highest.
randomOctave :: MonadRandom m => m Octave
randomOctave = getRandomR (0, 8)

randomPitch :: MonadRandom m => m Pitch
randomPitch = do
    pitchClass <- getRandom
    octave <- randomOctave
    return (pitchClass, octave)

-- There are 12 semitones in a whole octave, so a number between
-- 1 and 12 with give any possible ascending interval.
randomInterval :: MonadRandom m => m (Pitch, Int)
randomInterval = do
    basePitch <- randomPitch
    interval <- getRandomR (1, 12)
    return (basePitch, interval)

unconsRandom :: MonadRandom m => [a] -> m (Maybe (a, [a]))
unconsRandom [] = return Nothing
unconsRandom xs = do
    index <- getRandomR (0, (length xs) - 1)
    let (start, rest) = splitAt index xs
    return $ Just (head rest, start ++ tail rest)

-- In a list of length n, there are n+1 places to insert a new element:
--   * at the head of the list (1 location)
--   * after each element in the list (n locations)
consRandom :: MonadRandom m => a -> [a] -> m [a]
consRandom x xs = do
    index <- getRandomR (0, length xs)
    let (start, rest) = splitAt index xs
    return $ start ++ (x:rest)