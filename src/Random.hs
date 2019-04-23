module Random
    ( randomOctave
    , randomPitch
    , randomInterval
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
