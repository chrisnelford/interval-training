module Lib
    ( runQuiz
    ) where

import System.Random
import Euterpea

randomPitchClass :: (RandomGen g) => g -> (PitchClass, g)
randomPitchClass g = (toEnum randomInt, g')
  where minPitchClass = minBound :: PitchClass
        maxPitchClass = maxBound :: PitchClass
        (randomInt, g') = randomR (fromEnum minPitchClass, fromEnum maxPitchClass) g

runQuiz :: IO ()
runQuiz = do
    gen <- getStdGen
    let (pitch, _) = randomPitchClass gen
    play $ note 1 (pitch, 4 :: Octave)
    putStrLn "What note did you hear?"
    answer <- getLine
    let answerPitch = read answer :: PitchClass
    if absPitch (answerPitch, 4) == absPitch (pitch, 4)
        then putStrLn "Correct"
        else putStrLn $ "Wrong: that was a " ++ show pitch
