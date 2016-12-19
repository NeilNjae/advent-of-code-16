{-# LANGUAGE BangPatterns #-}

import Prelude hiding (length, take, drop)
import Data.Sequence

-- input = 5 
input = 3012210 

main :: IO ()
main = do 
    part1 
    part2

part1 :: IO ()
part1 = print $ 2 * (input - 2 ^ (toInteger (floor $ logBase 2 (fromIntegral input)))) + 1

part2 :: IO ()
part2 = print $ index (presentSteps initial) 0

presentSteps :: Seq Int -> Seq Int
presentSteps elves 
    | isFinished elves = elves
    | otherwise = presentSteps $ next elves

initial :: Seq Int
initial = fromList [1..input] 

isFinished :: Seq Int -> Bool
isFinished elves = length elves == 1

next :: Seq Int -> Seq Int
next elves = prefix >< (midfix |> suffix)
    where 
        target = length elves `quot` 2
        prefix = drop 1 $ take target elves
        midfix = drop (target+1) elves
        suffix = index elves 0
