module Main(main) where

import Data.List (transpose, maximum, minimum, sort, group)
import Data.Tuple (swap)

main :: IO ()
main = do 
    text <- readFile "advent06.txt" 
    let message = lines text
    part1 message
    part2 message

part1 :: [String] -> IO ()
part1 message = do 
    putStrLn $ map (snd . maximum . counts) $ transpose message

part2 :: [String] -> IO ()
part2 message = do 
    putStrLn $ map (snd . minimum . counts) $ transpose message

counts :: (Eq a, Ord a) => [a] -> [(Int, a)]
counts = map (\g -> (length g, head g)) . group . sort