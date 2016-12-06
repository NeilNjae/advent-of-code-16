-- Better version of advent06.hs, using more standard library functions.

module Main(main) where

import Data.List (transpose, maximum, minimum)
import Data.Char (isLetter)
import Data.Tuple (swap)
import qualified Data.Map.Lazy as Map

main :: IO ()
main = do 
    text <- readFile "advent06.txt" 
    let message = lines text
    part1 message
    part2 message

part1 :: [String] -> IO ()
part1 message = do 
    putStrLn $ map (fst) $ map (mostCommon) $ map (countedLetters) $ transpose message

part2 :: [String] -> IO ()
part2 message = do 
    putStrLn $ map (fst) $ map (leastCommon) $ map (countedLetters) $ transpose message

countedLetters :: String -> [(Char, Int)]
countedLetters name = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- filter (isLetter) name]

mostCommon :: [(Char, Int)] -> (Char, Int)
mostCommon = swap . maximum . map (swap)

leastCommon :: [(Char, Int)] -> (Char, Int)
leastCommon = swap . minimum . map (swap)
