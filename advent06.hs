module Main(main) where

import Data.List (transpose)
import Data.Char (isLetter)
import qualified Data.Map.Lazy as Map


input = "cxdnnyjw"

main :: IO ()
main = do 
    text <- readFile "advent06.txt" 
    let message = lines text
    part1 message
    part2 message


part1 :: [String] -> IO ()
part1 message = do 
    print $ map (fst) $ map (mostCommon) $ map (countedLetters) $ transpose message

part2 :: [String] -> IO ()
part2 message = do 
    print $ map (fst) $ map (leastCommon) $ map (countedLetters) $ transpose message



countedLetters :: String -> Map.Map Char Int
countedLetters name = Map.fromListWith (+) [(c, 1) | c <- filter (isLetter) name]

mostCommon = Map.foldlWithKey (mostCommonP) ('a', 0)

mostCommonP (letter0, count0) letter count
    | count > count0 = (letter, count)
    | otherwise = (letter0, count0)

leastCommon = Map.foldlWithKey (leastCommonP) ('a', maxBound :: Int)

leastCommonP (letter0, count0) letter count
    | count < count0 = (letter, count)
    | otherwise = (letter0, count0)