module Main(main) where

import Data.List (tails)

-- input = "..^^."
-- input = ".^^.^.^^^^"
input = "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^."

main :: IO ()
main = do 
        part1 
        part2

part1 :: IO ()
-- part1 = putStrLn $ unlines $ map (showRow) $ take 10 $ iterate nextRow $ readRow input
part1 = print $ length $ filter (not) $ concat $ take 40 $ iterate nextRow $ readRow input

part2 :: IO ()
part2 = print $ length $ filter (not) $ concat $ take 400000 $ iterate nextRow $ readRow input

readRow :: String -> [Bool]
readRow = map (=='^')

showRow :: [Bool] -> String
showRow = map (\c -> if c then '^' else '.')

extended :: [Bool] -> [Bool]
extended row = [False] ++ row ++ [False]

nextRow :: [Bool] -> [Bool]
nextRow = map (isTrap) . segments . extended

segments :: [a] -> [[a]]
segments = filter ((==3) . length) . map (take 3) . tails

isTrap :: [Bool] -> Bool
isTrap segment
    | segment == [True, True, False] = True
    | segment == [False, True, True] = True
    | segment == [True, False, False] = True
    | segment == [False, False, True] = True
    | otherwise = False
