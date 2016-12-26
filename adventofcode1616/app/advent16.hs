module Main(main) where

import Data.List (nub)

input = "11100010111110100"
disk1length = 272
disk2length = 35651584

-- input = "10000"
-- disk1length = 20

main :: IO ()
main = do 
    part1 
    part2

part1 :: IO ()
part1 = putStrLn $ checksum $ take disk1length $ expand disk1length input

part2 :: IO ()
part2 = putStrLn $ checksum $ take disk2length $ expand disk2length input


expand :: Int -> String -> String
expand len a
    | length a >= len = a
    | otherwise = expand len $ a ++ "0" ++ b
        where b = map (invert) $ reverse a
              invert '0' = '1'
              invert '1' = '0'

checksum :: String -> String
checksum digits
    | odd $ length digits = digits
    | otherwise = checksum $ map (checksumPair) $ pairs digits
        where checksumPair p = if (length $ nub p) == 1 then '1' else '0'


pairs :: [a] -> [[a]]
pairs [] = []
pairs xs = [p] ++ (pairs ys)
    where (p, ys) = splitAt 2 xs 
