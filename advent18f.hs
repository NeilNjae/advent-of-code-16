import Data.List (tails, foldl')

-- input = "..^^."
-- input = ".^^.^.^^^^"
input = "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^."

main :: IO ()
main = do 
        part1 
        part2

part1 :: IO ()
part1 = print $ fst $ foldl' nextRowFold (countSafe row, row) [2..40]
    where row = readRow input

part2 :: IO ()
part2 = print $ fst $ foldl' nextRowFold (countSafe row, row) [2..400000]
    where row = readRow input

readRow :: String -> [Bool]
readRow = map (=='^')

showRow :: [Bool] -> String
showRow = map (\c -> if c then '^' else '.')

extended :: [Bool] -> [Bool]
extended row = [False] ++ row ++ [False]

nextRow :: [Bool] -> [Bool]
nextRow = map (isTrap) . segments . extended

nextRowFold :: (Int, [Bool]) -> Int -> (Int, [Bool])
nextRowFold (n, row) _ = (n + countSafe newRow, newRow)
    where newRow = nextRow row

countSafe :: [Bool] -> Int
countSafe = length . filter (not)

segments :: [a] -> [[a]]
segments = filter ((==3) . length) . map (take 3) . tails

isTrap :: [Bool] -> Bool
isTrap segment
    | segment == [True, True, False] = True
    | segment == [False, True, True] = True
    | segment == [True, False, False] = True
    | segment == [False, False, True] = True
    | otherwise = False
