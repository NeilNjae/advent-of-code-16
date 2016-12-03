import Data.List
import Data.List.Split

type Triple = [Integer]

main :: IO ()
main = do 
        instrText <- readFile "advent03.txt" 
        let triangles = map (parseLine) $ lines instrText
        part1 triangles
        part2 triangles


part1 :: [Triple] -> IO ()
part1 triangles = do 
    print $ length $ filter (validTriangle) triangles 

part2 :: [Triple] -> IO ()
part2 triangles = do 
    print $ length $ filter (validTriangle) $ byColumns triangles 


parseLine :: String -> Triple
parseLine = map (read) . filter (not . null) . splitOn " "

validTriangle :: Triple -> Bool
validTriangle triple = sortedTriple!!0 + sortedTriple!!1 > sortedTriple!!2
    where sortedTriple = sort triple

byColumns :: [[Integer]] -> [Triple]
byColumns = chunksOf 3 . concat . transpose 
