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
part1 = print $ fill disk1length input

part2 :: IO ()
part2 = print $ fill disk2length input

fill :: Int -> String -> String
fill len filler = deBool $ checksum $ take len $ expand len $ enBool filler

enBool :: String -> [Bool]
enBool = map (== '1')

deBool :: [Bool] -> String
deBool = map (\b -> if b then '1' else '0')


expand :: Int -> [Bool] -> [Bool]
expand len = head . dropWhile ((<= len) . length) . iterate expandStep

expandStep :: [Bool] -> [Bool]
expandStep a = a ++ [False] ++ b
        where b = map (not) $ reverse a

checksum :: [Bool] -> [Bool]
checksum = head . dropWhile (even . length) . iterate checksumStep 

checksumStep :: [Bool] -> [Bool]
checksumStep [] = []
checksumStep [x] = [x]
checksumStep (x:y:xs) = (x==y):(checksumStep xs)

