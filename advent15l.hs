import Text.Parsec 
import Text.ParserCombinators.Parsec.Number

main :: IO ()
main = do 
    text <- readFile "advent15.txt" 
    let disks = successfulParse $ parseIfile text
    part1 disks
    part2 disks

part1 :: [[Int]] -> IO ()
part1 disks = print $ head $ filter (canFall disks) [0..]

part2 :: [[Int]] -> IO ()
part2 disks = print $ head $ filter (canFall disks2) [0..5000000]
    where disks2 = id $! map (take 5000000) $ disks ++ [drop 7 $ drop 0 $ cycle [0..(11-1)]]

canFall :: [[Int]] -> Int -> Bool
canFall ds i = all (\d -> (d!!i) == 0) ds


instructionFile = instructionLine `endBy` newline 
instructionLine = diskify <$> (string "Disc #" *> int) 
                          <*> (string " has " *> int)
                          <*> (string " positions; at time=0, it is at position " *> int)
                          <*  (string ".")
                    where diskify n size pos0 = drop n $ drop pos0 $ cycle [0..(size-1)]

parseIfile :: String -> Either ParseError [[Int]]
parseIfile input = parse instructionFile "(unknown)" input

parseIline :: String -> Either ParseError [Int]
parseIline input = parse instructionLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a
