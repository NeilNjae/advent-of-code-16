import Text.Parsec 
import Text.ParserCombinators.Parsec.Number

type Disk = (Int -> Bool)

main :: IO ()
main = do 
    text <- readFile "advent15.txt" 
    let disks = successfulParse $ parseIfile text
    part1 disks
    part2 disks

part1 :: [Disk] -> IO ()
part1 disks = print $ head $ filter (canFall disks) [0..]

part2 :: [Disk] -> IO ()
part2 disks = print $ head $ filter (canFall disks2) [0..]
    where disks2 = disks ++ [diskify 7 11 0]

canFall :: [Disk] -> Int -> Bool
canFall ds i = all (\d -> (d i)) ds


instructionFile = instructionLine `endBy` newline 
instructionLine = diskify <$> (string "Disc #" *> int) 
                          <*> (string " has " *> int)
                          <*> (string " positions; at time=0, it is at position " *> int)
                          <*  (string ".")

diskify :: Int -> Int -> Int -> (Int -> Bool)
diskify n size pos0 = (\i -> (size + n + pos0 + i) `mod` size == 0)

parseIfile :: String -> Either ParseError [Disk]
parseIfile input = parse instructionFile "(unknown)" input

parseIline :: String -> Either ParseError Disk
parseIline input = parse instructionLine "(unknown)" input

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a
