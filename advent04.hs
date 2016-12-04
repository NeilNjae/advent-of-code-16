module Main(main) where

import Data.List (last, intersperse, sortBy, intercalate, isInfixOf)
import Data.List.Split (splitOn)
import Data.Char (isLetter, ord, chr)
import qualified Data.Map.Lazy as Map

data Room = Room { name :: String
                 , sector :: Int
                 , checksum :: String
                 } deriving (Show)

main :: IO ()
main = do 
        instrText <- readFile "advent04.txt" 
        let rooms = map (parseLine) $ lines instrText
        part1 rooms
        part2 rooms


part1 :: [Room] -> IO ()
part1 rooms = do 
    print $ sum $ map (sector) validRooms
    where 
        validChecksum room = (checksum room) == makeChecksum (name room)
        validRooms = filter (validChecksum) rooms

part2 :: [Room] -> IO ()
part2 rooms = do 
    print $ fst $ head $ filter (\sn -> isInfixOf "north" (snd sn)) sectorNames
    where 
        validChecksum room = (checksum room) == makeChecksum (name room)
        validRooms = filter (validChecksum) rooms
        sectorNames = [((sector r),
            shiftWord (sector r) (name r)) | r <- validRooms]


parseLine :: String -> Room
parseLine line = Room {name=name, sector=sector, checksum=checksum}
    where components = splitOn "-" line
          name = intercalate "-" $ reverse $ tail $ reverse components
          sector = read $ head $ splitOn "[" $ last components
          checksum = filter (isLetter) $ last components

countedLetters :: String -> [(Char, Int)]
countedLetters name = sortBy sortCLetter $ unsortedCountedLetters name
    where unsortedCountedLetters name = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- filter (isLetter) name]

sortCLetter :: (Char, Int) -> (Char, Int) -> Ordering
sortCLetter (l1, n1) (l2, n2)
    | n1 < n2 = GT
    | n1 > n2 = LT
    | n1 == n2 = compare l1 l2

makeChecksum :: String -> String
makeChecksum name = [l | (l, _) <- take 5 $ countedLetters name]


shiftWord :: Int -> String -> String
shiftWord shift letters = map (shiftLetter shift) letters

shiftLetter :: Int -> Char -> Char
shiftLetter shift letter
    | isLetter letter = chr $ (ord letter - ord 'a' + shift) `mod` 26 + ord 'a'
    | otherwise = ' '
