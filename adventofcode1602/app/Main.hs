module Main(main) where

import Data.Array.IArray

-- Row 1 is top, column 1 is left
type Position = (Int, Int)
type Keyboard = Array Position Char

kb1 = ["xxxxx", 
       "x123x", 
       "x456x",
       "x789x",
       "xxxxx"]

kb2 = ["xxxxxxx",
       "xxx1xxx",
       "xx234xx",
       "x56789x",
       "xxABCxx",
       "xxxDxxx",
       "xxxxxxx"]

enumerate = zip [0..]

mkKeyboard :: [String] -> Keyboard
mkKeyboard kb = array ((0, 0), (length kb - 1, length (kb!!0) - 1))
    [((i, j), c) | (i, r) <- enumerate kb, (j, c) <- enumerate r]

keyboard1 = mkKeyboard kb1
keyboard2 = mkKeyboard kb2

findKey :: Keyboard -> Char-> Position
findKey kb c = fst $ head $ filter (\a -> (snd a) == c) $ assocs kb

-- data Coord = One | Two | Three
--     deriving (Read, Show, Eq, Ord, Enum, Bounded)
-- -- instance Bounded Coord where
-- --     minBound = Coord 1
-- --     maxBound = Coord 3

-- data Position = Position Coord Coord
--     deriving (Show, Eq)

main :: IO ()
main = do 
        instrText <- readFile "data/advent02.txt" 
        let instructions = lines instrText
        part1 instructions
        part2 instructions

part1 :: [String] -> IO ()
part1 instructions = do 
        putStrLn $ followInstructions keyboard1 instructions


part2 :: [String] -> IO ()
part2 instructions = do 
        putStrLn $ followInstructions keyboard2 instructions


followInstructions :: Keyboard -> [String] -> String
followInstructions kb instr = moveSeries kb (startPosition kb) instr


startPosition :: Keyboard -> Position
startPosition kb = findKey kb '5'

moveSeries :: Keyboard -> Position -> [String] -> String
moveSeries _ _ [] = []
moveSeries kb p (i:is) = (n:ns)
    where p' = makeMoves kb p i
          n = kb ! p'
          ns = moveSeries kb p' is

makeMoves :: Keyboard -> Position -> [Char] -> Position
makeMoves kb p ms = foldl (safeMove kb) p ms

safeMove :: Keyboard -> Position -> Char -> Position
safeMove kb pos dir = maybeRevert kb pos (move pos dir)

move :: Position -> Char -> Position
move (r, c) 'U' = (r-1, c)
move (r, c) 'D' = (r+1, c)
move (r, c) 'L' = (r, c-1)
move (r, c) 'R' = (r, c+1)

maybeRevert :: Keyboard -> Position -> Position -> Position
maybeRevert kb oldPos newPos 
    | kb ! newPos == 'x' = oldPos
    | otherwise = newPos
