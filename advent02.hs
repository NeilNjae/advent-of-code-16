import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Array.IArray


type Position = (Int, Int)
type Keyboard = Array Position Char

kb1 = [['x', 'x', 'x', 'x', 'x'], 
       ['x', '1', '2', '3', 'x'], 
       ['x', '4', '5', '6', 'x'],
       ['x', '7', '8', '9', 'x'],
       ['x', 'x', 'x', 'x', 'x']]


kb2 = ["xxxxxxx",
       "xxx1xxx",
       "xx234xx",
       "x56789x",
       "xxABCxx",
       "xxxDxxx",
       "xxxxxxx"]

enumerate = zip [0..]

mkKeyboard :: [String] -> Keyboard
mkKeyboard kb = array ((0, 0), (length kb - 1, length kb - 1))
    [((i, j), c) | (i, r) <- enumerate kb, (j, c) <- enumerate r]

keyboard1 = mkKeyboard kb1
keyboard2 = mkKeyboard kb2

findKey :: Keyboard -> Char-> Position
findKey kb c = fst $ head $ filter (\(i, e) -> e == c) $ assocs kb

-- data Coord = One | Two | Three
--     deriving (Read, Show, Eq, Ord, Enum, Bounded)
-- -- instance Bounded Coord where
-- --     minBound = Coord 1
-- --     maxBound = Coord 3

-- -- Row 1 is top, column 1 is left
-- data Position = Position Coord Coord
--     deriving (Show, Eq)

main :: IO ()
main = do 
        instrText <- readFile "advent02.txt" 
        let instructions = lines instrText
        part1 instructions
        part2 instructions

part1 :: [String] -> IO ()
part1 instructions = do 
        print $ followInstructions keyboard1 instructions


part2 :: [String] -> IO ()
part2 instructions = do 
        print $ followInstructions keyboard2 instructions


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
move (r, c) 'U' = (dec r, c)
move (r, c) 'D' = (inc r, c)
move (r, c) 'L' = (r, dec c)
move (r, c) 'R' = (r, inc c)

maybeRevert :: Keyboard -> Position -> Position -> Position
maybeRevert kb oldPos newPos 
    | kb ! newPos == 'x' = oldPos
    | otherwise = newPos

numberOf p = '1'
-- numberOf :: Position -> Char
-- numberOf (Position One One) = '1'
-- numberOf (Position One Two) = '2'
-- numberOf (Position One Three) = '3'
-- numberOf (Position Two One) = '4'
-- numberOf (Position Two Two) = '5'
-- numberOf (Position Two Three) = '6'
-- numberOf (Position Three One) = '7'
-- numberOf (Position Three Two) = '8'
-- numberOf (Position Three Three) = '9'

-- | a `succ` that stops
inc :: (Bounded a, Enum a, Eq a) => a -> a 
inc dir | dir == maxBound = maxBound
        | otherwise = succ dir

-- | a `pred` that stops
dec :: (Bounded a, Enum a, Eq a) => a -> a
dec dir | dir == minBound = minBound
        | otherwise = pred dir