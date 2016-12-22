module Main(main) where

import Data.List (sort)
import Data.List.Split (splitOn)

-- turn direction, number of steps
data Step = Step Char Int deriving (Show)

data Direction = North | East | South | West 
    deriving (Enum, Show, Bounded, Eq)

-- direction, easting, northing
data Position = Position Direction Int Int deriving (Show)
-- Two positions are the same if they're in the same place, 
-- regardless of facing
instance Eq Position where
    Position _ e n == Position _ e' n' = e == e' && n == n'

main :: IO ()
main = do 
        instructions <- readFile "data/advent01.txt"
        part1 instructions
        part2 instructions

part1 :: String -> IO ()
part1 instructions = do
        let answer = finalDistance $ last $ stepsFromStart $ steps instructions
        print answer

part2 :: String -> IO ()
part2 instructions = do
        let visited = finalDistance $ firstRepeat $ stepsFromStart $ expandSteps $ steps instructions
        print visited


-- Extract the steps from the input string.
steps :: String -> [Step]
steps s = map readStep $ splitOn ", " s
    where readStep (d:l) = Step d (read l)

-- Take steps from the starting position
stepsFromStart :: [Step] -> [Position]
stepsFromStart = takeSteps (Position North 0 0)

-- Calculate manhattan distance from start to this state
finalDistance :: Position -> Int
finalDistance (Position _ e n) = (abs e) + (abs n)

-- For part 2: convert one step of many spaces to many steps of one space each
expandSteps :: [Step] -> [Step]
expandSteps = 
    concatMap expandStep
    where expandStep (Step dir d) = (Step dir 1) : replicate (d - 1) (Step 'S' 1)

-- Execute a series of steps, keeping track of the positions after each step
takeSteps :: Position -> [Step] -> [Position]
-- takeSteps pos steps = scanl move pos steps
takeSteps = scanl move

-- Make one move, by updating direction then position
move :: Position -> Step -> Position
move (Position facing easting northing)
    (Step turnInstr distance) = 
    Position facing' easting' northing'
    where facing' = turn turnInstr facing
          (easting', northing') = takeStep facing' distance easting northing

-- Turn right, left, or straight
turn :: Char -> Direction -> Direction
turn 'R' direction = turnCW direction
turn 'L' direction = turnACW direction
turn 'S' direction = direction

-- Move in the current direction
takeStep :: Direction -> Int -> Int -> Int -> (Int, Int)
takeStep North d e n = (e, n+d)
takeStep South d e n = (e, n-d)
takeStep West  d e n = (e-d, n)
takeStep East  d e n = (e+d, n)


-- | a `succ` that wraps 
turnCW :: (Bounded a, Enum a, Eq a) => a -> a 
turnCW dir | dir == maxBound = minBound
         | otherwise = succ dir

-- | a `pred` that wraps
turnACW :: (Bounded a, Enum a, Eq a) => a -> a
turnACW dir | dir == minBound = maxBound
            | otherwise = pred dir

-- All the prefixes of a list of items
prefixes = scanl addTerm []
    where addTerm ps t = ps ++ [t]

-- The first item that exists in a prefix of the list to that point
firstRepeat positions = 
    last $ head $ dropWhile (\p -> (last p) `notElem` (tail $ reverse p)) 
                            (tail $ prefixes positions)
