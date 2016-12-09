module Main(main) where

import Data.Array.IArray
import Text.Parsec
-- import Control.Applicative
import Control.Applicative ((<$), (<*), (*>), (<*>), pure, liftA)
import Control.Monad (liftM, ap)

-- Row 1 is top, column 1 is left
type Position = (Int, Int)
type Screen = Array Position Bool

data Direction = Row | Column deriving (Show)
data Command = Rect Int Int | Rotate Direction Int Int deriving (Show)

data ScState a = ScState (Screen -> (Screen, a))

mkScreen :: Int -> Int -> Screen
mkScreen w h = array ((0, 0), (h - 1, w - 1))
    [((i, j), False) | i <- [0..(h-1)], j <- [0..(w-1)]]

showScreen :: Screen -> String
showScreen screen = unlines [showRow r | r <- [minRow..maxRow]]
    where ((minRow, minCol), (maxRow, maxCol)) = bounds screen
          showCell True  = '#'
          showCell False = '.'
          showRow r = [showCell (screen!(r, c)) | c <- [minCol..maxCol]]

countLights :: Screen -> Int
countLights screen = length $ filter (id) $ elems screen

screen0 = mkScreen 50 6


main :: IO ()
main = do
    text <- readFile "advent08.txt"
    let instrs = successfulParse $ parseCommands text
    -- print instrs
    part1 instrs
    part2 instrs

part1 :: [Command] -> IO ()
part1 commands =
    print $ countLights $ (extractScreen . doCommands) commands

part2 :: [Command] -> IO ()
part2 commands = 
    putStrLn $ showScreen $ (extractScreen . doCommands) commands


instance Functor ScState where
  fmap = liftM

instance Applicative ScState where
  pure  = return
  (<*>) = ap

instance Monad ScState where
    return x = ScState (\screen -> (screen, x))

    (ScState st) >>= f
        = ScState (\screen -> let
                            (newScreen, y) = st screen
                            (ScState transformer) = f y
                            in
                            transformer newScreen)

doCommands :: [Command] -> ScState (Int)
doCommands [] = return 0
doCommands (i:is) = 
    do doCommand i
       doCommands is
       return 0

doCommand :: Command -> ScState Int
doCommand i = ScState (execute i)

execute :: Command -> (Screen -> (Screen, Int))
execute (Rect w h) screen = (rect screen w h, 0)
execute (Rotate Column c n) screen = (rotateColumn screen c n, 0)
execute (Rotate Row r n) screen = (rotateRow screen r n, 0)

extractScreen :: ScState Int -> Screen
extractScreen (ScState st) = fst (st screen0)

parseCommands :: String -> Either ParseError [Command]
parseCommands input = parse commandFile "(unknown)" input

commandFile = commandLine `endBy` newline
commandLine = (try rectCommand) <|> rotateCommand

rectCommand = 
    do  string "rect"
        spaces
        w <- (many1 digit)
        char 'x'
        h <- (many1 digit)
        return (Rect (read w) (read h))

rotateCommand = 
    do  string "rotate"
        spaces
        direction <- (string "row" <|> string "column")
        spaces
        string "x=" <|> string "y="
        index <- (many1 digit)
        spaces
        string "by"
        spaces
        distance <- (many1 digit)
        return (buildCommand direction index distance)

buildCommand "row" i d = Rotate Row (read i) (read d)
buildCommand "column" i d = Rotate Column (read i) (read d)

successfulParse :: Either ParseError [a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = a




rect :: Screen -> Int -> Int -> Screen
rect screen w h = screen // newBits
    where newBits = [((i, j), True) | i <- [0..(h-1)], j <- [0..(w-1)]]

rotateColumn :: Screen -> Int -> Int -> Screen
rotateColumn screen column givenShift = screen // newCells
    where 
        ((minRow, minCol), (maxRow, maxCol)) = bounds screen
        colLength = 1 + maxRow - minRow
        shift = givenShift `mod` colLength
        offset = colLength - shift
        column0 = [screen!(r, column) | r <- [minRow..maxRow]]
        newColumn = (drop offset column0) ++ (take offset column0)
        newCells = [((r, column), cell) | (r, cell) <- zip [minRow..maxRow] newColumn]

rotateRow :: Screen -> Int -> Int -> Screen
rotateRow screen row givenShift = screen // newCells
    where 
        ((minRow, minCol), (maxRow, maxCol)) = bounds screen
        rowLength = 1 + maxCol - minCol
        shift = givenShift `mod` rowLength
        offset = rowLength - shift
        row0 = [screen!(row, c) | c <- [minCol..maxCol]]
        newRow = (drop offset row0) ++ (take offset row0)
        newCells = [((row, c), cell) | (c, cell) <- zip [minCol..maxCol] newRow]
