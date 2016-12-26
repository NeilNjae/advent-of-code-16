{-# LANGUAGE DeriveGeneric #-}

module Main(main) where

import GHC.Generics (Generic)
import Text.Parsec 
import Text.ParserCombinators.Parsec.Number
import Data.Maybe (catMaybes, fromJust)
import Data.List (find, delete, sort, sortOn, reverse)

data Node = Node { x :: Int
                 , y :: Int
                 , size :: Int
                 , used :: Int
                 , available :: Int
                 , use_pc :: Int 
                 } deriving (Show, Eq, Ord)


testGrid = "\
\Filesystem            Size  Used  Avail  Use%\n\
\/dev/grid/node-x0-y0   10T    8T     2T   80%\n\
\/dev/grid/node-x0-y1   11T    6T     5T   54%\n\
\/dev/grid/node-x0-y2   32T   28T     4T   87%\n\
\/dev/grid/node-x1-y0    9T    7T     2T   77%\n\
\/dev/grid/node-x1-y1    8T    0T     8T    0%\n\
\/dev/grid/node-x1-y2   11T    7T     4T   63%\n\
\/dev/grid/node-x2-y0   10T    6T     4T   60%\n\
\/dev/grid/node-x2-y1    9T    8T     1T   88%\n\
\/dev/grid/node-x2-y2    9T    6T     3T   66%\n\
\"

main :: IO ()
main = do 
    text <- readFile "data/advent22.txt" 
    let sizes = successfulParse $ parseFile text
    let maxX = maximum $ map (\n -> x n) sizes
    let maxY = maximum $ map (\n -> y n) sizes
    putStrLn $ unlines $ map (showRow maxX sizes) [0..(maxY-1)]


showRow maxX sizes r = map (charOf) row
  where row = sortOn (\c -> x c) $ filter (\c -> y c == r) sizes
        charOf c = if (used c) == 0 
                      then '+'
                      else if (used c) > 100
                              then '#'
                              else '_'




duFile = duLine `sepEndBy` newline 
-- duLine = (optionMaybe nodeL)

duLine = (nodeL >>= return . Just) <|> (headerL >> return Nothing)

headerL = (many (noneOf "\r\n"))

nodeL = nodeify <$> (string "/dev/grid/node-x" *> int)
                <*> (string "-y" *> int)
                <*> (spaces *> int <* string "T")
                <*> (spaces *> int <* string "T")
                <*> (spaces *> int <* string "T")
                <*> (spaces *> int <* string "%")
            where nodeify x y size used available use_pc = 
                      Node {x=x, y=y, size=size, used=used, available=available, use_pc=use_pc}

parseFile :: String -> Either ParseError [Maybe Node]
parseFile input = parse duFile "(unknown)" input

parseLine :: String -> Either ParseError (Maybe Node)
parseLine input = parse duLine "(unknown)" input

successfulParse :: Either ParseError [Maybe a] -> [a]
successfulParse (Left _) = []
successfulParse (Right a) = catMaybes a
