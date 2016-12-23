{-# LANGUAGE DeriveGeneric #-}

module Main(main) where

import GHC.Generics (Generic)
import Text.Parsec 
import Text.ParserCombinators.Parsec.Number
import Data.Maybe (catMaybes, fromJust)
import Data.List (find, delete, sort)
import Data.Graph.AStar
import Data.Hashable
import qualified Data.HashSet

data Node = Node { x :: Int
                 , y :: Int
                 , size :: Int
                 , used :: Int
                 , available :: Int
                 , use_pc :: Int 
                 } deriving (Show, Eq, Ord, Generic)
instance Hashable Node

data SearchState = SearchState { cx :: Int
                               , cy :: Int 
                               , grid :: [Node]
                               } deriving (Show, Eq, Generic)
instance Hashable SearchState
instance Ord SearchState where
    s1 `compare` s2 = (heuristic s1) `compare` (heuristic s2)


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
    part1 sizes
    part2 sizes

part1 :: [Node] ->  IO ()
part1 sizes = print $ length viable
    where viable = [(a, b) | a <- sizes, 
                             b <- sizes, 
                             a /= b,
                             (used a) > 0,
                             (used a) <= (available b)]


part2 :: [Node] ->  IO ()
part2 sizes = 
    -- do let testSizes = successfulParse $ parseFile testGrid
    --    putStrLn $ searchTrace $ fromJust $ 
    --                 aStar successors 
    --                         (\_ _ -> 1) 
    --                         heuristic
    --                         isGoal
    --                         (startSt testSizes)
    putStrLn $ searchTrace $ fromJust $ 
                    aStar successors 
                            (\_ _ -> 1) 
                            heuristic
                            isGoal
                            (startSt sizes)



searchTrace :: [SearchState] -> String
searchTrace ss = unlines $ map (sst) ss
    where sst s = "(" ++ show (cx s) ++ ", " ++ show (cy s) ++ ") :: " ++ holeS s
          hole sk = fromJust $ find (\n -> used n == 0) $ grid sk 
          holeS sk = "(" ++ show (x $ hole sk) ++ ", " ++ show (y $ hole sk) ++ ")"

startSt :: [Node] -> SearchState
startSt nodes = SearchState {cx = maximum xs, cy = 0, grid = nodes}
    where xs = map (\n -> x n) nodes

isGoal :: SearchState -> Bool
isGoal st = cx st == 0 && cy st == 0

adjacent :: Node -> Node -> Bool
adjacent n1 n2 = abs ((x n1) - (x n2)) + abs ((y n1) - (y n2)) == 1

-- A move of data from n1 to n2 is legal.
legal :: Node -> Node -> Bool
legal n1 n2 = adjacent n1 n2 && used n1 > 0 && used n1 <= available n2

heuristic :: SearchState -> Int
heuristic st = (cx st) + (cy st)

successors :: SearchState -> Data.HashSet.HashSet SearchState
successors st = Data.HashSet.fromList $ map (newState st current) possibleMoves
    where nodes = grid st
          current = fromJust $ find (\n -> (x n) == (cx st) && (y n) == (cy st)) nodes
          possibleMoves = [(n1, n2) | n1 <- nodes, n2 <- nodes, legal n1 n2]


-- Moving data from n1 to n2
newState :: SearchState -> Node -> (Node, Node) -> SearchState
newState st current (n1, n2) = st {cx = cx', cy = cy', grid = grid'}
    where cx' = if current == n1 then x n2 else x current
          cy' = if current == n1 then y n2 else y current
          grid' = sort $ (n2 {used = (used n2 + used n1), available = (available n2 - used n1)}):
                        (n1 {used = 0, available = (size n1)}):
                        (delete n1 $ delete n2 (grid st))


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
